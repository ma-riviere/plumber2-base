# HTTP client for back. Thin httr2 wrappers that attach the session user's
# access token (refreshing it through ensure_fresh_access_token, which raises
# fe_auth_expired when the session cannot produce one), map backend problem+json
# errors to the classed fe_backend_error condition, and retry idempotent GETs
# once. Guest sessions carry no token and send no Authorization header (the
# backend's dev-only bypass guard authenticates them).

BE_TIMEOUT_SECONDS <- 15

backend_error <- function(status, title, detail = "") {
    structure(
        class = c("fe_backend_error", "error", "condition"),
        list(
            message = sprintf("backend %d: %s%s", status, title, if (nzchar(detail)) paste0(" - ", detail) else ""),
            status = as.integer(status),
            title = title,
            detail = detail,
            call = NULL
        )
    )
}

be_request <- function(state, datastore, path, timeout = BE_TIMEOUT_SECONDS) {
    token <- ensure_fresh_access_token(datastore, state$config)
    req <- httr2::request(state$config$backend_url) |>
        httr2::req_url_path(path) |>
        httr2::req_timeout(timeout) |>
        httr2::req_error(is_error = function(resp) FALSE)
    if (!is.null(token)) {
        req <- httr2::req_auth_bearer_token(req, token)
    }
    req
}

# Parse a JSON response body with yyjsonr (faster than resp_body_json's
# jsonlite path). Data.frame promotion is off so backend shapes stay plain
# lists/vectors: arrays of objects arrive as lists of named lists, scalar
# arrays as atomic vectors, JSON null as NULL.
be_parse_json <- function(resp) {
    yyjsonr::read_json_str(
        httr2::resp_body_string(resp),
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
}

# Perform the request; anything outside `expected` becomes fe_backend_error
# (with the backend's problem+json title/detail when present). Connection-level
# failures surface as a 503 so callers treat "backend down" like any error.
be_perform <- function(req, expected = c(200L, 201L, 202L, 204L)) {
    resp <- tryCatch(
        httr2::req_perform(req),
        error = function(e) stop(backend_error(503L, "Backend unreachable", conditionMessage(e)))
    )
    status <- httr2::resp_status(resp)
    if (!status %in% expected) {
        problem <- tryCatch(be_parse_json(resp), error = function(e) NULL)
        stop(backend_error(
            status,
            problem$title %||% httr2::resp_status_desc(resp),
            problem$detail %||% ""
        ))
    }
    resp
}

be_body <- function(resp) {
    if (httr2::resp_status(resp) == 204L || length(httr2::resp_body_raw(resp)) == 0) {
        return(NULL)
    }
    be_parse_json(resp)
}

be_get <- function(state, datastore, path, query = list(), timeout = BE_TIMEOUT_SECONDS) {
    query <- query[!vapply(query, is.null, logical(1))]
    req <- be_request(state, datastore, path, timeout = timeout) |>
        httr2::req_retry(max_tries = 2, retry_on_failure = TRUE, backoff = function(i) 0.5)
    if (length(query)) {
        req <- httr2::req_url_query(req, !!!query)
    }
    be_body(be_perform(req))
}

# JSON-bodied POST/PATCH and bare DELETE.
be_send <- function(state, datastore, path, method = "POST", body = NULL) {
    req <- httr2::req_method(be_request(state, datastore, path), method)
    if (!is.null(body)) {
        req <- httr2::req_body_raw(req, yyjsonr::write_json_str(body, auto_unbox = TRUE), type = "application/json")
    }
    be_body(be_perform(req))
}

# Multipart CSV upload. The bytes are staged in a temp file named after the
# original upload so the part carries both the filename and text/csv (the
# backend's multipart parser dispatches on the part content type).
be_upload_dataset <- function(state, datastore, csv_bytes, filename, name = NULL, description = NULL) {
    stage_dir <- tempfile("upload")
    dir.create(stage_dir)
    on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)
    csv_path <- file.path(stage_dir, safe_upload_filename(filename))
    writeBin(csv_bytes, csv_path)

    parts <- list(file = curl::form_file(csv_path, type = "text/csv"))
    if (!is.null(name) && nzchar(name)) {
        parts$name <- name
    }
    if (!is.null(description) && nzchar(description)) {
        parts$description <- description
    }
    req <- be_request(state, datastore, "/v1/datasets") |>
        httr2::req_body_multipart(!!!parts)
    be_body(be_perform(req))
}

# Full CSV fetch for the download proxy: returns the raw body plus the headers
# the proxy forwards. The body stays in memory (uploads are capped well below
# that being a concern) and httpuv streams it to the client off the R thread.
be_fetch_csv <- function(state, datastore, dataset_id) {
    resp <- be_perform(be_request(state, datastore, sprintf("/v1/datasets/%d/data.csv", as.integer(dataset_id))))
    list(
        body = httr2::resp_body_raw(resp),
        content_type = httr2::resp_header(resp, "Content-Type") %||% "text/csv",
        content_disposition = httr2::resp_header(resp, "Content-Disposition") %||% "attachment"
    )
}

# The multipart filename is attacker-controlled and is used both to name a
# staged temp file and as the part filename sent to the backend. Reduce it to a
# bare name with path separators and control characters stripped, so it cannot
# traverse out of the staging dir (writeBin would otherwise write anywhere the
# service user can). Falls back to a fixed name when nothing usable remains.
safe_upload_filename <- function(filename) {
    cleaned <- gsub("[[:cntrl:]/\\\\]", "_", filename %||% "")
    cleaned <- basename(cleaned)
    if (!nzchar(cleaned) || cleaned %in% c(".", "..")) {
        return("dataset.csv")
    }
    cleaned
}

# --- multipart helpers ---------------------------------------------------------

# plumber2's multipart parser sub-parses text/csv parts into a data.frame and
# leaves unknown content types (some browsers send application/vnd.ms-excel or
# octet-stream for .csv) as raw bytes. Normalize both to the CSV bytes the
# backend receives; NULL means the part is not usable as a CSV.
part_as_csv_bytes <- function(part) {
    if (is.raw(part)) {
        return(part)
    }
    if (is.data.frame(part)) {
        con <- rawConnection(raw(0), "w")
        on.exit(close(con), add = TRUE)
        utils::write.csv(part, con, row.names = FALSE)
        return(rawConnectionValue(con))
    }
    if (is.character(part) && length(part) == 1) {
        return(charToRaw(part))
    }
    NULL
}

# A single trimmed string from a form field, or NULL when absent/empty.
scalar_field <- function(x) {
    if (is.null(x) || is.raw(x)) {
        return(NULL)
    }
    value <- trimws(as.character(x)[1])
    if (is.na(value) || !nzchar(value)) NULL else value
}

# --- session scopes ------------------------------------------------------------

# Scopes the backend grants this session's credential, fetched from /v1/me once
# and cached in the session. An unreachable backend yields an empty grant that
# is retried on the next page render after a short backoff (so a backend
# restart does not blank the Admin nav for the rest of the session).
SCOPES_RETRY_SECONDS <- 300

# NOTE: [[ ]] indexing throughout - $ would partial-match "scopes" to
# "scopes_failed_at" when the grant is not cached yet.
session_scopes <- function(state, datastore) {
    auth <- datastore$session$auth
    if (is.null(auth)) {
        return(character())
    }
    if (!is.null(auth[["scopes"]])) {
        return(unlist(auth[["scopes"]], use.names = FALSE))
    }
    now <- as.numeric(Sys.time())
    if (!is.null(auth[["scopes_failed_at"]]) && now - auth[["scopes_failed_at"]] < SCOPES_RETRY_SECONDS) {
        return(character())
    }
    me <- tryCatch(
        be_get(state, datastore, "/v1/me", timeout = 3),
        error = function(e) NULL
    )
    auth <- datastore$session$auth
    if (is.null(me)) {
        auth[["scopes_failed_at"]] <- now
        datastore$session$auth <- auth
        return(character())
    }
    auth[["scopes"]] <- unlist(me$scopes, use.names = FALSE) %||% character()
    auth[["scopes_failed_at"]] <- NULL
    datastore$session$auth <- auth
    auth[["scopes"]]
}

session_can <- function(state, datastore, scope) {
    scope %in% session_scopes(state, datastore)
}

# --- handler error boundary ----------------------------------------------------

# Wrap a page/partial handler body. fe_auth_expired becomes the login redirect
# (302 for navigation, 200 + HX-Redirect for htmx, mirroring the gate);
# fe_backend_error becomes an error alert carrying the backend's real status -
# bare for htmx (swapped into the target for 4xx per the shell's
# responseHandling config, surfaced as a toast for 5xx via app.js), wrapped in
# the page shell for full-page navigation.
with_fe_errors <- function(request, response, state, datastore, expr) {
    lang <- resolve_lang(request, state$translations)
    tryCatch(
        expr,
        fe_auth_expired = function(e) {
            if (is_htmx_request(request)) {
                set_html_headers(response)
                response$status <- 200L
                response$set_header("HX-Redirect", "/login")
                return("")
            }
            redirect(response, "/login")
        },
        fe_backend_error = function(e) {
            set_html_headers(response)
            response$status <- e$status
            alert <- render_error_alert(e, lang, state$translations)
            if (is_htmx_request(request)) {
                return(alert)
            }
            render_page(
                request,
                response,
                content = alert,
                title = tr("Error", lang, state$translations),
                lang = lang,
                state = state,
                user = datastore$session$auth
            )
        }
    )
}

render_error_alert <- function(e, lang, translations) {
    detail <- if (nzchar(e$detail %||% "")) e$detail else e$title
    render_tags(htmltools::div(
        class = "alert alert-danger",
        role = "alert",
        htmltools::strong(paste0(tr("Error", lang, translations), ": ")),
        detail
    ))
}

# Model selection policy: prefer the platform's private llama.cpp router when it
# already has one of the configured aliases LOADED, otherwise fall back to the
# configured remote provider.
#
# The router loads models on demand and a cold load takes minutes, so asking for
# an unloaded alias would hang the first turn; only `status.value == "loaded"`
# entries are eligible. One provider per session: a failed turn is never
# replayed elsewhere; pi's auto-retry may re-ask the SAME provider within the
# turn, and the walltime deadline caps it either way.

CHAT_MODELS_TIMEOUT_SECONDS <- 2

# The choice the NEXT session would make, cached so the header's model tooltip
# can render on every Explore load without re-probing the router each time
# (2s worst case when the router is configured but unreachable). NULL results
# are cached too, for the same reason. A session created within the TTL
# re-runs the real choice; the tooltip is advisory.
CHAT_CHOICE_TTL_SECONDS <- 60

chat_choice_cache <- new.env(parent = emptyenv())

chat_prospective_model <- function(config) {
    now <- as.numeric(Sys.time())
    if (!is.null(chat_choice_cache$at) && now - chat_choice_cache$at < CHAT_CHOICE_TTL_SECONDS) {
        return(chat_choice_cache$choice)
    }
    choice <- tryCatch(chat_choose_model(config), error = function(e) NULL)
    chat_choice_cache$at <- now
    chat_choice_cache$choice <- choice
    choice
}

# `model` is what the wire carries (the router routes on aliases); `display_model`
# is what the tooltip shows: the checkpoint name the router publishes as its
# first `tags` entry (deploy-llm derives it from the HF repo), else the alias.
chat_choose_model <- function(config) {
    chat <- config$chat
    loaded <- chat_router_loaded(chat)
    for (alias in chat$llama_models) {
        if (alias %in% names(loaded)) {
            return(list(provider = chat$llama_provider, model = alias, display_model = loaded[[alias]]))
        }
    }
    if (nzchar(chat$fallback_provider) && nzchar(chat$fallback_model)) {
        return(list(
            provider = chat$fallback_provider,
            model = chat$fallback_model,
            display_model = chat$fallback_model
        ))
    }
    stop(chat_error("No chat model is available right now"))
}

# The models the router reports as loaded, as a named character vector:
# names are the routable ids (aliases), values the display names (first tag,
# falling back to the id). An unreachable or malformed router is not an error
# here - it just means "nothing loaded", and the fallback applies.
chat_router_loaded <- function(chat) {
    if (!nzchar(chat$llama_base_url) || length(chat$llama_models) == 0L) {
        return(character())
    }
    body <- tryCatch(
        {
            req <- httr2::request(paste0(chat$llama_base_url, "/models")) |>
                httr2::req_timeout(CHAT_MODELS_TIMEOUT_SECONDS) |>
                httr2::req_error(is_error = function(resp) FALSE)
            if (nzchar(chat$llama_api_key)) {
                req <- httr2::req_auth_bearer_token(req, chat$llama_api_key)
            }
            resp <- httr2::req_perform(req)
            if (httr2::resp_status(resp) != 200L) {
                return(NULL)
            }
            be_parse_json(resp)
        },
        error = function(e) NULL
    )
    entries <- body$data
    if (!is.list(entries)) {
        return(character())
    }
    info <- vapply(
        entries,
        function(entry) {
            status <- be_scalar(entry$status$value) %||% be_scalar(entry$status) %||% ""
            if (!identical(as.character(status), "loaded")) {
                return(c("", ""))
            }
            id <- as.character(be_scalar(entry$id) %||% "")
            tags <- entry$tags %||% list()
            display <- if (length(tags) > 0L) as.character(be_scalar(tags[[1]]) %||% "") else ""
            c(id, if (nzchar(display)) display else id)
        },
        character(2)
    )
    keep <- nzchar(info[1, ])
    stats::setNames(info[2, keep], info[1, keep])
}

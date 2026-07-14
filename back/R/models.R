# Model fitting and persistence. fit_model_task() is the mirai worker payload:
# fully self-contained (no free variables), takes plain data + a pre-validated
# formula object and returns a plain list, so nothing externalptr-shaped crosses
# the process boundary (spike finding 5). Ported from shiny-base R/300_model_fn.R:
# metrics are computed BEFORE butchering (butcher removes what summary() needs),
# axe_call is skipped so the summary shows the formula, and the model frame is
# dropped manually (no axe_data method for lm).

fit_model_task <- function(data, formula) {
    tryCatch(
        {
            fit <- stats::lm(formula, data = data, na.action = stats::na.exclude)
            fit$call$formula <- formula

            summ <- summary(fit)
            metrics <- list(
                r_squared = summ$r.squared,
                rmse = sqrt(mean(summ$residuals^2, na.rm = TRUE)),
                aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
                summary_text = paste(utils::capture.output(print(summ)), collapse = "\n")
            )

            fit <- butcher::axe_env(fit)
            fit <- butcher::axe_fitted(fit)
            fit$model <- NULL

            list(
                success = TRUE,
                metrics = metrics,
                model_blob = serialize(fit, connection = NULL)
            )
        },
        error = function(e) {
            list(success = FALSE, error = conditionMessage(e))
        }
    )
}

MODEL_COLUMNS <- "id, user_id, dataset_id, formula, metrics, created_at"

# Refitting the same (user, dataset, formula) overwrites the stored model: the
# fit is deterministic, so this keeps the natural idempotency of the unique key.
db_upsert_model <- function(pool, user_id, dataset_id, formula_str, metrics, model_blob) {
    DBI::dbGetQuery(
        pool,
        "INSERT INTO models (user_id, dataset_id, formula, metrics, model_blob)
         VALUES ($1, $2, $3, $4::jsonb, $5)
         ON CONFLICT (user_id, dataset_id, formula)
         DO UPDATE SET metrics = EXCLUDED.metrics, model_blob = EXCLUDED.model_blob,
                       updated_at = now()
         RETURNING id",
        params = list(
            user_id,
            dataset_id,
            formula_str,
            yyjsonr::write_json_str(metrics, auto_unbox = TRUE),
            list(model_blob)
        )
    )$id
}

db_list_models <- function(pool, user_id, dataset_id = NULL, after = NULL, limit = 20L) {
    clauses <- "user_id = $1"
    params <- list(user_id)
    add <- function(clause, value) {
        params[[length(params) + 1]] <<- value
        clauses <<- c(clauses, sprintf(clause, length(params)))
    }
    if (!is.null(dataset_id)) {
        add("dataset_id = $%d", dataset_id)
    }
    if (!is.null(after)) {
        add("id < $%d", after)
    }
    params[[length(params) + 1]] <- limit
    DBI::dbGetQuery(
        pool,
        sprintf(
            "SELECT %s FROM models WHERE %s ORDER BY id DESC LIMIT $%d",
            MODEL_COLUMNS,
            paste(clauses, collapse = " AND "),
            length(params)
        ),
        params = params
    )
}

db_get_model <- function(pool, user_id, model_id) {
    row <- DBI::dbGetQuery(
        pool,
        sprintf("SELECT %s FROM models WHERE id = $1 AND user_id = $2", MODEL_COLUMNS),
        params = list(model_id, user_id)
    )
    if (nrow(row) == 0) NULL else row
}

db_delete_model <- function(pool, user_id, model_id) {
    DBI::dbExecute(
        pool,
        "DELETE FROM models WHERE id = $1 AND user_id = $2",
        params = list(model_id, user_id)
    ) >
        0
}

model_json <- function(row) {
    list(
        id = jsonlite::unbox(as.integer(row$id)),
        dataset_id = jsonlite::unbox(as.integer(row$dataset_id)),
        formula = jsonlite::unbox(row$formula),
        metrics = unbox_scalars(yyjsonr::read_json_str(row$metrics[[1]])),
        created_at = jsonlite::unbox(format(row$created_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    )
}

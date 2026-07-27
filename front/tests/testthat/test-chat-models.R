# Model-selection policy against a webfakes stand-in for the llama.cpp router.
# The router loads models on demand and a cold load takes minutes, so only
# entries it reports as LOADED are eligible; everything else falls back.

router_app <- function(entries, status = 200L, expect_key = NULL) {
    app <- webfakes::new_app()
    app$get("/models", function(req, res) {
        if (!is.null(expect_key) && !identical(req$get_header("Authorization"), paste("Bearer", expect_key))) {
            return(res$set_status(401L)$send_json(list(error = "unauthorized"), auto_unbox = TRUE))
        }
        if (status != 200L) {
            return(res$set_status(status)$send_json(list(error = "down"), auto_unbox = TRUE))
        }
        res$send_json(list(data = entries), auto_unbox = TRUE)
    })
    app
}

local_router <- function(entries, status = 200L, expect_key = NULL, env = parent.frame()) {
    process <- webfakes::local_app_process(router_app(entries, status, expect_key), .local_envir = env)
    sub("/+$", "", process$url())
}

chat_test_config <- function(...) {
    list(
        chat = utils::modifyList(
            list(
                llama_base_url = "",
                llama_api_key = "",
                llama_provider = "llama.cpp",
                llama_models = c("ds4-flash", "qwen3.6-27b"),
                fallback_provider = "openrouter",
                fallback_model = "vendor/model-1"
            ),
            list(...)
        )
    )
}

test_that("the first LOADED alias in configured order wins", {
    url <- local_router(list(
        list(id = "qwen3.6-27b", status = list(value = "loaded")),
        list(id = "ds4-flash", status = list(value = "loaded"))
    ))
    choice <- chat_choose_model(chat_test_config(llama_base_url = url))
    expect_equal(choice, list(provider = "llama.cpp", model = "ds4-flash"))
})

test_that("an alias the router has not loaded is skipped", {
    url <- local_router(list(
        list(id = "ds4-flash", status = list(value = "unloaded")),
        list(id = "qwen3.6-27b", status = list(value = "loaded"))
    ))
    choice <- chat_choose_model(chat_test_config(llama_base_url = url))
    expect_equal(choice$model, "qwen3.6-27b")
})

test_that("nothing loaded falls back to the remote provider", {
    url <- local_router(list(list(id = "ds4-flash", status = list(value = "unloaded"))))
    choice <- chat_choose_model(chat_test_config(llama_base_url = url))
    expect_equal(choice, list(provider = "openrouter", model = "vendor/model-1"))
})

test_that("a router that is down or refuses the key falls back instead of failing", {
    down <- local_router(list(), status = 503L)
    expect_equal(chat_choose_model(chat_test_config(llama_base_url = down))$provider, "openrouter")

    guarded <- local_router(list(list(id = "ds4-flash", status = list(value = "loaded"))), expect_key = "right")
    config <- chat_test_config(llama_base_url = guarded, llama_api_key = "wrong")
    expect_equal(chat_choose_model(config)$provider, "openrouter")
    config$chat$llama_api_key <- "right"
    expect_equal(chat_choose_model(config)$model, "ds4-flash")
})

test_that("an unreachable router does not stall the caller", {
    config <- chat_test_config(llama_base_url = "http://127.0.0.1:1")
    expect_equal(chat_choose_model(config)$provider, "openrouter")
})

test_that("no loaded model and no fallback is a chat error, not a crash", {
    config <- chat_test_config(fallback_provider = "", fallback_model = "")
    expect_error(chat_choose_model(config), class = "fe_chat_error")
    expect_equal(
        tryCatch(chat_choose_model(config), fe_chat_error = function(e) e$key),
        "No chat model is available right now"
    )
})

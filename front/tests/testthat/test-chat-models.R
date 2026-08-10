# Model-selection policy against a webfakes stand-in for the LLM gateway.
# Router-fronted entries carry a status and only LOADED ones are eligible;
# entries without a status (select-mode profiles) count as serving; nothing
# eligible falls back to the remote provider.

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
                llama_models = c("large", "small"),
                fallback_provider = "openrouter",
                fallback_model = "vendor/model-1"
            ),
            list(...)
        )
    )
}

test_that("the first LOADED alias in configured order wins; unloaded aliases are skipped", {
    url <- local_router(list(
        list(id = "small", status = list(value = "loaded")),
        list(id = "large", status = list(value = "loaded"))
    ))
    choice <- chat_choose_model(chat_test_config(llama_base_url = url))
    expect_equal(choice, list(provider = "llama.cpp", model = "large", display_model = "large"))

    partial <- local_router(list(
        list(id = "large", status = list(value = "unloaded")),
        list(id = "small", status = list(value = "loaded"))
    ))
    expect_equal(chat_choose_model(chat_test_config(llama_base_url = partial))$model, "small")
})

test_that("entries without a status field (select-mode profiles) are eligible", {
    url <- local_router(list(
        list(id = "full:deepseek-v4-flash", status = list(value = "loading")),
        list(id = "medium:deepseek-v4-flash")
    ))
    config <- chat_test_config(
        llama_base_url = url,
        llama_models = c("full:deepseek-v4-flash", "medium:deepseek-v4-flash")
    )
    choice <- chat_choose_model(config)
    expect_equal(choice$model, "medium:deepseek-v4-flash")
    expect_equal(choice$display_model, "medium:deepseek-v4-flash")
})

test_that("the router's first tag becomes the display name; the wire model stays the alias", {
    url <- local_router(list(
        list(id = "large", status = list(value = "loaded"), tags = list("DeepSeek-V4-Flash-0731")),
        list(id = "small", status = list(value = "loaded"), tags = list())
    ))
    choice <- chat_choose_model(chat_test_config(llama_base_url = url))
    expect_equal(choice$model, "large")
    expect_equal(choice$display_model, "DeepSeek-V4-Flash-0731")

    untagged <- chat_choose_model(chat_test_config(llama_base_url = url, llama_models = c("small")))
    expect_equal(untagged$display_model, "small")
})

test_that("nothing loaded falls back to the remote provider", {
    url <- local_router(list(list(id = "large", status = list(value = "unloaded"))))
    choice <- chat_choose_model(chat_test_config(llama_base_url = url))
    expect_equal(choice, list(provider = "openrouter", model = "vendor/model-1", display_model = "vendor/model-1"))
})

test_that("a router that is down, unreachable or refuses the key falls back instead of failing", {
    down <- local_router(list(), status = 503L)
    expect_equal(chat_choose_model(chat_test_config(llama_base_url = down))$provider, "openrouter")

    unreachable <- chat_test_config(llama_base_url = "http://127.0.0.1:1")
    expect_equal(chat_choose_model(unreachable)$provider, "openrouter")

    guarded <- local_router(list(list(id = "large", status = list(value = "loaded"))), expect_key = "right")
    config <- chat_test_config(llama_base_url = guarded, llama_api_key = "wrong")
    expect_equal(chat_choose_model(config)$provider, "openrouter")
    config$chat$llama_api_key <- "right"
    expect_equal(chat_choose_model(config)$model, "large")
})

test_that("no loaded model and no fallback is a chat error, not a crash", {
    config <- chat_test_config(fallback_provider = "", fallback_model = "")
    expect_error(chat_choose_model(config), class = "fe_chat_error")
    expect_equal(
        tryCatch(chat_choose_model(config), fe_chat_error = function(e) e$key),
        "No chat model is available right now"
    )
})

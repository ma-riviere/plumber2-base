# Baseline: clear every variable get_config() inspects so each test starts from a
# known state and only sets what it exercises.
clear_config_env <- function() {
    withr::local_envvar(
        c(
            ENVIRONMENT = NA,
            BYPASS_AUTH = NA,
            PORT = NA,
            HOST = NA,
            LOG_LEVEL = NA,
            AUTH0_DOMAIN = NA,
            AUTH0_CLIENT_ID = NA,
            AUTH0_CLIENT_SECRET = NA,
            AUTH0_AUDIENCE = NA,
            AUTH0_CLAIM_NAMESPACE = NA,
            AUTH0_MGMT_CLIENT_ID = NA,
            AUTH0_MGMT_CLIENT_SECRET = NA,
            SESSION_KEY = NA
        ),
        .local_envir = parent.frame()
    )
}

with_auth0 <- function() {
    withr::local_envvar(
        c(
            AUTH0_DOMAIN = "tenant.auth0.com",
            AUTH0_CLIENT_ID = "cid",
            AUTH0_CLIENT_SECRET = "secret",
            AUTH0_AUDIENCE = "https://base-api.example",
            AUTH0_CLAIM_NAMESPACE = "https://plumber-base.example",
            SESSION_KEY = "0123456789abcdef0123456789abcdef"
        ),
        .local_envir = parent.frame()
    )
}

test_that("dev with BYPASS_AUTH allows missing Auth0 values and uses defaults", {
    clear_config_env()
    withr::local_envvar(c(ENVIRONMENT = "dev", BYPASS_AUTH = "true"))

    config <- get_config()
    expect_equal(config$environment, "dev")
    expect_true(config$bypass_auth)
    expect_equal(config$port, 8080L)
    expect_equal(config$host, "0.0.0.0")
})

test_that("PORT is parsed as an integer from the environment", {
    clear_config_env()
    withr::local_envvar(c(ENVIRONMENT = "dev", BYPASS_AUTH = "true", PORT = "9099"))
    expect_equal(get_config()$port, 9099L)
})

test_that("prod with BYPASS_AUTH aborts", {
    clear_config_env()
    with_auth0()
    withr::local_envvar(c(ENVIRONMENT = "prod", BYPASS_AUTH = "true"))
    expect_error(get_config(), "BYPASS_AUTH")
})

test_that("prod with missing Auth0/SESSION_KEY aborts", {
    clear_config_env()
    withr::local_envvar(c(ENVIRONMENT = "prod"))
    expect_error(get_config(), "required configuration")
})

test_that("prod with all secrets present succeeds", {
    clear_config_env()
    with_auth0()
    withr::local_envvar(c(ENVIRONMENT = "prod"))
    config <- get_config()
    expect_equal(config$environment, "prod")
    expect_false(config$bypass_auth)
    expect_equal(config$auth0$audience, "https://base-api.example")
    # The claim namespace is normalized to end with a slash.
    expect_equal(config$auth0$claim_namespace, "https://plumber-base.example/")
})

test_that("dev without BYPASS_AUTH still requires Auth0/SESSION_KEY", {
    clear_config_env()
    withr::local_envvar(c(ENVIRONMENT = "dev", BYPASS_AUTH = "false"))
    expect_error(get_config(), "required configuration")
})

test_that("an unknown ENVIRONMENT is rejected", {
    clear_config_env()
    withr::local_envvar(c(ENVIRONMENT = "staging", BYPASS_AUTH = "true"))
    expect_error(get_config(), "ENVIRONMENT")
})

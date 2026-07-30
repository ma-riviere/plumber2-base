# get_config() env-var parsing and the "never in prod" startup assertions.
# withr::local_envvar keeps each case isolated and restores the ambient env.

test_that("env vars override defaults and are typed", {
    withr::local_envvar(
        ENVIRONMENT = "dev",
        PORT = "9090",
        BYPASS_AUTH = "true",
        RATE_LIMIT_PER_MIN = "30",
        PGPORT = "6000"
    )
    config <- get_config()

    expect_equal(config$port, 9090L)
    expect_true(config$bypass_auth)
    expect_equal(config$rate_limit_per_min, 30L)
    expect_equal(config$db$port, 6000L)
})

test_that("a non-integer numeric env var is rejected", {
    withr::local_envvar(PORT = "not-a-number")
    expect_error(get_config(), "integer")
})

test_that("prod refuses BYPASS_AUTH and every missing required setting", {
    # A complete prod env, broken one knob at a time; the error names the knob.
    prod_env <- c(
        ENVIRONMENT = "prod",
        HOST = "0.0.0.0",
        BYPASS_AUTH = NA,
        AUTH0_DOMAIN = "tenant.eu.auth0.com",
        AUTH0_AUDIENCE = "https://api.example",
        AUTH0_CLAIM_NAMESPACE = "https://api.example"
    )
    cases <- list(
        list(broken = c(BYPASS_AUTH = "true"), error = "BYPASS_AUTH"),
        list(broken = c(AUTH0_AUDIENCE = ""), error = "AUTH0_AUDIENCE"),
        list(broken = c(HOST = NA), error = "HOST"),
        list(broken = c(AUTH0_CLAIM_NAMESPACE = NA), error = "AUTH0_CLAIM_NAMESPACE")
    )
    for (case in cases) {
        env <- prod_env
        env[names(case$broken)] <- case$broken
        withr::with_envvar(env, expect_error(get_config(), case$error))
    }
})

test_that("a fully specified prod config is accepted", {
    withr::local_envvar(
        ENVIRONMENT = "prod",
        HOST = "0.0.0.0",
        BYPASS_AUTH = "false",
        AUTH0_DOMAIN = "tenant.eu.auth0.com",
        AUTH0_AUDIENCE = "https://api.example",
        AUTH0_CLAIM_NAMESPACE = "https://api.example"
    )
    config <- get_config()

    expect_true(config$is_prod)
    expect_equal(config$host, "0.0.0.0")
    expect_false(config$bypass_auth)
    # The namespace is normalized to end with a slash.
    expect_equal(config$auth0$claim_namespace, "https://api.example/")
})

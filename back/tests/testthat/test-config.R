# get_config() env-var parsing and the "never in prod" startup assertions.
# withr::local_envvar keeps each case isolated and restores the ambient env.

test_that("dev defaults are filled in and valid", {
    withr::local_envvar(
        ENVIRONMENT = NA,
        HOST = NA,
        PORT = NA,
        BYPASS_AUTH = NA,
        AUTH0_DOMAIN = NA,
        AUTH0_AUDIENCE = NA,
        RATE_LIMIT_PER_MIN = NA,
        LOG_LEVEL = NA
    )
    config <- get_config()

    expect_equal(config$environment, "dev")
    expect_false(config$is_prod)
    expect_equal(config$host, "127.0.0.1")
    expect_equal(config$port, 8081L)
    expect_false(config$bypass_auth)
    expect_equal(config$rate_limit_per_min, 120L)
    expect_equal(config$db$port, 5433L)
    expect_equal(config$db$user, "plumber_base")
})

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

test_that("an unknown ENVIRONMENT is rejected", {
    withr::local_envvar(ENVIRONMENT = "staging")
    expect_error(get_config(), "dev.+prod|prod.+dev")
})

test_that("prod with BYPASS_AUTH stops", {
    withr::local_envvar(
        ENVIRONMENT = "prod",
        HOST = "0.0.0.0",
        BYPASS_AUTH = "true",
        AUTH0_DOMAIN = "tenant.eu.auth0.com",
        AUTH0_AUDIENCE = "https://api.example"
    )
    expect_error(get_config(), "BYPASS_AUTH")
})

test_that("prod with a missing AUTH0 var stops", {
    withr::local_envvar(
        ENVIRONMENT = "prod",
        HOST = "0.0.0.0",
        BYPASS_AUTH = NA,
        AUTH0_DOMAIN = "tenant.eu.auth0.com",
        AUTH0_AUDIENCE = ""
    )
    expect_error(get_config(), "AUTH0_AUDIENCE")
})

test_that("prod without an explicit HOST stops", {
    withr::local_envvar(
        ENVIRONMENT = "prod",
        HOST = NA,
        BYPASS_AUTH = NA,
        AUTH0_DOMAIN = "tenant.eu.auth0.com",
        AUTH0_AUDIENCE = "https://api.example"
    )
    expect_error(get_config(), "HOST")
})

test_that("prod without a claim namespace stops", {
    withr::local_envvar(
        ENVIRONMENT = "prod",
        HOST = "0.0.0.0",
        BYPASS_AUTH = NA,
        AUTH0_DOMAIN = "tenant.eu.auth0.com",
        AUTH0_AUDIENCE = "https://api.example",
        AUTH0_CLAIM_NAMESPACE = NA
    )
    expect_error(get_config(), "AUTH0_CLAIM_NAMESPACE")
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

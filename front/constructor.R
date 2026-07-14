# _server.yml constructor: all programmatic setup for the front-end service.
# api("_server.yml") sources this file into the global environment BEFORE parsing
# the route files, so the helpers sourced here and the state published via
# api$set_data() are in scope for the route handlers.
#
# Working directory is the front root (where _server.yml lives).

local({
    helpers <- c(
        "assets.R",
        "i18n.R",
        "config.R",
        "session.R",
        "csrf.R",
        "auth0.R",
        "mgmt.R",
        "gate.R",
        "render.R",
        "backend_client.R",
        "ui.R",
        "ui_home.R",
        "ui_explore.R",
        "ui_model.R",
        "ui_admin.R",
        "ui_account.R",
        "ui_profile.R",
        "app.R"
    )
    for (helper in helpers) {
        source(file.path("R", helper))
    }
})

config <- get_config()
configure_jwks(auth0_base_url(config$auth0$domain))

cat(
    sprintf(
        "[front] starting: environment=%s host=%s port=%d bypass_auth=%s\n",
        config$environment,
        config$host,
        config$port,
        config$bypass_auth
    ),
    file = stderr()
)

state <- build_state(config, base_dir = ".")

assemble_api(state, env = globalenv())

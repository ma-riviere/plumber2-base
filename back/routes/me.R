#* Authenticated principal: the backing user record plus the scopes granted to
#* this request's credential. Auth is enforced by the constructor's central
#* /v1/* rule (api_key || jwt, + bypass in dev); paths are written with the full
#* /v1 prefix because the @root annotation is broken (spike finding 7).
#* @get /v1/me
#* @serializer json
function(datastore, response) {
    principal <- request_principal(datastore, response)
    user <- principal$user
    list(
        user = list(
            id = jsonlite::unbox(as.integer(user$id)),
            auth0_sub = jsonlite::unbox(user$auth0_sub),
            email = jsonlite::unbox(user$email),
            nickname = jsonlite::unbox(user$nickname),
            is_guest = jsonlite::unbox(user$is_guest),
            created_at = jsonlite::unbox(format(user$created_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
        ),
        auth = jsonlite::unbox(principal$guard),
        scopes = principal$scopes
    )
}

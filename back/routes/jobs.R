#* Poll a job. While pending/running keep polling (Retry-After hints the
#* cadence); done includes the result (model_id + metrics), error the reason.
#* @param id:string The job id (uuid)
#* @get /v1/jobs/<id:string>
#* @serializer json
function(id, datastore, response) {
    principal <- request_principal(datastore, response)
    if (!is_uuid(id)) {
        reqres::abort_not_found("no such job")
    }
    row <- db_get_job(app_pool(), principal$user_id, id)
    if (is.null(row)) {
        reqres::abort_not_found("no such job")
    }
    if (row$status %in% c("pending", "running")) {
        response$set_header("Retry-After", "2")
    }
    job_json(row)
}

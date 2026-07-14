# dataset row (home and explore contexts) and inline-edit form

    Code
      snap(dataset_row_html(fixture_dataset, "en", snap_translations))
    Output
      <div class="dataset-row" id="dataset-row-1">
        <a class="dataset-row-link clickable" href="/explore?dataset=1">
          <span class="dataset-col dataset-col-name">
            <span class="dataset-name">cars</span>
          </span>
          <span class="dataset-col dataset-col-age">
            <i class="bi bi-calendar-plus" aria-hidden="true">
      </i>
            <span>2026-07-01</span>
          </span>
          <span class="dataset-col dataset-col-size">
            <i class="bi bi-table" aria-hidden="true">
      </i>
            <span>50 rows × 2 cols</span>
          </span>
        </a>
        <div class="dataset-col dataset-col-actions">
          <button type="button" class="btn btn-sm btn-outline-secondary btn-action-dataset" title="Rename Dataset" hx-get="/partials/dataset/1/edit?context=home" hx-target="#dataset-row-1" hx-swap="outerHTML">
            <i class="bi bi-pencil" aria-hidden="true">
      </i>
          </button>
          <a class="btn btn-sm btn-outline-primary btn-action-dataset" title="Download" href="/datasets/1/download">
            <i class="bi bi-download" aria-hidden="true">
      </i>
          </a>
          <button type="button" class="btn btn-sm btn-outline-danger btn-action-dataset" title="Delete" hx-delete="/datasets/1" hx-confirm="Are you sure you want to delete this dataset?" hx-swap="none">
            <i class="bi bi-trash" aria-hidden="true">
      </i>
          </button>
        </div>
      </div>

---

    Code
      snap(dataset_row_html(fixture_dataset, "en", snap_translations, context = "explore"))
    Output
      <div class="dataset-row" id="dataset-row-1">
        <div class="dataset-row-link">
          <span class="dataset-col dataset-col-name">
            <span class="dataset-name">cars</span>
          </span>
          <span class="dataset-col dataset-col-age">
            <i class="bi bi-calendar-plus" aria-hidden="true">
      </i>
            <span>2026-07-01</span>
          </span>
          <span class="dataset-col dataset-col-size">
            <i class="bi bi-table" aria-hidden="true">
      </i>
            <span>50 rows × 2 cols</span>
          </span>
        </div>
        <div class="dataset-col dataset-col-actions">
          <button type="button" class="btn btn-sm btn-outline-secondary btn-action-dataset" title="Rename Dataset" hx-get="/partials/dataset/1/edit?context=explore" hx-target="#dataset-row-1" hx-swap="outerHTML">
            <i class="bi bi-pencil" aria-hidden="true">
      </i>
          </button>
          <a class="btn btn-sm btn-outline-primary btn-action-dataset" title="Download" href="/datasets/1/download">
            <i class="bi bi-download" aria-hidden="true">
      </i>
          </a>
          <button type="button" class="btn btn-sm btn-outline-danger btn-action-dataset" title="Delete" hx-delete="/datasets/1?context=explore" hx-confirm="Are you sure you want to delete this dataset?" hx-swap="none">
            <i class="bi bi-trash" aria-hidden="true">
      </i>
          </button>
        </div>
      </div>

---

    Code
      snap(dataset_row_edit_html(fixture_dataset, "en", snap_translations, error = "Nope"))
    Output
      <form class="dataset-row d-flex align-items-center gap-2" id="dataset-row-1" hx-patch="/datasets/1" hx-target="this" hx-swap="outerHTML">
        <div class="flex-grow-1">
          <input type="text" class="form-control form-control-sm is-invalid" name="name" id="dataset-name-1" value="cars" placeholder="Enter new dataset name" aria-label="New Name"/>
          <div class="invalid-feedback">Nope</div>
        </div>
        <button type="submit" class="btn btn-sm btn-primary">Rename</button>
        <button type="button" class="btn btn-sm btn-outline-secondary" hx-get="/partials/dataset/1/row?context=home" hx-target="#dataset-row-1" hx-swap="outerHTML">Cancel</button>
      </form>

# home data panel (empty state)

    Code
      snap(home_data_panel(list(), "en", snap_translations))
    Output
      <div id="home-data" hx-get="/partials/home/datasets" hx-trigger="fb:refresh-datasets from:body" hx-include="#home-filters" hx-swap="outerHTML">
        <div class="card stat-card mb-4">
          <div class="card-body d-flex align-items-center gap-3">
            <i class="bi bi-database fs-2 text-primary" aria-hidden="true">
      </i>
            <div>
              <span class="display-6 d-block" id="dataset-count">0</span>
              <span class="text-muted">Datasets</span>
            </div>
          </div>
        </div>
        <div class="card datasets-card">
          <div class="card-header d-flex justify-content-between align-items-center">
            <h3 class="h5 mb-0">Your Datasets</h3>
            <button type="button" class="btn btn-primary btn-sm" data-bs-toggle="modal" data-bs-target="#upload-modal">
              <i class="bi bi-upload me-1" aria-hidden="true">
      </i>
              Upload Dataset
            </button>
          </div>
          <div class="card-body">
            <div class="empty-state text-center text-muted py-5">
              <i class="bi bi-folder2-open fs-1 d-block mb-3" aria-hidden="true">
      </i>
              <p>No datasets match the current filter</p>
            </div>
          </div>
        </div>
      </div>

# explore preview with pagination state

    Code
      snap(preview_html(1L, preview, "en", snap_translations))
    Output
      <div id="preview">
        <div class="table-responsive">
          <table class="table table-sm table-striped table-hover align-middle">
            <thead>
              <tr>
                <th>speed</th>
                <th>dist</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>4</td>
                <td>2</td>
              </tr>
              <tr>
                <td>4</td>
                <td>10</td>
              </tr>
            </tbody>
          </table>
        </div>
        <div class="d-flex flex-wrap align-items-center justify-content-between gap-2">
          <span class="text-muted small">11-12 / 50</span>
          <nav aria-label="Data preview pages">
            <ul class="pagination pagination-sm mb-0">
              <li class="page-item">
                <button type="button" class="page-link" hx-get="/partials/explore/preview" hx-vals="{&quot;dataset&quot;: 1, &quot;offset&quot;: 0}" hx-target="#preview" hx-swap="outerHTML" hx-sync="#preview:replace">Previous</button>
              </li>
              <li class="page-item">
                <button type="button" class="page-link" hx-get="/partials/explore/preview" hx-vals="{&quot;dataset&quot;: 1, &quot;offset&quot;: 0}" hx-target="#preview" hx-swap="outerHTML" hx-sync="#preview:replace">1</button>
              </li>
              <li class="page-item active" aria-current="page">
                <button type="button" class="page-link" hx-get="/partials/explore/preview" hx-vals="{&quot;dataset&quot;: 1, &quot;offset&quot;: 10}" hx-target="#preview" hx-swap="outerHTML" hx-sync="#preview:replace">2</button>
              </li>
              <li class="page-item">
                <button type="button" class="page-link" hx-get="/partials/explore/preview" hx-vals="{&quot;dataset&quot;: 1, &quot;offset&quot;: 20}" hx-target="#preview" hx-swap="outerHTML" hx-sync="#preview:replace">3</button>
              </li>
              <li class="page-item disabled">
                <span class="page-link">…</span>
              </li>
              <li class="page-item">
                <button type="button" class="page-link" hx-get="/partials/explore/preview" hx-vals="{&quot;dataset&quot;: 1, &quot;offset&quot;: 40}" hx-target="#preview" hx-swap="outerHTML" hx-sync="#preview:replace">5</button>
              </li>
              <li class="page-item">
                <button type="button" class="page-link" hx-get="/partials/explore/preview" hx-vals="{&quot;dataset&quot;: 1, &quot;offset&quot;: 20}" hx-target="#preview" hx-swap="outerHTML" hx-sync="#preview:replace">Next</button>
              </li>
            </ul>
          </nav>
        </div>
      </div>

# model polling and result fragments

    Code
      snap(job_polling_fragment("job-1", 1L, "en", snap_translations))
    Output
      <div class="d-flex align-items-center gap-2 text-muted" hx-get="/partials/model/job/job-1?dataset=1&amp;model=" hx-trigger="load delay:2s" hx-target="this" hx-swap="outerHTML" hx-sync="#page-body:drop">
        <div class="spinner-border spinner-border-sm" role="status">
      </div>
        <span>Fitting model...</span>
      </div>

---

    Code
      snap(model_result_fragment(fixture_model, "en", snap_translations))
    Output
      <div class="card">
        <div class="card-body">
          <h5 class="card-title mb-3">Model Summary</h5>
          <div class="row g-3 mb-3">
            <div class="col-md-4">
              <div class="metric-card p-3 border rounded">
                <small class="text-muted">R-squared</small>
                <div class="h4 mb-0">0.6511</div>
              </div>
            </div>
            <div class="col-md-4">
              <div class="metric-card p-3 border rounded">
                <small class="text-muted">RMSE</small>
                <div class="h4 mb-0">15.07</div>
              </div>
            </div>
            <div class="col-md-4">
              <div class="metric-card p-3 border rounded">
                <small class="text-muted">AIC</small>
                <div class="h4 mb-0">419.2</div>
              </div>
            </div>
          </div>
          <pre class="border rounded p-3 bg-body-tertiary small mb-0">Call:
      lm(formula = dist ~ speed, data = data)</pre>
        </div>
      </div>

# model toolbar states (idle, active model, fitting)

    Code
      snap(model_toolbar_html(1L, "en", snap_translations))
    Output
      <div id="model-toolbar" class="d-flex align-items-center gap-2">
        <button type="submit" form="fit-form" class="btn btn-sm btn-primary">
          <i class="bi bi-play-fill me-1" aria-hidden="true">
      </i>
          Fit
        </button>
        <button type="button" class="btn btn-sm btn-outline-danger" disabled title="Delete model">
          <i class="bi bi-trash me-1" aria-hidden="true">
      </i>
          Delete
        </button>
      </div>

---

    Code
      snap(model_toolbar_html(1L, "en", snap_translations, active_model_id = 7L, oob = TRUE))
    Output
      <div id="model-toolbar" class="d-flex align-items-center gap-2" hx-swap-oob="true">
        <button type="submit" form="fit-form" class="btn btn-sm btn-primary">
          <i class="bi bi-play-fill me-1" aria-hidden="true">
      </i>
          Fit
        </button>
        <button type="button" class="btn btn-sm btn-outline-danger" title="Delete model" hx-delete="/models/7" hx-vals="{&quot;dataset&quot;: 1, &quot;model&quot;: 7}" hx-confirm="Are you sure?" hx-swap="none">
          <i class="bi bi-trash me-1" aria-hidden="true">
      </i>
          Delete
        </button>
      </div>

---

    Code
      snap(model_toolbar_html(1L, "en", snap_translations, fitting = TRUE))
    Output
      <div id="model-toolbar" class="d-flex align-items-center gap-2">
        <button type="submit" form="fit-form" class="btn btn-sm btn-primary" disabled>
          <i class="bi bi-play-fill me-1" aria-hidden="true">
      </i>
          Fit
        </button>
        <button type="button" class="btn btn-sm btn-outline-danger" disabled title="Delete model">
          <i class="bi bi-trash me-1" aria-hidden="true">
      </i>
          Delete
        </button>
      </div>

# saved models sidebar (active highlight, empty oob variant)

    Code
      snap(saved_models_html(list(fixture_model), 1L, "en", snap_translations,
      active_model_id = 7L))
    Output
      <div id="saved-models">
        <div class="mb-4">
          <h6 class="text-uppercase text-muted fw-semibold mb-3">Saved Models</h6>
          <div class="model-picker">
            <div class="model-picker-row selected">
              <button type="button" class="model-picker-select" title="dist ~ speed" hx-get="/partials/model/saved/7" hx-target="#fit-status" hx-swap="innerHTML" hx-sync="#page-body:drop">
                <span class="model-picker-formula">dist ~ speed</span>
              </button>
              <button type="button" class="model-picker-delete" title="Delete model" hx-delete="/models/7" hx-vals="{&quot;dataset&quot;: 1, &quot;model&quot;: 7}" hx-confirm="Are you sure?" hx-swap="none">
                <i class="bi bi-trash" aria-hidden="true">
      </i>
              </button>
            </div>
          </div>
        </div>
      </div>

---

    Code
      snap(saved_models_html(list(), 1L, "en", snap_translations, oob = TRUE))
    Output
      <div id="saved-models" hx-swap-oob="true">
        <div class="mb-4">
          <h6 class="text-uppercase text-muted fw-semibold mb-3">Saved Models</h6>
          <p class="text-muted small fst-italic mb-0">No saved models for this dataset</p>
        </div>
      </div>

# api keys table and one-time secret alert

    Code
      snap(keys_table_html(keys, "en", snap_translations))
    Output
      <div id="keys-table">
        <div class="table-responsive">
          <table class="table table-sm align-middle">
            <thead>
              <tr>
                <th>Name</th>
                <th>Prefix</th>
                <th>Scopes</th>
                <th>Created</th>
                <th>Last used</th>
                <th>Expires</th>
                <th>
      </th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>ci</td>
                <td>
                  <code>pbk_abcd...</code>
                </td>
                <td>write:datasets</td>
                <td>2026-07-01</td>
                <td>Never</td>
                <td>Never</td>
                <td>
                  <button type="button" class="btn btn-sm btn-outline-danger" hx-delete="/keys/9" hx-confirm="Are you sure you want to revoke this key?" hx-swap="none">Revoke</button>
                </td>
              </tr>
              <tr class="text-muted">
                <td>old</td>
                <td>
                  <code>pbk_dead...</code>
                </td>
                <td>
      </td>
                <td>2026-06-01</td>
                <td>2026-07-01</td>
                <td>Never</td>
                <td>
                  <span class="badge text-bg-secondary">Revoked</span>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>

---

    Code
      snap(key_created_html(created, "en", snap_translations))
    Output
      <div class="key-created" role="alert">
        <div class="key-created-title">
          <i class="bi bi-check-circle-fill" aria-hidden="true">
      </i>
          This key is shown only once. Copy it now.
        </div>
        <div class="key-created-secret">
          <code class="key-created-code">pbk_secret</code>
          <button type="button" class="btn btn-sm btn-outline-secondary flex-shrink-0" data-clipboard-text="pbk_secret" data-clipboard-message="Copied to clipboard">
            <i class="bi bi-clipboard me-1" aria-hidden="true">
      </i>
            Copy
          </button>
        </div>
      </div>

# profile modal content

    Code
      snap(profile_modal_content(auth, "en", snap_translations))
    Output
      <div class="modal fade" id="profile-modal" tabindex="-1" aria-hidden="true">
        <div class="modal-dialog">
          <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title">Profile</h5>
              <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close">
      </button>
            </div>
            <div class="modal-body">
              <form id="profile-form" hx-post="/profile" hx-target="#profile-status" hx-swap="innerHTML">
                <div class="text-center mb-3">
                  <img src="https://cdn.example.test/p.png" class="rounded-circle" width="96" height="96" alt="Profile picture"/>
                </div>
                <div class="mb-3">
                  <label class="form-label">Email</label>
                  <div class="form-control-plaintext">user@example.test</div>
                </div>
                <div class="mb-3">
                  <label class="form-label">Roles</label>
                  <div class="form-control-plaintext">user</div>
                </div>
                <div class="mb-3">
                  <label class="form-label" for="profile-nickname">Nickname</label>
                  <input type="text" class="form-control" id="profile-nickname" name="nickname" value="tester"/>
                </div>
                <div class="mb-3">
                  <label class="form-label" for="profile-language">Preferred Language</label>
                  <select class="form-select" id="profile-language" name="language">
                    <option value="en" selected>English</option>
                    <option value="fr">French</option>
                  </select>
                </div>
                <div id="profile-status">
      </div>
                <div class="d-flex justify-content-end gap-2">
                  <button type="button" class="btn btn-outline-secondary" data-bs-dismiss="modal">Cancel</button>
                  <button type="submit" class="btn btn-primary">Save</button>
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>

# admin requests table

    Code
      snap(admin_content("requests", list(items = items), 24L, "all", "en",
      snap_translations))
    Output
      <div id="page-body">
        <div class="page-header mb-4">
          <h1>Admin</h1>
        </div>
        <ul class="nav nav-tabs mb-4">
          <li class="nav-item">
            <a class="nav-link" href="/admin?tab=users">Users</a>
          </li>
          <li class="nav-item">
            <a class="nav-link active" href="/admin?tab=requests">Requests</a>
          </li>
        </ul>
        <div class="card">
          <div class="card-body">
            <div class="d-flex gap-2 mb-3">
              <a class="btn btn-sm btn-secondary" href="/admin?tab=requests&amp;hours=24">Last 24 hours</a>
              <a class="btn btn-sm btn-outline-secondary" href="/admin?tab=requests&amp;hours=168">Last 7 days</a>
              <a class="btn btn-sm btn-outline-secondary" href="/admin?tab=requests&amp;hours=720">Last 30 days</a>
            </div>
            <div class="table-responsive">
              <table class="table table-sm table-hover align-middle">
                <thead>
                  <tr>
                    <th>Service</th>
                    <th>Method</th>
                    <th>Path</th>
                    <th>Status</th>
                    <th>Count</th>
                    <th>Avg ms</th>
                    <th>Max ms</th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td>back</td>
                    <td>GET</td>
                    <td>
                      <code>/v1/datasets</code>
                    </td>
                    <td>200</td>
                    <td>42</td>
                    <td>12.5</td>
                    <td>40</td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>

# admin user card and role modal

    Code
      snap(admin_user_card_html(user, "en", snap_translations, can_manage_roles = TRUE))
    Output
      <div class="card admin-user-card h-100" id="admin-user-2">
        <div class="card-body d-flex gap-3">
          <i class="bi bi-person-fill admin-user-avatar" aria-hidden="true">
      </i>
          <div class="flex-grow-1 overflow-hidden">
            <div class="d-flex justify-content-between align-items-start gap-2">
              <div class="overflow-hidden">
                <div class="fw-semibold text-truncate">dev@example.com</div>
                <div class="text-muted small text-truncate">dev</div>
              </div>
              <button type="button" class="btn btn-sm btn-outline-secondary" title="Edit role" hx-get="/partials/admin/users/2/role" hx-target="#modal-slot" hx-swap="innerHTML">
                <i class="bi bi-person-gear" aria-hidden="true">
      </i>
              </button>
            </div>
            <div class="mt-1">
              <span class="badge me-1 text-bg-secondary">dev</span>
            </div>
            <div class="mt-2">
              <span class="text-muted small me-3" title="Datasets">
                <i class="bi bi-folder2-open me-1" aria-hidden="true">
      </i>
                3
              </span>
              <span class="text-muted small me-3" title="Models">
                <i class="bi bi-graph-up me-1" aria-hidden="true">
      </i>
                1
              </span>
              <span class="text-muted small me-3" title="API Keys">
                <i class="bi bi-key me-1" aria-hidden="true">
      </i>
                0
              </span>
            </div>
            <div class="text-muted small mt-1">
              <i class="bi bi-clock me-1" aria-hidden="true">
      </i>
              Last seen 2026-07-05
            </div>
          </div>
        </div>
      </div>

---

    Code
      snap(render_tags(admin_role_modal_html(user, roles, "user", "en",
        snap_translations)))
    Output
      <div class="modal fade" id="role-modal" tabindex="-1" aria-hidden="true">
        <div class="modal-dialog">
          <div class="modal-content">
            <div class="modal-header">
              <h5 class="modal-title">Change role: dev@example.com</h5>
              <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close">
      </button>
            </div>
            <div class="modal-body">
              <form hx-put="/admin/users/2/role" hx-target="#role-modal-status" hx-swap="innerHTML">
                <div class="mb-3">
                  <label class="form-label" for="role-select">Role</label>
                  <select class="form-select" id="role-select" name="role_id">
                    <option value="">user (default)</option>
                    <option value="rol_admin">admin</option>
                    <option value="rol_dev" selected>dev</option>
                    <option value="rol_beta">beta (no scopes mapped)</option>
                  </select>
                </div>
                <p class="text-muted small">Role changes apply on the next token refresh (up to 15 minutes).</p>
                <div id="role-modal-status" class="mb-3">
      </div>
                <div class="d-flex justify-content-end gap-2">
                  <button type="button" class="btn btn-outline-secondary" data-bs-dismiss="modal">Cancel</button>
                  <button type="submit" class="btn btn-primary">Save</button>
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>


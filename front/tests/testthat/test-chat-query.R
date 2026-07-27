# The agent's `query` tool hands model-written SQL to the DuckDB CLI, so the
# security boundary IS the invocation: trusted settings, then `.safe_mode`, on a
# read-only database, in a disposable child process. These tests run the real
# binary with exactly the arguments .pi/extensions/dataset-chat.ts uses - the two
# must be kept in sync, and a DuckDB upgrade that loosens any of this fails here.

# Mirrors DUCKDB_SETTINGS in the extension. The order matters: `.safe_mode` locks
# configuration, so our own tuning has to be applied before it.
duckdb_settings <- paste(
    "SET threads = 1;",
    "SET memory_limit = '128MB';",
    "SET max_temp_directory_size = '64MB';",
    "SET preserve_insertion_order = false;",
    "SET autoinstall_known_extensions = false;",
    "SET autoload_known_extensions = false;",
    "SET allow_community_extensions = false;",
    "SET allow_unsigned_extensions = false;",
    "SET allow_persistent_secrets = false;",
    "SET allow_unredacted_secrets = false;",
    "SET allowed_directories = [];",
    "SET allowed_paths = [];",
    "SET allowed_configs = [];",
    sep = "\n"
)

duckdb_args <- function(db = "data.duckdb") {
    c("-readonly", "-no-init", "-batch", "-bail", "-jsonlines", "-cmd", duckdb_settings, "-cmd", ".safe_mode", db)
}

# The wrapper's maxBuffer (MAX_TOOL_OUTPUT * 4): beyond this the child is killed
# and the model is told to aggregate instead.
query_max_output_bytes <- 131072L

local_chat_workdir <- function(csv = NULL, env = parent.frame()) {
    skip_if_no_duckdb()
    workdir <- withr::local_tempdir(.local_envir = env)
    if (is.null(csv)) {
        utils::write.csv(
            data.frame(
                speed = c(4, 7, 8, 12, 15, 20),
                dist = c(2, 4, 16, 24, 54, 64),
                grade = c("a", "b", "a", "b", "a", "c"),
                stringsAsFactors = FALSE
            ),
            file.path(workdir, "dataset.csv"),
            row.names = FALSE
        )
    } else {
        writeLines(csv, file.path(workdir, "dataset.csv"), useBytes = TRUE)
    }
    chat_build_duckdb(workdir, "duckdb")
    workdir
}

# processx::run redirects stdin from a FILE, so the model's SQL is staged the way
# the extension writes it to the child's stdin.
run_query <- function(workdir, sql, env = parent.frame()) {
    sql_file <- withr::local_tempfile(.local_envir = env)
    writeLines(sql, sql_file, useBytes = TRUE)
    result <- processx::run(
        Sys.which("duckdb")[[1]],
        duckdb_args(),
        wd = workdir,
        stdin = sql_file,
        timeout = 60,
        error_on_status = FALSE,
        env = c(HOME = workdir, TMPDIR = workdir, LANG = "C.UTF-8", LC_ALL = "C.UTF-8")
    )
    list(status = result$status, out = result$stdout, err = result$stderr)
}

test_that("the ingest step builds a database the query tool can read", {
    workdir <- local_chat_workdir()
    expect_true(file.exists(file.path(workdir, "data.duckdb")))
    expect_false(file.exists(file.path(workdir, "data.duckdb.part")))

    described <- run_query(workdir, "DESCRIBE dataset;")
    expect_equal(described$status, 0L)
    expect_match(described$out, '"column_name":"speed"', fixed = TRUE)
    expect_match(described$out, '"column_name":"grade"', fixed = TRUE)

    counted <- run_query(workdir, "SELECT count(*) AS n FROM dataset;")
    expect_equal(counted$status, 0L)
    expect_match(counted$out, '"n":6', fixed = TRUE)
})

test_that("an ingest failure raises the chat error the widget renders as a notice", {
    skip_if_no_duckdb()
    workdir <- withr::local_tempdir()
    writeLines("a,b\n1,2", file.path(workdir, "dataset.csv"))
    expect_error(chat_build_duckdb(workdir, "duckdb-not-installed"), class = "fe_chat_error")
    expect_false(file.exists(file.path(workdir, "data.duckdb")))
    expect_false(file.exists(file.path(workdir, "data.duckdb.part")))
})

test_that("the questions the old analyze operations answered are plain SQL now", {
    workdir <- local_chat_workdir()

    aggregate <- run_query(
        workdir,
        "SELECT count(*) AS n, avg(speed) AS mean_speed, max(dist) AS max_dist FROM dataset;"
    )
    expect_equal(aggregate$status, 0L)
    expect_match(aggregate$out, '"mean_speed":11.0', fixed = TRUE)

    grouped <- run_query(workdir, "SELECT grade, count(*) AS n FROM dataset GROUP BY grade ORDER BY grade;")
    expect_equal(grouped$status, 0L)
    expect_match(grouped$out, '{"grade":"a","n":3}', fixed = TRUE)

    quantiles <- run_query(workdir, "SELECT quantile_cont(speed, [0.25, 0.5, 0.75]) AS q FROM dataset;")
    expect_equal(quantiles$status, 0L)
    expect_match(quantiles$out, '"q":[', fixed = TRUE)

    filtered <- run_query(workdir, "SELECT count(*) AS n FROM dataset WHERE speed > 10;")
    expect_equal(filtered$status, 0L)
    expect_match(filtered$out, '"n":3', fixed = TRUE)

    correlation <- run_query(workdir, "SELECT corr(speed, dist) AS r FROM dataset;")
    expect_equal(correlation$status, 0L)
    expect_match(correlation$out, '"r":0.9', fixed = FALSE)

    # Several statements per call are allowed; -bail stops at the first error.
    multiple <- run_query(workdir, "SELECT 1 AS a;\nSELECT nosuchcolumn;\nSELECT 3 AS c;")
    expect_equal(multiple$status, 1L)
    expect_match(multiple$out, '"a":1', fixed = TRUE)
    expect_false(grepl('"c":3', multiple$out, fixed = TRUE))
})

test_that("every route out of the database file is refused", {
    workdir <- local_chat_workdir()
    escapes <- c(
        "SELECT * FROM read_csv('/etc/passwd');",
        "SELECT * FROM read_text('/etc/passwd');",
        "SELECT * FROM glob('/etc/*');",
        "ATTACH '/tmp/elsewhere.duckdb' AS other;",
        "COPY (SELECT 1) TO '/tmp/chat-query-escape.csv';",
        "SELECT getenv('OPENROUTER_API_KEY') AS key;",
        "INSTALL httpfs;",
        "LOAD httpfs;",
        "SELECT * FROM read_csv('https://example.com/data.csv');"
    )
    for (sql in escapes) {
        result <- run_query(workdir, sql)
        expect_equal(result$status, 1L, info = sql)
        expect_match(result$err, "Permission Error", fixed = TRUE, info = sql)
        expect_equal(result$out, "", info = sql)
    }
    expect_false(file.exists("/tmp/chat-query-escape.csv"))
})

test_that("the locked configuration cannot be reopened from model SQL", {
    workdir <- local_chat_workdir()
    for (sql in c(
        "SET memory_limit = '8GB';",
        "PRAGMA memory_limit = '8GB';",
        "RESET memory_limit;",
        "SET allowed_directories = ['/etc'];",
        "SET enable_external_access = true;"
    )) {
        result <- run_query(workdir, sql)
        expect_equal(result$status, 1L, info = sql)
        expect_match(result$err, "configuration has been locked", fixed = TRUE, info = sql)
    }
    # Reading the settings back stays allowed: nothing sensitive lives in them.
    applied <- run_query(workdir, "SELECT current_setting('threads') AS threads;")
    expect_equal(applied$status, 0L)
    expect_match(applied$out, '"threads":1', fixed = TRUE)
})

test_that("the database is read-only while scratch computation still works", {
    workdir <- local_chat_workdir()
    for (sql in c(
        "CREATE TABLE injected AS SELECT 1 AS x;",
        "INSERT INTO dataset VALUES (1, 1, 'z');",
        "DROP TABLE dataset;",
        "UPDATE dataset SET speed = 0;"
    )) {
        result <- run_query(workdir, sql)
        expect_equal(result$status, 1L, info = sql)
        expect_match(result$err, "read-only mode", fixed = TRUE, info = sql)
    }
    # Temp tables and spill stay available: they die with the child process.
    scratch <- run_query(
        workdir,
        "CREATE TEMP TABLE t AS SELECT * FROM dataset WHERE speed > 5; SELECT count(*) AS n FROM t;"
    )
    expect_equal(scratch$status, 0L)
    expect_match(scratch$out, '"n":5', fixed = TRUE)
})

test_that("an init file cannot run before the lockdown is applied", {
    workdir <- local_chat_workdir()
    init <- file.path(workdir, "planted.sql")
    writeLines(".print INIT_FILE_RAN", init)
    duckdb <- Sys.which("duckdb")[[1]]

    # `-safe`/`.safe_mode` do NOT block an init file: `-no-init` is what does.
    unsafe <- processx::run(
        duckdb,
        c("-safe", "-init", init, "-batch", "-bail", "-c", "SELECT 1;", "data.duckdb"),
        wd = workdir,
        error_on_status = FALSE
    )
    expect_match(unsafe$stdout, "INIT_FILE_RAN", fixed = TRUE)

    neutralized <- processx::run(
        duckdb,
        c("-no-init", "-init", init, "-batch", "-bail", "-c", "SELECT 1;", "data.duckdb"),
        wd = workdir,
        error_on_status = FALSE
    )
    expect_false(grepl("INIT_FILE_RAN", neutralized$stdout, fixed = TRUE))
})

test_that("safe mode stops the host-affecting dot commands but not all of them", {
    workdir <- local_chat_workdir()
    for (dot in c(".shell echo pwned", ".system echo pwned", ".read /etc/passwd", ".import /etc/passwd t")) {
        result <- run_query(workdir, dot)
        expect_match(paste(result$out, result$err), "cannot be used in -safe mode", fixed = TRUE, info = dot)
    }
    # `.dump` and `.print` survive safe mode, which is exactly why the extension
    # refuses every model line starting with a dot before spawning the child.
    dumped <- run_query(workdir, ".dump")
    expect_match(dumped$out, "CREATE TABLE dataset", fixed = TRUE)
})

test_that("a sort too big for memory spills to the workdir, and the temp cap holds", {
    workdir <- local_chat_workdir()
    # 1.5M padded rows exceed the 128MB memory limit (the same query fails with
    # the temp cap set to 0), so this exercises the spill path end to end.
    sorted <- run_query(
        workdir,
        "SELECT count(*) AS n FROM (SELECT x, repeat('y', 60) AS pad FROM range(1500000) t(x) ORDER BY pad, x DESC) s;"
    )
    expect_equal(sorted$status, 0L)
    expect_match(sorted$out, '"n":1500000', fixed = TRUE)
    # Nothing survives the child: no stray temp directory next to the database.
    expect_setequal(list.files(workdir), c("dataset.csv", "data.duckdb"))

    # A runaway sort stops at the cap instead of filling the host disk.
    runaway <- run_query(
        workdir,
        "SELECT count(*) AS n FROM (SELECT x, repeat('y', 60) AS pad FROM range(3000000) t(x) ORDER BY pad, x DESC) s;"
    )
    expect_equal(runaway$status, 1L)
    expect_match(runaway$err, "max_temp_directory_size", fixed = TRUE)
    expect_setequal(list.files(workdir), c("dataset.csv", "data.duckdb"))
})

test_that("an unbounded result outgrows the wrapper's output cap while an aggregate does not", {
    workdir <- local_chat_workdir()
    wide <- run_query(workdir, "SELECT x AS id, repeat('label-', 5) || x AS label FROM range(5000) t(x);")
    expect_equal(wide$status, 0L)
    expect_gt(nchar(wide$out, type = "bytes"), query_max_output_bytes)

    bounded <- run_query(workdir, "SELECT count(*) AS n, avg(x) AS mean FROM range(5000) t(x);")
    expect_lt(nchar(bounded$out, type = "bytes"), query_max_output_bytes)
})

test_that("control bytes are escaped in results but echoed raw in errors", {
    control <- intToUtf8(1L)
    workdir <- local_chat_workdir(csv = c("label,note", paste0("café,", control, "bell"), "naïve,plain"))

    rows <- run_query(workdir, "SELECT * FROM dataset ORDER BY label;")
    expect_equal(rows$status, 0L)
    expect_match(rows$out, "café", fixed = TRUE)
    # The JSON writer escapes a control byte coming out of the data itself.
    expect_match(rows$out, "\\u0001bell", fixed = TRUE)
    expect_false(grepl(control, rows$out, fixed = TRUE))

    # Error text is NOT escaped: it echoes the offending SQL byte for byte, which
    # is why the extension strips control characters before returning it.
    failed <- run_query(workdir, paste0("SELECT nosuch", control, "col FROM dataset;"))
    expect_equal(failed$status, 1L)
    expect_true(grepl(control, failed$err, fixed = TRUE))
})

test_that("DuckDB errors come back verbatim on stderr, with nothing on stdout", {
    workdir <- local_chat_workdir()
    binder <- run_query(workdir, "SELECT nosuchcolumn FROM dataset;")
    expect_equal(binder$status, 1L)
    expect_equal(binder$out, "")
    expect_match(binder$err, "Binder Error", fixed = TRUE)
    expect_match(binder$err, "nosuchcolumn", fixed = TRUE)

    catalog <- run_query(workdir, "SELECT * FROM nosuchtable;")
    expect_equal(catalog$status, 1L)
    expect_match(catalog$err, "Catalog Error", fixed = TRUE)

    parser <- run_query(workdir, "SELEC 1;")
    expect_equal(parser$status, 1L)
    expect_match(parser$err, "Parser Error", fixed = TRUE)
})

test_that("the spawn environment hands the agent a database and no R toolchain", {
    withr::local_envvar(c(OPENROUTER_API_KEY = "provider-key", SESSION_KEY = "must-not-leak"))
    state <- list(
        base_dir = normalizePath(dirname(r_dir)),
        config = list(chat = list(duckdb_bin = "duckdb", websearch = FALSE))
    )
    session <- list(workdir = withr::local_tempdir())

    env <- chat_spawn_env(state, session)
    expect_equal(unname(env[["CHAT_DUCKDB_DB"]]), file.path(session$workdir, "data.duckdb"))
    expect_true(nzchar(env[["CHAT_DUCKDB_BIN"]]))
    expect_equal(unname(env[["OPENROUTER_API_KEY"]]), "provider-key")
    expect_false(any(c("CHAT_ANALYZE_SCRIPT", "CHAT_RSCRIPT", "R_LIBS_SITE", "SESSION_KEY") %in% names(env)))
})

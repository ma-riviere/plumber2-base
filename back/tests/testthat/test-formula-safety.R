# Formula AST allowlist: raw user formulas are code execution, so these
# malicious payloads are the definition of done. validate_formula must refuse
# anything that is not
# columns + whitelisted operators/functions, and the returned formula's
# environment must contain nothing beyond the whitelist.

columns <- c("mpg", "wt", "hp", "cyl")

test_that("legitimate formulas validate and fit", {
    f <- validate_formula("mpg ~ wt + hp", columns)
    expect_s3_class(f, "formula")
    fit <- lm(f, data = mtcars)
    expect_s3_class(fit, "lm")

    for (good in c(
        "mpg ~ wt",
        "mpg ~ wt + hp + cyl",
        "mpg ~ wt * hp",
        "mpg ~ wt:hp",
        "mpg ~ I(wt^2) + log(hp)",
        "mpg ~ poly(wt, 2)",
        "mpg ~ sqrt(hp)",
        "log(mpg) ~ wt",
        "mpg ~ wt - 1",
        "mpg ~ (wt + hp)"
    )) {
        expect_s3_class(validate_formula(good, columns), "formula")
    }
})

test_that("malicious and malformed formulas are rejected", {
    reject <- function(bad, pattern) expect_error(validate_formula(bad, columns), pattern)

    reject("mpg ~ system('id')", "disallowed")
    reject("mpg ~ wt + system('rm -rf /')", "disallowed")
    reject("mpg ~ base::system('id')", "disallowed")
    reject("mpg ~ eval(parse(text='1'))", "disallowed")
    reject("mpg ~ .Internal(getwd())", "disallowed")
    reject("mpg ~ wt[1]", "disallowed")
    reject("mpg ~ wt$x", "disallowed")
    reject("mpg ~ {wt}", "disallowed")
    reject("mpg ~ if (TRUE) wt else hp", "disallowed")
    reject("mpg ~ function(x) x", "disallowed")
    reject("mpg ~ (function() 1)()", "disallowed")
    reject("mpg ~ 'string'", "disallowed element")
    reject("mpg ~ unknown_col", "unknown variable")
    reject("~ wt", "two-sided")
    reject("mpg ~ wt; system('id')", "cannot be parsed")
    reject("", "non-empty")
    reject(paste0("mpg ~ ", paste(rep("wt", 400), collapse = " + ")), "too long")
    reject("mpg <- wt", "two-sided")
    reject("mpg ~ assign('x', 1)", "disallowed")
    reject("mpg ~ Sys.getenv()", "disallowed")
})

test_that("the formula environment binds only whitelisted functions over base", {
    f <- validate_formula("mpg ~ log(wt)", columns)
    env <- environment(f)
    expect_setequal(ls(env, all.names = TRUE), FORMULA_ALLOWED_CALLS)
    # Parent is baseenv (model.frame's predvars needs base `list`); package
    # functions and globals must NOT be resolvable from the formula.
    expect_identical(parent.env(env), baseenv())
    expect_error(get("app_pool", envir = env), "not found")
    expect_error(get("glm", envir = env), "not found") # stats not reachable
})

test_that("fit_model_task fits, reports metrics and butchers the model", {
    f <- validate_formula("mpg ~ wt + hp", columns)
    result <- fit_model_task(mtcars, f)

    expect_true(result$success)
    expect_gt(result$metrics$r_squared, 0.7)
    expect_true(is.numeric(result$metrics$rmse))
    expect_true(is.numeric(result$metrics$aic))
    expect_match(result$metrics$summary_text, "Coefficients")
    expect_type(result$model_blob, "raw")

    fit <- unserialize(result$model_blob)
    expect_s3_class(fit, "lm")
    expect_null(fit$model) # frame dropped for storage
    # predict() still works on the butchered model (restore path used by the FE).
    expect_length(predict(fit, newdata = head(mtcars, 3)), 3L)
})

test_that("fit_model_task reports failures as plain data", {
    f <- validate_formula("mpg ~ wt", c("mpg", "wt"))
    result <- fit_model_task(data.frame(mpg = 1, wt = 2), f) # 1 row: lm cannot fit
    # lm() with 1 observation fits with NA coefficients rather than erroring;
    # use an empty frame to force a hard error.
    result <- fit_model_task(data.frame(mpg = numeric(), wt = numeric()), f)
    expect_false(result$success)
    expect_true(nzchar(result$error))
})

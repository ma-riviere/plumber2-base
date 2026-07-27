# Markdown rendering + sanitizer. Model output is untrusted (and with web
# search in the loop, partly attacker-authored), so these cover the tag
# allowlist, attribute stripping and link-scheme filtering rather than looks.

test_that("ordinary markdown renders to the allowed tags", {
    html <- chat_render_markdown("A **bold** claim with `code`.\n\n- one\n- two\n")
    expect_match(html, "<strong>bold</strong>", fixed = TRUE)
    expect_match(html, "<code>code</code>", fixed = TRUE)
    expect_match(html, "<li>one</li>", fixed = TRUE)
})

test_that("raw HTML in model output never survives", {
    html <- chat_render_markdown('Hi <script>alert(1)</script> and <iframe src="x"></iframe> done')
    expect_false(grepl("<script", html, fixed = TRUE))
    expect_false(grepl("<iframe", html, fixed = TRUE))
    # The disallowed TAGS go; their text content stays as inert text.
    expect_match(html, "alert(1)", fixed = TRUE)
})

test_that("event-handler and style attributes are stripped from allowed tags", {
    html <- chat_sanitize_html('<p onclick="steal()" style="position:fixed">x</p>')
    expect_equal(html, "<p>x</p>")
})

test_that("images are removed entirely", {
    html <- chat_render_markdown("![alt](https://example.com/tracker.png)")
    expect_false(grepl("<img", html, fixed = TRUE))
    expect_false(grepl("tracker.png", html, fixed = TRUE))
})

test_that("only http/https links keep their href", {
    safe <- chat_render_markdown("[ok](https://example.com/a?b=1)")
    expect_match(safe, 'href="https://example.com/a?b=1"', fixed = TRUE)
    expect_match(safe, 'rel="nofollow noopener noreferrer"', fixed = TRUE)

    for (bad in c("javascript:alert(1)", "data:text/html;base64,x", "vbscript:x", "/local/path")) {
        html <- chat_render_markdown(sprintf("[click](%s)", bad))
        expect_equal(html, "<p><a>click</a></p>\n", info = bad)
    }
})

test_that("an attribute quote holding '>' cannot smuggle a handler into a tag", {
    # The scanner splits at the first '>', so the tail becomes inert TEXT. What
    # matters is that no surviving TAG carries the handler.
    html <- chat_sanitize_html('<a href="x" alt=">" onerror="alert(1)">t</a>')
    tags <- regmatches(html, gregexpr("<[^>]*>", html))[[1]]
    expect_false(any(grepl("onerror", tags, fixed = TRUE)))
    expect_setequal(tags, c("<a>", "</a>"))
})

test_that("comments, doctypes and processing instructions are dropped", {
    expect_equal(chat_sanitize_html("<!-- hi --><p>x</p><!DOCTYPE html><?php ?>"), "<p>x</p>")
})

test_that("the streaming buffer is escaped, never parsed", {
    escaped <- chat_escape_text("<b>not bold</b> & <script>")
    expect_false(grepl("<b>", escaped, fixed = TRUE))
    expect_match(escaped, "&lt;b&gt;", fixed = TRUE)
    expect_match(escaped, "&amp;", fixed = TRUE)
})

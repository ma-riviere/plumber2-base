# Server-side markdown rendering for settled assistant answers.
#
# Model output is untrusted (and, with web search in the loop, partly
# attacker-authored). commonmark has no safe mode - it passes raw HTML through
# verbatim and emits `javascript:` hrefs unchanged - so rendering is a two-step:
# commonmark, then a rebuild-from-scratch allowlist over every tag. Disallowed
# tags are DROPPED (their text content survives as inert text) and allowed tags
# are re-emitted with no attributes at all except a scheme-validated href, so no
# attribute an attacker writes can ever reach the DOM.
#
# While a turn is still streaming the buffer is shown escaped and unparsed
# (chat_escape_text): half-received markdown would render as broken markup, and
# re-rendering per poll would be wasted work on the single R thread.

CHAT_ALLOWED_TAGS <- c(
    "p",
    "br",
    "hr",
    "strong",
    "em",
    "b",
    "i",
    "del",
    "code",
    "pre",
    "ul",
    "ol",
    "li",
    "blockquote",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "table",
    "thead",
    "tbody",
    "tr",
    "th",
    "td"
)

# GFM extensions, minus `tasklist` (its checkbox <input> is dropped by the
# allowlist, which would leave the item with no state marker at all - as plain
# text the `[x]` / `[ ]` prefix at least survives) and minus `footnotes` (its
# anchors would be stripped to bare <a>, leaving dangling markers).
CHAT_MARKDOWN_EXTENSIONS <- c("table", "strikethrough", "autolink", "tagfilter")

chat_render_markdown <- function(text) {
    html <- commonmark::markdown_html(
        text,
        hardbreaks = TRUE,
        smart = FALSE,
        extensions = CHAT_MARKDOWN_EXTENSIONS
    )
    chat_sanitize_html(html)
}

chat_sanitize_html <- function(html) {
    matches <- gregexpr("<[^>]*>", html, perl = TRUE)
    tags <- regmatches(html, matches)[[1]]
    if (length(tags) == 0L) {
        return(html)
    }
    regmatches(html, matches) <- list(vapply(tags, chat_clean_tag, character(1), USE.NAMES = FALSE))
    html
}

# Rebuild one tag from its name alone. Anything that is not a plain open/close
# tag with an allowlisted name (comments, doctypes, processing instructions,
# <script>, <img>, ...) collapses to the empty string.
chat_clean_tag <- function(tag) {
    parts <- regmatches(tag, regexec("^</?([A-Za-z][A-Za-z0-9]*)", tag))[[1]]
    if (length(parts) != 2L) {
        return("")
    }
    name <- tolower(parts[2])
    closing <- startsWith(tag, "</")
    if (name == "a") {
        return(if (closing) "</a>" else chat_clean_anchor(tag))
    }
    if (!name %in% CHAT_ALLOWED_TAGS) {
        return("")
    }
    if (closing) sprintf("</%s>", name) else sprintf("<%s>", name)
}

# http/https only, anchored so an entity- or scheme-smuggled destination
# (`javascript:`, `data:`, `vbscript:`) can never match. A rejected link keeps
# its text but loses the anchor's navigation.
chat_clean_anchor <- function(tag) {
    href <- regmatches(tag, regexec("href\\s*=\\s*[\"']([^\"']*)[\"']", tag, ignore.case = TRUE))[[1]]
    if (length(href) != 2L || !grepl("^https?://[^[:space:]<>\"']+$", href[2], ignore.case = TRUE)) {
        return("<a>")
    }
    sprintf(
        '<a href="%s" rel="nofollow noopener noreferrer" target="_blank">',
        htmltools::htmlEscape(href[2], attribute = TRUE)
    )
}

# Escaped, unparsed text for the streaming buffer (the widget CSS renders it
# with white-space: pre-wrap, so newlines survive without <br> injection).
chat_escape_text <- function(text) {
    htmltools::htmlEscape(text %||% "")
}

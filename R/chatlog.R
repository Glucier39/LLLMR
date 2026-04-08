# Path for persistent chat log (NDJSON — one JSON object per line, easy to append)
.lllmr_chatlog_file <- path.expand("~/.lllmr_chatlog.ndjson")

#' Append a completed exchange to the persistent chat log.
#' Uses NDJSON so appending never requires reading the full file.
#' @keywords internal
append_chat_log <- function(user_msg, assistant_msg, model, provider, session_id) {
  entry <- list(
    session   = session_id,
    time      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    model     = model,
    provider  = provider,
    user      = user_msg,
    assistant = assistant_msg
  )
  tryCatch(
    cat(jsonlite::toJSON(entry, auto_unbox = TRUE), "\n",
        file = .lllmr_chatlog_file, append = TRUE, sep = ""),
    error = function(e) NULL
  )
}

#' Read all chat log entries as a list.
#' @keywords internal
read_chat_log <- function() {
  if (!file.exists(.lllmr_chatlog_file)) return(list())
  lines <- tryCatch(readLines(.lllmr_chatlog_file, warn = FALSE),
                    error = function(e) character(0))
  lines <- lines[nzchar(trimws(lines))]
  entries <- lapply(lines, function(l) {
    tryCatch(jsonlite::fromJSON(l, simplifyVector = TRUE), error = function(e) NULL)
  })
  Filter(Negate(is.null), entries)
}

#' View lllmr Chat History
#'
#' Displays past lllmr conversations from the persistent chat log stored at
#' \code{~/.lllmr_chatlog.ndjson}. The log survives R session restarts and is
#' never committed to git (it lives outside any project directory).
#'
#' @param n Integer. Number of most-recent sessions to display. Default 5.
#' @param search Character. Optional keyword to filter messages. Only exchanges
#'   containing the term (in the user or assistant turn) are shown.
#'
#' @return Invisibly returns the full list of matching log entries.
#' @export
lllmr_history <- function(n = 5, search = NULL) {
  entries <- read_chat_log()

  if (length(entries) == 0) {
    message("No chat history found at ", .lllmr_chatlog_file)
    return(invisible(list()))
  }

  # Optionally filter by keyword
  if (!is.null(search) && nzchar(search)) {
    pat <- tolower(search)
    entries <- Filter(function(e) {
      grepl(pat, tolower(e$user),      fixed = TRUE) ||
      grepl(pat, tolower(e$assistant), fixed = TRUE)
    }, entries)
    if (length(entries) == 0) {
      message("No messages found matching: \"", search, "\"")
      return(invisible(list()))
    }
  }

  # Group into sessions and show the last n
  sessions <- unique(vapply(entries, `[[`, character(1), "session"))
  show     <- utils::tail(sessions, n)

  for (sess in show) {
    sess_entries <- Filter(function(e) identical(e$session, sess), entries)
    first        <- sess_entries[[1]]
    model_str    <- paste0(first$model, " (", first$provider, ")")

    cat("\n", strrep("\u2500", 64), "\n", sep = "")
    cat(" ", sess, "  \u2502  ", model_str, "\n", sep = "")
    cat(strrep("\u2500", 64), "\n", sep = "")

    for (e in sess_entries) {
      ts <- if (!is.null(e$time) && nchar(e$time) >= 19) substr(e$time, 12, 19) else ""

      cat("\n\033[1m[", ts, "] You:\033[0m\n", sep = "")
      cat(strwrap(e$user, width = 70, indent = 2, exdent = 2), sep = "\n")

      cat("\n\033[1m[", ts, "] lllmr:\033[0m\n", sep = "")
      preview <- if (nchar(e$assistant) > 400)
        paste0(substr(e$assistant, 1, 400), "  \u2026 [truncated]")
      else e$assistant
      cat(strwrap(preview, width = 70, indent = 2, exdent = 2), sep = "\n")
    }
  }
  cat("\n")

  hidden <- length(sessions) - length(show)
  if (hidden > 0)
    message(hidden, " older session(s) hidden. Use lllmr_history(n = ",
            length(sessions), ") to see all.")

  invisible(entries)
}

#' Build a markdown document from a conversation list.
#' @keywords internal
session_to_markdown <- function(conversation, model, provider, session_id) {
  header <- paste0(
    "# lllmr Chat Export\n\n",
    "| | |\n|---|---|\n",
    "| **Session** | ", session_id, " |\n",
    "| **Model** | ", model, " (", provider, ") |\n",
    "| **Exported** | ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " |\n\n",
    "---\n\n"
  )

  body <- paste(vapply(conversation, function(msg) {
    if (identical(msg$role, "user")) {
      paste0("**You**\n\n> ", gsub("\n", "\n> ", trimws(msg$content)), "\n")
    } else if (identical(msg$role, "assistant")) {
      paste0("**lllmr**\n\n", trimws(msg$content), "\n")
    } else {
      ""
    }
  }, character(1)), collapse = "\n---\n\n")

  paste0(header, body)
}

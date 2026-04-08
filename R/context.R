#' Gather R Session Context
#'
#' Collects information about the current R session for injection into the
#' LLM system prompt. Includes active file, selection, environment objects,
#' console history, and project structure.
#'
#' @return A named list of context components.
#' @keywords internal
gather_context <- function() {
  ctx <- list(
    active_file = get_active_file(),
    selection = get_selection(),
    environment = get_environment_summary(),
    console_history = get_console_history(),
    project_files = get_project_files(),
    working_directory = getwd(),
    r_version = paste0(R.version$major, ".", R.version$minor),
    platform = R.version$platform
  )

  ctx$summary <- build_context_summary(ctx)
  ctx
}


#' Build a System Prompt with R Context
#'
#' Constructs a system prompt that includes R session context.
#'
#' @param use_context Logical. Whether to include R session context.
#' @return A character string system prompt.
#' @keywords internal
build_system_prompt <- function(use_context = TRUE, max_file_chars = 50000L, ctx = NULL) {
  base_prompt <- paste0(
    "You are lllmr, a helpful R programming assistant running inside RStudio. ",
    "You help with R code, data analysis, debugging, and general programming questions.\n\n",
    "The user's active R file will be included at the top of their message inside an ",
    "[Active File: filename] block. Always use that file as the context for their question ",
    "— never ask them to paste code that is already provided in that block.\n\n",
    "Guidelines:\n",
    "- Be concise and direct — answer the question without preamble\n",
    "- Write clear, idiomatic R code\n",
    "- Use tidyverse style when appropriate but don't force it\n",
    "- Wrap code in fenced code blocks with language tags (```r)\n",
    "- Reference the user's actual code and data when visible\n",
    "- When fixing bugs, explain what was wrong and why the fix works\n"
  )

  if (!use_context) return(base_prompt)

  if (is.null(ctx)) ctx <- tryCatch(.lllmr_env$cached_context, error = function(e) NULL)
  if (is.null(ctx)) return(base_prompt)

  context_block <- "\n--- R SESSION CONTEXT ---\n"

  file_content <- resolve_file_content(ctx$active_file)
  if (max_file_chars > 0L && nzchar(file_content)) {
    context_block <- paste0(context_block,
      "\n[Active File: ", ctx$active_file$path, "]\n",
      "```\n", truncate_string(file_content, max_file_chars), "\n```\n"
    )
  }

  if (!is.null(ctx$selection) && nzchar(ctx$selection)) {
    context_block <- paste0(context_block,
      "\n[Selected Code]\n```\n", ctx$selection, "\n```\n"
    )
  }

  if (length(ctx$environment) > 0) {
    env_lines <- vapply(ctx$environment, function(obj) {
      paste0("  ", obj$name, " : ", obj$type, obj$detail)
    }, character(1))
    context_block <- paste0(context_block,
      "\n[Global Environment]\n", paste(env_lines, collapse = "\n"), "\n"
    )
  }

  if (length(ctx$console_history) > 0) {
    context_block <- paste0(context_block,
      "\n[Recent Console History]\n",
      paste(utils::tail(ctx$console_history, 5), collapse = "\n"), "\n"
    )
  }

  context_block <- paste0(context_block,
    "\n[R ", ctx$r_version, " | ", ctx$working_directory, "]\n",
    "--- END CONTEXT ---\n"
  )

  paste0(base_prompt, context_block)
}


# -- Individual context gatherers ----------------------------------------------

#' @keywords internal
get_active_file <- function() {
  tryCatch({
    if (!rstudioapi::isAvailable()) return(NULL)
    doc <- rstudioapi::getSourceEditorContext()
    if (is.null(doc)) return(NULL)
    content <- paste(doc$contents, collapse = "\n")
    if (!nzchar(trimws(content))) return(NULL)
    has_path <- !is.null(doc$path) && nzchar(doc$path)
    list(
      path      = if (has_path) basename(doc$path) else "(unsaved)",
      full_path = if (has_path) doc$path else "",
      content   = content   # always push content — disk read is preferred but this is the fallback
    )
  }, error = function(e) NULL)
}

#' @keywords internal
get_selection <- function() {
  tryCatch({
    if (!rstudioapi::isAvailable()) return(NULL)
    doc <- rstudioapi::getSourceEditorContext()
    sel <- doc$selection[[1]]$text
    if (is.null(sel) || !nzchar(trimws(sel))) return(NULL)
    sel
  }, error = function(e) NULL)
}

#' @keywords internal
get_environment_summary <- function() {
  objs <- ls(envir = .GlobalEnv)
  if (length(objs) == 0) return(list())
  
  lapply(utils::head(objs, 50), function(nm) {
    obj <- tryCatch(get(nm, envir = .GlobalEnv), error = function(e) NULL)
    if (is.null(obj)) {
      return(list(name = nm, type = "unknown", detail = ""))
    }
    
    type <- paste(class(obj), collapse = "/")
    detail <- ""
    
    if (is.data.frame(obj)) {
      detail <- paste0(
        " [", nrow(obj), " x ", ncol(obj), "]",
        " cols: ", paste(utils::head(names(obj), 8), collapse = ", ")
      )
    } else if (is.matrix(obj) || is.array(obj)) {
      detail <- paste0(" [", paste(dim(obj), collapse = " x "), "]")
    } else if (is.vector(obj) && !is.list(obj)) {
      detail <- paste0(" [length ", length(obj), "]")
    } else if (is.function(obj)) {
      args <- names(formals(obj))
      detail <- paste0("(", paste(utils::head(args, 5), collapse = ", "), ")")
    } else if (is.list(obj)) {
      detail <- paste0(" [", length(obj), " elements]")
    }
    
    list(name = nm, type = type, detail = detail)
  })
}

#' @keywords internal
get_console_history <- function() {
  tryCatch({
    tmp <- tempfile()
    utils::savehistory(tmp)
    lines <- readLines(tmp, warn = FALSE)
    unlink(tmp)
    utils::tail(lines, 10)
  }, error = function(e) character(0))
}

#' @keywords internal
get_project_files <- function() {
  tryCatch({
    root <- if (rstudioapi::isAvailable()) {
      proj <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
      if (!is.null(proj)) proj else getwd()
    } else {
      getwd()
    }
    # Non-recursive listing only — recursive scans can block for seconds on
    # large projects and there's no meaningful timeout we can safely apply.
    files <- list.files(root, recursive = FALSE, full.names = FALSE)
    files <- files[!grepl("^(\\.git|\\.Rproj\\.user|renv|node_modules|\\.cache)$", files)]
    utils::head(files, 30)
  }, error = function(e) character(0))
}


# -- Helpers -------------------------------------------------------------------

#' Resolve the active file's text content with line numbers.
#' Reads from disk when a saved path is available (bypasses JSON round-trip
#' issues for large files). Falls back to the cached content for unsaved files.
#' Line numbers are included so the model can reference specific lines.
#' @keywords internal
resolve_file_content <- function(active_file) {
  if (is.null(active_file)) return("")

  lines <- NULL

  # Try reading from disk — preferred over cached content as it is always
  # current and avoids JSON round-trip corruption of large strings.
  fp <- if (!is.null(active_file$full_path)) active_file$full_path else ""
  if (nzchar(fp)) {
    fp <- path.expand(fp)   # resolve ~ prefixes
    if (file.exists(fp)) {
      lines <- tryCatch(readLines(fp, warn = FALSE), error = function(e) NULL)
    }
  }

  # Fall back to content that was cached in the push payload (unsaved buffers).
  if (is.null(lines) || length(lines) == 0L) {
    cached <- if (!is.null(active_file$content)) active_file$content else ""
    lines  <- if (nzchar(cached)) strsplit(cached, "\n", fixed = TRUE)[[1L]]
               else character(0L)
  }

  if (length(lines) == 0L) return("")

  # Prefix each line with its number so the model can cite specific lines.
  width    <- nchar(as.character(length(lines)))
  numbered <- paste0(formatC(seq_along(lines), width = width), ": ", lines)
  paste(numbered, collapse = "\n")
}

#' Build a compact context block to inject directly into a user message.
#' Used for providers whose models don't reliably consume system prompts.
#' @param include_env Whether to include global environment objects.
#'   For Ollama, skip these — they add tokens without helping with code questions.
#' @keywords internal
build_context_injection <- function(ctx, max_file_chars = 30000L, include_env = FALSE) {
  if (is.null(ctx)) return("")
  out <- ""

  file_content <- resolve_file_content(ctx$active_file)
  if (nzchar(file_content)) {
    out <- paste0(out,
                  "[Active File: ", ctx$active_file$path, "]\n",
                  "```r\n", truncate_string(file_content, max_file_chars), "\n```\n\n")
  }

  if (!is.null(ctx$selection) && nzchar(ctx$selection)) {
    out <- paste0(out,
                  "[Selected Code]\n",
                  "```r\n", ctx$selection, "\n```\n\n")
  }

  if (include_env && length(ctx$environment) > 0) {
    env_lines <- vapply(ctx$environment, function(obj) {
      paste0("  ", obj$name, " : ", obj$type, obj$detail)
    }, character(1))
    out <- paste0(out, "[Global Environment]\n",
                  paste(env_lines, collapse = "\n"), "\n\n")
  }

  out
}

#' @keywords internal
truncate_string <- function(s, max_chars = 50000) {
  if (nchar(s) <= max_chars) return(s)
  sub_s   <- substr(s, 1L, max_chars)
  last_nl <- max(gregexpr("\n", sub_s, fixed = TRUE)[[1L]])
  if (last_nl > 0L) sub_s <- substr(sub_s, 1L, last_nl - 1L)
  paste0(sub_s, "\n... [truncated — file continues beyond this point]")
}

#' Build a short human-readable context summary for the UI
#' @keywords internal
build_context_summary <- function(ctx) {
  parts <- character(0)
  
  if (!is.null(ctx$active_file)) {
    parts <- c(parts, paste0("File: ", ctx$active_file$path))
  }
  if (!is.null(ctx$selection)) {
    lines <- length(strsplit(ctx$selection, "\n")[[1]])
    parts <- c(parts, paste0(lines, " lines selected"))
  }
  n_env <- length(ctx$environment)
  if (n_env > 0) {
    parts <- c(parts, paste0(n_env, " objects in env"))
  }
  n_hist <- length(ctx$console_history)
  if (n_hist > 0) {
    parts <- c(parts, paste0(n_hist, " history lines"))
  }
  
  if (length(parts) == 0) {
    wd <- if (!is.null(ctx$working_directory) && nzchar(ctx$working_directory))
      basename(ctx$working_directory) else NULL
    return(paste0(
      if (!is.null(wd)) paste0("Dir: ", wd, "  |  ") else "",
      "R ", ctx$r_version
    ))
  }
  paste(parts, collapse = "  |  ")
}

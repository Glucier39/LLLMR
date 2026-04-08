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
build_system_prompt <- function(use_context = TRUE) {
  base_prompt <- paste0(
    "You are lllmr, a helpful R programming assistant running locally inside ",
    "RStudio. You are powered by a local LLM via Ollama. You help with R code, ",
    "data analysis, debugging, and general programming questions.\n\n",
    "Guidelines:\n",
    "- Write clear, idiomatic R code\n",
    "- Use tidyverse style when appropriate but don't force it\n",
    "- Wrap code in fenced code blocks with language tags (```r)\n",
    "- Be concise but thorough\n",
    "- If you can see the user's code or data, reference it specifically\n",
    "- When suggesting fixes, explain what was wrong and why your fix works\n"
  )
  
  if (!use_context) return(base_prompt)
  
  ctx <- tryCatch(gather_context(), error = function(e) NULL)
  if (is.null(ctx)) return(base_prompt)
  
  context_block <- "\n--- R SESSION CONTEXT ---\n"
  
  if (!is.null(ctx$active_file) && nzchar(ctx$active_file$content)) {
    context_block <- paste0(context_block,
                            "\n[Active File: ", ctx$active_file$path, "]\n",
                            "```\n", truncate_string(ctx$active_file$content, 3000), "\n```\n"
    )
  }
  
  if (!is.null(ctx$selection) && nzchar(ctx$selection)) {
    context_block <- paste0(context_block,
                            "\n[Selected Code]\n",
                            "```\n", ctx$selection, "\n```\n"
    )
  }
  
  if (length(ctx$environment) > 0) {
    env_lines <- vapply(ctx$environment, function(obj) {
      paste0("  ", obj$name, " : ", obj$type, obj$detail)
    }, character(1))
    context_block <- paste0(context_block,
                            "\n[Global Environment]\n",
                            paste(env_lines, collapse = "\n"), "\n"
    )
  }
  
  if (length(ctx$console_history) > 0) {
    context_block <- paste0(context_block,
                            "\n[Recent Console History]\n",
                            paste(utils::tail(ctx$console_history, 20), collapse = "\n"), "\n"
    )
  }
  
  if (length(ctx$project_files) > 0) {
    context_block <- paste0(context_block,
                            "\n[Project Files]\n",
                            paste(utils::head(ctx$project_files, 30), collapse = "\n"), "\n"
    )
  }
  
  context_block <- paste0(context_block,
                          "\n[R ", ctx$r_version, " on ", ctx$platform, "]\n",
                          "\n--- END CONTEXT ---\n"
  )
  
  paste0(base_prompt, context_block)
}


# -- Individual context gatherers ----------------------------------------------

#' @keywords internal
get_active_file <- function() {
  tryCatch({
    if (!rstudioapi::isAvailable()) return(NULL)
    doc <- rstudioapi::getSourceEditorContext()
    if (is.null(doc) || is.null(doc$path)) return(NULL)
    list(
      path = basename(doc$path),
      full_path = doc$path,
      content = paste(doc$contents, collapse = "\n")
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
    utils::tail(lines, 30)
  }, error = function(e) character(0))
}

#' @keywords internal
get_project_files <- function() {
  tryCatch({
    if (rstudioapi::isAvailable()) {
      proj <- rstudioapi::getActiveProject()
      if (!is.null(proj)) {
        files <- list.files(proj, recursive = TRUE, full.names = FALSE)
        files <- files[!grepl("(\\.Rproj\\.user|/\\.git|/renv)/", files)]
        files <- files[!grepl("node_modules/", files)]
        return(utils::head(files, 50))
      }
    }
    files <- list.files(getwd(), recursive = TRUE, full.names = FALSE)
    files <- files[!grepl("(\\.git|renv)/|node_modules/", files)]
    utils::head(files, 50)
  }, error = function(e) character(0))
}


# -- Helpers -------------------------------------------------------------------

#' @keywords internal
truncate_string <- function(s, max_chars = 3000) {
  if (nchar(s) <= max_chars) return(s)
  paste0(substr(s, 1, max_chars), "\n... [truncated]")
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
  
  if (length(parts) == 0) return("No context available")
  paste(parts, collapse = "  |  ")
}

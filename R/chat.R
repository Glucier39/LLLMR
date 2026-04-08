#' Launch the lllmr Chat Interface
#'
#' Opens a modern chat interface in the RStudio Viewer pane (or browser)
#' connected to a local Ollama LLM. Zero API keys, zero accounts required.
#'
#' @param model Character. Ollama model to use. Defaults to "llama3.2".
#' @param port Integer. Port for the local httpuv server. Defaults to a random
#'   available port.
#' @param host Character. Host to bind the server to. Defaults to "127.0.0.1".
#' @param launch Logical. Whether to open the chat in the Viewer pane
#'   automatically. Defaults to TRUE.
#'
#' @return Invisibly returns the server handle.
#'
#' @examples
#' \dontrun{
#' lllmr_chat()
#' lllmr_chat(model = "codellama")
#' lllmr_chat(port = 8080)
#' }
#'
#' @export
lllmr_chat <- function(model = NULL,
                       port = NULL,
                       host = "127.0.0.1",
                       launch = TRUE,
                       ollama_url = NULL) {

  # Resolve Ollama URL: argument > env var > default localhost
  if (is.null(ollama_url)) {
    ollama_url <- Sys.getenv("LLLMR_OLLAMA_URL", "http://localhost:11434")
  }
  ollama_url <- sub("/$", "", ollama_url)   # strip trailing slash
  .lllmr_env$ollama_url <- ollama_url

  # Check Ollama connectivity
  ollama_ok <- lllmr_status(quiet = TRUE)
  if (!ollama_ok) {
    message("\n",
            "--- lllmr --------------------------------------------------\n",
            " Ollama is not running or not reachable at:\n",
            "   ", ollama_url, "\n",
            " \n",
            " Local setup:\n",
            "   1. Install Ollama:  https://ollama.com\n",
            "   2. Pull a model:    ollama pull llama3.2\n",
            " Remote setup:\n",
            "   lllmr_chat(ollama_url = \"http://<remote-ip>:11434\")\n",
            "   (or SSH tunnel: ssh -L 11434:localhost:11434 user@remote)\n",
            "------------------------------------------------------------\n")
    return(invisible(NULL))
  }

  # Resolve model — auto-select first available if not specified
  if (is.null(model)) {
    model <- tryCatch({
      resp <- httr2::request(paste0(ollama_url, "/api/tags")) |>
        httr2::req_perform()
      body <- httr2::resp_body_json(resp)
      if (length(body$models) == 0) NULL else body$models[[1]]$name
    }, error = function(e) NULL)
  }
  if (is.null(model) || !nzchar(model)) {
    message("No Ollama models found. Pull one first: ollama pull llama3.2")
    return(invisible(NULL))
  }

  # Pick a port
  if (is.null(port)) {
    port <- httpuv::randomPort(min = 7500, max = 8500)
  }
  
  # Stop any previously running lllmr server
  stop_existing_server()

  # Resolve the frontend assets directory
  app_dir <- system.file("app", package = "lllmr")
  if (app_dir == "") {
    app_dir <- file.path(getwd(), "inst", "app")
  }

  # Launch server in a background process so RStudio's R session stays free.
  # The blocking httpuv service loop runs in the child; the parent returns
  # immediately and the console remains usable.
  server_proc <- callr::r_bg(
    func = function(app_dir, model, host, port, ollama_url) {
      lllmr:::run_server(app_dir = app_dir, model = model,
                         host = host, port = port, ollama_url = ollama_url)
    },
    args = list(app_dir = app_dir, model = model, host = host,
                port = port, ollama_url = ollama_url),
    package = TRUE
  )

  # Store process handle for cleanup and write lockfile for cross-session cleanup
  .lllmr_env$server_proc <- server_proc
  .lllmr_env$port <- port
  writeLines(as.character(server_proc$get_pid()), .lllmr_lockfile)

  # Give the background process a moment to bind the port before opening the UI
  Sys.sleep(1)

  # Helper to snapshot current context and persist it to the shared file.
  # Called synchronously once now, then periodically via later for updates.
  flush_context <- function(ctx) {
    if (is.null(ctx)) return(invisible(NULL))
    ctx_lite <- ctx
    # For SAVED files: strip content — server reads directly from disk (avoids
    # JSON round-trip issues and keeps the file small).
    # For UNSAVED files: keep content — there is no disk path to fall back on.
    af <- ctx_lite$active_file
    if (!is.null(af) && !is.null(af$full_path) && nzchar(af$full_path)) {
      ctx_lite$active_file$content <- NULL
    }
    tmp <- paste0(.lllmr_context_file, ".tmp")
    tryCatch({
      writeLines(jsonlite::toJSON(ctx_lite, auto_unbox = TRUE, null = "null"), tmp)
      file.rename(tmp, .lllmr_context_file)
    }, error = function(e) NULL)
  }

  # Write context synchronously NOW so the file exists before the UI opens.
  # This eliminates the race where the user sends a message before the first
  # later() callback fires.
  flush_context(tryCatch(gather_context(), error = function(e) NULL))

  # Keep refreshing every 3 s so switching files / changing selection updates.
  write_context <- function() {
    proc <- .lllmr_env$server_proc
    if (is.null(proc) || !proc$is_alive()) return()
    flush_context(tryCatch(gather_context(), error = function(e) NULL))
    later::later(write_context, delay = 3)
  }
  later::later(write_context, delay = 3)

  url <- paste0("http://", host, ":", port)

  message("\n",
          "--- lllmr --------------------------------------------------\n",
          " Chat server running at: ", url, "\n",
          " Model: ", model, "\n",
          " \n",
          " Stop with:  lllmr:::stop_existing_server()\n",
          "------------------------------------------------------------\n")

  # Launch in Viewer or browser
  if (launch) {
    if (rstudioapi::isAvailable()) {
      rstudioapi::viewer(url)
    } else {
      utils::browseURL(url)
    }
  }

  invisible(server_proc)
}


#' List Available Ollama Models
#'
#' Queries the local Ollama instance for installed models.
#'
#' @return A data frame of installed models, or NULL if Ollama is unreachable.
#' @export
lllmr_models <- function() {
  base <- if (nzchar(.lllmr_env$ollama_url %||% "")) .lllmr_env$ollama_url else "http://localhost:11434"
  tryCatch({
    resp <- httr2::request(paste0(base, "/api/tags")) |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    if (length(body$models) == 0) {
      message("No models installed. Run: ollama pull llama3.2")
      return(invisible(NULL))
    }
    models <- data.frame(
      name = vapply(body$models, function(m) m$name, character(1)),
      size = vapply(body$models, function(m) {
        format_bytes(m$size)
      }, character(1)),
      modified = vapply(body$models, function(m) m$modified_at, character(1)),
      stringsAsFactors = FALSE
    )
    models
  }, error = function(e) {
    message("Could not reach Ollama. Is it running?")
    invisible(NULL)
  })
}


#' Check Ollama Status
#'
#' @param quiet Logical. If TRUE, suppresses messages.
#' @return Logical indicating whether Ollama is reachable.
#' @export
lllmr_status <- function(quiet = FALSE) {
  base <- if (nzchar(.lllmr_env$ollama_url %||% "")) .lllmr_env$ollama_url else "http://localhost:11434"
  tryCatch({
    resp <- httr2::request(paste0(base, "/api/tags")) |>
      httr2::req_perform()
    if (!quiet) {
      body <- httr2::resp_body_json(resp)
      n <- length(body$models)
      message("Ollama is running at ", base, ". ", n, " model(s) installed.")
    }
    invisible(TRUE)
  }, error = function(e) {
    if (!quiet) message("Ollama is not reachable at ", base, ".")
    invisible(FALSE)
  })
}

# Null-coalescing helper
`%||%` <- function(x, y) if (is.null(x)) y else x


# -- Internal helpers ----------------------------------------------------------

# Package-level environment for storing server state
.lllmr_env <- new.env(parent = emptyenv())

# Lockfile persists the server PID across R sessions for cleanup
.lllmr_lockfile <- path.expand("~/.lllmr_server.pid")

# Shared context file written by the main session, read by the server process
.lllmr_context_file <- path.expand("~/.lllmr_context.json")

stop_existing_server <- function() {
  # Kill in-session process handle
  if (!is.null(.lllmr_env$server_proc)) {
    tryCatch(.lllmr_env$server_proc$kill(), error = function(e) NULL)
    .lllmr_env$server_proc <- NULL
    .lllmr_env$port <- NULL
  }
  # Kill cross-session stale process via lockfile PID
  if (file.exists(.lllmr_lockfile)) {
    pid <- suppressWarnings(
      tryCatch(as.integer(readLines(.lllmr_lockfile, warn = FALSE)[[1L]]),
               error = function(e) NA_integer_)
    )
    if (!is.na(pid) && pid > 0L) {
      tryCatch(tools::pskill(pid), error = function(e) NULL)
    }
    unlink(.lllmr_lockfile)
  }
}

.onAttach <- function(libname, pkgname) {
  tryCatch({
    resp <- httr2::request("http://localhost:11434/api/tags") |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    if (length(body$models) == 0) {
      packageStartupMessage(
        "\n--- lllmr ---------------------------------------------------\n",
        " Ollama is running but no models are installed.\n",
        " Pull one first:  ollama pull llama3.2\n",
        "-------------------------------------------------------------\n"
      )
    } else {
      model_names <- vapply(body$models, function(m) m$name, character(1))
      packageStartupMessage(
        "\n--- lllmr ---------------------------------------------------\n",
        " Available Ollama models:\n",
        paste0("   - ", model_names, collapse = "\n"), "\n",
        " \n",
        " Start chat: lllmr::lllmr_chat()               # uses first model\n",
        " Pick model: lllmr::lllmr_chat(model = \"...\")  # specify one above\n",
        "-------------------------------------------------------------\n"
      )
    }
  }, error = function(e) {
    packageStartupMessage(
      "\n--- lllmr ---------------------------------------------------\n",
      " Ollama not detected. Install and start Ollama, then run:\n",
      "   lllmr::lllmr_chat()\n",
      "-------------------------------------------------------------\n"
    )
  })
}

format_bytes <- function(bytes) {
  if (bytes >= 1e9) {
    paste0(round(bytes / 1e9, 1), " GB")
  } else if (bytes >= 1e6) {
    paste0(round(bytes / 1e6, 1), " MB")
  } else {
    paste0(round(bytes / 1e3, 1), " KB")
  }
}

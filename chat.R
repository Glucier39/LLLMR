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
lllmr_chat <- function(model = "llama3.2",
                       port = NULL,
                       host = "127.0.0.1",
                       launch = TRUE) {
  
  # Check Ollama connectivity
  ollama_ok <- lllmr_status(quiet = TRUE)
  if (!ollama_ok) {
    message("\n",
            "--- lllmr --------------------------------------------------\n",
            " Ollama is not running or not reachable at localhost:11434.\n",
            " \n",
            " To get started:\n",
            "   1. Install Ollama:       https://ollama.com\n",
            "   2. Start Ollama:         (it runs on startup by default)\n",
            "   3. Pull a model:         ollama pull llama3.2\n",
            "   4. Re-run:               lllmr::lllmr_chat()\n",
            "------------------------------------------------------------\n")
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
  
  # Build the httpuv app
  app <- build_app(app_dir = app_dir, model = model)
  
  # Start the server
  server <- httpuv::startServer(host = host, port = port, app = app)
  
  # Store server reference for cleanup
  .lllmr_env$server <- server
  .lllmr_env$port <- port
  
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
  
  invisible(server)
}


#' List Available Ollama Models
#'
#' Queries the local Ollama instance for installed models.
#'
#' @return A data frame of installed models, or NULL if Ollama is unreachable.
#' @export
lllmr_models <- function() {
  tryCatch({
    resp <- httr2::request("http://localhost:11434/api/tags") |>
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
  tryCatch({
    resp <- httr2::request("http://localhost:11434/api/tags") |>
      httr2::req_perform()
    if (!quiet) {
      body <- httr2::resp_body_json(resp)
      n <- length(body$models)
      message("Ollama is running. ", n, " model(s) installed.")
    }
    invisible(TRUE)
  }, error = function(e) {
    if (!quiet) message("Ollama is not reachable at localhost:11434.")
    invisible(FALSE)
  })
}


# -- Internal helpers ----------------------------------------------------------

# Package-level environment for storing server state
.lllmr_env <- new.env(parent = emptyenv())

stop_existing_server <- function() {
  if (!is.null(.lllmr_env$server)) {
    tryCatch({
      httpuv::stopServer(.lllmr_env$server)
      message("Stopped previous lllmr server.")
    }, error = function(e) NULL)
    .lllmr_env$server <- NULL
    .lllmr_env$port <- NULL
  }
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

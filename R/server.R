#' Build the httpuv application
#'
#' Creates the httpuv app list with routing for static files and API endpoints.
#'
#' @param app_dir Path to the inst/app directory containing frontend assets.
#' @param model Default Ollama model name.
#' @return A list suitable for httpuv::startServer().
#' @keywords internal
build_app <- function(app_dir, model) {
  
  # Store mutable state in the package environment
  .lllmr_env$conversation <- list()
  .lllmr_env$active_model <- model
  
  list(
    call = function(req) {
      path <- req$PATH_INFO
      method <- req$REQUEST_METHOD
      
      # POST /api/chat
      if (method == "POST" && path == "/api/chat") {
        return(handle_chat(req))
      }
      
      # GET /api/models
      if (method == "GET" && path == "/api/models") {
        return(handle_models())
      }
      
      # POST /api/model
      if (method == "POST" && path == "/api/model") {
        body <- parse_request_body(req)
        if (!is.null(body$model)) {
          .lllmr_env$active_model <- body$model
        }
        return(json_response(list(model = .lllmr_env$active_model)))
      }
      
      # GET /api/context
      if (method == "GET" && path == "/api/context") {
        ctx <- gather_context()
        return(json_response(ctx))
      }
      
      # POST /api/insert
      if (method == "POST" && path == "/api/insert") {
        body <- parse_request_body(req)
        return(handle_insert(body$code))
      }
      
      # POST /api/reset
      if (method == "POST" && path == "/api/reset") {
        .lllmr_env$conversation <- list()
        return(json_response(list(status = "ok")))
      }
      
      # GET /api/status
      if (method == "GET" && path == "/api/status") {
        return(json_response(list(
          status = "ok",
          model = .lllmr_env$active_model,
          ollama = lllmr_status(quiet = TRUE)
        )))
      }
      
      # Static files
      serve_static(app_dir, path)
    },
    
    onWSOpen = function(ws) {
      ws$onMessage(function(binary, message) {
        tryCatch({
          data <- jsonlite::fromJSON(message)
          
          if (identical(data$type, "chat")) {
            handle_chat_ws(ws, data)
          }
        }, error = function(e) {
          ws$send(jsonlite::toJSON(list(
            type = "error",
            content = paste("Server error:", e$message)
          ), auto_unbox = TRUE))
        })
      })
    }
  )
}


# -- Route handlers ------------------------------------------------------------

#' Handle streaming chat via WebSocket
#' @keywords internal
handle_chat_ws <- function(ws, data) {
  user_message <- data$message
  use_context <- isTRUE(data$use_context)
  active_model <- if (!is.null(data$model) && nzchar(data$model)) {
    data$model
  } else {
    .lllmr_env$active_model
  }
  
  # Build system prompt with optional R context
  system_prompt <- build_system_prompt(use_context)
  
  # Append user message to conversation
  conv <- .lllmr_env$conversation
  conv[[length(conv) + 1L]] <- list(role = "user", content = user_message)
  .lllmr_env$conversation <- conv
  
  # Build messages array for Ollama
  messages <- c(
    list(list(role = "system", content = system_prompt)),
    .lllmr_env$conversation
  )
  
  # Stream from Ollama
  tryCatch({
    resp <- httr2::request("http://localhost:11434/api/chat") |>
      httr2::req_body_json(list(
        model = active_model,
        messages = messages,
        stream = TRUE
      )) |>
      httr2::req_perform_connection()
    
    full_response <- ""
    
    repeat {
      lines <- httr2::resp_stream_lines(resp, lines = 5)
      if (length(lines) == 0) break
      
      for (line in lines) {
        if (nchar(trimws(line)) == 0) next
        chunk <- tryCatch(jsonlite::fromJSON(line), error = function(e) NULL)
        if (is.null(chunk)) next
        
        if (!is.null(chunk$message$content)) {
          token <- chunk$message$content
          full_response <- paste0(full_response, token)
          
          ws$send(jsonlite::toJSON(list(
            type = "token",
            content = token
          ), auto_unbox = TRUE))
        }
        
        if (isTRUE(chunk$done)) break
      }
    }
    
    close(resp)
    
    # Store assistant response in conversation history
    conv <- .lllmr_env$conversation
    conv[[length(conv) + 1L]] <- list(role = "assistant", content = full_response)
    .lllmr_env$conversation <- conv
    
    ws$send(jsonlite::toJSON(list(type = "done"), auto_unbox = TRUE))
    
  }, error = function(e) {
    ws$send(jsonlite::toJSON(list(
      type = "error",
      content = paste("Ollama error:", e$message)
    ), auto_unbox = TRUE))
  })
}


#' Handle non-streaming chat (fallback)
#' @keywords internal
handle_chat <- function(req) {
  json_response(list(
    info = "Use WebSocket connection for streaming chat.",
    hint = "Connect via WebSocket and send {type:'chat', message:'...'}"
  ))
}

#' Handle model listing
#' @keywords internal
handle_models <- function() {
  tryCatch({
    resp <- httr2::request("http://localhost:11434/api/tags") |>
      httr2::req_perform()
    body <- httr2::resp_body_json(resp)
    models <- lapply(body$models, function(m) {
      fam <- if (!is.null(m$details$family)) m$details$family else "unknown"
      params <- if (!is.null(m$details$parameter_size)) m$details$parameter_size else "unknown"
      list(
        name = m$name,
        size = format_bytes(m$size),
        modified = m$modified_at,
        family = fam,
        parameters = params
      )
    })
    json_response(list(models = models))
  }, error = function(e) {
    json_response(list(models = list(), error = e$message), status = 503L)
  })
}

#' Handle code insertion into RStudio editor
#' @keywords internal
handle_insert <- function(code) {
  if (is.null(code) || !nzchar(code)) {
    return(json_response(list(status = "error", message = "No code provided"),
                         status = 400L))
  }
  tryCatch({
    if (rstudioapi::isAvailable()) {
      rstudioapi::insertText(text = code)
      json_response(list(status = "ok"))
    } else {
      json_response(list(status = "error",
                         message = "RStudio API not available"),
                    status = 503L)
    }
  }, error = function(e) {
    json_response(list(status = "error", message = e$message), status = 500L)
  })
}


# -- Static file server -------------------------------------------------------

#' Serve static files from the app directory
#' @keywords internal
serve_static <- function(app_dir, path) {
  if (path == "/" || path == "") {
    path <- "/index.html"
  }
  
  file_path <- file.path(app_dir, sub("^/", "", path))
  
  if (!file.exists(file_path)) {
    return(list(
      status = 404L,
      headers = list("Content-Type" = "text/plain"),
      body = "Not found"
    ))
  }
  
  ext <- tolower(tools::file_ext(file_path))
  mime <- switch(ext,
                 html = "text/html; charset=utf-8",
                 css  = "text/css; charset=utf-8",
                 js   = "application/javascript; charset=utf-8",
                 json = "application/json",
                 svg  = "image/svg+xml",
                 png  = "image/png",
                 "application/octet-stream"
  )
  
  list(
    status = 200L,
    headers = list(
      "Content-Type" = mime,
      "Cache-Control" = "no-cache"
    ),
    body = readBin(file_path, "raw", file.info(file_path)$size)
  )
}


# -- Utilities -----------------------------------------------------------------

#' Parse JSON request body
#' @keywords internal
parse_request_body <- function(req) {
  tryCatch({
    body_raw <- req$rook.input$read()
    jsonlite::fromJSON(rawToChar(body_raw))
  }, error = function(e) list())
}

#' Create a JSON HTTP response
#' @keywords internal
json_response <- function(data, status = 200L) {
  list(
    status = status,
    headers = list(
      "Content-Type" = "application/json; charset=utf-8",
      "Access-Control-Allow-Origin" = "*"
    ),
    body = jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")
  )
}

#' Run the httpuv server (blocking) — called in a background process
#' @keywords internal
run_server <- function(app_dir, model, host, port) {
  app <- build_app(app_dir = app_dir, model = model)
  server <- httpuv::startServer(host = host, port = port, app = app)
  on.exit(httpuv::stopServer(server), add = TRUE)
  while (TRUE) httpuv::service(100)
}

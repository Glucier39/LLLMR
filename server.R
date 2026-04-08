#' Build the httpuv application
#'
#' Creates the httpuv app list with routing for static files and API endpoints.
#'
#' @param app_dir Path to the inst/app directory containing frontend assets.
#' @param model Default Ollama model name.
#' @return A list suitable for httpuv::startServer().
#' @keywords internal
build_app <- function(app_dir, model) {

  # Conversation history stored per-session (single user, local)
  conversation <- list()

  list(
    call = function(req) {
      path <- req$PATH_INFO
      method <- req$REQUEST_METHOD

      # -- API routes ----------------------------------------------------------

      # POST /api/chat — stream a chat completion from Ollama
      if (method == "POST" && path == "/api/chat") {
        return(handle_chat(req, conversation, model))
      }

      # GET /api/models — list installed Ollama models
      if (method == "GET" && path == "/api/models") {
        return(handle_models())
      }

      # POST /api/model — switch the active model
      if (method == "POST" && path == "/api/model") {
        body <- parse_request_body(req)
        if (!is.null(body$model)) {
          model <<- body$model
        }
        return(json_response(list(model = model)))
      }

      # GET /api/context — gather R session context
      if (method == "GET" && path == "/api/context") {
        ctx <- gather_context()
        return(json_response(ctx))
      }

      # POST /api/insert — insert code into the active RStudio editor
      if (method == "POST" && path == "/api/insert") {
        body <- parse_request_body(req)
        return(handle_insert(body$code))
      }

      # POST /api/reset — clear conversation history
      if (method == "POST" && path == "/api/reset") {
        conversation <<- list()
        return(json_response(list(status = "ok")))
      }

      # GET /api/status — health check
      if (method == "GET" && path == "/api/status") {
        return(json_response(list(
          status = "ok",
          model = model,
          ollama = lllmr_status(quiet = TRUE)
        )))
      }

      # -- Static file serving -------------------------------------------------
      serve_static(app_dir, path)
    },

    onWSOpen = function(ws) {
      # WebSocket handler for streaming chat
      ws$onMessage(function(binary, message) {
        tryCatch({
          data <- jsonlite::fromJSON(message)

          if (identical(data$type, "chat")) {
            handle_chat_ws(ws, data, conversation, model)
            # Update conversation history after streaming completes
            # (done inside handle_chat_ws)
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
handle_chat_ws <- function(ws, data, conversation, model) {
  user_message <- data$message
  use_context <- isTRUE(data$use_context)
  active_model <- if (!is.null(data$model)) data$model else model

  # Build system prompt with optional R context

  system_prompt <- build_system_prompt(use_context)

  # Append user message to conversation
  conversation[[length(conversation) + 1]] <<- list(
    role = "user",
    content = user_message
  )

  # Build messages array for Ollama
  messages <- c(
    list(list(role = "system", content = system_prompt)),
    conversation
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
    conversation[[length(conversation) + 1]] <<- list(
      role = "assistant",
      content = full_response
    )

    ws$send(jsonlite::toJSON(list(
      type = "done"
    ), auto_unbox = TRUE))

  }, error = function(e) {
    ws$send(jsonlite::toJSON(list(
      type = "error",
      content = paste("Ollama error:", e$message)
    ), auto_unbox = TRUE))
  })
}


#' Handle non-streaming chat (fallback)
#' @keywords internal
handle_chat <- function(req, conversation, model) {
  body <- parse_request_body(req)
  json_response(list(
    info = "Use WebSocket connection for streaming chat.",
    hint = "Connect to ws://localhost:PORT and send {type:'chat', message:'...'}"
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
      list(
        name = m$name,
        size = format_bytes(m$size),
        modified = m$modified_at,
        family = m$details$family %||% "unknown",
        parameters = m$details$parameter_size %||% "unknown"
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
  # Default to index.html

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

  # MIME types
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

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a

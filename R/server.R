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
  if (is.null(.lllmr_env$claude_api_key)) {
    .lllmr_env$claude_api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  }
  
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

      # GET /api/apikey — check if Claude API key is set
      if (method == "GET" && path == "/api/apikey") {
        key <- .lllmr_env$claude_api_key
        if (is.null(key)) key <- ""
        return(json_response(list(has_key = nzchar(key))))
      }

      # POST /api/apikey — set Claude API key for this session
      if (method == "POST" && path == "/api/apikey") {
        body <- parse_request_body(req)
        if (!is.null(body$key) && nzchar(body$key)) {
          .lllmr_env$claude_api_key <- body$key
          return(json_response(list(status = "ok")))
        }
        return(json_response(list(status = "error", message = "No key provided"),
                             status = 400L))
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
  use_context  <- isTRUE(data$use_context)
  provider     <- if (!is.null(data$provider) && nzchar(data$provider)) data$provider else "ollama"
  active_model <- if (!is.null(data$model) && nzchar(data$model)) {
    data$model
  } else {
    .lllmr_env$active_model
  }

  # Build system prompt with optional R context
  system_prompt <- build_system_prompt(use_context)

  # Append user message to conversation (user/assistant turns only)
  conv <- .lllmr_env$conversation
  conv[[length(conv) + 1L]] <- list(role = "user", content = user_message)
  .lllmr_env$conversation <- conv

  # Temp files for IPC — worker appends one JSON token per line to token_file,
  # writes full response to done_file, or error message to error_file.
  token_file <- tempfile(pattern = "lllmr_tok_")
  done_file  <- tempfile(pattern = "lllmr_done_")
  error_file <- tempfile(pattern = "lllmr_err_")

  if (provider == "claude") {
    # -- Claude API streaming worker -------------------------------------------
    api_key <- .lllmr_env$claude_api_key
    if (is.null(api_key) || !nzchar(api_key)) {
      ws$send(jsonlite::toJSON(list(
        type    = "error",
        content = "Claude API key not set. Enter it in the UI or set ANTHROPIC_API_KEY."
      ), auto_unbox = TRUE))
      return()
    }

    worker <- callr::r_bg(
      func = function(messages, model, system_prompt, api_key,
                      token_file, done_file, error_file) {
        tryCatch({
          resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
            httr2::req_headers(
              "x-api-key"         = api_key,
              "anthropic-version" = "2023-06-01"
            ) |>
            httr2::req_body_json(list(
              model      = model,
              max_tokens = 8192L,
              system     = system_prompt,
              messages   = messages,
              stream     = TRUE
            )) |>
            httr2::req_perform_connection()

          full_response <- ""

          repeat {
            if (httr2::resp_stream_is_complete(resp)) break
            lines <- httr2::resp_stream_lines(resp, lines = 10)
            if (length(lines) == 0) break

            for (line in lines) {
              line <- trimws(line)
              if (!startsWith(line, "data: ")) next
              json_str <- substring(line, 7L)
              if (json_str == "[DONE]") break

              chunk <- tryCatch(jsonlite::fromJSON(json_str), error = function(e) NULL)
              if (is.null(chunk)) next

              if (identical(chunk$type, "content_block_delta")) {
                token <- chunk$delta$text
                if (!is.null(token) && nzchar(token)) {
                  full_response <- paste0(full_response, token)
                  cat(jsonlite::toJSON(list(token = token), auto_unbox = TRUE),
                      "\n", file = token_file, append = TRUE)
                }
              }
              if (identical(chunk$type, "message_stop")) break
            }
          }

          close(resp)
          writeLines(full_response, done_file)

        }, error = function(e) {
          writeLines(conditionMessage(e), error_file)
        })
      },
      args = list(
        messages      = .lllmr_env$conversation,
        model         = active_model,
        system_prompt = system_prompt,
        api_key       = api_key,
        token_file    = token_file,
        done_file     = done_file,
        error_file    = error_file
      )
    )

  } else {
    # -- Ollama streaming worker -----------------------------------------------
    ollama_messages <- c(
      list(list(role = "system", content = system_prompt)),
      .lllmr_env$conversation
    )

    worker <- callr::r_bg(
      func = function(messages, model, token_file, done_file, error_file) {
        tryCatch({
          resp <- httr2::request("http://localhost:11434/api/chat") |>
            httr2::req_body_json(list(
              model    = model,
              messages = messages,
              stream   = TRUE
            )) |>
            httr2::req_perform_connection()

          full_response <- ""
          done <- FALSE

          repeat {
            if (done || httr2::resp_stream_is_complete(resp)) break
            lines <- httr2::resp_stream_lines(resp, lines = 5)
            if (length(lines) == 0) break

            for (line in lines) {
              if (nchar(trimws(line)) == 0) next
              chunk <- tryCatch(jsonlite::fromJSON(line), error = function(e) NULL)
              if (is.null(chunk)) next

              if (!is.null(chunk$message$content)) {
                full_response <- paste0(full_response, chunk$message$content)
                cat(jsonlite::toJSON(
                  list(token = chunk$message$content), auto_unbox = TRUE
                ), "\n", file = token_file, append = TRUE)
              }

              if (isTRUE(chunk$done)) { done <- TRUE; break }
            }
          }

          close(resp)
          writeLines(full_response, done_file)

        }, error = function(e) {
          writeLines(conditionMessage(e), error_file)
        })
      },
      args = list(
        messages   = ollama_messages,
        model      = active_model,
        token_file = token_file,
        done_file  = done_file,
        error_file = error_file
      )
    )
  }

  # Poll every 50 ms for new tokens written by the worker.
  # All ws$send() calls are wrapped in tryCatch — if the WebSocket closes
  # while streaming, poll exits cleanly instead of crashing and losing the loop.
  last_line <- 0L

  safe_send <- function(msg) tryCatch(ws$send(msg), error = function(e) NULL)

  poll <- function() {
    keep_going <- TRUE

    tryCatch({

      # -- Forward any new token lines ------------------------------------------
      if (file.exists(token_file)) {
        all_lines <- tryCatch(readLines(token_file, warn = FALSE),
                              error = function(e) character(0))
        new_lines <- all_lines[seq_along(all_lines) > last_line]
        last_line <<- length(all_lines)
        for (ln in new_lines) {
          tok <- tryCatch(jsonlite::fromJSON(ln), error = function(e) NULL)
          if (!is.null(tok$token))
            safe_send(jsonlite::toJSON(list(type = "token", content = tok$token),
                                      auto_unbox = TRUE))
        }
      }

      # -- Completion -----------------------------------------------------------
      if (file.exists(done_file)) {
        resp <- tryCatch(paste(readLines(done_file, warn = FALSE), collapse = "\n"),
                         error = function(e) "")
        unlink(c(token_file, done_file, error_file))
        conv <- .lllmr_env$conversation
        conv[[length(conv) + 1L]] <- list(role = "assistant", content = resp)
        .lllmr_env$conversation <- conv
        safe_send(jsonlite::toJSON(list(type = "done"), auto_unbox = TRUE))
        keep_going <<- FALSE
        return()
      }

      # -- Worker wrote an error ------------------------------------------------
      if (file.exists(error_file)) {
        msg <- tryCatch(paste(readLines(error_file, warn = FALSE), collapse = " "),
                        error = function(e) "unknown error")
        unlink(c(token_file, done_file, error_file))
        safe_send(jsonlite::toJSON(list(type = "error", content = msg),
                                   auto_unbox = TRUE))
        keep_going <<- FALSE
        return()
      }

      # -- Worker crashed with no output ----------------------------------------
      if (!worker$is_alive()) {
        stderr_lines <- tryCatch(worker$read_error_lines(), error = function(e) character(0))
        msg <- if (length(stderr_lines) > 0)
          paste(stderr_lines, collapse = "\n")
        else
          "Streaming worker exited unexpectedly"
        unlink(c(token_file, done_file, error_file))
        safe_send(jsonlite::toJSON(list(type = "error", content = msg),
                                   auto_unbox = TRUE))
        keep_going <<- FALSE
        return()
      }

    }, error = function(e) {
      # Unexpected error inside poll — kill worker and stop
      tryCatch(worker$kill(), error = function(e2) NULL)
      unlink(c(token_file, done_file, error_file))
      safe_send(jsonlite::toJSON(list(type = "error",
                                      content = paste("Internal poll error:", e$message)),
                                  auto_unbox = TRUE))
      keep_going <<- FALSE
    })

    if (keep_going) later::later(poll, delay = 0.05)
  }

  later::later(poll, delay = 0.05)
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
  # Unset RStudio env vars so rstudioapi::isAvailable() returns FALSE immediately
  # from this child process. Without this, rstudioapi tries IPC that never
  # gets a response and can block the event loop indefinitely.
  Sys.unsetenv("RSTUDIO")
  Sys.unsetenv("RSTUDIO_SESSION_STREAM")
  Sys.unsetenv("RSTUDIO_SESSION_PORT")

  app <- build_app(app_dir = app_dir, model = model)
  server <- httpuv::startServer(host = host, port = port, app = app)
  on.exit(httpuv::stopServer(server), add = TRUE)
  # Call later::run_now() after each service() to ensure later() callbacks fire.
  # httpuv::service() drives the UV loop but doesn't always flush the later queue.
  while (TRUE) {
    httpuv::service(50)
    later::run_now(timeoutSecs = 0)
  }
}

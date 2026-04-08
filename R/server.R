#' Build the httpuv application
#'
#' Creates the httpuv app list with routing for static files and API endpoints.
#'
#' @param app_dir Path to the inst/app directory containing frontend assets.
#' @param model Default Ollama model name.
#' @return A list suitable for httpuv::startServer().
#' @keywords internal
build_app <- function(app_dir, model, ollama_url = "http://localhost:11434") {

  # Store mutable state in the package environment
  .lllmr_env$conversation       <- list()
  .lllmr_env$active_model       <- model
  .lllmr_env$ollama_url         <- sub("/$", "", ollama_url)
  .lllmr_env$cached_context     <- NULL
  .lllmr_env$injected_file_path <- ""
  .lllmr_env$active_provider    <- "ollama"
  .lllmr_env$session_id         <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (is.null(.lllmr_env$claude_api_key)) {
    key <- Sys.getenv("ANTHROPIC_API_KEY")
    if (!nzchar(key)) {
      keyfile <- path.expand("~/.lllmr_api_key")
      if (file.exists(keyfile)) {
        key <- tryCatch(trimws(readLines(keyfile, warn = FALSE)[[1L]]),
                        error = function(e) "")
      }
    }
    .lllmr_env$claude_api_key <- key
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
      
      # GET /api/context — read live context from the shared file
      if (method == "GET" && path == "/api/context") {
        ctx <- read_context_file()
        if (is.null(ctx)) ctx <- list(summary = "No context yet — gathering...")
        return(json_response(ctx))
      }
      
      # POST /api/insert
      if (method == "POST" && path == "/api/insert") {
        body <- parse_request_body(req)
        return(handle_insert(body$code))
      }
      
      # POST /api/reset
      if (method == "POST" && path == "/api/reset") {
        .lllmr_env$conversation        <- list()
        .lllmr_env$injected_file_path  <- ""
        .lllmr_env$session_id          <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        return(json_response(list(status = "ok")))
      }

      # GET /api/export — download current session as markdown
      if (method == "GET" && path == "/api/export") {
        md       <- session_to_markdown(
          .lllmr_env$conversation,
          .lllmr_env$active_model   %||% "unknown",
          .lllmr_env$active_provider %||% "ollama",
          .lllmr_env$session_id     %||% "unknown"
        )
        filename <- paste0("lllmr_", gsub("[ :]", "-", .lllmr_env$session_id %||% "chat"), ".md")
        return(list(
          status  = 200L,
          headers = list(
            "Content-Type"        = "text/markdown; charset=utf-8",
            "Content-Disposition" = paste0('attachment; filename="', filename, '"')
          ),
          body = chartr("\r", "", md)
        ))
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
          tryCatch({
            keyfile <- path.expand("~/.lllmr_api_key")
            writeLines(body$key, keyfile)
            Sys.chmod(keyfile, mode = "0600")
          }, error = function(e) NULL)
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
          } else if (identical(data$type, "stop")) {
            handle_stop_ws(ws)
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
  .lllmr_env$active_provider <- provider
  active_model <- if (!is.null(data$model) && nzchar(data$model)) {
    data$model
  } else {
    .lllmr_env$active_model
  }

  # Read context fresh from the shared file at request time — same pattern that
  # Cursor/VS Code use: read editor state on-demand, not from a cached/pushed copy.
  # For Ollama: file content goes only in the first-message injection (not the system
  # prompt) to avoid doubling input tokens and causing slow first-token latency.
  # For Claude: file content goes in the system prompt (Claude handles it better there).
  # Token budget strategy:
  #   Claude  — file + context in system prompt (handles long system prompts well)
  #   Ollama  — base system prompt only; context injected into first user message
  #             This avoids duplicating env/history/file across both roles and
  #             minimises prompt tokens, which is the primary driver of TTFT on
  #             local models.
  max_file_chars  <- if (provider == "claude") 50000L else 5000L
  ctx_in_prompt   <- if (provider == "claude") use_context else FALSE
  live_ctx        <- if (use_context) read_context_file() else NULL
  system_prompt   <- build_system_prompt(ctx_in_prompt, max_file_chars = max_file_chars,
                                         ctx = live_ctx)

  # Resolve any @filename mentions — read those files and append to prompt
  mentioned <- parse_at_mentions(user_message)
  if (length(mentioned) > 0) {
    file_contents <- read_mentioned_files(mentioned)
    if (length(file_contents) > 0) {
      extra <- paste(vapply(names(file_contents), function(nm) {
        paste0("\n[Referenced File: ", nm, "]\n```\n",
               truncate_string(file_contents[[nm]], 50000), "\n```\n")
      }, character(1)), collapse = "")
      system_prompt <- paste0(system_prompt,
                              "\n--- REFERENCED FILES ---\n", extra,
                              "--- END REFERENCED FILES ---\n")
    }
  }

  # Append user message to conversation (user/assistant turns only)
  conv <- .lllmr_env$conversation
  conv[[length(conv) + 1L]] <- list(role = "user", content = user_message)
  .lllmr_env$conversation <- conv

  # Temp files for IPC — worker appends one JSON token per line to token_file,
  # writes full response to done_file, or error message to error_file.
  token_file <- tempfile(pattern = "lllmr_tok_")
  done_file  <- tempfile(pattern = "lllmr_done_")
  error_file <- tempfile(pattern = "lllmr_err_")

  # Keep at most the last 40 turns (20 exchanges) to avoid context overflow.
  trimmed_conv <- .lllmr_env$conversation
  if (length(trimmed_conv) > 40L) {
    trimmed_conv <- trimmed_conv[(length(trimmed_conv) - 39L):length(trimmed_conv)]
  }

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
              max_tokens = 16384L,
              system     = system_prompt,
              messages   = messages,
              stream     = TRUE
            )) |>
            httr2::req_perform_connection()

          full_response <- ""

          repeat {
            if (httr2::resp_stream_is_complete(resp)) break
            lines <- httr2::resp_stream_lines(resp, lines = 10)
            if (length(lines) == 0) { Sys.sleep(0.05); next }

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
        messages      = trimmed_conv,
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
    # Inject context into the current user message when:
    #   (a) no file has been injected yet (first turn / context not ready at startup), or
    #   (b) the active file has changed since the last injection (user switched files).
    # On file change, explicitly tell the model the file has changed so it doesn't
    # confuse old and new content from conversation history.
    ollama_conv <- trimmed_conv
    current_file_path <- if (!is.null(live_ctx) && !is.null(live_ctx$active_file))
                           live_ctx$active_file$path %||% "" else ""
    last_injected     <- .lllmr_env$injected_file_path %||% ""
    file_changed      <- nzchar(current_file_path) && !identical(current_file_path, last_injected)
    needs_inject      <- use_context && (file_changed || !nzchar(last_injected))

    if (needs_inject) {
      ctx_inject <- build_context_injection(live_ctx, max_file_chars = max_file_chars,
                                             include_env = FALSE)
      if (nzchar(ctx_inject)) {
        last_idx <- length(ollama_conv)
        enriched <- ollama_conv[[last_idx]]
        prefix <- if (file_changed && nzchar(last_injected))
          paste0("[File switched to: ", current_file_path, "]\n\n", ctx_inject)
        else
          ctx_inject
        enriched$content <- paste0(prefix, "My question: ", enriched$content)
        ollama_conv[[last_idx]] <- enriched
        .lllmr_env$injected_file_path <- current_file_path
      }
      # ctx_inject empty = context file not ready yet; retry on next message.
    }

    ollama_messages <- c(
      list(list(role = "system", content = system_prompt)),
      ollama_conv
    )

    # Size the context window to fit the actual prompt plus response headroom.
    # Always allocating 32K forces Ollama to build a huge KV cache even for
    # simple queries, which is the main cause of slow first-token latency.
    input_chars <- sum(vapply(ollama_messages, function(m) {
      if (is.character(m$content)) nchar(m$content) else 0L
    }, integer(1L)))
    num_ctx <- max(4096L, min(16384L, as.integer(input_chars / 3L) + 2048L))

    worker <- callr::r_bg(
      func = function(messages, model, ollama_url, num_ctx,
                      token_file, done_file, error_file) {
        tryCatch({
          resp <- httr2::request(paste0(ollama_url, "/api/chat")) |>
            httr2::req_body_json(list(
              model    = model,
              messages = messages,
              stream   = TRUE,
              options  = list(num_ctx = num_ctx)
            )) |>
            httr2::req_perform_connection()

          full_response <- ""
          done <- FALSE

          repeat {
            if (done || httr2::resp_stream_is_complete(resp)) break
            lines <- httr2::resp_stream_lines(resp, lines = 5)
            if (length(lines) == 0) { Sys.sleep(0.05); next }

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
        ollama_url = .lllmr_env$ollama_url,
        num_ctx    = num_ctx,
        token_file = token_file,
        done_file  = done_file,
        error_file = error_file
      )
    )
  }

  # Store active worker so the stop handler can kill it
  .lllmr_env$active_worker     <- worker
  .lllmr_env$active_token_file <- token_file
  .lllmr_env$active_done_file  <- done_file
  .lllmr_env$active_error_file <- error_file
  .lllmr_env$stop_requested    <- FALSE

  # Poll every 50 ms for new tokens written by the worker.
  # All ws$send() calls are wrapped in tryCatch — if the WebSocket closes
  # while streaming, poll exits cleanly instead of crashing and losing the loop.
  last_line    <- 0L
  poll_count   <- 0L  # used for heartbeat cadence

  safe_send <- function(msg) tryCatch(ws$send(msg), error = function(e) NULL)

  poll <- function() {
    # Check if stop was requested by the user
    if (isTRUE(.lllmr_env$stop_requested)) {
      .lllmr_env$stop_requested <- FALSE
      .lllmr_env$active_worker  <- NULL
      unlink(c(token_file, done_file, error_file))
      return()  # poll already sent "stopped" from handle_stop_ws
    }

    keep_going  <- TRUE
    poll_count <<- poll_count + 1L
    # Send a heartbeat ping every ~5 s (100 polls × 50 ms) to prevent
    # RStudio's Viewer proxy from dropping the WebSocket on idle connections.
    if (poll_count %% 100L == 0L)
      safe_send(jsonlite::toJSON(list(type = "heartbeat"), auto_unbox = TRUE))

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
        # Persist this exchange to the chat log
        tryCatch(
          append_chat_log(
            user_message,
            resp,
            active_model,
            provider,
            .lllmr_env$session_id %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          ),
          error = function(e) NULL
        )
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


#' Stop an in-progress streaming response
#' @keywords internal
handle_stop_ws <- function(ws) {
  worker <- .lllmr_env$active_worker
  if (!is.null(worker)) {
    tryCatch(worker$kill(), error = function(e) NULL)
    .lllmr_env$active_worker <- NULL
  }
  unlink(c(.lllmr_env$active_token_file,
           .lllmr_env$active_done_file,
           .lllmr_env$active_error_file))
  .lllmr_env$stop_requested <- TRUE
  tryCatch(
    ws$send(jsonlite::toJSON(list(type = "stopped"), auto_unbox = TRUE)),
    error = function(e) NULL
  )
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
    resp <- httr2::request(paste0(.lllmr_env$ollama_url, "/api/tags")) |>
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

#' Read live context from the shared context file written by the main R session.
#' Returns NULL if the file doesn't exist or can't be parsed.
#' @keywords internal
read_context_file <- function() {
  f <- path.expand("~/.lllmr_context.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(f, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
}

#' Parse JSON request body
#' @keywords internal
parse_request_body <- function(req) {
  tryCatch({
    body_raw <- req$rook.input$read()
    # simplifyDataFrame=FALSE keeps arrays-of-objects as lists-of-lists rather
    # than collapsing them to data.frames, which would break context processing.
    jsonlite::fromJSON(rawToChar(body_raw), simplifyDataFrame = FALSE)
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

#' Parse @filename mentions from a chat message
#' @keywords internal
parse_at_mentions <- function(message) {
  m <- gregexpr("@[^\\s,;()\\[\\]{}\"']+", message, perl = TRUE)
  raw <- regmatches(message, m)[[1]]
  unique(sub("^@", "", raw))
}

#' Read files referenced by @filename mentions
#' @keywords internal
read_mentioned_files <- function(filenames) {
  ctx  <- .lllmr_env$cached_context
  root <- if (!is.null(ctx$working_directory) && nzchar(ctx$working_directory))
    ctx$working_directory else getwd()
  result <- list()
  for (fname in filenames) {
    fpath <- file.path(root, fname)
    if (!file.exists(fpath) || isTRUE(file.info(fpath)$isdir)) next
    content <- tryCatch(
      paste(readLines(fpath, warn = FALSE), collapse = "\n"),
      error = function(e) NULL
    )
    if (!is.null(content)) result[[fname]] <- content
  }
  result
}


#' Run the httpuv server (blocking) — called in a background process
#' @keywords internal
run_server <- function(app_dir, model, host, port, ollama_url = "http://localhost:11434") {
  # Unset RStudio env vars so rstudioapi::isAvailable() returns FALSE immediately
  # from this child process. Without this, rstudioapi tries IPC that never
  # gets a response and can block the event loop indefinitely.
  Sys.unsetenv("RSTUDIO")
  Sys.unsetenv("RSTUDIO_SESSION_STREAM")
  Sys.unsetenv("RSTUDIO_SESSION_PORT")

  app <- build_app(app_dir = app_dir, model = model, ollama_url = ollama_url)
  server <- httpuv::startServer(host = host, port = port, app = app)
  on.exit(httpuv::stopServer(server), add = TRUE)
  # Call later::run_now() after each service() to ensure later() callbacks fire.
  # httpuv::service() drives the UV loop but doesn't always flush the later queue.
  while (TRUE) {
    httpuv::service(50)
    later::run_now(timeoutSecs = 0)
  }
}

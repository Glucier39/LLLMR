# lllmr

> **Local Large Language Model in R** — A modern chat interface for local LLMs inside RStudio.

lllmr brings a clean, polished AI chat experience directly into RStudio's Viewer pane. Powered by [Ollama](https://ollama.com), it runs entirely on your machine — no API keys, no accounts, no internet required after model download. Your code and data never leave your computer.

## Features

- **Modern UI** — Clean, warm-toned interface with markdown rendering, syntax-highlighted code blocks, and smooth streaming
- **Fully local** — Runs via Ollama, zero cloud dependency
- **R session awareness** — Sees your active file, selected code, environment objects, console history, and project structure
- **One-click code insertion** — Insert generated code directly into your editor
- **Model switching** — Swap between any installed Ollama model on the fly
- **RStudio Addin** — Bind to a keyboard shortcut for instant access
- **No Shiny** — Lightweight `httpuv` server with vanilla HTML/CSS/JS frontend

## Setup

### 1. Install Ollama

Download from [ollama.com](https://ollama.com) — one-click installer for Mac, Windows, and Linux. Ollama starts automatically on login.

### 2. Pull a model

```bash
ollama pull qwen2.5-coder:14b
```

Other good options: `llama3.2`, `codellama`, `mistral`, `deepseek-coder`

### 3. Install lllmr

```r
# Requires devtools
install.packages("devtools")
devtools::install_github("Glucier39/LLLMR")
```

### 4. Chat

```r
lllmr::lllmr_chat()
```

The chat interface opens in your RStudio Viewer pane. Outside of RStudio it opens in your default browser.

## Usage

```r
# Check that Ollama is running and see installed models
lllmr::lllmr_status()
lllmr::lllmr_models()

# Launch with default model (llama3.2)
lllmr::lllmr_chat()

# Launch with a specific model
lllmr::lllmr_chat(model = "qwen2.5-coder:14b")

# Stop the server
lllmr:::stop_existing_server()
```

### Keyboard Shortcut

lllmr registers as an RStudio Addin. Go to **Tools > Modify Keyboard Shortcuts** and search for "Open lllmr Chat" to bind it.

## Context Awareness

When context is enabled (the default), lllmr gathers and sends to the model:

| Context | Source |
|---------|--------|
| Active file contents | `rstudioapi::getSourceEditorContext()` |
| Selected code | `rstudioapi::getSourceEditorContext()$selection` |
| Global environment | `ls()` with types, dimensions, column names |
| Console history | `savehistory()` (last 30 lines) |
| Project files | `list.files()` on the active project |

Toggle context on/off via the **Context** button in the header. A context bar shows exactly what's being sent.

## Architecture

```
┌─────────────────────────────────────────┐
│  RStudio Viewer Pane                    │
│  ┌───────────────────────────────────┐  │
│  │  HTML/CSS/JS Frontend             │  │
│  │  (marked.js + highlight.js)       │  │
│  └──────────────┬────────────────────┘  │
│                 │ WebSocket              │
│  ┌──────────────┴────────────────────┐  │
│  │  httpuv Server (R)                │  │
│  │  ├── Static file serving          │  │
│  │  ├── API routes                   │  │
│  │  ├── rstudioapi context           │  │
│  │  └── Ollama streaming proxy       │  │
│  └──────────────┬────────────────────┘  │
│                 │ HTTP                   │
│  ┌──────────────┴────────────────────┐  │
│  │  Ollama (localhost:11434)         │  │
│  │  Local LLM inference              │  │
│  └───────────────────────────────────┘  │
└─────────────────────────────────────────┘
```

## License

MIT

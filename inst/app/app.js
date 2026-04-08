/* ==========================================================================
   lllmr — Frontend Application
   ========================================================================== */

(function () {
  'use strict';

  // -- State ------------------------------------------------------------------
  const state = {
    ws: null,
    streaming: false,
    contextEnabled: true,
    currentModel: '',
    provider: 'ollama',
    streamBuffer: '',
    streamTarget: null,
  };

  // -- DOM refs ---------------------------------------------------------------
  const $ = (sel) => document.querySelector(sel);
  const chatArea = $('#chat-area');
  const welcome = $('#welcome');
  const input = $('#message-input');
  const sendBtn = $('#send-btn');
  const modelSelect = $('#model-select');
  const providerSelect = $('#provider-select');
  const apiKeyBar = $('#api-key-bar');
  const apiKeyInput = $('#api-key-input');
  const apiKeySave = $('#api-key-save');
  const apiKeyStatus = $('#api-key-status');
  const providerHint = $('#provider-hint');
  const contextToggle = $('#context-toggle');
  const contextBar = $('#context-bar');
  const contextSummary = $('#context-summary');
  const contextRefresh = $('#context-refresh');
  const newChatBtn = $('#new-chat-btn');

  // Available Claude models
  const CLAUDE_MODELS = [
    { name: 'claude-opus-4-6',  label: 'Claude Opus 4.6 (Most capable)' },
    { name: 'claude-sonnet-4-6', label: 'Claude Sonnet 4.6 (Balanced)' },
    { name: 'claude-haiku-4-5', label: 'Claude Haiku 4.5 (Fast)' },
  ];

  // -- Configure marked.js ----------------------------------------------------
  marked.setOptions({
    breaks: true,
    gfm: true,
    highlight: function (code, lang) {
      if (lang && hljs.getLanguage(lang)) {
        try {
          return hljs.highlight(code, { language: lang }).value;
        } catch (_) {}
      }
      return hljs.highlightAuto(code).value;
    },
  });

  // Custom renderer to wrap code blocks with action buttons
  const renderer = new marked.Renderer();
  const originalCodeRenderer = renderer.code;

  renderer.code = function(obj) {
    const code = typeof obj === 'object' ? obj.text : obj;
    const lang = typeof obj === 'object' ? (obj.lang || '') : (arguments[1] || '');

    let highlighted;
    if (lang && hljs.getLanguage(lang)) {
      try {
        highlighted = hljs.highlight(code, { language: lang }).value;
      } catch (_) {
        highlighted = escapeHtml(code);
      }
    } else {
      highlighted = escapeHtml(code);
    }

    const langLabel = lang || 'code';
    const escapedCode = code.replace(/"/g, '&quot;').replace(/'/g, '&#39;');

    return `
      <div class="code-block-wrapper">
        <div class="code-block-header">
          <span class="code-lang">${langLabel}</span>
          <div class="code-actions">
            <button class="code-btn copy-btn" onclick="window.lllmr.copyCode(this)" data-code="${escapedCode}">
              Copy
            </button>
            <button class="code-btn insert-btn" onclick="window.lllmr.insertCode(this)" data-code="${escapedCode}">
              Insert ↗
            </button>
          </div>
        </div>
        <pre><code class="hljs language-${langLabel}">${highlighted}</code></pre>
      </div>`;
  };

  marked.setOptions({ renderer });

  // -- WebSocket connection ---------------------------------------------------
  function connectWS() {
    const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
    const wsUrl = `${protocol}//${location.host}`;

    state.ws = new WebSocket(wsUrl);

    state.ws.onopen = () => {
      console.log('lllmr: WebSocket connected');
    };

    state.ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      handleWSMessage(data);
    };

    state.ws.onclose = () => {
      console.log('lllmr: WebSocket closed, reconnecting...');
      setTimeout(connectWS, 2000);
    };

    state.ws.onerror = (err) => {
      console.error('lllmr: WebSocket error', err);
    };
  }

  function handleWSMessage(data) {
    switch (data.type) {
      case 'token':
        appendToken(data.content);
        break;
      case 'done':
        finishStreaming();
        break;
      case 'error':
        showError(data.content);
        finishStreaming();
        break;
    }
  }

  // -- Message rendering ------------------------------------------------------
  function addUserMessage(text) {
    if (welcome) welcome.remove();

    const el = document.createElement('div');
    el.className = 'message user';
    el.innerHTML = `
      <div class="message-content">
        <div class="message-label">You</div>
        <div class="message-body">${escapeHtml(text)}</div>
      </div>`;
    chatArea.appendChild(el);
    scrollToBottom();
  }

  function startAssistantMessage() {
    const el = document.createElement('div');
    el.className = 'message assistant';
    el.innerHTML = `
      <div class="message-content">
        <div class="message-label">lllmr</div>
        <div class="message-body streaming-cursor"></div>
      </div>`;
    chatArea.appendChild(el);
    state.streamTarget = el.querySelector('.message-body');
    state.streamBuffer = '';
    scrollToBottom();
  }

  function appendToken(token) {
    if (!state.streamTarget) return;
    state.streamBuffer += token;
    // Re-render the full accumulated markdown on each token
    state.streamTarget.innerHTML = marked.parse(state.streamBuffer);
    state.streamTarget.classList.add('streaming-cursor');
    scrollToBottom();
  }

  function finishStreaming() {
    if (state.streamTarget) {
      state.streamTarget.classList.remove('streaming-cursor');
      // Final render pass
      state.streamTarget.innerHTML = marked.parse(state.streamBuffer);
    }
    state.streaming = false;
    state.streamTarget = null;
    state.streamBuffer = '';
    removeThinking();
    updateSendButton();
    scrollToBottom();
  }

  function showThinking() {
    const el = document.createElement('div');
    el.className = 'thinking-indicator';
    el.id = 'thinking';
    el.innerHTML = `
      <div class="message-content">
        <div class="thinking-dots"><span></span><span></span><span></span></div>
        <span class="thinking-text">Thinking...</span>
      </div>`;
    chatArea.appendChild(el);
    scrollToBottom();
  }

  function removeThinking() {
    const el = document.getElementById('thinking');
    if (el) el.remove();
  }

  function showError(message) {
    removeThinking();
    const el = document.createElement('div');
    el.className = 'error-message';
    el.textContent = message;
    chatArea.appendChild(el);
    scrollToBottom();
  }

  // -- Send message -----------------------------------------------------------
  function sendMessage() {
    const text = input.value.trim();
    if (!text || state.streaming) return;

    addUserMessage(text);
    input.value = '';
    autoResizeInput();
    state.streaming = true;
    updateSendButton();

    showThinking();
    startAssistantMessage();
    removeThinking();

    if (state.ws && state.ws.readyState === WebSocket.OPEN) {
      state.ws.send(JSON.stringify({
        type: 'chat',
        message: text,
        use_context: state.contextEnabled,
        model: state.currentModel,
        provider: state.provider,
      }));
    } else {
      showError('Not connected to server. Reconnecting...');
      finishStreaming();
      connectWS();
    }
  }

  // -- Code block actions -----------------------------------------------------
  window.lllmr = {
    copyCode: function (btn) {
      const code = decodeEntities(btn.dataset.code);
      navigator.clipboard.writeText(code).then(() => {
        btn.textContent = 'Copied!';
        btn.classList.add('copied');
        setTimeout(() => {
          btn.textContent = 'Copy';
          btn.classList.remove('copied');
        }, 1500);
      });
    },

    insertCode: function (btn) {
      const code = decodeEntities(btn.dataset.code);
      fetch('/api/insert', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ code }),
      })
        .then((r) => r.json())
        .then((data) => {
          if (data.status === 'ok') {
            btn.textContent = 'Inserted!';
            setTimeout(() => (btn.textContent = 'Insert ↗'), 1500);
          } else {
            btn.textContent = 'Failed';
            setTimeout(() => (btn.textContent = 'Insert ↗'), 1500);
          }
        })
        .catch(() => {
          btn.textContent = 'Error';
          setTimeout(() => (btn.textContent = 'Insert ↗'), 1500);
        });
    },
  };

  // -- Model management -------------------------------------------------------
  function loadModels() {
    fetch('/api/models')
      .then((r) => r.json())
      .then((data) => {
        modelSelect.innerHTML = '';
        if (!data.models || data.models.length === 0) {
          modelSelect.innerHTML = '<option value="">No models</option>';
          return;
        }
        data.models.forEach((m) => {
          const opt = document.createElement('option');
          opt.value = m.name;
          opt.textContent = `${m.name} (${m.parameters || m.size})`;
          modelSelect.appendChild(opt);
        });
        state.currentModel = data.models[0].name;
        modelSelect.value = state.currentModel;
      })
      .catch(() => {
        modelSelect.innerHTML = '<option value="">Ollama offline</option>';
      });
  }

  function loadClaudeModels() {
    modelSelect.innerHTML = '';
    CLAUDE_MODELS.forEach((m) => {
      const opt = document.createElement('option');
      opt.value = m.name;
      opt.textContent = m.label;
      modelSelect.appendChild(opt);
    });
    state.currentModel = CLAUDE_MODELS[0].name;
    modelSelect.value = state.currentModel;
  }

  // -- Provider management ----------------------------------------------------
  function checkApiKey() {
    fetch('/api/apikey')
      .then((r) => r.json())
      .then((data) => {
        if (data.has_key) {
          apiKeyStatus.textContent = 'Key set';
          apiKeyStatus.className = 'api-key-status ok';
        } else {
          apiKeyStatus.textContent = 'No key — enter below';
          apiKeyStatus.className = 'api-key-status warn';
        }
      })
      .catch(() => {
        apiKeyStatus.textContent = '';
      });
  }

  function saveApiKey() {
    const key = apiKeyInput.value.trim();
    if (!key) return;
    fetch('/api/apikey', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ key }),
    })
      .then((r) => r.json())
      .then((data) => {
        if (data.status === 'ok') {
          apiKeyStatus.textContent = 'Key saved';
          apiKeyStatus.className = 'api-key-status ok';
          apiKeyInput.value = '';
        }
      })
      .catch(() => {});
  }

  function onProviderChange() {
    state.provider = providerSelect.value;
    if (state.provider === 'claude') {
      apiKeyBar.classList.remove('hidden');
      loadClaudeModels();
      checkApiKey();
      if (providerHint) {
        providerHint.textContent = 'Using Claude API — requests are sent to Anthropic';
      }
    } else {
      apiKeyBar.classList.add('hidden');
      loadModels();
      if (providerHint) {
        providerHint.textContent = 'lllmr runs locally via Ollama — your data never leaves your machine';
      }
    }
  }

  // -- Context management -----------------------------------------------------
  function loadContext() {
    if (!state.contextEnabled) return;
    fetch('/api/context')
      .then((r) => r.json())
      .then((data) => {
        contextSummary.textContent = data.summary || 'No context available';
      })
      .catch(() => {
        contextSummary.textContent = 'Could not gather context';
      });
  }

  function toggleContext() {
    state.contextEnabled = !state.contextEnabled;
    contextToggle.classList.toggle('active', state.contextEnabled);
    contextBar.classList.toggle('hidden', !state.contextEnabled);
    if (state.contextEnabled) loadContext();
  }

  // -- New chat ---------------------------------------------------------------
  function resetChat() {
    fetch('/api/reset', { method: 'POST' }).catch(() => {});

    // Clear chat UI
    chatArea.innerHTML = '';

    // Re-add welcome screen
    const welcomeEl = document.createElement('div');
    welcomeEl.className = 'welcome';
    welcomeEl.id = 'welcome';
    welcomeEl.innerHTML = `
      <div class="welcome-logo">lllmr</div>
      <p class="welcome-sub">Local LLM, right inside RStudio</p>
      <div class="welcome-hints">
        <button class="hint-chip" data-prompt="Explain my current script and suggest improvements">Explain my code</button>
        <button class="hint-chip" data-prompt="Find potential bugs or issues in my selected code">Debug selection</button>
        <button class="hint-chip" data-prompt="Create a ggplot visualization for the data in my environment">Plot my data</button>
        <button class="hint-chip" data-prompt="What packages would you recommend for my current task?">Suggest packages</button>
      </div>`;
    chatArea.appendChild(welcomeEl);
    bindHintChips();

    state.streaming = false;
    state.streamBuffer = '';
    state.streamTarget = null;
    input.focus();
  }

  // -- UI helpers -------------------------------------------------------------
  function scrollToBottom() {
    requestAnimationFrame(() => {
      chatArea.scrollTop = chatArea.scrollHeight;
    });
  }

  function updateSendButton() {
    sendBtn.disabled = state.streaming || !input.value.trim();
  }

  function autoResizeInput() {
    input.style.height = 'auto';
    input.style.height = Math.min(input.scrollHeight, 160) + 'px';
  }

  function escapeHtml(str) {
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
  }

  function decodeEntities(str) {
    const ta = document.createElement('textarea');
    ta.innerHTML = str;
    return ta.value;
  }

  function bindHintChips() {
    document.querySelectorAll('.hint-chip').forEach((chip) => {
      chip.addEventListener('click', () => {
        input.value = chip.dataset.prompt;
        autoResizeInput();
        updateSendButton();
        sendMessage();
      });
    });
  }

  // -- Event listeners --------------------------------------------------------
  input.addEventListener('input', () => {
    autoResizeInput();
    updateSendButton();
  });

  input.addEventListener('keydown', (e) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      sendMessage();
    }
  });

  sendBtn.addEventListener('click', sendMessage);

  modelSelect.addEventListener('change', () => {
    state.currentModel = modelSelect.value;
    fetch('/api/model', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ model: state.currentModel }),
    });
  });

  contextToggle.addEventListener('click', toggleContext);
  contextRefresh.addEventListener('click', loadContext);
  newChatBtn.addEventListener('click', resetChat);
  providerSelect.addEventListener('change', onProviderChange);
  apiKeySave.addEventListener('click', saveApiKey);
  apiKeyInput.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') saveApiKey();
  });

  // -- Init -------------------------------------------------------------------
  function init() {
    connectWS();
    loadModels();
    loadContext();
    bindHintChips();

    // Set initial context toggle state
    contextToggle.classList.add('active');

    input.focus();
  }

  init();
})();

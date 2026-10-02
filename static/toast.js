(() => {
  htmx.config.noSwap = [204, 304, '4xx', '5xx'];
  const fallback_message = {
    401: "Unauthorized.",
    403: "Forbidden.",
    404: "Not found.",
    429: "Too many requests. Wait a moment and try again.",
  };

  function toast(message, type = 'error') {
    const el = document.createElement('div');
    el.className = `toast ${type}`;
    el.setAttribute('role', type === 'error' ? 'alert' : 'status');

    const text = document.createElement('p');
    text.textContent = message;

    const close = document.createElement('button');
    close.type = 'button';
    close.className = 'close';
    close.setAttribute('aria-label', 'Dismiss');
    close.textContent = '×';

    let timer;
    const dismiss = () => {
      clearTimeout(timer);
      el.classList.add('leaving');
      setTimeout(() => el.remove(), 200);
    };

    const start = () => { timer = setTimeout(dismiss, 5000); };

    close.addEventListener('click', dismiss);
    el.addEventListener('mouseenter', () => clearTimeout(timer));
    el.addEventListener('mouseleave', start);

    el.append(text, close);
    document.getElementById('toasts').append(el);
    start();
    return el;
  }

  document.addEventListener('htmx:finally:request', (evt) => {
    const ctx = evt.detail.ctx;
    if (ctx.response?.status >= 400 && ctx.swap === 'none') {
      const status = ctx.response.status;
      const message = ctx.text || fallback_message[status] || "Something went wrong (${status}).";
      toast(message);
    }
  });

  document.addEventListener('htmx:error', (evt) => {
    const { ctx, error } = evt.detail;
    if (!ctx || ctx.response || error?.name === 'AbortError') return;
    toast("Can't reach the server. Check your connection and try again.");
  });

  document.addEventListener('htmx:ws:after:message:incoming', async (evt) => {
    let data;
    try { data = await evt.detail.message.json(); } catch { return; } // plain HTML message
    if (data?.toast) toast(String(data.toast), data.type === 'error' ? 'error' : 'info');
  });

  window.toast = toast;
})();

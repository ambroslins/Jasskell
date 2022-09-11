const app = Elm.Main.init({ node: document.getElementById('app') });

let socket = null;

app.ports.open.subscribe((url) => {
  if (socket) { socket.close(); }
  socket = new WebSocket(url);
  socket.onopen = (event) => app.ports.onOpen.send();
  socket.onmessage = (event) => app.ports.onMessage.send(event.data);
  socket.onclose = (event) => app.ports.onClose.send();
  socket.onerror = (event) => app.ports.onError.send();
});

app.ports.send?.subscribe((msg) => socket?.send(msg));
app.ports.close?.subscribe(() => {
  socket?.close();
  socket = null;
});

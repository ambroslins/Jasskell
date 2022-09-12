const app = Elm.Main.init();

let socket = null;

app.ports.open.subscribe((path) => {
  if (socket) { socket.close(); }
  const url = 'ws://' + location.hostname + ':' + location.port + path;
  socket = new WebSocket(url);
  socket.onopen = (event) => app.ports.onOpen.send({});
  socket.onmessage = (event) => app.ports.onMessage.send(event.data);
  socket.onclose = (event) => app.ports.onClose.send({});
  socket.onerror = (event) => app.ports.onError.send({});
});

app.ports.send.subscribe((msg) => socket?.send(msg));
app.ports.closePort.subscribe((_) => {
  socket?.close();
  socket = null;
});

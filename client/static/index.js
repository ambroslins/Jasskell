let socket = null;

app.ports.open.subscribe((path) => {
  if (socket) { socket.close(); }
  const url = 'ws://' + location.hostname + ':' + location.port + path;
  socket = new WebSocket(url);
  socket.onopen = (event) => app.ports.onOpen.send({});
  socket.onmessage = (event) => {
    console.log(event.data)
    app.ports.onMessage.send(event.data);
  };
  socket.onclose = (event) => app.ports.onClose.send({});
  socket.onerror = (event) => app.ports.onError.send({});
});

app.ports.send.subscribe((msg) => socket?.send(JSON.stringify(msg)));
app.ports.closePort?.subscribe((_) => {
  socket?.close();
  socket = null;
});

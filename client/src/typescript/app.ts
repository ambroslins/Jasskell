let socket: WebSocket | null

function init(ports: any) {
    ports.connect.subscribe((address: string) => connect(address, ports));
}

function connect(address: string, ports: any) {
    socket = new WebSocket(address);
    socket.onopen = (ev) => {
        socket.onmessage = (event) => {
            ports.receive.send({ "type": "message", "data": event.data })
        };
        ports.send.subscribe((data: object) => socket.send(JSON.stringify(data)));
    }
}


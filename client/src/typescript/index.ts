function handleSocketPort(port: any) {
    let socket: WebSocket | null = null;
    return (event: { type: string, address?: string, data?: string }) => {
        switch (event.type) {
            case "connect":
                if (socket === null || socket.readyState === WebSocket.CLOSED) {
                    socket = new WebSocket(event.address);
                    socket.onopen = (_) => port.send({ type: "open" });
                    socket.onmessage = (ev) => port.send({ type: "message", data: ev.data });
                    socket.onclose = (_) => port.send({ type: "close" });
                    socket.onerror = (_) => port.send({ type: "error" });
                }

                break;
            case "send":
                if (socket && socket.readyState === WebSocket.OPEN) {
                    socket.send(JSON.stringify(event.data));
                }
                break;
            case "close":
                if (socket && socket.readyState === WebSocket.OPEN) {
                    socket.close();
                }
            default:
                break;
        }
    }
}


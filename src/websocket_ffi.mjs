import { WebSocket, WebSocketServer } from "ws";

import {
    CommandEvent,
    HandleErrorEvent,
    ParseErrorEvent,
} from "./websocket.mjs";

import { updatePackageTracker } from "./package_tracker_ffi.mjs";

let DEBUG = false;

export function setupServer(conductorPort, messageHandler) {
    const wss = new WebSocketServer({ port: `${conductorPort}` });

    wss.on("connection", function connection(ws) {
        ws.on("error", function message(error) {
            messageHandler(new ErrorEvent(error));
        });

        // ws.on("close", () => {
        //     console.log("Emacs client disconnected");
        // });

        ws.on("message", function message(data) {
            // Decode buffer to string
            const stringData = data.toString("utf-8");
            let parsedData;
            try {
                // data is type of JSON array
                parsedData = JSON.parse(stringData);
                if (DEBUG) {
                    const now = new Date();
                    console.log(
                        `[${now.getHours()}:${now.getMinutes()}:${now.getSeconds()}.${now.getMilliseconds()}]`,
                        "parsed:",
                        parsedData,
                    );
                }
            } catch (e) {
                messageHandler(new ParseErrorEvent(stringData));
            }

            const messageTag = parsedData[0];
            if (messageTag == "data") {
                const command = parsedData[1];
                try {
                    if (command[0] == "shutdown") {
                        shutdown(wss);
                    } else if (
                        command[0] === "package_installed" &&
                        command.length > 1
                    ) {
                        // Update the package tracker before handling the message
                        const packageName = command[1];
                        let packageTracker = updatePackageTracker(packageName);
                        if (DEBUG) {
                            console.log(
                                `Package installed: ${packageName}. ` +
                                    `${packageTracker.installed.length}/${packageTracker.total} ` +
                                    `(${packageTracker.pending.length} remaining)`,
                            );
                        }
                        messageHandler(new CommandEvent(command));
                    } else {
                        messageHandler(new CommandEvent(command));
                    }
                } catch (e) {
                    messageHandler(new HandleErrorEvent(command, e));
                }
            } else {
                messageHandler(
                    new HandleErrorEvent(
                        "",
                        `Unknown message tag: ${messageTag}`,
                    ),
                );
            }
        });
    });
}

export function setupClient(emacsPort) {
    return new Promise((resolve) => {
        const ws = new WebSocket(`ws://127.0.0.1:${emacsPort}`);

        ws.on("error", console.error);

        ws.on("open", function open() {
            console.log("Connected to Emacs server");
            // Resolve the promise with the websocket when it's open
            resolve(ws);
        });

        ws.on("message", function message(data) {
            console.error("Emcas Server: %s", data);
        });
    });
}

export function messageToEmacs(ws, message) {
    try {
        const payload = {
            type: "show-message",
            content: message,
        };
        if (DEBUG) console.debug("[show-message] " + message);

        ws.send(JSON.stringify(payload));

        return Promise.resolve({
            ok: true,
            value: "",
        });
    } catch (error) {
        return Promise.resolve({
            ok: false,
            error: `Failed to send message to Emacs: ${error.message}`,
        });
    }
}

export function callInEmacs(ws, code) {
    try {
        const payload = {
            type: "eval-code",
            content: code,
        };
        if (DEBUG) console.debug("[eval-code] " + code);

        ws.send(JSON.stringify(payload));

        return Promise.resolve({
            ok: true,
            value: "",
        });
    } catch (error) {
        return Promise.resolve({
            ok: false,
            error: `Failed to eval code in Emacs: ${error.message}`,
        });
    }
}

export function getEmacsVar(emacsPort, varName) {
    return new Promise((resolve) => {
        try {
            let ws = new WebSocket(`ws://127.0.0.1:${emacsPort}`);
            let timeout;
            ws.on("error", (error) => {
                clearTimeout(timeout);
                resolve({
                    ok: false,
                    error: `WebSocket error: ${error.message}`,
                });
            });

            ws.on("open", function open() {
                if (DEBUG) console.debug("[getEmacsVar] fetch: %s", varName);

                const payload = {
                    type: "fetch-var",
                    content: varName,
                };
                ws.send(JSON.stringify(payload));
            });

            ws.on("message", function message(data) {
                if (DEBUG) console.debug("[getEmacsVar] value: |%s|", data);

                clearTimeout(timeout);
                resolve({
                    ok: true,
                    value: data.toString("utf-8"),
                });
                ws.close();
            });

            timeout = setTimeout(() => {
                ws.close();
                resolve({
                    ok: false,
                    error: "Timeout waiting for Emacs variable",
                });
            }, 5000);
        } catch (error) {
            resolve({
                ok: false,
                error: `Unexpected error: ${error.message}`,
            });
        }
    });
}

export function evalInEmacsWithReturn(emacsPort, timeoutSeconds, code) {
    return new Promise((resolve) => {
        try {
            let ws = new WebSocket(`ws://127.0.0.1:${emacsPort}`);
            let timeout;
            ws.on("error", (error) => {
                clearTimeout(timeout);
                resolve({
                    ok: false,
                    error: `WebSocket error: ${error.message}`,
                });
            });

            ws.on("open", function open() {
                if (DEBUG) console.debug("[callEmacsFunc] call: %s", code);

                const payload = {
                    type: "fetch-var",
                    content: code,
                };
                ws.send(JSON.stringify(payload));
            });

            ws.on("message", function message(data) {
                if (DEBUG) console.debug("[callEmacsFunc] value: |%s|", data);

                clearTimeout(timeout);
                resolve({
                    ok: true,
                    value: data.toString("utf-8"),
                });
                ws.close();
            });

            timeout = setTimeout(() => {
                ws.close();
                resolve({
                    ok: false,
                    error: "Timeout waiting for Emacs return",
                });
            }, timeoutSeconds * 1000);
        } catch (error) {
            resolve({
                ok: false,
                error: `Unexpected error: ${error.message}`,
            });
        }
    });
}

export function enableDebug(flag) {
    DEBUG = Boolean(flag);
}

function shutdown(wss) {
    console.log("Shutting down Backbone server...");
    // Close all active connections
    wss.clients.forEach((client) => {
        client.close();
    });
    // Close the WebSocket server itself
    wss.close((err) => {
        if (err) {
            console.error("Error while shutting down:", err);
        } else {
            console.log("WebSocket server closed successfully");
        }
    });
}

export function awaitForever(promise) {
    promise
        .then(() => {})
        .catch((error) => {
            console.error("Error in awaited promise:", error);
        });
    return null;
}

let INIT_START_TIME = null;

export function updateInitStartTime(start_time) {
    INIT_START_TIME = start_time;
}

export function getInitStartTime() {
    return INIT_START_TIME;
}

import {
    CommandEvent,
    HandleErrorEvent,
    ParseErrorEvent,
} from "./websocket.mjs";

import { updatePackageTracker } from "./package_tracker_ffi.mjs";

let DEBUG = false;
let serverInstance = null;

export function setupServer(backbonePort, messageHandler) {
    serverInstance = Bun.serve({
        port: parseInt(backbonePort),
        fetch(req, server) {
            // Upgrade HTTP request to WebSocket
            if (server.upgrade(req)) {
                return; // Return nothing if upgrade succeeds
            }
            return new Response("Upgrade failed", { status: 500 });
        },
        websocket: {
            open(ws) {
                // Connection opened
            },
            message(ws, data) {
                // Decode data to string
                const stringData = typeof data === "string" ? data : new TextDecoder().decode(data);
                let parsedData;
                try {
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
                    return;
                }

                const messageTag = parsedData[0];
                if (messageTag == "data") {
                    const command = parsedData[1];
                    try {
                        if (command[0] == "shutdown") {
                            shutdown();
                        } else if (
                            command[0] === "package_installed" &&
                            command.length > 1
                        ) {
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
            },
            error(ws, error) {
                messageHandler(new HandleErrorEvent("", error.message));
            },
            close(ws, code, reason) {
                // Connection closed
            },
        },
    });
}

export function setupClient(emacsPort) {
    return new Promise((resolve, reject) => {
        const ws = new WebSocket(`ws://127.0.0.1:${emacsPort}`);

        ws.addEventListener("error", (event) => {
            console.error("WebSocket error:", event);
        });

        ws.addEventListener("open", () => {
            console.log("Connected to Emacs server");
            resolve(ws);
        });

        ws.addEventListener("message", (event) => {
            console.error("Emacs Server: %s", event.data);
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

            ws.addEventListener("error", (event) => {
                clearTimeout(timeout);
                resolve({
                    ok: false,
                    error: `WebSocket error: ${event.message || "Connection failed"}`,
                });
            });

            ws.addEventListener("open", () => {
                if (DEBUG) console.debug("[getEmacsVar] fetch: %s", varName);

                const payload = {
                    type: "fetch-var",
                    content: varName,
                };
                ws.send(JSON.stringify(payload));
            });

            ws.addEventListener("message", (event) => {
                if (DEBUG) console.debug("[getEmacsVar] value: |%s|", event.data);

                clearTimeout(timeout);
                resolve({
                    ok: true,
                    value: event.data,
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

            ws.addEventListener("error", (event) => {
                clearTimeout(timeout);
                resolve({
                    ok: false,
                    error: `WebSocket error: ${event.message || "Connection failed"}`,
                });
            });

            ws.addEventListener("open", () => {
                if (DEBUG) console.debug("[callEmacsFunc] call: %s", code);

                const payload = {
                    type: "fetch-var",
                    content: code,
                };
                ws.send(JSON.stringify(payload));
            });

            ws.addEventListener("message", (event) => {
                if (DEBUG) console.debug("[callEmacsFunc] value: |%s|", event.data);

                clearTimeout(timeout);
                resolve({
                    ok: true,
                    value: event.data,
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

function shutdown() {
    console.log("Shutting down Backbone server...");
    if (serverInstance) {
        serverInstance.stop();
        console.log("WebSocket server closed successfully");
    }
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

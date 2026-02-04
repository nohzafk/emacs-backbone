import {
    RequestEvent,
    NotificationEvent,
    ParseErrorEvent,
    HandleErrorEvent,
} from "./jsonrpc.mjs";

import { updatePackageTracker } from "./package_tracker_ffi.mjs";

let DEBUG = false;
let requestId = 0;
const pendingRequests = new Map();
let messageHandler = null;

// --- Content-Length framed stdio transport ---

// Use Buffer for byte-accurate Content-Length parsing
let inputBuffer = Buffer.alloc(0);

function writeMessage(obj) {
    const json = JSON.stringify(obj);
    const byteLength = Buffer.byteLength(json, "utf-8");
    const header = `Content-Length: ${byteLength}\r\n\r\n`;
    process.stdout.write(header + json);
}

function parseMessages(data) {
    // Convert incoming data to Buffer if it's a string
    const chunk = typeof data === "string" ? Buffer.from(data, "utf-8") : data;
    inputBuffer = Buffer.concat([inputBuffer, chunk]);

    while (true) {
        // Find header end in the buffer
        const headerEndStr = "\r\n\r\n";
        const headerEnd = inputBuffer.indexOf(headerEndStr);
        if (headerEnd === -1) break;

        const header = inputBuffer.slice(0, headerEnd).toString("utf-8");
        const match = header.match(/Content-Length:\s*(\d+)/i);
        if (!match) {
            // Malformed header, skip to after \r\n\r\n
            inputBuffer = inputBuffer.slice(headerEnd + 4);
            continue;
        }

        const contentLength = parseInt(match[1], 10);
        const bodyStart = headerEnd + 4;

        if (inputBuffer.length < bodyStart + contentLength) {
            // Not enough data yet, wait for more
            break;
        }

        const body = inputBuffer.slice(bodyStart, bodyStart + contentLength).toString("utf-8");
        inputBuffer = inputBuffer.slice(bodyStart + contentLength);

        try {
            const msg = JSON.parse(body);
            handleIncomingMessage(msg);
        } catch (e) {
            if (messageHandler) {
                messageHandler(new ParseErrorEvent(body));
            }
        }
    }
}

function handleIncomingMessage(msg) {
    if (DEBUG) {
        const now = new Date();
        console.error(
            `[${now.getHours()}:${now.getMinutes()}:${now.getSeconds()}.${now.getMilliseconds()}]`,
            "recv:",
            JSON.stringify(msg),
        );
    }

    // Response to our outgoing request
    if (msg.id !== undefined && msg.id !== null && !msg.method) {
        const pending = pendingRequests.get(msg.id);
        if (pending) {
            clearTimeout(pending.timeout);
            pendingRequests.delete(msg.id);
            if (msg.error) {
                pending.resolve({
                    ok: false,
                    error: msg.error.message || JSON.stringify(msg.error),
                });
            } else {
                pending.resolve({
                    ok: true,
                    value: typeof msg.result === "string" ? msg.result : JSON.stringify(msg.result),
                });
            }
        }
        return;
    }

    if (!messageHandler) return;

    // Track package installation progress (works for both requests and notifications)
    trackPackageInstallation(msg);

    // Request from Emacs (has id + method)
    if (msg.id !== undefined && msg.id !== null && msg.method) {
        messageHandler(new RequestEvent(msg.id, msg.method, msg.params || {}));
        return;
    }

    // Notification from Emacs (no id, has method)
    if (msg.method) {
        messageHandler(
            new NotificationEvent(msg.method, msg.params || {}),
        );
        return;
    }

    messageHandler(
        new HandleErrorEvent("unknown", "Unrecognized message format"),
    );
}

/**
 * Track package installation progress for package_installed messages.
 * Called for both requests and notifications to log installation progress.
 */
function trackPackageInstallation(msg) {
    if (msg.method !== "package_installed" || !msg.params) return;

    const packageName = msg.params.name;
    if (!packageName) return;

    const packageTracker = updatePackageTracker(packageName);
    console.error(
        `[${packageTracker.installed.length}/${packageTracker.total}] ${packageName}`,
    );

    if (DEBUG) {
        const pendingList =
            packageTracker.pending.length > 0
                ? packageTracker.pending.slice(0, 5).join(", ") +
                  (packageTracker.pending.length > 5
                      ? `, ... (+${packageTracker.pending.length - 5} more)`
                      : "")
                : "none";
        console.error(
            `  ${packageTracker.pending.length} remaining: ${pendingList}`,
        );
    }
}

// --- Exported functions ---

export function setupStdioServer(handler) {
    messageHandler = handler;

    // Handle stdin close (Emacs killed the process)
    const stdin = Bun.stdin.stream();
    const reader = stdin.getReader();

    (async () => {
        try {
            while (true) {
                const { done, value } = await reader.read();
                if (done) {
                    console.error("stdin closed, shutting down");
                    process.exit(0);
                }
                // Pass raw bytes/buffer to parseMessages - it handles byte-accurate Content-Length parsing
                parseMessages(value);
            }
        } catch (e) {
            console.error("stdin read error:", e.message);
            process.exit(1);
        }
    })();
}

export function sendNotification(method, params) {
    try {
        const msg = {
            jsonrpc: "2.0",
            method,
            params,
        };
        if (DEBUG) console.error(`[notify] ${method}`, params);
        writeMessage(msg);
        return Promise.resolve({ ok: true, value: "" });
    } catch (error) {
        return Promise.resolve({
            ok: false,
            error: `Failed to send notification: ${error.message}`,
        });
    }
}

export function sendRequest(method, params, timeoutMs) {
    const id = ++requestId;
    return new Promise((resolve) => {
        const timeout = setTimeout(() => {
            pendingRequests.delete(id);
            resolve({
                ok: false,
                error: `Timeout waiting for response to ${method}`,
            });
        }, timeoutMs);

        pendingRequests.set(id, { resolve, timeout });

        const msg = {
            jsonrpc: "2.0",
            id,
            method,
            params,
        };
        if (DEBUG) console.error(`[request] id=${id} ${method}`, params);
        writeMessage(msg);
    });
}

// --- Gleam-friendly wrappers that accept primitive arguments ---

export function showMessage(message) {
    return sendNotification("show-message", { content: message });
}

export function evalCode(code) {
    return sendNotification("eval-code", { content: code });
}

export function fetchVar(expr, timeoutMs) {
    return sendRequest("fetch-var", { expr }, timeoutMs);
}

export function sendResponse(id, result) {
    const msg = {
        jsonrpc: "2.0",
        id,
        result,
    };
    if (DEBUG) console.error(`[response] id=${id}`, result);
    writeMessage(msg);
}

export function sendErrorResponse(id, code, message) {
    const msg = {
        jsonrpc: "2.0",
        id,
        error: { code, message },
    };
    if (DEBUG) console.error(`[error-response] id=${id}`, code, message);
    writeMessage(msg);
}

export function enableDebug(flag) {
    DEBUG = Boolean(flag);
}

let INIT_START_TIME = null;

export function updateInitStartTime(startTime) {
    INIT_START_TIME = startTime;
}

export function getInitStartTime() {
    return INIT_START_TIME;
}

/**
 * Gracefully shut down the stdio server.
 * Allows pending operations to complete before exiting.
 */
export function shutdown() {
    // Use setImmediate to allow any pending I/O to flush
    setImmediate(() => {
        process.exit(0);
    });
}

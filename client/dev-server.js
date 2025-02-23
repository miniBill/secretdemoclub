import * as http from "node:http";
import elmWatch from "elm-watch";

try {
    const exitCode = await elmWatch(["hot"], {
        createServer: ({ onRequest, onUpgrade }) =>
            http
                .createServer((request, response) => {
                    if (request.url.startsWith("/api?")) {
                        // Proxy /api?* to localhost:3000.
                        localhostProxy(request, response, 3000);
                    } else {
                        // Let elm-watch’s server do its thing for all other URLs.
                        onRequest(request, response);
                    }
                })
                .on("upgrade", onUpgrade),
    });
    process.exit(exitCode);
} catch (error) {
    console.error("Unexpected elm-watch error:", error);
}

function localhostProxy(request, response, port) {
    const options = {
        hostname: "127.0.0.1",
        port,
        path: request.url,
        method: request.method,
        headers: request.headers,
    };

    const proxyRequest = http.request(options, (proxyResponse) => {
        response.writeHead(proxyResponse.statusCode, proxyResponse.headers);
        proxyResponse.pipe(response, { end: true });
    });

    proxyRequest.on("error", (error) => {
        response.writeHead(503);
        response.end(
            `Failed to proxy to localhost:${port}. Is nothing running there?\n\n${error.stack}`
        );
    });

    request.pipe(proxyRequest, { end: true });
}

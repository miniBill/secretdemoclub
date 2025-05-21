self.addEventListener("install", function (/** @type {ExtendableEvent} */ e) {
    console.log("Service worker installed:", e);
});

self.addEventListener("activate", function (/** @type {ExtendableEvent} */ e) {
    console.log("Service worker activated:", e);
});

self.addEventListener("fetch", function (/** @type {FetchEvent} */ e) {
    if (e.request.method !== "GET") return;

    const url = new URL(e.request.url);
    if (url.pathname !== "/download") return;

    let urls;
    try {
        urls = JSON.parse(url.searchParams.get("urls"));
    } catch {
        return;
    }
    if (!urls) return;

    debugger;

    e.respondWith(
        (async () => {
            return fetch(e.request);
        })()
    );
});

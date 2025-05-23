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

    let files;
    try {
        files = JSON.parse(url.searchParams.get("files"));
    } catch {
        return;
    }
    if (!files) return;

    const { writable, readable } = new TransformStream();

    e.waitUntil(
        (async () => {
            for (const { filename, url } of files) {
                const response = await fetch(url);
                await response.body.pipeTo(writable, { preventClose: true });
            }
            writable.close();
        })()
    );

    e.respondWith(
        new Response(readable, {
            headers: {
                "Content-Type": "application/x-tar",
                "Content-Disposition": "attachment; filename=sdc-download.tar",
            },
        })
    );
});

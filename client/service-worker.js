self.addEventListener("install", function (/** @type {ExtendableEvent} */ e) {
    console.log("Service worker installed:", e);
});

self.addEventListener("activate", function (/** @type {ExtendableEvent} */ e) {
    console.log("Service worker activated:", e);
});

self.addEventListener("fetch", function (/** @type {FetchEvent} */ e) {
    if (e.request.method !== "GET") {
        return;
    }

    const url = new URL(e.request.url);
    if (url.pathname !== "/download") {
        return;
    }

    let files;
    try {
        files = JSON.parse(url.searchParams.get("files"));
    } catch {
        return;
    }
    if (!files) {
        return;
    }

    e.respondWith(tarFiles(e, files));
});

/**
 * @param {{ filename: string; mtime: number; url: string; }[]} files
 * @param {FetchEvent} e
 */
async function tarFiles(e, files) {
    try {
        const { writable, readable } = new TransformStream();

        let responses = [];
        let contentSize = 0;
        for (const { filename, mtime, url } of files) {
            const response = await fetch(url);

            let length = Number(response.headers.get("Content-Length"));
            if (!length) {
                throw `Missing Content-Length for ${url}`;
            }

            contentSize += length;

            const header = buildHeader(filename, length, mtime);

            responses.push({ header, body: response.body, length });
        }

        e.waitUntil(
            (async () => {
                for (const { header, body } of responses) {
                    let writer = writable.getWriter();
                    writer.write(header);
                    writer.releaseLock();

                    await body.pipeTo(writable, {
                        preventClose: true,
                    });
                }

                const end = new Uint8Array(1024);
                let writer = writable.getWriter();
                writer.write(end);

                writable.close();
            })()
        );

        let headers = {
            "Content-Type": "application/x-tar",
            "Content-Disposition": "attachment; filename=sdc-download.tar",
            "Content-Length": (
                512 * (responses.length + 2) +
                contentSize
            ).toString(),
        };

        return new Response(readable, { headers });
    } catch (error) {
        return new Response(error, {
            status: 502,
            statusText: "Error while fetching files",
        });
    }
}

/**
 * @param {string} filename
 * @param {number} length
 * @param {number} mtime
 */
function buildHeader(filename, length, mtime) {
    const utf8Encoder = new TextEncoder();

    const header = new Uint8Array(512);
    let offset = 0;

    /**
     * @param {Uint8Array} bytes
     * @param {number} length
     */
    function writeBytes(bytes, length) {
        header.set(bytes, offset);
        offset += length;
    }

    /**
     * @param {string} name
     * @param {string} input
     * @param {number} length
     */
    function writeString(name, input, length) {
        let result = utf8Encoder.encode(input);
        if (result.length > length) {
            throw `${name} too long`;
        }
        writeBytes(result, length);
    }

    /**
     * @param {string} name
     * @param {number} [input]
     * @param {number} [length]
     */
    function writeNumber(name, input, length) {
        let padded = input.toString(8).padStart(length - 1, "0");
        let result = utf8Encoder.encode(padded);
        if (result.length > length) {
            throw `${name} too long: ${input} => \"${padded}\" [${result.length} > ${length}]`;
        }
        writeBytes(result, length);
    }

    writeString("filename", filename, 100);
    writeNumber("mode", 0o644, 8);
    writeNumber("uid", 1000, 8);
    writeNumber("gid", 1000, 8);
    writeNumber("size", length, 12);
    writeNumber("mtime", mtime, 12); // Seconds since epoch
    writeString("chksum", "        ", 8);
    writeNumber("typeflag", 0, 1);
    writeString("linkname", "", 100);
    writeString("magic", "ustar", 6);
    writeString("version", "00", 2);
    writeString("uname", "orla", 32);
    writeString("gname", "gartland", 32);
    writeNumber("devmajor", 0, 8);
    writeNumber("devminor", 0, 8);
    writeString("prefix", "", 155);

    let checksum = 0;
    for (let i = 0; i < header.length; i++) {
        checksum += header[i];
    }

    offset = 148;
    writeNumber("chksum", checksum, 8);

    return header;
}

const utf8Encoder = new TextEncoder();

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

    e.respondWith(zipResponse(e, "sdc-download.zip", files));
});

const crc32Lookup = Uint32Array.from({ length: 256 }, (_, c) => {
    for (let _ = 0; _ < 8; _++) c = ((c & 1) * 0xedb88320) ^ (c >>> 1);
    return c;
});

/**
 * @type {Transformer<Uint8Array, Uint8Array>}
 */
class Crc32Transformer {
    constructor() {
        this._crc = 0xdebb20e3;
        this.length = 0;
        // this._crc = ~0xdebb20e3;
    }

    start() {}

    /**
     * @param {Uint8Array} chunk
     * @param {TransformStreamDefaultController<Uint8Array>} controller
     */
    async transform(chunk, controller) {
        for (let i = 0; i < chunk.length; i++)
            this._crc =
                (this._crc >>> 8) ^ crc32Lookup[(this._crc ^ chunk[i]) & 0xff];
        this.length += chunk.length;
        controller.enqueue(chunk);
    }

    crc32() {
        return ~this._crc >>> 0;
    }

    flush() {}
}

class Crc32TransformStream extends TransformStream {
    constructor() {
        const transformer = new Crc32Transformer();
        super(transformer);
        this.transformer = transformer;
    }

    crc32() {
        return this.transformer.crc32();
    }

    length() {
        return this.transformer.length;
    }
}

/**
 * @param {{filename: string;mtime: number;url: string;}[]} files
 * @param {FetchEvent} e
 * @param {string} filename
 */
async function zipResponse(e, filename, files) {
    try {
        const { writable, readable } = new TransformStream();

        /**
         * @param {WritableStream<any>} writable
         * @param {any} block
         */
        async function writeTo(writable, block) {
            let writer = writable.getWriter();
            await writer.write(block);
            writer.releaseLock();
        }

        e.waitUntil(
            (async () => {
                let centralDirectoryHeaders = [];
                let offset = 0;

                for (const { filename, url, mtime } of files) {
                    const fetching = fetch(url);

                    const localHeader = buildLocalFileHeader(filename, mtime);

                    await writeTo(writable, localHeader);

                    let crcStream = new Crc32TransformStream();
                    await (await fetching).body
                        .pipeThrough(crcStream)
                        .pipeTo(writable, {
                            preventClose: true,
                        });

                    let length = crcStream.length();
                    let crc32 = crcStream.crc32();

                    const dataDescriptor = buildDataDescriptor(crc32, length);
                    await writeTo(writable, dataDescriptor);

                    const centralDirectoryHeader = buildCentralDirectoryHeader(
                        filename,
                        mtime,
                        crc32,
                        length,
                        offset
                    );
                    centralDirectoryHeaders.push(centralDirectoryHeader);

                    offset += localHeader.length;
                    offset += length;
                    offset += dataDescriptor.length;
                }

                let centralDirectorySize = 0;
                for (const centralDirectoryHeader of centralDirectoryHeaders) {
                    await writeTo(writable, centralDirectoryHeader);
                    centralDirectorySize += centralDirectoryHeader.length;
                }

                const endOfCentralDirectoryRecord =
                    buildEndOfCentralDirectoryRecord(
                        centralDirectoryHeaders.length,
                        centralDirectorySize,
                        offset
                    );
                await writeTo(writable, endOfCentralDirectoryRecord);

                writable.close();
            })()
        );

        let headers = {
            "Content-Type": "application/zip",
            "Content-Disposition": `attachment; filename=${filename}`,
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
 * @param {number} mtime
 */
function buildLocalFileHeader(filename, mtime) {
    let filenameLength = utf8Encoder.encode(filename).length;

    const header = new Uint8Array(30 + filenameLength);
    const view = new DataView(
        header.buffer,
        header.byteOffset,
        header.byteLength
    );
    let offset = 0;

    /**
     * @param {Uint8Array} bytes
     */
    function writeBytes(bytes) {
        header.set(bytes, offset);
        offset += bytes.length;
    }

    /**
     * @param {string} input
     */
    function writeString(input) {
        let result = utf8Encoder.encode(input);
        writeBytes(result);
    }

    /**
     * @param {number} [input]
     */
    function writeU16(input) {
        view.setUint16(offset, input, true);
        offset += 2;
    }

    /**
     * @param {number} [input]
     */
    function writeU32(input) {
        view.setUint32(offset, input, true);
        offset += 4;
    }

    let mtimeDate = new Date(mtime);

    writeString("PK\x03\x04"); // Signature
    writeU16(10); // Version: 1.0
    writeU16(1 << 3); // Flags: "data descriptor"
    writeU16(0); // Compression: none
    writeU16(
        (mtimeDate.getSeconds() / 2) |
            (mtimeDate.getMinutes() << 5) |
            (mtimeDate.getHours() << 11)
    ); // File modification time, in DOS format
    writeU16(
        mtimeDate.getDay() |
            (mtimeDate.getMonth() << 5) |
            ((mtimeDate.getFullYear() - 1980) << 9)
    );
    writeU32(0); // CRC is postponed
    writeU32(0); // Compressed size is postponed
    writeU32(0); // Uncompressed size is postponed
    writeU16(filenameLength);
    writeU16(0); // Extra field length
    writeString(filename);
    // No extra field

    return header;
}

/**
 * @param {number} crc32
 * @param {number} length
 */
function buildDataDescriptor(crc32, length) {
    const descriptor = new Uint8Array(12);
    const view = new DataView(
        descriptor.buffer,
        descriptor.byteOffset,
        descriptor.byteLength
    );

    view.setUint32(0, crc32, true); // CRC
    view.setUint32(4, length, true); // Compressed size
    view.setUint32(8, length, true); // Uncompressed size

    return descriptor;
}

/**
 * @param {string} filename
 * @param {number} mtime
 * @param {number} crc32
 * @param {number} length
 * @param {number} localHeaderOffset
 */
function buildCentralDirectoryHeader(
    filename,
    mtime,
    crc32,
    length,
    localHeaderOffset
) {
    let filenameLength = utf8Encoder.encode(filename).length;

    const header = new Uint8Array(46 + filenameLength);
    const view = new DataView(
        header.buffer,
        header.byteOffset,
        header.byteLength
    );
    let offset = 0;

    /**
     * @param {Uint8Array} bytes
     */
    function writeBytes(bytes) {
        header.set(bytes, offset);
        offset += bytes.length;
    }

    /**
     * @param {string} input
     */
    function writeString(input) {
        let result = utf8Encoder.encode(input);
        writeBytes(result);
    }

    /**
     * @param {number} [input]
     */
    function writeU8(input) {
        view.setUint8(offset, input);
        offset++;
    }

    /**
     * @param {number} [input]
     */
    function writeU16(input) {
        view.setUint16(offset, input, true);
        offset += 2;
    }

    /**
     * @param {number} [input]
     */
    function writeU32(input) {
        view.setUint32(offset, input, true);
        offset += 4;
    }

    let mtimeDate = new Date(mtime);

    writeString("PK\x01\x02"); // Signature
    writeU8(63); // Spec version. Copied from what Ark outputs 🤷
    writeU8(3); // UNIX
    writeU16(10); // Version: 1.0
    writeU16(1 << 3); // Flags: "data descriptor"
    writeU16(0); // Compression: none
    writeU16(
        (mtimeDate.getSeconds() / 2) |
            (mtimeDate.getMinutes() << 5) |
            (mtimeDate.getHours() << 11)
    ); // File modification time, in DOS format
    writeU16(
        mtimeDate.getDay() |
            (mtimeDate.getMonth() << 5) |
            ((mtimeDate.getFullYear() - 1980) << 9)
    );
    writeU32(crc32);
    writeU32(length); // Compressed size
    writeU32(length); // Uncompressed size
    writeU16(filenameLength);
    writeU16(0); // Extra field length
    writeU16(0); // File comment length
    writeU16(0); // Disk #
    writeU16(0); // Internal attributes: none
    writeU32(0); // External attributes: none
    writeU32(localHeaderOffset);
    writeString(filename);
    // No extra field
    // No file comment

    return header;
}

/**
 * @param {number} entries
 * @param {number} length
 * @param {number} centralDirectoryOffset
 */
function buildEndOfCentralDirectoryRecord(
    entries,
    length,
    centralDirectoryOffset
) {
    const header = new Uint8Array(22);
    const view = new DataView(
        header.buffer,
        header.byteOffset,
        header.byteLength
    );
    let offset = 0;

    /**
     * @param {Uint8Array} bytes
     */
    function writeBytes(bytes) {
        header.set(bytes, offset);
        offset += bytes.length;
    }

    /**
     * @param {string} input
     */
    function writeString(input) {
        let result = utf8Encoder.encode(input);
        writeBytes(result);
    }

    /**
     * @param {number} [input]
     */
    function writeU16(input) {
        view.setUint16(offset, input, true);
        offset += 2;
    }

    /**
     * @param {number} [input]
     */
    function writeU32(input) {
        view.setUint32(offset, input, true);
        offset += 4;
    }

    writeString("PK\x05\x06"); // Signature
    writeU16(0); // Disk #
    writeU16(0); // Disk with central directory record
    writeU16(entries); // Number of CD entries on this disk
    writeU16(entries); // Number of CD entries in total
    writeU32(length);
    writeU32(centralDirectoryOffset);
    writeU16(0); // Comment length

    return header;
}

import fs from "fs";

function fileExists(path) {
    return fs.existsSync(path);
}

export { fileExists };

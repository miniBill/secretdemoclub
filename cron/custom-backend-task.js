import fs from "fs";

function fileExists(path) {
    return fs.existsSync(path);
}

function profile(label) {
    console.profile(label);
}

function profileEnd(label) {
    console.profileEnd(label);
}

export { fileExists, profile, profileEnd };

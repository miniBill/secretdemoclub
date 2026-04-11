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

function setupDebugger() {
    const original = console.log;
    console.log = (...data) => {
        if (data[0] === "!!!BREAK!!!: ()") {
            debugger;
        } else {
            original(...data);
        }
    };
}

export { fileExists, profile, profileEnd, setupDebugger };

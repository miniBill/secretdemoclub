import { showDirectoryPicker } from "native-file-system-adapter";

/** @type {{ [key: string]: string }} flags */
const flags = {};
for (var i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key) {
        continue;
    }
    flags[key] = JSON.parse(localStorage[key]);
}

/**
 * @typedef {{ subscribe: ( handler: ( arg: T ) => void ) => void }} ElmToJs<T>
 * @template {type} T
 */

/** @typedef {{ sendToLocalStorage: ElmToJs<{key : string, value: string}>, saveFiles: ElmToJs<{url : string, filename: string}[]>}} Ports */
/** @typedef {{ ports: Ports }} ElmApp */
/** @type {ElmApp} app */
const app = Elm.Main.init({
    flags: flags,
    node: document.querySelector("main"),
});

if (app.ports) {
    if (app.ports.sendToLocalStorage) {
        app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
            window.localStorage[key] = JSON.stringify(value);
        });
    }
    if (app.ports.saveFiles) {
        app.ports.saveFiles.subscribe(async (files) => {
            const directory = await showDirectoryPicker();

            for (const { url, filename } of files) {
                const response = await fetch(url);

                const handle = await directory.getFileHandle(filename, {
                    create: true,
                });
                const writer = await handle.createWritable();

                await response.body.pipeTo(writer);
            }
        });
    }
}

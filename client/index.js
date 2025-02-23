/** @type {{ [key: string]: string }} flags */
const flags = {};
for (var i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key) {
        continue;
    }
    flags[key] = JSON.parse(localStorage[key]);
}

/** @typedef {{ sendToLocalStorage: { subscribe: ( handler: ( arg: { key: string, value: string } ) => void ) => void }}} Ports */
/** @typedef {{ ports: Ports }} ElmApp */
/** @type {ElmApp} app */
const app = Elm.Main.init({
    flags: flags,
    node: document.querySelector("main"),
});

if (app.ports && app.ports.sendToLocalStorage) {
    app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
        window.localStorage[key] = JSON.stringify(value);
    });
}

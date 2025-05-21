/** @typedef {{ [key: string]: string }} Flags */
/** @type Flags flags */
const flags = {};

for (var i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key) {
        continue;
    }
    flags[key] = JSON.parse(localStorage[key]);
}

/**
 * @template T
 * @typedef {{ subscribe: ( handler: ( arg: T ) => void ) => void }} ElmToJs<T>
 */

/**
 * @template T
 * @typedef {{ call: ( arg: T ) => void }} JsToElm<T>
 */

/** @typedef {{ sendToLocalStorage: ElmToJs<{key : string, value: string}> }} Ports */
/** @typedef {{ ports: Ports }} ElmApp */

/** @type {ElmApp} app */
// @ts-ignore
const app = Elm.Main.init({
    flags: flags,
    node: document.querySelector("main"),
});

app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
    window.localStorage[key] = JSON.stringify(value);
});

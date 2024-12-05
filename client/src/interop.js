export const flags = ({ env }) => {
    // Called before our Elm application starts
    const result = {};
    for (var i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i);
        result[key] = JSON.parse(localStorage[key]);
    }
    return result;
};

export const onReady = ({ env, app }) => {
    // Called after our Elm application starts
    if (app.ports && app.ports.sendToLocalStorage) {
        app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
            window.localStorage[key] = JSON.stringify(value);
        });
    }
};

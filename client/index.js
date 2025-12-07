const darkModePreference = window.matchMedia("(prefers-color-scheme: dark)");
/** @type Flags flags */
const flags = {
    theme: darkModePreference.matches ? "dark" : "light",
};

for (var i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key) {
        continue;
    }
    flags[key] = JSON.parse(localStorage[key]);
}

const app = Elm.Main.init({
    flags: flags,
    node: document.querySelector("main"),
});

app.ports.sendToLocalStorage.subscribe(({ key, value }) => {
    localStorage[key] = JSON.stringify(value);
});

darkModePreference.addEventListener("change", (e) =>
    app.ports.prefersColorSchemeDark.send(e.matches)
);

if ("serviceWorker" in navigator) {
    try {
        const registration = await navigator.serviceWorker.register(
            "/service-worker.js"
        );

        let serviceWorker;
        if (registration.installing) {
            serviceWorker = registration.installing;
        } else if (registration.waiting) {
            serviceWorker = registration.waiting;
        } else if (registration.active) {
            serviceWorker = registration.active;
            app.ports.serviceWorkerRegistrationSuccess.send({});
        }

        if (serviceWorker) {
            serviceWorker.addEventListener("statechange", () => {
                if (serviceWorker.state == "activated") {
                    app.ports.serviceWorkerRegistrationSuccess.send({});
                }
            });
        }
    } catch (error) {
        // Sadge
        console.error(error);
    }
}

export {};

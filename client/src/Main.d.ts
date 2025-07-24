interface Flags {
    [key: string]: string;
}

interface ElmToJs<T> {
    subscribe: (handler: (arg: T) => void) => void;
}

interface JsToElm<T> {
    send: (arg: T) => void;
}

interface ElmApp {
    ports: {
        sendToLocalStorage: ElmToJs<{ key: string; value: string }>;
        serviceWorkerRegistrationSuccess: JsToElm<{}>;
    };
}

interface Elm {
    Main: { init: (arg: { flags: Flags; node: HTMLElement }) => ElmApp };
}

declare var Elm: Elm;

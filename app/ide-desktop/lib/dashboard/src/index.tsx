/** @file Index file declaring main DOM structure for the app. */

import * as authentication from "enso-studio-authentication";

import * as app from "./authentication/src/components/app";

const props: app.AppProps = {
    logger: console,
    // This package is a standalone React app (i.e., IDE deployed to the Cloud), so we're not
    // running on the desktop.
    runningOnDesktop: false,
    onAuthenticated: () => {},
};

// For whatever reason, TypeScript fails to infer the type of `authentication.run` correctly, so we
// need to disable the type checking for this line. This is especially weird since VSCode shows the
// correct type for `authentication.run` when hovering over it.
authentication.run(props);

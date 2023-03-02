/** @file Authentication module used by Enso IDE & Cloud.
 *
 * This module declares the main DOM structure for the authentication/dashboard app. */
// This import is unused, but React doesn't work without it in Electron. This is probably because it
// gets tree-shaken out of the bundle, so we need to explicitly import it.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
import * as React from "react";
import * as reactDOM from "react-dom/client";

import App from "./components/app";
import * as app from "./components/app";
import "./styles/index.css";



// ===========
// === run ===
// ===========

/** Entrypoint for the authentication/dashboard app.
 *
 * Running this function finds a `div` element with the ID `authentication`, and renders the
 * authentication/dashboard UI using React. It also handles routing and other interactions (e.g.,
 * for redirecting the user to/from the login page). */
export const run = (props: AppProps) => {
    const { logger } = props;
    logger.log("Starting authentication/dashboard UI.");

    // The `id` attribute of the root element that the app will be rendered into.
    const rootElementId = "authentication";
    // The root element that the authentication/dashboard app will be rendered into.
    //
    // Return interface for `getElementById` is `HTMLElement` or `null`. Since we are fetching the
    // `authentication` element, and that element is expected to always be present in the `index.html`,
    // we can disable the `no-non-null-assertion` on this line.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const root: HTMLElement = document.getElementById(rootElementId)!;
    reactDOM.createRoot(root).render(<App {...props} />);
};

export type AppProps = app.AppProps;

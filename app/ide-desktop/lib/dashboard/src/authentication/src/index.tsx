/** @file Authentication module used by Enso IDE & Cloud.
 *
 * This module declares the main DOM structure for the authentication/dashboard app. */
/** This import is unused in this file, but React doesn't work without it, under Electron. This is
 * probably because it gets tree-shaken out of the bundle, so we need to explicitly import it.
 * Unlike all other imports in this project, this one is not `pascalCase`. We use `React` instead of
 * `react` here. This is because if the import is named any differently then React doesn't get
 * included in the final bundle. */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
import * as React from "react";
import * as reactDOM from "react-dom/client";

import App, * as app from "./components/app";
import * as loggerProvider from "./providers/logger";
import "./styles/index.css";

// ===========
// === run ===
// ===========

/** Entrypoint for the authentication/dashboard app.
 *
 * Running this function finds a `div` element with the ID `dashboard`, and renders the
 * authentication/dashboard UI using React. It also handles routing and other interactions (e.g.,
 * for redirecting the user to/from the login page). */
export const run = (
  /** Logger to use for logging. */
  logger: loggerProvider.Logger,
  platform: app.Platform,
  onAuthenticated: () => void
) => {
  logger.log("Starting authentication/dashboard UI.");
  /** The `id` attribute of the root element that the app will be rendered into. */
  const rootElementId = "dashboard";
  /** The root element that the authentication/dashboard app will be rendered into. */
  const root = document.getElementById(rootElementId);
  if (root == null) {
    logger.error(`Could not find root element with ID '${rootElementId}'.`);
    return;
  }
  const props = { logger, platform, onAuthenticated };
  reactDOM.createRoot(root).render(<App {...props} />);
};

export type AppProps = app.AppProps;
export const Platform = app.Platform;

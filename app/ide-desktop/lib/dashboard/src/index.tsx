/** @file Index file declaring main DOM structure for the app. */

import * as authentication from "enso-studio-authentication";

import * as app from "./authentication/src/components/app";

const props: app.AppProps = {
  logger: console,
  /** This package is a standalone React app (i.e., IDE deployed to the Cloud), so we're not
   * running on the desktop. */
  runningOnDesktop: false,
  onAuthenticated: () => {},
};

authentication.run(props);

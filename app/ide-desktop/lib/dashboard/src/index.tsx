/** @file Index file declaring main DOM structure for the app. */

import * as authentication from "enso-studio-authentication";

import * as app from "./authentication/src/components/app";

const logger = console;
/** This package is a standalone React app (i.e., IDE deployed to the Cloud), so we're not
 * running on the desktop. */
const platform = app.Platform.cloud;
const onAuthenticated = () => {};

authentication.run(logger, platform, onAuthenticated);

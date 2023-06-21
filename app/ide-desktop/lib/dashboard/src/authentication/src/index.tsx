/** @file Authentication module used by Enso IDE & Cloud.
 *
 * This module declares the main DOM structure for the authentication/dashboard app. */
/** This import is unused in this file, but React doesn't work without it, under Electron. This is
 * probably because it gets tree-shaken out of the bundle, so we need to explicitly import it.
 * Unlike all other imports in this project, this one is not `camelCase`. We use `React` instead of
 * `react` here. This is because if the import is named any differently then React doesn't get
 * included in the final bundle. */
// It is safe to disable `no-restricted-syntax` because the `PascalCase` naming is required
// as per the above comment.
// eslint-disable-next-line @typescript-eslint/no-unused-vars, no-restricted-syntax
import * as React from 'react'
import * as reactDOM from 'react-dom/client'

import * as detect from 'enso-common/src/detect'

import App, * as app from './components/app'

// =================
// === Constants ===
// =================

/** The `id` attribute of the root element that the app will be rendered into. */
const ROOT_ELEMENT_ID = 'enso-dashboard'
/** The `id` attribute of the element that the IDE will be rendered into. */
const IDE_ELEMENT_ID = 'root'

// ===========
// === run ===
// ===========

/** Entrypoint for the authentication/dashboard app.
 *
 * Running this function finds a `div` element with the ID `dashboard`, and renders the
 * authentication/dashboard UI using React. It also handles routing and other interactions (e.g.,
 * for redirecting the user to/from the login page). */
export // This export declaration must be broken up to satisfy the `require-jsdoc` rule.
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax
function run(props: app.AppProps) {
    const { logger, supportsDeepLinks } = props
    logger.log('Starting authentication/dashboard UI.')
    /** The root element that the authentication/dashboard app will be rendered into. */
    const root = document.getElementById(ROOT_ELEMENT_ID)
    const ideElement = document.getElementById(IDE_ELEMENT_ID)
    if (root == null) {
        logger.error(`Could not find root element with ID '${ROOT_ELEMENT_ID}'.`)
    } else if (ideElement == null) {
        logger.error(`Could not find IDE element with ID '${IDE_ELEMENT_ID}'.`)
    } else {
        ideElement.style.top = '-100vh'
        ideElement.style.display = 'fixed'
        // `supportsDeepLinks` will be incorrect when accessing the installed Electron app's pages
        // via the browser.
        const actuallySupportsDeepLinks = supportsDeepLinks && detect.isRunningInElectron()
        reactDOM
            .createRoot(root)
            .render(<App {...props} supportsDeepLinks={actuallySupportsDeepLinks} />)
    }
}

/** Global configuration for the {@link App} component. */
export type AppProps = app.AppProps

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

import * as platformModule from './platform'
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
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax
export function run(props: app.AppProps) {
    const { logger } = props
    logger.log('Starting authentication/dashboard UI.')
    /** The root element that the authentication/dashboard app will be rendered into. */
    const root = document.getElementById(ROOT_ELEMENT_ID)
    if (root == null) {
        logger.error(`Could not find root element with ID '${ROOT_ELEMENT_ID}'.`)
    } else {
        // FIXME: https://github.com/enso-org/cloud-v2/issues/386
        // Temporary workaround on hiding the Enso root element preventing it from
        // rendering next to authentication templates. We are uncovering this once the
        // authentication library sets the user session.
        const ide = document.getElementById(IDE_ELEMENT_ID)
        if (ide != null) {
            ide.style.display = 'none'
        }
        reactDOM.createRoot(root).render(<App {...props} />)
    }
}

export type AppProps = app.AppProps
// This export should be `PascalCase` because it is a re-export.
// eslint-disable-next-line no-restricted-syntax
export const Platform = platformModule.Platform

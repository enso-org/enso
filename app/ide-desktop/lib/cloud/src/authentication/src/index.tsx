// FIXME [NP]: Make all comments match the style of the first comment in this file: https://github.com/enso-org/enso/pull/5716/files#r1113048958
// FIXME [NP]: Find all locations we throw and error and update their docs accordingly: https://github.com/enso-org/enso/pull/5716/files#r1113052508
// FIXME [NP]: Find all locations where we use `{ ...  }` style in props and update them to use a variable that we deconstruct later: https://github.com/enso-org/enso/pull/5716/files#r1113049785
// FIXME [NP]: Find all locations where we create a named variable just to return it and remove them: https://github.com/enso-org/enso/pull/5716/files#r1113053192
// FIXME [NP]: Group all constants: https://github.com/enso-org/enso/pull/5716/files#r1113064368
// FIXME [NP]: Replace all imports with `import * as ... from ...` instead of `import { ... } from ...`: https://github.com/enso-org/enso/pull/5716/files#r1113059306
// FIXME [NP]: Rename this to `enso-cloud-dashboard` and move it to `app/ide-desktop/lib/dashboard`
// FIXME [NP]: Remove the `app/ide-desktop/.eslintrc.cjs` file
// FIXME [NP]: Remove the `@typescript-eslint/eslint-plugin` dependency from parent `package.json`
// FIXME [NP]: Remove the `@typescript-eslint/parser` dependency from parent `package.json`
// FIXME [NP]: Remove the `eslint` dependency from parent `package.json`
// FIXME [NP]: Remove the `eslint-plugin-jsdoc` dependency from parent `package.json`
// FIXME [NP]: Rebuild the parent `package.json` file and commit the reverted `package-lock.json` file
// FIXME [NP]: Move this back to lib/authentication once we figure out how to remove the create-react-app restriction on TypeScript sources outside of src/

// FIXME [NP]: Remove the "strictNullChecks" rule from the parent `tsconfig.json` file
// FIXME [NP]: Remove the `../tsconfig.json` file.
/** @file Authentication module used by Enso IDE & Cloud.
 *
 * This module declares the main DOM structure for the authentication/dashboard app.
 */

import * as React from 'react'
import * as ReactDOM from 'react-dom/client'

import App from './components/app'
import * as app from './components/app'
import "./styles/index.css"



// ===========
// === run ===
// ===========

/**
 * Entrypoint for the authentication/dashboard app.
 * 
 * Running this function finds a `div` element with the ID `authentication`, and renders the
 * authentication/dashboard UI using React. It also handles routing and other interactions (e.g.,
 * for redirecting the user to/from the login page).
 */
export const run = (props: AppProps) => {
    const { logger } = props
    logger.log("Starting authentication/dashboard UI.")

    // The `id` attribute of the root element that the app will be rendered into.
    const rootElementId = 'authentication'
    // The root element that the authentication/dashboard app will be rendered into.
    //
    // Return interface for `getElementById` is `HTMLElement` or `null`. Since we are fetching the
    // `authentication` element, and that element is expected to always be present in the `index.html`,
    // we can disable the `no-non-null-assertion` on this line.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const root: HTMLElement = document.getElementById(rootElementId)!
    ReactDOM.createRoot(root).render(<App {...props} />);
}

export type AppProps = app.AppProps

export default { run }

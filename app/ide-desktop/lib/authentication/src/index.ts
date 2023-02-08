// FIXME [NP]: Remove the `app/ide-desktop/.eslintrc.cjs` file
// FIXME [NP]: Remove the `@typescript-eslint/eslint-plugin` dependency from parent `package.json`
// FIXME [NP]: Remove the `@typescript-eslint/parser` dependency from parent `package.json`
// FIXME [NP]: Remove the `eslint` dependency from parent `package.json`
// FIXME [NP]: Remove the `eslint-plugin-jsdoc` dependency from parent `package.json`
// FIXME [NP]: Rebuild the parent `package.json` file and commit the reverted `package-lock.json` file
/** @file Authentication module used by Enso IDE & Cloud. */
import * as app from 'ensogl_app'

// FIXME [NP]: Remove these after Wojciech's PR is merged.
// eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-assignment
const logger = app.log.logger

export function run () {
    logger.log("FIXME [NP]: authentication/index.ts: run() is not implemented yet.")
}

export default { run }

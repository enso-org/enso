/** @file This file is used to simply run the IDE. It can be not invoked if the IDE needs to be used
 * as a library. */

// ===============
// === Run IDE ===
// ===============

// This `void` is used to explicitly not `await` a promise, not to produce an `undefined`.
// eslint-disable-next-line no-restricted-syntax
void window.enso?.main()

/** @file Flags for features that are implemented, but disabled either because
 * they are not production-ready, or because it is unknown whether they are still needed. */

export const FEATURE_FLAGS = {
    /** A selector that lets the user choose between pre-defined sets of visible columns. */
    columnDisplayModeSwitcher: false,
    /** Show toast notifications for almost every API request, rather than only on error. */
    moreToasts: false,
}

if (IS_DEV_MODE) {
    // @ts-expect-error This is exposed for development purposes only.
    window.featureFlags = FEATURE_FLAGS
}

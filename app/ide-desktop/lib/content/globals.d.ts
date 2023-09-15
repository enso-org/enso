/** @file Globals defined only in this module. */

// =====================================
// === Global namespace augmentation ===
// =====================================

declare global {
    // These are top-level constants, and therefore should be `CONSTANT_CASE`.
    /* eslint-disable @typescript-eslint/naming-convention */
    /** Whether the  */
    /** Whether the application may have the local backend running. */
    const SUPPORTS_LOCAL_BACKEND: boolean
    /** Whether the application supports deep links. This is only true when using
     * the installed app on macOS and Windows. */
    const SUPPORTS_DEEP_LINKS: boolean
    /* eslint-enable @typescript-eslint/naming-convention */
}

export {}

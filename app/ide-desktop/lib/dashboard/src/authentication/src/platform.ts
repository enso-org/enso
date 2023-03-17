/** @file The platform the application is running on (desktop or cloud). */

// ================
// === Platform ===
// ================

/** Defines the platform the application is running on.
 *
 * Depending on the platform, the application will use different routing mechanisms. For example, in
 * the cloud, the application will use the browser's URL bar to navigate between pages. In Electron,
 * the application will use the `MemoryRouter` to navigate between pages. Similarly, the
 * application will use different redirect URLs for authentication. For example, in the cloud, the
 * application will redirect using `http://` URLs. In Electron, the application will redirect using
 * `enso://` URLs. The former flow works entirely in-browser. The latter flow must go to the browser
 * and back to the application. */
export enum Platform {
    /** Application is running on a desktop (i.e., in Electron). */
    desktop = 'desktop',
    /** Application is running in the browser (i.e., in the cloud). */
    cloud = 'cloud',
}

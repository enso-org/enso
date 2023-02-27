/** @file Inter-Process communication configuration of the application. IPC allows the web-view
 * content to exchange information with the Electron application. */

// ===============
// === Channel ===
// ===============

/** Channel names used by the IPC protocol. */
export const channel = {
    error: 'error',
    loadProfiles: 'load-profiles',
    profilesLoaded: 'profiles-loaded',
    saveProfile: 'save-profile',
    quit: 'quit-ide',
    /**
     * Channel for requesting that a URL by opened by the system browser.
     * 
     * Events on this channel are emitted by our `enso-cloud-authentication` package. These events
     * are intended to send the user to the system browser to complete sensitive flows such as
     * signup and signin. These flows should not be done in the app as the user cannot be expected
     * to trust the app with their credentials.
     */
    openExternalUrl: 'open-external-url',
    /**
     * Channel for the `enso-cloud-authentication` module to **set** (not **use**!) a callback.
     * 
     * The callback is the complement of the `openExternalUrl` channel above, and is intended for
     * opening URLs in the app, rather than in the system browser. The callback is called by the
     * Electron app when a user is sent back to the app from their system browser via a deep link.
     * The URL requested in the deep link is sent to the callback. The `enso-cloud-authentication`
     * module then reads this URL and performs the necessary redirects.
     *
     * This flow is complex. The simpler answer would be to simply let the Electron app handle the
     * deep link without us catching it and sending it to the callback. But this won't work because
     * we have strict requirements about not redirecting the user to any URL. All our in-app
     * redirects are emulated via browser history, since the user never leaves our "index" page.
     * This is done for security reasons (to avoid the user getting redirected to a malicious URL)
     * and for performance reasons (reloading the page would cause a full re-render of our
     * authentication and dashboard UIs).
     */
    setOpenAuthenticationUrlCallback: 'set-open-authentication-url-callback',
    /**
     * Channel for requesting that a URL by opened by the Electron app.
     * 
     * This is the channel referred to in the `setOpenAuthenticationUrlCallback` channel above. When
     * the Electron app receives a deep link, it sends the URL to this channel. This channel will in
     * turn forward it to the callback set earlier by the `setOpenAuthenticationUrlCallback`
     * channel.
     */
    openAuthenticationUrl: 'open-authentication-url',
}

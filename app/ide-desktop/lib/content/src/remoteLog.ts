/** @file Defines the {@link RemoteLogger} class and {@link remoteLog} function for sending logs to a remote server.
 * {@link RemoteLogger} provides a convenient way to manage remote logging with access token authorization. */

import * as app from 'ensogl-runner/src/runner'
import * as authConfig from '../../dashboard/src/authentication/src/config'

const logger = app.log.logger

// =================
// === Constants ===
// =================

/** URL address where remote logs should be sent. */
const REMOTE_LOG_URL = new URL(`${authConfig.ACTIVE_CONFIG.apiUrl}/logs`)

// ====================
// === RemoteLogger ===
// ====================

// === Class ===

/** Helper class facilitating sending logs to a remote. */
export class RemoteLogger {
    /** Initialize a new instance.
     * @param accessToken - JWT token used to authenticate within the cloud. */
    constructor(public accessToken: string) {
        this.accessToken = accessToken
    }

    /** Sends a log message to a remote.
     * @param message - The log message to send.
     * @param metadata - Additional metadata to send along with the log.
     * @returns Promise which resolves when the log message has been sent. */
    async remoteLog(message: string, metadata: unknown): Promise<void> {
        await remoteLog(this.accessToken, message, metadata)
    }
}

// === Underlying logic ===

/** Sends a log message to a remote server using the provided access token.
 *
 * @param accessToken - The access token for authentication.
 * @param message - The message to be logged on the server.
 * @param metadata - Additional metadata to include in the log.
 * @throws Will throw an error if the response from the server is not okay (response status is not 200).
 * @returns Returns a promise that resolves when the log message is successfully sent. */
export async function remoteLog(
    accessToken: string,
    message: string,
    metadata: unknown
): Promise<void> {
    try {
        const headers: HeadersInit = new Headers()
        headers.set('Content-Type', 'application/json')
        headers.set('Authorization', `Bearer ${accessToken}`)
        const response = await fetch(REMOTE_LOG_URL, {
            method: 'POST',
            headers,
            body: JSON.stringify({ message, metadata }),
        })
        if (!response.ok) {
            const errorMessage = `Error while sending log to a remote: Status ${response.status}.`
            try {
                const text = await response.text()
                throw new Error(`${errorMessage} Response: ${text}.`)
            } catch (error) {
                throw new Error(`${errorMessage} Failed to read response: ${String(error)}.`)
            }
        }
    } catch (error) {
        logger.error(error)
        throw error
    }
}

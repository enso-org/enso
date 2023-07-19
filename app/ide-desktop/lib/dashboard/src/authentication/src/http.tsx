/** @file HTTP client definition that includes default HTTP headers for all sent requests.
 *
 * Used to build authenticated clients for external APIs, like our Cloud backend API. */

// ==================
// === HttpMethod ===
// ==================

/** HTTP method variants that can be used in an HTTP request. */
enum HttpMethod {
    get = 'GET',
    post = 'POST',
    put = 'PUT',
    delete = 'DELETE',
}

// ==============
// === Client ===
// ==============

/** A helper function to convert a `Blob` to a base64-encoded string. */
function blobToBase64(blob: Blob) {
    return new Promise<string>(resolve => {
        const reader = new FileReader()
        reader.onload = () => {
            resolve(
                // This cast is always safe because we read as data URL (a string).
                // eslint-disable-next-line no-restricted-syntax
                (reader.result as string).replace(/^data:application\/octet-stream;base64,/, '')
            )
        }
        reader.readAsDataURL(blob)
    })
}

/** An HTTP client that can be used to create and send HTTP requests asynchronously. */
export class Client {
    /** Create a new HTTP client with the specified headers to be sent on every request. */
    constructor(
        /** A map of default headers that are included in every HTTP request sent by this client.
         *
         * This is useful for setting headers that are required for every request, like
         * authentication tokens. */
        public defaultHeaders: Headers
    ) {}

    /** Send an HTTP GET request to the specified URL. */
    get<T = void>(url: string) {
        return this.request<T>(HttpMethod.get, url)
    }

    /** Send a JSON HTTP POST request to the specified URL. */
    post<T = void>(url: string, payload: object) {
        return this.request<T>(HttpMethod.post, url, JSON.stringify(payload), 'application/json')
    }

    /** Send a base64-encoded binary HTTP POST request to the specified URL. */
    async postBase64<T = void>(url: string, payload: Blob) {
        return await this.request<T>(
            HttpMethod.post,
            url,
            await blobToBase64(payload),
            'application/octet-stream'
        )
    }

    /** Send a JSON HTTP PUT request to the specified URL. */
    put<T = void>(url: string, payload: object) {
        return this.request<T>(HttpMethod.put, url, JSON.stringify(payload), 'application/json')
    }

    /** Send an HTTP DELETE request to the specified URL. */
    delete<T = void>(url: string) {
        return this.request<T>(HttpMethod.delete, url)
    }

    /** Execute an HTTP request to the specified URL, with the given HTTP method. */
    private request<T = void>(
        method: HttpMethod,
        url: string,
        payload?: string,
        mimetype?: string
    ) {
        const headers = new Headers(this.defaultHeaders)
        if (payload != null) {
            const contentType = mimetype ?? 'application/json'
            headers.set('Content-Type', contentType)
        }

        /** A {@link Response} with a properly typed return type for `response.json()`. */
        interface ResponseWithTypedJson<U> extends Response {
            json: () => Promise<U>
        }

        // This is an UNSAFE type assertion, however this is a HTTP client
        // and should only be used to query APIs with known response types.
        // eslint-disable-next-line no-restricted-syntax
        return fetch(url, {
            method,
            headers,
            ...(payload != null ? { body: payload } : {}),
        }) as Promise<ResponseWithTypedJson<T>>
    }
}

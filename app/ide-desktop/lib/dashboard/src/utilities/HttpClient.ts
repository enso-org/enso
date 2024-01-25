/** @file HTTP client definition that includes default HTTP headers for all sent requests. */
import isNetworkError from 'is-network-error'

// =================
// === Constants ===
// =================

export const FETCH_SUCCESS_EVENT_NAME = 'fetch-success'
export const FETCH_ERROR_EVENT_NAME = 'fetch-error'

// =============
// === Types ===
// =============

/** HTTP method variants that can be used in an HTTP request. */
enum HttpMethod {
  get = 'GET',
  post = 'POST',
  put = 'PUT',
  patch = 'PATCH',
  delete = 'DELETE',
}

// ==================
// === HttpClient ===
// ==================

/** An HTTP client that can be used to create and send HTTP requests asynchronously. */
export default class HttpClient {
  /** Create a new HTTP client with the specified headers to be sent on every request. */
  constructor(
    /** A map of default headers that are included in every HTTP request sent by this client.
     *
     * This is useful for setting headers that are required for every request, like
     * authentication tokens. */
    public defaultHeaders: HeadersInit
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
  async postBinary<T = void>(url: string, payload: Blob) {
    return await this.request<T>(HttpMethod.post, url, payload, 'application/octet-stream')
  }

  /** Send a JSON HTTP PATCH request to the specified URL. */
  patch<T = void>(url: string, payload: object) {
    return this.request<T>(HttpMethod.patch, url, JSON.stringify(payload), 'application/json')
  }

  /** Send a JSON HTTP PUT request to the specified URL. */
  put<T = void>(url: string, payload: object) {
    return this.request<T>(HttpMethod.put, url, JSON.stringify(payload), 'application/json')
  }

  /** Send an HTTP DELETE request to the specified URL. */
  delete<T = void>(url: string) {
    return this.request<T>(HttpMethod.delete, url)
  }

  /** Execute an HTTP request to the specified URL, with the given HTTP method.
   * @throws {Error} if the HTTP request fails. */
  private async request<T = void>(
    method: HttpMethod,
    url: string,
    payload?: BodyInit,
    mimetype?: string
  ) {
    const headers = new Headers(this.defaultHeaders)
    if (payload != null) {
      const contentType = mimetype ?? 'application/json'
      headers.set('Content-Type', contentType)
    }

    try {
      // This is an UNSAFE type assertion, however this is a HTTP client
      // and should only be used to query APIs with known response types.
      // eslint-disable-next-line no-restricted-syntax
      const response = (await fetch(url, {
        method,
        headers,
        ...(payload != null ? { body: payload } : {}),
      })) as ResponseWithTypedJson<T>
      document.dispatchEvent(new Event(FETCH_SUCCESS_EVENT_NAME))
      return response
    } catch (error) {
      if (isNetworkError(error)) {
        document.dispatchEvent(new Event(FETCH_ERROR_EVENT_NAME))
      }
      throw error
    }
  }
}

/** A {@link Response} with a properly typed return type for `response.json()`. */
export interface ResponseWithTypedJson<U> extends Response {
  json: () => Promise<U>
}

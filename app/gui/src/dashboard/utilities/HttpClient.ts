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

/** A {@link Response} with a properly typed return type for `response.json()`. */
export interface ResponseWithTypedJson<U> extends Response {
  readonly json: () => Promise<U>
}

/** Options for {@link HttpClient.post} method. */
export interface HttpClientPostOptions {
  readonly keepalive?: boolean
}

/** Options for {@link HttpClient.request} private method. */
export interface HttpClientRequestOptions {
  readonly method: HttpMethod
  readonly url: string
  readonly payload?: BodyInit | null
  readonly mimetype?: string
  readonly keepalive?: boolean
}

/** An HTTP client that can be used to create and send HTTP requests asynchronously. */
export default class HttpClient {
  /** Create a new HTTP client with the specified headers to be sent on every request. */
  constructor(
    /**
     * A map of default headers that are included in every HTTP request sent by this client.
     *
     * This is useful for setting headers that are required for every request, like
     * authentication tokens.
     */
    public defaultHeaders: Record<string, string> = {},
  ) {}

  /** Send an HTTP GET request to the specified URL. */
  get<T = void>(url: string) {
    return this.request<T>({ method: HttpMethod.get, url })
  }

  /** Send a JSON HTTP POST request to the specified URL. */
  post<T = void>(url: string, payload: object, options?: HttpClientPostOptions) {
    return this.request<T>({
      method: HttpMethod.post,
      url,
      payload: JSON.stringify(payload),
      mimetype: 'application/json',
      keepalive: options?.keepalive ?? false,
    })
  }

  /** Send a base64-encoded binary HTTP POST request to the specified URL. */
  async postBinary<T = void>(url: string, payload: Blob) {
    return await this.request<T>({
      method: HttpMethod.post,
      url,
      payload,
      mimetype: 'application/octet-stream',
    })
  }

  /** Send a JSON HTTP PATCH request to the specified URL. */
  patch<T = void>(url: string, payload: object) {
    return this.request<T>({
      method: HttpMethod.patch,
      url,
      payload: JSON.stringify(payload),
      mimetype: 'application/json',
    })
  }

  /** Send a JSON HTTP PUT request to the specified URL. */
  put<T = void>(url: string, payload: object) {
    return this.request<T>({
      method: HttpMethod.put,
      url,
      payload: JSON.stringify(payload),
      mimetype: 'application/json',
    })
  }

  /** Send a base64-encoded binary HTTP POST request to the specified URL. */
  async putBinary<T = void>(url: string, payload: Blob) {
    return await this.request<T>({
      method: HttpMethod.put,
      url,
      payload,
      mimetype: payload.type || 'application/octet-stream',
    })
  }

  /** Send an HTTP DELETE request to the specified URL. */
  delete<T = void>(url: string, payload?: Record<string, unknown>) {
    return this.request<T>({
      method: HttpMethod.delete,
      url,
      payload: payload ? JSON.stringify(payload) : null,
    })
  }

  /** Set the session token to be included in the Authorization header of every request. */
  setSessionToken(token: string) {
    this.defaultHeaders = {
      ...this.defaultHeaders,
      // eslint-disable-next-line @typescript-eslint/naming-convention
      Authorization: `Bearer ${token}`,
    }
  }

  /**
   * Execute an HTTP request to the specified URL, with the given HTTP method.
   * @throws {Error} if the HTTP request fails.
   */
  private async request<T = void>(options: HttpClientRequestOptions) {
    const headers = new Headers(this.defaultHeaders)
    let payload = options.payload
    if (payload != null) {
      const contentType = options.mimetype ?? 'application/json'
      headers.set('Content-Type', contentType)
    }

    // `Blob` request payloads are NOT VISIBLE in Playwright due to a Chromium bug.
    // https://github.com/microsoft/playwright/issues/6479#issuecomment-1574627457
    if (process.env.IS_IN_PLAYWRIGHT_TEST === 'true' && payload instanceof Blob) {
      payload = await payload.arrayBuffer()
    }

    try {
      // This is an UNSAFE type assertion, however this is a HTTP client
      // and should only be used to query APIs with known response types.
      // eslint-disable-next-line no-restricted-syntax
      const response = (await fetch(options.url, {
        method: options.method,
        headers,
        keepalive: options.keepalive ?? false,
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

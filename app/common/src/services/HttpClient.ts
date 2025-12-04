/** @file HTTP client definition that includes default HTTP headers for all sent requests. */
import { NetworkError, OfflineError, isNetworkError } from '../utilities/errors.js'

export const FETCH_SUCCESS_EVENT_NAME = 'fetch-success'
export const FETCH_ERROR_EVENT_NAME = 'fetch-error'
export const OFFLINE_EVENT_NAME = 'offline'

/** HTTP method variants that can be used in an HTTP request. */
type HttpMethod = 'DELETE' | 'GET' | 'PATCH' | 'POST' | 'PUT'

/** A {@link Response} with a properly typed return type for `response.json()`. */
export interface ResponseWithTypedJson<U> extends Response {
  readonly json: () => Promise<U>
}

/** Options for {@link HttpClient['post']} method. */
export interface HttpClientPostOptions {
  readonly keepalive?: boolean
  readonly abort?: AbortSignal | undefined
}

/** Options for {@link HttpClient.request} private method. */
export interface HttpClientRequestOptions<Method extends HttpMethod> {
  readonly method: Method
  readonly url: string
  readonly payload?: BodyInit | null
  readonly mimetype?: string
  readonly keepalive?: boolean
  readonly abort?: AbortSignal | undefined
}

/** An HTTP client that can be used to create and send HTTP requests asynchronously. */
export class HttpClient {
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
  get<T = void>(url: string, abort?: AbortSignal) {
    return this.request<'GET', T>({ method: 'GET', url, abort })
  }

  /** Send a JSON HTTP POST request to the specified URL. */
  post<T = void>(url: string, payload: object | null, options?: HttpClientPostOptions) {
    return this.request<'POST', T>({
      method: 'POST',
      url,
      payload: JSON.stringify(payload),
      mimetype: 'application/json',
      keepalive: options?.keepalive ?? false,
      abort: options?.abort,
    })
  }

  /** Send a multipart/form-data HTTP POST request to the specified URL. */
  async postFormData<T = void>(url: string, payload: FormData, options?: HttpClientPostOptions) {
    return this.request<'POST', T>({
      method: 'POST',
      url,
      payload,
      mimetype: 'multipart/form-data',
      keepalive: options?.keepalive ?? false,
      abort: options?.abort,
    })
  }

  /** Send a base64-encoded binary HTTP POST request to the specified URL. */
  async postBinary<T = void>(url: string, payload: Blob, options?: HttpClientPostOptions) {
    return await this.request<'POST', T>({
      method: 'POST',
      url,
      payload,
      mimetype: 'application/octet-stream',
      keepalive: options?.keepalive ?? false,
      abort: options?.abort,
    })
  }

  /** Send a JSON HTTP PATCH request to the specified URL. */
  patch<T = void>(url: string, payload: object) {
    return this.request<'PATCH', T>({
      method: 'PATCH',
      url,
      payload: JSON.stringify(payload),
      mimetype: 'application/json',
    })
  }

  /** Send a JSON HTTP PUT request to the specified URL. */
  put<T = void>(url: string, payload: object) {
    return this.request<'PUT', T>({
      method: 'PUT',
      url,
      payload: JSON.stringify(payload),
      mimetype: 'application/json',
    })
  }

  /** Send a base64-encoded binary HTTP POST request to the specified URL. */
  async putBinary<T = void>(url: string, payload: Blob) {
    return await this.request<'PUT', T>({
      method: 'PUT',
      url,
      payload,
      mimetype: payload.type || 'application/octet-stream',
    })
  }

  /** Send an HTTP DELETE request to the specified URL. */
  delete<T = void>(url: string, payload?: Record<string, unknown>) {
    return this.request<'DELETE', T>({
      method: 'DELETE',
      url,
      payload: payload ? JSON.stringify(payload) : null,
    })
  }

  /** Set the session token to be included in the Authorization header of every request. */
  setSessionToken(token: string) {
    this.defaultHeaders = {
      ...this.defaultHeaders,
      Authorization: `Bearer ${token}`,
    }
  }

  /**
   * Execute an HTTP request to the specified URL, with the given HTTP method.
   * @throws {Error} if the HTTP request fails.
   */
  private async request<Method extends HttpMethod, T = void>(
    options: HttpClientRequestOptions<Method>,
  ) {
    const headers = new Headers(this.defaultHeaders)
    const payload = options.payload
    if (payload != null) {
      const contentType = options.mimetype ?? 'application/json'
      // multipart/form-data should have no content-type set.
      // See https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest_API/Using_FormData_Objects#sending_files_using_a_formdata_object
      if (contentType !== 'multipart/form-data') {
        headers.set('Content-Type', contentType)
      }
    }

    // On node.js, `navigator` seems to be defined, but `navigator.onLine` is always `undefined`.
    if (navigator.onLine !== undefined && !navigator.onLine) {
      return Promise.reject(new OfflineError('User is offline'))
    }

    try {
      // This is an UNSAFE type assertion, however this is a HTTP client
      // and should only be used to query APIs with known response types.
      const response = (await fetch(options.url, {
        method: options.method,
        headers,
        keepalive: options.keepalive ?? false,
        ...(options.abort ? { signal: options.abort } : {}),
        ...(payload != null ? { body: payload } : {}),
      })) as ResponseWithTypedJson<T> & {
        readonly body: Method extends 'GET' | 'HEAD' ? null : NonNullable<Response['body']>
      }
      if (typeof document !== 'undefined') {
        document.dispatchEvent(new Event(FETCH_SUCCESS_EVENT_NAME))
      }
      return response
    } catch (error) {
      // Even though the condition might seem always falsy,
      // offline mode might happen during the request
      // and this case need to be handled
      if (navigator.onLine !== undefined && !navigator.onLine) {
        if (typeof document !== 'undefined') {
          document.dispatchEvent(new Event(OFFLINE_EVENT_NAME))
        }
        throw new OfflineError('User is offline', { cause: error })
      }

      if (isNetworkError(error)) {
        if (typeof document !== 'undefined') {
          document.dispatchEvent(new Event(FETCH_ERROR_EVENT_NAME))
        }
        throw new NetworkError(error.message, { cause: error })
      }
      throw error
    }
  }
}

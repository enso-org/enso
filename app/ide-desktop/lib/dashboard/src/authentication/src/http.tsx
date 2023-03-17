/** @file HTTP client definition that includes default HTTP headers for all sent requests.
 *
 * Used to build authenticated clients for external APIs, like our Cloud backend API. */

// ==================
// === HttpStatus ===
// ==================

/** HTTP status codes returned in a HTTP response. */
export enum HttpStatus {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  unauthorized = 401,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  notFound = 404,
}

// ==================
// === HttpMethod ===
// ==================

/** HTTP method variants that can be used in an HTTP request. */
enum HttpMethod {
  get = "GET",
  post = "POST",
  put = "PUT",
  delete = "DELETE",
}

// ==============
// === Client ===
// ==============

/** A helper function to convert a `Blob` to a base64-encoded string. */
function blobToBase64(blob: Blob) {
  return new Promise<string>((resolve) => {
    const reader = new FileReader();
    reader.onload = () => {
      resolve(
        (reader.result as string).replace(
          /^data:application\/octet-stream;base64,/,
          ""
        )
      );
    };
    reader.readAsDataURL(blob);
  });
}

/** An HTTP client that can be used to create and send HTTP requests asynchronously. */
export class Client {
  constructor(
    /** A map of default headers that are included in every HTTP request sent by this client.
     *
     * This is useful for setting headers that are required for every request, like authentication
     * tokens. */
    public defaultHeaders?: Headers
  ) {}

  /** Sends an HTTP GET request to the specified URL. */
  get = (url: string) => this.request(HttpMethod.get, url);

  /** Sends a JSON HTTP POST request to the specified URL. */
  post = (url: string, payload: object) =>
    this.request(
      HttpMethod.post,
      url,
      JSON.stringify(payload),
      "application/json"
    );

  /** Sends a base64-encoded binary HTTP POST request to the specified URL. */
  postBase64 = async (url: string, payload: Blob) =>
    await this.request(
      HttpMethod.post,
      url,
      await blobToBase64(payload),
      "application/octet-stream"
    );

  /** Sends a JSON HTTP PUT request to the specified URL. */
  put = (url: string, payload: object) =>
    this.request(
      HttpMethod.put,
      url,
      JSON.stringify(payload),
      "application/json"
    );

  /** Sends an HTTP DELETE request to the specified URL. */
  delete = (url: string) => this.request(HttpMethod.delete, url);

  /** Executes an HTTP request to the specified URL, with the given HTTP method. */
  private readonly request = (
    method: HttpMethod,
    url: string,
    payload?: string,
    mimetype?: string
  ) => {
    const defaultHeaders = this.defaultHeaders ?? [];
    const headers = new Headers(defaultHeaders);
    if (payload) {
      const contentType = mimetype ?? "application/json";
      headers.set("Content-Type", contentType);
    }
    return fetch(url, {
      method,
      headers,
      ...(payload ? { body: payload } : {}),
    });
  };
}

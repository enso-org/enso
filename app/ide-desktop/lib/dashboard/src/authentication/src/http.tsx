/** @file HTTP client definition that includes default HTTP headers for all sent requests.
 * 
 * Used to build authenticated clients for external APIs, like our Cloud backend API. */



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



// ============
// === Json ===
// ============

/** A JavaScript object that can be serialized to a JSON value for an HTTP body. */
type Json = Record<string, any>;



// ====================
// === HttpResponse ===
// ====================

/** A wrapper around the built-in JavaScript HTTP {@link Response} type.
 *
 * This wrapper provides convenience methods like {@link HttpResponse.model} to deserialize the
 * response body to a JSON object. */
export class HttpResponse {
  /** The wrapped HTTP {@link Response}. */
  private response: Response;

  /** Creates a new {@link HttpResponse} by wrapping the given {@link Response}. */
  constructor(response: Response) {
    this.response = response;
  }

  /** Returns the status code of the HTTP response. */
  status = () => this.response.status;

  /** @property {Function} model - Deserialize the HTTP response body as JSON, to the specified
   * type. */
  model = <T extends object>(): Promise<T> =>
    this.response.json().then((json) => json as T);
}



// ======================
// === RequestBuilder ===
// ======================

/** Builder for a new HTTP {@link RequestInitWithUrlAndHeaders}.
 *
 * Provides convenience methods for setting the request body, and sending the request using the HTTP
 * {@link Client} provided when the builder was created. */
class RequestBuilder {
  /** HTTP client used to send the HTTP request, once it is created. */
  private client: Client;
  /** HTTP request being created. */
  private request: RequestInitWithUrlAndHeaders;

  /** Creates a new {@link RequestBuilder}. */
  constructor(client: Client, url: string, config: RequestInit) {
    const headers = new Headers();
    this.client = client;
    this.request = { ...config, url, headers };
  }

  /** Sets the provided JSON value as the body of the HTTP request, and adjusts the HTTP headers
   * accordingly. */
  json = (body: Json) => {
    this.request.body = JSON.stringify(body);
    this.request.headers.set("Content-Type", "application/json");
    return this;
  };

  /** Finalizes the HTTP request and sends it using the HTTP client. */
  send = () => this.client.executeRequest(this.request);
}

// === RequestInitWithUrlAndHeaders ===

/** An HTTP request that is guaranteed to also have the URL it will be send to and an initialized map
 * of {@link Headers}.
 *
 * This is an extension of the built-in {@link RequestInit} interface, which does not provide the
 * aforementioned guarantees. In fact, it does not provide a `url` at all, and the `headers` field
 * may even be `undefined` for that interface. */
interface RequestInitWithUrlAndHeaders extends RequestInit {
  url: string;
  headers: Headers;
}



// ==============
// === Client ===
// ==============

/** An HTTP client that can be used to create and send HTTP requests asynchronously. */
export class Client {
  /** A map of default headers that are included in every HTTP request sent by this client.
   *
   * This is useful for setting headers that are required for every request, like authentication
   * tokens. */
  defaultHeaders?: Headers;

  /** Returns a new {@link ClientBuilder} that can be used to create an HTTP {@link Client}. */
  static builder = () => new ClientBuilder();

  /** Returns a builder for an HTTP GET request to the specified URL. */
  get = (url: string) => this.request(HttpMethod.get, url);

  /** Returns a builder for an HTTP POST request to the specified URL. */
  post = (url: string) => this.request(HttpMethod.post, url);

  /** Returns a builder for an HTTP request to the specified URL, with the given HTTP method. */
  request = (method: HttpMethod, url: string) => {
    const config = { method };
    return new RequestBuilder(this, url, config);
  };

  /** Asynchronously sends an HTTP request, returning a wrapped {@link HttpResponse}, which can be
   * used to access the status code and deserialize the response body. */
  executeRequest = async (config: RequestInitWithUrlAndHeaders) => {
    // Insert default headers into the request headers without overwriting already appended
    // headers.
    if (this.defaultHeaders != null) {
      this.defaultHeaders.forEach((value, key) => {
        if (!config.headers.has(key)) {
          config.headers.set(key, value);
        }
      });
    }

    // Construct the request from the options.
    const request = new Request(config.url, config);

    const response = await fetch(request);

    return new HttpResponse(response);
  };
}



// =====================
// === ClientBuilder ===
// =====================

/** Builder for a new HTTP {@link Client}. */
class ClientBuilder {
  /** Default headers that are included in every HTTP request sent by the client. */
  private _defaultHeaders: Headers = new Headers();

  /** Sets the default headers that will be included in every HTTP request sent by the client. */
  defaultHeaders = (headers: Headers) => {
    this._defaultHeaders = headers;
    return this;
  };

  /** Finalizes the builder and returns a fully-initialized HTTP {@link Client}. */
  build = () => {
    const client = new Client();
    client.defaultHeaders = this._defaultHeaders;
    return client;
  };
}

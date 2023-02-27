// FIXME [NP]: document all of the below


// ==================
// === HttpMethod ===
// ==================

enum HttpMethod {
    get = "GET",
    post = "POST",
    put = "PUT",
    delete = "DELETE",
}



// ============
// === Json ===
// ============

type Json = Record<string, any>;



// ====================
// === HttpResponse ===
// ====================

export class HttpResponse {
    private response: Response;

    constructor(response: Response) {
        this.response = response;
    }

    status = () => this.response.status

    model = <T extends object>(): Promise<T> => this.response.json().then((json) => json as T)
}



// ======================
// === RequestBuilder ===
// ======================

class RequestBuilder {
    private client: Client;
    private request: RequestInitWithUrlAndHeaders;

    constructor(client: Client, url: string, config: RequestInit) {
        const headers = new Headers();
        this.client = client;
        this.request = { ...config, url, headers };
    }

    json = (body: Json) => {
        this.request.body = JSON.stringify(body);
        this.request.headers.set("Content-Type", "application/json");
        return this;
    }

    send = () => this.client.executeRequest(this.request)
}


// === RequestInitWithUrlAndHeaders ===

interface RequestInitWithUrlAndHeaders extends RequestInit {
    url: string;
    headers: Headers;
}



// ==============
// === Client ===
// ==============

export class Client {
    defaultHeaders?: Headers;

    static builder = () => new ClientBuilder()

    get = (url: string) => this.request(HttpMethod.get, url)

    post = (url: string) => this.request(HttpMethod.post, url)

    request = (method: HttpMethod, url: string) => {
        const config = { method };
        const builder = new RequestBuilder(this, url, config);
        return builder;
    }

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

        const response = await fetch(request)

        return new HttpResponse(response);
    }
}



// =====================
// === ClientBuilder ===
// =====================

class ClientBuilder {
    private _defaultHeaders: Headers = new Headers();

    defaultHeaders = (headers: Headers) => {
        this._defaultHeaders = headers;
        return this;
    }

    build = () => {
        const client = new Client();
        client.defaultHeaders = this._defaultHeaders;
        return client;
    }
}

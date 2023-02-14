/**
 * @file Module containing the API client for the Cloud backend API.
 * 
 * Each exported function in this module corresponds to an API endpoint. The functions are
 * asynchronous and return a `Promise` that resolves to the response from the API.
 */
import { API_URL } from "./config";



// =============
// === Types ===
// =============

interface Request {
    accessToken: string;
    path: string;
    method: HttpMethod;
    body?: Record<string, string>;
}

enum HttpMethod {
    get = "GET",
    post = "POST",
    put = "PUT",
    delete = "DELETE",
}

export type Organization = {
    id: string;
    userEmail: string;
    name: string;
}



// ================
// === FetchApi ===
// ================

/// Utility function for performing a REST request to an API endpoint.
///
/// Parses the response as JSON (if possible) and returns the result.
/// Otherwise returns `null` on a successful API call.
const fetchApi = async (
    {
        accessToken,
        path,
        method,
        body,
    }: Request,
): Promise<Response> => {
    if (!accessToken) { throw new Error("No access token provided"); }
    
    const config: RequestInit = { method };
    // eslint-disable-next-line @typescript-eslint/naming-convention
    const headers = new Headers(config.headers);
    headers.set("Authorization", `Bearer ${accessToken}`);
    if (body != null) {
        config.body = JSON.stringify(body);
        headers.set("Content-Type", "application/json");
    }

    config.headers = headers;
    const response = await fetch(`${API_URL}/${path}`, config);

    return response;
}



// ===================
// === SetUsername ===
// ===================

export type SetUsernameBody = {
    userName: string;
    userEmail: string;
};

/// Wrapper for our `setUsername` Cloud backend API endpoint.
export const setUsername = async (accessToken: string, body: SetUsernameBody) => {
    const path = "users";
    const method = HttpMethod.post;
    const request = { accessToken, path, method, body };
    const response = await fetchApi(request);
    const text = await response.text();
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const json: Organization = JSON.parse(text);
    return json;
}



// ==================
// === GetUsersMe ===
// ==================

/// Wrapper for our `getUsersMe` Cloud backend API endpoint.
///
/// Returns organization info for the user associated with the given access token.
export const getUsersMe = async (accessToken: string): Promise<Organization | null> => {
    const path = "users/me";
    const method = HttpMethod.get;
    const request = { accessToken, path, method };
    const response = await fetchApi(request);
    // FIXME [NP]: handle 401 & 404 differently?
    if (response.status == 401 || response.status == 404) {
        return null;
    }
    // If the response is not 401, then we expect it to be 200 and contain the organization info.
    const text = await response.text();
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const json: Organization = JSON.parse(text);
    return json;
}

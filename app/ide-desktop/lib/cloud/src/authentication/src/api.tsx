import { API_URL } from "./config";



// =============
// === Types ===
// =============

enum HttpMethod {
    get = "GET",
    post = "POST",
    put = "PUT",
    delete = "DELETE",
}

type Organization = {
    id: string;
    userEmail: string;
    name: string;
}



// ===========
// === Api ===
// ===========

// === fetchApi ===

async function fetchApi<T = unknown>(
    accessToken: string,
    url: string,
    method: string,
    body?: Record<string, string>,
): Promise<T | null> {
    if (!accessToken) { throw new Error("No access token provided"); }
    
    const config: RequestInit = { method };
    // eslint-disable-next-line @typescript-eslint/naming-convention
    config.headers = { Authorization: `Bearer ${accessToken}` };
    if (body != null) {
        config.body = JSON.stringify(body);
        config.headers["Content-Type"] = "application/json";
    }

    const response = await fetch(`${API_URL}/${url}`, config);

    const text = await response.text();
    if (text) {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        const json: T = JSON.parse(text);
        return json;
    }
    return null;
}


// === setUsernameType ===

type SetUsernameType = {
    userName: string;
    userEmail: string;
};

// === setUsername ===

const setUsername = async (accessToken: string, body: SetUsernameType) => {
    await fetchApi<Organization>(accessToken, "users", HttpMethod.post, body);
};

export { setUsername }

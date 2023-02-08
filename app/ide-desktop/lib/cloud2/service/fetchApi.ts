import { API_URL } from "../config/api";

export async function fetchApi<T = unknown>(
  token: string | undefined,
  url: string,
  method: string,
  body?: Record<string, unknown>
): Promise<T | null> {
  if (!token) return;

  const requestConfig: RequestInit = { method };
  requestConfig.headers = { Authorization: `Bearer ${token}` };
  if (body != null) {
    requestConfig.headers["Content-Type"] = "application/json";
    requestConfig.body = JSON.stringify(body);
  }

  const response = await fetch(`${API_URL}/${url}`, requestConfig);

  if (response == null) {
    return null;
  }

  const resultText = await response.text();
  if (resultText) return JSON.parse(resultText);
  return;
}

import { fetchApi } from "./fetchApi";
import { HttpMethod, MyOrganization } from "./types";

// =========================
// === getMyOrganization ===
// =========================

// Function for getting the organization info by calling the my-organization api
// endpoint.
export async function getMyOrganization(
  token: string
): Promise<MyOrganization | null> {
  const result = await fetchApi<MyOrganization>(
    token,
    "users/me",
    HttpMethod.Get
  );

  if (!result) {
    return;
  }

  return result;
}

export async function setUsername(token: string, body: any) {
  return await fetchApi<MyOrganization>(token, "users", HttpMethod.Post, body);
}

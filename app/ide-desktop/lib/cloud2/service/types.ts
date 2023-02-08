export enum HttpMethod {
  Get = "GET",
  Post = "POST",
  Put = "PUT",
  Del = "DELETE",
}

export interface AuthContextType {
  // The access token of the current user. `undefined` if the user is not signed in.
  accessToken?: string;
  // The organization ID of the current user. `undefined` if the user is not signed in or has not
  // set a username yet.
  organizationId?: string;
  // A function that when invoked will sign the user out and redirect them back to the home page.
  signOut: () => void;
  // The email of the current user.
  userEmail?: string;
}

export type Session = {
  userEmail: string;
  accessToken: string;
};

export type MyOrganization = {
  id: string;
  userEmail: string;
  name: string;
};

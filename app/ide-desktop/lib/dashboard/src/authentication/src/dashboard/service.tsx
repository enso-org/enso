/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link Backend} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import * as http from "../http";
import * as config from "../config";
import * as loggerProvider from "../providers/logger";

// =================
// === Constants ===
// =================

/** Relative HTTP path to the "get user" endpoint of the Cloud backend API. */
const GET_USER_PATH = "users/me";

// =============
// === Types ===
// =============

/** Unique identifier for a user's project. */
export type ProjectId = string;

/** A user/organization in the application. These are the primary owners of a project. */
export interface Organization {
  id: string;
  userEmail: string;
  name: string;
}

/** Type of application that a {@link Version} applies to.
 *
 * We keep track of both backend and IDE versions, so that we can update the two independently.
 * However the format of the version numbers is the same for both, so we can use the same type for
 * both. We just need this enum to disambiguate. */
export enum VersionType {
  backend = "Backend",
  ide = "Ide",
}

/** A version describing a release of the backend or IDE. */
export interface Version {
  versionType: VersionType;
  ami: string | undefined;
  created: string;
  /** This field name does not follow the naming convention. This field name is snake case on the
   * backend. The case must match here for JSON deserialization to work. */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  version_number: string;
}

/** Possible states that a project can be in. */
export enum ProjectState {
  created = "Created",
  new = "New",
  openInProgress = "OpenInProgress",
  opened = "Opened",
  closed = "Closed",
}

/** Wrapper around a project state value. */
export interface ProjectStateType {
  type: ProjectState;
}

/** A user/organization's project containing and/or currently executing code. */
export interface Project {
  organizationId: string;
  projectId: ProjectId;
  name: string;
  state: ProjectStateType;
  packageName: string;
  address: string | null;
  ami: string | null;
  ideVersion: Version | null;
  engineVersion: Version | null;
}

// =================
// === Endpoints ===
// =================

/** HTTP request body for the "create project" endpoint. */
export type CreateProjectRequestBody = {
  projectName: string;
  projectTemplateName: string | undefined;
};

/** HTTP request body for the "open project" endpoint. */
export type OpenProjectRequestBody = {
  forceCreate: boolean;
};

// ===============
// === Backend ===
// ===============

/** Class for sending requests to the Cloud backend API endpoints. */
export class Backend {
  private client: http.Client;
  private logger: loggerProvider.Logger;

  /** Creates a new instance of the {@link Backend} API client.
   *
   * @throws An error if the `Authorization` header is not set on the given `client`. */
  constructor(client: http.Client, logger: loggerProvider.Logger) {
    this.client = client;
    this.logger = logger;
    /** All of our API endpoints are authenticated, so we expect the `Authorization` header to be
     * set. */
    if (!this.client.defaultHeaders?.has("Authorization")) {
      throw new Error("Authorization header not set.");
    }
  }

  /** Returns a {@link RequestBuilder} for an HTTP GET request to the given path. */
  get = (path: string) =>
    this.client.get(`${config.ACTIVE_CONFIG.apiUrl}/${path}`);

  /** Returns a {@link RequestBuilder} for an HTTP POST request to the given path. */
  post = (path: string, payload: object) =>
    this.client.post(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload);

  /** Logs the error that occurred and throws a new one with a more user-friendly message. */
  errorHandler = (message: string) => (error: Error) => {
    this.logger.error(error.message);
    throw new Error(message);
  };

  /** Returns organization info for the current user, from the Cloud backend API.
   *
   * @returns `null` if status code 401 or 404 was received. */
  getUser = (): Promise<Organization | null> =>
    this.get(GET_USER_PATH).then((response) => {
      if (
        response.status === http.HttpStatus.unauthorized ||
        response.status === http.HttpStatus.notFound
      ) {
        return null;
      }
      return response.json() as Promise<Organization>;
    });
}

// =====================
// === createBackend ===
// =====================

/** Shorthand method for creating a new instance of the backend API, along with the necessary
 * headers. */
/* TODO [NP]: https://github.com/enso-org/cloud-v2/issues/343
 * This is a hack to quickly create the backend in the format we want, until we get the provider
 * working. This should be removed entirely in favour of creating the backend once and using it from
 * the context. */
export const createBackend = (
  accessToken: string,
  logger: loggerProvider.Logger
): Backend => {
  const headers = new Headers();
  headers.append("Authorization", `Bearer ${accessToken}`);
  const client = new http.Client(headers);
  return new Backend(client, logger);
};

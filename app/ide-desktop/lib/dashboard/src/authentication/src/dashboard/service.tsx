/** @file Module containing the API client for the Cloud backend API.
 * 
 * Each exported function in the {@link Backend} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import { Client } from '../http';
import { API_URL } from "../config";
import { Logger } from '../providers/logger';



// =================
// === Constants ===
// =================

/** Default HTTP body for an "open project" request. */
const DEFAULT_OPEN_PROJECT_BODY: OpenProjectRequestBody = { forceCreate: false };

/** Relative HTTP path to the "set username" endpoint of the Cloud backend API. */
const SET_USER_NAME_PATH = "users";
/** Relative HTTP path to the "get user" endpoint of the Cloud backend API. */
const GET_USER_PATH = "users/me";
/** Relative HTTP path to the "list projects" endpoint of the Cloud backend API. */
const LIST_PROJECTS_PATH = "projects";
/** Relative HTTP path to the "create project" endpoint of the Cloud backend API. */
const CREATE_PROJECT_PATH = "projects";
/** Relative HTTP path to the "close project" endpoint of the Cloud backend API. */
const closeProjectPath = (projectId: ProjectId) => `projects/${projectId}/close`;
/** Relative HTTP path to the "get project" endpoint of the Cloud backend API. */
const getProjectPath = (projectId: ProjectId) => `projects/${projectId}`;
/** Relative HTTP path to the "open project" endpoint of the Cloud backend API. */
const openProjectPath = (projectId: ProjectId) => `projects/${projectId}/open`;



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
    // This does not follow our naming convention because it's defined this way in the backend, so
    // we need to match it.
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

/** HTTP request body for the "set username" endpoint. */
export interface SetUsernameRequestBody {
    userName: string;
    userEmail: string;
}

/** HTTP response body for the "list projects" endpoint. */
interface ListProjectsResponseBody {
    projects: Project[];
}

/** HTTP request body for the "create project" endpoint. */
export interface CreateProjectRequestBody {
    projectName: string;
    projectTemplateName: string | undefined;
}

/** HTTP request body for the "open project" endpoint. */
export interface OpenProjectRequestBody {
    forceCreate: boolean;
}



// ===============
// === Backend ===
// ===============

/** Class for sending requests to the Cloud backend API endpoints. */
export class Backend {
    private client: Client;
    private logger: Logger;

    /** Creates a new instance of the {@link Backend} API client.
     * 
     * @throws An error if the `Authorization` header is not set on the given `client`. */
    constructor(client: Client, logger: Logger) {
        this.client = client;        
        this.logger = logger;

        // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
        // set.
        if (!this.client.defaultHeaders?.has("Authorization")) {
            throw new Error("Authorization header not set.");
        }
    }

    /** Returns a {@link RequestBuilder} for an HTTP GET request to the given path. */
    get = (path: string) => this.client.get(`${API_URL}/${path}`)

    /** Returns a {@link RequestBuilder} for an HTTP POST request to the given path. */
    post = (path: string) => this.client.post(`${API_URL}/${path}`)

    /** Logs the error that occurred and throws a new one with a more user-friendly message. */
    errorHandler = (message: string) => (error: Error) => {
        this.logger.error(error.message);
        throw new Error(message);
    }

    /** Sets the username of the current user, on the Cloud backend API. */
    setUsername = (body: SetUsernameRequestBody): Promise<Organization> => this
        .post(SET_USER_NAME_PATH)
        .json(body)
        .send()
        .then((response) => response.model())
   
    /** Returns organization info for the current user, from the Cloud backend API.
     * 
     * @returns `null` if status code 401 or 404 was received. */
    getUser = (): Promise<Organization | null> => this
        .get(GET_USER_PATH)
        .send()
        .then((response) => {
            if (response.status() == 401 || response.status() == 404) {
                return null;
            }

            return response.model<Organization>()
        })

    /** Returns a list of projects belonging to the current user, from the Cloud backend API. */
    listProjects = (): Promise<Project[]> => this
        .get(LIST_PROJECTS_PATH)
        .send()
        .then(async (response) => {
            if (response.status() == 401 || response.status() == 404) {
                return [];
            }

            const model = await response.model<ListProjectsResponseBody>()
            return model.projects;
        })

    /** Creates a project for the current user, on the Cloud backend API.
     * 
     * @throws An error if a 401 or 404 status code was received. */
    createProject = async (body: CreateProjectRequestBody): Promise<Project> => {
        const request = this.post(CREATE_PROJECT_PATH).json(body);
        const response = await request.send()

        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to create project.");
        }

        return response.model<Project>()
    }

    /** Closes the project identified by the given project ID, on the Cloud backend API.
     * 
     * @throws An error if a 401 or 404 status code was received. */
    closeProject = async (projectId: ProjectId): Promise<void> => {
        const path = closeProjectPath(projectId);
        const request = this.post(path);
        const response = await request.send()

        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to close project.");
        }
    }

    /** Returns project details for the specified project ID, from the Cloud backend API.
     * 
     * @throws An error if a 401 or 404 status code was received. */
    getProject = async (projectId: ProjectId): Promise<Project> => {
        const path = getProjectPath(projectId);
        const request = this.get(path);
        const response = await request.send()

        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to get project details.");
        }

        return response.model<Project>()
    }

    /** Sets project to an open state, on the Cloud backend API.
     * 
     * @throws An error if a 401 or 404 status code was received. */
    openProject = async (projectId: ProjectId, body: OpenProjectRequestBody = DEFAULT_OPEN_PROJECT_BODY): Promise<void> => {
        const path = openProjectPath(projectId);
        const request = this.get(path).json(body);
        const response = await request.send()

        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to open project.");
        }
    }
}



// =====================
// === createBackend ===
// =====================

/** Shorthand method for creating a new instance of the backend API, along with the necessary
 * headers. */
// FIXME [NP2]: this is a hack to quickly create the backend in the format we want, until we get the
// provider working. This should be removed entirely in favour of creating the backend once and
// using it from the context.
export const createBackend = (accessToken: string, logger: Logger): Backend => {
    const headers = new Headers();
    headers.append("Authorization", `Bearer ${accessToken}`)
    const client = Client.builder().defaultHeaders(headers).build();
    return new Backend(client, logger);
}

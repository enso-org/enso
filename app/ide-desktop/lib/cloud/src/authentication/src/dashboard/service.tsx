/**
 * @file Module containing the API client for the Cloud backend API.
 * 
 * Each exported function in this module corresponds to an API endpoint. The functions are
 * asynchronous and return a `Promise` that resolves to the response from the API.
 */
// FIXME [NP]: document all of the below
import { Client } from '../http';

import { API_URL } from "../config";
import { Logger } from '../providers/logger';



// =================
// === Constants ===
// =================

const DEFAULT_OPEN_PROJECT_BODY: OpenProjectBody = { forceCreate: false };

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


export type ProjectId = string;

export interface Organization {
    id: string;
    userEmail: string;
    name: string;
}

export enum VersionType {
    backend = "Backend",
    ide = "Ide",
}

export interface Version {
    versionType: VersionType;
    ami: string | undefined;
    created: string;
    // This does not follow our naming convention because it's defined this way in the backend, so
    // we need to match it.
    // eslint-disable-next-line @typescript-eslint/naming-convention
    version_number: string;
}

export enum ProjectState {
    created = "Created",
    new = "New",
    openInProgress = "OpenInProgress",
    opened = "Opened",
    closed = "Closed",
}

export type ProjectStateType = {
    type: ProjectState;
}

export type Project = {
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


// === SetUsername ===

// FIXME [NP]: rename this to `SetUserNameRequestBody`?
export interface SetUsernameBody {
    userName: string;
    userEmail: string;
}


// === ListProjectsResponse ===

interface ListProjectsResponse {
    projects: Project[];
}


// === CreateProjectBody ===

export interface CreateProjectBody {
    projectName: string;
    projectTemplateName: string | undefined;
}


// === OpenProjectBody ===

export interface OpenProjectBody {
    forceCreate: boolean;
}



// ===============
// === Backend ===
// ===============

export class Backend {
    private client: Client;
    private logger: Logger;

    constructor(client: Client, logger: Logger) {
        this.client = client;        
        this.logger = logger;

        // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
        // set.
        if (!this.client.defaultHeaders?.has("Authorization")) {
            throw new Error("Authorization header not set.");
        }
    }

    get = (path: string) => this.client.get(`${API_URL}/${path}`)

    post = (path: string) => this.client.post(`${API_URL}/${path}`)

    /** Logs the error that occurred and throws a new one with a more user-friendly message. */
    errorHandler = (message: string) => (error: Error) => {
        this.logger.error(error.message);
        throw new Error(message);
    }

    /** Sets the username of the current user, on the Cloud backend API. */
    setUsername = (body: SetUsernameBody): Promise<Organization> => this
        .post(SET_USER_NAME_PATH)
        .json(body)
        .send()
        .then((response) => response.model())
   
    /** Returns organization info for the current user, from the Cloud backend API. */
    getUser = (): Promise<Organization | null> => this
        .get(GET_USER_PATH)
        .send()
        .then((response) => {
            // FIXME [NP]: make this error type-safe with Result.
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
            // FIXME [NP]: make this error type-safe with Result.
            // FIXME [NP]: should we really be treating these as special cases?
            if (response.status() == 401 || response.status() == 404) {
                return [];
            }

            const model = await response.model<ListProjectsResponse>()
            const projects = model.projects;
            return projects;
        })

    /** Creates a project for the current user, on the Cloud backend API. */
    createProject = async (body: CreateProjectBody): Promise<Project> => {
        const request = this.post(CREATE_PROJECT_PATH).json(body);
        const response = await request.send()

        // FIXME [NP]: make this error type-safe with Result.
        // FIXME [NP]: should we really be treating these as special cases?
        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to create project.");
        }

        const model = response.model<Project>()

        return model;
    }

    /** Closes the project identified by the given project ID, on the Cloud backend API. */
    closeProject = async (projectId: ProjectId): Promise<void> => {
        const path = closeProjectPath(projectId);
        const request = this.post(path);
        const response = await request.send()

        // FIXME [NP]: make this error type-safe with Result.
        // FIXME [NP]: should we really be treating these as special cases?
        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to close project.");
        }
    }

    /** Returns project details for the specified project ID, from the Cloud backend API. */
    getProject = async (projectId: ProjectId): Promise<Project> => {
        const path = getProjectPath(projectId);
        const request = this.get(path);
        const response = await request.send()

        // FIXME [NP]: make this error type-safe with Result.
        // FIXME [NP]: should we really be treating these as special cases?
        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to get project details.");
        }

        const model = response.model<Project>()

        return model;
    }

    /** Sets project to an open state, on the Cloud backend API. */
    openProject = async (projectId: ProjectId, body: OpenProjectBody = DEFAULT_OPEN_PROJECT_BODY): Promise<void> => {
        const path = openProjectPath(projectId);
        const request = this.get(path).json(body);
        const response = await request.send()

        // FIXME [NP]: make this error type-safe with Result.
        // FIXME [NP]: should we really be treating these as special cases?
        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to open project.");
        }
    }
}



// =====================
// === createBackend ===
// =====================

// FIXME [NP]: this is a hack to quickly create the backend in the format we want, until we get the
// provider working. This should be removed entirely in favour of creating the backend once and using it from the context.
export const createBackend = (accessToken: string, logger: Logger): Backend => {
    const headers = new Headers();
    headers.append("Authorization", `Bearer ${accessToken}`)
    const client = Client.builder().defaultHeaders(headers).build();
    const backend = new Backend(client, logger);
    return backend
}

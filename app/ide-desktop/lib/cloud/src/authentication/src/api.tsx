/**
 * @file Module containing the API client for the Cloud backend API.
 * 
 * Each exported function in this module corresponds to an API endpoint. The functions are
 * asynchronous and return a `Promise` that resolves to the response from the API.
 */
// FIXME [NP]: document all of the below
import { createContext, ReactNode, useContext } from 'react';
import { Client, HttpResponse } from './client';

import { Logger } from "./components/app";
import { API_URL } from "./config";



// =================
// === Constants ===
// =================

const DEFAULT_OPEN_PROJECT_BODY: OpenProjectBody = { forceCreate: false };



// =============
// === Types ===
// =============


export type ProjectId = string;

export type Organization = {
    id: string;
    userEmail: string;
    name: string;
}

export enum VersionType {
    Backend = "Backend",
    Ide = "Ide",
}

export type Version = {
    versionType: VersionType;
    ami: string | undefined;
    created: string;
    version_number: string;
};

export enum ProjectState {
    Created = "Created",
    New = "New",
    OpenInProgress = "OpenInProgress",
    Opened = "Opened",
    Closed = "Closed",
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

export interface SetUsernameBody {
    userName: string;
    userEmail: string;
}


// === ListProjectsResponse ===

type ListProjectsResponse = {
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

    errorHandler = (message: string) => (error: Error) => {
        this.logger.error(error);
        this.logger.error(error.message);
        this.logger.error(JSON.stringify(error));
        this.logger.error("Error: ", error);
        throw new Error(message);
    }

    /** Sets the username of the current user, on the Cloud backend API. */
    setUsername = (body: SetUsernameBody) => this
        .post(`users`)
        .json(body)
        .send()
        .then(assertStatus(200))
        .then((response) => response.model<Organization>())
        .catch(this.errorHandler("Unable to set username."))
   
    /** Returns organization info for the current user, from the Cloud backend API. */
    getUsersMe = () => this
        .get(`users/me`)
        .send()
        .then(assertStatus(200))
        .then((response) => response.model<Organization>())
        .catch(this.errorHandler("Unable to get user details."))

    /** Returns a list of projects belonging to the current user, from the Cloud backend API. */
    listProjects = () => this
        .get(`projects`)
        .send()
        .then(assertStatus(200))
        .then((response) => response.model<ListProjectsResponse>())
        .then((response) => response.projects)
        .catch(this.errorHandler("Unable to list projects."))

    /** Creates a project for the current user, on the Cloud backend API. */
    createProject = (body: CreateProjectBody) => this
        .post(`projects`)
        .json(body)
        .send()
        .then(assertStatus(200))
        .then((response) => response.model<Project>())
        .catch(this.errorHandler("Unable to create project."))

    /** Closes the project identified by the given project ID, on the Cloud backend API. */
    closeProject = (projectId: ProjectId) => this
        .post(`projects/${projectId}/close`)
        .send()
        .then(assertStatus(200))
        .then(() => {})
        .catch(this.errorHandler("Unable to close project."))

    /** Returns project details for the specified project ID, from the Cloud backend API */
    getProject = (projectId: ProjectId) => this
        .get(`projects/${projectId}`)
        .send()
        .then(assertStatus(200))
        .then((response) => response.model<Project>())
        .catch(this.errorHandler("Unable to get project."))

    /** Sets project to an open state, on the Cloud backend API. */
    openProject = (projectId: ProjectId, body: OpenProjectBody = DEFAULT_OPEN_PROJECT_BODY) => this
        .post(`projects/${projectId}/open`)
        .json(body)
        .send()
        .then(assertStatus(200))
        .then(() => {})
        .catch(this.errorHandler("Unable to open project."))
}

const assertStatus = (expected: number) => (response: HttpResponse): HttpResponse => {
    const status = response.status();
    if (status !== expected) {
        throw new Error(`Expected status ${expected}, got ${status}`);
    }
    return response;
}



// ======================
// === BackendContext ===
// ======================

type BackendContextType = Backend;

// eslint-disable-next-line @typescript-eslint/naming-convention
const BackendContext = createContext<BackendContextType>({} as BackendContextType)



// =======================
// === BackendProvider ===
// =======================

export interface BackendProviderProps {
    accessToken: string;
    logger: Logger;
    children: ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const BackendProvider = (props: BackendProviderProps) => {
    const { accessToken, logger, children } = props;

    // Create an HTTP client with the access token as a default header. This way, any request made
    // using this HTTP client will be authorized.
    const headers = new Headers();
    headers.append("Authorization", `Bearer ${accessToken}`);
    const client = Client.builder().defaultHeaders(headers).build();

    // Create a Cloud backend API client from the HTTP client.
    const backend = new Backend(client, logger);

    return (
        <BackendContext.Provider value={backend}>
            {children}
        </BackendContext.Provider>
    );
}



// ==================
// === useBackend ===
// ==================

export const useBackend = () => useContext(BackendContext);

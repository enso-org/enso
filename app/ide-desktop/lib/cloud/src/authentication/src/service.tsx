/**
 * @file Module containing the API client for the Cloud backend API.
 * 
 * Each exported function in this module corresponds to an API endpoint. The functions are
 * asynchronous and return a `Promise` that resolves to the response from the API.
 */
// FIXME [NP]: document all of the below
import { Client } from './http';

import { API_URL } from "./config";
import { Logger } from './logger';



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

// FIXME [NP2]: Rename all enums to camelCase and remove lint silences
export enum VersionType {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Backend = "Backend",
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Ide = "Ide",
}

export type Version = {
    versionType: VersionType;
    ami: string | undefined;
    created: string;
    // This does not follow our naming convention because it's defined this way in the backend, so
    // we need to match it.
    // eslint-disable-next-line @typescript-eslint/naming-convention
    version_number: string;
};

// FIXME [NP2]: Rename all enums to camelCase and remove lint silences
export enum ProjectState {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Created = "Created",
    // eslint-disable-next-line @typescript-eslint/naming-convention
    New = "New",
    // eslint-disable-next-line @typescript-eslint/naming-convention
    OpenInProgress = "OpenInProgress",
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Opened = "Opened",
    // eslint-disable-next-line @typescript-eslint/naming-convention
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
        .post(`users`)
        .json(body)
        .send()
        .then((response) => response.model())
   
    /** Returns organization info for the current user, from the Cloud backend API. */
    getUsersMe = (): Promise<Organization | null> => this
        .get(`users/me`)
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
        .get(`projects`)
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
        const request = this.post(`projects`).json(body);
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
        const request = this.post(`projects/${projectId}/close`);
        const response = await request.send()

        // FIXME [NP]: make this error type-safe with Result.
        // FIXME [NP]: should we really be treating these as special cases?
        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to close project.");
        }
    }

    /** Returns project details for the specified project ID, from the Cloud backend API */
    getProject = async (projectId: ProjectId): Promise<Project> => {
        const request = this.get(`projects/${projectId}`);
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
        const request = this.get(`projects/${projectId}/open`).json(body);
        const response = await request.send()

        // FIXME [NP]: make this error type-safe with Result.
        // FIXME [NP]: should we really be treating these as special cases?
        if (response.status() == 401 || response.status() == 404) {
            throw new Error("Unable to open project.");
        }
    }
}


// FIXME [NP]: uncomment this once the backend is ready.
//// ======================
//// === BackendContext ===
//// ======================
//
//type BackendContextType = Backend;
//
//// eslint-disable-next-line @typescript-eslint/naming-convention
//const BackendContext = createContext<BackendContextType>({} as BackendContextType)
//
//
//
//// =======================
//// === BackendProvider ===
//// =======================
//
//export interface BackendProviderProps {
//    accessToken: string;
//    logger: Logger;
//    children: ReactNode;
//}
//
//// eslint-disable-next-line @typescript-eslint/naming-convention
//export const BackendProvider = (props: BackendProviderProps) => {
//    const { accessToken, logger, children } = props;
//
//    // Create an HTTP client with the access token as a default header. This way, any request made
//    // using this HTTP client will be authorized.
//    const headers = new Headers();
//    headers.append("Authorization", `Bearer ${accessToken}`);
//    const client = Client.builder().defaultHeaders(headers).build();
//
//    // Create a Cloud backend API client from the HTTP client.
//    const backend = new Backend(client, logger);
//
//    return (
//        <BackendContext.Provider value={backend}>
//            {children}
//        </BackendContext.Provider>
//    );
//}
//
//
//
//// ==================
//// === useBackend ===
//// ==================
//
//export const useBackend = () => useContext(BackendContext);



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

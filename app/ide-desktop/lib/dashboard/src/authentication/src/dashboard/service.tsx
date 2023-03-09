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

/** Default HTTP body for an "open project" request. */
const DEFAULT_OPEN_PROJECT_BODY: OpenProjectRequestBody = {
    forceCreate: false,
};

/** Relative HTTP path to the "set username" endpoint of the Cloud backend API. */
const CREATE_USER_PATH = "users";
/** Relative HTTP path to the "get user" endpoint of the Cloud backend API. */
const USERS_ME_PATH = "users/me";
/** Relative HTTP path to the "list projects" endpoint of the Cloud backend API. */
const LIST_PROJECTS_PATH = "projects";
/** Relative HTTP path to the "create project" endpoint of the Cloud backend API. */
const CREATE_PROJECT_PATH = "projects";
/** Relative HTTP path to the "list files" endpoint of the Cloud backend API. */
const LIST_FILES_PATH = "files";
/** Relative HTTP path to the "upload file" endpoint of the Cloud backend API. */
const UPLOAD_FILE_PATH = "files";
/** Relative HTTP path to the "create secret" endpoint of the Cloud backend API. */
const CREATE_SECRET_PATH = "projects";
/** Relative HTTP path to the "list secrets" endpoint of the Cloud backend API. */
const LIST_SECRETS_PATH = "secrets";
/** Relative HTTP path to the "create tag" endpoint of the Cloud backend API. */
const CREATE_TAG_PATH = "tags";
/** Relative HTTP path to the "list tags" endpoint of the Cloud backend API. */
const LIST_TAGS_PATH = "tags";
/** Relative HTTP path to the "list versions" endpoint of the Cloud backend API. */
const LIST_VERSIONS_PATH = "versions";
/** Relative HTTP path to the "close project" endpoint of the Cloud backend API. */
const closeProjectPath = (projectId: ProjectId) =>
    `projects/${projectId}/close`;
/** Relative HTTP path to the "get project details" endpoint of the Cloud backend API. */
const getProjectDetailsPath = (projectId: ProjectId) =>
    `projects/${projectId}`;
/** Relative HTTP path to the "open project" endpoint of the Cloud backend API. */
const openProjectPath = (projectId: ProjectId) => `projects/${projectId}/open`;
/** Relative HTTP path to the "project update" endpoint of the Cloud backend API. */
const projectUpdatePath = (projectId: ProjectId) => `projects/${projectId}`;
/** Relative HTTP path to the "delete project" endpoint of the Cloud backend API. */
const deleteProjectPath = (projectId: ProjectId) => `projects/${projectId}`;
/** Relative HTTP path to the "check resources" endpoint of the Cloud backend API. */
const checkResourcesPath = (projectId: ProjectId) =>
    `projects/${projectId}/resources`;
/** Relative HTTP path to the "delete file" endpoint of the Cloud backend API. */
const deleteFilePath = (fileId: FileId) => `files/${fileId}`;
/** Relative HTTP path to the "get project" endpoint of the Cloud backend API. */
const getSecretPath = (secretId: SecretId) => `secrets/${secretId}`;
/** Relative HTTP path to the "delete secret" endpoint of the Cloud backend API. */
const deleteSecretPath = (secretId: SecretId) => `secrets/${secretId}`;
/** Relative HTTP path to the "delete tag" endpoint of the Cloud backend API. */
const deleteTagPath = (tagId: TagId) => `secrets/${tagId}`;



// =============
// === Types ===
// =============

/** Unique identifier for a user/organization. */
export type UserOrOrganizationId = string & { _brand: "UserOrOrganizationId"; };

/** Unique identifier for a user's project. */
export type ProjectId = string & { _brand: "ProjectId"; };

/** Unique identifier for an uploaded file. */
export type FileId = string & { _brand: "FileId"; };

/** Unique identifier for a secret environment variable. */
export type SecretId = string & { _brand: "SecretId"; };

/** Unique identifier for a file tag or project tag. */
export type TagId = string & { _brand: "TagId"; };

/** A URL. */
export type Address = string & { _brand: "Address"; };

/** An email address. */
export type EmailAddress = string & { _brand: "EmailAddress"; };

/** An AWS S3 file path. */
export type S3FilePath = string & { _brand: "S3FilePath"; };

export type Ami = string & { _brand: "Ami"; };

/** An RFC 3339 DateTime string. */
export type Rfc3339DateTime = string & { _brand: "Rfc3339DateTime"; };

/** A user/organization in the application. These are the primary owners of a project. */
export interface UserOrOrganization {
    id: UserOrOrganizationId;
    name: string;
    email: EmailAddress;
}

export enum VersionLifecycle {
    stable = "Stable",
    releaseCandidate = "ReleaseCandidate",
    nightly = "Nightly",
    development = "Development",
}

export interface VersionNumber {
    value: string;
    lifecycle: VersionLifecycle;
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

/** Common `Project` fields returned by all `Project`-related endpoints.  */
export interface BaseProject {
    organizationId: string;
    projectId: ProjectId;
    name: string;
}

/** A `Project` returned by `createProject`. */
export interface CreatedProject extends BaseProject {
    state: ProjectStateType;
    packageName: string;
}

/** A `Project` returned by `listProjects`. */
export interface ListedProject extends CreatedProject {
    address: Address | null;
}

/** A `Project` returned by `updateProject`. */
export interface UpdatedProject extends BaseProject {
    ami: Ami | null;
    ideVersion: VersionNumber | null;
    engineVersion: VersionNumber | null;
}

/** A user/organization's project containing and/or currently executing code. */
export interface Project extends ListedProject {
    ideVersion: VersionNumber | null;
    engineVersion: VersionNumber | null;
}

/** Metadata describing an uploaded file. */
export interface File {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    file_id: FileId;
    // eslint-disable-next-line @typescript-eslint/naming-convention
    file_name: string | null;
    path: S3FilePath;
}

/** Metadata uniquely identifying an uploaded file. */
export interface FileInfo {
    /* TODO: Should potentially be S3FilePath,
     * but it's just string on the backend. */
    path: string;
    id: FileId;
}

/** A secret environment variable. */
export interface Secret {
    id: SecretId;
    value: string;
}

/** A secret environment variable and metadata uniquely identifying it. */
export interface SecretAndInfo {
    id: SecretId;
    name: string;
    value: string;
}

/** Metadata uniquely identifying a secret environment variable. */
export interface SecretInfo {
    name: string;
    id: SecretId;
}

export enum TagObjectType {
    file = "File",
    project = "Project",
}

/** A file tag or project tag. */
export interface Tag {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    organization_id: UserOrOrganizationId;
    id: TagId;
    name: string;
    value: string;
    // eslint-disable-next-line @typescript-eslint/naming-convention
    object_type: TagObjectType;
    // eslint-disable-next-line @typescript-eslint/naming-convention
    object_id: string;
}

/** Metadata uniquely identifying a file tag or project tag. */
export interface TagInfo {
    id: TagId;
    name: string;
    value: string;
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
    number: VersionNumber;
    ami: Ami | undefined;
    created: Rfc3339DateTime;
    // This does not follow our naming convention because it's defined this way in the backend, so we need to match it.
    // eslint-disable-next-line @typescript-eslint/naming-convention
    version_type: VersionType;
}

/** Resource usage of a VM. */
export interface ResourceUsage {
    /** Percentage of memory used. */
    memory: number;
    /** Percentage of CPU time used since boot. */
    cpu: number;
    /** Percentage of disk space used. */
    storage: number;
}



// =================
// === Endpoints ===
// =================

/** HTTP request body for the "set username" endpoint. */
export interface CreateUserRequestBody {
    userName: string;
    userEmail: EmailAddress;
}

/** HTTP request body for the "create project" endpoint. */
export interface CreateProjectRequestBody {
    projectName: string;
    projectTemplateName?: string;
}

/**
 * HTTP request body for the "project update" endpoint.
 * Only updates of the `projectName` or `ami` are allowed.
 */
export interface ProjectUpdateRequestBody {
    projectName: string | null;
    ami: Ami | null;
    ideVersion: VersionNumber | null;
}

/** HTTP request body for the "open project" endpoint. */
export interface OpenProjectRequestBody {
    forceCreate: boolean;
}

/** HTTP request body for the "create secret" endpoint. */
export interface CreateSecretRequestBody {
    name: string;
    value: string;
}

/** HTTP request body for the "create tag" endpoint. */
export interface CreateTagRequestBody {
    name: string;
    value: string;
    objectType: TagObjectType;
    objectId: string;
}

/** URL query string parameters for the "upload file" endpoint. */
export interface UploadFileRequestParams {
    fileId?: string;
    fileName?: string;
}

/** URL query string parameters for the "list tags" endpoint. */
export interface ListTagsRequestParams {
    tagType: TagObjectType;
}

/** URL query string parameters for the "list versions" endpoint. */
export interface ListVersionsRequestParams {
    versionType: VersionType;
    default: boolean;
}

/** HTTP response body for the "list projects" endpoint. */
interface ListProjectsResponseBody {
    projects: ListedProject[];
}

/** HTTP response body for the "list files" endpoint. */
interface ListFilesResponseBody {
    files: File[];
}

/** HTTP response body for the "list secrets" endpoint. */
interface ListSecretsResponseBody {
    secrets: SecretInfo[];
}

/** HTTP response body for the "list tag" endpoint. */
interface ListTagsResponseBody {
    tags: Tag[];
}

/** HTTP response body for the "list versions" endpoint. */
interface ListVersionsResponseBody {
    versions: Version[];
}



// ===============
// === Backend ===
// ===============

/** Class for sending requests to the Cloud backend API endpoints. */
export class Backend {
    private readonly client: http.Client;
    private readonly logger: loggerProvider.Logger;

    /** Creates a new instance of the {@link Backend} API client.
     *
     * @throws An error if the `Authorization` header is not set on the given `client`. */
    constructor(client: http.Client, logger: loggerProvider.Logger) {
        this.client = client;
        this.logger = logger;

        // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
        // set.
        if (!this.client.defaultHeaders?.has("Authorization")) {
            throw new Error("Authorization header not set.");
        }
    }

    /** Sends an HTTP GET request to the given path. */
    private get = (path: string) => this.client.get(`${config.ACTIVE_CONFIG.apiUrl}/${path}`);

    /** Sends a JSON HTTP POST request to the given path. */
    private post = (path: string, payload: object) => this.client.post(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload);

    /** Sends a binary HTTP POST request to the given path. */
    private postBase64 = (path: string, payload: Blob) => this.client.postBase64(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload);

    /** Sends a JSON HTTP PUT request to the given path. */
    private put = (path: string, payload: object) => this.client.put(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload);

    /** Sends an HTTP DELETE request to the given path. */
    private delete = (path: string) => this.client.delete(`${config.ACTIVE_CONFIG.apiUrl}/${path}`);

    /* FIXME: unused */
    /** Logs the error that occurred and throws a new one with a more user-friendly message. */
    errorHandler = (message: string) => (error: Error) => {
        this.logger.error(error.message);
        throw new Error(message);
    };

    /** Sets the username of the current user, on the Cloud backend API. */
    createUser = async (body: CreateUserRequestBody): Promise<UserOrOrganization> => {
        const response = await this.post(CREATE_USER_PATH, body);
        return response.json() as Promise<UserOrOrganization>;
    };

    /** Returns organization info for the current user, from the Cloud backend API.
     *
     * @returns `null` if status code 401 or 404 was received. */
    usersMe = async (): Promise<UserOrOrganization | null> => {
        const response = await this.get(USERS_ME_PATH);

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            return null;
        }

        return response.json() as Promise<UserOrOrganization>;
    };

    /** Returns a list of projects belonging to the current user, from the Cloud backend API.
     * 
     * @returns `[]` if status code 401 or 404 was received.
     */
    listProjects = async (): Promise<ListedProject[]> => {
        const response = await this.get(LIST_PROJECTS_PATH);

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            return [];
        }

        const model = await (response.json() as Promise<ListProjectsResponseBody>);
        return model.projects;
    };

    /** Creates a project for the current user, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    createProject = async (body: CreateProjectRequestBody): Promise<CreatedProject> => {
        const response = await this.post(CREATE_PROJECT_PATH, body);

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to create project.");
        }

        return response.json() as Promise<CreatedProject>;
    };

    /** Closes the project identified by the given project ID, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    closeProject = async (projectId: ProjectId): Promise<void> => {
        const response = await this.post(closeProjectPath(projectId), {});

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to close project.");
        }
    };

    /** Returns project details for the specified project ID, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    getProjectDetails = async (projectId: ProjectId): Promise<Project> => {
        const response = await this.get(getProjectDetailsPath(projectId));

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to get project details.");
        }

        return response.json() as Promise<Project>;
    };

    /** Sets project to an open state, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    openProject = async (
        projectId: ProjectId,
        body: OpenProjectRequestBody = DEFAULT_OPEN_PROJECT_BODY
    ): Promise<void> => {
        const response = await this.post(openProjectPath(projectId), body);

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to open project.");
        }
    };

    projectUpdate = async (projectId: ProjectId, body: ProjectUpdateRequestBody) => {
        const response = await this.put(projectUpdatePath(projectId), body);


        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to open project.");
        }

        return await response.json() as Promise<UpdatedProject>;
    };

    /** Deletes project, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    deleteProject = async (projectId: ProjectId): Promise<void> => {
        const response = await this.delete(deleteProjectPath(projectId));

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to delete project.");
        }
    };

    /** Returns project memory, processor and storage usage, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    checkResources = async (projectId: ProjectId): Promise<ResourceUsage> => {
        const response = await this.get(checkResourcesPath(projectId));

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to get resource usage for project.");
        }

        return response.json() as Promise<ResourceUsage>;
    };

    /** Returns a list of files accessible by the current user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    listFiles = async (): Promise<File[]> => {
        const response = await this.get(LIST_FILES_PATH);

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to list files.");
        }

        const model = await (response.json() as Promise<ListFilesResponseBody>);
        return model.files;
    };

    /** Uploads a file, to the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    uploadFile = async (params: UploadFileRequestParams, body: Blob): Promise<FileInfo> => {
        const response = await this.postBase64(UPLOAD_FILE_PATH + "?" + new URLSearchParams({
            // eslint-disable-next-line @typescript-eslint/naming-convention
            ...(params.fileName ? { file_name: params.fileName } : {}),
            // eslint-disable-next-line @typescript-eslint/naming-convention
            ...(params.fileId ? { file_id: params.fileId } : {}),
        }).toString(), body);

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to upload file.");
        }

        return await response.json() as Promise<FileInfo>;
    };

    /** Deletes a file, to the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    deleteFile = async (fileId: FileId): Promise<void> => {
        const response = await this.delete(deleteFilePath(fileId));

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to delete file.");
        }
    };

    /** Creates a secret environment variable, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    createSecret = async (body: CreateSecretRequestBody): Promise<SecretAndInfo> => {
        const response = await this.post(CREATE_SECRET_PATH, {
            // eslint-disable-next-line @typescript-eslint/naming-convention
            secret_name: body.name,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            secret_value: body.value,
        });

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to create secret.");
        }

        return await response.json() as Promise<SecretAndInfo>;
    };

    /** Returns a secret environment variable, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    getSecret = async (secretId: SecretId): Promise<Secret> => {
        const response = await this.get(getSecretPath(secretId));

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to get secret.");
        }

        return response.json() as Promise<Secret>;
    };

    /** Returns the secret environment variables accessible by the user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    listSecrets = async (): Promise<SecretInfo[]> => {
        const response = await this.get(LIST_SECRETS_PATH);

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to list secrets.");
        }

        const model = await (response.json() as Promise<ListSecretsResponseBody>);
        return model.secrets;
    };

    /** Deletes a secret environment variable, to the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    deleteSecret = async (secretId: SecretId): Promise<void> => {
        const response = await this.delete(deleteSecretPath(secretId));

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to delete secret.");
        }
    };

    /** Creates a file tag or project tag, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    createTag = async (body: CreateTagRequestBody): Promise<TagInfo> => {
        const response = await this.post(CREATE_TAG_PATH, {
            // eslint-disable-next-line @typescript-eslint/naming-convention
            tag_name: body.name,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            tag_value: body.value,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            object_type: body.objectType,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            object_id: body.objectId,
        });

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to create create tag.");
        }

        return await response.json() as Promise<TagInfo>;
    };

    /** Returns file tags or project tags accessible by the user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    listTags = async (params: ListTagsRequestParams): Promise<Tag[]> => {
        const response = await this.get(LIST_TAGS_PATH + "?" + new URLSearchParams({
            // eslint-disable-next-line @typescript-eslint/naming-convention
            tag_type: params.tagType,
        }).toString());

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to list tags.");
        }

        const model = await (response.json() as Promise<ListTagsResponseBody>);
        return model.tags;
    };

    /** Deletes a secret environment variable, to the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    deleteTag = async (tagId: TagId): Promise<void> => {
        const response = await this.delete(deleteTagPath(tagId));

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to delete secret.");
        }
    };

    /** Returns list of backend or IDE versions, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    listVersions = async (params: ListVersionsRequestParams): Promise<Version[]> => {
        const response = await this.get(LIST_VERSIONS_PATH + "?" + new URLSearchParams({
            // eslint-disable-next-line @typescript-eslint/naming-convention
            version_type: params.versionType,
            default: String(params.default),
        }).toString());

        if (response.status === http.HttpStatus.unauthorized || response.status === http.HttpStatus.notFound) {
            throw new Error("Unable to list versions.");
        }

        const model = await (response.json() as Promise<ListVersionsResponseBody>);
        return model.versions;
    };
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
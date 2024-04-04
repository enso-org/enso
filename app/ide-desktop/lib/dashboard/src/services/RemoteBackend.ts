/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as detect from 'enso-common/src/detect'

import type * as text from '#/text'

import type * as loggerProvider from '#/providers/LoggerProvider'
import type * as textProvider from '#/providers/TextProvider'

import Backend, * as backend from '#/services/Backend'
import * as remoteBackendPaths from '#/services/remoteBackendPaths'

import * as dateTime from '#/utilities/dateTime'
import type HttpClient from '#/utilities/HttpClient'
import * as object from '#/utilities/object'
import * as uniqueString from '#/utilities/uniqueString'

// =================
// === Constants ===
// =================

/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_FIRST = 200
/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_LAST = 299
/** HTTP status indicating that the resource does not exist. */
const STATUS_NOT_FOUND = 404
/** HTTP status indicating that the server encountered a fatal exception. */
const STATUS_SERVER_ERROR = 500
/** The interval between requests checking whether the IDE is ready. */
const CHECK_STATUS_INTERVAL_MS = 5000
/** The number of milliseconds in one day. */
const ONE_DAY_MS = 86_400_000

// =============
// === Types ===
// =============

/** The format of all errors returned by the backend. */
interface RemoteBackendError {
  readonly type: string
  readonly code: string
  readonly message: string
  readonly param: string
}

// ============================
// === responseIsSuccessful ===
// ============================

/** Whether a response has a success HTTP status code (200-299). */
function responseIsSuccessful(response: Response) {
  return response.status >= STATUS_SUCCESS_FIRST && response.status <= STATUS_SUCCESS_LAST
}

// ============================
// === overwriteMaterialize ===
// ============================

/** Any object that has a `materialize()` method that accepts no arguments. */
interface HasMaterialize<T> {
  readonly materialize: () => Promise<T> | T
}

/** Overwrites `materialize` so that it does not. */
function overwriteMaterialize<T>(
  smartAsset: HasMaterialize<T>,
  materialize: () => Promise<T>
): () => Promise<T> {
  return () => {
    const promise = materialize()
    // This is SAFE - `materialize` is intended to be mutated here. This is the only place where
    // it should be mutated.
    object.unsafeMutable(smartAsset).materialize = () => promise
    return promise
  }
}

// ====================
// === listVersions ===
// ====================

/** URL query string parameters for the "list versions" endpoint. */
export interface ListVersionsRequestParams {
  readonly versionType: backend.VersionType
  readonly default: boolean
}

/** Return a list of backend or IDE versions.
 * @throws An error if a non-successful status code (not 200-299) was received. */
async function listVersions(
  client: HttpClient,
  params: ListVersionsRequestParams
): Promise<backend.Version[]> {
  /** HTTP response body for this endpoint. */
  interface ResponseBody {
    readonly versions: [backend.Version, ...backend.Version[]]
  }
  const paramsString = new URLSearchParams({
    // eslint-disable-next-line @typescript-eslint/naming-convention
    version_type: params.versionType,
    default: String(params.default),
  }).toString()
  const path = remoteBackendPaths.LIST_VERSIONS_PATH + '?' + paramsString
  const response = await client.get<ResponseBody>(`${process.env.ENSO_CLOUD_API_URL}/${path}`)
  if (!responseIsSuccessful(response)) {
    throw new Error(`Could not list versions of type '${params.versionType}'.`)
  } else {
    return (await response.json()).versions
  }
}

// =========================
// === getDefaultVersion ===
// =========================

const DEFAULT_VERSIONS: Partial<Record<backend.VersionType, DefaultVersionInfo>> = {}

/** Get the default version given the type of version (IDE or backend). */
async function getDefaultVersion(client: HttpClient, versionType: backend.VersionType) {
  const cached = DEFAULT_VERSIONS[versionType]
  const nowEpochMs = Number(new Date())
  if (cached != null && nowEpochMs - cached.lastUpdatedEpochMs < ONE_DAY_MS) {
    return cached.version
  } else {
    const version = (await listVersions(client, { versionType, default: true }))[0]?.number
    if (version == null) {
      throw new Error(`No default ${versionType} version found.`)
    } else {
      const info: DefaultVersionInfo = { version, lastUpdatedEpochMs: nowEpochMs }
      DEFAULT_VERSIONS[versionType] = info
      return info.version
    }
  }
}

// =============
// === Types ===
// =============

/** URL query string parameters for the "upload file" endpoint. */
export interface UploadFileRequestParams {
  readonly fileId: backend.AssetId | null
  // Marked as optional in the data type, however it is required by the actual route handler.
  readonly fileName: string
  readonly parentDirectoryId: backend.DirectoryId | null
}

// =====================
// === Smart objects ===
// =====================

/** A wrapper around a subset of the API endpoints. */
class SmartObject<T> implements backend.SmartObject<T> {
  /** Create a {@link SmartObject}. */
  constructor(
    protected readonly client: HttpClient,
    protected readonly logger: loggerProvider.Logger,
    protected readonly getText: GetText,
    readonly value: T
  ) {}

  /** Return a copy of this object, but with a different value. */
  withValue(value: T): this {
    // This is SAFE (as long as no child classes override the constructor),
    // as it uses runtime reflection to get the constructor of the current object.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-call, no-restricted-syntax, @typescript-eslint/no-explicit-any
    return new (this.constructor as any)(this.client, this.logger, this.getText, value)
  }

  /** Log an error message and throw an {@link Error} with the specified message.
   * @throws {Error} Always. */
  protected async throw<K extends Extract<text.TextId, `${string}BackendError`>>(
    response: Response | null,
    textId: K,
    ...replacements: text.Replacements[K]
  ): Promise<never> {
    const error =
      response == null
        ? { message: 'unknown error' }
        : // This is SAFE only when the response has been confirmed to have an erroring status code.
          // eslint-disable-next-line no-restricted-syntax
          ((await response.json()) as RemoteBackendError)
    const message = `${this.getText(textId, ...replacements)}: ${error.message}.`
    this.logger.error(message)
    throw new Error(message)
  }

  /** Send an HTTP GET request to the given path. */
  protected httpGet<R = void>(path: string) {
    return this.client.get<R>(`${process.env.ENSO_CLOUD_API_URL}/${path}`)
  }

  /** Send a JSON HTTP POST request to the given path. */
  protected httpPost<R = void>(path: string, payload: object) {
    return this.client.post<R>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a binary HTTP POST request to the given path. */
  protected httpPostBinary<R = void>(path: string, payload: Blob) {
    return this.client.postBinary<R>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  protected httpPatch<R = void>(path: string, payload: object) {
    return this.client.patch<R>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a JSON HTTP PUT request to the given path. */
  protected httpPut<R = void>(path: string, payload: object) {
    return this.client.put<R>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a binary HTTP PUT request to the given path. */
  protected httpPutBinary<R = void>(path: string, payload: Blob) {
    return this.client.putBinary<R>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send an HTTP DELETE request to the given path. */
  protected httpDelete<R = void>(path: string) {
    return this.client.delete<R>(`${process.env.ENSO_CLOUD_API_URL}/${path}`)
  }
}

/** A smart wrapper around a {@link backend.User}. */
class SmartUser extends SmartObject<backend.User> implements backend.SmartUser {
  /** Return details for the current organization.
   * @returns `null` if a non-successful status code (not 200-299) was received. */
  async getOrganization(): Promise<backend.SmartOrganization> {
    const path = remoteBackendPaths.GET_ORGANIZATION_PATH
    const response = await this.httpGet<backend.OrganizationInfo>(path)
    if (response.status === STATUS_NOT_FOUND) {
      // Organization info has not yet been created.
      return new SmartOrganization(this.client, this.logger, this.getText, null)
    } else if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getOrganizationBackendError')
    } else {
      return new SmartOrganization(this.client, this.logger, this.getText, await response.json())
    }
  }

  /** Change the username of the current user. */
  async update(body: backend.UpdateUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.UPDATE_CURRENT_USER_PATH
    const response = await this.httpPut(path, body)
    if (!responseIsSuccessful(response)) {
      if (body.username != null) {
        return this.throw(response, 'updateUsernameBackendError')
      } else {
        return this.throw(response, 'updateUserBackendError')
      }
    } else {
      return
    }
  }

  /** Delete the current user. */
  async delete(): Promise<void> {
    const response = await this.httpDelete(remoteBackendPaths.DELETE_USER_PATH)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'deleteUserBackendError')
    } else {
      return
    }
  }

  /** Upload a new profile picture for the current user. */
  async uploadPicture(
    params: backend.UploadPictureRequestParams,
    file: Blob
  ): Promise<backend.User> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_USER_PICTURE_PATH}?${paramsString}`
    const response = await this.httpPutBinary<backend.User>(path, file)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'uploadUserPictureBackendError')
    } else {
      return await response.json()
    }
  }

  /** Get the root directory for this user. */
  rootDirectory(): backend.SmartDirectory {
    const rootDirectory = backend.createRootDirectoryAsset(this.value.rootDirectoryId)
    return new SmartDirectory(this.client, this.logger, this.getText, rootDirectory)
  }

  /** Invite a new user to the organization by email. */
  async invite(email: backend.EmailAddress): Promise<void> {
    const path = remoteBackendPaths.INVITE_USER_PATH
    const response = await this.httpPost(path, { organizationId: this.value.id, userEmail: email })
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'inviteUserBackendError', email)
    } else {
      return
    }
  }

  /** Return a list of all users in the same organization. */
  async listUsers(): Promise<backend.SimpleUser[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly users: backend.SimpleUser[]
    }
    const path = remoteBackendPaths.LIST_USERS_PATH
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'listUsersBackendError')
    } else {
      return (await response.json()).users
    }
  }

  /** List recently modified assets. */
  async listRecentFiles(): Promise<backend.AnySmartAsset[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly assets: backend.AnyAsset[]
    }
    const paramsString = new URLSearchParams([['recent_projects', String(true)]]).toString()
    const path = remoteBackendPaths.LIST_DIRECTORY_PATH + '?' + paramsString
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      if (response.status === STATUS_SERVER_ERROR) {
        // The directory is probably empty.
        return []
      } else {
        return this.throw(response, 'listRecentFilesBackendError')
      }
    } else {
      const assets = (await response.json()).assets
        .map(asset =>
          object.merge(asset, {
            // eslint-disable-next-line no-restricted-syntax
            type: asset.id.match(/^(.+?)-/)?.[1] as backend.AssetType,
          })
        )
        .map(asset =>
          object.merge(asset, {
            permissions: [...(asset.permissions ?? [])].sort(backend.compareUserPermissions),
          })
        )
      return assets.map(intoSmartAsset(this.client, this.logger, this.getText))
    }
  }

  /** Return the secret environment variables accessible by the user. */
  async listSecrets(): Promise<backend.SecretInfo[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly secrets: backend.SecretInfo[]
    }
    const path = remoteBackendPaths.LIST_SECRETS_PATH
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'listSecretsBackendError')
    } else {
      return (await response.json()).secrets
    }
  }

  /** List events in the organization's audit log. */
  async getLogEvents(): Promise<backend.Event[]> {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly events: backend.Event[]
    }

    const path = remoteBackendPaths.GET_LOG_EVENTS_PATH
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getLogEventsBackendError')
    } else {
      const json = await response.json()
      return json.events
    }
  }
}

/** A smart wrapper around an {@link backend.OrganizationInfo}. */
class SmartOrganization
  extends SmartObject<backend.OrganizationInfo | null>
  implements backend.SmartOrganization
{
  /** Update details for the current organization. */
  async update(
    body: backend.UpdateOrganizationRequestBody
  ): Promise<backend.OrganizationInfo | null> {
    const path = remoteBackendPaths.UPDATE_ORGANIZATION_PATH
    const response = await this.httpPatch<backend.OrganizationInfo>(path, body)

    if (response.status === STATUS_NOT_FOUND) {
      // Organization info has not yet been created.
      return null
    } else if (!responseIsSuccessful(response)) {
      return this.throw(response, 'updateOrganizationBackendError')
    } else {
      return await response.json()
    }
  }

  /** Upload a new profile picture for the current organization. */
  async uploadPicture(
    params: backend.UploadPictureRequestParams,
    file: Blob
  ): Promise<backend.OrganizationInfo> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_ORGANIZATION_PICTURE_PATH}?${paramsString}`
    const response = await this.httpPutBinary<backend.OrganizationInfo>(path, file)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'uploadOrganizationPictureBackendError')
    } else {
      return await response.json()
    }
  }
}

/** A smart wrapper around a {@link backend.AnyAsset}. */
class SmartAsset<T extends backend.AnyAsset = backend.AnyAsset>
  extends SmartObject<T>
  implements backend.SmartAsset
{
  /** The type of the wrapped value. */
  get type(): T['type'] {
    return this.value.type
  }

  /** If this is a placeholder asset, return its non-placeholder equivalent after creating it on
   * the backend. Otherwise, return `this`. */
  materialize(): Promise<this> | this {
    return this
  }

  /** Change the parent directory of an asset. */
  async update(body: backend.UpdateAssetRequestBody): Promise<this> {
    if (body.description == null && body.parentDirectoryId == null) {
      // No action is needed.
      return this
    } else {
      const path = remoteBackendPaths.updateAssetPath(this.value.id)
      const response = await this.httpPatch(path, {
        description: body.description ?? null,
        parentDirectoryId: body.parentDirectoryId ?? null,
      })
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'updateAssetBackendError', this.value.title)
      } else {
        return body.description == null && body.parentDirectoryId == null
          ? this
          : this.withValue(
              // @ts-expect-error TypeScript is not capable of properly type-checking generic parameters.
              object.merge(this.value, {
                ...(body.description == null ? {} : { description: body.description }),
                ...(body.parentDirectoryId == null ? {} : { parentId: body.parentDirectoryId }),
              })
            )
      }
    }
  }

  /** Move an arbitrary asset to the trash. */
  async delete(force?: boolean): Promise<void> {
    const paramsString = new URLSearchParams([['force', String(force ?? false)]]).toString()
    const path = remoteBackendPaths.deleteAssetPath(this.value.id) + '?' + paramsString
    const response = await this.httpDelete(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'deleteAssetBackendError', this.value.title)
    } else {
      return
    }
  }

  /** Restore an arbitrary asset from the trash. */
  async undoDelete(): Promise<void> {
    const path = remoteBackendPaths.UNDO_DELETE_ASSET_PATH
    const response = await this.httpPatch(path, { assetId: this.value.id })
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'undoDeleteAssetBackendError', this.value.title)
    } else {
      return
    }
  }

  /** Copy an arbitrary asset to another directory. */
  async copy(
    parentDirectoryId: backend.DirectoryId,
    parentDirectoryTitle: string
  ): Promise<backend.CopyAssetResponse> {
    const path = remoteBackendPaths.copyAssetPath(this.value.id)
    const response = await this.httpPost<backend.CopyAssetResponse>(path, {
      parentDirectoryId,
    })
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'copyAssetBackendError', this.value.title, parentDirectoryTitle)
    } else {
      return await response.json()
    }
  }

  /** Return a copy of this object, but with a different value. */
  // @ts-expect-error This is SAFE, under the same conditions as `SmartObject.withValue`.
  override withValue(value: T): this {
    return super.withValue(value)
  }

  /** List all versions of this asset. Only works for projects and files. */
  async listVersions(): Promise<backend.AssetVersions> {
    const path = remoteBackendPaths.listAssetVersionsPath(this.value.id)
    const response = await this.httpGet<backend.AssetVersions>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'listAssetVersionsBackendError', this.value.title)
    } else {
      return await response.json()
    }
  }

  /** Set permissions for a user. */
  async setPermissions(body: backend.CreatePermissionRequestBody): Promise<void> {
    const path = remoteBackendPaths.CREATE_PERMISSION_PATH
    const response = await this.httpPost<backend.User>(path, {
      ...body,
      resourceId: this.value.id,
    })
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'createPermissionBackendError')
    } else {
      return
    }
  }

  /** Set the full list of labels for a specific asset.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  async setTags(labels: backend.LabelName[]) {
    const path = remoteBackendPaths.associateTagPath(this.value.id)
    const response = await this.httpPatch(path, { labels })
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'associateLabelsBackendError', this.value.title)
    } else {
      return
    }
  }
}

/** Converts an {@link backend.AnyAsset} into its corresponding
 * {@link backend.AnySmartAsset}. */
function intoSmartAsset(client: HttpClient, logger: loggerProvider.Logger, getText: GetText) {
  return (asset: backend.AnyAsset): backend.AnySmartAsset => {
    switch (asset.type) {
      case backend.AssetType.directory: {
        return new SmartDirectory(client, logger, getText, asset)
      }
      case backend.AssetType.project: {
        return new SmartProject(client, logger, getText, asset)
      }
      case backend.AssetType.file: {
        return new SmartFile(client, logger, getText, asset)
      }
      case backend.AssetType.dataLink: {
        return new SmartDataLink(client, logger, getText, asset)
      }
      case backend.AssetType.secret: {
        return new SmartSecret(client, logger, getText, asset)
      }
      case backend.AssetType.specialLoading:
      case backend.AssetType.specialEmpty: {
        throw new Error(
          `'${asset.type}' is a special asset type that should never be returned by the backend.`
        )
      }
    }
  }
}

/** A smart wrapper around a {@link backend.DirectoryAsset}. */
class SmartDirectory extends SmartAsset<backend.DirectoryAsset> implements backend.SmartDirectory {
  /** Return a list of assets in a directory. */
  async list(query: backend.ListDirectoryRequestParams): Promise<backend.AnySmartAsset[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly assets: backend.AnyAsset[]
    }
    const paramsString = new URLSearchParams([
      ['parent_id', this.value.id],
      ...(query.filterBy != null ? [['filter_by', query.filterBy]] : []),
      ...(query.labels != null ? query.labels.map(label => ['label', label]) : []),
    ]).toString()
    const path = remoteBackendPaths.LIST_DIRECTORY_PATH + '?' + paramsString
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      if (response.status === STATUS_SERVER_ERROR) {
        // The directory is probably empty.
        return []
      } else {
        return this.throw(response, 'listFolderBackendError', this.value.title)
      }
    } else {
      const assets = (await response.json()).assets
        .map(asset =>
          object.merge(asset, {
            // eslint-disable-next-line no-restricted-syntax
            type: asset.id.match(/^(.+?)-/)?.[1] as backend.AssetType,
          })
        )
        .map(asset =>
          object.merge(asset, {
            permissions: [...(asset.permissions ?? [])].sort(backend.compareUserPermissions),
          })
        )
      return assets.map(intoSmartAsset(this.client, this.logger, this.getText))
    }
  }

  /** Change the name, description or parent of a directory. */
  override async update(body: backend.UpdateAssetOrDirectoryRequestBody): Promise<this> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly id: backend.DirectoryId
      readonly parentId: backend.DirectoryId
      readonly title: string
    }
    const updated = await super.update(body)
    const path = remoteBackendPaths.updateDirectoryPath(this.value.id)
    if (body.title == null) {
      return updated
    } else {
      const response = await this.httpPut<ResponseBody>(path, { title: body.title })
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'updateFolderBackendError', this.value.title)
      } else {
        return this.withValue(object.merge(updated.value, await response.json()))
      }
    }
  }

  /** Create a {@link backend.SpecialLoadingAsset}. */
  createSpecialLoadingAsset(): backend.SmartSpecialLoadingAsset {
    return new SmartAsset<backend.SpecialLoadingAsset>(this.client, this.logger, this.getText, {
      type: backend.AssetType.specialLoading,
      title: '',
      id: backend.LoadingAssetId(
        `${backend.AssetType.specialLoading}-${uniqueString.uniqueString()}`
      ),
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions: [],
      projectState: null,
      labels: [],
      description: null,
    })
  }

  /** Create a {@link backend.SpecialEmptyAsset}. */
  createSpecialEmptyAsset(): backend.SmartSpecialEmptyAsset {
    return new SmartAsset<backend.SpecialEmptyAsset>(this.client, this.logger, this.getText, {
      type: backend.AssetType.specialEmpty,
      title: '',
      id: backend.EmptyAssetId(`${backend.AssetType.specialEmpty}-${uniqueString.uniqueString()}`),
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions: [],
      projectState: null,
      labels: [],
      description: null,
    })
  }

  /** Create a {@link SmartDirectory} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderDirectory(
    title: string,
    permissions: backend.UserPermission[]
  ): backend.SmartDirectory {
    const result = new SmartDirectory(this.client, this.logger, this.getText, {
      type: backend.AssetType.directory,
      id: backend.DirectoryId(`${backend.AssetType.directory}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: null,
      labels: [],
      description: null,
    })
    result.materialize = overwriteMaterialize(result, async (): Promise<SmartDirectory> => {
      /** HTTP request body for this endpoint. */
      interface Body {
        readonly title: string
        readonly parentId: backend.DirectoryId | null
      }
      /** HTTP response body for this endpoint. */
      interface ResponseBody {
        readonly id: backend.DirectoryId
        readonly parentId: backend.DirectoryId
        readonly title: string
      }
      const path = remoteBackendPaths.CREATE_DIRECTORY_PATH
      const body: Body = { title, parentId: this.value.id }
      const response = await this.httpPost<ResponseBody>(path, body)
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'createFolderBackendError', this.value.title)
      } else {
        const reponseBody = await response.json()
        return result.withValue(object.merge(result.value, reponseBody))
      }
    })
    return result
  }

  /** Create a {@link SmartProject} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderProject(
    title: string,
    fileOrTemplateName: File | string | null,
    permissions: backend.UserPermission[]
  ): backend.SmartProject {
    const result = new SmartProject(this.client, this.logger, this.getText, {
      type: backend.AssetType.project,
      id: backend.ProjectId(`${backend.AssetType.project}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: {
        type: backend.ProjectState.placeholder,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        volume_id: '',
      },
      labels: [],
      description: null,
    })
    /** A project returned by the endpoints. */
    interface CreatedProject {
      readonly organizationId: string
      readonly projectId: backend.ProjectId
      readonly name: string
      readonly state: backend.ProjectStateType
      readonly packageName: string
    }
    if (fileOrTemplateName instanceof File) {
      result.materialize = overwriteMaterialize(result, async () => {
        /** HTTP response body for this endpoint. */
        interface ResponseBody {
          readonly path: string
          readonly id: backend.FileId
          readonly project: CreatedProject | null
        }
        const paramsString = new URLSearchParams({
          // eslint-disable-next-line @typescript-eslint/naming-convention
          file_name: title,
          // eslint-disable-next-line @typescript-eslint/naming-convention
          parent_directory_id: this.value.id,
        }).toString()
        const path = `${remoteBackendPaths.UPLOAD_FILE_PATH}?${paramsString}`
        const response = await this.httpPostBinary<ResponseBody>(path, fileOrTemplateName)
        if (!responseIsSuccessful(response)) {
          return this.throw(response, 'uploadFileWithNameBackendError', title)
        } else {
          const responseBody = await response.json()
          if (responseBody.project == null) {
            return this.throw(response, 'badCreateProjectResponseBackendError', title)
          } else {
            return result.withValue(
              object.merge(result.value, {
                id: responseBody.project.projectId,
                title: responseBody.project.name,
                projectState: responseBody.project.state,
              })
            )
          }
        }
      })
    } else {
      result.materialize = overwriteMaterialize(result, async () => {
        /** HTTP request body for this endpoint. */
        interface Body {
          readonly projectName: string
          readonly projectTemplateName: string | null
          readonly parentDirectoryId: backend.DirectoryId | null
        }
        /** HTTP response body for this endpoint. */
        interface ResponseBody extends CreatedProject {}
        const path = remoteBackendPaths.CREATE_PROJECT_PATH
        const body: Body = {
          projectName: title,
          projectTemplateName: fileOrTemplateName,
          parentDirectoryId: this.value.id,
        }
        const response = await this.httpPost<ResponseBody>(path, body)
        if (!responseIsSuccessful(response)) {
          return this.throw(response, 'createProjectBackendError', body.projectName)
        } else {
          const responseBody = await response.json()
          return result.withValue(
            object.merge(result.value, {
              id: responseBody.projectId,
              title: responseBody.name,
              projectState: responseBody.state,
            })
          )
        }
      })
    }
    return result
  }

  /** Create a {@link SmartFile} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderFile(
    title: string,
    file: File,
    permissions: backend.UserPermission[]
  ): backend.SmartFile {
    const result = new SmartFile(this.client, this.logger, this.getText, {
      type: backend.AssetType.file,
      id: backend.FileId(`${backend.AssetType.file}-${uniqueString.uniqueString()}`),
      title,
      parentId: this.value.id,
      permissions,
      modifiedAt: dateTime.toRfc3339(new Date()),
      projectState: null,
      labels: [],
      description: null,
    })
    result.materialize = overwriteMaterialize(result, async () => {
      /** HTTP response body for this endpoint. */
      interface ResponseBody {
        readonly path: string
        readonly id: backend.FileId
        readonly project: NonNullable<unknown> | null
      }
      const paramsString = new URLSearchParams({
        // eslint-disable-next-line @typescript-eslint/naming-convention
        file_name: title,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        parent_directory_id: this.value.id,
      }).toString()
      const path = `${remoteBackendPaths.UPLOAD_FILE_PATH}?${paramsString}`
      const response = await this.httpPostBinary<ResponseBody>(path, file)
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'uploadFileWithNameBackendError', title)
      } else {
        const responseBody = await response.json()
        return result.withValue(object.merge(result.value, { id: responseBody.id }))
      }
    })
    return result
  }

  /** Create a {@link SmartDataLink} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderDataLink(
    title: string,
    value: unknown,
    permissions: backend.UserPermission[]
  ): backend.SmartDataLink {
    const result = new SmartDataLink(this.client, this.logger, this.getText, {
      type: backend.AssetType.dataLink,
      id: backend.ConnectorId(`${backend.AssetType.dataLink}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: null,
      labels: [],
      description: null,
    })
    result.materialize = overwriteMaterialize(result, async () => {
      /** HTTP request body for this endpoint. */
      interface Body {
        readonly name: string
        readonly value: unknown
        readonly parentDirectoryId: backend.DirectoryId | null
      }
      const path = remoteBackendPaths.CREATE_CONNECTOR_PATH
      const body: Body = { parentDirectoryId: this.value.id, name: title, value }
      const response = await this.httpPost<backend.ConnectorInfo>(path, body)
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'createConnectorBackendError', body.name)
      } else {
        const info = await response.json()
        return result.withValue(object.merge(result.value, { id: info.id }))
      }
    })
    return result
  }

  /** Create a {@link SmartSecret} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderSecret(
    title: string,
    value: string,
    permissions: backend.UserPermission[]
  ): backend.SmartSecret {
    const result = new SmartSecret(this.client, this.logger, this.getText, {
      type: backend.AssetType.secret,
      id: backend.SecretId(`${backend.AssetType.secret}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: null,
      labels: [],
      description: null,
    })
    result.materialize = overwriteMaterialize(result, async () => {
      /** HTTP request body for this endpoint. */
      interface Body {
        readonly name: string
        readonly value: string
        readonly parentDirectoryId: backend.DirectoryId | null
      }
      const path = remoteBackendPaths.CREATE_SECRET_PATH
      const body: Body = { parentDirectoryId: this.value.id, name: title, value }
      const response = await this.httpPost<backend.SecretId>(path, body)
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'createSecretBackendError', body.name)
      } else {
        const id = await response.json()
        return result.withValue(object.merge(result.value, { id }))
      }
    })
    return result
  }
}

/** A smart wrapper around a {@link backend.ProjectAsset}. */
class SmartProject extends SmartAsset<backend.ProjectAsset> implements backend.SmartProject {
  /** Set a project to an open state. */
  async open(body?: backend.OpenProjectRequestBody): Promise<void> {
    const path = remoteBackendPaths.openProjectPath(this.value.id)
    const response = await this.httpPost(path, body ?? { forceCreate: false, executeAsync: false })
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'openProjectBackendError', this.value.title)
    } else {
      return
    }
  }

  /** Return project details. */
  async getDetails(): Promise<backend.Project> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly organizationId: string
      readonly projectId: backend.ProjectId
      readonly name: string
      readonly state: backend.ProjectStateType
      readonly packageName: string
      readonly address?: backend.Address
      // eslint-disable-next-line @typescript-eslint/naming-convention
      readonly ide_version: backend.VersionNumber | null
      // eslint-disable-next-line @typescript-eslint/naming-convention
      readonly engine_version: backend.VersionNumber | null
    }
    const path = remoteBackendPaths.getProjectDetailsPath(this.value.id)
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getProjectDetailsBackendError', this.value.title)
    } else {
      const project = await response.json()
      const ideVersion =
        project.ide_version ?? (await getDefaultVersion(this.client, backend.VersionType.ide))
      return {
        ...project,
        ideVersion,
        engineVersion: project.engine_version,
        jsonAddress: project.address != null ? backend.Address(`${project.address}json`) : null,
        binaryAddress: project.address != null ? backend.Address(`${project.address}binary`) : null,
      }
    }
  }

  /** Return project memory, processor and storage usage. */
  async getResourceUsage(): Promise<backend.ResourceUsage> {
    const path = remoteBackendPaths.checkResourcesPath(this.value.id)
    const response = await this.httpGet<backend.ResourceUsage>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'checkResourcesBackendError', this.value.title)
    } else {
      return await response.json()
    }
  }

  /** Change the name, description, AMI, or parent of a project. */
  override async update(body: backend.UpdateAssetOrProjectRequestBody): Promise<this> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly organizationId: string
      readonly projectId: backend.ProjectId
      readonly name: string
      readonly ami: backend.Ami | null
      readonly ideVersion: backend.VersionNumber | null
      readonly engineVersion: backend.VersionNumber | null
    }
    const updated = await super.update(body)
    const path = remoteBackendPaths.projectUpdatePath(this.value.id)
    if (body.projectName == null && body.ami == null && body.ideVersion == null) {
      return updated
    } else {
      const response = await this.httpPut<ResponseBody>(path, {
        projectName: body.projectName ?? null,
        ami: body.ami ?? null,
        ideVersion: body.ideVersion ?? null,
      })
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'updateProjectBackendError', this.value.title)
      } else {
        const responseBody = await response.json()
        return this.withValue(object.merge(updated.value, { title: responseBody.name }))
      }
    }
  }

  /** Fetch the content of the `Main.enso` file of a project. */
  async getMainFile(version: string): Promise<string> {
    const path = remoteBackendPaths.getProjectContentPath(this.value.id, version)
    const response = await this.httpGet<string>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getFileContentsBackendError', this.value.title)
    } else {
      return await response.text()
    }
  }

  /** Close a project. */
  async close(): Promise<void> {
    const path = remoteBackendPaths.closeProjectPath(this.value.id)
    const response = await this.httpPost(path, {})
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'closeProjectBackendError', this.value.title)
    } else {
      return
    }
  }

  /** Resolve only when the project is ready to be opened. */
  async waitUntilReady(abortController: AbortController = new AbortController()) {
    let project = await this.getDetails()
    if (!backend.IS_OPENING_OR_OPENED[project.state.type]) {
      await this.open()
    }
    let nextCheckTimestamp = 0
    while (!abortController.signal.aborted && project.state.type !== backend.ProjectState.opened) {
      await new Promise<void>(resolve => {
        const delayMs = nextCheckTimestamp - Number(new Date())
        setTimeout(resolve, Math.max(0, delayMs))
      })
      nextCheckTimestamp = Number(new Date()) + CHECK_STATUS_INTERVAL_MS
      project = await this.getDetails()
    }
    if (project.state.type === backend.ProjectState.opened) {
      return project
    } else {
      // The request was aborted.
      throw new Error('Could not wait until the project was ready: the request was aborted.')
    }
  }
}

/** A smart wrapper around a {@link backend.FileAsset}. */
class SmartFile extends SmartAsset<backend.FileAsset> implements backend.SmartFile {
  /** Change the name or description of a file. */
  override async update(body: backend.UpdateAssetOrFileRequestBody): Promise<this> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly path: string
      readonly id: backend.FileId
    }
    const updated = await super.update(body)
    if (body.file == null) {
      return updated
    } else {
      const paramsString = new URLSearchParams({
        // eslint-disable-next-line @typescript-eslint/naming-convention
        file_name: body.file.name,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        file_id: this.value.id,
      }).toString()
      const path = `${remoteBackendPaths.UPLOAD_FILE_PATH}?${paramsString}`
      const response = await this.httpPostBinary<ResponseBody>(path, body.file)
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'updateFileBackendError', this.value.title)
      } else {
        const responseBody = await response.json()
        return this.withValue(object.merge(updated.value, { id: responseBody.id }))
      }
    }
  }

  /** Return file details. */
  async getDetails(): Promise<backend.FileDetails> {
    const path = remoteBackendPaths.getFileDetailsPath(this.value.id)
    const response = await this.httpGet<backend.FileDetails>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getFileDetailsBackendError', this.value.title)
    } else {
      return await response.json()
    }
  }
}

/** A smart wrapper around a {@link backend.SecretAsset}. */
class SmartDataLink extends SmartAsset<backend.DataLinkAsset> implements backend.SmartDataLink {
  /** Return the value of this Data Link. */
  async getValue(): Promise<backend.Connector> {
    const path = remoteBackendPaths.getConnectorPath(this.value.id)
    const response = await this.httpGet<backend.Connector>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getConnectorBackendError', this.value.title)
    } else {
      return await response.json()
    }
  }

  /** Change the value or description of a secret environment variable. */
  override async update(body: backend.UpdateAssetOrDataLinkRequestBody): Promise<this> {
    const updated = await super.update(body)
    if (body.value == null) {
      return updated
    } else {
      const path = remoteBackendPaths.CREATE_CONNECTOR_PATH
      const response = await this.httpPost(path, {
        name: this.value.title,
        value: body.value,
        connectorId: this.value.id,
      })
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'updateConnectorBackendError', this.value.title)
      } else {
        return updated
      }
    }
  }
}

/** A smart wrapper around a {@link backend.SecretAsset}. */
class SmartSecret extends SmartAsset<backend.SecretAsset> implements backend.SmartSecret {
  /** Return the value of this secret environment variable. */
  async getValue(): Promise<backend.Secret> {
    const path = remoteBackendPaths.getSecretPath(this.value.id)
    const response = await this.httpGet<backend.Secret>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getSecretBackendError', this.value.title)
    } else {
      return await response.json()
    }
  }

  /** Change the value or description of a secret environment variable. */
  override async update(body: backend.UpdateAssetOrSecretRequestBody): Promise<this> {
    const updated = await super.update(body)
    if (body.value == null) {
      return updated
    } else {
      const path = remoteBackendPaths.updateSecretPath(this.value.id)
      const response = await this.httpPut(path, { value: body.value })
      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'updateSecretBackendError', this.value.title)
      } else {
        return updated
      }
    }
  }
}

// =====================
// === RemoteBackend ===
// =====================

/** A function that turns a text ID (and a list of replacements, if required) to
 * human-readable text. */
type GetText = ReturnType<typeof textProvider.useText>['getText']

/** Information for a cached default version. */
interface DefaultVersionInfo {
  readonly version: backend.VersionNumber
  readonly lastUpdatedEpochMs: number
}

/** Class for sending requests to the Cloud backend API endpoints. */
export default class RemoteBackend extends Backend {
  readonly type = backend.BackendType.remote

  /** Create a new instance of the {@link RemoteBackend} API client.
   * @throws An error if the `Authorization` header is not set on the given `client`. */
  constructor(
    private readonly client: HttpClient,
    private readonly logger: loggerProvider.Logger,
    private getText: GetText
  ) {
    super()
    // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
    // set.
    if (!new Headers(this.client.defaultHeaders).has('Authorization')) {
      const message = 'Authorization header not set.'
      this.logger.error(message)
      throw new Error(message)
    } else {
      if (detect.IS_DEV_MODE) {
        // @ts-expect-error This exists only for debugging purposes. It does not have types
        // because it MUST NOT be used in this codebase.
        window.remoteBackend = this
      }
      return
    }
  }

  /** Set `this.getText`. This function is exposed rather than the property itself to make it clear
   * that it is intended to be mutable. */
  setGetText(getText: GetText) {
    this.getText = getText
  }

  /** Set the username and parent organization of the current user. */
  override async createUser(body: backend.CreateUserRequestBody): Promise<backend.User> {
    const path = remoteBackendPaths.CREATE_USER_PATH
    const response = await this.post<backend.User>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createUserBackendError')
    } else {
      return await response.json()
    }
  }

  /** Return details for the current user.
   * @returns `null` if a non-successful status code (not 200-299) was received. */
  override async self(): Promise<SmartUser | null> {
    const path = remoteBackendPaths.USERS_ME_PATH
    const response = await this.get<backend.User>(path)
    if (!responseIsSuccessful(response)) {
      // This user has probably not finished registering.
      return null
    } else {
      const json = await response.json()
      return new SmartUser(this.client, this.logger, this.getText, json)
    }
  }

  /** Create a label used for categorizing assets.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createTag(body: backend.CreateTagRequestBody): Promise<backend.Label> {
    const path = remoteBackendPaths.CREATE_TAG_PATH
    const response = await this.post<backend.Label>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'createLabelBackendError', body.value)
    } else {
      return await response.json()
    }
  }

  /** Return all labels accessible by the user.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listTags(): Promise<backend.Label[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      readonly tags: backend.Label[]
    }
    const path = remoteBackendPaths.LIST_TAGS_PATH
    const response = await this.get<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'listLabelsBackendError')
    } else {
      return (await response.json()).tags
    }
  }

  /** Delete a label.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async deleteTag(tagId: backend.TagId, value: backend.LabelName): Promise<void> {
    const path = remoteBackendPaths.deleteTagPath(tagId)
    const response = await this.delete(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'deleteLabelBackendError', value)
    } else {
      return
    }
  }

  /** Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  async getProject(
    parentId: backend.DirectoryId,
    projectId: backend.ProjectId,
    title: string
  ): Promise<backend.SmartProject> {
    const self = await this.self()
    if (self == null) {
      return this.throw(null, 'getProjectDetailsBackendError', title)
    } else {
      const rootDirectory = self.rootDirectory()
      // The rest of the values will be incorrect. This is fine, because this directory
      // is only being used to fetch the child project.
      const directory = rootDirectory.withValue(object.merge(rootDirectory.value, { id: parentId }))
      const children = await directory.list({ filterBy: backend.FilterBy.active, labels: null })
      const project = children.find(child => child.value.id === projectId)
      if (project?.type !== backend.AssetType.project) {
        return this.throw(null, 'projectNotFoundBackendError', title)
      } else {
        return project
      }
    }
  }

  /** Prepare a project for execution.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  async openProject(
    projectId: backend.ProjectId,
    body: backend.OpenProjectRequestBody | null,
    title: string
  ): Promise<void> {
    const path = remoteBackendPaths.openProjectPath(projectId)
    const response = await this.post(path, body ?? { forceCreate: false, executeAsync: false })
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'openProjectBackendError', title)
    } else {
      return
    }
  }

  /** Upload a file.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  async uploadFile(params: UploadFileRequestParams, file: File): Promise<void> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      file_name: params.fileName,
      ...(params.fileId != null ? { file_id: params.fileId } : {}),
      ...(params.parentDirectoryId ? { parent_directory_id: params.parentDirectoryId } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_FILE_PATH}?${paramsString}`
    // This endpoint does not return a nullish value. However, to prevent this from being used
    // incorrectly, the return value is not exposed.
    const response = await this.postBinary<unknown>(path, file)
    if (!responseIsSuccessful(response)) {
      if (params.fileId != null) {
        return this.throw(response, 'uploadFileWithNameBackendError', file.name)
      } else {
        return this.throw(response, 'uploadFileBackendError')
      }
    }
  }

  /** Create a payment checkout session.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createCheckoutSession(plan: backend.Plan): Promise<backend.CheckoutSession> {
    const response = await this.post<backend.CheckoutSession>(
      remoteBackendPaths.CREATE_CHECKOUT_SESSION_PATH,
      { plan } satisfies backend.CreateCheckoutSessionRequestBody
    )
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createCheckoutSessionBackendError', plan)
    } else {
      return await response.json()
    }
  }

  /** Gets the status of a payment checkout session.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getCheckoutSession(
    sessionId: backend.CheckoutSessionId
  ): Promise<backend.CheckoutSessionStatus> {
    const path = remoteBackendPaths.getCheckoutSessionPath(sessionId)
    const response = await this.get<backend.CheckoutSessionStatus>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getCheckoutSessionBackendError', sessionId)
    } else {
      return await response.json()
    }
  }

  /** Log an error message and throw an {@link Error} with the specified message.
   * @throws {Error} Always. */
  private async throw<K extends Extract<text.TextId, `${string}BackendError`>>(
    response: Response | null,
    textId: K,
    ...replacements: text.Replacements[K]
  ): Promise<never> {
    const error =
      response == null
        ? { message: 'unknown error' }
        : // This is SAFE only when the response has been confirmed to have an erroring status code.
          // eslint-disable-next-line no-restricted-syntax
          ((await response.json()) as RemoteBackendError)
    const message = `${this.getText(textId, ...replacements)}: ${error.message}.`
    this.logger.error(message)
    throw new Error(message)
  }

  /** Send an HTTP GET request to the given path. */
  private get<T = void>(path: string) {
    return this.client.get<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`)
  }

  /** Send a JSON HTTP POST request to the given path. */
  private post<T = void>(path: string, payload: object) {
    return this.client.post<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a binary HTTP POST request to the given path. */
  private postBinary<T = void>(path: string, payload: Blob) {
    return this.client.postBinary<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send an HTTP DELETE request to the given path. */
  private delete<T = void>(path: string) {
    return this.client.delete<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`)
  }
}

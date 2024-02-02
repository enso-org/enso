/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as detect from 'enso-common/src/detect'

import type * as loggerProvider from '#/providers/LoggerProvider'

import Backend, * as backendModule from '#/services/Backend'
import * as remoteBackendPaths from '#/services/remoteBackendPaths'

import * as config from '#/utilities/config'
import * as dateTime from '#/utilities/dateTime'
import * as errorModule from '#/utilities/error'
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
/** HTTP status indicating that the server encountered a fatal exception. */
const STATUS_SERVER_ERROR = 500

/** The number of milliseconds in one day. */
const ONE_DAY_MS = 86_400_000

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
  materialize: () => Promise<T>
}

/** Overwrites `materialize` so that it does not. */
function overwriteMaterialize<T>(
  smartAsset: HasMaterialize<T>,
  materialize: () => Promise<T>
): () => Promise<T> {
  return () => {
    const promise = materialize()
    smartAsset.materialize = () => promise
    return promise
  }
}

// ===============================
// === waitUntilProjectIsReady ===
// ===============================

/** The interval between requests checking whether the IDE is ready. */
const CHECK_STATUS_INTERVAL_MS = 5000

/** Return a {@link Promise} that resolves only when a project is ready to open. */
export async function waitUntilProjectIsReady(
  backend: Backend,
  id: backendModule.ProjectId,
  title: string,
  abortController: AbortController = new AbortController()
) {
  if (!(backend instanceof RemoteBackend)) {
    // Ignored.
  } else {
    let project = await backend.getProjectDetails(id, title)
    if (!backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[project.state.type]) {
      await backend.openProject(id, null, title)
    }
    let nextCheckTimestamp = 0
    while (
      !abortController.signal.aborted &&
      project.state.type !== backendModule.ProjectState.opened
    ) {
      await new Promise<void>(resolve => {
        const delayMs = nextCheckTimestamp - Number(new Date())
        setTimeout(resolve, Math.max(0, delayMs))
      })
      nextCheckTimestamp = Number(new Date()) + CHECK_STATUS_INTERVAL_MS
      project = await backend.getProjectDetails(id, title)
    }
  }
}

// ====================
// === listVersions ===
// ====================

/** URL query string parameters for the "list versions" endpoint. */
export interface ListVersionsRequestParams {
  versionType: backendModule.VersionType
  default: boolean
}

/** Return a list of backend or IDE versions.
 * @throws An error if a non-successful status code (not 200-299) was received. */
async function listVersions(
  client: HttpClient,
  params: ListVersionsRequestParams
): Promise<backendModule.Version[]> {
  /** HTTP response body for this endpoint. */
  interface ResponseBody {
    versions: [backendModule.Version, ...backendModule.Version[]]
  }
  const paramsString = new URLSearchParams({
    // eslint-disable-next-line @typescript-eslint/naming-convention
    version_type: params.versionType,
    default: String(params.default),
  }).toString()
  const path = remoteBackendPaths.LIST_VERSIONS_PATH + '?' + paramsString
  const response = await client.get<ResponseBody>(path)
  if (!responseIsSuccessful(response)) {
    throw new Error(`Could not list versions of type '${params.versionType}'.`)
  } else {
    return (await response.json()).versions
  }
}

// =========================
// === getDefaultVersion ===
// =========================

const DEFAULT_VERSIONS: Partial<Record<backendModule.VersionType, DefaultVersionInfo>> = {}

/** Get the default version given the type of version (IDE or backend). */
async function getDefaultVersion(client: HttpClient, versionType: backendModule.VersionType) {
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
  fileId: backendModule.AssetId | null
  // Marked as optional in the data type, however it is required by the actual route handler.
  fileName: string
  parentDirectoryId: backendModule.DirectoryId | null
}

// =====================
// === Smart objects ===
// =====================

/** A wrapper around a subset of the API endpoints. */
class SmartObject<T> implements backendModule.SmartObject<T> {
  /** Create a {@link SmartObject}. */
  constructor(
    protected readonly client: HttpClient,
    protected readonly logger: loggerProvider.Logger,
    readonly value: T
  ) {}

  /** Return a copy of this object, but with a different value. */
  withValue(value: T): this {
    // This is SAFE (as long as no child classes override the constructor),
    // as it uses runtime reflection to get the constructor of the current object.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-call, no-restricted-syntax, @typescript-eslint/no-explicit-any
    return new (this.constructor as any)(this.client, this.logger, value)
  }

  /** Log an error message and throws an {@link Error} with the specified message.
   * @throws {Error} Always. */
  protected throw(message: string): never {
    this.logger.error(message)
    throw new Error(message)
  }

  /** Send an HTTP GET request to the given path. */
  protected httpGet<R = void>(path: string) {
    return this.client.get<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }

  /** Send a JSON HTTP POST request to the given path. */
  protected httpPost<R = void>(path: string, payload: object) {
    return this.client.post<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a binary HTTP POST request to the given path. */
  protected httpPostBinary<R = void>(path: string, payload: Blob) {
    return this.client.postBinary<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  protected httpPatch<R = void>(path: string, payload: object) {
    return this.client.patch<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PUT request to the given path. */
  protected httpPut<R = void>(path: string, payload: object) {
    return this.client.put<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send an HTTP DELETE request to the given path. */
  protected httpDelete<R = void>(path: string) {
    return this.client.delete<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }
}

/** A smart wrapper around a {@link backendModule.UserOrOrganization}. */
class SmartUser
  extends SmartObject<backendModule.UserOrOrganization>
  implements backendModule.SmartUser
{
  /** Change the username of the current user. */
  async update(body: backendModule.UpdateUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.UPDATE_CURRENT_USER_PATH
    const response = await this.httpPut(path, body)
    if (!responseIsSuccessful(response)) {
      if (body.username != null) {
        return this.throw('Could not change username.')
      } else {
        return this.throw('Could not update user.')
      }
    } else {
      return
    }
  }

  /** Delete the current user. */
  async delete(): Promise<void> {
    const response = await this.httpDelete(remoteBackendPaths.DELETE_USER_PATH)
    if (!responseIsSuccessful(response)) {
      return this.throw('Could not delete user.')
    } else {
      return
    }
  }

  /** Upload a new profile picture for the current user. */
  async uploadPicture(
    params: backendModule.UploadUserPictureRequestParams,
    file: Blob
  ): Promise<backendModule.UserOrOrganization> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_USER_PICTURE_PATH}?${paramsString}`
    const response = await this.httpPostBinary<backendModule.UserOrOrganization>(path, file)
    if (!responseIsSuccessful(response)) {
      return this.throw('Could not upload user profile picture.')
    } else {
      return await response.json()
    }
  }

  /** Get the root directory for this user. */
  rootDirectory(): backendModule.SmartDirectory {
    const rootDirectory = backendModule.createRootDirectoryAsset(this.value.rootDirectoryId)
    return new SmartDirectory(this.client, this.logger, rootDirectory)
  }
}

/** A smart wrapper around a {@link backendModule.AnyAsset}. */
class SmartAsset<T extends backendModule.AnyAsset = backendModule.AnyAsset>
  extends SmartObject<T>
  implements backendModule.SmartAsset
{
  /** The type of the wrapped value. */
  get type(): T['type'] {
    return this.value.type
  }

  /** If this is a placeholder asset, return its non-placeholder equivalent after creating it on
   * the backend. Otherwise, return `this`. */
  materialize(): Promise<this> {
    return Promise.resolve(this)
  }

  /** Change the parent directory of an asset. */
  async update(body: backendModule.UpdateAssetRequestBody): Promise<this> {
    const path = remoteBackendPaths.updateAssetPath(this.value.id)
    const response = await this.httpPatch(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not update '${this.value.title}'.`)
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

  /** Move an arbitrary asset to the trash. */
  async delete(): Promise<void> {
    const path = remoteBackendPaths.deleteAssetPath(this.value.id)
    const response = await this.httpDelete(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Unable to delete '${this.value.title}'.`)
    } else {
      return
    }
  }

  /** Restore an arbitrary asset from the trash. */
  async undoDelete(): Promise<void> {
    const path = remoteBackendPaths.UNDO_DELETE_ASSET_PATH
    const response = await this.httpPatch(path, { assetId: this.value.id })
    if (!responseIsSuccessful(response)) {
      return this.throw(`Unable to restore ${this.value.title} from Trash.`)
    } else {
      return
    }
  }

  /** Copy an arbitrary asset to another directory. */
  async copy(
    parentDirectoryId: backendModule.DirectoryId,
    parentDirectoryTitle: string
  ): Promise<backendModule.CopyAssetResponse> {
    const path = remoteBackendPaths.copyAssetPath(this.value.id)
    const response = await this.httpPost<backendModule.CopyAssetResponse>(path, {
      parentDirectoryId,
    })
    if (!responseIsSuccessful(response)) {
      return this.throw(`Unable to copy '${this.value.title}' to '${parentDirectoryTitle}'.`)
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
  async listVersions(): Promise<backendModule.AssetVersions> {
    const path = remoteBackendPaths.listAssetVersionsPath(this.value.id)
    const response = await this.httpGet<backendModule.AssetVersions>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not list versions for '${this.value.title}'.`)
    } else {
      return await response.json()
    }
  }

  /** Set permissions for a user. */
  async setPermissions(body: backendModule.CreatePermissionRequestBody): Promise<void> {
    const path = remoteBackendPaths.CREATE_PERMISSION_PATH
    const response = await this.httpPost<backendModule.UserOrOrganization>(path, {
      ...body,
      resourceId: this.value.id,
    })
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not set permissions.`)
    } else {
      return
    }
  }
}

/** Converts an {@link backendModule.AnyAsset} into its corresponding
 * {@link backendModule.AnySmartAsset}. */
function intoSmartAsset(client: HttpClient, logger: loggerProvider.Logger) {
  return (asset: backendModule.AnyAsset): backendModule.AnySmartAsset => {
    switch (asset.type) {
      case backendModule.AssetType.project: {
        return new SmartProject(client, logger, asset)
      }
      case backendModule.AssetType.file: {
        return new SmartFile(client, logger, asset)
      }
      case backendModule.AssetType.secret: {
        return new SmartSecret(client, logger, asset)
      }
      case backendModule.AssetType.directory: {
        return new SmartDirectory(client, logger, asset)
      }
      case backendModule.AssetType.specialLoading:
      case backendModule.AssetType.specialEmpty: {
        throw new Error(
          `'${asset.type}' is a special asset type that should never be returned by the backend.`
        )
      }
    }
  }
}

/** A smart wrapper around a {@link backendModule.DirectoryAsset}. */
class SmartDirectory
  extends SmartAsset<backendModule.DirectoryAsset>
  implements backendModule.SmartDirectory
{
  /** Return a list of assets in a directory. */
  async list(
    query: backendModule.ListDirectoryRequestParams
  ): Promise<backendModule.AnySmartAsset[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      assets: backendModule.AnyAsset[]
    }
    const paramsString = new URLSearchParams([
      ['parent_id', this.value.id],
      ...(query.filterBy != null ? [['filter_by', query.filterBy]] : []),
      ...(query.recentProjects ? [['recent_projects', String(true)]] : []),
      ...(query.labels != null ? query.labels.map(label => ['label', label]) : []),
    ]).toString()
    const path = remoteBackendPaths.LIST_DIRECTORY_PATH + '?' + paramsString
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      if (response.status === STATUS_SERVER_ERROR) {
        // The directory is probably empty.
        return []
      } else {
        return this.throw('Could not list root folder.')
      }
    } else {
      const assets = (await response.json()).assets
        .map(asset =>
          object.merge(asset, {
            // eslint-disable-next-line no-restricted-syntax
            type: asset.id.match(/^(.+?)-/)?.[1] as backendModule.AssetType,
          })
        )
        .map(asset =>
          object.merge(asset, {
            permissions: [...(asset.permissions ?? [])].sort(backendModule.compareUserPermissions),
          })
        )
      // FIXME [sb]: This is TEMPORARY, until support for connectors is merged in.
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
      return assets.map(intoSmartAsset(this.client, this.logger)).filter(value => value != null)
    }
  }

  /** Change the name, description or parent of a directory. */
  override async update(body: backendModule.UpdateAssetOrDirectoryRequestBody): Promise<this> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      id: backendModule.DirectoryId
      parentId: backendModule.DirectoryId
      title: string
    }
    const updateAssetRequest =
      body.description == null && body.parentDirectoryId == null
        ? null
        : super.update({
            description: body.description ?? null,
            parentDirectoryId: body.parentDirectoryId ?? null,
          })
    const path = remoteBackendPaths.updateDirectoryPath(this.value.id)
    const updateDirectoryRequest =
      body.title == null ? null : this.httpPut<ResponseBody>(path, { title: body.title })
    const [updateAssetResponse, updateDirectoryResponse] = await Promise.allSettled([
      updateAssetRequest,
      updateDirectoryRequest,
    ])
    if (
      updateAssetResponse.status === 'rejected' ||
      updateDirectoryResponse.status === 'rejected' ||
      (updateDirectoryResponse.value != null &&
        !responseIsSuccessful(updateDirectoryResponse.value))
    ) {
      return this.throw(`Could not update folder '${this.value.title}'.`)
    } else {
      let newValue = this.value
      if (updateDirectoryResponse.value != null) {
        const responseBody = await updateDirectoryResponse.value.json()
        newValue = object.merge(newValue, responseBody)
      }
      if (body.description != null || body.parentDirectoryId != null) {
        newValue = object.merge(newValue, {
          ...(body.description == null ? {} : { description: body.description }),
          ...(body.parentDirectoryId == null ? {} : { parentId: body.parentDirectoryId }),
        })
      }
      return newValue === this.value ? this : this.withValue(newValue)
    }
  }

  /** Create a {@link backendModule.SpecialLoadingAsset}. */
  createSpecialLoadingAsset(): backendModule.SmartSpecialLoadingAsset {
    return new SmartAsset<backendModule.SpecialLoadingAsset>(this.client, this.logger, {
      type: backendModule.AssetType.specialLoading,
      title: '',
      id: backendModule.LoadingAssetId(
        `${backendModule.AssetType.specialLoading}-${uniqueString.uniqueString()}`
      ),
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions: [],
      projectState: null,
      labels: [],
      description: null,
    })
  }

  /** Create a {@link backendModule.SpecialEmptyAsset}. */
  createSpecialEmptyAsset(): backendModule.SmartSpecialEmptyAsset {
    return new SmartAsset<backendModule.SpecialEmptyAsset>(this.client, this.logger, {
      type: backendModule.AssetType.specialEmpty,
      title: '',
      id: backendModule.EmptyAssetId(
        `${backendModule.AssetType.specialEmpty}-${uniqueString.uniqueString()}`
      ),
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
    permissions: backendModule.UserPermission[]
  ): backendModule.SmartDirectory {
    const result = new SmartDirectory(this.client, this.logger, {
      type: backendModule.AssetType.directory,
      id: backendModule.DirectoryId(
        `${backendModule.AssetType.directory}-${uniqueString.uniqueString()}`
      ),
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
        title: string
        parentId: backendModule.DirectoryId | null
      }
      /** HTTP response body for this endpoint. */
      interface ResponseBody {
        id: backendModule.DirectoryId
        parentId: backendModule.DirectoryId
        title: string
      }
      const path = remoteBackendPaths.CREATE_DIRECTORY_PATH
      const body: Body = { title, parentId: this.value.id }
      const response = await this.httpPost<ResponseBody>(path, body)
      if (!responseIsSuccessful(response)) {
        return this.throw(`Could not create folder with name '${body.title}'.`)
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
    permissions: backendModule.UserPermission[]
  ): backendModule.SmartProject {
    const result = new SmartProject(this.client, this.logger, {
      type: backendModule.AssetType.project,
      id: backendModule.ProjectId(
        `${backendModule.AssetType.project}-${uniqueString.uniqueString()}`
      ),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: {
        type: backendModule.ProjectState.placeholder,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        volume_id: '',
      },
      labels: [],
      description: null,
    })
    /** A project returned by the endpoints. */
    interface CreatedProject {
      organizationId: string
      projectId: backendModule.ProjectId
      name: string
      state: backendModule.ProjectStateType
      packageName: string
    }
    if (fileOrTemplateName instanceof File) {
      result.materialize = overwriteMaterialize(result, async () => {
        /** HTTP response body for this endpoint. */
        interface ResponseBody {
          path: string
          id: backendModule.FileId
          project: CreatedProject | null
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
          let suffix = '.'
          try {
            const error = errorModule.tryGetError<unknown>(await response.json())
            if (error != null) {
              suffix = `: ${error}`
            }
          } catch {
            // Ignored.
          }
          return this.throw(`Could not upload project${suffix}`)
        } else {
          const responseBody = await response.json()
          if (responseBody.project == null) {
            return this.throw('Uploaded project but did not receive project details.')
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
          projectName: string
          projectTemplateName: string | null
          parentDirectoryId: backendModule.DirectoryId | null
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
          return this.throw(`Could not create project with name '${body.projectName}'.`)
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
    permissions: backendModule.UserPermission[]
  ): backendModule.SmartFile {
    const result = new SmartFile(this.client, this.logger, {
      type: backendModule.AssetType.file,
      id: backendModule.FileId(`${backendModule.AssetType.file}-${uniqueString.uniqueString()}`),
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
        path: string
        id: backendModule.FileId
        project: NonNullable<unknown> | null
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
        let suffix = '.'
        try {
          const error = errorModule.tryGetError<unknown>(await response.json())
          if (error != null) {
            suffix = `: ${error}`
          }
        } catch {
          // Ignored.
        }
        return this.throw(`Could not upload file${suffix}`)
      } else {
        const responseBody = await response.json()
        return result.withValue(object.merge(result.value, { id: responseBody.id }))
      }
    })
    return result
  }

  /** Create a {@link SmartSecret} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderSecret(
    title: string,
    value: string,
    permissions: backendModule.UserPermission[]
  ): backendModule.SmartSecret {
    const result = new SmartSecret(this.client, this.logger, {
      type: backendModule.AssetType.secret,
      id: backendModule.SecretId(
        `${backendModule.AssetType.secret}-${uniqueString.uniqueString()}`
      ),
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
        name: string
        value: string
        parentDirectoryId: backendModule.DirectoryId | null
      }
      const path = remoteBackendPaths.CREATE_SECRET_PATH
      const body: Body = { parentDirectoryId: this.value.id, name: title, value }
      const response = await this.httpPost<backendModule.SecretId>(path, body)
      if (!responseIsSuccessful(response)) {
        return this.throw(`Could not create secret with name '${body.name}'.`)
      } else {
        const id = await response.json()
        return result.withValue(object.merge(result.value, { id }))
      }
    })
    return result
  }
}

/** A smart wrapper around a {@link backendModule.ProjectAsset}. */
class SmartProject
  extends SmartAsset<backendModule.ProjectAsset>
  implements backendModule.SmartProject
{
  /** Set a project to an open state. */
  async open(body?: backendModule.OpenProjectRequestBody): Promise<void> {
    const path = remoteBackendPaths.openProjectPath(this.value.id)
    const response = await this.httpPost(path, body ?? { forceCreate: false, executeAsync: false })
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not open project '${this.value.title}'.`)
    } else {
      return
    }
  }

  /** Return project details. */
  async getDetails(): Promise<backendModule.Project> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      organizationId: string
      projectId: backendModule.ProjectId
      name: string
      state: backendModule.ProjectStateType
      packageName: string
      address?: backendModule.Address
      // eslint-disable-next-line @typescript-eslint/naming-convention
      ide_version: backendModule.VersionNumber | null
      // eslint-disable-next-line @typescript-eslint/naming-convention
      engine_version: backendModule.VersionNumber | null
    }
    const path = remoteBackendPaths.getProjectDetailsPath(this.value.id)
    const response = await this.httpGet<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not get details of project '${this.value.title}'.`)
    } else {
      const project = await response.json()
      const ideVersion =
        project.ide_version ?? (await getDefaultVersion(this.client, backendModule.VersionType.ide))
      return {
        ...project,
        ideVersion,
        engineVersion: project.engine_version,
        jsonAddress:
          project.address != null ? backendModule.Address(`${project.address}json`) : null,
        binaryAddress:
          project.address != null ? backendModule.Address(`${project.address}binary`) : null,
      }
    }
  }

  /** Return project memory, processor and storage usage. */
  async getResourceUsage(): Promise<backendModule.ResourceUsage> {
    const path = remoteBackendPaths.checkResourcesPath(this.value.id)
    const response = await this.httpGet<backendModule.ResourceUsage>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not get resource usage for project '${this.value.title}'.`)
    } else {
      return await response.json()
    }
  }

  /** Change the name, description, AMI, or parent of a project. */
  override async update(body: backendModule.UpdateAssetOrProjectRequestBody): Promise<this> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      organizationId: string
      projectId: backendModule.ProjectId
      name: string
      ami: backendModule.Ami | null
      ideVersion: backendModule.VersionNumber | null
      engineVersion: backendModule.VersionNumber | null
    }
    const updateAssetRequest =
      body.description == null && body.parentDirectoryId == null
        ? null
        : super.update({
            description: body.description ?? null,
            parentDirectoryId: body.parentDirectoryId ?? null,
          })
    const path = remoteBackendPaths.projectUpdatePath(this.value.id)
    const updateProjectRequest =
      body.projectName == null && body.ami == null && body.ideVersion == null
        ? null
        : this.httpPut<ResponseBody>(path, {
            projectName: body.projectName ?? null,
            ami: body.ami ?? null,
            ideVersion: body.ideVersion ?? null,
          })
    const [updateAssetResponse, updateProjectResponse] = await Promise.allSettled([
      updateAssetRequest,
      updateProjectRequest,
    ])
    if (
      updateAssetResponse.status === 'rejected' ||
      updateProjectResponse.status === 'rejected' ||
      (updateProjectResponse.value != null && !responseIsSuccessful(updateProjectResponse.value))
    ) {
      return this.throw(`Could not update folder '${this.value.title}'.`)
    } else {
      let newValue = this.value
      if (updateProjectResponse.value != null) {
        const responseBody = await updateProjectResponse.value.json()
        if (responseBody.name !== newValue.title) {
          newValue = object.merge(newValue, { title: responseBody.name })
        }
      }
      if (body.description != null || body.parentDirectoryId != null) {
        newValue = object.merge(newValue, {
          ...(body.description == null ? {} : { description: body.description }),
          ...(body.parentDirectoryId == null ? {} : { parentId: body.parentDirectoryId }),
        })
      }
      return newValue === this.value ? this : this.withValue(newValue)
    }
  }

  /** Close a project. */
  async close(): Promise<void> {
    const path = remoteBackendPaths.closeProjectPath(this.value.id)
    const response = await this.httpPost(path, {})
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not close project '${this.value.title}'.`)
    } else {
      return
    }
  }
}

/** A smart wrapper around a {@link backendModule.FileAsset}. */
class SmartFile extends SmartAsset<backendModule.FileAsset> implements backendModule.SmartFile {
  /** Change the name or description of a file. */
  override async update(body: backendModule.UpdateAssetOrFileRequestBody): Promise<this> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      path: string
      id: backendModule.FileId
    }
    const updateAssetRequest =
      body.description == null && body.parentDirectoryId == null
        ? null
        : super.update({
            description: body.description ?? null,
            parentDirectoryId: body.parentDirectoryId ?? null,
          })
    const paramsString =
      body.file == null
        ? ''
        : // eslint-disable-next-line @typescript-eslint/naming-convention
          new URLSearchParams({ file_name: body.file.name, file_id: this.value.id }).toString()
    const path = `${remoteBackendPaths.UPLOAD_FILE_PATH}?${paramsString}`
    const updateFileRequest =
      body.file == null ? null : this.httpPostBinary<ResponseBody>(path, body.file)
    const [updateAssetResponse, updateFileResponse] = await Promise.allSettled([
      updateAssetRequest,
      updateFileRequest,
    ])
    if (
      updateAssetResponse.status === 'rejected' ||
      updateFileResponse.status === 'rejected' ||
      (updateFileResponse.value != null && !responseIsSuccessful(updateFileResponse.value))
    ) {
      return this.throw(`Could not update folder '${this.value.title}'.`)
    } else {
      return updateAssetResponse.value == null ? this : updateAssetResponse.value
    }
  }

  /** Return file details. */
  async getDetails(): Promise<backendModule.FileDetails> {
    const path = remoteBackendPaths.getFileDetailsPath(this.value.id)
    const response = await this.httpGet<backendModule.FileDetails>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not get details of project '${this.value.title}'.`)
    } else {
      return await response.json()
    }
  }
}

/** A smart wrapper around a {@link backendModule.SecretAsset}. */
class SmartSecret
  extends SmartAsset<backendModule.SecretAsset>
  implements backendModule.SmartSecret
{
  /** Return a secret environment variable. */
  async getValue(): Promise<backendModule.Secret> {
    const path = remoteBackendPaths.getSecretPath(this.value.id)
    const response = await this.httpGet<backendModule.Secret>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not get secret '${this.value.title}'.`)
    } else {
      return await response.json()
    }
  }

  /** Change the value or description of a secret environment variable. */
  override async update(body: backendModule.UpdateAssetOrSecretRequestBody): Promise<this> {
    const updateAssetRequest =
      body.description == null && body.parentDirectoryId == null
        ? null
        : super.update({
            description: body.description ?? null,
            parentDirectoryId: body.parentDirectoryId ?? null,
          })
    const path = remoteBackendPaths.updateSecretPath(this.value.id)
    const updateSecretRequest =
      body.value == null ? null : this.httpPut(path, { value: body.value })
    const [updateAssetResponse, updateSecretResponse] = await Promise.allSettled([
      updateAssetRequest,
      updateSecretRequest,
    ])
    if (
      updateAssetResponse.status === 'rejected' ||
      updateSecretResponse.status === 'rejected' ||
      (updateSecretResponse.value != null && !responseIsSuccessful(updateSecretResponse.value))
    ) {
      return this.throw(`Could not update secret '${this.value.title}'.`)
    } else {
      return body.description == null && body.parentDirectoryId == null
        ? this
        : this.withValue(
            object.merge(this.value, {
              ...(body.description == null ? {} : { description: body.description }),
              ...(body.parentDirectoryId == null ? {} : { parentId: body.parentDirectoryId }),
            })
          )
    }
  }
}

// =====================
// === RemoteBackend ===
// =====================

/** Information for a cached default version. */
interface DefaultVersionInfo {
  version: backendModule.VersionNumber
  lastUpdatedEpochMs: number
}

/** Class for sending requests to the Cloud backend API endpoints. */
export default class RemoteBackend extends Backend {
  readonly type = backendModule.BackendType.remote

  /** Create a new instance of the {@link RemoteBackend} API client.
   * @throws An error if the `Authorization` header is not set on the given `client`. */
  constructor(
    private readonly client: HttpClient,
    private readonly logger: loggerProvider.Logger
  ) {
    super()
    // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
    // set.
    if (!new Headers(this.client.defaultHeaders).has('Authorization')) {
      return this.throw('Authorization header not set.')
    } else {
      if (detect.IS_DEV_MODE) {
        // @ts-expect-error This exists only for debugging purposes. It does not have types
        // because it MUST NOT be used in this codebase.
        window.remoteBackend = this
      }
      return
    }
  }

  /** Return a list of all users in the same organization. */
  override async listUsers(): Promise<backendModule.SimpleUser[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      users: backendModule.SimpleUser[]
    }
    const path = remoteBackendPaths.LIST_USERS_PATH
    const response = await this.get<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not list users in the organization.`)
    } else {
      return (await response.json()).users
    }
  }

  /** Set the username and parent organization of the current user. */
  override async createUser(
    body: backendModule.CreateUserRequestBody
  ): Promise<backendModule.UserOrOrganization> {
    const path = remoteBackendPaths.CREATE_USER_PATH
    const response = await this.post<backendModule.UserOrOrganization>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw('Could not create user.')
    } else {
      return await response.json()
    }
  }

  /** Invite a new user to the organization by email. */
  override async inviteUser(body: backendModule.InviteUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.INVITE_USER_PATH
    const response = await this.post(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not invite user '${body.userEmail}'.`)
    } else {
      return
    }
  }

  /** Return organization info for the current user.
   * @returns `null` if a non-successful status code (not 200-299) was received. */
  override async self(): Promise<SmartUser | null> {
    const path = remoteBackendPaths.USERS_ME_PATH
    const response = await this.get<backendModule.UserOrOrganization>(path)
    if (!responseIsSuccessful(response)) {
      // This user has probably not finished registering.
      return null
    } else {
      const json = await response.json()
      return new SmartUser(this.client, this.logger, json)
    }
  }

  /** Create a label used for categorizing assets.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createTag(body: backendModule.CreateTagRequestBody): Promise<backendModule.Label> {
    const path = remoteBackendPaths.CREATE_TAG_PATH
    const response = await this.post<backendModule.Label>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not create label '${body.value}'.`)
    } else {
      return await response.json()
    }
  }

  /** Return all labels accessible by the user.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listTags(): Promise<backendModule.Label[]> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      tags: backendModule.Label[]
    }
    const path = remoteBackendPaths.LIST_TAGS_PATH
    const response = await this.get<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not list labels.`)
    } else {
      return (await response.json()).tags
    }
  }

  /** Set the full list of labels for a specific asset.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async associateTag(
    assetId: backendModule.AssetId,
    labels: backendModule.LabelName[],
    title: string | null
  ) {
    const path = remoteBackendPaths.associateTagPath(assetId)
    const response = await this.patch(path, { labels })
    if (!responseIsSuccessful(response)) {
      const name = title != null ? `'${title}'` : `with ID '${assetId}'`
      return this.throw(`Could not set labels for asset ${name}.`)
    } else {
      return
    }
  }

  /** Delete a label.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async deleteTag(
    tagId: backendModule.TagId,
    value: backendModule.LabelName
  ): Promise<void> {
    const path = remoteBackendPaths.deleteTagPath(tagId)
    const response = await this.delete(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not delete label '${value}'.`)
    } else {
      return
    }
  }

  /** Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  async getProjectDetails(
    projectId: backendModule.ProjectId,
    title: string | null
  ): Promise<backendModule.Project> {
    /** HTTP response body for this endpoint. */
    interface ResponseBody {
      organizationId: string
      projectId: backendModule.ProjectId
      name: string
      state: backendModule.ProjectStateType
      packageName: string
      address?: backendModule.Address
      // eslint-disable-next-line @typescript-eslint/naming-convention
      ide_version: backendModule.VersionNumber | null
      // eslint-disable-next-line @typescript-eslint/naming-convention
      engine_version: backendModule.VersionNumber | null
    }
    const path = remoteBackendPaths.getProjectDetailsPath(projectId)
    const response = await this.get<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not get details of project ${
          title != null ? `'${title}'` : `with ID '${projectId}'`
        }.`
      )
    } else {
      const project = await response.json()
      const ideVersion =
        project.ide_version ?? (await getDefaultVersion(this.client, backendModule.VersionType.ide))
      return {
        ...project,
        ideVersion,
        engineVersion: project.engine_version,
        jsonAddress:
          project.address != null ? backendModule.Address(`${project.address}json`) : null,
        binaryAddress:
          project.address != null ? backendModule.Address(`${project.address}binary`) : null,
      }
    }
  }

  /** Prepare a project for execution.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  async openProject(
    projectId: backendModule.ProjectId,
    body: backendModule.OpenProjectRequestBody | null,
    title: string | null
  ): Promise<void> {
    const path = remoteBackendPaths.openProjectPath(projectId)
    const response = await this.post(path, body ?? { forceCreate: false, executeAsync: false })
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}.`
      )
    } else {
      return
    }
  }

  /** Upload a file.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  async uploadFile(params: UploadFileRequestParams, file: Blob): Promise<void> {
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
      let suffix = '.'
      try {
        const error = errorModule.tryGetError(await response.json())
        if (error != null) {
          suffix = `: ${error}`
        }
      } catch {
        // Ignored.
      }
      if (params.fileId != null) {
        return this.throw(`Could not upload file with ID '${params.fileId}'${suffix}`)
      } else {
        return this.throw(`Could not upload file${suffix}`)
      }
    } else {
      return
    }
  }

  /** Log an error message and throws an {@link Error} with the specified message.
   * @throws {Error} Always. */
  private throw(message: string): never {
    this.logger.error(message)
    throw new Error(message)
  }

  /** Send an HTTP GET request to the given path. */
  private get<T = void>(path: string) {
    return this.client.get<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }

  /** Send a JSON HTTP POST request to the given path. */
  private post<T = void>(path: string, payload: object) {
    return this.client.post<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a binary HTTP POST request to the given path. */
  private postBinary<T = void>(path: string, payload: Blob) {
    return this.client.postBinary<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  private patch<T = void>(path: string, payload: object) {
    return this.client.patch<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PUT request to the given path. */
  private put<T = void>(path: string, payload: object) {
    return this.client.put<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send an HTTP DELETE request to the given path. */
  private delete<T = void>(path: string) {
    return this.client.delete<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }
}

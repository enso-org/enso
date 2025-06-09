import {
  AnyAsset,
  AssetId,
  AssetType,
  compareAssets,
  CreatedDirectory,
  CreateDirectoryRequestBody,
  DirectoryAsset,
  DirectoryId,
  extractTypeFromId,
  FileAsset,
  FileId,
  ParentsPath,
  Path,
  ProjectAsset,
  ProjectId,
  ProjectState,
  UpdateAssetRequestBody,
  VirtualParentsPath,
  type UUID,
} from 'enso-common/src/services/Backend'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { basenameAndExtension, getFileName, getFolderPath } from 'enso-common/src/utilities/file'
import { createReadStream, statSync } from 'node:fs'
import { mkdir, readdir, rename, stat } from 'node:fs/promises'
import * as http from 'node:http'
import * as https from 'node:https'
import * as path from 'node:path'
import { createGzip } from 'node:zlib'
import { tarFsPack, zipWriteStream } from './archive'
import { BUNDLED_PROJECT_SUFFIX } from './fileAssociations'
import { getMetadata, getProjectsDirectory, unpackBundle } from './projectManagement'
import { extractTypeAndPath, newDirectoryId } from './server'

// ==================
// === fileExists ===
// ==================

/** Return whether a file exists. */
async function fileExists(path: string) {
  try {
    await stat(path)
    return true
  } catch {
    return false
  }
}

// ==============
// === result ===
// ==============

function result<T extends { readonly type: 'success' | 'error' }>(value: T) {
  return value
}

function resultFromError(error: unknown) {
  return result({
    type: 'error',
    message:
      typeof error === 'object' && error != null && 'message' in error ?
        String(error.message)
      : String(error),
  })
}

// =====================
// === API Functions ===
// =====================

/** Whether a file exists. */
export async function apiFileExists({ fileId }: { readonly fileId: FileId }) {
  const filePath = extractTypeAndPath(fileId).path
  return await fileExists(filePath)
}

/** Get the project's metadata. */
export function apiGetProjectMetadata({ projectId }: { projectId: ProjectId }) {
  const projectPath = extractTypeAndPath(projectId).path
  return { ...getMetadata(projectPath) }
}

/** Return a stream with the content of the project's main file. */
export function apiGetProjectContent({ projectId }: { projectId: ProjectId }) {
  const filePath = path.join(extractTypeAndPath(projectId).path, 'src/Main.enso')
  return createReadStream(filePath)
}

/** Get details for an asset by its path. */
export function apiGetAssetDetailsByPath<Type extends AssetType>({
  type,
  path,
}: {
  type?: Type
  path: Path
}): AnyAsset<Type> | undefined {
  try {
    // @ts-expect-error This is UNSAFE if `Type` is specified explicitly.
    // If it is inferred, this means `type` is present and the constraint correctly falls back to
    // `AssetType`
    type ??= (() => {
      const assetStat = statSync(path)
      if (assetStat.isDirectory()) {
        const metadata = getMetadata(path)
        if (metadata) {
          return AssetType.project
        } else {
          return AssetType.directory
        }
      } else {
        return AssetType.file
      }
    })()
    const shared = {
      title: getFileName(path),
      modifiedAt: toRfc3339(new Date()),
      parentId: newDirectoryId(Path(getFolderPath(path))),
      extension: null,
      permissions: [],
      projectState: null,
      parentsPath: ParentsPath(''),
      virtualParentsPath: VirtualParentsPath(''),
    } satisfies Partial<DirectoryAsset>
    switch (type) {
      case AssetType.project: {
        const result: ProjectAsset = {
          ...shared,
          type: AssetType.project,
          id: ProjectId(`project-${encodeURIComponent(path)}`),
          // FIXME: Get correct state.
          projectState: { type: ProjectState.closed },
        }
        // This is SAFE because `type` has been narrowed in the `switch` above.
        return result as AnyAsset<Type>
      }
      case AssetType.file: {
        const result: FileAsset = {
          ...shared,
          type: AssetType.file,
          id: FileId(`file-${encodeURIComponent(path)}`),
          extension: basenameAndExtension(path).extension,
        }
        // This is SAFE because `type` has been narrowed in the `switch` above.
        return result as AnyAsset<Type>
      }
      case AssetType.directory: {
        const result: DirectoryAsset = {
          ...shared,
          type: AssetType.directory,
          id: newDirectoryId(path),
        }
        // This is SAFE because `type` has been narrowed in the `switch` above.
        return result as AnyAsset<Type>
      }
      default: {
        throw new Error(`Unknown asset type '${type}'`)
      }
    }
  } catch {
    return
  }
}

/** Get an asset's details by its id. */
export function apiGetAssetDetails({ assetId }: { readonly assetId: AssetId }) {
  const typeAndPath = extractTypeAndPath(assetId)
  return apiGetAssetDetailsByPath(typeAndPath)
}

/** Get the project's metadata. */
export async function apiGetProjectByUuid({
  uuid,
  directoryId,
}: {
  uuid: UUID
  directoryId: DirectoryId
}) {
  const directoryPath = extractTypeAndPath(directoryId).path
  for (const entryName of await readdir(directoryPath)) {
    const entryPath = Path(path.join(directoryPath, entryName))
    const metadata = getMetadata(entryPath)
    if (metadata?.id === uuid) {
      return apiGetAssetDetailsByPath({ type: AssetType.project, path: entryPath })
    }
  }
}

/** HTTP response handler for "download project from cloud" endpoint. */
export async function apiCloudDownloadProject(downloadUrl: string, projectId: ProjectId) {
  const response = await new Promise<http.IncomingMessage>((resolve) =>
    https.get(downloadUrl, resolve),
  )
  const projectsDirectory = getProjectsDirectory()
  const parentDirectory = path.join(projectsDirectory, `cloud-${projectId}`)
  const targetDirectory = path.join(parentDirectory, 'project_root')

  await mkdir(targetDirectory, { recursive: true })
  await unpackBundle(response, targetDirectory)
  return { targetDirectory, parentDirectory }
}

/** Create a directory. */
export async function apiCreateDirectory({
  parentId,
  title,
}: CreateDirectoryRequestBody): Promise<CreatedDirectory> {
  parentId ??= this.projectsRootDirectoryId
  const parentPath = extractTypeAndPath(parentId).path
  let i = 1
  while (true) {
    const candidateName = title ?? `New Folder ${i}`
    const candidatePath = Path(path.join(parentPath, candidateName))
    if (title != null || !(await fileExists(candidatePath))) {
      await mkdir(candidatePath)
      return {
        id: newDirectoryId(candidatePath),
        parentId,
        title: candidateName,
      }
    }
    i += 1
  }
}

/** List a directory. */
export async function apiListDirectory(params: {
  readonly directoryId?: DirectoryId | null | undefined
  readonly rootPath?: Path | null | undefined
  readonly projectsRootDirectory: Path
}): Promise<readonly AnyAsset[]> {
  const { directoryId, projectsRootDirectory } = params
  const directoryPath = directoryId ? extractTypeAndPath(directoryId).path : projectsRootDirectory
  const assets: AnyAsset[] = []
  for (const entryName of await readdir(directoryPath)) {
    const entryPath = Path(path.join(directoryPath, entryName))
    const asset = apiGetAssetDetailsByPath({ path: entryPath })
    if (asset == null) {
      throw new Error(`File not found at '${entryPath}'`)
    }
    assets.push(asset)
  }
  return assets.sort(compareAssets)
}

/** Create a directory. */
export async function apiUpdateAsset({
  id,
  parentDirectoryId,
  title,
}: UpdateAssetRequestBody & { readonly id: AssetId }) {
  const { path: from } = extractTypeAndPath(id)
  const newParentPath =
    parentDirectoryId != null ? extractTypeAndPath(parentDirectoryId).path : getFolderPath(from)
  const to = path.join(newParentPath, title ?? getFileName(from))
  if (await fileExists(to)) {
    return result({ type: 'error', message: `File '${to}' already exists` })
  }
  try {
    await rename(from, to)
    return result({ type: 'success', data: null })
  } catch (error) {
    return resultFromError(error)
  }
}

/** Create an archive stream with the given assets. */
export function apiArchiveStream(assets: readonly AssetId[]) {
  const archive = zipWriteStream()

  const addProject = async (id: ProjectId, rootPath?: string) => {
    const assetPath = extractTypeAndPath(id).path
    rootPath ??= getFolderPath(assetPath)
    const pathInArchive = `${path.relative(rootPath, assetPath)}${BUNDLED_PROJECT_SUFFIX}`
    if (!(await fileExists(assetPath))) {
      return result({ type: 'error', message: `Project '${id}' not found` })
    }
    await archive.addFile(tarFsPack(assetPath).pipe(createGzip()), { name: pathInArchive })
  }

  const addFile = async (id: FileId, rootPath?: string) => {
    const assetPath = extractTypeAndPath(id).path
    rootPath ??= getFolderPath(assetPath)
    const pathInArchive = path.relative(rootPath, assetPath)
    if (!(await fileExists(assetPath))) {
      return result({ type: 'error', message: `File '${id}' not found` })
    }
    await archive.addFile(createReadStream(assetPath), { name: pathInArchive })
  }

  const addFolder = async (id: DirectoryId, rootPath?: string) => {
    const assetPath = extractTypeAndPath(id).path
    rootPath ??= getFolderPath(assetPath)
    const pathInArchive = path.relative(rootPath, assetPath)
    if (!(await fileExists(assetPath))) {
      return result({ type: 'error', message: `Folder '${id}' not found` })
    }
    await archive.addFolder({ name: pathInArchive })
    const entries = await apiListDirectory({ directoryId: id })
    for (const entry of entries) {
      await addAsset(entry.id, rootPath)
    }
  }

  const addAsset = async (id: AssetId, rootPath?: string) => {
    const typeAndId = extractTypeFromId(id)
    switch (typeAndId.type) {
      case AssetType.project: {
        const error = await addProject(typeAndId.id, rootPath)
        if (error) {
          return error
        }
        break
      }
      case AssetType.file: {
        const error = await addFile(typeAndId.id, rootPath)
        if (error) {
          return error
        }
        break
      }
      case AssetType.directory: {
        const error = await addFolder(typeAndId.id, rootPath)
        if (error) {
          return error
        }
        break
      }
      // These asset types are not valid, however include them to force any newly added
      // asset types to be handled (by causing a non-exhaustiveness error).
      case AssetType.secret:
      case AssetType.datalink:
      case AssetType.specialUp: {
        return
      }
    }
  }

  const promise = (async () => {
    for (const id of assets) {
      const error = await addAsset(id)
      if (error) {
        return error
      }
    }
    archive.finalize()
  })()

  return { stream: archive.stream, promise } as const
}

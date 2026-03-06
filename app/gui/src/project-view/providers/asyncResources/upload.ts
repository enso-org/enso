import type { OpenedProjectsStore } from '$/providers/openedProjects'
import type { Initialized as InitializedProject } from '$/providers/openedProjects/projectStates'
import { backendMutationOptions, backendQueryOptions } from '@/composables/backend'
import { useProjectFiles } from '@/stores/projectFiles'
import { QueryClient, useMutation } from '@tanstack/vue-query'
import { type Asset, AssetType, DirectoryId } from 'enso-common/src/services/Backend'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { unsafeKeys } from 'enso-common/src/utilities/data/object'
import { Err, mapOk, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { getFolderPath, readUserSelectedFile } from 'enso-common/src/utilities/file'
import type { FetchPartialProgress } from './AsyncResource'
import type { ResourceContextSnapshot } from './context'

export type AnyUploadSource =
  | UploadDefinition
  | DragEvent
  | DataTransferItem
  | ClipboardItem
  | File
  | FileList
  | Array<AnyUploadSource>

export interface UploadDefinition {
  filename: string
  data: Promise<Blob>
}

export interface UploadProgress {
  resourceUrl: string
  uploadData: Promise<Blob>
  upload: Promise<Result>
}

/**
 * Normalize any potential `AnyUploadSource` value into a series of uploads to perform, represented by `UploadDefinition` structs.
 */
export function* normalizeUploadSources(dataSource: AnyUploadSource): Generator<UploadDefinition> {
  if (Array.isArray(dataSource) || dataSource instanceof FileList) {
    for (const item of dataSource) yield* normalizeUploadSources(item)
  } else if (dataSource instanceof DragEvent) {
    const items = dataSource.dataTransfer?.items
    if (items) yield* normalizeUploadSources([...items])
  } else if (dataSource instanceof ClipboardItem) {
    const supportedDataType = dataSource.types.find(isSupportedType)
    if (supportedDataType) {
      yield {
        filename: supportedResourceTypes[supportedDataType].defaultFileName,
        data: dataSource.getType(supportedDataType),
      } satisfies UploadDefinition
    }
    return
  } else if (dataSource instanceof DataTransferItem) {
    const file = dataSource.getAsFile()
    if (file) yield* normalizeUploadSources(file)
  } else if (dataSource instanceof File) {
    if (isSupportedType(dataSource.type)) {
      yield {
        filename: dataSource.name,
        data: Promise.resolve(dataSource),
      } satisfies UploadDefinition
    }
  } else {
    yield dataSource
  }
}

function isSupportedType(type: string): type is keyof typeof supportedResourceTypes {
  return Object.hasOwn(supportedResourceTypes, type)
}

/**
 * Types of resources that can be uploaded.
 */
const supportedResourceTypes = {
  // List taken from https://developer.mozilla.org/en-US/docs/Web/Media/Formats/Image_types
  'image/apng': { extensions: ['apng'], defaultFileName: 'image.apng' },
  'image/avif': { extensions: ['avif'], defaultFileName: 'image.avif' },
  'image/gif': { extensions: ['gif'], defaultFileName: 'image.gif' },
  'image/jpeg': {
    extensions: ['jpg', 'jpeg', 'jfif', 'pjpeg', 'pjp'],
    defaultFileName: 'image.jpg',
  },
  'image/png': { extensions: ['png'], defaultFileName: 'image.png' },
  'image/svg+xml': { extensions: ['svg'], defaultFileName: 'image.svg' },
  'image/webp': { extensions: ['webp'], defaultFileName: 'image.webp' },
} as const satisfies Record<string, { extensions: string[]; defaultFileName: string }>

/**
 * Initialize async resource uploading logic.
 *
 * Part of 'asyncResources' store.
 * @internal
 */
export function useResourceUpload(
  openedProjects: OpenedProjectsStore,
  backend: RemoteBackend,
  query: QueryClient,
) {
  const createImgDirMutation = useMutation(
    backendMutationOptions('createDirectory', backend),
    query,
  )
  const uploadImageMutation = useMutation(backendMutationOptions('uploadImage', backend), query)

  async function uploadResourceToProject(
    project: InitializedProject,
    upload: UploadDefinition,
  ): Promise<Result<UploadProgress>> {
    const api = useProjectFiles(project.store)

    const rootId = await api.projectRootId
    if (!rootId) return Err('Cannot upload image: unknown project file tree root')

    const UPLOAD_PATH_SEGMENTS = ['images']

    const dirPath = { rootId, segments: UPLOAD_PATH_SEGMENTS }
    const existsResult = await api.ensureDirExists(dirPath)
    if (!existsResult.ok) return existsResult
    const nameResult = await api.pickUniqueName(dirPath, upload.filename)
    if (!nameResult.ok) return nameResult
    const fullFilePath = { rootId, segments: [...UPLOAD_PATH_SEGMENTS, nameResult.value] }
    return Ok({
      uploadData: upload.data,
      resourceUrl: `/${fullFilePath.segments.map(encodeURI).join('/')}`,
      upload: upload.data.then((blob) => api.writeFileBinary(fullFilePath, blob)),
    })
  }

  async function uploadResourceToCloud(
    data: UploadDefinition,
    asset: Asset,
  ): Promise<Result<UploadProgress>> {
    const directory = getFolderPath(asset.ensoPath)
    try {
      const parentContents = await query.fetchQuery(
        backendQueryOptions('listDirectory', [{ parentId: asset.parentId }, ''], backend),
      )
      let imagesDir = parentContents.assets.find(
        (asset) => asset.type === AssetType.directory && asset.title === 'images',
      )?.id as DirectoryId | undefined
      if (imagesDir == null) {
        imagesDir = (
          await createImgDirMutation.mutateAsync([
            { title: 'images', parentId: asset.parentId },
            false,
          ])
        ).id
      }

      const contents = await data.data
      const uploadResult = await uploadImageMutation.mutateAsync([
        imagesDir,
        [{ data: contents, name: data.filename }],
      ])

      return Ok({
        uploadData: data.data,
        resourceUrl: encodeURI(`${directory}images/${uploadResult.files[0]?.title}`),
        upload: Promise.resolve(Ok()),
      })
    } catch (err) {
      return Err(err)
    }
  }

  async function uploadResource(
    data: UploadDefinition,
    context: ResourceContextSnapshot,
  ): Promise<Result<UploadProgress>> {
    const openedProject = context.project && openedProjects.get(context.project)
    if (openedProject?.nextTask?.process === 'opening') {
      await openedProjects.waitForProcess(openedProject)
    }
    const initialized =
      openedProject?.state.status === 'initialized' ? openedProject.state : undefined
    if (initialized) {
      return uploadResourceToProject(initialized, data)
    } else if (context.asset) {
      return uploadResourceToCloud(data, context.asset)
    } else {
      return Err('Cannot upload resource: no Project nor asset in the context.')
    }
  }

  return uploadResource
}

/**
 * Translate an upload progress to partial fetch progress representation.
 */
export function uploadAsFetchProgress(
  progress: UploadProgress,
): Promise<Result<FetchPartialProgress>> {
  return progress.uploadData.then((data) => {
    return Ok({
      dataUpdate: data,
      continue: progress.upload.then((r) => mapOk(r, () => data)),
    })
  })
}

/**
 * Show a "open file" dialog to the user, allowing selection of any valid resource type.
 */
export async function selectResourceFiles(): Promise<Result<FileList>> {
  const mimeTypes = unsafeKeys(supportedResourceTypes)
  const extensions = Object.values(supportedResourceTypes)
    .flatMap((ty) => ty.extensions)
    .map((e) => `.${e}` as const)

  try {
    return Ok(await readUserSelectedFile({ accept: [...mimeTypes, ...extensions] }))
  } catch (error) {
    return Err(error)
  }
}

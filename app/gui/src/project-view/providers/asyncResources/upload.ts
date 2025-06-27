import { OpenedProject, OpenedProjectsStore } from '$/providers/openedProjects'
import { useProjectFiles } from '@/stores/projectFiles'
import { Err, mapOk, Ok, Result } from '@/util/data/result'
import { FetchPartialProgress } from './AsyncResource'
import { CapturedResourceContext } from './context'

export type AnyUploadSource =
  | UploadDefinition
  | DragEvent
  | DataTransferItem
  | ClipboardItem
  | File
  | Array<AnyUploadSource>

export interface UploadDefinition {
  filename: string
  data: Promise<Blob>
}

export interface UploadProgress {
  unparsedResourceUrl: string
  uploadData: Promise<Blob>
  upload: Promise<Result>
}

/**
 * Normalize any potential `AnyUploadSource` value into a series of uploads to perform, represented by `UploadDefinition` structs.
 */
export function* normalizeUploadSources(
  dataSource: AnyUploadSource,
): Generator<Result<UploadDefinition>> {
  if (Array.isArray(dataSource)) {
    for (const arrayElement of dataSource) {
      return yield* normalizeUploadSources(arrayElement)
    }
  } else if (dataSource instanceof DragEvent) {
    const items = dataSource.dataTransfer?.items
    if (items) return yield* normalizeUploadSources([...items])
  } else if (dataSource instanceof ClipboardItem) {
    const supportedDataType = dataSource.types.find(isSupportedType)
    if (supportedDataType) {
      return {
        filename: supportedResourceTypes[supportedDataType].defaultFileName,
        data: dataSource.getType(supportedDataType),
      } satisfies UploadDefinition
    }
  } else if (dataSource instanceof DataTransferItem) {
    const file = dataSource.getAsFile()
    if (file) return yield* normalizeUploadSources(file)
  } else if (dataSource instanceof File) {
    if (!isSupportedType(dataSource.type)) {
      return Err('Unsupported resource type: ' + dataSource.type)
    }
    return {
      filename: dataSource.name,
      data: Promise.resolve(dataSource),
    } satisfies UploadDefinition
  } else {
    return Ok(dataSource)
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
export function initResourceUpload(openedProjects: OpenedProjectsStore) {
  async function uploadResourceToProject(
    project: OpenedProject,
    upload: UploadDefinition,
  ): Promise<Result<UploadProgress>> {
    const api = useProjectFiles(project.store)
    const rootId = await api.projectRootId
    if (!rootId) return Err('Cannot upload image: unknown project file tree root.')

    // TODO: we might want to specify where the upload should go.
    const UPLOAD_PATH_SEGMENTS = ['image']

    const dirPath = { rootId, segments: UPLOAD_PATH_SEGMENTS }
    const existsResult = await api.ensureDirExists(dirPath)
    if (!existsResult.ok) return existsResult
    const nameResult = await api.pickUniqueName(dirPath, upload.filename)
    if (!nameResult.ok) return nameResult
    const fullFilePath = { rootId, segments: [...UPLOAD_PATH_SEGMENTS, nameResult.value] }
    const unparsedResourceUrl = `/${fullFilePath.segments.map(encodeURI).join('/')}`
    return Ok({
      unparsedResourceUrl,
      uploadData: upload.data,
      upload: upload.data.then((blob) => api.writeFileBinary(fullFilePath, blob)),
    })
  }

  async function uploadResourceToCloud(_data: UploadDefinition): Promise<Result<UploadProgress>> {
    return Err('Uploading documentation resources to cloud is not yet supported.')
  }

  async function uploadResource(
    data: UploadDefinition,
    context: CapturedResourceContext,
  ): Promise<Result<UploadProgress>> {
    const openedProject = context.project && openedProjects.get(context.project)
    if (openedProject) {
      return uploadResourceToProject(openedProject, data)
    } else {
      return uploadResourceToCloud(data)
    }
  }

  return uploadResource
}

export function generateUploadingDefinition(
  progress: UploadProgress,
): Promise<Result<FetchPartialProgress>> {
  return progress.uploadData.then((data) => {
    return Ok({
      dataUpdate: data,
      continue: progress.upload.then((r) => mapOk(r, () => data)),
    })
  })
}

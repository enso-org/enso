import { fetcherUrlTransformer } from '@/components/MarkdownEditor/imageUrlTransformer'
import { Vec2 } from '@/util/data/vec2'
import type { ToValue } from '@/util/reactivity'
import { useToast } from '@/util/toast'
import { unsafeKeys } from 'enso-common/src/utilities/data/object'
import {
  readUserSelectedFile,
  type FileExtension,
  type MimeType,
} from 'enso-common/src/utilities/file'
import { computed, reactive, toValue } from 'vue'
import type { Path } from 'ydoc-shared/languageServerTypes'
import { Err, mapOk, Ok, Result, withContext } from 'ydoc-shared/util/data/result'
import type { Uuid } from 'ydoc-shared/yjsModel'

type UploadedImagePosition = { type: 'selection' } | { type: 'coords'; coords: Vec2 }

/**
 * A Project File management API for {@link useDocumentationImages} composable.
 */
interface ProjectFilesAPI {
  projectRootId: Promise<Uuid | undefined>
  readFileBinary(path: Path): Promise<Result<Blob>>
  writeFileBinary(path: Path, content: Blob): Promise<Result>
  pickUniqueName(path: Path, suggestedName: string): Promise<Result<string>>
  ensureDirExists(path: Path): Promise<Result<void>>
}

const supportedImageTypes: Record<MimeType, { extensions: string[] }> = {
  // List taken from https://developer.mozilla.org/en-US/docs/Web/Media/Formats/Image_types
  'image/apng': { extensions: ['apng'] },
  'image/avif': { extensions: ['avif'] },
  'image/gif': { extensions: ['gif'] },
  'image/jpeg': { extensions: ['jpg', 'jpeg', 'jfif', 'pjpeg', 'pjp'] },
  'image/png': { extensions: ['png'] },
  'image/svg+xml': { extensions: ['svg'] },
  'image/webp': { extensions: ['webp'] },
}

function pathUniqueId(path: Path) {
  return path.rootId + ':' + path.segments.join('/')
}

function pathDebugRepr(path: Path) {
  return pathUniqueId(path)
}

/** Supports loading and uploading project images. */
export function useDocumentationImages(
  markdownEditor: ToValue<
    | {
        putText: (text: string) => void
        putTextAtCoord: (text: string, coord: Vec2) => void
      }
    | undefined
  >,
  modulePath: ToValue<Path | undefined>,
  projectFiles: ProjectFilesAPI,
) {
  const uploadErrorToast = useToast.error()

  function urlToPath(url: string): Result<Path> | undefined {
    const modulePathValue = toValue(modulePath)
    if (!modulePathValue) {
      return Err('Current module path is unknown.')
    }
    const appliedUrl = new URL(url, `file:///${modulePathValue.segments.join('/')}`)
    if (appliedUrl.protocol === 'file:') {
      // The pathname starts with '/', so we remove "" segment.
      const segments = decodeURI(appliedUrl.pathname).split('/').slice(1)
      return Ok({ rootId: modulePathValue.rootId, segments })
    } else {
      // Not a relative URL, custom fetching not needed.
      return undefined
    }
  }

  const currentlyUploading = reactive(new Map<string, Promise<Blob>>())

  /** URL transformer that enables displaying images from the current project. */
  const transformImageUrl = fetcherUrlTransformer(
    async (url: string) => {
      const path = urlToPath(url)
      if (!path) return
      return withContext(
        () => `Locating documentation image (${url})`,
        () =>
          mapOk(path, (path) => {
            const id = pathUniqueId(path)
            return {
              location: path,
              uniqueId: id,
              uploading: computed(() => currentlyUploading.has(id)),
            }
          }),
      )
    },
    async (path) => {
      return withContext(
        () => `Loading documentation image (${pathDebugRepr(path)})`,
        async () => {
          const uploaded = await currentlyUploading.get(pathUniqueId(path))
          return uploaded ? Ok(uploaded) : projectFiles.readFileBinary(path)
        },
      )
    },
  )

  async function uploadImage(
    name: string,
    blobPromise: Promise<Blob>,
    position: UploadedImagePosition = { type: 'selection' },
  ) {
    try {
      const rootId = await projectFiles.projectRootId
      if (!rootId) {
        uploadErrorToast.show('Cannot upload image: unknown project file tree root.')
        return
      }
      const markdownEditorValue = toValue(markdownEditor)
      if (!markdownEditorValue) {
        console.error('Tried to upload image while markdown editor is still not loaded')
        return
      }
      const dirPath = { rootId, segments: ['images'] }
      await projectFiles.ensureDirExists(dirPath)
      const filename = await projectFiles.pickUniqueName(dirPath, name)
      if (!filename.ok) {
        uploadErrorToast.reportError(filename.error)
        return
      }
      const path: Path = { rootId, segments: ['images', filename.value] }
      const id = pathUniqueId(path)
      currentlyUploading.set(id, blobPromise)

      const insertedLink = `\n![Image](/images/${encodeURI(filename.value)})\n`
      switch (position.type) {
        case 'selection':
          markdownEditorValue.putText(insertedLink)
          break
        case 'coords':
          markdownEditorValue.putTextAtCoord(insertedLink, position.coords)
          break
      }
      try {
        const blob = await blobPromise
        const uploadResult = await projectFiles.writeFileBinary(path, blob)
        if (!uploadResult.ok)
          uploadErrorToast.reportError(uploadResult.error, 'Failed to upload image')
      } finally {
        currentlyUploading.delete(id)
      }
    } catch (err) {
      uploadErrorToast.show(`Failed to upload image: ${err}`)
    }
  }

  /** If the given drag event contains supported image(s), upload them and insert references into the editor. */
  async function tryUploadDroppedImage(event: DragEvent) {
    if (!event.dataTransfer?.items) return
    for (const item of event.dataTransfer.items) {
      if (item.kind !== 'file' || !Object.hasOwn(supportedImageTypes, item.type)) continue
      const file = item.getAsFile()
      if (!file) continue
      const clientPos = new Vec2(event.clientX, event.clientY)
      event.stopPropagation()
      event.preventDefault()
      await uploadImage(file.name, Promise.resolve(file), { type: 'coords', coords: clientPos })
    }
  }

  /** If the given clipboard content contains a supported image, upload it and insert a reference into the editor. */
  function tryUploadPastedImage(item: ClipboardItem): boolean {
    const imageType = item.types.find((type): type is MimeType => type in supportedImageTypes)
    if (imageType) {
      const ext = supportedImageTypes[imageType]?.extensions[0] ?? ''
      uploadImage(`image.${ext}`, item.getType(imageType))
      return true
    } else {
      return false
    }
  }

  async function selectFiles() {
    try {
      const mimeTypes = unsafeKeys(supportedImageTypes)
      const extensions = Object.values(supportedImageTypes).flatMap(({ extensions }) =>
        extensions.map<FileExtension>((e) => `.${e}`),
      )
      return await readUserSelectedFile({ accept: [...mimeTypes, ...extensions] })
    } catch (e) {
      console.error(e)
      return []
    }
  }

  async function tryUploadImageFile() {
    for (const file of await selectFiles()) {
      await uploadImage(file.name, Promise.resolve(file))
    }
  }

  return { transformImageUrl, tryUploadDroppedImage, tryUploadPastedImage, tryUploadImageFile }
}

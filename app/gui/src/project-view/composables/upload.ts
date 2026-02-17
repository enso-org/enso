import { useUploadsToCloudStore } from '$/providers/upload'
import { backendMutationOptions } from '@/composables/backend'
import type { ToValue } from '@/util/reactivity'
import { useMutation } from '@tanstack/vue-query'
import type { Backend, UploadFileRequestParams } from 'enso-common/src/services/Backend'
import { computed, toValue } from 'vue'

/**
 * Function for uploading files to Local Backend. It requires less hassle than multipart
 * upload to Cloud.
 */
function useUploadLocally(backend: ToValue<Backend | null>) {
  const uploadFileStart = useMutation(backendMutationOptions('uploadFileStart', backend))
  const uploadFileEnd = useMutation(backendMutationOptions('uploadFileEnd', backend))

  return async (file: File, params: UploadFileRequestParams) => {
    const data = await uploadFileStart.mutateAsync([params, file])
    if (!data) {
      return
    }
    const { uploadId, sourcePath } = data
    return await uploadFileEnd.mutateAsync([
      {
        uploadId,
        sourcePath,
        parts: [],
        assetId: params.fileId,
        ...params,
      },
    ])
  }
}

/** A function to upload a file to the backend. */
export function useUpload(backend: ToValue<Backend | null>) {
  const uploads = useUploadsToCloudStore()
  const uploadLocally = useUploadLocally(backend)
  return computed(() =>
    toValue(backend)?.type === 'local' ? uploadLocally : uploads.uploadFile.bind(uploads),
  )
}

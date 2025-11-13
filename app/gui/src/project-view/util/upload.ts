import { backendMutationOptions } from '@/composables/backend'
import type { ToValue } from '@/util/reactivity'
import { useMutation } from '@tanstack/vue-query'
import type { Backend, UploadFileRequestParams } from 'enso-common/src/services/Backend'

/**
 * Function for uploading files to Local Backend. It requires less hassle than multipart
 * upload to Cloud.
 */
export function useUploadLocally(backend: ToValue<Backend | null>) {
  const localUploadFileStart = useMutation(backendMutationOptions('uploadFileStart', backend))
  const uploadFileEnd = useMutation(backendMutationOptions('uploadFileEnd', backend))

  return async (file: File, params: UploadFileRequestParams) => {
    const data = await localUploadFileStart.mutateAsync([params, file])
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

/** @file A hook to return the default download directory. */
import { useDownloadDirectory as originalUseDownloadDirectory } from '#/layouts/Drive/persistentState'
import { Path } from '#/services/Backend'
import { useBackends } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'

/** The default download directory. */
export function useDefaultDownloadDirectory() {
  const { localBackend } = useBackends()
  const { data: defaultDownloadDirectory } = useSuspenseQuery({
    queryKey: ['downloadDirectory'],
    queryFn: async () => {
      if (localBackend) {
        const response = await fetch('/api/download-directory')
        return Path(await response.text())
      } else {
        return null
      }
    },
  })
  return defaultDownloadDirectory
}

/** The download directory. */
export function useDownloadDirectory() {
  const downloadDirectory = originalUseDownloadDirectory()
  const defaultDownloadDirectory = useDefaultDownloadDirectory()
  return downloadDirectory ?? defaultDownloadDirectory
}

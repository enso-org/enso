/** @file A hook to return the default download directory. */
import { useStore } from '#/hooks/storeHooks'
import { localRootDirectoryStore } from '#/layouts/Drive/persistentState'
import { useBackends } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'
import { Path } from 'enso-common/src/services/Backend'

/** The default download directory. */
export function useDefaultDownloadDirectory() {
  const { localBackend } = useBackends()
  const { data: defaultDownloadDirectory } = useSuspenseQuery({
    queryKey: ['downloadDirectoryPath'],
    queryFn: async () => {
      if (localBackend) {
        const response = await fetch('/api/download-directory-path')
        return Path(await response.text())
      } else {
        return null
      }
    },
    staleTime: Infinity,
  })
  return defaultDownloadDirectory
}

/** The download directory. */
export function useDownloadDirectory() {
  const downloadDirectory = useStore(localRootDirectoryStore, (store) => store.downloadDirectory)
  const defaultDownloadDirectory = useDefaultDownloadDirectory()
  return downloadDirectory ?? defaultDownloadDirectory
}

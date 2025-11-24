import { downloadCloudProject } from '@/assetManagement'
import { getUpToDateAccessToken } from '@/authentication'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { extractIdFromDirectoryId } from 'enso-common/src/services/RemoteBackend/ids'
import {
  getText as originalGetText,
  TEXTS,
  type Replacements,
  type TextId,
} from 'enso-common/src/text'
import path from 'node:path'
import { createBundle } from 'project-manager-shim'
import buildInfo from '../buildInfo'

/**
 * A function that gets localized text for a given key, with optional replacements.
 * @param key - The key of the text to get.
 * @param replacements - The replacements to insert into the text.
 * If the text contains placeholders like `$0`, `$1`, etc.,
 * they will be replaced with the corresponding replacement.
 */
export type GetText = <K extends TextId>(key: K, ...replacements: Replacements[K]) => string

const getText: GetText = (key, ...replacements) => {
  return originalGetText(TEXTS.english, key, ...replacements)
}

/** Create a remote backend */
export async function createRemoteBackend() {
  const accessToken = await getUpToDateAccessToken()
  if (!accessToken) {
    throw new Error('No access token found for remote backend.')
  }
  const sessionId = crypto.randomUUID()
  const httpClient = new HttpClient({
    'x-enso-ide-version': buildInfo.version,
    'x-enso-session-id': sessionId,
    /**
     * For compatibility with backend versioned endpoints. The new project logs endpoint
     * checks for date strings that are at least `2025-01-16`.
     */
    'x-enso-version': '2025-01-16',
  })
  httpClient.setSessionToken(accessToken)
  const downloader = () => {
    // TODO: implement downloading (low priority)
    throw new Error('Downloading arbitrary URLs is not yet implemented.')
  }
  return new RemoteBackend({
    getText,
    client: httpClient,
    downloader,
    downloadCloudProject: (params) => downloadCloudProject(params.downloadUrl, params.projectId),
    getProjectArchive: async (directoryId, fileName) => {
      const parentDir = extractIdFromDirectoryId(directoryId)
      const projectDir = path.join(parentDir, 'project_root')
      const projectBundle = await createBundle(projectDir)
      return new File([projectBundle], fileName)
    },
  })
}

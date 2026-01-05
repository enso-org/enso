import { downloadCloudProject } from '@/assetManagement'
import { getUpToDateAccessToken } from '@/authentication'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { extractIdFromDirectoryId } from 'enso-common/src/services/RemoteBackend/ids'
import { defaultGetText } from 'enso-common/src/text'
import path from 'node:path'
import { createBundle } from 'project-manager-shim'
import buildInfo from '../buildInfo'
import { loadGuiConfig } from './guiConfig'

/** Create a remote backend */
export async function createRemoteBackend() {
  const { API_URL } = await loadGuiConfig()
  if (API_URL == null) {
    throw new Error('API_URL is not set in the GUI config.')
  }
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
    throw new Error(
      'Cannot download files in headless mode. If you see this message, please report a bug, as it means this functionality is now required.',
    )
  }
  return new RemoteBackend({
    apiUrl: API_URL,
    getText: defaultGetText,
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

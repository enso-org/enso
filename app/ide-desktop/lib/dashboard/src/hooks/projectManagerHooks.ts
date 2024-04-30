/** @file Hooks for getting the Project Manager. */

import * as reactQuery from '@tanstack/react-query'

import ProjectManager, * as projectManagerModule from '#/services/ProjectManager'

import * as appBaseUrl from '#/utilities/appBaseUrl'

// =========================
// === useProjectManager ===
// =========================

/** Fetch the project manager. */
export function useProjectManager(projectManagerUrl: string | null) {
  const query = reactQuery.useQuery({
    queryKey: ['projectManager', projectManagerUrl],
    staleTime: Infinity,
    queryFn: async () => {
      if (projectManagerUrl == null) {
        return null
      } else {
        const response = await fetch(`${appBaseUrl.APP_BASE_URL}/api/root-directory`)
        const rootPath = await response.text()
        return new ProjectManager(projectManagerUrl, projectManagerModule.Path(rootPath))
      }
    },
  })
  return query
}

/** @file Hooks related to `react-router`. */
import { type NavigateOptions, useNavigate as useRouterNavigate } from 'react-router'

import type { AppFullPath, AppPath } from '#/appUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { unsafeMutable } from 'enso-common/src/utilities/data/object'

/** An object representing a path. */
interface PathObject {
  readonly pathname: `/${AppPath}`
  readonly search?: string
  readonly hash?: string
}

/** A type-safe function to navigate to a specific page. */
export function useNavigate(): (url: AppFullPath | PathObject, options?: NavigateOptions) => void {
  const { user } = useFullUserSession()
  const localBackend = useLocalBackend()
  const { localStorage } = useLocalStorage()
  const navigate = useRouterNavigate()
  return useEventCallback((url: AppFullPath | PathObject, options?: NavigateOptions) => {
    // Routes MUST be adjusted *before* navigating to avoid unnecessary (and expensive) rerenders.
    const effectiveUrl = typeof url === 'string' ? new URL(url) : unsafeMutable(url)
    if (effectiveUrl.pathname === '/' || effectiveUrl.pathname === '/drive') {
      const newCategory =
        localStorage.get('driveCategory') ??
        (() => {
          const shouldDefaultToCloud = user.isEnabled || localBackend == null
          return shouldDefaultToCloud ? 'cloud' : 'local'
        })()
      effectiveUrl.pathname = `/drive/${newCategory}` satisfies AppFullPath
    }
    navigate(effectiveUrl, options)
  })
}

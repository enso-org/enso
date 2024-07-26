/** @file Hooks related to `react-router`. */
import { type NavigateOptions, useNavigate as useRouterNavigate } from 'react-router'

import type { AppFullPath, AppPath } from '#/appUtils'

/** An object representing a path. */
interface PathObject {
  readonly pathname: `/${AppPath}`
  readonly search?: string
  readonly hash?: string
}

/** A type-safe function to navigate to a specific page. */
export function useNavigate(): (url: AppFullPath | PathObject, options?: NavigateOptions) => void {
  return useRouterNavigate()
}

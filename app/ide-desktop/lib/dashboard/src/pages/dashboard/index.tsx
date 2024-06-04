/**
 * @file Barrel import for Dashboard route.
 */
import defineRoute from '#/utilities/defineRoute'

import * as dashboard from './Dashboard'

/**
 * Dashboard route.
 */
export default defineRoute({
  path: '/',
  element: dashboard.Dashboard,
})

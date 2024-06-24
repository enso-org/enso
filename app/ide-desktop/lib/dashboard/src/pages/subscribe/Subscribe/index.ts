/**
 * @file
 *
 * Barrel file for Subscribe page.
 */
import defineRoute from '#/utilities/defineRoute'

import * as subscribe from './Subscribe'

export default defineRoute({
  path: '/subscribe',
  element: subscribe.Subscribe,
})

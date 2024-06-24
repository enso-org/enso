import defineRoute from '#/utilities/defineRoute'

import * as subscribeSuccess from './SubscribeSuccess'

export default defineRoute({
  path: '/subscribe/success',
  element: subscribeSuccess.SubscribeSuccess,
})

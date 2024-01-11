/** @file Re-exports of React Router hooks. */
import * as router from 'react-router-dom'

/** Returns the current location object, which represents the current URL in web browsers.
 *
 * Note: If you're using this it may mean you're doing some of your own "routing" in your app,
 * and we'd like to know what your use case is. We may be able to provide something higher-level
 * to better suit your needs.
 * @see {@link https://reactrouter.com/hooks/use-location}
 */
// This is a function, even though it does not look like one.
// eslint-disable-next-line no-restricted-syntax
export const useLocation = router.useLocation

/** Returns the context (if provided) for the child route at this level of the route hierarchy.
 * @see {@link https://reactrouter.com/hooks/use-outlet-context} */
export const useOutletContext = router.useOutletContext

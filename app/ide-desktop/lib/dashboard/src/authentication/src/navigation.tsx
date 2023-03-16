/** @file Helper decorator for defining virtual router definition with a react component. */

import * as router from "react-router-dom";

// ==================
// === withRouter ===
// ==================

/** Function that upgrades a React component to a React higher-order component with emulated normal
 * routing in the browser. */
// The parameter `Component` is a React component even though it does not contain JSX.
// `withRouter` is not a React component even though it contains JSX.
// eslint-disable-next-line @typescript-eslint/naming-convention, no-restricted-syntax
function withRouter<T>(Component: (props: T) => JSX.Element) {
  /** Adds window navigation props that emulate normal routing in the browser. */
  return (props: T) => {
    const location = router.useLocation();
    const navigate = router.useNavigate();
    const params = router.useParams();
    return <Component {...props} router={{ location, navigate, params }} />;
  };
}

export default withRouter;

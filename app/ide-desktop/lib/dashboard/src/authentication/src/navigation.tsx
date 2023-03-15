/** @file Helper decorator for defining virtual router definition with a react component. */

import * as router from "react-router-dom";

// ==================
// === withRouter ===
// ==================

/** Function that upgrades a React component to a React higher-order component with emulated normal
 * routing in the browser. */
// eslint-disable-next-line @typescript-eslint/naming-convention
function withRouter<T extends object>(Component: React.FC<T>) {
  /** Adds window navigation props to given component that emulate normal routing in the browser. */
  const componentWithRouterProp = (props: T) => {
    const location = router.useLocation();
    const navigate = router.useNavigate();
    const params = router.useParams();
    return <Component {...props} router={{ location, navigate, params }} />;
  };
  return componentWithRouterProp;
}

export default withRouter;

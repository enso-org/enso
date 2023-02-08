/* eslint-disable @typescript-eslint/naming-convention */
/** @file Helper decorator that allows for defining virtual router definition with a react component. */

import { useLocation, useNavigate, useParams } from 'react-router-dom'



// ==================
// === withRouter ===
// ==================

/** Entry point function definition. */
function withRouter (Component: any) {
  /** Adds window navigation props to given component that emulates normal routing in the browser. */
  function componentWithRouterProp (props: any) {
    const location = useLocation()
    const navigate = useNavigate()
    const params = useParams()
    return <Component {...props} router={{ location, navigate, params }} />
  }

  return componentWithRouterProp
}

export default withRouter

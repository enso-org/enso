/* eslint-disable @typescript-eslint/naming-convention */
/**
 * @file Helper decorator that allows for defining virtual router definition with a react component.
 */

import { FC } from 'react'
import { useLocation, useNavigate, useParams } from 'react-router-dom'



// ==================
// === withRouter ===
// ==================

/**
 * Function that upgrades a React component to a React higher-order component with emulated normal
 * routing in the browser.
 */
const withRouter = <T extends object>(Component: FC<T>) => {
  /**
   * Adds window navigation props to given component that emulates normal routing in the browser.
   */
  const componentWithRouterProp: FC<T> = (props) => {
    const location = useLocation()
    const navigate = useNavigate()
    const params = useParams()
    return <Component {...props} router={{ location, navigate, params }} />
  }

  return componentWithRouterProp
}

export default withRouter

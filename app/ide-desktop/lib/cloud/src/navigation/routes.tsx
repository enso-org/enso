/** @file Router definition for the App. */

import * as React from 'react'
import { Routes, Route } from 'react-router-dom'

import DashboardContainer from "../containers/dashboardContainer";
import LoginContainer from "../containers/loginContainer";



// =================
// === AppRouter ===
// =================

class AppRouter extends React.Component<any, any> {
  override render () {
    return (
      <Routes>
        <React.Fragment>
          <Route index element={<DashboardContainer />} />
          <Route path={"/login"} element={<LoginContainer />} />
        </React.Fragment>
      </Routes>
    )
  }
}

export default AppRouter

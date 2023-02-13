/** @file Main App module responsible for rendering virtual router. */

import * as React from 'react'
import { Routes, Route, BrowserRouter } from 'react-router-dom'
import toast from "react-hot-toast"

import { AuthProvider } from '../authentication';
import DashboardContainer from "./dashboard";
import LoginContainer from "./login";
import RegistrationContainer from "./registration";
import { Toaster } from 'react-hot-toast';



// ===========
// === App ===
// ===========

/**
 * Functional component called by the parent module, returning the root React component for this package.
 * 
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app.
 */
// eslint-disable-next-line @typescript-eslint/naming-convention
const App = () => {
  return (
    <AuthProvider>
      <Toaster position="top-center" reverseOrder={false} />
      <BrowserRouter>
        {/* FIXME [NP]: Remove this after testing */}
        <h1>foo</h1>
        <AppRouter />
      </BrowserRouter>
    </AuthProvider>
  );
}



// =================
// === AppRouter ===
// =================

/// Router definition for the app.
// FIXME [NP]: React components are expected to use PascalCase, but our linter is not configured to
//   allow that. Do we want to allow that, even if it would disable the lint for non-React code?
// eslint-disable-next-line @typescript-eslint/naming-convention
const AppRouter: React.FC<any> = () => {
  return (
    <Routes>
      <React.Fragment>
        {/* FIXME [NP]: Uncomment this after testing */}
        {/*
        <Route index element={<DashboardContainer />} />
        <Route path={"/login"} element={<LoginContainer />} />
        */}
        <Route index element={<LoginContainer />} />
        <Route path={"/login"} element={<LoginContainer />} />
        <Route path={"/registration"} element={<RegistrationContainer />} />
      </React.Fragment>
    </Routes>
  )
}

export default App;


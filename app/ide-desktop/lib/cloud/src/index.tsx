/** @file Index file dclarating main DOM structure for the app. */

import * as ReactDOM from 'react-dom/client'

import App from './components/app'
import { BrowserRouter } from 'react-router-dom'

// Return interface for getElementById is element or null.
// Since we are fetching root it is not possible to be null.
// Hence we can allow for ignoring no non null assertion
/* eslint-disable  @typescript-eslint/no-non-null-assertion */
ReactDOM.createRoot(document.getElementById('root')!).render(
    <BrowserRouter>
        <App />
    </BrowserRouter>
)

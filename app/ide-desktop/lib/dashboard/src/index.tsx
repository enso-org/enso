/** @file Index file declaring main DOM structure for the app. */

// eslint-disable-next-line
// @ts-ignore
import * as authentication from 'enso-studio-authentication'
import { AppProps } from './authentication/src/components/app'

const props: AppProps = {
    logger: console,
    // This package is a standalone React app (i.e., IDE deployed to the Cloud), so we're not
    // running on the desktop.
    runningOnDesktop: false,
    onAuthenticated: () => {}
}

// eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
authentication.run(props)

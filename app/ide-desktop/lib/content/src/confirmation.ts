// FIXME [NP]: https://github.com/enso-org/enso/pull/4041/files#r1091509997
//   - no docs here and in other places
//   - no sections
//   DO THIS FOR ALL

import { Auth } from "aws-amplify"
import { amplifyConfig } from "./amplify"

function setUrlOpener(config: any) {
    config.oauth.options.urlOpener = async (url: string, redirectSignIn: string) => {
        console.log('urlOpener', url, redirectSignIn)
        window.loginAPI.open(url)
    }
}

function getAuthConfig() {
    let config = {
        ...amplifyConfig
    }
    setUrlOpener(config)
    return config
}

async function getAccessToken() {
    let jwt
    try {
        let session = await Auth.currentSession();
        jwt = session.getAccessToken().getJwtToken();
    } catch (error) { }

    return jwt
}

// FIXME [NP]: make this an async function
function confirm(email: string, verificationCode: string) {
    Auth.confirmSignUp(email, verificationCode)
        .then(() => {
            // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
            //   Add a success notification to inform the user that registration has been confirmed.
            window.location.replace('/assets/login.html')
        })
        .catch(() => {
            // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
            //   Add an error notification to inform the user that registration has errored.
        })
}

// TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
//   Deduplicate the repeated logic to check for auth and redirect between here and in login.ts and
//   other auth-related files.
window.onload = async () => {
    console.log('CONFIRMATION window.onload', window.location.search)

    let authConfig = getAuthConfig()
    Auth.configure(authConfig)
    console.log('CONFIRMATION Auth configured')

    let accessToken = await getAccessToken();
    console.log('CONFIRMATION got access token', accessToken, window.location)

    if (accessToken) {
        console.log('CONFIRMATION authenticated!')
        window.loginAPI.authenticatedRedirect()
    }

    // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
    //   Currently these values are hardcoded but they should be getting pulled from the link that
    //   redirected the user to this page from the email that they were sent. This is not setup yet
    //   as the backend does not send the right templates yet. This is being fixed.
    const email = "nikita@frecency.com";
    const verificationCode = "327245";
    confirm(email, verificationCode)
}

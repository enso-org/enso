import {
    CognitoHostedUIIdentityProvider,
    FederatedSignInOptions,
    FederatedSignInOptionsCustom
} from "@aws-amplify/auth";
import { Auth } from "aws-amplify"
import { amplifyConfig } from "./amplify"

const googleButton = document.getElementById('google-signin')
const githubButton = document.getElementById('github-signin')
const emailButton = document.getElementById('email-signin')
const emailInput = document.getElementById('email') as HTMLInputElement
const passwordInput = document.getElementById('password') as HTMLInputElement

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

// TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
//   Deduplicate the logic between this function and the `authenticateWithEmail` function.
async function authenticateWithOauth(options: FederatedSignInOptions | FederatedSignInOptionsCustom) {
    let authConfig = getAuthConfig()
    console.log('Auth.configure', authConfig)
    Auth.configure(authConfig)

    let accessToken = await getAccessToken()
    console.log('Current session', accessToken)

    if (!accessToken) {
        console.log('Auth.federatedSignIn')
        try {
            await Auth.federatedSignIn(options);
            console.log("LOGIN Auth.federatedSignIn success")
            // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
            //   Add a success notification to inform the user that they are logged in.
        } catch {
            // TOOD [NP]: https://www.pivotaltracker.com/story/show/184314879
            //   Add a failure notification to inform the user that they are not logged in and why.
            return;
        }
    } else {
        console.log('authenticate authenticated!')
        window.loginAPI.authenticatedRedirect()
    }
}

async function authenticateWithEmail(e: MouseEvent, email: string, password: string) {
    e.preventDefault()

    let authConfig = getAuthConfig()
    console.log('Auth.configure', authConfig)
    Auth.configure(authConfig)

    let accessToken = await getAccessToken()
    console.log('Current session', accessToken)

    if (!accessToken) {
        console.log('Auth.federatedSignIn')
        try {
            await Auth.signIn(email, password);
            // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
            //   Add a success notification to inform the user that they are logged in.
        } catch {
            // TOOD [NP]: https://www.pivotaltracker.com/story/show/184314879
            //   Add a failure notification to inform the user that they are not logged in and why.
            //   For example: "Uncaught (in promise) NotAuthorizedException: Incorrect username or
            //   password." needs to be caught and displayed to the user.
            return;
        }
    }

    console.log('authenticate authenticated!')
    window.loginAPI.authenticatedRedirect()
}

window.onload = async () => {
    console.log('LOGIN window.onload', window.location.search)

    googleButton.addEventListener('click', () => authenticateWithOauth({ provider: CognitoHostedUIIdentityProvider.Google }))
    githubButton.addEventListener('click', () => authenticateWithOauth({ provider: 'GitHub' }))
    emailButton.addEventListener('click', (e) => authenticateWithEmail(e, emailInput.value, passwordInput.value))

    let authConfig = getAuthConfig()
    Auth.configure(authConfig)
    console.log('LOGIN Auth configured')

    let accessToken = await getAccessToken();
    console.log('LOGIN got access token', accessToken, window.location)

    if (accessToken) {
        console.log('LOGIN authenticated!')
        window.loginAPI.authenticatedRedirect()
    }
}

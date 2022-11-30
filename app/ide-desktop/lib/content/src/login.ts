import {
    CognitoHostedUIIdentityProvider,
    FederatedSignInOptions,
    FederatedSignInOptionsCustom
} from "@aws-amplify/auth";
import { Auth } from "aws-amplify"
import { amplifyConfig } from "./amplify"

const googleButton = document.getElementById('google-signin')
const githubButton = document.getElementById('github-signin')

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

async function authenticate(options: FederatedSignInOptions | FederatedSignInOptionsCustom) {
    let authConfig = getAuthConfig()
    console.log('Auth.configure', authConfig)
    Auth.configure(authConfig)

    let accessToken = await getAccessToken()
    console.log('Current session', accessToken)

    if (!accessToken) {
        console.log('Auth.federatedSignIn')
        await Auth.federatedSignIn(options);
    } else {
        console.log('authenticate authenticated!')
        window.loginAPI.authenticatedRedirect()
    }
}

window.onload = async () => {
    console.log('LOGIN window.onload', window.location.search)

    googleButton.addEventListener('click', () => authenticate({ provider: CognitoHostedUIIdentityProvider.Google }))
    githubButton.addEventListener('click', () => authenticate({ provider: 'GitHub' }))

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

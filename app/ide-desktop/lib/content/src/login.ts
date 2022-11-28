import { amplifyConfig } from "./amplify"
import { Auth } from "aws-amplify"

const googleButton = document.getElementById('google-signin')

function setUrlOpener(config) {
    config.oauth.options.urlOpener = async (url, redirectSignIn) => {
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

async function authenticate(provider) {
    let authConfig = getAuthConfig()
    console.log('Auth.configure', authConfig)
    Auth.configure(authConfig)

    let accessToken = await getAccessToken()
    console.log('Current session', accessToken)

    if (!accessToken) {
        console.log('Auth.federatedSignIn')
        await Auth.federatedSignIn({ provider: provider });
    } else {
        console.log('authenticate authenticated!')
        window.location.href('/')
    }
}

window.onload = async () => {
    googleButton.addEventListener('click', () => authenticate('google'))

    let accessToken = await getAccessToken();
    console.log('login page', accessToken, window.location)

    if (accessToken) {
        console.log('authenticated!')
        window.location.href('/')
    }
}

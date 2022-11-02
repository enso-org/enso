import { Auth } from "aws-amplify"
import { amplifyConfig } from "./amplify"

const emailInput = document.getElementById('email') as HTMLInputElement
const passwordInput = document.getElementById('password') as HTMLInputElement
const passwordConfirmationInput = document.getElementById('password-confirmation') as HTMLInputElement
const registerButton = document.getElementById('register') as HTMLButtonElement
if (emailInput === null) { throw new Error('emailInput is null') }
if (passwordInput === null) { throw new Error('passwordInput is null') }
if (passwordConfirmationInput === null) { throw new Error('passwordConfirmationInput is null') }
if (registerButton === null) { throw new Error('registerButton is null') }

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

async function register(e: MouseEvent, email: string, password: string, passwordConfirmation: string) {
    e.preventDefault()

    if (password !== passwordConfirmation) {
        return;
    }

    try {
        const params = { username: email, password: password, attributes: { email } };
        await Auth.signUp(params);
        // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
        //   Add a success notification to inform the user that registration was successful.
        // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
        //   Add a notification to inform the user to check their email for a confirmation link.
    } catch {
        // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
        //   Add a failure notification to inform the user that registration failed and why.
    }
}

// TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
//   Deduplicate the repeated logic to check for auth and redirect between here and in login.ts and
//   other auth-related files.
window.onload = async () => {
    console.log('REGISTRATION window.onload', window.location.search)

    registerButton.addEventListener('click', (e) => register(e, emailInput.value, passwordInput.value, passwordConfirmationInput.value))

    let authConfig = getAuthConfig()
    Auth.configure(authConfig)
    console.log('REGISTRATION Auth configured')

    let accessToken = await getAccessToken();
    console.log('REGISTRATION got access token', accessToken, window.location)

    if (accessToken) {
        console.log('REGISTRATION authenticated!')
        window.loginAPI.authenticatedRedirect()
    }
}

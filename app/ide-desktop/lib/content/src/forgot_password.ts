import { Auth } from "aws-amplify"
import { amplifyConfig } from "./amplify"

const sendLinkButton = document.getElementById('send-link') as HTMLButtonElement
const emailInput = document.getElementById('email') as HTMLInputElement
if (sendLinkButton === null) { throw new Error('sendLinkButton is null') }
if (emailInput === null) { throw new Error('emailInput is null') }

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

async function forgotPassword(e: MouseEvent, email: string) {
    e.preventDefault()

    try {
        await Auth.forgotPassword(email);
        // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
        //   Add a success notification to inform the user that password reset request was sent and
        //   that they should check their inbox for a link to confirm.
        window.location.replace('/assets/reset_password.html')
    } catch {
        // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
        //   Add a failure notification to inform the user that password reset failed and why.
    }
}

// TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
//   Deduplicate the repeated logic to check for auth and redirect between here and in login.ts and
//   other auth-related files.
window.onload = async () => {
    console.log('FORGOT PASSWORD window.onload', window.location.search)

    sendLinkButton.addEventListener('click', (e) => forgotPassword(e, emailInput.value))

    let authConfig = getAuthConfig()
    Auth.configure(authConfig)
    console.log('FORGOT PASSWORD Auth configured')

    let accessToken = await getAccessToken();
    console.log('FORGOT PASSWORD got access token', accessToken, window.location)

    if (accessToken) {
        console.log('FORGOT PASSWORD authenticated!')
        window.loginAPI.authenticatedRedirect()
    }
}

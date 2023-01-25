import { Auth } from "aws-amplify"
import { amplifyConfig } from "./amplify"

const emailInput = document.getElementById('email') as HTMLInputElement
const codeInput = document.getElementById('code') as HTMLInputElement
const newPasswordInput = document.getElementById('new-password') as HTMLInputElement
const submitButton = document.getElementById('submit') as HTMLButtonElement
if (emailInput === null) { throw new Error('emailInput is null') }
if (codeInput === null) { throw new Error('codeInput is null') }
if (newPasswordInput === null) { throw new Error('newPasswordInput is null') }
if (submitButton === null) { throw new Error('submitButton is null') }
// FIXME [NP]: https://github.com/enso-org/enso/pull/4041/files#r1092216658
//   1. Top level code - it sohuld go to a function, like main or whatever
//   2. Use == null instead of === null
//   3. You generate the HTML in typescript so you are sure the references are always created and after refactoring the code doesnt break at runtime.
// DO THIS FOR ALL

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
// FIXME [NP]: https://github.com/enso-org/enso/pull/4041/files#r1092218880
//   - also, run `npx tsc --noEmit` to check for type errors.
//  DO THIS FOR ALL

async function getAccessToken() {
    let jwt
    try {
        let session = await Auth.currentSession();
        jwt = session.getAccessToken().getJwtToken();
    } catch (error) { }

    return jwt
}

// FIXME [NP]: https://github.com/enso-org/enso/pull/4041/files#r1092219434
//   - var names `e` -> `event`, in other places as well pls
// DO THIS FOR ALL
async function resetPassword(e: MouseEvent, email: string, code: string, newPassword: string) {
    e.preventDefault()

    try {
        await Auth.forgotPasswordSubmit(email, code, newPassword);
        // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
        //   Add a success notification to inform the user that password reset was successful.
        window.location.replace('/assets/login.html')
    } catch {
        // TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
        //   Add an error notification to inform the user that password reset failed.
    }
}

// TODO [NP]: https://www.pivotaltracker.com/story/show/184314879
//   Deduplicate the repeated logic to check for auth and redirect between here and in login.ts and
//   other auth-related files.
window.onload = async () => {
    console.log('RESET PASSWORD window.onload', window.location.search)

    submitButton.addEventListener('click', (e) => resetPassword(e, emailInput.value, codeInput.value, newPasswordInput.value))

    let authConfig = getAuthConfig()
    Auth.configure(authConfig)
    console.log('RESET PASSWORD Auth configured')

    let accessToken = await getAccessToken();
    console.log('RESET PASSWORD got access token', accessToken, window.location)

    if (accessToken) {
        console.log('RESET PASSWORD authenticated!')
        window.loginAPI.authenticatedRedirect()
    }
}

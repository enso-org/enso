/** @file Available actions for the login page. */
import * as actions from '../actions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
import SetUsernamePageActions from './SetUsernamePageActions'

// ========================
// === LoginPageActions ===
// ========================

/** Available actions for the login page. */
export default class LoginPageActions extends BaseActions {
  /** Perform a successful login. */
  login(email = 'email@example.com', password = actions.VALID_PASSWORD) {
    return this.step('Login', () => this.loginInternal(email, password)).into(DrivePageActions)
  }

  /** Perform a login as a new user (a user that does not yet have a username). */
  loginAsNewUser(email = 'email@example.com', password = actions.VALID_PASSWORD) {
    return this.step('Login', () => this.loginInternal(email, password)).into(
      SetUsernamePageActions
    )
  }

  /** Perform a failing login. */
  loginThatShouldFail(email = 'email@example.com', password = actions.VALID_PASSWORD) {
    return this.step('Login', () => this.loginInternal(email, password))
  }

  /** Internal login logic shared between all public methods. */
  private async loginInternal(email: string, password: string) {
    await this.page.goto('/')
    await actions.locateEmailInput(this.page).fill(email)
    await actions.locatePasswordInput(this.page).fill(password)
    await actions.locateLoginButton(this.page).click()
    await actions.locateToastCloseButton(this.page).click()
  }
}

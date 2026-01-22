/** @file Common logic for the credential creation form. */

import { openInNewBrowserTab } from '#/utilities/window'
import type { CredentialConfig, SecretId } from 'enso-common/src/services/Backend'
import type { CredentialRecipe } from './types'

/**
 * Takes a function that creates new credentials and transforms it into a createCredentials callback that works with the recipe and handles the common logic of generating the nonce and opening the browser.
 */
export function makeCredentialCreationHandler(
  doCreate: (name: string, value: CredentialConfig) => Promise<SecretId>,
) {
  return async (recipe: CredentialRecipe) => {
    const nonce = crypto.randomUUID()
    const metadata: CredentialConfig = {
      nonce,
      input: recipe.input,
    }

    const secretId = await doCreate(recipe.name, metadata)
    const url = recipe.makeAuthUrl(secretId, nonce)
    openInNewBrowserTab(url)
  }
}

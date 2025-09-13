/** @file Types common to all credentials dialogs. */
import type { CredentialInput, SecretId } from 'enso-common/src/services/Backend'
import type { TextId } from 'enso-common/src/text'

/**
 * A 'recipe' for creating a credential of given type.
 *
 * It describes the user settings for the credentials and the algorithm for generating the URL to open in the browser.
 */
export interface CredentialRecipe {
  readonly name: string
  readonly input: CredentialInput
  readonly makeAuthUrl: (secretId: SecretId, nonce: string) => string
}

/**
 * The props for any credential form that is used when creating new credentials.
 */
export interface CredentialFormProps {
  readonly createCredentials: (recipe: CredentialRecipe) => Promise<void>
}

/** Information to describe a credential in the list of credentials. */
export interface CredentialInfo {
  readonly nameId: TextId & `${string}CredentialType`
  /** The type of the credential, sent to the backend. */
  readonly credentialType: string
  /** A SVG data url. */
  readonly icon: string | undefined
  readonly form: React.ComponentType<CredentialFormProps>
}

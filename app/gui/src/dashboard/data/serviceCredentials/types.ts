/** @file Types common to all credentials dialogs. */
import type { CredentialMetadata, SecretId } from 'enso-common/src/services/Backend'
import type { TextId } from 'enso-common/src/text'

/** Information to describe a credential in the list of credentials. */
export interface CredentialInfo {
  readonly nameId: TextId & `${string}CredentialType`
  /** The type of the credential, sent to the backend. */
  readonly credentialType: string
  /** A SVG data url. */
  readonly icon: string | undefined
  readonly makeAuthUrl: (id: SecretId, metadata: CredentialMetadata) => string,
  readonly form: React.ComponentType
}

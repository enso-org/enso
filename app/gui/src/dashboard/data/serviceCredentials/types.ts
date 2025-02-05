/** @file Types common to all credentials dialogs. */
import type { TextId } from 'enso-common/src/text'

/** Props for a credentials dialog. */
export interface CredentialsDialogProps {
  readonly value?: unknown
  readonly upsertCredential: (value: unknown) => Promise<void>
}

/** Information to describe a credential in the list of credentials. */
export interface CredentialInfo {
  readonly nameId: TextId & `${string}CredentialType`
  /** A SVG data url. */
  readonly icon: string | undefined
  readonly component: (props: CredentialsDialogProps) => JSX.Element
}

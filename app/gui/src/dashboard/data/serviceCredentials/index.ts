import { SnowflakeCredentialsDialog } from '#/data/serviceCredentials/SnowflakeCredentialsDialog'
import type { TextId } from 'enso-common/src/text'
import type { JSX } from 'react'

/** @file Barrel file for credentials forms. */
export * from './SnowflakeCredentialsDialog'

/** Information to describe a credential in the list of credentials. */
export interface CredentialInfo {
  readonly nameId: TextId & `${string}CredentialType`
  /** A SVG data url. */
  readonly icon: string | undefined
  readonly component: () => JSX.Element
}

export const CREDENTIAL_TYPES: readonly CredentialInfo[] = [
  {
    icon: undefined,
    nameId: 'snowflakeCredentialType',
    component: SnowflakeCredentialsDialog,
  },
]

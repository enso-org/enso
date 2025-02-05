/** @file Constants related to credential dialogs. */
import { SnowflakeCredentialsDialog } from '#/data/serviceCredentials/SnowflakeCredentialsDialog'
import type { CredentialInfo } from '#/data/serviceCredentials/types'

export const CREDENTIAL_TYPES: readonly CredentialInfo[] = [
  {
    icon: undefined,
    nameId: 'snowflakeCredentialType',
    component: SnowflakeCredentialsDialog,
  },
]

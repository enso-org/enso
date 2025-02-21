/** @file Constants related to credential dialogs. */
import { GoogleCredentialsDialog } from '#/data/serviceCredentials/GoogleCredentialsDialog'
import { SnowflakeCredentialsDialog } from '#/data/serviceCredentials/SnowflakeCredentialsDialog'
import type { CredentialInfo } from '#/data/serviceCredentials/types'

export const CREDENTIAL_INFOS: readonly [CredentialInfo, ...CredentialInfo[]] = [
  {
    icon: undefined,
    nameId: 'snowflakeCredentialType',
    credentialType: 'snowflake',
    component: SnowflakeCredentialsDialog,
  },
  {
    icon: undefined,
    nameId: 'googleCredentialType',
    credentialType: 'google',
    component: GoogleCredentialsDialog,
  },
]

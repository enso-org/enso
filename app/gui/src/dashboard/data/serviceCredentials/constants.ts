/** @file Constants related to credential dialogs. */
import { GoogleCredentialsDialog } from '#/data/serviceCredentials/GoogleCredentialsForm'
import { SnowflakeCredentialsForm } from '#/data/serviceCredentials/SnowflakeCredentialsForm'
import type { CredentialInfo } from '#/data/serviceCredentials/types'

export const CREDENTIAL_INFOS: readonly [CredentialInfo, ...CredentialInfo[]] = [
  {
    icon: undefined,
    nameId: 'snowflakeCredentialType',
    credentialType: 'snowflake',
    component: SnowflakeCredentialsForm,
  },
  {
    icon: undefined,
    nameId: 'googleCredentialType',
    credentialType: 'google',
    component: GoogleCredentialsDialog,
  },
]

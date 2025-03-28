/** @file Constants related to credential dialogs. */
import { GoogleCredentialsDialog } from '#/data/serviceCredentials/GoogleCredentialsForm'
import { SnowflakeCredentialsForm } from '#/data/serviceCredentials/SnowflakeCredentialsForm'
import type { CredentialInfo } from '#/data/serviceCredentials/types'
import * as snowflake from './snowflake'
import * as google from './snowflake'

export const CREDENTIAL_INFOS: readonly [CredentialInfo, ...CredentialInfo[]] = [
  {
    icon: undefined,
    nameId: 'snowflakeCredentialType',
    credentialType: 'snowflake',
    makeAuthUrl: snowflake.makeAuthUrl,
    component: SnowflakeCredentialsForm,
  },
  {
    icon: undefined,
    nameId: 'googleCredentialType',
    credentialType: 'google',
    makeAuthUrl: google.makeAuthUrl,
    component: GoogleCredentialsDialog,
  },
]

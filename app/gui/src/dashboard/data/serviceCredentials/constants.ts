/** @file Constants related to credential dialogs. */
import { GoogleCredentialsForm } from '#/data/serviceCredentials/GoogleCredentialsForm'
import { MS365CredentialsForm } from '#/data/serviceCredentials/MS365CredentialsForm'
import { SnowflakeCredentialsForm } from '#/data/serviceCredentials/SnowflakeCredentialsForm'
import { StravaCredentialsForm } from '#/data/serviceCredentials/StravaCredentialsForm'
import type { CredentialInfo } from '#/data/serviceCredentials/types'

export const CREDENTIAL_INFOS: readonly [CredentialInfo, ...CredentialInfo[]] = [
  {
    icon: undefined,
    nameId: 'snowflakeCredentialType',
    credentialType: 'snowflake',
    form: SnowflakeCredentialsForm,
  },
  {
    icon: undefined,
    nameId: 'googleCredentialType',
    credentialType: 'google',
    form: GoogleCredentialsForm,
  },
  {
    icon: undefined,
    nameId: 'stravaCredentialType',
    credentialType: 'strava',
    form: StravaCredentialsForm,
  },
  {
    icon: undefined,
    nameId: 'ms365CredentialType',
    credentialType: 'ms365',
    form: MS365CredentialsForm,
  },
]

/** @file Constants related to credential dialogs. */
import { GoogleCredentialsForm } from '#/data/serviceCredentials/GoogleCredentialsForm'
import { MS365CredentialsForm } from '#/data/serviceCredentials/MS365CredentialsForm'
import { SalesforceCredentialsForm } from '#/data/serviceCredentials/SalesforceCredentialsForm'
import { SnowflakeCredentialsForm } from '#/data/serviceCredentials/SnowflakeCredentialsForm'
import { StravaCredentialsForm } from '#/data/serviceCredentials/StravaCredentialsForm'
import type { CredentialInfo } from '#/data/serviceCredentials/types'

export const CREDENTIAL_INFOS: readonly [CredentialInfo, ...CredentialInfo[]] = [
  {
    nameId: 'snowflakeCredentialType',
    credentialType: 'snowflake',
    form: SnowflakeCredentialsForm,
  },
  {
    nameId: 'googleCredentialType',
    credentialType: 'google',
    form: GoogleCredentialsForm,
  },
  {
    nameId: 'stravaCredentialType',
    credentialType: 'strava',
    form: StravaCredentialsForm,
  },
  {
    nameId: 'ms365CredentialType',
    credentialType: 'ms365',
    form: MS365CredentialsForm,
  },
  {
    nameId: 'salesforceCredentialType',
    credentialType: 'salesforce',
    form: SalesforceCredentialsForm,
  },
]

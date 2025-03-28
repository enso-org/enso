/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { CredentialsFormProps } from './types'

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsForm(_props: CredentialsFormProps) {
  const { getText } = useText()

  return (
    <div className="w-full">
      {/* `name` field is pre-filtered to oexport nly fields with a matching type! */}
      <Input name="account" label={getText('account')} />
      <Input name="client_id" label={getText('clientId')} />
      <Input name="client_secret" label={getText('clientSecret')} />
      <Input name="role" label={getText('role')} />
    </div>
  )
}

/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { CredentialsFormProps } from './types'
import { CheckboxGroup } from '#/components/AriaComponents/Checkbox/CheckboxGroup'

/** Dialog for a Google credential. */
export function GoogleCredentialsDialog(_props: CredentialsFormProps) {
  const { getText } = useText()
  return (
    <div className="w-full">
      {/* `name` field is pre-filtered to only fields with a matching type! */}
      <CheckboxGroup name="scopes">
        <Checkbox value="https://www.googleapis.com/auth/spreadsheets">
          {getText('googleCredentialSheetsScope')}
        </Checkbox>
        <Checkbox value="https://www.googleapis.com/auth/analytics">
          {getText('googleCredentialAnalyticsScope')}
        </Checkbox>
      </CheckboxGroup>
    </div>
  )
}

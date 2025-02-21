/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this file is re-exported by `./index.ts`.
 */

import { Form as FormComponent } from '#/components/AriaComponents'
import { CredentialsFormButtons } from '#/data/serviceCredentials/CredentialsFormButtons'
import { useSynchronizeCredentialsValue } from '#/data/serviceCredentials/hooks'
import { useText } from '#/providers/TextProvider'
import type { CredentialsDialogProps } from './types'

/** Dialog for a Google credential. */
export function GoogleCredentialsDialog(props: CredentialsDialogProps) {
  const { value, upsertCredential, ...buttonsProps } = props

  const { getText } = useText()
  const { form, Form, CheckboxGroup, Checkbox } = FormComponent.useFormWithComponents({
    mode: 'onChange',
    schema: (z) =>
      z.object({
        scopes: z.string().array(),
      }),
    onSubmit: upsertCredential,
  })
  useSynchronizeCredentialsValue(form, value)

  return (
    <Form className="w-full">
      {/* `name` field is pre-filtered to only fields with a matching type! */}
      <CheckboxGroup name="scopes">
        <Checkbox value="https://www.googleapis.com/auth/spreadsheets">
          {getText('googleCredentialSheetsScope')}
        </Checkbox>
        <Checkbox value="https://www.googleapis.com/auth/spreadsheets.readonly">
          {getText('googleCredentialSheetsReadScope')}
        </Checkbox>
        <Checkbox value="">{getText('googleCredentialSheetsWriteScope')}</Checkbox>
        <Checkbox value="">{getText('googleCredentialSheetsDriveBrowseScope')}</Checkbox>
        <Checkbox value="https://www.googleapis.com/auth/analytics">
          {getText('googleCredentialAnalyticsScope')}
        </Checkbox>
      </CheckboxGroup>
      <CredentialsFormButtons {...buttonsProps} />
    </Form>
  )
}

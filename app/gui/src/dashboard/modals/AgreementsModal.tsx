/** @file Modal for accepting the terms of service. */
import { Button } from '#/components/Button'
import { Checkbox } from '#/components/Checkbox'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Text } from '#/components/Text'
import { useText } from '$/providers/react'
import { memo } from 'react'

/** Properties of {@link AgreementsModal} component. */
export interface AgreementsModalProps {
  readonly agreedToTos: boolean
  readonly agreedToPrivacyPolicy: boolean
  readonly userAgreed: () => void
}

/** Modal for accepting the terms of service. */
export const AgreementsModal = memo(function AgreementsModal(props: AgreementsModalProps) {
  const { agreedToTos, agreedToPrivacyPolicy, userAgreed } = props
  const { getText } = useText()

  // Note that this produces warnings about missing a `<Heading slot="title">`, even though
  // all `ariaComponents.Dialog`s contain one. This is likely caused by Suspense discarding
  // renders, and so it does not seem to be fixable.
  return (
    <Dialog
      title={getText('licenseAgreementTitle')}
      isKeyboardDismissDisabled
      isDismissable={false}
      hideCloseButton
      modalProps={{ defaultOpen: true }}
      testId="agreements-modal"
      id="agreements-modal"
    >
      <Form
        schema={(schema) =>
          schema.object({
            // The user must agree to the ToS to proceed.
            agreedToTos: schema
              .array(schema.string())
              .min(1, { message: getText('licenseAgreementCheckboxError') }),
            agreedToPrivacyPolicy: schema
              .array(schema.string())
              .min(1, { message: getText('privacyPolicyCheckboxError') }),
          })
        }
        defaultValues={{
          agreedToTos: agreedToTos ? ['agree'] : [],
          agreedToPrivacyPolicy: agreedToPrivacyPolicy ? ['agree'] : [],
        }}
        testId="agreements-form"
        method="dialog"
        onSubmit={userAgreed}
      >
        {({ form }) => (
          <>
            <Text>{getText('someAgreementsHaveBeenUpdated')}</Text>

            <Checkbox.Group
              form={form}
              name="agreedToTos"
              description={
                <Button variant="link" target="_blank" href={`${$config.ENSO_HOST}/eula`}>
                  {getText('viewLicenseAgreement')}
                </Button>
              }
            >
              <Checkbox value="agree">{getText('licenseAgreementCheckbox')}</Checkbox>
            </Checkbox.Group>

            <Checkbox.Group
              form={form}
              name="agreedToPrivacyPolicy"
              description={
                <Button variant="link" target="_blank" href={`${$config.ENSO_HOST}/privacy`}>
                  {getText('viewPrivacyPolicy')}
                </Button>
              }
            >
              <Checkbox value="agree">{getText('privacyPolicyCheckbox')}</Checkbox>
            </Checkbox.Group>

            <Form.Submit fullWidth>{getText('accept')}</Form.Submit>

            <Form.FormError />
          </>
        )}
      </Form>
    </Dialog>
  )
})

/** @file Modal for confirming delete of any type of asset. */
import { ButtonGroup, Dialog, Form, Input, Password } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { SecretId } from '#/services/Backend'

// =========================
// === UpsertSecretModal ===
// =========================

/** Props for a {@link UpsertSecretModal}. */
export interface UpsertSecretModalProps {
  readonly id: SecretId | null
  readonly name: string | null
  readonly defaultOpen?: boolean
  readonly doCreate: (name: string, value: string) => Promise<void> | void
}

/** A modal for creating and editing a secret. */
export default function UpsertSecretModal(props: UpsertSecretModalProps) {
  const { id, name: nameRaw, defaultOpen, doCreate } = props
  const { getText } = useText()

  const isCreatingSecret = id == null
  const isNameEditable = nameRaw == null

  return (
    <Dialog
      title={isCreatingSecret ? getText('newSecret') : getText('editSecret')}
      modalProps={defaultOpen == null ? {} : { defaultOpen }}
      isDismissable={false}
    >
      <Form
        testId="upsert-secret-modal"
        method="dialog"
        schema={(z) =>
          z.object({ name: z.string().min(1, getText('emptyStringError')), value: z.string() })
        }
        defaultValues={{ name: nameRaw ?? '', value: '' }}
        onSubmit={async ({ name, value }) => {
          await doCreate(name, value)
        }}
      >
        {({ form }) => (
          <>
            <Input
              form={form}
              name="name"
              autoFocus
              autoComplete="off"
              disabled={!isNameEditable}
              label={getText('name')}
              placeholder={getText('secretNamePlaceholder')}
            />
            <Password
              form={form}
              name="value"
              autoFocus={!isNameEditable}
              autoComplete="off"
              label={getText('value')}
              placeholder={
                isNameEditable ? getText('secretValuePlaceholder') : getText('secretValueHidden')
              }
            />
            <ButtonGroup>
              <Form.Submit>{isCreatingSecret ? getText('create') : getText('update')}</Form.Submit>
              <Form.Submit formnovalidate />
            </ButtonGroup>
          </>
        )}
      </Form>
    </Dialog>
  )
}

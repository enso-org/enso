/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import { Button, ButtonGroup, Dialog, Form, Input, Password } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { SecretId } from '#/services/Backend'

/** Create the schema for this form. */
function createUpsertSecretSchema() {
  return z.object({
    name: z.string().min(1),
    value: z.string(),
  })
}

// =========================
// === UpsertSecretModal ===
// =========================

/** Props for a {@link UpsertSecretModal}. */
export interface UpsertSecretModalProps {
  readonly id: SecretId | null
  readonly name: string | null
  readonly doCreate: (name: string, value: string) => Promise<void> | void
}

/** A modal for creating and editing a secret. */
export default function UpsertSecretModal(props: UpsertSecretModalProps) {
  const { id, name: nameRaw, doCreate } = props
  const { getText } = useText()

  const isCreatingSecret = id == null
  const isNameEditable = nameRaw == null

  return (
    <Dialog title={isCreatingSecret ? getText('newSecret') : getText('editSecret')}>
      {({ close }) => (
        <Form
          data-testid="upsert-secret-modal"
          schema={createUpsertSecretSchema()}
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
                defaultValue={nameRaw ?? undefined}
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
              <ButtonGroup className="relative">
                <Form.Submit>
                  {isCreatingSecret ? getText('create') : getText('update')}
                </Form.Submit>
                <Button variant="cancel" onPress={close}>
                  {getText('cancel')}
                </Button>
              </ButtonGroup>
            </>
          )}
        </Form>
      )}
    </Dialog>
  )
}

/** @file A menu for creating and editing a secret. */
import { Button, ButtonGroup, Dialog, Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { SecretId } from '#/services/Backend'

/** Props for a {@link UpsertSecretMenu}. */
export interface UpsertSecretMenuProps {
  readonly secretId?: SecretId | null
  readonly name?: string | null
  readonly doCreate: (name: string, value: string) => void
  /**
   * If provided, a cancel button will be offered.
   *
   * The value may be:
   * - A callback to run if the button is pressed.
   * - 'close': The cancel button will close the containing dialog.
   * - 'reset': The cancel button will reset the form.
   */
  readonly doCancel?: 'close' | 'reset' | (() => void) | null
}

/** A modal for creating and editing a secret. */
export default function UpsertSecretMenu(props: UpsertSecretMenuProps) {
  const { secretId, name: nameRaw, doCreate, doCancel } = props
  const { getText } = useText()

  const isCreatingSecret = secretId == null

  return (
    <Form
      schema={(z) => z.object({ title: z.string().min(1), value: z.string() })}
      defaultValues={{ title: nameRaw ?? '', value: '' }}
      onSubmit={({ title, value }) => {
        doCreate(title, value)
      }}
      method="dialog"
      testId="upsert-secret-modal"
      className="w-full"
    >
      {isCreatingSecret && (
        <Input
          name="title"
          autoFocus
          autoComplete="off"
          label={getText('name')}
          placeholder={getText('secretNamePlaceholder')}
        />
      )}

      <Input
        name="value"
        type="password"
        autoComplete="off"
        label={getText('value')}
        placeholder={
          nameRaw == null ? getText('secretValuePlaceholder') : getText('secretValueHidden')
        }
      />

      <ButtonGroup className="mt-2">
        <Form.Submit>{isCreatingSecret ? getText('create') : getText('update')}</Form.Submit>
        {doCancel === 'reset' ?
          <Form.Reset>{getText('cancel')}</Form.Reset>
        : doCancel === 'close' ?
          <Dialog.Close>{getText('cancel')}</Dialog.Close>
        : doCancel ?
          <Button onPress={doCancel}>{getText('cancel')}</Button>
        : null}
      </ButtonGroup>

      <Form.FormError />
    </Form>
  )
}

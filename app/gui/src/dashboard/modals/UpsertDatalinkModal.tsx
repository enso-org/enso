/** @file A modal for creating and editing a Datalink. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import SCHEMA from '#/data/datalinkSchema.json' with { type: 'json' }
import { validateDatalink } from '#/data/datalinkValidator'
import { DatalinkFormInput } from '#/pages/dashboard/components/DatalinkInput'
import { constantValueOfSchema } from '#/utilities/jsonSchema'
import { useText } from '$/providers/react'

const DEFS: Record<string, object> = SCHEMA.$defs
const INITIAL_DATALINK_VALUE = constantValueOfSchema(DEFS, SCHEMA.$defs.DataLink, true)[0] ?? null

/** Props for a {@link UpsertDatalinkModal}. */
export interface UpsertDatalinkModalProps {
  readonly doCreate: (name: string, datalink: unknown) => Promise<void> | void
}

/** A modal for creating and editing a Datalink. */
export default function UpsertDatalinkModal(props: UpsertDatalinkModalProps) {
  const { doCreate } = props

  const { getText } = useText()

  return (
    <Dialog title={getText('createDatalink')} isDismissable={false}>
      <Form
        method="dialog"
        schema={(z) =>
          z.object({
            name: z.string().min(1),
            value: z.unknown().refine(validateDatalink),
          })
        }
        defaultValues={{ value: INITIAL_DATALINK_VALUE }}
        onSubmit={({ name, value }) => doCreate(name, value)}
      >
        <Input
          name="name"
          autoFocus
          label={getText('name')}
          placeholder={getText('datalinkNamePlaceholder')}
        />

        <div className="relative w-full">
          <DatalinkFormInput name="value" dropdownTitle={getText('type')} />
        </div>

        <Button.Group>
          <Form.Submit>{getText('create')}</Form.Submit>
          <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
        </Button.Group>

        <Form.FormError />
      </Form>
    </Dialog>
  )
}

/** @file A modal for creating a Datalink. */
import { ButtonGroup, Dialog, Form, Input } from '#/components/AriaComponents'
import { DatalinkFormInput } from '#/components/dashboard/DatalinkInput'
import SCHEMA from '#/data/datalinkSchema.json' with { type: 'json' }
import { validateDatalink } from '#/data/datalinkValidator'
import { useText } from '#/providers/TextProvider'
import { constantValueOfSchema } from '#/utilities/jsonSchema'

// =================
// === Constants ===
// =================

const DEFS: Record<string, object> = SCHEMA.$defs
const INITIAL_DATALINK_VALUE = constantValueOfSchema(DEFS, SCHEMA.$defs.DataLink, true)[0] ?? null

// ===========================
// === UpsertDataLinkModal ===
// ===========================

/** Props for a {@link UpsertDatalinkModal}. */
export interface UpsertDatalinkModalProps {
  readonly doCreate: (name: string, datalink: unknown) => Promise<void> | void
}

/** A modal for creating a Datalink. */
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

        <ButtonGroup>
          <Form.Submit>{getText('create')}</Form.Submit>
          <Form.Submit action="cancel" />
        </ButtonGroup>

        <Form.FormError />
      </Form>
    </Dialog>
  )
}

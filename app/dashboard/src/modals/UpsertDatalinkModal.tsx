/** @file A modal for creating a Datalink. */
import * as z from 'zod'

import { Button, ButtonGroup, Dialog, Form, Input } from '#/components/AriaComponents'
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

/** Create the schema for this form. */
function createUpsertDatalinkSchema() {
  return z.object({ name: z.string().min(1), value: z.unknown().refine(validateDatalink) })
}

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
    <Dialog size="xlarge" title={getText('createDatalink')} isDismissable={false}>
      <Form
        method="dialog"
        schema={createUpsertDatalinkSchema()}
        defaultValues={{ value: INITIAL_DATALINK_VALUE }}
        onSubmit={({ name, value }) => doCreate(name, value)}
      >
        {({ form }) => (
          <>
            <Input
              form={form}
              name="name"
              autoFocus
              label={getText('name')}
              placeholder={getText('datalinkNamePlaceholder')}
            />
            <div className="relative w-full">
              <DatalinkFormInput form={form} name="value" dropdownTitle={getText('type')} />
            </div>

            <ButtonGroup>
              <Form.Submit>{getText('create')}</Form.Submit>
              <Form.Submit formnovalidate>{getText('cancel')}</Form.Submit>
            </ButtonGroup>

            <Form.FormError />
          </>
        )}
      </Form>
    </Dialog>
  )
}

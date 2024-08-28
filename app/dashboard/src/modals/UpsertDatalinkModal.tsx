/** @file A modal for creating a Datalink. */
import * as React from 'react'

import SCHEMA from '#/data/datalinkSchema.json' with { type: 'json' }
import * as datalinkValidator from '#/data/datalinkValidator'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import DatalinkInput from '#/components/dashboard/DatalinkInput'
import Modal from '#/components/Modal'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import * as jsonSchema from '#/utilities/jsonSchema'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const DEFS: Record<string, object> = SCHEMA.$defs
const INITIAL_DATALINK_VALUE =
  jsonSchema.constantValue(DEFS, SCHEMA.$defs.DataLink, true)[0] ?? null

// ===========================
// === UpsertDataLinkModal ===
// ===========================

/** Props for a {@link UpsertDatalinkModal}. */
export interface UpsertDatalinkModalProps {
  readonly doCreate: (name: string, datalink: unknown) => void
}

/** A modal for creating a Datalink. */
export default function UpsertDatalinkModal(props: UpsertDatalinkModalProps) {
  const { doCreate } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const [name, setName] = React.useState('')
  const [value, setValue] = React.useState<NonNullable<unknown> | null>(INITIAL_DATALINK_VALUE)
  const isValueSubmittable = React.useMemo(() => datalinkValidator.validateDatalink(value), [value])
  const isSubmittable = name !== '' && isValueSubmittable

  const doSubmit = () => {
    unsetModal()
    doCreate(name, value)
  }

  return (
    <Modal centered className="bg-dim">
      <form
        className="pointer-events-auto relative flex min-w-upsert-data-link-modal max-w-upsert-data-link-modal-max flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={(event) => {
          event.preventDefault()
          doSubmit()
        }}
      >
        <aria.Heading className="relative text-sm font-semibold">
          {getText('createDatalink')}
        </aria.Heading>
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <aria.TextField
              aria-errormessage={getText('mustNotBeBlank')}
              className="relative flex items-center"
              {...innerProps}
            >
              <aria.Label className="text w-modal-label">{getText('name')}</aria.Label>
              <FocusRing>
                <aria.Input
                  autoFocus
                  placeholder={getText('datalinkNamePlaceholder')}
                  className={tailwindMerge.twMerge(
                    'focus-child text grow rounded-full border bg-transparent px-input-x',
                    name !== '' ? 'border-primary/10' : 'border-red-700/60',
                  )}
                  value={name}
                  onInput={(event) => {
                    setName(event.currentTarget.value)
                  }}
                />
              </FocusRing>
            </aria.TextField>
          )}
        </FocusArea>
        <div className="relative">
          <DatalinkInput dropdownTitle="Type" value={value} setValue={setValue} />
        </div>
        <ariaComponents.ButtonGroup className="relative">
          <ariaComponents.Button
            size="medium"
            variant="submit"
            isDisabled={!isSubmittable}
            onPress={doSubmit}
          >
            {getText('create')}
          </ariaComponents.Button>
          <ariaComponents.Button size="medium" variant="outline" onPress={unsetModal}>
            {getText('cancel')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </form>
    </Modal>
  )
}

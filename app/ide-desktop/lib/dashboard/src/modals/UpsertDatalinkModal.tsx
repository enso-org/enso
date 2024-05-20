/** @file A modal for creating a Datalink. */
import * as React from 'react'

import SCHEMA from '#/data/datalinkSchema.json' assert { type: 'json' }
import * as datalinkValidator from '#/data/datalinkValidator'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import DatalinkInput from '#/components/dashboard/DatalinkInput'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import UnstyledButton from '#/components/UnstyledButton'

import * as jsonSchema from '#/utilities/jsonSchema'

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
        onClick={event => {
          event.stopPropagation()
        }}
        onSubmit={event => {
          event.preventDefault()
          doSubmit()
        }}
      >
        <aria.Heading className="relative text-sm font-semibold">
          {getText('createDatalink')}
        </aria.Heading>
        <FocusArea direction="horizontal">
          {innerProps => (
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
                  className={`focus-child text grow rounded-full border bg-transparent px-input-x ${
                    name !== '' ? 'border-primary/10' : 'border-red-700/60'
                  }`}
                  value={name}
                  onInput={event => {
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
        <ButtonRow>
          <UnstyledButton
            isDisabled={!isSubmittable}
            className="button bg-invite text-white enabled:active"
            onPress={doSubmit}
          >
            {getText('create')}
          </UnstyledButton>
          <UnstyledButton className="button bg-selected-frame active" onPress={unsetModal}>
            {getText('cancel')}
          </UnstyledButton>
        </ButtonRow>
      </form>
    </Modal>
  )
}

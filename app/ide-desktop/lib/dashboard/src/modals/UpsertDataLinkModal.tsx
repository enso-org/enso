/** @file A modal for creating a Data Link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'
import DataLinkInput from '#/components/dashboard/DataLinkInput'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import UnstyledButton from '#/components/styled/UnstyledButton'

import * as jsonSchema from '#/utilities/jsonSchema'
import * as validateDataLink from '#/utilities/validateDataLink'

// =================
// === Constants ===
// =================

const DEFS: Record<string, object> = SCHEMA.$defs
const INITIAL_DATA_LINK_VALUE =
  jsonSchema.constantValue(DEFS, SCHEMA.$defs.DataLink, true)[0] ?? null

// ===========================
// === UpsertDataLinkModal ===
// ===========================

/** Props for a {@link UpsertDataLinkModal}. */
export interface UpsertDataLinkModalProps {
  readonly doCreate: (name: string, dataLink: unknown) => void
}

/** A modal for creating a Data Link. */
export default function UpsertDataLinkModal(props: UpsertDataLinkModalProps) {
  const { doCreate } = props
  const { unsetModal } = modalProvider.useSetModal()
  const [name, setName] = React.useState('')
  const [value, setValue] = React.useState<NonNullable<unknown> | null>(INITIAL_DATA_LINK_VALUE)
  const isValueSubmittable = React.useMemo(() => validateDataLink.validateDataLink(value), [value])
  const isSubmittable = name !== '' && isValueSubmittable

  const doSubmit = () => {
    unsetModal()
    doCreate(name, value)
  }

  return (
    <Modal centered className="bg-dim">
      <form
        className="pointer-events-auto relative flex w-upsert-data-link-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onKeyDown={event => {
          if (event.key !== 'Escape') {
            event.stopPropagation()
          }
        }}
        onClick={event => {
          event.stopPropagation()
        }}
        onSubmit={event => {
          event.preventDefault()
          doSubmit()
        }}
      >
        <aria.Heading className="relative text-sm font-semibold">Create Data Link</aria.Heading>
        <aria.TextField
          className="relative flex items-center"
          aria-errormessage="Must not be blank."
        >
          <aria.Label className="text w-modal-label">Name</aria.Label>
          <aria.Input
            autoFocus
            placeholder="Enter the name of the Data Link"
            className={`text grow rounded-full border bg-transparent px-input-x ${
              name !== '' ? 'border-primary/10' : 'border-red-700/60'
            }`}
            value={name}
            onInput={event => {
              setName(event.currentTarget.value)
            }}
          />
        </aria.TextField>
        <div className="relative">
          <DataLinkInput dropdownTitle="Type" value={value} setValue={setValue} />
        </div>
        <ButtonRow>
          <UnstyledButton
            isDisabled={!isSubmittable}
            className="button bg-invite text-white enabled:active"
            onPress={doSubmit}
          >
            Create
          </UnstyledButton>
          <UnstyledButton className="button bg-selected-frame active" onPress={unsetModal}>
            Cancel
          </UnstyledButton>
        </ButtonRow>
      </form>
    </Modal>
  )
}

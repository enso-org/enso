/** @file A modal for creating a Data Link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import * as modalProvider from '#/providers/ModalProvider'

import DataLinkInput from '#/components/dashboard/DataLinkInput'
import Modal from '#/components/Modal'

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

  return (
    <Modal centered className="bg-dim">
      <form
        className="relative flex flex-col gap-modal rounded-default w-upsert-data-link-modal p-modal-wide pt-modal pointer-events-auto before:inset before:absolute before:rounded-default before:bg-selected-frame before:backdrop-blur-default before:w-full before:h-full"
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
          unsetModal()
          doCreate(name, value)
        }}
      >
        <h1 className="relative text-sm font-semibold">Create Data Link</h1>
        <div className="relative flex items-center" title="Must not be blank.">
          <div className="text w-modal-label">Name</div>
          <input
            autoFocus
            placeholder="Enter the name of the Data Link"
            className={`text grow bg-transparent border rounded-full px-input-x disabled:opacity-disabled ${
              name !== '' ? 'border-black/10' : 'border-red-700/60'
            }`}
            value={name}
            onInput={event => {
              setName(event.currentTarget.value)
            }}
          />
        </div>
        <div className="relative">
          <DataLinkInput dropdownTitle="Type" value={value} setValue={setValue} />
        </div>
        <div className="relative flex gap-buttons">
          <button type="submit" disabled={!isSubmittable} className="button text-white bg-invite">
            Create
          </button>
          <button type="button" className="button active bg-selected-frame" onClick={unsetModal}>
            Cancel
          </button>
        </div>
      </form>
    </Modal>
  )
}

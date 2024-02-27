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
        className="relative flex flex-col gap-2 rounded-2xl w-96 p-4 pt-2 pointer-events-auto before:inset-0 before:absolute before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:w-full before:h-full"
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
        <div className="relative flex" title="Must not be blank.">
          <div className="w-12 h-6 py-1">Name</div>
          <input
            autoFocus
            placeholder="Enter the name of the Data Link"
            className={`grow bg-transparent border rounded-full leading-170 h-6 px-4 py-px disabled:opacity-50 ${
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
        <div className="relative flex gap-2">
          <button
            type="submit"
            disabled={!isSubmittable}
            className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
          >
            Create
          </button>
          <button
            type="button"
            className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1"
            onClick={unsetModal}
          >
            Cancel
          </button>
        </div>
      </form>
    </Modal>
  )
}

/** @file A modal for creating a Data Link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

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
  const { getText } = textProvider.useText()
  const [name, setName] = React.useState('')
  const [value, setValue] = React.useState<NonNullable<unknown> | null>(INITIAL_DATA_LINK_VALUE)
  const isValueSubmittable = React.useMemo(() => validateDataLink.validateDataLink(value), [value])
  const isSubmittable = name !== '' && isValueSubmittable

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
          unsetModal()
          doCreate(name, value)
        }}
      >
        <h1 className="relative text-sm font-semibold">{getText('createDataLink')}</h1>
        <div className="relative flex items-center" title={getText('mustNotBeBlank')}>
          <div className="text w-modal-label">{getText('name')}</div>
          <input
            autoFocus
            placeholder={getText('dataLinkNamePlaceholder')}
            className={`text grow rounded-full border bg-transparent px-input-x ${
              name !== '' ? 'border-primary/10' : 'border-red-700/60'
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
          <button
            type="submit"
            disabled={!isSubmittable}
            className="button bg-invite text-white enabled:active"
          >
            {getText('create')}
          </button>
          <button type="button" className="button bg-selected-frame active" onClick={unsetModal}>
            {getText('cancel')}
          </button>
        </div>
      </form>
    </Modal>
  )
}

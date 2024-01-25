/** @file A modal for creating a Data Link. */
import * as React from 'react'

import DataLinkWizard from '#/layouts/dashboard/DataLinkWizard'
import * as modalProvider from '#/providers/ModalProvider'

import Modal from '#/components/Modal'

/** Props for a {@link CreateDataLinkModal}. */
export interface CreateDataLinkModalProps {
  doCreate: (dataLink: unknown) => void
}

/** A modal for creating a Data Link. */
export default function CreateDataLinkModal(props: CreateDataLinkModalProps) {
  const { doCreate } = props
  const { unsetModal } = modalProvider.useSetModal()
  const [state, setState] = React.useState<unknown>({})

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
          doCreate(state)
        }}
      >
        <h1 className="relative text-sm font-semibold">Create Data Link</h1>
        <div className="relative">
          <DataLinkWizard dropdownTitle="Type" state={state} setState={setState} />
        </div>
        <div className="relative flex gap-2">
          <button
            type="submit"
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

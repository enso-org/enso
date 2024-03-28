/** @file A modal for creating a Data Link. */
import * as React from 'react'

import SCHEMA from '#/data/dataLinkSchema.json' assert { type: 'json' }
import * as dataLinkValidator from '#/data/dataLinkValidator'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import DataLinkInput from '#/components/dashboard/DataLinkInput'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import UnstyledButton from '#/components/styled/UnstyledButton'

import * as jsonSchema from '#/utilities/jsonSchema'

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
  const isValueSubmittable = React.useMemo(() => dataLinkValidator.validateDataLink(value), [value])
  const isSubmittable = name !== '' && isValueSubmittable

  const doSubmit = () => {
    unsetModal()
    doCreate(name, value)
  }

  return (
    <Modal centered className="bg-dim">
      <form
        className="pointer-events-auto relative flex w-upsert-data-link-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={event => {
          event.stopPropagation()
        }}
        onSubmit={event => {
          event.preventDefault()
          doSubmit()
        }}
      >
        <aria.Heading className="relative text-sm font-semibold">
          {getText('createDataLink')}
        </aria.Heading>
        <FocusArea direction="horizontal">
          {(ref, innerProps) => (
            <aria.TextField
              ref={ref}
              aria-errormessage={getText('mustNotBeBlank')}
              className="relative flex items-center"
              {...innerProps}
            >
              <aria.Label className="text w-modal-label">{getText('name')}</aria.Label>
              <FocusRing>
                <aria.Input
                  autoFocus
                  placeholder={getText('dataLinkNamePlaceholder')}
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
          <DataLinkInput dropdownTitle="Type" value={value} setValue={setValue} />
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

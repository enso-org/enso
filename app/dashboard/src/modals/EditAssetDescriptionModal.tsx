/** @file Modal for editing the description of an asset. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import Modal from '#/components/Modal'

// =================================
// === EditAssetDescriptionModal ===
// =================================

/** Props for a {@link EditAssetDescriptionModal}. */
export interface EditAssetDescriptionModalProps {
  readonly actionButtonLabel?: string
  readonly initialDescription: string | null
  /** Callback to change the asset's description. */
  readonly doChangeDescription: (newDescription: string) => Promise<void>
}

/** Modal for editing the description of an asset. */
export default function EditAssetDescriptionModal(props: EditAssetDescriptionModalProps) {
  const { getText } = textProvider.useText()
  const {
    doChangeDescription,
    initialDescription,
    actionButtonLabel = getText('editAssetDescriptionModalSubmit'),
  } = props
  const { unsetModal } = modalProvider.useSetModal()
  const [description, setDescription] = React.useState(initialDescription ?? '')
  const initialdescriptionRef = React.useRef(initialDescription)
  const textareaRef = React.useRef<HTMLTextAreaElement>(null)

  const { isPending, error, mutate } = reactQuery.useMutation({
    mutationFn: doChangeDescription,
    onSuccess: () => {
      unsetModal()
    },
  })

  React.useLayoutEffect(() => {
    if (
      textareaRef.current &&
      typeof initialdescriptionRef.current === 'string' &&
      initialdescriptionRef.current.length > 0
    ) {
      textareaRef.current.select()
    }
  }, [])

  return (
    <Modal centered className="bg-dim">
      <form
        data-testid="edit-description-modal"
        className="pointer-events-auto relative flex w-confirm-delete-modal flex-col gap-modal rounded-default p-modal-wide py-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onKeyDown={(event) => {
          if (event.key !== 'Escape') {
            event.stopPropagation()
          }
        }}
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={(event) => {
          event.preventDefault()
          mutate(description)
        }}
      >
        <div className="relative text-sm font-semibold">
          {getText('editAssetDescriptionModalTitle')}
        </div>
        <textarea
          ref={textareaRef}
          className="relative h-16 resize-none rounded-default bg-selected-frame px-4 py-2"
          placeholder={getText('editAssetDescriptionModalPlaceholder')}
          onChange={(event) => {
            setDescription(event.target.value)
          }}
          value={description}
          disabled={isPending}
          autoFocus
        />

        {error && <div className="relative text-sm text-red-500">{error.message}</div>}

        <ariaComponents.ButtonGroup className="relative">
          <ariaComponents.Button variant="submit" type="submit" loading={isPending}>
            {actionButtonLabel}
          </ariaComponents.Button>
          <ariaComponents.Button
            variant="outline"
            type="button"
            onPress={unsetModal}
            isDisabled={isPending}
          >
            {getText('editAssetDescriptionModalCancel')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </form>
    </Modal>
  )
}

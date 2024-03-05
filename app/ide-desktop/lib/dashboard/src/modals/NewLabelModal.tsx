/** @file A modal for creating a new label. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'

import ColorPicker from '#/components/ColorPicker'
import Modal from '#/components/Modal'

import * as backend from '#/services/Backend'

// =====================
// === NewLabelModal ===
// =====================

/** Props for a {@link NewLabelModal}. */
export interface NewLabelModalProps {
  readonly labels: backend.Label[]
  readonly eventTarget: HTMLElement
  readonly doCreate: (value: string, color: backend.LChColor) => void
}

/** A modal for creating a new label. */
export default function NewLabelModal(props: NewLabelModalProps) {
  const { labels, eventTarget, doCreate } = props
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { unsetModal } = modalProvider.useSetModal()
  const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])

  const labelNames = React.useMemo(
    () => new Set<string>(labels.map(label => label.value)),
    [labels]
  )
  const leastUsedColor = React.useMemo(() => backend.leastUsedColor(labels), [labels])

  const [value, setName] = React.useState('')
  const [color, setColor] = React.useState<backend.LChColor | null>(null)
  const canSubmit = Boolean(value && !labelNames.has(value))

  const onSubmit = () => {
    unsetModal()
    try {
      doCreate(value, color ?? leastUsedColor)
    } catch (error) {
      toastAndLog(null, error)
    }
  }

  return (
    <Modal className="absolute bg-dim">
      <form
        data-testid="new-label-modal"
        tabIndex={-1}
        style={{
          left: position.left + window.scrollX,
          top: position.top + window.scrollY,
        }}
        className="pointer-events-auto relative flex w-new-label-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
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
          // Consider not calling `onSubmit()` here to make it harder to accidentally
          // delete an important asset.
          onSubmit()
        }}
      >
        <h1 className="relative text-sm font-semibold">New Label</h1>
        <label className="relative flex items-center">
          <div className="text w-modal-label">Name</div>
          <input
            autoFocus
            size={1}
            placeholder="Enter the name of the label"
            className={`text grow rounded-full border border-black/10 bg-transparent px-input-x ${
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              color != null && color.lightness <= 50
                ? 'text-tag-text placeholder-selected-frame'
                : 'text-primary'
            }`}
            style={
              color == null
                ? {}
                : {
                    backgroundColor: backend.lChColorToCssColor(color),
                  }
            }
            onInput={event => {
              setName(event.currentTarget.value)
            }}
          />
        </label>
        <label
          className="relative flex items-center"
          onClick={event => {
            event.preventDefault()
          }}
        >
          <div className="text w-modal-label">Color</div>
          <div className="grow">
            <ColorPicker setColor={setColor} />
          </div>
        </label>
        <div className="relative flex gap-buttons">
          <button disabled={!canSubmit} type="submit" className="button bg-invite text-white">
            Create
          </button>
          <button type="button" className="button bg-selected-frame" onClick={unsetModal}>
            Cancel
          </button>
        </div>
      </form>
    </Modal>
  )
}

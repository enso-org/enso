/** @file A modal for creating a new label. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import ColorPicker from '#/components/ColorPicker'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import UnstyledButton from '#/components/styled/UnstyledButton'

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
  const { getText } = textProvider.useText()
  const [value, setName] = React.useState('')
  const [color, setColor] = React.useState<backend.LChColor | null>(null)
  const labelNames = React.useMemo(
    () => new Set<string>(labels.map(label => label.value)),
    [labels]
  )
  const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])
  const leastUsedColor = React.useMemo(() => backend.leastUsedColor(labels), [labels])
  const canSubmit = Boolean(value && !labelNames.has(value))

  const doSubmit = () => {
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
          doSubmit()
        }}
      >
        <aria.Heading level={2} className="relative text-sm font-semibold">
          {getText('newLabel')}
        </aria.Heading>
        <FocusArea direction="horizontal">
          {(ref, innerProps) => (
            <aria.Label ref={ref} className="relative flex items-center" {...innerProps}>
              <div className="text w-modal-label">{getText('name')}</div>
              <FocusRing>
                <aria.Input
                  autoFocus
                  size={1}
                  placeholder={getText('labelNamePlaceholder')}
                  className={`focus-child text grow rounded-full border border-primary/10 bg-transparent px-input-x ${
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
              </FocusRing>
            </aria.Label>
          )}
        </FocusArea>
        <FocusArea direction="horizontal">
          {(ref, innerProps) => (
            <aria.RadioGroup ref={ref} className="relative flex items-center" {...innerProps}>
              <aria.Label className="text w-modal-label">{getText('color')}</aria.Label>
              <div className="grow">
                <ColorPicker setColor={setColor} />
              </div>
            </aria.RadioGroup>
          )}
        </FocusArea>
        <ButtonRow>
          <UnstyledButton
            isDisabled={!canSubmit}
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

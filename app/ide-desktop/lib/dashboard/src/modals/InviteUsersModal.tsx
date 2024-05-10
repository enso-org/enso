/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import isEmail from 'validator/es/lib/isEmail'

import CrossIcon from 'enso-assets/cross.svg'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as focusHooks from '#/hooks/focusHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

// =================
// === Constants ===
// =================

/** The minimum width of the input for adding a new email. */
const MIN_EMAIL_INPUT_WIDTH = 128

// =============
// === Email ===
// =============

/** Props for an {@link Email}. */
interface InternalEmailProps {
  readonly email: string
  readonly isValid: boolean
  readonly doDelete: () => void
}

/** A self-validating email display. */
function Email(props: InternalEmailProps) {
  const { email, isValid, doDelete } = props
  const focusChildProps = focusHooks.useFocusChild()

  return (
    <div
      // This UI element does not appear anywhere else.
      // eslint-disable-next-line no-restricted-syntax
      className={`m-0.5 inline-flex items-center gap-0.5 rounded-full px-1 py-0.5 ${
        isValid ? 'bg-dim/5' : 'bg-red-400/25 text-red-900'
      }`}
    >
      <span {...focusChildProps}>{email}</span>{' '}
      <div
        {...aria.mergeProps<JSX.IntrinsicElements['div']>()(focusChildProps, {
          role: 'button',
          className: 'flex cursor-pointer rounded-full transition-colors hover:bg-primary/10',
          onClick: doDelete,
        })}
      >
        <SvgMask src={CrossIcon} />
      </div>
    </div>
  )
}

/** Props for a {@link EmailInput}. */
interface InternalEmailInputProps {
  readonly doAdd: (value: string) => void
  readonly doDelete: () => void
}

/** An input for entering an email. */
function EmailInput(props: InternalEmailInputProps) {
  const { doAdd, doDelete } = props
  const { getText } = textProvider.useText()
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  const doSubmit = (element: HTMLInputElement, force = false) => {
    const value = element.value + (force ? ' ' : '')
    if (/ /.test(value)) {
      const parts = value.split(' ')
      for (const newPart of parts.slice(0, -1).filter(part => part !== '')) {
        doAdd(newPart)
      }
      element.value = parts[parts.length - 1] ?? ''
      element.style.width = `${MIN_EMAIL_INPUT_WIDTH}px`
    } else {
      element.style.width = '0px'
      const contentWidth = element.scrollWidth
      element.style.width = `${Math.max(contentWidth, MIN_EMAIL_INPUT_WIDTH)}px`
    }
  }

  return (
    <FocusRing>
      <aria.Input
        autoFocus
        type="text"
        placeholder={getText('typeEmailToInvite')}
        className="text max-w-full rounded-full bg-transparent px-input-x"
        onKeyDown={event => {
          if (
            event.key === 'Backspace' &&
            event.currentTarget.selectionStart === 0 &&
            event.currentTarget.selectionEnd === 0
          ) {
            doDelete()
          } else if (event.key === 'Enter' && event.currentTarget.value !== '') {
            event.stopPropagation()
            doSubmit(event.currentTarget, true)
          } else {
            handleFocusMove(event)
          }
        }}
        onInput={event => {
          event.stopPropagation()
          doSubmit(event.currentTarget)
        }}
      />
    </FocusRing>
  )
}

// ========================
// === InviteUsersModal ===
// ========================

/** Props for an {@link InviteUsersModal}. */
export interface InviteUsersModalProps {
  readonly backend: Backend
  /** If this is absent, this modal will be centered. */
  readonly event?: Pick<React.MouseEvent, 'pageX' | 'pageY'>
}

/** A modal for inviting one or more users. */
export default function InviteUsersModal(props: InviteUsersModalProps) {
  const { backend, event: positionEvent } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [newEmails, setNewEmails] = React.useState<string[]>([])
  const members = asyncEffectHooks.useAsyncEffect([], () => backend.listUsers(), [backend])
  const existingEmails = React.useMemo(
    () => new Set(members.map<string>(member => member.email)),
    [members]
  )
  const canSubmit = React.useMemo(
    () =>
      newEmails.length > 0 &&
      newEmails.every(
        (newEmail, i) =>
          isEmail(newEmail) && !existingEmails.has(newEmail) && newEmails.indexOf(newEmail) === i
      ),
    [existingEmails, newEmails]
  )

  const doSubmit = () => {
    unsetModal()
    if (user != null) {
      for (const newEmail of newEmails) {
        void (async () => {
          try {
            await backend.inviteUser({
              organizationId: user.organizationId,
              userEmail: backendModule.EmailAddress(newEmail),
            })
          } catch (error) {
            toastAndLog('couldNotInviteUser', error, newEmail)
          }
        })()
      }
    }
  }

  return (
    <Modal
      centered={positionEvent == null}
      className="absolute left top size-full overflow-hidden bg-dim"
    >
      <div
        tabIndex={-1}
        style={positionEvent == null ? {} : { left: positionEvent.pageX, top: positionEvent.pageY }}
        className="sticky w-invite-users-modal rounded-default before:absolute before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={mouseEvent => {
          mouseEvent.stopPropagation()
        }}
        onContextMenu={mouseEvent => {
          mouseEvent.stopPropagation()
          mouseEvent.preventDefault()
        }}
      >
        <div className="relative flex flex-col gap-modal rounded-default p-modal-wide pt-modal">
          <aria.Heading level={2} className="text text-sm font-bold">
            {getText('invite')}
          </aria.Heading>
          <form
            className="grow"
            onSubmit={event => {
              event.preventDefault()
              if (canSubmit) {
                doSubmit()
              }
            }}
          >
            <FocusArea direction="horizontal">
              {innerProps => (
                <div
                  className="block min-h-paragraph-input rounded-default border border-primary/10 p-multiline-input"
                  {...innerProps}
                >
                  {Array.from(newEmails, (newEmail, i) => (
                    <Email
                      key={i}
                      email={newEmail}
                      isValid={
                        isEmail(newEmail) &&
                        !existingEmails.has(newEmail) &&
                        newEmails.indexOf(newEmail) === i
                      }
                      doDelete={() => {
                        setNewEmails([...newEmails.slice(0, i), ...newEmails.slice(i + 1)])
                      }}
                    />
                  ))}
                  <EmailInput
                    doAdd={value => {
                      setNewEmails(emails => [...emails, value])
                    }}
                    doDelete={() => {
                      setNewEmails(emails => emails.slice(0, -1))
                    }}
                  />
                </div>
              )}
            </FocusArea>
          </form>
          <ButtonRow>
            <UnstyledButton
              isDisabled={!canSubmit}
              className="button bg-invite text-tag-text enabled:active"
              onPress={doSubmit}
            >
              {getText('invite')}
            </UnstyledButton>
          </ButtonRow>
        </div>
      </div>
    </Modal>
  )
}

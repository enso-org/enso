/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import isEmail from 'validator/es/lib/isEmail'

import CrossIcon from 'enso-assets/cross.svg'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import Modal from '#/components/Modal'

import * as backendModule from '#/services/Backend'

// =================
// === Constants ===
// =================

/** The minimum width of the input for adding a new email. */
const MIN_EMAIL_INPUT_WIDTH = 120

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
  return (
    <div
      // This UI element does not appear anywhere else.
      // eslint-disable-next-line no-restricted-syntax
      className={`inline-flex gap-0.5 items-center rounded-full py-0.5 px-1 m-0.5 ${
        isValid ? 'bg-dim/5' : 'bg-red-400/25 text-red-900'
      }`}
    >
      {email}{' '}
      <img
        className="rounded-full cursor-pointer hover:brightness-50"
        src={CrossIcon}
        onClick={doDelete}
      />
    </div>
  )
}

// ========================
// === InviteUsersModal ===
// ========================

/** Props for an {@link InviteUsersModal}. */
export interface InviteUsersModalProps {
  /** If this is `null`, this modal will be centered. */
  readonly eventTarget: HTMLElement | null
}

/** A modal for inviting one or more users. */
export default function InviteUsersModal(props: InviteUsersModalProps) {
  const { eventTarget } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { backend } = backendProvider.useBackend()
  const { unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [newEmails, setNewEmails] = React.useState<string[]>([])
  const [email, setEmail] = React.useState<string>('')
  const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
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
              organizationId: user.id,
              userEmail: backendModule.EmailAddress(newEmail),
            })
          } catch (error) {
            toastAndLog(`Could not invite user '${newEmail}'`, error)
          }
        })()
      }
    }
  }

  return (
    <Modal
      centered={eventTarget == null}
      className="absolute overflow-hidden bg-dim size-full top left"
    >
      <div
        tabIndex={-1}
        style={
          position != null
            ? { left: position.left + window.scrollX, top: position.top + window.scrollY }
            : {}
        }
        className="sticky w-invite-users-modal rounded-default before:absolute before:bg-selected-frame before:backdrop-blur-default before:rounded-default before:w-full before:h-full"
        onClick={mouseEvent => {
          mouseEvent.stopPropagation()
        }}
        onContextMenu={mouseEvent => {
          mouseEvent.stopPropagation()
          mouseEvent.preventDefault()
        }}
        onKeyDown={event => {
          if (event.key !== 'Escape') {
            event.stopPropagation()
          }
        }}
      >
        <div className="relative flex flex-col rounded-default gap-modal p-modal-wide pt-modal">
          <h2 className="text text-sm font-bold">Invite</h2>
          <form
            className="grow"
            onSubmit={event => {
              event.preventDefault()
              if (email !== '') {
                setNewEmails([...newEmails, email])
                setEmail('')
              } else if (canSubmit) {
                doSubmit()
              }
            }}
          >
            <label className="block min-h-paragraph-input rounded-default border border-black/10 p-multiline-input">
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
              <input
                autoFocus
                type="text"
                placeholder="Type email to invite"
                className="text bg-transparent max-w-full"
                value={email}
                onKeyDown={event => {
                  if (
                    event.key === 'Backspace' &&
                    event.currentTarget.selectionStart === 0 &&
                    event.currentTarget.selectionEnd === 0
                  ) {
                    setNewEmails(newEmails.slice(0, -1))
                  }
                }}
                onInput={event => {
                  const element = event.currentTarget
                  const value = element.value
                  if (/ /.test(value)) {
                    const parts = value.split(' ')
                    setNewEmails([...newEmails, ...parts.slice(0, -1).filter(part => part !== '')])
                    setEmail(parts[parts.length - 1] ?? '')
                    element.style.width = `${MIN_EMAIL_INPUT_WIDTH}px`
                  } else {
                    setEmail(value)
                    element.style.width = '0px'
                    const contentWidth = element.scrollWidth
                    element.style.width = `${Math.max(contentWidth, MIN_EMAIL_INPUT_WIDTH)}px`
                  }
                }}
              />
            </label>
          </form>
          <div className="self-start">
            <button
              type="submit"
              disabled={!canSubmit}
              className="button/30 text-tag-text bg-invite"
              onClick={doSubmit}
            >
              Invite
            </button>
          </div>
        </div>
      </div>
    </Modal>
  )
}

/** @file A modal with inputs for user email and permission level. */
import * as React from 'react'

import isEmail from 'validator/es/lib/isEmail'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/backend'

import Modal from '#/components/Modal'

// ==============================
// === ManagePermissionsModal ===
// ==============================

/** Props for an {@link InviteUsersModal}. */
export interface InviteUsersModalProps {
  /** If this is `null`, this modal will be centered. */
  eventTarget: HTMLElement | null
}

/** A modal for inviting one or more users. */
export default function InviteUsersModal(props: InviteUsersModalProps) {
  const { eventTarget } = props
  const { organization } = authProvider.useNonPartialUserSession()
  const { backend } = backendProvider.useBackend()
  const { unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [newEmails, setNewEmails] = React.useState(new Set<string>())
  const [email, setEmail] = React.useState<string>('')
  const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
  const members = asyncEffectHooks.useAsyncEffect([], () => backend.listUsers(), [backend])
  const existingEmails = React.useMemo(
    () => new Set(members.map<string>(member => member.email)),
    [members]
  )
  const invalidEmailError = React.useMemo(
    () =>
      email === ''
        ? 'Email is blank'
        : !isEmail(email)
        ? `'${email}' is not a valid email`
        : existingEmails.has(email)
        ? `'${email}' is already in the organization`
        : newEmails.has(email)
        ? `You are already adding '${email}'`
        : null,
    [email, existingEmails, newEmails]
  )
  const isEmailValid = invalidEmailError == null

  const doAddEmail = () => {
    if (!isEmailValid) {
      toastAndLog(invalidEmailError)
    } else {
      setNewEmails(oldNewEmails => new Set([...oldNewEmails, email]))
      setEmail('')
    }
  }

  const doSubmit = () => {
    unsetModal()
    if (organization != null) {
      for (const newEmail of newEmails) {
        void (async () => {
          try {
            await backend.inviteUser({
              organizationId: organization.id,
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
      className="absolute overflow-hidden bg-dim w-full h-full top-0 left-0"
    >
      <div
        tabIndex={-1}
        style={
          position != null
            ? {
                left: position.left + window.scrollX,
                top: position.top + window.scrollY,
              }
            : {}
        }
        className="sticky w-115.25 rounded-2xl before:absolute before:bg-frame-selected before:backdrop-blur-3xl before:rounded-2xl before:w-full before:h-full"
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
        <div className="relative flex flex-col rounded-2xl gap-2 p-2">
          <div>
            <h2 className="text-sm font-bold">Invite</h2>
            {/* Space reserved for other tabs. */}
          </div>
          <form
            className="flex gap-1"
            onSubmit={event => {
              event.preventDefault()
              doAddEmail()
            }}
          >
            <div className="flex items-center grow rounded-full border border-black/10 gap-2 px-2">
              <input
                type="text"
                placeholder="Type email to invite"
                className="w-full bg-transparent"
                value={email}
                onInput={event => {
                  setEmail(event.currentTarget.value)
                }}
              />
            </div>
            <button
              type="submit"
              disabled={!isEmailValid}
              {...(!isEmailValid ? { title: invalidEmailError } : {})}
              className="text-tag-text bg-invite rounded-full whitespace-nowrap px-4 py-1 disabled:opacity-30"
            >
              <div className="h-6 py-0.5">Add User</div>
            </button>
          </form>
          <ul className="flex flex-col px-1">
            {[...newEmails].map(newEmail => (
              <li key={newEmail} className="h-6 leading-5 py-px">
                {newEmail}
              </li>
            ))}
          </ul>
          <div className="self-start">
            <button
              type="submit"
              disabled={!isEmailValid && email !== ''}
              {...(!isEmailValid && email !== '' ? { title: invalidEmailError } : {})}
              className="text-tag-text bg-invite rounded-full px-4 py-1 disabled:opacity-30"
              onClick={() => {
                doSubmit()
              }}
            >
              <div className="h-6 py-0.5">Invite All</div>
            </button>
          </div>
        </div>
      </div>
    </Modal>
  )
}

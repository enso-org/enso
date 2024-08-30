/** @file A list of members in the organization. */
import * as React from 'react'

import * as mimeTypes from '#/data/mimeTypes'

import * as backendHooks from '#/hooks/backendHooks'
import * as scrollHooks from '#/hooks/scrollHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import UserRow from '#/layouts/Settings/UserRow'

import * as aria from '#/components/aria'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// ====================
// === MembersTable ===
// ====================

/** Props for a {@link MembersTable}. */
export interface MembersTableProps {
  readonly backend: Backend
  /** If `true`, initialize the users list with self to avoid needing a loading spinner. */
  readonly populateWithSelf?: boolean
  readonly draggable?: boolean
  readonly allowDelete?: boolean
}

/** A list of members in the organization. */
export default function MembersTable(props: MembersTableProps) {
  const { backend, populateWithSelf = false, draggable = false, allowDelete = false } = props
  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [selectedKeys, setSelectedKeys] = React.useState<aria.Selection>(new Set())
  const rootRef = React.useRef<HTMLTableElement>(null)
  const scrollContainerRef = React.useRef<HTMLDivElement>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)
  const userWithPlaceholder = React.useMemo(() => ({ isPlaceholder: false, ...user }), [user])

  const backendListUsers = backendHooks.useListUsers(backend)

  const users = React.useMemo(
    () => backendListUsers ?? (populateWithSelf ? [userWithPlaceholder] : null),
    [backendListUsers, populateWithSelf, userWithPlaceholder],
  )
  const usersMap = React.useMemo(
    () => new Map((users ?? []).map((member) => [member.userId, member])),
    [users],
  )

  const { onScroll, shadowClassName } = scrollHooks.useStickyTableHeaderOnScroll(
    scrollContainerRef,
    bodyRef,
    { trackShadowClass: true },
  )

  const { dragAndDropHooks } = aria.useDragAndDrop({
    getItems: (keys) =>
      [...keys].flatMap((key) => {
        const userId = backendModule.UserId(String(key))
        const member = usersMap.get(userId)
        return member != null ? [{ [mimeTypes.USER_MIME_TYPE]: JSON.stringify(member) }] : []
      }),
    renderDragPreview: (items) => {
      return (
        <div className="flex flex-col rounded-default bg-white backdrop-blur-default">
          {items.flatMap((item) => {
            const payload = item[mimeTypes.USER_MIME_TYPE]
            if (payload == null) {
              return []
            } else {
              // This is SAFE. The type of the payload is known as it is set in `getItems` above.
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const member: backendModule.User = JSON.parse(payload)
              return [
                <div key={member.userId} className="flex h-row items-center px-cell-x">
                  <aria.Text className="text">{member.name}</aria.Text>
                </div>,
              ]
            }
          })}
        </div>
      )
    },
  })

  React.useEffect(() => {
    const onClick = (event: Event) => {
      if (event.target instanceof Node && rootRef.current?.contains(event.target) === false) {
        setSelectedKeys(new Set())
      }
    }
    document.addEventListener('click', onClick, { capture: true })
    return () => {
      document.removeEventListener('click', onClick, { capture: true })
    }
  }, [])

  const doDeleteUser = async (userToDelete: backendModule.User) => {
    try {
      await Promise.resolve()
      throw new Error('Not implemented yet')
    } catch (error) {
      toastAndLog('deleteUserError', error, userToDelete.name)
      return
    }
  }

  return (
    <div
      ref={scrollContainerRef}
      className={tailwindMerge.twMerge('overflow-auto overflow-x-hidden', shadowClassName)}
      onScroll={onScroll}
    >
      <aria.Table
        ref={rootRef}
        aria-label={getText('users')}
        selectionMode={draggable ? 'multiple' : 'none'}
        selectionBehavior="replace"
        selectedKeys={selectedKeys}
        onSelectionChange={setSelectedKeys}
        className="w-settings-main-section max-w-full table-fixed self-start rounded-rows"
        {...(draggable ? { dragAndDropHooks } : {})}
      >
        <aria.TableHeader className="sticky top h-row">
          <aria.Column
            isRowHeader
            className="w-48 border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
          >
            {getText('name')}
          </aria.Column>
          <aria.Column className="w-48 border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
            {getText('email')}
          </aria.Column>
          {/* Delete button. */}
          {allowDelete && <aria.Column className="w border-0" />}
        </aria.TableHeader>
        <aria.TableBody
          ref={bodyRef}
          items={users ?? []}
          dependencies={[users]}
          className="select-text"
        >
          {(member) => (
            <UserRow
              id={member.userId}
              draggable={draggable}
              user={member}
              doDeleteUser={!allowDelete ? null : doDeleteUser}
            />
          )}
        </aria.TableBody>
      </aria.Table>
    </div>
  )
}

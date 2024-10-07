/** @file A list of members in the organization. */
import { useEffect, useMemo, useRef, useState } from 'react'

import {
  Column,
  Table,
  TableBody,
  TableHeader,
  Text,
  useDragAndDrop,
  type Selection,
} from '#/components/aria'
import { USER_MIME_TYPE } from '#/data/mimeTypes'
import { useBackendQuery } from '#/hooks/backendHooks'
import { useStickyTableHeaderOnScroll } from '#/hooks/scrollHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { UserId, type User } from '#/services/Backend'
import { twMerge } from '#/utilities/tailwindMerge'
import UserRow from './UserRow'

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
  const { user } = useFullUserSession()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const [selectedKeys, setSelectedKeys] = useState<Selection>(new Set())
  const rootRef = useRef<HTMLTableElement>(null)
  const scrollContainerRef = useRef<HTMLDivElement>(null)
  const bodyRef = useRef<HTMLTableSectionElement>(null)
  const userWithPlaceholder = useMemo(() => ({ isPlaceholder: false, ...user }), [user])

  const { data: allUsers } = useBackendQuery(backend, 'listUsers', [])

  const users = useMemo(
    () => allUsers ?? (populateWithSelf ? [userWithPlaceholder] : null),
    [allUsers, populateWithSelf, userWithPlaceholder],
  )
  const usersMap = useMemo(
    () => new Map((users ?? []).map((member) => [member.userId, member])),
    [users],
  )

  const { onScroll, shadowClassName } = useStickyTableHeaderOnScroll(scrollContainerRef, bodyRef, {
    trackShadowClass: true,
  })

  const { dragAndDropHooks } = useDragAndDrop({
    getItems: (keys) =>
      [...keys].flatMap((key) => {
        const userId = UserId(String(key))
        const member = usersMap.get(userId)
        return member != null ? [{ [USER_MIME_TYPE]: JSON.stringify(member) }] : []
      }),
    renderDragPreview: (items) => {
      return (
        <div className="flex flex-col rounded-default bg-white backdrop-blur-default">
          {items.flatMap((item) => {
            const payload = item[USER_MIME_TYPE]
            if (payload == null) {
              return []
            } else {
              // This is SAFE. The type of the payload is known as it is set in `getItems` above.
              // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
              const member: User = JSON.parse(payload)
              return [
                <div key={member.userId} className="flex h-row items-center px-cell-x">
                  <Text className="text">{member.name}</Text>
                </div>,
              ]
            }
          })}
        </div>
      )
    },
  })

  useEffect(() => {
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

  const doDeleteUser = async (userToDelete: User) => {
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
      className={twMerge('overflow-auto overflow-x-hidden', shadowClassName)}
      onScroll={onScroll}
    >
      <Table
        ref={rootRef}
        aria-label={getText('users')}
        selectionMode={draggable ? 'multiple' : 'none'}
        selectionBehavior="replace"
        selectedKeys={selectedKeys}
        onSelectionChange={setSelectedKeys}
        className="w-settings-main-section max-w-full table-fixed self-start rounded-rows"
        {...(draggable ? { dragAndDropHooks } : {})}
      >
        <TableHeader className="sticky top h-row">
          <Column
            isRowHeader
            className="w-48 border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
          >
            {getText('name')}
          </Column>
          <Column className="w-48 border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
            {getText('email')}
          </Column>
          {/* Delete button. */}
          {allowDelete && <Column className="w border-0" />}
        </TableHeader>
        <TableBody ref={bodyRef} items={users ?? []} dependencies={[users]} className="select-text">
          {(member) => (
            <UserRow
              id={member.userId}
              draggable={draggable}
              user={member}
              doDeleteUser={!allowDelete ? null : doDeleteUser}
            />
          )}
        </TableBody>
      </Table>
    </div>
  )
}

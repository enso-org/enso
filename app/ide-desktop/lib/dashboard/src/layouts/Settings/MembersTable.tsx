/** @file A list of members in the organization. */
import * as React from 'react'

import * as mimeTypes from '#/data/mimeTypes'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import UserRow from '#/layouts/Settings/UserRow'

import * as aria from '#/components/aria'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import * as backendModule from '#/services/Backend'

// ====================
// === MembersTable ===
// ====================

/** Props for a {@link MembersTable}. */
export interface MembersTableProps {
  readonly draggable?: boolean
  readonly allowDelete?: boolean
}

/** A list of members in the organization. */
export default function MembersTable(props: MembersTableProps) {
  const { draggable = false, allowDelete = false } = props
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [selectedKeys, setSelectedKeys] = React.useState<aria.Selection>(new Set())
  const rootRef = React.useRef<HTMLTableElement | null>(null)
  const members = asyncEffectHooks.useAsyncEffect(null, () => backend.listUsers(), [backend])
  const membersMap = React.useMemo(
    () => new Map((members ?? []).map(member => [member.userId, member])),
    [members]
  )
  const isLoading = members == null
  const { dragAndDropHooks } = aria.useDragAndDrop({
    getItems: keys =>
      [...keys].flatMap(key => {
        const userId = backendModule.UserId(String(key))
        const member = membersMap.get(userId)
        return member != null ? [{ [mimeTypes.USER_MIME_TYPE]: JSON.stringify(member) }] : []
      }),
    renderDragPreview: items => {
      return (
        <div className="flex flex-col rounded-default bg-white backdrop-blur-default">
          {items.flatMap(item => {
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

  const doDeleteUser = async (user: backendModule.User) => {
    try {
      await Promise.resolve()
      throw new Error('Not implemented yet')
    } catch (error) {
      toastAndLog('deleteUserError', error, user.name)
      return
    }
  }

  return (
    <aria.Table
      ref={rootRef}
      aria-label={getText('users')}
      selectionMode={draggable ? 'multiple' : 'none'}
      selectionBehavior="replace"
      selectedKeys={selectedKeys}
      onSelectionChange={setSelectedKeys}
      className="w-full table-fixed self-start rounded-rows"
      {...(draggable ? { dragAndDropHooks } : {})}
    >
      <aria.TableHeader className="h-row">
        <aria.Column
          isRowHeader
          className="w-members-name-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0"
        >
          {getText('name')}
        </aria.Column>
        <aria.Column className="w-members-email-column border-x-2 border-transparent bg-clip-padding px-cell-x text-left text-sm font-semibold last:border-r-0">
          {getText('email')}
        </aria.Column>
        {/* Delete button. */}
        {allowDelete && <aria.Column className="border-0" />}
      </aria.TableHeader>
      <aria.TableBody items={members ?? []} dependencies={[members]} className="select-text">
        {isLoading ? (
          <aria.Row className="h-row">
            <aria.Cell
              ref={element => {
                if (element != null) {
                  element.colSpan = allowDelete ? 3 : 2
                }
              }}
              className="rounded-full bg-transparent"
            >
              <div className="flex justify-center">
                <StatelessSpinner size={32} state={statelessSpinner.SpinnerState.loadingMedium} />
              </div>
            </aria.Cell>
          </aria.Row>
        ) : (
          member => (
            <UserRow
              id={member.userId}
              draggable={draggable}
              user={member}
              doDeleteUser={doDeleteUser}
            />
          )
        )}
      </aria.TableBody>
    </aria.Table>
  )
}

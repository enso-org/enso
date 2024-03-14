/** @file A list of selectable labels. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'
import Trash2Icon from 'enso-assets/trash2.svg'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import Label from '#/components/dashboard/Label'
import * as labelUtils from '#/components/dashboard/Label/labelUtils'
import SvgMask from '#/components/SvgMask'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import DragModal from '#/modals/DragModal'
import NewLabelModal from '#/modals/NewLabelModal'

import type * as backend from '#/services/Backend'

import * as array from '#/utilities/array'
import type AssetQuery from '#/utilities/AssetQuery'
import * as drag from '#/utilities/drag'
import * as string from '#/utilities/string'

// ==============
// === Labels ===
// ==============

/** Props for a {@link Labels}. */
export interface LabelsProps {
  readonly labels: backend.Label[]
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly doCreateLabel: (name: string, color: backend.LChColor) => void
  readonly doDeleteLabel: (id: backend.TagId, name: backend.LabelName) => void
  readonly newLabelNames: Set<backend.LabelName>
  readonly deletedLabelNames: Set<backend.LabelName>
}

/** A list of selectable labels. */
export default function Labels(props: LabelsProps) {
  const { labels, query, setQuery, doCreateLabel, doDeleteLabel, newLabelNames } = props
  const { deletedLabelNames } = props
  const currentLabels = query.labels
  const currentNegativeLabels = query.negativeLabels
  const { setModal } = modalProvider.useSetModal()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const rootRef = React.useRef<HTMLDivElement>(null)
  const displayLabels = React.useMemo(
    () =>
      labels
        .filter(label => !deletedLabelNames.has(label.value))
        .sort((a, b) => string.compareCaseInsensitive(a.value, b.value)),
    [deletedLabelNames, labels]
  )

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, {
      length: labels.length,
    })

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      navigator2D.register(root, {
        focusPrimaryChild: setKeyboardSelectedIndex.bind(null, 0),
        focusWhenPressed: {
          up: setKeyboardSelectedIndex.bind(null, displayLabels.length - 1),
        },
      })
      return () => {
        navigator2D.unregister(root)
      }
    }
  }, [displayLabels.length, setKeyboardSelectedIndex, navigator2D])

  return (
    <div
      data-testid="labels"
      ref={rootRef}
      className="flex w-full flex-col items-start gap-sidebar-section-heading"
    >
      <div className="text-header px-sidebar-section-heading-x text-sm font-bold">Labels</div>
      <ul data-testid="labels-list" className="flex flex-col items-start gap-labels">
        {displayLabels.map((label, i) => {
          const negated = currentNegativeLabels.some(term =>
            array.shallowEqual(term, [label.value])
          )
          return (
            <li key={label.id} className="group flex items-center gap-label-icons">
              <Label
                ref={element => {
                  if (keyboardSelectedIndex === i) {
                    element?.focus()
                  }
                }}
                draggable
                focusRing={i === keyboardSelectedIndex}
                color={label.color}
                active={
                  negated || currentLabels.some(term => array.shallowEqual(term, [label.value]))
                }
                negated={negated}
                disabled={newLabelNames.has(label.value)}
                onClick={event => {
                  setQuery(oldQuery =>
                    oldQuery.withToggled('labels', 'negativeLabels', label.value, event.shiftKey)
                  )
                }}
                onDragStart={event => {
                  drag.setDragImageToBlank(event)
                  const payload: drag.LabelsDragPayload = new Set([label.value])
                  drag.LABELS.bind(event, payload)
                  setModal(
                    <DragModal
                      event={event}
                      doCleanup={() => {
                        drag.LABELS.unbind(payload)
                      }}
                    >
                      <Label active color={label.color} onClick={() => {}}>
                        {label.value}
                      </Label>
                    </DragModal>
                  )
                }}
              >
                {label.value}
              </Label>
              {!newLabelNames.has(label.value) && (
                <button
                  className="flex"
                  onClick={event => {
                    event.stopPropagation()
                    setModal(
                      <ConfirmDeleteModal
                        actionText={`delete the label '${label.value}'`}
                        doDelete={() => {
                          doDeleteLabel(label.id, label.value)
                        }}
                      />
                    )
                  }}
                >
                  <SvgMask
                    src={Trash2Icon}
                    alt="Delete"
                    className="size-icon text-delete transition-all transparent group-hover:active"
                  />
                </button>
              )}
            </li>
          )
        })}
        <li>
          <Label
            color={labelUtils.DEFAULT_LABEL_COLOR}
            className="bg-selected-frame"
            onClick={event => {
              event.stopPropagation()
              setModal(
                <NewLabelModal
                  labels={labels}
                  eventTarget={event.currentTarget}
                  doCreate={doCreateLabel}
                />
              )
            }}
          >
            {/* This is a non-standard-sized icon. */}
            {/* eslint-disable-next-line no-restricted-syntax */}
            <img src={PlusIcon} className="mr-[6px] size-[6px]" />
            <span className="text-header">new label</span>
          </Label>
        </li>
      </ul>
    </div>
  )
}

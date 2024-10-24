/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import FindIcon from '#/assets/find.svg'

import * as backendHooks from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Label from '#/components/dashboard/Label'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

import type Backend from '#/services/Backend'

import { useSuggestions } from '#/providers/DriveProvider'
import * as array from '#/utilities/array'
import AssetQuery from '#/utilities/AssetQuery'
import * as eventModule from '#/utilities/event'
import * as string from '#/utilities/string'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =============
// === Types ===
// =============

/** The reason behind a new query. */
enum QuerySource {
  /**
   * A query change initiated by tabbing. While *technically* internal, it is semantically
   * different in that tabbing does not update the base query.
   */
  tabbing = 'tabbing',
  /** A query change initiated from code in this component. */
  internal = 'internal',
  /** A query change initiated by typing in the search bar. */
  typing = 'typing',
  /** A query change initiated from code in another component. */
  external = 'external',
}

/** A suggested query. */
export interface Suggestion {
  readonly render: () => React.ReactNode
  readonly addToQuery: (query: AssetQuery) => AssetQuery
  readonly deleteFromQuery: (query: AssetQuery) => AssetQuery
}

// ============
// === Tags ===
// ============

/** Props for a {@link Tags}. */
interface InternalTagsProps {
  readonly isCloud: boolean
  readonly querySource: React.MutableRefObject<QuerySource>
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
}

/** Tags (`name:`, `modified:`, etc.) */
function Tags(props: InternalTagsProps) {
  const { isCloud, querySource, query, setQuery } = props
  const [isShiftPressed, setIsShiftPressed] = React.useState(false)

  React.useEffect(() => {
    const onKeyDown = (event: KeyboardEvent) => {
      setIsShiftPressed(event.shiftKey)
    }
    const onKeyUp = (event: KeyboardEvent) => {
      setIsShiftPressed(event.shiftKey)
    }
    document.addEventListener('keydown', onKeyDown)
    document.addEventListener('keyup', onKeyUp)
    return () => {
      document.removeEventListener('keydown', onKeyDown)
      document.removeEventListener('keyup', onKeyUp)
    }
  }, [])

  return (
    <div
      data-testid="asset-search-tag-names"
      className="pointer-events-auto flex flex-wrap gap-2 whitespace-nowrap px-1.5"
    >
      {(isCloud ? AssetQuery.tagNames : AssetQuery.localTagNames).flatMap((entry) => {
        const [key, tag] = entry
        return tag == null || isShiftPressed !== tag.startsWith('-') ?
            []
          : [
              <FocusRing key={key}>
                <ariaComponents.Button
                  variant="outline"
                  size="xsmall"
                  className="min-w-12"
                  onPress={() => {
                    querySource.current = QuerySource.internal
                    setQuery(query.add({ [key]: [[]] }))
                  }}
                >
                  {tag + ':'}
                </ariaComponents.Button>
              </FocusRing>,
            ]
      })}
    </div>
  )
}

// ======================
// === AssetSearchBar ===
// ======================

/** Props for a {@link AssetSearchBar}. */
export interface AssetSearchBarProps {
  readonly backend: Backend | null
  readonly isCloud: boolean
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
}

/** A search bar containing a text input, and a list of suggestions. */
export default function AssetSearchBar(props: AssetSearchBarProps) {
  const { backend, isCloud, query, setQuery } = props
  const { getText } = textProvider.useText()
  const { modalRef } = modalProvider.useModalRef()
  /** A cached query as of the start of tabbing. */
  const baseQuery = React.useRef(query)
  const rawSuggestions = useSuggestions()
  const [suggestions, setSuggestions] = React.useState(rawSuggestions)
  const suggestionsRef = React.useRef(rawSuggestions)
  const [selectedIndices, setSelectedIndices] = React.useState<ReadonlySet<number>>(
    new Set<number>(),
  )
  const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)
  const [areSuggestionsVisible, setAreSuggestionsVisible] = React.useState(false)
  const areSuggestionsVisibleRef = React.useRef(areSuggestionsVisible)
  const querySource = React.useRef(QuerySource.external)
  const rootRef = React.useRef<HTMLLabelElement | null>(null)
  const searchRef = React.useRef<HTMLInputElement | null>(null)
  const labels = backendHooks.useBackendQuery(backend, 'listTags', []).data ?? []
  areSuggestionsVisibleRef.current = areSuggestionsVisible

  React.useEffect(() => {
    if (querySource.current !== QuerySource.tabbing) {
      baseQuery.current = query
    }
    // This effect MUST only run when `query` changes.
  }, [query])

  React.useEffect(() => {
    if (querySource.current !== QuerySource.tabbing) {
      setSelectedIndex(null)
    }
    if (
      querySource.current !== QuerySource.internal &&
      querySource.current !== QuerySource.tabbing
    ) {
      if (searchRef.current != null) {
        searchRef.current.value = query.query
      }
    }
  }, [query])

  React.useEffect(() => {
    if (querySource.current !== QuerySource.tabbing) {
      setSuggestions(rawSuggestions)
      suggestionsRef.current = rawSuggestions
    }
  }, [rawSuggestions])

  React.useEffect(() => {
    if (
      querySource.current === QuerySource.internal ||
      querySource.current === QuerySource.tabbing
    ) {
      let newQuery = query
      const suggestion = selectedIndex == null ? null : suggestions[selectedIndex]
      if (suggestion != null) {
        newQuery = suggestion.addToQuery(baseQuery.current)
        setQuery(newQuery)
      }
      searchRef.current?.focus()
      const end = searchRef.current?.value.length ?? 0
      searchRef.current?.setSelectionRange(end, end)
      if (searchRef.current != null) {
        searchRef.current.value = newQuery.toString()
      }
    }
    // This effect MUST only run when `selectedIndex` changes.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [selectedIndex])

  React.useEffect(() => {
    const onSearchKeyDown = (event: KeyboardEvent) => {
      if (areSuggestionsVisibleRef.current) {
        if (event.key === 'ArrowUp' || event.key === 'ArrowDown') {
          event.preventDefault()
          event.stopImmediatePropagation()
          querySource.current = QuerySource.tabbing
          const reverse = event.key === 'ArrowUp'
          setSelectedIndex((oldIndex) => {
            const length = Math.max(1, suggestionsRef.current.length)
            if (reverse) {
              return oldIndex == null ? length - 1 : (oldIndex + length - 1) % length
            } else {
              return oldIndex == null ? 0 : (oldIndex + 1) % length
            }
          })
        }
        if (
          event.key === 'Enter' ||
          (event.key === ' ' && document.activeElement !== searchRef.current)
        ) {
          querySource.current = QuerySource.external
          if (searchRef.current != null) {
            searchRef.current.focus()
            const end = searchRef.current.value.length
            searchRef.current.setSelectionRange(end, end)
          }
        }
        if (event.key === 'Enter') {
          setAreSuggestionsVisible(false)
        }
        if (event.key === 'Escape') {
          if (querySource.current === QuerySource.tabbing) {
            querySource.current = QuerySource.external
            setQuery(baseQuery.current)
            setAreSuggestionsVisible(false)
          } else {
            searchRef.current?.blur()
          }
        }
      }
    }
    const onKeyDown = (event: KeyboardEvent) => {
      // Allow `alt` key to be pressed in case it is being used to enter special characters.
      if (
        !eventModule.isElementTextInput(event.target) &&
        (!(event.target instanceof Node) || rootRef.current?.contains(event.target) !== true) &&
        eventModule.isTextInputEvent(event) &&
        event.key !== ' ' &&
        (!detect.isOnMacOS() || event.key !== 'Delete') &&
        modalRef.current == null
      ) {
        searchRef.current?.focus()
      }
      if (
        event.target instanceof Node &&
        rootRef.current?.contains(event.target) === true &&
        eventModule.isPotentiallyShortcut(event)
      ) {
        searchRef.current?.focus()
      }
    }
    const root = rootRef.current
    root?.addEventListener('keydown', onSearchKeyDown)
    document.addEventListener('keydown', onKeyDown)
    return () => {
      root?.removeEventListener('keydown', onSearchKeyDown)
      document.removeEventListener('keydown', onKeyDown)
    }
  }, [setQuery, modalRef])

  // Reset `querySource` after all other effects have run.
  React.useEffect(() => {
    if (querySource.current !== QuerySource.typing && searchRef.current != null) {
      searchRef.current.value = query.toString()
    }
    if (querySource.current !== QuerySource.tabbing) {
      baseQuery.current = query
      querySource.current = QuerySource.external
    }
  }, [query, setQuery])

  return (
    <FocusArea direction="horizontal">
      {(innerProps) => (
        <aria.Label
          data-testid="asset-search-bar"
          {...aria.mergeProps<aria.LabelProps & React.RefAttributes<HTMLLabelElement>>()(
            innerProps,
            {
              className:
                'z-1 group relative flex grow max-w-[60em] items-center gap-asset-search-bar rounded-full px-1.5 py-1 text-primary',
              ref: rootRef,
              onFocus: () => {
                setAreSuggestionsVisible(true)
              },
              onBlur: (event) => {
                if (!event.currentTarget.contains(event.relatedTarget)) {
                  if (querySource.current === QuerySource.tabbing) {
                    querySource.current = QuerySource.external
                  }
                  setAreSuggestionsVisible(false)
                }
              },
            },
          )}
        >
          <div className="relative size-4 placeholder" />
          <div
            className={ariaComponents.DIALOG_BACKGROUND({
              className: tailwindMerge.twMerge(
                'absolute left-0 top-0 z-1 flex w-full flex-col overflow-hidden rounded-default border-0.5 border-primary/20 -outline-offset-1 outline-primary transition-colors',
                areSuggestionsVisible ? '' : 'bg-transparent',
              ),
            })}
          >
            <div className="h-[32px]" />

            <div
              className={tailwindMerge.twMerge(
                'grid transition-grid-template-rows duration-200',
                areSuggestionsVisible ? 'grid-rows-1fr' : 'grid-rows-0fr',
              )}
            >
              <div className="overflow-y-auto overflow-x-hidden">
                <div className="relative mt-3 flex flex-col gap-3">
                  {/* Tags (`name:`, `modified:`, etc.) */}
                  <Tags
                    isCloud={isCloud}
                    querySource={querySource}
                    query={query}
                    setQuery={setQuery}
                  />
                  {/* Asset labels */}
                  {isCloud && labels.length !== 0 && (
                    <div
                      data-testid="asset-search-labels"
                      className="pointer-events-auto flex gap-2 px-1.5"
                    >
                      {[...labels]
                        .sort((a, b) => string.compareCaseInsensitive(a.value, b.value))
                        .map((label) => {
                          const negated = query.negativeLabels.some((term) =>
                            array.shallowEqual(term, [label.value]),
                          )
                          return (
                            <Label
                              key={label.id}
                              color={label.color}
                              active={
                                negated ||
                                query.labels.some((term) => array.shallowEqual(term, [label.value]))
                              }
                              negated={negated}
                              onPress={(event) => {
                                querySource.current = QuerySource.internal
                                setQuery((oldQuery) => {
                                  const newQuery = oldQuery.withToggled(
                                    'labels',
                                    'negativeLabels',
                                    label.value,
                                    event.shiftKey,
                                  )
                                  baseQuery.current = newQuery
                                  return newQuery
                                })
                              }}
                            >
                              {label.value}
                            </Label>
                          )
                        })}
                    </div>
                  )}
                  {/* Suggestions */}
                  <div className="flex max-h-search-suggestions-list flex-col overflow-y-auto overflow-x-hidden pb-0.5 pl-0.5">
                    {suggestions.map((suggestion, index) => (
                      // This should not be a `<button>`, since `render()` may output a
                      // tree containing a button.
                      <aria.Button
                        data-testid="asset-search-suggestion"
                        key={index}
                        ref={(el) => {
                          if (index === selectedIndex) {
                            el?.focus()
                          }
                        }}
                        className={tailwindMerge.twMerge(
                          'flex cursor-pointer rounded-l-default rounded-r-sm px-[7px] py-0.5 text-left transition-[background-color] hover:bg-primary/5',
                          selectedIndices.has(index) && 'bg-primary/10',
                          index === selectedIndex && 'bg-selected-frame',
                        )}
                        onPress={(event) => {
                          querySource.current = QuerySource.internal
                          setQuery(
                            selectedIndices.has(index) ?
                              suggestion.deleteFromQuery(event.shiftKey ? query : baseQuery.current)
                            : suggestion.addToQuery(event.shiftKey ? query : baseQuery.current),
                          )
                          if (event.shiftKey) {
                            setSelectedIndices(
                              new Set(
                                selectedIndices.has(index) ?
                                  [...selectedIndices].filter((otherIndex) => otherIndex !== index)
                                : [...selectedIndices, index],
                              ),
                            )
                          } else {
                            setAreSuggestionsVisible(false)
                          }
                        }}
                      >
                        <ariaComponents.Text variant="body" truncate="1" className="w-full">
                          {suggestion.render()}
                        </ariaComponents.Text>
                      </aria.Button>
                    ))}
                  </div>
                </div>
              </div>
            </div>
          </div>
          <SvgMask
            src={FindIcon}
            className="absolute left-2 top-[50%] z-1 mt-[1px] -translate-y-1/2 text-primary/40"
          />
          <FocusRing placement="before">
            <aria.SearchField
              aria-label={getText('assetSearchFieldLabel')}
              className="relative grow before:text before:absolute before:-inset-x-1 before:my-auto before:rounded-full before:transition-all"
              value={query.query}
              onKeyDown={(event) => {
                event.continuePropagation()
              }}
            >
              <aria.Input
                type="search"
                ref={searchRef}
                size={1}
                placeholder={
                  isCloud ?
                    detect.isOnMacOS() ?
                      getText('remoteBackendSearchPlaceholderMacOs')
                    : getText('remoteBackendSearchPlaceholder')
                  : getText('localBackendSearchPlaceholder')
                }
                className="focus-child peer text relative z-1 w-full bg-transparent placeholder-primary/40"
                onChange={(event) => {
                  if (querySource.current !== QuerySource.internal) {
                    querySource.current = QuerySource.typing
                    setQuery(AssetQuery.fromString(event.target.value))
                  }
                }}
                onKeyDown={(event) => {
                  if (
                    event.key === 'Enter' &&
                    !event.shiftKey &&
                    !event.altKey &&
                    !event.metaKey &&
                    !event.ctrlKey
                  ) {
                    // Clone the query to refresh results.
                    setQuery(query.clone())
                  }
                }}
              />
            </aria.SearchField>
          </FocusRing>
        </aria.Label>
      )}
    </FocusArea>
  )
}

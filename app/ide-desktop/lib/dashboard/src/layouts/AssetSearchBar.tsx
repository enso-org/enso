/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'
import * as detect from 'enso-common/src/detect'

import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import Label from '#/components/dashboard/Label'

import type * as backend from '#/services/Backend'

import * as array from '#/utilities/array'
import AssetQuery from '#/utilities/AssetQuery'
import * as eventModule from '#/utilities/event'
import * as string from '#/utilities/string'

// =============
// === Types ===
// =============

/** The reason behind a new query. */
enum QuerySource {
  /** A query change initiated by tabbing. While *technically* internal, it is semantically
   * different in that tabbing does not update the base query. */
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

// ======================
// === AssetSearchBar ===
// ======================

/** Props for a {@link AssetSearchBar}. */
export interface AssetSearchBarProps {
  readonly isCloud: boolean
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly labels: backend.Label[]
  readonly suggestions: Suggestion[]
}

/** A search bar containing a text input, and a list of suggestions. */
export default function AssetSearchBar(props: AssetSearchBarProps) {
  const { isCloud, query, setQuery, labels, suggestions: rawSuggestions } = props
  const { modalRef } = modalProvider.useModalRef()
  /** A cached query as of the start of tabbing. */
  const baseQuery = React.useRef(query)
  const [suggestions, setSuggestions] = React.useState(rawSuggestions)
  const suggestionsRef = React.useRef(rawSuggestions)
  const [selectedIndices, setSelectedIndices] = React.useState<ReadonlySet<number>>(
    new Set<number>()
  )
  const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)
  const [areSuggestionsVisible, setAreSuggestionsVisible] = React.useState(false)
  const areSuggestionsVisibleRef = React.useRef(areSuggestionsVisible)
  const querySource = React.useRef(QuerySource.external)
  const [isShiftPressed, setIsShiftPressed] = React.useState(false)
  const rootRef = React.useRef<HTMLLabelElement>(null)
  const searchRef = React.useRef<HTMLInputElement>(null)
  const navigator2D = navigator2DProvider.useNavigator2D()

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      navigator2D.register(root, { primaryChild: searchRef.current })
      return () => {
        navigator2D.unregister(root)
      }
    }
  }, [navigator2D])

  React.useEffect(() => {
    areSuggestionsVisibleRef.current = areSuggestionsVisible
  }, [areSuggestionsVisible])

  React.useEffect(() => {
    if (querySource.current !== QuerySource.tabbing && !isShiftPressed) {
      baseQuery.current = query
    }
    // This effect MUST only run when `query` changes.
    // eslint-disable-next-line react-hooks/exhaustive-deps
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
    if (querySource.current !== QuerySource.tabbing && !isShiftPressed) {
      setSuggestions(rawSuggestions)
      suggestionsRef.current = rawSuggestions
    }
  }, [isShiftPressed, rawSuggestions])

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
        if (event.key === 'Tab' || event.key === 'ArrowUp' || event.key === 'ArrowDown') {
          event.preventDefault()
          event.stopImmediatePropagation()
          querySource.current = QuerySource.tabbing
          const reverse = (event.key === 'Tab' && event.shiftKey) || event.key === 'ArrowUp'
          setSelectedIndex(oldIndex => {
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
      setIsShiftPressed(event.shiftKey)
      // Allow `alt` key to be pressed in case it is being used to enter special characters.
      if (
        !(event.target instanceof HTMLInputElement) &&
        !(event.target instanceof HTMLTextAreaElement) &&
        (!(event.target instanceof HTMLElement) || !event.target.isContentEditable) &&
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
    const onKeyUp = (event: KeyboardEvent) => {
      setIsShiftPressed(event.shiftKey)
    }
    const root = rootRef.current
    root?.addEventListener('keydown', onSearchKeyDown)
    document.addEventListener('keydown', onKeyDown)
    document.addEventListener('keyup', onKeyUp)
    return () => {
      root?.removeEventListener('keydown', onSearchKeyDown)
      document.removeEventListener('keydown', onKeyDown)
      document.removeEventListener('keyup', onKeyUp)
    }
  }, [setQuery, /* should never change */ modalRef])

  // Reset `querySource` after all other effects have run.
  React.useEffect(() => {
    if (querySource.current !== QuerySource.typing && searchRef.current != null) {
      searchRef.current.value = query.toString()
    }
    if (querySource.current !== QuerySource.tabbing) {
      baseQuery.current = query
      querySource.current = QuerySource.external
    }
  }, [query, /* should never change */ setQuery])

  return (
    <label
      ref={rootRef}
      data-testid="asset-search-bar"
      tabIndex={-1}
      onFocus={() => {
        setAreSuggestionsVisible(true)
      }}
      onBlur={event => {
        if (!event.currentTarget.contains(event.relatedTarget)) {
          if (querySource.current === QuerySource.tabbing) {
            querySource.current = QuerySource.external
          }
          setAreSuggestionsVisible(false)
        }
      }}
      className="search-bar group relative flex h-row max-w-asset-search-bar grow items-center gap-asset-search-bar rounded-full px-input-x text-primary xl:max-w-asset-search-bar-wide"
    >
      <img src={FindIcon} className="relative z-1 placeholder" />
      <input
        ref={searchRef}
        type="search"
        size={1}
        placeholder={
          isCloud
            ? 'Type to search for projects, Data Links, users, and more.'
            : 'Type to search for projects.'
        }
        className="peer text relative z-1 grow bg-transparent placeholder:text-center"
        onChange={event => {
          if (querySource.current !== QuerySource.internal) {
            querySource.current = QuerySource.typing
            setQuery(AssetQuery.fromString(event.target.value))
          }
        }}
        onKeyDown={event => {
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
      <div className="pointer-events-none absolute left top flex w-full flex-col overflow-hidden rounded-default before:absolute before:inset before:bg-frame before:backdrop-blur-default">
        <div className="padding relative h-row" />
        {areSuggestionsVisible && (
          <div className="relative flex flex-col gap-search-suggestions">
            {/* Tags (`name:`, `modified:`, etc.) */}
            <div
              data-testid="asset-search-tag-names"
              className="pointer-events-auto flex flex-wrap gap-buttons whitespace-nowrap px-search-suggestions"
            >
              {(isCloud ? AssetQuery.tagNames : AssetQuery.localTagNames).flatMap(entry => {
                const [key, tag] = entry
                return tag == null || isShiftPressed !== tag.startsWith('-')
                  ? []
                  : [
                      <button
                        key={key}
                        className="h-text rounded-full bg-frame px-button-x transition-all hover:bg-selected-frame"
                        onClick={() => {
                          querySource.current = QuerySource.internal
                          setQuery(query.add({ [key]: [[]] }))
                        }}
                      >
                        {tag}:
                      </button>,
                    ]
              })}
            </div>
            {/* Asset labels */}
            {isCloud && labels.length !== 0 && (
              <div
                data-testid="asset-search-labels"
                className="pointer-events-auto flex gap-buttons p-search-suggestions"
              >
                {labels
                  .sort((a, b) => string.compareCaseInsensitive(a.value, b.value))
                  .map(label => {
                    const negated = query.negativeLabels.some(term =>
                      array.shallowEqual(term, [label.value])
                    )
                    return (
                      <Label
                        key={label.id}
                        color={label.color}
                        active={
                          negated ||
                          query.labels.some(term => array.shallowEqual(term, [label.value]))
                        }
                        negated={negated}
                        onClick={event => {
                          querySource.current = QuerySource.internal
                          setQuery(oldQuery => {
                            const newQuery = oldQuery.withToggled(
                              'labels',
                              'negativeLabels',
                              label.value,
                              event.shiftKey
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
            <div className="flex max-h-search-suggestions-list flex-col overflow-y-auto">
              {suggestions.map((suggestion, index) => (
                // This should not be a `<button>`, since `render()` may output a
                // tree containing a button.
                <div
                  data-testid="asset-search-suggestion"
                  key={index}
                  ref={el => {
                    if (index === selectedIndex) {
                      el?.focus()
                    }
                  }}
                  tabIndex={-1}
                  className={`pointer-events-auto mx-search-suggestion cursor-pointer rounded-default px-search-suggestions py-search-suggestion-y text-left transition-colors last:mb-search-suggestion hover:bg-selected-frame ${
                    index === selectedIndex
                      ? 'bg-selected-frame'
                      : selectedIndices.has(index)
                        ? 'bg-frame'
                        : ''
                  }`}
                  onClick={event => {
                    querySource.current = QuerySource.internal
                    setQuery(
                      selectedIndices.has(index)
                        ? suggestion.deleteFromQuery(event.shiftKey ? query : baseQuery.current)
                        : suggestion.addToQuery(event.shiftKey ? query : baseQuery.current)
                    )
                    if (event.shiftKey) {
                      setSelectedIndices(
                        new Set(
                          selectedIndices.has(index)
                            ? [...selectedIndices].filter(otherIndex => otherIndex !== index)
                            : [...selectedIndices, index]
                        )
                      )
                    } else {
                      setAreSuggestionsVisible(false)
                    }
                  }}
                >
                  {suggestion.render()}
                </div>
              ))}
            </div>
          </div>
        )}
      </div>
    </label>
  )
}

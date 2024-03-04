/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'
import * as detect from 'enso-common/src/detect'

import * as modalProvider from '#/providers/ModalProvider'

import Label from '#/components/dashboard/Label'

import type * as backend from '#/services/Backend'

import * as array from '#/utilities/array'
import AssetQuery from '#/utilities/AssetQuery'
import * as eventModule from '#/utilities/event'

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
    const onKeyDown = (event: KeyboardEvent) => {
      setIsShiftPressed(event.shiftKey)
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
    document.addEventListener('keydown', onKeyDown)
    document.addEventListener('keyup', onKeyUp)
    return () => {
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
      className="group search-bar relative flex items-center text-primary rounded-full gap-asset-search-bar h-row grow max-w-asset-search-bar xl:max-w-asset-search-bar-wide px-input-x"
    >
      <img src={FindIcon} className="placeholder relative z-1" />
      <input
        ref={searchRef}
        type="search"
        size={1}
        placeholder={
          isCloud
            ? 'Type to search for projects, Data Links, users, and more.'
            : 'Type to search for projects.'
        }
        className="peer relative z-1 grow bg-transparent text placeholder:text-center"
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
      <div className="absolute flex flex-col top left overflow-hidden w-full before:absolute before:bg-frame before:inset before:backdrop-blur-default rounded-default pointer-events-none">
        <div className="relative padding h-row" />
        {areSuggestionsVisible && (
          <div className="relative flex flex-col gap-search-suggestions">
            {/* Tags (`name:`, `modified:`, etc.) */}
            <div
              data-testid="asset-search-tag-names"
              className="flex flex-wrap gap-buttons whitespace-nowrap px-search-suggestions pointer-events-auto"
            >
              {(isCloud ? AssetQuery.tagNames : AssetQuery.localTagNames).flatMap(entry => {
                const [key, tag] = entry
                return tag == null || isShiftPressed !== tag.startsWith('-')
                  ? []
                  : [
                      <button
                        key={key}
                        className="bg-frame rounded-full h-text px-button-x hover:bg-selected-frame transition-all"
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
            {isCloud && (
              <div
                data-testid="asset-search-labels"
                className="flex gap-buttons p-search-suggestions pointer-events-auto"
              >
                {labels.map(label => {
                  const negated = query.negativeLabels.some(term =>
                    array.shallowEqual(term, [label.value])
                  )
                  return (
                    <Label
                      key={label.id}
                      color={label.color}
                      group={false}
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
            <div className="flex flex-col max-h-search-suggestions-list overflow-y-auto">
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
                  className={`cursor-pointer px-search-suggestions py-search-suggestion-y mx-search-suggestion rounded-default text-left hover:bg-selected-frame last:mb-search-suggestion transition-colors pointer-events-auto ${
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

/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'

import Label from '#/components/dashboard/Label'

import type * as backend from '#/services/Backend'

import * as array from '#/utilities/array'
import AssetQuery, * as assetQuery from '#/utilities/AssetQuery'
import * as shortcutManager from '#/utilities/ShortcutManager'

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
  render: () => React.ReactNode
  addToQuery: (query: AssetQuery) => AssetQuery
  deleteFromQuery: (query: AssetQuery) => AssetQuery
}

// ======================
// === AssetSearchBar ===
// ======================

/** Props for a {@link AssetSearchBar}. */
export interface AssetSearchBarProps {
  query: AssetQuery
  setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  labels: backend.Label[]
  suggestions: Suggestion[]
}

/** A search bar containing a text input, and a list of suggestions. */
export default function AssetSearchBar(props: AssetSearchBarProps) {
  const { query, setQuery, labels, suggestions: rawSuggestions } = props
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
        searchRef.current.value = query.toString()
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
        if (event.key === 'Tab') {
          event.preventDefault()
          querySource.current = QuerySource.tabbing
          setSelectedIndex(oldIndex => {
            const length = Math.max(1, suggestionsRef.current.length)
            if (event.shiftKey) {
              return oldIndex == null ? length - 1 : (oldIndex + length - 1) % length
            } else {
              return oldIndex == null ? 0 : (oldIndex + 1) % length
            }
          })
          // FIXME: `setQuery`?
        }
        if (event.key === 'Enter' || event.key === ' ') {
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
        searchRef.current?.blur()
      }
      // Allow `alt` key to be pressed in case it is being used to enter special characters.
      if (
        !(event.target instanceof HTMLInputElement) &&
        (!(event.target instanceof HTMLElement) || !event.target.isContentEditable) &&
        (!(event.target instanceof Node) || rootRef.current?.contains(event.target) !== true) &&
        shortcutManager.isTextInputEvent(event)
      ) {
        searchRef.current?.focus()
      }
      if (
        event.target instanceof Node &&
        rootRef.current?.contains(event.target) === true &&
        shortcutManager.isPotentiallyShortcut(event)
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
  }, [])

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
      className="group search-bar relative flex items-center text-primary rounded-full gap-2.5 h-8 grow max-w-98.25 xl:max-w-screen-1/3 px-2"
    >
      <img src={FindIcon} className="relative z-1 opacity-80" />
      <input
        ref={searchRef}
        type="search"
        size={1}
        placeholder="Type to search for projects, data connectors, users, and more."
        className="peer relative z-1 grow bg-transparent leading-5 h-6 py-px xl:placeholder:text-center"
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
      <div className="absolute flex flex-col top-0 left-0 overflow-hidden w-full before:absolute before:bg-frame before:inset-0 before:backdrop-blur-3xl rounded-2xl pointer-events-none transition-all duration-300">
        <div className="relative padding h-8"></div>
        {areSuggestionsVisible && (
          <div className="relative flex flex-col gap-2">
            {/* Tags (`name:`, `modified:`, etc.) */}
            <div
              data-testid="asset-search-tag-names"
              className="flex flex-wrap gap-2 whitespace-nowrap px-2 pointer-events-auto"
            >
              {AssetQuery.tagNames.flatMap(entry => {
                const [key, tag] = entry
                return tag == null || isShiftPressed !== tag.startsWith('-')
                  ? []
                  : [
                      <button
                        key={key}
                        className="bg-frame rounded-full h-6 px-2 hover:bg-frame-selected transition-all"
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
            <div data-testid="asset-search-labels" className="flex gap-2 p-2 pointer-events-auto">
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
                      negated || query.labels.some(term => array.shallowEqual(term, [label.value]))
                    }
                    negated={negated}
                    onClick={event => {
                      querySource.current = QuerySource.internal
                      setQuery(oldQuery => {
                        const newQuery = assetQuery.toggleLabel(
                          oldQuery,
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
            {/* Suggestions */}
            <div className="flex flex-col max-h-[16rem] overflow-y-auto">
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
                  className={`cursor-pointer px-2 py-1 mx-1 rounded-2xl text-left hover:bg-frame-selected last:mb-1 transition-colors pointer-events-auto ${
                    index === selectedIndex
                      ? 'bg-frame-selected'
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

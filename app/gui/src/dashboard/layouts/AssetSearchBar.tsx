/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import FindIcon from '#/assets/find.svg'
import { unsafeWriteValue } from '#/utilities/write'

import * as backendHooks from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Label from '#/components/dashboard/Label'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import type Backend from '#/services/Backend'
import type { Label as BackendLabel } from '#/services/Backend'
import * as array from '#/utilities/array'
import AssetQuery from '#/utilities/AssetQuery'
import * as eventModule from '#/utilities/event'
import * as string from '#/utilities/string'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import { createStore, useStore } from '#/utilities/zustand'
import { AnimatePresence, motion } from 'framer-motion'

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
  readonly key: string
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

export const searchbarSuggestionsStore = createStore<{
  readonly suggestions: readonly Suggestion[]
  readonly setSuggestions: (suggestions: readonly Suggestion[]) => void
}>((set) => ({
  suggestions: [],
  setSuggestions: (suggestions) => {
    set({ suggestions })
  },
}))

/**
 * Sets the suggestions.
 */
export function useSetSuggestions() {
  return useStore(searchbarSuggestionsStore, (state) => state.setSuggestions, {
    unsafeEnableTransition: true,
  })
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
                    unsafeWriteValue(querySource, 'current', QuerySource.internal)
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
function AssetSearchBar(props: AssetSearchBarProps) {
  const { backend, isCloud, query, setQuery } = props
  const { modalRef } = modalProvider.useModalRef()
  /** A cached query as of the start of tabbing. */
  const baseQuery = React.useRef(query)

  const rawSuggestions = useStore(searchbarSuggestionsStore, (state) => state.suggestions, {
    unsafeEnableTransition: true,
  })

  const [suggestions, setSuggestions] = React.useState(rawSuggestions)

  const suggestionsRef = useSyncRef(suggestions)

  const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)
  const [areSuggestionsVisible, privateSetAreSuggestionsVisible] = React.useState(false)
  const areSuggestionsVisibleRef = React.useRef(areSuggestionsVisible)
  const querySource = React.useRef(QuerySource.external)
  const rootRef = React.useRef<HTMLLabelElement | null>(null)
  const searchRef = React.useRef<HTMLInputElement | null>(null)

  const setAreSuggestionsVisible = useEventCallback((value: boolean) => {
    React.startTransition(() => {
      privateSetAreSuggestionsVisible(value)
      areSuggestionsVisibleRef.current = value
    })
  })

  React.useEffect(() => {
    if (querySource.current !== QuerySource.tabbing) {
      setSuggestions(rawSuggestions)
      unsafeWriteValue(suggestionsRef, 'current', rawSuggestions)
    }
  }, [rawSuggestions, suggestionsRef])

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

  const selectedIndexDeps = useSyncRef({ query, setQuery, suggestions })

  React.useEffect(() => {
    const deps = selectedIndexDeps.current
    if (
      querySource.current === QuerySource.internal ||
      querySource.current === QuerySource.tabbing
    ) {
      let newQuery = deps.query
      const suggestion = selectedIndex == null ? null : deps.suggestions[selectedIndex]
      if (suggestion != null) {
        newQuery = suggestion.addToQuery(baseQuery.current)
        deps.setQuery(newQuery)
      }
      searchRef.current?.focus()
      const end = searchRef.current?.value.length ?? 0
      searchRef.current?.setSelectionRange(end, end)
      if (searchRef.current != null) {
        searchRef.current.value = newQuery.toString()
      }
    }
  }, [selectedIndex, selectedIndexDeps])

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
  }, [setQuery, modalRef, setAreSuggestionsVisible, suggestionsRef])

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

  const onSearchFieldKeyDown = useEventCallback((event: aria.KeyboardEvent) => {
    event.continuePropagation()
  })

  const searchFieldOnChange = useEventCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    if (querySource.current !== QuerySource.internal) {
      querySource.current = QuerySource.typing
      setQuery(AssetQuery.fromString(event.target.value))
    }
  })

  const searchInputOnKeyDown = useEventCallback((event: React.KeyboardEvent<HTMLInputElement>) => {
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
  })

  const deferredSuggestions = React.useDeferredValue(suggestions)

  return (
    <FocusArea direction="horizontal">
      {(innerProps) => (
        <div className="relative w-full max-w-[60em]">
          <aria.Label
            data-testid="asset-search-bar"
            {...aria.mergeProps<aria.LabelProps & React.RefAttributes<HTMLLabelElement>>()(
              innerProps,
              {
                className:
                  'z-1 group flex grow items-center gap-asset-search-bar rounded-full px-1.5 py-1 text-primary border-0.5 border-primary/20',
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

            <AssetSearchBarPopover
              areSuggestionsVisible={areSuggestionsVisible}
              isCloud={isCloud}
              querySource={querySource}
              query={query}
              setQuery={setQuery}
              suggestions={deferredSuggestions}
              selectedIndex={selectedIndex}
              setAreSuggestionsVisible={setAreSuggestionsVisible}
              baseQuery={baseQuery}
              backend={backend}
            />

            <SvgMask
              src={FindIcon}
              className="absolute left-2 top-[50%] z-1 mt-[1px] -translate-y-1/2 text-primary/40"
            />

            <AssetSearchBarInput
              query={query}
              isCloud={isCloud}
              onSearchFieldKeyDown={onSearchFieldKeyDown}
              searchRef={searchRef}
              searchFieldOnChange={searchFieldOnChange}
              searchInputOnKeyDown={searchInputOnKeyDown}
            />
          </aria.Label>
        </div>
      )}
    </FocusArea>
  )
}

/** Props for a {@link AssetSearchBarInput}. */
interface AssetSearchBarInputProps {
  readonly query: AssetQuery
  readonly isCloud: boolean
  readonly onSearchFieldKeyDown: (event: aria.KeyboardEvent) => void
  readonly searchRef: React.RefObject<HTMLInputElement>
  readonly searchFieldOnChange: (event: React.ChangeEvent<HTMLInputElement>) => void
  readonly searchInputOnKeyDown: (event: React.KeyboardEvent<HTMLInputElement>) => void
}

/**
 * Renders the search field.
 */
// eslint-disable-next-line no-restricted-syntax
const AssetSearchBarInput = React.memo(function AssetSearchBarInput(
  props: AssetSearchBarInputProps,
) {
  const {
    query,
    isCloud,
    onSearchFieldKeyDown,
    searchRef,
    searchFieldOnChange,
    searchInputOnKeyDown,
  } = props
  const { getText } = textProvider.useText()
  return (
    <>
      <FocusRing placement="before">
        <aria.SearchField
          aria-label={getText('assetSearchFieldLabel')}
          className="relative grow before:text before:absolute before:-inset-x-1 before:my-auto before:rounded-full before:transition-all"
          value={query.query}
          onKeyDown={onSearchFieldKeyDown}
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
            onChange={searchFieldOnChange}
            onKeyDown={searchInputOnKeyDown}
          />
        </aria.SearchField>
      </FocusRing>
    </>
  )
})

/**
 * Props for a {@link AssetSearchBarPopover}.
 */
interface AssetSearchBarPopoverProps {
  readonly areSuggestionsVisible: boolean
  readonly isCloud: boolean
  readonly querySource: React.MutableRefObject<QuerySource>
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly suggestions: readonly Suggestion[]
  readonly selectedIndex: number | null
  readonly setAreSuggestionsVisible: (value: boolean) => void
  readonly baseQuery: React.MutableRefObject<AssetQuery>
  readonly backend: Backend | null
}

/**
 * Renders the popover containing suggestions.
 */
const AssetSearchBarPopover = React.memo(function AssetSearchBarPopover(
  props: AssetSearchBarPopoverProps,
) {
  const {
    areSuggestionsVisible,
    isCloud,
    querySource,
    query,
    setQuery,
    suggestions,
    selectedIndex,
    setAreSuggestionsVisible,
    baseQuery,
    backend,
  } = props

  const [selectedIndices, setSelectedIndices] = React.useState<ReadonlySet<number>>(
    new Set<number>(),
  )

  return (
    <>
      <AnimatePresence mode="wait" custom={suggestions.length}>
        {areSuggestionsVisible && (
          <motion.div
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: 'auto', opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
            className={ariaComponents.DIALOG_BACKGROUND({
              className:
                'absolute left-0 right-0 top-0 z-1 grid w-full overflow-hidden rounded-default border-0.5 border-primary/20 -outline-offset-1 outline-primary',
            })}
          >
            <div className="overflow-hidden">
              <div className="relative mt-3 flex flex-col gap-3 pt-8">
                {/* Tags (`name:`, `modified:`, etc.) */}
                <Tags
                  isCloud={isCloud}
                  querySource={querySource}
                  query={query}
                  setQuery={setQuery}
                />
                {/* Asset labels */}
                <Labels
                  isCloud={isCloud}
                  query={query}
                  setQuery={setQuery}
                  querySource={querySource}
                  baseQuery={baseQuery}
                  backend={backend}
                />
                {/* Suggestions */}
                <div className="flex max-h-search-suggestions-list flex-col overflow-y-auto overflow-x-hidden pb-0.5 pl-0.5">
                  {suggestions.map((suggestion, index) => (
                    <SuggestionRenderer
                      key={suggestion.key}
                      index={index}
                      selectedIndex={selectedIndex}
                      selectedIndices={selectedIndices}
                      querySource={querySource}
                      setQuery={setQuery}
                      suggestion={suggestion}
                      setSelectedIndices={setSelectedIndices}
                      setAreSuggestionsVisible={setAreSuggestionsVisible}
                      query={query}
                      baseQuery={baseQuery}
                    />
                  ))}
                </div>
              </div>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </>
  )
})

/**
 * Props for a {@link SuggestionRenderer}.
 */
interface SuggestionRendererProps {
  readonly index: number
  readonly suggestion: Suggestion
  readonly query: AssetQuery
  readonly baseQuery: React.MutableRefObject<AssetQuery>
  readonly selectedIndex: number | null
  readonly selectedIndices: ReadonlySet<number>
  readonly setSelectedIndices: React.Dispatch<React.SetStateAction<ReadonlySet<number>>>
  readonly querySource: React.MutableRefObject<QuerySource>
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly setAreSuggestionsVisible: (value: boolean) => void
}

/**
 * Renders a suggestion.
 */
const SuggestionRenderer = React.memo(function SuggestionRenderer(props: SuggestionRendererProps) {
  const {
    index,
    selectedIndex,
    selectedIndices,
    querySource,
    setQuery,
    suggestion,
    setSelectedIndices,
    setAreSuggestionsVisible,
    query,
    baseQuery,
  } = props

  return (
    <aria.Button
      data-testid="asset-search-suggestion"
      key={index}
      ref={(el) => {
        if (index === selectedIndex) {
          el?.focus()
        }
      }}
      className={tailwindMerge.twMerge(
        'flex w-full cursor-pointer rounded-l-default rounded-r-sm px-[7px] py-0.5 text-left transition-[background-color] hover:bg-primary/5',
        selectedIndices.has(index) && 'bg-primary/10',
        index === selectedIndex && 'bg-selected-frame',
      )}
      onPress={(event) => {
        unsafeWriteValue(querySource, 'current', QuerySource.internal)
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
  )
})

/** Props for a {@link Labels}. */
interface LabelsProps {
  readonly isCloud: boolean
  readonly query: AssetQuery
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly backend: Backend | null
  readonly querySource: React.MutableRefObject<QuerySource>
  readonly baseQuery: React.MutableRefObject<AssetQuery>
}

/** Renders labels. */
const Labels = React.memo(function Labels(props: LabelsProps) {
  const { isCloud, query, setQuery, backend, querySource, baseQuery } = props

  const labels = backendHooks.useBackendQuery(backend, 'listTags', []).data ?? []

  const labelOnPress = useEventCallback(
    (event: aria.PressEvent | React.MouseEvent<HTMLButtonElement>, label?: BackendLabel) => {
      if (label == null) {
        return
      }
      unsafeWriteValue(querySource, 'current', QuerySource.internal)
      setQuery((oldQuery) => {
        const newQuery = oldQuery.withToggled(
          'labels',
          'negativeLabels',
          label.value,
          event.shiftKey,
        )
        unsafeWriteValue(baseQuery, 'current', newQuery)
        return newQuery
      })
    },
  )

  return (
    <>
      {isCloud && labels.length !== 0 && (
        <div data-testid="asset-search-labels" className="pointer-events-auto flex gap-2 px-1.5">
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
                  label={label}
                  active={
                    negated || query.labels.some((term) => array.shallowEqual(term, [label.value]))
                  }
                  negated={negated}
                  onPress={labelOnPress}
                >
                  {label.value}
                </Label>
              )
            })}
        </div>
      )}
    </>
  )
})

export default React.memo(AssetSearchBar)

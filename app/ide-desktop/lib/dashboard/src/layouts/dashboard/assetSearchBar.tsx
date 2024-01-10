/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'

import type * as backend from '#/services/backend'
import * as array from '#/utilities/array'
import * as assetQuery from '#/utilities/assetQuery'
import * as shortcuts from '#/utilities/shortcuts'

import Label from '#/components/dashboard/Label'

/** A suggested query based on */
export interface Suggestion {
    render: () => React.ReactNode
    addToQuery: (query: assetQuery.AssetQuery) => assetQuery.AssetQuery
    deleteFromQuery: (query: assetQuery.AssetQuery) => assetQuery.AssetQuery
}

/** Props for a {@link AssetSearchBar}. */
export interface AssetSearchBarProps {
    query: assetQuery.AssetQuery
    setQuery: React.Dispatch<React.SetStateAction<assetQuery.AssetQuery>>
    labels: backend.Label[]
    suggestions: Suggestion[]
}

/** A search bar containing a text input, and a list of suggestions. */
export default function AssetSearchBar(props: AssetSearchBarProps) {
    const { query, setQuery, labels, suggestions: rawSuggestions } = props
    const [isTabbing, setIsTabbing] = React.useState(false)
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
    const [wasQueryModified, setWasQueryModified] = React.useState(false)
    const [wasQueryTyped, setWasQueryTyped] = React.useState(false)
    const [isShiftPressed, setIsShiftPressed] = React.useState(false)
    const rootRef = React.useRef<HTMLLabelElement>(null)
    const searchRef = React.useRef<HTMLInputElement>(null)

    React.useEffect(() => {
        if (!isTabbing && !isShiftPressed) {
            baseQuery.current = query
        }
    }, [isTabbing, isShiftPressed, query])

    React.useEffect(() => {
        if (!wasQueryTyped) {
            baseQuery.current = query
            if (searchRef.current != null) {
                searchRef.current.value = query.toString()
            }
        }
    }, [wasQueryTyped, query])

    React.useEffect(() => {
        if (!isTabbing && !isShiftPressed) {
            setSuggestions(rawSuggestions)
            suggestionsRef.current = rawSuggestions
        }
    }, [isTabbing, isShiftPressed, rawSuggestions])

    React.useEffect(() => {
        areSuggestionsVisibleRef.current = areSuggestionsVisible
    }, [areSuggestionsVisible])

    React.useEffect(() => {
        if (!wasQueryModified && selectedIndex == null) {
            setQuery(baseQuery.current)
        }
        // `wasQueryModified` MUST NOT be a dependency, as it is always set to `false` immediately
        // after it is set to true.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [selectedIndex, /* should never change */ setQuery])

    React.useEffect(() => {
        let newQuery = query
        if (wasQueryModified) {
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
        setWasQueryModified(false)
    }, [
        wasQueryModified,
        query,
        baseQuery,
        selectedIndex,
        suggestions,
        /* should never change */ setQuery,
    ])

    React.useEffect(() => {
        const onKeyDown = (event: KeyboardEvent) => {
            setIsShiftPressed(event.shiftKey)
            if (areSuggestionsVisibleRef.current) {
                if (event.key === 'Tab') {
                    event.preventDefault()
                    setIsTabbing(true)
                    setSelectedIndex(oldIndex => {
                        if (event.shiftKey) {
                            return oldIndex == null
                                ? suggestionsRef.current.length - 1
                                : (oldIndex + suggestionsRef.current.length - 1) %
                                      suggestionsRef.current.length
                        } else {
                            return oldIndex == null
                                ? 0
                                : (oldIndex + 1) % suggestionsRef.current.length
                        }
                    })
                    setWasQueryModified(true)
                }
            }
            if (event.key === 'Escape') {
                searchRef.current?.blur()
            }
            // Allow `alt` key to be pressed in case it is being used to enter special characters.
            if (
                !(event.target instanceof HTMLInputElement) &&
                (!(event.target instanceof HTMLElement) || !event.target.isContentEditable) &&
                (!(event.target instanceof Node) ||
                    rootRef.current?.contains(event.target) !== true) &&
                shortcuts.isTextInputEvent(event)
            ) {
                searchRef.current?.focus()
            }
            if (
                event.target instanceof Node &&
                rootRef.current?.contains(event.target) === true &&
                shortcuts.isPotentiallyShortcut(event)
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

    React.useEffect(() => {
        const onKeyDown = (event: KeyboardEvent) => {
            if (areSuggestionsVisibleRef.current) {
                if (event.key === 'Enter' || event.key === ' ') {
                    baseQuery.current = query
                    setIsTabbing(false)
                    setSelectedIndex(null)
                    searchRef.current?.focus()
                    const end = searchRef.current?.value.length ?? 0
                    searchRef.current?.setSelectionRange(end, end)
                }
                if (event.key === 'Enter') {
                    setAreSuggestionsVisible(false)
                }
            }
        }
        document.addEventListener('keydown', onKeyDown)
        return () => {
            document.removeEventListener('keydown', onKeyDown)
        }
    }, [query, setQuery])

    return (
        <label
            ref={rootRef}
            tabIndex={-1}
            onFocus={() => {
                setAreSuggestionsVisible(true)
            }}
            onBlur={event => {
                if (!event.currentTarget.contains(event.relatedTarget)) {
                    setIsTabbing(false)
                    setSelectedIndex(null)
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
                onFocus={() => {
                    if (!wasQueryModified) {
                        setSelectedIndex(null)
                    }
                }}
                onChange={event => {
                    if (!wasQueryModified) {
                        setQuery(assetQuery.AssetQuery.fromString(event.target.value))
                        setWasQueryTyped(true)
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
                        setQuery(query.clone())
                    }
                }}
            />
            <div className="absolute flex flex-col top-0 left-0 overflow-hidden w-full before:absolute before:bg-frame before:inset-0 before:backdrop-blur-3xl rounded-2xl pointer-events-none transition-all duration-300">
                <div className="relative padding h-8"></div>
                {areSuggestionsVisible && (
                    <div className="relative flex flex-col gap-2">
                        {/* Tags (`name:`, `modified:`, etc.) */}
                        <div className="flex flex-wrap gap-2 whitespace-nowrap px-2 pointer-events-auto">
                            {assetQuery.AssetQuery.tagNames.flatMap(entry => {
                                const [key, tag] = entry
                                return tag == null || isShiftPressed !== tag.startsWith('-')
                                    ? []
                                    : [
                                          <button
                                              key={key}
                                              className="bg-frame rounded-full h-6 px-2 hover:bg-frame-selected transition-all"
                                              onClick={() => {
                                                  setWasQueryModified(true)
                                                  setSelectedIndex(null)
                                                  setQuery(oldQuery => {
                                                      const newQuery = oldQuery.add({ [key]: [[]] })
                                                      baseQuery.current = newQuery
                                                      return newQuery
                                                  })
                                              }}
                                          >
                                              {tag}:
                                          </button>,
                                      ]
                            })}
                        </div>
                        {/* Asset labels */}
                        <div className="flex gap-2 p-2 pointer-events-auto">
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
                                            query.labels.some(term =>
                                                array.shallowEqual(term, [label.value])
                                            )
                                        }
                                        negated={negated}
                                        onClick={event => {
                                            setWasQueryModified(true)
                                            setSelectedIndex(null)
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
                                <div
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
                                        setSelectedIndex(null)
                                        setWasQueryModified(true)
                                        setQuery(
                                            selectedIndices.has(index)
                                                ? suggestion.deleteFromQuery(
                                                      event.shiftKey ? query : baseQuery.current
                                                  )
                                                : suggestion.addToQuery(
                                                      event.shiftKey ? query : baseQuery.current
                                                  )
                                        )
                                        if (event.shiftKey) {
                                            setSelectedIndices(
                                                new Set(
                                                    selectedIndices.has(index)
                                                        ? [...selectedIndices].filter(
                                                              otherIndex => otherIndex !== index
                                                          )
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

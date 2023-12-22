/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'

import * as array from '../../array'
import * as assetQuery from '../../assetQuery'
import type * as backend from '../backend'
import * as shortcuts from '../shortcuts'

import Label from './label'

/** A suggested query based on */
export interface Suggestion {
    render: () => React.ReactNode
    // eslint-disable-next-line no-restricted-syntax
    newQuery: (query: assetQuery.AssetQuery) => assetQuery.AssetQuery
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
    const { query: rawQuery, setQuery: setRawQuery, labels, suggestions: rawSuggestions } = props
    const [isTabbing, setIsTabbing] = React.useState(false)
    /** A cached query as of the start of tabbing. */
    const [baseQuery, setBaseQuery] = React.useState(rawQuery)
    /** The query that is actually displayed. */
    const [query, setQuery] = React.useState(baseQuery)
    const [suggestions, setSuggestions] = React.useState(rawSuggestions)
    const suggestionsRef = React.useRef(rawSuggestions)
    const [selectedIndex, setSelectedIndex] = React.useState<number | null>(null)
    const [areSuggestionsVisible, setAreSuggestionsVisible] = React.useState(false)
    const areSuggestionsVisibleRef = React.useRef(areSuggestionsVisible)
    const [wasQueryModified, setWasQueryModified] = React.useState(false)
    const [isShiftPressed, setIsShiftPressed] = React.useState(false)
    const searchRef = React.useRef<HTMLInputElement>(null)

    React.useEffect(() => {
        if (!isTabbing && !isShiftPressed) {
            setBaseQuery(rawQuery)
        }
    }, [isTabbing, isShiftPressed, rawQuery])

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
        const suggestion = selectedIndex == null ? null : suggestions[selectedIndex]
        if (suggestion == null) {
            setQuery(baseQuery)
        } else {
            setQuery(suggestion.newQuery(baseQuery))
            setRawQuery(suggestion.newQuery(baseQuery))
        }
    }, [baseQuery, selectedIndex, suggestions, /* should never change */ setRawQuery])

    React.useEffect(() => {
        if (wasQueryModified) {
            searchRef.current?.focus()
            const end = searchRef.current?.value.length ?? 0
            searchRef.current?.setSelectionRange(end, end)
        }
        setWasQueryModified(false)
    }, [wasQueryModified, query])

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
            // Allow `alt` key to be pressed in case it is being used to enter special characters.
            if (
                !(event.target instanceof HTMLInputElement) &&
                (!(event.target instanceof HTMLElement) || !event.target.isContentEditable) &&
                shortcuts.isTextInputEvent(event)
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
                    setBaseQuery(query)
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
    }, [query, setRawQuery])

    return (
        <label
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
            className="group search-bar absolute flex items-center text-primary rounded-full -translate-x-1/2 gap-2.5 left-1/2 h-8 w-98.25 min-w-31.5 px-2"
        >
            <img src={FindIcon} className="relative z-1 opacity-80" />
            <input
                ref={searchRef}
                type="search"
                size={1}
                placeholder="Type to search for projects, data connectors, users, and more."
                value={query.query}
                className="peer relative z-1 grow bg-transparent leading-5 h-6 py-px"
                onFocus={() => {
                    if (!wasQueryModified) {
                        setSelectedIndex(null)
                    }
                }}
                onChange={event => {
                    if (!wasQueryModified) {
                        setRawQuery(assetQuery.AssetQuery.fromString(event.target.value))
                    }
                }}
            />
            <div className="absolute flex flex-col top-0 left-0 overflow-hidden w-full before:absolute before:bg-frame before:inset-0 before:backdrop-blur-3xl rounded-2xl pointer-events-none transition-all duration-300">
                <div className="relative padding h-8"></div>
                {areSuggestionsVisible && (
                    <div className="relative flex flex-col gap-2">
                        {/* Tags (`name:`, `modified:`) */}
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
                                                  setRawQuery(baseQuery.add({ [key]: [['']] }))
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
                                            setRawQuery(oldQuery =>
                                                assetQuery.toggleLabel(
                                                    oldQuery,
                                                    label.value,
                                                    event.shiftKey
                                                )
                                            )
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
                                        index === selectedIndex ? 'bg-frame-selected' : ''
                                    }`}
                                    onClick={event => {
                                        const newQuery = suggestion.newQuery(query)
                                        if (event.shiftKey) {
                                            setQuery(newQuery)
                                        }
                                        setRawQuery(newQuery)
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

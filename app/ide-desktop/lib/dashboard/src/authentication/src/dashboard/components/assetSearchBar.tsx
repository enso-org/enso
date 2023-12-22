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
    const { query, setQuery, labels, suggestions } = props
    const [areSuggestionsVisible, setAreSuggestionsVisible] = React.useState(false)
    const [wasQueryModified, setWasQueryModified] = React.useState(false)
    const [showNegatedTags, setShowNegatedTags] = React.useState(false)
    const searchRef = React.useRef<HTMLInputElement>(null)

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
            setShowNegatedTags(event.shiftKey)
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
            setShowNegatedTags(event.shiftKey)
        }
        document.addEventListener('keydown', onKeyDown)
        document.addEventListener('keyup', onKeyUp)
        return () => {
            document.removeEventListener('keydown', onKeyDown)
            document.removeEventListener('keyup', onKeyUp)
        }
    }, [])

    return (
        <label
            tabIndex={-1}
            onFocus={() => {
                setAreSuggestionsVisible(true)
            }}
            onBlur={event => {
                if (!event.currentTarget.contains(event.relatedTarget)) {
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
                onChange={event => {
                    setQuery(assetQuery.AssetQuery.fromString(event.target.value))
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
                                return tag == null || showNegatedTags !== tag.startsWith('-')
                                    ? []
                                    : [
                                          <button
                                              key={key}
                                              className="bg-frame rounded-full h-6 px-2 hover:bg-frame-selected transition-all"
                                              onClick={() => {
                                                  setWasQueryModified(true)
                                                  setQuery(query.add({ [key]: [['']] }))
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
                                            setQuery(oldQuery =>
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
                                    className="cursor-pointer px-2 py-1 mx-1 rounded-2xl text-left hover:bg-frame-selected last:mb-1 transition-colors pointer-events-auto"
                                    onClick={() => {
                                        setQuery(suggestion.newQuery(query))
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

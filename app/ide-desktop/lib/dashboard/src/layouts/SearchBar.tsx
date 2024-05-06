/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from 'enso-assets/find.svg'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

// =================
// === SearchBar ===
// =================

/** Props for a {@link SearchBar}. */
export interface SearchBarProps {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'data-testid': string
  readonly query: string
  readonly setQuery: React.Dispatch<React.SetStateAction<string>>
  readonly label: string
  readonly placeholder: string
}

/** A search bar containing a text input, and a list of suggestions. */
export default function SearchBar(props: SearchBarProps) {
  const { query, setQuery, label, placeholder } = props

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <aria.Label
          data-testid={props['data-testid']}
          {...aria.mergeProps<aria.LabelProps>()(innerProps, {
            className:
              'search-bar group relative flex h-row max-w-asset-search-bar grow items-center gap-asset-search-bar rounded-full px-input-x text-primary xl:max-w-asset-search-bar-wide',
          })}
        >
          <img src={FindIcon} className="relative z-1 placeholder" />
          <div className="pointer-events-none absolute left top flex w-full flex-col overflow-hidden rounded-default before:absolute before:inset before:bg-frame before:backdrop-blur-default">
            <div className="padding relative h-row" />
          </div>
          <FocusRing placement="before">
            <aria.SearchField
              aria-label={label}
              className="relative grow before:text before:absolute before:inset-x-button-focus-ring-inset before:my-auto before:rounded-full before:transition-all"
              value={query}
              onKeyDown={event => {
                event.continuePropagation()
              }}
            >
              <aria.Input
                type="search"
                size={1}
                placeholder={placeholder}
                className="focus-child peer text relative z-1 w-full bg-transparent placeholder:text-center"
                onChange={event => {
                  setQuery(event.target.value)
                }}
              />
            </aria.SearchField>
          </FocusRing>
        </aria.Label>
      )}
    </FocusArea>
  )
}

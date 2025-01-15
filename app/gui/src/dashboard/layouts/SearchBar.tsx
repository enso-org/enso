/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from '#/assets/find.svg'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

// =================
// === SearchBar ===
// =================

/** Props for a {@link SearchBar}. */
export interface SearchBarProps {
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
      {(innerProps) => (
        <aria.Label
          data-testid={props['data-testid']}
          {...aria.mergeProps<aria.LabelProps>()(innerProps, {
            className:
              'group relative flex w-full sm:w-[512px] h-row items-center gap-asset-search-bar rounded-full px-input-x text-primary border-0.5 border-primary/20 transition-colors focus-within:outline focus-within:outline-2 outline-primary -outline-offset-1',
          })}
        >
          <SvgMask src={FindIcon} className="text-primary/30" />
          <aria.SearchField
            aria-label={label}
            className="relative grow"
            value={query}
            onKeyDown={(event) => {
              event.continuePropagation()
            }}
          >
            <aria.Input
              type="search"
              size={1}
              placeholder={placeholder}
              className="focus-child w-full bg-transparent text-xs placeholder:text-center"
              onChange={(event) => {
                setQuery(event.target.value)
              }}
            />
          </aria.SearchField>
        </aria.Label>
      )}
    </FocusArea>
  )
}

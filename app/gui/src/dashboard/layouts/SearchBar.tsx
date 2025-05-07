/** @file A search bar containing a text input, and a list of suggestions. */
import * as React from 'react'

import FindIcon from '#/assets/find.svg'

import * as aria from '#/components/aria'
import SvgMask from '#/components/SvgMask'

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
    <aria.Label
      data-testid={props['data-testid']}
      className="group relative flex h-row w-full items-center gap-asset-search-bar rounded-full border-0.5 border-primary/20 px-input-x text-primary -outline-offset-1 outline-primary transition-colors focus-within:outline focus-within:outline-2 sm:w-[512px]"
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
          className="w-full bg-transparent text-xs placeholder:text-center"
          onChange={(event) => {
            setQuery(event.target.value)
          }}
        />
      </aria.SearchField>
    </aria.Label>
  )
}

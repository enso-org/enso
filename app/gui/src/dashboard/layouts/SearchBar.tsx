/** @file A search bar containing a text input, and a list of suggestions. */
import { Input, Label, SearchField } from '#/components/aria'
import { Icon } from '#/components/Icon'
import type { Dispatch, SetStateAction } from 'react'
import { twMerge } from 'tailwind-merge'

/** Props for a {@link SearchBar}. */
export interface SearchBarProps {
  readonly 'data-testid': string
  readonly query: string
  readonly setQuery: Dispatch<SetStateAction<string>>
  readonly label: string
  readonly placeholder: string
  readonly className?: string
}

/** A search bar containing a text input, and a list of suggestions. */
export default function SearchBar(props: SearchBarProps) {
  const { query, setQuery, label, placeholder, className } = props

  return (
    <Label
      data-testid={props['data-testid']}
      className={twMerge(
        'group relative flex h-row w-full items-center gap-asset-search-bar rounded-full border-0.5 border-primary/50 px-input-x text-primary -outline-offset-1 outline-primary transition-colors focus-within:outline focus-within:outline-2 sm:w-[512px]',
        className,
      )}
    >
      <Icon icon="find" className="text-primary" />
      <SearchField
        aria-label={label}
        className="relative grow"
        value={query}
        onKeyDown={(event) => {
          event.continuePropagation()
        }}
      >
        <Input
          type="search"
          size={1}
          placeholder={placeholder}
          className="w-full bg-transparent text-xs placeholder:text-center"
          onChange={(event) => {
            setQuery(event.target.value)
          }}
        />
      </SearchField>
    </Label>
  )
}

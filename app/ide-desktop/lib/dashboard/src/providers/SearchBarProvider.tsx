/** @file The React provider (and associated hooks) for providing a search bar. */
import * as React from 'react'

// ========================
// === SearchBarContext ===
// ========================

/** The type of a search bar. */
export type SearchBar = JSX.Element

/** Information related to the current search bar. */
interface SearchBarInfo {
  readonly key: string
  readonly searchBar: SearchBar
}

/** State contained in a `SearchBarContext`. */
export type SearchBarContextType = SearchBarInfo | null

const SearchBarContext = React.createContext<SearchBarContextType>(null)

/** Props for a {@link SearchBarProvider}. */
export interface SearchBarProviderProps
  extends Readonly<React.PropsWithChildren>,
    Partial<NonNullable<SearchBarContextType>> {}

// =========================
// === SearchBarProvider ===
// =========================

/** A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused. */
export default function SearchBarProvider(props: SearchBarProviderProps) {
  const { children } = props
  const [searchBarInfo, setSearchBarInfo] = React.useState<SearchBarContextType>(null)

  const searchBarStatiProvider = React.useMemo(
    () => (
      <SearchBarStaticProvider setSearchBarInfo={setSearchBarInfo}>
        {children}
      </SearchBarStaticProvider>
    ),
    [children]
  )
  return (
    <SearchBarContext.Provider value={searchBarInfo}>
      {searchBarStatiProvider}
    </SearchBarContext.Provider>
  )
}

/** Whether the containing area is focused. */
export function useSearchBar() {
  return React.useContext(SearchBarContext)?.searchBar ?? null
}

/** State contained in a `SearchBarStaticContext`. */
interface SearchBarStaticContextType {
  readonly setSearchBarInfo: React.Dispatch<React.SetStateAction<SearchBarInfo | null>>
}

// ==============================
// === SearchBarStaticContext ===
// ==============================

const SearchBarStaticContext = React.createContext<SearchBarStaticContextType>({
  setSearchBarInfo: () => {
    // Ignored. This default value will never be used as `SearchBarProvider` always provides
    // its own value.
  },
})

// ===============================
// === SearchBarStaticProvider ===
// ===============================

/** Props for a {@link SearchBarStaticProvider}. */
interface InternalSearchBarStaticProviderProps extends Readonly<React.PropsWithChildren> {
  readonly setSearchBarInfo: React.Dispatch<React.SetStateAction<SearchBarInfo | null>>
}

/** A React provider containing a function to set the currently active search bar. */
function SearchBarStaticProvider(props: InternalSearchBarStaticProviderProps) {
  const { setSearchBarInfo, children } = props

  return (
    <SearchBarStaticContext.Provider value={{ setSearchBarInfo }}>
      {children}
    </SearchBarStaticContext.Provider>
  )
}

/** A React context hook exposing functions to set and unset the currently active search bar. */
export function useSetSearchBar(key: string) {
  const { setSearchBarInfo } = React.useContext(SearchBarStaticContext)
  const setSearchBar = React.useCallback(
    (searchBar: SearchBar) => {
      setSearchBarInfo({ key, searchBar })
    },
    [/* should never change */ key, /* should never change */ setSearchBarInfo]
  )
  const unsetSearchBar = React.useCallback(() => {
    setSearchBarInfo(oldInfo => (oldInfo?.key === key ? null : oldInfo))
  }, [/* should never change */ key, /* should never change */ setSearchBarInfo])
  return { setSearchBar, unsetSearchBar } as const
}

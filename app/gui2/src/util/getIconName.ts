import {
  SuggestionKind,
  type SuggestionEntry,
  type Typename,
} from '@/stores/suggestionDatabase/entry'
import type { Icon } from '@/util/iconName'
import type { MethodPointer } from 'shared/languageServerTypes'

const oldIconNameToNewIconNameLookup: Record<string, Icon> = {
  /* eslint-disable camelcase */
  dataframe_clean: 'table_clean',
  dataframe_map_row: 'map_row',
  dataframe_map_column: 'column_add',
  dataframes_join: 'join2-1',
  dataframes_union: 'union',
  sigma: 'transform4',
  io: 'in_out',
  date_and_time: 'time',
  spatial: 'location',
  predictive: 'predict',
  machine_learning: 'robot',
  /* eslint-enable camelcase */
}

export function mapOldIconName(oldIconName: string): Icon {
  const mappedName = oldIconNameToNewIconNameLookup[oldIconName] ?? oldIconName
  return mappedName as Icon
}

const typeNameToIconLookup: Record<string, Icon> = {
  'Standard.Base.Data.Text.Text': 'text_input',
  'Standard.Base.Data.Numbers.Integer': 'input_number',
  'Standard.Base.Data.Numbers.Float': 'input_number',
  'Standard.Base.Data.Array.Array': 'array_new',
  'Standard.Base.Data.Vector.Vector': 'array_new',
  'Standard.Base.Data.Time.Date.Date': 'calendar',
  'Standard.Base.Data.Time.Date_Time.Date_Time': 'calendar',
  'Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day': 'time',
}

export function typeNameToIcon(typeName: string): Icon {
  return typeNameToIconLookup[typeName] ?? 'enso_logo'
}

export function displayedIconOf(
  entry?: SuggestionEntry,
  methodCall?: MethodPointer,
  actualType?: Typename,
): Icon {
  if (entry) {
    if (entry.iconName) return mapOldIconName(entry.iconName)
    if (entry.kind === SuggestionKind.Local) return 'local_scope2'
    if (entry.kind === SuggestionKind.Module) return 'collection'
  } else if (!methodCall?.name && actualType) return typeNameToIcon(actualType)
  return 'enso_logo'
}

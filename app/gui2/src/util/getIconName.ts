import type { Icon } from '@/util/iconName'

const oldIconNameToNewIconNameLookup: Record<string, string> = {
  /* eslint-disable camelcase */
  dataframe_clean: 'clean_dataframe',
  dataframe_map_row: 'map_row',
  dataframe_map_column: 'add_column',
  dataframes_join: 'join3',
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
  'Standard.Base.Data.Numbers.Integer': 'number_input',
  'Standard.Base.Data.Numbers.Float': 'number_input',
  'Standard.Base.Data.Array.Array': 'array_new',
  'Standard.Base.Data.Vector.Vector': 'array_new',
  'Standard.Base.Data.Time.Date.Date': 'calendar',
  'Standard.Base.Data.Time.Date_Time.Date_Time': 'calendar',
  'Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day': 'time',
}

export function typeNameToIcon(typeName: string): Icon {
  return typeNameToIconLookup[typeName] ?? 'enso_logo'
}

import type { Icon } from '@/util/iconName'

const methodNameToIconLookup: Record<string, Icon> = {
  /* eslint-disable camelcase */
  read: 'text',
  join: 'join3',
  union: 'union',
  set: 'edit',
  filter: 'preparation',
  add_columns: 'add_column',
  add_rows: 'add_row',
  remove_columns: 'remove_column',
  remove_rows: 'remove_row',
  /* eslint-enable camelcase */
}

export function methodNameToIcon(methodName: string): Icon {
  return methodNameToIconLookup[methodName] ?? 'in_out'
}

const typeNameToIconLookup: Record<string, Icon> = {
  'Standard.Base.Data.Table.Table': 'array_new',
  'Standard.Base.Data.Vector.Vector': 'array_new2',
}

export function typeNameToIcon(typeName: string): Icon {
  return typeNameToIconLookup[typeName] ?? 'in_out'
}

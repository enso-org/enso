const methodNameToIconLookup: Record<string, string> = {
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

export function methodNameToIcon(methodName: string): string {
  return methodNameToIconLookup[methodName] ?? 'in_out'
}

const typeNameToIconLookup: Record<string, string> = {
  'Standard.Base.Data.Table.Table': 'array_new',
  'Standard.Base.Data.Vector.Vector': 'array_new2',
}

export function typeNameToIcon(typeName: string): string {
  return typeNameToIconLookup[typeName] ?? 'in_out'
}

const methodNameToIconLookup: Record<string, string> = {
  join: 'join3',
  union: 'union',
  set: 'edit',
}

export function methodNameToIcon(methodName: string | undefined): string {
  return methodName ? methodNameToIconLookup[methodName] ?? 'in_out' : 'in_out'
}

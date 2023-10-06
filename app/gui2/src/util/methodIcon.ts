const methodNameToIconLookup: Record<string, string> = {
  join: 'join2',
  union: 'union',
}

export function methodNameToIcon(methodName: string | undefined): string {
  return methodName ? methodNameToIconLookup[methodName] ?? 'in_out' : 'in_out'
}

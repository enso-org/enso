export type QualifiedName = string

export function qnLastSegment(name: QualifiedName): string {
  const separator = name.lastIndexOf('.')
  return name.substring(separator + 1)
}

export function qnParent(name: QualifiedName): QualifiedName {
  const separator = name.lastIndexOf('.')
  return name.substring(0, separator)
}

export function qnIsTopElement(name: QualifiedName): boolean {
  return (name.match(/\./g)?.length ?? 0) <= 2
}

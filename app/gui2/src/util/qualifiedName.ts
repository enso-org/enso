export type QualifiedName = string

export function qnSplit(name: QualifiedName): [QualifiedName, string] {
  const separator = name.lastIndexOf('.')
  return [name.substring(0, separator), name.substring(separator + 1)]
}

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

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest
  test.each([
    ['Name', '', 'Name'],
    ['Parent.Name', 'Parent', 'Name'],
    ['local.Project.Parent.Name', 'local.Project.Parent', 'Name'],
    ['', '', ''],
    ['Parent.', 'Parent', ''],
  ])("Qualified name %s's parent is %s and the last segment is %s", (qn, parent, lastSegment) => {
    expect(qnLastSegment(qn)).toBe(lastSegment)
    expect(qnParent(qn)).toBe(parent)
    expect(qnSplit(qn)).toStrictEqual([parent, lastSegment])
  })
}

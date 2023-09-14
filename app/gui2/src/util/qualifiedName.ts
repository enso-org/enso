/** Alias suggesting, that given string is a qualified name of our language.
 *
 * In our language, the segments are separated by `.`.
 */
export type QualifiedName = string

/** Split the qualified name to parent and last segment (name). */
export function qnSplit(name: QualifiedName): [QualifiedName, string] {
  const separator = name.lastIndexOf('.')
  return [name.substring(0, separator), name.substring(separator + 1)]
}

/** Get the last segment of qualified name. */
export function qnLastSegment(name: QualifiedName): string {
  const separator = name.lastIndexOf('.')
  return name.substring(separator + 1)
}

/** Get the parent qualified name. If the name has a single segment, empty string is returned. */
export function qnParent(name: QualifiedName): QualifiedName {
  const separator = name.lastIndexOf('.')
  return name.substring(0, separator)
}

/** Checks if given full qualified name is considered a top element of some project.
 *
 * The fully qualified names consists of namespace, project name, and then a path (possibly empty).
 * The element is considered a top element if there is max 1 segment in the path.
 */
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
  ])('Qualified name %s parent is %s and the last segment is %s', (qn, parent, lastSegment) => {
    expect(qnLastSegment(qn)).toBe(lastSegment)
    expect(qnParent(qn)).toBe(parent)
    expect(qnSplit(qn)).toStrictEqual([parent, lastSegment])
  })

  test.each([
    ['local.Project', true],
    ['local.Project.elem', true],
    ['local.Project.Module.elem', false],
  ])('qnIsTopElement(%s) returns %s', (name, result) => expect(qnIsTopElement(name)).toBe(result))
}

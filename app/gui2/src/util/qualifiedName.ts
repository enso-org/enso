import {
  isIdentifier,
  isIdentifierOrOperatorIdentifier,
  type Identifier,
  type IdentifierOrOperatorIdentifier,
  type QualifiedName,
} from '@/util/ast/abstract'
import { Err, Ok, unwrap, type Result } from '@/util/data/result'
export {
  isIdentifier,
  isIdentifierOrOperatorIdentifier,
  type Identifier,
  type IdentifierOrOperatorIdentifier,
  type QualifiedName,
}

const identifierRegexPart = '(?:(?:[a-zA-Z_][0-9]*)+|[!$%&*+,-./:;<=>?@\\^|~]+)'
const qnRegex = new RegExp(`^${identifierRegexPart}(?:\\.${identifierRegexPart})*$`)
const mainSegmentRegex = new RegExp(`^(${identifierRegexPart}\\.${identifierRegexPart})\\.Main`)

export function tryIdentifier(str: string): Result<Identifier> {
  return isIdentifier(str) ? Ok(str) : Err(`"${str}" is not a valid identifier`)
}

export function tryIdentifierOrOperatorIdentifier(
  str: string,
): Result<IdentifierOrOperatorIdentifier> {
  return isIdentifierOrOperatorIdentifier(str) ? Ok(str) : Err(`"${str}" is not a valid identifier`)
}

export function isQualifiedName(str: string): str is QualifiedName {
  return qnRegex.test(str)
}

export function tryQualifiedName(str: string): Result<QualifiedName> {
  return isQualifiedName(str) ? Ok(str) : Err(`"${str}" is not a valid qualified name`)
}

/** Normalize qualified name, removing `Main` module segment of a project if it is present. */
export function normalizeQualifiedName(name: QualifiedName): QualifiedName {
  return name.replace(mainSegmentRegex, '$1') as QualifiedName
}

/** The index of the `.` between the last segment and all other segments.
 * The start of the last segment is one higher than this index. */
export function qnLastSegmentIndex(name: QualifiedName) {
  return name.lastIndexOf('.')
}

/** Split the qualified name to parent and last segment (name). */
export function qnSplit(
  name: QualifiedName,
): [QualifiedName | null, IdentifierOrOperatorIdentifier] {
  const separator = qnLastSegmentIndex(name)
  const parent = separator > 0 ? (name.substring(0, separator) as QualifiedName) : null
  const lastSegment = name.substring(separator + 1) as IdentifierOrOperatorIdentifier
  return [parent, lastSegment]
}

/** Get the last segment of qualified name. */
export function qnLastSegment(name: QualifiedName): IdentifierOrOperatorIdentifier {
  const separator = qnLastSegmentIndex(name)
  return name.substring(separator + 1) as IdentifierOrOperatorIdentifier
}

/** Get the parent qualified name (without last segment) */
export function qnParent(name: QualifiedName): QualifiedName | null {
  const separator = qnLastSegmentIndex(name)
  return separator > 1 ? (name.substring(0, separator) as QualifiedName) : null
}

export function qnJoin(left: QualifiedName, right: QualifiedName): QualifiedName {
  return `${left}.${right}` as QualifiedName
}

export function qnFromSegments(segments: Iterable<IdentifierOrOperatorIdentifier>): QualifiedName {
  return [...segments].join('.') as QualifiedName
}

export function qnSegments(name: QualifiedName): IdentifierOrOperatorIdentifier[] {
  return name.split('.').map((segment) => segment as IdentifierOrOperatorIdentifier)
}

export function qnSlice(
  name: QualifiedName,
  start?: number | undefined,
  end?: number | undefined,
): Result<QualifiedName> {
  return tryQualifiedName(qnSegments(name).slice(start, end).join('.'))
}

/** Checks if given full qualified name is considered a top element of some project.
 *
 * The fully qualified names consists of namespace, project name, and then a path (possibly empty).
 * The element is considered a top element if there is max 1 segment in the path.
 */
export function qnIsTopElement(name: QualifiedName): boolean {
  return !/[.].*?[.].*?[.]/.test(name)
}

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest
  const { initializeFFI } = await import('shared/ast/ffi')
  await initializeFFI()

  const validIdentifiers = [
    'A',
    'a',
    '_A',
    'A_',
    '_1',
    'a_A',
    'abc',
    'Abc',
    'abC',
    'a1',
    'A10_70',
    '+',
    '<=>',
    '*',
    '.',
    '!=',
  ]
  const invalidIdentifiers = ['', '1', '1Abc', '1_', 'abA!', '$a', 'a$']
  // These are not valid identifiers but currently pass the qualified name regex: ['_', '.*']

  test.each(validIdentifiers)("'%s' is a valid identifier", (name) =>
    expect(unwrap(tryIdentifierOrOperatorIdentifier(name))).toStrictEqual(
      name as IdentifierOrOperatorIdentifier,
    ),
  )
  test.each(invalidIdentifiers)("'%s' is an invalid identifier", (name) =>
    expect(tryIdentifierOrOperatorIdentifier(name).ok).toBe(false),
  )

  test.each(
    validIdentifiers.concat('A._', 'a19_r14.zz9z', 'a.b.c.d.e.F', 'Standard.Base.Number.+'),
  )("'%s' is a valid qualified name", (name) =>
    expect(unwrap(tryQualifiedName(name))).toStrictEqual(name as QualifiedName),
  )

  test.each(invalidIdentifiers.concat('.Abc', 'Abc.', '.A.b.c', 'A.b.c.', 'A.B.8.D', '_.._'))(
    "'%s' is an invalid qualified name",
    (name) => expect(tryQualifiedName(name).ok).toBe(false),
  )

  test.each([
    ['Name', null, 'Name'],
    ['Parent.Name', 'Parent', 'Name'],
    ['local.Project.Parent.Name', 'local.Project.Parent', 'Name'],
  ])(
    "Qualified name '%s' parent is '%s' and the last segment is '%s'",
    (name, parent, lastSegment) => {
      const qn = unwrap(tryQualifiedName(name))
      expect(qnLastSegment(qn)).toBe(lastSegment)
      expect(qnParent(qn)).toBe(parent)
      expect(qnSplit(qn)).toStrictEqual([parent, lastSegment])
      if (parent != null) {
        const qnParent = unwrap(tryQualifiedName(parent))
        const qnLastSegment = unwrap(tryIdentifierOrOperatorIdentifier(lastSegment))
        expect(qnParent).not.toBeNull()
        expect(qnLastSegment).not.toBeNull()
        expect(qnJoin(qnParent!, qnLastSegment!)).toBe(qn)
      }
    },
  )

  test.each([
    ['local.Project', true],
    ['local.Project.elem', true],
    ['local.Project.Module.elem', false],
  ])('qnIsTopElement(%s) returns %s', (name, result) => {
    const qn = unwrap(tryQualifiedName(name))
    expect(qnIsTopElement(qn)).toBe(result)
  })

  test.each([
    ['local.Project.Main', 'local.Project'],
    ['Standard.Table.Main', 'Standard.Table'],
    ['Standard.Table.Main.Table', 'Standard.Table.Table'],
    ['Some.Path.Without.Main.Module', 'Some.Path.Without.Main.Module'],
    ['Standard.Base', 'Standard.Base'],
  ])('normalizeQualifiedName drops Main module in %s', (name, expected) => {
    const qn = unwrap(tryQualifiedName(name))
    expect(normalizeQualifiedName(qn)).toEqual(unwrap(tryQualifiedName(expected)))
  })
}

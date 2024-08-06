import { unwrap } from '@/util/data/result'
import {
  normalizeQualifiedName,
  qnIsTopElement,
  qnJoin,
  qnLastSegment,
  qnParent,
  qnReplaceProjectName,
  qnSplit,
  tryIdentifier,
  tryIdentifierOrOperatorIdentifier,
  tryQualifiedName,
  type IdentifierOrOperatorIdentifier,
  type QualifiedName,
} from '@/util/qualifiedName'
import { expect, test } from 'vitest'

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
  '!=',
]
const invalidIdentifiers = ['', '1', '1Abc', '1_', 'abA!', '$a', 'a$']
// These are not valid identifiers but currently pass the qualified name regex: ['_', '.*', '.']

test.each(validIdentifiers)("'%s' is a valid identifier", (name) =>
  expect(unwrap(tryIdentifierOrOperatorIdentifier(name))).toStrictEqual(
    name as IdentifierOrOperatorIdentifier,
  ),
)
test.each(invalidIdentifiers)("'%s' is an invalid identifier", (name) =>
  expect(tryIdentifierOrOperatorIdentifier(name).ok).toBe(false),
)

test.each(validIdentifiers.concat('A._', 'a19_r14.zz9z', 'a.b.c.d.e.F', 'Standard.Base.Number.+'))(
  "'%s' is a valid qualified name",
  (name) => expect(unwrap(tryQualifiedName(name))).toStrictEqual(name as QualifiedName),
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

test.each([
  ['local.Project.Main', 'Project', 'NewProject', 'local.NewProject.Main'],
  ['local.Project.Main', 'Project2', 'NewProject', 'local.Project.Main'],
  ['local.Project', 'Project', 'NewProject', 'local.NewProject'],
  ['Project', 'Project', 'NewProject', 'Project'],
  ['local.Project2.Project', 'Project', 'NewProject', 'local.Project2.Project'],
])('Replacing project name in %s from %s to %s', (qname, oldName, newName, expected) => {
  const qn = unwrap(tryQualifiedName(qname))
  const newIdent = unwrap(tryIdentifier(newName))
  expect(qnReplaceProjectName(qn, oldName, newIdent)).toBe(expected)
})

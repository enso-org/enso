import { mockProjectNameStore } from '@/stores/projectNames'
import { ProjectPath } from '@/util/projectPath'
import { type QualifiedName } from '@/util/qualifiedName'
import { expect, test } from 'vitest'

const cases = [
  {
    qn: 'Standard.Base.Main' as QualifiedName,
    path: ProjectPath.create('Standard.Base' as QualifiedName, 'Main' as QualifiedName),
    normalized: ProjectPath.create('Standard.Base' as QualifiedName, undefined),
    normalizedQn: 'Standard.Base' as QualifiedName,
  },
  {
    qn: 'Standard.Base.Main.my_func' as QualifiedName,
    path: ProjectPath.create('Standard.Base' as QualifiedName, 'Main.my_func' as QualifiedName),
    normalized: ProjectPath.create('Standard.Base' as QualifiedName, 'my_func' as QualifiedName),
    normalizedQn: 'Standard.Base.my_func' as QualifiedName,
  },
  {
    qn: 'local.Mock_Project.Main' as QualifiedName,
    path: ProjectPath.create(undefined, 'Main' as QualifiedName),
  },
  {
    qn: 'local.Mock_Project.Main.my_func' as QualifiedName,
    path: ProjectPath.create(undefined, 'Main.my_func' as QualifiedName),
  },
]

const projectNames = mockProjectNameStore()

test.each(cases)('Parse', ({ qn, path }) => {
  expect(projectNames.parseProjectPath(qn).toJSON()).toStrictEqual(path.toJSON())
})

test.each(cases)('Normalize', ({ path, normalized }) => {
  if (normalized) expect(path.normalized().toJSON()).toStrictEqual(normalized.toJSON())
})

test.each(cases)('Serialize', ({ path, qn }) => {
  expect(projectNames.serializeProjectPathForBackend(path)).toStrictEqual(qn)
})

test.each(cases)('Print', ({ path, normalized, qn, normalizedQn }) => {
  if (normalizedQn) {
    expect(projectNames.printProjectPath(path)).toStrictEqual(normalizedQn ?? qn)
    expect(projectNames.printProjectPath(normalized)).toStrictEqual(normalizedQn ?? qn)
  }
})

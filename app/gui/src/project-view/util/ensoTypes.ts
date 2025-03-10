import { ProjectPath } from '@/util/projectPath'
import { tryQualifiedName } from '@/util/qualifiedName'
import { unwrap } from 'ydoc-shared/util/data/result'

export const ANY_TYPE = ProjectPath.create(
  unwrap(tryQualifiedName('Standard.Base')),
  unwrap(tryQualifiedName('Any.Any')),
)
export const ANY_TYPE_QN = unwrap(tryQualifiedName('Standard.Base.Any.Any'))

import { ProjectPath } from '@/util/projectPath'
import { tryQualifiedName } from '@/util/qualifiedName'
import { unwrap } from 'enso-common/src/utilities/data/result'

export const ANY_TYPE = ProjectPath.create(
  unwrap(tryQualifiedName('Standard.Base')),
  unwrap(tryQualifiedName('Any.Any')),
)
export const ANY_TYPE_QN = unwrap(tryQualifiedName('Standard.Base.Any.Any'))

import type { Identifier } from '@/util/ast/abstract'
import { ProjectPath } from '@/util/projectPath'
import type { QualifiedName } from '@/util/qualifiedName'
export const WIDGETS_ENSO_MODULE = 'Standard.Visualization.Widgets'
export const GET_WIDGETS_METHOD = 'get_widget_json' as Identifier
export const WIDGETS_ENSO_PATH = ProjectPath.create(
  'Standard.Visualization' as QualifiedName,
  'Widgets' as Identifier,
)

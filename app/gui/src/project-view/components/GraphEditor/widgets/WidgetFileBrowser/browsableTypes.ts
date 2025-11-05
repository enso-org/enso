import { type WidgetUpdate } from '$/providers/openedProjects/widgetRegistry'
import { ExpressionTag } from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { type PortId } from '@/providers/portInfo'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { DynamicConfig } from '@/util/callTree'
import { methodPointerEquals, type MethodPointer } from '@/util/methodPointer'
import { ProjectPath, printAbsoluteProjectPath, type AbsoluteProjectPath } from '@/util/projectPath'
import { qnJoin, type Identifier, type QualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue, type ComputedRef } from 'vue'

export type BrowserItem = 'file' | 'directory' | 'secret'

const FILE_MODULE_PROJECT = 'Standard.Base' as QualifiedName
const FILE_MODULE_PATH = 'System.File' as QualifiedName
const FILE_MODULE = ProjectPath.create(FILE_MODULE_PROJECT, FILE_MODULE_PATH)
const FILE_TYPE_UNQUALIFIED = 'File' as Identifier
const FILE_TYPE = ProjectPath.create(
  FILE_MODULE_PROJECT,
  qnJoin(FILE_MODULE_PATH, FILE_TYPE_UNQUALIFIED),
)
const FILE_TYPE_QN = printAbsoluteProjectPath(FILE_TYPE)
const FILE_CONSTRUCTOR: MethodPointer = {
  module: FILE_MODULE,
  definedOnType: FILE_TYPE,
  name: 'new' as Identifier,
}
const WRITABLE_FILE_MODULE = 'Standard.Base.System.File.Generic.Writable_File'
const WRITABLE_FILE_TYPE_QN = WRITABLE_FILE_MODULE + '.Writable_File'

const ENSO_SECRET_MODULE_PROJECT = 'Standard.Base' as QualifiedName
const ENSO_SECRET_MODULE_PATH = 'Enso_Cloud.Enso_Secret' as QualifiedName
const ENSO_SECRET_MODULE = ProjectPath.create(ENSO_SECRET_MODULE_PROJECT, ENSO_SECRET_MODULE_PATH)
const ENSO_SECRET_TYPE_UNQUALIFIED = 'Enso_Secret' as Identifier
const ENSO_SECRET_TYPE = ProjectPath.create(
  ENSO_SECRET_MODULE_PROJECT,
  qnJoin(ENSO_SECRET_MODULE_PATH, ENSO_SECRET_TYPE_UNQUALIFIED),
)
const ENSO_SECRET_CONSTRUCTOR: MethodPointer = {
  module: ENSO_SECRET_MODULE,
  definedOnType: ENSO_SECRET_TYPE,
  name: 'get' as Identifier,
}
const ENSO_SECRET_TYPE_QN = printAbsoluteProjectPath(ENSO_SECRET_TYPE)

const TEXT_TYPE = ProjectPath.create(
  'Standard.Base' as QualifiedName,
  'Data.Text.Text' as QualifiedName,
)
const TEXT_TYPE_QN = printAbsoluteProjectPath(TEXT_TYPE)

export const SUPPORTED_DYNAMIC_CONFIG_KINDS = ['File_Browse', 'Folder_Browse', 'Secret_Browse']
export const SUPPORTED_TYPES = [FILE_TYPE_QN, WRITABLE_FILE_TYPE_QN, ENSO_SECRET_TYPE_QN]

/** Information about the type(s) of browsable items. */
export interface BrowserTypeInfo {
  /** What should be selectable. */
  types: {
    file: boolean
    directory: boolean
    secret: boolean
  }
  /** If true, selection of non-existent paths will be enabled. */
  write: boolean
  /**
   * If `null`, a plain text literal is not a valid path representation for this type; if non-null,
   * contains additional properties affecting the usage of plain text literals.
   */
  rawPath: null | RawPathAllowedProperties
}

/** When raw paths are allowed for a browser type, these properties are applicable to them. */
export interface RawPathAllowedProperties {
  /**
   * If `true`, a selected value is inserted as a plain path by default (instead of a constructor).
   */
  prefer: boolean
}

/**
 * @returns The lowest input not less than zero, or if all inputs are less than zero returns one of
 * the inputs.
 */
function minNonNegative(a: number, b: number) {
  return (
    a < 0 ? b
    : b < 0 ? a
    : Math.min(a, b)
  )
}

/** Determines the type(s) of browsable items. */
export function useBrowserTypeInfo({
  reprType,
  dynamicConfig,
}: {
  reprType: ToValue<string | undefined>
  dynamicConfig: ToValue<DynamicConfig | undefined>
}): ComputedRef<BrowserTypeInfo> {
  const reprTypeInfo = computed(() => {
    const type = toValue(reprType)
    if (!type) return null
    const allowSecret = type.includes(ENSO_SECRET_TYPE_QN)
    const writableFilePos = type.indexOf(WRITABLE_FILE_TYPE_QN)
    const filePos = type.indexOf(FILE_TYPE_QN)
    const firstFilePos = minNonNegative(filePos, writableFilePos)
    const textTypePos = type.indexOf(TEXT_TYPE_QN)
    const allowFile = firstFilePos !== -1
    return {
      types: { file: allowFile, directory: false, secret: allowSecret },
      write: writableFilePos !== -1,
      rawPath: allowFile && textTypePos !== -1 ? { prefer: textTypePos < firstFilePos } : null,
    }
  })

  const FILE_ONLY = { file: true, directory: false, secret: false }
  const DIRECTORY_ONLY = { file: false, directory: true, secret: false }
  const SECRET_ONLY = { file: false, directory: false, secret: true }

  return computed((): BrowserTypeInfo => {
    const config = toValue(dynamicConfig)
    switch (config?.kind) {
      case 'File_Browse':
        return {
          types: FILE_ONLY,
          write: !config.existing_only,
          rawPath: reprTypeInfo.value?.rawPath ?? null,
        }
      case 'Folder_Browse':
        return {
          types: DIRECTORY_ONLY,
          write: false,
          rawPath: reprTypeInfo.value?.rawPath ?? null,
        }
      case 'Secret_Browse':
        return { types: SECRET_ONLY, write: false, rawPath: null }
      default:
        return reprTypeInfo.value ?? { types: FILE_ONLY, write: false, rawPath: null }
    }
  })
}

interface CurrentPath {
  type: 'file' | 'secret'
  path: Ast.TextLiteral
}

/**
 * If the input AST is a path representation compatible with the given type info, return the text
 * literal of the path in the input.
 */
export function useCurrentPath({
  typeInfo,
  input,
  getMethodPointer,
}: {
  typeInfo: ToValue<Omit<BrowserTypeInfo, 'write'>>
  input: ToValue<Ast.Expression | string | undefined>
  getMethodPointer: (id: Ast.AstId) => MethodPointer | undefined
}): ComputedRef<CurrentPath | undefined> {
  const inputAst = computed(() => {
    const inputValue = toValue(input)
    return typeof inputValue === 'object' ? Ast.unwrapGroups(inputValue) : undefined
  })

  function isMethodCall(value: Ast.Expression | string | undefined, expected: MethodPointer) {
    return typeof value === 'object' && methodPointerEquals(getMethodPointer(value.id), expected)
  }

  const allowRawPath = computed(() => toValue(typeInfo).rawPath != null)
  const inputFileValue = computed((): Ast.TextLiteral | undefined => {
    const ast = inputAst.value
    return (
      (
        isMethodCall(ast, FILE_CONSTRUCTOR) &&
          ast instanceof Ast.App &&
          ast.argument instanceof Ast.TextLiteral
      ) ?
        ast.argument
      : allowRawPath.value && ast instanceof Ast.TextLiteral ? ast
      : undefined
    )
  })

  const inputSecretValue = computed((): Ast.TextLiteral | undefined => {
    const ast = inputAst.value
    return (
        isMethodCall(ast, ENSO_SECRET_CONSTRUCTOR) &&
          ast instanceof Ast.App &&
          ast.argument instanceof Ast.TextLiteral
      ) ?
        ast.argument
      : undefined
  })

  return computed((): CurrentPath | undefined => {
    const { file, directory, secret } = toValue(typeInfo).types
    return (
      secret && inputSecretValue.value ?
        {
          type: 'secret',
          path: inputSecretValue.value,
        }
      : (file || directory) && inputFileValue.value ?
        {
          type: 'file',
          path: inputFileValue.value,
        }
      : undefined
    )
  })
}

/** Supports creating a widget update for a selected browsable item. */
export function useSetPath({
  currentPath,
  preferRawPath,
  portId,
  addMissingConstructorImports,
}: {
  currentPath: ToValue<CurrentPath | undefined>
  preferRawPath: ToValue<boolean>
  portId: ToValue<PortId>
  addMissingConstructorImports: (module: Ast.MutableModule, type: ProjectPath) => boolean
}) {
  const FILE_CONSTRUCTOR_PATTERN = {
    default: Pattern.parseExpression(`(${FILE_TYPE_UNQUALIFIED}.new __)`),
    full: Pattern.parseExpression(`(${FILE_TYPE_QN}.new __)`),
  }
  function makeFile(
    module: Ast.MutableModule,
    path: Ast.Owned<Ast.MutableTextLiteral>,
    qnStyle: 'default' | 'full',
  ) {
    return FILE_CONSTRUCTOR_PATTERN[qnStyle].instantiate(module, [path])
  }

  const ENSO_SECRET_PATTERN = {
    default: Pattern.parseExpression(`(${ENSO_SECRET_TYPE_UNQUALIFIED}.get __)`),
    full: Pattern.parseExpression(`(${ENSO_SECRET_TYPE_QN}.get __)`),
  }
  function makeSecret(
    module: Ast.MutableModule,
    path: Ast.Owned<Ast.MutableTextLiteral>,
    qnStyle: 'default' | 'full',
  ) {
    return ENSO_SECRET_PATTERN[qnStyle].instantiate(module, [path])
  }

  type MakeValue = (
    module: Ast.MutableModule,
    path: Ast.Owned<Ast.MutableTextLiteral>,
    qnStyle: 'default' | 'full',
  ) => Ast.Owned<Ast.MutableExpression>

  function widgetUpdate(
    currentPath: Ast.TextLiteral | undefined,
    path: string,
    {
      type,
      makeValue,
      preferRawPath,
    }: { type: AbsoluteProjectPath; makeValue: MakeValue; preferRawPath: boolean },
    edit: Ast.MutableModule,
  ): WidgetUpdate {
    if (currentPath || preferRawPath) {
      return {
        portUpdate: {
          value: Ast.TextLiteral.new(path),
          origin: currentPath?.id ?? toValue(portId),
        },
        directInteraction: true,
      }
    } else {
      const importsOk = addMissingConstructorImports(edit, type)
      const pathText = Ast.TextLiteral.new(path, edit)
      const pathStyle = importsOk ? 'default' : 'full'
      return {
        portUpdate: {
          value: makeValue(edit, pathText, pathStyle),
          origin: toValue(portId),
        },
        edit,
        directInteraction: true,
      }
    }
  }

  function setPath(type: 'file' | 'secret', path: string, edit: Ast.MutableModule) {
    const oldPathInfo = toValue(currentPath)
    const oldPath = oldPathInfo?.type === type ? oldPathInfo.path : undefined
    return widgetUpdate(
      oldPath,
      path,
      type === 'secret' ?
        {
          type: ENSO_SECRET_TYPE,
          makeValue: makeSecret,
          preferRawPath: false,
        }
      : {
          type: FILE_TYPE,
          makeValue: makeFile,
          preferRawPath: toValue(preferRawPath),
        },
      edit,
    )
  }

  return setPath
}

const secretTextItem = new ExpressionTag("''", 'Provide secret as text', 'text')

/** @returns Dropdown items for providing an Enso Secret as plain text. */
export function useTextSecrets({
  dialogKind,
  reprType,
}: {
  dialogKind: ToValue<BrowserItem>
  reprType: ToValue<string | undefined>
}): ComputedRef<ExpressionTag[]> {
  return computed(() =>
    toValue(dialogKind) === 'secret' && toValue(reprType)?.includes(TEXT_TYPE_QN) ?
      [secretTextItem]
    : [],
  )
}

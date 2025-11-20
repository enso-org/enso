import {
  useBrowserTypeInfo,
  useCurrentPath,
  useSetPath,
  type BrowserTypeInfo,
} from '@/components/GraphEditor/widgets/WidgetFileBrowser/browsableTypes'
import { assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { DynamicConfig } from '@/util/callTree'
import { parseAbsoluteProjectPathRaw } from '@/util/projectPath'
import type { Identifier } from '@/util/qualifiedName'
import { unwrap } from 'enso-common/src/utilities/data/result'
import { describe, expect, test, vi } from 'vitest'

const FILE_TYPE_QN = 'Standard.Base.System.File.File'
const WRITABLE_FILE_TYPE_QN = 'Standard.Base.System.File.Generic.Writable_File.Writable_File'
const TEXT_TYPE_QN = 'Standard.Base.Data.Text.Text'
const ENSO_SECRET_TYPE_QN = 'Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret'

interface TypeInfoCase {
  description: string
  input: {
    types: string[] | undefined
    dynamicConfig: DynamicConfig | undefined
  }
  expected: Partial<BrowserTypeInfo>
}

test.each<TypeInfoCase>([
  {
    description: 'By type, file, constructor required, read',
    input: {
      types: [FILE_TYPE_QN],
      dynamicConfig: undefined,
    },
    expected: {
      types: { file: true, directory: false, secret: false },
      write: false,
      rawPath: null,
    },
  },
  {
    description: 'By type, file, constructor required, write',
    input: {
      types: [WRITABLE_FILE_TYPE_QN],
      dynamicConfig: undefined,
    },
    expected: {
      types: { file: true, directory: false, secret: false },
      write: true,
      rawPath: null,
    },
  },
  {
    description: 'By type, file, raw path allowed, read',
    input: {
      types: [FILE_TYPE_QN, TEXT_TYPE_QN],
      dynamicConfig: undefined,
    },
    expected: {
      types: { file: true, directory: false, secret: false },
      write: false,
      rawPath: { prefer: false },
    },
  },
  {
    description: 'By type, file, raw path allowed, write',
    input: {
      types: [WRITABLE_FILE_TYPE_QN, TEXT_TYPE_QN],
      dynamicConfig: undefined,
    },
    expected: {
      types: { file: true, directory: false, secret: false },
      write: true,
      rawPath: { prefer: false },
    },
  },
  {
    description: 'By type, file, raw path preferred, read',
    input: {
      types: [TEXT_TYPE_QN, FILE_TYPE_QN],
      dynamicConfig: undefined,
    },
    expected: {
      types: { file: true, directory: false, secret: false },
      write: false,
      rawPath: { prefer: true },
    },
  },
  {
    description: 'By type, file, raw path preferred, write',
    input: {
      types: [TEXT_TYPE_QN, WRITABLE_FILE_TYPE_QN],
      dynamicConfig: undefined,
    },
    expected: {
      types: { file: true, directory: false, secret: false },
      write: true,
      rawPath: { prefer: true },
    },
  },
  ...[
    [ENSO_SECRET_TYPE_QN],
    [ENSO_SECRET_TYPE_QN, TEXT_TYPE_QN],
    [TEXT_TYPE_QN, ENSO_SECRET_TYPE_QN],
  ].map<TypeInfoCase>((types) => ({
    description: `By type, secret, types: ${JSON.stringify(types)}`,
    input: {
      types,
      dynamicConfig: undefined,
    },
    expected: {
      types: { file: false, directory: false, secret: true },
      write: false,
    },
  })),
  ...[undefined, [FILE_TYPE_QN], [WRITABLE_FILE_TYPE_QN]].flatMap<TypeInfoCase>((types) => [
    {
      description: `By config, file, read, types: ${JSON.stringify(types)}`,
      input: {
        types,
        dynamicConfig: {
          kind: 'File_Browse',
          // eslint-disable-next-line camelcase
          existing_only: true,
        },
      },
      expected: {
        types: { file: true, directory: false, secret: false },
        write: false,
      },
    },
    {
      description: `By config, file, write, types: ${JSON.stringify(types)}`,
      input: {
        types,
        dynamicConfig: {
          kind: 'File_Browse',
          // eslint-disable-next-line camelcase
          existing_only: false,
        },
      },
      expected: {
        types: { file: true, directory: false, secret: false },
        write: true,
      },
    },
  ]),
  ...[undefined, [FILE_TYPE_QN]].map<TypeInfoCase>((types) => ({
    description: `By config, folder, types: ${JSON.stringify(types)}`,
    input: {
      types,
      dynamicConfig: {
        kind: 'Folder_Browse',
      },
    },
    expected: {
      types: { file: false, directory: true, secret: false },
      write: false,
    },
  })),
])('Type info: $description', ({ input: { types, dynamicConfig }, expected }) => {
  const typeInfo = useBrowserTypeInfo({
    reprType: types?.join(' | '),
    dynamicConfig,
  }).value
  expect(typeInfo).toMatchObject(expected)
})

test.each([
  {
    description: 'Path from file constructor argument',
    input: {
      typeInfo: {
        types: { file: true, directory: false, secret: false },
        rawPath: null,
      },
      code: '(File.new "path")',
    },
    expected: { type: 'file', path: 'path' },
  },
  {
    description: 'Path from secret constructor argument',
    input: {
      typeInfo: {
        types: { file: false, directory: false, secret: true },
        rawPath: null,
      },
      code: '(Enso_Secret.get "path")',
    },
    expected: { type: 'secret', path: 'path' },
  },
  {
    description: 'Raw path',
    input: {
      typeInfo: {
        types: { file: true, directory: false, secret: false },
        rawPath: { prefer: false },
      },
      code: '"path"',
    },
    expected: { type: 'file', path: 'path' },
  },
  {
    description: 'Raw path, parenthesized',
    input: {
      typeInfo: {
        types: { file: true, directory: false, secret: false },
        rawPath: { prefer: false },
      },
      code: '("path")',
    },
    expected: { type: 'file', path: 'path' },
  },
  {
    description: 'Unexpected raw path',
    input: {
      typeInfo: {
        types: { file: true, directory: false, secret: false },
        rawPath: null,
      },
      code: '"path"',
    },
    expected: undefined,
  },
  {
    description: 'Unexpected secret constructor',
    input: {
      typeInfo: {
        types: { file: true, directory: false, secret: false },
        rawPath: null,
      },
      code: '(Enso_Secret.get "path")',
    },
    expected: undefined,
  },
  {
    description: 'Unexpected file constructor',
    input: {
      typeInfo: {
        types: { file: false, directory: false, secret: true },
        rawPath: null,
      },
      code: '(File.new "path")',
    },
    expected: undefined,
  },
])('Current path: $description', ({ input: { typeInfo, code }, expected }) => {
  const argumentAst = Ast.parseExpression(code)
  assertDefined(argumentAst)
  const currentPathAst = useCurrentPath({
    typeInfo,
    input: argumentAst,
    getMethodPointer: (id) => {
      const ast = argumentAst.module.get(id)
      if (!(ast instanceof Ast.App)) return undefined
      const code = ast.function.code()
      if (code.endsWith('Enso_Secret.get'))
        return {
          module: unwrap(parseAbsoluteProjectPathRaw('Standard.Base.Enso_Cloud.Enso_Secret')),
          definedOnType: unwrap(
            parseAbsoluteProjectPathRaw('Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret'),
          ),
          name: 'get' as Identifier,
        }
      if (code.endsWith('File.new'))
        return {
          module: unwrap(parseAbsoluteProjectPathRaw('Standard.Base.System.File')),
          definedOnType: unwrap(parseAbsoluteProjectPathRaw('Standard.Base.System.File.File')),
          name: 'new' as Identifier,
        }
    },
  })
  expect(
    currentPathAst.value && {
      type: currentPathAst.value.type,
      path: currentPathAst.value.path.rawTextContent,
    },
  ).toEqual(expected)
})

describe('Set path', () => {
  test('File, prefer raw, no current path', () => {
    const addMissingConstructorImports = vi.fn()
    const portId = 'MOCK-PORT-ID' as Ast.AstId
    const setPath = useSetPath({
      currentPath: undefined,
      preferRawPath: true,
      portId,
      addMissingConstructorImports,
    })
    const widgetUpdate = setPath('file', 'New path', Ast.MutableModule.Transient())
    expect(widgetUpdate.portUpdate?.origin).toBe(portId)
    expect(widgetUpdate.portUpdate).toHaveProperty('value')
    expect((widgetUpdate.portUpdate as any).value.code()).toBe("'New path'")
    expect(addMissingConstructorImports).not.toHaveBeenCalled()
  })

  test.each(['file', 'secret'] as const)('Replace current path, type: %s', (type) => {
    const addMissingConstructorImports = vi.fn()
    const currentPath = Ast.TextLiteral.tryParse("'Old path'")
    assertDefined(currentPath)
    const setPath = useSetPath({
      currentPath: { type, path: currentPath },
      preferRawPath: true,
      portId: 'MOCK-PORT-ID' as Ast.AstId,
      addMissingConstructorImports,
    })
    const widgetUpdate = setPath(type, 'New path', Ast.MutableModule.Transient())
    expect(widgetUpdate.portUpdate?.origin).toBe(currentPath.id)
    expect(widgetUpdate.portUpdate).toHaveProperty('value')
    expect((widgetUpdate.portUpdate as any).value.code()).toBe("'New path'")
    expect(addMissingConstructorImports).not.toHaveBeenCalled()
  })

  test.each([
    {
      type: 'file' as const,
      conflicts: false,
      expected: "(File.new 'New path')",
    },
    {
      type: 'file' as const,
      conflicts: true,
      expected: "(Standard.Base.System.File.File.new 'New path')",
    },
    {
      type: 'secret' as const,
      conflicts: false,
      expected: "(Enso_Secret.get 'New path')",
    },
    {
      type: 'secret' as const,
      conflicts: true,
      expected: "(Standard.Base.Enso_Cloud.Enso_Secret.Enso_Secret.get 'New path')",
    },
  ])('Set path with constructor: $type, conflicts=$conflicts', ({ type, conflicts, expected }) => {
    const module = Ast.MutableModule.Transient()
    const addMissingConstructorImports = vi.fn(() => !conflicts)
    const portId = 'MOCK-PORT-ID' as Ast.AstId
    const setPath = useSetPath({
      currentPath: { type: 'wrong-type' as any, path: Ast.TextLiteral.tryParse("'Old path'")! },
      preferRawPath: false,
      portId,
      addMissingConstructorImports,
    })
    const widgetUpdate = setPath(type, 'New path', module)
    expect(widgetUpdate.portUpdate?.origin).toBe(portId)
    expect(widgetUpdate.portUpdate).toHaveProperty('value')
    expect((widgetUpdate.portUpdate as any).value.code()).toBe(expected)
    expect(addMissingConstructorImports).toHaveBeenCalled()
  })
})

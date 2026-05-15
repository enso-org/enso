/**
 * @file Unit tests for the AI-node AST helpers: documentation prefix matching, the
 * call-site → FunctionDef lookup, and the two AST builders ({@link createAiNode},
 * {@link updateAiNode}). Operates directly on a transient module so we don't need a live
 * graph store or Yjs document.
 */
import {
  AI_COMMENT_PREFIX,
  createAiNode,
  isAiAssignment,
  readAiCallTarget,
  readAiPrompt,
  updateAiNode,
} from '@/components/GraphEditor/aiNode'
import { Ast } from '@/util/ast'
import { Vec2 } from '@/util/data/vec2'
import { tryIdentifier } from '@/util/qualifiedName'
import type { AiComponentResponse } from 'enso-common/src/ai'
import { unwrap } from 'enso-common/src/utilities/data/result'
import { describe, expect, test } from 'vitest'

const STARTING_MODULE = ['main =', '    x = 1', ''].join('\n')

function buildModule(code: string = STARTING_MODULE) {
  const edit = Ast.MutableModule.Transient()
  const topLevel = Ast.parseModule(code, edit)
  edit.setRoot(topLevel)
  return { edit, topLevel }
}

function exampleResponse(overrides: Partial<AiComponentResponse> = {}): AiComponentResponse {
  return {
    functionName: 'ai_generated',
    argumentNames: ['arg1'],
    body: 'arg1 + 1',
    callArguments: ['x'],
    ...overrides,
  }
}

describe('readAiPrompt', () => {
  test('returns null for undefined documentation', () => {
    expect(readAiPrompt(undefined)).toBeNull()
  })

  test('returns null for plain documentation', () => {
    expect(readAiPrompt('User notes about the node')).toBeNull()
  })

  test('strips the prefix and returns the prompt body', () => {
    expect(readAiPrompt(`${AI_COMMENT_PREFIX}count letters`)).toBe('count letters')
  })

  test('treats a prefix-only string as an empty prompt', () => {
    expect(readAiPrompt(AI_COMMENT_PREFIX)).toBe('')
  })
})

describe('isAiAssignment', () => {
  test('is true for an Assignment with an AI documentation prefix', () => {
    const edit = Ast.MutableModule.Transient()
    const binding = unwrap(tryIdentifier('ai_node1'))
    const expression = Ast.parseExpression('Main.ai_generated 1', edit)!
    const assignment = Ast.Assignment.new(binding, expression, {
      edit,
      documentation: `${AI_COMMENT_PREFIX}count things`,
    })
    expect(isAiAssignment(assignment)).toBe(true)
  })

  test('is false for an Assignment without AI documentation', () => {
    const edit = Ast.MutableModule.Transient()
    const binding = unwrap(tryIdentifier('plain'))
    const expression = Ast.parseExpression('1 + 2', edit)!
    const assignment = Ast.Assignment.new(binding, expression, { edit })
    expect(isAiAssignment(assignment)).toBe(false)
  })

  test('is false for non-Assignment statements', () => {
    const edit = Ast.MutableModule.Transient()
    const expr = Ast.parseExpression('42', edit)!
    expect(isAiAssignment(expr)).toBe(false)
  })
})

describe('createAiNode', () => {
  test('inserts a FunctionDef before the current method and adds the call to its body', () => {
    const { edit, topLevel } = buildModule()
    const result = createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'do the thing', response: exampleResponse() },
    })
    expect(result.ok).toBe(true)
    const moduleCode = topLevel.code()
    // The new FunctionDef goes before `main`.
    expect(moduleCode.indexOf('ai_generated')).toBeLessThan(moduleCode.indexOf('main'))
    // The call site lives inside main.
    expect(moduleCode).toContain('ai_node1 = Main.ai_generated x')
    // Documentation captures the prompt verbatim.
    expect(moduleCode).toContain(`${AI_COMMENT_PREFIX}do the thing`)
  })

  test('rejects an invalid function name', () => {
    const { edit, topLevel } = buildModule()
    const result = createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: {
        prompt: 'p',
        response: exampleResponse({ functionName: 'Not An Identifier!' }),
      },
    })
    expect(result.ok).toBe(false)
  })

  test('rejects a mismatched callArguments / argumentNames length', () => {
    const { edit, topLevel } = buildModule()
    const result = createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: {
        prompt: 'p',
        response: exampleResponse({ argumentNames: ['a', 'b'], callArguments: ['x'] }),
      },
    })
    expect(result.ok).toBe(false)
  })

  test('renames the function on a collision with an existing top-level method', () => {
    const { edit, topLevel } = buildModule(
      ['ai_generated arg1 = arg1', 'main =', '    x = 1', ''].join('\n'),
    )
    const result = createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'p', response: exampleResponse() },
    })
    expect(result.ok).toBe(true)
    const moduleCode = topLevel.code()
    // `generateUniqueName` adds a `_<N>` suffix when the base name collides.
    expect(moduleCode).toMatch(/ai_generated_\d+\b/)
  })
})

describe('readAiCallTarget', () => {
  test('finds the FunctionDef + index for a Main.<name> call', () => {
    const { edit, topLevel } = buildModule()
    const created = createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'p', response: exampleResponse() },
    })
    expect(created.ok).toBe(true)
    // Recover the Assignment we just inserted into `main`.
    const found = Ast.findModuleMethod(topLevel, 'main')!
    const mainBody = found.statement.bodyAsBlock()
    const assignment = [...mainBody.statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!
    const target = readAiCallTarget(assignment, topLevel)
    expect(target).not.toBeNull()
    expect(target!.functionName).toBe('ai_generated')
    expect(target!.topLevelIndex).toBe(0)
    expect(target!.definitionCode).toContain('ai_generated arg1')
  })

  test('returns null when the assignment is not shaped like a Main.<name> call', () => {
    const edit = Ast.MutableModule.Transient()
    const binding = unwrap(tryIdentifier('plain'))
    const expression = Ast.parseExpression('1 + 2', edit)!
    const assignment = Ast.Assignment.new(binding, expression, { edit })
    const topLevel = Ast.parseModule('main = 1', edit)
    expect(readAiCallTarget(assignment, topLevel)).toBeNull()
  })
})

describe('updateAiNode', () => {
  test('rewrites the FunctionDef at the same line and replaces the call', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'original', response: exampleResponse() },
    })
    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const mainBody = mainStatement.bodyAsBlock()
    const assignmentId = [...mainBody.statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!.id
    const mutableAssignment = edit.get(assignmentId) as Ast.MutableAssignment

    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'main',
      prompt: 'rewritten',
      response: exampleResponse({
        functionName: 'ai_rewritten',
        argumentNames: ['n'],
        body: 'n * 2',
        callArguments: ['x'],
      }),
    })
    expect(result.ok).toBe(true)
    const moduleCode = topLevel.code()
    // The old function name is gone.
    expect(moduleCode).not.toContain('ai_generated')
    // The new function name appears at line index 0 (same slot as the old one).
    expect(moduleCode.indexOf('ai_rewritten')).toBeLessThan(moduleCode.indexOf('main'))
    // The call now reflects the new name; binding identifier is preserved.
    expect(moduleCode).toContain('ai_node1 = Main.ai_rewritten x')
    // Documentation was rewritten too.
    expect(moduleCode).toContain(`${AI_COMMENT_PREFIX}rewritten`)
    expect(moduleCode).not.toContain(`${AI_COMMENT_PREFIX}original`)
  })

  test('rejects when the agent returns an invalid function name', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'p', response: exampleResponse() },
    })
    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const assignmentId = [...mainStatement.bodyAsBlock().statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!.id
    const mutableAssignment = edit.get(assignmentId) as Ast.MutableAssignment
    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'main',
      prompt: 'rewritten',
      response: exampleResponse({ functionName: 'Not An Identifier!' }),
    })
    expect(result.ok).toBe(false)
  })

  test('rejects when the agent returns an invalid argument name', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'p', response: exampleResponse() },
    })
    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const assignmentId = [...mainStatement.bodyAsBlock().statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!.id
    const mutableAssignment = edit.get(assignmentId) as Ast.MutableAssignment
    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'main',
      prompt: 'rewritten',
      response: exampleResponse({
        argumentNames: ['Bad Name!'],
        callArguments: ['x'],
      }),
    })
    expect(result.ok).toBe(false)
  })

  test('rejects when callArguments and argumentNames have mismatched lengths', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'p', response: exampleResponse() },
    })
    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const assignmentId = [...mainStatement.bodyAsBlock().statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!.id
    const mutableAssignment = edit.get(assignmentId) as Ast.MutableAssignment
    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'main',
      prompt: 'rewritten',
      response: exampleResponse({ argumentNames: ['a', 'b'], callArguments: ['x'] }),
    })
    expect(result.ok).toBe(false)
  })

  test('allows the agent to keep the same function name on edit', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'p', response: exampleResponse() },
    })
    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const assignmentId = [...mainStatement.bodyAsBlock().statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!.id
    const mutableAssignment = edit.get(assignmentId) as Ast.MutableAssignment
    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'main',
      prompt: 'updated',
      response: exampleResponse({ body: 'arg1 + 99' }),
    })
    expect(result.ok).toBe(true)
    const moduleCode = topLevel.code()
    expect(moduleCode).toContain('ai_generated')
    // The unique-name check shouldn't have bumped the function to `ai_generated_<N>` — the
    // old definition was removed first so there is no collision to resolve.
    expect(moduleCode).not.toMatch(/ai_generated_\d+\b/)
    expect(moduleCode).toContain('arg1 + 99')
  })

  test('falls back to fresh insertion when the previous FunctionDef has been removed', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'original', response: exampleResponse() },
    })
    // Simulate the user manually deleting the AI-generated top-level FunctionDef.
    const oldFn = Ast.findModuleMethod(topLevel, 'ai_generated')!.statement
    const mutableOldFn = edit.get(oldFn.id) as Ast.MutableFunctionDef
    Ast.deleteFromParentBlock(mutableOldFn)
    expect(Ast.findModuleMethod(topLevel, 'ai_generated')).toBeUndefined()

    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const assignmentId = [...mainStatement.bodyAsBlock().statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!.id
    const mutableAssignment = edit.get(assignmentId) as Ast.MutableAssignment

    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'main',
      prompt: 'rewritten',
      response: exampleResponse({ functionName: 'ai_recovered', body: 'arg1' }),
    })
    expect(result.ok).toBe(true)
    const moduleCode = topLevel.code()
    expect(moduleCode).toContain('ai_recovered arg1')
    expect(moduleCode).toContain('ai_node1 = Main.ai_recovered x')
    expect(moduleCode.indexOf('ai_recovered')).toBeLessThan(moduleCode.indexOf('main'))
  })

  test('rejects fallback when the enclosing method cannot be located', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: Vec2.Zero,
      payload: { prompt: 'p', response: exampleResponse() },
    })
    const oldFn = Ast.findModuleMethod(topLevel, 'ai_generated')!.statement
    const mutableOldFn = edit.get(oldFn.id) as Ast.MutableFunctionDef
    Ast.deleteFromParentBlock(mutableOldFn)
    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const assignmentId = [...mainStatement.bodyAsBlock().statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!.id
    const mutableAssignment = edit.get(assignmentId) as Ast.MutableAssignment

    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'nonexistent_method',
      prompt: 'rewritten',
      response: exampleResponse(),
    })
    expect(result.ok).toBe(false)
  })

  test('preserves all node metadata across an edit', () => {
    const { edit, topLevel } = buildModule()
    createAiNode({
      edit,
      topLevel,
      currentMethodName: 'main',
      binding: unwrap(tryIdentifier('ai_node1')),
      position: new Vec2(42, 17),
      payload: { prompt: 'original', response: exampleResponse() },
    })
    const mainStatement = Ast.findModuleMethod(topLevel, 'main')!.statement
    const assignment = [...mainStatement.bodyAsBlock().statements()].find(
      (st): st is Ast.Assignment =>
        st instanceof Ast.Assignment && st.pattern.code() === 'ai_node1',
    )!
    const callBefore = edit.get(assignment.expression.id)!
    // Decorate the call expression with the full range of node-metadata fields the user might
    // have set, plus an arbitrary widget metadata blob.
    callBefore.setNodeMetadata({
      colorOverride: '#abcdef',
      displayMode: 'collapsed',
      visualization: {
        identifier: { module: { kind: 'CurrentProject' }, name: 'Table' },
        visible: true,
        width: 320,
        height: 240,
      },
    })
    callBefore.setWidgetMetadata('myWidget', { someState: 7 })
    const mutableAssignment = edit.get(assignment.id) as Ast.MutableAssignment

    const result = updateAiNode({
      edit,
      topLevel,
      assignment: mutableAssignment,
      currentMethodName: 'main',
      prompt: 'rewritten',
      response: exampleResponse({ functionName: 'ai_v2', body: 'arg1 * 2' }),
    })
    expect(result.ok).toBe(true)

    const newExpr = mutableAssignment.expression
    const meta = newExpr.nodeMetadata
    expect(meta.get('position')).toEqual({ x: 42, y: 17 })
    expect(meta.get('colorOverride')).toBe('#abcdef')
    expect(meta.get('displayMode')).toBe('collapsed')
    expect(meta.get('visualization')?.identifier?.name).toBe('Table')
    expect(newExpr.widgetMetadata('myWidget')).toEqual({ someState: 7 })
  })
})

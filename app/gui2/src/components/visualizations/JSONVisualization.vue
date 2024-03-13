<script lang="ts">
export const name = 'JSON'
export const icon = 'braces'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { useVisualizationConfig, VisualizationContainer } from '@/util/visualizationBuiltins'
import { isIdentifier } from 'shared/ast'
import { ref, watchEffect } from 'vue'

type Json = unknown
const props = defineProps<{ data: Json }>()

const config = useVisualizationConfig()

const content = ref<HTMLElement>()

/** Attaches a parent-pointer to Json data and supports traversal. */
abstract class JsonNode {
  public readonly data: Json
  public readonly parent: { node: JsonNode; selector: string | number } | undefined

  static new(data: Json, parent?: { node: JsonNode; selector: string | number }): JsonNode {
    return (
      Array.isArray(data) ? new JsonArray(data, parent)
      : typeof data === 'object' && data !== null ?
        new JsonObject(data as Record<string, Json>, parent)
      : new JsonPrimitive(data, parent)
    )
  }

  protected constructor(
    data: Json,
    parent: { node: JsonNode; selector: string | number } | undefined,
  ) {
    this.data = data
    this.parent = parent
  }

  path(): (string | number)[] {
    const keys = new Array<string | number>()
    let ix: JsonNode | undefined = this
    while (ix) {
      if (ix.parent) keys.push(ix.parent.selector)
      ix = ix.parent?.node
    }
    return keys.reverse()
  }

  pattern(): Pattern | undefined {
    let pattern: ((slot: Ast.Owned) => Ast.Owned) | undefined
    const module = Ast.MutableModule.Transient()
    const get = 'get'
    assert(isIdentifier(get))
    for (const selector of this.path()) {
      const parentPattern = pattern
      pattern = (source) =>
        Ast.App.positional(
          Ast.PropertyAccess.new(module, parentPattern ? parentPattern(source) : source, get, {
            spaced: parentPattern !== undefined,
          }),
          typeof selector === 'number' ?
            Ast.tryNumberToEnso(selector, module)!
          : Ast.TextLiteral.new(selector, module),
          module,
        )
    }
    return pattern && Pattern.new(pattern)
  }

  *children(): IterableIterator<JsonNode> {}
  childCount(): number {
    let count = 0
    for (const _child of this.children()) count += 1
    return count
  }
}

class JsonObject extends JsonNode {
  public declare readonly data: Record<string, Json>

  constructor(
    data: Record<string, Json>,
    parent: { node: JsonNode; selector: string | number } | undefined,
  ) {
    super(data, parent)
  }

  *children(): IterableIterator<JsonNode & ChildOfObject> {
    for (const [key, value] of Object.entries(this.data))
      yield JsonNode.new(value, { node: this, selector: key }) as JsonNode & ChildOfObject
  }

  keys(): string[] {
    return Object.keys(this.data)
  }
}
interface ChildOfObject {
  readonly parent: { node: JsonNode; selector: string }
}

class JsonArray extends JsonNode {
  public declare readonly data: Json[]

  constructor(data: Json[], parent: { node: JsonNode; selector: string | number } | undefined) {
    super(data, parent)
  }

  *children(): IterableIterator<JsonNode & ChildOfArray> {
    let index = 0
    for (const value of this.data) {
      yield JsonNode.new(value, { node: this, selector: index }) as JsonNode & ChildOfArray
      index += 1
    }
  }

  childCount(): number {
    return this.data.length
  }
}
interface ChildOfArray {
  readonly parent: { node: JsonNode; selector: number }
}

class JsonPrimitive extends JsonNode {
  constructor(data: Json, parent: { node: JsonNode; selector: string | number } | undefined) {
    super(data, parent)
  }
}

function render(value: JsonNode, depth: number = 0, parentInline?: boolean) {
  const html = document.createElement('span')
  const inline = parentInline || JSON.stringify(value.data).length <= 40
  if (value instanceof JsonArray) {
    html.append('[')
    let i = 0
    for (const element of value.children()) {
      if (i !== 0) html.append(',')
      html.append(inline ? ' ' : newline(depth + 1))
      const elementHtml = document.createElement('span')
      elementHtml.classList.add('element')
      elementHtml.append(render(element, depth + 1, inline))
      addClickHandler(elementHtml, element)
      html.append(elementHtml)
      i += 1
    }
    html.append(inline ? ' ' : newline(depth))
    html.append(']')
  } else if (value instanceof JsonObject) {
    html.append('{')
    let i = 0
    for (const field of value.children()) {
      if (i !== 0) html.append(',')
      html.append(inline ? ' ' : newline(depth + 1))
      const fieldHtml = document.createElement('span')
      fieldHtml.classList.add('field')
      fieldHtml.append(renderKey(field.parent.selector), ': ', render(field, depth + 1, inline))
      addClickHandler(fieldHtml, field)
      html.append(fieldHtml)
      i += 1
    }
    html.append(inline ? ' ' : newline(depth))
    html.append('}')
  } else {
    html.classList.add('primitive')
    html.append(JSON.stringify(value.data))
  }
  return html
}

function renderKey(data: string) {
  const element = document.createElement('span')
  element.classList.add('key')
  element.append(JSON.stringify(data))
  return element
}

function newline(indent: number) {
  return '\n' + ' '.repeat(indent)
}

function addClickHandler(element: HTMLElement, index: JsonNode) {
  if (!index.parent) return
  element.classList.add('button')
  const selector = index.parent.selector
  const target =
    index.parent.node instanceof JsonArray ?
      `element ${selector} of the array`
    : `the ${JSON.stringify(selector)} field`
  const parentChildren = index.parent.node.childCount()
  const shiftTargets =
    parentChildren < 2 ? undefined
    : index.parent.node instanceof JsonObject ?
      `all fields of the object (${Array.from(index.parent.node.keys(), (key) => JSON.stringify(key)).join(', ')})`
    : `all ${parentChildren} elements of the array`
  element.title =
    `Click to create a node selecting ${target}.` +
    (shiftTargets ? ` Shift-click to create nodes selecting ${shiftTargets}.` : '')
  element.addEventListener('click', (e) => onClick(e, index))
}

function onClick(e: MouseEvent, index: JsonNode) {
  e.stopPropagation()
  const paths = Array.from(e.shiftKey ? index.parent?.node.children() ?? [] : [index])
  const toCreate = new Array<NodeCreationOptions>()
  for (const path of paths) {
    const pattern = path.pattern()
    if (!pattern) continue
    toCreate.push({ content: pattern, commit: true })
  }
  config.createNodes(...toCreate)
}

watchEffect(() => {
  if (!content.value) return
  content.value.replaceChildren(render(JsonNode.new(props.data)))
})
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <div ref="content" class="JSONVisualization" @pointerdown.stop @pointerup.stop @click.stop />
  </VisualizationContainer>
</template>

<style scoped>
.JSONVisualization {
  font-family: var(--font-mono);
  white-space: pre;
  padding: 8px;
}

:deep(.key) {
  color: darkred;
}

:deep(.primitive) {
  color: green;
}
</style>

<script setup lang="ts">
import NodeToken from '@/components/GraphEditor/NodeToken.vue'
import { Ast, type AstExtended } from '@/util/ast'
import { useResizeObserver } from '@/util/events'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import type { ExprId } from 'shared/yjsModel'
import { computed, onUpdated, ref, shallowRef, watch } from 'vue'

const props = defineProps<{ nodeSpanStart: number; ast: AstExtended<Ast.Tree> }>()

const emit = defineEmits<{
  updateExprRect: [expr: ExprId, rect: Rect]
  updateHoveredExpr: [id: ExprId | undefined]
}>()

const rootNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode, false)
const exprRect = shallowRef<Rect>()

const spanClass = computed(() => Ast.Tree.typeNames[props.ast.inner.type])
const children = computed(() => [...props.ast.children()])
const isOnStart = computed(() => props.nodeSpanStart === props.ast.span()[0])
const whitespace = computed(() =>
  isOnStart.value ? '' : ' '.repeat(props.ast.inner.whitespaceLengthInCodeParsed),
)

const singularToken = computed(() =>
  whitespace.value.length === 0 && children.value.length === 1 && children.value[0]!.isToken()
    ? children.value[0]
    : null,
)

function updateRect() {
  let domNode = rootNode.value
  if (domNode == null) return
  const pos = new Vec2(domNode.offsetLeft, domNode.offsetTop)
  const size = nodeSize.value
  const rect = new Rect(pos, size)
  if (exprRect.value != null && rect.equals(exprRect.value)) return
  exprRect.value = rect
}

watch(nodeSize, updateRect)
onUpdated(updateRect)
watch(exprRect, (rect) => rect && emit('updateExprRect', props.ast.astId, rect))

// Return whether this node should interact with the mouse, e.g. when seeking an edge target.
function isHoverable(): boolean | 'tokensOnly' {
  switch (props.ast.treeTypeName()) {
    case 'Invalid':
    case 'BodyBlock':
    case 'Ident':
    case 'Number':
    case 'Wildcard':
    case 'TextLiteral':
      return true
    case 'DefaultApp':
      return 'tokensOnly'
    // Application should not be hoverable; typically their child nodes will be.
    case 'ArgumentBlockApplication':
    case 'OperatorBlockApplication':
    case 'OprApp':
    case 'UnaryOprApp':
    case 'MultiSegmentApp':
    case 'App':
    case 'NamedApp':
      return false
    // Other composite expressions.
    case 'Group':
    case 'TypeAnnotated':
    case 'CaseOf':
    case 'Lambda':
    case 'Array':
    case 'Tuple':
    case 'Documented':
    case 'OprSectionBoundary':
    case 'TemplateFunction':
      return false
    // Declarations; we won't generally display these within a node anyway.
    case 'Private':
    case 'TypeDef':
    case 'Assignment':
    case 'Function':
    case 'ForeignFunction':
    case 'Import':
    case 'Export':
    case 'TypeSignature':
    case 'Annotated':
    case 'AnnotatedBuiltin':
    case 'ConstructorDefinition':
      return false
    // Misc.
    case 'AutoScope':
      return false
  }
  console.log('Unexpected tree type', props.ast.treeTypeName())
  return true
}

function hover(part: 'tree' | 'token', isHovered: boolean) {
  const hoverable = isHoverable()
  if (hoverable == true || (hoverable == 'tokensOnly' && part == 'token'))
    emit('updateHoveredExpr', isHovered ? props.ast.astId : undefined)
}
</script>

<template>
  <NodeToken
    v-if="singularToken != null"
    :ast="singularToken"
    :nodeSpanStart="props.nodeSpanStart"
    @updateExprRect="(id, rect) => emit('updateExprRect', id, rect)"
    @pointerenter="hover('token', true)"
    @pointerleave="hover('token', false)"
  />
  <span
    v-else
    ref="rootNode"
    :class="['Tree', spanClass]"
    :data-span-start="props.ast.span()[0] - nodeSpanStart - whitespace.length"
    >{{ whitespace
    }}<template v-for="child in children" :key="child.astId">
      <NodeTree
        v-if="child.isTree()"
        :ast="child"
        :nodeSpanStart="props.nodeSpanStart"
        @updateExprRect="(id, rect) => emit('updateExprRect', id, rect)"
        @updateHoveredExpr="emit('updateHoveredExpr', $event)"
        @pointerenter="hover('tree', true)"
        @pointerleave="hover('tree', false)"
      />
      <NodeToken
        v-else-if="child.isToken()"
        :ast="child"
        :nodeSpanStart="props.nodeSpanStart"
        @updateExprRect="(id, rect) => emit('updateExprRect', id, rect)"
        @updateHoveredExpr="emit('updateHoveredExpr', $event)"
        @pointerenter="hover('token', true)"
        @pointerleave="hover('token', false)"
      />
    </template>
  </span>
</template>

<style scoped>
.Tree {
  color: white;
  white-space: pre;
  align-items: center;
  transition: background 0.2s ease;

  &.Root {
    color: rgb(255 255 255 / 0.33);
  }

  &.Ident,
  &.Literal {
    color: white;
  }

  &.Literal {
    font-weight: bold;
  }

  &.port {
    background-color: var(--node-color-port);
    border-radius: var(--node-border-radius);
    margin: -2px -4px;
    padding: 2px 4px;
  }
}
</style>

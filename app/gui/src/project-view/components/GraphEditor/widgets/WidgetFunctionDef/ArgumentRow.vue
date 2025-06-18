<script setup lang="ts">
import { injectCurrentProject } from '$/components/WithCurrentProject.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { PortId, syntheticPortId } from '@/providers/portInfo'
import {
  rewritePortValueUpdate,
  UpdateHandler,
  WidgetInput,
  WidgetUpdate,
} from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { Ast } from '@/util/ast'
import { unwrapGroups } from '@/util/ast/abstract'
import { endOnClick, targetIsOutside } from '@/util/autoBlur'
import { mapOrUndefined, Opt } from '@/util/data/opt'
import { Err, Ok } from '@/util/data/result'
import { computed, proxyRefs, useTemplateRef } from 'vue'
import { ComponentProps } from 'vue-component-type-helpers'
import { ArgumentDefinition, ConcreteRefs } from 'ydoc-shared/ast'
import { EnsoExpression } from '../WidgetEnsoExpression.vue'
import SelectionSubmenu from '../WidgetSelection/SelectionSubmenu.vue'
import { EnsoTypeExpression } from '../WidgetTypeExpression.vue'
import {
  ArgumentDefaultKind,
  createDefaultExpressionOfKind,
  getArgumentDefaultKind,
} from './argumentAst'

const { definition, onUpdate, portIdBase } = defineProps<{
  root: Opt<HTMLElement>
  definition: ArgumentDefinition<ConcreteRefs>
  onUpdate: UpdateHandler
  portIdBase: PortId
}>()
const emit = defineEmits<{
  rename: [value: Ast.Owned<Ast.MutableExpression>]
  updateType: [value: Ast.Owned<Ast.MutableExpression>]
  updateDefault: [value: Ast.Owned<Ast.MutableExpression> | undefined]
}>()
type WidgetProps = ComponentProps<typeof NodeWidget>
const openedProject = injectCurrentProject().ref

function defaultWidget(ast: Ast.Token | Ast.Ast): WidgetProps {
  return { input: WidgetInput.FromAst(ast) }
}

function patternWidget(pattern: Ast.Expression): WidgetProps {
  return {
    input: {
      portId: pattern.id,
      value: pattern,
      [EnsoExpression]: {},
    },
    onUpdate(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, onUpdate, pattern.id, (value) => {
        if (value instanceof Ast.Ast && value instanceof Ast.Ident) {
          emit('rename', value)
          return Ok()
        } else {
          return Err('Argument name must be a valid identifier.')
        }
      })
    },
  }
}

function mkWidget<T extends Ast.Ast | Ast.Token>(
  child: () => Ast.NodeChild<T> | undefined,
  toProps: (ast: T) => WidgetProps = defaultWidget,
) {
  return computed(() => mapOrUndefined(child()?.node, toProps))
}

const nodeSuspension = mkWidget(() => definition.suspension)
const nodePattern = mkWidget(() => definition.pattern, patternWidget)
const nodeType = computed((): WidgetProps => {
  const ty = definition.type?.type?.node
  const syntheticId = syntheticPortId(portIdBase, 'type')
  return {
    input: {
      ...WidgetInput.FromAstOrPlaceholder(ty, () => syntheticId),
      [EnsoTypeExpression]: {},
    },
    onUpdate(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, onUpdate, syntheticId, (rawValue) => {
        const value = typeof rawValue === 'string' ? Ast.parseExpression(rawValue) : rawValue
        if (value instanceof Ast.Ast && value.isExpression()) {
          emit('updateType', value)
          return Ok()
        } else {
          return Err('Argument type must be a valid expression.')
        }
      })
    },
  }
})

function resolveType(typeExpr: Ast.Ast) {
  const tyCode = typeExpr.code()
  // Hack: We have to resolve the fully qualified type name ourselves based on present imports.
  // To avoid implementing that for now, we only look up types selectable from dropdown.
  const matchingTypeEntry = openedProject.value?.suggestionDb.entries.selectableTypes.value.find(
    (ty) => ty.name === tyCode,
  )
  return matchingTypeEntry ?
      openedProject.value?.names.printProjectPath(matchingTypeEntry.definitionPath)
    : undefined
}

const nodeDefaultPortId = computed(() => syntheticPortId(portIdBase, 'defaultExpr'))
const nodeDefault = computed((): WidgetProps => {
  let expr = unwrapGroups(definition.defaultValue?.expression?.node)
  if (expr instanceof Ast.Group || expr instanceof Ast.Invalid) expr = undefined
  const syntheticId = nodeDefaultPortId.value
  const expectedType = mapOrUndefined(definition.type?.type?.node, resolveType)
  return {
    input: {
      ...WidgetInput.FromAstOrPlaceholder(expr, () => syntheticId),
      expectedType,
      editHandler: defaultValueDropdownInteraction.value,
      [EnsoExpression]: {
        weakMatch: true,
      },
    },
    onUpdate(update: WidgetUpdate) {
      return rewritePortValueUpdate(update, onUpdate, syntheticId, (rawValue) => {
        const value = typeof rawValue === 'string' ? Ast.parseExpression(rawValue) : rawValue
        if (value instanceof Ast.Ast && value.isExpression()) {
          emit('updateDefault', value)
          return Ok()
        } else {
          return Err('Argument default value must be a valid expression.')
        }
      })
    },
  }
})

const submenuRef = useTemplateRef('submenuRef')
const defaultValueRoot = useTemplateRef<HTMLElement>('defaultValueRoot')
function isOutsideDropdown(event: Event) {
  return submenuRef.value?.isTargetOutside(event) ?? false
}

function isOutsideWidget(event: Event) {
  return targetIsOutside(event, defaultValueRoot.value)
}

// Close the dropdown when clicking outside of it, but also end parent interaction when clicking outside of both.
const defaultValueDropdownInteraction = WidgetEditHandler.NewNested(
  nodeDefaultPortId,
  () => undefined,
  endOnClick((event) => isOutsideDropdown(event) && !isOutsideWidget(event), {
    end() {},
    cancel() {},
  }),
)

const defaultKind = computed(() => getArgumentDefaultKind(definition))

function defaultOnClick(entry: (typeof defaultEntries)[number]) {
  if (entry.value !== defaultKind.value) {
    emit('updateDefault', createDefaultExpressionOfKind(entry.key, definition.pattern.node.code()))
  }
  defaultValueDropdownInteraction.value.end()
}

function mkDefaultEntry(key: ArgumentDefaultKind, value: string) {
  return proxyRefs({
    value,
    key,
    selected: computed(() => defaultKind.value === key),
  })
}

const defaultEntries = [
  mkDefaultEntry('optional', 'optional argument'),
  mkDefaultEntry('required', 'required argument'),
  mkDefaultEntry('explicit', 'default value'),
] as const satisfies DropdownEntry[]
</script>

<template>
  <div class="ArgumentRow pad-right">
    <NodeWidget v-if="nodeSuspension" v-bind="nodeSuspension" />
    <NodeWidget v-if="nodePattern" v-bind="nodePattern" />
    <span class="tokenText">&nbsp;:&nbsp;</span>
    <NodeWidget v-bind="nodeType" />
    <span class="tokenText">&nbsp;=&nbsp;</span>
    <div
      ref="defaultValueRoot"
      class="defaultValueRoot"
      @click.stop="defaultValueDropdownInteraction.start()"
    >
      <SvgIcon
        name="arrow_right_head_only"
        class="dropdownArrow widgetOutOfLayout"
        :class="{ hovered: false }"
      />
      <SelectionSubmenu
        ref="submenuRef"
        :rootElement="root"
        :floatReference="defaultValueRoot"
        :show="defaultValueDropdownInteraction.isActive()"
        :entries="defaultEntries"
        :topLevel="true"
        :extendUpwards="false"
        @clickedEntry="defaultOnClick"
      />
      <template v-if="defaultKind == 'optional'">
        <span class="tokenText">optional</span>
      </template>
      <template v-else-if="defaultKind == 'required'">
        <span class="tokenText">required</span>
      </template>
      <template v-else>
        <span class="tokenText pad-right">default</span>
        <NodeWidget v-bind="nodeDefault" />
      </template>
    </div>
  </div>
</template>

<style scoped>
.ArgumentRow,
.defaultValueRoot {
  display: flex;
  flex-direction: row;
  place-items: center;
  overflow-x: clip;
}

.pad-right {
  margin-right: 4px;
}

.defaultValueRoot {
  position: relative;
}

svg.dropdownArrow {
  position: absolute;
  bottom: -8px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  transform-origin: center;
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
  &.hovered {
    opacity: 0.9;
  }
}

.tokenText {
  opacity: 0.33;
}
</style>

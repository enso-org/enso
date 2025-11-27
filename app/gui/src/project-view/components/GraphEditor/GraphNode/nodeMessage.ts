import { type NodeId } from '$/providers/openedProjects/graph'
import { type GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import { type ProjectStore } from '$/providers/openedProjects/project'
import type GraphNodeMessage from '@/components/GraphEditor/GraphNodeMessage.vue'
import {
  colorForMessageType,
  iconForMessageType,
  type MessageType,
} from '@/components/GraphEditor/GraphNodeMessage.vue'
import type SvgIcon from '@/components/SvgIcon.vue'
import { type Opt } from '@/util/data/opt'
import { type ToValue } from '@/util/reactivity'
import { computed, toValue } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'
import type { ExternalId } from 'ydoc-shared/yjsModel'

interface NodeMessageOptions {
  projectStore: ProjectStore
  graphDb: GraphDb
  expand: ToValue<boolean>
  nodeId: ToValue<NodeId>
}

/** Composable managing messages (warnings, errors, etc.) associated with a node. */
export function useNodeMessage({ projectStore, graphDb, expand, nodeId }: NodeMessageOptions) {
  const inputExternalIds = computed(() => {
    const externalIds = new Array<ExternalId>()
    for (const inputId of graphDb.nodeDependents.reverseLookup(toValue(nodeId))) {
      if (inputId) {
        externalIds.push(inputId)
      }
    }
    return externalIds
  })

  function getPanic(id: ExternalId) {
    const info = projectStore.computedValueRegistry.db.get(id)
    return info?.payload.type === 'Panic' ? info.payload.message : undefined
  }

  function getDataflowError(id: ExternalId) {
    return projectStore.dataflowErrors.lookup(id)?.value?.message
  }

  interface Message {
    type: MessageType
    text: string
    alwaysShow: boolean
  }
  const availableMessage = computed<Message | undefined>(() => {
    const externalId = toValue(nodeId)
    if (!externalId) return undefined
    const info = projectStore.computedValueRegistry.db.get(externalId)
    switch (info?.payload.type) {
      case 'Panic': {
        const text = info.payload.message
        const alwaysShow = !inputExternalIds.value.some((id) => getPanic(id) === text)
        return { type: 'panic', text, alwaysShow } satisfies Message
      }
      case 'DataflowError': {
        const rawText = getDataflowError(externalId)
        const text = rawText?.split(' (at')[0]
        if (!text) return undefined
        const alwaysShow = !inputExternalIds.value.some((id) => getDataflowError(id) === rawText)
        const type = rawText.includes('Missing_Argument') ? 'missing' : 'error'
        return { type, text, alwaysShow } satisfies Message
      }
      case 'Value': {
        const warning = info.payload.warnings?.value
        if (!warning) return undefined
        return {
          type: 'warning',
          text: 'Warning: ' + warning,
          alwaysShow: false,
        } satisfies Message
      }
      default:
        return undefined
    }
  })

  const visibleMessage = computed((): Opt<ComponentProps<typeof GraphNodeMessage>> => {
    if (!availableMessage.value || !(availableMessage.value?.alwaysShow || toValue(expand)))
      return null
    return {
      type: availableMessage.value.type,
      message: availableMessage.value.text,
    }
  })

  const hiddenMessage = computed((): Opt<ComponentProps<typeof SvgIcon>> => {
    if (visibleMessage.value || !availableMessage.value) return null
    return {
      name: iconForMessageType[availableMessage.value.type],
      style: { color: colorForMessageType[availableMessage.value.type] },
    }
  })

  return { visibleMessage, hiddenMessage }
}

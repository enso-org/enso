import { identity } from '@vueuse/core'
import { createContextStore } from '.'

interface PortInfo {
  portId: string
  connected: boolean
}

export { injectFn as injectPortInfo, provideFn as providePortInfo }
const { provideFn, injectFn } = createContextStore('Port info', identity<PortInfo>)

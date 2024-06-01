import { computed, type Ref } from 'vue'
import { createContextStore } from '.'

export type EventLogger = ReturnType<typeof injectFn>
export { injectFn as injectEventLogger, provideFn as provideEventLogger }
const { provideFn, injectFn } = createContextStore('event logger', eventLogger)

function eventLogger(logEvent: Ref<LogEvent>, projectId: Ref<string>) {
  const logProjectId = computed(() => {
    const id = projectId.value
    if (!id) return undefined
    const prefix = 'project-'
    const projectUuid = id.startsWith(prefix) ? id.substring(prefix.length) : id
    return `${prefix}${projectUuid.replace(/-/g, '')}`
  })

  return {
    async send(message: string) {
      logEvent.value(message, logProjectId.value)
    },
  }
}

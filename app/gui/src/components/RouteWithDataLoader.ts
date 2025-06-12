import { ComponentOptions, effectScope, EffectScope, watchEffect } from 'vue'

export function withReactiveDataFetcher<Data>(
  composable: () => Promise<Data>,
  fieldName: string = 'data',
): ComponentOptions {
  let scope: EffectScope | undefined
  return {
    async beforeRouteEnter(to, from, next) {
      scope = effectScope()
      const computedData = await scope.run(composable)
      return next((component) =>
        scope!.run(() => {
          watchEffect(() => {
            component[fieldName] = computedData
          })
        }),
      )
    },
    beforeRouteLeave() {
      scope?.stop()
    },
  }
}

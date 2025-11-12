/* eslint-disable vue/one-component-per-file */
import { proxyRefs } from '@/util/reactivity'
import { Ok } from 'enso-common/src/utilities/data/result'
import { describe, expect, test } from 'vitest'
import { createApp, defineComponent, nextTick, onScopeDispose, ref, watch } from 'vue'
import { createMemoryHistory, createRouter } from 'vue-router'
import { withDataLoader, type DataLoader } from '../dataLoader'

function fixture(dataLoader: DataLoader<{ data: number }>) {
  let dataWasNull = false
  let propValue: number | undefined

  const App = defineComponent({
    template: '<div><RouterView /></div>',
  })

  const A = defineComponent({
    props: { data: { type: Number, required: true } },
    watch: {
      data: {
        handler(val) {
          propValue = val
        },
        immediate: true,
      },
    },
    beforeMount() {
      if (this.$props.data == null) dataWasNull = true
    },
    template: '<div><RouterView /></div>',
  })

  const Leaf = defineComponent({
    template: '<div />',
  })

  const router = createRouter({
    routes: [
      {
        path: '/',
        component: withDataLoader(() => Promise.resolve({ dataLoader, default: A })),
        children: [
          {
            path: 'b',
            component: Leaf,
          },
          {
            path: 'c',
            component: Leaf,
          },
        ],
      },
      {
        path: '/d',
        component: Leaf,
      },
    ],
    history: createMemoryHistory(),
  })
  const app = createApp(App).use(router)
  app.mount(document.createElement('div'))

  function checkIfDataWasNull() {
    expect(dataWasNull).toBeFalsy()
  }

  function expectPropValue() {
    return expect(propValue)
  }

  return { router, checkIfDataWasNull, expectPropValue }
}

describe('Data Loader', () => {
  test('Load data before mounting', async () => {
    const { router, checkIfDataWasNull } = fixture({
      beforeRouteEnter: () => {
        // nextTick is not enough - we have to use 0 timeout.
        return new Promise((resolve) => setTimeout(() => resolve(Ok({ data: 13 })), 0))
      },
    })
    expect(await router.push('b')).toBeUndefined()
    checkIfDataWasNull()
  })

  test('Update data between routes', async () => {
    const { router, expectPropValue } = fixture({
      beforeRouteEnter: async () => {
        return Ok({ data: 13 })
      },
      beforeRouteUpdate: async (_to, _from, data) => {
        data.data = 14
      },
    })
    expect(await router.push('b')).toBeUndefined()
    expectPropValue().toBe(13)
    expect(await router.push('c')).toBeUndefined()
    expectPropValue().toBe(14)
  })

  test('effect scope is working', async () => {
    const watched = ref(14)
    let scopeDisposed = false
    const { router, expectPropValue } = fixture({
      beforeRouteEnter: async () => {
        const data = ref(14)
        watch(watched, (val) => (data.value = val))
        onScopeDispose(() => (scopeDisposed = true))
        return Ok(proxyRefs({ data }))
      },
    })
    expect(await router.push('b')).toBeUndefined()
    expectPropValue().toBe(14)
    watched.value = 15
    await nextTick()
    expectPropValue().toBe(15)
    expect(scopeDisposed).toBeFalsy()
    expect(await router.push('d')).toBeUndefined()
    expect(scopeDisposed).toBeTruthy()
  })
})

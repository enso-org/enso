<script setup lang="ts">
import { reactive, ref } from 'vue'

import GraphEditor from '@/components/GraphEditor.vue'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { ToastContainer } from 'react-toastify'
import 'react-toastify/dist/ReactToastify.css'
import { createReactWrapper } from 'vue-react-wrapper'
import MockProjectStoreWrapper from './MockProjectStoreWrapper.vue'
import HstCode from './histoire/HstCode.vue'

const text = ref(`\
from Standard.Base import all
from Standard.Base.Runtime.Ref import Ref

from Standard.Test import Bench

options = Bench.options . set_warmup (Bench.phase_conf 1 2) . set_measure (Bench.phase_conf 3 2)

collect_benches = Bench.build builder->
    range_size = 100000000
    data = 0.up_to range_size

    builder.group "Range" options group_builder->
        group_builder.specify "iterate" <|
            cell = Ref.new 0
            data . each _->
                x = cell.get
                cell.put x+1

            cell.get . should_equal range_size

main =
    benches = collect_benches
    result = run_main benches
    third_node = 2 + 2
`)

/**
 * Note: These props should be synced with the props in
 * `app/ide-desktop/lib/dashboard/src/authentication/src/components/app.tsx`.
 * We need this here, as the react component is not part of the dashboard and not usually available in the demo scenes.
 * Using this wrapper enables us to see toasts in the absence of the dashboard/React.
 */
const toastProps = reactive({
  position: 'top-center',
  theme: 'light',
  closeOnClick: false,
  draggable: false,
  toastClassName: 'text-sm leading-170 bg-frame-selected rounded-2xl backdrop-blur-3xl',
  limit: 3,
})
const WrappedToastContainer = createReactWrapper(ToastContainer, toastProps)
</script>

<template>
  <Story
    title="Editor"
    group="graph"
    :layout="{ type: 'single', iframe: false }"
    :setupApp="
      () => {
        useSuggestionDbStore()
      }
    "
    responsiveDisabled
    autoPropsDisabled
  >
    <WrappedToastContainer />t
    <MockProjectStoreWrapper v-model="text">
      <Suspense><GraphEditor /></Suspense>
    </MockProjectStoreWrapper>

    <template #controls><HstCode v-model="text" title="code" /></template>
  </Story>
</template>

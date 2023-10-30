<script setup lang="ts">
import { ref } from 'vue'

import CodeEditor from '@/components/CodeEditor.vue'
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
`)
</script>

<template>
  <Story
    title="Code Editor"
    group="graph"
    :layout="{ type: 'single', iframe: false }"
    responsiveDisabled
    autoPropsDisabled
  >
    <MockProjectStoreWrapper v-model="text">
      <Suspense><CodeEditor class="standalone" /></Suspense>
    </MockProjectStoreWrapper>

    <template #controls><HstCode v-model="text" title="code" /></template>
  </Story>
</template>

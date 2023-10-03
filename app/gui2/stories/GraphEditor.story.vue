<script setup lang="ts">
/// <reference types="@histoire/plugin-vue/components" />
import { computed } from 'vue'

import GraphEditor from '@/components/GraphEditor.vue'

import { useProjectStore } from '@/stores/project'
import { onMounted } from 'vue'

const projectStore = useProjectStore()

const text = computed({
  get() {
    // const ret = projectStore.module?.doc.contents.toString() ?? ''
    // console.log(ret, projectStore.module?.doc.contents)
    // console.trace()
    // return ret
    return ''
  },
  set(value) {
    // if (projectStore.module) {
    //   projectStore.module.transact(() => {
    //     const text = projectStore.module?.doc.contents
    //     text?.delete(0, text.length - 1)
    //     text?.insert(0, value)
    //   })
    // }
  },
})

onMounted(() => {
  text.value = `\
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

main = collect_benches . run_main`
})
</script>

<template>
  <Story title="Editor" group="graph" responsive-disabled>
    <GraphEditor />

    <template #controls><HstJson v-model="text" title="code" /></template>
  </Story>
</template>

<style>
:root {
  --color-text: var(--histoire-contrast-color);
}

.viewport {
  height: 100vh;
  width: 100vw;
}
</style>

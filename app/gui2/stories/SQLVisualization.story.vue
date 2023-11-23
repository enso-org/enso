<script setup lang="ts">
import { ref } from 'vue'

import SQLVisualization from '@/components/visualizations/SQLVisualization.vue'

const data = ref<any>({
  dialect: 'sql',
  code: `SELECT * FROM \`foo\` WHERE \`a\` = ? AND b LIKE ?;`,
  interpolations: [
    // eslint-disable-next-line camelcase
    { enso_type: 'Data.Numbers.Number', value: '123' },
    // eslint-disable-next-line camelcase
    { enso_type: 'Builtins.Main.Text', value: "a'bcd" },
  ],
})
</script>

<template>
  <Story
    title="SQL Query"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    autoPropsDisabled
  >
    <div style="height: 322px">
      <SQLVisualization :data="data" />
    </div>

    <template #controls>
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>

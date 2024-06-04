<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgIcon from '@/components/SvgIcon.vue'

import { type UseFormatting } from '@/components/lexical/formatting'

const _props = defineProps<{ formatting: UseFormatting }>()

// TODO(#10059): "Image"
// TODO(#9970): "Table"
</script>

<template>
  <DropdownMenu>
    <template #button>
      <SvgIcon name="add" />
      <div class="iconLabel">Insert</div>
    </template>
    <template #entries> </template>
  </DropdownMenu>
</template>

<script lang="ts">
export default {
  name: 'HstWrapper',
}

/*
 * MIT License
 *
 * Copyright (c) 2022 Guillaume Chau
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
</script>

<script lang="ts" setup>
import { VTooltip as vTooltip } from 'floating-vue'
import { withDefaults } from 'vue'

withDefaults(
  defineProps<{
    title?: string
    tag?: string
  }>(),
  {
    title: undefined!,
    tag: 'label',
  },
)
</script>

<template>
  <component
    :is="tag"
    class="histoire-wrapper htw-p-2 hover:htw-bg-primary-100 dark:hover:htw-bg-primary-800 htw-flex htw-gap-2 htw-flex-wrap"
  >
    <span
      v-tooltip="{
        content: title,
        placement: 'left',
        distance: 12,
      }"
      class="htw-w-28 htw-whitespace-nowrap htw-text-ellipsis htw-overflow-hidden htw-shrink-0"
    >
      {{ title }}
    </span>
    <span class="htw-grow htw-max-w-full htw-flex htw-items-center htw-gap-1">
      <span class="htw-block htw-grow htw-max-w-full">
        <slot />
      </span>
      <slot name="actions" />
    </span>
  </component>
</template>

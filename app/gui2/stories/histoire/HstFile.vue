<script setup lang="ts">
import { ref } from 'vue'
import HstWrapper from './HstWrapper.vue'

const props = defineProps<{ title?: string }>()
const emit = defineEmits<{ 'update:file': [file: File] }>()

const fileNode = ref<HTMLInputElement>()
const file = ref<File>()

function onFileChange(newFile: File | undefined) {
  if (!newFile) return
  emit('update:file', newFile)
  file.value = newFile
}

function asAny(x: unknown) {
  return x as any
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

<template>
  <HstWrapper
    :title="props.title!"
    class="histoire-file htw-cursor-pointer htw-items-center"
    :class="$attrs.class"
    :style="$attrs.style"
  >
    {{ file?.name ?? 'No file chosen' }}
    <input
      ref="fileNode"
      type="file"
      class="htw-hidden"
      v-bind="asAny($props)"
      @input="onFileChange(fileNode?.files?.[0])"
    />
  </HstWrapper>
</template>

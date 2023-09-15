<script setup lang="ts">
import { ref, watchEffect } from 'vue'
import {useWindowEvent} from "@/util/events";
import {useProjectStore} from "@/stores/project";

const proj = useProjectStore()

const emit = defineEmits<{
  codeUpdate: [code: string]
}>()

const content = ref('main = \n    2 + 2')
const shown = ref(false)

watchEffect(() => {
  proj.module?.updateCode(content.value)
  emit('codeUpdate', content.value)
})

useWindowEvent('keydown', e => { if (e.key == `\``) shown.value = !shown.value})

</script>

<template>
  <div v-if="shown" class="CodeEditor" @keydown.enter.stop>
    <textarea v-model="content"></textarea>
  </div>
</template>

<style scoped>
.CodeEditor {
  position: absolute;
  bottom: 0;
  left: 0;
}

.CodeEditor > textarea {
  background-color: rgba(1.0,1.0,1.0,0.1);
  border-color: transparent;
  resize: none;
  width: 500px;
  height: 500px;
}

.CodeEditor > textarea:focus {
  outline: none !important;
}
</style>
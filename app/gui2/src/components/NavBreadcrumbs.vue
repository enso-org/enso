<script setup lang="ts">
import NavBreadcrumb from '@/components/NavBreadcrumb.vue'
import SvgButton from '@/components/SvgButton.vue'
import { injectStackNavigator } from '@/providers/graphStackNavigator'
import { useProjectStore } from '@/stores/project'
import { useToast } from '@/util/toast'
import { ref } from 'vue'

export interface BreadcrumbItem {
  label: string
  active: boolean
}
const renameError = useToast.error()
const projectNameEdited = ref(false)

const stackNavigator = injectStackNavigator()
const project = useProjectStore()

async function renameBreadcrumb(index: number, newName: string) {
  if (index === 0) {
    const result = await project.renameProject(newName)
    if (!result.ok) {
      renameError.reportError(result.error)
    }
    projectNameEdited.value = false
  }
}
</script>

<template>
  <div class="NavBreadcrumbs">
    <SvgButton name="edit" title="Edit Project Name" @click.stop="projectNameEdited = true" />
    <template v-for="(breadcrumb, index) in stackNavigator.breadcrumbLabels.value" :key="index">
      <SvgButton
        v-if="index > 0"
        name="arrow_right_head_only"
        :disabled="!breadcrumb.active"
        class="arrow"
      />
      <NavBreadcrumb
        :modelValue="breadcrumb.label"
        :active="breadcrumb.active"
        :editing="index === 0 && projectNameEdited"
        :title="index === 0 ? 'Project Name' : ''"
        class="clickable"
        @click.stop="stackNavigator.handleBreadcrumbClick(index)"
        @renamed="renameBreadcrumb(index, $event)"
      />
    </template>
  </div>
</template>

<style scoped>
.NavBreadcrumbs {
  display: flex;
  align-items: center;
  gap: 2px;
}

.arrow {
  color: #666666;
}

.inactive {
  opacity: 0.4;
}
</style>

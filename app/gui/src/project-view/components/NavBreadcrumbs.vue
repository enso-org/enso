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
  isCurrentTop: boolean
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
  <div class="NavBar">
    <div class="NavBreadcrumbs">
      <SvgButton name="edit" title="Edit Project Name" @click.stop="projectNameEdited = true" />
      <template v-for="(breadcrumb, index) in stackNavigator.breadcrumbLabels.value" :key="index">
        <SvgButton
          v-if="index > 0"
          name="arrow_right_head_only"
          :disabled="!breadcrumb.active"
          :class="{ nonInteractive: breadcrumb.isCurrentTop }"
          class="arrow"
        />
        <NavBreadcrumb
          :modelValue="breadcrumb.label"
          :active="breadcrumb.active"
          :editing="index === 0 && projectNameEdited"
          :title="index === 0 ? 'Project Name' : ''"
          :class="{ nonInteractive: breadcrumb.isCurrentTop }"
          class="clickable"
          @click.stop="stackNavigator.handleBreadcrumbClick(index)"
          @update:modelValue="renameBreadcrumb(index, $event)"
          @focusout="projectNameEdited = false"
        />
      </template>
    </div>
  </div>
</template>

<style scoped>
.NavBar {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding-left: 8px;
  padding-right: 10px;
}

.NavBreadcrumbs {
  display: flex;
  align-items: center;
  gap: 2px;
}

.arrow {
  color: #666666;
}

.nonInteractive {
  pointer-events: none;
}
</style>

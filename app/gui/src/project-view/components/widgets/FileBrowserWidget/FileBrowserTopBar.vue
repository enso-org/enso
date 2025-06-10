<template>
  <div class="FileBrowserTopBar">
    <slot />
  </div>
</template>

<style scoped>
.FileBrowserTopBar {
  color: white;
  background-color: var(--background-color);
  display: flex;
  flex-direction: row;
  padding: 2px 8px;
  gap: 4px;
}
</style>

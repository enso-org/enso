<template>
  <div class="FileBrowserTopBar">
    <slot />
  </div>
</template>

<style scoped>
.FileBrowserTopBar {
  color: var(--file-browser-top-bar-color, white);
  display: flex;
  flex-direction: row;
  padding: 2px 8px;
  gap: 4px;
}
</style>

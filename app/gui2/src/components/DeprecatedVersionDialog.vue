<script setup lang="ts"></script>

<template>
  <div class="DeprecatedVersionDialog">
    This version is no longer supported.<wbr /> Please download a new one.
  </div>
</template>

<style scoped>
.DeprecatedVersionDialog {
  height: 100%;
  display: grid;
  place-items: center;
  text-align: center;
  font-size: 24px;
  font-family: var(--font-sans);
  color: #454545;
  margin: 24px auto;
  white-space: nowrap;
  line-height: 130%;
}
</style>

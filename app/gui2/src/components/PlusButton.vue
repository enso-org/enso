<template>
  <div class="circle-plus">
    <div class="vertical"></div>
    <div class="horizontal"></div>
  </div>
</template>

<script setup lang="ts"></script>

<style scoped>
.circle-plus {
  position: absolute;
  bottom: 10px;
  left: 10px;
  width: 60px;
  height: 60px;
  border-radius: 50%;
  background-color: white;
}

.circle-plus:hover {
  background: rgb(230, 230, 255);
}

.circle-plus:active {
  background: rgb(158, 158, 255);
}

.vertical,
.horizontal {
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  background: rgb(0, 115, 219);
  margin: auto;
}

.vertical {
  width: 4px;
  height: 70%;
}

.horizontal {
  width: 70%;
  height: 4px;
}
</style>

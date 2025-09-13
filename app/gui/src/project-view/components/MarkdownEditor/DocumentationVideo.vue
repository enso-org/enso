<script setup lang="ts">
const { src, title } = defineProps<{ src: string; title: string }>()
</script>

<template>
  <div class="DocumentationVideo">
    <div class="youtube-video-wrapper">
      <iframe
        class="youtube-video"
        :src="src"
        :title="title"
        allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
        referrerpolicy="strict-origin-when-cross-origin"
        allowfullscreen
      >
      </iframe>
    </div>
  </div>
</template>

<style scoped>
.DocumentationVideo {
  width: 100%;
  min-width: 400px;
  max-width: 800px;
}

.youtube-video-wrapper {
  position: relative;
  width: 100%;
  overflow: hidden;
  padding-top: 56.25%;
}

.youtube-video {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
}
</style>

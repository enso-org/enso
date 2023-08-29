<script setup lang="ts">
import { RouterLink, RouterView } from 'vue-router'
import HelloWorld from './components/HelloWorld.vue'
import { onMounted, onUnmounted, ref, watchEffect } from 'vue'
import { TextAreaBinding } from 'y-textarea'
import * as yjs from 'yjs'
import { Awareness } from 'y-protocols/awareness'

let doc = new yjs.Doc()
// const provider = new WebrtcProvider('enso-textarea-room', doc)

const awareness = new Awareness(doc)
let docText = doc.getText('textarea')

const textarea = ref<HTMLTextAreaElement | null>(null)

let textareaContent = ref('')

function docUpdateHandler() {
  textareaContent.value = docText.toString()
}

onMounted(() => {
  docText.observe(docUpdateHandler)
})

onUnmounted(() => {
  docText.unobserve(docUpdateHandler)
})

watchEffect((onCleanup) => {
  let textareaEl = textarea.value
  if (textareaEl == null) return
  let textBinding = new TextAreaBinding(docText, textareaEl, {
    awareness: awareness,
    clientName: `${awareness.clientID}`,
  })
  onCleanup(() => {
    textBinding.destroy()
  })
})
</script>

<template>
  <header>
    <div class="wrapper">
      <nav>
        <RouterLink to="/">Home</RouterLink>
        <RouterLink to="/about">About</RouterLink>
        <RouterLink to="/project/test">Project</RouterLink>
      </nav>
    </div>
  </header>

  <RouterView class="flex" />
</template>

<style scoped>
header {
  line-height: 1.5;
}

.logo {
  display: block;
  margin: 0 auto 2rem;
}

nav {
  font-size: 12px;
  text-align: center;
  margin-top: 2rem;
}

nav a.router-link-exact-active {
  color: var(--color-text);
}

nav a.router-link-exact-active:hover {
  background-color: transparent;
}

nav a {
  display: inline-block;
  padding: 0 1rem;
  border-left: 1px solid var(--color-border);
}

nav a:first-of-type {
  border: 0;
}

header {
  display: flex;
}

header .wrapper {
  display: flex;
  place-items: flex-start;
  flex-wrap: wrap;
}
.flex {
  flex: 1;
}
nav {
  text-align: left;
  font-size: 1rem;
  padding: 1rem 0;
  margin-top: 1rem;
}
</style>

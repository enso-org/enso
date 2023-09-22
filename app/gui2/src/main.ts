import './assets/main.css'

import { createApp } from 'vue'
import { createPinia } from 'pinia'
import { basicSetup } from 'codemirror'
import VueCodemirror from 'vue-codemirror'

import App from './App.vue'

const app = createApp(App)
app.use(createPinia())
app.use(VueCodemirror, { extensions: [basicSetup] })
app.mount('#app')

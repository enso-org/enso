import './assets/main.css'

import { createApp } from 'vue'
import { createPinia } from 'pinia'
import 'vue-virtual-scroller/dist/vue-virtual-scroller.css'

import App from './App.vue'

const app = createApp(App)
app.use(createPinia())

app.mount('#app')

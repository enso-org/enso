import '@/assets/base.css'

export async function AsyncApp() {
  return import('@/App.vue')
}

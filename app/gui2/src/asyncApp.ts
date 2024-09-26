import '@/assets/base.css'

export async function AsyncApp() {
  const app = await import('@/App.vue')
  return app
}

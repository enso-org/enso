import '@/assets/base.css'

/** TODO: Add docs */
export async function AsyncApp() {
  const app = await import('@/App.vue')
  return app
}

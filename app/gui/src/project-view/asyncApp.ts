import '@/assets/base.css'

/** Load App.vue asynchronously. */
export async function AsyncApp() {
  const app = await import('@/App.vue')
  return app
}

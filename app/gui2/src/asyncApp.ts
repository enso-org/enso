import '@/assets/base.css'

export async function AsyncApp() {
  console.log('Loading app...')
  const app = await import('@/App.vue')
  console.log('App loaded')
  return app
}

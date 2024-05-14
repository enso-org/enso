import '@/assets/main.css'

export async function AsyncApp() {
  const [_, app] = await Promise.all([
    import('shared/ast/ffi').then((mod) => mod.initializeFFI()),
    import('@/App.vue'),
  ])
  return app
}

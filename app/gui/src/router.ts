import LoginReact from '#/pages/authentication/Login'
import RegistrationReact from '#/pages/authentication/Registration'
import { GuestLayout as GuestLayoutReact } from '#/providers/AuthProvider'
import { applyPureReactInVue } from 'veaury'
import { createMemoryHistory, createRouter } from 'vue-router'

const Login = applyPureReactInVue(LoginReact)
const Registration = applyPureReactInVue(RegistrationReact)
const GuestLayout = applyPureReactInVue(GuestLayoutReact)

const routes = [
  {
    component: GuestLayout,
    children: [
      { path: '/login', component: Login },
      { path: '/registration', component: Registration },
    ],
  },
]

export const router = createRouter({
  history: createMemoryHistory(),
  routes,
})

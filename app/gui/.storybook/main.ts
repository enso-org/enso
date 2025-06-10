/**
 * @file
 *
 * Main file for Storybook configuration.
 */
import type { StorybookConfig as ReactStorybookConfig } from '@storybook/react-vite'
import type { StorybookConfig as VueStorybookConfig } from '@storybook/vue3-vite'
import z from 'zod'

const framework = z.enum(['vue', 'react']).parse(process.env.FRAMEWORK)

const sharedConfig: Partial<ReactStorybookConfig> = {
  addons: [
    '@storybook/addon-onboarding',
    '@storybook/addon-essentials',
    '@chromatic-com/storybook',
    '@storybook/addon-interactions',
  ],
  features: {},
  core: { disableTelemetry: true },
  env: { FRAMEWORK: framework },

  previewHead: (head) => {
    return /*html*/ `
    <link rel="preconnect" href="https://fonts.googleapis.com" crossorigin />
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin />
    <link
      href="https://fonts.googleapis.com/css2?family=M+PLUS+1:wght@300;400;500;600;700&display=swap"
      rel="preload"
      as="style"
      crossorigin
    />

    <script>
      window.global = window;

      // Pass environment variables to the storybook
      window.ENV = {
        // The framework used to render the story
        // Used by the preview to determine which framework to use
        FRAMEWORK: '${framework}',
      }

      // Allow React DevTools to work in Storybook
      if (window.parent !== window) {
        window.__REACT_DEVTOOLS_GLOBAL_HOOK__ = window.parent.__REACT_DEVTOOLS_GLOBAL_HOOK__
      }

      document.documentElement.classList.add('macos')
    </script>
    ${head}
    `
  },
}

const vueConfig: VueStorybookConfig = {
  ...sharedConfig,
  stories: [
    '../src/project-view/**/*.mdx',
    '../src/project-view/**/*.stories.@(js|jsx|mjs|ts|tsx)',
  ],
  framework: {
    name: '@storybook/vue3-vite',
    options: {},
  },
  refs: {
    Dashboard: {
      title: 'Dashboard',
      url: 'http://localhost:6007',
    },
  },
}

const reactConfig: ReactStorybookConfig = {
  ...sharedConfig,
  stories: ['../src/dashboard/**/*.mdx', '../src/dashboard/**/*.stories.tsx'],
  framework: {
    name: '@storybook/react-vite',
    options: { strictMode: true },
  },
}

export default framework === 'vue' ? vueConfig : reactConfig

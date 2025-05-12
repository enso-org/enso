/** @file Documentation display for an asset. */
import { Result } from '#/components/Result'
import { useLaunchedProjects } from '#/providers/ProjectsProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { vueComponent } from '#/utilities/vue'
// eslint-disable-next-line no-restricted-syntax
import ComponentDocumentationVue from '@/components/ComponentDocumentation.vue'

/** Props for a {@link ComponentHelp}. */
export interface ComponentHelpProps {
  readonly backend: Backend
}

// eslint-disable-next-line no-restricted-syntax
const ComponentDocumentation = vueComponent(ComponentDocumentationVue).default

/** Documentation display for an asset. */
export function ComponentHelp(props: ComponentHelpProps) {
  const { getText } = useText()

  const launchedProjects = useLaunchedProjects()

  if (launchedProjects.length === 0) {
    return <Result status="info" title={getText('componentHelp.noProjects')} centered />
  }

  return <ComponentHelpContent />
}

/** Props for an {@link ComponentHelpContent}. */
interface ComponentHelpContentProps {}

/** Documentation display for an asset. */
export function ComponentHelpContent(props: ComponentHelpContentProps) {
  return (
    <div className="flex h-full w-full flex-col">
      <ComponentDocumentation />
    </div>
  )
}

/** @file A modal containing project templates and news. */
import { Dialog } from '#/components/AriaComponents'
import { Samples } from '#/layouts/Samples'
import { WhatsNew } from '#/layouts/WhatsNew'
import { useText } from '#/providers/TextProvider'

/** Props for a {@link StartModal}. */
export interface StartModalProps {
  readonly createProject: (templateId?: string | null, templateName?: string | null) => void
}

/** A modal containing project templates and news. */
export default function StartModal(props: StartModalProps) {
  const { createProject } = props
  const { getText } = useText()

  return (
    <Dialog type="fullscreen" title={getText('selectTemplate')} testId="start-modal">
      {(opts) => (
        <div className="mb-4 flex flex-1 flex-col gap-home text-xs text-primary">
          <WhatsNew />

          <Samples
            groupName="Get Started"
            createProject={(templateId, templateName) => {
              createProject(templateId, templateName)
              opts.close()
            }}
          />

          <Samples
            groupName="Examples"
            createProject={(templateId, templateName) => {
              createProject(templateId, templateName)
              opts.close()
            }}
          />

          <Samples
            groupName="Advanced"
            createProject={(templateId, templateName) => {
              createProject(templateId, templateName)
              opts.close()
            }}
          />
        </div>
      )}
    </Dialog>
  )
}

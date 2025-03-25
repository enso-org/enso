/** @file A screen displaying an error. */
import { Text } from '#/components/aria'
import { useText } from '#/providers/TextProvider'
import { getMessageOrToString } from '#/utilities/error'

/** Props for an {@link ErrorScreen}. */
export interface ErrorScreenProps {
  readonly error: unknown
}

/** A screen displaying an error. */
export default function ErrorScreen(props: ErrorScreenProps) {
  const { error } = props
  const { getText } = useText()
  return (
    <div className="grid h-screen w-screen place-items-center text-primary">
      <div className="flex flex-col items-center gap-status-page text-center text-base">
        <Text>{getText('appErroredMessage')}</Text>
        <Text>{getText('appErroredPrompt')}</Text>
        <Text className="text-delete">{getMessageOrToString(error)}</Text>
      </div>
    </div>
  )
}

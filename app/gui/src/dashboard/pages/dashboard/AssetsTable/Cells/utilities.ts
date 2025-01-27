import { isNewTitleUnique2, type AnyAsset } from '#/services/Backend'
import type { GetText } from '#/providers/TextProvider'
import { z } from 'zod'

export interface NAME_SCHEMA_PROPS {
  itemId: AnyAsset['id']
  nodeMap: Set<AnyAsset>
  getText: GetText
}

export function getNameSchema(props: NAME_SCHEMA_PROPS) {
  return z.string().refine(
    (value) =>
      isNewTitleUnique2(
        props.itemId,
        value,
        props.nodeMap,
      ),
    { message: props.getText('nameShouldBeUnique') },
  )

import CONFIG from '@/config.json' assert { type: 'json' }
import { objectToGroup } from '@/util/config'
import * as version from '@/util/version'

export const options = objectToGroup(CONFIG, { Version: version })

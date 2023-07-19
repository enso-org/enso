/** @file Utilities for working with permissions. */
import * as backend from './backend'

/** Returns an array containing the owner permission if `owner` is not `null`;
 * else returns an empty array (`[]`). */
export function tryGetSingletonOwnerPermission(owner: backend.UserOrOrganization | null) {
    return owner != null
        ? [
              {
                  user: {
                      // The names are defined by the backend and cannot be changed.
                      /* eslint-disable @typescript-eslint/naming-convention */
                      pk: backend.Subject(''),
                      organization_id: owner.id,
                      user_email: owner.email,
                      user_name: owner.name,
                      /* eslint-enable @typescript-eslint/naming-convention */
                  },
                  permission: backend.PermissionAction.own,
              },
          ]
        : []
}

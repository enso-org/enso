// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck
import { dedent } from './dedent.js'

export async function setMessage({ header, body, prNumber, repo, github }) {
  const commentList = await github.paginate(
    'GET /repos/:owner/:repo/issues/:issue_number/comments',
    // eslint-disable-next-line camelcase
    { ...repo, issue_number: prNumber },
  )

  const commentBody = dedent`
    ${header}

    ${body}
  `

  const comment = commentList.find((comment) => comment.body.startsWith(header))

  if (!comment) {
    await github.rest.issues.createComment({
      ...repo,
      // eslint-disable-next-line camelcase
      issue_number: prNumber,
      body: commentBody,
    })
  } else {
    await github.rest.issues.updateComment({
      ...repo,
      // eslint-disable-next-line camelcase
      comment_id: comment.id,
      body: commentBody,
    })
  }
}

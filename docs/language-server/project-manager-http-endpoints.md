---
layout: developer-doc
title: Project Manager HTTP Endpoints
category: language-server
tags: [language-server, protocol, specification]
order: 7
---

# HTTP Endpoints

Project Manager exposes a number of HTTP endpoints on the same socket as the
JSONRPC protocol.

<!-- MarkdownTOC levels="2" autolink="true" indent="    " -->

- [`/projects/{project_id}/enso-project`](#projectsproject_idenso-project)

<!-- /MarkdownTOC -->

## `/projects/{project_id}/enso-project`

HTTP endpoint that returns the project structure in `.enso-project` format.

### `GET`

Returns:

- `200 OK` with the body containing the project structure as a `.tar.gz` archive
- `404 Not Found` when the project cannot be found by the provided id
- `500 Internal Server Error` in case of errors

#### Request

```text
> GET /projects/730a66ef-4222-46f8-8a03-d766946ab2bd/enso-project HTTP/1.1
> Host: localhost:30535
> User-Agent: curl/8.1.2
> Accept: */*
```

#### Response

```text
< HTTP/1.1 200 OK
< Server: akka-http/10.2.10
< Date: Thu, 13 Jul 2023 16:20:08 GMT
< Content-Type: application/gzip
< Content-Length: 2527
<
[2527 bytes data]
```

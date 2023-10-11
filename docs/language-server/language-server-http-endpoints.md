---
layout: developer-doc
title: Language Server HTTP Endpoints
category: language-server
tags: [language-server, protocol, specification]
order: 6
---

# HTTP Endpoints

Language server exposes a number of HTTP endpoints on the same socket as the
JSONRPC protocol.

<!-- MarkdownTOC levels="2" autolink="true" indent="    " -->

- [`/_health`](#_health)
- [`/_health/readiness`](#_healthreadiness)
- [`/_health/liveness`](#_healthliveness)
- [`/_idle`](#_idle)
- [`/_idle/reset`](#_idlereset)

<!-- /MarkdownTOC -->

## `/_health`

HTTP endpoint that provides basic health checking capabilities.

### `GET | HEAD`

Returns `200 OK` when the server is started and `500 Internal Server Error`
otherwise.

#### Request

```text
> GET /_health HTTP/1.1
> Host: localhost:63597
> User-Agent: curl/7.77.0
> Accept: */*
```

#### Response

```text
< HTTP/1.1 200 OK
< Server: akka-http/10.2.0-RC1
< Date: Fri, 09 Jul 2021 15:16:16 GMT
< Content-Type: text/plain; charset=UTF-8
< Content-Length: 2
<
OK
```

## `/_health/readiness`

The server readiness probe.

### `GET | HEAD`

Returns `200 OK` when the server is initialized and `500 Internal Server Error`
otherwise.

#### Request

```text
> GET /_health/readiness HTTP/1.1
> Host: localhost:63597
> User-Agent: curl/7.77.0
> Accept: */*
```

#### Response

```text
< HTTP/1.1 200 OK
< Server: akka-http/10.2.0-RC1
< Date: Fri, 09 Jul 2021 15:30:53 GMT
< Content-Type: text/plain; charset=UTF-8
< Content-Length: 2
<
OK
```

## `/_health/liveness`

The server liveness probe.

### `GET | HEAD`

Checks if all the server subsystems are functioning and returns `200 OK` or
`500 Internal Server Error` otherwise.

#### Request

```text
> GET /_health/liveness HTTP/1.1
> Host: localhost:60339
> User-Agent: curl/7.77.0
> Accept: */*
```

#### Response

```text
< HTTP/1.1 200 OK
< Server: akka-http/10.2.0-RC1
< Date: Fri, 09 Jul 2021 15:35:43 GMT
< Content-Type: text/plain; charset=UTF-8
< Content-Length: 2
<
OK
```

## `/_idle`

The server idleness probe.

### `GET`

Return the amount of time the language server is idle.

#### Request

```text
> GET /_idle HTTP/1.1
> Host: localhost:60339
> User-Agent: curl/7.77.0
> Accept: */*
```

#### Response

```text
< HTTP/1.1 200 OK
< Server: akka-http/10.2.0-RC1
< Date: Fri, 09 Jul 2021 15:44:51 GMT
< Content-Type: application/json
< Content-Length: 21
<
{"idle_time_sec":58}
```

## `/_idle/reset`

The reset request of the server idleness probe.

### `POST`

Reset the idle time of the language server.

#### Request

```text
> POST /_idle/reset HTTP/1.1
> Host: localhost:64996
> User-Agent: curl/7.77.0
> Accept: */*
```

#### Response

```text
< HTTP/1.1 200 OK
< Server: akka-http/10.2.0-RC1
< Date: Mon, 09 Aug 2021 15:37:27 GMT
< Content-Type: text/plain; charset=UTF-8
< Content-Length: 2
<
OK
```

# HTTPS endpoints

Language server can expose HTTPS endpoints when configured appropriately:

1. Project-manager must be told that project's language server should be started
   with https/wss support
   - `NETWORK_ENABLE_HTTPS=true`
2. User should provide appropriate secure configuration. Currently supported are
   PKCS12 bundle with password and certificate with private key. Depending on
   the configuration present, either choice will be sufficient.

If a project-manager is started with `ENSO_HTTPS_PUBLIC_CERTIFICATE` and
`ENSO_HTTPS_PRIVATE_KEY` env variables, SSL context will be created from a
certificate and a private key, respectively. If a project-manager is started
with `ENSO_HTTPS_PKCS12_PATH` and `ENSO_HTTPS_PKCS12_PASSWORD` env variables,
SSL context will be created from a file in PKCS12 format and a password to it,
respectively.

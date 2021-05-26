---
layout: developer-doc
title: Streaming File Transfer
category: language-server
tags: [language-server, protocol, specification]
order: 5
---

# Streaming File Transfer

Particularly important in the separate-server design that we provide with the
Language Server is the ability to transfer files to and from the remote machine.
To that end, it is important that we provide the ability for the IDE to both
upload and download very large files.

A few key requirements:

- We want to support resumption of transfers.
- We want to have transfers be as low-overhead as possible.
- Multiple transfers may be occurring at once.
- We want to keep implementation simple to avoid errors (ideally no state).

<!-- MarkdownTOC levels="2,3" autolink="true" indent="    " -->

- [Control](#control)
- [UX](#ux)

<!-- /MarkdownTOC -->

## Control

In order to make this portion of the protocol simple to manage, it is defined in
a stateless fashion. The Language Server provides two messages `file/writeBytes`
and `file/readByteRange` with corresponding responses. We make the following
assumptions:

- Each request must be completed with a response before sending another request
  for the same file.
- Basic file information can be provided by the existing `file/info` API, which
  allows the IDE to create progress spinners and other such niceties.
- All requests and responses are contained within the `InboundMessage` and
  `OutboundMessage` containers respectively.

The assumption is that, rather than encoding a stateful protocol, we instead
rely on the IDE to control the upload and download of files by sending
successive requests. To upload files, we intend for the IDE to use
`file/writeBytes`, and to download files we intend for `file/readBytes` to be
used.

Resumption of transfers is also handled by the IDE, which may keep track of what
portions of a file have been written or read.

## UX

The IDE wants to be able to provide two major UX benefits to users as part of
this work:

1. Loading spinners that show the progress of the file.
2. A display of parts of the graph that are waiting on the file.

The first requirement here is trivially handled due to the IDE-driven nature of
the file upload and download process. As they know the size of the file being
transferred and get acknowledgements of each chunk of the file that is sent,
they know both the speed and the amount of the file that has been uploaded.

The second requirement is being handled by the addition of the
`Visualization.file_uploading` method to the `Visualization` portion of the
standard library. This is a method that returns a dataflow error
`File_Being_Uploaded path` that will flow through the graph to annotate all
portions waiting on the file upload. All the IDE has to do is insert this
expression implicitly into the source file while the upload is progressing, and
it can trace the impacted nodes and display the necessary UI details.

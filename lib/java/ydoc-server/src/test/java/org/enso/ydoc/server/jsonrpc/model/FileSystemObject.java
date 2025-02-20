package org.enso.ydoc.server.jsonrpc.model;

public record FileSystemObject(String type, String name, FilePath path) {

  public static FileSystemObject file(String name, FilePath path) {
    return new FileSystemObject("File", name, path);
  }
}

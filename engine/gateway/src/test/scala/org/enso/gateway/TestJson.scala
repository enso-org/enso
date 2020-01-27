package org.enso.gateway

import io.circe.Json
import io.circe.literal._

trait TestJson {
  def request: Json
  def expectedResponse: Json
}

object TestJson {

  object Initialize extends TestJson {
    val request =
      json"""
         {
           "jsonrpc": "2.0",
           "method": "initialize",
           "id": 10,
           "params": {
             "capabilities": {}
           }
         }"""

    val expectedResponse =
      json"""
         {
           "jsonrpc": "2.0",
           "id": 10,
           "result": {
             "capabilities": {},
             "serverInfo": {
               "name": "Enso Language Server",
               "version": "1.0"
             }
           }
         }"""
  }

  object WrongJsonrpc extends TestJson {
    val request =
      json"""
         {
           "jsonrpc": "3.0",
           "method": "initialize",
           "id": 10
         }"""

    val expectedResponse =
      json"""
         {
           "jsonrpc": "2.0",
           "id": 10,
           "error": {
             "code": 1,
             "message": "Wrong JSON-RPC Version",
             "data": {
               "retry": false
             }
           }
         }"""
  }

  object WrongMethod extends TestJson {
    val request =
      json"""
         {
           "jsonrpc": "2.0",
           "id": 10,
           "method": "doesntExist"
         }"""

    val expectedResponse =
      json"""
         {
           "jsonrpc": "2.0",
           "id": 10,
           "error": {
             "code": -32601,
             "message": "Method not found"
           }
         }"""
  }

  object Shutdown extends TestJson {
    val request =
      json"""
         {
           "jsonrpc": "2.0",
           "id": 10,
           "method": "shutdown"         
         }"""

    val expectedResponse =
      json"""
         {
           "jsonrpc" : "2.0",
           "id" : 10         
         }"""
  }
}

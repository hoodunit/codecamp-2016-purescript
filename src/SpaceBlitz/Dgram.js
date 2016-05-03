"use strict";

// module SpaceBlitz.Dgram

exports.createRawSocket = function(port){
  return function(callback){
    return function(){
      const dgram = require("dgram");
      const socket = dgram.createSocket("udp4");
      socket.on("error", function(error){
        console.log("error:", error);
        callback({event: "error", rawData: error.toString()})();
      });
      socket.on("message", function(msg){
        callback({event: "message", rawData: msg.toString()})();
      });
      socket.on("listening", function(){
        callback({event: "listening", rawData: ""})();
      });
      socket.on("close", function(){
        callback({event: "close", rawData: ""})();
      });
      socket.bind(port);
      return socket;
    }
  }
}

exports.unsafeSend = function(socket){
  return function(host){
    return function(port){
      return function(msg){
        return function(){
          socket.send(msg, port, host);
        }
      }
    }
  }
}

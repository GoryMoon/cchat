
% This record defines the structure of the state of a client.
-record(client_st, {
  gui, % atom of the GUI process
  nick, % nick/username of the client
  server % atom of the chat server
}).
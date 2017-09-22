-module(server).
-export([start/1,stop/1]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(server_st, {
    server % atom of the chat server,
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(ServerAtom) ->
    #server_st{
        server = ServerAtom
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    PID = genserver:start(ServerAtom, initial_state(ServerAtom), fun handle/2),
    PID.

handle(State, {join, Channel, St}) ->
    io:fwrite("~p~n", ["Join: "++Channel]),
    {reply, ok, State};

handle(State, {leave, Channel, St}) ->
    io:fwrite("~p~n", ["Leave: "++Channel]),
    {reply, ok, State};

handle(State, {message_send, Channel, Msg, St}) ->
    io:fwrite("~p~n", [Channel++": " ++ "tmp>> "++Msg]),
    {reply, ok, State};

% Catch-all for any unhandled requests
handle(State, Req) ->
    {reply, {error, not_implemented, "Server does not handle this command"}, State} .

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.	

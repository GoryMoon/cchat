-module(server).
-include("client.hrl").
-export([start/1,stop/1]).

% This record defines the structure of the state of a server.
% Add whatever other fields you need.
-record(server_st, {
    server, % atom of the chat server,
    channels
}).

-record(channel_st, {
    name,
    atom,
    users
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(ServerAtom) ->
    #server_st{
        server = ServerAtom,
        channels = []
    }.

initial_channel(Name, ChannelAtom) ->
    #channel_st{
        name = Name,
        atom = ChannelAtom,
        users = []
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

handle(State, {join, Channel, Nick}) ->
    {NewState, Atom} = getChannel(State, Channel),
    case genserver:request(Atom, {join, Nick}) of
        ok -> {reply, ok, NewState};
        user_already_joined -> {reply, user_already_joined, NewState}
    end;

% Catch-all for any unhandled requests
handle(State, Req) ->
    {reply, {error, not_implemented, "Server does not handle this command"}, State} .

getChannel(State, Channel) ->
    case lists:keyfind(Channel, 1, State#server_st.channels) of
        false ->
            Atom = list_to_atom(Channel),
            genserver:start(Atom, initial_channel(Channel, Atom), fun handle_channel/2),
            NewState = State#server_st{channels = State#server_st.channels ++ [{Channel, Atom}]},
            {NewState, Atom};
        {Name, Atom} -> {State, Atom}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.


handle_channel(State, {join, Nick}) ->
    Reply = lists:member(list_to_atom(Nick), State#channel_st.users),
    io:fwrite("~p~n", [Reply]),
    if
        Reply =:= true -> {reply, user_already_joined, State};
        true -> NewState = State#channel_st{users = State#channel_st.users ++ [list_to_atom(Nick)]},
                io:fwrite("~p~n", [NewState#channel_st.users]),
                {reply, ok, NewState}
    end;

handle_channel(State, {leave, Nick}) ->
    erlang:error(not_implemented);

handle_channel(State, {message_send, Msg, Nick}) ->
    Reply = lists:member(list_to_atom(Nick), State#channel_st.users),
    io:fwrite("~p~n", [Reply]),
    io:fwrite("~p~n", [State]),
    if
        Reply =:= true ->
            lists:foreach(fun(Elem) ->
                spawn(fun() -> genserver:request(Elem, {message_receive, State#channel_st.name, Nick, Msg}) end) end,
                lists:delete(list_to_atom(Nick), State#channel_st.users)),
            io:fwrite("~p~n", ["Messages sent"]),
            {reply, ok, State};
        true -> {reply, user_not_joined, State}
    end;

handle_channel(State, Req) ->
    {reply, {error, not_implemented, "Channel does not handle this command"}, State} .
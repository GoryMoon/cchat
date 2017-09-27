-module(server).
-include("client.hrl").
-export([start/1,stop/1]).

% This record defines the structure of the state of a server.
-record(server_st, {
    server, % atom of the chat server,
    channels % lists of all channel names with atoms
}).

% This record defines the structure of the state of a channel.
-record(channel_st, {
    name, % name of channel
    atom, % the atom related to the channel
    users % the current users in the channel
}).

% Return an initial state record of a server.
initial_state(ServerAtom) ->
    #server_st{
        server = ServerAtom,
        channels = []
    }.

% Return an initial state record of a channel.
initial_channel(Name, ChannelAtom) ->
    #channel_st{
        name = Name,
        atom = ChannelAtom,
        users = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    PID = genserver:start(ServerAtom, initial_state(ServerAtom), fun handle/2),
    PID.

% Handles join commands from client
handle(State, {join, Channel, Client}) ->
    {NewState, Atom} = getChannel(State, Channel),
    case genserver:request(Atom, {join, Client}) of
        ok -> {reply, ok, NewState};
        user_already_joined -> {reply, user_already_joined, NewState}
    end;

% Catch-all for any unhandled requests
handle(State, _Req) ->
    {reply, {error, not_implemented, "Server does not handle this command"}, State} .

% Gets the channel or creates one if not existing
getChannel(State, Channel) ->
    Atom = list_to_atom(Channel),
    case lists:member(Atom, State#server_st.channels) of
        false ->
            genserver:start(Atom, initial_channel(Channel, Atom), fun handle_channel/2),
            NewState = State#server_st{channels = State#server_st.channels ++ [Atom]},
            {NewState, Atom};
        true -> {State, Atom}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom),
    ok.

% Handles join messages on the channel from the client.
handle_channel(State, {join, Client}) ->
    Reply = lists:member(Client, State#channel_st.users),
    if
        Reply =:= true -> {reply, user_already_joined, State};
        true -> NewState = State#channel_st{users = State#channel_st.users ++ [Client]},
            {reply, ok, NewState}
    end;

% Handles leave messages on the channel from the client.
handle_channel(State, {leave, Client}) ->
    Reply = lists:member(Client, State#channel_st.users),
    if
        Reply =/= true -> {reply, user_not_joined, State};
        true -> NewState = State#channel_st{users = State#channel_st.users -- [Client]},
            {reply, ok, NewState}
    end;

% Handles text messages on the channel from the client and sends to all other users in the channel
handle_channel(State, {message_send, Msg, Client, Nick}) ->
    Reply = lists:member(Client, State#channel_st.users),
    if
        Reply =:= true ->
            lists:foreach(fun(Elem) ->
                spawn(fun() -> genserver:request(Elem, {message_receive, State#channel_st.name, Nick, Msg}) end) end,
                lists:delete(Client, State#channel_st.users)),
            {reply, ok, State};
        true -> {reply, user_not_joined, State}
    end;

% Catch-all for any unhandled requests
handle_channel(State, _Req) ->
    {reply, {error, not_implemented, "Channel does not handle this command"}, State} .
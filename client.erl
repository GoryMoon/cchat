-module(client).
-include("client.hrl").
-export([handle/2, initial_state/3]).

% Return an initial state record. This is called from GUI.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = getServer(ServerAtom)
    }.

% Gets the server of the inputed atom
getServer(ServerAtom) ->
    case is_atom(ServerAtom) of
        true ->
            whereis(ServerAtom);
        false ->
            ServerAtom
end.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    case St#client_st.server of
        undefined ->
            {reply, {error, server_not_reached, "server_not_reached"}, St};
        _ ->
            case genserver:request(St#client_st.server, {join, Channel, self()}) of
                ok -> {reply, ok, St} ;
                user_already_joined -> {reply, {error, user_already_joined, "user_already_joined"}, St}
            end
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    case St#client_st.server of
        undefined ->
            {reply, {error, server_not_reached, "server_not_reached"}, St};
        _ ->
            case genserver:request(list_to_atom(Channel), {leave, self()}) of
                ok -> {reply, ok, St} ;
                user_not_joined -> {reply, {error, user_not_joined, "user_not_joined"}, St}
            end
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case St#client_st.server of
        undefined ->
            {reply, {error, server_not_reached, "server_not_reached"}, St};
        _ ->
            case genserver:request(list_to_atom(Channel), {message_send, Msg, self(), St#client_st.nick}) of
                ok -> {reply, ok, St} ;
                user_not_joined -> {reply, {error, user_not_joined, "user_not_joined"}, St}
            end
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .

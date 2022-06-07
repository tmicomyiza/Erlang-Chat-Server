%%% 
%%% A simple chat client
%%%
%%% Original author:  Caleb Helbling, Fall 2014
%%%
%%% Reorganized and documented by Mark A. Sheldon
%%%   - rather than a start function that takes server contact info,
%%%     now export join functions for joining a local chat room or a 
%%%     potentially remote chat room.
%%%   - Added a participant's name and have the client prefix all
%%%     messages with this.  Could use an empty string to remain
%%%     anonymous.
%%%
%%%
%%% Editor : Mico Theogene Micomyiza
%%% Edited : 15 Feb 2022
%%%
%%%

-module(chat_client).
-export([join_room/3, send_messages/3]).

%%
%% Join a chat room running on the local node
%%
% join(MyName, RoomName) -> join(MyName, node(), RoomName).
 
%%
%% Join a chat room running on an arbitrary node
%%
join_room(MyName, ServerNode, RoomName) ->
    Me       = self(),
    Listener = spawn(fun () -> receive_messages(ServerNode, RoomName, Me) end),
    Server   = {RoomName, ServerNode},
    gen_server:cast(Server, {subscribe, {MyName, Listener}}),
    send_messages(string:concat(MyName, ":  "), Server, {MyName, Listener}),
    gen_server:cast(Server, {unsubscribe, {MyName, Listener}}),
    Listener ! {Me, done},
    ok.



%%
%% A message send loop.
%% Each line typed is sent to the server to be broadcase to all 
%% members of the room
%% Terminates when the user types "$" on a line all by itself.
%%
send_messages(Prefix, ServerInfo, Sub) ->
    % io:format("~s server info ~w~n", [ServerInfo, Prefix]),
    {RoomName, _Pid} = ServerInfo, 
    case io:get_line("Enter a message: ") of 
        "--quit\n"-> gen_server:cast(ServerInfo, {unsubscribe, Prefix}),
                    ok;
        "--list\n"->gen_server:call(ServerInfo,{get_members,{RoomName, Sub}}),
                    send_messages(Prefix, ServerInfo, Sub);
        Message -> gen_server:call(ServerInfo, 
                {send_message, {RoomName, string:concat(Prefix, Message)}}),
                   send_messages(Prefix, ServerInfo, Sub)
    end.

%%
%% Receive and print messages on the screen.
%% Intended to run in its own process
%%
%% Stop if the server tells us to (e. g., because it's going down),
%% or if this client is leaving the chat.
%%
receive_messages(ServerNode, RoomName, ParentPid) ->
    receive
        {ServerNode, RoomName, message, Message} ->
            erlang:display(Message),
            receive_messages(ServerNode, RoomName, ParentPid);
        {ServerNode, RoomName, stop} ->
            ok;
        {ParentPid, done} ->
            ok;
        Any -> io:format("Unhandled message: ~p~n", [Any]),
               receive_messages(ServerNode, RoomName, ParentPid)
    end.

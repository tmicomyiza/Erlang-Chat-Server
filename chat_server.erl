%%%
%%% A simple distributed chat server.
%%%
%%% Original author:  Caleb Helbling, Fall 2014
%%%
%%% A subscription is a {Name, Pid} pair, but the server
%%% doesn't currently use the Name for anything.  Could look 
%%% up the sender's Pid in the list, get the name, and prefix
%%% message sent from that Pid with the Name.  But currently
%%% leave that up to the client code.
%%%
%%% TODO
%%%     Handle users leaving room
%%%
%%% Note:
%%%     To use without DNS issues, e. g., to use with laptops and
%%%     phones, etc., use the IP address in the node name.  For
%%%     example, you might start up erlang like this:
%%%
%%%         erl -name marks-laptop@192.164.1.123
%%%
%%%     You can list a devices IP addresses using inet:getifaddrs/0
%%%     Here is something someone posted at 
%%%         stackoverflow.com/questions/32984215/erlang-finding-my-ip-address
%%%     
%%%     local_ip_v4() ->
%%%          {ok, Addrs} = inet:getifaddrs(),
%%%          hd([
%%%               Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
%%%               size(Addr) == 4, Addr =/= {127,0,0,1}
%%%          ]).
%%%
-module(chat_server).
-export([start/1, stop/1]).

start(RoomName) -> register(RoomName, spawn(fun() -> loop(RoomName, []) end)).

stop(RoomName)  ->
    RoomPid = whereis(RoomName),
    unregister(RoomName),
    RoomPid ! stop,
    ok.
   

loop(RoomName, Subscribers) ->
    receive
	{subscribe, Sub = {Name, Pid}} when is_pid(Pid) ->
	    io:format("~s subscribed to ~w~n", [Name, RoomName]),
	    loop(RoomName, [Sub|Subscribers]);
	{unsubscribe, Sub = {Name, _}} ->
	    io:format("~s unsubscribed from ~w~n", 
		      [Name, RoomName]),
	    loop(RoomName, lists:delete(Sub, Subscribers));
	{send_message, Message} ->
	    lists:foreach(fun({_, Pid}) ->
				  Pid ! {node(), RoomName,
					 message, Message}
			  end,
			  Subscribers),
	    loop(RoomName, Subscribers);
	{get_subscribers, From} when is_pid(From) ->
	    From ! Subscribers,
	    loop(RoomName, Subscribers);
	stop ->
	    ok;
	Other ->
	    io:format("Unexpected message:  ~w~n",
		     [Other]),
	    loop(RoomName, Subscribers)
    end.

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
%%% TODO
%%%      Currently, listener process is never terminated.
%%%      Leaving chat room should tell server user is leaving
%%%      and server can send stop message to listener, or the message
%%%      send loop could be given PID of listener.  Anyway, server 
%%%      should stop sending messages here.

% -module(chat_client).
% -export([join_room/3, send_messages/2]).

% %%
% %% Join a chat room running on the local node
% %%
% % join(MyName, RoomName) -> join(MyName, node(), RoomName).
 
% %%
% %% Join a chat room running on an arbitrary node
% %%
% join_room(MyName, ServerNode, RoomName) ->
%     Me       = self(),
%     Listener = spawn(fun () -> receive_messages(ServerNode, RoomName, Me) end),
%     Server   = {RoomName, ServerNode},
%     gen_server:call(Server, {subscribe, {MyName, Listener}}),
%     send_messages(string:concat(MyName, ":  "), Server),
%     gen_server:call(Server, {unsubscribe, {MyName, Listener}}),
%     Listener ! {Me, done},
%     ok.


% %%
% %% A message send loop.
% %% Each line typed is sent to the server to be broadcase to all 
% %% members of the room
% %% Terminates when the user types "$" on a line all by itself.
% %%
% send_messages(Prefix, ServerInfo) ->
%     % io:format("~s server info ~w~n", [ServerInfo, Prefix]),
%     {RoomName, _} = ServerInfo, 
%     case io:get_line("Enter a message: ") of 
%         "\$\n" -> ok;
%         Message -> gen_server:call(ServerInfo, {send_message, {RoomName, string:concat(Prefix, Message)}}),
%                    send_messages(Prefix, ServerInfo)
%     end.

% %%
% %% Receive and print messages on the screen.
% %% Intended to run in its own process
% %%
% %% Stop if the server tells us to (e. g., because it's going down),
% %% or if this client is leaving the chat.
% %%
% receive_messages(ServerNode, RoomName, ParentPid) ->
%     receive
%         {ServerNode, RoomName, message, Message} ->
%             erlang:display(Message),
%             receive_messages(ServerNode, RoomName, ParentPid);
%         {ServerNode, RoomName, stop} ->
%             ok;
%         {ParentPid, done} ->
%             ok;
%         Any -> io:format("Unhandled message: ~p~n", [Any]),
%                receive_messages(ServerNode, RoomName, ParentPid)
%     end.

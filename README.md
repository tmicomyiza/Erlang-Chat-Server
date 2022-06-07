# Erlang-Chat-Server
Develop a general chat server using Erlang OTP

# How to use the chat

1. To create a chat room: chat:start_link(atom) *atom is the room Name

2. To join a chat room: chat_client:join_room(UserName, Room_Node, Room_Name)

3. To quit chat room: --quit

4. To list people in the chatroom: --list

# Module Structure
## chat_client: contains the client API

    - join_room/3: let user join the room if it exists.
    - send_messages/3: allows chat user to post message to chat room
    - recieve_messages/3: the listern for user to get messages from 
        other people in the chat room

## chat: contains Server API
    - start_link/1
    - stop/1
    -terminate/2
    -handle_call/3
    -handle_cast/2

## Key design decision

For my implementation when a user tries to join room which doesn't exists, the
server will exit with an exception. 
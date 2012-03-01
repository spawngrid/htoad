-module(htoad_ssh).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-rules([remote_ssh_command]).

remote_ssh_command(Engine, {remote, #endpoint{ 
                          type = ssh,
                          hostname = Host,
                          username = Username
                         }, #shell{ cmd = Cmd }}) ->
    Hostname = (case Username of undefined -> ""; _ -> Username ++ "@" end) ++ Host,
    htoad:assert(Engine,#shell{ cmd = "ssh " ++ Hostname ++ " " ++ Cmd }).

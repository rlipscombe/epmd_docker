-module(epmd_docker_dist).
%% https://erlang.org/doc/apps/erts/alt_dist.html
-export([listen/1, listen/2, address/0, accept/1, accept_connection/5, select/1, setup/5, close/1, childspecs/0]).

listen(Name) ->
    inet_tcp_dist:listen(Name).

listen(Name, Host) ->
    inet_tcp_dist:listen(Name, Host).

address() ->
    inet_tcp_dist:address().

accept(Listen) ->
    inet_tcp_dist:accept(Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    inet_tcp_dist:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).

select(_Node) ->
    % todo Is 'Node' a docker container or alias?
    true.

setup(Node, Type, MyModule, LongOrShortNames, SetupTime) ->
    inet_tcp_dist:setup(Node, Type, MyModule, LongOrShortNames, SetupTime).

close(Listen) ->
    inet_tcp_dist:close(Listen).

childspecs() ->
    inet_tcp_dist:childspecs().

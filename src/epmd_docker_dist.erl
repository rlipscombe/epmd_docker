-module(epmd_docker_dist).
-export([listen/1, listen/2, address/0, accept/1, accept_connection/5, select/1, setup/5, close/1, childspecs/0]).

-define(MOD, inet_tcp_dist).

listen(Name) ->
    ?MOD:listen(Name).

listen(Name, Host) ->
    ?MOD:listen(Name, Host).

address() ->
    ?MOD:address().

accept(Listen) ->
    ?MOD:accept(Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    ?MOD:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).

% This is the only interesting thing in here. By default, inet_tcp_dist does a DNS lookup to
% decide whether or not to handle this traffic. But the point of this exercise is that DNS
% lookups won't work. So we'll lie and say we *do* handle the traffic.
select(_Node) ->
    % @todo Is 'Node' *actually* a docker container or alias?
    true.

setup(Node, Type, MyModule, LongOrShortNames, SetupTime) ->
    ?MOD:setup(Node, Type, MyModule, LongOrShortNames, SetupTime).

close(Listen) ->
    ?MOD:close(Listen).

childspecs() ->
    ?MOD:childspecs().

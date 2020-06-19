%% Problem: You're attempting to connect to a docker container from the host.
%% DNS won't work to look up the container IP from the host.
%% But using the IP address won't work either, because the node is using the container ID,
%% and Erlang wants the names to match.
%% So, here's a pile of hacks to make it work.
-module(epmd_docker).
-export([start_link/0, register_node/3, names/1, port_please/2, address_please/3]).

start_link() ->
    erlang:display(io_lib:format("start_link\n", [])),
    ignore.

register_node(_Name, _Port, _Family) ->
    erlang:display(io_lib:format("register_node\n", [])),
    Creation = rand:uniform(3),
    {ok, Creation}.

names(_Host) ->
    {error, address}.

%% Note that address_please (or port_please) are never called if
%% inet_tcp_dist fails to look up the address. So we replace that, too. See epmd_docker_dist.
address_please(Name, Host, AddressFamily) ->
    io:format("address_please ~p ~p ~p\n", [Name, Host, AddressFamily]),
    IP = get_container_ip(Host),
    {ok, IP}.

get_container_ip(Host) ->
    Cmd = lists:flatten(
            io_lib:format(
              "docker inspect --format '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ~s", [Host])),
    Address = string:trim(os:cmd(Cmd)),
    {ok, IPAddress} = inet:parse_address(Address),
    IPAddress.

port_please(Name, IP) ->
    erlang:display(io_lib:format("port_please ~p ~p\n", [Name, IP])),
    error(not_implemented).

% See also https://www.erlang-solutions.com/blog/erlang-and-elixir-distribution-without-epmd.html
% and https://erlang.org/doc/man/erl_epmd.html#address_please-3

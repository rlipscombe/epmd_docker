%% Problem: You're attempting to connect to a docker container from the host.
%% DNS won't work to look up the container IP from the host.
%% But using the IP address won't work either, because the node is using the container ID,
%% and Erlang wants the names to match.
%% So, here's a pile of hacks to make it work.
-module(epmd_docker).
-export([start_link/0, register_node/3, names/1, port_please/2, address_please/3]).

start_link() ->
    ignore.

register_node(_Name, _Port, _Family) ->
    Creation = rand:uniform(3),
    {ok, Creation}.

names(_Host) ->
    {error, address}.

address_please(_Name, Host, _AddressFamily) ->
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
    erl_epmd:port_please(Name, IP).

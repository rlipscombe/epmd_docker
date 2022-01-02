%%% Problem: You're attempting to connect to a docker container from the host.
%%% DNS won't work to look up the container IP from the host.
%%% But using the IP address won't work either, because the node is using the container ID,
%%% and Erlang wants the names to match.
%%% So, here's a pile of hacks to make it work.
%%% By default, Erlang uses the erl_epmd module for discovery; see https://www.erlang.org/doc/man/erl_epmd.html.
%%% This is a replacement; see https://www.erlang.org/doc/apps/erts/alt_disco.html for callback details.
-module(epmd_docker).
-export([start_link/0, register_node/3, names/1, port_please/2, address_please/3]).

%% Start any processes needed by the discovery module.
start_link() ->
    ignore.

%% Register the given node name with the registrar.
%% Because we're a "client" node, we don't need to be discoverable, so we don't bother registering.
register_node(_Name, _Port, _Family) ->
    Creation = rand:uniform(3),
    {ok, Creation}.

%% Returns the names and associated port numbers of the Erlang nodes that epmd registered at the specified host.
%% Returns {error, address} if epmd is not operational.
names(_Host) ->
    {error, address}.

%% Return the address of the given node.
%% The default implementation (see https://github.com/erlang/otp/blob/master/lib/kernel/src/erl_epmd.erl)
%% uses inet:getaddr, but that won't work because host DNS doesn't know anything about docker containers.
%% So we use the docker CLI to find the IP address for the given container name instead.
address_please(_Name, Host, _AddressFamily) ->
    IP = get_container_ip(Host),
    {ok, IP}.

%% Run "docker inspect" to find the IP address of the container.
%% OTP-19 has support for unix sockets, so we _might_ be able to persuade httpc to talk to the Docker engine directly.
get_container_ip(Host) ->
    Cmd = lists:flatten(
            io_lib:format(
              "docker inspect --format '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ~s", [Host])),
    Address = string:trim(os:cmd(Cmd)),
    {ok, IPAddress} = inet:parse_address(Address),
    IPAddress.

%% Return the distribution port used by the given node.
%% Because we know that epmd is running on the other host, we can just use the default implementation.
port_please(Name, IP) ->
    erl_epmd:port_please(Name, IP).

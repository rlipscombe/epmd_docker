# epmd_docker

I've got a bunch of Erlang nodes running in Docker containers, and I'd
like to connect a remote shell, running on the host, to one of them.

To connect a remote shell, I'd normally just do this:

    erl -sname shell$$ -setcookie $COOKIE -remsh node@host

But when the node is running in a Docker container, this doesn't work, because
`host` needs to be resolvable to an IP address by the host.

You can't just use the IP address, because Erlang's distribution protocol
requires the host to be specified the same way on both ends.

This could be resolved by using `-name node@$MY_IP`, but I don't (for various reasons)
want to do that right now.

You can tell the host about the docker container IP addresses by:

1. messing with `/etc/hosts` (which requires root access)
2. or by specifying a custom inet configuration file (see http://erlang.org/pipermail/erlang-questions/2019-June/098036.html)

The first two items are brittle, because you need to update that file every time
your containers change their IP addresses.

So I decided to subvert epmd instead.

You can find a number of blog posts about doing Erlang distribution without epmd:

- https://www.erlang-solutions.com/blog/erlang-and-elixir-distribution-without-epmd.html
- https://github.com/tsloughter/epmdless
- https://erlang.org/doc/apps/erts/alt_disco.html

But epmd is working fine: it's running in the docker containers; I just need to subvert
the host name resolution.

The first thing we'll need is a "discovery module". It looks like this:

```
-module(epmd_docker).
-export([start_link/0,
         register_node/3,
         names/1,
         port_please/2,
         address_please/3]).

start_link() ->
    ignore.

register_node(_Name, _Port, _Family) ->
    Creation = rand:uniform(3),
    {ok, Creation}.

names(_Host) ->
    {error, address}.

address_please(Name, Host, AddressFamily) ->
    IP = get_container_ip(Host),
    {ok, IP}.

get_container_ip(Host) ->
    % This should probably be made more robust.
    Cmd = lists:flatten(
            io_lib:format(
              "docker inspect --format '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ~s", [Host])),
    Address = string:trim(os:cmd(Cmd)),
    {ok, IPAddress} = inet:parse_address(Address),
    IPAddress.

port_please(Name, IP) ->
    erl_epmd:port_please(Name, IP).
```

Somewhat annoyingly, `inet_tcp_dist` _also_ looks up the host in DNS, so we need
to subvert that too:

```
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

select(_Node) ->
    % This is the subversive bit. OTP iterates over registered distribution
    % modules, looking for the first one that says it can connect to the
    % specified node. 'inet_tcp_dist' does a name lookup, which fails.
    % We should probably check that the specified node is a docker container
    % ID, but for now we'll lie and say we can deal with it.
    true.

setup(Node, Type, MyModule, LongOrShortNames, SetupTime) ->
    ?MOD:setup(Node, Type, MyModule, LongOrShortNames, SetupTime).

close(Listen) ->
    ?MOD:close(Listen).

childspecs() ->
    ?MOD:childspecs().
```

And then we can try it out:

```
erlc -o ebin src/epmd_docker.erl
erlc -o ebin src/epmd_docker_dist.erl
```

```
erl -pa ebin \
    -proto_dist epmd_docker \
    -start_epmd false -epmd_module epmd_docker \
    -sname shell$$ -setcookie $COOKIE \
    -remsh node@$CONTAINER_ID
```

...or we can join the cluster (which allows us to run observer, etc.):

```
erl -pa ebin \
    -proto_dist epmd_docker \
    -start_epmd false -epmd_module epmd_docker \
    -sname admin$$ -setcookie $COOKIE
```

```
net_kernel:connect_node(Node).
observer:start().
```

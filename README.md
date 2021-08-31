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

Both of these are brittle, because you need to update that file every time
your containers change their IP addresses.

So I decided to subvert epmd instead.

You can find a number of blog posts about doing Erlang distribution without epmd:

- https://www.erlang-solutions.com/blog/erlang-and-elixir-distribution-without-epmd.html
- https://github.com/tsloughter/epmdless
- https://erlang.org/doc/apps/erts/alt_disco.html

But epmd is working fine: it's running in the docker containers; I just need to subvert
the host name resolution.

## Compiling it

    make

## Using it

```
erl -pa ebin \
    -proto_dist epmd_docker \
    -start_epmd false -epmd_module epmd_docker \
    -sname shell$$ -setcookie $COOKIE \
    -remsh node@$CONTAINER_ID
```

Note: The node name that you connect to needs to match the node's own idea of its
name. This means that if you're using hex-style container IDs as the container's
hostname (this is the default), you _also_ need to use hex-style container IDs in
`$CONTAINER_ID`. Using the container name will just give you the dreaded
`*** ERROR: Shell process terminated! (^G to start new job) ***` message.

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

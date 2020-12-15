all:
	-mkdir -p ebin
	erlc -o ebin src/epmd_docker.erl
	erlc -o ebin src/epmd_docker_dist.erl

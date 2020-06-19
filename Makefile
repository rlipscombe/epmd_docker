all:
	erlc -o ebin src/epmd_docker.erl
	erlc -o ebin src/epmd_docker_dist.erl

run:
	erl -pa ebin \
	    -proto_dist epmd_docker \
	    -start_epmd false -epmd_module epmd_docker \
	    -sname cli$$$$

INCLUDE=-pa deps/*/ebin -pa apps/*/ebin -pa ebin

all:
	./rebar compile
run:
	erl $(INCLUDE) \
	-kernel inetrc '"./erl_inetrc"' -connect_all false \
	-boot start_sasl -name gnssfe -config gnssfe.config -s lager start -s gnssfe start -s sync go
deploy:
	erl -detached -noshell -noinput $(INCLUDE) \
	-kernel inetrc '"./erl_inetrc"' -connect_all false \
	-boot start_sasl -name gnssfe -config gnssfe.config \
	-s lager start \
	-s application start feapi \
	-s sync go

attach:
	erl -name con`jot -r 1 0 100` -hidden -remsh gnssfe@`hostname`
	#rlwrap --always-readline 
stop:
	echo 'halt().' | erl -name obsrdrcon -remsh gnssfe@`hostname`

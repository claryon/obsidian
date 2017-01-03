all: client contract demo

client: client.beam

contract: contract.beam

demo: demo.beam demo_run

%.beam: %.erl
	erlc $<

demo_run:
	@erl -noshell -s demo run -s init stop	

run:
	@erl -noshell -s client start 1 -s init stop	

clean:
	rm -f *.beam
	rm -f *.dump

.PHONY: all client contract demo demo_run run clean 	

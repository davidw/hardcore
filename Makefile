all:
	rebar compile

dialyzer: build.plt
	dialyzer -nn --plt $< ebin

build.plt:
	dialyzer -q --build_plt --apps erts kernel stdlib ssl --output_plt $@

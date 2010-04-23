NAME		:= epgsql_pool
VERSION		:= 0.1

ERL  		:= erl
ERLC 		:= erlc

EPGSQL_EBIN	:= ~/src/epgsql/ebin

# ------------------------------------------------------------------------

ERLC_FLAGS	:= -Wall 

SRC			:= $(wildcard src/*.erl)
TESTS 		:= $(wildcard test_src/*.erl)
RELEASE		:= $(NAME)-$(VERSION).tar.gz

APPDIR		:= $(NAME)-$(VERSION)
BEAMS		:= $(SRC:src/%.erl=ebin/%.beam) 

compile: $(BEAMS)

app: compile
	@mkdir -p $(APPDIR)/ebin
	@cp -r ebin/* $(APPDIR)/ebin/

release: app
	@tar czvf $(RELEASE) $(APPDIR)

clean:
	@rm -f ebin/*.beam
	@rm -rf $(NAME)-$(VERSION) $(NAME)-*.tar.gz

test: $(TESTS:test_src/%.erl=test_ebin/%.beam) $(BEAMS)
	$(ERL) -pa $(EPGSQL_EBIN) -pa ebin/ -pa test_ebin/ -noshell -s pgsql_pool_tests test -s init stop

# ------------------------------------------------------------------------

.SUFFIXES: .erl .beam
.PHONY:    app compile clean test

ebin/%.beam : src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(dir $@) $<

test_ebin/%.beam : test_src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(dir $@) $<

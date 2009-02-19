all:
	erlc -o ebin src/erlsyslog.erl

clean:
	rm -f src/*~ ebin/*.beam *~

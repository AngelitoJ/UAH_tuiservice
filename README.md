TUI micro service demo
======================

TUI is a project leaded by a Spanish bank in collaboration with several Spanish universities, to provide smart card services 
to College Students and Staff.

This is a Quick and dirty hack based on cowlboy to provide micro setup for a TUI web services demo. 


	$ ./tuiservice -?
	tuiservice Version: 1.0

	Usage: ./tuiservice [-D <dbfile>] [-f <dbformat>] [-T [<timeout>]]
                    	[-P [<procs>]] [-V] [-v] [-d [<debug>]] [-?]
                    	[-P [<port>]]

		  -D, --dbfile   DB Data file.
		  -f, --format   DB format <terms|csv>
		  -T, --timeout  Default app timeout in seconds. [default: 300]
		  -P, --cores    Number of workers (default 2*core). [default: 8]
		  -V, --version  Show software version.
		  -v, --verbose  Show all actions performed.
		  -d, --debug    Show debug info. [default: 0]
		  -?, --help     Show this help.
		  -P, --Port     Default Listening TCP port [default: 8080]

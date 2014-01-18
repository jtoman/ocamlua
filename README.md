Introduction
--

Ocamlua (sometimes stylized as OCamLua) is a high level API that
allows OCaml code to call Lua code.

Some features of the library:
* It is a high level API abstracting away low-level manipulations of
the Lua stack. The API consists of only 4 core functions and a couple
of convenience functions.
* It allows for callbacks from Lua code into OCaml code; any arbitrary
closure passed into the Lua code can be called like any other Lua
function
* Great care has been taken to ensure that the library is documented thoroughly.

License
--

OCamLua is licensed under the terms of the Apache License V2.0. If for
some reason you want to use OCamLua and the terms of the license are
unacceptable please contact me and we'll see what we can work out.

Requirements
--

This API is designed to work with Lua 5.2 only. It's possible that if
there is considerable interest in a 5.1 version that 5.1 will be
supported sometime in the future. You will need the Lua shared
libraries and the header files installed before you can build this
software. If you wish to use the provided OMakefile you'll need the
Omake executables. The unit tests use the oUnit 1.x library.

Configuration
--

The location of the Lua 5.2 header files and the name of the Lua
shared are controled by the variables LUA\_INCLUDE\_DIR and LUA\_LIB
respectively. By default, if building the bytecode version of the library
the makefile will assume dynamic linking. If this is not desirable set the
USE_CUSTOM flag to true in the OMakefile.

Building
--

Included is an OMakefile that automatically builds the library. The
OMake project doesn't have very good support for building shared
OCaml/C libraries so support may be shaky. Please report any problems
you may have. To build the libraries run "omake" in the top level
directory.

To build the test run "omake ocamlua_test" and run the generated
executable of the same name.

Installing
--

To install ocamlua run "omake install". By default the OMakefile will
attempt to install the files in the system library folder (usually
/usr/lib/ocaml). This can be overridden by setting LIB\_DIR when
invoking omake. In addition an installation prefix can be specified
via DEST\_DIR on the command line.

Documentation
---

To get the documentation for the OCamLua library simply run ocamldoc
on the ocamlua.mli file. Consult the official documentation for the
full array of options.

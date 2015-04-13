#!/bin/bash
# Put your command below to execute your program.
# Replace "./my-program" with the command that can execute your program.
# Remember to preserve " $@" at the end, which will be the program options we give you.

# Do preprocessing (run it once is enough)
/usr/bin/time -p ./gauche/bin/gosh prep-inv.ss $@
/usr/bin/time -p ./gauche/bin/gosh prep-veclen.ss $@
/usr/bin/time -p ./gauche/bin/gosh prep-postidx.ss $@

# Do retrieval
./gauche/bin/gosh tf-idf.ss $@

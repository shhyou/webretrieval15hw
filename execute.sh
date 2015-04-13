#!/bin/bash
# Put your command below to execute your program.
# Replace "./my-program" with the command that can execute your program.
# Remember to preserve " $@" at the end, which will be the program options we give you.

# Do preprocessing (run it once is enough)
./gauche/bin/gosh prep-invidx.ss $@
./gauche/bin/gosh prep-veclen.ss $@
./gauche/bin/gosh prep-postidx.ss $@

# Do retrieval
./gauche/bin/gosh tf-idf.ss $@

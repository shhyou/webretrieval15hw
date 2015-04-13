#!/bin/bash
# Put your command below to execute your program.
# Replace "./my-program" with the command that can execute your program.
# Remember to preserve " $@" at the end, which will be the program options we give you.

# Do preprocessing (run it once is enough)
./gosh prep-invidx.ss $@
./gosh prep-veclen.ss $@
./gosh prep-postidx.ss $@

# Do retrieval
./gosh tf-idf.ss $@

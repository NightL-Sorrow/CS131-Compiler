import os
import sys
import re

os.system("rm -f 1.s std.s")
# os.system("./lexer t.cl | ./parser | ./semant | ./cgen > 1.s")
os.system("./lexer t.cl | ./parser | ./semant | ./cgen2 > std.s")

# os.system("spim -exception_file trap.s -file 1.s")
os.system("spim -exception_file trap.s -file std.s")

We are using an existing ES which we have modified in the past,
as our starting point.

birds-en.nkb        Knowledge base completely converted to "English".
birds-mix.nkb       Knowledge base part Prolog, part English.
birds.nkb           Knowledge base in full Prolog.
native-original.pl  The original, unmodified native shell.
native.pl           The shell that I modified and used for assignment 2 and 3.
op.pl               Operators file, included in other files when use of ops needed.
readme.txt          This file.

Using SWI-Prolog:
$ prolog -s native.pl
  ?- main. load. 'birds-en.nkb'.

hsflags = -XUnicodeSyntax -fwarn-incomplete-patterns
all:V:

o.%: `{ls *.hs}
	ghc $hsflags -main-is $stem.main $stem -o o.$stem


clean:V:
	rm -f *.o *.hi o.*

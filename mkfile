all:V:

o.%: `{ls *.hs}
	ghc -fwarn-incomplete-patterns -main-is $stem.main $stem -o o.$stem


clean:V:
	rm -f *.o *.hi o.*

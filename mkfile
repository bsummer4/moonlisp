all:V: o.Moonlisp
o.%: `{ls *.hs}
	ghc -main-is $stem.main $stem -o o.$stem

clean:V:
	rm -f *.o *.hi o.*

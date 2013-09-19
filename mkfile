all:V: o.Lua o.IR
o.%: `{ls *.hs}
	ghc -main-is $stem.main $stem -o o.$stem

clean:V:
	rm -f *.o *.hi o.*

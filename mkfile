all:V: Lua IR
%: %.hs
	ghc -main-is $stem.main $stem

clean:V:
	rm -f *.o *.hi Lua IR

all:V: Lua IR
%:V:
	ghc -main-is $stem.main $stem

clean:V:
	rm -f *.o *.hi Lua IR

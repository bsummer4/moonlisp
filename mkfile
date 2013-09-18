all:V:
Lua: Lua.hs Ty.hs Sexp.hs
	ghc -main-is Lua.main Lua
clean:V:
	rm -f *.o *.hi Lua

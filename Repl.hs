{-# LANGUAGE UnicodeSyntax #-}

module Repl(repl) where
import Prim
import Util
import Data.IORef
import System.IO
import System.IO.Error
import Control.Exception

data ChTy = NestCh | UnNestCh | SpaceCh | WordCh
classify c =
	if c `elem` "(“{[" then NestCh else
	if c `elem` "]}”)" then UnNestCh else
	if c `elem` " \t\n\r" then SpaceCh else
	WordCh

newBuf = newIORef (Nothing ∷ Maybe Char)
getc b = readIORef b >>= (\c→case c of {Nothing→getChar; Just c→return c})
ungetc b c = readIORef b >>= \c → case c of
	Nothing → writeIORef b c
	Just _ → error "invalid use of getc/ungetc."

endl = putStrLn "" >> hFlush stdout
unexpectedChar c = hPutStrLn stderr ("ignoring invalid character: " ++ show c)
getForm nl b = getc b >>= \c → case classify c of
	SpaceCh → do
		if and[nl,c≡'\n'] then endl else return ()
		getForm (c≡'\n') b
	NestCh → getNestForm b [c] 1
	UnNestCh → unexpectedChar c >> getForm nl b
	WordCh → getWordForm b [c]

getNestForm b acc d = getc b >>= \c → case (d,classify c) of
	(_,NestCh) → getNestForm b (c:acc) (d+1)
	(1,UnNestCh) → return $ reverse (c:acc)
	(_,UnNestCh) → getNestForm b (c:acc) (d-1)
	(_,SpaceCh) → getNestForm b (c:acc) d
	(_,WordCh) → getNestForm b (c:acc) d

getWordForm b acc = getc b >>= \c → case classify c of
	WordCh → getWordForm b (c:acc)
	_ → ungetc b c >> (return $ reverse acc)

repl init f = do
	init
	b ← newBuf
	handle die $ getForm False b >>= p >> hFlush stdout >> repl (return()) f where
		p s = case f s of {Nothing→return(); Just s'→putStrLn s'}
		die e = if isEOFError e then d else d where d=return()

-- This defines the basic types and typeclasses.
module Ty where
class CodeGen a where { cgen::a->String }

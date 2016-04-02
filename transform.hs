-- transform.hs
-- transformation rules in Haskell

-- symbolic data type
data Symbolic = SymInt Int | Sym [Char] deriving (Eq, Show)
displaySymbolic :: Symbolic -> String
displaySymbolic (SymInt x) = (show x) ++ " "
displaySymbolic (Sym x)    = x

data Expression = Exp [Symbolic]
displayExp :: Expression -> String
displayExp (Exp exp) = foldl (++) "" (map displaySymbolic exp)
instance Show Expression where show a = displayExp a

-- function to apply transformation rules
replace :: (Symbolic, Symbolic) -> Expression -> Expression
replace (patt, to) (Exp exp) = Exp (map (repSym (patt, to)) exp)

repSym :: (Symbolic, Symbolic) -> Symbolic -> Symbolic
repSym (patt, to) sym = if (patt == sym) then to else sym

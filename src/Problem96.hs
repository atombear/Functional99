module Problem96 (identifierAda) where

-- letter -> end
-- letter -> letter
-- letter -> -
-- - -> letter
-- letter -> digit
-- - -> digit
-- digit -> end
-- digit -> -
-- digit -> letter
-- digit -> digit


data Sym = Letter | Hyphen | Digit | Reject deriving (Eq, Show)

update :: Sym -> Sym -> Sym
update Reject _ = Reject
update _ Reject = Reject

update Letter Letter = Letter
update Letter Hyphen = Hyphen
update Letter Digit = Digit

update Hyphen Letter = Letter
update Hyphen Hyphen = Reject
update Hyphen Digit = Digit

update Digit Letter = Letter
update Digit Hyphen = Hyphen
update Digit Digit = Digit


toSym :: Char -> Sym
toSym c
    | elem c (['a'..'z'] ++ ['A'..'Z']) = Letter
    | elem c ['0'..'9'] = Digit
    | c == '-' = Hyphen
    | otherwise = Reject


identifierAda :: String -> Bool
identifierAda "" = False
identifierAda (x:xs) = (final == Letter) || (final == Digit)
    where final = foldl (\x -> \y -> update x (toSym y)) first_char xs
          first_char = if (toSym x) == Letter then Letter else Reject

main = do
    print $ identifierAda "this-is-a-long-identifier"
    print $ identifierAda "this-ends-in-"
    print $ identifierAda "two--hyphens"
    print $ identifierAda "123"
    print $ identifierAda ""

-- helper functions to generate a single letter’s phrase
helper :: [Char] -> [Char]
helper inputstring = let
    firststring = [inputstring !! 0] ++ " is for " ++ inputstring ++ ", "
    in firststring

-- helper function to slice a list
slice :: Int -> Int -> [Char] -> [Char]
slice from to xs = take (to - from + 1) (drop from xs)

-- speller function implementation
speller :: [[Char]] -> [Char]
speller inputlist = let
    helperlist = map helper inputlist
    completestring = foldl (++) "" helperlist
    result = slice 0 ((length completestring) - 3) completestring
    in result

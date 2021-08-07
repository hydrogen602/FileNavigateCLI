-- tries

zPlus = [2..]

odd = filter (\x -> x `mod` 2 > 0)

removeFactor f = filter (\x -> x `mod` f > 0)

sieveTest (e:ls) = e:sieveTest (removeFactor e ls)

-- refined


sieveHelper (e:ls) = e:sieveHelper (filter (\x -> x `mod` e > 0) ls)
sieve = sieveHelper [2..]


main :: IO ()
main = do
    putStrLn "hello world"
    print $ take 10 sieve
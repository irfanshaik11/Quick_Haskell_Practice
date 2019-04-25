import System.Random
import System.IO.Unsafe
import System.IO
import Control.Monad



placeLetter :: String -> String -> String
placeLetter word [] =replicate (length word) '_'
placeLetter [] _ = []
placeLetter word guessedletters
	| isValid   = [head word] ++ (placeLetter (tail word) guessedletters)
	| otherwise = "_" ++ (placeLetter (tail word) guessedletters)
	where
		isValid = elem (head word) guessedletters

invalidguess :: String -> String -> IO()
invalidguess word guessedletters= do
	print "Invalid Guess: Only One Character Allowed Per Guess"
	runGame word guessedletters

runGame :: String -> String -> IO()
runGame word guessedletters = do
	let str = placeLetter word guessedletters
	putStrLn str
	when (str /= word) $ do
		guess <- getLine
		if length guess == 1 then runGame word (guessedletters ++ guess)
		else 
			invalidguess word guessedletters
main :: IO()
main = do
	contents <- readFile "words.txt"
	let wordsinfile = lines contents
	let numwords = length wordsinfile
	randomnum <- randomIO
	let randomword = wordsinfile !! (randomnum `mod` numwords - 1)
	runGame randomword ""
import Data.Word (Word8)
import Data.List (foldl1')
import Data.Maybe (listToMaybe)
import System.Environment(getArgs)

type Program = [Command]

data Command
    = Increment
    | Decrement
    | Leftward
    | Rightward
    | Input
    | Output
    | Loop Program
    deriving (Show)

data Tape a = Tape [a] a [a]

instance Functor Tape where
  fmap f (Tape xs y zs) = Tape (fmap f xs) (f y) (fmap f zs)

initializeTape :: a -> Tape a
initializeTape x = x <$ Tape [1..] 1 [1..]

forward :: Tape a -> Tape a
forward (Tape xs y (z:zs)) = Tape (y:xs) z zs

backward :: Tape a -> Tape a
backward (Tape (x:xs) y zs) = Tape xs x (y:zs)

setCell :: a -> Tape a -> Tape a
setCell x (Tape ys z ws) = Tape ys x ws

modifyCell :: (a -> a) -> Tape a -> Tape a
modifyCell f (Tape xs y zs) = Tape xs (f y) zs

type BFTape = Tape Word8

parse :: String -> Program
parse ('+':xs) = Increment : parse xs
parse ('-':xs) = Decrement : parse xs
parse ('<':xs) = Leftward : parse xs
parse ('>':xs) = Rightward : parse xs
parse ('.':xs) = Output : parse xs
parse (',':xs) = Input : parse xs
parse ('[':xs) = parseLoop xs
parse (']':xs) = error "error: loops unbalanced, unexpected ']'"
parse (x:xs)   = parse xs
parse []       = []

parseLoop :: String -> Program
parseLoop = (\(a,b) -> (Loop . parse $ a) : parse b) . go 1
  where
    go :: Int -> String -> (String, String)
    go 1       (']':xs) = ("", xs)
    go nestLvl ('[':xs) = let (acc, rest) = go (nestLvl+1) xs in ('[':acc, rest)
    go nestLvl (']':xs) = let (acc, rest) = go (nestLvl-1) xs in (']':acc, rest)
    go nestLvl (x:xs)   = let (acc, rest) = go nestLvl xs in (x:acc, rest)
    go _       []       = error "error: loops unbalanced, unclosed '['"



interpret :: Program -> String -> String
interpret = interpret' $ initializeTape 0

interpret' :: BFTape -> Program -> String -> String
interpret' tape prog input = (\(_, output, _) -> output)
                           $ foldl1' composeSteps tapeTransforms tape input
  where
    tapeTransforms = map step prog

composeSteps :: (BFTape -> String -> (String, String, BFTape))
             -> (BFTape -> String -> (String, String, BFTape))
             -> (BFTape -> String -> (String, String, BFTape))
composeSteps f g tape input =
    let (input', out1, tape')  = f tape input
        (input'', out2, tape'') = g tape' input'
    in (input'', out1++out2, tape'')

step :: Command -> BFTape -> String -> (String, String, BFTape)
step Increment tape input = (input, "", modifyCell (+1) tape)
step Decrement tape input = (input, "", modifyCell (subtract 1) tape)
step Rightward tape input = (input, "", forward tape)
step Leftward  tape input = (input, "", backward tape)
step Output tape@(Tape _ x _) input = (input, [toEnum . fromIntegral $ x], tape)
step Input tape (i:is) = (is, "", setCell (toEnum . fromEnum $ i) tape)
step Input tape "" = ("", "", setCell 0 tape)
step (Loop comms) tape input = loop comms tape input

loop :: [Command] -> BFTape -> String -> (String, String, BFTape)
loop comms = runLoop
  where
    tapeTransforms = map step comms
    loopBody = foldl1' composeSteps tapeTransforms
    runLoop tape@(Tape _ x _) input
        | x == 0 = (input, "", tape)
        | otherwise = composeSteps loopBody runLoop tape input


main :: IO ()
main = do
    args <- getArgs
    case listToMaybe args of
        Just file -> do
            program <- readFile file
            input <- getContents
            putStr $ interpret (parse program) input
        Nothing -> putStr "usage: brainfuck file.bf\n"
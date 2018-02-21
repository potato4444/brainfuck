{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Maybe (listToMaybe)
import Data.Modular
import Data.Proxy
import Data.Word (Word64)
import GHC.TypeLits (natVal, someNatVal, SomeNat(..))
import System.Environment(getArgs)
import System.IO (isEOF, hSetBuffering, stdout, stdin, BufferMode(..))
import Text.Read (readMaybe)


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

initializeTape :: a -> Tape a
initializeTape x = Tape xs x xs
  where
    xs = x:xs

forward :: Tape a -> Tape a
forward (Tape xs y (z:zs)) = Tape (y:xs) z zs

backward :: Tape a -> Tape a
backward (Tape (x:xs) y zs) = Tape xs x (y:zs)

setCell :: a -> Tape a -> Tape a
setCell x (Tape ys z ws) = Tape ys x ws

getCell :: Tape a -> a
getCell (Tape _ x _) = x

modifyCell :: (a -> a) -> Tape a -> Tape a
modifyCell f (Tape xs y zs) = Tape xs (f y) zs

modifyCell' :: (a -> a) -> Tape a -> Tape a
modifyCell' f (Tape xs y zs) = Tape xs y' zs
  where
    !y' = f y

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



interpret :: forall a. Integral a => Proxy a -> Program -> IO ()
interpret proxy prog = evalStateT (interpret' prog) (initializeTape (0::a))

interpret' :: (Integral a) => Program -> StateT (Tape a) IO ()
interpret' = mapM_ step

step :: Integral a => Command -> StateT (Tape a) IO ()
step Increment = modify' (modifyCell' (+1))
step Decrement = modify' (modifyCell' (subtract 1))
step Rightward = modify' forward 
step Leftward  = modify' backward
step Output    = get >>= \tape ->
    lift $ putChar (toEnum . fromIntegral $ getCell tape)
step Input = lift isEOF >>= \case
    True  -> modify' (setCell 0)
    False -> do
        c <- lift getChar
        modify' (setCell (fromIntegral . fromEnum $ c))
step (Loop comms) = loop comms

loop :: Integral a => Program -> StateT (Tape a) IO ()
loop comms = do
    tape <- get
    if getCell tape == 0
        then return ()
        else interpret' comms *> loop comms


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    args <- getArgs
    case args of
        [file] -> do
            program <- readFile file
            interpret (Proxy @Integer) (parse program)
        [cellSizeString,file] -> do
            program <- readFile file
            let maybeCellSize = someNatVal =<< readMaybe @Integer cellSizeString
            case maybeCellSize of
                Nothing -> putStrLn "Cell size must be a postive integer"
                Just (SomeNat (sizeProxy :: Proxy cellSize)) ->
                    case natVal sizeProxy of
                        0 -> putStrLn "Cell size cannot be zero"
                        n | n < 2^64  -> interpret (Proxy @(Word64/cellSize)) (parse program)
                          | otherwise -> interpret (Proxy @(Integer/cellSize)) (parse program)
        _  -> putStrLn "usage: brainfuck [modulus] file.bf"
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module TM where

import Control.Monad.Writer
import Data.Digits (digits, unDigits)
import Data.Char
import Numeric

-- basic types
data HeaderOp = L | R | S deriving (Eq, Show)
data Symbol = BlankSym | StartSym | NoChange | Sym Char deriving (Eq, Show)
type Alphabet = [Symbol]
--type State = String

class (Eq s, Show s) => State s  where 
  qstart :: s
  qhalt :: s

--type States = [State]
data Tape = Tape { tape :: [Symbol], header :: Int} deriving (Eq)
type Tapes = [Tape]


instance Show Tape where
  show tp = "Tape [" ++ (showPartialTape tp) ++ "] @ " ++ (show $ header tp)

-- makes a new blank tape
mkBlankTape :: Tape
mkBlankTape = Tape ([StartSym] ++ (repeat BlankSym)) 0

mkInputTape :: [Symbol] -> Tape 
mkInputTape symbols = Tape ([StartSym] ++ symbols ++ (repeat BlankSym)) 0

readFromTape :: Tape -> Symbol
readFromTape t = (tape t) !! (header t)

writeOnTape :: Tape -> Symbol -> Tape
writeOnTape t s = case s of 
  NoChange -> t 
  s -> t {tape = pref ++ [s] ++ post}
  where h = header t
        pref = take h (tape t)
        post = drop (h+1) (tape t)
                        

moveHeaderTape :: Tape -> HeaderOp -> Tape
moveHeaderTape t op = case op
                      of
                        L -> if oldHeader == 0 then
                               error "Can't move on the left of first tape position"
                             else
                               t { header = oldHeader-1}
                        R -> t { header = oldHeader+1}
                        S -> t
  where
    oldHeader = header t
                 

type TransFunction s = s -> [Symbol] -> (s, [Symbol], [HeaderOp])
data TM s = TM { tmalphabet :: Alphabet,  tmtapes :: Tapes, tmtransfunction :: TransFunction s, tmcurstate :: s }

-- TODO: Check that states and symbols are in the set of the TM 

alphaNumericalAlphabet = map Sym (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])


mkTM :: (State s) => Int -> TransFunction s -> [Symbol] -> TM s
mkTM numWorkTapes tf input = TM alphaNumericalAlphabet  tapes tf qstart
  where tapes = [inputTape] ++ workTapes ++ [outputTape]
        workTapes = replicate numWorkTapes mkBlankTape
        outputTape = mkBlankTape
        inputTape = mkInputTape input

readFromTMTape :: TM s -> Int -> Symbol
readFromTMTape tm i = readFromTape (tmtapes tm !! i)

writeOnTapes :: TM s -> [Symbol] -> TM s
writeOnTapes tm symbols = let tapes = tail $ tmtapes tm
                              inputTape = head $ tmtapes tm
                              tapes' = zipWith  writeOnTape tapes symbols
                          in tm {tmtapes = inputTape : tapes'}

moveTapes :: TM s -> [HeaderOp] -> TM s
moveTapes tm ops = let tapes =  tmtapes tm
                       --inputTape = head $ tmtapes tm
                       tapes' = zipWith moveHeaderTape tapes ops
                   in tm {tmtapes = tapes'}
                                          

-- executes one step of the TM
step :: (State s) => TM s -> Writer [String] (TM s)
step tm = let readSymbols = map (readFromTMTape tm) [0..(length $ tmtapes tm)-1] 
              (nextState, symbolsToWrite, ops) = (tmtransfunction tm) (tmcurstate tm) readSymbols
              tm' = writeOnTapes tm symbolsToWrite
              tm'' = moveTapes tm' ops
              ret_tm = tm'' {tmcurstate = nextState}
          in  do tell ["Got TM in state " ++ (show $ tmcurstate tm) ++ " with tapes " ++ (show $ tmtapes tm)]
                 tell ["I have read symbols " ++ show readSymbols]
                 tell ["Writing symbols " ++ (show symbolsToWrite)]
                 tell ["Doing operations " ++ (show ops)]
                 tell ["Changing state to " ++ show nextState]
                 if halted ret_tm then
                   tell ["Final output is '" ++ showOutputTape ret_tm ++ "'"]
                 else 
                   tell []      
                 return ret_tm
              


              
outputTape :: TM s -> Tape
outputTape tm = last $ tmtapes tm

inputTape :: TM s -> Tape
inputTape tm = head $ tmtapes tm


showOutputTape :: TM s -> String
showOutputTape  = showPartialTape . outputTape 

showInputTape :: TM s -> String
showInputTape  = showPartialTape . inputTape 

takeNonBlankTape :: Tape -> [Symbol]
takeNonBlankTape = takeWhile (\c -> c /= BlankSym) . tape 
                       
showPartialTape :: Tape -> String
showPartialTape tp  = let output = takeNonBlankTape tp
                          symToString BlankSym = "#BLANK#"
                          symToString StartSym = "#START#"
                          symToString (Sym c) = [c]
                      in unwords $ map symToString output
                         
halted :: (Eq s, State s) => TM s -> Bool
halted tm = (tmcurstate tm) == qhalt
                         
steps :: (State s) => TM s -> Int -> Writer [String] (TM s)
steps tm n = steps' tm n 0
  where steps' ::  (State s) => TM s -> Int -> Int -> Writer [String] (TM s)
        steps' tm n acc 
          | n == acc = return tm
          | otherwise = do tm' <- step tm
                           if halted tm' then
                             return tm'
                             else  
                               do tm'' <- steps' tm' n (acc+1) 
                                  return tm''
                       


-- run a Turing Machine for a maximum number of steps printing logging information on screen  
runStepsTM :: (State s) => TM s -> Int -> IO ()
runStepsTM tm numSteps = let (tm', log) = runWriter $ steps tm numSteps
                             in do putStr $ unlines log
                                   putStrLn ("Result is: " ++ (decodeNum  $ tail $ takeNonBlankTape $ outputTape tm'))

-- Encoding/Decoding
                                   
fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base).digitToInt) digitToInt

toBase :: Int -> Int -> String
toBase base num = showIntAtBase base intToDigit num ""

fromBaseToBase :: Int -> Int -> String -> String
fromBaseToBase from to = toBase to . fromBase from
                                   
decodeNum :: [Symbol] -> String
decodeNum symbols = fromBaseToBase 2 10 (reverse strSymbols)
  where strSymbols = map (\(Sym c) -> c) symbols


encodeNum :: Int -> [Symbol]
encodeNum n = let binaryRep = showIntAtBase 2 intToDigit n ""
                  revBinaryRep = reverse binaryRep
              in map Sym revBinaryRep
                 
encodePairOfNum :: Int -> Int -> [Symbol]
encodePairOfNum x y = let xEnc = encodeNum x
                          yEnc = encodeNum y
                          maxDigits = max (length xEnc) (length yEnc)
                          fillWithZeros enc = enc ++ (replicate (maxDigits - (length enc)) (Sym '0'))
                          xEncFilled = fillWithZeros xEnc
                          yEncFilled = fillWithZeros yEnc
                      in xEncFilled ++ [StartSym] ++ yEncFilled

-- useful shortenings
--one = Sym '1'
--zero = Sym '0'
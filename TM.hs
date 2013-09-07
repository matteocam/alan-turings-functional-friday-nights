module TM where

import Control.Monad.Writer
import Data.Char
import Numeric

-- basic types
data HeaderOp = L | R | S deriving (Eq, Show)
data Symbol = BlankSym | StartSym | NoChange | Sym Char deriving (Eq, Show)
type Alphabet = [Symbol]
type State = String
type States = [State]
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
                 

type TransFunction = State -> [Symbol] -> (State, [Symbol], [HeaderOp])
data TM = TM { tmalphabet :: Alphabet, tmstates :: States, tmtapes :: Tapes, tmtransfunction :: TransFunction, tmcurstate :: State }

-- TODO: Check that states and symbols are in the set of the TM 

alphaNumericalAlphabet = map Sym (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

mkTM :: Int -> States -> TransFunction -> [Symbol] -> TM
mkTM numWorkTapes states tf input = TM alphaNumericalAlphabet states tapes tf "q_start"
  where tapes = [inputTape] ++ workTapes ++ [outputTape]
        workTapes = replicate numWorkTapes mkBlankTape
        outputTape = mkBlankTape
        inputTape = mkInputTape input

readFromTMTape :: TM -> Int -> Symbol
readFromTMTape tm i = readFromTape (tmtapes tm !! i)

writeOnTapes :: TM -> [Symbol] -> TM
writeOnTapes tm symbols = let tapes = tail $ tmtapes tm
                              inputTape = head $ tmtapes tm
                              tapes' = zipWith  writeOnTape tapes symbols
                          in tm {tmtapes = inputTape : tapes'}

moveTapes :: TM -> [HeaderOp] -> TM
moveTapes tm ops = let tapes =  tmtapes tm
                       --inputTape = head $ tmtapes tm
                       tapes' = zipWith moveHeaderTape tapes ops
                   in tm {tmtapes = tapes'}
                                          

-- executes one step of the TM
step :: TM -> Writer [String] TM
step tm = let readSymbols = map (readFromTMTape tm) [0..(length $ tmtapes tm)-1] 
              (nextState, symbolsToWrite, ops) = (tmtransfunction tm) (tmcurstate tm) readSymbols
              tm' = writeOnTapes tm symbolsToWrite
              tm'' = moveTapes tm' ops
              ret_tm = tm'' {tmcurstate = nextState}
          in  do tell ["Got TM in state " ++ (show $ tmcurstate tm) ++ " with tapes " ++ (show $ tmtapes tm)]
                 tell ["I have read symbols " ++ show readSymbols]
                 tell ["Writing symbols " ++ (show symbolsToWrite)]
                 tell ["Doing operations " ++ (show ops)]
                 tell ["Changing state to " ++ nextState]
                 if halted ret_tm then
                   tell ["Final output is '" ++ showOutputTape ret_tm ++ "'"]
                 else 
                   tell []      
                 return ret_tm
              


              
outputTape :: TM -> Tape
outputTape tm = last $ tmtapes tm

inputTape :: TM -> Tape
inputTape tm = head $ tmtapes tm


showOutputTape :: TM -> String
showOutputTape  = showPartialTape . outputTape 

showInputTape :: TM -> String
showInputTape  = showPartialTape . inputTape 

                       
showPartialTape :: Tape -> String
showPartialTape tp  = let output = takeWhile (\c -> c /= BlankSym) $ tape tp
                          symToString BlankSym = "#BLANK#"
                          symToString StartSym = "#START#"
                          symToString (Sym c) = [c]
                      in unwords $ map symToString output
                         
halted :: TM -> Bool
halted tm = (tmcurstate tm) == "q_halt"
                         
steps :: TM -> Int -> Writer [String] TM
steps tm n = steps' tm n 0
  where steps' ::  TM -> Int -> Int -> Writer [String] TM
        steps' tm n acc 
          | n == acc = return tm
          | otherwise = do tm' <- step tm
                           if halted tm' then
                             return tm'
                             else  
                               do tm'' <- steps' tm' n (acc+1) 
                                  return tm''
                       

-- XXX: only one step
runStepsTM :: TM -> Int -> IO ()
runStepsTM tm numSteps = putStr $ unlines $ snd $ runWriter $ steps tm numSteps

-- Turing Machines

-- copyTM: just copies the input (identity function)
copyTM = mkTM 
         0  -- # of working tapes
         ["q_copy"]  -- additional states
         tf -- transition function 
  where tf "q_start" _ = ("q_copy", [NoChange], [R, R]) -- start copying
        tf "q_copy" [BlankSym, _]  = ("q_halt", [NoChange], [S, S]) -- finished copying
        tf "q_copy" [s, _] = ("q_copy", [s], [R, R])
--        tf "q_halt" _  = error "TM has halted already"
--        tf st  _  = error (st ++  " is not a valid state")
        
        
-- assuming the lowest bit is on the left and the numbers have the same number of digits (filled with zeros) 
-- and the two numbers are separated by a start symbol
additionTM = mkTM
        1 -- # of Working tapes (just one: for copying the first number)
        ["q_read_copy_x", "q_rewind_x", "q_summing_no_carry", "q_summing_with_carry"] -- additional states
        tf -- transition function
  where tf "q_start" _ = ("q_read_copy_x", [NoChange, NoChange], [R, R, R])
        tf "q_read_copy_x" (StartSym:_) = ("q_rewind_x", [NoChange, NoChange], [R, L, S]) -- finished to copy x
        tf "q_read_copy_x" (s:_) = ("q_read_copy_x", [s, NoChange], [R, R, S]) -- copy x digits on scratch pad
        tf "q_rewind_x" [_, StartSym, _] = ("q_summing_no_carry", [NoChange, NoChange], [S, R, S]) -- finished rewinding
        tf "q_rewind_x" [_, s, _] = ("q_rewind_x", [NoChange, NoChange], [S, L, S]) -- still rewinding
        tf "q_summing_no_carry" [BlankSym, BlankSym, _] = ("q_halt", [NoChange, NoChange], [S, S, S]) -- finished summing (no carry)
        tf "q_summing_no_carry" [Sym y_digit, Sym x_digit, _ ] = 
          case (y_digit, x_digit) of
            ('0', '0') -> ("q_summing_no_carry", [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further w/o carry
            ('1', '0') -> ("q_summing_no_carry", [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further w/o carry
            ('0', '1') -> ("q_summing_no_carry", [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further w/o carry
            ('1', '1') -> ("q_summing_with_carry", [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further with carry
        tf "q_summing_with_carry" [BlankSym, BlankSym, _] = ("q_summing_no_carry", [NoChange, Sym '1'], [S, S, S]) -- finished summing (with carry)
        tf "q_summing_with_carry" [Sym y_digit, Sym x_digit, _ ] = 
          case (y_digit, x_digit) of
            ('0', '0') -> ("q_summing_no_carry", [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further w/o carry
            ('1', '0') -> ("q_summing_with_carry", [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further with carry
            ('0', '1') -> ("q_summing_with_carry", [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further with carry
            ('1', '1') -> ("q_summing_with_carry", [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further with carry
        
        
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

maxSteps = 100

runAdditionTM :: Int -> Int -> IO ()
runAdditionTM x y = runStepsTM (additionTM enc) maxSteps
  where enc = encodePairOfNum x y

--alltms = iterate step testtm
module Machines where

import Data.Char
import Numeric


import TM



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


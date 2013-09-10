module MultiplicationTM where
import TM

data MultiplicationState = QStart 
                         | QReadCopyX
                         | QRewindX
                         | QInitOutput
                         | QCheckIfDone
                         | QHalt deriving (Eq, Show)
                     
instance State MultiplicationState where
  qstart = QStart
  qhalt = QHalt
  
-- assuming the lowest bit is on the left and the numbers have the same number of digits (filled with zeros) 
-- and the two numbers are separated by a start symbol
multiplicationTM = mkTM
        1 -- # of Working tapes (just one: for copying the first number)
        [] -- additional states
        tf -- transition function
  where tf QStart _  = 
          (QReadCopyX, [NoChange, NoChange], [R, R, R])
          
        tf QReadCopyX (StartSym:_) 
          = (QRewindX, [NoChange, NoChange], [R, L, S]) -- finished to copy x
        tf QReadCopyX (s:_)           
          = (QReadCopyX, [s, NoChange], [R, R, S]) -- copy x digits on scratch pad
            
        tf QRewindX [_, StartSym, _] 
          = (QInitOutput, [NoChange, NoChange], [S, R, S]) -- finished rewinding
        tf QRewindX [_, s, _] 
          = (QRewindX, [NoChange, NoChange], [S, L, S]) -- still rewinding
            
        tf QInitOutput _ = 
          (QCheckIfDone, [NoChange, Sym '0'], [S, S, S]) -- write 0 on the output tape and
                                                         -- start the multiplication loop
          
        tf QSummingNoCarry [BlankSym, BlankSym, _] = (QHalt, [NoChange, NoChange], [S, S, S]) -- finished summing (no carry)
        tf QSummingNoCarry [Sym y_digit, Sym x_digit, _ ] = 
          case (y_digit, x_digit) of
            ('0', '0') -> (QSummingNoCarry, [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further w/o carry
            ('1', '0') -> (QSummingNoCarry, [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further w/o carry
            ('0', '1') -> (QSummingNoCarry, [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further w/o carry
            ('1', '1') -> (QSummingWithCarry, [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further with carry
        tf QSummingWithCarry [BlankSym, BlankSym, _] = (QSummingNoCarry, [NoChange, Sym '1'], [S, S, S]) -- finished summing (with carry)
        tf QSummingWithCarry [Sym y_digit, Sym x_digit, _ ] = 
          case (y_digit, x_digit) of
            ('0', '0') -> (QSummingNoCarry, [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further w/o carry
            ('1', '0') -> (QSummingWithCarry, [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further with carry
            ('0', '1') -> (QSummingWithCarry, [NoChange, Sym '0'], [R, R, R]) -- write 0 and go further with carry
            ('1', '1') -> (QSummingWithCarry, [NoChange, Sym '1'], [R, R, R]) -- write 1 and go further with 
        
        

maxSteps = 100

runMultiplicationTM :: Int -> Int -> IO ()
runMultiplicationTM x y = runStepsTM (multiplicationTM enc) maxSteps
  where enc = encodePairOfNum x y

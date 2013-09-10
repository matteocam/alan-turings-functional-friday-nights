module MultiplicationTM where
import TM

data MultiplicationState = QStart 
                         | QReadCopyX
                         | QRewindX
                         | QInitOutput
                         | QDecrementX
                         | QFlipDigits
                         | QReachLeftMost1
                         | QReachLeftMost1FirstTime
                         | QIterateSum
                         | QRewindY  
                         | QRewindOutput
                         | QSummingNoCarry
                         | QSummingWithCarry
                         | QHalt deriving (Eq, Show)
                     
instance State MultiplicationState where
  qstart = QStart
  qhalt = QHalt
  
-- assuming the lowest bit is on the left and the numbers have the same number of digits (filled with zeros) 
-- and the two numbers are separated by a start symbol
multiplicationTM = mkTM
        1 -- # of Working tapes (just Sym '1': for copying the first number)
        [] -- additional states
        tf -- transition function
  where tf QStart _  = 
          (QReadCopyX, [NoChange, NoChange], [R, R, R])
          
        tf QReadCopyX (StartSym:_) 
          = (QRewindX, [NoChange, NoChange], [R, L, S]) -- finished to copy x
        tf QReadCopyX (s:_)           
          = (QReadCopyX, [s, NoChange], [R, R, S]) -- copy x digits on scratch pad
            
        tf QRewindX [_, StartSym, _] 
          = (QReachLeftMost1FirstTime, [NoChange, NoChange], [S, R, S]) -- finished rewinding
        tf QRewindX [_, s, _] 
          = (QRewindX, [NoChange, NoChange], [S, L, S]) -- still rewinding
            
        -- Invariant: the pointer in the working pad is always on the lowest (leftmost) bit set to 1
        --            before iterating an addition
        --tf QInitOutput _ 
        --  = (QDecrementX, [NoChange, Sym '0'], [S, S, S]) -- write 0 on the output tape and
                                                         -- start the multiplication loop
          
        tf QDecrementX [_, Sym '0', _] 
          = (QHalt, [NoChange, NoChange], [S, S, S]) -- we are done
        tf QDecrementX [_, Sym '1', _]
          = (QFlipDigits, [Sym '0', NoChange], [S, L, S])
            
        tf QFlipDigits [_,  StartSym, _]
          = (QReachLeftMost1, [ NoChange, NoChange], [S, R, S]) -- flipped all the digits, now
                                                                -- keep the invariant
        tf QFlipDigits [_, Sym '1', _]
          = (QFlipDigits, [Sym '0', NoChange], [S, L, S])
        tf QFlipDigits [_, Sym '0', _]
          = (QFlipDigits, [Sym '1', NoChange], [S, L, S])
            
        tf QReachLeftMost1 [_,  StartSym, _] -- there is no 1, it's last iteration
          = (QIterateSum, [ NoChange, NoChange], [S, R, S])
        tf QReachLeftMost1 [_,  Sym '0', _]
          = (QReachLeftMost1, [ NoChange, NoChange], [S, L, S]) -- go further
        tf QReachLeftMost1 [_,  Sym '1', _]
          = (QIterateSum, [ NoChange, NoChange], [S, S, S]) -- found 1, stop and start adding
            
        tf QReachLeftMost1FirstTime [_,  StartSym, _] -- X is zero
          = (qhalt, [ NoChange, Sym '0'], [S, S, S])
        tf QReachLeftMost1FirstTime [_,  Sym '0', _]
          = (QReachLeftMost1FirstTime, [ NoChange, NoChange], [S, L, S]) -- go further
        tf QReachLeftMost1FirstTime [_,  Sym '1', _]
          = (QDecrementX, [ NoChange, NoChange], [S, S, S]) -- found 1, stop and start adding
          
        tf QIterateSum [_, _, BlankSym ]
          = (QInitOutput, [NoChange, NoChange], [S, S, S]) -- first time you sum, just copy Y
        tf QIterateSum [_, _, _ ]
          = (QSummingNoCarry, [NoChange, NoChange], [S, S, S]) -- start actual summing
        
        tf QInitOutput [BlankSym, _, _ ]
          = (QRewindY, [NoChange, NoChange], [L, S, S]) -- finished copying, start rewinding Y
        tf QInitOutput [y_digit, _, _ ]
          = (QInitOutput, [NoChange, y_digit], [R, S, R]) -- keep copying Y
        
        tf QRewindY [StartSym, _, _] 
          = (QRewindOutput, [NoChange, NoChange], [R, S, L]) -- stop rewinding Y, start rewinding output
        tf QRewindY [_, _, _] 
          = (QRewindY, [NoChange, NoChange], [L, S, S]) -- still rewinding

        tf QRewindOutput [_, _, StartSym] 
          = (QDecrementX, [NoChange, NoChange], [S, S, R]) -- stop rewinding output, start new iteration
        tf QRewindOutput [_, _, _] 
          = (QRewindOutput, [NoChange, NoChange], [S, S, L]) -- still rewinding

        
        tf QSummingNoCarry [BlankSym, _ , _] 
          = (QRewindY, [NoChange, NoChange], [L, S, S]) -- finished summing rewind 'Y'
        tf QSummingNoCarry [Sym y_digit, _, Sym partial_result_digit ] 
          = case (y_digit, partial_result_digit) of
            ('0', '0') -> (QSummingNoCarry, [NoChange, Sym '0'], [R, S, R]) -- write 0 and go further w/o carry
            ('1', '0') -> (QSummingNoCarry, [NoChange, Sym '1'], [R, S, R]) -- write 1 and go further w/o carry
            ('0', '1') -> (QSummingNoCarry, [NoChange, Sym '1'], [R, S, R]) -- write 1 and go further w/o carry
            ('1', '1') -> (QSummingWithCarry, [NoChange, Sym '0'], [R, S, R]) -- write 0 and go further with carry
        tf QSummingWithCarry [BlankSym, _, Srosarioym '1'] 
          = (QSummingWithCarry, [NoChange, Sym '0'], [S, S, R]) -- finished summing (propagate the carry)
        tf QSummingWithCarry [BlankSym, _, Sym '0'] 
          = (QRewindY, [NoChange, Sym '1'], [S, S, S]) -- finished summing (write the carry)
        tf QSummingWithCarry [Sym y_digit, _, Sym partial_result_digit ] = 
          case (y_digit, partial_result_digit) of
            ('0', '0') -> (QSummingNoCarry, [NoChange, Sym '1'], [R, S, R]) -- write 1 and go further w/o carry
            ('1', '0') -> (QSummingWithCarry, [NoChange, Sym '0'], [R, S, R]) -- write 0 and go further with carry
            ('0', '1') -> (QSummingWithCarry, [NoChange, Sym '0'], [R, S, R]) -- write 0 and go further with carry
            ('1', '1') -> (QSummingWithCarry, [NoChange, Sym '1'], [R, S, R]) -- write 1 and go further with 
        
        

maxSteps = 100

runMultiplicationTM :: Int -> Int -> IO ()
runMultiplicationTM x y = runStepsTM (multiplicationTM enc) maxSteps
  where enc = encodePairOfNum x y

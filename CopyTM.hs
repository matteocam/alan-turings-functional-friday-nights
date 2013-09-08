module CopyTM where

import TM


-- Turing Machines

data CopyTMState = QStart | QHalt | QCopy deriving (Eq, Show)
instance State CopyTMState where
  qstart = QStart
  qhalt = QHalt

-- copyTM: just copies the input (identity function)

copyTM :: [Symbol] -> TM CopyTMState
copyTM = mkTM 
         0  -- # of working tapes
         --["q_copy"]  -- additional states
         undefined -- additional states
         tf -- transition function 
  where tf QStart _ = (QCopy, [NoChange], [R, R]) -- start copying
        tf QHalt [BlankSym, _]  = (QHalt, [NoChange], [S, S]) -- finished copying
        tf QCopy [s, _] = (QCopy, [s], [R, R])
--        tf "q_halt" _  = error "TM has halted already"
--        tf st  _  = error (st ++  " is not a valid state")
        

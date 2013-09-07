alan-turings-functional-friday-nights
=====================================

An Haskell library for writing Turing Machines

## Introduction
If you have never heard of Turing Machines good: it probably means you went out often enough during your life.
However, if you have ever happened to have to write Turing Machines for a whatever reason
and felt like going out the same night, you probably learnt what "impossibility" is about (if you do not have a clear idea yet, look for a semantic path 
between the words "agreement" and "condominium meeting" or keep hanging out Turing Machines and ask them if
they ever halt). 
Turing Machines (TMs) are a formalism for computation. Given their complexity, even easy tasks and algorithms can be daunting.
At least two problems emerge:
* TMs are not easily testable
* TMs are described by long-winded definitions of transition functions

This library provides an Haskell DSL for defining Turing Machines. Some aspects of the Haskell programming language
make writing transition functions less comparable to drinking hemlock and having a library for it
make protyping and testing easier.

## Quick start

* Download the zip or clone the git repository
* Move to the directory of the project

Then fire up `ghci`:

    $ ghci
    Prelude> :l Machines
    *Machines> runAdditionTM 11 4
    Got TM in state "q_start" with tapes [Tape [#START# 1 1 0 1 #START# 0 0 1 0] @ 0,Tape [#START#] @ 0,Tape [#START#] @ 0]
    I have read symbols [StartSym,StartSym,StartSym]
    Writing symbols [NoChange,NoChange]
    Doing operations [R,R,R]
    Changing state to q_read_copy_x
    Got TM in state "q_read_copy_x" with tapes [Tape [#START# 1 1 0 1 #START# 0 0 1 0] @ 1,Tape [#START#] @ 1,Tape [#START#] @ 1]
    I have read symbols [Sym '1',BlankSym,BlankSym]
    Writing symbols [Sym '1',NoChange]
    Doing operations [R,R,S]
    ... (more output) ...
    Got TM in state "q_summing_no_carry" with tapes [Tape [#START# 1 1 0 1 #START# 0 0 1 0] @ 10,Tape [#START# 1 1 0 1] @ 5,Tape [#START# 1 1 1 1] @ 5]
    I have read symbols [BlankSym,BlankSym,BlankSym]
    Writing symbols [NoChange,NoChange]
    Doing operations [S,S,S]
    Changing state to q_halt
    Final output is '#START# 1 1 1 1'
    Result is: 15



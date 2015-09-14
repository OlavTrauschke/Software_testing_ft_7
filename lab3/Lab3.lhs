Lab 3 Software Specification and Testing

> module Lab3 where
> 
> import Data.List
> import System.Random

The lecture notes of this week discuss the notions of satisfiability, tautology, contradiction, logical entailment and logical equivalence for formulas of propositional logic.

The lecture notes give a definition of satisfiable, for objects of type Form.

Your task is to give definitions of:

 contradiction :: Form -> Bool

 tautology :: Form -> Bool

 -- | logical entailment 
 entails :: Form -> Form -> Bool

 -- | logical equivalence
 equiv :: Form -> Form -> Bool
Use a module that imports Lecture3.lhs. Check that your definitions are correct.

Deliverables: implementation, description of your method of checking the definitions, indication of time spent.

The lecture notes of this week define a function parse for parsing propositional formulas. Test this function. You can use any test method you want.

Deliverables: test report describing the test method used and the outcome of the test, indication of time spent.

The lecture notes of this week discuss the conversion of Boolean formulas (formulas of propositional logic) into CNF form. The lecture notes also give a definition of a Haskell datatype for formulas of propositional logic, using lists for conjunctions and disjunctions. Your task is to write a Haskell program for converting formulas into CNF.

Deliverables: conversion program with documentation, indication of time spent.

Write a formula generator for random testing of properties of propositional logic, or teach yourself enough QuickCheck to use random QuickCheck testing of formulas.

Use your random testing method to test the correctness of the conversion program from the previous exercise. Formulate a number of relevant properties to test, and carry out the tests, either with your own random formula generator or with QuickCheck.

Deliverables: generator for formulas, sequence of test properties, test report, indication of time spent.

Bonus
In SAT solving, one common technique is resolution style theorem proving. For that, it is usual to represent a formula in CNF as a list of clauses, were a clause is a list of literals, and where a literal is represented as an integer, with negative sign indicating negation. Here are the appropriate type declarations.

      type Clause  = [Int]
      type Clauses = [Clause]
Clauses should be read disjunctively, and clause lists conjunctively. So 5 represents the atom p5, −5 represents the literal ¬p5, the clause [5,−6] represents p5∨¬p6, and the clause list [[4],[5,−6]] represents the formula p4∧(p5∨¬p6).

Write a program for converting formulas to clause form. You may assume that your formulas are already in CNF. Here is the appropriate declaration:

 cnf2cls :: Form -> Clauses
If you combine your conversion function from an earlier exercise with cnf2cls you have a function that can convert any formula to clause form.

Use automated testing to check whether your translation is correct, employing some appropriate properties to check.

Deliverables: Conversion program, test generator, test properties, documentation of the automated testing process. Also, give an indication of time spent.

Submission deadline is Sunday evening, September 20th, at midnight.
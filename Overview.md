
Overview
========

   This Lesson covers some of the basic syntax of idris.
   While we go we will also introduce idris conecepts as we go.
   Most of these concepts will be elaborated on in other chapters.
   This chapter tries to give a motivational overview of haskell
   language syntax and concepts.

   This tutorial is intended to be viewed in one of the idris editors.
   There exists support for idris development for 
    * [emacs](https://github.com/idris-hackers/idris-mode) 
    * [vim](https://github.com/idris-hackers/idris-vim)
    * [sublime](https://github.com/laughedelic/sublime-idris)
     
   I have used the emacs mode while writing this 
   and will give examples for idris's interactive editing capabilities
   using the emacs key bindings.

   So let's start..
                                                                                          
```haskell
module Tutorial.Basics

```
   An haskell source file usually starts with a module definition much like
   you'd expect.
   The module definition is optional and there can be at most one per file.

                                                                                           
```haskell
import Data.SortedMap                      

```
   A module can import other modules. haskell will also silently import
   a module called the *prelude* that contains a host of usefull data types
   and functions. Sometimes you may want to use names, that the prelude
   also uses which may result in a name clash.
   In this case you can rename the import                                                  
```haskell
import Prelude.List as L

``` 
   haskell has all the usual data types built in, like Integers, Reals, String, Booleans, etc.
   I won't go into detail about this, since you can easily look them all up.

   Instead, we start off with something every developer will need: defining new data types.
   We can define a new data type like this:                                                
```haskell
data Day = monday | tuesday | wednesday | thursday | friday | saturday | sunday

``` This defines a new data type as the sum or union of all days.
   It is kind of like a enumeration in other programming languages.
   An instance of this data type may be a monday __or__ a tuesday __or__ any
   of the other days.

   There is another syntax for defining data types with dependent types, that is
   types that depend on values but we will come to this later

   Now with our glorius data type defined we can write function that operate on days      
```haskell

||| computes the next week day
nextWeekday : Day       -> Day
nextWeekday   monday    =  tuesday
nextWeekday   tuesday   =  wednesday
nextWeekday   wednesday =  thursday
nextWeekday   thursday  =  friday
nextWeekday   friday    =  monday
nextWeekday   saturday  =  monday
nextWeekday   sunday    =  monday

```
   This function is defined as function that takes a Day and returns another Day.
   The triple bar __|||__ is a comment intended for api documentation like
   haddock in haskell or javadocs. 

   We can give the implementation for each case of the data type explicitly.
   We could also have used other patterns to match the possible arguments
   for a function that expects a day. This is an example of _pattern matching_.

   Pattern matching is exploited heavyly in haskell.
   Since is is known what patterns are possible you can even let haskell
   generate all the match clauses when using the interactive editing mode.
   The interactive editing mode is supported (to my knowledge) by the emacs mode
   and vim mode and is also visible in the repl.

   If you have a type signature using a data type like this:                            
```haskell

defineMe : Day -> Day

```
   you can (in emacs) type in C-c C-s with the cursor on the type definition
   and haskell will supply a template definition like this

       defineMe : Day -> Day
       defineMe x = ?defineMe_rhs

   haskell has choosen a default name for the variable containing the argument _x_
   and this strange `?defineMe_rhs`. It stands (unsurprisingly) for the right hand side
   of `defineMe`. A variable beginning with a __?__ is called a _metavariable_ or a _hole_.
   Metavariables stand for something to be done.
   In this case the metavariable `?defineMe_rhs`stands for a real implementation
   of the function `defineMe`. Note that unlike the previous function the argument _x_
   is a completely generic instance of `Day`. In the previous function we
   had a line for each different member of the data type `Day`.
   We could type (or copy) all cases to this function, but haskell can help
   you also with this: since haskell knows by the definition of the data type what
   cases can occur you can ask haskell to provide the missing cases.
   In the emacs mode you can do this by moving the cursor over the `x` and typing
   C-c C-c. This splits the variable `x`into all the possible cases. The output
   looks like this:

       defineMe : Day -> Day
       defineMe monday = ?defineMe_rhs_1
       defineMe tuesday = ?defineMe_rhs_2
       defineMe wednesday = ?defineMe_rhs_3
       defineMe thursday = ?defineMe_rhs_4
       defineMe friday = ?defineMe_rhs_5
       defineMe saturday = ?defineMe_rhs_6
       defineMe sunday = ?defineMe_rhs_7

   We now have seven different cases, all unimplemented, and thus also seven metavariables.

   We will now leave this uninspiring data type an introduce another data type with
   even less cases: Booleans!
   Of course haskell comes already equipped with booleans, but we will roll out our
   own private version of Booleans, to give glimpse of proofs.                            
```haskell

-- A Boolean is a data type with two cases: truth and falsehood
data Boolean = T | F

-- the _real_ data type in haskell is called `Bool` so this should not cause any name clashes
-- (you can look up this real `Bool` in the module `Prelude.Bool`)

```
   We can now define the usual logical connectives `and`,`or` and `not`                   
                                                                                          
```haskell
and : Boolean -> Boolean -> Boolean
and T T = T
and _ _ = F

```
   So `and` returns `T` if and only if both arguments are also `T`.

   There are some things to notice if you are new to this syntax:
    * functions consist of a _type declaration_ and one more
      _patterns_ giving the implementation 
    * you can read the type of this function as something that takes 
      two `Booleans` and returns a `Boolean.
      But what it really says is: `and` is a function that takes a Boolean and 
      returns a function that takes a Boolean and returns a function. 
      So if you supply less that the maximal number of arguments
      you get back a function and not a Boolean.
    * the underscore `_` is a special pattern that matches everything. 
      It basically says to match anything
    * patterns are processed in the same sequence as they are written
 
   The other two connectives can be defined in a similar vein:      
                                                                                          
```haskell
or : Boolean -> Boolean -> Boolean
or F F = T
or _ _ = T

-- well you can try to implement this yourself. Remember C-c C-s creates
-- a template implementation (with the cursor on `not`) 
-- and C-c C-c splits out all possible cases with the cursor on the `x`.
not : Boolean -> Boolean 

```
   And now we come to the first proof: Will prove that `false` or `true` is 
   `true`. We didn't write that down explicitly in the definition of `or`
   so it will be nice to have it proved.
   
   Things that need to be proved are generally called _propositions_.
   Our proposition looks like this:
                                                                                          
```haskell
prfOrT : F `or` T = T           -- False or True is True 
```
   This looks a bit like a function declarations except that it 
   returns an equation, namely: `F or T = T`
   
   And that is because a proposition is the _same_ as function. 

   (Aside: you can use infix notation with a function name by surrounding 
    the function name with backticks.)
    
    If the type declaration is the proposition what will be the proof?
    Naturally it will be the implementation of that function
    To put it shortly proposition are types and proofs are programs.
    
    With our trivial proposition the proof looks like this:
                                                                                          
```haskell
prfOrT = refl

``` 
   It basically says that the left hand side and the right hand side of the 
   equality proposition are equal. `refl` is short for `reflexivity`. 
   (At this point, you should look up the type of `refl` for yourself (using C-c C-t). 
   That much is easy to see since the left hand side
   equal to `T` by the definition of `or`
   
   I now have used the word equal with to slighty different meanings that
   will turn out make a world off difference. There was a notion of _equal by definition_
   and of equal in the context of an equality proposition.
   We will have more to say on this later. For now just keep in mind that 
   we are sitting on an iceberg.
  
   But first some more examples. Our first proof holds absolutely. That is, there
   is no assumption or hypothesis involved.
   So let's now do a proof with an assumption.
                                                                                          
```haskell
prAfndF : (b: Boolean) -> b `and` F = F

``` 
   This proposition says, that __for all__ `Boolean` `b` the conjunction (think: _and_) of 
   this `b` with `F` is `F`  (which also follows directly from the definition of `and').
   This notion of something _for all_ and then an equational something which holds
   is something that might be familiar from math.
   
   You can do proof for yourself (using C-c C-s to create a template implementation
   and then C-c C-a with the cursor on the metavariable to ask idris to solve it 
   for you.
   
-----------------------------------------------------------------------------

Natural Numbers
---------------

   Now we come to a time honoured example: The Natural Numbers
   
   In this formulation the natural numbers will be a recursively generated data type.
   We will give a definition of the natural numbers like this:
    
   1. `zero` is a natural number
   2. the _successor_ of a natural number is also a natural number
   
   we can formulate this definition in idris like this:
                                                                                          
```haskell
data N = Zero | Succ N

-- this is a directive that gives idris a hint what are good
-- names for this type. This is nice when idrid generates code for us.
%name N n, m, i, j, k

``` 
   the nice thing about these kind of definition is, that if we want to
   formulate functions on the natural numbers (like addition, subtraction, you name it)
   we only need to define how a function operates on `Zero` and how it operates on
   the successor `Succ` and we're done.
   
   For example, addition can be defined like this
                                                                                          
```haskell
||| adds two natural nummbers
add : N -> N -> N
add Zero     m = m                -- 1. Zero + some number is the same number
add (Succ k) m = Succ (add k m)  -- 2. Any non Zero number needs recursion

```
   The second pattern off `add` peels back recursively all the
   `Succ`s of the first number an prepends them to the second number.
   The addition of `2 + 1` would look like this:
   
      add (Succ (Succ Zero)) (Succ Zero)  => (by 2.)
      Succ (add (Succ' Zero) (Succ Zero)) => (by 2.)
      Succ (Succ (add Zero   (Succ Zero)) => (by 1.)
      Succ (Succ              (Succ Zero))
   
   and that's 3.
   
   So far addition works. 
   
   But even if it works, it seems a very cumbersome way to represent numbers!
   I mean, how would you represent a moderately large number like 1000? It would
   be a loooooong line of
   
       Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ ..... (Succ Z) ....)))))))) 
   
   Well, we will address this question, but first we should test that
   our `add` functio really behaves like a decent addition.
   And by _test_ I really mean _prove_
   
   Some of the properties of an addition are:
   * `0 + n = n` 
   * `n + 0 = n`
   * `n + m = m + n`
      
   how hard can it be to prove it?
                                                                                          
```haskell

prfLeftAddZeroNeutral : (add Zero n) = n
prfLeftAddZeroNeutral = refl               

``` 
   Aha, its refl again.
   But what about this:
                                                                                          
```haskell
prfRightAddZeroNeutral : (add n Zero) = n
prfRightAddZeroNeutral = ?prfRightAddZeroNeutral_rhs3

```
   Now we're stuck. We can ask idris to solve this metavariable for
   us (using C-c C-a) but it just does not work.
   
   How did this happen? We can try to look what idris expects by looking
   at the type of the metavariable (using C-c C-t)
   and idris gives us something like this
   
         n : N
       --------------------------------------
       prfRightAddZeroNeutral_rhs3 : add n Zero = n

   this tells us we have a proof with an hypothesis (an unknown variable)
   that idris shows us above the line.
   
   It seems idris does not know what to do with this generic number.
   Why did it work for the proposition `0 + n = n` and not for `n + 0 = 0`
   
   The answer lies in the _definition_ of `add`.
   We defined `add` so that when the first argument is `Zero`
   the answer would simple be the other argument.
   And this difference is crucial! 
   If idris knows from the definition of a some term that something is equal
   it can use this definition to compute expressions.
   
   This kind of equality - equality by definition - is called _definitional equality_ 
   or _judgmental equality_.
   
   The other kind of equality arises whenever we need to prove that
   an equality holds.
   That is there needs to be an instance of the equality type around to _witness_
   the equality.
   
   To wrap up: for `0 + n = n` idris was able to show the equality by automatically
   using definitional equalities; for `n + 0 = 0` we need to supply a _witness_ 
   (aka _proof_).
   
   So how would we go about proving `n + 0 = 0` for a completely generic `n`?
   Well the first idea is: we really know what forms `n` can have from the definition
   of N - it can either be `Zero` or it can be the successor of another natural 
   number.
   So we would like to split the proof in two cases.
   The second idea is: suppose we know a proposition holds for the successor 
   if it holds for the number itself __and__ it holds for `Zero` then
   we know it holds for all natural numbers (members of the type `N`).
   
   This is exactly the principle of mathematical induction.
   
   The next task is to make a proof for the proposition `n + 0 = 0` using
   and induction argument.
   To use induction arguments on a data type we need to annotate it
   with another directive `%elim`:
                                                                                    
```haskell
-- I have to repeat some of the definition so we don't have a conflict
-- with our previous definition
-- It is idiomatic to append a prime symbol to signify a derivation of some definition
%elim data N' = Zero' | Succ' N'
%name N' n, m, i, j, k

add' : N' -> N' -> N'
add' Zero'     m = m                 
add' (Succ' k) m = Succ' (add' k m)  

prfRightAddZeroNeutral' : (n:N') -> (add' n Zero') = n
-- this time I have included the unknown n' as an explicit parameter
-- in the previous proof we did not mention n, so idris included
-- it as an _implicit_ parameter (this will come up again later) 
-- I had idris generate the intial match clause and already split
-- the cases of n
prfRightAddZeroNeutral' Zero'     = ?prfRightAddZeroNeutral'_rhs_1
prfRightAddZeroNeutral' (Succ' n) = ?prfRightAddZeroNeutral'_rhs_2

``` 
   We will now prove these two cases. Since the first case corresponds
   to `0 + 0 = 0` we can let idris solve it (C-c C-a). 
   
   The second case is of course the difficult one.
   
   haskell lets you prove propositions using a so called _proof script_
   A _proof script_ is a DSL for manipulating proof using transformations
   of the proof state that are called _tactics_.

   You can do that in your editor if it supports interactive proving,
   but I will show the steps of the proof in the repl.
   You should do this yourself so you can see the dynamics of the interactive
   proof solving.
   
   We start by telling idris we want to prove the metavariable corresponding
   to the second case. By the way, the idris repl has tab completion  
   
       *Basics> :p prfRightAddZeroNeutral'_rhs_2


       ----------                 Goal:                  ----------
       {hole0} : (n : N') -> Succ' (add' n Zero') = Succ' n
       
   haskell lists our proof obligations, which has exactly the same type
   as the corresponding metavariable. In the context of the prover the metavariables
   however are called _goals_.
   Our goal is a function type. We can _introduce_ the argument of the function
   type as an assumption by using the `intro` tactic. 
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> intro n
       ----------              Other goals:              ----------
       {hole0}
       ----------              Assumptions:              ----------
        n : N'
       ----------                 Goal:                  ----------
       {hole1} : Succ' (add' n Zero') = Succ' n
       
   This introduced a new assumption, and the remaining goal is reduced to 
   the result type of our original hole.
   We now try to use induction on `n`
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> induction n
       ----------              Other goals:              ----------
       elim_Succ'0,{hole0}
       ----------              Assumptions:              ----------
        n : N'
       ----------                 Goal:                  ----------
       elim_Zero'0 : Succ' (add' Zero' Zero') = Succ' Zero'
       
   This splits the curent goal into two new ones by splitting the assumption
   into its known forms. Only one goal remains in focus, so that any tactic
   will only be used on this current goal.
   This current goal can be simplified since `0 + 0` is known by definition to be `0`.
   We can tell the prover to simplify the goal by using the tactic `compute`:
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> compute
       ----------              Other goals:              ----------
       elim_Succ'0,{hole0}
       ----------              Assumptions:              ----------
        n : N'
       ----------                 Goal:                  ----------
       elim_Zero'0 : Succ' Zero' = Succ' Zero'
       
   The remaining obligation has exactly the form of `refl` and is
   rather trivial. Therefore we use the tactiv `trivial`:
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> trivial
       ----------              Other goals:              ----------
       {hole0}
       ----------              Assumptions:              ----------
        n : N'
       ----------                 Goal:                  ----------
       elim_Succ'0 : (n__0 : N') ->
                     (Succ' (add' n__0 Zero') = Succ' n__0) ->
                     Succ' (add' (Succ' n__0) Zero') = Succ' (Succ' n__0)
                     
   Woohoo, the first goal is finished an the other goal comes into focus.
   This one looks a more complicated, by we can see that is of function type.
   So we can move all the arguments to this function up to the assumption
   using the `intros` tactic. This tactic is the bulk variant of the `intro`
   tactic. It moves all argumentes to a function type up and assigns names
   as it sees fit:
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> intros
       ----------              Other goals:              ----------
       {hole4},elim_Succ'0,{hole0}
       ----------              Assumptions:              ----------
        n : N'
        n__0 : N'
        ihn__0 : Succ' (add' n__0 Zero') = Succ' n__0
       ----------                 Goal:                  ----------
       {hole5} : Succ' (add' (Succ' n__0) Zero') = Succ' (Succ' n__0)
       
   We can try to simplify this goal using `compute` again:
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> compute
       ----------              Other goals:              ----------
       {hole4},elim_Succ'0,{hole0}
       ----------              Assumptions:              ----------
        n : N'
        n__0 : N'
        ihn__0 : Succ' (add' n__0 Zero') = Succ' n__0
       ----------                 Goal:                  ----------
       {hole5} : Succ' (Succ' (add' n__0 Zero')) = Succ' (Succ' n__0)
       
   Looking carefully at the current hole, we can see that the left hand side
   contains a copy of the left hand side of the induction hypothesis `ihn__0`.
   We can therefore rewrite our goal using the induction hypothesis using
   the tactic `rewrite`. This will replace the matching terms of the left hand side
   of the hypothesis in the goal with the right hand side of the hypothesis:
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> rewrite ihn__0
       ----------              Other goals:              ----------
       {hole5},{hole4},elim_Succ'0,{hole0}
       ----------              Assumptions:              ----------
        n : N'
        n__0 : N'
        ihn__0 : Succ' (add' n__0 Zero') = Succ' n__0
       ----------                 Goal:                  ----------
       {hole6} : Succ' (Succ' (add' n__0 Zero')) =
                 Succ' (Succ' (add' n__0 Zero'))
                 
   That again looks trivial:
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> trivial
       prfRightAddZeroNeutral'_rhs_2: No more goals.
       
   When there are no more goals, we can conclude the proof using the `qed` tactic:
   
       -Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2> qed
       Proof completed!
       Tutorial.Basics.prfRightAddZeroNeutral'_rhs_2 = proof
         intro n
         induction n
         compute
         trivial
         intros
         compute
         rewrite ihn__0
         trivial
   
   Cool! We are done. haskell outputs the whole proof script for us 
   We could now use the repl command _:addproof_ to let idris add this proof
   for us in our module.
   
   haskell ships with an official type of natural number called `Nat` , that lives in
   `Prelude.Nat`, so you have it available automatically.
   In real idris the cases are called `Z` and `S` instead of `Zero` and `Succ`.

   You may wonder what happens with these incredibly nested S (S (S .... Z))) natural
   numbers during runtime. haskell is smart enough to use "normal" integers during runtime.
   In fact one of the nice things about static typing is, that all the type information
   can go away when the type checker is finished.   
         
   In the next section we look at a familiar data type: Lists.
   
-----------------------------------------------------------------------------
  
Lists and other Lists
---------------------

   We start of with a straight forward definition of Lists.
   (By List I mean a singly linked list)
   Our List is given by this definition:
                                                                                    
```haskell
%elim data ConsList a = 
  EmptyList | 
  Cons a (ConsList a)

%name ConsList xs, ys, zs

``` 
   Again we disambiguate our names from the `List`in the idris prelude. We named it
   `ConsList`, but I will refer to it as `List` to keep things short.

   A List is given by two cases
   1. An empty list is a `List`
   2. given a list and a thing that fits in the list we can make a new list
   
   Remember, that we had a similar thing with the natural numbers: We had a `Zero` 
   (the base case) and a way to make new numbers out of old numbers.
   A List has the same form, so maybe it will also support inductive proofs.
   Therefore we annotated the data type with the `%elim` directive.
   We also gave idris a hint how to name lists, like we did with the natural
   numbers. Only this time we choose pluraly names.    
   
   Another thing to note is, that the list has this `a` in the type
   We didn't say what `a` should be, so it could be anything.
   One says that this data type is _parametrized_ by `a`.
   And indeed, when we look at the type of `ConsList` (with the cursor on `ConsList`
   type C-c C-d) we see that it is of type
   
       Tutorial.Basics.ConsList : Type -> Type
       
   So the data type takes a `Type` and returns a `Type`.
    
   For example we could construct (which is were the _cons_  comes from) a
   list of strings:
                                                                                    
```haskell
silly : ConsList String
silly = Cons "And" (Cons "now" (Cons "for" (Cons "something" (Cons "completely" (Cons "different" EmptyList)))))

``` 
   Phew! Thats as bad as typing in natural numbers.
                                                                                    
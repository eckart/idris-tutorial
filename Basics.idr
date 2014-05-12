-- * Basic Datatypes
-- -----------------------------------------------------------------------------
{-

   An Idris source file usually starts with a module definition much like
   you'd expect.
   The module definition is optional and there can be at most one per file.

   This paragraph also illustrates the comment syntax: lines starting with "--"
   contain a single line comment whereas multiline comments are enclosed between "{-"
   and "-}".
   There exists another kind of comment which we will explaing later
 
-}
module Tutorial.Basics

import Data.SortedMap                      

{-
   A module can import other modules. Idris will also silently import
   a module called the *prelude* that contains a host of usefull data types
   and functions. Sometimes you may want to use names, that the prelude
   also uses which may result in a name clash.
   In this case you can rename the import -}
import Prelude.List as L

{-
   We can define a new data type like this: -}
data Day = monday | tuesday | wednesday | thursday | friday | saturday | sunday

{- This defines a new data type as the sum or union of all days.
   It is kind of like a enumeration in other programming languages.
   An instance of this data type may be a monday __or__ a tuesday __or__ any
   of the other days.

   There is another syntax for defining data types with dependent types, that is
   types that depend on values but we will come to this later

   Now with our glorius data type defined we can write function that operate on days
-}

||| computes the next week day
nextWeekday : Day       -> Day
nextWeekday   monday    =  tuesday
nextWeekday   tuesday   =  wednesday
nextWeekday   wednesday =  thursday
nextWeekday   thursday  =  friday
nextWeekday   friday    =  monday
nextWeekday   saturday  =  monday
nextWeekday   sunday    =  monday

{-
   This function is defined as function that takes a Day and returns another Day.
   The triple bar __|||__ is a comment intended for api documentation like
   haddock in haskell or javadocs. 

   We can give the implementation for each case of the data type explicitly.
   We could also have used other patterns to match the possible arguments
   for a function that expects a day. This is an example of _pattern matching_.

   Pattern matching is exploited heavyly in Idris.
   Since is is known what patterns are possible you can even let Idris
   generate all the match clauses when using the interactive editing mode.
   The interactive editing mode is supported (to my knowledge) by the emacs mode
   and vim mode and is also visible in the repl.

   If you have a type signature using a data type like this:  
-}

defineMe : Day -> Day

{-
   you can (in emacs) type in C-c C-s with the cursor on the type definition
   and Idris will supply a template definition like this

       defineMe : Day -> Day
       defineMe x = ?defineMe_rhs

   Idris has choosen a default name for the variable containing the argument _x_
   and this strange `?defineMe_rhs`. It stands (unsurprisingly) for the right hand side
   of `defineMe`. A variable beginning with a __?__ is called a _metavariable_ or a _hole_.
   Metavariables stand for something to be done.
   In this case the metavariable `?defineMe_rhs`stands for a real implementation
   of the function `defineMe`. Note that unlike the previous function the argument _x_
   is a completely generic instance of `Day`. In the previous function we
   had a line for each different member of the data type `Day`.
   We could type (or copy) all cases to this function, but Idris can help
   you also with this: since Idris knows by the definition of the data type what
   cases can occur you can ask Idris to provide the missing cases.
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
   Of course Idris comes already equipped with booleans, but we will roll out our
   own private version of Booleans, to give glimpse of proofs.
-}

-- A Boolean is a data type with two cases: truth and falsehood
data Boolean = T | F

-- the _real_ data type in Idris is called `Bool` so this should not cause any name clashes
-- (you can look up this real `Bool` in the module `Prelude.Bool`)

{-
   We can now define the usual logical connectives `and`,`or` and `not`

-}

and : Boolean -> Boolean -> Boolean
and T T = T
and _ _ = F

{-
   So `and` returns `T` if and only if both arguments are also `T`.

   There are two things to notice if you are new to this syntax:
   1. you can read the type of the function as something that takes to `Booleans` and returns a `Boolean.
      But what it really says is: `and` is a function that takes a Boolean and returns a function that
      takes a Boolean and returns a function. So if you supply less that the maximal number of arguments
      you get back a function and not a Boolean.
   2. the underscore `_` is a special pattern that matches everything. It basically says: I don't care about
      this case.
 
   The other two connectives can be defined in a similar vein:      
-}
or : Boolean -> Boolean -> Boolean
or F F = T
or _ _ = T

-- well you can try to implement this yourself. Remember C-c C-s creates
-- a template implementation (with the cursor on `not`) 
-- and C-c C-c splits out all possible cases with the cursor on the `x`.
not : Boolean -> Boolean 

{-
   And now we come to the first proof: Will prove that `false` or `true` is 
   `true`. We didn't write that down explicitly in the definition of `or`
   so it will be nice to have it proved.
   
   Things that need to be proved are generally called _propositions_.
   A proposition in Idris is basically a function.
   Notice that the return type of the propositional function is an 
   equality, namely: `or F T = T`
   (Aside: you can use infix notation with a function name by surrounding 
    the function name with backticks.)
    
    So here is out glorious proposition.
-}
prfOr : F `or` T = T
prfOr = refl

{- 
   We can see that the proposition is proved by something called `refl`
   which is short for `reflexivity`. 
   
-}




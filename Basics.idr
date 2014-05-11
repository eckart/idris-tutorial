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
   you can (in emacs) type in C-c C-d with the cursor on the type definition
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

   
-}

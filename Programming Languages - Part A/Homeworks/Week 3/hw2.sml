(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* (a) Write a function all_except_option, which takes a string and a string list.
Return NONE if the string is not in the list, else return SOME lst where lst is identical
to the argument list except the string is not in it. You may assume the string is in the
list at most once. Use same_string, provided to you, to compare strings. Sample solution
is around 8 lines. *)

fun all_except_option (str, strs) =
   case strs of
      [] => NONE
      | x::xs' => case same_string(str,x) of
                  true => SOME(xs')
                  | false => case all_except_option(str,xs') of
                                 NONE => NONE
                                 | SOME v => SOME(x::v)


(* (b) Write a function get_substitutions1, which takes a string list list (a list of list
of strings, the substitutions) and a string s and returns a string list. The result has
all the strings that are in some list in substitutions that also has s, but s itself
should not be in the result. *)

fun get_substitutions1(subs, str) =
   case subs of
      [] => []
      | x::xs' => case all_except_option(str, x) of
                  NONE => [] @ get_substitutions1(xs', str)
                  | SOME v => v @ get_substitutions1(xs',str)

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except
it uses a tail-recursive local helper function. *)

fun get_substitutions2(subs, str) =
   let
      fun helper(subs, str, acc) =
         case subs of
            [] => acc
            | x::xs' => case all_except_option(str,x) of
                        NONE => helper(xs',str, acc)
                        | SOME v => helper(xs',str,acc @ v)
   in
   helper(subs,str,[])
   end

(* (d) Write a function similar_names, which takes a string list list of substitutions
(as in parts (b) and (c)) and a full name of type {first:string,middle:string,last:string}
and returns a list of full names (type {first:string,middle:string,last:string} list).
The result is all the full names you can produce by substituting for the first name (and only the first name)
using substitutions and parts (b) or (c). The answer should begin with the original name
(then have 0 or more other names). *)

fun similar_names(subs, {first=f,middle=m,last=l}) =
   let
   fun helper(subs, acc) =
      case subs of
         [] => acc
         | (x::xs') => helper(xs', acc @ [{first=x,middle=m,last=l}])
   in
   helper(get_substitutions2(subs, f), [{first=f,middle=m,last=l}])
   end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough. *)

fun card_color((s, _)) =
   case s of
      Spades => Black
   |  Clubs => Black
   |  _ => Red

(* (b) Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)

fun card_value((_, r)) =
   case r of
      Num i => i
      | Ace => 11
      | _ => 10

(* (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e.
It returns a list that has all the elements of cs except c. If c is in the list more than once, remove
only the first one. If c is not in the list, raise the exception e. You can compare cards with =. *)

fun remove_card (cs, c, e) =
   case cs of
      [] => raise e
      | x::xs' => case x = c of
                     true => xs'
                     | false => x::remove_card(xs', c, e)

(* (d) Write a function all_same_color, which takes a list of cards and returns true if all the cards
in the list are the same color. Hint: An elegant solution is very similar to one of the functions using
nested pattern-matching in the lectures. *)

fun all_same_color(cs) =
   case cs of
   [] => false
   | x::[] => true
   | x::(y::r) => card_color(x) = card_color(y) andalso all_same_color(y::r)

(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values.
Use a locally defined helper function that is tail recursive. (Take “calls use a constant amount of
stack space” as a requirement for this problem.) *)

fun sum_cards(cs) =
   let
   fun helper(cs,acc) =
      case cs of
         [] => acc
         | x::xs' => helper(xs', acc + card_value(x))
   in
   helper(cs, 0)
   end

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and
computes the score as described above. *)

fun score(cs, t) =
   let
      val sum = sum_cards(cs)
      fun pre_score(cs) =
         case sum > t of
            true => 3 * (sum - t)
            | false => t - sum
   in
      case all_same_color(cs) of
         true => pre_score(cs) div 2
         | false => pre_score(cs)
   end

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a
move list (what the player “does” at each point), and an int (the goal) and returns the score
at the end of the game after processing (some or all of) the moves in the move list in order. Use
a locally defined recursive helper function that takes several arguments that together represent
the current state of the game. *)

fun officiate(cs, mvs, t) =
   let
   fun helper(pcs, cs, mvs, t) =
      case mvs of
         [] => score(pcs, t)
         | x::xs' => case x of
                        Discard c => helper(remove_card(pcs, c, IllegalMove), cs, xs', t)
                        | Draw => case cs of
                                 [] => score(pcs, t)
                                 | y::ys' => case sum_cards(y::pcs) > t of
                                             true => score(y::pcs, t)
                                             | false => helper(y::pcs, ys', xs', t)
   in
   helper([], cs, mvs, t)
   end
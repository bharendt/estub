
-module(mock_dummy).
-export([fun_with_arity_zero/0, fun_with_arity_one/1, fun_with_arity_two/2]).
-export([fun_with_differenct_clauses/1]).

fun_with_arity_zero() ->
  fun_with_arity_zero.
  
fun_with_arity_one(Arg) ->
  fun_with_arity_one.
  
fun_with_arity_two(Arg1, Arg2) ->
  fun_with_arity_two.
  
  
fun_with_differenct_clauses(Arg) when Arg == 1 ->
  fun_with_differenct_clauses_1;  
fun_with_differenct_clauses(Arg) when Arg == 2 ->
  fun_with_differenct_clauses_2;  
fun_with_differenct_clauses(Arg) when Arg == 3 ->
  fun_with_differenct_clauses_3.    
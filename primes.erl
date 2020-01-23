% to run:
% erlc primes.erl; erl -noshell -s primes main -s init stop

-module(primes).
-compile(export_all).

% Idiomatic Erlang Sieve of Erathosthenes

primes(Prime, Max, Primes, Integers) when Prime > Max ->
  lists:reverse([Prime|Primes]) ++ Integers;

primes(Prime, Max, Primes, Integers) ->
  [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
  primes(NewPrime, Max, [Prime|Primes], NewIntegers).

primes(N) ->
  primes(2, round(math:sqrt(N)), [], lists:seq(3,N,2)). % skip odds

% Clone with array

primes_a(N) when N < 2 -> [];
primes_a(N) when N == 2 -> [ 2 ];
primes_a(N) ->
  Mroot = trunc(math:sqrt(N)),
  S = filter_primes_a(0, Mroot, 1 + (N-3) div 2, array:from_list(lists:seq(3, N, 2))),
  [2 | [X || X <- array:to_list(S), X =/= 0]].

filter_primes_a(I, Mroot, _Half, S) when 2*I+3 > Mroot -> S;
filter_primes_a(I, Mroot, Half, S) ->
  M = 2*I+3,
  E = array:get(I, S),
  if
    E > 0 -> filter_primes_a(I+1, Mroot, Half, set_0a((M*M-3) div 2, M, Half, S));
    E == 0 -> filter_primes_a(I+1, Mroot, Half, S)
  end.

set_0a(J, _M, Jmax, S) when J >= Jmax -> S;
set_0a(J, M, Jmax, S) ->
  set_0a(J+M, M, Jmax, array:set(J, 0, S)).

% Clone with ets

primes_s(N) when N < 2 -> [];
primes_s(N) when N == 2 -> [ 2 ];
primes_s(N) ->
  Mroot = trunc(math:sqrt(N)),
  Seq = lists:seq(0, (N-3) div 2, 1),
  S = ets:new(tab, [ordered_set]),
  lists:foreach(fun(I) -> ets:insert(S, {I, 3+I*2}) end, Seq),
  filter_primes_s(0, Mroot, 1 + (N-3) div 2, S),
  ets:foldr(fun(E, A) -> [element(2,E)|A] end, [2], S).

filter_primes_s(I, Mroot, _Half, _S) when 2*I+3 > Mroot -> ok;
filter_primes_s(I, Mroot, Half, S) ->
  M = 2*I+3,
  E = ets:lookup(S, I),
  if
    length(E) > 0 -> set_0s((M*M-3) div 2, M, Half, S), filter_primes_s(I+1, Mroot, Half, S);
    length(E) == 0 -> filter_primes_s(I+1, Mroot, Half, S)
  end.

set_0s(J, _M, Jmax, _S) when J >= Jmax -> ok;
set_0s(J, M, Jmax, S) ->
  ets:delete(S, J),
  set_0s(J+M, M, Jmax, S).

% All of the above are almost equal and shitty anyway (~40 times slower than nodejs, ~100 times slower than C++)

print_times(T, N) ->
  io:format("Erlang: ~p iterations in ~p seconds = ~p seconds per 30 iterations~n", [N, T, T*30.0/N]).

start(T, N) ->
  lists:foreach(
    fun(_) ->
      P = primes(10000000),
      io:format("Found ~p primes~n", [length(P)])
    end, lists:seq(1, N)),
  print_times((os:system_time(millisecond)-T)/1000, N).

main() ->
  start(os:system_time(millisecond), 2).

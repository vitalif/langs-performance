% to run:
% erlc primes.erl; erl -noshell -s primes main -s init stop

-module(primes).
-compile(export_all).

primes(Prime, Max, Primes,Integers) when Prime > Max ->
  lists:reverse([Prime|Primes]) ++ Integers;

primes(Prime, Max, Primes, Integers) ->
  [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
  primes(NewPrime, Max, [Prime|Primes], NewIntegers).

primes(N) ->
  primes(2, round(math:sqrt(N)), [], lists:seq(3,N,2)). % skip odds

print_times(T, N) ->
  io:format("Erlang: ~p iterations in ~p seconds = ~p seconds per 30 iterations~n", [N, T, T*30.0/N]).

start(T, N) ->
  lists:foreach(
    fun(_) ->
      io:format("Found ~p primes~n", [length(primes(10000000))])
    end, lists:seq(1, N)),
  print_times((os:system_time(millisecond)-T)/1000, N).

main() ->
  start(os:system_time(millisecond), 5).

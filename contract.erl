-module(contract).
-export([run/1]).

run(N) -> fac(N).

fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

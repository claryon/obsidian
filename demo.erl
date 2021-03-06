-module(demo).
-export([run/0, feed/2, dump/1]).

% 1.1
run() ->
        io:fwrite("5 client demo start.\n"),
        crypto:start(),
        % 1.3 
        C5 = client:start(5, superauditor, nil),
        C4 = client:start(4, auditor, C5),
        C1 = client:start(1, bank, C4),
        C2 = client:start(2, bank, C4),
        C3 = client:start(3, bank, C4),
        % 1.4
        bench(contract, run, 100), % 17
        bench_stats(contract, run, [100], 1000),
        % 1.5
        spawn(demo, feed, 
                [self(),#{1=>C1, 2=>C2, 3=>C3, 4=>C4, 5=>C5}]),
        receive shutdown -> ok end,
        io:fwrite("demo end.\n").

% 18
% for demo this substitutes for agency of receivers
feed(P,Peers=#{1:=C1, 2:=C2, 3:=C3, 4:=C4}) ->
        timer:sleep(200), broadcast(Peers, {demo_peers, Peers}),
        timer:sleep(200), C2 ! {demo_peers, #{3=>C3,4=>C4}}, % demo: incomplete
        timer:sleep(200), C1 ! { event, "event x"},
        timer:sleep(200), C1 ! demo_request,
        timer:sleep(200), C1 ! demo_broadcast,
        timer:sleep(200), C1 ! demo_commit,
        timer:sleep(200), broadcast(Peers, demo_propose),
        timer:sleep(200), broadcast(Peers, demo_choose),
        timer:sleep(400), C1 ! demo_upgrade,
        timer:sleep(200), C1 ! demo_pong,
        timer:sleep(200), broadcast(Peers, demo_shutdown),
        timer:sleep(200), P ! shutdown. % 1.7

broadcast(Clients, Msg) ->
        [Client ! Msg || {_,Client} <- maps:to_list(Clients)].

bench(M,F,A) ->
        _ = M:F(A),
        T0 = os:timestamp(),
        _ = M:F(A),_ = M:F(A),_ = M:F(A),_ = M:F(A),_ = M:F(A),
        _ = M:F(A),_ = M:F(A),_ = M:F(A),_ = M:F(A),_ = M:F(A),
        T = os:timestamp(),
        D = timer:now_diff(T, T0),
        io:format("~s:~w bench: ~b.~b microsec~n",
                [M, F, D div 10, D rem 10]),
        ok.        

bench_stats(M, F, A, N) when N > 0 ->
    L = bench_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("bench stats: ~b - ~b mics, med: ~b mics, avg: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.
 
bench_loop(_M, _F, _A, 0, List) ->
    List;
bench_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    bench_loop(M, F, A, N - 1, [T|List]).

dump([Client | Rest]) ->
        Client ! {state, self()},
        receive
                {Client, Id, {chain,H,[V|_]}} -> 
                        io:format("client ~w: ~s, ~s\n", [Id, H, V])
                after 500 ->
                        io:format("client ~w does not respond.\n", [Client])
        end,
        dump(Rest);
dump([]) ->
        io:fwrite("\n").


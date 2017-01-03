-module(client).
-export([start/3, loop/1, chain/0]).


%%%----------------------------------------------------------------------------

-record(state, {id, role, aud, names=#{}, white=[], local, public,
                buffer=[], counts=#{}, subchains=#{}}).
-record(chain, {hash, blocks}).
-record(proposal, {signer, signature, time, block}).
-record(block, {hash, subhashes=#{}, subchains=#{}}).
-record(commit, {signer, signature, time, hash}).

start(Id, Role, Aud) ->
        Loc = spawn(client, chain, []), 
        Pub = spawn(client, chain, []), 
        Pid = spawn(client, loop,
                [#state{id=Id, role=Role, aud=Aud, local=Loc, public=Pub}]),
        io:format("client ~p ~p ~p\n", [Id, Role, Pid]),
        Pid.

loop(S) ->
        loop(receive
                {id, Pid} -> Pid ! {id, Pid, S#state.id}, S;
                {state, Pid} -> state(S, Pid), S;
                demo_commit -> commit(S), S;
                {commit, C} -> copy(S, C);
                demo_propose -> propose(S), S;
                P=#proposal{} -> buffer(S, P);
                demo_choose -> choose(S), S;
                demo_request -> request(S), S;
                demo_broadcast -> broadcast(S, {msg, self(), abc}), S;
                {msg, Pid, M} -> echo(S, Pid, M), S;
                {confirm, Pid, X} -> confirm(S, Pid, X), S;
                {receipt, X, R} -> receipt(S, X, R), S;
                {event, Ev} -> record(S, Ev), S;
                {demo_peers, W} -> peers(S, W);
                {Command, X} -> log(S, unknown, Command, X), S;
                demo_upgrade -> upgrade(S), client:loop(S);
                demo_shutdown -> exit(ok);
                demo_pong -> log(S, ping), S;
                Command -> log(S, unknown, Command), S
                after 1000 -> log(S, ping), S
        end).

choose(S) ->
        {_Hash, Block, _Count, _Choices} =
                maps:fold(fun(H,{B, C},{MaxH, MaxB, MaxC, I}) -> 
                        log(S, H, C),
                        case (C > MaxC orelse C == MaxC andalso H > MaxH) of
                                true -> {H, B, C, I+1};
                                _ -> {MaxH, MaxB, MaxC, I+1}
                        end end, {nil, nil, 0, 0}, S#state.counts),
        timer:sleep(200),
        log(S, choose, _Hash, _Count),
        put_block(S#state.public, Block).

state(S, Pid) ->
        Pid ! {self(), S#state.id, get_chain(S#state.local)}.

copy(S, Commit=#commit{signer=Signer, hash=Hash}) ->
        log(S, copy, Signer, Hash),
        Prev = maps:get(Signer, S#state.subchains, ordsets:new()),
        Now = ordsets:add_element(Commit, Prev), 
        S#state{subchains=maps:put(Signer, Now, S#state.subchains)}.

commit(S) ->
        Hash = get_hash(S#state.local),
        log(S, commit, Hash),
        Time = erlang:now(),
        Signature = sign(S, {Time, Hash}), 
        Commit = {commit, S#state.id, Signature, Time, Hash},
        copy(S, Commit),
        broadcast(S, {commit, Commit}).

chain() ->
        chain(#chain{hash=hash([#block{}]), blocks=[#block{}]}).

chain(Chain) ->
        chain(receive
                {add, Block} ->
                        Blocks = [Block|Chain#chain.blocks],
                        #chain{hash=hash(Blocks), blocks=Blocks};
                {get, To} -> To ! {chain, Chain, erlang:now()}, Chain; 
                {last, To} ->
                        [Block|_] = Chain#chain.blocks,
                         To ! {last, Block, erlang:now()}, Chain;
                {hash, To} ->
                         To ! {hash, Chain#chain.hash}, Chain 
               end). 

get_chain(Chain) ->
        Chain ! {get, self()},
        receive {chain, C, Time} -> {C, Time} end.

put_block(Chain, Block) ->
        Chain ! {add, Block}.

get_hash(Chain) ->
        Chain ! {hash, self()},
        receive {hash, Hash} -> Hash end.

get_last(Chain) ->
        Chain ! {last, self()},
        receive {last, Block, Time} -> {Block, Time} end.

propose(S) ->
        P = proposal(S),
        H = P#proposal.block#block.hash,
        log(S, propose, H, peerids(S)),
        buffer(S, P),
        broadcast(S, P).

proposal(S) ->
        SubChains = S#state.subchains,
        {Previous, Time} = get_last(S#state.public),
        SubHashes = subhashes(SubChains, Previous#block.subhashes),
        Hash = hash([SubHashes, SubChains]),
        Block = {block, Hash, SubHashes, SubChains},
        Signature = sign(S, {Time, Block}),
        {proposal, S#state.id, Signature, Time, Block}.

subhashes(SubChains, Previous) ->
        maps:fold(fun(Signer,New,Acc) -> 
                maps:put(Signer, hash(
                        case maps:find(Signer, Previous) of
                                {ok, #commit{hash=Old}} -> [New, Old];
                                _ -> New
                        end), Acc) end,
        #{}, SubChains).

buffer(S, Proposal=#proposal{block=Block}) ->
        Hash = Block#block.hash,
        {_,Count} = maps:get(Hash, S#state.counts, {nil,0}),
        Counts = maps:put(Hash, {Block, Count + 1}, S#state.counts),
        S#state{buffer=[Proposal|S#state.buffer], counts=Counts}.

broadcast(S, Msg) ->
        broadcast(S, S#state.white, Msg).

broadcast(S, Peers, Msg) ->
        [ P ! encrypt(S, Msg) || P <- Peers, P /= self() ].

peers(S0, Names) ->
        Whitelist =[ Pid || {_,Pid} <- maps:to_list(Names), Pid /= self()],
        S = S0#state{names=Names, white=Whitelist},
        log(S, peers, Whitelist),
        S.

request(S) ->
        Aud = S#state.aud,
        log(S, request, Aud, xyz),
        Aud ! encrypt(S, {confirm, self(), xyz}).

confirm(S, To, E) ->
        X = decrypt(S, E),
        log(S, confirm, X, sign(S, X)),
        record(S, {confirming, X, sign(S, X)}),
        To ! encrypt(S, {receipt, X, sign(S, X)}),
        case S#state.role of
                auditor -> S#state.aud ! encrypt(S, {confirm, To, X});
                _ -> nil
        end.

receipt(S, X, E) ->
        R = decrypt(S, E),
        log(S, receipt, X, R),
        record(S, {confirmed, X, R}).

echo(S, _Pid, E) ->
        V = decrypt(S, E),
        log(S, message, V).

record(S, V) ->
        log(S, 'record', V),
        put_block(S#state.local, V).

hash(B) when is_binary(B) ->
        <<X:256/big-unsigned-integer>> = crypto:hash(sha256, B),
        string:substr(integer_to_list(X, 16), 1, 7); % demo
hash(T) ->
        hash(term_to_binary(T)). % demo 

sign(_, X) -> % demo
        hash(erlang:iolist_to_binary(
                        [term_to_binary(X), term_to_binary(self())])). 

encrypt(_, X) ->
        X. % demo 

decrypt(_, E) ->
        E. % demo 

% ν / 16.
upgrade(S) ->
        log(S, upgrade),
        {ok, Org} = file:read_file("client.erl"),
        Up = binary:replace(Org, <<"ping">>, <<"ping_upgraded">>),
        ok = file:write_file("client.erl", Up),
        code:purge(?MODULE),
        {ok,client} = compile:file(client),
        code:load_file(?MODULE),
        ok = file:write_file("client.erl", Org).

peerids(S) ->
        [ Id || {Id,_} <- maps:to_list(S#state.names) ].

log(S, V) when is_record(S, state) ->
        io:format("client  ~w  ~p\n", [S#state.id, V]);
log(K, V) ->
        io:format("client    ~p: ~p\n", [K, V]).

log(S, K, V) when is_record(S, state) ->
        io:format("client  ~w  ~p: ~p\n", [S#state.id, K, V]).

log(S, K, V, D) when is_record(S, state) ->
        io:format("client  ~w  ~p: ~p -> ~p\n", [S#state.id, K, V, D]).


-module(client).
-export([start/3, loop/1]).


%%%----------------------------------------------------------------------------

-record(state, {id, role, aud, white, local, public, buffer, map}).
-record(local, {hash, values}).
-record(public, {hash, blocks}).
-record(proposal, {signer, signature, time, block}).
-record(block, {hash, subhashes=[], subchains=[]}).
-record(commit, {signer, signature, time, hash}).

start(Id, Role, Aud) ->
        Local = #local{hash=hash("genesis"), values=["genesis"]},
        Pid = spawn(client, loop,
                [#state{id=Id, role=Role, aud=Aud, local=Local, map=#{}}]),
        io:format("client ~p ~p ~p\n", [Id, Role, Pid]),
        Pid.

loop(S) ->
        loop(receive
                {state, Pid} -> Pid ! {self(), S#state.id, S#state.local}, S;
                demo_commit -> commit(S), S;
                {commit, C} -> map(S, C);
                demo_propose -> propose(S), S;
                demo_request -> request(S), S;
                demo_broadcast -> broadcast(S, {msg, self(), abc}), S;
                {proposal, P} -> buffer(S, P);
                {msg, Pid, M} -> echo(S, Pid, M), S;
                {confirm, Pid, X} -> confirm(S, Pid, X), S;
                {receipt, X, R} -> receipt(S, X, R), S;
                {event, Ev} -> log(S, Ev), add(S, Ev);
                {whitelist, W} -> whitelist(S, W);
                {Command, X} -> log(S, unknown, Command, X), S;
                demo_upgrade -> upgrade(S), client:loop(S);
                Command -> log(S, unknown, Command), S
                after 1000 -> log(S, ping), S
        end).

map(S, Commit=#commit{signer=Signer}) ->
        S#state{map=maps:put(Signer, Commit, S#state.map)}.

commit(S) ->
        Local = S#state.local,
        Hash = [V|Local#local.hash],
        Time = erlang:now(),
        Signature = sign(S, Time, Hash}, 
        Commit = {commit, S#state.id, Signature, Time, Hash},
        broadcast(S, {commit, Commit}.

public() ->
        public(#public{hash=hash([]), blocks=[]}).

public(Chain) ->
        public(receive
                {add, Block} ->
                        Blocks = [Block|Chain#public.blocks],
                        #public{hash=hash(Blocks), Blocks};
                {get, To} -> To ! {chain, Chain, erlang:now()}, Chain 
                {last, To} ->
                        [Block|_] = Chain#public.blocks,
                         To ! {last, Block, erlang:now()}, Chain 

               end). 

get_public(Chain) ->
        Chain ! {get, self()},
        receive {chain, Chain, Time} -> {Chain, Time} end.

get_last(Chain) ->
        Chain ! {last, self()},
        receive {last, Block, Time} -> {Block, Time} end.

propose(S) ->
        broadcast(S, proposal(S)).

proposal(S) ->
        SubChains = S#state.map,
        {Previous, Time} = get_last(S#state.public),
        SubHashes = subhashes(SubChains, Previous#block.subhashes),
        Hash = hash({SubHashes, SubChains}),
        Block = {block, Hash, SubHashes, SubChains},
        Signature = sign(S, {Time, Block}),
        Prop = {proposal, S#state.id, Signature, Time, Block}.

subhashes(SubChains, Previous) ->
        Fold = fun(Signer,New,Acc) -> 
                Acc#{Signer => 
                        case maps:find(Signer, Previous) of
                                {ok, #commit{hash=Old}} -> [New,Old];
                                _ -> [New]
                        end}
        maps:fold(Fold, #{}, SubChains).

buffer(S, Proposal) ->
        #state{buffer=[Proposal|S#state.buffer]}.

broadcast(S, Msg) ->
        log(S, broadcast, Msg),
        broadcast(S, S#state.white, Msg).

broadcast(S, [P|R], Msg) ->
        P ! encrypt(S, Msg),
        broadcast(S, R, Msg);
broadcast(_, [], _) ->
        nil.

whitelist(S, W) ->
        log(S, whitelist, W),
        S#state{white=W}.

request(S) ->
        Aud = S#state.aud,
        log(S, request, Aud, xyz),
        Aud ! encrypt(S, {confirm, self(), xyz}).

confirm(S, To, E) ->
        X = decrypt(S, E),
        log(S, confirm, X, sign(S, X)),
        add(S, {confirmed, X, sign(S, X)}),
        To ! encrypt(S, {receipt, X, sign(S, X)}),
        case S#state.role of
                auditor -> S#state.aud ! encrypt(S, {confirm, To, X});
                _ -> nil
        end.

receipt(S, X, E) ->
        R = decrypt(S, E),
        log(S, receipt, X, R),
        add(S, {confirmed, X, R}).

echo(S, _Pid, E) ->
        V = decrypt(S, E),
        log(S, message, V).

add(S, V) ->
        log(S, 'record', V),
        Local = S#state.local,
        Values = [V|Local#local.values],
        Hash = hash(term_to_binary([Local#local.hash|Values])),
        S#state{local=#local{hash=Hash, values=Values}}.

hash(B) ->
        <<X:256/big-unsigned-integer>> = crypto:hash(sha256, B),
        string:substr(integer_to_list(X, 16), 1, 7).

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

log(S, V) ->
        io:format("client ~w> ~p\n", [S#state.id, V]).

log(S, K, V) ->
        io:format("client ~w> ~p: ~p\n", [S#state.id, K, V]).

log(S, K, V, D) ->
        io:format("client ~w> ~p: ~p -> ~p\n", [S#state.id, K, V, D]).

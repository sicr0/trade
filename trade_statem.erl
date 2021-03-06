-module(trade_statem).
-behaviour(gen_statem).

% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
% gen_statem callbacks
-export([init/1, callback_mode/0, code_change/4, terminate/3]).
% Custom states
-export([idle/3, idle_wait/3, negotiate/3, wait/3, ready/3]).

% Data record
-record(data, {name = "", other, own_items = [], other_items = [], monitor, from}).

% Public API
start(Name) -> gen_statem:start(?MODULE, [Name], []).

start_link(Name) -> gen_statem:start_link(?MODULE, [Name], []).

trade(OwnPid, OtherPid) -> gen_statem:call(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) -> gen_statem:call(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) -> gen_statem:cast(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) -> gen_statem:cast(OwnPid, {retract_offer, Item}).

ready(OwnPid) -> gen_statem:call(OwnPid, ready, infinity).

cancel(OwnPid) -> gen_statem:stop(OwnPid).

% Client-To-Client API
ask_negotiate(OtherPid, OwnPid) -> gen_statem:cast(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) -> gen_statem:cast(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) -> gen_statem:cast(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) -> gen_statem:cast(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) -> gen_statem:cast(OtherPid, are_you_ready).

not_yet(OtherPid) -> gen_statem:cast(OtherPid, not_yet).

am_ready(OtherPid) -> gen_statem:cast(OtherPid, 'ready!').

ack_trans(OtherPid) -> gen_statem:cast(OtherPid, ack).

ask_commit(OtherPid) -> gen_statem:call(OtherPid, ask_commit).

do_commit(OtherPid) -> gen_statem:call(OtherPid, do_commit).

% gen_statem API
init(Name) -> {ok, idle, #data{name = Name}}.

callback_mode() -> state_functions.

code_change(_, StateName, Data, _) -> {ok, StateName, Data}.

terminate(normal, ready, D = #data{}) -> notice(D, "FSM leaving.", []);
terminate(_, _, _) -> ok.

% Custom states
idle(cast, {ask_negotiate, OtherPid}, D = #data{}) ->
    Ref = monitor(process, OtherPid),
    notice(D, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, D#data{other = OtherPid, monitor = Ref}};
idle({call, From}, {negotiate, OtherPid}, D = #data{}) ->
    ask_negotiate(OtherPid, self()),
    notice(D, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, D#data{other = OtherPid, monitor = Ref, from = From}};
idle(_, Event, _) ->
    unexpected(Event, idle),
    keep_state_and_data.

idle_wait(cast, {ask_negotiate, OtherPid}, D = #data{other = OtherPid}) ->
    gen_statem:reply(D#data.from, ok),
    notice(D, "starting negotiation", []),
    {next_state, negotiate, D};
idle_wait(cast, {accept_negotiate, OtherPid}, D = #data{other = OtherPid}) ->
    gen_statem:reply(D#data.from, ok),
    notice(D, "starting negotiation", []),
    {next_state, negotiate, D};
idle_wait({call, From}, accept_negotiate, D = #data{other = OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(D, "accepting negotiation", []),
    {next_state, negotiate, D, {reply, From, ok}};
idle_wait(_, Event, _) ->
    unexpected(Event, idle_wait),
    keep_state_and_data.

negotiate(cast, {make_offer, Item}, D = #data{own_items = OwnItems}) ->
    do_offer(D#data.other, Item),
    notice(D, "offering ~p", [Item]),
    {keep_state, D#data{own_items = add(Item, OwnItems)}};
negotiate(cast, {retract_offer, Item}, D = #data{own_items = OwnItems}) ->
    undo_offer(D#data.other, Item),
    notice(D, "cancelling offer on ~p", [Item]),
    {keep_state, D#data{own_items = remove(Item, OwnItems)}};
negotiate(cast, {do_offer, Item}, D = #data{other_items = OtherItems}) ->
    notice(D, "other player offering ~p", [Item]),
    {keep_state, D#data{other_items = add(Item, OtherItems)}};
negotiate(cast, {undo_offer, Item}, D = #data{other_items = OtherItems}) ->
    notice(D, "Other player cancelling offer on ~p", [Item]),
    {keep_state, D#data{other_items = remove(Item, OtherItems)}};
negotiate(cast, are_you_ready, D = #data{other = OtherPid}) ->
    io:format("Other user ready to trade~n"),
    notice(D, "Other user ready to transfer goods:~nYou get ~p, the other side gets ~p", [D#data.other_items, D#data.own_items]),
    not_yet(OtherPid),
    keep_state_and_data;
negotiate({call, From}, ready, D = #data{other = OtherPid}) ->
    are_you_ready(OtherPid),
    notice(D, "asking if ready, waiting", []),
    {next_state, wait, D#data{from = From}};
negotiate(_, Event, _) ->
    unexpected(Event, negotiate),
    keep_state_and_data.

wait(cast, {do_offer, Item}, D = #data{other_items = OtherItems}) ->
    gen_statem:reply(D#data.from, offer_changed),
    notice(D, "Other side offering ~p", [Item]),
    {next_state, negotiate, D#data{other_items = add(Item, OtherItems)}};
wait(cast, {undo_offer, Item}, D = #data{other_items = OtherItems}) ->
    gen_statem:reply(D#data.from, offer_changed),
    notice(D, "other side cancelling offer ~p", [Item]),
    {next_state, negotiate, D#data{other_items = remove(Item, OtherItems)}};
wait(cast, are_you_ready, D = #data{}) ->
    am_ready(D#data.other),
    notice(D, "asked if ready, and I am. Waiting for same reply", []),
    keep_state_and_data;
wait(cast, not_yet, D = #data{}) ->
    notice(D, "Other not ready yet", []),
    keep_state_and_data;
wait(cast, 'ready!', D = #data{}) ->
    am_ready(D#data.other),
    ack_trans(D#data.other),
    gen_statem:reply(D#data.from, ok),
    notice(D, "other side is ready. Moving to ready state", []),
    {next_state, ready, D};
wait(_, Event, _) ->
    unexpected(Event, wait),
    keep_state_and_data.

ready(cast, ack, D = #data{}) ->
    case priority(self(), D#data.other) of
        true ->
            try
                notice(D, "asking for commit", []),
                ready_commit = ask_commit(D#data.other),
                notice(D, "ordering commit", []),
                ok = do_commit(D#data.other),
                notice(D, "committing...", []),
                commit(D),
                {stop, normal, D}
            catch Class:Reason ->
                notice(D, "commit failed", []),
                {stop, {Class, Reason}, D}
            end;
        false ->
            keep_state_and_data
    end;
ready({call, From}, ask_commit, D) ->
    notice(D, "replying to ask commit", []),
    % Sus
    {keep_state_and_data, [{reply, From, ready_commit}]};
ready({call, _}, do_commit, D) ->
    notice(D, "committing...", []),
    commit(D),
    % Sus
    {stop, normal, D};
ready(_, Event, _) ->
    unexpected(Event, ready),
    keep_state_and_data.

% Private functions
add (Item, Items) -> [Item | Items].

remove(Item, Items) -> Items -- [Item].

notice(#data{name = N}, Str, Args) -> io:format("~s: " ++ Str ++ "~n", [N | Args]).

unexpected(Msg, State) -> io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

commit(D = #data{}) -> io:format("Transaction completed for ~s. Items sent are:~n~p,~n received are:~n~p.~nThis operation should have some atomic save in a database.~n", [D#data.name, D#data.own_items, D#data.other_items]).

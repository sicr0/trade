-module(trade_fsm).
-behaviour(gen_statem).

% Public API
-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/3, handle_event/4, handle_info/3, code_change/4, terminate/3]).
% Custom states
-export([idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2, ready/2, ready/3]).

-record(state, {name = "", other, own_items = [], other_items = [], monitor, from}).

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

am_ready(OtherPid) -> gen_statem:cast(OtherPid, ready).

ack_trans(OtherPid) -> gen_statem:cast(OtherPid, ack).

ask_commit(OtherPid) -> gen_statem:call(OtherPid, ask_commit).

do_commit(OtherPid) -> gen_statem:call(OtherPid, do_commit).

notify_cancel(OtherPid) -> gen_statem:stop(OtherPid).

% gen_statem API
init(Name) -> {ok, idle, #state{name = Name}}.

callback_mode() -> state_functions.

handle_event(cancel, _, S = #state{}) ->
    notice(S, "received cancel event", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.
handle_event(cancel, _, _, S = #state{}) ->
    notify_cancel(S#state.other),
    notice(S, "cancelling trade, sending cancel event", []),
    {stop, cancelled, ok, S};
handle_event(Event, _, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _, S = #state{other = Pid, monitor = Ref}) ->
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

code_change(_, StateName, Data, _) -> {ok, StateName, Data}.

terminate(normal, ready, S = #state{}) -> notice(S, "FSM leaving.", []);
terminate(_, _, _) -> ok.

% Custom states
idle({ask_negotiate, OtherPid}, S = #state{}) ->
    Ref = monitor(process, OtherPid),
    notice(S, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.
idle({negotiate, OtherPid}, From, S = #state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref, from = From}};
idle(Event, _, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
    gen_statem:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
    gen_statem:reply(S#state.from, ok),
    notice(S, "starting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.
idle_wait(accept_negotiate, _, S = #state{other = OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(S, "accepting negotiation", []),
    {reply, ok, negotiate, S};
idle_wait(Event, _, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

negotiate({make_offer, Item}, S = #state{own_items = OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate, S#state{own_items = OwnItems}};
negotiate({retract_offer, Item}, S = #state{own_items = OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{own_items = remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S = #state{other_items = OtherItems}) ->
    notice(S, "other player offering ~p", [Item]),
    {next_state, negotiate, S#state{other_items = add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S = #state{other_items = OtherItems}) ->
    notice(S, "Other player cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{other_items = remove(Item, OtherItems)}};
negotiate(are_you_ready, S = #state{other = OtherPid}) ->
    io:format("Other user ready to trade~n"),
    notice(S, "Other user ready to transfer goods:~nYou get ~p, the other side gets ~p", [S#state.other_items, S#state.own_items]),
    not_yet(OtherPid),
    {next_state, negotiate, S};
negotiate(Event, Data) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, Data}.
negotiate(ready, From, S = #state{other = OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "asking if ready, waiting", []),
    {next_state, wait, S#state{from = From}};
negotiate(Event, _, S) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, S}.

wait(are_you_ready, S = #state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if ready, and I am. Waiting for same reply", []),
    {next_state, wait, S};
wait(not_yet, S = #state{}) ->
    notice(S, "Other not ready yet", []),
    {next_state, wait, S};
wait(ready, S = #state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_statem:reply(S#state.from, ok),
    notice(S, "other side is ready. Moving to ready state", []),
    {next_state, ready, S};
wait(Event, Data) ->
    unexpected(Event, wait),
    {next_state, wait, Data}.

ready(ack, S = #state{}) ->
    case priority(self(), S#state.other) of
        true ->
            try
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.other),
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.other),
                notice(S, "committing...", []),
                commit(S),
                {stop, normal, S}
            catch Class:Reason ->
                notice(S, "commit failed", []),
                {stop, {Class, Reason}, S}
            end;
        false ->
            {next_state, ready, S}
    end;
ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.
ready(ask_commit, _, S) ->
    notice(S, "replying to ask commit", []),
    {reply, ready_commit, ready, S};
ready(do_commit, _, S) ->
    notice(S, "committing...", []),
    commit(S),
    {stop, normal, ok, S};
ready(Event, _, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

% Private functions
add (Item, Items) -> [Item | Items].

remove(Item, Items) -> Items -- [Item].

notice(#state{name = N}, Str, Args) -> io:format("~s: " ++ Str ++ "~n", [N | Args]).

unexpected(Msg, State) -> io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

commit(S = #state{}) -> io:format("Transaction completed for ~s. Items sent are:~n~p,~n received are:~n~p.~nThis operation should have some atomic save in a database.~n", [S#state.name, S#state.own_items, S#state.other_items]).

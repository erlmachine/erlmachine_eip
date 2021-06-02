-module(erlmachine_eip_model_splitter).

-behaviour(erlmachine_worker_model).

-export([startup/4]).

-export([process/3, execute/3]).
-export([pressure/3]).

-export([shutdown/3]).

-include_lib("erlmachine/include/erlmachine_user.hrl").
-include_lib("erlmachine/include/erlmachine_system.hrl").

-type state() :: map().

-spec startup(UID::uid(), State::state(), Opt::list(), Env::map()) ->
                     success(state()) | failure(term(), term(), state()).
startup(_UID, State, _Opt, _Env) ->
    erlmachine:success(State).

-spec process(UID::uid(), Motion::term(), State::state()) ->
                     success(state()) | success(term(), state()) | failure(term(), term(), state()).
process(_UID, Motion, State) ->
    Items = erlmachine:body(Motion),

    try Items of
        _ when is_list(Items) ->

            [begin Payload = erlmachine:body(Motion, Item), erlang:send(self(), Payload) end || Item <- Items]
    catch E:R ->
            erlmachine:failure(E, R, State)
    end.

-spec execute(UID::uid(), Action::term(), State::state()) ->
                     success(term(), state()) | failure(term(), term(), state()).
execute(_UID, _Action, State) ->
    erlmachine:success(ignore, State).

-spec pressure(UID::uid(), Load::term(), State::state()) ->
                      success(state()) | success(term(), state()) | failure(term(), term(), state()).
pressure(_UID, Load, State) ->
    erlmachine:success(Load, State).

-spec shutdown(UID::uid(), Reason::term(), State::state()) ->
                      success().
shutdown(_UID, _Reason, _State) ->
    erlmachine:success().

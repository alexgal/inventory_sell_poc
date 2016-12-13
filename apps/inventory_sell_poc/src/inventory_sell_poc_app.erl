%%%-------------------------------------------------------------------
%% @doc inventory_sell_poc public API
%% @end
%%%-------------------------------------------------------------------

-module(inventory_sell_poc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", basic_handler, []},
            {"/[...]", basic_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(
                http,
                200,
                [{port, 8080}],
                [{env, [{dispatch, Dispatch}]},
                  {max_keepalive, 5},
                  {nodelay, true}]
              ),
    inventory_sell_poc_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

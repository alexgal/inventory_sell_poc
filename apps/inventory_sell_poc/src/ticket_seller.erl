%%%-------------------------------------------------------------------
%%% @author alexey
%%% @doc
%%%
%%% @end
%%% Created : 12. Dec 2016 00:03
%%%-------------------------------------------------------------------
-module(ticket_seller).
-author("alexey").

-define(MAX_AMOUNT_OF_TICKETS_PER_ORDER, 10).

-include("inventory.hrl").

%% API
-export([get/2, buy/2]).

get(seated, OrderNUmber) when OrderNUmber > ?MAX_AMOUNT_OF_TICKETS_PER_ORDER ->
  {error, too_much};
get(seated, OrderNumber) ->
  Fun = fun() ->
          MatchHead = #seated_tickets{ticket_id = '$1',
                                      row_id ='$2',
                                      seat ='$3',
                                      is_bought = false},
          Result = ['$1', '$2', '$3'],
          mnesia:select(
            seated_tickets,
            [ {MatchHead, [], [Result]
            }],
            OrderNumber,
            read)
        end,
  {atomic, Result} = mnesia:transaction(Fun),
  get_result(Result, OrderNumber);
get(non_seated, OrderNumber) ->
  Fun = fun() ->
          MatchedHead = #non_seated_tickets{ticket_id = '$1',
                                            _= '_',
                                            is_bought = false},
          Result = '$1',
          mnesia:select(non_seated_tickets,
                        [{MatchedHead, [], [Result]}],
                        OrderNumber,
                        read)
        end,
  {atomic, Result} = mnesia:transaction(Fun),
  get_result(Result, OrderNumber).

buy(seated, TicketId) ->
  Fun = fun() ->
        MatchedHead = #seated_tickets{ticket_id = TicketId,
                                      _= '_',
                                      _= '_',
                                      is_bought = '$1'},
        Result = '$1',
        {[false], _} = mnesia:select(seated_tickets,
          [{MatchedHead, [], [Result]}],
          1,
          write),
          Ticket = #seated_tickets{ticket_id = TicketId,
                                   is_bought = true},
          mnesia:write(Ticket)
        end,
  buy_result(mnesia:transaction(Fun));
buy(non_seated, TicketId) ->
  Fun = fun() ->
        MatchedHead = #non_seated_tickets{ticket_id = TicketId,
                                          _= '_',
                                          is_bought = '$1'},
        Result = '$1',
        {[false], _} = mnesia:select(non_seated_tickets,
                                 [{MatchedHead, [], [Result]}],
                                 1,
                                 write),
        Ticket = #non_seated_tickets{ticket_id = TicketId, is_bought = true},
        mnesia:write(Ticket)
      end,
  buy_result(mnesia:transaction(Fun)).

buy_result({atomic, ok}) ->
  ok;
buy_result(_) ->
  {error, bought}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

get_result('$end_of_table', _) ->
  {error, soldout};
get_result([], _) ->
  {error, soldout};
get_result({Result, _Cont}, Number) when length(Result) < Number ->
  {error, too_much};
get_result({Result, _Cont}, _Number) ->
  {ok, Result}.

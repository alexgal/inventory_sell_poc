%%%-------------------------------------------------------------------
%%% @author alexey
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2016 17:47
%%%-------------------------------------------------------------------
-module(application_test).
-author("alexey").

-include_lib("eunit/include/eunit.hrl").

-include("../src/inventory.hrl").

%%%-------------------------------------------------------------------
%%% Test of test data population
%%%-------------------------------------------------------------------
can_populate_seated_tickets_test_() ->
      Rows = 20,
      Seats = 20,
      ok = ticket_storage:populate({seated, {Rows, Seats}}),
      %% total number of tickets expected is matrix of Rows*Seats
      TotalNr = ticket_storage:get_tickets_count(seated),
      ticket_storage:reset(), %% reset tickets count to 0
      TotalNr1 = ticket_storage:get_tickets_count(seated),
      [?_assert(TotalNr =:= Rows*Seats),
       ?_assert(TotalNr1 =:= 0)].

can_populate_non_seated_tickets_test_() ->
    TotalTickets = 400,
    ok = ticket_storage:populate({non_seated, TotalTickets}),
    %% total number of tickets expected is matrix of Rows*Seats
    TotalNr = ticket_storage:get_tickets_count(non_seated),
    ticket_storage:reset(), %% reset tickets count to 0
    TotalNr1 = ticket_storage:get_tickets_count(non_seated),
    [?_assert(TotalNr =:= TotalTickets),
    ?_assert(TotalNr1 =:= 0)].

populate_both_types_of_tickets_test_() ->
  Rows = 20,
  Seats = 20,
  TotalTickets = 400,
  insert_both_ticket_types(TotalTickets, {Rows, Seats}),
  TotalNr = ticket_storage:get_tickets_count(),
  ticket_storage:reset(), %% reset tickets count to 0
  ?_assert(TotalNr =:= TotalTickets + (Rows * Seats)).

%%%-------------------------------------------------------------------
%%% Test can buy ticket & test sold out state
%%%-------------------------------------------------------------------
can_get_seated_tickets_and_buy_them_afterwards_test_() ->
  insert_both_ticket_types(1, {2, 5}),
  %% test seated tickets
  %% trying to order 9 out of 10 available tickets
  {ok, Tickets} = ticket_seller:get(seated, 9),
  F = fun([Id,_,_], _) ->
        ticket_seller:buy(seated, Id)
      end,
  CouldBuy = lists:foldl(F, not_ok, Tickets),
  %% should still have 1 ticket in stock, therefore expect error "too_much"
  {error, too_much} = ticket_seller:get(seated, 10),
  %% buying the last ticket in stock
  {ok, [Ticket]} = ticket_seller:get(seated, 1),
  [Id1, _,_] = Ticket,
  ticket_seller:buy(seated, Id1),
  %% now stock should be completely soldout
  {error, soldout} = ticket_seller:get(seated, 10),
  ?_assert(CouldBuy =:= ok).

can_get_non_seated_tickets_and_buy_them_afterwards_test_() ->
  insert_both_ticket_types(10, {2, 5}),
  {ok, Tickets} = ticket_seller:get(non_seated, 9),
  F = fun(Id, _) ->
    ticket_seller:buy(non_seated, Id)
      end,
  CouldBuy = lists:foldl(F, not_ok, Tickets),
  %% should still have 1 ticket in stock, therefore expect error "too_much"
  {error, too_much} = ticket_seller:get(non_seated, 10),
  %% buying the last ticket in stock
  {ok, [Id1]} = ticket_seller:get(non_seated, 1),
  ticket_seller:buy(non_seated, Id1),
  %% now stock should be completely soldout
  {error, soldout} = ticket_seller:get(non_seated, 10),
  ?_assert(CouldBuy =:= ok).

cannot_buy_same_ticket_twice_test_() ->
  %% non seated
  insert_both_ticket_types(1, {1, 1}),
  {ok, [Id]} = ticket_seller:get(non_seated, 1),
  ok = ticket_seller:buy(non_seated, Id),
  Error = ticket_seller:buy(non_seated, Id),
  %% seated
  {ok, [[Id1,_,_]]} = ticket_seller:get(seated, 1),
  ok = ticket_seller:buy(seated, Id1),
  Error1 = ticket_seller:buy(seated, Id1),
  [?_assert({error, bought} =:= Error),
   ?_assert({error, bought} =:= Error1)].

%%@todo
%% 2 align code no max than 80 lines
%% 3 remove duplications


%%%-------------------------------------------------------------------
%%% Common test functions
%%%-------------------------------------------------------------------

insert_both_ticket_types(TotalTickets, {Rows, Seats}) ->
  Tickets = [{seated, {Rows, Seats}}, {non_seated, TotalTickets}],
  ok = ticket_storage:populate(Tickets).
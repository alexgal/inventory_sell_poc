%%%-------------------------------------------------------------------
%%% @author alexey
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2016 18:07
%%%-------------------------------------------------------------------
-module(ticket_storage).
-author("alexey").

-include("inventory.hrl").

%% API
-export([populate/1, get_tickets_count/1, get_tickets_count/0, reset/0]).

%%%-------------------------------------------------------------------
%%% 1. Creates mnesia Schema. If schema exists deletes it and creates new one
%%% 2. Starts mnesia database
%%% 3. Creates mnesia tables based on app records from inventory.hrl
%%% 4. Inserts test data
%%%-------------------------------------------------------------------
populate(List = [E1,E2]) when is_list(List) andalso (length(List) =:= 2)->
  mnesia:stop(), %% stop mnesia if its running
  create_schema(),
  mnesia:start(),
  create_tables(),
  populate(E1),
  populate(E2);
populate({seated, {Rows, Seats}}) ->
  insert_tickets({seated, Rows, Seats});
populate({non_seated, TotalNumber}) ->
  insert_tickets({non_seated, TotalNumber}).

insert_tickets({non_seated, TotalNumber}) ->
  insert_tickets({non_seated, TotalNumber}, 0);
insert_tickets({seated, 0, _Seats}) ->
  ok;
insert_tickets({seated, Row, Seats}) ->
  insert_seated_tickets(Row, Seats),
  Row1 = Row-1,
  insert_tickets({seated, Row1, Seats}).

insert_tickets({non_seated, TotalNumber}, TotalNumber) ->
  ok;
insert_tickets({non_seated, TotalNumber}, Number) ->
  Id = generate_id(non_seated),
  Ticket = #tickets{id=Id,
    name="Standing ticket " ++ integer_to_list(Id)},
  Fun =
    fun() ->
      mnesia:write(Ticket),
      SeatedTicket = #non_seated_tickets{ticket_id = Id,
        amount = TotalNumber,
        is_bought = false},
      mnesia:write(SeatedTicket)
    end,
  {atomic, ok} = mnesia:transaction(Fun),
  insert_tickets({non_seated, TotalNumber}, Number+1).

create_tables() ->
  mnesia:create_table(tickets,
    [{ram_copies, [node()]},
      {attributes, record_info(fields, tickets)}]),
  mnesia:create_table(seated_tickets,
    [{ram_copies, [node()]},
      {attributes, record_info(fields, seated_tickets)}]),
  mnesia:create_table(non_seated_tickets,
    [{ram_copies, [node()]},
      {attributes, record_info(fields, non_seated_tickets)}]).

create_schema() ->
  case mnesia_schema:ensure_no_schema([node()]) of
    ok ->
      mnesia:create_schema([node()]);
    _Any ->
      mnesia:delete_schema([node()]),
      mnesia:create_schema([node()])
  end.
%%counts total number of tickets in the system
get_tickets_count() ->
  Fun =
    fun() ->
      mnesia:match_object(tickets, {tickets, '_', '_'}, read)
    end,
  {atomic,Tickets} = mnesia:transaction(Fun),
  length(Tickets).

get_tickets_count(seated) ->
  Fun =
    fun() ->
      mnesia:match_object(seated_tickets,
                          {seated_tickets, '_', '_','_','_'},
                          read)
    end,
  {atomic,Tickets} = mnesia:transaction(Fun),
  length(Tickets);
get_tickets_count(non_seated) ->
  Fun =
    fun() ->
      mnesia:match_object(non_seated_tickets,
                          {non_seated_tickets, '_', '_','_'},
                          read)
    end,
  {atomic,Tickets} = mnesia:transaction(Fun),
  length(Tickets).

reset() ->
  % delete seated tickets
  Fun =
    fun() ->
      Tickets = mnesia:match_object(seated_tickets,
                                    {seated_tickets, '_', '_','_','_'},
                                    write),
      lists:foreach(fun(T) -> mnesia:delete_object(T) end, Tickets)
    end,
  {atomic, ok} = mnesia:transaction(Fun),
  % delete core tickets record
  Fun1 =
    fun() ->
      Tickets = mnesia:match_object(tickets, {tickets, '_', '_'}, write),
      lists:foreach(fun(T) -> mnesia:delete_object(T) end, Tickets)
    end,
  {atomic, ok} = mnesia:transaction(Fun1),
  % delete non-seated tickets
  Fun2 =
    fun() ->
      Tickets = mnesia:match_object(non_seated_tickets,
                                    {non_seated_tickets, '_', '_', '_'},
                                    write),
      lists:foreach(fun(T) -> mnesia:delete_object(T) end, Tickets)
    end,
  {atomic, ok} = mnesia:transaction(Fun2),
  % @todo
  ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
insert_seated_tickets(0, _Seats) ->
  ok;
insert_seated_tickets(Rows, Seats) ->
  insert_seated_row(Rows, Seats, Rows, Seats).

insert_seated_row(_Row, _Seats, 0, _SeatPos) ->
  ok;
insert_seated_row(Row, Seats, RowPos, 0) ->
  insert_seated_row(Row, Seats, RowPos-1, Seats);
insert_seated_row(Row, Seats, RowPos, SeatPos) ->
  Id = generate_id(Row, Seats, RowPos, SeatPos),
  Ticket = #tickets{id=Id,
    name="Seated ticket " ++ integer_to_list(Id)},
  Fun =
    fun() ->
      mnesia:write(Ticket),
      SeatedTicket = #seated_tickets{ticket_id = Id,
        is_bought = false,
        row_id = Row,
        seat = Seats},
      mnesia:write(SeatedTicket)
    end,
  mnesia:transaction(Fun),
  SeatPos1 = SeatPos-1,
  insert_seated_row(Row, Seats, RowPos, SeatPos1).

%%%-------------------------------------------------------------------
%%% Generates id based on matrix height(Rows), width(Seats) and
%%% current positions hp(Row) and wp(Seat)
%%%-------------------------------------------------------------------
generate_id(Rows, Seats, Row, Seat) ->
  (Rows * Seats) - (Rows - Row) * Seats - Seat + 1.
generate_id(non_seated) ->
  Fun = fun() ->
          mnesia:match_object(tickets, {tickets, '_', '_'}, read)
        end,
  {atomic, Tickets} = mnesia:transaction(Fun),
  Nr = length(Tickets),
  case Nr > 0 of
    true ->
      Nr+1;
    false ->
       1
  end.
-module(basic_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([handle_html/2]).
-export([handle_json/2]).
-export([handle_submit/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, handle_json}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_submit}],
    Req,
    State
  }.
allowed_methods(Req, State) ->
  {
    [<<"GET">>, <<"POST">>, <<"DELETE">>, <<"HEAD">>, <<"OPTIONS">>],
    Req,
    State
  }.


handle_html(Req, State) ->
  Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<title>Inventory selling example</title>
</head>
<body>
	<p>Please use rest in order to communicate with server :) </p>
</body>
</html>">>,
  {Body, Req, State}.

handle_json(Req, State) ->
  {Path, _} = cowboy_req:path(Req),
  get(Path, Req, State).

get(_Path = <<"/get/seated">>, Req, State) ->
  Amount = get_qs(Req, <<"amount">>, 1),
  Result = ticket_seller:get(seated, Amount),
  reply_get(Result, Req, State, seated);
get(_Path = <<"/get/non_seated">>, Req, State) ->
  Amount = get_qs(Req, <<"amount">>, 1),
  Result = ticket_seller:get(non_seated, Amount),
  reply_get(Result, Req, State, non_seated);
get(_Path, Req, State) ->
  Body = <<"{}">>,
  reply(Body, 404, Req, State).

reply_get({ok,Tickets}, Req, State, non_seated) ->
  {_, Map} =
    lists:foldl(
      fun(Id, {Ctr, Acc}) ->
        Nr =integer_to_binary(Ctr),
        Key = <<"ticket_", Nr/binary>>,
        Acc1 = maps:merge(Acc,#{Key=>Id}),
        {Ctr+1, Acc1}
      end,
      {0,#{}},
      Tickets
    ),
    Map1 = maps:merge(#{<<"message">>=><<"OK">>}, Map),
  Body = erljson:encode(Map1),
  reply(Body, 200, Req, State);
reply_get({ok,Tickets}, Req, State, seated) ->
  {_, Map} =
    lists:foldl(
      fun([Id, Row, Seat], {Ctr, Acc}) ->
        Nr =integer_to_binary(Ctr),
        Key = <<"id_", Nr/binary>>,
        Acc1 = maps:merge(Acc, #{Key=>#{<<"ticket_id">>=>Id,
                                        <<"row">>=> Row,
                                        <<"seat">>=>Seat}}),
        {Ctr+1, Acc1}
      end,
      {0,#{}},
      Tickets
    ),
    Map1 = #{<<"message">>=><<"OK">>, <<"tickets">> => Map},
  Body = erljson:encode(Map1),
  reply(Body, 200, Req, State);
reply_get({error,Reason}, Req, State,_) ->
  Body = erljson:encode(
    #{<<"error">>=>atom_to_binary(Reason, latin1)}
  ),
  reply(Body, 406, Req, State).

handle_submit(Req,State) ->
  {Path, _} = cowboy_req:path(Req),
  post(Path, Req, State).

post(_Path = <<"/populate">>, Req, State) ->
  Rows = get_qs(Req, <<"rows">>, 20),
  Seats = get_qs(Req, <<"seats">>, 20),
  TotalTickets = get_qs(Req, <<"non_seated">>, 100),
  Tickets = [{seated, {Rows, Seats}}, {non_seated, TotalTickets}],
  ok = ticket_storage:populate(Tickets),
  Body = erljson:encode(#{<<"message">>=><<"Database populated">>}),
  reply(Body, 200, Req, State);
post(_Path = <<"/buy/seated">>, Req, State) ->
  {ok, ReqBody, Req1} = cowboy_req:body_qs(Req),
  %@todo add buy_batch to ticket_seller and limit max amount of tickets per order
  Fun = fun({_, Id}, Acc) ->
          Id1 = binary_to_integer(Id),
          Result =
            case ticket_seller:buy(seated, Id1) of
              ok ->
                <<"ok">>;
              {error, Reason} ->
                atom_to_binary(Reason, latin1)
            end,
          maps:merge(Acc, #{Id=>Result})
        end,
  Message = lists:foldl(Fun, #{}, ReqBody),
  Body = erljson:encode(#{<<"message">>=>Message}),
  reply(Body, 200, Req1, State);
post(_Path = <<"/buy/non_seated">>, Req, State) ->
  {ok, ReqBody, Req1} = cowboy_req:body_qs(Req),
  Fun = fun({_, Id}, Acc) ->
          Id1 = binary_to_integer(Id),
          Result =
            case ticket_seller:buy(non_seated, Id1) of
              ok ->
                <<"ok">>;
              {error, Reason} ->
                atom_to_binary(Reason, latin1)
            end,
          maps:merge(Acc, #{Id=>Result})
        end,
  Message = lists:foldl(Fun, #{}, ReqBody),
  Body = erljson:encode(#{<<"message">>=>Message}),
  reply(Body, 200, Req1, State);
post(_Path, Req, State) ->
  Body = <<"{}">>,
  reply(Body, 404, Req, State).

get_qs(Req, Key, Default) ->
  {Number, _} = cowboy_req:qs_val(Key, Req, Default),
  case is_binary(Number) of
    true ->
      binary_to_integer(Number);
    false ->
      Default
  end.

reply(Body, Code, Req, State) ->
  Req1 = cowboy_req:set_resp_body(Body, Req),
  {ok, Req2} = cowboy_req:reply(Code, [{<<"connection">>, <<"close">>}], Req1),
  {halt, Req2, State}.

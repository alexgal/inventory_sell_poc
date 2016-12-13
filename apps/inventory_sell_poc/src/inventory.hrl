-record(tickets, {id,
                  name}).
-record(rows, {id,
               ticket_id,
               seats}).
-record(seated_tickets, {ticket_id,
                         row_id,
                         seat,
                         is_bought}).
-record(non_seated_tickets, {ticket_id,
                             amount,
                             is_bought}).


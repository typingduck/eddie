-define(server_name, "Eddie - Intelligent Gateway/1.1").

-define(html_status(Code),
	case srv_table:lookup({http,status,Code}) of
	    {_,_,Phrase} ->
		["HTTP/1.0 ", Code, " ", Phrase,
		 "\r\nDate: ",
		 srv_parse:enc_current_date(),
		 "\r\n",
		 "Server: ",
		 ?server_name,
		 "\r\nContent-type: text/plain\r\n",
		 "\r\n",
		 Code, " ", Phrase, "\r\n"];
	    _ ->
		["500 Internal Server Error",
		 "\r\nDate: ",
		 srv_parse:enc_current_date(),
		 "\r\n",
		 "Server: ",
		 ?server_name,
		 "\r\nContent-type: text/plain\r\n",
		 "\r\n",
		 "500 Internal Server Error\r\n"]
	end).

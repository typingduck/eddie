-module(eddie_rootdir).
-author('tobbe@eddieware.org').
%%% --------------------------------------------------------------------
%%% Created : 26 Mar 1999 by tobbe@eddieware.org
%%% Function: Write the Erlang root dir to the file: root_dir.result
%%% --------------------------------------------------------------------
-vc('$Id: eddie_rootdir.erl,v 1.1 2000/10/27 22:20:15 dredd Exp $ ').
-export([start/1]).

start([ResultFile]) when atom(ResultFile) ->
    case file:open(atom_to_list(ResultFile),[write,raw]) of
	{ok,Fd} ->
	    file:write(Fd,code:root_dir()),
	    file:sync(Fd),
	    file:close(Fd);
	_ ->
	    false
    end,
    halt().


		    

%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is Erfs-0.2b1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (c), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : erfs_file.erl
%%% Author  : Anders Dahlin <anders@erlang.ericsson.se>
%%% Purpose : Eddie Replicated File System file utilities
%%% Created :  5 Nov 1998 by Anders Dahlin <anders@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(erfs_file).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


%%% API Exports
-export([make_dir/1, make_dir/2, del_dir/1, del_dir/2, list_dir/1]).
-export([delete/1, copy/2]).
-export([read_info/1, read_info/2, write_info/2, dirname/1]).
-export([read/1, write/2, set_cwd/1, get_cwd/1, join_paths/1, join_paths/2]).


-include_lib("kernel/include/file.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func    : make_dir/1,2
%% Args    : Dir | {Node, Dir}
%%           Opts = recurse
%% Purpose : Create a directory, local or remote
%% Returns : ok | {error, Reason}
%%----------------------------------------------------------------------
make_dir({Node, DirName}) ->
    result(rpc:call(Node, file, make_dir, [DirName]));
make_dir(Dir) ->
    result(file:make_dir(Dir)).
	

make_dir(Dir, recurse) ->
    mkdir(rm_trailing_slash(Dir)).

mkdir(Dir) ->
    DirName = if tuple(Dir) -> element(2, Dir); true -> Dir end,
    Parent = dirname(Dir),
    case exist(Parent) of
	true ->
	    case Dir of
		{Node, DirName} ->
		    result(rpc:call(Node, file, make_dir, [DirName]));
		Dir ->
		    result(file:make_dir(Dir))
	    end;
	false when DirName =/= "/", DirName =/= "." ->
	    case mkdir(Parent) of
		ok ->
		    mkdir(Dir);
		{error, Error} ->
		    {error, Error}
	    end;
	false ->
	    {error, {"top level does not exist", DirName}};
	{error, Error} ->
	    {error, Error}
    end.


%%----------------------------------------------------------------------
%% Func    : del_dir/1,2
%% Args    : Dir | {Node, Dir}  
%%           Opts = recurse
%% Purpose : Delete a directory, local or remote
%% Returns : ok | {error, Reason}
%%----------------------------------------------------------------------
del_dir({Node, Dir}) ->
    result(rpc:call(Node, file, del_dir, [Dir]));
del_dir(Dir) ->
    result(file:del_dir(Dir)).

del_dir(Dir, recurse) ->
    rmdir(rm_trailing_slash(Dir)).


rmdir(Dir) ->
    case list_dir(Dir) of
	{ok, []} ->
	    del_dir(Dir);
	{ok, FileList} ->
	    {ok, OrgDir} = get_cwd(Dir),
	    set_cwd(Dir),
	    rmdir(Dir, FileList),
	    set_cwd(OrgDir),
	    del_dir(Dir);
	Other ->
	    Other
    end.


rmdir({Node, Dir}, [FileOrDir | FileList]) ->
    case read_info({Node, FileOrDir}, type) of
	directory ->
	    rmdir({Node, FileOrDir});
	Type ->
	    result(rpc:call(Node, file, delete, [FileOrDir]))
    end,
    rmdir({Node, Dir}, FileList);

rmdir(Dir, [FileOrDir | FileList]) ->
    case read_info(FileOrDir, type) of
	directory ->
	    rmdir(FileOrDir);
	Type ->
	    result(file:delete(FileOrDir))
    end,
    rmdir(Dir, FileList);

rmdir(Dir, []) ->
    ok.

%%----------------------------------------------------------------------
%% Func    : delete/1
%% Args    : File | Dir | {Node, File | Dir}  
%% Purpose : Delete a file or directory, local or remote
%% Returns : ok | {error, Reason}
%%----------------------------------------------------------------------
delete(FileOrDir) ->
    case read_info(FileOrDir, type) of
	regular ->
	    case FileOrDir of
		{Node, File} ->
		    rpc:call(Node, file, delete, [File]);
		File ->
		    file:delete(File)
	    end;
	directory ->
	    del_dir(FileOrDir);
	Type ->
	    {error, {"not a file or directory", Type}}
    end.


%%----------------------------------------------------------------------
%% Func    : copy/2
%% Args    : From : File | Dir | Bin | {Node, File | Dir}  
%%           To   : File | Dir | {Node, File | Dir}
%% Purpose : Copy From to To, local or remote
%%           {Node, File} | File | Bin => File | {Node, File}
%%           {Node, Dir} | Dir         => Dir | {Node, Dir}
%% Returns : ok | {error, Reason}
%%----------------------------------------------------------------------
copy(Bin, Target) when binary(Bin) -> 
    write(Bin, Target);   
copy({Bin, Info}, Target) when binary(Bin) -> 
    write(Bin, Target),
    write_info(Info, Target);   
copy(Source, Target) ->    
    case read(Source) of
 	{ok, Bin} ->
	    write(Bin, Target),
	    write_info(read_info(Source), Target);   
 	{error, Error} ->
	    {error, Error}
     end.


%%----------------------------------------------------------------------
%% Func    : read/1
%% Args    : File | {Node, File}
%% Purpose : Read a file into binary
%% Returns : {ok, Binary} | {error, Error}
%%----------------------------------------------------------------------
read({Node, File}) ->
    result(rpc:call(Node, file, read_file, [File]));
read(File) ->
    result(file:read_file(File)).


%%----------------------------------------------------------------------
%% Func    : write/1
%% Args    : Bin
%%           File | {Node, File}
%% Purpose : Write the binary to a file
%% Returns : ok | {error, Error}
%%----------------------------------------------------------------------
write(Bin, {Node, File}) ->
    result(rpc:call(Node, file, write_file, [File, Bin]));
write(Bin, File) ->
    file:write_file(File, Bin).


%%----------------------------------------------------------------------
%% Func    : write_info/1
%% Args    : Info = #file_info
%%           File | Dir | {Node, File | Dir}
%% Purpose : Write the file_info struct
%% Returns : ok | {error, Error}
%%----------------------------------------------------------------------
write_info(Info, {Node, File}) ->
    result(rpc:call(Node, file, write_file_info, [File, Info]));
write_info(Info, File) ->
    result(file:write_file_info(File, Info)).


%%----------------------------------------------------------------------
%% Func    : read_info/1
%% Args    : File | Dir | {Node, File | Dir}
%% Purpose : Return the file_info struct
%% Returns : FileInfo | {error, Error}
%%----------------------------------------------------------------------
read_info({Node, File}) ->
    case rpc:call(Node, file, read_file_info, [File]) of
	{ok, FileInfo} -> FileInfo;
	{error, Error} -> {error, file:format_error(Error)};
	Other          -> {error, Other}
    end;
read_info(File) ->
    case file:read_file_info(File) of
	{ok, FileInfo} -> FileInfo;
	{error, Error} -> {error, file:format_error(Error)};
	Other          -> {error, Other}
    end.

%%----------------------------------------------------------------------
%% Func    : read_info/2
%% Args    : File (se read_info/1), Info = type | size | mtime
%% Purpose : Returns the information on whats specified 
%% Returns : Type | Size | Mtime | {error, Error}
%%           Type = regular | directory 
%%           Size = size of file in bytes
%%           Mtime = {{YYYY, M, D}, {H, M, S}}
%%----------------------------------------------------------------------
read_info(File, Info) ->
    case read_info(File) of
	{error, Error} ->
	    {error, Error};
	FileInfo ->
	    case Info of
		type  -> FileInfo#file_info.type;
		size  -> FileInfo#file_info.size;
		mtime -> FileInfo#file_info.mtime
	    end
    end.


%%----------------------------------------------------------------------
%% Func    : dirname/1
%% Args    : File | Dir | {Node, File | Dir}
%% Returns : Return the parent of a file or directoy
%%----------------------------------------------------------------------
dirname({Node, File}) ->
    {Node, dirname(File)};
dirname(File) ->
    filename:dirname(File).


%%----------------------------------------------------------------------
%% Func    : exist/1
%% Args    : File | Dir | {Node, File | Dir}
%% Purpose : Check if the file or directory exists
%% Returns : true | false | {error, Error}
%%----------------------------------------------------------------------
exist({Node, File}) ->
    exist_end(rpc:call(Node, file, read_file_info, [File]));
exist(File) ->
    exist_end(file:read_file_info(File)).

exist_end(Result) ->
    case Result of
	{ok, FileInfo}  -> true;
	{error, enoent} -> false;
	{error, eacces} -> true;
	{error, Error}  -> {error, file:format_error(Error)};
	Other           -> {error, Other}
    end.


%%----------------------------------------------------------------------
%% Func    : list_dir/1
%% Args    : File | Dir | {Node, File | Dir}
%% Purpose : List the contents of a directory
%% Returns : {ok, FilenameList} | {error, Error}
%%----------------------------------------------------------------------
list_dir({Node, Dir}) ->
    result(rpc:call(Node, file, list_dir, [Dir]));
list_dir(Dir) ->
    result(file:list_dir(Dir)).


%%----------------------------------------------------------------------
%% Func    : get_cwd/1
%% Args    : Node | {Node, _Any}
%% Purpose : Get the Current Working Directory on node Node
%% Returns : {ok, Dir}
%%----------------------------------------------------------------------
get_cwd({Node, _Any}) ->
    {ok, Dir} = rpc:call(Node, file, get_cwd, []),
    {ok, {Node, Dir}};
get_cwd(Node) when atom(Node) ->
    {ok, Dir} = rpc:call(Node, file, get_cwd, []),
    {ok, {Node, Dir}};
get_cwd(Dir) when list(Dir) ->
    file:get_cwd().


%%----------------------------------------------------------------------
%% Func    : set_cwd/1
%% Args    : Dir | {Node, Dir}
%% Purpose : Set the Current Working Directory (on node Node)
%% Returns : ok | {error, Error}
%%----------------------------------------------------------------------
set_cwd({Node, Dir}) ->
    result(rpc:call(Node, file, set_cwd, [Dir]));
set_cwd(Dir) ->
    result(file:set_cwd(Dir)).


%%----------------------------------------------------------------------
%% Func    : join_paths/1,2
%% Args    : Dir | {Node, Dir}
%% Purpose : Join two or more paths
%% Returns : {ok, Path} | {error, Error}
%%----------------------------------------------------------------------
join_paths(X, Y) ->
    join_paths([X, Y]).
join_paths([{Node, H} | T]) ->
    {Node, join_paths([H | T])};
join_paths([H | T]) ->
    filename:join([H| lists:map(fun rm_heading_slash/1, T)]).



%%----------------------------------------------------------------------
%% Funcs: Small utilities
%%----------------------------------------------------------------------

rm_trailing_slash([$/])  -> [];
rm_trailing_slash([$\\])  -> [];
rm_trailing_slash([E])   -> [E];
rm_trailing_slash([H|T]) -> [H | rm_trailing_slash(T)];
rm_trailing_slash({N,F}) -> {N, rm_trailing_slash(F)}.
    
rm_heading_slash([$/|T]) -> T;
rm_heading_slash(X) -> X.
	    
result(ok)             -> ok;
result({ok, Value})    -> {ok, Value};
result({error, Error}) -> {error, file:format_error(Error)};
result(Other)          -> {error, Other}.


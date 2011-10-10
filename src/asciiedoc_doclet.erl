%% The MIT License
%%
%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(asciiedoc_doclet).

-export([run/2, parse_xml/2, expand_text/2]).

-include_lib("xmerl/include/xmerl.hrl").


%%%===================================================================
%%% API
%%%===================================================================

run(Cmd, Ctxt) ->
    %% NOTE: Enable tracing to troubleshoot failures.
    %% user_default:dbgon(edoc_lib,write_file),
    meck:new(edoc_wiki, []),
    try
        meck:expect(edoc_wiki, parse_xml, fun parse_xml/2),
        meck:expect(edoc_wiki, expand_text, fun expand_text/2),
        %% NOTE: Enable edoc_doclet for HTML output
        %% edoc_doclet:run(Cmd, Ctxt)
        edown_doclet:run(Cmd, Ctxt)
    after
        meck:unload(edoc_wiki)
    end.

parse_xml(Data, Line) ->
    parse_xml_1(expand_text(Data, Line), Line).

parse_xml_1(Text, Line) ->
    Text1 = "<doc>" ++ Text ++ "</doc>",
    Opts = [{line, Line}, {encoding, 'iso-8859-1'}],
    case catch {ok, xmerl_scan:string(Text1, Opts)} of
        {ok, {E, _}} ->
            E#xmlElement.content;
        {'EXIT', {fatal, {Reason, L, _C}}} ->
            throw_error(L, {"XML parse error: ~p.", [Reason]});
        {'EXIT', Reason} ->
            throw_error(Line, {"error in XML parser: ~P.", [Reason, 10]});
        Other ->
            throw_error(Line, {"nocatch in XML parser: ~P.", [Other, 10]})
    end.

expand_text(Cs, L) ->
    DirName = filename:join(["/", "tmp", ?MODULE_STRING]),
    {A,B,C}=now(),
    Unique = lists:flatten(io_lib:format("~p-~p.~p.~p",[node(),A,B,C])),
    FileName = filename:join([DirName, Unique]),
    In = FileName ++ ".in",
    Out = FileName ++ ".out",
    ok = filelib:ensure_dir(In),
    ok = file:write_file(In, Cs),
    try
        {ok, _Ok} = asciidoc(In, Out, L),
        {ok, Res} = file:read_file(Out),
        binary_to_list(Res)
    after
        ok = file:delete(In),
        ok = file:delete(Out)
    end.

asciidoc(In, Out, _Line) ->
    Command = os:cmd("which asciidoc | tr -d \"\n\""),
    PortSettings = [exit_status, use_stdio,
                    {args, ["-s",
                            "-a", "data-uri",
                            "-a", "encoding=ISO-8859-1",
                            "-b", "xhtml11",
                            "-o", Out, In]}],
    Port = open_port({spawn_executable, Command}, PortSettings),
    case asciidoc_loop(Port, []) of
        {ok, _Output}=Ok ->
            Ok;
        {error, {_Rc, _Output}=Err} ->
            exit(Err)
    end.

asciidoc_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            asciidoc_loop(Port, ["\n"|[Line|Acc]]);
        {Port, {data, {noeol, Line}}} ->
            asciidoc_loop(Port, [Line|Acc]);
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.

-spec throw_error(non_neg_integer(), {string(), [_]}) -> no_return().
throw_error(L, D) ->
    throw({error, L, D}).

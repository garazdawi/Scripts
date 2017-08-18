#!/usr/bin/env escript
-module(asm_comp).

-mode(compile).

main(["parse", AsmFile, OpcodesH, Outputfile]) ->
    Data = get_asm_for_opcodes(AsmFile, get_opcodes(OpcodesH)),
    {ok, D} = file:open(Outputfile, [write]),
    file:write(D, term_to_binary(Data)),
    file:close(D);
main(["compare", Db1Name, Db2Name]) ->
    {ok, Db1Bin} = file:read_file(Db1Name),
    {ok, Db2Bin} = file:read_file(Db2Name),
    Db1 = binary_to_term(Db1Bin),
    Db2 = binary_to_term(Db2Bin),
    compare(Db1, Db2).

get_opcodes(File) ->
    {ok, Bin} = file:read_file(File),
    {match, Matches} = re:run(Bin,"^#define op_(.*)",
                              [{capture,all,binary},multiline,global]),
    lists:flatmap(
      fun([_,Match]) ->
              case string:prefix(Match,"count_") of
                  nomatch ->
                      [Name, Pos] = string:lexemes(Match," "),
                      {PosI, _} = string:to_integer(Pos),
                      [#{ name => Name, pos => PosI}];
                  _ ->
                      []
              end
      end, Matches).

get_asm_for_opcodes(AsmFile, Opcodes) ->
    {ok, Bin} = file:read_file(AsmFile),
    OpcodesWLabels = get_opcodes_labels(Bin, Opcodes),
    Labels = get_labels(Bin, OpcodesWLabels),
    [M#{ asm => maps:get(asm,mapfind(label, Label, Labels)) }
     || #{ label := Label} = M <- OpcodesWLabels].

get_opcodes_labels(Bin, Opcodes) ->
    {match, [{Start, Len}]} = re:run(Bin, "^opcodes.[0-9]+:$", [multiline]),
    get_opcodes_labels(string:slice(Bin, Start + Len),Opcodes,0).

get_opcodes_labels(_Bin, [], _Pos) ->
    [];
get_opcodes_labels(Bin, [#{ pos := P} = Op | T], Pos) ->
    P = Pos,
    {match,[{Start,Len}]} = re:run(Bin,".quad\t(.*)",[{capture, all_but_first}]),
    [Op#{ label => string:slice(Bin,Start,Len) } |
     get_opcodes_labels(string:slice(Bin,Start+Len), T, Pos+1)].

get_labels(Bin, Opcodes) ->
    Labels = [ Lbl || #{ label := Lbl } <- Opcodes],
    Re = ["^(\\",lists:join("|", Labels),"):$"],
    {match, Matches} = re:run(Bin, Re, [global, multiline]),
    split_asm(Bin, lists:sort([M || [_, M] <- Matches])).

split_asm(Bin, [{From,FromLen}]) ->
    [#{ label => binary:part(Bin, From, FromLen),
        asm => binary:part(Bin, From, 20) }];
split_asm(Bin, [{From,FromLen},{Until,_} = U|T]) ->
    [#{ label => binary:part(Bin, From, FromLen),
        asm => binary:part(Bin, From, Until - From) }
     | split_asm(Bin,[U|T])].

compare(Db1, Db2) ->
    do_compare(mapsort(name, Db1),mapsort(name, Db2)).

do_compare([ #{ label := Lbl1, name := Name, asm := A1 } | T1 ],
           [ #{ label := Lbl2,name := Name, asm := A2 } | T2]) ->
    io:format("~25s\t~p\t~p\t~p\t~6s ~6s~n",
              [Name, asm_len(A1), asm_len(A2),
               asm_len(A1) - asm_len(A2), Lbl1, Lbl2]),
    do_compare(T1, T2);
do_compare([ #{ label := Lbl1, name := Name1, asm := A1 } | T1 ],
           [ #{ name := Name2, asm := _A2 } | _] = T2) when Name1 < Name2 ->
    io:format("~25s\t~p\t\t\t~6s~n",[Name1, asm_len(A1), Lbl1]),
    do_compare(T1, T2);
do_compare([ #{ name := _Name1, asm := _A1 } | _] = T1,
           [ #{ label := Lbl2, name := Name2, asm := A2 } | T2])  ->
    io:format("~25s\t\t~p\t\t~6s ~6s~n",[Name2, asm_len(A2), "", Lbl2]),
    do_compare(T1, T2);
do_compare([], []) ->
    ok.

asm_len(Asm) ->
    case re:run(Asm,"^\t[a-z]",[multiline,global]) of
        {match, Matches} ->
            length(Matches);
        _ ->
            0
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%
mapsort(Key, Maps) ->
    lists:sort(fun(#{ Key := V1 }, #{ Key := V2 }) ->
                       V1 < V2
               end, Maps).

mapfind(Key, Value, [H|T]) ->
    case H of
        #{ Key := Value } ->
            H;
        _ ->
            mapfind(Key, Value, T)
    end;
mapfind(_, _, []) ->
    false.

#!/usr/bin/env escript
-module(asm_comp).

-mode(compile).

main(["parse", ERL_TOP, Outputfile]) ->
    OrigPwd = file:get_cwd(),
    file:set_cwd(ERL_TOP),
    Triple = string:trim(os:cmd("erts/autoconf/config.guess")),
    OpcodesH = filename:join([ERL_TOP,"erts","emulator",Triple,
                              "opt","smp","beam_opcodes.h"]),
    AsmFile = string:trim(os:cmd("mktemp")),

    %% First make everything so that we know that only beam_emu is made
    os:cmd("make emulator"),

    %% Touch and remake with V=1 in order to get correct arguments to gcc
    os:cmd("touch erts/emulator/beam/beam_emu.c"),
    VerboseMake = os:cmd("V=1 make emulator"),

    %% Create the correct gcc command line
    {match, [OrigCmd]} = re:run(VerboseMake,"gcc.*beam_emu.o",
                                [unicode,{capture,all,list}]),
    AsmCmd = re:replace(OrigCmd,"-o .*$",["-S -o ",AsmFile],[unicode,{return,list}]),

    %% Create the new asm
    io:format("Compiling .S file: ~s~n",[AsmCmd]),
    os:cmd(["cd erts/emulator && ",AsmCmd]),

    file:set_cwd(OrigPwd),
    main(["parse", AsmFile, OpcodesH, Outputfile]);
main(["parse", AsmFile, OpcodesH, Outputfile]) ->
    io:format("calling emu_comp.es ~s ~s ~s~n",[AsmFile, OpcodesH, Outputfile]),
    Data = get_asm_for_opcodes(AsmFile, get_opcodes(OpcodesH)),
    {ok, D} = file:open(Outputfile, [write]),
    file:write(D, term_to_binary(Data)),
    file:close(D);
main(["compare", Db1Name, Db2Name]) ->
    main(["compare", Db1Name, Db2Name, "output.html"]);
main(["compare", Db1Name, Db2Name, OutFile]) ->
    {ok, Db1Bin} = file:read_file(Db1Name),
    {ok, Db2Bin} = file:read_file(Db2Name),
    Db1 = binary_to_term(Db1Bin),
    Db2 = binary_to_term(Db2Bin),
    compare(Db1, Db2, OutFile);
main(_) ->
    io:format("Usage: emu_comp.es parse ERL_TOP output.edb\n"
              "   or: emu_comp.es compare FIRST.edb SECOND.edb [output.html]\n"
              "\n"
              "Parse the beam_emu assembly file for data about opcodes.\n"
              " or \n"
              "Create a comparison in between two different beam_emu assembly files.\n"
              "\n"
              "Parsing:\n"
              "  Precondition: A autoconf:ed and configured otp src tree\n"
              "\n"
              "  You only need to give the ERL_TOP path to the otp src\n"
              "  that you want to parse. The script should be able to\n"
              "  compile and parse the files it needs without any help.\n"
              "  If the wrong files are parsed (say you want to parse the\n"
              "  debug emulator for some reason), it is possible to give\n"
              "  the script the beam_emu.S and beam_opcodes.h like this:\n"
              "    emu_comp.es beam_emu.S beam_opcodes.h output.edb\n"
              "\n"
              "\n"
              "Comparing:\n"
              "  Precondition: Two parsed beam_emu.S in the form of edb files\n"
              "\n"
              "  The FIRST argument is considered to be the original beam_emu\n"
              "  and the SECOND is the new with (hopefully) the improvements in.\n"
              "\n"
              "Example:\n"
              "  emu_comp.es parse otp_orig orig.edb\n"
              "  emu_comp.es parse otp new.edb\n"
              "  emu_comp.es compare orig.edb new.edb\n"
             ).


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


%% Below are the functions that are used to compare and generate the html
compare(Db1, Db2, HtmlFile) ->
    Opcodes = merge_opcodes(mapsort(name, Db1), mapsort(name, Db2)),
    file:write_file(HtmlFile,
                    [header(),
                     body(Opcodes, comparison(Opcodes)),
                     footer()]).

merge_opcodes([ #{ label := Lbl1, name := Name, asm := A1 } | T1 ],
              [ #{ label := Lbl2, name := Name, asm := A2 } | T2]) ->
    [make_opcode(Name, Lbl1, Lbl2, A1, A2) | merge_opcodes(T1, T2)];
merge_opcodes([ #{ label := Lbl1, name := Name1, asm := A1 } | T1 ],
              [ #{ name := Name2, asm := _A2 } | _] = T2) when Name1 < Name2 ->
    [make_opcode(Name1, Lbl1, A1, undefined, undefined) | merge_opcodes(T1, T2)];
merge_opcodes([ #{ name := _Name1, asm := _A1 } | _] = T1,
              [ #{ label := Lbl2, name := Name2, asm := A2 } | T2])  ->
    [make_opcode(Name2, undefined, undefined, Lbl2, A2) | merge_opcodes(T1, T2)];
merge_opcodes([], []) ->
    [].

make_opcode(Name, L1, A1, undefined, undefined) ->
    #{ name => Name,
       first => #{ label => L1, asm => A1}
     };
make_opcode(Name, undefined, undefined, L2, A2) ->
    #{ name => Name,
       second => #{ label => L2, asm => A2}
     };
make_opcode(Name, L1, L2, A1, A2) ->
    #{ name => Name,
       first => #{ label => L1, asm => A1},
       second => #{ label => L2, asm => A2 }
     }.

%% This function creates one file for each opcode in /tmp/ for both of ther
%% versions and then does a `diff` on them. The output from the diff is returned.
comparison(Opcodes) ->
    TmpDir = string:trim(os:cmd("mktemp -d")),
    D1 = filename:join(TmpDir,"1"),
    file:make_dir(D1),
    D2 = filename:join(TmpDir,"2"),
    file:make_dir(D2),
    write_opcodes(Opcodes, D1, D2),
    Diff = os:cmd("diff -N -u " ++ D1 ++ " " ++ D2),
    re:replace(Diff,D1++"/|"++D2++"/","",[multiline,global]).

write_opcodes([ #{ name := Name,
                   first := #{ asm := A1 },
                   second := #{ asm := A2 } } | T ],
              D1, D2) ->
    file:write_file(filename:join(D1,Name), asm_strip(A1)),
    file:write_file(filename:join(D2,Name), asm_strip(A2)),
    write_opcodes(T, D1, D2);
write_opcodes([ #{ name := Name, first := #{ asm := A1 } } | T ], D1, D2) ->
    file:write_file(filename:join(D1,Name), asm_strip(A1)),
    write_opcodes(T, D1, D2);
write_opcodes([ #{ name := Name, second := #{ asm := A2 } } | T ], D1, D2)  ->
    file:write_file(filename:join(D2,Name), asm_strip(A2)),
    write_opcodes(T, D1, D2);
write_opcodes([], _, _) ->
    [].

asm_len(Asm) ->
    case re:run(Asm,"^\t[a-z]",[multiline,global]) of
        {match, Matches} ->
            length(Matches);
        _ ->
            0
    end.

asm_strip(Asm) ->
    NoLabels = re:replace(Asm,"^\\t?\\..*","",[multiline,global]),
    NoComments = re:replace(NoLabels,"\\t#.*","",[multiline,global]),
    strip_double_newlines(NoComments).

strip_double_newlines(Text) ->
    case re:replace(Text,"\\n *\\n","\n",[multiline,global]) of
        Text ->
            Text;
        NewText ->
            strip_double_newlines(erlang:iolist_to_binary(NewText))
    end.

header() ->
    ["<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <!-- CSS -->",
     css_link("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css",
              "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7"),
     css_link("https://cdnjs.cloudflare.com/ajax/libs/jquery.tablesorter/2.28.15/css/theme.dark.min.css",
              "sha256-pZnAlEAdMsKGZa1Q+WF2Y8EF5hYdXyE9SewMktdANwA="),
     css_link("https://cdnjs.cloudflare.com/ajax/libs/diff2html/2.3.0/diff2html.min.css",
              "sha256-HXXk7bgd2iz15UVkGyhtNXa9MRgnjYHdCwtQvLsE2Ck="),
     "
    <!-- Javascripts -->
",
     js_link("https://cdnjs.cloudflare.com/ajax/libs/jquery/3.2.1/jquery.min.js",
             "sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4="),
     js_link("https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js",
             "sha256-/BfiIkHlHoVihZdc6TFuj7MmJ0TWcWsMXkeDFwhi0zw="),
     js_link("https://cdnjs.cloudflare.com/ajax/libs/jquery.tablesorter/2.28.15/js/jquery.tablesorter.js",
             "sha256-AjKBsg6NJq2LBuNqpl8JRRIoMxatNZ/JkK1yWYbOhQc="),    
     js_link("https://cdnjs.cloudflare.com/ajax/libs/diff2html/2.3.0/diff2html.js",
             "sha256-4caLkzTJfAL8kP6TtC0zh8DLiT7MC9KOlVcU5RuFIT0="),
     js_link("https://cdnjs.cloudflare.com/ajax/libs/diff2html/2.3.0/diff2html-ui.min.js",
             "sha256-OrGLYrMJmpNSQzNeRnByAxVY3npgIT6Os98GW87iDrw="),
  "</head>"].

css_link(Url,Sha) ->
    ["<link rel=\"stylesheet\" href=\"",Url,"\" integrity=\"",Sha,"\" crossorigin=\"anonymous\">\n"].

js_link(Url,Sha) ->
    ["<script src=\"",Url,"\" integrity=\"",Sha,"\" crossorigin=\"anonymous\"></script>"].


body(Opcodes, Diff) ->
    NewLines = re:replace(Diff,"\n","\\\\n",[multiline,global]),
    Quotes = re:replace(NewLines,"\"","\\\\\"",[multiline,global]),
    ["<body>",
     table(Opcodes),
     "<div id=\"html-target-elem\"/></body>",
     "<script type=\"text/javascript\">",
     io_lib:format("var diff = \"~s\";",[Quotes]),
     "var diff2htmlUi = new Diff2HtmlUI({diff: diff});
  /* A special template is used to make the id something that we can jump to */
  var template = `<div id=\"{{{file.newName}}}\" class=\"d2h-file-wrapper\" data-lang=\"{{file.language}}\">
    <div class=\"d2h-file-header\">
      {{{filePath}}}
    </div>
    <div class=\"d2h-files-diff\">
        <div class=\"d2h-file-side-diff\">
            <div class=\"d2h-code-wrapper\">
                <table class=\"d2h-diff-table\">
                    <tbody class=\"d2h-diff-tbody\">
                    {{{diffs.left}}}
                    </tbody>
                </table>
            </div>
        </div>
        <div class=\"d2h-file-side-diff\">
            <div class=\"d2h-code-wrapper\">
                <table class=\"d2h-diff-table\">
                    <tbody class=\"d2h-diff-tbody\">
                    {{{diffs.right}}}
                    </tbody>
                </table>
            </div>
        </div>
    </div>
</div>`;
  diff2htmlUi.draw('#html-target-elem', { /* showFiles: true, */
                                          outputFormat: 'side-by-side',
                                          matching: 'words',
                                          rawTemplates: { 'side-by-side-file-diff': template }
                                        });
</script>"].

table(Opcodes) ->
    ["<table id = \"overview-table\" class=\"table table-condensed\">
        <thead>
          <tr>
            <th>instruction</th><th>First Length</th><th>Second Length</th><th>Diff</th>
          </tr>
        </thead>
        <tbody>",
     lists:map(fun table_rows/1, Opcodes),
     "  </tbody>
      </table>
      <script type=\"text/javascript\">
      $(function(){
        $(\"#overview-table\").tablesorter();
      });
      </script>"].

table_rows(#{ name := Name,
                first := #{ asm := A1 },
                second := #{ asm := A2 } }) ->
    ["<tr>
        <td><a href=\"#",Name,"\">",Name,"</a></td>
        <td>",integer_to_list(asm_len(A1)),"</td>
        <td>",integer_to_list(asm_len(A2)),"</td>
        <td>",integer_to_list(asm_len(A1) - asm_len(A2)),"</td>
      </tr>"];
table_rows(#{ name := Name, first := #{ asm := A1 } }) ->
    ["<tr>
        <td><a href=\"#",Name,"\">",Name,"</a></td>
        <td>",integer_to_list(asm_len(A1)),"</td>
        <td></td>
        <td></td>
      </tr>"];
table_rows(#{ name := Name, second := #{ asm := A2 } })  ->
    ["<tr>
        <td><a href=\"#",Name,"\">",Name,"</a></td>
        <td></td>
        <td>",integer_to_list(asm_len(A2)),"</td>
        <td></td>
      </tr>"];
table_rows([]) ->
    [].

footer() ->
    "</html>".

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

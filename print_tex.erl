-module(print_tex).

-compile(export_all).

print_sections(Sections) ->
    string:join([print_section(Section) || Section <- Sections],
                "~n~n").


print_section(Section) ->
    {{name, SecName},
     {style, Style},
     {highlight, Workout_highlight},
     {pic_steps, Pic_steps},
     {explain, Explain}} = Section,
    Sec_title = gen_sec_title(SecName),
    Style_dec = gen_style_dec(Style),
    Workout_highlight_dec = gen_workout_highlight_dec(Workout_highlight),
    Pic_steps_dec = gen_pic_steps_dec(Pic_steps),
    Workout_explain = gen_workout_explain(Explain),
    string:join([Sec_title, Style_dec, Workout_highlight_dec,
                 Pic_steps_dec, Workout_explain],
                "").
    
gen_sec_title(SecName) ->    
    string:join(["\\section{",
                 SecName,
                 "}\n"], "").

gen_style_dec(Style) ->
    string:join(["{\\bfseries 站架：}",
                 Style,
                 "\n\n"],
                "").

gen_workout_highlight_dec(Wh) ->
    string:join(["{\\bfseries 动作要领：}",
                 Wh, "\n"], "").

gen_pic_steps_dec(Pic_steps) ->
    Item_strings = [string:join(["\\item ", S], "") || {_, S} <- Pic_steps],
    Item_string = string:join(Item_strings, "\n"),
    string:join(["\\begin{enumerate} \\setlength{\\itemsep}{-\\itemsep}\n",
                 Item_string,
                 "\n\\end{enumerate}\n\n"], "").

gen_workout_explain(Explain) ->
    string:join(["{\\bfseries 动作说明：}", Explain], "").


dump(String) ->
    Bin = unicode:characters_to_binary(String, unicode),
    io:format("~ts~n", [Bin]).

dump(String, Fn) ->
    {ok, IO} = file:open(Fn, [write]),
    Bin = unicode:characters_to_binary(String, unicode),
    io:format(IO, "~s", [Bin]).

-module(jiao_parse).

-export([scan_and_parse/1]).

-export_type([section/0]).

-type raw_text() :: [char()].

-type section() :: {Name::{name, raw_text()},
                    Style::{style, raw_text()},
                    Highlight::{highlight, raw_text()},
                    Pic_Steps::{pic_steps,[{Pic_number::integer(),
                                            State::raw_text()}]},
                    Explain::{explain, raw_text()}}.

parse(Toks) ->
    {Sections, _} = parse_multiple(fun parse_section/1, Toks, []),
    Sections.

parse_section(Toks) ->
    {R1, _T1} = wait_for(Toks, order_number),
    {SecName, R2, _T2} = fetch_until(R1, [colon]),
    {Style, [{colon}|R3], _T3} = fetch_until(R2, [workout_highlight]),
    {Highlight, R4, T4} = fetch_until(R3, [pic_item]),
    {Pic_steps, R5} = parse_pic_items([T4|R4]),
    {[{colon}|R6], _T5} = wait_for(R5, workout_explain),
    {Explain, R7, T6} = fetch_until(R6, [order_number]),
    {{{name, SecName},
      {style, Style},
      {highlight, Highlight},
      {pic_steps, Pic_steps},
      {explain, Explain}},
     [T6|R7]}.

scan_and_parse(Fn) ->
    String = load_string_from_file(Fn),
    {ok, Toks, _} = jiao_tok:string(String),
    parse(Toks).

load_string_from_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    unicode:characters_to_list(Data, unicode).

wait_for([Tok|RemToks], TokType) when is_tuple(Tok), element(1, Tok) =:= TokType ->
    {RemToks, Tok}.
    
fetch_until(Toks, TokTypes) ->
    fetch_until([], Toks, TokTypes).

fetch_until(AggList, [], _TokTypes) ->
    {lists:reverse(AggList), [], none};
fetch_until(AggList, [Tok|RemTok], TokTypes) when is_tuple(Tok) ->
    Tp =  element(1, Tok),
    case lists:member(Tp, TokTypes) of
        true ->
            {lists:reverse(AggList), RemTok, Tok};
        false ->
            {text, [C]} = Tok,
            fetch_until([C|AggList], RemTok, TokTypes)
    end.

parse_pic_items(Toks) ->
    parse_multiple(fun parse_pic_item/1, Toks, []).

parse_pic_item(Toks) ->
    {[{colon}|R1], PicNumber} = wait_for(Toks, pic_item),
    {State, R2, T} = fetch_until(R1, [pic_item, workout_explain]),
    {{PicNumber, State}, [T|R2]}.

parse_multiple(F, Toks, Result) ->
    try F(Toks) of
        {Var, R} -> parse_multiple(F, R, [Var|Result])
    catch
        _:_ -> {lists:reverse(Result), Toks}
    end.

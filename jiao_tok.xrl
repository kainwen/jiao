Definitions.


Rules.

动作要领         : {token, {workout_highlight}}.
动作说明         : {token, {workout_explain}}.

图[0-9]+        : {token, {pic_item, get_number_from_rawtext(TokenChars)}}.

[一二三四五六七八九十][.]           : {token, {order_number}}.
[1-9][0-9]*[.]          : {token, {order_number}}.

：              : {token, {colon}}.


\t              : skip_token.
\n              : skip_token.
\s              : skip_token.

.               : {token, {text, TokenChars}}.

Erlang code.

get_number_from_rawtext([$图|S]) ->
   list_to_integer(S).
                                        
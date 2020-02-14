-module(euaparser).

-export([parse/1]).
-include("euaparser.hrl").

-spec parse(binary()) -> [{os|browser, [{atom(), ua_value()}]}].
parse(UserAgent) when is_list(UserAgent) ->
  parse(iolist_to_binary(UserAgent));
parse(UserAgent) when is_binary(UserAgent) ->
  Lower = list_to_binary(string:to_lower(binary_to_list(UserAgent))),
  [
    {browser, euaparser_browser:parse(Lower)},
    {os, euaparser_os:parse(Lower)}
  ].

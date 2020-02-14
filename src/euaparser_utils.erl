-module(euaparser_utils).
-include("euaparser.hrl").
-export([
  bin_to_num/1,
  jsx_to_browser/1,
  jsx_to_os/1,
  load_browsers/0,
  load_operating_systems/0
]).

-type jsx_basic() :: number() | binary().
-type jsx_array() :: [jsx_basic()|jsx_array()|jsx_struct()].
-type jsx_struct() :: {[{binary(), jsx_basic() | jsx_struct() | jsx_array()}]}.
-type jsx_term() :: jsx_basic()|jsx_array()|jsx_struct().

-spec jsx_to_browser(jsx_struct()) -> #browser{}.
jsx_to_browser(Browser) ->
  lists:foldl(fun jsx_to_browser/2, #browser{}, Browser).

-spec jsx_to_browser({binary(), jsx_term()}, #browser{}) -> #browser{}.
jsx_to_browser({<<"family">>, Family}, Browser) ->
  Browser#browser{family = binary_to_atom(Family, utf8)};
jsx_to_browser({<<"manufacturer">>, Manufacturer}, Browser) ->
  Browser#browser{manufacturer = binary_to_atom(Manufacturer, utf8)};
jsx_to_browser({<<"name">>, Name}, Browser) ->
  Browser#browser{name = Name};
jsx_to_browser({<<"aliases">>, Aliases}, Browser) ->
  Browser#browser{aliases = Aliases};
jsx_to_browser({<<"exclusions">>, Exclusions}, Browser) ->
  Browser#browser{exclusions = Exclusions};
jsx_to_browser({<<"browser_type">>, BrowserType}, Browser) ->
  Browser#browser{browser_type = binary_to_atom(BrowserType, utf8)};
jsx_to_browser({<<"rendering_engine">>, RenderingEngine}, Browser) ->
  Browser#browser{renderer = binary_to_atom(RenderingEngine, utf8)};
jsx_to_browser({<<"children">>, Children}, Browser) ->
  Browser#browser{children = lists:map(fun jsx_to_browser/1, Children)};
jsx_to_browser({<<"version_regex">>, VersionRegex}, Browser) ->
  {ok, Compiled} = re:compile(VersionRegex, [unicode]),
  Browser#browser{version_regex = Compiled}.

-spec jsx_to_os(jsx_struct()) -> #os{}.
jsx_to_os(OS) ->
  lists:foldl(fun jsx_to_os/2, #os{}, OS).

-spec jsx_to_os({binary(), jsx_term()}, #os{}) -> #os{}.
jsx_to_os({<<"family">>, Family}, OS) ->
  OS#os{family = binary_to_atom(Family, utf8)};
jsx_to_os({<<"manufacturer">>, Manufacturer}, OS) ->
  OS#os{manufacturer = binary_to_atom(Manufacturer, utf8)};
jsx_to_os({<<"name">>, Name}, OS) ->
  OS#os{name = Name};
jsx_to_os({<<"aliases">>, Aliases}, OS) ->
  OS#os{aliases = Aliases};
jsx_to_os({<<"exclusions">>, Exclusions}, OS) ->
  OS#os{exclusions = Exclusions};
jsx_to_os({<<"device_type">>, DeviceType}, OS) ->
  OS#os{device_type = binary_to_atom(DeviceType, utf8)};
jsx_to_os({<<"children">>, Children}, OS) ->
  OS#os{children = lists:map(fun jsx_to_os/1, Children)};
jsx_to_os({<<"version_regex">>, VersionRegex}, OS) ->
  {ok, Compiled} = re:compile(VersionRegex, [unicode]),
  OS#os{version_regex = Compiled}.

-spec bin_to_num(Bin :: binary()) -> number().
bin_to_num(Binary) when is_binary(Binary) ->
  {ok, C} = re:compile("\\.", [unicode]),
  List = re:replace(Binary, C, "", [{return, list}]),
  case string:to_float(List) of
    {error, no_float} -> list_to_integer(List);
    {F, _Rest} -> F
  end.

-spec load_operating_systems() -> [#os{}].
load_operating_systems() ->
  {ok, C} = read_priv_file("operating_systems.json"),
  lists:map(fun jsx_to_os/1, jsx:decode(C)).

-spec load_browsers() -> [#browser{}].
load_browsers() ->
  {ok, C} = read_priv_file("browsers.json"),
  lists:map(fun euaparser_utils:jsx_to_browser/1, jsx:decode(C)).

-spec read_priv_file(Filename :: string()) -> {'ok', binary()} | {'error', atom()}.
read_priv_file(Filename) ->
  Dir = case code:priv_dir(euaparser) of
          {error, bad_name} ->
            "priv/";
          D ->
            D
        end,
  Path = filename:join(Dir, Filename),
  file:read_file(Path).

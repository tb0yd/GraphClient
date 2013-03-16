-module(shortest).

%% API
-export([add_path/2]).

%% throw away 0- or 1-length Paths
add_path(_, []) ->
  ok;
add_path(_, [_]) ->
  ok;
add_path(Tab, Path) ->
  Shortest_paths = ets:lookup_element(Tab, lists:nth(1, Path), 4),
  Shortest_paths_with_same_ends = [X || X <- Shortest_paths, (lists:nth(1, Path) == lists:nth(1, X)) and (lists:last(Path) == lists:last(X))],
  add_path(Tab, Path, Shortest_paths_with_same_ends).

add_path(_, Path, [Shortest_path]) when (length(Shortest_path) =< length(Path)) ->
  ok;
add_path(Tab, Path, [Shortest_path]) ->
  Paths = ets:lookup_element(Tab, lists:nth(1, Path), 4),
  ets:update_element(Tab, lists:nth(1, Path), {4, [Path|lists:delete(Shortest_path, Paths)]}),
  ok;
add_path(Tab, Path, []) ->
  Paths = ets:lookup_element(Tab, lists:nth(1, Path), 4),
  ets:update_element(Tab, lists:nth(1, Path), {4, [Path|Paths]}),
  ok.


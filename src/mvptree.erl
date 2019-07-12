%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(mvptree).

-export([from_list/3, search/5]).

split([], _) ->
    [];
split(List, N) when length(List) =< N ->
    [List];
split(List, N) ->
    {List1, List2} = lists:split(N, List),
    [List1|split(List2, N)].

from_list(_, _, []) ->
    none;
from_list(M, Distance, [{Point, Value}|Rest]) ->
    {tree, {Point, Value},
     [ {element(1, hd(List)), element(1, lists:last(List)),
        from_list(M, Distance, [L || {_, L} <- List])}
       || List <-
              split(
                lists:keysort(1, [{Distance(Point, L), {L,V}} || {L, V} <- Rest]),
                case length(Rest) of
                    Len when Len rem M == 0 ->
                        Len div M;
                    Len ->
                        Len div M + 1
                end
               )
     ]}.

search(Distance, Loc, Limit, Default, Tree) ->
    {_, V} = search(Distance, Loc, {Limit, Default}, Tree),
    V.

search(_, _, State, none) ->
    State;
search(Distance, Loc, {Tau, Nearest}, {tree, {Point, Value}, []}) ->
    D = Distance(Point, Loc),
    if D =< Tau ->
            {D, Value};
       true ->
            {Tau, Nearest}
    end;
search(Distance, Loc, {Tau, Nearest}, {tree, {Point, Value}, [{Min, Max, Tree}]}) ->
    D = Distance(Point, Loc),
    {Tau1, _} = State1 =
        if D =< Tau ->
                {D, Value};
            true ->
                {Tau, Nearest}
        end,
    if D + Tau1 >= Min, D - Tau1 =< Max ->
            search(Distance, Loc, State1, Tree);
       true ->
            State1
    end;
search(Distance, Loc, {Tau, Nearest}, {tree, {Point, Value}, [{Min1, Max1, Tree1}, {Min2, Max2, Tree2}]}) ->
    D = Distance(Point, Loc),
    {Tau1, _} = State1 =
        if D =< Tau ->
                {D, Value};
            true ->
                {Tau, Nearest}
        end,
    Mid = (Max1 + Min2) / 2.0,
    if D < Mid ->
            if D + Tau1 >= Min1 ->
                    {Tau2, _} = State2 = search(Distance, Loc, State1, Tree1),
                    if D + Tau2 >= Min2 ->
                            search(Distance, Loc, State2, Tree2);
                       true ->
                            State2
                    end;
               true ->
                    State1
            end;
       true ->
            if D - Tau1 =< Max2 ->
                    {Tau2, _} = State2 = search(Distance, Loc, State1, Tree2),
                    if D - Tau2 =< Max1 ->
                            search(Distance, Loc, State2, Tree1);
                       true ->
                            State2
                    end;
               true ->
                    State1
            end
    end;
search(Distance, Loc, {Tau, Nearest}, {tree, {Point, Value}, SubTrees}) ->
    D = Distance(Point, Loc),
    search_subtrees(
      Distance,
      Loc,
      if D =< Tau ->
              {D, Value};
         true ->
              {Tau, Nearest}
      end,
      sort_subtree(SubTrees, [], D)).

search_subtrees(_, _, State, []) ->
    State;
search_subtrees(Distance, Loc, {Tau, Nearest}, [{MinD, Tree}|Rest]) ->
    if MinD =< Tau ->
            search_subtrees(
              Distance,
              Loc,
              search(Distance, Loc, {Tau,Nearest}, Tree),
              Rest);
       true ->
            {Tau, Nearest}
    end.

sort_subtree([], Acc, _) ->
    Acc;
sort_subtree([{_, Max, Tree}|Rest], Acc, D) when Max =< D ->
    sort_subtree(Rest, [{D-Max, Tree}|Acc], D);
sort_subtree([{Min, _, Tree}|Rest], Acc, D) when Min =< D ->
    sort_subtree(Rest, [{0.0, Tree}|Acc], D);
sort_subtree(List, Acc, D) ->
    merge(Acc, sort_subtree(List, D)).

sort_subtree([], _) ->
    [];
sort_subtree([{Min, _, Tree}|Rest], D) when Min > D ->
    [{Min-D, Tree}|sort_subtree(Rest, D)].

merge([], B) ->
    B;
merge(A, []) ->
    A;
merge([{K1,V1}|T1], [{K2,_}|_]=L2) when K1 =< K2->
    [{K1,V1}|merge(T1, L2)];
merge(L1, [H2|T2]) ->
    [H2|merge(L1,T2)].

-module(vptree_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [validate_infinite_distance, validate_finite_distance, empty_list].

rand_point() ->
    {rand:uniform(), rand:uniform()}.

distance({X1, Y1}, {X2, Y2}) ->
    X = X2 - X1,
    Y = Y2 - Y1,
    math:sqrt(X*X+Y*Y).

search(_, _, _, V, []) ->
    V;
search(Distance, Loc, Min, Value, [{Point, V}|T]) ->
    case Distance(Point, Loc) of
        D when D < Min ->
            search(Distance, Loc, D, V, T);
        _ ->
            search(Distance, Loc, Min, Value, T)
    end.

validate(M, Limit, Size, Times) ->
    rand:seed(exrop),
    Seed = rand:export_seed(),
    validate(M, Limit, Seed, Size, Times).

validate(M, Limit, Seed, Size, Times) ->
    rand:seed(Seed),
    io:format("m: ~p~nseed: ~p~n", [M, Seed]),
    Points = [ {rand_point(), make_ref()} || _ <- lists:seq(1,Size)],
    Tree =
        case M of
            2 ->
                vptree:from_list(fun distance/2, Points);
            _ ->
                mvptree:from_list(M, fun distance/2, Points)
        end,
    do_validate(Limit, Points, Tree, Times).

do_validate(_, _, _, 0) ->
    ok;
do_validate(Limit, Points, Tree, Times) ->
    Point = rand_point(),
    Expected = search(fun distance/2, Point, Limit, undefined, Points),
    Got = vptree:search(fun distance/2, Point, Limit, undefined, Tree),
    Expected = Got,
    do_validate(Limit, Points, Tree, Times-1).

validate_infinite_distance(_Config) ->
    [validate(M, 2.0, 10000, 5000) || M <- lists:seq(2,8)],
    ok.

validate_finite_distance(_Config) ->
    [validate(M, 0.2, 10000, 5000) || M <- lists:seq(2,8)],
    ok.

empty_list(_Config) ->
    Tree = vptree:from_list(fun distance/2, []),
    undefined = vptree:search(fun distance/2, {0.0,0.0}, 2.0, undefined, Tree),
    ok.

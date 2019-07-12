## vptree

[![Build Status](https://travis-ci.org/seniverse/vptree.svg?branch=master)](https://travis-ci.org/seniverse/vptree)
[![Coverage Status](https://coveralls.io/repos/github/seniverse/vptree/badge.svg?branch=master)](https://coveralls.io/github/seniverse/vptree?branch=master)
[![hex.pm version](https://img.shields.io/hexpm/v/vptree.svg)](https://hex.pm/packages/vptree)
[![hex.pm downloads](https://img.shields.io/hexpm/dt/vptree.svg)](https://hex.pm/packages/vptree)
![hex.pm license](https://img.shields.io/hexpm/l/vptree.svg)
![GitHub top language](https://img.shields.io/github/languages/top/seniverse/vptree.svg)

An Erlang implementation of vantage point tree.

### Usage

Define a distance function

```
1> Distance = fun({X1,Y1},{X2,Y2}) -> X = X2-X1, Y = Y2-Y1, math:sqrt(X*X+Y*Y) end.
```

Create vantage point tree from a list of points

```
2> Tree = vptree:from_list(Distance, [{{0.0, 0.0}, a}, {{1.0, 1.0}, b}]).
```

Find nearest point within a given distance

```
3>  vptree:search(Distance, {0.2,0.2}, 100.0, not_found, Tree).
a
4> vptree:search(Distance, {0.2,0.2}, 0.1, not_found, Tree).  
not_found
```

### Installation

add `vptree` dependency to your project's `rebar.config`

```
{deps, [vptree]}.
```


### License

vptree is released under Apache 2 License. Check [LICENSE](./LICENSE) file for more information.

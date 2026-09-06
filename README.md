# lilac

[![Clojars Project](https://img.shields.io/clojars/v/com.github.vssekorin/lilac.svg)](https://clojars.org/com.github.vssekorin/lilac)

A Lambda Calculus implementation in Clojure.

It provides a `λ` macro for
writing lambda terms directly, along with capture-avoiding β-reduction, and
a library of terms built on top of it: Church booleans, numerals, pairs,
and lists, arbitrary-precision-style integers and rationals, classic
combinators (Y, Θ, S, K, I, B, C, W, T, M, V), and conversions between
these terms and native Clojure values.

name: fermat
version: 1.0
description: Fermat's little theorem library
author: François Thiré <francois.thire@inria.fr>
license: MIT
show: "Fermat"

connectives
{
        article: "connectives.art"
}

leibniz
{
        article: "leibniz.art"
}

logic
{
        import: connectives
        article: "logic.art"
}

relations
{
        import: connectives
        import: logic
        article: "relations.art"
}

bool
{
        import: connectives
        import: leibniz
        import: logic
        import: relations
        article: "bool.art"
}

nat
{
        import: connectives
        import: leibniz
        import: logic
        import: relations
        import: bool
        article: "nat.art"
}

divmod
{
        import: connectives
        import: leibniz
        import: logic
        import: relations
        import: bool
        import: nat
        article: "div_mod.art"
}

bigops
{
        import: connectives
        import: logic
        import: relations
        import: bool
        import: leibniz
        import: nat
        import: divmod
        article: "bigops.art"
}

primes
{

        import: leibniz
        import: connectives
        import: logic
        import: relations
        import: bool
        import: nat
        import: divmod
        article: "primes.art"
}

cong
{
        import: leibniz
        import: connectives
        import: logic
        import: relations
        import: bool
        import: nat
        import: divmod
        import: primes
        article: "cong.art"
}

exp
{
        import: connectives
        import: logic
        import: leibniz
        import: relations
        import: bool
        import: nat
        article: "exp.art"
}

fact
{
        import: connectives
        import: leibniz
        import: logic
        import: relations
        import: nat
        article: "fact.art"
}

gcd
{
        import: connectives
        import: leibniz
        import: logic
        import: relations
        import: bool
        import: nat
        import: divmod
        import: primes
        article: "gcd.art"
}

permutation
{
        import: connectives
        import: leibniz
        import: logic
        import: relations
        import: bool
        import: nat
        article: "permutation.art"
}

sigmapi
{
        import: connectives
        import: logic
        import: bool
        import: nat
        import: exp
        import: bigops
        article: "sigma_pi.art"
}

fermat
{
        import: connectives
        import: logic
        import: relations
        import: bool
        import: nat
        import: permutation
        import: divmod
        import: primes
        import: bigops
        import: cong
        import: exp
        import: fact
        import: sigmapi
        import: gcd
        article: "fermat.art"
}

main
{
        import: fermat
}

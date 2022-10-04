# Optimizing Point Representation

We chose to convert to the twisted Edwards curve, rather than working in the
Weierstrass form, to reduce resource usage of the pipelined mixed adder. With
the tricks documented here, we managed to get down to `7M + 6A`, a very
big improvement over [`7M + 4S + 9A` required for Jacobian coordinates](https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-madd-2007-bl)

We did some modelling of all this in OCaml to validate all the requirements
for the transformations are met, and a point addition in twisted Edwards form do
indeed map to an addition in Weierstrass form (by using arkworks as a reference).
See [here](../../../libs/twisted_edwards/model) for our modelling tests.

## Converting Curve Parameters to Twisted Edwards

The transformation from the curve in Weierstrass form to twisted Edwards form is a
2-step process - First it needs to be transformed to the Montgomery form, only
then it can be transformed to the elliptic curve form. The [Wikipedia article on
Montgomery Curve](https://en.wikipedia.org/wiki/Montgomery_curve) has a good
explanation on this.

Here is a summary of the method.

An elliptic curve in Weierstrass form has the following formula:

```
y^2 = x^3 + ax + b

where a and b are the parameters of the curve
```

The Montgomery curve has the following formula:

```
By^2 = x^3 + (A * x^2) + x

where A and B are the parameters of the curve
```

An elliptic curve in Weierstrass form can be rewritten as a Montgomery
curve by transforming the parameters as follows (whenever the parameter alpha as defined
below exists - it does exist for BLS12-377).

```
A = 3 * alpha * s
B = s

where
alpha is a root of the equation x^3 + ax + b = 0
s = sqrt((3 * alpha^2) + a)^(-1)
```

Twisted Edwards curves have the following form:

```
(a * x^2) + y^2 = 1 + (d * x^2 * y^2)

where a and d are parameters of the curve
```

A Montgomery curve can be rewritten as a twisted Edwards curve with the following
formulae when `a != d`:

```
A = 2 * (a + d) / (a - d)
B = 4 / (a - d)
```

The linked Wikipedia article above goes into detail on when these parameter
transformations are valid. We validated that the required assumptions
do hold in BLS12-377, and hence can be represented as a twisted Edwards curve.

## Converting Points from Weierstrass to Twisted Edwards

The formulae for points conversion is detailed in the Wikipedia article
linked above. Here's a summary:

```
Given (x, y) on a curve in Weierstrass form:
x_montgomery = s * (x - alpha)
y_montgomery = s * y

Given (x, y) on a Montgomery curve:

x_twisted_edwards = x / y
y_twisted_edwards = (x - 1) / (x + 1)
```

The main catch here is the mapping for points from Weierstrass to twisted Edwards
is not always defined. The transformation for points from Montgomery curve
-> twisted Edwards is undefined when `y = 0` or `x = -1` on the Montgomery curve
representation. This implies there is no twisted Edwards curve representation
for points where `y = 0` or `x = alpha - s^(-1)`. Indeed there are exactly 5 such
points on the BLS12-377 curve.

In practice however, this is not a problem.

- The probability of these points occurring is miniscule, so we can fallback
to a slow code path when handling these points. In our implementation, we simply
offload these points to the CPU.
- It's unclear if these points lie in the G1 subgroup, so it's not clear if this case will ever
occur at all!

## Converting to Scaled Twisted Edwards Curve

A mixed addition on the scaled twisted Edwards curve [costs `8M + 1*a + 7A`](https://hyperelliptic.org/EFD/g1p/auto-twisted-extended.html#addition-madd-2008-hwcd-2). But with a simple scaling transformation, we can [reduce
this further to `7M + 1*k + 8A + 1*2`](https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html#addition-madd-2008-hwcd-3).
The reduced operation count applies to twisted Edwards curve with `a = -1`. We
can achieve that by transforming our coordinate system once more:

```
transform_twisted_edwards_to_scaled_twisted_edwards(u, v) -> (twisted_scale * u, v)

where twisted_scale = sqrt(((-3 * alpha * s) - 2) / s)
```

In our actual implementation, we go one step further, reducing the amount of work to `7M + 6A`
by exploiting very heavy precomputation. The key idea is that the two summands do not
need to be in the same coordinate space. The exact algorithm used for precomputation is
[described here in the documentation of the precompute mixed adder component](https://fyquah.github.io/hardcaml_zprize/zprize/Twisted_edwards_lib/Mixed_add_precompute/index.html)

While there is a [mixed addition formula for addition in scaled twisted Edwards form with `7M + 8A`](https://hyperelliptic.org/EFD/g1p/auto-twisted-extended-1.html#addition-madd-2008-hwcd-4), that formulae is not strongly unified. This means we need to specially handle identities and
cases where both points are equivalent (in affine coordinates). Although we are guaranteed 
that the input points do not contain infinites (also known as identity points), our intermediate result
and initial state can still contain infinities. Having a strongly unified adder makes dealing with
this a lot of this easier to reason about.

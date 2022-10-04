# Optimizing Point Representation

We chose to convert to the twisted edwards curve, rather than working in the
weistrass form, to reduce resource usage of the pipelined mixed adder. With
the tricks documented here, we managed to get down to `7M + 6A`, a very
big improvement over [`7M + 4S + 9A` required for Jacobian coordinates](https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-madd-2007-bl)

We did some modelling of all this in OCaml to validate all the requirements
for the transformations are met, and a point addition in twisted edwards form do
indeed map to an addition in weistrass form (by using arkworks as a reference).
See [here](../../../libs/twisted_edwards/model) for our modelling tests.

## Converting Curve Parameters to Twisted Edwards

The transformation from the curve in weistrass form to twisted edwards form is a
2-step process - First it needs to be transformed to the Montgomery form, only
then it can be transformed to the elliptic curve form. The [wikipedia article on
Montgomery Curve](https://en.wikipedia.org/wiki/Montgomery_curve) has a good
explaination on this.

Here's is a summary of the method:

Elliptic curve in weistrass form exhibits the following formula

```
y^2 = x^3 + ax + b

where a and b are the parameters of the curve
```

The montgomery curve has the following formula:

```
By^2 = x^3 + (A * x^2) + x

where A and B are the parameters of the curve
```

A elliptic curve in weistrass form can be rewritten as a formulae in montgomery
curve by rewriting the parameters as follows when certain conditions hold.
(Said conditions do hold in BLS12-377)

```
A = 3 * alpha * s
B = s

where
alpha is a root of the equation x^3 + ax + b = 0
s = sqrt((3 * alpha^2) + a)^(-1)
```

Twisted Edwards Curves has the following formulae

```
(a * x^2) + y^2 = 1 + (d * x^2 * y^2)

where a and d are parameters of the curve
```

A montgomery curve can be rewritten as a twisted edwards curve with the following
formulae when `a != d`:

```
A = 2 * (a + d) / (a - d)
B = 4 / (a - d)
```

The linked wikipedia article above goes into detail on when these parameter
transformations are valid. We validated that the required assumptions
do hold in bls12-377, and hence can be represented as a twisted edwards curve.

## Converting Points from Weistrass to Twisted Edwards

The formulae for points conversion is detailed in the wikipedia article
linked above. Here's a summary:

```
Given (x, y) on a curve in weistrass form:
x_montgomery = s * (x - alpha)
y_montgomery = s * y

Given (x, y) on a montgomery curve:

x_twisted_edwards = x / y
y_twisted_edwards = (x - 1) / (x + 1)
```

The main catch here is the mapping for points from weistrass to twisted edwards
is not always defined. The transformation for points from montgomery curve
-> twisted edwards is undefined when `y = 0` or `x = -1` on the montgomery curve
representation. This implies there is no twisted edwards curve representation
for points where `y = 0` or `x = alpha - s^(-1)`. Indeed there are exactly 5 such
points on the BLS12-377 curve.

In practice however, this is not a problem.

- The probability of these points occuring is very minisicue that we can fallback
to a slow code path when handling these points. In our implementation, we simply
offload these points to the CPU. 
- It's unclear if it can occur from repeated addition of the G1 Generator, so it's not clear if it and occur at all!

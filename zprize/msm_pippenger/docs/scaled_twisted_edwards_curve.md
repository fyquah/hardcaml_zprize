# Conversion to a Different Form

We chose to convert to the twisted edwards curve, rather than working in the
weistrass form, to reduce resource usage of the pipelined mixed adder. With
the tricks documented here, we managed to get down to `7M + 6A`, a very
big improvement over `11M + 8A` required for projective coordinates in
weistrass form.

## Conversion to Twisted Edwards Curve

The BLS curve in its vanilla form (Weistrass Form) is as follows:

```
y^2 = x^3 + 1

parameters:
a = 0
b = 1
```

We first convert it to a montgomery curve form

```
A = 3 * alpha * s
B = s

where
s = sqrt((3 * alpha^2) + a)^(-1)
```

Alas, to go from montgomery curve to the twisted edewards curve

```
a = P - 1
d = (2 - A) / (2 + A)
```

Converting affine points from the weistrass form to the twisted edwards curve
follows the similar to step process too:

```
```

In practice, we expect these numbers not to show up at all. Assuming a uniform
distribution, probability is minisiicue. In our implementation, the host driver
checks for the existence of these points, and offload their computation to the
host with the naive high-school multiplication algorithm.

## Reducing Work with Scaled Twisted Edwards Curve

## Tests

We did some modelling of all this in OCaml to convince ourselves that all this
is right, as well as compared

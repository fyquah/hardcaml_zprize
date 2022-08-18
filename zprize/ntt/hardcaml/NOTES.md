https://arxiv.org/pdf/2011.11524.pdf

Lots of relevant information about the ntt transform, including the 4 and 6 step variants.

-----------------------------------------------

https://www.davidhbailey.com/dhbpapers/fftq.pdf

Original paper on 4 step transform

-----------------------------------------------

https://cp4space.hatsya.com/2021/09/01/an-efficient-prime-for-number-theoretic-transforms/

Talks a bit about the mathematical basis of the field we are working in.

-----------------------------------------------

https://github.com/itzmeanjan/ff-gpu

GPU code which uses our prime field, and implements a 6 step fft algorithm
similar to what we want to consider.

-----------------------------------------------

https://eprint.iacr.org/2017/727.pdf

Pseudo code for the Gentleman-Sande INTT, which has more sensible bit reversed
addressing needs. Also the Cooley-turkey implementation stores and bit reverses
the W coefs rather than the input from what I can see (though in a forward
transform admitedly).

-----------------------------------------------

https://github.com/SRI-CSL/NTT
https://github.com/IBM/optimized-number-theoretic-transform-implementations
https://github.com/mhostetter/galois
https://github.com/shakes76/finite-transform-library
https://github.com/loretanr/NTT-ASL

Various references to be looked at more closely.


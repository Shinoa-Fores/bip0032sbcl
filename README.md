bip0032sbcl
===========

Implementation of BIP0032 (HD Wallets) in Steel Bank Common Lisp

This is an implementation of bip0032 (HD Wallets) in Steel Bank Common Lisp (sbcl).
It is intended to work on unix-based operating systems and delegates the computation
of hash functions to openssl, sha256sum and sha512sum.

Run:

./configure

This should create a file config.lisp which contains the location of the executables openssl, sha256sum and sha512sum.

Use sbcl to compile the relevant lisp files and create an executable:

sbcl --load make.lisp

The created executable is bip0032sbclexec.

./bip0032sbclexec help

=====================================

Example:

1. Create the root extended private key from some long random master
hex file (not the one used here, obviously).  This gives the extended
private key as the private key and the chain code, both in base58.

./bip0032sbclexec masterhex 497320697420747275653f

DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj
3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ

Alternatively, use a string which is interpreted as a sequence of bytes in ASCII.

./bip0032sbclexec master "Is it true?"

DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj
3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ

2. Used the private key from Step 1 to compute the corresponding
public key given as two base58 numbers x and y representing the point
(x,y).

./bip0032sbclexec pubkey DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj

9egqHyQ2U4FRJhD1TYv1T36cSZgi71ttEzLKJwRGiw7c
FjVLq81iEotBdEQxYgBZ9FCNGRTwYkwVKKnhAREf3YeR

3. Use the extended private key of the root (from Step 1) to compute
the extended private key of the child with index 7 from the root.

./bip0032sbclexec ckd DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj 3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ 7

aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt
FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi

4. Compute the corresponding public key of the private key computed in
Step 3.

./bip0032sbclexec pubkey aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt

CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE
4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw

5. Use the extended public key of the root (from Steps 1 and 2) to compute
the extended public key of the child with index 7 from the root.  Note
that the public key part matches the public key computed in Step 4,
and the chain code corresponds to the chain code computed in Step 3.

./bip0032sbclexec ckdpub 9egqHyQ2U4FRJhD1TYv1T36cSZgi71ttEzLKJwRGiw7c FjVLq81iEotBdEQxYgBZ9FCNGRTwYkwVKKnhAREf3YeR 3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ 7

CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE
4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw
FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi

6. Use the extended public key from Step 5 to generate 3 btc addresses
(starting at index 5).  The three addresses are returned on three
lines and the final line gives the next unused index.  Usually this
will be the starting index plus the number of addresses generated, but
sometimes an index is invalid and must be skipped.

./bip0032sbclexec btcaddrs CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE 4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi 5 3

1ELX99uVdVMoqDfqLS3px8mjD8DaVutjT1
1MvtMkHJCfDWi2kpSt7spHGpLibPuEkDaF
1QBTyb15HZskDsucGK9u69mDeNc7bytGxL
8

7. Use the extended private key from Step 3 to generate 3 btc
wif/address pairs (starting at index 5). The three wif/address pairs
are given over 6 lines (wif, then address).  The final line is the
next unused index. Note that the addresses correspond to those
generated in Step 6.

./bip0032sbclexec btcpairs aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi 5 3

5HrWRC7qL56h6ysGmxyyarqGN9td118DvamTMEAgRLrdxYnjFyQ
1ELX99uVdVMoqDfqLS3px8mjD8DaVutjT1
5JFRdAzSo2NFbXsLgvF24HbrqNy4r71haygSSoUt9aR4Rb84az2
1MvtMkHJCfDWi2kpSt7spHGpLibPuEkDaF
5KBe2BcaoR8Mx3Qj7AY3x6tQNpbodEKDz9yUEgk7XH6JVXaJmJP
1QBTyb15HZskDsucGK9u69mDeNc7bytGxL
8

8. Repeat the same process to obtain ltc and ftc addresses and wif/address pairs.

./bip0032sbclexec ltcaddrs CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE 4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi 5 3

LYZUQNDKi9bs62MzWa38E9qVRLarfARfDW
Lg9qcxb8HKTZxqSyd27B6JLaYvxg64NHQc
LiQREoJuNE7oUgbmST9CNApyrayPjTTAx9
8

./bip0032sbclexec ftcaddrs CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE 4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi 5 3

6s4xvg5Xa1r5HHd3gJiHjtak2BpnUFfvcE
6zfL9GTL9BhnA6i2nknLc35q9nCbxP4cCF
72uum7B7E6N1fwrpcBpMsuaETSDKYEfpWo
8

./bip0032sbclexec ltcpairs aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi 5 3

6uAEtKfNEVZZaMm8HnmwNFcSKdT6CoaFhGAd4RBi8oBFeQZkSNF
LYZUQNDKi9bs62MzWa38E9qVRLarfARfDW
6uZA6JXyhSq84umCCk2yqgP2nrXY3uTjMf5c9zVus2jg7N9HaFg
Lg9qcxb8HKTZxqSyd27B6JLaYvxg64NHQc
6vVNVKA7hqbERRJaczL1jVfaLJAGq2mFkqNdwsm9EjQvBP1sgRD
LiQREoJuNE7oUgbmST9CNApyrayPjTTAx9
8

./bip0032sbclexec ftcpairs aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi 5 3

5m4vtSQHG2ZrzU19U5fxwME7BkWg6rvUwXkxmKoELWx4sFizwkJ
6s4xvg5Xa1r5HHd3gJiHjtak2BpnUFfvcE
5mTr6RGtiyqRV21DP2w1Qmzheyb7wxoxbvfwru7S4kWVLF8oN9n
6zfL9GTL9BhnA6i2nknLc35q9nCbxP4cCF
5nQ4VRu2jNbXqXYboHE3JbHFCRDrj67V16xyenNfSTBjQBgwhHS
72uum7B7E6N1fwrpcBpMsuaETSDKYEfpWo
8

=====================================

sbcl --load make.lisp also created compiled lisp files that can be loaded and used from within lisp.
For example, unittests.lisp contains lisp code using the main code.

sbcl
(load "config")
(load "secp256k1")
(load "hdw")
(load "unittests.lisp")

Remarks on the implementation:

secp256k1.lisp contains code for the Elliptic Curve secp256k1.
https://en.bitcoin.it/wiki/Secp256k1

hdw.lisp contains code for BIP0032 (HD Wallets).
https://en.bitcoin.it/wiki/BIP_0032

unittests.lisp contains test code. It is mostly from here:
https://en.bitcoin.it/wiki/BIP_0032_TestVectors

bip0032sbcl.lisp is code the executable uses to call the functions in the files above.

Integers in lisp can be of arbitrary size, so, e.g., 256 bit integers
are simply integers and ordinary arithmetic operators can be applied to them.

Private keys are simply integers.
Public keys are two integers x and y representing the point (x,y) on the elliptic curve.

A public key can be computed from the private key k using scalar multiplication on the base point:
(smulp k xg yg)
This may return 'nil' representing the zero point.

Extended private keys are two integers k and c where k is the private key and c is the chain code.
Extended public keys are three integers x, y and c where (x,y) is the public key and c is the chain code.

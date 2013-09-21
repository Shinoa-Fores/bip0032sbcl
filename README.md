bip0032sbcl
===========

Implementation of BIP0032 (HD Wallets) in Steel Bank Common Lisp

This is an implementation of bip0032 (HD Wallets) in Steel Bank Common Lisp (sbcl).
It is intended to work on unix-based operating systems and delegates the computation
of hash functions to openssl, sha256sum and sha512sum.

Run:

./configure

This should create a file config.lisp which contains the location of the executables openssl, sha256sum and sha512sum.

Compile the main lisp files:

sbcl
(compile-file "config.lisp")
(compile-file "secp256k1.lisp")
(compile-file "hdw.lisp")

Load the main lisp files:

(load "config.lisp")
(load "secp256k1")
(load "hdw")

The file unittests.lisp demonstrates how to use the code.

(load "unittests.lisp")

Remarks on the implementation:

secp256k1.lisp contains code for the Elliptic Curve secp256k1.
https://en.bitcoin.it/wiki/Secp256k1

hdw.lisp contains code for BIP0032 (HD Wallets).
https://en.bitcoin.it/wiki/BIP_0032

unittests.lisp contains test code. It is mostly from here:
https://en.bitcoin.it/wiki/BIP_0032_TestVectors

Integers in lisp can be of arbitrary size, so, e.g., 256 bit integers
are simply integers and ordinary arithmetic operators can be applied to them.

Private keys are simply integers.
Public keys are two integers x and y representing the point (x,y) on the elliptic curve.

A public key can be computed from the private key k using scalar multiplication on the base point:
(smulp k xg yg)
This may return 'nil' representing the zero point.

Extended private keys are two integers k and c where k is the private key and c is the chain code.
Extended public keys are three integers x, y and c where (x,y) is the public key and c is the chain code.

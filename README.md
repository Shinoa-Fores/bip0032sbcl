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

There are some examples and unit tests below.
There is also a description below of how to create
a metablock chain containing arbitrary data
secured and timestamped using the Bitcoin block chain.

=====================================

Quick start:

1. Use 'master' with a seed to get an extended private key.
Since this is just an example, I will use a terrible phrase for the seed.
Make sure you don't use a terrible phrase for the seed.

./bip0032sbclexec master "Is it true?"

DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj  (the root private key)
3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ  (the root chain code)

2. Use 'pubkey' to get the corresponding root public key:

./bip0032sbclexec pubkey DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj

9egqHyQ2U4FRJhD1TYv1T36cSZgi71ttEzLKJwRGiw7c  (x coordinate of public key in base 58)
FjVLq81iEotBdEQxYgBZ9FCNGRTwYkwVKKnhAREf3YeR  (y coordinate of public key in base 58)

Extended public key: this x, this y, the chain code above.

Note: We can specify nodes in the directory structure using paths like "9/2/5" or "72'/4"
where the ' indicates a private child derivation.
A path with a private child is only accessible using an extended private key.

3. Use the extended public key to get 3 btc addresses starting with index 105 at path "9/2/5":

./bip0032sbclexec btcaddrs 9egqHyQ2U4FRJhD1TYv1T36cSZgi71ttEzLKJwRGiw7c FjVLq81iEotBdEQxYgBZ9FCNGRTwYkwVKKnhAREf3YeR 3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ "9/2/5" 105 3

1DQasDQBGPN1YERYoD4EcAxYbUfbCJTwj1
16U7Sf2Eh1oudposKqgMGx3ZWzeayZ93c3
1FgiXAqy7UjxJC24LHxYGsSrP2CQ1C1sh5

4. Use the extended private key to get the private keys for these btc addresses:

./bip0032sbclexec btcpairs DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj 3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ "9/2/5" 105 3

5JaRvqhAfJXVdg6twZ7AD9xg53Ff2MtSBkBtfnig839y17PtYAM
1DQasDQBGPN1YERYoD4EcAxYbUfbCJTwj1
5KUB8G7EwLuZciPEspKz7VfFkfJLpBqqREsHicbJ5i1qAVHGmQR
16U7Sf2Eh1oudposKqgMGx3ZWzeayZ93c3
5JmSHubq7PzavcX6CCxKdULoXBaqoxwUyNxiVdHQiVctHDyJFsy
1FgiXAqy7UjxJC24LHxYGsSrP2CQ1C1sh5

5. Use the extended private key to get 1 private key and address at index 0 at the path "72'/4"

./bip0032sbclexec btcpairs DuuexuuW9DHFXLAsrrfwsciwWbou7fNGLrQhpQfNDNCj 3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ "72'/4" 0 1

6. Try to use the extended public key to get the address at index 0 at the path "72'/4". It will fail because 72' is a private child.

./bip0032sbclexec btcaddrs 9egqHyQ2U4FRJhD1TYv1T36cSZgi71ttEzLKJwRGiw7c FjVLq81iEotBdEQxYgBZ9FCNGRTwYkwVKKnhAREf3YeR 3v3RcRzG8oiaS8AyCP65mNhtsaBgDe7bAdk2CkvvxtCJ "72'/4" 0 1

=====================================

An example demonstrating the fine-grained commands 'ckd', 'ckdp' and 'ckdpub'.

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
(starting at index 5).  We could give a path, but let us use "/" for
the empty path.  The three addresses are returned on three lines and
the final line gives the next unused index.  Usually this will be the
starting index plus the number of addresses generated, but sometimes
an index is invalid and must be skipped.

./bip0032sbclexec btcaddrs CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE 4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi "/" 5 3

1ELX99uVdVMoqDfqLS3px8mjD8DaVutjT1
1MvtMkHJCfDWi2kpSt7spHGpLibPuEkDaF
1QBTyb15HZskDsucGK9u69mDeNc7bytGxL
8

7. Use the extended private key from Step 3 to generate 3 btc
wif/address pairs (starting at index 5). The three wif/address pairs
are given over 6 lines (wif, then address).  The final line is the
next unused index. Note that the addresses correspond to those
generated in Step 6.

./bip0032sbclexec btcpairs aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi "/" 5 3

5HrWRC7qL56h6ysGmxyyarqGN9td118DvamTMEAgRLrdxYnjFyQ
1ELX99uVdVMoqDfqLS3px8mjD8DaVutjT1
5JFRdAzSo2NFbXsLgvF24HbrqNy4r71haygSSoUt9aR4Rb84az2
1MvtMkHJCfDWi2kpSt7spHGpLibPuEkDaF
5KBe2BcaoR8Mx3Qj7AY3x6tQNpbodEKDz9yUEgk7XH6JVXaJmJP
1QBTyb15HZskDsucGK9u69mDeNc7bytGxL
8

8. Repeat the same process to obtain ltc and ftc addresses and wif/address pairs.

./bip0032sbclexec ltcaddrs CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE 4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi "/" 5 3

LYZUQNDKi9bs62MzWa38E9qVRLarfARfDW
Lg9qcxb8HKTZxqSyd27B6JLaYvxg64NHQc
LiQREoJuNE7oUgbmST9CNApyrayPjTTAx9
8

./bip0032sbclexec ftcaddrs CebQefqW9HGKfcVit1zP2VFzFF1o2V6EHh87CrRD91aE 4B1WboXeevEDCem7GcEyuRer9moByPZcF3mKdQazsXZw FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi "/" 5 3

6s4xvg5Xa1r5HHd3gJiHjtak2BpnUFfvcE
6zfL9GTL9BhnA6i2nknLc35q9nCbxP4cCF
72uum7B7E6N1fwrpcBpMsuaETSDKYEfpWo
8

./bip0032sbclexec ltcpairs aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi "/" 5 3

6uAEtKfNEVZZaMm8HnmwNFcSKdT6CoaFhGAd4RBi8oBFeQZkSNF
LYZUQNDKi9bs62MzWa38E9qVRLarfARfDW
6uZA6JXyhSq84umCCk2yqgP2nrXY3uTjMf5c9zVus2jg7N9HaFg
Lg9qcxb8HKTZxqSyd27B6JLaYvxg64NHQc
6vVNVKA7hqbERRJaczL1jVfaLJAGq2mFkqNdwsm9EjQvBP1sgRD
LiQREoJuNE7oUgbmST9CNApyrayPjTTAx9
8

./bip0032sbclexec ftcpairs aFvVvUn6Pm3ffpKaCXZZMMt8eW2hfqkR6ug8PMqzGdt FzFEfp5ERtGuDTHkHGNoN6rjTmRUchWfeMgbckAtoNJi "/" 5 3

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
https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki

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

=================================
How to create a metablockchain:

1. Generate an extended private key with some reasonably random seed.

./bip0032sbclexec masterhex B8BF4CD6C5E90939147FF261EE6345B8BC663D4469D0DB3CDC94E4C86C4D4C7D3CB89

DGpM6Kwkbi2mVtfCPGQR74nKkh36AAj1MirnfagiKtb2 (the root private key)
GoLWY3832BemUX5JUk9GfvePoC6eh6ev9rcbpo7z8Aov (the root chain code)

2. Generate a sequence of private and public keys. I will generate
two here.  We will use a new key pair for each 'metablock'.  Each key
pair is given in two lines, the first being the private key in WIF
and the second being the uncompressed public key (i.e., prefix of 04, then x in hex, then y in hex).

./bip0032sbclexec keypairs DGpM6Kwkbi2mVtfCPGQR74nKkh36AAj1MirnfagiKtb2 GoLWY3832BemUX5JUk9GfvePoC6eh6ev9rcbpo7z8Aov "1" 0 2

5HsoiMv5TH6asaXhgxSZAkp5pVSMp1QwdWRMiKGGN6Lhe12Hdgj
044E964BA401D7395B86251EC3E924D6A20CA8805164E36FBEBA5627A8848C39B37842C2E7D512D027F849F4BE3662B5A0657583AD16228D7690C9BF5B2AE62BAB
5JDn4W7PT7UFU1ZjemVjwv3MBmHDe2888Fj5i9ds72v5vQHmLf3
04B078B40CEAE16B7A4A66B540B0E68CD636DD26AF8586564F2D8CD7822225CD3C3F7927678B96043DC1806C2E11BC87929F5DE6023B879E0A17C0331A0AAEA263

3. Start creating metablocks.

A metablock starts with an uncompressed public key, followed by the
transaction id certifying the previous metablock, and then arbitrary data.
For the genesis metablock, use any transaction id for which you can
spend output 0.

As an example, we give a metablock chain with three metablocks.

The first (genesis) metablock is given in the file MetaBlock1.
It contains a public key, a transaction id, Krona Rev's PGP public
key, and an example of a contract.  The public key is the first one
generated above:
044E964BA401D7395B86251EC3E924D6A20CA8805164E36FBEBA5627A8848C39B37842C2E7D512D027F849F4BE3662B5A0657583AD16228D7690C9BF5B2AE62BAB
The first transaction id is
925e591d7873c1a59ada5a7b3dd80f90b7f093b45059e0b780640f15cafba886
Output 0 of this transaction sends 0.01184844 to 1KYVAEsjuahWCJxdFUe8VapwT9X1k4k3PJ.
The private key for this address is 
5K17BrbrQR9Ateg3FcLR5NEhfeEokQJtDvbuC9oZ91uyKqQVLh1

The process of certifying this genesis metablock is given by calling:
./bip0032sbclexec certifymetablock MetaBlock1 5K17BrbrQR9Ateg3FcLR5NEhfeEokQJtDvbuC9oZ91uyKqQVLh1

If the code were integrated with ./bitcoind, then the process could be automated. At the moment,
the call to ./bip0032sbclexec provides an explanation of what to do by hand:

To certify the metablock spend output 0 for transaction
925e591d7873c1a59ada5a7b3dd80f90b7f093b45059e0b780640f15cafba886
with a transaction that spends output 0 to address 1ECYmKcntwjkEJf8bD1WEyKFzLBigxBEZ5.
The transaction id will correspond to the certificate verifying the metablock.
createrawtransaction '[{"txid":"925e591d7873c1a59ada5a7b3dd80f90b7f093b45059e0b780640f15cafba886","vout":0}]' '{"1ECYmKcntwjkEJf8bD1WEyKFzLBigxBEZ5":0.???}'
signrawtransaction ??? [] '["5K17BrbrQR9Ateg3FcLR5NEhfeEokQJtDvbuC9oZ91uyKqQVLh1"]'
sendrawtransaction ???

The ???'s should be need to filled in by hand at the moment.
In this case, the completed commands we execute in the Satoshi QT client are:

createrawtransaction '[{"txid":"925e591d7873c1a59ada5a7b3dd80f90b7f093b45059e0b780640f15cafba886","vout":0}]' '{"1ECYmKcntwjkEJf8bD1WEyKFzLBigxBEZ5":0.01174844}'
signrawtransaction 010000000186a8fbca150f6480b7e05950b493f0b7900fd83d7b5ada9aa5c173781d595e920000000000ffffffff013ced1100000000001976a91490c8b9e4f75e7109826c3318d56955627d20c06388ac00000000 [] '["5K17BrbrQR9Ateg3FcLR5NEhfeEokQJtDvbuC9oZ91uyKqQVLh1"]'
sendrawtransaction 010000000186a8fbca150f6480b7e05950b493f0b7900fd83d7b5ada9aa5c173781d595e92000000008a4730440220127373c384acb45564fa74ae57d844cdecace0a5cb8a119126d220ad1be92d45022019623aea51d03d23b3499c51196fd40d507c5844b5abcc07c933f952628dfe1f014104e29c3efb96b257b1be8b3f6c6c310d7c2d3ba87fb44c51da923408bd6c0b2067baab01eeecbb4ba0bb371990d07291eec9d5301f025e0e433ad34771ab747a23ffffffff013ced1100000000001976a91490c8b9e4f75e7109826c3318d56955627d20c06388ac00000000

This last sendrawtransaction returns the transaction id:
4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768

The second metablock is given in the file MetaBlock2.
It contains a message signed using Krona Rev's PGP key,
the key from MetaBlock1. The public key for the metablock
is the second one generated above:
04B078B40CEAE16B7A4A66B540B0E68CD636DD26AF8586564F2D8CD7822225CD3C3F7927678B96043DC1806C2E11BC87929F5DE6023B879E0A17C0331A0AAEA263
The transaction id is the one used to certify MetaBlock1:
4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768

For information about how certify MetaBlock2 as the successor of MetaBlock1 using the private key for the public key in MetaBlock1 (see Step 2):
./bip0032sbclexec certifynextmetablock MetaBlock1 MetaBlock2 5HsoiMv5TH6asaXhgxSZAkp5pVSMp1QwdWRMiKGGN6Lhe12Hdgj

To certify the metablock spend output 0 for transaction
4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768
with a transaction that spends output 0 to address 1Bjz3WXYCn7dSks97DDwxDDU1zV8Ecv4Lk.
The transaction id will correspond to the certificate verifying the metablock.
Sign with 5HsM1D2y3ZzB7sMtudndTJznCKYx9jp4i1h8DtwEvmQ4pax9uJS
createrawtransaction '[{"txid":"4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768","vout":0}]' '{"1Bjz3WXYCn7dSks97DDwxDDU1zV8Ecv4Lk":0.???}'
signrawtransaction ??? [] '["5HsM1D2y3ZzB7sMtudndTJznCKYx9jp4i1h8DtwEvmQ4pax9uJS"]'
sendrawtransaction ???

Filling in the commands by hand we execute:

createrawtransaction '[{"txid":"4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768","vout":0}]' '{"1Bjz3WXYCn7dSks97DDwxDDU1zV8Ecv4Lk":0.01164844}'
signrawtransaction 010000000168578cf9cbd5e1cbd71e6187976820d3e0f7d9e3efd2b9f715d734717066504b0000000000ffffffff012cc61100000000001976a91475d25875db561acb96cd3e1e3214f65d1e0e07cd88ac00000000 [] '["5HsM1D2y3ZzB7sMtudndTJznCKYx9jp4i1h8DtwEvmQ4pax9uJS"]'
sendrawtransaction 010000000168578cf9cbd5e1cbd71e6187976820d3e0f7d9e3efd2b9f715d734717066504b000000008c493046022100cae9d4c4214807ededb5bacf44304fea8700a2663080754a1e319d1d3989cef6022100f9c34f00ab0d01d2fe04a001c81acfa1404cb3f79a9fd6187f05473729624726014104e852e940dcd5e1a213d767ed76085d955bc0f8fa2dbad6588589f736b333febbdff2637bec1195acf43f397ddaa11d4e3401d97626554b86b29d34ea51ae4c08ffffffff012cc61100000000001976a91475d25875db561acb96cd3e1e3214f65d1e0e07cd88ac00000000

The transaction id of the sent transaction is:
97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2

The third metablock is given in the file MetaBlock3.
It contains an episode guide to the science fiction series Blake's 7.
The public key in for this metablock is not one generated above:
04DAF1DF5C331A77454561D1AEC6CC89C7BBB3FC63EC0B11E74C4A0049DC4093D88ABFBD3FB0A8BB239BFBF97A807FA58AF225AFCCB4A1A469CFC3063F6EF33610
The transaction id is the one used to certify MetaBlock2:
97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2

For information about how certify MetaBlock3 as the successor of MetaBlock2 using the private key for the public key in MetaBlock2 (see Step 2):

./bip0032sbclexec certifynextmetablock MetaBlock2 MetaBlock3 5JDn4W7PT7UFU1ZjemVjwv3MBmHDe2888Fj5i9ds72v5vQHmLf3

To certify the metablock spend output 0 for transaction
97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2
with a transaction that spends output 0 to address 1BkwaK5bxyRagz8rk5L75j8CMpyfRkmEPw.
The transaction id will correspond to the certificate verifying the metablock.
Sign with 5Ht5aJybQsiXpzd5MvD6F5MnHPoy39eF9Rz2H4JPgg66iCDMuZf
createrawtransaction '[{"txid":"97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2","vout":0}]' '{"1BkwaK5bxyRagz8rk5L75j8CMpyfRkmEPw":0.???}'
signrawtransaction ??? [] '["5Ht5aJybQsiXpzd5MvD6F5MnHPoy39eF9Rz2H4JPgg66iCDMuZf"]'
sendrawtransaction ???

Filling in the commands by hand we execute:

createrawtransaction '[{"txid":"97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2","vout":0}]' '{"1BkwaK5bxyRagz8rk5L75j8CMpyfRkmEPw":0.01154844}'
signrawtransaction 0100000001f2e0f0d540a4c92bb27c07a28825cec12fb0d5bf25af5c7c86bbf5daed23e0970000000000ffffffff011c9f1100000000001976a9147600b357dd20dca1854203d6784c96cfa965193288ac00000000 [] '["5Ht5aJybQsiXpzd5MvD6F5MnHPoy39eF9Rz2H4JPgg66iCDMuZf"]'
sendrawtransaction 0100000001f2e0f0d540a4c92bb27c07a28825cec12fb0d5bf25af5c7c86bbf5daed23e097000000008b4830450220659f099ee5f1df1f2fc56b4b7adf3b4ef78ccbb14885e850eb438182863b33d6022100e2054b56c67caafb5462aea45db67a4108a5eb3983f338d91ed818e68503e4220141042d397afdaa12e0420974382e183745f67a4bb72a085804fdcddd7d1bbdc77e2c861be288ee9609eef0760e465a4e873be9a0d5bd219f95801afb6a0d09591bd6ffffffff011c9f1100000000001976a9147600b357dd20dca1854203d6784c96cfa965193288ac00000000

The transaction id of the sent transaction is:
f2fb9b6481ed928aedef27279f7ad3d8db657c81407eee20d5f3a21daf15a28d

This transaction id certifies the third metablock in the chain, which is currently the lat metablock.

4. Verification of Metablocks:

We can now get information about how to verify a metablock is certified by a transaction id as follows:

./bip0032sbclexec verifymetablock MetaBlock1 4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768

To verify the metablock get transaction
4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768
and check that two properties hold:
input 0 spends output 0 of transaction 925e591d7873c1a59ada5a7b3dd80f90b7f093b45059e0b780640f15cafba886
and output 0 of spends to address 1ECYmKcntwjkEJf8bD1WEyKFzLBigxBEZ5
getrawtransaction 4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768 1

We can also verify that a metablock is a successor to another metablock.
This does not verify that the successor is certified.

./bip0032sbclexec verifymetablocksucc MetaBlock1 MetaBlock2

To verify the metablock successor get transaction
4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768
and check that two properties hold:
input 0 spends output 0 of transaction 925e591d7873c1a59ada5a7b3dd80f90b7f093b45059e0b780640f15cafba886
and output 0 of spends to address 1ECYmKcntwjkEJf8bD1WEyKFzLBigxBEZ5
getrawtransaction 4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768 1

./bip0032sbclexec verifymetablocksucc MetaBlock2 MetaBlock3

To verify the metablock successor get transaction
97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2
and check that two properties hold:
input 0 spends output 0 of transaction 4b5066707134d715f7b9d2efe3d9f7e0d320689787611ed7cbe1d5cbf98c5768
and output 0 of spends to address 1Bjz3WXYCn7dSks97DDwxDDU1zV8Ecv4Lk
getrawtransaction 97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2 1

The last metablock in the chain must be verified by explicitly giving its certifying transaction id:

./bip0032sbclexec verifymetablock MetaBlock3 f2fb9b6481ed928aedef27279f7ad3d8db657c81407eee20d5f3a21daf15a28d

To verify the metablock get transaction
f2fb9b6481ed928aedef27279f7ad3d8db657c81407eee20d5f3a21daf15a28d
and check that two properties hold:
input 0 spends output 0 of transaction 97e023eddaf5bb867c5caf25bfd5b02fc1ce2588a2077cb22bc9a440d5f0e0f2
and output 0 of spends to address 1BkwaK5bxyRagz8rk5L75j8CMpyfRkmEPw
getrawtransaction f2fb9b6481ed928aedef27279f7ad3d8db657c81407eee20d5f3a21daf15a28d 1


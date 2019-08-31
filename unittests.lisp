; https://en.bitcoin.it/wiki/BIP_0032_TestVectors

; unit tests for hierarchical deterministic wallets (bip0032)

; defvar all global variables I use in this file to prevent lisp from giving warnings.
(defvar x nil)(defvar y nil)(defvar masterseed nil)(defvar priv nil)(defvar c nil)(defvar pub nil)(defvar priv0p nil)(defvar c0p nil)(defvar priv0p1 nil)(defvar c0p1 nil)(defvar xpub0p1 nil)(defvar ypub0p1 nil)(defvar cu0p1 nil)(defvar xxpub0p1 nil)(defvar yypub0p1 nil)(defvar c0p12p nil)(defvar priv0p12p nil)(defvar c0p12p2 nil)(defvar priv0p12p2 nil)(defvar cu0p12p2 nil)(defvar xpub0p12p2 nil)(defvar ypub0p12p2 nil)(defvar xxpub0p12p2 nil)(defvar yypub0p12p2 nil)(defvar c0p12p2big nil)(defvar priv0p12p2big nil)(defvar xpub0p12p2big nil)(defvar ypub0p12p2big nil)(defvar cu0p12p2big nil)(defvar xxpub0p12p2big nil)(defvar yypub0p12p2big nil)(defvar masterhex nil)(defvar priv0 nil)(defvar c02bp1 nil)(defvar priv02bp1 nil)(defvar c02bp12bp nil)(defvar priv02bp12bp nil)(defvar xpub0 nil)(defvar ypub0 nil)(defvar xxpub0 nil)(defvar yypub0 nil)(defvar cu0 nil)(defvar c0 nil)(defvar priv02bp nil)(defvar c02bp nil)(defvar xpub02bp1 nil)(defvar ypub02bp1 nil)(defvar cu02bp1 nil)(defvar xxpub02bp1 nil)(defvar yypub02bp1 nil)(defvar priv02bp12bp2 nil)(defvar c02bp12bp nil)(defvar c02bp12bp2 nil)(defvar xpub02bp12bp2 nil)(defvar ypub02bp12bp2 nil)(defvar cu02bp12bp2 nil)(defvar xxpub02bp12bp2 nil)(defvar yypub02bp12bp2 nil)

; Test Vector 1
(setq masterseed (make-string 16))
(dotimes (i 16) (setf (aref masterseed i) (code-char i)))
(multiple-value-setq (priv c) (master masterseed))

(unless (= priv #xe8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35) (break))
(unless (= c #x873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508) (break))

(setq pub (comprpubkey priv))
(unless (= pub #x0339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2) (break))

(multiple-value-setq (priv0p c0p) (ckd priv c (logior (ash 1 31) 0)))
(unless (and (= #xedb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea priv0p)
	     (= #x47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141 c0p)
	     (= (comprpubkey priv0p) #x035a784662a4a20a65bf6aab9ae98a6c068a81c52e4b032c0fb5400c706cfccc56))
  (break))

(multiple-value-setq (priv0p1 c0p1) (ckd priv0p c0p 1))
(unless (and (= priv0p1 #x3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368)
	     (= c0p1 #x2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19)
	     (= (comprpubkey priv0p1) #x03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c))
  (break))

(multiple-value-setq (x y) (smulp priv0p xg yg))
(multiple-value-setq (xpub0p1 ypub0p1 cu0p1) (ckdp x y c0p 1))
(multiple-value-setq (xxpub0p1 yypub0p1) (smulp priv0p1 xg yg))
(unless (and (= cu0p1 c0p1)
	     (= xpub0p1 xxpub0p1)
	     (= ypub0p1 yypub0p1))
  (break))

(multiple-value-setq (priv0p12p c0p12p) (ckd priv0p1 c0p1 (logior (ash 1 31) 2)))
(unless (and (= priv0p12p #xcbce0d719ecf7431d88e6a89fa1483e02e35092af60c042b1df2ff59fa424dca)
	     (= c0p12p #x04466b9cc8e161e966409ca52986c584f07e9dc81f735db683c3ff6ec7b1503f)
	     (= (comprpubkey priv0p12p) #x0357bfe1e341d01c69fe5654309956cbea516822fba8a601743a012a7896ee8dc2))
  (break))

(multiple-value-setq (priv0p12p2 c0p12p2) (ckd priv0p12p c0p12p 2))
(unless (and (= priv0p12p2 #x0f479245fb19a38a1954c5c7c0ebab2f9bdfd96a17563ef28a6a4b1a2a764ef4)
	     (= c0p12p2 #xcfb71883f01676f587d023cc53a35bc7f88f724b1f8c2892ac1275ac822a3edd)
	     (= (comprpubkey priv0p12p2) #x02e8445082a72f29b75ca48748a914df60622a609cacfce8ed0e35804560741d29))
  (break))

(multiple-value-setq (x y) (smulp priv0p12p xg yg))
(multiple-value-setq (xpub0p12p2 ypub0p12p2 cu0p12p2) (ckdp x y c0p12p 2))
(multiple-value-setq (xxpub0p12p2 yypub0p12p2) (smulp priv0p12p2 xg yg))
(unless (and (= cu0p12p2 c0p12p2)
	     (= xpub0p12p2 xxpub0p12p2)
	     (= ypub0p12p2 yypub0p12p2))
  (break))

(multiple-value-setq (priv0p12p2big c0p12p2big) (ckd priv0p12p2 c0p12p2 1000000000))
(unless (and (= priv0p12p2big #x471b76e389e528d6de6d816857e012c5455051cad6660850e58372a6c3e6e7c8)
	     (= c0p12p2big #xc783e67b921d2beb8f6b389cc646d7263b4145701dadd2161548a8b078e65e9e)
	     (= (comprpubkey priv0p12p2big) #x022a471424da5e657499d1ff51cb43c47481a03b1e77f951fe64cec9f5a48f7011))
  (break))

(multiple-value-setq (x y) (smulp priv0p12p2 xg yg))
(multiple-value-setq (xpub0p12p2big ypub0p12p2big cu0p12p2big) (ckdp x y c0p12p2 1000000000))
(multiple-value-setq (xxpub0p12p2big yypub0p12p2big) (smulp priv0p12p2big xg yg))
(unless (and (= cu0p12p2big c0p12p2big)
	     (= xpub0p12p2big xxpub0p12p2big)
	     (= ypub0p12p2big yypub0p12p2big))
  (break))

; Test Vector 2
(setq masterhex #xfffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542)
(setq masterseed (make-string 64))
(dotimes (i 64) (setf (aref masterseed i) (code-char (logand #xff (ash masterhex (- (* 8 (- 63 i))))))))
(multiple-value-setq (priv c) (master masterseed))

(unless (= priv #x4b03d6fc340455b363f51020ad3ecca4f0850280cf436c70c727923f6db46c3e) (break))
(unless (= c #x60499f801b896d83179a4374aeb7822aaeaceaa0db1f85ee3e904c4defbd9689) (break))

(setq pub (comprpubkey priv))
(unless (= pub #x03cbcaa9c98c877a26977d00825c956a238e8dddfbd322cce4f74b0b5bd6ace4a7) (break))

(multiple-value-setq (priv0 c0) (ckd priv c 0))
(unless (and (= #xabe74a98f6c7eabee0428f53798f0ab8aa1bd37873999041703c742f15ac7e1e priv0)
	     (= #xf0909affaa7ee7abe5dd4e100598d4dc53cd709d5a5c2cac40e7412f232f7c9c c0)
	     (= (comprpubkey priv0) #x02fc9e5af0ac8d9b3cecfe2a888e2117ba3d089d8585886c9c826b6b22a98d12ea))
  (break))

(multiple-value-setq (x y) (smulp priv xg yg))
(multiple-value-setq (xpub0 ypub0 cu0) (ckdp x y c 0))
(multiple-value-setq (xxpub0 yypub0) (smulp priv0 xg yg))
(unless (and (= cu0 c0)
	     (= xpub0 xxpub0)
	     (= ypub0 yypub0))
  (break))

(multiple-value-setq (priv02bp c02bp) (ckd priv0 c0 (logior (ash 1 31) 2147483647)))
(unless (and (= priv02bp #x877c779ad9687164e9c2f4f0f4ff0340814392330693ce95a58fe18fd52e6e93)
	     (= c02bp #xbe17a268474a6bb9c61e1d720cf6215e2a88c5406c4aee7b38547f585c9a37d9)
	     (= (comprpubkey priv02bp) #x03c01e7425647bdefa82b12d9bad5e3e6865bee0502694b94ca58b666abc0a5c3b))
  (break))

(multiple-value-setq (priv02bp1 c02bp1) (ckd priv02bp c02bp 1))
(unless (and (= priv02bp1 #x704addf544a06e5ee4bea37098463c23613da32020d604506da8c0518e1da4b7)
	     (= c02bp1 #xf366f48f1ea9f2d1d3fe958c95ca84ea18e4c4ddb9366c336c927eb246fb38cb)
	     (= (comprpubkey priv02bp1) #x03a7d1d856deb74c508e05031f9895dab54626251b3806e16b4bd12e781a7df5b9))
  (break))

(multiple-value-setq (x y) (smulp priv02bp xg yg))
(multiple-value-setq (xpub02bp1 ypub02bp1 cu02bp1) (ckdp x y c02bp 1))
(multiple-value-setq (xxpub02bp1 yypub02bp1) (smulp priv02bp1 xg yg))
(unless (and (= cu02bp1 c02bp1)
	     (= xpub02bp1 xxpub02bp1)
	     (= ypub02bp1 yypub02bp1))
  (break))

(multiple-value-setq (priv02bp12bp c02bp12bp) (ckd priv02bp1 c02bp1 (logior (ash 1 31) 2147483646)))
(unless (and (= priv02bp12bp #xf1c7c871a54a804afe328b4c83a1c33b8e5ff48f5087273f04efa83b247d6a2d)
	     (= c02bp12bp #x637807030d55d01f9a0cb3a7839515d796bd07706386a6eddf06cc29a65a0e29)
	     (= (comprpubkey priv02bp12bp) #x02d2b36900396c9282fa14628566582f206a5dd0bcc8d5e892611806cafb0301f0))
  (break))

(multiple-value-setq (priv02bp12bp2 c02bp12bp2) (ckd priv02bp12bp c02bp12bp 2))
(unless (and (= priv02bp12bp2 #xbb7d39bdb83ecf58f2fd82b6d918341cbef428661ef01ab97c28a4842125ac23)
	     (= c02bp12bp2 #x9452b549be8cea3ecb7a84bec10dcfd94afe4d129ebfd3b3cb58eedf394ed271)
	     (= (comprpubkey priv02bp12bp2) #x024d902e1a2fc7a8755ab5b694c575fce742c48d9ff192e63df5193e4c7afe1f9c))
  (break))

(multiple-value-setq (x y) (smulp priv02bp12bp xg yg))
(multiple-value-setq (xpub02bp12bp2 ypub02bp12bp2 cu02bp12bp2) (ckdp x y c02bp12bp 2))
(multiple-value-setq (xxpub02bp12bp2 yypub02bp12bp2) (smulp priv02bp12bp2 xg yg))
(unless (and (= cu02bp12bp2 c02bp12bp2)
	     (= xpub02bp12bp2 xxpub02bp12bp2)
	     (= ypub02bp12bp2 yypub02bp12bp2))
  (break))

; Generation of some 5 private keys starting at index 40 in btc wif from the external chain of account 0
; at the node above.
(defvar privkeys (external-chain-privkeys priv02bp12bp2 c02bp12bp2 0 40 5))

; Generation of the corresponding 5 public keys from the corresponding extended public key.
(defvar pubkeys (external-chain-pubkeys xpub02bp12bp2 ypub02bp12bp2 c02bp12bp2 0 40 5))

; Check the public keys really correspond to the private keys.
(dotimes (i 5)
  (multiple-value-bind
   (x y)
   (smulp (nth i privkeys) xg yg)
   (unless (equal (list x y) (nth i pubkeys)) (break))))

(dotimes (i 5)
  (format t "BTC~%WIF ~d~%address ~d~%" (btcwif (nth i privkeys)) (btcaddr (car (nth i pubkeys)) (cadr (nth i pubkeys)))))

(dotimes (i 5)
  (format t "LTC~%WIF ~d~%address ~d~%" (ltcwif (nth i privkeys)) (ltcaddr (car (nth i pubkeys)) (cadr (nth i pubkeys)))))

(dotimes (i 5)
  (format t "UNO~%WIF ~d~%address ~d~%" (unowif (nth i privkeys)) (unoaddr (car (nth i pubkeys)) (cadr (nth i pubkeys)))))

; Tests for computing addresses and wif:

; Bitcoin addresses from https://www.bitaddress.org
(defvar btc-addr-wif '(
("15EbFuBDCdCtAe7M7ib18x89xCe4yNWgUp"
 "5KUSLaLgRU3bG8Fh4mNDbYvvH1LuxssSxaoSc2u91i8PG116y2i")
("1LiSB7SZfyUAVhTywHqvAk5XNWB26o5sVu"
 "5JtrQAgiaBPNPagyinJ8eBCKLhb2WUBFgYUL4MpS54ZXVv9gdww")
("1A8JEfrWCL55PqfXN3Hf8QszWZsQ5E7wXh"
 "5KSj7k7kyMPKDHzJHkMoiNBxkUx91bcV5U8Bn2CghCxUB1wrfUy")
("1Fm6q9khu8hnxMVkgzmNTkwZghGP1ku1Jh"
 "5J4QMe1yUbrhsfKDh1mdEpJkHjaPdEzQEa436FWyc5nshGqcL9S")
("1NSTPFDSn3iJG4DbdFSDSjU8QFofJXv2cf"
 "5HuduDWspkQy1ckzYKnFSNH5tgX59eXmDPvhx5JFJcW6Ji65VVz")
("13YFVBT7u4q57ztfUN8B8wjeESzhB1yC5f"
 "5JJSVTF7w4aReS7Bk99jkn8sdZqEjsxn4bofkA3aFFgeDgtSDML")
("1A5BzugfH7baHszbJ3Vf7jezYint5ZQACM"
 "5KHAiH49dztSMMAh164eG8HEnKeyUFzwtJoWHkYetRbdYc3MLcK")
))

; Litecoin addresses from https://liteaddress.org/
(defvar ltc-addr-wif '(
("LiR71wzEEJMsUn2JvHihtCohfgQ6QxChtx"
 "6vPhURt5Drof12A8Uii61a8DX8tjnLtq27vzJLTRgT8zHxJy72D")
("LbGNbE8zWuQXRSZw2cA2Kh3AgaZtnrkfXT"
 "6vqrXU65RLWf7arJ215zT85Sumzc7K8VLDrky4DQNM6yU4RzJRA")
("LdnTginxd83UPZmR7UNFsJwyYsXUCQQyEU"
 "6vd7Yhf5AsCV1KPQi1hTAvCfCNYTZGpjwzz6qpVmdrgg7PXZERp")
("LNWyUBMa7zx5t5ZiaQZxgkDp7fT91HqS9u"
 "6uU3xn83fqepsrF5exVaLMExvnb7ZHqMnTotLh5ekauiytGjypj")
("LiY8Lif2k6ryqNvvb64zcUDK6BxibL9L4U"
 "6uiTApPxcjpgoVebXd9rxjCnHvnaJBfGQE9VhtouhdzRy3vPYPe")
("LS5kQ5Fd2KJwoyrSeKGXfQeXPMsaug2SS6"
 "6uCiTUVmwzvAeUAS2Gda88k8s1NLHLmKBasL4yL86s4ZLryGnzz")
("La9YjzMjhoQKYeQJ7cCfBg6ZEHdr44crmN"
 "6vDVmjuFvNirxvnDwiXGAM2bk4yqPVJ61GgNAM37A3NyXfVpPGK")
))

; Unobtanium addresses
(defvar uno-addr-wif '(
("uQNwQ9x1aVYC7nktDjcXzPZbJDyVgy7VCW" 
 "8XGmvSbyHAXPSNLvQUYxkLacRPUTKbX9JVXk3iHv2HeDSMBABYm")
("uRmnrYpSe8zH8qGuE4PPfsxhBX8YN2QyCJ" 
 "8XJFXHAR4KdPSEPzK5dYpMMS1wuDr3afYPgrZE7byTctTpn8mzs")
("uM4YgQpEkQBcZr2tQd2686P9fqHfrC1thL" 
 "8WrGhytJrtwx4ar7VkAMudfsGcbuiDwbJTYmsCXCF4r2wXCPGTB")
("ubZWDTU5rQrwxApxyuq6zijb8CkEnQRj9f" 
 "8WUNcL3VeLveTtgpHT3XFJc678tYctuBaaWQJMvbNbqx4AXxPYm")
("uXpayw8EPv8pKRqvVk4XaxsBFBBFBezrzq" 
 "8WU1yc1iZqsyNndW4ZKHyBXb2S3s6WV7Z7wAG9ukUgKSEoZkvAh")
("uXjToegWm9BMn7J3bYoMHTNVP3fxvnCRR5" 
 "8XoKE4MKm4maHPhLvUtxbXMBkkEGoAwcbx912ZiX3PBmAri7tfd")
("uNwLMekA6Y5GJ9gEmQ6CGAcVxEwSJEPZEz" 
 "8Y4dQYHTp34SF57cWyRYprC8tpxJ3HbTctiWsMXSRUMY1PeicA3")))

(dolist (aw btc-addr-wif)
  (let ((k (privkeyfromwif (cadr aw))))
    (unless (equal (btcwif k) (cadr aw)) (error "wif error btc"))
    (multiple-value-bind
     (x y)
     (smulp k xg yg)
     (unless (equal (btcaddr x y) (car aw)) (error "addr error btc")))))

(dolist (aw ltc-addr-wif)
  (let ((k (privkeyfromwif (cadr aw))))
    (unless (equal (ltcwif k) (cadr aw)) (error "wif error ltc"))
    (multiple-value-bind
     (x y)
     (smulp k xg yg)
     (unless (equal (ltcaddr x y) (car aw)) (error "addr error ltc")))))

(dolist (aw uno-addr-wif)
  (let ((k (privkeyfromwif (cadr aw))))
    (unless (equal (unowif k) (cadr aw)) (error "wif error uno"))
    (multiple-value-bind
     (x y)
     (smulp k xg yg)
     (unless (equal (unoaddr x y) (car aw)) (error "addr error uno")))))


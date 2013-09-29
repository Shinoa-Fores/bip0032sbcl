; Code for BIP0032 (HD Wallets)
; https://en.bitcoin.it/wiki/BIP_0032

; Computing compressed public keys

; x,y : 256 bit ints giving a public key as a point on the elliptic curve
; return compressed public key as a 258 bit int
(defun compr (x y)
  (if (oddp y)
      (logior (ash 3 256) x)
    (logior (ash 2 256) x)))

; k : 256 bit int, private key
; return corresponding compressed public key as a 258 bit int
(defun comprpubkey (k)
  (multiple-value-bind
   (x y)
   (smulp k xg yg)
   (if (and x y)
       (compr x y)
     (error "Bad private key"))))

; Hash functions (relying on openssl, sha256sum and sha512sum to do the work)
; Set the values of *openssl*, *sha256sum* and *sha512sum* in config.lisp

; n is a number with nlen bytes. Return the 20 bit number given by ripemd160
(defun ripemd160num (n nlen)
  (let ((nbytes nil))
    (dotimes (i nlen) (push (mod n 256) nbytes) (setq n (ash n -8)))
    (let ((p (sb-ext:run-program *openssl* '("dgst" "-ripemd160") :input :stream :output :stream :wait nil)))
      (dolist (b nbytes) (write-byte b (sb-ext:process-input p)))
      (close (sb-ext:process-input p))
      (sb-ext:process-wait p t)
      (prog1 (read-from-string (format nil "#x~d" (read-line (sb-ext:process-output p)))) (close (sb-ext:process-output p))))))

; n is a number with nlen bytes. Return the 256 bit number given by sha256
(defun sha256num (n nlen)
  (let ((nbytes nil))
    (dotimes (i nlen) (push (mod n 256) nbytes) (setq n (ash n -8)))
    (let ((p (sb-ext:run-program *sha256sum* '("-b") :input :stream :output :stream :wait nil)))
      (dolist (b nbytes) (write-byte b (sb-ext:process-input p)))
      (close (sb-ext:process-input p))
      (sb-ext:process-wait p t)
      (prog1 (read-from-string (format nil "#x~d" (read-line (sb-ext:process-output p)))) (close (sb-ext:process-output p))))))

; n is a number with nlen bytes. Return the 512 bit number given by sha512
(defun sha512num (n nlen)
  (let ((nbytes nil))
    (dotimes (i nlen) (push (mod n 256) nbytes) (setq n (ash n -8)))
    (let ((p (sb-ext:run-program *sha512sum* '("-b") :input :stream :output :stream :wait nil)))
      (dolist (b nbytes) (write-byte b (sb-ext:process-input p)))
      (close (sb-ext:process-input p))
      (sb-ext:process-wait p t)
      (prog1 (read-from-string (format nil "#x~d" (read-line (sb-ext:process-output p)))) (close (sb-ext:process-output p))))))

; HMAC-SHA512

; keybytes : list of bytes
; data,datalen : int
; datalen is number of bytes in data
; return 512 bit int
(defun hmac-sha512b (keybytes data datalen)
  (let ((okpad 0) (ikpad 0))
    (dotimes (i 128)
      (let ((k (pop keybytes)))
	(if k
	    (progn
	      (setq okpad (logior (ash okpad 8) (logxor k #x5c)))
	      (setq ikpad (logior (ash ikpad 8) (logxor k #x36))))
	  (progn
	    (setq okpad (logior (ash okpad 8) #x5c))
	    (setq ikpad (logior (ash ikpad 8) #x36))))))
    (sha512num (logior (ash okpad 512)
		       (sha512num (logior (ash ikpad (* 8 datalen)) data)
				  (+ 128 datalen)))
	       192)))

; for a general key and data
; key,keylen,data,datalen : ints
; keylen is number of bytes in key
; datalen is number of bytes in data
; return 512 bit int
(defun hmac-sha512a (key keylen data datalen)
  (let ((keybytes nil))
    (dotimes (i keylen) (push (mod key 256) keybytes) (setq key (ash key -8)))
    (hmac-sha512b keybytes data datalen)))

; key: 256 bit, data: 296 bit (1 + 32 + 4 bytes)
; return 512 bit int
(defun hmac-sha512 (key data)
  (hmac-sha512a key 32 data 37))

; Private Child Key Derivation
; https://en.bitcoin.it/wiki/BIP_0032#Private_child_key_derivation
; kpar,cpar : 256 bit ints, the extended private key
; returns ki ci
;   where ki is the private key of child i and ci is the chain code of child i
;   -- that is, (ki,ci) is the extended private key of child i
; or return nil if invalid
(defun ckd (kpar cpar i)
  (let* ((bi
	  (if (oddp (ash i -31)) ; most significant bit is set, use private derivation
	      (hmac-sha512 cpar (logior (ash kpar 32) i))
	    (hmac-sha512 cpar (logior (ash (comprpubkey kpar) 32) i))))
	 (bil (ash bi -256))
	 (bir (logand bi #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))
    (let ((ki (mod (+ bil kpar) n)))
      (if (and (> ki 0) (< bil n))
	  (values ki bir)
	nil))))

; Public Child Key Derivation
; https://en.bitcoin.it/wiki/BIP_0032#Public_child_key_derivation
; xKpar,yKpar,cpar : 256 bit ints, the extended public key
; i : 32 bit int, which public child to compute
; (xKpar,yKpar) : public key of parent as a point on the curve.
; return xKi yKi ci
;   where (xKi,yKi) is the public key of child i and ci is the chain code of child i
;   -- that is, (xKi,yKi,ci) is the extended public key of child i
; or return nil if invalid
(defun ckdp (xKpar yKpar cpar i)
  (if (oddp (ash i -31)) ; most significant bit is set, error since should be computing public derivation
      (error "ckdp cannot perform private child derivation")
    (let* ((bi
	    (hmac-sha512 cpar (logior (ash (compr xKpar yKpar) 32) i)))
	   (bil (ash bi -256))
	   (bir (logand bi #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))
      (multiple-value-bind
       (x y)
       (smulp bil xg yg)
       (multiple-value-bind
	(xKi yKi)
	(addp x y xKpar yKpar)
	(if (and xKi yKi (< bil n))
	    (values xKi yKi bir)
	  nil))))))

; https://en.bitcoin.it/wiki/BIP_0032#Master_key_generation
; s : seed string
; return two values: I_L master secret key and I_R master chain code
(defun master (s)
  (let ((x "#x"))
    (dotimes (i (length s)) (setq x (format nil "~d~2,'0X" x (char-code (aref s i)))))
    (let ((m (hmac-sha512a #x426974636F696E2073656564 12 (read-from-string x) (length s))))
      (values (ash m -256) (logand m #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))))

; https://en.bitcoin.it/wiki/BIP_0032#Specification:_Wallet_structure

; priv,c : extended private key
; i,kstart,klen : int
; Return the <=klen private keys (usually klen private keys, unless some are invalid)
; The private keys are the ones with indices kstart,...,kstart+klen-1 from the external chain of account i with
; extended private key priv,c.
(defun external-chain-privkeys (priv c i kstart klen)
  (multiple-value-bind
   (privi ci)
   (ckd priv c i)
   (if (and privi ci)
       (multiple-value-bind
	(priv0 c0)
	(ckd privi ci 0)
	(if (and priv0 c0)
	    (let ((r nil))
	      (dotimes (k klen (reverse r))
		(push (ckd priv0 c0 (+ kstart k)) r)))
	  nil))
     nil)))

; priv,c : extended private key
; i,kstart,klen : int
; Return the <=klen private keys (usually klen private keys, unless some are invalid)
; The private keys are the ones with indices kstart,...,kstart+klen-1 from the internal chain of account i with
; extended private key priv,c.
(defun internal-chain-privkeys (priv c i kstart klen)
  (multiple-value-bind
   (privi ci)
   (ckd priv c i)
   (if (and privi ci)
       (multiple-value-bind
	(priv1 c1)
	(ckd privi ci 1)
	(if (and priv1 c1)
	    (let ((r nil))
	      (dotimes (k klen (reverse r))
		(push (ckd priv1 c1 (+ kstart k)) r)))
	  nil))
     nil)))

; x,y,c : extended public key
; i,kstart,klen : int
; Return the <=klen public keys (usually klen public keys, unless some are invalid)
; The public keys are the ones with indices kstart,...,kstart+klen-1 from the external chain of account i with
; extended public key x,y,c.
(defun external-chain-pubkeys (x y c i kstart klen)
  (if (oddp (ash i -31)) ; most significant bit is set, error since such accounts require the extended private key
      (error "ckdp cannot perform private child derivation")
    (multiple-value-bind
     (xi yi ci)
     (ckdp x y c i)
     (if (and xi yi ci)
	 (multiple-value-bind
	  (x0 y0 c0)
	  (ckdp xi yi ci 0)
	  (if (and x0 y0 c0)
	      (let ((r nil))
		(dotimes (k klen (reverse r))
		  (multiple-value-bind
		   (xk yk ck)
		   (ckdp x0 y0 c0 (+ kstart k))
		   (when (and xk yk) (push (list xk yk) r)))))
	    nil))
       nil))))

; x,y,c : extended public key
; i,kstart,klen : int
; Return the <=klen public keys (usually klen public keys, unless some are invalid)
; The public keys are the ones with indices kstart,...,kstart+klen-1 from the internal chain of account i with
; extended public key x,y,c.
(defun internal-chain-pubkeys (x y c i kstart klen)
  (if (oddp (ash i -31)) ; most significant bit is set, error since such accounts require the extended private key
      (error "ckdp cannot perform private child derivation")
    (multiple-value-bind
     (xi yi ci)
     (ckdp x y c i)
     (if (and xi yi ci)
	 (multiple-value-bind
	  (x1 y1 c1)
	  (ckdp xi yi ci 1)
	  (if (and x1 y1 c1)
	      (let ((r nil))
		(dotimes (k klen (reverse r))
		  (multiple-value-bind
		   (xk yk ck)
		   (ckdp x1 y1 c1 (+ kstart k))
		   (when (and xk yk) (push (list xk yk) r)))))
	    nil))
       nil))))

; base58 representation
(defvar *base58chars* '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\G #\H #\J #\K #\L #\M #\N #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

; c : int
; return base58 string representation of c
(defun base58 (c &optional (r ""))
  (if (> c 0)
      (let ((m (mod c 58)))
	(base58 (/ (- c m) 58) (format nil "~d~d" (nth m *base58chars*) r)))
    r))

; s : base58 string
; return int representation of s
(defun frombase58 (s)
  (let ((r 0))
    (dotimes (i (length s) r)
      (let ((p (position (aref s i) *base58chars*)))
	(if p
	    (setq r (+ (* r 58) p))
	  (error "string is not a base58 number"))))))

; Computation of Wallet Import Formats for Private Keys

; generic wif from private key with prefix depending on the coin
; k : private key, int
; pre : prefix byte, int
; return string, base58
(defun genwif (k pre)
  (let* ((k1 (logior (ash pre 256) k))
	 (sh1 (sha256num k1 33))
	 (sh2 (sha256num sh1 32))
	 (sh24 (ash sh2 -224))
	 (c (logior (ash k1 32) sh24)))
    (base58 c)))

; btc private key in wallet import format, btc prefix is #x80.
; k : private key, int
; return string, base58 btc wif
(defun btcwif (k)
  (genwif k #x80))

; ltc private key in wallet import format, ltc prefix is #xb0, otherwise the process is the same as btcwif
; k : private key, int
; return string, base58 ltc wif
(defun ltcwif (k)
  (genwif k #xb0))

; ftc private key in wallet import format, ftc prefix is #x8e, otherwise the process is the same as btcwif
; k : private key, int
; return string, base58 ftc wif
(defun ftcwif (k)
  (genwif k #x8e))

; w : wif (of btc, ltc, ftc), base58 string
; return private key, int
; Note: This doesn't check that the input was a valid wif string.
(defun privkeyfromwif (w)
  (logand (ash (frombase58 w) -32) #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))

; Computation of addresses

; Helper function to count the leading 0 bytes. btcaddr uses this.
(defun count0bytes (rm1 n)
  (if (>= n 0)
      (if (= (ash rm1 (* n -8)) 0)
	  (1+ (count0bytes rm1 (- n 1)))
	0)
    0))

; (x,y) : public key (as point on the elliptic curve)
; returns btc address, base58 string
(defun btcaddr (x y)
  (let* ((pub (logior (logior (ash #x04 512) (ash x 256)) y))
	 (sh1 (sha256num pub 65))
	 (rm1 (ripemd160num sh1 32))
	 (c0 (count0bytes rm1 20)) ; this seems to be particular to BTC
	 (sh2 (sha256num rm1 21)) ; the first byte is 0, the rest are from rm1
	 (sh3 (sha256num sh2 32))
	 (sh34 (ash sh3 -224))
	 (a (logior (ash rm1 32) sh34)))
    (format nil "~d~d" (make-string c0 :initial-element #\1) (base58 a))))

; generic address computation which works for ltc and ftc
; (x,y) : public key (as point on the elliptic curve)
; pre : prefix byte
(defun genaddr (x y pre)
  (let* ((pub (logior (logior (ash #x04 512) (ash x 256)) y))
	 (sh1 (sha256num pub 65))
	 (rm1 (ripemd160num sh1 32))
	 (rm2 (logior (ash pre 160) rm1))
	 (sh2 (sha256num rm2 21))
	 (sh3 (sha256num sh2 32))
	 (sh34 (ash sh3 -224))
	 (a (logior (ash rm2 32) sh34)))
    (base58 a)))

; (x,y) : public key (as point on the elliptic curve)
; return ltc address, base58 string
; the first byte is #x30 for LTC
(defun ltcaddr (x y)
  (genaddr x y #x30))

; (x,y) : public key (as point on the elliptic curve)
; return ftc address, base58 string
; the first byte is #x0e for FTC
(defun ftcaddr (x y)
  (genaddr x y #x0e)
  )


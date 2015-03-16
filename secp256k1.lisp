;;;; Code for the Elliptic Curve secp256k1

;;; excerpt from “Standards for Efficient Cryptography 2 (SEC 2)”:

;;; 2.4.1 Recommended Parameters secp256k1

;;; The elliptic curve domain parameters over Fp associated with a Koblitz curve
;;; secp256k1 are specified by the sextuple T = (p,a,b,G,n,h) where the finite
;;; field Fp is defined by:
;;; p= FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F
;;;  = 2^256 − 2^32 − 2^9 − 2^8 − 2^7 − 2^6 − 2^4 − 1
;;; The curve E: y^2 = x^3 + ax + b over Fp is defined by:
;;; a = 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
;;; b = 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000007
;;; The base point G in compressed form is:
;;; 02 79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798
;;; and in uncompressed form is:
;;; 04 79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798
;;;    483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8
;;; Finally the order n of G and the cofactor are:
;;; n = FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141
;;; h = 01

;;; confirmed with:
;;; http://www.secg.org/sec2-v2.pdf (source)
;;; https://en.bitcoin.it/wiki/Secp256k1
;;; https://github.com/warner/python-ecdsa.git ecdsa.py
;;; http://safecurves.cr.yp.to/ (according to which, secp256k1 is unsafe!)

(macrolet ((define-constants (&rest constants)
             `(progn ,@(loop for (name value) on constants by #'cddr
                          collect `(defconstant ,name ,value)))))
  (define-constants
      p  #.`(- ,@(mapcar (lambda (n) (ash 1 n)) '(256 32 9 8 7 6 4 0)))
      xg #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
      yg #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
      ng #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141))

;;; Fp operations
(macrolet ((define-operations (&rest pairs)
             `(progn ,@(loop for (op name) on pairs by #'cddr collect
                            `(defun ,name (x y) (mod (,op x y) p))))))
  (define-operations + add - sub * mul #|expt pow lolno|#))

;;; modular exponentiation (by squaring)
(defun pow (x n &optional (x^n 1))
  (do ((x x (mul x x)) (n n (ash n -1))) ((zerop n) x^n)
    (when (oddp n) (setf x^n (mul x^n x)))))

;;; Extended Euclidean Algorithm
(defun eea (a b &optional (x 0) (prevx 1) (y 1) (prevy 0))
  (if (zerop b) (values prevx prevy)
      (multiple-value-bind (q r) (floor a b)
        (eea b r (sub prevx (mul q x)) x (sub prevy (mul q y)) y))))

;;; multiplicative inverse mod p
(defun inv (x) (nth-value 1 (eea p (mod x p))))

;;; Tests if a point is on the curve
(defun e (x y) (= (mul y y) (add (pow x 3) 7)))

;;; Addition for points on the elliptic curve
(defun addp (xp yp xq yq) ; https://hyperelliptic.org/EFD/g1p/auto-shortw.html
  (if (and xp yp xq yq)   ; first screen out those pesky infinite points
      (macrolet ((ua (s r) `(let* ((s ,s) (x (sub (mul s s) ,r))) ; iyhtaynk
                              (values x (sub 0 (add yp (mul s (sub x xp))))))))
        (if (/= xp xq) (ua (mul (sub yp yq) (inv (- xp xq))) (add xp xq))  ; p+q
            (if (zerop (add yp yq)) (values nil nil) ; p = -q, so p+q = infinity
                (ua (mul (inv (* 2 yp)) (mul 3 (pow xp 2))) (mul 2 xp))))) ; 2*p
      (if (and xp yp) (values xp yp) (values xq yq)))) ; pick the [in]finite one

;;; Scalar multiplication (by doubling)
(defun smulp (k xp yp)
  (if (zerop k) (values nil nil)
      (multiple-value-bind (xq yq) (addp xp yp xp yp)
        (multiple-value-bind (xr yr) (smulp (ash k -1) xq yq)
          (if (evenp k) (values xr yr) (addp xp yp xr yr))))))

;;; "a horseshoe will bring you good luck whether you believe in it or not"
(macrolet ((check-sanity (&rest checks)
             `(progn ,@(loop for (test text) on checks by #'cddr
                          collect `(assert ,test () ,text)))))
  (check-sanity (= 977 (sub (pow 2 256) (pow 2 32)))       "madness! blasphemy!"
                (e xg yg) "base autism" (not (smulp ng xg yg)) "base disorder"))

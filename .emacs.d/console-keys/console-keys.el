;;; TTY Key hacks
;;; The translations with the settings below fit the adjustments to the key table.
(define-key function-key-map "\e[25~" [(control return)]) ;f13
(define-key input-decode-map "\e[26~" [(control shift return)]) ;f14
(define-key input-decode-map "\e[28~" [(meta shift return)]) ;f15
(define-key input-decode-map "\e[29~" [(meta shift left)]) ;f16
(define-key input-decode-map "\e[31~" [(meta shift right)]) ;f17
(define-key input-decode-map "\e[32~" [(meta shift up)]) ;f18
(define-key input-decode-map "\e[33~" [(meta shift down)]) ;f19
(define-key input-decode-map "\e[34~" [(meta left)]) ;f20
(define-key input-decode-map "\e[35~" [(meta right)]) ;f21
(define-key input-decode-map "\e[36~" [(meta up)]) ;f22
(define-key input-decode-map "\e[37~" [(meta down)]) ;f23
(define-key input-decode-map "\e[38~" [(control left)]) ;f24
(define-key input-decode-map "\e[39~" [(control right)]) ;f25
(define-key input-decode-map "\e[40~" [(control up)]) ;f26
(define-key input-decode-map "\e[41~" [(control down)]) ;f27
(define-key input-decode-map "\e[42~" [(shift left)]) ;f28
(define-key input-decode-map "\e[43~" [(shift right)]) ;f29
(define-key input-decode-map "\e[44~" [(shift up)]) ;f30
(define-key input-decode-map "\e[45~" [(shift down)]) ;f31
(define-key input-decode-map "\e[46~" [(control shift left)]) ;f32
(define-key input-decode-map "\e[47~" [(control shift right)]) ;f33
(define-key input-decode-map "\e[48~" [(control shift up)]) ;f34
(define-key input-decode-map "\e[49~" [(control shift down)]) ;f35
(define-key input-decode-map "\e[50~" [(shift tab)]) ;f36
(define-key input-decode-map "\e[51~" [(shift return)]) ;f37
(define-key input-decode-map "\e[52~" [(control meta left)]) ;f38
(define-key input-decode-map "\e[53~" [(control meta right)]) ;f39
(define-key input-decode-map "\e[54~" [(control meta up)]) ;f40
(define-key input-decode-map "\e[55~" [(control meta down)]) ;f41

(provide 'console-keys)

(ns shrdlu.core)

(defmacro § [& _])
(defmacro ß [& _])

(defmacro def- [s i] `(def ~(vary-meta s assoc :private true) ~i))

(defmacro lambda [& _] `(fn ~@_))

(def- ascii char)

(def- car first)
(def- cdr next)

(defn- caar [l] (car (car l)))
(defn- cadr [l] (car (cdr l)))
(defn- cdar [l] (cdr (car l)))
(defn- cddr [l] (cdr (cdr l)))

(defn- caaar [l] (car (caar l)))
(defn- caadr [l] (car (cadr l)))
(defn- cadar [l] (car (cdar l)))
(defn- caddr [l] (car (cddr l)))
(defn- cdaar [l] (cdr (caar l)))
(defn- cdadr [l] (cdr (cadr l)))
(defn- cddar [l] (cdr (cdar l)))
(defn- cdddr [l] (cdr (cddr l)))

(defn- caadar [l] (car (cadar l)))
(defn- caaddr [l] (car (caddr l)))
(defn- cadaar [l] (car (cdaar l)))
(defn- cadadr [l] (car (cdadr l)))
(defn- caddar [l] (car (cddar l)))
(defn- cadddr [l] (car (cdddr l)))
(defn- cdadar [l] (cdr (cadar l)))
(defn- cdaddr [l] (cdr (caddr l)))
(defn- cddaar [l] (cdr (cdaar l)))
(defn- cddadr [l] (cdr (cdadr l)))
(defn- cdddar [l] (cdr (cddar l)))
(defn- cddddr [l] (cdr (cdddr l)))

(defn- term? [q] (not (coll? q)))

(defn- memq [x y] (cond (nil? y) nil (= x (car y)) y :else (recur x (cdr y))))
(defn- assq [x y] (cond (nil? y) nil (= x (caar y)) (car y) :else (recur x (cdr y))))
(defn- sassq [x y fun] (or (assq x y) (fun)))

#_(ns shrdlu.blockp)

;; ################################################################
;;
;;     BLOCKP > MICRO-PLANNER CODE FOR THE "BLOCKS" MICRO-WORLD
;;
;; ################################################################

(§ DEFPROP TA-AT
    (THANTE (x y) (!AT ($? x) ($? y)) (THRPLACA (cdr (atab ($? x))) ($? y)))
    THEOREM)

(§ DEFPROP TA-CONTAIN
    (THANTE (x y z)
        (!AT ($? x) ?)
        (THGOAL (!MANIP ($? x)))
        (THGOAL (!SUPPORT ($? y) ($? x)))
        (THOR (THAND (THGOAL (!IS ($? y) !BOX)) (THVSETQ ($_ z) ($? y)))
            (THGOAL (!CONTAIN ($? z) ($? y))))
        (THASSERT (!CONTAIN ($? z) ($? x))))
    THEOREM)

(§ DEFPROP TA-EXISTS
    (THANTE (x) (!EXISTS ($? x)) (THSUCCEED))
    THEOREM)

(def- NOSTACKS true)

(§ DEFPROP TA-SUPP
    (THANTE (x y z)
        (!AT ($? x) ($? y))
        (THCOND ((THVSETQ ($_ z) (SUPPORT ($? y) (size ($? x)) ($? x)))
            (THCOND ((THGOAL (!MANIP ($? z)))
                (THGOAL (!SHAPE ($? z) !RECTANGULAR)))
                ((THSUCCEED)))
            (THASSERT (!SUPPORT ($? z) ($? x)))
            (THCOND ((THGOAL (!CLEARTOP ($? z)))
                (THERASE (!CLEARTOP ($? z))))
                ((THSUCCEED)))
            (THCOND (NOSTACKS)
                ((THNOT (THGOAL (!MANIP ($? z)))))
                ((THAND (THGOAL (!PART ($? z) ($_ y)))
                    (THGOAL (!IS ($? y) !STACK)))
                (THASSERT (!PART ($? x) ($? y))))
                ((THVSETQ ($_ y) (MAKESYM 'STACK))
                (THASSERT (!PART ($? x) ($? y)))
                (THASSERT (!PART ($? z) ($? y)))
                (THASSERT (!IS ($? y) !STACK))
                (THASSERT (!EXISTS ($? y)) (THUSE TA-EXISTS)))))
            ((THGOAL (!GRASPING ($? x))))
            ((ert "TA-SUPP"))))
    THEOREM)

(§ DEFPROP TC-2
    (THCONSE (x y yy)
        (($? x) ($? y))
        (THGOAL (!CHOOSE ($? y) ($_ yy) ($E (get ($? x) 'CHOOSE)))
            (THUSE TC-CHOOSE))
        (THGOAL (($? x) ($? yy)) (THTBF THTRUE)))
    THEOREM)

(§ DEFPROP TC-3
    (THCONSE (x y z YY ZZ)
        (($? x) ($? y) ($? z))
        (THGOAL (!CHOOSE ($? y) ($_ YY) ($E (get ($? x) 'CHOOSE)))
            (THUSE TC-CHOOSE))
        (THGOAL (!CHOOSE ($? z) ($_ ZZ) ($E (get ($? x) 'CHOOSE2)))
            (THUSE TC-CHOOSE))
        (THGOAL (($? x) ($? YY) ($? ZZ)) (THTBF THTRUE)))
    THEOREM)

(§ DEFPROP TC-ASMUCH
    (THCONSE (measure x y)
        (!ASMUCH measure ($? x) ($? y))
        (THVSETQ ($_ measure) (get ($? measure) 'MEASFN))
        (not (< (($? measure) ($? x)) (($? measure) ($? y)))))
    THEOREM)

(§ DEFPROP TC-BELONG
    (THCONSE (x y)
        (!BELONG ($? x) ($? y))
        (THAMONG ($? y) '(ßSHRDLU))
        (THGOAL (!PHYSOB ($? x)) (THUSE TC-PHYSOB)))
    THEOREM)

(§ DEFPROP TC-CALL
    (THCONSE (x y z)
        (!CALL ($? x) ($? y))
        (THCOND ((THGOAL (!CALL ($_ z) ($? y)))
            (PRINT ($? y))
            (not (PRINT 'NAME-ALREADY-USED)))
            ((THASSERT (!CALL ($? x) ($? y)))
            (THASSERT (!IS ($? y) !NAME))
            (!PROPDEFINE ($? y))
            (or DOIT (SETQ PLAN (cons true PLAN))))))
    THEOREM)

(§ DEFPROP TC-CHOOSE
    (THCONSE (x y z w)
        (!CHOOSE ($? x) ($? y) ($? z))
        (THCOND
            ((and (THASVAL ($? x)) (not (oss? ($? x)))) (THSETQ ($_ y) ($? x)))
            ((THASVAL ($? x))
                (or (nil? DISCOURSE)
                    (THPUTPROP (variable? ($? x)) ($? x) 'NG))
                (THSETQ ($_ y) (FINDCHOOSE ($? x) ($? z) nil)))
        ((THGOAL (!MANIP ($? y))) (THNOT (THGOAL (!SUPPORT ($? y) ?))))))
    THEOREM)

(§ DEFPROP TC-CHOOSEPLACE
    (THCONSE (x) (!CHOOSEPLACE ($? x)) (ert "CHOOSEPLACE UNDEFINED"))
    THEOREM)

(§ DEFPROP TC-CLEARTOP
    (THCONSE (x y (WHY (EV)) EV)
        (!CLEARTOP ($? x))
        (term? ($? x))
        (THOR (THGOAL (!SUPPORT ($? x) ?))
        (THAND (THASSERT (!CLEARTOP ($? x))) (THSUCCEED THEOREM)))
        (MEMORY)
    =>  (THCOND ((THGOAL (!SUPPORT ($? x) ($_ y)))
            (THGOAL (!GET-RID-OF ($? y))
                (THNODB)
                (THUSE TC-GET-RID-OF))
            (THGO =>))
            ((THASSERT (!CLEARTOP ($? x)))
            (MEMOREND (!CLEARTOP ($? EV) ($? x)))
            (THSUCCEED THEOREM))))
    THEOREM)

(§ DEFPROP TC-EXISTS
    (THCONSE (x) (!EXISTS ($? x)) (THSUCCEED))
    THEOREM)

(§ DEFPROP TC-FINDSPACE
    (THCONSE (surf size obj space)
        (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space))
        (THOR (and (not (memq ($? surf) '(ßBOX ßTABLE))) (not (get '!NOCLEAR 'THASSERTION))
            (THSETQ ($_ space) (FINDSPACE 'CENTER ($? surf) ($? size) ($? obj))))
        (and (or (= ($? surf) 'ßBOX) (and (not (= ($? surf) 'ßTABLE)) (get '!NOCLEAR 'THASSERTION)))
            (THSETQ ($_ space) (FINDSPACE 'PACK ($? surf) ($? size) ($? obj))))
        (THSETQ ($_ space) (FINDSPACE 'RANDOM ($? surf) ($? size) ($? obj)))))
    THEOREM)

(§ DEFPROP TC-GET-RID-OF
    (THCONSE (x y (WHY (EV)) EV)
        (!GET-RID-OF ($? x))
        (or NOMEM (THVSETQ ($_ EV) ($? WHY)))
    =>  (THCOND ((nil? ($? x)))
            ((term? ($? x))
                (MEMORY)
                (THGOAL (!FINDSPACE ßTABLE ($E (size ($? x))) ($? x) ($_ y)) (THUSE TC-FINDSPACE))
                (THGOAL (!PUT ($? x) ($? y)) (THNODB) (THUSE TC-PUT))
                (MEMOREND (!GET-RID-OF ($? EV) ($? x))))
            ((THGOAL (!GET-RID-OF ($E (car ($? x)))) (THUSE TC-GET-RID-OF))
                (or (THSETQ ($_ x) (cdr ($? x))) (THSUCCEED THEOREM))
                (THGO =>))))
    THEOREM)

(§ DEFPROP TC-GRASP
    (THCONSE (x y (WHY (EV)) EV)
        (!GRASP ($? x))
        (THCOND ((THGOAL (!GRASPING ($? x))) (THSUCCEED THEOREM))
            ((term? ($? x))))
        (MEMORY)
        (THGOAL (!CLEARTOP ($? x)) (THUSE TC-CLEARTOP))
        (THCOND ((THGOAL (!GRASPING ($_ y)))
            (THOR (THGOAL (!UNGRASP) (THNODB) (THUSE TC-UNGRASP))
                (THGOAL (!GET-RID-OF ($? y)) (THNODB) (THUSE TC-GET-RID-OF))))
            ((THSUCCEED)))
        (THSETQ ($_ y) (topcenter ($? x)))
        (THGOAL (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2))
        (THASSERT (!GRASPING ($? x)))
        (MEMOREND (!GRASP ($? EV) ($? x)))
        (or NOMEM
            (THSETQ GRASPLIST (cons (list THTIME ($? x)) GRASPLIST)))
        (THCOND (DOIT (THOR (GRASP ($? x)) (and (UNGRASP) nil)))
            ((THSETQ PLAN (cons (list 'GRASP (list 'quote ($? x))) PLAN)))))
    THEOREM)

(§ DEFPROP TC-LOC
    (THCONSE (x y z loc)
        (($? loc) ($? x) ($? y) ($? z))
        (THOR (THGOAL (!MANIP ($? y))) (THGOAL (!IS ($? y) !BOX)))
        (THOR (THGOAL (!MANIP ($? z))) (THGOAL (!IS ($? z) !BOX)))
        (not (= ($? y) ($? z)))
        (LOCGREATER ($? y) ($? z)
            ((lambda [x] (COND ((= x '!RIGHT) #'car) ((= x '!BEHIND) #'cadr) ((= x '!ABOVE) #'caddr) ((ert "TC-LOC")))) ($? x))))
    THEOREM)

(§ DEFPROP TC-MAKESPACE
    (THCONSE (surf size obj space x (WHY (EV)) EV)
        (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space))
        (THNOT (THGOAL (!IS ($? surf) !BOX)))
        (MEMORY)
    => (THAND (THGOAL (!SUPPORT ($? surf) ($_ x)))
            (THGOAL (!GET-RID-OF ($? x)) (THUSE TC-GET-RID-OF)))
        (THOR (THGOAL (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space)) (THUSE TC-FINDSPACE))
            (THGO =>))
        (MEMOREND (!MAKESPACE ($? EV) ($? surf))))
    THEOREM)

(§ DEFPROP TC-MORE
    (THCONSE (measure x y)
        (!MORE ($? measure) ($? x) ($? y))
        (THVSETQ ($_ measure) (get ($? measure) 'MEASFN))
        (> (($? measure) ($? x)) (($? measure) ($? y))))
    THEOREM)

(§ DEFPROP TC-MOVEHAND
    (THCONSE (x y w z)
        (!MOVEHAND ($? y))
        (THCOND
            ((= HANDAT ($? y)) (THSUCCEED THEOREM))
            ((THGOAL (!GRASPING ($? x)))
                (THVSETQ ($_ z) (let [x (atab ($? x)) y (diff ($? y) (tcent '(0 0 0) (caddr x)))]
                    (and (CLEAR y (list (caaddr x) (cadadr (cdr x)) (- 512 (caddr y))) (car x))
                        (RETURN y))
                    nil))
                (THGOAL (!AT ($? x) ($_ w)))
                (THERASE (!AT ($? x) ($? w)) (THUSE TE-SUPP TE-CONTAIN))
                (THASSERT (!AT ($? x) ($? z)) (THUSE TA-AT TA-SUPP TA-CONTAIN))
                (THGOAL (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2))
                (or NOMEM
                    (THPUTPROP ($? x)
                        (cons (list THTIME
                                ($? z)
                                (cadar (or (THVAL '(THGOAL (!SUPPORT ($? y) ($? x)))
                                        (cons (list 'Y 'THUNASSIGNED) THALIST))
                                    '((nil ßHAND))))
                                nil)
                            (get ($? x) 'HISTORY))
                        'HISTORY)))
        ((THGOAL (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2)))))
    THEOREM)

(§ DEFPROP TC-MOVEHAND2
    (THCONSE (y loc)
        (!MOVEHAND2 ($? y))
        (COND ((= ($? y) HANDAT) (THSUCCEED THEOREM))
            ((and (< 31 (car ($? y)) 609) (< -1 (cadr ($? y)) 609) (< -1 (caddr ($? y)) 513))))
        (THVSETQ ($_ loc) HANDAT)
        (THSETQ HANDAT ($? y))
        (THSETQ THTIME (inc THTIME))
        (THCOND (DOIT (THOR (eval (cons 'MOVETO HANDAT))
                    (let [ADJUST nil] (eval (cons 'MOVETO ($? loc))) nil)))
            ((THSETQ PLAN (cons (cons 'MOVETO ($? y)) PLAN)))))
    THEOREM)

(§ DEFPROP TC-NAME
    (THCONSE (x)
        (!NAME ($? x))
        (THVSETQ ($_ x) (listify ($? x)))
        (THVSETQ ($_ x) (THFIND ALL ($? y) (y z) (THAMONG ($? z) ($? x)) (THOR (THGOAL (!CALL ($? z) ($? y))) (THSETQ ($_ y) ($? z)))))
        (dorun (map #'PRINT ($? x))))
    THEOREM)

(§ DEFPROP TC-NOTICE
    (THCONSE (x)
        (!NOTICE ($? x))
        (COND (DOIT (BLINK ($? x)) (THSUCCEED))
            ((THSETQ PLAN (cons (list 'BLINK (list 'quote ($? x))) PLAN)))))
    THEOREM)

(§ DEFPROP TC-ON
    (THCONSE (x y z)
        (!ON ($? x) ($? y))
        (THOR (THGOAL (!SUPPORT ($? y) ($? x))) (THAND (THASVAL ($? x)) (THGOAL (!SUPPORT ($_ z) ($? x))) (THGOAL (!ON ($? z) ($? y)) (THUSE TC-ON)))))
    THEOREM)

(§ DEFPROP TC-PACK
    (THCONSE (OBJ SURF BLOCKS PYR x y)
        (!PACK ($? OBJ) ($? SURF))
        (or (THVSETQ ($_ BLOCKS) (PACKO ($? OBJ) '!BLOCK)) true)
        (or (THVSETQ ($_ PYR) (PACKO ($? OBJ) '!PYRAMID)) true)
    =>  (THCOND ((nil? ($? BLOCKS))
            (THCOND ((nil? ($? PYR)) (THSUCCEED THEOREM))
                ((THVSETQ ($_ y) (FINDSPACE 'PACK ($? SURF) (size (car ($? PYR))) (car ($? PYR))))
                    (THGOAL (!PUT ($E (car ($? PYR))) ($? y)) (THUSE TC-PUT))
                    (or (THSETQ ($? PYR) (cdr ($? PYR))) true)
                    (THGO =>))))
                ((THSETQ ($_ x) (car ($? BLOCKS)))
                    (THVSETQ ($? y) (FINDSPACE 'PACK ($? SURF) (size ($? x)) ($? x)))
                    (THGOAL (!PUT ($? x) ($? y)) (THUSE TC-PUT))
                    (or (THSETQ ($? BLOCKS) (cdr ($? BLOCKS))) true)
                    (THCOND ((THVSETQ ($_ y) (or (PACKON ($? x) ($? PYR)) (PACKON ($? x) ($? BLOCKS))))
                            (THGOAL (!PUTON ($? y) ($? x)) (THUSE TC-PUTON))
                            (COND ((memq ($? y) ($? PYR))
                                    (THSETQ ($_ PYR) (DELQ ($? y) (concat ($? PYR) nil))))
                                ((THSETQ ($_ BLOCKS) (DELQ ($? y) (concat ($? BLOCKS) nil))))))
                        ((THSUCCEED)))
                    (THGO =>))))
    THEOREM)

(§ DEFPROP TC-PART
    (THCONSE (x y z)
        (!PART ($? x) ($? y))
        (THGOAL (!IS ($? y) !STACK))
        (THGOAL (!CHOOSE ($? x) ($_ z) '(((THGOAL (!PART ($? *) ($? y)))))) (THUSE TC-CHOOSE))
        (or (not (term? ($? z))) (THSETQ ($_ z) (list ($? z))))
    =>  (THCOND ((nil? ($? z)) (THSUCCEED))
            ((THGOAL (!PART ($E (car ($? z))) ($? y)))
                (or (THSETQ ($_ z) (cdr ($? z))) true)
                (THGO =>))
            ((THFAIL))))
    THCONSE)

(§ DEFPROP TC-PHYSOB
    (THCONSE (x)
        (!PHYSOB ($? x))
        (THOR (THGOAL (!MANIP ($? x))) (THAMONG ($? x) '(ßBOX ßTABLE ßHAND))))
    THEOREM)

(§ DEFPROP TC-PICKUP
    (THCONSE (x (WHY (EV)) EV)
        (!PICKUP ($? x))
        (MEMORY)
        (THGOAL (!GRASP ($? x)) (THUSE TC-GRASP))
        (THGOAL (!RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
        (MEMOREND (!PICKUP ($? EV) ($? x))))
    THEOREM)

(§ DEFPROP TC-REFERS
    (THCONSE (x)
        (!REFERS ($? x))
        (eval (list 'THSETQ (list 'THV ($? x)) (list 'quote (atomify (get ($? x) 'BIND))))))
    THEOREM)

(§ DEFPROP TC-PUT
    (THCONSE (x y z)
        (!PUT ($? x) ($? y))
        (THCOND ((THASVAL ($? y))
                (THCOND ((term? ($? y)) (THGOAL (!CHOOSEPLACE ($? y)) (THUSE TC-CHOOSEPLACE)))
                    ((THSUCCEED))))
            ((THGOAL (!GET-RID-OF ($? x)) (THNODB) (THUSE TC-GET-RID-OF))
                (THSUCCEED THEOREM)))
        (CLEAR ($? y) (size ($? x)) ($? x))
        (SUPPORT ($? y) (size ($? x)) ($? x))
        (THGOAL (!GRASP ($? x)) (THUSE TC-GRASP))
        (THSETQ ($_ z) (tcent ($? y) (size ($? x))))
        (THGOAL (!MOVEHAND ($? z)) (THNODB) (THUSE TC-MOVEHAND))
        (THGOAL (!UNGRASP) (THNODB) (THUSE TC-UNGRASP)))
    THEOREM)

(§ DEFPROP TC-PUTIN
    (THCONSE (x y z (WHY (EV)) EV)
        (!PUTIN ($? x) ($? y))
        (MEMORY)
        (THCOND ((THGOAL (!PUTON ($? x) ($? y)) (THUSE TC-PUTON))
                (MEMOREND (!PUTIN ($? EV) ($? x) ($? y)))
                (THSUCCEED THEOREM))
            ((THSUCCEED)))
        (THGOAL (!IS ($? y) !BOX))
        (THVSETQ ($_ z)
            (UNION (listify ($? x))
                (THVAL '(THFIND ALL ($? w) (w) (THGOAL (!ON ($? w) ($? y)))) THALIST)))
        (THGOAL (!CLEARTOP ($? y)) (THUSE TC-CLEARTOP))
        (THGOAL (!PACK ($? z) ($? y)) (THUSE TC-PACK))
        (MEMOREND (!PUTIN ($? EV) ($? x) ($? y))))
    THEOREM)

(§ DEFPROP TC-PUTON
    (THCONSE (x y z (WHY (EV)) EV)
        (!PUTON ($? x) ($? y))
        (term? ($? y))
        (or (cdr ($? x)) (THSETQ ($_ x) (car ($? x))))
        (not (COND ((term? ($? x)) (= ($? x) ($? y))) ((memq ($? y) ($? x)))))
        (MEMORY)
        (THCOND ((term? ($? x))
                (THGOAL (!CLEARTOP ($? x)) (THUSE TC-CLEARTOP))
                (THOR (THGOAL (!FINDSPACE ($? y) ($E (size ($? x))) ($? x) ($_ z)) (THUSE TC-FINDSPACE))
                    (and (nil? (get '!NOCLEAR 'THASSERTION))
                        (THGOAL (!FINDSPACE ($? y) ($E (size ($? x))) ($? x) ($_ z)) (THUSE TC-MAKESPACE))))
                (THGOAL (!PUT ($? x) ($? z)) (THNODB) (THUSE TC-PUT)))
            ((THASSERT (!NOCLEAR))
                (THPROG ((w ($? x)))
                =>  (THOR (THGOAL (!PUTON ($E (car ($? w))) ($? y)) (THUSE TC-PUTON))
                        (THFAIL THPROG))
                    (THOR (THSETQ ($? w) (cdr ($? w))) (THRETURN true))
                    (THGO =>))
            (THERASE (!NOCLEAR)))
            ((THNOT (THGOAL (!IS ($? y) !BOX)))
                (THGOAL (!CLEARTOP ($? y)) (THUSE TC-CLEARTOP))
                (THGOAL (!PACK ($? x) ($? y)) (THUSE TC-PACK))))
        (MEMOREND (!PUTON ($? EV) ($? x) ($? y))))
    THEOREM)

(§ DEFPROP TC-RAISEHAND
    (THCONSE ((WHY (EV)) EV)
        (!RAISEHAND)
        (MEMORY)
        (THGOAL (!MOVEHAND ($E (list (car HANDAT) (cadr HANDAT) 512))) (THNODB) (THUSE TC-MOVEHAND))
        (MEMOREND (!RAISEHAND ($? EV))))
    THEOREM)

(§ DEFPROP TC-STACK
    (THCONSE (x y)
        (!IS ($? x) !STACK)
        (not (THASVAL ($? x)))
        (THGOAL (!MANIP ($? y)))
        (THGOAL (!SUPPORT ($? y) ?))
        (THNOT (THAND (THGOAL (!PART ($? y) ($_ x)))
            (THGOAL (!IS ($? x) !STACK))))
    =>  (THGOAL (!SUPPORT ($_ x) ($? y)))
        (THCOND ((memq ($? x) '(ßTABLE ßBOX)))
            ((THSETQ ($_ y) ($? x)) (THGO =>)))
        (THSETQ ($_ x) (MAKESYM 'STACK))
        (THASSERT (!IS ($? x) !STACK))
        (THASSERT (!EXISTS ($? x)))
        (THFIND ALL
            ($? z)
            (z)
            (THGOAL (!ON ($? z) ($? y)) (THUSE TC-ON))
            (THAND (THASSERT (!PART ($? z) ($? x))) (THFINALIZE THAND))))
    THEOREM)

(§ DEFPROP TC-STACKUP
    (THCONSE (x y BLOCKS PYR (WHY (EV)) EV)
        (!STACKUP ($? x))
        (or (< (reduce #'+ (map #'(lambda [x] (caddr (size x))) ($? x))) 641)
            (not (DPRINT2 "TOO HIGH,")))
        (THCOND
            ((and ($? x) (cdr ($? x))))
            ((THSETQ ($_ x)
                (concat ($? x)
                    (THVAL (list 'THFIND
                        (COND ((nil? ($? x)) 3) (2))
                        '($? y)
                        '(y)
                        '(THOR (THAND (THGOAL (!IS ($? y) !BLOCK)) (THNOT (THGOAL (!SUPPORT ($? y) ?)))) (THGOAL (!IS ($? y) !BLOCK)))
                        '(not (= ($? x) ($? y))))
                        THALIST)))))
        (COND ((THVSETQ ($_ PYR) (PACKO ($? x) '!PYRAMID)) (nil? (cdr ($? PYR)))) (:else true))
        (THVSETQ ($_ BLOCKS) (cons 'ßTABLE (PACKO ($? x) '!BLOCK)))
        (MEMORY)
    =>  (THCOND
            ((cdr ($? BLOCKS))
                (THGOAL (!PUTON ($E (cadr ($? BLOCKS))) ($E (car ($? BLOCKS)))) (THUSE TC-PUTON))
                (THSETQ ($_ BLOCKS) (cdr ($? BLOCKS)))
                (THGO =>))
            (($? PYR) (THGOAL (!PUTON ($E (car ($? PYR))) ($E (car ($? BLOCKS)))) (THUSE TC-PUTON)))
            ((MEMOREND (!STACKUP ($? EV) ($? x))))))
    THEOREM)

(§ DEFPROP TC-STARTEND3
    (THCONSE (x y EV TIME) (($? x) ($? EV) ($? TIME)) (THGOAL (($? x) ($? y) ($? EV) ($? TIME)) (THUSE TC-STARTEND4)))
    THEOREM)

(§ DEFPROP TC-STARTEND4
    (THCONSE (x NEWEV z EV TIME)
        (($? x) ($? NEWEV) ($? EV) ($? TIME))
        (or (and (THASVAL ($? x)) (THASVAL ($? EV)) (THASVAL ($? TIME)) (not (THASVAL ($? NEWEV)))) (ert "TC-STARTEND4"))
        (THGOAL (!CHOOSE ($? EV) ($_ z) nil) (THUSE TC-CHOOSE))
        (or (term? ($? z)) (ert "TC-STARTEND4 ATOM"))
        (THSETQ ($_ NEWEV) (MAKESYM 'EV))
        (PUTPROP ($? NEWEV)
            (PUTPROP ($? NEWEV)
                (get ($? z) (COND ((= ($? x) '!START) 'START) ((= ($? x) '!END) 'END) ((ert "TC-STARTEND (THV x)"))))
                'START)
            'END)
        (timechk ($? NEWEV) ($? TIME))
        (PUTPROP ($? NEWEV) ($? z) 'WHY)
        (PUTPROP ($? NEWEV) '!START 'TYPE))
    THEOREM)

(§ DEFPROP TC-UNGRASP
    (THCONSE (x OBJ (WHO (EV)) EV)
        (!UNGRASP)
        (THCOND ((THGOAL (!GRASPING ($? x)))
                (MEMORY)
                (THGOAL (!SUPPORT ? ($? x)))
                (THERASE (!GRASPING ($? x)))
                (MEMOREND (!UNGRASP ($? EV) ($? x)))
                (THSETQ THTIME (inc THTIME))
                (THCOND (DOIT (THOR (UNGRASP) (and (GRASP ($? x)) nil)))
                    ((THSETQ PLAN (cons '(UNGRASP) PLAN)))))
            ((THSUCCEED))))
    THEOREM)

(§ DEFPROP TC-WANT4
    (THCONSE (x EV TIME y)
        (!WANT ($? x) ($? EV) ($? TIME))
        (THGOAL (!WANT ($? y) ($? x) ($? EV) ($? TIME)) (THUSE TC-WANT5)))
    THEOREM)

(§ DEFPROP TC-WANT5
    (THCONSE (x NEWEV EV TIME z)
        (!WANT ($? NEWEV) ($? x) ($? EV) ($? TIME))
        (or (and (THASVAL ($? x)) (THASVAL ($? EV)) (THASVAL ($? TIME))) (ert "TC-WANT5 THASVAL"))
        (= ($? x) 'ßFRIEND)
        (= (get ($? EV) 'WHY) 'COMMAND)
        (THSETQ ($_ NEWEV) (MAKESYM 'EV))
        (PUTPROP ($? NEWEV)
            (PUTPROP ($? NEWEV)
                (get ($? EV) 'START)
                'START)
            'END)
        (timechk ($? NEWEV) ($? TIME))
        (PUTPROP ($? NEWEV) '!TELL 'TYPE)
        (PUTPROP ($? NEWEV) 'ESP 'WHY))
    THEOREM)

(§ DEFPROP TCT-EXISTS
    (THCONSE nil (!EXISTS ? ?) (THSUCCEED))
    THEOREM)

(§ DEFPROP TCT-PICKUP
    (THCONSE (x EV TIME)
        (!PICKUP ($? x) ($? TIME))
        (THOR (THAND (THGOAL (!PICKUP($? EV) ($? x))) (timechk ($? EV) ($? TIME)))
            (THGOAL (!PICKUP ($? EV) ($? x) ($? TIME)) (THUSE TCTE-PICKUP))))
    THEOREM)

(§ DEFPROP TCT-PUT
    (THCONSE (x EV TIME y)
        (!PUT ($? x) ($? y) ($? TIME))
        (THGOAL (!PUT ($? EV) ($? x) ($? y) ($? TIME)) (THUSE TCTE-PUT)))
    THEOREM)

(§ DEFPROP TCT-AT
    (THCONSE (x y z TIME w)
        (!AT ($? y) ($? z) ($? TIME))
        (THOR (THGOAL (!MANIP ($? y)))
            (THAND (THGOAL (!IS ($? y) !BOX)) (THGOAL (!AT ($? y) ($? z))) (THSUCCEED THEOREM)))
        (THSETQ ($_ x) (TFIND ($? y) ($? TIME)))
        (THOR (THSETQ ($_ w) (car ($? x)))
            (THAND (THAMONG ($? w) (cdr ($? x))) (or (not (< (car ($? w)) (or (start? ($? TIME)) -1))) (THFAIL THAND))))
        (THSETQ ($? z) (cadr ($? w))))
    THEOREM)

(§ DEFPROP TCT-LOC
    (THCONSE (YY ZZ x y z TIME)
        (!LOC ($? x) ($? y) ($? z) ($? TIME))
        (THGOAL (!AT ($? y) ($? YY) ($? TIME)) (THUSE TCT-AT))
        (THGOAL (!AT ($? z) ($? ZZ) ($? TIME)) (THUSE TCT-AT))
        (THGOAL (!TLOC ($? x) ($? y) ($? z)) (THUSE TC-LOC)))
    THEOREM)

(§ DEFPROP TCT-SUPPORT
    (THCONSE (x y z TIME)
        (!SUPPORT ($? x) ($? y) ($? TIME))
        (THOR (THGOAL (!MANIP ($? y))) (THGOAL (!IS ($? y) !BOX)))
        (THAMONG ($? z) (TFIND ($? y) ($? TIME)))
        (not (< (car ($? z)) (or (start? ($? TIME)) -1)))
        (THAMONG ($? x) (list (caddr ($? z)))))
    THEOREM)

(§ DEFPROP TCT-2
    (THCONSE (x EV TIME) (($? x) ($? TIME)) (THGOAL (($? x) ($? EV) ($? TIME)) (THUSE TCTE-3)))
    THEOREM)

(§ DEFPROP TCT-3
    (THCONSE (x y EV TIME) (($? x) ($? y) ($? TIME)) (THGOAL (($? x) ($? EV) ($? y) ($? TIME)) (THUSE TCTE-4)))
    THEOREM)

(§ DEFPROP TCT-4
    (THCONSE (x y z EV TIME) (($? x) ($? y) ($? z) ($? TIME)) (THGOAL (($? x) ($? EV) ($? y) ($? z) ($? TIME)) (THUSE TCTE-5)))
    THEOREM)

(§ DEFPROP TCTE-PICKUP
    (THCONSE (x EV EVENT TIME)
        (!PICKUP ($? EV) ($? x) ($? TIME))
        (THOR (THAND (THGOAL (!PICKUP ($? EV) ($? x))) (timechk ($? EV) ($? TIME)) (THSUCCEED THEOREM))
            (THSUCCEED))
        (THAMONG ($? EVENT) EVENTLIST)
        (memq (get ($? EVENT) 'TYPE) '(!PUTON !GET-RID-OF))
        (timechk ($? EVENT) ($? TIME))
        (THOR (THGOAL (!PUTON ($? EVENT) ($? x) ?))
            (THGOAL (!GET-RID-OF ($? EVENT) ($? x))))
        (THVSETQ ($_ EV) (MAKESYM 'E))
        (and (PUTPROP ($? EV) (PUTPROP ($? EV) (get ($? EVENT) 'END) 'START) 'END)
            (PUTPROP ($? EV) '!PICKUP 'TYPE)
            (PUTPROP ($? EV) ($? EVENT) 'WHY)
            (SETQ EVENTLIST (cons ($? EV) EVENTLIST))
            (THASSERT (!PICKUP ($? EV) ($? x)))))
    THEOREM)

(§ DEFPROP TCTE-PUT
    (THCONSE (x y EV EVENT TIME z)
        (!PUT ($? EV) ($? x) ($? y) ($? TIME))
        (THAMONG ($? EVENT) EVENTLIST)
        (memq (get ($? EVENT) 'TYPE) '(!PICKUP !PUTON))
        (timechk ($? EVENT) ($? TIME))
        (THOR (THGOAL (!PUTON ($? EVENT) ($? x) ?))
            (THGOAL (!PICKUP ($? EVENT) ($? x))))
        (or (THVSETQ ($_ z) (dec (assq (get ($? EVENT) 'END) (get ($? x) 'HISTORY))))
            (ert "TCTE-PUT WRONG"))
        (THAMONG ($? y) (list (cadr ($? z))))
        (THSETQ ($_ EV) (MAKESYM 'E))
        (and (PUTPROP ($? EV) (PUTPROP ($? EV) (car ($? z)) 'START) 'END)
            (PUTPROP ($? EV) ($? EVENT) 'WHY)
            (PUTPROP ($? EV) '!PUT 'TYPE)
            (SETQ EVENTLIST (cons ($? EV) EVENTLIST))
            (THASSERT (!PUT ($? EV) ($? x) ($? y)))))
    THEOREM)

(§ DEFPROP TCTE-3
    (THCONSE (x EV TIME)
        (($? x) ($? EV) ($? TIME))
        (or (THASVAL TIME) (ert "TCTE-3"))
        (THGOAL (($? x) ($? EV)))
        (timechk ($? EV) ($? TIME)))
    THEOREM)

(§ DEFPROP TCTE-4
    (THCONSE (x y EV TIME)
        (($? x) ($? EV) ($? y) ($? TIME))
        (or (THASVAL ($? TIME)) (ert "TCTE-4"))
        (THGOAL (($? x) ($? EV) ($? y)))
        (timechk ($? EV) ($? TIME)))
    THEOREM)

(§ DEFPROP TCTE-5
    (THCONSE (x y z EV TIME)
        (($? x) ($? EV) ($? y) ($? z) ($? TIME))
        (or (THASVAL ($? TIME)) (ert "TCT-5"))
        (THGOAL (($? x) ($? EV) ($? y) ($? z)))
        (timechk ($? EV) ($? TIME)))
    THEOREM)

(§ DEFPROP TCT-GRASP
    (THCONSE (x z TIME)
        (!GRASP ($? x) ($? TIME))
        (THVSETQ ($_ z) (ENDTIME GRASPLIST ($? TIME)))
    =>  (THCOND ((or (nil? ($? z)) (startime ($? z) ($? TIME))) (THFAIL))
            ((or (and (not (THASVAL ($? x))) (THSETQ ($_ x) (cadar ($? z)))) (= ($? x) (cadar ($? z)))))
            ((THSETQ ($_ z) (cdr ($? z))) (THGO =>))
            ((THFAIL))))
    THEOREM)

(§ DEFPROP TE-CONTAIN
    (THERASING (x y)
        (!AT ($? x) ?)
        (THGOAL (!CONTAIN ($_ y) ($? x)))
        (THERASE (!CONTAIN ($? y) ($? x))))
    THEOREM)

(§ DEFPROP TE-EXISTS
    (THERASING (x) (!EXISTS ($? x)) (THSUCCEED))
    THEOREM)

(§ DEFPROP TE-SUPP
    (THERASING (x y z)
        (!AT ($? x) ?)
        (THCOND ((THGOAL (!SUPPORT ($? x) ($_ y))) (ert "TE-SUPP"))
            ((THGOAL (!SUPPORT ($_ y) ($? x)))
                (THERASE (!SUPPORT ($? y) ($? x)))
                (THCOND
                    ((THGOAL (!PART ($? x) ($_ y)))
                        (THERASE (!PART ($? x) ($? y)))
                        (THCOND ((THFIND 2 ($? w) (w) (THGOAL (!PART ($? w) ($? y))))
                                (THSUCCEED THEOREM))
                            ((THGOAL (!PART ($_ z) ($? y)))
                                (THERASE (!PART ($? z) ($? y))))
                            ((THSUCCEED)))
                        (THERASE (!EXISTS ($? y)) (THUSE TE-EXISTS)))
                    ((THSUCCEED))))))
    THEOREM)

(defn- topcenter [x] (let [x (atab x)] (tcent (cadr x) (caddr x))))

(§ DEFPROP !CLEARTOP
    (((THGOAL (!SUPPORT ($? *) ?))))
    CHOOSE)

(§ DEFPROP !GRASP
    (((THNOT (THGOAL (!GRASPING ($? *)))) (THGOAL (!CLEARTOP ($? *))))
     ((THNOT (THGOAL (!GRASPING ($? *))))))
    CHOOSE)

(§ DEFPROP !PICKUP
    (((THGOAL (!SUPPORT ? ($? *))) (THGOAL (!CLEARTOP ($? *))))
     ((THGOAL (!SUPPORT ? ($? *)))))
    CHOOSE)

(§ DEFPROP !PUTIN
    (((THNOT (THGOAL (!CONTAIN ßBOX ($? *)))) (THGOAL (!CLEARTOP ($? *))))
     ((THNOT (THGOAL (!CONTAIN ßBOX ($? *))))))
    CHOOSE)

(§ DEFPROP !PUTIN
    (((THGOAL (!IS ($? *) !BOX))))
    CHOOSE2)

(§ DEFPROP !PUTON
    (((THGOAL (!CLEARTOP ($? *)))))
    CHOOSE)

(§ DEFPROP !PUTON
    (((THGOAL (!CLEARTOP ($? *))) (THNOT (THGOAL (!IS ($? *) !PYRAMID))))
     ((THNOT (THGOAL (!IS ($? *) !PYRAMID)))))
    CHOOSE2)

(§ DEFPROP !STACKUP
    (((THGOAL (!CLEARTOP ($? *))) (THNOT (THGOAL (!IS ($? *) !PYRAMID))))
     ((THNOT (THGOAL (!IS ($? *) !PYRAMID)))))
    CHOOSE)

(§ THDATA)
    (TC-CALL)
    (TC-CLEARTOP)
    (TC-GET-RID-OF)
    (TC-GRASP)
    (TC-NAME)
    (TC-NOTICE)
    (TC-PACK)
    (TC-PICKUP)
    (TC-PUTIN)
    (TC-PUTON)
    (TC-RAISEHAND)
    (TC-STACKUP)
    (TC-UNGRASP)
    (TC-ON)
    (TC-PHYSOB)
nil

#_(ns shrdlu.blockl)

;; ################################################################
;;
;;            BLOCKL - LISP CODE FOR THE BLOCKS WORLD
;;
;; ################################################################

(defn- abs [x] (if (neg? x) (- x) x))

(defn- atab [x] (or (assq x ATABLE) (ert "ATABLE")))

(§ defn- CLEAR [loc size obj]
    (let [w nil X1 nil X2 nil]
        (SETQ obj (listify obj))
        (and (memq nil (doall (map #'(lambda [x y] (and (> x -1) (> 641 (+ x y)) true)) loc size)))
            (RETURN nil))
        (SETQ w ATABLE)
    =>  (COND ((nil? w) (RETURN loc))
            ((memq (caar w) obj))
            ((and (< (car loc) (+ (car (SETQ X1 (cadar w))) (car (SETQ X2 (caddar w)))))
                (< (car X1) (+ (car loc) (car size)))
                (< (cadr loc) (+ (cadr X1) (cadr X2)))
                (< (cadr X1) (+ (cadr loc) (cadr size)))
                (< (caddr loc) (+ (caddr X1) (caddr X2)))
                (< (caddr X1) (+ (caddr loc) (caddr size))))
            (RETURN nil)))
        (SETQ w (cdr w))
        (GO =>)))

(defn- diff [x y] (doall (map - x y)))

(defn- half [x] (/ x 2))

(§ defn- ENDTIME [l time]
    (let [y nil]
        (or (SETQ y (end? time)) (RETURN l))
    =>  (COND ((nil? l) (RETURN nil))
            ((not (> (caar l) y)) (RETURN l))
            ((SETQ l (cdr l)) (GO =>)))))

(§ defn- EV [] (or NOMEM ($? EV)))

(§ defn- FINDSPACE [type surf size obj]
    (let [xymax nil xymin nil n nil v nil x1 nil x2 nil]
        (SETQ obj (listify obj))
        (and (memq surf obj) (RETURN nil))
        (COND ((= surf 'ßTABLE)
                (SETQ xymin '(0 0))
                (SETQ xymax '(640 640))
                (SETQ LEVEL 0)
                (GO ON))
            ((SETQ X (atab surf))))
        (COND
            ((= type 'CENTER)
                (COND ((CLEAR (SETQ v
                        (list (max 0 (+ (caadr X) (half (- (caaddr X) (car size)))))
                            (max 0 (+ (cadadr X) (half (- (cadr (caddr X)) (cadr size)))))
                            (+ (caddr (cadr X)) (caddr (caddr X)))))
                        size
                        obj)
                    (RETURN v))
                ((RETURN nil))))
            ((= (car X) 'ßBOX)
                (SETQ xymin (list (caadr X) (cadadr X)))
                (SETQ xymax (list (+ (caaddr X) (caadr X)) (+ (cadr (caddr X)) (cadadr X))))
                (SETQ LEVEL 1))
            ((SETQ x1 (half (car size)))
                (SETQ y1 (half (cadr size)))
                (SETQ xymax (list (min 640 (dec (+ (caaddr X) (caadr X) x1))) (min 640 (dec (+ (cadr (caddr X)) (cadadr X) y1)))))
                (SETQ xymin (list (max 0 (- (caadr X) x1)) (max 0 (- (cadadr X) y1))))
                (SETQ LEVEL (+ (caddr (cadr X)) (caddr (caddr X))))))
    ON  (SETQ n 8)
        (SETQ x1 (- (car xymax) (car xymin)))
        (SETQ y1 (- (cadr xymax) (cadr xymin)))
    GO  (COND ((zero? (SETQ n (dec n))) (RETURN nil))
            ((or (not (SETQ v
                        (GROW (list
                                (+ (car xymin) (rem (abs (RANDOM)) x1))
                                (+ (cadr xymin) (rem (abs (RANDOM)) y1))
                                LEVEL)
                            xymin xymax obj)))
                    (< (- (caadr v) (caar v)) (car size))
                    (< (- (cadadr v) (cadar v)) (cadr size)))
                (GO GO))
            ((RETURN (COND
                ((= type 'RANDOM)
                    (list (half (- (+ (caar v) (caadr v)) (car size)))
                        (half (- (+ (cadar v) (cadadr v)) (cadr size)))
                        LEVEL))
                ((= type 'PACK)
                    (list (caar v) (cadar v) LEVEL))
                ((ert "FINDSPACE -- type"))))))))

(§ defq- GOAL [x]
    (SETQ PLAN nil)
    (THVAL (list 'THGOAL (car x) '(THTBF THTRUE)) '((EV COMMAND)))
    (evlis (reverse PLAN)))

(§ defn- GROW [loc _min _max obj]
    (let [GROW nil XL nil XH nil XO nil YL nil YH nil YO nil]
        (SETQ obj (listify obj))
        (COND
            ((or
                (neg? (caar (SETQ XL (list (list (- (car loc) (car _min)) nil)))))
                (neg? (caar (SETQ XH (list (list (- (car _max) (car loc)) nil)))))
                (neg? (caar (SETQ YL (list (list (- (cadr loc) (cadr _min)) nil)))))
                (neg? (caar (SETQ YH (list (list (- (cadr _max) (cadr loc)) nil)))))
                (nil? (ERRSET
                    (dorun (map #'(lambda [x]
                        (let [XX nil YY nil]
                            (COND ((or (memq (car x) obj)
                                    (not (< (caadr x) (car _max)))
                                    (not (< (cadadr x) (cadr _max)))
                                    (not (> (SETQ XX (+ (caadr x) (caaddr x))) (car _min)))
                                    (not (> (SETQ YY (+ (cadadr x) (cadr (caddr x)))) (cadr _min)))
                                    (not (> (+ (caddr (cadr x)) (caddr (caddr x))) (caddr loc))))
                                (RETURN nil))
                            ((> (caadr x) (car loc))
                                (SETQ XH (order (list (- (caadr x) (car loc)) (car x)) XH)))
                            ((< XX (car loc))
                                (SETQ XL (order (list (- (car loc) XX) (car x)) XL)))
                            ((SETQ XO (cons (car x) XO))))
                            (COND ((> (cadadr x) (cadr loc))
                                (SETQ YH (order (list (- (cadadr x) (cadr loc)) (car x)) YH)))
                            ((< YY (cadr loc))
                                (SETQ YL (order (list (- (cadr loc) YY) (car x)) YL)))
                            ((memq (car x) XO) (ERR nil))
                            ((SETQ YO (cons (car x) YO))))
                            nil))
                    ATABLE)))))
                (RETURN nil)))
    =>   (COND ((== (SETQ GROW (min (caar XL) (caar XH) (caar YL) (caar YH))) 1024)
            (RETURN (list
                (list (- (car loc) (cadar XL)) (- (cadr loc) (cadar YL)))
                (list (+ (car loc) (cadar XH)) (+ (cadr loc) (cadar YH))))))
            ((dorun (map #'(lambda [y z w] (let [x (eval w)]
                        (COND ((> (caar x) GROW))
                            ((or (nil? (cadar x)) (memq (cadar x) (eval y)))
                                (RPLACA x (list 2000 (caar x))))
                            ((SET z (cons (cadar x) (eval z)))
                                (SET w (cdr x))))
                        nil))
                    '(YO YO XO XO)
                    '(XO XO YO YO)
                    '(XL XH YL YH)))
                (GO =>)))))

(defn- listify [x] (if (term? x) (list x) x))

(§ defn- LOCGREATER [x y fun]
    ((lambda [XX YY] (not (< (fun (cadr XX)) (+ (fun (cadr YY)) (fun (caddr YY))))))
        (locg2 '($? YY) x)
        (locg2 '($? ZZ) y)))

(defn- locg2 [x y]
    (if (= ($? LOC) '!LOC) (atab y) (cons nil (cons (eval x) (cddr (atab y))))))

(§ defq- MEMOREND [a]
    (or NOMEM
        (and (PUTPROP ($? EV) THTIME 'END)
            (APPLY #'THASSERT (list (THVARSUBST (car a) nil)))
            (PUTPROP ($? EV) (caar a) 'TYPE))))

(§ defn- MEMORY []
    (or NOMEM
        (THAND (THVSETQ ($_ EV) (MAKESYM 'E))
            (THSETQ EVENTLIST (cons ($? EV) EVENTLIST))
            (PUTPROP ($? EV) THTIME 'START)
            (PUTPROP ($? EV) ($? WHY) 'WHY))))

(§ defn- OCCUPIER [x y z]
    (let [w nil X1 nil X2 nil]
        (COND ((neg? z) (RETURN 'ßTABLE)))
        (SETQ w ATABLE)
    =>  (COND ((nil? w) (RETURN nil))
            ((and (< (dec (car (SETQ X1 (cadar w)))) x (+ (car X1) (car (SETQ X2 (caddar w)))))
                (< (dec (cadr X1)) y (+ (cadr X1) (cadr X2)))
                (< (dec (caddr X1)) z (+ (caddr X1) (caddr X2))))
            (RETURN (caar w))))
        (SETQ w (cdr w))
        (GO =>)))

(defn- order [x y]
    (cond (nil? y) (list x)
        (> (car x) (caar y)) (cons (car y) (order x (cdr y)))
        :else (cons x y)))

(§ defn- PACKO [obj TYPE]
    (let [XX nil]
        (dorun (map #'(lambda [x]
            (and (THVAL '(THGOAL (!IS ($? x) ($E TYPE))) (list (list 'X x))) (SETQ XX (packord x (size x) XX))))
        obj))
        (doall (map #'cadr XX))))

(§ defn- PACKON [surf l]
    (let [x nil]
        (SETQ surf (atab surf))
    =>  (COND ((nil? l) (RETURN nil))
            ((or (> (car (SETQ x (size (car l)))) (caaddr surf))
                    (> (cadr x) (cadr (caddr surf)))
                    (> (+ (caddr x) (caddr (cadr surf)) (caddr (caddr surf))) 321))
                (SETQ l (cdr l))
                (GO =>))
            ((RETURN (car x))))))

(defn- packord [x size l]
    (cond (nil? l) (list (list size x))
        (or (> (caaar l) (car size)) (and (= (car size) (caaar l)) (> (cadaar l) (cadr size))))
            (cons (car l) (packord x size (cdr l)))
        :else (cons (list size x) l)))

(defn- size [x]
    (cond (= x 'ßBOX) '(256 256 192) (= x 'ßTABLE) '(640 640 640) (term? x) (caddr (atab x)) :else x))

(§ defn- STARTHISTORY []
    (SETQ THTIME 0)
    (SETQ GRASPLIST nil)
    (DEFPROP EE COMMAND WHY)
    (DEFPROP EE 0 START)
    (DEFPROP EE 0 END)
    (DEFPROP EE !START TYPE)
    (SETQ EVENTLIST '(EE))
    (THADD '(!START EE ßDIALOG) nil)
    (ERRSET (CLEANOUT E) nil)
    (dorun (map #'(lambda [x]
        (and (get (car x) 'THASSERTION)
        (PUTPROP (car x)
            (list (list 0
                    (cadr x)
                    (cadar (THVAL '(THGOAL (!SUPPORT ($? x) ($? Y))) (list (list 'X 'THUNASSIGNED) (list 'Y (car x)))))))
            'HISTORY)))
    ATABLE)))

(defn- startime [l time]
    (< (caar l) (or (start? time) -1)))

(§ defn- SUPPORT [loc size x]
    (COND ((== (caddr loc) 0) 'ßTABLE)
        ((SETQ loc (OCCUPIER (+ (car loc) (half (car size))) (+ (cadr loc) (half (cadr size))) (dec (caddr loc))))
            (COND ((= loc x) nil) (loc)))))

(defn- tcent [x1 x2]
    (list (+ (car x1) (half (car x2))) (+ (cadr x1) (half (cadr x2))) (+ (caddr x1) (caddr x2))))

(§ defn- TFIND [x y]
    (let [z (get x 'HISTORY)]
        (or z (RETURN nil))
    =>  (COND ((not (> (caar z) (or (end? y) 32767)))
                (RETURN z))
            ((SETQ z (cdr z)) (GO =>)))))

(defn- timechk [ev time]
    (if (IMPERF? time)
        (not (or (< (get ev 'END) (or (start? time) -1)) (< (or (end? time) 262143) (get ev 'START))))
        (not (or (< (get ev 'START) (or (start? time) -1)) (< (or (end? time) 262143) (get ev 'END))))))

#_(ns shrdlu.data)

;; ####################################################################
;;
;;      DATA > INITIAL MICROPLANNER DATA BASE FOR THE BLOCKS WORLD
;;
;; ####################################################################

(§ THFLUSH THASSERTION)

(def- ATABLE
  '((ßB1 (72 64 0) (64 64 64))
    (ßB2 (72 64 64) (64 64 64))
    (ßB3 (256 0 0) (128 128 128))
    (ßB4 (416 416 1) (128 128 128))
    (ßB5 (320 64 128) (64 64 192))
    (ßB6 (0 192 0) (128 192 192))
    (ßB7 (0 160 192) (128 128 128))
    (ßB10 (192 416 0) (128 64 256))
    (ßBW1 (376 376 0) (8 256 192))
    (ßBW2 (376 376 0) (256 8 192))
    (ßBW3 (376 640 0) (256 8 192))
    (ßBW4 (640 376 0) (8 256 192))
    (ßBOX (384 384 0) (256 256 1))))

(def- DISPLAY-AS
  '((ßB1 !DISPLAY !BLOCK (72 64 0) (64 64 64) RED)
    (ßB2 !DISPLAY !PYRAMID (72 64 64) (64 64 64) GREEN)
    (ßB3 !DISPLAY !BLOCK (256 0 0) (128 128 128) GREEN)
    (ßB4 !DISPLAY !PYRAMID (416 416 1) (128 128 128) BLUE)
    (ßB5 !DISPLAY !PYRAMID (320 64 128) (64 64 192) RED)
    (ßB6 !DISPLAY !BLOCK (0 192 0) (128 192 192) RED)
    (ßB7 !DISPLAY !BLOCK (0 160 192) (128 128 128) GREEN)
    (ßB10 !DISPLAY !BLOCK (192 416 0) (128 64 256) BLUE)
    (ßHAND !DISPLAY !HAND (32 0 0) (0 0 0) WHITE)
    (ßTABLE !DISPLAY !TABLE (0 0 0) (512 512 0) BLACK)
    (ßBOX !DISPLAY !BOX (384 384 0) (254 254 192) WHITE)))

(§ THDATA)
    ((!IS ßB1 !BLOCK))
    ((!IS ßB2 !PYRAMID))
    ((!IS ßB3 !BLOCK))
    ((!IS ßB4 !PYRAMID))
    ((!IS ßB5 !PYRAMID))
    ((!IS ßB6 !BLOCK))
    ((!IS ßB7 !BLOCK))
    ((!IS ßB10 !BLOCK))

    ((!IS !RED !COLOR))
    ((!IS !BLUE !COLOR))
    ((!IS !GREEN !COLOR))
    ((!IS !WHITE !COLOR))
    ((!IS !BLACK !COLOR))

    ((!IS !RECTANGULAR !SHAPE))
    ((!IS !ROUND !SHAPE))
    ((!IS !POINTED !SHAPE))

    ((!IS ßSHRDLU !ROBOT))
    ((!IS ßFRIEND !PERSON))
    ((!IS ßHAND !HAND))

    ((!AT ßB1 (64 64 0)))
    ((!AT ßB2 (64 64 64)))
    ((!AT ßB3 (256 0 0)))
    ((!AT ßB4 (416 416 1)))
    ((!AT ßB5 (320 64 128)))
    ((!AT ßB6 (0 192 0)))
    ((!AT ßB7 (0 160 192)))
    ((!AT ßB10 (192 416 0)))

    ((!SUPPORT ßB1 ßB2))
    ((!SUPPORT ßB3 ßB5))
    ((!SUPPORT ßB6 ßB7))

    ((!CLEARTOP ßB2))
    ((!CLEARTOP ßB4))
    ((!CLEARTOP ßB5))
    ((!CLEARTOP ßB7))
    ((!CLEARTOP ßB10))

    ((!MANIP ßB1))
    ((!MANIP ßB2))
    ((!MANIP ßB3))
    ((!MANIP ßB4))
    ((!MANIP ßB5))
    ((!MANIP ßB6))
    ((!MANIP ßB7))
    ((!MANIP ßB10))

    ((!SUPPORT ßTABLE ßB1))
    ((!SUPPORT ßTABLE ßB3))
    ((!SUPPORT ßBOX ßB4))
    ((!SUPPORT ßTABLE ßB10))
    ((!SUPPORT ßTABLE ßB6))
    ((!SUPPORT ßTABLE ßBOX))

    ((!AT ßBOX (384 384 0)))
    ((!IS ßBOX !BOX))
    ((!IS ßTABLE !TABLE))
    ((!CONTAIN ßBOX ßB4))

    ((!SHAPE ßB1 !RECTANGULAR))
    ((!SHAPE ßB3 !RECTANGULAR))
    ((!SHAPE ßB2 !POINTED))
    ((!SHAPE ßB4 !POINTED))
    ((!SHAPE ßB5 !POINTED))
    ((!SHAPE ßB6 !RECTANGULAR))
    ((!SHAPE ßB7 !RECTANGULAR))
    ((!SHAPE ßB10 !RECTANGULAR))

    ((!COLOR ßB1 !RED))
    ((!COLOR ßB2 !GREEN))
    ((!COLOR ßB3 !GREEN))
    ((!COLOR ßB4 !BLUE))
    ((!COLOR ßB5 !RED))
    ((!COLOR ßB6 !RED))
    ((!COLOR ßB7 !GREEN))
    ((!COLOR ßB10 !BLUE))
    ((!COLOR ßBOX !WHITE))
    ((!COLOR ßTABLE !BLACK))

    ((!CALL ßSHRDLU SHRDLU))
    ((!CALL ßFRIEND YOU))
nil

(§ SETQ HANDAT '(32 0 0))

(§ SETQ THTIME 0)

(§ THFLUSH HISTORY)

(§ ERRSET (STARTHISTORY))

(§ SETQ PLAN nil)

(§ dorun (map #'(lambda [x y] (PUTPROP x (list y) 'COLOR))
    '(ßB1 ßB2 ßB3 ßB4 ßB5 ßB6 ßB7 ßB10)
    '(CB1 CB2 CB3 CB4 CB5 CB6 CB7 CB10)))

#_(ns shrdlu.plnr)

;; DO NOT GRIND THIS FILE WITH THE STANDARD GRIND

(defmacro $? [x] (list 'THV x))
(defmacro $E [x] (list 'THEV x))
(defmacro $_ [x] (list 'THNV x))
(defmacro $T [] '(THTBF THTRUE))
(defmacro $R [] 'THRESTRICT)
(defmacro $G [] 'THGOAL)
(defmacro $A [] 'THASSERT)
(defmacro $N [x] (list 'THANUM x))

(§ defmacro THPUSH [& a]
    ;; (THPUSH THTREE NEWINFO) CONSES NEWINFO ONTO THTREE
    (list 'SETQ (car a) (list 'cons (cadr a) (car a))))

(defn- evlis [l]
    ;; EVLIS EVALS ELEMENTS OF ARG, THEN RETURNS ARG.
    (dorun (map eval l))
    l)

(defn- thprintc [x] (TERPRI) (PRINC x) (PRINC \space))

(§ defn- THADD [THTT thpl]
    ;; THADD ADDS THEOREMS OR ASSERTION TO THE INPUT
    ;; THPL - PROPERTY LIST TO BE PLACED ON ASSERTION DATABASE INPUTS
    ;; THTT - NAME OF THM OR ACTUAL ASSERTION
    ;; RETURNS NIL IF ALREADY THERE ELSE RETURNS THTT
    (let [THNF nil THWH nil THCK nil THLAS nil THTTL nil THT1 nil THFST nil THFSTP nil THFOO nil]
        (SETQ THCK
            ;; IF THTT IS ATOMIC, WE ARE ASSERTING A THEOREM
            (COND ((term? THTT)
                    ;; IF NO THEOREM PROPERTY, THE GUY MADE A MISTAKE
                    (or (SETQ THT1 (get THTT 'THEOREM))
                        (do (PRINT THTT) (thert "CAN'T THASSERT, NO THEOREM - THADD")))
                    ;; THWH NOW SET TO KIND OF THEOREM, LIKE THERASING
                    (SETQ THWH (car THT1))
                    ;; MAKE AN EXTRA POINTER TO THTT
                    (SETQ THTTL THTT)
                    ;; IF WE HAVE A PL FOR OUR THEOREM, IT GOES ON THE ATOM WHICH IS THE NAME OF THE THEOREM
                    (and thpl (do
                    ;; GO THROUGH ITEMS ON PL ONE BY ONE
                    =>  (THPUTPROP THTT (cadr thpl) (car thpl))
                        (COND ((SETQ thpl (cddr thpl))
                            (GO =>)))
                        nil))
                    (caddr THT1))
                ;; SO WE HAVE AN ASSERTION TO ASSERT, MAKE THWH REFLECT THIS FACT
                (:else (SETQ THWH 'THASSERTION)
                    ;; PROPERTY LIST IS "CDR" OF ASSERTION
                    (SETQ THTTL (cons THTT thpl))
                    THTT)))
        (SETQ THNF 0) ;; THNF IS COUNTER SAYING WHICH ATOM WE ARE FILING UNDER
        (SETQ THLAS (count THCK)) ;; THLAS IS THE NUMBER OF TOP LEVEL ITEMS
        (SETQ THFST true)
        ;; THFST SAYS WE ARE TRYING TO PUT THE ITEM IN FOR THE FIRST TIME.
        ;; WE NEED TO KNOW THIS SINCE THE FIRST TIME THROUGH
        ;; WE MUST TEST THAT THE ASSERTEE IS NOT ALREADY THERE.
        ;; THCK IS INITIALLY THE ASSERTION OR THEOREM PATTERN.
        ;; THE FIRST TIME WE GO INTO THE DATABASE WE CHECK TO SEE IF THE ITEM IS THERE.
        ;; THAT MEANS DOING AN EQUAL TEST ON EVERY ITEM IN THE BUCKET.
        ;; AFTER THE FIRST TIME THIS IS NOT NECESSARY.
        ;; SINCE VARIABLES WILL IN GENERAL HAVE MANY MORE ITEMS IN THEIR BUCKET,
        ;; WE WILL WANT TO DO OUR CHECK ON A NON VARIABLE ITEM IN THE PATTERN.
    THP1 (COND ((nil? THCK)
                ;; THCK NIL MEANS THAT ALL THE ITEMS IN THE PATTERN ARE VARIABLES
                ;; SO WE TRY AGAIN ONLY THIS TIME DOING EQUAL CHECK ON
                ;; THE FIRST VARIABLE. THFOO NOW IS SIMPLY THE PATTERN
                (SETQ THCK THFOO)
                (SETQ THNF 0)
                (SETQ THFOO (SETQ THFST nil))
                ;; THFIRSTP SAYS WE AGAIN NEED TO CHECK FOR ASSERTEE
                ;; BEING IN DATA BASE, BUT NOW USE VARIABLES FOR EQ CHECK
                (SETQ THFSTP true)
                (GO THP1))
            ((nil? (SETQ THT1 (THIP (car THCK)))) (RETURN nil))
            ;; THIP IS THE WORKHORSE FOR THADD IF IT RETURNS NIL.
            ;; IT MEANS THE ASSERTEE IS ALREADY IN, SO FAIL.
            ((= THT1 'THOK))
            ;; THOK WHICH IS RETURN BY THIP SAYS THAT THE ASSERTEE IS NOT IN ALREADY.
            ;; OTHERWISE WE GO AROUND AGAIN, STILL LOOKING FOR A NON VARIABLE ITEM TO DO THE EQ CHECK.
            ((SETQ THFOO (concat THFOO (list (COND ((= THT1 'THVRB) (car THCK))))))
                (SETQ THCK (cdr THCK))
                (GO THP1)))
        (SETQ THFST nil)
        (dorun (map #'THIP (cdr THCK)))
        (SETQ THNF 0)
        (dorun (map #'THIP THFOO))
        THTTL))

(§ defq- THAMONG [tha]
    ;; EXAMPLE - (THAMONG ($? X) (THFIND ... ))
    ;; $E - (THAMONG ($E ($? X)) (THFIND ... )) CAUSES THE THVALUE OF ($? X) TO BE THE FIRST INPUT TO THAMONG.
    ;; THXX SET TO OLD BINDING CELL OF ($? X) (OR ($E ($? X))) IF ($? X) VALUES PUSHED ONTO THTREE AND THAMONG
    ;; FAILS TO THUNASSIGNED, OLD VALUE AND LIST OF NEW THAMONGF.
    (COND
        ((= (cadr (SETQ THXX (THGAL (COND ((= (caar tha) 'THEV) (THVAL (cadar tha) THALIST)) (:else (car tha))) THALIST))) 'THUNASSIGNED)
            (THPUSH THTREE (list 'THAMONG THXX (THVAL (cadr tha) THALIST)))
            nil)
        (:else (memq (cadr THXX) (THVAL (cadr tha) THALIST)))))       ;; IF ($? X) ASSIGNED, THAMONG REDUCES TO A MEMBERSHIP TEST

(§ defn- THAMONGF []                                                 ;; (CAR THTREE) = (THAMONG OLDBINDINGCELL (NEW VALUES))
    (COND (THMESSAGE (THPOPT) nil)
        ((caddar THTREE)                                            ;; LIST OF NEW VALUES NON NIL
            (RPLACA (cdadar THTREE) (caaddr (car THTREE)))          ;; REPLACE OLD VALUE WITH NEW VALUE
            (RPLACA (cddar THTREE) (cdaddr (car THTREE)))           ;; POP NEW VALUES
            (SETQ THBRANCH THTREE)                                  ;; STORE AWAY TREE FOR POSSIBLE BACKTRACKING
            (SETQ THABRANCH THALIST)                                ;; STORE AWAY THALIST FOR POSSIBLE BACKTRACKING
            (THPOPT)                                                ;; POP TREE
            true)                                                      ;; SUCCEED
        (:else (RPLACA (cdadar THTREE) 'THUNASSIGNED)                   ;; NO NEW VALUES LEFT. RETURN X TO THUNASSIGNED,
            (THPOPT)                                                ;; POP TREE AND CONTINUE FAILING.
            nil)))

(§ defq- THAND [a]
    (or (not a)
        (do (THPUSH THTREE (list 'THAND a nil)) (SETQ THEXP (car a)))))

(defn- thandf [] (THBRANCHUN) nil)

(§ defn- THANDT []
    (COND ((cdadar THTREE)
            (THBRANCH)
            (SETQ THEXP (cadr (cadar THTREE)))
            (RPLACA (cdar THTREE) (cdadar THTREE)))
        ((THPOPT)))
    THVALUE)

(§ defq- THANTE [thx]
    ;; DEFINES AND OPTIONALLY ASSERTS ANTECEDENT THEOREMS
    (THDEF 'THANTE thx))

(§ defq- THAPPLY [l]
    (THAPPLY1 (car l)
        ;; THAPPLY1 DOES THE REAL WORK, ALL WE DO IS GET THE THEOREM OFF THE PROPERTY LIST
        (get (car l) 'THEOREM)
        (cadr l)))

(§ defn- THAPPLY1 [thm thb dat]
    ;; MAKE SURE THE THEOREM PATTERN MATCHES THE GOAL
    (COND ((and (THBIND (cadr thb)) (THMATCH1 dat (caddr thb)))
            (and THTRACE (THTRACES 'THEOREM thm))
            ;; AS FAR AS THTREE GOES, ALL THEOREMS LOOK LIKE THPROG, AND
            ;; WHEN YOU COME DOWN TO IT, THEY ALL ACT LIKE THPROGS.
            (THPUSH THTREE (list 'THPROG (cddr thb) nil (cddr thb)))
            ;; CALL THE MAIN THPROG WORKHORSE
            (THPROGA)
            true)
        ;; IF THE THEOREM PATTERN DIDN'T MATCH, START FAILING.
        (:else (SETQ THALIST THOLIST) (THPOPT) nil)))

(§ defn- THASS1 [tha p]
    (let [THX nil THY1 nil THY nil TYPE nil PSEUDO nil]
        (and (cdr tha) (= (caadr tha) 'THPSEUDO) (SETQ PSEUDO true))
        ;; IF YOU SEE "THPSEUDO" SET FLAG "PSEUDO" TO T
        (or (term? (SETQ THX (car tha)))
            ;; IF (CAR THA) IS AN ATOM WE ARE ASSERTING (ERRASING) A THEOREM
            (THPURE (SETQ THX (THVARSUBST THX nil)))
            ;; THVARSUBST SUBSTITUTES THE ASSIGNMENTS FOR ALL ASSIGNED VARIABLES
            ;; THPURE CHECKS THAT ALL VARIABLES ARE ASSIGNED
            PSEUDO
            ;; IF WE ARE NOT REALLY ASSERTING, THE VARIABLES DO NOT ALL HAVE TO BE ASSIGNED
            (do (PRINT THX)
                (thert "IMPURE ASSERTION OR ERASURE - THASS1")))
        (and THTRACE (not PSEUDO) (THTRACES (COND (p 'THASSERT) ('THERASE)) THX))
        (SETQ tha (COND (PSEUDO (cddr tha)) ((cdr tha))))
        ;; THX IS NOW WHAT WE ARE ASSERTING, AND THA IS THE RECOMMENDATION LIST
        (or
            ;; WE ARE NOW GOING TO PHYSICALLY ADD OR REMOVE ITEM
            (SETQ THX
                (COND (PSEUDO (list THX))
                    ;; IF THPSEUDO, DON'T ALTER THE DATA BASE
                    ;; IF P IS "T" WE ARE ASSERTING SO USE THADD
                    (p (THADD THX
                        ;; THADD TAKES TWO ARGS THE FIRST IS ITEM TO BE ADDED
                        ;; THE SECOND IS THE PROPERTY LIST FOR THE ITEM
                        (SETQ THY
                            ;; THPROP SAYS "MY" CADR IS TO BE EVALED TO GET THE PROPERTY LIST
                            (COND ((and tha (= (caar tha) 'THPROP))
                                (PROG1 (eval (cadar tha))
                                    ;; AND REMOVE THPROP FROM THE RECOMENDATION LIST
                                    (SETQ tha (cdr tha))))))))
                    ;; OTHERWISE WE ARE ERASING, SO USE THREMOVE
                    (:else (THREMOVE THX))))
            ;; THE LAST ITEM WILL BE NIL ONLY IF THADD OR THREMOVE FAILED.
            ;; THAT IS, IF THE ITEM TO BE ADDED WAS ALREADY THERE, OR THE ONE TO BE REMOVED, WASN'T.
            (RETURN nil))
        ;; TYPE IS THE KIND OF THEOREM WE WILL BE LOOKING FOR
        (COND (p (SETQ TYPE 'THANTE))
            ((SETQ TYPE 'THERASING)))
        ;; IF WE ACTUALLY MUNGED THE DATABASE, PUT THE FACT IN THTREE
        (or PSEUDO
            (THPUSH THTREE (list (COND (p 'THASSERT) ('THERASE)) THX THY)))
        ;; MAPCAN IS A MAC-LISP FUNCTION, LIKE MAPCAR BUT USES NCONC.
        ;; THTAE LOOKS AT THE RECOMENDATION LIST AND PRODUCES A LIST OF INSTRUCTIONS ABOUT WHAT THEOREMS TO TRY.
        (SETQ THY (doall (map #'THTAE tha)))
        ;; THEXP IS A HACK TELLING THVAL TO THVAL THIS ITEM BEFORE IT GOES ON TO THE NEXT LINE OF PLANNER CODE.
        ;; THEXP IS NOW (THDO <APPROPRIATE ANTECEDENT OR ERASING THEOREMS>).
        (COND (THY (SETQ THEXP (cons 'THDO THY))))
        THX))

(§ defq- THASSERT [tha]
    ;; THASS1 IS USED FOR BOTH ASSERTING AND ERASING, THE "T" AS SECOND ARG TELLS IT THAT WE ARE ASSERTING.
    (THASS1 tha true))

(§ defn- THASSERTF []
    (THREMOVE (COND ((term? (cadar THTREE)) (cadar THTREE)) (:else (caadar THTREE))))
    (THPOPT)
    nil)

(§ defn- THASSERTT [] (PROG1 (cadar THTREE) (THPOPT)))

(§ defq- THASVAL [x]
    ((lambda [x] (and x (not (= (cadr x) 'THUNASSIGNED))))
        (THGAL (car x) THALIST)))

(§ defn- THBA [th1 th2]
    ;; JUST LIKE ASSQ IN LISP, ONLY RETURN WITH THE POINTER 1 ELEMENT PRIOR TO THE ONE ASKED FOR.
    ;; USED ONLY BY THAD AND THREMOVE.
    (let [THP th2]
    =>  (and (= (COND (THPC (cadr THP)) (:else (caadr THP))) th1)
            (RETURN THP))
        (or (cdr (SETQ THP (cdr THP))) (RETURN nil))
        (GO =>)))

(§ defn- THBAP [th1 th2]
    ;; LIKE THBA, ONLY USED EQUAL RATHER THAN EQ
    (let [THP th2]
    =>  (and (= (COND (THPC (cadr THP)) (:else (caadr THP))) th1)
            (RETURN THP))
        (or (cdr (SETQ THP (cdr THP))) (RETURN nil))
        (GO =>)))

(§ defn- THBIND [a]
    ;; WHEN WE ENTER A NEW THEOREM OR THPROG, WE MUST BIND THE NEW VARIABLES.  A IS THE VARIABLE LIST
    (SETQ THOLIST THALIST) ;; THOLIST IS THE OLD THALIST
    ;; IF A IS NIL THERE IS NOTHING TO DO
    (or (nil? a)
        (do
        ;; WHEN A IS NIL, WE ARE DONE AND JUST PUT A MARKER ON THTREE WITH A POINTER TO THE OLD THALIST,
        ;; SO IT CAN BE RESTORED.
        =>  (COND ((nil? a)
                (THPUSH THTREE (list 'THREMBIND THOLIST))
                (RETURN true)))
            ;; OTHERWISE ADD TO THE ALIST THE NEW BINDING CELL
            (THPUSH THALIST
                ;; THE FIRST ELEMENT IS THE NAME OF THE VARIABLE IF THE ENTRY IS AN ATOM,
                ;; THEN WE ARE JUST GIVEN THE VARIABLE AND ITS INITIAL ASSIGNMENT IS "THUNASSIGNED",
                ;; I.E. NO INITIAL ASSIGNMENT.
                (COND ((term? (car a)) (list (car a) 'THUNASSIGNED))
                    ;; OTHERWISE OUR ENTRY IS A LIST
                    ;; IF THE FIRST ELEMENT OF THE LIST IS ($R) OR THRESTRICT
                    ;; WE ADD THE RESTRICTION TO THE BINDING CELL
                    ;; THE CDDR OF THE CELL GIVES THE RESTRICTION LIST
                    ((= (caar a) 'THRESTRICT) (concat (thbi1 (cadar a)) (cddar a)))
                    ;; OTHERWISE WE ARE GIVEN BOTH THE VARIABLE AND ITS
                    ;; INITIAL ASSIGNMENT, SO MAKE THE SECOND ELEMENT OF THE
                    ;; BINDING CELL A POINTER TO THE INITIAL ASSIGNMENT
                    (:else (list (caar a) (eval (cadar a))))))
            (SETQ a (cdr a))
            ;; REPEAT FOR THE NEXT VARIABLE IN THE LIST
            (GO =>))))

(defn- thbi1 [x]
    (if (term? x) (list x 'THUNASSIGNED) (list (car x) (eval (cadr x)))))

(§ defq- THBKPT [l]
    (or (and THTRACE (THTRACES 'THBKPT l)) THVALUE))

(§ defn- THBRANCH []
    ;; THBRANCH IS CALLED BY THPROGT AND WE ARE SUCCEEDING BACKWARDS.
    ;; CAR THTREE IS THE THPROG MARKING.
    (COND ;; THERE ARE NO MORE EXPRESSIONS TO EXECUTE IN THE THPROG.
        ((not (cdadar THTREE)))
        ((= THBRANCH THTREE) (SETQ THBRANCH nil))
        ;; NORMAL CASE
        ;; CADDAR THTREE IS THE SECOND OF THE THREE ARGS ON THE THPROG MARK
        ;; THBRANCH AND THABRANCH ARE POINTERS TO THE THTREE AND THALIST
        ;; RESPECTIVELY AT THE POINT WHERE WE HAD JUST SUCCEEDED.
        ;; IN GENERAL, BY THE TIME WE GET BACK TO THE THPROG MARK ON THTREE
        ;; WE HAVE REMOVED THE THINGS PUT ON THTREE BY THE SUCCESSFUL
        ;; LAST LINE OF THE THPROG
        ;; WE WILL NOW STORE THIS INFORMATION ON THE THPROG MARK SO
        ;; THAT IF WE FAIL WE WILL HAVE RECORDS OF WHAT HAPPEND
        ;; IT IS STORED BY HACKING THE SECOND ARG TO THE THPROG MARK
        ((RPLACA (cddar THTREE) (cons (list THBRANCH THABRANCH (cadar THTREE)) (caddar THTREE)))
            ;; WE NOW SETQ THBRANCH TO NIL.  IF THE NEXT LINE ALSO SUCCEEDS,
            ;; THVAL WILL LOOK FOR A NIL THBRRANCH TO INDICATE THAT IT SHOULD
            ;; SETQ IT AGAIN TO THE POINT OF SUCCESS
            (SETQ THBRANCH nil))))

(§ defn- THBRANCHUN []
    ;; WE ARE NOW FAILING.  THBRANCHUN IS CALLED BY THPROGF.
    (let [x nil] (RETURN
        (COND ;; IF THE SECOND ARG TO THE PROG MARK IS NON-NIL, IT MEANS THAT THERE ARE PREVIOUS LINES IN THE THPROG TO FAIL BACK TO
            ((SETQ x (caddar THTREE))
                ;; A COMPAIRISON OF THIS WITH WHAT HAPPEND IN THBRANCK WILL REVEAL THAT
                ;; ALL WE ARE DOING HERE IS RESTORING THE PROG MARK TO IS STATE BEFORE THE LAST SUCCESS.
                (RPLACA (cdar THTREE) (caddar x))
                (RPLACA (cddar THTREE) (cdr x))
                ;; RESET THALIST AND THTREE
                (SETQ THALIST (cadar x))
                (SETQ THTREE (caar x))
                true)
            ;; THERE AREN'T ANY MORE THINGS IN THE THPROG TO TRY,
            ;; SO JUST RETURN NIL.
            (:else (THPOPT) nil)))))

(§ defq- THCOND [tha]
    (THPUSH THTREE (list 'THCOND tha nil))
    (SETQ THEXP (caar tha)))

(defn- thcondf [] (THOR2 nil))

(§ defn- THCONDT []
    (RPLACA (car THTREE) 'THAND)
    (RPLACA (cdar THTREE) (caadar THTREE))
    THVALUE)

;; THCONSE DEFINES AND OPTIONALLY ASSERTS CONSEQUENT THEOREMS

(§ defq- THCONSE [thx] (THDEF 'THCONSE thx))

(§ defn- THDATA []
    (let [x nil]
    =>  (TERPRI)
        (COND ((nil? (SETQ x (READ nil))) (RETURN true))
            ((PRINT (THADD (car x) (cdr x)))))
        (GO =>)))

;; THDEF DEFINES AND OPTIONALLY ASSERTS THEOREMS

(§ defn- THDEF [thmtype thx]
    (let [THNOASSERT? nil THMNAME nil THMBODY nil]
        (COND ((not (term? (car thx)))
                (SETQ THMBODY thx)
                (COND
                    ((= thmtype 'THCONSE) (SETQ THMNAME (THGENAME TC-G)))
                    ((= thmtype 'THANTE) (SETQ THMNAME (THGENAME TA-G)))
                    ((= thmtype 'THERASING) (SETQ THMNAME (THGENAME TE-G)))))
            ((SETQ THMNAME (car thx)) (SETQ THMBODY (cdr thx))))    ;; THNOASSERT FEATURE
        (COND ((= (car THMBODY) 'THNOASSERT)
            (SETQ THNOASSERT? true)
            (SETQ THMBODY (cdr THMBODY))))
        (THPUTPROP THMNAME (cons thmtype THMBODY) 'THEOREM)
        (COND
            (THNOASSERT?  (PRINT (list THMNAME 'DEFINED 'BUT 'NOT 'ASSERTED)))
            ((THASS1 (list THMNAME) true) (PRINT (list THMNAME 'DEFINED 'AND 'ASSERTED)))
            (:else (PRINT (list THMNAME 'REDEFINED))))
        true))

(§ defq- THDO [a]
    (or (not a)
        (do (THPUSH THTREE (list 'THDO a nil nil)) (SETQ THEXP (car a)))))

(§ defn- THDO1 []
    (RPLACA (cdar THTREE) (cdadar THTREE))
    (SETQ THEXP (caadar THTREE))
    (COND (THBRANCH
        (RPLACA (cddar THTREE) (cons THBRANCH (caddar THTREE)))
        (SETQ THBRANCH nil)
        (RPLACA (cdddar THTREE) (cons THABRANCH (car (cdddar THTREE)))))))

(§ defn- THDOB []
    (COND ((or THMESSAGE (nil? (cdadar THTREE)))
            (RPLACA (car THTREE) 'THUNDO)
            true)
        ((THDO1))))

(§ defq- THERASE [tha] (THASS1 tha nil))

(§ defn- THERASEF []
    (THADD (COND ((term? (cadar THTREE)) (cadar THTREE)) (:else (caadar THTREE)))
        (COND ((term? (cadar THTREE)) nil) (:else (cdadar THTREE))))
    (THPOPT)
    nil)

(§ defn- THERASET [] (PROG1 (cadar THTREE) (THPOPT)))

;; THERASING DEFINES AND OPTIONALLY ASSERTS ERASING THEOREMS

(§ defq- THERASING [thx] (THDEF 'THERASING thx))

(§ defq- THFAIL [tha]
    (and tha
        (let [THTREE1 nil THA1 nil THX nil]
        F   (SETQ THA1 (COND
                ((= (car tha) 'THEOREM) 'THPROG)
                ((= (car tha) 'THTAG) 'THPROG)
                ((= (car tha) 'THMESSAGE) (SETQ THMESSAGE (cadr tha)) (RETURN nil))
                (:else (car tha))))
            (SETQ THTREE1 THTREE)
        LP1 (COND ((nil? THTREE1)
                    (PRINT tha)
                    (COND ((term? (SETQ tha (thert "NOT FOUND - THFAIL"))) (RETURN tha))
                        (:else (GO F))))
                ((= (caar THTREE1) THA1) (GO ELP1)))
        ALP1 (SETQ THTREE1 (cdr THTREE1))
            (GO LP1)
        ELP1 (COND ((= (car tha) 'THTAG)
                (COND ((memq (cadr tha) (cadddr (car THTREE1))) (GO TAGS))
                    (:else (GO ALP1)))))
            (SETQ THMESSAGE (list (cdr THTREE1) (and (cdr tha) (cadr tha))))
            (RETURN nil)
        TAGS (SETQ THX (caddar THTREE1))
        LP2  (COND ((nil? THX) (GO ALP1))
                ((= (caaddr (car THX)) (cadr tha))
                    (SETQ THMESSAGE (list (caar THX) (and (cddr tha) (caddr tha))))
                    (RETURN nil)))
            (SETQ THX (cdr THX))
            (GO LP2))))

(§ defn- THFAIL? [prd act]
    (THPUSH THTREE (list 'THFAIL? prd act))
    THVALUE)

(§ defn- THFAIL?F []
    (COND ((eval (cadar THTREE))
            (eval (PROG2 (SETQ THMESSAGE nil) (caddar THTREE) (THPOPT))))
        (:else (THPOPT) nil)))

(defn- thfail?t [] (THPOPT) THVALUE)

(§ defq- THFINALIZE [tha]
    (let [THTREE1 nil THT nil THX nil]
        (COND ((nil? tha) (SETQ tha (thert "BAD CALL - THFINALIZE"))))
        (COND ((term? tha) (RETURN tha))
            ((= (car tha) 'THTAG) (SETQ THT (cadr tha)))
            ((= (car tha) 'THEOREM) (SETQ tha (list 'THPROG))))
        (SETQ THTREE (SETQ THTREE1 (cons nil THTREE)))
    PLUP (SETQ THX (cadr THTREE1))
        (COND ((nil? (cdr THTREE1)) (PRINT tha) (thert "OVERPOP - THFINALIZE"))
            ((and THT (= (car THX) 'THPROG) (memq THT (cadddr THX)))
                (GO RTLEV))
            ((or (= (car THX) 'THPROG) (= (car THX) 'THAND))
                (RPLACA (cddr THX) nil)
                (SETQ THTREE1 (cdr THTREE1)))
            ((= (car THX) 'THREMBIND)
                (SETQ THTREE1 (cdr THTREE1)))
            ((RPLACD THTREE1 (cddr THTREE1))))
        (COND ((= (car THX) (car tha)) (GO DONE)))
        (GO PLUP)
    RTLEV (SETQ THX (cddr THX))
    LEVLP (COND ((nil? (car THX)) (SETQ THTREE1 (cdr THTREE1)) (GO PLUP))
            ((= (caaddr (caar THX)) THT) (GO DONE)))
        (RPLACA THX (cdar THX))
        (GO LEVLP)
    DONE (SETQ THTREE (cdr THTREE))
        true))

(§ defq- THFIND [tha]
    (THBIND (caddr tha))
    (THPUSH THTREE
        (list 'THFIND
            (COND ((= (car tha) 'ALL) ' (1 nil nil))               ;; STANDARD ALL
                ((number? (car tha))
                    (list (car tha) (car tha) true))                       ;; SINGLE NUMBER
                ((number? (caar tha)) (car tha))                    ;; WINOGRAD CROCK FORMAT
                ((= (caar tha) 'EXACTLY)
                    (list (cadar tha) (inc (cadar tha)) nil))
                ((= (caar tha) 'AT-MOST)
                    (list 1 (inc (cadar tha)) nil))
                ((= (caar tha) 'AS-MANY-AS)
                    (list 1 (cadar tha) true))
                (:else (cons (cadar tha)                                ;; ONLY THING LEFT IS AT-LEAST
                    (COND ((nil? (cddar tha)) (list nil true))         ;; NO AT-MOST
                        ((= (caddar tha) 'AT-MOST)
                            (list (inc (car (cdddar tha)))
                            nil))
                        (:else (list (car (cdddar tha)) true))))))
            (cons 0 nil)
            (cadr tha)))
    (THPUSH THTREE (list 'THPROG (cddr tha) nil (cddr tha)))
    (THPROGA))

(§ defn- THFINDF []
    (SETQ THBRANCH nil)
    (COND ((or THMESSAGE (< (caadr (SETQ THXX (cdar THTREE))) (caar THXX)))
            (THPOPT)
            nil)
        (:else (THPOPT) (cdadr THXX))))

(§ defn- THFINDT []
    (let [THCDAR (cdar THTREE) THX nil THY nil THZ (caddr THCDAR)]
        (and (memq (SETQ THX (THVARSUBST THZ nil)) (cdadr THCDAR))
            (GO =>))
        (RPLACD (cadr THCDAR) (cons THX (cdadr THCDAR)))
        (and (= (SETQ THY (inc (caadr THCDAR))) (cadar THCDAR))
            (RETURN (PROG2 (SETQ THBRANCH nil) (and (caddar THCDAR) (cdadr THCDAR)) (THPOPT))))
        (RPLACA (cadr THCDAR) THY)
    =>  (SETQ THTREE THBRANCH)
        (SETQ THALIST THABRANCH)
        (SETQ THBRANCH nil)
        nil))

(§ defq- THFLUSH [a]
    ;; (THFLUSH) FLUSHES ALL ASSERTIONS AND THEOREMS
    ;; INPUT = SEQUENCE OF INDICATORS DEFAULT =
    ;; EFFECT = FLUSHES THE PROPERTIES OF THESE
    ;; (THASSERTION THCONSE THANTE THERASING)
    ;; INDICATORS FROM ALL ATOMS
    (dorun (map #'(lambda [B]
        (dorun (map #'(lambda [C]
            (dorun (map #'(lambda [D]
                (REMPROP D B))
            C)))
        (MAKOBLIST nil))))
    (COND (a) (' (THASSERTION THCONSE THANTE THERASING))))))

(§ defn- THGAL [x y]
    ;; (THGAL ($? X) THALIST) RETURNS THE BINDING CELL (X -) OF X ON THALIST
    (SETQ THXX x)
    (sassq (cadr x) y #'(lambda [] (PRINT THXX) (thert "THUNBOUND - THGAL"))))

(§ defq- THGENAME [x]
    ;; GENERATES UNIQUE NAME WITH ARG AS PREFIX
    (READLIST (concat (EXPLODE (car x)) (EXPLODE (SETQ THGENAME (inc THGENAME))))))

(§ defq- THGO [x]
    (APPLY #'THSUCCEED (cons 'THTAG x)))

(§ defq- THGOAL [tha]
    ;; THA = (PATTERN RECOMMENDATION)
    ;; PATTERN IS EITHER EXPLICIT, THE VALUE OF A PLANNER VARIABLE OR THVAL OF $E...
    ;; THA2 = INSTANTIATED PATTERN
    ;; THA1 = RECOMMENDATIONS
    (let [THY nil THY1 nil THZ nil THZ1 nil THA1 nil THA2 nil]
        (SETQ THA2 (THVARSUBST (car tha) true))
        (SETQ THA1 (cdr tha))
        (COND ((or (nil? THA1)                                      ;; SHOULD DATA BASE BE SEARCHED?  TRIED IF NO RECS
                (and (not (and (= (caar THA1) 'THANUM)
                        (SETQ THA1 (cons (list 'THNUM (cadar THA1)) (cons (list 'THDBF 'THTRUE) (cdr THA1))))))
                    (not (and (= (caar THA1) 'THNODB)              ;; TRIED IF REC NOT THNODB OR (THDBF PRED)
                        (do (SETQ THA1 (cdr THA1)) true)))
                    (not (= (caar THA1) 'THDBF))))
            (SETQ THA1 (cons (list 'THDBF 'THTRUE) THA1))))
        (SETQ THA1 (doall (map #'THTRY THA1)))                      ;; THMS AND ASSERTIONS SATISFYING RECS APPENDED TO RECS
        (and THTRACE (THTRACES 'THGOAL THA2))
        (COND ((nil? THA1) (RETURN nil)))
        (THPUSH THTREE (list 'THGOAL THA2 THA1))                    ;; (THGOAL PATTERN MATCHES)
        (RPLACD (cddar THTREE) 262143)
        nil))                                                       ;; FAILS TO THGOALF

(§ defn- THGOALF []
    ;; BASICALLY ALL IT DOES IS TO SEND OFF TO THTRY1 TO TRY ANOTHER POSSIBILITY.
    ;; IF THTRY1 RETURNS NIL, IT MEANS THAT IT COULDN'T FIND ANOTHER POSSIBILITY
    ;; AND WE SHOULD TELL THVAL THAT WE HAVE FAILED.
    ;; ALL THPOPT DOES IS TO LOB THE THGOAL ENTRY OFF THTREE.
    (COND (THMESSAGE (THPOPT) nil) ((THTRY1)) (:else (THPOPT) nil)))

(§ defn- THGOALT []
    (PROG1
        (COND ((= THVALUE 'THNOVAL) (THVARSUBST (cadar THTREE) nil))
            (THVALUE))
        (THPOPT)))

(§ defn- THIP [thi]
    ;; THI IS AN ITEM FROM THE ASSERTION OR PATTERN OF THE THEOREM BEING ENTERED
    (let [THT1 nil THT3 nil THSV nil THT2 nil THI1 nil]
        (SETQ THNF (inc THNF))
        ;; THNF IS A FREE VARIABLE FROM THADD (WHO CALLS THIS BUGER)
        ;; IT SAYS WE ARE LOOKING AT THE N'TH PLACE IN THE PATTERN
        (COND ((and (term? thi) (not (= thi '?)) (not (number? thi)))
                ;; THI1 IS THE NAME OF THE ATOM TO LOOK UNDER WHEN THI IS A USUAL ATOM
                ;; THI1 = THI NUMBERS DON'T HAVE PROPERTY LISTS SO THEY DON'T COUNT AS
                ;; NORMAL ATOMS, NOR DOES "?" SINCE IT IS A SORT OF VARIABLE IN PLANNER
                (SETQ THI1 thi))
            ((or (= thi '?) (memq (car thi) '(THV THNV)))
                ;; SEE IF THI IS A VARIABLE
                (COND (THFST (RETURN 'THVRB))
                    ;; IF WE ARE DOING THIS FOR THE FIRST TIME, DON'T CONSIDER VARIABLES
                    ;; FOR EXPLANATION WHY, SEE THADD
                    ((SETQ THI1 'THVRB))))
            ((RETURN 'THVRB)))
        ;; OTHERWISE THI IS SOMETHING WITH NO PROPERTY LIST LIKE A NUMBER, OR LIST
        ;; RETURNING THVRB TO THADD TELLS IT THAT EVERYTHING IS OK SO
        ;; FAR, BUT NOTHING WAS DONE ON THIS ITEM
        (COND ((not (SETQ THT1 (get THI1 THWH)))
                ;; THWH IS THE NAME OF THE PROPERTY TO LOOK UNDER ON THE ATOM
                ;; IF THIS PROPERTY IS NOT THERE THEN WE MUST PUT IT THERE
                ;; IN PARTICULAR, NO PROPERTY MEANS THAT THE
                ;; ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
                (PUTPROP THI1 (list nil (list THNF (list THLAS 1 THTTL))) THWH))
            ((= THT1 'THNOHASH) (RETURN 'THBQF))
            ;; IF THE PROPERTY IS "THNOHASH" IT MEANS THAT WE
            ;; SHOULD NOT BOTHER TO INDEX UNDER THIS ATOM, SO
            ;; JUST RETURN TO THADD
            ((not (SETQ THT2 (assq THNF (cdr THT1))))
                ;; LOOK ON THE PROPERTY LIST ENTRY TO SEE
                ;; IF THERE IS A SUB-ENTRY FOR PATTERNS WITH THIS ATOM IN THE THNF'TH POSITION.
                ;; IF NOT, HACK THE ENTRY SO THERE IS.
                ;; AGAIN THIS IMPLIES THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE.
                (concat THT1 (list (list THNF (list THLAS 1 THTTL)))))
            ((not (SETQ THT3 (assq THLAS (cdr THT2))))
                ;; NOW LOOK WITHIN THE SUB-ENTRY FOR A SUB-SUB-ENTRY.
                ;; I.E. THOSE PATTERNS WHICH ARE ALSO OF THE CORRECT TOTAL LENGTH
                ;; THLAS IS A VARIABLE FROM THADD WHICH GIVES THE LENGTH OF THE ASSERTEE
                ;; AGAIN, IF NOT THERE, HACK IT IN
                (concat THT2 (list (list THLAS 1 THTTL))))
            ((and (or THFST THFSTP)
                    ;; THIS BRANCH SAYS THAT WE STILL NEED TO CHECK THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
                    ;; THIS MEANS THAT WE MUST LOOK DOWN THE REMAINING SUB-SUB-BUCKET LOOKING FOR THE ASSERTEE
                    (COND ((= THWH 'THASSERTION) (assq THTT (cddr THT3)))
                        ;; RANDOMNESS DUE TO THE FACT THAT ASSERTIONS HAVE PROPERY LIST ON THEM,
                        ;; WHILE THEOREM NAMES ARE ATOMS WHOES PROPERTY LISTS ARE OF THE
                        ;; USUAL "INVISIBLE" VARIETY
                        (:else (memq THTT (cddr THT3)))))
                ;; IF THE ASSERTEE IS FOUND, RETURN NIL INDICATING FAILURE
                (RETURN nil))
            ((SETQ THSV (cddr THT3))
            ;; HACK IN THE LATEST ENTRY INTO THE SUB-SUB-BUCKET
            (RPLACA (cdr THT3) (inc (cadr THT3)))
            (RPLACD (cdr THT3) (concat (list THTTL) THSV))))
        ;; IF WE GET TO THIS POINT, EVERYTHING IS OK, SO TELL THADD SO
        'THOK))

(§ defn- THMATCH2 [thx thy]
    ;; THX IS ONE ITEM FROM THE PATTERN.
    ;; THY IS THE CORESPONDING ITEM FROM THE CANDIDATE.
    ;; THMATCH2 DECIDES IF THE TWO ITEMS REALLY MATCH.

    ;; THOLIST IS THE "THALIST" WHICH WAS IN EXISTANCE BEFORE
    ;; WE STARTED WORKING ON THE CURRENT LINE OF PLANNER CODE
    ;; STANDARD CHECK FOR $E
    (and (= (car thx) 'THEV)
        (SETQ thx (THVAL (cadr thx) THOLIST)))
    (and (= (car thy) 'THEV)
        (SETQ thy (THVAL (cadr thy) THALIST)))
    (COND
        ;; IF EITHER IS A ? ANYTHING WILL MATCH, SO OK
        ((= thx '?))
        ((= thy '?))
        ;; IF EITHER IS A VARIABLE THINGS GET MESSY.
        ;; EVERYTHING DOWN TO ***** IS CONCERNED WITH THIS CASE
        ((or (memq (car thx) '(THV THNV THRESTRICT)) (memq (car thy) '(THV THNV THRESTRICT)))
        ((lambda [XPAIR YPAIR]
            ;; X AND Y PAIR ARE THE RESPECTIVE BINDING CELLS WHICH WILL HAVE ANY NEW RESTRICTIONS MENTIONED.
            ;; IF THX OR THY IS NOT A VARIABLE (I.E. THE OTHER IS) THEN X OR Y PAIR WILL BE NIL.
            (COND ((and XPAIR
                ;; THX IS A VARIABLE
                ;; THIS SEES IF THX IS UNASSIGNED
                (or (= (car thx) 'THNV)
                    (and (= (car thx) 'THV) (= (cadr XPAIR) 'THUNASSIGNED)))
                ;; THCHECK MACKES SURE THE RESTRICTIONS (IF ANY) ON
                ;; THX ARE COMPATIBLE WITH THY
                (THCHECK (cddr XPAIR)
                    (COND (YPAIR (cadr YPAIR)) (:else thy))))
                ;; FURTHERMORE, THY IS ALSO A VARIABLE
                ;; THIS MEANS WE MUST DO THE MYSTERIOUS VARIABLE LINKING
                (COND (YPAIR (THRPLACAS (cdr XPAIR) (cadr YPAIR))
                        ;; IF THY ALSO HAS RESTRICTIONS WHEN WE LINK VARIABLES, WE COMBINE RESTRICTIONS
                        (and (cddr YPAIR) (THRPLACDS (cdr XPAIR) (THUNION (cddr XPAIR) (cddr YPAIR))))
                    (THRPLACDS YPAIR (cdr XPAIR)))
                ;; IF THY IS NOT A VARIALBE, JUST ASSIGN THX TO THY
                ;; THRPLACAS WILL HACK THML THE FREE VARIABLE FROM THMATCH1
                (:else (THRPLACAS (cdr XPAIR) thy))))
            ;; IN THIS COND PAIR THY IS A VARIABLE AND THX IS EITHER
            ;; A CONSTANT OR A PREVIOUSLY ASSIGNED VARIALBE
            ((and YPAIR
                (or (= (car thy) 'THNV)
                    ;; FURTHERMORE THY IS UNASSIGNED
                    (and (= (car thy) 'THV) (= (cadr YPAIR) 'THUNASSIGNED)))
                ;; MAKE SURE RESTRICTIONS ARE OK
                (THCHECK (cddr YPAIR) (COND (XPAIR (cadr XPAIR)) (:else thx))))
                ;; IF THX IS A VARIABLE, LINK
                (COND (XPAIR (THRPLACAS (cdr YPAIR) (cadr XPAIR)))
                    ;; OTHERWISE JUST ASSIGN THY TO THX
                    (:else (THRPLACAS (cdr YPAIR) thx))))
            ;; THX IS AN ASSIGED VARIABLE, SO JUST MAKE
            ;; SURE ITS ASSIGNEMENT IS EQUAL TO THY
            ((and XPAIR (= (cadr XPAIR) (COND (YPAIR (cadr YPAIR)) (:else thy)))))
            ;; THX IS A CONSTANT, THY IS A VARIABLE, AND THEY ARE EQUAL
            ((and YPAIR (= (cadr YPAIR) thx)))
            ;; LOOSE, SO RETURN WITH AN ERROR
            (:else (ERR nil))))

            ;; THE FOLLOWING TWO CONDS BIND XPAIR AND YPAIR RESPECTIVELY
            (COND
                ;; IF THX IS A NORMAL VARIALBE, IN PARTICULAR
                ;; WE ARE NOT INTRODUCING NEW RESTRICTIONS AT THIS TIME,
                ;; THEN X PAIR IS JUST THE BINDING LIST
                ((THVAR thx) (THGAL thx THOLIST))
                ;; WE MUST HACK A NEW RESTRICTION ONTO THE BINDING LIST
                ((= (car thx) 'THRESTRICT)
                ;; WE ARE "RESTRICTING" A ?.  SINCE ? HAS NO
                ;; BINDING LIST, WE MAKE UP A PSEUDO BINDING LIST
                    (COND ((= (cadr thx) '?)
                            (PROG1 (cons '? (cons 'THUNASSIGNED (concat (cddr thx) nil)))
                                (SETQ thx '(THNV ?))))
                        ;; WE ARE RESTRICTING A VARIABLE.  THIS MEANS THAT WE MUST PUT IN ON THE BINDING LIST.
                        (:else ((lambda [U]
                                ;; THUNION MAKES SURE WE DON'T PUT THE SAME RESTRICTION ON TWICE.
                                (THRPLACDS (cdr U) (THUNION (cddr U) (cddr thx)))
                                (SETQ thx (cadr thx))
                                U)
                            (THGAL (cadr thx) THOLIST))))))
            ;; NOTE THAT IF THX IS NOT A VARIABLE THEN XPAIR IS ()
            ;; WE DO THE EXACT SAME THING FOR THY AS WE JUST DID FOR THX
            (COND ((THVAR thy) (THGAL thy THALIST))
                ((= (car thy) 'THRESTRICT)
                    (COND ((= (cadr thy) '?)
                        (PROG1 (cons '? (cons 'THUNASSIGNED (concat (cddr thy) nil)))
                            (SETQ thy '(THNV ?))))
                        (:else ((lambda [U]
                                (THRPLACDS (cdr U) (THUNION (cddr U) (cddr thy)))
                                (SETQ thy (cadr thy))
                                U)
                            (THGAL (cadr thy) THALIST))))))))
        ;; **************
        ;; IF THE TWO ARE EQUAL, NATURALLY THEY MATCH
        ((= thx thy))
        ;; IF NOT, THEY DON'T, SO REPORT FAILURE
        (:else (ERR nil))))

(§ defn- THCHECK [thprd thx]
    (or (nil? thprd)
        (= thx 'THUNASSIGNED)
        (ERRSET (dorun (map #'(lambda [THY] (or (THY thx) (ERR nil))) thprd)))))

(§ defn- THUNION [l1 l2]
    (dorun (map #'(lambda [THX]
            (COND ((memq THX l2))
                (:else (SETQ l2 (cons THX l2)))))
        l1))
    l2)

(§ defn- THMATCH1 [thx thy]
    ;; THX IS THE PATTERN TO BE MATCHED.
    ;; THY IS THE POSSIBLE CANDIDATE.
    ;; THMATCH1 DOES PRELIMINARY WORK BEFORE HANDING THE PATTERN AND CANDIDATE OFF TO THMATCH2
    ;; WHO DOES THE REAL WORK.
    (let [THML nil]
        ;; THML IS A FREE VARIABLE WHO WILL BE HACKED BY THMATCH2 WHEN THMATCH2 IS DONE,
        ;; THML WILL HAVE A RECORD OF ALL VARIABLE ASSIGNMENTS MADE DURING THE MATCH.
        ;; NATURALLY WE MUST KEEP TRACK SO IF WE FAIL BACK WE CAN UNDO THEM.
        ;; WE HAVE TO CHECK THAT THE PATTERN AND CANDIDATE ARE OF THE SAME LENGTH
        ;; SINCE THE USER MAY HAVE SPECIFIED THE CANDIDATE WITH A "THUSE" RECOMMENDATION.
        (COND ((and (= (count (COND ((= (car thx) 'THEV) (SETQ thx (THVAL (cadr thx) THOLIST))) (thx))) (count thy))
                ;; IF THE MATCH FAILS, THMATCH2 EXITS WITH AN ERR
                ;; WILL BE "TRUE" PROVIDED THE MATCH WORKED
                (ERRSET (dorun (map #'THMATCH2 thx thy))))
            ;; SO RECORD THE ASSIGNMENTS ON THTREE
            (and THML (THPUSH THTREE (list 'THMUNG THML)))
            (RETURN true))
        ;; IF THE MATCH FAILED, WE MAY STILL HAVE SOME ASSIGNEMENTS ALREADY MADE.
        ;; THESE MUST IMMEDIATELY BE UNDONE.  EVLIS JUST EVALS EVERYTHING ON THML,
        ;; WHICH IS A LIST OF EXPRESSIONS, WHICH WHEN EVALED, UNASSIGN THE VARIABLES.
        (:else (evlis THML) (RETURN nil)))))

(§ defn- THMATCHLIST [thtb thwh]
    ;; THTB IS A PATTERN WHICH EVENTUALLY IS TO BE MATCHED.
    ;; THWH SAYS IF IT IS AN ASSERTION, CONSEQUENT THEOREM, ETC.
    ;; THMATCHLIST GOES THROUGH THE DATA BASE, LOOKING ON ALL THE BUCKETS OF THE ATOMS IN THE PATTERN.
    ;; IT RETURNS THE SHORTEST BUCKET TO THGOAL.
    (let [THB1 nil THB2 nil THL nil THNF nil THAL nil THA1 nil THA2 nil THRN nil THL1 nil THL2 nil THRVC nil]
        ;; THL IS THE LENGTH OF THE SHORTEST BUCKET FOUND SO FAR.
        ;; INITIALLY IT IS SET TO A VERY LARGE NUMBER.
        (SETQ THL 34359738367)
        ;; COUNTER WHICH SAYS WHICH PATTERN ITEM WE ARE WORKING ON.
        (SETQ THNF 0)
        ;; LENGTH OF PATTERN.
        (SETQ THAL (count thtb))
        ;; THB1 WILL BE THE REMAINDER OF THE PATTERN TO YET BE WORKED ON.
        ;; WHEN IT IS NIL, WE ARE DONE, SO RETURN THE BUCKET.
        ;; THL1 IS THE BUCKET UNDER THE ATOM.
        ;; THL2 IS THE BUCKET UNDER THE VARIABLE IN THAT POSITION.
        ;; IF WE ARE WORKING ON AN ASSERTION, THL2 WILL BE () SINCE THERE ARE NO VARIABLES IN ASSERTIONS.
        ;; IN THEOREMS, WE MUST TAKE INTO ACCOUNT THE FACT THAT THE THEOREM MAY HAVE EITHER THE CORRECT ATOM,
        ;; OR A VARIALBE IN A GIVEN POSITION, AND STILL MATCH.
        (SETQ THB1 thtb)
    THP1 (or THB1
            (RETURN (COND (THL2 (concat THL1 THL2)) (THL1))))
        ;; ADD1 TO POSITION COUNTER.
        (SETQ THNF (inc THNF))
        ;; THB2 IS THE ITEM WE ARE WORKING ON IN THIS PASS.
        (SETQ THB2 (car THB1))
        ;; UPDATE THB1.
        (SETQ THB1 (cdr THB1))
        ;; IF THE ITEM IS NOT A NORMAL ATOM, SKIP IT AND GO TO NEXT PASS.
    THP3 (COND ((or (nil? (term? THB2)) (number? THB2) (= THB2 '?))
                (GO THP1))
            ;; IF THE ITEM DOES NOT HAVE THE PROPERTY ON ITS PROPERTY LIST,
            ;; THEN IT OBVIOUSLY DOSEN'T HAVE ANY BUCKET AT ALL.
            ;; SO THA1, WHICH RECORDS THE NUMBER IN THE BUCKET IS SET TO 0
            ((not (SETQ THA1 (get THB2 thwh)))
                ;; IF A BUCKET IS FOUND, THE FIRST THING IN THE BUCKET WILL BE THE NUMBER OF GOODIES THERE.
                ;; THE REST WILL BE THE GOODIES.  THE FIRST 0 IN THA1 THEN SAYS THAT THERE WAS NO BUCKET.
                ;; THE SECOND 0 IS JUST A DUMMY FOR THE GOODIES WHICH ARN'T THERE.
                (SETQ THA1 '(0 0)))
            ;; IF IT IS A THNOHASH, WE IGNORE IT JUST LIKE A LIST, OR NUMBER.
            ((= THA1 'THNOHASH) (GO THP1))
            ;; SAME IF THERE IS NO SUB-BUCKET FOR THE ATOM IN THE CORRECT POSITION.
            ((not (SETQ THA1 (assq THNF (cdr THA1))))
                (SETQ THA1 '(0 0)))
            ;; SAME FOR SUB-SUB-BUCKET (PATTERN LENGTH).
            ((not (SETQ THA1 (assq THAL (cdr THA1))))
                (SETQ THA1 '(0 0))))
        (SETQ THRN (cadr THA1))
        (SETQ THA1 (cddr THA1))
        ;; IF IT'S AN ASSERTION, THEN WE DONT HAVE TO LOOK FOR VARIABLES.
        (and (= thwh 'THASSERTION) (GO THP2))
        ;; THVRB IS THE ATOM WHICH HAS THE BUCKET FOR VARIABLES.
        ;; WE WILL NOW LOOK TO SEE IF THERE ARE ANY THEOREMS WHICH
        ;; HAVE A VARIABLE IN THE CORRECT POSSITION.
        (COND
            ((not (SETQ THA2 (get 'THVRB thwh))) (SETQ THA2 '(0 0)))
            ((not (SETQ THA2 (assq THNF (cdr THA2)))) (SETQ THA2 '(0 0)))
            ((not (SETQ THA2 (assq THAL (cdr THA2)))) (SETQ THA2 '(0 0))))
        (SETQ THRVC (cadr THA2))
        (SETQ THA2 (cddr THA2))
        ;; SEE IF THE SUM OF THE NUMBER OF GOODIES IN THE ATOM BUCKET PLUS
        ;; THE NUMBER IN THE VARIABLE BUCKET IS GREATER THAN THE SMALLEST
        ;; NUMBER SO FAR.  IF SO, WE KEEP THE PREVIOUS NUMBER.
        (and (> (+ THRVC THRN) THL) (GO THP1))
        ;; OTHERWISE THIS BECOMES THE NEW SMALLEST,
        (SETQ THL (+ THRVC THRN))
        ;; AND THL1 AND THL2 ARE POINTERS TO THE NEWLY DISCOVERD BUCKETS.
        (SETQ THL1 THA1)
        (SETQ THL2 THA2)
        ;; GO BACK FOR ANOTHER PASS.
        (GO THP1)

        ;; THIS SECTION IS FOR ASSERTIONS, I.E. DON'T HAVE TO CONSIDER VARIABLES.
    THP2 (COND
            ;; IF THERE IS NO BUCKET, THEN RETURN SINCE NOTHING WILL MATCH THE PATTERN.
            ((== THRN 0) (RETURN nil))
            ;; IF THE NEW BUCKET IS SMALLER, IT BECOMES THE SMALLEST SO FAR.
            ((> THL THRN) (SETQ THL1 THA1) (SETQ THL THRN)))
        ;; GO BACK FOR ANOTHER PASS.
        (GO THP1)))

(§ defq- THMESSAGE [tha]
    (THPUSH THTREE (cons 'THMESSAGE tha))
    THVALUE)

(§ defn- THMESSAGEF []
    (let [bod (car THTREE)]
        (THPOPT)
        (COND ((and (THBIND (cadr bod)) (THMATCH1 (caddr bod) THMESSAGE))
                (THPUSH THTREE (list 'THPROG (cddr bod) nil (cddr bod)))
                (SETQ THMESSAGE nil)
                (RETURN (THPROGA)))
            (:else (SETQ THALIST THOLIST)))
        nil))

(§ defn- THMESSAGET [] (THPOPT) THVALUE)

(§ defn- THMUNGF [] (evlis (cadar THTREE)) (THPOPT) nil)

(§ defn- THMUNGT [] (THPOPT) THVALUE)

(§ defn- THNOFAIL [thx]
    (COND (thx (DEFPROP THPROG THPROGT THFAIL))
        (:else (DEFPROP THPROG THPROGF THFAIL))))

(§ defq- THNOHASH [tha]
    (dorun (map #'(lambda [x] (PUTPROP (car tha) 'THNOHASH x))
        (or (cdr tha) '(THASSERTION THCONSE THANTE THERASING)))))

(§ defq- THNOT [tha]
    (SETQ THEXP (list 'THCOND (list (car tha) '(THFAIL THAND)) '((THSUCCEED)))))

(§ defq- THNV [x] (THV1 (car x)))

(§ defq- THOR [tha]
    (and tha
        (THPUSH THTREE (list 'THOR tha))
        (SETQ THEXP (car tha))))

(§ defn- THOR2 [p]
    (COND (THMESSAGE (THPOPT) nil)
        ((and (cadar THTREE) (cdadar THTREE))
            (RPLACA (cdar THTREE) (cdadar THTREE))
            (SETQ THEXP
                (COND (p (PROG1 (caadar THTREE) (or (cadar THTREE) (THPOPT))))
                    ((car (caadar THTREE))))))
        (:else (THPOPT) nil)))

(§ defn- THORF [] (THOR2 true))

(§ defn- THORT [] (THPOPT) THVALUE)

(§ defn- THPOPT [] (SETQ THTREE (cdr THTREE)))

(§ defq- THPROG [tha]
    ;; THBIND HACKS THALIST TO BIND THE VARIABLES.
    ;; IT ALSO HACKS THTREE SO WE CAN UNDO IT IF NEEDED.
    (THBIND (car tha))
    ;; PUT THPROG MARK ON THTREE.
    ;; THE FIRST THA IS A POINTER ONE BEFORE THE NEXT PART OF THE THPROG TO BE HANDELED.
    ;; THE SECOND ONE WILL BE KEPT WHOLE TO SEARCH FOR PROG TAGS.
    (THPUSH THTREE (list 'THPROG tha nil tha))
    ;; CALL WORKHORSE
    (THPROGA))

(§ defn- THPROGA []
    ((lambda [x] (COND
            ;; ODD CASE WHERE THE THPROG HAS NO SUBEXPRESSIONS.  RETURN SUCCESS.
            ((nil? (cdar x)) (THPOPT) 'THNOVAL)
            ;; NEXT ITEM IS AN ATOM, HENCE A THPROG TAG.
            ((term? (cadar x))
                ;; USE THEXP TO MARK IT ON THTREE.
                (SETQ THEXP (list 'THTAG (cadar x)))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA x (cdar x))
                THVALUE)
            ;; OTHERWISE NEXT EXPRESSION TO BE EVALUATED IS THE NEXT EXPRESSION OF THE THPROG.
            (:else (SETQ THEXP (cadar x))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA x (cdar x))
                THVALUE)))
        (cdar THTREE)))

;; THBRANCH AND THBRANCHUN ARE THE MAIN FUNCTIONS IN CHARGE OF HANDLING THE EFFECTS OF SUCCESS AND FAILURE.
;; THEY ARE ONLY CALLED BY THPROGT AND F

(§ defn- THPROGF [] (THBRANCHUN) nil)

(§ defn- THPROGT [] (THBRANCH) (THPROGA))

(§ defn- THPURE [xx]
    ;; CHECKS TO MAKE SURE THAT THE PATTERN HAS NO UNASSIGNED VARIABLES IN IT.

    ;; XX, NATURALLY ENOUGH IS THE PATTERN
    ;; SINCE THPURE IS ALWAYS CALLED AFTER THVARSUBST
    ;; ANY VARIABLES WHICH DO HAVE ASSIGNMENTS WILL HAVE
    ;; GONE AWAY, RREPLACED BY THEIR ASSIGNMENTS
    ;; SO ALL WE NEED DO IS LOOK FOR ANY VARIABLES APPEARING AT ALL
    (ERRSET (dorun (map #'(lambda [y] (and (THVAR y) (ERR nil))) xx))))

(§ defn- THPUTPROP [ato val ind]
    (THPUSH THTREE
        (list 'THMUNG
            (list (list 'PUTPROP
                (list 'quote ato)
                (list 'quote (get ato ind))
                (list 'quote ind)))))
    (PUTPROP ato val ind))

(§ defn- THREM1 [thb]
    ;; THIS FUNCTION IS ROUGHLY THE SAME AS THIP, EXCEPT WHILE THIP ADDS ASSERTIONS TO THE DATABASE, THREM1 REMOVES THEM
    ;; HENCE ALL COMMENTS WILL BE GUIDES TO THE CORRESPONDENCE BETWEEN THREM1 AND THIP

    ;; THB = THI IN THIP
    (let [THA nil THSV nil THA1 nil THA2 nil THA3 nil THA4 nil THA5 nil THONE nil THPC nil]
        ;; THA AND THA1 DO THE WORK OF THT1 IN THIP
        ;; THA1 = THT2
        ;; THA3 = THT3
        ;; THA4, THA5, THONE, AND THPC ARE NEW
        (SETQ THNF (inc THNF))
        ;; THIS COND SERVES THE SAME PURPOSE AS THE FIRST COND IN THIP
        (COND ((and (term? thb) (not (= thb '?)) (not (number? thb)))
                (SETQ THA thb))
            ((or (= thb '?) (memq (car thb) '(THV THNV)))
                (COND (THFST (RETURN 'THVRB))
                    ((SETQ THA 'THVRB))))
            ((RETURN 'THVRB)))
        ;; ALL THE REST SERVES THE SAME PURPOSE AS THE SECOND COND IN THIP.
        ;; IT WAS ORIGINALLY WRITTEN AS A SINGLE COND, BUT THE COMPILER BARFED ON IT,
        ;; SO IT WAS BROKEN UP INTO BITE SIZE PIECES.
        (SETQ THA1 (get THA THWH))
        (or THA1 (RETURN nil))
        (and (= THA1 'THNOHASH) (RETURN 'THBQF))
        (SETQ THA2 (THBA THNF THA1))
        (or THA2 (RETURN nil))
        (SETQ THA3 (THBA THAL (cadr THA2)))
        (or THA3 (RETURN nil))
        (SETQ THA4 (cadr THA3))
        (SETQ THPC (not (= THWH 'THASSERTION)))
        (SETQ THA5
            (COND ((or THFST THFSTP) (THBAP THBS (cdr THA4)))
                ((THBA (COND (THPC THON) (:else (car THON))) (cdr THA4)))))
        (or THA5 (RETURN nil))
        (SETQ THONE (cadr THA5))
        (RPLACD THA5 (cddr THA5))
        (and (not (== (cadr THA4) 1))
            (or (SETQ THSV (cddr THA4)) true)
            (RPLACA (cdr THA4) (dec (cadr THA4)))
            (RETURN THONE))
        (SETQ THSV (cddr THA3))
        (RPLACD THA3 THSV)
        (and (cdadr THA2) (RETURN THONE))
        (SETQ THSV (cddr THA2))
        (RPLACD THA2 THSV)
        (and (cdr THA1) (RETURN THONE))
        (REMPROP THA THWH)
        THONE))

(§ defn- THREMBINDF [] (SETQ THALIST (cadar THTREE)) (THPOPT) nil)

(§ defn- THREMBINDT [] (SETQ THALIST (cadar THTREE)) (THPOPT) THVALUE)

(§ defn- THREMOVE [thb]
    ;; THIS FUNCTION IS ANALAGOUS TO THADD EXCEPT THREMOVE REMOVES RATHER THAN ADDS.
    ;; AS WITH THREM1, ALL COMMENTS WILL EXPLICATE THE ANALOGY,
    ;; SO ONE SHOULD FIRST BECOME FAMILIAR WITH THADD.

    ;; THB = THTT IN THADD.
    ;; THREMOVE TAKES ONLY ONE ARG SINCE THE PROPERTY LIST FOR THE ASSERTION
    ;; PLAYS NO ROLE IN REMOVING THE ASSERTION.
    (let [THB1 nil THWH nil THNF nil THAL nil THON nil THBS nil THFST nil THFSTP nil THFOO nil]
        ;; THB1 AND THON TOGETHER SHARE THE WORK OF THT1 AND THCK IN THADD
        ;; THAL = THLAS
        ;; THBS = THTTL
        (SETQ THNF 0)
        ;; THE STRUCTURE OF THE TWO PROGRAMS IS VIRTUALLY IDENTICAL.
        (SETQ THB1
            (COND ((term? thb)
                    (SETQ THBS thb)
                    (SETQ THWH (car (SETQ THB1 (get thb 'THEOREM))))
                    (caddr THB1))
                ((SETQ THWH 'THASSERTION)
                    (SETQ THBS thb))))
        (SETQ THAL (count THB1))
        (SETQ THFST true)
    THP1 (COND ((nil? THB1)
                (SETQ THB1 THFOO)
                (SETQ THNF 0)
                (SETQ THFST (SETQ THFOO nil))
                (SETQ THFSTP true)
                (GO THP1))
            ((nil? (SETQ THON (THREM1 (car THB1))))
                (RETURN nil))
            ((memq THON '(THBQF THVRB))
                (SETQ THFOO (concat THFOO (list (COND ((= THON 'THVRB) (car THB1))))))
                (SETQ THB1 (cdr THB1))
                (GO THP1)))
        (SETQ THFST nil)
        (dorun (map #'THREM1 (cdr THB1)))
        (SETQ THNF 0)
        (dorun (map #'THREM1 THFOO))
        THON))

(§ defn- THREMPROP [ato ind]
    (THPUSH THTREE
        (list 'THMUNG
            (list (list 'PUTPROP
                (list 'quote ato)
                (list 'quote (get ato ind))
                (list 'quote ind)))))
    (REMPROP ato ind))

(§ defq- THRESTRICT [thb]
    (let [x (THGAL (car thb) THALIST)]
        (COND ((term? x)
                (thprintc "THRESTRICT IGNORED - CONTINUING"))
            ((THRPLACD (cdr x) (THUNION (cddr x) (cdr thb)))))
        x))

(§ defq- THRETURN [x]
    (APPLY #'THSUCCEED (cons 'THPROG x)))

(§ defn- THRPLACA [x y]
    (let [THML nil]
        (THRPLACAS x y)
        (THPUSH THTREE (list 'THMUNG THML))
        x))

(§ defn- THRPLACAS [x y]
    (THPUSH THML (list 'THURPLACA x (car x)))
    (RPLACA x y))

(§ defq- THURPLACA [l] (RPLACA (car l) (cadr l)))

(§ defn- THRPLACD [x y]
    (let [THML nil]
        (THRPLACDS x y)
        (THPUSH THTREE (list 'THMUNG THML))
        x))

(§ defn- THRPLACDS [x y]
    (THPUSH THML (list 'THURPLACD x (cdr x)))
    (RPLACD x y))

(§ defq- THURPLACD [l] (RPLACD (car l) (cadr l)))

(§ defq- THSETQ [thl1]
    (let [THML nil thl thl1]
    =>  (COND
            ((nil? thl)
                (THPUSH THTREE (list 'THMUNG THML))
                (RETURN THVALUE))
            ((nil? (cdr thl))
                (PRINT thl1)
                (thert "ODD NUMBER OF GOODIES - THSETQ"))
            ((term? (car thl))
                (THPUSH THML (list 'SETQ (car thl) (list 'quote (eval (car thl)))))
                (SET (car thl) (SETQ THVALUE (eval (cadr thl)))))
            (:else (THRPLACAS (cdr (THSGAL (car thl)))
                (SETQ THVALUE (THVAL (cadr thl) THALIST)))))
        (SETQ thl (cddr thl))
        (GO =>)))

(§ defn- THSGAL [x]
    (sassq (cadr x) THALIST
        #'(lambda [] (let [y (list (cadr x) 'THUNASSIGNED)]
            (concat (get 'THALIST 'VALUE) (list y))
            y))))

(§ defq- THSTATE [thindicators]
    ;; PRINTS THAT PART OF THE STATE OF THE MICRO-PLANNER WORLD SPECIFIED BY THE INDICATORS IN REREADABLE FORM.
    ;; NOTE THAT IT IS BLIND TO ASSERTIONS THAT BEGIN WITH EITHER NUMBERS, LIST STRUCTURE, NOHASHED ATOMS OR NON-INTERNED ATOMS.
    (let [THP nil]
        (PRINT '(THDATA))
        (dorun (map #'(lambda [BUCKET]
            (dorun (map #'(lambda [THATOM]
                (dorun (map #'(lambda [THWH]
                    (and (SETQ THP (get THATOM THWH)) (SETQ THP (assq 1 (cdr THP)))
                        (dorun (map #'(lambda [LENGTH-BUCKET]
                            (dorun (map #'(lambda [ASRT]
                                (COND ((= THWH 'THASSERTION) (PRINT ASRT)) ((PRINT (list ASRT)))))
                            (cddr LENGTH-BUCKET))))
                        (cdr THP)))))
                (COND (thindicators) (' (THASSERTION THANTE THCONSE THERASING))))))
            BUCKET)))
        (MAKOBLIST nil)))
        (PRINT nil)
        nil))

(§ defq- THSUCCEED [tha]
    (or (not tha)
        (let [THX nil]
            (and (= (car tha) 'THEOREM)
                (SETQ tha (cons 'THPROG (cdr tha))))
            (SETQ THBRANCH THTREE)
            (SETQ THABRANCH THALIST)
        =>  (COND
                ((nil? THTREE)
                    (PRINT tha)
                    (thert "OVERPOP - THSUCCEED"))
                ((= (caar THTREE) 'THREMBIND)
                    (SETQ THALIST (cadar THTREE))
                    (THPOPT)
                    (GO =>))
                ((= (caar THTREE) (car tha))
                    (THPOPT)
                    (RETURN (COND ((cdr tha) (eval (cadr tha))) ('THNOVAL))))
                ((and (= (car tha) 'THTAG) (= (caar THTREE) 'THPROG) (SETQ THX (memq (cadr tha) (cadddr (car THTREE)))))
                    (RPLACA (cdar THTREE) (cons nil THX))
                    (RETURN (THPROGT)))
                (:else (THPOPT) (GO =>)))
        nil)))

(§ defn- THTAE [xx]
    (COND
        ((term? xx) nil)
        ((= (car xx) 'THUSE)
            (doall (map #'(lambda [x]
                    (COND ((not (and (SETQ THXX (get x 'THEOREM)) (= (car THXX) TYPE)))
                            (PRINT x)
                            (list 'THAPPLY (thert "BAD THEOREM - THTAE") (car THX)))
                        (:else (list 'THAPPLY x (car THX)))))
                (cdr xx))))
        ((= (car xx) 'THTBF)
            (doall (map #'(lambda [y]
                    (COND (((cadr xx) y)
                        (list (list 'THAPPLY y (car THX))))))
                (COND (THY1 THY)
                    ((SETQ THY1 true)
                        (SETQ THY (THMATCHLIST (car THX) TYPE)))))))
    (:else (PRINT xx) (THTAE (thert "UNCLEAR RECOMMENDATION - THTAE")))))

(§ defq- THTAG [l]
    (and (car l)
        (THPUSH THTREE (list 'THTAG (car l)))))

(§ defn- THTAGF [] (THPOPT) nil)

(§ defn- THTAGT [] (THPOPT) THVALUE)

(§ defn- THTRUE [_] true)

(§ defn- THTRY1 []
    ;; TRIES NEXT RECOMMENDATION ON TREE FOR THGOAL.
    ;; THZ = (THGOAL PATTERN EXPANDED-RECOMMENDATIONS)
    (let [THX nil THY nil THZ (car THTREE) THW nil THEOREM nil]
        (SETQ THY (cddr THZ))                                       ;; = RECOMMENDATIONS
        (RPLACD THY (dec (cdr THY)))
    NXTREC (COND ((or (nil? (car THY)) (zero? (cdr THY)))
            (RETURN nil)))                                          ;; RECOMMENDATIONS EXHAUSTED. FAIL
        (SETQ THX (caar THY))
        (GO (car THX))
    THNUM (RPLACD THY (cadr THX))
        (RPLACA THY (cdar THY))
        (GO NXTREC)
    THDBF (SETQ THOLIST THALIST)
        (COND ((nil? (caddr THX))
                (RPLACA THY (cdar THY))
                (GO NXTREC))                                        ;; NO MORE CANDIDATES SATISFYING THIS REC, TRY NEXT
            ((PROG1 (and ((cadr THX) (SETQ THW (caaddr THX))) (THMATCH1 (cadr THZ) (car THW)))
                    (RPLACA (cddr THX) (cdaddr THX)))
                (RETURN THW))
            (:else (GO THDBF)))
    THTBF (COND ((nil? (caddr THX))
                (RPLACA THY (cdar THY))
                (GO NXTREC)))                                       ;; NO MORE CANDIDATES SATISFYING THIS REC, TRY NEXT
        (SETQ THEOREM (caaddr THX))
    THTBF1 (COND ((not (and (SETQ THW (get THEOREM 'THEOREM)) (= (car THW) 'THCONSE)))
                (PRINT THEOREM)
                (COND ((= (SETQ THEOREM (thert "BAD THEOREM - THTRY1")) 'true)
                        (GO NXTREC))
                    (:else (GO THTBF1)))))
        (COND ((PROG1 (and ((cadr THX) (caaddr THX)) (THAPPLY1 THEOREM THW (cadr THZ)))
                    (RPLACA (cddr THX) (cdaddr THX)))
                (RETURN true))
            (:else (GO THTBF)))))

(§ defn- THTRY [x]
    ;; THTRY IS IN CHARGE OF MAKING UP THE "THINGS TO DO" LIST, WHICH IS PUT ON THTREE.
    ;; SO WHENEVER WE FAIL BACK TO A THGOAL, WE GO TO THE NEXT "THING TO DO".
    ;; X IS THE LIST OF RECOMMENDATIONS.
    (COND ;; ANY ATOMIC RECOMMENDATION IS IGNORED.  THIS IS USEFUL IN ERROR RECOVERY.
        ((term? x) nil)
        ;; HAVE A THEOREM BASE FILTER
        ((= (car x) 'THTBF)
            ;; MAKE UP A LIST WHICH GIVES
            ;; 1 - THE INDICATOR "THTBF"
            ;; 2 - THE ACTUAL FILTER (THTRUE IS THE MOST COMMON)
            ;; 3 - THE BUCKET RETURNED BY THMATCHLIST
            (COND ((not THZ1) (SETQ THZ1 true) (SETQ THZ (THMATCHLIST THA2 'THCONSE))))
            (COND (THZ (list (list 'THTBF (cadr x) THZ))) (:else nil)))
        ;; DO THE SAME THING, ONLY FOR DATA BASE FILTERS.
        ((= (car x) 'THDBF)
            (COND ((not THY1) (SETQ THY1 true) (SETQ THY (THMATCHLIST THA2 'THASSERTION))))
            (COND (THY (list (list 'THDBF (cadr x) THY))) (:else nil)))
        ;; THUSE STATEMENTS ARE TRANSLATED INTO THTBF THTRUE STATEMENTS,
        ;; WHICH THE "BUCKET" IS THE LIST GIVEN IN THE THUSE.
        ((= (car x) 'THUSE)
            (list (list 'THTBF 'THTRUE (cdr x))))
        ((= (car x) 'THNUM)
            (list x))
        (:else (PRINT x) (THTRY (thert "UNCLEAR RECOMMENDATION - THTRY")))))

(§ defn- THUNDOF []
    (COND ((nil? (caddar THTREE)) (THPOPT))
        (:else (SETQ THXX (cddar THTREE))
            (SETQ THALIST (caadr THXX))
            (RPLACA (cdr THXX) (cdadr THXX))
            (SETQ THTREE (caar THXX))
            (RPLACA THXX (cdar THXX))))
    nil)

(§ defn- THUNDOT [] (THPOPT) true)

(§ defq- THUNIQUE [tha]
    (SETQ tha (cons 'THUNIQUE (doall (map eval tha))))
    (let [x THALIST]
    =>  (COND ((nil? x) (THPUSH THALIST tha) (RETURN true))
            ((= (caar x) 'THUNIQUE)
                (COND ((= (car x) tha) (RETURN nil)))))
        (SETQ x (cdr x))
        (GO =>)))

(§ defn- THV1 [x]
    ;; (THV1 'X) IS THE VALUE OF THE PLANNER VARIABLE.
    ;; ($? X) RETURNS ERROR MESSAGE IF X UNBOUND OR UNASSIGNED.
    (SETQ THXX x)
    (COND ((= (SETQ x (cadr (sassq x THALIST #'(lambda [] (PRINT THXX) (thert "THUNBOUND - THV1"))))) 'THUNASSIGNED)
            (PRINT THXX)
            (thert "THUNASSIGNED - THV1"))
        (:else x)))

(§ defq- THV [x]
    ;; (THV X) IS THE VALUE OF THE PLANNER VARIABLE ($? X)
    (THV1 (car x)))

(§ defn- THVAL [THEXP THALIST]
    ;; CORRESPONDS TO LISP EVAL.
    ;; THEXP IS THE EXPRESSION TO BE THVALUATED.
    ;; THALIST IS THE VARIABLE BINDING LIST.

    (let [THTREE nil THVALUE 'THNOVAL THBRANCH nil THOLIST nil THABRANCH nil THE nil THMESSAGE nil]
        (SETQ THV '(THV THNV))

        ;; "THE" BECOMES THE CURRENT EXPRESSION.
        ;; THEXP IS RESERVED FOR FURTHER EXPRESSIONS,
        ;; WHICH SHOULD BE THVALED BEFORE WE GO TO THE NEXT ITEM OF ACTUAL CODE.
        ;; FOR EXAMPLE, THASSERT USES THIS FEATURE TO PROCESS ANTECEDENT THEOREMS.
    GO  (SETQ THE THEXP)
        (SETQ THEXP nil)
        ;; THSTEP AND ITS RELATIVES ARE FOR STEPPING THROUGH PLANNER FUNCTIONS IN A SPECIAL WAY.
        ;; TO THIS DATE ONLY SUSSMAN KNOWS EXACTLY WHAT IT IS SUPPOSE TO DO.
        ;; YOU CAN SAFELY IGNORE ANY EXPRESSION WHICH MENTIONS IT.
        (COND (THSTEP (eval THSTEP)))
        ;; EVAL THE CURRENT EXPRESSION TO BE THVALED.
        ;; NOTE THAT EACH PLANNER FUNCTION CORRESPONDS TO THREE LISP FUNCTIONS:
        ;; ONE TO SET THINGS UP (THIS IS WHAT IS GETTING EVALED AT THIS POINT),
        ;; ONE TO HANDLE SUCCESS AND ONE FOR FAILURE.
        (COND ((ERRSET (SETQ THVALUE (eval THE))))
            ;; IF THERE WAS A LISP ERROR, REPORT IT TO THE USER.
            (:else (PRINT THE)
                (SETQ THVALUE (thert "LISP ERROR - THVAL"))))
    GO1 (COND (THSTEPD (eval THSTEPD)))
        ;; USUALLY THEMESSAGE WILL BE NIL.
        ;; EXCEPTION IS WHEN USER HAS USED THE THMESSAGE FUNCTION.
        (COND (THMESSAGE (GO MFAIL))
            ;; IF THEXP IS NON NIL, IT MEANS THAT WE HAVE MORE PLANNER TO WORK ON BEFORE GOING TO NEXT LINE OF USER CODE.
            (THEXP (GO GO))
            ;; IF THVALUE IS NON NIL, IT MEANS THAT SO FAR THE THEOREM IS SUCCEEDING.
            (THVALUE (GO SUCCEED))
            ;; ELSE WE ARE IN A FAILURE SITUATION.
            (:else (GO FAIL)))
    SUCCEED ;; HANDLES SUCCESS
        (COND (THSTEPT (eval THSTEPT)))
        ;; SAVE CURRENT STATE OF THTREE AND THALIST IN CASE WE HAVE TO BACK UP.
        (COND ((nil? THBRANCH)
            (SETQ THBRANCH THTREE)
            (SETQ THABRANCH THALIST)))
        ;; IF THE THTREE IS NIL, IT MEANS THAT THE THPROG OR WHATEVER HAS BEEN COMPLETED,
        ;; SO THERE ARE NO MORE EXPRESSIONS TO DO.  ALL THEOREMS ACT LIKE A THPROG,
        ;; INCLUDING PUTTING ITS MARK ON THTREE, SEE THAPPLY, HENCE NO NEED TO GROW MORE BRANCHES ON THTREE.
        (COND ((nil? THTREE)
                (RETURN THVALUE))
            ;; THIS IS THE NORMAL CASE.
            ;; WE EVAL THE SUCCEED-FUNCTION OF THE PLANNER FUNCTION WHICH JUST SUCCEEDED.
            ((SETQ THEXP (get (caar THTREE) 'THSUCCEED))
                (GO GO2))
            ;; IN CASE OF LOSSAGE, LETS THE USER SUCCEED ANYWAY.
            ((thert "BAD SUCCEED - THVAL")
                (GO SUCCEED))
            ((GO FAIL)))
        ;; HAS TO DO WITH FAILURE + MESSAGE
    MFAIL (COND ((= (car THMESSAGE) THTREE)
            (SETQ THEXP (cadr THMESSAGE))
            (SETQ THMESSAGE nil)
            (GO GO)))
    FAIL (COND (THSTEPF (eval THSTEPF)))
        ;; IF THTREE IS NIL, WE HAVE FAILED THE ENTIRE EXPRESSION.
        (COND ((nil? THTREE)
                (RETURN nil))
            ;; NORMAL CASE.  EVAL THE FAILURE FUNCTION ASSOCIATED
            ;; WITH THE PLANNER FUNCTION WHICH JUST FAILED.
            ((SETQ THEXP (get (caar THTREE) 'THFAIL))
                (GO GO2))
            ((thert "BAD FAIL - THVAL")
                (GO SUCCEED))
            ((GO FAIL)))
        ;; THEXP AT THIS POINT IS THE APPROPRIATE SUCCESS OR FAILURE ASSOCIATED FUNCTION.
        ;; EVAL IT AND AT THE SAME TIME,
        ;; SET IT TO NIL IN CASE WE NEED THEXP FOR MORE EXPRESSIONS TO BE PROCESSED.
    GO2 (SETQ THVALUE ((PROG1 THEXP (SETQ THEXP nil))))
        ;; GO THROUGH THE ENTIRE PROCESS AGAIN.
        ;; A TYPICAL PROCESS IN SUCCESS IS TO KEEP REMOVING EXPRESSIONS FROM THTREE
        ;; UNTIL WE GET BACK TO THE THREE ENTRY PUT ON BY THPROG.
        ;; AT THIS POINT IT EVALS THPROGT, AND SEE THAT LISTING
        (GO GO1)))

(§ defn- THVAR [x]
    ;; PREDICATE - IS ITS INPUT A PLANNER VARIABLE
    (memq (car x) '(THV THNV)))

(§ defn- THVARS2 [x]
    ;; THIS IS THE WORKHORSE FOR THVARSUBST.
    ;; X IS A SINGLE ITEM FROM A PATTERN.
    (let [A nil]
        (and (term? x) (RETURN x))
        ;; IF IT'S AN ATOM, NOTHING NEED BE DONE.
        (and (= (car x) 'THEV)
            (SETQ x (THVAL (cadr x) THALIST)))
        ;; IF THE EXPRESSION HAS A $E BEFORE IT, THVAL BEFORE GOING ON.
        (or (THVAR x) (RETURN x))
        ;; IF THE ITEM IS NOT A VARIABLE, IT MUST BE SOME RANDOM LIST, SO IT HAS NO ASSIGNED VALUE.
        (SETQ A (THGAL x THALIST))
        ;; AT THIS POINT X MUST BE A VARIABLE, SO FIND ITS ASSIGNMENT, THAT'S WHAT THGAL DOES.
        ;; THALIST IS WHERE THE VARIABLE ASSIGNMENTS RESIDE.
        (RETURN (COND
            ((= (cadr A) 'THUNASSIGNED) x)
            ;; IF THE VARIABLE IS UNASSIGNED, THEN RETURN THE ACTUAL VARIABLE.
            ((and THY (= (car x) 'THNV))
                ;; THY WILL BE T JUST IN THE CASES WHERE THVARSUBST WAS CALLED BY A THGOAL SITUATION.
                ;; IT IS THEN NECESSARY TO IMMEDIATELY HACK IN A THUNASSIGNED SO THAT IF THE SAME VARIABLE IS USED
                ;; TWICE IN THE SAME PATTERN, WE WON'T PUT IN ITS OLD VALUE THE SECOND TIME IT IS ENCOUNTERED.
                (THRPLACA (cdr A) 'THUNASSIGNED)
                x)
            ;; OTHERWISE THE ASSIGNMENT IS THE SECOND ELEMENT IN THE BINDING LIST.
            (:else (cadr A))))))

(§ defn- THVARSUBST [thx THY]
    ;; THX IS A GOAL OR ASSERTION PATTERN OR THEOREM NAME.
    ;; THIS FUNCTION RETURNS THE SAME PATTERN, EXCEPT IN PLACE OF ALL ASSIGNED VARIABLES
    ;; THERE WILL BE THE VALUES THEY ARE ASSIGNED TO.
    (COND ((= (car thx) 'THEV)
            ;; IF THE CAR IS THEV, IT MEANS THAT THERE WAS A $E BEFORE THE PATTERN,
            ;; IN WHICH CASE WE ARE TO GET THE REAL PATTERN BY THVALUATING WHAT IS THERE.
            (SETQ thx (THVAL (cadr thx) THALIST)))
        ((THVAR thx)
            (SETQ thx (eval thx))))
    ;; THVAR TESTS TO SEE IF ARG IS A VARIABLE.
    ;; IF THE PATTERN IS A SINGLE VARIABLE, THE PROGRAM ASSUMES THERE SHOULD BE AN IMPLICIT THVAL.
    ;; UNLESS THE ASSERTEE IS A THEOREM NAME, GO THROUGH IT PLACE BY PLACE WITH THVARS2.
    (COND ((term? thx) thx) (:else (doall (map #'THVARS2 thx)))))

(§ defq- THVSETQ [tha]
    (let [a tha]
    =>  (COND ((nil? a) (RETURN THVALUE))
            ((nil? (cdr a))
                (PRINT tha)
                (thert "ODD NUMBER OF GOODIES - THVSETQ"))
            (:else (SETQ THVALUE
                (car (RPLACA (cdr (THSGAL (car a))) (THVAL (cadr a) THALIST))))))
        (SETQ a (cddr a))
        (GO =>)))

(§ DEFPROP THTAG THTAGF THFAIL)
(§ DEFPROP THTAG THTAGT THSUCCEED)
(§ DEFPROP THGOAL THGOALT THSUCCEED)
(§ DEFPROP THGOAL THGOALF THFAIL)
(§ DEFPROP THFAIL? THFAIL?F THFAIL)
(§ DEFPROP THFAIL? thfail?t THSUCCEED)
(§ DEFPROP THAMONG THAMONGF THFAIL)
(§ DEFPROP THFIND THFINDF THFAIL)
(§ DEFPROP THFIND THFINDT THSUCCEED)
(§ DEFPROP THPROG THPROGT THSUCCEED)
(§ DEFPROP THAND THANDT THSUCCEED)
(§ DEFPROP THMUNG THMUNGT THSUCCEED)
(§ DEFPROP THERASE THERASET THSUCCEED)
(§ DEFPROP THASSERT THASSERTT THSUCCEED)
(§ DEFPROP THOR THORT THSUCCEED)
(§ DEFPROP THCOND THCONDT THSUCCEED)
(§ DEFPROP THAND thandf THFAIL)
(§ DEFPROP THPROG THPROGF THFAIL)
(§ DEFPROP THMUNG THMUNGF THFAIL)
(§ DEFPROP THASSERT THASSERTF THFAIL)
(§ DEFPROP THERASE THERASEF THFAIL)
(§ DEFPROP THCOND thcondf THFAIL)
(§ DEFPROP THOR THORF THFAIL)
(§ DEFPROP THDO THDOB THSUCCEED)
(§ DEFPROP THDO THDOB THFAIL)
(§ DEFPROP THUNDO THUNDOF THFAIL)
(§ DEFPROP THUNDO THUNDOT THSUCCEED)
(§ DEFPROP THMESSAGE THMESSAGEF THFAIL)
(§ DEFPROP THMESSAGE THMESSAGET THSUCCEED)
(§ DEFPROP THREMBIND THREMBINDT THSUCCEED)
(§ DEFPROP THREMBIND THREMBINDF THFAIL)

#_(ns shrdlu.thtrac)

;; SYSTEM FUNCTIONS SUCH AS THGOAL, THASSERT, THERASE AND THEOREM
;; (ALL THMS) ARE TRACED IF THEY ARE ON 'THTRACE'.
;; THTRACES1 PUTS THEM THERE AND THUNTRACE TAKES THEM OFF.

;; THTRACE IS INITIALLY SET TO NIL BY TS PLNR.

(§ defq- THTRACE [l] (dorun (map #'THTRACE1 l)))

(§ defn- THTRACE1 [x]
    (let [y nil]
        ;; VARIETY OF POSSIBLE INPUT FORMATS TRANSFORMED TO STANDARD 3 ELEMENT LIST
        ;; (OBJECT-TO-BE-TRACED TRACE-CONDITION BREAK-CONDITION)
        (SETQ x (COND
            ((term? x) (list x true nil))
            ((cddr x) x)
            ((nil? (cdr x)) (PRINT x) (PRINC "BAD FORMAT") (RETURN nil))
            ((list (car x) (cadr x) nil))))
        ;; IF OBJECT-TO-BE-TRACED IS A PARTICULAR THEOREM, THEN THE TRIPLET
        ;; '(THEOREM (THSEL 'CADR) (THSEL 'CADDDR)) IS GUARANTEED TO
        ;; BE ON THTRACE IN ADDITION TO THE STANDARD TRIPLET.
        (COND ((get (car x) 'THEOREM)
            (COND ((SETQ y (assq 'THEOREM THTRACE)) (RPLACD y '((THSEL #'cadr) (THSEL #'caddr))))
                ((SETQ THTRACE (list x (concat '(THEOREM (THSEL #'cadr) (THSEL #'caddr)) THTRACE)))))))
        ;; THTRACE IS UPDATED.  IF THE OBJECT-TO-BE-TRACED IS ALREADY ON THTHRACE, THEN
        ;; THE TRACE AND BREAK CONDITIONS ARE UPDATED, ELSE THE WHOLE TRIPLET IS PLACED ON THTRACE.
        (COND ((SETQ y (assq (car x) THTRACE)) (RPLACD y (cdr x)))
            ((SETQ THTRACE (cons x THTRACE))))
        x))

;; THUNTRACE REMOVES ELEMENTS OF ITS ARG FROM THTRACE.
;; IF NOT GIVEN ANY ARGS, THUNTRACE SETS THTRACE TO NIL.

(§ defq- THUNTRACE [l]
    (COND (l (SETQ THTRACE (doall (map #'(lambda [x]
                        (COND ((memq (car x) l) (PRINT x) nil) ((list x))))
                    THTRACE))))
        ((dorun (map #'PRINT THTRACE)) (SETQ THTRACE nil)))
    'DONE)

;; THTRACES IS ACTIVATED BY THGOAL, THASSERT, ... IF THTRACE IS NON-NIL,
;; THF IS SET TO THE PARTICULAR CANDIDATE FOR TRACEAGE, E.G. TO 'THGOAL
;; IF THE PLANNER FUNCTION THGOAL ACTIVATED THTRACES.
;; THL = THE INSTANTIATED ARG OF THF. SEE DESC OF X ON NEXT PAGE.

(§ defn- THTRACES [thf thl]
    (let [THY nil THZ nil THB nil]
        (and
            ;; THY SET TO TRIPLET ON THTRACE. IF NOT THERE, NO TRACING
            (SETQ THY (assq thf THTRACE))
            ;; IF BOTH TRACE AND BREAK ARE FALSE, DON'T TRACE
            ;; SIDE EFFECT - THB SET TO VALUE OF BREAK
            (or (SETQ THB (THVAL (caddr THY) THALIST)) (THVAL (cadr THY) THALIST))
            ;; THZ IS SET TO THE TRACE FUNCTION FOR THE OBJECT-TO-BE-TRACED
            (or (SETQ THZ (get thf 'THTRACE)) (thert "THTRACES - TRACE LOSSAGE"))
            ;; THE TRACE FUNCTION IS EXECUTED
            (THZ thl THB)
            ;; IF THB IS NON-NIL, BREAK
            THB
            (thert nil))
        nil))

;; THE CAR OF THE TREE IS '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; THUS, THESE TWO FNS PRINT THE NAME OF THE TRACE POINT, "FAIL"-OR-"SUCCEED"
;; PRINT THVALUE IF ANY, AND FINALLY BREAK IF (THERT) IS PRESENT, THEN POP THE TREE

(§ DEFPROP THTRACES
    (lambda []
        (PRINT (cadar THTREE))
        (PRINC "FAILED ")
        (evlis (cddar THTREE))
        (THPOPT)
        nil)
    THFAIL)

(§ DEFPROP THTRACES
    (lambda []
        (PRINT (cadar THTREE))
        (PRINC "SUCCEEDED ")
        (evlis (cddar THTREE))
        (THPOPT)
        THVALUE)
    THSUCCEED)

;; THE TRACE FNS THBKPT, THGOAL, THEOREM, THASSERT, AND THERASE PUSH ONTO THE TREE
;; '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; X = THL = INSTANTIATED GOAL, ASSERTION OR ERASURE, NAME OF THE THM, OR MESSAGE OF THE BREAKPOINT

(§ DEFPROP THBKPT
    (lambda [x B]
        (THPUSH THTREE (list 'THTRACES (THGENS B) (and B '(thert nil))))
        (thprintc "PASSING BKPT")
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        ;; BY SETTING THBRANCH AND THABRANCH, A TRIPLE IS CREATED BY THVAL FOR BACKTRACKING.
        ;; THEN, THE TREE IS POPPED TO PREVENT THTRACES FROM TYPING OUT THE MEANINGLESS
        ;; THAT THE BREAKPOINT SUCCEEDED.
        (SETQ THBRANCH THTREE)
        (SETQ THABRANCH THALIST)
        (THPOPT)
        (PRIN1 x))
    THTRACE)

(§ DEFPROP THGOAL
    (lambda [x B]
        (THPUSH THTREE (list 'THTRACES (THGENS G) '(and THVALUE (PRIN1 THVALUE)) (and B '(thert nil))))
        (thprintc "TRYING GOAL")
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        (PRIN1 x))
    THTRACE)

(§ DEFPROP THEOREM
    (lambda [x B]
        (THPUSH THTREE (list 'THTRACES x '(and THVALUE (PRIN1 THVALUE)) (and B '(thert nil))))
        (thprintc "ENTERING THEOREM")
        (PRIN1 x))
    THTRACE)

(§ DEFPROP THASSERT
    (lambda [x B]
        (THPUSH THTREE (list 'THTRACES (THGENS A) (and B '(thert nil))))
        (PRINT 'ASSERTING)
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        (PRIN1 x))
    THTRACE)

(§ DEFPROP THERASE
    (lambda [x B]
        (THPUSH THTREE (list 'THTRACES (THGENS E) (and B '(thert nil))))
        (PRINT 'ERASING)
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        (PRIN1 x))
    THTRACE)

;; FOR THE TRACE-OBJECT 'THEOREM, IF ANY SPECIFIC THMS ARE TRACED, '(THSEL 'CADR) AND '(THSEL 'CADDDR)
;; ARE THE TRACE AND BREAK PREDICATES.  HENCE THTRACES CAUSES THESE EXPR'S TO BE THVALED.  THL IS SET
;; TO THE SECOND ARG OF THTRACES WHICH IN THIS CASE IS PRESUMABLY THE NAME OF THE PARTICULAR THM THAT
;; ACTIVATED THTRACES.  THSEL FIRST CHECKS TO SEE WHETHER THIS THM IS INDEPENDENTLY ON THTRACE. IF NOT,
;; IT DOES NO MORE.  BUT IF IT IS, THX GETS SET TO THE THM'S TRIPLET.  THEN THX GETS SET TO EITHER THE
;; TRACE (ARG = 'CADR) OR THE BREAK (ARG = 'CADDDR) CONDITION OF THE TRIPLET.  FINALLY, THESE CONDITIONS
;; ARE THVALED, THUS THSEL SERVES THE PURPOSE OF REFERENCING THE TRACE AND BREAK PREDICATES OF PARTICULAR
;; THMS ON THTRACE.

(§ defn- THSEL [thf]
    (let [THX nil] (RETURN (and (SETQ THX (assq THL THTRACE)) (SETQ THX (thf THX)) (THVAL THX THALIST)))))

;; MAKES A NAME WITH PREFIX X AND SUFFIX A UNIQUE NUMBER.
(§ defq- THGENS [x]
    (MAKNAM (concat (EXPLODE (car x)) (EXPLODE (SETQ THGENS (inc THGENS))))))

(§ SETQ THGENS 0)

#_(ns shrdlu.syscom)

;; ##################################################################
;;
;;          SYSCOM - TOPLEVEL AND GENERAL UTILITY FUNCTIONS
;;
;; ##################################################################

(§ defn- SHRDLU []
    (let [END nil BOTH nil BACKREF nil BACKREF2 nil ANSNAME nil LASTREL nil WHO nil PT nil PTW nil SENT nil PUNCT nil H nil N nil NB nil FE nil SM nil RE nil MES nil MESP nil C nil CUT nil GLOBAL-MESSAGE nil LEVEL nil]
        (CLEANOUT TSS EVX NODE ANS OSS RSS X)       ;; FLUSH OLD GENSYMS
    CATCH-LOOP
        (CATCH
            (do
        LOOP    (SETQ SENTNO (inc SENTNO)
                      LEVEL 0
                      LASTSENTNO (inc LASTSENTNO)
                      LASTSENT C
                      GLOBAL-MESSAGE nil
                      MES 'NOPE
                      BACKREF nil)                  ;; ???????????????????
                (SETQ N (SETQ SENT (ETAOIN)))
                (COND
                    ((and
                        (COND
                            (TOPLEVEL-ERRSET? (ERRSET (SETQ PT (SETQ C (parseval PARSEARGS)))))
                            (:else (SETQ PT (SETQ C (parseval PARSEARGS)))))
                        C)
                    (SETQ FE (FE C))
                    (SETQ NB SENT)
                    (SETQ H (H C))
                    (SETQ INTERPRETATION (SM C))
                    (COND (SMN (GO LOOP))
                        ((not ANSWER?) nil)
                        ((COND
                            (TOPLEVEL-ERRSET? (ERRSET (eval '(ANSWER C))))
                            (:else (eval '(ANSWER C)))))
                        ((APPLY #'SAY (or GLOBAL-MESSAGE "I DON'T UNDERSTAND."))))))
            (GO LOOP))
        ABORT-PARSER)
        (GO CATCH-LOOP)))

(defn- parseval [a] (eval (cons 'PARSE a)))

(def- PARSEARGS '(CLAUSE MAJOR TOPLEVEL))

(§ defq- DEFLIST [l]
    (dorun (map #'(lambda [a]
                (PUTPROP (car a) (cadr a) (car l)))
            (cdr l)))
    (car l))

(§ defn- %SENT []
    ;; THIS FUNCTION PRINTS THE CURRENT SENTENCE
    (TERPRI)
    (dorun (map #'PRINT3 SENT))
    (PRINC PUNCT))

(§ defn- DA [x]
    (and (get x 'THASSERTION) (DISP (APPLY #'concat (doall (map #'cddr (APPLY #'concat (doall (map #'cdr (cdr (get x 'THASSERTION)))))))))))

(§ defq- DISP [a]
    (TYO 12)
    (TERPRI)
    (and (cdr a)
        (PRINC (car a))
        (PRINC " >> ")
        (PRINC (cadr a))
        (TERPRI))
    (SPRINT (COND ((cdr a) (get (car a) (cadr a))) ((eval (car a)))) LINEL 0)
    '*)

(§ defn- DP [x]
    (TERPRI)
    (TERPRI)
    (PRINC \[)
    (PRINC x)
    (PRINC \])
    (let [L (PLIST x)]
    A   (COND ((memq (car L) '(PNAME VALUE)) (GO B)))
        (TERPRI)
        (TAB 4)
        (PRINC (car L))
        (SPRINT (cadr L) (- LINEL 18) 18)
    B   (COND ((SETQ L (cddr L)) (GO A)))
        (TERPRI)
        '*))

;; ################################################################
;;         FUNCTIONS FOR HAND-TAILORED GARBAGE COLLECTION
;; ################################################################

(§ defn- FORGET []
    (SETQ LASTSENT nil LASTREL nil BACKREF nil BACKREF2 nil LASTTIME nil LASTPLACE nil)
    (SETQ LASTSENTNO 0)
    (dorun (map #'(lambda [PN]
        (dorun (map #'(lambda [PROP] (REMPROP PN PROP))
            '(BIND LASTBIND))))
        '(IT THEY ONE)))
    (and EVENTLIST (do (THFLUSH HISTORY) (STARTHISTORY))))

;; THIS FUNCTION HAS ALSO INCLUDED A CALL TO "PLNRCLEAN"
;; TO SCRUB AWAY THE EVENTLIST - BUT THE DETAILS OF ITS
;; MICROPLANNER MANIPULATIONS ARE STILL BEING CHECKED FOR
;; VERACTITY IN THE PRESENT DAY ENVIRONMENT (6/24/74)
;;
;; THE CODE WAS:
;;  (DEFUN PLNRCLEAN (X)
;;     (MAPC #'(LAMBDA (Y)
;;               (MAPC #'(LAMBDA (Z)
;;                         (THREMOVE (CAR Z)))
;;                     (CDDR Y)))
;;           (GET X 'THASSERTION)))
;;
;; AND THE CALL WAS:
;;    (MAPC 'PLNRCLEAN EVENTLIST)

(§ defq- CLEANOUT [l]        ;; REMOB'S ALL GENSYMS OF THE MEMBERS OF LIST
    (dorun (map #'(lambda [A]
            (CLEANX A 0 (get A 'MAKESYM))
            (PUTPROP A 0 'MAKESYM))
        l)))

(§ defn- CLEANX [a b c]
    ;; CLEANX REMOB'S GENSYMS OF THE SYMBOL "A" FROM B+1 UP TO AND INCLUDING C
    (let [save (get a 'MAKESYM) I nil]
        (SETQ b (or b 0))
        (and c
            (> c b)
            (PUTPROP a b 'MAKESYM)
            (DO I b (inc I) (= I c) (REMOB (MAKESYM a))))
        (PUTPROP a save 'MAKESYM)))

;; ################################################################
;;         A MOST COMPLETE AND SOPHISTICATED BREAK PACKAGE
;; ################################################################

(defn- ert [message] (ERTEX message nil true))       ;; ALWAYS STOPS, NEVER CAUSES ABORTION.  USED FOR GUARENTEED STOPS AS IN DEBUGGING OR ETAOIN.

(def- thert ert)

(defn- erterr [message] (ERTEX message true nil))    ;; USED FOR KNOWN WIERD STATES SUCH AS CHOP.  USES "NOSTOP" SWITCH, CAUSES ABORTION.

(defn- bug [message]
    (ERTEX (str "BUG!!!!!!!!!! " message) true nil))        ;; MARKES UNANTICIPATED WEIRD STATES WHICH INDICATE MISTAKES IN THE CODE.

(§ defq- GLOBAL-ERR [message]
    (ERTEX (SETQ GLOBAL-MESSAGE message) true nil))        ;; MARKES KNOWN INADEQUACIES OF THE SYSTEM.  SWITCHABLE STOP, CAUSES ABORTION.

(§ defn- ERTEX [message cause-abortion ignore-nostop-switch?]
    (let [GLOP nil EXP nil ST-BUFFER nil BUILDING-ST-FORM nil FIRSTWORD nil]
        (and NOSTOP
            (not IGNORE-NOSTOP-SWITCH?)
            (and cause-abortion
                (THROW cause-abortion ABORT-PARSER))
            (RETURN true))
        (TERPRI)
        (dorun (map #'PRINT3 message))
    PRINT
        (SETQ FIRSTWORD true ST-BUFFER nil BUILDING-ST-FORM nil)   ;; "ST" REFERS TO SHOW, TELL
        (PRINT '>>>)
    LISTEN
        (COND                                                   ;; SHELP UP SPURIOUS CHARACTERS
            ((memq (TYIPEEK) '(32 10))                        ;; SP, LF
                (READCH)
                (GO LISTEN))
            ((== (TYIPEEK) 13)                                  ;; CR
                (COND (BUILDING-ST-FORM
                        (SETQ EXP (reverse ST-BUFFER))
                        (GO EVAL-EXP))                          ;; DELIMITER CASE
                    (:else (READCH) (GO LISTEN)))))                 ;; SPURIOUS CHARACTER CASE

        (or (ERRSET (SETQ GLOP (READ))) (GO PRINT))

        (COND ((term? GLOP)
            (SETQ GLOP (or (get GLOP 'ABBREV) GLOP))
            (COND ((memq GLOP '(true P nil))                     ;; LEAVE-LOOP CHARS
                    (RETURN GLOP))
                ((= GLOP 'GO)                                  ;; CAUSE RETURN TO READY-STATE
                    (THROW 'GO ABORT-PARSER))
                (BUILDING-ST-FORM
                    (SETQ ST-BUFFER (cons GLOP ST-BUFFER))
                    (GO LISTEN))
                ((and FIRSTWORD (memq GLOP '(SHOW TELL)))
                    (SETQ BUILDING-ST-FORM true
                        ST-BUFFER (cons GLOP ST-BUFFER)
                        FIRSTWORD nil)
                    (GO LISTEN))
                (:else (SETQ EXP GLOP) (GO EVAL-EXP))))
            (:else (COND ((= (car GLOP) 'RETURN)
                    (RETURN (eval (cadr GLOP))))
                (:else (SETQ EXP GLOP) (GO EVAL-EXP)))))

    EVAL-EXP
        (COND (ERT-ERRSET? (ERRSET (PRINT (eval EXP))))
            (:else (PRINT (eval EXP))))
        (GO PRINT)))

(§ defq- COMBINATION? [words]
    ;; THIS FUNCTION CHECKS TO SEE IF THE WORDS PASSED AS ARGS FORM
    ;; A COMBINATION SUCH AS "STACK-UP" OR "ON-TOP-OF" COMBINATIONS
    ;; ARE IN THE DICTIONARY AS A SINGLE ATOM COMPOSED OF THE WORDS
    ;; IN THE COMBINATION SEPARATED BY DASHES ALL COMBINATIONS HAVE
    ;; THE FEATURE "COMBINATION" AND HAVE A ROOT WHICH IS A LIST OF
    ;; THE WORDS IN THE COMBINATION
    (let [COMBINE nil]
        (dorun (map #'(lambda [x]
            (SETQ COMBINE (concat COMBINE (cons '- (EXPLODE (eval x))))))
            words))
        (SETQ COMBINE (list (INTERN (MAKNAM (cdr COMBINE)))))
        (and (ISQ COMBINE COMBINATION) (RETURN COMBINE))
        nil))

(§ defn- FINDB [x y]
    (COND ((nil? x) nil) ((= y (cdr x)) x) (:else (FINDB (cdr x) y))))

(§ defn- FROM [a b]
    (COND ((or (not a) (= a b)) nil) (:else (cons (WORD a) (FROM (cdr a) b)))))

(§ defn- MAKESYM [a]
    ;; FUNCTION MAKESYM MAKES UP A GENSYM OF ITS ARG
    (PUTPROP a (inc (or (get a 'MAKESYM) 0)) 'MAKESYM)
    (SETQ a (MAKNAM (concat (or (get a 'EXPLO) (PUTPROP a (EXPLODE a) 'EXPLO)) (EXPLODE (get a 'MAKESYM)))))
    (INTERN a))

(defn- lis2fy [x]
    (cond (term? x) (list (list x)) (term? (car x)) (list x) :else x))

(§ defn- MEET [a b]
    ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
    (let [s nil]
    =>  (COND ((nil? a) (RETURN (reverse s)))
            ((memq (car a) b)
                (SETQ s (cons (car a) s))))
        (SETQ a (cdr a))
        (GO =>)))

(§ defn- MOD [a b] (UNION (SETDIF a (cadr b)) (car b)))

(§ defn- NTH [num l]
    (COND ((term? l) (ert "NTH - ILLEGAL LIST"))
        ((< num 1) (ert "NTH - ILLEGAL NUMBER")))
    (do
    =>  (COND ((== num 1) (RETURN (car l)))
            ((SETQ l (cdr l))
                (SETQ num (dec num))
                (GO =>))
            (:else (ert "NTH - LIST TOO SHORT")))
    nil))

(§ defn- PR1 [a]
    (COND ((term? (H a)) (list (WORD (NB a)) (FE a)))
        ((PR2 (SM a))
            (list (FROM (NB a) (N a)) (FE a) (SM a) (COND ((term? (H a)) \space) ((MAPLIST #'PR1 (reverse (H a)))))))))

(§ defn- PR2 [a]
    (or (term? a)
        (dorun (map #'(lambda [b]
            (and (get b 'SM)
                (or (memq b ALIST)
                    (SETQ ALIST (cons (list b (get b 'SM) (get b 'REFER)) ALIST)))))
            a))))

(§ defn- PRINT2 [x]
    (COND ((> CHRCT (FLATSIZE x)) (PRINC \space))
        (:else (TERPRI)))
    (PRINC x))

(§ defn- PRINT3 [x]
    (PROG2 (or (> CHRCT (FLATSIZE x)) (TERPRI)) (PRINC x) (PRINC \space)))

(§ defn- PRINTEXT [text]
    (COND (text
        (TERPRI)
        (eval (cons 'SAY (listify text))))))

(§ defq- PRINTC [l]
    (TERPRI)
=>  (COND ((nil? l) (RETURN nil)))
    (let [test (eval (car l))]
        (COND ((= test '<TAB>))
            (:else (PRINC test) (PRINC \space))))
    (SETQ l (cdr l))
    (GO =>))

(defn- quotify [x] (list 'quote x))

(§ defq- SAY [a] (dorun (map #'PRINT3 a)))

(§ defn- SETDIF [a b]
    (let [dif nil]
    =>  (COND ((nil? a) (RETURN (reverse dif)))
            ((memq (car a) b))
            ((SETQ dif (cons (car a) dif))))
        (SETQ a (cdr a))
        (GO =>)))

(§ defn- STA [a b]
=>  (COND ((nil? b) (RETURN true))
        ((nil? a))
        ((= (car a) (car b))
            (SETQ a (cdr a))
            (SETQ b (cdr b))
            (GO =>)))
    nil)

(§ defn- UNION [a b]
    (let [a (reverse a)]
    =>  (COND ((nil? b) (RETURN (reverse a)))
            ((memq (car b) a))
            ((SETQ a (cons (car b) a))))
        (SETQ b (cdr b))
        (GO =>)))

(§ defn- WALLP [a]
    (let [ALIST nil LINEL 72]
        (TYO 12)
        (TERPRI)
        (SPRINT (list (PR1 a) (reverse ALIST)) LINEL 0)
        nil))

(§ defn- TAB [n]
    (let [P nil]
        (COND ((> n LINEL) (RETURN '<TAB>)))
    =>  (SETQ P (- LINEL CHRCT))
        (COND ((not (> n P)) (RETURN '<TAB>)))
        (PRINC \space)
        (GO =>)))

#_(ns shrdlu.morpho)

;; ################################################################################
;;
;;                    MORPHO - CODE FOR MORPHOLOGICAL ANALYSIS
;;
;;                INCLUDES ETAOIN, THE INPUT HANDLER FOR THE SYSTEM
;;
;; ################################################################################

(def- CARRET (ascii 13))

(def- FINAL '(\. ? !))

(def- PUNCL '(\. ? ! \: \; \"))

(def- VOWEL '(nil A E I O U Y))

(def- CONSO '(B C D F G H J K L M N P Q R S T V W X Z))

(def- LIQUID '(L R S Z V))

(def- RUBOUT (ascii 127))

(defn- uppercase-ify-char [c] (if (< 96 c 123) (- c 32) c))

(§ defn- ETAOIN []
    (let [WORD nil NEWWORD nil CHAR nil ALTN nil ALREADY-BLGING-NEWWRD nil WRD nil LAST nil NEXT nil Y nil WORD1 nil x nil RD nil POSS nil]
    THRU (SETQ SENT (SETQ WORD (SETQ PUNCT (SETQ POSS nil))))
        (PRINT 'READY)
        (TERPRI)
    CHAR (COND ((== (TYIPEEK) 24) (READCH) (ert nil) (GO THRU)) ;; "CNTRL-X" BREAK LEFT OVER FROM CMU
            ((== (TYIPEEK) 3) (bug "ETAOIN: ABOUT TO READ EOF")))
        ;; THIS LITTLE HACK MAPS ALL LOWERCASE LETTERS INTO UPPERCASE.
        (SETQ CHAR (COND ((> 123 (SETQ CHAR (TYI)) 96) (- CHAR 32)) ((> 91 CHAR 64) CHAR) (:else CHAR))
            CHAR (ascii CHAR))
        (COND ((= CHAR \space) (GO WORD))                     ;; DELIMITER
            ((memq CHAR ALTMODE)
                (SETQ CHAR (ascii (uppercase-ify-char (TYI))))
                (COND ((memq CHAR ALTMODE) (ert nil) (GO THRU)) ;; ALTMODE-ALTMODE
                    ((= CHAR 'C) (TYO 12) (GO DO))         ;; ALTMODE-C
                    ((= CHAR 'R) (TERPRI) (GO DO))         ;; ALTMODE-R
                    ((and (= CHAR 'S) SAVESENT)            ;; ALTMODE-S CAUSES THE LAST SENTENCE TYPED IN TO
                        (SETQ SENT (car SAVESENT))          ;; RETURNED AS THE SENTENCE TO BE INTERPRETED
                        (SETQ PUNCT (cdr SAVESENT))
                        (%SENT)
                        (RETURN SENT))
                    ((= CHAR 'N)                           ;; ALTMODE-N COMPLEMENTS THE NEWWORD FLAG, WHICH
                                                            ;; DETERMINES WHETHER UNRECOGNIZED WORDS WILL BE
                                                            ;; CONSIDERED SPELLING ERRORS OR NEW WORDS.
                        (SETQ NEWWORD (not NEWWORD) ALTN (not ALTN))
                        (GO CHAR))
                    ((GO THRU))))
            ((= CHAR RUBOUT)
                (COND (WORD (PRINC (car WORD)) (SETQ WORD (cdr WORD)))
                    (SENT (PRINT (car SENT)) (SETQ SENT (cdr SENT))))
                (GO CHAR))
            ((= CHAR CARRET) (GO WORD))
            ((memq CHAR PUNCL)
                (SETQ PUNCT CHAR)                           ;; DELIMITER
                (and WORD (GO WORD))
                (GO PUNC)))
        (and
            (or (and (= CHAR '\") (not ALREADY-BLGING-NEWRD) (SETQ NEWWORD (SETQ ALREADY-BLGING-NEWRD true)) (GO CHAR)) ;; "sic!
                (and (= CHAR '\") ALREADY-BLGING-NEWRD (not (SETQ ALREADY-BLGING-NEWRD nil)) (GO WORD)) ;; "sic!
                                                            ;; WITHIN THIS "AND" ARE ALL THE CHARACTERS THAT ARE UNDERSTOOD BY THE SYSTEM
                (number? CHAR)
                (and (= CHAR '=) (nil? WORD))
                (memq CHAR VOWEL)
                (memq CHAR CONSO))
            (SETQ WORD (cons CHAR WORD)))
        (GO CHAR)

    DO  (PRINT 'READY)
        (TERPRI)
        (dorun (map #'(lambda [x] (PRINT2 x)) (reverse SENT)))
        (PRINC \space)
        (dorun (map #'PRINC (reverse WORD)))
        (GO CHAR)

    WORD (COND ((nil? WORD) (GO CHAR))
            ((= WORD '(P L E H)) (HELP) (GO THRU))
            ((and (SETQ WRD (ERRSET (READLIST (reverse WORD)))) (number? (SETQ WRD (car WRD))))
                (SETQ SENT (cons WRD SENT))
                (BUILDWORD WRD (or (and (zero? (dec WRD)) '(NUM NS)) '(NUM)) (list 'NUM WRD) nil))
                                                            ;; NO ROOT FOR NUMBERS
            ((nil? WRD) (SETQ WRD (reverse WORD)) (GO NO))
            ((get WRD 'FEATURES))                           ;; IF A WORD HAS FEATURES, IT'S PROPERTIES ARE ALL SET UP IN THE DICTIONARY
            ((SETQ x (get WRD 'IRREGULAR))
                (BUILDWORD WRD (MOD (get (car x) 'FEATURES) (cdr x)) (SM x) (car x)))
            ((= (car (LAST WORD)) '=)
                (BUILDWORD WRD (COND ((memq '\" WORD) '(PROPN NS POSS)) ('(PROPN NS))) '((PROPN T)) nil)) ;; "sic!
            ((GO CUT)))
        (GO WRD)

        ;; --------------------------------------------
        ;;               MORPHOLOGY CODE
        ;; --------------------------------------------

    CUT (COND
            ((STA WORD '(T \" N)) (SETQ RD (cdddr WORD)) (SETQ WORD (cons '* WORD)) (GO TRY)) ;; "sic!
            ((STA WORD '(S \")) (SETQ WORD (cddr WORD)) (SETQ POSS WRD) (GO WORD)) ;; "sic!
            ((STA WORD '(\")) (SETQ WORD (cdr WORD)) (SETQ POSS WRD) (GO WORD)) ;; "sic!
            ((STA WORD '(Y L)) (SETQ RD (cddr WORD)) (GO LY))
            ((STA WORD '(G N I)) (SETQ RD (cdddr WORD)))
            ((STA WORD '(D E)) (SETQ RD (cddr WORD)))
            ((STA WORD '(N E)) (SETQ RD (cddr WORD)))
            ((STA WORD '(R E)) (SETQ RD (cddr WORD)))
            ((STA WORD '(T S E)) (SETQ RD (cdddr WORD)))
            ((STA WORD '(S)) (SETQ RD (cdr WORD)) (GO SIB))
            (:else (GO NO)))
        (SETQ LAST (car RD))
        (SETQ NEXT (cadr RD))
        (COND ((and (memq LAST CONSO) (not (memq LAST LIQUID)) (= LAST NEXT)) (SETQ RD (cdr RD)))
            ((= LAST 'I) (SETQ RD (cons 'Y (cdr RD))))
            ((or
                (and (memq LAST CONSO) (memq NEXT VOWEL) (not (= NEXT 'E)) (memq (caddr RD) CONSO))
                (and (memq LAST LIQUID) (memq NEXT CONSO) (not (memq NEXT LIQUID)))
                (and (= LAST 'H) (= NEXT 'T))
                (and (memq LAST '(C G S J V Z)) (or (memq NEXT LIQUID) (and (memq NEXT VOWEL) (memq (caddr RD) VOWEL)))))
                    (SETQ RD (cons 'E RD))))
        (GO TRY)

    LY  (COND ((and (memq (car RD) VOWEL) (not (= (car RD) 'E)) (memq (cadr RD) CONSO))
            (SETQ RD (cons 'E RD))))
        (COND ((memq 'ADJ (get (SETQ ROOT (READLIST (reverse RD))) 'FEATURES))
            (BUILDWORD WRD '(ADV VBAD) nil ROOT) ;; TEMP NIL SEMANTICS ;; ROOT IS THE ADJECTIVE
            (GO WRD)))
        (GO NO)

    SIB (SETQ LAST (car RD))
        (SETQ NEXT (cadr RD))
        (COND ((not (= LAST 'E)))
            ((= NEXT 'I) (SETQ RD (cons 'Y (cddr RD))))
            ((= NEXT 'X) (SETQ RD (cdr RD)))
            ((and (= NEXT 'H) (not (= (caddr RD) 'T))) (SETQ RD (cdr RD)))
            ((and (memq NEXT '(S Z)) (= NEXT (caddr RD))) (SETQ RD (cddr RD))))
    TRY (COND
            ((or (SETQ FEATURES (get (SETQ ROOT (READLIST (reverse RD))) 'FEATURES))
                    (and (SETQ x (get ROOT 'IRREGULAR))
                        (SETQ FEATURES (MOD (get (SETQ ROOT (car x)) 'FEATURES) (cdr x)))))
                (BUILDWORD WRD (MOD FEATURES (get (car WORD) 'MOD)) (get ROOT 'SEMANTICS) ROOT))
            ((= (car RD) 'E) (SETQ RD (cdr RD)) (GO TRY))
            ((GO NO)))

        ;; -------------------------------------------------------
        ;;   BUILD UP THE PROCESSED LIST OF WORDS TO BE RETURNED
        ;; -------------------------------------------------------

    WRD (SETQ SENT
            (COND (POSS
                (COND ((or (memq 'NOUN (SETQ FEATURES (get WRD 'FEATURES))) (memq 'PROPN FEATURES))
                        ;; IF IT'S A NOUN OR A PROPER NOUN, MARK IT AS POSSESSIVE
                        (BUILDWORD POSS (concat (MEET FEATURES (get 'POSS 'ELIM)) '(POSS)) (get WRD 'SEMANTICS) ROOT)
                        (cons POSS SENT))
                    ;; CAN WE GENERALIZE IT???
                    ((BUILDWORD "'S" '(VB BE V3PS PRES) (get 'BE 'SEMANTICS) 'BE) (cons "'S" (cons WRD SENT)))))
                ((cons WRD SENT))))

    PUNC (COND (PUNCT
            (COND ((and (= PUNCT '?) (nil? SENT)) (HELP) (GO THRU))
                ((memq PUNCT FINAL)
                    (RETURN (car (SETQ SAVESENT (cons (reverse SENT) PUNCT))))) ;; RETURN POINT !!!!!!!!!!!!!
                ((SETQ SENT (cons PUNCT SENT))))))

        (SETQ PUNCT nil)
        (SETQ WORD (SETQ POSS nil))
        (GO CHAR)

    NO  (COND (NEWWORD
            (BUILDWORD WRD '(NOUN NS) '((NOUN (SMNEWNOUN)) (PROPN (SMNEWPROPN))) WRD)
            (or ALTN (SETQ NEWWORD nil))
            (GO PUNC)))
        (TERPRI)
        (SAY *SORRY I DON'T KNOW THE WORD \") ;; "sic!
        (PRINC WRD)
        (PRINC "\".") ;; "sic!
        (TERPRI)
        (SAY PLEASE TYPE <LF> AND CONTINUE THE SENTENCE\.)

    NOGO (or (== (TYI) 10) (GO NOGO))
        (SETQ PUNCT nil WORD nil)
        (GO DO)))

(§ defn- PROPNAME [x] (= (car (EXPLODE x)) '=))

(§ defn- BUILDWORD [word features semantics root]
    (PUTPROP word features 'FEATURES)
    (PUTPROP word (or SMN semantics) 'SEMANTICS)
    (and root (PUTPROP word root 'ROOT))
    word)

(§ defq- BUILDWORDLIST [a]
    (dorun (map #'(lambda [x]
        (PRINT (BUILDWORD (car x)
            (cadr x)
            (caddr x)
            (and (cdddr x) (cadddr x)))))
        a)))

(§ defn- UNDEFINED [] (PRINC (WORD N)) (ert "UNDEFINED"))

#_(ns shrdlu.show)

(§ defq- LABELTRACE [a]
    (dorun (map #'(lambda [x]
        (let [BODY nil]
            (PRINT x)
            (COND ((get x 'LABELTRACED) (PRINC 'ALREADY-) (GO TRACED))
                ((get x 'INTERPRET) (SETQ BODY (cdr (get x 'INTERPRET))))
                ((get x 'EXPR) (SETQ BODY (cddr (caddr (get x 'EXPR)))))
                (:else (PRINC "CAN'T BE-") (GO TRACED)))
            (MAP #'(lambda [y]
                    (and (term? (car y))
                        (RPLACD y (cons (list 'PASSING (list 'quote (car y))) (cdr y)))))
                BODY)
            (PUTPROP x true 'LABELTRACED)
        TRACED
            (PRINC 'LABELTRACED)
            nil))
        a)))

(§ defn- PASSING [a]
    (SETQ LASTLABEL a)
    (and (COND
            ((term? LABELTRACE) (and LABELTRACE (PRINT 'PASSING) (PRINC a)))
            ((memq a LABELTRACE) (PRINT 'PASSING) (PRINC a)))
        (COND
            ((term? LABELBREAK) (and LABELBREAK (ert "LABELBREAK")))
            ((memq a LABELBREAK) (ert "LABELBREAK")))))

(§ SETQ LABELTRACE nil)

(§ SETQ LABELBREAK nil)

(§ defq- UNLABELTRACE [a]
    (dorun (map #'(lambda [x]
        (let [BODY nil]
            (PRINT x)
            (COND
                ((not (get x 'LABELTRACED)) (PRINC "ISN'T ALREADY-") (GO TRACED))
                ((get x 'INTERPRET) (SETQ BODY (cdr (get x 'INTERPRET))))
                ((get x 'EXPR) (SETQ BODY (cddr (caddr (get x 'EXPR)))))
                (:else (PRINC "CAN'T BE-") (GO TRACED)))
            (MAP #'(lambda [y] (and (term? (car y)) (RPLACD y (cddr y)))) BODY)
            (PUTPROP x nil 'LABELTRACED)
            (PRINC 'UN)
        TRACED
            (PRINC 'LABELTRACED)
            nil))
        a)))

(§ defq- DEFS [l]
    (let [a nil]
        (and (nil? (cdr l)) (RETURN l))
        (SETQ a (car l))
        (SETQ l (cdr l))
    =>  (PUTPROP a (cadr l) (car l))
        (COND ((SETQ l (cddr l)) (GO =>)))
        a))

(§ DEFS TELLABLE
    TELL #'(lambda [x] (APPLY #'TELLABLE
            (list (CHARG x "CONCEPT:"
                '(ANY PLANNER GOAL PATTERN BEGINNING WITH THIS CONCEPT NAME CAN BE ACCEPTED BY THE SYSTEM AS NEW INFORMATION
                -- BEWARE OF INTERACTIONS WITH SPECIAL HACKS FOR LOCATION\, ETC\.))))))

(§ defn- PEV [ev col top]
    (TERPRI)
    (TAB col)
    (PRINC ev)
    (PRINC "  ")
    (PRINC (get ev 'TYPE))
    (PRINC "  TIME: ")
    (PRINC (get ev 'START))
    (PRINC " TO ")
    (PRINC (get ev 'END))
    (and top
        (PRINC " REASON: ")
        (PRINC (get ev 'WHY)))
    (dorun (map #'(lambda [x] (and (= ev (get x 'WHY)) (PEV x (+ col 8) nil)))
        (reverse EVENTLIST))))

(§ DEFS EVENT
    SHOW (lambda [x]
            (SETQ x (CHARG x "EVENT:" '(EVENT TO BE DISPLAYED --<LF> FOR ENTIRE EVENT LIST)))
            (COND (x (PEV x 0 true))
                (:else (dorun (map #'(lambda [y] (and (= 'COMMAND (get y 'WHY)) (PEV y 0 true)))
                    (reverse EVENTLIST)))))))

(§ defq- ABBREVIATE [a]
    (dorun (map #'(lambda [x]
        (PUTPROP (READLIST (doall (map #'(lambda [x y] x) (EXPLODE x) '(true true)))) x 'ABBREV))
        a))
    'DONE)

(§ ABBREVIATE
    SHOW TELL LISP PLANNER PARSING DEFINITIONS SCENE INPUT RUN SEMANTICS PROPERTY FUNCTION VALUE ASSERTIONS THEOREM SCENE ACTION NODE TREE LABEL ATTEMPT UNIT WORD MARKER ALL REST CURRENT STOP DO)

(§ defn- SHOWSCENE [x]
    (let [PLANNERSEE nil]
        (TERPRI)
        (TAB 16)
        (PRINC "CURRENT SCENE")
        (TERPRI)
        (TERPRI)
        (dorun (map #'(lambda [OBJ]
            (PRINT OBJ)
            (PRINC "-->  ")
            (evlis (car (NAMEOBJ OBJ 'DESCRIBE)))
            (PRINC " AT ")
            (PRINC (cadr (assq OBJ ATABLE)))
            (and (SETQ OBJ (THVAL '(THFIND ALL ($? x) (x) (THGOAL (!SUPPORT ($? OBJ) ($? x)))) (list (list 'OBJ OBJ))))
                    (TAB 13)
                    (PRINC "SUPPORTS ")
                    (PRINC OBJ)))
                '(ßB1 ßB2 ßB3 ßB4 ßB5 ßB6 ßB7 ßB10 ßBOX)))
        (TERPRI)
        (SAY THE HAND IS GRASPING)
        (PRINC \space)
        (PRINC (COND
            ((SETQ OBJ (THVAL '(THGOAL (!GRASPING ($_ x))) '((x THUNBOUND)))) (cadar OBJ))
            (:else 'NOTHING)))
        nil))

(§ defn- TELLCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE))

(§ defn- SHOWCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE))

(§ defn- SHOWTELL [a node SYSTEMS INFO ACTION]
    (COND ((nil? a) (SHOWTELLCHOICE))
        ((get (car a) ACTION)
            (APPLY (get (car a) ACTION) (list a)))
        ((PRINTEXT '(I DON'T KNOW HOW TO))
            (PRINT2 ACTION)
            (PRINT2 (car a))))
    '*)

(§ defn- SHOWTELLCHOICE []
    (APPLY (get (SETQ NODE (QUERY '(WHICH OPTION?) (PRINT (get NODE SYSTEMS)) (get NODE INFO))) ACTION) (list (list NODE))))

(§ defn- QUERY [text choices help]
    (let [EXPL nil CH2 nil EX2 nil CH3 nil EX3 nil CHAR nil NOTINIT nil]
        (SETQ EXPL (doall (map #'EXPLODE (cons 'QUIT choices))))
    TOP (SETQ CH2 (cons 'QUIT choices) EX2 EXPL)
        (PRINTEXT text)
    READ (COND ((memq (SETQ CHAR (READCH)) BREAKCHARS)
            (COND ((not NOTINIT) (GO READ))
                ((cdr CH2) (TYO 7) (GO READ))
                (:else (dorun (map #'PRINC (car EX2)))
                    (and (= (car CH2) 'QUIT)
                    (ERR nil))
                    (RETURN (car CH2)))))
            ((= CHAR (ascii 10)) (GO READ))
            ((= CHAR '?) (PRINTEXT help) (GO choices)))
        (SETQ CH3 nil EX3 nil)
        (dorun (map #'(lambda [x y]
                (and (= CHAR (car x))
                    (SETQ CH3 (cons y CH3))
                    (SETQ EX3 (cons (cdr x) EX3))))
            EX2 CH2))
        (and CH3
            (SETQ EX2 EX3 CH2 CH3)
            (SETQ NOTINIT true)
            (GO READ))
    GO  (or (memq (READCH) BREAKCHARS) (GO GO))
    choices
        (PRINTEXT '(THE choices "ARE:"))
        (PRINT choices)
        (GO TOP)))

(§ defn- REQUEST [text help]
    (let [x nil]
    TOP (PRINTEXT text)
    READ (COND
            ((memq (ascii (TYIPEEK)) BREAKCHARS) (READCH) (GO READ))
            ((== (TYIPEEK) 10) (READCH) (RETURN nil))
            ((= (ascii (TYIPEEK)) '?) (READCH) (PRINTEXT (or help '(NO INFORMATION AVAILABLE))) (GO TOP))
            ((= (SETQ x (READ)) 'QUIT) (ERR nil))
            (:else (RETURN x)))
    nil))

(§ defn- SHOWPROP [x]
    (COND ((nil? x)
            (SHOWPROP (cons
                (REQUEST "ATOM:"
                    '(THE NAME OF THE ATOM WHOSE PROPERTY (IES) YOU WANT TO EXAMINE))
                (listify (REQUEST "PROPERTY:"
                    '(THE PROPERTY (IES) YOU WANT TO SEE\.  A LINE FEED MEANS ALL PROPERTIES OF THE ATOM))))))
        ((cdr x) (APPLY #'DISP x))
        (:else (DP (car x)) nil)))

(§ defq- TELL [a]
    (SHOWTELL a 'CANTELL 'TELLTREE 'TELLINFO 'TELL))

(§ defn- TREEPRINT [root tr col]
    (TERPRI)
    (TAB col)
    (PRINC root)
    (dorun (map #'(lambda [x] (TREEPRINT x tr (+ col 8)))
        (get root tr)))
    '*)

(§ defn- CHARG [x text help]
    (COND ((cdr x) (cadr x)) (:else (REQUEST text help))))

(§ defq- SHOW [a]
    (SHOWTELL a 'CANSHOW 'SHOWTREE 'SHOWINFO 'SHOW))

(§ DEFS CANSHOW
    SHOWTREE (SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT)
    SHOWINFO (THINGS WHICH CAN BE DISPLAYED)
    SHOW SHOWCHOICE)

(§ DEFS CANTELL
    TELLTREE (LISP PLANNER PARSING DEFINITIONS SEMANTICS)
    TELLINFO (THINGS WHICH CAN BE SET TO CONTROL HOW THE SYSTEM RUNS)
    TELL TELLCHOICE)

(§ DEFS SHOW
    SHOW (lambda [x] (TREEPRINT 'CANSHOW 'SHOWTREE 0)))

(§ DEFS TELL
    SHOW (lambda [x] (TREEPRINT 'CANTELL 'TELLTREE 0)))

(§ DEFS LISP
    SHOWTREE (PROPERTY FUNCTION VALUE)
    TELL TELLCHOICE
    TELLTREE (FUNCTION)
    SHOW SHOWCHOICE)

(§ DEFS DO
    TELL #'(lambda [x] (PRINTEXT '(NOT YET DEFINED))))

(§ DEFS STOP
    TELL (lambda [x] (SETQ PLANNERSEE (and PLANNERSEE (COND ((ONOFF x '(STOP AFTER SHOWING PLANNER INPUT?)) true) ('NOSTOP))))))

(§ DEFS PLANNER
    SHOWTREE (ASSERTIONS THEOREM SCENE EVENT)
    SHOW SHOWCHOICE
    TELLTREE (INPUT ACTION THEOREM ASSERTIONS TELLABLE)
    TELL (lambda [x]
            (COND ((nil? (cdr x)) (TELLCHOICE x))
                ((= (cadr x) 'ON)
                    (TYO 12)
                    (THTRACE THEOREM THASSERT THERASE (THGOAL true true))
                    (SETQ PLANNERSEE true))
                ((= (cadr x) 'OFF)
                    (TYO 12)
                    (SETQ PLANNERSEE nil)
                    (THUNTRACE))
                (:else (TELLCHOICE x)))))

(§ DEFS PARSING
    SHOWTREE (NODE TREE)
    SHOW SHOWCHOICE
    TELLTREE (NODE LABEL ATTEMPT)
    TELL (lambda [x]
            (COND ((nil? (cdr x)) (TELLCHOICE x))
                ((= (cadr x) 'ON)
                    (TYO 12)
                    (SETQ LABELTRACE true)
                    (TRACE CALLSM PARSE))
                ((= (cadr x) 'OFF)
                    (TYO 12)
                    (SETQ LABELTRACE nil)
                    (UNTRACE CALLSM PARSE))
                (:else (TELLCHOICE x)))))

(§ DEFS DEFINITIONS
    SHOWTREE (UNIT WORD MARKER)
    SHOW SHOWCHOICE
    TELL TELLCHOICE
    TELLTREE (WORD MARKER))

(§ DEFS INPUT
    TELL (lambda [x] (SETQ PLANNERSEE (ONOFF x '(TO SEE INPUT TO PLANNER))))
    SHOW SHOWCHOICE
    SHOWTREE (ALL REST CURRENT))

(§ DEFS SEMANTICS
    TELL (lambda [x]
            (SETQ SMN nil)
            (COND ((= (QUERY '(DO SEMANTIC ANALYSIS?) '(YES NO) nil) 'NO)
                    (SETQ SMN true)))))

(§ DEFS RUN
    TELLTREE (STOP DO)
    TELL TELLCHOICE
    TELLINFO '(PARAMETERS TO CONTROL WHAT SHRDLU DOES AS IT RUNS))

(§ DEFS PROPERTY
    SHOW (lambda [x] (SHOWPROP (cdr x))))

(§ DEFS VALUE
    SHOW (lambda [x] (DISP (eval (CHARG x "EXPRESSION:" '(EXPRESSION TO BE EVALUATED BY THE LISP INTERPRETER))))))

(§ DEFS FUNCTION
    TELL (lambda [x] (SETQ x (list (CHARG x "FUNCTION:" '(LISP FUNCTION WHOSE ACTION IS TO BE TRACED))
                (COND ((and (cdr x) (cddr x) (memq (caddr x) '(TRACE BREAK UNTRACE UNBREAK))) (caddr x))
                    (:else (QUERY '(TRACE BREAK UNTRACE OR UNBREAK?)
                        '(TRACE BREAK UNTRACE UNBREAK)
                        '(TRACE CAUSES PRINTOUT ON ENTRY AND EXIT OF FUNCTION\.
                            BREAK CAUSES LISP TO STOP ON ENTRY AND EXIT\,
                            ACCEPTING USER COMMANDS AND CONTINUING WHEN <CONTROL X> IS TYPED\.))))))
            (APPLY (SUBST 'WBREAK 'BREAK (cadr x)) (list (car x))))
    SHOW (lambda [x] (APPLY #'GB (list (CHARG x "FUNCTION:" '(LISP FUNCTION WHOSE LISP DEFINITION IS TO BE SHOWN))))))

(§ DEFS ASSERTIONS
    TELL (lambda [x] (THVAL (list 'THASSERT (CHARG x "ASSERTION:" '(PLANNER ASSERTION TO BE ADDED TO DATA BASE)) '(THTBF THTRUE)) nil))
    SHOW (lambda [x] (DA (CHARG x "ATOM:" '(SHOW ALL ASSERTIONS WHICH CONTAIN THE GIVEN ATOM)))))

(§ DEFS THEOREM
    TELL DEFINETHEOREM
    SHOW (lambda [x] (DISP (get (CHARG x "THEOREM-NAME:" '(PLANNER THEOREM WHOSE DEFINITION IS TO BE SHOWN)) 'THEOREM))))

(§ DEFS NODE
    SHOW (lambda [x]
            (COND ((get (cadr x) 'FEATURES) (DP (cadr x)))
                ((SHOWMOVE (cdr x)) (DP (car PT)) (RESTOREPT))
                (:else (SAY NO SUCH NODE)))))

(§ DEFS TREE
    SHOW (lambda [x]
            (COND ((get (cadr x) 'FEATURES) (WALLP (list (cadr x))))
                ((SHOWMOVE (cdr x)) (WALLP PT) (RESTOREPT))
                (:else (SAY NO SUCH TREE)))))

(§ DEFS UNIT
    SHOW (lambda [x] (APPLY #'DG (or (cdr x) (list (REQUEST "UNIT:" '(GRAMMAR UNIT WHOSE PROGRAM IS TO BE EXAMINED -- "E.G."  CLAUSE NG PREPG VG ADJG)))))))

(§ DEFS WORD
    SHOW (lambda [x] (DP (CHARG x "WORD:" '(ENGLISH WORD IN THE VOCABULARY))))
    TELL (lambda [x] (APPLY #'DEFINE (list (CHARG x "WORD:" '(ENGLISH WORD TO BE DEFINED -- MUST BE NOUN OR VERB))))))

(§ DEFS ACTION
    TELL (lambda [x]
            (COND ((cdr x)
                    (COND ((= (cadr x) 'ON) (SETQ x nil))
                        ((= x 'OFF) (SETQ x '(THUNTRACE)))))
                ((ONOFF x '(WATCH PLANNER PROGRAMS STEP BY STEP?))
                    (SETQ x nil))
                (:else (SETQ x '(THUNTRACE))))
            (COND (x (THUNTRACE))
                (:else (APPLY #'THTRACE '(THEOREM THGOAL THASSERT THERASE))))))

(§ DEFS LABEL
    TELL (lambda [x]
            (or (cdr x)
                (SETQ x (list (REQUEST '(TYPE LIST OF LABELS\, OR ON OR "OFF:") '(WATCHES PARSER GO PAST PROGRAM LABELS IN THE GRAMMAR)))))
            (SETQ LABELTRACE (COND ((= (car x) 'OFF) nil) (:else (car x))))))

(§ DEFS ATTEMPT
    TELL (lambda [x] (COND
            ((ONOFF x '(TO SEE ALL ATTEMPTS TO PARSE SYNTACTIC UNITS\, INCLUDING FAILURES))
                (TRACE PARSE)
                (TRACE CALLSM))
            (:else (UNTRACE PARSE)))))

(§ defn- SHOWMOVE [x]
    (SETQ SAVEPT PT)
    (APPLY #'MOVE-PT
        (listify (or x
            (REQUEST "NODE-SPECIFICATION:"
                '(C MEANS CURRENT NODE -- H IS MOST RECENTLY PARSED FOR OTHER POSSIBILITIES\, SEE THESIS SECTION ON POINTER-MOVING COMMANDS))))))

(§ defn- ONOFF [arg help]
    (COND
        ((= (cadr arg) 'ON) true)
        ((= (cadr arg) 'OFF) nil)
        ((= 'ON (QUERY '(ON OR OFF?) '(ON OFF) help)))))

(§ defn- DEFINETHEOREM [x]
    (PUTPROP (COND ((cdr x) (SETQ x (cadr x))) (:else (SETQ x (MAKESYM 'THEOREM))))
        (concat (list (QUERY '(WHICH THEOREM TYPE?) '(THANTE THERASING THCONSE) '(ANTECEDENT\, ERASING\, OR CONSEQUENT THEOREM))
                    (listify (REQUEST "VARIABLE-LIST:" nil))
                    (REQUEST "PATTERN:" '(A LIST ENCLOSED IN PARENS\, LIKE (!IS ($? x) !ZOG)))
                    (REQUEST "BODY:" '(LIST OF MICROPLANNER STATEMENTS))))
        'THEOREM)
    (THADD x nil)
    (PRINT x))

(§ DEFS MARKER
    TELL (lambda [x]
            (let [y nil]
                (PUTPROP (SETQ x (CHARG x "MARKER:" '(MARKER TO BE ADDED)))
                    (list (SETQ y (REQUEST "PARENT:" '(NODE TO WHICH IT IS ATTACHED IN THE TREE))))
                    'SYS)
                (PUTPROP y
                    (cons x (get y 'SYSTEM))
                    'SYSTEM)
                nil))
    SHOW (lambda [x]
            (TREEPRINT (or (CHARG x "MARKER:" '(SEMANTIC MARKER WHOSE SUBSETS ARE TO BE EXAMINED\. TYPE <LF> FOR ENTIRE TREE\.)) '!SYSTEMS)
                'SYSTEM
                0)))

(§ DEFS ALL SHOW (lambda [x] (%SENT)))

(§ DEFS CURRENT SHOW (lambda [x] (PRINTEXT (FROM NB N))))

(§ DEFS REST SHOW (lambda [x] (PRINTEXT N)))

(§ DEFS SCENE SHOW SHOWSCENE)

(§ defq- DEFINE [a]
    (let [FE nil TYPE nil MARK nil REST nil TR nil]
        (SETQ a (COND (a (car a)) (:else (REQUEST "WORD:" '(ENGLISH WORD TO BE DEFINED)))))
        (SETQ TYPE (QUERY '(NOUN OR VERB?) '(NOUN VERB) '(OTHER TYPES MUST BE DEFINED IN LISP)))
    =>  (or (SETQ MARK (REQUEST 'MARKERSß '(LIST OF SEMANTIC MARKERS FOR WORD BEING DEFINED - TO SEE MARKER TREE TYPE <LF>)))
            (and (SHOW MARKER !SYSTEMS) (GO =>)))
        (SETQ MARK (listify MARK))
        (COND
            ((= TYPE 'NOUN)
                (PUTPROP a '(NOUN NS) 'FEATURES)
                (PUTPROP a
                    (list (list 'NOUN (list 'OBJECT (list 'MARKERSß MARK
                        'PROCEDUREß
                        (lis2fy (REQUEST 'PROCEDUREß
                                    '(EXPRESSION OR LIST OF EXPRESSIONS TO BE PUT IN PLANNER GOALS TO DESCRIBE OBJECT - USE *** TO REPRESENT OBJECT BEING DESCRIBED BY WORD
                                        -- "E.G."  (!IS *** !ZOG) OR ((!IS *** !ZOG) (!LOVE ßEVERYONE ***)))))))))
                    'SEMANTICS)
                (RETURN true))
            ((SETQ TR (= (QUERY '(TRANSITIVE OR INTRANSITIVE?) '(TRANSITIVE INTRANSITIVE) nil) 'TRANSITIVE))
                (PUTPROP a '(VB TRANS INF) 'FEATURES))
            (:else (PUTPROP a '(VB ITRNS INF) 'FEATURES)))
        (SETQ REST (list (list (listify (REQUEST '(RESTRICTIONS ON "SUBJECT:") '(LIST OF SEMANTIC MARKERS))))))
        (and TR
            (SETQ REST (concat REST (list (listify (REQUEST '(RESTRICTIONS ON "OBJECT:") '(LIST OF SEMANTIC MARKERS)))))))
        (PUTPROP a
            (list (list 'VB (list 'RELATION (list
                'MARKERSß MARK
                'RESTRICTIONSß REST
                'PROCEDUREß
                    (lis2fy (REQUEST 'PROCEDUREß
                                '(LIST OF EXPRESSIONS TO BE PUT INTO PLANNER GOALS TO DESCRIBE ACTION OR RELATION -- USE !1 FOR SUBJECT\, !2 FOR OBJECT\.
                                    "E.G."  (!SUPPORT !1 !2) OR ((!HAPPY !1) (!SMILING !1)))))))))
            'SEMANTICS)
        true))

(§ defn- HELP []
    (COND ((= 'S (QUERY '(TYPE L FOR LONG FORM (85 LINES) S FOR SHORT (16 LINES)) '(S L) nil))
            (UREAD MINIH DOC DSK LANG))
        (:else (UREAD HELP DOC DSK LANG)))
    (THRUTEXT)
    '*)

#_(ns shrdlu.macros)

;; #############################################################
;;
;;              A PRECOMPILER FOR PROGRAMMAR CODE
;;
;; #############################################################

(§ defq- TOPLEVEL-LIST [l]
    ;; ACTS LIKE LIST EXCEPT THAT IF ANY ELEMEMNT EVALUATES IN TO
    ;; MORE THAN A SINGLE ELEMENT (RETURNS A LIST WHOSE CAR IS
    ;; ALSO A LIST) THEN THE ELEMENTS OF THAT ELEMENT ARE ADDED
    ;; TO THE SAME LEVEL AS THE SEPARATE ELEMENTS IN THE CALL.
    (doall (map #'(lambda [E]
            (SETQ E (eval E))
            (COND ((term? (car E)) (list E)) (:else E)))
        l)))

(§ defq- PDEFINE [moby]
    (list 'DEFUN (car moby) 'nil
        (concat (list 'PROG
            (concat '(FE H ME NB C SM CUT NN T1 T2 T3 ßRESULT) (cadr moby))
            '(SETQ NN true)
            '(SETQ CUT END)
            '(SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
            '(SETR 'PARENT PARENT C))
            (APPLY #'SPREAD (cddr moby))
            (list 'FAIL
                '(SETQ MES ME)
                '(SETQ N (or (N RE) NB))
                '(RETURN nil)
                'RETURN
                '(SETQ MES ME)
                '(RETURN (REBUILD (reverse FE) NB N H SM C))))))

(§ defq- SPREAD [l]
    (doall (map #'(lambda [E]
        (let [PREDICATE nil T1 nil T2 nil T3 nil]
            (COND
                ((term? E)
                    (RETURN (list E (list 'AND 'LABELTRACE (list 'PASSING (list 'quote E))))))
                ((= (car E) 'GOCOND)
                    (RETURN (list (list 'COND (list 'NN (list 'GO (cadr E))) (list ':else (list 'GO (caddr E)))))))
                ((= (car E) '|)
                    (SETQ PREDICATE (cadr E) T1 (caddr E) T2 (cadddr E))
                (and (cddddr E) (SETQ T3 (car (cddddr E))))
                    (RETURN (list
                        (list 'SETQ 'ßRESULT PREDICATE)
                        (COND
                            ((and T1 (nil? T2)) ;; T3 CAN BE EITHER THERE OR NOT
                                (list 'COND (TOPLEVEL-LIST 'ßRESULT
                                    (COND
                                        (T3 (list 'COND (TOPLEVEL-LIST (list 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST ':else (TAG-CHECK T1))))
                                        (:else (TAG-CHECK T1))))))
                            ((and (nil? T1) T2 (nil? T3))
                                (list 'COND (TOPLEVEL-LIST (list 'NULL 'ßRESULT) (TAG-CHECK T2))))
                            ((and (nil? T1) T2 T3)
                                (list 'COND (list (list 'NULL 'ßRESULT) (list 'COND (TOPLEVEL-LIST (list 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST ':else (TAG-CHECK T2))))))
                            ((and T1 T2 (nil? T3))
                                (list 'COND (TOPLEVEL-LIST 'ßRESULT (TAG-CHECK T1)) (TOPLEVEL-LIST ':else (TAG-CHECK T2))))
                            ((and T1 T2 T3)
                                (list 'COND (list 'ßRESULT (list 'COND (TOPLEVEL-LIST (list 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST ':else (TAG-CHECK T1))))
                                            (TOPLEVEL-LIST ':else (TAG-CHECK T2))))
                            ((and (nil? T1) (nil? T2) T3)
                                (list 'COND (TOPLEVEL-LIST (list 'AND (list 'NULL 'NN) 'ßRESULT) (TAG-CHECK T3))))
                            ((and (nil? T1) (nil? T2) (nil? T3))
                                (list 'I-AM-A-TAG))))))
                (:else (RETURN (list E))))
            nil))
        l)))

(§ defn- TAG-CHECK [tag-exp]
    (COND ((term? tag-exp) (list (list 'GO tag-exp)))
        (:else (list (list 'M (car tag-exp)) (list 'GO 'FAIL)))))

#_(ns shrdlu.progmr)

;; ###########################################################
;;
;;                         PROGMR
;;  (INTERPRETER FOR THE PROGRAMMAR GRAMMAR WRITING LANGUAGE)
;;
;; ###########################################################

(§ defn- RESTOREPT [] (SETQ PT SAVEPT))

(§ defn- SETMVB [ptr-mvb]
    (let [save PT]
        (SETQ MVB ptr-mvb)                              ;; IF THERE ARE SEVERAL CLAUSES ACTIVE AT THE
        (SETQ PT ptr-mvb)                               ;; SAME TIME, IT SETS THE NEAREST ONE.
        (SETR 'MVB ptr-mvb (MOVE-PT C U (CLAUSE)))
        (SETQ PT save)
        true))

(§ defn- ADD-F-PT [feature ptr]
    (PUTPROP (car ptr) (cons feature (FE ptr)) 'FEATURES)
    (and (= ptr C) (SETQ FE (FE ptr)))
    true)

(§ defn- REMOVE-F-PT [feature ptr]
    (PUTPROP (car ptr) (SETDIF (get (car ptr) 'FEATURES) (list feature)) 'FEATURES)
    (and (= ptr C) (SETQ FE (FE ptr)))
    true)

(defn- one-word-left [nb] (and (cdr nb) (not (cddr nb))))

(§ SETQ SMNBREAKS nil) ;; A LIST OF SMNFNS WHICH WILL BE BROKEN AT (BEFORE CALLING)

(§ defq- CALLSM [semantic-expression]
    (let [RESULT nil SMNFN (car semantic-expression)]
        (and SMN
            (COND ((or (= SMNBREAKS true) (memq SMNFN SMNBREAKS)) (RETURN (ert nil)))
                (:else (RETURN true))))
        (SETQ RESULT (eval (car semantic-expression)))
        (COND ((or (= SMNBREAKS 'ALL) (memq SMNFN SMNBREAKS)) (ert nil)))
        RESULT))

(§ defq- MOVE-PT [l]
    (let [XX nil YY nil L2 nil EXEC l save PT]
    TEST1 (COND ((and (cdr EXEC) (not (term? (cadr EXEC))))
            (GO TEST)))
    LOOK1 (SETQ XX (car EXEC))
    LOOK (COND
            ((= XX 'H) (or (SETQ PT H) (GO FAIL)) (GO EX))
            ((= XX 'C) (SETQ PT C) (GO EX))
            ((= XX 'PC) (SETQ PT (H (PARENT C))) (GO EX))
            ((= XX 'LASTSENT) (SETQ PT LASTSENT) (GO EX))
            ((= XX 'U) (or (SETQ PT (PARENT PT)) (GO FAIL)))
            ((= XX 'DLC) (or (SETQ PT (H PT)) (GO FAIL)))
            ((= XX 'DF) (SETQ L2 (cons 'DLC (cons 'FR L2))) (SETQ XX 'DLC) (GO LOOK))
            ((= XX 'FR) (COND ((MOVE-PT PV) (GO LOOK))))
            ((= XX 'NX) (or (SETQ PT (PREVIOUS (H (PARENT PT)) (car PT))) (GO FAIL)))
            ((= XX 'PV) (SETQ PT (or (and (= PT C) (H (PARENT C))) (FOLLOWING (H (PARENT PT)) (car PT)) (GO FAIL))))
            (:else (PRINT XX) (ert "MOVE-PT: ILLEGAL INSTRUCTION")))
    EX  (COND ((or (nil? L2) (nil? (SETQ L2 (cdr L2))))
            (GO TEST)))
        (SETQ XX (car L2))
        (GO LOOK)
    TEST (COND ((nil? (cdr EXEC)) (RETURN PT))
            ((term? (cadr EXEC)) true)
            ((COND ((cdadr EXEC) (eval (cadr EXEC)))
                (:else (ISX PT (caadr EXEC))))
            (SETQ EXEC (cdr EXEC)))
            (:else (GO LOOK1)))
        (COND ((SETQ EXEC (cdr EXEC)) (GO TEST1)))
        (RETURN PT)
    FAIL (SETQ PT save)
        nil))

(§ defq- MOVE-PTW [l]
    (let [EXEC l save PTW XX nil]
    TEST1 (COND ((and (cdr EXEC) (not (term? (cadr EXEC))))
            (GO EX)))
    LOOK1 (SETQ XX (car EXEC))
    LOOK (COND ((= XX 'N) (SETQ PTW N))
            ((= XX 'LASTSENT) (SETQ PTW (NB LASTSENT)))
            ((= XX 'FW) (SETQ PTW (NB PT)))
            ((= XX 'AW) (COND ((= PT C) (GO FAIL)) ((SETQ PTW (N PT)) (SETQ XX 'PW) (GO LOOK))))
            ((= XX 'LW) (COND ((= PT C) (GO FAIL)) ((SETQ PTW (N PT)) (SETQ XX 'PW) (GO LOOK))))
            ((= XX 'NW) (COND ((SETQ PTW (cdr PTW))) ((SETQ PTW (FINDB SENT nil)) (GO FAIL))))
            ((= XX 'PW) (COND ((SETQ PTW (FINDB SENT PTW))) ((SETQ PTW SENT) (GO FAIL))))
            ((= XX 'SFW) (SETQ PTW SENT))
            ((= XX 'SLW) (SETQ PTW (FINDB SENT nil)))
            (:else (bug "MOVE-PTW: ILLEGAL INSTRUCTION")))
    EX  (COND ((nil? (cdr EXEC)) (RETURN PTW))
            ((term? (cadr EXEC)) true)
            ((COND ((cdadr EXEC) (eval (cadr EXEC)))
                (:else (ISX PTW (caadr EXEC))))
            (SETQ EXEC (cdr EXEC)))
            (:else (GO LOOK1)))
        (COND ((SETQ EXEC (cdr EXEC)) (GO TEST1)))
        (RETURN PTW)
    FAIL (SETQ PTW save)
        nil))

(defn- apply-grammar [UNIT]
    (if (get UNIT 'INTERPRET) (INTERPRET UNIT) (eval (list UNIT))))

(§ defn- BUILDNODE [features firstword wordafter daughters semantics]
    (let [node (list (MAKESYM 'NODE))]
        (SETR 'FEATURES features node)
        (SETR 'FIRSTWORD firstword node)
        (SETR 'WORDAFTER wordafter node)
        (SETR 'DAUGHTERS daughters node)
        (SETR 'SEMANTICS semantics node)
        node))

(§ defq- CQ [feature] (memq (car feature) FE))

(§ defn- CUT [a]
    (let [b N C nil]
    =>  (COND
            ((= a b) (SETQ CUT a) (SETQ NN (not (= CUT N))) (RETURN true))
            ((= b END) (RETURN nil))
            ((SETQ b (cdr b)) (GO =>))
            ((nil? a) (SETQ CUT nil) (SETQ NN N) (RETURN true)))))

(§ defn- CUT-BACK-ONE [] (MOVE-PTW N PW) (POP) (CUT PTW))

(§ defn- F [a]
    (COND ((memq a FE) true) ((SETR 'FEATURES (SETQ FE (cons a FE)) C))))

(§ defn- FE [node] (GETR 'FEATURES node))

(§ defn- FEATURE? [feature] (memq feature FE))

(§ defn- FESET [node features] (SETR 'FEATURES features node))

(§ defn- FLUSHME []
    ;; IF YOU HAVEN'T REAHED THE CUT, FLUSHES THE NEXT WORD IN THE SENTENCE.  FAILS IF IT REACHES CUT POINT
    (and N NN (SETQ NN (not (= CUT (SETQ N (cdr N)))))))

(§ defn- FOLLOWING [l member]
    ;; GET THE ELEMENT OF LIST FOLLOWING MEMBER
    (and (memq member l) (cdr (memq member l))))

(§ defq- FQ [a]
    (dorun (map #'(lambda [x] (or (memq x FE) (SETQ FE (cons x FE)))) a))
    (SETR 'FEATURES FE C))

(§ defn- GETR [register node]
    ;; THIS FUNCTION RETRIEVES THE CONTENTS OF THE REGISTER ASSOCIATED WITH THE GIVEN NODE
    (get (car node) register))

(§ defn- H [node] (GETR 'DAUGHTERS node))

(§ defq- ISQ [a] (memq (cadr a) (FE (eval (car a)))))

(§ defn- ISX [a b] (memq b (FE a)))

(§ defn- M [a] (SETQ ME (cons a ME)))

(§ defq- MP [a] (SETQ MESP a))

(§ defq- MQ [a] (SETQ ME (cons a ME)))

(§ defn- N [node] (GETR 'WORDAFTER node))

(§ defn- NB [node] (GETR 'FIRSTWORD node))

(§ defq- NEXTW [a] (= (car N) (car a)))

(§ defn- NEXTWORD [] (car N))                ;; RETURN THE NEXT WORD IN THE SENTENCE

(§ defn- NEXTWORD? [a] (= (car N) a))

(§ defq- NQ [a] (memq (car a) (FE N)))

(§ defn- ONLY-ONE-WORD-LEFT [] (and N (not (cdr N))))

(§ defn- PARENT [node] (GETR 'PARENT node))

(§ defq- PARSE [a]
    (COND ((memq (car a) '(NG CLAUSE VG PREPG ADJG)) (PARSE2 a (memq 'TOPLEVEL a))) ((PARSE3 a nil))))

(§ defn- PARSE2 [REST p]
    ;; THIS FUNCTION CALLS THE PROGRAMMAR FUNCTION INDICATED BY THE FIRST MEMBER OF REST - A FEATURE LIST.
    ;; THE PARAMETER P INDICATES WHETHER PARSE2 IS BEING CALLED FROM THE TOPLEVEL.
    ;; IF P IS TRUE, PARSE2 DOES NOT APPEND THE NODE JUST PARSED TO THE PARSING TREE.
    ;; PARSE2 WILL CALL EITHER A COMPILED OR INTERPRETED VERSION OF THE GRAMMAR PROGRAM.
    (let [UNIT (car REST) CREATED-NODE nil END nil PARENT nil RE nil SPECIAL nil nbb N]
        (SETQ LEVEL (inc LEVEL))
        (COND ((= N CUT)
            (SETQ LEVEL (dec LEVEL))
            (RETURN nil)))
        (SETQ END CUT)
        (SETQ NN (not (= N CUT)))
        (SETQ PARENT C)
        (COND ((NQ B-SPECIAL)
            (eval (GETR 'B-SPECIAL N))))
        (COND ((= SPECIAL 'SKIP) (GO SKIP))
            ((= SPECIAL 'DONE) (GO DONE))
            ((= SPECIAL 'LOSE) (SETQ N nbb) (GO LOSE)))
        (COND ((nil? (SETQ RE (apply-grammar UNIT)))    ;; THIS IS WHERE ALL THE WORK HAPPENS.  IF THE PARSE SUCCEEDS,
            (SETQ RE nil)                               ;; IT WILL RETURN THE NODE THAT HAS BEEN BUILT UP
            (SETQ N nbb)                                ;; (SEE THE FUNCTION "INTERPRETATION" IN IN GINTER)
            (GO LOSE)))
    SKIP (COND ((= N CUT))
            ((NQ SPECIAL) (eval (GETR 'SPECIAL N))))
    DONE (or p
            (REBUILD (SETQ FE (get (car C) 'FEATURES))  ;; REBUILDS THE HIGHER LEVEL NODE TO INCORPORATE
                NB                                      ;; THE DAUGHTER THAT WAS JUST PARSED EXCEPT IN THE
                N                                       ;; CASE WHERE THIS NODE IS THE TOPLEVEL
                (SETQ H (concat RE H))
                SM
                C))
    LOSE (SETQ NN (not (= N CUT)))
    OK  (SETQ LEVEL (dec LEVEL))
        RE))

(§ defn- PARSE3 [rest p]
    ;; PARSE3 IS CALLED TO PARSE JUST THE NEXT WORD IN THE SENTENCE
    (let [XP nil LABL nil RE nil SPECIAL nil NBB nil NODE nil]
        (COND ((= (SETQ NBB N) CUT) (MQ CUT) (RETURN nil))
            ((NQ B-SPECIAL)                                     ;; IS THE NEXT WORD MARKED SPECL?
            (eval (GETR 'B-SPECIAL N))                          ;; YES, DO SOMETHING SPECIALL
            (COND ((= SPECIAL 'SKIP) (GO SKIP))
                ((= SPECIAL 'LOSE) (SETQ N NBB) (RETURN nil))
                ((= SPECIAL 'DONE) (GO DONE)))))
        (COND ((car (SETQ XP rest)))                            ;; IF CALL IS (PARSE NIL FOO)
            ((NEXTWORD? (cadr rest)) (GO OK))                   ;; THEN LOOK FOR EXACT WORD "FOO"
            ((SETQ N NBB) (RETURN nil)))                        ;; IF NOT THERE, FAIL
    LOOP (COND ((not (term? (car XP)))
            (SETQ LABL (cons (caar XP) LABL)))                  ;; IF THE FEATURE IS NOT AN ATOM JUST ADD THE
            ((= (car XP) 'NULL))                               ;; FEATURE TO THE LIST
            ((memq (car XP) (FE N)))
            ((memq (car XP) '(COMPONENT BOTH)))
            ((M (car XP)) (SETQ N NBB) (RETURN nil)))
        (COND ((SETQ XP (cdr XP)) (GO LOOP)))
    OK  (SETQ RE
            (BUILDNODE (MEET (concat (FE N) LABL) (get (car rest) 'ELIM))
                N
                (cdr N)
                'WORD
                (or SMN
                    (nil? (car rest))
                    (and (nil? (SM N)) (UNDEFINED))
                    (cadr (sassq (car rest) (SM N) #'UNDEFINED)))))
        (SETQ N (cdr N))
    SKIP (SETQ NN (not (= N CUT)))
        (COND ((and NN (NQ SPECIAL))
            (eval (GETR 'SPECIAL N))))
    DONE (SETR 'PARENT C RE)
        (COND (p RE)
            (:else (REBUILD FE NB N (SETQ H (concat RE H)) SM C)))
        RE))

(§ defn- PARSEREL [a b node]
=>  (COND ((nil? a) (RETURN nil))
        ((not (ISX node (caar a))))
        ((eval (concat '(PARSE CLAUSE RSNG) (cdar a) b))
            (RETURN H)))
    (SETQ a (cdr a))
    (GO =>))

(§ defq- POP [a]
    (COND ((or (nil? a) (nil? (car a)))
            (COND ((nil? H) nil)
                ((SETQ N (NB H))
                    (SETQ H (cdr H))
                    (REBUILD FE NB N H SM C)
                    (SETQ NN (not (= N CUT)))
                    (or SMN
                        (let [XX nil]
                            (MAP #'(lambda [BACKNODE]
                                (ERRSET
                                    (and (MAP #'(lambda [PLACE] (and (= PLACE (NB BACKNODE)) (ERR))) N)
                                        (SETQ XX (cons (car BACKNODE) XX)))))
                                BACKREF)
                            (SETQ BACKREF XX)
                            nil))
                    true)))
        ((eval (cons 'POPTO a)) (POP))))

(§ defq- POPTO [a]
    (let [XX H]
    LOOP (COND ((eval (cons 'ISQ (cons 'XX a))))
            ((SETQ XX (cdr XX)) (GO LOOP))
            ((MQ POPTO) (RETURN nil)))
    EX  (COND ((= XX H) (RETURN C))
            ((POP) (GO EX)))))

(§ defn- PREVIOUS [l member]
    ;; GET THE ELEMENT OF LIST BEFORE MEMBER
    (let [goodie nil]
    =>  (COND ((nil? l) (RETURN nil))
            ((= member (car l)) (RETURN goodie))
            (:else (SETQ goodie (car l)) (SETQ l (cdr l))))
        (GO =>)))

(§ defn- PTFIND [x yy z]
    (let [foo (car x)]
    UP  (COND ((MOVE-PT U) (GO UP)) ((= (NB PT) x) (GO ON)))
    DOWN (or (MOVE-PT DLC PV (memq foo (NB PT))) (RETURN nil))
    ON  (COND ((not (= x (NB PT))) (GO DOWN))
            ((= yy true))
            ((MOVE-PT DF (= (N PT) yy)))
            ((RETURN nil)))
    CHECK (COND ((eval z) (RETURN PT))
            ((not (= yy true)))
            ((MOVE-PT DF) (GO CHECK)))
        nil))

(§ defn- REBUILD [features firstword wordafter daughters semantics node]
    (SETR 'FEATURES features node)
    (SETR 'FIRSTWORD firstword node)
    (SETR 'WORDAFTER wordafter node)
    (SETR 'DAUGHTERS daughters node)
    (SETR 'SEMANTICS semantics node)
    node)

(§ defn- ROOT [x]
    ;; INPUT= PIECE OF SENTENCE
    ;; OUTPUT= ROOT OF FIRST WORD IN THAT PIECE
    ;; IF WORD HAS NO ROOT PROPERTY, THE ROOT == WORD
    (or (get (car x) 'ROOT) (car x)))

(§ defq- RQ [a] (SETR 'FEATURES (SETQ FE (SETDIF FE a)) C)) ;; REMOVE THE FEATURE A FROM FEATURE LIST OF THE CURRENT NODE

(§ defn- SECONDWORD? [word] (and N (cdr N) (= (cadr N) word)))

(§ defn- SETR [register value node]
    ;; THIS FUNCTION ASSOCIATES THE GIVEN VALUE WITH THE GIVEN
    ;; NODE UNDER THE GIVEN INDICATOR, REGISTER
    (PUTPROP (car node) value register))

(§ defn- SM [node] (GETR 'SEMANTICS node))

(§ defq- TRNSF [a]
    (SETR 'FEATURES
        (SETQ FE (UNION (MEET a (FE PT)) FE))
        C))

(§ defn- UPREL [x]
    (and (not (term? x))
        (or (memq 'UPREL (FE x)) (UPREL (H x)) (UPREL (cdr x))))) ;; FIND NODE WITH UPREL FEATURE

(§ defn- WORD [n] (car n))

(§ defn- UPCHECK []
    (and (MOVE-PT C U (REL-NOT-FOUND))
        (not (MEET (FE PT) '(OBJ1Q OBJ1REL OBJ2Q OBJ2REL LOBREL LOBQ)))))

#_(ns shrdlu.ginter)

(§ defq- PDEFINE [a]
    ;; THIS PDEFINE MERELY PUT THE PROGRAMMAR FUNCTION ON THE
    ;; PROPERTY LIST OF THE PARSE NAME UNDER THE INDICATOR
    ;; 'INTERPRET. IT ALSO ADDS THE TAGS FAIL AND RETURN. NOTE THAT
    ;; THE PDEFINE BODY IS SIMILIAR TO PROG BODY. THIS SETS UP
    ;; INTERPRETED PROGRAMMAR EXECUTIONS
    (PUTPROP (car a)
        (concat (cdr a) (list 'FAIL '(RETURN 'FAIL) 'RETURN '(RETURN 'RETURN)))
        'INTERPRET))

(§ defn- INTERPRET [unit]
    ;; INTERPRET IS THE FUNCTION WHICH 'CALLS' AN INTERPRETED PROGRAMMAR PROGRAM.
    ;; IT FIRST DECLARES AND INITIALIZES ALL THE RELAVENT VARIABLES, THEN
    ;; IT EXECUTES THE PROGRAMMAR BODY AS A PROG.  NOTE THE USE OF "RE":
    ;; IT IS SET TO A NODE ONE WISHES TO BE THE INITIAL DAUGHTER OF THIS NODE.
    ;; ONLY CONJ NEEDS THIS HACK.
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN true T1 nil T2 nil T3 nil]
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST))     ;; FEATURE LIST
            (SETQ NB (or (NB RE) N))                    ;; BEGINNING IN SENTENCE OF THIS NODE
            N                                           ;; SENTENCE POINTER JUST AFTER THIS NODE
            (SETQ H RE)                                 ;; DAUGHTERS OF THIS NODE
            nil))                                       ;; SEMANTIC JAZZ
        (SETR 'PARENT PARENT C)                         ;; SET PARENT REGISTER
        (COND ((= (APPLY #'PROG (get unit 'INTERPRET)) 'RETURN)
            (GO =>)))                               ;; APPLY THE PROGRAMMAR PROGRAM
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))                         ;; RESET SENTENCE POINTER
        (RETURN nil)
    =>  (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ defq- | [branch]
    (COND ((eval (car branch))                          ;; EVALUATE BRANCH CONDITION
            (COND ((and (nil? NN) (cdddr branch))
                    (GOCHECK (cddr branch)))            ;; IF TRUE AND NO MORE SENTENCE REMAINS
                (:else (GOCHECK branch))))                  ;; AND IF THIRD BRANCH GIVEN,THEN GO TO THIRD
        (:else (GOCHECK (cdr branch)))))                    ;; BRANCH 2ND BRANCH

(§ defn- GOCHECK [label]
    ;; THE GOCHECK FUNCTION CHECKS THE BRANCHES OF THE PROGRAMMAR CONDITIONAL.
    ;; IF THE BRANCH IS NIL, GOCHECK MERELY RETURNS.
    ;; IF THE BRANCH IS NON-ATOMIC, IT IS TAKEN AS A FAILURE MESSAGE.
    ;; GOCHECK PUTS THE MESSAGE ON THE MESSAGE LIST AND GOES TO FAIL (IN GINTERP)
    ;; IF THE BRANCH IS ATOMIC, GOCHECK GOES TO IT.
    ;; REMEMBER THAT THE GO LEADS TO A POINT SOMEWHERE IN THE PROGRAMMAR PROGRAM,
    ;; UNLESS IT IS 'FAIL OR 'RETURN.
    (COND ((nil? (cadr label)) true)
        ((term? (cadr label)) (GO (cadr label)))
        (:else (M (cadr label)) (GO FAIL))))

(§ defq- GOCOND [a]
    ;; GOCOND GOES TO THE 1ST OR 2ND OF TWO TAGS DEPENDING IF THERE REMAINS
    ;; ANY MORE OF THE SENTENCE YET TO BE PARSED.
    (COND (NN (GO (car a))) (:else (GO (cadr a)))))

#_(ns shrdlu.gramar)

(§ PDEFINE CLAUSE (POSITION-OF-PRT MVB LOCATIONMARKER SUBJ-VB-BACKUP-TYPE1 POSITION-OF-PTW)

    ENTERING-CLAUSE
        (SETR 'TIME (BUILD TSSNODE= (MAKESYM 'TSS)) C)
        (| (CQ SIMP) SUBJ nil)
        (| (CQ MAJOR) INIT SEC)

    INIT
        (SETQ LOCATIONMARKER N)
        (| (and (NQ BINDER) (PARSE CLAUSE BOUND INIT)) nil MAJOR FIXIT)
        (FQ BIND)
        (| (CALLSM (SMBIND)) INIT nil)

    FIXIT
        (SETQ PTW CUT)
        (| (CUT (MOVE-PTW)) INIT MAJOR)

    MAJOR
        (CUT END)
        (COND ((= PUNCT '?) (GO QUEST))
            ((or (CQ IMPER) (= PUNCT '!)) (GO IMPER)))
        (GO THEREINIT)

    FDEC
        (FQ DECLAR)

    THEREINIT                                                       ;; CONSTRUCTIONS USING THE FUNCTION WORD "THERE"
        (| (and (NEXTWORD? 'THERE)                                  ;; ARE CHECKED FOR EXPLICITLY AND PROCESSED BY A
                (PARSE nil THERE)                                   ;; SPECIAL BLOCK OF CODE (SEE ABOVE)
                (FQ DECLAR))
            THERE
            nil
            (INIT))

    THER2
        (and (NQ PREP)
            (PARSE PREPG INIT)
            (or (CALLSM (SMRELATE H)) (POP)))                       ;; MORE INITIAL (BEFORE THE SUBJECT) MODIFIERS
        (and (NQ ADV)
            (PARSE ADV TIMW)
            (or (CALLSM (SMADVERB)) (POP)))
        (and (NQ ADV)
            (PARSE ADJG ADV VBAD)
            (or (CALLSM (SMRELATE H)) (POP)))
        (PARSE NG TIME)

        (| (= LOCATIONMARKER N) CLAUSETYPE INIT INPOP)

    ;; THE VARIABLE "LOCATIONMARKER" MARKS THE POSITION OF PTW AT THE TIME THAT IT WAS SET.
    ;; IF IT HAS NOT MOVED (IS STILL EQUAL TO N), THEN THAT INDICATES THAT NOTHING HAS BEEN
    ;; PARSED AND WE CAN GO ON.  OTHERWISE, THERE CAN BE ANY NUMBER OF INITIAL MODIFIERS AND
    ;; THE CODE STARTING AT "INIT" IS REPEATED, AS MANY TIMES AS NECESSARY.  IF PTW HITS THE
    ;; CUT POINT, THEN IT IS ASSUMED THAT SOMETHING WAS MISTAKENLY PARSED AS A MODIFIER WHEN
    ;; IT WAS NOT, AND EVERYTHING IS POPPED OFF (BY THE "INPOP" CODE).

    INPOP
        (| (MOVE-PT C DLC) nil (INPOP))                             ;; DOES ANYTHING REMAIN ON THE TREE?

    BICUT
        (CUT-BACK-ONE)                                              ;; "CUT-BACK-ONE" IS THE NORMAL BACKINGUP
        (GO INIT)                                                   ;; MECHANISM FOR THE GRAMMAR, IT SETS PTW (POINTER
                                                                    ;; TO THE WORD) BACK ONE FROM WHERE IT WAS AND
                                                                    ;; SETS "CUT" TO PTW. THE FOLLOWING GOTO TELLS
                                                                    ;; WHICH BLOCK OF CODE IS TO BE REPEATED.

    ;; RE-EXAMINE THE CLAUSETYPE, PARTICULARLY TO CHECK FOR VERB-INITIAL IMPERATIVES

    CLAUSETYPE
        (| (CQ DECLAR) SUBJ nil)
        (| (and (NQ VB) (NQ INF) (PARSE VG IMPER) (FQ IMPER))
            VG1
            nil)                                                    ;; SEE THE NOTE UNDER IMPERATIVES BELOW
        (FQ DECLAR)
        (| (CQ IMPER) (IMPER) nil)

    ;; ###############################################################
    ;;         TRY TO PARSE A GRAMMATICLY ACCEPTABLE SUBJECT
    ;; ###############################################################

    ;; ONCE THAT IS DONE, SET THE SUBJECT REGISTER (FOR USE BY SEMANTIC ROUTINES AND OTHER PARTS OF THE GRAMMAR)
    ;; AND MOVE ONE TO THE CODE FOR WHICH LOOKS FOR THE MAIN VERB (MVB) - "VG"

    SUBJ (CUT END)                                                  ;; RESET CUTPOINT INCASE IT WAS MODIFIED BY
    SUBJ3                                                           ;; PREVIOUS BACKUPS IF THE FIRST WORD INDICATES
        (| (or (and (NEXTWORD? 'TO)                                 ;; THE POSSIBILITY OF A RANK-SHIFTED CLAUSE
                    (PARSE CLAUSE RSNG TO SUBJ))                    ;; SERVING AS THE SUBJECT, THEN TRY TO PARSE ONE
                (and (PARSE CLAUSE RSNG ING SUBJ)))                 ;; AS SUCH FEATURE "SUBJ" INSURES A CHECK THAT ANY
            SUBREG                                                  ;; PRONOUNS FOUND ARE IN SUBJECTIVE CASE.
            nil
            SUBJ1)
    SUBJ4
        (| (PARSE NG SUBJ) SUBREG nil SUBJ1)                        ;; IF PARSING THE SUBJ CAUSES THE CUT POINT TO BE
                                                                    ;; REACHED, THEN JUMP TO "SUBJ1" TO SEE IF WE ARE
                                                                    ;; IN CONDITIONS WHERE THAT IS ALLOWED

        ;; WHAT TO DO IF THE SUBJECT CANNOT BE DIRECTLY PARSED?  THIS IS CHECKING FOR THE SITUATION WHERE A QUESTION
        ;; WORD IS ACTING AS LOGICAL SUBJECT AND HAS ALREADY BEEN PARSED AS IN "WHAT IS IN THE BOX?"  THE CLAUSE WILL
        ;; HAVE THIS FEATURE IF IT IS ACTIVE AS A RSQ AND ITS MISSING ELEMENT HAS NOT YET BEEN DETERMINED.  SINCE WE
        ;; CANNOT FIND ANY SUBJECT, WE ASSUME THAT IT IS A SUBJECT-RELATIVE IN THIS CASE.

        (COND ((CQ REL-NOT-FOUND)
                (RQ REL-NOT-FOUND)
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VB))
            (SUBJ-VB-BACKUP-TYPE1
                (SETQ SUBJ-VB-BACKUP-TYPE1 nil)
                (GO SUBJ11))                                        ;; SEE THE LARGE NOTE ABOUT THIS IN "NOVERB".
            ((and H (ISQ H TIME) (ISQ H NG))
                (SETR 'SUBJECT H C)
                (GO VB))                                            ;; WHAT WAS INITIALLY PARSED AS A TIME-NG MODIFING
            ((MOVE-PT C U (REL-NOT-FOUND))                          ;; THE WHOLE CLAUSE MAY PROBABLY BEEN THE SUBJECT
                                                                    ;; OF THE CLAUSE THIS WORRIES ABOUT RELATIVE
                                                                    ;; CLAUSES. PLEASE NOTE THAT THE CURRENT
                                                                    ;; HALF-VERSION HAS NOT YET GOT ITS HEAD TOGETHER
                                                                    ;; ABOUT RELATIVE CLAUSES.  IE. THE CODE ISN'T
                (SETR 'SUBJECT (GETR 'RELHEAD PT) C)                ;; DEBUGGED AND HAS GAPS IN IT ESP. WHO SETS WHAT
                (SETR 'RELHEAD (GETR 'RELHEAD PT) C)                ;; REGISTER WHEN THIS WILL BE FIXED BEFORE THE
                (REMOVE-F-PT 'REL-NOT-FOUND PT)                     ;; VERSION IS FINALIZED
                (GO VB))
            ((and (CQ COMPONENT) NN) (FQ SUBJFORK) (GO VB))         ;; "SARAH ATE DINNER AND WENT TO THE MOVIES."
            (H (POP) (GO SUBJ))                                     ;; POP OFF THE CLOSEST INITIAL MODIFIER AND TRY TO
            ((GO FAIL)))                                            ;; PARSE A SUBJ AGAIN

    HEAD
        (| (or (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON)))       ;; COME HERE (ONLY?) TO TRY TIME PHRASE AS SUBJECT
            nil
            (HEAD))                                                 ;; MOVE PTW TO THE CLOSEST NOUN THEN SET THE CUT

    SUB2
        (| (POP) nil FAIL)                                          ;; POINT TO IT AND ATTEMPT A NEW PARSING IF
        (| (CUT PTW) INIT SUB2)                                     ;; NOTHING MORE TO POP, LOSE

    SUBJ1
        (COND ((ISQ H QUOTED)                                       ;; CIRCUMSTANCES UNDER WHICH IT IS ALLRIGHT TO
            (and (ISQ H LIST) (FQ LIST))                            ;; HAVE NOTHING FOLLOWING THE SUBJECT OF THE
            (FQ QUOTED)                                             ;; CLAUSE "  "MUMBLE", SAID JOHN."
            (SETQ H (H H))
            (GO RETSM)))
        (and (CQ REL-NOT-FOUND)                                     ;; THIS IS PART OF A BACKUP MECHANISM WHICH NEEDS
            (MOVE-PT H PV (QAUX))                                   ;; TO BE MORE THROUGHLY THOUGHT OUT. THE SITUATION
            (COND
                ((ISQ PT BE)                                        ;; IS EXPLAINED IN DETAIL IN QUESTION.NGQST MOVE
                    (FQ INT AUXBE)                                  ;; PT TO A VERB WHICH CAN BE AN AUXILLIARY AND
                    (RQ REL-NOT-FOUND)                              ;; WHICH CAN BEGIN A CLAUSE
                    (SETR 'COMP (GETR 'RELHEAD C) C)
                    (SETR 'SUBJECT H C)                             ;; "WHAT COLOR IS THE BLOCK?" OR "HOW BIG IS THE BLOCK?"
                    (SETMVB PT)
                    (GO ONT))
                ((ISQ PT HAVE)
                    (FQ SUBQ)
                    (RQ REL-NOT-FOUND)
                    (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                    (GO VBL))))

    SUBJ11
        (| (CUT-BACK-ONE) SUBJ3 (SUBJ11))                           ;; IF WE CAN'T CUT BACK ANY FURTHER, THEN FAIL

    SUBREG
        (SETR 'SUBJECT H C)                                         ;; THIS SETS THE "SUBJECT" REGISTER OF THE CURRENT
        (GO VB)                                                     ;; CURRENT NODE TO WHATEVER IS POINTED TO BY "H"
                                                                    ;; (IN THIS CASE THAT WOULD BE THE MOST RECENTLY
                                                                    ;; PARSED DAUGHTER OF THE CURRENT NODE)

    ;; ##################################################################
    ;;                         PARSE A VERB GROUP
    ;; ##################################################################

    ;; ONCE THE VERB GROUP IS PARSED, THE TRANSITIVITY OF THE MAIN VERB IS EXAMINED - CAUSING THE APPROPRIATE
    ;; SECTIONS OF THE CODE BELOW TO BE EXECUTED AND TO ATTEMPT TO PARSE THE REQUIRED OBJECTS.
    ;; ONCE ALL THE OBJECTS HAVE BEEN PARSED, THE PROGRAM JUMPS TO THE TAG "ONT", WHERE A SEMANTICS PROGRAM
    ;; IS CALLED TO MAKE SENSE OUT OF EVERYTHING.

    VB  (| (PARSE ADJG ADV VBAD) VB nil (VB-ADJG))                  ;; PARSE ANY INITIAL MODIFIERS
        (RQ VBLOK)                                                  ;; ?????

    VBL (| (PARSE VG) VBREG nil)                                    ;; ONCE THE VERB GROUP IS PARSED, SET THE REGISTER

    NOVERB
        (COND                                                       ;; WHAT TO DO IF THE VG CANNOT BE DIRECTLY PARSED?
            ((CQ SUBJFORK) (FQ VBFORK) (GO FINDOBJ1))
            ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
            ((not (ISQ H SUBJ)) (GO FAIL))
            ((ISQ H CLAUSE)
                (SETQ SUBJ-VB-BACKUP-TYPE1 true)
                (POP)
                (GO SUBJ4))                                         ;; THIS IS EXACTLY WHAT IT LOOKS LIKE.
                                                                    ;; AN ARBITRARY, NOT TOO WELL THOUGHTOUT BACKUP
                                                                    ;; MECHANISM.  (NEEDLESS TO SAY IT WILL GO AWAY
                                                                    ;; FAST).  WE HAVE BEEN UNABLE TO FIND A VERB
                                                                    ;; AND HAVE NOTICED THAT WE PARSED A CLAUSE OF
                                                                    ;; SOME SORT AS THE SUBJECT.  HYPOTHESIS:  WE
                                                                    ;; MISSINTERPRETED SOMETHING WHILE PARSING THAT
                                                                    ;; CLAUSE AND MANAGED TO SWALLOW UP THE VERB OF
            ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))            ;; THE HIGHER CLAUSE WITH IT.  SOLUTION:  POP OFF
    VB2
        (CUT-BACK-ONE)                                              ;; THE CLAUSE AND TRY TO REPARSE THE SEGMENT IN
        (GO SUBJ3)                                                  ;; ANOTHER FASHION.  "SUBJ4" IS PLACED THE SUBJECT
                                                                    ;; CODE AFTER LOOKING FOR CLAUSES AND BEFORE NOUN
                                                                    ;; GROUPS.  DEFAULT CUTTING MECHANISM FOR VBL.
    VBREG
        (SETR 'VG H C)

    ;; ###############################################################
    ;;
    ;;             PARSE ANY OBJECTS REQUIRED BY THE VERB
    ;;
    ;; ###############################################################

    VG1 (CUT END)                                                   ;; RESET THE CUTPOINT IN CASE ANYONE CHANGED IT
        (| (ISQ MVB BE) BE nil (BE))                                ;; JUMP TO "BE" PROCESSOR

        ;; There used to be a check here for a quoting MVB with a quoted subject.
        ;; It was deleted because it went to a tag that no longer exists and doesn't seem to have any modern analogs.
        ;; For the original code: see "gramar 19" or earlier.
        ;; It was put in by Jeff Hill in the spring of 1972.

        ;; VERB-PARTICLE COMBINATIONS SUCH AS "PUT ON", "SET DOWN", ETC.
        ;; THEIR ESSENTIAL PROPERTY IS THAT VERB AND PARTICLE CAN BE DISPLACED BY THE OBJECT.
        ;;    "PUT DOWN THE BLOCK."
        ;;    "PUT THE BLOCK DOWN."

        (| (ISQ MVB VPRT) nil CHECKPASV CHECKPASV)
        (| (and (NQ PRT) (PARSE PRT)) nil DPRT)                             ;; IF THE PARTICLE IS NOT THE WORD FOLLOWING THE VERB, THEN
        (FQ PRT)                                                            ;; IT IS SEARCHED FOR BY CODE AT "DPRT" (DISPLACED PARTICLE)

        (| (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD (NB H))))            ;; IS THIS A LEGITIMATE COMBINATION OF VERB AND PARTICLE?
            CHECKPASV
            POPRT)

    DPRT
        (| (ISQ H PASV) CHECKPASV nil)                                      ;; SEARCH FOR DISPLACED PARTICLE.  NO DISPLACED PARTICLES
        (| (SETQ POSITION-OF-PRT (MOVE-PTW N NW (PRT))) nil FINDOBJ1)       ;; IN PASV'S IF NOT FOUND ASSUME THAT IT IS OPTIONAL AND
        (| (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD POSITION-OF-PRT)))   ;; WE ARE DEALING WITH THE CASE WITHOUT THE PARTICLE
            nil
            POPRT)
        (| (ISQ MVB TRANS) nil FINDOBJ1)
        (CUT POSITION-OF-PRT)
        (| (PARSE NG OBJ OBJ1)                                              ;; PARSE UP ANY NOUN GROUP YOU FIND
            POPRT
            FINDOBJ1                                                        ;; IF THERE ARE MORE OR LESS NP'S THAN EXPECTED,
            nil)                                                            ;; THEN DON'T PARSE ANYTHING BUT GO TO NPRT
        (CUT END)                                                           ;; INSTEAD. SIMILARLY, IF ANYTHING FOLLOWS THE
        (SETR 'OBJ1 H C)                                                    ;; DISPLACED PARTICLE THEN A GRAMMATICALLY BAD
        (PARSE PRT)                                                         ;; FORM IS ASSUMED AND THE PIECES POPED OFF
        (FQ PRT DPRT)
        (GO FINDOBJ2)

    POPRT
        (POPTO VG)
        (GO FINDOBJ1)

    CHECKPASV                                                               ;; CHECK THE VERB FOR THE PASSIVE CONSTRUCTION
        (| (and (ISQ H PASV) (FQ PASV) (SETR 'OBJ1 (GETR 'SUBJECT C) C))
            FINDOBJ2
            nil
            FINDFAKE2)
        (FQ ACTV)                                                           ;; NOT PASV=ACTIVE
        (GO FINDOBJ1)

    BE
        (FQ BE)
        (and (PARSE nil NOT) (FQ NEG))
        (PARSE ADV VBAD)

    FINDOBJ1
        (| (or (CANPARSE 1 '(ADJG COMP) 'INT) (CANPARSE 1 '(NG COMP) 'INT))
            CHECKIT
            nil
            ONT)
        (| (or (CANPARSE 1 '(PREPG COMP) 'INT)
                (CANPARSE 1 '(CLAUSE RSNG ING) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG REPORT) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG TO) 'TRANS)
                (CANPARSE 1 '(PREPG LOC) 'ITRNSL)
                (CANPARSE 1 '(ADV PLACE) 'ITRNSL))
            ONT
            nil)
        (| (CANPARSE 1 '(NG) 'TRANS)
            FINDOBJ2
            nil
            FINDFAKE2)

    FINDFAKE1
        (| (MOVE-PT C U (REL-NOT-FOUND)) OBJ1REL nil)
        (| (and (CANTAKE 1 '(PREPG LOC) 'ITRNSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ ITRANSL))
            PUTLOBJ
            nil)
        (| (CANPARSE 1 nil 'ITRNS) ONT nil)

    GOOF1
        (or GLOBAL-MESSAGE (erterr "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)

    OBJ1REL
        (SETR 'OBJ1 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ1REL)

    FINDOBJ2
        (| (CANPARSE 2 '(CLAUSE RSNG TO) 'TRANS2)
            FIXSUBJECT
            nil)
        (| (or (CANPARSE 2 '(ADV PLACE) 'TRANSL) (CANPARSE 2 '(PREPG LOC) 'TRANSL))
            ONT
            nil)
        (| (or (CANPARSE 2 '(ADJG COMP) 'TRANSINT) (CANPARSE 2 '(NG COMP) 'TRANSINT))
            ONT
            nil)
        (| (CANPARSE 2 '(NG) 'TRANS2) ONT nil)

    FINDFAKE2
        (| (and (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND)))
            OBJ2REL
            nil)
        (| (and (CANTAKE 2 '(PREPG LOC) 'TRANSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ TRANSL))
            PUTLOBJ
            nil)

    OBJ2TO
        (PARSE ADV VBAD)
        (| (COND ((and (NEXTWORD? 'TO)
                    (ISQ MVB TO2)
                    (PARSE PREPG TO))                               ;; THE SECOND-OBJECT THAT WE HAVE BEEN LOOKING FOR
                (SETR 'OBJ2 (GETR 'OBJ1 H) C)                       ;; MAY BE A PREPG AS IN "GIVE IT TO THE LIONS"
                (FQ TRANS2TO TRANS2))                               ;; TAKES THE OBJECT OF THE PREPOSITION "TO" AND
            ((and (CQ PREPQ)                                        ;; MAKES IT THE OBJ2 OF THE CLAUSE.
                (MOVE-PT H PV (QUEST))
                (= (WORD (MOVE-PTW FW)) 'TO)
                (RQ PREPQ)
                (FQ TRANS2TOQ TRANS2)
                (SETR 'OBJ2 (GETR 'OBJ1 PT) C))))                   ;; "TO WHOM DID YOU GIVE THE MEAT?"
            ONT
            nil)
        (| (CANPARSE 2 nil 'TRANS) ONT FAIL)

    PUTLOBJ
        (SETR 'LOBJ PT C)
        (SETR 'RELHEAD (GETR 'QADJ PT) PT)
        (SETR 'QADJ nil PT)
        (REMOVE-F-PT 'QADJ PT)
        (GO ONT)

    OBJ2REL
        (SETR 'OBJ2 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ2REL)
        (GO ONT)

    FIXSUBJECT
        (SETR 'SUBJECT (GETR 'OBJ1 C) H)
        (GO ONT)

    CHECKIT                                                         ;; CHECK FOR THE POSSIBILITY THAT THE SUBJECT WAS
        (| (= (WORD (NB (GETR 'SUBJECT C))) 'IT)                   ;; A DUMMY FUNCTION WORD ("IT"), AS IN "IT WAS NICE TO SEE HIM."
            nil
            ONT)                                                    ;; TO BE ADDED HERE: "JOHN WAS EAGER/EASY TO PLEASE."
        (| (or (and (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ))
                (and (NQ ING) (PARSE CLAUSE RSNG ING SUBJ))
                (PARSE CLAUSE REPORT))
            nil
            ONT)
        (FQ IT)
        (SETR 'LOGICAL-SUBJECT H C)                                 ;; THE CLAUSE IS THE REAL SUBJECT.
        (GO ONT)

    GOOF2
        (or GLOBAL-MESSAGE (erterr "NEW TRANSITIVITY - SECOND OBJECT"))
        (GO FAIL)

    ;; ###########################################################################################
    ;;
    ;;                               INITIAL SEMANTIC PROCESSING
    ;;
    ;; ###########################################################################################

    ONT
        (| (CQ PASV) PONT nil)
    ONT1
        (| (CALLSM (SMCL1)) nil (SMCL1))

        (| (not (CQ REL-NOT-FOUND)) TONT nil RETSM)                 ;; IF THE FEATURE "REL-NOT-FOUND" IS PRESENT AT
                                                                    ;; THIS POINT, IT INDICATES THAT WE ARE IN A
        (| (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1)                ;; RELATIVE CLAUSE AND MAY HAVE TO DO SOME
            nil                                                     ;; GYMNASTICS IF THE CLAUSE IS NOT TO FAIL
            PREPSHORT)                                              ;; MOVE BACK TO A QUESTION-NOUNGROUP, THEN DOWN

    TIMEQ
        (RQ REL-NOT-FOUND)                                          ;; AND BACK TO THE NOUN. IF THAT NOUN IS "TIM1"
        (FQ TIMEQ)                                                  ;; THEN ASSUME WE HAVE FOUND OUR RELATIVE ELEMENT.
        (GO TONT)

    PREPSHORT
        (| (and (NQ PREP) (PARSE PREPG)) nil (ONT-SHORT-PREP))
        (| (CALLSM (SMRELATE H)) nil (ONTß SMRELATE PREPQ))
        (| (CQ REL-NOT-FOUND) PREPSHORT TONT (ONT-NOT-FOUND))       ;; WE HAVE A PREP TO TAKE THE UNATTACHED RELATIVE
                                                                    ;; AS ITS OBJECT. THE FEATURE REL-NOT-FOUND WILL
                                                                    ;; BE REMOVED IF THE PREPG DISCOVERS IT CAN'T FIND

    PONT
        (and (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))        ;; AN OBJECT (THE REMOVING WILL BE DONE WHILE IN PREPG).
        (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)                    ;; "LOGICAL" IE. SUBJECT IN RELATIONSHIP
        (GO ONT1)                                                   ;; TO THE PROPER SEMANTIC INTERPRETATION OF THE
                                                                    ;; MAIN VERB. AGENT-PREPG CAN BE PARSED (REFLECTS
                                                                    ;; THE OPTIONALITY OF THE CONSTRUCTION)

    ;; ###################################################################################
    ;;                    CHECK FOR ADDITIONAL MODIFYING PHRASES
    ;; ###################################################################################

    TONT (| (SETQ POSITION-OF-PTW N) nil RETSM RETSM)               ;; WE ARE USING THE SAME TECHNIQUE HERE AS WITH THE INITIAL MODIFIERS.
                                                                    ;; IE. LOOP THROUGH THE POSSIBILITIES UNTIL YOU MAKE A PASS THAT ADDS
                                                                    ;; NOTHING NEW.

    NPASV
        (| (and (NQ PREP) (PARSE PREPG) (CALLSM (SMRELATE H)))                                  ;; PREPG
            nil
            nil
            RETSM)
        (| (and (NQ TIMW) (PARSE ADV TIMW) (or (CALLSM (SMTIME)) (GO FAIL)))                    ;; TIMW
            nil
            nil
            RETSM)
        (| (and (not (CQ BE)) (PARSE ADJG ADV) (or (CALLSM (SMRELATE H)) (GO FAIL)))            ;; ADV
            nil
            nil
            RETSM)
        (| (and (PARSE NG TIME) (or (CALLSM (SMTIME)) (GO FAIL)))                               ;; TIME NOUN GROUP
            nil
            nil
            RETSM)
        (| (and (NQ PLACE) (PARSE ADV PLACE) (or (CALLSM (SMPLACE)) (GO FAIL)))                 ;; PLACE
            nil
            nil
            RETSM)
        (| (and (NQ BINDER) (PARSE CLAUSE BOUND) (or (CALLSM (SMBIND)) (GO FAIL)))              ;; BINDER
            nil
            nil
            RETSM)
        (| (and (NEXTWORD? 'TO) (PARSE CLAUSE TO ADJUNCT) (or (CALLSM (SMTOADJ)) (GO FAIL)))    ;; TO CLAUSE (ADJUNCT)
            nil
            nil
            RETSM)
        (| (= N POSITION-OF-PTW) nil TONT RETSM)                   ;; LOOP UNTIL NOTHING ELSE CAN BE PARSED.
        (| (or (not (CQ TOPLEVEL)) (NQ SPECIAL)) RETSM nil)         ;; SPECIAL WORD (E.G. COMMA AND) COULD INDICATE
        (ert "CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL")              ;; A CONJUNCTION OR A BINDER.
        (GO FAIL)

    ;; ##############################################################################
    ;;                                   THERE
    ;;             AS IN:  "THERE IS A BIRD SITTING ON YOUR SHOULDER"
    ;; ##############################################################################

    THERE
        (FQ THERE)
        (CUT END)
        (| (PARSE ADV TIMW) nil nil (THERE))                        ;; "THERE IS A BIRD.."
        (| (and (PARSE VG) (ISQ MVB BE)) THEF NOTHE (THERE))

    THERQ
        (| (ISQ (MOVE-PT H PV (QAUX)) BE) THERQ2 nil)               ;; IF THIS FAILS, THE THERE IS CONSIDERED TO BE
        (| (and (NQ TIMW) (PARSE ADV TIMW)) nil nil (THEREQ))
        (| (and (PARSE VG) (ISQ MVB BE)) THERQ2 nil)
        (RQ POLR2)
        (GO NOTHE)

    THERQ2
        (FQ SUBJTQ) (FQ THERE)
        ;; THIS MAY NOT INTERFACE PROPERLY WITH THE SEMANTIC ROUTINES FOR BE
        (| (CQ POLAR) THEF ONT)

    THEF
        (| (and (NQ ADV) (PARSE ADV TIMW)) nil nil (THEF))
        (| (PARSE NG SUBJ SUBJT) nil THERREL)
        (FQ THERE)
        (SETR 'SUBJECT H C)
        (GO ONT)

    THERREL
        (| (MOVE-PT C U (REL-NOT-FOUND)) nil NOTHE)
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (GO ONT)

    NOTHE
        (RQ THERE)
        (POP THERE)
        (and (NQ ADV) (PARSE ADV PLACE))
        (GO THER2)

    ;; ####################################################################
    ;;                            IMPERATIVES
    ;; ####################################################################

    IMPER
        (| (PARSE NG TIME) nil nil IMPOP)                           ;; MODIFIERS WHICH MAY PRECEED THE VERB
        (| (and (NQ ADV) (PARSE ADJG ADV VBAD)) nil nil IMPOP)
        (| (and (NQ ADV) (PARSE ADV TIMW)) nil nil IMPOP)

    IMPE
        (| (PARSE VG IMPER) nil IMPOP)
        (FQ IMPER)
        (GO VG1)

    IMPOP
        (| (POP nil) IMPE (IMPOP))

    ;; ####################################################################
    ;;                             QUESTIONS
    ;; ####################################################################

    QUEST ;; PREP QUESTION
        (FQ QUEST)
        (| (NQ PREP) nil NGQUES)
        (| (PARSE PREPG) nil NGQUES (PREPQ-INCOMPLETE))             ;; "ON WHICH BLOCK DID YOU PUT IT?"
        (| (ISQ H QUEST) nil QUEST)                                 ;; IF THE PREPG ISN'T THE QUESTION, TRY AGAIN "ON
        (SETR 'QADJ H C)                                            ;; THAT DAY, WHERE DID YOU GO?" -- MAYBE WE COULD
                                                                    ;; MAKE USE OF THE COMMA CLUE. PREPQ IS HANDLED
        (GO POLAR)                                                  ;; MUCH LIKE QADJS LIKE WHEN AND WHERE THE REST OF
                                                                    ;; THE QUESTION HAS THE SAME SYNTAX AS A POLAR (YES-NO).
    NGQUES ;; NOUN GROUP QUESTION
        (| (PARSE NG QUEST) NGQST nil)                              ;; "WHICH ONE IS THE MURDURER?"
        (| (or (and (NEXTWORD? 'HOW)
                (PARSE ADJG QUEST)
                (SETR 'RELHEAD H C))                                ;; "HOW BIG...."
            (and (NQ QADJ)
                (PARSE QADJ)
                (FQ QADJ)
                (SETR 'QADJ H C)))                                  ;; "WHAT...?",  "WHERE...?"
            POLAR
            POLAR
            nil)
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))                                      ;; IF ALL THE SENTENCE CONSISTS OF IS THE QUESTION

    ADJQS
        (GO RETURN)                                                 ;; ADJECTIVE THEN WE SHOULD RETURN DIRECTLY

    NGQST
        (SETR 'RELHEAD H C)

    NGQST2
        (CUT END)
        (SETR 'SUBJECT H C)
        (and (NQ ADV) (PARSE ADJG ADV VBAD))

        ;; WE HAVE HERE A VERY INTERESTING SITUATION INVOLVING A
        ;; TEMPORARY AMBIGUITY IN THE INTERPRETATION OF CERTAIN VERB
        ;; WHICH IS ELIMINATED WHEN MORE CONSTITUENTS OF THE CLAUSE
        ;; HAVE BEEN PARSED.  CONSIDER SENTENCES LIKE:
        ;;        WHICH BOX CONTAINS A RED BLOCK?
        ;;        WHICH HAND HAS THE M&M'S?
        ;;        WHICH HAND HAS HE BEEN HAVING TROUBLE WITH?
        ;; A THIS POINT IN THE CLAUSE PROGRAM WE HAVE PARSED THE
        ;; FIRST NG AND ARE ABOUT TO PARSE THE VERB GROUP.  IN THE
        ;; FIRST SENTENCE WE WILL NEVER HAVE ANY PROBLEM BECAUSE THE
        ;; VERB IS MARKED WITH THE FEATURE "NAUX" MEANING THAT IT CAN
        ;; NEVER SERVE A AN AUXILLIARY TO ANOTHER VERB.  HOWEVER IS
        ;; THE VERB WE SEE AT THIS POINT IN THE PARSEING IS A FORM OF
        ;; "HAVE" OR "BE" WE CANNOT YET DETERMINE WHETHER IT IS IN A
        ;; CONSTRUCTION SUCH AS THE SECOND OR THE THIRD SENTENCE.  -
        ;; IS IT THE MAIN VERB OF THE CLAUSE OR ONLY AN AUX TO A VERB
        ;; WHICH WE HAVEN'T PARSED YET??? WHAT WE DO IT MAKE A
        ;; TENTATIVE DECISION ONE WAY OR THE OTHER AND THEN MAKE
        ;; ARRANGEMENTS TO CHANGE THINGS LATER IF WE FIND WE WERE
        ;; WRONG.  ONCE WE HAVE PARSED THE NEXT NOUN GROUP WE CAN
        ;; MAKE THE FINAL DECISION.  OUR TENTATIVE CHOICE IS TO CALL
        ;; THE VERB WE JUST PARSED THE MAIN VERB OF THE SENTENCE.
        ;; THEN WE KNOW THAT IF ANOTHER VERB FOLLOWS THE NEXT NG WHEN
        ;; WE SHOULDN'T EXPECT ONE THAT WE HAVE MADE THE WRONG CHOICE
        ;; AND SHOULD REARRANGE OUR ANALYSIS

        (COND ((PARSE VG NAUX) (FQ SUBJQ) (GO VG1))
            ((NQ VB) (FQ REL-NOT-FOUND) (GO POLAR))
            (:else (MOVE-PTW N PW)
                (POP NG QUEST)
                (CUT PTW)
                (GO NGQUES)))                                       ;; POP BACK AND START FIGURING OUT THE QUESTION

    QUEST2                                                          ;; ALL OVER AGAIN
        (| (and (NEXTWORD? 'THERE) (PARSE nil THERE))
            THERQ
            SUBF)                                                   ;; "ARE THERE....?"

    SUBF
        (| (PARSE NG SUBJ)                                          ;; PARSE THE SUBJECT OF ANYTHING LIKE: "DID THE
                                                                    ;; WOMAN GET THE JOB?" IF SUCCESSFUL, CONTINUE AT
                                                                    ;; "SUBREG" IN THE NORMAL PART OF THE CLAUSE
                                                                    ;; PROGRAM (RESETTING THE SUBJECT REGISTER)  (THE
            SUBREG                                                  ;; BEGINNING OF THE VERB GROUP SECTION). "SUBJ1"
            nil                                                     ;; WORRIES ABOUT WHAT SHOULD HAPPEN IF THE SUBJECT
            SUBJ1)                                                  ;; SEEMS TO FINISH THE SENTENCE
        (RQ REL-NOT-FOUND)
        (GO BE)

    POLAR
        (| (and (NQ VB) (PARSE VB AUX (QAUX)) (SETR 'QAUX H C) (CALLSM (SMVAUX)) (SETMVB H))
            nil
            QCHOP)
        (or (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
        (FQ POLR2)
        (GO QUEST2)

    QCHOP
        (ert "CLAUSE: QCHOP")
        (| (POPTO CLAUSE BOUND) BICUT (QCHOP))

    ;; ##############################################################################
    ;;                              SECONDARY CLAUSES
    ;; ##############################################################################

    ;; SECONDARY CLAUSES ARE PRINCABLY THOSE THAT ARE NOT MAJOR.
    ;; THIS INCLUDES ADJUNCTS, RANK-SHIFTED-NOUN-GROUP'S,
    ;; RANK-SHIFTED-QUALIFIERS, FOR-TO CLAUSES AND OTHERS.
    ;; IF THE CLAUSE IS MARKED "RSQ", THEN IT AUTOMATICALLY WILL
    ;; HAVE SEVERAL SPECIAL REGISTERS ASSOCIATED WITH IT TO
    ;; FACILITATE SEMANTIC PROCESSING.  'RELWORD WILL POINT TO
    ;; THE INITIAL RELATIVE PRONOUN IF THERE IS ONE (THAT, WHICH,
    ;; WHO...).  ALSO "REL-NOT-FOUND" IS A TEMPORARY FEATURE
    ;; WHICH IS RELIVANT DURING THE PROCESSING OF A CLAUSE, IT
    ;; INDICATES THAT THE ELEMENT OF THE CLAUSE WHICH WAS TAKEN
    ;; OVER INTO THE RELATIVE WORD (SUBJ, OBJ1, ...) HAS NOT YET
    ;; BEEN DETERMINED.  'RELHEAD IS A REGISTER WHICH POINTS TO
    ;; WHATEVER THE CLAUSE MODIFIES.  IN THE CASE OF AN RSQ IT IS
    ;; SET INITIALLY TO "(GETR 'HEAD (MOVE-PT U))" WHICH IS THE
    ;; HEAD NOUN OF THE NP WITHIN WHICH THE RSQ IS BEING PROCESSED.

    SEC
        (COND ((CQ BOUND) (GO BOUND))                               ;; CHECK INITIAL FEATURES AND JUMP ACCORDINGLY
            ((CQ TO) (GO TO))
            ((CQ RSQ) (GO RSQ))
            ((CQ REPORT) (GO REPORT))
            ((CQ ING) (GO ING))
            (:else (MQ RSNG-TYPE) (GO FAIL)))

    BOUND ;; BINDER
        (| (PARSE BINDER) nil (BOUND) (BINDER))
        (SETQ LOCATIONMARKER N)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)                                                   ;; "FDEC" IS NEAR THE TOP OF THE MAJOR CLAUSE

    RSQ
        (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
        (| (CQ PREPREL) nil RSQ2)
        (PARSE PREPG PRONREL)                                       ;; THIS CALL IS BASED ON INFORMATION PASSED FROM
        (SETR 'QADJ H C)                                            ;; FAR AWAY AND EXPLAINED IN DETAIL IN THE CODE
        (GO REPORT)                                                 ;; FOR PREPOSITION GROUPS

    RSQ2
        (COND ((PARSE VG EN PASV)                                   ;; HAVING DETERMINED THAT THE VERB IS PASSIVE IF
                (or (ISQ MVB TRANS) (GO FAIL))                      ;; IT WERE NOT ALSO TRANSITIVE, THEN WE WOULDN'T
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)                 ;; KNOW WHAT TO DO WITH WHATEVER WAS PARSED AS A
                (GO VG1))                                           ;; SUBJECT - SO WE FAIL
            ((PARSE VG ING)
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VG1))
            ((NQ PRONREL) (PARSE NG RELWD) (GO REL))
            ((CQ COMPONENT)                                         ;; IN A COMPONENT RELATIVE THE RELWD MIGHT BE IN
                (SETR 'RELHEAD                                      ;; THE FIRST CLAUSE.
                    (GETR 'RELHEAD (MOVE-PT C PC))
                    C)                                              ;; MAKE RELHEAD SAME AS PREVIOUS COMPONENT RSQ.
                (GO REL))
            ((PARSE NG SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
            (:else (GO FAIL)))                                          ;; THIS REALLY ISN'T AN RSQ

    REL
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (| (PARSE VG) VG1 nil)                                      ;; OUR FIRST HYPOTHESIS, THAT THE SUBJECT WAS THE
                                                                    ;; RELWORD, WAS JUST PROVEN WRONG SINCE WE CANNOT
                                                                    ;; PARSE THE VG NEXT. SO WE REVISE OUR FEATURES
        (FQ REL-NOT-FOUND)                                          ;; AND JUMP TO PARSE A REAL FULL SUBJECT AS IN
        (GO SUBJ)                                                   ;; "...WHICH MARY THOUGHT WAS CHAUVANISTIC" AS
                                                                    ;; OPPOSED TO "...WHICH WAS CHAUVANISTIC"

    TO
        (| (and (CQ COMPONENT) (PARSE VG TO TODEL)) VG1 nil)        ;; "I WANTED TO DANCE AND SING"
        (| (NEXTWORD? 'FOR) nil TO1)                                ;; THIS IS EXPERIMENTAL
        (PARSE nil FOR)                                             ;; PLEASE CHECK OUT ANY FOR-CLAUSES YOU CAN THINK OF
        (FQ FOR)
        (PARSE NG SUBJ TOSUBJ)
        (SETR 'SUBJECT H C)

    TO1
        (| (PARSE VG TO) VG1 (TO))

    ING
        (| (MOVE-PTW N NW (ING)) nil FAIL)
        (| (or (NQ ING) (CQ OBJ2) (and (PARSE NG SUBJ INGSUBJ) (SETR 'SUBJECT H C) (FQ SUBING) (RQ ING)))
            nil
            nil
            (ING))
        (| (PARSE VG ING) VG1 (ING))

    REPORT
        (and (NEXTWORD? 'THAT) (PARSE nil THAT) (FQ THAT))
        (SETQ LOCATIONMARKER N)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)

    ;; ##############################################################
    ;;                            RETURN
    ;; ##############################################################

    RETSM
        (or (CALLSM (SMCL2)) (GO FAIL))
        (GO RETURN))

(§ PDEFINE NG nil

    ENTERING-NG

    NGSTART                                                         ;; EXAMINE INITIAL FEATURES AND JUMP TO
        (COND ((CQ RELWD) (GO RELWD))                               ;; CORRESPONDING SPECIAL BLOCKS OF CODE
            ((CQ QUEST) (GO QUEST))
            ((or (NQ QDET) (NQ QPRON)) (FQ QUEST) (GO QUEST))
            ((CQ TIME) (GO TIME))                                   ;; LOOK AT FIRST WORD
            ((NQ PROPN) (GO PROPN))
            ((NQ TPRON) (GO TPRON))
            ((NQ EVERPRON) (GO EVERPRON))
            ((NQ PRON) (GO PRON)))

    LOOK
        (COND ((NQ DET) (GO DET))                                   ;; THIS POINT MAY BE JUMPED BACK TO
            ((NQ NUM) (GO NUM))
            ((or (NQ ING) (NQ EN) (NQ ADJ)) (GO ADJ))
            ((NQ CLASF) (GO CLASF))
            ((NQ NUMD) (GO NUMD))
            ((NEXTWORD? 'AT) (GO AT))
            ((NEXTWORD? 'AS) (GO AS))
            ((NQ NOUN) (GO NOUN))
            ((NQ TIMORD) (GO TIMORD))
            ((and (CQ COMPONENT) (ISQ (MOVE-PT PC) QUEST)) (GO QUEST))
            ((MQ START) (GO FAIL)))

    ;; #######################################################
    ;; IF YOU CAN PARSE ANY OF THESE SMALL THINGS, YOU'RE DONE
    ;; #######################################################

    START                                                           ;; PARSE A PROPER NOUN

    PROPN
        (PARSE PROPN)
        (FQ DEF PROPNG)
        (| (ISQ H POSS) PROPS nil)
        (| (and NN (NQ PROPN)) PROPN nil)

    PROPS
        (or (CALLSM (SMPROP)) (GO FAIL))                            ;; EXAMINE ITS SEMANTICS
        (| (ISQ H POSS) POSS PRAG)

    ;; -------------- PRONOUNS ---------------

    PRON
        (| (PARSE PRON POSS) POSS nil RED2)                         ;; IS IT POSSESSIVE?

    PRON2
        (| (CQ NPRON) (NPRON) nil)
        (| (or (and (CQ SUBJ) (PARSE PRON SUBJ))                    ;; CHECK SUBJECTIVE OR OBJECTIVE CASE
                (and (or (CQ OBJ) (CQ TOSUBJ) (CQ INGSUBJ)) (PARSE PRON OBJ))
                (CQ INGSUBJ))
            nil
            (PRON))
        (FQ PRONG DEF)

    PRON3
        (| (CALLSM (SMPRON H)) nil FAIL)                            ;; EXAMINE SEMANTICS OF PN

    PRAG
        (SETR 'HEAD H C)
        (MOVE-PT H)
        (TRNSF NS NPL NFS NEG)                                      ;; MODIFY PN FEATURES TO CORRECT NUMBER
        (GO RETURN)

    ;; -------------- ANYTHING, SOMETHING, ... --------------

    TPRON
        (PARSE TPRON)
        (FQ TPRON)
        (MOVE-PT H)
        (TRNSF NS NPL ANY NEG)
        (SETR 'HEAD C H)
        (and NN (NQ ADJ) (PARSE ADJ))
        (GO SMNG)

    ;; ----------- WHATEVER, WHENEVER, WHOEVER, ... -----------

    EVERPRON
        (| (and (PARSE PRON EVERPRON) (CALLSM (SMPRON H)))
            nil
            FAIL)
        (| (and (PARSE CLAUSE RSQ NOREL) (CALLSM (SMRELATE H)))
            RETSM
            FAIL)

    AS  (| (and (PARSE nil AS) (PARSE NUMD NUMDAS) NN (PARSE nil AS))
            NUMD2
            (AS)
            (AS))

    ;; -------------- AT + NUM ---------------

    AT
        (| (and (PARSE nil AT) (PARSE NUMD NUMDAT)) nil (AT) (AT))
    NUMD2
        (| (and (PARSE NUM) (FQ NUM NUMD)) DET1 (NUMD2) INCOM)

    ;; -------------- OTHER NUMBER WORDS ---------------

    NUMD
        (| (PARSE NUMD NUMDAN) nil ND3 INCOM)
        (| (PARSE nil THAN) NUMD2 INCOM POPCOM)
    ND3
        (| (PARSE NUMD NUMDALONE) NUMD2 (NUMD) (NUMD))

    ;; -------------- TIME WORDS ---------------

    TIME
        (| (and (NQ TIME) (PARSE NOUN TIME)) RETSM nil)
        (| (MOVE-PTW N NW (TIM1)) LOOK (TIME))

    TIMORD
        (| (PARSE ORD TIMORD) nil FAIL)
        (| (and (PARSE NOUN TIM1) (FQ DET DEF) (CALLSM (SMNGTIME)))
            RETURN
            FAIL)

    ;; #################################################
    ;;     THE MAINSTREAM - MORE CMPLICATED NG TYPES
    ;; #################################################

    ;; -------------- PARSE A DETERMINER ---------------

    DET
        (PARSE DET)
        (FQ DET)
        (MOVE-PT H)                                                 ;; SHIFT PTR TO THE DETERMINER
        (| (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR)
            IND
            (bug nil)
            INCOM)

    ;; -------------- INDETERMINATE ---------------

    IND
        (| (and (= (WORD (NB H)) 'ALL) (= (WORD N) 'THE) (PARSE DET) (FQ DEF))
            NUM
            nil
            (THE))
        (| (and (ISQ H QNTFR) (FQ QNTFR)) QNUM nil)

    ;; -------------- ORDINALS AND NUMBERS ---------------

    ORD
        (| (and (PARSE ORD) (FQ ORD)) nil NUM INCOM)
        (| (and (NEXTWORD? 'OF)                                     ;; TWELTH OF OCTOBER...
                (ISQ (MOVE-PTW N NW) MONTH)
                (PARSE nil OF)
                (PARSE NOUN MONTH)
                (FQ DATE))                                          ;; REMEMBER THAT FEATURES ARE DESIGNED AS AIDS TO
            RETSM                                                   ;; SEMANTIC COMPREHENSION AS WELL AS SYNTACTIC PARSING.
            nil)
        (| (CQ DEF) nil ADJ)

    NUM
        (| (PARSE NUM) nil ADJ)                                     ;; LARGE JUMP IF FALSE
        (FQ NUM)
        (| (CQ DET) nil DET1)
        (| (COND ((and (ISQ H NS) (CQ NS)) (RQ NPL PART)) ((CQ NPL) (RQ NS PART)))
            ADJ
            (NUM)
            INCOM)

    DET1
        (COND ((ISQ H NS) (FQ NS)) (:else (FQ NPL)))                    ;; EXPLICIT CHECK FOR THE VALUE 1
        (or NN (and (FQ NUMBER) (GO INCOM)))

    NUMBER
        (FQ DET)
        (| (NQ OF) OF ADJ)

    QNUM
        (| (ISQ H NONUM) OF nil)
        (| (and (PARSE NUM) (FQ NUM)) nil OF)
        (| (COND ((== (SM H) 1) (and (CQ NS) (RQ NPL))) ((CQ NPL) (RQ NS))) ;; EXPLICIT CHECT FOR THE VALUE 1
            nil
            (NUMD)
            INCOM)
        (| (= (WORD (NB H)) 'NO) ADJ nil)                          ;; CHECKS FOR WORD "NO"

    ;; -------------- PREPG WITH "OF" ---------------

    OF  (| (and (NQ OF) (PARSE PREPG OF)) SMOF NONE)                ;; "FIVE OF THE BLOCKS"

    SMOF
        (FQ OF)
        (| (or (CALLSM (SMNGOF)) (not (POP))) RETSM INCOM)

    NONE
        (| (= (WORD (NB H)) 'NONE) INCOM ADJ)

    ;; ----------- PARSE ALL THE ADJECTIVES -----------

    ADJ
        (| (PARSE ADJ) nil EPR INCOM)
        (and (ISQ H COMPAR)
            (FQ COMPARATIVE-MODIFIER)
            (SETR 'COMPARATIVE-MODIFIER H C))
        (GO ADJ)

    EPR
        (| (or (ISQ H SUP) (ISQ H COMPAR)) nil CLASF INCOM)         ;; WE PARSED AN ADJ AND RAN OUT OF WORDS
        (FQ ADJ)
        (and (NEXTWORD? 'OF)
            (PARSE PREPG OF)
            (or (CALLSM (SMNGOF)) (GO FAIL))
            (FQ OF)
            (GO RETSM))

    ;; -------------- PARSE ALL THE CLASIFIERS ---------------

    CLASF
        (| (or (PARSE VB ING (CLASF))                               ;; TRIES TO PARSE THE LARGEST POSSIBLE NG FIRST
                (PARSE VB EN (CLASF))
                (PARSE CLASF))
            CLASF
            nil
            REDUC)

    ;; -------------- AND FINALLY - THE NOUN ---------------

    NOUN
        (| (PARSE NOUN) nil RED2)

        (| (and (CQ TIME) (not (ISQ H TIM1))) RED1 nil)

    ;; -------------- MODIFY FEATURES FOR NUMBER AND SUCH --------------

        (SETQ T1 FE)
        (COND ((and (ISQ H MASS) (or (CQ PART) (not (CQ DET))))
            (FQ MASS)))
        (COND ((not (ISQ H NPL)) (RQ NPL PART)))
        (COND ((not (ISQ H NS)) (RQ NS)))
        (COND ((and (not (CQ DET)) (not (CQ NUMD)))
            (MOVE-PT H)
            (TRNSF NPL MASS)))
        (| (MEET FE '(NS NPL PART MASS)) nil RED0)

        (| (NEXTWORD? 'THAN) nil SMNG)                              ;; "...A BIGGER BLOCK THAN..."
        (FQ THAN)                                                   ;; THE PRESENCE OF THIS FEATURE IS NOTED BELOW AND IN ADJG

    ;; AT THIS POINT SMNG1 IS CALLED FOR PRELIMINARY CHECKS AND ANALYSIS BEFORE CHECKING QUALIFIERS

    SMNG
        (SETR 'HEAD H C)                                            ;; SET HEAD REGISTER TO THE NOUN

        (| (and (CQ OBOFJ) (not (CQ DEF))) FAIL nil)                ;; JUST PARSED
        (or (CALLSM (SMNG1)) (GO FAIL))
        (| (not (ISQ H POSS)) nil POSS RETSM)                       ;; CHECK FOR POSSIVE

    ;; #################################################
    ;;               POSSIBLE QUALIFIERS
    ;; #################################################

        (| (and (CQ THAN) (PARSE ADJG)) nil RSQ-TO)                 ;; "...A BIGGER BLOCK THAN..."
        (| (CALLSM (SMRELATE H)) RETSM FAIL)

    RSQ-TO
        (| (and (NEXTWORD? 'TO) (MEET FE '(COMP SUBJ)) (PARSE CLAUSE RSQ TO) (or (CALLSM (SMRELATE H)) (GO POPRET)))
            RETSM
            nil)

    ;; -------------- AS OR COMPARATIVE ---------------

        (| (and (or (NEXTWORD? 'AS) (NQ COMPAR)) (PARSE ADJG THANNEED))
            nil
            PREPNG)                                                 ;; WHAT IS THE REASON FOR THE EXISTANCE OF THIS
        (and (nil? N)                                               ;; STRANGE ANIMAL (ALSO THE ONEBELOW) -- CHECK
            (CQ SUBJ)                                               ;; THEM OVER AND HACK THEM PROPERLY
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (GO POPRET))                                            ;; AVOIDS ATTACHING MODIFIER WHEN IT GOBBLES TO
        (| (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)               ;; MUCH E.G. IS THE BLOCK ON THE TABLE? DOESN'T
                                                                    ;; WANT "THE BLOCK ON THE TABLE" AS A CONSTITUENT.
                                                                    ;; I ADMIT ITS A HACK.

    ;; -------------- ANY SORT OR PREPOSITION GROUP --------------

    PREPNG
        (| (and (NQ PREP)
                (not (or (and (NQ PLACE) (CQ NOLOC))
                    (and (CQ OBJ1)
                        (ISQ MVB TRANSL)
                        (not (ISQ (MOVE-PT C U) QUEST)))))
                (PARSE PREPG Q))
            nil
            DISGRSQ)
        (and (nil? N)
            (CQ SUBJ)
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (not (ISQ (MOVE-PT U) NGQ))
            (GO POPRET))
        (| (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)

    ;; CHECK FOR DISGUISED RSQ CLAUSES BY READING THE FAILURE MESSAGES SENT UP FROM PREPG.

    DISGRSQ
        (| (= (car MES) 'PREP-WHICH) nil RSQ)
        (SETQ MES (cdr MES))
        (| (PARSE CLAUSE RSQ PREPREL) PREPNG (RSQ-PREPREL) RETSM)

    ;; -------------- ANY OTHER RSQ ---------------

    RSQ
        (| (and (ISQ (MOVE-PT C U) POLR2) (CQ SUBJ) (NQ VB) (not (CQ SUBJT)) (not (ISQ PT QADJ)))
            RETSM
            nil)
        (| (PARSE CLAUSE RSQ) nil RETSM)
        (| (CALLSM (SMRELATE H)) RETSM POPRET)

    ;; -------------------------------------------------
    ;; THE ENTIRE NG SHOULD HAVE BEEN PROCESSED BY THIS POINT
    ;; -------------------------------------------------

    ;; -------------------------------------------------
    ;; IF AT FIRST YOU DON'T SUCEED.......
    ;; -------------------------------------------------

    RED0
        (SETQ FE T1)
    RED1
        (POP)
    RED2
        (COND ((nil? H) (MQ NO) (GO FAIL))
           ((ISQ H NUMBER) (GO INCOM))
           ((and (ISQ H POSS) (or (ISQ H PRON) (and (MOVE-PT H DLC) (ISQ PT PRON))))
                (POP)
                (GO PRON2))
           ((and (nil? (cdr H)) (CQ DEFPOSS)) (GO POSSDEF))
           ((and (CQ QUEST) (nil? (cdr H))) (GO QDETCHECK))         ;; (CDR H) = T IF THERE IS ONLY ONE DAUGHTER TO THE CURRENT NODE
           ((ISQ H ADJ) (GO EPR))
           ((not (ISQ H CLASF)) (GO INCOM)))

    REDUC
        (POP)
        (| (and (nil? H) (NQ PROPN)) PROPN NOUN)

    POPCOM
        (POP)

    ;; -------------- INCOMPLETE PHRASES ---------------

    INCOM
        (FQ INCOM)
        (| (and (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM)))
            RETURN
            nil)
        (| (and (nil? CUT) (CQ NUM)) SMNG nil)

    QDETCHECK
        (COND ((and (ISQ H QDET) (ISQ (NB H) QPRON))
                (POP)
                (GO QPRON))
            ((and (ISQ H QDET) (ISQ (NB H) EVERPRON))
                (POP)
                (GO EVERPRON)))
        (GO FAIL)

    ;; -------------------------------------------------
    ;; POSSESSIVE HANDLER
    ;; -------------------------------------------------

    POSS
        (or (CALLSM (SMNG2)) (GO FAIL))

    POSS2
        (| (CQ INGSUBJ) RETSM nil)
        ;; IF POSSESSIVE, ALL PREVIOUS MODIFIERS MODIFY THE POSSESSIVE NOUN, NOT THE NG HEAD
        (SETQ H (BUILDNODE (reverse (cons 'POSS (SETDIF FE '(COMPONENT)))) NB N H SM))
        (SETQ BACKREF (concat H (cdr BACKREF)))
        (| (SETR 'FEATURES (SETQ FE (concat '(POSES DET DEF NS NPL) (reverse REST))) C)
            nil
            (bug nil))
        (| (or (not NN) (ISQ H DEFPOSS)) nil ORD)

    POSSDEF ;; THE PLACEMENT OF THIS TAG IS A GUESS. THE ORIGINAL IS LOST, ASSUMING THAT IT EVER EXISTED
        (RQ POSES DET DEF)
        (FQ POSSDEF NS NPL)

    ;; -------------- RELATIVES---------------

    QUEST
        (| (PARSE nil HOW) nil QDET FAIL)
        (| (PARSE nil MANY) nil FAIL INCOM)
        (FQ DET NPL INDEF HOWMANY)
        (GO OF)

    QDET
        (| (and (PARSE DET QDET) (FQ DET NPL QDET NS))
            QNUM
            nil
            INCOM)

    QPRON
        (| (PARSE PRON QPRON) PRON3 FAIL)

    RELWD
        (| (and (PARSE PRONREL)
                (CALLSM (SMSET (SM (MOVE-PT C U U (NG))))))         ;; SET SM TO THE NOUNGROUP DIRECTLY UPSTAIRS
            RETURN
            nil)

    POPRET
        (POP)

    ;; --------------------------------------------------------------
    ;; RETURN AFTER CALLING SMNG2 TO PROCESS THE COMPLETED NOUN GROUP
    ;; --------------------------------------------------------------

    RETSM
        (or (CALLSM (SMNG2)) (GO TRYA))
        (GO RETURN)

    ;; -------------- YOU PROBABLY GOOFED, CUT AND TRY AGAIN. --------------

    TRYA
        (| (ISQ H NOUN) nil (TRYA))
        (POP)
        (CUT N)

    UP
        (| (POP) UP nil)                                            ;; POP EVERYTHING OFF
        (SETQ FE (reverse REST))
        (SMSET nil)
        (GO NGSTART))

(§ PDEFINE VG (TENSE)

    ;; ##################################################################
    ;; CHECK INITIAL FEATURES TO SEE IF SOME SPECIAL TYPE OF VG IS WANTED
    ;; ##################################################################

    ENTERING-VG
        (COND ((CQ TO) (GO TO))
            ((CQ EN) (GO EN))
            ((CQ ING) (GO ING))
            ((CQ IMPER) (GO IMPER))
            ((ISQ (MOVE-PT C U) POLR2) (GO POLR2)))                 ;; CHECKS IF THE CLAUSE IS MARKED AS POLR2

    ;; -------------- DISPATCH TABLE FOR EXAMINEING THE FIRST WORD ---------------

    NEW                                                             ;; PARSE THE FIRST WORD WITH APPROPRIATE FEATURES
        (COND ((not (NQ VB)) (MQ VB) (GO FAIL))                     ;; AND JUMP TO CODE THAT KNOWS WHAT SHOULD BE
            ((and (NQ DO) (PARSE VB AUX DO)) (GO DO))               ;; LOOKED FOR NEXT IN EACH CASE
            ((and (NQ MODAL) (PARSE VB AUX MODAL)) (GO MODAL))
            ((and (NQ WILL) (PARSE VB AUX WILL)) (GO WILL))
            ((and (NQ BE) (PARSE VB AUX BE)) (GO BE))
            ((and (NQ HAVE) (PARSE VB AUX HAVE)) (GO HAVE))
            ((not (PARSE VB (MVB))) (MQ VB) (GO FAIL)))

    SIMPLE
        (MOVE-PT C DLC)                                             ;; MOVE PT DOWN FROM THE CURRENT NODE BEING PARSED
        (TRNSF VPL INF V3PS)                                        ;; (VG) AND ACROSS TO THE MOST RECENTLY PARSED
        (SETQ TENSE (COND ((and (ISQ PT PRESENT) (ISQ PT PAST))     ;; DAUGHTER.  IN THIS CASE THAT DAUGHTER WAS PARSED
                    '(PAST-PRESENT))                                ;; IN THE DISPATCH TABLE JUST ABOVE
                ((ISQ PT PAST) '(PAST))
                (:else '(PRESENT))))
        (GO REV)

    TO
        (FQ NAGR)                                                   ;; "NAGR" MARKS THAT SUBJECT AND MAIN VERB NEED
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))            ;; NOT AGREE IN NUMBER AND PERSON AND INSURES THAT
        (| (or (PARSE nil TO) (CQ TODEL)) nil (TO) (TO))            ;; THE AGREEMENT CHECKER AT THE END OF THE PROGRAM
                                                                    ;; ("REV") WILL NOT BE APPLIED "TODEL" MUST BE
        (SETQ TENSE '(INFINITIVE))                                  ;; GIVEN AS AN INITIAL FEATURE OR ELSE THIS
        (GO MODAL2)                                                 ;; STATEMENT FAILS TENSE IS USED TO HOLD THE TENSE
                                                                    ;; WHILE IT IS BEING COLLECTED.

    EN
        (FQ NAGR)
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))
        (SETQ TENSE '(PAST))
        (| (and (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)) RETSM FAIL) ;; DONE AT "EN2"

    ING
        (FQ NAGR)
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))

    INGADV
        (| (or (PARSE ADV TIMW) (PARSE ADV VBAD)) INGADV nil)
        (SETQ TENSE '(PRESENT))
        (GO BE2)

    IMPER
        (| (and (PARSE VB DO NEG INF) (FQ NEG)) nil nil (DONT))
        (| (and (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG)))
            RETURN
            (IMPER))                                                ;; MVB IS BOUND BY CLAUSE

    POLR2                                                           ;; THE CLAUSE COULD ONLY BE MARKED AS "POLR2"
        (or (SETQ PT (GETR 'QAUX (MOVE-PT C U)))                    ;; ("DID THE...?") IF AN AUX OF SOME VERIETY HAD
            (and (bug "VGßPOLR2") (GO FAIL)))                         ;; ALREADY BEEN PARSED, IF THAT IS NOT THE CASE,
        (SETQ H (list (car PT)))                                    ;; THEN WE HAVE A BUG IN THE PROGRAM SOMEWHERE SET
        (TRNSF NEG)                                                 ;; THE INITIAL DAUGHTER OF THE VG TO BE THE
        (COND ((ISQ H DO) (GO DO))                                  ;; PREVIOUSLY PARSED AUX MARK THE VG AS NEG IF
            ((ISQ H MODAL) (GO MODAL))                              ;; APPROPRIATE (SEE PROGMR FILE FOR THE OPPERATION
            ((ISQ H WILL) (GO WILL))                                ;; OF THIS FUNCTION) DISPATCH TABLE , CHECKING THE
            ((ISQ H BE) (GO BE))                                    ;; AUX
            ((ISQ H HAVE) (GO HAVE)))
        (ert "BUG VGßPOLR2VB")                                        ;; NOTHING BUT UNGRAMATICAL NONSENSE SHOULD REACH
        (GO FAIL)                                                   ;; THIS POINT

    ;; ###############################################################
    ;; PROCESSING OF VB'S NOT SPECIALLY MARKED FOR BY INITIAL FEATURES
    ;; ###############################################################

    DO  (FQ DO)
        (MOVE-PT C DLC)                                             ;; MOVE TO THE "DO"
        (TRNSF VPL NEG INF V3PS)                                    ;; ARRANGE ITS FEATURES
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (GOCOND DO2 MVB)                                            ;; GO CONDITIONALY TO THE FIRST TAG IF MORE WORDS
                                                                    ;; REMAIN BEFORE THE CUT POINT, AND TO THE SECOND
                                                                    ;; TAG IF THERE ARE NONE

    DO2 (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))

    ADV2
        (| (or (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV2 nil (ADV))
        (| (PARSE VB (MVB) INF) nil MVB)                            ;; "MVB" ARRANGES FOR A CHECK TO INSURE THAT THE
        (GO REV)                                                    ;; VERB BEING PARSED CAN BE A MAIN VERB

    MODAL
        (FQ NAGR MODAL)
        (SETQ TENSE '(MODAL))
        (GOCOND MODAL2 INCOMP)

    MODAL2
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))

    ADV3
        (| (or (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV3 nil (ADV))
        (COND ((PARSE VB BE INF) (GOCOND BE2 MVB))                  ;; DISPATCH TABLE FOR THE NEXT VERB
            ((PARSE VB HAVE INF) (GOCOND HAV2 MVB))
            ((PARSE VB INF (MVB)) (GO REV))
            (:else (GO INCOMP)))

    WILL
        (FQ NAGR)
        (SETQ TENSE '(FUTURE))
        (GOCOND MODAL2 INCOMP)                                      ;; THE SAME POSSIBILITIES FOR THE NEXT VERB APPLY
                                                                    ;; AFTER BOTH WILL AND MODALS

    BE
        (MOVE-PT C DLC)                                             ;; POINT TO WHAT WAS JUST PARSED
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (GOCOND BE2 MVB)

    BE2
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))

    ADV4
        (| (or (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV4 nil (ADV))
        (COND ((and (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))      ;; "...WILL BE GOING TO..."
            ((and (NQ BE) (PARSE VB ING))                           ;; "BE BEING"
                (SETQ TENSE (cons 'PRESENT TENSE))
                (GO EN2))                                           ;; AS IN "BE BEING X'EN(ED)"
            ((and (NQ ING) (PARSE VB ING (MVB)))                    ;; "BE X'ING"
                (SETQ TENSE (cons 'PRESENT TENSE))
                (GO REV))
            ((CQ ING) (MQ ING) (GO FAIL)))                          ;; IF TRUE, IT IMPLYS THAT WE STARTED OFF WITH
                                                                    ;; "BEING" - AS IN "BEING EATEN CAN BE UNPLEASANT"
                                                                    ;; - OTHERWISE IT IMPLYS THAT WE HAVE SOMETHING
                                                                    ;; OTHER THAN A VG ON OUR HANDS AND SHOULD FAIL TO
                                                                    ;; WHOEVER CALLED US AND TRY TO PARSE IT
                                                                    ;; DIFFERENTLY

    EN2
        (| (PARSE VB EN (MVB)) nil MVBE)                            ;; THIS ASKS: DO WE HAVE A VERB IN ITS EN FORM
                                                                    ;; WHICH CAN ACT AS A MAIN VERB (IN WHICH CASE IT
        (FQ PASV)                                                   ;; IS MARKED AS PASSIVE AND WE RETURN) OTHERWISE
        (GO REV)                                                    ;; CHECK IF THE VERB BEING POINTED AT IS A
                                                                    ;; LEGITIMATE FORM OF "BE" IN ITS MAIN VERB SENSE
                                                                    ;; - WHICH IS DONE AT "MVBE"

    GOING
        (| (PARSE nil TO) nil GOI)
        (| (NQ INF) GOING2 nil nil)
        (POP)

    GOI
        (SETQ TENSE (cons 'PRESENT TENSE))                          ;; WE HAVE DETERMINED THAT "GOING" IS THE ACTUAL
        (GO MVB)                                                    ;; MAIN VERB AND SHOULD BE PARSED AS SUCH

    GOING2
        (SETQ TENSE (cons 'FUTURE TENSE))                           ;; HERE WE DETERMINE THAT THE PHRASE IS ACTUALLY
        (GO MODAL2)                                                 ;; OF THE FORM "...IS GOING TO FALL IN LOVE..."
                                                                    ;; AND WE SHOULD RUN THROUGH THE DISPATCH TABLE AT
                                                                    ;; "MODAL2" TO DETERMINE HOW TO CONTINUE

    MVBE
        (| (ISQ (MOVE-PT H PV (VB)) AUX) nil MVB)                   ;; MOVE TO EARLIER AND EARLIER DAUGHTERS  UNTILL
        (| (ISQ PT BE) nil (MVBE))                                  ;; YOU REACH A VERB WHICH IS A "QAUX" - IF THERE
                                                                    ;; ARE NONE THEN CONTINUE AT "MVB" IF WHAT YOU ARE
                                                                    ;; POINTING TO (THE "QAUX") IS NOT A FORM OF "BE",
        (SETMVB PT)                                                 ;; THEN FAIL BECAUSE OF THE UNGRAMATICALITY OF THE
        (GO REV)                                                    ;; CONSTRUCTION OF "BE"'S OTHERWISE MARK IT AS THE
                                                                    ;; MVB AND PREPARE TO RETURN

    HAVE
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST)) (:else '(PRESENT))))
        (GOCOND HAV2 MVB)                                           ;; HAV2 WILL CATCH "HAVE HAD", OR "HAVE BEEN ..."

    HAV2
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))            ;; OR "HAVE KISSED"

    ADV5
        (| (PARSE ADV) ADV5 nil (ADV))
        (| (PARSE VB BE EN) nil HAV3)
        (SETQ TENSE (cons 'PAST TENSE))                             ;; "HAVE BEEN ..."
        (GOCOND BE2 MVB)

    HAV3
        (| (PARSE VB (MVB) EN) nil MVB)
        (SETQ TENSE (cons 'PAST TENSE))                             ;; "HAVE KISSED"
        (GO REV)

    INCOMP
        (FQ INCOMP)
        (GO FAIL)

    MVB
        (| (= (FE MVB) (FE H)) MVB2 nil)
        (POP VB)                                                    ;; POP OFF EVERY THING UNTILL YOU REACH A VERB
        (| (PARSE VB (MVB)) nil (MVB))

    MVB2
        (GO REV)

    ;; -------------------------------------------------
    ;;   CHECK AGREEMENT BETWEEN SUBJECT AND MAIN VERB
    ;; -------------------------------------------------

    REV
        (SETR 'TENSE TENSE C)
        (and NN (PARSE nil NOT) (FQ NEG))
        (COND ((or (= TENSE '(PAST))
                    (CQ NAGR)
                    (ISQ (MOVE-PT C U) IMPER)                       ;; MOVE PT TO THE CLAUSE REMEMBER THAT THE POINTER
                    (ISQ PT THERE)                                  ;; STAYS WHERE IT'S PUT UNTILL RETURNING FROM A
                    (ISQ PT RSNG))                                  ;; CALL TO PARSE
                (GO NAUX))
            ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))               ;; "SUBJECT" IS THE SYNTACTIC SUBJECT OF THE
            (:else (erterr "VG -- NO SUBJECT TO CHECK FOR AGREEMENT")))   ;; CLAUSE THAT THE VG IS IN, WHOSE ESSENTIAL
                                                                    ;; DISTINGUISHING FEATURE IS AGREEMENT WITH THE VERB

        (SETQ T3 nil)                                               ;; T3 WILL ACT AS A SWITCH AT "NAGR" BELOW. NOTE
                                                                    ;; THAT IT IS EXPLICITLY SET BY THE CODE BELOW BY
                                                                    ;; THE FOLLOWING CRITERIA;   IF T3 IS NON-NIL THEN
                                                                    ;; SUBJECT AND VERB HAVE BEEN DETERMINED TO AGREE
                                                                    ;; IF IT IS NIL THEN THEY WILL BE CONSIDERED TO
                                                                    ;; AGREE ONLY IF THE FEATURE "PAST-PRESENT" IS ON
        (COND ((ISQ PT NFS)                                         ;; THE MVB, IN WHICH CASE, THIS IS EVIDENCE THAT
                (or (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))      ;; THE PROPER CHOISE OF TENSE IS PAST - WHERE
            ((ISQ PT CLAUSE) (or (SETQ T3 (CQ V3PS)) (GO NAGR)))    ;; AGREEMENT IS IRRELEVANT (SEE BELOW AT "NAGR")
            ((or (ISQ PT NS) (ISQ PT MASS))
                (or (and (CQ V3PS) (SETQ T3 true))
                    (FESET PT (SETDIF (FE PT) '(NS MASS))))))
        (COND ((or (ISQ PT PART) (ISQ PT NPL))
            (or (and (MEET FE '(INF VPL)) (SETQ T3 true))
                (FESET PT (SETDIF (FE PT) '(PART NPL))))))

    NAGR
        (| (or T3
            (and (= '(PAST-PRESENT) TENSE)                      ;; NOTES WHETHER VERB AND SUBJECT WERE FOUND TO
                (SETQ TENSE '(PAST))))                              ;; AGREE AND FAILS UNLESS A SPECIAL CONDITION
            nil                                                     ;; EXISTS AS NOTED DIRECTLY ABOVE
            (NAGR))

    NAUX
        (SETMVB (or (MOVE-PT H PV (MVB)) MVB))
        (| (and (CQ NAUX) (ISQ (MOVE-PT H PV (VB)) AUX) (not (MOVE-PT PV PV (VB))))
            (NAUX)
            RETSM)                                                  ;; THE VB MAY HAVE THE FEATURE "NAUX" WHICH
                                                                    ;; INDICATES THAT IT CAN NEVER SERVE AS THE
                                                                    ;; AUXILLIARY OF ANOTHER VERB. IF THE PRESENT
                                                                    ;; PARSING REQUIRES IT TO THEN IT FAILS WE CHECK
                                                                    ;; BY SEEING IF THE VG CONTAINS ONLY ONE VERB,
                                                                    ;; WHICH IS AN AUX.

    POPV
        (ert "POPV")
        (GO FAIL)

    ;; -------------------------------------------------
    ;;           RETURN AND CHECK SEMANTICS
    ;; -------------------------------------------------

    RETSM
        (| (CALLSM (SMVG)) RETURN FAIL))

(§ PDEFINE PREPG nil

    ENTERING-PREPG

    ADV
        (| (and (NQ PREPADV) (PARSE ADV PREPADV)) ADV nil (PREPADV)) ;; CHECK FOR ANY INITIAL MODIFING ADVERBS
        (| (COND ((CQ AGENT) (NEXTWORD? 'BY))                       ;; EXAMINE THE INITIAL FEATURES OF THE PREPG TO
            ((CQ LOC) (NQ PLACE))                                   ;; CHECK FOR CONSTRAINTS ON THE PREPOSITION
            ((CQ Q) (not (NQ MOTOR)))
            (true))
            nil
            (PREP))                                                 ;; FAIL IF THE CONSTRAINTS AREN'T MET

        ;; PARSE THE PREPOSITION

        (| (PARSE PREP) nil (PREP))
        (MOVE-PT H)
        (TRNSF PLACE TIME)                                          ;; THIS IS NOT EXACTLY RIGHT, SINCE "ON WHAT DAY" IS NOT "PLACE"

        ;; AT THIS POINT THE POSSIBILITIES ARE:
        ;;   1. THERE ARE NO MORE WORDS AND THE PREP IS "SHORT"
        ;;   2. YOU HAVE A MULTIPLE WORD PREPOSITION
        ;;   3. IT IS INDEED A SINGLE WORD PREP, PARSE ITS OBJECT

        (SETQ T1 H)                                                 ;; SAVE THE PREPOSITION JUST PARSED IN CASE IT IS
        (and (NQ PREP2)                                             ;; ONLY THE FIRST WORD OF A MULTIPLE WORD PREPOSITION
            (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N)))
                    (PARSE PREP2))
                ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N) (WORD (cdr N))))
                    (PARSE PREP2)
                    (PARSE PREP2)))
            (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))        ;; CREATE NODE FOR THE COMPOUND WORD
            (SETR 'PARENT C T1))
        (| (ISQ H NEED2) (NEED2) nil)                               ;; FAIL IF LAST PARSED NEEDS ANOTHER WORD

                                                                    ;; GIVE IT A PARENT
        (SETR 'HEAD T1 C)                                           ;; SET THE REGESTER "PREP" TO THE CONSTITUENT JUST
                                                                    ;; PARSED - IF IT WAS A MULTIPLE-WORD-PREP THEN
        (or NN (GO SHORT))                                          ;; "PREP" IS SET TO THE NODE WHICH CONTAINS THE
                                                                    ;; ENTIRE FORM NN POINTS TO WHATEVER WORDS ARE
                                                                    ;; LEFT BEFORE THE CUT POINT

        ;; ADD FEATURES TO THE PREPG DEPENDING ON THE PREPOSITION PARSED

        (COND ((= (WORD H) 'BY) (FQ AGENT)))

    ;; ###################################
    ;; PARSE THE OBJECT TO THE PREPOSITION
    ;; ###################################

    QUEST
        (| (CQ QUEST) nil NG)
                                                                    ;; CERTAIN RESTRICTIONS PLACED ON THE POSSIBLE
                                                                    ;; NOUN GROUPS THAT IT CAN TAKE - HENSE THE
                                                                    ;; SPECIAL CALL TO PARSE AS ABOVE, IF THE PREPG IS
        (| (PARSE NG QUEST OBJ) OBJR (PREPQUEST))                   ;; MARKED WITH THE FEATURE "QUEST" (INDICATING
        (| (and (CQ OF) (PARSE NG OFOBJ)) OBJR nil)                 ;; THAT IT SHOULD CONTAIN THE QUESTION ELEMENT OF

    NG
        (| (PARSE NG OBJ) OBJR nil)                                 ;; THE CLAUSE) THEN WE PARSE IT SPECIALLY SIMPLE

    REL
        (| (NEXTWORD? 'WHICH) nil REST)                             ;; NOUN GROUP - NO RESTRICTIONS
        (| (ISQ (MOVE-PT U) CLAUSE) nil (PREP-WHICH))               ;; IF THE NEXT WORD IS A RELWORD, SUCH AS "WHICH"
                                                                    ;; OR "WHOM", THEN A FAIRLY STRICT SET OF
                                                                    ;; CONSTRAINTS APPLY. THE PREPG IS REQUIRED TO BE
                                                                    ;; WITHIN A RANK-SHIFTED-QUALIFIER CLAUSE (RSQ)
                                                                    ;; WHERE IT CAN APPEAR AT PRACTICLY ANY POINT - "OF
                                                                    ;; WHOM WERE YOU SPEAKING" - "GIVE THE THE
                                                                    ;; MANUSCRIPT, THE LETTERING ON THE COVER OF WHICH
                                                                    ;; IS LARGER THAN PROSCRIBED IN THE GOVERNMENT
                                                                    ;; MANUAL" (HONEST - I HAD ONE OF THOSE IN A
                                                                    ;; LINGUITICS CLASS). IT IS MOST LIKELY THAT THE
                                                                    ;; CONSTRUCTION WILL APPEAR AT THE START OF THE
                                                                    ;; CLAUSE AND THAT ACCORDINGLY, THE NOUNGROUP
                                                                    ;; PROGRAM WHICH SAW IT WILL HAVE INITIALLY ASKED
                                                                    ;; TO PARSE A PREPG, INSTEAD OF THE RSQ WHICH THE
                                                                    ;; CONSTRUCTION ACTUALLY INFERS. BECAUSE OF THIS,
                                                                    ;; PREPG FAILS WITH THE MESSAGE "PREP-WHICH" IF IT
                                                                    ;; IS NOT EXPLICITLY WITHIN AN RSQ. THE FAILURE
                                                                    ;; MESSAGE IS READ BY THE NG PROGRAM WHICH THEN
        (| (ISQ PT PRONREL) nil PRONREL)                            ;; CHANGES ITS REQUEST FROM (PARSE PREPG Q)  TO
        (SETQ MES (cdr MES))                                        ;; (PARSE CLAUSE RSQ PREPREL), WHICH SHOULD PICK
                                                                    ;; UP THE BOTHERSOME PREPG AS AN INITIAL MODIFIER
                                                                    ;; TO THE CLAUSE AND DEAL WITH IT APPROPRIATELY
                                                                    ;; RESET THE FAILURE MESSAGE LIST (WE KNOW TO DO
        (GO P-RELWRD)                                               ;; THIS BECAUSE THE "PRONREL" AS AN INITIAL
                                                                    ;; FEATURE OF THE CLAUSE IMPLICATES THE PASSAGE OF
                                                                    ;; THE PROS CESS DESCRIBED ABOVE)

    PRONREL
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PRONREL PT)

    P-RELWRD
        (PARSE NG RELWD OBJ)
        (SETR 'OBJ1 (GETR 'HEAD PT) C)                              ;; THE REGISTER IS ACCESSED BY CODE IN THE PASSIVE
        (GO RETT)                                                   ;; SECTION OF CLAUSE AND BY THE APPROPRIATE

    REST
        (| (PARSE CLAUSE RSNG ING) OBJR SHORT)                      ;; SEMANTIC SPECIALIST "HEAD" IS HERE THE HEAD OF

    OBJR
        (SETR 'OBJ1 H C)                                            ;; THE HIGHER NOUNGROUP
        (GO RETT)

    SHORT
        (| (MEET FE '(NOSHORT Q)) (SHORT) nil)
        (or (ISQ (MOVE-PT C U) REL-NOT-FOUND)
            (ISQ (GETR 'QUESTION-ELEMENT PT) QADJ)
            (GO FAIL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PREPREL PT)
        (SETR 'OBJ1 (GETR 'RELHEAD (MOVE-PT C U)) C)

    ;; IF THE REFERENT OF THE RELATIVE CLAUSE THIS SHORT
    ;; PREPOSITION IS ASUMED TO BE IN, HAS NOT BEEN DETERMINED,
    ;; THEN SET THE REGISTER FOR THE OBJECT OF THE PREP.  TO THE
    ;; RELWORD.  IF THERE IS NO RELWORD THEN THE PREPG FAILS
    ;; AFTER SENDING UP A COMPLAINING MESSAGE.

    ;; ---------------  FINAL CHECKS, AND RETURN ---------------

    ;; CHECK IF THIS PREPG SHOULD BE MARKED AS CONTAINING A QUESTION ELEMENT.
    ;; IE. "FOR WHAT", "BETWEEN THE RED BLOCK AND WHICH?" (ECHO)

    RETT
        (and (or (ISQ H QUEST)                                      ;; H IS THE NG FOUND FOR AN OBJECT
            (and (ISQ H COMPOUND)                                   ;; IF THE NOUN GROUP IS COUMPOUND, CHECK EACH
                (MOVE-PT H H PV (QUEST))))                          ;; COMPONENT FOR THE FEATURE "QUEST"
            (FQ QUEST))
        (| (CALLSM (SMADJG-PREPG)) RETURN FAIL))

(§ PDEFINE ADJG nil

    ENTERING-ADJG                                                   ;; THIS LABEL IS MARKED BY DEBUGGING ROUTINES AND

    COMPCHECK                                                       ;; IS USEFUL FOR FOLLOWING THE FLOW OF CONTROL
        (| (and (MOVE-PT C U (BE)) (not (CQ COMP))) FAIL nil)       ;; CONDITIONS WHICH MUST BE MET BY ANY ADJECTIVE
                                                                    ;; GROUP IF THERE IS A FORM OF "BE" IN THE HIGHER
                                                                    ;; CLAUSE, THEN THE ADJG SHOULD HAVE BEEN CALLED
                                                                    ;; WITH THE FEATURE "COMP" FOR COMPLIMENT

        ;; EXAMINE THE INITIAL FEATURES (THOSE DESIGNATED BY THE
        ;; CALLING PROGRAM) ALSO EXAMINE THE NEXT WORD - THESE GIVE
        ;; CLUES AND CONSTRAINTS TO THE STRUCTURE TRYING TO BE PARSED
        ;; AND DIRECT JUMPS TO THE APPROPRIATE SECTIONS OF CODE

        (| (ISQ (MOVE-PT C U) THAN) nil DISP)                       ;; THE WORD "THAN" WAS DETECTED BY THE IMMEDIATELY
                                                                    ;; UPSTAIRS NG AS FOLLOWING THE HEAD NOUN
        (SETR 'HEAD (GETR 'COMPARATIVE-MODIFIER PT) C)              ;; INDICATING A STURCTURE SUCH AS "... A BIGGER
        (GO THAN)                                                   ;; BLOCK THAN THAT ONE ..." "HEAD REFERS TO THE
                                                                    ;; ADJG'S HEAD ADJECTIVE

    DISP
        (| (and (NQ AS) (PARSE nil AS)) AS nil (AS))
        (| (and (NQ AS) (PARSE nil AS)) AS nil (AS))
        (| (NEXTWORD? 'HOW) HOW ADV)

    ;; --------------- HOW + ADJG ----------------

    HOW
        (| (and (PARSE nil HOW) (FQ QUEST)) nil FAIL FAIL)
        (| (and (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C)) RETSM nil)
        (| (and (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C)) RETSM FAIL)

    ADV
        (| (PARSE ADV ADVADV) ADV nil POPAD)                        ;; THIS LOOPS UNTILL ALL CONTIG- UOUS ADVERBS HAVE
        (| (PARSE nil MORE) nil ADJ)                                ;; BEEN PARSED "MORE" IS EXPLICITLY CHECKED FOR
        (FQ COMPAR)                                                 ;; SINCE IT SIGNALS THE FEATURE, COMPARATIVE

    ADJ
        (| (COND ((CQ ADV) (PARSE ADV VBAD)) (:else (PARSE ADJ)))
            nil
            (ADJ))                                                  ;; IF THE CUT POINT WAS REACHED THEN NO MORE
        (| (SETR 'HEAD H C) nil nil RETSM)                          ;; PROCESSING (SUCH AS COMPAR BELOW) IS POSSIBLE.

    ;; -------------------------------------------
    ;;               COMPARATIVES
    ;; -------------------------------------------

    ;; IF THE FEATURE "COMPAR" IS ALREADY ON THE LIST, OR IF THE JUST PARSED ADJECTIVE CAN HAVE THAT FEATURE, THEN
    ;; ATTEMPT TO PARSE SOME SORT OF COMPARATIVE CONSTRUCTION (ASSUMING THAT THEREARE ANY MORE WORDS BEFORE THE CUT POINT.)

        (| (or (CQ COMPAR) (ISQ H COMPAR)) nil RETSM)
        (FQ COMPAR)
        (| NN nil RETSM)                                            ;; IF THERE ARE UNPARSED WORDS LEFT BEFORE THE CUT
                                                                    ;; POINT THEN THE POSSIBILITY OF MORE COMPLICATED
                                                                    ;; FORMS IS CHECKED FOR

    THAN
        (COND ((not NN) (GO RETSM)))
        (| (PARSE nil THAN) nil RETSM (THAN))
        (RQ THANNEED)                                               ;; THE FEATURE "THANNEEED" MARKS THAT THE WORD
        (FQ THAN)                                                   ;; "THAN" IS EXPLICITLY  REQUIRED IN THE PHRASE.
        (GO SUBJ)

    AS
        (FQ AS)
        (RQ THANNEED)
        (| (and (PARSE ADJ) (SETR 'HEAD H C)) nil (ADJ) RETSM)
        (| (PARSE nil AS) SUBJ RETSM (AS))

    ;; FIND A SUBJECT FOR THE COMPARATIVE
    ;; IE.  "AS BIG AS ..." , "BIGGER THAN ..."

    SUBJ
        (| (PARSE NG SUBJ COMPAR) nil (THAN))
        (| (SETR 'OBJ1 H C) nil nil RETSM)
        (| (and (one-word-left NB) (PARSE VB AUX)) nil RETSM)
        (| (CHECK-AGREEMENT H (cdr H))                              ;; CHECKS FOR AGREEMENT IN NUMBER AND PERSON
            RETSM                                                   ;; BETWEEN THE NG PARSED AS SUBJ AND THE
            nil)                                                    ;; JUST-PARSED VERB
        (POP)
        (GO RETSM)

    ;; AT PRESENT, THIS ENTIRE ROUTINE IS INADIQUATE IN SEVERAL
    ;; RESPECTS: THE EXISTING BACKUP MECHANISM CORRECTLY REFUSES
    ;; TO PARSE THE "ARE" IN "SOME PEOPLE BIGGER THAN JOHN ARE
    ;; STANDING ..." AS PART OF THE ADJG, BUT DOES SO ONLY BECAUSE
    ;; OF THE DISSAGREEMENT IN NUMBER (CHECKED FOR ABOVE) MORE
    ;; COMPLICATED FORMS (THE VERB IS BY NO MEANS LIMITED TO
    ;; FORMS OF "BE"), IF IT IS PARSABLE AT ALL WITHOUT
    ;; CONSIDERABLE PRINCIPLED MODIFICATION OF THE CODE, IT WILL
    ;; BE CAUGHT BY THE GENERAL CUTTING MECHANISM WHICH REPARSES
    ;; THE SUBJ-NP WHEN THE VERB IS MISSING OR INCOMPATABLE (SEE CLAUSE).

    POPAD
        (POP)                                                       ;; IF THE CUT POINT WAS HIT HAVING ONLY PARSED
        (GO ADJ)                                                    ;; ADVERBS, POP OFF THE FINAL ADV AND TRY TO
                                                                    ;; REPARSE IT AS AN ADJECTIVE

    ;; FINAL CHECKS ON COMPARATIVES (SEMANTIC AND OTHERWISE)

    RETSM
        (| (CQ THANNEED) (THANNEED) nil)                            ;; IF ONE OF THE WORDS PARSED REQUIRED A "THAN",
        (| (CALLSM (SMADJG-PREPG)) RETURN (SMADJ)))                 ;; FAIL IF ONE WAS NOT FOUND.

(§ defn- CONJ []
    (let [END CUT goodie (apply-grammar 'CONJOIN)]
        (when goodie (SETQ RE goodie))))

(§ defn- COMMA []
    (COND ((SECONDWORD? '\") (FLUSHME) true)                           ;; IF " FOLLOWS, FLUSH COMMA AND CONTINUE
        ((CONJ))                                                    ;; IF COMMA IS PART OF CONJOINED STRUCTURE, GREAT
        ((ISQ RE INIT) (FLUSHME) true)))                               ;; IF COMMA FOLLOWS INITIAL-TYPE PHRASE, FLUSH IT AND CONTINUE DIRECT ADDRESS JAZZ

(§ PDEFINE CONJOIN (PREV)

    ;; THIS PROGRAM IS CALLED TO PARSE A CONJOINED STRUCTURE THE
    ;; FIRST MEMBER OF THE SUPPOSED STRUCTURE HAS ALREADY BEEN
    ;; PARSED AND IS SET TO THE INITIAL DAUGHTER (H) OF THIS NODE
    ;; AN ATTEMPT IS MADE TO PARSE AS MANY CONSTITUENTS JUST LIKE
    ;; IT AS IS POSSIBLE

    ENTERING-CONJOIN                                                ;;  HACK LABEL FOR LABELTRACER

    UP
        (SETQ PREV (NEXTWORD))
        (FLUSHME)
        (COND ((and (= PREV '\,)                                   ;; IF WE HAVE A COMMA AND
                (or (cdr H)                                         ;; IF MORE THAN 1 COMPONENT HAS BEEN PARSED
                (> (- (count (NB H))               ;; OR IF THAT ONE COMPONENT
                            (count (N H)))                         ;; IS MORE THAN 4 WORDS LONG
                    4))
                (memq (NEXTWORD) '(OR AND NOR BUT))
                (F (NEXTWORD)))                                     ;; THEN CHECK FOR COMMA COMBINATION
            (SETQ PREV (list PREV (NEXTWORD)))
            (FLUSHME)))
        (and (term? PREV)
            (MOVE-PTW N NW (= (WORD PTW) PREV))
            (CUT PTW))
        (and (or (= PREV 'BUT) (= (cadr PREV) 'BUT))
            (NEXTWORD? 'NOT)                                        ;; CHECK FOR BUT-NOT COMBINATION
            (or (FLUSHME) (GO LOSE2))
            (FQ NEGBUT))
        (| (COND ((memq (car REST) '(ADJ NUM NOUN PREP VB ADV))
                (PARSE3 (concat REST '(COMPONENT)) nil))
            ((memq (car REST) '(NG PREPG ADJG))
                (and (not (CQ OFOBJ)) (PARSE2 (concat REST '(COMPONENT)) nil)))
            ((= (car REST) 'CLAUSE)
                ((lambda [LASTSENT AUXFE]
                    (and (PARSE2 (concat REST AUXFE '(COMPONENT)) nil)
                        (or (not AUXFE) (F (car AUXFE)))
                        (SETR 'TIME (GETR 'TIME H) C)))             ;; MARK COMPOUND CLAUSE AS TO DECLAR/IMPER FOR ANSGEN
                    (COND ((ISQ H MAJOR) H) (LASTSENT))
                    (MEET (FE H) '(DECLAR IMPER)))))
            nil
            LOSE2)
        (CUT END)                                                   ;; RESTORE CUT POINT
        (COND ((not (term? PREV))
                ;; IF WE HAD COMMA FOLLOWED BY (AND OR BUT NOR), RETURN THE LIST OF GOODIES WE'VE FOUND
                (GO RETSM))
            ((= PREV '\,)
                (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP))
                    (:else (GO LIST))))
            ((memq PREV '(AND OR NOR BUT))
                (COND ((= BOTH (NB H)) (FQ BOTH)))
                (COND ((or (NEXTWORD? 'BUT)
                        (and (NEXTWORD? PREV)
                            (not (and (= BOTH (NB H))              ;; IF WE HAD THE 'BOTH' WORD AND
                                (= PREV 'AND)))))                  ;; IF THE 'BOTH' WORD WAS "AND", STOP PARSING
                        (FQ LISTA)                                  ;; ELSE GO LOOK FOR THE NEXT COMPONENT
                        (F PREV)
                        (GO UP))
                    (:else (GO LISTA)))))

    LOSE2
        (| (CQ LISTA) LISTA nil)

    LIST
        (| (and (= PREV '\,)                                       ;; COME HERE FOR ABORTED LIST AND CHECK FOR APPOSITIVE
                (== (count H) 2)
                (ISQ H NG)
                (not (or (ISQ H PRONG) (ISQ (cdr H) PRONG)))
                (or (NEXTWORD? COMMA) (nil? N)))
            nil
            (CONJOINß HOPELESS LIST))
        (FLUSHME)                                                   ;; GET RID OF TRAILING COMMA
        (FQ APPOSITIVE)
        (GO RETSM)

    LISTA
        (F PREV)

    RETSM
        (FQ COMPOUND)                                               ;; CALL SEMANTICS AND RETURN EVERY PARSED BY THIS
        (and (> (count H) 2) (FQ LIST))                     ;; GOODIE IS COMPOUND IF MORE THAN 2 COMPONENTS
        (COND ((or (CQ NG) (CQ NOUN))
                (COND ((CQ AND) (FQ NPL))
                    (:else (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
            ((CQ VB)
                (let [COMMON nil]
                    (SETQ COMMON (get 'VB 'ELIM))
                    (MAP #'(lambda [x]
                            (SETQ COMMON (MEET COMMON (FE x))))
                    H)
                    nil)
                (FESET (UNION COMMON (FE C)) C)))
        (| (CALLSM (SMCONJ)) RETURN (CONJOINß SMCONJ)))             ;; THEN MARK AS A LIST

(defn- doublequoter [] (apply-grammar 'PARSEQUOTED))

(§ defn- CANTAKE [num type feature]
    (let [vbfeat (FE MVB)]
        (COND
            ((memq 'RSNG type)
                (memq (READLIST (concat
                        (COND ((memq 'TO type) '(T O)) ((memq 'ING type) '(I N G)) ((memq 'REPORT type) '(R E P)))
                        '(O B)
                        (list (COND ((== num 1) '\1) (:else '\2)))))
                    vbfeat))
            ((memq 'COMP type)
                (memq 'INT vbfeat))
            ((memq 'NG type)
                (COND ((== num 1)
                        (not (nil? (MEET '(TRANS TRANS2 TRANSL TRANSINT) vbfeat))))
                    (:else (memq 'TRANS2 vbfeat))))
            (:else (memq feature vbfeat)))))

(§ defn- CANPARSE [num type feature]
    (let [REG nil]
        (and (CANTAKE num type feature)
            (or (nil? type)
                (and (APPLY #'PARSE
                    (concat type
                        (COND ((memq 'COMP type) (SETQ REG 'COMP) nil)
                            (:else (list 'OBJ
                                (SETQ REG (COND
                                    ((or (memq 'LOC type) (memq 'PLACE type)) 'LOBJ)
                                    ((== num 1) 'OBJ1)
                                    (:else 'OBJ2))))))))
                    (SETR REG H C)))
            (or (nil? feature) (F feature))
            (RETURN true))
        nil))

#_(ns shrdlu.dictio)

;; ###########################################################
;;
;;                          WORDS
;;
;; ###########################################################

(§ DEFS \, FEATURES (SPECIAL) SPECIAL (COMMA))

(§ DEFS \" FEATURES (B-SPECIAL RELWRD) B-SPECIAL (doublequoter)) ;; "sic!

(§ DEFS A SEMANTICS ((DET true)) FEATURES (DET NS INDEF))

(§ DEFS ABOVE SEMANTICS ((PREP (!LOC !ABOVE true))) FEATURES (PREP PLACE))

(§ DEFS AFTER SEMANTICS ((BINDER (SMBINDER END nil))) FEATURES (BINDER TIME))

(§ DEFS ALL
    SEMANTICS ((DET (COND ((CQ OF) 'ALL) ((MEET '(NUM DEF) FE) 'DEF) ('NDET))))
    FEATURES (DET NPL QNTFR))

(§ DEFS AN IRREGULAR (A nil nil))

(§ DEFS AND FEATURES (SPECIAL) SEMANTICS true SPECIAL (CONJ))

(§ DEFS ANY SEMANTICS ((DET 'INDEF)) FEATURES (DET ANY NS NPL QNTFR))

(§ DEFS ANYTHING SEMANTICS ((TPRON 'INDEF)) FEATURES (TPRON ANY NS))

(§ DEFS ARE IRREGULAR (BE (VPL PRESENT) (INF)))

(§ DEFS AS SEMANTICS ((nil? true)) FEATURES (AS))

(§ DEFS ASK
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!WANT !1 !2 *TIME))))))))
    FEATURES (VB TRANS INF SUBTOB))

(§ DEFS AT SEMANTICS ((NUMD true)) FEATURES (AT))

(§ DEFS AWAY SEMANTICS ((PRT true)) FEATURES (PRT))

(§ DEFS BACK SEMANTICS ((PREP2 true) (NOUN true)) FEATURES (NOUN NS PREP2))

(§ DEFS BALL
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!MANIP !ROUND) PROCEDUREß ((!IS *** !BALL))))))
    FEATURES (NOUN NS))

(§ DEFS BE
    FEATURES (INT AUX VB BE INF)
    SEMANTICS ((VB ((THERE (!BETHERE)) (INT (!BEINT))))))

(§ defn- !BETHERE []
    (RELATION (RESTRICTIONSß (((!THING) (= (quantifier? SMSUB) 'INDEF))) PROCEDUREß nil)))

(§ defn- !BEINT []
    (COND
        ((RELATION
            (RESTRICTIONSß (((!PHYSOB)) (SMCOMP (!PROPERTY)))
                PROCEDUREß (!EVAL (let [PROPERTY nil]
                                    (COND
                                        ((SETQ PROPERTY (MEET (get '!PROPERTY 'SYSTEM) (markers? SMCOMP)))
                                            (RETURN (list (list (car PROPERTY) '!1 '!2))))
                                    ((RETURN (list '(!2 !1))))))))
            (RESTRICTIONSß (((!THING)) (SMCOMP (!SYSTEMS) (and (not (refer? SMCOMP)) (= (rel? SMCOMP) SMSUB))))
                PROCEDUREß (!EVAL (relations? SMCOMP)))
            (RESTRICTIONSß (((!THING)) (SMCOMP (!THING) (refer? SMCOMP)))
                PROCEDUREß ((!EVAL (list 'THAMONG '!1 (list 'quote (refer? !2))))))))
        (:else (ERTSTOP SORRY I DON'T UNDERSTAND THE VERB BE WHEN YOU USE IT LIKE THAT))))

(§ DEFS BEFORE SEMANTICS ((BINDER (SMBINDER nil START))) FEATURES (BINDER TIME))

(§ DEFS BEGIN
    SEMANTICS ((VB
        ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!START !2 *TIME)))))
         (ITRNS (RELATION (RESTRICTIONSß (((!ANIMATE))) MARKERSß (!EVENT) PROCEDUREß ((!START EE *TIME))))))))
    FEATURES (VB TRANS INF TOOB INGOB ITRNS))

(§ DEFS BEGAN IRREGULAR (BEGIN (PAST) (INF)))

(§ DEFS BEHIND
    SEMANTICS ((PREP (!LOC !BEHIND true)))
    FEATURES (PREP PLACE))

(§ DEFS BELOW
    SEMANTICS ((PREP (!LOC !ABOVE nil)))
    FEATURES (PREP PLACE))

(§ DEFS BENEATH
    SEMANTICS ((PREP (!LOC !ABOVE nil)))
    FEATURES (PREP PLACE))

(§ DEFS BESIDE
    SEMANTICS ((PREP (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PROCEDUREß ((!NEXTO !1 !2 *TIME))))))
    FEATURES (PREP PLACE))

(§ DEFS BIG
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !BIG) PROCEDUREß ((!MORE !SIZE *** (128 128 128)))))))
    FEATURES (ADJ))

(§ DEFS BLACK SEMANTICS ((ADJ (!COLOR !BLACK))) FEATURES (ADJ))

(§ DEFS BLOCK
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!MANIP !RECTANGULAR) PROCEDUREß ((!IS *** !BLOCK))))))
    FEATURES (NOUN NS))

(§ DEFS BLUE SEMANTICS ((ADJ (!COLOR !BLUE))) FEATURES (ADJ))

(§ DEFS BOTH
    SEMANTICS ((DET 'DEF))
    FEATURES (B-SPECIAL QNTFR DET DEF NPL BOTH)
    B-SPECIAL (BOTH AND))

(§ defq- BOTH [A]
    ;; HANDLES (BOTH AND) (EITHER OR) (NEITHER NOR) COMBINATIONS
    ;; THE CONJOIN PROGRAM DOES SPECIAL THINGS WHEN BOTH IS SET
    (let [END CUT]                                                ;; MAKE END OUT OF PREVIOUS CUT POINT
        (let [CUT nil nbb N BOTH nil]
            (and (FLUSHME)
                (MOVE-PTW N NW (= (WORD PTW) (car A)) NW)         ;; LOOK FOR THE MATCHING WORD E.G. AND, OR, NOR
                (CUT END)
                (SETQ BOTH PTW)                                     ;; SAVE POINTER TO THE WORD AFTER THE MATCHING WORD
                (SETQ RE (COND
                    ((memq (car REST) '(PREP ADV)) (PARSE3 REST true))
                    ((memq (car REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST true))))
                (< (count N) (count BOTH))                          ;; FAIL UNLESS WE PARSED BEYOND MATCHING WORD
                (RETURN (SETQ SPECIAL 'SKIP)))
            (SETQ RE nil)
            (SETQ N nbb)
            nil)))

(§ DEFS BOX
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!BOX) PROCEDUREß ((!IS *** !BOX))))))
    FEATURES (NOUN NS))

(§ DEFS BRICK FEATURES (NOUN NS))

(§ DEFS BUILD
    SEMANTICS ((VB ((TRANS (!BUILD)))))
    FEATURES (VB INF TRANS))

(§ DEFS BUT FEATURES (SPECIAL) SEMANTICS true SPECIAL (CONJ))

(§ DEFS BY
    SEMANTICS ((PREP (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PROCEDUREß ((!NEXTO !1 !2 *TIME))))))
    FEATURES (PREP))

(§ DEFS CALL
    SEMANTICS ((VB ((TRANS2 (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!THING)) ((!NAME))) PROCEDUREß ((!CALL !2 !3 *TIME))))))))
    FEATURES (VB INF TRANS2))

(§ DEFS CAN SEMANTICS ((VB true)) FEATURES (V3PS VFS VPL VB MODAL AUX))

(§ DEFS CHOOSE
    SEMANTICS ((VB ((TRANS (!NOTICE)))))
    FEATURES (VB INF TRANS))

(§ DEFS CLEAN SEMANTICS ((VB true)) FEATURES (VB INF VPRT TRANS))

(§ DEFS CLEAN-OFF
    ROOT (CLEAN OFF)
    SEMANTICS ((TRANS (!CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(§ DEFS CLEAN-OUT
    ROOT (CLEAN OUT)
    SEMANTICS ((TRANS (!CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(§ DEFS CLEAN-UP
    ROOT (CLEAN UP)
    SEMANTICS ((TRANS (!CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(§ DEFS CLEAR SEMANTICS ((VB true)) FEATURES (VB INF VPRT TRANS))

(§ DEFS CLEAR-OFF
    ROOT (CLEAR OFF)
    SEMANTICS ((TRANS (!CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(§ DEFS CLEAR-OUT
    ROOT (CLEAR OUT)
    SEMANTICS ((TRANS (!CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(§ DEFS COLOR
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!COLOR) PROCEDUREß ((!IS *** !COLOR))))))
    FEATURES (NOUN NS))

(§ DEFS CONSTRUCT
    SEMANTICS ((VB ((TRANS (!BUILD)))))
    FEATURES (VB INF TRANS))

(§ DEFS CONTAIN
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONSß (((!BOX)) ((!PHYSOB))) PROCEDUREß ((!CONTAIN !1 !2 *TIME)))
                (RESTRICTIONSß (((!CONSTRUCT)) ((!THING))) PROCEDUREß ((!PART !2 !1 *TIME))))))))
    FEATURES (VB INF TRANS))

(§ DEFS CONTAINER
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!BOX) PROCEDUREß ((!IS *** !BOX))))))
    FEATURES (NOUN NS))

(§ DEFS CORNER FEATURES (NOUN NS))

(§ DEFS CUBE
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!MANIP !RECTANGULAR) PROCEDUREß ((!IS *** !BLOCK) (!EQDIM ***))))))
    FEATURES (NOUN NS))

(§ DEFS DID IRREGULAR (DO (PAST V3PS) (INF PRESENT)))

(§ DEFS DO
    SEMANTICS ((VB
        ((TRANS (RELATION
            (RESTRICTIONSß RESTRICTIONSß
            PROCEDUREß ((((!ANIMATE)) ((!EVENT))))
            MARKERSß PROCEDUREß
            PLAUSIBILITYß (!EVAL (or (get MAP2 'REFER) (ert "DO DEFINITION")))))))))
    FEATURES (TRANS VFS PRESENT VPL VB AUX DO INF))

(§ DEFS DOES IRREGULAR (DO (V3PS) (VFS VPL INF)))

(§ DEFS DOWN SEMANTICS ((PRT true)) FEATURES (PRT))

(§ DEFS DROP
    SEMANTICS ((VB
        ((TRANSL (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP)) (SMOBL (!PLACE *TIME))) PROCEDUREß ((!DROP !1 !2 !3)) MARKERSß ((!MOTION)))))
         (TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!DROP !1 !2 PLACE *TIME)) MARKERSß ((!MOTION))))))))
    FEATURES (TRANSL TRANSL2 VB INF TRANS))

(§ DEFS EACH SEMANTICS ((DET 'ALL)) FEATURES (DET NS QNTFR))

(§ DEFS EITHER FEATURES (B-SPECIAL) SEMANTICS true B-SPECIAL (BOTH OR))

(§ DEFS EVERY SEMANTICS ((DET 'ALL)) FEATURES (DET NS QNTFR))

(§ DEFS EVERYTHING SEMANTICS ((TPRON 'ALL)) FEATURES (TPRON NS))

(§ DEFS EXACTLY
    SEMANTICS ((NUMD (list 'EXACTLY NUM)))
    FEATURES (NUMD NUMDALONE))

(§ DEFS FEW
    SEMANTICS ((NUMD (list '< (inc NUM))))
    FEATURES (NUMD NUMDAS))

(§ DEFS FEWER
    SEMANTICS ((NUMD (list '< NUM)))
    FEATURES (NUMD NUMDAN))

(§ DEFS FIND
    SEMANTICS ((VB ((TRANS (!NOTICE)))))
    FEATURES (VB INF TRANS))

(§ DEFS FINISH
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!END !2 *TIME))))))))
    FEATURES (VB INF TRANS INFOB))

(§ DEFS FIVE SEMANTICS ((NUM 5)) FEATURES (NUM))

(§ DEFS FOUR SEMANTICS ((NUM 4)) FEATURES (NUM))

(§ DEFS FRIEND REFER ßFRIEND FEATURES (NOUN NS))

(§ DEFS FROM FEATURES (PREP))

(§ DEFS FRONT SEMANTICS ((NOUN true) (PREP2 true)) FEATURES (NOUN NS PREP2))

(§ DEFS GAVE IRREGULAR (GIVE (PAST) (INF)))

(§ DEFS GIVE
    SEMANTICS ((VB ((TRANS2 (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!GIVE !1 !2 !3 *TIME))))))))
    FEATURES (VB INF TRANS2))

(§ DEFS GO FEATURES (ITRNS VB INF))

(§ DEFS GOING FEATURES (VB ITRNS ING))

(§ DEFS GRAB
    SEMANTICS ((VB ((TRANS (!GRASP)))))
    FEATURES (VB TRANS INF))

(§ DEFS GRASP
    SEMANTICS ((VB ((TRANS (!GRASP)))))
    FEATURES (VB TRANS INF))

(§ DEFS GREATER
    SEMANTICS ((NUMD (list '> NUM)))
    FEATURES (NUMD NUMDAN))

(§ DEFS GREEN SEMANTICS ((ADJ (!COLOR !GREEN))) FEATURES (ADJ))

(§ DEFS HAD IRREGULAR (HAVE (PAST) (INF)))

(§ DEFS HAND
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!HAND) PROCEDUREß ((!IS *** !HAND))))))
    FEATURES (NOUN NS))

(§ DEFS HANDLE
    SEMANTICS ((VB ((TRANS (!GRASP)))))
    FEATURES (VB INF TRANS))

(§ DEFS HAS IRREGULAR (HAVE (V3PS PRESENT) (INF)))

(§ DEFS HAVE
    SEMANTICS ((VB ((TRANS (!HAVE)))))
    FEATURES (HAVE VB AUX INF TRANS))

(§ DEFS HIGH
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !HEIGHT RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!HIGH ***))))))
    FEATURES (ADJ))

(§ DEFS HOLD
    SEMANTICS ((VB
        ((TRANS (RELATION
              (RESTRICTIONSß (((!HAND)) ((!MANIP))) PROCEDUREß ((!GRASPING !2 *TIME)))
              (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) PROCEDUREß ((!GRASPING !2 *TIME))))))))
    FEATURES (VB INF TRANS))

(§ DEFS HE FEATURES (PRON NS SUBJ))

(§ DEFS HER IRREGULAR (SHE (OBJ POSS) (SUBJ)))

(§ DEFS HIM IRREGULAR (HE (OBJ) (SUBJ)))

(§ DEFS HIS FEATURES (PRON POSS))

(§ DEFS HOW SEMANTICS ((QADJ true)) FEATURES (QADJ))

(§ DEFS HOWEVER FEATURES (PRON EVERPRON))

(§ DEFS I
    SEMANTICS ((PRON (SMSET (list (NEWCOPY 'FRIEND-OSS)))))
    FEATURES (SUBJ PRON NFS))

(§ DEFS IF FEATURES (BINDER))

(§ DEFS IN SEMANTICS ((PREP (!IN))) FEATURES (ADV PLACE PREP PLACE))

(§ DEFS IN-BACK-OF
    ROOT (IN BACK OF)
    SEMANTICS (!LOC !BEHIND true)
    FEATURES (PREP COMBINATION))

(§ DEFS IN-FRONT-OF
    ROOT (IN FRONT OF)
    SEMANTICS (!LOC !BEHIND nil)
    FEATURES (PREP COMBINATION))

(§ DEFS INSIDE SEMANTICS ((PREP (!IN))) FEATURES (PREP PLACE))

(§ DEFS INSIDE-OF
    ROOT (INSIDE OF)
    SEMANTICS (!IN)
    FEATURES (PREP COMBINATION))

(§ DEFS INTO SEMANTICS ((PREP (!IN))) FEATURES (PREP PLACE))

(§ DEFS IS IRREGULAR (BE (V3PS PRESENT) (INF)))

(§ DEFS IT
    SEMANTICS ((PRON (SMIT 'IT)))
    FEATURES (PRON NS SUBJ OBJ))

(§ DEFS ITS IRREGULAR (IT (POSS) nil))

(§ DEFS KNOW FEATURES (VB INF TRANS REPOB))

(§ DEFS LARGE
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !BIG) PROCEDUREß ((!MORE !SIZE *** (128 128 128)))))))
    FEATURES (ADJ))

(§ DEFS LEAST
    SEMANTICS ((NUMD (list '> (dec NUM))))
    FEATURES (NUMD NUMDAT))

(§ DEFS LEFT
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!DIRECTION) PROCEDUREß ((!DIRECTION !RIGHT nil))))))
    FEATURES (NOUN NS))

(§ DEFS LESS
    SEMANTICS ((NUMD (list '< NUM)))
    FEATURES (NUMD NUMDAN))

(§ DEFS LIKE
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!THING))) PROCEDUREß ((!LIKE !1 !2))))))))
    FEATURES (VB INF TRANS))

(§ DEFS LIST SEMANTICS ((VB ((TRANS (!NAME))))) FEATURES (VB VO TRANS))

(§ DEFS LITTLE
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !LITTLE) PROCEDUREß ((!MORE !SIZE (128 128 128) ***))))))
    FEATURES (ADJ))

(§ DEFS LONG
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !LENGTH RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !LENGTH *** (128 128 128)))))))
    FEATURES (ADJ))

(§ DEFS MAKE
    SEMANTICS ((VB ((TRANS (!BUILD)))))
    FEATURES (VB INF TRANS))

(§ DEFS MANY
    SEMANTICS ((NUMD (list '> (dec NUM))) (DET true))
    FEATURES (DET QNTFR NPL NONUM NUMD NUMDAS))

(§ DEFS ME IRREGULAR (I (OBJ) (SUBJ)))

(§ DEFS MORE
    SEMANTICS ((NUMD (list '> NUM)))
    FEATURES (NUMD NUMDAN))

(§ DEFS MOST
    SEMANTICS ((NUMD (list '< (inc NUM))))
    FEATURES (NUMD NUMDAT DET QNTFR NPL NONUM))

(§ DEFS MOVE
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) PROCEDUREß ((!PUT !2 PLACE *TIME)) MARKERSß ((!MOTION))))))))
    FEATURES (VB INF TRANS))

(§ DEFS MY IRREGULAR (I (POSS) (SUBJ)))

(§ DEFS NAME
    SEMANTICS ((NOUN (OBJECT ((!NAME !ROLE) ((IS *** !NAME) (!CALL ? ***) (!ROLE (!THING) (!CALL !2 !1))))))
           (VB ((TRANS (!NAME)))))
    FEATURES (NOUN NS VB INF TRANS))

(§ DEFS NARROW
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !WIDTH (128 0 0) ***)))))
            (MEASURE (MEASURE DIMENSIONß !WIDTH RESTRICTIONSß (!PHSYOB) DIRECTIONß nil)))
    FEATURES (ADJ))

(§ DEFS NEITHER FEATURES (B-SPECIAL) SEMANTICS true B-SPECIAL (BOTH NOR))

(§ DEFS NICE
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!THING) PROCEDUREß ((!LIKE ßFRIEND ***))))))
    FEATURES (ADJ))

(§ DEFS NO SEMANTICS ((DET 'NO)) FEATURES (DET QNTFR NS NPL))

(§ DEFS NONE
    SEMANTICS ((DET 'NO))
    FEATURES (DET QNTFR NPL NS NONUM))

(§ DEFS NOR FEATURES (SPECIAL) SEMANTICS true SPECIAL (CONJ))

(§ DEFS NOT SEMANTICS ((ADV true)) FEATURES (ADV NEG))

(§ DEFS NOTHING SEMANTICS ((TPRON 'NO)) FEATURES (TPRON NEG NS))

(§ DEFS NOW
    SEMANTICS ((ADV (or (= (cadr (assq 'TIME FE)) 'ßNOW) (ert "NOW DEFINITION"))))
    FEATURES (ADV TIMW))

(§ DEFS OBJECT
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!PHYSOB !VAGUE) PROCEDUREß ((!PHYSOB ***))))))
    FEATURES (NOUN NS))

(§ DEFS OF
    SEMANTICS ((PREP
        (and (CQ NG)
            (RELATION
                (RESTRICTIONSß (((!DIRECTION)) ((!PHYSOB)))
                PROCEDUREß ((!EVAL (list '!DIRECTION
                                (cadr (SETQ XX (or (assq '!DIRECTION (cddaar (INTERP MAP1))) (ert "OF DEFINITION"))))
                                (COND ((caddr XX) '*OF) ('!2)) (COND ((caddr XX) '!2) ('*OF)) '*TIME)))))))
           (PREP2 true))
    FEATURES (PREP PREP2 OF))

(§ DEFS OFF SEMANTICS ((PRT true)) FEATURES (PRT))

(§ DEFS ON SEMANTICS ((PREP (!ON))) FEATURES (PREP PLACE))

(§ DEFS ON-TOP-OF
    ROOT (ON TOP OF)
    SEMANTICS (!ON)
    FEATURES (PREP COMBINATION))

(§ DEFS ONE SEMANTICS ((NOUN (SMONE)) (NUM 1)) FEATURES (NUM NOUN NS))

(§ DEFS ONLY
    SEMANTICS ((NUMD (list 'EXACTLY NUM)))
    FEATURES (NUMD NUMDALONE))

(§ DEFS ONTO SEMANTICS ((PREP (!ON))) FEATURES (PREP PLACE))

(§ DEFS OR FEATURES (SPECIAL) SEMANTICS true SPECIAL (CONJ))

(§ DEFS OUT SEMANTICS ((PRT true)) FEATURES (PRT))

(§ DEFS OUT-OF
    ROOT (OUT OF)
    SEMANTICS (!OUTOF)
    FEATURES (PREP COMBINATION))

(§ DEFS OVER SEMANTICS ((PREP (!LOC !ABOVE true))) FEATURES (PREP PLACE))

(§ DEFS PICK
    SEMANTICS ((VB ((TRANS (!NOTICE)))))
    FEATURES (VPRT VB INF TRANS))

(§ DEFS PICK-UP
    ROOT (PICK UP)
    SEMANTICS ((TRANS (RELATION
            (RESTRICTIONSß (((!ANIMATE)) ((!MANIP)))
             MARKERSß (!EVENT)
             PROCEDUREß ((!EVAL (COND ((memq (num? SMOB1) '(1 NS)) '(!PICKUP !2 *TIME)) ('(!PUTIN !2 ßBOX *TIME)))))))))
    FEATURES (COMBINATION TRANS))

(§ DEFS PLEASE FEATURES (B-SPECIAL) SEMANTICS true B-SPECIAL (FLUSHME))

(§ DEFS POINTED
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!PHYSOB !POINTED) PROCEDUREß ((!SHAPE *** !POINTED))))))
    FEATURES (ADJ))

(§ DEFS PUT
    PAST PUT
    SEMANTICS ((VB
        ((TRANSL (RELATION
              (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB)) (SMOBL (!PLACE)))
               MARKERSß (!EVENT)
               PROCEDUREß (!EVAL
                   (doall (map #'(lambda [%PLNRPHRASE]
                        (COND ((= (car %PLNRPHRASE) '!ON) (list '!PUTON '!2 (cadr %PLNRPHRASE) '*TIME))
                            ((= (car %PLNRPHRASE) '!IN) (list '!PUTIN '!2 (cadr %PLNRPHRASE) '*TIME))
                            ((ert "PUT DEFINITION"))))
                        (relations? SMOBL))))))))))
    FEATURES (INF PAST VB TRANSL VPRT))

(§ DEFS PUT-AWAY
    ROOT (PUT AWAY)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTIN !2 ßBOX *TIME))))))
    FEATURES (COMBINATION TRANS))

(§ DEFS PUT-DOWN
    ROOT (PUT DOWN)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTON !2 ßTABLE *TIME))))))
    FEATURES (COMBINATION TRANS))

(§ DEFS PUT-TOGETHER
    ROOT (PUT TOGETHER)
    SEMANTICS ((TRANS (!BUILD)))
    FEATURES (COMBINATION TRANS))

(§ DEFS PYRAMID
    FEATURES (NOUN NS)
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!PHYSOB !POINTED) PROCEDUREß ((!IS *** !PYRAMID)))))))

(§ DEFS RED SEMANTICS ((ADJ (!COLOR !RED))) FEATURES (ADJ))

(§ DEFS RELEASE FEATURES (VB TRANS INF))

(§ DEFS RIGHT
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!DIRECTION) PROCEDUREß ((!DIRECTION !RIGHT true))))))
    FEATURES (NOUN NS))

(§ DEFS ROUND
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!PHYSOB !ROUND) PROCEDUREß ((!SHAPE *** !ROUND))))))
    FEATURES (ADJ))

(§ DEFS SAW IRREGULAR (SEE (PAST) (INF)))

(§ DEFS SEE FEATURES (VB INF TRANS))

(§ DEFS SET SEMANTICS ((VB true)) FEATURES (VB INF))

(§ DEFS SET-DOWN
    ROOT (SET DOWN)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTON !2 ßTABLE *TIME))))))
    FEATURES (COMBINATION TRANS))

(§ DEFS SHAPE
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!SHAPE) PROCEDUREß ((!IS *** !SHAPE))))))
    FEATURES (NOUN NS))

(§ DEFS SHE FEATURES (PRON SUBJ NS))

(§ DEFS SHORT
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !HEIGHT RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !HEIGHT (128 0 0) ***))))))
    FEATURES (ADJ))

(§ DEFS SHRDLU REFER ßSHRDLU)

(§ DEFS SINCE SEMANTICS ((BINDER (SMBINDER END nil))) FEATURES (BINDER TIME))

(§ DEFS SIT
    SEMANTICS ((VB
        ((ITRNSL (RELATION
              (RESTRICTIONSß (((!PHYSOB)) (SMOBL (!PLACE)))
               PROCEDUREß (!EVAL
                   (doall (map #'(lambda [%PLNRPHRASE]
                        (COND ((memq (car %PLNRPHRASE) '(!ON !IN)) (list '!SUPPORT (cadr %PLNRPHRASE) '!1 '*TIME))
                            ((ert "SIT DEFINITION"))))
                        (relations? SMOBL))))))))))
    FEATURES (VB INF ITRNSL))

(§ DEFS SIZE
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!SIZE) PROCEDUREß ((!IS *** !SIZE))))))
    FEATURES (NOUN NS))

(§ DEFS SMALL
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !LITTLE) PROCEDUREß ((!MORE !SIZE (128 128 128) ***))))))
    FEATURES (ADJ))

(§ DEFS SOME
    SEMANTICS ((DET 'INDEF))
    FEATURES (DET QNTFR NS NPL NONUM))

(§ DEFS SOMETHING SEMANTICS ((TPRON 'INDEF)) FEATURES (TPRON NS))

(§ DEFS SPHERE FEATURES (NOUN NS))

(§ DEFS SQUARE
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!PHYSOB !RECTANGULAR) PROCEDUREß ((!SHAPE ** !RECTANGULAR))))))
    FEATURES (ADJ))

(§ DEFS STACK
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!STACK) PROCEDUREß ((!IS *** !STACK)))))
           (VB ((TRANS (!STACKUP)))))
    FEATURES (NOUN NS VB INF VPRT TRANS))

(§ DEFS STACK-UP
    ROOT (STACK UP)
    SEMANTICS ((TRANS (!STACKUP)))
    FEATURES (COMBINATION TRANS))

(§ DEFS START
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!START !2 *TIME))))))))
    FEATURES (VB INF TRANS INGOB1 TOOB1))

(§ DEFS SUPPORT
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!PHYSOB !ROLE) PROCEDUREß ((!SUPPORT *** ?) (!ROLE (!PHYSOB) (!SUPPORT !1 !2))))))
           (VB ((TRANS (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!MANIP))) PROCEDUREß ((!SUPPORT !1 !2 *TIME))))))))
    FEATURES (VB INF TRANS IMPERF NOUN NS))

(§ DEFS TABLE
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!TABLE) PROCEDUREß ((!IS *** !TABLE))))))
    FEATURES (NOUN NS))

(§ DEFS TAKE FEATURES (VB INF TRANSL TRANS))

(§ DEFS TALL
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !HEIGHT RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !HEIGHT *** (128 0 0)))))))
    FEATURES (ADJ))

(§ DEFS TELL
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!WANT !1 !2 *TIME))))))))
    FEATURES (VB INF TRANS2 TOOB2))

(§ DEFS THAN SEMANTICS ((nil? true)) FEATURES (THAN))

(§ DEFS THANK FEATURES (B-SPECIAL) SEMANTICS  (THANK)B-SPECIAL (THANK))

(§ defn- THANK []
    (COND ((= (cadr N) 'YOU)
        (SAY YOU'RE WELCOME)
        (FLUSHME)
        (FLUSHME)
        (or NN (ß_G))
        (SETQ SPECIAL 'DONE))))

(§ DEFS THAT
    SEMANTICS ((PRONREL true) (DET (SMTHAT)) (nil? true))
    FEATURES (NS THAT DET DEM DEF PRONREL INCOM))

(§ DEFS THE SEMANTICS ((DET true)) FEATURES (DET NPL NS DEF))

(§ DEFS THEIR IRREGULAR (THEY (POSS) nil))

(§ DEFS THEM IRREGULAR (THEY (OBJ) (SUBJ)))

(§ DEFS THEN
    SEMANTICS ((ADV
        (and LASTIME
             (RPLACD (cddadr (or (and (SETQ XX (assq 'TIME FE)) (not (term? (cadr XX))) XX) '(TIME (!TIME (PAST) nil))))
                 (list (or (cadddr LASTIME) (car (cddddr LASTIME))) (or (car (cddddr LASTIME)) (cadddr LASTIME)))))))
    FEATURES (ADV TIMW))

(§ DEFS THERE SEMANTICS ((ADV true)) FEATURES (ADV PLACE))

(§ DEFS THEY
    SEMANTICS ((PRON (SMIT 'THEY)))
    FEATURES (PRON SUBJ NPL))

(§ DEFS THICK
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !THICKNESS *** (0 128 0))))))
            (MEASURE (MEASURE DIMENSIONß !THICKNESS RESTRICTIONSß (!PHYSOB) DIRECTIONß true)))
    FEATURES (ADJ))

(§ DEFS THIN
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !THICKNESS (0 128 0) ***)))))
           (MEASURE (MEASURE DIMENSIONß !THICKNESS RESTRICTIONSß (!PHYSOB) DIRECTIONß nil)))
    FEATURES (ADJ))

(§ DEFS THING
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!THING !VAGUE !PHYSOB) PROCEDUREß ((!PHYSOB  *** ))))))
    FEATURES (NOUN NS))

(§ DEFS THIS FEATURES (NS DET DEM DEF))

(§ DEFS THREE SEMANTICS ((NUM 3)) FEATURES (NUM))

(§ DEFS TIME FEATURES (NOUN NS TIM1))

(§ DEFS TO
    SEMANTICS ((PREP (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!DIRECTION))) PROCEDUREß ((!EVAL (SUBTOP '!1 '*OF (REFERENCE? SMOB1))))))))
    FEATURES (PREP))

(§ DEFS TOGETHER SEMANTICS ((PRT true)) FEATURES (PRT))

(§ DEFS TOLD IRREGULAR (TELL (PAST) (INF)))

(§ DEFS TOP SEMANTICS ((PREP2 true)) FEATURES (PREP2))

(§ DEFS TOUCH
    SEMANTICS ((VB ((TRANS (!GRASP)))))
    FEATURES (VB INF TRANS))

(§ DEFS TOY
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MANIP ***))))))
    FEATURES (NOUN NS))

(§ DEFS TWO SEMANTICS ((NUM 2)) FEATURES (NUM))

(§ DEFS UNDER
    SEMANTICS ((PREP (!LOC !ABOVE nil)))
    FEATURES (PREP PLACE))

(§ DEFS UNDERNEATH
    SEMANTICS ((PREP ((!LOC !ABOVE nil))))
    FEATURES (PREP PLACE))

(§ DEFS UP SEMANTICS ((PRT true)) FEATURES (PRT))

(§ DEFS US IRREGULAR (WE (OBJ) (SUBJ)))

(§ DEFS WANT
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!WANT !1 !2 *TIME))))))))
    FEATURES (VB INF TRANS TOOB SUBTOB))

(§ DEFS WAS IRREGULAR (BE (V3PS VFS PAST) (INF)))

(§ DEFS WE
    SEMANTICS ((PRON (SMSET (list (NEWCOPY 'WE-OSS)))))
    FEATURES (PRON NPL SUBJ))

(§ DEFS WERE IRREGULAR (BE (VPL PAST) (INF)))

(§ DEFS WHAT
    SEMANTICS ((DET true) (PRON (SMSET (list (NEWCOPY 'UNKNOWN-OSS)))))
    FEATURES (QDET DET NPL PRON QPRON NS))

(§ DEFS WHATEVER FEATURES (PRON EVERPRON NS))

(§ DEFS WE REFER (ßSHRDLU ßFRIEND))

(§ DEFS WHERE SEMANTICS ((QADJ (FQ WHERE))) FEATURES (QADJ PLACE))

(§ DEFS WHEREVER FEATURES (PRON EVERPRON NS))

(§ DEFS WHEN
    SEMANTICS ((BINDER (SMBINDER START END)) (QADJ (FQ WHEN)))
    FEATURES (QADJ BINDER TIME))

(§ DEFS WHENEVER FEATURES (BINDER))

(§ DEFS WHICH
    SEMANTICS ((PRONREL true) (DET true))
    FEATURES (QDET DET PRONREL NS NPL))

(§ DEFS WHICHEVER FEATURES (DET RSQDET NS NPL))

(§ DEFS WHILE SEMANTICS ((BINDER (SMBINDER START END))) FEATURES (BINDER TIME))

(§ DEFS WHITE SEMANTICS ((ADJ (!COLOR !WHITE))) FEATURES (ADJ))

(§ DEFS WHO
    SEMANTICS ((PRONREL true) (PRON (SMSET (list (NEWCOPY ANIMATE-OSS)))))
    FEATURES (PRONREL QPRON PRON NS))

(§ DEFS WHOEVER FEATURES (PRON EVERPRON NS))

(§ DEFS WHOSE FEATURES (DET QDET NPL NS))

(§ DEFS WHY SEMANTICS ((QADJ (FQ WHY))) FEATURES (QADJ))

(§ DEFS WHYEVER FEATURES (PRON EVERPRON NS))

(§ DEFS WIDE
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !WIDTH *** (0 128 0))))))
            (MEASURE (MEASURE DIMENSIONß !WIDTH RESTRICTIONSß (!PHYSOB) DIRECTIONß true)))
    FEATURES (ADJ))

(§ DEFS WILL SEMANTICS ((VB true)) FEATURES (VB AUX WILL MODAL V3PS VFS VPL))

(§ DEFS WITH FEATURES (PREP))

(§ DEFS WOULD SEMANTICS ((VB true)) FEATURES (VB AUX MODAL))

(§ DEFS YOU
    SEMANTICS ((PRON (SMSET (list (NEWCOPY 'SHRDLU-OSS)))))
    FEATURES (PRON NPL NS SUBJ OBJ))

(§ DEFS YOUR IRREGULAR (YOU (POSS) nil))

;; ############################################################
;;
;;                          !WORDS
;;
;; ############################################################

(§ DEFS !ANIMATE SYSTEM (!ROBOT !PERSON) SYS (!THING))

(§ DEFS !ASMUCH THMLIST ((4 '((THUSE TC-ASMUCH)))))

(§ DEFS !BELONG THMLIST ((3 '((THUSE TC-BELONG)))))

(§ DEFS !BLACK SYS (!SPECTRUM))

(§ DEFS !BLUE SYS (!SPECTRUM))

(§ defn- !BLUEPRINT [x]
    (let [PARTS nil]
        (COND ((get x 'REFER) (RETURN '!2))
            ((nil? (SETQ x (cddaar (INTERP x))))
                (GO DONE)))
    LOOP (COND ((not (= (caar x) 'THGOAL)) (ert "!BLUEPRINT THGOAL"))
            ((= (caadar x) '!IS))
            ((= (caadar x) '!PART) (SETQ PARTS (cons (cadr (cadar x)) PARTS)))
            ((ert "!BLUEPRINT")))
        (and (SETQ x (cdr x)) (GO LOOP))
    DONE (and PARTS
            (get (car PARTS) 'REFER)
            (RETURN (get (car PARTS) 'REFER)))
        (PUTPROP 'BLUEPRINT
            (COND ((nil? PARTS) (get 'STACKPARTS 'SM))
                ((cdr PARTS) (ert "!BLUEPRINT PARTS"))
                ((get (car PARTS) 'SM)))
            'SM)
        'BLUEPRINT))

(§ DEFS !BOX SYS (!PHYSOB))

(§ defn- !BUILD []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!STACK))) MARKERSß (!EVENT) PROCEDUREß ((!EVAL (list '!STACKUP (!BLUEPRINT SMOB1) '*TIME))))))

(§ DEFS !CALL THMLIST ((3 '((THUSE TC-3)))))

(§ DEFS !COLOR
    PRIORITY 192
    SYS (!PROPERTY))

(§ defq- !COLOR [A]
    (eval (SUBST (car A) 'COLOR '(OBJECT (MARKERSß (!PHYSOB COLOR) PROCEDUREß ((!COLOR *** COLOR)))))))

(§ DEFS !CONSTRUCT SYSTEM (!STACK !ROW) SYS (!PHYSOB))

(§ DEFS !CONTAIN PRIORITY -1)

(§ defn- !CLEANOFF []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!CLEARTOP !2 *TIME)))))

(§ DEFS !CLEARTOP
    NAMEVENT (I3 (cons (VBFIX 'CLEAN false) (prtput 'OFF OBJ1)))
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(§ defn- !DEFINE [x y]
    (list '!DEFINITION
        (cadadr (cdaar (INTERP x)))
        (let [x (MAKESYM 'ATM)]
            (PUTPROP x (INTERP y) 'NEWWORD)
            (RETURN x))))

(§ DEFS !DEFINITION
    NOGOAL true)

(§ defq- !DEFINITION [A]
    (PUTPROP (cadar A) '(NOUN NS) 'WORD)
    (PUTPROP (cadar A)
        (SUBST (SUBST '*** (caddr (get (cadr A) 'NEWWORD)) (car (get (cadr A) 'NEWWORD)))
            'NG
            '((NOUN (SETQ LIST2 (list (SUBST (SUBST (caddar LIST1) '*** 'NG) (caar LIST1) (car LIST1)))))))
        'SMNTC))

(§ DEFS !DIRECTION NOGOAL true)

(§ DEFS !END
    THMLIST ((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(§ DEFS !EQDIM
    NOGOAL true)

(§ defn- !EQDIM [x]
    (SETQ x (size x))
    (and (= (car x) (cadr x)) (= (car x) (caddr x))))

(§ DEFS !EQUIV PRIORITY 512)

(§ DEFS !EVENT SYS (!SYSTEMS))

(§ DEFS !EXISTS
    THMLIST ((2 '((THUSE TC-EXISTS))) (3 '((THUSE TCT-EXISTS)))))

(§ DEFS !GET-RID-OF
    THMLIST ((2 '((THUSE TCT-EXISTS))) (3 '((THUSE THUSE TCT-3))) (4 '((THUSE TCTE-4))))
    NAMEVENT (I3 (concat (list (VBFIX 'GET true) 'RID 'OF) OBJ1)))

(§ DEFS !GRASP
    NAMEVENT (I3 (cons (VBFIX 'GRASP false) OBJ1))
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(§ defn- !GRASP []
    (RELATION
        (RESTRICTIONSß (((!ANIMATE)) ((!MANIP)))
        MARKERSß (!EVENT)
        PROCEDUREß ((!EVAL (COND ((IMPERF) '(!GRASPING !2 *TIME)) ('(!GRASP !2 *TIME))))))))

(§ DEFS !GRASPING THMLIST ((3 '((THUSE TCT-GRASPING)))))

(§ DEFS !GREEN SYS (!SPECTRUM))

(§ DEFS !HAND SYS (!PHYSOB))

(§ defn- !HAVE []
    (RELATION
        (RESTRICTIONSß (((!THING)) ((!THING) (and (memq '!ROLE (markers? SMOB1)) (CHECK (cadr (assq '!ROLE (relations? SMOB1))) (markers? SMSUB) (systems? SMSUB)))))
        PROCEDUREß ((!SUBST !1 ?)))
        (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB)))
        PROCEDUREß ((!BELONG !2 !1)))))

(§ DEFS !HEIGHT MEASFN (lambda [x] (caddr (size x))))

(§ defn- !IN []
    (COND ((CQ LOBJ)
        (RELATION
            (RESTRICTIONSß (((!THING)) ((!BOX))) MARKERSß (!PLACE) PROCEDUREß ((!IN !2)))))
        ((RELATION
            (RESTRICTIONSß (((!MANIP)) ((!BOX))) PROCEDUREß ((!CONTAIN !2 !1 *TIME)))
            (RESTRICTIONSß (((!MANIP)) ((!HAND))) PROCEDUREß ((!GRASPING !1 *TIME)))
            (RESTRICTIONSß (((!PLACE)) ((!BOX))) PROCEDUREß ((!IN !1 !2)))
            (RESTRICTIONSß (((!MANIP)) ((!CONSTRUCT))) PROCEDUREß ((!PART !1 !2 *TIME)))))))

(§ DEFS !IS PRIORITY 64)

(§ DEFS !LIKE TELLABLE true THMLIST ((3 '((THTBF THTRUE)))))

(§ DEFS !LOC
    THMLIST ((4 '((THUSE TC-LOC))) (5 '((THUSE TCT-LOC)))))

(§ defq- !LOC [a] (!LOC2 (car a) (cadr a)))

(§ defn- !LOC2 [LOCTYPE !LOC]
    (COND ((CQ LOBJ)
        (RELATION (RESTRICTIONSß (((!THING)) (LOBJ (!PHYSOB))) MARKERSß (!PLACE) PROCEDUREß ((!EVAL (list '!LOC LOCTYPE !LOC !2))))))
    ((RELATION (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PROCEDUREß ((!EVAL (list '!LOC LOCTYPE (COND (!LOC '!1) ('!2)) (COND (!LOC '!2) ('!1)) '*TIME))))))))

(§ DEFS !MANIP SYS (!PHYSOB))

(§ DEFS !MORE THMLIST ((4 '((THUSE TC-MORE)))))

(§ DEFS !NAME
    THMLIST ((2 '((THUSE TC-2))))
    SYS (!SYSTEMS))

(§ defn- !NAME []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!NAME !2)))))

(§ DEFS !NEWWORD SYS (!THING))

(§ DEFS !NOTICE
    THMLIST ((2 '((THUSE TC-2)))))

(§ defn- !NOTICE []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!NOTICE !2 *TIME)))))

(§ DEFS !ON
    THMLIST ((3 '((THUSE TC-ON))) (4 '((THUSE TCT-ON)))))

(§ defn- !ON []
    (COND ((CQ LOBJ)
        (RELATION (RESTRICTIONSß (((!THING)) ((!PHYSOB))) MARKERSß (!PLACE) PROCEDUREß ((!ON !2)))))
        ((RELATION
            (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PARAPHRASEß (ANYWHERE ON TOP OF) PROCEDUREß ((!ON !1 !2 *TIME)))
            (RESTRICTIONSß (((!PHYSOB)) ((!MANIP))) PARAPHRASEß (DIRECTLY ON THE SURFACE) PROCEDUREß ((!SUPPORT !2 !1 *TIME)))
            (RESTRICTIONSß (((!PLACE)) ((!PHYSOB))) PROCEDUREß ((!ON !1 !2)))))))

(§ DEFS !PACK THMLIST ((3 '((THUSE TC-3)))))

(§ DEFS !PART THMLIST ((3 '((THUSE TC-PART)))))                       ;; PERFORMED ON GRADUATION DAY, JUNE 2, 1972 BY JMH

(§ DEFS !PERSON SYS (!ANIMATE))

(§ DEFS !PICKUP
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-PICKUP))) (4 '((THUSE TCTE-PICKUP))))
    NAMEVENT (I3 (cons (VBFIX 'PICK false) (prtput 'UP OBJ1))))

(§ DEFS !PLACE SYS (!SYSTEMS))

(§ DEFS !PUT
    THMLIST ((3 '((THUSE TCT-3))) (4 '((THUSE TCT-PUT))) (5 '((THUSE TCTE-PUT)))))

(§ DEFS !PUTIN
    THMLIST ((3 '((THUSE TC-3))) (4 '((THUSE TCT-4))) (5 '((THUSE TCT-5)))))

(§ DEFS !PUTON
    NAMEVENT (I4 (cons (VBFIX 'PUT true) (concat OBJ1 '(ON) OBJ2)))
    THMLIST ((3 '((THUSE TC-3))) (4 '((THUSE TCT-4))) (5 '((THUSE TCTE-5)))))

(§ DEFS !RAISE THMLIST ((1 '((THUSE TC-RAISE)))))

(§ DEFS !RECTANGULAR SYS (!SHAPES))

(§ DEFS !REFERS THMLIST ((2 '((THUSE TC-REFERS)))))

(§ DEFS !PHYSOB
    SYSTEM (!BOX !CONSTRUCT !HAND !MANIP !TABLE)
    SYS (!THING)
    THMLIST ((2 '((THUSE TC-PHYSOB)))))

(§ defn- !PROPDEFINE [x]
    (PUTPROP x '(PROPN NS) 'FEATURES)               ;; CHANGED TO FEATURES FROM 'WORD' IN THE OLD DICTIONARY
    (PUTPROP x '((PROPN T)) 'SEMANTICS))

(§ DEFS !PROPERTY SYSTEM (!COLOR !SIZE !SHAPE) SYS (!THING))

(§ DEFS !POINTED SYS (!SHAPES))

(§ DEFS !RED SYS (!SPECTRUM))

(§ DEFS !RELATION SYS (!SYSTEMS))

(§ DEFS !ROLE
    NOGOAL true)

(§ defq- !ROLE [_] true)

(§ DEFS !ROUND SYS (!SHAPES))

(§ DEFS !ROW SYS (!CONSTRUCT))

(§ DEFS !ROBOT SYS (!ANIMATE))

(§ DEFS !SIZE
    MEASFN (lambda [x] (reduce #'+ (size x)))
    SYS (!PROPERTY))

(§ DEFS !SHAPE PRIORITY 128 SYS (!PROPERTY))

(§ DEFS !STACK SYS (!CONSTRUCT))

(§ DEFS !STACKUP
    THMLIST ((2 '((THUSE TC-2))))
    NAMEVENT (I3 (cons (VBFIX 'STACK false) (prtput 'UP OBJ1))))

(§ defn- !STACKUP []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!STACKUP !2 *TIME)))))

(§ DEFS !START
    THMLIST ((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(§ DEFS !SUBST NOGOAL true)

(§ DEFS !SUPPORT
    PRIORITY 256
    THMLIST ((3 nil) (4 '((THUSE TCT-SUPPORT)))))

(§ DEFS !SYSTEMS SYSTEM (!THING !EVENT !NAME !RELATION !PLACE))

(§ DEFS !TABLE SYS (!PHYSOB))

(§ DEFS !THICKNESS MEASFN (lambda [x] (cadr (size x))))

(§ DEFS !THING SYS (!SYSTEMS) SYSTEM (!ANIMATE !NAME !PHYSOB !PROPERTY))

(§ DEFS !UNGRASP
    THMLIST ((1 '((THUSE TC-UNGRASP))))
    NAMEVENT (I3 (concat (list (VBFIX 'LET true) 'GO 'OF) OBJ1)))

(§ DEFS !WANT
    THMLIST ((4 '((THUSE TC-WANT4))) (5 '((THUSE TC-WANT5)))))

(§ DEFS !WHITE SYS (!SPECTRUM))

(§ DEFS !WIDTH MEASFN (lambda [x] (car (size x))))

;; ############################################################
;;
;;                     PARTS OF SPEECH
;;
;; ############################################################

(§ DEFS ADJ ELIM (ADJ SUP COMPAR))

(§ DEFS ADV ELIM (ADV PREPADV TIMW TIM2 ADVADV VBAD PLACE LOBJ))

(§ DEFS BINDER ELIM (BINDER TIME))

(§ DEFS CLASF ELIM (CLASF))

(§ DEFS DET ELIM (DET NPL NS PART DEF INDEF NEG DEM INCOM OFD QNTFR NONUM QDET))

(§ DEFS NOUN ELIM (NOUN POSS MASS NPL NS TIM1 TIME MONTH))

(§ DEFS NUM ELIM (NUM NPL NS))

(§ DEFS NUMD ELIM (NUMD NUMDAN NUMDAT NUMDALONE))

(§ DEFS ORD ELIM (ORD TIMORD))

(§ DEFS POSS ELIM (NOUN NPL NS MASS NFS PRON))

(§ DEFS PREP ELIM (PREP MOTOR PLACE NEED2))

(§ DEFS PREP2 ELIM (PREP2))

(§ DEFS PRON ELIM (PRON QPRON EVERPRON POSS SUBJ OBJ NS NPL NFS NEG DEFPOSS))

(§ DEFS PRT ELIM (PRT))

(§ DEFS QADJ ELIM (PLACE QADJ))

(§ DEFS PROPN ELIM (PROPN POSS NS NPL))

(§ DEFS TPRON ELIM (TPRON NS NPL NEG ANY))

(§ DEFS VB ELIM (VB MVB AUX QAUX MODAL WILL BE DO HAVE ING EN INF V3PS QUOTING VFS VPL PAST PRESENT NEG ITRNS TRANS TRANSL TRANS2 TRANSL2 INT ITRNSL INGOB TOOB SUBTOB REPOB INGOB2 TOOB2 SUBTOB2 REPOB2 VPRT TO2 TRANSINT TOOB1 INGOB1 REPOB1))

;; ############################################################
;;
;;     I'M NOT QUITE SURE WHAT TO DO WITH THIS RANDOM STUFF
;;
;; ############################################################

(§ DEFS D MOD ((PAST EN) (INF MODAL AUX)))

(§ DEFS G MOD ((ING) (INF)))

(§ DEFS R MOD ((COMPAR) nil))

(§ DEFS T MOD ((SUP) nil))

(§ DEFS N MOD ((EN) (INF)))

(§ DEFS S MOD ((PRESENT V3PS NPL) (NS INF MODAL AUS MAS)))

(§ DEFS * MOD ((NEG) (nil)))

(§ DEFS THAMONG NOGOAL true)

(§ DEFS THSETQ NOGOAL true)

(§ DEFS THGOAL NOGOAL true)

(§ DEFS THOR NOGOAL true)

(§ DEFS THNOT NOGOAL true)

(§ DEFS THAND NOGOAL true)

(§ DEFS THPROG NOGOAL true)

(§ DEFS THFIND NOGOAL true)

;; ############################################################
;;
;;                     PRE-BUILT OSS'S
;;
;; ############################################################

(§ DEFS ANIMATE-OSS
    OSSNODE= ANIMATE-OSS
    MARKERS= (!ANIMATE !THING !SYSTEMS)
    RELATIONS= ((!IS ($? ANIM) ?))
    SYSTEMS= (!THING !SYSTEMS)
    DETERMINER= (SG-PL INDEF WHICH)
    VARIABLE= ANIM)

(§ DEFS FAKE-AGENT
    FEATURES (NG INDEF SG-PL)
    SEMANTICS (UNKNOWN-OSS-BY)
    PARENT (FAKE-BY-PHRASE))

(§ DEFS FAKE-BY-PHRASE
    FEATURES (PREPG AGENT)
    FIRSTWORD (BY)
    DAUGHTERS (FAKE-AGENT FAKE-BY))

(§ DEFS FAKE-BY
    FEATURES (PREP BY)
    FIRSTWORD (BY)
    DAUGHTERS WORD)

(§ DEFS FINDEVENTS-OSS
    OSSNODE= FINDEVENTS-OSS
    MARKERS= (!EVENT !SYSTEMS)
    SYSTEMS= (!SYSTEMS)
    DETERMINER= (SG-PL INDEF nil)
    VARIABLE= FINDEVENTS)

(§ DEFS FRIEND-OSS
    OSSNODE= FRIEND-OSS
    MARKERS= (!PERSON !ANIMATE !THING !SYSTEMS)
    SYSTEMS= (!ANIMATE !THING !SYSTEMS)
    REFER= (ßFRIEND)
    DETERMINER= (1 DEF nil))

(§ DEFS NAME-OSS
    OSSNODE= NAME-OSS
    MARKERS= (!NAME !THING !SYSTEMS)
    SYSTEMS= (!THING !SYSTEMS)
    DETERMINER= (1 DEF nil))

(§ DEFS PLACE-OSS
    OSSNODE= PLACE-OSS
    MARKERS= (!PLACE !SYSTEMS)
    SYSTEMS= (!SYSTEMS)
    DETERMINER= (SG-PL INDEF WHICH)
    VARIABLE= PLACE)

(§ DEFS SHRDLU-OSS
    OSSNODE= SHRDLU-OSS
    MARKERS= (!ROBOT !ANIMATE !THING !SYSTEMS)
    SYSTEMS= (!ANIMATE !THING !SYSTEMS)
    REFER= (ßSHRDLU)
    DETERMINER= (1 DEF nil))

(§ DEFS STACKPARTS-OSS
    OSSNODE= STACKPARTS-OSS
    MARKERS= (!THING !PHYSOB !MANIP !SYSTEMS)
    SYSTEMS= (!THING !PHYSOB !SYSTEMS)
    DETERMINER= (3 INDEF nil)
    VARIABLE= PART)

(§ DEFS UNKNOWN-OSS
    OSSNODE= UNKNOWN-OSS
    MARKERS= (!THING !SYSTEMS !PHYSOB !VAGUE)
    SYSTEMS= (!THING !SYSTEMS)
    DETERMINER= (SG-PL INDEF WHICH)
    VARIABLE= UNKNOWN)

(§ DEFS UNKNOWN-OSS-BY
    OSSNODE= UNKNOWN-OSS-BY
    RELATIONS= ((!IS ($? UNKNOWN) ?))
    MARKERS= (!THING !SYSTEMS !PHYSOB !VAGUE)
    SYSTEMS= (!THING !SYSTEMS)
    DETERMINER= (SG-PL INDEF nil)
    PARSENODE= (FAKE-AGENT)
    VARIABLE= UNKNOWN)

(§ DEFS UNKNOWNSG-OSS
    OSSNODE= UNKNOWNSG-OSS
    MARKERS= (!THING !SYSTEMS !PHYSOB !VAGUE)
    RELATIONS=  ((!IS ($? UNKNOWN) ?))
    SYSTEMS= (!THING !SYSTEMS)
    DETERMINER= (NS INDEF WHICH)
    VARIABLE= UNKNOWN)

(§ DEFS WE-OSS
    OSSNODE= WE-OSS
    MARKERS= (!ANIMATE !THING !SYSTEMS)
    SYSTEMS= (!ANIMATE !THING !SYSTEMS)
    REFER= (ßSHRDLU ßFRIEND)
    AND= (FRIEND-OSS SHRDLU-OSS))

;; ======>>> TEMPORARY PLACE FOR OSS-PROPERTY DEFS - MOVE WHEN APPROVED

(§ DEFS ANIMATE OSS ANIMATE-OSS)

(§ DEFS FINDEVENTS OSS FINDEVENTS-OSS)

(§ DEFS FRIEND OSS FRIEND-OSS)

(§ DEFS NAME OSS NAME-OSS)

(§ DEFS PLACE OSS PLACE-OSS)

(§ DEFS SHRDLU OSS SHRDLU-OSS)

(§ DEFS STACKPARTS OSS STACKPARTS-OSS)

(§ DEFS UNKNOWN OSS UNKNOWN-OSS)

(§ DEFS UNKNOWNSG OSS UNKNOWNSG-OSS)

(§ DEFS WE OSS WE-OSS)

(§ DEFLIST CONTRAST
    (RED !COLOR) (BLUE !COLOR) (GREEN !COLOR) (WHITE !COLOR) (BLACK !COLOR)
    (BIG !SIZE) (LITTLE !SIZE) (LARGE !SIZE) (SMALL !SIZE)
    (WIDE !WIDTH) (NARROW !WIDTH)
    (TALL !HEIGHT) (SHORT !HEIGHT)
    (THICK !THICKNESS) (THIN !THICKNESS))

#_(ns shrdlu.smspec)

;; ############################################################
;;
;;                           SMSPEC
;;                    (SEMANTIC SPECIALISTS)
;;
;; ############################################################

(§ defn- SMTIME [] (ert "SMTIME NOT WRITTEN YET"))

(§ defn- SMTIME2 [] (ert "SMTIME2 NOT WRITTEN YET"))

(§ defn- SMNEWNOUN []
    (OBJECT (MARKERSß (!NEWNOUN) PROCEDUREß ((!NEWWORD)))))

(§ defn- SMNEWPROPN [] (SMSET (list (NEWCOPY 'NAME-OSS))))

(§ defn- SMCONJ []
    ;; FOR ALL CONJOINED THINGS -- IT CURRENTLY ONLY HANDLES THINGS
    ;; WHICH HAVE AN OSS OR RSS STRUCTURE AS THEIR SEMANTICS.  THIS
    ;; DOES NOT INCLUDE SINGLE WORDS OF MOST TYPES.  IT USES RECURSION
    (let [%SM nil] (SMCONJ2 nil H) (RETURN (SMSET %SM))))

(§ defn- SMCONJ2 [INTERPLIST restlist]
    ;; INTERPLIST IS THE LIST OF INTERPRETATIONS FOR THE CONJUNCTS
    ;; HANDLED SO FAR -- THIS FUNCTION WILL BE CALLED ONCE FOR EACH
    ;; POSSIBLE COMBINATION.  THE MARKERS FOR THE CONJOINED
    ;; STRUCTURE ARE THOSE OF THE FIRST CONJUNCT -- NEEDS MORE
    ;; SOPHISTICATION.  RESTLIST IS THE REST OF NODES YET TO BE HANDLED.
    (let [%X nil]
        (or restlist
            (RETURN (SETQ %SM
                (cons (BUILD
                        RSSNODE= (and (rss? INTERP) (MAKESYM 'RSS))
                        OSSNODE= (and (oss? INTERP) (MAKESYM 'OSS))
                        MARKERS= (markers? INTERP)
                        SYSTEMS= (systems? INTERP)
                        REL= (rel? INTERP)
                        AND= (and (or (CQ BUT) (CQ AND)) INTERPLIST)
                        OR= (and (or (CQ OR) (CQ NOR)) INTERPLIST))
                    %SM))))
        ;; WHEN THERE IS NO RESTLIST, WE HAVE LOOPED TO THE END OF THE LIST OF CONJUNCTS, AND THE RESULTING INTERPRETATION IS OK.
        ;; THE MAPPING IS DOWN THE LIST OF INTERPRETATIONS FOR A SINGLE CONJUNCT WHILE THE RECURSION GETS US DOWN THE LIST OF CONJUNCTS.
        ;; THUS WE GET EVERY POSSIBLE COMBINATION OF THE INTERPRETATIONS. -- ISN'T LISP SUPER-DUPER-WONDERFUL!
        ;; NOTICE THAT INTERP IS GETTING PICKED UP AS A FREE VARIABLE BY SMCONJ2, EVEN THOUGH IT IS BOUND ONLY INSIDE A MAPCAR INSIDE SMCONJ2.
        ;; THIS WORKS BECAUSE THE CLAUSE CONTAINING IT CAN NEVER GET CALLED EXCEPT BY RECURSION,
        (dorun (map #'(lambda [INTERP] (SMCONJ2 (cons INTERP INTERPLIST) (cdr restlist))) (SM restlist)))))

(§ defn- SMVG [] ;; CALLED INSIDE ANY VG
    (let [TSS nil TENSE nil]
        (SETQ TSS (GETR 'TIME (MOVE-PT C U (CLAUSE))))
        (and (CQ NEG) (ADD-F-PT 'NEG PT))                           ;; NEG IS TRANSFERRED FROM THE VG TO THE CLAUSE IN WHICH IT IS EMBEDDED.
        (SETQ TENSE (GETR 'TENSE C))
        (COND ((memq TENSE '((PRESENT) (IMPER) (INFINITIVE))) true)
            ((= TENSE '(MODAL))
                (SETQ GLOBAL-MESSAGE "THAT DOESN'T MAKE ANY SENSE TO ME.")
                (ADD-F-PT 'MODAL PT))                               ;; CLAUSES ARE ALSO MARKED AS MODAL.
            ((and (= TENSE '(FUTURE)) (ISQ PT QUEST) (= (refer? (car (SM (GETR 'SUBJECT PT)))) '(ßSHRDLU))) ;; FUTURE QUESTIONS WITH "YOU"
                (SETQ TENSE '(PRESENT))                             ;; SUBJECT IS REALLY IMPERATIVE.
                (REMOVE-F-PT 'QUEST PT)
                (ADD-F-PT 'IMPER PT))                               ;; THE CLAUSE IS NO LONGER QUESTION, BUT RATHER, IMPERATIVE.
            ((SETDIF TENSE '(PAST PRESENT))
                (GLOBAL-ERR "I DON'T KNOW HOW TO HANDLE TENSES INVOLVING FUTURE EVENTS OR MODALS OTHER THAN IN THE PRESENT")))
        (PUTPROP TSS TENSE 'TENSE=)
        true))

(§ defn- SMADJGQSHORT [] (ert "SMADJQSHORT NOT WRITTEN YET"))

(§ defn- SMPRON [node]
    (eval (SM node))
    (COND ((nil? SM)
        (SETQ GLOBAL-MESSAGE (str "I DON'T KNOW WHAT \"" (FROM (NB H) (N H)) "\" REFERS TO"))))
    SM)

(§ defn- SMVAUX []
    (COND ((ISQ H NEG) (FQ NEG)) (:else true))
    (PUTPROP (GETR 'TIME C) (or (MEET (FE H) '(PRESENT PAST MODAL)) (erterr "SMVAUX -- FUNNY TENSE")) 'TENSE=))

(§ defn- SMADV [] (ert "SMADV NOT WRITTEN YET"))

(§ defn- SMPLACE [] (ert "SMPLACE NOT WRITTEN YET"))

(§ defn- SMTOADJ [] (ert "SMTOADJ (UNCT) NOT WRITTEN YET"))

(§ defn- SMPROP []
    ;; THIS IS THE SEMANTICS FOR PROPER NOUNS.  IT PRODUCES TWO
    ;; INTERPRETATIONS.  ONE IS THE OPAQUE REFERENCE TO THE NAME
    ;; ITSELF, AS IN "CALL IT SAM".  THE OTHER IS THE TRANSPARENT
    ;; REFERENT AS IN "PICK UP SAM".
    (SMSET (list
        (BUILD
            OSSNODE= (MAKESYM 'OSS)
            VARIABLE= 'NAME
            DETERMINER= (1 DEF nil)
            PARSENODE= C
            MARKERS= (!NAME)
            REFER= (list (WORD (NB H))))
        (BUILD
            OSSNODE= (MAKESYM 'OSS)
            DETERMINER= (1 DEF nil)
            PARSENODE= C
            VARIABLE= (MAKESYM 'X)
            RELATIONS= (list (list '!NAME OSSNODE= (WORD (NB H)))))))
    (SMNG2))

(§ defn- SMADJ [WORD-BEING-INTERPRETED]
    ;; THIS FUNCTION TAKES AS INPUT THE PARSE NODE FOR AN
    ;; ADJECTIVE - NOT COMPARATIVE OR SUPERLATIVE.  IT JUST EVAL'S
    ;; THE DEFINITION.  THAT DEFINITION (WHICH SHOULD BE AN NMEANS)
    ;; MAP'S DOWN THE LIST OF OSS'S IN THE FREE VARIABLE "SM".  IT
    ;; CHECKS FOR MARKER COMPATIBILITY AND ATTACHES ITS "PLANNER"
    ;; TO THAT OF THE OSS.  IT SHOULD MAKE UP NEW OSS'S IN CASE OF
    ;; MULTIPLE INTERPRETATIONS OF THE PHRASE.  THE OSS'S IT
    ;; CREATES ARE LEFT IN THE FREE VARIABLE "SM".  THIS FUNCTION
    ;; CALLED BY: SMNG1 IT NEEDS TO BE HAIRED UP FOR CONJOINED
    ;; ADJECTIVES LIKE "GREEN AND RED BALL".
    (eval (SM WORD-BEING-INTERPRETED)))
    ;; EVALUATE THE DEFINITION OF THE ADJECTIVE

;; --------------------------------------------

(§ defn- SMADJG-PREPG []
    ;; HANDLES ADJECTIVE GROUPS AND PREPGS BOTH AS COMPLEMENTS AND QUALIFIERS.
    ;; DO NOTHING FOR "BY" PHRASES IN PASSIVE CLAUSES OR "OF" PHRASES LIKE IN "THREE OF THE BLOCKS".
    ;; SEMANTIC SUBJECT IS THE SUBJECT OF AN INTENSIVE OR THE NG TO WHICH THE GROUP IS A QUALIFIER,
    ;; OR THE CLAUSE OF WHICH IT IS AN ADJUNCT.
    (let [X nil SMSUB nil]
        (and (or (CQ AGENT) (CQ OF)) (RETURN true))
        (SETR 'LOGICAL-SUBJECT
            (COND ((CQ COMP) (GETR 'SUBJECT (MOVE-PT C U (CLAUSE))))
                ((CQ LOBJ) (or (GETR 'OBJ1 (MOVE-PT C U (CLAUSE))) (GETR 'SUBJECT PT)))
                ((ISQ (MOVE-PT C U (not (ISQ PT COMPONENT)) U) NG) PT)
                ((ISQ PT CLAUSE) PT)
                ((erterr "SMADJG-PREPG FUNNY POSITION")))
            C)
        (SETQ SMSUB (SM (GETR 'LOGICAL-SUBJECT C)))
        (and (CQ ADJG)
            (GETR 'OBJ1 C)
            (SETR 'ADJGHEAD
            (COMPARE-BUILD (GETR 'HEAD C) (COND ((CQ AS) '!ASMUCH) ((CQ THAN) '!MORE) ((erterr "SMADJG-PREPG FUNNY TYPE"))))
            C))
        (COND
            ((GETR 'OBJ1 C) (SMCL1) (RETURN SM))
            ((RETURN (SMSET
                (let [SM nil]
                    (SMSET (doall (map #'(lambda [OSS]
                        (BUILD
                            OSSNODE= (MAKESYM 'OSS)
                            MARKERS= (markers? OSS)
                            SYSTEMS= (systems? OSS)
                            VARIABLE= (variable? OSS)
                            REFER= (refer? OSS)
                            REL= OSS
                            REFER= (refer? OSS)
                            DETERMINER= '(NS-PL INDEF nil)))
                        SMSUB)))
                    (eval (COND
                        ((or (CQ COMPAR) (CQ SUP)) (FINDMEASURE (GETR 'HEAD C)))
                        (:else (SM (GETR 'HEAD C)))))
                    (RETURN SM))))))))

;; -------------------------------------------

(§ defn- SMIT [PRONOUN]
    ;; PRONOUN IS (IT THEY ONE) A NODE LIST OF POSSIBLE REFERENTS.
    ;; IS THIS A "DO IT!" COMMAND?  IF SO, RETURN THE LAST EVENT MENTIONED.
    ;; IF THIS PRONOUN HAS BEEN USED BEFORE IN THIS SENTENCE, THEN USE THE SAME CANDIDATES.
    ;; IF THIS PRONOUN WAS USED IN THE PREVIOUS SENTENCE,
    ;; LOOK FOR A STRUCTURE LIKE "A BLOCK WHICH IS TALLER THAN ANYTHING WHICH SUPPORTS IT"
    ;; OR "A BLOCK TALLER THAN ANYTHING WHICH SUPPORTS IT".
    (let [CANDIDATES nil AMBIGUITIES nil]
        (or DISCOURSE (ert "SMIT: DISCOURSE SWITCH NOT ON"))
        (and MVB
            (ISQ MVB DO)
            (CQ OBJ1)
            (RETURN (SMSET LASTEVENT)))
        (COND ((get PRONOUN 'BIND)
                (MAP #'(lambda [BINDNODE] (SMIT2 BINDNODE 0)) (get PRONOUN 'BIND))
                (RETURN SM))
            ((SMIT2 (get PRONOUN 'LASTBIND) 0)
                (GO DONE))
            ((or (MOVE-PT C U U (NG) U U (NG)) (MOVE-PT C U U (NG) U (COMP) PV (SUBJ)))
                (SMIT2 PT 0)
                (MOVE-PT C U U (NG))
                (COND ((ISQ PT DEF)
                    (ADD-F-PT 'INDEF PT)
                    (REMOVE-F-PT 'DEF PT)
                    (dorun (map #'(lambda [INTERP] (PUTPROP INTERP '((EXACTLY 1) INDEF nil) 'DETERMINER=)) (SM PT)))))
                (RETURN SM))
            ((or (MOVE-PT C U (BOUND) U) (MOVE-PT C U (and (ISQ PT CLAUSE) (ISQ PT COMPONENT)) U DLC))
                (SMIT2 (GETR 'OBJ2 PT) 0)
                (SMIT2 (GETR 'OBJ1 PT) 0)
                (SMIT2 (GETR 'SUBJECT PT) 0)
                (and (nil? SM)
                    (ISQ PT RSQ)
                    (SMIT2 (GETR 'RELHEAD PT) 0))
                (and SM (RETURN SM))))
        (SMIT2 (GETR 'SUBJECT LASTSENT) 192)
        (SMIT2 (parsenode? LASTREL) 128)                ;; TRY REL (I.E. QUESTION FOCUS) OF THE LAST SENTENCE
        (MOVE-PT LASTSENT DLC)
    UP  (COND ((not (MOVE-PT PV (NG))) (GO ON))
            (:else (SMIT2 PT 64)))                       ;; GO THROUGH TOP LEVEL NG'S OF LAST SENTENCE
        (and (MOVE-PT PV) (GO UP))
    ON  (or SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE ANSREF (NG'S IN LAST ANSWER)
            (MAP #'(lambda [ANSNODE] (SMIT2 ANSNODE 0)) ANSNAME))
        (or SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE BACKREF2 (NG'S IN LAST SENTENCE) LIST
            (MAP #'(lambda [BACKNODE] (SMIT2 BACKNODE 0)) BACKREF2))
    DONE (PUTPROP PRONOUN CANDIDATES 'BIND)
        (or (cdr SM) (REMPROP (car SM) 'AMBIGUITIES=))
        SM))

(§ defn- SMIT2 [NODE PLAUSIBILITY]
    ;; MAKE SURE NODE IS REALLY THERE.
    ;; QUEST NODES (SUCH AS "WHAT") OR OTHER NODES WITHOUT HEAD NOUNS ARE NOT SUITABLE FOR REFERENTS.
    ;; MAKE SURE THAT NODE HASN'T ALREADY BEEN USED AS REFERENT.
    ;; MAKE SURE NODE AND PRONOUN AGREE IN NUMBER.
    (and NODE
        (GETR 'HEAD NODE)
        (not (memq (car NODE) CANDIDATES))
        (COND ((= PRONOUN 'IT)
            (and (ISQ NODE NS) (not (ISQ NODE PRONG))))
            (:else (ISQ NODE NPL)))
        (SETQ CANDIDATES (cons (car NODE) CANDIDATES))
        (SMSET (concat
            (doall (map #'(lambda [REFERENT-OSS]
                (BUILD
                    OSSNODE= (MAKESYM 'OSS)
                    MARKERS= (markers? REFERENT-OSS)
                    SYSTEMS= (systems? REFERENT-OSS)
                    PLAUSIBILITY= PLAUSIBILITY
                    AMBIGUITIES= (list (list OSSNODE= (FROM (NB NODE) (N NODE)) C))
                    REFER= (refer? REFERENT-OSS)
                    VARIABLE= (variable? REFERENT-OSS)
                    PARSENODE= C ;; INPUT PARAMETER
                    ;; USE THE REFERENT'S REFERENT, IF IT HAS ONE.
                    DETERMINER= (list (COND ((ISQ C NPL) 'NPL) ('NS)) 'INDEF nil)
                    ;; DONE SO THAT IF VARIBLE IS BOUND, PLANNER GENERATOR WILL USE IT.
                    ;; RELATION SAYS THAT THIS OSS "REFERS" TO THE OSS WHOSE VARIABLE NAME IS GIVEN.
                    RELATIONS= (list (list '!REFERS (variable? REFERENT-OSS)))))
                (SM NODE)))
            SM))))

(§ defn- SMNGOF []
    ;; MAP DOWN THE LIST OF "OF" OBJECT INTERPRETATIONS.
    ;; USED TO PROCESS NOUN GROUPS LIKE "THREE OF THE BLOCKS", "BOTH OF THEM".
    ;; SINCE THE OBJECT OF THE "OF" MUST BE DEFINITE (SYNTACTICALLY) AND HAS ALREADY BEEN PROCESSED,
    ;; THE PLANNER CODE BUILT IS JUST A THAMONG EXPRESSION OF THE LIST
    ;; OF POSSIBLE REFERENTS OF THE "OF" OBJECT.
    (SMSET (MAPBLAND
        #'(lambda [OFOSS]
            (BUILD
                OSSNODE= (MAKESYM 'OSS)
                VARIABLE= (variable? OFOSS)
                SYSTEMS= (systems? OFOSS)
                MARKERS= (markers? OFOSS)
                PARSENODE= C
                DETERMINER= (list (COND ((CQ NUM) (SM (MOVE-PT H PV (NUM)))) ((ISQ NB BOTH) 2) ('NPL))
                                (COND ((MOVE-PT H PV (QNTFR)) (eval (SM PT))) ('INDEF))
                                (COND ((CQ HOWMANY) 'HOWMANY) ((CQ QDET) 'WHICH)))
                RELATIONS= (list (list 'THAMONG (list 'THV (variable? OFOSS)) (list 'quote (refer? OFOSS))))))
        (SM (MOVE-PT H DLC)))))

;; ============================================================

(§ defn- SMNG1 []
    ;; SMNG1 IS CALLED AS SOON AS THE HEAD OF A NOUN GROUP IS
    ;; PARSED.  IT FIRST BUILDS A SKELETON OSS CONTAINING ONLY THE
    ;; DETERMINERS AND ORDINALS.  IT THEN EVAL'S THE DICTIONARY
    ;; DEFINITION OF THE HEAD NOUN WHICH SHOULD BUILD OSS'S FOR
    ;; EACH POSSIBLE INTERPRETATION OF THE NOUN.  IT THEN CYCLES
    ;; THROUGH ALL THE GOODIES IN FROUNT OF THE HEAD NOUN, EVALING
    ;; THEIR DEFINITIONS.  THE FREE VARIABLE "SM" IS USED TO KEEP
    ;; THE LIST OF OSS'S DURING THIS ENTIRE PROCESS.  NOTE THE
    ;; SPECIAL HANDLING OF TPRONS (ANYTHING SOMETHING ETC.) AND OF
    ;; SUPERLATIVE AND COMPARATIVE ADJECTIVES.
    (let [WORD-BEING-INTERPRETED nil DETERS nil]
        (SETQ DETERS (list
            (COND
                ((CQ NUMD) ((lambda [NUM] (eval (SM (MOVE-PT H PV (NUMD))))) (SM (MOVE-PT H PV (NUM)))))
                ((CQ NUM) (SM (MOVE-PT H PV (NUM))))
                ((CQ NPL) (COND ((ISQ NB BOTH) 2) ((CQ NS) 'SG-PL) ('NPL)))
                ('NS))
            (COND
                ((CQ QNTFR) (eval (SM (MOVE-PT H PV (QNTFR)))))
                ((CQ TPRON) (eval (SM (MOVE-PT H PV (TPRON)))))
                ((CQ DEF) 'DEF)
                ((CQ DET) 'INDEF)
                ('NDET))
            (COND
                ((CQ HOWMANY) 'HOWMANY)
                ((CQ QDET) 'WHICH))))
        ;; BUILD AN INITIAL OSS.  SETUP TO LOOP THROUGH ADJECTIVES.
        ;; IF IT'S A TPRON, IT WAS EVALED ABOVE, SO SKIP INCOMPLETES SUCH AS "PICK UP TWO".
        ;; EVAL THE HEAD NOUN IF AN ADJECTIVE ELIMINATES ANY POSSIBLE INTERPRETATION FOR THIS NG,
        ;; FAIL IF WE'VE LOOPED THRU ALL THE MODIFIERS, THEN RETURN THE LIST OF POSSIBLE INTERPRETATIONS.
        ;; IF IT'S A COMPARATIVE OR SUPERLATIVE ADJECTIVE,
        ;; IF IT'S AN ADJECTIVE OR CLASSIFIER, THEN EVAL THE DICTIONARY DEFINITION OF IT.
        (SMSET (list (BUILD
            OSSNODE= (MAKESYM 'OSS)
            PARSENODE= C
            VARIABLE= (MAKESYM 'X)
            MARKERS= (and (CQ TPRON) '(!VAGUE !PHYSOB !THING))
            RELATIONS= (and (CQ TPRON) (list (list '!PHYSOB OSSNODE=)))
            DETERMINER= DETERS)))
        (SETQ WORD-BEING-INTERPRETED H)
        (COND ((ISQ H TPRON) (GO LOOP))
            ((CQ INCOM) (SMONE) (GO LOOP)))
        (SMSET (eval (SM WORD-BEING-INTERPRETED)))
    LOOP (COND ((nil? SM) (RETURN nil)))
        (COND ((nil? (SETQ WORD-BEING-INTERPRETED (cdr WORD-BEING-INTERPRETED)))
                (RETURN SM))
            ((or (ISQ WORD-BEING-INTERPRETED COMPAR) (ISQ WORD-BEING-INTERPRETED SUP))
                (eval (FINDMEASURE WORD-BEING-INTERPRETED))
                (GO LOOP))
            ((or (ISQ WORD-BEING-INTERPRETED ADJ) (ISQ WORD-BEING-INTERPRETED CLASF))
                (SMADJ WORD-BEING-INTERPRETED)
                (GO LOOP))
            ((ISQ WORD-BEING-INTERPRETED POSS)
                (SMPOSS)
                (GO LOOP)))
        (GO LOOP)))

;; ============================================================

(§ defn- SMNG2 []
    ;; CALLED FROM NG WHEN ALL QUALIFIERS HAVE BEEN FOUND.
    ;; BASICALLY, IT SAVES THE NG ON THE BACKREF(ERENCE) LIST, AND CALLS SMNG3
    ;; (ON EACH POSSIBLE NG INTERPRETATION) TO EVAL ANY DEFINITE NOUN GROUPS, EG. "THE RED BLOCK".
    ;; AS USUAL, THE INITIAL OSS LIST IS IN "SM" AND THE FINAL OSS LIST IS PUT IN "SM".

    ;; DON'T USE FAKEY ANSWER NAME NODES FOR REFERENCE.
    ;; QUEST NODES ARE NOT SUITABLE REFERENTS.
    ;; SAVE THIS NG AWAY FOR POSSIBLE LATER BACK REFERENCE.
    ;; GO THRU ALL THE POSSIBLE INTERPRETATIONS OF THIS NOUN GROUP.
    (and (not (CQ ANSNAME))
        (GETR 'HEAD C)
        DISCOURSE
        (SETQ BACKREF (cons (car C) BACKREF)))
    (SMSET (MAPBLAND #'SMNG3 SM)))

(§ defn- SMNG3 [oss]
    ;; TAKES AN OSS AS ARGUMENT AND TRIES TO FIND ITS REFERENCE IF THE NOUN GROUP IS DEFINITE.
    ;; EXPECT FOR SPECIAL "ONLY DEFINITE" DEFINITES SUCH AS "THE RIGHT" AND "THE THING".
    (let [FINDER nil MUNG nil INTER nil LIST nil CANDIDATES nil UNBOUND nil]
        (COND
            ((not (= (quantifier? oss) 'DEF)) (RETURN oss))        ;; IF ITS NOT DEFINITE OR IT
            ((refer? oss) (RETURN oss))                             ;; ALREADY HAS A REFERENT
            ((CQ ANSNAME) (RETURN oss)))                            ;; MARKED,  IF ITS KLUDGY
        (SETQ FINDER
            (PLNR-FINDIFY 'ALL                                      ;; ANSWER NAME, JUST RETURN IT
                (variable? oss)                                     ;; JUST RETURN IT
                (list (variable? oss))
                (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))) ;; BUILDS UP THFIND EXPRESSION
        (PUTPROP oss FINDER 'PLNRCODE=)
        (SETQ WHO nil)
    UP  (COND
            ((not (SETQ CANDIDATES (THVAL2 WHO FINDER))) (GO TOOFEW))
            ((number? (num? oss)) (COND ((< (count CANDIDATES) (num? oss)) (GO TOOFEW)) ((> (count CANDIDATES) (num? oss)) (GO TOOMANY))))
            ((= (num? oss) 'NS) (COND ((nil? CANDIDATES) (GO TOOFEW)) ((cdr CANDIDATES) (GO TOOMANY))))
            ((memq (num? oss) '(NPL SG-PL)))
            ((ert "SMNG3: SCREWY NUMBER PROPERTY OF OSS")))
        (PUTPROP oss CANDIDATES 'REFER=)
    DONE (RETURN oss)

    TOOFEW ;; WE DIDN'T FIND ANY (OR ENOUGH) REFERENTS FOR THE NG
        (COND
            ((or (nil? DISCOURSE) (nil? WHO))
                (SETQ GLOBAL-MESSAGE (str "I DON'T KNOW WHAT YOU MEAN BY \"" (FROM NB N) "\"."))
                (RETURN nil))
            ;; IF WE AREN'T REMEMBERING SENTENCES, FORGET IT IF WE JUST TRIED TO
            ;; FIND EVERYTHING (OR EVERYTHING THAT "HE" KNOWS ABOUT), THEN FAIL
            ((memq WHO '(HE nil))
                (SETQ GLOBAL-MESSAGE (str "I DON'T KNOW WHICH " (cdr (FROM NB N)) " YOU MEAN."))
                (RETURN nil)))
        (SETQ MUNG true)

    TOOMANY ;; ELSE SET UP TO EXPAND THE SENTENCES WE'RE LOOKING AT
        (and (memq WHO '(HE nil))
            (SETQ FINDER (plnr-mung FINDER CANDIDATES)))
        ;; RESTRICT THE POSSIBLE REFERENTS TO BE AMONG THE LIST ALREADY FOUND
        (SETQ WHO (COND
            ((= WHO nil) 'HE)
            ((= WHO 'HE) (list (dec LASTSENTNO) (inc LASTSENTNO)))
            ((or (not MUNG) (== (car WHO) 1)) (SETQ WHO 'HE) (GO TOOFEW))
            ((cons (dec (car WHO)) (cdr WHO)))))
        (SETQ MUNG nil)
        (GO UP)))

(§ defn- SMONE []
    (let [CONTRAST nil x H] ;; SET X TO DAUGHTERS OF CURRENT NODE
    GO  (COND ((SETQ CONTRAST (get (ROOT (NB x)) 'CONTRAST))
                (SETQ CONTRAST (list CONTRAST (ROOT (NB x)))))
            ((SETQ x (cdr x)) (GO GO)))
    UP  (or (and (MOVE-PT C U U (NG)) (SMONE2 (list (car PT))))
            (SMONE2 (parsenode? LASTREL))
            (SMONE2 BACKREF)
            (SMONE2 ANSNAME)
            (SMONE2 BACKREF2)
            (COND (CONTRAST (SETQ CONTRAST nil) (GO UP)))
            (and (MOVE-PT LASTSENT DLC PV (NG)) (SMONE2 (list (car PT))))
            (ert "SMONE: CAN'T FIND REFERENT FOR \"ONE\""))
        SM))

(§ defn- SMONE2 [x]
    ;; SMONE2 TAKES IN A LIST OF NOUN GROUP NODES AND TRIES TO SEE
    ;; IF ANY OF THOSE NOUN GROUPS COULD BE THE REFERENT FOR "ONE".
    (let [WORD-BEING-INTERPRETED nil]
        ;; IF X IS EMPTY, FAIL.
        ;; TRY TO SEE IF FIRST NG OF X SATIFIES CONTRAST AND/OR COULD BE REFERENT, ELSE TRY NEXT NG IN X
    UP  (COND ((nil? x) (RETURN nil))
            ((SETQ WORD-BEING-INTERPRETED (SMONE3 x)))
            (:else (SETQ x (cdr x)) (GO UP)))
        ;; AT THIS POINT WORD-BEING-INTERPRETED IS (SHOULD BE) A
        ;; LIST A WORD NODES OF THE NG WHICH IS THE REFERENT FOR
        ;; "ONE" WE NOW PROCEED TO BUILD UP AN OSS FOR THE "ONE"
        ;; NG THE LIST IS IN ORDER (NOUN ADJ ... ADJ ETC NUM DET)
        ;; ONLY THE NOUN AND THE ADJ'S ARE USED
        (or (ISQ WORD-BEING-INTERPRETED NOUN)
            (bug "SMONE2: REFERENT OF \"ONE\" IS SCREWED UP"))
        (eval (SM WORD-BEING-INTERPRETED))                          ;; EVAL THE NOUN DEFINITION
    GO  (and
            (SETQ WORD-BEING-INTERPRETED (cdr WORD-BEING-INTERPRETED))
            (ISQ WORD-BEING-INTERPRETED ADJ)                        ;; IF WE REACHED END OF ADJECTIVES, STOP
            (eval (SM WORD-BEING-INTERPRETED))
            (GO GO))
        SM))

(§ defn- SMONE3 [ONENG]
    ;; SMONE3 TAKES AN NG WHICH IS A POSSIBLE REFERENT FOR "ONE".
    ;; IT FIRST CUTS THE NG TO BE ONLY (NOUN ADJ ... ADJ ETC) I.E.
    ;; IT STRIPS OF QUALIFYING PHRASES.  IF THERE IS NO CONTRAST,
    ;; THEN THIS MUNGED NG IS RETURNED AS THE REFERENT.  IF THERE
    ;; IS A CONTRAST, THEN IT CHECKS TO SEE IF THE NG SATISFIES
    ;; THAT CONTRAST.
    (let [NGWORDS nil x nil]
        (or (ISQ ONENG NG)
            (bug "SMONE3: ONE REFERENT IS NOT A NG"))
        (SETQ NGWORDS (H ONENG))
    LOOP (COND ((nil? NGWORDS) (RETURN nil))                        ;; FAIL, IF NG HAS NO NOUN HEAD
            ((ISQ NGWORDS NOUN))                                    ;; IF FIND NOUN HEAD OF NG, WIN
            (:else (SETQ NGWORDS (cdr NGWORDS)) (GO LOOP)))
        (or CONTRAST (RETURN NGWORDS))                              ;; IF THERE IS NO CONTRAST, REFERENT WINS BY DEFAULT
        (SETQ x (reverse NGWORDS))
    LOOK (COND ((and (= (car CONTRAST) (get (ROOT (NB x)) 'CONTRAST)) (not (= (cadr CONTRAST) (ROOT (NB x)))))
                (RETURN (reverse (cdr x))))
            ((SETQ x (cdr x)) (GO LOOK))
            (:else (RETURN nil)))))                                  ;; FAIL, IF NO WORD SUPPLIES CONTRAST

(§ defn- SMPOSS []
    (let [x (SMPOSS2 C (MOVE-PT H PV (POSS)))]
        (and x (SMRELATE x))))

(§ defn- SMPOSS2 [headnode modnode]
    (let [SM nil SMSUB (SM modnode) SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST (RETQ AMOB1 (SM headnode))]
        (SMSET '(!HAVE))
        (and SM (let [x (MAKESYM 'NODE)] (and x (PUTPROP x SM 'SEMANTICS) (list x))))))

;; SMPOSS WORKS BY ACTING LIKE SMCL1 AND SETTING UP AN RSS (HAVE X Y).  NODE IS THE NODE OF THE POSSESSIVE
;; WHICH HAS ALREADY BEEN SEMANTICALLY PROCESSED.  ITS SM CONTAINS THE OSS'S FOR WHOSE DOING THE POSSESSING.
;; THE SM CURRENTLY ACTIVE IS THE THING BEING POSSESSED.

(§ defn- SMRELATE [node]
    ;; RELATES A DESCRIPTIVE RSS TO ANOTHER RSS OR OSS ADDING IT
    ;; TO THE LIST OF RELATIONS.  IT TAKES THE LIST OF SS IN SM,
    ;; AND REPLACES SM WITH THE NEW LIST OF MODIFIED SS'S.  THE
    ;; MODIFYING RSS'S HAVE TO HAVE ONE OF THE SM SS'S AS A REL
    ;; (WHICH SHOULD ALWAYS BE TRUE IF THEY WERE SET UP PROPERLY).
    ((lambda [x] (and x (SMSET x)))
        (doall (map #'(lambda [RSS]
            (let [REL (rel? RSS)]
                (or (memq REL SM)
                    (erterr "SMRELATE - TO WHOM?"))
                (RETURN (BUILD
                    OSSNODE= (and (oss? REL) (MAKESYM 'OSS))
                    RSSNODE= (and (rss? REL) (MAKESYM 'RSS))
                    MARKERS= (or (and (relmarkers? RSS) (car (relmarkers? RSS))) (markers? REL))
                    SYSTEMS= (or (and (relmarkers? RSS) (cadr (relmarkers? RSS))) (systems? REL))
                    PLAUSIBILITY= (plausibility? RSS)
                    PARSENODE= (parsenode? REL)
                    AMBIGUITIES= (ambiguities? RSS)
                    VARIABLE= (variable? REL)
                    NEGATIVE= (negative? REL)
                    DETERMINER= (determiner? REL)
                    RELATIONS= (cons RSS (relations? REL))
                    REL= (rel? REL)))))
            (SM node)))))

;; -----------------------------------------------------

(§ defn- SMCL1 []
    (let [SMSUB nil SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST nil]
        ;; SET UP GLOBAL VARIABLES WHICH CONSIST OF POINTERS TO THE SEMANTIC DEFINITIONS
        ;; OF THE VARIOUS NOUN-GROUPS (ALSO RSNG'S) REQUIRED BY THE TRANSITIVITY OF THE VERB
        (SETQ SMSUB (COND
            ((SETQ SMSUB (GETR 'LOGICAL-SUBJECT C)) (SM SMSUB))
            ((CQ IMPER) '(SHRDLU-OSS))
            ((not (CQ PASV)) (SM (or (GETR 'SUBJECT C) (erterr "SMCL1 -- NO SUBJECT"))))
            ((CQ AGENT) (erterr "SMCL1 -- AGENT MISSING"))
            ('(UNKNOWN-OSS-BY))))
        (SETQ SMOB1 (SM (COND ((CQ PASV) (GETR 'SUBJECT C)) ((GETR 'OBJ1 C)))))
        (SETQ SMOB2 (SM (GETR 'OBJ2 C)))
        (SETQ SMOBL (SM (GETR 'LOBJ C)))
        (SETQ SMCOMP (SM (GETR 'COMP C)))
        ;; NATURALLY SEVERAL OF THESE GLOBAL VARIABLES (BOUND IN THIS PROG AND ACCESSED IN DEEPER ONES)
        ;; ARE NIL AT THIS POINT IN THE PROCEDURE.  THE FOLLOWING CHECKS ARE PRIMARILY FOR DEBUGGING PURPOSES
        ;; (HENSE THE "ERT") TO INSURE THAT THE NON-NIL REGISTERS AND THE TRANSITIVITY OF THE VERB ARE
        ;; BEING MATCHED IN EVERY CASE.
        (or SMSUB (and (MEET '(THERE ITRNS) FE) (GO CHECK)))
        (or SMOB1 (and (or (CQ TRANS) (not (CQ CLAUSE))) (GO CHECK)))
        (or (and SMOB1 SMOB2) (and (CQ TRANS2) (GO CHECK)))
        (or (and SMOB1 SMOBL) (and (CQ TRANSL) (GO CHECK)))
        (or SMCOMP (and (CQ INT) (GO CHECK)))
        (GO REL)
    CHECK (ert "BUG: SMCL1 TRANSITIVITY")
    REL (SETQ RELLIST
            (SM (COND
                ((CQ RSQ) (GETR 'RELHEAD C))
                ((or (CQ PREPG) (CQ ADJG)) (GETR 'LOGICAL-SUBJECT C))
                ((CQ QUEST) (GETR 'RELHEAD C)))))
        (and (not RELLIST)
            (or (CQ POLAR) (CQ DECLAR))
            (SETQ X (RELFIND C))
            ;; FIND RELATIVE ELEMENT FOR POLAR CLAUSES WHICH CONTAIN INDEFINITE.
            ;; APPLIES TO TOPLEVEL CLAUSES SINCE ONLY THEY CAN HAVE FEATURES POLAR OR DECLAR.
            (or (= X SMSUB)
                (= X SMOB1)
                (= X SMOB2)
                (= X SMOBL)
                (= X SMCOMP)
                (erterr "SMCL1 -- POLAR REL DOESN'T MATCH"))
            (SETQ RELLIST X))
        (SETQ TIME (GETR 'TIME (MOVE-PT C U (CLAUSE))))
        ;; THIS REFERS TO THE SEMANTIC SENSE OF THE VERB WHICH WILL PROBABLY VARY WITH ITS TRANSITIVITY.
        ;; THE VALUE THAT IS FINALLY DETERMINED REPRESENTS ALL POSSIBLE SENSES OF THE MEANING OF THE WORD
        ;; THAT ARE APPROPRIATE TO THE TRANSITIVITY FIGURED OUT BY THE SYNTACTIC PROGRAMS
        (SETQ SENSE-OF-VERB
            (COND
                ((CQ PREPG) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'HEAD C))))
                ((CQ ADJG) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'ADJGHEAD C))))
                ((cadr (assq (car (MEET FE '(ITRNS TRANS INT TRANSL TRANS2 THERE ITRNSL))) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'MVB C))))))))
        (SMSET (eval SENSE-OF-VERB))
        ;; THIS DETERMINES THE APPROPRIATE SEMANTIC INTERPRETATION(S) FOR THE CLAUSE BY CHECKING
        ;; THE RESTRICTIONS OF EACH DEFINITION AGAINST THE MARKERS OF THE VARIOUS CANDIDATES FOR SMSUB,
        ;; SMOB1, ETC.  THE VALUE OF THE EVALUATION IS A LIST OF RELATION-SEMANTIC-STRUCTURES, ONE FOR
        ;; EACH PLAUSIBLE INTERPRETATION
        (MAP #'SMCL-MODIFIERS H) ;; SMCL-MODIFIERS WILL EXAMINE
        (RETURN SM)))
        ;; ALL OF THE CONSTITUENTS OF THE CLAUSE THAT WERE NOT INVOLVED IN THE BUILDRSS AND WILL EVALUATE
        ;; THE MEANINGS OF EACH IN TURN FOR THEIR EFFECT ON THE ESTABLISHED SM, THE PARSING TREE, OR
        ;; ANYTHING ELSE THAT WOULD BE APPROPRIATE THE VALUE OF SMCL1 IS NON-NIL ONLY IF SOME
        ;; REASONABLE MEANING HAS BEEN FOUND FOR THE CLAUSE

(§ defn- SMCL2 []
    ;; THERE USED TO BE A CALL TO SMPREPREL AT THIS POINT, BUT IT
    ;; HAS GONE AWAY PENDING FURTHER THOUGHT.
    (MAP #'SMCL-MODIFIERS H))
    ;; AS IN SMCL1, WE NEED TO SCAN THE CONSTITUENTS OF
    ;; THE CLAUSE AND ALLOW THEM TO MAKE WHATEVER MODIFICATION ARE APPROPRIATE

(§ defn- SMCL-MODIFIERS [WORD-BEING-INTERPRETED]
    ;; AS IN CONSTITUENT, THIS PROCEDURE IS BASICLY ONE LARGE DISPATCH TABLE WHICH
    ;; ARRANGES THAT THE PROPER KIND OF PROCESSING HAPPEN TO THE APPROPRIATE CONSTITUENT.
    ;; SOME SHOULD BE IGNORED SINCE THEY HAVE ALREADY BEEN DEALT WITH AND OTHERS SHOULD
    ;; BE EVALUATED AS MODIFIERS OR FOR THEIR SIDE-EFFECTS.
    (COND ((nil? (get WORD-BEING-INTERPRETED 'FEATURES)))
    ;; IF THE CONSTITUENT HAS A NULL FEATURE LIST, THEN
    ;; IT IS A FUNCTION WORD (IE. (PARSE NIL FOR)) WHICH SHOULD BE IGNORED.
;;      ((OR (ISQ WORD-BEING-INTERPRETED VG)
;;          (ISQ WORD-BEING-INTERPRETED AUX))
;;      (AND (ISQ WORD-BEING-INTERPRETED NEG)
;;          (FQ NEG)
;;          (BUILDRSS WORD-BEING-INTERPRETED 'NEG 'NEG)))
    ;; THIS HAS THE EFFECT OF CHECKING IF THE VERB IS NEGATIVE AND THEN ARRANGING THAT THE FEATURE
    ;; LIST OF THE WHOLE CLAUSE AND OF ITS MEANING AGREE. ******* MAYBE SOMEONE ELSE SHOULD DO IT
    ;; ?????????????? IGNORE ALL CONSTITUENTS WITH THESE FEATURES SKIPS TO THE OTHER PART OF THE "AND"
    ;; TO CALL ANOTHER SEMANTIC SPECIALIST CF. REFERENCE IN CLAUSE.SEC EG. "DAY" IN "WHAT DAY IS..."
    ;; TIE UP AS SYNTACTIC LOOSE END.  GIVE IT A REFERENCE PROP. - WHY ?????????
    ((MEET FE '(THERE LOBJ COMP PRT SUBJ OBJ1 OBJ2)))
    ((ISQ WORD-BEING-INTERPRETED NG)
        (and (COND ((ISQ WORD-BEING-INTERPRETED TIM))
            ((and (CQ REL-NOT-FOUND)
                (ISQ WORD-BEING-INTERPRETED QUEST)
                (ISQ (H WORD-BEING-INTERPRETED) TIM1))
            (RQ REL-NOT-FOUND)
            (FQ TIMEQ)))
        (SMTIME)))
    ;; IN WHICH CASE IT WAS ALREADY PROCESSED MIGHT GO AWAY IN A FEW DAYS
    ;; BUG CHATCHER MIGHT WANT TO CHANGE THAT THE REST ARE HOOKS FOR WHEN
    ;; WE FIGURE OUT WHAT TO DO WITH THEM
    ((ISQ WORD-BEING-INTERPRETED PREPG)
        (or (ISQ WORD-BEING-INTERPRETED AGENT)
            (ert "SMCL-MOD BAD PREPG")))
    ((ISQ WORD-BEING-INTERPRETED QADJ)
        (or (MEET FE '(LOBJ COMPQ))
            (eval (SM WORD-BEING-INTERPRETED))))
    ((ISQ WORD-BEING-INTERPRETED BOUND))
    ((ISQ WORD-BEING-INTERPRETED BINDER))
    ((ISQ WORD-BEING-INTERPRETED QUEST))
    ((ISQ WORD-BEING-INTERPRETED CLAUSE))
    ((ert "SMCL-MODIFIERS ATTEMPTED TO PROCESS AN UNEXPECTED TYPE OF CONSTITUENT"))))

(§ defn- SMBIND []
    (let [TSS nil EVENT nil START nil END nil]
        ;; DOES THE SM HAVE MORE THAN ONE VALUE???
        (and (cdr (SM H))
            (ert "I DON'T KNOW WHAT TO DO WITH AMBIGUOUS BOUND CLAUSES"))
        ;; DISPATCH TABLE TO MATCH THE APPROPRIATE ACTION WITH EACH BINDER.
        ;; MOVE TO THE FIRST WORD OF THE CLAUSE (TO THE BINDER) AND CHECK FOR THE FEATURE TIME
        ;; (MAYBE ALSO CHECK FOR THE SM BEING MARKED AS AN EVENT???)
        (COND ((ISQ (MOVE-PT H DF) TIME)
            (SETQ TSS (GETR 'TIME C))
            (or (SETQ EVENT (FINDEVENTS (car (SM H))))
                (GLOBAL-ERR "NO SUCH THING EVER HAPPENED"))
            (SETQ EVENT (car EVENT))
            (SETQ START (get EVENT 'START))
            (SETQ END (get EVENT 'END))
            (eval (SM PT))
            (RETURN true)))
        nil))

(§ defn- SMBINDER [start-ev end-ev]
    ;; CALLED FOR A ABINDER - THE FIRST ARGUMENT GIVES THE BEGINNING, SECOND THE END.
    ;; A TYPICAL USE IS THE DEFINITION OF "AFTER", WHICH IS (SMBINDER END NIL) I.E.
    ;; THE EVENT STARTS AFTER THE END OF THE BOUND EVENT, WITH NO SPECIFICATION ON WHEN IT ENDS.
    (PUTPROP TSS start-ev 'START=)
    (PUTPROP TSS end-ev 'END=))

#_(ns shrdlu.smutil)

;; ############################################################
;;
;;                          SMUTIL
;;
;; ############################################################

(§ defn- ISTENSE [node arg]
    ;; CHECKS VARIOUS THINGS ABOUT TENSE.
    (let [x nil]
        (or (SETQ x (GETR 'TIME node)) (ert "ISTENSE -- NO TIME REGISTER"))
        (or (SETQ x (tense? x)) (ert "ISTENSE -- NO TENSE"))
        (RETURN (COND
            ((= arg 'PRESENT) (memq x '((PRESENT) (PRESENT PRESENT))))
            ((= arg 'FUTURE) (= x '(FUTURE)))
            ((= arg 'MODAL) (= x '(MODAL)))
            ((= arg 'IMPERF) (and (cdr x) (= (car x) 'PRESENT)))
            (:else (ert "ISTENSE -- FUNNY ARG"))))))

(§ defn- IMPERF? [tss]
    (and (cdr (SETQ X (tense? X))) (= (car X) 'PRESENT)))

(§ defn- IMPERF [] (ISTENSE C 'IMPERF))

(§ defq- BUILD [%L]
    ;; BUILD CONSTRUCTS AN OSS OR AN RSS FROM ITS ARGUMENTS.
    ;;
    ;; INPUTS:
    ;;   A LIST OF KEYWORDS FOLLOWED BY THEIR VALUES.  EVERY OTHER
    ;;   ARGUMENT IS EVALUATED (THAT IS KEYWORDS SHOULD NOT BE QUOTED).
    ;;
    ;; POSSIBLE KEYWORDS:
    ;;   MARKERS=        LIST OF SEMANTIC MARKERS
    ;;   SYSTEMS=        LIST OF SYSTEMS FOR THOSE MARKERS--USED FOR FAST MARKER CHECKING?
    ;;   PLAUSIBILITY=   INTEGER BETWEEN 0 AND 512 USED IN DISAMBIGNUATION
    ;;   RSSNODE=        NODE NAME FOR AN RSS
    ;;   OSSNODE=        NODE NAME FOR AN OSS
    ;;   PARSENODE=      CORRESPONDING NODE OF PARSE TREE
    ;;   VARIABLE=       NAME OF VARIABLE TO BE USED IN BUILDING PLANNER CODE
    ;;   DETERMINER=     DETERMINER INFORMATION - A 3-LIST
    ;;   RELATIONS=
    ;;   AMBIGUITIES=    LIST OF POTENTIAL AMBIGUITIES FOR THIS INTERPRETATION
    ;;   AND=            LIST OF CONJUNCTS
    ;;   OR=             LIST OF DISJUNCTS
    ;;
    ;; VALUE:
    ;;   THE NODE NAME OF THE OSS OR RSS CONSTRUCTED.  AN ATOM.
    ;;
    ;; SIDE-EFFECTS:
    ;;   USES PUTPROP TO ATTACH PROPERTIES TO NODE NAMES
    (let [%X nil NEGATIVE= nil REFER= nil PLNRCODE= nil MARKERS= nil SYSTEMS= nil TENSE= nil TSSNODE= nil RELMARKERS= nil ANSNODE= nil ACTION= nil ANSRSS= nil PLAUSIBILITY= nil DETERMINER= nil AND= nil OR= nil AMBIGUITIES= nil RELATIONS= nil VARIABLE= nil VARLIST= nil REL= nil RSSNODE= nil PARSENODE= nil OSSNODE= nil NODE= nil %PROPS nil]
        (SETQQCHECK true %L
            (SETQ %PROPS '(NEGATIVE= REFER= PLNRCODE= MARKERS= SYSTEMS= RELMARKERS= PLAUSIBILITY= DETERMINER= AND= OR= AMBIGUITIES=
                           RELATIONS= VARIABLE= VARLIST= REL= RSSNODE= PARSENODE= OSSNODE= TSSNODE= TENSE= ANSNODE= ANSRSS= ACTION=))
            'BUILD)
        (and RSSNODE= (not MARKERS=) (SETQ MARKERS= '(!RELATION)))
        (and MARKERS= (not SYSTEMS=) (SETQ %X (CHECK MARKERS= nil nil)) (SETQ MARKERS= (car %X)) (SETQ SYSTEMS= (cadr %X)))
        (SETQ NODE= (or OSSNODE= RSSNODE= TSSNODE= ANSNODE= (ert "BUILD: NO NODE=")))
        (dorun (map #'(lambda [%PROP] (and (SETQ %X (eval %PROP)) (PUTPROP NODE= %X %PROP))) %PROPS))
        NODE=))

(§ defn- NEWCOPY [oss]
    (let [old (cdr oss) new (MAKESYM 'OSS)]
        ;; WATCH OUT -- THIS IS IMPLEMENTATION DEPENDENT,
        ;; AND GETS THE ENTIRE PROPERTY LIST IN OUR LISP.
    =>  (COND
            ((nil? old) (PUTPROP new C 'PARSENODE=) (RETURN new))
            ((= (car old) 'PNAME))
            ((PUTPROP new (cadr old) (car old))))
        (SETQ old (cddr old))
        (GO =>)))

(§ defq- RELATION [%DEFL]
    ;; CONSTRUCTS RSS'S FOR GARDEN VARIETY VERBS.  USED IN DEFINITION OF SAME.
    ;;
    ;; INPUTS:
    ;;   INPUTS ARE IN KEYWORD FORMAT (ATTRIBUTE-VALUE PAIRS).
    ;;   THE KEYWORD IS NOT EVALUATED BUT ITS VALUE IS.
    ;;
    ;; POSSIBLE KEYWORDS:
    ;;   RESTRICTIONSß   LIST OF RESTRICTIONS ON VARIOUS SEMANTIC REGISTERS
    ;;                       EACH RESTRICTION (CALLED %MARL FOR MARKER LIST IN THE CODE) IS A LIST
    ;;                           EITHER WHOSE CAR IS A REGISTER NAME (E.G. SMSUB) AND WHOSE CADR IS A LIST OF MARKERS
    ;;                              OR WHOSE CAR IS A LIST OF MARKERS IN WHICH CASE
    ;;                                 THE ASSOCIATED REGISTER NAME IS DETERMINED BY THE POSITION IN RESTRICTIONSß
    ;;                                    SMSUB FOR CAR
    ;;                                    SMOB1 FOR CADR
    ;;                                    SMOB2 FOR CADDR
    ;;   PROCEDUREß      CONDENSED PLANNER CODE SCHEMA
    ;;   MARKERSß        SEMANTIC MARKERS
    ;;   PLAUSIBILITYß   EVALUATED TO GET INTEGER FROM 0 TO 512 INDICATING RELATIVE LIKLIHOOD OF THIS DEFINITION SENSE.
    ;;   RELß            ******>>>
    ;;
    ;; VALUE:
    ;;   LIST OF RSS NODE NAMES CREATED
    ;;
    ;; SIDE-EFFECTS:
    ;;   CREATES AN RSS BY CALLING BUILD
    (ITERATE
        #'(lambda ARGLIST
            (let [SMCOMP nil SMSUB nil SMOB1 nil SMOB2 nil SMOBL nil MARKERSß nil RESTRICTIONSß nil PLAUSIBILITYß nil RELß nil PARAPHRASEß nil RELMARKERSß nil RSSNAME nil PROCEDUREß nil !1 nil !2 nil !3 nil %NEWRSS nil %OSSNODE nil]
                (SETQ %DEF (ARG 1) SMSUB (ARG 2) SMOB1 (ARG 3) SMOB2 (ARG 4) SMOBL (ARG 5) SMCOMP (ARG 6))
                ;; AN LEXPR IS USED HERE IN ORDER TO GET AROUND THE LIMITATION OF FIVE EXPR ARGUMENTS IN COMPILED CODE.
                ;; NOTICE THAT WITHIN THIS LAMBDA EXPRESSION THAT
                ;; SMSUB = ONE OSS FOR SEMANTIC SUBJECT
                ;; SMOB1 = ONE OSS FOR SEMANTIC OBJECT 1
                ;; SMOB2 = ONE OSS FOR SEMANTIC OBJECT 2
                ;; SMOBL = ONE OSS FOR LOCATIVE OBJECT
                ;; SMCOMP = ONE OSS FOR SEMANTIC COMPLEMENT
                ;; WHEREAS OUTSIDE OF THE LAMBDA EXPRESSION EACH OF THESE NAMES REPRESENTS A LIST OF THE SAME.
                ;; THIS IS TO ALLOW DICTIONARY WRITERS TO USE THESE SELF SAME NAMES IN WRITING DEFINITIONS, A SIMPLY TERRIBLE IDEA.
                (SETQQCHECK nil %DEF '(RESTRICTIONSß PROCEDUREß PLAUSIBILITYß PARAPHRASEß MARKERSß) 'RELATION)
                ;; (EVAL ...) DECODES KEYWORD ARGUMENTS.  SETQQ EFFECTIVLY QUOTES BOTH PAIRS
                ;; RESTRICTIONSß IS EXPANDED HERE PUTING IN IMPLICIT REGISTER REFERENCES SO THAT IT CAN BE UNIFORMLY GOBBLED BELOW
                (SETQ RESTRICTIONSß
                    ;; MARKL IS A SINGLE MARKER LIST FROM ON OF THE RESTRICTIONS IN THE DEFINITION, E.G. (!PHYSOB !RED).
                    ;; %RESTRICTNAM IS A NAME LIKE SMSUB, SMOBL, SMCOMP, .... WHICH REFERS TO REGISTERS ELSEWHERE
                    ;; IN THE PROGRAM WHOSE MARKERS MUST BE COMPATIBLE WITH %MARKL AS CHECKED BELOW.
                    ;; %NUM IS THE NUMBER WHICH WILL BE USED TO SUBSTITUTE IN THE DICTIONARY EXPRESSION.
                    (doall (map #'(lambda [%RESTRICTNAM %MARKL %NUM]
                        ((lambda [x] (SET %NUM (eval (car x))) x)
                            (COND ((term? (car %MARKL)) %MARKL) ((cons %RESTRICTNAM %MARKL)))))
                        '(SMSUB SMOB1 SMOB2)
                        RESTRICTIONSß
                        '(!1 !2 !3))))
                (and
                    ;; CHECK THAT THIS DEFINITION SENSE MEETS ALL OF THE RESTRICTIONS SET FORTH IN THE DEFINITION UNDER RESTRICTIONSß
                    (ERRSET
                        ;; ENCLOSED IN A ERRSET SO THAT THE FAILURE OF A CHECK CAN CAUSE IMMEDIATE ESCAPE FROM THE MAPC
                        ;; AND HENCE TO THE AND WHICH CUTS OFF ALL FURTHER PROCESSING OF THIS DEFINITION SENSE
                        ;; TEMPORARY STORAGE ON THE PROPERTY LIST OF TEMP USED TO AVOID SEARCHING FOR THESE ITEMS ON THE
                        ;; ONE HAND OR THE CONFLICT OF NAMES BETWEEN THE THE MARKERS RESULTING FROM CHECKING THE REL ARE
                        ;; SAVED TO PUT ON IT LATER WHEN THE CLAUSE IS RELATED.
                        (dorun (map #'(lambda [%MARKL]
                            (let [OSS (eval (car %MARKL)) x (CHECKREL OSS) CHECK nil]
                                (and x (SETQ RELß (car x)))
                                (COND
                                    ((not (and (or (nil? (cddr %MARKL)) (eval (caddr %MARKL)))
                                            (SETQ CHECK (CHECK (cadr %MARKL) (markers? OSS) (systems? OSS)))))
                                        (ERR nil))
                                    ((= OSS RELß)
                                        (SETQ RELMARKERSß CHECK)))
                                nil))
                            RESTRICTIONSß)))
                        ;; SUBJECT RESTRICTION MARKERS USED IN THE DEFINITION AND THE REGISTERS OF THE SAME NAME REFERENCED
                        ;; AS FREE VARIABLES IN THIS PROGRAM.  ON THE OTHER HAND, IF THE RESTRICTIONS HAVE BEEN MET, THEN
                        ;; BUILD AN RSS NODE
                    (SETQ %NEWRSS
                        ;; NEWRSS IS THE NEW RSS NODE NAME CREATED BY BUILD.  RSSNODE= IS KEYWORD FOR INPUT INTO BUILD
                        ;; OF RSS NODE NAME.  IN THE CALL TO BUILD THE ITEMS ENDING IN = ARE KEYWORDS WHOSE VALUE IS
                        ;; THE FOLLOWING ITEM.  MARKERSß, OF COURSE IS A VARIABLE IN THIS FUNCTION.  THIS CALL JUST SAYS
                        ;; SEND TO BUILD FOR MARKERS= THE VALUE SENT TO RELATION FOR MARKERSß  THE PARSENODE IS THE
                        ;; CURRENT NODE ***, #1, #2, AND #3 SUBSTITUTIONS DONE.  THIS IS FIRST STEP IN BUILDING PLANNER CODE.
                        ;; NUMSUB IS THE ****, #!, #", #3 SUBSTITUTION FUNCTION.
                        ;; %PLNRPHRASE IS ONE CHUNK OF CONDENSED PLANNER CODE LIKE (!COLOR *** !RED).
                        (BUILD
                            RSSNODE= (SETQ RSSNAME (MAKESYM 'RSS))
                            MARKERS= MARKERSß
                            VARIABLE= ((lambda [x] (PUTPROP x x 'RSSVAR)) (MAKESYM 'EVX))
                            PARSENODE= C
                            RELATIONS= (reverse (doall (map #'(lambda [%PLNRPHRASE] (PLNR-NUMSUB '<<<RELATION-ERROR>>> %PLNRPHRASE)) (EVALCHECK PROCEDUREß))))
                            REL= RELß
                            NEGATIVE= (and (CQ NEG) true)
                            RELMARKERS= RELMARKERSß
                            PLAUSIBILITY= (+ (plausibility? SMSUB) (plausibility? SMOB1) (plausibility? SMOB2) (or (eval PLAUSIBILITYß) 0))
                            AMBIGUITIES= (concat (ambiguities? !1) (ambiguities? !2) (ambiguities? !3)
                                                 (and PARAPHRASEß (list (list RSSNAME PARAPHRASEß WORD-BEING-INTERPRETED)))))))
            (RETURN %NEWRSS)))
    %DEFL SMSUB SMOB1 SMOB2 SMOBL SMCOMP))

(§ defn- DOBACKREF [answer]
    ;; CALLED WHEN THE PROCESSING OF A SENTENCE IS COMPLETED.
    ;; SETS UP VARIABLES SO THAT NEXT SENTENCE MAY REFER TO GOODIES IN THIS SENTENCE.
    ;; DOES THE FOLLOWING:
    ;;    1. SET LASTREL TO REL OF THIS SENTENCE
    ;;    2. SET LASTEVENT TO THE EVENT DESCRIBED IN THIS SENTENCE
    ;;       FOR REFERENCE OF "DO IT!" COMMAND.
    ;;    3. SET LASTANSEVENT TO THE EVENT DESCRIBED IN THE
    ;;       ANSWER OF THIS SENTENCE. FOR REFERENCE OF "THAT"
    ;;       IN QUERY: "WHY DID YOU DO THAT?"
    ;;    4. SET BACKREF2 TO BACKREF AND (!!!) GO THROUGH ALL
    ;;       THINGS ON BACKREF AND SET THEIR REFER(ENT)= PROPERTY
    ;;       TO THE BIND OF THEIR VARIABLES. ALSO MAKE SURE EACH
    ;;       NG NODE ON BACKREF POINT TO THE RIGHT OSS IN ITS SM.
    ;;       THIS ENSURES THAT EACH OSS POINTS TO THE ACTUAL
    ;;       THING IT WAS ASSUMED TO REFER TO.
    ;;    5. REMOVE THE BIND PROPERTY OF PRONOUNS (IT THEY) USED
    ;;       IN THIS SENTENCE AND MAKE IT THEIR LASTBIND PROPERTY.
    (let [ansrss (get answer 'ANSRSS=)]
        (SETQ BACKREF2 BACKREF)
        (SETQ BACKREF nil)
        (SETQ LASTREL (rel? ansrss))
        (dorun (map #'(lambda [PRONOUN] (PUTPROP PRONOUN (get PRONOUN 'BIND) 'LASTBIND) (REMPROP PRONOUN 'BIND)) '(IT THEY ONE)))
        (or
            (CQ MODAL)
            (CQ DECLAR)
            (MAP #'(lambda [BACKNODE]
                (COND ((cdr (SM BACKNODE)) ;; TRUE IF NODE HAD MULTIPLE INTERPRETATIONS
                    (PRINT (SM BACKNODE))
                    (SETR 'SEMANTICS (ert "DOBACKREF: RETURN AN OSS FOR BACKNODE") BACKNODE)))
            (COND
                ((refer? (car (SM BACKNODE)))) ;; IF NODE HAS REFERENT, FINE
                (:else
                    (PUTPROP (car (SM BACKNODE))
                        (or (get (variable? (car (SM BACKNODE))) 'BIND) (ert "DOBACKREF: RETURN REFERENT FOR BACKNODE"))
                        'REFER=))))
            BACKREF2))
        ;; A FEW MISSING PIECES GO HERE
        nil))

(§ defn- EVALCHECK [l]
    ;; EVALCHECK CHECKS FOR THE PRESENCE OF (!EVAL (MUMBLE ...) ...) IN THE INPUT S-EXPRESSION L.
    ;; IF IT FINDS ONE THEN THE EXPRESSION MUMBLE IS EVALUATED AND REPACES (!EVAL ...), OTHERWISE
    ;; L IS RETURNED JUST THE WAY IT WAS.  HENCE THIS FUNCTION IS THE INVERSE OF QUOTE.
    (COND ((term? l) l)
        ((= (car l) '!EVAL) (eval (cadr l)))
        (l)))

(§ defq- ITERATE [%L]
    ;; GENERALIZED MAPPING FUNCTION.
    ;;  APPLIES FUNCTION TO THE CARTESIAN PRODUCT OF N LISTS GIVEN AS ARGUMENTS.
    ;;
    ;; INPUTS:
    ;;     (CAR %L)     FUNCTIONAL ARGUMENT.
    ;;                  FUNCTION OF N ARGS WHERE N IS THE NUMBER OF LISTS GIVEN.
    ;;     (CDR %L)     THE N LISTS.
    ;;
    ;; VALUE:
    ;;  THE RESULT OF APPLYING THE FUNCTION IS MAPBLANDED TOGETHER, THAT IS
    ;;  IF EACH APPLICATION RESULTS IN AN ATOM, THE RESULTING LIST
    ;;  IS A LIST OF ATOMS.  IF EACH APPLICATION RESULTS IN A LIST OF ATOMS,
    ;;  THE RESULT IS STILL A LIST OF ATOMS.  IF THE APPLICATION RESULTS
    ;;  IN NIL, THE NIL VANISHES FROM THE RESULTANT LIST.
    (let [%X nil %XL nil] (RETURN (eval (ITERATEX (car %L) (cdr %L))))))
    ;; THIS SHOULD BECOME A MACRO SO THAT ITERATE TAILORS A MAPPING FUNCTION
    ;; FOR EACH APPLICATION WHICH IS THEN COMPILED

(§ defn- ITERATEX [f l]
    ;; EXPANDS CALL ON ITERATE INTO A TAILORED SET OF MAPBLAND
    ;; CALLS WHICH DO THE APPROPRIATE COMPUTATION WHEN EVALUATED.
    ;; INPUTS:
    ;;      F      FUNCTION OF N ARGUMENTS
    ;;      L      LIST OF N LISTS WHICH F IS TO BE APPLIED TO
    ;; VALUE:
    ;;  TAILORED FUNCTION
    (COND ((nil? l) (cons (eval f) %XL))
        ((list 'MAPBLAND
            (list 'FUNCTION (list 'LAMBDA
                ;; %X IS USED TO STORE THE VARIABLE NAME WHICH IT GETS FROM (GENSYM)
                ;; %XL IS USED TO SAVE A LIST OF ALL OF THE VARIABLE NAMES SO FAR SO
                ;; THAT THEY CAN BE GIVEN TO THE FUNCTION AT THE END (CONS %X NIL).
                ;; CREATES A SHINY NEW CELL WHICH IS BOLTED ONTO THE BACK END OF %XL
                ;; BY NCONC.  THIS PAIR IS NECESSARY BECAUSE SETQ RETURNS AS ITS VALUE
                ;; THE RESULT OF THE LAST PAIR THAT IT PROCESSES.  A RUSE. PUTS (NIL)
                ;; IN PLACE OF NIL AS A LIST TO PREVENT MAPBLAND FROM QUITTNG.
                (list (SETQ %X (GENSYM) %XL (concat %XL (cons %X nil)) %X %X))
                (ITERATEX f (cdr l))))
            (or (car l) '(nil))))))

(§ defn- MAPBLAND [fun l]
    ;; THIS IS THE INSTAMATIC CAMERA FUNCTION.  NO MATTER WHAT YOU PUT INTO THE FUNCTION,
    ;; IT AUTOMATICALLY ADJUSTS INTERNALLY TO THE AVAILABLE LIGHT SO THAT WHAT COMES OUT
    ;; THE END ALWAYS LOOKS THE SAME -- ONE BIG NIL-LESS LIST OF ALL THE APPLICATIONS OF
    ;; THE FUNCTION FUN.  THE PURPOSE IN THIS IS SO THAT THE FUNCTION CAN BE EASILY NESTED.
    ;;
    ;; INPUTS:
    ;;     FUN -  FUNCTION OF ONE ARGUMENT TO BE APPLIED TO EACH ELEMENT IN L
    ;;     L   -  LIST
    ;; VALUE:
    ;;     IF (FUN L) IS AN ATOM, THEN A LIST OF ATOMS.
    ;;     IF (FUN L) IS A LIST, THEN ALL THE LISTS APPENDED, THAT IS A LIST OF ATOMS.
    ;;     IF (FUN L) IS NIL, THEN ALL TRACE DISAPPEARS (SAVE FOR SIDE-EFFECTS).
    (let [ANS nil F nil]
        (and (nil? l) (SETQ l '(nil)))
    =>  (COND ((nil? (SETQ F (fun (car l)))))
            ((term? F) (SETQ ANS (concat ANS (cons F nil))))
            ((SETQ ANS (concat ANS F))))
        (SETQ l (cdr l))
        (and l (GO =>))
        ANS))

(§ defn- MAPC2 [fun l]
    ;; MAPPING FUNCTION FOR GOING 2 AT A TIME THROUGH A LIST.
    ;; INPUTS:
    ;;     FUN  -   FUNCTION OF TWO ARGUMENTS
    ;;     L    -   LIST (ESPECIALLY ATTRIBUTE VALUE TYPE LIST)
    ;; VALUE:
    ;;  LIST (LIKE MAPCAR) OF FUN APPLIED TO TOP TWO ELEMENTS.
    ;;
    ;; FUN APPLIED TO TOP TWO ELEMENTS.  LIST IS STEPPED TWO AT A TIME.
=>  (COND ((nil? l) (RETURN true)))
    (fun (car l) (cadr l))
    (SETQ l (cddr l))
    (GO =>))

(§ defn- MUMBLE [x]
    ;; MUMBLE IS THE PLANNER FILTER FOR LOOKING AT ASSERTIONS TO
    ;; SEE WHEN THEY WERE MENTIONED IN THE DIALOG. IT USES THE FREE
    ;; VARIABLE "WHO" TO DECIDE WHAT TO LOOK FOR. WHO IS EITHER NIL
    ;; (USE ANYTHING) "HE" (USE ANYTHING WHICH HAS BEEN MENTIONED)
    ;; OR A LIST OF TWO SENTENCE NUMBERS, MIN AND MAX (USE ANY WHO
    ;; PROPERTY WHICH IS ON OR BETWEEN THEM). THE WHO PROPERTY OF
    ;; ANY ASSERTION IS A SENTENCE NUMBER WHEN IT WAS MOST RECENTLY
    ;; MENTIONED.
    (COND ((nil? WHO))
        ((= WHO 'HE) (get x 'WHO))
        ((SETQ x (get x 'WHO)) (not (or (< x (car WHO)) (> x (cadr WHO)))))))

(§ defq- OBJECT [%DEFL]
    ;; %DEFL IS THE LIST OF DEFINITION SENSES.
    ;; CONSTRUCTS OSS FOR GARDEN VARIETY NOUNS AND ADJECTIVES.
    ;; USED IN DEFINITIONS.
    ;;
    ;; INPUTS:
    ;;  INPUTS ARE IN KEYWORD FORMAT (ATTRIBUTE-VALUE PAIRS).
    ;;  THE KEYWORD IS NOT EVALUATED BUT ITS VLUE IS.
    ;; POSIBLE KEYWORDS:
    ;;     MARKERSß            LIST OF SEMANTIC MARKERS
    ;;     PLAUSIBILITYß       EVALS TO INTEGER FROM 0 TO 512 INDICATING RELATIVE LIKLIHOOD OF THIS DEFINITION SENSE
    ;;     DETß                *** I'M TOO LAZY TO LOOK THIS UP NOW FILL IN DET ***
    ;; FREE VARIABLE INPUT:
    ;;     SM  -  A LIST OF CURRENT OSS'S WITH WHICH THE TO-BE-CREATED ONES MUST BE COMPATIBLE.
    ;; VALUE:
    ;;     A NEW LIST OF OSS'S TO TAKE THE PLACE OF THE OLD ONE.
    ;; SIDE-EFFECTS:
    ;;  SM IS RESET TO THE VALUE OF OBJECT.
    ;;  A SET OF OSS'S ARE CREATED AT THE GLOBAL LEVEL.
    (let [%VARNAM nil]
        ;; PROG TO DECLARE VARIABLE.
        ;; IF SM IS EMPTY (AS IN THE CASE OF A HEAD), MAKE A NEW VARIABLE SYSMBOL,
        ;; OTHERWISE THE APPROPRIATE %VARNAM WILL BE DECIDED INSIDE THE ITERATE LOOP.
        (or SM (SETQ %VARNAM (MAKESYM 'X)))
        (RETURN (SMSET
            ;; %OSS IS A SINGLE OSS NODE NAME PICKED OFF OF SM.
            ;; %DEF IS A SINGLE DEFINITION SENSE PICKED OFF OF %DEFL.
            (ITERATE #'(lambda [%OSS %DEF]
                (let [%OSSNODE nil %CHKRESULT nil MARKERSß nil SYSTEMSß nil PLAUSIBILITYß nil DETß nil RELATIONSß nil PARAPHRASEß nil PROCEDUREß nil]
                    ;; DECODE KEYWORDS
                    (SETQQCHECK nil %DEF '(MARKERSß PLAUSIBILITYß PARAPHRASEß PROCEDUREß) 'OBJECT)
                    ;; CHECK FOR MARKER AGREENT.  IF OK, THEN BUILD OSS, ELSE CHUCK THIS COMBINATION.
                    (and
                        (SETQ %CHKRESULT (CHECK MARKERSß (markers? %OSS) (systems? %OSS)))
                        ;; BUILD OSS COMBINING INFORMATION FROM CURRENT OSS WITH INFORMATION IN THE DEFINITION.
                        ;; NOTE THAT THE INITIAL OSS WHICH GETS BUILT UP FOR A WORD DEPENDS NOT ONLY ON ITS DEFINITION,
                        ;; BUT ALSO ON THE CONTEXT IN WHICH IT IS USED.
                        (RETURN (BUILD
                            OSSNODE= (SETQ %OSSNODE (MAKESYM 'OSS))
                            PARSENODE= (parsenode? %OSS)
                            MARKERS= (car %CHKRESULT)
                            SYSTEMS= (cadr %CHKRESULT)
                            DETERMINER= (determiner? %OSS)
                            VARIABLE= (variable? %OSS)
                            RELATIONS= (concat (reverse (doall (map #'(lambda [%PLNRPHRASE] (PLNR-NUMSUB %OSS %PLNRPHRASE)) (EVALCHECK PROCEDUREß)))) (relations? %OSS))
                            REL= (rel? %OSS)
                            ;; THE OSS NAME PROVIDES A UNIQUE LABEL FOR WHERE THE AMBIGUITY OCCURRED FOR LATER COMPARISON.
                            AMBIGUITIES= (concat (ambiguities? %OSS) (and PARAPHRASEß (list (list %OSS PARAPHRASEß WORD-BEING-INTERPRETED))))
                            PLAUSIBILITY= (+ (or (eval PLAUSIBILITYß) 0) (plausibility? %OSS)))))
                    nil))
            SM %DEFL)))))

;; ######################################################
;;
;;              PLANNER BUILDING ROUTINES
;;
;; ######################################################

(§ defn- PLNR-JUNKIFY [code]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (COND
        ((term? code) code)
        ((= (car code) 'THGOAL) (list 'THAND code '(VALUEPUT)))
        ((= (car code) 'THFIND) (list 'THAND code (list 'THPUTPROP (quotify (cadr (caddr code))) 'THVALUE ''BIND)))
        ((or (= (car code) 'THAND) (= (car code) 'THPROG)) (doall (map #'PLNR-JUNKIFY2 code)))
        ((doall (map #'PLNR-JUNKIFY code)))))

(§ defn- PLNR-JUNKIFY2 [code]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (COND
        ((term? code) (list code))
        ((= (car code) 'THGOAL) (list code '(VALUEPUT)))
        ((= (car code) 'THFIND) (list code (list 'THPUTPROP (quotify (cadr (caddr code))) 'THVALUE ''BIND)))
        ((or (= (car code) 'THAND) (= (car code) 'THPROG)) (list (doall (map #'PLNR-JUNKIFY2 code))))
        ((list (doall (map #'PLNR-JUNKIFY code))))))

(§ defn- VALUEPUT [] (PUTPROP THVALUE SENTNO 'WHO))

(§ defn- PLNR-THCONSIFY [varlist exp body]
    ;; GENERATES A CONSEQUENT THEOREM.
    (let [TH (MAKESYM 'THEOREM)]
        (PUTPROP TH
            (COND ((= (car body) 'THPROG)
                    (concat (list 'THCONSE (UNION varlist (cadr body)) exp) (cddr body)))
                (:else (list 'THCONSE varlist exp body)))
            'THEOREM)
        TH))

(§ defn- PLNR-FINDIFY [mode variable varlist body]
    ;; GENERATES A THFIND STATEMENT FOR THE NOUN GROUP DESCRIBED IN THE OSS.
    ;; IT (CURRENTLY) ASSUMES THAT THE PLNRCODE PROPERTY OF THE OSS IS A LIST
    ;; OF PATERNS OF THGOAL STATEMENTS.  MODE IS DEFINED TO BE <MODE> IN THE
    ;; MICRO-PLANNER DESCRIPTION OF THFIND (SEE AI MEMO !203A) BODY
    ;; IS A SINGLE PLANNER EXPRESSION (POSSIBLY A THAND OR THPROG).
    (COND
        ((= (car body) 'THAND) (SETQ body (cdr body)))
        ((= (car body) 'THPROG) (SETQ varlist (concat varlist (cadr body))) (SETQ body (cddr body)))
        ((SETQ body (list body))))
    ;; VARLIST = <SKELETON>
    ;; BODY = <VARIABLE DECLARATIONS>
    (concat (list 'THFIND mode (plnr-var variable) varlist) body))

(defn- plnr-findspec [x]
    ;; GENERATES PARAMETER FOR THFIND FROM THE NOTATION USED IN THE DETERMINER.
    (cond
        (number? x) x
        (memq x '(NS NPL SG-PL)) 1
        (= (car x) 'EXACTLY) (list (cadr x) (inc (cadr x)) nil)
        (= (car x) '>) (inc (cadr x))
        (= (car x) '<) (list 0 (cadr x) nil)
        :else (erterr "plnr-findspec -- FUNNY SPECIFICATION")))

(§ defn- PLNR-GOALIFY [plnrphrase]
    ;; TAKES A PLNRPHRASE AND MAKES A THGOAL STATEMENT OUT OF IT UNLESS
    ;; IT ISN'T SUPPOSED TO BE ONE.  ALSO CALLS PLNR-NOTIFY IF APPROPRIATE.
    ;; PRESENT TENSE TIME MARKERS ARE REMOVED TO SIMPLIFY THE MOST COMMON EXPRESSIONS.
    (SETQ plnrphrase (PLNR-REMTIME plnrphrase))
    (COND ((get (car plnrphrase) 'NOGOAL) plnrphrase)
        ((concat (list 'THGOAL plnrphrase '(THDBF MUMBLE)) (PLNR-RECOMMENDIFY plnrphrase)))))

(defn- plnr-mung [findexpr candidates]
    ;; DOES A HORRIBLE THING: MUNGS A THFIND EXPRESSION WHICH FINDS A NOUN GROUP REFERENCE.
    ;; IT PUTS A THAMONG EXPRESSION AS THE FIRST STATEMENT OF THE THFIND.  IF THERE IS ALREADY
    ;; A THAMONG EXPRESSION IN THE THFIND, THEN MUNG JUST CLOBBERS THE LIST IN THAT THAMONG.
    (cons (car findexpr)
        (cons (cadr findexpr)
            (cons (caddr findexpr)
                (cons (cadddr findexpr)
                    (cons (list 'THAMONG (caddr findexpr) (quotify candidates))
                        (cond (= (caaddr (cdr findexpr)) 'THFIND) (cddddr (cdr findexpr))
                            :else (cddddr findexpr))))))))

(§ defn- PLNR-NOTIFY [NEG? %PLNRPHRASE]
    ;; PUTS IN THNOT, BUT ELIMINATE DOUBLE NEGATIVES.
    (COND ((not NEG?) %PLNRPHRASE)
        ((= (car %PLNRPHRASE) 'THNOT) (cadr %PLNRPHRASE))
        ((list 'THNOT %PLNRPHRASE))))

(§ defn- PLNR-NEWBODY [x] (SETQ NEWBODY (cons x NEWBODY)))

(§ defn- PLNR-PROGIFY [varlist body]
    ;; SETS UP A THPROG OR THE SIMPLEST EQUIVALENT EXPRESSION FOR
    ;; THE PARTICULAR CASE.  BODY IS A LIST OF EXPRESSIONS
    (let [NEWBODY nil]
        (or body (RETURN nil))
        (dorun (map #'(lambda [x] (COND
                ((= (car x) 'THPROG)
                    (COND ((MEET varlist (cadr x)) (PLNR-NEWBODY x))
                        (:else (SETQ varlist (concat varlist (cadr x))) (dorun (map #'PLNR-NEWBODY (cddr x))))))
                ((= (car x) 'THAND)
                    (dorun (map #'PLNR-NEWBODY (cdr x))))
                ((PLNR-NEWBODY x))))
            body))
        (RETURN (COND
            (varlist (cons 'THPROG (cons varlist (reverse NEWBODY))))
            ((cdr NEWBODY) (cons 'THAND (reverse NEWBODY)))
            ((car NEWBODY))))))

(§ defn- PLNR-NUMREL [oss]
    ;; THIS IS USED BY PLNR-NUMSUB TO HAVE THE VARIABLE NAME SUBSTITUTED
    ;; FOR THE OSS WHICH IS THE REL OF A PARTICULAR EXPRESSION.
    (COND ((memq oss RELLIST) (SETQ RELß oss)) (oss)))

(§ defn- PLNR-NUMSUB [%ME %PLNRPHRASE]
    ;; FUNCTION WHICH SUBSTITUTES THE PROPER PARSE TIME VARIABLES
    ;; FOR !1, !2, !3, AND *** IN THE PLANNER SHCEMAS FROM DICTIONARY DEFINITIONS.
    ;;
    ;; INPUTS:
    ;;     %ME         - OSS FOR CURRENT OBJECT
    ;;     %PLNRPHRASE - A PHRASE FROM THE DICTIONARY IN WHICH SUBSTITUTIONS ARE TO BE MADE
    ;; FREE VARIABLE INPUTS:
    ;;     !1, !2, !3, CORRESPOND TO POSITIONS IN THE RESTRICTION LIST OF THE DEFINITION.
    ;;                 EACH POINTS TO A SINGLE OSS OR NIL IF NOT APPLICABLE.
    ;;     SMSUB - ONE OSS FROM SMSUB REGISTER
    ;;     SMOB1 - ONE OSS FROM SMOB1 REGISTER
    ;;     SMOB2 - ONE OSS FROM SMOB2 REGISTER
    ;;     *TIME - CURRENT TIME
    ;; VALUE:
    ;;  THE CONDENSED PLANNER CODE AFTER THE SUBSTITUTIONS HAVE BEEN MADE.
    (doall (map #'(lambda [%ELEMENT] ;; %ELEMENT IS AN ATOM OF THE PHRASE
            (COND
                ((memq %ELEMENT '(!1 !2 !3)) (PLNR-NUMREL (eval %ELEMENT)))
                ((= %ELEMENT '***) %ME)
                ((= %ELEMENT '*TIME) TIME) ;; GETS THE CURRENT TIME
                (%ELEMENT)))
        (EVALCHECK %PLNRPHRASE))))

(§ defn- PLNR-RECOMMENDIFY [%PLNRPHRASE]
    ;; LOOKS UP IN THE DICTIONARY A RECOMMENDED THEOREM TO USE IN
    ;; PROCESSING A PLNRPHRASE BY THGOAL.  IF IT FINDS ONE IT TACKS
    ;; IT ON AS A RECOMENDATION.
    (let [%ELEMENT nil]
        ;; LOOK A RELATION UP IN THE DICTIONARY.  THE ENTRIES ARE SET UP AS A PROPERTY LIST.
        ;; THERE ARE DIFFERENT RECOMMENDATIONS FOR THE SAME RELATION DEPENDING ON THE NUMBER
        ;; OF ARGUMENTS THIS INSTANCE OF IT HAS.  (LENGTH ...) COMPUTES THE NUMBER OF ARGUMENTS
        ;; + 1 AND THE (ASSQ ...) RETRIEVES THE APPROPRIATE RECOMMENDATION USING THIS NUMBER.
        ;; IF THERE IS NO SUCH NUMBER, NUMBERP FAILS AND SOME ARBITARY FUNCTION WHICH
        ;; IS STORED OUT THERE IS EVALUATED TO GIVE THE RECOMMENDATION.
        (RETURN
            (and (SETQ %ELEMENT (get (car %PLNRPHRASE) 'THMLIST))
                (eval (COND
                    ((number? (caar %ELEMENT)) (cadr (or (assq (count %PLNRPHRASE) %ELEMENT) '(nil nil))))
                    (%ELEMENT)))))))

(§ defn- PLNR-REMTIME [exp]
    ;; REMOVES ALL PRESENT TENSE TIME STRUCTURES
    ((lambda [y]
        ;; Y IS BOUND TO A UNIQUE POINTER SO IT CAN'T POSSIBLY SCREW ANYTHING
        ;; IN THE EXPRESSION WHEN IT DOES THE DELETION.  DELQ USES EQ.
        (DELQ y
            (doall (map #'(lambda [x] (COND
                ((not (term? x)) x)
                ((tss? x) (COND ((and (tense? x) (not (memq (tense? x) '((PRESENT PRESENT) (MODAL) (PRESENT))))) x) (y)))
                (x)))
            exp))))
        '(T)))

(defn- plnr-var [x]
    ;; GENERATES SYNTAX FOR VARIABLE NAMES IN PLANNER
    (list 'THV x))

(§ defn- COMPARE-BUILD [node degree]
    ;; USED BY SMADJG-PREPG TO BUILD A PSUEDO VERB FOR THE COMPARATIVE.  SMCL1 IS THEN CALLED.
    (let [RESTRICTIONSß nil DIMENSIONß nil DIRECTIONß nil]
        ;; THESE ARE THE POSSIBLE PARTS OF A MEASURE.
        (SETQQCHECK nil (cdr (FINDMEASURE node)) '(RESTRICTIONSß DIMENSIONß DIRECTIONß) 'MEASURE)
        ;; DEFINITION
        (PUTPROP 'COMPARE-PSEUDO-VERB
            (list 'RELATION
            (list 'RESTRICTIONSß
                (list (list RESTRICTIONSß)
                (list RESTRICTIONSß))
                'PROCEDUREß (list (list degree DIMENSIONß (COND (DIRECTIONß '!1) ('!2)) (COND (DIRECTIONß '!2) ('!1))))))
            'SEMANTICS)
        (RETURN '(COMPARE-PSEUDO-VERB))))

(§ defn- FINDMEASURE [node]
    ;; GETS THE MEASURE DEFINITION
    (COND
        ((SETQ X (assq 'MEASURE (get (ROOT (NB node)) 'SEMANTICS))) (cadr X))
        ((GLOBAL-ERR (str "I DON'T KNOW HOW TO COMPARE THINGS WITH RESPECT TO " (ROOT (NB node)))))))

(§ defq- MEASURE [meas]
    ;; USED TO GENERATE ORDINALS -- IT IS CALLED WHEN A MEASURE DEFINITION IS EVALLED
    (APPLY #'OBJECT
        (list (list 'MARKERSß
            (cadr (memq 'RESTRICTIONSß meas))
            'PROCEDUREß
            (list (list '*ORDINAL* meas))))))

(§ defn- PLNR-DESCRIBE [exps var FREEVARS]
    ;; BUILDS THE PLANNER DESCRIPTION, IGNORING THE QUANTIFIER ACOUNTS FOR ORDINALS, SUBSTS, ETC.
    (let [ORDINAL nil BODY nil x nil]
    =>  (COND
            ((nil? exps) (RETURN (COND (ORDINAL (ORDMAKE ORDINAL var BODY)) ((PLNR-PROGIFY nil BODY)))))
            ((= (SETQ x (EXPAND (car exps) (and (nil? (cdr exps)) (rssvar? var) (get var 'USED) (plnr-var var)))) '*ORDINAL*))
            ;; A SUBST DEFINITION IF IT IS THE ONLY THING IS TO BE APPLIED TO THE OSS TO WHICH THIS RSS WILL BE RELATED.
            ;; THE VARIABLE FOR A RELATION IS INSERTED INTO THE SECOND PLACE OF THE RELATION IF IT IS REFERRED TO ANYWHERE ELSE.
            ((and (cdr exps) (= (car x) '!SUBST)) (MAPC2 #'(lambda [x y] (SETQ exps (SUBST x y exps))) (cdr x)))
            (x (SETQ BODY (cons x BODY))))
        (SETQ exps (cdr exps))
        (GO =>)))

(§ defn- RELFIND [node]
    ;; LOOKS FOR THE REL OF A POLAR
    (let [REL nil]
        (ERRSET
            ;; IT GOESFROM THE BEGINNINGOF THE SENTENCE LOOKING FOR AN INDEFINITE NG,
            ;; EITHER AT THE TOP LEVEL OR AS A FIRST LEVEL PREPOBJ, BUT NOT A COMPLEMENT.
            (MAP #'(lambda [X] (COND
                ((ISQ X NG) (and (not (ISQ X COMP)) (not (ISQ X DEF)) (SETQ REL X) (ERR nil)))
                ((ISQ X LOBJ) (and (ISQ (H X) INDEF) (SETQ REL X) (ERR nil)))
                ((ISQ X PREPG) (and (ISQ (H X) INDEF) (SETQ REL (H X)) (ERR nil)))))
            (reverse (H node))))
        (or REL (and (CQ PASV) (not (CQ AGENT)) (SETQ REL '(FAKE-AGENT))))
        (RETURN (and REL (SM REL)))))

(§ defn- ORDMAKE [ordinal var body]
    ;; MAKES THE LOGICAL FORM FOR SUPERLATIVES.
    ;; ORDINAL GIVES THE THING BEING COMPARED IN MEASURE FORM.
    (let [NEWVAR (MAKESYM 'X)]
        (PLNR-PROGIFY nil
            (concat body (list (PLNR-NOTIFY true
                (PLNR-PROGIFY (list NEWVAR) (concat (SUBST NEWVAR var body) (list (PLNR-GOALIFY (COMPARE-PROC var NEWVAR ordinal)))))))))))

(§ defn- COMPARE-PROC [var newvar ordinal]
    (let [RESTRICTIONSß nil DIRECTIONß nil DIMENSIONß nil]
        (SETQQCHECK nil ordinal '(RESTRICTIONSß DIRECTIONß DIMENSIONß) 'MEASURE)
        (RETURN (list '!MORE DIMENSIONß (plnr-var (COND (DIRECTIONß newvar) (var))) (plnr-var (COND (DIRECTIONß var) (newvar)))))))

(§ defn- EXPAND [exp event]
    ;; THE HEART OF THE PLANNER BUILDER.
    ;; EXPANDS AN EXPRESSION WORRYING ABOUT THE QUANTIFIERS AND CONNECTIVES OF ITS CONSTITUENTS.
    ;; IT DOESN'T REALLY HANDLE EXPRESSIONS WITH MORE THAN ONE QUANTIFIED SS UNLESS ONE OF THEM IS THE REL.
    ;; THE EVENT IS NON-NIL ONLY IF THE EVENT-NAME IS TO BE INCLUDED IN THE EXPANSION OF THE EXPRESSION.
    (COND
        ((rss? exp)
            (COND
                ((and? exp) (PLNR-PROGIFY nil (doall (map #'(lambda [x] (EXPAND x nil)) (and? exp)))))
                ((or? exp) (PLNR-ORIFY (doall (map #'(lambda [x] (EXPAND x nil)) (or? exp)))))
                ((PLNR-NOTIFY (negative? exp) (PLNR-DESCRIBE (relations? exp) (variable? exp) (cons (variable? exp) FREEVARS))))))
        ((term? exp) (bug "EXPAND - ATOMIC MODIFIER"))
        ((= (car exp) '*ORDINAL*)
            (COND (ORDINAL (GLOBAL-ERR "I CAN'T HANDLE TWO ORDINALS OR SUPERLATIVES AT ONCE"))
                ((SETQ ORDINAL (cadr exp)) '*ORDINAL*)))
        ((= (car exp) '!SUBST)
            (ert "EXPAND - IS !SUBST BEING HANDLED BY SOMEONE ELSE?")
            exp)
        ((let [BODY nil QUANTIFIER nil CHOICE nil VAR nil MULTIPLE (eval (get (car exp) 'MULTIPLE))]
            (SETQ exp
                (doall (map #'(lambda [x] (COND
                    ((or (not (term? x)) (not (or (rss? x) (oss? x))))
                        x)
                    ((refer? x)
                        (COND ((cdr (refer? x))
                            (COND (MULTIPLE (ERQSET 'AND) (SETQ CHOICE (refer? x)) '*AND*)
                                ((refer? x))))
                        ((car (refer? x)))))
                    ((memq (variable? x) FREEVARS)
                        (and (rssvar? (variable? x)) (PUTPROP (variable? x) true 'USED))
                        (plnr-var (variable? x)))
                    ((SETQ CHOICE (and? x))
                        (ERQSET 'AND)
                        (and MULTIPLE (refer? x) (SETQ CHOICE (refer? x)))
                        '*AND*)
                    ((SETQ CHOICE (or? x))
                        (ERQSET 'OR)
                        '*OR*)
                    ((COND
                        ((rss? x) (ERQSET 'EVENT) (PUTPROP (variable? x) true 'USED))
                        ((memq (quantifier? x)
                            '(ALL NO)) (ERQSET (quantifier? x)) true)
                        ((memq (quantifier? x) '(NDET INDEF))
                            (COND ((memq (num? x) '(NS SG-PL)) (ERQSET 'INDEF)) ((SETQ CHOICE (plnr-findspec (num? x))) (ERQSET 'FIND))) true))
                        (SETQ BODY (PLNR-DESCRIBE (relations? x) (variable? x) (cons (variable? x) FREEVARS)))
                        (plnr-var (SETQ VAR (variable? x))))
                    ((erterr "EXPAND - STRANGE QUANTIFIER"))))
                    (COND (event (cons (car exp) (cons event (cdr exp)))) (:else exp)))))
            ;; THE EVENT NAME IS STUCK INTO THE SECOND POSITION IF THERE IS ONE.
            (RETURN (COND
                ((nil? QUANTIFIER) (PLNR-GOALIFY exp))
                ((= QUANTIFIER 'AND)
                    (PLNR-PROGIFY nil (doall (map #'(lambda [x] (EXPAND (SUBST x '*AND* exp) nil)) CHOICE))))
                ((= QUANTIFIER 'OR)
                    (PLNR-ORIFY (doall (map #'(lambda [x] (EXPAND (SUBST x '*OR* exp) nil)) CHOICE))))
                ((= QUANTIFIER 'FIND)
                    (PLNR-FINDIFY CHOICE VAR (list VAR) (PLNR-PROGIFY nil (cons BODY (list (PLNR-GOALIFY exp))))))
                (:else
                    (PLNR-NOTIFY (memq QUANTIFIER '(ALL NO))
                        (PLNR-PROGIFY (and VAR (list VAR)) (cons BODY (list (PLNR-NOTIFY (= QUANTIFIER 'ALL) (PLNR-GOALIFY exp)))))))))))))

(§ defn- ERQSET [x]
    ;; USED BY EXPAND TO MAKE SURE IT ISN'T GETTING CONFUSED BY TOO
    ;; MANY CONNECTIVES AND QUANTIFIERS IN THE SAME EXPRESSION.
    (COND (QUANTIFIER (GLOBAL-ERR "I CAN'T HANDLE COMBINATIONS OF QUANTIFIERS AND CONNECTIVES WHICH ARE SO COMPLICATED"))
        ((SETQ QUANTIFIER x))))

(§ defn- SETQQCHECK [%EVALFLAG %LIST %CHECKLIST %NAME]
    ;; SETQQCHECK IS LIKE SETQQ (OR LIKE SETQ DEPENDING ON EVALFLAG) BUT IT CHECKS TO MAKE SURE
    ;; THE VARIABLE NAME IS A MEMBER OF THE %CHECKLIST, AND IF NOT PRINTS AN ERROR MESSAGE.
    (let [%X nil]
    GO  (COND ((nil? %LIST) (RETURN true))
            ((memq (car %LIST) %CHECKLIST)
                (SET (car %LIST) (COND (%EVALFLAG (eval (cadr %LIST))) (:else (cadr %LIST))))
                (SETQ %LIST (cddr %LIST))
                (GO GO))
            (:else (SETQ %X (ert (str (car %LIST) " IS NOT A LEGAL SPECIFICATION FOR " %NAME)))))
    UP  (COND
            ;; A QUESTION MARK GETS THE LIST OF POSSIBILITIES PRINTED OUT, THEN LETS YOU TRY AGAIN.
            ;; TO DO THIS YOU MUST TYPE (RETURN '?) AT THE ERT.
            ;; IF YOU RETURN ANY OTHER VALUE, IT ASSUMES THIS IS THE VARIABLE NAME INTENDED,
            ;; OTHERWISE IT JUST CAUSES AN ERROR.
            ((= %X '?)
                (PRINT %CHECKLIST)
                (SETQ %X (ert "FOO: SETQQCHECK ????"))
                (GO UP))
            ((SETQ %LIST (cons %X (cdr %LIST))) (GO GO)))))

(§ defn- THVAL2 [WHO aa]
    (let [RESULT nil x nil]
        (SETQ x (SETQ RESULT '(nil)))
        (and PLANNERSEE (DISP aa))
        (and (not (= RESULT x))
            (RETURN RESULT))
        (SETQ RESULT (THVAL aa '((EV COMMAND))))
        RESULT))

(§ defn- WHO [x]
    (COND ((nil? WHO))
        ((term? x))
        ((not (SETQ x (get x 'WHO))) nil)
        ((= WHO 'HE))
        ((< (car WHO) x LASTSENTNO))))

(§ defn- CHECK [new-markers MARKERS SYSTEMS]
    ;; TAKES A LIST OF NEW MARKERS AND CHECKS FOR COMPATIBILITY WITH THE EXISTING
    ;; MARKERS AND SYSTEMS (AS GIVEN BY ARGS MARKERS AND SYSTEMS).  IF COMPATIBLE,
    ;; RETURNS A TWO-LIST OF THE NEW MARKERS AND SYSTEMS, ELSE RETURNS NIL.
=>  (COND
        ((nil? new-markers) (RETURN (list MARKERS SYSTEMS)))
        ((CHECKAMARKER (car new-markers)) (SETQ new-markers (cdr new-markers)) (GO =>))
        (:else (RETURN nil)))) ;; FAIL IF CHECKAMARKER FAILS

(§ defn- CHECKAMARKER [marker]
    ;; CHECKS A SINGLE MARKER FOR COMPATIBILITY
    ;; USES FREE VARIABLES:
    ;;    SYSTEMS - THE SYSTEM LIST SO FAR
    ;;    MARKERS - THE MARKER LIST SO FAR
    ;; IF SUCCESSFULL, THE MARKER AND ITS SYSTEM(S) ARE APPENDED TO THESE FREE VARIBLES
    (let [NEW-SYSTEMS nil]
        (COND ((memq marker MARKERS) (RETURN true)))               ;; IF MARKER ALREADY THERE, FINE
        (SETQ MARKERS (cons marker MARKERS))                    ;; ADD NEW MARKER TO LIST
        (SETQ NEW-SYSTEMS (get marker 'SYS))                    ;; GET THE SYSTEMS OF THE NEW MARKER
    =>  (COND ((nil? NEW-SYSTEMS) (RETURN true))
            ((memq (car NEW-SYSTEMS) SYSTEMS) (RETURN nil)) ;; FAIL IF SYSTEM THERE BY ANOTHER PATH
            ((CHECKAMARKER (car NEW-SYSTEMS))
                (SETQ SYSTEMS (cons (car NEW-SYSTEMS) SYSTEMS))
                (SETQ NEW-SYSTEMS (cdr NEW-SYSTEMS))
                (GO =>))
            (:else (RETURN nil)))))

(§ defn- FINDEVENTS [rss]
    ;; FINDS ALL THE EVENTS FITTING THE RSS DESCRIPTION
    (PUTPROP (variable? rss) true 'USED)
    (THVAL2 nil
        (PLNR-FINDIFY 'ALL
            (variable? rss)
            (list (variable? rss))
            (PLNR-DESCRIBE (relations? rss)
                (variable? rss)
                (list (variable? rss))))))

(§ defn- CHECKREL [oss]
    ;; CHECKS FOR POSSIBLE RELATIVE, EITHER BECAUSE OSS IS ON THE RELLIST,
    ;; OR BECAUSE RSS INVOLVES INSIDE IT AN OSS ON THE RELLIST.
    ;; IT RETURNS EITHER NIL OR A LIST OF WHICH THE FIRST ELEMENT IS THE REAL RELATIVE.
    ;; IT USES THIS FACT TO CHEAT ON RECURSION BY USING MAPCAN.
    (COND
        ((oss? oss) (memq oss RELLIST))
        ((rss? oss) (doall (map #'(lambda [RELATION] (COND ((term? RELATION) nil) ((doall (map #'CHECKREL RELATION))))) (relations? oss))))))

#_(ns shrdlu.smass)

;; ################################################################
;;
;;               SMASS - SEMANTIC ACCESS FUNCTIONS
;;
;; ################################################################

(defn- action? [x]
    ;; THE WORKING PART OF AN ANSWER -- TELLS WHAT TO DO IF THE ANSWER IS THE ONE TO BE GIVEN.
    ;; MIGHT INCLUDE ACTIONS ON THE DISPLAY, AS WELL AS THINGS TO BE PRINTED AND VARIABLES TO BE SET, ETC.
    ;; THE WHOLE THING IS EVAL-LISTED.
    (get x 'ACTION=))

(defn- ambiguities? [x]
    ;; LIST OF POSSIBLE AMBIGUITIES FOR A SEMANTIC STRUCTURE.
    (get x 'AMBIGUITIES=))

(defn- and? [x]
    ;; FIND THE CONJUNCTION LIST OF A CONJOINED SEMANTIC STRUCTURE.
    ;; NIL IF THERE IS NONE.
    (get x 'AND=))

(defn- ansrss? [x]
    ;; THE RSS CORRESPONDING TO AN ANSWER NODE (A PROPERTY OF THE ANSNODE)
    (get x 'ANSRSS=))

(defn- determiner? [x]
    ;; ACCESS FUNCTION.  GETS DET OF AN OSS.
    (get x 'DETERMINER=))

(defn- end? [tss]
    ;; END TIME FOR TSS.
    (get tss 'END=))

(defn- markers? [%SENSE]
    ;; ACCESS FUNCTION USED TO GET MARKERS FROM OSS OR RSS.
    (get %SENSE 'MARKERS=))

(defn- modifiers? [%XSS]
    ;; ACCESS FUNCTION FOR GETTING THE PLANNER CODE SCHEMA OF AN OSS OR RSS.
    (get %XSS 'MODIFIERS=))

(defn- negative? [%XSS]
    ;; ACCESS FUNCTION FOR OSS.
    (get %XSS 'NEGATIVE=))

(defn- num? [oss]
    ;; GETS THE NUMBER FIELD OF AN OSS.
    (car (get oss 'DETERMINER=)))

(defn- or? [x]
    ;; ACCESS FOR LIST OF CONSTITUENTS IN DISJOINED SEMANTIC STRUCTURE.
    ;; NIL IF IT ISN'T.
    (get x 'OR=))

(defn- oss? [x]
    ;; CHECKS TO SEE IF X IS AN OSS.
    (get x 'OSSNODE=))

(§ defn- PARENT? [node] (GETR 'PARENT node))

(defn- parsenode? [x] (get x 'PARSENODE=))

;; THE PARSE NODE ASSOCIATED WITH THE SEMANTIC STRUCTURE.

(defn- plausibility? [%XSS]
    ;; ACCESS FUNCTION FOR GETTING PLAUSIBILITY OF AN OSS OR RSS.
    (or (get %XSS 'PLAUSIBILITY=) 0))

(defn- plnrcode? [x] (get x 'PLNRCODE=))

;; THE PLANNERCODE GENERATED WHEN AN OBJECT IS ACTUALLY LOOKED FOR IN THE DATA BASE.
;; IT IS NOT USED AGAIN, BUT IS LEFT SITTING AROUND FOR PEOPLE TO LOOK AT.

(defn- qtype? [x] (caddr (get x 'DETERMINER=)))

;; QUESTION TYPE FOR QUESTION OSS.

(defn- quantifier? [oss]
    ;; GETS THE DETERMINER FIELD OF AN OSS.
    (cadr (get oss 'DETERMINER=)))

(defn- refer? [%XSS]
    ;; ACCESS FUNCTION FOR REFER OF OSS OR RSS.
    (get %XSS 'REFER=))

(defn- rel? [x] (get x 'REL=))

;; THE OBJECT TO WHICH THIS DESCRIPTION IS TO BE RELATED.

(defn- relations? [x]
    ;; THE MATERIAL THAT WILL BECOME PLANNER CODE.
    (get x 'RELATIONS=))

(defn- relmarkers? [x]
    ;; MARKERS HELD BY A RELATIVE CLAUSE PRODUCED BY ITS SELECTION
    ;; RESTRICTIONS, TO BE ATTACHED TO THE OBJECT DESCRIPTION.
    (get x 'RELMARKERS=))

(defn- rss? [x]
    ;; CHECKS TO SEE IF X IS AN RSS.
    (get x 'RSSNODE=))

(defn- rssvar? [x]
    ;; A VARIABLE OF THE TYPE USED FOR RSS'S -- I.E. EVX3, ETC.
    (get x 'RSSVAR))

(defn- start? [tss]
    ;; START TIME FOR TSS.
    (get tss 'START=))

(defn- systems? [%SENSE]
    ;; ACCESS FUNCTION FOR GETTING THE SYSTEMS OF AN OSS OR RSS.
    (get %SENSE 'SYSTEMS=))

(defn- tense? [x]
    ;; FOR A TSS.
    (get x 'TENSE=))

(defn- tss? [x] (get x 'TSSNODE=))

;; ASSOCIATED WITH EVERY TIME SEMANTIC STRUCTURE.

(defn- variable? [x]
    ;; ACCESS FUNCTION FOR GETTING THE VARIABLE NAME ASSOCIATED WITH AN OSS OR RSS.
    (get x 'VARIABLE=))

(§ defn- SMSET [x] (SETR 'SEMANTICS x C) (SETQ SM x))

#_(ns shrdlu.newans)

;; ################################################################
;;
;;              NEWANS - (NEW) ANSWERING COMPONENT
;;
;; ################################################################

(§ defn- ANSWER [node]
    ;; THE TOP LEVEL ANSWER FUNCTION CALLED TO CARRY OUT THE RESULTS OF ANY INPUT SENTENCE,
    ;; WHETHER COMMAND, QUESTION, OR STATEMENT.
    (let [anslist nil AMBIG nil]
        ;; ANSLIST IS THE LIST OF POSSIBLE ANSWERS.
        ;; AMBIG IS A FLAG SET IF THERE IS A POSSIBLE AMBIGUITY.
        ;; CLEAR OUT ANSWER NAMES SAVED FOR BACKREF(ERENCE), I.E. MORE THAN ONE RSS FOR THE SENTENCE.
        (SETQ ANSNAME nil)
        (SETQ AMBIG (cdr (SM node)))
        (SETQ anslist (ANSORDER (ansunique (doall (map #'ANSGEN (SM node))))))
        ;; ANSGEN GENERATES AN ANSWER FOR EACH INTERPRETATION.
        ;; ANSUNIQUE TAKES OUT REDUNDANT ONES IN THE CASE THAT DIFFERENT INTERPRETATIONS LEAD TO THE SAME ANSWER.
        ;; ANSORDER ORDERS THE REMAINING ONES BY PLAUSIBILITY.
        ;; IF NO ANSWER IS CLEARLY BEST, ASK THE USER FOR CLARIFICATION AND TRY AGAIN.
    =>  (COND ((and (cdr anslist) (not (enough-better (car anslist) (cadr anslist))))
                (SETQ anslist (ANSELIMINATE anslist))
                (GO =>)))
        ;; THE ACTION INCLUDES BOTH THE THINGS TO BE DONE
        ;; AND THE INSTRUCTIONS FOR PRINTING A RESPONSE.
        (evlis (action? (car anslist)))
        (PRINC '\.)
        (TERPRI)
        ;; DOBACKREF STORES AWAY DISCOURSE INFORMATION.
        (DOBACKREF (car anslist))
        true))

(§ defn- AMBPUT [code]
    ;; PUTS IN THE JUNK FOR DISCOURSE IF THERE IS NO AMBIGUITY, SO THERE IS
    ;; NO NEED TO EVALUATE THE CODE A SECOND TIME WHEN GIVING THE ANSWER.
    (COND (AMBIG code) (:else (PLNR-JUNKIFY code))))

(defn- ansay [x]
    ;; GENERATES THE SYNTAX FOR ANSWER ACTIONS FROM A PHRASE.
    (list (cons 'SAY x)))

(§ defn- ANSBUILD [PLAUS ACTION REDEDUCE]
    ;; BUILDS AN ANSWER NODE.
    ;; IF REDEDUCE IS NON-NIL, IT ADDS A REDEDUCTION OF THE ANSWER,
    ;; ADDING THE DISCOURSE JUNK TO THE ACTION.
    (BUILD
        ANSNODE= (MAKESYM 'ANS)
        PLAUSIBILITY= PLAUS
        ANSRSS= RSS
        ACTION= (concat
                    (COND
                        ((and AMBIG REDEDUCE (not (CQ DECLAR)))
                            (cons (list 'THVAL2 nil (list 'PLNR-JUNKIFY (list 'plnrcode? (list 'quote RSS)))) ACTION))
                        (:else ACTION))
                    (and (rel? RSS) (not (CQ DECLAR)) (list (list 'PUTPROP (quotify (rel? RSS)) (quotify ANS) (quotify 'REFER=)))))))

(§ defn- ANSCOMMAND [RSS]
    ;; ANSCOMMAND RESPONDS TO IMPERATIVES.
    ;; PLNR-ANDORIFY COMBINES ANDS AND ORS INTO APPROPRIATE PLANNER THANDS AND THORS.
    (let [exp (PLNR-ANDORIFY RSS) ANS nil SUCCESS nil PLAN nil PLAN2 nil]
        (PUTPROP RSS exp 'PLNRCODE=)
        (SETQ exp (AMBPUT exp))
        (SETQ exp (COND
            ((= (car exp) 'THAND) (concat exp '((SETQ SUCCESS true) (SETQ PLAN2 PLAN))))
            (:else (list 'THAND exp '(SETQ SUCCESS true) '(SETQ PLAN2 PLAN)))))
        ;; IN CASE OF MULTIPLE INTERPRETATION, THE SYSTEM USES FAILURE TO WIPE OUT THE EFFECTS OF TRYING OUT ONE OF THEM.
        ;; BEFORE FAILING IT MARKS DOWN WHETHER IT SUCCEEDED AND SAVES THE PLAN FROM BACKTRACKING.
        ;; PLNR-JUNKIFY PUTS ON THE JUNK FOR SAVING THE DISCOURSE REFERENTS ETC.
        (THVAL2 nil (COND (AMBIG (concat exp '((THFAIL)))) (:else exp)))
        (RETURN
            ;; THE THIRD ARGUMENT TO ANSBUILD CAUSES THE SYSTEM TO GO BACK THROUGH THE DEDUCTION
            ;; TO GET THE DATA BASE STRAIGHT IF THIS ANSWER IS PICKED.  IT ALSO TAKES CARE OF THE BACKREF STUFF.
            (ANSBUILD
                (COND (SUCCESS (plausibility? RSS)) (:else (- (plausibility? RSS) 512)))
                (COND (SUCCESS (concat (reverse PLAN2) '((SAY OK)))) (:else '((SAY I CAN'T))))
                true))))

(§ defn- ANSDECLARE [RSS]
    ;; FOR DECLARATIVES.
    (COND
        ((or? RSS)
            (GLOBAL-ERR "I DON'T UNDERSTAND DISJUNCTIVE DECLARATIVES"))
        ((and? RSS)
            (let [ANS nil]
                ;; CONJOINED DECLARATIVES ARE HANDLED BY DOING EACH ONE SEPARATELY.
                (SETQ ANS (doall (map #'ANSDECLARE (and? RSS))))
                (RETURN (ANSBUILD
                    (reduce #'+ (map #'plausibility? ANS))
                    (cons '(SAY I UNDERSTAND) (doall (map #'(lambda [x] (DELETE '(SAY I UNDERSTAND) (action? x))) ANS)))
                    nil))))
        ((not (ISTENSE (parsenode? RSS) 'PRESENT))
            (GLOBAL-ERR "I ONLY UNDERSTAND PRESENT TENSE DECLARATIVES"))
        (:else (ANSBUILD
            (plausibility? RSS)
            ;; ANSTHM GENERATES THE APPROPRIATE ASSERTION OR THEOREM.
            (cons '(SAY I UNDERSTAND) (doall (map #'(lambda [x] (list 'THADD (quotify (ANSTHM x)) nil)) (relations? RSS))))
            nil))))

(§ defn- ANSELIMINATE [anslist]
    ;; ELIMINATES ANSWERS FROM LIST BY ASKING PERSON TO CLEAR UP THE AMBIGUITIES.
    (let [AMB nil POSSIBILITIES nil XX nil]
        (or (SETQ AMB (ambiguities? (ansrss? (car anslist))))
            (bug "ANSELIMINATE -- NO AMBIGUITIES LIST"))
        ;; POSSIBILITIES IS THE LIST OF POSSIBLE INTERPRETATIONS FOR A SINGLE AMBIGUITY.
        ;; WE ARE INSIDE A LOOP STARTING AT UP WHICH GOES THROUGH ALL THE DIFFERENT POSSIBLE AMBIGUITIES ON THE LIST FOR THE FIRST ANSWER ON ANSLIST.
        ;; ON EACH ANSWER WE LOOK FOR POSSIBLE INTERPRETATIONS FOR THE PARTICULAR NODE WHERE THE AMBIGUITY WAS CREATED.
    UP  (SETQ POSSIBILITIES (list (car AMB)))
        (dorun (map #'(lambda [ANS]
            (and (SETQ XX (PARSE-ASSOC (caar AMB) (ambiguities? (ansrss? ANS))))
                (not (memq XX POSSIBILITIES))
                (SETQ POSSIBILITIES (cons XX POSSIBILITIES))))
            (cdr anslist)))
        (COND ((cdr POSSIBILITIES) true)
            ((SETQ AMB (cdr AMB)) (GO UP))
            (:else (bug "ANSELIMINATE -- NO CONFLICT")))
        (TERPRI)
        (SAY I'M NOT SURE WHAT YOU MEAN BY \") ;; "sic!
        (dorun (map #'PRINT2 (FROM (NB (caddar AMB)) (N (caddar AMB)))))
        (SAY \" IN THE PHRASE \") ;; "sic!
        (dorun (map #'PRINT2 (FROM (NB (SETQ XX (PARENT? (caddar AMB)))) (N XX))))
        (PRINC "\".")
        (TERPRI)
        (SAY DO YOU "MEAN:")
        (SETQ XX 0)
        (dorun (map #'(lambda [POSS]
                (PRINT (SETQ XX (inc XX)))
                (dorun (map #'PRINT2 (cadr POSS)))) ;; THE PARAPHRASE
            POSSIBILITIES))
        (PRINC '?)
        (TERPRI)
    READ (SETQ XX (READ))
        (COND ((or (not (number? XX)) (> XX (count POSSIBILITIES)))
            (TERPRI)
            (SAY PLEASE TYPE ONE OF THE NUMBERS)
            (TERPRI)
            (GO READ)))
        (SETQ POSSIBILITIES (NTH XX POSSIBILITIES))
        (RETURN
            (MAPBLAND #'(lambda [ANS]
                (COND ((or (not (SETQ XX (PARSE-ASSOC (caar AMB) (ambiguities? (ansrss? ANS))))) (= XX POSSIBILITIES)) ANS)))
            anslist))))

(§ defn- PARSE-ASSOC [oss ambig-list]
    ;; PARSE-ASSOC GOES THRU AMBIG-LIST LOOKING FOR AN INTERPRETATION WITH THE SAME PARSE NODE
    (let [ass (car (parsenode? oss))]
    =>  (COND ((nil? ambig-list) (RETURN nil))
            ((= ass (car (parsenode? (caar ambig-list))))
                (RETURN (car ambig-list))))
        (SETQ ambig-list (cdr ambig-list))
        (GO =>)))

(§ defn- ANSGEN [rss]
    ;; ANSGEN GENERATES AN ANSWER FOR A SINGLE INTERPRETATION.
    (COND
        ((or (CQ IMPER) (and (CQ QUEST) (ISTENSE (parsenode? rss) 'FUTURE))) ;; FUTURE QUESTIONS ARE TREATED LIKE COMMANDS.
            (ANSCOMMAND rss))
        ((CQ DECLAR)
            (let [x nil]
                (RETURN (COND
                    ((ERRSET (SETQ x (ANSDECLARE rss))) x)
                    ;; THIS STRANGE CONSTRUCTION ALLOWS US A SECOND CHANCE ON DECLARATIVES ABOUT THINGS WHICH CAN'T
                    ;; BE TOLD TO THE SYSTEM.  IF IT RUNS INTO ONE OF THEM, IT TRIES TO ANSWER IT AS A QUESTION.
                    ((= GLOBAL-MESSAGE "THAT ISN'T THE KIND OF THING I CAN BE TOLD") (ANSQUEST rss))
                    ((ERR nil))))))
        ((CQ QUEST) (ANSQUEST rss))
        ((bug "ANSGEN -- WHAT KIND OF SENTENCE IS THIS?"))))

(§ defn- ANSNAME [phrase]
    ;; THIS IS THE FUNCTION WHICH PARSES THE NAME PHRASES GENERATED BY THE ANSWER ROUTINES,
    ;; SO THAT THEY CAN BE USED AS REFERENTS FOR PRONOUNS (IT THEY ONE).  ITS INPUT IS A TWO-LIST.
    ;; THE SECOND MEMBER IS THE ACTUAL REFERENT OF THE PHRASE.  THE FIRST IS A LIST OF COMMANDS
    ;; FOR SAYING THE NAME OF AN OBJECT(S).  THE FIRST MEMBER OF THIS COMMAND LIST IS GUARANTEED
    ;; (BY ANSWER, VIA TW) TO BE A "SAY" COMMAND WHICH ENDS WITH THE HEAD NOUN OF THE PHRASE.
    ;; NOTE THAT ANSNAME IS CALLED BEFORE ONEIFYING AND ITIFYING AND THE REST OF THAT CRAP.
    ;;
    ;; ANSNAME WORKS BY CALLED PARSE NG ON THE FIRST COMMAND OF THE LIST.  IT WANTS TO HAVE
    ;; A PARSENODE AND AN OSSNODE BUILT UP FOR THE OBJECTS.  HOWEVER, IT DOES NOT WANT REFERENT
    ;; ASSIGNMENT DONE BY SMNG3, SINCE IT ALREADY KNOWS THE REFERENT.  THE FEATURE "ANSNAME"
    ;; IS ADDED TO THE INITIAL NG PARSE LIST SPECIFICALLY SO SMNG3 WILL IGNORE THIS NOUN GROUP.
    ;;
    ;; THE WAY ANSNAME WORKS IS THE DECLARE A LOT OF THE RELAVENT PARSE FREE VARIABLES, SO THAT
    ;; IT LOOKS A LITTLE LIKE SHRDLU.  THE CRITICAL VARIABLES ARE:
    ;;
    ;; CUT - WHICH TELLS THE NG GUY HOW FAR TO GO.
    ;; N   - WHICH CONTAINS THE CURRENT SENTENCE.
    ;; C   - WHICH CONTAINS THE PARENT OF THE NEXT NODE.
    ;;       WE WANT C TO BE NIL TO STOP THE NG PROGRAM FROM CRAWLING OVER THE PARSE TREE.
    (let [ansnode nil C nil N nil CUT nil]
        (SETQ N (cdaar phrase))                                     ;; CDR IS TO REMOVE "SAY"
        (SETQ ansnode (PARSE2 '(NG ANSNAME) true))                     ;; THE T SAYS NOT TO ATTACH THIS TO THE TREE
        (or ansnode
            (RETURN (ert "ANSNAME: FAILURE TO PARSE ANSWER NAME, BUT IF YOU ONLY EXPECT THE ANSWER TO BE AN ADJ, PROCEED THIS AND DON'T WORRY")))
        (SETQ ANSNAME (concat ansnode ANSNAME))                     ;; LEAVE NODE AROUND IT ACCESSABLE PLACE
        (PUTPROP (car (SM ansnode)) (cadr phrase) 'REFER=)          ;; PUT THE REFERENT ON AS THE GUY GIVEN BY ANSWER
        nil))

(§ defn- ANSNOREL [rss]
    ;; FOR QUESTIONS WITH NO RELATIVE, LIKE "DID YOU PICK UP THE BLOCK?" OR "WHY DID YOU DO THAT?"
    (let [ans nil type nil code nil NODE (parsenode? rss) var nil]
        (SETQ type (COND ;; THE TYPE SHOULD BE POLAR, WHY, WHERE, WHEN, OR HOW.
            ((ISQ NODE POLAR) 'POLAR)
            ((SETQ type (GETR 'QADJ NODE)) (car (NB type)))
            ((bug "ANSNOREL -- FUNNY TYPE"))))
        (PUTPROP (variable? rss) true 'USED)
        (SETQ code
            (PLNR-DESCRIBE (relations? rss)
                ;; IN PRESENT TENSE CASES, WE DON'T LOOK FOR EVENTS.
                ;; OTHERWISE WE LOOK FOR A SET OF APPROPRIATE EVENTS NO MATTER WHAT THE TYPE.
                (COND ((ISTENSE NODE 'PRESENT) nil) ((SETQ var (variable? rss))))
                (list (variable? rss))))
        (PUTPROP rss code 'PLNRCODE=)
        (RETURN (COND
            ((not var)
                (SETQ ans (THVAL-MULT (AMBPUT code)))
                (ANSBUILD
                    (+ (car ans) (plausibility? rss))
                    (COND ((cadr ans) '((SAY YES))) ((ISTENSE NODE 'MODAL) '((SAY I DON'T KNOW))) (:else '((SAY NO))))
                    true))
            ((SETQ ans (THVAL-MULT (PLNR-FINDIFY 'ALL var (list var) (AMBPUT code))))
                (ANSBUILD
                    ;; AN ANSWER IS VERY IMPLAUSIBILE IF IT MENTIONS AN EVENT THE SYSTEM CAN'T FIND.
                    (COND ((cadr ans) (+ (plausibility? rss) (car ans))) (:else (- (plausibility? rss) 512)))
                    (COND ((nil? (cadr ans)) '((SAY I CAN'T DISCUSS A NON-EXISTENT EVENT)))
                        ((concat (and (= type 'POLAR) '((SAY YES))) (list (list 'evlis (list 'DESCRIBEVENT (quotify (cadr ans)) (quotify type)))))))
                    true))))))

(§ defn- ANSORDER [l]
    ;; ORDERS A LIST BY PLAUSIBILITY HIGHEST FIRST.
GO  (let [x l]
    UP  (COND ((nil? (cdr x)) (RETURN l))
            ((< (plausibility? (car x)) (plausibility? (cadr x)))
                (let [y (car x)] (RPLACA x (cadr x)) (RPLACA (cdr x) y) (GO GO)))
            ((SETQ x (cdr x)) (GO UP)))))

(§ defn- ANSQUEST [RSS]
    ;; ANSQUEST ANSWERS ALL TYPES OF QUESTIONS BY SENDING THEM OUT
    ;; TO ANSREL OR ANSNOREL DEPENDING ON WHETHER THERE IS A REL.
    (cond (or (or? RSS) (and? RSS))
        (let [ans (doall (map #'ANSQUEST (or (and? RSS) (or? RSS))))]
            (ANSBUILD
                (reduce #'+ (map #'plausibility? ans))
                (concat
                    (and (not (ISQ (parsenode? RSS) COMPONENT)) '((SAY YOU'RE TRYING TO CONFUSE ME\.)))
                    (doall (map #'(lambda [QUEST]
                        (concat '((TERPRI))
                            (ansay (ELIZA (FROM (NB (parsenode? (ansrss? QUEST))) (N (parsenode? (ansrss? QUEST))))))
                            '((PRINC '?) (TERPRI))
                            ;; CONJOINED QUESTIONS ARE HANDLED BY SIMPLY REPEATING EACH PART AND ANSWERING IT SEPARATELY.
                            (action? QUEST)))
                    ans)))
                nil))
        (rel? RSS)
            (ANSREL RSS)
        :else
            (ANSNOREL RSS)))

(§ defn- ANSREL [RSS]
    ;; ANSREL HANDLES ALL QUESTIONS WITH A RELATIVE NG OF ANY TYPE.
    (let [TYPE nil REL nil CODE nil PLAUS nil ANS nil PHRASE nil LENGTH nil NUM nil]
        (or (SETQ REL (rel? RSS)) (bug "ANSREL -- NO REL"))
        (SETQ PHRASE (cons 'nil (HEADPART (parsenode? REL))))
        ;; THIS IS FOR THE PART OF THE GENERATOR THAT WILL SUBSITUTE "ONE" FOR NOUN NAMES.
        ;; THE LEADING NIL IS TO MAKE THIS PHRASE COMPATIBLE WITH THE "SAY" PHRASES WHICH THE OTHER PARTS GENERATE.
        ;; UNIVERSALS ARE CONVERTED TO NOT THERE EXISTS NOT.
        (SETQ TYPE (or (qtype? REL) (quantifier? REL) (bug "ANSREL -- NO TYPE")))
        (and (= TYPE 'ALL) (PUTPROP RSS true 'NEGATIVE=))
        (PUTPROP RSS
            (SETQ CODE
                (PLNR-FINDIFY 'ALL (variable? REL) (list (variable? REL)) (PLNR-DESCRIBE (cons RSS (relations? REL)) (variable? REL) (list (variable? REL)))))
            'PLNRCODE=)
        ;; CONSING THE RSS ONTO THE THINGS TO BE DESCRIBED HAS THE EFFECT OF PUTTING THE RELATION INTO THE DESCRIPTION OF THE OBJECT.
        ;; DISAMB PUTS IN THE JUNK IF THERE IS NO AMBIGUIT, AVOIDING HAVING TO GO THROUGH THE EVALUATION A SECOND TIME.
        ;; THVAL-MULT RETURNS A LIST OF A PLAUSIBILITY AND AN ANSWER.
        (SETQ ANS (THVAL-MULT (AMBPUT CODE)))
        (SETQ PLAUS (car ANS))
        (SETQ LENGTH (count (SETQ ANS (cadr ANS))))
        (RETURN (COND
            ((= TYPE 'ALL)
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS))
                    (COND ((nil? ANS) '((SAY YES))) ((cons '(SAY NO, NOT) (PREPPUT (namelist PHRASE 'INDEF ANS)))))
                    true))
            ((= TYPE 'HOWMANY)
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS))
                    (PREPPUT (NAMESUGAR LENGTH REL))
                    true))
            ((memq TYPE '(WHICH WHAT))
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS) (COND (ANS 512) (0)))
                    (PREPPUT (namelist PHRASE 'DEF ANS))
                    true))
            ((= TYPE 'INDEF)
                (SETQ NUM (num? REL))
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS))
                    (COND
                        ((memq NUM '(NS SG-PL))
                            (COND
                                ((nil? ANS) (COND ((ISTENSE (parsenode? RSS) 'MODAL) '((SAY I DON'T KNOW))) (:else '((SAY NO)))))
                                (:else (concat '((SAY YES,)) (COND
                                    ((ISTENSE (parsenode? RSS) 'MODAL) nil)
                                    ((PREPPUT (concat (and (cdr ANS) (concat (NAMESUGAR LENGTH REL) '((PRINC \:)))) (namelist PHRASE 'INDEF ANS)))))))))
                        ((number? NUM)
                            ;; THIS IS THE CASE WHERE WE ARE CAGEY AND AVOID ANSWERING YES OR NO.
                            ;; THE NUMBER ISN'T REPEATED IF IT IS THE SAME AS THE NUMBER IN THE SPECIFICATION.
                            (concat
                                (COND
                                    ((= NUM LENGTH) '((SAY YES,)))
                                    ((> LENGTH NUM) nil)
                                    ((zero? NUM) '((SAY NO,)))
                                    (:else '((SAY NO, ONLY))))
                                (COND
                                    ((= NUM LENGTH) nil)
                                    (:else (PREPPUT (concat (NAMESUGAR LENGTH REL) '((PRINC \:))))))
                                (PREPPUT (namelist PHRASE 'INDEF ANS))))
                        ((= (car NUM) 'EXACTLY)
                            (COND ((= LENGTH NUM) '((SAY YES)))
                                (:else (cons '(SAY NO,) (PREPPUT (NAMESUGAR LENGTH RES))))))
                        ((= (car NUM) '>)
                            (cons (COND
                                    ((> LENGTH NUM) '(SAY YES,))
                                    ((zero? LENGTH) '(SAY NO,))
                                    (:else '(SAY NO, ONLY)))
                                (PREPPUT (NAMESUGAR LENGTH REL))))
                        ((= (car NUM) '<)
                            (cons (COND
                                    ((< LENGTH NUM) '(SAY YES,))
                                    (:else '(SAY NO,)))
                                (PREPPUT (NAMESUGAR LENGTH REL))))
                        ((ert "ANSREL -- FUNNY NUMBER")))
                    true))
            ((ert "ANSREL-- FUNNY TYPE"))))))

(§ defn- ANSTHM [exp]
    ;; GENRATES A THEOREM OR ASSERTION FOR AN EXPRESSION.
    (let [NEG nil VARLIST nil BODY nil]
        (COND
            ;; NOTELL MARKS THAT THIS ISN'T THE KIND OF ASSERTION IT CAN HANDLE.
            ;; IT USES GLOBAL-ERR VAR AND NEG ARE SET AS FREE VARIABLES BY ANSTHMELEMENT WHICH ANALYZES EACH ELEMENT.
            ;; IF THERE ARE NO VARS, IT IS A SIMPLE ASSERTION.
            ((term? exp) (notell))
            ((not (get (car exp) 'TELLABLE)) (notell))
            (:else
                (SETQ NEG (negative? RSS))
                (SETQ exp (doall (map #'ANSTHMELEMENT (PLNR-REMTIME exp))))
                (RETURN (COND ((not (or VARLIST NEG)) exp)
                    (:else (PLNR-THCONSIFY VARLIST exp (COND (NEG (PLNR-PROGIFY nil (list BODY '(THFAIL THEOREM)))) (:else BODY))))))))
        nil))

(§ defn- ANSTHMADD [oss]
    (SETQ VARLIST (cons (variable? oss) VARLIST))
    (SETQ BODY (COND
        (BODY (PLNR-PROGIFY nil (list BODY (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))))
        (:else (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))))
    (plnr-var (variable? oss)))

(§ defn- ANSTHMELEMENT [X]
    (COND ((not (term? X)) X)
        ((tss? X) (notell))
        ((rss? X) (notell))
        ((not (oss? X)) X)
        ((refer? X) (atomify (refer? X)))
        ((= (quantifier? X) 'ALL) (COND (NEG (notell)) (:else (ANSTHMADD X))))
        ((= (quantifier? X) 'NO) (SETQ NEG true) (ANSTHMADD X))
        ((= (quantifier? X) 'NDET) (ANSTHMADD X))
        ((not (= (quantifier? X) 'INDEF)) (notell))
        ((ISQ (parsenode? X) ANY) (ANSTHMADD X))
        (:else (GLOBAL-ERR "YOU HAVE TO TELL ME WHICH"))))

(defn- ansunique [l]
    ;; THIS FUNCTION SHOULD ELIMINATE ANSWERS WHICH GIVE THE SAME
    ;; RESULT EVEN THOUGH THEY INVOLVE DIFFERENT INTERPRETATIONS.
    ;; IT NEEDS TO CHECK FOR SIGNIFICANT DIFFERENCES, E.G. IN WHAT
    ;; GETS PRINTED OR DONE, WHILE IGNORING INSIGNIFICANT ONES,
    ;; E.G. THE NAMES OF ATOMS TO WHICH THINGS ARE ATTACHED.
    ;; FOR THE MOMENT, IT JUST RETURNS THE LIST UNTOUCHED.
    l)

(defn- atomify [x] (cond (term? x) x (cdr x) x :else (car x)))

(§ defn- CUTOFF [x]
    ;; FOR CUTTING # OFF OF CONCEPT NAMES TO GET ENGLISH WORDS.
    (READLIST (cdr (EXPLODE x))))

(§ defn- DESCRIBEVENT [event type]
    (SETQ event (car event))
    (COND
        ((= type 'WHERE)
            (GLOBAL-ERR "I CAN'T ANSWER \"WHERE\" QUESTIONS YET"))
        ((= type 'WHY)
            (COND ((= (get event 'WHY) 'COMMAND) '((SAY BECAUSE YOU TOLD ME TO)))
                (:else (cons '(SAY TO) (NAMEACTION 'INFINITIVE (get event 'WHY))))))
        ((= type 'HOW)
            (let [ans nil]
                (dorun (map #'(lambda [x] (and (= (get x 'WHY) event) (SETQ ans (cons x ans)))) EVENTLIST))
                (COND
                    ((nil? ans) '((SAY I CAN'T ANALYZE HOW I DID IT)))
                    (:else (concat '((SAY BY)) (NAMEACTION 'ING (car ans)) (doall (map #'(lambda [x] (cons '(PRINC '\;) (CONS '(SAY THEN) (NAMEACTION 'ING x)))) (CDR ans))))))))
        ((or (= type 'POLAR) (= type 'WHEN))
            (COND
                ((= (get event 'WHY) 'COMMAND)
                    (COND
                        ((= event (toplevel (car EVENTLIST))) '((SAY JUST NOW)))
                        (:else (cons '(SAY BEFORE) (NAMEACTION 'PAST (toplevel (car (FINDB event EVENTLIST))))))))
                (:else (cons '(SAY WHILE) (NAMEACTION 'PRES-PAST (toplevel event))))))
        ((bug "DESCRIBEVENT -- FUNNY type"))))

(§ defn- DISPUT [assertion]
    ;; PUT THE SENTENCE NUMBER ON THE ASSERTION AS A WHO PROPERTY
    (or (not DISCOURSE) (PUTPROP assertion SENTNO 'WHO)))

(§ defn- ELIZA [node]
    ;; DOES THE OBVIOUS THING.
    (let [xx nil num (count (N node))]
        (APPLY #'concat
            (MAPLIST #'(lambda [WORD]
                (COND
                    ((not (< num (count WORD))) nil)                ;; THIS KLUDGE STOPS IT AT THE END OF THE NODE
                    ((SETQ xx (assq (car WORD) '((I YOU) (ME YOU) (AM ARE) (ARE AM))))
                        (cdr xx))                                   ;; WE RETURN LIST OF THE THING REALLY WANTED, SO
                    ((= (car WORD) 'YOU)                           ;; THE APPLY APPEND CAN GET RID OF THE EMPTY ONES.
                        (SETQ xx (FINDMOTHER WORD node))            ;; UNFORTUNATELY, FOR "YOU" IT IS NECESSARY TO
                        (COND ((ISQ xx SUBJ) '(I))                  ;; DECIDE WHETHER IT SHOULD BE REPLACED BY "I" OR
                            ((ISQ xx OBJ) '(YOU))                   ;; "ME", ACCORDING TO WHETHER IT WAS PARSED AS AN
                            ((bug "ELIZA -- SUBJ OBJ"))))             ;; OBJECT OR SUBJECT. FINDMOTHER IS USED TO FIND
                    ((list (car WORD)))))                           ;; THE PARSE NODE. WORDS OTHER THAN THE SPECIAL
                (NB node)))))                                       ;; ONES GO THROUGH DIRECTLY.

(defn- enough-better [ans1 ans2]
    (> (plausibility? ans1) (+ (plausibility? ans2) TIMID)))

(§ defn- FINDMOTHER [word node]
    ;; FINDMOTHER TAKES A PLACE IN THE SENTENCE AND A GRAMMAR NODE
    ;; (BOTH ARE ACTUALLY LISTS) AND FINDS THE SINGLE-WORD
    ;; CONSTITUTENT BEGINNING AT THAT PLACE IN THE SENTENCE.
    (COND ((and (= word (NB node)) (= (cdr word) (N node))) node)
        (:else (APPLY #'concat (MAPLIST #'(lambda [NOD] (FINDMOTHER word NOD)) (H node))))))

(§ defn- HEADPART [node]
    ;; EVERYTHING UP TO THE NOUN, FOR EXAMPLE "THE RED BLOCK" IN "THE RED BLOCK WHICH ..."
    ;; NOTE THAT NODE IS ACTUALLY A LIST OF NODE (A PROPER GRAMMAR POINTER).
    (and (SETQ PT node) (MOVE-PT DLC PV (NOUN)) (FROM (NB node) (N PT))))

(§ defn- LISTNAMES [PHRASE spec names]
    ;; PHRASE IS THE INITIAL THING TO COMPARE FOR USING "ONE", SPEC IS EITHER DEF OR INDEF, AND THE NAMES ARE OF DATA-BASE OBJECTS.
    ;; LISTNAMES PUTS OUT AN ACTION LIST, AS WELL AS PUTTING THINGS ONTO THE BACKREF.
    ;; IT IS CALLED AFTER THE ANSWER HAS BEEN DECIDED ON.
    (let [COUNT nil EXAM nil x nil RES nil ANS nil COMMA? nil]
        ;; NAMEOBJ RETURNS A LIST OF THE OBJECT AND THE ...
        ;; THIS PATCH MAY WELL BE TOTAL OUT OF PHASE WITH THE BACKREF HACKER - DDM 5-12-73 INSTRUCTIONS FOR NAMING IT.
        (SETQ names (doall (map #'(lambda [x] (NAMEOBJ x spec)) names)))
        (COND ((nil? names) (RETURN '(SAY NOTHING))))
    UP  (SETQ COUNT 1)
        (SETQ EXAM (car names))
        (SETQ names (cdr names))
    BACK (COND ((SETQ x (assq (car EXAM) names))
            (SETQ names (DELQ x names))
            (SETQ COUNT (inc COUNT))
            (SETQ EXAM (list (car EXAM) (concat (cadr x) (cadr EXAM))))
            (GO BACK)))
        ;; WHEN THERE ARE TWO OBJECTS WITH THE SAME ENGLISH DESCRIPTIONS, A JOINT OBJECT IS PRODUCED COMBINING THE OBJECTS.
        ;; THE COUNT IS LATER USED TO PUT IN THE APPROPRIATE NUMBER, AND THE DESCRIPTION IS CHECKED TO SEE IF "ONE" CAN BE USED.
        ;; ADD THE ONE JUST PRODUCED TO THE RESULT LIST.  TRY ANOTHER.
        (SETQ RES (cons (cons (PLURALIZE (car EXAM) COUNT) (cdr EXAM)) RES))
        (and names (GO UP))
        (SETQ RES
            (doall (map #'(lambda [PHRASE2]
                    (COND ((PROPNAME (caadr PHRASE2)) (car PHRASE2))
                        ;; ANSNAME PARSES THE PHRASE AND PUTS THE ...
                        ;; ANSONE SUBSTITUTES "ONE" IF POSSIBLE
                        (:else (ANSNAME PHRASE2) (ONECHECK (car PHRASE2)))))
                RES)))
        (SETQ ANS (car RES))
    OUTPUT (COND
            ((nil? (SETQ RES (cdr RES))) (RETURN ANS))
            ((cdr RES) (SETQ COMMA? true) (SETQ ANS (concat ANS '((PRINC '\,)) (car RES))))
            ((SETQ ANS (concat ANS (and COMMA? '((PRINC '\,))) '((SAY AND)) (car RES)))))
        (GO OUTPUT)))

(§ defn- NAMEACTION [TENSE event]
    ;; THIS FUNCTION SETS UP A LIST OF S-EXPRESSIONS WHICH ARE RETURNED TO DESCRIBEVENT AND WHICH
    ;; WHEN EVALUATED WILL PRINT OUT AN ENGLISH DESCRIPTION OF THE SINGLE, SIMPLE EVENT IMBEDDED
    ;; IN THE LIST "THASSERTION" WITH THE TENSE SPECIFIED.
    ;; THE THASSERTION PROPERTY IS A LIST THAT TYPICALLY LOOKS LIKE "(NIL (2 (3 1 ((!GRASP ßE2 ßB6)))))"
    (let [a (car (caddr (cadadr (get event 'THASSERTION)))) verb (CUTOFF (car a)) obj1 (caddr a) obj2 (cadddr a)]
        (cond
            (= verb 'CLEARTOP)
                ;; SAYIFY WRAPS THE FUNCTION "SAY" ARROUND A LIST OF WORDS AND RETURNS THE RESULTING S-EXPRESSION.
                (cons (SAYIFY (VBFIX 'CLEAN false)) (pron-prt 'OFF obj1))
            (= verb 'GET-RID-OF)
                ;; NAMELIST-EVALED '(NIL) 'DEF RETURNS A LIST (!!!) OF S-EXPRESSIONS.
                (cons (SAYIFY (VBFIX 'GET true) 'RID 'OF) (namelist-evaled '(nil) 'DEF obj1))
            (= verb 'GRASP)
                (cons (SAYIFY (VBFIX 'GRASP true)) (namelist-evaled '(nil) 'DEF obj1))
            (= verb 'PICKUP)
                (cons (SAYIFY (VBFIX 'PUT true)) (pron-prt 'UP obj1))
            (= verb 'PUTON)
                (concat (cons (SAYIFY (VBFIX 'PUT true)) (namelist-evaled '(nil) 'DEF obj1)) (cons '(SAY ON) (namelist-evaled '(nil) 'DEF obj2)))
            (= verb 'STACKUP)
                (cons (VBFIX STACK true) (pron-prt 'UP obj1))
            (= verb 'RAISEHAND)
                nil
            :else (bug "NAMEACTION: I DON'T KNOW WHAT TO DO WITH THE VERB I GOT"))))

(defn- namelist [one spec listx]
    ;; GENERATES A LIST OF EXPRESSIONS TO BE EVALUATED WHICH WILL CAUSE THE APPROPRIATE NAMELIST TO BE PRINTED OUT.
    ;; THE ARGUMENTS ARE JUST THOSE TO LISTNAMES.
    (list (list 'evlis (list 'LISTNAMES (quotify one) (quotify spec) (quotify listx)))))
    ;; A TYPICAL CALL WOULD RESULT IN A VALUE OF ((EVLIS (LISTNAMES '(A RED BLOCK) 'INDEF '(ßB1 ßB7)))) WHICH WOULD BE EVALUATED LATER.
    ;; NOTE THAT LISTNAMES WILL IN TURN PRODUCE A LIST OF EXPRESSIONS TO BE EVALUATED, WHICH WILL BE CAUGHT BY THE EVLIS.  CONFUSING?

(defn- namelist-evaled [one spec listx]
    (list (eval (list 'LISTNAMES (quotify one) (quotify spec) (quotify listx)))))

(§ defn- NAMENUM [x]
    ;; GENERATES NUMBER NAMES.
    (or (NTH (inc x) '(NONE ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN))
        (GLOBAL-ERR "I CAN'T COUNT THAT HIGH")))

(§ defn- NAMEOBJ [ITEM spec]
    ;; NAMES THE OBJECT IN ENGLISH -- GENERATES LIST OF THINGS TO BE EVALUATED.  SPEC IS EITHER 'INDEF OR 'DEF
    (let [TYPEß nil TYPELIST nil TYPE nil NAMEß nil COLORß nil COLORLIST nil SIZEß nil SIZELIST nil CUBE nil NAME nil x nil]
        (and (SETQ x (assq ITEM '((ßSHRDLU I) (ßFRIEND YOU))))
            (RETURN (list (ansay (cdr x)) (list ITEM))))                        ;;  SPECIAL CASE CHECK
        (THVAL2 nil '(THGOAL (!NAMEOBJ) (THUSE TC-NAMEOBJ)))
        (or TYPELIST
            (ert "NAMEOBJ -- OBJECT WITH NO !IS ASSERTION"))
        ;; DISPUT CHECKS TO SEE IF DISCOURSE IS BEING KEPT, AND IF SO PUTS THE RELEVANT SENTENCE NUMBER AS A PROPERTY ON THE ASSERTION.
        (DISPUT TYPEß)
        (COND ((= (SETQ TYPE (caddar TYPEß)) '!NAME)                           ;; A NAME IS ITS OWN NAME.
                (RETURN (list (ansay (list ITEM)) (list ITEM))))
            ((memq '!PROPERTY (get TYPE 'SYS))
                ;; CUTOFF CUTS THE # OFF OF NAMES LIKE !RED AND !POINTED WHICH ARE USED FOR PROPERTIES.
                (RETURN (list (ansay (list (CUTOFF ITEM))) (list ITEM))))
            ((not (cdr TYPELIST))
                (RETURN (list (ansay (list 'THE (CUTOFF TYPE))) (list ITEM))))  ;; THERE IS ONLY ONE OBJECT OF THIS TYPE (E.G. TABLE, BOX, HAND)
            (CUBE (SETQ NAME '(CUBE)))
            ((SETQ NAME (list (CUTOFF TYPE)))))                                 ;; E.G. !BLOCK BECOMES BLOCK.
        (and NAMEß
            (RETURN (list (ansay (list 'THE (car NAME) 'NAMED (caddar NAMEß))) (list ITEM)))) ;; E.G. THE BLOCK NAMED SUPERBLOCK.
        (DISPUT COLORß)                                                         ;; IF WE HAVEN'T RETURNED YET, COLOR WILL BE NEEDED TO FULLY DESCRIBE THE OBJECT.
        (SETQ NAME (cons (CUTOFF (caddar COLORß)) NAME))
        (or (cdr COLORLIST)
            (RETURN (list (ansay (cons 'THE NAME)) (list ITEM))))               ;; THERE ARE NO OTHERS OF THE SAME COLOR.  IF THERE ARE, WE MUST USE SIZE AS WELL.
        (SETQ NAME (cons SIZEß NAME))
        (RETURN (list (COND
            ((nil? (cdr SIZELIST))
                (ansay (cons 'THE NAME)))                                       ;; THE SIZE MANAGES TO FINISH SPECIFYING IT.
            ((= spec 'INDEF)
                (ansay (cons 'A NAME)))                                         ;; IN THE INDEFINITE CASE WE DON'T CARE IF THIS ISN'T A FULL SPECIFICATION.
            ((SETQ x (THVAL2 nil '(THFIND ALL ($? x) (x (y ITEM)) (($G) (!SUPPORT ($? y) ($? x))))))
                (cons (concat '(SAY THE) NAME)
                    (cons '(SAY WHICH SUPPORTS)
                        (LISTNAMES nil 'INDEF x))))                             ;; IF IT SUPPORTS ANYTHING, NAME THEM.
            ((cons (concat '(SAY THE) NAME)
                (cons '(SAY WHICH IS TO THE RIGHT OF)
                    (COND ((SETQ x (THVAL2 nil '(THFIND ALL ($? x) (x (y ITEM))
                                (($G) (!AT ($? x) ?)) (($G) (!LOC !RIGHT ($? y) ($? x)) (THUSE TC-LOC))))) ;; MAKE SURE IT IS AN ITEM WITH A LOCATION.
                            (LISTNAMES nil 'INDEF x))
                        ('((SAY NOTHING))))))))
            (list ITEM)))))

(§ DEFPROP TC-NAMEOBJ
    ;; PLANNER IS CALLED TO SEE HOW MANY OBJECTS FIT VARIOUS FORMS OF THE DESCRIPTION.  IT USES FAILURE TO LOOP THROUGH THEM,
    ;; SUCCESSIVELY FILTERING THEM THROUGH GOALS IN WHICH THEY ARE FORCED TO MATCH THE CHOSEN ITEM.  THIS VALUE IS THE ENTIRE
    ;; TYPE ASSERTION FOR SPECIAL CHECK TO CALL EQUIDIMENSIONAL BLOCKS "CUBE".  THE OR IS TO PREVENT PLANNER FROM FAILING THE
    ;; CHOSEN OBJECT.  IT IS SAVED SO THE SENTENCE NUMBER CAN BE PUT ON ITS PROPERTY LIST IF THE FACT IS USED IN THE DESCRIPTION.
    ;; IF THE ITEM HAS A NAME, NO MORE IS NEEDED.  FIND SOMETHING ELSE OF THE SAME TYPE.  NOTE THAT THIS WILL FIND THE ITEM ITSELF
    ;; ALONG WITH THE OTHERS AND THUS PUT IT ON THE LIST.  THIS KEEPS A LIST OF ALL THE OBJECTS WHICH MAKE IT THIS FAR.  NOTE
    ;; THAT SINCE IT IS SETQ INSTEAD OF THSETQ, BACKUP DOESN'T UNDO IT.  ANYTHING WHICH MAKES IT THIS FAR IS BOTH THE SAME TYPE
    ;; AND THE SAME COLOR.  WE DON'T WANT TO CHECK FOR EXACT EQUALITY OF SIZE, JUST WHETHER THEY WOULD BE CALLED THE SAME THING.
    ;; THE THFAIL SENDS IT BACK UP SEARCHING FOR MORE.
    (THCONSE ((x ITEM) type color name #_size y z)
        (!NAMEOBJ)
        (($G) (!IS ($? x) ($? type)))
        (SETQ TYPEß THVALUE)
        (or (SETQ CUBE (and (= ($? type) '!BLOCK) (!EQDIM ($? x))))
            true)
        (THCOND
            ((($G) (!NAME ($? x) ($? name)))
                (SETQ name THVALUE))
            ((($G) (!IS ($? y) ($? type)))
                (or (not CUBE) (!EQDIM ($? y)))
                (SETQ TYPELIST (cons ($? y) TYPELIST))
                (($G) (!COLOR ($? x) ($? color)))
                (SETQ COLORß THVALUE)
                (($G) (!COLOR ($? y) ($? color)))
                (SETQ COLORLIST (cons ($? y) COLORLIST))
                (SETQ SIZEß (NAMESIZE (size ($? x))))
                (= SIZEß (NAMESIZE (size ($? y))))
                (SETQ SIZELIST (cons ($? y) SIZELIST))
                (THFAIL))))
    THEOREM)

(§ defn- NAMESIZE [x]
    ;; ACCEPTS EITHER SINGLE NUMBER OR LIST OF DIMENSIONS.
    (or (number? x) (SETQ x (reduce #'+ x)))
    (COND ((> x 383) 'LARGE) (:else 'SMALL)))

(§ defn- NAMESUGAR [num oss]
    ;; GENERATES PHRASES LIKE "THREE OF THEM".
    (let [VAGUE nil]
        ;; VAGUE IS FOR WORDS LIKE "ANYTHING", "SOMETHING", "NOTHING" TO AVOID SAYING "OF THEM" WHEN IT ISN'T APPROPRIATE.
        (SETQ VAGUE (memq '!VAGUE (markers? oss)))
        (RETURN (list (cons 'SAY
            (COND ((and VAGUE (zero? num)) '(NOTHING))
                ((cons (NAMENUM num)
                    (COND (VAGUE (COND ((== num 1) '(THING)) ('(THINGS))))
                        ('(OF THEM)))))))))))

(defn- notell [] (GLOBAL-ERR "THAT ISN'T THE KIND OF THING I CAN BE TOLD"))

(§ defn- ONECHECK [ITEM]
    ;; CHECKS TO SEE IF A SUBSTITUTE "ONE" CAN BE USED.
    ;; ITEM IS A SINGLE "SAY" PHRASE.
    ;; "PHRASE" IS A FREE VARIABLE IN LISTNAMES.
    (let [ANS nil OLD nil NEW nil]
        (and (= PHRASE '(nil))
            (SETQ PHRASE (car ITEM))
            (RETURN ITEM))
        (SETQ OLD (reverse PHRASE))
        (SETQ NEW (reverse (car ITEM)))
        (or (= (car OLD) (car NEW))
            (= (car OLD) (get (car NEW) 'ROOT))
            (= (car NEW) (get (car OLD) 'ROOT))
            ;; IF THE NOUNS DON'T MATCH, JUST RETURN WHAT YOU GOT.
            ;; MATCHING INCLUDES PLURALS TO THEIR CORRESPONDING SINGULAR FORMS.
            (RETURN ITEM))
    LOOP (SETQ NEW (cdr NEW))
        (SETQ OLD (cdr OLD))
        (COND ((or (nil? NEW) (nil? OLD) (ISQ NEW NUM) (ISQ NEW DET) (not (= (car NEW) (car OLD))))
            (RETURN (cons
                (reverse (cons (COND ((ISQ (LAST (car ITEM)) NPL) 'ONES) (:else 'ONE)) NEW))
                (cdr ITEM)))))
        (GO LOOP)))

(§ defn- ORDNAME [num]
    ;; NAME AN ORDINAL
    (COND ((== num 1) 'ONCE) ((== num 2) 'TWICE)
        ((READLIST (concat (EXPLODE (NAMENUM num)) '(\space T I M E S))))))

(§ DEFLIST PAST (PUT PUT))

(§ defn- PLNR-ANDORIFY [rss]
    ;; TURNS AN RSS INTO A COLLECTION OF PLANNER CODE FOR A COMMAND
    (COND ((and? rss)
            (PLNR-PROGIFY nil (doall (map #'PLNR-ANDORIFY (and? rss)))))
        ((or? rss)
            ;; (PLNR-ORIFY NIL (MAPCAR 'PLNR-ANDORIFY (OR? RSS)))
            (ert "SORRY, PLNR-ORIFY NOT WRITTEN"))
        ((PLNR-PROGIFY nil (doall (map #'PLNR-GOALIFY (relations? rss)))))))

(§ defn- PREPPUT [x]
    (COND ((and (rel? RSS) (SETQ PT (parsenode? (rel? RSS))) (ISQ (MOVE-PT U) PREPG))
            (cons (cons 'SAY (FROM (NB PT) (NB (MOVE-PT DLC)))) x))
        (:else x)))

(§ defn- PLURALIZE [item num]
    ;; CONVERTS A SINGULAR NOUNPHRASE OR "ONCE" STATEMENT INTO PLURAL.
    (COND ((> 2 num) item)
        (:else (COND ((memq 'A (car item))
                (cons (PLURALMAKE (SUBST (NAMENUM num) 'A (car item))) (cdr item)))
            ((memq 'ONCE (car item))
                (cons (SUBST (ORDNAME num) 'ONCE (car item)) (cdr item)))
            ((bug "PLURALIZE -- FUNNY item"))))))

(§ defn- PLURALMAKE [phrase]
    ;; CONVERTS SINGULAR PHRASE TO PLURAL.
    (let [SING nil PLURAL nil]
        (or (ISQ (SETQ SING (LAST phrase)) NOUN)
            (bug "PLURALMAKE -- NO NOUN"))
        (SETQ PLURAL (MAKNAM (concat (EXPLODE (car SING)) '(S))))
        (or (get PLURAL 'FEATURES)
            (BUILDWORD PLURAL '(NOUN NPL) (SM SING) (car SING)))
        (RETURN (SUBST PLURAL (car SING) phrase))))

(defn- pron-prt [particle ng]
    ;; THIS IS EVENTUALLY SUPPOSED TO BE THE PLACE FOR THE PRONOUN-PARTICLE-INTERACTION MAGIC
    ;; TO HAPPEN, IE. "CLEAR OFF THE BLOCK." VS. "CLEAR IT OFF" SINCE "CLEAR OFF IT." IS
    ;; UNGRAMMATICAL AND "CLEAR THE BLOCK OFF." WOULD NOT BE APPROPRIATE IN CASES OF HEAVY-NP'S.
    ;;
    ;; AT THE MOMENT, FOR SIMPLICITY'S SAKE, I'VE IGNORED THE
    ;; PROBLEM AND THE PARTICLE IS ALWAYS PUT BEFORE THE NG.
    (cons (list 'SAY particle) (namelist-evaled '(nil) 'DEF ng)))

(§ defq- SAYIFY [exp-list]
    (cons 'SAY (doall (map #'(lambda [y] (eval y)) exp-list))))

(§ defn- THVAL-MULT [code]
    ;; DOES A THVAL WITH DIFFERENT VALUES OF WHO (I.E. NIL (EVERYTHING I KNOW),
    ;; 'HE (EVERYTHING HE KNOWS), AND THE PREVIOUS SENTENCE) USED TO TELL IF AN
    ;; ANSWER COULD HAVE BEEN GENERATED WITH HIS KNOWLEDGE TO SEE WHETHER HE REALLY
    ;; MEANT THIS INTERPRETATION.  RETURNS A LIST OF A PLAUSIBILITY AND THE RESULT
    ;; OF THE THVAL USING ALL THE KNOWLEDGE IN THE DATA BASE.
    (let [ANS (THVAL2 nil code)]
        ;; THIS FEATURE IS ONLY RELEVANT IN DISCOURSE AND WHEN THERE ARE AMBIGUITIES.
        (or (and AMBIG DISCOURSE) (RETURN (list 0 ANS)))
        ;; GIVE A VALUE OF 256 IF HE COULDN'T HAVE ANSWERED IT AT ALL.
        (or (= ANS (THVAL2 'HE code)) (RETURN (list 256 ANS)))
        ;; PLAUSIBILITY IS 0 IF HE COULD HAVE ANSWERED IT WITH RECENTLY MENTIONED INFORMATION.
        ;; 128 IF HE COULD ANSWER IT BUT NOT WITH RECENT INFO.
        (RETURN (COND ((= ANS (THVAL2 (list (- SENTNO 2) (inc SENTNO)) code)) (list 0 ANS)) ((list 128 ANS))))))

(defn- toplevel [event]
    ;; FINDS THE TOP LEVEL EVENT GOING ON AT THE TIME
    (let [why (get event 'WHY)] (if (= why 'COMMAND) event (recur why))))

(§ defn- FINDCHOOSE [oss x ANS2]
    (let [HAVE nil NEED nil XX nil ANS nil PLNRCODE nil LOOP nil]
        (and (refer? oss) (RETURN (atomify (refer? oss))))
        (COND
            ((and? oss)
                (RETURN (MAPBLAND #'(lambda [oss]
                    (let [y (FINDCHOOSE oss x ANS2)]
                        (SETQ ANS2 (concat y ANS2))
                        (RETURN y)))
                    (and? oss))))
            ((or? oss)
                (SETQ LOOP (or? oss))
                (RETURN (let [y nil]
                GO  (COND
                    ((SETQ y (FINDCHOOSE (car LOOP) x ANS2)) (RETURN y))
                    ((SETQ LOOP (cdr LOOP)) (GO GO)))))))
        (SETQ PLNRCODE (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))
        (PUTPROP oss PLNRCODE 'PLNRCODE=)
        (COND
            ((= (quantifier? oss) 'ALL)
                (RETURN (atomify (THVAL (PLNR-FINDIFY 'ALL (variable? oss) (list (variable? oss)) PLNRCODE) nil))))
            ((or (and? oss) (or? oss)) (GO CONJ)))
        (or (term? (SETQ NEED (num? oss)))
            (SETQ NEED (cadr NEED)))
        (and (= NEED 'NS) (SETQ NEED 1))
        (SETQ HAVE 0)
    GO  (COND
            ((or (= HAVE NEED)
                (and (> HAVE NEED)
                    (SETQ ANS (FINDREDUCE ANS (- HAVE NEED)))))
                (GO DONE))
            ((= x 'NOMORE) (RETURN nil))
            ((SETQ HAVE (count
                (SETQ ANS (concat
                    (THVAL (PLNR-FINDIFY
                        (list 1 (- NEED HAVE) true)
                        (variable? oss)
                        (list (variable? oss))
                        (PLNR-PROGIFY nil
                            (concat (list PLNRCODE)
                                (SUBST (variable? oss) '*** '((not (or (memq (THV ***) ANS) (memq (THV ***) ANS2)))))
                                (and x (SUBST (variable? oss) '* (car x))))))
                        THALIST)
                    ANS))))
            (SETQ x (COND (x (cdr x)) ('NOMORE)))
            (GO GO)))
    CONJ (SETQ LOOP (or (and? RSS) (or? RSS)))
    UP  (COND ((get (car LOOP) 'REFER)
                (SETQ ANS (concat (get (car LOOP) 'REFER) ANS)))
            ((SETQ XX (FINDCHOOSE (car LOOP) x (concat ANS2 ANS)))
                (SETQ ANS (concat XX ANS))))
        (COND ((and ANS (or? oss)))
            ((SETQ LOOP (cdr LOOP)) (GO UP))
            (ANS)
            ((RETURN nil)))
    DONE (and (term? (variable? oss))
            (PUTPROP (variable? oss) (reverse ANS) 'BIND))
        (RETURN (atomify (reverse ANS)))))

(§ defn- FINDREDUCE [x y]
=>  (SETQ x (cdr x))
    (COND ((zero? (SETQ y (dec y))) (RETURN x)) ((GO =>))))

(§ defn- MUNG [l mung]
    (SETQ mung (list 'quote mung))
    (and DISCOURSE (SETQ l (caddr l)))
    (COND ((= (caar (cdddr l)) 'THAMONG)
            (RPLACD (cdar (cddddr l)) mung))
        ((RPLACD (cdddr l) (cons (list 'THAMONG (list 'THV (cadr (caddr l))) mung) (cddddr l))))))

(§ defn- NAMEVENT [event type]
    (let [THALIST nil EV nil SUBJ nil OBJ1 nil OBJ2 nil]
        (or (SETQ EV (get (get event 'TYPE) 'NAMEVENT))
            (ert "NAMEVENT"))
        (or (THVAL (list 'THGOAL
            (COND
                ((== (car EV) 2) '(? ($? event)))
                ((== (car EV) 3) '(? ($? event) (THNV SUBJ)))
                ((= (car EV) 'I3) '(? ($? event) (THNV OBJ1)))
                ((== (car EV) 4) '(? ($? event) (THNV SUBJ) (THNV OBJ1)))
                ((= (car EV) 'I4) '(? ($? event) (THNV OBJ1) (THNV OBJ2)))
                ((== (car EV) 5) '(? ($? event) (THNV SUBJ) (THNV OBJ1) (THNV OBJ2)))
                ((ert "NAMEVENT DATA"))))
            (SETQ THALIST (list
                (list 'EVENT event)
                (list 'SUBJ (COND ((number? (car EV)) nil) ('I)))
                (list 'OBJ1 nil) (list 'OBJ2 nil))))
            (ert "NAMEVENT THVAL"))
        (dorun (map #'(lambda [x]
                (and (cadr x) (SET (car x) (ert "OOPS! (NAMES (listify (cadr x)) 'EV)"))))
            (cdr THALIST)))
        (SETQ ANSBACK2 (or ANSBACK true))
        (SETQ LASTANSEV event)
        (RETURN (concat
            (COND ((= type 'PAST) SUBJ) ((= type 'TO) '(TO)))
            (eval (cadr EV))))))

(defn- prtput [x y] (if (cdr y) (cons x y) (conj y x)))

(§ defn- VBFIX [x pp]
    (COND
        ((= TENSE 'ING)
            (SETQ x (reverse (EXPLODE x)))
            (READLIST (reverse (concat '(G N I) (vbfix2 x pp) x))))
        ((= TENSE 'PAST)
            (or (get x 'PAST)
                (and (SETQ x (reverse (EXPLODE x)))
                    (READLIST (reverse (concat '(D E) (vbfix2 x pp) x))))))
        ((= TENSE 'INFINITIVE) x)
        (:else (bug "VBFIX: WHAT SHOULD I DO WITH THIS TENSE?"))))

(defn- vbfix2 [x pp] (when (and pp (memq (car x) CONSO) (memq (cadr x) VOWEL)) (list (car x))))

#_(ns shrdlu.cgram)

;; #################################################################
;;
;;  CGRAM > THE REGULAR GRAMMAR AFTER GOING THROUGH THE PRECOMPILER
;;
;; #################################################################

(§ defn- CLAUSE []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil POSITION-OF-PRTMVB nil LOCATIONMARKER nil SUBJ-VB-BACKUP-TYPE1 nil POSITION-OF-PTW nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-CLAUSE
        (and LABELTRACE (PASSING 'ENTERING-CLAUSE))
        (SETR 'TIME (BUILD TSSNODE= (MAKESYM 'TSS)) C)
        (SETQ ßRESULT (CQ SIMP))
        (COND (ßRESULT (GO SUBJ)))
        (SETQ ßRESULT (CQ MAJOR))
        (COND (ßRESULT (GO INIT)) (:else (GO SEC)))
INIT
        (and LABELTRACE (PASSING 'INIT))
        (SETQ LOCATIONMARKER N)
        (SETQ ßRESULT (and (NQ BINDER) (PARSE CLAUSE BOUND INIT)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO FIXIT)) (:else (GO MAJOR)))))
        (FQ BIND)
        (SETQ ßRESULT (CALLSM (SMBIND)))
        (COND (ßRESULT (GO INIT)))
FIXIT
        (and LABELTRACE (PASSING 'FIXIT))
        (SETQ PTW CUT)
        (SETQ ßRESULT (CUT (MOVE-PTW)))
        (COND (ßRESULT (GO INIT)) (:else (GO MAJOR)))
MAJOR
        (and LABELTRACE (PASSING 'MAJOR))
        (CUT END)
        (COND ((= PUNCT '?) (GO QUEST))
        ((or (CQ IMPER) (= PUNCT '!)) (GO IMPER)))
        (GO THEREINIT)
FDEC
        (and LABELTRACE (PASSING 'FDEC))
        (FQ DECLAR)
THEREINIT
        (and LABELTRACE (PASSING 'THEREINIT))
        (SETQ ßRESULT (and (NEXTWORD? 'THERE) (PARSE nil THERE) (FQ DECLAR)))
        (COND (ßRESULT (COND ((nil? NN) (M INIT) (GO FAIL)) (:else (GO THERE)))))
THER2
        (and LABELTRACE (PASSING 'THER2))
        (and (NQ PREP) (PARSE PREPG INIT) (or (CALLSM (SMRELATE H)) (POP)))
        (and (NQ ADV) (PARSE ADV TIMW) (or (CALLSM (SMADVERB)) (POP)))
        (and (NQ ADV) (PARSE ADJG ADV VBAD) (or (CALLSM (SMRELATE H)) (POP)))
        (PARSE NG TIME)
        (SETQ ßRESULT (= LOCATIONMARKER N))
        (COND (ßRESULT (COND ((nil? NN) (GO INPOP)) (:else (GO CLAUSETYPE)))) (:else (GO INIT)))
INPOP
        (and LABELTRACE (PASSING 'INPOP))
        (SETQ ßRESULT (MOVE-PT C DLC))
        (COND ((nil? ßRESULT) (M INPOP) (GO FAIL)))
BICUT
        (and LABELTRACE (PASSING 'BICUT))
        (CUT-BACK-ONE)
        (GO INIT)
CLAUSETYPE
        (and LABELTRACE (PASSING 'CLAUSETYPE))
        (SETQ ßRESULT (CQ DECLAR))
        (COND (ßRESULT (GO SUBJ)))
        (SETQ ßRESULT (and (NQ VB) (NQ INF) (PARSE VG IMPER) (FQ IMPER)))
        (COND (ßRESULT (GO VG1)))
        (FQ DECLAR)
        (SETQ ßRESULT (CQ IMPER))
        (COND (ßRESULT (M IMPER) (GO FAIL)))
SUBJ
        (and LABELTRACE (PASSING 'SUBJ))
        (CUT END)
SUBJ3
        (and LABELTRACE (PASSING 'SUBJ3))
        (SETQ ßRESULT (or (and (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ)) (and (PARSE CLAUSE RSNG ING SUBJ))))
        (COND (ßRESULT (COND ((nil? NN) (GO SUBJ1)) (:else (GO SUBREG)))))
SUBJ4
        (and LABELTRACE (PASSING 'SUBJ4))
        (SETQ ßRESULT (PARSE NG SUBJ))
        (COND (ßRESULT (COND ((nil? NN) (GO SUBJ1)) (:else (GO SUBREG)))))
        (COND ((CQ REL-NOT-FOUND)
                (RQ REL-NOT-FOUND)
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VB))
            (SUBJ-VB-BACKUP-TYPE1 (SETQ SUBJ-VB-BACKUP-TYPE1 nil) (GO SUBJ11))
            ((and H (ISQ H TIME) (ISQ H NG)) (SETR 'SUBJECT H C) (GO VB))
            ((MOVE-PT C U (REL-NOT-FOUND))
                (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
                (SETR 'RELHEAD (GETR 'RELHEAD PT) C)
                (REMOVE-F-PT 'REL-NOT-FOUND PT)
                (GO VB))
            ((and (CQ COMPONENT) NN) (FQ SUBJFORK) (GO VB))
            (H (POP) (GO SUBJ))
            ((GO FAIL)))
HEAD
        (and LABELTRACE (PASSING 'HEAD))
        (SETQ ßRESULT (or (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON))))
        (COND ((nil? ßRESULT) (M HEAD) (GO FAIL)))
SUB2
        (and LABELTRACE (PASSING 'SUB2))
        (SETQ ßRESULT (POP))
        (COND ((nil? ßRESULT) (GO FAIL)))
        (SETQ ßRESULT (CUT PTW))
        (COND (ßRESULT (GO INIT)) (:else (GO SUB2)))
SUBJ1
        (and LABELTRACE (PASSING 'SUBJ1))
        (COND ((ISQ H QUOTED) (and (ISQ H LIST) (FQ LIST)) (FQ QUOTED) (SETQ H (H H)) (GO RETSM)))
        (and (CQ REL-NOT-FOUND) (MOVE-PT H PV (QAUX))
            (COND ((ISQ PT BE) (FQ INT AUXBE) (RQ REL-NOT-FOUND) (SETR 'COMP (GETR 'RELHEAD C) C) (SETR 'SUBJECT H C) (SETMVB PT) (GO ONT))
                ((ISQ PT HAVE) (FQ SUBQ) (RQ REL-NOT-FOUND) (SETR 'SUBJECT (GETR 'RELHEAD C) C) (GO VBL))))
SUBJ11
        (and LABELTRACE (PASSING 'SUBJ11))
        (SETQ ßRESULT (CUT-BACK-ONE))
        (COND (ßRESULT (GO SUBJ3)) (:else (M SUBJ11) (GO FAIL)))
SUBREG
        (and LABELTRACE (PASSING 'SUBREG))
        (SETR 'SUBJECT H C)
        (GO VB)
VB
        (and LABELTRACE (PASSING 'VB))
        (SETQ ßRESULT (PARSE ADJG ADV VBAD))
        (COND (ßRESULT (COND ((nil? NN) (M VB-ADJG) (GO FAIL)) (:else (GO VB)))))
        (RQ VBLOK)
VBL
        (and LABELTRACE (PASSING 'VBL))
        (SETQ ßRESULT (PARSE VG))
        (COND (ßRESULT (GO VBREG)))
NOVERB
        (and LABELTRACE (PASSING 'NOVERB))
        (COND ((CQ SUBJFORK) (FQ VBFORK) (GO FINDOBJ1))
            ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
            ((not (ISQ H SUBJ)) (GO FAIL))
            ((ISQ H CLAUSE) (SETQ SUBJ-VB-BACKUP-TYPE1 true) (POP) (GO SUBJ4))
            ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))
VB2
        (and LABELTRACE (PASSING 'VB2))
        (CUT-BACK-ONE)
        (GO SUBJ3)
VBREG
        (and LABELTRACE (PASSING 'VBREG))
        (SETR 'VG H C)
VG1
        (and LABELTRACE (PASSING 'VG1))
        (CUT END)
        (SETQ ßRESULT (ISQ MVB BE))
        (COND (ßRESULT (COND ((nil? NN) (M BE) (GO FAIL)) (:else (GO BE)))))
        (SETQ ßRESULT (ISQ MVB VPRT))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO CHECKPASV)) (:else (GO CHECKPASV)))))
        (SETQ ßRESULT (and (NQ PRT) (PARSE PRT)))
        (COND ((nil? ßRESULT) (GO DPRT)))
        (FQ PRT)
        (SETQ ßRESULT (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD (NB H)))))
        (COND (ßRESULT (GO CHECKPASV)) (:else (GO POPRT)))
DPRT
        (and LABELTRACE (PASSING 'DPRT))
        (SETQ ßRESULT (ISQ H PASV))
        (COND (ßRESULT (GO CHECKPASV)))
        (SETQ ßRESULT (SETQ POSITION-OF-PRT (MOVE-PTW N NW (PRT))))
        (COND ((nil? ßRESULT) (GO FINDOBJ1)))
        (SETQ ßRESULT (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD POSITION-OF-PRT))))
        (COND ((nil? ßRESULT) (GO POPRT)))
        (SETQ ßRESULT (ISQ MVB TRANS))
        (COND ((nil? ßRESULT) (GO FINDOBJ1)))
        (CUT POSITION-OF-PRT)
        (SETQ ßRESULT (PARSE NG OBJ OBJ1))
        (COND (ßRESULT (GO POPRT)) (:else (GO FINDOBJ1)))
        (CUT END)
        (SETR 'OBJ1 H C)
        (PARSE PRT)
        (FQ PRT DPRT)
        (GO FINDOBJ2)
POPRT
        (and LABELTRACE (PASSING 'POPRT))
        (POPTO VG)
        (GO FINDOBJ1)
CHECKPASV
        (and LABELTRACE (PASSING 'CHECKPASV))
        (SETQ ßRESULT (and (ISQ H PASV) (FQ PASV) (SETR 'OBJ1 (GETR 'SUBJECT C) C)))
        (COND (ßRESULT (COND ((nil? NN) (GO FINDFAKE2)) (:else (GO FINDOBJ2)))))
        (FQ ACTV)
        (GO FINDOBJ1)
BE
        (and LABELTRACE (PASSING 'BE))
        (FQ BE)
        (and (PARSE nil NOT) (FQ NEG))
        (PARSE ADV VBAD)
FINDOBJ1
        (and LABELTRACE (PASSING 'FINDOBJ1))
        (SETQ ßRESULT (or (CANPARSE 1 '(ADJG COMP) 'INT)
                (CANPARSE 1 '(NG COMP) 'INT)))
        (COND (ßRESULT (COND ((nil? NN) (GO ONT)) (:else (GO CHECKIT)))))
        (SETQ ßRESULT (or (CANPARSE 1 '(PREPG COMP) 'INT)
                (CANPARSE 1 '(CLAUSE RSNG ING) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG REPORT) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG TO) 'TRANS)
                (CANPARSE 1 '(PREPG LOC) 'ITRNSL)
                (CANPARSE 1 '(ADV PLACE) 'ITRNSL)))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (CANPARSE 1 '(NG) 'TRANS))
        (COND (ßRESULT (COND ((nil? NN) (GO FINDFAKE2)) (:else (GO FINDOBJ2)))))
FINDFAKE1
        (and LABELTRACE (PASSING 'FINDFAKE1))
        (SETQ ßRESULT (MOVE-PT C U (REL-NOT-FOUND)))
        (COND (ßRESULT (GO OBJ1REL)))
        (SETQ ßRESULT (and (CANTAKE 1 '(PREPG LOC) 'ITRNSL) (MOVE-PT C U (QADJ)) (ISQ (GETR 'QADJ PT) PLACE) (FQ ITRANSL)))
        (COND (ßRESULT (GO PUTLOBJ)))
        (SETQ ßRESULT (CANPARSE 1 nil 'ITRNS))
        (COND (ßRESULT (GO ONT)))
GOOF1
        (and LABELTRACE (PASSING 'GOOF1))
        (or GLOBAL-MESSAGE (erterr "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)
OBJ1REL
        (and LABELTRACE (PASSING 'OBJ1REL))
        (SETR 'OBJ1 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ1REL)
FINDOBJ2
        (and LABELTRACE (PASSING 'FINDOBJ2))
        (SETQ ßRESULT (CANPARSE 2 '(CLAUSE RSNG TO) 'TRANS2))
        (COND (ßRESULT (GO FIXSUBJECT)))
        (SETQ ßRESULT (or (CANPARSE 2 '(ADV PLACE) 'TRANSL) (CANPARSE 2 '(PREPG LOC) 'TRANSL)))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (or (CANPARSE 2 '(ADJG COMP) 'TRANSINT) (CANPARSE 2 '(NG COMP) 'TRANSINT)))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (CANPARSE 2 '(NG) 'TRANS2))
        (COND (ßRESULT (GO ONT)))
FINDFAKE2
        (and LABELTRACE (PASSING 'FINDFAKE2))
        (SETQ ßRESULT (and (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND))))
        (COND (ßRESULT (GO OBJ2REL)))
        (SETQ ßRESULT (and (CANTAKE 2 '(PREPG LOC) 'TRANSL) (MOVE-PT C U (QADJ)) (ISQ (GETR 'QADJ PT) PLACE) (FQ TRANSL)))
        (COND (ßRESULT (GO PUTLOBJ)))
OBJ2TO
        (and LABELTRACE (PASSING 'OBJ2TO))
        (PARSE ADV VBAD)
        (SETQ ßRESULT
        (COND ((and (NEXTWORD? 'TO) (ISQ MVB TO2) (PARSE PREPG TO)) (SETR 'OBJ2 (GETR 'OBJ1 H) C) (FQ TRANS2TO TRANS2))
            ((and (CQ PREPQ) (MOVE-PT H PV (QUEST)) (= (WORD (MOVE-PTW FW)) 'TO) (RQ PREPQ) (FQ TRANS2TOQ TRANS2) (SETR 'OBJ2 (GETR 'OBJ1 PT) C)))))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (CANPARSE 2 nil 'TRANS))
        (COND (ßRESULT (GO ONT)) (:else (GO FAIL)))
PUTLOBJ
        (and LABELTRACE (PASSING 'PUTLOBJ))
        (SETR 'LOBJ PT C)
        (SETR 'RELHEAD (GETR 'QADJ PT) PT)
        (SETR 'QADJ nil PT)
        (REMOVE-F-PT 'QADJ PT)
        (GO ONT)
OBJ2REL
        (and LABELTRACE (PASSING 'OBJ2REL))
        (SETR 'OBJ2 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ2REL)
        (GO ONT)
FIXSUBJECT
        (and LABELTRACE (PASSING 'FIXSUBJECT))
        (SETR 'SUBJECT (GETR 'OBJ1 C) H)
        (GO ONT)
CHECKIT
        (and LABELTRACE (PASSING 'CHECKIT))
        (SETQ ßRESULT (= (WORD (NB (GETR 'SUBJECT C))) 'IT))
        (COND ((nil? ßRESULT) (GO ONT)))
        (SETQ ßRESULT (or (and (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ)) (and (NQ ING) (PARSE CLAUSE RSNG ING SUBJ)) (PARSE CLAUSE REPORT)))
        (COND ((nil? ßRESULT) (GO ONT)))
        (FQ IT)
        (SETR 'LOGICAL-SUBJECT H C)
        (GO ONT)
GOOF2
        (and LABELTRACE (PASSING 'GOOF2))
        (or GLOBAL-MESSAGE (erterr "NEW TRANSITIVITY - SECOND OBJECT"))
        (GO FAIL)
ONT
        (and LABELTRACE (PASSING 'ONT))
        (SETQ ßRESULT (CQ PASV))
        (COND (ßRESULT (GO PONT)))
ONT1
        (and LABELTRACE (PASSING 'ONT1))
        (SETQ ßRESULT (CALLSM (SMCL1)))
        (COND ((nil? ßRESULT) (M SMCL1) (GO FAIL)))
        (SETQ ßRESULT (not (CQ REL-NOT-FOUND)))
        (COND (ßRESULT (COND ((nil? NN) (GO RETSM)) (:else (GO TONT)))))
        (SETQ ßRESULT (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1))
        (COND ((nil? ßRESULT) (GO PREPSHORT)))
TIMEQ
        (and LABELTRACE (PASSING 'TIMEQ))
        (RQ REL-NOT-FOUND)
        (FQ TIMEQ)
        (GO TONT)
PREPSHORT
        (and LABELTRACE (PASSING 'PREPSHORT))
        (SETQ ßRESULT (and (NQ PREP) (PARSE PREPG)))
        (COND ((nil? ßRESULT) (M ONT-SHORT-PREP) (GO FAIL)))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND ((nil? ßRESULT) (M ONTß) (GO FAIL)))
        (SETQ ßRESULT (CQ REL-NOT-FOUND))
        (COND (ßRESULT (COND ((nil? NN) (M ONT-NOT-FOUND) (GO FAIL)) (:else (GO PREPSHORT))))
            (:else (GO TONT)))
PONT
        (and LABELTRACE (PASSING 'PONT))
        (and (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))
        (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)
        (GO ONT1)
TONT
        (and LABELTRACE (PASSING 'TONT))
        (SETQ ßRESULT (SETQ POSITION-OF-PTW N))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO RETSM)))))
NPASV
        (and LABELTRACE (PASSING 'NPASV))
        (SETQ ßRESULT (and (NQ PREP) (PARSE PREPG) (CALLSM (SMRELATE H))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (NQ TIMW) (PARSE ADV TIMW) (or (CALLSM (SMTIME)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (not (CQ BE)) (PARSE ADJG ADV) (or (CALLSM (SMRELATE H)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (PARSE NG TIME) (or (CALLSM (SMTIME)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (NQ PLACE) (PARSE ADV PLACE) (or (CALLSM (SMPLACE)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (NQ BINDER) (PARSE CLAUSE BOUND) (or (CALLSM (SMBIND)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (NEXTWORD? 'TO) (PARSE CLAUSE TO ADJUNCT) (or (CALLSM (SMTOADJ)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (= N POSITION-OF-PTW))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO TONT)))))
        (SETQ ßRESULT (or (not (CQ TOPLEVEL)) (NQ SPECIAL)))
        (COND (ßRESULT (GO RETSM)))
        (ert "CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL")
        (GO FAIL)
THERE
        (and LABELTRACE (PASSING 'THERE))
        (FQ THERE)
        (CUT END)
        (SETQ ßRESULT (PARSE ADV TIMW))
        (COND ((and (nil? NN) ßRESULT) (M THERE) (GO FAIL)))
        (SETQ ßRESULT (and (PARSE VG) (ISQ MVB BE)))
        (COND (ßRESULT (COND ((nil? NN) (M THERE) (GO FAIL)) (:else (GO THEF))))
            (:else (GO NOTHE)))
THERQ
        (and LABELTRACE (PASSING 'THERQ))
        (SETQ ßRESULT (ISQ (MOVE-PT H PV (QAUX)) BE))
        (COND (ßRESULT (GO THERQ2)))
        (SETQ ßRESULT (and (NQ TIMW) (PARSE ADV TIMW)))
        (COND ((and (nil? NN) ßRESULT) (M THEREQ) (GO FAIL)))
        (SETQ ßRESULT (and (PARSE VG) (ISQ MVB BE)))
        (COND (ßRESULT (GO THERQ2)))
        (RQ POLR2)
        (GO NOTHE)
THERQ2
        (and LABELTRACE (PASSING 'THERQ2))
        (FQ SUBJTQ)
        (FQ THERE)
        (SETQ ßRESULT (CQ POLAR))
        (COND (ßRESULT (GO THEF)) (:else (GO ONT)))
THEF
        (and LABELTRACE (PASSING 'THEF))
        (SETQ ßRESULT (and (NQ ADV) (PARSE ADV TIMW)))
        (COND ((and (nil? NN) ßRESULT) (M THEF) (GO FAIL)))
        (SETQ ßRESULT (PARSE NG SUBJ SUBJT))
        (COND ((nil? ßRESULT) (GO THERREL)))
        (FQ THERE)
        (SETR 'SUBJECT H C)
        (GO ONT)
THERREL
        (and LABELTRACE (PASSING 'THERREL))
        (SETQ ßRESULT (MOVE-PT C U (REL-NOT-FOUND)))
        (COND ((nil? ßRESULT) (GO NOTHE)))
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (GO ONT)
NOTHE
        (and LABELTRACE (PASSING 'NOTHE))
        (RQ THERE)
        (POP THERE)
        (and (NQ ADV) (PARSE ADV PLACE))
        (GO THER2)
IMPER
        (and LABELTRACE (PASSING 'IMPER))
        (SETQ ßRESULT (PARSE NG TIME))
        (COND ((and (nil? NN) ßRESULT) (GO IMPOP)))
        (SETQ ßRESULT (and (NQ ADV) (PARSE ADJG ADV VBAD)))
        (COND ((and (nil? NN) ßRESULT) (GO IMPOP)))
        (SETQ ßRESULT (and (NQ ADV) (PARSE ADV TIMW)))
        (COND ((and (nil? NN) ßRESULT) (GO IMPOP)))
IMPE
        (and LABELTRACE (PASSING 'IMPE))
        (SETQ ßRESULT (PARSE VG IMPER))
        (COND ((nil? ßRESULT) (GO IMPOP)))
        (FQ IMPER)
        (GO VG1)
IMPOP
        (and LABELTRACE (PASSING 'IMPOP))
        (SETQ ßRESULT (POP nil))
        (COND (ßRESULT (GO IMPE)) (:else (M IMPOP) (GO FAIL)))
QUEST
        (and LABELTRACE (PASSING 'QUEST))
        (FQ QUEST)
        (SETQ ßRESULT (NQ PREP))
        (COND ((nil? ßRESULT) (GO NGQUES)))
        (SETQ ßRESULT (PARSE PREPG))
        (COND ((nil? ßRESULT)
            (COND ((nil? NN) (M PREPQ-INCOMPLETE) (GO FAIL)) (:else (GO NGQUES)))))
        (SETQ ßRESULT (ISQ H QUEST))
        (COND ((nil? ßRESULT) (GO QUEST)))
        (SETR 'QADJ H C)
        (GO POLAR)
NGQUES
        (and LABELTRACE (PASSING 'NGQUES))
        (SETQ ßRESULT (PARSE NG QUEST))
        (COND (ßRESULT (GO NGQST)))
        (SETQ ßRESULT
        (or (and (NEXTWORD? 'HOW) (PARSE ADJG QUEST) (SETR 'RELHEAD H C)) (and (NQ QADJ) (PARSE QADJ) (FQ QADJ) (SETR 'QADJ H C))))
        (COND (ßRESULT (GO POLAR)) (:else (GO POLAR)))
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))
ADJQS
        (and LABELTRACE (PASSING 'ADJQS))
        (GO RETURN)
NGQST
        (and LABELTRACE (PASSING 'NGQST))
        (SETR 'RELHEAD H C)
NGQST2
        (and LABELTRACE (PASSING 'NGQST2))
        (CUT END)
        (SETR 'SUBJECT H C)
        (and (NQ ADV) (PARSE ADJG ADV VBAD))
        (COND ((PARSE VG NAUX) (FQ SUBJQ) (GO VG1))
            ((NQ VB) (FQ REL-NOT-FOUND) (GO POLAR))
            (:else (MOVE-PTW N PW) (POP NG QUEST) (CUT PTW) (GO NGQUES)))
QUEST2
        (and LABELTRACE (PASSING 'QUEST2))
        (SETQ ßRESULT (and (NEXTWORD? 'THERE) (PARSE nil THERE)))
        (COND (ßRESULT (GO THERQ)) (:else (GO SUBF)))
SUBF
        (and LABELTRACE (PASSING 'SUBF))
        (SETQ ßRESULT (PARSE NG SUBJ))
        (COND (ßRESULT (COND ((nil? NN) (GO SUBJ1)) (:else (GO SUBREG)))))
        (RQ REL-NOT-FOUND)
        (GO BE)
POLAR
        (and LABELTRACE (PASSING 'POLAR))
        (SETQ ßRESULT (and (NQ VB) (PARSE VB AUX (QAUX)) (SETR 'QAUX H C) (CALLSM (SMVAUX)) (SETMVB H)))
        (COND ((nil? ßRESULT) (GO QCHOP)))
        (or (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
        (FQ POLR2)
        (GO QUEST2)
QCHOP
        (and LABELTRACE (PASSING 'QCHOP))
        (ert "CLAUSE: QCHOP")
        (SETQ ßRESULT (POPTO CLAUSE BOUND))
        (COND (ßRESULT (GO BICUT)) (:else (M QCHOP) (GO FAIL)))
SEC
        (and LABELTRACE (PASSING 'SEC))
        (COND ((CQ BOUND) (GO BOUND))
            ((CQ TO) (GO TO))
            ((CQ RSQ) (GO RSQ))
            ((CQ REPORT) (GO REPORT))
            ((CQ ING) (GO ING))
            (:else (MQ RSNG-TYPE) (GO FAIL)))
BOUND
        (and LABELTRACE (PASSING 'BOUND))
        (SETQ ßRESULT (PARSE BINDER))
        (COND ((nil? ßRESULT)
            (COND ((nil? NN) (M BINDER) (GO FAIL)) (:else (M BOUND) (GO FAIL)))))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RSQ
        (and LABELTRACE (PASSING 'RSQ))
        (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
        (SETQ ßRESULT (CQ PREPREL))
        (COND ((nil? ßRESULT) (GO RSQ2)))
        (PARSE PREPG PRONREL)
        (SETR 'QADJ H C)
        (GO REPORT)
RSQ2
        (and LABELTRACE (PASSING 'RSQ2))
        (COND ((PARSE VG EN PASV)
                (or (ISQ MVB TRANS) (GO FAIL))
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VG1))
            ((PARSE VG ING) (SETR 'SUBJECT (GETR 'RELHEAD C) C) (GO VG1))
            ((NQ PRONREL) (PARSE NG RELWD) (GO REL))
            ((CQ COMPONENT)
                (SETR 'RELHEAD (GETR 'RELHEAD (MOVE-PT C PC)) C)
                (GO REL))
            ((PARSE NG SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
            (:else (GO FAIL)))
REL
        (and LABELTRACE (PASSING 'REL))
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (SETQ ßRESULT (PARSE VG))
        (COND (ßRESULT (GO VG1)))
        (FQ REL-NOT-FOUND)
        (GO SUBJ)
TO
        (and LABELTRACE (PASSING 'TO))
        (SETQ ßRESULT (and (CQ COMPONENT) (PARSE VG TO TODEL)))
        (COND (ßRESULT (GO VG1)))
        (SETQ ßRESULT (NEXTWORD? 'FOR))
        (COND ((nil? ßRESULT) (GO TO1)))
        (PARSE nil FOR)
        (FQ FOR)
        (PARSE NG SUBJ TOSUBJ)
        (SETR 'SUBJECT H C)
TO1
        (and LABELTRACE (PASSING 'TO1))
        (SETQ ßRESULT (PARSE VG TO))
        (COND (ßRESULT (GO VG1)) (:else (M TO) (GO FAIL)))
ING
        (and LABELTRACE (PASSING 'ING))
        (SETQ ßRESULT (MOVE-PTW N NW (ING)))
        (COND ((nil? ßRESULT) (GO FAIL)))
        (SETQ ßRESULT (or (NQ ING) (CQ OBJ2) (and (PARSE NG SUBJ INGSUBJ) (SETR 'SUBJECT H C) (FQ SUBING) (RQ ING))))
        (COND ((and (nil? NN) ßRESULT) (M ING) (GO FAIL)))
        (SETQ ßRESULT (PARSE VG ING))
        (COND (ßRESULT (GO VG1)) (:else (M ING) (GO FAIL)))
REPORT
        (and LABELTRACE (PASSING 'REPORT))
        (and (NEXTWORD? 'THAT) (PARSE nil THAT) (FQ THAT))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (or (CALLSM (SMCL2)) (GO FAIL))
        (GO RETURN)
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ defn- NG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-NG
        (and LABELTRACE (PASSING 'ENTERING-NG))
NGSTART
        (and LABELTRACE (PASSING 'NGSTART))
        (COND ((CQ RELWD) (GO RELWD))
            ((CQ QUEST) (GO QUEST))
            ((or (NQ QDET) (NQ QPRON)) (FQ QUEST) (GO QUEST))
            ((CQ TIME) (GO TIME))
            ((NQ PROPN) (GO PROPN))
            ((NQ TPRON) (GO TPRON))
            ((NQ EVERPRON) (GO EVERPRON))
            ((NQ PRON) (GO PRON)))
LOOK
        (and LABELTRACE (PASSING 'LOOK))
        (COND ((NQ DET) (GO DET))
            ((NQ NUM) (GO NUM))
            ((or (NQ ING) (NQ EN) (NQ ADJ)) (GO ADJ))
            ((NQ CLASF) (GO CLASF))
            ((NQ NUMD) (GO NUMD))
            ((NEXTWORD? 'AT) (GO AT))
            ((NEXTWORD? 'AS) (GO AS))
            ((NQ NOUN) (GO NOUN))
            ((NQ TIMORD) (GO TIMORD))
            ((and (CQ COMPONENT) (ISQ (MOVE-PT PC) QUEST)) (GO QUEST))
            ((MQ START) (GO FAIL)))
START
        (and LABELTRACE (PASSING 'START))
PROPN
        (and LABELTRACE (PASSING 'PROPN))
        (PARSE PROPN)
        (FQ DEF PROPNG)
        (SETQ ßRESULT (ISQ H POSS))
        (COND (ßRESULT (GO PROPS)))
        (SETQ ßRESULT (and NN (NQ PROPN)))
        (COND (ßRESULT (GO PROPN)))
PROPS
        (and LABELTRACE (PASSING 'PROPS))
        (or (CALLSM (SMPROP)) (GO FAIL))
        (SETQ ßRESULT (ISQ H POSS))
        (COND (ßRESULT (GO POSS)) (:else (GO PRAG)))
PRON
        (and LABELTRACE (PASSING 'PRON))
        (SETQ ßRESULT (PARSE PRON POSS))
        (COND (ßRESULT (COND ((nil? NN) (GO RED2)) (:else (GO POSS)))))
PRON2
        (and LABELTRACE (PASSING 'PRON2))
        (SETQ ßRESULT (CQ NPRON))
        (COND (ßRESULT (M NPRON) (GO FAIL)))
        (SETQ ßRESULT (or (and (CQ SUBJ) (PARSE PRON SUBJ)) (and (or (CQ OBJ) (CQ TOSUBJ) (CQ INGSUBJ)) (PARSE PRON OBJ)) (CQ INGSUBJ)))
        (COND ((nil? ßRESULT) (M PRON) (GO FAIL)))
        (FQ PRONG DEF)
PRON3
        (and LABELTRACE (PASSING 'PRON3))
        (SETQ ßRESULT (CALLSM (SMPRON H)))
        (COND ((nil? ßRESULT) (GO FAIL)))
PRAG
        (and LABELTRACE (PASSING 'PRAG))
        (SETR 'HEAD H C)
        (MOVE-PT H)
        (TRNSF NS NPL NFS NEG)
        (GO RETURN)
TPRON
        (and LABELTRACE (PASSING 'TPRON))
        (PARSE TPRON)
        (FQ TPRON)
        (MOVE-PT H)
        (TRNSF NS NPL ANY NEG)
        (SETR 'HEAD C H)
        (and NN (NQ ADJ) (PARSE ADJ))
        (GO SMNG)
EVERPRON
        (and LABELTRACE (PASSING 'EVERPRON))
        (SETQ ßRESULT (and (PARSE PRON EVERPRON) (CALLSM (SMPRON H))))
        (COND ((nil? ßRESULT) (GO FAIL)))
        (SETQ ßRESULT (and (PARSE CLAUSE RSQ NOREL) (CALLSM (SMRELATE H))))
        (COND (ßRESULT (GO RETSM)) (:else (GO FAIL)))
AS
        (and LABELTRACE (PASSING 'AS))
        (SETQ ßRESULT (and (PARSE nil AS) (PARSE NUMD NUMDAS) NN (PARSE nil AS)))
        (COND (ßRESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO NUMD2)))) (:else (M AS) (GO FAIL)))
AT
        (and LABELTRACE (PASSING 'AT))
        (SETQ ßRESULT (and (PARSE nil AT) (PARSE NUMD NUMDAT)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (M AT) (GO FAIL)) (:else (M AT) (GO FAIL)))))
NUMD2
        (and LABELTRACE (PASSING 'NUMD2))
        (SETQ ßRESULT (and (PARSE NUM) (FQ NUM NUMD)))
        (COND (ßRESULT (COND ((nil? NN) (GO INCOM)) (:else (GO DET1)))) (:else (M NUMD2) (GO FAIL)))
NUMD
        (and LABELTRACE (PASSING 'NUMD))
        (SETQ ßRESULT (PARSE NUMD NUMDAN))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO ND3)))))
        (SETQ ßRESULT (PARSE nil THAN))
        (COND (ßRESULT (COND ((nil? NN) (GO POPCOM)) (:else (GO NUMD2)))) (:else (GO INCOM)))
ND3
        (and LABELTRACE (PASSING 'ND3))
        (SETQ ßRESULT (PARSE NUMD NUMDALONE))
        (COND (ßRESULT (COND ((nil? NN) (M NUMD) (GO FAIL)) (:else (GO NUMD2)))) (:else (M NUMD) (GO FAIL)))
TIME
        (and LABELTRACE (PASSING 'TIME))
        (SETQ ßRESULT (and (NQ TIME) (PARSE NOUN TIME)))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (MOVE-PTW N NW (TIM1)))
        (COND (ßRESULT (GO LOOK)) (:else (M TIME) (GO FAIL)))
TIMORD
        (and LABELTRACE (PASSING 'TIMORD))
        (SETQ ßRESULT (PARSE ORD TIMORD))
        (COND ((nil? ßRESULT) (GO FAIL)))
        (SETQ ßRESULT (and (PARSE NOUN TIM1) (FQ DET DEF) (CALLSM (SMNGTIME))))
        (COND (ßRESULT (GO RETURN)) (:else (GO FAIL)))
DET
        (and LABELTRACE (PASSING 'DET))
        (PARSE DET)
        (FQ DET)
        (MOVE-PT H)
        (SETQ ßRESULT (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR))
        (COND (ßRESULT (COND ((nil? NN) (GO INCOM)) (:else (GO IND)))) (:else (M bug) (GO FAIL)))
IND
        (and LABELTRACE (PASSING 'IND))
        (SETQ ßRESULT (and (= (WORD (NB H)) 'ALL) (= (WORD N) 'THE) (PARSE DET) (FQ DEF)))
        (COND (ßRESULT (COND ((nil? NN) (M THE) (GO FAIL)) (:else (GO NUM)))))
        (SETQ ßRESULT (and (ISQ H QNTFR) (FQ QNTFR)))
        (COND (ßRESULT (GO QNUM)))
ORD
        (and LABELTRACE (PASSING 'ORD))
        (SETQ ßRESULT (and (PARSE ORD) (FQ ORD)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO NUM)))))
        (SETQ ßRESULT (and (NEXTWORD? 'OF) (ISQ (MOVE-PTW N NW) MONTH) (PARSE nil OF) (PARSE NOUN MONTH) (FQ DATE)))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (CQ DEF))
        (COND ((nil? ßRESULT) (GO ADJ)))
NUM
        (and LABELTRACE (PASSING 'NUM))
        (SETQ ßRESULT (PARSE NUM))
        (COND ((nil? ßRESULT) (GO ADJ)))
        (FQ NUM)
        (SETQ ßRESULT (CQ DET))
        (COND ((nil? ßRESULT) (GO DET1)))
        (SETQ ßRESULT (COND ((and (ISQ H NS) (CQ NS)) (RQ NPL PART)) ((CQ NPL) (RQ NS PART))))
        (COND (ßRESULT (COND ((nil? NN) (GO INCOM)) (:else (GO ADJ)))) (:else (M NUM) (GO FAIL)))
DET1
        (and LABELTRACE (PASSING 'DET1))
        (COND ((ISQ H NS) (FQ NS)) (:else (FQ NPL)))
        (or NN (and (FQ NUMBER) (GO INCOM)))
NUMBER
        (and LABELTRACE (PASSING 'NUMBER))
        (FQ DET)
        (SETQ ßRESULT (NQ OF))
        (COND (ßRESULT (GO OF)) (:else (GO ADJ)))
QNUM
        (and LABELTRACE (PASSING 'QNUM))
        (SETQ ßRESULT (ISQ H NONUM))
        (COND (ßRESULT (GO OF)))
        (SETQ ßRESULT (and (PARSE NUM) (FQ NUM)))
        (COND ((nil? ßRESULT) (GO OF)))
        (SETQ ßRESULT (COND ((== (SM H) 1) (and (CQ NS) (RQ NPL))) ((CQ NPL) (RQ NS))))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (M NUMD) (GO FAIL)))))
        (SETQ ßRESULT (= (WORD (NB H)) 'NO))
        (COND (ßRESULT (GO ADJ)))
OF
        (and LABELTRACE (PASSING 'OF))
        (SETQ ßRESULT (and (NQ OF) (PARSE PREPG OF)))
        (COND (ßRESULT (GO SMOF)) (:else (GO NONE)))
SMOF
        (and LABELTRACE (PASSING 'SMOF))
        (FQ OF)
        (SETQ ßRESULT (or (CALLSM (SMNGOF)) (not (POP))))
        (COND (ßRESULT (GO RETSM)) (:else (GO INCOM)))
NONE
        (and LABELTRACE (PASSING 'NONE))
        (SETQ ßRESULT (= (WORD (NB H)) 'NONE))
        (COND (ßRESULT (GO INCOM)) (:else (GO ADJ)))
ADJ
        (and LABELTRACE (PASSING 'ADJ))
        (SETQ ßRESULT (PARSE ADJ))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO EPR)))))
        (and (ISQ H COMPAR) (FQ COMPARATIVE-MODIFIER) (SETR 'COMPARATIVE-MODIFIER H C))
        (GO ADJ)
EPR
        (and LABELTRACE (PASSING 'EPR))
        (SETQ ßRESULT (or (ISQ H SUP) (ISQ H COMPAR)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO CLASF)))))
        (FQ ADJ)
        (and (NEXTWORD? 'OF)
            (PARSE PREPG OF)
            (or (CALLSM (SMNGOF)) (GO FAIL))
            (FQ OF)
            (GO RETSM))
CLASF
        (and LABELTRACE (PASSING 'CLASF))
        (SETQ ßRESULT (or (PARSE VB ING (CLASF)) (PARSE VB EN (CLASF)) (PARSE CLASF)))
        (COND (ßRESULT (COND ((nil? NN) (GO REDUC)) (:else (GO CLASF)))))
NOUN
        (and LABELTRACE (PASSING 'NOUN))
        (SETQ ßRESULT (PARSE NOUN))
        (COND ((nil? ßRESULT) (GO RED2)))
        (SETQ ßRESULT (and (CQ TIME) (not (ISQ H TIM1))))
        (COND (ßRESULT (GO RED1)))
        (SETQ T1 FE)
        (COND ((and (ISQ H MASS) (or (CQ PART) (not (CQ DET)))) (FQ MASS)))
        (COND ((not (ISQ H NPL)) (RQ NPL PART)))
        (COND ((not (ISQ H NS)) (RQ NS)))
        (COND ((and (not (CQ DET)) (not (CQ NUMD))) (MOVE-PT H) (TRNSF NPL MASS)))
        (SETQ ßRESULT (MEET FE '(NS NPL PART MASS)))
        (COND ((nil? ßRESULT) (GO RED0)))
        (SETQ ßRESULT (NEXTWORD? 'THAN))
        (COND ((nil? ßRESULT) (GO SMNG)))
        (FQ THAN)
SMNG
        (and LABELTRACE (PASSING 'SMNG))
        (SETR 'HEAD H C)
        (SETQ ßRESULT (and (CQ OBOFJ) (not (CQ DEF))))
        (COND (ßRESULT (GO FAIL)))
        (or (CALLSM (SMNG1)) (GO FAIL))
        (SETQ ßRESULT (not (ISQ H POSS)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO POSS)))))
        (SETQ ßRESULT (and (CQ THAN) (PARSE ADJG)))
        (COND ((nil? ßRESULT) (GO RSQ-TO)))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND (ßRESULT (GO RETSM)) (:else (GO FAIL)))
RSQ-TO
        (and LABELTRACE (PASSING 'RSQ-TO))
        (SETQ ßRESULT (and (NEXTWORD? 'TO) (MEET FE '(COMP SUBJ)) (PARSE CLAUSE RSQ TO) (or (CALLSM (SMRELATE H)) (GO POPRET))))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (and (or (NEXTWORD? 'AS) (NQ COMPAR)) (PARSE ADJG THANNEED)))
        (COND ((nil? ßRESULT) (GO PREPNG)))
        (and (nil? N) (CQ SUBJ) (ISQ (MOVE-PT C PV) AUX) (ISQ PT BE) (GO POPRET))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND (ßRESULT (COND ((nil? NN) (GO RETSM)) (:else (GO RSQ-TO)))) (:else (GO POPRET)))
PREPNG
        (and LABELTRACE (PASSING 'PREPNG))
        (SETQ ßRESULT (and (NQ PREP) (not (or (and (NQ PLACE) (CQ NOLOC)) (and (CQ OBJ1) (ISQ MVB TRANSL) (not (ISQ (MOVE-PT C U) QUEST))))) (PARSE PREPG Q)))
        (COND ((nil? ßRESULT) (GO DISGRSQ)))
        (and (nil? N)
            (CQ SUBJ)
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (not (ISQ (MOVE-PT U) NGQ))
            (GO POPRET))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND (ßRESULT (COND ((nil? NN) (GO RETSM)) (:else (GO RSQ-TO)))) (:else (GO POPRET)))
DISGRSQ
        (and LABELTRACE (PASSING 'DISGRSQ))
        (SETQ ßRESULT (= (car MES) 'PREP-WHICH))
        (COND ((nil? ßRESULT) (GO RSQ)))
        (SETQ MES (cdr MES))
        (SETQ ßRESULT (PARSE CLAUSE RSQ PREPREL))
        (COND (ßRESULT (COND ((nil? NN) (GO RETSM)) (:else (GO PREPNG)))) (:else (M RSQ-PREPREL) (GO FAIL)))
RSQ
        (and LABELTRACE (PASSING 'RSQ))
        (SETQ ßRESULT (and (ISQ (MOVE-PT C U) POLR2) (CQ SUBJ) (NQ VB) (not (CQ SUBJT)) (not (ISQ PT QADJ))))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (PARSE CLAUSE RSQ))
        (COND ((nil? ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND (ßRESULT (GO RETSM)) (:else (GO POPRET)))
RED0
        (and LABELTRACE (PASSING 'RED0))
        (SETQ FE T1)
RED1
        (and LABELTRACE (PASSING 'RED1))
        (POP)
RED2
        (and LABELTRACE (PASSING 'RED2))
        (COND ((nil? H) (MQ NO) (GO FAIL))
            ((ISQ H NUMBER) (GO INCOM))
            ((and (ISQ H POSS) (or (ISQ H PRON) (and (MOVE-PT H DLC) (ISQ PT PRON)))) (POP) (GO PRON2))
            ((and (nil? (cdr H)) (CQ DEFPOSS)) (GO POSSDEF))
            ((and (CQ QUEST) (nil? (cdr H))) (GO QDETCHECK))
            ((ISQ H ADJ) (GO EPR))
            ((not (ISQ H CLASF)) (GO INCOM)))
REDUC
        (and LABELTRACE (PASSING 'REDUC))
        (POP)
        (SETQ ßRESULT (and (nil? H) (NQ PROPN)))
        (COND (ßRESULT (GO PROPN)) (:else (GO NOUN)))
POPCOM
        (and LABELTRACE (PASSING 'POPCOM))
        (POP)
INCOM
        (and LABELTRACE (PASSING 'INCOM))
        (FQ INCOM)
        (SETQ ßRESULT (and (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM))))
        (COND (ßRESULT (GO RETURN)))
        (SETQ ßRESULT (and (nil? CUT) (CQ NUM)))
        (COND (ßRESULT (GO SMNG)))
QDETCHECK
        (and LABELTRACE (PASSING 'QDETCHECK))
        (COND ((and (ISQ H QDET) (ISQ (NB H) QPRON)) (POP) (GO QPRON))
            ((and (ISQ H QDET) (ISQ (NB H) EVERPRON)) (POP) (GO EVERPRON)))
        (GO FAIL)
POSS
        (and LABELTRACE (PASSING 'POSS))
        (or (CALLSM (SMNG2)) (GO FAIL))
POSS2
        (and LABELTRACE (PASSING 'POSS2))
        (SETQ ßRESULT (CQ INGSUBJ))
        (COND (ßRESULT (GO RETSM)))
        (SETQ H (BUILDNODE (reverse (cons 'POSS (SETDIF FE '(COMPONENT)))) NB N H SM))
        (SETQ BACKREF (concat H (cdr BACKREF)))
        (SETQ ßRESULT (SETR 'FEATURES (SETQ FE (concat '(POSES DET DEF NS NPL) (reverse REST))) C))
        (COND ((nil? ßRESULT) (M bug) (GO FAIL)))
        (SETQ ßRESULT (or (not NN) (ISQ H DEFPOSS)))
        (COND ((nil? ßRESULT) (GO ORD)))
POSSDEF
        (and LABELTRACE (PASSING 'POSSDEF))
        (RQ POSES DET DEF)
        (FQ POSSDEF NS NPL)
QUEST
        (and LABELTRACE (PASSING 'QUEST))
        (SETQ ßRESULT (PARSE nil HOW))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO FAIL)) (:else (GO QDET)))))
        (SETQ ßRESULT (PARSE nil MANY))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO FAIL)))))
        (FQ DET NPL INDEF HOWMANY)
        (GO OF)
QDET
        (and LABELTRACE (PASSING 'QDET))
        (SETQ ßRESULT (and (PARSE DET QDET) (FQ DET NPL QDET NS)))
        (COND (ßRESULT (COND ((nil? NN) (GO INCOM)) (:else (GO QNUM)))))
QPRON
        (and LABELTRACE (PASSING 'QPRON))
        (SETQ ßRESULT (PARSE PRON QPRON))
        (COND (ßRESULT (GO PRON3)) (:else (GO FAIL)))
RELWD
        (and LABELTRACE (PASSING 'RELWD))
        (SETQ ßRESULT (and (PARSE PRONREL) (CALLSM (SMSET (SM (MOVE-PT C U U (NG)))))))
        (COND (ßRESULT (GO RETURN)))
POPRET
        (and LABELTRACE (PASSING 'POPRET))
        (POP)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (or (CALLSM (SMNG2)) (GO TRYA))
        (GO RETURN)
TRYA
        (and LABELTRACE (PASSING 'TRYA))
        (SETQ ßRESULT (ISQ H NOUN))
        (COND ((nil? ßRESULT) (M TRYA) (GO FAIL)))
        (POP)
        (CUT N)
UP
        (and LABELTRACE (PASSING 'UP))
        (SETQ ßRESULT (POP))
        (COND (ßRESULT (GO UP)))
        (SETQ FE (reverse REST))
        (SMSET nil)
        (GO NGSTART)
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ defn- VG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil TENSE nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-VG
        (and LABELTRACE (PASSING 'ENTERING-VG))
        (COND ((CQ TO) (GO TO))
            ((CQ EN) (GO EN))
            ((CQ ING) (GO ING))
            ((CQ IMPER) (GO IMPER))
            ((ISQ (MOVE-PT C U) POLR2) (GO POLR2)))
NEW
        (and LABELTRACE (PASSING 'NEW))
        (COND ((not (NQ VB)) (MQ VB) (GO FAIL))
            ((and (NQ DO) (PARSE VB AUX DO)) (GO DO))
            ((and (NQ MODAL) (PARSE VB AUX MODAL)) (GO MODAL))
            ((and (NQ WILL) (PARSE VB AUX WILL)) (GO WILL))
            ((and (NQ BE) (PARSE VB AUX BE)) (GO BE))
            ((and (NQ HAVE) (PARSE VB AUX HAVE)) (GO HAVE))
            ((not (PARSE VB (MVB))) (MQ VB) (GO FAIL)))
SIMPLE
        (and LABELTRACE (PASSING 'SIMPLE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS)
        (SETQ TENSE (COND ((and (ISQ PT PRESENT) (ISQ PT PAST)) '(PAST-PRESENT)) ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (GO REV)
TO
        (and LABELTRACE (PASSING 'TO))
        (FQ NAGR)
        (SETQ ßRESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M NOT) (GO FAIL)))
        (SETQ ßRESULT (or (PARSE nil TO) (CQ TODEL)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (M TO) (GO FAIL)) (:else (M TO) (GO FAIL)))))
        (SETQ TENSE '(INFINITIVE))
        (GO MODAL2)
EN
        (and LABELTRACE (PASSING 'EN))
        (FQ NAGR)
        (SETQ ßRESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M NOT) (GO FAIL)))
        (SETQ TENSE '(PAST))
        (SETQ ßRESULT (and (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)))
        (COND (ßRESULT (GO RETSM)) (:else (GO FAIL)))
ING
        (and LABELTRACE (PASSING 'ING))
        (FQ NAGR)
        (SETQ ßRESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M NOT) (GO FAIL)))
INGADV
        (and LABELTRACE (PASSING 'INGADV))
        (SETQ ßRESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (ßRESULT (GO INGADV)))
        (SETQ TENSE '(PRESENT))
        (GO BE2)
IMPER
        (and LABELTRACE (PASSING 'IMPER))
        (SETQ ßRESULT (and (PARSE VB DO NEG INF) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M DONT) (GO FAIL)))
        (SETQ ßRESULT (and (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG))))
        (COND (ßRESULT (GO RETURN)) (:else (M IMPER) (GO FAIL)))
POLR2
        (and LABELTRACE (PASSING 'POLR2))
        (or (SETQ PT (GETR 'QAUX (MOVE-PT C U))) (and (bug "VGßPOLR2") (GO FAIL)))
        (SETQ H (list (car PT)))
        (TRNSF NEG)
        (COND ((ISQ H DO) (GO DO))
            ((ISQ H MODAL) (GO MODAL))
            ((ISQ H WILL) (GO WILL))
            ((ISQ H BE) (GO BE))
            ((ISQ H HAVE) (GO HAVE)))
        (ert "BUG VGßPOLR2VB")
        (GO FAIL)
DO
        (and LABELTRACE (PASSING 'DO))
        (FQ DO)
        (MOVE-PT C DLC)
        (TRNSF VPL NEG INF V3PS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (COND (NN (GO DO2)) (:else (GO MVB)))
DO2
        (and LABELTRACE (PASSING 'DO2))
        (SETQ ßRESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M NOT) (GO FAIL)))
ADV2
        (and LABELTRACE (PASSING 'ADV2))
        (SETQ ßRESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (ßRESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV2)))))
        (SETQ ßRESULT (PARSE VB (MVB) INF))
        (COND ((nil? ßRESULT) (GO MVB)))
        (GO REV)
MODAL
        (and LABELTRACE (PASSING 'MODAL))
        (FQ NAGR MODAL)
        (SETQ TENSE '(MODAL))
        (COND (NN (GO MODAL2)) (:else (GO INCOMP)))
MODAL2
        (and LABELTRACE (PASSING 'MODAL2))
        (SETQ ßRESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M NOT) (GO FAIL)))
ADV3
        (and LABELTRACE (PASSING 'ADV3))
        (SETQ ßRESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (ßRESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV3)))))
        (COND ((PARSE VB BE INF) (GOCOND BE2 MVB))
            ((PARSE VB HAVE INF) (GOCOND HAV2 MVB))
            ((PARSE VB INF (MVB)) (GO REV))
            (:else (GO INCOMP)))
WILL
        (and LABELTRACE (PASSING 'WILL))
        (FQ NAGR)
        (SETQ TENSE '(FUTURE))
        (COND (NN (GO MODAL2)) (:else (GO INCOMP)))
BE
        (and LABELTRACE (PASSING 'BE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (COND (NN (GO BE2)) (:else (GO MVB)))
BE2
        (and LABELTRACE (PASSING 'BE2))
        (SETQ ßRESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M NOT) (GO FAIL)))
ADV4
        (and LABELTRACE (PASSING 'ADV4))
        (SETQ ßRESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (ßRESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV4)))))
        (COND ((and (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))
            ((and (NQ BE) (PARSE VB ING)) (SETQ TENSE (cons 'PRESENT TENSE)) (GO EN2))
            ((and (NQ ING) (PARSE VB ING (MVB))) (SETQ TENSE (cons 'PRESENT TENSE)) (GO REV))
            ((CQ ING) (MQ ING) (GO FAIL)))
EN2
        (and LABELTRACE (PASSING 'EN2))
        (SETQ ßRESULT (PARSE VB EN (MVB)))
        (COND ((nil? ßRESULT) (GO MVBE)))
        (FQ PASV)
        (GO REV)
GOING
        (and LABELTRACE (PASSING 'GOING))
        (SETQ ßRESULT (PARSE nil TO))
        (COND ((nil? ßRESULT) (GO GOI)))
        (SETQ ßRESULT (NQ INF))
        (COND (ßRESULT (GO GOING2)))
        (POP)
GOI
        (and LABELTRACE (PASSING 'GOI))
        (SETQ TENSE (cons 'PRESENT TENSE))
        (GO MVB)
GOING2
        (and LABELTRACE (PASSING 'GOING2))
        (SETQ TENSE (cons 'FUTURE TENSE))
        (GO MODAL2)
MVBE
        (and LABELTRACE (PASSING 'MVBE))
        (SETQ ßRESULT (ISQ (MOVE-PT H PV (VB)) AUX))
        (COND ((nil? ßRESULT) (GO MVB)))
        (SETQ ßRESULT (ISQ PT BE))
        (COND ((nil? ßRESULT) (M MVBE) (GO FAIL)))
        (SETMVB PT)
        (GO REV)
HAVE
        (and LABELTRACE (PASSING 'HAVE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST)) (:else '(PRESENT))))
        (COND (NN (GO HAV2)) (:else (GO MVB)))
HAV2
        (and LABELTRACE (PASSING 'HAV2))
        (SETQ ßRESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (M NOT) (GO FAIL)))
ADV5
        (and LABELTRACE (PASSING 'ADV5))
        (SETQ ßRESULT (PARSE ADV))
        (COND (ßRESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV5)))))
        (SETQ ßRESULT (PARSE VB BE EN))
        (COND ((nil? ßRESULT) (GO HAV3)))
        (SETQ TENSE (cons 'PAST TENSE))
        (COND (NN (GO BE2)) (:else (GO MVB)))
HAV3
        (and LABELTRACE (PASSING 'HAV3))
        (SETQ ßRESULT (PARSE VB (MVB) EN))
        (COND ((nil? ßRESULT) (GO MVB)))
        (SETQ TENSE (cons 'PAST TENSE))
        (GO REV)
INCOMP
        (and LABELTRACE (PASSING 'INCOMP))
        (FQ INCOMP)
        (GO FAIL)
MVB
        (and LABELTRACE (PASSING 'MVB))
        (SETQ ßRESULT (= (FE MVB) (FE H)))
        (COND (ßRESULT (GO MVB2)))
        (POP VB)
        (SETQ ßRESULT (PARSE VB (MVB)))
        (COND ((nil? ßRESULT) (M MVB) (GO FAIL)))
MVB2
        (and LABELTRACE (PASSING 'MVB2))
        (GO REV)
REV
        (and LABELTRACE (PASSING 'REV))
        (SETR 'TENSE TENSE C)
        (and NN (PARSE nil NOT) (FQ NEG))
        (COND ((or (= TENSE '(PAST)) (CQ NAGR) (ISQ (MOVE-PT C U) IMPER) (ISQ PT THERE) (ISQ PT RSNG)) (GO NAUX))
            ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))
            (:else (erterr "VG -- NO SUBJECT TO CHECK FOR AGREEMENT")))
        (SETQ T3 nil)
        (COND ((ISQ PT NFS) (or (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))
            ((ISQ PT CLAUSE) (or (SETQ T3 (CQ V3PS)) (GO NAGR)))
            ((or (ISQ PT NS) (ISQ PT MASS)) (or (and (CQ V3PS) (SETQ T3 true)) (FESET PT (SETDIF (FE PT) '(NS MASS))))))
        (COND ((or (ISQ PT PART) (ISQ PT NPL)) (or (and (MEET FE '(INF VPL)) (SETQ T3 true)) (FESET PT (SETDIF (FE PT) '(PART NPL))))))
NAGR
        (and LABELTRACE (PASSING 'NAGR))
        (SETQ ßRESULT (or T3 (and (= '(PAST-PRESENT) TENSE) (SETQ TENSE '(PAST)))))
        (COND ((nil? ßRESULT) (M NAGR) (GO FAIL)))
NAUX
        (and LABELTRACE (PASSING 'NAUX))
        (SETMVB (or (MOVE-PT H PV (MVB)) MVB))
        (SETQ ßRESULT
        (and (CQ NAUX) (ISQ (MOVE-PT H PV (VB)) AUX) (not (MOVE-PT PV PV (VB)))))
        (COND (ßRESULT (M NAUX) (GO FAIL)) (:else (GO RETSM)))
POPV
        (and LABELTRACE (PASSING 'POPV))
        (ert "POPV")
        (GO FAIL)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (SETQ ßRESULT (CALLSM (SMVG)))
        (COND (ßRESULT (GO RETURN)) (:else (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ defn- PREPG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-PREPG
        (and LABELTRACE (PASSING 'ENTERING-PREPG))
ADV
        (and LABELTRACE (PASSING 'ADV))
        (SETQ ßRESULT (and (NQ PREPADV) (PARSE ADV PREPADV)))
        (COND (ßRESULT (COND ((nil? NN) (M PREPADV) (GO FAIL)) (:else (GO ADV)))))
        (SETQ ßRESULT (COND ((CQ AGENT) (NEXTWORD? 'BY)) ((CQ LOC) (NQ PLACE)) ((CQ Q) (not (NQ MOTOR))) (:else true)))
        (COND ((nil? ßRESULT) (M PREP) (GO FAIL)))
        (SETQ ßRESULT (PARSE PREP))
        (COND ((nil? ßRESULT) (M PREP) (GO FAIL)))
        (MOVE-PT H)
        (TRNSF PLACE TIME)
        (SETQ T1 H)
        (and (NQ PREP2)
            (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N))) (PARSE PREP2))
                ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N) (WORD (cdr N)))) (PARSE PREP2) (PARSE PREP2)))
            (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))
            (SETR 'PARENT C T1))
        (SETQ ßRESULT (ISQ H NEED2))
        (COND (ßRESULT (M NEED2) (GO FAIL)))
        (SETR 'HEAD T1 C)
        (or NN (GO SHORT))
        (COND ((= (WORD H) 'BY) (FQ AGENT)))
QUEST
        (and LABELTRACE (PASSING 'QUEST))
        (SETQ ßRESULT (CQ QUEST))
        (COND ((nil? ßRESULT) (GO NG)))
        (SETQ ßRESULT (PARSE NG QUEST OBJ))
        (COND (ßRESULT (GO OBJR)) (:else (M PREPQUEST) (GO FAIL)))
        (SETQ ßRESULT (and (CQ OF) (PARSE NG OFOBJ)))
        (COND (ßRESULT (GO OBJR)))
NG
        (and LABELTRACE (PASSING 'NG))
        (SETQ ßRESULT (PARSE NG OBJ))
        (COND (ßRESULT (GO OBJR)))
REL
        (and LABELTRACE (PASSING 'REL))
        (SETQ ßRESULT (NEXTWORD? 'WHICH))
        (COND ((nil? ßRESULT) (GO REST)))
        (SETQ ßRESULT (ISQ (MOVE-PT U) CLAUSE))
        (COND ((nil? ßRESULT) (M PREP-WHICH) (GO FAIL)))
        (SETQ ßRESULT (ISQ PT PRONREL))
        (COND ((nil? ßRESULT) (GO PRONREL)))
        (SETQ MES (cdr MES))
        (GO P-RELWRD)
PRONREL
        (and LABELTRACE (PASSING 'PRONREL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PRONREL PT)
P-RELWRD
        (and LABELTRACE (PASSING 'P-RELWRD))
        (PARSE NG RELWD OBJ)
        (SETR 'OBJ1 (GETR 'HEAD PT) C)
        (GO RETT)
REST
        (and LABELTRACE (PASSING 'REST))
        (SETQ ßRESULT (PARSE CLAUSE RSNG ING))
        (COND (ßRESULT (GO OBJR)) (:else (GO SHORT)))
OBJR
        (and LABELTRACE (PASSING 'OBJR))
        (SETR 'OBJ1 H C)
        (GO RETT)
SHORT
        (and LABELTRACE (PASSING 'SHORT))
        (SETQ ßRESULT (MEET FE '(NOSHORT Q)))
        (COND (ßRESULT (M SHORT) (GO FAIL)))
        (or (ISQ (MOVE-PT C U) REL-NOT-FOUND) (ISQ (GETR 'QUESTION-ELEMENT PT) QADJ) (GO FAIL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PREPREL PT)
        (SETR 'OBJ1 (GETR 'RELHEAD (MOVE-PT C U)) C)
RETT
        (and LABELTRACE (PASSING 'RETT))
        (and (or (ISQ H QUEST) (and (ISQ H COMPOUND) (MOVE-PT H H PV (QUEST)))) (FQ QUEST))
        (SETQ ßRESULT (CALLSM (SMADJG-PREPG)))
        (COND (ßRESULT (GO RETURN)) (:else (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ defn- ADJG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-ADJG
        (and LABELTRACE (PASSING 'ENTERING-ADJG))
COMPCHECK
        (and LABELTRACE (PASSING 'COMPCHECK))
        (SETQ ßRESULT (and (MOVE-PT C U (BE)) (not (CQ COMP))))
        (COND (ßRESULT (GO FAIL)))
        (SETQ ßRESULT (ISQ (MOVE-PT C U) THAN))
        (COND ((nil? ßRESULT) (GO DISP)))
        (SETR 'HEAD (GETR 'COMPARATIVE-MODIFIER PT) C)
        (GO THAN)
DISP
        (and LABELTRACE (PASSING 'DISP))
        (SETQ ßRESULT (and (NQ AS) (PARSE nil AS)))
        (COND (ßRESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO AS)))))
        (SETQ ßRESULT (and (NQ AS) (PARSE nil AS)))
        (COND (ßRESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO AS)))))
        (SETQ ßRESULT (NEXTWORD? 'HOW))
        (COND (ßRESULT (GO HOW)) (:else (GO ADV)))
HOW
        (and LABELTRACE (PASSING 'HOW))
        (SETQ ßRESULT (and (PARSE nil HOW) (FQ QUEST)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO FAIL)) (:else (GO FAIL)))))
        (SETQ ßRESULT (and (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C)))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (and (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C)))
        (COND (ßRESULT (GO RETSM)) (:else (GO FAIL)))
ADV
        (and LABELTRACE (PASSING 'ADV))
        (SETQ ßRESULT (PARSE ADV ADVADV))
        (COND (ßRESULT (COND ((nil? NN) (GO POPAD)) (:else (GO ADV)))))
        (SETQ ßRESULT (PARSE nil MORE))
        (COND ((nil? ßRESULT) (GO ADJ)))
        (FQ COMPAR)
ADJ
        (and LABELTRACE (PASSING 'ADJ))
        (SETQ ßRESULT (COND ((CQ ADV) (PARSE ADV VBAD)) (:else (PARSE ADJ))))
        (COND ((nil? ßRESULT) (M ADJ) (GO FAIL)))
        (SETQ ßRESULT (SETR 'HEAD H C))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (or (CQ COMPAR) (ISQ H COMPAR)))
        (COND ((nil? ßRESULT) (GO RETSM)))
        (FQ COMPAR)
        (SETQ ßRESULT NN)
        (COND ((nil? ßRESULT) (GO RETSM)))
THAN
        (and LABELTRACE (PASSING 'THAN))
        (COND ((not NN) (GO RETSM)))
        (SETQ ßRESULT (PARSE nil THAN))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (M THAN) (GO FAIL)) (:else (GO RETSM)))))
        (RQ THANNEED)
        (FQ THAN)
        (GO SUBJ)
AS
        (and LABELTRACE (PASSING 'AS))
        (FQ AS)
        (RQ THANNEED)
        (SETQ ßRESULT (and (PARSE ADJ) (SETR 'HEAD H C)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (M ADJ) (GO FAIL)))))
        (SETQ ßRESULT (PARSE nil AS))
        (COND (ßRESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO SUBJ)))) (:else (GO RETSM)))
SUBJ
        (and LABELTRACE (PASSING 'SUBJ))
        (SETQ ßRESULT (PARSE NG SUBJ COMPAR))
        (COND ((nil? ßRESULT) (M THAN) (GO FAIL)))
        (SETQ ßRESULT (SETR 'OBJ1 H C))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (one-word-left NB) (PARSE VB AUX)))
        (COND ((nil? ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (CHECK-AGREEMENT H (cdr H)))
        (COND (ßRESULT (GO RETSM)))
        (POP)
        (GO RETSM)
POPAD
        (and LABELTRACE (PASSING 'POPAD))
        (POP)
        (GO ADJ)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (SETQ ßRESULT (CQ THANNEED))
        (COND (ßRESULT (M THANNEED) (GO FAIL)))
        (SETQ ßRESULT (CALLSM (SMADJG-PREPG)))
        (COND (ßRESULT (GO RETURN)) (:else (M SMADJ) (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ defn- CONJOIN []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil PREV nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-CONJOIN
        (and LABELTRACE (PASSING 'ENTERING-CONJOIN))
UP
        (and LABELTRACE (PASSING 'UP))
        (SETQ PREV (NEXTWORD))
        (FLUSHME)
        (COND ((and (= PREV '\,) (or (cdr H) (> (- (count (NB H)) (count (N H))) 4)) (memq (NEXTWORD) '(OR AND NOR BUT)) (F (NEXTWORD)))
            (SETQ PREV (list PREV (NEXTWORD)))
            (FLUSHME)))
        (and (term? PREV) (MOVE-PTW N NW (= (WORD PTW) PREV)) (CUT PTW))
        (and (or (= PREV 'BUT) (= (cadr PREV) 'BUT)) (NEXTWORD? 'NOT) (or (FLUSHME) (GO LOSE2)) (FQ NEGBUT))
        (SETQ ßRESULT (COND
            ((memq (car REST) '(ADJ NUM NOUN PREP VB ADV)) (PARSE3 (concat REST '(COMPONENT)) nil))
            ((memq (car REST) '(NG PREPG ADJG)) (and (not (CQ OFOBJ)) (PARSE2 (concat REST '(COMPONENT)) nil)))
            ((= (car REST) 'CLAUSE)
                ((lambda [LASTSENT AUXFE]
                    (and (PARSE2 (concat REST AUXFE '(COMPONENT)) nil) (or (not AUXFE) (F (car AUXFE))) (SETR 'TIME (GETR 'TIME H) C)))
                (COND ((ISQ H MAJOR) H) (LASTSENT))
                (MEET (FE H) '(DECLAR IMPER))))))
        (COND ((nil? ßRESULT) (GO LOSE2)))
        (CUT END)
        (COND ((not (term? PREV)) (GO RETSM))
            ((= PREV '\,) (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP)) (:else (GO LIST))))
            ((memq PREV '(AND OR NOR BUT))
                (COND ((= BOTH (NB H)) (FQ BOTH)))
                (COND ((or (NEXTWORD? 'BUT) (and (NEXTWORD? PREV) (not (and (= BOTH (NB H)) (= PREV 'AND)))))
                        (FQ LISTA)
                        (F PREV)
                        (GO UP))
                    (:else (GO LISTA)))))
LOSE2
        (and LABELTRACE (PASSING 'LOSE2))
        (SETQ ßRESULT (CQ LISTA))
        (COND (ßRESULT (GO LISTA)))
LIST
        (and LABELTRACE (PASSING 'LIST))
        (SETQ ßRESULT (and (= PREV '\,) (== (count H) 2) (ISQ H NG) (not (or (ISQ H PRONG) (ISQ (cdr H) PRONG))) (or (NEXTWORD? COMMA) (nil? N))))
        (COND ((nil? ßRESULT) (M CONJOINß) (GO FAIL)))
        (FLUSHME)
        (FQ APPOSITIVE)
        (GO RETSM)
LISTA
        (and LABELTRACE (PASSING 'LISTA))
        (F PREV)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (FQ COMPOUND)
        (and (> (count H) 2) (FQ LIST))
        (COND ((or (CQ NG) (CQ NOUN)) (COND ((CQ AND) (FQ NPL)) (:else (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
            ((CQ VB) (let [COMMON nil] (SETQ COMMON (get 'VB 'ELIM)) (MAP #'(lambda [x] (SETQ COMMON (MEET COMMON (FE x)))) H) nil) (FESET (UNION COMMON (FE C)) C)))
        (SETQ ßRESULT (CALLSM (SMCONJ)))
        (COND (ßRESULT (GO RETURN)) (:else (M CONJOINß) (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

#_(ns shrdlu.init)

(§ defn- QUIETMODE [] (SETQ LABELTRACE nil LABELBREAK nil PLANNERSEE nil))

(§ defn- NORMALFEATUREMODE [] (SETQ DISCOURSE true NOMEM nil TIMID 200))

(§ defn -main [& args]
    ;; LOAD '(PLNR THTRAC)

    (SETQ THGENAME 0
        THSTEP nil
        THSTEPD nil
        THSTEPT nil
        THSTEPF nil
        THXX nil
        THTRACE nil
        THALIST '((nil nil))
        THTREE nil)

    ;; LOAD '(SYSCOM MORPHO SHOW PROGMR GINTER GRAMAR DICTIO SMSPEC SMASS SMUTIL NEWANS BLOCKS DATA)

    (SETQ SAVESENT nil
        LASTSENTNO 0
        SENTNO 1
        LASTIME nil)

    (SETQ ALTMODE (list (ascii 27))
        BREAKCHARS (list (ascii 32) (ascii 13) (ascii 46))
        LINEL 65)

    (LABELTRACE CLAUSE NG VG ADJG PREPG CONJOIN)

    (SETQ LABELTRACE nil
        LABELBREAK nil
        PLANNERSEE nil)

    (SETQ DOIT nil)

    (QUIETMODE)

    (NORMALFEATUREMODE)

    (SETQ NOSTOP true
        ANSWER? true
        SMN nil
        TOPLEVEL-ERRSET? true
        ERT-ERRSET true)

    (TERPRI)
    (SAY YOU ARE NOW IN A READ-EVAL-PRINT LOOP)
    (TERPRI)
    (SAY TYPE "\"GO \"" TO ENTER READY STATE) ;; "sic!

    (CATCH (ert nil) ABORT-PARSER)
    (SHRDLU))


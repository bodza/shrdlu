(ns shrdlu.core)

(defmacro § [& _])
(defmacro ß [& _])

(defmacro def- [s i] `(def ~(vary-meta s assoc :private true) ~i))

(defmacro lambda [& _] `(fn ~@_))

(def- car first)
(def- cdr rest)

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

#_(ns shrdlu.blockp)

;; ################################################################
;;
;;     BLOCKP > MICRO-PLANNER CODE FOR THE "BLOCKS" MICRO-WORLD
;;
;; ################################################################

(§ DEFPROP TA-AT
    (THANTE (X Y) (!AT $?X $?Y) (THRPLACA (cdr (ATAB $?X)) $?Y))
    THEOREM)

(§ DEFPROP TA-CONTAIN
    (THANTE (X Y Z)
        (!AT $?X ?)
        (THGOAL (!MANIP $?X))
        (THGOAL (!SUPPORT $?Y $?X))
        (THOR (THAND (THGOAL (!IS $?Y !BOX)) (THVSETQ $_Z $?Y))
            (THGOAL (!CONTAIN $?Z $?Y)))
        (THASSERT (!CONTAIN $?Z $?X)))
    THEOREM)

(§ DEFPROP TA-EXISTS (THANTE (X) (!EXISTS $?X) (THSUCCEED)) THEOREM)

(§ DEFPROP TA-SUPP
    (THANTE (X Y Z)
        (!AT $?X $?Y)
        (THCOND ((THVSETQ $_Z (SUPPORT $?Y (SIZE $?X) $?X))
            (THCOND ((THGOAL (!MANIP $?Z))
                (THGOAL (!SHAPE $?Z !RECTANGULAR)))
                ((THSUCCEED)))
            (THASSERT (!SUPPORT $?Z $?X))
            (THCOND ((THGOAL (!CLEARTOP $?Z))
                (THERASE (!CLEARTOP $?Z)))
                ((THSUCCEED)))
            (THCOND (NOSTACKS)
                ((THNOT (THGOAL (!MANIP $?Z))))
                ((THAND (THGOAL (!PART $?Z $_Y))
                    (THGOAL (!IS $?Y !STACK)))
                (THASSERT (!PART $?X $?Y)))
                ((THVSETQ $_Y (MAKESYM 'STACK))
                (THASSERT (!PART $?X $?Y))
                (THASSERT (!PART $?Z $?Y))
                (THASSERT (!IS $?Y !STACK))
                (THASSERT (!EXISTS $?Y) (THUSE TA-EXISTS)))))
            ((THGOAL (!GRASPING $?X)))
            ((ERT TA-SUPP))))
    THEOREM)

(§ DEFPROP TC-2
    (THCONSE (X Y YY)
        ($?X $?Y)
        (THGOAL (!CHOOSE $?Y $_YY $E (GET $?X 'CHOOSE))
            (THUSE TC-CHOOSE))
        (THGOAL ($?X $?YY) (THTBF THTRUE)))
    THEOREM)

(§ DEFPROP TC-3
    (THCONSE (X Y Z YY ZZ)
        ($?X $?Y $?Z)
        (THGOAL (!CHOOSE $?Y $_YY $E (GET $?X 'CHOOSE))
            (THUSE TC-CHOOSE))
        (THGOAL (!CHOOSE $?Z $_ZZ $E (GET $?X 'CHOOSE2))
            (THUSE TC-CHOOSE))
        (THGOAL ($?X $?YY $?ZZ) (THTBF THTRUE)))
    THEOREM)

(§ DEFPROP TC-ASMUCH
    (THCONSE (MEASURE X Y)
        (!ASMUCH MEASURE $?X $?Y)
        (THVSETQ $_MEASURE (GET $?MEASURE 'MEASFN))
        (not (< ($?MEASURE $?X) ($?MEASURE $?Y))))
    THEOREM)

(§ DEFPROP TC-BELONG
    (THCONSE (X Y)
        (!BELONG $?X $?Y)
        (THAMONG $?Y '(:SHRDLU))
        (THGOAL (!PHYSOB $?X) (THUSE TC-PHYSOB)))
    THEOREM)

(§ DEFPROP TC-CALL
    (THCONSE (X Y Z)
        (!CALL $?X $?Y)
        (THCOND ((THGOAL (!CALL $_Z $?Y))
            (PRINT $?Y)
            (not (PRINT 'NAME-ALREADY-USED)))
            ((THASSERT (!CALL $?X $?Y))
            (THASSERT (!IS $?Y !NAME))
            (!PROPDEFINE $?Y)
            (or DOIT (SETQ PLAN (cons true PLAN))))))
    THEOREM)

(§ DEFPROP TC-CHOOSE
    (THCONSE (X Y Z W)
        (!CHOOSE $?X $?Y $?Z)
        (THCOND
            ((and (THASVAL $?X) (not (OSS? $?X))) (THSETQ $_Y $?X))
            ((THASVAL $?X)
                (or (nil? DISCOURSE)
                (THPUTPROP (VARIABLE? $?X) $?X 'NG))
                (THSETQ $_Y (FINDCHOOSE $?X $?Z nil)))
        ((THGOAL (!MANIP $?Y)) (THNOT (THGOAL (!SUPPORT $?Y ?))))))
    THEOREM)

(§ DEFPROP TC-CHOOSEPLACE
    (THCONSE (X) (!CHOOSEPLACE $?X) (ERT CHOOSEPLACE UNDEFINED))
    THEOREM)

(§ DEFPROP TC-CLEARTOP
    (THCONSE (X Y (WHY (EV)) EV)
        (!CLEARTOP $?X)
        (ATOM $?X)
        (THOR (THGOAL (!SUPPORT $?X ?))
        (THAND (THASSERT (!CLEARTOP $?X)) (THSUCCEED THEOREM)))
        (MEMORY)
    =>  (THCOND ((THGOAL (!SUPPORT $?X $_Y))
            (THGOAL (!GET-RID-OF $?Y)
                (THNODB)
                (THUSE TC-GET-RID-OF))
            (THGO =>))
            ((THASSERT (!CLEARTOP $?X))
            (MEMOREND (!CLEARTOP $?EV $?X))
            (THSUCCEED THEOREM))))
    THEOREM)

(§ DEFPROP TC-EXISTS (THCONSE (X) (!EXISTS $?X) (THSUCCEED)) THEOREM)

(§ DEFPROP TC-FINDSPACE
    (THCONSE (SURF SIZE OBJ SPACE)
        (!FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE)
        (THOR (and (not (MEMQ $?SURF '(:BOX :TABLE))) (not (GET '!NOCLEAR 'THASSERTION))
            (THSETQ $_SPACE (FINDSPACE 'CENTER $?SURF $?SIZE $?OBJ)))
        (and (or (EQ $?SURF ':BOX) (and (not (EQ $?SURF ':TABLE)) (GET '!NOCLEAR 'THASSERTION)))
            (THSETQ $_SPACE (FINDSPACE 'PACK $?SURF $?SIZE $?OBJ)))
        (THSETQ $_SPACE (FINDSPACE 'RANDOM $?SURF $?SIZE $?OBJ))))
    THEOREM)

(§ DEFPROP TC-GET-RID-OF
    (THCONSE (X Y (WHY (EV)) EV)
        (!GET-RID-OF $?X)
        (or NOMEM (THVSETQ $_EV $?WHY))
    =>  (THCOND ((nil? $?X))
            ((ATOM $?X)
                (MEMORY)
                (THGOAL (!FINDSPACE :TABLE $E (SIZE $?X) $?X $_Y) (THUSE TC-FINDSPACE))
                (THGOAL (!PUT $?X $?Y) (THNODB) (THUSE TC-PUT))
                (MEMOREND (!GET-RID-OF $?EV $?X)))
            ((THGOAL (!GET-RID-OF $E (car $?X)) (THUSE TC-GET-RID-OF))
                (or (THSETQ $_X (cdr $?X)) (THSUCCEED THEOREM))
                (THGO =>))))
    THEOREM)

(§ DEFPROP TC-GRASP
    (THCONSE (X Y (WHY (EV)) EV)
        (!GRASP $?X)
        (THCOND ((THGOAL (!GRASPING $?X)) (THSUCCEED THEOREM))
            ((ATOM $?X)))
        (MEMORY)
        (THGOAL (!CLEARTOP $?X) (THUSE TC-CLEARTOP))
        (THCOND ((THGOAL (!GRASPING $_Y))
            (THOR (THGOAL (!UNGRASP) (THNODB) (THUSE TC-UNGRASP))
                (THGOAL (!GET-RID-OF $?Y) (THNODB) (THUSE TC-GET-RID-OF))))
            ((THSUCCEED)))
        (THSETQ $_Y (TOPCENTER $?X))
        (THGOAL (!MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2))
        (THASSERT (!GRASPING $?X))
        (MEMOREND (!GRASP $?EV $?X))
        (or NOMEM
            (THSETQ GRASPLIST (cons (list THTIME $?X) GRASPLIST)))
        (THCOND (DOIT (THOR (GRASP $?X) (and (UNGRASP) nil)))
            ((THSETQ PLAN (cons (list 'GRASP (list 'quote $?X)) PLAN)))))
    THEOREM)

(§ DEFPROP TC-LOC
    (THCONSE (X Y Z LOC)
        ($?LOC $?X $?Y $?Z)
        (THOR (THGOAL (!MANIP $?Y)) (THGOAL (!IS $?Y !BOX)))
        (THOR (THGOAL (!MANIP $?Z)) (THGOAL (!IS $?Z !BOX)))
        (not (EQ $?Y $?Z))
        (LOCGREATER $?Y $?Z
            ((lambda (X) (COND ((EQ X '!RIGHT) #'car) ((EQ X '!BEHIND) #'cadr) ((EQ X '!ABOVE) #'caddr) ((ERT TC-LOC)))) $?X)))
    THEOREM)

(§ DEFPROP TC-MAKESPACE
    (THCONSE (SURF SIZE OBJ SPACE X (WHY (EV)) EV)
        (!FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE)
        (THNOT (THGOAL (!IS $?SURF !BOX)))
        (MEMORY)
    => (THAND (THGOAL (!SUPPORT $?SURF $_X))
            (THGOAL (!GET-RID-OF $?X) (THUSE TC-GET-RID-OF)))
        (THOR (THGOAL (!FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE) (THUSE TC-FINDSPACE))
            (THGO =>))
        (MEMOREND (!MAKESPACE $?EV $?SURF)))
    THEOREM)

(§ DEFPROP TC-MORE
    (THCONSE (MEASURE X Y)
        (!MORE $?MEASURE $?X $?Y)
        (THVSETQ $_MEASURE (GET $?MEASURE 'MEASFN))
        (> ($?MEASURE $?X) ($?MEASURE $?Y)))
    THEOREM)

(§ DEFPROP TC-MOVEHAND
    (THCONSE (X Y W Z)
        (!MOVEHAND $?Y)
        (THCOND
            ((EQUAL HANDAT $?Y) (THSUCCEED THEOREM))
            ((THGOAL (!GRASPING $?X))
                (THVSETQ $_Z (let [X nil Y nil]
                    (SETQ X (ATAB $?X))
                    (and (CLEAR (SETQ Y (DIFF $?Y (TCENT '(0 0 0) (caddr X))))
                            (list (caaddr X) (cadadr (cdr X)) (- 512 (caddr Y)))
                            (car X))
                        (RETURN Y))
                    nil))
                (THGOAL (!AT $?X $_W))
                (THERASE (!AT $?X $?W) (THUSE TE-SUPP TE-CONTAIN))
                (THASSERT (!AT $?X $?Z) (THUSE TA-AT TA-SUPP TA-CONTAIN))
                (THGOAL (!MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2))
                (or NOMEM
                    (THPUTPROP $?X
                        (cons (list THTIME
                                $?Z
                                (cadar (or (THVAL '(THGOAL (!SUPPORT $?Y $?X))
                                        (cons (list 'Y 'THUNASSIGNED) THALIST))
                                    '((nil :HAND))))
                                nil)
                            (GET $?X 'HISTORY))
                        'HISTORY)))
        ((THGOAL (!MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2)))))
    THEOREM)

(§ DEFPROP TC-MOVEHAND2
    (THCONSE (Y LOC)
        (!MOVEHAND2 $?Y)
        (COND ((EQUAL $?Y HANDAT) (THSUCCEED THEOREM))
            ((and (< 31 (car $?Y) 609) (< -1 (cadr $?Y) 609) (< -1 (caddr $?Y) 513))))
        (THVSETQ $_LOC HANDAT)
        (THSETQ HANDAT $?Y)
        (THSETQ THTIME (inc THTIME))
        (THCOND (DOIT (THOR (eval (cons 'MOVETO HANDAT))
                    (let [ADJUST nil] (eval (cons 'MOVETO $?LOC)) nil)))
            ((THSETQ PLAN (cons (cons 'MOVETO $?Y) PLAN)))))
    THEOREM)

(§ DEFPROP TC-NAME
    (THCONSE (X)
        (!NAME $?X)
        (THVSETQ $_X (LISTIFY $?X))
        (THVSETQ $_X (THFIND ALL $?Y (Y Z) (THAMONG $?Z $?X) (THOR (THGOAL (!CALL $?Z $?Y)) (THSETQ $_Y $?Z))))
        (dorun (map #'PRINT $?X)))
    THEOREM)

(§ DEFPROP TC-NOTICE
    (THCONSE (X)
        (!NOTICE $?X)
        (COND (DOIT (BLINK $?X) (THSUCCEED))
            ((THSETQ PLAN (cons (list 'BLINK (list 'quote $?X)) PLAN)))))
    THEOREM)

(§ DEFPROP TC-ON
    (THCONSE (X Y Z)
        (!ON $?X $?Y)
        (THOR (THGOAL (!SUPPORT $?Y $?X)) (THAND (THASVAL $?X) (THGOAL (!SUPPORT $_Z $?X)) (THGOAL (!ON $?Z $?Y) (THUSE TC-ON)))))
    THEOREM)

(§ DEFPROP TC-PACK
    (THCONSE (OBJ SURF BLOCKS PYR X Y)
        (!PACK $?OBJ $?SURF)
        (or (THVSETQ $_BLOCKS (PACKO $?OBJ '!BLOCK)) true)
        (or (THVSETQ $_PYR (PACKO $?OBJ '!PYRAMID)) true)
    =>  (THCOND ((nil? $?BLOCKS)
            (THCOND ((nil? $?PYR) (THSUCCEED THEOREM))
                ((THVSETQ $_Y (FINDSPACE 'PACK $?SURF (SIZE (car $?PYR)) (car $?PYR)))
                    (THGOAL (!PUT $E (car $?PYR) $?Y) (THUSE TC-PUT))
                    (or (THSETQ $?PYR (cdr $?PYR)) true)
                    (THGO =>))))
                ((THSETQ $_X (car $?BLOCKS))
                    (THVSETQ $?Y (FINDSPACE 'PACK $?SURF (SIZE $?X) $?X))
                    (THGOAL (!PUT $?X $?Y) (THUSE TC-PUT))
                    (or (THSETQ $?BLOCKS (cdr $?BLOCKS)) true)
                    (THCOND ((THVSETQ $_Y (or (PACKON $?X $?PYR) (PACKON $?X $?BLOCKS)))
                            (THGOAL (!PUTON $?Y $?X) (THUSE TC-PUTON))
                            (COND ((MEMQ $?Y $?PYR)
                                    (THSETQ $_PYR (DELQ $?Y (concat $?PYR nil))))
                                ((THSETQ $_BLOCKS (DELQ $?Y (concat $?BLOCKS nil))))))
                        ((THSUCCEED)))
                    (THGO =>))))
    THEOREM)

(§ DEFPROP TC-PART
    (THCONSE (X Y Z)
        (!PART $?X $?Y)
        (THGOAL (!IS $?Y !STACK))
        (THGOAL (!CHOOSE $?X $_Z '(((THGOAL (!PART $?* $?Y))))) (THUSE TC-CHOOSE))
        (or (not (ATOM $?Z)) (THSETQ $_Z (list $?Z)))
    =>  (THCOND ((nil? $?Z) (THSUCCEED))
            ((THGOAL (!PART $E (car $?Z) $?Y))
                (or (THSETQ $_Z (cdr $?Z)) true)
                (THGO =>))
            ((THFAIL))))
    THCONSE)

(§ DEFPROP TC-PHYSOB
    (THCONSE (X)
        (!PHYSOB $?X)
        (THOR (THGOAL (!MANIP $?X)) (THAMONG $?X '(:BOX :TABLE :HAND))))
    THEOREM)

(§ DEFPROP TC-PICKUP
    (THCONSE (X (WHY (EV)) EV)
        (!PICKUP $?X)
        (MEMORY)
        (THGOAL (!GRASP $?X) (THUSE TC-GRASP))
        (THGOAL (!RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
        (MEMOREND (!PICKUP $?EV $?X)))
    THEOREM)

(§ DEFPROP TC-REFERS
    (THCONSE (X)
        (!REFERS $?X)
        (eval (list 'THSETQ (list 'THV $?X) (list 'quote (ATOMIFY (GET $?X 'BIND))))))
    THEOREM)

(§ DEFPROP TC-PUT
    (THCONSE (X Y Z)
        (!PUT $?X $?Y)
        (THCOND ((THASVAL $?Y)
                (THCOND ((ATOM $?Y) (THGOAL (!CHOOSEPLACE $?Y) (THUSE TC-CHOOSEPLACE)))
                    ((THSUCCEED))))
            ((THGOAL (!GET-RID-OF $?X) (THNODB) (THUSE TC-GET-RID-OF))
                (THSUCCEED THEOREM)))
        (CLEAR $?Y (SIZE $?X) $?X)
        (SUPPORT $?Y (SIZE $?X) $?X)
        (THGOAL (!GRASP $?X) (THUSE TC-GRASP))
        (THSETQ $_Z (TCENT $?Y (SIZE $?X)))
        (THGOAL (!MOVEHAND $?Z) (THNODB) (THUSE TC-MOVEHAND))
        (THGOAL (!UNGRASP) (THNODB) (THUSE TC-UNGRASP)))
    THEOREM)

(§ DEFPROP TC-PUTIN
    (THCONSE (X Y Z (WHY (EV)) EV)
        (!PUTIN $?X $?Y)
        (MEMORY)
        (THCOND ((THGOAL (!PUTON $?X $?Y) (THUSE TC-PUTON))
                (MEMOREND (!PUTIN $?EV $?X $?Y))
                (THSUCCEED THEOREM))
            ((THSUCCEED)))
        (THGOAL (!IS $?Y !BOX))
        (THVSETQ $_Z
            (UNION (LISTIFY $?X)
                (THVAL '(THFIND ALL $?W (W) (THGOAL (!ON $?W $?Y))) THALIST)))
        (THGOAL (!CLEARTOP $?Y) (THUSE TC-CLEARTOP))
        (THGOAL (!PACK $?Z $?Y) (THUSE TC-PACK))
        (MEMOREND (!PUTIN $?EV $?X $?Y)))
    THEOREM)

(§ DEFPROP TC-PUTON
    (THCONSE (X Y Z (WHY (EV)) EV)
        (!PUTON $?X $?Y)
        (ATOM $?Y)
        (or (cdr $?X) (THSETQ $_X (car $?X)))
        (not (COND ((ATOM $?X) (EQ $?X $?Y)) ((MEMQ $?Y $?X))))
        (MEMORY)
        (THCOND ((ATOM $?X)
                (THGOAL (!CLEARTOP $?X) (THUSE TC-CLEARTOP))
                (THOR (THGOAL (!FINDSPACE $?Y $E (SIZE $?X) $?X $_Z) (THUSE TC-FINDSPACE))
                    (and (nil? (GET '!NOCLEAR 'THASSERTION))
                        (THGOAL (!FINDSPACE $?Y $E (SIZE $?X) $?X $_Z) (THUSE TC-MAKESPACE))))
                (THGOAL (!PUT $?X $?Z) (THNODB) (THUSE TC-PUT)))
            ((THASSERT (!NOCLEAR))
                (THPROG ((W $?X))
                =>  (THOR (THGOAL (!PUTON $E (car $?W) $?Y) (THUSE TC-PUTON))
                        (THFAIL THPROG))
                    (THOR (THSETQ $?W (cdr $?W)) (THRETURN true))
                    (THGO =>))
            (THERASE (!NOCLEAR)))
            ((THNOT (THGOAL (!IS $?Y !BOX)))
                (THGOAL (!CLEARTOP $?Y) (THUSE TC-CLEARTOP))
                (THGOAL (!PACK $?X $?Y) (THUSE TC-PACK))))
        (MEMOREND (!PUTON $?EV $?X $?Y)))
    THEOREM)

(§ DEFPROP TC-RAISEHAND
    (THCONSE ((WHY (EV)) EV)
        (!RAISEHAND)
        (MEMORY)
        (THGOAL (!MOVEHAND $E (list (car HANDAT) (cadr HANDAT) 512)) (THNODB) (THUSE TC-MOVEHAND))
        (MEMOREND (!RAISEHAND $?EV)))
    THEOREM)

(§ DEFPROP TC-STACK
    (THCONSE (X Y)
        (!IS $?X !STACK)
        (not (THASVAL $?X))
        (THGOAL (!MANIP $?Y))
        (THGOAL (!SUPPORT $?Y ?))
        (THNOT (THAND (THGOAL (!PART $?Y $_X))
            (THGOAL (!IS $?X !STACK))))
    =>  (THGOAL (!SUPPORT $_X $?Y))
        (THCOND ((MEMQ $?X '(:TABLE :BOX)))
            ((THSETQ $_Y $?X) (THGO =>)))
        (THSETQ $_X (MAKESYM 'STACK))
        (THASSERT (!IS $?X !STACK))
        (THASSERT (!EXISTS $?X))
        (THFIND ALL
            $?Z
            (Z)
            (THGOAL (!ON $?Z $?Y) (THUSE TC-ON))
            (THAND (THASSERT (!PART $?Z $?X)) (THFINALIZE THAND))))
    THEOREM)

(§ DEFPROP TC-STACKUP
    (THCONSE (X Y BLOCKS PYR (WHY (EV)) EV)
        (!STACKUP $?X)
        (or (< (reduce #'+ (map #'(lambda (X) (caddr (SIZE X))) $?X)) 641)
            (not (DPRINT2 "TOO HIGH,")))
        (THCOND
            ((and $?X (cdr $?X)))
            ((THSETQ $_X
                (concat $?X
                    (THVAL (list 'THFIND
                        (COND ((nil? $?X) 3) (2))
                        '$?Y
                        '(Y)
                        '(THOR (THAND (THGOAL (!IS $?Y !BLOCK)) (THNOT (THGOAL (!SUPPORT $?Y ?)))) (THGOAL (!IS $?Y !BLOCK)))
                        '(not (EQ $?X $?Y)))
                        THALIST)))))
        (COND ((THVSETQ $_PYR (PACKO $?X '!PYRAMID)) (nil? (cdr $?PYR))) (:else true))
        (THVSETQ $_BLOCKS (cons ':TABLE (PACKO $?X '!BLOCK)))
        (MEMORY)
    =>  (THCOND
            ((cdr $?BLOCKS)
                (THGOAL (!PUTON $E (cadr $?BLOCKS) $E (car $?BLOCKS)) (THUSE TC-PUTON))
                (THSETQ $_BLOCKS (cdr $?BLOCKS))
                (THGO =>))
            ($?PYR (THGOAL (!PUTON $E (car $?PYR) $E (car $?BLOCKS)) (THUSE TC-PUTON)))
            ((MEMOREND (!STACKUP $?EV $?X)))))
    THEOREM)

(§ DEFPROP TC-STARTEND3
    (THCONSE (X Y EV TIME) ($?X $?EV $?TIME) (THGOAL ($?X $?Y $?EV $?TIME) (THUSE TC-STARTEND4)))
    THEOREM)

(§ DEFPROP TC-STARTEND4
    (THCONSE (X NEWEV Z EV TIME)
        ($?X $?NEWEV $?EV $?TIME)
        (or (and (THASVAL $?X) (THASVAL $?EV) (THASVAL $?TIME) (not (THASVAL $?NEWEV))) (ERT TC-STARTEND4))
        (THGOAL (!CHOOSE $?EV $_Z nil) (THUSE TC-CHOOSE))
        (or (ATOM $?Z) (ERT TC-STARTEND4 ATOM))
        (THSETQ $_NEWEV (MAKESYM 'EV))
        (PUTPROP $?NEWEV
            (PUTPROP $?NEWEV
                (GET $?Z (COND ((EQ $?X '!START) 'START) ((EQ $?X '!END) 'END) ((ERT TC-STARTEND (THV X)))))
                'START)
            'END)
        (TIMECHK $?NEWEV $?TIME)
        (PUTPROP $?NEWEV $?Z 'WHY)
        (PUTPROP $?NEWEV '!START 'TYPE))
    THEOREM)

(§ DEFPROP TC-UNGRASP
    (THCONSE (X OBJ (WHO (EV)) EV)
        (!UNGRASP)
        (THCOND ((THGOAL (!GRASPING $?X))
                (MEMORY)
                (THGOAL (!SUPPORT ? $?X))
                (THERASE (!GRASPING $?X))
                (MEMOREND (!UNGRASP $?EV $?X))
                (THSETQ THTIME (inc THTIME))
                (THCOND (DOIT (THOR (UNGRASP) (and (GRASP $?X) nil)))
                    ((THSETQ PLAN (cons '(UNGRASP) PLAN)))))
            ((THSUCCEED))))
    THEOREM)

(§ DEFPROP TC-WANT4
    (THCONSE (X EV TIME Y)
        (!WANT $?X $?EV $?TIME)
        (THGOAL (!WANT $?Y $?X $?EV $?TIME) (THUSE TC-WANT5)))
    THEOREM)

(§ DEFPROP TC-WANT5
    (THCONSE (X NEWEV EV TIME Z)
        (!WANT $?NEWEV $?X $?EV $?TIME)
        (or (and (THASVAL $?X) (THASVAL $?EV) (THASVAL $?TIME)) (ERT TC-WANT5 THASVAL))
        (EQ $?X ':FRIEND)
        (EQ (GET $?EV 'WHY) 'COMMAND)
        (THSETQ $_NEWEV (MAKESYM 'EV))
        (PUTPROP $?NEWEV
            (PUTPROP $?NEWEV
                (GET $?EV 'START)
                'START)
            'END)
        (TIMECHK $?NEWEV $?TIME)
        (PUTPROP $?NEWEV '!TELL 'TYPE)
        (PUTPROP $?NEWEV 'ESP 'WHY))
    THEOREM)

(§ DEFPROP TCT-EXISTS (THCONSE nil (!EXISTS ? ?) (THSUCCEED)) THEOREM)

(§ DEFPROP TCT-PICKUP
    (THCONSE (X EV TIME)
        (!PICKUP $?X $?TIME)
        (THOR (THAND (THGOAL (!PICKUP$?EV $?X)) (TIMECHK $?EV $?TIME))
            (THGOAL (!PICKUP $?EV $?X $?TIME) (THUSE TCTE-PICKUP))))
    THEOREM)

(§ DEFPROP TCT-PUT
    (THCONSE (X EV TIME Y)
        (!PUT $?X $?Y $?TIME)
        (THGOAL (!PUT $?EV $?X $?Y $?TIME) (THUSE TCTE-PUT)))
    THEOREM)

(§ DEFPROP TCT-AT
    (THCONSE (X Y Z TIME W)
        (!AT $?Y $?Z $?TIME)
        (THOR (THGOAL (!MANIP $?Y))
            (THAND (THGOAL (!IS $?Y !BOX)) (THGOAL (!AT $?Y $?Z)) (THSUCCEED THEOREM)))
        (THSETQ $_X (TFIND $?Y $?TIME))
        (THOR (THSETQ $_W (car $?X))
            (THAND (THAMONG $?W (cdr $?X)) (or (not (< (car $?W) (or (START? $?TIME) -1))) (THFAIL THAND))))
        (THSETQ $?Z (cadr $?W)))
    THEOREM)

(§ DEFPROP TCT-LOC
    (THCONSE (YY ZZ X Y Z TIME)
        (!LOC $?X $?Y $?Z $?TIME)
        (THGOAL (!AT $?Y $?YY $?TIME) (THUSE TCT-AT))
        (THGOAL (!AT $?Z $?ZZ $?TIME) (THUSE TCT-AT))
        (THGOAL (!TLOC $?X $?Y $?Z) (THUSE TC-LOC)))
    THEOREM)

(§ DEFPROP TCT-SUPPORT
    (THCONSE (X Y Z TIME)
        (!SUPPORT $?X $?Y $?TIME)
        (THOR (THGOAL (!MANIP $?Y)) (THGOAL (!IS $?Y !BOX)))
        (THAMONG $?Z (TFIND $?Y $?TIME))
        (not (< (car $?Z) (or (START? $?TIME) -1)))
        (THAMONG $?X (list (caddr $?Z))))
    THEOREM)

(§ DEFPROP TCT-2
    (THCONSE (X EV TIME) ($?X $?TIME) (THGOAL ($?X $?EV $?TIME) (THUSE TCTE-3)))
    THEOREM)

(§ DEFPROP TCT-3
    (THCONSE (X Y EV TIME) ($?X $?Y $?TIME) (THGOAL ($?X $?EV $?Y $?TIME) (THUSE TCTE-4)))
    THEOREM)

(§ DEFPROP TCT-4
    (THCONSE (X Y Z EV TIME) ($?X $?Y $?Z $?TIME) (THGOAL ($?X $?EV $?Y $?Z $?TIME) (THUSE TCTE-5)))
    THEOREM)

(§ DEFPROP TCTE-PICKUP
    (THCONSE (X EV EVENT TIME)
        (!PICKUP $?EV $?X $?TIME)
        (THOR (THAND (THGOAL (!PICKUP $?EV $?X)) (TIMECHK $?EV $?TIME) (THSUCCEED THEOREM))
            (THSUCCEED))
        (THAMONG $?EVENT EVENTLIST)
        (MEMQ (GET $?EVENT 'TYPE) '(!PUTON !GET-RID-OF))
        (TIMECHK $?EVENT $?TIME)
        (THOR (THGOAL (!PUTON $?EVENT $?X ?))
            (THGOAL (!GET-RID-OF $?EVENT $?X)))
        (THVSETQ $_EV (MAKESYM 'E))
        (and (PUTPROP $?EV (PUTPROP $?EV (GET $?EVENT 'END) 'START) 'END)
            (PUTPROP $?EV '!PICKUP 'TYPE)
            (PUTPROP $?EV $?EVENT 'WHY)
            (SETQ EVENTLIST (cons $?EV EVENTLIST))
            (THASSERT (!PICKUP $?EV $?X))))
    THEOREM)

(§ DEFPROP TCTE-PUT
    (THCONSE (X Y EV EVENT TIME Z)
        (!PUT $?EV $?X $?Y $?TIME)
        (THAMONG $?EVENT EVENTLIST)
        (MEMQ (GET $?EVENT 'TYPE) '(!PICKUP !PUTON))
        (TIMECHK $?EVENT $?TIME)
        (THOR (THGOAL (!PUTON $?EVENT $?X ?))
            (THGOAL (!PICKUP $?EVENT $?X)))
        (or (THVSETQ $_Z (dec (ASSQ (GET $?EVENT 'END) (GET $?X 'HISTORY))))
            (ERT TCTE-PUT WRONG))
        (THAMONG $?Y (list (cadr $?Z)))
        (THSETQ $_EV (MAKESYM 'E))
        (and (PUTPROP $?EV (PUTPROP $?EV (car $?Z) 'START) 'END)
            (PUTPROP $?EV $?EVENT 'WHY)
            (PUTPROP $?EV '!PUT 'TYPE)
            (SETQ EVENTLIST (cons $?EV EVENTLIST))
            (THASSERT (!PUT $?EV $?X $?Y))))
    THEOREM)

(§ DEFPROP TCTE-3
    (THCONSE (X EV TIME)
        ($?X $?EV $?TIME)
        (or (THASVAL TIME) (ERT TCTE-3))
        (THGOAL ($?X $?EV))
        (TIMECHK $?EV $?TIME))
    THEOREM)

(§ DEFPROP TCTE-4
    (THCONSE (X Y EV TIME)
        ($?X $?EV $?Y $?TIME)
        (or (THASVAL $?TIME) (ERT TCTE-4))
        (THGOAL ($?X $?EV $?Y))
        (TIMECHK $?EV $?TIME))
    THEOREM)

(§ DEFPROP TCTE-5
    (THCONSE (X Y Z EV TIME)
        ($?X $?EV $?Y $?Z $?TIME)
        (or (THASVAL $?TIME) (ERT TCT-5))
        (THGOAL ($?X $?EV $?Y $?Z))
        (TIMECHK $?EV $?TIME))
    THEOREM)

(§ DEFPROP TCT-GRASP
    (THCONSE (X Z TIME)
        (!GRASP $?X $?TIME)
        (THVSETQ $_Z (ENDTIME GRASPLIST $?TIME))
    =>  (THCOND ((or (nil? $?Z) (STARTIME $?Z $?TIME)) (THFAIL))
            ((or (and (not (THASVAL $?X)) (THSETQ $_X (cadar $?Z))) (EQ $?X (cadar $?Z))))
            ((THSETQ $_Z (cdr $?Z)) (THGO =>))
            ((THFAIL))))
    THEOREM)

(§ DEFPROP TE-CONTAIN
    (THERASING (X Y)
        (!AT $?X ?)
        (THGOAL (!CONTAIN $_Y $?X))
        (THERASE (!CONTAIN $?Y $?X)))
    THEOREM)

(§ DEFPROP TE-EXISTS (THERASING (X) (!EXISTS $?X) (THSUCCEED)) THEOREM)

(§ DEFPROP TE-SUPP
    (THERASING (X Y Z)
        (!AT $?X ?)
        (THCOND ((THGOAL (!SUPPORT $?X $_Y)) (ERT TE-SUPP))
            ((THGOAL (!SUPPORT $_Y $?X))
                (THERASE (!SUPPORT $?Y $?X))
                (THCOND
                    ((THGOAL (!PART $?X $_Y))
                        (THERASE (!PART $?X $?Y))
                        (THCOND ((THFIND 2 $?W (W) (THGOAL (!PART $?W $?Y)))
                                (THSUCCEED THEOREM))
                            ((THGOAL (!PART $_Z $?Y))
                                (THERASE (!PART $?Z $?Y)))
                            ((THSUCCEED)))
                        (THERASE (!EXISTS $?Y) (THUSE TE-EXISTS)))
                    ((THSUCCEED))))))
    THEOREM)

(§ DEFUN TOPCENTER [X] ((lambda (X) (TCENT (cadr X) (caddr X))) (ATAB X)))

(§ SETQ DOIT nil)

(§ SETQ NOSTACKS true)

(§ DEFUN SASSQ [X Y Z] (or (ASSQ X Y) (APPLY Z nil)))

(§ DEFPROP !CLEARTOP (((THGOAL (!SUPPORT $?* ?)))) CHOOSE)

(§ DEFPROP !GRASP
    (((THNOT (THGOAL (!GRASPING $?*))) (THGOAL (!CLEARTOP $?*)))
     ((THNOT (THGOAL (!GRASPING $?*)))))
    CHOOSE)

(§ DEFPROP !PICKUP
    (((THGOAL (!SUPPORT ? $?*)) (THGOAL (!CLEARTOP $?*)))
     ((THGOAL (!SUPPORT ? $?*))))
    CHOOSE)

(§ DEFPROP !PUTIN
    (((THNOT (THGOAL (!CONTAIN :BOX $?*))) (THGOAL (!CLEARTOP $?*)))
     ((THNOT (THGOAL (!CONTAIN :BOX $?*)))))
    CHOOSE)

(§ DEFPROP !PUTIN (((THGOAL (!IS $?* !BOX)))) CHOOSE2)

(§ DEFPROP !PUTON (((THGOAL (!CLEARTOP $?*)))) CHOOSE)

(§ DEFPROP !PUTON
    (((THGOAL (!CLEARTOP $?*)) (THNOT (THGOAL (!IS $?* !PYRAMID))))
     ((THNOT (THGOAL (!IS $?* !PYRAMID)))))
    CHOOSE2)

(§ DEFPROP !STACKUP
    (((THGOAL (!CLEARTOP $?*)) (THNOT (THGOAL (!IS $?* !PYRAMID))))
     ((THNOT (THGOAL (!IS $?* !PYRAMID)))))
    CHOOSE)

(§ THDATA)
    (ß TC-CALL)
    (ß TC-CLEARTOP)
    (ß TC-GET-RID-OF)
    (ß TC-GRASP)
    (ß TC-NAME)
    (ß TC-NOTICE)
    (ß TC-PACK)
    (ß TC-PICKUP)
    (ß TC-PUTIN)
    (ß TC-PUTON)
    (ß TC-RAISEHAND)
    (ß TC-STACKUP)
    (ß TC-UNGRASP)
    (ß TC-ON)
    (ß TC-PHYSOB)
nil

(§ DEFUN UNION [A B]
=>  (COND ((nil? A) (RETURN B))
        ((MEMQ (car A) B))
        ((SETQ B (cons (car A) B))))
    (SETQ A (cdr A))
    (GO =>))

#_(ns shrdlu.blockl)

;; ################################################################
;;
;;            BLOCKL - LISP CODE FOR THE BLOCKS WORLD
;;
;; ################################################################

(§ DEFUN ABSVAL [X] (COND ((neg? X) (- X)) (X)))

(§ DEFUN ATAB [X] (or (ASSQ X ATABLE) (ERT ATABLE)))

(§ DEFUN CLEAR [LOC SIZE OBJ]
    (let [W nil X1 nil X2 nil]
        (SETQ OBJ (LISTIFY OBJ))
        (and (MEMQ nil (doall (map #'(lambda (X Y) (and (> X -1) (> 641 (+ X Y)) true)) LOC SIZE)))
            (RETURN nil))
        (SETQ W ATABLE)
    =>  (COND ((nil? W) (RETURN LOC))
            ((MEMQ (caar W) OBJ))
            ((and (< (car LOC) (+ (car (SETQ X1 (cadar W))) (car (SETQ X2 (caddar W)))))
                (< (car X1) (+ (car LOC) (car SIZE)))
                (< (cadr LOC) (+ (cadr X1) (cadr X2)))
                (< (cadr X1) (+ (cadr LOC) (cadr SIZE)))
                (< (caddr LOC) (+ (caddr X1) (caddr X2)))
                (< (caddr X1) (+ (caddr LOC) (caddr SIZE))))
            (RETURN nil)))
        (SETQ W (cdr W))
        (GO =>)))

(§ DEFUN DIFF [X Y] (doall (map #'- X Y)))

(§ DEFUN DIV2 [X] (/ X 2))

(§ DEFUN ENDTIME [LIST TIME]
    (let [Y nil]
        (or (SETQ Y (END? TIME)) (RETURN LIST))
    =>  (COND ((nil? LIST) (RETURN nil))
            ((not (> (caar LIST) Y)) (RETURN LIST))
            ((SETQ LIST (cdr LIST)) (GO =>)))))

(§ DEFUN EV [] (or NOMEM $?EV))

(§ DEFUN FINDSPACE
    (TYPE SURF SIZE OBJ)
    (let [XYMAX nil XYMIN nil N nil V nil X1 nil X2 nil]
        (SETQ OBJ (LISTIFY OBJ))
        (and (MEMQ SURF OBJ) (RETURN nil))
        (COND ((EQ SURF ':TABLE)
                (SETQ XYMIN '(0 0))
                (SETQ XYMAX '(640 640))
                (SETQ LEVEL 0)
                (GO ON))
            ((SETQ X (ATAB SURF))))
        (COND
            ((EQ TYPE 'CENTER)
                (COND ((CLEAR (SETQ V
                        (list (max 0 (+ (caadr X) (DIV2 (- (caaddr X) (car SIZE)))))
                            (max 0 (+ (cadadr X) (DIV2 (- (cadr (caddr X)) (cadr SIZE)))))
                            (+ (caddr (cadr X)) (caddr (caddr X)))))
                        SIZE
                        OBJ)
                    (RETURN V))
                ((RETURN nil))))
            ((EQ (car X) ':BOX)
                (SETQ XYMIN (list (caadr X) (cadadr X)))
                (SETQ XYMAX (list (+ (caaddr X) (caadr X)) (+ (cadr (caddr X)) (cadadr X))))
                (SETQ LEVEL 1))
            ((SETQ X1 (DIV2 (car SIZE)))
                (SETQ Y1 (DIV2 (cadr SIZE)))
                (SETQ XYMAX (list (min 640 (dec (+ (caaddr X) (caadr X) X1))) (min 640 (dec (+ (cadr (caddr X)) (cadadr X) Y1)))))
                (SETQ XYMIN (list (max 0 (- (caadr X) X1)) (max 0 (- (cadadr X) Y1))))
                (SETQ LEVEL (+ (caddr (cadr X)) (caddr (caddr X))))))
    ON  (SETQ N 8)
        (SETQ X1 (- (car XYMAX) (car XYMIN)))
        (SETQ Y1 (- (cadr XYMAX) (cadr XYMIN)))
    GO  (COND ((zero? (SETQ N (dec N))) (RETURN nil))
            ((or (not (SETQ V
                        (GROW (list
                                (+ (car XYMIN) (rem (ABSVAL (RANDOM)) X1))
                                (+ (cadr XYMIN) (rem (ABSVAL (RANDOM)) Y1))
                                LEVEL)
                            XYMIN XYMAX OBJ)))
                    (< (- (caadr V) (caar V)) (car SIZE))
                    (< (- (cadadr V) (cadar V)) (cadr SIZE)))
                (GO GO))
            ((RETURN (COND
                ((EQ TYPE 'RANDOM)
                    (list (DIV2 (- (+ (caar V) (caadr V)) (car SIZE)))
                        (DIV2 (- (+ (cadar V) (cadadr V)) (cadr SIZE)))
                        LEVEL))
                ((EQ TYPE 'PACK)
                    (list (caar V) (cadar V) LEVEL))
                ((ERT FINDSPACE "--" TYPE))))))))

(§ DEFUN GOAL FEXPR [X]
    (SETQ PLAN nil)
    (THVAL (list 'THGOAL (car X) '(THTBF THTRUE)) '((EV COMMAND)))
    (EVLIS (reverse PLAN)))

(§ DEFUN GROW [LOC MIN MAX OBJ]
    (let [GROW nil XL nil XH nil XO nil YL nil YH nil YO nil]
        (SETQ OBJ (LISTIFY OBJ))
        (COND
            ((or
                (neg? (caar (SETQ XL (list (list (- (car LOC) (car MIN)) nil)))))
                (neg? (caar (SETQ XH (list (list (- (car MAX) (car LOC)) nil)))))
                (neg? (caar (SETQ YL (list (list (- (cadr LOC) (cadr MIN)) nil)))))
                (neg? (caar (SETQ YH (list (list (- (cadr MAX) (cadr LOC)) nil)))))
                (nil? (ERRSET
                    (dorun (map #'(lambda (X)
                        (let [XX nil YY nil]
                            (COND ((or (MEMQ (car X) OBJ)
                                    (not (< (caadr X) (car MAX)))
                                    (not (< (cadadr X) (cadr MAX)))
                                    (not (> (SETQ XX (+ (caadr X) (caaddr X))) (car MIN)))
                                    (not (> (SETQ YY (+ (cadadr X) (cadr (caddr X)))) (cadr MIN)))
                                    (not (> (+ (caddr (cadr X)) (caddr (caddr X))) (caddr LOC))))
                                (RETURN nil))
                            ((> (caadr X) (car LOC))
                                (SETQ XH (ORDER (list (- (caadr X) (car LOC)) (car X)) XH)))
                            ((< XX (car LOC))
                                (SETQ XL (ORDER (list (- (car LOC) XX) (car X)) XL)))
                            ((SETQ XO (cons (car X) XO))))
                            (COND ((> (cadadr X) (cadr LOC))
                                (SETQ YH (ORDER (list (- (cadadr X) (cadr LOC)) (car X)) YH)))
                            ((< YY (cadr LOC))
                                (SETQ YL (ORDER (list (- (cadr LOC) YY) (car X)) YL)))
                            ((MEMQ (car X) XO) (ERR nil))
                            ((SETQ YO (cons (car X) YO))))
                            nil))
                    ATABLE)))))
                (RETURN nil)))
    =>   (COND ((== (SETQ GROW (min (caar XL) (caar XH) (caar YL) (caar YH))) 1024)
            (RETURN (list
                (list (- (car LOC) (cadar XL)) (- (cadr LOC) (cadar YL)))
                (list (+ (car LOC) (cadar XH)) (+ (cadr LOC) (cadar YH))))))
            ((dorun (map #'(lambda (Y Z W) (let [X nil]
                        (SETQ X (eval W))
                        (COND ((> (caar X) GROW))
                            ((or (nil? (cadar X)) (MEMQ (cadar X) (eval Y)))
                                (RPLACA X (list 2000 (caar X))))
                            ((SET Z (cons (cadar X) (eval Z)))
                                (SET W (cdr X))))
                        nil))
                    '(YO YO XO XO)
                    '(XO XO YO YO)
                    '(XL XH YL YH)))
                (GO =>)))))

(§ DEFUN LISTIFY [X] (COND ((ATOM X) (list X)) (X)))

(§ DEFUN LOCGREATER [X Y FN]
    ((lambda (XX YY) (not (< (FN (cadr XX)) (+ (FN (cadr YY)) (FN (caddr YY))))))
        (LOCG2 '$?YY X)
        (LOCG2 '$?ZZ Y)))

(§ DEFUN LOCG2 [X Y] (COND ((EQ $?LOC '!LOC) (ATAB Y)) ((cons nil (cons (eval X) (cddr (ATAB Y)))))))

(§ DEFUN MEMOREND FEXPR [A]
    (or NOMEM
        (and (PUTPROP $?EV THTIME 'END)
            (APPLY #'THASSERT (list (THVARSUBST (car A) nil)))
            (PUTPROP $?EV (caar A) 'TYPE))))

(§ DEFUN MEMORY []
    (or NOMEM
        (THAND (THVSETQ $_EV (MAKESYM 'E))
            (THSETQ EVENTLIST (cons $?EV EVENTLIST))
            (PUTPROP $?EV THTIME 'START)
            (PUTPROP $?EV $?WHY 'WHY))))

(§ DEFUN OCCUPIER [X Y Z]
    (let [W nil X1 nil X2 nil]
        (COND ((neg? Z) (RETURN ':TABLE)))
        (SETQ W ATABLE)
    =>  (COND ((nil? W) (RETURN nil))
            ((and (< (dec (car (SETQ X1 (cadar W)))) X (+ (car X1) (car (SETQ X2 (caddar W)))))
                (< (dec (cadr X1)) Y (+ (cadr X1) (cadr X2)))
                (< (dec (caddr X1)) Z (+ (caddr X1) (caddr X2))))
            (RETURN (caar W))))
        (SETQ W (cdr W))
        (GO =>)))

(§ DEFUN ORDER [X Y]
    (COND ((nil? Y) (list X))
        ((> (car X) (caar Y))
            (cons (car Y) (ORDER X (cdr Y))))
        ((cons X Y))))

(§ DEFUN PACKO [OBJ TYPE]
    (let [XX nil]
        (dorun (map #'(lambda (X)
            (and (THVAL '(THGOAL (!IS $?X $E TYPE)) (list (list 'X X))) (SETQ XX (PACKORD X (SIZE X) XX))))
        OBJ))
        (RETURN (doall (map #'cadr XX)))))

(§ DEFUN PACKON [SURF LIST]
    (let [X nil]
        (SETQ SURF (ATAB SURF))
    =>  (COND ((nil? LIST) (RETURN nil))
            ((or (> (car (SETQ X (SIZE (car LIST)))) (caaddr SURF))
                    (> (cadr X) (cadr (caddr SURF)))
                    (> (+ (caddr X) (caddr (cadr SURF)) (caddr (caddr SURF))) 321))
                (SETQ LIST (cdr LIST))
                (GO =>))
            ((RETURN (car X))))))

(§ DEFUN PACKORD [X SIZE LIST]
    (COND ((nil? LIST) (list (list SIZE X)))
        ((or (> (caaar LIST) (car SIZE))
            (and (EQ (car SIZE) (caaar LIST)) (> (cadaar LIST) (cadr SIZE))))
        (cons (car LIST) (PACKORD X SIZE (cdr LIST))))
        ((cons (list SIZE X) LIST))))

(§ DEFUN SIZE [X]
    (COND ((EQ X ':BOX) '(256 256 192))
        ((EQ X ':TABLE) '(640 640 640))
        ((ATOM X) (caddr (ATAB X)))
        (X)))

(§ DEFUN STARTHISTORY []
    (SETQ THTIME 0)
    (SETQ GRASPLIST nil)
    (DEFPROP EE COMMAND WHY)
    (DEFPROP EE 0 START)
    (DEFPROP EE 0 END)
    (DEFPROP EE !START TYPE)
    (SETQ EVENTLIST '(EE))
    (THADD '(!START EE :DIALOG) nil)
    (ERRSET (CLEANOUT E) nil)
    (dorun (map #'(lambda (X)
        (and (GET (car X) 'THASSERTION)
        (PUTPROP (car X)
            (list (list 0
                    (cadr X)
                    (cadar (THVAL '(THGOAL (!SUPPORT $?X $?Y)) (list (list 'X 'THUNASSIGNED) (list 'Y (car X)))))))
            'HISTORY)))
    ATABLE)))

(§ DEFUN STARTIME [LIST TIME] (< (caar LIST) (or (START? TIME) -1)))

(§ DEFUN SUPPORT [LOC SIZE X]
    (COND ((EQ (caddr LOC) 0) ':TABLE)
        ((SETQ LOC (OCCUPIER (+ (car LOC) (DIV2 (car SIZE))) (+ (cadr LOC) (DIV2 (cadr SIZE))) (dec (caddr LOC))))
            (COND ((EQ LOC X) nil) (LOC)))))

(§ DEFUN TCENT [X1 X2]
    (list (+ (car X1) (DIV2 (car X2))) (+ (cadr X1) (DIV2 (cadr X2))) (+ (caddr X1) (caddr X2))))

(§ DEFUN TFIND [X Y]
    (let [Z nil]
        (or (SETQ Z (GET X 'HISTORY)) (RETURN nil))
    =>  (COND ((not (> (caar Z) (or (END? Y) 32767)))
                (RETURN Z))
            ((SETQ Z (cdr Z)) (GO =>)))))

(§ DEFUN TIMECHK [EV TIME]
    (COND ((IMPERF? TIME)
        (not (or (< (GET EV 'END) (or (START? TIME) -1))
            (< (or (END? TIME) 262143)
                (GET EV 'START)))))
        ((not (or (< (GET EV 'START) (or (START? TIME) -1))
            (< (or (END? TIME) 262143)
                (GET EV 'END)))))))

#_(ns shrdlu.data)

;; ####################################################################
;;
;;      DATA > INITIAL MICROPLANNER DATA BASE FOR THE BLOCKS WORLD
;;
;; ####################################################################

(§ THFLUSH THASSERTION)

(§ SETQ ATABLE
  '((:B1 (72 64 0) (64 64 64))
    (:B2 (72 64 64) (64 64 64))
    (:B3 (256 0 0) (128 128 128))
    (:B4 (416 416 1) (128 128 128))
    (:B5 (320 64 128) (64 64 192))
    (:B6 (0 192 0) (128 192 192))
    (:B7 (0 160 192) (128 128 128))
    (:B10 (192 416 0) (128 64 256))
    (:BW1 (376 376 0) (8 256 192))
    (:BW2 (376 376 0) (256 8 192))
    (:BW3 (376 640 0) (256 8 192))
    (:BW4 (640 376 0) (8 256 192))
    (:BOX (384 384 0) (256 256 1))))

(§ SETQ DISPLAY-AS
  '((:B1 !DISPLAY !BLOCK (72 64 0) (64 64 64) RED)
    (:B2 !DISPLAY !PYRAMID (72 64 64) (64 64 64) GREEN)
    (:B3 !DISPLAY !BLOCK (256 0 0) (128 128 128) GREEN)
    (:B4 !DISPLAY !PYRAMID (416 416 1) (128 128 128) BLUE)
    (:B5 !DISPLAY !PYRAMID (320 64 128) (64 64 192) RED)
    (:B6 !DISPLAY !BLOCK (0 192 0) (128 192 192) RED)
    (:B7 !DISPLAY !BLOCK (0 160 192) (128 128 128) GREEN)
    (:B10 !DISPLAY !BLOCK (192 416 0) (128 64 256) BLUE)
    (:HAND !DISPLAY !HAND (32 0 0) (0 0 0) WHITE)
    (:TABLE !DISPLAY !TABLE (0 0 0) (512 512 0) BLACK)
    (:BOX !DISPLAY !BOX (384 384 0) (254 254 192) WHITE)))

(§ THDATA)
    (ß (!IS :B1 !BLOCK))
    (ß (!IS :B2 !PYRAMID))
    (ß (!IS :B3 !BLOCK))
    (ß (!IS :B4 !PYRAMID))
    (ß (!IS :B5 !PYRAMID))
    (ß (!IS :B6 !BLOCK))
    (ß (!IS :B7 !BLOCK))
    (ß (!IS :B10 !BLOCK))

    (ß (!IS !RED !COLOR))
    (ß (!IS !BLUE !COLOR))
    (ß (!IS !GREEN !COLOR))
    (ß (!IS !WHITE !COLOR))
    (ß (!IS !BLACK !COLOR))

    (ß (!IS !RECTANGULAR !SHAPE))
    (ß (!IS !ROUND !SHAPE))
    (ß (!IS !POINTED !SHAPE))

    (ß (!IS :SHRDLU !ROBOT))
    (ß (!IS :FRIEND !PERSON))
    (ß (!IS :HAND !HAND))

    (ß (!AT :B1 (64 64 0)))
    (ß (!AT :B2 (64 64 64)))
    (ß (!AT :B3 (256 0 0)))
    (ß (!AT :B4 (416 416 1)))
    (ß (!AT :B5 (320 64 128)))
    (ß (!AT :B6 (0 192 0)))
    (ß (!AT :B7 (0 160 192)))
    (ß (!AT :B10 (192 416 0)))

    (ß (!SUPPORT :B1 :B2))
    (ß (!SUPPORT :B3 :B5))
    (ß (!SUPPORT :B6 :B7))

    (ß (!CLEARTOP :B2))
    (ß (!CLEARTOP :B4))
    (ß (!CLEARTOP :B5))
    (ß (!CLEARTOP :B7))
    (ß (!CLEARTOP :B10))

    (ß (!MANIP :B1))
    (ß (!MANIP :B2))
    (ß (!MANIP :B3))
    (ß (!MANIP :B4))
    (ß (!MANIP :B5))
    (ß (!MANIP :B6))
    (ß (!MANIP :B7))
    (ß (!MANIP :B10))

    (ß (!SUPPORT :TABLE :B1))
    (ß (!SUPPORT :TABLE :B3))
    (ß (!SUPPORT :BOX :B4))
    (ß (!SUPPORT :TABLE :B10))
    (ß (!SUPPORT :TABLE :B6))
    (ß (!SUPPORT :TABLE :BOX))

    (ß (!AT :BOX (384 384 0)))
    (ß (!IS :BOX !BOX))
    (ß (!IS :TABLE !TABLE))
    (ß (!CONTAIN :BOX :B4))

    (ß (!SHAPE :B1 !RECTANGULAR))
    (ß (!SHAPE :B3 !RECTANGULAR))
    (ß (!SHAPE :B2 !POINTED))
    (ß (!SHAPE :B4 !POINTED))
    (ß (!SHAPE :B5 !POINTED))
    (ß (!SHAPE :B6 !RECTANGULAR))
    (ß (!SHAPE :B7 !RECTANGULAR))
    (ß (!SHAPE :B10 !RECTANGULAR))

    (ß (!COLOR :B1 !RED))
    (ß (!COLOR :B2 !GREEN))
    (ß (!COLOR :B3 !GREEN))
    (ß (!COLOR :B4 !BLUE))
    (ß (!COLOR :B5 !RED))
    (ß (!COLOR :B6 !RED))
    (ß (!COLOR :B7 !GREEN))
    (ß (!COLOR :B10 !BLUE))
    (ß (!COLOR :BOX !WHITE))
    (ß (!COLOR :TABLE !BLACK))

    (ß (!CALL :SHRDLU SHRDLU))
    (ß (!CALL :FRIEND YOU))
nil

(§ SETQ HANDAT '(32 0 0))

(§ SETQ THTIME 0)

(§ THFLUSH HISTORY)

(§ ERRSET (STARTHISTORY))

(§ SETQ PLAN nil)

(§ dorun (map #'(lambda (X Y) (PUTPROP X (list Y) 'COLOR))
    '(:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B10)
    '(CB1 CB2 CB3 CB4 CB5 CB6 CB7 CB10)))

#_(ns shrdlu.plnr)

;; DO NOT GRIND THIS FILE WITH THE STANDARD GRIND

(§ SETQ THVERSION nil)

(§ DEFUN THREAD []
    ;; FUNCTION FOR THE /$ READ MACRO
    (let [CHAR nil]
        (RETURN (COND
            ((EQ (SETQ CHAR (READCH)) '?) (list 'THV (READ)))
            ((EQ CHAR 'E) (list 'THEV (READ)))
            ((EQ CHAR '_) (list 'THNV (READ)))
            ((EQ CHAR 'T) '(THTBF THTRUE))
            ((EQ CHAR 'R) 'THRESTRICT)
            ((EQ CHAR 'G) 'THGOAL)
            ((EQ CHAR 'A) 'THASSERT)
            ((EQ CHAR 'N) (list 'THANUM (READ)))
            ((PRINT 'ILLEGAL-PREFIX)
                (PRINC '$)
                (PRINC CHAR)
                (PRINC (READ))
                (ERR nil))))))

(§ defmacro THPUSH [& A]
    ;; (THPUSH THTREE NEWINFO) CONSES NEWINFO ONTO THTREE
    (list 'SETQ (car A) (list 'cons (cadr A) (car A))))

(§ DEFUN EVLIS [X]
    ;; EVLIS EVALS ELEMENTS OF ARG THEN RETURNS ARG
    (dorun (map #'EVAL X)))

(§ DEFUN THPRINTC [X] (TERPRI) (PRINC X) (PRINC \space))

(§ DEFUN THADD [THTT THPL]
    ;; THADD ADDS THEOREMS OR ASSERTION TO THE INPUT
    ;; THPL - PROPERTY LIST TO BE PLACED ON ASSERTION DATABASE INPUTS
    ;; THTT - NAME OF THM OR ACTUAL ASSERTION
    ;; RETURNS NIL IF ALREADY THERE ELSE RETURNS THTT
    (let [THNF nil THWH nil THCK nil THLAS nil THTTL nil THT1 nil THFST nil THFSTP nil THFOO nil]
        (SETQ THCK
            ;; IF THTT IS ATOMIC, WE ARE ASSERTING A THEOREM
            (COND ((ATOM THTT)
                    ;; IF NO THEOREM PROPERTY, THE GUY MADE A MISTAKE
                    (or (SETQ THT1 (GET THTT 'THEOREM))
                        (do (PRINT THTT) (THERT CANT THASSERT\, NO THEOREM - THADD)))
                    ;; THWH NOW SET TO KIND OF THEOREM, LIKE THERASING
                    (SETQ THWH (car THT1))
                    ;; MAKE AN EXTRA POINTER TO THTT
                    (SETQ THTTL THTT)
                    ;; IF WE HAVE A PL FOR OUR THEOREM, IT GOES ON THE ATOM WHICH IS THE NAME OF THE THEOREM
                    (and THPL (do
                    ;; GO THROUGH ITEMS ON PL ONE BY ONE
                    =>  (THPUTPROP THTT (cadr THPL) (car THPL))
                        (COND ((SETQ THPL (cddr THPL))
                            (GO =>)))
                        nil))
                    (caddr THT1))
                ;; SO WE HAVE AN ASSERTION TO ASSERT, MAKE THWH REFLECT THIS FACT
                (:else (SETQ THWH 'THASSERTION)
                    ;; PROPERTY LIST IS "CDR" OF ASSERTION
                    (SETQ THTTL (cons THTT THPL))
                    THTT)))
        (SETQ THNF 0) ;; THNF IS COUNTER SAYING WHICH ATOM WE ARE FILING UNDER
        (SETQ THLAS (count THCK)) ;; THLAS IS THE NUMBER OF TOP LEVEL ITEMS
        (SETQ THFST true)
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
                (SETQ THNF 0)
                (SETQ THFOO (SETQ THFST nil))
                ;; THFIRSTP SAYS WE AGAIN NEED TO CHECK FOR ASSERTEE
                ;; BEING IN DATA BASE, BUT NOW USE VARIABLES FOR EQ CHECK
                (SETQ THFSTP true)
                (GO THP1))
            ((nil? (SETQ THT1 (THIP (car THCK)))) (RETURN nil))
            ;; THIP IS THE WORKHORSE FOR THADD IF IT RETURNS NIL.
            ;; IT MEANS THE ASSERTEE IS ALREADY IN, SO FAIL.
            ((EQ THT1 'THOK))
            ;; THOK WHICH IS RETURN BY THIP SAYS THAT THE ASSERTEE IS NOT IN ALREADY.
            ;; OTHERWISE WE GO AROUND AGAIN, STILL LOOKING FOR A NON VARIABLE ITEM TO DO THE EQ CHECK.
            ((SETQ THFOO (concat THFOO (list (COND ((EQ THT1 'THVRB) (car THCK))))))
                (SETQ THCK (cdr THCK))
                (GO THP1)))
        (SETQ THFST nil)
        (dorun (map #'THIP (cdr THCK)))
        (SETQ THNF 0)
        (dorun (map #'THIP THFOO))
        (RETURN THTTL)))

(§ DEFUN THAMONG FEXPR [THA]
    ;; EXAMPLE - (THAMONG $?X (THFIND ... ))
    ;; $E - (THAMONG $E$?X (THFIND ... )) CAUSES THE THVALUE OF $?X TO BE THE FIRST INPUT TO THAMONG.
    ;; THXX SET TO OLD BINDING CELL OF $?X (OR $E$?X) IF $?X VALUES PUSHED ONTO THTREE AND THAMONG
    ;; FAILS TO THUNASSIGNED, OLD VALUE AND LIST OF NEW THAMONGF.
    (COND
        ((EQ (cadr (SETQ THXX (THGAL (COND ((EQ (caar THA) 'THEV) (THVAL (cadar THA) THALIST)) (:else (car THA))) THALIST))) 'THUNASSIGNED)
            (THPUSH THTREE (list 'THAMONG THXX (THVAL (cadr THA) THALIST)))
            nil)
        (:else (MEMBER (cadr THXX) (THVAL (cadr THA) THALIST)))))       ;; IF $?X ASSIGNED, THAMONG REDUCES TO A MEMBERSHIP TEST

(§ DEFUN THAMONGF []                                                 ;; (CAR THTREE) = (THAMONG OLDBINDINGCELL (NEW VALUES))
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

(§ DEFUN THAND FEXPR [A]
    (or (not A)
        (do (THPUSH THTREE (list 'THAND A nil)) (SETQ THEXP (car A)))))

(§ DEFUN THANDF [] (THBRANCHUN) nil)

(§ DEFUN THANDT []
    (COND ((cdadar THTREE)
            (THBRANCH)
            (SETQ THEXP (cadr (cadar THTREE)))
            (RPLACA (cdar THTREE) (cdadar THTREE)))
        ((THPOPT)))
    THVALUE)

(§ DEFUN THANTE FEXPR [THX]
    ;; DEFINES AND OPTIONALLY ASSERTS ANTECEDENT THEOREMS
    (THDEF 'THANTE THX))

(§ DEFUN THAPPLY FEXPR [L]
    (THAPPLY1 (car L)
        ;; THAPPLY1 DOES THE REAL WORK, ALL WE DO IS GET THE THEOREM OFF THE PROPERTY LIST
        (GET (car L) 'THEOREM)
        (cadr L)))

(§ DEFUN THAPPLY1 [THM THB DAT]
    ;; MAKE SURE THE THEOREM PATTERN MATCHES THE GOAL
    (COND ((and (THBIND (cadr THB)) (THMATCH1 DAT (caddr THB)))
            (and THTRACE (THTRACES 'THEOREM THM))
            ;; AS FAR AS THTREE GOES, ALL THEOREMS LOOK LIKE THPROG, AND
            ;; WHEN YOU COME DOWN TO IT, THEY ALL ACT LIKE THPROGS.
            (THPUSH THTREE (list 'THPROG (cddr THB) nil (cddr THB)))
            ;; CALL THE MAIN THPROG WORKHORSE
            (THPROGA)
            true)
        ;; IF THE THEOREM PATTERN DIDN'T MATCH, START FAILING.
        (:else (SETQ THALIST THOLIST) (THPOPT) nil)))

(§ DEFUN THASS1 [THA P]
    (let [THX nil THY1 nil THY nil TYPE nil PSEUDO nil]
        (and (cdr THA) (EQ (caadr THA) 'THPSEUDO) (SETQ PSEUDO true))
        ;; IF YOU SEE "THPSEUDO" SET FLAG "PSEUDO" TO T
        (or (ATOM (SETQ THX (car THA)))
            ;; IF (CAR THA) IS AN ATOM WE ARE ASSERTING (ERRASING) A THEOREM
            (THPURE (SETQ THX (THVARSUBST THX nil)))
            ;; THVARSUBST SUBSTITUTES THE ASSIGNMENTS FOR ALL ASSIGNED VARIABLES
            ;; THPURE CHECKS THAT ALL VARIABLES ARE ASSIGNED
            PSEUDO
            ;; IF WE ARE NOT REALLY ASSERTING, THE VARIABLES DO NOT ALL HAVE TO BE ASSIGNED
            (do (PRINT THX)
                (THERT IMPURE ASSERTION OR ERASURE - THASS1)))
        (and THTRACE (not PSEUDO) (THTRACES (COND (P 'THASSERT) ('THERASE)) THX))
        (SETQ THA (COND (PSEUDO (cddr THA)) ((cdr THA))))
        ;; THX IS NOW WHAT WE ARE ASSERTING, AND THA IS THE RECOMMENDATION LIST
        (or
            ;; WE ARE NOW GOING TO PHYSICALLY ADD OR REMOVE ITEM
            (SETQ THX
                (COND (PSEUDO (list THX))
                    ;; IF THPSEUDO, DON'T ALTER THE DATA BASE
                    ;; IF P IS "T" WE ARE ASSERTING SO USE THADD
                    (P (THADD THX
                        ;; THADD TAKES TWO ARGS THE FIRST IS ITEM TO BE ADDED
                        ;; THE SECOND IS THE PROPERTY LIST FOR THE ITEM
                        (SETQ THY
                            ;; THPROP SAYS "MY" CADR IS TO BE EVALED TO GET THE PROPERTY LIST
                            (COND ((and THA (EQ (caar THA) 'THPROP))
                                (PROG1 (eval (cadar THA))
                                    ;; AND REMOVE THPROP FROM THE RECOMENDATION LIST
                                    (SETQ THA (cdr THA))))))))
                    ;; OTHERWISE WE ARE ERASING, SO USE THREMOVE
                    (:else (THREMOVE THX))))
            ;; THE LAST ITEM WILL BE NIL ONLY IF THADD OR THREMOVE FAILED.
            ;; THAT IS, IF THE ITEM TO BE ADDED WAS ALREADY THERE, OR THE ONE TO BE REMOVED, WASN'T.
            (RETURN nil))
        ;; TYPE IS THE KIND OF THEOREM WE WILL BE LOOKING FOR
        (COND (P (SETQ TYPE 'THANTE))
            ((SETQ TYPE 'THERASING)))
        ;; IF WE ACTUALLY MUNGED THE DATABASE, PUT THE FACT IN THTREE
        (or PSEUDO
            (THPUSH THTREE (list (COND (P 'THASSERT) ('THERASE)) THX THY)))
        ;; MAPCAN IS A MAC-LISP FUNCTION, LIKE MAPCAR BUT USES NCONC.
        ;; THTAE LOOKS AT THE RECOMENDATION LIST AND PRODUCES A LIST OF INSTRUCTIONS ABOUT WHAT THEOREMS TO TRY.
        (SETQ THY (doall (map #'THTAE THA)))
        ;; THEXP IS A HACK TELLING THVAL TO THVAL THIS ITEM BEFORE IT GOES ON TO THE NEXT LINE OF PLANNER CODE.
        ;; THEXP IS NOW (THDO <APPROPRIATE ANTECEDENT OR ERASING THEOREMS>).
        (COND (THY (SETQ THEXP (cons 'THDO THY))))
        (RETURN THX)))

(§ DEFUN THASSERT FEXPR [THA]
    ;; THASS1 IS USED FOR BOTH ASSERTING AND ERASING, THE "T" AS SECOND ARG TELLS IT THAT WE ARE ASSERTING.
    (THASS1 THA true))

(§ DEFUN THASSERTF []
    (THREMOVE (COND ((ATOM (cadar THTREE)) (cadar THTREE)) (:else (caadar THTREE))))
    (THPOPT)
    nil)

(§ DEFUN THASSERTT [] (PROG1 (cadar THTREE) (THPOPT)))

(§ DEFUN THASVAL FEXPR [X]
    ((lambda (X) (and X (not (EQ (cadr X) 'THUNASSIGNED))))
        (THGAL (car X) THALIST)))

(§ DEFUN THBA [TH1 TH2]
    ;; JUST LIKE ASSQ IN LISP, ONLY RETURN WITH THE POINTER 1 ELEMENT PRIOR TO THE ONE ASKED FOR.
    ;; USED ONLY BY THAD AND THREMOVE.
    (let [THP nil]
        (SETQ THP TH2)
    =>  (and (EQ (COND (THPC (cadr THP)) (:else (caadr THP))) TH1)
            (RETURN THP))
        (or (cdr (SETQ THP (cdr THP))) (RETURN nil))
        (GO =>)))

(§ DEFUN THBAP [TH1 TH2]
    ;; LIKE THBA, ONLY USED EQUAL RATHER THAN EQ
    (let [THP nil]
        (SETQ THP TH2)
    =>  (and (EQUAL (COND (THPC (cadr THP)) (:else (caadr THP))) TH1)
            (RETURN THP))
        (or (cdr (SETQ THP (cdr THP))) (RETURN nil))
        (GO =>)))

(§ DEFUN THBIND [A]
    ;; WHEN WE ENTER A NEW THEOREM OR THPROG, WE MUST BIND THE NEW VARIABLES.  A IS THE VARIABLE LIST
    (SETQ THOLIST THALIST) ;; THOLIST IS THE OLD THALIST
    ;; IF A IS NIL THERE IS NOTHING TO DO
    (or (nil? A)
        (do
        ;; WHEN A IS NIL, WE ARE DONE AND JUST PUT A MARKER ON THTREE WITH A POINTER TO THE OLD THALIST,
        ;; SO IT CAN BE RESTORED.
        =>  (COND ((nil? A)
                (THPUSH THTREE (list 'THREMBIND THOLIST))
                (RETURN true)))
            ;; OTHERWISE ADD TO THE ALIST THE NEW BINDING CELL
            (THPUSH THALIST
                ;; THE FIRST ELEMENT IS THE NAME OF THE VARIABLE IF THE ENTRY IS AN ATOM,
                ;; THEN WE ARE JUST GIVEN THE VARIABLE AND ITS INITIAL ASSIGNMENT IS "THUNASSIGNED",
                ;; I.E. NO INITIAL ASSIGNMENT.
                (COND ((ATOM (car A)) (list (car A) 'THUNASSIGNED))
                    ;; OTHERWISE OUR ENTRY IS A LIST
                    ;; IF THE FIRST ELEMENT OF THE LIST IS $R OR THRESTRICT
                    ;; WE ADD THE RESTRICTION TO THE BINDING CELL
                    ;; THE CDDR OF THE CELL GIVES THE RESTRICTION LIST
                    ((EQ (caar A) 'THRESTRICT) (concat (THBI1 (cadar A)) (cddar A)))
                    ;; OTHERWISE WE ARE GIVEN BOTH THE VARIABLE AND ITS
                    ;; INITIAL ASSIGNMENT, SO MAKE THE SECOND ELEMENT OF THE
                    ;; BINDING CELL A POINTER TO THE INITIAL ASSIGNMENT
                    (:else (list (caar A) (eval (cadar A))))))
            (SETQ A (cdr A))
            ;; REPEAT FOR THE NEXT VARIABLE IN THE LIST
            (GO =>))))

(§ DEFUN THBI1 [X] (COND ((ATOM X) (list X 'THUNASSIGNED)) (:else (list (car X) (eval (cadr X))))))

(§ DEFUN THBKPT FEXPR [L] (or (and THTRACE (THTRACES 'THBKPT L)) THVALUE))

(§ DEFUN THBRANCH []
    ;; THBRANCH IS CALLED BY THPROGT AND WE ARE SUCCEEDING BACKWARDS.
    ;; CAR THTREE IS THE THPROG MARKING.
    (COND ;; THERE ARE NO MORE EXPRESSIONS TO EXECUTE IN THE THPROG.
        ((not (cdadar THTREE)))
        ((EQ THBRANCH THTREE) (SETQ THBRANCH nil))
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

(§ DEFUN THBRANCHUN []
    ;; WE ARE NOW FAILING.  THBRANCHUN IS CALLED BY THPROGF.
    (let [X nil] (RETURN
        (COND ;; IF THE SECOND ARG TO THE PROG MARK IS NON-NIL, IT MEANS THAT THERE ARE PREVIOUS LINES IN THE THPROG TO FAIL BACK TO
            ((SETQ X (caddar THTREE))
                ;; A COMPAIRISON OF THIS WITH WHAT HAPPEND IN THBRANCK WILL REVEAL THAT
                ;; ALL WE ARE DOING HERE IS RESTORING THE PROG MARK TO IS STATE BEFORE THE LAST SUCCESS.
                (RPLACA (cdar THTREE) (caddar X))
                (RPLACA (cddar THTREE) (cdr X))
                ;; RESET THALIST AND THTREE
                (SETQ THALIST (cadar X))
                (SETQ THTREE (caar X))
                true)
            ;; THERE AREN'T ANY MORE THINGS IN THE THPROG TO TRY,
            ;; SO JUST RETURN NIL.
            (:else (THPOPT) nil)))))

(§ DEFUN THCOND FEXPR [THA]
    (THPUSH THTREE (list 'THCOND THA nil))
    (SETQ THEXP (caar THA)))

(§ DEFUN THCONDF [] (THOR2 nil))

(§ DEFUN THCONDT []
    (RPLACA (car THTREE) 'THAND)
    (RPLACA (cdar THTREE) (caadar THTREE))
    THVALUE)

;; THCONSE DEFINES AND OPTIONALLY ASSERTS CONSEQUENT THEOREMS

(§ DEFUN THCONSE FEXPR [THX] (THDEF 'THCONSE THX))

(§ DEFUN THDATA []
    (let [X nil]
    =>  (TERPRI)
        (COND ((nil? (SETQ X (READ nil))) (RETURN true))
            ((PRINT (THADD (car X) (cdr X)))))
        (GO =>)))

;; THDEF DEFINES AND OPTIONALLY ASSERTS THEOREMS

(§ DEFUN THDEF [THMTYPE THX]
    (let [THNOASSERT? nil THMNAME nil THMBODY nil]
        (COND ((not (ATOM (car THX)))
                (SETQ THMBODY THX)
                (COND
                    ((EQ THMTYPE 'THCONSE) (SETQ THMNAME (THGENAME TC-G)))
                    ((EQ THMTYPE 'THANTE) (SETQ THMNAME (THGENAME TA-G)))
                    ((EQ THMTYPE 'THERASING) (SETQ THMNAME (THGENAME TE-G)))))
            ((SETQ THMNAME (car THX)) (SETQ THMBODY (cdr THX))))    ;; THNOASSERT FEATURE
        (COND ((EQ (car THMBODY) 'THNOASSERT)
            (SETQ THNOASSERT? true)
            (SETQ THMBODY (cdr THMBODY))))
        (THPUTPROP THMNAME (cons THMTYPE THMBODY) 'THEOREM)
        (COND
            (THNOASSERT?  (PRINT (list THMNAME 'DEFINED 'BUT 'NOT 'ASSERTED)))
            ((THASS1 (list THMNAME) true) (PRINT (list THMNAME 'DEFINED 'AND 'ASSERTED)))
            (:else (PRINT (list THMNAME 'REDEFINED))))
        (RETURN true)))

(§ DEFUN THDO FEXPR [A]
    (or (not A)
        (do (THPUSH THTREE (list 'THDO A nil nil)) (SETQ THEXP (car A)))))

(§ DEFUN THDO1 []
    (RPLACA (cdar THTREE) (cdadar THTREE))
    (SETQ THEXP (caadar THTREE))
    (COND (THBRANCH
        (RPLACA (cddar THTREE) (cons THBRANCH (caddar THTREE)))
        (SETQ THBRANCH nil)
        (RPLACA (cdddar THTREE) (cons THABRANCH (car (cdddar THTREE)))))))

(§ DEFUN THDOB []
    (COND ((or THMESSAGE (nil? (cdadar THTREE)))
            (RPLACA (car THTREE) 'THUNDO)
            true)
        ((THDO1))))

(§ DEFUN THERASE FEXPR [THA] (THASS1 THA nil))

(§ DEFUN THERASEF []
    (THADD (COND ((ATOM (cadar THTREE)) (cadar THTREE)) (:else (caadar THTREE)))
        (COND ((ATOM (cadar THTREE)) nil) (:else (cdadar THTREE))))
    (THPOPT)
    nil)

(§ DEFUN THERASET [] (PROG1 (cadar THTREE) (THPOPT)))

;; THERASING DEFINES AND OPTIONALLY ASSERTS ERASING THEOREMS

(§ DEFUN THERASING FEXPR [THX] (THDEF 'THERASING THX))

(§ DEFUN THFAIL FEXPR [THA]
    (and THA
        (let [THTREE1 nil THA1 nil THX nil]
        F   (SETQ THA1 (COND
                ((EQ (car THA) 'THEOREM) 'THPROG)
                ((EQ (car THA) 'THTAG) 'THPROG)
                ((EQ (car THA) 'THINF) (SETQ THINF true) (RETURN nil))
                ((EQ (car THA) 'THMESSAGE) (SETQ THMESSAGE (cadr THA)) (RETURN nil))
                (:else (car THA))))
            (SETQ THTREE1 THTREE)
        LP1 (COND ((nil? THTREE1)
                    (PRINT THA)
                    (COND ((ATOM (SETQ THA (THERT NOT FOUND - THFAIL))) (RETURN THA))
                        (:else (GO F))))
                ((EQ (caar THTREE1) THA1) (GO ELP1)))
        ALP1 (SETQ THTREE1 (cdr THTREE1))
            (GO LP1)
        ELP1 (COND ((EQ (car THA) 'THTAG)
                (COND ((MEMQ (cadr THA) (cadddr (car THTREE1))) (GO TAGS))
                    (:else (GO ALP1)))))
            (SETQ THMESSAGE (list (cdr THTREE1) (and (cdr THA) (cadr THA))))
            (RETURN nil)
        TAGS (SETQ THX (caddar THTREE1))
        LP2  (COND ((nil? THX) (GO ALP1))
                ((EQ (caaddr (car THX)) (cadr THA))
                    (SETQ THMESSAGE (list (caar THX) (and (cddr THA) (caddr THA))))
                    (RETURN nil)))
            (SETQ THX (cdr THX))
            (GO LP2))))

(§ DEFUN THFAIL? [PRD ACT]
    (THPUSH THTREE (list 'THFAIL? PRD ACT))
    THVALUE)

(§ DEFUN THFAIL?F []
    (COND ((eval (cadar THTREE))
            (eval (PROG2 (SETQ THMESSAGE nil) (caddar THTREE) (THPOPT))))
        (:else (THPOPT) nil)))

(§ DEFUN THFAIL?T [] (THPOPT) THVALUE)

(§ DEFUN THFINALIZE FEXPR [THA]
    (let [THTREE1 nil THT nil THX nil]
        (COND ((nil? THA) (SETQ THA (THERT BAD CALL - THFINALIZE))))
        (COND ((ATOM THA) (RETURN THA))
            ((EQ (car THA) 'THTAG) (SETQ THT (cadr THA)))
            ((EQ (car THA) 'THEOREM) (SETQ THA (list 'THPROG))))
        (SETQ THTREE (SETQ THTREE1 (cons nil THTREE)))
    PLUP (SETQ THX (cadr THTREE1))
        (COND ((nil? (cdr THTREE1)) (PRINT THA) (THERT OVERPOP - THFINALIZE))
            ((and THT (EQ (car THX) 'THPROG) (MEMQ THT (cadddr THX)))
                (GO RTLEV))
            ((or (EQ (car THX) 'THPROG) (EQ (car THX) 'THAND))
                (RPLACA (cddr THX) nil)
                (SETQ THTREE1 (cdr THTREE1)))
            ((EQ (car THX) 'THREMBIND)
                (SETQ THTREE1 (cdr THTREE1)))
            ((RPLACD THTREE1 (cddr THTREE1))))
        (COND ((EQ (car THX) (car THA)) (GO DONE)))
        (GO PLUP)
    RTLEV (SETQ THX (cddr THX))
    LEVLP (COND ((nil? (car THX)) (SETQ THTREE1 (cdr THTREE1)) (GO PLUP))
            ((EQ (caaddr (caar THX)) THT) (GO DONE)))
        (RPLACA THX (cdar THX))
        (GO LEVLP)
    DONE (SETQ THTREE (cdr THTREE))
        (RETURN true)))

(§ DEFUN THFIND FEXPR [THA]
    (THBIND (caddr THA))
    (THPUSH THTREE
        (list 'THFIND
            (COND ((EQ (car THA) 'ALL) ' (1 nil nil))               ;; STANDARD ALL
                ((number? (car THA))
                    (list (car THA) (car THA) true))                       ;; SINGLE NUMBER
                ((number? (caar THA)) (car THA))                    ;; WINOGRAD CROCK FORMAT
                ((EQ (caar THA) 'EXACTLY)
                    (list (cadar THA) (inc (cadar THA)) nil))
                ((EQ (caar THA) 'AT-MOST)
                    (list 1 (inc (cadar THA)) nil))
                ((EQ (caar THA) 'AS-MANY-AS)
                    (list 1 (cadar THA) true))
                (:else (cons (cadar THA)                                ;; ONLY THING LEFT IS AT-LEAST
                    (COND ((nil? (cddar THA)) (list nil true))         ;; NO AT-MOST
                        ((EQ (caddar THA) 'AT-MOST)
                            (list (inc (car (cdddar THA)))
                            nil))
                        (:else (list (car (cdddar THA)) true))))))
            (cons 0 nil)
            (cadr THA)))
    (THPUSH THTREE (list 'THPROG (cddr THA) nil (cddr THA)))
    (THPROGA))

(§ DEFUN THFINDF []
    (SETQ THBRANCH nil)
    (COND ((or THMESSAGE (< (caadr (SETQ THXX (cdar THTREE))) (caar THXX)))
            (THPOPT)
            nil)
        (:else (THPOPT) (cdadr THXX))))

(§ DEFUN THFINDT []
    (let [THX nil THY nil THZ nil THCDAR nil]
        (SETQ THZ (caddr (SETQ THCDAR (cdar THTREE))))
        (and (MEMBER (SETQ THX (THVARSUBST THZ nil)) (cdadr THCDAR))
            (GO =>))
        (RPLACD (cadr THCDAR) (cons THX (cdadr THCDAR)))
        (and (EQ (SETQ THY (inc (caadr THCDAR))) (cadar THCDAR))
            (RETURN (PROG2 (SETQ THBRANCH nil) (and (caddar THCDAR) (cdadr THCDAR)) (THPOPT))))
        (RPLACA (cadr THCDAR) THY)
    =>  (SETQ THTREE THBRANCH)
        (SETQ THALIST THABRANCH)
        (SETQ THBRANCH nil)
        (RETURN nil)))

(§ DEFUN THFLUSH FEXPR [A]
    ;; (THFLUSH) FLUSHES ALL ASSERTIONS AND THEOREMS
    ;; INPUT = SEQUENCE OF INDICATORS DEFAULT =
    ;; EFFECT = FLUSHES THE PROPERTIES OF THESE
    ;; (THASSERTION THCONSE THANTE THERASING)
    ;; INDICATORS FROM ALL ATOMS
    (dorun (map #'(lambda (B)
        (dorun (map #'(lambda (C)
            (dorun (map #'(lambda (D)
                (REMPROP D B))
            C)))
        (MAKOBLIST nil))))
    (COND (A) (' (THASSERTION THCONSE THANTE THERASING))))))

(§ DEFUN THGAL [X Y]
    ;; (THGAL $?X THALIST) RETURNS THE BINDING CELL (X -) OF X ON THALIST
    (SETQ THXX X)
    (SASSQ (cadr X) Y #'(lambda nil (PRINT THXX) (THERT THUNBOUND THGAL))))

(§ DEFUN THGENAME FEXPR [X]
    ;; GENERATES UNIQUE NAME WITH ARG AS PREFIX
    (READLIST (concat (EXPLODE (car X)) (EXPLODE (SETQ THGENAME (inc THGENAME))))))

(§ DEFUN THGO FEXPR [X]
    (APPLY #'THSUCCEED (cons 'THTAG X)))

(§ DEFUN THGOAL FEXPR [THA]                                   ;; THA = (PATTERN RECOMMENDATION)
    (let [THY nil THY1 nil THZ nil THZ1 nil THA1 nil THA2 nil]                     ;; PATTERN IS EITHER EXPLICIT, THE VALUE OF A
        (SETQ THA2 (THVARSUBST (car THA) true))                ;; PLANNER VARIABLE OR THVAL OF $E... THA2 =
        (SETQ THA1 (cdr THA))                               ;; INSTANTIATED PATTERN THA1 = RECOMMENDATIONS
        (COND ((or (nil? THA1)                              ;; SHOULD DATA BASE BE SEARCHED TRYED IF NO RECS
                (and (not (and (EQ (caar THA1) 'THANUM)
                        (SETQ THA1 (cons (list 'THNUM (cadar THA1)) (cons (list 'THDBF 'THTRUE) (cdr THA1))))))
                    (not (and (EQ (caar THA1) 'THNODB)      ;; TRIED IF REC NOT THNODB OR (THDBF PRED)
                        (do (SETQ THA1 (cdr THA1)) true)))
                    (not (EQ (caar THA1) 'THDBF))))
            (SETQ THA1 (cons (list 'THDBF 'THTRUE) THA1))))
        (SETQ THA1 (doall (map #'THTRY THA1)))                   ;; THMS AND ASSERTIONS SATISFYING RECS APPENDED TO RECS
        (and THTRACE (THTRACES 'THGOAL THA2))
        (COND ((nil? THA1) (RETURN nil)))
        (THPUSH THTREE (list 'THGOAL THA2 THA1))            ;; (THGOAL PATTERN MATCHES)
        (RPLACD (cddar THTREE) 262143)
        (RETURN nil)))                                      ;; FAILS TO THGOALF

(§ DEFUN THGOALF []
    ;; BASICALLY ALL IT DOES IS TO SEND OFF TO THTRY1 TO TRY ANOTHER POSSIBILITY.
    ;; IF THTRY1 RETURNS NIL, IT MEANS THAT IT COULDN'T FIND ANOTHER POSSIBILITY
    ;; AND WE SHOULD TELL THVAL THAT WE HAVE FAILED.
    ;; ALL THPOPT DOES IS TO LOB THE THGOAL ENTRY OFF THTREE.
    (COND (THMESSAGE (THPOPT) nil) ((THTRY1)) (:else (THPOPT) nil)))

(§ DEFUN THGOALT []
    (PROG1
        (COND ((EQ THVALUE 'THNOVAL) (THVARSUBST (cadar THTREE) nil))
            (THVALUE))
        (THPOPT)))

(§ DEFUN THIP [THI]
    ;; THI IS AN ITEM FROM THE ASSERTION OR PATTERN OF THE THEOREM BEING ENTERED
    (let [THT1 nil THT3 nil THSV nil THT2 nil THI1 nil]
        (SETQ THNF (inc THNF))
        ;; THNF IS A FREE VARIABLE FROM THADD (WHO CALLS THIS BUGER)
        ;; IT SAYS WE ARE LOOKING AT THE N'TH PLACE IN THE PATTERN
        (COND ((and (ATOM THI) (not (EQ THI '?)) (not (number? THI)))
                ;; THI1 IS THE NAME OF THE ATOM TO LOOK UNDER WHEN THI IS A USUAL ATOM
                ;; THI1 = THI NUMBERS DON'T HAVE PROPERTY LISTS SO THEY DON'T COUNT AS
                ;; NORMAL ATOMS, NOR DOES "?" SINCE IT IS A SORT OF VARIABLE IN PLANNER
                (SETQ THI1 THI))
            ((or (EQ THI '?) (MEMQ (car THI) '(THV THNV)))
                ;; SEE IF THI IS A VARIABLE
                (COND (THFST (RETURN 'THVRB))
                    ;; IF WE ARE DOING THIS FOR THE FIRST TIME, DON'T CONSIDER VARIABLES
                    ;; FOR EXPLANATION WHY, SEE THADD
                    ((SETQ THI1 'THVRB))))
            ((RETURN 'THVRB)))
        ;; OTHERWISE THI IS SOMETHING WITH NO PROPERTY LIST LIKE A NUMBER, OR LIST
        ;; RETURNING THVRB TO THADD TELLS IT THAT EVERYTHING IS OK SO
        ;; FAR, BUT NOTHING WAS DONE ON THIS ITEM
        (COND ((not (SETQ THT1 (GET THI1 THWH)))
                ;; THWH IS THE NAME OF THE PROPERTY TO LOOK UNDER ON THE ATOM
                ;; IF THIS PROPERTY IS NOT THERE THEN WE MUST PUT IT THERE
                ;; IN PARTICULAR, NO PROPERTY MEANS THAT THE
                ;; ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
                (PUTPROP THI1 (list nil (list THNF (list THLAS 1 THTTL))) THWH))
            ((EQ THT1 'THNOHASH) (RETURN 'THBQF))
            ;; IF THE PROPERTY IS "THNOHASH" IT MEANS THAT WE
            ;; SHOULD NOT BOTHER TO INDEX UNDER THIS ATOM, SO
            ;; JUST RETURN TO THADD
            ((not (SETQ THT2 (ASSQ THNF (cdr THT1))))
                ;; LOOK ON THE PROPERTY LIST ENTRY TO SEE
                ;; IF THERE IS A SUB-ENTRY FOR PATTERNS WITH THIS ATOM IN THE THNF'TH POSITION.
                ;; IF NOT, HACK THE ENTRY SO THERE IS.
                ;; AGAIN THIS IMPLIES THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE.
                (concat THT1 (list (list THNF (list THLAS 1 THTTL)))))
            ((not (SETQ THT3 (ASSQ THLAS (cdr THT2))))
                ;; NOW LOOK WITHIN THE SUB-ENTRY FOR A SUB-SUB-ENTRY.
                ;; I.E. THOSE PATTERNS WHICH ARE ALSO OF THE CORRECT TOTAL LENGTH
                ;; THLAS IS A VARIABLE FROM THADD WHICH GIVES THE LENGTH OF THE ASSERTEE
                ;; AGAIN, IF NOT THERE, HACK IT IN
                (concat THT2 (list (list THLAS 1 THTTL))))
            ((and (or THFST THFSTP)
                    ;; THIS BRANCH SAYS THAT WE STILL NEED TO CHECK THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
                    ;; THIS MEANS THAT WE MUST LOOK DOWN THE REMAINING SUB-SUB-BUCKET LOOKING FOR THE ASSERTEE
                    (COND ((EQ THWH 'THASSERTION) (ASSOC THTT (cddr THT3)))
                        ;; RANDOMNESS DUE TO THE FACT THAT ASSERTIONS HAVE PROPERY LIST ON THEM,
                        ;; WHILE THEOREM NAMES ARE ATOMS WHOES PROPERTY LISTS ARE OF THE
                        ;; USUAL "INVISIBLE" VARIETY
                        (:else (MEMQ THTT (cddr THT3)))))
                ;; IF THE ASSERTEE IS FOUND, RETURN NIL INDICATING FAILURE
                (RETURN nil))
            ((SETQ THSV (cddr THT3))
            ;; HACK IN THE LATEST ENTRY INTO THE SUB-SUB-BUCKET
            (RPLACA (cdr THT3) (inc (cadr THT3)))
            (RPLACD (cdr THT3) (concat (list THTTL) THSV))))
        ;; IF WE GET TO THIS POINT, EVERYTHING IS OK, SO TELL THADD SO
        (RETURN 'THOK)))

(§ DEFUN THMATCH2 [THX THY]
    ;; THX IS ONE ITEM FROM THE PATTERN.
    ;; THY IS THE CORESPONDING ITEM FROM THE CANDIDATE.
    ;; THMATCH2 DECIDES IF THE TWO ITEMS REALLY MATCH.

    ;; THOLIST IS THE "THALIST" WHICH WAS IN EXISTANCE BEFORE
    ;; WE STARTED WORKING ON THE CURRENT LINE OF PLANNER CODE
    ;; STANDARD CHECK FOR $E
    (and (EQ (car THX) 'THEV)
        (SETQ THX (THVAL (cadr THX) THOLIST)))
    (and (EQ (car THY) 'THEV)
        (SETQ THY (THVAL (cadr THY) THALIST)))
    (COND
        ;; IF EITHER IS A ? ANYTHING WILL MATCH, SO OK
        ((EQ THX '?))
        ((EQ THY '?))
        ;; IF EITHER IS A VARIABLE THINGS GET MESSY.
        ;; EVERYTHING DOWN TO ***** IS CONCERNED WITH THIS CASE
        ((or (MEMQ (car THX) '(THV THNV THRESTRICT)) (MEMQ (car THY) '(THV THNV THRESTRICT)))
        ((lambda (XPAIR YPAIR)
            ;; X AND Y PAIR ARE THE RESPECTIVE BINDING CELLS WHICH WILL HAVE ANY NEW RESTRICTIONS MENTIONED.
            ;; IF THX OR THY IS NOT A VARIABLE (I.E. THE OTHER IS) THEN X OR Y PAIR WILL BE NIL.
            (COND ((and XPAIR
                ;; THX IS A VARIABLE
                ;; THIS SEES IF THX IS UNASSIGNED
                (or (EQ (car THX) 'THNV)
                    (and (EQ (car THX) 'THV) (EQ (cadr XPAIR) 'THUNASSIGNED)))
                ;; THCHECK MACKES SURE THE RESTRICTIONS (IF ANY) ON
                ;; THX ARE COMPATIBLE WITH THY
                (THCHECK (cddr XPAIR)
                    (COND (YPAIR (cadr YPAIR)) (:else THY))))
                ;; FURTHERMORE, THY IS ALSO A VARIABLE
                ;; THIS MEANS WE MUST DO THE MYSTERIOUS VARIABLE LINKING
                (COND (YPAIR (THRPLACAS (cdr XPAIR) (cadr YPAIR))
                        ;; IF THY ALSO HAS RESTRICTIONS WHEN WE LINK VARIABLES, WE COMBINE RESTRICTIONS
                        (and (cddr YPAIR) (THRPLACDS (cdr XPAIR) (THUNION (cddr XPAIR) (cddr YPAIR))))
                    (THRPLACDS YPAIR (cdr XPAIR)))
                ;; IF THY IS NOT A VARIALBE, JUST ASSIGN THX TO THY
                ;; THRPLACAS WILL HACK THML THE FREE VARIABLE FROM THMATCH1
                (:else (THRPLACAS (cdr XPAIR) THY))))
            ;; IN THIS COND PAIR THY IS A VARIABLE AND THX IS EITHER
            ;; A CONSTANT OR A PREVIOUSLY ASSIGNED VARIALBE
            ((and YPAIR
                (or (EQ (car THY) 'THNV)
                    ;; FURTHERMORE THY IS UNASSIGNED
                    (and (EQ (car THY) 'THV) (EQ (cadr YPAIR) 'THUNASSIGNED)))
                ;; MAKE SURE RESTRICTIONS ARE OK
                (THCHECK (cddr YPAIR) (COND (XPAIR (cadr XPAIR)) (:else THX))))
                ;; IF THX IS A VARIABLE, LINK
                (COND (XPAIR (THRPLACAS (cdr YPAIR) (cadr XPAIR)))
                    ;; OTHERWISE JUST ASSIGN THY TO THX
                    (:else (THRPLACAS (cdr YPAIR) THX))))
            ;; THX IS AN ASSIGED VARIABLE, SO JUST MAKE
            ;; SURE ITS ASSIGNEMENT IS EQUAL TO THY
            ((and XPAIR (EQUAL (cadr XPAIR) (COND (YPAIR (cadr YPAIR)) (:else THY)))))
            ;; THX IS A CONSTANT, THY IS A VARIABLE, AND THEY ARE EQUAL
            ((and YPAIR (EQUAL (cadr YPAIR) THX)))
            ;; LOOSE, SO RETURN WITH AN ERROR
            (:else (ERR nil))))

            ;; THE FOLLOWING TWO CONDS BIND XPAIR AND YPAIR RESPECTIVELY
            (COND
                ;; IF THX IS A NORMAL VARIALBE, IN PARTICULAR
                ;; WE ARE NOT INTRODUCING NEW RESTRICTIONS AT THIS TIME,
                ;; THEN X PAIR IS JUST THE BINDING LIST
                ((THVAR THX) (THGAL THX THOLIST))
                ;; WE MUST HACK A NEW RESTRICTION ONTO THE BINDING LIST
                ((EQ (car THX) 'THRESTRICT)
                ;; WE ARE "RESTRICTING" A ?.  SINCE ? HAS NO
                ;; BINDING LIST, WE MAKE UP A PSEUDO BINDING LIST
                    (COND ((EQ (cadr THX) '?)
                            (PROG1 (cons '? (cons 'THUNASSIGNED (concat (cddr THX) nil)))
                                (SETQ THX '(THNV ?))))
                        ;; WE ARE RESTRICTING A VARIABLE.  THIS MEANS THAT WE MUST PUT IN ON THE BINDING LIST.
                        (:else ((lambda (U)
                                ;; THUNION MAKES SURE WE DON'T PUT THE SAME RESTRICTION ON TWICE.
                                (THRPLACDS (cdr U) (THUNION (cddr U) (cddr THX)))
                                (SETQ THX (cadr THX))
                                U)
                            (THGAL (cadr THX) THOLIST))))))
            ;; NOTE THAT IF THX IS NOT A VARIABLE THEN XPAIR IS ()
            ;; WE DO THE EXACT SAME THING FOR THY AS WE JUST DID FOR THX
            (COND ((THVAR THY) (THGAL THY THALIST))
                ((EQ (car THY) 'THRESTRICT)
                    (COND ((EQ (cadr THY) '?)
                        (PROG1 (cons '? (cons 'THUNASSIGNED (concat (cddr THY) nil)))
                            (SETQ THY '(THNV ?))))
                        (:else ((lambda (U)
                                (THRPLACDS (cdr U) (THUNION (cddr U) (cddr THY)))
                                (SETQ THY (cadr THY))
                                U)
                            (THGAL (cadr THY) THALIST))))))))
        ;; **************
        ;; IF THE TWO ARE EQUAL, NATURALLY THEY MATCH
        ((EQUAL THX THY))
        ;; IF NOT, THEY DON'T, SO REPORT FAILURE
        (:else (ERR nil))))

(§ DEFUN THCHECK [THPRD THX]
    (or (nil? THPRD)
        (EQ THX 'THUNASSIGNED)
        (ERRSET (dorun (map #'(lambda (THY) (or (THY THX) (ERR nil))) THPRD)))))

(§ DEFUN THUNION [L1 L2]
    (dorun (map #'(lambda (THX)
            (COND ((MEMBER THX L2))
                (:else (SETQ L2 (cons THX L2)))))
        L1))
    L2)

(§ DEFUN THMATCH THX
    ((lambda (THOLIST THALIST) (THMATCH1 (ARG 1) (ARG 2)))
        (COND ((> THX 2) (ARG 3)) (:else THALIST))
        (COND ((> THX 3) (ARG 4)) (:else THALIST))))

(§ DEFUN THMATCH1 [THX THY]
    ;; THX IS THE PATTERN TO BE MATCHED.
    ;; THY IS THE POSSIBLE CANDIDATE.
    ;; THMATCH1 DOES PRELIMINARLY WORK BEFORE HANDING THE PATTERN AND CANDIDATE OFF TO THMATCH2
    ;; WHO DOES THE REAL WORK.
    (let [THML nil]
        ;; THML IS A FREE VARIABLE WHO WILL BE HACKED BY THMATCH2 WHEN THMATCH2 IS DONE,
        ;; THML WILL HAVE A RECORD OF ALL VARIABLE ASSIGNMENTS MADE DURING THE MATCH.
        ;; NATURALLY WE MUST KEEP TRACK SO IF WE FAIL BACK WE CAN UNDO THEM.
        ;; WE HAVE TO CHECK THAT THE PATTERN AND CANDIDATE ARE OF THE SAME LENGTH
        ;; SINCE THE USER MAY HAVE SPECIFIED THE CANDIDATE WITH A "THUSE" RECOMMENDATION.
        (COND ((and (EQ (count (COND ((EQ (car THX) 'THEV) (SETQ THX (THVAL (cadr THX) THOLIST))) (THX))) (count THY))
                ;; IF THE MATCH FAILS, THMATCH2 EXITS WITH AN ERR
                ;; WILL BE "TRUE" PROVIDED THE MATCH WORKED
                (ERRSET (dorun (map #'THMATCH2 THX THY))))
            ;; SO RECORD THE ASSIGNMENTS ON THTREE
            (and THML (THPUSH THTREE (list 'THMUNG THML)))
            (RETURN true))
        ;; IF THE MATCH FAILED, WE MAY STILL HAVE SOME ASSIGNEMENTS ALREADY MADE.
        ;; THESE MUST IMMEDIATELY BE UNDONE.  EVLIS JUST EVALS EVERYTHING ON THML
        ;; WHICH IS A LIST OF EXPRESSIONS WHICH, WHEN EVALED, UNASSIGN THE VARIABLES
        (:else (EVLIS THML) (RETURN nil)))))

(§ DEFUN THMATCHLIST [THTB THWH]
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
        (SETQ THAL (count THTB))
        ;; THB1 WILL BE THE REMAINDER OF THE PATTERN TO YET BE WORKED ON.
        ;; WHEN IT IS NIL, WE ARE DONE, SO RETURN THE BUCKET.
        ;; THL1 IS THE BUCKET UNDER THE ATOM.
        ;; THL2 IS THE BUCKET UNDER THE VARIABLE IN THAT POSITION.
        ;; IF WE ARE WORKING ON AN ASSERTION, THL2 WILL BE () SINCE THERE ARE NO VARIABLES IN ASSERTIONS.
        ;; IN THEOREMS, WE MUST TAKE INTO ACCOUNT THE FACT THAT THE THEOREM MAY HAVE EITHER THE CORRECT ATOM,
        ;; OR A VARIALBE IN A GIVEN POSITION, AND STILL MATCH.
        (SETQ THB1 THTB)
    THP1 (or THB1
            (RETURN (COND (THL2 (concat THL1 THL2)) (THL1))))
        ;; ADD1 TO POSITION COUNTER.
        (SETQ THNF (inc THNF))
        ;; THB2 IS THE ITEM WE ARE WORKING ON IN THIS PASS.
        (SETQ THB2 (car THB1))
        ;; UPDATE THB1.
        (SETQ THB1 (cdr THB1))
        ;; IF THE ITEM IS NOT A NORMAL ATOM, SKIP IT AND GO TO NEXT PASS.
    THP3 (COND ((or (nil? (ATOM THB2)) (number? THB2) (EQ THB2 '?))
                (GO THP1))
            ;; IF THE ITEM DOES NOT HAVE THE PROPERTY ON ITS PROPERTY LIST,
            ;; THEN IT OBVIOUSLY DOSEN'T HAVE ANY BUCKET AT ALL.
            ;; SO THA1, WHICH RECORDS THE NUMBER IN THE BUCKET IS SET TO 0
            ((not (SETQ THA1 (GET THB2 THWH)))
                ;; IF A BUCKET IS FOUND, THE FIRST THING IN THE BUCKET WILL BE THE NUMBER OF GOODIES THERE.
                ;; THE REST WILL BE THE GOODIES.  THE FIRST 0 IN THA1 THEN SAYS THAT THERE WAS NO BUCKET.
                ;; THE SECOND 0 IS JUST A DUMMY FOR THE GOODIES WHICH ARN'T THERE.
                (SETQ THA1 '(0 0)))
            ;; IF IT IS A THNOHASH, WE IGNORE IT JUST LIKE A LIST, OR NUMBER.
            ((EQ THA1 'THNOHASH) (GO THP1))
            ;; SAME IF THERE IS NO SUB-BUCKET FOR THE ATOM IN THE CORRECT POSITION.
            ((not (SETQ THA1 (ASSQ THNF (cdr THA1))))
                (SETQ THA1 '(0 0)))
            ;; SAME FOR SUB-SUB-BUCKET (PATTERN LENGTH).
            ((not (SETQ THA1 (ASSQ THAL (cdr THA1))))
                (SETQ THA1 '(0 0))))
        (SETQ THRN (cadr THA1))
        (SETQ THA1 (cddr THA1))
        ;; IF IT'S AN ASSERTION, THEN WE DONT HAVE TO LOOK FOR VARIABLES.
        (and (EQ THWH 'THASSERTION) (GO THP2))
        ;; THVRB IS THE ATOM WHICH HAS THE BUCKET FOR VARIABLES.
        ;; WE WILL NOW LOOK TO SEE IF THERE ARE ANY THEOREMS WHICH
        ;; HAVE A VARIABLE IN THE CORRECT POSSITION.
        (COND
            ((not (SETQ THA2 (GET 'THVRB THWH))) (SETQ THA2 '(0 0)))
            ((not (SETQ THA2 (ASSQ THNF (cdr THA2)))) (SETQ THA2 '(0 0)))
            ((not (SETQ THA2 (ASSQ THAL (cdr THA2)))) (SETQ THA2 '(0 0))))
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
            ((EQ THRN 0) (RETURN nil))
            ;; IF THE NEW BUCKET IS SMALLER, IT BECOMES THE SMALLEST SO FAR.
            ((> THL THRN) (SETQ THL1 THA1) (SETQ THL THRN)))
        ;; GO BACK FOR ANOTHER PASS.
        (GO THP1)))

(§ DEFUN THMESSAGE FEXPR [THA]
    (THPUSH THTREE (cons 'THMESSAGE THA))
    THVALUE)

(§ DEFUN THMESSAGEF []
    (let [BOD nil]
        (SETQ BOD (car THTREE))
        (THPOPT)
        (COND ((and (THBIND (cadr BOD)) (THMATCH1 (caddr BOD) THMESSAGE))
            (THPUSH THTREE (list 'THPROG (cddr BOD) nil (cddr BOD)))
            (SETQ THMESSAGE nil)
            (RETURN (THPROGA)))
            (:else (SETQ THALIST THOLIST)))
        (RETURN nil)))

(§ DEFUN THMESSAGET [] (THPOPT) THVALUE)

(§ DEFUN THMUNGF [] (EVLIS (cadar THTREE)) (THPOPT) nil)

(§ DEFUN THMUNGT [] (THPOPT) THVALUE)

(§ DEFUN THNOFAIL [THX]
    (COND (THX (DEFPROP THPROG THPROGT THFAIL))
        (:else (DEFPROP THPROG THPROGF THFAIL))))

(§ DEFUN THNOHASH FEXPR [THA]
    (dorun (map #'(lambda (X) (PUTPROP (car THA) 'THNOHASH X))
        (or (cdr THA) '(THASSERTION THCONSE THANTE THERASING)))))

(§ DEFUN THNOT FEXPR [THA]
    (SETQ THEXP (list 'THCOND (list (car THA) '(THFAIL THAND)) '((THSUCCEED)))))

(§ DEFUN THNV FEXPR [X] (THV1 (car X)))

(§ DEFUN THOR FEXPR [THA]
    (and THA
        (THPUSH THTREE (list 'THOR THA))
        (SETQ THEXP (car THA))))

(§ DEFUN THOR2 [P]
    (COND (THMESSAGE (THPOPT) nil)
        ((and (cadar THTREE) (cdadar THTREE))
            (RPLACA (cdar THTREE) (cdadar THTREE))
            (SETQ THEXP
                (COND (P (PROG1 (caadar THTREE) (or (cadar THTREE) (THPOPT))))
                    ((car (caadar THTREE))))))
        (:else (THPOPT) nil)))

(§ DEFUN THORF [] (THOR2 true))

(§ DEFUN THORT [] (THPOPT) THVALUE)

(§ DEFUN THPOPT [] (SETQ THTREE (cdr THTREE)))

(§ DEFUN THPROG FEXPR [THA]
    ;; THBIND HACKS THALIST TO BIND THE VARIABLES.
    ;; IT ALSO HACKS THTREE SO WE CAN UNDO IT IF NEEDED.
    (THBIND (car THA))
    ;; PUT THPROG MARK ON THTREE.
    ;; THE FIRST THA IS A POINTER ONE BEFORE THE NEXT PART OF THE THPROG TO BE HANDELED.
    ;; THE SECOND ONE WILL BE KEPT WHOLE TO SEARCH FOR PROG TAGS.
    (THPUSH THTREE (list 'THPROG THA nil THA))
    ;; CALL WORKHORSE
    (THPROGA))

(§ DEFUN THPROGA []
    ((lambda (X) (COND
            ;; ODD CASE WHERE THE THPROG HAS NO SUBEXPRESSIONS.  RETURN SUCCESS.
            ((nil? (cdar X)) (THPOPT) 'THNOVAL)
            ;; NEXT ITEM IS AN ATOM, HENCE A THPROG TAG.
            ((ATOM (cadar X))
                ;; USE THEXP TO MARK IT ON THTREE.
                (SETQ THEXP (list 'THTAG (cadar X)))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA X (cdar X))
                THVALUE)
            ;; OTHERWISE NEXT EXPRESSION TO BE EVALUATED IS THE NEXT EXPRESSION OF THE THPROG.
            (:else (SETQ THEXP (cadar X))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA X (cdar X))
                THVALUE)))
        (cdar THTREE)))

;; THBRANCH AND THBRANCHUN ARE THE MAIN FUNCTIONS IN CHARGE OF HANDLING THE EFFECTS OF SUCCESS AND FAILURE.
;; THEY ARE ONLY CALLED BY THPROGT AND F

(§ DEFUN THPROGF [] (THBRANCHUN) nil)

(§ DEFUN THPROGT [] (THBRANCH) (THPROGA))

(§ DEFUN THPURE [XX]
    ;; CHECKS TO MAKE SURE THAT THE PATTERN HAS NO UNASSIGNED VARIABLES IN IT.

    ;; XX, NATURALLY ENOUGH IS THE PATTERN
    ;; SINCE THPURE IS ALWAYS CALLED AFTER THVARSUBST
    ;; ANY VARIABLES WHICH DO HAVE ASSIGNMENTS WILL HAVE
    ;; GONE AWAY, RREPLACED BY THEIR ASSIGNMENTS
    ;; SO ALL WE NEED DO IS LOOK FOR ANY VARIABLES APPEARING AT ALL
    (ERRSET (dorun (map #'(lambda (Y) (and (THVAR Y) (ERR nil))) XX))))

(§ DEFUN THPUTPROP [ATO VAL IND]
    (THPUSH THTREE
        (list 'THMUNG
            (list (list 'PUTPROP
                (list 'quote ATO)
                (list 'quote (GET ATO IND))
                (list 'quote IND)))))
    (PUTPROP ATO VAL IND))

(§ DEFUN THREM1 [THB]
    ;; THIS FUNCTION IS ROUGHLY THE SAME AS THIP, EXCEPT WHILE THIP ADDS ASSERTIONS TO THE DATABASE, THREM1 REMOVES THEM
    ;; HENCE ALL COMMENTS WILL BE GUIDES TO THE CORRESPONDENCE BETWEEN THREM1 AND THIP

    ;; THB = THI IN THIP
    (let [THA nil THSV nil THA1 nil THA2 nil THA3 nil THA4 nil THA5 nil THONE nil THPC nil]
        ;; THA AND THA1 DO THE WORK OF THT1 IN THIP
        ;; THA1 = THT2
        ;; THA3 = THT3
        ;; THA4, THA5, THONE, AND THPC ARE NEW
        (SETQ THNF (inc THNF))
        ;; THIS COND SERVES THE SAME PURPOSE AS THE FIRST COND IN THIP
        (COND ((and (ATOM THB) (not (EQ THB '?)) (not (number? THB)))
                (SETQ THA THB))
            ((or (EQ THB '?) (MEMQ (car THB) '(THV THNV)))
                (COND (THFST (RETURN 'THVRB))
                    ((SETQ THA 'THVRB))))
            ((RETURN 'THVRB)))
        ;; ALL THE REST SERVES THE SAME PURPOSE AS THE SECOND COND IN THIP.
        ;; IT WAS ORIGINALLY WRITTEN AS A SINGLE COND, BUT THE COMPILER BARFED ON IT,
        ;; SO IT WAS BROKEN UP INTO BITE SIZE PIECES.
        (SETQ THA1 (GET THA THWH))
        (or THA1 (RETURN nil))
        (and (EQ THA1 'THNOHASH) (RETURN 'THBQF))
        (SETQ THA2 (THBA THNF THA1))
        (or THA2 (RETURN nil))
        (SETQ THA3 (THBA THAL (cadr THA2)))
        (or THA3 (RETURN nil))
        (SETQ THA4 (cadr THA3))
        (SETQ THPC (not (EQ THWH 'THASSERTION)))
        (SETQ THA5
            (COND ((or THFST THFSTP) (THBAP THBS (cdr THA4)))
                ((THBA (COND (THPC THON) (:else (car THON))) (cdr THA4)))))
        (or THA5 (RETURN nil))
        (SETQ THONE (cadr THA5))
        (RPLACD THA5 (cddr THA5))
        (and (not (EQ (cadr THA4) 1))
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
        (RETURN THONE)))

(§ DEFUN THREMBINDF [] (SETQ THALIST (cadar THTREE)) (THPOPT) nil)

(§ DEFUN THREMBINDT [] (SETQ THALIST (cadar THTREE)) (THPOPT) THVALUE)

(§ DEFUN THREMOVE [THB]
    ;; THIS FUNCTION IS ANALAGOUS TO THADD EXCEPT THREMOVE REMOVES RATHER THAN ADDS.
    ;; AS WITH THREM1, ALL COMMENTS WILL EXPLICATE THE ANALOGY,
    ;; SO ONE SHOULD FIRST BECOME FAMILIAR WITH THADD.

    ;; THB = THTT IN THADD.
    ;; THREMOVE TAKES ONLY ONE ARG SINCE THE PROPERTY LIST FOR THE ASSERTION
    ;; PLAYS NO ROLE IN REMOVING THE ASSERTION.
    (let [THB1 nil THWH nil THNF nil THAL nil THON nil THBS nil THFST nil THFSTP nil THFOO nil]
        ;; THB1 AND THON TOGETHER SHARE THE WORK OF THT1 AND THCK IN THADD
        ;; THAL = THLAS
        ;; THBS = THTTL
        (SETQ THNF 0)
        ;; THE STRUCTURE OF THE TWO PROGRAMS IS VIRTUALLY IDENTICAL.
        (SETQ THB1
            (COND ((ATOM THB)
                    (SETQ THBS THB)
                    (SETQ THWH (car (SETQ THB1 (GET THB 'THEOREM))))
                    (caddr THB1))
                ((SETQ THWH 'THASSERTION)
                    (SETQ THBS THB))))
        (SETQ THAL (count THB1))
        (SETQ THFST true)
    THP1 (COND ((nil? THB1)
                (SETQ THB1 THFOO)
                (SETQ THNF 0)
                (SETQ THFST (SETQ THFOO nil))
                (SETQ THFSTP true)
                (GO THP1))
            ((nil? (SETQ THON (THREM1 (car THB1))))
                (RETURN nil))
            ((MEMQ THON '(THBQF THVRB))
                (SETQ THFOO (concat THFOO (list (COND ((EQ THON 'THVRB) (car THB1))))))
                (SETQ THB1 (cdr THB1))
                (GO THP1)))
        (SETQ THFST nil)
        (dorun (map #'THREM1 (cdr THB1)))
        (SETQ THNF 0)
        (dorun (map #'THREM1 THFOO))
        (RETURN THON)))

(§ DEFUN THREMPROP [ATO IND]
    (THPUSH THTREE
        (list 'THMUNG
            (list (list 'PUTPROP
                (list 'quote ATO)
                (list 'quote (GET ATO IND))
                (list 'quote IND)))))
    (REMPROP ATO IND))

(§ DEFUN THRESTRICT FEXPR [THB]
    (let [X nil]
        (COND ((ATOM (SETQ X (THGAL (car THB) THALIST)))
                (THPRINTC "THRESTRICT IGNORED - CONTINUING"))
            ((THRPLACD (cdr X) (THUNION (cddr X) (cdr THB)))))
        (RETURN X)))

(§ DEFUN THRETURN FEXPR [X]
    (APPLY #'THSUCCEED (cons 'THPROG X)))

(§ DEFUN THRPLACA [X Y]
    (let [THML nil]
        (THRPLACAS X Y)
        (THPUSH THTREE (list 'THMUNG THML))
        (RETURN X)))

(§ DEFUN THRPLACAS [X Y]
    (THPUSH THML (list 'THURPLACA X (car X)))
    (RPLACA X Y))

(§ DEFUN THURPLACA FEXPR [L] (RPLACA (car L) (cadr L)))

(§ DEFUN THRPLACD [X Y]
    (let [THML nil]
        (THRPLACDS X Y)
        (THPUSH THTREE (list 'THMUNG THML))
        (RETURN X)))

(§ DEFUN THRPLACDS [X Y]
    (THPUSH THML (list 'THURPLACD X (cdr X)))
    (RPLACD X Y))

(§ DEFUN THURPLACD FEXPR [L] (RPLACD (car L) (cadr L)))

(§ DEFUN THSETQ FEXPR [THL1]
    (let [THML nil THL nil]
        (SETQ THL THL1)
    =>  (COND
            ((nil? THL)
                (THPUSH THTREE (list 'THMUNG THML))
                (RETURN THVALUE))
            ((nil? (cdr THL))
                (PRINT THL1)
                (THERT ODD NUMBER OF GOODIES - THSETQ))
            ((ATOM (car THL))
                (THPUSH THML (list 'SETQ (car THL) (list 'quote (eval (car THL)))))
                (SET (car THL) (SETQ THVALUE (eval (cadr THL)))))
            (:else (THRPLACAS (cdr (THSGAL (car THL)))
                (SETQ THVALUE (THVAL (cadr THL) THALIST)))))
        (SETQ THL (cddr THL))
        (GO =>)))

(§ DEFUN THSGAL [X]
    (SASSQ (cadr X) THALIST
        #'(lambda nil (let [Y nil]
            (SETQ Y (list (cadr X) 'THUNASSIGNED))
            (concat (GET 'THALIST 'VALUE) (list Y))
            (RETURN Y)))))

(§ DEFUN THSTATE FEXPR [THINDICATORS]
    ;; PRINTS THAT PART OF THE STATE OF THE MICRO-PLANNER WORLD SPECIFIED BY THE INDICATORS IN REREADABLE FORM.
    ;; NOTE THAT IT IS BLIND TO ASSERTIONS THAT BEGIN WITH EITHER NUMBERS, LIST STRUCTURE, NOHASHED ATOMS OR NON-INTERNED ATOMS.
    (let [THP nil]
        (PRINT '(THDATA))
        (dorun (map #'(lambda (BUCKET)
            (dorun (map #'(lambda (THATOM)
                (dorun (map #'(lambda (THWH)
                    (and (SETQ THP (GET THATOM THWH)) (SETQ THP (ASSOC 1 (cdr THP)))
                        (dorun (map #'(lambda (LENGTH-BUCKET)
                            (dorun (map #'(lambda (ASRT)
                                (COND ((EQ THWH 'THASSERTION) (PRINT ASRT)) ((PRINT (list ASRT)))))
                            (cddr LENGTH-BUCKET))))
                        (cdr THP)))))
                (COND (THINDICATORS) (' (THASSERTION THANTE THCONSE THERASING))))))
            BUCKET)))
        (MAKOBLIST nil)))
        (PRINT nil)
        nil))

(§ DEFUN THSUCCEED FEXPR [THA]
    (or (not THA)
        (let [THX nil]
            (and (EQ (car THA) 'THEOREM)
                (SETQ THA (cons 'THPROG (cdr THA))))
            (SETQ THBRANCH THTREE)
            (SETQ THABRANCH THALIST)
        =>  (COND
                ((nil? THTREE)
                    (PRINT THA)
                    (THERT OVERPOP - THSUCCEED))
                ((EQ (caar THTREE) 'THREMBIND)
                    (SETQ THALIST (cadar THTREE))
                    (THPOPT)
                    (GO =>))
                ((EQ (caar THTREE) (car THA))
                    (THPOPT)
                    (RETURN (COND ((cdr THA) (eval (cadr THA))) ('THNOVAL))))
                ((and (EQ (car THA) 'THTAG) (EQ (caar THTREE) 'THPROG) (SETQ THX (MEMQ (cadr THA) (cadddr (car THTREE)))))
                    (RPLACA (cdar THTREE) (cons nil THX))
                    (RETURN (THPROGT)))
                (:else (THPOPT) (GO =>)))
        nil)))

(§ DEFUN THTAE [XX]
    (COND
        ((ATOM XX) nil)
        ((EQ (car XX) 'THUSE)
            (doall (map #'(lambda (X)
                    (COND ((not (and (SETQ THXX (GET X 'THEOREM)) (EQ (car THXX) TYPE)))
                            (PRINT X)
                            (list 'THAPPLY (THERT BAD THEOREM "-THTAE") (car THX)))
                        (:else (list 'THAPPLY X (car THX)))))
                (cdr XX))))
        ((EQ (car XX) 'THTBF)
            (doall (map #'(lambda (Y)
                    (COND (((cadr XX) Y)
                        (list (list 'THAPPLY Y (car THX))))))
                (COND (THY1 THY)
                    ((SETQ THY1 true)
                        (SETQ THY (THMATCHLIST (car THX) TYPE)))))))
    (:else (PRINT XX) (THTAE (THERT UNCLEAR RECCOMMENDATION "-THTAE")))))

(§ DEFUN THTAG FEXPR [L]
    (and (car L)
        (THPUSH THTREE (list 'THTAG (car L)))))

(§ DEFUN THTAGF [] (THPOPT) nil)

(§ DEFUN THTAGT [] (THPOPT) THVALUE)

(§ DEFUN THTRUE [X] true)

(§ DEFUN THTRY1 []                                           ;; TRIES NEXT RECOMMENDATION ON TREE FOR THGOAL
    (let [THX nil THY nil THZ nil THW nil THEOREM nil]
        (SETQ THZ (car THTREE))                             ;; = (THGOAL PATTERN EXPANDED-RECOMMENDATIONS)
        (SETQ THY (cddr THZ))                               ;; = RECOMMENDATIONS
        (RPLACD THY (dec (cdr THY)))
    NXTREC (COND ((or (nil? (car THY)) (zero? (cdr THY)))
            (RETURN nil)))                                  ;; RECOMMENDATIONS EXHAUSTED. FAIL
        (SETQ THX (caar THY))
        (GO (car THX))
    THNUM (RPLACD THY (cadr THX))
        (RPLACA THY (cdar THY))
        (GO NXTREC)
    THDBF (SETQ THOLIST THALIST)
        (COND ((nil? (caddr THX))
                (RPLACA THY (cdar THY))
                (GO NXTREC))                                ;; NO MORE CANDIDATES SATISFYING THIS REC, TRY NEXT
            ((PROG1 (and ((cadr THX) (SETQ THW (caaddr THX))) (THMATCH1 (cadr THZ) (car THW)))
                    (RPLACA (cddr THX) (cdaddr THX)))
                (RETURN THW))
            (:else (GO THDBF)))
    THTBF (COND ((nil? (caddr THX))
                (RPLACA THY (cdar THY))
                (GO NXTREC)))                               ;; NO MORE CANDIDATES SATISFYING THIS REC, TRY NEXT
        (SETQ THEOREM (caaddr THX))
    THTBF1 (COND ((not (and (SETQ THW (GET THEOREM 'THEOREM)) (EQ (car THW) 'THCONSE)))
                (PRINT THEOREM)
                (COND ((EQ (SETQ THEOREM (THERT BAD THEOREM - THTRY1)) 'true)
                        (GO NXTREC))
                    (:else (GO THTBF1)))))
        (COND ((PROG1 (and ((cadr THX) (caaddr THX)) (THAPPLY1 THEOREM THW (cadr THZ)))
                    (RPLACA (cddr THX) (cdaddr THX)))
                (RETURN true))
            (:else (GO THTBF)))))

(§ DEFUN THTRY [X]
    ;; THTRY IS IN CHARGE OF MAKING UP THE "THINGS TO DO" LIST, WHICH IS PUT ON THTREE.
    ;; SO WHENEVER WE FAIL BACK TO A THGOAL, WE GO TO THE NEXT "THING TO DO".
    ;; X IS THE LIST OF RECOMMENDATIONS.
    (COND ;; ANY ATOMIC RECOMMENDATION IS IGNORED.  THIS IS USEFUL IN ERROR RECOVERY.
        ((ATOM X) nil)
        ;; HAVE A THEOREM BASE FILTER
        ((EQ (car X) 'THTBF)
            ;; MAKE UP A LIST WHICH GIVES
            ;; 1 - THE INDICATOR "THTBF"
            ;; 2 - THE ACTUAL FILTER (THTRUE IS THE MOST COMMON)
            ;; 3 - THE BUCKET RETURNED BY THMATCHLIST
            (COND ((not THZ1) (SETQ THZ1 true) (SETQ THZ (THMATCHLIST THA2 'THCONSE))))
            (COND (THZ (list (list 'THTBF (cadr X) THZ))) (:else nil)))
        ;; DO THE SAME THING, ONLY FOR DATA BASE FILTERS.
        ((EQ (car X) 'THDBF)
            (COND ((not THY1) (SETQ THY1 true) (SETQ THY (THMATCHLIST THA2 'THASSERTION))))
            (COND (THY (list (list 'THDBF (cadr X) THY))) (:else nil)))
        ;; THUSE STATEMENTS ARE TRANSLATED INTO THTBF THTRUE STATEMENTS,
        ;; WHICH THE "BUCKET" IS THE LIST GIVEN IN THE THUSE.
        ((EQ (car X) 'THUSE)
            (list (list 'THTBF 'THTRUE (cdr X))))
        ((EQ (car X) 'THNUM)
            (list X))
        (:else (PRINT X) (THTRY (THERT UNCLEAR RECOMMENDATION - THTRY)))))

(§ DEFUN THUNDOF []
    (COND ((nil? (caddar THTREE)) (THPOPT))
        (:else (SETQ THXX (cddar THTREE))
            (SETQ THALIST (caadr THXX))
            (RPLACA (cdr THXX) (cdadr THXX))
            (SETQ THTREE (caar THXX))
            (RPLACA THXX (cdar THXX))))
    nil)

(§ DEFUN THUNDOT [] (THPOPT) true)

(§ DEFUN THUNIQUE FEXPR [THA]
    (SETQ THA (cons 'THUNIQUE (doall (map #'EVAL THA))))
    (let [X nil]
        (SETQ X THALIST)
    =>  (COND ((nil? X) (THPUSH THALIST THA) (RETURN true))
            ((EQ (caar X) 'THUNIQUE)
                (COND ((EQUAL (car X) THA) (RETURN nil)))))
        (SETQ X (cdr X))
        (GO =>)))

(§ DEFUN THV1 [X]
    ;; (THV1 'X) IS THE VALUE OF THE PLANNER VARIABLE.
    ;; $?X RETURNS ERROR MESSAGE IF X UNBOUND OR UNASSIGNED.
    (SETQ THXX X)
    (COND ((EQ (SETQ X (cadr (SASSQ X THALIST #'(lambda nil (PRINT THXX) (THERT THUNBOUND - THV1))))) 'THUNASSIGNED)
            (PRINT THXX)
            (THERT THUNASSIGNED - THV1))
        (:else X)))

(§ DEFUN THV FEXPR [X]
    ;; (THV X) IS THE VALUE OF THE PLANNER VARIABLE $?X
    (THV1 (car X)))

(§ DEFUN THVAL [THEXP THALIST]
    ;; CORRESPONDS TO LISP EVAL.
    ;; THEXP IS THE EXPRESSION TO BE THVALUATED.
    ;; THALIST IS THE VARIABLE BINDING LIST.

    ;; ALL THPUSH DOES IS TO CONSE ON THE SECOND ITEM TO THE FIRST.
    (THPUSH THLEVEL (list THTREE THALIST))
    (let [THTREE nil THVALUE nil THBRANCH nil THOLIST nil THABRANCH nil THE nil THMESSAGE nil]
        (SETQ THV '(THV THNV))
        (SETQ THVALUE 'THNOVAL)

        ;; "THE" BECOMES THE CURRENT EXPRESSION.
        ;; THEXP IS RESERVED FOR FURTHER EXPRESSIONS,
        ;; WHICH SHOULD BE THVALED BEFORE WE GO TO THE NEXT ITEM OF ACTUAL CODE.
        ;; FOR EXAMPLE, THASSERT USES THIS FEATURE TO PROCESS ANTECEDENT THEOREMS.
    GO  (SETQ THE THEXP)
        (SETQ THEXP nil)
        ;; TYPING ^A (CONTROL A) AT MAC-AI LISP CAUSES ^A (UPARROW A) TO BE SET TO T.
        ;; THIS CAN BE DONE WHILE A FUNCTION IS BEING PROCESSED.
        ;; THE NET EFFECT IS TO TEMPORARILY HALT EVALUATION.
        (COND (ß_A (SETQ ß_A nil)
            (or (THERT ß_A - THVAL) (GO FAIL))))
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
                (SETQ THVALUE (THERT LISPERROR - THVAL))))
    GO1 (COND (THSTEPD (eval THSTEPD)))
        ;; USUALLY THEMESSAGE WILL BE NIL.
        ;; EXCEPTION IS WHEN USER HAS USED THE THMESSAGE FUNCTION.
        (COND (THMESSAGE (GO MFAIL))
            ;; IF THEXP IS NON NIL, IT MEANS THAT WE HAVE MORE PLANNER TO WORK ON BEFORE GOING TO NEXT LINE OF USER CODE.
            (THEXP (GO GO))
            ;; IF THVALUE IS NON NIL, IT MEANS THAT SO FAR THE THEOREM IS SUCCEEDING.
            (THVALUE (GO SUCCEED))
            ;; ELSE WE ARE IN A FAILURE SITUATION.
            (:else (GO FAIL)))
    SUCCEED ;; HANDLES SUCCESS
        (COND (THSTEPT (eval THSTEPT)))
        ;; SAVE CURRENT STATE OF THTREE AND THALIST IN CASE WE HAVE TO BACK UP.
        (COND ((nil? THBRANCH)
            (SETQ THBRANCH THTREE)
            (SETQ THABRANCH THALIST)))
        ;; IF THE THTREE IS NIL, IT MEANS THAT THE THPROG OR WHATEVER HAS BEEN COMPLETED,
        ;; SO THERE ARE NO MORE EXPRESSIONS TO DO.  ALL THEOREMS ACT LIKE A THPROG,
        ;; INCLUDING PUTTING ITS MARK ON THTREE, SEE THAPPLY, HENCE NO NEED TO GROW MORE BRANCHES ON THTREE.
        (COND ((nil? THTREE)
                (SETQ THLEVEL (cdr THLEVEL))
                (RETURN THVALUE))
            ;; THIS IS THE NORMAL CASE.
            ;; WE EVAL THE SUCCEED-FUNCTION OF THE PLANNER FUNCTION WHICH JUST SUCCEEDED.
            ((SETQ THEXP (GET (caar THTREE) 'THSUCCEED))
                (GO GO2))
            ;; IN CASE OF LOSSAGE, LETS THE USER SUCCEED ANYWAY.
            ((THERT BAD SUCCEED - THVAL)
                (GO SUCCEED))
            ((GO FAIL)))
        ;; HAS TO DO WITH FAILURE + MESSAGE
    MFAIL (COND ((EQ (car THMESSAGE) THTREE)
            (SETQ THEXP (cadr THMESSAGE))
            (SETQ THMESSAGE nil)
            (GO GO)))
    FAIL (COND (THSTEPF (eval THSTEPF)))
        ;; IF THTREE IS NIL, WE HAVE FAILED THE ENTIRE EXPRESSION.
        (COND ((nil? THTREE)
                (SETQ THLEVEL (cdr THLEVEL))
                (RETURN nil))
            ;; NORMAL CASE.  EVAL THE FAILURE FUNCTION ASSOCIATED
            ;; WITH THE PLANNER FUNCTION WHICH JUST FAILED.
            ((SETQ THEXP (GET (caar THTREE) 'THFAIL))
                (GO GO2))
            ((THERT BAD FAIL - THVAL)
                (GO SUCCEED))
            ((GO FAIL)))
        ;; THEXP AT THIS POINT IS THE APPROPRIATE SUCCESS OR FAILURE ASSOCIATED FUNCTION.
        ;; EVAL IT AND AT THE SAME TIME,
        ;; SET IT TO NIL IN CASE WE NEED THEXP FOR MORE EXPRESSIONS TO BE PROCESSED.
    GO2 (SETQ THVALUE ((PROG1 THEXP (SETQ THEXP nil))))
        ;; GO THROUGH THE ENTIRE PROCESS AGAIN.
        ;; A TYPICAL PROCESS IN SUCCESS IS TO KEEP REMOVING EXPRESSIONS FROM THTREE
        ;; UNTIL WE GET BACK TO THE THREE ENTRY PUT ON BY THPROG.
        ;; AT THIS POINT IT EVALS THPROGT, AND SEE THAT LISTING
        (GO GO1)))

(§ DEFUN THVAR [X]
    ;; PREDICATE - IS ITS INPUT A PLANNER VARIABLE
    (MEMQ (car X) '(THV THNV)))

(§ DEFUN THVARS2 [X]
    ;; THIS IS THE WORKHORSE FOR THVARSUBST.
    ;; X IS A SINGLE ITEM FROM A PATTERN.
    (let [A nil]
        (and (ATOM X) (RETURN X))
        ;; IF IT'S AN ATOM, NOTHING NEED BE DONE.
        (and (EQ (car X) 'THEV)
            (SETQ X (THVAL (cadr X) THALIST)))
        ;; IF THE EXPRESSION HAS A $E BEFORE IT, THVAL BEFORE GOING ON.
        (or (THVAR X) (RETURN X))
        ;; IF THE ITEM IS NOT A VARIABLE, IT MUST BE SOME RANDOM LIST, SO IT HAS NO ASSIGNED VALUE.
        (SETQ A (THGAL X THALIST))
        ;; AT THIS POINT X MUST BE A VARIABLE, SO FIND ITS ASSIGNMENT, THAT'S WHAT THGAL DOES.
        ;; THALIST IS WHERE THE VARIABLE ASSIGNMENTS RESIDE.
        (RETURN (COND
            ((EQ (cadr A) 'THUNASSIGNED) X)
            ;; IF THE VARIABLE IS UNASSIGNED, THEN RETURN THE ACTUAL VARIABLE.
            ((and THY (EQ (car X) 'THNV))
                ;; THY WILL BE T JUST IN THE CASES WHERE THVARSUBST WAS CALLED BY A THGOAL SITUATION.
                ;; IT IS THEN NECESSARY TO IMMEDIATELY HACK IN A THUNASSIGNED SO THAT IF THE SAME VARIABLE IS USED
                ;; TWICE IN THE SAME PATTERN, WE WON'T PUT IN ITS OLD VALUE THE SECOND TIME IT IS ENCOUNTERED.
                (THRPLACA (cdr A) 'THUNASSIGNED)
                X)
            ;; OTHERWISE THE ASSIGNMENT IS THE SECOND ELEMENT IN THE BINDING LIST.
            (:else (cadr A))))))

(§ DEFUN THVARSUBST [THX THY]
    ;; THX IS A GOAL OR ASSERTION PATTERN OR THEOREM NAME.
    ;; THIS FUNCTION RETURNS THE SAME PATTERN, EXCEPT IN PLACE OF ALL ASSIGNED VARIABLES
    ;; THERE WILL BE THE VALUES THEY ARE ASSIGNED TO.
    (COND ((EQ (car THX) 'THEV)
            ;; IF THE CAR IS THEV, IT MEANS THAT THERE WAS A $E BEFORE THE PATTERN,
            ;; IN WHICH CASE WE ARE TO GET THE REAL PATTERN BY THVALUATING WHAT IS THERE.
            (SETQ THX (THVAL (cadr THX) THALIST)))
        ((THVAR THX)
            (SETQ THX (eval THX))))
    ;; THVAR TESTS TO SEE IF ARG IS A VARIABLE.
    ;; IF THE PATTERN IS A SINGLE VARIABLE, THE PROGRAM ASSUMES THERE SHOULD BE AN IMPLICIT THVAL.
    ;; UNLESS THE ASSERTEE IS A THEOREM NAME, GO THROUGH IT PLACE BY PLACE WITH THVARS2.
    (COND ((ATOM THX) THX) (:else (doall (map #'THVARS2 THX)))))

(§ DEFUN THVSETQ FEXPR [THA]
    (let [A nil]
        (SETQ A THA)
    =>  (COND ((nil? A) (RETURN THVALUE))
            ((nil? (cdr A))
                (PRINT THA)
                (THERT ODD NUMBER OF GOODIES-THSETQ))
            (:else (SETQ THVALUE
                (car (RPLACA (cdr (THSGAL (car A))) (THVAL (cadr A) THALIST))))))
        (SETQ A (cddr A))
        (GO =>)))

(§ DEFPROP THTAG THTAGF THFAIL)
(§ DEFPROP THTAG THTAGT THSUCCEED)
(§ DEFPROP THGOAL THGOALT THSUCCEED)
(§ DEFPROP THGOAL THGOALF THFAIL)
(§ DEFPROP THFAIL? THFAIL?F THFAIL)
(§ DEFPROP THFAIL? THFAIL?T THSUCCEED)
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
(§ DEFPROP THAND THANDF THFAIL)
(§ DEFPROP THPROG THPROGF THFAIL)
(§ DEFPROP THMUNG THMUNGF THFAIL)
(§ DEFPROP THASSERT THASSERTF THFAIL)
(§ DEFPROP THERASE THERASEF THFAIL)
(§ DEFPROP THCOND THCONDF THFAIL)
(§ DEFPROP THOR THORF THFAIL)
(§ DEFPROP THDO THDOB THSUCCEED)
(§ DEFPROP THDO THDOB THFAIL)
(§ DEFPROP THUNDO THUNDOF THFAIL)
(§ DEFPROP THUNDO THUNDOT THSUCCEED)
(§ DEFPROP THMESSAGE THMESSAGEF THFAIL)
(§ DEFPROP THMESSAGE THMESSAGET THSUCCEED)
(§ DEFPROP THREMBIND THREMBINDT THSUCCEED)
(§ DEFPROP THREMBIND THREMBINDF THFAIL)

(§ DEFUN THPRINT2 [X] (PRINC \space) (PRINC X))

(§ DEFUN THERT FEXPR [ERTA]
    ;; THERT IS THE BREAK FUNCTION, AND ALSO THE TOP LEVEL FUNCTION.
    ;; IT IS CALLED DIRECTLY BY LISP BEFORE LISP GOES INTO THE READ EVAL LOOP.
    ;; FOR HOW THIS IS DONE, SEE MAC-AI LISP DOCUMENTATION.
    ;; IN ESSENCE, THERT CONTAINS ITS OWN LOOP, WHICH IS READ THVAL.
    ;; ERTA IS THE ERROR MESSAGE TO BE PRINTED OUT WHEN THERT IS USED FOR ERROR BREAKING.
    (let [LISTEN nil ß_W nil ß_Q nil]
        (PRINT '>>>)
        (COND ((dorun (map #'THPRINT2 ERTA))                ;; THE NORMAL MESSAGE PRINTOUT
            (PRINT 'LISTENING)
            (or THLEVEL (THPRINT2 'THVAL))))                ;; IF WE ARE AT TOP LEVEL, THLEVEL WILL BE NIL
    =>  (SETQ THINF nil)                                    ;; GO INTO READ LOOP
        (TERPRI)
        (ERRSET (COND
            ((EQ (SETQ LISTEN (READ)) 'P)                 ;; READ IN S EXPRESSION
                (RETURN true))                              ;; $P IMPLIES PROCEDE
            ((and (not (ATOM LISTEN))                       ;; ($P EXP) IMPLIES PROCEDE AND OUTPUT (EVAL EXP)
                    (EQ (car LISTEN) 'P))
                (RETURN (eval (cadr LISTEN))))
            (THLEVEL (PRINT (eval LISTEN)))                 ;; EVAL LISTENING IF NOT AT TOP LEVEL
            (:else (PRINT (THVAL LISTEN THALIST)))))        ;; THVAL LISTENING AT TOP LEVEL
        (GO =>)))

(§ DEFUN THINIT FEXPR [L]
    (SETQ THGENAME 0)
    (SETQ THSTEP nil)
    (SETQ THSTEPD nil)
    (SETQ THSTEPT nil)
    (SETQ THSTEPF nil)
    (SETQ THXX nil)
    (SETQ THTRACE nil)
    (SETQ THALIST '((nil nil)))
    (SSTATUS MACRO $ 'THREAD)
    (SETQ ERRLIST
        '((PRINT 'MICRO-PLANNER)
            (PRINC THVERSION)
            (SETQ ERRLIST (cddddr ERRLIST))
            (SETQ THINF nil)
            (SETQ THTREE nil)
            (SETQ THLEVEL nil)
            (THERT TOP LEVEL))))

#_(ns shrdlu.thtrac)

;; FOR PLNR 159 AND GREATER, THPRINTC CAN BE ELIMINATED

(§ DEFUN THPRINTC [X] (TERPRI) (PRINC X) (PRINC \space))

;; SYSTEM FUNCTIONS SUCH AS THGOAL, THASSERT, THERASE AND THEOREM
;; (ALL THMS) ARE TRACED IF THEY ARE ON 'THTRACE'.
;; THTRACES1 PUTS THEM THERE AND THUNTRACE TAKES THEM OFF.

;; THTRACE IS INITIALLY SET TO NIL BY TS PLNR.

(§ DEFUN THTRACE FEXPR [L] (dorun (map #'THTRACE1 L)))

(§ DEFUN THTRACE1 [X]
    (let [Y nil]
        ;; VARIETY OF POSSIBLE INPUT FORMATS TRANSFORMED TO STANDARD 3 ELEMENT LIST
        ;; (OBJECT-TO-BE-TRACED TRACE-CONDITION BREAK-CONDITION)
        (SETQ X (COND
            ((ATOM X) (list X true nil))
            ((cddr X) X)
            ((nil? (cdr X)) (PRINT X) (PRINC "BAD FORMAT") (RETURN nil))
            ((list (car X) (cadr X) nil))))
        ;; IF OBJECT-TO-BE-TRACED IS A PARTICULAR THEOREM, THEN THE TRIPLET
        ;; '(THEOREM (THSEL 'CADR) (THSEL 'CADDDR)) IS GUARANTEED TO
        ;; BE ON THTRACE IN ADDITION TO THE STANDARD TRIPLET.
        (COND ((GET (car X) 'THEOREM)
            (COND ((SETQ Y (ASSQ 'THEOREM THTRACE)) (RPLACD Y '((THSEL #'cadr) (THSEL #'caddr))))
                ((SETQ THTRACE (list X (concat '(THEOREM (THSEL #'cadr) (THSEL #'caddr)) THTRACE)))))))
        ;; THTRACE IS UPDATED.  IF THE OBJECT-TO-BE-TRACED IS ALREADY ON THTHRACE, THEN
        ;; THE TRACE AND BREAK CONDITIONS ARE UPDATED, ELSE THE WHOLE TRIPLET IS PLACED ON THTRACE.
        (COND ((SETQ Y (ASSQ (car X) THTRACE)) (RPLACD Y (cdr X)))
            ((SETQ THTRACE (cons X THTRACE))))
        (RETURN X)))

;; THUNTRACE REMOVES ELEMENTS OF ITS ARG FROM THTRACE.
;; IF NOT GIVEN ANY ARGS, THUNTRACE SETS THTRACE TO NIL.

(§ DEFUN THUNTRACE FEXPR [L]
    (COND (L (SETQ THTRACE (doall (map #'(lambda (X)
                        (COND ((MEMQ (car X) L) (PRINT X) nil) ((list X))))
                    THTRACE))))
        ((dorun (map #'PRINT THTRACE)) (SETQ THTRACE nil)))
    'DONE)

;; THTRACES IS ACTIVATED BY THGOAL, THASSERT, ... IF THTRACE IS NON-NIL,
;; THF IS SET TO THE PARTICULAR CANDIDATE FOR TRACEAGE, E.G. TO 'THGOAL
;; IF THE PLANNER FUNCTION THGOAL ACTIVATED THTRACES.
;; THL = THE INSTANTIATED ARG OF THF. SEE DESC OF X ON NEXT PAGE.

(§ DEFUN THTRACES [THF THL]
    (let [THY nil THZ nil THB nil]
        (and
            ;; THY SET TO TRIPLET ON THTRACE. IF NOT THERE, NO TRACING
            (SETQ THY (ASSQ THF THTRACE))
            ;; IF BOTH TRACE AND BREAK ARE FALSE, DON'T TRACE
            ;; SIDE EFFECT - THB SET TO VALUE OF BREAK
            (or (SETQ THB (THVAL (caddr THY) THALIST)) (THVAL (cadr THY) THALIST))
            ;; THZ IS SET TO THE TRACE FUNCTION FOR THE OBJECT-TO-BE-TRACED
            (or (SETQ THZ (GET THF 'THTRACE)) (THERT THTRACES - TRACE LOSSAG))
            ;; THE TRACE FN IS EXECUTED
            (THZ THL THB)
            ;; IF THB IS NON-NIL, BREAK
            THB
            (THERT))
        nil))

;; THE CAR OF THE TREE IS '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; THUS, THESE TWO FNS PRINT THE NAME OF THE TRACE POINT, "FAIL"-OR-"SUCCEED"
;; PRINT THVALUE IF ANY, AND FINALLY BREAK IF (THERT) IS PRESENT, THEN POP THE TREE

(§ DEFPROP THTRACES
    (lambda nil
        (PRINT (cadar THTREE))
        (PRINC "FAILED ")
        (EVLIS (cddar THTREE))
        (THPOPT)
        nil)
    THFAIL)

(§ DEFPROP THTRACES
    (lambda nil
        (PRINT (cadar THTREE))
        (PRINC "SUCCEEDED ")
        (EVLIS (cddar THTREE))
        (THPOPT)
        THVALUE)
    THSUCCEED)

;; THE TRACE FNS THBKPT, THGOAL, THEOREM, THASSERT, AND THERASE PUSH ONTO THE TREE
;; '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; X = THL = INSTANTIATED GOAL, ASSERTION OR ERASURE, NAME OF THE THM, OR MESSAGE OF THE BREAKPOINT

(§ DEFPROP THBKPT
    (lambda (X B)
        (THPUSH THTREE (list 'THTRACES (THGENS B) (and B '(THERT))))
        (THPRINTC "PASSING BKPT")
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        ;; BY SETTING THBRANCH AND THABRANCH, A TRIPLE IS CREATED BY THVAL FOR BACKTRACKING.
        ;; THEN, THE TREE IS POPPED TO PREVENT THTRACES FROM TYPING OUT THE MEANINGLESS
        ;; THAT THE BREAKPOINT SUCCEEDED.
        (SETQ THBRANCH THTREE)
        (SETQ THABRANCH THALIST)
        (THPOPT)
        (PRIN1 X))
    THTRACE)

(§ DEFPROP THGOAL
    (lambda (X B)
        (THPUSH THTREE (list 'THTRACES (THGENS G) '(and THVALUE (PRIN1 THVALUE)) (and B '(THERT))))
        (THPRINTC "TRYING GOAL")
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        (PRIN1 X))
    THTRACE)

(§ DEFPROP THEOREM
    (lambda (X B)
        (THPUSH THTREE (list 'THTRACES X '(and THVALUE (PRIN1 THVALUE)) (and B '(THERT))))
        (THPRINTC "ENTERING THEOREM")
        (PRIN1 X))
    THTRACE)

(§ DEFPROP THASSERT
    (lambda (X B)
        (THPUSH THTREE (list 'THTRACES (THGENS A) (and B '(THERT))))
        (PRINT 'ASSERTING)
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        (PRIN1 X))
    THTRACE)

(§ DEFPROP THERASE
    (lambda (X B)
        (THPUSH THTREE (list 'THTRACES (THGENS E) (and B '(THERT))))
        (PRINT 'ERASING)
        (PRIN1 (cadar THTREE))
        (PRINC \space)
        (PRIN1 X))
    THTRACE)

;; FOR THE TRACE-OBJECT 'THEOREM, IF ANY SPECIFIC THMS ARE TRACED, '(THSEL 'CADR) AND '(THSEL 'CADDDR)
;; ARE THE TRACE AND BREAK PREDICATES.  HENCE THTRACES CAUSES THESE EXPR'S TO BE THVALED.  THL IS SET
;; TO THE SECOND ARG OF THTRACES WHICH IN THIS CASE IS PRESUMABLY THE NAME OF THE PARTICULAR THM THAT
;; ACTIVATED THTRACES.  THSEL FIRST CHECKS TO SEE WHETHER THIS THM IS INDEPENDENTLY ON THTRACE. IF NOT,
;; IT DOES NO MORE.  BUT IF IT IS, THX GETS SET TO THE THM'S TRIPLET.  THEN THX GETS SET TO EITHER THE
;; TRACE (ARG = 'CADR) OR THE BREAK (ARG = 'CADDDR) CONDITION OF THE TRIPLET.  FINALLY, THESE CONDITIONS
;; ARE THVALED, THUS THSEL SERVES THE PURPOSE OF REFERENCING THE TRACE AND BREAK PREDICATES OF PARTICULAR
;; THMS ON THTRACE.

(§ DEFUN THSEL [THF]
    (let [THX nil] (RETURN (and (SETQ THX (ASSQ THL THTRACE)) (SETQ THX (THF THX)) (THVAL THX THALIST)))))

;; MAKES A NAME WITH PREFIX X AND SUFFIX A UNIQUE NUMBER.
(§ DEFUN THGENS FEXPR [X]
    (MAKNAM (concat (EXPLODE (car X)) (EXPLODE (SETQ THGENS (inc THGENS))))))

(§ SETQ THGENS 0)

#_(ns shrdlu.syscom)

;; ##################################################################
;;
;;          SYSCOM - TOPLEVEL AND GENERAL UTILITY FUNCTIONS
;;
;; ##################################################################

(§ DEFUN SHRDLU []
    (let [END nil AMB nil TIMAMB nil BOTH nil BACKREF nil BACKREF2 nil ANSNAME nil LASTREL nil WHO nil PT nil PTW nil SENT nil PUNCT nil IGNORE nil H nil N nil NB nil FE nil SM nil RE nil MES nil MESP nil C nil CUT nil CURTIME nil STATE nil GLOBAL-MESSAGE nil LEVEL nil]
        (CLEANOUT TSS EVX NODE ANS OSS RSS X)       ;; FLUSH OLD GENSYMS
    CATCH-LOOP
        (CATCH
            (do
        LOOP    (SETQ SENTNO (inc SENTNO)
                      PARSINGS 0
                      LEVEL 0
                      LASTSENTNO (inc LASTSENTNO)
                      LASTSENT C
                      GLOBAL-MESSAGE nil
                      MES 'NOPE
                      BACKREF nil)                  ;; ???????????????????
        UP      (SETQ N (SETQ SENT (ETAOIN)))
                (or ANNOYANCE (PRINT *1))
                (and ß_Q (%SENT))
                (SETQ ß_Q nil)
                (and IGNORE (GO UP))
                (COND
                    ((and
                        (COND
                            (TOPLEVEL-ERRSET? (ERRSET (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
                            (:else (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
                        C)
                    (or ANNOYANCE (PRINT *2))
                    (SETQ FE (FE C))
                    (SETQ NB SENT)
                    (SETQ H (H C))
                    (SETQ INTERPRETATION (SM C))
                    (and SH-BEFOREANSWER-PAUSE (ERT BEFORE ANSWERING))
                    (COND
                        (SMN (and SH-PARSE-PAUSE (ERT PARSING COMPLETED))
                            (GO LOOP))
                        ((not ANSWER?)
                            (and SH-PARSESMNTC-PAUSE (ERT ANALYSIS COMPLETED)))
                        ((COND
                            (TOPLEVEL-ERRSET? (ERRSET (eval '(ANSWER C))))
                            (:else (eval '(ANSWER C)))))
                        ((APPLY #'SAY (or GLOBAL-MESSAGE '(I DON'T UNDERSTAND\.))))))
                    ((PRINT *3)
                        (APPLY #'SAY (or GLOBAL-MESSAGE '(I DON'T UNDERSTAND\.)))))
            (and SH-STANDARD-PRINTOUT (SHSTPO))
            (and SH-AFTERANSWER-PAUSE (ERT))
            (GO LOOP))
        ABORT-PARSER)
        (GO CATCH-LOOP)))

(§ DEFUN PARSEVAL [A] (eval (cons 'PARSE A)))

(§ SETQ PARSEARGS '(CLAUSE MAJOR TOPLEVEL))

(§ DEFUN PARSE-STATISTICS []
    (COND ((== PARSINGS 0)
            (PUTPROP 'PARSINGS 0 'WINS)))
        (and RE
            (PUTPROP 'PARSINGS (inc (GET 'PARSINGS 'WINS)) 'WINS))
        (SETQ PARSINGS (inc PARSINGS)))

(§ DEFUN DEFLIST FEXPR [L]
    (dorun (map #'(lambda (A)
                (PUTPROP (car A) (cadr A) (car L)))
            (cdr L)))
    (car L))

(§ DEFUN %SENT []
    ;; THIS FUNCTION PRINTS THE CURRENT SENTENCE
    (TERPRI)
    (dorun (map #'PRINT3 SENT))
    (PRINC PUNCT))

(§ DEFUN DA [X]
    (and
        (GET X 'THASSERTION)
        (DISP (APPLY #'concat (doall (map #'cddr (APPLY #'concat (doall (map #'cdr (cdr (GET X 'THASSERTION)))))))))))

(§ DEFUN DISP FEXPR [A]
    (and (STATUS TTY) (TYO 12))
    (TERPRI)
    (and (cdr A)
        (PRINC (car A))
        (PRINC " >> ")
        (PRINC (cadr A))
        (TERPRI))
    (SPRINT (COND ((cdr A) (GET (car A) (cadr A))) ((eval (car A)))) LINEL 0)
    *4)

(§ DEFUN DP [X]
    (TERPRI)
    (TERPRI)
    (PRINC \[)
    (PRINC X)
    (PRINC \])
    (let [L (PLIST X)]
    A   (COND ((MEMQ (car L) '(PNAME VALUE)) (GO B)))
        (TERPRI)
        (TAB 4)
        (PRINC (car L))
        (SPRINT (cadr L) (*DIF LINEL 18) 18)
    B   (COND ((SETQ L (cddr L)) (GO A)))
        (TERPRI)
        (and DPSTOP (ERT))
        (RETURN '*)))

;; ################################################################
;;         FUNCTIONS FOR HAND-TAILORED GARBAGE COLLECTION
;; ################################################################

(§ DEFUN FORGET []
    (SETQ LASTSENT nil LASTREL nil BACKREF nil BACKREF2 nil LASTTIME nil LASTPLACE nil)
    (SETQ LASTSENTNO 0)
    (dorun (map #'(lambda (PN)
        (dorun (map #'(lambda (PROP) (REMPROP PN PROP))
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

(§ DEFUN CLEANOUT FEXPR [LIST]        ;; REMOB'S ALL GENSYMS OF THE MEMBERS OF LIST
    (dorun (map #'(lambda (A)
            (CLEANX A 0 (GET A 'MAKESYM))
            (PUTPROP A 0 'MAKESYM))
        LIST)))

(§ DEFUN CLEANX [A B C]
    ;; CLEANX REMOB'S GENSYMS OF THE SYMBOL "A" FROM B+1 UP TO AND INCLUDING C
    (let [SAVE nil I nil]
        (SETQ B (or B 0))
        (SETQ SAVE (GET A 'MAKESYM))
        (and C
            (> C B)
            (PUTPROP A B 'MAKESYM)
            (DO I B (inc I) (EQUAL I C) (REMOB (MAKESYM A))))
        (RETURN (PUTPROP A SAVE 'MAKESYM))))

;; ################################################################
;;         A MOST COMPLETE AND SOPHISTICATED BREAK PACKAGE
;; ################################################################

(§ DEFPROP THERT ERT FEXPR)

(§ DEFUN ERT FEXPR [MESSAGE] (ERTEX MESSAGE nil true))       ;; ALWAYS STOPS, NEVER CAUSES ABORTION.  USED FOR GUARENTEED STOPS AS IN DEBUGGING OR ETAOIN.

(§ DEFUN ERTERR FEXPR [MESSAGE] (ERTEX MESSAGE true nil))    ;; USED FOR KNOWN WIERD STATES SUCH AS CHOP.  USES "NOSTOP" SWITCH, CAUSES ABORTION.

(§ DEFUN BUG FEXPR [MESSAGE]
    (ERTEX (cons 'BUG!!!!!!!!!! MESSAGE) true nil))        ;; MARKES UNANTICIPATED WIERD STATES WHICH INDICATE MISTAKES IN THE CODE.

(§ DEFUN GLOBAL-ERR FEXPR [MESSAGE]
    (ERTEX (SETQ GLOBAL-MESSAGE MESSAGE) true nil))        ;; MARKES KNOWN INADEQUACIES OF THE SYSTEM.  SWITCHABLE STOP, CAUSES ABORTION.

(§ DEFUN ERTEX [MESSAGE CAUSE-ABORTION IGNORE-NOSTOP-SWITCH?]
    (let [GLOP nil EXP nil ST-BUFFER nil BUILDING-ST-FORM nil ß_W nil ß_Q nil FIRSTWORD nil]
        (and NOSTOP
            (not IGNORE-NOSTOP-SWITCH?)
            (and CAUSE-ABORTION
                (THROW CAUSE-ABORTION ABORT-PARSER))
            (RETURN true))
        (TERPRI)
        (dorun (map #'PRINT3 MESSAGE))
    PRINT
        (SETQ FIRSTWORD true ST-BUFFER nil BUILDING-ST-FORM nil)   ;; "ST" REFERS TO SHOW, TELL
        (PRINT '>>>)
    LISTEN
        (COND                                                   ;; SHELP UP SPURIOUS CHARACTERS
            ((MEMBER (TYIPEEK) '(32 10))                        ;; SP, LF
                (READCH)
                (GO LISTEN))
            ((EQ (TYIPEEK) 13)                                  ;; CR
                (COND (BUILDING-ST-FORM
                        (SETQ EXP (reverse ST-BUFFER))
                        (GO EVAL-EXP))                          ;; DELIMITER CASE
                    (:else (READCH) (GO LISTEN)))))                 ;; SPURIOUS CHARACTER CASE

        (or (ERRSET (SETQ GLOP (READ))) (GO PRINT))

        (COND ((ATOM GLOP)
            (SETQ GLOP (or (GET GLOP 'ABBREV) GLOP))
            (COND ((MEMQ GLOP '(true P nil))                     ;; LEAVE-LOOP CHARS
                    (RETURN GLOP))
                ((EQ GLOP 'GO)                                  ;; CAUSE RETURN TO READY-STATE
                    (THROW 'GO ABORT-PARSER))
                (BUILDING-ST-FORM
                    (SETQ ST-BUFFER (cons GLOP ST-BUFFER))
                    (GO LISTEN))
                ((and FIRSTWORD (MEMQ GLOP '(SHOW TELL)))
                    (SETQ BUILDING-ST-FORM true
                        ST-BUFFER (cons GLOP ST-BUFFER)
                        FIRSTWORD nil)
                    (GO LISTEN))
                (:else (SETQ EXP GLOP) (GO EVAL-EXP))))
            (:else (COND ((EQ (car GLOP) 'RETURN)
                    (RETURN (eval (cadr GLOP))))
                (:else (SETQ EXP GLOP) (GO EVAL-EXP)))))

    EVAL-EXP
        (COND (ERT-ERRSET? (ERRSET (PRINT (eval EXP))))
            (:else (PRINT (eval EXP))))
        (GO PRINT)))

(§ DEFUN COMBINATION? FEXPR [WORDS]
    ;; THIS FUNCTION CHECKS TO SEE IF THE WORDS PASSED AS ARGS FORM
    ;; A COMBINATION SUCH AS "STACK-UP" OR "ON-TOP-OF" COMBINATIONS
    ;; ARE IN THE DICTIONARY AS A SINGLE ATOM COMPOSED OF THE WORDS
    ;; IN THE COMBINATION SEPARATED BY DASHES ALL COMBINATIONS HAVE
    ;; THE FEATURE "COMBINATION" AND HAVE A ROOT WHICH IS A LIST OF
    ;; THE WORDS IN THE COMBINATION
    (let [COMBINE nil]
        (dorun (map #'(lambda (X)
            (SETQ COMBINE (concat COMBINE (cons '- (EXPLODE (eval X))))))
            WORDS))
        (SETQ COMBINE (list (INTERN (MAKNAM (cdr COMBINE)))))
        (and (ISQ COMBINE COMBINATION) (RETURN COMBINE))
        (RETURN nil)))

(§ SETQ CONSO '(B C D F G H J K L M N P Q R S T V W X Z))

(§ DEFUN FINDB [X Y]
    (COND ((nil? X) nil) ((EQ Y (cdr X)) X) (:else (FINDB (cdr X) Y))))

(§ DEFUN FROM [A B]
    (COND ((or (not A) (EQ A B)) nil) (:else (cons (WORD A) (FROM (cdr A) B)))))

(§ DEFUN MAKESYM [A]
    ;; FUNCTION MAKESYM MAKES UP A GENSYM OF ITS ARG
    (PUTPROP A (inc (or (GET A 'MAKESYM) 0)) 'MAKESYM)
    (SETQ A (MAKNAM (concat (or (GET A 'EXPLO)
                (PUTPROP A (EXPLODE A) 'EXPLO))
                (EXPLODE (GET A 'MAKESYM)))))
    (COND (MAKEINTERN (INTERN A)) (A)))

(§ DEFUN LIS2FY [X]
    (COND ((ATOM X) (list (list X))) ((ATOM (car X)) (list X)) (X)))

(§ DEFUN MEET [A MEET]
    ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
    (let [SET nil]
    =>  (COND ((nil? A) (RETURN (reverse SET)))
            ((MEMQ (car A) MEET)
                (SETQ SET (cons (car A) SET))))
        (SETQ A (cdr A))
        (GO =>)))

(§ DEFUN MOD [A B] (UNION (SETDIF A (cadr B)) (car B)))

(§ DEFUN NTH [NUM LIST]
    (COND ((ATOM LIST) (ERT NTH - ILLEGAL LIST))
        ((< NUM 1) (ERT NTH - ILLEGAL NUMBER)))
    (do
    =>  (COND ((== NUM 1) (RETURN (car LIST)))
            ((SETQ LIST (cdr LIST))
                (SETQ NUM (dec NUM))
                (GO =>))
            (:else (ERT NTH - LIST TOO SHORT)))
    nil))

(§ DEFUN PR1 [A]
    (COND ((ATOM (H A)) (list (WORD (NB A)) (FE A)))
        ((PR2 (SM A))
            (list (FROM (NB A) (N A)) (FE A) (SM A) (COND ((ATOM (H A)) \space) ((MAPLIST #'PR1 (reverse (H A)))))))))

(§ DEFUN PR2 [A]
    (or (ATOM A)
        (dorun (map #'(lambda (B)
            (and (GET B 'SM)
                (or (MEMQ B ALIST)
                    (SETQ ALIST (cons (list B (GET B 'SM) (GET B 'REFER)) ALIST)))))
            A))))

(§ DEFUN PRINT2 [X]
    (COND ((> CHRCT (FLATSIZE X)) (PRINC \space))
        (:else (TERPRI)))
    (PRINC X))

(§ DEFUN PRINT3 [X]
    (PROG2 (or (> CHRCT (FLATSIZE X)) (TERPRI)) (PRINC X) (PRINC \space)))

(§ DEFUN PRINTEXT [TEXT]
    (COND (TEXT
        (TERPRI)
        (eval (cons 'SAY (LISTIFY TEXT))))))

(§ DEFUN PRINTC FEXPR [L]
    (let [TEST nil]
        (TERPRI)
    =>  (COND ((nil? L) (RETURN nil)))
        (SETQ TEST (eval (car L)))
        (COND ((EQ TEST '<TAB>))
            (:else (PRINC TEST) (PRINC \space)))
        (SETQ L (cdr L))
        (GO =>)))

(§ DEFUN QUOTIFY [X] (list 'quote X))

(§ DEFUN SAY FEXPR [A] (dorun (map #'PRINT3 A)))

(§ DEFUN SETDIF [A SETDIF]
    (let [SET nil]
    =>  (COND ((nil? A) (RETURN (reverse SET)))
            ((MEMQ (car A) SETDIF))
            ((SETQ SET (cons (car A) SET))))
        (SETQ A (cdr A))
        (GO =>)))

(§ DEFUN STA [A B]
=>  (COND ((nil? B) (RETURN true))
        ((nil? A))
        ((EQ (car A) (car B))
            (SETQ A (cdr A))
            (SETQ B (cdr B))
            (GO =>)))
    nil)

(§ DEFUN UNION [A B]
    (let [SET nil]
        (SETQ SET (reverse A))
    =>  (COND ((nil? B) (RETURN (reverse SET)))
            ((MEMQ (car B) SET))
            ((SETQ SET (cons (car B) SET))))
        (SETQ B (cdr B))
        (GO =>)))

(§ DEFUN WALLP [A]
    (let [ALIST nil LINEL nil]
        (SETQ LINEL 72)
        (and (STATUS TTY) (TYO 12))
        (TERPRI)
        (SPRINT (list (PR1 A) (reverse ALIST)) LINEL 0)
        nil))

(§ DEFUN DEFS FEXPR [L]
    (let [A nil]
        (and (nil? (cdr L)) (RETURN L))
        (SETQ A (car L))
        (SETQ L (cdr L))
    =>  (PUTPROP A (cadr L) (car L))
        (COND ((SETQ L (cddr L)) (GO =>)))
        (RETURN A)))

(§ DEFUN TAB [N]
    (let [P nil]
        (COND ((> N LINEL) (RETURN '<TAB>)))
    =>  (SETQ P (- LINEL CHRCT))
        (COND ((not (> N P)) (RETURN '<TAB>)))
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

(§ DEFUN ETAOIN []
    (let [WORD nil NEWWORD nil CHAR nil ALTN nil ALREADY-BLGING-NEWWRD nil WRD nil LAST nil NEXT nil Y nil WORD1 nil X nil RD nil POSS nil]
    THRU (SETQ SENT (SETQ WORD (SETQ PUNCT (SETQ POSS nil))))
        (PRINT 'READY)
        (TERPRI)
    CHAR (COND ((EQUAL (TYIPEEK) 24) (READCH) (ERT) (GO THRU)) ;; "CNTRL-X" BREAK LEFT OVER FROM CMU
            ((== (TYIPEEK) 3) (BUG "ETAOIN:" ABOUT TO READ EOF)))
        ;; THIS LITTLE HACK MAPS ALL LOWERCASE LETTERS INTO UPPERCASE.
        (SETQ CHAR (COND ((> 123 (SETQ CHAR (TYI)) 96) (- CHAR 32)) ((> 91 CHAR 64) CHAR) (:else CHAR))
            CHAR (ASCII CHAR))
        (COND ((EQ CHAR \space) (GO WORD))                     ;; DELIMITER
            ((MEMQ CHAR ALTMODE)
                (SETQ CHAR (ASCII (UPPERCASE-IFY-CHAR (TYI))))
                (COND ((MEMQ CHAR ALTMODE) (ERT) (GO THRU)) ;; ALTMODE-ALTMODE
                    ((EQ CHAR 'C) (TYO 12) (GO DO))         ;; ALTMODE-C
                    ((EQ CHAR 'R) (TERPRI) (GO DO))         ;; ALTMODE-R
                    ((and (EQ CHAR 'S) SAVESENT)            ;; ALTMODE-S CAUSES THE LAST SENTENCE TYPED IN TO
                        (SETQ SENT (car SAVESENT))          ;; RETURNED AS THE SENTENCE TO BE INTERPRETED
                        (SETQ PUNCT (cdr SAVESENT))
                        (%SENT)
                        (RETURN SENT))
                    ((EQ CHAR 'N)                           ;; ALTMODE-N COMPLEMENTS THE NEWWORD FLAG, WHICH
                                                            ;; DETERMINES WHETHER UNRECOGNIZED WORDS WILL BE
                                                            ;; CONSIDERED SPELLING ERRORS OR NEW WORDS.
                        (SETQ NEWWORD (not NEWWORD) ALTN (not ALTN))
                        (GO CHAR))
                    ((EQ CHAR 'Q)                           ;; ALTMODE-Q CAUSES READIN FROM DISK FILE.
                        (SETQ ß_Q true)
                        (SETQ IGNORE nil)
                        (GO THRU))
                    ((EQ CHAR 'M)
                        (SETQ ß_Q true)
                        (SETQ IGNORE nil)
                        (GO THRU))
                    ((EQ CHAR 'I)                           ;; ALTMODE-I IGNORES SENTENCE READ FROM FILE.
                        (SETQ IGNORE true)
                        (SETQ ß_Q true)
                        (GO THRU))
                    ((GO THRU))))
            ((EQ CHAR RUBOUT)
                (COND (WORD (PRINC (car WORD)) (SETQ WORD (cdr WORD)))
                    (SENT (PRINT (car SENT)) (SETQ SENT (cdr SENT))))
                (GO CHAR))
            ((EQ CHAR CARRET) (GO WORD))
            ((MEMQ CHAR PUNCL)
                (SETQ PUNCT CHAR)                           ;; DELIMITER
                (and WORD (GO WORD))
                (GO PUNC)))
        (and
            (or (and (EQ CHAR '\") (not ALREADY-BLGING-NEWRD) (SETQ NEWWORD (SETQ ALREADY-BLGING-NEWRD true)) (GO CHAR))
                (and (EQ CHAR '\") ALREADY-BLGING-NEWRD (not (SETQ ALREADY-BLGING-NEWRD nil)) (GO WORD))
                                                            ;; WITHIN THIS "AND" ARE ALL THE CHARACTERS THAT ARE UNDERSTOOD BY THE SYSTEM
                (number? CHAR)
                (and (EQ CHAR '=) (nil? WORD))
                (MEMQ CHAR VOWEL)
                (MEMQ CHAR CONSO))
            (SETQ WORD (cons CHAR WORD)))
        (GO CHAR)

    DO  (PRINT 'READY)
        (TERPRI)
        (dorun (map #'(lambda (X) (PRINT2 X)) (reverse SENT)))
        (PRINC \space)
        (dorun (map #'PRINC (reverse WORD)))
        (GO CHAR)

    WORD (COND ((nil? WORD) (GO CHAR))
            ((EQUAL WORD '(P L E H)) (HELP) (GO THRU))
            ((and (SETQ WRD (ERRSET (READLIST (reverse WORD)))) (number? (SETQ WRD (car WRD))))
                (SETQ SENT (cons WRD SENT))
                (BUILDWORD WRD (or (and (zero? (dec WRD)) '(NUM NS)) '(NUM)) (list 'NUM WRD) nil))
                                                            ;; NO ROOT FOR NUMBERS
            ((nil? WRD) (SETQ WRD (reverse WORD)) (GO NO))
            ((GET WRD 'FEATURES))                           ;; IF A WORD HAS FEATURES, IT'S PROPERTIES ARE ALL SET UP IN THE DICTIONARY
            ((SETQ X (GET WRD 'IRREGULAR))
                (BUILDWORD WRD (MOD (GET (car X) 'FEATURES) (cdr X)) (SM X) (car X)))
            ((EQ (car (LAST WORD)) '=)
                (BUILDWORD WRD (COND ((MEMQ '\" WORD) '(PROPN NS POSS)) ('(PROPN NS))) '((PROPN T)) nil))
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
        (COND ((and (MEMQ LAST CONSO) (not (MEMQ LAST LIQUID)) (EQ LAST NEXT)) (SETQ RD (cdr RD)))
            ((EQ LAST 'I) (SETQ RD (cons 'Y (cdr RD))))
            ((or
                (and (MEMQ LAST CONSO) (MEMQ NEXT VOWEL) (not (EQ NEXT 'E)) (MEMQ (caddr RD) CONSO))
                (and (MEMQ LAST LIQUID) (MEMQ NEXT CONSO) (not (MEMQ NEXT LIQUID)))
                (and (EQ LAST 'H) (EQ NEXT 'T))
                (and (MEMQ LAST '(C G S J V Z)) (or (MEMQ NEXT LIQUID) (and (MEMQ NEXT VOWEL) (MEMQ (caddr RD) VOWEL)))))
                    (SETQ RD (cons 'E RD))))
        (GO TRY)

    LY  (COND ((and (MEMQ (car RD) VOWEL) (not (EQ (car RD) 'E)) (MEMQ (cadr RD) CONSO))
            (SETQ RD (cons 'E RD))))
        (COND ((MEMQ 'ADJ (GET (SETQ ROOT (READLIST (reverse RD))) 'FEATURES))
            (BUILDWORD WRD '(ADV VBAD) nil ROOT) ;; TEMP NIL SEMANTICS ;; ROOT IS THE ADJECTIVE
            (GO WRD)))
        (GO NO)

    SIB (SETQ LAST (car RD))
        (SETQ NEXT (cadr RD))
        (COND ((not (EQ LAST 'E)))
            ((EQ NEXT 'I) (SETQ RD (cons 'Y (cddr RD))))
            ((EQ NEXT 'X) (SETQ RD (cdr RD)))
            ((and (EQ NEXT 'H) (not (EQ (caddr RD) 'T))) (SETQ RD (cdr RD)))
            ((and (MEMQ NEXT '(S Z)) (EQ NEXT (caddr RD))) (SETQ RD (cddr RD))))
    TRY (COND
            ((or (SETQ FEATURES (GET (SETQ ROOT (READLIST (reverse RD))) 'FEATURES))
                    (and (SETQ X (GET ROOT 'IRREGULAR))
                        (SETQ FEATURES (MOD (GET (SETQ ROOT (car X)) 'FEATURES) (cdr X)))))
                (BUILDWORD WRD (MOD FEATURES (GET (car WORD) 'MOD)) (GET ROOT 'SEMANTICS) ROOT))
            ((EQ (car RD) 'E) (SETQ RD (cdr RD)) (GO TRY))
            ((GO NO)))

        ;; -------------------------------------------------------
        ;;   BUILD UP THE PROCESSED LIST OF WORDS TO BE RETURNED
        ;; -------------------------------------------------------

    WRD (SETQ SENT
            (COND (POSS
                (COND ((or (MEMQ 'NOUN (SETQ FEATURES (GET WRD 'FEATURES))) (MEMQ 'PROPN FEATURES))
                        ;; IF IT'S A NOUN OR A PROPER NOUN, MARK IT AS POSSESSIVE
                        (BUILDWORD POSS (concat (MEET FEATURES (GET 'POSS 'ELIM)) '(POSS)) (GET WRD 'SEMANTICS) ROOT)
                        (cons POSS SENT))
                    ;; CAN WE GENERALIZE IT???
                    ((BUILDWORD "'S" '(VB BE V3PS PRES) (GET 'BE 'SEMANTICS) 'BE) (cons "'S" (cons WRD SENT))))) ;; "sic!
                ((cons WRD SENT))))

    PUNC (COND (PUNCT
            (COND ((and (EQ PUNCT '?) (nil? SENT)) (HELP) (GO THRU))
                ((MEMQ PUNCT FINAL)
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

    NOGO (or (EQUAL (TYI) 10) (GO NOGO))
        (SETQ PUNCT nil WORD nil)
        (GO DO)))

(§ DEFUN PROPNAME [X] (EQ (car (EXPLODE X)) '=))

(§ DEFUN BUILDWORD [WORD FEATURES SEMANTICS ROOT]
    (PUTPROP WORD FEATURES 'FEATURES)
    (PUTPROP WORD (or SMN SEMANTICS) 'SEMANTICS)
    (and ROOT (PUTPROP WORD ROOT 'ROOT))
    WORD)

(§ DEFUN BUILDWORDLIST FEXPR [A]
    (dorun (map #'(lambda (X)
        (PRINT (BUILDWORD (car X)
            (cadr X)
            (caddr X)
            (and (cdddr X) (cadddr X)))))
        A)))

(§ SETQ CARRET (ASCII 13))

(§ SETQ FINAL '(\. ? !))

(§ SETQ CONSO '(B C D F G H J K L M N P Q R S T V W X Z))

(§ SETQ LIQUID '(L R S Z V))

(§ SETQ PUNCL '(\. ? \: \; \" !))

(§ SETQ RUBOUT (ASCII 127))

(§ DEFUN UNDEFINED [] (PRINC (WORD N)) (ERT UNDEFINED))

(§ DEFUN UPPERCASE-IFY-CHAR [CHAR] (COND ((> 123 CHAR 96) (- CHAR 32)) (:else CHAR)))

(§ SETQ VOWEL '(nil A E I O U Y))

#_(ns shrdlu.show)

;; QUICKIES

(§ DEFUN SHSTPO [] ;; "SH-STANDARD-PRINTOUT"
    (PARSINGS))

(§ DEFUN PARSINGS []
    (PRINTC "  RATIO OF WINNING PARSES TO TOTAL ")
    (PRINC (GET 'PARSINGS 'WINS))
    (PRINC '\/)
    (PRINC PARSINGS))

(§ DEFUN PARSETRACE LABELS (COND ((== (ARG nil) 0) (SETQ PARSETRACE 'ALL)) (:else (SETQ PARSETRACE (LISTIFY LABELS)))))

(§ DEFUN PARSEBREAK LABELS (COND ((== (ARG nil) 0) (SETQ PARSEBREAK 'ALL)) (:else (SETQ PARSEBREAK (LISTIFY LABELS)))))

(§ DEFUN SMNTRACE OFF? (COND ((== (ARG nil) 1) (SETQ SMNTRACE nil)) (:else (SETQ SMNTRACE true))))

(§ DEFUN SMNBREAK OFF? (COND ((== (ARG nil) 1) (SETQ SMNBREAK nil)) (:else (SETQ SMNBREAK true))))

(§ DEFUN LBK FEXPR [LABELS] (SETQ LABELBREAK LABELS))

(§ DEFUN LABELTRACE FEXPR [A]
    (dorun (map #'(lambda (X)
        (let [BODY nil]
            (PRINT X)
            (COND ((GET X 'LABELTRACED) (PRINC 'ALREADY-) (GO TRACED))
                ((GET X 'INTERPRET) (SETQ BODY (cdr (GET X 'INTERPRET))))
                ((GET X 'EXPR) (SETQ BODY (cddr (caddr (GET X 'EXPR)))))
                (:else (PRINC "CAN'T BE-") (GO TRACED)))
            (MAP #'(lambda (Y)
                    (and (ATOM (car Y))
                        (RPLACD Y (cons (list 'PASSING (list 'quote (car Y))) (cdr Y)))))
                BODY)
            (PUTPROP X true 'LABELTRACED)
        TRACED
            (PRINC 'LABELTRACED)
            nil))
        A)))

(§ DEFUN PASSING [A]
    (SETQ LASTLABEL A)
    (and (COND
            ((ATOM LABELTRACE) (and LABELTRACE (PRINT 'PASSING) (PRINC A)))
            ((MEMQ A LABELTRACE) (PRINT 'PASSING) (PRINC A)))
        (COND
            ((ATOM LABELBREAK) (and LABELBREAK (ERT LABELBREAK)))
            ((MEMQ A LABELBREAK) (ERT LABELBREAK)))))

(§ SETQ LABELTRACE nil)

(§ SETQ LABELBREAK nil)

(§ DEFUN UNLABELTRACE FEXPR [A]
    (dorun (map #'(lambda (X)
        (let [BODY nil]
            (PRINT X)
            (COND
                ((not (GET X 'LABELTRACED)) (PRINC "ISN'T ALREADY-") (GO TRACED))
                ((GET X 'INTERPRET) (SETQ BODY (cdr (GET X 'INTERPRET))))
                ((GET X 'EXPR) (SETQ BODY (cddr (caddr (GET X 'EXPR)))))
                (:else (PRINC "CAN'T BE-") (GO TRACED)))
            (MAP #'(lambda (Y) (and (ATOM (car Y)) (RPLACD Y (cddr Y)))) BODY)
            (PUTPROP X nil 'LABELTRACED)
            (PRINC 'UN)
        TRACED
            (PRINC 'LABELTRACED)
            nil))
        A)))

(§ DEFS TELLABLE
    TELL
    #'(lambda (X) (APPLY #'TELLABLE
        (list (CHARG X "CONCEPT:"
            '(ANY PLANNER GOAL PATTERN BEGINNING WITH THIS CONCEPT NAME CAN BE ACCEPTED BY THE SYSTEM AS NEW INFORMATION
            -- BEWARE OF INTERACTIONS WITH SPECIAL HACKS FOR LOCATION\, ETC\.))))))

(§ DEFUN PEV [EV COL TOP]
    (TERPRI)
    (TAB COL)
    (PRINC EV)
    (PRINC "  ")
    (PRINC (GET EV 'TYPE))
    (PRINC "  TIME: ")
    (PRINC (GET EV 'START))
    (PRINC " TO ")
    (PRINC (GET EV 'END))
    (and TOP
        (PRINC " REASON: ")
        (PRINC (GET EV 'WHY)))
    (dorun (map #'(lambda (X) (and (EQ EV (GET X 'WHY)) (PEV X (+ COL 8) nil)))
        (reverse EVENTLIST))))

(§ DEFS EVENT
    SHOW
    (lambda (X)
        (SETQ X (CHARG X "EVENT:" '(EVENT TO BE DISPLAYED --<LF> FOR ENTIRE EVENT LIST)))
        (COND (X (PEV X 0 true))
            (:else (dorun (map #'(lambda (Y) (and (EQ 'COMMAND (GET Y 'WHY)) (PEV Y 0 true)))
                (reverse EVENTLIST)))))))

(§ DEFUN ABBREVIATE FEXPR [A]
    (dorun (map #'(lambda (X)
        (PUTPROP (READLIST (doall (map #'(lambda (X Y) X) (EXPLODE X) '(true true)))) X 'ABBREV))
        A))
    'DONE)

(§ ABBREVIATE
    SHOW TELL LISP PLANNER PARSING DEFINITIONS SCENE INPUT RUN SEMANTICS PROPERTY FUNCTION VALUE ASSERTIONS THEOREM SCENE ACTION NODE TREE LABEL ATTEMPT UNIT WORD MARKER ALL REST CURRENT STOP DO)

(§ DEFUN SHOWSCENE [X]
    (let [PLANNERSEE nil]
        (TERPRI)
        (TAB 16)
        (PRINC "CURRENT SCENE")
        (TERPRI)
        (TERPRI)
        (dorun (map #'(lambda (OBJ)
            (PRINT OBJ)
            (PRINC "-->  ")
            (EVLIS (car (NAMEOBJ OBJ 'DESCRIBE)))
            (PRINC " AT ")
            (PRINC (cadr (ASSOC OBJ ATABLE)))
            (and (SETQ OBJ (THVAL '(THFIND ALL $?X (X) (THGOAL (!SUPPORT $?OBJ $?X))) (list (list 'OBJ OBJ))))
                    (TAB 13)
                    (PRINC "SUPPORTS ")
                    (PRINC OBJ)))
                '(:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B10 :BOX)))
        (TERPRI)
        (SAY THE HAND IS GRASPING)
        (PRINC \space)
        (PRINC (COND
            ((SETQ OBJ (THVAL '(THGOAL (!GRASPING $_X)) '((X THUNBOUND)))) (cadar OBJ))
            (:else 'NOTHING)))
        nil))

(§ DEFUN TELLCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE))

(§ DEFUN SHOWCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE))

(§ DEFUN SHOWTELL [A NODE SYSTEMS INFO ACTION]
    (COND ((nil? A) (SHOWTELLCHOICE))
        ((GET (car A) ACTION)
            (APPLY (GET (car A) ACTION) (list A)))
        ((PRINTEXT '(I DON'T KNOW HOW TO))
            (PRINT2 ACTION)
            (PRINT2 (car A))))
    '*)

(§ DEFUN SHOWTELLCHOICE []
    (APPLY (GET (SETQ NODE (QUERY '(WHICH OPTION?)
                    (PRINT (GET NODE SYSTEMS))
                    (GET NODE INFO)))
        ACTION)
        (list (list NODE))))

(§ DEFUN SUBLEAF [KID DAD]
    (CATCH (and (dorun (map #'SUBL2 (GET DAD SYSTEMS))) nil)))

(§ DEFUN SUBL2 [X]
    (COND ((EQ X KID) (THROW true))
        (:else (dorun (map #'SUBL2 (GET X SYSTEMS))))))

(§ DEFUN QUERY [TEXT CHOICES HELP]
    (let [EXPL nil CH2 nil EX2 nil CH3 nil EX3 nil CHAR nil NOTINIT nil]
        (SETQ EXPL (doall (map #'EXPLODE (cons 'QUIT CHOICES))))
    TOP (SETQ CH2 (cons 'QUIT CHOICES) EX2 EXPL)
        (PRINTEXT TEXT)
    READ (COND ((MEMBER (SETQ CHAR (READCH)) BREAKCHARS)
            (COND ((not NOTINIT) (GO READ))
                ((cdr CH2) (TYO 7) (GO READ))
                (:else (dorun (map #'PRINC (car EX2)))
                    (and (EQ (car CH2) 'QUIT)
                    (ERR nil))
                    (RETURN (car CH2)))))
            ((EQ CHAR (ASCII 10)) (GO READ))
            ((EQ CHAR '?) (PRINTEXT HELP) (GO CHOICES)))
        (SETQ CH3 nil EX3 nil)
        (dorun (map #'(lambda (X Y)
                (and (EQ CHAR (car X))
                    (SETQ CH3 (cons Y CH3))
                    (SETQ EX3 (cons (cdr X) EX3))))
            EX2 CH2))
        (and CH3
            (SETQ EX2 EX3 CH2 CH3)
            (SETQ NOTINIT true)
            (GO READ))
    GO  (or (MEMBER (READCH) BREAKCHARS) (GO GO))
    CHOICES
        (PRINTEXT '(THE CHOICES "ARE:"))
        (PRINT CHOICES)
        (GO TOP)))

(§ DEFUN REQUEST [TEXT HELP]
    (let [X nil]
    TOP (PRINTEXT TEXT)
    READ (COND
            ((MEMBER (ASCII (TYIPEEK)) BREAKCHARS) (READCH) (GO READ))
            ((EQUAL (TYIPEEK) 10) (READCH) (RETURN nil))
            ((EQ (ASCII (TYIPEEK)) '?) (READCH) (PRINTEXT (or HELP '(NO INFORMATION AVAILABLE))) (GO TOP))
            ((EQ (SETQ X (READ)) 'QUIT) (ERR nil))
            (:else (RETURN X)))
    nil))

(§ DEFUN SHOWPROP [X]
    (COND ((nil? X)
            (SHOWPROP (cons
                (REQUEST "ATOM:"
                    '(THE NAME OF THE ATOM WHOSE PROPERTY (IES) YOU WANT TO EXAMINE))
                (LISTIFY (REQUEST "PROPERTY:"
                    '(THE PROPERTY (IES) YOU WANT TO SEE\.  A LINE FEED MEANS ALL PROPERTIES OF THE ATOM))))))
        ((cdr X) (APPLY #'DISP X))
        (:else (let [DPSTOP nil] (DP (car X)) nil))))

(§ DEFUN TELL FEXPR [A]
    (SHOWTELL A 'CANTELL 'TELLTREE 'TELLINFO 'TELL))

(§ DEFUN TREEPRINT [ROOT TR COL]
    (TERPRI)
    (TAB COL)
    (PRINC ROOT)
    (dorun (map #'(lambda (X) (TREEPRINT X TR (+ COL 8)))
        (GET ROOT TR)))
    '*)

(§ DEFUN CHARG [X TEXT HELP]
    (COND ((cdr X) (cadr X)) (:else (REQUEST TEXT HELP))))

(§ DEFUN SHOW FEXPR [A]
    (SHOWTELL A 'CANSHOW 'SHOWTREE 'SHOWINFO 'SHOW))

(§ DEFS CANSHOW
    SHOWTREE
    (SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT)
    SHOWINFO
    (THINGS WHICH CAN BE DISPLAYED)
    SHOW
    SHOWCHOICE)

(§ DEFS CANTELL
    TELLTREE
    (LISP PLANNER PARSING DEFINITIONS SEMANTICS)
    TELLINFO
    (THINGS WHICH CAN BE SET TO CONTROL HOW THE SYSTEM RUNS)
    TELL
    TELLCHOICE)

(§ DEFS SHOW
    SHOW
    (lambda (X) (TREEPRINT 'CANSHOW 'SHOWTREE 0)))

(§ DEFS TELL
    SHOW
    (lambda (X) (TREEPRINT 'CANTELL 'TELLTREE 0)))

(§ DEFS LISP
    SHOWTREE
    (PROPERTY FUNCTION VALUE)
    TELLTELLCHOICE
    TELLTREE
    (FUNCTION)
    SHOW
    SHOWCHOICE)

(§ DEFS DO
    TELL
    #'(lambda (X) (PRINTEXT '(not YET DEFINED))))

(§ DEFS STOP
    TELL
    (lambda (X)
        (SETQ DPSTOP (ONOFF X '(STOP AFTER DISPLAYING EACH NODE AND SEMANTIC STRUCTURE?)))
        (SETQ PLANNERSEE (and PLANNERSEE
            (COND ((ONOFF X '(STOP AFTER SHOWING PLANNER INPUT?)) true) ('NOSTOP))))))

(§ DEFS PLANNER
    SHOWTREE
    (ASSERTIONS THEOREM SCENE EVENT)
    SHOW
    SHOWCHOICE
    TELLTREE
    (INPUT ACTION THEOREM ASSERTIONS TELLABLE)
    TELL
    (lambda (X)
        (COND ((nil? (cdr X)) (TELLCHOICE X))
            ((EQ (cadr X) 'ON)
                (do (SETQ ß_W true) (CLEAR-OUTPUT true))
                (THTRACE THEOREM THASSERT THERASE (THGOAL true true))
                (SETQ PLANNERSEE true))
            ((EQ (cadr X) 'OFF)
                (do (SETQ ß_W true) (CLEAR-OUTPUT true))
                (SETQ PLANNERSEE nil)
                (THUNTRACE))
            (:else (TELLCHOICE X)))
        (SETQ ß_W nil)))

(§ DEFS PARSING
    SHOWTREE
    (NODE TREE)
    SHOW
    SHOWCHOICE
    TELLTREE
    (NODE LABEL ATTEMPT)
    TELL
    (lambda (X)
        (COND ((nil? (cdr X)) (TELLCHOICE X))
            ((EQ (cadr X) 'ON)
                (do (SETQ ß_W true) (CLEAR-OUTPUT true))
                (SETQ PARSENODE-SEE true LABELTRACE true)
                (TRACE CALLSM PARSE))
            ((EQ (cadr X) 'OFF)
                (do (SETQ ß_W true) (CLEAR-OUTPUT true))
                (SETQ PARSENODE-SEE nil LABELTRACE nil)
                (UNTRACE CALLSM PARSE))
            (:else (TELLCHOICE X)))
        (SETQ ß_W nil)))

(§ DEFS DEFINITIONS
    SHOWTREE
    (UNIT WORD MARKER)
    SHOW
    SHOWCHOICE
    TELL
    TELLCHOICE
    TELLTREE
    (WORD MARKER))

(§ DEFS INPUT
    TELL
    (lambda (X) (SETQ PLANNERSEE (ONOFF X '(TO SEE INPUT TO PLANNER))))
    SHOW
    SHOWCHOICE
    SHOWTREE
    (ALL REST CURRENT))

(§ DEFS SEMANTICS
    TELL
    (lambda (X)
        (SETQ SMN nil BUILD-SEE true SMN-STOP true)
        (COND
            ((EQ (QUERY '(DO SEMANTIC ANALYSIS?) '(YES NO) nil) 'NO)
                (SETQ SMN true))
            ((EQ (QUERY '(SHOW BUILDING OF SEMANTIC STRUCTURES?) '(YES NO) nil) 'NO)
                (SETQ BUILD-SEE nil))
            ((EQ (QUERY '(STOP AFTER DISPLAYING SEMANTIC STRUCTURES?) '(YES NO) nil) 'NO)
                (SETQ SMN-STOP nil)))))

(§ DEFS RUN
    TELLTREE
    (STOP DO)
    TELL
    TELLCHOICE
    TELLINFO
    '(PARAMETERS TO CONTROL WHAT SHRDLU DOES AS IT RUNS))

(§ DEFS PROPERTY
    SHOW
    (lambda (X) (SHOWPROP (cdr X))))

(§ DEFS VALUE
    SHOW
    (lambda (X) (DISP (eval (CHARG X "EXPRESSION:" '(EXPRESSION TO BE EVALUATED BY THE LISP INTERPRETER))))))

(§ DEFS FUNCTION
    TELL
    (lambda (X) (SETQ X (list (CHARG X
                    "FUNCTION:"
                    '(LISP FUNCTION WHOSE ACTION IS TO BE TRACED))
            (COND ((and (cdr X) (cddr X) (MEMQ (caddr X) '(TRACE BREAK UNTRACE UNBREAK))) (caddr X))
                (:else (QUERY '(TRACE BREAK UNTRACE OR UNBREAK?)
                    '(TRACE BREAK UNTRACE UNBREAK)
                    '(TRACE CAUSES PRINTOUT ON ENTRY AND EXIT OF FUNCTION\.
                        BREAK CAUSES LISP TO STOP ON ENTRY AND EXIT\,
                        ACCEPTING USER COMMANDS AND CONTINUING WHEN <CONTROL X> IS TYPED\.))))))
        (APPLY (SUBST 'WBREAK 'BREAK (cadr X)) (list (car X))))
    SHOW
    (lambda (X) (APPLY #'GB
            (list (CHARG X
                    "FUNCTION:"
                    '(LISP FUNCTION WHOSE LISP DEFINITION IS TO BE SHOWN))))))

(§ DEFS ASSERTIONS
    TELL
    (lambda (X) (THVAL (list 'THASSERT
                (CHARG X
                    "ASSERTION:"
                    '(PLANNER ASSERTION TO BE ADDED TO DATA BASE))
                '(THTBF THTRUE))
            nil))
    SHOW
    (lambda (X) (DA (CHARG X
                "ATOM:"
                '(SHOW ALL ASSERTIONS WHICH CONTAIN THE GIVEN ATOM)))))

(§ DEFS THEOREM
    TELL
    DEFINETHEOREM
    SHOW
    (lambda (X) (DISP (GET (CHARG X "THEOREM-NAME:" '(PLANNER THEOREM WHOSE DEFINITION IS TO BE SHOWN)) 'THEOREM))))

(§ DEFS NODE
    TELL
    (lambda (X) (SETQ PARSENODE-SEE true NODE-STOP true)
        (COND
            ((EQ (QUERY '(SEE SUCCESSFUL PARSE NODES BEING BUILT?) '(YES NO) nil) 'NO)
                (SETQ PARSENODE-SEE nil))
            ((EQ (QUERY '(STOP AFTER DISPLAY OF NODES?) '(YES NO) nil) 'NO)
                (SETQ NODE-STOP nil))))
    SHOW
    (lambda (X)
        (COND ((GET (cadr X) 'FEATURES) (DP (cadr X)))
            ((SHOWMOVE (cdr X)) (let [DPSTOP nil] (DP (car PT)) nil) (RESTOREPT))
            (:else (SAY NO SUCH NODE)))))

(§ DEFS TREE
    SHOW
    (lambda (X)
        (COND ((GET (cadr X) 'FEATURES) (WALLP (list (cadr X))))
            ((SHOWMOVE (cdr X)) (WALLP PT) (RESTOREPT))
            (:else (SAY NO SUCH NODE)))))

(§ DEFS UNIT
    SHOW
    (lambda (X)
        (APPLY #'DG (or (cdr X) (list (REQUEST "UNIT:" '(GRAMMAR UNIT WHOSE PROGRAM IS TO BE EXAMINED -- "E.G."  CLAUSE NG PREPG VG ADJG)))))))

(§ DEFS WORD
    SHOW
    (lambda (X) (DP (CHARG X "WORD:" '(ENGLISH WORD IN THE VOCABULARY))))
    TELL
    (lambda (X)
        (APPLY #'DEFINE (list (CHARG X "WORD:" '(ENGLISH WORD TO BE DEFINED -- MUST BE NOUN OR VERB))))))

(§ DEFS ACTION
    TELL
    (lambda (X)
        (COND ((cdr X)
                (COND ((EQ (cadr X) 'ON) (SETQ X nil))
                    ((EQ X 'OFF) (SETQ X '(THUNTRACE)))))
            ((ONOFF X '(WATCH PLANNER PROGRAMS STEP BY STEP?))
                (SETQ X nil))
            (:else (SETQ X '(THUNTRACE))))
        (COND (X (THUNTRACE))
            (:else (APPLY #'THTRACE '(THEOREM THGOAL THASSERT THERASE))))))

(§ DEFS LABEL
    TELL
    (lambda (X)
        (or (cdr X)
            (SETQ X (list (REQUEST '(TYPE LIST OF LABELS\, OR ON OR "OFF:") '(WATCHES PARSER GO PAST PROGRAM LABELS IN THE GRAMMAR)))))
        (SETQ LABELTRACE (COND ((EQ (car X) 'OFF) nil) (:else (car X))))))

(§ DEFS ATTEMPT
    TELL
    (lambda (X) (COND
        ((ONOFF X '(TO SEE ALL ATTEMPTS TO PARSE SYNTACTIC UNITS\, INCLUDING FAILURES))
            (TRACE PARSE)
            (TRACE CALLSM))
        (:else (UNTRACE PARSE)))))

(§ DEFUN SHOWMOVE [X]
    (SETQ SAVEPT PT)
    (APPLY #'MOVE-PT
        (LISTIFY (or X
            (REQUEST "NODE-SPECIFICATION:"
                '(C MEANS CURRENT NODE -- H IS MOST RECENTLY PARSED FOR OTHER POSSIBILITIES\, SEE THESIS SECTION ON POINTER-MOVING COMMANDS))))))

(§ DEFUN ONOFF [ARG HELP]
    (COND
        ((EQ (cadr ARG) 'ON) true)
        ((EQ (cadr ARG) 'OFF) nil)
        ((EQ 'ON (QUERY '(ON OR OFF?) '(ON OFF) HELP)))))

(§ DEFUN DEFINETHEOREM [X]
    (PUTPROP (COND ((cdr X) (SETQ X (cadr X))) (:else (SETQ X (MAKESYM 'THEOREM))))
        (concat (list (QUERY '(WHICH THEOREM TYPE?)
                        '(THANTE THERASING THCONSE)
                        '(ANTECEDENT\, ERASING\, OR CONSEQUENT THEOREM))
                    (LISTIFY (REQUEST "VARIABLE-LIST:"
                            nil))
                    (REQUEST "PATTERN:"
                        '(A LIST ENCLOSED IN PARENS\, LIKE (!IS $?X !ZOG)))
                    (REQUEST "BODY:"
                        '(list OF MICROPLANNER STAEMENTS))))
        'THEOREM)
    (THADD X nil)
    (PRINT X))

(§ DEFS MARKER
    TELL
    (lambda (X)
        (let [Y nil]
            (PUTPROP (SETQ X (CHARG X "MARKER:" '(MARKER TO BE ADDED)))
                (list (SETQ Y (REQUEST "PARENT:" '(NODE TO WHICH IT IS ATTACHED IN THE TREE))))
                'SYS)
            (PUTPROP Y
                (cons X (GET Y 'SYSTEM))
                'SYSTEM)
            nil))
    SHOW
    (lambda (X)
        (TREEPRINT (or (CHARG X "MARKER:" '(SEMANTIC MARKER WHOSE SUBSETS ARE TO BE EXAMINED\. TYPE <LF> FOR ENTIRE TREE\.)) '!SYSTEMS)
            'SYSTEM
            0)))

(§ DEFS ALL SHOW (lambda (X) (%SENT)))

(§ DEFS CURRENT SHOW (lambda (X) (PRINTEXT (FROM NB N))))

(§ DEFS REST SHOW (lambda (X) (PRINTEXT N)))

(§ DEFS SCENE SHOW SHOWSCENE)

(§ DEFUN DEFINE FEXPR [A]
    (let [FE nil TYPE nil MARK nil REST nil TR nil]
        (SETQ A (COND (A (car A)) (:else (REQUEST "WORD:" '(ENGLISH WORD TO BE DEFINED)))))
        (SETQ TYPE (QUERY '(NOUN OR VERB?) '(NOUN VERB) '(OTHER TYPES MUST BE DEFINED IN LISP)))
    MAR (or (SETQ MARK (REQUEST 'MARKERSß '(list OF SEMANTIC MARKERS FOR WORD BEING DEFINED - TO SEE MARKER TREE TYPE <LF>)))
            (and (SHOW MARKER !SYSTEMS) (GO MAR)))
        (SETQ MARK (LISTIFY MARK))
        (COND
            ((EQ TYPE 'NOUN)
                (PUTPROP A '(NOUN NS) 'FEATURES)
                (PUTPROP A
                    (list (list 'NOUN (list 'OBJECT (list 'MARKERSß MARK
                        'PROCEDUREß
                        (LIS2FY (REQUEST 'PROCEDUREß
                                    '(EXPRESSION OR LIST OF EXPRESSIONS TO BE PUT IN PLANNER GOALS TO DESCRIBE OBJECT - USE *** TO REPRESENT OBJECT BEING DESCRIBED BY WORD
                                        -- "E.G."  (!IS *** !ZOG) OR ((!IS *** !ZOG) (!LOVE :EVERYONE ***)))))))))
                    'SEMANTICS)
                (RETURN true))
            ((SETQ TR (EQ (QUERY '(TRANSITIVE OR INTRANSITIVE?) '(TRANSITIVE INTRANSITIVE) nil) 'TRANSITIVE))
                (PUTPROP A '(VB TRANS INF) 'FEATURES))
            (:else (PUTPROP A '(VB ITRNS INF) 'FEATURES)))
        (SETQ REST (list (list (LISTIFY (REQUEST '(RESTRICTIONS ON "SUBJECT:") '(list OF SEMANTIC MARKERS))))))
        (and TR
            (SETQ REST (concat REST (list (LISTIFY (REQUEST '(RESTRICTIONS ON "OBJECT:") '(list OF SEMANTIC MARKERS)))))))
        (PUTPROP A
            (list (list 'VB (list 'RELATION (list
                'MARKERSß MARK
                'RESTRICTIONSß REST
                'PROCEDUREß
                    (LIS2FY (REQUEST 'PROCEDUREß
                                '(list OF EXPRESSIONS TO BE PUT INTO PLANNER GOALS TO DESCRIBE ACTION OR RELATION -- USE !1 FOR SUBJECT\, !2 FOR OBJECT\.
                                    "E.G."  (!SUPPORT !1 !2) OR ((!HAPPY !1) (!SMILING !1)))))))))
            'SEMANTICS)
        (RETURN true)))

(§ DEFUN HELP []
    (COND ((EQ 'S (QUERY '(TYPE L FOR LONG FORM (85 LINES) S FOR SHORT (16 LINES)) '(S L) nil))
            (UREAD MINIH DOC DSK LANG))
        (:else (UREAD HELP DOC DSK LANG)))
    (THRUTEXT)
    '*)

(§ DEFUN LIS2FY [X]
    (COND ((ATOM X) (list (list X))) ((ATOM (car X)) (list X)) (X)))

#_(ns shrdlu.setup)

;; ################################################################
;;
;;            SETUP - INITIALIZATION FILE FOR SHRDLU
;;
;; ################################################################

(§ SETQ PARSINGS 0) ;; ATOM USED IN THE TIMING PACKAGE

(§ SETQ SAVESENT nil
      DOT (ASCII 46)
      *1 "[1]"
      *2 "[2]"
      *3 "[3]"
      *4 "[4]"
      *5 "[5]"
      *6 "[6]"
      LASTSENTNO 0
      SENTNO 1
      UNMKD '(COMPONENT BOTH)
      LASTIME nil)

(§ SETQ DPSTOP nil
      NODE-STOP nil
      SMN-STOP nil
      ALTMODE (list (ASCII 27))
      BREAKCHARS (list (ASCII 32) (ASCII 13) (ASCII 46))
      LINEL 65)

(§ or (GET 'CLAUSE 'SUBR)
    (LABELTRACE CLAUSE NG VG ADJG PREPG CONJOIN))

;; ######################################################################
;;
;;                SWITCHES AND SWITCH-SETTING PACKAGES
;;
;; ######################################################################

(§ SETQ FEATURESWITCHES '(DISCOURSE NOMEM IASSUME TIMID))

(§ SETQ PAUSESWITCHES '(ANS-AFTEREVALUATION-PAUSE ANS-AFTERFORMULATION-PAUSE EVALANS-PAUSE SH-SENT-PAUSE SH-BEFOREANSWER-PAUSE SH-FINISHED-PAUSE PNS-BK PLNRSEE-PAUSE))

(§ SETQ CONTROLSWITCHES '(NOSTOP ANSWER? SMN TOPLEVEL-ERRSET? ERT-ERRSET? MAKEINTERN))

(§ SETQ DISPLAYSWITCHES '(PARSETRACE PARSEBREAK PARSENODE-SEE LABELTRACE MAKE-VERBOSE LABELBREAK BUILDSEE BUILD-SEE PLANNERSEE))

;; ************************

(§ SETQ MAKE-VERBOSE nil
      PARSETRACE nil
      PARSEBREAK nil
      PARSENODE-SEE nil
      LABELTRACE nil
      LABELBREAK nil
      BUILDSEE nil
      BUILD-SEE nil
      PLANNERSEE nil)

(§ SETQ DISCOURSE true
      WANT-DISPLAY nil
      NOMEM nil
      IASSUME true
      TIMID 200)

(§ SETQ MAKEINTERN nil)

(§ SETQ SH-BEFOREANSWER-PAUSE nil
      ANS-AFTEREVALUATION-PAUSE nil
      ANS-AFTERFORMULATION-PAUSE nil
      EVALANS-PAUSE nil
      NOSTOP nil
      ANSWER? true
      SMN nil
      DOIT nil
      TOPLEVEL-ERRSET? nil
      ERT-ERRSET? true
      SH-PARSE-PAUSE nil
      SH-PARSESMNTC-PAUSE nil
      SH-AFTERANSWER-PAUSE nil
      PNS-BK nil
      PLNRSEE-PAUSE nil)

;; **********************************

(§ DEFUN QUIETMODE []
    (dorun (map #'(lambda (X) (SET X nil)) DISPLAYSWITCHES)))

(§ DEFUN NOPAUSES []
    (dorun (map #'(lambda (X) (SET X nil)) PAUSESWITCHES)))

(§ DEFUN NORMALFEATUREMODE []
    (SETQ DISCOURSE true NOMEM nil IASUME true TIMID 200))

(§ DEFUN USERMODE []
    (QUIETMODE)
    (NORMALFEATUREMODE)
    (NOPAUSES)
    (SETQ NOSTOP true ANSWER? true SMN nil TOPLEVEL-ERRSET? true ERT-ERRSET true)
    (SETQ ß_D nil))

(§ DEFUN DEBUGMODE []
    (QUIETMODE)
    (NORMALFEATUREMODE)
    (NOPAUSES)
    (SETQ NOSTOP nil ANSWER? true SMN nil TOPLEVEL-ERRSET? nil ERT-ERRSET true)
    (SETQ ß_D true))

;; #################################################################
;;
;;                     INITIALIZATION ROUTINES
;;
;; #################################################################

(§ DEFUN INITIALSTUFF [VERSION DATE NOTE]
    (SUSPEND)
    (CURSORPOS 'C)
    (TERPRI)
    (PRINC "SHRDLU VERSION ")
    (PRINC VERSION)
    (PRINC "   ")
    (PRINC "LOADED ")
    (PRINC DATE)
    (PRINC \space)
    (PRINC "IN BLISP ")
    (PRINC (STATUS LISPVERSION))
    (TERPRI)
    (SAY REFER COMMENTS AND QUESTIONS TO DDM)
    (TERPRI)
    (TERPRI)
    (and NOTE (do (TERPRI) (APPLY #'SAY NOTE) (TERPRI) (TERPRI)))
    (SAY YOU ARE NOW IN A READ-EVAL-PRINT LOOP)
    (TERPRI)
    (SAY TYPE "\"GO \"" TO ENTER READY STATE) ;; "sic!
    (CATCH (ERT) ABORT-PARSER)
    (SSTATUS TOPLEVEL '(SHRDLU))
    (SHRDLU))

(§ DEBUGMODE)

(§ SETQ SH-STANDARD-PRINTOUT true SMNBREAK nil SMNTRACE nil MAKINTERN true ANNOYANCE nil)

(§ SSTATUS PAGEPAUSE true)

(§ SETQ ß_D true)
(§ SETQ ERRLIST nil)

#_(ns shrdlu.macros)

;; #############################################################
;;
;;              A PRECOMPILER FOR PROGRAMMAR CODE
;;
;; #############################################################

(§ DEFUN TOPLEVEL-LIST FEXPR [ELEMENTS]
    ;; ACTS LIKE LIST EXCEPT THAT IF ANY ELEMEMNT EVALUATES IN TO
    ;; MORE THAN A SINGLE ELEMENT (RETURNS A LIST WHOSE CAR IS
    ;; ALSO A LIST) THEN THE ELEMENTS OF THAT ELEMENT ARE ADDED
    ;; TO THE SAME LEVEL AS THE SEPARATE ELEMENTS IN THE CALL.
    (doall (map #'(lambda (ELEMENT)
            (SETQ ELEMENT (eval ELEMENT))
            (COND ((ATOM (car ELEMENT)) (list ELEMENT)) (:else ELEMENT)))
        ELEMENTS)))

(§ DEFUN PDEFINE FEXPR [MOBY]
    (list 'DEFUN (car MOBY) 'nil
        (concat (list 'PROG
            (concat '(FE H ME NB C SM CUT NN T1 T2 T3 :RESULT) (cadr MOBY))
            '(SETQ NN true)
            '(SETQ CUT END)
            '(SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
            '(SETR 'PARENT PARENT C))
            (APPLY #'SPREAD (cddr MOBY))
            (list 'FAIL
                '(SETQ MES ME)
                '(SETQ N (or (N RE) NB))
                '(RETURN nil)
                'RETURN
                '(SETQ MES ME)
                '(RETURN (REBUILD (reverse FE) NB N H SM C))))))

(§ DEFUN SPREAD FEXPR [LIST]
    (doall (map #'(lambda (EXP)
        (let [PREDICATE nil T1 nil T2 nil T3 nil]
            (COND
                ((ATOM EXP)
                    (RETURN (list EXP (list 'AND 'LABELTRACE (list 'PASSING (list 'quote EXP))))))
                ((EQ (car EXP) 'GOCOND)
                    (RETURN (list (list 'COND (list 'NN (list 'GO (cadr EXP))) (list ':else (list 'GO (caddr EXP)))))))
                ((EQ (car EXP) '|)
                    (SETQ PREDICATE (cadr EXP) T1 (caddr EXP) T2 (cadddr EXP))
                (and (cddddr EXP) (SETQ T3 (car (cddddr EXP))))
                    (RETURN (list
                        (list 'SETQ ':RESULT PREDICATE)
                        (COND
                            ((and T1 (nil? T2)) ;; T3 CAN BE EITHER THERE OR NOT
                                (list 'COND (TOPLEVEL-LIST ':RESULT
                                    (COND
                                        (T3 (list 'COND (TOPLEVEL-LIST (list 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST ':else (TAG-CHECK T1))))
                                        (:else (TAG-CHECK T1))))))
                            ((and (nil? T1) T2 (nil? T3))
                                (list 'COND (TOPLEVEL-LIST (list 'NULL ':RESULT) (TAG-CHECK T2))))
                            ((and (nil? T1) T2 T3)
                                (list 'COND (list (list 'NULL ':RESULT) (list 'COND (TOPLEVEL-LIST (list 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST ':else (TAG-CHECK T2))))))
                            ((and T1 T2 (nil? T3))
                                (list 'COND (TOPLEVEL-LIST ':RESULT (TAG-CHECK T1)) (TOPLEVEL-LIST ':else (TAG-CHECK T2))))
                            ((and T1 T2 T3)
                                (list 'COND (list ':RESULT (list 'COND (TOPLEVEL-LIST (list 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST ':else (TAG-CHECK T1))))
                                            (TOPLEVEL-LIST ':else (TAG-CHECK T2))))
                            ((and (nil? T1) (nil? T2) T3)
                                (list 'COND (TOPLEVEL-LIST (list 'AND (list 'NULL 'NN) ':RESULT) (TAG-CHECK T3))))
                            ((and (nil? T1) (nil? T2) (nil? T3))
                                (list 'I-AM-A-TAG))))))
                (:else (RETURN (list EXP))))
            nil))
    LIST)))

(§ DEFUN TAG-CHECK [TAG-EXP]
    (COND ((ATOM TAG-EXP) (list (list 'GO TAG-EXP)))
        (:else (list (list 'M (car TAG-EXP)) (list 'GO 'FAIL)))))

#_(ns shrdlu.progmr)

;; ###########################################################
;;
;;                         PROGMR
;;  (INTERPRETER FOR THE PROGRAMMAR GRAMMAR WRITING LANGUAGE)
;;
;; ###########################################################

(§ DEFUN RESTOREPT [] (SETQ PT SAVEPT))

(§ DEFUN SETMVB [PTR-MVB]
    (let [SAVE nil]
        (SETQ MVB PTR-MVB)                              ;; IF THERE ARE SEVERAL CLAUSES ACTIVE AT THE
        (SETQ SAVE PT)
        (SETQ PT PTR-MVB)                               ;; SAME TIME, IT SETS THE NEAREST ONE.
        (SETR 'MVB PTR-MVB (MOVE-PT C U (CLAUSE)))
        (SETQ PT SAVE)
        (RETURN true)))

(§ DEFUN ADD-F-PT [FEATURE PTR]
    (PUTPROP (car PTR) (cons FEATURE (FE PTR)) 'FEATURES)
    (and (EQ PTR C) (SETQ FE (FE PTR)))
    true)

(§ DEFUN REMOVE-F-PT [FEATURE PTR]
    (PUTPROP (car PTR) (SETDIF (GET (car PTR) 'FEATURES) (list FEATURE)) 'FEATURES)
    (and (EQ PTR C) (SETQ FE (FE PTR)))
    true)

(§ DEFUN ONE-WORD-LEFT [] (and (cdr NB) (not (cddr NB))))

(§ SETQ SMNBREAKS nil) ;; A LIST OF SMNFNS WHICH WILL BE BROKEN AT (BEFORE CALLING)

(§ DEFUN CALLSM FEXPR [SEMANTIC-EXPRESSION]
    (let [RESULT nil SMNFN nil]
        (SETQ SMNFN (car SEMANTIC-EXPRESSION))
        (and SMNTRACE
            (APPLY #'SAY (list 'SEMANTICS '***** UNIT 'CALLING SMNFN)))
        (and SMN
            (COND ((or (EQ SMNBREAKS true) (MEMQ SMNFN SMNBREAKS)) (RETURN (ERT)))
                (:else (RETURN true))))
        (and SMNTRACE
            (do (PRINTC "  CALLSM: ") (PRINC (car SEMANTIC-EXPRESSION))))
        (SETQ RESULT (eval (car SEMANTIC-EXPRESSION)))
        (and SMNTRACE
            (do (PRINTC "CALLSM RETURNING: ") (PRINC RESULT)))
        (COND ((or (EQ SMNBREAKS 'ALL) (MEMQ SMNFN SMNBREAKS)) (ERT)))
        (RETURN RESULT)))

(§ DEFUN MOVE-PT FEXPR [L]
    (let [XX nil YY nil L2 nil EXEC nil SAVE nil]
        (SETQ EXEC L)
        (SETQ SAVE PT)
    TEST1 (COND ((and (cdr EXEC) (not (ATOM (cadr EXEC))))
            (GO TEST)))
    LOOK1 (SETQ XX (car EXEC))
    LOOK (COND
            ((EQ XX 'H) (or (SETQ PT H) (GO FAIL)) (GO EX))
            ((EQ XX 'C) (SETQ PT C) (GO EX))
            ((EQ XX 'PC) (SETQ PT (H (PARENT C))) (GO EX))
            ((EQ XX 'LASTSENT) (SETQ PT LASTSENT) (GO EX))
            ((EQ XX 'U) (or (SETQ PT (PARENT PT)) (GO FAIL)))
            ((EQ XX 'DLC) (or (SETQ PT (H PT)) (GO FAIL)))
            ((EQ XX 'DF) (SETQ L2 (cons 'DLC (cons 'FR L2))) (SETQ XX 'DLC) (GO LOOK))
            ((EQ XX 'FR) (COND ((MOVE-PT PV) (GO LOOK))))
            ((EQ XX 'NX) (or (SETQ PT (PREVIOUS (H (PARENT PT)) (car PT))) (GO FAIL)))
            ((EQ XX 'PV) (SETQ PT (or (and (EQ PT C) (H (PARENT C))) (FOLLOWING (H (PARENT PT)) (car PT)) (GO FAIL))))
            (:else (PRINT XX) (ERT MOVE-PT ILLEGAL INSTRUCTION)))
    EX  (COND ((or (nil? L2) (nil? (SETQ L2 (cdr L2))))
            (GO TEST)))
        (SETQ XX (car L2))
        (GO LOOK)
    TEST (COND ((nil? (cdr EXEC)) (RETURN PT))
            ((ATOM (cadr EXEC)) true)
            ((COND ((cdadr EXEC) (eval (cadr EXEC)))
                (:else (ISX PT (caadr EXEC))))
            (SETQ EXEC (cdr EXEC)))
            (:else (GO LOOK1)))
        (COND ((SETQ EXEC (cdr EXEC)) (GO TEST1)))
        (RETURN PT)
    FAIL (SETQ PT SAVE)
        (RETURN nil)))

(§ DEFUN MOVE-PTW FEXPR [L]
    (let [EXEC nil SAVE nil XX nil]
        (SETQ SAVE PTW)
        (SETQ EXEC L)
    TEST1 (COND ((and (cdr EXEC) (not (ATOM (cadr EXEC))))
            (GO EX)))
    LOOK1 (SETQ XX (car EXEC))
    LOOK (COND ((EQ XX 'N) (SETQ PTW N))
            ((EQ XX 'LASTSENT) (SETQ PTW (NB LASTSENT)))
            ((EQ XX 'FW) (SETQ PTW (NB PT)))
            ((EQ XX 'AW) (COND ((EQ PT C) (GO FAIL)) ((SETQ PTW (N PT)) (SETQ XX 'PW) (GO LOOK))))
            ((EQ XX 'LW) (COND ((EQ PT C) (GO FAIL)) ((SETQ PTW (N PT)) (SETQ XX 'PW) (GO LOOK))))
            ((EQ XX 'NW) (COND ((SETQ PTW (cdr PTW))) ((SETQ PTW (FINDB SENT nil)) (GO FAIL))))
            ((EQ XX 'PW) (COND ((SETQ PTW (FINDB SENT PTW))) ((SETQ PTW SENT) (GO FAIL))))
            ((EQ XX 'SFW) (SETQ PTW SENT))
            ((EQ XX 'SLW) (SETQ PTW (FINDB SENT nil)))
            ((BUG MOVE-PTW ILLEGAL INSTRUCTION)))
    EX  (COND ((nil? (cdr EXEC)) (RETURN PTW))
            ((ATOM (cadr EXEC)) true)
            ((COND ((cdadr EXEC) (eval (cadr EXEC)))
                (:else (ISX PTW (caadr EXEC))))
            (SETQ EXEC (cdr EXEC)))
            (:else (GO LOOK1)))
        (COND ((SETQ EXEC (cdr EXEC)) (GO TEST1)))
        (RETURN PTW)
    FAIL (SETQ PTW SAVE)
        (RETURN nil)))

(§ DEFUN APPLY-GRAMMAR [UNIT]
    (COND ((GET UNIT 'INTERPRET) (INTERPRET UNIT))
        (:else (eval (list UNIT)))))

(§ DEFUN BUILDNODE [FEATURES FIRSTWORD WORDAFTER DAUGHTERS SEMANTICS]
    (let [NODE nil]
        (SETQ NODE (list (MAKESYM 'NODE)))
        (SETR 'FEATURES FEATURES NODE)
        (SETR 'FIRSTWORD FIRSTWORD NODE)
        (SETR 'WORDAFTER WORDAFTER NODE)
        (SETR 'DAUGHTERS DAUGHTERS NODE)
        (SETR 'SEMANTICS SEMANTICS NODE)
        (RETURN NODE)))

(§ DEFUN CQ FEXPR [FEATURE] (MEMQ (car FEATURE) FE))

(§ DEFUN CUT [A]
    (let [B nil C nil]
        (SETQ B N)
    =>  (COND
            ((EQ A B) (SETQ CUT A) (SETQ NN (not (EQ CUT N))) (RETURN true))
            ((EQ B END) (RETURN nil))
            ((SETQ B (cdr B)) (GO =>))
            ((nil? A) (SETQ CUT nil) (SETQ NN N) (RETURN true)))))

(§ DEFUN CUT-BACK-ONE [] (MOVE-PTW N PW) (POP) (CUT PTW))

(§ DEFUN F [A]
    (COND ((MEMBER A FE) true) ((SETR 'FEATURES (SETQ FE (cons A FE)) C))))

(§ DEFUN FE [NODE] (GETR 'FEATURES NODE))

(§ DEFUN FEATURE? [FEATURE] (MEMQ FEATURE FE))

(§ DEFUN FESET [NODE FEATURES] (SETR 'FEATURES FEATURES NODE))

(§ DEFUN FLUSHME []
    ;; IF YOU HAVEN'T REAHED THE CUT, FLUSHES THE NEXT WORD IN THE SENTENCE.  FAILS IF IT REACHES CUT POINT
    (and N NN (SETQ NN (not (EQ CUT (SETQ N (cdr N)))))))

(§ DEFUN FOLLOWING [LIST MEMBER]
    ;; GET THE ELEMENT OF LIST FOLLOWING MEMBER
    (and (MEMQ MEMBER LIST) (cdr (MEMQ MEMBER LIST))))

(§ DEFUN FQ FEXPR [A]
    (dorun (map #'(lambda (X) (or (MEMQ X FE) (SETQ FE (cons X FE)))) A))
    (SETR 'FEATURES FE C))

(§ DEFUN GETR [REGISTER NODE]
    ;; THIS FUNCTION RETRIEVES THE CONTENTS OF THE REGISTER ASSOCIATED WITH THE GIVEN NODE
    (GET (car NODE) REGISTER))

(§ DEFUN H [NODE] (GETR 'DAUGHTERS NODE))

(§ DEFUN ISQ FEXPR [A] (MEMQ (cadr A) (FE (eval (car A)))))

(§ DEFUN ISX [A B] (MEMBER B (FE A)))

(§ DEFUN M [A] (SETQ ME (cons A ME)))

(§ DEFUN MP FEXPR [A] (SETQ MESP A))

(§ DEFUN MQ FEXPR [A] (SETQ ME (cons A ME)))

(§ DEFUN N [NODE] (GETR 'WORDAFTER NODE))

(§ DEFUN NB [NODE] (GETR 'FIRSTWORD NODE))

(§ DEFUN NEXTW FEXPR [A] (EQ (car N) (car A)))

(§ DEFUN NEXTWORD [] (car N))                ;; RETURN THE NEXT WORD IN THE SENTENCE

(§ DEFUN NEXTWORD? [A] (EQ (car N) A))

(§ DEFUN NQ FEXPR [A] (MEMQ (car A) (FE N)))

(§ DEFUN ONLY-ONE-WORD-LEFT [] (and N (not (cdr N))))

(§ DEFUN PARENT [NODE] (GETR 'PARENT NODE))

(§ DEFUN PARSE FEXPR [A]
    (COND ((MEMQ (car A) '(NG CLAUSE VG PREPG ADJG)) (PARSE2 A (MEMQ 'TOPLEVEL A))) ((PARSE3 A nil))))

(§ DEFUN PARSE2 [REST P]
    ;; THIS FUNCTION CALLS THE PROGRAMMAR FUNCTION INDICATED BY THE FIRST MEMBER OF REST - A FEATURE LIST.
    ;; THE PARAMETER P INDICATES WHETHER PARSE2 IS BEING CALLED FROM THE TOPLEVEL.
    ;; IF P IS TRUE, PARSE2 DOES NOT APPEND THE NODE JUST PARSED TO THE PARSING TREE.
    ;; PARSE2 WILL CALL EITHER A COMPILED OR INTERPRETED VERSION OF THE GRAMMAR PROGRAM.
    (let [UNIT nil CREATED-NODE nil END nil PARENT nil RE nil SPECIAL nil NBB nil]
        (SETQ UNIT (car REST) LEVEL (inc LEVEL))
        (COND ((EQ (SETQ NBB N) CUT)
            (SETQ LEVEL (dec LEVEL))
            (RETURN nil)))
        (SETQ END CUT)
        (SETQ NN (not (EQ N CUT)))
        (SETQ PARENT C)
        (COND ((NQ B-SPECIAL)
            (and PARSETRACE
                (do (PRINTC "  SPECIAL WORD") (PRINC (car N))))
            (eval (GETR 'B-SPECIAL N))))
        (COND ((EQ SPECIAL 'SKIP) (GO SKIP))
            ((EQ SPECIAL 'DONE) (GO DONE))
            ((EQ SPECIAL 'LOSE) (SETQ N NBB) (GO LOSE)))
        (and PARSETRACE
            (do (TERPRI)
                (PRINC '\()
                (PRINC LEVEL)
                (PRINC " #### PARSING: ")
                (PRINC REST)))
        (COND ((nil? (SETQ RE (APPLY-GRAMMAR UNIT)))    ;; THIS IS WHERE ALL THE WORK HAPPENS.  IF THE PARSE SUCCEEDS,
            (SETQ RE nil)                               ;; IT WILL RETURN THE NODE THAT HAS BEEN BUILT UP
            (SETQ N NBB)                                ;; (SEE THE FUNCTION "INTERPRETATION" IN IN GINTER)
            (GO LOSE)))
    SKIP (COND ((EQ N CUT))
            ((NQ SPECIAL) (eval (GETR 'SPECIAL N))))
    DONE (or P
            (REBUILD (SETQ FE (GET (car C) 'FEATURES))  ;; REBUILDS THE HIGHER LEVEL NODE TO INCORPORATE
                NB                                      ;; THE DAUGHTER THAT WAS JUST PARSED EXCEPT IN THE
                N                                       ;; CASE WHERE THIS NODE IS THE TOPLEVEL
                (SETQ H (concat RE H))
                SM
                C))
    LOSE (SETQ NN (not (EQ N CUT)))
    OK  (COND ((and RE (or (EQ PARSETRACE 'ALL) (EQ PARSEBREAK 'ALL) (MEMQ UNIT PARSEBREAK) (MEMQ UNIT PARSETRACE)))
            (TERPRI)
            (PRINC '\()
            (PRINC LEVEL)
            (PRINC " PARSE SUCEEDED: ")
            (PRINC UNIT)
            (PRINC "  ")
            (PRINC (FROM (NB RE) N))
            (and PARSENODE-SEE (DP (car RE)))
            (and (or (EQ PARSEBREAK 'ALL)
                    (MEMQ UNIT PARSEBREAK))
                (ERT)))
            ((or PARSEBREAK PARSETRACE)
                (TERPRI)
                (PRINC '\()
                (PRINC LEVEL)
                (PRINC " PARSE FAILED")
                (and (or (EQ PARSEBREAK 'ALL)
                        (MEMQ UNIT PARSEBREAK))
                    (ERT))))
        (PARSE-STATISTICS)                              ;; DEFINED IN SYSCOM
        (SETQ LEVEL (dec LEVEL))
        (RETURN RE)))

(§ DEFUN PARSE3 [REST P]
    ;; PARSE3 IS CALLED TO PARSE JUST THE NEXT WORD IN THE SENTENCE
    (let [XP nil LABL nil RE nil SPECIAL nil NBB nil NODE nil]
        (COND ((EQ (SETQ NBB N) CUT) (MQ CUT) (RETURN nil))
            ((NQ B-SPECIAL)                                     ;; IS THE NEXT WORD MARKED SPECL?
            (eval (GETR 'B-SPECIAL N))                          ;; YES, DO SOMETHING SPECIALL
            (COND ((EQ SPECIAL 'SKIP) (GO SKIP))
                ((EQ SPECIAL 'LOSE) (SETQ N NBB) (RETURN nil))
                ((EQ SPECIAL 'DONE) (GO DONE)))))
        (COND ((car (SETQ XP REST)))                            ;; IF CALL IS (PARSE NIL FOO)
            ((NEXTWORD? (cadr REST)) (GO OK))                   ;; THEN LOOK FOR EXACT WORD "FOO"
            ((SETQ N NBB) (RETURN nil)))                        ;; IF NOT THERE, FAIL
    LOOP (COND ((not (ATOM (car XP)))
            (SETQ LABL (cons (caar XP) LABL)))                  ;; IF THE FEATURE IS NOT AN ATOM JUST ADD THE
            ((EQ (car XP) 'NULL))                               ;; FEATURE TO THE LIST
            ((MEMQ (car XP) (FE N)))
            ((MEMQ (car XP) UNMKD))
            ((M (car XP)) (SETQ N NBB) (RETURN nil)))
        (COND ((SETQ XP (cdr XP)) (GO LOOP)))
    OK  (SETQ RE
            (BUILDNODE (MEET (concat (FE N) LABL) (GET (car REST) 'ELIM))
                N
                (cdr N)
                'WORD
                (or SMN
                    (nil? (car REST))
                    (and (nil? (SM N)) (UNDEFINED))
                    (cadr (SASSOC (car REST) (SM N) #'UNDEFINED)))))
        (SETQ N (cdr N))
    SKIP (SETQ NN (not (EQ N CUT)))
        (COND ((and NN (NQ SPECIAL))
            (eval (GETR 'SPECIAL N))))
    DONE (SETR 'PARENT C RE)
        (COND (P RE)
            (:else (REBUILD FE NB N (SETQ H (concat RE H)) SM C)))
        (and PARSENODE-SEE RE (DP (car RE)) PNS-BK (ERT))
        (PARSE-STATISTICS)
        (RETURN RE)))

(§ DEFUN PARSEREL [A B NODE]
=>  (COND ((nil? A) (RETURN nil))
        ((not (ISX NODE (caar A))))
        ((eval (concat '(PARSE CLAUSE RSNG) (cdar A) B))
            (RETURN H)))
    (SETQ A (cdr A))
    (GO =>))

(§ DEFUN POP FEXPR [A]
    (COND ((or (nil? A) (nil? (car A)))
            (COND ((nil? H) nil)
                ((SETQ N (NB H))
                    (SETQ H (cdr H))
                    (REBUILD FE NB N H SM C)
                    (SETQ NN (not (EQ N CUT)))
                    (or SMN
                        (let [XX nil]
                            (MAP #'(lambda (BACKNODE)
                                (ERRSET
                                    (and (MAP #'(lambda (PLACE) (and (EQ PLACE (NB BACKNODE)) (ERR))) N)
                                        (SETQ XX (cons (car BACKNODE) XX)))))
                                BACKREF)
                            (SETQ BACKREF XX)
                            nil))
                    true)))
        ((eval (cons 'POPTO A)) (POP))))

(§ DEFUN POPTO FEXPR [A]
    (let [XX nil]
        (SETQ XX H)
    LOOP (COND ((eval (cons 'ISQ (cons 'XX A))))
            ((SETQ XX (cdr XX)) (GO LOOP))
            ((MQ POPTO) (RETURN nil)))
    EX  (COND ((EQ XX H) (RETURN C))
            ((POP) (GO EX)))))

(§ DEFUN PREVIOUS [LIST MEMBER]
    ;; GET THE ELEMENT OF LIST BEFORE MEMBER
    (let [GOODIE nil]
    =>  (COND ((nil? LIST) (RETURN nil))
            ((EQ MEMBER (car LIST)) (RETURN GOODIE))
            (:else (SETQ GOODIE (car LIST)) (SETQ LIST (cdr LIST))))
        (GO =>)))

(§ DEFUN PTFIND [X YY Z]
    (let [FOO nil]
        (SETQ FOO (car X))
    UP  (COND ((MOVE-PT U) (GO UP)) ((EQ (NB PT) X) (GO ON)))
    DOWN (or (MOVE-PT DLC PV (MEMQ FOO (NB PT))) (RETURN nil))
    ON  (COND ((not (EQ X (NB PT))) (GO DOWN))
            ((EQ YY true))
            ((MOVE-PT DF (EQ (N PT) YY)))
            ((RETURN nil)))
    CHECK (COND ((eval Z) (RETURN PT))
            ((not (EQ YY true)))
            ((MOVE-PT DF) (GO CHECK)))
    nil))

(§ DEFUN REBUILD [FEATURES FIRSTWORD WORDAFTER DAUGHTERS SEMANTICS NODE]
    (SETR 'FEATURES FEATURES NODE)
    (SETR 'FIRSTWORD FIRSTWORD NODE)
    (SETR 'WORDAFTER WORDAFTER NODE)
    (SETR 'DAUGHTERS DAUGHTERS NODE)
    (SETR 'SEMANTICS SEMANTICS NODE)
    NODE)

(§ DEFUN ROOT [X]
    ;; INPUT= PIECE OF SENTENCE
    ;; OUTPUT= ROOT OF FIRST WORD IN THAT PIECE
    ;; IF WORD HAS NO ROOT PROPERTY, THE ROOT == WORD
    (or (GET (car X) 'ROOT) (car X)))

(§ DEFUN RQ FEXPR [A] (SETR 'FEATURES (SETQ FE (SETDIF FE A)) C)) ;; REMOVE THE FEATURE A FROM FEATURE LIST OF THE CURRENT NODE

(§ DEFUN SECONDWORD? [WORD] (and N (cdr N) (EQ (cadr N) WORD)))

(§ DEFUN SETR [REGISTER VALUE NODE]
    ;; THIS FUNCTION ASSOCIATES THE GIVEN VALUE WITH THE GIVEN
    ;; NODE UNDER THE GIVEN INDICATOR, REGISTER
    (PUTPROP (car NODE) VALUE REGISTER))

(§ DEFUN SM [NODE] (GETR 'SEMANTICS NODE))

(§ DEFUN TRNSF FEXPR [A]
    (SETR 'FEATURES
        (SETQ FE (UNION (MEET A (FE PT)) FE))
        C))

(§ DEFUN UPREL [X]
    (and (not (ATOM X))
        (or (MEMQ 'UPREL (FE X)) (UPREL (H X)) (UPREL (cdr X))))) ;; FIND NODE WITH UPREL FEATURE

(§ DEFUN WORD [N] (car N))

(§ DEFUN UPCHECK []
    (and (MOVE-PT C U (REL-NOT-FOUND))
        (not (MEET (FE PT) '(OBJ1Q OBJ1REL OBJ2Q OBJ2REL LOBREL LOBQ)))))

#_(ns shrdlu.ginter)

(§ DEFUN PDEFINE FEXPR [A]
    ;; THIS PDEFINE MERELY PUT THE PROGRAMMAR FUNCTION ON THE
    ;; PROPERTY LIST OF THE PARSE NAME UNDER THE INDICATOR
    ;; 'INTERPRET. IT ALSO ADDS THE TAGS FAIL AND RETURN. NOTE THAT
    ;; THE PDEFINE BODY IS SIMILIAR TO PROG BODY. THIS SETS UP
    ;; INTERPRETED PROGRAMMAR EXECUTIONS
    (PUTPROP (car A)
        (concat (cdr A) (list 'FAIL '(RETURN 'FAIL) 'RETURN '(RETURN 'RETURN)))
        'INTERPRET))

(§ DEFUN INTERPRET [UNIT]
    ;; INTERPRET IS THE FUNCTION WHICH 'CALLS' AN INTERPRETED PROGRAMMAR PROGRAM.
    ;; IT FIRST DECLARES AND INITIALIZES ALL THE RELAVENT VARIABLES, THEN
    ;; IT EXECUTES THE PROGRAMMAR BODY AS A PROG.  NOTE THE USE OF "RE":
    ;; IT IS SET TO A NODE ONE WISHES TO BE THE INITIAL DAUGHTER OF THIS NODE.
    ;; ONLY CONJ NEEDS THIS HACK.
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST))     ;; FEATURE LIST
            (SETQ NB (or (NB RE) N))                    ;; BEGINNING IN SENTENCE OF THIS NODE
            N                                           ;; SENTENCE POINTER JUST AFTER THIS NODE
            (SETQ H RE)                                 ;; DAUGHTERS OF THIS NODE
            nil))                                       ;; SEMANTIC JAZZ
        (SETR 'PARENT PARENT C)                         ;; SET PARENT REGISTER
        (COND ((EQ (APPLY #'PROG (GET UNIT 'INTERPRET)) 'RETURN)
            (GO RETURN)))                               ;; APPLY THE PROGRAMMAR PROGRAM
    FAIL (SETQ MES ME)
        (SETQ N (or (N RE) NB))                         ;; RESET SENTENCE POINTER
        (RETURN nil)
    RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ DEFUN | FEXPR [BRANCH]
    (COND ((eval (car BRANCH))                          ;; EVALUATE BRANCH CONDITION
            (COND ((and (nil? NN) (cdddr BRANCH))
                    (GOCHECK (cddr BRANCH)))            ;; IF TRUE AND NO MORE SENTENCE REMAINS
                (:else (GOCHECK BRANCH))))                  ;; AND IF THIRD BRANCH GIVEN,THEN GO TO THIRD
        (:else (GOCHECK (cdr BRANCH)))))                    ;; BRANCH 2ND BRANCH

(§ DEFUN GOCHECK [LABEL]
    ;; THE GOCHECK FUNCTION CHECKS THE BRANCHES OF THE PROGRAMMAR CONDITIONAL.
    ;; IF THE BRANCH IS NIL, GOCHECK MERELY RETURNS.
    ;; IF THE BRANCH IS NON-ATOMIC, IT IS TAKEN AS A FAILURE MESSAGE.
    ;; GOCHECK PUTS THE MESSAGE ON THE MESSAGE LIST AND GOES TO FAIL (IN GINTERP)
    ;; IF THE BRANCH IS ATOMIC, GOCHECK GOES TO IT.
    ;; REMEMBER THAT THE GO LEADS TO A POINT SOMEWHERE IN THE PROGRAMMAR PROGRAM,
    ;; UNLESS IT IS 'FAIL OR 'RETURN.
    (COND ((nil? (cadr LABEL)) true)
        ((ATOM (cadr LABEL)) (GO (cadr LABEL)))
        (:else (M (cadr LABEL)) (GO FAIL))))

(§ DEFUN GOCOND FEXPR [A]
    ;; GOCOND GOES TO THE 1ST OR 2ND OF TWO TAGS DEPENDING IF THERE REMAINS
    ;; ANY MORE OF THE SENTENCE YET TO BE PARSED.
    (COND (NN (GO (car A))) (:else (GO (cadr A)))))

#_(ns shrdlu.gramar)

(§ PDEFINE CLAUSE (POSITION-OF-PRT MVB LOCATIONMARKER SUBJ-VB-BACKUP-TYPE1 POSITION-OF-PTW)

    ENTERING-CLAUSE
        (SETR 'TIME (BUILD TSSNODE= (MAKESYM 'TSS)) C)
        (| (CQ SIMP) SUBJ nil)
        (| (CQ MAJOR) INIT SEC)

    INIT
        (SETQ LOCATIONMARKER N)
        (| (and (NQ BINDER) (PARSE CLAUSE BOUND INIT)) nil MAJOR FIXIT)
        (FQ BIND)
        (| (CALLSM (SMBIND)) INIT nil)

    FIXIT
        (SETQ PTW CUT)
        (| (CUT (MOVE-PTW)) INIT MAJOR)

    MAJOR
        (CUT END)
        (COND ((EQ PUNCT '?) (GO QUEST))
            ((or (CQ IMPER) (EQ PUNCT '!)) (GO IMPER)))
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
            (or (CALLSM (SMRELATE H)) (POP)))                       ;; MORE INITIAL (BEFORE THE SUBJECT) MODIFIERS
        (and (NQ ADV)
            (PARSE ADV TIMW)
            (or (CALLSM (SMADVERB)) (POP)))
        (and (NQ ADV)
            (PARSE ADJG ADV VBAD)
            (or (CALLSM (SMRELATE H)) (POP)))
        (PARSE NG TIME)

        (| (EQ LOCATIONMARKER N) CLAUSETYPE INIT INPOP)

    ;; THE VARIABLE "LOCATIONMARKER" MARKS THE POSITION OF PTW AT THE TIME THAT IT WAS SET.
    ;; IF IT HAS NOT MOVED (IS STILL EQUAL TO N), THEN THAT INDICATES THAT NOTHING HAS BEEN
    ;; PARSED AND WE CAN GO ON.  OTHERWISE, THERE CAN BE ANY NUMBER OF INITIAL MODIFIERS AND
    ;; THE CODE STARTING AT "INIT" IS REPEATED, AS MANY TIMES AS NECESSARY.  IF PTW HITS THE
    ;; CUT POINT, THEN IT IS ASSUMED THAT SOMETHING WAS MISTAKENLY PARSED AS A MODIFIER WHEN
    ;; IT WAS NOT, AND EVERYTHING IS POPPED OFF (BY THE "INPOP" CODE).

    INPOP
        (| (MOVE-PT C DLC) nil (INPOP))                             ;; DOES ANYTHING REMAIN ON THE TREE?

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

    SUBJ (CUT END)                                                  ;; RESET CUTPOINT INCASE IT WAS MODIFIED BY
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
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VB))
            (SUBJ-VB-BACKUP-TYPE1
                (SETQ SUBJ-VB-BACKUP-TYPE1 nil)
                (GO SUBJ11))                                        ;; SEE THE LARGE NOTE ABOUT THIS IN "NOVERB".
            ((and H (ISQ H TIME) (ISQ H NG))
                (SETR 'SUBJECT H C)
                (GO VB))                                            ;; WHAT WAS INITIALLY PARSED AS A TIME-NG MODIFING
            ((MOVE-PT C U (REL-NOT-FOUND))                          ;; THE WHOLE CLAUSE MAY PROBABLY BEEN THE SUBJECT
                                                                    ;; OF THE CLAUSE THIS WORRIES ABOUT RELATIVE
                                                                    ;; CLAUSES. PLEASE NOTE THAT THE CURRENT
                                                                    ;; HALF-VERSION HAS NOT YET GOT ITS HEAD TOGETHER
                                                                    ;; ABOUT RELATIVE CLAUSES.  IE. THE CODE ISN'T
                (SETR 'SUBJECT (GETR 'RELHEAD PT) C)                ;; DEBUGGED AND HAS GAPS IN IT ESP. WHO SETS WHAT
                (SETR 'RELHEAD (GETR 'RELHEAD PT) C)                ;; REGISTER WHEN THIS WILL BE FIXED BEFORE THE
                (REMOVE-F-PT 'REL-NOT-FOUND PT)                     ;; VERSION IS FINALIZED
                (GO VB))
            ((and (CQ COMPONENT) NN) (FQ SUBJFORK) (GO VB))         ;; "SARAH ATE DINNER AND WENT TO THE MOVIES."
            (H (POP) (GO SUBJ))                                     ;; POP OFF THE CLOSEST INITIAL MODIFIER AND TRY TO
            ((GO FAIL)))                                            ;; PARSE A SUBJ AGAIN

    HEAD
        (| (or (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON)))       ;; COME HERE (ONLY?) TO TRY TIME PHRASE AS SUBJECT
            nil
            (HEAD))                                                 ;; MOVE PTW TO THE CLOSEST NOUN THEN SET THE CUT

    SUB2
        (| (POP) nil FAIL)                                          ;; POINT TO IT AND ATTEMPT A NEW PARSING IF
        (| (CUT PTW) INIT SUB2)                                     ;; NOTHING MORE TO POP, LOSE

    SUBJ1
        (COND ((ISQ H QUOTED)                                       ;; CIRCUMSTANCES UNDER WHICH IT IS ALLRIGHT TO
            (and (ISQ H LIST) (FQ LIST))                            ;; HAVE NOTHING FOLLOWING THE SUBJECT OF THE
            (FQ QUOTED)                                             ;; CLAUSE "  "MUMBLE", SAID JOHN."
            (SETQ H (H H))
            (GO RETSM)))
        (and (CQ REL-NOT-FOUND)                                     ;; THIS IS PART OF A BACKUP MECHANISM WHICH NEEDS
            (MOVE-PT H PV (QAUX))                                   ;; TO BE MORE THROUGHLY THOUGHT OUT. THE SITUATION
            (COND
                ((ISQ PT BE)                                        ;; IS EXPLAINED IN DETAIL IN QUESTION.NGQST MOVE
                    (FQ INT AUXBE)                                  ;; PT TO A VERB WHICH CAN BE AN AUXILLIARY AND
                    (RQ REL-NOT-FOUND)                              ;; WHICH CAN BEGIN A CLAUSE
                    (SETR 'COMP (GETR 'RELHEAD C) C)
                    (SETR 'SUBJECT H C)                             ;; "WHAT COLOR IS THE BLOCK?" OR "HOW BIG IS THE BLOCK?"
                    (SETMVB PT)
                    (GO ONT))
                ((ISQ PT HAVE)
                    (FQ SUBQ)
                    (RQ REL-NOT-FOUND)
                    (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                    (GO VBL))))

    SUBJ11
        (| (CUT-BACK-ONE) SUBJ3 (SUBJ11))                           ;; IF WE CAN'T CUT BACK ANY FURTHER, THEN FAIL

    SUBREG
        (SETR 'SUBJECT H C)                                         ;; THIS SETS THE "SUBJECT" REGISTER OF THE CURRENT
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
            ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
            ((not (ISQ H SUBJ)) (GO FAIL))
            ((ISQ H CLAUSE)
                (SETQ SUBJ-VB-BACKUP-TYPE1 true)
                (POP)
                (GO SUBJ4))                                         ;; THIS IS EXACTLY WHAT IT LOOKS LIKE.
                                                                    ;; AN ARBITRARY, NOT TOO WELL THOUGHTOUT BACKUP
                                                                    ;; MECHANISM.  (NEEDLESS TO SAY IT WILL GO AWAY
                                                                    ;; FAST).  WE HAVE BEEN UNABLE TO FIND A VERB
                                                                    ;; AND HAVE NOTICED THAT WE PARSED A CLAUSE OF
                                                                    ;; SOME SORT AS THE SUBJECT.  HYPOTHESIS:  WE
                                                                    ;; MISSINTERPRETED SOMETHING WHILE PARSING THAT
                                                                    ;; CLAUSE AND MANAGED TO SWALLOW UP THE VERB OF
            ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))            ;; THE HIGHER CLAUSE WITH IT.  SOLUTION:  POP OFF
    VB2
        (CUT-BACK-ONE)                                              ;; THE CLAUSE AND TRY TO REPARSE THE SEGMENT IN
        (GO SUBJ3)                                                  ;; ANOTHER FASHION.  "SUBJ4" IS PLACED THE SUBJECT
                                                                    ;; CODE AFTER LOOKING FOR CLAUSES AND BEFORE NOUN
                                                                    ;; GROUPS.  DEFAULT CUTTING MECHANISM FOR VBL.
    VBREG
        (SETR 'VG H C)

    ;; ###############################################################
    ;;
    ;;             PARSE ANY OBJECTS REQUIRED BY THE VERB
    ;;
    ;; ###############################################################

    VG1 (CUT END)                                                   ;; RESET THE CUTPOINT IN CASE ANYONE CHANGED IT
        (| (ISQ MVB BE) BE nil (BE))                                ;; JUMP TO "BE" PROCESSOR

        ;; There used to be a check here for a quoting MVB with a quoted subject.
        ;; It was deleted because it went to a tag that no longer exists and doesn't seem to have any modern analogs.
        ;; For the original code: see "gramar 19" or earlier.
        ;; It was put in by Jeff Hill in the spring of 1972.

        ;; VERB-PARTICLE COMBINATIONS SUCH AS "PUT ON", "SET DOWN", ETC.
        ;; THEIR ESSENTIAL PROPERTY IS THAT VERB AND PARTICLE CAN BE DISPLACED BY THE OBJECT.
        ;;    "PUT DOWN THE BLOCK."
        ;;    "PUT THE BLOCK DOWN."

        (| (ISQ MVB VPRT) nil CHECKPASV CHECKPASV)
        (| (and (NQ PRT) (PARSE PRT)) nil DPRT)                             ;; IF THE PARTICLE IS NOT THE WORD FOLLOWING THE VERB, THEN
        (FQ PRT)                                                            ;; IT IS SEARCHED FOR BY CODE AT "DPRT" (DISPLACED PARTICLE)

        (| (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD (NB H))))            ;; IS THIS A LEGITIMATE COMBINATION OF VERB AND PARTICLE?
            CHECKPASV
            POPRT)

    DPRT
        (| (ISQ H PASV) CHECKPASV nil)                                      ;; SEARCH FOR DISPLACED PARTICLE.  NO DISPLACED PARTICLES
        (| (SETQ POSITION-OF-PRT (MOVE-PTW N NW (PRT))) nil FINDOBJ1)       ;; IN PASV'S IF NOT FOUND ASSUME THAT IT IS OPTIONAL AND
        (| (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD POSITION-OF-PRT)))   ;; WE ARE DEALING WITH THE CASE WITHOUT THE PARTICLE
            nil
            POPRT)
        (| (ISQ MVB TRANS) nil FINDOBJ1)
        (CUT POSITION-OF-PRT)
        (| (PARSE NG OBJ OBJ1)                                              ;; PARSE UP ANY NOUN GROUP YOU FIND
            POPRT
            FINDOBJ1                                                        ;; IF THERE ARE MORE OR LESS NP'S THAN EXPECTED,
            nil)                                                            ;; THEN DON'T PARSE ANYTHING BUT GO TO NPRT
        (CUT END)                                                           ;; INSTEAD. SIMILARLY, IF ANYTHING FOLLOWS THE
        (SETR 'OBJ1 H C)                                                    ;; DISPLACED PARTICLE THEN A GRAMMATICALLY BAD
        (PARSE PRT)                                                         ;; FORM IS ASSUMED AND THE PIECES POPED OFF
        (FQ PRT DPRT)
        (GO FINDOBJ2)

    POPRT
        (POPTO VG)
        (GO FINDOBJ1)

    CHECKPASV                                                               ;; CHECK THE VERB FOR THE PASSIVE CONSTRUCTION
        (| (and (ISQ H PASV) (FQ PASV) (SETR 'OBJ1 (GETR 'SUBJECT C) C))
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
        (| (MOVE-PT C U (REL-NOT-FOUND)) OBJ1REL nil)
        (| (and (CANTAKE 1 '(PREPG LOC) 'ITRNSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ ITRANSL))
            PUTLOBJ
            nil)
        (| (CANPARSE 1 nil 'ITRNS) ONT nil)

    GOOF1
        (or GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - FIRST OBJ))
        (GO FAIL)

    OBJ1REL
        (SETR 'OBJ1 (GETR 'RELHEAD PT) C)
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
        (| (and (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND)))
            OBJ2REL
            nil)
        (| (and (CANTAKE 2 '(PREPG LOC) 'TRANSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ TRANSL))
            PUTLOBJ
            nil)

    OBJ2TO
        (PARSE ADV VBAD)
        (| (COND ((and (NEXTWORD? 'TO)
                    (ISQ MVB TO2)
                    (PARSE PREPG TO))                               ;; THE SECOND-OBJECT THAT WE HAVE BEEN LOOKING FOR
                (SETR 'OBJ2 (GETR 'OBJ1 H) C)                       ;; MAY BE A PREPG AS IN "GIVE IT TO THE LIONS"
                (FQ TRANS2TO TRANS2))                               ;; TAKES THE OBJECT OF THE PREPOSITION "TO" AND
            ((and (CQ PREPQ)                                        ;; MAKES IT THE OBJ2 OF THE CLAUSE.
                (MOVE-PT H PV (QUEST))
                (EQ (WORD (MOVE-PTW FW)) 'TO)
                (RQ PREPQ)
                (FQ TRANS2TOQ TRANS2)
                (SETR 'OBJ2 (GETR 'OBJ1 PT) C))))                   ;; "TO WHOM DID YOU GIVE THE MEAT?"
            ONT
            nil)
        (| (CANPARSE 2 nil 'TRANS) ONT FAIL)

    PUTLOBJ
        (SETR 'LOBJ PT C)
        (SETR 'RELHEAD (GETR 'QADJ PT) PT)
        (SETR 'QADJ nil PT)
        (REMOVE-F-PT 'QADJ PT)
        (GO ONT)

    OBJ2REL
        (SETR 'OBJ2 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ2REL)
        (GO ONT)

    FIXSUBJECT
        (SETR 'SUBJECT (GETR 'OBJ1 C) H)
        (GO ONT)

    CHECKIT                                                         ;; CHECK FOR THE POSSIBILITY THAT THE SUBJECT WAS
        (| (EQ (WORD (NB (GETR 'SUBJECT C))) 'IT)                   ;; A DUMMY FUNCTION WORD ("IT"), AS IN "IT WAS NICE TO SEE HIM."
            nil
            ONT)                                                    ;; TO BE ADDED HERE: "JOHN WAS EAGER/EASY TO PLEASE."
        (| (or (and (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ))
                (and (NQ ING) (PARSE CLAUSE RSNG ING SUBJ))
                (PARSE CLAUSE REPORT))
            nil
            ONT)
        (FQ IT)
        (SETR 'LOGICAL-SUBJECT H C)                                 ;; THE CLAUSE IS THE REAL SUBJECT.
        (GO ONT)

    GOOF2
        (or GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - SECOND OBJECT))
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
        (| (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1)                ;; RELATIVE CLAUSE AND MAY HAVE TO DO SOME
            nil                                                     ;; GYMNASTICS IF THE CLAUSE IS NOT TO FAIL
            PREPSHORT)                                              ;; MOVE BACK TO A QUESTION-NOUNGROUP, THEN DOWN

    TIMEQ
        (RQ REL-NOT-FOUND)                                          ;; AND BACK TO THE NOUN. IF THAT NOUN IS "TIM1"
        (FQ TIMEQ)                                                  ;; THEN ASSUME WE HAVE FOUND OUR RELATIVE ELEMENT.
        (GO TONT)

    PREPSHORT
        (| (and (NQ PREP) (PARSE PREPG)) nil (ONT-SHORT-PREP))
        (| (CALLSM (SMRELATE H)) nil (ONTß SMRELATE PREPQ))
        (| (CQ REL-NOT-FOUND) PREPSHORT TONT (ONT-NOT-FOUND))       ;; WE HAVE A PREP TO TAKE THE UNATTACHED RELATIVE
                                                                    ;; AS ITS OBJECT. THE FEATURE REL-NOT-FOUND WILL
                                                                    ;; BE REMOVED IF THE PREPG DISCOVERS IT CAN'T FIND

    PONT
        (and (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))        ;; AN OBJECT (THE REMOVING WILL BE DONE WHILE IN PREPG).
        (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)                    ;; "LOGICAL" IE. SUBJECT IN RELATIONSHIP
        (GO ONT1)                                                   ;; TO THE PROPER SEMANTIC INTERPRETATION OF THE
                                                                    ;; MAIN VERB. AGENT-PREPG CAN BE PARSED (REFLECTS
                                                                    ;; THE OPTIONALITY OF THE CONSTRUCTION)

    ;; ###################################################################################
    ;;                    CHECK FOR ADDITIONAL MODIFYING PHRASES
    ;; ###################################################################################

    TONT (| (SETQ POSITION-OF-PTW N) nil RETSM RETSM)               ;; WE ARE USING THE SAME TECHNIQUE HERE AS WITH THE INITIAL MODIFIERS.
                                                                    ;; IE. LOOP THROUGH THE POSSIBILITIES UNTIL YOU MAKE A PASS THAT ADDS
                                                                    ;; NOTHING NEW.

    NPASV
        (| (and (NQ PREP) (PARSE PREPG) (CALLSM (SMRELATE H)))                                  ;; PREPG
            nil
            nil
            RETSM)
        (| (and (NQ TIMW) (PARSE ADV TIMW) (or (CALLSM (SMTIME)) (GO FAIL)))                    ;; TIMW
            nil
            nil
            RETSM)
        (| (and (not (CQ BE)) (PARSE ADJG ADV) (or (CALLSM (SMRELATE H)) (GO FAIL)))            ;; ADV
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
        (| (EQ N POSITION-OF-PTW) nil TONT RETSM)                   ;; LOOP UNTIL NOTHING ELSE CAN BE PARSED.
        (| (or (not (CQ TOPLEVEL)) (NQ SPECIAL)) RETSM nil)         ;; SPECIAL WORD (E.G. COMMA AND) COULD INDICATE
        (ERT "CLAUSE:" SOMETHING LEFT OVER AT TOP LEVEL)              ;; A CONJUNCTION OR A BINDER.
        (GO FAIL)

    ;; ##############################################################################
    ;;                                   THERE
    ;;             AS IN:  "THERE IS A BIRD SITTING ON YOUR SHOULDER"
    ;; ##############################################################################

    THERE
        (FQ THERE)
        (CUT END)
        (| (PARSE ADV TIMW) nil nil (THERE))                        ;; "THERE IS A BIRD.."
        (| (and (PARSE VG) (ISQ MVB BE)) THEF NOTHE (THERE))

    THERQ
        (| (ISQ (MOVE-PT H PV (QAUX)) BE) THERQ2 nil)               ;; IF THIS FAILS, THE THERE IS CONSIDERED TO BE
        (| (and (NQ TIMW) (PARSE ADV TIMW)) nil nil (THEREQ))
        (| (and (PARSE VG) (ISQ MVB BE)) THERQ2 nil)
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
        (SETR 'SUBJECT H C)
        (GO ONT)

    THERREL
        (| (MOVE-PT C U (REL-NOT-FOUND)) nil NOTHE)
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
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
        (| (ISQ H QUEST) nil QUEST)                                 ;; IF THE PREPG ISN'T THE QUESTION, TRY AGAIN "ON
        (SETR 'QADJ H C)                                            ;; THAT DAY, WHERE DID YOU GO?" -- MAYBE WE COULD
                                                                    ;; MAKE USE OF THE COMMA CLUE. PREPQ IS HANDLED
        (GO POLAR)                                                  ;; MUCH LIKE QADJS LIKE WHEN AND WHERE THE REST OF
                                                                    ;; THE QUESTION HAS THE SAME SYNTAX AS A POLAR (YES-NO).
    NGQUES ;; NOUN GROUP QUESTION
        (| (PARSE NG QUEST) NGQST nil)                              ;; "WHICH ONE IS THE MURDURER?"
        (| (or (and (NEXTWORD? 'HOW)
                (PARSE ADJG QUEST)
                (SETR 'RELHEAD H C))                                ;; "HOW BIG...."
            (and (NQ QADJ)
                (PARSE QADJ)
                (FQ QADJ)
                (SETR 'QADJ H C)))                                  ;; "WHAT...?",  "WHERE...?"
            POLAR
            POLAR
            nil)
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))                                      ;; IF ALL THE SENTENCE CONSISTS OF IS THE QUESTION

    ADJQS
        (GO RETURN)                                                 ;; ADJECTIVE THEN WE SHOULD RETURN DIRECTLY

    NGQST
        (SETR 'RELHEAD H C)

    NGQST2
        (CUT END)
        (SETR 'SUBJECT H C)
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
                (CUT PTW)
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
        (| (and (NQ VB) (PARSE VB AUX (QAUX)) (SETR 'QAUX H C) (CALLSM (SMVAUX)) (SETMVB H))
            nil
            QCHOP)
        (or (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
        (FQ POLR2)
        (GO QUEST2)

    QCHOP
        (ERT "CLAUSE:" QCHOP)
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
        (SETQ LOCATIONMARKER N)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)                                                   ;; "FDEC" IS NEAR THE TOP OF THE MAJOR CLAUSE

    RSQ
        (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
        (| (CQ PREPREL) nil RSQ2)
        (PARSE PREPG PRONREL)                                       ;; THIS CALL IS BASED ON INFORMATION PASSED FROM
        (SETR 'QADJ H C)                                            ;; FAR AWAY AND EXPLAINED IN DETAIL IN THE CODE
        (GO REPORT)                                                 ;; FOR PREPOSITION GROUPS

    RSQ2
        (COND ((PARSE VG EN PASV)                                   ;; HAVING DETERMINED THAT THE VERB IS PASSIVE IF
                (or (ISQ MVB TRANS) (GO FAIL))                      ;; IT WERE NOT ALSO TRANSITIVE, THEN WE WOULDN'T
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)                 ;; KNOW WHAT TO DO WITH WHATEVER WAS PARSED AS A
                (GO VG1))                                           ;; SUBJECT - SO WE FAIL
            ((PARSE VG ING)
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VG1))
            ((NQ PRONREL) (PARSE NG RELWD) (GO REL))
            ((CQ COMPONENT)                                         ;; IN A COMPONENT RELATIVE THE RELWD MIGHT BE IN
                (SETR 'RELHEAD                                      ;; THE FIRST CLAUSE.
                    (GETR 'RELHEAD (MOVE-PT C PC))
                    C)                                              ;; MAKE RELHEAD SAME AS PREVIOUS COMPONENT RSQ.
                (GO REL))
            ((PARSE NG SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
            (:else (GO FAIL)))                                          ;; THIS REALLY ISN'T AN RSQ

    REL
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
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
        (SETR 'SUBJECT H C)

    TO1
        (| (PARSE VG TO) VG1 (TO))

    ING
        (| (MOVE-PTW N NW (ING)) nil FAIL)
        (| (or (NQ ING) (CQ OBJ2) (and (PARSE NG SUBJ INGSUBJ) (SETR 'SUBJECT H C) (FQ SUBING) (RQ ING)))
            nil
            nil
            (ING))
        (| (PARSE VG ING) VG1 (ING))

    REPORT
        (and (NEXTWORD? 'THAT) (PARSE nil THAT) (FQ THAT))
        (SETQ LOCATIONMARKER N)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
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
        (| (ISQ H POSS) PROPS nil)
        (| (and NN (NQ PROPN)) PROPN nil)

    PROPS
        (or (CALLSM (SMPROP)) (GO FAIL))                            ;; EXAMINE ITS SEMANTICS
        (| (ISQ H POSS) POSS PRAG)

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
        (| (CALLSM (SMPRON H)) nil FAIL)                            ;; EXAMINE SEMANTICS OF PN

    PRAG
        (SETR 'HEAD H C)
        (MOVE-PT H)
        (TRNSF NS NPL NFS NEG)                                      ;; MODIFY PN FEATURES TO CORRECT NUMBER
        (GO RETURN)

    ;; -------------- ANYTHING, SOMETHING, ... --------------

    TPRON
        (PARSE TPRON)
        (FQ TPRON)
        (MOVE-PT H)
        (TRNSF NS NPL ANY NEG)
        (SETR 'HEAD C H)
        (and NN (NQ ADJ) (PARSE ADJ))
        (GO SMNG)

    ;; ----------- WHATEVER, WHENEVER, WHOEVER, ... -----------

    EVERPRON
        (| (and (PARSE PRON EVERPRON) (CALLSM (SMPRON H)))
            nil
            FAIL)
        (| (and (PARSE CLAUSE RSQ NOREL) (CALLSM (SMRELATE H)))
            RETSM
            FAIL)

    AS  (| (and (PARSE nil AS) (PARSE NUMD NUMDAS) NN (PARSE nil AS))
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
        (MOVE-PT H)                                                 ;; SHIFT PTR TO THE DETERMINER
        (| (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR)
            IND
            (BUG)
            INCOM)

    ;; -------------- INDETERMINATE ---------------

    IND
        (| (and (EQ (WORD (NB H)) 'ALL) (EQ (WORD N) 'THE) (PARSE DET) (FQ DEF))
            NUM
            nil
            (THE))
        (| (and (ISQ H QNTFR) (FQ QNTFR)) QNUM nil)

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
        (| (COND ((and (ISQ H NS) (CQ NS)) (RQ NPL PART)) ((CQ NPL) (RQ NS PART)))
            ADJ
            (NUM)
            INCOM)

    DET1
        (COND ((ISQ H NS) (FQ NS)) (:else (FQ NPL)))                    ;; EXPLICIT CHECK FOR THE VALUE 1
        (or NN (and (FQ NUMBER) (GO INCOM)))

    NUMBER
        (FQ DET)
        (| (NQ OF) OF ADJ)

    QNUM
        (| (ISQ H NONUM) OF nil)
        (| (and (PARSE NUM) (FQ NUM)) nil OF)
        (| (COND ((EQ (SM H) 1) (and (CQ NS) (RQ NPL))) ((CQ NPL) (RQ NS))) ;; EXPLICIT CHECT FOR THE VALUE 1
            nil
            (NUMD)
            INCOM)
        (| (EQ (WORD (NB H)) 'NO) ADJ nil)                          ;; CHECKS FOR WORD "NO"

    ;; -------------- PREPG WITH "OF" ---------------

    OF  (| (and (NQ OF) (PARSE PREPG OF)) SMOF NONE)                ;; "FIVE OF THE BLOCKS"

    SMOF
        (FQ OF)
        (| (or (CALLSM (SMNGOF)) (not (POP))) RETSM INCOM)

    NONE
        (| (EQ (WORD (NB H)) 'NONE) INCOM ADJ)

    ;; ----------- PARSE ALL THE ADJECTIVES -----------

    ADJ
        (| (PARSE ADJ) nil EPR INCOM)
        (and (ISQ H COMPAR)
            (FQ COMPARATIVE-MODIFIER)
            (SETR 'COMPARATIVE-MODIFIER H C))
        (GO ADJ)

    EPR
        (| (or (ISQ H SUP) (ISQ H COMPAR)) nil CLASF INCOM)         ;; WE PARSED AN ADJ AND RAN OUT OF WORDS
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

        (| (and (CQ TIME) (not (ISQ H TIM1))) RED1 nil)

    ;; -------------- MODIFY FEATURES FOR NUMBER AND SUCH --------------

        (SETQ T1 FE)
        (COND ((and (ISQ H MASS) (or (CQ PART) (not (CQ DET))))
            (FQ MASS)))
        (COND ((not (ISQ H NPL)) (RQ NPL PART)))
        (COND ((not (ISQ H NS)) (RQ NS)))
        (COND ((and (not (CQ DET)) (not (CQ NUMD)))
            (MOVE-PT H)
            (TRNSF NPL MASS)))
        (| (MEET FE '(NS NPL PART MASS)) nil RED0)

        (| (NEXTWORD? 'THAN) nil SMNG)                              ;; "...A BIGGER BLOCK THAN..."
        (FQ THAN)                                                   ;; THE PRESENCE OF THIS FEATURE IS NOTED BELOW AND IN ADJG

    ;; AT THIS POINT SMNG1 IS CALLED FOR PRELIMINARY CHECKS AND ANALYSIS BEFORE CHECKING QUALIFIERS

    SMNG
        (SETR 'HEAD H C)                                            ;; SET HEAD REGISTER TO THE NOUN

        (| (and (CQ OBOFJ) (not (CQ DEF))) FAIL nil)                ;; JUST PARSED
        (or (CALLSM (SMNG1)) (GO FAIL))
        (| (not (ISQ H POSS)) nil POSS RETSM)                       ;; CHECK FOR POSSIVE

    ;; #################################################
    ;;               POSSIBLE QUALIFIERS
    ;; #################################################

        (| (and (CQ THAN) (PARSE ADJG)) nil RSQ-TO)                 ;; "...A BIGGER BLOCK THAN..."
        (| (CALLSM (SMRELATE H)) RETSM FAIL)

    RSQ-TO
        (| (and (NEXTWORD? 'TO) (MEET FE '(COMP SUBJ)) (PARSE CLAUSE RSQ TO) (or (CALLSM (SMRELATE H)) (GO POPRET)))
            RETSM
            nil)

    ;; -------------- AS OR COMPARATIVE ---------------

        (| (and (or (NEXTWORD? 'AS) (NQ COMPAR)) (PARSE ADJG THANNEED))
            nil
            PREPNG)                                                 ;; WHAT IS THE REASON FOR THE EXISTANCE OF THIS
        (and (nil? N)                                               ;; STRANGE ANIMAL (ALSO THE ONEBELOW) -- CHECK
            (CQ SUBJ)                                               ;; THEM OVER AND HACK THEM PROPERLY
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (GO POPRET))                                            ;; AVOIDS ATTACHING MODIFIER WHEN IT GOBBLES TO
        (| (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)               ;; MUCH E.G. IS THE BLOCK ON THE TABLE? DOESN'T
                                                                    ;; WANT "THE BLOCK ON THE TABLE" AS A CONSTITUENT.
                                                                    ;; I ADMIT ITS A HACK.

    ;; -------------- ANY SORT OR PREPOSITION GROUP --------------

    PREPNG
        (| (and (NQ PREP)
                (not (or (and (NQ PLACE) (CQ NOLOC))
                    (and (CQ OBJ1)
                        (ISQ MVB TRANSL)
                        (not (ISQ (MOVE-PT C U) QUEST)))))
                (PARSE PREPG Q))
            nil
            DISGRSQ)
        (and (nil? N)
            (CQ SUBJ)
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (not (ISQ (MOVE-PT U) NGQ))
            (GO POPRET))
        (| (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)

    ;; CHECK FOR DISGUISED RSQ CLAUSES BY READING THE FAILURE MESSAGES SENT UP FROM PREPG.

    DISGRSQ
        (| (EQ (car MES) 'PREP-WHICH) nil RSQ)
        (SETQ MES (cdr MES))
        (| (PARSE CLAUSE RSQ PREPREL) PREPNG (RSQ-PREPREL) RETSM)

    ;; -------------- ANY OTHER RSQ ---------------

    RSQ
        (| (and (ISQ (MOVE-PT C U) POLR2) (CQ SUBJ) (NQ VB) (not (CQ SUBJT)) (not (ISQ PT QADJ)))
            RETSM
            nil)
        (| (PARSE CLAUSE RSQ) nil RETSM)
        (| (CALLSM (SMRELATE H)) RETSM POPRET)

    ;; -------------------------------------------------
    ;; THE ENTIRE NG SHOULD HAVE BEEN PROCESSED BY THIS POINT
    ;; -------------------------------------------------

    ;; -------------------------------------------------
    ;; IF AT FIRST YOU DON'T SUCEED.......
    ;; -------------------------------------------------

    RED0
        (SETQ FE T1)
    RED1
        (POP)
    RED2
        (COND ((nil? H) (MQ NO) (GO FAIL))
           ((ISQ H NUMBER) (GO INCOM))
           ((and (ISQ H POSS) (or (ISQ H PRON) (and (MOVE-PT H DLC) (ISQ PT PRON))))
                (POP)
                (GO PRON2))
           ((and (nil? (cdr H)) (CQ DEFPOSS)) (GO POSSDEF))
           ((and (CQ QUEST) (nil? (cdr H))) (GO QDETCHECK))         ;; (CDR H) = T IF THERE IS ONLY ONE DAUGHTER TO THE CURRENT NODE
           ((ISQ H ADJ) (GO EPR))
           ((not (ISQ H CLASF)) (GO INCOM)))

    REDUC
        (POP)
        (| (and (nil? H) (NQ PROPN)) PROPN NOUN)

    POPCOM
        (POP)

    ;; -------------- INCOMPLETE PHRASES ---------------

    INCOM
        (FQ INCOM)
        (| (and (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM)))
            RETURN
            nil)
        (| (and (nil? CUT) (CQ NUM)) SMNG nil)

    QDETCHECK
        (COND ((and (ISQ H QDET) (ISQ (NB H) QPRON))
                (POP)
                (GO QPRON))
            ((and (ISQ H QDET) (ISQ (NB H) EVERPRON))
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
        (SETQ H (BUILDNODE (reverse (cons 'POSS (SETDIF FE '(COMPONENT)))) NB N H SM))
        (SETQ BACKREF (concat H (cdr BACKREF)))
        (| (SETR 'FEATURES (SETQ FE (concat '(POSES DET DEF NS NPL) (reverse REST))) C)
            nil
            (BUG))
        (| (or (not NN) (ISQ H DEFPOSS)) nil ORD)

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
                (CALLSM (SMSET (SM (MOVE-PT C U U (NG))))))         ;; SET SM TO THE NOUNGROUP DIRECTLY UPSTAIRS
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
        (| (ISQ H NOUN) nil (TRYA))
        (POP)
        (CUT N)

    UP
        (| (POP) UP nil)                                            ;; POP EVERYTHING OFF
        (SETQ FE (reverse REST))
        (SMSET nil)
        (GO NGSTART))

(§ PDEFINE VG (TENSE)

    ;; ##################################################################
    ;; CHECK INITIAL FEATURES TO SEE IF SOME SPECIAL TYPE OF VG IS WANTED
    ;; ##################################################################

    ENTERING-VG
        (COND ((CQ TO) (GO TO))
            ((CQ EN) (GO EN))
            ((CQ ING) (GO ING))
            ((CQ IMPER) (GO IMPER))
            ((ISQ (MOVE-PT C U) POLR2) (GO POLR2)))                 ;; CHECKS IF THE CLAUSE IS MARKED AS POLR2

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
        (MOVE-PT C DLC)                                             ;; MOVE PT DOWN FROM THE CURRENT NODE BEING PARSED
        (TRNSF VPL INF V3PS)                                        ;; (VG) AND ACROSS TO THE MOST RECENTLY PARSED
        (SETQ TENSE (COND ((and (ISQ PT PRESENT) (ISQ PT PAST))     ;; DAUGHTER.  IN THIS CASE THAT DAUGHTER WAS PARSED
                    '(PAST-PRESENT))                                ;; IN THE DISPATCH TABLE JUST ABOVE
                ((ISQ PT PAST) '(PAST))
                (:else '(PRESENT))))
        (GO REV)

    TO
        (FQ NAGR)                                                   ;; "NAGR" MARKS THAT SUBJECT AND MAIN VERB NEED
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))            ;; NOT AGREE IN NUMBER AND PERSON AND INSURES THAT
        (| (or (PARSE nil TO) (CQ TODEL)) nil (TO) (TO))            ;; THE AGREEMENT CHECKER AT THE END OF THE PROGRAM
                                                                    ;; ("REV") WILL NOT BE APPLIED "TODEL" MUST BE
        (SETQ TENSE '(INFINITIVE))                                  ;; GIVEN AS AN INITIAL FEATURE OR ELSE THIS
        (GO MODAL2)                                                 ;; STATEMENT FAILS TENSE IS USED TO HOLD THE TENSE
                                                                    ;; WHILE IT IS BEING COLLECTED.

    EN
        (FQ NAGR)
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))
        (SETQ TENSE '(PAST))
        (| (and (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)) RETSM FAIL) ;; DONE AT "EN2"

    ING
        (FQ NAGR)
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))

    INGADV
        (| (or (PARSE ADV TIMW) (PARSE ADV VBAD)) INGADV nil)
        (SETQ TENSE '(PRESENT))
        (GO BE2)

    IMPER
        (| (and (PARSE VB DO NEG INF) (FQ NEG)) nil nil (DONT))
        (| (and (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG)))
            RETURN
            (IMPER))                                                ;; MVB IS BOUND BY CLAUSE

    POLR2                                                           ;; THE CLAUSE COULD ONLY BE MARKED AS "POLR2"
        (or (SETQ PT (GETR 'QAUX (MOVE-PT C U)))                    ;; ("DID THE...?") IF AN AUX OF SOME VERIETY HAD
            (and (BUG "VG:POLR2") (GO FAIL)))                         ;; ALREADY BEEN PARSED, IF THAT IS NOT THE CASE,
        (SETQ H (list (car PT)))                                    ;; THEN WE HAVE A BUG IN THE PROGRAM SOMEWHERE SET
        (TRNSF NEG)                                                 ;; THE INITIAL DAUGHTER OF THE VG TO BE THE
        (COND ((ISQ H DO) (GO DO))                                  ;; PREVIOUSLY PARSED AUX MARK THE VG AS NEG IF
            ((ISQ H MODAL) (GO MODAL))                              ;; APPROPRIATE (SEE PROGMR FILE FOR THE OPPERATION
            ((ISQ H WILL) (GO WILL))                                ;; OF THIS FUNCTION) DISPATCH TABLE , CHECKING THE
            ((ISQ H BE) (GO BE))                                    ;; AUX
            ((ISQ H HAVE) (GO HAVE)))
        (ERT BUG "VG:POLR2VB")                                        ;; NOTHING BUT UNGRAMATICAL NONSENSE SHOULD REACH
        (GO FAIL)                                                   ;; THIS POINT

    ;; ###############################################################
    ;; PROCESSING OF VB'S NOT SPECIALLY MARKED FOR BY INITIAL FEATURES
    ;; ###############################################################

    DO  (FQ DO)
        (MOVE-PT C DLC)                                             ;; MOVE TO THE "DO"
        (TRNSF VPL NEG INF V3PS)                                    ;; ARRANGE ITS FEATURES
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
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
        (SETQ TENSE '(MODAL))
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
        (SETQ TENSE '(FUTURE))
        (GOCOND MODAL2 INCOMP)                                      ;; THE SAME POSSIBILITIES FOR THE NEXT VERB APPLY
                                                                    ;; AFTER BOTH WILL AND MODALS

    BE
        (MOVE-PT C DLC)                                             ;; POINT TO WHAT WAS JUST PARSED
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (GOCOND BE2 MVB)

    BE2
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))

    ADV4
        (| (or (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV4 nil (ADV))
        (COND ((and (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))      ;; "...WILL BE GOING TO..."
            ((and (NQ BE) (PARSE VB ING))                           ;; "BE BEING"
                (SETQ TENSE (cons 'PRESENT TENSE))
                (GO EN2))                                           ;; AS IN "BE BEING X'EN(ED)"
            ((and (NQ ING) (PARSE VB ING (MVB)))                    ;; "BE X'ING"
                (SETQ TENSE (cons 'PRESENT TENSE))
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
        (SETQ TENSE (cons 'PRESENT TENSE))                          ;; WE HAVE DETERMINED THAT "GOING" IS THE ACTUAL
        (GO MVB)                                                    ;; MAIN VERB AND SHOULD BE PARSED AS SUCH

    GOING2
        (SETQ TENSE (cons 'FUTURE TENSE))                           ;; HERE WE DETERMINE THAT THE PHRASE IS ACTUALLY
        (GO MODAL2)                                                 ;; OF THE FORM "...IS GOING TO FALL IN LOVE..."
                                                                    ;; AND WE SHOULD RUN THROUGH THE DISPATCH TABLE AT
                                                                    ;; "MODAL2" TO DETERMINE HOW TO CONTINUE

    MVBE
        (| (ISQ (MOVE-PT H PV (VB)) AUX) nil MVB)                   ;; MOVE TO EARLIER AND EARLIER DAUGHTERS  UNTILL
        (| (ISQ PT BE) nil (MVBE))                                  ;; YOU REACH A VERB WHICH IS A "QAUX" - IF THERE
                                                                    ;; ARE NONE THEN CONTINUE AT "MVB" IF WHAT YOU ARE
                                                                    ;; POINTING TO (THE "QAUX") IS NOT A FORM OF "BE",
        (SETMVB PT)                                                 ;; THEN FAIL BECAUSE OF THE UNGRAMATICALITY OF THE
        (GO REV)                                                    ;; CONSTRUCTION OF "BE"'S OTHERWISE MARK IT AS THE
                                                                    ;; MVB AND PREPARE TO RETURN

    HAVE
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST)) (:else '(PRESENT))))
        (GOCOND HAV2 MVB)                                           ;; HAV2 WILL CATCH "HAVE HAD", OR "HAVE BEEN ..."

    HAV2
        (| (and (PARSE nil NOT) (FQ NEG)) nil nil (NOT))            ;; OR "HAVE KISSED"

    ADV5
        (| (PARSE ADV) ADV5 nil (ADV))
        (| (PARSE VB BE EN) nil HAV3)
        (SETQ TENSE (cons 'PAST TENSE))                             ;; "HAVE BEEN ..."
        (GOCOND BE2 MVB)

    HAV3
        (| (PARSE VB (MVB) EN) nil MVB)
        (SETQ TENSE (cons 'PAST TENSE))                             ;; "HAVE KISSED"
        (GO REV)

    INCOMP
        (FQ INCOMP)
        (GO FAIL)

    MVB
        (| (EQ (FE MVB) (FE H)) MVB2 nil)
        (POP VB)                                                    ;; POP OFF EVERY THING UNTILL YOU REACH A VERB
        (| (PARSE VB (MVB)) nil (MVB))

    MVB2
        (GO REV)

    ;; -------------------------------------------------
    ;;   CHECK AGREEMENT BETWEEN SUBJECT AND MAIN VERB
    ;; -------------------------------------------------

    REV
        (SETR 'TENSE TENSE C)
        (and NN (PARSE nil NOT) (FQ NEG))
        (COND ((or (EQUAL TENSE '(PAST))
                    (CQ NAGR)
                    (ISQ (MOVE-PT C U) IMPER)                       ;; MOVE PT TO THE CLAUSE REMEMBER THAT THE POINTER
                    (ISQ PT THERE)                                  ;; STAYS WHERE IT'S PUT UNTILL RETURNING FROM A
                    (ISQ PT RSNG))                                  ;; CALL TO PARSE
                (GO NAUX))
            ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))               ;; "SUBJECT" IS THE SYNTACTIC SUBJECT OF THE
            (:else (ERTERR VG -- NO SUBJECT TO CHECK FOR AGREEMENT)))   ;; CLAUSE THAT THE VG IS IN, WHOSE ESSENTIAL
                                                                    ;; DISTINGUISHING FEATURE IS AGREEMENT WITH THE VERB

        (SETQ T3 nil)                                               ;; T3 WILL ACT AS A SWITCH AT "NAGR" BELOW. NOTE
                                                                    ;; THAT IT IS EXPLICITLY SET BY THE CODE BELOW BY
                                                                    ;; THE FOLLOWING CRITERIA;   IF T3 IS NON-NIL THEN
                                                                    ;; SUBJECT AND VERB HAVE BEEN DETERMINED TO AGREE
                                                                    ;; IF IT IS NIL THEN THEY WILL BE CONSIDERED TO
                                                                    ;; AGREE ONLY IF THE FEATURE "PAST-PRESENT" IS ON
        (COND ((ISQ PT NFS)                                         ;; THE MVB, IN WHICH CASE, THIS IS EVIDENCE THAT
                (or (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))      ;; THE PROPER CHOISE OF TENSE IS PAST - WHERE
            ((ISQ PT CLAUSE) (or (SETQ T3 (CQ V3PS)) (GO NAGR)))    ;; AGREEMENT IS IRRELEVANT (SEE BELOW AT "NAGR")
            ((or (ISQ PT NS) (ISQ PT MASS))
                (or (and (CQ V3PS) (SETQ T3 true))
                    (FESET PT (SETDIF (FE PT) '(NS MASS))))))
        (COND ((or (ISQ PT PART) (ISQ PT NPL))
            (or (and (MEET FE '(INF VPL)) (SETQ T3 true))
                (FESET PT (SETDIF (FE PT) '(PART NPL))))))

    NAGR
        (| (or T3
            (and (EQUAL '(PAST-PRESENT) TENSE)                      ;; NOTES WHETHER VERB AND SUBJECT WERE FOUND TO
                (SETQ TENSE '(PAST))))                              ;; AGREE AND FAILS UNLESS A SPECIAL CONDITION
            nil                                                     ;; EXISTS AS NOTED DIRECTLY ABOVE
            (NAGR))

    NAUX
        (SETMVB (or (MOVE-PT H PV (MVB)) MVB))
        (| (and (CQ NAUX) (ISQ (MOVE-PT H PV (VB)) AUX) (not (MOVE-PT PV PV (VB))))
            (NAUX)
            RETSM)                                                  ;; THE VB MAY HAVE THE FEATURE "NAUX" WHICH
                                                                    ;; INDICATES THAT IT CAN NEVER SERVE AS THE
                                                                    ;; AUXILLIARY OF ANOTHER VERB. IF THE PRESENT
                                                                    ;; PARSING REQUIRES IT TO THEN IT FAILS WE CHECK
                                                                    ;; BY SEEING IF THE VG CONTAINS ONLY ONE VERB,
                                                                    ;; WHICH IS AN AUX.

    POPV
        (ERT POPV)
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
        (MOVE-PT H)
        (TRNSF PLACE TIME)                                          ;; THIS IS NOT EXACTLY RIGHT, SINCE "ON WHAT DAY" IS NOT "PLACE"

        ;; AT THIS POINT THE POSSIBILITIES ARE:
        ;;   1. THERE ARE NO MORE WORDS AND THE PREP IS "SHORT"
        ;;   2. YOU HAVE A MULTIPLE WORD PREPOSITION
        ;;   3. IT IS INDEED A SINGLE WORD PREP, PARSE ITS OBJECT

        (SETQ T1 H)                                                 ;; SAVE THE PREPOSITION JUST PARSED IN CASE IT IS
        (and (NQ PREP2)                                             ;; ONLY THE FIRST WORD OF A MULTIPLE WORD PREPOSITION
            (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N)))
                    (PARSE PREP2))
                ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N) (WORD (cdr N))))
                    (PARSE PREP2)
                    (PARSE PREP2)))
            (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))        ;; CREATE NODE FOR THE COMPOUND WORD
            (SETR 'PARENT C T1))
        (| (ISQ H NEED2) (NEED2) nil)                               ;; FAIL IF LAST PARSED NEEDS ANOTHER WORD

                                                                    ;; GIVE IT A PARENT
        (SETR 'HEAD T1 C)                                           ;; SET THE REGESTER "PREP" TO THE CONSTITUENT JUST
                                                                    ;; PARSED - IF IT WAS A MULTIPLE-WORD-PREP THEN
        (or NN (GO SHORT))                                          ;; "PREP" IS SET TO THE NODE WHICH CONTAINS THE
                                                                    ;; ENTIRE FORM NN POINTS TO WHATEVER WORDS ARE
                                                                    ;; LEFT BEFORE THE CUT POINT

        ;; ADD FEATURES TO THE PREPG DEPENDING ON THE PREPOSITION PARSED

        (COND ((EQ (WORD H) 'BY) (FQ AGENT)))

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
        (SETR 'OBJ1 (GETR 'HEAD PT) C)                              ;; THE REGISTER IS ACCESSED BY CODE IN THE PASSIVE
        (GO RETT)                                                   ;; SECTION OF CLAUSE AND BY THE APPROPRIATE

    REST
        (| (PARSE CLAUSE RSNG ING) OBJR SHORT)                      ;; SEMANTIC SPECIALIST "HEAD" IS HERE THE HEAD OF

    OBJR
        (SETR 'OBJ1 H C)                                            ;; THE HIGHER NOUNGROUP
        (GO RETT)

    SHORT
        (| (MEET FE '(NOSHORT Q)) (SHORT) nil)
        (or (ISQ (MOVE-PT C U) REL-NOT-FOUND)
            (ISQ (GETR 'QUESTION-ELEMENT PT) QADJ)
            (GO FAIL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PREPREL PT)
        (SETR 'OBJ1 (GETR 'RELHEAD (MOVE-PT C U)) C)

    ;; IF THE REFERENT OF THE RELATIVE CLAUSE THIS SHORT
    ;; PREPOSITION IS ASUMED TO BE IN, HAS NOT BEEN DETERMINED,
    ;; THEN SET THE REGISTER FOR THE OBJECT OF THE PREP.  TO THE
    ;; RELWORD.  IF THERE IS NO RELWORD THEN THE PREPG FAILS
    ;; AFTER SENDING UP A COMPLAINING MESSAGE.

    ;; ---------------  FINAL CHECKS, AND RETURN ---------------

    ;; CHECK IF THIS PREPG SHOULD BE MARKED AS CONTAINING A QUESTION ELEMENT.
    ;; IE. "FOR WHAT", "BETWEEN THE RED BLOCK AND WHICH?" (ECHO)

    RETT
        (and (or (ISQ H QUEST)                                      ;; H IS THE NG FOUND FOR AN OBJECT
            (and (ISQ H COMPOUND)                                   ;; IF THE NOUN GROUP IS COUMPOUND, CHECK EACH
                (MOVE-PT H H PV (QUEST))))                          ;; COMPONENT FOR THE FEATURE "QUEST"
            (FQ QUEST))
        (| (CALLSM (SMADJG-PREPG)) RETURN FAIL))

(§ PDEFINE ADJG nil

    ENTERING-ADJG                                                   ;; THIS LABEL IS MARKED BY DEBUGGING ROUTINES AND

    COMPCHECK                                                       ;; IS USEFUL FOR FOLLOWING THE FLOW OF CONTROL
        (| (and (MOVE-PT C U (BE)) (not (CQ COMP))) FAIL nil)       ;; CONDITIONS WHICH MUST BE MET BY ANY ADJECTIVE
                                                                    ;; GROUP IF THERE IS A FORM OF "BE" IN THE HIGHER
                                                                    ;; CLAUSE, THEN THE ADJG SHOULD HAVE BEEN CALLED
                                                                    ;; WITH THE FEATURE "COMP" FOR COMPLIMENT

        ;; EXAMINE THE INITIAL FEATURES (THOSE DESIGNATED BY THE
        ;; CALLING PROGRAM) ALSO EXAMINE THE NEXT WORD - THESE GIVE
        ;; CLUES AND CONSTRAINTS TO THE STRUCTURE TRYING TO BE PARSED
        ;; AND DIRECT JUMPS TO THE APPROPRIATE SECTIONS OF CODE

        (| (ISQ (MOVE-PT C U) THAN) nil DISP)                       ;; THE WORD "THAN" WAS DETECTED BY THE IMMEDIATELY
                                                                    ;; UPSTAIRS NG AS FOLLOWING THE HEAD NOUN
        (SETR 'HEAD (GETR 'COMPARATIVE-MODIFIER PT) C)              ;; INDICATING A STURCTURE SUCH AS "... A BIGGER
        (GO THAN)                                                   ;; BLOCK THAN THAT ONE ..." "HEAD REFERS TO THE
                                                                    ;; ADJG'S HEAD ADJECTIVE

    DISP
        (| (and (NQ AS) (PARSE nil AS)) AS nil (AS))
        (| (and (NQ AS) (PARSE nil AS)) AS nil (AS))
        (| (NEXTWORD? 'HOW) HOW ADV)

    ;; --------------- HOW + ADJG ----------------

    HOW
        (| (and (PARSE nil HOW) (FQ QUEST)) nil FAIL FAIL)
        (| (and (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C)) RETSM nil)
        (| (and (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C)) RETSM FAIL)

    ADV
        (| (PARSE ADV ADVADV) ADV nil POPAD)                        ;; THIS LOOPS UNTILL ALL CONTIG- UOUS ADVERBS HAVE
        (| (PARSE nil MORE) nil ADJ)                                ;; BEEN PARSED "MORE" IS EXPLICITLY CHECKED FOR
        (FQ COMPAR)                                                 ;; SINCE IT SIGNALS THE FEATURE, COMPARATIVE

    ADJ
        (| (COND ((CQ ADV) (PARSE ADV VBAD)) (:else (PARSE ADJ)))
            nil
            (ADJ))                                                  ;; IF THE CUT POINT WAS REACHED THEN NO MORE
        (| (SETR 'HEAD H C) nil nil RETSM)                          ;; PROCESSING (SUCH AS COMPAR BELOW) IS POSSIBLE.

    ;; -------------------------------------------
    ;;               COMPARATIVES
    ;; -------------------------------------------

    ;; IF THE FEATURE "COMPAR" IS ALREADY ON THE LIST, OR IF THE JUST PARSED ADJECTIVE CAN HAVE THAT FEATURE, THEN
    ;; ATTEMPT TO PARSE SOME SORT OF COMPARATIVE CONSTRUCTION (ASSUMING THAT THEREARE ANY MORE WORDS BEFORE THE CUT POINT.)

        (| (or (CQ COMPAR) (ISQ H COMPAR)) nil RETSM)
        (FQ COMPAR)
        (| NN nil RETSM)                                            ;; IF THERE ARE UNPARSED WORDS LEFT BEFORE THE CUT
                                                                    ;; POINT THEN THE POSSIBILITY OF MORE COMPLICATED
                                                                    ;; FORMS IS CHECKED FOR

    THAN
        (COND ((not NN) (GO RETSM)))
        (| (PARSE nil THAN) nil RETSM (THAN))
        (RQ THANNEED)                                               ;; THE FEATURE "THANNEEED" MARKS THAT THE WORD
        (FQ THAN)                                                   ;; "THAN" IS EXPLICITLY  REQUIRED IN THE PHRASE.
        (GO SUBJ)

    AS
        (FQ AS)
        (RQ THANNEED)
        (| (and (PARSE ADJ) (SETR 'HEAD H C)) nil (ADJ) RETSM)
        (| (PARSE nil AS) SUBJ RETSM (AS))

    ;; FIND A SUBJECT FOR THE COMPARATIVE
    ;; IE.  "AS BIG AS ..." , "BIGGER THAN ..."

    SUBJ
        (| (PARSE NG SUBJ COMPAR) nil (THAN))
        (| (SETR 'OBJ1 H C) nil nil RETSM)
        (| (and (ONE-WORD-LEFT) (PARSE VB AUX)) nil RETSM)
        (| (CHECK-AGREEMENT H (cdr H))                              ;; CHECKS FOR AGREEMENT IN NUMBER AND PERSON
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

(§ DEFUN CONJ []
    (let [END nil GOODIE nil]
        (SETQ END CUT)
        (COND ((SETQ GOODIE (APPLY-GRAMMAR 'CONJOIN))
                (RETURN (SETQ RE GOODIE)))
            (:else (RETURN nil)))))

(§ DEFUN COMMA []
    (COND ((SECONDWORD? '\") (FLUSHME) true)                           ;; IF " FOLLOWS, FLUSH COMMA AND CONTINUE
        ((CONJ))                                                    ;; IF COMMA IS PART OF CONJOINED STRUCTURE, GREAT
        ((ISQ RE INIT) (FLUSHME) true)))                               ;; IF COMMA FOLLOWS INITIAL-TYPE PHRASE, FLUSH IT AND CONTINUE DIRECT ADDRESS JAZZ

(§ PDEFINE CONJOIN (PREV)

    ;; THIS PROGRAM IS CALLED TO PARSE A CONJOINED STRUCTURE THE
    ;; FIRST MEMBER OF THE SUPPOSED STRUCTURE HAS ALREADY BEEN
    ;; PARSED AND IS SET TO THE INITIAL DAUGHTER (H) OF THIS NODE
    ;; AN ATTEMPT IS MADE TO PARSE AS MANY CONSTITUENTS JUST LIKE
    ;; IT AS IS POSSIBLE

    ENTERING-CONJOIN                                                ;;  HACK LABEL FOR LABELTRACER

    UP
        (SETQ PREV (NEXTWORD))
        (FLUSHME)
        (COND ((and (EQ PREV '\,)                                   ;; IF WE HAVE A COMMA AND
                (or (cdr H)                                         ;; IF MORE THAN 1 COMPONENT HAS BEEN PARSED
                (> (- (count (NB H))               ;; OR IF THAT ONE COMPONENT
                            (count (N H)))                         ;; IS MORE THAN 4 WORDS LONG
                    4))
                (MEMQ (NEXTWORD) '(or AND NOR BUT))
                (F (NEXTWORD)))                                     ;; THEN CHECK FOR COMMA COMBINATION
            (SETQ PREV (list PREV (NEXTWORD)))
            (FLUSHME)))
        (and (ATOM PREV)
            (MOVE-PTW N NW (EQ (WORD PTW) PREV))
            (CUT PTW))
        (and (or (EQ PREV 'BUT) (EQ (cadr PREV) 'BUT))
            (NEXTWORD? 'NOT)                                        ;; CHECK FOR BUT-NOT COMBINATION
            (or (FLUSHME) (GO LOSE2))
            (FQ NEGBUT))
        (| (COND ((MEMQ (car REST) '(ADJ NUM NOUN PREP VB ADV))
                (PARSE3 (concat REST '(COMPONENT)) nil))
            ((MEMQ (car REST) '(NG PREPG ADJG))
                (and (not (CQ OFOBJ)) (PARSE2 (concat REST '(COMPONENT)) nil)))
            ((EQ (car REST) 'CLAUSE)
                ((lambda (LASTSENT AUXFE)
                    (and (PARSE2 (concat REST AUXFE '(COMPONENT)) nil)
                        (or (not AUXFE) (F (car AUXFE)))
                        (SETR 'TIME (GETR 'TIME H) C)))             ;; MARK COMPOUND CLAUSE AS TO DECLAR/IMPER FOR ANSGEN
                    (COND ((ISQ H MAJOR) H) (LASTSENT))
                    (MEET (FE H) '(DECLAR IMPER)))))
            nil
            LOSE2)
        (CUT END)                                                   ;; RESTORE CUT POINT
        (COND ((not (ATOM PREV))
                ;; IF WE HAD COMMA FOLLOWED BY (AND OR BUT NOR), RETURN THE LIST OF GOODIES WE'VE FOUND
                (GO RETSM))
            ((EQ PREV '\,)
                (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP))
                    (:else (GO LIST))))
            ((MEMQ PREV '(and OR NOR BUT))
                (COND ((EQ BOTH (NB H)) (FQ BOTH)))
                (COND ((or (NEXTWORD? 'BUT)
                        (and (NEXTWORD? PREV)
                            (not (and (EQ BOTH (NB H))              ;; IF WE HAD THE 'BOTH' WORD AND
                                (EQ PREV 'AND)))))                  ;; IF THE 'BOTH' WORD WAS "AND", STOP PARSING
                        (FQ LISTA)                                  ;; ELSE GO LOOK FOR THE NEXT COMPONENT
                        (F PREV)
                        (GO UP))
                    (:else (GO LISTA)))))

    LOSE2
        (| (CQ LISTA) LISTA nil)

    LIST
        (| (and (EQ PREV '\,)                                       ;; COME HERE FOR ABORTED LIST AND CHECK FOR APPOSITIVE
                (== (count H) 2)
                (ISQ H NG)
                (not (or (ISQ H PRONG) (ISQ (cdr H) PRONG)))
                (or (NEXTWORD? COMMA) (nil? N)))
            nil
            (CONJOINß HOPELESS LIST))
        (FLUSHME)                                                   ;; GET RID OF TRAILING COMMA
        (FQ APPOSITIVE)
        (GO RETSM)

    LISTA
        (F PREV)

    RETSM
        (FQ COMPOUND)                                               ;; CALL SEMANTICS AND RETURN EVERY PARSED BY THIS
        (and (> (count H) 2) (FQ LIST))                     ;; GOODIE IS COMPOUND IF MORE THAN 2 COMPONENTS
        (COND ((or (CQ NG) (CQ NOUN))
                (COND ((CQ AND) (FQ NPL))
                    (:else (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
            ((CQ VB)
                (let [COMMON nil]
                    (SETQ COMMON (GET 'VB 'ELIM))
                    (MAP #'(lambda (X)
                            (SETQ COMMON (MEET COMMON (FE X))))
                    H)
                    nil)
                (FESET (UNION COMMON (FE C)) C)))
        (| (CALLSM (SMCONJ)) RETURN (CONJOINß SMCONJ)))             ;; THEN MARK AS A LIST

(§ DEFUN BOTH FEXPR [A]
    ;; HANDLES (BOTH AND) (EITHER OR) (NEITHER NOR) COMBINATIONS
    ;; THE CONJOIN PROGRAM DOES SPECIAL THINGS WHEN BOTH IS SET
    (let [END nil]
        (SETQ END CUT)                                              ;; MAKE END OUT OF PREVIOUS CUT POINT
        (RETURN (let [CUT nil NBB nil BOTH nil]
            (SETQ NBB N)
            (and (FLUSHME)
                (MOVE-PTW N NW (EQ (WORD PTW) (car A)) NW)          ;; LOOK FOR THE MATCHING WORD E.G. AND, OR, NOR
                (CUT END)
                (SETQ BOTH PTW)                                     ;; SAVE POINTER TO THE WORD AFTER THE MATCHING WORD
                (SETQ RE (COND
                    ((MEMQ (car REST) '(PREP ADV)) (PARSE3 REST true))
                    ((MEMQ (car REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST true))))
                (< (count N) (count BOTH))                    ;; FAIL UNLESS WE PARSED BEYOND MATCHING WORD
                (RETURN (SETQ SPECIAL 'SKIP)))
            (SETQ RE nil)
            (SETQ N NBB)
            (RETURN nil)))))

(§ DEFUN DOUBLEQUOTER [] (APPLY-GRAMMAR 'PARSEQUOTED))

(§ DEFUN CANTAKE [NUM TYPE FEATURE]
    (let [VBFEAT nil]
        (SETQ VBFEAT (FE MVB))
        (RETURN (COND
            ((MEMQ 'RSNG TYPE)
                (MEMQ
                    (READLIST (concat
                        (COND ((MEMQ 'TO TYPE) '(T O)) ((MEMQ 'ING TYPE) '(I N G)) ((MEMQ 'REPORT TYPE) '(R E P)))
                        '(O B)
                        (list (COND ((EQ NUM 1) '\1) (:else '\2)))))
                    VBFEAT))
            ((MEMQ 'COMP TYPE)
                (MEMQ 'INT VBFEAT))
            ((MEMQ 'NG TYPE)
                (COND ((== NUM 1)
                    (not (nil? (MEET '(TRANS TRANS2 TRANSL TRANSINT) VBFEAT))))
                (:else (MEMQ 'TRANS2 VBFEAT))))
            (:else (MEMQ FEATURE VBFEAT))))))

(§ DEFUN CANPARSE [NUM TYPE FEATURE]
    (let [REG nil]
        (and (CANTAKE NUM TYPE FEATURE)
            (or (nil? TYPE)
                (and (APPLY #'PARSE
                    (concat TYPE
                        (COND ((MEMQ 'COMP TYPE) (SETQ REG 'COMP) nil)
                            (:else (list 'OBJ
                                (SETQ REG (COND
                                    ((or (MEMQ 'LOC TYPE) (MEMQ 'PLACE TYPE)) 'LOBJ)
                                    ((== NUM 1) 'OBJ1)
                                    (:else 'OBJ2))))))))
                    (SETR REG H C)))
            (or (nil? FEATURE) (F FEATURE))
            (RETURN true))
        nil))

#_(ns shrdlu.dictio)

;; ###########################################################
;;
;;                          WORDS
;;
;; ###########################################################

(§ DEFS \, FEATURES (SPECIAL) SPECIAL (COMMA))

(§ DEFS \" FEATURES (B-SPECIAL RELWRD) B-SPECIAL (DOUBLEQUOTER)) ;; "sic!

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

(§ DEFUN !BETHERE []
    (RELATION (RESTRICTIONSß (((!THING) (EQ (QUANTIFIER? SMSUB) 'INDEF))) PROCEDUREß nil)))

(§ DEFUN !BEINT []
    (COND
        ((RELATION
            (RESTRICTIONSß (((!PHYSOB)) (SMCOMP (!PROPERTY)))
                PROCEDUREß (!EVAL (let [PROPERTY nil]
                                    (COND
                                        ((SETQ PROPERTY (MEET (GET '!PROPERTY 'SYSTEM) (MARKERS? SMCOMP)))
                                            (RETURN (list (list (car PROPERTY) '!1 '!2))))
                                    ((RETURN (list '(!2 !1))))))))
            (RESTRICTIONSß (((!THING)) (SMCOMP (!SYSTEMS) (and (not (REFER? SMCOMP)) (EQ (REL? SMCOMP) SMSUB))))
                PROCEDUREß (!EVAL (RELATIONS? SMCOMP)))
            (RESTRICTIONSß (((!THING)) (SMCOMP (!THING) (REFER? SMCOMP)))
                PROCEDUREß ((!EVAL (list 'THAMONG '!1 (list 'quote (REFER? !2))))))))
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
    B-SPECIAL (BOTH AND)
    FEXPR (lambda (A)
        (let [END nil]
            (SETQ END CUT)
            (RETURN
                (let [CUT nil NBB nil BOTH nil]
                    (SETQ NBB N)
                    (and (FLUSHME)
                        (** N NW (EQ (WORD PTW) (car A)) NW)
                        (CUT END)
                        (SETQ BOTH PTW)
                        (SETQ RE
                        (COND ((MEMQ (car REST) '(PREP ADV)) (PARSE3 REST true))
                            ((MEMQ (car REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST true))))
                        (< (count N) (count BOTH))
                        (RETURN (SETQ SPECIAL 'SKIP)))
                    (SETQ RE nil)
                    (SETQ N NBB)
                    (RETURN nil))))))

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
            PLAUSIBILITYß (!EVAL (or (GET MAP2 'REFER) (ERT DO DEFINITION)))))))))
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

(§ DEFS FRIEND REFER :FRIEND FEATURES (NOUN NS))

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
    SEMANTICS ((ADJ (OBJECT (MARKERSß (!THING) PROCEDUREß ((!LIKE :FRIEND ***))))))
    FEATURES (ADJ))

(§ DEFS NO SEMANTICS ((DET 'NO)) FEATURES (DET QNTFR NS NPL))

(§ DEFS NONE
    SEMANTICS ((DET 'NO))
    FEATURES (DET QNTFR NPL NS NONUM))

(§ DEFS NOR FEATURES (SPECIAL) SEMANTICS true SPECIAL (CONJ))

(§ DEFS NOT SEMANTICS ((ADV true)) FEATURES (ADV NEG))

(§ DEFS NOTHING SEMANTICS ((TPRON 'NO)) FEATURES (TPRON NEG NS))

(§ DEFS NOW
    SEMANTICS ((ADV (or (EQ (cadr (ASSQ 'TIME FE)) ':NOW) (ERT NOW DEFINITION))))
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
                                (cadr (SETQ XX (or (ASSQ '!DIRECTION (cddaar (INTERP MAP1))) (ERT OF DEFINITION))))
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
             PROCEDUREß ((!EVAL (COND ((MEMQ (NUMBER? SMOB1) '(1 NS)) '(!PICKUP !2 *TIME)) ('(!PUTIN !2 :BOX *TIME)))))))))
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
                   (doall (map #'(lambda (%PLNRPHRASE)
                        (COND ((EQ (car %PLNRPHRASE) '!ON) (list '!PUTON '!2 (cadr %PLNRPHRASE) '*TIME))
                            ((EQ (car %PLNRPHRASE) '!IN) (list '!PUTIN '!2 (cadr %PLNRPHRASE) '*TIME))
                            ((ERT PUT DEFINITION))))
                        (RELATIONS? SMOBL))))))))))
    FEATURES (INF PAST VB TRANSL VPRT))

(§ DEFS PUT-AWAY
    ROOT (PUT AWAY)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTIN !2 :BOX *TIME))))))
    FEATURES (COMBINATION TRANS))

(§ DEFS PUT-DOWN
    ROOT (PUT DOWN)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTON !2 :TABLE *TIME))))))
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
    SEMANTICS ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTON !2 :TABLE *TIME))))))
    FEATURES (COMBINATION TRANS))

(§ DEFS SHAPE
    SEMANTICS ((NOUN (OBJECT (MARKERSß (!SHAPE) PROCEDUREß ((!IS *** !SHAPE))))))
    FEATURES (NOUN NS))

(§ DEFS SHE FEATURES (PRON SUBJ NS))

(§ DEFS SHORT
    SEMANTICS ((MEASURE (MEASURE DIMENSIONß !HEIGHT RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !HEIGHT (128 0 0) ***))))))
    FEATURES (ADJ))

(§ DEFS SHRDLU REFER :SHRDLU)

(§ DEFS SINCE SEMANTICS ((BINDER (SMBINDER END nil))) FEATURES (BINDER TIME))

(§ DEFS SIT
    SEMANTICS ((VB
        ((ITRNSL (RELATION
              (RESTRICTIONSß (((!PHYSOB)) (SMOBL (!PLACE)))
               PROCEDUREß (!EVAL
                   (doall (map #'(lambda (%PLNRPHRASE)
                        (COND ((MEMQ (car %PLNRPHRASE) '(!ON !IN)) (list '!SUPPORT (cadr %PLNRPHRASE) '!1 '*TIME))
                            ((ERT SIT DEFINITION))))
                        (RELATIONS? SMOBL))))))))))
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

(§ DEFUN THANK []
    (COND ((EQ (cadr N) 'YOU)
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
             (RPLACD (cddadr (or (and (SETQ XX (ASSQ 'TIME FE)) (not (ATOM (cadr XX))) XX) '(TIME (!TIME (PAST) nil))))
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

(§ DEFS WE REFER (:SHRDLU :FRIEND))

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

(§ DEFS !BLUEPRINT
    EXPR (lambda (X)
        (let [PARTS nil]
            (COND ((GET X 'REFER) (RETURN '!2))
                ((nil? (SETQ X (cddaar (INTERP X))))
                    (GO DONE)))
        LOOP (COND ((not (EQ (caar X) 'THGOAL)) (ERT BLUEPRINT THGOAL))
                ((EQ (caadar X) '!IS))
                ((EQ (caadar X) '!PART) (SETQ PARTS (cons (cadr (cadar X)) PARTS)))
                ((ERT !BLUEPRINT)))
            (and (SETQ X (cdr X)) (GO LOOP))
        DONE (and PARTS
                (GET (car PARTS) 'REFER)
                (RETURN (GET (car PARTS) 'REFER)))
            (PUTPROP 'BLUEPRINT
                (COND ((nil? PARTS) (GET 'STACKPARTS 'SM))
                    ((cdr PARTS) (ERT !BLUEPRINT PARTS))
                    ((GET (car PARTS) 'SM)))
                'SM)
            (RETURN 'BLUEPRINT))))

(§ DEFS !BOX SYS (!PHYSOB))

(§ DEFS !BUILD
    EXPR (lambda nil
        (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!STACK))) MARKERSß (!EVENT) PROCEDUREß ((!EVAL (list '!STACKUP (!BLUEPRINT SMOB1) '*TIME)))))))

(§ DEFS !CALL THMLIST ((3 '((THUSE TC-3)))))

(§ DEFS !COLOR
    FEXPR (lambda (A)
        (eval (SUBST (car A) 'COLOR '(OBJECT (MARKERSß (!PHYSOB COLOR) PROCEDUREß ((!COLOR *** COLOR)))))))
    PRIORITY 192
    SYS (!PROPERTY))

(§ DEFS !CONSTRUCT SYSTEM (!STACK !ROW) SYS (!PHYSOB))

(§ DEFS !CONTAIN PRIORITY -1)

(§ DEFS !CLEANOFF
    EXPR (lambda nil
        (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!CLEARTOP !2 *TIME))))))

(§ DEFS !CLEARTOP
    NAMEVENT (I3 (cons (VBFIX 'CLEAN nil) (PRTPUT 'OFF OBJ1)))
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(§ DEFS !DEFINE
    EXPR (lambda (X Y)
        (list '!DEFINITION
            (cadadr (cdaar (INTERP X)))
            (let [X nil]
                (PUTPROP (SETQ X (MAKESYM 'ATM)) (INTERP Y) 'NEWWORD)
                (RETURN X)))))

(§ DEFS !DEFINITION
    FEXPR (lambda (A)
        (PUTPROP (cadar A) '(NOUN NS) 'WORD)
        (PUTPROP (cadar A)
            (SUBST (SUBST '*** (caddr (GET (cadr A) 'NEWWORD)) (car (GET (cadr A) 'NEWWORD)))
                'NG
                '((NOUN (SETQ LIST2 (list (SUBST (SUBST (caddar LIST1) '*** 'NG) (caar LIST1) (car LIST1)))))))
            'SMNTC))
    NOGOAL true)

(§ DEFS !DIRECTION NOGOAL true)

(§ DEFS !END
    THMLIST ((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(§ DEFS !EQDIM
    EXPR (lambda (X)
            (SETQ X (SIZE X))
            (and (EQ (car X) (cadr X)) (EQ (car X) (caddr X))))
    NOGOAL true)

(§ DEFS !EQUIV PRIORITY 512)

(§ DEFS !EVENT SYS (!SYSTEMS))

(§ DEFS !EXISTS
    THMLIST ((2 '((THUSE TC-EXISTS))) (3 '((THUSE TCT-EXISTS)))))

(§ DEFS !GET-RID-OF
    THMLIST ((2 '((THUSE TCT-EXISTS))) (3 '((THUSE THUSE TCT-3))) (4 '((THUSE TCTE-4))))
    NAMEVENT (I3 (concat (list (VBFIX 'GET true) 'RID 'OF) OBJ1)))

(§ DEFS !GRASP
    EXPR (lambda nil
        (RELATION
            (RESTRICTIONSß (((!ANIMATE)) ((!MANIP)))
            MARKERSß (!EVENT)
            PROCEDUREß ((!EVAL (COND ((IMPERF) '(!GRASPING !2 *TIME)) ('(!GRASP !2 *TIME))))))))
    NAMEVENT (I3 (cons (VBFIX 'GRASP nil) OBJ1))
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(§ DEFS !GRASPING THMLIST ((3 '((THUSE TCT-GRASPING)))))

(§ DEFS !GREEN SYS (!SPECTRUM))

(§ DEFS !HAND SYS (!PHYSOB))

(§ DEFS !HAVE
    EXPR (lambda nil
        (RELATION
            (RESTRICTIONSß (((!THING)) ((!THING) (and (MEMQ '!ROLE (MARKERS? SMOB1)) (CHECK (cadr (ASSOC '!ROLE (RELATIONS? SMOB1))) (MARKERS? SMSUB) (SYSTEMS? SMSUB)))))
            PROCEDUREß ((!SUBST !1 ?)))
            (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB)))
            PROCEDUREß ((!BELONG !2 !1))))))

(§ DEFS !HEIGHT MEASFN (lambda (X) (caddr (SIZE X))))

(§ DEFS !IN
    EXPR (lambda nil
        (COND ((CQ LOBJ)
            (RELATION
                (RESTRICTIONSß (((!THING)) ((!BOX))) MARKERSß (!PLACE) PROCEDUREß ((!IN !2)))))
            ((RELATION
                (RESTRICTIONSß (((!MANIP)) ((!BOX))) PROCEDUREß ((!CONTAIN !2 !1 *TIME)))
                (RESTRICTIONSß (((!MANIP)) ((!HAND))) PROCEDUREß ((!GRASPING !1 *TIME)))
                (RESTRICTIONSß (((!PLACE)) ((!BOX))) PROCEDUREß ((!IN !1 !2)))
                (RESTRICTIONSß (((!MANIP)) ((!CONSTRUCT))) PROCEDUREß ((!PART !1 !2 *TIME))))))))

(§ DEFS !IS PRIORITY 64)

(§ DEFS !LIKE TELLABLE true THMLIST ((3 '((THTBF THTRUE)))))

(§ DEFS !LOC
    THMLIST ((4 '((THUSE TC-LOC))) (5 '((THUSE TCT-LOC))))
    FEXPR (lambda (A) (!LOC2 (car A) (cadr A))))

(§ DEFS !LOC2
    EXPR (lambda (LOCTYPE !LOC)
        (COND ((CQ LOBJ)
            (RELATION (RESTRICTIONSß (((!THING)) (LOBJ (!PHYSOB))) MARKERSß (!PLACE) PROCEDUREß ((!EVAL (list '!LOC LOCTYPE !LOC !2))))))
        ((RELATION (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PROCEDUREß ((!EVAL (list '!LOC LOCTYPE (COND (!LOC '!1) ('!2)) (COND (!LOC '!2) ('!1)) '*TIME)))))))))

(§ DEFS !MANIP SYS (!PHYSOB))

(§ DEFS !MORE THMLIST ((4 '((THUSE TC-MORE)))))

(§ DEFS !NAME
    THMLIST ((2 '((THUSE TC-2))))
    EXPR (lambda nil
        (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!NAME !2)))))
    SYS (!SYSTEMS))

(§ DEFS !NEWWORD SYS (!THING))

(§ DEFS !NOTICE
    THMLIST ((2 '((THUSE TC-2))))
    EXPR (lambda nil
        (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!NOTICE !2 *TIME))))))

(§ DEFS !ON
    THMLIST ((3 '((THUSE TC-ON))) (4 '((THUSE TCT-ON))))
    EXPR (lambda nil
        (COND ((CQ LOBJ)
            (RELATION (RESTRICTIONSß (((!THING)) ((!PHYSOB))) MARKERSß (!PLACE) PROCEDUREß ((!ON !2)))))
            ((RELATION
                (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PARAPHRASEß (ANYWHERE ON TOP OF) PROCEDUREß ((!ON !1 !2 *TIME)))
                (RESTRICTIONSß (((!PHYSOB)) ((!MANIP))) PARAPHRASEß (DIRECTLY ON THE SURFACE) PROCEDUREß ((!SUPPORT !2 !1 *TIME)))
                (RESTRICTIONSß (((!PLACE)) ((!PHYSOB))) PROCEDUREß ((!ON !1 !2))))))))

(§ DEFS !PACK THMLIST ((3 '((THUSE TC-3)))))

(§ DEFS !PART THMLIST ((3 '((THUSE TC-PART)))))                       ;; PERFORMED ON GRADUATION DAY, JUNE 2, 1972 BY JMH

(§ DEFS !PERSON SYS (!ANIMATE))

(§ DEFS !PICKUP
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-PICKUP))) (4 '((THUSE TCTE-PICKUP))))
    NAMEVENT (I3 (cons (VBFIX 'PICK nil) (PRTPUT 'UP OBJ1))))

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

(§ DEFS !ROLE FEXPR (lambda (A) true) NOGOAL true)

(§ DEFS !PHYSOB
    SYSTEM (!BOX !CONSTRUCT !HAND !MANIP !TABLE)
    SYS (!THING)
    THMLIST ((2 '((THUSE TC-PHYSOB)))))

(§ DEFS !PROPDEFINE
    EXPR (lambda (X)
        (PUTPROP X '(PROPN NS) 'FEATURES)               ;; CHANGED TO FEATURES FROM 'WORD' IN THE OLD DICTIONARY
        (PUTPROP X '((PROPN T)) 'SEMANTICS)))

(§ DEFS !PROPERTY SYSTEM (!COLOR !SIZE !SHAPE) SYS (!THING))

(§ DEFS !POINTED SYS (!SHAPES))

(§ DEFS !RED SYS (!SPECTRUM))

(§ DEFS !RELATION SYS (!SYSTEMS))

(§ DEFS !ROLE FEXPR (lambda (A) true) NOGOAL true)

(§ DEFS !ROUND SYS (!SHAPES))

(§ DEFS !ROW SYS (!CONSTRUCT))

(§ DEFS !ROBOT SYS (!ANIMATE))

(§ DEFS !SIZE
    MEASFN (lambda (X) (reduce #'+ (SIZE X)))
    SYS (!PROPERTY))

(§ DEFS !SHAPE PRIORITY 128 SYS (!PROPERTY))

(§ DEFS !STACK SYS (!CONSTRUCT))

(§ DEFS !STACKUP
    THMLIST ((2 '((THUSE TC-2))))
    EXPR (lambda nil (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!STACKUP !2 *TIME)))))
    NAMEVENT (I3 (cons (VBFIX 'STACK nil) (PRTPUT 'UP OBJ1))))

(§ DEFS !START
    THMLIST ((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(§ DEFS !SUBST NOGOAL true)

(§ DEFS !SUPPORT
    PRIORITY 256
    THMLIST ((3 nil) (4 '((THUSE TCT-SUPPORT)))))

(§ DEFS !SYSTEMS SYSTEM (!THING !EVENT !NAME !RELATION !PLACE))

(§ DEFS !TABLE SYS (!PHYSOB))

(§ DEFS !THICKNESS MEASFN (lambda (X) (cadr (SIZE X))))

(§ DEFS !THING SYS (!SYSTEMS) SYSTEM (!ANIMATE !NAME !PHYSOB !PROPERTY))

(§ DEFS !UNGRASP
    THMLIST ((1 '((THUSE TC-UNGRASP))))
    NAMEVENT (I3 (concat (list (VBFIX 'LET true) 'GO 'OF) OBJ1)))

(§ DEFS !WANT
    THMLIST ((4 '((THUSE TC-WANT4))) (5 '((THUSE TC-WANT5)))))

(§ DEFS !WHITE SYS (!SPECTRUM))

(§ DEFS !WIDTH MEASFN (lambda (X) (car (SIZE X))))

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
    RELATIONS= ((!IS $?ANIM ?))
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
    REFER= (:FRIEND)
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
    REFER= (:SHRDLU)
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
    RELATIONS= ((!IS $?UNKNOWN ?))
    MARKERS= (!THING !SYSTEMS !PHYSOB !VAGUE)
    SYSTEMS= (!THING !SYSTEMS)
    DETERMINER= (SG-PL INDEF nil)
    PARSENODE= (FAKE-AGENT)
    VARIABLE= UNKNOWN)

(§ DEFS UNKNOWNSG-OSS
    OSSNODE= UNKNOWNSG-OSS
    MARKERS= (!THING !SYSTEMS !PHYSOB !VAGUE)
    RELATIONS=  ((!IS $?UNKNOWN ?))
    SYSTEMS= (!THING !SYSTEMS)
    DETERMINER= (NS INDEF WHICH)
    VARIABLE= UNKNOWN)

(§ DEFS WE-OSS
    OSSNODE= WE-OSS
    MARKERS= (!ANIMATE !THING !SYSTEMS)
    SYSTEMS= (!ANIMATE !THING !SYSTEMS)
    REFER= (:SHRDLU :FRIEND)
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

(§ DEFUN SMTIME [] (ERT SMTIME NOT WRITTEN YET))

(§ DEFUN SMTIME2 [] (ERT SMTIME2 NOT WRITTEN YET))

(§ DEFUN SMNEWNOUN []
    (OBJECT (MARKERSß (!NEWNOUN) PROCEDUREß ((!NEWWORD)))))

(§ DEFUN SMNEWPROPN [] (SMSET (list (NEWCOPY 'NAME-OSS))))

(§ DEFUN SMCONJ []
    ;; FOR ALL CONJOINED THINGS -- IT CURRENTLY ONLY HANDLES THINGS
    ;; WHICH HAVE AN OSS OR RSS STRUCTURE AS THEIR SEMANTICS.  THIS
    ;; DOES NOT INCLUDE SINGLE WORDS OF MOST TYPES.  IT USES RECURSION
    (let [%SM nil] (SMCONJ2 nil H) (RETURN (SMSET %SM))))

(§ DEFUN SMCONJ2 [INTERPLIST RESTLIST]
    ;; INTERPLIST IS THE LIST OF INTERPRETATIONS FOR THE CONJUNCTS
    ;; HANDLED SO FAR -- THIS FUNCTION WILL BE CALLED ONCE FOR EACH
    ;; POSSIBLE COMBINATION.  THE MARKERS FOR THE CONJOINED
    ;; STRUCTURE ARE THOSE OF THE FIRST CONJUNCT -- NEEDS MORE
    ;; SOPHISTICATION.  RESTLIST IS THE REST OF NODES YET TO BE HANDLED.
    (let [%X nil]
        (or RESTLIST
            (RETURN (SETQ %SM
                (cons (BUILD
                        RSSNODE= (and (RSS? INTERP) (MAKESYM 'RSS))
                        OSSNODE= (and (OSS? INTERP) (MAKESYM 'OSS))
                        MARKERS= (MARKERS? INTERP)
                        SYSTEMS= (SYSTEMS? INTERP)
                        REL= (REL? INTERP)
                        AND= (and (or (CQ BUT) (CQ AND)) INTERPLIST)
                        OR= (and (or (CQ OR) (CQ NOR)) INTERPLIST))
                    %SM))))
        ;; WHEN THERE IS NO RESTLIST, WE HAVE LOOPED TO THE END OF THE LIST OF CONJUNCTS, AND THE RESULTING INTERPRETATION IS OK.
        ;; THE MAPPING IS DOWN THE LIST OF INTERPRETATIONS FOR A SINGLE CONJUNCT WHILE THE RECURSION GETS US DOWN THE LIST OF CONJUNCTS.
        ;; THUS WE GET EVERY POSSIBLE COMBINATION OF THE INTERPRETATIONS. -- ISN'T LISP SUPER-DUPER-WONDERFUL!
        ;; NOTICE THAT INTERP IS GETTING PICKED UP AS A FREE VARIABLE BY SMCONJ2, EVEN THOUGH IT IS BOUND ONLY INSIDE A MAPCAR INSIDE SMCONJ2.
        ;; THIS WORKS BECAUSE THE CLAUSE CONTAINING IT CAN NEVER GET CALLED EXCEPT BY RECURSION,
        (dorun (map #'(lambda (INTERP) (SMCONJ2 (cons INTERP INTERPLIST) (cdr RESTLIST))) (SM RESTLIST)))))

(§ DEFUN SMVG [] ;; CALLED INSIDE ANY VG
    (let [TSS nil TENSE nil]
        (SETQ TSS (GETR 'TIME (MOVE-PT C U (CLAUSE))))
        (and (CQ NEG) (ADD-F-PT 'NEG PT))                           ;; NEG IS TRANSFERRED FROM THE VG TO THE CLAUSE IN WHICH IT IS EMBEDDED.
        (SETQ TENSE (GETR 'TENSE C))
        (COND ((MEMBER TENSE '((PRESENT) (IMPER) (INFINITIVE))) true)
            ((EQUAL TENSE '(MODAL))
                (SETQ GLOBAL-MESSAGE '(THAT DOESN'T MAKE ANY SENSE TO ME\.))
                (ADD-F-PT 'MODAL PT))                               ;; CLAUSES ARE ALSO MARKED AS MODAL.
            ((and (EQUAL TENSE '(FUTURE)) (ISQ PT QUEST) (EQUAL (REFER? (car (SM (GETR 'SUBJECT PT)))) '(:SHRDLU))) ;; FUTURE QUESTIONS WITH "YOU"
                (SETQ TENSE '(PRESENT))                             ;; SUBJECT IS REALLY IMPERATIVE.
                (REMOVE-F-PT 'QUEST PT)
                (ADD-F-PT 'IMPER PT))                               ;; THE CLAUSE IS NO LONGER QUESTION, BUT RATHER, IMPERATIVE.
            ((SETDIF TENSE '(PAST PRESENT))
                (GLOBAL-ERR '(I DON'T KNOW HOW TO HANDLE TENSES INVOLVING FUTURE EVENTS OR MODALS OTHER THAN IN THE PRESENT))))
        (PUTPROP TSS TENSE 'TENSE=)
        (RETURN true)))

(§ DEFUN SMADJGQSHORT [] (ERT SMADJQSHORT NOT WRITTEN YET))

(§ DEFUN SMPRON [NODE]
    (eval (SM NODE))
    (COND ((nil? SM)
        (SETQ GLOBAL-MESSAGE (concat '(I DON'T KNOW WHAT \") (FROM (NB H) (N H)) '(\" REFERS TO))))) ;; "sic!
    SM)

(§ DEFUN SMVAUX []
    (COND ((ISQ H NEG) (FQ NEG)) (:else true))
    (PUTPROP (GETR 'TIME C) (or (MEET (FE H) '(PRESENT PAST MODAL)) (ERTERR SMVAUX -- FUNNY TENSE)) 'TENSE=))

(§ DEFUN SMADV [] (ERT SMADV NOT WRITTEN YET))

(§ DEFUN SMPLACE [] (ERT SMPLACE NOT WRITTEN YET))

(§ DEFUN SMTOADJ [] (ERT SMTOADJ (UNCT) NOT WRITTEN YET))

(§ DEFUN SMPROP []
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

(§ DEFUN SMADJ [WORD-BEING-INTERPRETED]
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
    (eval (SM WORD-BEING-INTERPRETED)))
    ;; EVALUATE THE DEFINITION OF THE ADJECTIVE

;; --------------------------------------------

(§ DEFUN SMADJG-PREPG []
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
                ((ERTERR SMADJG-PREPG FUNNY POSITION)))
            C)
        (SETQ SMSUB (SM (GETR 'LOGICAL-SUBJECT C)))
        (and (CQ ADJG)
            (GETR 'OBJ1 C)
            (SETR 'ADJGHEAD
            (COMPARE-BUILD (GETR 'HEAD C) (COND ((CQ AS) '!ASMUCH) ((CQ THAN) '!MORE) ((ERTERR SMADJG-PREPG FUNNY TYPE))))
            C))
        (COND
            ((GETR 'OBJ1 C) (SMCL1) (RETURN SM))
            ((RETURN (SMSET
                (let [SM nil]
                    (SMSET (doall (map #'(lambda (OSS)
                        (BUILD
                            OSSNODE= (MAKESYM 'OSS)
                            MARKERS= (MARKERS? OSS)
                            SYSTEMS= (SYSTEMS? OSS)
                            VARIABLE= (VARIABLE? OSS)
                            REFER= (REFER? OSS)
                            REL= OSS
                            REFER= (REFER? OSS)
                            DETERMINER= '(NS-PL INDEF nil)))
                        SMSUB)))
                    (eval (COND
                        ((or (CQ COMPAR) (CQ SUP)) (FINDMEASURE (GETR 'HEAD C)))
                        (:else (SM (GETR 'HEAD C)))))
                    (RETURN SM))))))))

;; -------------------------------------------

(§ DEFUN SMIT [PRONOUN]
    ;; PRONOUN IS (IT THEY ONE) A NODE LIST OF POSSIBLE REFERENTS.
    ;; IS THIS A "DO IT!" COMMAND?  IF SO, RETURN THE LAST EVENT MENTIONED.
    ;; IF THIS PRONOUN HAS BEEN USED BEFORE IN THIS SENTENCE, THEN USE THE SAME CANDIDATES.
    ;; IF THIS PRONOUN WAS USED IN THE PREVIOUS SENTENCE,
    ;; LOOK FOR A STRUCTURE LIKE "A BLOCK WHICH IS TALLER THAN ANYTHING WHICH SUPPORTS IT"
    ;; OR "A BLOCK TALLER THAN ANYTHING WHICH SUPPORTS IT".
    (let [CANDIDATES nil AMBIGUITIES nil]
        (or DISCOURSE (ERT "SMIT:" DISCOURSE SWITCH NOT ON))
        (and MVB
            (ISQ MVB DO)
            (CQ OBJ1)
            (RETURN (SMSET LASTEVENT)))
        (COND ((GET PRONOUN 'BIND)
                (MAP #'(lambda (BINDNODE) (SMIT2 BINDNODE 0)) (GET PRONOUN 'BIND))
                (RETURN SM))
            ((SMIT2 (GET PRONOUN 'LASTBIND) 0)
                (GO DONE))
            ((or (MOVE-PT C U U (NG) U U (NG)) (MOVE-PT C U U (NG) U (COMP) PV (SUBJ)))
                (SMIT2 PT 0)
                (MOVE-PT C U U (NG))
                (COND ((ISQ PT DEF)
                    (ADD-F-PT 'INDEF PT)
                    (REMOVE-F-PT 'DEF PT)
                    (dorun (map #'(lambda (INTERP) (PUTPROP INTERP '((EXACTLY 1) INDEF nil) 'DETERMINER=)) (SM PT)))))
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
        (SMIT2 (PARSENODE? LASTREL) 128)                ;; TRY REL (I.E. QUESTION FOCUS) OF THE LAST SENTENCE
        (MOVE-PT LASTSENT DLC)
    UP  (COND ((not (MOVE-PT PV (NG))) (GO ON))
            (:else (SMIT2 PT 64)))                       ;; GO THROUGH TOP LEVEL NG'S OF LAST SENTENCE
        (and (MOVE-PT PV) (GO UP))
    ON  (or SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE ANSREF (NG'S IN LAST ANSWER)
            (MAP #'(lambda (ANSNODE) (SMIT2 ANSNODE 0)) ANSNAME))
        (or SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE BACKREF2 (NG'S IN LAST SENTENCE) LIST
            (MAP #'(lambda (BACKNODE) (SMIT2 BACKNODE 0)) BACKREF2))
    DONE (PUTPROP PRONOUN CANDIDATES 'BIND)
        (or (cdr SM) (REMPROP (car SM) 'AMBIGUITIES=))
        (RETURN SM)))

(§ DEFUN SMIT2 [NODE PLAUSIBILITY]
    ;; MAKE SURE NODE IS REALLY THERE.
    ;; QUEST NODES (SUCH AS "WHAT") OR OTHER NODES WITHOUT HEAD NOUNS ARE NOT SUITABLE FOR REFERENTS.
    ;; MAKE SURE THAT NODE HASN'T ALREADY BEEN USED AS REFERENT.
    ;; MAKE SURE NODE AND PRONOUN AGREE IN NUMBER.
    (and NODE
        (GETR 'HEAD NODE)
        (not (MEMQ (car NODE) CANDIDATES))
        (COND ((EQ PRONOUN 'IT)
            (and (ISQ NODE NS) (not (ISQ NODE PRONG))))
            (:else (ISQ NODE NPL)))
        (SETQ CANDIDATES (cons (car NODE) CANDIDATES))
        (SMSET (concat
            (doall (map #'(lambda (REFERENT-OSS)
                (BUILD
                    OSSNODE= (MAKESYM 'OSS)
                    MARKERS= (MARKERS? REFERENT-OSS)
                    SYSTEMS= (SYSTEMS? REFERENT-OSS)
                    PLAUSIBILITY= PLAUSIBILITY
                    AMBIGUITIES= (list (list OSSNODE= (FROM (NB NODE) (N NODE)) C))
                    REFER= (REFER? REFERENT-OSS)
                    VARIABLE= (VARIABLE? REFERENT-OSS)
                    PARSENODE= C ;; INPUT PARAMETER
                    ;; USE THE REFERENT'S REFERENT, IF IT HAS ONE.
                    DETERMINER= (list (COND ((ISQ C NPL) 'NPL) ('NS)) 'INDEF nil)
                    ;; DONE SO THAT IF VARIBLE IS BOUND, PLANNER GENERATOR WILL USE IT.
                    ;; RELATION SAYS THAT THIS OSS "REFERS" TO THE OSS WHOSE VARIABLE NAME IS GIVEN.
                    RELATIONS= (list (list '!REFERS (VARIABLE? REFERENT-OSS)))))
                (SM NODE)))
            SM))))

(§ DEFUN SMNGOF []
    ;; MAP DOWN THE LIST OF "OF" OBJECT INTERPRETATIONS.
    ;; USED TO PROCESS NOUN GROUPS LIKE "THREE OF THE BLOCKS", "BOTH OF THEM".
    ;; SINCE THE OBJECT OF THE "OF" MUST BE DEFINITE (SYNTACTICALLY) AND HAS ALREADY BEEN PROCESSED,
    ;; THE PLANNER CODE BUILT IS JUST A THAMONG EXPRESSION OF THE LIST
    ;; OF POSSIBLE REFERENTS OF THE "OF" OBJECT.
    (SMSET (MAPBLAND
        #'(lambda (OFOSS)
            (BUILD
                OSSNODE= (MAKESYM 'OSS)
                VARIABLE= (VARIABLE? OFOSS)
                SYSTEMS= (SYSTEMS? OFOSS)
                MARKERS= (MARKERS? OFOSS)
                PARSENODE= C
                DETERMINER= (list (COND ((CQ NUM) (SM (MOVE-PT H PV (NUM)))) ((ISQ NB BOTH) 2) ('NPL))
                                (COND ((MOVE-PT H PV (QNTFR)) (eval (SM PT))) ('INDEF))
                                (COND ((CQ HOWMANY) 'HOWMANY) ((CQ QDET) 'WHICH)))
                RELATIONS= (list (list 'THAMONG (list 'THV (VARIABLE? OFOSS)) (list 'quote (REFER? OFOSS))))))
        (SM (MOVE-PT H DLC)))))

;; ============================================================

(§ DEFUN SMNG1 []
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
    (let [WORD-BEING-INTERPRETED nil DETERS nil]
        (SETQ DETERS (list
            (COND
                ((CQ NUMD) ((lambda (NUM) (eval (SM (MOVE-PT H PV (NUMD))))) (SM (MOVE-PT H PV (NUM)))))
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
            DETERMINER= DETERS)))
        (SETQ WORD-BEING-INTERPRETED H)
        (COND ((ISQ H TPRON) (GO LOOP))
            ((CQ INCOM) (SMONE) (GO LOOP)))
        (SMSET (eval (SM WORD-BEING-INTERPRETED)))
    LOOP (COND ((nil? SM) (RETURN nil)))
        (COND ((nil? (SETQ WORD-BEING-INTERPRETED (cdr WORD-BEING-INTERPRETED)))
                (RETURN SM))
            ((or (ISQ WORD-BEING-INTERPRETED COMPAR) (ISQ WORD-BEING-INTERPRETED SUP))
                (eval (FINDMEASURE WORD-BEING-INTERPRETED))
                (GO LOOP))
            ((or (ISQ WORD-BEING-INTERPRETED ADJ) (ISQ WORD-BEING-INTERPRETED CLASF))
                (SMADJ WORD-BEING-INTERPRETED)
                (GO LOOP))
            ((ISQ WORD-BEING-INTERPRETED POSS)
                (SMPOSS)
                (GO LOOP)))
        (GO LOOP)))

;; ============================================================

(§ DEFUN SMNG2 []
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

(§ DEFUN SMNG3 [OSS]
    ;; TAKES AN OSS AS ARGUMENT AND TRIES TO FIND ITS REFERENCE IF THE NOUN GROUP IS DEFINITE.
    ;; EXPECT FOR SPECIAL "ONLY DEFINITE" DEFINITES SUCH AS "THE RIGHT" AND "THE THING".
    (let [FINDER nil MUNG nil INTER nil LIST nil CANDIDATES nil UNBOUND nil]
        (COND
            ((not (EQ (QUANTIFIER? OSS) 'DEF)) (RETURN OSS))        ;; IF ITS NOT DEFINITE OR IT
            ((REFER? OSS) (RETURN OSS))                             ;; ALREADY HAS A REFERENT
            ((CQ ANSNAME) (RETURN OSS)))                            ;; MARKED,  IF ITS KLUDGY
        (SETQ FINDER
            (PLNR-FINDIFY 'ALL                                      ;; ANSWER NAME, JUST RETURN IT
                (VARIABLE? OSS)                                     ;; JUST RETURN IT
                (list (VARIABLE? OSS))
                (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (list (VARIABLE? OSS))))) ;; BUILDS UP THFIND EXPRESSION
        (PUTPROP OSS FINDER 'PLNRCODE=)
        (SETQ WHO nil)
    UP  (COND
            ((not (SETQ CANDIDATES (THVAL2 WHO FINDER))) (GO TOOFEW))
            ((number? (NUMBER? OSS)) (COND ((< (count CANDIDATES) (NUMBER? OSS)) (GO TOOFEW)) ((> (count CANDIDATES) (NUMBER? OSS)) (GO TOOMANY))))
            ((EQ (NUMBER? OSS) 'NS) (COND ((nil? CANDIDATES) (GO TOOFEW)) ((cdr CANDIDATES) (GO TOOMANY))))
            ((MEMQ (NUMBER? OSS) '(NPL SG-PL)))
            ((ERT SMNG3= SCREWY NUMBER PROPERTY OF OSS)))
        (PUTPROP OSS CANDIDATES 'REFER=)
    DONE (RETURN OSS)

    TOOFEW ;; WE DIDN'T FIND ANY (OR ENOUGH) REFERENTS FOR THE NG
        (COND
            ((or (nil? DISCOURSE) (nil? WHO))
                (SETQ GLOBAL-MESSAGE (concat '(I DON'T KNOW WHAT YOU MEAN BY \") (FROM NB N) '(\"\.))) ;; "sic!
                (RETURN nil))
            ;; IF WE AREN'T REMEMBERING SENTENCES, FORGET IT IF WE JUST TRIED TO
            ;; FIND EVERYTHING (OR EVERYTHING THAT "HE" KNOWS ABOUT), THEN FAIL
            ((MEMQ WHO '(HE nil))
                (SETQ GLOBAL-MESSAGE (concat '(I DON'T KNOW WHICH) (cdr (FROM NB N)) '(YOU MEAN\.)))
                (RETURN nil)))
        (SETQ MUNG true)

    TOOMANY ;; ELSE SET UP TO EXPAND THE SENTENCES WE'RE LOOKING AT
        (and (MEMQ WHO '(HE nil))
            (SETQ FINDER (PLNR-MUNG FINDER CANDIDATES)))
        ;; RESTRICT THE POSSIBLE REFERENTS TO BE AMONG THE LIST ALREADY FOUND
        (SETQ WHO (COND
            ((EQ WHO nil) 'HE)
            ((EQ WHO 'HE) (list (dec LASTSENTNO) (inc LASTSENTNO)))
            ((or (not MUNG) (EQ (car WHO) 1)) (SETQ WHO 'HE) (GO TOOFEW))
            ((cons (dec (car WHO)) (cdr WHO)))))
        (SETQ MUNG nil)
        (GO UP)))

(§ DEFUN SMONE []
    (let [CONTRAST nil X nil]
        (SETQ X H) ;; SET X TO DAUGHTERS OF CURRENT NODE
    GO  (COND ((SETQ CONTRAST (GET (ROOT (NB X)) 'CONTRAST))
                (SETQ CONTRAST (list CONTRAST (ROOT (NB X)))))
            ((SETQ X (cdr X)) (GO GO)))
    UP  (or (and (MOVE-PT C U U (NG)) (SMONE2 (list (car PT))))
            (SMONE2 (PARSENODE? LASTREL))
            (SMONE2 BACKREF)
            (SMONE2 ANSNAME)
            (SMONE2 BACKREF2)
            (COND (CONTRAST (SETQ CONTRAST nil) (GO UP)))
            (and (MOVE-PT LASTSENT DLC PV (NG)) (SMONE2 (list (car PT))))
            (ERT SMONE= CAN'T FIND REFERENT FOR "ONE"))
        (RETURN SM)))

(§ DEFUN SMONE2 [X]
    ;; SMONE2 TAKES IN A LIST OF NOUN GROUP NODES AND TRIES TO SEE
    ;; IF ANY OF THOSE NOUN GROUPS COULD BE THE REFERENT FOR "ONE".
    (let [WORD-BEING-INTERPRETED nil]
        ;; IF X IS EMPTY, FAIL.
        ;; TRY TO SEE IF FIRST NG OF X SATIFIES CONTRAST AND/OR COULD BE REFERENT, ELSE TRY NEXT NG IN X
    UP  (COND ((nil? X) (RETURN nil))
            ((SETQ WORD-BEING-INTERPRETED (SMONE3 X)))
            (:else (SETQ X (cdr X)) (GO UP)))
        ;; AT THIS POINT WORD-BEING-INTERPRETED IS (SHOULD BE) A
        ;; LIST A WORD NODES OF THE NG WHICH IS THE REFERENT FOR
        ;; "ONE" WE NOW PROCEED TO BUILD UP AN OSS FOR THE "ONE"
        ;; NG THE LIST IS IN ORDER (NOUN ADJ ... ADJ ETC NUM DET)
        ;; ONLY THE NOUN AND THE ADJ'S ARE USED
        (or (ISQ WORD-BEING-INTERPRETED NOUN)
            (BUG "SMONE2:" REFERENT OF "\"ONE\"" IS SCREWED UP)) ;; "sic!
        (eval (SM WORD-BEING-INTERPRETED))                          ;; EVAL THE NOUN DEFINITION
    GO  (and
            (SETQ WORD-BEING-INTERPRETED (cdr WORD-BEING-INTERPRETED))
            (ISQ WORD-BEING-INTERPRETED ADJ)                        ;; IF WE REACHED END OF ADJECTIVES, STOP
            (eval (SM WORD-BEING-INTERPRETED))
            (GO GO))
        (RETURN SM)))

(§ DEFUN SMONE3 [ONENG]
    ;; SMONE3 TAKES AN NG WHICH IS A POSSIBLE REFERENT FOR "ONE".
    ;; IT FIRST CUTS THE NG TO BE ONLY (NOUN ADJ ... ADJ ETC) I.E.
    ;; IT STRIPS OF QUALIFYING PHRASES.  IF THERE IS NO CONTRAST,
    ;; THEN THIS MUNGED NG IS RETURNED AS THE REFERENT.  IF THERE
    ;; IS A CONTRAST, THEN IT CHECKS TO SEE IF THE NG SATISFIES
    ;; THAT CONTRAST.
    (let [NGWORDS nil X nil]
        (or (ISQ ONENG NG)
            (BUG "SMONE3:" ONE REFERENT IS NOT A NG))
        (SETQ NGWORDS (H ONENG))
    LOOP (COND ((nil? NGWORDS) (RETURN nil))                        ;; FAIL, IF NG HAS NO NOUN HEAD
            ((ISQ NGWORDS NOUN))                                    ;; IF FIND NOUN HEAD OF NG, WIN
            (:else (SETQ NGWORDS (cdr NGWORDS)) (GO LOOP)))
        (or CONTRAST (RETURN NGWORDS))                              ;; IF THERE IS NO CONTRAST, REFERENT WINS BY DEFAULT
        (SETQ X (reverse NGWORDS))
    LOOK (COND ((and (EQ (car CONTRAST) (GET (ROOT (NB X)) 'CONTRAST)) (not (EQ (cadr CONTRAST) (ROOT (NB X)))))
                (RETURN (reverse (cdr X))))
            ((SETQ X (cdr X)) (GO LOOK))
            (:else (RETURN nil)))))                                  ;; FAIL, IF NO WORD SUPPLIES CONTRAST

(§ DEFUN SMPOSS []
    (let [X nil]
        (RETURN (and (SETQ X (SMPOSS2 C (MOVE-PT H PV (POSS))))
            (SMRELATE X)))))

(§ DEFUN SMPOSS2 [HEADNODE MODNODE]
    (let [X nil SM nil SMSUB nil SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST nil]
        (SETQ SMRUB (SM MODNODE))
        (SETQ RELLIST (RETQ AMOB1 (SM HEADNODE)))
        (SMSET '(!HAVE))
        (RETURN (and SM
            (SETQ X (MAKESYM 'NODE))
            (PUTPROP X SM 'SEMANTICS)
            (list X)))))

;; SMPOSS WORKS BY ACTING LIKE SMCL1 AND SETTING UP AN RSS (HAVE X Y) .  NODE IS THE NODE OF THE POSSESSIVE
;; WHICH HAS ALREADY BEEN SEMANTICALLY PROCESSED.  ITS SM CONTAINS THE OSS'S FOR WHOSE DOING THE POSSESSING.
;; THE SM CURRENTLY ACTIVE IS THE THING BEING POSSESSED.

(§ DEFUN SMRELATE [NODE]
    ;; RELATES A DESCRIPTIVE RSS TO ANOTHER RSS OR OSS ADDING IT
    ;; TO THE LIST OF RELATIONS.  IT TAKES THE LIST OF SS IN SM,
    ;; AND REPLACES SM WITH THE NEW LIST OF MODIFIED SS'S.  THE
    ;; MODIFYING RSS'S HAVE TO HAVE ONE OF THE SM SS'S AS A REL
    ;; (WHICH SHOULD ALWAYS BE TRUE IF THEY WERE SET UP PROPERLY).
    ((lambda (X) (and X (SMSET X)))
        (doall (map #'(lambda (RSS)
            (let [REL nil]
                (or (MEMQ (SETQ REL (REL? RSS)) SM)
                    (ERTERR SMRELATE - TO WHOM?))
                (RETURN (BUILD
                    OSSNODE= (and (OSS? REL) (MAKESYM 'OSS))
                    RSSNODE= (and (RSS? REL) (MAKESYM 'RSS))
                    MARKERS= (or (and (RELMARKERS? RSS) (car (RELMARKERS? RSS))) (MARKERS? REL))
                    SYSTEMS= (or (and (RELMARKERS? RSS) (cadr (RELMARKERS? RSS))) (SYSTEMS? REL))
                    PLAUSIBILITY= (PLAUSIBILITY? RSS)
                    PARSENODE= (PARSENODE? REL)
                    AMBIGUITIES= (AMBIGUITIES? RSS)
                    VARIABLE= (VARIABLE? REL)
                    NEGATIVE= (NEGATIVE? REL)
                    DETERMINER= (DETERMINER? REL)
                    RELATIONS= (cons RSS (RELATIONS? REL))
                    REL= (REL? REL)))))
            (SM NODE)))))

;; -----------------------------------------------------

(§ DEFUN SMCL1 []
    (let [SMSUB nil SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST nil]
        ;; SET UP GLOBAL VARIABLES WHICH CONSIST OF POINTERS TO THE SEMANTIC DEFINITIONS
        ;; OF THE VARIOUS NOUN-GROUPS (ALSO RSNG'S) REQUIRED BY THE TRANSITIVITY OF THE VERB
        (SETQ SMSUB (COND
            ((SETQ SMSUB (GETR 'LOGICAL-SUBJECT C)) (SM SMSUB))
            ((CQ IMPER) '(SHRDLU-OSS))
            ((not (CQ PASV)) (SM (or (GETR 'SUBJECT C) (ERTERR SMCL1 -- NO SUBJECT))))
            ((CQ AGENT) (ERTERR SMCL1 -- AGENT MISSING))
            ('(UNKNOWN-OSS-BY))))
        (SETQ SMOB1 (SM (COND ((CQ PASV) (GETR 'SUBJECT C)) ((GETR 'OBJ1 C)))))
        (SETQ SMOB2 (SM (GETR 'OBJ2 C)))
        (SETQ SMOBL (SM (GETR 'LOBJ C)))
        (SETQ SMCOMP (SM (GETR 'COMP C)))
        ;; NATURALLY SEVERAL OF THESE GLOBAL VARIABLES (BOUND IN THIS PROG AND ACCESSED IN DEEPER ONES)
        ;; ARE NIL AT THIS POINT IN THE PROCEDURE.  THE FOLLOWING CHECKS ARE PRIMARILY FOR DEBUGGING PURPOSES
        ;; (HENSE THE "ERT") TO INSURE THAT THE NON-NIL REGISTERS AND THE TRANSITIVITY OF THE VERB ARE
        ;; BEING MATCHED IN EVERY CASE.
        (or SMSUB (and (MEET '(THERE ITRNS) FE) (GO CHECK)))
        (or SMOB1 (and (or (CQ TRANS) (not (CQ CLAUSE))) (GO CHECK)))
        (or (and SMOB1 SMOB2) (and (CQ TRANS2) (GO CHECK)))
        (or (and SMOB1 SMOBL) (and (CQ TRANSL) (GO CHECK)))
        (or SMCOMP (and (CQ INT) (GO CHECK)))
        (GO REL)
    CHECK (ERT "BUG:" SMCL1 TRANSITIVITY)
    REL (SETQ RELLIST
            (SM (COND
                ((CQ RSQ) (GETR 'RELHEAD C))
                ((or (CQ PREPG) (CQ ADJG)) (GETR 'LOGICAL-SUBJECT C))
                ((CQ QUEST) (GETR 'RELHEAD C)))))
        (and (not RELLIST)
            (or (CQ POLAR) (CQ DECLAR))
            (SETQ X (RELFIND C))
            ;; FIND RELATIVE ELEMENT FOR POLAR CLAUSES WHICH CONTAIN INDEFINITE.
            ;; APPLIES TO TOPLEVEL CLAUSES SINCE ONLY THEY CAN HAVE FEATURES POLAR OR DECLAR.
            (or (EQUAL X SMSUB)
                (EQUAL X SMOB1)
                (EQUAL X SMOB2)
                (EQUAL X SMOBL)
                (EQUAL X SMCOMP)
                (ERTERR SMCL1 -- POLAR REL DOESN'T MATCH))
            (SETQ RELLIST X))
        (SETQ TIME (GETR 'TIME (MOVE-PT C U (CLAUSE))))
        ;; THIS REFERS TO THE SEMANTIC SENSE OF THE VERB WHICH WILL PROBABLY VARY WITH ITS TRANSITIVITY.
        ;; THE VALUE THAT IS FINALLY DETERMINED REPRESENTS ALL POSSIBLE SENSES OF THE MEANING OF THE WORD
        ;; THAT ARE APPROPRIATE TO THE TRANSITIVITY FIGURED OUT BY THE SYNTACTIC PROGRAMS
        (SETQ SENSE-OF-VERB
            (COND
                ((CQ PREPG) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'HEAD C))))
                ((CQ ADJG) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'ADJGHEAD C))))
                ((cadr (ASSQ (car (MEET FE '(ITRNS TRANS INT TRANSL TRANS2 THERE ITRNSL))) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'MVB C))))))))
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

(§ DEFUN SMCL2 []
    ;; THERE USED TO BE A CALL TO SMPREPREL AT THIS POINT, BUT IT
    ;; HAS GONE AWAY PENDING FURTHER THOUGHT.
    (MAP #'SMCL-MODIFIERS H))
    ;; AS IN SMCL1, WE NEED TO SCAN THE CONSTITUENTS OF
    ;; THE CLAUSE AND ALLOW THEM TO MAKE WHATEVER MODIFICATION ARE APPROPRIATE

(§ DEFUN SMCL-MODIFIERS [WORD-BEING-INTERPRETED]
    ;; AS IN CONSTITUENT, THIS PROCEDURE IS BASICLY ONE LARGE DISPATCH TABLE WHICH
    ;; ARRANGES THAT THE PROPER KIND OF PROCESSING HAPPEN TO THE APPROPRIATE CONSTITUENT.
    ;; SOME SHOULD BE IGNORED SINCE THEY HAVE ALREADY BEEN DEALT WITH AND OTHERS SHOULD
    ;; BE EVALUATED AS MODIFIERS OR FOR THEIR SIDE-EFFECTS.
    (COND ((nil? (GET WORD-BEING-INTERPRETED 'FEATURES)))
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
    ((ISQ WORD-BEING-INTERPRETED NG)
        (and (COND ((ISQ WORD-BEING-INTERPRETED TIM))
            ((and (CQ REL-NOT-FOUND)
                (ISQ WORD-BEING-INTERPRETED QUEST)
                (ISQ (H WORD-BEING-INTERPRETED) TIM1))
            (RQ REL-NOT-FOUND)
            (FQ TIMEQ)))
        (SMTIME)))
    ;; IN WHICH CASE IT WAS ALREADY PROCESSED MIGHT GO AWAY IN A FEW DAYS
    ;; BUG CHATCHER MIGHT WANT TO CHANGE THAT THE REST ARE HOOKS FOR WHEN
    ;; WE FIGURE OUT WHAT TO DO WITH THEM
    ((ISQ WORD-BEING-INTERPRETED PREPG)
        (or (ISQ WORD-BEING-INTERPRETED AGENT)
            (ERT SMCL-MOD BADPREPG)))
    ((ISQ WORD-BEING-INTERPRETED QADJ)
        (or (MEET FE '(LOBJ COMPQ))
            (eval (SM WORD-BEING-INTERPRETED))))
    ((ISQ WORD-BEING-INTERPRETED BOUND))
    ((ISQ WORD-BEING-INTERPRETED BINDER))
    ((ISQ WORD-BEING-INTERPRETED QUEST))
    ((ISQ WORD-BEING-INTERPRETED CLAUSE))
    ((ERT SMCL-MODIFIERS ATTEMPTED TO PROCESS AN UNEXPECTED TYPE OF CONSTITUENT))))

(§ DEFUN SMBIND []
    (let [TSS nil EVENT nil START nil END nil]
        ;; DOES THE SM HAVE MORE THAN ONE VALUE???
        (and (cdr (SM H))
            (ERT I DON'T KNOW WHAT TO DO WITH AMBIGUOUS BOUND CLAUSES))
        ;; DISPATCH TABLE TO MATCH THE APPROPRIATE ACTION WITH EACH BINDER.
        ;; MOVE TO THE FIRST WORD OF THE CLAUSE (TO THE BINDER) AND CHECK FOR THE FEATURE TIME
        ;; (MAYBE ALSO CHECK FOR THE SM BEING MARKED AS AN EVENT???)
        (COND ((ISQ (MOVE-PT H DF) TIME)
            (SETQ TSS (GETR 'TIME C))
            (or (SETQ EVENT (FINDEVENTS (car (SM H))))
                (GLOBAL-ERR '(NO SUCH THING EVER HAPPENED)))
            (SETQ EVENT (car EVENT))
            (SETQ START (GET EVENT 'START))
            (SETQ END (GET EVENT 'END))
            (eval (SM PT))
            (RETURN true)))
        nil))

(§ DEFUN SMBINDER [START-EV END-EV]
    ;; CALLED FOR A ABINDER - THE FIRST ARGUMENT GIVES THE BEGINNING, SECOND THE END.
    ;; A TYPICAL USE IS THE DEFINITION OF "AFTER", WHICH IS (SMBINDER END NIL) I.E.
    ;; THE EVENT STARTS AFTER THE END OF THE BOUND EVENT, WITH NO SPECIFICATION ON WHEN IT ENDS.
    (PUTPROP TSS START-EV 'START=)
    (PUTPROP TSS END-EV 'END=))

#_(ns shrdlu.smutil)

;; ############################################################
;;
;;                          SMUTIL
;;
;; ############################################################

(§ DEFUN ATOMIFY [X] (COND ((ATOM X) X) ((cdr X) X) (:else (car X))))

(§ DEFUN ISTENSE [NODE ARG]
    ;; CHECKS VARIOUS THINGS ABOUT TENSE.
    (let [X nil]
        (or (SETQ X (GETR 'TIME NODE)) (ERT ISTENSE -- NO TIME REGISTER))
        (or (SETQ X (TENSE? X)) (ERT ISTENSE -- NO TENSE))
        (RETURN (COND
            ((EQ ARG 'PRESENT) (MEMBER X '((PRESENT) (PRESENT PRESENT))))
            ((EQ ARG 'FUTURE) (EQUAL X '(FUTURE)))
            ((EQ ARG 'MODAL) (EQUAL X '(MODAL)))
            ((EQ ARG 'IMPERF) (and (cdr X) (EQ (car X) 'PRESENT)))
            (:else (ERT ISTENSE -- FUNNY ARG))))))

(§ DEFUN IMPERF? [TSS]
    (and (cdr (SETQ X (TENSE? X))) (EQ (car X) 'PRESENT)))

(§ DEFUN IMPERF [] (ISTENSE C 'IMPERF))

(§ DEFUN BUILD FEXPR [%L]
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
    (let [%X nil NEGATIVE= nil REFER= nil PLNRCODE= nil MARKERS= nil SYSTEMS= nil TENSE= nil TSSNODE= nil RELMARKERS= nil ANSNODE= nil ACTION= nil ANSRSS= nil PLAUSIBILITY= nil DETERMINER= nil AND= nil OR= nil AMBIGUITIES= nil RELATIONS= nil VARIABLE= nil VARLIST= nil REL= nil RSSNODE= nil PARSENODE= nil OSSNODE= nil NODE= nil %PROPS nil]
        (SETQQCHECK true %L
            (SETQ %PROPS '(NEGATIVE= REFER= PLNRCODE= MARKERS= SYSTEMS= RELMARKERS= PLAUSIBILITY= DETERMINER= AND= OR= AMBIGUITIES=
                           RELATIONS= VARIABLE= VARLIST= REL= RSSNODE= PARSENODE= OSSNODE= TSSNODE= TENSE= ANSNODE= ANSRSS= ACTION=))
            'BUILD)
        (and RSSNODE= (not MARKERS=) (SETQ MARKERS= '(!RELATION)))
        (and MARKERS= (not SYSTEMS=) (SETQ %X (CHECK MARKERS= nil nil)) (SETQ MARKERS= (car %X)) (SETQ SYSTEMS= (cadr %X)))
        (SETQ NODE= (or OSSNODE= RSSNODE= TSSNODE= ANSNODE= (ERT "...BUILD:" NO NODE=)))
        (dorun (map #'(lambda (%PROP) (and (SETQ %X (eval %PROP)) (PUTPROP NODE= %X %PROP))) %PROPS))
        (and BUILD-SEE ((lambda (DPSTOP) (DP NODE=)) SMN-STOP))
        (RETURN NODE=)))

(§ DEFUN NEWCOPY [OSS]
    (let [OLD nil NEW nil]
        (SETQ NEW (MAKESYM 'OSS))
        (SETQ OLD (cdr OSS))
        ;; WATCH OUT -- THIS IS IMPLEMENTATION DEPENDENT,
        ;; AND GETS THE ENTIRE PROPERTY LIST IN OUR LISP.
    =>  (COND
            ((nil? OLD) (PUTPROP NEW C 'PARSENODE=) (RETURN NEW))
            ((EQ (car OLD) 'PNAME))
            ((PUTPROP NEW (cadr OLD) (car OLD))))
        (SETQ OLD (cddr OLD))
        (GO =>)))

(§ DEFUN RELATION FEXPR [%DEFL]
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
            (let [SMCOMP nil SMSUB nil SMOB1 nil SMOB2 nil SMOBL nil MARKERSß nil RESTRICTIONSß nil PLAUSIBILITYß nil RELß nil PARAPHRASEß nil RELMARKERSß nil RSSNAME nil PROCEDUREß nil !1 nil !2 nil !3 nil %NEWRSS nil %OSSNODE nil]
                (SETQ %DEF (ARG 1) SMSUB (ARG 2) SMOB1 (ARG 3) SMOB2 (ARG 4) SMOBL (ARG 5) SMCOMP (ARG 6))
                ;; AN LEXPR IS USED HERE IN ORDER TO GET AROUND THE LIMITATION OF FIVE EXPR ARGUMENTS IN COMPILED CODE.
                ;; NOTICE THAT WITHIN THIS LAMBDA EXPRESSION THAT
                ;; SMSUB = ONE OSS FOR SEMANTIC SUBJECT
                ;; SMOB1 = ONE OSS FOR SEMANTIC OBJECT 1
                ;; SMOB2 = ONE OSS FOR SEMANTIC OBJECT 2
                ;; SMOBL = ONE OSS FOR LOCATIVE OBJECT
                ;; SMCOMP = ONE OSS FOR SEMANTIC COMPLEMENT
                ;; WHEREAS OUTSIDE OF THE LAMBDA EXPRESSION EACH OF THESE NAMES REPRESENTS A LIST OF THE SAME.
                ;; THIS IS TO ALLOW DICTIONARY WRITERS TO USE THESE SELF SAME NAMES IN WRITING DEFINITIONS, A SIMPLY TERRIBLE IDEA.
                (SETQQCHECK nil %DEF '(RESTRICTIONSß PROCEDUREß PLAUSIBILITYß PARAPHRASEß MARKERSß) 'RELATION)
                ;; (EVAL ...) DECODES KEYWORD ARGUMENTS.  SETQQ EFFECTIVLY QUOTES BOTH PAIRS
                ;; RESTRICTIONSß IS EXPANDED HERE PUTING IN IMPLICIT REGISTER REFERENCES SO THAT IT CAN BE UNIFORMLY GOBBLED BELOW
                (SETQ RESTRICTIONSß
                    ;; MARKL IS A SINGLE MARKER LIST FROM ON OF THE RESTRICTIONS IN THE DEFINITION, E.G. (!PHYSOB !RED).
                    ;; %RESTRICTNAM IS A NAME LIKE SMSUB, SMOBL, SMCOMP, .... WHICH REFERS TO REGISTERS ELSEWHERE
                    ;; IN THE PROGRAM WHOSE MARKERS MUST BE COMPATIBLE WITH %MARKL AS CHECKED BELOW.
                    ;; %NUM IS THE NUMBER WHICH WILL BE USED TO SUBSTITUTE IN THE DICTIONARY EXPRESSION.
                    (doall (map #'(lambda (%RESTRICTNAM %MARKL %NUM)
                        ((lambda (X) (SET %NUM (eval (car X))) X)
                            (COND ((ATOM (car %MARKL)) %MARKL) ((cons %RESTRICTNAM %MARKL)))))
                        '(SMSUB SMOB1 SMOB2)
                        RESTRICTIONSß
                        '(!1 !2 !3))))
                (and
                    ;; CHECK THAT THIS DEFINITION SENSE MEETS ALL OF THE RESTRICTIONS SET FORTH IN THE DEFINITION UNDER RESTRICTIONSß
                    (ERRSET
                        ;; ENCLOSED IN A ERRSET SO THAT THE FAILURE OF A CHECK CAN CAUSE IMMEDIATE ESCAPE FROM THE MAPC
                        ;; AND HENCE TO THE AND WHICH CUTS OFF ALL FURTHER PROCESSING OF THIS DEFINITION SENSE
                        ;; TEMPORARY STORAGE ON THE PROPERTY LIST OF TEMP USED TO AVOID SEARCHING FOR THESE ITEMS ON THE
                        ;; ONE HAND OR THE CONFLICT OF NAMES BETWEEN THE THE MARKERS RESULTING FROM CHECKING THE REL ARE
                        ;; SAVED TO PUT ON IT LATER WHEN THE CLAUSE IS RELATED.
                        (dorun (map #'(lambda (%MARKL)
                            (let [OSS nil X nil CHECK nil]
                                (SETQ OSS (eval (car %MARKL)))
                                (and (SETQ X (CHECKREL OSS)) (SETQ RELß (car X)))
                                (COND
                                    ((not (and (or (nil? (cddr %MARKL))
                                            (eval (caddr %MARKL)))
                                            (SETQ CHECK
                                                (CHECK (cadr %MARKL)
                                                (MARKERS? OSS)
                                                (SYSTEMS? OSS)))))
                                        (ERR nil))
                                    ((EQ OSS RELß)
                                        (SETQ RELMARKERSß CHECK)))
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
                        ;; CURRENT NODE ***, !1, !2, AND !3 SUBSTITUTIONS DONE.  THIS IS FIRST STEP IN BUILDING PLANNER CODE.
                        ;; NUMSUB IS THE ****, #!, #", !3 SUBSTITUTION FUNCTION.
                        ;; %PLNRPHRASE IS ONE CHUNK OF CONDENSED PLANNER CODE LIKE (!COLOR *** !RED).
                        (BUILD
                            RSSNODE= (SETQ RSSNAME (MAKESYM 'RSS))
                            MARKERS= MARKERSß
                            VARIABLE= ((lambda (X) (PUTPROP X X 'RSSVAR)) (MAKESYM 'EVX))
                            PARSENODE= C
                            RELATIONS= (reverse (doall (map #'(lambda (%PLNRPHRASE) (PLNR-NUMSUB '<<<RELATION-ERROR>>> %PLNRPHRASE)) (EVALCHECK PROCEDUREß))))
                            REL= RELß
                            NEGATIVE= (and (CQ NEG) true)
                            RELMARKERS= RELMARKERSß
                            PLAUSIBILITY= (+ (PLAUSIBILITY? SMSUB) (PLAUSIBILITY? SMOB1) (PLAUSIBILITY? SMOB2) (or (eval PLAUSIBILITYß) 0))
                            AMBIGUITIES= (concat (AMBIGUITIES? !1) (AMBIGUITIES? !2) (AMBIGUITIES? !3)
                                                 (and PARAPHRASEß (list (list RSSNAME PARAPHRASEß WORD-BEING-INTERPRETED)))))))
            (RETURN %NEWRSS)))
    %DEFL SMSUB SMOB1 SMOB2 SMOBL SMCOMP))

(§ DEFUN DOBACKREF [ANSWER]
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
    (let [ANSRSS nil]
        (SETQ ANSRSS (GET ANSWER 'ANSRSS=))
        (SETQ BACKREF2 BACKREF)
        (SETQ BACKREF nil)
        (SETQ LASTREL (REL? ANSRSS))
        (dorun (map #'(lambda (PRONOUN) (PUTPROP PRONOUN (GET PRONOUN 'BIND) 'LASTBIND) (REMPROP PRONOUN 'BIND)) '(IT THEY ONE)))
        (or
            (CQ MODAL)
            (CQ DECLAR)
            (MAP #'(lambda (BACKNODE)
                (COND ((cdr (SM BACKNODE)) ;; TRUE IF NODE HAD MULTIPLE INTERPRETATIONS
                    (PRINT (SM BACKNODE))
                    (SETR 'SEMANTICS (ERT "DOBACKREF:" RETURN AN OSS FOR BACKNODE) BACKNODE)))
            (COND
                ((REFER? (car (SM BACKNODE)))) ;; IF NODE HAS REFERENT, FINE
                (:else
                    (PUTPROP (car (SM BACKNODE))
                        (or (GET (VARIABLE? (car (SM BACKNODE))) 'BIND) (ERT "DOBACKREF:" RETURN REFERENT FOR BACKNODE))
                        'REFER=))))
            BACKREF2))
        ;; A FEW MISSING PIECES GO HERE
        nil))

(§ DEFUN EVALCHECK [L]
    ;; EVALCHECK CHECKS FOR THE PRESENCE OF (!EVAL (MUMBLE ...) ...) IN THE INPUT S-EXPRESSION L.
    ;; IF IT FINDS ONE THEN THE EXPRESSION MUMBLE IS EVALUATED AND REPACES (!EVAL ...), OTHERWISE
    ;; L IS RETURNED JUST THE WAY IT WAS.  HENCE THIS FUNCTION IS THE INVERSE OF QUOTE.
    (COND ((ATOM L) L)
        ((EQ (car L) '!EVAL) (eval (cadr L)))
        (L)))

(§ DEFUN ITERATE FEXPR [%L]
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
    (let [%X nil %XL nil] (RETURN (eval (ITERATEX (car %L) (cdr %L))))))
    ;; THIS SHOULD BECOME A MACRO SO THAT ITERATE TAILORS A MAPPING FUNCTION
    ;; FOR EACH APPLICATION WHICH IS THEN COMPILED

(§ DEFUN ITERATEX [F L]
    ;; EXPANDS CALL ON ITERATE INTO A TAILORED SET OF MAPBLAND
    ;; CALLS WHICH DO THE APPROPRIATE COMPUTATION WHEN EVALUATED.
    ;; INPUTS:
    ;;      F      FUNCTION OF N ARGUMENTS
    ;;      L      LIST OF N LISTS WHICH F IS TO BE APPLIED TO
    ;; VALUE:
    ;;  TAILORED FUNCTION
    (COND ((nil? L) (cons (eval F) %XL))
        ((list 'MAPBLAND
            (list 'FUNCTION (list 'LAMBDA
                ;; %X IS USED TO STORE THE VARIABLE NAME WHICH IT GETS FROM (GENSYM)
                ;; %XL IS USED TO SAVE A LIST OF ALL OF THE VARIABLE NAMES SO FAR SO
                ;; THAT THEY CAN BE GIVEN TO THE FUNCTION AT THE END (CONS %X NIL).
                ;; CREATES A SHINY NEW CELL WHICH IS BOLTED ONTO THE BACK END OF %XL
                ;; BY NCONC.  THIS PAIR IS NECESSARY BECAUSE SETQ RETURNS AS ITS VALUE
                ;; THE RESULT OF THE LAST PAIR THAT IT PROCESSES.  A RUSE. PUTS (NIL)
                ;; IN PLACE OF NIL AS A LIST TO PREVENT MAPBLAND FROM QUITTNG.
                (list (SETQ %X (GENSYM) %XL (concat %XL (cons %X nil)) %X %X))
                (ITERATEX F (cdr L))))
            (or (car L) '(nil))))))

(§ DEFUN MAPBLAND [FN L]
    ;; THIS IS THE INSTAMATIC CAMERA FUNCTION.  NO MATTER WHAT YOU
    ;; PUT INTO THE FUNCTION, IT AUTOMATICALLY ADJUSTS INTERNALLY TO
    ;; THE AVAILABLE LIGHT SO THAT WHAT COMES OUT THE END ALWAYS
    ;; LOOKS THE SAME -- ONE BIG NIL-LESS LIST OF ALL THE APPLICATIONS
    ;; OF THE FUNCTION FN.  THE PURPOSE IN THIS IS SO THAT
    ;; THE FUNCTION CAN BE EASILY NESTED.
    ;;
    ;; INPUTS:
    ;;     FN  -  FUNCTION OF ONE ARGUMENT TO BE APPLIED TO EACH ELEMENT IN L
    ;;     L   -  LIST
    ;; VALUE:
    ;;     IF (FN L) IS AN ATOM, THEN A LIST OF ATOMS.
    ;;     IF (FN L) IS A LIST, THEN ALL THE LISTS APPENDED, THAT IS A LIST OF ATOMS.
    ;;     IF (FN L) IS NIL, THEN ALL TRACE DISAPPEARS (SAVE FOR SIDE-EFFECTS).
    (let [ANS nil F nil]
        (and (nil? L) (SETQ L '(nil)))
    =>  (COND ((nil? (SETQ F ((eval 'FN) (car L)))))
            ((ATOM F) (SETQ ANS (concat ANS (cons F nil))))
            ((SETQ ANS (concat ANS F))))
        (SETQ L (cdr L))
        (and L (GO =>))
        (RETURN ANS)
        (GO =>)))

(§ DEFUN MAPC2 [FN L]
    ;; MAPPING FUNCTION FOR GOING 2 AT A TIME THROUGH A LIST.
    ;; INPUTS:
    ;;     FN   -   FUNCTION OF TWO ARGUMENTS
    ;;     L    -   LIST (ESPECIALLY ATTRIBUTE VALUE TYPE LIST)
    ;; VALUE:
    ;;  LIST (LIKE MAPCAR) OF FN APPLIED TO TOP TWO ELEMENTS.
    (let [DUMMY nil]
        ;; DUMMY IS USED TO ESCAPE FROM A SYSTEM ERROR WHICH OCCURS WHEN NIL IS USED.
        ;; FN APPLIED TO TOP TWO ELEMENTS.  EVAL IS TO AVOID CONFLICT WITH FUNCTION
        ;; REALLY NAMED FN.  LIST IS STEPPED TWO AT A TIME.
    =>  (COND ((nil? L) (RETURN true)))
        ((eval 'FN) (car L) (cadr L))
        (SETQ L (cddr L))
        (GO =>)))

(§ DEFUN MUMBLE [X]
    ;; MUMBLE IS THE PLANNER FILTER FOR LOOKING AT ASSERTIONS TO
    ;; SEE WHEN THEY WERE MENTIONED IN THE DIALOG. IT USES THE FREE
    ;; VARIABLE "WHO" TO DECIDE WHAT TO LOOK FOR. WHO IS EITHER NIL
    ;; (USE ANYTHING) "HE" (USE ANYTHING WHICH HAS BEEN MENTIONED)
    ;; OR A LIST OF TWO SENTENCE NUMBERS, MIN AND MAX (USE ANY WHO
    ;; PROPERTY WHICH IS ON OR BETWEEN THEM). THE WHO PROPERTY OF
    ;; ANY ASSERTION IS A SENTENCE NUMBER WHEN IT WAS MOST RECENTLY
    ;; MENTIONED.
    (COND ((nil? WHO))
        ((EQ WHO 'HE) (GET X 'WHO))
        ((SETQ X (GET X 'WHO)) (not (or (< X (car WHO)) (> X (cadr WHO)))))))

(§ DEFUN OBJECT FEXPR [%DEFL]
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
            (ITERATE #'(lambda (%OSS %DEF)
                (let [%OSSNODE nil %CHKRESULT nil MARKERSß nil SYSTEMSß nil PLAUSIBILITYß nil DETß nil RELATIONSß nil PARAPHRASEß nil PROCEDUREß nil]
                    ;; DECODE KEYWORDS
                    (SETQQCHECK nil %DEF '(MARKERSß PLAUSIBILITYß PARAPHRASEß PROCEDUREß) 'OBJECT)
                    ;; CHECK FOR MARKER AGREENT.  IF OK, THEN BUILD OSS, ELSE CHUCK THIS COMBINATION.
                    (and
                        (SETQ %CHKRESULT (CHECK MARKERSß (MARKERS? %OSS) (SYSTEMS? %OSS)))
                        ;; BUILD OSS COMBINING INFORMATION FROM CURRENT OSS WITH INFORMATION IN THE DEFINITION.
                        ;; NOTE THAT THE INITIAL OSS WHICH GETS BUILT UP FOR A WORD DEPENDS NOT ONLY ON ITS DEFINITION,
                        ;; BUT ALSO ON THE CONTEXT IN WHICH IT IS USED.
                        (RETURN (BUILD
                            OSSNODE= (SETQ %OSSNODE (MAKESYM 'OSS))
                            PARSENODE= (PARSENODE? %OSS)
                            MARKERS= (car %CHKRESULT)
                            SYSTEMS= (cadr %CHKRESULT)
                            DETERMINER= (DETERMINER? %OSS)
                            VARIABLE= (VARIABLE? %OSS)
                            RELATIONS= (concat (reverse (doall (map #'(lambda (%PLNRPHRASE) (PLNR-NUMSUB %OSS %PLNRPHRASE)) (EVALCHECK PROCEDUREß)))) (RELATIONS? %OSS))
                            REL= (REL? %OSS)
                            ;; THE OSS NAME PROVIDES A UNIQUE LABEL FOR WHERE THE AMBIGUITY OCCURRED FOR LATER COMPARISON.
                            AMBIGUITIES= (concat (AMBIGUITIES? %OSS) (and PARAPHRASEß (list (list %OSS PARAPHRASEß WORD-BEING-INTERPRETED))))
                            PLAUSIBILITY= (+ (or (eval PLAUSIBILITYß) 0) (PLAUSIBILITY? %OSS)))))
                    nil))
            SM %DEFL)))))

;; ######################################################
;;
;;              PLANNER BUILDING ROUTINES
;;
;; ######################################################

(§ DEFUN PLNR-JUNKIFY [CODE]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (COND
        ((ATOM CODE) CODE)
        ((EQ (car CODE) 'THGOAL) (list 'THAND CODE '(VALUEPUT)))
        ((EQ (car CODE) 'THFIND) (list 'THAND CODE (list 'THPUTPROP (QUOTIFY (cadr (caddr CODE))) 'THVALUE ''BIND)))
        ((or (EQ (car CODE) 'THAND) (EQ (car CODE) 'THPROG)) (doall (map #'PLNR-JUNKIFY2 CODE)))
        ((doall (map #'PLNR-JUNKIFY CODE)))))

(§ DEFUN PLNR-JUNKIFY2 [CODE]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (COND
        ((ATOM CODE) (list CODE))
        ((EQ (car CODE) 'THGOAL) (list CODE '(VALUEPUT)))
        ((EQ (car CODE) 'THFIND) (list CODE (list 'THPUTPROP (QUOTIFY (cadr (caddr CODE))) 'THVALUE ''BIND)))
        ((or (EQ (car CODE) 'THAND) (EQ (car CODE) 'THPROG)) (list (doall (map #'PLNR-JUNKIFY2 CODE))))
        ((list (doall (map #'PLNR-JUNKIFY CODE))))))

(§ DEFUN VALUEPUT [] (PUTPROP THVALUE SENTNO 'WHO))

(§ DEFUN PLNR-THCONSIFY [VARLIST EXP BODY]
    ;; GENERATES A CONSEQUENT THEOREM.
    (let [TH nil]
        (SETQ TH (MAKESYM 'THEOREM))
        (PUTPROP TH
            (COND ((EQ (car BODY) 'THPROG)
                    (concat (list 'THCONSE (UNION VARLIST (cadr BODY)) EXP) (cddr BODY)))
                (:else (list 'THCONSE VARLIST EXP BODY)))
            'THEOREM)
        (RETURN TH)))

(§ DEFUN PLNR-FINDIFY [MODE VARIABLE VARLIST BODY]
    ;; GENERATES A THFIND STATEMENT FOR THE NOUN GROUP DESCRIBED IN THE OSS.
    ;; IT (CURRENTLY) ASSUMES THAT THE PLNRCODE PROPERTY OF THE OSS IS A LIST
    ;; OF PATERNS OF THGOAL STATEMENTS.  MODE IS DEFINED TO BE <MODE> IN THE
    ;; MICRO-PLANNER DESCRIPTION OF THFIND (SEE AI MEMO !203A) BODY
    ;; IS A SINGLE PLANNER EXPRESSION (POSSIBLY A THAND OR THPROG).
    (COND
        ((EQ (car BODY) 'THAND) (SETQ BODY (cdr BODY)))
        ((EQ (car BODY) 'THPROG) (SETQ VARLIST (concat VARLIST (cadr BODY))) (SETQ BODY (cddr BODY)))
        ((SETQ BODY (list BODY))))
    ;; VARLIST = <SKELETON>
    ;; BODY = <VARIABLE DECLARATIONS>
    (concat (list 'THFIND MODE (PLNR-VAR VARIABLE) VARLIST) BODY))

(§ DEFUN PLNR-FINDSPEC [X]
    ;; GENERATES PAMETER FOR THFIND FROM THE NOTATION USED IN THE DETERMINER?
    (COND
        ((number? X) X)
        ((MEMQ X '(NS NPL SG-PL)) 1)
        ((EQ (car X) 'EXACTLY) (list (cadr X) (inc (cadr X)) nil))
        ((EQ (car X) '>) (inc (cadr X)))
        ((EQ (car X) '<) (list 0 (cadr X) nil))
        ((ERTERR PLNR-FINDSPEC -- FUNNY SPECIFICATION))))

(§ DEFUN PLNR-GOALIFY [PLNRPHRASE]
    ;; TAKES A PLNRPHRASE AND MAKES A THGOAL STATEMENT OUT OF IT UNLESS
    ;; IT ISN'T SUPPOSED TO BE ONE.  ALSO CALLS PLNR-NOTIFY IF APPROPRIATE.
    ;; PRESENT TENSE TIME MARKERS ARE REMOVED TO SIMPLIFY THE MOST COMMON EXPRESSIONS.
    (SETQ PLNRPHRASE (PLNR-REMTIME PLNRPHRASE))
    (COND ((GET (car PLNRPHRASE) 'NOGOAL) PLNRPHRASE)
        ((concat (list 'THGOAL PLNRPHRASE '(THDBF MUMBLE)) (PLNR-RECOMMENDIFY PLNRPHRASE)))))

(§ DEFUN PLNR-MUNG [FINDEXPR CANDIDATES]
    ;; DOES A HORRIBLE THING: MUNGS A THFIND EXPRESSION WHICH FINDS A NOUN GROUP REFERENCE.
    ;; IT PUTS A THAMONG EXPRESSION AS THE FIRST STATEMENT OF THE THFIND.  IF THERE IS ALREADY
    ;; A THAMONG EXPRESSION IN THE THFIND, THEN MUNG JUST CLOBBERS THE LIST IN THAT THAMONG.
    (cons (car FINDEXPR)
        (cons (cadr FINDEXPR)
            (cons (caddr FINDEXPR)
                (cons (cadddr FINDEXPR)
                    (cons (list 'THAMONG (caddr FINDEXPR) (QUOTIFY CANDIDATES))
                        (COND ((EQ (caaddr (cdr FINDEXPR)) 'THFIND) (cddddr (cdr FINDEXPR)))
                            (:else (cddddr FINDEXPR)))))))))

(§ DEFUN PLNR-NOTIFY [NEG? %PLNRPHRASE]
    ;; PUTS IN THNOT, BUT ELIMINATE DOUBLE NEGATIVES.
    (COND ((not NEG?) %PLNRPHRASE)
        ((EQ (car %PLNRPHRASE) 'THNOT) (cadr %PLNRPHRASE))
        ((list 'THNOT %PLNRPHRASE))))

(§ DEFUN PLNR-NEWBODY [X] (SETQ NEWBODY (cons X NEWBODY)))

(§ DEFUN PLNR-PROGIFY [VARLIST BODY]
    ;; SETS UP A THPROG OR THE SIMPLEST EQUIVALENT EXPRESSION FOR
    ;; THE PARTICULAR CASE.  BODY IS A LIST OF EXPRESSIONS
    (let [NEWBODY nil]
        (or BODY (RETURN nil))
        (dorun (map #'(lambda (X) (COND
                ((EQ (car X) 'THPROG)
                    (COND ((MEET VARLIST (cadr X)) (PLNR-NEWBODY X))
                        (:else (SETQ VARLIST (concat VARLIST (cadr X))) (dorun (map #'PLNR-NEWBODY (cddr X))))))
                ((EQ (car X) 'THAND)
                    (dorun (map #'PLNR-NEWBODY (cdr X))))
                ((PLNR-NEWBODY X))))
            BODY))
        (RETURN (COND
            (VARLIST (cons 'THPROG (cons VARLIST (reverse NEWBODY))))
            ((cdr NEWBODY) (cons 'THAND (reverse NEWBODY)))
            ((car NEWBODY))))))

(§ DEFUN PLNR-NUMREL [OSS]
    ;; THIS IS USED BY PLNR-NUMSUB TO HAVE THE VARIABLE NAME SUBSTITUTED
    ;; FOR THE OSS WHICH IS THE REL OF A PARTICULAR EXPRESSION.
    (COND ((MEMQ OSS RELLIST) (SETQ RELß OSS)) (OSS)))

(§ DEFUN PLNR-NUMSUB [%ME %PLNRPHRASE]
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
    (doall (map #'(lambda (%ELEMENT) ;; %ELEMENT IS AN ATOM OF THE PHRASE
            (COND
                ((MEMQ %ELEMENT '(!1 !2 !3)) (PLNR-NUMREL (eval %ELEMENT)))
                ((EQ %ELEMENT '***) %ME)
                ((EQ %ELEMENT '*TIME) TIME) ;; GETS THE CURRENT TIME
                (%ELEMENT)))
        (EVALCHECK %PLNRPHRASE))))

(§ DEFUN PLNR-RECOMMENDIFY [%PLNRPHRASE]
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
            (and (SETQ %ELEMENT (GET (car %PLNRPHRASE) 'THMLIST))
                (eval (COND
                    ((number? (caar %ELEMENT)) (cadr (or (ASSQ (count %PLNRPHRASE) %ELEMENT) '(nil nil))))
                    (%ELEMENT)))))))

(§ DEFUN PLNR-REMTIME [EXP]
    ;; REMOVES ALL PRESENT TENSE TIME STRUCTURES
    ((lambda (Y)
        ;; Y IS BOUND TO A UNIQUE POINTER SO IT CAN'T POSSIBLY SCREW ANYTHING
        ;; IN THE EXPRESSION WHEN IT DOES THE DELETION.  DELQ USES EQ.
        (DELQ Y
            (doall (map #'(lambda (X) (COND
                ((not (ATOM X)) X)
                ((TSS? X) (COND ((and (TENSE? X) (not (MEMBER (TENSE? X) '((PRESENT PRESENT) (MODAL) (PRESENT))))) X) (Y)))
                (X)))
            EXP))))
        '(T)))

(§ DEFUN PLNR-VAR [X]
    ;; GENERATES SYNTAX FOR VARIABLE NAMES IN PLANNER
    (list 'THV X))

(§ DEFUN COMPARE-BUILD [NODE DEGREE]
    ;; USED BY SMADJG-PREPG TO BUILD A PSUEDO VERB FOR THE COMPARATIVE.  SMCL1 IS THEN CALLED.
    (let [RESTRICTIONSß nil DIMENSIONß nil DIRECTIONß nil]
        ;; THESE ARE THE POSSIBLE PARTS OF A MEASURE.
        (SETQQCHECK nil (cdr (FINDMEASURE NODE)) '(RESTRICTIONSß DIMENSIONß DIRECTIONß) 'MEASURE)
        ;; DEFINITION
        (PUTPROP 'COMPARE-PSEUDO-VERB
            (list 'RELATION
            (list 'RESTRICTIONSß
                (list (list RESTRICTIONSß)
                (list RESTRICTIONSß))
                'PROCEDUREß (list (list DEGREE DIMENSIONß (COND (DIRECTIONß '!1) ('!2)) (COND (DIRECTIONß '!2) ('!1))))))
            'SEMANTICS)
        (RETURN '(COMPARE-PSEUDO-VERB))))

(§ DEFUN FINDMEASURE [NODE]
    ;; GETS THE MEASURE DEFINITION
    (COND
        ((SETQ X (ASSOC 'MEASURE (GET (ROOT (NB NODE)) 'SEMANTICS))) (cadr X))
        ((GLOBAL-ERR (concat '(I DON'T KNOW HOW TO COMPARE THINGS WITH RESPECT TO) (list (ROOT (NB NODE))))))))

(§ DEFUN MEASURE FEXPR [MEAS]
    ;; USED TO GENERATE ORDINALS -- IT IS CALLED WHEN A MEASURE DEFINITION IS EVALLED
    (APPLY #'OBJECT
        (list (list 'MARKERSß
            (cadr (MEMQ 'RESTRICTIONSß MEAS))
            'PROCEDUREß
            (list (list '*ORDINAL* MEAS))))))

(§ DEFUN PLNR-DESCRIBE [EXPS VAR FREEVARS]
    ;; BUILDS THE PLANNER DESCRIPTION, IGNORING THE QUANTIFIER ACOUNTS FOR ORDINALS, SUBSTS, ETC.
    (let [ORDINAL nil BODY nil X nil]
    =>  (COND
            ((nil? EXPS) (RETURN (COND (ORDINAL (ORDMAKE ORDINAL VAR BODY)) ((PLNR-PROGIFY nil BODY)))))
            ((EQ (SETQ X (EXPAND (car EXPS) (and (nil? (cdr EXPS)) (RSSVAR? VAR) (GET VAR 'USED) (PLNR-VAR VAR)))) '*ORDINAL*))
            ;; A SUBST DEFINITION IF IT IS THE ONLY THING IS TO BE APPLIED TO THE OSS TO WHICH THIS RSS WILL BE RELATED.
            ;; THE VARIABLE FOR A RELATION IS INSERTED INTO THE SECOND PLACE OF THE RELATION IF IT IS REFERRED TO ANYWHERE ELSE.
            ((and (cdr EXPS) (EQ (car X) '!SUBST)) (MAPC2 #'(lambda (X Y) (SETQ EXPS (SUBST X Y EXPS))) (cdr X)))
            (X (SETQ BODY (cons X BODY))))
        (SETQ EXPS (cdr EXPS))
        (GO =>)))

(§ DEFUN RELFIND [NODE]
    ;; LOOKS FOR THE REL OF A POLAR
    (let [REL nil]
        (ERRSET
            ;; IT GOESFROM THE BEGINNINGOF THE SENTENCE LOOKING FOR AN INDEFINITE NG,
            ;; EITHER AT THE TOP LEVEL OR AS A FIRST LEVEL PREPOBJ, BUT NOT A COMPLEMENT.
            (MAP #'(lambda (X) (COND
                ((ISQ X NG) (and (not (ISQ X COMP)) (not (ISQ X DEF)) (SETQ REL X) (ERR nil)))
                ((ISQ X LOBJ) (and (ISQ (H X) INDEF) (SETQ REL X) (ERR nil)))
                ((ISQ X PREPG) (and (ISQ (H X) INDEF) (SETQ REL (H X)) (ERR nil)))))
            (reverse (H NODE))))
        (or REL (and (CQ PASV) (not (CQ AGENT)) (SETQ REL '(FAKE-AGENT))))
        (RETURN (and REL (SM REL)))))

(§ DEFUN ORDMAKE [ORDINAL VAR BODY]
    ;; MAKES THE LOGICAL FORM FOR SUPERLATIVES.
    ;; ORDINAL GIVES THE THING BEING COMPARED IN MEASURE FORM.
    (let [NEWVAR nil]
        (SETQ NEWVAR (MAKESYM 'X))
        (RETURN (PLNR-PROGIFY nil
            (concat BODY (list (PLNR-NOTIFY true
                (PLNR-PROGIFY (list NEWVAR) (concat (SUBST NEWVAR VAR BODY) (list (PLNR-GOALIFY (COMPARE-PROC VAR NEWVAR ORDINAL))))))))))))

(§ DEFUN COMPARE-PROC [VAR NEWVAR ORDINAL]
    (let [RESTRICTIONSß nil DIRECTIONß nil DIMENSIONß nil]
        (SETQQCHECK nil ORDINAL '(RESTRICTIONSß DIRECTIONß DIMENSIONß) 'MEASURE)
        (RETURN (list '!MORE DIMENSIONß (PLNR-VAR (COND (DIRECTIONß NEWVAR) (VAR))) (PLNR-VAR (COND (DIRECTIONß VAR) (NEWVAR)))))))

(§ DEFUN EXPAND [EXP EVENT]
    ;; THE HEART OF THE PLANNER BUILDER.
    ;; EXPANDS AN EXPRESSION WORRYING ABOUT THE QUANTIFIERS AND CONNECTIVES OF ITS CONSTITUENTS.
    ;; IT DOESN'T REALLY HANDLE EXPRESSIONS WITH MORE THAN ONE QUANTIFIED SS UNLESS ONE OF THEM IS THE REL.
    ;; THE EVENT IS NON-NIL ONLY IF THE EVENT-NAME IS TO BE INCLUDED IN THE EXPANSION OF THE EXPRESSION.
    (COND
        ((RSS? EXP)
            (COND
                ((AND? EXP) (PLNR-PROGIFY nil (doall (map #'(lambda (X) (EXPAND X nil)) (AND? EXP)))))
                ((OR? EXP) (PLNR-ORIFY (doall (map #'(lambda (X) (EXPAND X nil)) (OR? EXP)))))
                ((PLNR-NOTIFY (NEGATIVE? EXP) (PLNR-DESCRIBE (RELATIONS? EXP) (VARIABLE? EXP) (cons (VARIABLE? EXP) FREEVARS))))))
        ((ATOM EXP) (BUG EXPAND - ATOMIC MODIFIER))
        ((EQ (car EXP) '*ORDINAL*)
            (COND (ORDINAL (GLOBAL-ERR '(I CAN'T HANDLE TWO ORDINALS OR SUPERLATIVES AT ONCE)))
                ((SETQ ORDINAL (cadr EXP)) '*ORDINAL*)))
        ((EQ (car EXP) '!SUBST)
            (ERT EXPAND - IS !SUBST BEING HANDLED BY SOMEONE ELSE?)
            EXP)
        ((let [BODY nil QUANTIFIER nil CHOICE nil VAR nil MULTIPLE nil]
            (SETQ MULTIPLE (eval (GET (car EXP) 'MULTIPLE)))
            (SETQ EXP
                (doall (map #'(lambda (X) (COND
                    ((or (not (ATOM X)) (not (or (RSS? X) (OSS? X))))
                        X)
                    ((REFER? X)
                        (COND ((cdr (REFER? X))
                            (COND (MULTIPLE (ERQSET 'AND) (SETQ CHOICE (REFER? X)) '*AND*)
                                ((REFER? X))))
                        ((car (REFER? X)))))
                    ((MEMQ (VARIABLE? X) FREEVARS)
                        (and (RSSVAR? (VARIABLE? X)) (PUTPROP (VARIABLE? X) true 'USED))
                        (PLNR-VAR (VARIABLE? X)))
                    ((SETQ CHOICE (AND? X))
                        (ERQSET 'AND)
                        (and MULTIPLE (REFER? X) (SETQ CHOICE (REFER? X)))
                        '*AND*)
                    ((SETQ CHOICE (OR? X))
                        (ERQSET 'OR)
                        '*OR*)
                    ((COND
                        ((RSS? X) (ERQSET 'EVENT) (PUTPROP (VARIABLE? X) true 'USED))
                        ((MEMQ (QUANTIFIER? X)
                            '(ALL NO)) (ERQSET (QUANTIFIER? X)) true)
                        ((MEMQ (QUANTIFIER? X) '(NDET INDEF))
                            (COND ((MEMQ (NUMBER? X) '(NS SG-PL)) (ERQSET 'INDEF)) ((SETQ CHOICE (PLNR-FINDSPEC (NUMBER? X))) (ERQSET 'FIND))) true))
                        (SETQ BODY (PLNR-DESCRIBE (RELATIONS? X) (VARIABLE? X) (cons (VARIABLE? X) FREEVARS)))
                        (PLNR-VAR (SETQ VAR (VARIABLE? X))))
                    ((ERTERR EXPAND - STRANGE QUANTIFIER))))
                    (COND (EVENT (cons (car EXP) (cons EVENT (cdr EXP)))) (:else EXP)))))
            ;; THE EVENT NAME IS STUCK INTO THE SECOND POSITION IF THERE IS ONE.
            (RETURN (COND
                ((nil? QUANTIFIER) (PLNR-GOALIFY EXP))
                ((EQ QUANTIFIER 'AND)
                    (PLNR-PROGIFY nil (doall (map #'(lambda (X) (EXPAND (SUBST X '*AND* EXP) nil)) CHOICE))))
                ((EQ QUANTIFIER 'OR)
                    (PLNR-ORIFY (doall (map #'(lambda (X) (EXPAND (SUBST X '*OR* EXP) nil)) CHOICE))))
                ((EQ QUANTIFIER 'FIND)
                    (PLNR-FINDIFY CHOICE VAR (list VAR) (PLNR-PROGIFY nil (cons BODY (list (PLNR-GOALIFY EXP))))))
                (:else
                    (PLNR-NOTIFY (MEMQ QUANTIFIER '(ALL NO))
                        (PLNR-PROGIFY (and VAR (list VAR)) (cons BODY (list (PLNR-NOTIFY (EQ QUANTIFIER 'ALL) (PLNR-GOALIFY EXP)))))))))))))

(§ DEFUN ERQSET [X]
    ;; USED BY EXPAND TO MAKE SURE IT ISN'T GETTING CONFUSED BY TOO
    ;; MANY CONNECTIVES AND QUANTIFIERS IN THE SAME EXPRESSION.
    (COND (QUANTIFIER (GLOBAL-ERR '(I CAN'T HANDLE COMBINATIONS OF QUANTIFIERS AND CONNECTIVES WHICH ARE SO COMPLICATED)))
        ((SETQ QUANTIFIER X))))

(§ DEFUN SETQQCHECK [%EVALFLAG %LIST %CHECKLIST %NAME]
    ;; SETQQCHECK IS LIKE SETQQ (OR LIKE SETQ DEPENDING ON EVALFLAG) BUT IT CHECKS TO MAKE SURE
    ;; THE VARIABLE NAME IS A MEMBER OF THE %CHECKLIST, AND IF NOT PRINTS AN ERROR MESSAGE.
    (let [%X nil]
    GO  (COND ((nil? %LIST) (RETURN true))
            ((MEMQ (car %LIST) %CHECKLIST)
                (SET (car %LIST) (COND (%EVALFLAG (eval (cadr %LIST))) (:else (cadr %LIST))))
                (SETQ %LIST (cddr %LIST))
                (GO GO))
            (:else (SETQ %X (APPLY #'ERT (cons (car %LIST) (concat '(IS NOT A LEGAL SPECIFICATION FOR) (list %NAME)))))))
    UP  (COND
            ;; A QUESTION MARK GETS THE LIST OF POSSIBILITIES PRINTED OUT, THEN LETS YOU TRY AGAIN.
            ;; TO DO THIS YOU MUST TYPE (RETURN '?) AT THE ERT.
            ;; IF YOU RETURN ANY OTHER VALUE, IT ASSUMES THIS IS THE VARIABLE NAME INTENDED,
            ;; OTHERWISE IT JUST CAUSES AN ERROR.
            ((EQ %X '?)
                (PRINT %CHECKLIST)
                (SETQ %X (ERT "FOO:" SETQQCHECK ????))
                (GO UP))
            ((SETQ %LIST (cons %X (cdr %LIST))) (GO GO)))))

(§ DEFUN THVAL2 [WHO AA]
    (let [RESULT nil X nil]
        (SETQ THLEVEL '(true))
        (SETQ X (SETQ RESULT '(nil)))
        (and PLANNERSEE (DISP AA) PLNRSEE-PAUSE (ERT FOR PLANNER))
        (and (not (EQ RESULT X))
            (RETURN RESULT))
        (SETQ RESULT (THVAL AA '((EV COMMAND))))
        (RETURN RESULT)))

(§ DEFUN WHO [X]
    (COND ((nil? WHO))
        ((ATOM X))
        ((not (SETQ X (GET X 'WHO))) nil)
        ((EQ WHO 'HE))
        ((< (car WHO) X LASTSENTNO))))

(§ DEFUN CHECK [NEW-MARKERS MARKERS SYSTEMS]
    ;; TAKES A LIST OF NEW MARKERS AND CHECKS FOR COMPATIBILITY WITH THE EXISTING
    ;; MARKERS AND SYSTEMS (AS GIVEN BY ARGS MARKERS AND SYSTEMS).  IF COMPATIBLE,
    ;; RETURNS A TWO-LIST OF THE NEW MARKERS AND SYSTEMS, ELSE RETURNS NIL.
=>  (COND
        ((nil? NEW-MARKERS) (RETURN (list MARKERS SYSTEMS)))
        ((CHECKAMARKER (car NEW-MARKERS)) (SETQ NEW-MARKERS (cdr NEW-MARKERS)) (GO =>))
        (:else (RETURN nil)))) ;; FAIL IF CHECKAMARKER FAILS

(§ DEFUN CHECKAMARKER [MARKER]
    ;; CHECKS A SINGLE MARKER FOR COMPATIBILITY
    ;; USES FREE VARIABLES:
    ;;    SYSTEMS - THE SYSTEM LIST SO FAR
    ;;    MARKERS - THE MARKER LIST SO FAR
    ;; IF SUCCESSFULL, THE MARKER AND ITS SYSTEM(S) ARE APPENDED TO THESE FREE VARIBLES
    (let [NEW-SYSTEMS nil]
        (COND ((MEMQ MARKER MARKERS) (RETURN true)))               ;; IF MARKER ALREADY THERE, FINE
        (SETQ MARKERS (cons MARKER MARKERS))                    ;; ADD NEW MARKER TO LIST
        (SETQ NEW-SYSTEMS (GET MARKER 'SYS))                    ;; GET THE SYSTEMS OF THE NEW MARKER
    =>  (COND ((nil? NEW-SYSTEMS) (RETURN true))
            ((MEMQ (car NEW-SYSTEMS) SYSTEMS) (RETURN nil)) ;; FAIL IF SYSTEM THERE BY ANOTHER PATH
            ((CHECKAMARKER (car NEW-SYSTEMS))
                (SETQ SYSTEMS (cons (car NEW-SYSTEMS) SYSTEMS))
                (SETQ NEW-SYSTEMS (cdr NEW-SYSTEMS))
                (GO =>))
            (:else (RETURN nil)))))

(§ DEFUN FINDEVENTS [RSS]
    ;; FINDS ALL THE EVENTS FITTING THE RSS DESCRIPTION
    (PUTPROP (VARIABLE? RSS) true 'USED)
    (THVAL2 nil
        (PLNR-FINDIFY 'ALL
            (VARIABLE? RSS)
            (list (VARIABLE? RSS))
            (PLNR-DESCRIBE (RELATIONS? RSS)
                (VARIABLE? RSS)
                (list (VARIABLE? RSS))))))

(§ DEFUN CHECKREL [OSS]
    ;; CHECKS FOR POSSIBLE RELATIVE, EITHER BECAUSE OSS IS ON THE RELLIST,
    ;; OR BECAUSE RSS INVOLVES INSIDE IT AN OSS ON THE RELLIST.
    ;; IT RETURNS EITHER NIL OR A LIST OF WHICH THE FIRST ELEMENT IS THE REAL RELATIVE.
    ;; IT USES THIS FACT TO CHEAT ON RECURSION BY USING MAPCAN.
    (COND
        ((OSS? OSS) (MEMQ OSS RELLIST))
        ((RSS? OSS) (doall (map #'(lambda (RELATION) (COND ((ATOM RELATION) nil) ((doall (map #'CHECKREL RELATION))))) (RELATIONS? OSS))))))

#_(ns shrdlu.smass)

;; ################################################################
;;
;;               SMASS - SEMANTIC ACCESS FUNCTIONS
;;
;; ################################################################

(§ DEFUN ACTION? [X]
    ;; THE WORKING PART OF AN ANSWER -- TELLS WHAT TO DO IF THE ANSWER IS THE ONE TO BE GIVEN.
    ;; MIGHT INCLUDE ACTIONS ON THE DISPLAY, AS WELL AS THINGS TO BE PRINTED AND VARIABLES TO BE SET, ETC.
    ;; THE WHOLE THING IS EVAL-LISTED.
    (GET X 'ACTION=))

(§ DEFUN AMBIGUITIES? [X]
    ;; LIST OF POSSIBLE AMBIGUITIES FOR A SEMANTIC STRUCTURE.
    (GET X 'AMBIGUITIES=))

(§ DEFUN AND? [X]
    ;; FIND THE CONJUNCTION LIST OF A CONJOINED SEMANTIC STRUCTURE.
    ;; NIL IF THERE IS NONE.
    (GET X 'AND=))

(§ DEFUN ANSRSS? [X]
    ;; THE RSS CORRESPONDING TO AN ANSWER NODE (A PROPERTY OF THE ANSNODE)
    (GET X 'ANSRSS=))

(§ DEFUN DETERMINER? [X]
    ;; ACCESS FUNCTION.  GETS DET OF AN OSS.
    (GET X 'DETERMINER=))

(§ DEFUN END? [TSS]
    ;; END TIME FOR TSS.
    (GET TSS 'END=))

(§ DEFUN MARKERS? [%SENSE]
    ;; ACCESS FUNCTION USED TO GET MARKERS FROM OSS OR RSS.
    (GET %SENSE 'MARKERS=))

(§ DEFUN MODIFIERS? [%XSS]
    ;; ACCESS FUNCTION FOR GETTING THE PLANNER CODE SCHEMA OF AN OSS OR RSS.
    (GET %XSS 'MODIFIERS=))

(§ DEFUN NEGATIVE? [%XSS]
    ;; ACCESS FUNCTION FOR OSS.
    (GET %XSS 'NEGATIVE=))

(§ DEFUN NUMBER? [OSS]
    ;; GETS THE NUMBER FIELD OF AN OSS.
    (car (GET OSS 'DETERMINER=)))

(§ DEFUN OR? [X]
    ;; ACCESS FOR LIST OF CONSTITUENTS IN DISJOINED SEMANTIC STRUCTURE.
    ;; NIL IF IT ISN'T.
    (GET X 'OR=))

(§ DEFUN OSS? [X]
    ;; CHECKS TO SEE IF X IS AN OSS.
    (GET X 'OSSNODE=))

(§ DEFUN PARENT? [NODE] (GETR 'PARENT NODE))

(§ DEFUN PARSENODE? [X] (GET X 'PARSENODE=))

;; THE PARSE NODE ASSOCIATED WITH THE SEMANTIC STRUCTURE.

(§ DEFUN PLAUSIBILITY? [%XSS]
    ;; ACCESS FUNCTION FOR GETTING PLAUSIBILITY OF AN OSS OR RSS.
    (or (GET %XSS 'PLAUSIBILITY=) 0))

(§ DEFUN PLNRCODE? [X] (GET X 'PLNRCODE=))

;; THE PLANNERCODE GENERATED WHEN AN OBJECT IS ACTUALLY LOOKED FOR IN THE DATA BASE.
;; IT IS NOT USED AGAIN, BUT IS LEFT SITTING AROUND FOR PEOPLE TO LOOK AT.

(§ DEFUN QTYPE? [X] (caddr (GET X 'DETERMINER=)))

;; QUESTION TYPE FOR QUESTION OSS.

(§ DEFUN QUANTIFIER? [OSS]
    ;; GETS THE DETERMINER FIELD OF AN OSS.
    (cadr (GET OSS 'DETERMINER=)))

(§ DEFUN REFER? [%XSS]
    ;; ACCESS FUNCTION FOR REFER OF OSS OR RSS.
    (GET %XSS 'REFER=))

(§ DEFUN REL? [X] (GET X 'REL=))

;; THE OBJECT TO WHICH THIS DESCRIPTION IS TO BE RELATED.

(§ DEFUN RELATIONS? [X]
    ;; THE MATERIAL THAT WILL BECOME PLANNER CODE.
    (GET X 'RELATIONS=))

(§ DEFUN RELMARKERS? [X]
    ;; MARKERS HELD BY A RELATIVE CLAUSE PRODUCED BY ITS SELECTION
    ;; RESTRICTIONS, TO BE ATTACHED TO THE OBJECT DESCRIPTION.
    (GET X 'RELMARKERS=))

(§ DEFUN RSS? [X]
    ;; CHECKS TO SEE IF X IS AN RSS.
    (GET X 'RSSNODE=))

(§ DEFUN RSSVAR? [X]
    ;; A VARIABLE OF THE TYPE USED FOR RSS'S -- I.E. EVX3, ETC.
    (GET X 'RSSVAR))

(§ DEFUN START? [TSS]
    ;; START TIME FOR TSS.
    (GET TSS 'START=))

(§ DEFUN SYSTEMS? [%SENSE]
    ;; ACCESS FUNCTION FOR GETTING THE SYSTEMS OF AN OSS OR RSS.
    (GET %SENSE 'SYSTEMS=))

(§ DEFUN TENSE? [X]
    ;; FOR A TSS.
    (GET X 'TENSE=))

(§ DEFUN TSS? [X] (GET X 'TSSNODE=))

;; ASSOCIATED WITH EVERY TIME SEMANTIC STRUCTURE.

(§ DEFUN VARIABLE? [X]
    ;; ACCESS FUNCTION FOR GETTING THE VARIABLE NAME ASSOCIATED WITH AN OSS OR RSS.
    (GET X 'VARIABLE=))

(§ DEFUN SMSET [X] (SETR 'SEMANTICS X C) (SETQ SM X))

#_(ns shrdlu.newans)

;; ################################################################
;;
;;              NEWANS - (NEW) ANSWERING COMPONENT
;;
;; ################################################################

(§ DEFUN ANSWER [NODE]
    ;; THE TOP LEVEL ANSWER FUNCTION CALLED TO CARRY OUT THE RESULTS OF ANY INPUT SENTENCE,
    ;; WHETHER COMMAND, QUESTION, OR STATEMENT.
    (let [ANSLIST nil AMBIG nil]
        ;; ANSLIST IS THE LIST OF POSSIBLE ANSWERS.
        ;; AMBIG IS A FLAG SET IF THERE IS A POSSIBLE AMBIGUITY.
        ;; CLEAR OUT ANSWER NAMES SAVED FOR BACKREF(ERENCE), I.E. MORE THAN ONE RSS FOR THE SENTENCE.
        (SETQ ANSNAME nil)
        (SETQ AMBIG (cdr (SM NODE)))
        (SETQ ANSLIST (ANSORDER (ANSUNIQUE (doall (map #'ANSGEN (SM NODE))))))
        ;; ANSGEN GENERATES AN ANSWER FOR EACH INTERPRETATION.
        ;; ANSUNIQUE TAKES OUT REDUNDANT ONES IN THE CASE THAT DIFFERENT INTERPRETATIONS LEAD TO THE SAME ANSWER.
        ;; ANSORDER ORDERS THE REMAINING ONES BY PLAUSIBILITY.
        ;; IF NO ANSWER IS CLEARLY BEST, ASK THE USER FOR CLARIFICATION AND TRY AGAIN.
    CHOOSE (COND
            ((and (cdr ANSLIST) (not (ENOUGH-BETTER (car ANSLIST) (cadr ANSLIST))))
                (SETQ ANSLIST (ANSELIMINATE ANSLIST))
                (GO CHOOSE)))
            (or ANNOYANCE (PRINT *3))
    TEST-LOOP
        (and ANS-AFTERFORMULATION-PAUSE  (ERT ANSWER HAS BEEN DETERMINED))
        ;; THE ACTION INCLUDES BOTH THE THINGS TO BE DONE
        ;; AND THE INSTRUCTIONS FOR PRINTING A RESPONSE.
        (EVLIS (ACTION? (car ANSLIST)))
        (PRINC '\.)
        (TERPRI)
        (and ANS-TEST? (GO TEST-LOOP))
        ;; DOBACKREF STORES AWAY DISCOURSE INFORMATION.
        (DOBACKREF (car ANSLIST))
        (RETURN true)))

(§ DEFUN AMBPUT [CODE]
    ;; PUTS IN THE JUNK FOR DISCOURSE IF THERE IS NO AMBIGUITY, SO THERE IS
    ;; NO NEED TO EVALUATE THE CODE A SECOND TIME WHEN GIVING THE ANSWER.
    (COND (AMBIG CODE) (:else (PLNR-JUNKIFY CODE))))

(§ DEFUN ANSAY [X]
    ;; GENERATES THE SYNTAX FOR ANSWER ACTIONS FROM A PHRASE.
    (list (cons 'SAY X)))

(§ DEFUN ANSBUILD [PLAUS ACTION REDEDUCE]
    ;; BUILDS AN ANSWER NODE.
    ;; IF REDEDUCE IS NON-NIL, IT ADDS A REDEDUCTION OF THE ANSWER,
    ;; ADDING THE DISCOURSE JUNK TO THE ACTION.
    (BUILD
        ANSNODE= (MAKESYM 'ANS)
        PLAUSIBILITY= PLAUS
        ANSRSS= RSS
        ACTION= (concat
                    (COND
                        ((and AMBIG REDEDUCE (not (CQ DECLAR)))
                            (cons (list 'THVAL2 nil (list 'PLNR-JUNKIFY (list 'PLNRCODE? (list 'quote RSS)))) ACTION))
                        (:else ACTION))
                    (and (REL? RSS) (not (CQ DECLAR)) (list (list 'PUTPROP (QUOTIFY (REL? RSS)) (QUOTIFY ANS) (QUOTIFY 'REFER=)))))))

(§ DEFUN ANSCOMMAND [RSS]
    ;; ANSCOMMAND RESPONDS TO IMPERATIVES.
    (let [EXP nil ANS nil SUCCESS nil PLAN nil PLAN2 nil]
        ;; PLNR-ANDORIFY COMBINES ANDS AND ORS INTO APPROPRIATE PLANNER THANDS AND THORS.
        (SETQ EXP (PLNR-ANDORIFY RSS))
        (PUTPROP RSS EXP 'PLNRCODE=)
        (SETQ EXP (AMBPUT EXP))
        (SETQ EXP (COND
            ((EQ (car EXP) 'THAND) (concat EXP '((SETQ SUCCESS true) (SETQ PLAN2 PLAN))))
            (:else (list 'THAND EXP '(SETQ SUCCESS true) '(SETQ PLAN2 PLAN)))))
        ;; IN CASE OF MULTIPLE INTERPRETATION, THE SYSTEM USES FAILURE TO WIPE OUT THE EFFECTS OF TRYING OUT ONE OF THEM.
        ;; BEFORE FAILING IT MARKS DOWN WHETHER IT SUCCEEDED AND SAVES THE PLAN FROM BACKTRACKING.
        ;; PLNR-JUNKIFY PUTS ON THE JUNK FOR SAVING THE DISCOURSE REFERENTS ETC.
        (THVAL2 nil (COND (AMBIG (concat EXP '((THFAIL)))) (:else EXP)))
        (RETURN
            ;; THE THIRD ARGUMENT TO ANSBUILD CAUSES THE SYSTEM TO GO BACK THROUGH THE DEDUCTION
            ;; TO GET THE DATA BASE STRAIGHT IF THIS ANSWER IS PICKED.  IT ALSO TAKES CARE OF THE BACKREF STUFF.
            (ANSBUILD
                (COND (SUCCESS (PLAUSIBILITY? RSS)) (:else (- (PLAUSIBILITY? RSS) 512)))
                (COND (SUCCESS (concat (reverse PLAN2) '((SAY OK)))) (:else '((SAY I CAN'T))))
                true))))

(§ DEFUN ANSDECLARE [RSS]
    ;; FOR DECLARATIVES.
    (COND
        ((OR? RSS)
            (GLOBAL-ERR I DON'T UNDERSTAND DISJUNCTIVE DECLARATIVES))
        ((AND? RSS)
            (let [ANS nil]
                ;; CONJOINED DECLARATIVES ARE HANDLED BY DOING EACH ONE SEPARATELY.
                (SETQ ANS (doall (map #'ANSDECLARE (AND? RSS))))
                (RETURN (ANSBUILD
                    (reduce #'+ (map #'PLAUSIBILITY? ANS))
                    (cons '(SAY I UNDERSTAND) (doall (map #'(lambda (X) (DELETE '(SAY I UNDERSTAND) (ACTION? X))) ANS)))
                    nil))))
        ((not (ISTENSE (PARSENODE? RSS) 'PRESENT))
            (GLOBAL-ERR I ONLY UNDERSTAND PRESENT TENSE DECLARATIVES))
        (:else (ANSBUILD
            (PLAUSIBILITY? RSS)
            ;; ANSTHM GENERATES THE APPROPRIATE ASSERTION OR THEOREM.
            (cons '(SAY I UNDERSTAND) (doall (map #'(lambda (X) (list 'THADD (QUOTIFY (ANSTHM X)) nil)) (RELATIONS? RSS))))
            nil))))

(§ DEFUN ANSELIMINATE [ANSLIST]
    ;; ELIMINATES ANSWERS FROM LIST BY ASKING PERSON TO CLEAR UP THE AMBIGUITIES.
    (let [AMB nil POSSIBILITIES nil XX nil]
        (or (SETQ AMB (AMBIGUITIES? (ANSRSS? (car ANSLIST))))
            (BUG ANSELIMINATE -- NO AMBIGUITIES LIST))
        ;; POSSIBILITIES IS THE LIST OF POSSIBLE INTERPRETATIONS FOR A SINGLE AMBIGUITY.
        ;; WE ARE INSIDE A LOOP STARTING AT UP WHICH GOES THROUGH ALL THE DIFFERENT POSSIBLE AMBIGUITIES ON THE LIST FOR THE FIRST ANSWER ON ANSLIST.
        ;; ON EACH ANSWER WE LOOK FOR POSSIBLE INTERPRETATIONS FOR THE PARTICULAR NODE WHERE THE AMBIGUITY WAS CREATED.
    UP  (SETQ POSSIBILITIES (list (car AMB)))
        (dorun (map #'(lambda (ANS)
            (and (SETQ XX (PARSE-ASSOC (caar AMB) (AMBIGUITIES? (ANSRSS? ANS))))
                (not (MEMBER XX POSSIBILITIES))
                (SETQ POSSIBILITIES (cons XX POSSIBILITIES))))
            (cdr ANSLIST)))
        (COND ((cdr POSSIBILITIES) true)
            ((SETQ AMB (cdr AMB)) (GO UP))
            (:else (BUG ANSELIMINATE -- NO CONFLICT)))
        (TERPRI)
        (SAY I'M NOT SURE WHAT YOU MEAN BY \") ;; "sic!
        (dorun (map #'PRINT2 (FROM (NB (caddar AMB)) (N (caddar AMB)))))
        (SAY \" IN THE PHRASE \") ;; "sic!
        (dorun (map #'PRINT2 (FROM (NB (SETQ XX (PARENT? (caddar AMB)))) (N XX))))
        (PRINC "\".")
        (TERPRI)
        (SAY DO YOU "MEAN:")
        (SETQ XX 0)
        (dorun (map #'(lambda (POSS)
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
            (MAPBLAND #'(lambda (ANS)
                (COND ((or (not (SETQ XX (PARSE-ASSOC (caar AMB) (AMBIGUITIES? (ANSRSS? ANS))))) (EQUAL XX POSSIBILITIES)) ANS)))
            ANSLIST))))

(§ DEFUN PARSE-ASSOC [OSS AMBIG-LIST]
    ;; PARSE-ASSOC GOES THRU AMBIG-LIST LOOKING FOR AN INTERPRETATION WITH THE SAME PARSE NODE
    (let [ASS nil]
        (SETQ ASS (car (PARSENODE? OSS)))
    =>  (COND ((nil? AMBIG-LIST) (RETURN nil))
            ((EQ ASS (car (PARSENODE? (caar AMBIG-LIST))))
                (RETURN (car AMBIG-LIST))))
        (SETQ AMBIG-LIST (cdr AMBIG-LIST))
        (GO =>)))

(§ DEFUN ANSGEN [RSS]
    ;; ANSGEN GENERATES AN ANSWER FOR A SINGLE INTERPRETATION.
    (COND
        ((or (CQ IMPER) (and (CQ QUEST) (ISTENSE (PARSENODE? RSS) 'FUTURE))) ;; FUTURE QUESTIONS ARE TREATED LIKE COMMANDS.
            (ANSCOMMAND RSS))
        ((CQ DECLAR)
            (let [X nil]
                (RETURN (COND
                    ((ERRSET (SETQ X (ANSDECLARE RSS))) X)
                    ;; THIS STRANGE CONSTRUCTION ALLOWS US A SECOND CHANCE ON DECLARATIVES ABOUT THINGS WHICH CAN'T
                    ;; BE TOLD TO THE SYSTEM.  IF IT RUNS INTO ONE OF THEM, IT TRIES TO ANSWER IT AS A QUESTION.
                    ((EQUAL GLOBAL-MESSAGE '(THAT ISN'T THE KIND OF THING I CAN BE TOLD)) (ANSQUEST RSS))
                    ((ERR nil))))))
        ((CQ QUEST) (ANSQUEST RSS))
        ((BUG ANSGEN -- WHAT KIND OF SENTENCE IS THIS?))))

(§ DEFUN ANSNAME [PHRASE]
    ;; THIS IS THE FUNCTION WHICH PARSES THE NAME PHRASES GENERATED BY THE ANSWER ROUTINES
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
    ;; THE WAY ANSNAME WORKS IS THE DECLARE A LOT OF THE RELAVENT PARSE FREE VARIABLES SO THAT
    ;; IT LOOKS A LITTLE LIKE SHRDLU.  THE CRITICAL VARIABLES ARE:
    ;;
    ;; CUT - WHICH TELLS THE NG GUY HOW FAR TO GO.
    ;; N   - WHICH CONTAINS THE CURRENT SENTENCE.
    ;; C   - WHICH CONTAINS THE PARENT OF THE NEXT NODE.
    ;;       WE WANT C TO BE NIL TO STOP THE NG PROGRAM FROM CRAWLING OVER THE PARSE TREE.
    (let [ANSNODE nil C nil N nil CUT nil]
        (SETQ N (cdaar PHRASE))                                     ;; CDR IS TO REMOVE "SAY"
        (SETQ ANSNODE (PARSE2 '(NG ANSNAME) true))                     ;; THE T SAYS NOT TO ATTACH THIS TO THE TREE
        (or ANSNODE
            (RETURN (ERT "ANSNAME:" FAILURE TO PARSE ANSWER NAME BUT IF YOU ONLY EXPECT THE ANSWER TO BE AN ADJ, PROCEED THIS AND DON'T WORRY)))
        (SETQ ANSNAME (concat ANSNODE ANSNAME))                     ;; LEAVE NODE AROUND IT ACCESSABLE PLACE
        (PUTPROP (car (SM ANSNODE)) (cadr PHRASE) 'REFER=)          ;; PUT THE REFERENT ON AS THE GUY GIVEN BY ANSWER
        nil))

(§ DEFUN ANSNOREL [RSS]
    ;; FOR QUESTIONS WITH NO RELATIVE, LIKE "DID YOU PICK UP THE BLOCK?" OR "WHY DID YOU DO THAT?"
    (let [ANS nil TYPE nil CODE nil NODE nil VAR nil]
        (SETQ NODE (PARSENODE? RSS))
        (SETQ TYPE (COND ;; THE TYPE SHOULD BE POLAR, WHY, WHERE, WHEN, OR HOW.
            ((ISQ NODE POLAR) 'POLAR)
            ((SETQ TYPE (GETR 'QADJ NODE)) (car (NB TYPE)))
            ((BUG ANSNOREL -- FUNNY TYPE))))
        (PUTPROP (VARIABLE? RSS) true 'USED)
        (SETQ CODE
            (PLNR-DESCRIBE (RELATIONS? RSS)
                ;; IN PRESENT TENSE CASES, WE DON'T LOOK FOR EVENTS.
                ;; OTHERWISE WE LOOK FOR A SET OF APPROPRIATE EVENTS NO MATTER WHAT THE TYPE.
                (COND ((ISTENSE NODE 'PRESENT) nil) ((SETQ VAR (VARIABLE? RSS))))
                (list (VARIABLE? RSS))))
        (PUTPROP RSS CODE 'PLNRCODE=)
        (RETURN (COND
            ((not VAR)
                (SETQ ANS (THVAL-MULT (AMBPUT CODE)))
                (ANSBUILD
                    (+ (car ANS) (PLAUSIBILITY? RSS))
                    (COND ((cadr ANS) '((SAY YES))) ((ISTENSE NODE 'MODAL) '((SAY I DON'T KNOW))) (:else '((SAY NO))))
                    true))
            ((SETQ ANS (THVAL-MULT (PLNR-FINDIFY 'ALL VAR (list VAR) (AMBPUT CODE))))
                (ANSBUILD
                    ;; AN ANSWER IS VERY IMPLAUSIBILE IF IT MENTIONS AN EVENT THE SYSTEM CAN'T FIND.
                    (COND ((cadr ANS) (+ (PLAUSIBILITY? RSS) (car ANS))) (:else (- (PLAUSIBILITY? RSS) 512)))
                    (COND ((nil? (cadr ANS)) '((SAY I CAN'T DISCUSS A NON-EXISTENT EVENT)))
                        ((concat (and (EQ TYPE 'POLAR) '((SAY YES))) (list (list 'EVLIS (list 'DESCRIBEVENT (QUOTIFY (cadr ANS)) (QUOTIFY TYPE)))))))
                    true))))))

(§ DEFUN ANSORDER [LIST]
    ;; ORDERS A LIST BY PLAUSIBILITY HIGHEST FIRST.
    (let [X nil Y nil]
    GO  (SETQ X LIST)
    UP  (COND ((nil? (cdr X)) (RETURN LIST))
            ((< (PLAUSIBILITY? (car X)) (PLAUSIBILITY? (cadr X)))
                (SETQ Y (car X))
                (RPLACA X (cadr X))
                (RPLACA (cdr X) Y)
                (GO GO))
            ((SETQ X (cdr X)) (GO UP)))))

(§ DEFUN ANSQUEST [RSS]
    ;; ANSQUEST ANSWERS ALL TYPES OF QUESTIONS BY SENDING THEM OUT
    ;; TO ANSREL OR ANSNOREL DEPENDING ON WHETHER THERE IS A REL.
    (COND
        ((or (OR? RSS) (AND? RSS))
            (let [ANS nil]
                (SETQ ANS (doall (map #'ANSQUEST (or (AND? RSS) (OR? RSS)))))
                (RETURN (ANSBUILD
                    (reduce #'+ (map #'PLAUSIBILITY? ANS))
                    (concat
                        (and (not (ISQ (PARSENODE? RSS) COMPONENT)) '((SAY YOU'RE TRYING TO CONFUSE ME\.)))
                        (doall (map #'(lambda (QUEST)
                            (concat '((TERPRI))
                                (ANSAY (ELIZA (FROM (NB (PARSENODE? (ANSRSS? QUEST))) (N (PARSENODE? (ANSRSS? QUEST))))))
                                '((PRINC '?) (TERPRI))
                                ;; CONJOINED QUESTIONS ARE HANDLED BY SIMPLY REPEATING EACH PART AND ANSWERING IT SEPARATELY.
                                (ACTION? QUEST)))
                        ANS)))
                    nil))))
        ((REL? RSS) (ANSREL RSS))
        (:else (ANSNOREL RSS))))

(§ DEFUN ANSREL [RSS]
    ;; ANSREL HANDLES ALL QUESTIONS WITH A RELATIVE NG OF ANY TYPE.
    (let [TYPE nil REL nil CODE nil PLAUS nil ANS nil PHRASE nil LENGTH nil NUM nil]
        (or (SETQ REL (REL? RSS)) (BUG ANSREL -- NO REL))
        (SETQ PHRASE (cons 'nil (HEADPART (PARSENODE? REL))))
        ;; THIS IS FOR THE PART OF THE GENERATOR THAT WILL SUBSITUTE "ONE" FOR NOUN NAMES.
        ;; THE LEADING NIL IS TO MAKE THIS PHRASE COMPATIBLE WITH THE "SAY" PHRASES WHICH THE OTHER PARTS GENERATE.
        ;; UNIVERSALS ARE CONVERTED TO NOT THERE EXISTS NOT.
        (SETQ TYPE (or (QTYPE? REL) (QUANTIFIER? REL) (BUG ANSREL -- NO TYPE)))
        (and (EQ TYPE 'ALL) (PUTPROP RSS true 'NEGATIVE=))
        (PUTPROP RSS
            (SETQ CODE
                (PLNR-FINDIFY 'ALL (VARIABLE? REL) (list (VARIABLE? REL)) (PLNR-DESCRIBE (cons RSS (RELATIONS? REL)) (VARIABLE? REL) (list (VARIABLE? REL)))))
            'PLNRCODE=)
        ;; CONSING THE RSS ONTO THE THINGS TO BE DESCRIBED HAS THE EFFECT OF PUTTING THE RELATION INTO THE DESCRIPTION OF THE OBJECT.
        ;; DISAMB PUTS IN THE JUNK IF THERE IS NO AMBIGUIT, AVOIDING HAVING TO GO THROUGH THE EVALUATION A SECOND TIME.
        ;; THVAL-MULT RETURNS A LIST OF A PLAUSIBILITY AND AN ANSWER.
        (SETQ ANS (THVAL-MULT (AMBPUT CODE)))
        (SETQ PLAUS (car ANS))
        (SETQ LENGTH (count (SETQ ANS (cadr ANS))))
        (RETURN (COND
            ((EQ TYPE 'ALL)
                (ANSBUILD
                    (+ PLAUS (PLAUSIBILITY? RSS))
                    (COND ((nil? ANS) '((SAY YES))) ((cons '(SAY NO, NOT) (PREPPUT (NAMELIST PHRASE 'INDEF ANS)))))
                    true))
            ((EQ TYPE 'HOWMANY)
                (ANSBUILD
                    (+ PLAUS (PLAUSIBILITY? RSS))
                    (PREPPUT (NAMESUGAR LENGTH REL))
                    true))
            ((MEMQ TYPE '(WHICH WHAT))
                (ANSBUILD
                    (+ PLAUS (PLAUSIBILITY? RSS) (COND (ANS 512) (0)))
                    (PREPPUT (NAMELIST PHRASE 'DEF ANS))
                    true))
            ((EQ TYPE 'INDEF)
                (SETQ NUM (NUMBER? REL))
                (ANSBUILD
                    (+ PLAUS (PLAUSIBILITY? RSS))
                    (COND
                        ((MEMQ NUM '(NS SG-PL))
                            (COND
                                ((nil? ANS) (COND ((ISTENSE (PARSENODE? RSS) 'MODAL) '((SAY I DON'T KNOW))) (:else '((SAY NO)))))
                                (:else (concat '((SAY YES,)) (COND
                                    ((ISTENSE (PARSENODE? RSS) 'MODAL) nil)
                                    ((PREPPUT (concat (and (cdr ANS) (concat (NAMESUGAR LENGTH REL) '((PRINC \:)))) (NAMELIST PHRASE 'INDEF ANS)))))))))
                        ((number? NUM)
                            ;; THIS IS THE CASE WHERE WE ARE CAGEY AND AVOID ANSWERING YES OR NO.
                            ;; THE NUMBER ISN'T REPEATED IF IT IS THE SAME AS THE NUMBER IN THE SPECIFICATION.
                            (concat
                                (COND
                                    ((EQ NUM LENGTH) '((SAY YES,)))
                                    ((> LENGTH NUM) nil)
                                    ((zero? NUM) '((SAY NO,)))
                                    (:else '((SAY NO, ONLY))))
                                (COND
                                    ((EQ NUM LENGTH) nil)
                                    (:else (PREPPUT (concat (NAMESUGAR LENGTH REL) '((PRINC \:))))))
                                (PREPPUT (NAMELIST PHRASE 'INDEF ANS))))
                        ((EQ (car NUM) 'EXACTLY)
                            (COND ((EQ LENGTH NUM) '((SAY YES)))
                                (:else (cons '(SAY NO,) (PREPPUT (NAMESUGAR LENGTH RES))))))
                        ((EQ (car NUM) '>)
                            (cons (COND
                                    ((> LENGTH NUM) '(SAY YES,))
                                    ((zero? LENGTH) '(SAY NO,))
                                    (:else '(SAY NO, ONLY)))
                                (PREPPUT (NAMESUGAR LENGTH REL))))
                        ((EQ (car NUM) '<)
                            (cons (COND
                                    ((< LENGTH NUM) '(SAY YES,))
                                    (:else '(SAY NO,)))
                                (PREPPUT (NAMESUGAR LENGTH REL))))
                        ((ERT ANSREL -- FUNNY NUMBER)))
                    true))
            ((ERT ANSREL-- FUNNY TYPE))))))

(§ DEFUN ANSTHM [EXP]
    ;; GENRATES A THEOREM OR ASSERTION FOR AN EXPRESSION.
    (let [NEG nil VARLIST nil BODY nil]
        (COND
            ;; NOTELL MARKS THAT THIS ISN'T THE KIND OF ASSERTION IT CAN HANDLE.
            ;; IT USES GLOBAL-ERR VAR AND NEG ARE SET AS FREE VARIABLES BY ANSTHMELEMENT WHICH ANALYZES EACH ELEMENT.
            ;; IF THERE ARE NO VARS, IT IS A SIMPLE ASSERTION.
            ((ATOM EXP) (NOTELL))
            ((not (GET (car EXP) 'TELLABLE)) (NOTELL))
            (:else
                (SETQ NEG (NEGATIVE? RSS))
                (SETQ EXP (doall (map #'ANSTHMELEMENT (PLNR-REMTIME EXP))))
                (RETURN (COND
                    ((not (or VARLIST NEG)) EXP)
                    (:else
                        (PLNR-THCONSIFY VARLIST EXP (COND (NEG (PLNR-PROGIFY nil (list BODY '(THFAIL THEOREM)))) (:else BODY))))))))
        nil))

(§ DEFUN ANSTHMADD [OSS]
    (SETQ VARLIST (cons (VARIABLE? OSS) VARLIST))
    (SETQ BODY (COND
        (BODY (PLNR-PROGIFY nil (list BODY (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (list (VARIABLE? OSS))))))
        (:else (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (list (VARIABLE? OSS))))))
    (PLNR-VAR (VARIABLE? OSS)))

(§ DEFUN ANSTHMELEMENT [X]
    (COND ((not (ATOM X)) X)
        ((TSS? X) (NOTELL))
        ((RSS? X) (NOTELL))
        ((not (OSS? X)) X)
        ((REFER? X) (ATOMIFY (REFER? X)))
        ((EQ (QUANTIFIER? X) 'ALL) (COND (NEG (NOTELL)) (:else (ANSTHMADD X))))
        ((EQ (QUANTIFIER? X) 'NO) (SETQ NEG true) (ANSTHMADD X))
        ((EQ (QUANTIFIER? X) 'NDET) (ANSTHMADD X))
        ((not (EQ (QUANTIFIER? X) 'INDEF)) (NOTELL))
        ((ISQ (PARSENODE? X) ANY) (ANSTHMADD X))
        (:else (GLOBAL-ERR YOU HAVE TO TELL ME WHICH))))

(§ DEFUN ANSUNIQUE [LIST]
    ;; THIS FUNCTION SHOULD ELIMINATE ANSWERS WHICH GIVE THE SAME
    ;; RESULT EVEN THHOUGH THEY INVOLVE DIFFERENT INTERPRETATIONS.
    ;; IT NEEDS TO CHECK FOR SIGNIFICANT DIFFERENCES, E.G. IN WHAT
    ;; GETS PRINTED OR DONE, WHILE IGNORING INSIGNIFICANT ONES,
    ;; E.G. THE NAMES OF ATOMS TO WHICH THINGS ARE ATTACHED.
    ;; FOR THE MOMENT, IT JUST RETURNS THE LIST UNTOUCHED.
    LIST)

        ;; FROM BOTH THE INPUT SENTENCE AND THE ANSWER.

(§ SETQ ANS-TEST? nil)

(§ DEFUN ATOMIFY [X] (COND ((ATOM X) X) ((cdr X) X) ((car X))))

(§ DEFUN CUTOFF [X]
    ;; FOR CUTTING # OFF OF CONCEPT NAMES TO GET ENGLISH WORDS.
    (READLIST (cdr (EXPLODE X))))

(§ DEFUN DESCRIBEVENT [EVENT TYPE]
    (let [ANS nil]
        (SETQ EVENT (car EVENT))
        (RETURN (COND
            ((EQ TYPE 'WHERE)
                (GLOBAL-ERR I CAN'T ANSWER "\"WHERE\"" QUESTIONS YET)) ;; "sic!
            ((EQ TYPE 'WHY)
                (COND ((EQ (GET EVENT 'WHY) 'COMMAND) '((SAY BECAUSE YOU TOLD ME TO)))
                    (:else (cons '(SAY TO) (NAMEACTION 'INFINITIVE (GET EVENT 'WHY))))))
            ((EQ TYPE 'HOW)
                (dorun (map #'(lambda (X) (and (EQ (GET X 'WHY) EVENT) (SETQ ANS (cons X ANS)))) EVENTLIST))
                (COND
                    ((nil? ANS) '((SAY I CAN'T ANALYZE HOW I DID IT)))
                    (:else (concat '((SAY BY)) (NAMEACTION 'ING (car ANS)) (doall (map #'(lambda (X) (cons '(PRINC '\;) (CONS '(SAY THEN) (NAMEACTION 'ING X)))) (CDR ANS)))))))
            ((or (EQ TYPE 'POLAR) (EQ TYPE 'WHEN))
                (COND
                    ((EQ (GET EVENT 'WHY) 'COMMAND)
                        (COND
                            ((EQ EVENT (TOPLEVEL (car EVENTLIST))) '((SAY JUST NOW)))
                            (:else (cons '(SAY BEFORE) (NAMEACTION 'PAST (TOPLEVEL (car (FINDB EVENT EVENTLIST))))))))
                    (:else (cons '(SAY WHILE) (NAMEACTION 'PRES-PAST (TOPLEVEL EVENT))))))
            ((BUG DESCRIBEVENT -- FUNNY TYPE))))))

(§ DEFUN DISPUT [ASSERTION]
    ;; PUT THE SENTENCE NUMBER ON THE ASSERTION AS A WHO PROPERTY
    (or (not DISCOURSE) (PUTPROP ASSERTION SENTNO 'WHO)))

(§ DEFUN ELIZA [NODE]
    ;; DOES THE OBVIOUS THING.
    (let [XX nil NUM nil]
        (SETQ NUM (count (N NODE)))
        (RETURN
            (APPLY #'concat
                (MAPLIST #'(lambda (WORD)
                    (COND
                        ((not (< NUM (count WORD))) nil)            ;; THIS KLUDGE STOPS IT AT THE END OF THE NODE
                        ((SETQ XX (ASSQ (car WORD) '((I YOU) (ME YOU) (AM ARE) (ARE AM))))
                            (cdr XX))                               ;; WE RETURN LIST OF THE THING REALLY WANTED, SO
                        ((EQ (car WORD) 'YOU)                       ;; THE APPLY APPEND CAN GET RID OF THE EMPTY ONES.
                            (SETQ XX (FINDMOTHER WORD NODE))        ;; UNFORTUNATELY, FOR "YOU" IT IS NECESSARY TO
                            (COND ((ISQ XX SUBJ) '(I))              ;; DECIDE WHETHER IT SHOULD BE REPLACED BY "I" OR
                                ((ISQ XX OBJ) '(YOU))               ;; "ME", ACCORDING TO WHETHER IT WAS PARSED AS AN
                                ((BUG ELIZA -- SUBJ OBJ))))         ;; OBJECT OR SUBJECT. FINDMOTHER IS USED TO FIND
                        ((list (car WORD)))))                       ;; THE PARSE NODE. WORDS OTHER THAN THE SPECIAL
                    (NB NODE))))))                                  ;; ONES GO THROUGH DIRECTLY.

(§ DEFUN ENOUGH-BETTER [ANS1 ANS2]
    (> (PLAUSIBILITY? ANS1) (+ (PLAUSIBILITY? ANS2) TIMID)))

(§ DEFUN FINDMOTHER [WORD NODE]
    ;; FINDMOTHER TAKES A PLACE IN THE SENTENCE AND A GRAMMAR NODE
    ;; (BOTH ARE ACTUALLY LISTS) AND FINDS THE SINGLE-WORD
    ;; CONSTITUTENT BEGINNING AT THAT PLACE IN THE SENTENCE.
    (COND ((and (EQ WORD (NB NODE)) (EQ (cdr WORD) (N NODE))) NODE)
        (:else (APPLY #'concat (MAPLIST #'(lambda (NOD) (FINDMOTHER WORD NOD)) (H NODE))))))

(§ DEFUN HEADPART [NODE]
    ;; EVERYTHING UP TO THE NOUN, FOR EXAMPLE "THE RED BLOCK" IN "THE RED BLOCK WHICH ..."
    ;; NOTE THAT NODE IS ACTUALLY A LIST OF NODE (A PROPER GRAMMAR POINTER).
    (and (SETQ PT NODE) (MOVE-PT DLC PV (NOUN)) (FROM (NB NODE) (N PT))))

(§ DEFUN LISTNAMES [PHRASE SPEC NAMES]
    ;; PHRASE IS THE INITIAL THING TO COMPARE FOR USING "ONE", SPEC IS EITHER DEF OR INDEF, AND THE NAMES ARE OF DATA-BASE OBJECTS.
    ;; LISTNAMES PUTS OUT AN ACTION LIST, AS WELL AS PUTTING THINGS ONTO THE BACKREF.
    ;; IT IS CALLED AFTER THE ANSWER HAS BEEN DECIDED ON.
    (let [COUNT nil EXAM nil X nil RES nil ANS nil COMMA? nil]
        ;; NAMEOBJ RETURNS A LIST OF THE OBJECT AND THE ...
        ;; THIS PATCH MAY WELL BE TOTAL OUT OF PHASE WITH THE BACKREF HACKER - DDM 5-12-73 INSTRUCTIONS FOR NAMING IT.
        (SETQ NAMES (doall (map #'(lambda (X) (NAMEOBJ X SPEC)) NAMES)))
        (COND ((nil? NAMES) (RETURN '(SAY NOTHING))))
    UP  (SETQ COUNT 1)
        (SETQ EXAM (car NAMES))
        (SETQ NAMES (cdr NAMES))
    BACK (COND ((SETQ X (ASSOC (car EXAM) NAMES))
            (SETQ NAMES (DELQ X NAMES))
            (SETQ COUNT (inc COUNT))
            (SETQ EXAM (list (car EXAM) (concat (cadr X) (cadr EXAM))))
            (GO BACK)))
        ;; WHEN THERE ARE TWO OBJECTS WITH THE SAME ENGLISH DESCRIPTIONS, A JOINT OBJECT IS PRODUCED COMBINING THE OBJECTS.
        ;; THE COUNT IS LATER USED TO PUT IN THE APPROPRIATE NUMBER, AND THE DESCRIPTION IS CHECKED TO SEE IF "ONE" CAN BE USED.
        ;; ADD THE ONE JUST PRODUCED TO THE RESULT LIST.  TRY ANOTHER.
        (SETQ RES (cons (cons (PLURALIZE (car EXAM) COUNT) (cdr EXAM)) RES))
        (and NAMES (GO UP))
        (SETQ RES
            (doall (map #'(lambda (PHRASE2)
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

(§ DEFUN NAMEACTION [TENSE EVENT]
    ;; THIS FUNCTION SETS UP A LIST OF S-EXPRESSIONS WHICH ARE RETURNED TO DESCRIBEVENT AND WHICH
    ;; WHEN EVALUATED WILL PRINT OUT AN ENGLISH DESCRIPTION OF THE SINGLE, SIMPLE EVENT IMBEDDED
    ;; IN THE LIST "THASSERTION" WITH THE TENSE SPECIFIED.
    (let [PLNR-FORM nil VERB nil OBJ1 nil OBJ2 nil]
        ;; THE THASSERTION PROPERTY IS A LIST THAT TYPICALLY LOOKS LIKE "(NIL (2 (3 1 ((!GRASP :E2 :B6)))))"
        (SETQ PLNR-FORM (car (caddr (cadadr (GET EVENT 'THASSERTION))))
            VERB (CUTOFF (car PLNR-FORM))
            OBJ1 (caddr PLNR-FORM)
            OBJ2 (cadddr PLNR-FORM))
        (SETQ FOOBAR (COND
            ((EQ VERB 'CLEARTOP)
                ;; SAYIFY WRAPS THE FUNCTION "SAY" ARROUND A LIST OF WORDS AND RETURNS THE RESULTING S-EXPRESSION.
                (cons (SAYIFY (VBFIX 'CLEAN nil)) (PRON-PRT 'OFF OBJ1)))
            ((EQ VERB 'GET-RID-OF)
                ;; NAMELIST-EVALED '(NIL) 'DEF RETURNS A LIST (!!!) OF S-EXPRESSIONS.
                (cons (SAYIFY (VBFIX 'GET true) 'RID 'OF) (NAMELIST-EVALED '(nil) 'DEF OBJ1)))
            ((EQ VERB 'GRASP)
                (cons (SAYIFY (VBFIX 'GRASP true)) (NAMELIST-EVALED '(nil) 'DEF OBJ1)))
            ((EQ VERB 'PICKUP)
                (cons (SAYIFY (VBFIX 'PUT true)) (PRON-PRT 'UP OBJ1)))
            ((EQ VERB 'PUTON)
                (concat (cons (SAYIFY (VBFIX 'PUT true)) (NAMELIST-EVALED '(nil) 'DEF OBJ1)) (cons '(SAY ON) (NAMELIST-EVALED '(nil) 'DEF OBJ2))))
            ((EQ VERB 'STACKUP)
                (cons (VBFIX STACK true) (PRON-PRT 'UP OBJ1)))
            ((EQ VERB 'RAISEHAND) nil)
            (:else (BUG NAMEACTION - I DON'T KNOW WHAT TO DO WITH THE VERB I GOT))))
        (RETURN FOOBAR)))

(§ DEFUN NAMELIST [ONE SPEC LISTX]
    ;; GENERATES A LIST OF EXPRESSIONS TO BE EVALUATED WHICH WILL CAUSE THE APPROPRIATE NAMELIST TO BE PRINTED OUT.
    ;; THE ARGUMENTS ARE JUST THOSE TO LISTNAMES.
    (list (list 'EVLIS (list 'LISTNAMES (QUOTIFY ONE) (QUOTIFY SPEC) (QUOTIFY LISTX)))))
    ;; A TYPICAL CALL WOULD RESULT IN A VALUE OF ((EVLIS (LISTNAMES '(A RED BLOCK) 'INDEF '(:B1 :B7)))) WHICH WOULD BE EVALUATED LATER.
    ;; NOTE THAT LISTNAMES WILL IN TURN PRODUCE A LIST OF EXPRESSIONS TO BE EVALUATED, WHICH WILL BE CAUGHT BY THE EVLIS.  CONFUSING?

(§ DEFUN NAMELIST-EVALED [ONE SPEC LISTX]
    (let [F nil]
        (SETQ F (list 'LISTNAMES
            (QUOTIFY ONE)
            (QUOTIFY SPEC)
            (QUOTIFY LISTX)))
        (RETURN (list (eval F)))))

(§ DEFUN NAMENUM [X]
    ;; GENERATES NUMBER NAMES.
    (or (NTH (inc X) '(NONE ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN))
        (GLOBAL-ERR I CAN'T COUNT THAT HIGH)))

(§ DEFUN NAMEOBJ [ITEM SPEC]
    ;; NAMES THE OBJECT IN ENGLISH -- GENERATES LIST OF THINGS TO BE EVALUATED.  SPEC IS EITHER 'INDEF OR 'DEF
    (let [TYPEß nil TYPELIST nil TYPE nil NAMEß nil COLORß nil COLORLIST nil SIZEß nil SIZELIST nil CUBE nil NAME nil X nil]
        (and (SETQ X (ASSOC ITEM '((:SHRDLU I) (:FRIEND YOU))))
            (RETURN (list (ANSAY (cdr X)) (list ITEM))))                        ;;  SPECIAL CASE CHECK
        (THVAL2 nil '(THGOAL (!NAMEOBJ) (THUSE TC-NAMEOBJ)))
        (or TYPELIST
            (ERT NAMEOBJ -- OBJECT WITH NO !IS ASSERTION))
        ;; DISPUT CHECKS TO SEE IF DISCOURSE IS BEING KEPT, AND IF SO PUTS THE RELEVANT SENTENCE NUMBER AS A PROPERTY ON THE ASSERTION.
        (DISPUT TYPEß)
        (COND ((EQ (SETQ TYPE (caddar TYPEß)) '!NAME)                           ;; A NAME IS ITS OWN NAME.
                (RETURN (list (ANSAY (list ITEM)) (list ITEM))))
            ((MEMQ '!PROPERTY (GET TYPE 'SYS))
                ;; CUTOFF CUTS THE # OFF OF NAMES LIKE !RED AND !POINTED WHICH ARE USED FOR PROPERTIES.
                (RETURN (list (ANSAY (list (CUTOFF ITEM))) (list ITEM))))
            ((not (cdr TYPELIST))
                (RETURN (list (ANSAY (list 'THE (CUTOFF TYPE))) (list ITEM))))  ;; THERE IS ONLY ONE OBJECT OF THIS TYPE (E.G. TABLE, BOX, HAND)
            (CUBE (SETQ NAME '(CUBE)))
            ((SETQ NAME (list (CUTOFF TYPE)))))                                 ;; E.G. !BLOCK BECOMES BLOCK.
        (and NAMEß
            (RETURN (list (ANSAY (list 'THE (car NAME) 'NAMED (caddar NAMEß))) (list ITEM)))) ;; E.G. THE BLOCK NAMED SUPERBLOCK.
        (DISPUT COLORß)                                                         ;; IF WE HAVEN'T RETURNED YET, COLOR WILL BE NEEDED TO FULLY DESCRIBE THE OBJECT.
        (SETQ NAME (cons (CUTOFF (caddar COLORß)) NAME))
        (or (cdr COLORLIST)
            (RETURN (list (ANSAY (cons 'THE NAME)) (list ITEM))))               ;; THERE ARE NO OTHERS OF THE SAME COLOR.  IF THERE ARE, WE MUST USE SIZE AS WELL.
        (SETQ NAME (cons SIZEß NAME))
        (RETURN (list (COND
            ((nil? (cdr SIZELIST))
                (ANSAY (cons 'THE NAME)))                                       ;; THE SIZE MANAGES TO FINISH SPECIFYING IT.
            ((EQ SPEC 'INDEF)
                (ANSAY (cons 'A NAME)))                                         ;; IN THE INDEFINITE CASE WE DON'T CARE IF THIS ISN'T A FULL SPECIFICATION.
            ((SETQ X (THVAL2 nil '(THFIND ALL $?X (X (Y ITEM)) ($G (!SUPPORT $?Y $?X)))))
                (cons (concat '(SAY THE) NAME)
                    (cons '(SAY WHICH SUPPORTS)
                        (LISTNAMES nil 'INDEF X))))                             ;; IF IT SUPPORTS ANYTHING, NAME THEM.
            ((cons (concat '(SAY THE) NAME)
                (cons '(SAY WHICH IS TO THE RIGHT OF)
                    (COND ((SETQ X (THVAL2 nil '(THFIND ALL $?X (X (Y ITEM))
                                ($G (!AT $?X ?)) ($G (!LOC !RIGHT $?Y $?X) (THUSE TC-LOC))))) ;; MAKE SURE IT IS AN ITEM WITH A LOCATION.
                            (LISTNAMES nil 'INDEF X))
                        ('((SAY NOTHING))))))))
            (list ITEM)))))

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
    (THCONSE ((X ITEM) TYPE COLOR NAME SIZE Y Z)
        (!NAMEOBJ)
        ($G (!IS $?X $?TYPE))
        (SETQ TYPEß THVALUE)
        (or (SETQ CUBE (and (EQ $?TYPE '!BLOCK) (!EQDIM $?X)))
            true)
        (THCOND
            (($G (!NAME $?X $?NAME))
                (SETQ NAME THVALUE))
            (($G (!IS $?Y $?TYPE))
                (or (not CUBE) (!EQDIM $?Y))
                (SETQ TYPELIST (cons $?Y TYPELIST))
                ($G (!COLOR $?X $?COLOR))
                (SETQ COLORß THVALUE)
                ($G (!COLOR $?Y $?COLOR))
                (SETQ COLORLIST (cons $?Y COLORLIST))
                (SETQ SIZEß (NAMESIZE (SIZE $?X)))
                (EQ SIZEß (NAMESIZE (SIZE $?Y)))
                (SETQ SIZELIST (cons $?Y SIZELIST))
                (THFAIL))))
    THEOREM)

(§ DEFUN NAMESIZE [X]
    ;; ACCEPTS EITHER SINGLE NUMBER OR LIST OF DIMENSIONS.
    (or (number? X) (SETQ X (reduce #'+ X)))
    (COND ((> X 383) 'LARGE) (:else 'SMALL)))

(§ DEFUN NAMESUGAR [NUM OSS]
    ;; GENERATES PHRASES LIKE "THREE OF THEM".
    (let [VAGUE nil]
        ;; VAGUE IS FOR WORDS LIKE "ANYTHING", "SOMETHING", "NOTHING" TO AVOID SAYING "OF THEM" WHEN IT ISN'T APPROPRIATE.
        (SETQ VAGUE (MEMQ '!VAGUE (MARKERS? OSS)))
        (RETURN (list (cons 'SAY
            (COND ((and VAGUE (zero? NUM)) '(NOTHING))
                ((cons (NAMENUM NUM)
                    (COND (VAGUE (COND ((== NUM 1) '(THING)) ('(THINGS))))
                        ('(OF THEM)))))))))))

(§ DEFUN NOTELL []
    (GLOBAL-ERR THAT ISN'T THE KIND OF THING I CAN BE TOLD))

(§ DEFUN ONECHECK [ITEM]
    ;; CHECKS TO SEE IF A SUBSTITUTE "ONE" CAN BE USED.
    ;; ITEM IS A SINGLE "SAY" PHRASE.
    ;; "PHRASE" IS A FREE VARIABLE IN LISTNAMES.
    (let [ANS nil OLD nil NEW nil]
        (and (EQUAL PHRASE '(nil))
            (SETQ PHRASE (car ITEM))
            (RETURN ITEM))
        (SETQ OLD (reverse PHRASE))
        (SETQ NEW (reverse (car ITEM)))
        (or (EQ (car OLD) (car NEW))
            (EQ (car OLD) (GET (car NEW) 'ROOT))
            (EQ (car NEW) (GET (car OLD) 'ROOT))
            ;; IF THE NOUNS DON'T MATCH, JUST RETURN WHAT YOU GOT.
            ;; MATCHING INCLUDES PLURALS TO THEIR CORRESPONDING SINGULAR FORMS.
            (RETURN ITEM))
    LOOP (SETQ NEW (cdr NEW))
        (SETQ OLD (cdr OLD))
        (COND ((or (nil? NEW) (nil? OLD) (ISQ NEW NUM) (ISQ NEW DET) (not (EQ (car NEW) (car OLD))))
            (RETURN (cons
                (reverse (cons (COND ((ISQ (LAST (car ITEM)) NPL) 'ONES) (:else 'ONE)) NEW))
                (cdr ITEM)))))
        (GO LOOP)))

(§ DEFUN ORDNAME [NUM]
    ;; NAME AN ORDINAL
    (COND ((== NUM 1) 'ONCE) ((== NUM 2) 'TWICE)
        ((READLIST (concat (EXPLODE (NAMENUM NUM)) '(\space T I M E S))))))

(§ DEFLIST PAST (PUT PUT))

(§ DEFUN PLNR-ANDORIFY [RSS]
    ;; TURNS AN RSS INTO A COLLECTION OF PLANNER CODE FOR A COMMAND
    (COND ((AND? RSS)
            (PLNR-PROGIFY nil (doall (map #'PLNR-ANDORIFY (AND? RSS)))))
        ((OR? RSS)
            ;; (PLNR-ORIFY NIL (MAPCAR 'PLNR-ANDORIFY (OR? RSS)))
            (ERT SORRY, PLNR-ORIFY NOT WRITTEN))
        ((PLNR-PROGIFY nil (doall (map #'PLNR-GOALIFY (RELATIONS? RSS)))))))

(§ DEFUN PREPPUT [X]
    (COND ((and (REL? RSS) (SETQ PT (PARSENODE? (REL? RSS))) (ISQ (MOVE-PT U) PREPG))
            (cons (cons 'SAY (FROM (NB PT) (NB (MOVE-PT DLC)))) X))
        (:else X)))

(§ DEFUN PLURALIZE [ITEM NUM]
    ;; CONVERTS A SINGULAR NOUNPHRASE OR "ONCE" STATEMENT INTO PLURAL.
    (COND ((> 2 NUM) ITEM)
        (:else (COND ((MEMQ 'A (car ITEM))
                (cons (PLURALMAKE (SUBST (NAMENUM NUM) 'A (car ITEM))) (cdr ITEM)))
            ((MEMQ 'ONCE (car ITEM))
                (cons (SUBST (ORDNAME NUM) 'ONCE (car ITEM)) (cdr ITEM)))
            ((BUG PLURALIZE -- FUNNY ITEM))))))

(§ DEFUN PLURALMAKE [PHRASE]
    ;; CONVERTS SINGULAR PHRASE TO PLURAL.
    (let [SING nil PLURAL nil]
        (or (ISQ (SETQ SING (LAST PHRASE)) NOUN)
            (BUG PLURALMAKE -- NO NOUN))
        (SETQ PLURAL (MAKNAM (concat (EXPLODE (car SING)) '(S))))
        (or (GET PLURAL 'FEATURES)
            (BUILDWORD PLURAL '(NOUN NPL) (SM SING) (car SING)))
        (RETURN (SUBST PLURAL (car SING) PHRASE))))

(§ DEFUN PRON-PRT [PARTICLE NG]
    ;; THIS IS EVENTUALLY SUPPOSED TO BE THE PLACE FOR THE PRONOUN-PARTICLE-INTERACTION MAGIC
    ;; TO HAPPEN, IE. "CLEAR OFF THE BLOCK." VS. "CLEAR IT OFF" SINCE "CLEAR OFF IT." IS
    ;; UNGRAMMATICAL AND "CLEAR THE BLOCK OFF." WOULD NOT BE APPROPRIATE IN CASES OF HEAVY-NP'S.
    ;;
    ;; AT THE MOMENT, FOR SIMPLICITY'S SAKE, I'VE IGNORED THE
    ;; PROBLEM AND THE PARTICLE IS ALWAYS PUT BEFORE THE NG.
    (cons (list 'SAY PARTICLE)
        (NAMELIST-EVALED '(nil) 'DEF NG)))

(§ DEFUN SAYIFY FEXPR [EXP-LIST]
    (cons 'SAY (doall (map #'(lambda (Y) (eval Y)) EXP-LIST))))

(§ DEFUN THVAL-MULT [CODE]
    ;; DOES A THVAL WITH DIFFERENT VALUES OF WHO (I.E. NIL (EVERYTHING I KNOW),
    ;; 'HE (EVERYTHING HE KNOWS), AND THE PREVIOUS SENTENCE) USED TO TELL IF AN
    ;; ANSWER COULD HAVE BEEN GENERATED WITH HIS KNOWLEDGE TO SEE WHETHER HE REALLY
    ;; MEANT THIS INTERPRETATION.  RETURNS A LIST OF A PLAUSIBILITY AND THE RESULT
    ;; OF THE THVAL USING ALL THE KNOWLEDGE IN THE DATA BASE.
    (let [ANS nil]
        (SETQ ANS (THVAL2 nil CODE))
        ;; THIS FEATURE IS ONLY RELEVANT IN DISCOURSE AND WHEN THERE ARE AMBIGUITIES.
        (or (and AMBIG DISCOURSE) (RETURN (list 0 ANS)))
        ;; GIVE A VALUE OF 256 IF HE COULDN'T HAVE ANSWERED IT AT ALL.
        (or (EQUAL ANS (THVAL2 'HE CODE)) (RETURN (list 256 ANS)))
        ;; PLAUSIBILITY IS 0 IF HE COULD HAVE ANSWERED IT WITH RECENTLY MENTIONED INFORMATION.
        ;; 128 IF HE COULD ANSWER IT BUT NOT WITH RECENT INFO.
        (RETURN (COND ((EQUAL ANS (THVAL2 (list (*DIF SENTNO 2) (inc SENTNO)) CODE)) (list 0 ANS)) ((list 128 ANS))))))

(§ DEFUN TOPLEVEL [EVENT]
    ;; FINDS THE TOP LEVEL EVENT GOING ON AT THE TIME
    (COND ((EQ (GET EVENT 'WHY) 'COMMAND) EVENT)
        (:else (TOPLEVEL (GET EVENT 'WHY)))))

(§ DEFUN FINDCHOOSE [OSS X ANS2]
    (let [HAVE nil NEED nil XX nil ANS nil PLNRCODE nil LOOP nil]
        (and (REFER? OSS) (RETURN (ATOMIFY (REFER? OSS))))
        (COND
            ((AND? OSS)
                (RETURN (MAPBLAND #'(lambda (OSS)
                    (let [Y nil]
                        (SETQ Y (FINDCHOOSE OSS X ANS2))
                        (SETQ ANS2 (concat Y ANS2))
                        (RETURN Y)))
                    (AND? OSS))))
            ((OR? OSS)
                (SETQ LOOP (OR? OSS))
                (RETURN (let [Y nil]
                GO  (COND
                    ((SETQ Y (FINDCHOOSE (car LOOP) X ANS2)) (RETURN Y))
                    ((SETQ LOOP (cdr LOOP)) (GO GO)))))))
        (SETQ PLNRCODE (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (list (VARIABLE? OSS))))
        (PUTPROP OSS PLNRCODE 'PLNRCODE=)
        (COND
            ((EQ (QUANTIFIER? OSS) 'ALL)
                (RETURN (ATOMIFY (THVAL (PLNR-FINDIFY 'ALL (VARIABLE? OSS) (list (VARIABLE? OSS)) PLNRCODE) nil))))
            ((or (AND? OSS) (OR? OSS)) (GO CONJ)))
        (or (ATOM (SETQ NEED (NUMBER? OSS)))
            (SETQ NEED (cadr NEED)))
        (and (EQ NEED 'NS) (SETQ NEED 1))
        (SETQ HAVE 0)
    GO  (COND
            ((or (EQ HAVE NEED)
                (and (> HAVE NEED)
                    (SETQ ANS (FINDREDUCE ANS (- HAVE NEED)))))
                (GO DONE))
            ((EQ X 'NOMORE) (RETURN nil))
            ((SETQ HAVE (count
                (SETQ ANS (concat
                    (THVAL (PLNR-FINDIFY
                        (list 1 (- NEED HAVE) true)
                        (VARIABLE? OSS)
                        (list (VARIABLE? OSS))
                        (PLNR-PROGIFY nil
                            (concat (list PLNRCODE)
                                (SUBST (VARIABLE? OSS) '*** '((not (or (MEMQ (THV ***) ANS) (MEMQ (THV ***) ANS2)))))
                                (and X (SUBST (VARIABLE? OSS) '* (car X))))))
                        THALIST)
                    ANS))))
            (SETQ X (COND (X (cdr X)) ('NOMORE)))
            (GO GO)))
    CONJ (SETQ LOOP (or (AND? RSS) (OR? RSS)))
    UP  (COND ((GET (car LOOP) 'REFER)
                (SETQ ANS (concat (GET (car LOOP) 'REFER) ANS)))
            ((SETQ XX (FINDCHOOSE (car LOOP) X (concat ANS2 ANS)))
                (SETQ ANS (concat XX ANS))))
        (COND ((and ANS (OR? OSS)))
            ((SETQ LOOP (cdr LOOP)) (GO UP))
            (ANS)
            ((RETURN nil)))
    DONE (and (ATOM (VARIABLE? OSS))
            (PUTPROP (VARIABLE? OSS) (reverse ANS) 'BIND))
        (RETURN (ATOMIFY (reverse ANS)))))

(§ DEFUN FINDNUM [X]
    (COND ((number? X) X)
        ((EQ (car X) 'EXACTLY) (list (cadr X) (inc (cadr X)) nil))
        ((EQ (car X) '>) (inc (cadr X)))
        ((EQ (car X) '<) (cadr X))
        ((EQ X 'NS) 1)
        ((EQ X 'NPL) 2)
        ((ERT FINDNUM))))

(§ DEFUN FINDREDUCE [X Y]
=>  (SETQ X (cdr X))
    (COND ((zero? (SETQ Y (dec Y))) (RETURN X)) ((GO =>))))

(§ DEFUN IASS [X]
    (let [XX nil]
        (or (SETQ XX (cadr (SASSQ X (cadr (cadddr ANS)) #'SASS)))
            (RETURN true))
        (SAY "
BY")         (PRINC (COND ((EQ X 'IT) "\"IT\"") ((MEMQ 'THEY (FROM SENT nil)) "\"THEY\"") ("\"THEM\"")))
        (SAY , I ASSUME YOU)
        (PRINC 'MEAN)
        (dorun (map #'PRINT2 (PARAP XX)))
        (RETURN (PRINC ".
"))))

(§ DEFUN MUNG [LIST MUNG]
    (SETQ MUNG (list 'quote MUNG))
    (and DISCOURSE (SETQ LIST (caddr LIST)))
    (COND ((EQ (caar (cdddr LIST)) 'THAMONG)
            (RPLACD (cdar (cddddr LIST)) MUNG))
        ((RPLACD (cdddr LIST) (cons (list 'THAMONG (list 'THV (cadr (caddr LIST))) MUNG) (cddddr LIST))))))

(§ DEFUN NAMEVENT [EVENT TYPE]
    (let [THALIST nil EV nil SUBJ nil OBJ1 nil OBJ2 nil]
        (or (SETQ EV (GET (GET EVENT 'TYPE) 'NAMEVENT))
            (ERT NAMEVENT))
        (or (THVAL (list 'THGOAL
            (COND
                ((EQ (car EV) 2) '(? $?EVENT))
                ((EQ (car EV) 3) '(? $?EVENT (THNV SUBJ)))
                ((EQ (car EV) 'I3) '(? $?EVENT (THNV OBJ1)))
                ((EQ (car EV) 4) '(? $?EVENT (THNV SUBJ) (THNV OBJ1)))
                ((EQ (car EV) 'I4) '(? $?EVENT (THNV OBJ1) (THNV OBJ2)))
                ((EQ (car EV) 5) '(? $?EVENT (THNV SUBJ) (THNV OBJ1) (THNV OBJ2)))
                ((ERT NAMEVENT DATA))))
            (SETQ THALIST (list
                (list 'EVENT EVENT)
                (list 'SUBJ (COND ((number? (car EV)) nil) ('I)))
                (list 'OBJ1 nil) (list 'OBJ2 nil))))
            (ERT NAMEVENT THVAL))
        (dorun (map #'(lambda (X)
                (and (cadr X) (SET (car X) (ERT "UNDEF-FN:" NAMES NAMES (LISTIFY (cadr X)) 'EV))))
            (cdr THALIST)))
        (SETQ ANSBACK2 (or ANSBACK true))
        (SETQ LASTANSEV EVENT)
        (RETURN (concat
            (COND ((EQ TYPE 'PAST) SUBJ) ((EQ TYPE 'TO) '(TO)))
            (eval (cadr EV))))))

(§ DEFUN PARAP [] (ERT YOU LOSE, PARAP IS FLUSHED UNTIL IT CAN BE FIGURED OUT))

(§ DEFUN PRTPUT [X Y] (COND ((cdr Y) (cons X Y)) ((concat Y (list X)))))

(§ DEFUN VBFIX [X PP]
    (COND
        ((EQ TENSE 'ING)
            (SETQ X (reverse (EXPLODE X)))
            (READLIST (reverse (concat '(G N I) (VBFIX2 X) X))))
        ((EQ TENSE 'PAST)
            (or (GET X 'PAST)
                (and (SETQ X (reverse (EXPLODE X)))
                    (READLIST (reverse (concat '(D E) (VBFIX2 X) X))))))
        ((EQ TENSE 'INFINITIVE) X)
        (:else (BUG VBFIX - WHAT DO I DO WITH THIS TENSE?))))

(§ DEFUN VBFIX2 [X] (and PP (MEMQ (car X) CONSO) (MEMQ (cadr X) VOWEL) (list (car X))))

#_(ns shrdlu.cgram)

;; #################################################################
;;
;;  CGRAM > THE REGULAR GRAMMAR AFTER GOING THROUGH THE PRECOMPILER
;;
;; #################################################################

(§ DEFUN CLAUSE []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil :RESULT nil POSITION-OF-PRTMVB nil LOCATIONMARKER nil SUBJ-VB-BACKUP-TYPE1 nil POSITION-OF-PTW nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-CLAUSE
        (and LABELTRACE (PASSING 'ENTERING-CLAUSE))
        (SETR 'TIME (BUILD TSSNODE= (MAKESYM 'TSS)) C)
        (SETQ :RESULT (CQ SIMP))
        (COND (:RESULT (GO SUBJ)))
        (SETQ :RESULT (CQ MAJOR))
        (COND (:RESULT (GO INIT)) (:else (GO SEC)))
INIT
        (and LABELTRACE (PASSING 'INIT))
        (SETQ LOCATIONMARKER N)
        (SETQ :RESULT (and (NQ BINDER) (PARSE CLAUSE BOUND INIT)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO FIXIT)) (:else (GO MAJOR)))))
        (FQ BIND)
        (SETQ :RESULT (CALLSM (SMBIND)))
        (COND (:RESULT (GO INIT)))
FIXIT
        (and LABELTRACE (PASSING 'FIXIT))
        (SETQ PTW CUT)
        (SETQ :RESULT (CUT (MOVE-PTW)))
        (COND (:RESULT (GO INIT)) (:else (GO MAJOR)))
MAJOR
        (and LABELTRACE (PASSING 'MAJOR))
        (CUT END)
        (COND ((EQ PUNCT '?) (GO QUEST))
        ((or (CQ IMPER) (EQ PUNCT '!)) (GO IMPER)))
        (GO THEREINIT)
FDEC
        (and LABELTRACE (PASSING 'FDEC))
        (FQ DECLAR)
THEREINIT
        (and LABELTRACE (PASSING 'THEREINIT))
        (SETQ :RESULT (and (NEXTWORD? 'THERE) (PARSE nil THERE) (FQ DECLAR)))
        (COND (:RESULT (COND ((nil? NN) (M INIT) (GO FAIL)) (:else (GO THERE)))))
THER2
        (and LABELTRACE (PASSING 'THER2))
        (and (NQ PREP) (PARSE PREPG INIT) (or (CALLSM (SMRELATE H)) (POP)))
        (and (NQ ADV) (PARSE ADV TIMW) (or (CALLSM (SMADVERB)) (POP)))
        (and (NQ ADV) (PARSE ADJG ADV VBAD) (or (CALLSM (SMRELATE H)) (POP)))
        (PARSE NG TIME)
        (SETQ :RESULT (EQ LOCATIONMARKER N))
        (COND (:RESULT (COND ((nil? NN) (GO INPOP)) (:else (GO CLAUSETYPE)))) (:else (GO INIT)))
INPOP
        (and LABELTRACE (PASSING 'INPOP))
        (SETQ :RESULT (MOVE-PT C DLC))
        (COND ((nil? :RESULT) (M INPOP) (GO FAIL)))
BICUT
        (and LABELTRACE (PASSING 'BICUT))
        (CUT-BACK-ONE)
        (GO INIT)
CLAUSETYPE
        (and LABELTRACE (PASSING 'CLAUSETYPE))
        (SETQ :RESULT (CQ DECLAR))
        (COND (:RESULT (GO SUBJ)))
        (SETQ :RESULT (and (NQ VB) (NQ INF) (PARSE VG IMPER) (FQ IMPER)))
        (COND (:RESULT (GO VG1)))
        (FQ DECLAR)
        (SETQ :RESULT (CQ IMPER))
        (COND (:RESULT (M IMPER) (GO FAIL)))
SUBJ
        (and LABELTRACE (PASSING 'SUBJ))
        (CUT END)
SUBJ3
        (and LABELTRACE (PASSING 'SUBJ3))
        (SETQ :RESULT (or (and (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ)) (and (PARSE CLAUSE RSNG ING SUBJ))))
        (COND (:RESULT (COND ((nil? NN) (GO SUBJ1)) (:else (GO SUBREG)))))
SUBJ4
        (and LABELTRACE (PASSING 'SUBJ4))
        (SETQ :RESULT (PARSE NG SUBJ))
        (COND (:RESULT (COND ((nil? NN) (GO SUBJ1)) (:else (GO SUBREG)))))
        (COND ((CQ REL-NOT-FOUND)
                (RQ REL-NOT-FOUND)
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VB))
            (SUBJ-VB-BACKUP-TYPE1 (SETQ SUBJ-VB-BACKUP-TYPE1 nil) (GO SUBJ11))
            ((and H (ISQ H TIME) (ISQ H NG)) (SETR 'SUBJECT H C) (GO VB))
            ((MOVE-PT C U (REL-NOT-FOUND))
                (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
                (SETR 'RELHEAD (GETR 'RELHEAD PT) C)
                (REMOVE-F-PT 'REL-NOT-FOUND PT)
                (GO VB))
            ((and (CQ COMPONENT) NN) (FQ SUBJFORK) (GO VB))
            (H (POP) (GO SUBJ))
            ((GO FAIL)))
HEAD
        (and LABELTRACE (PASSING 'HEAD))
        (SETQ :RESULT (or (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON))))
        (COND ((nil? :RESULT) (M HEAD) (GO FAIL)))
SUB2
        (and LABELTRACE (PASSING 'SUB2))
        (SETQ :RESULT (POP))
        (COND ((nil? :RESULT) (GO FAIL)))
        (SETQ :RESULT (CUT PTW))
        (COND (:RESULT (GO INIT)) (:else (GO SUB2)))
SUBJ1
        (and LABELTRACE (PASSING 'SUBJ1))
        (COND ((ISQ H QUOTED) (and (ISQ H LIST) (FQ LIST)) (FQ QUOTED) (SETQ H (H H)) (GO RETSM)))
        (and (CQ REL-NOT-FOUND) (MOVE-PT H PV (QAUX))
            (COND ((ISQ PT BE) (FQ INT AUXBE) (RQ REL-NOT-FOUND) (SETR 'COMP (GETR 'RELHEAD C) C) (SETR 'SUBJECT H C) (SETMVB PT) (GO ONT))
                ((ISQ PT HAVE) (FQ SUBQ) (RQ REL-NOT-FOUND) (SETR 'SUBJECT (GETR 'RELHEAD C) C) (GO VBL))))
SUBJ11
        (and LABELTRACE (PASSING 'SUBJ11))
        (SETQ :RESULT (CUT-BACK-ONE))
        (COND (:RESULT (GO SUBJ3)) (:else (M SUBJ11) (GO FAIL)))
SUBREG
        (and LABELTRACE (PASSING 'SUBREG))
        (SETR 'SUBJECT H C)
        (GO VB)
VB
        (and LABELTRACE (PASSING 'VB))
        (SETQ :RESULT (PARSE ADJG ADV VBAD))
        (COND (:RESULT (COND ((nil? NN) (M VB-ADJG) (GO FAIL)) (:else (GO VB)))))
        (RQ VBLOK)
VBL
        (and LABELTRACE (PASSING 'VBL))
        (SETQ :RESULT (PARSE VG))
        (COND (:RESULT (GO VBREG)))
NOVERB
        (and LABELTRACE (PASSING 'NOVERB))
        (COND ((CQ SUBJFORK) (FQ VBFORK) (GO FINDOBJ1))
            ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
            ((not (ISQ H SUBJ)) (GO FAIL))
            ((ISQ H CLAUSE) (SETQ SUBJ-VB-BACKUP-TYPE1 true) (POP) (GO SUBJ4))
            ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))
VB2
        (and LABELTRACE (PASSING 'VB2))
        (CUT-BACK-ONE)
        (GO SUBJ3)
VBREG
        (and LABELTRACE (PASSING 'VBREG))
        (SETR 'VG H C)
VG1
        (and LABELTRACE (PASSING 'VG1))
        (CUT END)
        (SETQ :RESULT (ISQ MVB BE))
        (COND (:RESULT (COND ((nil? NN) (M BE) (GO FAIL)) (:else (GO BE)))))
        (SETQ :RESULT (ISQ MVB VPRT))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO CHECKPASV)) (:else (GO CHECKPASV)))))
        (SETQ :RESULT (and (NQ PRT) (PARSE PRT)))
        (COND ((nil? :RESULT) (GO DPRT)))
        (FQ PRT)
        (SETQ :RESULT (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD (NB H)))))
        (COND (:RESULT (GO CHECKPASV)) (:else (GO POPRT)))
DPRT
        (and LABELTRACE (PASSING 'DPRT))
        (SETQ :RESULT (ISQ H PASV))
        (COND (:RESULT (GO CHECKPASV)))
        (SETQ :RESULT (SETQ POSITION-OF-PRT (MOVE-PTW N NW (PRT))))
        (COND ((nil? :RESULT) (GO FINDOBJ1)))
        (SETQ :RESULT (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD POSITION-OF-PRT))))
        (COND ((nil? :RESULT) (GO POPRT)))
        (SETQ :RESULT (ISQ MVB TRANS))
        (COND ((nil? :RESULT) (GO FINDOBJ1)))
        (CUT POSITION-OF-PRT)
        (SETQ :RESULT (PARSE NG OBJ OBJ1))
        (COND (:RESULT (GO POPRT)) (:else (GO FINDOBJ1)))
        (CUT END)
        (SETR 'OBJ1 H C)
        (PARSE PRT)
        (FQ PRT DPRT)
        (GO FINDOBJ2)
POPRT
        (and LABELTRACE (PASSING 'POPRT))
        (POPTO VG)
        (GO FINDOBJ1)
CHECKPASV
        (and LABELTRACE (PASSING 'CHECKPASV))
        (SETQ :RESULT (and (ISQ H PASV) (FQ PASV) (SETR 'OBJ1 (GETR 'SUBJECT C) C)))
        (COND (:RESULT (COND ((nil? NN) (GO FINDFAKE2)) (:else (GO FINDOBJ2)))))
        (FQ ACTV)
        (GO FINDOBJ1)
BE
        (and LABELTRACE (PASSING 'BE))
        (FQ BE)
        (and (PARSE nil NOT) (FQ NEG))
        (PARSE ADV VBAD)
FINDOBJ1
        (and LABELTRACE (PASSING 'FINDOBJ1))
        (SETQ :RESULT (or (CANPARSE 1 '(ADJG COMP) 'INT)
                (CANPARSE 1 '(NG COMP) 'INT)))
        (COND (:RESULT (COND ((nil? NN) (GO ONT)) (:else (GO CHECKIT)))))
        (SETQ :RESULT (or (CANPARSE 1 '(PREPG COMP) 'INT)
                (CANPARSE 1 '(CLAUSE RSNG ING) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG REPORT) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG TO) 'TRANS)
                (CANPARSE 1 '(PREPG LOC) 'ITRNSL)
                (CANPARSE 1 '(ADV PLACE) 'ITRNSL)))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (CANPARSE 1 '(NG) 'TRANS))
        (COND (:RESULT (COND ((nil? NN) (GO FINDFAKE2)) (:else (GO FINDOBJ2)))))
FINDFAKE1
        (and LABELTRACE (PASSING 'FINDFAKE1))
        (SETQ :RESULT (MOVE-PT C U (REL-NOT-FOUND)))
        (COND (:RESULT (GO OBJ1REL)))
        (SETQ :RESULT (and (CANTAKE 1 '(PREPG LOC) 'ITRNSL) (MOVE-PT C U (QADJ)) (ISQ (GETR 'QADJ PT) PLACE) (FQ ITRANSL)))
        (COND (:RESULT (GO PUTLOBJ)))
        (SETQ :RESULT (CANPARSE 1 nil 'ITRNS))
        (COND (:RESULT (GO ONT)))
GOOF1
        (and LABELTRACE (PASSING 'GOOF1))
        (or GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - FIRST OBJ))
        (GO FAIL)
OBJ1REL
        (and LABELTRACE (PASSING 'OBJ1REL))
        (SETR 'OBJ1 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ1REL)
FINDOBJ2
        (and LABELTRACE (PASSING 'FINDOBJ2))
        (SETQ :RESULT (CANPARSE 2 '(CLAUSE RSNG TO) 'TRANS2))
        (COND (:RESULT (GO FIXSUBJECT)))
        (SETQ :RESULT (or (CANPARSE 2 '(ADV PLACE) 'TRANSL) (CANPARSE 2 '(PREPG LOC) 'TRANSL)))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (or (CANPARSE 2 '(ADJG COMP) 'TRANSINT) (CANPARSE 2 '(NG COMP) 'TRANSINT)))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (CANPARSE 2 '(NG) 'TRANS2))
        (COND (:RESULT (GO ONT)))
FINDFAKE2
        (and LABELTRACE (PASSING 'FINDFAKE2))
        (SETQ :RESULT (and (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND))))
        (COND (:RESULT (GO OBJ2REL)))
        (SETQ :RESULT (and (CANTAKE 2 '(PREPG LOC) 'TRANSL) (MOVE-PT C U (QADJ)) (ISQ (GETR 'QADJ PT) PLACE) (FQ TRANSL)))
        (COND (:RESULT (GO PUTLOBJ)))
OBJ2TO
        (and LABELTRACE (PASSING 'OBJ2TO))
        (PARSE ADV VBAD)
        (SETQ :RESULT
        (COND ((and (NEXTWORD? 'TO) (ISQ MVB TO2) (PARSE PREPG TO)) (SETR 'OBJ2 (GETR 'OBJ1 H) C) (FQ TRANS2TO TRANS2))
            ((and (CQ PREPQ) (MOVE-PT H PV (QUEST)) (EQ (WORD (MOVE-PTW FW)) 'TO) (RQ PREPQ) (FQ TRANS2TOQ TRANS2) (SETR 'OBJ2 (GETR 'OBJ1 PT) C)))))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (CANPARSE 2 nil 'TRANS))
        (COND (:RESULT (GO ONT)) (:else (GO FAIL)))
PUTLOBJ
        (and LABELTRACE (PASSING 'PUTLOBJ))
        (SETR 'LOBJ PT C)
        (SETR 'RELHEAD (GETR 'QADJ PT) PT)
        (SETR 'QADJ nil PT)
        (REMOVE-F-PT 'QADJ PT)
        (GO ONT)
OBJ2REL
        (and LABELTRACE (PASSING 'OBJ2REL))
        (SETR 'OBJ2 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ2REL)
        (GO ONT)
FIXSUBJECT
        (and LABELTRACE (PASSING 'FIXSUBJECT))
        (SETR 'SUBJECT (GETR 'OBJ1 C) H)
        (GO ONT)
CHECKIT
        (and LABELTRACE (PASSING 'CHECKIT))
        (SETQ :RESULT (EQ (WORD (NB (GETR 'SUBJECT C))) 'IT))
        (COND ((nil? :RESULT) (GO ONT)))
        (SETQ :RESULT (or (and (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ)) (and (NQ ING) (PARSE CLAUSE RSNG ING SUBJ)) (PARSE CLAUSE REPORT)))
        (COND ((nil? :RESULT) (GO ONT)))
        (FQ IT)
        (SETR 'LOGICAL-SUBJECT H C)
        (GO ONT)
GOOF2
        (and LABELTRACE (PASSING 'GOOF2))
        (or GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - SECOND OBJECT))
        (GO FAIL)
ONT
        (and LABELTRACE (PASSING 'ONT))
        (SETQ :RESULT (CQ PASV))
        (COND (:RESULT (GO PONT)))
ONT1
        (and LABELTRACE (PASSING 'ONT1))
        (SETQ :RESULT (CALLSM (SMCL1)))
        (COND ((nil? :RESULT) (M SMCL1) (GO FAIL)))
        (SETQ :RESULT (not (CQ REL-NOT-FOUND)))
        (COND (:RESULT (COND ((nil? NN) (GO RETSM)) (:else (GO TONT)))))
        (SETQ :RESULT (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1))
        (COND ((nil? :RESULT) (GO PREPSHORT)))
TIMEQ
        (and LABELTRACE (PASSING 'TIMEQ))
        (RQ REL-NOT-FOUND)
        (FQ TIMEQ)
        (GO TONT)
PREPSHORT
        (and LABELTRACE (PASSING 'PREPSHORT))
        (SETQ :RESULT (and (NQ PREP) (PARSE PREPG)))
        (COND ((nil? :RESULT) (M ONT-SHORT-PREP) (GO FAIL)))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND ((nil? :RESULT) (M ONTß) (GO FAIL)))
        (SETQ :RESULT (CQ REL-NOT-FOUND))
        (COND (:RESULT (COND ((nil? NN) (M ONT-NOT-FOUND) (GO FAIL)) (:else (GO PREPSHORT))))
            (:else (GO TONT)))
PONT
        (and LABELTRACE (PASSING 'PONT))
        (and (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))
        (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)
        (GO ONT1)
TONT
        (and LABELTRACE (PASSING 'TONT))
        (SETQ :RESULT (SETQ POSITION-OF-PTW N))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO RETSM)))))
NPASV
        (and LABELTRACE (PASSING 'NPASV))
        (SETQ :RESULT (and (NQ PREP) (PARSE PREPG) (CALLSM (SMRELATE H))))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (and (NQ TIMW) (PARSE ADV TIMW) (or (CALLSM (SMTIME)) (GO FAIL))))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (and (not (CQ BE)) (PARSE ADJG ADV) (or (CALLSM (SMRELATE H)) (GO FAIL))))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (and (PARSE NG TIME) (or (CALLSM (SMTIME)) (GO FAIL))))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (and (NQ PLACE) (PARSE ADV PLACE) (or (CALLSM (SMPLACE)) (GO FAIL))))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (and (NQ BINDER) (PARSE CLAUSE BOUND) (or (CALLSM (SMBIND)) (GO FAIL))))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (and (NEXTWORD? 'TO) (PARSE CLAUSE TO ADJUNCT) (or (CALLSM (SMTOADJ)) (GO FAIL))))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (EQ N POSITION-OF-PTW))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO TONT)))))
        (SETQ :RESULT (or (not (CQ TOPLEVEL)) (NQ SPECIAL)))
        (COND (:RESULT (GO RETSM)))
        (ERT "CLAUSE:" SOMETHING LEFT OVER AT TOP LEVEL)
        (GO FAIL)
THERE
        (and LABELTRACE (PASSING 'THERE))
        (FQ THERE)
        (CUT END)
        (SETQ :RESULT (PARSE ADV TIMW))
        (COND ((and (nil? NN) :RESULT) (M THERE) (GO FAIL)))
        (SETQ :RESULT (and (PARSE VG) (ISQ MVB BE)))
        (COND (:RESULT (COND ((nil? NN) (M THERE) (GO FAIL)) (:else (GO THEF))))
            (:else (GO NOTHE)))
THERQ
        (and LABELTRACE (PASSING 'THERQ))
        (SETQ :RESULT (ISQ (MOVE-PT H PV (QAUX)) BE))
        (COND (:RESULT (GO THERQ2)))
        (SETQ :RESULT (and (NQ TIMW) (PARSE ADV TIMW)))
        (COND ((and (nil? NN) :RESULT) (M THEREQ) (GO FAIL)))
        (SETQ :RESULT (and (PARSE VG) (ISQ MVB BE)))
        (COND (:RESULT (GO THERQ2)))
        (RQ POLR2)
        (GO NOTHE)
THERQ2
        (and LABELTRACE (PASSING 'THERQ2))
        (FQ SUBJTQ)
        (FQ THERE)
        (SETQ :RESULT (CQ POLAR))
        (COND (:RESULT (GO THEF)) (:else (GO ONT)))
THEF
        (and LABELTRACE (PASSING 'THEF))
        (SETQ :RESULT (and (NQ ADV) (PARSE ADV TIMW)))
        (COND ((and (nil? NN) :RESULT) (M THEF) (GO FAIL)))
        (SETQ :RESULT (PARSE NG SUBJ SUBJT))
        (COND ((nil? :RESULT) (GO THERREL)))
        (FQ THERE)
        (SETR 'SUBJECT H C)
        (GO ONT)
THERREL
        (and LABELTRACE (PASSING 'THERREL))
        (SETQ :RESULT (MOVE-PT C U (REL-NOT-FOUND)))
        (COND ((nil? :RESULT) (GO NOTHE)))
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
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
        (SETQ :RESULT (PARSE NG TIME))
        (COND ((and (nil? NN) :RESULT) (GO IMPOP)))
        (SETQ :RESULT (and (NQ ADV) (PARSE ADJG ADV VBAD)))
        (COND ((and (nil? NN) :RESULT) (GO IMPOP)))
        (SETQ :RESULT (and (NQ ADV) (PARSE ADV TIMW)))
        (COND ((and (nil? NN) :RESULT) (GO IMPOP)))
IMPE
        (and LABELTRACE (PASSING 'IMPE))
        (SETQ :RESULT (PARSE VG IMPER))
        (COND ((nil? :RESULT) (GO IMPOP)))
        (FQ IMPER)
        (GO VG1)
IMPOP
        (and LABELTRACE (PASSING 'IMPOP))
        (SETQ :RESULT (POP nil))
        (COND (:RESULT (GO IMPE)) (:else (M IMPOP) (GO FAIL)))
QUEST
        (and LABELTRACE (PASSING 'QUEST))
        (FQ QUEST)
        (SETQ :RESULT (NQ PREP))
        (COND ((nil? :RESULT) (GO NGQUES)))
        (SETQ :RESULT (PARSE PREPG))
        (COND ((nil? :RESULT)
            (COND ((nil? NN) (M PREPQ-INCOMPLETE) (GO FAIL)) (:else (GO NGQUES)))))
        (SETQ :RESULT (ISQ H QUEST))
        (COND ((nil? :RESULT) (GO QUEST)))
        (SETR 'QADJ H C)
        (GO POLAR)
NGQUES
        (and LABELTRACE (PASSING 'NGQUES))
        (SETQ :RESULT (PARSE NG QUEST))
        (COND (:RESULT (GO NGQST)))
        (SETQ :RESULT
        (or (and (NEXTWORD? 'HOW) (PARSE ADJG QUEST) (SETR 'RELHEAD H C)) (and (NQ QADJ) (PARSE QADJ) (FQ QADJ) (SETR 'QADJ H C))))
        (COND (:RESULT (GO POLAR)) (:else (GO POLAR)))
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))
ADJQS
        (and LABELTRACE (PASSING 'ADJQS))
        (GO RETURN)
NGQST
        (and LABELTRACE (PASSING 'NGQST))
        (SETR 'RELHEAD H C)
NGQST2
        (and LABELTRACE (PASSING 'NGQST2))
        (CUT END)
        (SETR 'SUBJECT H C)
        (and (NQ ADV) (PARSE ADJG ADV VBAD))
        (COND ((PARSE VG NAUX) (FQ SUBJQ) (GO VG1))
            ((NQ VB) (FQ REL-NOT-FOUND) (GO POLAR))
            (:else (MOVE-PTW N PW) (POP NG QUEST) (CUT PTW) (GO NGQUES)))
QUEST2
        (and LABELTRACE (PASSING 'QUEST2))
        (SETQ :RESULT (and (NEXTWORD? 'THERE) (PARSE nil THERE)))
        (COND (:RESULT (GO THERQ)) (:else (GO SUBF)))
SUBF
        (and LABELTRACE (PASSING 'SUBF))
        (SETQ :RESULT (PARSE NG SUBJ))
        (COND (:RESULT (COND ((nil? NN) (GO SUBJ1)) (:else (GO SUBREG)))))
        (RQ REL-NOT-FOUND)
        (GO BE)
POLAR
        (and LABELTRACE (PASSING 'POLAR))
        (SETQ :RESULT (and (NQ VB) (PARSE VB AUX (QAUX)) (SETR 'QAUX H C) (CALLSM (SMVAUX)) (SETMVB H)))
        (COND ((nil? :RESULT) (GO QCHOP)))
        (or (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
        (FQ POLR2)
        (GO QUEST2)
QCHOP
        (and LABELTRACE (PASSING 'QCHOP))
        (ERT "CLAUSE:" QCHOP)
        (SETQ :RESULT (POPTO CLAUSE BOUND))
        (COND (:RESULT (GO BICUT)) (:else (M QCHOP) (GO FAIL)))
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
        (SETQ :RESULT (PARSE BINDER))
        (COND ((nil? :RESULT)
            (COND ((nil? NN) (M BINDER) (GO FAIL)) (:else (M BOUND) (GO FAIL)))))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RSQ
        (and LABELTRACE (PASSING 'RSQ))
        (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
        (SETQ :RESULT (CQ PREPREL))
        (COND ((nil? :RESULT) (GO RSQ2)))
        (PARSE PREPG PRONREL)
        (SETR 'QADJ H C)
        (GO REPORT)
RSQ2
        (and LABELTRACE (PASSING 'RSQ2))
        (COND ((PARSE VG EN PASV)
                (or (ISQ MVB TRANS) (GO FAIL))
                (SETR 'SUBJECT (GETR 'RELHEAD C) C)
                (GO VG1))
            ((PARSE VG ING) (SETR 'SUBJECT (GETR 'RELHEAD C) C) (GO VG1))
            ((NQ PRONREL) (PARSE NG RELWD) (GO REL))
            ((CQ COMPONENT)
                (SETR 'RELHEAD (GETR 'RELHEAD (MOVE-PT C PC)) C)
                (GO REL))
            ((PARSE NG SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
            (:else (GO FAIL)))
REL
        (and LABELTRACE (PASSING 'REL))
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (SETQ :RESULT (PARSE VG))
        (COND (:RESULT (GO VG1)))
        (FQ REL-NOT-FOUND)
        (GO SUBJ)
TO
        (and LABELTRACE (PASSING 'TO))
        (SETQ :RESULT (and (CQ COMPONENT) (PARSE VG TO TODEL)))
        (COND (:RESULT (GO VG1)))
        (SETQ :RESULT (NEXTWORD? 'FOR))
        (COND ((nil? :RESULT) (GO TO1)))
        (PARSE nil FOR)
        (FQ FOR)
        (PARSE NG SUBJ TOSUBJ)
        (SETR 'SUBJECT H C)
TO1
        (and LABELTRACE (PASSING 'TO1))
        (SETQ :RESULT (PARSE VG TO))
        (COND (:RESULT (GO VG1)) (:else (M TO) (GO FAIL)))
ING
        (and LABELTRACE (PASSING 'ING))
        (SETQ :RESULT (MOVE-PTW N NW (ING)))
        (COND ((nil? :RESULT) (GO FAIL)))
        (SETQ :RESULT (or (NQ ING) (CQ OBJ2) (and (PARSE NG SUBJ INGSUBJ) (SETR 'SUBJECT H C) (FQ SUBING) (RQ ING))))
        (COND ((and (nil? NN) :RESULT) (M ING) (GO FAIL)))
        (SETQ :RESULT (PARSE VG ING))
        (COND (:RESULT (GO VG1)) (:else (M ING) (GO FAIL)))
REPORT
        (and LABELTRACE (PASSING 'REPORT))
        (and (NEXTWORD? 'THAT) (PARSE nil THAT) (FQ THAT))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (or (CALLSM (SMCL2)) (GO FAIL))
        (GO RETURN)
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ DEFUN NG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil :RESULT nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
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
        (SETQ :RESULT (ISQ H POSS))
        (COND (:RESULT (GO PROPS)))
        (SETQ :RESULT (and NN (NQ PROPN)))
        (COND (:RESULT (GO PROPN)))
PROPS
        (and LABELTRACE (PASSING 'PROPS))
        (or (CALLSM (SMPROP)) (GO FAIL))
        (SETQ :RESULT (ISQ H POSS))
        (COND (:RESULT (GO POSS)) (:else (GO PRAG)))
PRON
        (and LABELTRACE (PASSING 'PRON))
        (SETQ :RESULT (PARSE PRON POSS))
        (COND (:RESULT (COND ((nil? NN) (GO RED2)) (:else (GO POSS)))))
PRON2
        (and LABELTRACE (PASSING 'PRON2))
        (SETQ :RESULT (CQ NPRON))
        (COND (:RESULT (M NPRON) (GO FAIL)))
        (SETQ :RESULT (or (and (CQ SUBJ) (PARSE PRON SUBJ)) (and (or (CQ OBJ) (CQ TOSUBJ) (CQ INGSUBJ)) (PARSE PRON OBJ)) (CQ INGSUBJ)))
        (COND ((nil? :RESULT) (M PRON) (GO FAIL)))
        (FQ PRONG DEF)
PRON3
        (and LABELTRACE (PASSING 'PRON3))
        (SETQ :RESULT (CALLSM (SMPRON H)))
        (COND ((nil? :RESULT) (GO FAIL)))
PRAG
        (and LABELTRACE (PASSING 'PRAG))
        (SETR 'HEAD H C)
        (MOVE-PT H)
        (TRNSF NS NPL NFS NEG)
        (GO RETURN)
TPRON
        (and LABELTRACE (PASSING 'TPRON))
        (PARSE TPRON)
        (FQ TPRON)
        (MOVE-PT H)
        (TRNSF NS NPL ANY NEG)
        (SETR 'HEAD C H)
        (and NN (NQ ADJ) (PARSE ADJ))
        (GO SMNG)
EVERPRON
        (and LABELTRACE (PASSING 'EVERPRON))
        (SETQ :RESULT (and (PARSE PRON EVERPRON) (CALLSM (SMPRON H))))
        (COND ((nil? :RESULT) (GO FAIL)))
        (SETQ :RESULT (and (PARSE CLAUSE RSQ NOREL) (CALLSM (SMRELATE H))))
        (COND (:RESULT (GO RETSM)) (:else (GO FAIL)))
AS
        (and LABELTRACE (PASSING 'AS))
        (SETQ :RESULT (and (PARSE nil AS) (PARSE NUMD NUMDAS) NN (PARSE nil AS)))
        (COND (:RESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO NUMD2)))) (:else (M AS) (GO FAIL)))
AT
        (and LABELTRACE (PASSING 'AT))
        (SETQ :RESULT (and (PARSE nil AT) (PARSE NUMD NUMDAT)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (M AT) (GO FAIL)) (:else (M AT) (GO FAIL)))))
NUMD2
        (and LABELTRACE (PASSING 'NUMD2))
        (SETQ :RESULT (and (PARSE NUM) (FQ NUM NUMD)))
        (COND (:RESULT (COND ((nil? NN) (GO INCOM)) (:else (GO DET1)))) (:else (M NUMD2) (GO FAIL)))
NUMD
        (and LABELTRACE (PASSING 'NUMD))
        (SETQ :RESULT (PARSE NUMD NUMDAN))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO ND3)))))
        (SETQ :RESULT (PARSE nil THAN))
        (COND (:RESULT (COND ((nil? NN) (GO POPCOM)) (:else (GO NUMD2)))) (:else (GO INCOM)))
ND3
        (and LABELTRACE (PASSING 'ND3))
        (SETQ :RESULT (PARSE NUMD NUMDALONE))
        (COND (:RESULT (COND ((nil? NN) (M NUMD) (GO FAIL)) (:else (GO NUMD2)))) (:else (M NUMD) (GO FAIL)))
TIME
        (and LABELTRACE (PASSING 'TIME))
        (SETQ :RESULT (and (NQ TIME) (PARSE NOUN TIME)))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (MOVE-PTW N NW (TIM1)))
        (COND (:RESULT (GO LOOK)) (:else (M TIME) (GO FAIL)))
TIMORD
        (and LABELTRACE (PASSING 'TIMORD))
        (SETQ :RESULT (PARSE ORD TIMORD))
        (COND ((nil? :RESULT) (GO FAIL)))
        (SETQ :RESULT (and (PARSE NOUN TIM1) (FQ DET DEF) (CALLSM (SMNGTIME))))
        (COND (:RESULT (GO RETURN)) (:else (GO FAIL)))
DET
        (and LABELTRACE (PASSING 'DET))
        (PARSE DET)
        (FQ DET)
        (MOVE-PT H)
        (SETQ :RESULT (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR))
        (COND (:RESULT (COND ((nil? NN) (GO INCOM)) (:else (GO IND)))) (:else (M BUG) (GO FAIL)))
IND
        (and LABELTRACE (PASSING 'IND))
        (SETQ :RESULT (and (EQ (WORD (NB H)) 'ALL) (EQ (WORD N) 'THE) (PARSE DET) (FQ DEF)))
        (COND (:RESULT (COND ((nil? NN) (M THE) (GO FAIL)) (:else (GO NUM)))))
        (SETQ :RESULT (and (ISQ H QNTFR) (FQ QNTFR)))
        (COND (:RESULT (GO QNUM)))
ORD
        (and LABELTRACE (PASSING 'ORD))
        (SETQ :RESULT (and (PARSE ORD) (FQ ORD)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO NUM)))))
        (SETQ :RESULT (and (NEXTWORD? 'OF) (ISQ (MOVE-PTW N NW) MONTH) (PARSE nil OF) (PARSE NOUN MONTH) (FQ DATE)))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (CQ DEF))
        (COND ((nil? :RESULT) (GO ADJ)))
NUM
        (and LABELTRACE (PASSING 'NUM))
        (SETQ :RESULT (PARSE NUM))
        (COND ((nil? :RESULT) (GO ADJ)))
        (FQ NUM)
        (SETQ :RESULT (CQ DET))
        (COND ((nil? :RESULT) (GO DET1)))
        (SETQ :RESULT (COND ((and (ISQ H NS) (CQ NS)) (RQ NPL PART)) ((CQ NPL) (RQ NS PART))))
        (COND (:RESULT (COND ((nil? NN) (GO INCOM)) (:else (GO ADJ)))) (:else (M NUM) (GO FAIL)))
DET1
        (and LABELTRACE (PASSING 'DET1))
        (COND ((ISQ H NS) (FQ NS)) (:else (FQ NPL)))
        (or NN (and (FQ NUMBER) (GO INCOM)))
NUMBER
        (and LABELTRACE (PASSING 'NUMBER))
        (FQ DET)
        (SETQ :RESULT (NQ OF))
        (COND (:RESULT (GO OF)) (:else (GO ADJ)))
QNUM
        (and LABELTRACE (PASSING 'QNUM))
        (SETQ :RESULT (ISQ H NONUM))
        (COND (:RESULT (GO OF)))
        (SETQ :RESULT (and (PARSE NUM) (FQ NUM)))
        (COND ((nil? :RESULT) (GO OF)))
        (SETQ :RESULT (COND ((EQ (SM H) 1) (and (CQ NS) (RQ NPL))) ((CQ NPL) (RQ NS))))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO INCOM)) (:else (M NUMD) (GO FAIL)))))
        (SETQ :RESULT (EQ (WORD (NB H)) 'NO))
        (COND (:RESULT (GO ADJ)))
OF
        (and LABELTRACE (PASSING 'OF))
        (SETQ :RESULT (and (NQ OF) (PARSE PREPG OF)))
        (COND (:RESULT (GO SMOF)) (:else (GO NONE)))
SMOF
        (and LABELTRACE (PASSING 'SMOF))
        (FQ OF)
        (SETQ :RESULT (or (CALLSM (SMNGOF)) (not (POP))))
        (COND (:RESULT (GO RETSM)) (:else (GO INCOM)))
NONE
        (and LABELTRACE (PASSING 'NONE))
        (SETQ :RESULT (EQ (WORD (NB H)) 'NONE))
        (COND (:RESULT (GO INCOM)) (:else (GO ADJ)))
ADJ
        (and LABELTRACE (PASSING 'ADJ))
        (SETQ :RESULT (PARSE ADJ))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO EPR)))))
        (and (ISQ H COMPAR) (FQ COMPARATIVE-MODIFIER) (SETR 'COMPARATIVE-MODIFIER H C))
        (GO ADJ)
EPR
        (and LABELTRACE (PASSING 'EPR))
        (SETQ :RESULT (or (ISQ H SUP) (ISQ H COMPAR)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO CLASF)))))
        (FQ ADJ)
        (and (NEXTWORD? 'OF)
            (PARSE PREPG OF)
            (or (CALLSM (SMNGOF)) (GO FAIL))
            (FQ OF)
            (GO RETSM))
CLASF
        (and LABELTRACE (PASSING 'CLASF))
        (SETQ :RESULT (or (PARSE VB ING (CLASF)) (PARSE VB EN (CLASF)) (PARSE CLASF)))
        (COND (:RESULT (COND ((nil? NN) (GO REDUC)) (:else (GO CLASF)))))
NOUN
        (and LABELTRACE (PASSING 'NOUN))
        (SETQ :RESULT (PARSE NOUN))
        (COND ((nil? :RESULT) (GO RED2)))
        (SETQ :RESULT (and (CQ TIME) (not (ISQ H TIM1))))
        (COND (:RESULT (GO RED1)))
        (SETQ T1 FE)
        (COND ((and (ISQ H MASS) (or (CQ PART) (not (CQ DET)))) (FQ MASS)))
        (COND ((not (ISQ H NPL)) (RQ NPL PART)))
        (COND ((not (ISQ H NS)) (RQ NS)))
        (COND ((and (not (CQ DET)) (not (CQ NUMD))) (MOVE-PT H) (TRNSF NPL MASS)))
        (SETQ :RESULT (MEET FE '(NS NPL PART MASS)))
        (COND ((nil? :RESULT) (GO RED0)))
        (SETQ :RESULT (NEXTWORD? 'THAN))
        (COND ((nil? :RESULT) (GO SMNG)))
        (FQ THAN)
SMNG
        (and LABELTRACE (PASSING 'SMNG))
        (SETR 'HEAD H C)
        (SETQ :RESULT (and (CQ OBOFJ) (not (CQ DEF))))
        (COND (:RESULT (GO FAIL)))
        (or (CALLSM (SMNG1)) (GO FAIL))
        (SETQ :RESULT (not (ISQ H POSS)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO POSS)))))
        (SETQ :RESULT (and (CQ THAN) (PARSE ADJG)))
        (COND ((nil? :RESULT) (GO RSQ-TO)))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (GO RETSM)) (:else (GO FAIL)))
RSQ-TO
        (and LABELTRACE (PASSING 'RSQ-TO))
        (SETQ :RESULT (and (NEXTWORD? 'TO) (MEET FE '(COMP SUBJ)) (PARSE CLAUSE RSQ TO) (or (CALLSM (SMRELATE H)) (GO POPRET))))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (and (or (NEXTWORD? 'AS) (NQ COMPAR)) (PARSE ADJG THANNEED)))
        (COND ((nil? :RESULT) (GO PREPNG)))
        (and (nil? N) (CQ SUBJ) (ISQ (MOVE-PT C PV) AUX) (ISQ PT BE) (GO POPRET))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (COND ((nil? NN) (GO RETSM)) (:else (GO RSQ-TO)))) (:else (GO POPRET)))
PREPNG
        (and LABELTRACE (PASSING 'PREPNG))
        (SETQ :RESULT (and (NQ PREP) (not (or (and (NQ PLACE) (CQ NOLOC)) (and (CQ OBJ1) (ISQ MVB TRANSL) (not (ISQ (MOVE-PT C U) QUEST))))) (PARSE PREPG Q)))
        (COND ((nil? :RESULT) (GO DISGRSQ)))
        (and (nil? N)
            (CQ SUBJ)
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (not (ISQ (MOVE-PT U) NGQ))
            (GO POPRET))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (COND ((nil? NN) (GO RETSM)) (:else (GO RSQ-TO)))) (:else (GO POPRET)))
DISGRSQ
        (and LABELTRACE (PASSING 'DISGRSQ))
        (SETQ :RESULT (EQ (car MES) 'PREP-WHICH))
        (COND ((nil? :RESULT) (GO RSQ)))
        (SETQ MES (cdr MES))
        (SETQ :RESULT (PARSE CLAUSE RSQ PREPREL))
        (COND (:RESULT (COND ((nil? NN) (GO RETSM)) (:else (GO PREPNG)))) (:else (M RSQ-PREPREL) (GO FAIL)))
RSQ
        (and LABELTRACE (PASSING 'RSQ))
        (SETQ :RESULT (and (ISQ (MOVE-PT C U) POLR2) (CQ SUBJ) (NQ VB) (not (CQ SUBJT)) (not (ISQ PT QADJ))))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (PARSE CLAUSE RSQ))
        (COND ((nil? :RESULT) (GO RETSM)))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (GO RETSM)) (:else (GO POPRET)))
RED0
        (and LABELTRACE (PASSING 'RED0))
        (SETQ FE T1)
RED1
        (and LABELTRACE (PASSING 'RED1))
        (POP)
RED2
        (and LABELTRACE (PASSING 'RED2))
        (COND ((nil? H) (MQ NO) (GO FAIL))
            ((ISQ H NUMBER) (GO INCOM))
            ((and (ISQ H POSS) (or (ISQ H PRON) (and (MOVE-PT H DLC) (ISQ PT PRON)))) (POP) (GO PRON2))
            ((and (nil? (cdr H)) (CQ DEFPOSS)) (GO POSSDEF))
            ((and (CQ QUEST) (nil? (cdr H))) (GO QDETCHECK))
            ((ISQ H ADJ) (GO EPR))
            ((not (ISQ H CLASF)) (GO INCOM)))
REDUC
        (and LABELTRACE (PASSING 'REDUC))
        (POP)
        (SETQ :RESULT (and (nil? H) (NQ PROPN)))
        (COND (:RESULT (GO PROPN)) (:else (GO NOUN)))
POPCOM
        (and LABELTRACE (PASSING 'POPCOM))
        (POP)
INCOM
        (and LABELTRACE (PASSING 'INCOM))
        (FQ INCOM)
        (SETQ :RESULT (and (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM))))
        (COND (:RESULT (GO RETURN)))
        (SETQ :RESULT (and (nil? CUT) (CQ NUM)))
        (COND (:RESULT (GO SMNG)))
QDETCHECK
        (and LABELTRACE (PASSING 'QDETCHECK))
        (COND ((and (ISQ H QDET) (ISQ (NB H) QPRON)) (POP) (GO QPRON))
            ((and (ISQ H QDET) (ISQ (NB H) EVERPRON)) (POP) (GO EVERPRON)))
        (GO FAIL)
POSS
        (and LABELTRACE (PASSING 'POSS))
        (or (CALLSM (SMNG2)) (GO FAIL))
POSS2
        (and LABELTRACE (PASSING 'POSS2))
        (SETQ :RESULT (CQ INGSUBJ))
        (COND (:RESULT (GO RETSM)))
        (SETQ H (BUILDNODE (reverse (cons 'POSS (SETDIF FE '(COMPONENT)))) NB N H SM))
        (SETQ BACKREF (concat H (cdr BACKREF)))
        (SETQ :RESULT (SETR 'FEATURES (SETQ FE (concat '(POSES DET DEF NS NPL) (reverse REST))) C))
        (COND ((nil? :RESULT) (M BUG) (GO FAIL)))
        (SETQ :RESULT (or (not NN) (ISQ H DEFPOSS)))
        (COND ((nil? :RESULT) (GO ORD)))
POSSDEF
        (and LABELTRACE (PASSING 'POSSDEF))
        (RQ POSES DET DEF)
        (FQ POSSDEF NS NPL)
QUEST
        (and LABELTRACE (PASSING 'QUEST))
        (SETQ :RESULT (PARSE nil HOW))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO FAIL)) (:else (GO QDET)))))
        (SETQ :RESULT (PARSE nil MANY))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO FAIL)))))
        (FQ DET NPL INDEF HOWMANY)
        (GO OF)
QDET
        (and LABELTRACE (PASSING 'QDET))
        (SETQ :RESULT (and (PARSE DET QDET) (FQ DET NPL QDET NS)))
        (COND (:RESULT (COND ((nil? NN) (GO INCOM)) (:else (GO QNUM)))))
QPRON
        (and LABELTRACE (PASSING 'QPRON))
        (SETQ :RESULT (PARSE PRON QPRON))
        (COND (:RESULT (GO PRON3)) (:else (GO FAIL)))
RELWD
        (and LABELTRACE (PASSING 'RELWD))
        (SETQ :RESULT (and (PARSE PRONREL) (CALLSM (SMSET (SM (MOVE-PT C U U (NG)))))))
        (COND (:RESULT (GO RETURN)))
POPRET
        (and LABELTRACE (PASSING 'POPRET))
        (POP)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (or (CALLSM (SMNG2)) (GO TRYA))
        (GO RETURN)
TRYA
        (and LABELTRACE (PASSING 'TRYA))
        (SETQ :RESULT (ISQ H NOUN))
        (COND ((nil? :RESULT) (M TRYA) (GO FAIL)))
        (POP)
        (CUT N)
UP
        (and LABELTRACE (PASSING 'UP))
        (SETQ :RESULT (POP))
        (COND (:RESULT (GO UP)))
        (SETQ FE (reverse REST))
        (SMSET nil)
        (GO NGSTART)
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ DEFUN VG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil :RESULT nil TENSE nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-VG
        (and LABELTRACE (PASSING 'ENTERING-VG))
        (COND ((CQ TO) (GO TO))
            ((CQ EN) (GO EN))
            ((CQ ING) (GO ING))
            ((CQ IMPER) (GO IMPER))
            ((ISQ (MOVE-PT C U) POLR2) (GO POLR2)))
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
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS)
        (SETQ TENSE (COND ((and (ISQ PT PRESENT) (ISQ PT PAST)) '(PAST-PRESENT)) ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (GO REV)
TO
        (and LABELTRACE (PASSING 'TO))
        (FQ NAGR)
        (SETQ :RESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M NOT) (GO FAIL)))
        (SETQ :RESULT (or (PARSE nil TO) (CQ TODEL)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (M TO) (GO FAIL)) (:else (M TO) (GO FAIL)))))
        (SETQ TENSE '(INFINITIVE))
        (GO MODAL2)
EN
        (and LABELTRACE (PASSING 'EN))
        (FQ NAGR)
        (SETQ :RESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M NOT) (GO FAIL)))
        (SETQ TENSE '(PAST))
        (SETQ :RESULT (and (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)))
        (COND (:RESULT (GO RETSM)) (:else (GO FAIL)))
ING
        (and LABELTRACE (PASSING 'ING))
        (FQ NAGR)
        (SETQ :RESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M NOT) (GO FAIL)))
INGADV
        (and LABELTRACE (PASSING 'INGADV))
        (SETQ :RESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (GO INGADV)))
        (SETQ TENSE '(PRESENT))
        (GO BE2)
IMPER
        (and LABELTRACE (PASSING 'IMPER))
        (SETQ :RESULT (and (PARSE VB DO NEG INF) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M DONT) (GO FAIL)))
        (SETQ :RESULT (and (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG))))
        (COND (:RESULT (GO RETURN)) (:else (M IMPER) (GO FAIL)))
POLR2
        (and LABELTRACE (PASSING 'POLR2))
        (or (SETQ PT (GETR 'QAUX (MOVE-PT C U))) (and (BUG "VG:POLR2") (GO FAIL)))
        (SETQ H (list (car PT)))
        (TRNSF NEG)
        (COND ((ISQ H DO) (GO DO))
            ((ISQ H MODAL) (GO MODAL))
            ((ISQ H WILL) (GO WILL))
            ((ISQ H BE) (GO BE))
            ((ISQ H HAVE) (GO HAVE)))
        (ERT BUG "VG:POLR2VB")
        (GO FAIL)
DO
        (and LABELTRACE (PASSING 'DO))
        (FQ DO)
        (MOVE-PT C DLC)
        (TRNSF VPL NEG INF V3PS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (COND (NN (GO DO2)) (:else (GO MVB)))
DO2
        (and LABELTRACE (PASSING 'DO2))
        (SETQ :RESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M NOT) (GO FAIL)))
ADV2
        (and LABELTRACE (PASSING 'ADV2))
        (SETQ :RESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV2)))))
        (SETQ :RESULT (PARSE VB (MVB) INF))
        (COND ((nil? :RESULT) (GO MVB)))
        (GO REV)
MODAL
        (and LABELTRACE (PASSING 'MODAL))
        (FQ NAGR MODAL)
        (SETQ TENSE '(MODAL))
        (COND (NN (GO MODAL2)) (:else (GO INCOMP)))
MODAL2
        (and LABELTRACE (PASSING 'MODAL2))
        (SETQ :RESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M NOT) (GO FAIL)))
ADV3
        (and LABELTRACE (PASSING 'ADV3))
        (SETQ :RESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV3)))))
        (COND ((PARSE VB BE INF) (GOCOND BE2 MVB))
            ((PARSE VB HAVE INF) (GOCOND HAV2 MVB))
            ((PARSE VB INF (MVB)) (GO REV))
            (:else (GO INCOMP)))
WILL
        (and LABELTRACE (PASSING 'WILL))
        (FQ NAGR)
        (SETQ TENSE '(FUTURE))
        (COND (NN (GO MODAL2)) (:else (GO INCOMP)))
BE
        (and LABELTRACE (PASSING 'BE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (:else '(PRESENT))))
        (COND (NN (GO BE2)) (:else (GO MVB)))
BE2
        (and LABELTRACE (PASSING 'BE2))
        (SETQ :RESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M NOT) (GO FAIL)))
ADV4
        (and LABELTRACE (PASSING 'ADV4))
        (SETQ :RESULT (or (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV4)))))
        (COND ((and (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))
            ((and (NQ BE) (PARSE VB ING)) (SETQ TENSE (cons 'PRESENT TENSE)) (GO EN2))
            ((and (NQ ING) (PARSE VB ING (MVB))) (SETQ TENSE (cons 'PRESENT TENSE)) (GO REV))
            ((CQ ING) (MQ ING) (GO FAIL)))
EN2
        (and LABELTRACE (PASSING 'EN2))
        (SETQ :RESULT (PARSE VB EN (MVB)))
        (COND ((nil? :RESULT) (GO MVBE)))
        (FQ PASV)
        (GO REV)
GOING
        (and LABELTRACE (PASSING 'GOING))
        (SETQ :RESULT (PARSE nil TO))
        (COND ((nil? :RESULT) (GO GOI)))
        (SETQ :RESULT (NQ INF))
        (COND (:RESULT (GO GOING2)))
        (POP)
GOI
        (and LABELTRACE (PASSING 'GOI))
        (SETQ TENSE (cons 'PRESENT TENSE))
        (GO MVB)
GOING2
        (and LABELTRACE (PASSING 'GOING2))
        (SETQ TENSE (cons 'FUTURE TENSE))
        (GO MODAL2)
MVBE
        (and LABELTRACE (PASSING 'MVBE))
        (SETQ :RESULT (ISQ (MOVE-PT H PV (VB)) AUX))
        (COND ((nil? :RESULT) (GO MVB)))
        (SETQ :RESULT (ISQ PT BE))
        (COND ((nil? :RESULT) (M MVBE) (GO FAIL)))
        (SETMVB PT)
        (GO REV)
HAVE
        (and LABELTRACE (PASSING 'HAVE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST)) (:else '(PRESENT))))
        (COND (NN (GO HAV2)) (:else (GO MVB)))
HAV2
        (and LABELTRACE (PASSING 'HAV2))
        (SETQ :RESULT (and (PARSE nil NOT) (FQ NEG)))
        (COND ((and (nil? NN) :RESULT) (M NOT) (GO FAIL)))
ADV5
        (and LABELTRACE (PASSING 'ADV5))
        (SETQ :RESULT (PARSE ADV))
        (COND (:RESULT (COND ((nil? NN) (M ADV) (GO FAIL)) (:else (GO ADV5)))))
        (SETQ :RESULT (PARSE VB BE EN))
        (COND ((nil? :RESULT) (GO HAV3)))
        (SETQ TENSE (cons 'PAST TENSE))
        (COND (NN (GO BE2)) (:else (GO MVB)))
HAV3
        (and LABELTRACE (PASSING 'HAV3))
        (SETQ :RESULT (PARSE VB (MVB) EN))
        (COND ((nil? :RESULT) (GO MVB)))
        (SETQ TENSE (cons 'PAST TENSE))
        (GO REV)
INCOMP
        (and LABELTRACE (PASSING 'INCOMP))
        (FQ INCOMP)
        (GO FAIL)
MVB
        (and LABELTRACE (PASSING 'MVB))
        (SETQ :RESULT (EQ (FE MVB) (FE H)))
        (COND (:RESULT (GO MVB2)))
        (POP VB)
        (SETQ :RESULT (PARSE VB (MVB)))
        (COND ((nil? :RESULT) (M MVB) (GO FAIL)))
MVB2
        (and LABELTRACE (PASSING 'MVB2))
        (GO REV)
REV
        (and LABELTRACE (PASSING 'REV))
        (SETR 'TENSE TENSE C)
        (and NN (PARSE nil NOT) (FQ NEG))
        (COND ((or (EQUAL TENSE '(PAST)) (CQ NAGR) (ISQ (MOVE-PT C U) IMPER) (ISQ PT THERE) (ISQ PT RSNG)) (GO NAUX))
            ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))
            (:else (ERTERR VG -- NO SUBJECT TO CHECK FOR AGREEMENT)))
        (SETQ T3 nil)
        (COND ((ISQ PT NFS) (or (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))
            ((ISQ PT CLAUSE) (or (SETQ T3 (CQ V3PS)) (GO NAGR)))
            ((or (ISQ PT NS) (ISQ PT MASS)) (or (and (CQ V3PS) (SETQ T3 true)) (FESET PT (SETDIF (FE PT) '(NS MASS))))))
        (COND ((or (ISQ PT PART) (ISQ PT NPL)) (or (and (MEET FE '(INF VPL)) (SETQ T3 true)) (FESET PT (SETDIF (FE PT) '(PART NPL))))))
NAGR
        (and LABELTRACE (PASSING 'NAGR))
        (SETQ :RESULT (or T3 (and (EQUAL '(PAST-PRESENT) TENSE) (SETQ TENSE '(PAST)))))
        (COND ((nil? :RESULT) (M NAGR) (GO FAIL)))
NAUX
        (and LABELTRACE (PASSING 'NAUX))
        (SETMVB (or (MOVE-PT H PV (MVB)) MVB))
        (SETQ :RESULT
        (and (CQ NAUX) (ISQ (MOVE-PT H PV (VB)) AUX) (not (MOVE-PT PV PV (VB)))))
        (COND (:RESULT (M NAUX) (GO FAIL)) (:else (GO RETSM)))
POPV
        (and LABELTRACE (PASSING 'POPV))
        (ERT POPV)
        (GO FAIL)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (SETQ :RESULT (CALLSM (SMVG)))
        (COND (:RESULT (GO RETURN)) (:else (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ DEFUN PREPG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil :RESULT nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-PREPG
        (and LABELTRACE (PASSING 'ENTERING-PREPG))
ADV
        (and LABELTRACE (PASSING 'ADV))
        (SETQ :RESULT (and (NQ PREPADV) (PARSE ADV PREPADV)))
        (COND (:RESULT (COND ((nil? NN) (M PREPADV) (GO FAIL)) (:else (GO ADV)))))
        (SETQ :RESULT (COND ((CQ AGENT) (NEXTWORD? 'BY)) ((CQ LOC) (NQ PLACE)) ((CQ Q) (not (NQ MOTOR))) (:else true)))
        (COND ((nil? :RESULT) (M PREP) (GO FAIL)))
        (SETQ :RESULT (PARSE PREP))
        (COND ((nil? :RESULT) (M PREP) (GO FAIL)))
        (MOVE-PT H)
        (TRNSF PLACE TIME)
        (SETQ T1 H)
        (and (NQ PREP2)
            (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N))) (PARSE PREP2))
                ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N) (WORD (cdr N)))) (PARSE PREP2) (PARSE PREP2)))
            (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))
            (SETR 'PARENT C T1))
        (SETQ :RESULT (ISQ H NEED2))
        (COND (:RESULT (M NEED2) (GO FAIL)))
        (SETR 'HEAD T1 C)
        (or NN (GO SHORT))
        (COND ((EQ (WORD H) 'BY) (FQ AGENT)))
QUEST
        (and LABELTRACE (PASSING 'QUEST))
        (SETQ :RESULT (CQ QUEST))
        (COND ((nil? :RESULT) (GO NG)))
        (SETQ :RESULT (PARSE NG QUEST OBJ))
        (COND (:RESULT (GO OBJR)) (:else (M PREPQUEST) (GO FAIL)))
        (SETQ :RESULT (and (CQ OF) (PARSE NG OFOBJ)))
        (COND (:RESULT (GO OBJR)))
NG
        (and LABELTRACE (PASSING 'NG))
        (SETQ :RESULT (PARSE NG OBJ))
        (COND (:RESULT (GO OBJR)))
REL
        (and LABELTRACE (PASSING 'REL))
        (SETQ :RESULT (NEXTWORD? 'WHICH))
        (COND ((nil? :RESULT) (GO REST)))
        (SETQ :RESULT (ISQ (MOVE-PT U) CLAUSE))
        (COND ((nil? :RESULT) (M PREP-WHICH) (GO FAIL)))
        (SETQ :RESULT (ISQ PT PRONREL))
        (COND ((nil? :RESULT) (GO PRONREL)))
        (SETQ MES (cdr MES))
        (GO P-RELWRD)
PRONREL
        (and LABELTRACE (PASSING 'PRONREL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PRONREL PT)
P-RELWRD
        (and LABELTRACE (PASSING 'P-RELWRD))
        (PARSE NG RELWD OBJ)
        (SETR 'OBJ1 (GETR 'HEAD PT) C)
        (GO RETT)
REST
        (and LABELTRACE (PASSING 'REST))
        (SETQ :RESULT (PARSE CLAUSE RSNG ING))
        (COND (:RESULT (GO OBJR)) (:else (GO SHORT)))
OBJR
        (and LABELTRACE (PASSING 'OBJR))
        (SETR 'OBJ1 H C)
        (GO RETT)
SHORT
        (and LABELTRACE (PASSING 'SHORT))
        (SETQ :RESULT (MEET FE '(NOSHORT Q)))
        (COND (:RESULT (M SHORT) (GO FAIL)))
        (or (ISQ (MOVE-PT C U) REL-NOT-FOUND) (ISQ (GETR 'QUESTION-ELEMENT PT) QADJ) (GO FAIL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PREPREL PT)
        (SETR 'OBJ1 (GETR 'RELHEAD (MOVE-PT C U)) C)
RETT
        (and LABELTRACE (PASSING 'RETT))
        (and (or (ISQ H QUEST) (and (ISQ H COMPOUND) (MOVE-PT H H PV (QUEST)))) (FQ QUEST))
        (SETQ :RESULT (CALLSM (SMADJG-PREPG)))
        (COND (:RESULT (GO RETURN)) (:else (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ DEFUN ADJG []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil :RESULT nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-ADJG
        (and LABELTRACE (PASSING 'ENTERING-ADJG))
COMPCHECK
        (and LABELTRACE (PASSING 'COMPCHECK))
        (SETQ :RESULT (and (MOVE-PT C U (BE)) (not (CQ COMP))))
        (COND (:RESULT (GO FAIL)))
        (SETQ :RESULT (ISQ (MOVE-PT C U) THAN))
        (COND ((nil? :RESULT) (GO DISP)))
        (SETR 'HEAD (GETR 'COMPARATIVE-MODIFIER PT) C)
        (GO THAN)
DISP
        (and LABELTRACE (PASSING 'DISP))
        (SETQ :RESULT (and (NQ AS) (PARSE nil AS)))
        (COND (:RESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO AS)))))
        (SETQ :RESULT (and (NQ AS) (PARSE nil AS)))
        (COND (:RESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO AS)))))
        (SETQ :RESULT (NEXTWORD? 'HOW))
        (COND (:RESULT (GO HOW)) (:else (GO ADV)))
HOW
        (and LABELTRACE (PASSING 'HOW))
        (SETQ :RESULT (and (PARSE nil HOW) (FQ QUEST)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO FAIL)) (:else (GO FAIL)))))
        (SETQ :RESULT (and (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C)))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (and (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C)))
        (COND (:RESULT (GO RETSM)) (:else (GO FAIL)))
ADV
        (and LABELTRACE (PASSING 'ADV))
        (SETQ :RESULT (PARSE ADV ADVADV))
        (COND (:RESULT (COND ((nil? NN) (GO POPAD)) (:else (GO ADV)))))
        (SETQ :RESULT (PARSE nil MORE))
        (COND ((nil? :RESULT) (GO ADJ)))
        (FQ COMPAR)
ADJ
        (and LABELTRACE (PASSING 'ADJ))
        (SETQ :RESULT (COND ((CQ ADV) (PARSE ADV VBAD)) (:else (PARSE ADJ))))
        (COND ((nil? :RESULT) (M ADJ) (GO FAIL)))
        (SETQ :RESULT (SETR 'HEAD H C))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (or (CQ COMPAR) (ISQ H COMPAR)))
        (COND ((nil? :RESULT) (GO RETSM)))
        (FQ COMPAR)
        (SETQ :RESULT NN)
        (COND ((nil? :RESULT) (GO RETSM)))
THAN
        (and LABELTRACE (PASSING 'THAN))
        (COND ((not NN) (GO RETSM)))
        (SETQ :RESULT (PARSE nil THAN))
        (COND ((nil? :RESULT) (COND ((nil? NN) (M THAN) (GO FAIL)) (:else (GO RETSM)))))
        (RQ THANNEED)
        (FQ THAN)
        (GO SUBJ)
AS
        (and LABELTRACE (PASSING 'AS))
        (FQ AS)
        (RQ THANNEED)
        (SETQ :RESULT (and (PARSE ADJ) (SETR 'HEAD H C)))
        (COND ((nil? :RESULT) (COND ((nil? NN) (GO RETSM)) (:else (M ADJ) (GO FAIL)))))
        (SETQ :RESULT (PARSE nil AS))
        (COND (:RESULT (COND ((nil? NN) (M AS) (GO FAIL)) (:else (GO SUBJ)))) (:else (GO RETSM)))
SUBJ
        (and LABELTRACE (PASSING 'SUBJ))
        (SETQ :RESULT (PARSE NG SUBJ COMPAR))
        (COND ((nil? :RESULT) (M THAN) (GO FAIL)))
        (SETQ :RESULT (SETR 'OBJ1 H C))
        (COND ((and (nil? NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (and (ONE-WORD-LEFT) (PARSE VB AUX)))
        (COND ((nil? :RESULT) (GO RETSM)))
        (SETQ :RESULT (CHECK-AGREEMENT H (cdr H)))
        (COND (:RESULT (GO RETSM)))
        (POP)
        (GO RETSM)
POPAD
        (and LABELTRACE (PASSING 'POPAD))
        (POP)
        (GO ADJ)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (SETQ :RESULT (CQ THANNEED))
        (COND (:RESULT (M THANNEED) (GO FAIL)))
        (SETQ :RESULT (CALLSM (SMADJG-PREPG)))
        (COND (:RESULT (GO RETURN)) (:else (M SMADJ) (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ DEFUN CONJ []
    (let [END nil GOODIE nil]
        (SETQ END CUT)
        (COND ((SETQ GOODIE (APPLY-GRAMMAR 'CONJOIN)) (RETURN (SETQ RE GOODIE))) (:else (RETURN nil)))))

(§ DEFUN COMMA []
    (COND ((SECONDWORD? '\") (FLUSHME) true) ((CONJ)) ((ISQ RE INIT) (FLUSHME) true)))

(§ DEFUN CONJOIN []
    (let [FE nil H nil ME nil NB nil C nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil :RESULT nil PREV nil]
        (SETQ NN true)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (reverse REST)) (SETQ NB (or (NB RE) N)) N (SETQ H RE) nil))
        (SETR 'PARENT PARENT C)
ENTERING-CONJOIN
        (and LABELTRACE (PASSING 'ENTERING-CONJOIN))
UP
        (and LABELTRACE (PASSING 'UP))
        (SETQ PREV (NEXTWORD))
        (FLUSHME)
        (COND ((and (EQ PREV '\,) (or (cdr H) (> (- (count (NB H)) (count (N H))) 4)) (MEMQ (NEXTWORD) '(or AND NOR BUT)) (F (NEXTWORD)))
            (SETQ PREV (list PREV (NEXTWORD)))
            (FLUSHME)))
        (and (ATOM PREV) (MOVE-PTW N NW (EQ (WORD PTW) PREV)) (CUT PTW))
        (and (or (EQ PREV 'BUT) (EQ (cadr PREV) 'BUT)) (NEXTWORD? 'NOT) (or (FLUSHME) (GO LOSE2)) (FQ NEGBUT))
        (SETQ :RESULT (COND
            ((MEMQ (car REST) '(ADJ NUM NOUN PREP VB ADV)) (PARSE3 (concat REST '(COMPONENT)) nil))
            ((MEMQ (car REST) '(NG PREPG ADJG)) (and (not (CQ OFOBJ)) (PARSE2 (concat REST '(COMPONENT)) nil)))
            ((EQ (car REST) 'CLAUSE)
                ((lambda (LASTSENT AUXFE)
                    (and (PARSE2 (concat REST AUXFE '(COMPONENT)) nil) (or (not AUXFE) (F (car AUXFE))) (SETR 'TIME (GETR 'TIME H) C)))
                (COND ((ISQ H MAJOR) H) (LASTSENT))
                (MEET (FE H) '(DECLAR IMPER))))))
        (COND ((nil? :RESULT) (GO LOSE2)))
        (CUT END)
        (COND ((not (ATOM PREV)) (GO RETSM))
            ((EQ PREV '\,) (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP)) (:else (GO LIST))))
            ((MEMQ PREV '(and OR NOR BUT))
                (COND ((EQ BOTH (NB H)) (FQ BOTH)))
                (COND ((or (NEXTWORD? 'BUT) (and (NEXTWORD? PREV) (not (and (EQ BOTH (NB H)) (EQ PREV 'AND)))))
                        (FQ LISTA)
                        (F PREV)
                        (GO UP))
                    (:else (GO LISTA)))))
LOSE2
        (and LABELTRACE (PASSING 'LOSE2))
        (SETQ :RESULT (CQ LISTA))
        (COND (:RESULT (GO LISTA)))
LIST
        (and LABELTRACE (PASSING 'LIST))
        (SETQ :RESULT (and (EQ PREV '\,) (== (count H) 2) (ISQ H NG) (not (or (ISQ H PRONG) (ISQ (cdr H) PRONG))) (or (NEXTWORD? COMMA) (nil? N))))
        (COND ((nil? :RESULT) (M CONJOINß) (GO FAIL)))
        (FLUSHME)
        (FQ APPOSITIVE)
        (GO RETSM)
LISTA
        (and LABELTRACE (PASSING 'LISTA))
        (F PREV)
RETSM
        (and LABELTRACE (PASSING 'RETSM))
        (FQ COMPOUND)
        (and (> (count H) 2) (FQ LIST))
        (COND ((or (CQ NG) (CQ NOUN)) (COND ((CQ AND) (FQ NPL)) (:else (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
            ((CQ VB) (let [COMMON nil] (SETQ COMMON (GET 'VB 'ELIM)) (MAP #'(lambda (X) (SETQ COMMON (MEET COMMON (FE X)))) H) nil) (FESET (UNION COMMON (FE C)) C)))
        (SETQ :RESULT (CALLSM (SMCONJ)))
        (COND (:RESULT (GO RETURN)) (:else (M CONJOINß) (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (or (N RE) NB))
        (RETURN nil)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (reverse FE) NB N H SM C))))

(§ DEFUN BOTH FEXPR [A]
    (let [END nil]
        (SETQ END CUT)
        (RETURN (let [CUT nil NBB nil BOTH nil]
            (SETQ NBB N)
            (and (FLUSHME)
                (MOVE-PTW N NW (EQ (WORD PTW) (car A)) NW)
                (CUT END)
                (SETQ BOTH PTW)
                (SETQ RE (COND ((MEMQ (car REST) '(PREP ADV)) (PARSE3 REST true)) ((MEMQ (car REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST true))))
                (< (count N) (count BOTH))
                (RETURN (SETQ SPECIAL 'SKIP)))
            (SETQ RE nil)
            (SETQ N NBB)
            (RETURN nil)))))

(§ DEFUN DOUBLEQUOTER [] (APPLY-GRAMMAR 'PARSEQUOTED))

(§ DEFUN CANTAKE [NUM TYPE FEATURE]
    (let [VBFEAT nil]
        (SETQ VBFEAT (FE MVB))
        (RETURN (COND
            ((MEMQ 'RSNG TYPE)
                (MEMQ (READLIST (concat
                        (COND ((MEMQ 'TO TYPE) '(T O)) ((MEMQ 'ING TYPE) '(I N G)) ((MEMQ 'REPORT TYPE) '(R E P)))
                        '(O B)
                        (list (COND ((EQ NUM 1) '\1) (:else '\2)))))
                    VBFEAT))
            ((MEMQ 'COMP TYPE) (MEMQ 'INT VBFEAT))
            ((MEMQ 'NG TYPE)
                (COND ((== NUM 1)
                        (not (nil? (MEET '(TRANS TRANS2 TRANSL TRANSINT) VBFEAT))))
                    (:else (MEMQ 'TRANS2 VBFEAT))))
            (:else (MEMQ FEATURE VBFEAT))))))

(§ DEFUN CANPARSE [NUM TYPE FEATURE]
    (let [REG nil]
        (and (CANTAKE NUM TYPE FEATURE)
            (or (nil? TYPE)
                (and (APPLY #'PARSE (concat TYPE
                    (COND ((MEMQ 'COMP TYPE) (SETQ REG 'COMP) nil)
                        (:else (list 'OBJ (SETQ REG (COND ((or (MEMQ 'LOC TYPE) (MEMQ 'PLACE TYPE)) 'LOBJ) ((== NUM 1) 'OBJ1) (:else 'OBJ2))))))))
                (SETR REG H C)))
            (or (nil? FEATURE) (F FEATURE))
            (RETURN true))
        nil))

#_(ns shrdlu.loader)

;; THIS IS A PACKAGE FOR LOADING SHRDLU'S INTO CORE FROM THE DISK FILES.
;; THE PROCEDURE IS TO FIRST LOAD A BLISP (IGNORE ALLOCATIONS, THE PROGRAMS DO THEIR OWN), AND UREAD THIS FILE.
;; EXECUTING "LOADSHRDLU" WILL GENERATE (AFTER SOME TIME) A FULLY INTERPRETED VERSION.
;; PARTIALLY COMPILED MIXES ARE AVAILABLE, AS SEEN BELOW.
;; IF ANY ERRORS OCCUR DURING READIN, THEY ARE PROTECTED BY AN "ERRSET" AND LOADING CONTINUES.
;; NOTE IF AN UNBOUND PAREN CAUSES THE FILE TO BE TERMINATED TOO SOON, YOU'LL NEVER NOTICE.

(§ DEFUN LOADER [*!?KEY]
    (or (ERRSET (eval (list 'UREAD *!?KEY '> 'DSK 'SHRDLU)) nil)
        (and (PRINT *!?KEY) (PRINC 'NOT-FOUND) (RETURN nil)))
    (LOADX))

(§ DEFUN LOADX []
    (let [*!?H nil *!?F nil *!?EOF nil]
        (SETQ *!?EOF (GENSYM))
        (PRINT 'READING)
        (PRINC *!?KEY)
    =>  ((lambda (ß_Q) (SETQ *!?H (READ *!?EOF))) true)
        (and (EQ *!?H *!?EOF) (RETURN true))
        (or (ERRSET ((lambda (ß_W ß_Q) (eval *!?H)) true true))
            (do (PRINT 'ERROR-IN-FILE) (PRINT *!?H)))
        (GO =>)))

(§ DEFUN LOADSHRDLU []
    (dorun (map #'LOADER '(PLNR THTRAC)))
    (THINIT)
    (SETQ THINF nil THTREE nil THLEVEL nil)
    (SETQ ERRLIST nil) ;; REMOVES MICRO-PLANNER'S FANGS
    (dorun (map #'LOADER '(SYSCOM MORPHO SHOW PROGMR GINTER GRAMAR DICTIO SMSPEC SMASS SMUTIL NEWANS BLOCKS DATA SETUP)))
    'OK)


(ns shrdlu.core)

(defmacro § [& _])
(defmacro ß [& _])

(defmacro def- [s i] `(def ~(vary-meta s assoc :private true) ~i))

#_(ns shrdlu.blockp)

;; ################################################################
;;
;;     BLOCKP > MICRO-PLANNER CODE FOR THE "BLOCKS" MICRO-WORLD
;;
;; ################################################################

(DEFPROP TA-AT
    (THANTE (X Y) (#AT $?X $?Y) (THRPLACA (CDR (ATAB $?X)) $?Y))
    THEOREM)

(DEFPROP TA-CONTAIN
    (THANTE (X Y Z)
        (#AT $?X ?)
        (THGOAL (#MANIP $?X))
        (THGOAL (#SUPPORT $?Y $?X))
        (THOR (THAND (THGOAL (#IS $?Y #BOX)) (THVSETQ $_Z $?Y))
            (THGOAL (#CONTAIN $?Z $?Y)))
        (THASSERT (#CONTAIN $?Z $?X)))
    THEOREM)

(DEFPROP TA-EXISTS (THANTE (X) (#EXISTS $?X) (THSUCCEED)) THEOREM)

(DEFPROP TA-SUPP
    (THANTE (X Y Z)
        (#AT $?X $?Y)
        (THCOND ((THVSETQ $_Z (SUPPORT $?Y (SIZE $?X) $?X))
            (THCOND ((THGOAL (#MANIP $?Z))
                (THGOAL (#SHAPE $?Z #RECTANGULAR)))
                ((THSUCCEED)))
            (THASSERT (#SUPPORT $?Z $?X))
            (THCOND ((THGOAL (#CLEARTOP $?Z))
                (THERASE (#CLEARTOP $?Z)))
                ((THSUCCEED)))
            (THCOND (NOSTACKS)
                ((THNOT (THGOAL (#MANIP $?Z))))
                ((THAND (THGOAL (#PART $?Z $_Y))
                    (THGOAL (#IS $?Y #STACK)))
                (THASSERT (#PART $?X $?Y)))
                ((THVSETQ $_Y (MAKESYM 'STACK))
                (THASSERT (#PART $?X $?Y))
                (THASSERT (#PART $?Z $?Y))
                (THASSERT (#IS $?Y #STACK))
                (THASSERT (#EXISTS $?Y) (THUSE TA-EXISTS)))))
            ((THGOAL (#GRASPING $?X)))
            ((ERT TA-SUPP))))
    THEOREM)

(DEFPROP TC-2
    (THCONSE (X Y YY)
        ($?X $?Y)
        (THGOAL (#CHOOSE $?Y $_YY $E (GET $?X 'CHOOSE))
            (THUSE TC-CHOOSE))
        (THGOAL ($?X $?YY) (THTBF THTRUE)))
    THEOREM)

(DEFPROP TC-3
    (THCONSE (X Y Z YY ZZ)
        ($?X $?Y $?Z)
        (THGOAL (#CHOOSE $?Y $_YY $E (GET $?X 'CHOOSE))
            (THUSE TC-CHOOSE))
        (THGOAL (#CHOOSE $?Z $_ZZ $E (GET $?X 'CHOOSE2))
            (THUSE TC-CHOOSE))
        (THGOAL ($?X $?YY $?ZZ) (THTBF THTRUE)))
    THEOREM)

(DEFPROP TC-ASMUCH
    (THCONSE (MEASURE X Y)
        (#ASMUCH MEASURE $?X $?Y)
        (THVSETQ $_MEASURE (GET $?MEASURE 'MEASFN))
        (NOT (LESSP ($?MEASURE $?X) ($?MEASURE $?Y))))
    THEOREM)

(DEFPROP TC-BELONG
    (THCONSE (X Y)
        (#BELONG $?X $?Y)
        (THAMONG $?Y '(:SHRDLU))
        (THGOAL (#PHYSOB $?X) (THUSE TC-PHYSOB)))
    THEOREM)

(DEFPROP TC-CALL
    (THCONSE (X Y Z)
        (#CALL $?X $?Y)
        (THCOND ((THGOAL (#CALL $_Z $?Y))
            (PRINT $?Y)
            (NOT (PRINT 'NAME-ALREADY-USED)))
            ((THASSERT (#CALL $?X $?Y))
            (THASSERT (#IS $?Y #NAME))
            (#PROPDEFINE $?Y)
            (OR DOIT (SETQ PLAN (CONS T PLAN))))))
    THEOREM)

(DEFPROP TC-CHOOSE
    (THCONSE (X Y Z W)
        (#CHOOSE $?X $?Y $?Z)
        (THCOND
            ((AND (THASVAL $?X) (NOT (OSS? $?X))) (THSETQ $_Y $?X))
            ((THASVAL $?X)
                (OR (NULL DISCOURSE)
                (THPUTPROP (VARIABLE? $?X) $?X 'NG))
                (THSETQ $_Y (FINDCHOOSE $?X $?Z NIL)))
        ((THGOAL (#MANIP $?Y)) (THNOT (THGOAL (#SUPPORT $?Y ?))))))
    THEOREM)

(DEFPROP TC-CHOOSEPLACE
    (THCONSE (X) (#CHOOSEPLACE $?X) (ERT CHOOSEPLACE UNDEFINED))
    THEOREM)

(DEFPROP TC-CLEARTOP
    (THCONSE (X Y (WHY (EV)) EV)
        (#CLEARTOP $?X)
        (ATOM $?X)
        (THOR (THGOAL (#SUPPORT $?X ?))
        (THAND (THASSERT (#CLEARTOP $?X)) (THSUCCEED THEOREM)))
        (MEMORY)
    =>  (THCOND ((THGOAL (#SUPPORT $?X $_Y))
            (THGOAL (#GET-RID-OF $?Y)
                (THNODB)
                (THUSE TC-GET-RID-OF))
            (THGO =>))
            ((THASSERT (#CLEARTOP $?X))
            (MEMOREND (#CLEARTOP $?EV $?X))
            (THSUCCEED THEOREM))))
    THEOREM)

(DEFPROP TC-EXISTS (THCONSE (X) (#EXISTS $?X) (THSUCCEED)) THEOREM)

(DEFPROP TC-FINDSPACE
    (THCONSE (SURF SIZE OBJ SPACE)
        (#FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE)
        (THOR (AND (NOT (MEMQ $?SURF '(:BOX :TABLE))) (NOT (GET '#NOCLEAR 'THASSERTION))
            (THSETQ $_SPACE (FINDSPACE 'CENTER $?SURF $?SIZE $?OBJ)))
        (AND (OR (EQ $?SURF ':BOX) (AND (NOT (EQ $?SURF ':TABLE)) (GET '#NOCLEAR 'THASSERTION)))
            (THSETQ $_SPACE (FINDSPACE 'PACK $?SURF $?SIZE $?OBJ)))
        (THSETQ $_SPACE (FINDSPACE 'RANDOM $?SURF $?SIZE $?OBJ))))
    THEOREM)

(DEFPROP TC-GET-RID-OF
    (THCONSE (X Y (WHY (EV)) EV)
        (#GET-RID-OF $?X)
        (OR NOMEM (THVSETQ $_EV $?WHY))
    =>  (THCOND ((NULL $?X))
            ((ATOM $?X)
                (MEMORY)
                (THGOAL (#FINDSPACE :TABLE $E (SIZE $?X) $?X $_Y) (THUSE TC-FINDSPACE))
                (THGOAL (#PUT $?X $?Y) (THNODB) (THUSE TC-PUT))
                (MEMOREND (#GET-RID-OF $?EV $?X)))
            ((THGOAL (#GET-RID-OF $E (CAR $?X)) (THUSE TC-GET-RID-OF))
                (OR (THSETQ $_X (CDR $?X)) (THSUCCEED THEOREM))
                (THGO =>))))
    THEOREM)

(DEFPROP TC-GRASP
    (THCONSE (X Y (WHY (EV)) EV)
        (#GRASP $?X)
        (THCOND ((THGOAL (#GRASPING $?X)) (THSUCCEED THEOREM))
            ((ATOM $?X)))
        (MEMORY)
        (THGOAL (#CLEARTOP $?X) (THUSE TC-CLEARTOP))
        (THCOND ((THGOAL (#GRASPING $_Y))
            (THOR (THGOAL (#UNGRASP) (THNODB) (THUSE TC-UNGRASP))
                (THGOAL (#GET-RID-OF $?Y) (THNODB) (THUSE TC-GET-RID-OF))))
            ((THSUCCEED)))
        (THSETQ $_Y (TOPCENTER $?X))
        (THGOAL (#MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2))
        (THASSERT (#GRASPING $?X))
        (MEMOREND (#GRASP $?EV $?X))
        (OR NOMEM
            (THSETQ GRASPLIST (CONS (LIST THTIME $?X) GRASPLIST)))
        (THCOND (DOIT (THOR (GRASP $?X) (AND (UNGRASP) NIL)))
            ((THSETQ PLAN (CONS (LIST 'GRASP (LIST 'QUOTE $?X)) PLAN)))))
    THEOREM)

(DEFPROP TC-LOC
    (THCONSE (X Y Z LOC)
        ($?LOC $?X $?Y $?Z)
        (THOR (THGOAL (#MANIP $?Y)) (THGOAL (#IS $?Y #BOX)))
        (THOR (THGOAL (#MANIP $?Z)) (THGOAL (#IS $?Z #BOX)))
        (NOT (EQ $?Y $?Z))
        (LOCGREATER $?Y $?Z
            ((LAMBDA (X) (COND ((EQ X '#RIGHT) 'CAR) ((EQ X '#BEHIND) 'CADR) ((EQ X '#ABOVE) 'CADDR) ((ERT TC-LOC)))) $?X)))
    THEOREM)

(DEFPROP TC-MAKESPACE
    (THCONSE (SURF SIZE OBJ SPACE X (WHY (EV)) EV)
        (#FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE)
        (THNOT (THGOAL (#IS $?SURF #BOX)))
        (MEMORY)
    => (THAND (THGOAL (#SUPPORT $?SURF $_X))
            (THGOAL (#GET-RID-OF $?X) (THUSE TC-GET-RID-OF)))
        (THOR (THGOAL (#FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE) (THUSE TC-FINDSPACE))
            (THGO =>))
        (MEMOREND (#MAKESPACE $?EV $?SURF)))
    THEOREM)

(DEFPROP TC-MORE
    (THCONSE (MEASURE X Y)
        (#MORE $?MEASURE $?X $?Y)
        (THVSETQ $_MEASURE (GET $?MEASURE 'MEASFN))
        (GREATERP ($?MEASURE $?X) ($?MEASURE $?Y)))
    THEOREM)

(DEFPROP TC-MOVEHAND
    (THCONSE (X Y W Z)
        (#MOVEHAND $?Y)
        (THCOND
            ((EQUAL HANDAT $?Y) (THSUCCEED THEOREM))
            ((THGOAL (#GRASPING $?X))
                (THVSETQ $_Z (PROG (X Y)
                    (SETQ X (ATAB $?X))
                    (AND (CLEAR (SETQ Y (DIFF $?Y (TCENT '(0 0 0) (CADDR X))))
                            (LIST (CAADDR X) (CADADR (CDR X)) (DIFFERENCE 512 (CADDR Y)))
                            (CAR X))
                        (RETURN Y))))
                (THGOAL (#AT $?X $_W))
                (THERASE (#AT $?X $?W) (THUSE TE-SUPP TE-CONTAIN))
                (THASSERT (#AT $?X $?Z) (THUSE TA-AT TA-SUPP TA-CONTAIN))
                (THGOAL (#MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2))
                (OR NOMEM
                    (THPUTPROP $?X
                        (CONS (LIST THTIME
                                $?Z
                                (CADAR (OR (THVAL '(THGOAL (#SUPPORT $?Y $?X))
                                        (CONS (LIST 'Y 'THUNASSIGNED) THALIST))
                                    '((NIL :HAND))))
                                NIL)
                            (GET $?X 'HISTORY))
                        'HISTORY)))
        ((THGOAL (#MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2)))))
    THEOREM)

(DEFPROP TC-MOVEHAND2
    (THCONSE (Y LOC)
        (#MOVEHAND2 $?Y)
        (COND ((EQUAL $?Y HANDAT) (THSUCCEED THEOREM))
            ((AND (LESSP 31 (CAR $?Y) 609) (LESSP -1 (CADR $?Y) 609) (LESSP -1 (CADDR $?Y) 513))))
        (THVSETQ $_LOC HANDAT)
        (THSETQ HANDAT $?Y)
        (THSETQ THTIME (ADD1 THTIME))
        (THCOND (DOIT (THOR (EVAL (CONS 'MOVETO HANDAT))
                    (PROG (ADJUST) (EVAL (CONS 'MOVETO $?LOC)))))
            ((THSETQ PLAN (CONS (CONS 'MOVETO $?Y) PLAN)))))
    THEOREM)

(DEFPROP TC-NAME
    (THCONSE (X)
        (#NAME $?X)
        (THVSETQ $_X (LISTIFY $?X))
        (THVSETQ $_X (THFIND ALL $?Y (Y Z) (THAMONG $?Z $?X) (THOR (THGOAL (#CALL $?Z $?Y)) (THSETQ $_Y $?Z))))
        (MAPC 'PRINT $?X))
    THEOREM)

(DEFPROP TC-NOTICE
    (THCONSE (X)
        (#NOTICE $?X)
        (COND (DOIT (BLINK $?X) (THSUCCEED))
            ((THSETQ PLAN (CONS (LIST 'BLINK (LIST 'QUOTE $?X)) PLAN)))))
    THEOREM)

(DEFPROP TC-ON
    (THCONSE (X Y Z)
        (#ON $?X $?Y)
        (THOR (THGOAL (#SUPPORT $?Y $?X)) (THAND (THASVAL $?X) (THGOAL (#SUPPORT $_Z $?X)) (THGOAL (#ON $?Z $?Y) (THUSE TC-ON)))))
    THEOREM)

(DEFPROP TC-PACK
    (THCONSE (OBJ SURF BLOCKS PYR X Y)
        (#PACK $?OBJ $?SURF)
        (OR (THVSETQ $_BLOCKS (PACKO $?OBJ '#BLOCK)) T)
        (OR (THVSETQ $_PYR (PACKO $?OBJ '#PYRAMID)) T)
    =>  (THCOND ((NULL $?BLOCKS)
            (THCOND ((NULL $?PYR) (THSUCCEED THEOREM))
                ((THVSETQ $_Y (FINDSPACE 'PACK $?SURF (SIZE (CAR $?PYR)) (CAR $?PYR)))
                    (THGOAL (#PUT $E (CAR $?PYR) $?Y) (THUSE TC-PUT))
                    (OR (THSETQ $?PYR (CDR $?PYR)) T)
                    (THGO =>))))
                ((THSETQ $_X (CAR $?BLOCKS))
                    (THVSETQ $?Y (FINDSPACE 'PACK $?SURF (SIZE $?X) $?X))
                    (THGOAL (#PUT $?X $?Y) (THUSE TC-PUT))
                    (OR (THSETQ $?BLOCKS (CDR $?BLOCKS)) T)
                    (THCOND ((THVSETQ $_Y (OR (PACKON $?X $?PYR) (PACKON $?X $?BLOCKS)))
                            (THGOAL (#PUTON $?Y $?X) (THUSE TC-PUTON))
                            (COND ((MEMQ $?Y $?PYR)
                                    (THSETQ $_PYR (DELQ $?Y (APPEND $?PYR NIL))))
                                ((THSETQ $_BLOCKS (DELQ $?Y (APPEND $?BLOCKS NIL))))))
                        ((THSUCCEED)))
                    (THGO =>))))
    THEOREM)

(DEFPROP TC-PART
    (THCONSE (X Y Z)
        (#PART $?X $?Y)
        (THGOAL (#IS $?Y #STACK))
        (THGOAL (#CHOOSE $?X $_Z '(((THGOAL (#PART $?* $?Y))))) (THUSE TC-CHOOSE))
        (OR (NOT (ATOM $?Z)) (THSETQ $_Z (LIST $?Z)))
    =>  (THCOND ((NULL $?Z) (THSUCCEED))
            ((THGOAL (#PART $E (CAR $?Z) $?Y))
                (OR (THSETQ $_Z (CDR $?Z)) T)
                (THGO =>))
            ((THFAIL))))
    THCONSE)

(DEFPROP TC-PHYSOB
    (THCONSE (X)
        (#PHYSOB $?X)
        (THOR (THGOAL (#MANIP $?X)) (THAMONG $?X '(:BOX :TABLE :HAND))))
    THEOREM)

(DEFPROP TC-PICKUP
    (THCONSE (X (WHY (EV)) EV)
        (#PICKUP $?X)
        (MEMORY)
        (THGOAL (#GRASP $?X) (THUSE TC-GRASP))
        (THGOAL (#RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
        (MEMOREND (#PICKUP $?EV $?X)))
    THEOREM)

(DEFPROP TC-REFERS
    (THCONSE (X)
        (#REFERS $?X)
        (EVAL (LIST 'THSETQ (LIST 'THV $?X) (LIST 'QUOTE (ATOMIFY (GET $?X 'BIND))))))
    THEOREM)

(DEFPROP TC-PUT
    (THCONSE (X Y Z)
        (#PUT $?X $?Y)
        (THCOND ((THASVAL $?Y)
                (THCOND ((ATOM $?Y) (THGOAL (#CHOOSEPLACE $?Y) (THUSE TC-CHOOSEPLACE)))
                    ((THSUCCEED))))
            ((THGOAL (#GET-RID-OF $?X) (THNODB) (THUSE TC-GET-RID-OF))
                (THSUCCEED THEOREM)))
        (CLEAR $?Y (SIZE $?X) $?X)
        (SUPPORT $?Y (SIZE $?X) $?X)
        (THGOAL (#GRASP $?X) (THUSE TC-GRASP))
        (THSETQ $_Z (TCENT $?Y (SIZE $?X)))
        (THGOAL (#MOVEHAND $?Z) (THNODB) (THUSE TC-MOVEHAND))
        (THGOAL (#UNGRASP) (THNODB) (THUSE TC-UNGRASP)))
    THEOREM)

(DEFPROP TC-PUTIN
    (THCONSE (X Y Z (WHY (EV)) EV)
        (#PUTIN $?X $?Y)
        (MEMORY)
        (THCOND ((THGOAL (#PUTON $?X $?Y) (THUSE TC-PUTON))
                (MEMOREND (#PUTIN $?EV $?X $?Y))
                (THSUCCEED THEOREM))
            ((THSUCCEED)))
        (THGOAL (#IS $?Y #BOX))
        (THVSETQ $_Z
            (UNION (LISTIFY $?X)
                (THVAL '(THFIND ALL $?W (W) (THGOAL (#ON $?W $?Y))) THALIST)))
        (THGOAL (#CLEARTOP $?Y) (THUSE TC-CLEARTOP))
        (THGOAL (#PACK $?Z $?Y) (THUSE TC-PACK))
        (MEMOREND (#PUTIN $?EV $?X $?Y)))
    THEOREM)

(DEFPROP TC-PUTON
    (THCONSE (X Y Z (WHY (EV)) EV)
        (#PUTON $?X $?Y)
        (ATOM $?Y)
        (OR (CDR $?X) (THSETQ $_X (CAR $?X)))
        (NOT (COND ((ATOM $?X) (EQ $?X $?Y)) ((MEMQ $?Y $?X))))
        (MEMORY)
        (THCOND ((ATOM $?X)
                (THGOAL (#CLEARTOP $?X) (THUSE TC-CLEARTOP))
                (THOR (THGOAL (#FINDSPACE $?Y $E (SIZE $?X) $?X $_Z) (THUSE TC-FINDSPACE))
                    (AND (NULL (GET '#NOCLEAR 'THASSERTION))
                        (THGOAL (#FINDSPACE $?Y $E (SIZE $?X) $?X $_Z) (THUSE TC-MAKESPACE))))
                (THGOAL (#PUT $?X $?Z) (THNODB) (THUSE TC-PUT)))
            ((THASSERT (#NOCLEAR))
                (THPROG ((W $?X))
                =>  (THOR (THGOAL (#PUTON $E (CAR $?W) $?Y) (THUSE TC-PUTON))
                        (THFAIL THPROG))
                    (THOR (THSETQ $?W (CDR $?W)) (THRETURN T))
                    (THGO =>))
            (THERASE (#NOCLEAR)))
            ((THNOT (THGOAL (#IS $?Y #BOX)))
                (THGOAL (#CLEARTOP $?Y) (THUSE TC-CLEARTOP))
                (THGOAL (#PACK $?X $?Y) (THUSE TC-PACK))))
        (MEMOREND (#PUTON $?EV $?X $?Y)))
    THEOREM)

(DEFPROP TC-RAISEHAND
    (THCONSE ((WHY (EV)) EV)
        (#RAISEHAND)
        (MEMORY)
        (THGOAL (#MOVEHAND $E (LIST (CAR HANDAT) (CADR HANDAT) 512)) (THNODB) (THUSE TC-MOVEHAND))
        (MEMOREND (#RAISEHAND $?EV)))
    THEOREM)

(DEFPROP TC-STACK
    (THCONSE (X Y)
        (#IS $?X #STACK)
        (NOT (THASVAL $?X))
        (THGOAL (#MANIP $?Y))
        (THGOAL (#SUPPORT $?Y ?))
        (THNOT (THAND (THGOAL (#PART $?Y $_X))
            (THGOAL (#IS $?X #STACK))))
    =>  (THGOAL (#SUPPORT $_X $?Y))
        (THCOND ((MEMQ $?X '(:TABLE :BOX)))
            ((THSETQ $_Y $?X) (THGO =>)))
        (THSETQ $_X (MAKESYM 'STACK))
        (THASSERT (#IS $?X #STACK))
        (THASSERT (#EXISTS $?X))
        (THFIND ALL
            $?Z
            (Z)
            (THGOAL (#ON $?Z $?Y) (THUSE TC-ON))
            (THAND (THASSERT (#PART $?Z $?X)) (THFINALIZE THAND))))
    THEOREM)

(DEFPROP TC-STACKUP
    (THCONSE (X Y BLOCKS PYR (WHY (EV)) EV)
        (#STACKUP $?X)
        (OR (LESSP (APPLY 'PLUS (MAPCAR #'(LAMBDA (X) (CADDR (SIZE X))) $?X)) 641)
            (NOT (DPRINT2 'TOO\ HIGH\,)))
        (THCOND
            ((AND $?X (CDR $?X)))
            ((THSETQ $_X
                (APPEND $?X
                    (THVAL (LIST 'THFIND
                        (COND ((NULL $?X) 3) (2))
                        '$?Y
                        '(Y)
                        '(THOR (THAND (THGOAL (#IS $?Y #BLOCK)) (THNOT (THGOAL (#SUPPORT $?Y ?)))) (THGOAL (#IS $?Y #BLOCK)))
                        '(NOT (EQ $?X $?Y)))
                        THALIST)))))
        (COND ((THVSETQ $_PYR (PACKO $?X '#PYRAMID)) (NULL (CDR $?PYR))) (T))
        (THVSETQ $_BLOCKS (CONS ':TABLE (PACKO $?X '#BLOCK)))
        (MEMORY)
    =>  (THCOND
            ((CDR $?BLOCKS)
                (THGOAL (#PUTON $E (CADR $?BLOCKS) $E (CAR $?BLOCKS)) (THUSE TC-PUTON))
                (THSETQ $_BLOCKS (CDR $?BLOCKS))
                (THGO =>))
            ($?PYR (THGOAL (#PUTON $E (CAR $?PYR) $E (CAR $?BLOCKS)) (THUSE TC-PUTON)))
            ((MEMOREND (#STACKUP $?EV $?X)))))
    THEOREM)

(DEFPROP TC-STARTEND3
    (THCONSE (X Y EV TIME) ($?X $?EV $?TIME) (THGOAL ($?X $?Y $?EV $?TIME) (THUSE TC-STARTEND4)))
    THEOREM)

(DEFPROP TC-STARTEND4
    (THCONSE (X NEWEV Z EV TIME)
        ($?X $?NEWEV $?EV $?TIME)
        (OR (AND (THASVAL $?X) (THASVAL $?EV) (THASVAL $?TIME) (NOT (THASVAL $?NEWEV))) (ERT TC-STARTEND4))
        (THGOAL (#CHOOSE $?EV $_Z NIL) (THUSE TC-CHOOSE))
        (OR (ATOM $?Z) (ERT TC-STARTEND4 ATOM))
        (THSETQ $_NEWEV (MAKESYM 'EV))
        (PUTPROP $?NEWEV
            (PUTPROP $?NEWEV
                (GET $?Z (COND ((EQ $?X '#START) 'START) ((EQ $?X '#END) 'END) ((ERT TC-STARTEND (THV X)))))
                'START)
            'END)
        (TIMECHK $?NEWEV $?TIME)
        (PUTPROP $?NEWEV $?Z 'WHY)
        (PUTPROP $?NEWEV '#START 'TYPE))
    THEOREM)

(DEFPROP TC-UNGRASP
    (THCONSE (X OBJ (WHO (EV)) EV)
        (#UNGRASP)
        (THCOND ((THGOAL (#GRASPING $?X))
                (MEMORY)
                (THGOAL (#SUPPORT ? $?X))
                (THERASE (#GRASPING $?X))
                (MEMOREND (#UNGRASP $?EV $?X))
                (THSETQ THTIME (ADD1 THTIME))
                (THCOND (DOIT (THOR (UNGRASP) (AND (GRASP $?X) NIL)))
                    ((THSETQ PLAN (CONS '(UNGRASP) PLAN)))))
            ((THSUCCEED))))
    THEOREM)

(DEFPROP TC-WANT4
    (THCONSE (X EV TIME Y)
        (#WANT $?X $?EV $?TIME)
        (THGOAL (#WANT $?Y $?X $?EV $?TIME) (THUSE TC-WANT5)))
    THEOREM)

(DEFPROP TC-WANT5
    (THCONSE (X NEWEV EV TIME Z)
        (#WANT $?NEWEV $?X $?EV $?TIME)
        (OR (AND (THASVAL $?X) (THASVAL $?EV) (THASVAL $?TIME)) (ERT TC-WANT5 THASVAL))
        (EQ $?X ':FRIEND)
        (EQ (GET $?EV 'WHY) 'COMMAND)
        (THSETQ $_NEWEV (MAKESYM 'EV))
        (PUTPROP $?NEWEV
            (PUTPROP $?NEWEV
                (GET $?EV 'START)
                'START)
            'END)
        (TIMECHK $?NEWEV $?TIME)
        (PUTPROP $?NEWEV '#TELL 'TYPE)
        (PUTPROP $?NEWEV 'ESP 'WHY))
    THEOREM)

(DEFPROP TCT-EXISTS (THCONSE NIL (#EXISTS ? ?) (THSUCCEED)) THEOREM)

(DEFPROP TCT-PICKUP
    (THCONSE (X EV TIME)
        (#PICKUP $?X $?TIME)
        (THOR (THAND (THGOAL (#PICKUP$?EV $?X)) (TIMECHK $?EV $?TIME))
            (THGOAL (#PICKUP $?EV $?X $?TIME) (THUSE TCTE-PICKUP))))
    THEOREM)

(DEFPROP TCT-PUT
    (THCONSE (X EV TIME Y)
        (#PUT $?X $?Y $?TIME)
        (THGOAL (#PUT $?EV $?X $?Y $?TIME) (THUSE TCTE-PUT)))
    THEOREM)

(DEFPROP TCT-AT
    (THCONSE (X Y Z TIME W)
        (#AT $?Y $?Z $?TIME)
        (THOR (THGOAL (#MANIP $?Y))
            (THAND (THGOAL (#IS $?Y #BOX)) (THGOAL (#AT $?Y $?Z)) (THSUCCEED THEOREM)))
        (THSETQ $_X (TFIND $?Y $?TIME))
        (THOR (THSETQ $_W (CAR $?X))
            (THAND (THAMONG $?W (CDR $?X)) (OR (NOT (LESSP (CAR $?W) (OR (START? $?TIME) -1))) (THFAIL THAND))))
        (THSETQ $?Z (CADR $?W)))
    THEOREM)

(DEFPROP TCT-LOC
    (THCONSE (YY ZZ X Y Z TIME)
        (#LOC $?X $?Y $?Z $?TIME)
        (THGOAL (#AT $?Y $?YY $?TIME) (THUSE TCT-AT))
        (THGOAL (#AT $?Z $?ZZ $?TIME) (THUSE TCT-AT))
        (THGOAL (#TLOC $?X $?Y $?Z) (THUSE TC-LOC)))
    THEOREM)

(DEFPROP TCT-SUPPORT
    (THCONSE (X Y Z TIME)
        (#SUPPORT $?X $?Y $?TIME)
        (THOR (THGOAL (#MANIP $?Y)) (THGOAL (#IS $?Y #BOX)))
        (THAMONG $?Z (TFIND $?Y $?TIME))
        (NOT (LESSP (CAR $?Z) (OR (START? $?TIME) -1)))
        (THAMONG $?X (LIST (CADDR $?Z))))
    THEOREM)

(DEFPROP TCT-2
    (THCONSE (X EV TIME) ($?X $?TIME) (THGOAL ($?X $?EV $?TIME) (THUSE TCTE-3)))
    THEOREM)

(DEFPROP TCT-3
    (THCONSE (X Y EV TIME) ($?X $?Y $?TIME) (THGOAL ($?X $?EV $?Y $?TIME) (THUSE TCTE-4)))
    THEOREM)

(DEFPROP TCT-4
    (THCONSE (X Y Z EV TIME) ($?X $?Y $?Z $?TIME) (THGOAL ($?X $?EV $?Y $?Z $?TIME) (THUSE TCTE-5)))
    THEOREM)

(DEFPROP TCTE-PICKUP
    (THCONSE (X EV EVENT TIME)
        (#PICKUP $?EV $?X $?TIME)
        (THOR (THAND (THGOAL (#PICKUP $?EV $?X)) (TIMECHK $?EV $?TIME) (THSUCCEED THEOREM))
            (THSUCCEED))
        (THAMONG $?EVENT EVENTLIST)
        (MEMQ (GET $?EVENT 'TYPE) '(#PUTON #GET-RID-OF))
        (TIMECHK $?EVENT $?TIME)
        (THOR (THGOAL (#PUTON $?EVENT $?X ?))
            (THGOAL (#GET-RID-OF $?EVENT $?X)))
        (THVSETQ $_EV (MAKESYM 'E))
        (AND (PUTPROP $?EV (PUTPROP $?EV (GET $?EVENT 'END) 'START) 'END)
            (PUTPROP $?EV '#PICKUP 'TYPE)
            (PUTPROP $?EV $?EVENT 'WHY)
            (SETQ EVENTLIST (CONS $?EV EVENTLIST))
            (THASSERT (#PICKUP $?EV $?X))))
    THEOREM)

(DEFPROP TCTE-PUT
    (THCONSE (X Y EV EVENT TIME Z)
        (#PUT $?EV $?X $?Y $?TIME)
        (THAMONG $?EVENT EVENTLIST)
        (MEMQ (GET $?EVENT 'TYPE) '(#PICKUP #PUTON))
        (TIMECHK $?EVENT $?TIME)
        (THOR (THGOAL (#PUTON $?EVENT $?X ?))
            (THGOAL (#PICKUP $?EVENT $?X)))
        (OR (THVSETQ $_Z (SUB1 (ASSQ (GET $?EVENT 'END) (GET $?X 'HISTORY))))
            (ERT TCTE-PUT WRONG))
        (THAMONG $?Y (LIST (CADR $?Z)))
        (THSETQ $_EV (MAKESYM 'E))
        (AND (PUTPROP $?EV (PUTPROP $?EV (CAR $?Z) 'START) 'END)
            (PUTPROP $?EV $?EVENT 'WHY)
            (PUTPROP $?EV '#PUT 'TYPE)
            (SETQ EVENTLIST (CONS $?EV EVENTLIST))
            (THASSERT (#PUT $?EV $?X $?Y))))
    THEOREM)

(DEFPROP TCTE-3
    (THCONSE (X EV TIME)
        ($?X $?EV $?TIME)
        (OR (THASVAL TIME) (ERT TCTE-3))
        (THGOAL ($?X $?EV))
        (TIMECHK $?EV $?TIME))
    THEOREM)

(DEFPROP TCTE-4
    (THCONSE (X Y EV TIME)
        ($?X $?EV $?Y $?TIME)
        (OR (THASVAL $?TIME) (ERT TCTE-4))
        (THGOAL ($?X $?EV $?Y))
        (TIMECHK $?EV $?TIME))
    THEOREM)

(DEFPROP TCTE-5
    (THCONSE (X Y Z EV TIME)
        ($?X $?EV $?Y $?Z $?TIME)
        (OR (THASVAL $?TIME) (ERT TCT-5))
        (THGOAL ($?X $?EV $?Y $?Z))
        (TIMECHK $?EV $?TIME))
    THEOREM)

(DEFPROP TCT-GRASP
    (THCONSE (X Z TIME)
        (#GRASP $?X $?TIME)
        (THVSETQ $_Z (ENDTIME GRASPLIST $?TIME))
    =>  (THCOND ((OR (NULL $?Z) (STARTIME $?Z $?TIME)) (THFAIL))
            ((OR (AND (NOT (THASVAL $?X)) (THSETQ $_X (CADAR $?Z))) (EQ $?X (CADAR $?Z))))
            ((THSETQ $_Z (CDR $?Z)) (THGO =>))
            ((THFAIL))))
    THEOREM)

(DEFPROP TE-CONTAIN
    (THERASING (X Y)
        (#AT $?X ?)
        (THGOAL (#CONTAIN $_Y $?X))
        (THERASE (#CONTAIN $?Y $?X)))
    THEOREM)

(DEFPROP TE-EXISTS (THERASING (X) (#EXISTS $?X) (THSUCCEED)) THEOREM)

(DEFPROP TE-SUPP
    (THERASING (X Y Z)
        (#AT $?X ?)
        (THCOND ((THGOAL (#SUPPORT $?X $_Y)) (ERT TE-SUPP))
            ((THGOAL (#SUPPORT $_Y $?X))
                (THERASE (#SUPPORT $?Y $?X))
                (THCOND
                    ((THGOAL (#PART $?X $_Y))
                        (THERASE (#PART $?X $?Y))
                        (THCOND ((THFIND 2 $?W (W) (THGOAL (#PART $?W $?Y)))
                                (THSUCCEED THEOREM))
                            ((THGOAL (#PART $_Z $?Y))
                                (THERASE (#PART $?Z $?Y)))
                            ((THSUCCEED)))
                        (THERASE (#EXISTS $?Y) (THUSE TE-EXISTS)))
                    ((THSUCCEED))))))
    THEOREM)

(DEFUN TOPCENTER (X) ((LAMBDA (X) (TCENT (CADR X) (CADDR X))) (ATAB X)))

(SETQ DOIT NIL)

(SETQ NOSTACKS T)

(DEFUN SASSQ (X Y Z) (OR (ASSQ X Y) (APPLY Z NIL)))

(DEFPROP #CLEARTOP (((THGOAL (#SUPPORT $?* ?)))) CHOOSE)

(DEFPROP #GRASP
    (((THNOT (THGOAL (#GRASPING $?*))) (THGOAL (#CLEARTOP $?*)))
     ((THNOT (THGOAL (#GRASPING $?*)))))
    CHOOSE)

(DEFPROP #PICKUP
    (((THGOAL (#SUPPORT ? $?*)) (THGOAL (#CLEARTOP $?*)))
     ((THGOAL (#SUPPORT ? $?*))))
    CHOOSE)

(DEFPROP #PUTIN
    (((THNOT (THGOAL (#CONTAIN :BOX $?*))) (THGOAL (#CLEARTOP $?*)))
     ((THNOT (THGOAL (#CONTAIN :BOX $?*)))))
    CHOOSE)

(DEFPROP #PUTIN (((THGOAL (#IS $?* #BOX)))) CHOOSE2)

(DEFPROP #PUTON (((THGOAL (#CLEARTOP $?*)))) CHOOSE)

(DEFPROP #PUTON
    (((THGOAL (#CLEARTOP $?*)) (THNOT (THGOAL (#IS $?* #PYRAMID))))
     ((THNOT (THGOAL (#IS $?* #PYRAMID)))))
    CHOOSE2)

(DEFPROP #STACKUP
    (((THGOAL (#CLEARTOP $?*)) (THNOT (THGOAL (#IS $?* #PYRAMID))))
     ((THNOT (THGOAL (#IS $?* #PYRAMID)))))
    CHOOSE)

(THDATA)
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
NIL

(DEFUN UNION (A B)
    (PROG NIL
    =>  (COND ((NULL A) (RETURN B))
            ((MEMQ (CAR A) B))
            ((SETQ B (CONS (CAR A) B))))
        (SETQ A (CDR A))
        (GO =>)))

(SETQ ^W NIL)

'(COMMANDS ARE: #CLEARTOP #GET-RID-OF #GRASP #PACK #PICKUP #PUTIN #PUTON #RAISEHAND #STACKUP #UNGRASP)

'(PREDICATES ARE: #LOC #SUPPORT #ON #PHYSOB)

#_(ns shrdlu.blockl)

;; ################################################################
;;
;;            BLOCKL - LISP CODE FOR THE BLOCKS WORLD
;;
;; ################################################################

(DEFUN ABSVAL (X) (COND ((MINUSP X) (MINUS X)) (X)))

(DEFUN ATAB (X) (OR (ASSQ X ATABLE) (ERT ATABLE)))

(DEFUN CLEAR (LOC SIZE OBJ)
    (PROG (W X1 X2)
        (SETQ OBJ (LISTIFY OBJ))
        (AND (MEMQ NIL (MAPCAR #'(LAMBDA (X Y) (AND (GREATERP X -1) (GREATERP 641 (PLUS X Y)) T)) LOC SIZE))
            (RETURN NIL))
        (SETQ W ATABLE)
    =>  (COND ((NULL W) (RETURN LOC))
            ((MEMQ (CAAR W) OBJ))
            ((AND (LESSP (CAR LOC) (PLUS (CAR (SETQ X1 (CADAR W))) (CAR (SETQ X2 (CADDAR W)))))
                (LESSP (CAR X1) (PLUS (CAR LOC) (CAR SIZE)))
                (LESSP (CADR LOC) (PLUS (CADR X1) (CADR X2)))
                (LESSP (CADR X1) (PLUS (CADR LOC) (CADR SIZE)))
                (LESSP (CADDR LOC) (PLUS (CADDR X1) (CADDR X2)))
                (LESSP (CADDR X1) (PLUS (CADDR LOC) (CADDR SIZE))))
            (RETURN NIL)))
        (SETQ W (CDR W))
        (GO =>)))

(DEFUN DIFF (X Y) (MAPCAR #'DIFFERENCE X Y))

(DEFUN DIV2 (X) (QUOTIENT X 2))

(DEFUN ENDTIME (LIST TIME)
    (PROG (Y)
        (OR (SETQ Y (END? TIME)) (RETURN LIST))
    =>  (COND ((NULL LIST) (RETURN NIL))
            ((NOT (GREATERP (CAAR LIST) Y)) (RETURN LIST))
            ((SETQ LIST (CDR LIST)) (GO =>)))))

(DEFUN EV NIL (OR NOMEM $?EV))

(DEFUN FINDSPACE
    (TYPE SURF SIZE OBJ)
    (PROG (XYMAX XYMIN N V X1 X2)
        (SETQ OBJ (LISTIFY OBJ))
        (AND (MEMQ SURF OBJ) (RETURN NIL))
        (COND ((EQ SURF ':TABLE)
                (SETQ XYMIN '(0 0))
                (SETQ XYMAX '(640 640))
                (SETQ LEVEL 0)
                (GO ON))
            ((SETQ X (ATAB SURF))))
        (COND
            ((EQ TYPE 'CENTER)
                (COND ((CLEAR (SETQ V
                        (LIST (MAX 0 (PLUS (CAADR X) (DIV2 (DIFFERENCE (CAADDR X) (CAR SIZE)))))
                            (MAX 0 (PLUS (CADADR X) (DIV2 (DIFFERENCE (CADR (CADDR X)) (CADR SIZE)))))
                            (PLUS (CADDR (CADR X)) (CADDR (CADDR X)))))
                        SIZE
                        OBJ)
                    (RETURN V))
                ((RETURN NIL))))
            ((EQ (CAR X) ':BOX)
                (SETQ XYMIN (LIST (CAADR X) (CADADR X)))
                (SETQ XYMAX (LIST (PLUS (CAADDR X) (CAADR X)) (PLUS (CADR (CADDR X)) (CADADR X))))
                (SETQ LEVEL 1))
            ((SETQ X1 (DIV2 (CAR SIZE)))
                (SETQ Y1 (DIV2 (CADR SIZE)))
                (SETQ XYMAX (LIST (MIN 640 (SUB1 (PLUS (CAADDR X) (CAADR X) X1))) (MIN 640 (SUB1 (PLUS (CADR (CADDR X)) (CADADR X) Y1)))))
                (SETQ XYMIN (LIST (MAX 0 (DIFFERENCE (CAADR X) X1)) (MAX 0 (DIFFERENCE (CADADR X) Y1))))
                (SETQ LEVEL (PLUS (CADDR (CADR X)) (CADDR (CADDR X))))))
    ON  (SETQ N 8)
        (SETQ X1 (DIFFERENCE (CAR XYMAX) (CAR XYMIN)))
        (SETQ Y1 (DIFFERENCE (CADR XYMAX) (CADR XYMIN)))
    GO  (COND ((ZEROP (SETQ N (SUB1 N))) (RETURN NIL))
            ((OR (NOT (SETQ V
                        (GROW (LIST
                                (PLUS (CAR XYMIN) (REMAINDER (ABSVAL (RANDOM)) X1))
                                (PLUS (CADR XYMIN) (REMAINDER (ABSVAL (RANDOM)) Y1))
                                LEVEL)
                            XYMIN XYMAX OBJ)))
                    (LESSP (DIFFERENCE (CAADR V) (CAAR V)) (CAR SIZE))
                    (LESSP (DIFFERENCE (CADADR V) (CADAR V)) (CADR SIZE)))
                (GO GO))
            ((RETURN (COND
                ((EQ TYPE 'RANDOM)
                    (LIST (DIV2 (DIFFERENCE (PLUS (CAAR V) (CAADR V)) (CAR SIZE)))
                        (DIV2 (DIFFERENCE (PLUS (CADAR V) (CADADR V)) (CADR SIZE)))
                        LEVEL))
                ((EQ TYPE 'PACK)
                    (LIST (CAAR V) (CADAR V) LEVEL))
                ((ERT FINDSPACE \-- TYPE))))))))

(DEFUN GOAL FEXPR (X)
    (SETQ PLAN NIL)
    (THVAL (LIST 'THGOAL (CAR X) '(THTBF THTRUE)) '((EV COMMAND)))
    (EVLIS (REVERSE PLAN)))

(DEFUN GROW (LOC MIN MAX OBJ)
    (PROG (GROW XL XH XO YL YH YO)
        (SETQ OBJ (LISTIFY OBJ))
        (COND
            ((OR
                (MINUSP (CAAR (SETQ XL (LIST (LIST (DIFFERENCE (CAR LOC) (CAR MIN)) NIL)))))
                (MINUSP (CAAR (SETQ XH (LIST (LIST (DIFFERENCE (CAR MAX) (CAR LOC)) NIL)))))
                (MINUSP (CAAR (SETQ YL (LIST (LIST (DIFFERENCE (CADR LOC) (CADR MIN)) NIL)))))
                (MINUSP (CAAR (SETQ YH (LIST (LIST (DIFFERENCE (CADR MAX) (CADR LOC)) NIL)))))
                (NULL (ERRSET
                    (MAPC #'(LAMBDA (X)
                        (PROG (XX YY)
                            (COND ((OR (MEMQ (CAR X) OBJ)
                                    (NOT (LESSP (CAADR X) (CAR MAX)))
                                    (NOT (LESSP (CADADR X) (CADR MAX)))
                                    (NOT (GREATERP (SETQ XX (PLUS (CAADR X) (CAADDR X))) (CAR MIN)))
                                    (NOT (GREATERP (SETQ YY (PLUS (CADADR X) (CADR (CADDR X)))) (CADR MIN)))
                                    (NOT (GREATERP (PLUS (CADDR (CADR X)) (CADDR (CADDR X))) (CADDR LOC))))
                                (RETURN NIL))
                            ((GREATERP (CAADR X) (CAR LOC))
                                (SETQ XH (ORDER (LIST (DIFFERENCE (CAADR X) (CAR LOC)) (CAR X)) XH)))
                            ((LESSP XX (CAR LOC))
                                (SETQ XL (ORDER (LIST (DIFFERENCE (CAR LOC) XX) (CAR X)) XL)))
                            ((SETQ XO (CONS (CAR X) XO))))
                            (COND ((GREATERP (CADADR X) (CADR LOC))
                                (SETQ YH (ORDER (LIST (DIFFERENCE (CADADR X) (CADR LOC)) (CAR X)) YH)))
                            ((LESSP YY (CADR LOC))
                                (SETQ YL (ORDER (LIST (DIFFERENCE (CADR LOC) YY) (CAR X)) YL)))
                            ((MEMQ (CAR X) XO) (ERR NIL))
                            ((SETQ YO (CONS (CAR X) YO))))))
                    ATABLE))))
                (RETURN NIL)))
    =>   (COND ((== (SETQ GROW (MIN (CAAR XL) (CAAR XH) (CAAR YL) (CAAR YH))) 1024)
            (RETURN (LIST
                (LIST (DIFFERENCE (CAR LOC) (CADAR XL)) (DIFFERENCE (CADR LOC) (CADAR YL)))
                (LIST (PLUS (CAR LOC) (CADAR XH)) (PLUS (CADR LOC) (CADAR YH))))))
            ((MAPC #'(LAMBDA (Y Z W) (PROG (X)
                        (SETQ X (EVAL W))
                        (COND ((GREATERP (CAAR X) GROW))
                            ((OR (NULL (CADAR X)) (MEMQ (CADAR X) (EVAL Y)))
                                (RPLACA X (LIST 2000 (CAAR X))))
                            ((SET Z (CONS (CADAR X) (EVAL Z)))
                                (SET W (CDR X))))))
                '(YO YO XO XO)
                '(XO XO YO YO)
                '(XL XH YL YH))
            (GO =>)))))

(DEFUN LISTIFY (X) (COND ((ATOM X) (LIST X)) (X)))

(DEFUN LOCGREATER (X Y FN)
    ((LAMBDA (XX YY) (NOT (LESSP (FN (CADR XX)) (PLUS (FN (CADR YY)) (FN (CADDR YY))))))
        (LOCG2 '$?YY X)
        (LOCG2 '$?ZZ Y)))

(DEFUN LOCG2 (X Y) (COND ((EQ $?LOC '#LOC) (ATAB Y)) ((CONS NIL (CONS (EVAL X) (CDDR (ATAB Y)))))))

(DEFUN MEMOREND FEXPR (A)
    (OR NOMEM
        (AND (PUTPROP $?EV THTIME 'END)
            (APPLY 'THASSERT (LIST (THVARSUBST (CAR A) NIL)))
            (PUTPROP $?EV (CAAR A) 'TYPE))))

(DEFUN MEMORY NIL
    (OR NOMEM
        (THAND (THVSETQ $_EV (MAKESYM 'E))
            (THSETQ EVENTLIST (CONS $?EV EVENTLIST))
            (PUTPROP $?EV THTIME 'START)
            (PUTPROP $?EV $?WHY 'WHY))))

(DEFUN OCCUPIER (X Y Z)
    (PROG (W X1 X2)
        (COND ((MINUSP Z) (RETURN ':TABLE)))
        (SETQ W ATABLE)
    =>  (COND ((NULL W) (RETURN NIL))
            ((AND (LESSP (SUB1 (CAR (SETQ X1 (CADAR W)))) X (PLUS (CAR X1) (CAR (SETQ X2 (CADDAR W)))))
                (LESSP (SUB1 (CADR X1)) Y (PLUS (CADR X1) (CADR X2)))
                (LESSP (SUB1 (CADDR X1)) Z (PLUS (CADDR X1) (CADDR X2))))
            (RETURN (CAAR W))))
        (SETQ W (CDR W))
        (GO =>)))

(DEFUN ORDER (X Y)
    (COND ((NULL Y) (LIST X))
        ((GREATERP (CAR X) (CAAR Y))
            (CONS (CAR Y) (ORDER X (CDR Y))))
        ((CONS X Y))))

(DEFUN PACKO (OBJ TYPE)
    (PROG (XX)
        (MAPC #'(LAMBDA (X)
            (AND (THVAL '(THGOAL (#IS $?X $E TYPE)) (LIST (LIST 'X X))) (SETQ XX (PACKORD X (SIZE X) XX))))
        OBJ)
        (RETURN (MAPCAR 'CADR XX))))

(DEFUN PACKON (SURF LIST)
    (PROG (X)
        (SETQ SURF (ATAB SURF))
    =>  (COND ((NULL LIST) (RETURN NIL))
            ((OR
                (GREATERP (CAR (SETQ X (SIZE (CAR LIST)))) (CAADDR SURF))
                (GREATERP (CADR X) (CADR (CADDR SURF)))
                (GREATERP (PLUS (CADDR X) (CADDR (CADR SURF)) (CADDR (CADDR SURF))) 321))
            (SETQ LIST (CDR LIST))
            (GO =>))
            ((RETURN (CAR X))))))

(DEFUN PACKORD (X SIZE LIST)
    (COND ((NULL LIST) (LIST (LIST SIZE X)))
        ((OR (GREATERP (CAAAR LIST) (CAR SIZE))
            (AND (EQ (CAR SIZE) (CAAAR LIST)) (GREATERP (CADAAR LIST) (CADR SIZE))))
        (CONS (CAR LIST) (PACKORD X SIZE (CDR LIST))))
        ((CONS (LIST SIZE X) LIST))))

(DEFUN SIZE (X)
    (COND ((EQ X ':BOX) '(256 256 192))
        ((EQ X ':TABLE) '(640 640 640))
        ((ATOM X) (CADDR (ATAB X)))
        (X)))

(DEFUN STARTHISTORY NIL
    (SETQ THTIME 0)
    (SETQ GRASPLIST NIL)
    (DEFPROP EE COMMAND WHY)
    (DEFPROP EE 0 START)
    (DEFPROP EE 0 END)
    (DEFPROP EE #START TYPE)
    (SETQ EVENTLIST '(EE))
    (THADD '(#START EE :DIALOG) NIL)
    (ERRSET (CLEANOUT E) NIL)
    (MAPC #'(LAMBDA (X)
        (AND (GET (CAR X) 'THASSERTION)
        (PUTPROP (CAR X)
            (LIST (LIST 0
                    (CADR X)
                    (CADAR (THVAL '(THGOAL (#SUPPORT $?X $?Y)) (LIST (LIST 'X 'THUNASSIGNED) (LIST 'Y (CAR X)))))))
            'HISTORY)))
    ATABLE))

(DEFUN STARTIME (LIST TIME) (LESSP (CAAR LIST) (OR (START? TIME) -1)))

(DEFUN SUPPORT (LOC SIZE X)
    (COND ((EQ (CADDR LOC) 0) ':TABLE)
        ((SETQ LOC (OCCUPIER (PLUS (CAR LOC) (DIV2 (CAR SIZE))) (PLUS (CADR LOC) (DIV2 (CADR SIZE))) (SUB1 (CADDR LOC))))
            (COND ((EQ LOC X) NIL) (LOC)))))

(DEFUN TCENT (X1 X2)
    (LIST (PLUS (CAR X1) (DIV2 (CAR X2))) (PLUS (CADR X1) (DIV2 (CADR X2))) (PLUS (CADDR X1) (CADDR X2))))

(DEFUN TFIND (X Y)
    (PROG (Z)
        (OR (SETQ Z (GET X 'HISTORY)) (RETURN NIL))
    =>  (COND ((NOT (GREATERP (CAAR Z) (OR (END? Y) 32767)))
                (RETURN Z))
            ((SETQ Z (CDR Z)) (GO =>)))))

(DEFUN TIMECHK (EV TIME)
    (COND ((IMPERF? TIME)
        (NOT (OR (LESSP (GET EV 'END) (OR (START? TIME) -1))
            (LESSP (OR (END? TIME) 262143)
                (GET EV 'START)))))
        ((NOT (OR (LESSP (GET EV 'START) (OR (START? TIME) -1))
            (LESSP (OR (END? TIME) 262143)
                (GET EV 'END)))))))

#_(ns shrdlu.data)

;; ####################################################################
;;
;;      DATA > INITIAL MICROPLANNER DATA BASE FOR THE BLOCKS WORLD
;;
;; ####################################################################

(THFLUSH THASSERTION)

(SETQ ATABLE
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

(SETQ DISPLAY-AS
  '((:B1 #DISPLAY #BLOCK (72 64 0) (64 64 64) RED)
    (:B2 #DISPLAY #PYRAMID (72 64 64) (64 64 64) GREEN)
    (:B3 #DISPLAY #BLOCK (256 0 0) (128 128 128) GREEN)
    (:B4 #DISPLAY #PYRAMID (416 416 1) (128 128 128) BLUE)
    (:B5 #DISPLAY #PYRAMID (320 64 128) (64 64 192) RED)
    (:B6 #DISPLAY #BLOCK (0 192 0) (128 192 192) RED)
    (:B7 #DISPLAY #BLOCK (0 160 192) (128 128 128) GREEN)
    (:B10 #DISPLAY #BLOCK (192 416 0) (128 64 256) BLUE)
    (:HAND #DISPLAY #HAND (32 0 0) (0 0 0) WHITE)
    (:TABLE #DISPLAY #TABLE (0 0 0) (512 512 0) BLACK)
    (:BOX #DISPLAY #BOX (384 384 0) (254 254 192) WHITE)))

(THDATA)
    ((#IS :B1 #BLOCK))
    ((#IS :B2 #PYRAMID))
    ((#IS :B3 #BLOCK))
    ((#IS :B4 #PYRAMID))
    ((#IS :B5 #PYRAMID))
    ((#IS :B6 #BLOCK))
    ((#IS :B7 #BLOCK))
    ((#IS :B10 #BLOCK))

    ((#IS #RED #COLOR))
    ((#IS #BLUE #COLOR))
    ((#IS #GREEN #COLOR))
    ((#IS #WHITE #COLOR))
    ((#IS #BLACK #COLOR))

    ((#IS #RECTANGULAR #SHAPE))
    ((#IS #ROUND #SHAPE))
    ((#IS #POINTED #SHAPE))

    ((#IS :SHRDLU #ROBOT))
    ((#IS :FRIEND #PERSON))
    ((#IS :HAND #HAND))

    ((#AT :B1 (64 64 0)))
    ((#AT :B2 (64 64 64)))
    ((#AT :B3 (256 0 0)))
    ((#AT :B4 (416 416 1)))
    ((#AT :B5 (320 64 128)))
    ((#AT :B6 (0 192 0)))
    ((#AT :B7 (0 160 192)))
    ((#AT :B10 (192 416 0)))

    ((#SUPPORT :B1 :B2))
    ((#SUPPORT :B3 :B5))
    ((#SUPPORT :B6 :B7))

    ((#CLEARTOP :B2))
    ((#CLEARTOP :B4))
    ((#CLEARTOP :B5))
    ((#CLEARTOP :B7))
    ((#CLEARTOP :B10))

    ((#MANIP :B1))
    ((#MANIP :B2))
    ((#MANIP :B3))
    ((#MANIP :B4))
    ((#MANIP :B5))
    ((#MANIP :B6))
    ((#MANIP :B7))
    ((#MANIP :B10))

    ((#SUPPORT :TABLE :B1))
    ((#SUPPORT :TABLE :B3))
    ((#SUPPORT :BOX :B4))
    ((#SUPPORT :TABLE :B10))
    ((#SUPPORT :TABLE :B6))
    ((#SUPPORT :TABLE :BOX))

    ((#AT :BOX (384 384 0)))
    ((#IS :BOX #BOX))
    ((#IS :TABLE #TABLE))
    ((#CONTAIN :BOX :B4))

    ((#SHAPE :B1 #RECTANGULAR))
    ((#SHAPE :B3 #RECTANGULAR))
    ((#SHAPE :B2 #POINTED))
    ((#SHAPE :B4 #POINTED))
    ((#SHAPE :B5 #POINTED))
    ((#SHAPE :B6 #RECTANGULAR))
    ((#SHAPE :B7 #RECTANGULAR))
    ((#SHAPE :B10 #RECTANGULAR))

    ((#COLOR :B1 #RED))
    ((#COLOR :B2 #GREEN))
    ((#COLOR :B3 #GREEN))
    ((#COLOR :B4 #BLUE))
    ((#COLOR :B5 #RED))
    ((#COLOR :B6 #RED))
    ((#COLOR :B7 #GREEN))
    ((#COLOR :B10 #BLUE))
    ((#COLOR :BOX #WHITE))
    ((#COLOR :TABLE #BLACK))

    ((#CALL :SHRDLU SHRDLU))
    ((#CALL :FRIEND YOU))
NIL

(SETQ HANDAT '(32 0 0))

(SETQ THTIME 0)

(THFLUSH HISTORY)

(ERRSET (STARTHISTORY))

(SETQ PLAN NIL)

(MAPC #'(LAMBDA (X Y) (PUTPROP X (LIST Y) 'COLOR))
    '(:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B10)
    '(CB1 CB2 CB3 CB4 CB5 CB6 CB7 CB10))

(SETQ ^W NIL)

#_(ns shrdlu.plnr)

(COMMENT DO NOT GRIND THIS FILE WITH THE STANDARD GRIND)

(SETQ THVERSION NIL)

(DEFUN THREAD NIL
    ;; FUNCTION FOR THE /$ READ MACRO
    (PROG (CHAR)
        (RETURN (COND
            ((EQ (SETQ CHAR (READCH)) '?) (LIST 'THV (READ)))
            ((EQ CHAR 'E) (LIST 'THEV (READ)))
            ((EQ CHAR '_) (LIST 'THNV (READ)))
            ((EQ CHAR '&) ;; TREATS & - - & AS A COMMENT
                (PROG NIL
                =>  (COND ((EQ '& (READCH))
                        (RETURN '(COMMENT))))
                    (GO =>)))
            ((EQ CHAR 'T) '(THTBF THTRUE))
            ((EQ CHAR 'R) 'THRESTRICT)
            ((EQ CHAR 'G) 'THGOAL)
            ((EQ CHAR 'A) 'THASSERT)
            ((EQ CHAR 'N) (LIST 'THANUM (READ)))
            ((PRINT 'ILLEGAL-PREFIX)
                (PRINC '$)
                (PRINC CHAR)
                (PRINC (READ))
                (ERR NIL))))))

(DEFUN THPUSH MACRO (A)
    ;; (THPUSH THTREE NEWINFO) CONSES NEWINFO ONTO THTREE
    (LIST 'SETQ (CADR A) (LIST 'CONS (CADDR A) (CADR A))))

(DEFUN EVLIS (X)
    ;; EVLIS EVALS ELEMENTS OF ARG THEN RETURNS ARG
    (MAPC #'EVAL X))

(DEFUN THPRINTC (X) (TERPRI) (PRINC X) (PRINC '\ ))

(DEFUN THADD (THTT THPL)
    ;; THADD ADDS THEOREMS OR ASSERTION TO THE INPUT
    ;; THPL - PROPERTY LIST TO BE PLACED ON ASSERTION DATABASE INPUTS
    ;; THTT - NAME OF THM OR ACTUAL ASSERTION
    ;; RETURNS NIL IF ALREADY THERE ELSE RETURNS THTT
    (PROG (THNF THWH THCK THLAS THTTL THT1 THFST THFSTP THFOO)
        (SETQ THCK
            ;; IF THTT IS ATOMIC, WE ARE ASSERTING A THEOREM
            (COND ((ATOM THTT)
                    ;; IF NO THEOREM PROPERTY, THE GUY MADE A MISTAKE
                    (OR (SETQ THT1 (GET THTT 'THEOREM))
                        (PROG2 (PRINT THTT) (THERT CANT THASSERT\, NO THEOREM - THADD)))
                    ;; THWH NOW SET TO KIND OF THEOREM, LIKE THERASING
                    (SETQ THWH (CAR THT1))
                    ;; MAKE AN EXTRA POINTER TO THTT
                    (SETQ THTTL THTT)
                    ;; IF WE HAVE A PL FOR OUR THEOREM, IT GOES ON THE ATOM WHICH IS THE NAME OF THE THEOREM
                    (AND THPL (PROG NIL
                    ;; GO THROUGH ITEMS ON PL ONE BY ONE
                    =>  (THPUTPROP THTT (CADR THPL) (CAR THPL))
                        (COND ((SETQ THPL (CDDR THPL))
                            (GO =>)))))
                    (CADDR THT1))
                ;; SO WE HAVE AN ASSERTION TO ASSERT, MAKE THWH REFLECT THIS FACT
                (T (SETQ THWH 'THASSERTION)
                    ;; PROPERTY LIST IS "CDR" OF ASSERTION
                    (SETQ THTTL (CONS THTT THPL))
                    THTT)))
        (SETQ THNF 0) ;; THNF IS COUNTER SAYING WHICH ATOM WE ARE FILING UNDER
        (SETQ THLAS (LENGTH THCK)) ;; THLAS IS THE NUMBER OF TOP LEVEL ITEMS
        (SETQ THFST T)
        ;; THFST SAYS WE ARE TRYING TO PUT THE ITEM IN FOR THE FIRST TIME.
        ;; WE NEED TO KNOW THIS SINCE THE FIRST TIME THROUGH
        ;; WE MUST TEST THAT THE ASSERTEE IS NOT ALREADY THERE.
        ;; THCK IS INITIALLY THE ASSERTION OR THEOREM PATTERN.
        ;; THE FIRST TIME WE GO INTO THE DATABASE WE CHECK TO SEE IF THE ITEM IS THERE.
        ;; THAT MEANS DOING AN EQUAL TEST ON EVERY ITEM IN THE BUCKET.
        ;; AFTER THE FIRST TIME THIS IS NOT NECESSARY.
        ;; SINCE VARIABLES WILL IN GENERAL HAVE MANY MORE ITEMS IN THEIR BUCKET,
        ;; WE WILL WANT TO DO OUR CHECK ON A NON VARIABLE ITEM IN THE PATTERN.
    THP1 (COND ((NULL THCK)
                ;; THCK NIL MEANS THAT ALL THE ITEMS IN THE PATTERN ARE VARIABLES
                ;; SO WE TRY AGAIN ONLY THIS TIME DOING EQUAL CHECK ON
                ;; THE FIRST VARIABLE. THFOO NOW IS SIMPLY THE PATTERN
                (SETQ THCK THFOO)
                (SETQ THNF 0)
                (SETQ THFOO (SETQ THFST NIL))
                ;; THFIRSTP SAYS WE AGAIN NEED TO CHECK FOR ASSERTEE
                ;; BEING IN DATA BASE, BUT NOW USE VARIABLES FOR EQ CHECK
                (SETQ THFSTP T)
                (GO THP1))
            ((NULL (SETQ THT1 (THIP (CAR THCK)))) (RETURN NIL))
            ;; THIP IS THE WORKHORSE FOR THADD IF IT RETURNS NIL.
            ;; IT MEANS THE ASSERTEE IS ALREADY IN, SO FAIL.
            ((EQ THT1 'THOK))
            ;; THOK WHICH IS RETURN BY THIP SAYS THAT THE ASSERTEE IS NOT IN ALREADY.
            ;; OTHERWISE WE GO AROUND AGAIN, STILL LOOKING FOR A NON VARIABLE ITEM TO DO THE EQ CHECK.
            ((SETQ THFOO (NCONC THFOO (LIST (COND ((EQ THT1 'THVRB) (CAR THCK))))))
                (SETQ THCK (CDR THCK))
                (GO THP1)))
        (SETQ THFST NIL)
        (MAPC #'THIP (CDR THCK))
        (SETQ THNF 0)
        (MAPC #'THIP THFOO)
        (RETURN THTTL)))

(DEFUN THAMONG FEXPR (THA)
    ;; EXAMPLE - (THAMONG $?X (THFIND ... ))
    ;; $E - (THAMONG $E$?X (THFIND ... )) CAUSES THE THVALUE OF $?X TO BE THE FIRST INPUT TO THAMONG.
    ;; THXX SET TO OLD BINDING CELL OF $?X (OR $E$?X) IF $?X VALUES PUSHED ONTO THTREE AND THAMONG
    ;; FAILS TO THUNASSIGNED, OLD VALUE AND LIST OF NEW THAMONGF.
    (COND
        ((EQ (CADR (SETQ THXX (THGAL (COND ((EQ (CAAR THA) 'THEV) (THVAL (CADAR THA) THALIST)) (T (CAR THA))) THALIST))) 'THUNASSIGNED)
            (THPUSH THTREE (LIST 'THAMONG THXX (THVAL (CADR THA) THALIST)))
            NIL)
        (T (MEMBER (CADR THXX) (THVAL (CADR THA) THALIST)))))       ;; IF $?X ASSIGNED, THAMONG REDUCES TO A MEMBERSHIP TEST

(DEFUN THAMONGF NIL                                                 ;; (CAR THTREE) = (THAMONG OLDBINDINGCELL (NEW VALUES))
    (COND (THMESSAGE (THPOPT) NIL)
        ((CADDAR THTREE)                                            ;; LIST OF NEW VALUES NON NIL
            (RPLACA (CDADAR THTREE) (CAADDR (CAR THTREE)))          ;; REPLACE OLD VALUE WITH NEW VALUE
            (RPLACA (CDDAR THTREE) (CDADDR (CAR THTREE)))           ;; POP NEW VALUES
            (SETQ THBRANCH THTREE)                                  ;; STORE AWAY TREE FOR POSSIBLE BACKTRACKING
            (SETQ THABRANCH THALIST)                                ;; STORE AWAY THALIST FOR POSSIBLE BACKTRACKING
            (THPOPT)                                                ;; POP TREE
            T)                                                      ;; SUCCEED
        (T (RPLACA (CDADAR THTREE) 'THUNASSIGNED)                   ;; NO NEW VALUES LEFT. RETURN X TO THUNASSIGNED,
            (THPOPT)                                                ;; POP TREE AND CONTINUE FAILING.
            NIL)))

(DEFUN THAND FEXPR (A)
    (OR (NOT A)
        (PROG2 (THPUSH THTREE (LIST 'THAND A NIL))
            (SETQ THEXP (CAR A)))))

(DEFUN THANDF NIL (THBRANCHUN) NIL)

(DEFUN THANDT NIL
    (COND ((CDADAR THTREE)
            (THBRANCH)
            (SETQ THEXP (CADR (CADAR THTREE)))
            (RPLACA (CDAR THTREE) (CDADAR THTREE)))
        ((THPOPT)))
    THVALUE)

(DEFUN THANTE FEXPR (THX)
    ;; DEFINES AND OPTIONALLY ASSERTS ANTECEDENT THEOREMS
    (THDEF 'THANTE THX))

(DEFUN THAPPLY FEXPR (L)
    (THAPPLY1 (CAR L)
        ;; THAPPLY1 DOES THE REAL WORK, ALL WE DO IS GET THE THEOREM OFF THE PROPERTY LIST
        (GET (CAR L) 'THEOREM)
        (CADR L)))

(DEFUN THAPPLY1 (THM THB DAT)
    ;; MAKE SURE THE THEOREM PATTERN MATCHES THE GOAL
    (COND ((AND (THBIND (CADR THB)) (THMATCH1 DAT (CADDR THB)))
            (AND THTRACE (THTRACES 'THEOREM THM))
            ;; AS FAR AS THTREE GOES, ALL THEOREMS LOOK LIKE THPROG, AND
            ;; WHEN YOU COME DOWN TO IT, THEY ALL ACT LIKE THPROGS.
            (THPUSH THTREE (LIST 'THPROG (CDDR THB) NIL (CDDR THB)))
            ;; CALL THE MAIN THPROG WORKHORSE
            (THPROGA)
            T)
        ;; IF THE THEOREM PATTERN DIDN'T MATCH, START FAILING.
        (T (SETQ THALIST THOLIST) (THPOPT) NIL)))

(DEFUN THASS1 (THA P)
    (PROG (THX THY1 THY TYPE PSEUDO)
        (AND (CDR THA) (EQ (CAADR THA) 'THPSEUDO) (SETQ PSEUDO T))
        ;; IF YOU SEE "THPSEUDO" SET FLAG "PSEUDO" TO T
        (OR (ATOM (SETQ THX (CAR THA)))
            ;; IF (CAR THA) IS AN ATOM WE ARE ASSERTING (ERRASING) A THEOREM
            (THPURE (SETQ THX (THVARSUBST THX NIL)))
            ;; THVARSUBST SUBSTITUTES THE ASSIGNMENTS FOR ALL ASSIGNED VARIABLES
            ;; THPURE CHECKS THAT ALL VARIABLES ARE ASSIGNED
            PSEUDO
            ;; IF WE ARE NOT REALLY ASSERTING, THE VARIABLES DO NOT ALL HAVE TO BE ASSIGNED
            (PROG2 (PRINT THX)
                (THERT IMPURE ASSERTION OR ERASURE - THASS1)))
        (AND THTRACE (NOT PSEUDO) (THTRACES (COND (P 'THASSERT) ('THERASE)) THX))
        (SETQ THA (COND (PSEUDO (CDDR THA)) ((CDR THA))))
        ;; THX IS NOW WHAT WE ARE ASSERTING, AND THA IS THE RECOMMENDATION LIST
        (OR
            ;; WE ARE NOW GOING TO PHYSICALLY ADD OR REMOVE ITEM
            (SETQ THX
                (COND (PSEUDO (LIST THX))
                    ;; IF THPSEUDO, DON'T ALTER THE DATA BASE
                    ;; IF P IS "T" WE ARE ASSERTING SO USE THADD
                    (P (THADD THX
                        ;; THADD TAKES TWO ARGS THE FIRST IS ITEM TO BE ADDED
                        ;; THE SECOND IS THE PROPERTY LIST FOR THE ITEM
                        (SETQ THY
                            ;; THPROP SAYS "MY" CADR IS TO BE EVALED TO GET THE PROPERTY LIST
                            (COND ((AND THA (EQ (CAAR THA) 'THPROP))
                                (PROG2 0
                                    (EVAL (CADAR THA))
                                    ;; AND REMOVE THPROP FROM THE RECOMENDATION LIST
                                    (SETQ THA (CDR THA))))))))
                    ;; OTHERWISE WE ARE ERASING, SO USE THREMOVE
                    (T (THREMOVE THX))))
            ;; THE LAST ITEM WILL BE NIL ONLY IF THADD OR THREMOVE FAILED.
            ;; THAT IS, IF THE ITEM TO BE ADDED WAS ALREADY THERE, OR THE ONE TO BE REMOVED, WASN'T.
            (RETURN NIL))
        ;; TYPE IS THE KIND OF THEOREM WE WILL BE LOOKING FOR
        (COND (P (SETQ TYPE 'THANTE))
            ((SETQ TYPE 'THERASING)))
        ;; IF WE ACTUALLY MUNGED THE DATABASE, PUT THE FACT IN THTREE
        (OR PSEUDO
            (THPUSH THTREE (LIST (COND (P 'THASSERT) ('THERASE)) THX THY)))
        (SETQ THY (MAPCAN #'THTAE THA))
        ;; MAPCAN IS A MAC-LISP FUNCTION, LIKE MAPCAR BUT USES NCONC.
        ;; THTAE LOOKS AT THE RECOMENDATION LIST AND PRODUCES A LIST OF INSTRUCTIONS ABOUT WHAT THEOREMS TO TRY.
        (COND (THY (SETQ THEXP (CONS 'THDO THY))))
        ;; THEXP IS A HACK TELLING THVAL TO THVAL THIS ITEM BEFORE IT GOES ON TO THE NEXT LINE OF PLANNER CODE.
        ;; THEXP IS NOW (THDO <APPROPRIATE ANTECEDENT OR ERASING THEOREMS>).
        (RETURN THX)))

(DEFUN THASSERT FEXPR (THA)
    ;; THASS1 IS USED FOR BOTH ASSERTING AND ERASING, THE "T" AS SECOND ARG TELLS IT THAT WE ARE ASSERTING.
    (THASS1 THA T))

(DEFUN THASSERTF NIL
    (THREMOVE (COND ((ATOM (CADAR THTREE)) (CADAR THTREE)) (T (CAADAR THTREE))))
    (THPOPT)
    NIL)

(DEFUN THASSERTT NIL (PROG2 0 (CADAR THTREE) (THPOPT)))

(DEFUN THASVAL FEXPR (X)
    ((LAMBDA (X) (AND X (NOT (EQ (CADR X) 'THUNASSIGNED))))
        (THGAL (CAR X) THALIST)))

(DEFUN THBA (TH1 TH2)
    ;; JUST LIKE ASSQ IN LISP, ONLY RETURN WITH THE POINTER 1 ELEMENT PRIOR TO THE ONE ASKED FOR.
    ;; USED ONLY BY THAD AND THREMOVE.
    (PROG (THP)
        (SETQ THP TH2)
    =>  (AND (EQ (COND (THPC (CADR THP)) (T (CAADR THP))) TH1)
            (RETURN THP))
        (OR (CDR (SETQ THP (CDR THP))) (RETURN NIL))
        (GO =>)))

(DEFUN THBAP (TH1 TH2)
    ;; LIKE THBA, ONLY USED EQUAL RATHER THAN EQ
    (PROG (THP)
        (SETQ THP TH2)
    =>  (AND (EQUAL (COND (THPC (CADR THP)) (T (CAADR THP))) TH1)
            (RETURN THP))
        (OR (CDR (SETQ THP (CDR THP))) (RETURN NIL))
        (GO =>)))

(DEFUN THBIND (A)
    ;; WHEN WE ENTER A NEW THEOREM OR THPROG, WE MUST BIND THE NEW VARIABLES.  A IS THE VARIABLE LIST
    (SETQ THOLIST THALIST) ;; THOLIST IS THE OLD THALIST
    ;; IF A IS NIL THERE IS NOTHING TO DO
    (OR (NULL A)
        (PROG NIL
        ;; WHEN A IS NIL, WE ARE DONE AND JUST PUT A MARKER ON THTREE WITH A POINTER TO THE OLD THALIST,
        ;; SO IT CAN BE RESTORED.
        =>  (COND ((NULL A)
                    (THPUSH THTREE (LIST 'THREMBIND THOLIST))
                    (RETURN T)))
            ;; OTHERWISE ADD TO THE ALIST THE NEW BINDING CELL
            (THPUSH THALIST
                ;; THE FIRST ELEMENT IS THE NAME OF THE VARIABLE IF THE ENTRY IS AN ATOM,
                ;; THEN WE ARE JUST GIVEN THE VARIABLE AND ITS INITIAL ASSIGNMENT IS "THUNASSIGNED",
                ;; I.E. NO INITIAL ASSIGNMENT.
                (COND ((ATOM (CAR A)) (LIST (CAR A) 'THUNASSIGNED))
                    ;; OTHERWISE OUR ENTRY IS A LIST
                    ;; IF THE FIRST ELEMENT OF THE LIST IS $R OR THRESTRICT
                    ;; WE ADD THE RESTRICTION TO THE BINDING CELL
                    ;; THE CDDR OF THE CELL GIVES THE RESTRICTION LIST
                    ((EQ (CAAR A) 'THRESTRICT) (NCONC (THBI1 (CADAR A)) (CDDAR A)))
                    ;; OTHERWISE WE ARE GIVEN BOTH THE VARIABLE AND ITS
                    ;; INITIAL ASSIGNMENT, SO MAKE THE SECOND ELEMENT OF THE
                    ;; BINDING CELL A POINTER TO THE INITIAL ASSIGNMENT
                    (T (LIST (CAAR A) (EVAL (CADAR A))))))
            (SETQ A (CDR A))
            ;; REPEAT FOR THE NEXT VARIABLE IN THE LIST
            (GO =>))))

(DEFUN THBI1 (X) (COND ((ATOM X) (LIST X 'THUNASSIGNED)) (T (LIST (CAR X) (EVAL (CADR X))))))

(DEFUN THBKPT FEXPR (L) (OR (AND THTRACE (THTRACES 'THBKPT L)) THVALUE))

(DEFUN THBRANCH NIL
    ;; THBRANCH IS CALLED BY THPROGT AND WE ARE SUCCEEDING BACKWARDS.
    ;; CAR THTREE IS THE THPROG MARKING.
    (COND ;; THERE ARE NO MORE EXPRESSIONS TO EXECUTE IN THE THPROG.
        ((NOT (CDADAR THTREE)))
        ((EQ THBRANCH THTREE) (SETQ THBRANCH NIL))
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
        ((RPLACA (CDDAR THTREE) (CONS (LIST THBRANCH THABRANCH (CADAR THTREE)) (CADDAR THTREE)))
            ;; WE NOW SETQ THBRANCH TO NIL.  IF THE NEXT LINE ALSO SUCCEEDS,
            ;; THVAL WILL LOOK FOR A NIL THBRRANCH TO INDICATE THAT IT SHOULD
            ;; SETQ IT AGAIN TO THE POINT OF SUCCESS
            (SETQ THBRANCH NIL))))

(DEFUN THBRANCHUN NIL
    ;; WE ARE NOW FAILING.  THBRANCHUN IS CALLED BY THPROGF.
    (PROG (X) (RETURN
        (COND ;; IF THE SECOND ARG TO THE PROG MARK IS NON-NIL, IT MEANS THAT THERE ARE PREVIOUS LINES IN THE THPROG TO FAIL BACK TO
            ((SETQ X (CADDAR THTREE))
                ;; A COMPAIRISON OF THIS WITH WHAT HAPPEND IN THBRANCK WILL REVEAL THAT
                ;; ALL WE ARE DOING HERE IS RESTORING THE PROG MARK TO IS STATE BEFORE THE LAST SUCCESS.
                (RPLACA (CDAR THTREE) (CADDAR X))
                (RPLACA (CDDAR THTREE) (CDR X))
                ;; RESET THALIST AND THTREE
                (SETQ THALIST (CADAR X))
                (SETQ THTREE (CAAR X))
                T)
            ;; THERE AREN'T ANY MORE THINGS IN THE THPROG TO TRY,
            ;; SO JUST RETURN NIL.
            (T (THPOPT) NIL)))))

(DEFUN THCOND FEXPR (THA)
    (THPUSH THTREE (LIST 'THCOND THA NIL))
    (SETQ THEXP (CAAR THA)))

(DEFUN THCONDF NIL (THOR2 NIL))

(DEFUN THCONDT NIL
    (RPLACA (CAR THTREE) 'THAND)
    (RPLACA (CDAR THTREE) (CAADAR THTREE))
    THVALUE)

(COMMENT THCONSE DEFINES AND OPTIONALLY ASSERTS CONSEQUENT THEOREMS)

(DEFUN THCONSE FEXPR (THX) (THDEF 'THCONSE THX))

(DEFUN THDATA NIL
    (PROG (X)
    =>  (TERPRI)
        (COND ((NULL (SETQ X (READ NIL))) (RETURN T))
            ((PRINT (THADD (CAR X) (CDR X)))))
        (GO =>)))

(COMMENT THDEF DEFINES AND OPTIONALLY ASSERTS THEOREMS)

(DEFUN THDEF (THMTYPE THX)
    (PROG (THNOASSERT? THMNAME THMBODY)
        (COND ((NOT (ATOM (CAR THX)))
                (SETQ THMBODY THX)
                (COND
                    ((EQ THMTYPE 'THCONSE) (SETQ THMNAME (THGENAME TC-G)))
                    ((EQ THMTYPE 'THANTE) (SETQ THMNAME (THGENAME TA-G)))
                    ((EQ THMTYPE 'THERASING) (SETQ THMNAME (THGENAME TE-G)))))
            ((SETQ THMNAME (CAR THX)) (SETQ THMBODY (CDR THX))))    ;; THNOASSERT FEATURE
        (COND ((EQ (CAR THMBODY) 'THNOASSERT)
            (SETQ THNOASSERT? T)
            (SETQ THMBODY (CDR THMBODY))))
        (THPUTPROP THMNAME (CONS THMTYPE THMBODY) 'THEOREM)
        (COND
            (THNOASSERT?  (PRINT (LIST THMNAME 'DEFINED 'BUT 'NOT 'ASSERTED)))
            ((THASS1 (LIST THMNAME) T) (PRINT (LIST THMNAME 'DEFINED 'AND 'ASSERTED)))
            (T (PRINT (LIST THMNAME 'REDEFINED))))
        (RETURN T)))

(DEFUN THDO FEXPR (A)
    (OR (NOT A)
        (PROG2 (THPUSH THTREE (LIST 'THDO A NIL NIL))
            (SETQ THEXP (CAR A)))))

(DEFUN THDO1 NIL
    (RPLACA (CDAR THTREE) (CDADAR THTREE))
    (SETQ THEXP (CAADAR THTREE))
    (COND (THBRANCH
        (RPLACA (CDDAR THTREE) (CONS THBRANCH (CADDAR THTREE)))
        (SETQ THBRANCH NIL)
        (RPLACA (CDDDAR THTREE) (CONS THABRANCH (CAR (CDDDAR THTREE)))))))

(DEFUN THDOB NIL
    (COND ((OR THMESSAGE (NULL (CDADAR THTREE)))
            (RPLACA (CAR THTREE) 'THUNDO)
            T)
        ((THDO1))))

(DEFUN THERASE FEXPR (THA) (THASS1 THA NIL))

(DEFUN THERASEF NIL
    (THADD (COND ((ATOM (CADAR THTREE)) (CADAR THTREE)) (T (CAADAR THTREE)))
        (COND ((ATOM (CADAR THTREE)) NIL) (T (CDADAR THTREE))))
    (THPOPT)
    NIL)

(DEFUN THERASET NIL (PROG2 0 (CADAR THTREE) (THPOPT)))

(COMMENT THERASING DEFINES AND OPTIONALLY ASSERTS ERASING THEOREMS)

(DEFUN THERASING FEXPR (THX) (THDEF 'THERASING THX))

(DEFUN THFAIL FEXPR (THA)
    (AND THA
        (PROG (THTREE1 THA1 THX)
        F   (SETQ THA1 (COND
                ((EQ (CAR THA) 'THEOREM) 'THPROG)
                ((EQ (CAR THA) 'THTAG) 'THPROG)
                ((EQ (CAR THA) 'THINF) (SETQ THINF T) (RETURN NIL))
                ((EQ (CAR THA) 'THMESSAGE) (SETQ THMESSAGE (CADR THA)) (RETURN NIL))
                (T (CAR THA))))
            (SETQ THTREE1 THTREE)
        LP1 (COND ((NULL THTREE1)
                    (PRINT THA)
                    (COND ((ATOM (SETQ THA (THERT NOT FOUND - THFAIL))) (RETURN THA))
                        (T (GO F))))
                ((EQ (CAAR THTREE1) THA1) (GO ELP1)))
        ALP1 (SETQ THTREE1 (CDR THTREE1))
            (GO LP1)
        ELP1 (COND ((EQ (CAR THA) 'THTAG)
                (COND ((MEMQ (CADR THA) (CADDDR (CAR THTREE1))) (GO TAGS))
                    (T (GO ALP1)))))
            (SETQ THMESSAGE (LIST (CDR THTREE1) (AND (CDR THA) (CADR THA))))
            (RETURN NIL)
        TAGS (SETQ THX (CADDAR THTREE1))
        LP2  (COND ((NULL THX) (GO ALP1))
                ((EQ (CAADDR (CAR THX)) (CADR THA))
                    (SETQ THMESSAGE (LIST (CAAR THX) (AND (CDDR THA) (CADDR THA))))
                    (RETURN NIL)))
            (SETQ THX (CDR THX))
            (GO LP2))))

(DEFUN THFAIL? (PRD ACT)
    (THPUSH THTREE (LIST 'THFAIL? PRD ACT))
    THVALUE)

(DEFUN THFAIL?F NIL
    (COND ((EVAL (CADAR THTREE))
            (EVAL (PROG2 (SETQ THMESSAGE NIL) (CADDAR THTREE) (THPOPT))))
        (T (THPOPT) NIL)))

(DEFUN THFAIL?T NIL (THPOPT) THVALUE)

(DEFUN THFINALIZE FEXPR (THA)
    (PROG (THTREE1 THT THX)
        (COND ((NULL THA) (SETQ THA (THERT BAD CALL - THFINALIZE))))
        (COND ((ATOM THA) (RETURN THA))
            ((EQ (CAR THA) 'THTAG) (SETQ THT (CADR THA)))
            ((EQ (CAR THA) 'THEOREM) (SETQ THA (LIST 'THPROG))))
        (SETQ THTREE (SETQ THTREE1 (CONS NIL THTREE)))
    PLUP (SETQ THX (CADR THTREE1))
        (COND ((NULL (CDR THTREE1)) (PRINT THA) (THERT OVERPOP - THFINALIZE))
            ((AND THT (EQ (CAR THX) 'THPROG) (MEMQ THT (CADDDR THX)))
                (GO RTLEV))
            ((OR (EQ (CAR THX) 'THPROG) (EQ (CAR THX) 'THAND))
                (RPLACA (CDDR THX) NIL)
                (SETQ THTREE1 (CDR THTREE1)))
            ((EQ (CAR THX) 'THREMBIND)
                (SETQ THTREE1 (CDR THTREE1)))
            ((RPLACD THTREE1 (CDDR THTREE1))))
        (COND ((EQ (CAR THX) (CAR THA)) (GO DONE)))
        (GO PLUP)
    RTLEV (SETQ THX (CDDR THX))
    LEVLP (COND ((NULL (CAR THX)) (SETQ THTREE1 (CDR THTREE1)) (GO PLUP))
            ((EQ (CAADDR (CAAR THX)) THT) (GO DONE)))
        (RPLACA THX (CDAR THX))
        (GO LEVLP)
    DONE (SETQ THTREE (CDR THTREE))
        (RETURN T)))

(DEFUN THFIND FEXPR (THA)
    (THBIND (CADDR THA))
    (THPUSH THTREE
        (LIST 'THFIND
            (COND ((EQ (CAR THA) 'ALL) ' (1 NIL NIL))               ;; STANDARD ALL
                ((NUMBERP (CAR THA))
                    (LIST (CAR THA) (CAR THA) T))                       ;; SINGLE NUMBER
                ((NUMBERP (CAAR THA)) (CAR THA))                    ;; WINOGRAD CROCK FORMAT
                ((EQ (CAAR THA) 'EXACTLY)
                    (LIST (CADAR THA) (ADD1 (CADAR THA)) NIL))
                ((EQ (CAAR THA) 'AT-MOST)
                    (LIST 1 (ADD1 (CADAR THA)) NIL))
                ((EQ (CAAR THA) 'AS-MANY-AS)
                    (LIST 1 (CADAR THA) T))
                (T (CONS (CADAR THA)                                ;; ONLY THING LEFT IS AT-LEAST
                    (COND ((NULL (CDDAR THA)) (LIST NIL T))         ;; NO AT-MOST
                        ((EQ (CADDAR THA) 'AT-MOST)
                            (LIST (ADD1 (CAR (CDDDAR THA)))
                            NIL))
                        (T (LIST (CAR (CDDDAR THA)) T))))))
            (CONS 0 NIL)
            (CADR THA)))
    (THPUSH THTREE (LIST 'THPROG (CDDR THA) NIL (CDDR THA)))
    (THPROGA))

(DEFUN THFINDF NIL
    (SETQ THBRANCH NIL)
    (COND ((OR THMESSAGE (LESSP (CAADR (SETQ THXX (CDAR THTREE))) (CAAR THXX)))
            (THPOPT)
            NIL)
        (T (THPOPT) (CDADR THXX))))

(DEFUN THFINDT NIL
    (PROG (THX THY THZ THCDAR)
        (SETQ THZ (CADDR (SETQ THCDAR (CDAR THTREE))))
        (AND (MEMBER (SETQ THX (THVARSUBST THZ NIL)) (CDADR THCDAR))
            (GO =>))
        (RPLACD (CADR THCDAR) (CONS THX (CDADR THCDAR)))
        (AND (EQ (SETQ THY (ADD1 (CAADR THCDAR))) (CADAR THCDAR))
            (RETURN (PROG2
                (SETQ THBRANCH NIL)
                (AND (CADDAR THCDAR) (CDADR THCDAR))
                (THPOPT))))
        (RPLACA (CADR THCDAR) THY)
    =>  (SETQ THTREE THBRANCH)
        (SETQ THALIST THABRANCH)
        (SETQ THBRANCH NIL)
        (RETURN NIL)))

(DEFUN THFLUSH FEXPR (A)
    ;; (THFLUSH) FLUSHES ALL ASSERTIONS AND THEOREMS
    ;; INPUT = SEQUENCE OF INDICATORS DEFAULT =
    ;; EFFECT = FLUSHES THE PROPERTIES OF THESE
    ;; (THASSERTION THCONSE THANTE THERASING)
    ;; INDICATORS FROM ALL ATOMS
    (MAPC #'(LAMBDA (B)
        (MAPC #'(LAMBDA (C)
            (MAPC #'(LAMBDA (D)
                (REMPROP D B))
            C))
        (MAKOBLIST NIL)))
    (COND (A) (' (THASSERTION THCONSE THANTE THERASING)))))

(DEFUN THGAL (X Y)
    ;; (THGAL $?X THALIST) RETURNS THE BINDING CELL (X -) OF X ON THALIST
    (SETQ THXX X)
    (SASSQ (CADR X) Y #'(LAMBDA NIL (PRINT THXX) (THERT THUNBOUND THGAL))))

(DEFUN THGENAME FEXPR (X)
    ;; GENERATES UNIQUE NAME WITH ARG AS PREFIX
    (READLIST (NCONC (EXPLODE (CAR X)) (EXPLODE (SETQ THGENAME (ADD1 THGENAME))))))

(DEFUN THGO FEXPR (X)
    (APPLY 'THSUCCEED (CONS 'THTAG X)))

(DEFUN THGOAL FEXPR (THA)                                   ;; THA = (PATTERN RECOMMENDATION)
    (PROG (THY THY1 THZ THZ1 THA1 THA2)                     ;; PATTERN IS EITHER EXPLICIT, THE VALUE OF A
        (SETQ THA2 (THVARSUBST (CAR THA) T))                ;; PLANNER VARIABLE OR THVAL OF $E... THA2 =
        (SETQ THA1 (CDR THA))                               ;; INSTANTIATED PATTERN THA1 = RECOMMENDATIONS
        (COND ((OR (NULL THA1)                              ;; SHOULD DATA BASE BE SEARCHED TRYED IF NO RECS
                (AND (NOT (AND (EQ (CAAR THA1) 'THANUM)
                        (SETQ THA1 (CONS (LIST 'THNUM (CADAR THA1)) (CONS (LIST 'THDBF 'THTRUE) (CDR THA1))))))
                    (NOT (AND (EQ (CAAR THA1) 'THNODB)      ;; TRIED IF REC NOT THNODB OR (THDBF PRED)
                        (PROG2 (SETQ THA1 (CDR THA1)) T)))
                    (NOT (EQ (CAAR THA1) 'THDBF))))
            (SETQ THA1 (CONS (LIST 'THDBF 'THTRUE) THA1))))
        (SETQ THA1 (MAPCAN #'THTRY THA1))                   ;; THMS AND ASSERTIONS SATISFYING RECS APPENDED TO RECS
        (AND THTRACE (THTRACES 'THGOAL THA2))
        (COND ((NULL THA1) (RETURN NIL)))
        (THPUSH THTREE (LIST 'THGOAL THA2 THA1))            ;; (THGOAL PATTERN MATCHES)
        (RPLACD (CDDAR THTREE) 262143)
        (RETURN NIL)))                                      ;; FAILS TO THGOALF

(DEFUN THGOALF NIL
    ;; BASICALLY ALL IT DOES IS TO SEND OFF TO THTRY1 TO TRY ANOTHER POSSIBILITY.
    ;; IF THTRY1 RETURNS NIL, IT MEANS THAT IT COULDN'T FIND ANOTHER POSSIBILITY
    ;; AND WE SHOULD TELL THVAL THAT WE HAVE FAILED.
    ;; ALL THPOPT DOES IS TO LOB THE THGOAL ENTRY OFF THTREE.
    (COND (THMESSAGE (THPOPT) NIL) ((THTRY1)) (T (THPOPT) NIL)))

(DEFUN THGOALT NIL
    (PROG2 0
        (COND ((EQ THVALUE 'THNOVAL) (THVARSUBST (CADAR THTREE) NIL))
            (THVALUE))
        (THPOPT)))

(DEFUN THIP (THI)
    ;; THI IS AN ITEM FROM THE ASSERTION OR PATTERN OF THE THEOREM BEING ENTERED
    (PROG (THT1 THT3 THSV THT2 THI1)
        (SETQ THNF (ADD1 THNF))
        ;; THNF IS A FREE VARIABLE FROM THADD (WHO CALLS THIS BUGER)
        ;; IT SAYS WE ARE LOOKING AT THE N'TH PLACE IN THE PATTERN
        (COND ((AND (ATOM THI) (NOT (EQ THI '?)) (NOT (NUMBERP THI)))
                ;; THI1 IS THE NAME OF THE ATOM TO LOOK UNDER WHEN THI IS A USUAL ATOM
                ;; THI1 = THI NUMBERS DON'T HAVE PROPERTY LISTS SO THEY DON'T COUNT AS
                ;; NORMAL ATOMS, NOR DOES "?" SINCE IT IS A SORT OF VARIABLE IN PLANNER
                (SETQ THI1 THI))
            ((OR (EQ THI '?) (MEMQ (CAR THI) '(THV THNV)))
                ;; SEE IF THI IS A VARIABLE
                (COND (THFST (RETURN 'THVRB))
                    ;; IF WE ARE DOING THIS FOR THE FIRST TIME, DON'T CONSIDER VARIABLES
                    ;; FOR EXPLANATION WHY, SEE THADD
                    ((SETQ THI1 'THVRB))))
            ((RETURN 'THVRB)))
        ;; OTHERWISE THI IS SOMETHING WITH NO PROPERTY LIST LIKE A NUMBER, OR LIST
        ;; RETURNING THVRB TO THADD TELLS IT THAT EVERYTHING IS OK SO
        ;; FAR, BUT NOTHING WAS DONE ON THIS ITEM
        (COND ((NOT (SETQ THT1 (GET THI1 THWH)))
                ;; THWH IS THE NAME OF THE PROPERTY TO LOOK UNDER ON THE ATOM
                ;; IF THIS PROPERTY IS NOT THERE THEN WE MUST PUT IT THERE
                ;; IN PARTICULAR, NO PROPERTY MEANS THAT THE
                ;; ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
                (PUTPROP THI1 (LIST NIL (LIST THNF (LIST THLAS 1 THTTL))) THWH))
            ((EQ THT1 'THNOHASH) (RETURN 'THBQF))
            ;; IF THE PROPERTY IS "THNOHASH" IT MEANS THAT WE
            ;; SHOULD NOT BOTHER TO INDEX UNDER THIS ATOM, SO
            ;; JUST RETURN TO THADD
            ((NOT (SETQ THT2 (ASSQ THNF (CDR THT1))))
                ;; LOOK ON THE PROPERTY LIST ENTRY TO SEE
                ;; IF THERE IS A SUB-ENTRY FOR PATTERNS WITH THIS ATOM IN THE THNF'TH POSITION.
                ;; IF NOT, HACK THE ENTRY SO THERE IS.
                ;; AGAIN THIS IMPLIES THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE.
                (NCONC THT1 (LIST (LIST THNF (LIST THLAS 1 THTTL)))))
            ((NOT (SETQ THT3 (ASSQ THLAS (CDR THT2))))
                ;; NOW LOOK WITHIN THE SUB-ENTRY FOR A SUB-SUB-ENTRY.
                ;; I.E. THOSE PATTERNS WHICH ARE ALSO OF THE CORRECT TOTAL LENGTH
                ;; THLAS IS A VARIABLE FROM THADD WHICH GIVES THE LENGTH OF THE ASSERTEE
                ;; AGAIN, IF NOT THERE, HACK IT IN
                (NCONC THT2 (LIST (LIST THLAS 1 THTTL))))
            ((AND (OR THFST THFSTP)
                    ;; THIS BRANCH SAYS THAT WE STILL NEED TO CHECK THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
                    ;; THIS MEANS THAT WE MUST LOOK DOWN THE REMAINING SUB-SUB-BUCKET LOOKING FOR THE ASSERTEE
                    (COND ((EQ THWH 'THASSERTION) (ASSOC THTT (CDDR THT3)))
                        ;; RANDOMNESS DUE TO THE FACT THAT ASSERTIONS HAVE PROPERY LIST ON THEM,
                        ;; WHILE THEOREM NAMES ARE ATOMS WHOES PROPERTY LISTS ARE OF THE
                        ;; USUAL "INVISIBLE" VARIETY
                        (T (MEMQ THTT (CDDR THT3)))))
                ;; IF THE ASSERTEE IS FOUND, RETURN NIL INDICATING FAILURE
                (RETURN NIL))
            ((SETQ THSV (CDDR THT3))
            ;; HACK IN THE LATEST ENTRY INTO THE SUB-SUB-BUCKET
            (RPLACA (CDR THT3) (ADD1 (CADR THT3)))
            (RPLACD (CDR THT3) (NCONC (LIST THTTL) THSV))))
        ;; IF WE GET TO THIS POINT, EVERYTHING IS OK, SO TELL THADD SO
        (RETURN 'THOK)))

(DEFUN THMATCH2 (THX THY)
    ;; THX IS ONE ITEM FROM THE PATTERN.
    ;; THY IS THE CORESPONDING ITEM FROM THE CANDIDATE.
    ;; THMATCH2 DECIDES IF THE TWO ITEMS REALLY MATCH.

    ;; THOLIST IS THE "THALIST" WHICH WAS IN EXISTANCE BEFORE
    ;; WE STARTED WORKING ON THE CURRENT LINE OF PLANNER CODE
    ;; STANDARD CHECK FOR $E
    (AND (EQ (CAR THX) 'THEV)
        (SETQ THX (THVAL (CADR THX) THOLIST)))
    (AND (EQ (CAR THY) 'THEV)
        (SETQ THY (THVAL (CADR THY) THALIST)))
    (COND
        ;; IF EITHER IS A ? ANYTHING WILL MATCH, SO OK
        ((EQ THX '?))
        ((EQ THY '?))
        ;; IF EITHER IS A VARIABLE THINGS GET MESSY.
        ;; EVERYTHING DOWN TO ***** IS CONCERNED WITH THIS CASE
        ((OR (MEMQ (CAR THX) '(THV THNV THRESTRICT)) (MEMQ (CAR THY) '(THV THNV THRESTRICT)))
        ((LAMBDA (XPAIR YPAIR)
            ;; X AND Y PAIR ARE THE RESPECTIVE BINDING CELLS WHICH WILL HAVE ANY NEW RESTRICTIONS MENTIONED.
            ;; IF THX OR THY IS NOT A VARIABLE (I.E. THE OTHER IS) THEN X OR Y PAIR WILL BE NIL.
            (COND ((AND XPAIR
                ;; THX IS A VARIABLE
                ;; THIS SEES IF THX IS UNASSIGNED
                (OR (EQ (CAR THX) 'THNV)
                    (AND (EQ (CAR THX) 'THV) (EQ (CADR XPAIR) 'THUNASSIGNED)))
                ;; THCHECK MACKES SURE THE RESTRICTIONS (IF ANY) ON
                ;; THX ARE COMPATIBLE WITH THY
                (THCHECK (CDDR XPAIR)
                    (COND (YPAIR (CADR YPAIR)) (T THY))))
                ;; FURTHERMORE, THY IS ALSO A VARIABLE
                ;; THIS MEANS WE MUST DO THE MYSTERIOUS VARIABLE LINKING
                (COND (YPAIR (THRPLACAS (CDR XPAIR) (CADR YPAIR))
                        ;; IF THY ALSO HAS RESTRICTIONS WHEN WE LINK VARIABLES, WE COMBINE RESTRICTIONS
                        (AND (CDDR YPAIR) (THRPLACDS (CDR XPAIR) (THUNION (CDDR XPAIR) (CDDR YPAIR))))
                    (THRPLACDS YPAIR (CDR XPAIR)))
                ;; IF THY IS NOT A VARIALBE, JUST ASSIGN THX TO THY
                ;; THRPLACAS WILL HACK THML THE FREE VARIABLE FROM THMATCH1
                (T (THRPLACAS (CDR XPAIR) THY))))
            ;; IN THIS COND PAIR THY IS A VARIABLE AND THX IS EITHER
            ;; A CONSTANT OR A PREVIOUSLY ASSIGNED VARIALBE
            ((AND YPAIR
                (OR (EQ (CAR THY) 'THNV)
                    ;; FURTHERMORE THY IS UNASSIGNED
                    (AND (EQ (CAR THY) 'THV) (EQ (CADR YPAIR) 'THUNASSIGNED)))
                ;; MAKE SURE RESTRICTIONS ARE OK
                (THCHECK (CDDR YPAIR) (COND (XPAIR (CADR XPAIR)) (T THX))))
                ;; IF THX IS A VARIABLE, LINK
                (COND (XPAIR (THRPLACAS (CDR YPAIR) (CADR XPAIR)))
                    ;; OTHERWISE JUST ASSIGN THY TO THX
                    (T (THRPLACAS (CDR YPAIR) THX))))
            ;; THX IS AN ASSIGED VARIABLE, SO JUST MAKE
            ;; SURE ITS ASSIGNEMENT IS EQUAL TO THY
            ((AND XPAIR (EQUAL (CADR XPAIR) (COND (YPAIR (CADR YPAIR)) (T THY)))))
            ;; THX IS A CONSTANT, THY IS A VARIABLE, AND THEY ARE EQUAL
            ((AND YPAIR (EQUAL (CADR YPAIR) THX)))
            ;; LOOSE, SO RETURN WITH AN ERROR
            (T (ERR NIL))))

            ;; THE FOLLOWING TWO CONDS BIND XPAIR AND YPAIR RESPECTIVELY
            (COND
                ;; IF THX IS A NORMAL VARIALBE, IN PARTICULAR
                ;; WE ARE NOT INTRODUCING NEW RESTRICTIONS AT THIS TIME,
                ;; THEN X PAIR IS JUST THE BINDING LIST
                ((THVAR THX) (THGAL THX THOLIST))
                ;; WE MUST HACK A NEW RESTRICTION ONTO THE BINDING LIST
                ((EQ (CAR THX) 'THRESTRICT)
                ;; WE ARE "RESTRICTING" A ?.  SINCE ? HAS NO
                ;; BINDING LIST, WE MAKE UP A PSEUDO BINDING LIST
                    (COND ((EQ (CADR THX) '?)
                            (PROG2 0
                                (CONS '? (CONS 'THUNASSIGNED (APPEND (CDDR THX) NIL)))
                                (SETQ THX '(THNV ?))))
                        ;; WE ARE RESTRICTING A VARIABLE.  THIS MEANS THAT WE MUST PUT IN ON THE BINDING LIST.
                        (T ((LAMBDA (U)
                                ;; THUNION MAKES SURE WE DON'T PUT THE SAME RESTRICTION ON TWICE.
                                (THRPLACDS (CDR U) (THUNION (CDDR U) (CDDR THX)))
                                (SETQ THX (CADR THX))
                                U)
                            (THGAL (CADR THX) THOLIST))))))
            ;; NOTE THAT IF THX IS NOT A VARIABLE THEN XPAIR IS ()
            ;; WE DO THE EXACT SAME THING FOR THY AS WE JUST DID FOR THX
            (COND ((THVAR THY) (THGAL THY THALIST))
                ((EQ (CAR THY) 'THRESTRICT)
                    (COND ((EQ (CADR THY) '?)
                        (PROG2 0
                            (CONS '? (CONS 'THUNASSIGNED (APPEND (CDDR THY) NIL)))
                            (SETQ THY '(THNV ?))))
                        (T ((LAMBDA (U)
                                (THRPLACDS (CDR U) (THUNION (CDDR U) (CDDR THY)))
                                (SETQ THY (CADR THY))
                                U)
                            (THGAL (CADR THY) THALIST))))))))
        ;; **************
        ;; IF THE TWO ARE EQUAL, NATURALLY THEY MATCH
        ((EQUAL THX THY))
        ;; IF NOT, THEY DON'T, SO REPORT FAILURE
        (T (ERR NIL))))

(DEFUN THCHECK (THPRD THX)
    (OR (NULL THPRD)
        (EQ THX 'THUNASSIGNED)
        (ERRSET (MAPC #'(LAMBDA (THY) (OR (THY THX) (ERR NIL))) THPRD))))

(DEFUN THUNION (L1 L2)
    (MAPC #'(LAMBDA (THX)
            (COND ((MEMBER THX L2))
                (T (SETQ L2 (CONS THX L2)))))
        L1)
    L2)

(DEFUN THMATCH THX
    ((LAMBDA (THOLIST THALIST) (THMATCH1 (ARG 1) (ARG 2)))
        (COND ((GREATERP THX 2) (ARG 3)) (T THALIST))
        (COND ((GREATERP THX 3) (ARG 4)) (T THALIST))))

(DEFUN THMATCH1 (THX THY)
    ;; THX IS THE PATTERN TO BE MATCHED.
    ;; THY IS THE POSSIBLE CANDIDATE.
    ;; THMATCH1 DOES PRELIMINARLY WORK BEFORE HANDING THE PATTERN AND CANDIDATE OFF TO THMATCH2
    ;; WHO DOES THE REAL WORK.
    (PROG (THML)
        ;; THML IS A FREE VARIABLE WHO WILL BE HACKED BY THMATCH2 WHEN THMATCH2 IS DONE,
        ;; THML WILL HAVE A RECORD OF ALL VARIABLE ASSIGNMENTS MADE DURING THE MATCH.
        ;; NATURALLY WE MUST KEEP TRACK SO IF WE FAIL BACK WE CAN UNDO THEM.
        ;; WE HAVE TO CHECK THAT THE PATTERN AND CANDIDATE ARE OF THE SAME LENGTH
        ;; SINCE THE USER MAY HAVE SPECIFIED THE CANDIDATE WITH A "THUSE" RECOMMENDATION.
        (COND ((AND (EQ (LENGTH (COND ((EQ (CAR THX) 'THEV) (SETQ THX (THVAL (CADR THX) THOLIST))) (THX))) (LENGTH THY))
                ;; IF THE MATCH FAILS, THMATCH2 EXITS WITH AN ERR
                ;; WILL BE "TRUE" PROVIDED THE MATCH WORKED
                (ERRSET (MAPC #'THMATCH2 THX THY)))
            ;; SO RECORD THE ASSIGNMENTS ON THTREE
            (AND THML (THPUSH THTREE (LIST 'THMUNG THML)))
            (RETURN T))
        ;; IF THE MATCH FAILED, WE MAY STILL HAVE SOME ASSIGNEMENTS ALREADY MADE.
        ;; THESE MUST IMMEDIATELY BE UNDONE.  EVLIS JUST EVALS EVERYTHING ON THML
        ;; WHICH IS A LIST OF EXPRESSIONS WHICH, WHEN EVALED, UNASSIGN THE VARIABLES
        (T (EVLIS THML) (RETURN NIL)))))

(DEFUN THMATCHLIST (THTB THWH)
    ;; THTB IS A PATTERN WHICH EVENTUALLY IS TO BE MATCHED.
    ;; THWH SAYS IF IT IS AN ASSERTION, CONSEQUENT THEOREM, ETC.
    ;; THMATCHLIST GOES THROUGH THE DATA BASE, LOOKING ON ALL THE BUCKETS OF THE ATOMS IN THE PATTERN.
    ;; IT RETURNS THE SHORTEST BUCKET TO THGOAL.
    (PROG (THB1 THB2 THL THNF THAL THA1 THA2 THRN THL1 THL2 THRVC)
        ;; THL IS THE LENGTH OF THE SHORTEST BUCKET FOUND SO FAR.
        ;; INITIALLY IT IS SET TO A VERY LARGE NUMBER.
        (SETQ THL 34359738367)
        ;; COUNTER WHICH SAYS WHICH PATTERN ITEM WE ARE WORKING ON.
        (SETQ THNF 0)
        ;; LENGTH OF PATTERN.
        (SETQ THAL (LENGTH THTB))
        ;; THB1 WILL BE THE REMAINDER OF THE PATTERN TO YET BE WORKED ON.
        ;; WHEN IT IS NIL, WE ARE DONE, SO RETURN THE BUCKET.
        ;; THL1 IS THE BUCKET UNDER THE ATOM.
        ;; THL2 IS THE BUCKET UNDER THE VARIABLE IN THAT POSITION.
        ;; IF WE ARE WORKING ON AN ASSERTION, THL2 WILL BE () SINCE THERE ARE NO VARIABLES IN ASSERTIONS.
        ;; IN THEOREMS, WE MUST TAKE INTO ACCOUNT THE FACT THAT THE THEOREM MAY HAVE EITHER THE CORRECT ATOM,
        ;; OR A VARIALBE IN A GIVEN POSITION, AND STILL MATCH.
        (SETQ THB1 THTB)
    THP1 (OR THB1
            (RETURN (COND (THL2 (APPEND THL1 THL2)) (THL1))))
        ;; ADD1 TO POSITION COUNTER.
        (SETQ THNF (ADD1 THNF))
        ;; THB2 IS THE ITEM WE ARE WORKING ON IN THIS PASS.
        (SETQ THB2 (CAR THB1))
        ;; UPDATE THB1.
        (SETQ THB1 (CDR THB1))
        ;; IF THE ITEM IS NOT A NORMAL ATOM, SKIP IT AND GO TO NEXT PASS.
    THP3 (COND ((OR (NULL (ATOM THB2)) (NUMBERP THB2) (EQ THB2 '?))
                (GO THP1))
            ;; IF THE ITEM DOES NOT HAVE THE PROPERTY ON ITS PROPERTY LIST,
            ;; THEN IT OBVIOUSLY DOSEN'T HAVE ANY BUCKET AT ALL.
            ;; SO THA1, WHICH RECORDS THE NUMBER IN THE BUCKET IS SET TO 0
            ((NOT (SETQ THA1 (GET THB2 THWH)))
                ;; IF A BUCKET IS FOUND, THE FIRST THING IN THE BUCKET WILL BE THE NUMBER OF GOODIES THERE.
                ;; THE REST WILL BE THE GOODIES.  THE FIRST 0 IN THA1 THEN SAYS THAT THERE WAS NO BUCKET.
                ;; THE SECOND 0 IS JUST A DUMMY FOR THE GOODIES WHICH ARN'T THERE.
                (SETQ THA1 '(0 0)))
            ;; IF IT IS A THNOHASH, WE IGNORE IT JUST LIKE A LIST, OR NUMBER.
            ((EQ THA1 'THNOHASH) (GO THP1))
            ;; SAME IF THERE IS NO SUB-BUCKET FOR THE ATOM IN THE CORRECT POSITION.
            ((NOT (SETQ THA1 (ASSQ THNF (CDR THA1))))
                (SETQ THA1 '(0 0)))
            ;; SAME FOR SUB-SUB-BUCKET (PATTERN LENGTH).
            ((NOT (SETQ THA1 (ASSQ THAL (CDR THA1))))
                (SETQ THA1 '(0 0))))
        (SETQ THRN (CADR THA1))
        (SETQ THA1 (CDDR THA1))
        ;; IF IT'S AN ASSERTION, THEN WE DONT HAVE TO LOOK FOR VARIABLES.
        (AND (EQ THWH 'THASSERTION) (GO THP2))
        ;; THVRB IS THE ATOM WHICH HAS THE BUCKET FOR VARIABLES.
        ;; WE WILL NOW LOOK TO SEE IF THERE ARE ANY THEOREMS WHICH
        ;; HAVE A VARIABLE IN THE CORRECT POSSITION.
        (COND
            ((NOT (SETQ THA2 (GET 'THVRB THWH))) (SETQ THA2 '(0 0)))
            ((NOT (SETQ THA2 (ASSQ THNF (CDR THA2)))) (SETQ THA2 '(0 0)))
            ((NOT (SETQ THA2 (ASSQ THAL (CDR THA2)))) (SETQ THA2 '(0 0))))
        (SETQ THRVC (CADR THA2))
        (SETQ THA2 (CDDR THA2))
        ;; SEE IF THE SUM OF THE NUMBER OF GOODIES IN THE ATOM BUCKET PLUS
        ;; THE NUMBER IN THE VARIABLE BUCKET IS GREATER THAN THE SMALLEST
        ;; NUMBER SO FAR.  IF SO, WE KEEP THE PREVIOUS NUMBER.
        (AND (GREATERP (PLUS THRVC THRN) THL) (GO THP1))
        ;; OTHERWISE THIS BECOMES THE NEW SMALLEST,
        (SETQ THL (PLUS THRVC THRN))
        ;; AND THL1 AND THL2 ARE POINTERS TO THE NEWLY DISCOVERD BUCKETS.
        (SETQ THL1 THA1)
        (SETQ THL2 THA2)
        ;; GO BACK FOR ANOTHER PASS.
        (GO THP1)

        ;; THIS SECTION IS FOR ASSERTIONS, I.E. DON'T HAVE TO CONSIDER VARIABLES.
    THP2 (COND
            ;; IF THERE IS NO BUCKET, THEN RETURN SINCE NOTHING WILL MATCH THE PATTERN.
            ((EQ THRN 0) (RETURN NIL))
            ;; IF THE NEW BUCKET IS SMALLER, IT BECOMES THE SMALLEST SO FAR.
            ((GREATERP THL THRN) (SETQ THL1 THA1) (SETQ THL THRN)))
        ;; GO BACK FOR ANOTHER PASS.
        (GO THP1)))

(DEFUN THMESSAGE FEXPR (THA)
    (THPUSH THTREE (CONS 'THMESSAGE THA))
    THVALUE)

(DEFUN THMESSAGEF NIL
    (PROG (BOD)
        (SETQ BOD (CAR THTREE))
        (THPOPT)
        (COND ((AND (THBIND (CADR BOD)) (THMATCH1 (CADDR BOD) THMESSAGE))
            (THPUSH THTREE (LIST 'THPROG (CDDR BOD) NIL (CDDR BOD)))
            (SETQ THMESSAGE NIL)
            (RETURN (THPROGA)))
            (T (SETQ THALIST THOLIST)))
        (RETURN NIL)))

(DEFUN THMESSAGET NIL (THPOPT) THVALUE)

(DEFUN THMUNGF NIL (EVLIS (CADAR THTREE)) (THPOPT) NIL)

(DEFUN THMUNGT NIL (THPOPT) THVALUE)

(DEFUN THNOFAIL (THX)
    (COND (THX (DEFPROP THPROG THPROGT THFAIL))
            (T (DEFPROP THPROG THPROGF THFAIL))))

(DEFUN THNOHASH FEXPR (THA)
    (MAPC #'(LAMBDA (X) (PUTPROP (CAR THA) 'THNOHASH X))
        (OR (CDR THA) '(THASSERTION THCONSE THANTE THERASING))))

(DEFUN THNOT FEXPR (THA)
    (SETQ THEXP (LIST 'THCOND (LIST (CAR THA) '(THFAIL THAND)) '((THSUCCEED)))))

(DEFUN THNV FEXPR (X) (THV1 (CAR X)))

(DEFUN THOR FEXPR (THA)
    (AND THA
        (THPUSH THTREE (LIST 'THOR THA))
        (SETQ THEXP (CAR THA))))

(DEFUN THOR2 (P)
    (COND (THMESSAGE (THPOPT) NIL)
        ((AND (CADAR THTREE) (CDADAR THTREE))
            (RPLACA (CDAR THTREE) (CDADAR THTREE))
            (SETQ THEXP
                (COND (P (PROG2 0 (CAADAR THTREE) (OR (CADAR THTREE) (THPOPT))))
                    ((CAR (CAADAR THTREE))))))
        (T (THPOPT) NIL)))

(DEFUN THORF NIL (THOR2 T))

(DEFUN THORT NIL (THPOPT) THVALUE)

(DEFUN THPOPT NIL (SETQ THTREE (CDR THTREE)))

(DEFUN THPROG FEXPR (THA)
    ;; THBIND HACKS THALIST TO BIND THE VARIABLES.
    ;; IT ALSO HACKS THTREE SO WE CAN UNDO IT IF NEEDED.
    (THBIND (CAR THA))
    ;; PUT THPROG MARK ON THTREE.
    ;; THE FIRST THA IS A POINTER ONE BEFORE THE NEXT PART OF THE THPROG TO BE HANDELED.
    ;; THE SECOND ONE WILL BE KEPT WHOLE TO SEARCH FOR PROG TAGS.
    (THPUSH THTREE (LIST 'THPROG THA NIL THA))
    ;; CALL WORKHORSE
    (THPROGA))

(DEFUN THPROGA NIL
    ((LAMBDA (X) (COND
            ;; ODD CASE WHERE THE THPROG HAS NO SUBEXPRESSIONS.  RETURN SUCCESS.
            ((NULL (CDAR X)) (THPOPT) 'THNOVAL)
            ;; NEXT ITEM IS AN ATOM, HENCE A THPROG TAG.
            ((ATOM (CADAR X))
                ;; USE THEXP TO MARK IT ON THTREE.
                (SETQ THEXP (LIST 'THTAG (CADAR X)))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA X (CDAR X))
                THVALUE)
            ;; OTHERWISE NEXT EXPRESSION TO BE EVALUATED IS THE NEXT EXPRESSION OF THE THPROG.
            (T (SETQ THEXP (CADAR X))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA X (CDAR X))
                THVALUE)))
        (CDAR THTREE)))

;; THBRANCH AND THBRANCHUN ARE THE MAIN FUNCTIONS IN CHARGE OF HANDLING THE EFFECTS OF SUCCESS AND FAILURE.
;; THEY ARE ONLY CALLED BY THPROGT AND F

(DEFUN THPROGF NIL (THBRANCHUN) NIL)

(DEFUN THPROGT NIL (THBRANCH) (THPROGA))

(DEFUN THPURE (XX)
    ;; CHECKS TO MAKE SURE THAT THE PATTERN HAS NO UNASSIGNED VARIABLES IN IT.

    ;; XX, NATURALLY ENOUGH IS THE PATTERN
    ;; SINCE THPURE IS ALWAYS CALLED AFTER THVARSUBST
    ;; ANY VARIABLES WHICH DO HAVE ASSIGNMENTS WILL HAVE
    ;; GONE AWAY, RREPLACED BY THEIR ASSIGNMENTS
    ;; SO ALL WE NEED DO IS LOOK FOR ANY VARIABLES APPEARING AT ALL
    (ERRSET (MAPC #'(LAMBDA (Y) (AND (THVAR Y) (ERR NIL))) XX)))

(DEFUN THPUTPROP (ATO VAL IND)
    (THPUSH THTREE
        (LIST 'THMUNG
            (LIST (LIST 'PUTPROP
                (LIST 'QUOTE ATO)
                (LIST 'QUOTE (GET ATO IND))
                (LIST 'QUOTE IND)))))
    (PUTPROP ATO VAL IND))

(DEFUN THREM1 (THB)
    ;; THIS FUNCTION IS ROUGHLY THE SAME AS THIP, EXCEPT WHILE THIP ADDS ASSERTIONS TO THE DATABASE, THREM1 REMOVES THEM
    ;; HENCE ALL COMMENTS WILL BE GUIDES TO THE CORRESPONDENCE BETWEEN THREM1 AND THIP

    ;; THB = THI IN THIP
    (PROG (THA THSV THA1 THA2 THA3 THA4 THA5 THONE THPC)
        ;; THA AND THA1 DO THE WORK OF THT1 IN THIP
        ;; THA1 = THT2
        ;; THA3 = THT3
        ;; THA4, THA5, THONE, AND THPC ARE NEW
        (SETQ THNF (ADD1 THNF))
        ;; THIS COND SERVES THE SAME PURPOSE AS THE FIRST COND IN THIP
        (COND ((AND (ATOM THB) (NOT (EQ THB '?)) (NOT (NUMBERP THB)))
                (SETQ THA THB))
            ((OR (EQ THB '?) (MEMQ (CAR THB) '(THV THNV)))
                (COND (THFST (RETURN 'THVRB))
                    ((SETQ THA 'THVRB))))
            ((RETURN 'THVRB)))
        ;; ALL THE REST SERVES THE SAME PURPOSE AS THE SECOND COND IN THIP.
        ;; IT WAS ORIGINALLY WRITTEN AS A SINGLE COND, BUT THE COMPILER BARFED ON IT,
        ;; SO IT WAS BROKEN UP INTO BITE SIZE PIECES.
        (SETQ THA1 (GET THA THWH))
        (OR THA1 (RETURN NIL))
        (AND (EQ THA1 'THNOHASH) (RETURN 'THBQF))
        (SETQ THA2 (THBA THNF THA1))
        (OR THA2 (RETURN NIL))
        (SETQ THA3 (THBA THAL (CADR THA2)))
        (OR THA3 (RETURN NIL))
        (SETQ THA4 (CADR THA3))
        (SETQ THPC (NOT (EQ THWH 'THASSERTION)))
        (SETQ THA5
            (COND ((OR THFST THFSTP) (THBAP THBS (CDR THA4)))
                ((THBA (COND (THPC THON) (T (CAR THON))) (CDR THA4)))))
        (OR THA5 (RETURN NIL))
        (SETQ THONE (CADR THA5))
        (RPLACD THA5 (CDDR THA5))
        (AND (NOT (EQ (CADR THA4) 1))
            (OR (SETQ THSV (CDDR THA4)) T)
            (RPLACA (CDR THA4) (SUB1 (CADR THA4)))
            (RETURN THONE))
        (SETQ THSV (CDDR THA3))
        (RPLACD THA3 THSV)
        (AND (CDADR THA2) (RETURN THONE))
        (SETQ THSV (CDDR THA2))
        (RPLACD THA2 THSV)
        (AND (CDR THA1) (RETURN THONE))
        (REMPROP THA THWH)
        (RETURN THONE)))

(DEFUN THREMBINDF NIL (SETQ THALIST (CADAR THTREE)) (THPOPT) NIL)

(DEFUN THREMBINDT NIL (SETQ THALIST (CADAR THTREE)) (THPOPT) THVALUE)

(DEFUN THREMOVE (THB)
    ;; THIS FUNCTION IS ANALAGOUS TO THADD EXCEPT THREMOVE REMOVES RATHER THAN ADDS.
    ;; AS WITH THREM1, ALL COMMENTS WILL EXPLICATE THE ANALOGY,
    ;; SO ONE SHOULD FIRST BECOME FAMILIAR WITH THADD.

    ;; THB = THTT IN THADD.
    ;; THREMOVE TAKES ONLY ONE ARG SINCE THE PROPERTY LIST FOR THE ASSERTION
    ;; PLAYS NO ROLE IN REMOVING THE ASSERTION.
    (PROG (THB1 THWH THNF THAL THON THBS THFST THFSTP THFOO)
        ;; THB1 AND THON TOGETHER SHARE THE WORK OF THT1 AND THCK IN THADD
        ;; THAL = THLAS
        ;; THBS = THTTL
        (SETQ THNF 0)
        ;; THE STRUCTURE OF THE TWO PROGRAMS IS VIRTUALLY IDENTICAL.
        (SETQ THB1
            (COND ((ATOM THB)
                    (SETQ THBS THB)
                    (SETQ THWH (CAR (SETQ THB1 (GET THB 'THEOREM))))
                    (CADDR THB1))
                ((SETQ THWH 'THASSERTION)
                    (SETQ THBS THB))))
        (SETQ THAL (LENGTH THB1))
        (SETQ THFST T)
    THP1 (COND ((NULL THB1)
                (SETQ THB1 THFOO)
                (SETQ THNF 0)
                (SETQ THFST (SETQ THFOO NIL))
                (SETQ THFSTP T)
                (GO THP1))
            ((NULL (SETQ THON (THREM1 (CAR THB1))))
                (RETURN NIL))
            ((MEMQ THON '(THBQF THVRB))
                (SETQ THFOO (NCONC THFOO (LIST (COND ((EQ THON 'THVRB) (CAR THB1))))))
                (SETQ THB1 (CDR THB1))
                (GO THP1)))
        (SETQ THFST NIL)
        (MAPC #'THREM1 (CDR THB1))
        (SETQ THNF 0)
        (MAPC #'THREM1 THFOO)
        (RETURN THON)))

(DEFUN THREMPROP (ATO IND)
    (THPUSH THTREE
        (LIST 'THMUNG
            (LIST (LIST 'PUTPROP
                (LIST 'QUOTE ATO)
                (LIST 'QUOTE (GET ATO IND))
                (LIST 'QUOTE IND)))))
    (REMPROP ATO IND))

(DEFUN THRESTRICT FEXPR (THB)
    (PROG (X)
        (COND ((ATOM (SETQ X (THGAL (CAR THB) THALIST)))
                (THPRINTC 'THRESTRICT\ IGNORED\ -\ CONTINUING))
            ((THRPLACD (CDR X) (THUNION (CDDR X) (CDR THB)))))
        (RETURN X)))

(DEFUN THRETURN FEXPR (X)
    (APPLY 'THSUCCEED (CONS 'THPROG X)))

(DEFUN THRPLACA (X Y)
    (PROG (THML)
        (THRPLACAS X Y)
        (THPUSH THTREE (LIST 'THMUNG THML))
        (RETURN X)))

(DEFUN THRPLACAS (X Y)
    (THPUSH THML (LIST 'THURPLACA X (CAR X)))
    (RPLACA X Y))

(DEFUN THURPLACA FEXPR (L) (RPLACA (CAR L) (CADR L)))

(DEFUN THRPLACD (X Y)
    (PROG (THML)
        (THRPLACDS X Y)
        (THPUSH THTREE (LIST 'THMUNG THML))
        (RETURN X)))

(DEFUN THRPLACDS (X Y)
    (THPUSH THML (LIST 'THURPLACD X (CDR X)))
    (RPLACD X Y))

(DEFUN THURPLACD FEXPR (L) (RPLACD (CAR L) (CADR L)))

(DEFUN THSETQ FEXPR (THL1)
    (PROG (THML THL)
        (SETQ THL THL1)
    =>  (COND
            ((NULL THL)
                (THPUSH THTREE (LIST 'THMUNG THML))
                (RETURN THVALUE))
            ((NULL (CDR THL))
                (PRINT THL1)
                (THERT ODD NUMBER OF GOODIES - THSETQ))
            ((ATOM (CAR THL))
                (THPUSH THML (LIST 'SETQ (CAR THL) (LIST 'QUOTE (EVAL (CAR THL)))))
                (SET (CAR THL) (SETQ THVALUE (EVAL (CADR THL)))))
            (T (THRPLACAS (CDR (THSGAL (CAR THL)))
                (SETQ THVALUE (THVAL (CADR THL) THALIST)))))
        (SETQ THL (CDDR THL))
        (GO =>)))

(DEFUN THSGAL (X)
    (SASSQ (CADR X) THALIST
        #'(LAMBDA NIL (PROG (Y)
            (SETQ Y (LIST (CADR X) 'THUNASSIGNED))
            (NCONC (GET 'THALIST 'VALUE) (LIST Y))
            (RETURN Y)))))

(DEFUN THSTATE FEXPR (THINDICATORS)
    ;; PRINTS THAT PART OF THE STATE OF THE MICRO-PLANNER WORLD SPECIFIED BY THE INDICATORS IN REREADABLE FORM.
    ;; NOTE THAT IT IS BLIND TO ASSERTIONS THAT BEGIN WITH EITHER NUMBERS, LIST STRUCTURE, NOHASHED ATOMS OR NON-INTERNED ATOMS.
    (PROG (THP)
        (PRINT '(THDATA))
        (MAPC #'(LAMBDA (BUCKET)
            (MAPC #'(LAMBDA (THATOM)
                (MAPC #'(LAMBDA (THWH)
                    (AND (SETQ THP (GET THATOM THWH)) (SETQ THP (ASSOC 1 (CDR THP)))
                        (MAPC #'(LAMBDA (LENGTH-BUCKET)
                            (MAPC #'(LAMBDA (ASRT)
                                (COND ((EQ THWH 'THASSERTION) (PRINT ASRT)) ((PRINT (LIST ASRT)))))
                            (CDDR LENGTH-BUCKET)))
                        (CDR THP))))
                (COND (THINDICATORS) (' (THASSERTION THANTE THCONSE THERASING)))))
            BUCKET))
        (MAKOBLIST NIL))
        (PRINT NIL)))

(DEFUN THSUCCEED FEXPR (THA)
    (OR (NOT THA)
        (PROG (THX)
            (AND (EQ (CAR THA) 'THEOREM)
                (SETQ THA (CONS 'THPROG (CDR THA))))
            (SETQ THBRANCH THTREE)
            (SETQ THABRANCH THALIST)
        =>  (COND
                ((NULL THTREE)
                    (PRINT THA)
                    (THERT OVERPOP - THSUCCEED))
                ((EQ (CAAR THTREE) 'THREMBIND)
                    (SETQ THALIST (CADAR THTREE))
                    (THPOPT)
                    (GO =>))
                ((EQ (CAAR THTREE) (CAR THA))
                    (THPOPT)
                    (RETURN (COND ((CDR THA) (EVAL (CADR THA))) ('THNOVAL))))
                ((AND (EQ (CAR THA) 'THTAG) (EQ (CAAR THTREE) 'THPROG) (SETQ THX (MEMQ (CADR THA) (CADDDR (CAR THTREE)))))
                    (RPLACA (CDAR THTREE) (CONS NIL THX))
                    (RETURN (THPROGT)))
                (T (THPOPT) (GO =>))))))

(DEFUN THTAE (XX)
    (COND
        ((ATOM XX) NIL)
        ((EQ (CAR XX) 'THUSE)
            (MAPCAR #'(LAMBDA (X)
                    (COND ((NOT (AND (SETQ THXX (GET X 'THEOREM)) (EQ (CAR THXX) TYPE)))
                            (PRINT X)
                            (LIST 'THAPPLY (THERT BAD THEOREM \-THTAE) (CAR THX)))
                        (T (LIST 'THAPPLY X (CAR THX)))))
                (CDR XX)))
        ((EQ (CAR XX) 'THTBF)
            (MAPCAN #'(LAMBDA (Y)
                    (COND (((CADR XX) Y)
                        (LIST (LIST 'THAPPLY Y (CAR THX))))))
                (COND (THY1 THY)
                    ((SETQ THY1 T)
                        (SETQ THY (THMATCHLIST (CAR THX) TYPE))))))
    (T (PRINT XX) (THTAE (THERT UNCLEAR RECCOMMENDATION \-THTAE)))))

(DEFUN THTAG FEXPR (L)
    (AND (CAR L)
        (THPUSH THTREE (LIST 'THTAG (CAR L)))))

(DEFUN THTAGF NIL (THPOPT) NIL)

(DEFUN THTAGT NIL (THPOPT) THVALUE)

(DEFUN THTRUE (X) T)

(DEFUN THTRY1 NIL                                           ;; TRIES NEXT RECOMMENDATION ON TREE FOR THGOAL
    (PROG (THX THY THZ THW THEOREM)
        (SETQ THZ (CAR THTREE))                             ;; = (THGOAL PATTERN EXPANDED-RECOMMENDATIONS)
        (SETQ THY (CDDR THZ))                               ;; = RECOMMENDATIONS
        (RPLACD THY (SUB1 (CDR THY)))
    NXTREC (COND ((OR (NULL (CAR THY)) (ZEROP (CDR THY)))
            (RETURN NIL)))                                  ;; RECOMMENDATIONS EXHAUSTED. FAIL
        (SETQ THX (CAAR THY))
        (GO (CAR THX))
    THNUM (RPLACD THY (CADR THX))
        (RPLACA THY (CDAR THY))
        (GO NXTREC)
    THDBF (SETQ THOLIST THALIST)
        (COND ((NULL (CADDR THX))
                (RPLACA THY (CDAR THY))
                (GO NXTREC))                                ;; NO MORE CANDIDATES SATISFYING THIS REC, TRY NEXT
            ((PROG2 0
                    (AND ((CADR THX) (SETQ THW (CAADDR THX))) (THMATCH1 (CADR THZ) (CAR THW)))
                    (RPLACA (CDDR THX) (CDADDR THX)))
                (RETURN THW))
            (T (GO THDBF)))
    THTBF (COND ((NULL (CADDR THX))
                (RPLACA THY (CDAR THY))
                (GO NXTREC)))                               ;; NO MORE CANDIDATES SATISFYING THIS REC, TRY NEXT
        (SETQ THEOREM (CAADDR THX))
    THTBF1 (COND ((NOT (AND (SETQ THW (GET THEOREM 'THEOREM)) (EQ (CAR THW) 'THCONSE)))
                (PRINT THEOREM)
                (COND ((EQ (SETQ THEOREM (THERT BAD THEOREM - THTRY1)) 'T)
                        (GO NXTREC))
                    (T (GO THTBF1)))))
        (COND ((PROG2 0
                    (AND ((CADR THX) (CAADDR THX)) (THAPPLY1 THEOREM THW (CADR THZ)))
                    (RPLACA (CDDR THX) (CDADDR THX)))
                (RETURN T))
            (T (GO THTBF)))))

(DEFUN THTRY (X)
    ;; THTRY IS IN CHARGE OF MAKING UP THE "THINGS TO DO" LIST, WHICH IS PUT ON THTREE.
    ;; SO WHENEVER WE FAIL BACK TO A THGOAL, WE GO TO THE NEXT "THING TO DO".
    ;; X IS THE LIST OF RECOMMENDATIONS.
    (COND ;; ANY ATOMIC RECOMMENDATION IS IGNORED.  THIS IS USEFUL IN ERROR RECOVERY.
        ((ATOM X) NIL)
        ;; HAVE A THEOREM BASE FILTER
        ((EQ (CAR X) 'THTBF)
            ;; MAKE UP A LIST WHICH GIVES
            ;; 1 - THE INDICATOR "THTBF"
            ;; 2 - THE ACTUAL FILTER (THTRUE IS THE MOST COMMON)
            ;; 3 - THE BUCKET RETURNED BY THMATCHLIST
            (COND ((NOT THZ1) (SETQ THZ1 T) (SETQ THZ (THMATCHLIST THA2 'THCONSE))))
            (COND (THZ (LIST (LIST 'THTBF (CADR X) THZ))) (T NIL)))
        ;; DO THE SAME THING, ONLY FOR DATA BASE FILTERS.
        ((EQ (CAR X) 'THDBF)
            (COND ((NOT THY1) (SETQ THY1 T) (SETQ THY (THMATCHLIST THA2 'THASSERTION))))
            (COND (THY (LIST (LIST 'THDBF (CADR X) THY))) (T NIL)))
        ;; THUSE STATEMENTS ARE TRANSLATED INTO THTBF THTRUE STATEMENTS,
        ;; WHICH THE "BUCKET" IS THE LIST GIVEN IN THE THUSE.
        ((EQ (CAR X) 'THUSE)
            (LIST (LIST 'THTBF 'THTRUE (CDR X))))
        ((EQ (CAR X) 'THNUM)
            (LIST X))
        (T (PRINT X) (THTRY (THERT UNCLEAR RECOMMENDATION - THTRY)))))

(DEFUN THUNDOF NIL
    (COND ((NULL (CADDAR THTREE)) (THPOPT))
        (T (SETQ THXX (CDDAR THTREE))
            (SETQ THALIST (CAADR THXX))
            (RPLACA (CDR THXX) (CDADR THXX))
            (SETQ THTREE (CAAR THXX))
            (RPLACA THXX (CDAR THXX))))
    NIL)

(DEFUN THUNDOT NIL (THPOPT) T)

(DEFUN THUNIQUE FEXPR (THA)
    (SETQ THA (CONS 'THUNIQUE (MAPCAR #'EVAL THA)))
    (PROG (X)
        (SETQ X THALIST)
    =>  (COND ((NULL X) (THPUSH THALIST THA) (RETURN T))
            ((EQ (CAAR X) 'THUNIQUE)
                (COND ((EQUAL (CAR X) THA) (RETURN NIL)))))
        (SETQ X (CDR X))
        (GO =>)))

(DEFUN THV1 (X)
    ;; (THV1 'X) IS THE VALUE OF THE PLANNER VARIABLE.
    ;; $?X RETURNS ERROR MESSAGE IF X UNBOUND OR UNASSIGNED.
    (SETQ THXX X)
    (COND ((EQ (SETQ X (CADR (SASSQ X THALIST #'(LAMBDA NIL (PRINT THXX) (THERT THUNBOUND - THV1))))) 'THUNASSIGNED)
            (PRINT THXX)
            (THERT THUNASSIGNED - THV1))
        (T X)))

(DEFUN THV FEXPR (X)
    ;; (THV X) IS THE VALUE OF THE PLANNER VARIABLE $?X
    (THV1 (CAR X)))

(DEFUN THVAL (THEXP THALIST)
    ;; CORRESPONDS TO LISP EVAL.
    ;; THEXP IS THE EXPRESSION TO BE THVALUATED.
    ;; THALIST IS THE VARIABLE BINDING LIST.

    ;; ALL THPUSH DOES IS TO CONSE ON THE SECOND ITEM TO THE FIRST.
    (THPUSH THLEVEL (LIST THTREE THALIST))
    (PROG (THTREE THVALUE THBRANCH THOLIST THABRANCH THE THMESSAGE)
        (SETQ THV '(THV THNV))
        (SETQ THVALUE 'THNOVAL)

        ;; "THE" BECOMES THE CURRENT EXPRESSION.
        ;; THEXP IS RESERVED FOR FURTHER EXPRESSIONS,
        ;; WHICH SHOULD BE THVALED BEFORE WE GO TO THE NEXT ITEM OF ACTUAL CODE.
        ;; FOR EXAMPLE, THASSERT USES THIS FEATURE TO PROCESS ANTECEDENT THEOREMS.
    GO  (SETQ THE THEXP)
        (SETQ THEXP NIL)
        ;; TYPING ^A (CONTROL A) AT MAC-AI LISP CAUSES ^A (UPARROW A) TO BE SET TO T.
        ;; THIS CAN BE DONE WHILE A FUNCTION IS BEING PROCESSED.
        ;; THE NET EFFECT IS TO TEMPORARILY HALT EVALUATION.
        (COND (^A (SETQ ^A NIL)
            (OR (THERT ^A - THVAL) (GO FAIL))))
        ;; THSTEP AND ITS RELATIVES ARE FOR STEPPING THROUGH PLANNER FUNCTIONS IN A SPECIAL WAY.
        ;; TO THIS DATE ONLY SUSSMAN KNOWS EXACTLY WHAT IT IS SUPPOSE TO DO.
        ;; YOU CAN SAFELY IGNORE ANY EXPRESSION WHICH MENTIONS IT.
        (COND (THSTEP (EVAL THSTEP)))
        ;; EVAL THE CURRENT EXPRESSION TO BE THVALED.
        ;; NOTE THAT EACH PLANNER FUNCTION CORRESPONDS TO THREE LISP FUNCTIONS:
        ;; ONE TO SET THINGS UP (THIS IS WHAT IS GETTING EVALED AT THIS POINT),
        ;; ONE TO HANDLE SUCCESS AND ONE FOR FAILURE.
        (COND ((ERRSET (SETQ THVALUE (EVAL THE))))
            ;; IF THERE WAS A LISP ERROR, REPORT IT TO THE USER.
            (T (PRINT THE)
                (SETQ THVALUE (THERT LISPERROR - THVAL))))
    GO1 (COND (THSTEPD (EVAL THSTEPD)))
        ;; USUALLY THEMESSAGE WILL BE NIL.
        ;; EXCEPTION IS WHEN USER HAS USED THE THMESSAGE FUNCTION.
        (COND (THMESSAGE (GO MFAIL))
            ;; IF THEXP IS NON NIL, IT MEANS THAT WE HAVE MORE PLANNER TO WORK ON BEFORE GOING TO NEXT LINE OF USER CODE.
            (THEXP (GO GO))
            ;; IF THVALUE IS NON NIL, IT MEANS THAT SO FAR THE THEOREM IS SUCCEEDING.
            (THVALUE (GO SUCCEED))
            ;; ELSE WE ARE IN A FAILURE SITUATION.
            (T (GO FAIL)))
    SUCCEED ;; HANDLES SUCCESS
        (COND (THSTEPT (EVAL THSTEPT)))
        ;; SAVE CURRENT STATE OF THTREE AND THALIST IN CASE WE HAVE TO BACK UP.
        (COND ((NULL THBRANCH)
            (SETQ THBRANCH THTREE)
            (SETQ THABRANCH THALIST)))
        ;; IF THE THTREE IS NIL, IT MEANS THAT THE THPROG OR WHATEVER HAS BEEN COMPLETED,
        ;; SO THERE ARE NO MORE EXPRESSIONS TO DO.  ALL THEOREMS ACT LIKE A THPROG,
        ;; INCLUDING PUTTING ITS MARK ON THTREE, SEE THAPPLY, HENCE NO NEED TO GROW MORE BRANCHES ON THTREE.
        (COND ((NULL THTREE)
                (SETQ THLEVEL (CDR THLEVEL))
                (RETURN THVALUE))
            ;; THIS IS THE NORMAL CASE.
            ;; WE EVAL THE SUCCEED-FUNCTION OF THE PLANNER FUNCTION WHICH JUST SUCCEEDED.
            ((SETQ THEXP (GET (CAAR THTREE) 'THSUCCEED))
                (GO GO2))
            ;; IN CASE OF LOSSAGE, LETS THE USER SUCCEED ANYWAY.
            ((THERT BAD SUCCEED - THVAL)
                (GO SUCCEED))
            ((GO FAIL)))
        ;; HAS TO DO WITH FAILURE + MESSAGE
    MFAIL (COND ((EQ (CAR THMESSAGE) THTREE)
            (SETQ THEXP (CADR THMESSAGE))
            (SETQ THMESSAGE NIL)
            (GO GO)))
    FAIL (COND (THSTEPF (EVAL THSTEPF)))
        ;; IF THTREE IS NIL, WE HAVE FAILED THE ENTIRE EXPRESSION.
        (COND ((NULL THTREE)
                (SETQ THLEVEL (CDR THLEVEL))
                (RETURN NIL))
            ;; NORMAL CASE.  EVAL THE FAILURE FUNCTION ASSOCIATED
            ;; WITH THE PLANNER FUNCTION WHICH JUST FAILED.
            ((SETQ THEXP (GET (CAAR THTREE) 'THFAIL))
                (GO GO2))
            ((THERT BAD FAIL - THVAL)
                (GO SUCCEED))
            ((GO FAIL)))
        ;; THEXP AT THIS POINT IS THE APPROPRIATE SUCCESS OR FAILURE ASSOCIATED FUNCTION.
        ;; EVAL IT AND AT THE SAME TIME,
        ;; SET IT TO NIL IN CASE WE NEED THEXP FOR MORE EXPRESSIONS TO BE PROCESSED.
    GO2 (SETQ THVALUE ((PROG2 0 THEXP (SETQ THEXP NIL))))
        ;; GO THROUGH THE ENTIRE PROCESS AGAIN.
        ;; A TYPICAL PROCESS IN SUCCESS IS TO KEEP REMOVING EXPRESSIONS FROM THTREE
        ;; UNTIL WE GET BACK TO THE THREE ENTRY PUT ON BY THPROG.
        ;; AT THIS POINT IT EVALS THPROGT, AND SEE THAT LISTING
        (GO GO1))))

(DEFUN THVAR (X)
    ;; PREDICATE - IS ITS INPUT A PLANNER VARIABLE
    (MEMQ (CAR X) '(THV THNV)))

(DEFUN THVARS2 (X)
    ;; THIS IS THE WORKHORSE FOR THVARSUBST.
    ;; X IS A SINGLE ITEM FROM A PATTERN.
    (PROG (A)
        (AND (ATOM X) (RETURN X))
        ;; IF IT'S AN ATOM, NOTHING NEED BE DONE.
        (AND (EQ (CAR X) 'THEV)
            (SETQ X (THVAL (CADR X) THALIST)))
        ;; IF THE EXPRESSION HAS A $E BEFORE IT, THVAL BEFORE GOING ON.
        (OR (THVAR X) (RETURN X))
        ;; IF THE ITEM IS NOT A VARIABLE, IT MUST BE SOME RANDOM LIST, SO IT HAS NO ASSIGNED VALUE.
        (SETQ A (THGAL X THALIST))
        ;; AT THIS POINT X MUST BE A VARIABLE, SO FIND ITS ASSIGNMENT, THAT'S WHAT THGAL DOES.
        ;; THALIST IS WHERE THE VARIABLE ASSIGNMENTS RESIDE.
        (RETURN (COND
            ((EQ (CADR A) 'THUNASSIGNED) X)
            ;; IF THE VARIABLE IS UNASSIGNED, THEN RETURN THE ACTUAL VARIABLE.
            ((AND THY (EQ (CAR X) 'THNV))
                ;; THY WILL BE T JUST IN THE CASES WHERE THVARSUBST WAS CALLED BY A THGOAL SITUATION.
                ;; IT IS THEN NECESSARY TO IMMEDIATELY HACK IN A THUNASSIGNED SO THAT IF THE SAME VARIABLE IS USED
                ;; TWICE IN THE SAME PATTERN, WE WON'T PUT IN ITS OLD VALUE THE SECOND TIME IT IS ENCOUNTERED.
                (THRPLACA (CDR A) 'THUNASSIGNED)
                X)
            ;; OTHERWISE THE ASSIGNMENT IS THE SECOND ELEMENT IN THE BINDING LIST.
            (T (CADR A))))))

(DEFUN THVARSUBST (THX THY)
    ;; THX IS A GOAL OR ASSERTION PATTERN OR THEOREM NAME.
    ;; THIS FUNCTION RETURNS THE SAME PATTERN, EXCEPT IN PLACE OF ALL ASSIGNED VARIABLES
    ;; THERE WILL BE THE VALUES THEY ARE ASSIGNED TO.
    (COND ((EQ (CAR THX) 'THEV)
            ;; IF THE CAR IS THEV, IT MEANS THAT THERE WAS A $E BEFORE THE PATTERN,
            ;; IN WHICH CASE WE ARE TO GET THE REAL PATTERN BY THVALUATING WHAT IS THERE.
            (SETQ THX (THVAL (CADR THX) THALIST)))
        ((THVAR THX)
            (SETQ THX (EVAL THX))))
    ;; THVAR TESTS TO SEE IF ARG IS A VARIABLE.
    ;; IF THE PATTERN IS A SINGLE VARIABLE, THE PROGRAM ASSUMES THERE SHOULD BE AN IMPLICIT THVAL.
    ;; UNLESS THE ASSERTEE IS A THEOREM NAME, GO THROUGH IT PLACE BY PLACE WITH THVARS2.
    (COND ((ATOM THX) THX) (T (MAPCAR #'THVARS2 THX))))

(DEFUN THVSETQ FEXPR (THA)
    (PROG (A)
        (SETQ A THA)
    =>  (COND ((NULL A) (RETURN THVALUE))
            ((NULL (CDR A))
                (PRINT THA)
                (THERT ODD NUMBER OF GOODIES-THSETQ))
            (T (SETQ THVALUE
                (CAR (RPLACA (CDR (THSGAL (CAR A))) (THVAL (CADR A) THALIST))))))
        (SETQ A (CDDR A))
        (GO =>)))

(DEFPROP THTAG THTAGF THFAIL)
(DEFPROP THTAG THTAGT THSUCCEED)
(DEFPROP THGOAL THGOALT THSUCCEED)
(DEFPROP THGOAL THGOALF THFAIL)
(DEFPROP THFAIL? THFAIL?F THFAIL)
(DEFPROP THFAIL? THFAIL?T THSUCCEED)
(DEFPROP THAMONG THAMONGF THFAIL)
(DEFPROP THFIND THFINDF THFAIL)
(DEFPROP THFIND THFINDT THSUCCEED)
(DEFPROP THPROG THPROGT THSUCCEED)
(DEFPROP THAND THANDT THSUCCEED)
(DEFPROP THMUNG THMUNGT THSUCCEED)
(DEFPROP THERASE THERASET THSUCCEED)
(DEFPROP THASSERT THASSERTT THSUCCEED)
(DEFPROP THOR THORT THSUCCEED)
(DEFPROP THCOND THCONDT THSUCCEED)
(DEFPROP THAND THANDF THFAIL)
(DEFPROP THPROG THPROGF THFAIL)
(DEFPROP THMUNG THMUNGF THFAIL)
(DEFPROP THASSERT THASSERTF THFAIL)
(DEFPROP THERASE THERASEF THFAIL)
(DEFPROP THCOND THCONDF THFAIL)
(DEFPROP THOR THORF THFAIL)
(DEFPROP THDO THDOB THSUCCEED)
(DEFPROP THDO THDOB THFAIL)
(DEFPROP THUNDO THUNDOF THFAIL)
(DEFPROP THUNDO THUNDOT THSUCCEED)
(DEFPROP THMESSAGE THMESSAGEF THFAIL)
(DEFPROP THMESSAGE THMESSAGET THSUCCEED)
(DEFPROP THREMBIND THREMBINDT THSUCCEED)
(DEFPROP THREMBIND THREMBINDF THFAIL)

(DEFUN THPRINT2 (X) (PRINC '\ ) (PRINC X))

(DEFUN THERT FEXPR (\0ERTA)
    ;; THERT IS THE BREAK FUNCTION, AND ALSO THE TOP LEVEL FUNCTION.
    ;; IT IS CALLED DIRECTLY BY LISP BEFORE LISP GOES INTO THE READ EVAL LOOP.
    ;; FOR HOW THIS IS DONE, SEE MAC-AI LISP DOCUMENTATION.
    ;; IN ESSENCE, THERT CONTAINS ITS OWN LOOP, WHICH IS READ THVAL.
    ;; \0ERTA IS THE ERROR MESSAGE TO BE PRINTED OUT WHEN THERT IS USED FOR ERROR BREAKING.
    (PROG (\0LISTEN ^W ^Q)
        (PRINT '>>>)
        (COND ((MAPC #'THPRINT2 \0ERTA)                     ;; THE NORMAL MESSAGE PRINTOUT
            (PRINT 'LISTENING)
            (OR THLEVEL (THPRINT2 'THVAL))))                ;; IF WE ARE AT TOP LEVEL, THLEVEL WILL BE NIL
    =>  (SETQ THINF NIL)                                    ;; GO INTO READ LOOP
        (TERPRI)
        (ERRSET (COND ((EQ (SETQ \0LISTEN (READ)) 'P)     ;; READ IN S EXPRESSION
            (RETURN T))                                     ;; $P IMPLIES PROCEDE
            ((AND (NOT (ATOM \0LISTEN))                     ;; ($P EXP) IMPLIES PROCEDE AND OUTPUT (EVAL EXP)
                (EQ (CAR \0LISTEN) 'P))
            (RETURN (EVAL (CADR \0LISTEN))))
            (THLEVEL (PRINT (EVAL \0LISTEN)))               ;; EVAL LISTENING IF NOT AT TOP LEVEL
            (T (PRINT (THVAL \0LISTEN THALIST)))))          ;; THVAL LISTENING AT TOP LEVEL
        (GO =>)))

(DEFUN THINIT FEXPR (L)
    (SETQ THGENAME 0)
    (SETQ THSTEP NIL)
    (SETQ THSTEPD NIL)
    (SETQ THSTEPT NIL)
    (SETQ THSTEPF NIL)
    (SETQ THXX NIL)
    (SETQ THTRACE NIL)
    (SETQ THALIST '((NIL NIL)))
    (SSTATUS MACRO $ 'THREAD)
    (SETQ ERRLIST
        '((PRINT 'MICRO-PLANNER)
            (PRINC THVERSION)
            (SETQ ERRLIST (CDDDDR ERRLIST))
            (SETQ THINF NIL)
            (SETQ THTREE NIL)
            (SETQ THLEVEL NIL)
            (THERT TOP LEVEL))))

#_(ns shrdlu.thtrac)

(COMMENT FOR PLNR 159 AND GREATER, THPRINTC CAN BE ELIMINATED)

(DEFUN THPRINTC (X) (TERPRI) (PRINC X) (PRINC '\ ))

;; SYSTEM FUNCTIONS SUCH AS THGOAL, THASSERT, THERASE AND THEOREM
;; (ALL THMS) ARE TRACED IF THEY ARE ON 'THTRACE'.
;; THTRACES1 PUTS THEM THERE AND THUNTRACE TAKES THEM OFF.

;; THTRACE IS INITIALLY SET TO NIL BY TS PLNR.

(DEFUN THTRACE FEXPR (L) (MAPC #'THTRACE1 L))

(DEFUN THTRACE1 (X)
    (PROG (Y)
        ;; VARIETY OF POSSIBLE INPUT FORMATS TRANSFORMED TO STANDARD 3 ELEMENT LIST
        ;; (OBJECT-TO-BE-TRACED TRACE-CONDITION BREAK-CONDITION)
        (SETQ X (COND
            ((ATOM X) (LIST X T NIL))
            ((CDDR X) X)
            ((NULL (CDR X)) (PRINT X) (PRINC 'BAD\ FORMAT) (RETURN NIL))
            ((LIST (CAR X) (CADR X) NIL))))
        ;; IF OBJECT-TO-BE-TRACED IS A PARTICULAR THEOREM, THEN THE TRIPLET
        ;; '(THEOREM (THSEL 'CADR) (THSEL 'CADDDR)) IS GUARANTEED TO
        ;; BE ON THTRACE IN ADDITION TO THE STANDARD TRIPLET.
        (COND ((GET (CAR X) 'THEOREM)
            (COND ((SETQ Y (ASSQ 'THEOREM THTRACE)) (RPLACD Y '((THSEL 'CADR) (THSEL 'CADDR))))
                ((SETQ THTRACE (LIST X (APPEND '(THEOREM (THSEL 'CADR) (THSEL 'CADDR)) THTRACE)))))))
        ;; THTRACE IS UPDATED.  IF THE OBJECT-TO-BE-TRACED IS ALREADY ON THTHRACE, THEN
        ;; THE TRACE AND BREAK CONDITIONS ARE UPDATED, ELSE THE WHOLE TRIPLET IS PLACED ON THTRACE.
        (COND ((SETQ Y (ASSQ (CAR X) THTRACE)) (RPLACD Y (CDR X)))
            ((SETQ THTRACE (CONS X THTRACE))))
        (RETURN X)))

;; THUNTRACE REMOVES ELEMENTS OF ITS ARG FROM THTRACE.
;; IF NOT GIVEN ANY ARGS, THUNTRACE SETS THTRACE TO NIL.

(DEFUN THUNTRACE FEXPR (L)
    (COND (L (SETQ THTRACE (MAPCAN #'(LAMBDA (X)
                        (COND ((MEMQ (CAR X) L) (PRINT X) NIL) ((LIST X))))
                    THTRACE)))
        ((MAPC #'PRINT THTRACE) (SETQ THTRACE NIL)))
    'DONE)

;; THTRACES IS ACTIVATED BY THGOAL, THASSERT, ... IF THTRACE IS NON-NIL,
;; THF IS SET TO THE PARTICULAR CANDIDATE FOR TRACEAGE, E.G. TO 'THGOAL
;; IF THE PLANNER FUNCTION THGOAL ACTIVATED THTRACES.
;; THL = THE INSTANTIATED ARG OF THF. SEE DESC OF X ON NEXT PAGE.

(DEFUN THTRACES (THF THL)
    (PROG (THY THZ THB)
        (AND
            ;; THY SET TO TRIPLET ON THTRACE. IF NOT THERE, NO TRACING
            (SETQ THY (ASSQ THF THTRACE))
            ;; IF BOTH TRACE AND BREAK ARE FALSE, DON'T TRACE
            ;; SIDE EFFECT - THB SET TO VALUE OF BREAK
            (OR (SETQ THB (THVAL (CADDR THY) THALIST)) (THVAL (CADR THY) THALIST))
            ;; THZ IS SET TO THE TRACE FUNCTION FOR THE OBJECT-TO-BE-TRACED
            (OR (SETQ THZ (GET THF 'THTRACE)) (THERT THTRACES - TRACE LOSSAG))
            ;; THE TRACE FN IS EXECUTED
            (THZ THL THB)
            ;; IF THB IS NON-NIL, BREAK
            THB
            (THERT))))

;; THE CAR OF THE TREE IS '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; THUS, THESE TWO FNS PRINT THE NAME OF THE TRACE POINT, "FAIL"-OR-"SUCCEED"
;; PRINT THVALUE IF ANY, AND FINALLY BREAK IF (THERT) IS PRESENT, THEN POP THE TREE

(DEFPROP THTRACES
    (LAMBDA NIL
        (PRINT (CADAR THTREE))
        (PRINC 'FAILED\ )
        (EVLIS (CDDAR THTREE))
        (THPOPT)
        NIL)
    THFAIL)

(DEFPROP THTRACES
    (LAMBDA NIL
        (PRINT (CADAR THTREE))
        (PRINC 'SUCCEEDED\ )
        (EVLIS (CDDAR THTREE))
        (THPOPT)
        THVALUE)
    THSUCCEED)

;; THE TRACE FNS THBKPT, THGOAL, THEOREM, THASSERT, AND THERASE PUSH ONTO THE TREE
;; '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; X = THL = INSTANTIATED GOAL, ASSERTION OR ERASURE, NAME OF THE THM, OR MESSAGE OF THE BREAKPOINT

(DEFPROP THBKPT
    (LAMBDA (X B)
        (THPUSH THTREE (LIST 'THTRACES (THGENS B) (AND B '(THERT))))
        (THPRINTC 'PASSING\ BKPT)
        (PRIN1 (CADAR THTREE))
        (PRINC '\ )
        ;; BY SETTING THBRANCH AND THABRANCH, A TRIPLE IS CREATED BY THVAL FOR BACKTRACKING.
        ;; THEN, THE TREE IS POPPED TO PREVENT THTRACES FROM TYPING OUT THE MEANINGLESS
        ;; THAT THE BREAKPOINT SUCCEEDED.
        (SETQ THBRANCH THTREE)
        (SETQ THABRANCH THALIST)
        (THPOPT)
        (PRIN1 X))
    THTRACE)

(DEFPROP THGOAL
    (LAMBDA (X B)
        (THPUSH THTREE (LIST 'THTRACES (THGENS G) '(AND THVALUE (PRIN1 THVALUE)) (AND B '(THERT))))
        (THPRINTC 'TRYING\ GOAL)
        (PRIN1 (CADAR THTREE))
        (PRINC '\ )
        (PRIN1 X))
    THTRACE)

(DEFPROP THEOREM
    (LAMBDA (X B)
        (THPUSH THTREE (LIST 'THTRACES X '(AND THVALUE (PRIN1 THVALUE)) (AND B '(THERT))))
        (THPRINTC 'ENTERING\ THEOREM)
        (PRIN1 X))
    THTRACE)

(DEFPROP THASSERT
    (LAMBDA (X B)
        (THPUSH THTREE (LIST 'THTRACES (THGENS A) (AND B '(THERT))))
        (PRINT 'ASSERTING)
        (PRIN1 (CADAR THTREE))
        (PRINC '\ )
        (PRIN1 X))
    THTRACE)

(DEFPROP THERASE
    (LAMBDA (X B)
        (THPUSH THTREE (LIST 'THTRACES (THGENS E) (AND B '(THERT))))
        (PRINT 'ERASING)
        (PRIN1 (CADAR THTREE))
        (PRINC '\ )
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

(DEFUN THSEL (THF)
    (PROG (THX) (RETURN
        (AND (SETQ THX (ASSQ THL THTRACE)) (SETQ THX (THF THX)) (THVAL THX THALIST)))))

;; MAKES A NAME WITH PREFIX X AND SUFFIX A UNIQUE NUMBER.
(DEFUN THGENS FEXPR (X)
    (MAKNAM (NCONC (EXPLODE (CAR X)) (EXPLODE (SETQ THGENS (ADD1 THGENS))))))

(SETQ THGENS 0)

#_(ns shrdlu.syscom)

;; ##################################################################
;;
;;          SYSCOM - TOPLEVEL AND GENERAL UTILITY FUNCTIONS
;;
;; ##################################################################

(DEFUN SHRDLU NIL
    (PROG (ERT-TIME END AMB TIMAMB BOTH BACKREF BACKREF2 ANSNAME LASTREL WHO PT PTW SENT PUNCT IGNORE H N NB FE SM RE
        MES MESP C CUT CURTIME STATE GLOBAL-MESSAGE LEVEL P-TIME SMN-TIME PLNR-TIME ANS-TIME ANS-PLNR-TIME SH-GCTIME)
        (CLEANOUT TSS EVX NODE ANS OSS RSS X)       ;; FLUSH OLD GENSYMS
    CATCH-LOOP
        (CATCH
            (PROG NIL
        LOOP    (SETQ SENTNO (ADD1 SENTNO)
                      PARSINGS 0
                      LEVEL 0
                      LASTSENTNO (ADD1 LASTSENTNO)
                      LASTSENT C
                      GLOBAL-MESSAGE NIL
                      MES 'NOPE
                      BACKREF NIL                   ;; ???????????????????
                      RUNTIME (RUNTIME)
                      SH-GCTIME (STATUS GCTIME)
                      PLNR-TIME 0
                      ANS-PLNR-TIME 0
                      SMN-TIME 0
                      ERT-TIME 0)
        UP      (SETQ N (SETQ SENT (ETAOIN)))
                (OR ANNOYANCE (PRINT *1))
                (AND ^Q (%))
                (SETQ ^Q NIL)
                (AND IGNORE (GO UP))
                (COND
                    ((AND
                        (COND
                            (TOPLEVEL-ERRSET? (ERRSET (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
                            (T (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
                        C)
                    (OR ANNOYANCE (PRINT *2))
                    (SETQ FE (FE C))
                    (SETQ NB SENT)
                    (SETQ H (H C))
                    (SETQ INTERPRETATION (SM C))
                    (AND SH-BEFOREANSWER-PAUSE (ERT BEFORE ANSWERING))
                    (COND
                        (SMN (AND SH-PARSE-PAUSE (ERT PARSING COMPLETED))
                            (GO LOOP))
                        ((NOT ANSWER?)
                            (AND SH-PARSESMNTC-PAUSE (ERT ANALYSIS COMPLETED)))
                        ((COND
                            (TOPLEVEL-ERRSET? (ERRSET (TIME-ANSWER '(ANSWER C))))
                            (T (TIME-ANSWER '(ANSWER C)))))
                        ((APPLY 'SAY (OR GLOBAL-MESSAGE '(I DON\'T UNDERSTAND\.))))))
                    ((PRINT *3)
                        (APPLY 'SAY (OR GLOBAL-MESSAGE '(I DON\'T UNDERSTAND\.)))))
            (SHRDLU-TIMER)
            (AND SH-STANDARD-PRINTOUT (SHSTPO))
            (AND SH-AFTERANSWER-PAUSE (ERT))
            (GO LOOP))
        ABORT-PARSER)
        (GO CATCH-LOOP)))

(DEFUN TIMER (T0 T1) (QUOTIENT (- T1 T0) 1000000.0))

(DEFUN PARSEVAL (A)
    (PROG (P-TTIME P-GC SM-TIME MP-TIME RETURN-NODE)
        (SETQ P-GC (STATUS GCTIME)
              SM-TIME 0
              MP-TIME 0
              P-TTIME (RUNTIME))
        (SETQ RETURN-NODE (EVAL (CONS 'PARSE A)))
        (SETQ P-TIME (DIFFERENCE (TIMER P-TTIME (RUNTIME)) SM-TIME PLNR-TIME))
        (OR (== P-GC (STATUS GCTIME))
            (SETQ P-TIME
                (DIFFERENCE P-TIME (TIMER P-GC (STATUS GCTIME)))))
        (SETQ SMN-TIME SM-TIME PLNR-TIME MP-TIME)
        (RETURN RETURN-NODE)))

(SETQ PARSEARGS '(CLAUSE MAJOR TOPLEVEL))

;; ######################################################################
;;                         FANCY TIMING PACKAGE
;; ######################################################################

(DEFUN SHRDLU-TIMER NIL
    (PROG NIL
        (OR SH-PRINT-TIME (RETURN T))
        (TERPRI)
        (PRINC 'TOTAL\ TIME\ USED:\ )
        (PRINC (TIMER RUNTIME (RUNTIME)))
        (PRINTC '\ \ AMOUNT\ SPENT\ IN\ GARBAGE\ COLLECTION)
        (PRINC (TIMER SH-GCTIME (STATUS GCTIME)))
        (OR (EQ SH-PRINT-TIME 'FANCY) (RETURN T))
        (TERPRI)
        (PRINC 'BREAKDOWN:)
        (PRINTC '\ \ \ PARSING)
        (PRINC P-TIME)
        (PRINTC '\ \ \ SEMANTICS)
        (PRINC SMN-TIME)
        (PRINTC '\ \ \ MICROPLANNER)
        (PRINTC '\ \ \ \ \ \ FOR\ SEMANTICS)
        (PRINC PLNR-TIME)
        (PRINTC '\ \ \ \ \ \ FOR\ ANSWERING)
        (PRINC ANS-PLNR-TIME)
        (PRINTC '\ \ \ ANSWERING)
        (PRINC ANS-TIME)
        (TERPRI)))

(DEFUN TIME-ANSWER (REAL-CALL)
    (PROG (MP-TIME SM-TIME PLNR-TIME ANS-TTIME GC RESULT)
        (SETQ MP-TIME 0
              SM-TIME 0
              GC (STATUS GCTIME)
              ANS-TTIME (RUNTIME)
              PLNR-TIME 0)
        (SETQ RESULT (EVAL REAL-CALL))
        (SETQ ANS-TIME
            (DIFFERENCE (TIMER ANS-TTIME (RUNTIME)) PLNR-TIME))
        (OR (== GC (STATUS GCTIME))
            (SETQ ANS-TIME
                (DIFFERENCE ANS-TIME (TIMER GC (STATUS GCTIME)))))
        (SETQ ANS-PLNR-TIME MPLNR-TIME
              SMN-TIME (PLUS SMN-TIME SM-TIME))
        (RETURN RESULT)))

(DEFUN PARSE-STATISTICS NIL
    (COND ((== PARSINGS 0)
            (PUTPROP 'PARSINGS 0 'WINS)))
        (AND RE
            (PUTPROP 'PARSINGS (1+ (GET 'PARSINGS 'WINS)) 'WINS))
        (SETQ PARSINGS (1+ PARSINGS)))

;; THESE NEXT TWO ARE LEFT OVER FROM PREVIOUS INCARNATIONS.

;; (DEFUN TIMER NIL
;;        (AND SH-PRINT-TIME
;;         (PRINT 'TIME-USED)
;;         (PRINC (DIFFERENCE (TIME-SINCE RUNTIME) ERT-TIME))))

(DEFUN TIME-SINCE (X) (QUOTIENT (- (RUNTIME) X) 1000000.0))

;; ############################################################
;;         FUNCTIONS THAT EXTRACT INPUT FROM THE USER
;; ############################################################

(DEFPROP DEFLIST
    (LAMBDA (LIST)
        (MAPC #'(LAMBDA (A)
                    (PUTPROP (CAR A) (CADR A) (CAR LIST)))
                (CDR LIST))
        (CAR LIST))
    FEXPR)

;; ################################################################
;;            SPECIALIZED AND NOT SO, OUTPUT ROUTINES
;; ################################################################

(DEFUN % NIL        ;; THIS FUNCTION PRINTS THE CURRENT SENTENCE
    (TERPRI)
    (MAPC 'PRINT3 SENT)
    (PRINC PUNCT))

(DEFUN DA (X)
    (AND
        (GET X 'THASSERTION)
        (DISP (APPLY 'APPEND (MAPCAR 'CDDR (APPLY 'APPEND (MAPCAR 'CDR (CDR (GET X 'THASSERTION)))))))))

(DEFPROP DISP
    (LAMBDA (0A)
        (AND (STATUS TTY) (TYO 12))
        (TERPRI)
        (AND (CDR 0A)
            (PRINC (CAR 0A))
            (PRINC '\ >>\ )
            (PRINC (CADR 0A))
            (TERPRI))
        (SPRINT (COND ((CDR 0A) (GET (CAR 0A) (CADR 0A)))
                ((EVAL (CAR 0A))))
            LINEL
            0)
        *4)
    FEXPR)

(DEFUN DP (X)
    (PROG (PLIST)
        (TERPRI)
        (TERPRI)
        (PRINC '[)
        (PRINC X)
        (PRINC '])
        (SETQ PLIST (PLIST X))
    A   (COND ((MEMQ (CAR PLIST) '(PNAME VALUE)) (GO B)))
        (TERPRI)
        (TAB 4)
        (PRINC (CAR PLIST))
        (SPRINT (CADR PLIST) (*DIF LINEL 18) 18)
    B   (COND ((SETQ PLIST (CDDR PLIST)) (GO A)))
        (TERPRI)
        (AND DPSTOP (ERT))
        (RETURN '*)))

;; ################################################################
;;         FUNCTIONS FOR HAND-TAILORED GARBAGE COLLECTION
;; ################################################################

(DEFUN FORGET NIL
    (SETQ LASTSENT NIL LASTREL NIL BACKREF NIL BACKREF2 NIL LASTTIME NIL LASTPLACE NIL)
    (SETQ LASTSENTNO 0)
    (MAPC #'(LAMBDA (PN)
        (MAPC #'(LAMBDA (PROP) (REMPROP PN PROP))
            '(BIND LASTBIND)))
        '(IT THEY ONE))
    (AND EVENTLIST (PROGN (THFLUSH HISTORY) (STARTHISTORY))))

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

(DEFUN CLEANOUT FEXPR (LIST)        ;; REMOB'S ALL GENSYMS OF THE MEMBERS OF LIST
    (MAPC #'(LAMBDA (A)
            (CLEANX A 0 (GET A 'MAKESYM))
            (PUTPROP A 0 'MAKESYM))
        LIST))

(DEFUN CLEANX (A B C)
    ;; CLEANX REMOB'S GENSYMS OF THE SYMBOL "A" FROM B+1 UP TO AND INCLUDING C
    (PROG (SAVE I)
        (SETQ B (OR B 0))
        (SETQ SAVE (GET A 'MAKESYM))
        (AND C
            (GREATERP C B)
            (PUTPROP A B 'MAKESYM)
            (DO I B (ADD1 I) (EQUAL I C) (REMOB (MAKESYM A))))
        (RETURN (PUTPROP A SAVE 'MAKESYM))))

;; ################################################################
;;         A MOST COMPLETE AND SOPHISTICATED BREAK PACKAGE
;; ################################################################

(DEFPROP THERT ERT FEXPR)

(DEFUN ERT FEXPR (MESSAGE) (ERTEX MESSAGE NIL T))       ;; ALWAYS STOPS, NEVER CAUSES ABORTION.  USED FOR GUARENTEED STOPS AS IN DEBUGGING OR ETAOIN.

(DEFUN ERTERR FEXPR (MESSAGE) (ERTEX MESSAGE T NIL))    ;; USED FOR KNOWN WIERD STATES SUCH AS CHOP.  USES "NOSTOP" SWITCH, CAUSES ABORTION.

(DEFUN BUG FEXPR (MESSAGE)
    (ERTEX (CONS 'BUG!!!!!!!!!! MESSAGE) T NIL))        ;; MARKES UNANTICIPATED WIERD STATES WHICH INDICATE MISTAKES IN THE CODE.

(DEFUN GLOBAL-ERR FEXPR (MESSAGE)
    (ERTEX (SETQ GLOBAL-MESSAGE MESSAGE) T NIL))        ;; MARKES KNOWN INADEQUACIES OF THE SYSTEM.  SWITCHABLE STOP, CAUSES ABORTION.

(DEFUN ERTEX (MESSAGE CAUSE-ABORTION IGNORE-NOSTOP-SWITCH?)
    (PROG (ERT-TIME GLOP EXP ST-BUFFER BUILDING-ST-FORM ^W ^Q FIRSTWORD)
        (AND NOSTOP
            (NOT IGNORE-NOSTOP-SWITCH?)
            (AND CAUSE-ABORTION
                (THROW CAUSE-ABORTION ABORT-PARSER))
            (RETURN T))
        (SETQ ERT-TIME (RUNTIME))
        (TERPRI)
        (MAPC #'PRINT3 MESSAGE)
    PRINT
        (SETQ FIRSTWORD T ST-BUFFER NIL BUILDING-ST-FORM NIL)   ;; "ST" REFERS TO SHOW, TELL
        (PRINT '>>>)
    LISTEN
        (COND                                                   ;; SHELP UP SPURIOUS CHARACTERS
            ((MEMBER (TYIPEEK) '(32 10))                        ;; SP, LF
                (READCH)
                (GO LISTEN))
            ((EQ (TYIPEEK) 13)                                  ;; CR
                (COND (BUILDING-ST-FORM
                        (SETQ EXP (REVERSE ST-BUFFER))
                        (GO EVAL-EXP))                          ;; DELIMITER CASE
                    (T (READCH) (GO LISTEN)))))                 ;; SPURIOUS CHARACTER CASE

        (OR (ERRSET (SETQ GLOP (READ))) (GO PRINT))

        (COND ((ATOM GLOP)
            (SETQ GLOP (OR (GET GLOP 'ABBREV) GLOP))
            (COND ((MEMQ GLOP '(T P NIL))                     ;; LEAVE-LOOP CHARS
                    (SETQ ERT-TIME (PLUS (TIME-SINCE ERT-TIME) ERT-TIME)) ;; ERT-TIME IS BOUND BY SHRDLU
                    (RETURN GLOP))
                ((EQ GLOP 'GO)                                  ;; CAUSE RETURN TO READY-STATE
                    (THROW 'GO ABORT-PARSER))
                (BUILDING-ST-FORM
                    (SETQ ST-BUFFER (CONS GLOP ST-BUFFER))
                    (GO LISTEN))
                ((AND FIRSTWORD (MEMQ GLOP '(SHOW TELL)))
                    (SETQ BUILDING-ST-FORM T
                        ST-BUFFER (CONS GLOP ST-BUFFER)
                        FIRSTWORD NIL)
                    (GO LISTEN))
                (T (SETQ EXP GLOP) (GO EVAL-EXP))))
            (T (COND ((EQ (CAR GLOP) 'RETURN)
                    (RETURN (EVAL (CADR GLOP))))
                (T (SETQ EXP GLOP) (GO EVAL-EXP)))))

    EVAL-EXP
        (COND (ERT-ERRSET? (ERRSET (PRINT (EVAL EXP))))
            (T (PRINT (EVAL EXP))))
        (GO PRINT)))

(DEFUN COMBINATION? FEXPR (WORDS)
    ;; THIS FUNCTION CHECKS TO SEE IF THE WORDS PASSED AS ARGS FORM
    ;; A COMBINATION SUCH AS "STACK-UP" OR "ON-TOP-OF" COMBINATIONS
    ;; ARE IN THE DICTIONARY AS A SINGLE ATOM COMPOSED OF THE WORDS
    ;; IN THE COMBINATION SEPARATED BY DASHES ALL COMBINATIONS HAVE
    ;; THE FEATURE "COMBINATION" AND HAVE A ROOT WHICH IS A LIST OF
    ;; THE WORDS IN THE COMBINATION
    (PROG (COMBINE)
        (MAPC #'(LAMBDA (X)
            (SETQ COMBINE (NCONC COMBINE (CONS '- (EXPLODE (EVAL X))))))
            WORDS)
        (SETQ COMBINE (LIST (INTERN (MAKNAM (CDR COMBINE)))))
        (AND (ISQ COMBINE COMBINATION) (RETURN COMBINE))
        (RETURN NIL)))

(SETQ CONSO '(B C D F G H J K L M N P Q R S T V W X Z))

(DEFPROP FINDB
    (LAMBDA (X Y)
        (COND ((NULL X) NIL) ((EQ Y (CDR X)) X) (T (FINDB (CDR X) Y))))
    EXPR)

(DEFPROP FROM
    (LAMBDA (A B)
        (COND ((OR (NOT A) (EQ A B)) NIL) (T (CONS (WORD A) (FROM (CDR A) B)))))
    EXPR)

(DEFUN MAKESYM (A)
    ;; FUNCTION MAKESYM MAKES UP A GENSYM OF ITS ARG
    (PUTPROP A (ADD1 (OR (GET A 'MAKESYM) 0)) 'MAKESYM)
    (SETQ A (MAKNAM (APPEND (OR (GET A 'EXPLO)
                (PUTPROP A (EXPLODE A) 'EXPLO))
                (EXPLODE (GET A 'MAKESYM)))))
    (COND (MAKEINTERN (INTERN A)) (A)))

(DEFUN LIS2FY (X)
    (COND ((ATOM X) (LIST (LIST X))) ((ATOM (CAR X)) (LIST X)) (X)))

(DEFUN MEET (A MEET)
    ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
    (PROG (SET)
    =>  (COND ((NULL A) (RETURN (REVERSE SET)))
            ((MEMQ (CAR A) MEET)
                (SETQ SET (CONS (CAR A) SET))))
        (SETQ A (CDR A))
        (GO =>)))

(DEFPROP MOD (LAMBDA (A B) (UNION (SETDIF A (CADR B)) (CAR B))) EXPR)

(DEFUN NTH (NUM LIST)
    (COND ((ATOM LIST) (ERT NTH - ILLEGAL LIST))
        ((LESSP NUM 1) (ERT NTH - ILLEGAL NUMBER)))
    (PROG NIL
    =>  (COND ((EQUAL NUM 1) (RETURN (CAR LIST)))
            ((SETQ LIST (CDR LIST))
                (SETQ NUM (SUB1 NUM))
                (GO =>))
            (T (ERT NTH - LIST TOO SHORT)))))

(DEFPROP PR1
    (LAMBDA (A)
        (COND ((ATOM (H A)) (LIST (WORD (NB A)) (FE A)))
            ((PR2 (SM A))
                (LIST (FROM (NB A) (N A)) (FE A) (SM A) (COND ((ATOM (H A)) '\ ) ((MAPLIST #'PR1 (REVERSE (H A)))))))))
    EXPR)

(DEFPROP PR2
    (LAMBDA (A)
        (OR (ATOM A)
            (MAPC #'(LAMBDA (B)
                (AND (GET B 'SM)
                    (OR (MEMQ B ALIST)
                        (SETQ ALIST (CONS (LIST B (GET B 'SM) (GET B 'REFER)) ALIST)))))
                A)))
    EXPR)

(DEFUN PRINT2 (X)
    (COND ((GREATERP CHRCT (FLATSIZE X)) (PRINC '\ ))
        (T (TERPRI)))
    (PRINC X))

(DEFUN PRINT3 (X)
    (PROG2 (OR (GREATERP CHRCT (FLATSIZE X)) (TERPRI))
        (PRINC X)
        (PRINC '\ )))

(DEFUN PRINTEXT (TEXT)
    (COND (TEXT
        (TERPRI)
        (EVAL (CONS 'SAY (LISTIFY TEXT))))))

(DEFPROP PRINTC
    (LAMBDA (L)
        (PROG (TEST)
            (TERPRI)
        =>  (COND ((NULL L) (RETURN NIL)))
            (SETQ TEST (EVAL (CAR L)))
            (COND ((EQ TEST '<TAB>))
                (T (PRINC TEST) (PRINC '\ )))
            (SETQ L (CDR L))
            (GO =>)))
    FEXPR)

(DEFUN QUOTIFY (X) (LIST 'QUOTE X))

(DEFPROP SAY (LAMBDA (A) (MAPC #'PRINT3 A)) FEXPR)

(DEFUN SETDIF (A SETDIF)
    (PROG (SET)
    =>  (COND ((NULL A) (RETURN (REVERSE SET)))
            ((MEMQ (CAR A) SETDIF))
            ((SETQ SET (CONS (CAR A) SET))))
        (SETQ A (CDR A))
        (GO =>)))

(DEFPROP STA
    (LAMBDA (A B)
        (PROG NIL
        =>  (COND ((NULL B) (RETURN T))
                ((NULL A))
                ((EQ (CAR A) (CAR B))
                    (SETQ A (CDR A))
                    (SETQ B (CDR B))
                    (GO =>)))))
    EXPR)

(DEFUN UNION (A B)
    (PROG (SET)
        (SETQ SET (REVERSE A))
    =>  (COND ((NULL B) (RETURN (REVERSE SET)))
            ((MEMQ (CAR B) SET))
            ((SETQ SET (CONS (CAR B) SET))))
        (SETQ B (CDR B))
        (GO =>)))

(DEFPROP WALLP
    (LAMBDA (A)
        (PROG (ALIST LINEL)
            (SETQ LINEL 72)
            (AND (STATUS TTY) (TYO 12))
            (TERPRI)
            (SPRINT (LIST (PR1 A) (REVERSE ALIST)) LINEL 0)))
    EXPR)

(DEFUN DEFS FEXPR (L)
    (PROG (A)
        (AND (NULL (CDR L)) (RETURN L))
        (SETQ A (CAR L))
        (SETQ L (CDR L))
    =>  (PUTPROP A (CADR L) (CAR L))
        (COND ((SETQ L (CDDR L)) (GO =>)))
        (RETURN A)))

(DEFPROP TAB
    (LAMBDA (N)
        (PROG (P)
            (COND ((GREATERP N LINEL) (RETURN '<TAB>)))
        =>  (SETQ P (DIFFERENCE LINEL CHRCT))
            (COND ((NOT (GREATERP N P)) (RETURN '<TAB>)))
            (PRINC '\ )
            (GO =>)))
    EXPR)

#_(ns shrdlu.morpho)

;; ################################################################################
;;
;;                    MORPHO - CODE FOR MORPHOLOGICAL ANALYSIS
;;
;;                INCLUDES ETAOIN, THE INPUT HANDLER FOR THE SYSTEM
;;
;; ################################################################################

(DEFUN ETAOIN NIL
    (PROG (WORD NEWWORD CHAR ALTN ALREADY-BLGING-NEWWRD WRD LAST NEXT Y WORD1 X RD POSS)
    THRU (SETQ SENT (SETQ WORD (SETQ PUNCT (SETQ POSS NIL))))
        (PRINT 'READY)
        (TERPRI)
    CHAR (COND ((EQUAL (TYIPEEK) 24) (READCH) (ERT) (GO THRU)) ;; "CNTRL-X" BREAK LEFT OVER FROM CMU
            ((== (TYIPEEK) 3) (BUG ETAOIN: ABOUT TO READ EOF)))
        ;; THIS LITTLE HACK MAPS ALL LOWERCASE LETTERS INTO UPPERCASE.
        (SETQ CHAR (COND ((GREATERP 123 (SETQ CHAR (TYI)) 96) (- CHAR 32)) ((GREATERP 91 CHAR 64) CHAR) (T CHAR))
            CHAR (ASCII CHAR))
        (COND ((EQ CHAR '\ ) (GO WORD))                     ;; DELIMITER
            ((MEMQ CHAR ALTMODE)
                (SETQ CHAR (ASCII (UPPERCASE-IFY-CHAR (TYI))))
                (COND ((MEMQ CHAR ALTMODE) (ERT) (GO THRU)) ;; ALTMODE-ALTMODE
                    ((EQ CHAR 'C) (TYO 12) (GO DO))         ;; ALTMODE-C
                    ((EQ CHAR 'R) (TERPRI) (GO DO))         ;; ALTMODE-R
                    ((AND (EQ CHAR 'S) SAVESENT)            ;; ALTMODE-S CAUSES THE LAST SENTENCE TYPED IN TO
                        (SETQ SENT (CAR SAVESENT))          ;; RETURNED AS THE SENTENCE TO BE INTERPRETED
                        (SETQ PUNCT (CDR SAVESENT))
                        (%)
                        (RETURN SENT))
                    ((EQ CHAR 'N)                           ;; ALTMODE-N COMPLEMENTS THE NEWWORD FLAG, WHICH
                                                            ;; DETERMINES WHETHER UNRECOGNIZED WORDS WILL BE
                                                            ;; CONSIDERED SPELLING ERRORS OR NEW WORDS.
                        (SETQ NEWWORD (NOT NEWWORD) ALTN (NOT ALTN))
                        (GO CHAR))
                    ((EQ CHAR 'Q)                           ;; ALTMODE-Q CAUSES READIN FROM DISK FILE.
                        (SETQ ^Q T)
                        (SETQ IGNORE NIL)
                        (GO THRU))
                    ((EQ CHAR 'M)
                        (SETQ ^Q T)
                        (SETQ IGNORE NIL)
                        (GO THRU))
                    ((EQ CHAR 'I)                           ;; ALTMODE-I IGNORES SENTENCE READ FROM FILE.
                        (SETQ IGNORE T)
                        (SETQ ^Q T)
                        (GO THRU))
                    ((GO THRU))))
            ((EQ CHAR RUBOUT)
                (COND (WORD (PRINC (CAR WORD)) (SETQ WORD (CDR WORD)))
                    (SENT (PRINT (CAR SENT)) (SETQ SENT (CDR SENT))))
                (GO CHAR))
            ((EQ CHAR CARRET) (GO WORD))
            ((MEMQ CHAR PUNCL)
                (SETQ PUNCT CHAR)                           ;; DELIMITER
                (AND WORD (GO WORD))
                (GO PUNC)))
        (AND
            (OR (AND (EQ CHAR '\") (NOT ALREADY-BLGING-NEWRD) (SETQ NEWWORD (SETQ ALREADY-BLGING-NEWRD T)) (GO CHAR))
                (AND (EQ CHAR '\") ALREADY-BLGING-NEWRD (NOT (SETQ ALREADY-BLGING-NEWRD NIL)) (GO WORD))
                                                            ;; WITHIN THIS "AND" ARE ALL THE CHARACTERS THAT ARE UNDERSTOOD BY THE SYSTEM
                (NUMBERP CHAR)
                (AND (EQ CHAR '=) (NULL WORD))
                (MEMQ CHAR VOWEL)
                (MEMQ CHAR CONSO))
            (SETQ WORD (CONS CHAR WORD)))
        (GO CHAR)

    DO  (PRINT 'READY)
        (TERPRI)
        (MAPC #'(LAMBDA (X) (PRINT2 X)) (REVERSE SENT))
        (PRINC '\ )
        (MAPC #'PRINC (REVERSE WORD))
        (GO CHAR)

    WORD (COND ((NULL WORD) (GO CHAR))
            ((EQUAL WORD '(P L E H)) (HELP) (GO THRU))
            ((AND (SETQ WRD (ERRSET (READLIST (REVERSE WORD)))) (NUMBERP (SETQ WRD (CAR WRD))))
                (SETQ SENT (CONS WRD SENT))
                (BUILDWORD WRD (OR (AND (ZEROP (SUB1 WRD)) '(NUM NS)) '(NUM)) (LIST 'NUM WRD) NIL))
                                                            ;; NO ROOT FOR NUMBERS
            ((NULL WRD) (SETQ WRD (REVERSE WORD)) (GO NO))
            ((GET WRD 'FEATURES))                           ;; IF A WORD HAS FEATURES, IT'S PROPERTIES ARE ALL SET UP IN THE DICTIONARY
            ((SETQ X (GET WRD 'IRREGULAR))
                (BUILDWORD WRD (MOD (GET (CAR X) 'FEATURES) (CDR X)) (SM X) (CAR X)))
            ((EQ (CAR (LAST WORD)) '=)
                (BUILDWORD WRD (COND ((MEMQ '\" WORD) '(PROPN NS POSS)) ('(PROPN NS))) '((PROPN T)) NIL))
            ((GO CUT)))
        (GO WRD)

        ;; --------------------------------------------
        ;;               MORPHOLOGY CODE
        ;; --------------------------------------------

    CUT (COND
            ((STA WORD '(T \" N)) (SETQ RD (CDDDR WORD)) (SETQ WORD (CONS '* WORD)) (GO TRY)) ;; "sic!
            ((STA WORD '(S \")) (SETQ WORD (CDDR WORD)) (SETQ POSS WRD) (GO WORD)) ;; "sic!
            ((STA WORD '(\")) (SETQ WORD (CDR WORD)) (SETQ POSS WRD) (GO WORD)) ;; "sic!
            ((STA WORD '(Y L)) (SETQ RD (CDDR WORD)) (GO LY))
            ((STA WORD '(G N I)) (SETQ RD (CDDDR WORD)))
            ((STA WORD '(D E)) (SETQ RD (CDDR WORD)))
            ((STA WORD '(N E)) (SETQ RD (CDDR WORD)))
            ((STA WORD '(R E)) (SETQ RD (CDDR WORD)))
            ((STA WORD '(T S E)) (SETQ RD (CDDDR WORD)))
            ((STA WORD '(S)) (SETQ RD (CDR WORD)) (GO SIB))
            (T (GO NO)))
        (SETQ LAST (CAR RD))
        (SETQ NEXT (CADR RD))
        (COND ((AND (MEMQ LAST CONSO) (NOT (MEMQ LAST LIQUID)) (EQ LAST NEXT)) (SETQ RD (CDR RD)))
            ((EQ LAST 'I) (SETQ RD (CONS 'Y (CDR RD))))
            ((OR
                (AND (MEMQ LAST CONSO) (MEMQ NEXT VOWEL) (NOT (EQ NEXT 'E)) (MEMQ (CADDR RD) CONSO))
                (AND (MEMQ LAST LIQUID) (MEMQ NEXT CONSO) (NOT (MEMQ NEXT LIQUID)))
                (AND (EQ LAST 'H) (EQ NEXT 'T))
                (AND (MEMQ LAST '(C G S J V Z)) (OR (MEMQ NEXT LIQUID) (AND (MEMQ NEXT VOWEL) (MEMQ (CADDR RD) VOWEL)))))
                    (SETQ RD (CONS 'E RD))))
        (GO TRY)

    LY  (COND ((AND (MEMQ (CAR RD) VOWEL) (NOT (EQ (CAR RD) 'E)) (MEMQ (CADR RD) CONSO))
            (SETQ RD (CONS 'E RD))))
        (COND ((MEMQ 'ADJ (GET (SETQ ROOT (READLIST (REVERSE RD))) 'FEATURES))
            (BUILDWORD WRD '(ADV VBAD) NIL ROOT) ;; TEMP NIL SEMANTICS ;; ROOT IS THE ADJECTIVE
            (GO WRD)))
        (GO NO)

    SIB (SETQ LAST (CAR RD))
        (SETQ NEXT (CADR RD))
        (COND ((NOT (EQ LAST 'E)))
            ((EQ NEXT 'I) (SETQ RD (CONS 'Y (CDDR RD))))
            ((EQ NEXT 'X) (SETQ RD (CDR RD)))
            ((AND (EQ NEXT 'H) (NOT (EQ (CADDR RD) 'T))) (SETQ RD (CDR RD)))
            ((AND (MEMQ NEXT '(S Z)) (EQ NEXT (CADDR RD))) (SETQ RD (CDDR RD))))
    TRY (COND
            ((OR (SETQ FEATURES (GET (SETQ ROOT (READLIST (REVERSE RD))) 'FEATURES))
                    (AND (SETQ X (GET ROOT 'IRREGULAR))
                        (SETQ FEATURES (MOD (GET (SETQ ROOT (CAR X)) 'FEATURES) (CDR X)))))
                (BUILDWORD WRD (MOD FEATURES (GET (CAR WORD) 'MOD)) (GET ROOT 'SEMANTICS) ROOT))
            ((EQ (CAR RD) 'E) (SETQ RD (CDR RD)) (GO TRY))
            ((GO NO)))

        ;; -------------------------------------------------------
        ;;   BUILD UP THE PROCESSED LIST OF WORDS TO BE RETURNED
        ;; -------------------------------------------------------

    WRD (SETQ SENT
            (COND (POSS
                (COND ((OR (MEMQ 'NOUN (SETQ FEATURES (GET WRD 'FEATURES))) (MEMQ 'PROPN FEATURES))
                        ;; IF IT'S A NOUN OR A PROPER NOUN, MARK IT AS POSSESSIVE
                        (BUILDWORD POSS (APPEND (MEET FEATURES (GET 'POSS 'ELIM)) '(POSS)) (GET WRD 'SEMANTICS) ROOT)
                        (CONS POSS SENT))
                    ;; CAN WE GENERALIZE IT???
                    ((BUILDWORD '\"S '(VB BE V3PS PRES) (GET 'BE 'SEMANTICS) 'BE) (CONS '\"S (CONS WRD SENT))))) ;; "sic!
                ((CONS WRD SENT))))

    PUNC (COND (PUNCT
            (COND ((AND (EQ PUNCT '?) (NULL SENT)) (HELP) (GO THRU))
                ((MEMQ PUNCT FINAL)
                    (RETURN (CAR (SETQ SAVESENT (CONS (REVERSE SENT) PUNCT))))) ;; RETURN POINT !!!!!!!!!!!!!
                ((SETQ SENT (CONS PUNCT SENT))))))

        (SETQ PUNCT NIL)
        (SETQ WORD (SETQ POSS NIL))
        (GO CHAR)

    NO  (COND (NEWWORD
            (BUILDWORD WRD '(NOUN NS) '((NOUN (SMNEWNOUN)) (PROPN (SMNEWPROPN))) WRD)
            (OR ALTN (SETQ NEWWORD NIL))
            (GO PUNC)))
        (TERPRI)
        (SAY *SORRY I DON\'T KNOW THE WORD \") ;; "sic!
        (PRINC WRD)
        (PRINC '\ \"\.) ;; "sic!
        (TERPRI)
        (SAY PLEASE TYPE <LF> AND CONTINUE THE SENTENCE\.)

    NOGO (OR (EQUAL (TYI) 10) (GO NOGO))
        (SETQ PUNCT NIL WORD NIL)
        (GO DO)))

(DEFUN PROPNAME (X) (EQ (CAR (EXPLODE X)) '=))

(DEFUN BUILDWORD (WORD FEATURES SEMANTICS ROOT)
    (PUTPROP WORD FEATURES 'FEATURES)
    (PUTPROP WORD (OR SMN SEMANTICS) 'SEMANTICS)
    (AND ROOT (PUTPROP WORD ROOT 'ROOT))
    WORD)

(DEFUN BUILDWORDLIST FEXPR (A)
    (MAPC #'(LAMBDA (X)
        (PRINT (BUILDWORD (CAR X)
            (CADR X)
            (CADDR X)
            (AND (CDDDR X) (CADDDR X)))))
        A))

(SETQ CARRET (ASCII 13))

(SETQ FINAL '(\. ? !))

(SETQ CONSO '(B C D F G H J K L M N P Q R S T V W X Z))

(SETQ LIQUID '(L R S Z V))

(SETQ PUNCL '(\. ? : \; \" !))

(SETQ RUBOUT (ASCII 127))

(DEFPROP UNDEFINED
    (LAMBDA NIL (PROG2 (PRINC (WORD N)) (ERT UNDEFINED)))
    EXPR)

(DEFUN UPPERCASE-IFY-CHAR (CHAR) (COND ((GREATERP 123 CHAR 96) (- CHAR 32)) (T CHAR)))

(SETQ VOWEL '(NIL A E I O U Y))

#_(ns shrdlu.show)

;; QUICKIES

(DEFUN SHSTPO NIL ;; "SH-STANDARD-PRINTOUT"
    (PARSINGS))

(DEFUN PARSINGS NIL
    (PRINTC '\ \ RATIO\ OF\ WINNING\ PARSES\ TO\ TOTAL\ )
    (PRINC (GET 'PARSINGS 'WINS))
    (PRINC '\/)
    (PRINC PARSINGS))

(DEFUN PARSETRACE LABELS (COND ((== (ARG NIL) 0) (SETQ PARSETRACE 'ALL)) (T (SETQ PARSETRACE (LISTIFY LABELS)))))

(DEFUN PARSEBREAK LABELS (COND ((== (ARG NIL) 0) (SETQ PARSEBREAK 'ALL)) (T (SETQ PARSEBREAK (LISTIFY LABELS)))))

(DEFUN FANCYTIMER OFF? (COND ((== (ARG NIL) 1) (SETQ SH-PRINT-TIME NIL)) (T (SETQ SH-PRINT-TIME 'FANCY))))

(DEFUN TOTALTIME OFF? (COND ((== (ARG NIL) 1) (SETQ SH-PRINT-TIME NIL)) (T (SETQ SH-PRINT-TIME T))))

(DEFUN SMNTRACE OFF? (COND ((== (ARG NIL) 1) (SETQ SMNTRACE NIL)) (T (SETQ SMNTRACE T))))

(DEFUN SMNBREAK OFF? (COND ((== (ARG NIL) 1) (SETQ SMNBREAK NIL)) (T (SETQ SMNBREAK T))))

(DEFUN LBK FEXPR (LABELS) (SETQ LABELBREAK LABELS))

(DEFUN LABELTRACE FEXPR (A)
    (MAPC #'(LAMBDA (X)
        (PROG (BODY)
            (PRINT X)
            (COND ((GET X 'LABELTRACED) (PRINC 'ALREADY-) (GO TRACED))
                ((GET X 'INTERPRET) (SETQ BODY (CDR (GET X 'INTERPRET))))
                ((GET X 'EXPR) (SETQ BODY (CDDR (CADDR (GET X 'EXPR)))))
                (T (PRINC 'CAN\'T\ BE-) (GO TRACED)))
            (MAP #'(LAMBDA (Y)
                    (AND (ATOM (CAR Y))
                        (RPLACD Y (CONS (LIST 'PASSING (LIST 'QUOTE (CAR Y))) (CDR Y)))))
                BODY)
            (PUTPROP X T 'LABELTRACED)
        TRACED
            (PRINC 'LABELTRACED)))
        A))

(DEFUN PASSING (A)
    (SETQ LASTLABEL A)
    (AND (COND
            ((ATOM LABELTRACE) (AND LABELTRACE (PRINT 'PASSING) (PRINC A)))
            ((MEMQ A LABELTRACE) (PRINT 'PASSING) (PRINC A)))
        (COND
            ((ATOM LABELBREAK) (AND LABELBREAK (ERT LABELBREAK)))
            ((MEMQ A LABELBREAK) (ERT LABELBREAK)))))

(SETQ LABELTRACE NIL)

(SETQ LABELBREAK NIL)

(DEFUN UNLABELTRACE FEXPR (A)
    (MAPC #'(LAMBDA (X)
        (PROG (BODY)
            (PRINT X)
            (COND
                ((NOT (GET X 'LABELTRACED)) (PRINC 'ISN\'T\ ALREADY-) (GO TRACED))
                ((GET X 'INTERPRET) (SETQ BODY (CDR (GET X 'INTERPRET))))
                ((GET X 'EXPR) (SETQ BODY (CDDR (CADDR (GET X 'EXPR)))))
                (T (PRINC 'CAN\'T\ BE-) (GO TRACED)))
            (MAP #'(LAMBDA (Y) (AND (ATOM (CAR Y)) (RPLACD Y (CDDR Y)))) BODY)
            (PUTPROP X NIL 'LABELTRACED)
            (PRINC 'UN)
        TRACED
            (PRINC 'LABELTRACED)))
        A))

(DEFS TELLABLE
    TELL
    #'(LAMBDA (X) (APPLY 'TELLABLE
        (LIST (CHARG X 'CONCEPT:
            '(ANY PLANNER GOAL PATTERN BEGINNING WITH THIS CONCEPT NAME CAN BE ACCEPTED BY THE SYSTEM AS NEW INFORMATION
            -- BEWARE OF INTERACTIONS WITH SPECIAL HACKS FOR LOCATION\, ETC\.))))))

(DEFUN PEV (EV COL TOP)
    (TERPRI)
    (TAB COL)
    (PRINC EV)
    (PRINC '\ \ )
    (PRINC (GET EV 'TYPE))
    (PRINC '\ \ TIME:\ )
    (PRINC (GET EV 'START))
    (PRINC '\ TO\ )
    (PRINC (GET EV 'END))
    (AND TOP
        (PRINC '\ REASON:\ )
        (PRINC (GET EV 'WHY)))
    (MAPC #'(LAMBDA (X) (AND (EQ EV (GET X 'WHY)) (PEV X (PLUS COL 8) NIL)))
        (REVERSE EVENTLIST)))

(DEFS EVENT
    SHOW
    (LAMBDA (X)
        (SETQ X (CHARG X 'EVENT: '(EVENT TO BE DISPLAYED --<LF> FOR ENTIRE EVENT LIST)))
        (COND (X (PEV X 0 T))
            (T (MAPC #'(LAMBDA (Y) (AND (EQ 'COMMAND (GET Y 'WHY)) (PEV Y 0 T)))
                (REVERSE EVENTLIST))))))

(DEFUN ABBREVIATE FEXPR (A)
    (MAPCAR #'(LAMBDA (X)
        (PUTPROP (READLIST (MAPCAR #'(LAMBDA (X Y) X) (EXPLODE X) '(T T))) X 'ABBREV))
        A)
    'DONE)

(ABBREVIATE
    SHOW TELL LISP PLANNER PARSING DEFINITIONS SCENE INPUT RUN SEMANTICS PROPERTY FUNCTION VALUE ASSERTIONS THEOREM SCENE ACTION NODE TREE LABEL ATTEMPT UNIT WORD MARKER ALL REST CURRENT STOP DO)

(DEFUN SHOWSCENE (X)
    (PROG (PLANNERSEE)
        (TERPRI)
        (TAB 16)
        (PRINC 'CURRENT\ SCENE)
        (TERPRI)
        (TERPRI)
        (MAPC #'(LAMBDA (OBJ)
            (PRINT OBJ)
            (PRINC '-->\ \ )
            (EVLIS (CAR (NAMEOBJ OBJ 'DESCRIBE)))
            (PRINC '\ AT\ )
            (PRINC (CADR (ASSOC OBJ ATABLE)))
            (AND (SETQ OBJ (THVAL '(THFIND ALL $?X (X) (THGOAL (#SUPPORT $?OBJ $?X))) (LIST (LIST 'OBJ OBJ))))
                    (TAB 13)
                    (PRINC 'SUPPORTS\ )
                    (PRINC OBJ)))
                '(:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B10 :BOX))
        (TERPRI)
        (SAY THE HAND IS GRASPING)
        (PRINC '\ )
        (PRINC (COND
            ((SETQ OBJ (THVAL '(THGOAL (#GRASPING $_X)) '((X THUNBOUND)))) (CADAR OBJ))
            (T 'NOTHING)))))

(DEFUN TELLCHOICE (NODE) (SETQ NODE (CAR NODE)) (SHOWTELLCHOICE))

(DEFUN SHOWCHOICE (NODE) (SETQ NODE (CAR NODE)) (SHOWTELLCHOICE))

(DEFUN SHOWTELL (A NODE SYSTEMS INFO ACTION)
    (COND ((NULL A) (SHOWTELLCHOICE))
        ((GET (CAR A) ACTION)
            (APPLY (GET (CAR A) ACTION) (LIST A)))
        ((PRINTEXT '(I DON\'T KNOW HOW TO))
            (PRINT2 ACTION)
            (PRINT2 (CAR A))))
    '*)

(DEFUN SHOWTELLCHOICE NIL
    (APPLY (GET (SETQ NODE (QUERY '(WHICH OPTION?)
                    (PRINT (GET NODE SYSTEMS))
                    (GET NODE INFO)))
        ACTION)
        (LIST (LIST NODE))))

(DEFUN SUBLEAF (KID DAD)
    (CATCH (AND (MAPC 'SUBL2 (GET DAD SYSTEMS)) NIL)))

(DEFUN SUBL2 (X)
    (COND ((EQ X KID) (THROW T))
        (T (MAPC 'SUBL2 (GET X SYSTEMS)))))

(DEFUN QUERY (TEXT CHOICES HELP)
    (PROG (EXPL CH2 EX2 CH3 EX3 CHAR NOTINIT)
        (SETQ EXPL (MAPCAR 'EXPLODE (CONS 'QUIT CHOICES)))
    TOP (SETQ CH2 (CONS 'QUIT CHOICES) EX2 EXPL)
        (PRINTEXT TEXT)
    READ (COND ((MEMBER (SETQ CHAR (READCH)) BREAKCHARS)
            (COND ((NOT NOTINIT) (GO READ))
                ((CDR CH2) (TYO 7) (GO READ))
                (T (MAPC 'PRINC (CAR EX2))
                    (AND (EQ (CAR CH2) 'QUIT)
                    (ERR NIL))
                    (RETURN (CAR CH2)))))
            ((EQ CHAR (ASCII 10)) (GO READ))
            ((EQ CHAR '?) (PRINTEXT HELP) (GO CHOICES)))
        (SETQ CH3 NIL EX3 NIL)
        (MAPC #'(LAMBDA (X Y)
                (AND (EQ CHAR (CAR X))
                    (SETQ CH3 (CONS Y CH3))
                    (SETQ EX3 (CONS (CDR X) EX3))))
            EX2 CH2)
        (AND CH3
            (SETQ EX2 EX3 CH2 CH3)
            (SETQ NOTINIT T)
            (GO READ))
    GO  (OR (MEMBER (READCH) BREAKCHARS) (GO GO))
    CHOICES
        (PRINTEXT '(THE CHOICES ARE:))
        (PRINT CHOICES)
        (GO TOP)))

(DEFUN REQUEST (TEXT HELP)
    (PROG (X)
    TOP (PRINTEXT TEXT)
    READ (COND
            ((MEMBER (ASCII (TYIPEEK)) BREAKCHARS) (READCH) (GO READ))
            ((EQUAL (TYIPEEK) 10) (READCH) (RETURN NIL))
            ((EQ (ASCII (TYIPEEK)) '?) (READCH) (PRINTEXT (OR HELP '(NO INFORMATION AVAILABLE))) (GO TOP))
            ((EQ (SETQ X (READ)) 'QUIT) (ERR NIL))
            (T (RETURN X)))))

(DEFUN SHOWPROP (X)
    (COND ((NULL X)
            (SHOWPROP (CONS
                (REQUEST 'ATOM:
                    '(THE NAME OF THE ATOM WHOSE PROPERTY (IES) YOU WANT TO EXAMINE))
                (LISTIFY (REQUEST 'PROPERTY:
                    '(THE PROPERTY (IES) YOU WANT TO SEE\.  A LINE FEED MEANS ALL PROPERTIES OF THE ATOM))))))
        ((CDR X) (APPLY 'DISP X))
        (T (PROG (DPSTOP) (DP (CAR X))))))

(DEFUN TELL FEXPR (A)
    (SHOWTELL A 'CANTELL 'TELLTREE 'TELLINFO 'TELL))

(DEFUN TREEPRINT (ROOT TR COL)
    (TERPRI)
    (TAB COL)
    (PRINC ROOT)
    (MAPC #'(LAMBDA (X) (TREEPRINT X TR (PLUS COL 8)))
        (GET ROOT TR))
    '*)

(DEFUN CHARG (X TEXT HELP)
    (COND ((CDR X) (CADR X)) (T (REQUEST TEXT HELP))))

(DEFUN SHOW FEXPR (A)
    (SHOWTELL A 'CANSHOW 'SHOWTREE 'SHOWINFO 'SHOW))

(DEFS CANSHOW
    SHOWTREE
    (SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT)
    SHOWINFO
    (THINGS WHICH CAN BE DISPLAYED)
    SHOW
    SHOWCHOICE)

(DEFS CANTELL
    TELLTREE
    (LISP PLANNER PARSING DEFINITIONS SEMANTICS)
    TELLINFO
    (THINGS WHICH CAN BE SET TO CONTROL HOW THE SYSTEM RUNS)
    TELL
    TELLCHOICE)

(DEFS SHOW
    SHOW
    (LAMBDA (X) (TREEPRINT 'CANSHOW 'SHOWTREE 0)))

(DEFS TELL
    SHOW
    (LAMBDA (X) (TREEPRINT 'CANTELL 'TELLTREE 0)))

(DEFS LISP
    SHOWTREE
    (PROPERTY FUNCTION VALUE)
    TELLTELLCHOICE
    TELLTREE
    (FUNCTION)
    SHOW
    SHOWCHOICE)

(DEFS DO
    TELL
    #'(LAMBDA (X) (PRINTEXT '(NOT YET DEFINED))))

(DEFS STOP
    TELL
    (LAMBDA (X)
        (SETQ DPSTOP (ONOFF X '(STOP AFTER DISPLAYING EACH NODE AND SEMANTIC STRUCTURE?)))
        (SETQ PLANNERSEE (AND PLANNERSEE
            (COND ((ONOFF X '(STOP AFTER SHOWING PLANNER INPUT?)) T) ('NOSTOP))))))

(DEFS PLANNER
    SHOWTREE
    (ASSERTIONS THEOREM SCENE EVENT)
    SHOW
    SHOWCHOICE
    TELLTREE
    (INPUT ACTION THEOREM ASSERTIONS TELLABLE)
    TELL
    (LAMBDA (X)
        (COND ((NULL (CDR X)) (TELLCHOICE X))
            ((EQ (CADR X) 'ON)
                (PROGN (SETQ ^W T) (CLEAR-OUTPUT T))
                (THTRACE THEOREM THASSERT THERASE (THGOAL T T))
                (SETQ PLANNERSEE T))
            ((EQ (CADR X) 'OFF)
                (PROGN (SETQ ^W T) (CLEAR-OUTPUT T))
                (SETQ PLANNERSEE NIL)
                (THUNTRACE))
            (T (TELLCHOICE X)))
        (SETQ ^W NIL)))

(DEFS PARSING
    SHOWTREE
    (NODE TREE)
    SHOW
    SHOWCHOICE
    TELLTREE
    (NODE LABEL ATTEMPT)
    TELL
    (LAMBDA (X)
        (COND ((NULL (CDR X)) (TELLCHOICE X))
            ((EQ (CADR X) 'ON)
                (PROGN (SETQ ^W T) (CLEAR-OUTPUT T))
                (SETQ PARSENODE-SEE T LABELTRACE T)
                (TRACE CALLSM PARSE))
            ((EQ (CADR X) 'OFF)
                (PROGN (SETQ ^W T) (CLEAR-OUTPUT T))
                (SETQ PARSENODE-SEE NIL LABELTRACE NIL)
                (UNTRACE CALLSM PARSE))
            (T (TELLCHOICE X)))
        (SETQ ^W NIL)))

(DEFS DEFINITIONS
    SHOWTREE
    (UNIT WORD MARKER)
    SHOW
    SHOWCHOICE
    TELL
    TELLCHOICE
    TELLTREE
    (WORD MARKER))

(DEFS INPUT
    TELL
    (LAMBDA (X) (SETQ PLANNERSEE (ONOFF X '(TO SEE INPUT TO PLANNER))))
    SHOW
    SHOWCHOICE
    SHOWTREE
    (ALL REST CURRENT))

(DEFS SEMANTICS
    TELL
    (LAMBDA (X)
        (SETQ SMN NIL BUILD-SEE T SMN-STOP T)
        (COND
            ((EQ (QUERY '(DO SEMANTIC ANALYSIS?) '(YES NO) NIL) 'NO)
                (SETQ SMN T))
            ((EQ (QUERY '(SHOW BUILDING OF SEMANTIC STRUCTURES?) '(YES NO) NIL) 'NO)
                (SETQ BUILD-SEE NIL))
            ((EQ (QUERY '(STOP AFTER DISPLAYING SEMANTIC STRUCTURES?) '(YES NO) NIL) 'NO)
                (SETQ SMN-STOP NIL)))))

(DEFS RUN
    TELLTREE
    (STOP DO)
    TELL
    TELLCHOICE
    TELLINFO
    '(PARAMETERS TO CONTROL WHAT SHRDLU DOES AS IT RUNS))

(DEFS PROPERTY
    SHOW
    (LAMBDA (X) (SHOWPROP (CDR X))))

(DEFS VALUE
    SHOW
    (LAMBDA (X) (DISP (EVAL (CHARG X 'EXPRESSION: '(EXPRESSION TO BE EVALUATED BY THE LISP INTERPRETER))))))

(DEFS FUNCTION
    TELL
    (LAMBDA (X) (SETQ X (LIST (CHARG X
                    'FUNCTION:
                    '(LISP FUNCTION WHOSE ACTION IS TO BE TRACED))
            (COND ((AND (CDR X) (CDDR X) (MEMQ (CADDR X) '(TRACE BREAK UNTRACE UNBREAK))) (CADDR X))
                (T (QUERY '(TRACE BREAK UNTRACE OR UNBREAK?)
                    '(TRACE BREAK UNTRACE UNBREAK)
                    '(TRACE CAUSES PRINTOUT ON ENTRY AND EXIT OF FUNCTION\.
                        BREAK CAUSES LISP TO STOP ON ENTRY AND EXIT\,
                        ACCEPTING USER COMMANDS AND CONTINUING WHEN <CONTROL X> IS TYPED\.))))))
        (APPLY (SUBST 'WBREAK 'BREAK (CADR X)) (LIST (CAR X))))
    SHOW
    (LAMBDA (X) (APPLY 'GB
            (LIST (CHARG X
                    'FUNCTION:
                    '(LISP FUNCTION WHOSE LISP DEFINITION IS TO BE SHOWN))))))

(DEFS ASSERTIONS
    TELL
    (LAMBDA (X) (THVAL (LIST 'THASSERT
                (CHARG X
                    'ASSERTION:
                    '(PLANNER ASSERTION TO BE ADDED TO DATA BASE))
                '(THTBF THTRUE))
            NIL))
    SHOW
    (LAMBDA (X) (DA (CHARG X
                'ATOM:
                '(SHOW ALL ASSERTIONS WHICH CONTAIN THE GIVEN ATOM)))))

(DEFS THEOREM
    TELL
    DEFINETHEOREM
    SHOW
    (LAMBDA (X) (DISP (GET (CHARG X 'THEOREM-NAME: '(PLANNER THEOREM WHOSE DEFINITION IS TO BE SHOWN)) 'THEOREM))))

(DEFS NODE
    TELL
    (LAMBDA (X) (SETQ PARSENODE-SEE T NODE-STOP T)
        (COND
            ((EQ (QUERY '(SEE SUCCESSFUL PARSE NODES BEING BUILT?) '(YES NO) NIL) 'NO)
                (SETQ PARSENODE-SEE NIL))
            ((EQ (QUERY '(STOP AFTER DISPLAY OF NODES?) '(YES NO) NIL) 'NO)
                (SETQ NODE-STOP NIL))))
    SHOW
    (LAMBDA (X)
        (COND ((GET (CADR X) 'FEATURES) (DP (CADR X)))
            ((SHOWMOVE (CDR X)) (PROG (DPSTOP) (DP (CAR PT))) (RESTOREPT))
            (T (SAY NO SUCH NODE)))))

(DEFS TREE
    SHOW
    (LAMBDA (X)
        (COND ((GET (CADR X) 'FEATURES) (WALLP (LIST (CADR X))))
            ((SHOWMOVE (CDR X)) (WALLP PT) (RESTOREPT))
            (T (SAY NO SUCH NODE)))))

(DEFS UNIT
    SHOW
    (LAMBDA (X)
        (APPLY 'DG (OR (CDR X) (LIST (REQUEST 'UNIT: '(GRAMMAR UNIT WHOSE PROGRAM IS TO BE EXAMINED -- E\.G\.  CLAUSE NG PREPG VG ADJG)))))))

(DEFS WORD
    SHOW
    (LAMBDA (X) (DP (CHARG X 'WORD: '(ENGLISH WORD IN THE VOCABULARY))))
    TELL
    (LAMBDA (X)
        (APPLY 'DEFINE (LIST (CHARG X 'WORD: '(ENGLISH WORD TO BE DEFINED -- MUST BE NOUN OR VERB))))))

(DEFS ACTION
    TELL
    (LAMBDA (X)
        (COND ((CDR X)
                (COND ((EQ (CADR X) 'ON) (SETQ X NIL))
                    ((EQ X 'OFF) (SETQ X '(THUNTRACE)))))
            ((ONOFF X '(WATCH PLANNER PROGRAMS STEP BY STEP?))
                (SETQ X NIL))
            (T (SETQ X '(THUNTRACE))))
        (COND (X (THUNTRACE))
            (T (APPLY 'THTRACE '(THEOREM THGOAL THASSERT THERASE))))))

(DEFS LABEL
    TELL
    (LAMBDA (X)
        (OR (CDR X)
            (SETQ X (LIST (REQUEST '(TYPE LIST OF LABELS\, OR ON OR OFF:) '(WATCHES PARSER GO PAST PROGRAM LABELS IN THE GRAMMAR)))))
        (SETQ LABELTRACE (COND ((EQ (CAR X) 'OFF) NIL) (T (CAR X))))))

(DEFS ATTEMPT
    TELL
    (LAMBDA (X) (COND
        ((ONOFF X '(TO SEE ALL ATTEMPTS TO PARSE SYNTACTIC UNITS\, INCLUDING FAILURES))
            (TRACE PARSE)
            (TRACE CALLSM))
        (T (UNTRACE PARSE)))))

(DEFUN SHOWMOVE (X)
    (SETQ SAVEPT PT)
    (APPLY 'MOVE-PT
        (LISTIFY (OR X
            (REQUEST 'NODE-SPECIFICATION:
                '(C MEANS CURRENT NODE -- H IS MOST RECENTLY PARSED FOR OTHER POSSIBILITIES\, SEE THESIS SECTION ON POINTER-MOVING COMMANDS))))))

(DEFUN ONOFF (ARG HELP)
    (COND
        ((EQ (CADR ARG) 'ON) T)
        ((EQ (CADR ARG) 'OFF) NIL)
        ((EQ 'ON (QUERY '(ON OR OFF?) '(ON OFF) HELP)))))

(DEFUN DEFINETHEOREM (X)
    (PUTPROP (COND ((CDR X) (SETQ X (CADR X))) (T (SETQ X (MAKESYM 'THEOREM))))
        (NCONC (LIST (QUERY '(WHICH THEOREM TYPE?)
                        '(THANTE THERASING THCONSE)
                        '(ANTECEDENT\, ERASING\, OR CONSEQUENT THEOREM))
                    (LISTIFY (REQUEST 'VARIABLE-LIST:
                            NIL))
                    (REQUEST 'PATTERN:
                        '(A LIST ENCLOSED IN PARENS\, LIKE (#IS $?X #ZOG)))
                    (REQUEST 'BODY:
                        '(LIST OF MICROPLANNER STAEMENTS))))
        'THEOREM)
    (THADD X NIL)
    (PRINT X))

(DEFS MARKER
    TELL
    (LAMBDA (X)
        (PROG (Y)
            (PUTPROP (SETQ X (CHARG X 'MARKER: '(MARKER TO BE ADDED)))
                (LIST (SETQ Y (REQUEST 'PARENT: '(NODE TO WHICH IT IS ATTACHED IN THE TREE))))
                'SYS)
            (PUTPROP Y
                (CONS X (GET Y 'SYSTEM))
                'SYSTEM)))
    SHOW
    (LAMBDA (X)
        (TREEPRINT (OR (CHARG X 'MARKER: '(SEMANTIC MARKER WHOSE SUBSETS ARE TO BE EXAMINED\. TYPE <LF> FOR ENTIRE TREE\.)) '#SYSTEMS)
            'SYSTEM
            0)))

(DEFS ALL SHOW (LAMBDA (X) (%)))

(DEFS CURRENT SHOW (LAMBDA (X) (PRINTEXT (FROM NB N))))

(DEFS REST SHOW (LAMBDA (X) (PRINTEXT N)))

(DEFS SCENE SHOW SHOWSCENE)

(DEFUN DEFINE FEXPR (A)
    (PROG (FE TYPE MARK REST TR)
        (SETQ A (COND (A (CAR A)) (T (REQUEST 'WORD: '(ENGLISH WORD TO BE DEFINED)))))
        (SETQ TYPE (QUERY '(NOUN OR VERB?) '(NOUN VERB) '(OTHER TYPES MUST BE DEFINED IN LISP)))
    MAR (OR (SETQ MARK (REQUEST 'MARKERS: '(LIST OF SEMANTIC MARKERS FOR WORD BEING DEFINED - TO SEE MARKER TREE TYPE <LF>)))
            (AND (SHOW MARKER #SYSTEMS) (GO MAR)))
        (SETQ MARK (LISTIFY MARK))
        (COND
            ((EQ TYPE 'NOUN)
                (PUTPROP A '(NOUN NS) 'FEATURES)
                (PUTPROP A
                    (LIST (LIST 'NOUN (LIST 'OBJECT (LIST 'MARKERS: MARK
                        'PROCEDURE:
                        (LIS2FY (REQUEST 'PROCEDURE:
                                    '(EXPRESSION OR LIST OF EXPRESSIONS TO BE PUT IN PLANNER GOALS TO DESCRIBE OBJECT - USE *** TO REPRESENT OBJECT BEING DESCRIBED BY WORD
                                        -- E\.G\.  (#IS *** #ZOG) OR ((#IS *** #ZOG) (#LOVE :EVERYONE ***)))))))))
                    'SEMANTICS)
                (RETURN T))
            ((SETQ TR (EQ (QUERY '(TRANSITIVE OR INTRANSITIVE?) '(TRANSITIVE INTRANSITIVE) NIL) 'TRANSITIVE))
                (PUTPROP A '(VB TRANS INF) 'FEATURES))
            (T (PUTPROP A '(VB ITRNS INF) 'FEATURES)))
        (SETQ REST (LIST (LIST (LISTIFY (REQUEST '(RESTRICTIONS ON SUBJECT:) '(LIST OF SEMANTIC MARKERS))))))
        (AND TR
            (SETQ REST (NCONC REST (LIST (LISTIFY (REQUEST '(RESTRICTIONS ON OBJECT:) '(LIST OF SEMANTIC MARKERS)))))))
        (PUTPROP A
            (LIST (LIST 'VB (LIST 'RELATION (LIST
                'MARKERS: MARK
                'RESTRICTIONS: REST
                'PROCEDURE:
                    (LIS2FY (REQUEST 'PROCEDURE:
                                '(LIST OF EXPRESSIONS TO BE PUT INTO PLANNER GOALS TO DESCRIBE ACTION OR RELATION -- USE #1 FOR SUBJECT\, #2 FOR OBJECT\.
                                    E\.G\.  (#SUPPORT #1 #2) OR ((#HAPPY #1) (#SMILING #1)))))))))
            'SEMANTICS)
        (RETURN T))))

(DEFUN HELP NIL
    (COND ((EQ 'S (QUERY '(TYPE L FOR LONG FORM (85 LINES) S FOR SHORT (16 LINES)) '(S L) NIL))
            (UREAD MINIH DOC DSK LANG))
        (T (UREAD HELP DOC DSK LANG)))
    (THRUTEXT)
    '*)

(DEFUN LIS2FY (X)
    (COND ((ATOM X) (LIST (LIST X))) ((ATOM (CAR X)) (LIST X)) (X)))

#_(ns shrdlu.setup)

;; ################################################################
;;
;;            SETUP - INITIALIZATION FILE FOR SHRDLU
;;
;; ################################################################

(SETQ PARSINGS 0) ;; ATOM USED IN THE TIMING PACKAGE

(SETQ SAVESENT NIL
      DOT (ASCII 46)
      *1 '[1]
      *2 '[2]
      *3 '[3]
      *4 '[4]
      *5 '[5]
      *6 '[6]
      LASTSENTNO 0
      SENTNO 1
      UNMKD '(COMPONENT BOTH)
      LASTIME NIL)

(SETQ DPSTOP NIL
      NODE-STOP NIL
      SMN-STOP NIL
      ERT-TIME 0
      ALTMODE (LIST (ASCII 27))
      BREAKCHARS (LIST (ASCII 32) (ASCII 13) (ASCII 46))
      LINEL 65)

(OR (GET 'CLAUSE 'SUBR)
    (LABELTRACE CLAUSE NG VG ADJG PREPG CONJOIN))

;; ######################################################################
;;
;;                SWITCHES AND SWITCH-SETTING PACKAGES
;;
;; ######################################################################

(SETQ FEATURESWITCHES '(DISCOURSE NOMEM IASSUME TIMID))

(SETQ PAUSESWITCHES '(ANS-AFTEREVALUATION-PAUSE ANS-AFTERFORMULATION-PAUSE EVALANS-PAUSE SH-SENT-PAUSE SH-BEFOREANSWER-PAUSE SH-FINISHED-PAUSE PNS-BK PLNRSEE-PAUSE))

(SETQ CONTROLSWITCHES '(NOSTOP ANSWER? SMN TOPLEVEL-ERRSET? ERT-ERRSET? MAKEINTERN))

(SETQ DISPLAYSWITCHES '(PARSETRACE PARSEBREAK PARSENODE-SEE LABELTRACE MAKE-VERBOSE LABELBREAK BUILDSEE BUILD-SEE PLANNERSEE SH-PRINT-TIME))

;; ************************

(SETQ MAKE-VERBOSE NIL
      PARSETRACE NIL
      PARSEBREAK NIL
      PARSENODE-SEE NIL
      LABELTRACE NIL
      LABELBREAK NIL
      BUILDSEE NIL
      BUILD-SEE NIL
      PLANNERSEE NIL
      SH-PRINT-TIME NIL)

(SETQ DISCOURSE T
      WANT-DISPLAY NIL
      NOMEM NIL
      IASSUME T
      TIMID 200)

(SETQ MAKEINTERN NIL)

(SETQ SH-BEFOREANSWER-PAUSE NIL
      ANS-AFTEREVALUATION-PAUSE NIL
      ANS-AFTERFORMULATION-PAUSE NIL
      EVALANS-PAUSE NIL
      NOSTOP NIL
      ANSWER? T
      SMN NIL
      DOIT NIL
      TOPLEVEL-ERRSET? NIL
      ERT-ERRSET? T
      SH-PARSE-PAUSE NIL
      SH-PARSESMNTC-PAUSE NIL
      SH-AFTERANSWER-PAUSE NIL
      PNS-BK NIL
      PLNRSEE-PAUSE NIL)

;; **********************************

(DEFUN QUIETMODE NIL
    (MAPC #'(LAMBDA (X) (SET X NIL)) DISPLAYSWITCHES))

(DEFUN NOPAUSES NIL
    (MAPC #'(LAMBDA (X) (SET X NIL)) PAUSESWITCHES))

(DEFUN NORMALFEATUREMODE NIL
    (SETQ DISCOURSE T NOMEM NIL IASUME T TIMID 200))

(DEFUN USERMODE NIL
    (QUIETMODE)
    (NORMALFEATUREMODE)
    (NOPAUSES)
    (SETQ NOSTOP T ANSWER? T SMN NIL TOPLEVEL-ERRSET? T ERT-ERRSET T)
    (SETQ ^D NIL)
    (SETQ SH-PRINT-TIME T))

(DEFUN DEBUGMODE NIL
    (QUIETMODE)
    (NORMALFEATUREMODE)
    (NOPAUSES)
    (SETQ NOSTOP NIL ANSWER? T SMN NIL TOPLEVEL-ERRSET? NIL ERT-ERRSET T)
    (SETQ ^D T))

;; #################################################################
;;
;;                     INITIALIZATION ROUTINES
;;
;; #################################################################

(DEFUN INITIALSTUFF (VERSION DATE NOTE)
    (SUSPEND)
    (CURSORPOS 'C)
    (TERPRI)
    (PRINC 'SHRDLU\ VERSION\ )
    (PRINC VERSION)
    (PRINC '\ \ \ )
    (PRINC 'LOADED\ )
    (PRINC DATE )
    (PRINC '\ )
    (PRINC 'IN\ BLISP\ )
    (PRINC (STATUS LISPVERSION))
    (TERPRI)
    (SAY REFER COMMENTS AND QUESTIONS TO DDM)
    (TERPRI)
    (TERPRI)
    (AND NOTE (PROGN (TERPRI) (APPLY 'SAY NOTE) (TERPRI) (TERPRI)))
    (SAY YOU ARE NOW IN A READ-EVAL-PRINT LOOP)
    (TERPRI)
    (SAY TYPE \"GO\ \" TO ENTER READY STATE) ;; "sic!
    (CATCH (ERT) ABORT-PARSER)
    (SSTATUS TOPLEVEL '(SHRDLU))
    (SHRDLU))

(DEBUGMODE)

(SETQ SH-STANDARD-PRINTOUT T SMNBREAK NIL SMNTRACE NIL MAKINTERN T ANNOYANCE NIL)

(SSTATUS PAGEPAUSE T)

(SETQ ^D T)
(SETQ ERRLIST NIL)

#_(ns shrdlu.progmr)

;; ###########################################################
;;
;;                         PROGMR
;;  (INTERPRETER FOR THE PROGRAMMAR GRAMMAR WRITING LANGUAGE)
;;
;; ###########################################################

(DEFUN RESTOREPT NIL (SETQ PT SAVEPT))

(DEFUN SETMVB (PTR-MVB)
    (PROG (SAVE)
        (SETQ MVB PTR-MVB)                              ;; IF THERE ARE SEVERAL CLAUSES ACTIVE AT THE
        (SETQ SAVE PT)
        (SETQ PT PTR-MVB)                               ;; SAME TIME, IT SETS THE NEAREST ONE.
        (SETR 'MVB PTR-MVB (MOVE-PT C U (CLAUSE)))
        (SETQ PT SAVE)
        (RETURN T)))

(DEFUN ADD-F-PT (FEATURE PTR)
    (PUTPROP (CAR PTR) (CONS FEATURE (FE PTR)) 'FEATURES)
    (AND (EQ PTR C) (SETQ FE (FE PTR)))
    T)

(DEFUN REMOVE-F-PT (FEATURE PTR)
    (PUTPROP (CAR PTR) (SETDIF (GET (CAR PTR) 'FEATURES) (LIST FEATURE)) 'FEATURES)
    (AND (EQ PTR C) (SETQ FE (FE PTR)))
    T)

(DEFUN ONE-WORD-LEFT NIL (AND (CDR NB) (NOT (CDDR NB))))

(SETQ SMNBREAKS NIL) ;; A LIST OF SMNFNS WHICH WILL BE BROKEN AT (BEFORE CALLING)

(DEFUN CALLSM FEXPR (SEMANTIC-EXPRESSION)
    (PROG (RESULT MPLNR-TIME SM-TTIME GC SMNFN)
        (SETQ SMNFN (CAR SEMANTIC-EXPRESSION))
        (AND SMNTRACE
            (APPLY 'SAY (LIST 'SEMANTICS '***** UNIT 'CALLING SMNFN)))
        (AND SMN
            (COND ((OR (EQ SMNBREAKS T) (MEMQ SMNFN SMNBREAKS)) (RETURN (ERT)))
                (T (RETURN T))))
        (AND SMNTRACE
            (PROGN (PRINTC '\ \ CALLSM:\ )
                (PRINC (CAR SEMANTIC-EXPRESSION))))
        (SETQ MPLNR-TIME 0)
        (SETQ GC (STATUS GCTIME)
              SM-TTIME (RUNTIME)
              RESULT (EVAL (CAR SEMANTIC-EXPRESSION)))
        (SETQ SM-TIME (PLUS SM-TIME (DIFFERENCE (TIMER SM-TTIME (RUNTIME)) MPLNR-TIME)))
        (OR (== GC (STATUS GCTIME))
            (SETQ SM-TIME (DIFFERENCE SM-TIME (TIMER GC (STATUS GCTIME)))
                  P-GC (STATUS GCTIME)))
        (SETQ MP-TIME (PLUS MP-TIME MPLNR-TIME))
        (AND SMNTRACE
            (PROGN (PRINTC 'CALLSM\ RETURNING:\ )
                (PRINC RESULT)))
        (COND ((OR (EQ SMNBREAKS 'ALL) (MEMQ SMNFN SMNBREAKS)) (ERT)))
        (RETURN RESULT)))

(DEFUN MOVE-PT FEXPR (L)
    (PROG (XX YY L2 EXEC SAVE)
        (SETQ EXEC L)
        (SETQ SAVE PT)
    TEST1 (COND ((AND (CDR EXEC) (NOT (ATOM (CADR EXEC))))
            (GO TEST)))
    LOOK1 (SETQ XX (CAR EXEC))
    LOOK (COND
            ((EQ XX 'H) (OR (SETQ PT H) (GO FAIL)) (GO EX))
            ((EQ XX 'C) (SETQ PT C) (GO EX))
            ((EQ XX 'PC) (SETQ PT (H (PARENT C))) (GO EX))
            ((EQ XX 'LASTSENT) (SETQ PT LASTSENT) (GO EX))
            ((EQ XX 'U) (OR (SETQ PT (PARENT PT)) (GO FAIL)))
            ((EQ XX 'DLC) (OR (SETQ PT (H PT)) (GO FAIL)))
            ((EQ XX 'DF) (SETQ L2 (CONS 'DLC (CONS 'FR L2))) (SETQ XX 'DLC) (GO LOOK))
            ((EQ XX 'FR) (COND ((MOVE-PT PV) (GO LOOK))))
            ((EQ XX 'NX) (OR (SETQ PT (PREVIOUS (H (PARENT PT)) (CAR PT))) (GO FAIL)))
            ((EQ XX 'PV) (SETQ PT (OR (AND (EQ PT C) (H (PARENT C))) (FOLLOWING (H (PARENT PT)) (CAR PT)) (GO FAIL))))
            (T (PRINT XX) (ERT MOVE-PT ILLEGAL INSTRUCTION)))
    EX  (COND ((OR (NULL L2) (NULL (SETQ L2 (CDR L2))))
            (GO TEST)))
        (SETQ XX (CAR L2))
        (GO LOOK)
    TEST (COND ((NULL (CDR EXEC)) (RETURN PT))
            ((ATOM (CADR EXEC)) T)
            ((COND ((CDADR EXEC) (EVAL (CADR EXEC)))
                (T (ISX PT (CAADR EXEC))))
            (SETQ EXEC (CDR EXEC)))
            (T (GO LOOK1)))
        (COND ((SETQ EXEC (CDR EXEC)) (GO TEST1)))
        (RETURN PT)
    FAIL (SETQ PT SAVE)
        (RETURN NIL)))

(DEFUN MOVE-PTW FEXPR (L)
    (PROG (EXEC SAVE XX)
        (SETQ SAVE PTW)
        (SETQ EXEC L)
    TEST1 (COND ((AND (CDR EXEC) (NOT (ATOM (CADR EXEC))))
            (GO EX)))
    LOOK1 (SETQ XX (CAR EXEC))
    LOOK (COND ((EQ XX 'N) (SETQ PTW N))
            ((EQ XX 'LASTSENT) (SETQ PTW (NB LASTSENT)))
            ((EQ XX 'FW) (SETQ PTW (NB PT)))
            ((EQ XX 'AW) (COND ((EQ PT C) (GO FAIL)) ((SETQ PTW (N PT)) (SETQ XX 'PW) (GO LOOK))))
            ((EQ XX 'LW) (COND ((EQ PT C) (GO FAIL)) ((SETQ PTW (N PT)) (SETQ XX 'PW) (GO LOOK))))
            ((EQ XX 'NW) (COND ((SETQ PTW (CDR PTW))) ((SETQ PTW (FINDB SENT NIL)) (GO FAIL))))
            ((EQ XX 'PW) (COND ((SETQ PTW (FINDB SENT PTW))) ((SETQ PTW SENT) (GO FAIL))))
            ((EQ XX 'SFW) (SETQ PTW SENT))
            ((EQ XX 'SLW) (SETQ PTW (FINDB SENT NIL)))
            ((BUG MOVE-PTW ILLEGAL INSTRUCTION)))
    EX  (COND ((NULL (CDR EXEC)) (RETURN PTW))
            ((ATOM (CADR EXEC)) T)
            ((COND ((CDADR EXEC) (EVAL (CADR EXEC)))
                (T (ISX PTW (CAADR EXEC))))
            (SETQ EXEC (CDR EXEC)))
            (T (GO LOOK1)))
        (COND ((SETQ EXEC (CDR EXEC)) (GO TEST1)))
        (RETURN PTW)
    FAIL (SETQ PTW SAVE)
        (RETURN NIL)))

(DEFUN APPLY-GRAMMAR (UNIT)
    (COND ((GET UNIT 'INTERPRET) (INTERPRET UNIT))
        (T (EVAL (LIST UNIT)))))

(DEFUN BUILDNODE (FEATURES FIRSTWORD WORDAFTER DAUGHTERS SEMANTICS)
    (PROG (NODE)
        (SETQ NODE (LIST (MAKESYM 'NODE)))
        (SETR 'FEATURES FEATURES NODE)
        (SETR 'FIRSTWORD FIRSTWORD NODE)
        (SETR 'WORDAFTER WORDAFTER NODE)
        (SETR 'DAUGHTERS DAUGHTERS NODE)
        (SETR 'SEMANTICS SEMANTICS NODE)
        (RETURN NODE)))

(DEFUN CQ FEXPR (FEATURE) (MEMQ (CAR FEATURE) FE))

(DEFPROP CUT
    (LAMBDA (A)
        (PROG (B C)
            (SETQ B N)
        =>  (COND
                ((EQ A B) (SETQ CUT A) (SETQ NN (NOT (EQ CUT N))) (RETURN T))
                ((EQ B END) (RETURN NIL))
                ((SETQ B (CDR B)) (GO =>))
                ((NULL A) (SETQ CUT NIL) (SETQ NN N) (RETURN T)))))
    EXPR)

(DEFUN CUT-BACK-ONE NIL (MOVE-PTW N PW) (POP) (CUT PTW))

(DEFPROP F
    (LAMBDA (A)
        (COND ((MEMBER A FE) T) ((SETR 'FEATURES (SETQ FE (CONS A FE)) C))))
    EXPR)

(DEFUN FE (NODE) (GETR 'FEATURES NODE))

(DEFUN FEATURE? (FEATURE) (MEMQ FEATURE FE))

(DEFUN FESET (NODE FEATURES) (SETR 'FEATURES FEATURES NODE))

(DEFUN FLUSHME NIL
    ;; IF YOU HAVEN'T REAHED THE CUT, FLUSHES THE NEXT WORD IN THE SENTENCE.  FAILS IF IT REACHES CUT POINT
    (AND N NN (SETQ NN (NOT (EQ CUT (SETQ N (CDR N)))))))

(DEFUN FOLLOWING (LIST MEMBER)
    ;; GET THE ELEMENT OF LIST FOLLOWING MEMBER
    (AND (MEMQ MEMBER LIST) (CDR (MEMQ MEMBER LIST))))

(DEFUN FQ FEXPR (A)
    (MAPCAR #'(LAMBDA (X) (OR (MEMQ X FE) (SETQ FE (CONS X FE)))) A)
    (SETR 'FEATURES FE C))

(DEFUN GETR (REGISTER NODE)
    ;; THIS FUNCTION RETRIEVES THE CONTENTS OF THE REGISTER ASSOCIATED WITH THE GIVEN NODE
    (GET (CAR NODE) REGISTER))

(DEFUN H (NODE) (GETR 'DAUGHTERS NODE))

(DEFPROP ISQ (LAMBDA (A) (MEMQ (CADR A) (FE (EVAL (CAR A))))) FEXPR)

(DEFUN ISX (A B) (MEMBER B (FE A)))

(DEFPROP M (LAMBDA (A) (SETQ ME (CONS A ME))) EXPR)

(DEFPROP MP (LAMBDA (A) (SETQ MESP A)) FEXPR)

(DEFPROP MQ (LAMBDA (A) (SETQ ME (CONS A ME))) FEXPR)

(DEFUN N (NODE) (GETR 'WORDAFTER NODE))

(DEFUN NB (NODE) (GETR 'FIRSTWORD NODE))

(DEFUN NEXTW FEXPR (A) (EQ (CAR N) (CAR A)))

(DEFUN NEXTWORD NIL (CAR N))                ;; RETURN THE NEXT WORD IN THE SENTENCE

(DEFUN NEXTWORD? (A) (EQ (CAR N) A))

(DEFUN NQ FEXPR (A) (MEMQ (CAR A) (FE N)))

(DEFUN ONLY-ONE-WORD-LEFT NIL (AND N (NOT (CDR N))))

(DEFUN PARENT (NODE) (GETR 'PARENT NODE))

(DEFUN PARSE FEXPR (A)
    (COND ((MEMQ (CAR A) '(NG CLAUSE VG PREPG ADJG)) (PARSE2 A (MEMQ 'TOPLEVEL A))) ((PARSE3 A NIL))))

(DEFUN PARSE2 (REST P)
    ;; THIS FUNCTION CALLS THE PROGRAMMAR FUNCTION INDICATED BY THE
    ;; FIRST MEMBER OF REST - A FEATURE LIST THE PARAMETER P
    ;; INDICATES WHETHER PARSE2 IS BEING CALLED FROM THE TOPLEVEL
    ;; IF P IS TRUE, PARSE2 DOES NOT APPEND THE NODE JUST PARSED TO
    ;; THE PARSING TREE PARSE2 WILL CALL EITHER A COMPILED OR
    ;; INTERPRETED VERSION OF THE GRAMMAR PROGRAM
    (PROG (UNIT CREATED-NODE END PARENT RE SPECIAL NBB)
        (SETQ UNIT (CAR REST) LEVEL (1+ LEVEL))
        (COND ((EQ (SETQ NBB N) CUT)
            (SETQ LEVEL (1- LEVEL))
            (RETURN NIL)))
        (SETQ END CUT)
        (SETQ NN (NOT (EQ N CUT)))
        (SETQ PARENT C)
        (COND ((NQ B-SPECIAL)
            (AND PARSETRACE
                (PROGN (PRINTC '\ \ SPECIAL\ WORD)
                (PRINC (CAR N))))
            (EVAL (GETR 'B-SPECIAL N))))
        (COND ((EQ SPECIAL 'SKIP) (GO SKIP))
            ((EQ SPECIAL 'DONE) (GO DONE))
            ((EQ SPECIAL 'LOSE) (SETQ N NBB) (GO LOSE)))
        (AND PARSETRACE
            (PROGN (TERPRI)
                (PRINC '\() ;; sic!)
                (PRINC LEVEL)
                (PRINC '\ ####\ PARSING:\ )
                (PRINC REST)))
        (COND ((NULL (SETQ RE (APPLY-GRAMMAR UNIT)))    ;; THIS IS WHERE ALL THE WORK HAPPENS.  IF THE PARSE SUCEEDS,
            (SETQ RE NIL)                               ;; IT WILL RETURN THE NODE THAT HAS BEEN BUILT UP
            (SETQ N NBB)                                ;; (SEE THE FUNCTION "INTERPRETATION" IN IN GINTER)
            (GO LOSE)))
    SKIP (COND ((EQ N CUT))
            ((NQ SPECIAL) (EVAL (GETR 'SPECIAL N))))
    DONE (OR P
            (REBUILD (SETQ FE (GET (CAR C) 'FEATURES))  ;; REBUILDS THE HIGHER LEVEL NODE TO INCORPORATE
                NB                                      ;; THE DAUGHTER THAT WAS JUST PARSED EXCEPT IN THE
                N                                       ;; CASE WHERE THIS NODE IS THE TOPLEVEL
                (SETQ H (APPEND RE H))
                SM
                C))
    LOSE (SETQ NN (NOT (EQ N CUT)))
    OK  (COND ((AND RE (OR (EQ PARSETRACE 'ALL) (EQ PARSEBREAK 'ALL) (MEMQ UNIT PARSEBREAK) (MEMQ UNIT PARSETRACE)))
            (TERPRI)
            (PRINC '\() ;; sic!)
            (PRINC LEVEL)
            (PRINC '\ PARSE\ SUCEEDED:\ )
            (PRINC UNIT)
            (PRINC '\ \ )
            (PRINC (FROM (NB RE) N))
            (AND PARSENODE-SEE (DP (CAR RE)))
            (AND (OR (EQ PARSEBREAK 'ALL)
                    (MEMQ UNIT PARSEBREAK))
                (ERT)))
            ((OR PARSEBREAK PARSETRACE)
                (TERPRI)
                (PRINC '\() ;; sic!)
                (PRINC LEVEL)
                (PRINC '\ PARSE\ FAILED)
                (AND (OR (EQ PARSEBREAK 'ALL)
                        (MEMQ UNIT PARSEBREAK))
                    (ERT))))
        (PARSE-STATISTICS)                              ;; DEFINED IN SYSCOM
        (SETQ LEVEL (1- LEVEL))
        (RETURN RE)))

(DEFUN PARSE3 (REST P)
    ;; PARSE3 IS CALLED TO PARSE JUST THE NEXT WORD IN THE SENTENCE
    (PROG (XP LABL RE SPECIAL NBB NODE)
        (COND ((EQ (SETQ NBB N) CUT) (MQ CUT) (RETURN NIL))
            ((NQ B-SPECIAL)                                     ;; IS THE NEXT WORD MARKED SPECL?
            (EVAL (GETR 'B-SPECIAL N))                          ;; YES, DO SOMETHING SPECIALL
            (COND ((EQ SPECIAL 'SKIP) (GO SKIP))
                ((EQ SPECIAL 'LOSE) (SETQ N NBB) (RETURN NIL))
                ((EQ SPECIAL 'DONE) (GO DONE)))))
        (COND ((CAR (SETQ XP REST)))                            ;; IF CALL IS (PARSE NIL FOO)
            ((NEXTWORD? (CADR REST)) (GO OK))                   ;; THEN LOOK FOR EXACT WORD "FOO"
            ((SETQ N NBB) (RETURN NIL)))                        ;; IF NOT THERE, FAIL
    LOOP (COND ((NOT (ATOM (CAR XP)))
            (SETQ LABL (CONS (CAAR XP) LABL)))                  ;; IF THE FEATURE IS NOT AN ATOM JUST ADD THE
            ((EQ (CAR XP) 'NULL))                               ;; FEATURE TO THE LIST
            ((MEMQ (CAR XP) (FE N)))
            ((MEMQ (CAR XP) UNMKD))
            ((M (CAR XP)) (SETQ N NBB) (RETURN NIL)))
        (COND ((SETQ XP (CDR XP)) (GO LOOP)))
    OK  (SETQ RE
            (BUILDNODE (MEET (APPEND (FE N) LABL) (GET (CAR REST) 'ELIM))
                N
                (CDR N)
                'WORD
                (OR SMN
                    (NULL (CAR REST))
                    (AND (NULL (SM N)) (UNDEFINED))
                    (CADR (SASSOC (CAR REST)
                        (SM N)
                        #'UNDEFINED)))))
        (SETQ N (CDR N))
    SKIP (SETQ NN (NOT (EQ N CUT)))
        (COND ((AND NN (NQ SPECIAL))
            (EVAL (GETR 'SPECIAL N))))
    DONE (SETR 'PARENT C RE)
        (COND (P RE)
            (T (REBUILD FE NB N (SETQ H (APPEND RE H)) SM C)))
        (AND PARSENODE-SEE RE (DP (CAR RE)) PNS-BK (ERT))
        (PARSE-STATISTICS)
        (RETURN RE)))

(DEFUN PARSEREL (A B NODE)
    (PROG NIL
    =>  (COND ((NULL A) (RETURN NIL))
            ((NOT (ISX NODE (CAAR A))))
            ((EVAL (APPEND '(PARSE CLAUSE RSNG) (CDAR A) B))
                (RETURN H)))
        (SETQ A (CDR A))
        (GO =>)))

(DEFUN POP FEXPR (A)
    (COND ((OR (NULL A) (NULL (CAR A)))
            (COND ((NULL H) NIL)
                ((SETQ N (NB H))
                    (SETQ H (CDR H))
                    (REBUILD FE NB N H SM C)
                    (SETQ NN (NOT (EQ N CUT)))
                    (OR SMN
                        (PROG (XX)
                            (MAP #'(LAMBDA (BACKNODE)
                                (ERRSET
                                    (AND (MAP #'(LAMBDA (PLACE) (AND (EQ PLACE (NB BACKNODE)) (ERR))) N)
                                        (SETQ XX (CONS (CAR BACKNODE) XX)))))
                                BACKREF)
                            (SETQ BACKREF XX)))
                    T)))
        ((EVAL (CONS 'POPTO A)) (POP))))

(DEFUN POPTO FEXPR (A)
    (PROG (XX)
        (SETQ XX H)
    LOOP (COND ((EVAL (CONS 'ISQ (CONS 'XX A))))
            ((SETQ XX (CDR XX)) (GO LOOP))
            ((MQ POPTO) (RETURN NIL)))
    EX  (COND ((EQ XX H) (RETURN C))
            ((POP) (GO EX)))))

(DEFUN PREVIOUS (LIST MEMBER)
    ;; GET THE ELEMENT OF LIST BEFORE MEMBER
    (PROG (GOODIE)
    =>  (COND ((NULL LIST) (RETURN NIL))
            ((EQ MEMBER (CAR LIST)) (RETURN GOODIE))
            (T (SETQ GOODIE (CAR LIST)) (SETQ LIST (CDR LIST))))
        (GO =>)))

(DEFUN PTFIND (X YY Z)
    (PROG (FOO)
        (SETQ FOO (CAR X))
    UP  (COND ((MOVE-PT U) (GO UP)) ((EQ (NB PT) X) (GO ON)))
    DOWN (OR (MOVE-PT DLC PV (MEMQ FOO (NB PT))) (RETURN NIL))
    ON  (COND ((NOT (EQ X (NB PT))) (GO DOWN))
            ((EQ YY T))
            ((MOVE-PT DF (EQ (N PT) YY)))
            ((RETURN NIL)))
    CHECK (COND ((EVAL Z) (RETURN PT))
            ((NOT (EQ YY T)))
            ((MOVE-PT DF) (GO CHECK)))))

(DEFUN REBUILD (FEATURES FIRSTWORD WORDAFTER DAUGHTERS SEMANTICS NODE)
    (SETR 'FEATURES FEATURES NODE)
    (SETR 'FIRSTWORD FIRSTWORD NODE)
    (SETR 'WORDAFTER WORDAFTER NODE)
    (SETR 'DAUGHTERS DAUGHTERS NODE)
    (SETR 'SEMANTICS SEMANTICS NODE)
    NODE)

(DEFUN ROOT (X)
    ;; INPUT= PIECE OF SENTENCE
    ;; OUTPUT= ROOT OF FIRST WORD IN THAT PIECE
    ;; IF WORD HAS NO ROOT PROPERTY, THE ROOT == WORD
    (OR (GET (CAR X) 'ROOT) (CAR X)))

(DEFUN RQ FEXPR (A) (SETR 'FEATURES (SETQ FE (SETDIF FE A)) C)) ;; REMOVE THE FEATURE A FROM FEATURE LIST OF THE CURRENT NODE

(DEFUN SECONDWORD? (WORD) (AND N (CDR N) (EQ (CADR N) WORD)))

(DEFUN SETR (REGISTER VALUE NODE)
    ;; THIS FUNCTION ASSOCIATES THE GIVEN VALUE WITH THE GIVEN
    ;; NODE UNDER THE GIVEN INDICATOR, REGISTER
    (PUTPROP (CAR NODE) VALUE REGISTER))

(DEFUN SM (NODE) (GETR 'SEMANTICS NODE))

(DEFUN TRNSF FEXPR (A)
    (SETR 'FEATURES
        (SETQ FE (UNION (MEET A (FE PT)) FE))
        C))

(DEFUN UPREL (X)
    (AND (NOT (ATOM X))
        (OR (MEMQ 'UPREL (FE X)) (UPREL (H X)) (UPREL (CDR X))))) ;; FIND NODE WITH UPREL FEATURE

(DEFUN WORD (N) (CAR N))

(DEFPROP UPCHECK
    (LAMBDA NIL
        (AND (MOVE-PT C U (REL-NOT-FOUND))
            (NOT (MEET (FE PT) '(OBJ1Q OBJ1REL OBJ2Q OBJ2REL LOBREL LOBQ)))))
    EXPR)

#_(ns shrdlu.ginter)

(DEFUN PDEFINE FEXPR (A)
    ;; THIS PDEFINE MERELY PUT THE PROGRAMMAR FUNCTION ON THE
    ;; PROPERTY LIST OF THE PARSE NAME UNDER THE INDICATOR
    ;; 'INTERPRET. IT ALSO ADDS THE TAGS FAIL AND RETURN. NOTE THAT
    ;; THE PDEFINE BODY IS SIMILIAR TO PROG BODY. THIS SETS UP
    ;; INTERPRETED PROGRAMMAR EXECUTIONS
    (PUTPROP (CAR A)
        (NCONC (CDR A) (LIST 'FAIL '(RETURN 'FAIL) 'RETURN '(RETURN 'RETURN)))
        'INTERPRET))

(DEFUN INTERPRET (UNIT)
    ;; INTERPRET IS THE FUNCTION WHICH 'CALLS' AN INTERPRETED PROGRAMMAR PROGRAM.
    ;; IT FIRST DECLARES AND INITIALIZES ALL THE RELAVENT VARIABLES, THEN
    ;; IT EXECUTES THE PROGRAMMAR BODY AS A PROG.  NOTE THE USE OF "RE":
    ;; IT IS SET TO A NODE ONE WISHES TO BE THE INITIAL DAUGHTER OF THIS NODE.
    ;; ONLY CONJ NEEDS THIS HACK.
    (PROG (FE H ME NB C SM CUT NN T1 T2 T3)
        (SETQ NN T)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))     ;; FEATURE LIST
            (SETQ NB (OR (NB RE) N))                    ;; BEGINNING IN SENTENCE OF THIS NODE
            N                                           ;; SENTENCE POINTER JUST AFTER THIS NODE
            (SETQ H RE)                                 ;; DAUGHTERS OF THIS NODE
            NIL))                                       ;; SEMANTIC JAZZ
        (SETR 'PARENT PARENT C)                         ;; SET PARENT REGISTER
        (COND ((EQ (APPLY 'PROG (GET UNIT 'INTERPRET)) 'RETURN)
            (GO RETURN)))                               ;; APPLY THE PROGRAMMAR PROGRAM
    FAIL (SETQ MES ME)
        (SETQ N (OR (N RE) NB))                         ;; RESET SENTENCE POINTER
        (RETURN NIL)
    RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN : FEXPR (BRANCH)
    (COND ((EVAL (CAR BRANCH))                          ;; EVALUATE BRANCH CONDITION
            (COND ((AND (NULL NN) (CDDDR BRANCH))
                    (GOCHECK (CDDR BRANCH)))            ;; IF TRUE AND NO MORE SENTENCE REMAINS
                (T (GOCHECK BRANCH))))                  ;; AND IF THIRD BRANCH GIVEN,THEN GO TO THIRD
        (T (GOCHECK (CDR BRANCH)))))                    ;; BRANCH 2ND BRANCH

(DEFUN GOCHECK (LABEL)
    ;; THE GOCHECK FUNCTION CHECKS THE BRANCHES OF THE PROGRAMMAR CONDITIONAL.
    ;; IF THE BRANCH IS NIL, GOCHECK MERELY RETURNS.
    ;; IF THE BRANCH IS NON-ATOMIC, IT IS TAKEN AS A FAILURE MESSAGE.
    ;; GOCHECK PUTS THE MESSAGE ON THE MESSAGE LIST AND GOES TO FAIL (IN GINTERP)
    ;; IF THE BRANCH IS ATOMIC, GOCHECK GOES TO IT.
    ;; REMEMBER THAT THE GO LEADS TO A POINT SOMEWHERE IN THE PROGRAMMAR PROGRAM,
    ;; UNLESS IT IS 'FAIL OR 'RETURN.
    (COND ((NULL (CADR LABEL)) T)
        ((ATOM (CADR LABEL)) (GO (CADR LABEL)))
        (T (M (CADR LABEL)) (GO FAIL))))

(DEFUN GOCOND FEXPR (A)
    ;; GOCOND GOES TO THE 1ST OR 2ND OF TWO TAGS DEPENDING IF THERE REMAINS
    ;; ANY MORE OF THE SENTENCE YET TO BE PARSED.
    (COND (NN (GO (CAR A))) (T (GO (CADR A)))))

#_(ns shrdlu.gramar)

(PDEFINE CLAUSE (POSITION-OF-PRT MVB LOCATIONMARKER SUBJ-VB-BACKUP-TYPE1 POSITION-OF-PTW)

    ENTERING-CLAUSE
        (SETR 'TIME (BUILD TSSNODE= (MAKESYM 'TSS)) C)
        (: (CQ SIMP) SUBJ NIL)
        (: (CQ MAJOR) INIT SEC)

    INIT
        (SETQ LOCATIONMARKER N)
        (: (AND (NQ BINDER) (PARSE CLAUSE BOUND INIT)) NIL MAJOR FIXIT)
        (FQ BIND)
        (: (CALLSM (SMBIND)) INIT NIL)

    FIXIT
        (SETQ PTW CUT)
        (: (CUT (MOVE-PTW)) INIT MAJOR)

    MAJOR
        (CUT END)
        (COND ((EQ PUNCT '?) (GO QUEST))
            ((OR (CQ IMPER) (EQ PUNCT '!)) (GO IMPER)))
        (GO THEREINIT)

    FDEC
        (FQ DECLAR)

    THEREINIT                                                       ;; CONSTRUCTIONS USING THE FUNCTION WORD "THERE"
        (: (AND (NEXTWORD? 'THERE)                                  ;; ARE CHECKED FOR EXPLICITLY AND PROCESSED BY A
                (PARSE NIL THERE)                                   ;; SPECIAL BLOCK OF CODE (SEE ABOVE)
                (FQ DECLAR))
            THERE
            NIL
            (INIT))

    THER2
        (AND (NQ PREP)
            (PARSE PREPG INIT)
            (OR (CALLSM (SMRELATE H))                               ;; MORE INITIAL (BEFORE THE SUBJECT) MODIFIERS
            (POP)))
        (AND (NQ ADV)
            (PARSE ADV TIMW)
            (OR (CALLSM (SMADVERB)) (POP)))
        (AND (NQ ADV)
            (PARSE ADJG ADV VBAD)
            (OR (CALLSM (SMRELATE H)) (POP)))
        (PARSE NG TIME)

        (: (EQ LOCATIONMARKER N) CLAUSETYPE INIT INPOP)

    ;; THE VARIABLE "LOCATIONMARKER" MARKS THE POSITION OF PTW
    ;; AT THE TIME THAT IT WAS SET.  IF IT HAS NOT MOVED (IS
    ;; STILL EQUAL TO N) THAN THAT INDICATES THAT NOTHING HAS
    ;; BEEN PARSED AND WE CAN GO ON, OTHERWISE, THERE CAN BE ANY
    ;; NUMBER OF INITIAL MODIFIERS AND THE CODE STARTING AT
    ;; "INIT" IS REPEATED, AS MANY TIMES AS NECESSARY.  IF PTW
    ;; HITS THE CUT POINT, THAN IT IS ASSUMED THAT SOMETHING WAS
    ;; MISTAKENLY PARSED AS A MODIFIER WHEN IT WAS NOT, AND
    ;; EVERYTHING IS POPPED OFF (BY THE "INPOP" CODE)

    INPOP
        (: (MOVE-PT C DLC) NIL (INPOP))                             ;; DOES ANYTHING REMAIN ON THE TREE?

    BICUT
        (CUT-BACK-ONE)                                              ;; "CUT-BACK-ONE" IS THE NORMAL BACKINGUP
        (GO INIT)                                                   ;; MECHANISM FOR THE GRAMMAR, IT SETS PTW (POINTER
                                                                    ;; TO THE WORD) BACK ONE FROM WHERE IT WAS AND
                                                                    ;; SETS "CUT" TO PTW. THE FOLLOWING GOTO TELLS
                                                                    ;; WHICH BLOCK OF CODE IS TO BE REPEATED.

    ;; RE-EXAMINE THE CLAUSETYPE, PARTICULARLY TO CHECK FOR VERB-INITIAL IMPERATIVES

    CLAUSETYPE
        (: (CQ DECLAR) SUBJ NIL)
        (: (AND (NQ VB) (NQ INF) (PARSE VG IMPER) (FQ IMPER))
            VG1
            NIL)                                                    ;; SEE THE NOTE UNDER IMPERATIVES BELOW
        (FQ DECLAR)
        (: (CQ IMPER) (IMPER) NIL)

    ;; ###############################################################
    ;;         TRY TO PARSE A GRAMMATICLY ACCEPTABLE SUBJECT
    ;; ###############################################################

    ;; ONCE THAT IS DONE, SET THE SUBJECT REGISTER (FOR USE BY SEMANTIC ROUTINES AND OTHER PARTS OF THE GRAMMAR)
    ;; AND MOVE ONE TO THE CODE FOR WHICH LOOKS FOR THE MAIN VERB (MVB) - "VG"

    SUBJ (CUT END)                                                  ;; RESET CUTPOINT INCASE IT WAS MODIFIED BY
    SUBJ3                                                           ;; PREVIOUS BACKUPS IF THE FIRST WORD INDICATES
        (: (OR (AND (NEXTWORD? 'TO)                                 ;; THE POSSIBILITY OF A RANK-SHIFTED CLAUSE
                    (PARSE CLAUSE RSNG TO SUBJ))                    ;; SERVING AS THE SUBJECT, THEN TRY TO PARSE ONE
                (AND (PARSE CLAUSE RSNG ING SUBJ)))                 ;; AS SUCH FEATURE "SUBJ" INSURES A CHECK THAT ANY
            SUBREG                                                  ;; PRONOUNS FOUND ARE IN SUBJECTIVE CASE.
            NIL
            SUBJ1)
    SUBJ4
        (: (PARSE NG SUBJ) SUBREG NIL SUBJ1)                        ;; IF PARSING THE SUBJ CAUSES THE CUT POINT TO BE
                                                                    ;; REACHED, THEN JUMP TO "SUBJ1" TO SEE IF WE ARE
                                                                    ;; IN CONDITIONS WHERE THAT IS ALLOWED

                                                                    ;; WHAT TO DO IF THE SUBJECT CANNOT BE DIRECTLY
                                                                    ;; PARSED THIS IS CHECKING FOR THE SITUATION WHERE
                                                                    ;; A QUESTION WORD IS ACTING AS LOGICAL SUBJECT
        (COND ((CQ REL-NOT-FOUND)                                   ;; AND HAS ALREADY BEEN PARSED AS IN "WHAT IS IN
                                                                    ;; THE BOX?" THE CLAUSE WILL HAVE THIS FEATURE IF
                                                                    ;; IT IS ACTIVE AS A RSQ AND ITS MISSING ELEMENT
            (RQ REL-NOT-FOUND)                                      ;; HAS NOT YET BEEN DETERMINED.  SINCE WE CANNOT
            (SETR 'SUBJECT (GETR 'RELHEAD C) C)                     ;; FIND ANY SUBJECT, WE ASSUME THAT IT IS A
            (GO VB))                                                ;; SUBJECT-RELATIVE IN THIS CASE.
        (SUBJ-VB-BACKUP-TYPE1
            (SETQ SUBJ-VB-BACKUP-TYPE1 NIL)
            (GO SUBJ11))                                            ;; SEE THE LARGE NOTE ABOUT THIS IN "NOVERB".
        ((AND H (ISQ H TIME) (ISQ H NG))
            (SETR 'SUBJECT H C)
            (GO VB))                                                ;; WHAT WAS INITIALLY PARSED AS A TIME-NG MODIFING
        ((MOVE-PT C U (REL-NOT-FOUND))                              ;; THE WHOLE CLAUSE MAY PROBABLY BEEN THE SUBJECT
                                                                    ;; OF THE CLAUSE THIS WORRIES ABOUT RELATIVE
                                                                    ;; CLAUSES. PLEASE NOTE THAT THE CURRENT
                                                                    ;; HALF-VERSION HAS NOT YET GOT ITS HEAD TOGETHER
                                                                    ;; ABOUT RELATIVE CLAUSES. -IE. THE CODE ISN'T
            (SETR 'SUBJECT (GETR 'RELHEAD PT) C)                    ;; DEBUGGED AND HAS GAPS IN IT ESP. WHO SETS WHAT
            (SETR 'RELHEAD (GETR 'RELHEAD PT) C)                    ;; REGISTER WHEN THIS WILL BE FIXED BEFORE THE
            (REMOVE-F-PT 'REL-NOT-FOUND PT)                         ;; VERSION IS FINALIZED
            (GO VB))
        ((AND (CQ COMPONENT) NN) (FQ SUBJFORK) (GO VB))             ;; "SARAH ATE DINNER AND WENT TO THE MOVIES."
        (H (POP) (GO SUBJ))                                         ;; POP OFF THE CLOSEST INITIAL MODIFIER AND TRY TO
        ((GO FAIL)))                                                ;; PARSE A SUBJ AGAIN

    HEAD
        (: (OR (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON)))       ;; COME HERE (ONLY?) TO TRY TIME PHRASE AS SUBJECT
            NIL
            (HEAD))                                                 ;; MOVE PTW TO THE CLOSEST NOUN THEN SET THE CUT

    SUB2
        (: (POP) NIL FAIL)                                          ;; POINT TO IT AND ATTEMPT A NEW PARSING IF
        (: (CUT PTW) INIT SUB2)                                     ;; NOTHING MORE TO POP, LOSE

    SUBJ1
        (COND ((ISQ H QUOTED)                                       ;; CIRCUMSTANCES UNDER WHICH IT IS ALLRIGHT TO
            (AND (ISQ H LIST) (FQ LIST))                            ;; HAVE NOTHING FOLLOWING THE SUBJECT OF THE
            (FQ QUOTED)                                             ;; CLAUSE "  "MUMBLE", SAID JOHN."
            (SETQ H (H H))
            (GO RETSM)))
        (AND (CQ REL-NOT-FOUND)                                     ;; THIS IS PART OF A BACKUP MECHANISM WHICH NEEDS
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
        (: (CUT-BACK-ONE) SUBJ3 (SUBJ11))                           ;; IF WE CAN'T CUT BACK ANY FURTHER, THEN FAIL

    SUBREG
        (SETR 'SUBJECT H C)                                         ;; THIS SETS THE "SUBJECT" REGISTER OF THE CURRENT
        (GO VB)                                                     ;; CURRENT NODE TO WHATEVER IS POINTED TO BY "H"
                                                                    ;; (IN THIS CASE THAT WOULD BE THE MOST RECENTLY
                                                                    ;; PARSED DAUGHTER OF THE CURRENT NODE)

    ;; ##################################################################
    ;;                         PARSE A VERB GROUP
    ;; ##################################################################

    ;; ONCE THE VERB GROUP IS PARSED, THE TRANSITIVITY OF THE MAIN VERBIS EXAMINED - CAUSEING THE
    ;; APPROPRIATE SECTIONS OF THE CODE BELOW TO BE EXECUTED AND TO ATTEMPT TO PARSE THE REQUIRED OBJECTS
    ;; ONCE ALL THE OBJECTS HAVE BEEN PARSED, THE PROGRAM JUMPS TO THE TAG "ONT", WHERE A SEMANTICS PROGRAM
    ;; IS CALLED TO MAKE SENCE OUT OF EVERYTHING

    VB  (: (PARSE ADJG ADV VBAD) VB NIL (VB-ADJG))                  ;; PARSE ANY INITIAL MODIFIERS
        (RQ VBLOK)                                                  ;; ?????

    VBL (: (PARSE VG) VBREG NIL)                                    ;; ONCE THE VERB GROUP IS PARSED, SET THE REGISTER

    NOVERB
        (COND                                                       ;; WHAT TO DO IF THE VG CANNOT BE DIRECTLY PARSED?
            ((CQ SUBJFORK) (FQ VBFORK) (GO FINDOBJ1))
            ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
            ((NOT (ISQ H SUBJ)) (GO FAIL))
            ((ISQ H CLAUSE)
                (SETQ SUBJ-VB-BACKUP-TYPE1 T)
                (POP)
                (GO SUBJ4))                                         ;; THIS IS EXACTLY WHAT IS LOOKS LIKE. IE. AN
                                                                    ;; ARBITRARY, NOT TOO WELL THOUGHTOUT BACKUP
                                                                    ;; MECHANISM. (NEEDLESS TO SAY IT WILL GO AWAY
                                                                    ;; FAST)  WE HAVE BEEN UNABLE TO FIND A VERB AND
                                                                    ;; HAVE NOTICED THAT WE PARSED A CLAUSE OF SOME
                                                                    ;; SORT AS THE SUBJECT. HYPOTHESIS: WE
                                                                    ;; MISSINTERPRETED SOMETHING WHILE PARSEING THAT
                                                                    ;; CLAUSE AND MANAGED TO SWALLOW UP THE VERB OF
            ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))            ;; THE HIGHER CLAUSE WITH IT. SOLUTION: POP OFF
    VB2
        (CUT-BACK-ONE)                                              ;; THE CLAUSE AND TRY TO  REPARSE THE SEGMENT IN
        (GO SUBJ3)                                                  ;; ANOTHER FASHION. "SUBJ4" IS PLACED THE THE
                                                                    ;; SUBJECT CODE AFTER LOOKING FOR CLAUSES AND
                                                                    ;; BEFORE NOUN GROUPS. DEFAULT CUTTING MECHANISM
                                                                    ;; FOR VBL
    VBREG
        (SETR 'VG H C)

    ;; ###############################################################
    ;;
    ;;             PARSE ANY OBJECTS REQUIRED BY THE VERB
    ;;
    ;; ###############################################################

    VG1 (CUT END)                                                   ;; RESET THE CUTPOINT IN CASE ANYONE CHANGED IT
        (: (ISQ MVB BE) BE NIL (BE))                                ;; JUMP TO "BE" PROCESSOR

        ;; There used to be a check here for a quoting MVB with a quoted subject.
        ;; It was deleted because it went to a tag that no longer exists and doesn't seem to have any modern analogs.
        ;; For the original code: see "gramar 19" or earlier.
        ;; It was put in by Jeff Hill in the spring of 1972.

        ;; VERB-PARTICLE COMBINATIONS SUCH AS "PUT ON", "SET DOWN", ETC.
        ;; THEIR ESSENTIAL PROPERTY IS THAT VERB AND PARTICLE CAN BE DISPLACED BY THE OBJECT.
        ;;    "PUT DOWN THE BLOCK."
        ;;    "PUT THE BLOCK DOWN."

        (: (ISQ MVB VPRT) NIL CHECKPASV CHECKPASV)
        (: (AND (NQ PRT) (PARSE PRT)) NIL DPRT)                             ;; IF THE PARTICLE IS NOT THE WORD FOLLOWING THE VERB, THEN
        (FQ PRT)                                                            ;; IT IS SEARCHED FOR BY CODE AT "DPRT" (DISPLACED PARTICLE)

        (: (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD (NB H))))            ;; IS THIS A LEGITIMATE COMBINATION OF VERB AND PARTICLE?
            CHECKPASV
            POPRT)

    DPRT
        (: (ISQ H PASV) CHECKPASV NIL)                                      ;; SEARCH FOR DISPLACED PARTICLE.  NO DISPLACED PARTICLES
        (: (SETQ POSITION-OF-PRT (MOVE-PTW N NW (PRT))) NIL FINDOBJ1)       ;; IN PASV'S IF NOT FOUND ASSUME THAT IT IS OPTIONAL AND
        (: (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD POSITION-OF-PRT)))   ;; WE ARE DEALING WITH THE CASE WITHOUT THE PARTICLE
            NIL
            POPRT)
        (: (ISQ MVB TRANS) NIL FINDOBJ1)
        (CUT POSITION-OF-PRT)
        (: (PARSE NG OBJ OBJ1)                                              ;; PARSE UP ANY NOUN GROUP YOU FIND
            POPRT
            FINDOBJ1                                                        ;; IF THERE ARE MORE OR LESS NP'S THAN EXPECTED,
            NIL)                                                            ;; THEN DON'T PARSE ANYTHING BUT GO TO NPRT
        (CUT END)                                                           ;; INSTEAD. SIMILARLY, IF ANYTHING FOLLOWS THE
        (SETR 'OBJ1 H C)                                                    ;; DISPLACED PARTICLE THEN A GRAMMATICALLY BAD
        (PARSE PRT)                                                         ;; FORM IS ASSUMED AND THE PIECES POPED OFF
        (FQ PRT DPRT)
        (GO FINDOBJ2)

    POPRT
        (POPTO VG)
        (GO FINDOBJ1)

    ;;  CHECK THE VERB FOR THE PASSIVE CONSTRUCTION

    CHECKPASV
        (: (AND (ISQ H PASV)
                (FQ PASV)
                (SETR 'OBJ1 (GETR 'SUBJECT C) C))
            FINDOBJ2
            NIL
            FINDFAKE2)
        (FQ ACTV)                                                   ;; NOT PASV=ACTIVE
        (GO FINDOBJ1)

    BE
        (FQ BE)
        (AND (PARSE NIL NOT) (FQ NEG))
        (PARSE ADV VBAD)

    FINDOBJ1
        (: (OR (CANPARSE 1 '(ADJG COMP) 'INT) (CANPARSE 1 '(NG COMP) 'INT))
            CHECKIT
            NIL
            ONT)
        (: (OR (CANPARSE 1 '(PREPG COMP) 'INT)
                (CANPARSE 1 '(CLAUSE RSNG ING) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG REPORT) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG TO) 'TRANS)
                (CANPARSE 1 '(PREPG LOC) 'ITRNSL)
                (CANPARSE 1 '(ADV PLACE) 'ITRNSL))
            ONT
            NIL)
        (: (CANPARSE 1 '(NG) 'TRANS)
            FINDOBJ2
            NIL
            FINDFAKE2)

    FINDFAKE1
        (: (MOVE-PT C U (REL-NOT-FOUND)) OBJ1REL NIL)
        (: (AND (CANTAKE 1 '(PREPG LOC) 'ITRNSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ ITRANSL))
            PUTLOBJ
            NIL)
        (: (CANPARSE 1 NIL 'ITRNS) ONT NIL)

    GOOF1
        (OR GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - FIRST OBJ))
        (GO FAIL)

    OBJ1REL
        (SETR 'OBJ1 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ1REL)

    FINDOBJ2
        (: (CANPARSE 2 '(CLAUSE RSNG TO) 'TRANS2)
            FIXSUBJECT
            NIL)
        (: (OR (CANPARSE 2 '(ADV PLACE) 'TRANSL) (CANPARSE 2 '(PREPG LOC) 'TRANSL))
            ONT
            NIL)
        (: (OR (CANPARSE 2 '(ADJG COMP) 'TRANSINT) (CANPARSE 2 '(NG COMP) 'TRANSINT))
            ONT
            NIL)
        (: (CANPARSE 2 '(NG) 'TRANS2) ONT NIL)

    FINDFAKE2
        (: (AND (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND)))
            OBJ2REL
            NIL)
        (: (AND (CANTAKE 2 '(PREPG LOC) 'TRANSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ TRANSL))
            PUTLOBJ
            NIL)

    OBJ2TO
        (PARSE ADV VBAD)
        (: (COND ((AND (NEXTWORD? 'TO)
                    (ISQ MVB TO2)
                    (PARSE PREPG TO))                               ;; THE SECOND-OBJECT THAT WE HAVE BEEN LOOKING FOR
                (SETR 'OBJ2 (GETR 'OBJ1 H) C)                       ;; MAY BE A PREPG AS IN "GIVE IT TO THE LIONS"
                (FQ TRANS2TO TRANS2))                               ;; TAKES THE OBJECT OF THE PREPOSITION "TO" AND
            ((AND (CQ PREPQ)                                        ;; MAKES IT THE OBJ2 OF THE CLAUSE.
                (MOVE-PT H PV (QUEST))
                (EQ (WORD (MOVE-PTW FW)) 'TO)
                (RQ PREPQ)
                (FQ TRANS2TOQ TRANS2)
                (SETR 'OBJ2 (GETR 'OBJ1 PT) C))))                   ;; "TO WHOM DID YOU GIVE THE MEAT?"
            ONT
            NIL)
        (: (CANPARSE 2 NIL 'TRANS) ONT FAIL)

    PUTLOBJ
        (SETR 'LOBJ PT C)
        (SETR 'RELHEAD (GETR 'QADJ PT) PT)
        (SETR 'QADJ NIL PT)
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
        (: (EQ (WORD (NB (GETR 'SUBJECT C))) 'IT)                   ;; A DUMMY FUNCTION WORD ("IT"), AS IN "IT WAS NICE TO SEE HIM."
            NIL
            ONT)                                                    ;; TO BE ADDED HERE: JOHN WAS EAGER/EASY TO PLEASE
        (: (OR (AND (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ))
                (AND (NQ ING) (PARSE CLAUSE RSNG ING SUBJ))
                (PARSE CLAUSE REPORT))
            NIL
            ONT)
        (FQ IT)
        (SETR 'LOGICAL-SUBJECT H C)                                 ;; THE CLAUSE IS THE REAL SUBJECT.
        (GO ONT)

    GOOF2
        (OR GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - SECOND OBJECT))
        (GO FAIL)

    ;; ###########################################################################################
    ;;
    ;;                               INITIAL SEMANTIC PROCESSING
    ;;
    ;; ###########################################################################################

    ONT
        (: (CQ PASV) PONT NIL)
    ONT1
        (: (CALLSM (SMCL1)) NIL (SMCL1))

        (: (NOT (CQ REL-NOT-FOUND)) TONT NIL RETSM)                 ;; IF THE FEATURE "REL-NOT-FOUND" IS PRESENT AT
                                                                    ;; THIS POINT, IT INDICATES THAT WE ARE IN A
        (: (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1)                ;; RELATIVE CLAUSE AND MAY HAVE TO DO SOME
            NIL                                                     ;; GYMNASTICS IF THE CLAUSE IS NOT TO FAIL
            PREPSHORT)                                              ;; MOVE BACK TO A QUESTION-NOUNGROUP, THEN DOWN

    TIMEQ
        (RQ REL-NOT-FOUND)                                          ;; AND BACK TO THE NOUN. IF THAT NOUN IS "TIM1"
        (FQ TIMEQ)                                                  ;; THEN ASSUME WE HAVE FOUND OUR RELATIVE ELEMENT.
        (GO TONT)

    PREPSHORT
        (: (AND (NQ PREP) (PARSE PREPG)) NIL (ONT-SHORT-PREP))
        (: (CALLSM (SMRELATE H)) NIL (ONT: SMRELATE PREPQ))
        (: (CQ REL-NOT-FOUND) PREPSHORT TONT (ONT-NOT-FOUND))       ;; WE HAVE A PREP TO TAKE THE UNATTACHED RELATIVE
                                                                    ;; AS ITS OBJECT. THE FEATURE REL-NOT-FOUND WILL
                                                                    ;; BE REMOVED IF THE PREPG DISCOVERS IT CAN'T FIND

    PONT
        (AND (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))        ;; AN OBJECT (THE REMOVING WILL BE DONE WHILE IN PREPG).
        (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)                    ;; "LOGICAL" IE. SUBJECT IN RELATIONSHIP
        (GO ONT1)                                                   ;; TO THE PROPER SEMANTIC INTERPRETATION OF THE
                                                                    ;; MAIN VERB. AGENT-PREPG CAN BE PARSED (REFLECTS
                                                                    ;; THE OPTIONALITY OF THE CONSTRUCTION)

    ;; ###################################################################################
    ;;                    CHECK FOR ADDITIONAL MODIFYING PHRASES
    ;; ###################################################################################

    TONT (: (SETQ POSITION-OF-PTW N) NIL RETSM RETSM)               ;; WE ARE USING THE SAME TECHNIQUE HERE AS WITH THE INITIAL MODIFIERS.
                                                                    ;; IE. LOOP THROUGH THE POSSIBILITIES UNTIL YOU MAKE A PASS THAT ADDS
                                                                    ;; NOTHING NEW.

    NPASV
        (: (AND (NQ PREP) (PARSE PREPG) (CALLSM (SMRELATE H)))                                  ;; PREPG
            NIL
            NIL
            RETSM)
        (: (AND (NQ TIMW) (PARSE ADV TIMW) (OR (CALLSM (SMTIME)) (GO FAIL)))                    ;; TIMW
            NIL
            NIL
            RETSM)
        (: (AND (NOT (CQ BE)) (PARSE ADJG ADV) (OR (CALLSM (SMRELATE H)) (GO FAIL)))            ;; ADV
            NIL
            NIL
            RETSM)
        (: (AND (PARSE NG TIME) (OR (CALLSM (SMTIME)) (GO FAIL)))                               ;; TIME NOUN GROUP
            NIL
            NIL
            RETSM)
        (: (AND (NQ PLACE) (PARSE ADV PLACE) (OR (CALLSM (SMPLACE)) (GO FAIL)))                 ;; PLACE
            NIL
            NIL
            RETSM)
        (: (AND (NQ BINDER) (PARSE CLAUSE BOUND) (OR (CALLSM (SMBIND)) (GO FAIL)))              ;; BINDER
            NIL
            NIL
            RETSM)
        (: (AND (NEXTWORD? 'TO) (PARSE CLAUSE TO ADJUNCT) (OR (CALLSM (SMTOADJ)) (GO FAIL)))    ;; TO CLAUSE (ADJUNCT)
            NIL
            NIL
            RETSM)
        (: (EQ N POSITION-OF-PTW) NIL TONT RETSM)                   ;; LOOP UNTILL NOTHING ELSE CAN BE PARSED
        (: (OR (NOT (CQ TOPLEVEL)) (NQ SPECIAL)) RETSM NIL)         ;; SPECIAL WORD (E.G. COMMA AND) COULD INDICATE A
        (ERT CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL)              ;; CONJUNCTION OR A BINDER
        (GO FAIL)

    ;; ##############################################################################
    ;;                                   THERE
    ;;             AS IN:  "THERE IS A BIRD SITTING ON YOUR SHOULDER"
    ;; ##############################################################################

    THERE
        (FQ THERE)
        (CUT END)
        (: (PARSE ADV TIMW) NIL NIL (THERE))                        ;; "THERE IS A BIRD.."
        (: (AND (PARSE VG) (ISQ MVB BE)) THEF NOTHE (THERE))

    THERQ
        (: (ISQ (MOVE-PT H PV (QAUX)) BE) THERQ2 NIL)               ;; IF THIS FAILS, THE THERE IS CONSIDERED TO BE
        (: (AND (NQ TIMW) (PARSE ADV TIMW)) NIL NIL (THEREQ))
        (: (AND (PARSE VG) (ISQ MVB BE)) THERQ2 NIL)
        (RQ POLR2)
        (GO NOTHE)

    THERQ2
        (FQ SUBJTQ) (FQ THERE)
        ;; THIS MAY NOT INTERFACE PROPERLY WITH THE SEMANTIC ROUTINES FOR BE
        (: (CQ POLAR) THEF ONT)

    THEF
        (: (AND (NQ ADV) (PARSE ADV TIMW)) NIL NIL (THEF))
        (: (PARSE NG SUBJ SUBJT) NIL THERREL)
        (FQ THERE)
        (SETR 'SUBJECT H C)
        (GO ONT)

    THERREL
        (: (MOVE-PT C U (REL-NOT-FOUND)) NIL NOTHE)
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (GO ONT)

    NOTHE
        (RQ THERE)
        (POP THERE)
        (AND (NQ ADV) (PARSE ADV PLACE))
        (GO THER2)

    ;; ####################################################################
    ;;                            IMPERATIVES
    ;; ####################################################################

    IMPER
        (: (PARSE NG TIME) NIL NIL IMPOP)                           ;; MODIFIERS WHICH MAY PRECEED THE VERB
        (: (AND (NQ ADV) (PARSE ADJG ADV VBAD)) NIL NIL IMPOP)
        (: (AND (NQ ADV) (PARSE ADV TIMW)) NIL NIL IMPOP)

    IMPE
        (: (PARSE VG IMPER) NIL IMPOP)
        (FQ IMPER)
        (GO VG1)

    IMPOP
        (: (POP NIL) IMPE (IMPOP))

    ;; ####################################################################
    ;;                             QUESTIONS
    ;; ####################################################################

    QUEST ;; PREP QUESTION
        (FQ QUEST)
        (: (NQ PREP) NIL NGQUES)
        (: (PARSE PREPG) NIL NGQUES (PREPQ-INCOMPLETE))             ;; "ON WHICH BLOCK DID YOU PUT IT?"
        (: (ISQ H QUEST) NIL QUEST)                                 ;; IF THE PREPG ISN'T THE QUESTION, TRY AGAIN "ON
        (SETR 'QADJ H C)                                            ;; THAT DAY, WHERE DID YOU GO?" -- MAYBE WE COULD
                                                                    ;; MAKE USE OF THE COMMA CLUE. PREPQ IS HANDLED
        (GO POLAR)                                                  ;; MUCH LIKE QADJS LIKE WHEN AND WHERE THE REST OF
                                                                    ;; THE QUESTION HAS THE SAME SYNTAX AS A POLAR (YES-NO).
    NGQUES ;; NOUN GROUP QUESTION
        (: (PARSE NG QUEST) NGQST NIL)                              ;; "WHICH ONE IS THE MURDURER?"
        (: (OR (AND (NEXTWORD? 'HOW)
                (PARSE ADJG QUEST)
                (SETR 'RELHEAD H C))                                ;; "HOW BIG...."
            (AND (NQ QADJ)
                (PARSE QADJ)
                (FQ QADJ)
                (SETR 'QADJ H C)))                                  ;; "WHAT...?",  "WHERE...?"
            POLAR
            POLAR
            NIL)
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))                                      ;; IF ALL THE SENTENCE CONSISTS OF IS THE QUESTION

    ADJQS
        (GO RETURN)                                                 ;; ADJECTIVE THEN WE SHOULD RETURN DIRECTLY

    NGQST
        (SETR 'RELHEAD H C)

    NGQST2
        (CUT END)
        (SETR 'SUBJECT H C)
        (AND (NQ ADV) (PARSE ADJG ADV VBAD))

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
            (T (MOVE-PTW N PW)
                (POP NG QUEST)
                (CUT PTW)
                (GO NGQUES)))                                       ;; POP BACK AND START FIGURING OUT THE QUESTION

    QUEST2                                                          ;; ALL OVER AGAIN
        (: (AND (NEXTWORD? 'THERE) (PARSE NIL THERE))
            THERQ
            SUBF)                                                   ;; "ARE THERE....?"

    SUBF
        (: (PARSE NG SUBJ)                                          ;; PARSE THE SUBJECT OF ANYTHING LIKE: "DID THE
                                                                    ;; WOMAN GET THE JOB?" IF SUCCESSFUL, CONTINUE AT
                                                                    ;; "SUBREG" IN THE NORMAL PART OF THE CLAUSE
                                                                    ;; PROGRAM (RESETTING THE SUBJECT REGISTER)  (THE
            SUBREG                                                  ;; BEGINNING OF THE VERB GROUP SECTION). "SUBJ1"
            NIL                                                     ;; WORRIES ABOUT WHAT SHOULD HAPPEN IF THE SUBJECT
            SUBJ1)                                                  ;; SEEMS TO FINISH THE SENTENCE
        (RQ REL-NOT-FOUND)
        (GO BE)

    POLAR
        (: (AND (NQ VB) (PARSE VB AUX (QAUX)) (SETR 'QAUX H C) (CALLSM (SMVAUX)) (SETMVB H))
            NIL
            QCHOP)
        (OR (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
        (FQ POLR2)
        (GO QUEST2)

    QCHOP
        (ERT CLAUSE: QCHOP)
        (: (POPTO CLAUSE BOUND) BICUT (QCHOP))

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
    ;; HEAD NOUN OF THE NP WITHIN WHICH THE RSQ IS BEING PROCESSED

    SEC
        (COND ((CQ BOUND) (GO BOUND))                               ;; CHECK INITIAL FEATURES AND JUMP ACCORDINGLY
            ((CQ TO) (GO TO))
            ((CQ RSQ) (GO RSQ))
            ((CQ REPORT) (GO REPORT))
            ((CQ ING) (GO ING))
            (T (MQ RSNG-TYPE) (GO FAIL)))

    BOUND ;; BINDER
        (: (PARSE BINDER) NIL (BOUND) (BINDER))
        (SETQ LOCATIONMARKER N)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)                                                   ;; "FDEC" IS NEAR THE TOP OF THE MAJOR CLAUSE

    RSQ
        (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
        (: (CQ PREPREL) NIL RSQ2)
        (PARSE PREPG PRONREL)                                       ;; THIS CALL IS BASED ON INFORMATION PASSED FROM
        (SETR 'QADJ H C)                                            ;; FAR AWAY AND EXPLAINED IN DETAIL IN THE CODE
        (GO REPORT)                                                 ;; FOR PREPOSITION GROUPS

    RSQ2
        (COND ((PARSE VG EN PASV)                                   ;; HAVING DETERMINED THAT THE VERB IS PASSIVE IF
                (OR (ISQ MVB TRANS) (GO FAIL))                      ;; IT WERE NOT ALSO TRANSITIVE, THEN WE WOULDN'T
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
            (T (GO FAIL)))                                          ;; THIS REALLY ISN'T AN RSQ

    REL
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (: (PARSE VG) VG1 NIL)                                      ;; OUR FIRST HYPOTHESIS, THAT THE SUBJECT WAS THE
                                                                    ;; RELWORD, WAS JUST PROVEN WRONG SINCE WE CANNOT
                                                                    ;; PARSE THE VG NEXT. SO WE REVISE OUR FEATURES
        (FQ REL-NOT-FOUND)                                          ;; AND JUMP TO PARSE A REAL FULL SUBJECT AS IN
        (GO SUBJ)                                                   ;; "...WHICH MARY THOUGHT WAS CHAUVANISTIC" AS
                                                                    ;; OPPOSED TO "...WHICH WAS CHAUVANISTIC"

    TO
        (: (AND (CQ COMPONENT) (PARSE VG TO TODEL)) VG1 NIL)        ;; "I WANTED TO DANCE AND SING"
        (: (NEXTWORD? 'FOR) NIL TO1)                                ;; THIS IS EXPERIMENTAL
        (PARSE NIL FOR)                                             ;; PLEASE CHECK OUT ANY FOR-CLAUSES YOU CAN THINK OF
        (FQ FOR)
        (PARSE NG SUBJ TOSUBJ)
        (SETR 'SUBJECT H C)

    TO1
        (: (PARSE VG TO) VG1 (TO))

    ING
        (: (MOVE-PTW N NW (ING)) NIL FAIL)
        (: (OR (NQ ING)
            (CQ OBJ2)
            (AND (PARSE NG SUBJ INGSUBJ)
                (SETR 'SUBJECT H C)
                (FQ SUBING)
                (RQ ING)))
            NIL
            NIL
            (ING))
        (: (PARSE VG ING) VG1 (ING))

    REPORT
        (AND (NEXTWORD? 'THAT) (PARSE NIL THAT) (FQ THAT))
        (SETQ LOCATIONMARKER N)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)

    ;; ##############################################################
    ;;                            RETURN
    ;; ##############################################################

    RETSM
        (OR (CALLSM (SMCL2)) (GO FAIL))
        (GO RETURN))

(PDEFINE NG NIL

    ENTERING-NG

    NGSTART                                                         ;; EXAMINE INITIAL FEATURES AND JUMP TO
        (COND ((CQ RELWD) (GO RELWD))                               ;; CORRESPONDING SPECIAL BLOCKS OF CODE
            ((CQ QUEST) (GO QUEST))
            ((OR (NQ QDET) (NQ QPRON)) (FQ QUEST) (GO QUEST))
            ((CQ TIME) (GO TIME))                                   ;; LOOK AT FIRST WORD
            ((NQ PROPN) (GO PROPN))
            ((NQ TPRON) (GO TPRON))
            ((NQ EVERPRON) (GO EVERPRON))
            ((NQ PRON) (GO PRON)))

    LOOK
        (COND ((NQ DET) (GO DET))                                   ;; THIS POINT MAY BE JUMPED BACK TO
            ((NQ NUM) (GO NUM))
            ((OR (NQ ING) (NQ EN) (NQ ADJ)) (GO ADJ))
            ((NQ CLASF) (GO CLASF))
            ((NQ NUMD) (GO NUMD))
            ((NEXTWORD? 'AT) (GO AT))
            ((NEXTWORD? 'AS) (GO AS))
            ((NQ NOUN) (GO NOUN))
            ((NQ TIMORD) (GO TIMORD))
            ((AND (CQ COMPONENT) (ISQ (MOVE-PT PC) QUEST))
                (GO QUEST))
            ((MQ START) (GO FAIL)))

    ;; #######################################################
    ;; IF YOU CAN PARSE ANY OF THESE SMALL THINGS, YOU'RE DONE
    ;; #######################################################

    START                                                           ;; PARSE A PROPER NOUN

    PROPN
        (PARSE PROPN)
        (FQ DEF PROPNG)
        (: (ISQ H POSS) PROPS NIL)
        (: (AND NN (NQ PROPN)) PROPN NIL)

    PROPS
        (OR (CALLSM (SMPROP)) (GO FAIL))                            ;; EXAMINE ITS SEMANTICS
        (: (ISQ H POSS) POSS PRAG)

    ;; -------------- PRONOUNS ---------------

    PRON
        (: (PARSE PRON POSS) POSS NIL RED2)                         ;; IS IT POSSESSIVE?

    PRON2
        (: (CQ NPRON) (NPRON) NIL)
        (: (OR (AND (CQ SUBJ) (PARSE PRON SUBJ))                    ;; CHECK SUBJECTIVE OR OBJECTIVE CASE
                (AND (OR (CQ OBJ) (CQ TOSUBJ) (CQ INGSUBJ)) (PARSE PRON OBJ))
                (CQ INGSUBJ))
            NIL
            (PRON))
        (FQ PRONG DEF)

    PRON3
        (: (CALLSM (SMPRON H)) NIL FAIL)                            ;; EXAMINE SEMANTICS OF PN

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
        (AND NN (NQ ADJ) (PARSE ADJ))
        (GO SMNG)

    ;; ----------- WHATEVER, WHENEVER, WHOEVER, ... -----------

    EVERPRON
        (: (AND (PARSE PRON EVERPRON) (CALLSM (SMPRON H))) NIL FAIL)
        (: (AND (PARSE CLAUSE RSQ NOREL) (CALLSM (SMRELATE H)))
            RETSM
            FAIL)

    AS  (: (AND (PARSE NIL AS) (PARSE NUMD NUMDAS) NN (PARSE NIL AS))
            NUMD2
            (AS)
            (AS))

    ;; -------------- AT + NUM ---------------

    AT
        (: (AND (PARSE NIL AT) (PARSE NUMD NUMDAT)) NIL (AT) (AT))
    NUMD2
        (: (AND (PARSE NUM) (FQ NUM NUMD)) DET1 (NUMD2) INCOM)

    ;; -------------- OTHER NUMBER WORDS ---------------

    NUMD
        (: (PARSE NUMD NUMDAN) NIL ND3 INCOM)
        (: (PARSE NIL THAN) NUMD2 INCOM POPCOM)
    ND3
        (: (PARSE NUMD NUMDALONE) NUMD2 (NUMD) (NUMD))

    ;; -------------- TIME WORDS ---------------

    TIME
        (: (AND (NQ TIME) (PARSE NOUN TIME)) RETSM NIL)
        (: (MOVE-PTW N NW (TIM1)) LOOK (TIME))

    TIMORD
        (: (PARSE ORD TIMORD) NIL FAIL)
        (: (AND (PARSE NOUN TIM1) (FQ DET DEF) (CALLSM (SMNGTIME)))
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
        (: (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR)
            IND
            (BUG)
            INCOM)

    ;; -------------- INDETERMINATE ---------------

    IND
        (: (AND (EQ (WORD (NB H)) 'ALL) (EQ (WORD N) 'THE) (PARSE DET) (FQ DEF))
            NUM
            NIL
            (THE))
        (: (AND (ISQ H QNTFR) (FQ QNTFR)) QNUM NIL)

    ;; -------------- ORDINALS AND NUMBERS ---------------

    ORD
        (: (AND (PARSE ORD) (FQ ORD)) NIL NUM INCOM)
        (: (AND (NEXTWORD? 'OF)                                     ;; TWELTH OF OCTOBER...
                (ISQ (MOVE-PTW N NW) MONTH)
                (PARSE NIL OF)
                (PARSE NOUN MONTH)
                (FQ DATE))                                          ;; REMEMBER THAT FEATURES ARE DESIGNED AS AIDS TO
            RETSM                                                   ;; SEMANTIC COMPREHENSION AS WELL AS SYNTACTIC PARSING.
            NIL)
        (: (CQ DEF) NIL ADJ)

    NUM
        (: (PARSE NUM) NIL ADJ)                                     ;; LARGE JUMP IF FALSE
        (FQ NUM)
        (: (CQ DET) NIL DET1)
        (: (COND ((AND (ISQ H NS) (CQ NS)) (RQ NPL PART)) ((CQ NPL) (RQ NS PART)))
            ADJ
            (NUM)
            INCOM)

    DET1
        (COND ((ISQ H NS) (FQ NS)) (T (FQ NPL)))                    ;; EXPLICIT CHECK FOR THE VALUE 1
        (OR NN (AND (FQ NUMBER) (GO INCOM)))

    NUMBER
        (FQ DET)
        (: (NQ OF) OF ADJ)

    QNUM
        (: (ISQ H NONUM) OF NIL)
        (: (AND (PARSE NUM) (FQ NUM)) NIL OF)
        (: (COND ((EQ (SM H) 1) (AND (CQ NS) (RQ NPL))) ((CQ NPL) (RQ NS))) ;; EXPLICIT CHECT FOR THE VALUE 1
            NIL
            (NUMD)
            INCOM)
        (: (EQ (WORD (NB H)) 'NO) ADJ NIL)                          ;; CHECKS FOR WORD "NO"

    ;; -------------- PREPG WITH "OF" ---------------

    OF  (: (AND (NQ OF) (PARSE PREPG OF)) SMOF NONE)                ;; "FIVE OF THE BLOCKS"

    SMOF
        (FQ OF)
        (: (OR (CALLSM (SMNGOF)) (NOT (POP))) RETSM INCOM)

    NONE
        (: (EQ (WORD (NB H)) 'NONE) INCOM ADJ)

    ;; ----------- PARSE ALL THE ADJECTIVES -----------

    ADJ
        (: (PARSE ADJ) NIL EPR INCOM)
        (AND (ISQ H COMPAR)
            (FQ COMPARATIVE-MODIFIER)
            (SETR 'COMPARATIVE-MODIFIER H C))
        (GO ADJ)

    EPR
        (: (OR (ISQ H SUP) (ISQ H COMPAR)) NIL CLASF INCOM)         ;; WE PARSED AN ADJ AND RAN OUT OF WORDS
        (FQ ADJ)
        (AND (NEXTWORD? 'OF)
            (PARSE PREPG OF)
            (OR (CALLSM (SMNGOF)) (GO FAIL))
            (FQ OF)
            (GO RETSM))

    ;; -------------- PARSE ALL THE CLASIFIERS ---------------

    CLASF
        (: (OR (PARSE VB ING (CLASF))                               ;; TRIES TO PARSE THE LARGEST POSSIBLE NG FIRST
                (PARSE VB EN (CLASF))
                (PARSE CLASF))
            CLASF
            NIL
            REDUC)

    ;; -------------- AND FINALLY - THE NOUN ---------------

    NOUN
        (: (PARSE NOUN) NIL RED2)

        (: (AND (CQ TIME) (NOT (ISQ H TIM1))) RED1 NIL)

    ;; -------------- MODIFY FEATURES FOR NUMBER AND SUCH --------------

        (SETQ T1 FE)
        (COND ((AND (ISQ H MASS) (OR (CQ PART) (NOT (CQ DET))))
            (FQ MASS)))
        (COND ((NOT (ISQ H NPL)) (RQ NPL PART)))
        (COND ((NOT (ISQ H NS)) (RQ NS)))
        (COND ((AND (NOT (CQ DET)) (NOT (CQ NUMD)))
            (MOVE-PT H)
            (TRNSF NPL MASS)))
        (: (MEET FE '(NS NPL PART MASS)) NIL RED0)

        (: (NEXTWORD? 'THAN) NIL SMNG)                              ;; "...A BIGGER BLOCK THAN..."
        (FQ THAN)                                                   ;; THE PRESENCE OF THIS FEATURE IS NOTED BELOW AND IN ADJG

    ;; AT THIS POINT SMNG1 IS CALLED FOR PRELIMINARY CHECKS AND ANALYSIS BEFORE CHECKING QUALIFIERS

    SMNG
        (SETR 'HEAD H C)                                            ;; SET HEAD REGISTER TO THE NOUN

        (: (AND (CQ OBOFJ) (NOT (CQ DEF))) FAIL NIL)                ;; JUST PARSED
        (OR (CALLSM (SMNG1)) (GO FAIL))
        (: (NOT (ISQ H POSS)) NIL POSS RETSM)                       ;; CHECK FOR POSSIVE

    ;; #################################################
    ;;               POSSIBLE QUALIFIERS
    ;; #################################################

        (: (AND (CQ THAN) (PARSE ADJG)) NIL RSQ-TO)                 ;; "...A BIGGER BLOCK THAN..."
        (: (CALLSM (SMRELATE H)) RETSM FAIL)

    RSQ-TO
        (: (AND (NEXTWORD? 'TO)
            (MEET FE '(COMP SUBJ))
            (PARSE CLAUSE RSQ TO)
            (OR (CALLSM (SMRELATE H)) (GO POPRET)))
            RETSM
            NIL)

    ;; -------------- AS OR COMPARATIVE ---------------

        (: (AND (OR (NEXTWORD? 'AS) (NQ COMPAR))
            (PARSE ADJG THANNEED))
            NIL
            PREPNG)                                                 ;; WHAT IS THE REASON FOR THE EXISTANCE OF THIS
        (AND (NULL N)                                               ;; STRANGE ANIMAL (ALSO THE ONEBELOW) -- CHECK
            (CQ SUBJ)                                               ;; THEM OVER AND HACK THEM PROPERLY
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (GO POPRET))                                            ;; AVOIDS ATTACHING MODIFIER WHEN IT GOBBLES TO
        (: (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)               ;; MUCH E.G. IS THE BLOCK ON THE TABLE? DOESN'T
                                                                    ;; WANT "THE BLOCK ON THE TABLE" AS A CONSTITUENT.
                                                                    ;; I ADMIT ITS A HACK.

    ;; -------------- ANY SORT OR PREPOSITION GROUP --------------

    PREPNG
        (: (AND (NQ PREP)
            (NOT (OR (AND (NQ PLACE) (CQ NOLOC))
                (AND (CQ OBJ1)
                    (ISQ MVB TRANSL)
                    (NOT (ISQ (MOVE-PT C U) QUEST)))))
            (PARSE PREPG Q))
            NIL
            DISGRSQ)
        (AND (NULL N)
            (CQ SUBJ)
            (ISQ (MOVE-PT C PV) AUX)
            (ISQ PT BE)
            (NOT (ISQ (MOVE-PT U) NGQ))
            (GO POPRET))
        (: (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)

    ;; CHECK FOR DISGUISED RSQ CLAUSES BY READING THE FAILURE MESSAGES SENT UP FROM PREPG.

    DISGRSQ
        (: (EQ (CAR MES) 'PREP-WHICH) NIL RSQ)
        (SETQ MES (CDR MES))
        (: (PARSE CLAUSE RSQ PREPREL) PREPNG (RSQ-PREPREL) RETSM)

    ;; -------------- ANY OTHER RSQ ---------------

    RSQ
        (: (AND (ISQ (MOVE-PT C U) POLR2) (CQ SUBJ) (NQ VB) (NOT (CQ SUBJT)) (NOT (ISQ PT QADJ)))
            RETSM
            NIL)
        (: (PARSE CLAUSE RSQ) NIL RETSM)
        (: (CALLSM (SMRELATE H)) RETSM POPRET)

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
        (COND ((NULL H) (MQ NO) (GO FAIL))
           ((ISQ H NUMBER) (GO INCOM))
           ((AND (ISQ H POSS)
             (OR (ISQ H PRON)
             (AND (MOVE-PT H DLC) (ISQ PT PRON))))
                (POP)
                (GO PRON2))
           ((AND (NULL (CDR H)) (CQ DEFPOSS)) (GO POSSDEF))
           ((AND (CQ QUEST) (NULL (CDR H))) (GO QDETCHECK))         ;; (CDR H) = T IF THERE IS ONLY ONE DAUGHTER TO THE CURRENT NODE
           ((ISQ H ADJ) (GO EPR))
           ((NOT (ISQ H CLASF)) (GO INCOM)))

    REDUC
        (POP)
        (: (AND (NULL H) (NQ PROPN)) PROPN NOUN)

    POPCOM
        (POP)

    ;; -------------- INCOMPLETE PHRASES ---------------

    INCOM
        (FQ INCOM)
        (: (AND (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM)))
            RETURN
            NIL)
        (: (AND (NULL CUT) (CQ NUM)) SMNG NIL)

    QDETCHECK
        (COND ((AND (ISQ H QDET) (ISQ (NB H) QPRON))
                (POP)
                (GO QPRON))
            ((AND (ISQ H QDET) (ISQ (NB H) EVERPRON))
                (POP)
                (GO EVERPRON)))
        (GO FAIL)

    ;; -------------------------------------------------
    ;; POSSESSIVE HANDLER
    ;; -------------------------------------------------

    POSS
        (OR (CALLSM (SMNG2)) (GO FAIL))

    POSS2
        (: (CQ INGSUBJ) RETSM NIL)
        ;; IF POSSESSIVE, ALL PREVIOUS MODIFIERS MODIFY THE POSSESSIVE NOUN, NOT THE NG HEAD
        (SETQ H (BUILDNODE (REVERSE (CONS 'POSS (SETDIF FE '(COMPONENT)))) NB N H SM))
        (SETQ BACKREF (APPEND H (CDR BACKREF)))
        (: (SETR 'FEATURES
            (SETQ FE (APPEND '(POSES DET DEF NS NPL) (REVERSE REST)))
            C)
            NIL
            (BUG))
        (: (OR (NOT NN) (ISQ H DEFPOSS)) NIL ORD)

    POSSDEF ;; THE PLACEMENT OF THIS TAG IS A GUESS. THE ORIGINAL IS LOST, ASSUMING THAT IT EVER EXISTED
        (RQ POSES DET DEF)
        (FQ POSSDEF NS NPL)

    ;; -------------- RELATIVES---------------

    QUEST
        (: (PARSE NIL HOW) NIL QDET FAIL)
        (: (PARSE NIL MANY) NIL FAIL INCOM)
        (FQ DET NPL INDEF HOWMANY)
        (GO OF)

    QDET
        (: (AND (PARSE DET QDET) (FQ DET NPL QDET NS))
            QNUM
            NIL
            INCOM)

    QPRON
        (: (PARSE PRON QPRON) PRON3 FAIL)

    RELWD
        (: (AND (PARSE PRONREL)
            (CALLSM (SMSET (SM (MOVE-PT C U U (NG))))))             ;; SET SM TO THE NOUNGROUP DIRECTLY UPSTAIRS
            RETURN
            NIL)

    POPRET
        (POP)

    ;; -------------------------------------------------
    ;; RETURN AFTER CALLING SMNG2 TO PROCESS THE COMPLETED NOUN GROUP
    ;; -------------------------------------------------

    RETSM
        (OR (CALLSM (SMNG2)) (GO TRYA))
        (GO RETURN)

    ;; -------------- YOU PROBABLY GOOFED, CUT AND TRY AGAIN. --------------

    TRYA
        (: (ISQ H NOUN) NIL (TRYA))
        (POP)
        (CUT N)

    UP
        (: (POP) UP NIL)                                            ;; POP EVERYTHING OFF
        (SETQ FE (REVERSE REST))
        (SMSET NIL)
        (GO NGSTART))

(PDEFINE VG (TENSE)

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
        (COND ((NOT (NQ VB)) (MQ VB) (GO FAIL))                     ;; AND JUMP TO CODE THAT KNOWS WHAT SHOULD BE
            ((AND (NQ DO) (PARSE VB AUX DO)) (GO DO))               ;; LOOKED FOR NEXT IN EACH CASE
            ((AND (NQ MODAL) (PARSE VB AUX MODAL)) (GO MODAL))
            ((AND (NQ WILL) (PARSE VB AUX WILL)) (GO WILL))
            ((AND (NQ BE) (PARSE VB AUX BE)) (GO BE))
            ((AND (NQ HAVE) (PARSE VB AUX HAVE)) (GO HAVE))
            ((NOT (PARSE VB (MVB))) (MQ VB) (GO FAIL)))

    SIMPLE
        (MOVE-PT C DLC)                                             ;; MOVE PT DOWN FROM THE CURRENT NODE BEING PARSED
        (TRNSF VPL INF V3PS)                                        ;; (VG) AND ACROSS TO THE MOST RECENTLY PARSED
        (SETQ TENSE (COND ((AND (ISQ PT PRESENT) (ISQ PT PAST))     ;; DAUGHTER. IN THIS CASE THAT DAUGHTER WAS PARSED
                    '(PAST-PRESENT))                                ;; IN THE DISPATCH TABLE JUST ABOVE
                ((ISQ PT PAST) '(PAST))
                (T '(PRESENT))))
        (GO REV)

    TO
        (FQ NAGR)                                                   ;; "NAGR" MARKS THAT SUBJECT AND MAIN VERB NEED
        (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))            ;; NOT AGREE IN NUMBER AND PERSON AND INSURES THAT
        (: (OR (PARSE NIL TO) (CQ TODEL)) NIL (TO) (TO))            ;; THE AGREEMENT CHECKER AT THE END OF THE PROGRAM
                                                                    ;; ("REV") WILL NOT BE APPLIED "TODEL" MUST BE
        (SETQ TENSE '(INFINITIVE))                                  ;; GIVEN AS AN INITIAL FEATURE OR ELSE THIS
        (GO MODAL2)                                                 ;; STATEMENT FAILS TENSE IS USED TO HOLD THE TENSE
                                                                    ;; WHILE IT IS BEING COLLECTED.

    EN
        (FQ NAGR)
        (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))
        (SETQ TENSE '(PAST))
        (: (AND (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)) RETSM FAIL) ;; DONE AT "EN2"

    ING
        (FQ NAGR)
        (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))

    INGADV
        (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) INGADV NIL)
        (SETQ TENSE '(PRESENT))
        (GO BE2)

    IMPER
        (: (AND (PARSE VB DO NEG INF) (FQ NEG)) NIL NIL (DONT))
        (: (AND (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG)))
            RETURN
            (IMPER))                                                ;; MVB IS BOUND BY CLAUSE

    POLR2                                                           ;; THE CLAUSE COULD ONLY BE MARKED AS "POLR2"
        (OR (SETQ PT (GETR 'QAUX (MOVE-PT C U)))                    ;; ("DID THE...?") IF AN AUX OF SOME VERIETY HAD
            (AND (BUG VG:POLR2) (GO FAIL)))                         ;; ALREADY BEEN PARSED, IF THAT IS NOT THE CASE,
        (SETQ H (LIST (CAR PT)))                                    ;; THEN WE HAVE A BUG IN THE PROGRAM SOMEWHERE SET
        (TRNSF NEG)                                                 ;; THE INITIAL DAUGHTER OF THE VG TO BE THE
        (COND ((ISQ H DO) (GO DO))                                  ;; PREVIOUSLY PARSED AUX MARK THE VG AS NEG IF
            ((ISQ H MODAL) (GO MODAL))                              ;; APPROPRIATE (SEE PROGMR FILE FOR THE OPPERATION
            ((ISQ H WILL) (GO WILL))                                ;; OF THIS FUNCTION) DISPATCH TABLE , CHECKING THE
            ((ISQ H BE) (GO BE))                                    ;; AUX
            ((ISQ H HAVE) (GO HAVE)))
        (ERT BUG VG:POLR2VB)                                        ;; NOTHING BUT UNGRAMATICAL NONSENSE SHOULD REACH
        (GO FAIL)                                                   ;; THIS POINT

    ;; ###############################################################
    ;; PROCESSING OF VB'S NOT SPECIALLY MARKED FOR BY INITIAL FEATURES
    ;; ###############################################################

    DO  (FQ DO)
        (MOVE-PT C DLC)                                             ;; MOVE TO THE "DO"
        (TRNSF VPL NEG INF V3PS)                                    ;; ARRANGE ITS FEATURES
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (T '(PRESENT))))
        (GOCOND DO2 MVB)                                            ;; GO CONDITIONALY TO THE FIRST TAG IF MORE WORDS
                                                                    ;; REMAIN BEFORE THE CUT POINT, AND TO THE SECOND
                                                                    ;; TAG IF THERE ARE NONE

    DO2 (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))

    ADV2
        (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV2 NIL (ADV))
        (: (PARSE VB (MVB) INF) NIL MVB)                            ;; "MVB" ARRANGES FOR A CHECK TO INSURE THAT THE
        (GO REV)                                                    ;; VERB BEING PARSED CAN BE A MAIN VERB

    MODAL
        (FQ NAGR MODAL)
        (SETQ TENSE '(MODAL))
        (GOCOND MODAL2 INCOMP)

    MODAL2
        (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))

    ADV3
        (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV3 NIL (ADV))
        (COND ((PARSE VB BE INF) (GOCOND BE2 MVB))                  ;; DISPATCH TABLE FOR THE NEXT VERB
            ((PARSE VB HAVE INF) (GOCOND HAV2 MVB))
            ((PARSE VB INF (MVB)) (GO REV))
            (T (GO INCOMP)))

    WILL
        (FQ NAGR)
        (SETQ TENSE '(FUTURE))
        (GOCOND MODAL2 INCOMP)                                      ;; THE SAME POSSIBILITIES FOR THE NEXT VERB APPLY
                                                                    ;; AFTER BOTH WILL AND MODALS

    BE
        (MOVE-PT C DLC)                                             ;; POINT TO WHAT WAS JUST PARSED
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (T '(PRESENT))))
        (GOCOND BE2 MVB)

    BE2
        (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))

    ADV4
        (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV4 NIL (ADV))
        (COND ((AND (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))      ;; "...WILL BE GOING TO..."
            ((AND (NQ BE) (PARSE VB ING))                           ;; "BE BEING"
                (SETQ TENSE (CONS 'PRESENT TENSE))
                (GO EN2))                                           ;; AS IN "BE BEING X'EN(ED)"
            ((AND (NQ ING) (PARSE VB ING (MVB)))                    ;; "BE X'ING"
                (SETQ TENSE (CONS 'PRESENT TENSE))
                (GO REV))
            ((CQ ING) (MQ ING) (GO FAIL)))                          ;; IF TRUE, IT IMPLYS THAT WE STARTED OFF WITH
                                                                    ;; "BEING" - AS IN "BEING EATEN CAN BE UNPLEASANT"
                                                                    ;; - OTHERWISE IT IMPLYS THAT WE HAVE SOMETHING
                                                                    ;; OTHER THAN A VG ON OUR HANDS AND SHOULD FAIL TO
                                                                    ;; WHOEVER CALLED US AND TRY TO PARSE IT
                                                                    ;; DIFFERENTLY

    EN2
        (: (PARSE VB EN (MVB)) NIL MVBE)                            ;; THIS ASKS -DO WE HAVE A VERB IN ITS EN FORM
                                                                    ;; WHICH CAN ACT AS A MAIN VERB (IN WHICH CASE IT
        (FQ PASV)                                                   ;; IS MARKED AS PASSIVE AND WE RETURN)OTHERWISE
        (GO REV)                                                    ;; CHECK IF THE VERB BEING POINTED AT IS A
                                                                    ;; LEGITIMATE FORM OF "BE" IN ITS MAIN VERB SENSE
                                                                    ;; - WHICH IS DONE AT "MVBE"

    GOING
        (: (PARSE NIL TO) NIL GOI)
        (: (NQ INF) GOING2 NIL NIL)
        (POP)

    GOI
        (SETQ TENSE (CONS 'PRESENT TENSE))                          ;; WE HAVE DETERMINED THAT "GOING" IS THE ACTUAL
        (GO MVB)                                                    ;; MAIN VERB AND SHOULD BE PARSED AS SUCH

    GOING2
        (SETQ TENSE (CONS 'FUTURE TENSE))                           ;; HERE WE DETERMINE THAT THE PHRASE IS ACTUALLY
        (GO MODAL2)                                                 ;; OF THE FORM "...IS GOING TO FALL IN LOVE..."
                                                                    ;; AND WE SHOULD RUN THROUGH THE DISPATCH TABLE AT
                                                                    ;; "MODAL2" TO DETERMINE HOW TO CONTINUE

    MVBE
        (: (ISQ (MOVE-PT H PV (VB)) AUX) NIL MVB)                   ;; MOVE TO EARLIER AND EARLIER DAUGHTERS  UNTILL
        (: (ISQ PT BE) NIL (MVBE))                                  ;; YOU REACH A VERB WHICH IS A "QAUX" - IF THERE
                                                                    ;; ARE NONE THEN CONTINUE AT "MVB" IF WHAT YOU ARE
                                                                    ;; POINTING TO (THE "QAUX") IS NOT A FORM OF "BE",
        (SETMVB PT)                                                 ;; THEN FAIL BECAUSE OF THE UNGRAMATICALITY OF THE
        (GO REV)                                                    ;; CONSTRUCTION OF "BE"'S OTHERWISE MARK IT AS THE
                                                                    ;; MVB AND PREPARE TO RETURN

    HAVE
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST)) (T '(PRESENT))))
        (GOCOND HAV2 MVB)                                           ;; HAV2 WILL CATCH "HAVE HAD", OR "HAVE BEEN ..."

    HAV2
        (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))            ;; OR "HAVE KISSED"

    ADV5
        (: (PARSE ADV) ADV5 NIL (ADV))
        (: (PARSE VB BE EN) NIL HAV3)
        (SETQ TENSE (CONS 'PAST TENSE))                             ;; "HAVE BEEN ..."
        (GOCOND BE2 MVB)

    HAV3
        (: (PARSE VB (MVB) EN) NIL MVB)
        (SETQ TENSE (CONS 'PAST TENSE))                             ;; "HAVE KISSED"
        (GO REV)

    INCOMP
        (FQ INCOMP)
        (GO FAIL)

    MVB
        (: (EQ (FE MVB) (FE H)) MVB2 NIL)
        (POP VB)                                                    ;; POP OFF EVERY THING UNTILL YOU REACH A VERB
        (: (PARSE VB (MVB)) NIL (MVB))

    MVB2
        (GO REV)

    ;; -------------------------------------------------
    ;;   CHECK AGREEMENT BETWEEN SUBJECT AND MAIN VERB
    ;; -------------------------------------------------

    REV
        (SETR 'TENSE TENSE C)
        (AND NN (PARSE NIL NOT) (FQ NEG))
        (COND ((OR (EQUAL TENSE '(PAST))
                (CQ NAGR)
                (ISQ (MOVE-PT C U) IMPER)                           ;; MOVE PT TO THE CLAUSE REMEMBER THAT THE POINTER
                (ISQ PT THERE)                                      ;; STAYS WHERE IT'S PUT UNTILL RETURNING FROM A
                (ISQ PT RSNG))                                      ;; CALL TO PARSE
            (GO NAUX))
            ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))               ;; "SUBJECT" IS THE SYNTACTIC SUBJECT OF THE
            (T (ERTERR VG -- NO SUBJECT TO CHECK FOR AGREEMENT)))   ;; CLAUSE THAT THE VG IS IN, WHOSE ESSENTIAL
                                                                    ;; DISTINGUISHING FEATURE IS AGREEMENT WITH THE VERB

        (SETQ T3 NIL)                                               ;; T3 WILL ACT AS A SWITCH AT "NAGR" BELOW. NOTE
                                                                    ;; THAT IT IS EXPLICITLY SET BY THE CODE BELOW BY
                                                                    ;; THE FOLLOWING CRITERIA;   IF T3 IS NON-NIL THEN
                                                                    ;; SUBJECT AND VERB HAVE BEEN DETERMINED TO AGREE
                                                                    ;; IF IT IS NIL THEN THEY WILL BE CONSIDERED TO
                                                                    ;; AGREE ONLY IF THE FEATURE "PAST-PRESENT" IS ON
        (COND ((ISQ PT NFS)                                         ;; THE MVB, IN WHICH CASE, THIS IS EVIDENCE THAT
                (OR (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))      ;; THE PROPER CHOISE OF TENSE IS PAST - WHERE
            ((ISQ PT CLAUSE) (OR (SETQ T3 (CQ V3PS)) (GO NAGR)))    ;; AGREEMENT IS IRRELEVANT (SEE BELOW AT "NAGR")
            ((OR (ISQ PT NS) (ISQ PT MASS))
                (OR (AND (CQ V3PS) (SETQ T3 T))
                    (FESET PT (SETDIF (FE PT) '(NS MASS))))))
        (COND ((OR (ISQ PT PART) (ISQ PT NPL))
            (OR (AND (MEET FE '(INF VPL)) (SETQ T3 T))
                (FESET PT (SETDIF (FE PT) '(PART NPL))))))

    NAGR
        (: (OR T3
            (AND (EQUAL '(PAST-PRESENT) TENSE)                      ;; NOTES WHETHER VERB AND SUBJECT WERE FOUND TO
                (SETQ TENSE '(PAST))))                              ;; AGREE AND FAILS UNLESS A SPECIAL CONDITION
            NIL                                                     ;; EXISTS AS NOTED DIRECTLY ABOVE
            (NAGR))

    NAUX
        (SETMVB (OR (MOVE-PT H PV (MVB)) MVB))
        (: (AND (CQ NAUX) (ISQ (MOVE-PT H PV (VB)) AUX) (NOT (MOVE-PT PV PV (VB))))
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
        (: (CALLSM (SMVG)) RETURN FAIL))

(PDEFINE PREPG NIL

    ENTERING-PREPG

    ADV
        (: (AND (NQ PREPADV) (PARSE ADV PREPADV)) ADV NIL (PREPADV)) ;; CHECK FOR ANY INITIAL MODIFING ADVERBS
        (: (COND ((CQ AGENT) (NEXTWORD? 'BY))                       ;; EXAMINE THE INITIAL FEATURES OF THE PREPG TO
            ((CQ LOC) (NQ PLACE))                                   ;; CHECK FOR CONSTRAINTS ON THE PREPOSITION
            ((CQ Q) (NOT (NQ MOTOR)))
            (T))
            NIL
            (PREP))                                                 ;; FAIL IF THE CONSTRAINTS AREN'T MET

        ;; PARSE THE PREPOSITION

        (: (PARSE PREP) NIL (PREP))
        (MOVE-PT H)
        (TRNSF PLACE TIME)                                          ;; THIS IS NOT EXACTLY RIGHT, SINCE "ON WHAT DAY" IS NOT "PLACE"

        ;; AT THIS POINT THE POSSIBILITIES ARE:
        ;;   1. THERE ARE NO MORE WORDS AND THE PREP IS "SHORT"
        ;;   2. YOU HAVE A MULTIPLE WORD PREPOSITION
        ;;   3. IT IS INDEED A SINGLE WORD PREP, PARSE ITS OBJECT

        (SETQ T1 H)                                                 ;; SAVE THE PREPOSITION JUST PARSED IN CASE IT IS
        (AND (NQ PREP2)                                             ;; ONLY THE FIRST WORD OF A MULTIPLE WORD PREPOSITION
            (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N)))
                    (PARSE PREP2))
                ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N) (WORD (CDR N))))
                    (PARSE PREP2)
                    (PARSE PREP2)))
            (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))        ;; CREATE NODE FOR THE COMPOUND WORD
            (SETR 'PARENT C T1))
        (: (ISQ H NEED2) (NEED2) NIL)                               ;; FAIL IF LAST PARSED NEEDS ANOTHER WORD

                                                                    ;; GIVE IT A PARENT
        (SETR 'HEAD T1 C)                                           ;; SET THE REGESTER "PREP" TO THE CONSTITUENT JUST
                                                                    ;; PARSED - IF IT WAS A MULTIPLE-WORD-PREP THEN
        (OR NN (GO SHORT))                                          ;; "PREP" IS SET TO THE NODE WHICH CONTAINS THE
                                                                    ;; ENTIRE FORM NN POINTS TO WHATEVER WORDS ARE
                                                                    ;; LEFT BEFORE THE CUT POINT

        ;; ADD FEATURES TO THE PREPG DEPENDING ON THE PREPOSITION PARSED

        (COND ((EQ (WORD H) 'BY) (FQ AGENT)))

    ;; ###################################
    ;; PARSE THE OBJECT TO THE PREPOSITION
    ;; ###################################

    QUEST
        (: (CQ QUEST) NIL NG)
                                                                    ;; CERTAIN RESTRICTIONS PLACED ON THE POSSIBLE
                                                                    ;; NOUN GROUPS THAT IT CAN TAKE - HENSE THE
                                                                    ;; SPECIAL CALL TO PARSE AS ABOVE, IF THE PREPG IS
        (: (PARSE NG QUEST OBJ) OBJR (PREPQUEST))                   ;; MARKED WITH THE FEATURE "QUEST" (INDICATING
        (: (AND (CQ OF) (PARSE NG OFOBJ)) OBJR NIL)                 ;; THAT IT SHOULD CONTAIN THE QUESTION ELEMENT OF

    NG
        (: (PARSE NG OBJ) OBJR NIL)                                 ;; THE CLAUSE) THEN WE PARSE IT SPECIALLY SIMPLE

    REL
        (: (NEXTWORD? 'WHICH) NIL REST)                             ;; NOUN GROUP - NO RESTRICTIONS
        (: (ISQ (MOVE-PT U) CLAUSE) NIL (PREP-WHICH))               ;; IF THE NEXT WORD IS A RELWORD, SUCH AS "WHICH"
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
        (: (ISQ PT PRONREL) NIL PRONREL)                            ;; CHANGES ITS REQUEST FROM (PARSE PREPG Q)  TO
        (SETQ MES (CDR MES))                                        ;; (PARSE CLAUSE RSQ PREPREL), WHICH SHOULD PICK
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
        (: (PARSE CLAUSE RSNG ING) OBJR SHORT)                      ;; SEMANTIC SPECIALIST "HEAD" IS HERE THE HEAD OF

    OBJR
        (SETR 'OBJ1 H C)                                            ;; THE HIGHER NOUNGROUP
        (GO RETT)

    SHORT
        (: (MEET FE '(NOSHORT Q)) (SHORT) NIL)
        (OR (ISQ (MOVE-PT C U) REL-NOT-FOUND)
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
        (AND (OR (ISQ H QUEST)                                      ;; H IS THE NG FOUND FOR AN OBJECT
            (AND (ISQ H COMPOUND)                                   ;; IF THE NOUN GROUP IS COUMPOUND, CHECK EACH
                (MOVE-PT H H PV (QUEST))))                          ;; COMPONENT FOR THE FEATURE "QUEST"
            (FQ QUEST))
        (: (CALLSM (SMADJG-PREPG)) RETURN FAIL))

(PDEFINE ADJG NIL

    ENTERING-ADJG                                                   ;; THIS LABEL IS MARKED BY DEBUGGING ROUTINES AND

    COMPCHECK                                                       ;; IS USEFUL FOR FOLLOWING THE FLOW OF CONTROL
        (: (AND (MOVE-PT C U (BE)) (NOT (CQ COMP))) FAIL NIL)       ;; CONDITIONS WHICH MUST BE MET BY ANY ADJECTIVE
                                                                    ;; GROUP IF THERE IS A FORM OF "BE" IN THE HIGHER
                                                                    ;; CLAUSE, THEN THE ADJG SHOULD HAVE BEEN CALLED
                                                                    ;; WITH THE FEATURE "COMP" FOR COMPLIMENT

        ;; EXAMINE THE INITIAL FEATURES (THOSE DESIGNATED BY THE
        ;; CALLING PROGRAM) ALSO EXAMINE THE NEXT WORD - THESE GIVE
        ;; CLUES AND CONSTRAINTS TO THE STRUCTURE TRYING TO BE PARSED
        ;; AND DIRECT JUMPS TO THE APPROPRIATE SECTIONS OF CODE

        (: (ISQ (MOVE-PT C U) THAN) NIL DISP)                       ;; THE WORD "THAN" WAS DETECTED BY THE IMMEDIATELY
                                                                    ;; UPSTAIRS NG AS FOLLOWING THE HEAD NOUN
        (SETR 'HEAD (GETR 'COMPARATIVE-MODIFIER PT) C)              ;; INDICATING A STURCTURE SUCH AS "... A BIGGER
        (GO THAN)                                                   ;; BLOCK THAN THAT ONE ..." "HEAD REFERS TO THE
                                                                    ;; ADJG'S HEAD ADJECTIVE

    DISP
        (: (AND (NQ AS) (PARSE NIL AS)) AS NIL (AS))
        (: (AND (NQ AS) (PARSE NIL AS)) AS NIL (AS))
        (: (NEXTWORD? 'HOW) HOW ADV)

    ;; ---------------- HOW + ADJG --------------

    HOW
        (: (AND (PARSE NIL HOW) (FQ QUEST)) NIL FAIL FAIL)
        (: (AND (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C))
            RETSM
            NIL)
        (: (AND (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C))
            RETSM
            FAIL)

    ADV
        (: (PARSE ADV ADVADV) ADV NIL POPAD)                        ;; THIS LOOPS UNTILL ALL CONTIG- UOUS ADVERBS HAVE
        (: (PARSE NIL MORE) NIL ADJ)                                ;; BEEN PARSED "MORE" IS EXPLICITLY CHECKED FOR
        (FQ COMPAR)                                                 ;; SINCE IT SIGNALS THE FEATURE, COMPARATIVE

    ADJ
        (: (COND ((CQ ADV) (PARSE ADV VBAD)) (T (PARSE ADJ)))
            NIL
            (ADJ))                                                  ;; IF THE CUT POINT WAS REACHED THEN NO MORE
        (: (SETR 'HEAD H C) NIL NIL RETSM)                          ;; PROCESSING (SUCH AS COMPAR BELOW) IS POSSIBLE.

    ;; --------------------------------------------
    ;;                 COMPARATIVES
    ;; --------------------------------------------

    ;; IF THE FEATURE "COMPAR" IS ALREADY ON THE LIST, OR IF THE JUST PARSED ADJECTIVE CAN HAVE THAT FEATURE, THEN
    ;; ATTEMPT TO PARSE SOME SORT OF COMPARATIVE CONSTRUCTION (ASSUMING THAT THEREARE ANY MORE WORDS BEFORE THE CUT POINT.)

        (: (OR (CQ COMPAR) (ISQ H COMPAR)) NIL RETSM)
        (FQ COMPAR)
        (: NN NIL RETSM)                                            ;; IF THERE ARE UNPARSED WORDS LEFT BEFORE THE CUT
                                                                    ;; POINT THEN THE POSSIBILITY OF MORE COMPLICATED
                                                                    ;; FORMS IS CHECKED FOR

    THAN
        (COND ((NOT NN) (GO RETSM)))
        (: (PARSE NIL THAN) NIL RETSM (THAN))
        (RQ THANNEED)                                               ;; THE FEATURE "THANNEEED" MARKS THAT THE WORD
        (FQ THAN)                                                   ;; "THAN" IS EXPLICITLY  REQUIRED IN THE PHRASE.
        (GO SUBJ)

    AS
        (FQ AS)
        (RQ THANNEED)
        (: (AND (PARSE ADJ) (SETR 'HEAD H C)) NIL (ADJ) RETSM)
        (: (PARSE NIL AS) SUBJ RETSM (AS))

    ;; FIND A SUBJECT FOR THE COMPARATIVE
    ;; IE.  "AS BIG AS ..." , "BIGGER THAN ..."

    SUBJ
        (: (PARSE NG SUBJ COMPAR) NIL (THAN))
        (: (SETR 'OBJ1 H C) NIL NIL RETSM)
        (: (AND (ONE-WORD-LEFT) (PARSE VB AUX)) NIL RETSM)
        (: (CHECK-AGREEMENT H (CDR H))                              ;; CHECKS FOR AGREEMENT IN NUMBER AND PERSON
            RETSM                                                   ;; BETWEEN THE NG PARSED AS SUBJ AND THE
            NIL)                                                    ;; JUST-PARSED VERB
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
        (: (CQ THANNEED) (THANNEED) NIL)                            ;; IF ONE OF THE WORDS PARSED REQUIRED A "THAN",
        (: (CALLSM (SMADJG-PREPG)) RETURN (SMADJ)))                 ;; FAIL IF ONE WAS NOT FOUND.

(DEFUN CONJ NIL
    (PROG (END GOODIE)
        (SETQ END CUT)
        (COND ((SETQ GOODIE (APPLY-GRAMMAR 'CONJOIN))
            (RETURN (SETQ RE GOODIE)))
            (T (RETURN NIL)))))

(DEFUN COMMA NIL
    (COND ((SECONDWORD? '\") (FLUSHME) T)                            ;; IF " FOLLOWS, FLUSH COMMA AND CONTINUE
        ((CONJ))                                                    ;; IF COMMA IS PART OF CONJOINED STRUCTURE, GREAT
        ((ISQ RE INIT) (FLUSHME) T)))                               ;; IF COMMA FOLLOWS INITIAL-TYPE PHRASE, FLUSH IT AND CONTINUE DIRECT ADDRESS JAZZ

(PDEFINE CONJOIN (PREV)

    ;; THIS PROGRAM IS CALLED TO PARSE A CONJOINED STRUCTURE THE
    ;; FIRST MEMBER OF THE SUPPOSED STRUCTURE HAS ALREADY BEEN
    ;; PARSED AND IS SET TO THE INITIAL DAUGHTER (H) OF THIS NODE
    ;; AN ATTEMPT IS MADE TO PARSE AS MANY CONSTITUENTS JUST LIKE
    ;; IT AS IS POSSIBLE

    ENTERING-CONJOIN                                                ;;  HACK LABEL FOR LABELTRACER

    UP
        (SETQ PREV (NEXTWORD))
        (FLUSHME)
        (COND ((AND (EQ PREV '\,)                                   ;; IF WE HAVE A COMMA AND
                (OR (CDR H)                                         ;; IF MORE THAN 1 COMPONENT HAS BEEN PARSED
                (GREATERP (DIFFERENCE (LENGTH (NB H))               ;; OR IF THAT ONE COMPONENT
                            (LENGTH (N H)))                         ;; IS MORE THAN 4 WORDS LONG
                    4))
                (MEMQ (NEXTWORD) '(OR AND NOR BUT))
                (F (NEXTWORD)))                                     ;; THEN CHECK FOR COMMA COMBINATION
            (SETQ PREV (LIST PREV (NEXTWORD)))
            (FLUSHME)))
        (AND (ATOM PREV)
            (MOVE-PTW N NW (EQ (WORD PTW) PREV))
            (CUT PTW))
        (AND (OR (EQ PREV 'BUT) (EQ (CADR PREV) 'BUT))
            (NEXTWORD? 'NOT)                                        ;; CHECK FOR BUT-NOT COMBINATION
            (OR (FLUSHME) (GO LOSE2))
            (FQ NEGBUT))
        (: (COND ((MEMQ (CAR REST)
                '(ADJ NUM NOUN PREP VB ADV))
            (PARSE3 (APPEND REST '(COMPONENT)) NIL))
            ((MEMQ (CAR REST) '(NG PREPG ADJG))
            (AND (NOT (CQ OFOBJ))
                (PARSE2 (APPEND REST '(COMPONENT))
                    NIL)))
            ((EQ (CAR REST) 'CLAUSE)
            ((LAMBDA (LASTSENT AUXFE)
                    (AND (PARSE2 (APPEND REST
                            AUXFE
                            '(COMPONENT))
                        NIL)
                    (OR (NOT AUXFE) (F (CAR AUXFE)))
                    (SETR 'TIME
                        (GETR 'TIME H)
                        C)))                                        ;; MARK COMPOUND CLAUSE AS TO DECLAR/IMPER FOR ANSGEN
                (COND ((ISQ H MAJOR) H) (LASTSENT))
                (MEET (FE H) '(DECLAR IMPER)))))
            NIL
            LOSE2)
        (CUT END)                                                   ;; RESTORE CUT POINT
        (COND ((NOT (ATOM PREV))
                ;; IF WE HAD COMMA FOLLOWED BY (AND OR BUT NOR), RETURN THE LIST OF GOODIES WE'VE FOUND
                (GO RETSM))
            ((EQ PREV '\,)
                (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP))
                    (T (GO LIST))))
            ((MEMQ PREV '(AND OR NOR BUT))
                (COND ((EQ BOTH (NB H)) (FQ BOTH)))
                (COND ((OR (NEXTWORD? 'BUT)
                    (AND (NEXTWORD? PREV)
                        (NOT (AND (EQ BOTH (NB H))                  ;; IF WE HAD THE 'BOTH' WORD AND
                            (EQ PREV 'AND)))))                      ;; IF THE 'BOTH' WORD WAS "AND", STOP PARSING
                    (FQ LISTA)                                      ;; ELSE GO LOOK FOR THE NEXT COMPONENT
                    (F PREV)
                    (GO UP))
                    (T (GO LISTA)))))

    LOSE2
        (: (CQ LISTA) LISTA NIL)

    LIST
        (: (AND (EQ PREV '\,)                                       ;; COME HERE FOR ABORTED LIST AND CHECK FOR APPOSITIVE
                (EQUAL (LENGTH H) 2)
                (ISQ H NG)
                (NOT (OR (ISQ H PRONG) (ISQ (CDR H) PRONG)))
                (OR (NEXTWORD? COMMA) (NULL N)))
            NIL
            (CONJOIN: HOPELESS LIST))
        (FLUSHME)                                                   ;; GET RID OF TRAILING COMMA
        (FQ APPOSITIVE)
        (GO RETSM)

    LISTA
        (F PREV)

    RETSM
        (FQ COMPOUND)                                               ;; CALL SEMANTICS AND RETURN EVERY PARSED BY THIS
        (AND (GREATERP (LENGTH H) 2) (FQ LIST))                     ;; GOODIE IS COMPOUND IF MORE THAN 2 COMPONENTS
        (COND ((OR (CQ NG) (CQ NOUN))
            (COND ((CQ AND) (FQ NPL))
                (T (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
            ((CQ VB)
            (PROG (COMMON)
                (SETQ COMMON (GET 'VB 'ELIM))
                (MAP #'(LAMBDA (X)
                        (SETQ COMMON (MEET COMMON (FE X))))
                H))
            (FESET (UNION COMMON (FE C)) C)))
        (: (CALLSM (SMCONJ)) RETURN (CONJOIN: SMCONJ)))             ;; THEN MARK AS A LIST

(DEFUN BOTH FEXPR (A)
    ;; HANDLES (BOTH AND) (EITHER OR) (NEITHER NOR) COMBINATIONS
    ;; THE CONJOIN PROGRAM DOES SPECIAL THINGS WHEN BOTH IS SET
    (PROG (END)
        (SETQ END CUT)                                              ;; MAKE END OUT OF PREVIOUS CUT POINT
        (RETURN (PROG (CUT NBB BOTH)
            (SETQ NBB N)
            (AND (FLUSHME)
                (MOVE-PTW N
                        NW
                        (EQ (WORD PTW) (CAR A))
                        NW)                                         ;; LOOK FOR THE MATCHING WORD E.G. AND, OR, NOR
                (CUT END)
                (SETQ BOTH PTW)                                     ;; SAVE POINTER TO THE WORD AFTER THE MATCHING WORD
                (SETQ RE (COND
                    ((MEMQ (CAR REST) '(PREP ADV)) (PARSE3 REST T))
                    ((MEMQ (CAR REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST T))))
                (LESSP (LENGTH N) (LENGTH BOTH))                    ;; FAIL UNLESS WE PARSED BEYOND MATCHING WORD
                (RETURN (SETQ SPECIAL 'SKIP)))
            (SETQ RE NIL)
            (SETQ N NBB)
            (RETURN NIL)))))

(DEFUN DOUBLEQUOTER NIL (APPLY-GRAMMAR 'PARSEQUOTED))

(DEFUN CANTAKE (NUM TYPE FEATURE)
    (PROG (VBFEAT)
        (SETQ VBFEAT (FE MVB))
        (RETURN (COND
            ((MEMQ 'RSNG TYPE)
                (MEMQ
                    (READLIST (APPEND
                        (COND ((MEMQ 'TO TYPE) '(T O)) ((MEMQ 'ING TYPE) '(I N G)) ((MEMQ 'REPORT TYPE) '(R E P)))
                        '(O B)
                        (LIST (COND ((EQ NUM 1) '\1) (T '\2)))))
                    VBFEAT))
            ((MEMQ 'COMP TYPE)
                (MEMQ 'INT VBFEAT))
            ((MEMQ 'NG TYPE)
                (COND ((EQUAL NUM 1)
                    (NOT (NULL (MEET '(TRANS TRANS2 TRANSL TRANSINT) VBFEAT))))
                (T (MEMQ 'TRANS2 VBFEAT))))
            (T (MEMQ FEATURE VBFEAT))))))

(DEFUN CANPARSE (NUM TYPE FEATURE)
    (PROG (REG)
        (AND (CANTAKE NUM TYPE FEATURE)
            (OR (NULL TYPE)
                (AND (APPLY 'PARSE
                    (APPEND TYPE
                        (COND ((MEMQ 'COMP TYPE) (SETQ REG 'COMP) NIL)
                            (T (LIST 'OBJ
                                (SETQ REG (COND
                                    ((OR (MEMQ 'LOC TYPE) (MEMQ 'PLACE TYPE)) 'LOBJ)
                                    ((EQUAL NUM 1) 'OBJ1)
                                    (T 'OBJ2))))))))
                    (SETR REG H C)))
            (OR (NULL FEATURE) (F FEATURE))
            (RETURN T))))

#_(ns shrdlu.dictio)

;; ###########################################################
;;
;;                          WORDS
;;
;; ###########################################################

(DEFS \, FEATURES (SPECIAL) SPECIAL (COMMA))

(DEFS \" FEATURES (B-SPECIAL RELWRD) B-SPECIAL (DOUBLEQUOTER)) ;; "sic!

(DEFS A SEMANTICS ((DET T)) FEATURES (DET NS INDEF))

(DEFS ABOVE SEMANTICS ((PREP (#LOC #ABOVE T))) FEATURES (PREP PLACE))

(DEFS AFTER SEMANTICS ((BINDER (SMBINDER END NIL))) FEATURES (BINDER TIME))

(DEFS ALL
    SEMANTICS ((DET (COND ((CQ OF) 'ALL) ((MEET '(NUM DEF) FE) 'DEF) ('NDET))))
    FEATURES (DET NPL QNTFR))

(DEFS AN IRREGULAR (A NIL NIL))

(DEFS AND FEATURES (SPECIAL) SEMANTICS T SPECIAL (CONJ))

(DEFS ANY SEMANTICS ((DET 'INDEF)) FEATURES (DET ANY NS NPL QNTFR))

(DEFS ANYTHING SEMANTICS ((TPRON 'INDEF)) FEATURES (TPRON ANY NS))

(DEFS ARE IRREGULAR (BE (VPL PRESENT) (INF)))

(DEFS AS SEMANTICS ((NULL T)) FEATURES (AS))

(DEFS ASK
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#EVENT))) MARKERS: (#EVENT) PROCEDURE: ((#WANT #1 #2 *TIME))))))))
    FEATURES (VB TRANS INF SUBTOB))

(DEFS AT SEMANTICS ((NUMD T)) FEATURES (AT))

(DEFS AWAY SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS BACK SEMANTICS ((PREP2 T) (NOUN T)) FEATURES (NOUN NS PREP2))

(DEFS BALL
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#MANIP #ROUND) PROCEDURE: ((#IS *** #BALL))))))
    FEATURES (NOUN NS))

(DEFS BE
    FEATURES (INT AUX VB BE INF)
    SEMANTICS ((VB ((THERE (#BETHERE)) (INT (#BEINT))))))

(DEFUN #BETHERE NIL
    (RELATION (RESTRICTIONS: (((#THING) (EQ (QUANTIFIER? SMSUB) 'INDEF))) PROCEDURE: NIL)))

(DEFUN #BEINT NIL
    (COND
        ((RELATION
            (RESTRICTIONS: (((#PHYSOB)) (SMCOMP (#PROPERTY)))
                PROCEDURE: (#EVAL (PROG (PROPERTY)
                                    (COND
                                        ((SETQ PROPERTY (MEET (GET '#PROPERTY 'SYSTEM) (MARKERS? SMCOMP)))
                                            (RETURN (LIST (LIST (CAR PROPERTY) '#1 '#2))))
                                    ((RETURN (LIST '(#2 #1))))))))
            (RESTRICTIONS: (((#THING)) (SMCOMP (#SYSTEMS) (AND (NOT (REFER? SMCOMP)) (EQ (REL? SMCOMP) SMSUB))))
                PROCEDURE: (#EVAL (RELATIONS? SMCOMP)))
            (RESTRICTIONS: (((#THING)) (SMCOMP (#THING) (REFER? SMCOMP)))
                PROCEDURE: ((#EVAL (LIST 'THAMONG '#1 (LIST 'QUOTE (REFER? #2))))))))
        (T (ERTSTOP SORRY I DON 'T UNDERSTAND THE VERB BE WHEN YOU USE IT LIKE THAT))))

(DEFS BEFORE SEMANTICS ((BINDER (SMBINDER NIL START))) FEATURES (BINDER TIME))

(DEFS BEGIN
    SEMANTICS ((VB
        ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#EVENT))) MARKERS: (#EVENT) PROCEDURE: ((#START #2 *TIME)))))
         (ITRNS (RELATION (RESTRICTIONS: (((#ANIMATE))) MARKERS: (#EVENT) PROCEDURE: ((#START EE *TIME))))))))
    FEATURES (VB TRANS INF TOOB INGOB ITRNS))

(DEFS BEGAN IRREGULAR (BEGIN (PAST) (INF)))

(DEFS BEHIND
    SEMANTICS ((PREP (#LOC #BEHIND T)))
    FEATURES (PREP PLACE))

(DEFS BELOW
    SEMANTICS ((PREP (#LOC #ABOVE NIL)))
    FEATURES (PREP PLACE))

(DEFS BENEATH
    SEMANTICS ((PREP (#LOC #ABOVE NIL)))
    FEATURES (PREP PLACE))

(DEFS BESIDE
    SEMANTICS ((PREP (RELATION (RESTRICTIONS: (((#PHYSOB)) ((#PHYSOB))) PROCEDURE: ((#NEXTO #1 #2 *TIME))))))
    FEATURES (PREP PLACE))

(DEFS BIG
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #SIZE RESTRICTIONS: (#PHYSOB) DIRECTION: T))
           (ADJ (OBJECT (MARKERS: (#PHYSOB #BIG) PROCEDURE: ((#MORE #SIZE *** (128 128 128)))))))
    FEATURES (ADJ))

(DEFS BLACK SEMANTICS ((ADJ (#COLOR #BLACK))) FEATURES (ADJ))

(DEFS BLOCK
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#MANIP #RECTANGULAR) PROCEDURE: ((#IS *** #BLOCK))))))
    FEATURES (NOUN NS))

(DEFS BLUE SEMANTICS ((ADJ (#COLOR #BLUE))) FEATURES (ADJ))

(DEFS BOTH
    SEMANTICS ((DET 'DEF))
    FEATURES (B-SPECIAL QNTFR DET DEF NPL BOTH)
    B-SPECIAL (BOTH AND)
    FEXPR (LAMBDA (A)
        (PROG (END)
            (SETQ END CUT)
            (RETURN
                (PROG (CUT NBB BOTH)
                    (SETQ NBB N)
                    (AND (FLUSHME)
                        (** N NW (EQ (WORD PTW) (CAR A)) NW)
                        (CUT END)
                        (SETQ BOTH PTW)
                        (SETQ RE
                        (COND ((MEMQ (CAR REST) '(PREP ADV)) (PARSE3 REST T))
                            ((MEMQ (CAR REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST T))))
                        (LESSP (LENGTH N) (LENGTH BOTH))
                        (RETURN (SETQ SPECIAL 'SKIP)))
                    (SETQ RE NIL)
                    (SETQ N NBB)
                    (RETURN NIL))))))

(DEFS BOX
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#BOX) PROCEDURE: ((#IS *** #BOX))))))
    FEATURES (NOUN NS))

(DEFS BRICK FEATURES (NOUN NS))

(DEFS BUILD
    SEMANTICS ((VB ((TRANS (#BUILD)))))
    FEATURES (VB INF TRANS))

(DEFS BUT FEATURES (SPECIAL) SEMANTICS T SPECIAL (CONJ))

(DEFS BY
    SEMANTICS ((PREP (RELATION (RESTRICTIONS: (((#PHYSOB)) ((#PHYSOB))) PROCEDURE: ((#NEXTO #1 #2 *TIME))))))
    FEATURES (PREP))

(DEFS CALL
    SEMANTICS ((VB ((TRANS2 (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#THING)) ((#NAME))) PROCEDURE: ((#CALL #2 #3 *TIME))))))))
    FEATURES (VB INF TRANS2))

(DEFS CAN SEMANTICS ((VB T)) FEATURES (V3PS VFS VPL VB MODAL AUX))

(DEFS CHOOSE
    SEMANTICS ((VB ((TRANS (#NOTICE)))))
    FEATURES (VB INF TRANS))

(DEFS CLEAN SEMANTICS ((VB T)) FEATURES (VB INF VPRT TRANS))

(DEFS CLEAN-OFF
    ROOT (CLEAN OFF)
    SEMANTICS ((TRANS (#CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(DEFS CLEAN-OUT
    ROOT (CLEAN OUT)
    SEMANTICS ((TRANS (#CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(DEFS CLEAN-UP
    ROOT (CLEAN UP)
    SEMANTICS ((TRANS (#CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(DEFS CLEAR SEMANTICS ((VB T)) FEATURES (VB INF VPRT TRANS))

(DEFS CLEAR-OFF
    ROOT (CLEAR OFF)
    SEMANTICS ((TRANS (#CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(DEFS CLEAR-OUT
    ROOT (CLEAR OUT)
    SEMANTICS ((TRANS (#CLEANOFF)))
    FEATURES (COMBINATION TRANS))

(DEFS COLOR
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#COLOR) PROCEDURE: ((#IS *** #COLOR))))))
    FEATURES (NOUN NS))

(DEFS CONSTRUCT
    SEMANTICS ((VB ((TRANS (#BUILD)))))
    FEATURES (VB INF TRANS))

(DEFS CONTAIN
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONS: (((#BOX)) ((#PHYSOB))) PROCEDURE: ((#CONTAIN #1 #2 *TIME)))
                (RESTRICTIONS: (((#CONSTRUCT)) ((#THING))) PROCEDURE: ((#PART #2 #1 *TIME))))))))
    FEATURES (VB INF TRANS))

(DEFS CONTAINER
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#BOX) PROCEDURE: ((#IS *** #BOX))))))
    FEATURES (NOUN NS))

(DEFS CORNER FEATURES (NOUN NS))

(DEFS CUBE
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#MANIP #RECTANGULAR) PROCEDURE: ((#IS *** #BLOCK) (#EQDIM ***))))))
    FEATURES (NOUN NS))

(DEFS DID IRREGULAR (DO (PAST V3PS) (INF PRESENT)))

(DEFS DO
    SEMANTICS ((VB
        ((TRANS (RELATION
            (RESTRICTIONS: RESTRICTIONS:
            PROCEDURE: ((((#ANIMATE)) ((#EVENT))))
            MARKERS: PROCEDURE:
            PLAUSIBILITY: (#EVAL (OR (GET MAP2 'REFER) (ERT DO DEFINITION)))))))))
    FEATURES (TRANS VFS PRESENT VPL VB AUX DO INF))

(DEFS DOES IRREGULAR (DO (V3PS) (VFS VPL INF)))

(DEFS DOWN SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS DROP
    SEMANTICS ((VB
        ((TRANSL (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)) (SMOBL (#PLACE *TIME))) PROCEDURE: ((#DROP #1 #2 #3)) MARKERS: ((#MOTION)))))
         (TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB))) MARKERS: (#EVENT) PROCEDURE: ((#DROP #1 #2 PLACE *TIME)) MARKERS: ((#MOTION))))))))
    FEATURES (TRANSL TRANSL2 VB INF TRANS))

(DEFS EACH SEMANTICS ((DET 'ALL)) FEATURES (DET NS QNTFR))

(DEFS EITHER FEATURES (B-SPECIAL) SEMANTICS T B-SPECIAL (BOTH OR))

(DEFS EVERY SEMANTICS ((DET 'ALL)) FEATURES (DET NS QNTFR))

(DEFS EVERYTHING SEMANTICS ((TPRON 'ALL)) FEATURES (TPRON NS))

(DEFS EXACTLY
    SEMANTICS ((NUMD (LIST 'EXACTLY NUM)))
    FEATURES (NUMD NUMDALONE))

(DEFS FEW
    SEMANTICS ((NUMD (LIST '< (ADD1 NUM))))
    FEATURES (NUMD NUMDAS))

(DEFS FEWER
    SEMANTICS ((NUMD (LIST '< NUM)))
    FEATURES (NUMD NUMDAN))

(DEFS FIND
    SEMANTICS ((VB ((TRANS (#NOTICE)))))
    FEATURES (VB INF TRANS))

(DEFS FINISH
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#EVENT))) MARKERS: (#EVENT) PROCEDURE: ((#END #2 *TIME))))))))
    FEATURES (VB INF TRANS INFOB))

(DEFS FIVE SEMANTICS ((NUM 5)) FEATURES (NUM))

(DEFS FOUR SEMANTICS ((NUM 4)) FEATURES (NUM))

(DEFS FRIEND REFER :FRIEND FEATURES (NOUN NS))

(DEFS FROM FEATURES (PREP))

(DEFS FRONT SEMANTICS ((NOUN T) (PREP2 T)) FEATURES (NOUN NS PREP2))

(DEFS GAVE IRREGULAR (GIVE (PAST) (INF)))

(DEFS GIVE
    SEMANTICS ((VB ((TRANS2 (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#ANIMATE)) ((#PHYSOB))) MARKERS: (#EVENT) PROCEDURE: ((#GIVE #1 #2 #3 *TIME))))))))
    FEATURES (VB INF TRANS2))

(DEFS GO FEATURES (ITRNS VB INF))

(DEFS GOING FEATURES (VB ITRNS ING))

(DEFS GRAB
    SEMANTICS ((VB ((TRANS (#GRASP)))))
    FEATURES (VB TRANS INF))

(DEFS GRASP
    SEMANTICS ((VB ((TRANS (#GRASP)))))
    FEATURES (VB TRANS INF))

(DEFS GREATER
    SEMANTICS ((NUMD (LIST '> NUM)))
    FEATURES (NUMD NUMDAN))

(DEFS GREEN SEMANTICS ((ADJ (#COLOR #GREEN))) FEATURES (ADJ))

(DEFS HAD IRREGULAR (HAVE (PAST) (INF)))

(DEFS HAND
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#HAND) PROCEDURE: ((#IS *** #HAND))))))
    FEATURES (NOUN NS))

(DEFS HANDLE
    SEMANTICS ((VB ((TRANS (#GRASP)))))
    FEATURES (VB INF TRANS))

(DEFS HAS IRREGULAR (HAVE (V3PS PRESENT) (INF)))

(DEFS HAVE
    SEMANTICS ((VB ((TRANS (#HAVE)))))
    FEATURES (HAVE VB AUX INF TRANS))

(DEFS HIGH
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #HEIGHT RESTRICTIONS: (#PHYSOB) DIRECTION: T))
           (ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#HIGH ***))))))
    FEATURES (ADJ))

(DEFS HOLD
    SEMANTICS ((VB
        ((TRANS (RELATION
              (RESTRICTIONS: (((#HAND)) ((#MANIP))) PROCEDURE: ((#GRASPING #2 *TIME)))
              (RESTRICTIONS: (((#ANIMATE)) ((#MANIP))) PROCEDURE: ((#GRASPING #2 *TIME))))))))
    FEATURES (VB INF TRANS))

(DEFS HE FEATURES (PRON NS SUBJ))

(DEFS HER IRREGULAR (SHE (OBJ POSS) (SUBJ)))

(DEFS HIM IRREGULAR (HE (OBJ) (SUBJ)))

(DEFS HIS FEATURES (PRON POSS))

(DEFS HOW SEMANTICS ((QADJ T)) FEATURES (QADJ))

(DEFS HOWEVER FEATURES (PRON EVERPRON))

(DEFS I
    SEMANTICS ((PRON (SMSET (LIST (NEWCOPY 'FRIEND-OSS)))))
    FEATURES (SUBJ PRON NFS))

(DEFS IF FEATURES (BINDER))

(DEFS IN SEMANTICS ((PREP (#IN))) FEATURES (ADV PLACE PREP PLACE))

(DEFS IN-BACK-OF
    ROOT (IN BACK OF)
    SEMANTICS (#LOC #BEHIND T)
    FEATURES (PREP COMBINATION))

(DEFS IN-FRONT-OF
    ROOT (IN FRONT OF)
    SEMANTICS (#LOC #BEHIND NIL)
    FEATURES (PREP COMBINATION))

(DEFS INSIDE SEMANTICS ((PREP (#IN))) FEATURES (PREP PLACE))

(DEFS INSIDE-OF
    ROOT (INSIDE OF)
    SEMANTICS (#IN)
    FEATURES (PREP COMBINATION))

(DEFS INTO SEMANTICS ((PREP (#IN))) FEATURES (PREP PLACE))

(DEFS IS IRREGULAR (BE (V3PS PRESENT) (INF)))

(DEFS IT
    SEMANTICS ((PRON (SMIT 'IT)))
    FEATURES (PRON NS SUBJ OBJ))

(DEFS ITS IRREGULAR (IT (POSS) NIL))

(DEFS KNOW FEATURES (VB INF TRANS REPOB))

(DEFS LARGE
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #SIZE RESTRICTIONS: (#PHYSOB) DIRECTION: T))
           (ADJ (OBJECT (MARKERS: (#PHYSOB #BIG) PROCEDURE: ((#MORE #SIZE *** (128 128 128)))))))
    FEATURES (ADJ))

(DEFS LEAST
    SEMANTICS ((NUMD (LIST '> (SUB1 NUM))))
    FEATURES (NUMD NUMDAT))

(DEFS LEFT
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#DIRECTION) PROCEDURE: ((#DIRECTION #RIGHT NIL))))))
    FEATURES (NOUN NS))

(DEFS LESS
    SEMANTICS ((NUMD (LIST '< NUM)))
    FEATURES (NUMD NUMDAN))

(DEFS LIKE
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#THING))) PROCEDURE: ((#LIKE #1 #2))))))))
    FEATURES (VB INF TRANS))

(DEFS LIST SEMANTICS ((VB ((TRANS (#NAME))))) FEATURES (VB VO TRANS))

(DEFS LITTLE
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #SIZE RESTRICTIONS: (#PHYSOB) DIRECTION: NIL))
           (ADJ (OBJECT (MARKERS: (#PHYSOB #LITTLE) PROCEDURE: ((#MORE #SIZE (128 128 128) ***))))))
    FEATURES (ADJ))

(DEFS LONG
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #LENGTH RESTRICTIONS: (#PHYSOB) DIRECTION: T))
           (ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MORE #LENGTH *** (128 128 128)))))))
    FEATURES (ADJ))

(DEFS MAKE
    SEMANTICS ((VB ((TRANS (#BUILD)))))
    FEATURES (VB INF TRANS))

(DEFS MANY
    SEMANTICS ((NUMD (LIST '> (SUB1 NUM))) (DET T))
    FEATURES (DET QNTFR NPL NONUM NUMD NUMDAS))

(DEFS ME IRREGULAR (I (OBJ) (SUBJ)))

(DEFS MORE
    SEMANTICS ((NUMD (LIST '> NUM)))
    FEATURES (NUMD NUMDAN))

(DEFS MOST
    SEMANTICS ((NUMD (LIST '< (ADD1 NUM))))
    FEATURES (NUMD NUMDAT DET QNTFR NPL NONUM))

(DEFS MOVE
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB))) PROCEDURE: ((#PUT #2 PLACE *TIME)) MARKERS: ((#MOTION))))))))
    FEATURES (VB INF TRANS))

(DEFS MY IRREGULAR (I (POSS) (SUBJ)))

(DEFS NAME
    SEMANTICS ((NOUN (OBJECT ((#NAME #ROLE) ((IS *** #NAME) (#CALL ? ***) (#ROLE (#THING) (#CALL #2 #1))))))
           (VB ((TRANS (#NAME)))))
    FEATURES (NOUN NS VB INF TRANS))

(DEFS NARROW
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MORE #WIDTH (128 0 0) ***)))))
            (MEASURE (MEASURE DIMENSION: #WIDTH RESTRICTIONS: (#PHSYOB) DIRECTION: NIL)))
    FEATURES (ADJ))

(DEFS NEITHER FEATURES (B-SPECIAL) SEMANTICS T B-SPECIAL (BOTH NOR))

(DEFS NICE
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#THING) PROCEDURE: ((#LIKE :FRIEND ***))))))
    FEATURES (ADJ))

(DEFS NO SEMANTICS ((DET 'NO)) FEATURES (DET QNTFR NS NPL))

(DEFS NONE
    SEMANTICS ((DET 'NO))
    FEATURES (DET QNTFR NPL NS NONUM))

(DEFS NOR FEATURES (SPECIAL) SEMANTICS T SPECIAL (CONJ))

(DEFS NOT SEMANTICS ((ADV T)) FEATURES (ADV NEG))

(DEFS NOTHING SEMANTICS ((TPRON 'NO)) FEATURES (TPRON NEG NS))

(DEFS NOW
    SEMANTICS ((ADV (OR (EQ (CADR (ASSQ 'TIME FE)) ':NOW) (ERT NOW DEFINITION))))
    FEATURES (ADV TIMW))

(DEFS OBJECT
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#PHYSOB #VAGUE) PROCEDURE: ((#PHYSOB ***))))))
    FEATURES (NOUN NS))

(DEFS OF
    SEMANTICS ((PREP
        (AND (CQ NG)
            (RELATION
                (RESTRICTIONS: (((#DIRECTION)) ((#PHYSOB)))
                PROCEDURE: ((#EVAL (LIST '#DIRECTION
                                (CADR (SETQ XX (OR (ASSQ '#DIRECTION (CDDAAR (INTERP MAP1))) (ERT OF DEFINITION))))
                                (COND ((CADDR XX) '*OF) ('#2)) (COND ((CADDR XX) '#2) ('*OF)) '*TIME)))))))
           (PREP2 T))
    FEATURES (PREP PREP2 OF))

(DEFS OFF SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS ON SEMANTICS ((PREP (#ON))) FEATURES (PREP PLACE))

(DEFS ON-TOP-OF
    ROOT (ON TOP OF)
    SEMANTICS (#ON)
    FEATURES (PREP COMBINATION))

(DEFS ONE SEMANTICS ((NOUN (SMONE)) (NUM 1)) FEATURES (NUM NOUN NS))

(DEFS ONLY
    SEMANTICS ((NUMD (LIST 'EXACTLY NUM)))
    FEATURES (NUMD NUMDALONE))

(DEFS ONTO SEMANTICS ((PREP (#ON))) FEATURES (PREP PLACE))

(DEFS OR FEATURES (SPECIAL) SEMANTICS T SPECIAL (CONJ))

(DEFS OUT SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS OUT-OF
    ROOT (OUT OF)
    SEMANTICS (#OUTOF)
    FEATURES (PREP COMBINATION))

(DEFS OVER SEMANTICS ((PREP (#LOC #ABOVE T))) FEATURES (PREP PLACE))

(DEFS PICK
    SEMANTICS ((VB ((TRANS (#NOTICE)))))
    FEATURES (VPRT VB INF TRANS))

(DEFS PICK-UP
    ROOT (PICK UP)
    SEMANTICS ((TRANS (RELATION
            (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
             MARKERS: (#EVENT)
             PROCEDURE: ((#EVAL (COND ((MEMQ (NUMBER? SMOB1) '(1 NS)) '(#PICKUP #2 *TIME)) ('(#PUTIN #2 :BOX *TIME)))))))))
    FEATURES (COMBINATION TRANS))

(DEFS PLEASE FEATURES (B-SPECIAL) SEMANTICS T B-SPECIAL (FLUSHME))

(DEFS POINTED
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#PHYSOB #POINTED) PROCEDURE: ((#SHAPE *** #POINTED))))))
    FEATURES (ADJ))

(DEFS PUT
    PAST PUT
    SEMANTICS ((VB
        ((TRANSL (RELATION
              (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB)) (SMOBL (#PLACE)))
               MARKERS: (#EVENT)
               PROCEDURE: (#EVAL
                   (MAPCAR #'(LAMBDA (%PLNRPHRASE)
                        (COND ((EQ (CAR %PLNRPHRASE) '#ON) (LIST '#PUTON '#2 (CADR %PLNRPHRASE) '*TIME))
                            ((EQ (CAR %PLNRPHRASE) '#IN) (LIST '#PUTIN '#2 (CADR %PLNRPHRASE) '*TIME))
                            ((ERT PUT DEFINITION))))
                        (RELATIONS? SMOBL)))))))))
    FEATURES (INF PAST VB TRANSL VPRT))

(DEFS PUT-AWAY
    ROOT (PUT AWAY)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#MANIP))) MARKERS: (#EVENT) PROCEDURE: ((#PUTIN #2 :BOX *TIME))))))
    FEATURES (COMBINATION TRANS))

(DEFS PUT-DOWN
    ROOT (PUT DOWN)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#MANIP))) MARKERS: (#EVENT) PROCEDURE: ((#PUTON #2 :TABLE *TIME))))))
    FEATURES (COMBINATION TRANS))

(DEFS PUT-TOGETHER
    ROOT (PUT TOGETHER)
    SEMANTICS ((TRANS (#BUILD)))
    FEATURES (COMBINATION TRANS))

(DEFS PYRAMID
    FEATURES (NOUN NS)
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#PHYSOB #POINTED) PROCEDURE: ((#IS *** #PYRAMID)))))))

(DEFS RED SEMANTICS ((ADJ (#COLOR #RED))) FEATURES (ADJ))

(DEFS RELEASE FEATURES (VB TRANS INF))

(DEFS RIGHT
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#DIRECTION) PROCEDURE: ((#DIRECTION #RIGHT T))))))
    FEATURES (NOUN NS))

(DEFS ROUND
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#PHYSOB #ROUND) PROCEDURE: ((#SHAPE *** #ROUND))))))
    FEATURES (ADJ))

(DEFS SAW IRREGULAR (SEE (PAST) (INF)))

(DEFS SEE FEATURES (VB INF TRANS))

(DEFS SET SEMANTICS ((VB T)) FEATURES (VB INF))

(DEFS SET-DOWN
    ROOT (SET DOWN)
    SEMANTICS ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#MANIP))) MARKERS: (#EVENT) PROCEDURE: ((#PUTON #2 :TABLE *TIME))))))
    FEATURES (COMBINATION TRANS))

(DEFS SHAPE
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#SHAPE) PROCEDURE: ((#IS *** #SHAPE))))))
    FEATURES (NOUN NS))

(DEFS SHE FEATURES (PRON SUBJ NS))

(DEFS SHORT
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #HEIGHT RESTRICTIONS: (#PHYSOB) DIRECTION: NIL))
           (ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MORE #HEIGHT (128 0 0) ***))))))
    FEATURES (ADJ))

(DEFS SHRDLU REFER :SHRDLU)

(DEFS SINCE SEMANTICS ((BINDER (SMBINDER END NIL))) FEATURES (BINDER TIME))

(DEFS SIT
    SEMANTICS ((VB
        ((ITRNSL (RELATION
              (RESTRICTIONS: (((#PHYSOB)) (SMOBL (#PLACE)))
               PROCEDURE: (#EVAL
                   (MAPCAR #'(LAMBDA (%PLNRPHRASE)
                        (COND ((MEMQ (CAR %PLNRPHRASE) '(#ON #IN)) (LIST '#SUPPORT (CADR %PLNRPHRASE) '#1 '*TIME))
                            ((ERT SIT DEFINITION))))
                        (RELATIONS? SMOBL)))))))))
    FEATURES (VB INF ITRNSL))

(DEFS SIZE
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#SIZE) PROCEDURE: ((#IS *** #SIZE))))))
    FEATURES (NOUN NS))

(DEFS SMALL
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #SIZE RESTRICTIONS: (#PHYSOB) DIRECTION: NIL))
           (ADJ (OBJECT (MARKERS: (#PHYSOB #LITTLE) PROCEDURE: ((#MORE #SIZE (128 128 128) ***))))))
    FEATURES (ADJ))

(DEFS SOME
    SEMANTICS ((DET 'INDEF))
    FEATURES (DET QNTFR NS NPL NONUM))

(DEFS SOMETHING SEMANTICS ((TPRON 'INDEF)) FEATURES (TPRON NS))

(DEFS SPHERE FEATURES (NOUN NS))

(DEFS SQUARE
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#PHYSOB #RECTANGULAR) PROCEDURE: ((#SHAPE ** #RECTANGULAR))))))
    FEATURES (ADJ))

(DEFS STACK
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#STACK) PROCEDURE: ((#IS *** #STACK)))))
           (VB ((TRANS (#STACKUP)))))
    FEATURES (NOUN NS VB INF VPRT TRANS))

(DEFS STACK-UP
    ROOT (STACK UP)
    SEMANTICS ((TRANS (#STACKUP)))
    FEATURES (COMBINATION TRANS))

(DEFS START
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#EVENT))) MARKERS: (#EVENT) PROCEDURE: ((#START #2 *TIME))))))))
    FEATURES (VB INF TRANS INGOB1 TOOB1))

(DEFS SUPPORT
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#PHYSOB #ROLE) PROCEDURE: ((#SUPPORT *** ?) (#ROLE (#PHYSOB) (#SUPPORT #1 #2))))))
           (VB ((TRANS (RELATION (RESTRICTIONS: (((#PHYSOB)) ((#MANIP))) PROCEDURE: ((#SUPPORT #1 #2 *TIME))))))))
    FEATURES (VB INF TRANS IMPERF NOUN NS))

(DEFS TABLE
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#TABLE) PROCEDURE: ((#IS *** #TABLE))))))
    FEATURES (NOUN NS))

(DEFS TAKE FEATURES (VB INF TRANSL TRANS))

(DEFS TALL
    SEMANTICS ((MEASURE (MEASURE DIMENSION: #HEIGHT RESTRICTIONS: (#PHYSOB) DIRECTION: T))
           (ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MORE #HEIGHT *** (128 0 0)))))))
    FEATURES (ADJ))

(DEFS TELL
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#EVENT))) MARKERS: (#EVENT) PROCEDURE: ((#WANT #1 #2 *TIME))))))))
    FEATURES (VB INF TRANS2 TOOB2))

(DEFS THAN SEMANTICS ((NULL T)) FEATURES (THAN))

(DEFS THANK FEATURES (B-SPECIAL) SEMANTICS  (THANK)B-SPECIAL (THANK))

(DEFUN THANK NIL
    (COND ((EQ (CADR N) 'YOU)
        (SAY YOU'RE WELCOME)
        (FLUSHME)
        (FLUSHME)
        (OR NN (^G))
        (SETQ SPECIAL 'DONE))))

(DEFS THAT
    SEMANTICS ((PRONREL T) (DET (SMTHAT)) (NULL T))
    FEATURES (NS THAT DET DEM DEF PRONREL INCOM))

(DEFS THE SEMANTICS ((DET T)) FEATURES (DET NPL NS DEF))

(DEFS THEIR IRREGULAR (THEY (POSS) NIL))

(DEFS THEM IRREGULAR (THEY (OBJ) (SUBJ)))

(DEFS THEN
    SEMANTICS ((ADV
        (AND LASTIME
             (RPLACD (CDDADR (OR (AND (SETQ XX (ASSQ 'TIME FE)) (NOT (ATOM (CADR XX))) XX) '(TIME (#TIME (PAST) NIL))))
                 (LIST (OR (CADDDR LASTIME) (CAR (CDDDDR LASTIME))) (OR (CAR (CDDDDR LASTIME)) (CADDDR LASTIME)))))))
    FEATURES (ADV TIMW))

(DEFS THERE SEMANTICS ((ADV T)) FEATURES (ADV PLACE))

(DEFS THEY
    SEMANTICS ((PRON (SMIT 'THEY)))
    FEATURES (PRON SUBJ NPL))

(DEFS THICK
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MORE #THICKNESS *** (0 128 0))))))
            (MEASURE (MEASURE DIMENSION: #THICKNESS RESTRICTIONS: (#PHYSOB) DIRECTION: T)))
    FEATURES (ADJ))

(DEFS THIN
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MORE #THICKNESS (0 128 0) ***)))))
           (MEASURE (MEASURE DIMENSION: #THICKNESS RESTRICTIONS: (#PHYSOB) DIRECTION: NIL)))
    FEATURES (ADJ))

(DEFS THING
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#THING #VAGUE #PHYSOB) PROCEDURE: ((#PHYSOB  *** ))))))
    FEATURES (NOUN NS))

(DEFS THIS FEATURES (NS DET DEM DEF))

(DEFS THREE SEMANTICS ((NUM 3)) FEATURES (NUM))

(DEFS TIME FEATURES (NOUN NS TIM1))

(DEFS TO
    SEMANTICS ((PREP (RELATION (RESTRICTIONS: (((#PHYSOB)) ((#DIRECTION))) PROCEDURE: ((#EVAL (SUBTOP '#1 '*OF (REFERENCE? SMOB1))))))))
    FEATURES (PREP))

(DEFS TOGETHER SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS TOLD IRREGULAR (TELL (PAST) (INF)))

(DEFS TOP SEMANTICS ((PREP2 T)) FEATURES (PREP2))

(DEFS TOUCH
    SEMANTICS ((VB ((TRANS (#GRASP)))))
    FEATURES (VB INF TRANS))

(DEFS TOY
    SEMANTICS ((NOUN (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MANIP ***))))))
    FEATURES (NOUN NS))

(DEFS TWO SEMANTICS ((NUM 2)) FEATURES (NUM))

(DEFS UNDER
    SEMANTICS ((PREP (#LOC #ABOVE NIL)))
    FEATURES (PREP PLACE))

(DEFS UNDERNEATH
    SEMANTICS ((PREP ((#LOC #ABOVE NIL))))
    FEATURES (PREP PLACE))

(DEFS UP SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS US IRREGULAR (WE (OBJ) (SUBJ)))

(DEFS WANT
    SEMANTICS ((VB ((TRANS (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#EVENT))) MARKERS: (#EVENT) PROCEDURE: ((#WANT #1 #2 *TIME))))))))
    FEATURES (VB INF TRANS TOOB SUBTOB))

(DEFS WAS IRREGULAR (BE (V3PS VFS PAST) (INF)))

(DEFS WE
    SEMANTICS ((PRON (SMSET (LIST (NEWCOPY 'WE-OSS)))))
    FEATURES (PRON NPL SUBJ))

(DEFS WERE IRREGULAR (BE (VPL PAST) (INF)))

(DEFS WHAT
    SEMANTICS ((DET T) (PRON (SMSET (LIST (NEWCOPY 'UNKNOWN-OSS)))))
    FEATURES (QDET DET NPL PRON QPRON NS))

(DEFS WHATEVER FEATURES (PRON EVERPRON NS))

(DEFS WE REFER (:SHRDLU :FRIEND))

(DEFS WHERE SEMANTICS ((QADJ (FQ WHERE))) FEATURES (QADJ PLACE))

(DEFS WHEREVER FEATURES (PRON EVERPRON NS))

(DEFS WHEN
    SEMANTICS ((BINDER (SMBINDER START END)) (QADJ (FQ WHEN)))
    FEATURES (QADJ BINDER TIME))

(DEFS WHENEVER FEATURES (BINDER))

(DEFS WHICH
    SEMANTICS ((PRONREL T) (DET T))
    FEATURES (QDET DET PRONREL NS NPL))

(DEFS WHICHEVER FEATURES (DET RSQDET NS NPL))

(DEFS WHILE SEMANTICS ((BINDER (SMBINDER START END))) FEATURES (BINDER TIME))

(DEFS WHITE SEMANTICS ((ADJ (#COLOR #WHITE))) FEATURES (ADJ))

(DEFS WHO
    SEMANTICS ((PRONREL T) (PRON (SMSET (LIST (NEWCOPY ANIMATE-OSS)))))
    FEATURES (PRONREL QPRON PRON NS))

(DEFS WHOEVER FEATURES (PRON EVERPRON NS))

(DEFS WHOSE FEATURES (DET QDET NPL NS))

(DEFS WHY SEMANTICS ((QADJ (FQ WHY))) FEATURES (QADJ))

(DEFS WHYEVER FEATURES (PRON EVERPRON NS))

(DEFS WIDE
    SEMANTICS ((ADJ (OBJECT (MARKERS: (#PHYSOB) PROCEDURE: ((#MORE #WIDTH *** (0 128 0))))))
            (MEASURE (MEASURE DIMENSION: #WIDTH RESTRICTIONS: (#PHYSOB) DIRECTION: T)))
    FEATURES (ADJ))

(DEFS WILL SEMANTICS ((VB T)) FEATURES (VB AUX WILL MODAL V3PS VFS VPL))

(DEFS WITH FEATURES (PREP))

(DEFS WOULD SEMANTICS ((VB T)) FEATURES (VB AUX MODAL))

(DEFS YOU
    SEMANTICS ((PRON (SMSET (LIST (NEWCOPY 'SHRDLU-OSS)))))
    FEATURES (PRON NPL NS SUBJ OBJ))

(DEFS YOUR IRREGULAR (YOU (POSS) NIL))

;; ############################################################
;;
;;                          #WORDS
;;
;; ############################################################

(DEFS #ANIMATE SYSTEM (#ROBOT #PERSON) SYS (#THING))

(DEFS #ASMUCH THMLIST ((4 '((THUSE TC-ASMUCH)))))

(DEFS #BELONG THMLIST ((3 '((THUSE TC-BELONG)))))

(DEFS #BLACK SYS (#SPECTRUM))

(DEFS #BLUE SYS (#SPECTRUM))

(DEFS #BLUEPRINT
    EXPR (LAMBDA (X)
        (PROG (PARTS)
            (COND ((GET X 'REFER) (RETURN '#2))
                ((NULL (SETQ X (CDDAAR (INTERP X))))
                    (GO DONE)))
        LOOP (COND ((NOT (EQ (CAAR X) 'THGOAL)) (ERT BLUEPRINT THGOAL))
                ((EQ (CAADAR X) '#IS))
                ((EQ (CAADAR X) '#PART) (SETQ PARTS (CONS (CADR (CADAR X)) PARTS)))
                ((ERT #BLUEPRINT)))
            (AND (SETQ X (CDR X)) (GO LOOP))
        DONE (AND PARTS
                (GET (CAR PARTS) 'REFER)
                (RETURN (GET (CAR PARTS) 'REFER)))
            (PUTPROP 'BLUEPRINT
                (COND ((NULL PARTS) (GET 'STACKPARTS 'SM))
                    ((CDR PARTS) (ERT #BLUEPRINT PARTS))
                    ((GET (CAR PARTS) 'SM)))
                'SM)
            (RETURN 'BLUEPRINT))))

(DEFS #BOX SYS (#PHYSOB))

(DEFS #BUILD
    EXPR (LAMBDA NIL
        (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#STACK))) MARKERS: (#EVENT) PROCEDURE: ((#EVAL (LIST '#STACKUP (#BLUEPRINT SMOB1) '*TIME)))))))

(DEFS #CALL THMLIST ((3 '((THUSE TC-3)))))

(DEFS #COLOR
    FEXPR (LAMBDA (A)
        (EVAL (SUBST (CAR A) 'COLOR '(OBJECT (MARKERS: (#PHYSOB COLOR) PROCEDURE: ((#COLOR *** COLOR)))))))
    PRIORITY 192
    SYS (#PROPERTY))

(DEFS #CONSTRUCT SYSTEM (#STACK #ROW) SYS (#PHYSOB))

(DEFS #CONTAIN PRIORITY -1)

(DEFS #CLEANOFF
    EXPR (LAMBDA NIL
        (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB))) MARKERS: (#EVENT) PROCEDURE: ((#CLEARTOP #2 *TIME))))))

(DEFS #CLEARTOP
    NAMEVENT (I3 (CONS (VBFIX 'CLEAN NIL) (PRTPUT 'OFF OBJ1)))
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(DEFS #DEFINE
    EXPR (LAMBDA (X Y)
        (LIST '#DEFINITION
            (CADADR (CDAAR (INTERP X)))
            (PROG (X)
                (PUTPROP (SETQ X (MAKESYM 'ATM)) (INTERP Y) 'NEWWORD)
                (RETURN X)))))

(DEFS #DEFINITION
    FEXPR (LAMBDA (A)
        (PUTPROP (CADAR A) '(NOUN NS) 'WORD)
        (PUTPROP (CADAR A)
            (SUBST (SUBST '*** (CADDR (GET (CADR A) 'NEWWORD)) (CAR (GET (CADR A) 'NEWWORD)))
                'NG
                '((NOUN (SETQ LIST2 (LIST (SUBST (SUBST (CADDAR LIST1) '*** 'NG) (CAAR LIST1) (CAR LIST1)))))))
            'SMNTC))
    NOGOAL T)

(DEFS #DIRECTION NOGOAL T)

(DEFS #END
    THMLIST ((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(DEFS #EQDIM
    EXPR (LAMBDA (X)
            (SETQ X (SIZE X))
            (AND (EQ (CAR X) (CADR X)) (EQ (CAR X) (CADDR X))))
    NOGOAL T)

(DEFS #EQUIV PRIORITY 512)

(DEFS #EVENT SYS (#SYSTEMS))

(DEFS #EXISTS
    THMLIST ((2 '((THUSE TC-EXISTS))) (3 '((THUSE TCT-EXISTS)))))

(DEFS #GET-RID-OF
    THMLIST ((2 '((THUSE TCT-EXISTS))) (3 '((THUSE THUSE TCT-3))) (4 '((THUSE TCTE-4))))
    NAMEVENT (I3 (APPEND (LIST (VBFIX 'GET T) 'RID 'OF) OBJ1)))

(DEFS #GRASP
    EXPR (LAMBDA NIL
        (RELATION
            (RESTRICTIONS: (((#ANIMATE)) ((#MANIP))) MARKERS: (#EVENT)
            PROCEDURE: ((#EVAL (COND ((IMPERF) '(#GRASPING #2 *TIME)) ('(#GRASP #2 *TIME))))))))
    NAMEVENT (I3 (CONS (VBFIX 'GRASP NIL) OBJ1))
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(DEFS #GRASPING THMLIST ((3 '((THUSE TCT-GRASPING)))))

(DEFS #GREEN SYS (#SPECTRUM))

(DEFS #HAND SYS (#PHYSOB))

(DEFS #HAVE
    EXPR (LAMBDA NIL
        (RELATION
            (RESTRICTIONS: (((#THING)) ((#THING) (AND (MEMQ '#ROLE (MARKERS? SMOB1)) (CHECK (CADR (ASSOC '#ROLE (RELATIONS? SMOB1))) (MARKERS? SMSUB) (SYSTEMS? SMSUB)))))
            PROCEDURE: ((#SUBST #1 ?)))
            (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB)))
            PROCEDURE: ((#BELONG #2 #1))))))

(DEFS #HEIGHT MEASFN (LAMBDA (X) (CADDR (SIZE X))))

(DEFS #IN
    EXPR (LAMBDA NIL
        (COND ((CQ LOBJ)
            (RELATION
                (RESTRICTIONS: (((#THING)) ((#BOX))) MARKERS: (#PLACE) PROCEDURE: ((#IN #2)))))
            ((RELATION
                (RESTRICTIONS: (((#MANIP)) ((#BOX))) PROCEDURE: ((#CONTAIN #2 #1 *TIME)))
                (RESTRICTIONS: (((#MANIP)) ((#HAND))) PROCEDURE: ((#GRASPING #1 *TIME)))
                (RESTRICTIONS: (((#PLACE)) ((#BOX))) PROCEDURE: ((#IN #1 #2)))
                (RESTRICTIONS: (((#MANIP)) ((#CONSTRUCT))) PROCEDURE: ((#PART #1 #2 *TIME))))))))

(DEFS #IS PRIORITY 64)

(DEFS #LIKE TELLABLE T THMLIST ((3 '((THTBF THTRUE)))))

(DEFS #LOC
    THMLIST ((4 '((THUSE TC-LOC))) (5 '((THUSE TCT-LOC))))
    FEXPR (LAMBDA (A) (#LOC2 (CAR A) (CADR A))))

(DEFS #LOC2
    EXPR (LAMBDA (LOCTYPE #LOC)
        (COND ((CQ LOBJ)
            (RELATION (RESTRICTIONS: (((#THING)) (LOBJ (#PHYSOB))) MARKERS: (#PLACE) PROCEDURE: ((#EVAL (LIST '#LOC LOCTYPE #LOC #2))))))
        ((RELATION (RESTRICTIONS: (((#PHYSOB)) ((#PHYSOB))) PROCEDURE: ((#EVAL (LIST '#LOC LOCTYPE (COND (#LOC '#1) ('#2)) (COND (#LOC '#2) ('#1)) '*TIME)))))))))

(DEFS #MANIP SYS (#PHYSOB))

(DEFS #MORE THMLIST ((4 '((THUSE TC-MORE)))))

(DEFS #NAME
    THMLIST ((2 '((THUSE TC-2))))
    EXPR (LAMBDA NIL
        (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB))) MARKERS: (#EVENT) PROCEDURE: ((#NAME #2)))))
    SYS (#SYSTEMS))

(DEFS #NEWWORD SYS (#THING))

(DEFS #NOTICE
    THMLIST ((2 '((THUSE TC-2))))
    EXPR (LAMBDA NIL
        (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB))) MARKERS: (#EVENT) PROCEDURE: ((#NOTICE #2 *TIME))))))

(DEFS #ON
    THMLIST ((3 '((THUSE TC-ON))) (4 '((THUSE TCT-ON))))
    EXPR (LAMBDA NIL
        (COND ((CQ LOBJ)
            (RELATION (RESTRICTIONS: (((#THING)) ((#PHYSOB))) MARKERS: (#PLACE) PROCEDURE: ((#ON #2)))))
            ((RELATION
                (RESTRICTIONS: (((#PHYSOB)) ((#PHYSOB))) PARAPHRASE: (ANYWHERE ON TOP OF) PROCEDURE: ((#ON #1 #2 *TIME)))
                (RESTRICTIONS: (((#PHYSOB)) ((#MANIP))) PARAPHRASE: (DIRECTLY ON THE SURFACE) PROCEDURE: ((#SUPPORT #2 #1 *TIME)))
                (RESTRICTIONS: (((#PLACE)) ((#PHYSOB))) PROCEDURE: ((#ON #1 #2))))))))

(DEFS #PACK THMLIST ((3 '((THUSE TC-3)))))

(DEFS #PART THMLIST ((3 '((THUSE TC-PART)))))                       ;; PERFORMED ON GRADUATION DAY, JUNE 2, 1972 BY JMH

(DEFS #PERSON SYS (#ANIMATE))

(DEFS #PICKUP
    THMLIST ((2 '((THUSE TC-2))) (3 '((THUSE TCT-PICKUP))) (4 '((THUSE TCTE-PICKUP))))
    NAMEVENT (I3 (CONS (VBFIX 'PICK NIL) (PRTPUT 'UP OBJ1))))

(DEFS #PLACE SYS (#SYSTEMS))

(DEFS #PUT
    THMLIST ((3 '((THUSE TCT-3))) (4 '((THUSE TCT-PUT))) (5 '((THUSE TCTE-PUT)))))

(DEFS #PUTIN
    THMLIST ((3 '((THUSE TC-3))) (4 '((THUSE TCT-4))) (5 '((THUSE TCT-5)))))

(DEFS #PUTON
    NAMEVENT (I4 (CONS (VBFIX 'PUT T) (APPEND OBJ1 '(ON) OBJ2)))
    THMLIST ((3 '((THUSE TC-3))) (4 '((THUSE TCT-4))) (5 '((THUSE TCTE-5)))))

(DEFS #RAISE THMLIST ((1 '((THUSE TC-RAISE)))))

(DEFS #RECTANGULAR SYS (#SHAPES))

(DEFS #REFERS THMLIST ((2 '((THUSE TC-REFERS)))))

(DEFS #ROLE FEXPR (LAMBDA (A) T) NOGOAL T)

(DEFS #PHYSOB
    SYSTEM (#BOX #CONSTRUCT #HAND #MANIP #TABLE)
    SYS (#THING)
    THMLIST ((2 '((THUSE TC-PHYSOB)))))

(DEFS #PROPDEFINE
    EXPR (LAMBDA (X)
        (PUTPROP X '(PROPN NS) 'FEATURES)               ;; CHANGED TO FEATURES FROM 'WORD' IN THE OLD DICTIONARY
        (PUTPROP X '((PROPN T)) 'SEMANTICS)))

(DEFS #PROPERTY SYSTEM (#COLOR #SIZE #SHAPE) SYS (#THING))

(DEFS #POINTED SYS (#SHAPES))

(DEFS #RED SYS (#SPECTRUM))

(DEFS #RELATION SYS (#SYSTEMS))

(DEFS #ROLE FEXPR (LAMBDA (A) T) NOGOAL T)

(DEFS #ROUND SYS (#SHAPES))

(DEFS #ROW SYS (#CONSTRUCT))

(DEFS #ROBOT SYS (#ANIMATE))

(DEFS #SIZE
    MEASFN (LAMBDA (X) (APPLY 'PLUS (SIZE X)))
    SYS (#PROPERTY))

(DEFS #SHAPE PRIORITY 128 SYS (#PROPERTY))

(DEFS #STACK SYS (#CONSTRUCT))

(DEFS #STACKUP
    THMLIST ((2 '((THUSE TC-2))))
    EXPR (LAMBDA NIL (RELATION (RESTRICTIONS: (((#ANIMATE)) ((#MANIP))) MARKERS: (#EVENT) PROCEDURE: ((#STACKUP #2 *TIME)))))
    NAMEVENT (I3 (CONS (VBFIX 'STACK NIL) (PRTPUT 'UP OBJ1))))

(DEFS #START
    THMLIST ((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(DEFS #SUBST NOGOAL T)

(DEFS #SUPPORT
    PRIORITY 256
    THMLIST ((3 NIL) (4 '((THUSE TCT-SUPPORT)))))

(DEFS #SYSTEMS SYSTEM (#THING #EVENT #NAME #RELATION #PLACE))

(DEFS #TABLE SYS (#PHYSOB))

(DEFS #THICKNESS MEASFN (LAMBDA (X) (CADR (SIZE X))))

(DEFS #THING SYS (#SYSTEMS) SYSTEM (#ANIMATE #NAME #PHYSOB #PROPERTY))

(DEFS #UNGRASP
    THMLIST ((1 '((THUSE TC-UNGRASP))))
    NAMEVENT (I3 (APPEND (LIST (VBFIX 'LET T) 'GO 'OF) OBJ1)))

(DEFS #WANT
    THMLIST ((4 '((THUSE TC-WANT4))) (5 '((THUSE TC-WANT5)))))

(DEFS #WHITE SYS (#SPECTRUM))

(DEFS #WIDTH MEASFN (LAMBDA (X) (CAR (SIZE X))))

;; ############################################################
;;
;;                     PARTS OF SPEECH
;;
;; ############################################################

(DEFS ADJ ELIM (ADJ SUP COMPAR))

(DEFS ADV ELIM (ADV PREPADV TIMW TIM2 ADVADV VBAD PLACE LOBJ))

(DEFS BINDER ELIM (BINDER TIME))

(DEFS CLASF ELIM (CLASF))

(DEFS DET ELIM (DET NPL NS PART DEF INDEF NEG DEM INCOM OFD QNTFR NONUM QDET))

(DEFS NOUN ELIM (NOUN POSS MASS NPL NS TIM1 TIME MONTH))

(DEFS NUM ELIM (NUM NPL NS))

(DEFS NUMD ELIM (NUMD NUMDAN NUMDAT NUMDALONE))

(DEFS ORD ELIM (ORD TIMORD))

(DEFS POSS ELIM (NOUN NPL NS MASS NFS PRON))

(DEFS PREP ELIM (PREP MOTOR PLACE NEED2))

(DEFS PREP2 ELIM (PREP2))

(DEFS PRON ELIM (PRON QPRON EVERPRON POSS SUBJ OBJ NS NPL NFS NEG DEFPOSS))

(DEFS PRT ELIM (PRT))

(DEFS QADJ ELIM (PLACE QADJ))

(DEFS PROPN ELIM (PROPN POSS NS NPL))

(DEFS TPRON ELIM (TPRON NS NPL NEG ANY))

(DEFS VB ELIM (VB MVB AUX QAUX MODAL WILL BE DO HAVE ING EN INF V3PS QUOTING VFS VPL PAST PRESENT NEG ITRNS TRANS TRANSL TRANS2 TRANSL2 INT ITRNSL INGOB TOOB SUBTOB REPOB INGOB2 TOOB2 SUBTOB2 REPOB2 VPRT TO2 TRANSINT TOOB1 INGOB1 REPOB1))

;; ############################################################
;;
;;     I'M NOT QUITE SURE WHAT TO DO WITH THIS RANDOM STUFF
;;
;; ############################################################

(DEFS D MOD ((PAST EN) (INF MODAL AUX)))

(DEFS G MOD ((ING) (INF)))

(DEFS R MOD ((COMPAR) NIL))

(DEFS T MOD ((SUP) NIL))

(DEFS N MOD ((EN) (INF)))

(DEFS S MOD ((PRESENT V3PS NPL) (NS INF MODAL AUS MAS)))

(DEFS * MOD ((NEG) (NIL)))

(DEFS THAMONG NOGOAL T)

(DEFS THSETQ NOGOAL T)

(DEFS THGOAL NOGOAL T)

(DEFS THOR NOGOAL T)

(DEFS THNOT NOGOAL T)

(DEFS THAND NOGOAL T)

(DEFS THPROG NOGOAL T)

(DEFS THFIND NOGOAL T)

;; ############################################################
;;
;;                     PRE-BUILT OSS'S
;;
;; ############################################################

(DEFS ANIMATE-OSS
    OSSNODE= ANIMATE-OSS
    MARKERS= (#ANIMATE #THING #SYSTEMS)
    RELATIONS= ((#IS $?ANIM ?))
    SYSTEMS= (#THING #SYSTEMS)
    DETERMINER= (SG-PL INDEF WHICH)
    VARIABLE= ANIM)

(DEFS FAKE-AGENT
    FEATURES (NG INDEF SG-PL)
    SEMANTICS (UNKNOWN-OSS-BY)
    PARENT (FAKE-BY-PHRASE))

(DEFS FAKE-BY-PHRASE
    FEATURES (PREPG AGENT)
    FIRSTWORD (BY)
    DAUGHTERS (FAKE-AGENT FAKE-BY))

(DEFS FAKE-BY
    FEATURES (PREP BY)
    FIRSTWORD (BY)
    DAUGHTERS WORD)

(DEFS FINDEVENTS-OSS
    OSSNODE= FINDEVENTS-OSS
    MARKERS= (#EVENT #SYSTEMS)
    SYSTEMS= (#SYSTEMS)
    DETERMINER= (SG-PL INDEF NIL)
    VARIABLE= FINDEVENTS)

(DEFS FRIEND-OSS
    OSSNODE= FRIEND-OSS
    MARKERS= (#PERSON #ANIMATE #THING #SYSTEMS)
    SYSTEMS= (#ANIMATE #THING #SYSTEMS)
    REFER= (:FRIEND)
    DETERMINER= (1 DEF NIL))

(DEFS NAME-OSS
    OSSNODE= NAME-OSS
    MARKERS= (#NAME #THING #SYSTEMS)
    SYSTEMS= (#THING #SYSTEMS)
    DETERMINER= (1 DEF NIL))

(DEFS PLACE-OSS
    OSSNODE= PLACE-OSS
    MARKERS= (#PLACE #SYSTEMS)
    SYSTEMS= (#SYSTEMS)
    DETERMINER= (SG-PL INDEF WHICH)
    VARIABLE= PLACE)

(DEFS SHRDLU-OSS
    OSSNODE= SHRDLU-OSS
    MARKERS= (#ROBOT #ANIMATE #THING #SYSTEMS)
    SYSTEMS= (#ANIMATE #THING #SYSTEMS)
    REFER= (:SHRDLU)
    DETERMINER= (1 DEF NIL))

(DEFS STACKPARTS-OSS
    OSSNODE= STACKPARTS-OSS
    MARKERS= (#THING #PHYSOB #MANIP #SYSTEMS)
    SYSTEMS= (#THING #PHYSOB #SYSTEMS)
    DETERMINER= (3 INDEF NIL)
    VARIABLE= PART)

(DEFS UNKNOWN-OSS
    OSSNODE= UNKNOWN-OSS
    MARKERS= (#THING #SYSTEMS #PHYSOB #VAGUE)
    SYSTEMS= (#THING #SYSTEMS)
    DETERMINER= (SG-PL INDEF WHICH)
    VARIABLE= UNKNOWN)

(DEFS UNKNOWN-OSS-BY
    OSSNODE= UNKNOWN-OSS-BY
    RELATIONS= ((#IS $?UNKNOWN ?))
    MARKERS= (#THING #SYSTEMS #PHYSOB #VAGUE)
    SYSTEMS= (#THING #SYSTEMS)
    DETERMINER= (SG-PL INDEF NIL)
    PARSENODE= (FAKE-AGENT)
    VARIABLE= UNKNOWN)

(DEFS UNKNOWNSG-OSS
    OSSNODE= UNKNOWNSG-OSS
    MARKERS= (#THING #SYSTEMS #PHYSOB #VAGUE)
    RELATIONS=  ((#IS $?UNKNOWN ?))
    SYSTEMS= (#THING #SYSTEMS)
    DETERMINER= (NS INDEF WHICH)
    VARIABLE= UNKNOWN)

(DEFS WE-OSS
    OSSNODE= WE-OSS
    MARKERS= (#ANIMATE #THING #SYSTEMS)
    SYSTEMS= (#ANIMATE #THING #SYSTEMS)
    REFER= (:SHRDLU :FRIEND)
    AND= (FRIEND-OSS SHRDLU-OSS))

;; ======>>> TEMPORARY PLACE FOR OSS-PROPERTY DEFS - MOVE WHEN APPROVED

(DEFS ANIMATE OSS ANIMATE-OSS)

(DEFS FINDEVENTS OSS FINDEVENTS-OSS)

(DEFS FRIEND OSS FRIEND-OSS)

(DEFS NAME OSS NAME-OSS)

(DEFS PLACE OSS PLACE-OSS)

(DEFS SHRDLU OSS SHRDLU-OSS)

(DEFS STACKPARTS OSS STACKPARTS-OSS)

(DEFS UNKNOWN OSS UNKNOWN-OSS)

(DEFS UNKNOWNSG OSS UNKNOWNSG-OSS)

(DEFS WE OSS WE-OSS)

(DEFLIST CONTRAST
    (RED #COLOR) (BLUE #COLOR) (GREEN #COLOR) (WHITE #COLOR) (BLACK #COLOR)
    (BIG #SIZE) (LITTLE #SIZE) (LARGE #SIZE) (SMALL #SIZE)
    (WIDE #WIDTH) (NARROW #WIDTH)
    (TALL #HEIGHT) (SHORT #HEIGHT)
    (THICK #THICKNESS) (THIN #THICKNESS))

#_(ns shrdlu.smspec)

;; ############################################################
;;
;;                           SMSPEC
;;                    (SEMANTIC SPECIALISTS)
;;
;; ############################################################

(DEFUN SMTIME NIL (ERT SMTIME NOT WRITTEN YET))

(DEFUN SMTIME2 NIL (ERT SMTIME2 NOT WRITTEN YET))

(DEFUN SMNEWNOUN NIL
    (OBJECT (MARKERS: (#NEWNOUN) PROCEDURE: ((#NEWWORD)))))

(DEFUN SMNEWPROPN NIL (SMSET (LIST (NEWCOPY 'NAME-OSS))))

(DEFUN SMCONJ NIL
    ;; FOR ALL CONJOINED THINGS -- IT CURRENTLY ONLY HANDLES THINGS
    ;; WHICH HAVE AN OSS OR RSS STRUCTURE AS THEIR SEMANTICS.  THIS
    ;; DOES NOT INCLUDE SINGLE WORDS OF MOST TYPES.  IT USES RECURSION
    (PROG (%SM) (SMCONJ2 NIL H) (RETURN (SMSET %SM))))

(DEFUN SMCONJ2 (INTERPLIST RESTLIST)
    ;; INTERPLIST IS THE LIST OF INTERPRETATIONS FOR THE CONJUNCTS
    ;; HANDLED SO FAR -- THIS FUNCTION WILL BE CALLED ONCE FOR EACH
    ;; POSSIBLE COMBINATION.  THE MARKERS FOR THE CONJOINED
    ;; STRUCTURE ARE THOSE OF THE FIRST CONJUNCT -- NEEDS MORE
    ;; SOPHISTICATION.  RESTLIST IS THE REST OF NODES YET TO BE HANDLED.
    (PROG (%X)
        (OR RESTLIST
            (RETURN (SETQ %SM
                (CONS (BUILD
                        RSSNODE= (AND (RSS? INTERP) (MAKESYM 'RSS))
                        OSSNODE= (AND (OSS? INTERP) (MAKESYM 'OSS))
                        MARKERS= (MARKERS? INTERP)
                        SYSTEMS= (SYSTEMS? INTERP)
                        REL= (REL? INTERP)
                        AND= (AND (OR (CQ BUT) (CQ AND)) INTERPLIST)
                        OR= (AND (OR (CQ OR) (CQ NOR)) INTERPLIST))
                    %SM))))
        ;; WHEN THERE IS NO RESTLIST, WE HAVE LOOPED TO THE END OF THE LIST OF CONJUNCTS, AND THE RESULTING INTERPRETATION IS OK.
        ;; THE MAPPING IS DOWN THE LIST OF INTERPRETATIONS FOR A SINGLE CONJUNCT WHILE THE RECURSION GETS US DOWN THE LIST OF CONJUNCTS.
        ;; THUS WE GET EVERY POSSIBLE COMBINATION OF THE INTERPRETATIONS. -- ISN'T LISP SUPER-DUPER-WONDERFUL!
        ;; NOTICE THAT INTERP IS GETTING PICKED UP AS A FREE VARIABLE BY SMCONJ2, EVEN THOUGH IT IS BOUND ONLY INSIDE A MAPCAR INSIDE SMCONJ2.
        ;; THIS WORKS BECAUSE THE CLAUSE CONTAINING IT CAN NEVER GET CALLED EXCEPT BY RECURSION,
        (MAPCAR #'(LAMBDA (INTERP) (SMCONJ2 (CONS INTERP INTERPLIST) (CDR RESTLIST))) (SM RESTLIST))))

(DEFUN SMVG NIL ;; CALLED INSIDE ANY VG
    (PROG (TSS TENSE)
        (SETQ TSS (GETR 'TIME (MOVE-PT C U (CLAUSE))))
        (AND (CQ NEG) (ADD-F-PT 'NEG PT))                   ;; NEG IS TRANSFERRED FROM THE VG TO THE CLAUSE IN WHICH IT IS EMBEDDED.
        (SETQ TENSE (GETR 'TENSE C))
        (COND ((MEMBER TENSE '((PRESENT) (IMPER) (INFINITIVE))) T)
            ((EQUAL TENSE '(MODAL))
                (SETQ GLOBAL-MESSAGE '(THAT DOESN\'T MAKE ANY SENSE TO ME\.))
                (ADD-F-PT 'MODAL PT))                       ;; CLAUSES ARE ALSO MARKED AS MODAL.
            ((AND (EQUAL TENSE '(FUTURE)) (ISQ PT QUEST) (EQUAL (REFER? (CAR (SM (GETR 'SUBJECT PT)))) '(:SHRDLU))) ;; FUTURE QUESTIONS WITH "YOU"
                (SETQ TENSE '(PRESENT))                     ;; SUBJECT IS REALLY IMPERATIVE
                (REMOVE-F-PT 'QUEST PT)
                (ADD-F-PT 'IMPER PT))                       ;; THE CLAUSE IS NO LONGER QUESTION, BUT RATHER, IMPERATIVE
            ((SETDIF TENSE '(PAST PRESENT))
                (GLOBAL-ERR '(I DON\'T KNOW HOW TO HANDLE TENSES INVOLVING FUTURE EVENTS OR MODALS OTHER THAN IN THE PRESENT))))
        (PUTPROP TSS TENSE 'TENSE=)
        (RETURN T)))

(DEFUN SMADJGQSHORT NIL (ERT SMADJQSHORT NOT WRITTEN YET))

(DEFUN SMPRON (NODE)
    (EVAL (SM NODE))
    (COND ((NULL SM)
        (SETQ GLOBAL-MESSAGE (APPEND '(I DON\'T KNOW WHAT \") (FROM (NB H) (N H)) '(\" REFERS TO))))) ;; "sic!
    SM)

(DEFUN SMVAUX NIL
    (COND ((ISQ H NEG) (FQ NEG)) (T))
    (PUTPROP (GETR 'TIME C) (OR (MEET (FE H) '(PRESENT PAST MODAL)) (ERTERR SMVAUX -- FUNNY TENSE)) 'TENSE=))

(DEFUN SMADV NIL (ERT SMADV NOT WRITTEN YET))

(DEFUN SMPLACE NIL (ERT SMPLACE NOT WRITTEN YET))

(DEFUN SMTOADJ NIL (ERT SMTOADJ (UNCT) NOT WRITTEN YET))

(DEFUN SMPROP NIL
    ;; THIS IS THE SEMANTICS FOR PROPER NOUNS.  IT PRODUCES TWO
    ;; INTERPRETATIONS.  ONE IS THE OPAQUE REFERENCE TO THE NAME
    ;; ITSELF, AS IN "CALL IT SAM".  THE OTHER IS THE TRANSPARENT
    ;; REFERENT AS IN "PICK UP SAM".
    (SMSET (LIST
        (BUILD
            OSSNODE= (MAKESYM 'OSS)
            VARIABLE= 'NAME
            DETERMINER= (1 DEF NIL)
            PARSENODE= C
            MARKERS= (#NAME)
            REFER= (LIST (WORD (NB H))))
        (BUILD
            OSSNODE= (MAKESYM 'OSS)
            DETERMINER= (1 DEF NIL)
            PARSENODE= C
            VARIABLE= (MAKESYM 'X)
            RELATIONS= (LIST (LIST '#NAME OSSNODE= (WORD (NB H)))))))
    (SMNG2))

(DEFUN SMADJ (WORD-BEING-INTERPRETED)
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
    (EVAL (SM WORD-BEING-INTERPRETED)))
    ;; EVALUATE THE DEFINITION OF THE ADJECTIVE

;; --------------------------------------------

(DEFUN SMADJG-PREPG NIL
    ;; HANDLES ADJECTIVE GROUPS AND PREPGS BOTH AS COMPLEMENTS AND QUALIFIERS.
    ;; DO NOTHING FOR "BY" PHRASES IN PASSIVE CLAUSES OR "OF" PHRASES LIKE IN "THREE OF THE BLOCKS".
    ;; SEMANTIC SUBJECT IS THE SUBJECT OF AN INTENSIVE OR THE NG TO WHICH THE GROUP IS A QUALIFIER,
    ;; OR THE CLAUSE OF WHICH IT IS AN ADJUNCT.
    (PROG (X SMSUB)
        (AND (OR (CQ AGENT) (CQ OF)) (RETURN T))
        (SETR 'LOGICAL-SUBJECT
            (COND ((CQ COMP) (GETR 'SUBJECT (MOVE-PT C U (CLAUSE))))
                ((CQ LOBJ) (OR (GETR 'OBJ1 (MOVE-PT C U (CLAUSE))) (GETR 'SUBJECT PT)))
                ((ISQ (MOVE-PT C U (NOT (ISQ PT COMPONENT)) U) NG) PT)
                ((ISQ PT CLAUSE) PT)
                ((ERTERR SMADJG-PREPG FUNNY POSITION)))
            C)
        (SETQ SMSUB (SM (GETR 'LOGICAL-SUBJECT C)))
        (AND (CQ ADJG)
            (GETR 'OBJ1 C)
            (SETR 'ADJGHEAD
            (COMPARE-BUILD (GETR 'HEAD C) (COND ((CQ AS) '#ASMUCH) ((CQ THAN) '#MORE) ((ERTERR SMADJG-PREPG FUNNY TYPE))))
            C))
        (COND
            ((GETR 'OBJ1 C) (SMCL1) (RETURN SM))
            ((RETURN (SMSET
                (PROG (SM)
                    (SMSET (MAPCAR #'(LAMBDA (OSS)
                        (BUILD
                            OSSNODE= (MAKESYM 'OSS)
                            MARKERS= (MARKERS? OSS)
                            SYSTEMS= (SYSTEMS? OSS)
                            VARIABLE= (VARIABLE? OSS)
                            REFER= (REFER? OSS)
                            REL= OSS
                            REFER= (REFER? OSS)
                            DETERMINER= '(NS-PL INDEF NIL)))
                        SMSUB))
                    (EVAL (COND
                        ((OR (CQ COMPAR) (CQ SUP)) (FINDMEASURE (GETR 'HEAD C)))
                        (T (SM (GETR 'HEAD C)))))
                    (RETURN SM))))))))

;; -------------------------------------------

(DEFUN SMIT (PRONOUN)
    ;; PRONOUN IS (IT THEY ONE) A NODE LIST OF POSSIBLE REFERENTS.
    ;; IS THIS A "DO IT!" COMMAND?  IF SO, RETURN THE LAST EVENT MENTIONED.
    ;; IF THIS PRONOUN HAS BEEN USED BEFORE IN THIS SENTENCE, THEN USE THE SAME CANDIDATES.
    ;; IF THIS PRONOUN WAS USED IN THE PREVIOUS SENTENCE,
    ;; LOOK FOR A STRUCTURE LIKE "A BLOCK WHICH IS TALLER THAN ANYTHING WHICH SUPPORTS IT"
    ;; OR "A BLOCK TALLER THAN ANYTHING WHICH SUPPORTS IT".
    (PROG (CANDIDATES AMBIGUITIES)
        (OR DISCOURSE (ERT SMIT: DISCOURSE SWITCH NOT ON))
        (AND MVB
            (ISQ MVB DO)
            (CQ OBJ1)
            (RETURN (SMSET LASTEVENT)))
        (COND ((GET PRONOUN 'BIND)
                (MAP #'(LAMBDA (BINDNODE) (SMIT2 BINDNODE 0)) (GET PRONOUN 'BIND))
                (RETURN SM))
            ((SMIT2 (GET PRONOUN 'LASTBIND) 0)
                (GO DONE))
            ((OR (MOVE-PT C U U (NG) U U (NG)) (MOVE-PT C U U (NG) U (COMP) PV (SUBJ)))
                (SMIT2 PT 0)
                (MOVE-PT C U U (NG))
                (COND ((ISQ PT DEF)
                    (ADD-F-PT 'INDEF PT)
                    (REMOVE-F-PT 'DEF PT)
                    (MAPC #'(LAMBDA (INTERP) (PUTPROP INTERP '((EXACTLY 1) INDEF NIL) 'DETERMINER=)) (SM PT))))
                (RETURN SM))
            ((OR (MOVE-PT C U (BOUND) U) (MOVE-PT C U (AND (ISQ PT CLAUSE) (ISQ PT COMPONENT)) U DLC))
                (SMIT2 (GETR 'OBJ2 PT) 0)
                (SMIT2 (GETR 'OBJ1 PT) 0)
                (SMIT2 (GETR 'SUBJECT PT) 0)
                (AND (NULL SM)
                    (ISQ PT RSQ)
                    (SMIT2 (GETR 'RELHEAD PT) 0))
                (AND SM (RETURN SM))))
        (SMIT2 (GETR 'SUBJECT LASTSENT) 192)
        (SMIT2 (PARSENODE? LASTREL) 128)                ;; TRY REL (I.E. QUESTION FOCUS) OF THE LAST SENTENCE
        (MOVE-PT LASTSENT DLC)
    UP  (COND ((NOT (MOVE-PT PV (NG))) (GO ON))
            (T (SMIT2 PT 64)))                       ;; GO THROUGH TOP LEVEL NG'S OF LAST SENTENCE
        (AND (MOVE-PT PV) (GO UP))
    ON  (OR SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE ANSREF (NG'S IN LAST ANSWER)
            (MAP #'(LAMBDA (ANSNODE) (SMIT2 ANSNODE 0)) ANSNAME))
        (OR SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE BACKREF2 (NG'S IN LAST SENTENCE) LIST
            (MAP #'(LAMBDA (BACKNODE) (SMIT2 BACKNODE 0)) BACKREF2))
    DONE (PUTPROP PRONOUN CANDIDATES 'BIND)
        (OR (CDR SM) (REMPROP (CAR SM) 'AMBIGUITIES=))
        (RETURN SM)))

(DEFUN SMIT2 (NODE PLAUSIBILITY)
    ;; MAKE SURE NODE IS REALLY THERE.
    ;; QUEST NODES (SUCH AS "WHAT") OR OTHER NODES WITHOUT HEAD NOUNS ARE NOT SUITABLE FOR REFERENTS.
    ;; MAKE SURE THAT NODE HASN'T ALREADY BEEN USED AS REFERENT.
    ;; MAKE SURE NODE AND PRONOUN AGREE IN NUMBER.
    (AND NODE
        (GETR 'HEAD NODE)
        (NOT (MEMQ (CAR NODE) CANDIDATES))
        (COND ((EQ PRONOUN 'IT)
            (AND (ISQ NODE NS) (NOT (ISQ NODE PRONG))))
            (T (ISQ NODE NPL)))
        (SETQ CANDIDATES (CONS (CAR NODE) CANDIDATES))
        (SMSET (NCONC
            (MAPCAR #'(LAMBDA (REFERENT-OSS)
                (BUILD
                    OSSNODE= (MAKESYM 'OSS)
                    MARKERS= (MARKERS? REFERENT-OSS)
                    SYSTEMS= (SYSTEMS? REFERENT-OSS)
                    PLAUSIBILITY= PLAUSIBILITY
                    AMBIGUITIES= (LIST (LIST OSSNODE= (FROM (NB NODE) (N NODE)) C))
                    REFER= (REFER? REFERENT-OSS)
                    VARIABLE= (VARIABLE? REFERENT-OSS)
                    PARSENODE= C ;; INPUT PARAMETER
                    ;; USE THE REFERENT'S REFERENT, IF IT HAS ONE.
                    DETERMINER= (LIST (COND ((ISQ C NPL) 'NPL) ('NS)) 'INDEF NIL)
                    ;; DONE SO THAT IF VARIBLE IS BOUND, PLANNER GENERATOR WILL USE IT.
                    ;; RELATION SAYS THAT THIS OSS "REFERS" TO THE OSS WHOSE VARIABLE NAME IS GIVEN.
                    RELATIONS= (LIST (LIST '#REFERS (VARIABLE? REFERENT-OSS)))))
                (SM NODE))
            SM))))

(DEFUN SMNGOF NIL
    ;; MAP DOWN THE LIST OF "OF" OBJECT INTERPRETATIONS.
    ;; USED TO PROCESS NOUN GROUPS LIKE "THREE OF THE BLOCKS", "BOTH OF THEM".
    ;; SINCE THE OBJECT OF THE "OF" MUST BE DEFINITE (SYNTACTICALLY) AND HAS ALREADY BEEN PROCESSED,
    ;; THE PLANNER CODE BUILT IS JUST A THAMONG EXPRESSION OF THE LIST
    ;; OF POSSIBLE REFERENTS OF THE "OF" OBJECT.
    (SMSET (MAPBLAND
        #'(LAMBDA (OFOSS)
            (BUILD
                OSSNODE= (MAKESYM 'OSS)
                VARIABLE= (VARIABLE? OFOSS)
                SYSTEMS= (SYSTEMS? OFOSS)
                MARKERS= (MARKERS? OFOSS)
                PARSENODE= C
                DETERMINER= (LIST (COND ((CQ NUM) (SM (MOVE-PT H PV (NUM)))) ((ISQ NB BOTH) 2) ('NPL))
                                (COND ((MOVE-PT H PV (QNTFR)) (EVAL (SM PT))) ('INDEF))
                                (COND ((CQ HOWMANY) 'HOWMANY) ((CQ QDET) 'WHICH)))
                RELATIONS= (LIST (LIST 'THAMONG (LIST 'THV (VARIABLE? OFOSS)) (LIST 'QUOTE (REFER? OFOSS))))))
        (SM (MOVE-PT H DLC)))))

;; ============================================================

(DEFUN SMNG1 NIL
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
    (PROG (WORD-BEING-INTERPRETED DETERS)
        (SETQ DETERS (LIST
            (COND
                ((CQ NUMD) ((LAMBDA (NUM) (EVAL (SM (MOVE-PT H PV (NUMD))))) (SM (MOVE-PT H PV (NUM)))))
                ((CQ NUM) (SM (MOVE-PT H PV (NUM))))
                ((CQ NPL) (COND ((ISQ NB BOTH) 2) ((CQ NS) 'SG-PL) ('NPL)))
                ('NS))
            (COND
                ((CQ QNTFR) (EVAL (SM (MOVE-PT H PV (QNTFR)))))
                ((CQ TPRON) (EVAL (SM (MOVE-PT H PV (TPRON)))))
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
        (SMSET (LIST (BUILD
            OSSNODE= (MAKESYM 'OSS)
            PARSENODE= C
            VARIABLE= (MAKESYM 'X)
            MARKERS= (AND (CQ TPRON) '(#VAGUE #PHYSOB #THING))
            RELATIONS= (AND (CQ TPRON) (LIST (LIST '#PHYSOB OSSNODE=)))
            DETERMINER= DETERS)))
        (SETQ WORD-BEING-INTERPRETED H)
        (COND ((ISQ H TPRON) (GO LOOP))
            ((CQ INCOM) (SMONE) (GO LOOP)))
        (SMSET (EVAL (SM WORD-BEING-INTERPRETED)))
    LOOP (COND ((NULL SM) (RETURN NIL)))
        (COND ((NULL (SETQ WORD-BEING-INTERPRETED (CDR WORD-BEING-INTERPRETED)))
                (RETURN SM))
            ((OR (ISQ WORD-BEING-INTERPRETED COMPAR) (ISQ WORD-BEING-INTERPRETED SUP))
                (EVAL (FINDMEASURE WORD-BEING-INTERPRETED))
                (GO LOOP))
            ((OR (ISQ WORD-BEING-INTERPRETED ADJ) (ISQ WORD-BEING-INTERPRETED CLASF))
                (SMADJ WORD-BEING-INTERPRETED)
                (GO LOOP))
            ((ISQ WORD-BEING-INTERPRETED POSS)
                (SMPOSS)
                (GO LOOP)))
        (GO LOOP)))

;; ============================================================

(DEFUN SMNG2 NIL
    ;; CALLED FROM NG WHEN ALL QUALIFIERS HAVE BEEN FOUND.
    ;; BASICALLY, IT SAVES THE NG ON THE BACKREF(ERENCE) LIST, AND CALLS SMNG3
    ;; (ON EACH POSSIBLE NG INTERPRETATION) TO EVAL ANY DEFINITE NOUN GROUPS, EG. "THE RED BLOCK".
    ;; AS USUAL, THE INITIAL OSS LIST IS IN "SM" AND THE FINAL OSS LIST IS PUT IN "SM".

    ;; DON'T USE FAKEY ANSWER NAME NODES FOR REFERENCE.
    ;; QUEST NODES ARE NOT SUITABLE REFERENTS.
    ;; SAVE THIS NG AWAY FOR POSSIBLE LATER BACK REFERENCE.
    ;; GO THRU ALL THE POSSIBLE INTERPRETATIONS OF THIS NOUN GROUP.
    (AND (NOT (CQ ANSNAME))
        (GETR 'HEAD C)
        DISCOURSE
        (SETQ BACKREF (CONS (CAR C) BACKREF)))
    (SMSET (MAPBLAND #'SMNG3 SM)))

(DEFUN SMNG3 (OSS)
    ;; TAKES AN OSS AS ARGUMENT AND TRIES TO FIND ITS REFERENCE IF THE NOUN GROUP IS DEFINITE.
    ;; EXPECT FOR SPECIAL "ONLY DEFINITE" DEFINITES SUCH AS "THE RIGHT" AND "THE THING".
    (PROG (FINDER MUNG INTER LIST CANDIDATES UNBOUND)
        (COND
            ((NOT (EQ (QUANTIFIER? OSS) 'DEF)) (RETURN OSS))        ;; IF ITS NOT DEFINITE OR IT
            ((REFER? OSS) (RETURN OSS))                             ;; ALREADY HAS A REFERENT
            ((CQ ANSNAME) (RETURN OSS)))                            ;; MARKED,  IF ITS KLUDGY
        (SETQ FINDER
            (PLNR-FINDIFY 'ALL                                      ;; ANSWER NAME, JUST RETURN IT
                (VARIABLE? OSS)                                     ;; JUST RETURN IT
                (LIST (VARIABLE? OSS))
                (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (LIST (VARIABLE? OSS))))) ;; BUILDS UP THFIND EXPRESSION
        (PUTPROP OSS FINDER 'PLNRCODE=)
        (SETQ WHO NIL)
    UP  (COND
            ((NOT (SETQ CANDIDATES (THVAL2 WHO FINDER))) (GO TOOFEW))
            ((NUMBERP (NUMBER? OSS)) (COND ((LESSP (LENGTH CANDIDATES) (NUMBER? OSS)) (GO TOOFEW)) ((GREATERP (LENGTH CANDIDATES) (NUMBER? OSS)) (GO TOOMANY))))
            ((EQ (NUMBER? OSS) 'NS) (COND ((NULL CANDIDATES) (GO TOOFEW)) ((CDR CANDIDATES) (GO TOOMANY))))
            ((MEMQ (NUMBER? OSS) '(NPL SG-PL)))
            ((ERT SMNG3= SCREWY NUMBER PROPERTY OF OSS)))
        (PUTPROP OSS CANDIDATES 'REFER=)
    DONE (RETURN OSS)

    TOOFEW ;; WE DIDN'T FIND ANY (OR ENOUGH) REFERENTS FOR THE NG
        (COND
            ((OR (NULL DISCOURSE) (NULL WHO))
                (SETQ GLOBAL-MESSAGE (APPEND '(I DON\'T KNOW WHAT YOU MEAN BY \") (FROM NB N) '(\"\.))) ;; "sic!
                (RETURN NIL))
            ;; IF WE AREN'T REMEMBERING SENTENCES, FORGET IT IF WE JUST TRIED TO
            ;; FIND EVERYTHING (OR EVERYTHING THAT "HE" KNOWS ABOUT), THEN FAIL
            ((MEMQ WHO '(HE NIL))
                (SETQ GLOBAL-MESSAGE (APPEND '(I DON\'T KNOW WHICH) (CDR (FROM NB N)) '(YOU MEAN\.)))
                (RETURN NIL)))
        (SETQ MUNG T)

    TOOMANY ;; ELSE SET UP TO EXPAND THE SENTENCES WE'RE LOOKING AT
        (AND (MEMQ WHO '(HE NIL))
            (SETQ FINDER (PLNR-MUNG FINDER CANDIDATES)))
        ;; RESTRICT THE POSSIBLE REFERENTS TO BE AMONG THE LIST ALREADY FOUND
        (SETQ WHO (COND
            ((EQ WHO NIL) 'HE)
            ((EQ WHO 'HE) (LIST (SUB1 LASTSENTNO) (ADD1 LASTSENTNO)))
            ((OR (NOT MUNG) (EQ (CAR WHO) 1)) (SETQ WHO 'HE) (GO TOOFEW))
            ((CONS (SUB1 (CAR WHO)) (CDR WHO)))))
        (SETQ MUNG NIL)
        (GO UP)))

(DEFUN SMONE NIL
    (PROG (CONTRAST X)
        (SETQ X H) ;; SET X TO DAUGHTERS OF CURRENT NODE
    GO  (COND ((SETQ CONTRAST (GET (ROOT (NB X)) 'CONTRAST))
                (SETQ CONTRAST (LIST CONTRAST (ROOT (NB X)))))
            ((SETQ X (CDR X)) (GO GO)))
    UP  (OR (AND (MOVE-PT C U U (NG)) (SMONE2 (LIST (CAR PT))))
            (SMONE2 (PARSENODE? LASTREL))
            (SMONE2 BACKREF)
            (SMONE2 ANSNAME)
            (SMONE2 BACKREF2)
            (COND (CONTRAST (SETQ CONTRAST NIL) (GO UP)))
            (AND (MOVE-PT LASTSENT DLC PV (NG)) (SMONE2 (LIST (CAR PT))))
            (ERT SMONE= CAN\'T FIND REFERENT FOR "ONE"))
        (RETURN SM)))

(DEFUN SMONE2 (X)
    ;; SMONE2 TAKES IN A LIST OF NOUN GROUP NODES AND TRIES TO SEE
    ;; IF ANY OF THOSE NOUN GROUPS COULD BE THE REFERENT FOR "ONE".
    (PROG (WORD-BEING-INTERPRETED)
        ;; IF X IS EMPTY, FAIL.
        ;; TRY TO SEE IF FIRST NG OF X SATIFIES CONTRAST AND/OR COULD BE REFERENT, ELSE TRY NEXT NG IN X
    UP  (COND ((NULL X) (RETURN NIL))
            ((SETQ WORD-BEING-INTERPRETED (SMONE3 X)))
            (T (SETQ X (CDR X)) (GO UP)))
        ;; AT THIS POINT WORD-BEING-INTERPRETED IS (SHOULD BE) A
        ;; LIST A WORD NODES OF THE NG WHICH IS THE REFERENT FOR
        ;; "ONE" WE NOW PROCEED TO BUILD UP AN OSS FOR THE "ONE"
        ;; NG THE LIST IS IN ORDER (NOUN ADJ ... ADJ ETC NUM DET)
        ;; ONLY THE NOUN AND THE ADJ'S ARE USED
        (OR (ISQ WORD-BEING-INTERPRETED NOUN)
            (BUG SMONE2: REFERENT OF \"ONE\" IS SCREWED UP)) ;; "sic!
        (EVAL (SM WORD-BEING-INTERPRETED))                          ;; EVAL THE NOUN DEFINITION
    GO  (AND
            (SETQ WORD-BEING-INTERPRETED (CDR WORD-BEING-INTERPRETED))
            (ISQ WORD-BEING-INTERPRETED ADJ)                        ;; IF WE REACHED END OF ADJECTIVES, STOP
            (EVAL (SM WORD-BEING-INTERPRETED))
            (GO GO))
        (RETURN SM)))

(DEFUN SMONE3 (ONENG)
    ;; SMONE3 TAKES AN NG WHICH IS A POSSIBLE REFERENT FOR "ONE".
    ;; IT FIRST CUTS THE NG TO BE ONLY (NOUN ADJ ... ADJ ETC) I.E.
    ;; IT STRIPS OF QUALIFYING PHRASES.  IF THERE IS NO CONTRAST,
    ;; THEN THIS MUNGED NG IS RETURNED AS THE REFERENT.  IF THERE
    ;; IS A CONTRAST, THEN IT CHECKS TO SEE IF THE NG SATISFIES
    ;; THAT CONTRAST.
    (PROG (NGWORDS X)
        (OR (ISQ ONENG NG)
            (BUG SMONE3: ONE REFERENT IS NOT A NG))
        (SETQ NGWORDS (H ONENG))
    LOOP (COND ((NULL NGWORDS) (RETURN NIL))                        ;; FAIL, IF NG HAS NO NOUN HEAD
            ((ISQ NGWORDS NOUN))                                    ;; IF FIND NOUN HEAD OF NG, WIN
            (T (SETQ NGWORDS (CDR NGWORDS)) (GO LOOP)))
        (OR CONTRAST (RETURN NGWORDS))                              ;; IF THERE IS NO CONTRAST, REFERENT WINS BY DEFAULT
        (SETQ X (REVERSE NGWORDS))
    LOOK (COND ((AND (EQ (CAR CONTRAST) (GET (ROOT (NB X)) 'CONTRAST)) (NOT (EQ (CADR CONTRAST) (ROOT (NB X)))))
                (RETURN (REVERSE (CDR X))))
            ((SETQ X (CDR X)) (GO LOOK))
            (T (RETURN NIL)))))                                  ;; FAIL, IF NO WORD SUPPLIES CONTRAST

(DEFUN SMPOSS NIL
    (PROG (X)
        (RETURN (AND (SETQ X (SMPOSS2 C (MOVE-PT H PV (POSS))))
            (SMRELATE X)))))

(DEFUN SMPOSS2 (HEADNODE MODNODE)
    (PROG (X SM SMSUB SMOB1 SMOB2 SMOBL SMCOMP RELLIST)
        (SETQ SMRUB (SM MODNODE))
        (SETQ RELLIST (RETQ AMOB1 (SM HEADNODE)))
        (SMSET '(#HAVE))
        (RETURN (AND SM
            (SETQ X (MAKESYM 'NODE))
            (PUTPROP X SM 'SEMANTICS)
            (LIST X)))))

;; SMPOSS WORKS BY ACTING LIKE SMCL1 AND SETTING UP AN RSS (HAVE X Y) .  NODE IS THE NODE OF THE POSSESSIVE
;; WHICH HAS ALREADY BEEN SEMANTICALLY PROCESSED.  ITS SM CONTAINS THE OSS'S FOR WHOSE DOING THE POSSESSING.
;; THE SM CURRENTLY ACTIVE IS THE THING BEING POSSESSED.

(DEFUN SMRELATE (NODE)
    ;; RELATES A DESCRIPTIVE RSS TO ANOTHER RSS OR OSS ADDING IT
    ;; TO THE LIST OF RELATIONS.  IT TAKES THE LIST OF SS IN SM,
    ;; AND REPLACES SM WITH THE NEW LIST OF MODIFIED SS'S.  THE
    ;; MODIFYING RSS'S HAVE TO HAVE ONE OF THE SM SS'S AS A REL
    ;; (WHICH SHOULD ALWAYS BE TRUE IF THEY WERE SET UP PROPERLY).
    ((LAMBDA (X) (AND X (SMSET X)))
        (MAPCAR #'(LAMBDA (RSS)
            (PROG (REL)
                (OR (MEMQ (SETQ REL (REL? RSS)) SM)
                    (ERTERR SMRELATE - TO WHOM?))
                (RETURN (BUILD
                    OSSNODE= (AND (OSS? REL) (MAKESYM 'OSS))
                    RSSNODE= (AND (RSS? REL) (MAKESYM 'RSS))
                    MARKERS= (OR (AND (RELMARKERS? RSS) (CAR (RELMARKERS? RSS))) (MARKERS? REL))
                    SYSTEMS= (OR (AND (RELMARKERS? RSS) (CADR (RELMARKERS? RSS))) (SYSTEMS? REL))
                    PLAUSIBILITY= (PLAUSIBILITY? RSS)
                    PARSENODE= (PARSENODE? REL)
                    AMBIGUITIES= (AMBIGUITIES? RSS)
                    VARIABLE= (VARIABLE? REL)
                    NEGATIVE= (NEGATIVE? REL)
                    DETERMINER= (DETERMINER? REL)
                    RELATIONS= (CONS RSS (RELATIONS? REL))
                    REL= (REL? REL)))))
            (SM NODE))))

;; -----------------------------------------------------

(DEFUN SMCL1 NIL
    (PROG (SMSUB SMOB1 SMOB2 SMOBL SMCOMP RELLIST)
        ;; SET UP GLOBAL VARIABLES WHICH CONSIST OF POINTERS TO THE SEMANTIC DEFINITIONS
        ;; OF THE VARIOUS NOUN-GROUPS (ALSO RSNG'S) REQUIRED BY THE TRANSITIVITY OF THE VERB
        (SETQ SMSUB (COND
            ((SETQ SMSUB (GETR 'LOGICAL-SUBJECT C)) (SM SMSUB))
            ((CQ IMPER) '(SHRDLU-OSS))
            ((NOT (CQ PASV)) (SM (OR (GETR 'SUBJECT C) (ERTERR SMCL1 -- NO SUBJECT))))
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
        (OR SMSUB (AND (MEET '(THERE ITRNS) FE) (GO CHECK)))
        (OR SMOB1 (AND (OR (CQ TRANS) (NOT (CQ CLAUSE))) (GO CHECK)))
        (OR (AND SMOB1 SMOB2) (AND (CQ TRANS2) (GO CHECK)))
        (OR (AND SMOB1 SMOBL) (AND (CQ TRANSL) (GO CHECK)))
        (OR SMCOMP (AND (CQ INT) (GO CHECK)))
        (GO REL)
    CHECK (ERT BUG: SMCL1 TRANSITIVITY)
    REL (SETQ RELLIST
            (SM (COND
                ((CQ RSQ) (GETR 'RELHEAD C))
                ((OR (CQ PREPG) (CQ ADJG)) (GETR 'LOGICAL-SUBJECT C))
                ((CQ QUEST) (GETR 'RELHEAD C)))))
        (AND (NOT RELLIST)
            (OR (CQ POLAR) (CQ DECLAR))
            (SETQ X (RELFIND C))
            ;; FIND RELATIVE ELEMENT FOR POLAR CLAUSES WHICH CONTAIN INDEFINITE.
            ;; APPLIES TO TOPLEVEL CLAUSES SINCE ONLY THEY CAN HAVE FEATURES POLAR OR DECLAR.
            (OR (EQUAL X SMSUB)
                (EQUAL X SMOB1)
                (EQUAL X SMOB2)
                (EQUAL X SMOBL)
                (EQUAL X SMCOMP)
                (ERTERR SMCL1 -- POLAR REL DOESN\'T MATCH))
            (SETQ RELLIST X))
        (SETQ TIME (GETR 'TIME (MOVE-PT C U (CLAUSE))))
        ;; THIS REFERS TO THE SEMANTIC SENSE OF THE VERB WHICH WILL PROBABLY VARY WITH ITS TRANSITIVITY.
        ;; THE VALUE THAT IS FINALLY DETERMINED REPRESENTS ALL POSSIBLE SENSES OF THE MEANING OF THE WORD
        ;; THAT ARE APPROPRIATE TO THE TRANSITIVITY FIGURED OUT BY THE SYNTACTIC PROGRAMS
        (SETQ SENSE-OF-VERB
            (COND
                ((CQ PREPG) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'HEAD C))))
                ((CQ ADJG) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'ADJGHEAD C))))
                ((CADR (ASSQ (CAR (MEET FE '(ITRNS TRANS INT TRANSL TRANS2 THERE ITRNSL))) (SM (SETQ WORD-BEING-INTERPRETED (GETR 'MVB C))))))))
        (SMSET (EVAL SENSE-OF-VERB))
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

(DEFUN SMCL2 NIL
    ;; THERE USED TO BE A CALL TO SMPREPREL AT THIS POINT, BUT IT
    ;; HAS GONE AWAY PENDING FURTHER THOUGHT.
    (MAP #'SMCL-MODIFIERS H))
    ;; AS IN SMCL1, WE NEED TO SCAN THE CONSTITUENTS OF
    ;; THE CLAUSE AND ALLOW THEM TO MAKE WHATEVER MODIFICATION ARE APPROPRIATE

(DEFUN SMCL-MODIFIERS (WORD-BEING-INTERPRETED)
    ;; AS IN CONSTITUENT, THIS PROCEDURE IS BASICLY ONE LARGE DISPATCH TABLE WHICH
    ;; ARRANGES THAT THE PROPER KIND OF PROCESSING HAPPEN TO THE APPROPRIATE CONSTITUENT.
    ;; SOME SHOULD BE IGNORED SINCE THEY HAVE ALREADY BEEN DEALT WITH AND OTHERS SHOULD
    ;; BE EVALUATED AS MODIFIERS OR FOR THEIR SIDE-EFFECTS.
    (COND ((NULL (GET WORD-BEING-INTERPRETED 'FEATURES)))
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
        (AND (COND ((ISQ WORD-BEING-INTERPRETED TIM))
            ((AND (CQ REL-NOT-FOUND)
                (ISQ WORD-BEING-INTERPRETED QUEST)
                (ISQ (H WORD-BEING-INTERPRETED) TIM1))
            (RQ REL-NOT-FOUND)
            (FQ TIMEQ)))
        (SMTIME)))
    ;; IN WHICH CASE IT WAS ALREADY PROCESSED MIGHT GO AWAY IN A FEW DAYS
    ;; BUG CHATCHER MIGHT WANT TO CHANGE THAT THE REST ARE HOOKS FOR WHEN
    ;; WE FIGURE OUT WHAT TO DO WITH THEM
    ((ISQ WORD-BEING-INTERPRETED PREPG)
        (OR (ISQ WORD-BEING-INTERPRETED AGENT)
            (ERT SMCL-MOD BADPREPG)))
    ((ISQ WORD-BEING-INTERPRETED QADJ)
        (OR (MEET FE '(LOBJ COMPQ))
            (EVAL (SM WORD-BEING-INTERPRETED))))
    ((ISQ WORD-BEING-INTERPRETED BOUND))
    ((ISQ WORD-BEING-INTERPRETED BINDER))
    ((ISQ WORD-BEING-INTERPRETED QUEST))
    ((ISQ WORD-BEING-INTERPRETED CLAUSE))
    ((ERT SMCL-MODIFIERS ATTEMPTED TO PROCESS AN UNEXPECTED TYPE OF CONSTITUENT))))

(DEFUN SMBIND NIL
    (PROG (TSS EVENT START END)
        ;; DOES THE SM HAVE MORE THAN ONE VALUE???
        (AND (CDR (SM H))
            (ERT I DON\'T KNOW WHAT TO DO WITH AMBIGUOUS BOUND CLAUSES))
        ;; DISPATCH TABLE TO MATCH THE APPROPRIATE ACTION WITH EACH BINDER.
        ;; MOVE TO THE FIRST WORD OF THE CLAUSE (TO THE BINDER) AND CHECK FOR THE FEATURE TIME
        ;; (MAYBE ALSO CHECK FOR THE SM BEING MARKED AS AN EVENT???)
        (COND ((ISQ (MOVE-PT H DF) TIME)
            (SETQ TSS (GETR 'TIME C))
            (OR (SETQ EVENT (FINDEVENTS (CAR (SM H))))
                (GLOBAL-ERR '(NO SUCH THING EVER HAPPENED)))
            (SETQ EVENT (CAR EVENT))
            (SETQ START (GET EVENT 'START))
            (SETQ END (GET EVENT 'END))
            (EVAL (SM PT))
            (RETURN T)))))

(DEFUN SMBINDER (START-EV END-EV)
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

(DEFUN ATOMIFY (X) (COND ((ATOM X) X) ((CDR X) X) (T (CAR X))))

(DEFUN ISTENSE (NODE ARG)
    ;; CHECKS VARIOUS THINGS ABOUT TENSE.
    (PROG (X)
        (OR (SETQ X (GETR 'TIME NODE)) (ERT ISTENSE -- NO TIME REGISTER))
        (OR (SETQ X (TENSE? X)) (ERT ISTENSE -- NO TENSE))
        (RETURN (COND
            ((EQ ARG 'PRESENT) (MEMBER X '((PRESENT) (PRESENT PRESENT))))
            ((EQ ARG 'FUTURE) (EQUAL X '(FUTURE)))
            ((EQ ARG 'MODAL) (EQUAL X '(MODAL)))
            ((EQ ARG 'IMPERF) (AND (CDR X) (EQ (CAR X) 'PRESENT)))
            (T (ERT ISTENSE -- FUNNY ARG))))))

(DEFUN IMPERF? (TSS)
    (AND (CDR (SETQ X (TENSE? X))) (EQ (CAR X) 'PRESENT)))

(DEFUN IMPERF NIL (ISTENSE C 'IMPERF))

(DEFUN BUILD FEXPR (%L)
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
    (PROG (%X NEGATIVE= REFER= PLNRCODE= MARKERS= SYSTEMS= TENSE= TSSNODE= RELMARKERS= ANSNODE= ACTION= ANSRSS= PLAUSIBILITY=
              DETERMINER= AND= OR= AMBIGUITIES= RELATIONS= VARIABLE= VARLIST= REL= RSSNODE= PARSENODE= OSSNODE= NODE= %PROPS)
        (SETQQCHECK T %L
            (SETQ %PROPS '(NEGATIVE= REFER= PLNRCODE= MARKERS= SYSTEMS= RELMARKERS= PLAUSIBILITY= DETERMINER= AND= OR= AMBIGUITIES=
                           RELATIONS= VARIABLE= VARLIST= REL= RSSNODE= PARSENODE= OSSNODE= TSSNODE= TENSE= ANSNODE= ANSRSS= ACTION=))
            'BUILD)
        (AND RSSNODE= (NOT MARKERS=) (SETQ MARKERS= '(#RELATION)))
        (AND MARKERS= (NOT SYSTEMS=) (SETQ %X (CHECK MARKERS= NIL NIL)) (SETQ MARKERS= (CAR %X)) (SETQ SYSTEMS= (CADR %X)))
        (SETQ NODE= (OR OSSNODE= RSSNODE= TSSNODE= ANSNODE= (ERT \.\.\.BUILD: NO NODE=)))
        (MAPC #'(LAMBDA (%PROP) (AND (SETQ %X (EVAL %PROP)) (PUTPROP NODE= %X %PROP))) %PROPS)
        (AND BUILD-SEE ((LAMBDA (DPSTOP) (DP NODE=)) SMN-STOP))
        (RETURN NODE=)))

(DEFUN NEWCOPY (OSS)
    (PROG (OLD NEW)
        (SETQ NEW (MAKESYM 'OSS))
        (SETQ OLD (CDR OSS))
        ;; WATCH OUT -- THIS IS IMPLEMENTATION DEPENDENT,
        ;; AND GETS THE ENTIRE PROPERTY LIST IN OUR LISP.
    =>  (COND
            ((NULL OLD) (PUTPROP NEW C 'PARSENODE=) (RETURN NEW))
            ((EQ (CAR OLD) 'PNAME))
            ((PUTPROP NEW (CADR OLD) (CAR OLD))))
        (SETQ OLD (CDDR OLD))
        (GO =>)))

(DEFUN RELATION FEXPR (%DEFL)
    ;; CONSTRUCTS RSS'S FOR GARDEN VARIETY VERBS.  USED IN DEFINITION OF SAME.
    ;;
    ;; INPUTS:
    ;;   INPUTS ARE IN KEYWORD FORMAT (ATTRIBUTE-VALUE PAIRS).
    ;;   THE KEYWORD IS NOT EVALUATED BUT ITS VALUE IS.
    ;;
    ;; POSSIBLE KEYWORDS:
    ;;   RESTRICTIONS:   LIST OF RESTRICTIONS ON VARIOUS SEMANTIC REGISTERS
    ;;                       EACH RESTRICTION (CALLED %MARL FOR MARKER LIST IN THE CODE) IS A LIST
    ;;                           EITHER WHOSE CAR IS A REGISTER NAME (E.G. SMSUB) AND WHOSE CADR IS A LIST OF MARKERS
    ;;                              OR WHOSE CAR IS A LIST OF MARKERS IN WHICH CASE
    ;;                                 THE ASSOCIATED REGISTER NAME IS DETERMINED BY THE POSITION IN RESTRICTIONS:
    ;;                                    SMSUB FOR CAR
    ;;                                    SMOB1 FOR CADR
    ;;                                    SMOB2 FOR CADDR
    ;;   PROCEDURE:      CONDENSED PLANNER CODE SCHEMA
    ;;   MARKERS:        SEMANTIC MARKERS
    ;;   PLAUSIBILITY:   EVALUATED TO GET INTEGER FROM 0 TO 512 INDICATING RELATIVE LIKLIHOOD OF THIS DEFINITION SENSE.
    ;;   REL:            ******>>>
    ;;
    ;; VALUE:
    ;;   LIST OF RSS NODE NAMES CREATED
    ;;
    ;; SIDE-EFFECTS:
    ;;   CREATES AN RSS BY CALLING BUILD
    (ITERATE
        #'(LAMBDA ARGLIST
            (PROG (SMCOMP SMSUB SMOB1 SMOB2 SMOBL MARKERS: RESTRICTIONS: PLAUSIBILITY: REL: PARAPHRASE: RELMARKERS: RSSNAME PROCEDURE: #1 #2 #3 %NEWRSS %OSSNODE)
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
                (SETQQCHECK NIL %DEF '(RESTRICTIONS: PROCEDURE: PLAUSIBILITY: PARAPHRASE: MARKERS:) 'RELATION)
                ;; (EVAL ...) DECODES KEYWORD ARGUMENTS.  SETQQ EFFECTIVLY QUOTES BOTH PAIRS
                ;; RESTRICTIONS: IS EXPANDED HERE PUTING IN IMPLICIT REGISTER REFERENCES SO THAT IT CAN BE UNIFORMLY GOBBLED BELOW
                (SETQ RESTRICTIONS:
                    ;; MARKL IS A SINGLE MARKER LIST FROM ON OF THE RESTRICTIONS IN THE DEFINITION, E.G. (#PHYSOB #RED).
                    ;; %RESTRICTNAM IS A NAME LIKE SMSUB, SMOBL, SMCOMP, .... WHICH REFERS TO REGISTERS ELSEWHERE
                    ;; IN THE PROGRAM WHOSE MARKERS MUST BE COMPATIBLE WITH %MARKL AS CHECKED BELOW.
                    ;; %NUM IS THE NUMBER WHICH WILL BE USED TO SUBSTITUTE IN THE DICTIONARY EXPRESSION.
                    (MAPCAR #'(LAMBDA (%RESTRICTNAM %MARKL %NUM)
                        ((LAMBDA (X) (SET %NUM (EVAL (CAR X))) X)
                            (COND ((ATOM (CAR %MARKL)) %MARKL) ((CONS %RESTRICTNAM %MARKL)))))
                        '(SMSUB SMOB1 SMOB2)
                        RESTRICTIONS:
                        '(#1 #2 #3)))
                (AND
                    ;; CHECK THAT THIS DEFINITION SENSE MEETS ALL OF THE RESTRICTIONS SET FORTH IN THE DEFINITION UNDER RESTRICTIONS:
                    (ERRSET
                        ;; ENCLOSED IN A ERRSET SO THAT THE FAILURE OF A CHECK CAN CAUSE IMMEDIATE ESCAPE FROM THE MAPC
                        ;; AND HENCE TO THE AND WHICH CUTS OFF ALL FURTHER PROCESSING OF THIS DEFINITION SENSE
                        ;; TEMPORARY STORAGE ON THE PROPERTY LIST OF TEMP USED TO AVOID SEARCHING FOR THESE ITEMS ON THE
                        ;; ONE HAND OR THE CONFLICT OF NAMES BETWEEN THE THE MARKERS RESULTING FROM CHECKING THE REL ARE
                        ;; SAVED TO PUT ON IT LATER WHEN THE CLAUSE IS RELATED.
                        (MAPC #'(LAMBDA (%MARKL)
                            (PROG (OSS X CHECK)
                                (SETQ OSS (EVAL (CAR %MARKL)))
                                (AND (SETQ X (CHECKREL OSS)) (SETQ REL: (CAR X)))
                                (COND
                                    ((NOT (AND (OR (NULL (CDDR %MARKL))
                                            (EVAL (CADDR %MARKL)))
                                            (SETQ CHECK
                                                (CHECK (CADR %MARKL)
                                                (MARKERS? OSS)
                                                (SYSTEMS? OSS)))))
                                        (ERR NIL))
                                    ((EQ OSS REL:)
                                        (SETQ RELMARKERS: CHECK)))))
                            RESTRICTIONS:))
                        ;; SUBJECT RESTRICTION MARKERS USED IN THE DEFINITION AND THE REGISTERS OF THE SAME NAME REFERENCED
                        ;; AS FREE VARIABLES IN THIS PROGRAM.  ON THE OTHER HAND, IF THE RESTRICTIONS HAVE BEEN MET, THEN
                        ;; BUILD AN RSS NODE
                    (SETQ %NEWRSS
                        ;; NEWRSS IS THE NEW RSS NODE NAME CREATED BY BUILD.  RSSNODE= IS KEYWORD FOR INPUT INTO BUILD
                        ;; OF RSS NODE NAME.  IN THE CALL TO BUILD THE ITEMS ENDING IN = ARE KEYWORDS WHOSE VALUE IS
                        ;; THE FOLLOWING ITEM.  MARKERS:, OF COURSE IS A VARIABLE IN THIS FUNCTION.  THIS CALL JUST SAYS
                        ;; SEND TO BUILD FOR MARKERS= THE VALUE SENT TO RELATION FOR MARKERS:  THE PARSENODE IS THE
                        ;; CURRENT NODE ***, #1, #2, AND #3 SUBSTITUTIONS DONE.  THIS IS FIRST STEP IN BUILDING PLANNER CODE.
                        ;; NUMSUB IS THE ****, #!, #", #3 SUBSTITUTION FUNCTION.
                        ;; %PLNRPHRASE IS ONE CHUNK OF CONDENSED PLANNER CODE LIKE (#COLOR *** #RED).
                        (BUILD
                            RSSNODE= (SETQ RSSNAME (MAKESYM 'RSS))
                            MARKERS= MARKERS:
                            VARIABLE= ((LAMBDA (X) (PUTPROP X X 'RSSVAR)) (MAKESYM 'EVX))
                            PARSENODE= C
                            RELATIONS= (REVERSE (MAPCAR #'(LAMBDA (%PLNRPHRASE) (PLNR-NUMSUB '<<<RELATION-ERROR>>> %PLNRPHRASE)) (EVALCHECK PROCEDURE:)))
                            REL= REL:
                            NEGATIVE= (AND (CQ NEG) T)
                            RELMARKERS= RELMARKERS:
                            PLAUSIBILITY= (PLUS (PLAUSIBILITY? SMSUB) (PLAUSIBILITY? SMOB1) (PLAUSIBILITY? SMOB2) (OR (EVAL PLAUSIBILITY:) 0))
                            AMBIGUITIES= (APPEND (AMBIGUITIES? #1) (AMBIGUITIES? #2) (AMBIGUITIES? #3)
                                                 (AND PARAPHRASE: (LIST (LIST RSSNAME PARAPHRASE: WORD-BEING-INTERPRETED)))))))
            (RETURN %NEWRSS)))
    %DEFL SMSUB SMOB1 SMOB2 SMOBL SMCOMP))

(DEFUN DOBACKREF (ANSWER)
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
    (PROG (ANSRSS)
        (SETQ ANSRSS (GET ANSWER 'ANSRSS=))
        (SETQ BACKREF2 BACKREF)
        (SETQ BACKREF NIL)
        (SETQ LASTREL (REL? ANSRSS))
        (MAPC #'(LAMBDA (PRONOUN) (PUTPROP PRONOUN (GET PRONOUN 'BIND) 'LASTBIND) (REMPROP PRONOUN 'BIND)) '(IT THEY ONE))
        (OR
            (CQ MODAL)
            (CQ DECLAR)
            (MAP #'(LAMBDA (BACKNODE)
                (COND ((CDR (SM BACKNODE)) ;; TRUE IF NODE HAD MULTIPLE INTERPRETATIONS
                    (PRINT (SM BACKNODE))
                    (SETR 'SEMANTICS (ERT DOBACKREF: RETURN AN OSS FOR BACKNODE) BACKNODE)))
            (COND
                ((REFER? (CAR (SM BACKNODE)))) ;; IF NODE HAS REFERENT, FINE
                (T
                    (PUTPROP (CAR (SM BACKNODE))
                        (OR (GET (VARIABLE? (CAR (SM BACKNODE))) 'BIND) (ERT DOBACKREF: RETURN REFERENT FOR BACKNODE))
                        'REFER=))))
            BACKREF2))
        ;; A FEW MISSING PIECES GO HERE
        ))

(DEFUN EVALCHECK (L)
    ;; EVALCHECK CHECKS FOR THE PRESENCE OF (#EVAL (MUMBLE ...) ...) IN THE INPUT S-EXPRESSION L.
    ;; IF IT FINDS ONE THEN THE EXPRESSION MUMBLE IS EVALUATED AND REPACES (#EVAL ...), OTHERWISE
    ;; L IS RETURNED JUST THE WAY IT WAS.  HENCE THIS FUNCTION IS THE INVERSE OF QUOTE.
    (COND ((ATOM L) L)
        ((EQ (CAR L) '#EVAL) (EVAL (CADR L)))
        (L)))

(DEFUN ITERATE FEXPR (%L)
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
    (PROG (%X %XL) (RETURN (EVAL (ITERATEX (CAR %L) (CDR %L))))))
    ;; THIS SHOULD BECOME A MACRO SO THAT ITERATE TAILORS A MAPPING FUNCTION
    ;; FOR EACH APPLICATION WHICH IS THEN COMPILED

(DEFUN ITERATEX (F L)
    ;; EXPANDS CALL ON ITERATE INTO A TAILORED SET OF MAPBLAND
    ;; CALLS WHICH DO THE APPROPRIATE COMPUTATION WHEN EVALUATED.
    ;; INPUTS:
    ;;      F      FUNCTION OF N ARGUMENTS
    ;;      L      LIST OF N LISTS WHICH F IS TO BE APPLIED TO
    ;; VALUE:
    ;;  TAILORED FUNCTION
    (COND ((NULL L) (CONS (EVAL F) %XL))
        ((LIST 'MAPBLAND
            (LIST 'FUNCTION (LIST 'LAMBDA
                ;; %X IS USED TO STORE THE VARIABLE NAME WHICH IT GETS FROM (GENSYM)
                ;; %XL IS USED TO SAVE A LIST OF ALL OF THE VARIABLE NAMES SO FAR SO
                ;; THAT THEY CAN BE GIVEN TO THE FUNCTION AT THE END (CONS %X NIL).
                ;; CREATES A SHINY NEW CELL WHICH IS BOLTED ONTO THE BACK END OF %XL
                ;; BY NCONC.  THIS PAIR IS NECESSARY BECAUSE SETQ RETURNS AS ITS VALUE
                ;; THE RESULT OF THE LAST PAIR THAT IT PROCESSES.  A RUSE. PUTS (NIL)
                ;; IN PLACE OF NIL AS A LIST TO PREVENT MAPBLAND FROM QUITTNG.
                (LIST (SETQ %X (GENSYM) %XL (NCONC %XL (CONS %X NIL)) %X %X))
                (ITERATEX F (CDR L))))
            (OR (CAR L) '(NIL))))))

(DEFUN MAPBLAND (FN L)
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
    (PROG (ANS F)
        (AND (NULL L) (SETQ L '(NIL)))
    A   (COND ((NULL (SETQ F ((EVAL 'FN) (CAR L)))))
            ((ATOM F) (SETQ ANS (NCONC ANS (CONS F NIL))))
            ((SETQ ANS (APPEND ANS F))))
        (SETQ L (CDR L))
        (AND L (GO A))
        (RETURN ANS)
        (GO A)))

(DEFUN MAPC2 (FN L)
    ;; MAPPING FUNCTION FOR GOING 2 AT A TIME THROUGH A LIST.
    ;; INPUTS:
    ;;     FN   -   FUNCTION OF TWO ARGUMENTS
    ;;     L    -   LIST (ESPECIALLY ATTRIBUTE VALUE TYPE LIST)
    ;; VALUE:
    ;;  LIST (LIKE MAPCAR) OF FN APPLIED TO TOP TWO ELEMENTS.
    (PROG (DUMMY)
        ;; DUMMY IS USED TO ESCAPE FROM A SYSTEM ERROR WHICH OCCURS WHEN NIL IS USED.
        ;; FN APPLIED TO TOP TWO ELEMENTS.  EVAL IS TO AVOID CONFLICT WITH FUNCTION
        ;; REALLY NAMED FN.  LIST IS STEPPED TWO AT A TIME.
    A   (COND ((NULL L) (RETURN T)))
        ((EVAL 'FN) (CAR L) (CADR L))
        (SETQ L (CDDR L))
        (GO A)))

(DEFUN MUMBLE (X)
    ;; MUMBLE IS THE PLANNER FILTER FOR LOOKING AT ASSERTIONS TO
    ;; SEE WHEN THEY WERE MENTIONED IN THE DIALOG. IT USES THE FREE
    ;; VARIABLE "WHO" TO DECIDE WHAT TO LOOK FOR. WHO IS EITHER NIL
    ;; (USE ANYTHING) "HE" (USE ANYTHING WHICH HAS BEEN MENTIONED)
    ;; OR A LIST OF TWO SENTENCE NUMBERS, MIN AND MAX (USE ANY WHO
    ;; PROPERTY WHICH IS ON OR BETWEEN THEM). THE WHO PROPERTY OF
    ;; ANY ASSERTION IS A SENTENCE NUMBER WHEN IT WAS MOST RECENTLY
    ;; MENTIONED.
    (COND ((NULL WHO))
        ((EQ WHO 'HE) (GET X 'WHO))
        ((SETQ X (GET X 'WHO)) (NOT (OR (LESSP X (CAR WHO)) (GREATERP X (CADR WHO)))))))

(DEFUN OBJECT FEXPR (%DEFL)
    ;; %DEFL IS THE LIST OF DEFINITION SENSES.
    ;; CONSTRUCTS OSS FOR GARDEN VARIETY NOUNS AND ADJECTIVES.
    ;; USED IN DEFINITIONS.
    ;;
    ;; INPUTS:
    ;;  INPUTS ARE IN KEYWORD FORMAT (ATTRIBUTE-VALUE PAIRS).
    ;;  THE KEYWORD IS NOT EVALUATED BUT ITS VLUE IS.
    ;; POSIBLE KEYWORDS:
    ;;     MARKERS:            LIST OF SEMANTIC MARKERS
    ;;     PLAUSIBILITY:       EVALS TO INTEGER FROM 0 TO 512 INDICATING RELATIVE LIKLIHOOD OF THIS DEFINITION SENSE
    ;;     DET:                *** I'M TOO LAZY TO LOOK THIS UP NOW FILL IN DET ***
    ;; FREE VARIABLE INPUT:
    ;;     SM  -  A LIST OF CURRENT OSS'S WITH WHICH THE TO-BE-CREATED ONES MUST BE COMPATIBLE.
    ;; VALUE:
    ;;     A NEW LIST OF OSS'S TO TAKE THE PLACE OF THE OLD ONE.
    ;; SIDE-EFFECTS:
    ;;  SM IS RESET TO THE VALUE OF OBJECT.
    ;;  A SET OF OSS'S ARE CREATED AT THE GLOBAL LEVEL.
    (PROG (%VARNAM)
        ;; PROG TO DECLARE VARIABLE.
        ;; IF SM IS EMPTY (AS IN THE CASE OF A HEAD), MAKE A NEW VARIABLE SYSMBOL,
        ;; OTHERWISE THE APPROPRIATE %VARNAM WILL BE DECIDED INSIDE THE ITERATE LOOP.
        (OR SM (SETQ %VARNAM (MAKESYM 'X)))
        (RETURN (SMSET
            ;; %OSS IS A SINGLE OSS NODE NAME PICKED OFF OF SM.
            ;; %DEF IS A SINGLE DEFINITION SENSE PICKED OFF OF %DEFL.
            (ITERATE #'(LAMBDA (%OSS %DEF)
                (PROG (%OSSNODE %CHKRESULT MARKERS: SYSTEMS: PLAUSIBILITY: DET: RELATIONS: PARAPHRASE: PROCEDURE:)
                    ;; DECODE KEYWORDS
                    (SETQQCHECK NIL %DEF '(MARKERS: PLAUSIBILITY: PARAPHRASE: PROCEDURE:) 'OBJECT)
                    ;; CHECK FOR MARKER AGREENT.  IF OK, THEN BUILD OSS, ELSE CHUCK THIS COMBINATION.
                    (AND
                        (SETQ %CHKRESULT (CHECK MARKERS: (MARKERS? %OSS) (SYSTEMS? %OSS)))
                        ;; BUILD OSS COMBINING INFORMATION FROM CURRENT OSS WITH INFORMATION IN THE DEFINITION.
                        ;; NOTE THAT THE INITIAL OSS WHICH GETS BUILT UP FOR A WORD DEPENDS NOT ONLY ON ITS DEFINITION,
                        ;; BUT ALSO ON THE CONTEXT IN WHICH IT IS USED.
                        (RETURN (BUILD
                            OSSNODE= (SETQ %OSSNODE (MAKESYM 'OSS))
                            PARSENODE= (PARSENODE? %OSS)
                            MARKERS= (CAR %CHKRESULT)
                            SYSTEMS= (CADR %CHKRESULT)
                            DETERMINER= (DETERMINER? %OSS)
                            VARIABLE= (VARIABLE? %OSS)
                            RELATIONS= (NCONC (REVERSE (MAPCAR #'(LAMBDA (%PLNRPHRASE) (PLNR-NUMSUB %OSS %PLNRPHRASE)) (EVALCHECK PROCEDURE:))) (RELATIONS? %OSS))
                            REL= (REL? %OSS)
                            ;; THE OSS NAME PROVIDES A UNIQUE LABEL FOR WHERE THE AMBIGUITY OCCURRED FOR LATER COMPARISON.
                            AMBIGUITIES= (APPEND (AMBIGUITIES? %OSS) (AND PARAPHRASE: (LIST (LIST %OSS PARAPHRASE: WORD-BEING-INTERPRETED))))
                            PLAUSIBILITY= (PLUS (OR (EVAL PLAUSIBILITY:) 0) (PLAUSIBILITY? %OSS)))))))
            SM %DEFL)))))

;; ######################################################
;;
;;              PLANNER BUILDING ROUTINES
;;
;; ######################################################

(DEFUN PLNR-JUNKIFY (CODE)
    ;; PUTS DISCOURSE STUFF INTO CODE
    (COND
        ((ATOM CODE) CODE)
        ((EQ (CAR CODE) 'THGOAL) (LIST 'THAND CODE '(VALUEPUT)))
        ((EQ (CAR CODE) 'THFIND) (LIST 'THAND CODE (LIST 'THPUTPROP (QUOTIFY (CADR (CADDR CODE))) 'THVALUE ''BIND)))
        ((OR (EQ (CAR CODE) 'THAND) (EQ (CAR CODE) 'THPROG)) (MAPCAN 'PLNR-JUNKIFY2 CODE))
        ((MAPCAR 'PLNR-JUNKIFY CODE))))

(DEFUN PLNR-JUNKIFY2 (CODE)
    ;; PUTS DISCOURSE STUFF INTO CODE
    (COND
        ((ATOM CODE) (LIST CODE))
        ((EQ (CAR CODE) 'THGOAL) (LIST CODE '(VALUEPUT)))
        ((EQ (CAR CODE) 'THFIND) (LIST CODE (LIST 'THPUTPROP (QUOTIFY (CADR (CADDR CODE))) 'THVALUE ''BIND)))
        ((OR (EQ (CAR CODE) 'THAND) (EQ (CAR CODE) 'THPROG)) (LIST (MAPCAN 'PLNR-JUNKIFY2 CODE)))
        ((LIST (MAPCAR 'PLNR-JUNKIFY CODE)))))

(DEFUN VALUEPUT NIL (PUTPROP THVALUE SENTNO 'WHO))

(DEFUN PLNR-THCONSIFY (VARLIST EXP BODY)
    ;; GENERATES A CONSEQUENT THEOREM.
    (PROG (TH)
        (SETQ TH (MAKESYM 'THEOREM))
        (PUTPROP TH
            (COND ((EQ (CAR BODY) 'THPROG)
                    (NCONC (LIST 'THCONSE (UNION VARLIST (CADR BODY)) EXP) (CDDR BODY)))
                (T (LIST 'THCONSE VARLIST EXP BODY)))
            'THEOREM)
        (RETURN TH)))

(DEFUN PLNR-FINDIFY (MODE VARIABLE VARLIST BODY)
    ;; GENERATES A THFIND STATEMENT FOR THE NOUN GROUP DESCRIBED IN THE OSS.
    ;; IT (CURRENTLY) ASSUMES THAT THE PLNRCODE PROPERTY OF THE OSS IS A LIST
    ;; OF PATERNS OF THGOAL STATEMENTS.  MODE IS DEFINED TO BE <MODE> IN THE
    ;; MICRO-PLANNER DESCRIPTION OF THFIND (SEE AI MEMO #203A) BODY
    ;; IS A SINGLE PLANNER EXPRESSION (POSSIBLY A THAND OR THPROG).
    (COND
        ((EQ (CAR BODY) 'THAND) (SETQ BODY (CDR BODY)))
        ((EQ (CAR BODY) 'THPROG) (SETQ VARLIST (APPEND VARLIST (CADR BODY))) (SETQ BODY (CDDR BODY)))
        ((SETQ BODY (LIST BODY))))
    ;; VARLIST = <SKELETON>
    ;; BODY = <VARIABLE DECLARATIONS>
    (NCONC (LIST 'THFIND MODE (PLNR-VAR VARIABLE) VARLIST) BODY))

(DEFUN PLNR-FINDSPEC (X)
    ;; GENERATES PAMETER FOR THFIND FROM THE NOTATION USED IN THE DETERMINER?
    (COND
        ((NUMBERP X) X)
        ((MEMQ X '(NS NPL SG-PL)) 1)
        ((EQ (CAR X) 'EXACTLY) (LIST (CADR X) (ADD1 (CADR X)) NIL))
        ((EQ (CAR X) '>) (ADD1 (CADR X)))
        ((EQ (CAR X) '<) (LIST 0 (CADR X) NIL))
        ((ERTERR PLNR-FINDSPEC -- FUNNY SPECIFICATION))))

(DEFUN PLNR-GOALIFY (PLNRPHRASE)
    ;; TAKES A PLNRPHRASE AND MAKES A THGOAL STATEMENT OUT OF IT UNLESS
    ;; IT ISN'T SUPPOSED TO BE ONE.  ALSO CALLS PLNR-NOTIFY IF APPROPRIATE.
    ;; PRESENT TENSE TIME MARKERS ARE REMOVED TO SIMPLIFY THE MOST COMMON EXPRESSIONS.
    (SETQ PLNRPHRASE (PLNR-REMTIME PLNRPHRASE))
    (COND ((GET (CAR PLNRPHRASE) 'NOGOAL) PLNRPHRASE)
        ((APPEND (LIST 'THGOAL PLNRPHRASE '(THDBF MUMBLE)) (PLNR-RECOMMENDIFY PLNRPHRASE)))))

(DEFUN PLNR-MUNG (FINDEXPR CANDIDATES)
    ;; DOES A HORRIBLE THING: MUNGS A THFIND EXPRESSION WHICH FINDS A NOUN GROUP REFERENCE.
    ;; IT PUTS A THAMONG EXPRESSION AS THE FIRST STATEMENT OF THE THFIND.  IF THERE IS ALREADY
    ;; A THAMONG EXPRESSION IN THE THFIND, THEN MUNG JUST CLOBBERS THE LIST IN THAT THAMONG.
    (CONS (CAR FINDEXPR)
        (CONS (CADR FINDEXPR)
            (CONS (CADDR FINDEXPR)
                (CONS (CADDDR FINDEXPR)
                    (CONS (LIST 'THAMONG (CADDR FINDEXPR) (QUOTIFY CANDIDATES))
                        (COND ((EQ (CAADDR (CDR FINDEXPR)) 'THFIND) (CDDDDR (CDR FINDEXPR)))
                            (T (CDDDDR FINDEXPR)))))))))

(DEFUN PLNR-NOTIFY (NEG? %PLNRPHRASE)
    ;; PUTS IN THNOT, BUT ELIMINATE DOUBLE NEGATIVES.
    (COND ((NOT NEG?) %PLNRPHRASE)
        ((EQ (CAR %PLNRPHRASE) 'THNOT) (CADR %PLNRPHRASE))
        ((LIST 'THNOT %PLNRPHRASE))))

(DEFUN PLNR-NEWBODY (X) (SETQ NEWBODY (CONS X NEWBODY)))

(DEFUN PLNR-PROGIFY (VARLIST BODY)
    ;; SETS UP A THPROG OR THE SIMPLEST EQUIVALENT EXPRESSION FOR
    ;; THE PARTICULAR CASE.  BODY IS A LIST OF EXPRESSIONS
    (PROG (NEWBODY)
        (OR BODY (RETURN NIL))
        (MAPC #'(LAMBDA (X) (COND
                ((EQ (CAR X) 'THPROG)
                    (COND ((MEET VARLIST (CADR X)) (PLNR-NEWBODY X))
                        (T (SETQ VARLIST (APPEND VARLIST (CADR X))) (MAPC 'PLNR-NEWBODY (CDDR X)))))
                ((EQ (CAR X) 'THAND)
                    (MAPC 'PLNR-NEWBODY (CDR X)))
                ((PLNR-NEWBODY X))))
            BODY)
        (RETURN (COND
            (VARLIST (CONS 'THPROG (CONS VARLIST (REVERSE NEWBODY))))
            ((CDR NEWBODY) (CONS 'THAND (REVERSE NEWBODY)))
            ((CAR NEWBODY))))))

(DEFUN PLNR-NUMREL (OSS)
    ;; THIS IS USED BY PLNR-NUMSUB TO HAVE THE VARIABLE NAME SUBSTITUTED
    ;; FOR THE OSS WHICH IS THE REL OF A PARTICULAR EXPRESSION.
    (COND ((MEMQ OSS RELLIST) (SETQ REL: OSS)) (OSS)))

(DEFUN PLNR-NUMSUB (%ME %PLNRPHRASE)
    ;; FUNCTION WHICH SUBSTITUTES THE PROPER PARSE TIME VARIABLES
    ;; FOR #1, #2, #3, AND *** IN THE PLANNER SHCEMAS FROM DICTIONARY DEFINITIONS.
    ;;
    ;; INPUTS:
    ;;     %ME         - OSS FOR CURRENT OBJECT
    ;;     %PLNRPHRASE - A PHRASE FROM THE DICTIONARY IN WHICH SUBSTITUTIONS ARE TO BE MADE
    ;; FREE VARIABLE INPUTS:
    ;;     #1, #2, #3, CORRESPOND TO POSITIONS IN THE RESTRICTION LIST OF THE DEFINITION.
    ;;                 EACH POINTS TO A SINGLE OSS OR NIL IF NOT APPLICABLE.
    ;;     SMSUB - ONE OSS FROM SMSUB REGISTER
    ;;     SMOB1 - ONE OSS FROM SMOB1 REGISTER
    ;;     SMOB2 - ONE OSS FROM SMOB2 REGISTER
    ;;     *TIME - CURRENT TIME
    ;; VALUE:
    ;;  THE CONDENSED PLANNER CODE AFTER THE SUBSTITUTIONS HAVE BEEN MADE.
    (MAPCAR #'(LAMBDA (%ELEMENT) ;; %ELEMENT IS AN ATOM OF THE PHRASE
            (COND
                ((MEMQ %ELEMENT '(#1 #2 #3)) (PLNR-NUMREL (EVAL %ELEMENT)))
                ((EQ %ELEMENT '***) %ME)
                ((EQ %ELEMENT '*TIME) TIME) ;; GETS THE CURRENT TIME
                (%ELEMENT)))
        (EVALCHECK %PLNRPHRASE)))

(DEFUN PLNR-RECOMMENDIFY (%PLNRPHRASE)
    ;; LOOKS UP IN THE DICTIONARY A RECOMMENDED THEOREM TO USE IN
    ;; PROCESSING A PLNRPHRASE BY THGOAL.  IF IT FINDS ONE IT TACKS
    ;; IT ON AS A RECOMENDATION.
    (PROG (%ELEMENT)
        ;; LOOK A RELATION UP IN THE DICTIONARY.  THE ENTRIES ARE SET UP AS A PROPERTY LIST.
        ;; THERE ARE DIFFERENT RECOMMENDATIONS FOR THE SAME RELATION DEPENDING ON THE NUMBER
        ;; OF ARGUMENTS THIS INSTANCE OF IT HAS.  (LENGTH ...) COMPUTES THE NUMBER OF ARGUMENTS
        ;; + 1 AND THE (ASSQ ...) RETRIEVES THE APPROPRIATE RECOMMENDATION USING THIS NUMBER.
        ;; IF THERE IS NO SUCH NUMBER, NUMBERP FAILS AND SOME ARBITARY FUNCTION WHICH
        ;; IS STORED OUT THERE IS EVALUATED TO GIVE THE RECOMMENDATION.
        (RETURN
            (AND (SETQ %ELEMENT (GET (CAR %PLNRPHRASE) 'THMLIST))
                (EVAL (COND
                    ((NUMBERP (CAAR %ELEMENT)) (CADR (OR (ASSQ (LENGTH %PLNRPHRASE) %ELEMENT) '(NIL NIL))))
                    (%ELEMENT)))))))

(DEFUN PLNR-REMTIME (EXP)
    ;; REMOVES ALL PRESENT TENSE TIME STRUCTURES
    ((LAMBDA (Y)
        ;; Y IS BOUND TO A UNIQUE POINTER SO IT CAN'T POSSIBLY SCREW ANYTHING
        ;; IN THE EXPRESSION WHEN IT DOES THE DELETION.  DELQ USES EQ.
        (DELQ Y
            (MAPCAR #'(LAMBDA (X) (COND
                ((NOT (ATOM X)) X)
                ((TSS? X) (COND ((AND (TENSE? X) (NOT (MEMBER (TENSE? X) '((PRESENT PRESENT) (MODAL) (PRESENT))))) X) (Y)))
                (X)))
            EXP)))
        '(T)))

(DEFUN PLNR-VAR (X)
    ;; GENERATES SYNTAX FOR VARIABLE NAMES IN PLANNER
    (LIST 'THV X))

(DEFUN COMPARE-BUILD (NODE DEGREE)
    ;; USED BY SMADJG-PREPG TO BUILD A PSUEDO VERB FOR THE COMPARATIVE.  SMCL1 IS THEN CALLED.
    (PROG (RESTRICTIONS: DIMENSION: DIRECTION:)
        ;; THESE ARE THE POSSIBLE PARTS OF A MEASURE.
        (SETQQCHECK NIL (CDR (FINDMEASURE NODE)) '(RESTRICTIONS: DIMENSION: DIRECTION:) 'MEASURE)
        ;; DEFINITION
        (PUTPROP 'COMPARE-PSEUDO-VERB
            (LIST 'RELATION
            (LIST 'RESTRICTIONS:
                (LIST (LIST RESTRICTIONS:)
                (LIST RESTRICTIONS:))
                'PROCEDURE: (LIST (LIST DEGREE DIMENSION: (COND (DIRECTION: '#1) ('#2)) (COND (DIRECTION: '#2) ('#1))))))
            'SEMANTICS)
        (RETURN '(COMPARE-PSEUDO-VERB))))

(DEFUN FINDMEASURE (NODE)
    ;; GETS THE MEASURE DEFINITION
    (COND
        ((SETQ X (ASSOC 'MEASURE (GET (ROOT (NB NODE)) 'SEMANTICS))) (CADR X))
        ((GLOBAL-ERR (APPEND '(I DON\'T KNOW HOW TO COMPARE THINGS WITH RESPECT TO) (LIST (ROOT (NB NODE))))))))

(DEFUN MEASURE FEXPR (MEAS)
    ;; USED TO GENERATE ORDINALS -- IT IS CALLED WHEN A MEASURE DEFINITION IS EVALLED
    (APPLY 'OBJECT
        (LIST (LIST 'MARKERS:
            (CADR (MEMQ 'RESTRICTIONS: MEAS))
            'PROCEDURE:
            (LIST (LIST '*ORDINAL* MEAS))))))

(DEFUN PLNR-DESCRIBE (EXPS VAR FREEVARS)
    ;; BUILDS THE PLANNER DESCRIPTION, IGNORING THE QUANTIFIER ACOUNTS FOR ORDINALS, SUBSTS, ETC.
    (PROG (ORDINAL BODY X)
    =>  (COND
            ((NULL EXPS) (RETURN (COND (ORDINAL (ORDMAKE ORDINAL VAR BODY)) ((PLNR-PROGIFY NIL BODY)))))
            ((EQ (SETQ X (EXPAND (CAR EXPS) (AND (NULL (CDR EXPS)) (RSSVAR? VAR) (GET VAR 'USED) (PLNR-VAR VAR)))) '*ORDINAL*))
            ;; A SUBST DEFINITION IF IT IS THE ONLY THING IS TO BE APPLIED TO THE OSS TO WHICH THIS RSS WILL BE RELATED.
            ;; THE VARIABLE FOR A RELATION IS INSERTED INTO THE SECOND PLACE OF THE RELATION IF IT IS REFERRED TO ANYWHERE ELSE.
            ((AND (CDR EXPS) (EQ (CAR X) '#SUBST)) (MAPC2 #'(LAMBDA (X Y) (SETQ EXPS (SUBST X Y EXPS))) (CDR X)))
            (X (SETQ BODY (CONS X BODY))))
        (SETQ EXPS (CDR EXPS))
        (GO =>)))

(DEFUN RELFIND (NODE)
    ;; LOOKS FOR THE REL OF A POLAR
    (PROG (REL)
        (ERRSET
            ;; IT GOESFROM THE BEGINNINGOF THE SENTENCE LOOKING FOR AN INDEFINITE NG,
            ;; EITHER AT THE TOP LEVEL OR AS A FIRST LEVEL PREPOBJ, BUT NOT A COMPLEMENT.
            (MAP #'(LAMBDA (X) (COND
                ((ISQ X NG) (AND (NOT (ISQ X COMP)) (NOT (ISQ X DEF)) (SETQ REL X) (ERR NIL)))
                ((ISQ X LOBJ) (AND (ISQ (H X) INDEF) (SETQ REL X) (ERR NIL)))
                ((ISQ X PREPG) (AND (ISQ (H X) INDEF) (SETQ REL (H X)) (ERR NIL)))))
            (REVERSE (H NODE))))
        (OR REL (AND (CQ PASV) (NOT (CQ AGENT)) (SETQ REL '(FAKE-AGENT))))
        (RETURN (AND REL (SM REL)))))

(DEFUN ORDMAKE (ORDINAL VAR BODY)
    ;; MAKES THE LOGICAL FORM FOR SUPERLATIVES.
    ;; ORDINAL GIVES THE THING BEING COMPARED IN MEASURE FORM.
    (PROG (NEWVAR)
        (SETQ NEWVAR (MAKESYM 'X))
        (RETURN (PLNR-PROGIFY NIL
            (APPEND BODY (LIST (PLNR-NOTIFY T
                (PLNR-PROGIFY (LIST NEWVAR) (APPEND (SUBST NEWVAR VAR BODY) (LIST (PLNR-GOALIFY (COMPARE-PROC VAR NEWVAR ORDINAL))))))))))))

(DEFUN COMPARE-PROC (VAR NEWVAR ORDINAL)
    (PROG (RESTRICTIONS: DIRECTION: DIMENSION:)
        (SETQQCHECK NIL ORDINAL '(RESTRICTIONS: DIRECTION: DIMENSION:) 'MEASURE)
        (RETURN (LIST '#MORE DIMENSION: (PLNR-VAR (COND (DIRECTION: NEWVAR) (VAR))) (PLNR-VAR (COND (DIRECTION: VAR) (NEWVAR)))))))

(DEFUN EXPAND (EXP EVENT)
    ;; THE HEART OF THE PLANNER BUILDER.
    ;; EXPANDS AN EXPRESSION WORRYING ABOUT THE QUANTIFIERS AND CONNECTIVES OF ITS CONSTITUENTS.
    ;; IT DOESN'T REALLY HANDLE EXPRESSIONS WITH MORE THAN ONE QUANTIFIED SS UNLESS ONE OF THEM IS THE REL.
    ;; THE EVENT IS NON-NIL ONLY IF THE EVENT-NAME IS TO BE INCLUDED IN THE EXPANSION OF THE EXPRESSION.
    (COND
        ((RSS? EXP)
            (COND
                ((AND? EXP) (PLNR-PROGIFY NIL (MAPCAR #'(LAMBDA (X) (EXPAND X NIL)) (AND? EXP))))
                ((OR? EXP) (PLNR-ORIFY (MAPCAR #'(LAMBDA (X) (EXPAND X NIL)) (OR? EXP))))
                ((PLNR-NOTIFY (NEGATIVE? EXP) (PLNR-DESCRIBE (RELATIONS? EXP) (VARIABLE? EXP) (CONS (VARIABLE? EXP) FREEVARS))))))
        ((ATOM EXP) (BUG EXPAND - ATOMIC MODIFIER))
        ((EQ (CAR EXP) '*ORDINAL*)
            (COND (ORDINAL (GLOBAL-ERR '(I CAN\'T HANDLE TWO ORDINALS OR SUPERLATIVES AT ONCE)))
                ((SETQ ORDINAL (CADR EXP)) '*ORDINAL*)))
        ((EQ (CAR EXP) '#SUBST)
            (ERT EXPAND - IS #SUBST BEING HANDLED BY SOMEONE ELSE?)
            EXP)
        ((PROG (BODY QUANTIFIER CHOICE VAR MULTIPLE)
            (SETQ MULTIPLE (EVAL (GET (CAR EXP) 'MULTIPLE)))
            (SETQ EXP
                (MAPCAR #'(LAMBDA (X) (COND
                    ((OR (NOT (ATOM X)) (NOT (OR (RSS? X) (OSS? X))))
                        X)
                    ((REFER? X)
                        (COND ((CDR (REFER? X))
                            (COND (MULTIPLE (ERQSET 'AND) (SETQ CHOICE (REFER? X)) '*AND*)
                                ((REFER? X))))
                        ((CAR (REFER? X)))))
                    ((MEMQ (VARIABLE? X) FREEVARS)
                        (AND (RSSVAR? (VARIABLE? X)) (PUTPROP (VARIABLE? X) T 'USED))
                        (PLNR-VAR (VARIABLE? X)))
                    ((SETQ CHOICE (AND? X))
                        (ERQSET 'AND)
                        (AND MULTIPLE (REFER? X) (SETQ CHOICE (REFER? X)))
                        '*AND*)
                    ((SETQ CHOICE (OR? X))
                        (ERQSET 'OR)
                        '*OR*)
                    ((COND
                        ((RSS? X) (ERQSET 'EVENT) (PUTPROP (VARIABLE? X) T 'USED))
                        ((MEMQ (QUANTIFIER? X)
                            '(ALL NO)) (ERQSET (QUANTIFIER? X)) T)
                        ((MEMQ (QUANTIFIER? X) '(NDET INDEF))
                            (COND ((MEMQ (NUMBER? X) '(NS SG-PL)) (ERQSET 'INDEF)) ((SETQ CHOICE (PLNR-FINDSPEC (NUMBER? X))) (ERQSET 'FIND))) T))
                        (SETQ BODY (PLNR-DESCRIBE (RELATIONS? X) (VARIABLE? X) (CONS (VARIABLE? X) FREEVARS)))
                        (PLNR-VAR (SETQ VAR (VARIABLE? X))))
                    ((ERTERR EXPAND - STRANGE QUANTIFIER))))
                    (COND (EVENT (CONS (CAR EXP) (CONS EVENT (CDR EXP)))) (T EXP))))
            ;; THE EVENT NAME IS STUCK INTO THE SECOND POSITION IF THERE IS ONE.
            (RETURN (COND
                ((NULL QUANTIFIER) (PLNR-GOALIFY EXP))
                ((EQ QUANTIFIER 'AND)
                    (PLNR-PROGIFY NIL (MAPCAR #'(LAMBDA (X) (EXPAND (SUBST X '*AND* EXP) NIL)) CHOICE)))
                ((EQ QUANTIFIER 'OR)
                    (PLNR-ORIFY (MAPCAR #'(LAMBDA (X) (EXPAND (SUBST X '*OR* EXP) NIL)) CHOICE)))
                ((EQ QUANTIFIER 'FIND)
                    (PLNR-FINDIFY CHOICE VAR (LIST VAR) (PLNR-PROGIFY NIL (CONS BODY (LIST (PLNR-GOALIFY EXP))))))
                (T
                    (PLNR-NOTIFY (MEMQ QUANTIFIER '(ALL NO))
                        (PLNR-PROGIFY (AND VAR (LIST VAR)) (CONS BODY (LIST (PLNR-NOTIFY (EQ QUANTIFIER 'ALL) (PLNR-GOALIFY EXP)))))))))))))

(DEFUN ERQSET (X)
    ;; USED BY EXPAND TO MAKE SURE IT ISN'T GETTING CONFUSED BY TOO
    ;; MANY CONNECTIVES AND QUANTIFIERS IN THE SAME EXPRESSION.
    (COND (QUANTIFIER (GLOBAL-ERR '(I CAN\'T HANDLE COMBINATIONS OF QUANTIFIERS AND CONNECTIVES WHICH ARE SO COMPLICATED)))
        ((SETQ QUANTIFIER X))))

(DEFUN SETQQCHECK (%EVALFLAG %LIST %CHECKLIST %NAME)
    ;; SETQQCHECK IS LIKE SETQQ (OR LIKE SETQ DEPENDING ON EVALFLAG) BUT IT CHECKS TO MAKE SURE
    ;; THE VARIABLE NAME IS A MEMBER OF THE %CHECKLIST, AND IF NOT PRINTS AN ERROR MESSAGE.
    (PROG (%X)
    GO  (COND ((NULL %LIST) (RETURN T))
            ((MEMQ (CAR %LIST) %CHECKLIST)
                (SET (CAR %LIST) (COND (%EVALFLAG (EVAL (CADR %LIST))) (T (CADR %LIST))))
                (SETQ %LIST (CDDR %LIST))
                (GO GO))
            (T (SETQ %X (APPLY 'ERT (CONS (CAR %LIST) (APPEND '(IS NOT A LEGAL SPECIFICATION FOR) (LIST %NAME)))))))
    UP  (COND
            ;; A QUESTION MARK GETS THE LIST OF POSSIBILITIES PRINTED OUT, THEN LETS YOU TRY AGAIN.
            ;; TO DO THIS YOU MUST TYPE (RETURN '?) AT THE ERT.
            ;; IF YOU RETURN ANY OTHER VALUE, IT ASSUMES THIS IS THE VARIABLE NAME INTENDED,
            ;; OTHERWISE IT JUST CAUSES AN ERROR.
            ((EQ %X '?)
                (PRINT %CHECKLIST)
                (SETQ %X (ERT FOO: SETQQCHECK ????))
                (GO UP))
            ((SETQ %LIST (CONS %X (CDR %LIST))) (GO GO)))))

(DEFUN THVAL2 (WHO AA)
    (PROG (RESULT X MPLNR-TTIME M-GC)
        (SETQ THLEVEL '(T))
        (SETQ X (SETQ RESULT '(NIL)))
        (AND PLANNERSEE (DISP AA) PLNRSEE-PAUSE (ERT FOR PLANNER))
        (AND (NOT (EQ RESULT X))
            (RETURN RESULT))
        (SETQ MPLNR-TTIME (RUNTIME) M-GC (STATUS GCTIME))
        (SETQ RESULT (THVAL AA '((EV COMMAND))))
        (SETQ MPLNR-TIME (TIMER MPLNR-TTIME (RUNTIME)))
        (OR (== M-GC (STATUS GCTIME))
            (SETQ MPLNR-TIME (DIFFERENCE MPLNR-TIME (TIMER M-GC (STATUS GCTIME))) GC (STATUS GCTIME)))
        (RETURN RESULT)))

(DEFUN WHO (X)
    (COND ((NULL WHO))
        ((ATOM X))
        ((NOT (SETQ X (GET X 'WHO))) NIL)
        ((EQ WHO 'HE))
        ((LESSP (CAR WHO) X LASTSENTNO))))

(DEFUN CHECK (NEW-MARKERS MARKERS SYSTEMS)
    ;; TAKES A LIST OF NEW MARKERS AND CHECKS FOR COMPATIBILITY
    ;; WITH THE EXISTING MARKERS AND SYSTEMS (AS GIVEN BY ARGS
    ;; MARKERS AND SYSTEMS).  IF COMPATIBLE, RETURNS A TWO-LIST OF
    ;; THE NEW MARKERS AND SYSTEMS, ELSE RETURNS NIL
    (PROG NIL
    =>  (COND
            ((NULL NEW-MARKERS) (RETURN (LIST MARKERS SYSTEMS)))
            ((CHECKAMARKER (CAR NEW-MARKERS)) (SETQ NEW-MARKERS (CDR NEW-MARKERS)) (GO =>))
            (T (RETURN NIL))))) ;; FAIL IF CHECKAMARKER FAILS

(DEFUN CHECKAMARKER (MARKER)
    ;; CHECKS A SINGLE MARKER FOR COMPATIBILITY
    ;; USES FREE VARIABLES:
    ;;    SYSTEMS - THE SYSTEM LIST SO FAR
    ;;    MARKERS - THE MARKER LIST SO FAR
    ;; IF SUCCESSFULL, THE MARKER AND ITS SYSTEM(S) ARE APPENDED TO THESE FREE VARIBLES
    (PROG (NEW-SYSTEMS)
        (COND ((MEMQ MARKER MARKERS) (RETURN T)))               ;; IF MARKER ALREADY THERE, FINE
        (SETQ MARKERS (CONS MARKER MARKERS))                    ;; ADD NEW MARKER TO LIST
        (SETQ NEW-SYSTEMS (GET MARKER 'SYS))                    ;; GET THE SYSTEMS OF THE NEW MARKER
    =>  (COND ((NULL NEW-SYSTEMS) (RETURN T))
                ((MEMQ (CAR NEW-SYSTEMS) SYSTEMS) (RETURN NIL)) ;; FAIL IF SYSTEM THERE BY ANOTHER PATH
            ((CHECKAMARKER (CAR NEW-SYSTEMS))
                (SETQ SYSTEMS (CONS (CAR NEW-SYSTEMS) SYSTEMS))
                (SETQ NEW-SYSTEMS (CDR NEW-SYSTEMS))
                (GO =>))
            (T (RETURN NIL)))))

(DEFUN FINDEVENTS (RSS)
    ;; FINDS ALL THE EVENTS FITTING THE RSS DESCRIPTION
    (PUTPROP (VARIABLE? RSS) T 'USED)
    (THVAL2 NIL
        (PLNR-FINDIFY 'ALL
            (VARIABLE? RSS)
            (LIST (VARIABLE? RSS))
            (PLNR-DESCRIBE (RELATIONS? RSS)
                (VARIABLE? RSS)
                (LIST (VARIABLE? RSS))))))

(DEFUN CHECKREL (OSS)
    ;; CHECKS FOR POSSIBLE RELATIVE, EITHER BECAUSE OSS IS ON THE RELLIST,
    ;; OR BECAUSE RSS INVOLVES INSIDE IT AN OSS ON THE RELLIST.
    ;; IT RETURNS EITHER NIL OR A LIST OF WHICH THE FIRST ELEMENT IS THE REAL RELATIVE.
    ;; IT USES THIS FACT TO CHEAT ON RECURSION BY USING MAPCAN.
    (COND
        ((OSS? OSS) (MEMQ OSS RELLIST))
        ((RSS? OSS) (MAPCAN #'(LAMBDA (RELATION) (COND ((ATOM RELATION) NIL) ((MAPCAN 'CHECKREL RELATION)))) (RELATIONS? OSS)))))

#_(ns shrdlu.smass)

;; ################################################################
;;
;;               SMASS - SEMANTIC ACCESS FUNCTIONS
;;
;; ################################################################

(DEFUN ACTION? (X)
    ;; THE WORKING PART OF AN ANSWER -- TELLS WHAT TO DO IF THE ANSWER IS THE ONE TO BE GIVEN.
    ;; MIGHT INCLUDE ACTIONS ON THE DISPLAY, AS WELL AS THINGS TO BE PRINTED AND VARIABLES TO BE SET, ETC.
    ;; THE WHOLE THING IS EVAL-LISTED.
    (GET X 'ACTION=))

(DEFUN AMBIGUITIES? (X)
    ;; LIST OF POSSIBLE AMBIGUITIES FOR A SEMANTIC STRUCTURE.
    (GET X 'AMBIGUITIES=))

(DEFUN AND? (X)
    ;; FIND THE CONJUNCTION LIST OF A CONJOINED SEMANTIC STRUCTURE.
    ;; NIL IF THERE IS NONE.
    (GET X 'AND=))

(DEFUN ANSRSS? (X)
    ;; THE RSS CORRESPONDING TO AN ANSWER NODE (A PROPERTY OF THE ANSNODE)
    (GET X 'ANSRSS=))

(DEFUN DETERMINER? (X)
    ;; ACCESS FUNCTION.  GETS DET OF AN OSS.
    (GET X 'DETERMINER=))

(DEFUN END? (TSS)
    ;; END TIME FOR TSS.
    (GET TSS 'END=))

(DEFUN MARKERS? (%SENSE)
    ;; ACCESS FUNCTION USED TO GET MARKERS FROM OSS OR RSS.
    (GET %SENSE 'MARKERS=))

(DEFUN MODIFIERS? (%XSS)
    ;; ACCESS FUNCTION FOR GETTING THE PLANNER CODE SCHEMA OF AN OSS OR RSS.
    (GET %XSS 'MODIFIERS=))

(DEFUN NEGATIVE? (%XSS)
    ;; ACCESS FUNCTION FOR OSS.
    (GET %XSS 'NEGATIVE=))

(DEFUN NUMBER? (OSS)
    ;; GETS THE NUMBER FIELD OF AN OSS.
    (CAR (GET OSS 'DETERMINER=)))

(DEFUN OR? (X)
    ;; ACCESS FOR LIST OF CONSTITUENTS IN DISJOINED SEMANTIC STRUCTURE.
    ;; NIL IF IT ISN'T.
    (GET X 'OR=))

(DEFUN OSS? (X)
    ;; CHECKS TO SEE IF X IS AN OSS.
    (GET X 'OSSNODE=))

(DEFUN PARENT? (NODE) (GETR 'PARENT NODE))

(DEFUN PARSENODE? (X) (GET X 'PARSENODE=))

;; THE PARSE NODE ASSOCIATED WITH THE SEMANTIC STRUCTURE.

(DEFUN PLAUSIBILITY? (%XSS)
    ;; ACCESS FUNCTION FOR GETTING PLAUSIBILITY OF AN OSS OR RSS.
    (OR (GET %XSS 'PLAUSIBILITY=) 0))

(DEFUN PLNRCODE? (X) (GET X 'PLNRCODE=))

;; THE PLANNERCODE GENERATED WHEN AN OBJECT IS ACTUALLY LOOKED FOR IN THE DATA BASE.
;; IT IS NOT USED AGAIN, BUT IS LEFT SITTING AROUND FOR PEOPLE TO LOOK AT.

(DEFUN QTYPE? (X) (CADDR (GET X 'DETERMINER=)))

;; QUESTION TYPE FOR QUESTION OSS.

(DEFUN QUANTIFIER? (OSS)
    ;; GETS THE DETERMINER FIELD OF AN OSS.
    (CADR (GET OSS 'DETERMINER=)))

(DEFUN REFER? (%XSS)
    ;; ACCESS FUNCTION FOR REFER OF OSS OR RSS.
    (GET %XSS 'REFER=))

(DEFUN REL? (X) (GET X 'REL=))

;; THE OBJECT TO WHICH THIS DESCRIPTION IS TO BE RELATED.

(DEFUN RELATIONS? (X)
    ;; THE MATERIAL THAT WILL BECOME PLANNER CODE.
    (GET X 'RELATIONS=))

(DEFUN RELMARKERS? (X)
    ;; MARKERS HELD BY A RELATIVE CLAUSE PRODUCED BY ITS SELECTION
    ;; RESTRICTIONS, TO BE ATTACHED TO THE OBJECT DESCRIPTION.
    (GET X 'RELMARKERS=))

(DEFUN RSS? (X)
    ;; CHECKS TO SEE IF X IS AN RSS.
    (GET X 'RSSNODE=))

(DEFUN RSSVAR? (X)
    ;; A VARIABLE OF THE TYPE USED FOR RSS'S -- I.E. EVX3, ETC.
    (GET X 'RSSVAR))

(DEFUN START? (TSS)
    ;; START TIME FOR TSS.
    (GET TSS 'START=))

(DEFUN SYSTEMS? (%SENSE)
    ;; ACCESS FUNCTION FOR GETTING THE SYSTEMS OF AN OSS OR RSS.
    (GET %SENSE 'SYSTEMS=))

(DEFUN TENSE? (X)
    ;; FOR A TSS.
    (GET X 'TENSE=))

(DEFUN TSS? (X) (GET X 'TSSNODE=))

;; ASSOCIATED WITH EVERY TIME SEMANTIC STRUCTURE.

(DEFUN VARIABLE? (X)
    ;; ACCESS FUNCTION FOR GETTING THE VARIABLE NAME ASSOCIATED WITH AN OSS OR RSS.
    (GET X 'VARIABLE=))

(DEFUN SMSET (X) (SETR 'SEMANTICS X C) (SETQ SM X))

#_(ns shrdlu.newans)

;; ################################################################
;;
;;              NEWANS - (NEW) ANSWERING COMPONENT
;;
;; ################################################################

(DEFUN ANSWER (NODE)
    ;; THE TOP LEVEL ANSWER FUNCTION CALLED TO CARRY OUT THE RESULTS OF ANY INPUT SENTENCE,
    ;; WHETHER COMMAND, QUESTION, OR STATEMENT.
    (PROG (ANSLIST AMBIG)
        ;; ANSLIST IS THE LIST OF POSSIBLE ANSWERS.
        ;; AMBIG IS A FLAG SET IF THERE IS A POSSIBLE AMBIGUITY.
        ;; CLEAR OUT ANSWER NAMES SAVED FOR BACKREF(ERENCE), I.E. MORE THAN ONE RSS FOR THE SENTENCE.
        (SETQ ANSNAME NIL)
        (SETQ AMBIG (CDR (SM NODE)))
        (SETQ ANSLIST (ANSORDER (ANSUNIQUE (MAPCAR 'ANSGEN (SM NODE)))))
        ;; ANSGEN GENERATES AN ANSWER FOR EACH INTERPRETATION.
        ;; ANSUNIQUE TAKES OUT REDUNDANT ONES IN THE CASE THAT DIFFERENT INTERPRETATIONS LEAD TO THE SAME ANSWER.
        ;; ANSORDER ORDERS THE REMAINING ONES BY PLAUSIBILITY.
        ;; IF NO ANSWER IS CLEARLY BEST, ASK THE USER FOR CLARIFICATION AND TRY AGAIN.
    CHOOSE (COND
            ((AND (CDR ANSLIST) (NOT (ENOUGH-BETTER (CAR ANSLIST) (CADR ANSLIST))))
                (SETQ ANSLIST (ANSELIMINATE ANSLIST))
                (GO CHOOSE)))
            (OR ANNOYANCE (PRINT *3))
    TEST-LOOP
        (AND ANS-AFTERFORMULATION-PAUSE  (ERT ANSWER HAS BEEN DETERMINED))
        ;; THE ACTION INCLUDES BOTH THE THINGS TO BE DONE
        ;; AND THE INSTRUCTIONS FOR PRINTING A RESPONSE.
        (EVLIS (ACTION? (CAR ANSLIST)))
        (PRINC '\.)
        (TERPRI)
        (AND ANS-TEST? (GO TEST-LOOP))
        ;; DOBACKREF STORES AWAY DISCOURSE INFORMATION.
        (DOBACKREF (CAR ANSLIST))
        (RETURN T)))

(DEFUN AMBPUT (CODE)
    ;; PUTS IN THE JUNK FOR DISCOURSE IF THERE IS NO AMBIGUITY, SO THERE IS
    ;; NO NEED TO EVALUATE THE CODE A SECOND TIME WHEN GIVING THE ANSWER.
    (COND (AMBIG CODE) (T (PLNR-JUNKIFY CODE))))

(DEFUN ANSAY (X)
    ;; GENERATES THE SYNTAX FOR ANSWER ACTIONS FROM A PHRASE.
    (LIST (CONS 'SAY X)))

(DEFUN ANSBUILD (PLAUS ACTION REDEDUCE)
    ;; BUILDS AN ANSWER NODE.
    ;; IF REDEDUCE IS NON-NIL, IT ADDS A REDEDUCTION OF THE ANSWER,
    ;; ADDING THE DISCOURSE JUNK TO THE ACTION.
    (BUILD
        ANSNODE= (MAKESYM 'ANS)
        PLAUSIBILITY= PLAUS
        ANSRSS= RSS
        ACTION= (APPEND
                    (COND
                        ((AND AMBIG REDEDUCE (NOT (CQ DECLAR)))
                            (CONS (LIST 'THVAL2 NIL (LIST 'PLNR-JUNKIFY (LIST 'PLNRCODE? (LIST 'QUOTE RSS)))) ACTION))
                        (T ACTION))
                    (AND (REL? RSS) (NOT (CQ DECLAR)) (LIST (LIST 'PUTPROP (QUOTIFY (REL? RSS)) (QUOTIFY ANS) (QUOTIFY 'REFER=)))))))

(DEFUN ANSCOMMAND (RSS)
    ;; ANSCOMMAND RESPONDS TO IMPERATIVES.
    (PROG (EXP ANS SUCCESS PLAN PLAN2)
        ;; PLNR-ANDORIFY COMBINES ANDS AND ORS INTO APPROPRIATE PLANNER THANDS AND THORS.
        (SETQ EXP (PLNR-ANDORIFY RSS))
        (PUTPROP RSS EXP 'PLNRCODE=)
        (SETQ EXP (AMBPUT EXP))
        (SETQ EXP (COND
            ((EQ (CAR EXP) 'THAND) (APPEND EXP '((SETQ SUCCESS T) (SETQ PLAN2 PLAN))))
            (T (LIST 'THAND EXP '(SETQ SUCCESS T) '(SETQ PLAN2 PLAN)))))
        ;; IN CASE OF MULTIPLE INTERPRETATION, THE SYSTEM USES FAILURE TO WIPE OUT THE EFFECTS OF TRYING OUT ONE OF THEM.
        ;; BEFORE FAILING IT MARKS DOWN WHETHER IT SUCCEEDED AND SAVES THE PLAN FROM BACKTRACKING.
        ;; PLNR-JUNKIFY PUTS ON THE JUNK FOR SAVING THE DISCOURSE REFERENTS ETC.
        (THVAL2 NIL (COND (AMBIG (APPEND EXP '((THFAIL)))) (T EXP)))
        (RETURN
            ;; THE THIRD ARGUMENT TO ANSBUILD CAUSES THE SYSTEM TO GO BACK THROUGH THE DEDUCTION
            ;; TO GET THE DATA BASE STRAIGHT IF THIS ANSWER IS PICKED.  IT ALSO TAKES CARE OF THE BACKREF STUFF.
            (ANSBUILD
                (COND (SUCCESS (PLAUSIBILITY? RSS)) (T (DIFFERENCE (PLAUSIBILITY? RSS) 512)))
                (COND (SUCCESS (APPEND (REVERSE PLAN2) '((SAY OK)))) (T '((SAY I CAN\'T))))
                T))))

(DEFUN ANSDECLARE (RSS)
    ;; FOR DECLARATIVES.
    (COND
        ((OR? RSS)
            (GLOBAL-ERR I DON\'T UNDERSTAND DISJUNCTIVE DECLARATIVES))
        ((AND? RSS)
            (PROG (ANS)
                ;; CONJOINED DECLARATIVES ARE HANDLED BY DOING EACH ONE SEPARATELY.
                (SETQ ANS (MAPCAR 'ANSDECLARE (AND? RSS)))
                (RETURN (ANSBUILD
                    (APPLY 'PLUS (MAPCAR 'PLAUSIBILITY? ANS))
                    (CONS '(SAY I UNDERSTAND) (MAPCAN #'(LAMBDA (X) (DELETE '(SAY I UNDERSTAND) (ACTION? X))) ANS))
                    NIL))))
        ((NOT (ISTENSE (PARSENODE? RSS) 'PRESENT))
            (GLOBAL-ERR I ONLY UNDERSTAND PRESENT TENSE DECLARATIVES))
        (T (ANSBUILD
            (PLAUSIBILITY? RSS)
            ;; ANSTHM GENERATES THE APPROPRIATE ASSERTION OR THEOREM.
            (CONS '(SAY I UNDERSTAND) (MAPCAR #'(LAMBDA (X) (LIST 'THADD (QUOTIFY (ANSTHM X)) NIL)) (RELATIONS? RSS)))
            NIL))))

(DEFUN ANSELIMINATE (ANSLIST)
    ;; ELIMINATES ANSWERS FROM LIST BY ASKING PERSON TO CLEAR UP THE AMBIGUITIES.
    (PROG (AMB POSSIBILITIES XX)
        (OR (SETQ AMB (AMBIGUITIES? (ANSRSS? (CAR ANSLIST))))
            (BUG ANSELIMINATE -- NO AMBIGUITIES LIST))
        ;; POSSIBILITIES IS THE LIST OF POSSIBLE INTERPRETATIONS FOR A SINGLE AMBIGUITY.
        ;; WE ARE INSIDE A LOOP STARTING AT UP WHICH GOES THROUGH ALL THE DIFFERENT POSSIBLE AMBIGUITIES ON THE LIST FOR THE FIRST ANSWER ON ANSLIST.
        ;; ON EACH ANSWER WE LOOK FOR POSSIBLE INTERPRETATIONS FOR THE PARTICULAR NODE WHERE THE AMBIGUITY WAS CREATED.
    UP  (SETQ POSSIBILITIES (LIST (CAR AMB)))
        (MAPC #'(LAMBDA (ANS)
            (AND (SETQ XX (PARSE-ASSOC (CAAR AMB) (AMBIGUITIES? (ANSRSS? ANS))))
                (NOT (MEMBER XX POSSIBILITIES))
                (SETQ POSSIBILITIES (CONS XX POSSIBILITIES))))
            (CDR ANSLIST))
        (COND ((CDR POSSIBILITIES) T)
            ((SETQ AMB (CDR AMB)) (GO UP))
            (T (BUG ANSELIMINATE -- NO CONFLICT)))
        (TERPRI)
        (SAY I\'M NOT SURE WHAT YOU MEAN BY \") ;; "sic!
        (MAPC 'PRINT2 (FROM (NB (CADDAR AMB)) (N (CADDAR AMB))))
        (SAY \" IN THE PHRASE \") ;; "sic!
        (MAPC 'PRINT2 (FROM (NB (SETQ XX (PARENT? (CADDAR AMB)))) (N XX)))
        (PRINC '\"\.)
        (TERPRI)
        (SAY DO YOU MEAN:)
        (SETQ XX 0)
        (MAPC #'(LAMBDA (POSS)
                (PRINT (SETQ XX (ADD1 XX)))
                (MAPC 'PRINT2 (CADR POSS))) ;; THE PARAPHRASE
            POSSIBILITIES)
        (PRINC '?)
        (TERPRI)
    READ (SETQ XX (READ))
        (COND ((OR (NOT (NUMBERP XX)) (GREATERP XX (LENGTH POSSIBILITIES)))
            (TERPRI)
            (SAY PLEASE TYPE ONE OF THE NUMBERS)
            (TERPRI)
            (GO READ)))
        (SETQ POSSIBILITIES (NTH XX POSSIBILITIES))
        (RETURN
            (MAPBLAND #'(LAMBDA (ANS)
                (COND ((OR (NOT (SETQ XX (PARSE-ASSOC (CAAR AMB) (AMBIGUITIES? (ANSRSS? ANS))))) (EQUAL XX POSSIBILITIES)) ANS)))
            ANSLIST))))

(DEFUN PARSE-ASSOC (OSS AMBIG-LIST)
    ;; PARSE-ASSOC GOES THRU AMBIG-LIST LOOKING FOR AN INTERPRETATION WITH THE SAME PARSE NODE
    (PROG (ASS)
        (SETQ ASS (CAR (PARSENODE? OSS)))
    =>  (COND ((NULL AMBIG-LIST) (RETURN NIL))
            ((EQ ASS (CAR (PARSENODE? (CAAR AMBIG-LIST))))
                (RETURN (CAR AMBIG-LIST))))
        (SETQ AMBIG-LIST (CDR AMBIG-LIST))
        (GO =>)))

(DEFUN ANSGEN (RSS)
    ;; ANSGEN GENERATES AN ANSWER FOR A SINGLE INTERPRETATION.
    (COND
        ((OR (CQ IMPER) (AND (CQ QUEST) (ISTENSE (PARSENODE? RSS) 'FUTURE))) ;; FUTURE QUESTIONS ARE TREATED LIKE COMMANDS.
            (ANSCOMMAND RSS))
        ((CQ DECLAR)
            (PROG (X)
                (RETURN (COND
                    ((ERRSET (SETQ X (ANSDECLARE RSS))) X)
                    ;; THIS STRANGE CONSTRUCTION ALLOWS US A SECOND CHANCE ON DECLARATIVES ABOUT THINGS WHICH CAN'T
                    ;; BE TOLD TO THE SYSTEM.  IF IT RUNS INTO ONE OF THEM, IT TRIES TO ANSWER IT AS A QUESTION.
                    ((EQUAL GLOBAL-MESSAGE '(THAT ISN\'T THE KIND OF THING I CAN BE TOLD)) (ANSQUEST RSS))
                    ((ERR NIL))))))
        ((CQ QUEST) (ANSQUEST RSS))
        ((BUG ANSGEN -- WHAT KIND OF SENTENCE IS THIS?))))

(DEFUN ANSNAME (PHRASE)
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
    (PROG (ANSNODE C N CUT)
        (SETQ N (CDAAR PHRASE))                                     ;; CDR IS TO REMOVE "SAY"
        (SETQ ANSNODE (PARSE2 '(NG ANSNAME) T))                     ;; THE T SAYS NOT TO ATTACH THIS TO THE TREE
        (OR ANSNODE
            (RETURN (ERT ANSNAME: FAILURE TO PARSE ANSWER NAME BUT IF YOU ONLY EXPECT THE ANSWER TO BE AN ADJ, PROCEED THIS AND DON 'T WORRY)))
        (SETQ ANSNAME (APPEND ANSNODE ANSNAME))                     ;; LEAVE NODE AROUND IT ACCESSABLE PLACE
        (PUTPROP (CAR (SM ANSNODE)) (CADR PHRASE) 'REFER=)))        ;; PUT THE REFERENT ON AS THE GUY GIVEN BY ANSWER

(DEFUN ANSNOREL (RSS)
    ;; FOR QUESTIONS WITH NO RELATIVE, LIKE "DID YOU PICK UP THE BLOCK?" OR "WHY DID YOU DO THAT?"
    (PROG (ANS TYPE CODE NODE VAR)
        (SETQ NODE (PARSENODE? RSS))
        (SETQ TYPE (COND ;; THE TYPE SHOULD BE POLAR, WHY, WHERE, WHEN, OR HOW.
            ((ISQ NODE POLAR) 'POLAR)
            ((SETQ TYPE (GETR 'QADJ NODE)) (CAR (NB TYPE)))
            ((BUG ANSNOREL -- FUNNY TYPE))))
        (PUTPROP (VARIABLE? RSS) T 'USED)
        (SETQ CODE
            (PLNR-DESCRIBE (RELATIONS? RSS)
                ;; IN PRESENT TENSE CASES, WE DON'T LOOK FOR EVENTS.
                ;; OTHERWISE WE LOOK FOR A SET OF APPROPRIATE EVENTS NO MATTER WHAT THE TYPE.
                (COND ((ISTENSE NODE 'PRESENT) NIL) ((SETQ VAR (VARIABLE? RSS))))
                (LIST (VARIABLE? RSS))))
        (PUTPROP RSS CODE 'PLNRCODE=)
        (RETURN (COND
            ((NOT VAR)
                (SETQ ANS (THVAL-MULT (AMBPUT CODE)))
                (ANSBUILD
                    (PLUS (CAR ANS) (PLAUSIBILITY? RSS))
                    (COND ((CADR ANS) '((SAY YES))) ((ISTENSE NODE 'MODAL) '((SAY I DON\'T KNOW))) (T '((SAY NO))))
                    T))
            ((SETQ ANS (THVAL-MULT (PLNR-FINDIFY 'ALL VAR (LIST VAR) (AMBPUT CODE))))
                (ANSBUILD
                    ;; AN ANSWER IS VERY IMPLAUSIBILE IF IT MENTIONS AN EVENT THE SYSTEM CAN'T FIND.
                    (COND ((CADR ANS) (PLUS (PLAUSIBILITY? RSS) (CAR ANS))) (T (DIFFERENCE (PLAUSIBILITY? RSS) 512)))
                    (COND ((NULL (CADR ANS)) '((SAY I CAN\'T DISCUSS A NON-EXISTENT EVENT)))
                        ((APPEND (AND (EQ TYPE 'POLAR) '((SAY YES))) (LIST (LIST 'EVLIS (LIST 'DESCRIBEVENT (QUOTIFY (CADR ANS)) (QUOTIFY TYPE)))))))
                    T))))))

(DEFUN ANSORDER (LIST)
    ;; ORDERS A LIST BY PLAUSIBILITY HIGHEST FIRST.
    (PROG (X Y)
    GO  (SETQ X LIST)
    UP  (COND ((NULL (CDR X)) (RETURN LIST))
            ((LESSP (PLAUSIBILITY? (CAR X)) (PLAUSIBILITY? (CADR X)))
                (SETQ Y (CAR X))
                (RPLACA X (CADR X))
                (RPLACA (CDR X) Y)
                (GO GO))
            ((SETQ X (CDR X)) (GO UP)))))

(DEFUN ANSQUEST (RSS)
    ;; ANSQUEST ANSWERS ALL TYPES OF QUESTIONS BY SENDING THEM OUT
    ;; TO ANSREL OR ANSNOREL DEPENDING ON WHETHER THERE IS A REL.
    (COND
        ((OR (OR? RSS) (AND? RSS))
            (PROG (ANS)
                (SETQ ANS (MAPCAR 'ANSQUEST (OR (AND? RSS) (OR? RSS))))
                (RETURN (ANSBUILD
                    (APPLY 'PLUS (MAPCAR 'PLAUSIBILITY? ANS))
                    (APPEND
                        (AND (NOT (ISQ (PARSENODE? RSS) COMPONENT)) '((SAY YOU\'RE TRYING TO CONFUSE ME\.)))
                        (MAPCAN #'(LAMBDA (QUEST)
                            (APPEND '((TERPRI))
                                (ANSAY (ELIZA (FROM (NB (PARSENODE? (ANSRSS? QUEST))) (N (PARSENODE? (ANSRSS? QUEST))))))
                                '((PRINC '?) (TERPRI))
                                ;; CONJOINED QUESTIONS ARE HANDLED BY SIMPLY REPEATING EACH PART AND ANSWERING IT SEPARATELY.
                                (ACTION? QUEST)))
                        ANS))
                    NIL))))
        ((REL? RSS) (ANSREL RSS))
        (T (ANSNOREL RSS))))

(DEFUN ANSREL (RSS)
    ;; ANSREL HANDLES ALL QUESTIONS WITH A RELATIVE NG OF ANY TYPE.
    (PROG (TYPE REL CODE PLAUS ANS PHRASE LENGTH NUM)
        (OR (SETQ REL (REL? RSS)) (BUG ANSREL -- NO REL))
        (SETQ PHRASE (CONS 'NIL (HEADPART (PARSENODE? REL))))
        ;; THIS IS FOR THE PART OF THE GENERATOR THAT WILL SUBSITUTE "ONE" FOR NOUN NAMES.
        ;; THE LEADING NIL IS TO MAKE THIS PHRASE COMPATIBLE WITH THE "SAY" PHRASES WHICH THE OTHER PARTS GENERATE.
        ;; UNIVERSALS ARE CONVERTED TO NOT THERE EXISTS NOT.
        (SETQ TYPE (OR (QTYPE? REL) (QUANTIFIER? REL) (BUG ANSREL -- NO TYPE)))
        (AND (EQ TYPE 'ALL) (PUTPROP RSS T 'NEGATIVE=))
        (PUTPROP RSS
            (SETQ CODE
                (PLNR-FINDIFY 'ALL (VARIABLE? REL) (LIST (VARIABLE? REL)) (PLNR-DESCRIBE (CONS RSS (RELATIONS? REL)) (VARIABLE? REL) (LIST (VARIABLE? REL)))))
            'PLNRCODE=)
        ;; CONSING THE RSS ONTO THE THINGS TO BE DESCRIBED HAS THE EFFECT OF PUTTING THE RELATION INTO THE DESCRIPTION OF THE OBJECT.
        ;; DISAMB PUTS IN THE JUNK IF THERE IS NO AMBIGUIT, AVOIDING HAVING TO GO THROUGH THE EVALUATION A SECOND TIME.
        ;; THVAL-MULT RETURNS A LIST OF A PLAUSIBILITY AND AN ANSWER.
        (SETQ ANS (THVAL-MULT (AMBPUT CODE)))
        (SETQ PLAUS (CAR ANS))
        (SETQ LENGTH (LENGTH (SETQ ANS (CADR ANS))))
        (RETURN (COND
            ((EQ TYPE 'ALL)
                (ANSBUILD
                    (PLUS PLAUS (PLAUSIBILITY? RSS))
                    (COND ((NULL ANS) '((SAY YES))) ((CONS '(SAY NO, NOT) (PREPPUT (NAMELIST PHRASE 'INDEF ANS)))))
                    T))
            ((EQ TYPE 'HOWMANY)
                (ANSBUILD
                    (PLUS PLAUS (PLAUSIBILITY? RSS))
                    (PREPPUT (NAMESUGAR LENGTH REL))
                    T))
            ((MEMQ TYPE '(WHICH WHAT))
                (ANSBUILD
                    (PLUS PLAUS (PLAUSIBILITY? RSS) (COND (ANS 512) (0)))
                    (PREPPUT (NAMELIST PHRASE 'DEF ANS))
                    T))
            ((EQ TYPE 'INDEF)
                (SETQ NUM (NUMBER? REL))
                (ANSBUILD
                    (PLUS PLAUS (PLAUSIBILITY? RSS))
                    (COND
                        ((MEMQ NUM '(NS SG-PL))
                            (COND
                                ((NULL ANS) (COND ((ISTENSE (PARSENODE? RSS) 'MODAL) '((SAY I DON\'T KNOW))) (T '((SAY NO)))))
                                (T (APPEND '((SAY YES,)) (COND
                                    ((ISTENSE (PARSENODE? RSS) 'MODAL) NIL)
                                    ((PREPPUT (APPEND (AND (CDR ANS) (APPEND (NAMESUGAR LENGTH REL) '((PRINC ':)))) (NAMELIST PHRASE 'INDEF ANS)))))))))
                        ((NUMBERP NUM)
                            ;; THIS IS THE CASE WHERE WE ARE CAGEY AND AVOID ANSWERING YES OR NO.
                            ;; THE NUMBER ISN'T REPEATED IF IT IS THE SAME AS THE NUMBER IN THE SPECIFICATION.
                            (APPEND
                                (COND
                                    ((EQ NUM LENGTH) '((SAY YES,)))
                                    ((GREATERP LENGTH NUM) NIL)
                                    ((ZEROP NUM) '((SAY NO,)))
                                    (T '((SAY NO, ONLY))))
                                (COND
                                    ((EQ NUM LENGTH) NIL)
                                    (T (PREPPUT (APPEND (NAMESUGAR LENGTH REL) '((PRINC ':))))))
                                (PREPPUT (NAMELIST PHRASE 'INDEF ANS))))
                        ((EQ (CAR NUM) 'EXACTLY)
                            (COND ((EQ LENGTH NUM) '((SAY YES)))
                                (T (CONS '(SAY NO,) (PREPPUT (NAMESUGAR LENGTH RES))))))
                        ((EQ (CAR NUM) '>)
                            (CONS (COND
                                    ((GREATERP LENGTH NUM) '(SAY YES,))
                                    ((ZEROP LENGTH) '(SAY NO,))
                                    (T '(SAY NO, ONLY)))
                                (PREPPUT (NAMESUGAR LENGTH REL))))
                        ((EQ (CAR NUM) '<)
                            (CONS (COND
                                    ((LESSP LENGTH NUM) '(SAY YES,))
                                    (T '(SAY NO,)))
                                (PREPPUT (NAMESUGAR LENGTH REL))))
                        ((ERT ANSREL -- FUNNY NUMBER)))
                    T))
            ((ERT ANSREL-- FUNNY TYPE))))))

(DEFUN ANSTHM (EXP)
    ;; GENRATES A THEOREM OR ASSERTION FOR AN EXPRESSION.
    (PROG (NEG VARLIST BODY)
        (COND
            ;; NOTELL MARKS THAT THIS ISN'T THE KIND OF ASSERTION IT CAN HANDLE.
            ;; IT USES GLOBAL-ERR VAR AND NEG ARE SET AS FREE VARIABLES BY ANSTHMELEMENT WHICH ANALYZES EACH ELEMENT.
            ;; IF THERE ARE NO VARS, IT IS A SIMPLE ASSERTION.
            ((ATOM EXP) (NOTELL))
            ((NOT (GET (CAR EXP) 'TELLABLE)) (NOTELL))
            (T
                (SETQ NEG (NEGATIVE? RSS))
                (SETQ EXP (MAPCAR 'ANSTHMELEMENT (PLNR-REMTIME EXP)))
                (RETURN (COND
                    ((NOT (OR VARLIST NEG)) EXP)
                    (T
                        (PLNR-THCONSIFY VARLIST EXP (COND (NEG (PLNR-PROGIFY NIL (LIST BODY '(THFAIL THEOREM)))) (T BODY))))))))))

(DEFUN ANSTHMADD (OSS)
    (SETQ VARLIST (CONS (VARIABLE? OSS) VARLIST))
    (SETQ BODY (COND
        (BODY (PLNR-PROGIFY NIL (LIST BODY (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (LIST (VARIABLE? OSS))))))
        (T (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (LIST (VARIABLE? OSS))))))
    (PLNR-VAR (VARIABLE? OSS)))

(DEFUN ANSTHMELEMENT (X)
    (COND ((NOT (ATOM X)) X)
        ((TSS? X) (NOTELL))
        ((RSS? X) (NOTELL))
        ((NOT (OSS? X)) X)
        ((REFER? X) (ATOMIFY (REFER? X)))
        ((EQ (QUANTIFIER? X) 'ALL) (COND (NEG (NOTELL)) (T (ANSTHMADD X))))
        ((EQ (QUANTIFIER? X) 'NO) (SETQ NEG T) (ANSTHMADD X))
        ((EQ (QUANTIFIER? X) 'NDET) (ANSTHMADD X))
        ((NOT (EQ (QUANTIFIER? X) 'INDEF)) (NOTELL))
        ((ISQ (PARSENODE? X) ANY) (ANSTHMADD X))
        (T (GLOBAL-ERR YOU HAVE TO TELL ME WHICH))))

(DEFUN ANSUNIQUE (LIST)
    ;; THIS FUNCTION SHOULD ELIMINATE ANSWERS WHICH GIVE THE SAME
    ;; RESULT EVEN THHOUGH THEY INVOLVE DIFFERENT INTERPRETATIONS.
    ;; IT NEEDS TO CHECK FOR SIGNIFICANT DIFFERENCES, E.G. IN WHAT
    ;; GETS PRINTED OR DONE, WHILE IGNORING INSIGNIFICANT ONES,
    ;; E.G. THE NAMES OF ATOMS TO WHICH THINGS ARE ATTACHED.
    ;; FOR THE MOMENT, IT JUST RETURNS THE LIST UNTOUCHED.
    LIST)

        ;; FROM BOTH THE INPUT SENTENCE AND THE ANSWER.

(SETQ ANS-TEST? NIL)

(DEFUN ATOMIFY (X) (COND ((ATOM X) X) ((CDR X) X) ((CAR X))))

(DEFUN CUTOFF (X)
    ;; FOR CUTTING # OFF OF CONCEPT NAMES TO GET ENGLISH WORDS.
    (READLIST (CDR (EXPLODE X))))

(DEFUN DESCRIBEVENT (EVENT TYPE)
    (PROG (ANS)
        (SETQ EVENT (CAR EVENT))
        (RETURN (COND
            ((EQ TYPE 'WHERE)
                (GLOBAL-ERR I CAN\'T ANSWER \"WHERE\" QUESTIONS YET)) ;; "sic!
            ((EQ TYPE 'WHY)
                (COND ((EQ (GET EVENT 'WHY) 'COMMAND) '((SAY BECAUSE YOU TOLD ME TO)))
                    (T (CONS '(SAY TO) (NAMEACTION 'INFINITIVE (GET EVENT 'WHY))))))
            ((EQ TYPE 'HOW)
                (MAPCAR #'(LAMBDA (X) (AND (EQ (GET X 'WHY) EVENT) (SETQ ANS (CONS X ANS)))) EVENTLIST)
                (COND
                    ((NULL ANS) '((SAY I CAN\'T ANALYZE HOW I DID IT)))
                    (T (APPEND '((SAY BY)) (NAMEACTION 'ING (CAR ANS)) (MAPCAN #'(LAMBDA (X) (CONS '(PRINC '\;) (CONS '(SAY THEN) (NAMEACTION 'ING X)))) (CDR ANS))))))
            ((OR (EQ TYPE 'POLAR) (EQ TYPE 'WHEN))
                (COND
                    ((EQ (GET EVENT 'WHY) 'COMMAND)
                        (COND
                            ((EQ EVENT (TOPLEVEL (CAR EVENTLIST))) '((SAY JUST NOW)))
                            (T (CONS '(SAY BEFORE) (NAMEACTION 'PAST (TOPLEVEL (CAR (FINDB EVENT EVENTLIST))))))))
                    (T (CONS '(SAY WHILE) (NAMEACTION 'PRES-PAST (TOPLEVEL EVENT))))))
            ((BUG DESCRIBEVENT -- FUNNY TYPE))))))

(DEFUN DISPUT (ASSERTION)
    ;; PUT THE SENTENCE NUMBER ON THE ASSERTION AS A WHO PROPERTY
    (OR (NOT DISCOURSE) (PUTPROP ASSERTION SENTNO 'WHO)))

(DEFUN ELIZA (NODE)
    ;; DOES THE OBVIOUS THING.
    (PROG (XX NUM)
        (SETQ NUM (LENGTH (N NODE)))
        (RETURN
            (APPLY 'APPEND
                (MAPLIST #'(LAMBDA (WORD)
                    (COND
                        ((NOT (LESSP NUM (LENGTH WORD))) NIL)       ;; THIS KLUDGE STOPS IT AT THE END OF THE NODE
                        ((SETQ XX (ASSQ (CAR WORD) '((I YOU) (ME YOU) (AM ARE) (ARE AM))))
                            (CDR XX))                               ;; WE RETURN LIST OF THE THING REALLY WANTED, SO
                        ((EQ (CAR WORD) 'YOU)                       ;; THE APPLY APPEND CAN GET RID OF THE EMPTY ONES.
                            (SETQ XX (FINDMOTHER WORD NODE))        ;; UNFORTUNATELY, FOR "YOU" IT IS NECESSARY TO
                            (COND ((ISQ XX SUBJ) '(I))              ;; DECIDE WHETHER IT SHOULD BE REPLACED BY "I" OR
                                ((ISQ XX OBJ) '(YOU))               ;; "ME", ACCORDING TO WHETHER IT WAS PARSED AS AN
                                ((BUG ELIZA -- SUBJ OBJ))))         ;; OBJECT OR SUBJECT. FINDMOTHER IS USED TO FIND
                        ((LIST (CAR WORD)))))                       ;; THE PARSE NODE. WORDS OTHER THAN THE SPECIAL
                    (NB NODE))))))                                  ;; ONES GO THROUGH DIRECTLY.

(DEFUN ENOUGH-BETTER (ANS1 ANS2)
    (GREATERP (PLAUSIBILITY? ANS1)
        (PLUS (PLAUSIBILITY? ANS2) TIMID)))

(DEFUN FINDMOTHER (WORD NODE)
    ;; FINDMOTHER TAKES A PLACE IN THE SENTENCE AND A GRAMMAR NODE
    ;; (BOTH ARE ACTUALLY LISTS) AND FINDS THE SINGLE-WORD
    ;; CONSTITUTENT BEGINNING AT THAT PLACE IN THE SENTENCE.
    (COND ((AND (EQ WORD (NB NODE)) (EQ (CDR WORD) (N NODE))) NODE)
        (T (APPLY 'APPEND (MAPLIST #'(LAMBDA (NOD) (FINDMOTHER WORD NOD)) (H NODE))))))

(DEFUN HEADPART (NODE)
    ;; EVERYTHING UP TO THE NOUN, FOR EXAMPLE "THE RED BLOCK" IN "THE RED BLOCK WHICH ..."
    ;; NOTE THAT NODE IS ACTUALLY A LIST OF NODE (A PROPER GRAMMAR POINTER).
    (AND (SETQ PT NODE) (MOVE-PT DLC PV (NOUN)) (FROM (NB NODE) (N PT))))

(DEFUN LISTNAMES (PHRASE SPEC NAMES)
    ;; PHRASE IS THE INITIAL THING TO COMPARE FOR USING "ONE", SPEC IS EITHER DEF OR INDEF, AND THE NAMES ARE OF DATA-BASE OBJECTS.
    ;; LISTNAMES PUTS OUT AN ACTION LIST, AS WELL AS PUTTING THINGS ONTO THE BACKREF.
    ;; IT IS CALLED AFTER THE ANSWER HAS BEEN DECIDED ON.
    (PROG (COUNT EXAM X RES ANS COMMA?)
        ;; NAMEOBJ RETURNS A LIST OF THE OBJECT AND THE ...
        ;; THIS PATCH MAY WELL BE TOTAL OUT OF PHASE WITH THE BACKREF HACKER - DDM 5-12-73 INSTRUCTIONS FOR NAMING IT.
        (SETQ NAMES (MAPCAR #'(LAMBDA (X) (NAMEOBJ X SPEC)) NAMES))
        (COND ((NULL NAMES) (RETURN '(SAY NOTHING))))
    UP  (SETQ COUNT 1)
        (SETQ EXAM (CAR NAMES))
        (SETQ NAMES (CDR NAMES))
    BACK (COND ((SETQ X (ASSOC (CAR EXAM) NAMES))
            (SETQ NAMES (DELQ X NAMES))
            (SETQ COUNT (ADD1 COUNT))
            (SETQ EXAM (LIST (CAR EXAM) (APPEND (CADR X) (CADR EXAM))))
            (GO BACK)))
        ;; WHEN THERE ARE TWO OBJECTS WITH THE SAME ENGLISH DESCRIPTIONS, A JOINT OBJECT IS PRODUCED COMBINING THE OBJECTS.
        ;; THE COUNT IS LATER USED TO PUT IN THE APPROPRIATE NUMBER, AND THE DESCRIPTION IS CHECKED TO SEE IF "ONE" CAN BE USED.
        ;; ADD THE ONE JUST PRODUCED TO THE RESULT LIST.  TRY ANOTHER.
        (SETQ RES (CONS (CONS (PLURALIZE (CAR EXAM) COUNT) (CDR EXAM)) RES))
        (AND NAMES (GO UP))
        (SETQ RES
            (MAPCAR #'(LAMBDA (PHRASE2)
                    (COND ((PROPNAME (CAADR PHRASE2)) (CAR PHRASE2))
                        ;; ANSNAME PARSES THE PHRASE AND PUTS THE ...
                        ;; ANSONE SUBSTITUTES "ONE" IF POSSIBLE
                        (T (ANSNAME PHRASE2) (ONECHECK (CAR PHRASE2)))))
                RES))
        (SETQ ANS (CAR RES))
    OUTPUT (COND
            ((NULL (SETQ RES (CDR RES))) (RETURN ANS))
            ((CDR RES) (SETQ COMMA? T) (SETQ ANS (APPEND ANS '((PRINC '\,)) (CAR RES))))
            ((SETQ ANS (APPEND ANS (AND COMMA? '((PRINC '\,))) '((SAY AND)) (CAR RES)))))
        (GO OUTPUT)))

(DEFUN NAMEACTION (TENSE EVENT)
    ;; THIS FUNCTION SETS UP A LIST OF S-EXPRESSIONS WHICH ARE RETURNED TO DESCRIBEVENT AND WHICH
    ;; WHEN EVALUATED WILL PRINT OUT AN ENGLISH DESCRIPTION OF THE SINGLE, SIMPLE EVENT IMBEDDED
    ;; IN THE LIST "THASSERTION" WITH THE TENSE SPECIFIED.
    (PROG (PLNR-FORM VERB OBJ1 OBJ2)
        ;; THE THASSERTION PROPERTY IS A LIST THAT TYPICALLY LOOKS LIKE "(NIL (2 (3 1 ((#GRASP :E2 :B6)))))"
        (SETQ PLNR-FORM (CAR (CADDR (CADADR (GET EVENT 'THASSERTION))))
            VERB (CUTOFF (CAR PLNR-FORM))
            OBJ1 (CADDR PLNR-FORM)
            OBJ2 (CADDDR PLNR-FORM))
        (SETQ FOOBAR (COND
            ((EQ VERB 'CLEARTOP)
                ;; SAYIFY WRAPS THE FUNCTION "SAY" ARROUND A LIST OF WORDS AND RETURNS THE RESULTING S-EXPRESSION.
                (CONS (SAYIFY (VBFIX 'CLEAN NIL)) (PRON-PRT 'OFF OBJ1)))
            ((EQ VERB 'GET-RID-OF)
                ;; NAMELIST-EVALED '(NIL) 'DEF RETURNS A LIST (!!!) OF S-EXPRESSIONS.
                (CONS (SAYIFY (VBFIX 'GET T) 'RID 'OF) (NAMELIST-EVALED '(NIL) 'DEF OBJ1)))
            ((EQ VERB 'GRASP)
                (CONS (SAYIFY (VBFIX 'GRASP T)) (NAMELIST-EVALED '(NIL) 'DEF OBJ1)))
            ((EQ VERB 'PICKUP)
                (CONS (SAYIFY (VBFIX 'PUT T)) (PRON-PRT 'UP OBJ1)))
            ((EQ VERB 'PUTON)
                (APPEND (CONS (SAYIFY (VBFIX 'PUT T)) (NAMELIST-EVALED '(NIL) 'DEF OBJ1)) (CONS '(SAY ON) (NAMELIST-EVALED '(NIL) 'DEF OBJ2))))
            ((EQ VERB 'STACKUP)
                (CONS (VBFIX STACK T) (PRON-PRT 'UP OBJ1)))
            ((EQ VERB 'RAISEHAND) NIL)
            (T (BUG NAMEACTION - I DON\'T KNOW WHAT TO DO WITH THE VERB I GOT))))
        (RETURN FOOBAR)))

(DEFUN NAMELIST (ONE SPEC LISTX)
    ;; GENERATES A LIST OF EXPRESSIONS TO BE EVALUATED WHICH WILL CAUSE THE APPROPRIATE NAMELIST TO BE PRINTED OUT.
    ;; THE ARGUMENTS ARE JUST THOSE TO LISTNAMES.
    (LIST (LIST 'EVLIS (LIST 'LISTNAMES (QUOTIFY ONE) (QUOTIFY SPEC) (QUOTIFY LISTX)))))
    ;; A TYPICAL CALL WOULD RESULT IN A VALUE OF ((EVLIS (LISTNAMES '(A RED BLOCK) 'INDEF '(:B1 :B7)))) WHICH WOULD BE EVALUATED LATER.
    ;; NOTE THAT LISTNAMES WILL IN TURN PRODUCE A LIST OF EXPRESSIONS TO BE EVALUATED, WHICH WILL BE CAUGHT BY THE EVLIS.  CONFUSING?

(DEFUN NAMELIST-EVALED (ONE SPEC LISTX)
    (PROG (F)
        (SETQ F (LIST 'LISTNAMES
            (QUOTIFY ONE)
            (QUOTIFY SPEC)
            (QUOTIFY LISTX)))
        (RETURN (LIST (EVAL F)))))

(DEFUN NAMENUM (X)
    ;; GENERATES NUMBER NAMES.
    (OR (NTH (ADD1 X) '(NONE ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN))
        (GLOBAL-ERR I CAN\'T COUNT THAT HIGH)))

(DEFUN NAMEOBJ (ITEM SPEC)
    ;; NAMES THE OBJECT IN ENGLISH -- GENERATES LIST OF THINGS TO BE EVALUATED.  SPEC IS EITHER 'INDEF OR 'DEF
    (PROG (TYPE: TYPELIST TYPE NAME: COLOR: COLORLIST SIZE: SIZELIST CUBE NAME X)
        (AND (SETQ X (ASSOC ITEM '((:SHRDLU I) (:FRIEND YOU))))
            (RETURN (LIST (ANSAY (CDR X)) (LIST ITEM))))                        ;;  SPECIAL CASE CHECK
        (THVAL2 NIL '(THGOAL (#NAMEOBJ) (THUSE TC-NAMEOBJ)))
        (OR TYPELIST
            (ERT NAMEOBJ -- OBJECT WITH NO #IS ASSERTION))
        ;; DISPUT CHECKS TO SEE IF DISCOURSE IS BEING KEPT, AND IF SO PUTS THE RELEVANT SENTENCE NUMBER AS A PROPERTY ON THE ASSERTION.
        (DISPUT TYPE:)
        (COND ((EQ (SETQ TYPE (CADDAR TYPE:)) '#NAME)                           ;; A NAME IS ITS OWN NAME.
                (RETURN (LIST (ANSAY (LIST ITEM)) (LIST ITEM))))
            ((MEMQ '#PROPERTY (GET TYPE 'SYS))
                ;; CUTOFF CUTS THE # OFF OF NAMES LIKE #RED AND #POINTED WHICH ARE USED FOR PROPERTIES.
                (RETURN (LIST (ANSAY (LIST (CUTOFF ITEM))) (LIST ITEM))))
            ((NOT (CDR TYPELIST))
                (RETURN (LIST (ANSAY (LIST 'THE (CUTOFF TYPE))) (LIST ITEM))))  ;; THERE IS ONLY ONE OBJECT OF THIS TYPE (E.G. TABLE, BOX, HAND)
            (CUBE (SETQ NAME '(CUBE)))
            ((SETQ NAME (LIST (CUTOFF TYPE)))))                                 ;; E.G. #BLOCK BECOMES BLOCK.
        (AND NAME:
            (RETURN (LIST (ANSAY (LIST 'THE (CAR NAME) 'NAMED (CADDAR NAME:))) (LIST ITEM)))) ;; E.G. THE BLOCK NAMED SUPERBLOCK.
        (DISPUT COLOR:)                                                         ;; IF WE HAVEN'T RETURNED YET, COLOR WILL BE NEEDED TO FULLY DESCRIBE THE OBJECT.
        (SETQ NAME (CONS (CUTOFF (CADDAR COLOR:)) NAME))
        (OR (CDR COLORLIST)
            (RETURN (LIST (ANSAY (CONS 'THE NAME)) (LIST ITEM))))               ;; THERE ARE NO OTHERS OF THE SAME COLOR.  IF THERE ARE, WE MUST USE SIZE AS WELL.
        (SETQ NAME (CONS SIZE: NAME))
        (RETURN (LIST (COND
            ((NULL (CDR SIZELIST))
                (ANSAY (CONS 'THE NAME)))                                       ;; THE SIZE MANAGES TO FINISH SPECIFYING IT.
            ((EQ SPEC 'INDEF)
                (ANSAY (CONS 'A NAME)))                                         ;; IN THE INDEFINITE CASE WE DON'T CARE IF THIS ISN'T A FULL SPECIFICATION.
            ((SETQ X (THVAL2 NIL '(THFIND ALL $?X (X (Y ITEM)) ($G (#SUPPORT $?Y $?X)))))
                (CONS (APPEND '(SAY THE) NAME)
                    (CONS '(SAY WHICH SUPPORTS)
                        (LISTNAMES NIL 'INDEF X))))                             ;; IF IT SUPPORTS ANYTHING, NAME THEM.
            ((CONS (APPEND '(SAY THE) NAME)
                (CONS '(SAY WHICH IS TO THE RIGHT OF)
                    (COND ((SETQ X (THVAL2 NIL '(THFIND ALL $?X (X (Y ITEM))
                                ($G (#AT $?X ?)) ($G (#LOC #RIGHT $?Y $?X) (THUSE TC-LOC))))) ;; MAKE SURE IT IS AN ITEM WITH A LOCATION.
                            (LISTNAMES NIL 'INDEF X))
                        ('((SAY NOTHING))))))))
            (LIST ITEM)))))

(DEFPROP TC-NAMEOBJ
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
        (#NAMEOBJ)
        ($G (#IS $?X $?TYPE))
        (SETQ TYPE: THVALUE)
        (OR (SETQ CUBE (AND (EQ $?TYPE '#BLOCK) (#EQDIM $?X)))
            T)
        (THCOND
            (($G (#NAME $?X $?NAME))
                (SETQ NAME THVALUE))
            (($G (#IS $?Y $?TYPE))
                (OR (NOT CUBE) (#EQDIM $?Y))
                (SETQ TYPELIST (CONS $?Y TYPELIST))
                ($G (#COLOR $?X $?COLOR))
                (SETQ COLOR: THVALUE)
                ($G (#COLOR $?Y $?COLOR))
                (SETQ COLORLIST (CONS $?Y COLORLIST))
                (SETQ SIZE: (NAMESIZE (SIZE $?X)))
                (EQ SIZE: (NAMESIZE (SIZE $?Y)))
                (SETQ SIZELIST (CONS $?Y SIZELIST))
                (THFAIL))))
    THEOREM)

(DEFUN NAMESIZE (X)
    ;; ACCEPTS EITHER SINGLE NUMBER OR LIST OF DIMENSIONS.
    (OR (NUMBERP X) (SETQ X (APPLY 'PLUS X)))
    (COND ((GREATERP X 383) 'LARGE)
        (T 'SMALL)))

(DEFUN NAMESUGAR (NUM OSS)
    ;; GENERATES PHRASES LIKE "THREE OF THEM".
    (PROG (VAGUE)
        ;; VAGUE IS FOR WORDS LIKE "ANYTHING", "SOMETHING", "NOTHING" TO AVOID SAYING "OF THEM" WHEN IT ISN'T APPROPRIATE.
        (SETQ VAGUE (MEMQ '#VAGUE (MARKERS? OSS)))
        (RETURN (LIST (CONS 'SAY
            (COND ((AND VAGUE (ZEROP NUM)) '(NOTHING))
                ((CONS (NAMENUM NUM)
                    (COND (VAGUE (COND ((EQUAL NUM 1) '(THING)) ('(THINGS))))
                        ('(OF THEM)))))))))))

(DEFUN NOTELL NIL
    (GLOBAL-ERR THAT ISN 'T THE KIND OF THING I CAN BE TOLD))

(DEFUN ONECHECK (ITEM)
    ;; CHECKS TO SEE IF A SUBSTITUTE "ONE" CAN BE USED.
    ;; ITEM IS A SINGLE "SAY" PHRASE.
    ;; "PHRASE" IS A FREE VARIABLE IN LISTNAMES.
    (PROG (ANS OLD NEW)
        (AND (EQUAL PHRASE '(NIL))
            (SETQ PHRASE (CAR ITEM))
            (RETURN ITEM))
        (SETQ OLD (REVERSE PHRASE))
        (SETQ NEW (REVERSE (CAR ITEM)))
        (OR (EQ (CAR OLD) (CAR NEW))
            (EQ (CAR OLD) (GET (CAR NEW) 'ROOT))
            (EQ (CAR NEW) (GET (CAR OLD) 'ROOT))
            ;; IF THE NOUNS DON'T MATCH, JUST RETURN WHAT YOU GOT.
            ;; MATCHING INCLUDES PLURALS TO THEIR CORRESPONDING SINGULAR FORMS.
            (RETURN ITEM))
    LOOP (SETQ NEW (CDR NEW))
        (SETQ OLD (CDR OLD))
        (COND ((OR (NULL NEW) (NULL OLD) (ISQ NEW NUM) (ISQ NEW DET) (NOT (EQ (CAR NEW) (CAR OLD))))
            (RETURN (CONS
                (REVERSE (CONS (COND ((ISQ (LAST (CAR ITEM)) NPL) 'ONES) (T 'ONE)) NEW))
                (CDR ITEM)))))
        (GO LOOP)))

(DEFUN ORDNAME (NUM)
    ;; NAME AN ORDINAL
    (COND ((EQUAL NUM 1) 'ONCE) ((EQUAL NUM 2) 'TWICE)
        ((READLIST (NCONC (EXPLODE (NAMENUM NUM)) '(\ T I M E S))))))

(DEFLIST PAST (PUT PUT))

(DEFUN PLNR-ANDORIFY (RSS)
    ;; TURNS AN RSS INTO A COLLECTION OF PLANNER CODE FOR A COMMAND
    (COND ((AND? RSS)
            (PLNR-PROGIFY NIL (MAPCAR 'PLNR-ANDORIFY (AND? RSS))))
        ((OR? RSS)
            ;; (PLNR-ORIFY NIL (MAPCAR 'PLNR-ANDORIFY (OR? RSS)))
            (ERT SORRY, PLNR-ORIFY NOT WRITTEN))
        ((PLNR-PROGIFY NIL (MAPCAR 'PLNR-GOALIFY (RELATIONS? RSS))))))

(DEFUN PREPPUT (X)
    (COND ((AND (REL? RSS) (SETQ PT (PARSENODE? (REL? RSS))) (ISQ (MOVE-PT U) PREPG))
            (CONS (CONS 'SAY (FROM (NB PT) (NB (MOVE-PT DLC)))) X))
        (T X)))

(DEFUN PLURALIZE (ITEM NUM)
    ;; CONVERTS A SINGULAR NOUNPHRASE OR "ONCE" STATEMENT INTO PLURAL.
    (COND ((GREATERP 2 NUM) ITEM)
        (T (COND ((MEMQ 'A (CAR ITEM))
                (CONS (PLURALMAKE (SUBST (NAMENUM NUM) 'A (CAR ITEM))) (CDR ITEM)))
            ((MEMQ 'ONCE (CAR ITEM))
                (CONS (SUBST (ORDNAME NUM) 'ONCE (CAR ITEM)) (CDR ITEM)))
            ((BUG PLURALIZE -- FUNNY ITEM))))))

(DEFUN PLURALMAKE (PHRASE)
    ;; CONVERTS SINGULAR PHRASE TO PLURAL.
    (PROG (SING PLURAL)
        (OR (ISQ (SETQ SING (LAST PHRASE)) NOUN)
            (BUG PLURALMAKE -- NO NOUN))
        (SETQ PLURAL (MAKNAM (NCONC (EXPLODE (CAR SING)) '(S))))
        (OR (GET PLURAL 'FEATURES)
            (BUILDWORD PLURAL '(NOUN NPL) (SM SING) (CAR SING)))
        (RETURN (SUBST PLURAL (CAR SING) PHRASE))))

(DEFUN PRON-PRT (PARTICLE NG)
    ;; THIS IS EVENTUALLY SUPPOSED TO BE THE PLACE FOR THE PRONOUN-PARTICLE-INTERACTION MAGIC
    ;; TO HAPPEN, IE. "CLEAR OFF THE BLOCK." VS. "CLEAR IT OFF" SINCE "CLEAR OFF IT." IS
    ;; UNGRAMMATICAL AND "CLEAR THE BLOCK OFF." WOULD NOT BE APPROPRIATE IN CASES OF HEAVY-NP'S.
    ;;
    ;; AT THE MOMENT, FOR SIMPLICITY'S SAKE, I'VE IGNORED THE
    ;; PROBLEM AND THE PARTICLE IS ALWAYS PUT BEFORE THE NG.
    (CONS (LIST 'SAY PARTICLE)
        (NAMELIST-EVALED '(NIL) 'DEF NG)))

(DEFUN SAYIFY FEXPR (EXP-LIST)
    (CONS 'SAY (MAPCAR #'(LAMBDA (Y) (EVAL Y)) EXP-LIST)))

(DEFUN THVAL-MULT (CODE)
    ;; DOES A THVAL WITH DIFFERENT VALUES OF WHO (I.E. NIL (EVERYTHING I KNOW),
    ;; 'HE (EVERYTHING HE KNOWS), AND THE PREVIOUS SENTENCE) USED TO TELL IF AN
    ;; ANSWER COULD HAVE BEEN GENERATED WITH HIS KNOWLEDGE TO SEE WHETHER HE REALLY
    ;; MEANT THIS INTERPRETATION.  RETURNS A LIST OF A PLAUSIBILITY AND THE RESULT
    ;; OF THE THVAL USING ALL THE KNOWLEDGE IN THE DATA BASE.
    (PROG (ANS)
        (SETQ ANS (THVAL2 NIL CODE))
        ;; THIS FEATURE IS ONLY RELEVANT IN DISCOURSE AND WHEN THERE ARE AMBIGUITIES.
        (OR (AND AMBIG DISCOURSE) (RETURN (LIST 0 ANS)))
        ;; GIVE A VALUE OF 256 IF HE COULDN'T HAVE ANSWERED IT AT ALL.
        (OR (EQUAL ANS (THVAL2 'HE CODE)) (RETURN (LIST 256 ANS)))
        ;; PLAUSIBILITY IS 0 IF HE COULD HAVE ANSWERED IT WITH RECENTLY MENTIONED INFORMATION.
        ;; 128 IF HE COULD ANSWER IT BUT NOT WITH RECENT INFO.
        (RETURN (COND ((EQUAL ANS (THVAL2 (LIST (*DIF SENTNO 2) (ADD1 SENTNO)) CODE)) (LIST 0 ANS)) ((LIST 128 ANS))))))

(DEFUN TOPLEVEL (EVENT)
    ;; FINDS THE TOP LEVEL EVENT GOING ON AT THE TIME
    (COND ((EQ (GET EVENT 'WHY) 'COMMAND) EVENT)
        (T (TOPLEVEL (GET EVENT 'WHY)))))

(DEFUN FINDCHOOSE (OSS X ANS2)
    (PROG (HAVE NEED XX ANS PLNRCODE LOOP)
        (AND (REFER? OSS) (RETURN (ATOMIFY (REFER? OSS))))
        (COND
            ((AND? OSS)
                (RETURN (MAPBLAND #'(LAMBDA (OSS)
                    (PROG (Y)
                        (SETQ Y (FINDCHOOSE OSS X ANS2))
                        (SETQ ANS2 (APPEND Y ANS2))
                        (RETURN Y)))
                    (AND? OSS))))
            ((OR? OSS)
                (SETQ LOOP (OR? OSS))
                (RETURN (PROG (Y)
                GO  (COND
                    ((SETQ Y (FINDCHOOSE (CAR LOOP) X ANS2)) (RETURN Y))
                    ((SETQ LOOP (CDR LOOP)) (GO GO)))))))
        (SETQ PLNRCODE (PLNR-DESCRIBE (RELATIONS? OSS) (VARIABLE? OSS) (LIST (VARIABLE? OSS))))
        (PUTPROP OSS PLNRCODE 'PLNRCODE=)
        (COND
            ((EQ (QUANTIFIER? OSS) 'ALL)
                (RETURN (ATOMIFY (THVAL (PLNR-FINDIFY 'ALL (VARIABLE? OSS) (LIST (VARIABLE? OSS)) PLNRCODE) NIL))))
            ((OR (AND? OSS) (OR? OSS)) (GO CONJ)))
        (OR (ATOM (SETQ NEED (NUMBER? OSS)))
            (SETQ NEED (CADR NEED)))
        (AND (EQ NEED 'NS) (SETQ NEED 1))
        (SETQ HAVE 0)
    GO  (COND
            ((OR (EQ HAVE NEED)
                (AND (GREATERP HAVE NEED)
                    (SETQ ANS (FINDREDUCE ANS (DIFFERENCE HAVE NEED)))))
                (GO DONE))
            ((EQ X 'NOMORE) (RETURN NIL))
            ((SETQ HAVE (LENGTH
                (SETQ ANS (APPEND
                    (THVAL (PLNR-FINDIFY
                        (LIST 1 (DIFFERENCE NEED HAVE) T)
                        (VARIABLE? OSS)
                        (LIST (VARIABLE? OSS))
                        (PLNR-PROGIFY NIL
                            (APPEND (LIST PLNRCODE)
                                (SUBST (VARIABLE? OSS) '*** '((NOT (OR (MEMQ (THV ***) ANS) (MEMQ (THV ***) ANS2)))))
                                (AND X (SUBST (VARIABLE? OSS) '* (CAR X))))))
                        THALIST)
                    ANS))))
            (SETQ X (COND (X (CDR X)) ('NOMORE)))
            (GO GO)))
    CONJ (SETQ LOOP (OR (AND? RSS) (OR? RSS)))
    UP  (COND ((GET (CAR LOOP) 'REFER)
                (SETQ ANS (APPEND (GET (CAR LOOP) 'REFER) ANS)))
            ((SETQ XX (FINDCHOOSE (CAR LOOP) X (APPEND ANS2 ANS)))
                (SETQ ANS (APPEND XX ANS))))
        (COND ((AND ANS (OR? OSS)))
            ((SETQ LOOP (CDR LOOP)) (GO UP))
            (ANS)
            ((RETURN NIL)))
    DONE (AND (ATOM (VARIABLE? OSS))
            (PUTPROP (VARIABLE? OSS) (REVERSE ANS) 'BIND))
        (RETURN (ATOMIFY (REVERSE ANS)))))

(DEFUN FINDNUM (X)
    (COND ((NUMBERP X) X)
        ((EQ (CAR X) 'EXACTLY) (LIST (CADR X) (ADD1 (CADR X)) NIL))
        ((EQ (CAR X) '>) (ADD1 (CADR X)))
        ((EQ (CAR X) '<) (CADR X))
        ((EQ X 'NS) 1)
        ((EQ X 'NPL) 2)
        ((ERT FINDNUM))))

(DEFUN FINDREDUCE (X Y)
    (PROG NIL
    =>  (SETQ X (CDR X))
        (COND ((ZEROP (SETQ Y (SUB1 Y))) (RETURN X)) ((GO =>)))))

(DEFPROP IASS
    (LAMBDA (X)
        (PROG (XX)
            (OR (SETQ XX (CADR (SASSQ X (CADR (CADDDR ANS)) #'SASS)))
                (RETURN T))
            (SAY \
BY)         (PRINC (COND ((EQ X 'IT) '\"IT\") ((MEMQ 'THEY (FROM SENT NIL)) '\"THEY\") ('\"THEM\")))
            (SAY , I ASSUME YOU)
            (PRINC 'MEAN)
            (MAPC #'PRINT2 (PARAP XX))
            (RETURN (PRINC '\.\
))))   EXPR)

(DEFUN MUNG (LIST MUNG)
    (SETQ MUNG (LIST 'QUOTE MUNG))
    (AND DISCOURSE (SETQ LIST (CADDR LIST)))
    (COND ((EQ (CAAR (CDDDR LIST)) 'THAMONG)
            (RPLACD (CDAR (CDDDDR LIST)) MUNG))
        ((RPLACD (CDDDR LIST) (CONS (LIST 'THAMONG (LIST 'THV (CADR (CADDR LIST))) MUNG) (CDDDDR LIST))))))

(DEFUN NAMEVENT (EVENT TYPE)
    (PROG (THALIST EV SUBJ OBJ1 OBJ2)
        (OR (SETQ EV (GET (GET EVENT 'TYPE) 'NAMEVENT))
            (ERT NAMEVENT))
        (OR (THVAL (LIST 'THGOAL
            (COND
                ((EQ (CAR EV) 2) '(? $?EVENT))
                ((EQ (CAR EV) 3) '(? $?EVENT (THNV SUBJ)))
                ((EQ (CAR EV) 'I3) '(? $?EVENT (THNV OBJ1)))
                ((EQ (CAR EV) 4) '(? $?EVENT (THNV SUBJ) (THNV OBJ1)))
                ((EQ (CAR EV) 'I4) '(? $?EVENT (THNV OBJ1) (THNV OBJ2)))
                ((EQ (CAR EV) 5) '(? $?EVENT (THNV SUBJ) (THNV OBJ1) (THNV OBJ2)))
                ((ERT NAMEVENT DATA))))
            (SETQ THALIST (LIST
                (LIST 'EVENT EVENT)
                (LIST 'SUBJ (COND ((NUMBERP (CAR EV)) NIL) ('I)))
                (LIST 'OBJ1 NIL) (LIST 'OBJ2 NIL))))
            (ERT NAMEVENT THVAL))
        (MAPC #'(LAMBDA (X)
                (AND (CADR X) (SET (CAR X) (ERT UNDEF-FN: NAMES NAMES (LISTIFY (CADR X)) 'EV))))
            (CDR THALIST))
        (SETQ ANSBACK2 (OR ANSBACK T))
        (SETQ LASTANSEV EVENT)
        (RETURN (APPEND
            (COND ((EQ TYPE 'PAST) SUBJ) ((EQ TYPE 'TO) '(TO)))
            (EVAL (CADR EV))))))

(DEFUN PARAP NIL (ERT YOU LOSE, PARAP IS FLUSHED UNTIL IT CAN BE FIGURED OUT))

(DEFUN PRTPUT (X Y) (COND ((CDR Y) (CONS X Y)) ((APPEND Y (LIST X)))))

(DEFUN VBFIX (X PP)
    (COND
        ((EQ TENSE 'ING)
            (SETQ X (REVERSE (EXPLODE X)))
            (READLIST (REVERSE (APPEND '(G N I) (VBFIX2 X) X))))
        ((EQ TENSE 'PAST)
            (OR (GET X 'PAST)
                (AND (SETQ X (REVERSE (EXPLODE X)))
                    (READLIST (REVERSE (APPEND '(D E) (VBFIX2 X) X))))))
        ((EQ TENSE 'INFINITIVE) X)
        (T (BUG VBFIX - WHAT DO I DO WITH THIS TENSE?))))

(DEFUN VBFIX2 (X) (AND PP (MEMQ (CAR X) CONSO) (MEMQ (CADR X) VOWEL) (LIST (CAR X))))

#_(ns shrdlu.cgram)

;; #################################################################
;;
;;  CGRAM > THE REGULAR GRAMMAR AFTER GOING THROUGH THE PRECOMPILER
;;
;; #################################################################

(DEFUN CLAUSE NIL
    (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT POSITION-OF-PRTMVB LOCATIONMARKER SUBJ-VB-BACKUP-TYPE1 POSITION-OF-PTW)
        (SETQ NN T)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (REVERSE REST)) (SETQ NB (OR (NB RE) N)) N (SETQ H RE) NIL))
        (SETR 'PARENT PARENT C)
ENTERING-CLAUSE
        (AND LABELTRACE (PASSING 'ENTERING-CLAUSE))
        (SETR 'TIME (BUILD TSSNODE= (MAKESYM 'TSS)) C)
        (SETQ :RESULT (CQ SIMP))
        (COND (:RESULT (GO SUBJ)))
        (SETQ :RESULT (CQ MAJOR))
        (COND (:RESULT (GO INIT)) (T (GO SEC)))
INIT
        (AND LABELTRACE (PASSING 'INIT))
        (SETQ LOCATIONMARKER N)
        (SETQ :RESULT (AND (NQ BINDER) (PARSE CLAUSE BOUND INIT)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO FIXIT)) (T (GO MAJOR)))))
        (FQ BIND)
        (SETQ :RESULT (CALLSM (SMBIND)))
        (COND (:RESULT (GO INIT)))
FIXIT
        (AND LABELTRACE (PASSING 'FIXIT))
        (SETQ PTW CUT)
        (SETQ :RESULT (CUT (MOVE-PTW)))
        (COND (:RESULT (GO INIT)) (T (GO MAJOR)))
MAJOR
        (AND LABELTRACE (PASSING 'MAJOR))
        (CUT END)
        (COND ((EQ PUNCT '?) (GO QUEST))
        ((OR (CQ IMPER) (EQ PUNCT '!)) (GO IMPER)))
        (GO THEREINIT)
FDEC
        (AND LABELTRACE (PASSING 'FDEC))
        (FQ DECLAR)
THEREINIT
        (AND LABELTRACE (PASSING 'THEREINIT))
        (SETQ :RESULT (AND (NEXTWORD? 'THERE) (PARSE NIL THERE) (FQ DECLAR)))
        (COND (:RESULT (COND ((NULL NN) (M INIT) (GO FAIL)) (T (GO THERE)))))
THER2
        (AND LABELTRACE (PASSING 'THER2))
        (AND (NQ PREP) (PARSE PREPG INIT) (OR (CALLSM (SMRELATE H)) (POP)))
        (AND (NQ ADV) (PARSE ADV TIMW) (OR (CALLSM (SMADVERB)) (POP)))
        (AND (NQ ADV) (PARSE ADJG ADV VBAD) (OR (CALLSM (SMRELATE H)) (POP)))
        (PARSE NG TIME)
        (SETQ :RESULT (EQ LOCATIONMARKER N))
        (COND (:RESULT (COND ((NULL NN) (GO INPOP)) (T (GO CLAUSETYPE)))) (T (GO INIT)))
INPOP
        (AND LABELTRACE (PASSING 'INPOP))
        (SETQ :RESULT (MOVE-PT C DLC))
        (COND ((NULL :RESULT) (M INPOP) (GO FAIL)))
BICUT
        (AND LABELTRACE (PASSING 'BICUT))
        (CUT-BACK-ONE)
        (GO INIT)
CLAUSETYPE
        (AND LABELTRACE (PASSING 'CLAUSETYPE))
        (SETQ :RESULT (CQ DECLAR))
        (COND (:RESULT (GO SUBJ)))
        (SETQ :RESULT (AND (NQ VB) (NQ INF) (PARSE VG IMPER) (FQ IMPER)))
        (COND (:RESULT (GO VG1)))
        (FQ DECLAR)
        (SETQ :RESULT (CQ IMPER))
        (COND (:RESULT (M IMPER) (GO FAIL)))
SUBJ
        (AND LABELTRACE (PASSING 'SUBJ))
        (CUT END)
SUBJ3
        (AND LABELTRACE (PASSING 'SUBJ3))
        (SETQ :RESULT (OR (AND (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ)) (AND (PARSE CLAUSE RSNG ING SUBJ))))
        (COND (:RESULT (COND ((NULL NN) (GO SUBJ1)) (T (GO SUBREG)))))
SUBJ4
        (AND LABELTRACE (PASSING 'SUBJ4))
        (SETQ :RESULT (PARSE NG SUBJ))
        (COND (:RESULT (COND ((NULL NN) (GO SUBJ1)) (T (GO SUBREG)))))
        (COND ((CQ REL-NOT-FOUND)
        (RQ REL-NOT-FOUND)
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (GO VB))
        (SUBJ-VB-BACKUP-TYPE1 (SETQ SUBJ-VB-BACKUP-TYPE1 NIL) (GO SUBJ11))
        ((AND H (ISQ H TIME) (ISQ H NG)) (SETR 'SUBJECT H C) (GO VB))
        ((MOVE-PT C U (REL-NOT-FOUND))
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
        (SETR 'RELHEAD (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (GO VB))
        ((AND (CQ COMPONENT) NN) (FQ SUBJFORK) (GO VB))
        (H (POP) (GO SUBJ))
        ((GO FAIL)))
HEAD
        (AND LABELTRACE (PASSING 'HEAD))
        (SETQ :RESULT (OR (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON))))
        (COND ((NULL :RESULT) (M HEAD) (GO FAIL)))
SUB2
        (AND LABELTRACE (PASSING 'SUB2))
        (SETQ :RESULT (POP))
        (COND ((NULL :RESULT) (GO FAIL)))
        (SETQ :RESULT (CUT PTW))
        (COND (:RESULT (GO INIT)) (T (GO SUB2)))
SUBJ1
        (AND LABELTRACE (PASSING 'SUBJ1))
        (COND ((ISQ H QUOTED) (AND (ISQ H LIST) (FQ LIST)) (FQ QUOTED) (SETQ H (H H)) (GO RETSM)))
        (AND (CQ REL-NOT-FOUND) (MOVE-PT H PV (QAUX))
            (COND ((ISQ PT BE) (FQ INT AUXBE) (RQ REL-NOT-FOUND) (SETR 'COMP (GETR 'RELHEAD C) C) (SETR 'SUBJECT H C) (SETMVB PT) (GO ONT))
                ((ISQ PT HAVE) (FQ SUBQ) (RQ REL-NOT-FOUND) (SETR 'SUBJECT (GETR 'RELHEAD C) C) (GO VBL))))
SUBJ11
        (AND LABELTRACE (PASSING 'SUBJ11))
        (SETQ :RESULT (CUT-BACK-ONE))
        (COND (:RESULT (GO SUBJ3)) (T (M SUBJ11) (GO FAIL)))
SUBREG
        (AND LABELTRACE (PASSING 'SUBREG))
        (SETR 'SUBJECT H C)
        (GO VB)
VB
        (AND LABELTRACE (PASSING 'VB))
        (SETQ :RESULT (PARSE ADJG ADV VBAD))
        (COND (:RESULT (COND ((NULL NN) (M VB-ADJG) (GO FAIL)) (T (GO VB)))))
        (RQ VBLOK)
VBL
        (AND LABELTRACE (PASSING 'VBL))
        (SETQ :RESULT (PARSE VG))
        (COND (:RESULT (GO VBREG)))
NOVERB
        (AND LABELTRACE (PASSING 'NOVERB))
        (COND ((CQ SUBJFORK) (FQ VBFORK) (GO FINDOBJ1))
            ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
            ((NOT (ISQ H SUBJ)) (GO FAIL))
            ((ISQ H CLAUSE) (SETQ SUBJ-VB-BACKUP-TYPE1 T) (POP) (GO SUBJ4))
            ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))
VB2
        (AND LABELTRACE (PASSING 'VB2))
        (CUT-BACK-ONE)
        (GO SUBJ3)
VBREG
        (AND LABELTRACE (PASSING 'VBREG))
        (SETR 'VG H C)
VG1
        (AND LABELTRACE (PASSING 'VG1))
        (CUT END)
        (SETQ :RESULT (ISQ MVB BE))
        (COND (:RESULT (COND ((NULL NN) (M BE) (GO FAIL)) (T (GO BE)))))
        (SETQ :RESULT (ISQ MVB VPRT))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO CHECKPASV)) (T (GO CHECKPASV)))))
        (SETQ :RESULT (AND (NQ PRT) (PARSE PRT)))
        (COND ((NULL :RESULT) (GO DPRT)))
        (FQ PRT)
        (SETQ :RESULT (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD (NB H)))))
        (COND (:RESULT (GO CHECKPASV)) (T (GO POPRT)))
DPRT
        (AND LABELTRACE (PASSING 'DPRT))
        (SETQ :RESULT (ISQ H PASV))
        (COND (:RESULT (GO CHECKPASV)))
        (SETQ :RESULT (SETQ POSITION-OF-PRT (MOVE-PTW N NW (PRT))))
        (COND ((NULL :RESULT) (GO FINDOBJ1)))
        (SETQ :RESULT (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD POSITION-OF-PRT))))
        (COND ((NULL :RESULT) (GO POPRT)))
        (SETQ :RESULT (ISQ MVB TRANS))
        (COND ((NULL :RESULT) (GO FINDOBJ1)))
        (CUT POSITION-OF-PRT)
        (SETQ :RESULT (PARSE NG OBJ OBJ1))
        (COND (:RESULT (GO POPRT)) (T (GO FINDOBJ1)))
        (CUT END)
        (SETR 'OBJ1 H C)
        (PARSE PRT)
        (FQ PRT DPRT)
        (GO FINDOBJ2)
POPRT
        (AND LABELTRACE (PASSING 'POPRT))
        (POPTO VG)
        (GO FINDOBJ1)
CHECKPASV
        (AND LABELTRACE (PASSING 'CHECKPASV))
        (SETQ :RESULT (AND (ISQ H PASV) (FQ PASV) (SETR 'OBJ1 (GETR 'SUBJECT C) C)))
        (COND (:RESULT (COND ((NULL NN) (GO FINDFAKE2)) (T (GO FINDOBJ2)))))
        (FQ ACTV)
        (GO FINDOBJ1)
BE
        (AND LABELTRACE (PASSING 'BE))
        (FQ BE)
        (AND (PARSE NIL NOT) (FQ NEG))
        (PARSE ADV VBAD)
FINDOBJ1
        (AND LABELTRACE (PASSING 'FINDOBJ1))
        (SETQ :RESULT (OR (CANPARSE 1 '(ADJG COMP) 'INT)
                (CANPARSE 1 '(NG COMP) 'INT)))
        (COND (:RESULT (COND ((NULL NN) (GO ONT)) (T (GO CHECKIT)))))
        (SETQ :RESULT (OR (CANPARSE 1 '(PREPG COMP) 'INT)
                (CANPARSE 1 '(CLAUSE RSNG ING) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG REPORT) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG TO) 'TRANS)
                (CANPARSE 1 '(PREPG LOC) 'ITRNSL)
                (CANPARSE 1 '(ADV PLACE) 'ITRNSL)))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (CANPARSE 1 '(NG) 'TRANS))
        (COND (:RESULT (COND ((NULL NN) (GO FINDFAKE2)) (T (GO FINDOBJ2)))))
FINDFAKE1
        (AND LABELTRACE (PASSING 'FINDFAKE1))
        (SETQ :RESULT (MOVE-PT C U (REL-NOT-FOUND)))
        (COND (:RESULT (GO OBJ1REL)))
        (SETQ :RESULT (AND (CANTAKE 1 '(PREPG LOC) 'ITRNSL) (MOVE-PT C U (QADJ)) (ISQ (GETR 'QADJ PT) PLACE) (FQ ITRANSL)))
        (COND (:RESULT (GO PUTLOBJ)))
        (SETQ :RESULT (CANPARSE 1 NIL 'ITRNS))
        (COND (:RESULT (GO ONT)))
GOOF1
        (AND LABELTRACE (PASSING 'GOOF1))
        (OR GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - FIRST OBJ))
        (GO FAIL)
OBJ1REL
        (AND LABELTRACE (PASSING 'OBJ1REL))
        (SETR 'OBJ1 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ1REL)
FINDOBJ2
        (AND LABELTRACE (PASSING 'FINDOBJ2))
        (SETQ :RESULT (CANPARSE 2 '(CLAUSE RSNG TO) 'TRANS2))
        (COND (:RESULT (GO FIXSUBJECT)))
        (SETQ :RESULT (OR (CANPARSE 2 '(ADV PLACE) 'TRANSL) (CANPARSE 2 '(PREPG LOC) 'TRANSL)))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (OR (CANPARSE 2 '(ADJG COMP) 'TRANSINT) (CANPARSE 2 '(NG COMP) 'TRANSINT)))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (CANPARSE 2 '(NG) 'TRANS2))
        (COND (:RESULT (GO ONT)))
FINDFAKE2
        (AND LABELTRACE (PASSING 'FINDFAKE2))
        (SETQ :RESULT (AND (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND))))
        (COND (:RESULT (GO OBJ2REL)))
        (SETQ :RESULT (AND (CANTAKE 2 '(PREPG LOC) 'TRANSL) (MOVE-PT C U (QADJ)) (ISQ (GETR 'QADJ PT) PLACE) (FQ TRANSL)))
        (COND (:RESULT (GO PUTLOBJ)))
OBJ2TO
        (AND LABELTRACE (PASSING 'OBJ2TO))
        (PARSE ADV VBAD)
        (SETQ :RESULT
        (COND ((AND (NEXTWORD? 'TO) (ISQ MVB TO2) (PARSE PREPG TO)) (SETR 'OBJ2 (GETR 'OBJ1 H) C) (FQ TRANS2TO TRANS2))
            ((AND (CQ PREPQ) (MOVE-PT H PV (QUEST)) (EQ (WORD (MOVE-PTW FW)) 'TO) (RQ PREPQ) (FQ TRANS2TOQ TRANS2) (SETR 'OBJ2 (GETR 'OBJ1 PT) C)))))
        (COND (:RESULT (GO ONT)))
        (SETQ :RESULT (CANPARSE 2 NIL 'TRANS))
        (COND (:RESULT (GO ONT)) (T (GO FAIL)))
PUTLOBJ
        (AND LABELTRACE (PASSING 'PUTLOBJ))
        (SETR 'LOBJ PT C)
        (SETR 'RELHEAD (GETR 'QADJ PT) PT)
        (SETR 'QADJ NIL PT)
        (REMOVE-F-PT 'QADJ PT)
        (GO ONT)
OBJ2REL
        (AND LABELTRACE (PASSING 'OBJ2REL))
        (SETR 'OBJ2 (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (FQ OBJ2REL)
        (GO ONT)
FIXSUBJECT
        (AND LABELTRACE (PASSING 'FIXSUBJECT))
        (SETR 'SUBJECT (GETR 'OBJ1 C) H)
        (GO ONT)
CHECKIT
        (AND LABELTRACE (PASSING 'CHECKIT))
        (SETQ :RESULT (EQ (WORD (NB (GETR 'SUBJECT C))) 'IT))
        (COND ((NULL :RESULT) (GO ONT)))
        (SETQ :RESULT (OR (AND (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ)) (AND (NQ ING) (PARSE CLAUSE RSNG ING SUBJ)) (PARSE CLAUSE REPORT)))
        (COND ((NULL :RESULT) (GO ONT)))
        (FQ IT)
        (SETR 'LOGICAL-SUBJECT H C)
        (GO ONT)
GOOF2
        (AND LABELTRACE (PASSING 'GOOF2))
        (OR GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - SECOND OBJECT))
        (GO FAIL)
ONT
        (AND LABELTRACE (PASSING 'ONT))
        (SETQ :RESULT (CQ PASV))
        (COND (:RESULT (GO PONT)))
ONT1
        (AND LABELTRACE (PASSING 'ONT1))
        (SETQ :RESULT (CALLSM (SMCL1)))
        (COND ((NULL :RESULT) (M SMCL1) (GO FAIL)))
        (SETQ :RESULT (NOT (CQ REL-NOT-FOUND)))
        (COND (:RESULT (COND ((NULL NN) (GO RETSM)) (T (GO TONT)))))
        (SETQ :RESULT (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1))
        (COND ((NULL :RESULT) (GO PREPSHORT)))
TIMEQ
        (AND LABELTRACE (PASSING 'TIMEQ))
        (RQ REL-NOT-FOUND)
        (FQ TIMEQ)
        (GO TONT)
PREPSHORT
        (AND LABELTRACE (PASSING 'PREPSHORT))
        (SETQ :RESULT (AND (NQ PREP) (PARSE PREPG)))
        (COND ((NULL :RESULT) (M ONT-SHORT-PREP) (GO FAIL)))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND ((NULL :RESULT) (M ONT:) (GO FAIL)))
        (SETQ :RESULT (CQ REL-NOT-FOUND))
        (COND (:RESULT (COND ((NULL NN) (M ONT-NOT-FOUND) (GO FAIL)) (T (GO PREPSHORT))))
        (T (GO TONT)))
PONT
        (AND LABELTRACE (PASSING 'PONT))
        (AND (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))
        (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)
        (GO ONT1)
TONT
        (AND LABELTRACE (PASSING 'TONT))
        (SETQ :RESULT (SETQ POSITION-OF-PTW N))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO RETSM)) (T (GO RETSM)))))
NPASV
        (AND LABELTRACE (PASSING 'NPASV))
        (SETQ :RESULT (AND (NQ PREP) (PARSE PREPG) (CALLSM (SMRELATE H))))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (AND (NQ TIMW) (PARSE ADV TIMW) (OR (CALLSM (SMTIME)) (GO FAIL))))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT
        (AND (NOT (CQ BE)) (PARSE ADJG ADV) (OR (CALLSM (SMRELATE H)) (GO FAIL))))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (AND (PARSE NG TIME) (OR (CALLSM (SMTIME)) (GO FAIL))))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT
        (AND (NQ PLACE) (PARSE ADV PLACE) (OR (CALLSM (SMPLACE)) (GO FAIL))))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT
        (AND (NQ BINDER) (PARSE CLAUSE BOUND) (OR (CALLSM (SMBIND)) (GO FAIL))))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (AND (NEXTWORD? 'TO) (PARSE CLAUSE TO ADJUNCT) (OR (CALLSM (SMTOADJ)) (GO FAIL))))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (EQ N POSITION-OF-PTW))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO RETSM)) (T (GO TONT)))))
        (SETQ :RESULT (OR (NOT (CQ TOPLEVEL)) (NQ SPECIAL)))
        (COND (:RESULT (GO RETSM)))
        (ERT CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL)
        (GO FAIL)
THERE
        (AND LABELTRACE (PASSING 'THERE))
        (FQ THERE)
        (CUT END)
        (SETQ :RESULT (PARSE ADV TIMW))
        (COND ((AND (NULL NN) :RESULT) (M THERE) (GO FAIL)))
        (SETQ :RESULT (AND (PARSE VG) (ISQ MVB BE)))
        (COND (:RESULT (COND ((NULL NN) (M THERE) (GO FAIL)) (T (GO THEF))))
        (T (GO NOTHE)))
THERQ
        (AND LABELTRACE (PASSING 'THERQ))
        (SETQ :RESULT (ISQ (MOVE-PT H PV (QAUX)) BE))
        (COND (:RESULT (GO THERQ2)))
        (SETQ :RESULT (AND (NQ TIMW) (PARSE ADV TIMW)))
        (COND ((AND (NULL NN) :RESULT) (M THEREQ) (GO FAIL)))
        (SETQ :RESULT (AND (PARSE VG) (ISQ MVB BE)))
        (COND (:RESULT (GO THERQ2)))
        (RQ POLR2)
        (GO NOTHE)
THERQ2
        (AND LABELTRACE (PASSING 'THERQ2))
        (FQ SUBJTQ)
        (FQ THERE)
        (SETQ :RESULT (CQ POLAR))
        (COND (:RESULT (GO THEF)) (T (GO ONT)))
THEF
        (AND LABELTRACE (PASSING 'THEF))
        (SETQ :RESULT (AND (NQ ADV) (PARSE ADV TIMW)))
        (COND ((AND (NULL NN) :RESULT) (M THEF) (GO FAIL)))
        (SETQ :RESULT (PARSE NG SUBJ SUBJT))
        (COND ((NULL :RESULT) (GO THERREL)))
        (FQ THERE)
        (SETR 'SUBJECT H C)
        (GO ONT)
THERREL
        (AND LABELTRACE (PASSING 'THERREL))
        (SETQ :RESULT (MOVE-PT C U (REL-NOT-FOUND)))
        (COND ((NULL :RESULT) (GO NOTHE)))
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (GO ONT)
NOTHE
        (AND LABELTRACE (PASSING 'NOTHE))
        (RQ THERE)
        (POP THERE)
        (AND (NQ ADV) (PARSE ADV PLACE))
        (GO THER2)
IMPER
        (AND LABELTRACE (PASSING 'IMPER))
        (SETQ :RESULT (PARSE NG TIME))
        (COND ((AND (NULL NN) :RESULT) (GO IMPOP)))
        (SETQ :RESULT (AND (NQ ADV) (PARSE ADJG ADV VBAD)))
        (COND ((AND (NULL NN) :RESULT) (GO IMPOP)))
        (SETQ :RESULT (AND (NQ ADV) (PARSE ADV TIMW)))
        (COND ((AND (NULL NN) :RESULT) (GO IMPOP)))
IMPE
        (AND LABELTRACE (PASSING 'IMPE))
        (SETQ :RESULT (PARSE VG IMPER))
        (COND ((NULL :RESULT) (GO IMPOP)))
        (FQ IMPER)
        (GO VG1)
IMPOP
        (AND LABELTRACE (PASSING 'IMPOP))
        (SETQ :RESULT (POP NIL))
        (COND (:RESULT (GO IMPE)) (T (M IMPOP) (GO FAIL)))
QUEST
        (AND LABELTRACE (PASSING 'QUEST))
        (FQ QUEST)
        (SETQ :RESULT (NQ PREP))
        (COND ((NULL :RESULT) (GO NGQUES)))
        (SETQ :RESULT (PARSE PREPG))
        (COND ((NULL :RESULT)
        (COND ((NULL NN) (M PREPQ-INCOMPLETE) (GO FAIL)) (T (GO NGQUES)))))
        (SETQ :RESULT (ISQ H QUEST))
        (COND ((NULL :RESULT) (GO QUEST)))
        (SETR 'QADJ H C)
        (GO POLAR)
NGQUES
        (AND LABELTRACE (PASSING 'NGQUES))
        (SETQ :RESULT (PARSE NG QUEST))
        (COND (:RESULT (GO NGQST)))
        (SETQ :RESULT
        (OR (AND (NEXTWORD? 'HOW) (PARSE ADJG QUEST) (SETR 'RELHEAD H C)) (AND (NQ QADJ) (PARSE QADJ) (FQ QADJ) (SETR 'QADJ H C))))
        (COND (:RESULT (GO POLAR)) (T (GO POLAR)))
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))
ADJQS
        (AND LABELTRACE (PASSING 'ADJQS))
        (GO RETURN)
NGQST
        (AND LABELTRACE (PASSING 'NGQST))
        (SETR 'RELHEAD H C)
NGQST2
        (AND LABELTRACE (PASSING 'NGQST2))
        (CUT END)
        (SETR 'SUBJECT H C)
        (AND (NQ ADV) (PARSE ADJG ADV VBAD))
        (COND ((PARSE VG NAUX) (FQ SUBJQ) (GO VG1))
        ((NQ VB) (FQ REL-NOT-FOUND) (GO POLAR))
        (T (MOVE-PTW N PW) (POP NG QUEST) (CUT PTW) (GO NGQUES)))
QUEST2
        (AND LABELTRACE (PASSING 'QUEST2))
        (SETQ :RESULT (AND (NEXTWORD? 'THERE) (PARSE NIL THERE)))
        (COND (:RESULT (GO THERQ)) (T (GO SUBF)))
SUBF
        (AND LABELTRACE (PASSING 'SUBF))
        (SETQ :RESULT (PARSE NG SUBJ))
        (COND (:RESULT (COND ((NULL NN) (GO SUBJ1)) (T (GO SUBREG)))))
        (RQ REL-NOT-FOUND)
        (GO BE)
POLAR
        (AND LABELTRACE (PASSING 'POLAR))
        (SETQ :RESULT (AND (NQ VB) (PARSE VB AUX (QAUX)) (SETR 'QAUX H C) (CALLSM (SMVAUX)) (SETMVB H)))
        (COND ((NULL :RESULT) (GO QCHOP)))
        (OR (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
        (FQ POLR2)
        (GO QUEST2)
QCHOP
        (AND LABELTRACE (PASSING 'QCHOP))
        (ERT CLAUSE: QCHOP)
        (SETQ :RESULT (POPTO CLAUSE BOUND))
        (COND (:RESULT (GO BICUT)) (T (M QCHOP) (GO FAIL)))
SEC
        (AND LABELTRACE (PASSING 'SEC))
        (COND ((CQ BOUND) (GO BOUND))
        ((CQ TO) (GO TO))
        ((CQ RSQ) (GO RSQ))
        ((CQ REPORT) (GO REPORT))
        ((CQ ING) (GO ING))
        (T (MQ RSNG-TYPE) (GO FAIL)))
BOUND
        (AND LABELTRACE (PASSING 'BOUND))
        (SETQ :RESULT (PARSE BINDER))
        (COND ((NULL :RESULT)
        (COND ((NULL NN) (M BINDER) (GO FAIL)) (T (M BOUND) (GO FAIL)))))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RSQ
        (AND LABELTRACE (PASSING 'RSQ))
        (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
        (SETQ :RESULT (CQ PREPREL))
        (COND ((NULL :RESULT) (GO RSQ2)))
        (PARSE PREPG PRONREL)
        (SETR 'QADJ H C)
        (GO REPORT)
RSQ2
        (AND LABELTRACE (PASSING 'RSQ2))
        (COND ((PARSE VG EN PASV)
        (OR (ISQ MVB TRANS) (GO FAIL))
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (GO VG1))
        ((PARSE VG ING) (SETR 'SUBJECT (GETR 'RELHEAD C) C) (GO VG1))
        ((NQ PRONREL) (PARSE NG RELWD) (GO REL))
        ((CQ COMPONENT)
        (SETR 'RELHEAD (GETR 'RELHEAD (MOVE-PT C PC)) C)
        (GO REL))
        ((PARSE NG SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
        (T (GO FAIL)))
REL
        (AND LABELTRACE (PASSING 'REL))
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (SETQ :RESULT (PARSE VG))
        (COND (:RESULT (GO VG1)))
        (FQ REL-NOT-FOUND)
        (GO SUBJ)
TO
        (AND LABELTRACE (PASSING 'TO))
        (SETQ :RESULT (AND (CQ COMPONENT) (PARSE VG TO TODEL)))
        (COND (:RESULT (GO VG1)))
        (SETQ :RESULT (NEXTWORD? 'FOR))
        (COND ((NULL :RESULT) (GO TO1)))
        (PARSE NIL FOR)
        (FQ FOR)
        (PARSE NG SUBJ TOSUBJ)
        (SETR 'SUBJECT H C)
TO1
        (AND LABELTRACE (PASSING 'TO1))
        (SETQ :RESULT (PARSE VG TO))
        (COND (:RESULT (GO VG1)) (T (M TO) (GO FAIL)))
ING
        (AND LABELTRACE (PASSING 'ING))
        (SETQ :RESULT (MOVE-PTW N NW (ING)))
        (COND ((NULL :RESULT) (GO FAIL)))
        (SETQ :RESULT (OR (NQ ING) (CQ OBJ2) (AND (PARSE NG SUBJ INGSUBJ) (SETR 'SUBJECT H C) (FQ SUBING) (RQ ING))))
        (COND ((AND (NULL NN) :RESULT) (M ING) (GO FAIL)))
        (SETQ :RESULT (PARSE VG ING))
        (COND (:RESULT (GO VG1)) (T (M ING) (GO FAIL)))
REPORT
        (AND LABELTRACE (PASSING 'REPORT))
        (AND (NEXTWORD? 'THAT) (PARSE NIL THAT) (FQ THAT))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RETSM
        (AND LABELTRACE (PASSING 'RETSM))
        (OR (CALLSM (SMCL2)) (GO FAIL))
        (GO RETURN)
FAIL
        (SETQ MES ME)
        (SETQ N (OR (N RE) NB))
        (RETURN NIL)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN NG NIL
    (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT)
        (SETQ NN T)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (REVERSE REST)) (SETQ NB (OR (NB RE) N)) N (SETQ H RE) NIL))
        (SETR 'PARENT PARENT C)
ENTERING-NG
        (AND LABELTRACE (PASSING 'ENTERING-NG))
NGSTART
        (AND LABELTRACE (PASSING 'NGSTART))
        (COND ((CQ RELWD) (GO RELWD))
        ((CQ QUEST) (GO QUEST))
        ((OR (NQ QDET) (NQ QPRON)) (FQ QUEST) (GO QUEST))
        ((CQ TIME) (GO TIME))
        ((NQ PROPN) (GO PROPN))
        ((NQ TPRON) (GO TPRON))
        ((NQ EVERPRON) (GO EVERPRON))
        ((NQ PRON) (GO PRON)))
LOOK
        (AND LABELTRACE (PASSING 'LOOK))
        (COND ((NQ DET) (GO DET))
        ((NQ NUM) (GO NUM))
        ((OR (NQ ING) (NQ EN) (NQ ADJ)) (GO ADJ))
        ((NQ CLASF) (GO CLASF))
        ((NQ NUMD) (GO NUMD))
        ((NEXTWORD? 'AT) (GO AT))
        ((NEXTWORD? 'AS) (GO AS))
        ((NQ NOUN) (GO NOUN))
        ((NQ TIMORD) (GO TIMORD))
        ((AND (CQ COMPONENT) (ISQ (MOVE-PT PC) QUEST)) (GO QUEST))
        ((MQ START) (GO FAIL)))
START
        (AND LABELTRACE (PASSING 'START))
PROPN
        (AND LABELTRACE (PASSING 'PROPN))
        (PARSE PROPN)
        (FQ DEF PROPNG)
        (SETQ :RESULT (ISQ H POSS))
        (COND (:RESULT (GO PROPS)))
        (SETQ :RESULT (AND NN (NQ PROPN)))
        (COND (:RESULT (GO PROPN)))
PROPS
        (AND LABELTRACE (PASSING 'PROPS))
        (OR (CALLSM (SMPROP)) (GO FAIL))
        (SETQ :RESULT (ISQ H POSS))
        (COND (:RESULT (GO POSS)) (T (GO PRAG)))
PRON
        (AND LABELTRACE (PASSING 'PRON))
        (SETQ :RESULT (PARSE PRON POSS))
        (COND (:RESULT (COND ((NULL NN) (GO RED2)) (T (GO POSS)))))
PRON2
        (AND LABELTRACE (PASSING 'PRON2))
        (SETQ :RESULT (CQ NPRON))
        (COND (:RESULT (M NPRON) (GO FAIL)))
        (SETQ :RESULT (OR (AND (CQ SUBJ) (PARSE PRON SUBJ)) (AND (OR (CQ OBJ) (CQ TOSUBJ) (CQ INGSUBJ)) (PARSE PRON OBJ)) (CQ INGSUBJ)))
        (COND ((NULL :RESULT) (M PRON) (GO FAIL)))
        (FQ PRONG DEF)
PRON3
        (AND LABELTRACE (PASSING 'PRON3))
        (SETQ :RESULT (CALLSM (SMPRON H)))
        (COND ((NULL :RESULT) (GO FAIL)))
PRAG
        (AND LABELTRACE (PASSING 'PRAG))
        (SETR 'HEAD H C)
        (MOVE-PT H)
        (TRNSF NS NPL NFS NEG)
        (GO RETURN)
TPRON
        (AND LABELTRACE (PASSING 'TPRON))
        (PARSE TPRON)
        (FQ TPRON)
        (MOVE-PT H)
        (TRNSF NS NPL ANY NEG)
        (SETR 'HEAD C H)
        (AND NN (NQ ADJ) (PARSE ADJ))
        (GO SMNG)
EVERPRON
        (AND LABELTRACE (PASSING 'EVERPRON))
        (SETQ :RESULT (AND (PARSE PRON EVERPRON) (CALLSM (SMPRON H))))
        (COND ((NULL :RESULT) (GO FAIL)))
        (SETQ :RESULT (AND (PARSE CLAUSE RSQ NOREL) (CALLSM (SMRELATE H))))
        (COND (:RESULT (GO RETSM)) (T (GO FAIL)))
AS
        (AND LABELTRACE (PASSING 'AS))
        (SETQ :RESULT (AND (PARSE NIL AS) (PARSE NUMD NUMDAS) NN (PARSE NIL AS)))
        (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO NUMD2)))) (T (M AS) (GO FAIL)))
AT
        (AND LABELTRACE (PASSING 'AT))
        (SETQ :RESULT (AND (PARSE NIL AT) (PARSE NUMD NUMDAT)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (M AT) (GO FAIL)) (T (M AT) (GO FAIL)))))
NUMD2
        (AND LABELTRACE (PASSING 'NUMD2))
        (SETQ :RESULT (AND (PARSE NUM) (FQ NUM NUMD)))
        (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO DET1)))) (T (M NUMD2) (GO FAIL)))
NUMD
        (AND LABELTRACE (PASSING 'NUMD))
        (SETQ :RESULT (PARSE NUMD NUMDAN))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO ND3)))))
        (SETQ :RESULT (PARSE NIL THAN))
        (COND (:RESULT (COND ((NULL NN) (GO POPCOM)) (T (GO NUMD2)))) (T (GO INCOM)))
ND3
        (AND LABELTRACE (PASSING 'ND3))
        (SETQ :RESULT (PARSE NUMD NUMDALONE))
        (COND (:RESULT (COND ((NULL NN) (M NUMD) (GO FAIL)) (T (GO NUMD2)))) (T (M NUMD) (GO FAIL)))
TIME
        (AND LABELTRACE (PASSING 'TIME))
        (SETQ :RESULT (AND (NQ TIME) (PARSE NOUN TIME)))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (MOVE-PTW N NW (TIM1)))
        (COND (:RESULT (GO LOOK)) (T (M TIME) (GO FAIL)))
TIMORD
        (AND LABELTRACE (PASSING 'TIMORD))
        (SETQ :RESULT (PARSE ORD TIMORD))
        (COND ((NULL :RESULT) (GO FAIL)))
        (SETQ :RESULT (AND (PARSE NOUN TIM1) (FQ DET DEF) (CALLSM (SMNGTIME))))
        (COND (:RESULT (GO RETURN)) (T (GO FAIL)))
DET
        (AND LABELTRACE (PASSING 'DET))
        (PARSE DET)
        (FQ DET)
        (MOVE-PT H)
        (SETQ :RESULT (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR))
        (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO IND)))) (T (M BUG) (GO FAIL)))
IND
        (AND LABELTRACE (PASSING 'IND))
        (SETQ :RESULT (AND (EQ (WORD (NB H)) 'ALL) (EQ (WORD N) 'THE) (PARSE DET) (FQ DEF)))
        (COND (:RESULT (COND ((NULL NN) (M THE) (GO FAIL)) (T (GO NUM)))))
        (SETQ :RESULT (AND (ISQ H QNTFR) (FQ QNTFR)))
        (COND (:RESULT (GO QNUM)))
ORD
        (AND LABELTRACE (PASSING 'ORD))
        (SETQ :RESULT (AND (PARSE ORD) (FQ ORD)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO NUM)))))
        (SETQ :RESULT (AND (NEXTWORD? 'OF) (ISQ (MOVE-PTW N NW) MONTH) (PARSE NIL OF) (PARSE NOUN MONTH) (FQ DATE)))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (CQ DEF))
        (COND ((NULL :RESULT) (GO ADJ)))
NUM
        (AND LABELTRACE (PASSING 'NUM))
        (SETQ :RESULT (PARSE NUM))
        (COND ((NULL :RESULT) (GO ADJ)))
        (FQ NUM)
        (SETQ :RESULT (CQ DET))
        (COND ((NULL :RESULT) (GO DET1)))
        (SETQ :RESULT (COND ((AND (ISQ H NS) (CQ NS)) (RQ NPL PART)) ((CQ NPL) (RQ NS PART))))
        (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO ADJ)))) (T (M NUM) (GO FAIL)))
DET1
        (AND LABELTRACE (PASSING 'DET1))
        (COND ((ISQ H NS) (FQ NS)) (T (FQ NPL)))
        (OR NN (AND (FQ NUMBER) (GO INCOM)))
NUMBER
        (AND LABELTRACE (PASSING 'NUMBER))
        (FQ DET)
        (SETQ :RESULT (NQ OF))
        (COND (:RESULT (GO OF)) (T (GO ADJ)))
QNUM
        (AND LABELTRACE (PASSING 'QNUM))
        (SETQ :RESULT (ISQ H NONUM))
        (COND (:RESULT (GO OF)))
        (SETQ :RESULT (AND (PARSE NUM) (FQ NUM)))
        (COND ((NULL :RESULT) (GO OF)))
        (SETQ :RESULT (COND ((EQ (SM H) 1) (AND (CQ NS) (RQ NPL))) ((CQ NPL) (RQ NS))))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (M NUMD) (GO FAIL)))))
        (SETQ :RESULT (EQ (WORD (NB H)) 'NO))
        (COND (:RESULT (GO ADJ)))
OF
        (AND LABELTRACE (PASSING 'OF))
        (SETQ :RESULT (AND (NQ OF) (PARSE PREPG OF)))
        (COND (:RESULT (GO SMOF)) (T (GO NONE)))
SMOF
        (AND LABELTRACE (PASSING 'SMOF))
        (FQ OF)
        (SETQ :RESULT (OR (CALLSM (SMNGOF)) (NOT (POP))))
        (COND (:RESULT (GO RETSM)) (T (GO INCOM)))
NONE
        (AND LABELTRACE (PASSING 'NONE))
        (SETQ :RESULT (EQ (WORD (NB H)) 'NONE))
        (COND (:RESULT (GO INCOM)) (T (GO ADJ)))
ADJ
        (AND LABELTRACE (PASSING 'ADJ))
        (SETQ :RESULT (PARSE ADJ))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO EPR)))))
        (AND (ISQ H COMPAR) (FQ COMPARATIVE-MODIFIER) (SETR 'COMPARATIVE-MODIFIER H C))
        (GO ADJ)
EPR
        (AND LABELTRACE (PASSING 'EPR))
        (SETQ :RESULT (OR (ISQ H SUP) (ISQ H COMPAR)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO CLASF)))))
        (FQ ADJ)
        (AND (NEXTWORD? 'OF)
        (PARSE PREPG OF)
        (OR (CALLSM (SMNGOF)) (GO FAIL))
        (FQ OF)
        (GO RETSM))
CLASF
        (AND LABELTRACE (PASSING 'CLASF))
        (SETQ :RESULT (OR (PARSE VB ING (CLASF)) (PARSE VB EN (CLASF)) (PARSE CLASF)))
        (COND (:RESULT (COND ((NULL NN) (GO REDUC)) (T (GO CLASF)))))
NOUN
        (AND LABELTRACE (PASSING 'NOUN))
        (SETQ :RESULT (PARSE NOUN))
        (COND ((NULL :RESULT) (GO RED2)))
        (SETQ :RESULT (AND (CQ TIME) (NOT (ISQ H TIM1))))
        (COND (:RESULT (GO RED1)))
        (SETQ T1 FE)
        (COND ((AND (ISQ H MASS) (OR (CQ PART) (NOT (CQ DET)))) (FQ MASS)))
        (COND ((NOT (ISQ H NPL)) (RQ NPL PART)))
        (COND ((NOT (ISQ H NS)) (RQ NS)))
        (COND ((AND (NOT (CQ DET)) (NOT (CQ NUMD))) (MOVE-PT H) (TRNSF NPL MASS)))
        (SETQ :RESULT (MEET FE '(NS NPL PART MASS)))
        (COND ((NULL :RESULT) (GO RED0)))
        (SETQ :RESULT (NEXTWORD? 'THAN))
        (COND ((NULL :RESULT) (GO SMNG)))
        (FQ THAN)
SMNG
        (AND LABELTRACE (PASSING 'SMNG))
        (SETR 'HEAD H C)
        (SETQ :RESULT (AND (CQ OBOFJ) (NOT (CQ DEF))))
        (COND (:RESULT (GO FAIL)))
        (OR (CALLSM (SMNG1)) (GO FAIL))
        (SETQ :RESULT (NOT (ISQ H POSS)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO RETSM)) (T (GO POSS)))))
        (SETQ :RESULT (AND (CQ THAN) (PARSE ADJG)))
        (COND ((NULL :RESULT) (GO RSQ-TO)))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (GO RETSM)) (T (GO FAIL)))
RSQ-TO
        (AND LABELTRACE (PASSING 'RSQ-TO))
        (SETQ :RESULT (AND (NEXTWORD? 'TO) (MEET FE '(COMP SUBJ)) (PARSE CLAUSE RSQ TO) (OR (CALLSM (SMRELATE H)) (GO POPRET))))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (AND (OR (NEXTWORD? 'AS) (NQ COMPAR)) (PARSE ADJG THANNEED)))
        (COND ((NULL :RESULT) (GO PREPNG)))
        (AND (NULL N) (CQ SUBJ) (ISQ (MOVE-PT C PV) AUX) (ISQ PT BE) (GO POPRET))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (COND ((NULL NN) (GO RETSM)) (T (GO RSQ-TO)))) (T (GO POPRET)))
PREPNG
        (AND LABELTRACE (PASSING 'PREPNG))
        (SETQ :RESULT (AND (NQ PREP) (NOT (OR (AND (NQ PLACE) (CQ NOLOC)) (AND (CQ OBJ1) (ISQ MVB TRANSL) (NOT (ISQ (MOVE-PT C U) QUEST))))) (PARSE PREPG Q)))
        (COND ((NULL :RESULT) (GO DISGRSQ)))
        (AND (NULL N)
        (CQ SUBJ)
        (ISQ (MOVE-PT C PV) AUX)
        (ISQ PT BE)
        (NOT (ISQ (MOVE-PT U) NGQ))
        (GO POPRET))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (COND ((NULL NN) (GO RETSM)) (T (GO RSQ-TO)))) (T (GO POPRET)))
DISGRSQ
        (AND LABELTRACE (PASSING 'DISGRSQ))
        (SETQ :RESULT (EQ (CAR MES) 'PREP-WHICH))
        (COND ((NULL :RESULT) (GO RSQ)))
        (SETQ MES (CDR MES))
        (SETQ :RESULT (PARSE CLAUSE RSQ PREPREL))
        (COND (:RESULT (COND ((NULL NN) (GO RETSM)) (T (GO PREPNG)))) (T (M RSQ-PREPREL) (GO FAIL)))
RSQ
        (AND LABELTRACE (PASSING 'RSQ))
        (SETQ :RESULT (AND (ISQ (MOVE-PT C U) POLR2) (CQ SUBJ) (NQ VB) (NOT (CQ SUBJT)) (NOT (ISQ PT QADJ))))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (PARSE CLAUSE RSQ))
        (COND ((NULL :RESULT) (GO RETSM)))
        (SETQ :RESULT (CALLSM (SMRELATE H)))
        (COND (:RESULT (GO RETSM)) (T (GO POPRET)))
RED0
        (AND LABELTRACE (PASSING 'RED0))
        (SETQ FE T1)
RED1
        (AND LABELTRACE (PASSING 'RED1))
        (POP)
RED2
        (AND LABELTRACE (PASSING 'RED2))
        (COND ((NULL H) (MQ NO) (GO FAIL))
            ((ISQ H NUMBER) (GO INCOM))
            ((AND (ISQ H POSS) (OR (ISQ H PRON) (AND (MOVE-PT H DLC) (ISQ PT PRON)))) (POP) (GO PRON2))
            ((AND (NULL (CDR H)) (CQ DEFPOSS)) (GO POSSDEF))
            ((AND (CQ QUEST) (NULL (CDR H))) (GO QDETCHECK))
            ((ISQ H ADJ) (GO EPR))
            ((NOT (ISQ H CLASF)) (GO INCOM)))
REDUC
        (AND LABELTRACE (PASSING 'REDUC))
        (POP)
        (SETQ :RESULT (AND (NULL H) (NQ PROPN)))
        (COND (:RESULT (GO PROPN)) (T (GO NOUN)))
POPCOM
        (AND LABELTRACE (PASSING 'POPCOM))
        (POP)
INCOM
        (AND LABELTRACE (PASSING 'INCOM))
        (FQ INCOM)
        (SETQ :RESULT (AND (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM))))
        (COND (:RESULT (GO RETURN)))
        (SETQ :RESULT (AND (NULL CUT) (CQ NUM)))
        (COND (:RESULT (GO SMNG)))
QDETCHECK
        (AND LABELTRACE (PASSING 'QDETCHECK))
        (COND ((AND (ISQ H QDET) (ISQ (NB H) QPRON)) (POP) (GO QPRON))
            ((AND (ISQ H QDET) (ISQ (NB H) EVERPRON)) (POP) (GO EVERPRON)))
        (GO FAIL)
POSS
        (AND LABELTRACE (PASSING 'POSS))
        (OR (CALLSM (SMNG2)) (GO FAIL))
POSS2
        (AND LABELTRACE (PASSING 'POSS2))
        (SETQ :RESULT (CQ INGSUBJ))
        (COND (:RESULT (GO RETSM)))
        (SETQ H (BUILDNODE (REVERSE (CONS 'POSS (SETDIF FE '(COMPONENT)))) NB N H SM))
        (SETQ BACKREF (APPEND H (CDR BACKREF)))
        (SETQ :RESULT (SETR 'FEATURES (SETQ FE (APPEND '(POSES DET DEF NS NPL) (REVERSE REST))) C))
        (COND ((NULL :RESULT) (M BUG) (GO FAIL)))
        (SETQ :RESULT (OR (NOT NN) (ISQ H DEFPOSS)))
        (COND ((NULL :RESULT) (GO ORD)))
POSSDEF
        (AND LABELTRACE (PASSING 'POSSDEF))
        (RQ POSES DET DEF)
        (FQ POSSDEF NS NPL)
QUEST
        (AND LABELTRACE (PASSING 'QUEST))
        (SETQ :RESULT (PARSE NIL HOW))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO FAIL)) (T (GO QDET)))))
        (SETQ :RESULT (PARSE NIL MANY))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO FAIL)))))
        (FQ DET NPL INDEF HOWMANY)
        (GO OF)
QDET
        (AND LABELTRACE (PASSING 'QDET))
        (SETQ :RESULT (AND (PARSE DET QDET) (FQ DET NPL QDET NS)))
        (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO QNUM)))))
QPRON
        (AND LABELTRACE (PASSING 'QPRON))
        (SETQ :RESULT (PARSE PRON QPRON))
        (COND (:RESULT (GO PRON3)) (T (GO FAIL)))
RELWD
        (AND LABELTRACE (PASSING 'RELWD))
        (SETQ :RESULT (AND (PARSE PRONREL) (CALLSM (SMSET (SM (MOVE-PT C U U (NG)))))))
        (COND (:RESULT (GO RETURN)))
POPRET
        (AND LABELTRACE (PASSING 'POPRET))
        (POP)
RETSM
        (AND LABELTRACE (PASSING 'RETSM))
        (OR (CALLSM (SMNG2)) (GO TRYA))
        (GO RETURN)
TRYA
        (AND LABELTRACE (PASSING 'TRYA))
        (SETQ :RESULT (ISQ H NOUN))
        (COND ((NULL :RESULT) (M TRYA) (GO FAIL)))
        (POP)
        (CUT N)
UP
        (AND LABELTRACE (PASSING 'UP))
        (SETQ :RESULT (POP))
        (COND (:RESULT (GO UP)))
        (SETQ FE (REVERSE REST))
        (SMSET NIL)
        (GO NGSTART)
FAIL
        (SETQ MES ME)
        (SETQ N (OR (N RE) NB))
        (RETURN NIL)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN VG NIL
    (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT TENSE)
        (SETQ NN T)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (REVERSE REST)) (SETQ NB (OR (NB RE) N)) N (SETQ H RE) NIL))
        (SETR 'PARENT PARENT C)
ENTERING-VG
        (AND LABELTRACE (PASSING 'ENTERING-VG))
        (COND ((CQ TO) (GO TO))
            ((CQ EN) (GO EN))
            ((CQ ING) (GO ING))
            ((CQ IMPER) (GO IMPER))
            ((ISQ (MOVE-PT C U) POLR2) (GO POLR2)))
NEW
        (AND LABELTRACE (PASSING 'NEW))
        (COND ((NOT (NQ VB)) (MQ VB) (GO FAIL))
            ((AND (NQ DO) (PARSE VB AUX DO)) (GO DO))
            ((AND (NQ MODAL) (PARSE VB AUX MODAL)) (GO MODAL))
            ((AND (NQ WILL) (PARSE VB AUX WILL)) (GO WILL))
            ((AND (NQ BE) (PARSE VB AUX BE)) (GO BE))
            ((AND (NQ HAVE) (PARSE VB AUX HAVE)) (GO HAVE))
            ((NOT (PARSE VB (MVB))) (MQ VB) (GO FAIL)))
SIMPLE
        (AND LABELTRACE (PASSING 'SIMPLE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS)
        (SETQ TENSE (COND ((AND (ISQ PT PRESENT) (ISQ PT PAST)) '(PAST-PRESENT)) ((ISQ PT PAST) '(PAST)) (T '(PRESENT))))
        (GO REV)
TO
        (AND LABELTRACE (PASSING 'TO))
        (FQ NAGR)
        (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
        (SETQ :RESULT (OR (PARSE NIL TO) (CQ TODEL)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (M TO) (GO FAIL)) (T (M TO) (GO FAIL)))))
        (SETQ TENSE '(INFINITIVE))
        (GO MODAL2)
EN
        (AND LABELTRACE (PASSING 'EN))
        (FQ NAGR)
        (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
        (SETQ TENSE '(PAST))
        (SETQ :RESULT (AND (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)))
        (COND (:RESULT (GO RETSM)) (T (GO FAIL)))
ING
        (AND LABELTRACE (PASSING 'ING))
        (FQ NAGR)
        (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
INGADV
        (AND LABELTRACE (PASSING 'INGADV))
        (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (GO INGADV)))
        (SETQ TENSE '(PRESENT))
        (GO BE2)
IMPER
        (AND LABELTRACE (PASSING 'IMPER))
        (SETQ :RESULT (AND (PARSE VB DO NEG INF) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M DONT) (GO FAIL)))
        (SETQ :RESULT (AND (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG))))
        (COND (:RESULT (GO RETURN)) (T (M IMPER) (GO FAIL)))
POLR2
        (AND LABELTRACE (PASSING 'POLR2))
        (OR (SETQ PT (GETR 'QAUX (MOVE-PT C U))) (AND (BUG VG:POLR2) (GO FAIL)))
        (SETQ H (LIST (CAR PT)))
        (TRNSF NEG)
        (COND ((ISQ H DO) (GO DO))
            ((ISQ H MODAL) (GO MODAL))
            ((ISQ H WILL) (GO WILL))
            ((ISQ H BE) (GO BE))
            ((ISQ H HAVE) (GO HAVE)))
        (ERT BUG VG:POLR2VB)
        (GO FAIL)
DO
        (AND LABELTRACE (PASSING 'DO))
        (FQ DO)
        (MOVE-PT C DLC)
        (TRNSF VPL NEG INF V3PS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (T '(PRESENT))))
        (COND (NN (GO DO2)) (T (GO MVB)))
DO2
        (AND LABELTRACE (PASSING 'DO2))
        (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
ADV2
        (AND LABELTRACE (PASSING 'ADV2))
        (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV2)))))
        (SETQ :RESULT (PARSE VB (MVB) INF))
        (COND ((NULL :RESULT) (GO MVB)))
        (GO REV)
MODAL
        (AND LABELTRACE (PASSING 'MODAL))
        (FQ NAGR MODAL)
        (SETQ TENSE '(MODAL))
        (COND (NN (GO MODAL2)) (T (GO INCOMP)))
MODAL2
        (AND LABELTRACE (PASSING 'MODAL2))
        (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
ADV3
        (AND LABELTRACE (PASSING 'ADV3))
        (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV3)))))
        (COND ((PARSE VB BE INF) (GOCOND BE2 MVB))
        ((PARSE VB HAVE INF) (GOCOND HAV2 MVB))
        ((PARSE VB INF (MVB)) (GO REV))
        (T (GO INCOMP)))
WILL
        (AND LABELTRACE (PASSING 'WILL))
        (FQ NAGR)
        (SETQ TENSE '(FUTURE))
        (COND (NN (GO MODAL2)) (T (GO INCOMP)))
BE
        (AND LABELTRACE (PASSING 'BE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (T '(PRESENT))))
        (COND (NN (GO BE2)) (T (GO MVB)))
BE2
        (AND LABELTRACE (PASSING 'BE2))
        (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
ADV4
        (AND LABELTRACE (PASSING 'ADV4))
        (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
        (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV4)))))
        (COND ((AND (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))
            ((AND (NQ BE) (PARSE VB ING)) (SETQ TENSE (CONS 'PRESENT TENSE)) (GO EN2))
            ((AND (NQ ING) (PARSE VB ING (MVB))) (SETQ TENSE (CONS 'PRESENT TENSE)) (GO REV))
            ((CQ ING) (MQ ING) (GO FAIL)))
EN2
        (AND LABELTRACE (PASSING 'EN2))
        (SETQ :RESULT (PARSE VB EN (MVB)))
        (COND ((NULL :RESULT) (GO MVBE)))
        (FQ PASV)
        (GO REV)
GOING
        (AND LABELTRACE (PASSING 'GOING))
        (SETQ :RESULT (PARSE NIL TO))
        (COND ((NULL :RESULT) (GO GOI)))
        (SETQ :RESULT (NQ INF))
        (COND (:RESULT (GO GOING2)))
        (POP)
GOI
        (AND LABELTRACE (PASSING 'GOI))
        (SETQ TENSE (CONS 'PRESENT TENSE))
        (GO MVB)
GOING2
        (AND LABELTRACE (PASSING 'GOING2))
        (SETQ TENSE (CONS 'FUTURE TENSE))
        (GO MODAL2)
MVBE
        (AND LABELTRACE (PASSING 'MVBE))
        (SETQ :RESULT (ISQ (MOVE-PT H PV (VB)) AUX))
        (COND ((NULL :RESULT) (GO MVB)))
        (SETQ :RESULT (ISQ PT BE))
        (COND ((NULL :RESULT) (M MVBE) (GO FAIL)))
        (SETMVB PT)
        (GO REV)
HAVE
        (AND LABELTRACE (PASSING 'HAVE))
        (MOVE-PT C DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST)) (T '(PRESENT))))
        (COND (NN (GO HAV2)) (T (GO MVB)))
HAV2
        (AND LABELTRACE (PASSING 'HAV2))
        (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
        (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
ADV5
        (AND LABELTRACE (PASSING 'ADV5))
        (SETQ :RESULT (PARSE ADV))
        (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV5)))))
        (SETQ :RESULT (PARSE VB BE EN))
        (COND ((NULL :RESULT) (GO HAV3)))
        (SETQ TENSE (CONS 'PAST TENSE))
        (COND (NN (GO BE2)) (T (GO MVB)))
HAV3
        (AND LABELTRACE (PASSING 'HAV3))
        (SETQ :RESULT (PARSE VB (MVB) EN))
        (COND ((NULL :RESULT) (GO MVB)))
        (SETQ TENSE (CONS 'PAST TENSE))
        (GO REV)
INCOMP
        (AND LABELTRACE (PASSING 'INCOMP))
        (FQ INCOMP)
        (GO FAIL)
MVB
        (AND LABELTRACE (PASSING 'MVB))
        (SETQ :RESULT (EQ (FE MVB) (FE H)))
        (COND (:RESULT (GO MVB2)))
        (POP VB)
        (SETQ :RESULT (PARSE VB (MVB)))
        (COND ((NULL :RESULT) (M MVB) (GO FAIL)))
MVB2
        (AND LABELTRACE (PASSING 'MVB2))
        (GO REV)
REV
        (AND LABELTRACE (PASSING 'REV))
        (SETR 'TENSE TENSE C)
        (AND NN (PARSE NIL NOT) (FQ NEG))
        (COND ((OR (EQUAL TENSE '(PAST)) (CQ NAGR) (ISQ (MOVE-PT C U) IMPER) (ISQ PT THERE) (ISQ PT RSNG)) (GO NAUX))
            ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))
            (T (ERTERR VG -- NO SUBJECT TO CHECK FOR AGREEMENT)))
        (SETQ T3 NIL)
        (COND ((ISQ PT NFS) (OR (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))
            ((ISQ PT CLAUSE) (OR (SETQ T3 (CQ V3PS)) (GO NAGR)))
            ((OR (ISQ PT NS) (ISQ PT MASS)) (OR (AND (CQ V3PS) (SETQ T3 T)) (FESET PT (SETDIF (FE PT) '(NS MASS))))))
        (COND ((OR (ISQ PT PART) (ISQ PT NPL)) (OR (AND (MEET FE '(INF VPL)) (SETQ T3 T)) (FESET PT (SETDIF (FE PT) '(PART NPL))))))
NAGR
        (AND LABELTRACE (PASSING 'NAGR))
        (SETQ :RESULT (OR T3 (AND (EQUAL '(PAST-PRESENT) TENSE) (SETQ TENSE '(PAST)))))
        (COND ((NULL :RESULT) (M NAGR) (GO FAIL)))
NAUX
        (AND LABELTRACE (PASSING 'NAUX))
        (SETMVB (OR (MOVE-PT H PV (MVB)) MVB))
        (SETQ :RESULT
        (AND (CQ NAUX) (ISQ (MOVE-PT H PV (VB)) AUX) (NOT (MOVE-PT PV PV (VB)))))
        (COND (:RESULT (M NAUX) (GO FAIL)) (T (GO RETSM)))
POPV
        (AND LABELTRACE (PASSING 'POPV))
        (ERT POPV)
        (GO FAIL)
RETSM
        (AND LABELTRACE (PASSING 'RETSM))
        (SETQ :RESULT (CALLSM (SMVG)))
        (COND (:RESULT (GO RETURN)) (T (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (OR (N RE) NB))
        (RETURN NIL)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN PREPG NIL
    (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT)
        (SETQ NN T)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (REVERSE REST)) (SETQ NB (OR (NB RE) N)) N (SETQ H RE) NIL))
        (SETR 'PARENT PARENT C)
ENTERING-PREPG
        (AND LABELTRACE (PASSING 'ENTERING-PREPG))
ADV
        (AND LABELTRACE (PASSING 'ADV))
        (SETQ :RESULT (AND (NQ PREPADV) (PARSE ADV PREPADV)))
        (COND (:RESULT (COND ((NULL NN) (M PREPADV) (GO FAIL)) (T (GO ADV)))))
        (SETQ :RESULT (COND ((CQ AGENT) (NEXTWORD? 'BY)) ((CQ LOC) (NQ PLACE)) ((CQ Q) (NOT (NQ MOTOR))) (T)))
        (COND ((NULL :RESULT) (M PREP) (GO FAIL)))
        (SETQ :RESULT (PARSE PREP))
        (COND ((NULL :RESULT) (M PREP) (GO FAIL)))
        (MOVE-PT H)
        (TRNSF PLACE TIME)
        (SETQ T1 H)
        (AND (NQ PREP2)
            (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N))) (PARSE PREP2))
                ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N) (WORD (CDR N)))) (PARSE PREP2) (PARSE PREP2)))
            (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))
            (SETR 'PARENT C T1))
        (SETQ :RESULT (ISQ H NEED2))
        (COND (:RESULT (M NEED2) (GO FAIL)))
        (SETR 'HEAD T1 C)
        (OR NN (GO SHORT))
        (COND ((EQ (WORD H) 'BY) (FQ AGENT)))
QUEST
        (AND LABELTRACE (PASSING 'QUEST))
        (SETQ :RESULT (CQ QUEST))
        (COND ((NULL :RESULT) (GO NG)))
        (SETQ :RESULT (PARSE NG QUEST OBJ))
        (COND (:RESULT (GO OBJR)) (T (M PREPQUEST) (GO FAIL)))
        (SETQ :RESULT (AND (CQ OF) (PARSE NG OFOBJ)))
        (COND (:RESULT (GO OBJR)))
NG
        (AND LABELTRACE (PASSING 'NG))
        (SETQ :RESULT (PARSE NG OBJ))
        (COND (:RESULT (GO OBJR)))
REL
        (AND LABELTRACE (PASSING 'REL))
        (SETQ :RESULT (NEXTWORD? 'WHICH))
        (COND ((NULL :RESULT) (GO REST)))
        (SETQ :RESULT (ISQ (MOVE-PT U) CLAUSE))
        (COND ((NULL :RESULT) (M PREP-WHICH) (GO FAIL)))
        (SETQ :RESULT (ISQ PT PRONREL))
        (COND ((NULL :RESULT) (GO PRONREL)))
        (SETQ MES (CDR MES))
        (GO P-RELWRD)
PRONREL
        (AND LABELTRACE (PASSING 'PRONREL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PRONREL PT)
P-RELWRD
        (AND LABELTRACE (PASSING 'P-RELWRD))
        (PARSE NG RELWD OBJ)
        (SETR 'OBJ1 (GETR 'HEAD PT) C)
        (GO RETT)
REST
        (AND LABELTRACE (PASSING 'REST))
        (SETQ :RESULT (PARSE CLAUSE RSNG ING))
        (COND (:RESULT (GO OBJR)) (T (GO SHORT)))
OBJR
        (AND LABELTRACE (PASSING 'OBJR))
        (SETR 'OBJ1 H C)
        (GO RETT)
SHORT
        (AND LABELTRACE (PASSING 'SHORT))
        (SETQ :RESULT (MEET FE '(NOSHORT Q)))
        (COND (:RESULT (M SHORT) (GO FAIL)))
        (OR (ISQ (MOVE-PT C U) REL-NOT-FOUND) (ISQ (GETR 'QUESTION-ELEMENT PT) QADJ) (GO FAIL))
        (REMOVE-F-PT 'REL-NOT-FOUND PT)
        (ADD-F-PT 'PREPREL PT)
        (SETR 'OBJ1 (GETR 'RELHEAD (MOVE-PT C U)) C)
RETT
        (AND LABELTRACE (PASSING 'RETT))
        (AND (OR (ISQ H QUEST) (AND (ISQ H COMPOUND) (MOVE-PT H H PV (QUEST)))) (FQ QUEST))
        (SETQ :RESULT (CALLSM (SMADJG-PREPG)))
        (COND (:RESULT (GO RETURN)) (T (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (OR (N RE) NB))
        (RETURN NIL)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN ADJG NIL
    (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT)
        (SETQ NN T)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (REVERSE REST)) (SETQ NB (OR (NB RE) N)) N (SETQ H RE) NIL))
        (SETR 'PARENT PARENT C)
ENTERING-ADJG
        (AND LABELTRACE (PASSING 'ENTERING-ADJG))
COMPCHECK
        (AND LABELTRACE (PASSING 'COMPCHECK))
        (SETQ :RESULT (AND (MOVE-PT C U (BE)) (NOT (CQ COMP))))
        (COND (:RESULT (GO FAIL)))
        (SETQ :RESULT (ISQ (MOVE-PT C U) THAN))
        (COND ((NULL :RESULT) (GO DISP)))
        (SETR 'HEAD (GETR 'COMPARATIVE-MODIFIER PT) C)
        (GO THAN)
DISP
        (AND LABELTRACE (PASSING 'DISP))
        (SETQ :RESULT (AND (NQ AS) (PARSE NIL AS)))
        (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO AS)))))
        (SETQ :RESULT (AND (NQ AS) (PARSE NIL AS)))
        (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO AS)))))
        (SETQ :RESULT (NEXTWORD? 'HOW))
        (COND (:RESULT (GO HOW)) (T (GO ADV)))
HOW
        (AND LABELTRACE (PASSING 'HOW))
        (SETQ :RESULT (AND (PARSE NIL HOW) (FQ QUEST)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO FAIL)) (T (GO FAIL)))))
        (SETQ :RESULT (AND (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C)))
        (COND (:RESULT (GO RETSM)))
        (SETQ :RESULT (AND (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C)))
        (COND (:RESULT (GO RETSM)) (T (GO FAIL)))
ADV
        (AND LABELTRACE (PASSING 'ADV))
        (SETQ :RESULT (PARSE ADV ADVADV))
        (COND (:RESULT (COND ((NULL NN) (GO POPAD)) (T (GO ADV)))))
        (SETQ :RESULT (PARSE NIL MORE))
        (COND ((NULL :RESULT) (GO ADJ)))
        (FQ COMPAR)
ADJ
        (AND LABELTRACE (PASSING 'ADJ))
        (SETQ :RESULT (COND ((CQ ADV) (PARSE ADV VBAD)) (T (PARSE ADJ))))
        (COND ((NULL :RESULT) (M ADJ) (GO FAIL)))
        (SETQ :RESULT (SETR 'HEAD H C))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (OR (CQ COMPAR) (ISQ H COMPAR)))
        (COND ((NULL :RESULT) (GO RETSM)))
        (FQ COMPAR)
        (SETQ :RESULT NN)
        (COND ((NULL :RESULT) (GO RETSM)))
THAN
        (AND LABELTRACE (PASSING 'THAN))
        (COND ((NOT NN) (GO RETSM)))
        (SETQ :RESULT (PARSE NIL THAN))
        (COND ((NULL :RESULT) (COND ((NULL NN) (M THAN) (GO FAIL)) (T (GO RETSM)))))
        (RQ THANNEED)
        (FQ THAN)
        (GO SUBJ)
AS
        (AND LABELTRACE (PASSING 'AS))
        (FQ AS)
        (RQ THANNEED)
        (SETQ :RESULT (AND (PARSE ADJ) (SETR 'HEAD H C)))
        (COND ((NULL :RESULT) (COND ((NULL NN) (GO RETSM)) (T (M ADJ) (GO FAIL)))))
        (SETQ :RESULT (PARSE NIL AS))
        (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO SUBJ)))) (T (GO RETSM)))
SUBJ
        (AND LABELTRACE (PASSING 'SUBJ))
        (SETQ :RESULT (PARSE NG SUBJ COMPAR))
        (COND ((NULL :RESULT) (M THAN) (GO FAIL)))
        (SETQ :RESULT (SETR 'OBJ1 H C))
        (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
        (SETQ :RESULT (AND (ONE-WORD-LEFT) (PARSE VB AUX)))
        (COND ((NULL :RESULT) (GO RETSM)))
        (SETQ :RESULT (CHECK-AGREEMENT H (CDR H)))
        (COND (:RESULT (GO RETSM)))
        (POP)
        (GO RETSM)
POPAD
        (AND LABELTRACE (PASSING 'POPAD))
        (POP)
        (GO ADJ)
RETSM
        (AND LABELTRACE (PASSING 'RETSM))
        (SETQ :RESULT (CQ THANNEED))
        (COND (:RESULT (M THANNEED) (GO FAIL)))
        (SETQ :RESULT (CALLSM (SMADJG-PREPG)))
        (COND (:RESULT (GO RETURN)) (T (M SMADJ) (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (OR (N RE) NB))
        (RETURN NIL)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN CONJ NIL
    (PROG (END GOODIE)
        (SETQ END CUT)
        (COND ((SETQ GOODIE (APPLY-GRAMMAR 'CONJOIN)) (RETURN (SETQ RE GOODIE))) (T (RETURN NIL)))))

(DEFUN COMMA NIL
    (COND ((SECONDWORD? '\") (FLUSHME) T) ((CONJ)) ((ISQ RE INIT) (FLUSHME) T)))

(DEFUN CONJOIN NIL
    (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT PREV)
        (SETQ NN T)
        (SETQ CUT END)
        (SETQ C (BUILDNODE (SETQ FE (REVERSE REST)) (SETQ NB (OR (NB RE) N)) N (SETQ H RE) NIL))
        (SETR 'PARENT PARENT C)
ENTERING-CONJOIN
        (AND LABELTRACE (PASSING 'ENTERING-CONJOIN))
UP
        (AND LABELTRACE (PASSING 'UP))
        (SETQ PREV (NEXTWORD))
        (FLUSHME)
        (COND ((AND (EQ PREV '\,) (OR (CDR H) (GREATERP (DIFFERENCE (LENGTH (NB H)) (LENGTH (N H))) 4)) (MEMQ (NEXTWORD) '(OR AND NOR BUT)) (F (NEXTWORD)))
            (SETQ PREV (LIST PREV (NEXTWORD)))
            (FLUSHME)))
        (AND (ATOM PREV) (MOVE-PTW N NW (EQ (WORD PTW) PREV)) (CUT PTW))
        (AND (OR (EQ PREV 'BUT) (EQ (CADR PREV) 'BUT)) (NEXTWORD? 'NOT) (OR (FLUSHME) (GO LOSE2)) (FQ NEGBUT))
        (SETQ :RESULT (COND
            ((MEMQ (CAR REST) '(ADJ NUM NOUN PREP VB ADV)) (PARSE3 (APPEND REST '(COMPONENT)) NIL))
            ((MEMQ (CAR REST) '(NG PREPG ADJG)) (AND (NOT (CQ OFOBJ)) (PARSE2 (APPEND REST '(COMPONENT)) NIL)))
            ((EQ (CAR REST) 'CLAUSE)
                ((LAMBDA (LASTSENT AUXFE)
                    (AND (PARSE2 (APPEND REST AUXFE '(COMPONENT)) NIL) (OR (NOT AUXFE) (F (CAR AUXFE))) (SETR 'TIME (GETR 'TIME H) C)))
                (COND ((ISQ H MAJOR) H) (LASTSENT))
                (MEET (FE H) '(DECLAR IMPER))))))
        (COND ((NULL :RESULT) (GO LOSE2)))
        (CUT END)
        (COND ((NOT (ATOM PREV)) (GO RETSM))
            ((EQ PREV '\,) (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP)) (T (GO LIST))))
            ((MEMQ PREV '(AND OR NOR BUT))
                (COND ((EQ BOTH (NB H)) (FQ BOTH)))
                (COND ((OR (NEXTWORD? 'BUT) (AND (NEXTWORD? PREV) (NOT (AND (EQ BOTH (NB H)) (EQ PREV 'AND)))))
                        (FQ LISTA)
                        (F PREV)
                        (GO UP))
                    (T (GO LISTA)))))
LOSE2
        (AND LABELTRACE (PASSING 'LOSE2))
        (SETQ :RESULT (CQ LISTA))
        (COND (:RESULT (GO LISTA)))
LIST
        (AND LABELTRACE (PASSING 'LIST))
        (SETQ :RESULT (AND (EQ PREV '\,) (EQUAL (LENGTH H) 2) (ISQ H NG) (NOT (OR (ISQ H PRONG) (ISQ (CDR H) PRONG))) (OR (NEXTWORD? COMMA) (NULL N))))
        (COND ((NULL :RESULT) (M CONJOIN:) (GO FAIL)))
        (FLUSHME)
        (FQ APPOSITIVE)
        (GO RETSM)
LISTA
        (AND LABELTRACE (PASSING 'LISTA))
        (F PREV)
RETSM
        (AND LABELTRACE (PASSING 'RETSM))
        (FQ COMPOUND)
        (AND (GREATERP (LENGTH H) 2) (FQ LIST))
        (COND ((OR (CQ NG) (CQ NOUN)) (COND ((CQ AND) (FQ NPL)) (T (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
            ((CQ VB) (PROG (COMMON) (SETQ COMMON (GET 'VB 'ELIM)) (MAP #'(LAMBDA (X) (SETQ COMMON (MEET COMMON (FE X)))) H)) (FESET (UNION COMMON (FE C)) C)))
        (SETQ :RESULT (CALLSM (SMCONJ)))
        (COND (:RESULT (GO RETURN)) (T (M CONJOIN:) (GO FAIL)))
FAIL
        (SETQ MES ME)
        (SETQ N (OR (N RE) NB))
        (RETURN NIL)
RETURN
        (SETQ MES ME)
        (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN BOTH FEXPR (A)
    (PROG (END)
        (SETQ END CUT)
        (RETURN (PROG (CUT NBB BOTH)
            (SETQ NBB N)
            (AND (FLUSHME)
                (MOVE-PTW N NW (EQ (WORD PTW) (CAR A)) NW)
                (CUT END)
                (SETQ BOTH PTW)
                (SETQ RE (COND ((MEMQ (CAR REST) '(PREP ADV)) (PARSE3 REST T)) ((MEMQ (CAR REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST T))))
                (LESSP (LENGTH N) (LENGTH BOTH))
                (RETURN (SETQ SPECIAL 'SKIP)))
            (SETQ RE NIL)
            (SETQ N NBB)
            (RETURN NIL)))))

(DEFUN DOUBLEQUOTER NIL (APPLY-GRAMMAR 'PARSEQUOTED))

(DEFUN CANTAKE (NUM TYPE FEATURE)
    (PROG (VBFEAT)
        (SETQ VBFEAT (FE MVB))
        (RETURN (COND
            ((MEMQ 'RSNG TYPE)
                (MEMQ (READLIST (APPEND
                        (COND ((MEMQ 'TO TYPE) '(T O)) ((MEMQ 'ING TYPE) '(I N G)) ((MEMQ 'REPORT TYPE) '(R E P)))
                        '(O B)
                        (LIST (COND ((EQ NUM 1) '\1) (T '\2)))))
                    VBFEAT))
            ((MEMQ 'COMP TYPE) (MEMQ 'INT VBFEAT))
            ((MEMQ 'NG TYPE)
                (COND ((EQUAL NUM 1)
                        (NOT (NULL (MEET '(TRANS TRANS2 TRANSL TRANSINT) VBFEAT))))
                    (T (MEMQ 'TRANS2 VBFEAT))))
            (T (MEMQ FEATURE VBFEAT))))))

(DEFUN CANPARSE (NUM TYPE FEATURE)
    (PROG (REG)
        (AND (CANTAKE NUM TYPE FEATURE)
            (OR (NULL TYPE)
                (AND (APPLY 'PARSE (APPEND TYPE
                    (COND ((MEMQ 'COMP TYPE) (SETQ REG 'COMP) NIL)
                        (T (LIST 'OBJ (SETQ REG (COND ((OR (MEMQ 'LOC TYPE) (MEMQ 'PLACE TYPE)) 'LOBJ) ((EQUAL NUM 1) 'OBJ1) (T 'OBJ2))))))))
                (SETR REG H C)))
            (OR (NULL FEATURE) (F FEATURE))
            (RETURN T))))

#_(ns shrdlu.init)

;; THIS IS A PACKAGE FOR LOADING SHRDLU'S INTO CORE FROM THE DISK FILES.
;; THE PROCEDURE IS TO FIRST LOAD A BLISP (IGNORE ALLOCATIONS, THE PROGRAMS DO THEIR OWN), AND UREAD THIS FILE.
;; EXECUTING "LOADSHRDLU" WILL GENERATE (AFTER SOME TIME) A FULLY INTERPRETED VERSION.
;; PARTIALLY COMPILED MIXES ARE AVAILLABLE, AS SEEN BELOW.
;; THE VARIABLE "VERSION-FILES" KEEPS A RUNNING TAB OF THE FILES LOADER VIA "LOADER".
;; IF ANY ERRORS OCCUR DURING READIN, THEY ARE PROTECTED BY AN "ERRSET" AND LOADING CONTINUES.
;; NOTE IF AN UNBOUND PAREN CAUSES THE FILE TO BE TERMINATED TOO SOON, YOU'LL NEVER NOTICE.

(COMMENT SYMBOL 2560) ;; THIS FILE CAN NOW BE USED AS A LISP INIT FILE VIA THE JCL LINE

(DEFUN LOADER (*!?KEY)
    (OR (ERRSET (EVAL (LIST 'UREAD *!?KEY '> 'DSK 'SHRDLU)) NIL)
        (AND (PRINT *!?KEY) (PRINC 'NOT-FOUND) (RETURN NIL)))
    (LOADX))

(DEFUN LOADX NIL
    (PROG (*!?H *!?F *!?EOF)
        (SETQ *!?EOF (GENSYM))
        (PRINT 'READING)
        (PRINC *!?KEY)
        (SETQ VERSION-FILES (CONS (STATUS UREAD) VERSION-FILES))
    LOOP ((LAMBDA (^Q) (SETQ *!?H (READ *!?EOF))) T)
        (AND (EQ *!?H *!?EOF) (RETURN T))
        (OR (ERRSET ((LAMBDA (^W ^Q) (EVAL *!?H)) T T))
            (PROG2 (PRINT 'ERROR-IN-FILE) (PRINT *!?H)))
        (GO LOOP)))

(SETQ VERSION-FILES NIL)

(DEFUN LOADSHRDLU NIL
;;  (MAPC 'LOADER '(PLNR THTRAC))
;;  (THINIT)
;;  (SETQ THINF NIL THTREE NIL THLEVEL NIL)
;;  (SETQ ERRLIST NIL) ;; REMOVES MICRO-PLANNER'S FANGS
    (MAPC 'LOADER '(SYSCOM MORPHO SHOW))
    (MAPC 'LOADER '(PROGMR GINTER GRAMAR DICTIO))
    (MAPC 'LOADER '(SMSPEC SMASS SMUTIL))
;;  (LOADER 'NEWANS)
;;  (MAPC 'LOADER '(BLOCKS DATA))
    (FASLOAD TRACE FASL COM COM)
    (LOADER 'SETUP)
    'CONSTRUCTION\ COMPLETED)

;; THIS NOW LOADS ALL OF SHRDLU EXCEPT THE BLOCKS WORLD CODE, MICROPLANNER, AND THE ANSWERING CAPABILITY.
;; THE SHIFT TO BIBOP LISP SEEMS TO HAVE CRIPPLED MICROPLANNER IN A NON-OBVIOUS WAY, AND THE ANSWERING CODE IS USELESS WITHOUT THE BLOCKS WORLD.

(LOADSHRDLU)

#_(ns shrdlu.loader)

;; THIS IS A PACKAGE FOR LOADING SHRDLU'S INTO CORE FROM THE DISK FILES.
;; THE PROCEDURE IS TO FIRST LOAD A BLISP (IGNORE ALLOCATIONS, THE
;; PROGRAMS DO THEIR OWN). AND UREAD THIS FILE. EXECUTING "LOADSHRDLU"
;; WILL GENERATE (AFTER SOME TIME) A FULLY INTERPRETED VERSION.
;; PARTIALLY COMPILED MIXES ARE AVAILLABLE, AS SEEN BELOW.
;; THE VARIABLE "VERSION-FILES" KEEPS A RUNNING TAB OF THE FILES
;; LOADER VIA "LOADER". IF ANY ERRORS OCCUR DURING READIN THEY
;; ARE PROTECTED BY AN "ERRSET" AND LOADING CONTINUES. (NOTE !! IF AN
;; UNBOUND PAREN CAUSES THE FILE TO BE TERMINATED TOO SOON, YOU'LL
;; NEVER NOTICE)

(DEFUN LOADER (*!?KEY)
    (OR (ERRSET (EVAL (LIST 'UREAD *!?KEY '> 'DSK 'SHRDLU)) NIL)
        (AND (PRINT *!?KEY) (PRINC 'NOT-FOUND) (RETURN NIL)))
    (LOADX))

(DEFUN LOADX NIL
    (PROG (*!?H *!?F *!?EOF)
        (SETQ *!?EOF (GENSYM))
        (PRINT 'READING)
        (PRINC *!?KEY)
        (SETQ VERSION-FILES (CONS (STATUS UREAD) VERSION-FILES))
    LOOP ((LAMBDA (^Q) (SETQ *!?H (READ *!?EOF))) T)
        (AND (EQ *!?H *!?EOF) (RETURN T))
        (OR (ERRSET ((LAMBDA (^W ^Q) (EVAL *!?H)) T T))
            (PROG2 (PRINT 'ERROR-IN-FILE) (PRINT *!?H)))
        (GO LOOP)))

(DEFUN FLOAD FEXPR (SPECS)
    (TERPRI)
    (PRINC (CAR SPECS))
    (PRINC '\ )
    (PRINC (CADR SPECS))
    (OR (ERRSET (APPLY 'FASLOAD SPECS))
        (ERT LOSSAGE IN LOADING - TRY AGAIN ?)))

(SETQ VERSION-FILES NIL)

(DEFUN LOADSHRDLU NIL
    (MAPC 'LOADER '(PLNR THTRAC))
    (THINIT)
    (SETQ THINF NIL THTREE NIL THLEVEL NIL)
    (SETQ ERRLIST NIL) ;; REMOVES MICRO-PLANNER'S FANGS
    (MAPC 'LOADER '(SYSCOM MORPHO SHOW))
    (MAPC 'LOADER '(PROGMR GINTER GRAMAR DICTIO))
    (MAPC 'LOADER '(SMSPEC SMASS SMUTIL))
    (LOADER 'NEWANS)
    (MAPC 'LOADER '(BLOCKS DATA))
    (FASLOAD TRACE FASL COM COM)
    (LOADER 'SETUP)
    'CONSTRUCTION\ COMPLETED)

#_(ns shrdlu.macros)

;; #############################################################
;;
;;              A PRECOMPILER FOR PROGRAMMER CODE
;;
;; #############################################################

(DEFUN LIST-NO-NILS FEXPR (ELEMENTS)
    (DO ((TAKEUP-REEL)
            (TEMP NIL (EVAL (CAR ELEMENTS)))
            (ELEMENTS ELEMENTS (CDR ELEMENTS)))
        ((NULL ELEMENTS) (REVERSE TAKEUP-REEL))
        (AND TEMP (SETQ TAKEUP-REEL (CONS TEMP TAKEUP-REEL)))))

(DEFUN TOPLEVEL-LIST FEXPR (ELEMENTS)
    ;; ACTS LIKE LIST EXCEPT THAT IF ANY ELEMEMNT EVALUATES IN TO
    ;; MORE THAN A SINGLE ELEMENT (RETURNS A LIST WHOSE CAR IS
    ;; ALSO A LIST) THEN THE ELEMENTS OF THAT ELEMENT ARE ADDED
    ;; TO THE SAME LEVEL AS THE SEPARATE ELEMENTS IN THE CALL.
    (MAPCAN #'(LAMBDA (ELEMENT)
            (SETQ ELEMENT (EVAL ELEMENT))
            (COND ((ATOM (CAR ELEMENT)) (LIST ELEMENT)) (T ELEMENT)))
        ELEMENTS))

(DEFUN GRAM-COMP (FILE)
    (PROG (^Q UNIQUE ^R ^D)
        (OR (APPLY 'UREAD FILE)
            (RETURN 'BAD-FILE-SPECS))
        (SETQ ^Q T)
        (SETQ UNIQUE (GENSYM))
        (APPLY 'UWRITE (STATUS CRUNIT))
        (DO ((R (READ UNIQUE) (READ UNIQUE)))
            ((EQ R UNIQUE))
            (COND ((MEMQ (CAR R) '(DEFUN SETQ DEFPROP)))
                (T (SETQ R (EVAL R))))
            ((LAMBDA (^R) (SPRINTER R)) T)))
    'REMEMBER\ TO\ UFILE)

(DEFUN PDEFINE FEXPR (MOBY)
    (LIST 'DEFUN (CAR MOBY) 'NIL
        (APPEND (LIST 'PROG
            (APPEND '(FE H ME NB C SM CUT NN T1 T2 T3 :RESULT) (CADR MOBY))
            '(SETQ NN T)
            '(SETQ CUT END)
            '(SETQ C (BUILDNODE (SETQ FE (REVERSE REST)) (SETQ NB (OR (NB RE) N)) N (SETQ H RE) NIL))
            '(SETR 'PARENT PARENT C))
            (APPLY ':-SPREAD (CDDR MOBY))
            (LIST 'FAIL
                '(SETQ MES ME)
                '(SETQ N (OR (N RE) NB))
                '(RETURN NIL)
                'RETURN
                '(SETQ MES ME)
                '(RETURN (REBUILD (REVERSE FE) NB N H SM C))))))

(DEFUN :-SPREAD FEXPR (LIST)
    (MAPCAN #'(LAMBDA (EXP)
        (PROG (PREDICATE T1 T2 T3)
            (COND
                ((ATOM EXP)
                    (RETURN (LIST EXP (LIST 'AND 'LABELTRACE (LIST 'PASSING (LIST 'QUOTE EXP))))))
                ((EQ (CAR EXP) 'GOCOND)
                    (RETURN (LIST (LIST 'COND (LIST 'NN (LIST 'GO (CADR EXP))) (LIST 'T (LIST 'GO (CADDR EXP)))))))
                ((EQ (CAR EXP) ':)
                    (SETQ PREDICATE (CADR EXP) T1 (CADDR EXP) T2 (CADDDR EXP))
                (AND (CDDDDR EXP) (SETQ T3 (CAR (CDDDDR EXP))))
                    (RETURN (LIST
                        (LIST 'SETQ ':RESULT PREDICATE)
                        (COND
                            ((AND T1 (NULL T2)) ;; T3 CAN BE EITHER THERE OR NOT
                                (LIST 'COND (TOPLEVEL-LIST ':RESULT
                                    (COND
                                        (T3 (LIST 'COND (TOPLEVEL-LIST (LIST 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST 'T (TAG-CHECK T1))))
                                        (T (TAG-CHECK T1))))))
                            ((AND (NULL T1) T2 (NULL T3))
                                (LIST 'COND (TOPLEVEL-LIST (LIST 'NULL ':RESULT) (TAG-CHECK T2))))
                            ((AND (NULL T1) T2 T3)
                                (LIST 'COND (LIST (LIST 'NULL ':RESULT) (LIST 'COND (TOPLEVEL-LIST (LIST 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST 'T (TAG-CHECK T2))))))
                            ((AND T1 T2 (NULL T3))
                                (LIST 'COND (TOPLEVEL-LIST ':RESULT (TAG-CHECK T1)) (TOPLEVEL-LIST 'T (TAG-CHECK T2))))
                            ((AND T1 T2 T3)
                                (LIST 'COND (LIST ':RESULT (LIST 'COND (TOPLEVEL-LIST (LIST 'NULL 'NN) (TAG-CHECK T3)) (TOPLEVEL-LIST 'T (TAG-CHECK T1))))
                                            (TOPLEVEL-LIST 'T (TAG-CHECK T2))))
                            ((AND (NULL T1) (NULL T2) T3)
                                (LIST 'COND (TOPLEVEL-LIST (LIST 'AND (LIST 'NULL 'NN) ':RESULT) (TAG-CHECK T3))))
                            ((AND (NULL T1) (NULL T2) (NULL T3))
                                (LIST 'I-AM-A-TAG))))))
                (T (RETURN (LIST EXP))))))
    LIST))

(DEFUN TAG-CHECK (TAG-EXP)
    (COND ((ATOM TAG-EXP) (LIST (LIST 'GO TAG-EXP)))
        (T (LIST (LIST 'M (CAR TAG-EXP)) (LIST 'GO 'FAIL)))))


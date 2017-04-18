(ns shrdlu.core)

(defmacro § [& _])
(defmacro ß [& _])

(defmacro def- [s i] `(def ~(vary-meta s assoc :private true) ~i))

(defmacro lambda [& _] `(fn ~@_))

(def- ascii char)
(def- terpri newline)
(def- say print)

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

(def- discourse? true)
(def- nomem? false)

(defmacro dynamic- [s] `(def ~(vary-meta s assoc :dynamic true :private true)))

(dynamic- *thtime*)
(dynamic- *thtrace*)
(dynamic- *thtree*)
(dynamic- *thxx*)
(dynamic- *thalist*)
(dynamic- *tholist*)
(dynamic- *thvalue*)
(dynamic- *global-message*)
(dynamic- *savesent*)
(dynamic- *lastsentno*)
(dynamic- *lastsent*)
(dynamic- *sentno*)
(dynamic- *level*)
(dynamic- *sent*)
(dynamic- *punct*)
(dynamic- *plan*)
(dynamic- *plan2*)

(dynamic- *me*)
(dynamic- *mes*)

(dynamic- *start*)
(dynamic- *end*)

(dynamic- BOTH)
(dynamic- *backref*)
(dynamic- *backref2*)
(dynamic- *ansname*)
(dynamic- *lastrel*)
(dynamic- WHO)
(dynamic- *pt*)
(dynamic- *ptw*)
(dynamic- H)
(dynamic- N)
(dynamic- NB)
(dynamic- *fe*)
(dynamic- SM)
(dynamic- *re*)
(dynamic- *c*)
(dynamic- CUT)

(dynamic- *savept*)

(defn- atomify [x] (cond (term? x) x (cdr x) x :else (car x)))
(defn- listify [x] (if (term? x) (list x) x))
(defn- lis2fy [x] (cond (term? x) (list (list x)) (term? (car x)) (list x) :else x))
(defn- quotify [x] (list 'quote x))

(defn- -print [x] (print \space) (print x))
(defn- print- [x] (print x) (print \space))

(def- world (atom {}))

(defn- putprop! [x y z] (swap! world assoc-in [x y] z) nil)
(defn- remprop! [x y] (swap! world update x dissoc y) nil)
(defn- getprop [x y] (get-in @world [x y]))

(declare topcenter abs atab CLEAR diff half ENDTIME EV FINDSPACE GOAL GROW LOCGREATER locg2 MEMOREND MEMORY OCCUPIER order PACKO PACKON packord size STARTHISTORY startime support tcent TFIND timechk evlis THADD THAMONG THAMONGF THAND thandf THANDT THANTE THAPPLY thapply1 THASS1 THASSERT thassertf thassertt THASVAL THBA THBIND thbi1 THBKPT THBRANCH THBRANCHUN THCOND thcondf THCONDT THCONSE THDEF THDO THDO1 THDOB THERASE therasef theraset THERASING THFAIL thfail? thfail?f thfail?t THFINALIZE THFIND thfindf THFINDT THFLUSH thgal THGO THGOAL thgoalf thgoalt THIP THMATCH2 THCHECK THUNION THMATCH1 THMATCHLIST THMESSAGE thmessagef thmessaget thmungf thmungt thnofail THNOHASH THNOT THNV THOR THOR2 thorf thort thpopt THPROG THPROGA thprogf thprogt thpure thputprop THREM1 thrembindf thrembindt THREMOVE thremprop THRESTRICT THRETURN THRPLACA THRPLACAS THURPLACA THRPLACD THRPLACDS THURPLACD THSETQ thsgal thstate THSUCCEED THTAE THTAG thtagf thtagt thtrue THTRY1 THTRY THUNDOF thundot THUNIQUE thv1 THV THVAL thvar? THVARS2 THVARSUBST THVSETQ thtrace thtrace1 thuntrace THTRACES THSEL %sent DA DISP DP FORGET ert bug global-err ERTEX COMBINATION? findb from MEET MOD PR1 PR2 PRINTC SETDIF STA UNION WALLP TAB uppercase-ify-char ETAOIN propname buildword undefined LABELTRACE passing UNLABELTRACE pev TELLCHOICE SHOWCHOICE SHOWTELL SHOWTELLCHOICE QUERY onoff REQUEST SHOWPROP treeprint charg SHOW TELL showmove DEFINE HELP spread1 PDEFINE setmvb add-f-pt remove-f-pt one-word-left CALLSM MOVE-PT MOVE-PTW apply-grammar buildnode cq CUT CUT-BACK-ONE F features FESET FLUSHME following FQ getr H ISQ isx m! mq! N NB nextword nextword? nq parent parse PARSE2 PARSE3 PARSEREL POP POPTO previous PTFIND rebuild root RQ secondword? setr SM TRNSF UPREL word UPCHECK PDEFINE INTERPRET gocheck | CONJ COMMA doublequoter CANTAKE CANPARSE !BETHERE !BEINT BOTH THANK !BLUEPRINT !BUILD !COLOR !CLEANOFF !DEFINE !DEFINITION !EQDIM !GRASP !HAVE !IN !LOC !LOC2 !NAME !NOTICE !ON !PROPDEFINE !ROLE !STACKUP smtime SMNEWNOUN SMNEWPROPN SMCONJ SMCONJ2 SMVG SMPRON SMVAUX smplace smtoadj SMPROP SMADJ SMADJG-PREPG SMIT SMIT2 SMNGOF SMNG1 SMNG2 SMNG3 SMONE SMONE2 SMONE3 SMPOSS SMPOSS2 SMRELATE SMCL1 SMCL2 SMCL-MODIFIERS SMBIND smbinder istense imperf? BUILD newcopy RELATION DOBACKREF evalcheck ITERATE ITERATEX MAPBLAND mapc2 MUMBLE OBJECT valueput plnr-junkify plnr-junkify2 PLNR-THCONSIFY PLNR-FINDIFY plnr-findspec PLNR-GOALIFY plnr-mung PLNR-NOTIFY PLNR-NEWBODY PLNR-PROGIFY PLNR-NUMREL PLNR-NUMSUB PLNR-RECOMMENDIFY PLNR-REMTIME plnr-var COMPARE-BUILD findmeasure MEASURE PLNR-DESCRIBE RELFIND ORDMAKE COMPARE-PROC EXPAND ERQSET SETQQCHECK THVAL2 WHO CHECK CHECKAMARKER FINDEVENTS checkrel action? ambiguities? and? ansrss? determiner? end? markers? modifiers? negative? num? or? oss? parsenode? plausibility? plnrcode? qtype? quantifier? refer? rel? relations? relmarkers? rss? rssvar? start? systems? tense? tss? variable? SMSET ANSWER AMBPUT ANSBUILD ANSCOMMAND ANSDECLARE ANSELIMINATE PARSE-ASSOC ANSGEN ANSNAME ANSNOREL ANSORDER ANSQUEST ANSREL ANSTHM ANSTHMADD ANSTHMELEMENT ansunique CUTOFF DESCRIBEVENT disput ELIZA enough-better FINDMOTHER headpart LISTNAMES pron-prt NAMEACTION namelist namelist-evaled namenum ansay NAMEOBJ namesize namesugar notell ONECHECK ORDNAME PLNR-ANDORIFY PREPPUT PLURALIZE PLURALMAKE THVAL-MULT toplevel findreduce FINDCHOOSE MUNG NAMEVENT prtput vbfix vbfix2 CLAUSE NG VG PREPG ADJG CONJOIN SHRDLU)

#_(ns shrdlu.plnr)

;; DO NOT GRIND THIS FILE WITH THE STANDARD GRIND

(defmacro $? [x] (list 'THV x))
(defmacro $E [x] (list 'THEV x))
(defmacro $_ [x] (list 'THNV x))
(defmacro $T [] '(THTBF thtrue))
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

(§ defn- THADD [THTT thpl]
    ;; THADD ADDS THEOREMS OR ASSERTION TO THE INPUT
    ;; THTT - NAME OF THM OR ACTUAL ASSERTION
    ;; THPL - PROPERTY LIST TO BE PLACED ON ASSERTION DATABASE INPUTS
    ;; RETURNS NIL IF ALREADY THERE ELSE RETURNS THTT
    (let [THNF nil THWH nil THCK nil THLAS nil THTTL nil THT1 nil THFST nil THFSTP nil THFOO nil]
        (SETQ THCK
            ;; IF THTT IS ATOMIC, WE ARE ASSERTING A THEOREM
            (COND ((term? THTT)
                    ;; IF NO THEOREM PROPERTY, THE GUY MADE A MISTAKE
                    (or (SETQ THT1 (getprop THTT 'THEOREM))
                        (do (terpri) (pr THTT) (ert "CAN'T THASSERT, NO THEOREM - THADD")))
                    ;; THWH NOW SET TO KIND OF THEOREM, LIKE THERASING
                    (SETQ THWH (car THT1))
                    ;; MAKE AN EXTRA POINTER TO THTT
                    (SETQ THTTL THTT)
                    ;; IF WE HAVE A PL FOR OUR THEOREM, IT GOES ON THE ATOM WHICH IS THE NAME OF THE THEOREM
                    (and thpl (do
                    ;; GO THROUGH ITEMS ON PL ONE BY ONE
                    =>  (thputprop THTT (car thpl) (cadr thpl))
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
                ;; BEING IN DATABASE, BUT NOW USE VARIABLES FOR EQ CHECK
                (SETQ THFSTP true)
                (GO THP1))
            ((nil? (SETQ THT1 (THIP (car THCK)))) (RETURN nil))
            ;; THIP IS THE WORKHORSE FOR THADD IF IT RETURNS NIL.
            ;; IT MEANS THE ASSERTEE IS ALREADY IN, SO FAIL.
            ((= THT1 'THOK))
            ;; THOK WHICH IS RETURN BY THIP SAYS THAT THE ASSERTEE IS NOT IN ALREADY.
            ;; OTHERWISE WE GO AROUND AGAIN, STILL LOOKING FOR A NON VARIABLE ITEM TO DO THE EQ CHECK.
            ((SETQ THFOO (concat THFOO (list (COND ((= THT1 'THVRB) (car THCK))))))
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
    (set! *thxx* (thgal (if (= (caar tha) 'THEV) (THVAL (cadar tha) *thalist*) (car tha)) *thalist*))
    (COND
        ((= (cadr *thxx*) 'THUNASSIGNED)
            (THPUSH *thtree* (list 'THAMONG *thxx* (THVAL (cadr tha) *thalist*)))
            nil)
        (:else (memq (cadr *thxx*) (THVAL (cadr tha) *thalist*)))))       ;; IF ($? X) ASSIGNED, THAMONG REDUCES TO A MEMBERSHIP TEST

(§ defn- THAMONGF []                                                 ;; (CAR THTREE) = (THAMONG OLDBINDINGCELL (NEW VALUES))
    (COND (THMESSAGE (thpopt) nil)
        ((caddar *thtree*)                                            ;; LIST OF NEW VALUES NON NIL
            (RPLACA (cdadar *thtree*) (caaddr (car *thtree*)))          ;; REPLACE OLD VALUE WITH NEW VALUE
            (RPLACA (cddar *thtree*) (cdaddr (car *thtree*)))           ;; POP NEW VALUES
            (SETQ THBRANCH *thtree*)                                  ;; STORE AWAY TREE FOR POSSIBLE BACKTRACKING
            (SETQ THABRANCH *thalist*)                                ;; STORE AWAY THALIST FOR POSSIBLE BACKTRACKING
            (thpopt)                                                ;; POP TREE
            true)                                                      ;; SUCCEED
        (:else (RPLACA (cdadar *thtree*) 'THUNASSIGNED)                   ;; NO NEW VALUES LEFT. RETURN X TO THUNASSIGNED,
            (thpopt)                                                ;; POP TREE AND CONTINUE FAILING.
            nil)))

(§ defq- THAND [a]
    (or (not a)
        (do (THPUSH *thtree* (list 'THAND a nil)) (SETQ THEXP (car a)))))

(defn- thandf [] (THBRANCHUN) nil)

(§ defn- THANDT []
    (COND ((cdadar *thtree*)
            (THBRANCH)
            (SETQ THEXP (cadr (cadar *thtree*)))
            (RPLACA (cdar *thtree*) (cdadar *thtree*)))
        ((thpopt)))
    *thvalue*)

(§ defq- THANTE [thx]
    ;; DEFINES AND OPTIONALLY ASSERTS ANTECEDENT THEOREMS
    (THDEF 'THANTE thx))

(§ defq- THAPPLY [a]
    ;; THAPPLY1 DOES THE REAL WORK, ALL WE DO IS GET THE THEOREM OFF THE PROPERTY LIST
    (thapply1 (car a) (getprop (car a) 'THEOREM) (cadr a)))

(defn- thapply1 [thm thb dat]
    ;; MAKE SURE THE THEOREM PATTERN MATCHES THE GOAL
    (if (and (THBIND (cadr thb)) (THMATCH1 dat (caddr thb)))
        (do (when *thtrace* (THTRACES 'THEOREM thm))
            ;; AS FAR AS THTREE GOES, ALL THEOREMS LOOK LIKE THPROG, AND
            ;; WHEN YOU COME DOWN TO IT, THEY ALL ACT LIKE THPROGS.
            (THPUSH *thtree* (list 'THPROG (cddr thb) nil (cddr thb)))
            ;; CALL THE MAIN THPROG WORKHORSE.
            (THPROGA)
            true)
        ;; IF THE THEOREM PATTERN DIDN'T MATCH, START FAILING.
        (do (set! *thalist* *tholist*) (thpopt) nil)))

(§ defn- THASS1 [tha p]
    (let [THX nil THY1 nil THY nil TYPE nil PSEUDO nil]
        ;; IF YOU SEE "THPSEUDO", SET FLAG "PSEUDO" TO T.
        (and (cdr tha) (= (caadr tha) 'THPSEUDO)
            (SETQ PSEUDO true))
        ;; IF (CAR THA) IS AN ATOM, WE ARE ASSERTING (ERASING) A THEOREM.
        (when-not (or (term? (SETQ THX (car tha)))
                ;; THVARSUBST SUBSTITUTES THE ASSIGNMENTS FOR ALL ASSIGNED VARIABLES.
                ;; THPURE CHECKS THAT ALL VARIABLES ARE ASSIGNED.
                (thpure (SETQ THX (THVARSUBST THX nil)))
                ;; IF WE ARE NOT REALLY ASSERTING, THE VARIABLES DO NOT ALL HAVE TO BE ASSIGNED.
                PSEUDO)
            (terpri) (pr THX) (ert "IMPURE ASSERTION OR ERASURE - THASS1"))
        (and *thtrace* (not PSEUDO)
            (THTRACES (if p 'THASSERT 'THERASE) THX))
        (SETQ tha (if PSEUDO (cddr tha) (cdr tha)))
        ;; THX IS NOW WHAT WE ARE ASSERTING, AND THA IS THE RECOMMENDATION LIST.
        ;; WE ARE NOW GOING TO PHYSICALLY ADD OR REMOVE ITEM.
        (SETQ THX
            ;; IF THPSEUDO, DON'T ALTER THE DATABASE.
            (COND (PSEUDO (list THX))
                ;; IF P IS "T", WE ARE ASSERTING, SO USE THADD.
                (p (SETQ THY
                        ;; THPROP SAYS "MY" CADR IS TO BE EVALED TO GET THE PROPERTY LIST,
                        (when (and tha (= (caar tha) 'THPROP))
                            (PROG1 (eval (cadar tha))
                                ;; AND REMOVE THPROP FROM THE RECOMENDATION LIST.
                                (SETQ tha (cdr tha)))))
                    ;; THADD TAKES TWO ARGS: THE FIRST IS ITEM TO BE ADDED,
                    ;; THE SECOND IS THE PROPERTY LIST FOR THE ITEM.
                    (THADD THX THY))
                ;; OTHERWISE WE ARE ERASING, SO USE THREMOVE.
                (:else (THREMOVE THX))))
        ;; THE LAST ITEM WILL BE NIL ONLY IF THADD OR THREMOVE FAILED.
        ;; THAT IS, IF THE ITEM TO BE ADDED WAS ALREADY THERE, OR THE ONE TO BE REMOVED, WASN'T.
        (or THX
            (RETURN nil))
        ;; TYPE IS THE KIND OF THEOREM WE WILL BE LOOKING FOR.
        (SETQ TYPE (if p 'THANTE 'THERASING))
        ;; IF WE ACTUALLY MUNGED THE DATABASE, PUT THE FACT IN THTREE.
        (or PSEUDO
            (THPUSH *thtree* (list (COND (p 'THASSERT) ('THERASE)) THX THY)))
        ;; MAPCAN IS A MAC-LISP FUNCTION, LIKE MAPCAR BUT USES NCONC.
        ;; THTAE LOOKS AT THE RECOMENDATION LIST AND PRODUCES A LIST OF INSTRUCTIONS ABOUT WHAT THEOREMS TO TRY.
        (SETQ THY (doall (map #'THTAE tha)))
        ;; THEXP IS A HACK TELLING THVAL TO THVAL THIS ITEM BEFORE IT GOES ON TO THE NEXT LINE OF PLANNER CODE.
        ;; THEXP IS NOW (THDO <APPROPRIATE ANTECEDENT OR ERASING THEOREMS>).
        (COND (THY (SETQ THEXP (cons 'THDO THY))))
        THX))

(§ defq- THASSERT [tha]
    ;; THASS1 IS USED FOR BOTH ASSERTING AND ERASING, THE "T" AS SECOND ARG TELLS IT THAT WE ARE ASSERTING.
    (THASS1 tha true))

(defn- thassertf []
    (THREMOVE (if (term? (cadar *thtree*)) (cadar *thtree*) (caadar *thtree*)))
    (thpopt)
    nil)

(defn- thassertt [] (let [_ (cadar *thtree*)] (thpopt) _))

(§ defq- THASVAL [a]
    (let [x (thgal (car a) *thalist*)] (and x (not (= (cadr x) 'THUNASSIGNED)))))

(§ defn- THBA [th1 th2]
    ;; JUST LIKE ASSQ IN LISP, ONLY RETURN WITH THE POINTER 1 ELEMENT PRIOR TO THE ONE ASKED FOR.
    ;; USED ONLY BY THAD AND THREMOVE.
    (let [thp th2]
    =>  (and (= (if THPC (cadr thp) (caadr thp)) th1)
            (RETURN thp))
        (SETQ thp (cdr thp))
        (or (cdr thp)
            (RETURN nil))
        (GO =>)))

(§ defn- THBIND [a]
    ;; WHEN WE ENTER A NEW THEOREM OR THPROG, WE MUST BIND THE NEW VARIABLES.  A IS THE VARIABLE LIST
    (set! *tholist* *thalist*) ;; THOLIST IS THE OLD THALIST
    ;; IF A IS NIL THERE IS NOTHING TO DO
    (or (nil? a)
        (do
        ;; WHEN A IS NIL, WE ARE DONE AND JUST PUT A MARKER ON THTREE WITH A POINTER TO THE OLD THALIST,
        ;; SO IT CAN BE RESTORED.
        =>  (COND ((nil? a)
                (THPUSH *thtree* (list 'THREMBIND *tholist*))
                (RETURN true)))
            ;; OTHERWISE ADD TO THE ALIST THE NEW BINDING CELL
            (THPUSH *thalist*
                ;; THE FIRST ELEMENT IS THE NAME OF THE VARIABLE IF THE ENTRY IS AN ATOM,
                ;; THEN WE ARE JUST GIVEN THE VARIABLE AND ITS INITIAL ASSIGNMENT IS "THUNASSIGNED",
                ;; I.E. NO INITIAL ASSIGNMENT.
                (COND ((term? (car a)) (list (car a) 'THUNASSIGNED))
                    ;; OTHERWISE OUR ENTRY IS A LIST
                    ;; IF THE FIRST ELEMENT OF THE LIST IS ($R) OR THRESTRICT
                    ;; WE ADD THE RESTRICTION TO THE BINDING CELL
                    ;; THE CDDR OF THE CELL GIVES THE RESTRICTION LIST
                    ((= (caar a) 'THRESTRICT) (concat (thbi1 (cadar a)) (cddar a)))
                    ;; OTHERWISE WE ARE GIVEN BOTH THE VARIABLE AND ITS
                    ;; INITIAL ASSIGNMENT, SO MAKE THE SECOND ELEMENT OF THE
                    ;; BINDING CELL A POINTER TO THE INITIAL ASSIGNMENT
                    (:else (list (caar a) (eval (cadar a))))))
            (SETQ a (cdr a))
            ;; REPEAT FOR THE NEXT VARIABLE IN THE LIST
            (GO =>))))

(defn- thbi1 [x]
    (if (term? x) (list x 'THUNASSIGNED) (list (car x) (eval (cadr x)))))

(§ defq- THBKPT [a]
    (or (and *thtrace* (THTRACES 'THBKPT a)) *thvalue*))

(§ defn- THBRANCH []
    ;; THBRANCH IS CALLED BY THPROGT AND WE ARE SUCCEEDING BACKWARDS.
    ;; CAR THTREE IS THE THPROG MARKING.
    (COND ;; THERE ARE NO MORE EXPRESSIONS TO EXECUTE IN THE THPROG.
        ((not (cdadar *thtree*)))
        ((= THBRANCH *thtree*) (SETQ THBRANCH nil))
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
        ((RPLACA (cddar *thtree*) (cons (list THBRANCH THABRANCH (cadar *thtree*)) (caddar *thtree*)))
            ;; WE NOW SETQ THBRANCH TO NIL.  IF THE NEXT LINE ALSO SUCCEEDS,
            ;; THVAL WILL LOOK FOR A NIL THBRRANCH TO INDICATE THAT IT SHOULD
            ;; SETQ IT AGAIN TO THE POINT OF SUCCESS
            (SETQ THBRANCH nil))))

(§ defn- THBRANCHUN []
    ;; WE ARE NOW FAILING.  THBRANCHUN IS CALLED BY THPROGF.
    (let [x (caddar *thtree*)]
        (if x ;; IF THE SECOND ARG TO THE PROG MARK IS NON-NIL, IT MEANS THAT THERE ARE PREVIOUS LINES IN THE THPROG TO FAIL BACK TO
            (do ;; A COMPAIRISON OF THIS WITH WHAT HAPPEND IN THBRANCK WILL REVEAL THAT
                ;; ALL WE ARE DOING HERE IS RESTORING THE PROG MARK TO IS STATE BEFORE THE LAST SUCCESS.
                (RPLACA (cdar *thtree*) (caddar x))
                (RPLACA (cddar *thtree*) (cdr x))
                ;; RESET THALIST AND THTREE
                (set! *thalist* (cadar x))
                (set! *thtree* (caar x))
                true)
            ;; THERE AREN'T ANY MORE THINGS IN THE THPROG TO TRY,
            ;; SO JUST RETURN NIL.
            (do (thpopt) nil))))

(§ defq- THCOND [tha]
    (THPUSH *thtree* (list 'THCOND tha nil))
    (SETQ THEXP (caar tha)))

(defn- thcondf [] (THOR2 nil))

(§ defn- THCONDT []
    (RPLACA (car *thtree*) 'THAND)
    (RPLACA (cdar *thtree*) (caadar *thtree*))
    *thvalue*)

;; THCONSE DEFINES AND OPTIONALLY ASSERTS CONSEQUENT THEOREMS

(§ defq- THCONSE [thx] (THDEF 'THCONSE thx))

;; THDEF DEFINES AND OPTIONALLY ASSERTS THEOREMS

(§ defn- THDEF [thmtype thx]
    (let [THNOASSERT? nil thmname nil thmbody nil]
        (COND ((not (term? (car thx)))
                (SETQ thmbody thx)
                (COND
                    ((= thmtype 'THCONSE) (SETQ thmname (gensym 'TC-G)))
                    ((= thmtype 'THANTE) (SETQ thmname (gensym 'TA-G)))
                    ((= thmtype 'THERASING) (SETQ thmname (gensym 'TE-G)))))
            ((SETQ thmname (car thx)) (SETQ thmbody (cdr thx))))    ;; THNOASSERT FEATURE
        (COND ((= (car thmbody) 'THNOASSERT)
            (SETQ THNOASSERT? true)
            (SETQ thmbody (cdr thmbody))))
        (thputprop thmname 'THEOREM (cons thmtype thmbody))
        (terpri)
        (pr thmname)
        (cond
            THNOASSERT? (-print "DEFINED, BUT NOT ASSERTED")
            (THASS1 (list thmname) true) (-print "DEFINED AND ASSERTED")
            :else (-print "REDEFINED"))
        true))

(§ defq- THDO [a]
    (or (not a)
        (do (THPUSH *thtree* (list 'THDO a nil nil)) (SETQ THEXP (car a)))))

(§ defn- THDO1 []
    (RPLACA (cdar *thtree*) (cdadar *thtree*))
    (SETQ THEXP (caadar *thtree*))
    (COND (THBRANCH
        (RPLACA (cddar *thtree*) (cons THBRANCH (caddar *thtree*)))
        (SETQ THBRANCH nil)
        (RPLACA (cdddar *thtree*) (cons THABRANCH (car (cdddar *thtree*)))))))

(§ defn- THDOB []
    (COND ((or THMESSAGE (nil? (cdadar *thtree*)))
            (RPLACA (car *thtree*) 'THUNDO)
            true)
        ((THDO1))))

(§ defq- THERASE [tha] (THASS1 tha nil))

(defn- therasef []
    (let [x (cadar *thtree*)]
        (THADD (if (term? x) x (caadar *thtree*)) (if (term? x) nil (cdadar *thtree*)))
        (thpopt)
        nil))

(defn- theraset [] (let [_ (cadar *thtree*)] (thpopt) _))

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
            (SETQ THTREE1 *thtree*)
        LP1 (COND ((nil? THTREE1)
                    (terpri)
                    (pr tha)
                    (SETQ tha (ert "NOT FOUND - THFAIL"))
                    (COND ((term? tha) (RETURN tha))
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

(defn- thfail? [prd act]
    (THPUSH *thtree* (list 'thfail? prd act))
    *thvalue*)

(defn- thfail?f []
    (if (eval (cadar *thtree*))
            (eval (let [_ (SETQ THMESSAGE nil) _ (caddar *thtree*)] (thpopt) _))
        (do (thpopt) nil)))

(defn- thfail?t [] (thpopt) *thvalue*)

(§ defq- THFINALIZE [tha]
    (let [THTREE1 nil THT nil THX nil]
        (COND ((nil? tha) (SETQ tha (ert "BAD CALL - THFINALIZE"))))
        (COND ((term? tha) (RETURN tha))
            ((= (car tha) 'THTAG) (SETQ THT (cadr tha)))
            ((= (car tha) 'THEOREM) (SETQ tha (list 'THPROG))))
        (set! *thtree* (SETQ THTREE1 (cons nil *thtree*)))
    PLUP (SETQ THX (cadr THTREE1))
        (COND ((nil? (cdr THTREE1)) (terpri) (pr tha) (ert "OVERPOP - THFINALIZE"))
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
    DONE (set! *thtree* (cdr *thtree*))
        true))

(§ defq- THFIND [tha]
    (THBIND (caddr tha))
    (THPUSH *thtree*
        (list 'THFIND
            (COND ((= (car tha) 'ALL) '(1 nil nil))               ;; STANDARD ALL
                ((number? (car tha))
                    (list (car tha) (car tha) true))                       ;; SINGLE NUMBER
                ((number? (caar tha)) (car tha))                    ;; WINOGRAD CROCK FORMAT
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
    (THPUSH *thtree* (list 'THPROG (cddr tha) nil (cddr tha)))
    (THPROGA))

(defn- thfindf []
    (SETQ THBRANCH nil)
    (if (or THMESSAGE (< (caadr (set! *thxx* (cdar *thtree*))) (caar *thxx*)))
        (do (thpopt) nil)
        (do (thpopt) (cdadr *thxx*))))

(§ defn- THFINDT []
    (let [THCDAR (cdar *thtree*) THX nil THY nil THZ (caddr THCDAR)]
        (and (memq (SETQ THX (THVARSUBST THZ nil)) (cdadr THCDAR))
            (GO =>))
        (RPLACD (cadr THCDAR) (cons THX (cdadr THCDAR)))
        (and (= (SETQ THY (inc (caadr THCDAR))) (cadar THCDAR))
            (RETURN (PROG2 (SETQ THBRANCH nil) (and (caddar THCDAR) (cdadr THCDAR)) (thpopt))))
        (RPLACA (cadr THCDAR) THY)
    =>  (set! *thtree* THBRANCH)
        (set! *thalist* THABRANCH)
        (SETQ THBRANCH nil)
        nil))

(§ defq- THFLUSH [a]
    ;; (THFLUSH) FLUSHES ALL ASSERTIONS AND THEOREMS
    ;; INPUT = SEQUENCE OF INDICATORS DEFAULT =
    ;; EFFECT = FLUSHES THE PROPERTIES OF THESE
    ;; (THASSERTION THCONSE THANTE THERASING)
    ;; INDICATORS FROM ALL ATOMS
    (dorun (map #'(lambda [b]
        (dorun (map #'(lambda [c]
            (dorun (map #'(lambda [d]
                (remprop! d b))
            c)))
        (MAKOBLIST nil))))
    (or a '(THASSERTION THCONSE THANTE THERASING)))))

(defn- thgal [x y]
    ;; (THGAL ($? X) THALIST) RETURNS THE BINDING CELL (X -) OF X ON THALIST
    (set! *thxx* x)
    (sassq (cadr x) y #(do (terpri) (pr *thxx*) (ert "THUNBOUND - THGAL"))))

(§ defq- THGO [a]
    (APPLY #'THSUCCEED (cons 'THTAG a)))

(§ defq- THGOAL [tha]
    ;; THA = (PATTERN RECOMMENDATION)
    ;; PATTERN IS EITHER EXPLICIT, THE VALUE OF A PLANNER VARIABLE OR THVAL OF $E...
    ;; THA2 = INSTANTIATED PATTERN
    ;; THA1 = RECOMMENDATIONS
    (let [THY nil THY1 nil THZ nil THZ1 nil THA1 nil THA2 nil]
        (SETQ THA2 (THVARSUBST (car tha) true))
        (SETQ THA1 (cdr tha))
        (COND ((or (nil? THA1)                                      ;; SHOULD DATABASE BE SEARCHED?  TRIED IF NO RECS
                (and (not (and (= (caar THA1) 'THANUM)
                        (SETQ THA1 (cons (list 'THNUM (cadar THA1)) (cons (list 'THDBF 'thtrue) (cdr THA1))))))
                    (not (and (= (caar THA1) 'THNODB)              ;; TRIED IF REC NOT THNODB OR (THDBF PRED)
                        (do (SETQ THA1 (cdr THA1)) true)))
                    (not (= (caar THA1) 'THDBF))))
            (SETQ THA1 (cons (list 'THDBF 'thtrue) THA1))))
        (SETQ THA1 (doall (map #'THTRY THA1)))                      ;; THMS AND ASSERTIONS SATISFYING RECS APPENDED TO RECS
        (and *thtrace* (THTRACES 'THGOAL THA2))
        (COND ((nil? THA1) (RETURN nil)))
        (THPUSH *thtree* (list 'THGOAL THA2 THA1))                    ;; (THGOAL PATTERN MATCHES)
        (RPLACD (cddar *thtree*) 262143)
        nil))                                                       ;; FAILS TO THGOALF

(defn- thgoalf []
    ;; BASICALLY ALL IT DOES IS TO SEND OFF TO THTRY1 TO TRY ANOTHER POSSIBILITY.
    ;; IF THTRY1 RETURNS NIL, IT MEANS THAT IT COULDN'T FIND ANOTHER POSSIBILITY
    ;; AND WE SHOULD TELL THVAL THAT WE HAVE FAILED.
    ;; ALL THPOPT DOES IS TO LOB THE THGOAL ENTRY OFF THTREE.
    (if THMESSAGE (do (thpopt) nil) (or (THTRY1) (do (thpopt) nil))))

(defn- thgoalt []
    (let [_ (if (= *thvalue* 'THNOVAL) (THVARSUBST (cadar *thtree*) nil) *thvalue*)] (thpopt) _))

(§ defn- THIP [thi]
    ;; THI IS AN ITEM FROM THE ASSERTION OR PATTERN OF THE THEOREM BEING ENTERED
    (let [THT1 nil THT3 nil THSV nil THT2 nil THI1 nil]
        (SETQ THNF (inc THNF))
        ;; THNF IS A FREE VARIABLE FROM THADD (WHO CALLS THIS BUGER)
        ;; IT SAYS WE ARE LOOKING AT THE N'TH PLACE IN THE PATTERN
        (COND ((and (term? thi) (not (= thi '?)) (not (number? thi)))
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
        (COND ((not (SETQ THT1 (getprop THI1 THWH)))
                ;; THWH IS THE NAME OF THE PROPERTY TO LOOK UNDER ON THE ATOM
                ;; IF THIS PROPERTY IS NOT THERE THEN WE MUST PUT IT THERE
                ;; IN PARTICULAR, NO PROPERTY MEANS THAT THE
                ;; ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
                (putprop! THI1 THWH (list nil (list THNF (list THLAS 1 THTTL)))))
            ((= THT1 'THNOHASH) (RETURN 'THBQF))
            ;; IF THE PROPERTY IS "THNOHASH" IT MEANS THAT WE
            ;; SHOULD NOT BOTHER TO INDEX UNDER THIS ATOM, SO
            ;; JUST RETURN TO THADD
            ((not (SETQ THT2 (assq THNF (cdr THT1))))
                ;; LOOK ON THE PROPERTY LIST ENTRY TO SEE
                ;; IF THERE IS A SUB-ENTRY FOR PATTERNS WITH THIS ATOM IN THE THNF'TH POSITION.
                ;; IF NOT, HACK THE ENTRY SO THERE IS.
                ;; AGAIN THIS IMPLIES THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE.
                (concat THT1 (list (list THNF (list THLAS 1 THTTL)))))
            ((not (SETQ THT3 (assq THLAS (cdr THT2))))
                ;; NOW LOOK WITHIN THE SUB-ENTRY FOR A SUB-SUB-ENTRY.
                ;; I.E. THOSE PATTERNS WHICH ARE ALSO OF THE CORRECT TOTAL LENGTH
                ;; THLAS IS A VARIABLE FROM THADD WHICH GIVES THE LENGTH OF THE ASSERTEE
                ;; AGAIN, IF NOT THERE, HACK IT IN
                (concat THT2 (list (list THLAS 1 THTTL))))
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
            (RPLACD (cdr THT3) (concat (list THTTL) THSV))))
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
        (SETQ thx (THVAL (cadr thx) *tholist*)))
    (and (= (car thy) 'THEV)
        (SETQ thy (THVAL (cadr thy) *thalist*)))
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
                    (or (= (car thx) 'THNV) (and (= (car thx) 'THV) (= (cadr XPAIR) 'THUNASSIGNED)))
                    ;; THCHECK MAKES SURE THE RESTRICTIONS (IF ANY) ON THX ARE COMPATIBLE WITH THY
                    (THCHECK (cddr XPAIR) (if YPAIR (cadr YPAIR) thy)))
                ;; FURTHERMORE, THY IS ALSO A VARIABLE
                ;; THIS MEANS WE MUST DO THE MYSTERIOUS VARIABLE LINKING
                (COND (YPAIR
                        (THRPLACAS (cdr XPAIR) (cadr YPAIR))
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
                    (THCHECK (cddr YPAIR) (if XPAIR (cadr XPAIR) thx)))
                ;; IF THX IS A VARIABLE, LINK.  OTHERWISE JUST ASSIGN THY TO THX.
                (THRPLACAS (cdr YPAIR) (if XPAIR (cadr XPAIR) thx)))
            ;; THX IS AN ASSIGED VARIABLE, SO JUST MAKE
            ;; SURE ITS ASSIGNEMENT IS EQUAL TO THY
            ((and XPAIR (= (cadr XPAIR) (if YPAIR (cadr YPAIR) thy))))
            ;; THX IS A CONSTANT, THY IS A VARIABLE, AND THEY ARE EQUAL
            ((and YPAIR (= (cadr YPAIR) thx)))
            ;; LOOSE, SO RETURN WITH AN ERROR
            (:else (ERR nil))))

            ;; THE FOLLOWING TWO CONDS BIND XPAIR AND YPAIR RESPECTIVELY
            (COND
                ;; IF THX IS A NORMAL VARIALBE, IN PARTICULAR
                ;; WE ARE NOT INTRODUCING NEW RESTRICTIONS AT THIS TIME,
                ;; THEN X PAIR IS JUST THE BINDING LIST
                ((thvar? thx) (thgal thx *tholist*))
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
                            (thgal (cadr thx) *tholist*))))))
            ;; NOTE THAT IF THX IS NOT A VARIABLE THEN XPAIR IS ()
            ;; WE DO THE EXACT SAME THING FOR THY AS WE JUST DID FOR THX
            (COND ((thvar? thy) (thgal thy *thalist*))
                ((= (car thy) 'THRESTRICT)
                    (COND ((= (cadr thy) '?)
                        (PROG1 (cons '? (cons 'THUNASSIGNED (concat (cddr thy) nil)))
                            (SETQ thy '(THNV ?))))
                        (:else ((lambda [U]
                                (THRPLACDS (cdr U) (THUNION (cddr U) (cddr thy)))
                                (SETQ thy (cadr thy))
                                U)
                            (thgal (cadr thy) *thalist*))))))))
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
            (or (memq THX l2) (SETQ l2 (cons THX l2))))
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
        (COND ((and (= (count (COND ((= (car thx) 'THEV) (SETQ thx (THVAL (cadr thx) *tholist*))) (thx))) (count thy))
                ;; IF THE MATCH FAILS, THMATCH2 EXITS WITH AN ERR
                ;; WILL BE "TRUE" PROVIDED THE MATCH WORKED
                (ERRSET (dorun (map #'THMATCH2 thx thy))))
            ;; SO RECORD THE ASSIGNMENTS ON THTREE
            (and THML (THPUSH *thtree* (list 'THMUNG THML)))
            (RETURN true))
        ;; IF THE MATCH FAILED, WE MAY STILL HAVE SOME ASSIGNEMENTS ALREADY MADE.
        ;; THESE MUST IMMEDIATELY BE UNDONE.  EVLIS JUST EVALS EVERYTHING ON THML,
        ;; WHICH IS A LIST OF EXPRESSIONS, WHICH WHEN EVALED, UNASSIGN THE VARIABLES.
        (:else (evlis THML) (RETURN nil)))))

(§ defn- THMATCHLIST [thtb thwh]
    ;; THTB IS A PATTERN WHICH EVENTUALLY IS TO BE MATCHED.
    ;; THWH SAYS IF IT IS AN ASSERTION, CONSEQUENT THEOREM, ETC.
    ;; THMATCHLIST GOES THROUGH THE DATABASE, LOOKING ON ALL THE BUCKETS OF THE ATOMS IN THE PATTERN.
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
    THP3 (COND ((or (nil? (term? THB2)) (number? THB2) (= THB2 '?))
                (GO THP1))
            ;; IF THE ITEM DOES NOT HAVE THE PROPERTY ON ITS PROPERTY LIST,
            ;; THEN IT OBVIOUSLY DOSEN'T HAVE ANY BUCKET AT ALL.
            ;; SO THA1, WHICH RECORDS THE NUMBER IN THE BUCKET IS SET TO 0
            ((not (SETQ THA1 (getprop THB2 thwh)))
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
            ((not (SETQ THA2 (getprop 'THVRB thwh))) (SETQ THA2 '(0 0)))
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
            ((zero? THRN) (RETURN nil))
            ;; IF THE NEW BUCKET IS SMALLER, IT BECOMES THE SMALLEST SO FAR.
            ((> THL THRN) (SETQ THL1 THA1) (SETQ THL THRN)))
        ;; GO BACK FOR ANOTHER PASS.
        (GO THP1)))

(§ defq- THMESSAGE [tha]
    (THPUSH *thtree* (cons 'THMESSAGE tha))
    *thvalue*)

(defn- thmessagef []
    (let [x (car *thtree*)]
        (thpopt)
        (if (and (THBIND (cadr x)) (THMATCH1 (caddr x) THMESSAGE))
            (do (THPUSH *thtree* (list 'THPROG (cddr x) nil (cddr x)))
                (SETQ THMESSAGE nil)
                (THPROGA))
            (do (set! *thalist* *tholist*) nil))))

(defn- thmessaget [] (thpopt) *thvalue*)

(defn- thmungf [] (evlis (cadar *thtree*)) (thpopt) nil)

(defn- thmungt [] (thpopt) *thvalue*)

(defn- thnofail [thx]
    (putprop! 'THPROG 'THFAIL (if thx 'thprogt 'thprogf)))

(§ defq- THNOHASH [tha]
    (dorun (map #'(lambda [x] (putprop! (car tha) x 'THNOHASH))
        (or (cdr tha) '(THASSERTION THCONSE THANTE THERASING)))))

(§ defq- THNOT [tha]
    (SETQ THEXP (list 'THCOND (list (car tha) '(THFAIL THAND)) '((THSUCCEED)))))

(§ defq- THNV [x] (thv1 (car x)))

(§ defq- THOR [tha]
    (and tha
        (THPUSH *thtree* (list 'THOR tha))
        (SETQ THEXP (car tha))))

(§ defn- THOR2 [p]
    (COND (THMESSAGE (thpopt) nil)
        ((and (cadar *thtree*) (cdadar *thtree*))
            (RPLACA (cdar *thtree*) (cdadar *thtree*))
            (SETQ THEXP
                (COND (p (PROG1 (caadar *thtree*) (or (cadar *thtree*) (thpopt))))
                    ((car (caadar *thtree*))))))
        (:else (thpopt) nil)))

(defn- thorf [] (THOR2 true))

(defn- thort [] (thpopt) *thvalue*)

(defn- thpopt [] (set! *thtree* (cdr *thtree*)))

(§ defq- THPROG [tha]
    ;; THBIND HACKS THALIST TO BIND THE VARIABLES.
    ;; IT ALSO HACKS THTREE SO WE CAN UNDO IT IF NEEDED.
    (THBIND (car tha))
    ;; PUT THPROG MARK ON THTREE.
    ;; THE FIRST THA IS A POINTER ONE BEFORE THE NEXT PART OF THE THPROG TO BE HANDELED.
    ;; THE SECOND ONE WILL BE KEPT WHOLE TO SEARCH FOR PROG TAGS.
    (THPUSH *thtree* (list 'THPROG tha nil tha))
    ;; CALL WORKHORSE
    (THPROGA))

(§ defn- THPROGA []
    (let [x (cdar *thtree*)]
        ;; ODD CASE WHERE THE THPROG HAS NO SUBEXPRESSIONS.  RETURN SUCCESS.
        (COND ((nil? (cdar x))
                (thpopt)
                'THNOVAL)
            ;; NEXT ITEM IS AN ATOM, HENCE A THPROG TAG.
            ((term? (cadar x))
                ;; USE THEXP TO MARK IT ON THTREE.
                (SETQ THEXP (list 'THTAG (cadar x)))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA x (cdar x))
                *thvalue*)
            ;; OTHERWISE NEXT EXPRESSION TO BE EVALUATED IS THE NEXT EXPRESSION OF THE THPROG.
            (:else
                (SETQ THEXP (cadar x))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA x (cdar x))
                *thvalue*))))

;; THBRANCH AND THBRANCHUN ARE THE MAIN FUNCTIONS IN CHARGE OF HANDLING THE EFFECTS OF SUCCESS AND FAILURE.
;; THEY ARE ONLY CALLED BY THPROGT AND THPROGF.

(defn- thprogf [] (THBRANCHUN) nil)

(defn- thprogt [] (THBRANCH) (THPROGA))

;; MAKE SURE THAT THE PATTERN HAS NO UNASSIGNED VARIABLES IN IT.  XX, NATURALLY ENOUGH, IS THE PATTERN.
;; SINCE THPURE IS ALWAYS CALLED AFTER THVARSUBST, ANY VARIABLES WHICH DO HAVE ASSIGNMENTS WILL HAVE GONE AWAY,
;; REPLACED BY THEIR ASSIGNMENTS, SO ALL WE NEED TO DO IS LOOKING FOR ANY VARIABLES APPEARING AT ALL.

(defn- thpure [xx] (not-any? thvar? xx))

(defn- thputprop [ato ind val]
    (THPUSH *thtree* (list 'THMUNG (list (list 'putprop! (list 'quote ato) (list 'quote ind) (list 'quote (getprop ato ind))))))
    (putprop! ato ind val))

(§ defn- THREM1 [thb]
    ;; THREM1 IS ROUGHLY THE SAME AS THIP, BUT FOR REMOVING ASSERTIONS FROM THE DATABASE
    ;; HENCE ALL COMMENTS WILL BE GUIDES TO THE CORRESPONDENCE BETWEEN THREM1 AND THIP

    ;; THB = THI IN THIP
    (let [THA nil THSV nil THA1 nil THA2 nil THA3 nil THA4 nil THA5 nil THONE nil THPC nil]
        ;; THA AND THA1 DO THE WORK OF THT1 IN THIP
        ;; THA1 = THT2
        ;; THA3 = THT3
        ;; THA4, THA5, THONE, AND THPC ARE NEW
        (SETQ THNF (inc THNF))
        ;; THIS COND SERVES THE SAME PURPOSE AS THE FIRST COND IN THIP
        (COND ((and (term? thb) (not (= thb '?)) (not (number? thb)))
                (SETQ THA thb))
            ((or (= thb '?) (memq (car thb) '(THV THNV)))
                (COND (THFST (RETURN 'THVRB))
                    ((SETQ THA 'THVRB))))
            ((RETURN 'THVRB)))
        ;; ALL THE REST SERVES THE SAME PURPOSE AS THE SECOND COND IN THIP.
        ;; IT WAS ORIGINALLY WRITTEN AS A SINGLE COND, BUT THE COMPILER BARFED ON IT,
        ;; SO IT WAS BROKEN UP INTO BITE SIZE PIECES.
        (SETQ THA1 (getprop THA THWH))
        (or THA1 (RETURN nil))
        (and (= THA1 'THNOHASH) (RETURN 'THBQF))
        (SETQ THA2 (THBA THNF THA1))
        (or THA2 (RETURN nil))
        (SETQ THA3 (THBA THAL (cadr THA2)))
        (or THA3 (RETURN nil))
        (SETQ THA4 (cadr THA3))
        (SETQ THPC (not (= THWH 'THASSERTION)))
        (SETQ THA5
            (COND ((or THFST THFSTP) (THBA THBS (cdr THA4)))
                ((THBA (if THPC THON (car THON)) (cdr THA4)))))
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
        (remprop! THA THWH)
        THONE))

(defn- thrembindf [] (set! *thalist* (cadar *thtree*)) (thpopt) nil)

(defn- thrembindt [] (set! *thalist* (cadar *thtree*)) (thpopt) *thvalue*)

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
                    (SETQ THWH (car (SETQ THB1 (getprop thb 'THEOREM))))
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
                (SETQ THFOO (concat THFOO (list (COND ((= THON 'THVRB) (car THB1))))))
                (SETQ THB1 (cdr THB1))
                (GO THP1)))
        (SETQ THFST nil)
        (dorun (map #'THREM1 (cdr THB1)))
        (SETQ THNF 0)
        (dorun (map #'THREM1 THFOO))
        THON))

(defn- thremprop [ato ind]
    (THPUSH *thtree* (list 'THMUNG (list (list 'putprop! (list 'quote ato) (list 'quote ind) (list 'quote (getprop ato ind))))))
    (remprop! ato ind))

(§ defq- THRESTRICT [thb]
    (let [x (thgal (car thb) *thalist*)]
        (COND ((term? x)
                (terpri)
                (print "THRESTRICT IGNORED - CONTINUING")
                (print \space))
            ((THRPLACD (cdr x) (THUNION (cddr x) (cdr thb)))))
        x))

(§ defq- THRETURN [x]
    (APPLY #'THSUCCEED (cons 'THPROG x)))

(§ defn- THRPLACA [x y]
    (let [THML nil]
        (THRPLACAS x y)
        (THPUSH *thtree* (list 'THMUNG THML))
        x))

(§ defn- THRPLACAS [x y]
    (THPUSH THML (list 'THURPLACA x (car x)))
    (RPLACA x y))

(§ defq- THURPLACA [l] (RPLACA (car l) (cadr l)))

(§ defn- THRPLACD [x y]
    (let [THML nil]
        (THRPLACDS x y)
        (THPUSH *thtree* (list 'THMUNG THML))
        x))

(§ defn- THRPLACDS [x y]
    (THPUSH THML (list 'THURPLACD x (cdr x)))
    (RPLACD x y))

(§ defq- THURPLACD [l] (RPLACD (car l) (cadr l)))

(§ defq- THSETQ [thl1]
    (let [THML nil thl thl1]
    =>  (COND
            ((nil? thl)
                (THPUSH *thtree* (list 'THMUNG THML))
                (RETURN *thvalue*))
            ((nil? (cdr thl))
                (terpri)
                (pr thl1)
                (ert "ODD NUMBER OF GOODIES - THSETQ"))
            ((term? (car thl))
                (THPUSH THML (list 'SETQ (car thl) (list 'quote (eval (car thl)))))
                (SET (car thl) (set! *thvalue* (eval (cadr thl)))))
            (:else (THRPLACAS (cdr (thsgal (car thl)))
                (set! *thvalue* (THVAL (cadr thl) *thalist*)))))
        (SETQ thl (cddr thl))
        (GO =>)))

(defn- thsgal [x]
    (sassq (cadr x) *thalist* #(let [y (list (cadr x) 'THUNASSIGNED)] (set! *thalist* (conj *thalist* y)) y)))

(§ defn- thstate [& a]
    ;; PRINTS THAT PART OF THE STATE OF THE MICRO-PLANNER WORLD SPECIFIED BY THE INDICATORS IN REREADABLE FORM.
    ;; NOTE THAT IT IS BLIND TO ASSERTIONS THAT BEGIN WITH EITHER NUMBERS, LIST STRUCTURE, NOHASHED ATOMS OR NON-INTERNED ATOMS.
    (let [thp nil]
        (terpri)
        (pr '(THDATA))
        (dorun (map #'(lambda [BUCKET]
            (dorun (map #'(lambda [THATOM]
                (dorun (map #'(lambda [THWH]
                    (and (SETQ thp (getprop THATOM THWH)) (SETQ thp (assq 1 (cdr thp)))
                        (dorun (map #'(lambda [LENGTH-BUCKET]
                            (dorun (map #'(lambda [asrt]
                                    (terpri)
                                    (if (= THWH 'THASSERTION) (pr asrt) (pr (list asrt))))
                                (cddr LENGTH-BUCKET))))
                            (cdr thp)))))
                    (or a '(THASSERTION THANTE THCONSE THERASING)))))
                BUCKET)))
            (MAKOBLIST nil)))
        (terpri)
        nil))

(§ defq- THSUCCEED [tha]
    (or (not tha)
        (let [THX nil]
            (and (= (car tha) 'THEOREM)
                (SETQ tha (cons 'THPROG (cdr tha))))
            (SETQ THBRANCH *thtree*)
            (SETQ THABRANCH *thalist*)
        =>  (COND
                ((nil? *thtree*)
                    (terpri)
                    (pr tha)
                    (ert "OVERPOP - THSUCCEED"))
                ((= (caar *thtree*) 'THREMBIND)
                    (set! *thalist* (cadar *thtree*))
                    (thpopt)
                    (GO =>))
                ((= (caar *thtree*) (car tha))
                    (thpopt)
                    (RETURN (COND ((cdr tha) (eval (cadr tha))) ('THNOVAL))))
                ((and (= (car tha) 'THTAG) (= (caar *thtree*) 'THPROG) (SETQ THX (memq (cadr tha) (cadddr (car *thtree*)))))
                    (RPLACA (cdar *thtree*) (cons nil THX))
                    (RETURN (thprogt)))
                (:else (thpopt) (GO =>)))
        nil)))

(§ defn- THTAE [xx]
    (COND
        ((term? xx) nil)
        ((= (car xx) 'THUSE)
            (doall (map #'(lambda [x]
                    (COND ((not (and (set! *thxx* (getprop x 'THEOREM)) (= (car *thxx*) TYPE)))
                            (terpri)
                            (pr x)
                            (list 'THAPPLY (ert "BAD THEOREM - THTAE") (car THX)))
                        (:else (list 'THAPPLY x (car THX)))))
                (cdr xx))))
        ((= (car xx) 'THTBF)
            (doall (map #'(lambda [y]
                    (COND (((cadr xx) y)
                        (list (list 'THAPPLY y (car THX))))))
                (COND (THY1 THY)
                    ((SETQ THY1 true)
                        (SETQ THY (THMATCHLIST (car THX) TYPE)))))))
    (:else (terpri) (pr xx) (THTAE (ert "UNCLEAR RECOMMENDATION - THTAE")))))

(§ defq- THTAG [a]
    (when (car a)
        (THPUSH *thtree* (list 'THTAG (car a)))))

(defn- thtagf [] (thpopt) nil)

(defn- thtagt [] (thpopt) *thvalue*)

(defn- thtrue [_] true)

(§ defn- THTRY1 []
    ;; TRIES NEXT RECOMMENDATION ON TREE FOR THGOAL.
    ;; THZ = (THGOAL PATTERN EXPANDED-RECOMMENDATIONS)
    (let [THX nil THY nil THZ (car *thtree*) THW nil THEOREM nil]
        (SETQ THY (cddr THZ))                                       ;; = RECOMMENDATIONS
        (RPLACD THY (dec (cdr THY)))
    NXTREC (COND ((or (nil? (car THY)) (zero? (cdr THY)))
            (RETURN nil)))                                          ;; RECOMMENDATIONS EXHAUSTED. FAIL
        (SETQ THX (caar THY))
        (GO (car THX))
    THNUM (RPLACD THY (cadr THX))
        (RPLACA THY (cdar THY))
        (GO NXTREC)
    THDBF (set! *tholist* *thalist*)
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
    THTBF1 (COND ((not (and (SETQ THW (getprop THEOREM 'THEOREM)) (= (car THW) 'THCONSE)))
                (terpri)
                (pr THEOREM)
                (SETQ THEOREM (ert "BAD THEOREM - THTRY1"))
                (COND ((= THEOREM true) (GO NXTREC))
                    (:else (GO THTBF1)))))
        (COND ((PROG1 (and ((cadr THX) (caaddr THX)) (thapply1 THEOREM THW (cadr THZ)))
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
            (when THZ (list (list 'THTBF (cadr x) THZ))))
        ;; DO THE SAME THING, ONLY FOR DATABASE FILTERS.
        ((= (car x) 'THDBF)
            (COND ((not THY1) (SETQ THY1 true) (SETQ THY (THMATCHLIST THA2 'THASSERTION))))
            (when THY (list (list 'THDBF (cadr x) THY))))
        ;; THUSE STATEMENTS ARE TRANSLATED INTO THTBF THTRUE STATEMENTS,
        ;; WHICH THE "BUCKET" IS THE LIST GIVEN IN THE THUSE.
        ((= (car x) 'THUSE)
            (list (list 'THTBF 'thtrue (cdr x))))
        ((= (car x) 'THNUM)
            (list x))
        (:else (terpri) (pr x) (THTRY (ert "UNCLEAR RECOMMENDATION - THTRY")))))

(§ defn- THUNDOF []
    (COND ((nil? (caddar *thtree*)) (thpopt))
        (:else (set! *thxx* (cddar *thtree*))
            (set! *thalist* (caadr *thxx*))
            (RPLACA (cdr *thxx*) (cdadr *thxx*))
            (set! *thtree* (caar *thxx*))
            (RPLACA *thxx* (cdar *thxx*))))
    nil)

(defn- thundot [] (thpopt) true)

(§ defq- THUNIQUE [tha]
    (SETQ tha (cons 'THUNIQUE (doall (map eval tha))))
    (let [x *thalist*]
    =>  (COND ((nil? x) (THPUSH *thalist* tha) (RETURN true))
            ((= (caar x) 'THUNIQUE)
                (COND ((= (car x) tha) (RETURN nil)))))
        (SETQ x (cdr x))
        (GO =>)))

(defn- thv1 [x]
    ;; (THV1 'X) IS THE VALUE OF THE PLANNER VARIABLE.
    ;; ($? X) RETURNS ERROR MESSAGE IF X UNBOUND OR UNASSIGNED.
    (set! *thxx* x)
    (let [x (cadr (sassq x *thalist* #(do (terpri) (pr *thxx*) (ert "THUNBOUND - THV1"))))]
        (if (= x 'THUNASSIGNED) (do (terpri) (pr *thxx*) (ert "THUNASSIGNED - THV1")) x)))

(§ defq- THV [x]
    ;; (THV X) IS THE VALUE OF THE PLANNER VARIABLE ($? X)
    (thv1 (car x)))

(§ defn- THVAL [THEXP *thalist*]
    ;; CORRESPONDS TO LISP EVAL.
    ;; THEXP IS THE EXPRESSION TO BE THVALUATED.
    ;; THALIST IS THE VARIABLE BINDING LIST.
    (let [*thtree* nil *thvalue* 'THNOVAL THBRANCH nil *tholist* nil THABRANCH nil THE nil THMESSAGE nil]
        (SETQ THV '(THV THNV))
        ;; "THE" BECOMES THE CURRENT EXPRESSION.
        ;; THEXP IS RESERVED FOR FURTHER EXPRESSIONS,
        ;; WHICH SHOULD BE THVALED BEFORE WE GO TO THE NEXT ITEM OF ACTUAL CODE.
        ;; FOR EXAMPLE, THASSERT USES THIS FEATURE TO PROCESS ANTECEDENT THEOREMS.
    GO  (SETQ THE THEXP)
        (SETQ THEXP nil)
        ;; EVAL THE CURRENT EXPRESSION TO BE THVALED.
        ;; NOTE THAT EACH PLANNER FUNCTION CORRESPONDS TO THREE LISP FUNCTIONS:
        ;; ONE TO SET THINGS UP (THIS IS WHAT IS GETTING EVALED AT THIS POINT),
        ;; ONE TO HANDLE SUCCESS AND ONE FOR FAILURE.
        (COND ((ERRSET (set! *thvalue* (eval THE))))
            ;; IF THERE WAS A LISP ERROR, REPORT IT TO THE USER.
            (:else (terpri) (pr THE) (set! *thvalue* (ert "LISP ERROR - THVAL"))))
        ;; USUALLY THEMESSAGE WILL BE NIL.
        ;; EXCEPTION IS WHEN USER HAS USED THE THMESSAGE FUNCTION.
    GO1 (COND (THMESSAGE (GO MFAIL))
            ;; IF THEXP IS NON NIL, IT MEANS THAT WE HAVE MORE PLANNER TO WORK ON BEFORE GOING TO NEXT LINE OF USER CODE.
            (THEXP (GO GO))
            ;; IF THVALUE IS NON NIL, IT MEANS THAT SO FAR THE THEOREM IS SUCCEEDING.
            (*thvalue* (GO SUCCEED))
            ;; ELSE WE ARE IN A FAILURE SITUATION.
            (:else (GO FAIL)))
    SUCCEED
        ;; SAVE CURRENT STATE OF THTREE AND THALIST IN CASE WE HAVE TO BACK UP.
        (COND ((nil? THBRANCH)
            (SETQ THBRANCH *thtree*)
            (SETQ THABRANCH *thalist*)))
        ;; IF THE THTREE IS NIL, IT MEANS THAT THE THPROG OR WHATEVER HAS BEEN COMPLETED,
        ;; SO THERE ARE NO MORE EXPRESSIONS TO DO.  ALL THEOREMS ACT LIKE A THPROG, INCLUDING
        ;; PUTTING ITS MARK ON THTREE, SEE THAPPLY, HENCE NO NEED TO GROW MORE BRANCHES ON THTREE.
        (COND ((nil? *thtree*)
                (RETURN *thvalue*))
            ;; THIS IS THE NORMAL CASE.
            ;; WE EVAL THE SUCCEED-FUNCTION OF THE PLANNER FUNCTION WHICH JUST SUCCEEDED.
            ((SETQ THEXP (getprop (caar *thtree*) 'THSUCCEED))
                (GO GO2))
            ;; IN CASE OF LOSSAGE, LETS THE USER SUCCEED ANYWAY.
            ((ert "BAD SUCCEED - THVAL")
                (GO SUCCEED))
            ((GO FAIL)))
        ;; HAS TO DO WITH FAILURE + MESSAGE
    MFAIL (COND ((= (car THMESSAGE) *thtree*)
            (SETQ THEXP (cadr THMESSAGE))
            (SETQ THMESSAGE nil)
            (GO GO)))
        ;; IF THTREE IS NIL, WE HAVE FAILED THE ENTIRE EXPRESSION.
    FAIL (COND ((nil? *thtree*)
                (RETURN nil))
            ;; NORMAL CASE.  EVAL THE FAILURE FUNCTION ASSOCIATED
            ;; WITH THE PLANNER FUNCTION WHICH JUST FAILED.
            ((SETQ THEXP (getprop (caar *thtree*) 'THFAIL))
                (GO GO2))
            ((ert "BAD FAIL - THVAL")
                (GO SUCCEED))
            ((GO FAIL)))
        ;; THEXP AT THIS POINT IS THE APPROPRIATE SUCCESS OR FAILURE ASSOCIATED FUNCTION.
        ;; EVAL IT AND AT THE SAME TIME,
        ;; SET IT TO NIL IN CASE WE NEED THEXP FOR MORE EXPRESSIONS TO BE PROCESSED.
    GO2 (set! *thvalue* ((PROG1 THEXP (SETQ THEXP nil))))
        ;; GO THROUGH THE ENTIRE PROCESS AGAIN.
        ;; A TYPICAL PROCESS IN SUCCESS IS TO KEEP REMOVING EXPRESSIONS FROM THTREE
        ;; UNTIL WE GET BACK TO THE THREE ENTRY PUT ON BY THPROG.
        ;; AT THIS POINT IT EVALS THPROGT, AND SEE THAT LISTING
        (GO GO1)))

(defn- thvar? [x]
    ;; IS X A PLANNER VARIABLE?
    (memq (car x) '(THV THNV)))

(§ defn- THVARS2 [x]
    ;; THIS IS THE WORKHORSE FOR THVARSUBST.
    ;; X IS A SINGLE ITEM FROM A PATTERN.
    (let [a nil]
        (and (term? x) (RETURN x))
        ;; IF IT'S AN ATOM, NOTHING NEED BE DONE.
        (and (= (car x) 'THEV)
            (SETQ x (THVAL (cadr x) *thalist*)))
        ;; IF THE EXPRESSION HAS A $E BEFORE IT, THVAL BEFORE GOING ON.
        (or (thvar? x) (RETURN x))
        ;; IF THE ITEM IS NOT A VARIABLE, IT MUST BE SOME RANDOM LIST, SO IT HAS NO ASSIGNED VALUE.
        (SETQ a (thgal x *thalist*))
        ;; AT THIS POINT X MUST BE A VARIABLE, SO FIND ITS ASSIGNMENT, THAT'S WHAT THGAL DOES.
        ;; THALIST IS WHERE THE VARIABLE ASSIGNMENTS RESIDE.
        (RETURN (COND
            ((= (cadr a) 'THUNASSIGNED) x)
            ;; IF THE VARIABLE IS UNASSIGNED, THEN RETURN THE ACTUAL VARIABLE.
            ((and THY (= (car x) 'THNV))
                ;; THY WILL BE T JUST IN THE CASES WHERE THVARSUBST WAS CALLED BY A THGOAL SITUATION.
                ;; IT IS THEN NECESSARY TO IMMEDIATELY HACK IN A THUNASSIGNED SO THAT IF THE SAME VARIABLE IS USED
                ;; TWICE IN THE SAME PATTERN, WE WON'T PUT IN ITS OLD VALUE THE SECOND TIME IT IS ENCOUNTERED.
                (THRPLACA (cdr a) 'THUNASSIGNED)
                x)
            ;; OTHERWISE THE ASSIGNMENT IS THE SECOND ELEMENT IN THE BINDING LIST.
            (:else (cadr a))))))

(§ defn- THVARSUBST [thx THY]
    ;; THX IS A GOAL OR ASSERTION PATTERN OR THEOREM NAME.
    ;; THIS FUNCTION RETURNS THE SAME PATTERN, EXCEPT IN PLACE OF ALL ASSIGNED VARIABLES
    ;; THERE WILL BE THE VALUES THEY ARE ASSIGNED TO.
    (COND ((= (car thx) 'THEV)
            ;; IF THE CAR IS THEV, IT MEANS THAT THERE WAS A $E BEFORE THE PATTERN,
            ;; IN WHICH CASE WE ARE TO GET THE REAL PATTERN BY THVALUATING WHAT IS THERE.
            (SETQ thx (THVAL (cadr thx) *thalist*)))
        ((thvar? thx)
            (SETQ thx (eval thx))))
    ;; THVAR TESTS TO SEE IF ARG IS A VARIABLE.
    ;; IF THE PATTERN IS A SINGLE VARIABLE, THE PROGRAM ASSUMES THERE SHOULD BE AN IMPLICIT THVAL.
    ;; UNLESS THE ASSERTEE IS A THEOREM NAME, GO THROUGH IT PLACE BY PLACE WITH THVARS2.
    (if (term? thx) thx (doall (map #'THVARS2 thx))))

(§ defq- THVSETQ [tha]
    (let [a tha]
    =>  (COND ((nil? a) (RETURN *thvalue*))
            ((nil? (cdr a))
                (terpri)
                (pr tha)
                (ert "ODD NUMBER OF GOODIES - THVSETQ"))
            (:else (set! *thvalue*
                (car (RPLACA (cdr (thsgal (car a))) (THVAL (cadr a) *thalist*))))))
        (SETQ a (cddr a))
        (GO =>)))

(putprop! 'THTAG 'THFAIL 'thtagf)
(putprop! 'THTAG 'THSUCCEED 'thtagt)
(putprop! 'THGOAL 'THSUCCEED 'thgoalt)
(putprop! 'THGOAL 'THFAIL 'thgoalf)
(putprop! 'thfail? 'THFAIL 'thfail?f)
(putprop! 'thfail? 'THSUCCEED 'thfail?t)
(putprop! 'THAMONG 'THFAIL 'THAMONGF)
(putprop! 'THFIND 'THFAIL 'thfindf)
(putprop! 'THFIND 'THSUCCEED 'THFINDT)
(putprop! 'THPROG 'THSUCCEED 'thprogt)
(putprop! 'THAND 'THSUCCEED 'THANDT)
(putprop! 'THMUNG 'THSUCCEED 'thmungt)
(putprop! 'THERASE 'THSUCCEED 'theraset)
(putprop! 'THASSERT 'THSUCCEED 'thassertt)
(putprop! 'THOR 'THSUCCEED 'thort)
(putprop! 'THCOND 'THSUCCEED 'THCONDT)
(putprop! 'THAND 'THFAIL 'thandf)
(putprop! 'THPROG 'THFAIL 'thprogf)
(putprop! 'THMUNG 'THFAIL 'thmungf)
(putprop! 'THASSERT 'THFAIL 'thassertf)
(putprop! 'THERASE 'THFAIL 'therasef)
(putprop! 'THCOND 'THFAIL 'thcondf)
(putprop! 'THOR 'THFAIL 'thorf)
(putprop! 'THDO 'THSUCCEED 'THDOB)
(putprop! 'THDO 'THFAIL 'THDOB)
(putprop! 'THUNDO 'THFAIL 'THUNDOF)
(putprop! 'THUNDO 'THSUCCEED 'thundot)
(putprop! 'THMESSAGE 'THFAIL 'thmessagef)
(putprop! 'THMESSAGE 'THSUCCEED 'thmessaget)
(putprop! 'THREMBIND 'THSUCCEED 'thrembindt)
(putprop! 'THREMBIND 'THFAIL 'thrembindf)

#_(ns shrdlu.thtrac)

;; SYSTEM FUNCTIONS SUCH AS THGOAL, THASSERT, THERASE AND THEOREM
;; (ALL THMS) ARE TRACED IF THEY ARE ON "THTRACE".
;; THTRACES1 PUTS THEM THERE AND THUNTRACE TAKES THEM OFF.

;; THTRACE IS INITIALLY SET TO NIL BY TS PLNR.

(defn- thtrace [& a] (dorun (map thtrace1 a)))

(§ defn- thtrace1 [x]
    ;; VARIETY OF POSSIBLE INPUT FORMATS TRANSFORMED TO STANDARD 3 ELEMENT LIST
    ;; (OBJECT-TO-BE-TRACED TRACE-CONDITION BREAK-CONDITION)
    (let [x (cond
                (term? x) (list x true nil)
                (cddr x) x
                (some? (cdr x)) (list (car x) (cadr x) nil)
                :else (do (terpri) (pr x) (-print "BAD FORMAT") (RETURN nil)))
          y nil
    ]
        ;; IF OBJECT-TO-BE-TRACED IS A PARTICULAR THEOREM, THEN THE TRIPLET
        ;; '(THEOREM (THSEL 'CADR) (THSEL 'CADDDR))
        ;; IS GUARANTEED TO BE ON THTRACE IN ADDITION TO THE STANDARD TRIPLET.
        (COND ((getprop (car x) 'THEOREM)
            (COND ((SETQ y (assq 'THEOREM *thtrace*)) (RPLACD y '((THSEL #'cadr) (THSEL #'caddr))))
                ((set! *thtrace* (list x (concat '(THEOREM (THSEL #'cadr) (THSEL #'caddr)) *thtrace*)))))))
        ;; THTRACE IS UPDATED.  IF THE OBJECT-TO-BE-TRACED IS ALREADY ON THTHRACE, THEN
        ;; THE TRACE AND BREAK CONDITIONS ARE UPDATED, ELSE THE WHOLE TRIPLET IS PLACED ON THTRACE.
        (SETQ y (assq (car x) *thtrace*))
        (if y (RPLACD y (cdr x)) (set! *thtrace* (cons x *thtrace*)))
        x))

;; THUNTRACE REMOVES ELEMENTS OF ITS ARG FROM THTRACE.
;; IF NOT GIVEN ANY ARGS, THUNTRACE SETS THTRACE TO NIL.

(defn- thuntrace [& a]
    (when *thtrace*
        (let [! #(do (terpri) (pr %)) ! (if a #(if (memq (car %) a) (! %) %) !) ? (doall (filter ! *thtrace*))]
            (set! *thtrace* (when (seq ?) ?))))
    'done)

;; THTRACES IS ACTIVATED BY THGOAL, THASSERT, ... IF THTRACE IS NON-NIL,
;; THF IS SET TO THE PARTICULAR CANDIDATE FOR TRACEAGE, E.G. TO 'THGOAL
;; IF THE PLANNER FUNCTION THGOAL ACTIVATED THTRACES.
;; THL = THE INSTANTIATED ARG OF THF. SEE DESC OF X ON NEXT PAGE.

(§ defn- THTRACES [thf thl]
    (let [THY nil THZ nil THB nil]
        (and
            ;; THY SET TO TRIPLET ON THTRACE. IF NOT THERE, NO TRACING
            (SETQ THY (assq thf *thtrace*))
            ;; IF BOTH TRACE AND BREAK ARE FALSE, DON'T TRACE
            ;; SIDE EFFECT - THB SET TO VALUE OF BREAK
            (or (SETQ THB (THVAL (caddr THY) *thalist*)) (THVAL (cadr THY) *thalist*))
            ;; THZ IS SET TO THE TRACE FUNCTION FOR THE OBJECT-TO-BE-TRACED
            (or (SETQ THZ (getprop thf 'THTRACE)) (ert "THTRACES - TRACE LOSSAGE"))
            ;; THE TRACE FUNCTION IS EXECUTED
            (THZ thl THB)
            ;; IF THB IS NON-NIL, BREAK
            THB
            (ert nil))
        nil))

;; THE CAR OF THE TREE IS '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; THUS, THESE TWO FNS PRINT THE NAME OF THE TRACE POINT, "FAIL"-OR-"SUCCEED"
;; PRINT THVALUE IF ANY, AND FINALLY BREAK IF (THERT) IS PRESENT, THEN POP THE TREE

(putprop! 'THTRACES 'THFAIL
    '(lambda []
        (terpri)
        (pr (cadar *thtree*))
        (print " FAILED ")
        (evlis (cddar *thtree*))
        (thpopt)
        nil))

(putprop! 'THTRACES 'THSUCCEED
    '(lambda []
        (terpri)
        (pr (cadar *thtree*))
        (print " SUCCEEDED ")
        (evlis (cddar *thtree*))
        (thpopt)
        *thvalue*))

;; THE TRACE FNS THBKPT, THGOAL, THEOREM, THASSERT, AND THERASE PUSH ONTO THE TREE
;; '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;; X = THL = INSTANTIATED GOAL, ASSERTION OR ERASURE, NAME OF THE THM, OR MESSAGE OF THE BREAKPOINT

(putprop! 'THBKPT 'THTRACE
    '(lambda [x b]
        (THPUSH *thtree* (list 'THTRACES (gensym 'B) (and b '(ert nil))))
        (terpri)
        (print "PASSING BKPT ")
        (pr (cadar *thtree*))
        (print \space)
        ;; BY SETTING THBRANCH AND THABRANCH, A TRIPLE IS CREATED BY THVAL FOR BACKTRACKING.
        ;; THEN, THE TREE IS POPPED TO PREVENT THTRACES FROM TYPING OUT THE MEANINGLESS
        ;; THAT THE BREAKPOINT SUCCEEDED.
        (SETQ THBRANCH *thtree*)
        (SETQ THABRANCH *thalist*)
        (thpopt)
        (pr x)))

(putprop! 'THGOAL 'THTRACE
    '(lambda [x b]
        (THPUSH *thtree* (list 'THTRACES (gensym 'G) '(and *thvalue* (pr *thvalue*)) (and b '(ert nil))))
        (terpri)
        (print "TRYING GOAL ")
        (pr (cadar *thtree*))
        (print \space)
        (pr x)))

(putprop! 'THEOREM 'THTRACE
    '(lambda [x b]
        (THPUSH *thtree* (list 'THTRACES x '(and *thvalue* (pr *thvalue*)) (and b '(ert nil))))
        (terpri)
        (print "ENTERING THEOREM ")
        (pr x)))

(putprop! 'THASSERT 'THTRACE
    '(lambda [x b]
        (THPUSH *thtree* (list 'THTRACES (gensym 'A) (and b '(ert nil))))
        (terpri)
        (print "ASSERTING ")
        (pr (cadar *thtree*))
        (print \space)
        (pr x)))

(putprop! 'THERASE 'THTRACE
    '(lambda [x b]
        (THPUSH *thtree* (list 'THTRACES (gensym 'E) (and b '(ert nil))))
        (terpri)
        (print "ERASING ")
        (pr (cadar *thtree*))
        (print \space)
        (pr x)))

;; FOR THE TRACE-OBJECT 'THEOREM, IF ANY SPECIFIC THMS ARE TRACED, '(THSEL 'CADR) AND '(THSEL 'CADDDR)
;; ARE THE TRACE AND BREAK PREDICATES.  HENCE THTRACES CAUSES THESE EXPR'S TO BE THVALED.  THL IS SET
;; TO THE SECOND ARG OF THTRACES WHICH IN THIS CASE IS PRESUMABLY THE NAME OF THE PARTICULAR THM THAT
;; ACTIVATED THTRACES.  THSEL FIRST CHECKS TO SEE WHETHER THIS THM IS INDEPENDENTLY ON THTRACE.  IF NOT,
;; IT DOES NO MORE.  BUT IF IT IS, THX GETS SET TO THE THM'S TRIPLET.  THEN THX GETS SET TO EITHER THE
;; TRACE (ARG = 'CADR) OR THE BREAK (ARG = 'CADDDR) CONDITION OF THE TRIPLET.  FINALLY, THESE CONDITIONS
;; ARE THVALED, THUS THSEL SERVES THE PURPOSE OF REFERENCING THE TRACE AND BREAK PREDICATES OF PARTICULAR
;; THMS ON THTRACE.

(§ defn- THSEL [thf]
    (let [THX nil] (and (SETQ THX (assq THL *thtrace*)) (SETQ THX (thf THX)) (THVAL THX *thalist*))))

#_(ns shrdlu.blockp)

;; ################################################################
;;
;;     BLOCKP > MICRO-PLANNER CODE FOR THE "BLOCKS" MICRO-WORLD
;;
;; ################################################################

(putprop! 'TA-AT 'THEOREM
    '(THANTE (x y) (!AT ($? x) ($? y)) (THRPLACA (cdr (atab ($? x))) ($? y))))

(putprop! 'TA-CONTAIN 'THEOREM
    '(THANTE (x y z)
        (!AT ($? x) ?)
        (THGOAL (!MANIP ($? x)))
        (THGOAL (!SUPPORT ($? y) ($? x)))
        (THOR (THAND (THGOAL (!IS ($? y) !BOX)) (THVSETQ ($_ z) ($? y)))
            (THGOAL (!CONTAIN ($? z) ($? y))))
        (THASSERT (!CONTAIN ($? z) ($? x)))))

(putprop! 'TA-EXISTS 'THEOREM
    '(THANTE (x) (!EXISTS ($? x)) (THSUCCEED)))

(def- NOSTACKS true)

(putprop! 'TA-SUPP 'THEOREM
    '(THANTE (x y z)
        (!AT ($? x) ($? y))
        (THCOND ((THVSETQ ($_ z) (support ($? y) (size ($? x)) ($? x)))
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
                ((THVSETQ ($_ y) (gensym 'STACK))
                (THASSERT (!PART ($? x) ($? y)))
                (THASSERT (!PART ($? z) ($? y)))
                (THASSERT (!IS ($? y) !STACK))
                (THASSERT (!EXISTS ($? y)) (THUSE TA-EXISTS)))))
            ((THGOAL (!GRASPING ($? x))))
            ((ert "TA-SUPP")))))

(putprop! 'TC-2 'THEOREM
    '(THCONSE (x y yy)
        (($? x) ($? y))
        (THGOAL (!CHOOSE ($? y) ($_ yy) ($E (getprop ($? x) 'CHOOSE)))
            (THUSE TC-CHOOSE))
        (THGOAL (($? x) ($? yy)) (THTBF thtrue))))

(putprop! 'TC-3 'THEOREM
    '(THCONSE (x y z YY ZZ)
        (($? x) ($? y) ($? z))
        (THGOAL (!CHOOSE ($? y) ($_ YY) ($E (getprop ($? x) 'CHOOSE)))
            (THUSE TC-CHOOSE))
        (THGOAL (!CHOOSE ($? z) ($_ ZZ) ($E (getprop ($? x) 'CHOOSE2)))
            (THUSE TC-CHOOSE))
        (THGOAL (($? x) ($? YY) ($? ZZ)) (THTBF thtrue))))

(putprop! 'TC-ASMUCH 'THEOREM
    '(THCONSE (measure x y)
        (!ASMUCH measure ($? x) ($? y))
        (THVSETQ ($_ measure) (getprop ($? measure) 'MEASFN))
        (not (< (($? measure) ($? x)) (($? measure) ($? y))))))

(putprop! 'TC-BELONG 'THEOREM
    '(THCONSE (x y)
        (!BELONG ($? x) ($? y))
        (THAMONG ($? y) '(ßSHRDLU))
        (THGOAL (!PHYSOB ($? x)) (THUSE TC-PHYSOB))))

(putprop! 'TC-CALL 'THEOREM
    '(THCONSE (x y z)
        (!CALL ($? x) ($? y))
        (THCOND ((THGOAL (!CALL ($_ z) ($? y)))
                (terpri)
                (pr ($? y))
                (-print "NAME ALREADY USED")
                nil)
            ((THASSERT (!CALL ($? x) ($? y)))
                (THASSERT (!IS ($? y) !NAME))
                (!PROPDEFINE ($? y))
                (or doit? (set! *plan* (cons true *plan*)))))))

(putprop! 'TC-CHOOSE 'THEOREM
    '(THCONSE (x y z w)
        (!CHOOSE ($? x) ($? y) ($? z))
        (THCOND
            ((and (THASVAL ($? x)) (not (oss? ($? x)))) (THSETQ ($_ y) ($? x)))
            ((THASVAL ($? x))
                (or (not discourse?)
                    (thputprop (variable? ($? x)) 'NG ($? x)))
                (THSETQ ($_ y) (FINDCHOOSE ($? x) ($? z) nil)))
        ((THGOAL (!MANIP ($? y))) (THNOT (THGOAL (!SUPPORT ($? y) ?)))))))

(putprop! 'TC-CHOOSEPLACE 'THEOREM
    '(THCONSE (x) (!CHOOSEPLACE ($? x)) (ert "CHOOSEPLACE UNDEFINED")))

(putprop! 'TC-CLEARTOP 'THEOREM
    '(THCONSE (x y (WHY (EV)) EV)
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
            (THSUCCEED THEOREM)))))

(putprop! 'TC-EXISTS 'THEOREM
    '(THCONSE (x) (!EXISTS ($? x)) (THSUCCEED)))

(putprop! 'TC-FINDSPACE 'THEOREM
    '(THCONSE (surf size obj space)
        (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space))
        (THOR (and (not (memq ($? surf) '(ßBOX ßTABLE))) (not (getprop '!NOCLEAR 'THASSERTION))
            (THSETQ ($_ space) (FINDSPACE 'CENTER ($? surf) ($? size) ($? obj))))
        (and (or (= ($? surf) 'ßBOX) (and (not (= ($? surf) 'ßTABLE)) (getprop '!NOCLEAR 'THASSERTION)))
            (THSETQ ($_ space) (FINDSPACE 'PACK ($? surf) ($? size) ($? obj))))
        (THSETQ ($_ space) (FINDSPACE 'RANDOM ($? surf) ($? size) ($? obj))))))

(putprop! 'TC-GET-RID-OF 'THEOREM
    '(THCONSE (x y (WHY (EV)) EV)
        (!GET-RID-OF ($? x))
        (or nomem? (THVSETQ ($_ EV) ($? WHY)))
    =>  (THCOND ((nil? ($? x)))
            ((term? ($? x))
                (MEMORY)
                (THGOAL (!FINDSPACE ßTABLE ($E (size ($? x))) ($? x) ($_ y)) (THUSE TC-FINDSPACE))
                (THGOAL (!PUT ($? x) ($? y)) (THNODB) (THUSE TC-PUT))
                (MEMOREND (!GET-RID-OF ($? EV) ($? x))))
            ((THGOAL (!GET-RID-OF ($E (car ($? x)))) (THUSE TC-GET-RID-OF))
                (or (THSETQ ($_ x) (cdr ($? x))) (THSUCCEED THEOREM))
                (THGO =>)))))

(putprop! 'TC-GRASP 'THEOREM
    '(THCONSE (x y (WHY (EV)) EV)
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
        (or nomem?
            (THSETQ GRASPLIST (cons (list *thtime* ($? x)) GRASPLIST)))
        (THCOND (doit? (THOR (GRASP ($? x)) (and (UNGRASP) nil)))
            ((THSETQ *plan* (cons (list 'GRASP (list 'quote ($? x))) *plan*))))))

(putprop! 'TC-LOC 'THEOREM
    '(THCONSE (x y z loc)
        (($? loc) ($? x) ($? y) ($? z))
        (THOR (THGOAL (!MANIP ($? y))) (THGOAL (!IS ($? y) !BOX)))
        (THOR (THGOAL (!MANIP ($? z))) (THGOAL (!IS ($? z) !BOX)))
        (not (= ($? y) ($? z)))
        (LOCGREATER LOC ($? y) ($? z)
            ((lambda [x] (COND ((= x '!RIGHT) #'car) ((= x '!BEHIND) #'cadr) ((= x '!ABOVE) #'caddr) ((ert "TC-LOC")))) ($? x)))))

(putprop! 'TC-MAKESPACE 'THEOREM
    '(THCONSE (surf size obj space x (WHY (EV)) EV)
        (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space))
        (THNOT (THGOAL (!IS ($? surf) !BOX)))
        (MEMORY)
    => (THAND (THGOAL (!SUPPORT ($? surf) ($_ x)))
            (THGOAL (!GET-RID-OF ($? x)) (THUSE TC-GET-RID-OF)))
        (THOR (THGOAL (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space)) (THUSE TC-FINDSPACE))
            (THGO =>))
        (MEMOREND (!MAKESPACE ($? EV) ($? surf)))))

(putprop! 'TC-MORE 'THEOREM
    '(THCONSE (measure x y)
        (!MORE ($? measure) ($? x) ($? y))
        (THVSETQ ($_ measure) (getprop ($? measure) 'MEASFN))
        (> (($? measure) ($? x)) (($? measure) ($? y)))))

(putprop! 'TC-MOVEHAND 'THEOREM
    '(THCONSE (x y w z)
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
                (or nomem?
                    (thputprop ($? x) 'HISTORY
                        (cons (list *thtime*
                                ($? z)
                                (cadar (or (THVAL '(THGOAL (!SUPPORT ($? y) ($? x))) (cons (list 'Y 'THUNASSIGNED) *thalist*))
                                    '((nil ßHAND))))
                                nil)
                            (getprop ($? x) 'HISTORY)))))
        ((THGOAL (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2))))))

(putprop! 'TC-MOVEHAND2 'THEOREM
    '(THCONSE (y loc)
        (!MOVEHAND2 ($? y))
        (COND ((= ($? y) HANDAT) (THSUCCEED THEOREM))
            ((and (<= 32 (car ($? y)) 608) (<= 0 (cadr ($? y)) 608) (<= 0 (caddr ($? y)) 512))))
        (THVSETQ ($_ loc) HANDAT)
        (THSETQ HANDAT ($? y))
        (THSETQ *thtime* (inc *thtime*))
        (THCOND (doit? (THOR (eval (cons 'MOVETO HANDAT))
                    (let [ADJUST nil] (eval (cons 'MOVETO ($? loc))) nil)))
            ((THSETQ *plan* (cons (cons 'MOVETO ($? y)) *plan*))))))

(putprop! 'TC-NAME 'THEOREM
    '(THCONSE (x)
        (!NAME ($? x))
        (THVSETQ ($_ x) (listify ($? x)))
        (THVSETQ ($_ x) (THFIND ALL ($? y) (y z) (THAMONG ($? z) ($? x)) (THOR (THGOAL (!CALL ($? z) ($? y))) (THSETQ ($_ y) ($? z)))))))

(putprop! 'TC-NOTICE 'THEOREM
    '(THCONSE (x)
        (!NOTICE ($? x))
        (COND (doit? (BLINK ($? x)) (THSUCCEED))
            ((THSETQ *plan* (cons (list 'BLINK (list 'quote ($? x))) *plan*))))))

(putprop! 'TC-ON 'THEOREM
    '(THCONSE (x y z)
        (!ON ($? x) ($? y))
        (THOR (THGOAL (!SUPPORT ($? y) ($? x))) (THAND (THASVAL ($? x)) (THGOAL (!SUPPORT ($_ z) ($? x))) (THGOAL (!ON ($? z) ($? y)) (THUSE TC-ON))))))

(putprop! 'TC-PACK 'THEOREM
    '(THCONSE (OBJ SURF BLOCKS PYR x y)
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
                    (THGO =>)))))

(putprop! 'TC-PART 'THCONSE
    '(THCONSE (x y z)
        (!PART ($? x) ($? y))
        (THGOAL (!IS ($? y) !STACK))
        (THGOAL (!CHOOSE ($? x) ($_ z) '(((THGOAL (!PART ($? *) ($? y)))))) (THUSE TC-CHOOSE))
        (or (not (term? ($? z))) (THSETQ ($_ z) (list ($? z))))
    =>  (THCOND ((nil? ($? z)) (THSUCCEED))
            ((THGOAL (!PART ($E (car ($? z))) ($? y)))
                (or (THSETQ ($_ z) (cdr ($? z))) true)
                (THGO =>))
            ((THFAIL)))))

(putprop! 'TC-PHYSOB 'THEOREM
    '(THCONSE (x)
        (!PHYSOB ($? x))
        (THOR (THGOAL (!MANIP ($? x))) (THAMONG ($? x) '(ßBOX ßTABLE ßHAND)))))

(putprop! 'TC-PICKUP 'THEOREM
    '(THCONSE (x (WHY (EV)) EV)
        (!PICKUP ($? x))
        (MEMORY)
        (THGOAL (!GRASP ($? x)) (THUSE TC-GRASP))
        (THGOAL (!RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
        (MEMOREND (!PICKUP ($? EV) ($? x)))))

(putprop! 'TC-REFERS 'THEOREM
    '(THCONSE (x)
        (!REFERS ($? x))
        (eval (list 'THSETQ (list 'THV ($? x)) (list 'quote (atomify (getprop ($? x) 'BIND)))))))

(putprop! 'TC-PUT 'THEOREM
    '(THCONSE (x y z)
        (!PUT ($? x) ($? y))
        (THCOND ((THASVAL ($? y))
                (THCOND ((term? ($? y)) (THGOAL (!CHOOSEPLACE ($? y)) (THUSE TC-CHOOSEPLACE)))
                    ((THSUCCEED))))
            ((THGOAL (!GET-RID-OF ($? x)) (THNODB) (THUSE TC-GET-RID-OF))
                (THSUCCEED THEOREM)))
        (CLEAR ($? y) (size ($? x)) ($? x))
        (support ($? y) (size ($? x)) ($? x))
        (THGOAL (!GRASP ($? x)) (THUSE TC-GRASP))
        (THSETQ ($_ z) (tcent ($? y) (size ($? x))))
        (THGOAL (!MOVEHAND ($? z)) (THNODB) (THUSE TC-MOVEHAND))
        (THGOAL (!UNGRASP) (THNODB) (THUSE TC-UNGRASP))))

(putprop! 'TC-PUTIN 'THEOREM
    '(THCONSE (x y z (WHY (EV)) EV)
        (!PUTIN ($? x) ($? y))
        (MEMORY)
        (THCOND ((THGOAL (!PUTON ($? x) ($? y)) (THUSE TC-PUTON))
                (MEMOREND (!PUTIN ($? EV) ($? x) ($? y)))
                (THSUCCEED THEOREM))
            ((THSUCCEED)))
        (THGOAL (!IS ($? y) !BOX))
        (THVSETQ ($_ z)
            (UNION (listify ($? x))
                (THVAL '(THFIND ALL ($? w) (w) (THGOAL (!ON ($? w) ($? y)))) *thalist*)))
        (THGOAL (!CLEARTOP ($? y)) (THUSE TC-CLEARTOP))
        (THGOAL (!PACK ($? z) ($? y)) (THUSE TC-PACK))
        (MEMOREND (!PUTIN ($? EV) ($? x) ($? y)))))

(putprop! 'TC-PUTON 'THEOREM
    '(THCONSE (x y z (WHY (EV)) EV)
        (!PUTON ($? x) ($? y))
        (term? ($? y))
        (or (cdr ($? x)) (THSETQ ($_ x) (car ($? x))))
        (not (COND ((term? ($? x)) (= ($? x) ($? y))) ((memq ($? y) ($? x)))))
        (MEMORY)
        (THCOND ((term? ($? x))
                (THGOAL (!CLEARTOP ($? x)) (THUSE TC-CLEARTOP))
                (THOR (THGOAL (!FINDSPACE ($? y) ($E (size ($? x))) ($? x) ($_ z)) (THUSE TC-FINDSPACE))
                    (and (nil? (getprop '!NOCLEAR 'THASSERTION))
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
        (MEMOREND (!PUTON ($? EV) ($? x) ($? y)))))

(putprop! 'TC-RAISEHAND 'THEOREM
    '(THCONSE ((WHY (EV)) EV)
        (!RAISEHAND)
        (MEMORY)
        (THGOAL (!MOVEHAND ($E (list (car HANDAT) (cadr HANDAT) 512))) (THNODB) (THUSE TC-MOVEHAND))
        (MEMOREND (!RAISEHAND ($? EV)))))

(putprop! 'TC-STACK 'THEOREM
    '(THCONSE (x y)
        (!IS ($? x) !STACK)
        (not (THASVAL ($? x)))
        (THGOAL (!MANIP ($? y)))
        (THGOAL (!SUPPORT ($? y) ?))
        (THNOT (THAND (THGOAL (!PART ($? y) ($_ x)))
            (THGOAL (!IS ($? x) !STACK))))
    =>  (THGOAL (!SUPPORT ($_ x) ($? y)))
        (THCOND ((memq ($? x) '(ßTABLE ßBOX)))
            ((THSETQ ($_ y) ($? x)) (THGO =>)))
        (THSETQ ($_ x) (gensym 'STACK))
        (THASSERT (!IS ($? x) !STACK))
        (THASSERT (!EXISTS ($? x)))
        (THFIND ALL
            ($? z)
            (z)
            (THGOAL (!ON ($? z) ($? y)) (THUSE TC-ON))
            (THAND (THASSERT (!PART ($? z) ($? x))) (THFINALIZE THAND)))))

(putprop! 'TC-STACKUP 'THEOREM
    '(THCONSE (x y BLOCKS PYR (WHY (EV)) EV)
        (!STACKUP ($? x))
        (or (< (reduce + (map #'(lambda [x] (caddr (size x))) ($? x))) 641)
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
                        *thalist*)))))
        (COND ((THVSETQ ($_ PYR) (PACKO ($? x) '!PYRAMID)) (nil? (cdr ($? PYR)))) (:else true))
        (THVSETQ ($_ BLOCKS) (cons 'ßTABLE (PACKO ($? x) '!BLOCK)))
        (MEMORY)
    =>  (THCOND
            ((cdr ($? BLOCKS))
                (THGOAL (!PUTON ($E (cadr ($? BLOCKS))) ($E (car ($? BLOCKS)))) (THUSE TC-PUTON))
                (THSETQ ($_ BLOCKS) (cdr ($? BLOCKS)))
                (THGO =>))
            (($? PYR) (THGOAL (!PUTON ($E (car ($? PYR))) ($E (car ($? BLOCKS)))) (THUSE TC-PUTON)))
            ((MEMOREND (!STACKUP ($? EV) ($? x)))))))

(putprop! 'TC-STARTEND3 'THEOREM
    '(THCONSE (x y EV TIME) (($? x) ($? EV) ($? TIME)) (THGOAL (($? x) ($? y) ($? EV) ($? TIME)) (THUSE TC-STARTEND4))))

(putprop! 'TC-STARTEND4 'THEOREM
    '(THCONSE (x NEWEV z EV TIME)
        (($? x) ($? NEWEV) ($? EV) ($? TIME))
        (or (and (THASVAL ($? x)) (THASVAL ($? EV)) (THASVAL ($? TIME)) (not (THASVAL ($? NEWEV)))) (ert "TC-STARTEND4"))
        (THGOAL (!CHOOSE ($? EV) ($_ z) nil) (THUSE TC-CHOOSE))
        (or (term? ($? z)) (ert "TC-STARTEND4 ATOM"))
        (THSETQ ($_ NEWEV) (gensym 'EV))
        (putprop! ($? NEWEV) 'END
            (putprop! ($? NEWEV) 'START
                (getprop ($? z) (COND ((= ($? x) '!START) 'START) ((= ($? x) '!END) 'END) ((ert "TC-STARTEND (THV x)"))))))
        (timechk ($? NEWEV) ($? TIME))
        (putprop! ($? NEWEV) 'WHY ($? z))
        (putprop! ($? NEWEV) 'TYPE '!START)))

(putprop! 'TC-UNGRASP 'THEOREM
    '(THCONSE (x OBJ (WHO (EV)) EV)
        (!UNGRASP)
        (THCOND ((THGOAL (!GRASPING ($? x)))
                (MEMORY)
                (THGOAL (!SUPPORT ? ($? x)))
                (THERASE (!GRASPING ($? x)))
                (MEMOREND (!UNGRASP ($? EV) ($? x)))
                (THSETQ *thtime* (inc *thtime*))
                (THCOND (doit? (THOR (UNGRASP) (and (GRASP ($? x)) nil)))
                    ((THSETQ *plan* (cons '(UNGRASP) *plan*)))))
            ((THSUCCEED)))))

(putprop! 'TC-WANT4 'THEOREM
    '(THCONSE (x EV TIME y)
        (!WANT ($? x) ($? EV) ($? TIME))
        (THGOAL (!WANT ($? y) ($? x) ($? EV) ($? TIME)) (THUSE TC-WANT5))))

(putprop! 'TC-WANT5 'THEOREM
    '(THCONSE (x NEWEV EV TIME z)
        (!WANT ($? NEWEV) ($? x) ($? EV) ($? TIME))
        (or (and (THASVAL ($? x)) (THASVAL ($? EV)) (THASVAL ($? TIME))) (ert "TC-WANT5 THASVAL"))
        (= ($? x) 'ßFRIEND)
        (= (getprop ($? EV) 'WHY) 'COMMAND)
        (THSETQ ($_ NEWEV) (gensym 'EV))
        (putprop! ($? NEWEV) 'END
            (putprop! ($? NEWEV) 'START
                (getprop ($? EV) 'START)))
        (timechk ($? NEWEV) ($? TIME))
        (putprop! ($? NEWEV) 'TYPE '!TELL)
        (putprop! ($? NEWEV) 'WHY 'ESP)))

(putprop! 'TCT-EXISTS 'THEOREM
    '(THCONSE nil (!EXISTS ? ?) (THSUCCEED)))

(putprop! 'TCT-PICKUP 'THEOREM
    '(THCONSE (x EV TIME)
        (!PICKUP ($? x) ($? TIME))
        (THOR (THAND (THGOAL (!PICKUP($? EV) ($? x))) (timechk ($? EV) ($? TIME)))
            (THGOAL (!PICKUP ($? EV) ($? x) ($? TIME)) (THUSE TCTE-PICKUP)))))

(putprop! 'TCT-PUT 'THEOREM
    '(THCONSE (x EV TIME y)
        (!PUT ($? x) ($? y) ($? TIME))
        (THGOAL (!PUT ($? EV) ($? x) ($? y) ($? TIME)) (THUSE TCTE-PUT))))

(putprop! 'TCT-AT 'THEOREM
    '(THCONSE (x y z TIME w)
        (!AT ($? y) ($? z) ($? TIME))
        (THOR (THGOAL (!MANIP ($? y)))
            (THAND (THGOAL (!IS ($? y) !BOX)) (THGOAL (!AT ($? y) ($? z))) (THSUCCEED THEOREM)))
        (THSETQ ($_ x) (TFIND ($? y) ($? TIME)))
        (THOR (THSETQ ($_ w) (car ($? x)))
            (THAND (THAMONG ($? w) (cdr ($? x))) (or (not (< (car ($? w)) (or (start? ($? TIME)) -1))) (THFAIL THAND))))
        (THSETQ ($? z) (cadr ($? w)))))

(putprop! 'TCT-LOC 'THEOREM
    '(THCONSE (YY ZZ x y z TIME)
        (!LOC ($? x) ($? y) ($? z) ($? TIME))
        (THGOAL (!AT ($? y) ($? YY) ($? TIME)) (THUSE TCT-AT))
        (THGOAL (!AT ($? z) ($? ZZ) ($? TIME)) (THUSE TCT-AT))
        (THGOAL (!TLOC ($? x) ($? y) ($? z)) (THUSE TC-LOC))))

(putprop! 'TCT-SUPPORT 'THEOREM
    '(THCONSE (x y z TIME)
        (!SUPPORT ($? x) ($? y) ($? TIME))
        (THOR (THGOAL (!MANIP ($? y))) (THGOAL (!IS ($? y) !BOX)))
        (THAMONG ($? z) (TFIND ($? y) ($? TIME)))
        (not (< (car ($? z)) (or (start? ($? TIME)) -1)))
        (THAMONG ($? x) (list (caddr ($? z))))))

(putprop! 'TCT-2 'THEOREM
    '(THCONSE (x EV TIME) (($? x) ($? TIME)) (THGOAL (($? x) ($? EV) ($? TIME)) (THUSE TCTE-3))))

(putprop! 'TCT-3 'THEOREM
    '(THCONSE (x y EV TIME) (($? x) ($? y) ($? TIME)) (THGOAL (($? x) ($? EV) ($? y) ($? TIME)) (THUSE TCTE-4))))

(putprop! 'TCT-4 'THEOREM
    '(THCONSE (x y z EV TIME) (($? x) ($? y) ($? z) ($? TIME)) (THGOAL (($? x) ($? EV) ($? y) ($? z) ($? TIME)) (THUSE TCTE-5))))

(putprop! 'TCTE-PICKUP 'THEOREM
    '(THCONSE (x EV EVENT TIME)
        (!PICKUP ($? EV) ($? x) ($? TIME))
        (THOR (THAND (THGOAL (!PICKUP ($? EV) ($? x))) (timechk ($? EV) ($? TIME)) (THSUCCEED THEOREM))
            (THSUCCEED))
        (THAMONG ($? EVENT) EVENTLIST)
        (memq (getprop ($? EVENT) 'TYPE) '(!PUTON !GET-RID-OF))
        (timechk ($? EVENT) ($? TIME))
        (THOR (THGOAL (!PUTON ($? EVENT) ($? x) ?))
            (THGOAL (!GET-RID-OF ($? EVENT) ($? x))))
        (THVSETQ ($_ EV) (gensym 'E))
        (and (putprop! ($? EV) 'END (putprop! ($? EV) 'START (getprop ($? EVENT) 'END)))
            (putprop! ($? EV) 'TYPE '!PICKUP)
            (putprop! ($? EV) 'WHY ($? EVENT))
            (SETQ EVENTLIST (cons ($? EV) EVENTLIST))
            (THASSERT (!PICKUP ($? EV) ($? x))))))

(putprop! 'TCTE-PUT 'THEOREM
    '(THCONSE (x y EV EVENT TIME z)
        (!PUT ($? EV) ($? x) ($? y) ($? TIME))
        (THAMONG ($? EVENT) EVENTLIST)
        (memq (getprop ($? EVENT) 'TYPE) '(!PICKUP !PUTON))
        (timechk ($? EVENT) ($? TIME))
        (THOR (THGOAL (!PUTON ($? EVENT) ($? x) ?))
            (THGOAL (!PICKUP ($? EVENT) ($? x))))
        (or (THVSETQ ($_ z) (dec (assq (getprop ($? EVENT) 'END) (getprop ($? x) 'HISTORY))))
            (ert "TCTE-PUT WRONG"))
        (THAMONG ($? y) (list (cadr ($? z))))
        (THSETQ ($_ EV) (gensym 'E))
        (and (putprop! ($? EV) 'END (putprop! ($? EV) 'START (car ($? z))))
            (putprop! ($? EV) 'WHY ($? EVENT))
            (putprop! ($? EV) 'TYPE '!PUT)
            (SETQ EVENTLIST (cons ($? EV) EVENTLIST))
            (THASSERT (!PUT ($? EV) ($? x) ($? y))))))

(putprop! 'TCTE-3 'THEOREM
    '(THCONSE (x EV TIME)
        (($? x) ($? EV) ($? TIME))
        (or (THASVAL TIME) (ert "TCTE-3"))
        (THGOAL (($? x) ($? EV)))
        (timechk ($? EV) ($? TIME))))

(putprop! 'TCTE-4 'THEOREM
    '(THCONSE (x y EV TIME)
        (($? x) ($? EV) ($? y) ($? TIME))
        (or (THASVAL ($? TIME)) (ert "TCTE-4"))
        (THGOAL (($? x) ($? EV) ($? y)))
        (timechk ($? EV) ($? TIME))))

(putprop! 'TCTE-5 'THEOREM
    '(THCONSE (x y z EV TIME)
        (($? x) ($? EV) ($? y) ($? z) ($? TIME))
        (or (THASVAL ($? TIME)) (ert "TCT-5"))
        (THGOAL (($? x) ($? EV) ($? y) ($? z)))
        (timechk ($? EV) ($? TIME))))

(putprop! 'TCT-GRASP 'THEOREM
    '(THCONSE (x z TIME)
        (!GRASP ($? x) ($? TIME))
        (THVSETQ ($_ z) (ENDTIME GRASPLIST ($? TIME)))
    =>  (THCOND ((or (nil? ($? z)) (startime ($? z) ($? TIME))) (THFAIL))
            ((or (and (not (THASVAL ($? x))) (THSETQ ($_ x) (cadar ($? z)))) (= ($? x) (cadar ($? z)))))
            ((THSETQ ($_ z) (cdr ($? z))) (THGO =>))
            ((THFAIL)))))

(putprop! 'TE-CONTAIN 'THEOREM
    '(THERASING (x y)
        (!AT ($? x) ?)
        (THGOAL (!CONTAIN ($_ y) ($? x)))
        (THERASE (!CONTAIN ($? y) ($? x)))))

(putprop! 'TE-EXISTS 'THEOREM
    '(THERASING (x) (!EXISTS ($? x)) (THSUCCEED)))

(putprop! 'TE-SUPP 'THEOREM
    '(THERASING (x y z)
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
                    ((THSUCCEED)))))))

(defn- topcenter [x] (let [x (atab x)] (tcent (cadr x) (caddr x))))

(putprop! '!CLEARTOP 'CHOOSE
    '(((THGOAL (!SUPPORT ($? *) ?)))))

(putprop! '!GRASP 'CHOOSE
    '(((THNOT (THGOAL (!GRASPING ($? *)))) (THGOAL (!CLEARTOP ($? *))))
     ((THNOT (THGOAL (!GRASPING ($? *)))))))

(putprop! '!PICKUP 'CHOOSE
    '(((THGOAL (!SUPPORT ? ($? *))) (THGOAL (!CLEARTOP ($? *))))
     ((THGOAL (!SUPPORT ? ($? *))))))

(putprop! '!PUTIN 'CHOOSE
    '(((THNOT (THGOAL (!CONTAIN ßBOX ($? *)))) (THGOAL (!CLEARTOP ($? *))))
     ((THNOT (THGOAL (!CONTAIN ßBOX ($? *)))))))

(putprop! '!PUTIN 'CHOOSE2
    '(((THGOAL (!IS ($? *) !BOX)))))

(putprop! '!PUTON 'CHOOSE
    '(((THGOAL (!CLEARTOP ($? *))))))

(putprop! '!PUTON 'CHOOSE2
    '(((THGOAL (!CLEARTOP ($? *))) (THNOT (THGOAL (!IS ($? *) !PYRAMID))))
     ((THNOT (THGOAL (!IS ($? *) !PYRAMID))))))

(putprop! '!STACKUP 'CHOOSE
    '(((THGOAL (!CLEARTOP ($? *))) (THNOT (THGOAL (!IS ($? *) !PYRAMID))))
     ((THNOT (THGOAL (!IS ($? *) !PYRAMID))))))

(§ dorun (map #(THADD % nil)
    '(TC-CALL TC-CLEARTOP TC-GET-RID-OF TC-GRASP TC-NAME TC-NOTICE TC-PACK TC-PICKUP TC-PUTIN TC-PUTON TC-RAISEHAND TC-STACKUP TC-UNGRASP TC-ON TC-PHYSOB)))

#_(ns shrdlu.data)

;; ###################################################################
;;
;;      DATA > INITIAL MICROPLANNER DATABASE FOR THE BLOCKS WORLD
;;
;; ###################################################################

(def- doit? true)

(§ SETQ HANDAT '(32 0 0))

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

(§ dorun (map #(THADD % nil) '(
    (!IS ßB1 !BLOCK)
    (!IS ßB2 !PYRAMID)
    (!IS ßB3 !BLOCK)
    (!IS ßB4 !PYRAMID)
    (!IS ßB5 !PYRAMID)
    (!IS ßB6 !BLOCK)
    (!IS ßB7 !BLOCK)
    (!IS ßB10 !BLOCK)

    (!IS !RED !COLOR)
    (!IS !BLUE !COLOR)
    (!IS !GREEN !COLOR)
    (!IS !WHITE !COLOR)
    (!IS !BLACK !COLOR)

    (!IS !RECTANGULAR !SHAPE)
    (!IS !ROUND !SHAPE)
    (!IS !POINTED !SHAPE)

    (!IS ßSHRDLU !ROBOT)
    (!IS ßFRIEND !PERSON)
    (!IS ßHAND !HAND)

    (!AT ßB1 (64 64 0))
    (!AT ßB2 (64 64 64))
    (!AT ßB3 (256 0 0))
    (!AT ßB4 (416 416 1))
    (!AT ßB5 (320 64 128))
    (!AT ßB6 (0 192 0))
    (!AT ßB7 (0 160 192))
    (!AT ßB10 (192 416 0))

    (!SUPPORT ßB1 ßB2)
    (!SUPPORT ßB3 ßB5)
    (!SUPPORT ßB6 ßB7)

    (!CLEARTOP ßB2)
    (!CLEARTOP ßB4)
    (!CLEARTOP ßB5)
    (!CLEARTOP ßB7)
    (!CLEARTOP ßB10)

    (!MANIP ßB1)
    (!MANIP ßB2)
    (!MANIP ßB3)
    (!MANIP ßB4)
    (!MANIP ßB5)
    (!MANIP ßB6)
    (!MANIP ßB7)
    (!MANIP ßB10)

    (!SUPPORT ßTABLE ßB1)
    (!SUPPORT ßTABLE ßB3)
    (!SUPPORT ßBOX ßB4)
    (!SUPPORT ßTABLE ßB10)
    (!SUPPORT ßTABLE ßB6)
    (!SUPPORT ßTABLE ßBOX)

    (!AT ßBOX (384 384 0))
    (!IS ßBOX !BOX)
    (!IS ßTABLE !TABLE)
    (!CONTAIN ßBOX ßB4)

    (!SHAPE ßB1 !RECTANGULAR)
    (!SHAPE ßB3 !RECTANGULAR)
    (!SHAPE ßB2 !POINTED)
    (!SHAPE ßB4 !POINTED)
    (!SHAPE ßB5 !POINTED)
    (!SHAPE ßB6 !RECTANGULAR)
    (!SHAPE ßB7 !RECTANGULAR)
    (!SHAPE ßB10 !RECTANGULAR)

    (!COLOR ßB1 !RED)
    (!COLOR ßB2 !GREEN)
    (!COLOR ßB3 !GREEN)
    (!COLOR ßB4 !BLUE)
    (!COLOR ßB5 !RED)
    (!COLOR ßB6 !RED)
    (!COLOR ßB7 !GREEN)
    (!COLOR ßB10 !BLUE)
    (!COLOR ßBOX !WHITE)
    (!COLOR ßTABLE !BLACK)

    (!CALL ßSHRDLU SHRDLU)
    (!CALL ßFRIEND YOU)
)))

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

(§ defn- EV [] (or nomem? ($? EV)))

(§ defn- FINDSPACE [type surf size obj]
    (let [xymax nil xymin nil n nil v nil x1 nil x2 nil]
        (SETQ obj (listify obj))
        (and (memq surf obj) (RETURN nil))
        (COND ((= surf 'ßTABLE)
                (SETQ xymin '(0 0))
                (SETQ xymax '(640 640))
                (set! *level* 0)
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
                (set! *level* 1))
            ((SETQ x1 (half (car size)))
                (SETQ y1 (half (cadr size)))
                (SETQ xymax (list (min 640 (dec (+ (caaddr X) (caadr X) x1))) (min 640 (dec (+ (cadr (caddr X)) (cadadr X) y1)))))
                (SETQ xymin (list (max 0 (- (caadr X) x1)) (max 0 (- (cadadr X) y1))))
                (set! *level* (+ (caddr (cadr X)) (caddr (caddr X))))))
    ON  (SETQ n 8)
        (SETQ x1 (- (car xymax) (car xymin)))
        (SETQ y1 (- (cadr xymax) (cadr xymin)))
    GO  (COND ((zero? (SETQ n (dec n))) (RETURN nil))
            ((or (not (SETQ v
                        (GROW (list
                                (+ (car xymin) (rem (abs (RANDOM)) x1))
                                (+ (cadr xymin) (rem (abs (RANDOM)) y1))
                                *level*)
                            xymin xymax obj)))
                    (< (- (caadr v) (caar v)) (car size))
                    (< (- (cadadr v) (cadar v)) (cadr size)))
                (GO GO))
            ((RETURN (COND
                ((= type 'RANDOM)
                    (list (half (- (+ (caar v) (caadr v)) (car size)))
                        (half (- (+ (cadar v) (cadadr v)) (cadr size)))
                        *level*))
                ((= type 'PACK)
                    (list (caar v) (cadar v) *level*))
                ((ert "FINDSPACE -- type"))))))))

(§ defq- GOAL [x]
    (binding [*plan* nil]
        (THVAL (list 'THGOAL (car x) '(THTBF thtrue)) '((EV COMMAND)))
        (evlis (reverse *plan*))))

(§ defn- GROW [loc _min _max obj]
    (let [grow nil XL nil XH nil XO nil YL nil YH nil YO nil]
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
    =>  (SETQ grow (min (caar XL) (caar XH) (caar YL) (caar YH)))
        (COND ((== grow 1024)
            (RETURN (list
                (list (- (car loc) (cadar XL)) (- (cadr loc) (cadar YL)))
                (list (+ (car loc) (cadar XH)) (+ (cadr loc) (cadar YH))))))
            (:else (dorun (map #'(lambda [y z w] (let [x (eval w)]
                        (COND ((> (caar x) grow))
                            ((or (nil? (cadar x)) (memq (cadar x) (eval y)))
                                (RPLACA x (list 2000 (caar x))))
                            ((SET z (cons (cadar x) (eval z)))
                                (SET w (cdr x))))
                        nil))
                    '(YO YO XO XO)
                    '(XO XO YO YO)
                    '(XL XH YL YH)))
                (GO =>)))))

(§ defn- LOCGREATER [loc x y fun]
    ((lambda [XX YY] (not (< (fun (cadr XX)) (+ (fun (cadr YY)) (fun (caddr YY))))))
        (locg2 loc '($? YY) x)
        (locg2 loc '($? ZZ) y)))

(defn- locg2 [loc x y]
    (if (= ($? loc) '!LOC) (atab y) (cons nil (cons (eval x) (cddr (atab y))))))

(§ defq- MEMOREND [a]
    (or nomem?
        (and (putprop! ($? EV) 'END *thtime*)
            (APPLY #'THASSERT (list (THVARSUBST (car a) nil)))
            (putprop! ($? EV) 'TYPE (caar a)))))

(§ defn- MEMORY []
    (or nomem?
        (THAND (THVSETQ ($_ EV) (gensym 'E))
            (THSETQ EVENTLIST (cons ($? EV) EVENTLIST))
            (putprop! ($? EV) 'START *thtime*)
            (putprop! ($? EV) 'WHY ($? WHY)))))

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
        (doall (map cadr XX))))

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
    (SETQ GRASPLIST nil)
    (putprop! 'EE 'WHY 'COMMAND)
    (putprop! 'EE 'START 0)
    (putprop! 'EE 'END 0)
    (putprop! 'EE 'TYPE '!START)
    (SETQ EVENTLIST '(EE))
    (THADD '(!START EE ßDIALOG) nil)
    (dorun (map #'(lambda [x]
        (and (getprop (car x) 'THASSERTION)
            (putprop! (car x) 'HISTORY
                (list (list 0 (cadr x) (cadar (THVAL '(THGOAL (!SUPPORT ($? x) ($? Y))) (list (list 'X 'THUNASSIGNED) (list 'Y (car x))))))))))
        ATABLE)))

(defn- startime [l time]
    (< (caar l) (or (start? time) -1)))

(defn- support [loc size x]
    (if (zero? (caddr loc)) 'ßTABLE
        (let [loc (OCCUPIER (+ (car loc) (half (car size))) (+ (cadr loc) (half (cadr size))) (dec (caddr loc)))]
            (when (and loc (not= loc x)) loc))))

(defn- tcent [x1 x2]
    (list (+ (car x1) (half (car x2))) (+ (cadr x1) (half (cadr x2))) (+ (caddr x1) (caddr x2))))

(§ defn- TFIND [x y]
    (let [z (getprop x 'HISTORY)]
        (or z (RETURN nil))
    =>  (COND ((not (> (caar z) (or (end? y) 32767)))
                (RETURN z))
            ((SETQ z (cdr z)) (GO =>)))))

(defn- timechk [ev time]
    (if (imperf? time)
        (not (or (< (getprop ev 'END) (or (start? time) -1)) (< (or (end? time) 262143) (getprop ev 'START))))
        (not (or (< (getprop ev 'START) (or (start? time) -1)) (< (or (end? time) 262143) (getprop ev 'END))))))

#_(ns shrdlu.syscom)

(defn- %sent []
    ;; THIS FUNCTION PRINTS THE CURRENT SENTENCE
    (terpri)
    (dorun (map print- *sent*))
    (print *punct*))

(§ defn- DA [x]
    (and (getprop x 'THASSERTION) (DISP (APPLY #'concat (doall (map cddr (APPLY #'concat (doall (map cdr (cdr (getprop x 'THASSERTION)))))))))))

(def- LINEL 65)

(§ defq- DISP [a]
    (print (ascii 12))
    (terpri)
    (when (cdr a)
        (print (car a) ">>" (cadr a))
        (terpri))
    (SPRINT (if (cdr a) (getprop (car a) (cadr a)) (eval (car a))) LINEL 0)
    '*)

(§ defn- DP [x]
    (terpri)
    (terpri)
    (print \[)
    (print x)
    (print \])
    (let [l (PLIST x)]
    A   (when (= (car l) 'VALUE) (GO B))
        (terpri)
        (TAB 4)
        (print (car l))
        (SPRINT (cadr l) (- LINEL 18) 18)
    B   (SETQ l (cddr l))
        (when l (GO A))
        (terpri)
        '*))

;; ################################################################
;;         FUNCTIONS FOR HAND-TAILORED GARBAGE COLLECTION
;; ################################################################

(§ defn- FORGET []
    (set! *lastsentno* 0)
    (set! *lastsent* nil)
    (set! *lastrel* nil)
    (set! *backref* nil)
    (set! *backref2* nil)
    (dorun (map #'(lambda [pn]
        (dorun (map #'(lambda [prop] (remprop! pn prop))
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

;; ################################################################
;;         A MOST COMPLETE AND SOPHISTICATED BREAK PACKAGE
;; ################################################################

(defn- ert [message] (ERTEX message nil true))       ;; ALWAYS STOPS, NEVER CAUSES ABORTION.  USED FOR GUARENTEED STOPS AS IN DEBUGGING OR ETAOIN.

(defn- bug [message] (ERTEX message true nil))        ;; MARKES UNANTICIPATED WEIRD STATES WHICH INDICATE MISTAKES IN THE CODE.

(defn- global-err [message]
    (set! *global-message* message)
    (ERTEX message true nil))        ;; MARKES KNOWN INADEQUACIES OF THE SYSTEM.  SWITCHABLE STOP, CAUSES ABORTION.

(def- nostop? true)

(§ defn- ERTEX [message cause-abortion ignore-nostop?]
    (let [GLOP nil EXP nil ST-BUFFER nil BUILDING-ST-FORM nil FIRSTWORD nil]
        (and nostop?
            (not ignore-nostop?)
            (and cause-abortion
                (*THROW 'ABORT-PARSER cause-abortion))
            (RETURN true))
        (terpri)
        (dorun (map print- message))
    PRINT
        (SETQ FIRSTWORD true ST-BUFFER nil BUILDING-ST-FORM nil)   ;; "ST" REFERS TO SHOW, TELL
        (terpri)
        (print ">>> ")
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
            (SETQ GLOP (or (getprop GLOP 'ABBREV) GLOP))
            (COND ((memq GLOP '(true P nil))                     ;; LEAVE-LOOP CHARS
                    (RETURN GLOP))
                ((= GLOP 'GO)                                  ;; CAUSE RETURN TO READY-STATE
                    (*THROW 'ABORT-PARSER 'GO))
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
        (ERRSET (let [_ (eval EXP)] (terpri) (pr _)))
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
            (SETQ COMBINE (concat COMBINE (cons '- (EXPLODE (eval x))))))
            words))
        (SETQ COMBINE (list (INTERN (MAKNAM (cdr COMBINE)))))
        (and (ISQ COMBINE COMBINATION) (RETURN COMBINE))
        nil))

(defn- findb [x y]
    (cond (nil? x) nil (= (cdr x) y) x :else (recur (cdr x) y)))

(defn- from [a b]
    (when (and a (not= a b)) (cons (word a) (from (cdr a) b))))

(§ defn- MEET [a b]
    ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
    (let [s nil]
    =>  (COND ((nil? a) (RETURN (reverse s)))
            ((memq (car a) b)
                (SETQ s (cons (car a) s))))
        (SETQ a (cdr a))
        (GO =>)))

(§ defn- MOD [a b] (UNION (SETDIF a (cadr b)) (car b)))

(§ defn- PR1 [a]
    (COND ((term? (H a)) (list (word (NB a)) (features a)))
        ((PR2 (SM a))
            (list (from (NB a) (N a)) (features a) (SM a) (COND ((term? (H a)) \space) ((MAPLIST #'PR1 (reverse (H a)))))))))

(§ defn- PR2 [a]
    (or (term? a)
        (dorun (map #'(lambda [b]
            (and (getprop b 'SM)
                (or (memq b ALIST)
                    (SETQ ALIST (cons (list b (getprop b 'SM) (getprop b 'REFER)) ALIST)))))
            a))))

(§ defq- PRINTC [l]
    (terpri)
=>  (COND ((nil? l) (RETURN nil)))
    (let [test (eval (car l))]
        (COND ((= test '<TAB>))
            (:else (print test) (print \space))))
    (SETQ l (cdr l))
    (GO =>))

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
        (print (ascii 12))
        (terpri)
        (SPRINT (list (PR1 a) (reverse ALIST)) LINEL 0)
        nil))

(def- CHRCT 76)

(§ defn- TAB [n]
    (let [P nil]
        (COND ((> n LINEL) (RETURN '<TAB>)))
    =>  (SETQ P (- LINEL CHRCT))
        (COND ((not (> n P)) (RETURN '<TAB>)))
        (print \space)
        (GO =>)))

#_(ns shrdlu.morpho)

;; ################################################################################
;;
;;                    MORPHO - CODE FOR MORPHOLOGICAL ANALYSIS
;;
;;                INCLUDES ETAOIN, THE INPUT HANDLER FOR THE SYSTEM
;;
;; ################################################################################

(def- ALTMODE (ascii 27))

(def- FINAL (seq ".?!"))
(def- PUNCL (seq ".?!:;\""))

(def- VOWEL (cons nil "AEIOUY"))
(def- CONSO (seq "BCDFGHJKLMNPQRSTVWXZ"))
(def- LRSZV (seq "LRSZV"))
(def- CGSJVZ (seq "CGSJVZ"))

(defn- uppercase-ify-char [c] (if (< 96 c 123) (- c 32) c))

(§ defn- ETAOIN []
    (let [WORD nil NEWWORD nil c nil ALTN nil ALREADY-BLGING-NEWWRD nil WRD nil LAST nil NEXT nil Y nil WORD1 nil x nil RD nil POSS nil]
    THRU (set! *sent* (SETQ WORD (set! *punct* (SETQ POSS nil))))
        (terpri)
        (print "READY")
        (terpri)
    CHAR (COND ((== (TYIPEEK) 24) (READCH) (ert nil) (GO THRU)) ;; "CTRL-X" BREAK LEFT OVER FROM CMU
            ((== (TYIPEEK) 3) (bug "ETAOIN: ABOUT TO READ EOF")))
        (SETQ c (ascii (uppercase-ify-char (TYI))))
        (COND ((= c \space) (GO WORD))                     ;; DELIMITER
            ((= c ALTMODE)
                (SETQ c (ascii (uppercase-ify-char (TYI))))
                (COND ((= c ALTMODE) (ert nil) (GO THRU)) ;; ALTMODE-ALTMODE
                    ((= c 'C) (print (ascii 12)) (GO DO))         ;; ALTMODE-C
                    ((= c 'R) (terpri) (GO DO))         ;; ALTMODE-R
                    ((and (= c 'S) *savesent*)            ;; ALTMODE-S CAUSES THE LAST SENTENCE TYPED IN TO
                        (set! *sent* (car *savesent*))          ;; RETURNED AS THE SENTENCE TO BE INTERPRETED
                        (set! *punct* (cdr *savesent*))
                        (%sent)
                        (RETURN *sent*))
                    ((= c 'N)                           ;; ALTMODE-N COMPLEMENTS THE NEWWORD FLAG, WHICH
                                                            ;; DETERMINES WHETHER UNRECOGNIZED WORDS WILL BE
                                                            ;; CONSIDERED SPELLING ERRORS OR NEW WORDS.
                        (SETQ NEWWORD (not NEWWORD) ALTN (not ALTN))
                        (GO CHAR))
                    ((GO THRU))))
            ((= c (ascii 127))
                (COND (WORD (print (car WORD)) (SETQ WORD (cdr WORD)))
                    (*sent* (print (car *sent*)) (set! *sent* (cdr *sent*))))
                (GO CHAR))
            ((= c (ascii 13)) (GO WORD))
            ((memq c PUNCL)
                (set! *punct* c)                           ;; DELIMITER
                (and WORD (GO WORD))
                (GO PUNC)))
        (and
            (or (and (= c '\") (not ALREADY-BLGING-NEWRD) (SETQ NEWWORD (SETQ ALREADY-BLGING-NEWRD true)) (GO CHAR)) ;; "sic!
                (and (= c '\") ALREADY-BLGING-NEWRD (not (SETQ ALREADY-BLGING-NEWRD nil)) (GO WORD)) ;; "sic!
                                                            ;; WITHIN THIS "AND" ARE ALL THE CHARACTERS THAT ARE UNDERSTOOD BY THE SYSTEM
                (number? c)
                (and (= c '=) (nil? WORD))
                (memq c VOWEL)
                (memq c CONSO))
            (SETQ WORD (cons c WORD)))
        (GO CHAR)

    DO  (terpri)
        (print "READY")
        (terpri)
        (dorun (map -print (reverse *sent*)))
        (print \space)
        (dorun (map print (reverse WORD)))
        (GO CHAR)

    WORD (COND ((nil? WORD) (GO CHAR))
            ((= WORD '(P L E H)) (HELP) (GO THRU))
            ((and (SETQ WRD (ERRSET (READLIST (reverse WORD)))) (number? (SETQ WRD (car WRD))))
                (set! *sent* (cons WRD *sent*))
                (buildword WRD (or (and (zero? (dec WRD)) '(NUM NS)) '(NUM)) (list 'NUM WRD) nil))
                                                            ;; NO ROOT FOR NUMBERS
            ((nil? WRD) (SETQ WRD (reverse WORD)) (GO NO))
            ((getprop WRD 'FEATURES))                           ;; IF A WORD HAS FEATURES, IT'S PROPERTIES ARE ALL SET UP IN THE DICTIONARY
            ((SETQ x (getprop WRD 'IRREGULAR))
                (buildword WRD (MOD (getprop (car x) 'FEATURES) (cdr x)) (SM x) (car x)))
            ((= (car (LAST WORD)) '=)
                (buildword WRD (COND ((memq '\" WORD) '(PROPN NS POSS)) ('(PROPN NS))) '((PROPN T)) nil)) ;; "sic!
            ((GO CUT)))
        (GO WRD)

        ;; --------------------------------------------
        ;;               MORPHOLOGY CODE
        ;; --------------------------------------------

    CUT (COND
            ((STA WORD '(T \' N)) (SETQ RD (cdddr WORD)) (SETQ WORD (cons '* WORD)) (GO TRY))
            ((STA WORD '(S \')) (SETQ WORD (cddr WORD)) (SETQ POSS WRD) (GO WORD))
            ((STA WORD '(\')) (SETQ WORD (cdr WORD)) (SETQ POSS WRD) (GO WORD))
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
        (COND ((and (memq LAST CONSO) (not (memq LAST LRSZV)) (= LAST NEXT)) (SETQ RD (cdr RD)))
            ((= LAST 'I) (SETQ RD (cons 'Y (cdr RD))))
            ((or
                (and (memq LAST CONSO) (memq NEXT VOWEL) (not (= NEXT 'E)) (memq (caddr RD) CONSO))
                (and (memq LAST LRSZV) (memq NEXT CONSO) (not (memq NEXT LRSZV)))
                (and (= LAST 'H) (= NEXT 'T))
                (and (memq LAST CGSJVZ) (or (memq NEXT LRSZV) (and (memq NEXT VOWEL) (memq (caddr RD) VOWEL)))))
                    (SETQ RD (cons 'E RD))))
        (GO TRY)

    LY  (COND ((and (memq (car RD) VOWEL) (not (= (car RD) 'E)) (memq (cadr RD) CONSO))
            (SETQ RD (cons 'E RD))))
        (COND ((memq 'ADJ (getprop (SETQ ROOT (READLIST (reverse RD))) 'FEATURES))
            (buildword WRD '(ADV VBAD) nil ROOT) ;; TEMP NIL SEMANTICS ;; ROOT IS THE ADJECTIVE
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
            ((or (SETQ FEATURES (getprop (SETQ ROOT (READLIST (reverse RD))) 'FEATURES))
                    (and (SETQ x (getprop ROOT 'IRREGULAR))
                        (SETQ FEATURES (MOD (getprop (SETQ ROOT (car x)) 'FEATURES) (cdr x)))))
                (buildword WRD (MOD FEATURES (getprop (car WORD) 'MOD)) (getprop ROOT 'SEMANTICS) ROOT))
            ((= (car RD) 'E) (SETQ RD (cdr RD)) (GO TRY))
            ((GO NO)))

        ;; -------------------------------------------------------
        ;;   BUILD UP THE PROCESSED LIST OF WORDS TO BE RETURNED
        ;; -------------------------------------------------------

    WRD (set! *sent*
            (COND (POSS
                (COND ((or (memq 'NOUN (SETQ FEATURES (getprop WRD 'FEATURES))) (memq 'PROPN FEATURES))
                        ;; IF IT'S A NOUN OR A PROPER NOUN, MARK IT AS POSSESSIVE
                        (buildword POSS (concat (MEET FEATURES (getprop 'POSS 'ELIM)) '(POSS)) (getprop WRD 'SEMANTICS) ROOT)
                        (cons POSS *sent*))
                    ;; CAN WE GENERALIZE IT???
                    ((buildword "'S" '(VB BE V3PS PRES) (getprop 'BE 'SEMANTICS) 'BE) (cons "'S" (cons WRD *sent*)))))
                ((cons WRD *sent*))))

    PUNC (COND (*punct*
            (COND ((and (= *punct* '?) (nil? *sent*)) (HELP) (GO THRU))
                ((memq *punct* FINAL)
                    (RETURN (car (set! *savesent* (cons (reverse *sent*) *punct*))))) ;; RETURN POINT !!!!!!!!!!!!!
                ((set! *sent* (cons *punct* *sent*))))))

        (set! *punct* nil)
        (SETQ WORD (SETQ POSS nil))
        (GO CHAR)

    NO  (COND (NEWWORD
            (buildword WRD '(NOUN NS) '((NOUN (SMNEWNOUN)) (PROPN (SMNEWPROPN))) WRD)
            (or ALTN (SETQ NEWWORD nil))
            (GO PUNC)))
        (terpri)
        (print (str "SORRY, I DON'T KNOW THE WORD \"" WRD "\"."))
        (terpri)
        (print "PLEASE TYPE <LF> AND CONTINUE THE SENTENCE.")

    NOGO (or (== (TYI) 10) (GO NOGO))
        (set! *punct* nil)
        (SETQ WORD nil)
        (GO DO)))

(defn- propname [x] (= (car (name x)) \=))

(defn- buildword [word features semantics root]
    (putprop! word 'FEATURES features)
    (putprop! word 'SEMANTICS semantics)
    (when root
        (putprop! word 'ROOT root))
    word)

(defn- undefined [n] (print (word n)) (ert "UNDEFINED"))

#_(ns shrdlu.show)

(§ SETQ LABELTRACE nil)
(§ SETQ LABELBREAK nil)

(§ defq- LABELTRACE [a]
    (dorun (map #'(lambda [x]
        (let [BODY nil]
            (terpri)
            (pr x)
            (COND ((getprop x 'LABELTRACED) (-print "ALREADY LABELTRACED") (RETURN nil))
                ((getprop x 'INTERPRET) (SETQ BODY (cdr (getprop x 'INTERPRET))))
                ((getprop x 'EXPR) (SETQ BODY (cddr (caddr (getprop x 'EXPR)))))
                (:else (-print "CAN'T BE LABELTRACED") (RETURN nil)))
            (MAP #'(lambda [y] (when (term? (car y)) (RPLACD y (cons (list 'passing (list 'quote (car y))) (cdr y))))) BODY)
            (putprop! x 'LABELTRACED true)
            (-print "LABELTRACED")
            nil))
        a)))

(defn- passing [x]
    (and (cond
            (term? LABELTRACE) (when LABELTRACE (terpri) (print "PASSING" x) true)
            (memq x LABELTRACE) (do (terpri) (print "PASSING" x) true))
        (cond
            (term? LABELBREAK) (when LABELBREAK (ert "LABELBREAK"))
            (memq x LABELBREAK) (ert "LABELBREAK"))))

(§ defq- UNLABELTRACE [a]
    (dorun (map #'(lambda [x]
        (let [BODY nil]
            (terpri)
            (pr x)
            (COND ((not (getprop x 'LABELTRACED)) (-print "ISN'T ALREADY LABELTRACED") (RETURN nil))
                ((getprop x 'INTERPRET) (SETQ BODY (cdr (getprop x 'INTERPRET))))
                ((getprop x 'EXPR) (SETQ BODY (cddr (caddr (getprop x 'EXPR)))))
                (:else (-print "CAN'T BE LABELTRACED") (RETURN nil)))
            (MAP #'(lambda [y] (and (term? (car y)) (RPLACD y (cddr y)))) BODY)
            (putprop! x 'LABELTRACED nil)
            (-print "UNLABELTRACED")
            nil))
        a)))

(putprop! 'TELLABLE 'TELL '(lambda [x]
    (APPLY #'TELLABLE
        (list (charg x "CONCEPT:" "ANY PLANNER GOAL PATTERN BEGINNING WITH THIS CONCEPT NAME CAN BE ACCEPTED BY THE SYSTEM AS NEW INFORMATION -- BEWARE OF INTERACTIONS WITH SPECIAL HACKS FOR LOCATION, ETC.")))))

(defn- pev [ev col top]
    (terpri)
    (TAB col)
    (print ev (getprop ev 'TYPE) "TIME:" (getprop ev 'START) "TO" (getprop ev 'END))
    (when top
        (_print "REASON:" (getprop ev 'WHY)))
    (dorun (map #'(lambda [x] (and (= ev (getprop x 'WHY)) (pev x (+ col 8) nil)))
        (reverse EVENTLIST))))

(putprop! 'EVENT 'SHOW '(lambda [x]
    (let [x (charg x "EVENT:" "EVENT TO BE DISPLAYED -- <LF> FOR ENTIRE EVENT LIST")]
        (if x (pev x 0 true) (dorun (map #(when (= (getprop % 'WHY) 'COMMAND) (pev % 0 true)) (reverse EVENTLIST)))))))

(§ defn- TELLCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE))

(§ defn- SHOWCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE))

(§ defn- SHOWTELL [a node SYSTEMS INFO ACTION]
    (COND ((nil? a) (SHOWTELLCHOICE))
        ((getprop (car a) ACTION)
            (APPLY (getprop (car a) ACTION) (list a)))
        (:else
            (terpri)
            (print "I DON'T KNOW HOW TO" ACTION (car a))))
    '*)

(§ defn- SHOWTELLCHOICE []
    (SETQ NODE (QUERY "WHICH OPTION?" (let [_ (getprop NODE SYSTEMS)] (terpri) (pr _) _) (getprop NODE INFO)))
    (APPLY (getprop NODE ACTION) (list (list NODE))))

(def- breakchars (list (ascii 32) (ascii 13) (ascii 46)))

(§ defn- QUERY [text choices help]
    (let [EXPL nil CH2 nil EX2 nil CH3 nil EX3 nil CHAR nil NOTINIT nil]
        (SETQ EXPL (doall (map #'EXPLODE (cons 'QUIT choices))))
    TOP (SETQ CH2 (cons 'QUIT choices) EX2 EXPL)
        (terpri)
        (print- text)
    READ (COND ((memq (SETQ CHAR (READCH)) breakchars)
            (COND ((not NOTINIT) (GO READ))
                ((cdr CH2) (print (ascii 7)) (GO READ))
                (:else (dorun (map print (car EX2)))
                    (and (= (car CH2) 'QUIT)
                    (ERR nil))
                    (RETURN (car CH2)))))
            ((= CHAR (ascii 10)) (GO READ))
            ((= CHAR '?) (when help (terpri) (print help)) (GO CHOICES)))
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
    =>  (or (memq (READCH) breakchars) (GO =>))
    CHOICES
        (terpri)
        (print "THE CHOICES ARE:")
        (terpri)
        (pr choices)
        (GO TOP)))

(defn- onoff [arg help]
    (condp = (cadr arg) 'ON true 'OFF nil (= (QUERY "ON OR OFF?" '(ON OFF) help) 'ON)))

(§ defn- REQUEST [text help]
    (let [x nil]
    TOP (terpri)
        (print- text)
    READ (COND
            ((memq (ascii (TYIPEEK)) breakchars) (READCH) (GO READ))
            ((== (TYIPEEK) 10) (READCH) (RETURN nil))
            ((= (ascii (TYIPEEK)) '?) (READCH) (terpri) (print (or help "NO INFORMATION AVAILABLE")) (GO TOP))
            ((= (SETQ x (READ)) 'QUIT) (ERR nil))
            (:else (RETURN x)))
    nil))

(§ defn- SHOWPROP [x]
    (COND ((nil? x)
            (SHOWPROP (cons
                (REQUEST "ATOM:"
                    "THE NAME OF THE ATOM WHOSE PROPERTY (IES) YOU WANT TO EXAMINE")
                (listify (REQUEST "PROPERTY:"
                    "THE PROPERTY (IES) YOU WANT TO SEE.  <LF> MEANS ALL PROPERTIES OF THE ATOM")))))
        ((cdr x) (APPLY #'DISP x))
        (:else (DP (car x)) nil)))

(defn- treeprint [root tr col]
    (terpri)
    (TAB col)
    (print root)
    (dorun (map #(treeprint % tr (+ col 8)) (getprop root tr)))
    '*)

(defn- charg [x text help]
    (if (cdr x) (cadr x) (REQUEST text help)))

(§ defq- SHOW [a]
    (SHOWTELL a 'CANSHOW 'SHOWTREE 'SHOWINFO 'SHOW))

(§ defq- TELL [a]
    (SHOWTELL a 'CANTELL 'TELLTREE 'TELLINFO 'TELL))

(putprop! 'CANSHOW 'SHOWTREE '(SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT))
(putprop! 'CANSHOW 'SHOWINFO "THINGS WHICH CAN BE DISPLAYED")
(putprop! 'CANSHOW 'SHOW 'SHOWCHOICE)

(putprop! 'CANTELL 'TELLTREE '(LISP PLANNER PARSING DEFINITIONS SEMANTICS))
(putprop! 'CANTELL 'TELLINFO "THINGS WHICH CAN BE SET TO CONTROL HOW THE SYSTEM RUNS")
(putprop! 'CANTELL 'TELL 'TELLCHOICE)

(putprop! 'SHOW 'SHOW '(lambda [_] (treeprint 'CANSHOW 'SHOWTREE 0)))

(putprop! 'TELL 'SHOW '(lambda [_] (treeprint 'CANTELL 'TELLTREE 0)))

(putprop! 'LISP 'SHOWTREE '(PROPERTY FUNCTION VALUE))
(putprop! 'LISP 'TELL 'TELLCHOICE)
(putprop! 'LISP 'TELLTREE '(FUNCTION))
(putprop! 'LISP 'SHOW 'SHOWCHOICE)

(putprop! 'DO 'TELL '(lambda [_] (terpri) (print "NOT YET DEFINED")))

(putprop! 'STOP 'TELL '(lambda [x] (SETQ PLANNERSEE (and PLANNERSEE (if (onoff x "STOP AFTER SHOWING PLANNER INPUT?") true 'NOSTOP)))))

(putprop! 'PLANNER 'SHOWTREE '(ASSERTIONS THEOREM SCENE EVENT))
(putprop! 'PLANNER 'SHOW 'SHOWCHOICE)
(putprop! 'PLANNER 'TELLTREE '(INPUT ACTION THEOREM ASSERTIONS TELLABLE))
(putprop! 'PLANNER 'TELL '(lambda [x]
            (cond (nil? (cdr x)) (TELLCHOICE x)
                (= (cadr x) 'ON)
                    (do (print (ascii 12)) (thtrace 'THEOREM 'THASSERT 'THERASE '(THGOAL true true)) (SETQ PLANNERSEE true))
                (= (cadr x) 'OFF)
                    (do (print (ascii 12)) (SETQ PLANNERSEE nil) (thuntrace))
                :else (TELLCHOICE x))))

(putprop! 'PARSING 'SHOWTREE '(NODE TREE))
(putprop! 'PARSING 'SHOW 'SHOWCHOICE)
(putprop! 'PARSING 'TELLTREE '(NODE LABEL))
(putprop! 'PARSING 'TELL '(lambda [x]
            (cond (nil? (cdr x)) (TELLCHOICE x)
                (= (cadr x) 'ON)
                    (do (print (ascii 12)) (SETQ LABELTRACE true) (TRACE CALLSM #'PARSE))
                (= (cadr x) 'OFF)
                    (do (print (ascii 12)) (SETQ LABELTRACE nil) (UNTRACE CALLSM #'PARSE))
                :else (TELLCHOICE x))))

(putprop! 'DEFINITIONS 'SHOWTREE '(UNIT WORD MARKER))
(putprop! 'DEFINITIONS 'SHOW 'SHOWCHOICE)
(putprop! 'DEFINITIONS 'TELL 'TELLCHOICE)
(putprop! 'DEFINITIONS 'TELLTREE '(WORD MARKER))

(putprop! 'INPUT 'TELL '(lambda [x] (SETQ PLANNERSEE (onoff x "TO SEE INPUT TO PLANNER"))))
(putprop! 'INPUT 'SHOW 'SHOWCHOICE)
(putprop! 'INPUT 'SHOWTREE '(ALL REST CURRENT))

(putprop! 'RUN 'TELLTREE '(STOP DO))
(putprop! 'RUN 'TELL 'TELLCHOICE)
(putprop! 'RUN 'TELLINFO "PARAMETERS TO CONTROL WHAT SHRDLU DOES AS IT RUNS")

(putprop! 'PROPERTY 'SHOW '(lambda [x] (SHOWPROP (cdr x))))

(putprop! 'VALUE 'SHOW '(lambda [x] (DISP (eval (charg x "EXPRESSION:" "EXPRESSION TO BE EVALUATED BY THE LISP INTERPRETER")))))

(putprop! 'FUNCTION 'TELL '(lambda [x]
            (let [x (list (charg x "FUNCTION:" "LISP FUNCTION WHOSE ACTION IS TO BE TRACED")
                (if (and (cdr x) (cddr x) (memq (caddr x) '(TRACE BREAK UNTRACE UNBREAK))) (caddr x)
                    (QUERY "TRACE, BREAK, UNTRACE OR UNBREAK?"
                        '(TRACE BREAK UNTRACE UNBREAK)
                        "TRACE CAUSES PRINTOUT ON ENTRY AND EXIT OF FUNCTION.  BREAK CAUSES LISP TO STOP ON ENTRY AND EXIT, ACCEPTING USER COMMANDS AND CONTINUING WHEN <CTRL-X> IS TYPED.")))]
                (APPLY (SUBST 'WBREAK 'BREAK (cadr x)) (list (car x))))))
(putprop! 'FUNCTION 'SHOW '(lambda [x] (APPLY #'GB (list (charg x "FUNCTION:" "LISP FUNCTION WHOSE DEFINITION IS TO BE SHOWN")))))

(putprop! 'ASSERTIONS 'TELL '(lambda [x] (THVAL (list 'THASSERT (charg x "ASSERTION:" "PLANNER ASSERTION TO BE ADDED TO DATABASE") '(THTBF thtrue)) nil)))
(putprop! 'ASSERTIONS 'SHOW '(lambda [x] (DA (charg x "ATOM:" "SHOW ALL ASSERTIONS WHICH CONTAIN THE GIVEN ATOM"))))

(putprop! 'THEOREM 'TELL '(lambda [x]
            (let [x (if (cdr x) (cadr x) (gensym 'THEOREM))]
                (putprop! x 'THEOREM
                    (list (QUERY "WHICH THEOREM TYPE?" '(THANTE THERASING THCONSE) "ANTECEDENT, ERASING, OR CONSEQUENT THEOREM")
                        (listify (REQUEST "VARIABLE-LIST:" nil))
                        (REQUEST "PATTERN:" "A LIST ENCLOSED IN PARENS, LIKE (!IS ($? x) !ZOG)")
                        (REQUEST "BODY:" "LIST OF MICROPLANNER STATEMENTS")))
                (THADD x nil)
                (terpri)
                (pr x))))
(putprop! 'THEOREM 'SHOW '(lambda [x] (DISP (getprop (charg x "THEOREM-NAME:" "PLANNER THEOREM WHOSE DEFINITION IS TO BE SHOWN") 'THEOREM))))

(defn- showmove [x]
    (apply #'MOVE-PT
        (listify (or x
            (REQUEST "NODE-SPECIFICATION:"
                "C MEANS CURRENT NODE, H IS MOST RECENTLY PARSED FOR OTHER POSSIBILITIES.  SEE THESIS SECTION ON POINTER-MOVING COMMANDS")))))

(putprop! 'NODE 'SHOW '(lambda [x]
    (cond (getprop (cadr x) 'FEATURES) (DP (cadr x)) (do (set! *savept* *pt*) (showmove (cdr x))) (do (DP (car *pt*)) (set! *pt* *savept*)) :else (print "NO SUCH NODE"))))

(putprop! 'TREE 'SHOW '(lambda [x]
    (cond (getprop (cadr x) 'FEATURES) (WALLP (list (cadr x))) (do (set! *savept* *pt*) (showmove (cdr x))) (do (WALLP *pt*) (set! *pt* *savept*)) :else (print "NO SUCH TREE"))))

(putprop! 'UNIT 'SHOW '(lambda [x]
    (APPLY #'DG (or (cdr x) (list (REQUEST "UNIT:" "GRAMMAR UNIT WHOSE PROGRAM IS TO BE EXAMINED -- E.G. CLAUSE, NG, PREPG, VG, ADJG"))))))

(putprop! 'WORD 'SHOW '(lambda [x] (DP (charg x "WORD:" "ENGLISH WORD IN THE VOCABULARY"))))
(putprop! 'WORD 'TELL '(lambda [x] (APPLY #'DEFINE (list (charg x "WORD:" "ENGLISH WORD TO BE DEFINED -- MUST BE NOUN OR VERB")))))

(putprop! 'ACTION 'TELL '(lambda [x]
            (let [x (cond (cdr x) (cond (= (cadr x) 'ON) nil (= x 'OFF) '(thuntrace) :else x)
                        (onoff x "WATCH PLANNER PROGRAMS STEP BY STEP?") nil
                        :else '(thuntrace))]
                (if x (thuntrace) (thtrace 'THEOREM 'THGOAL 'THASSERT 'THERASE)))))

(putprop! 'LABEL 'TELL '(lambda [x]
            (let [x (if (cdr x) x (list (REQUEST "TYPE LIST OF LABELS, OR ON OR OFF:" "WATCHES PARSER GO PAST PROGRAM LABELS IN THE GRAMMAR")))]
                (SETQ LABELTRACE (if (= (car x) 'OFF) nil (car x))))))

(putprop! 'MARKER 'TELL '(lambda [x]
            (let [x (charg x "MARKER:" "MARKER TO BE ADDED") y (REQUEST "PARENT:" "NODE TO WHICH IT IS ATTACHED IN THE TREE")]
                (putprop! x 'SYS (list y))
                (putprop! y 'SYSTEM (cons x (getprop y 'SYSTEM)))
                nil)))
(putprop! 'MARKER 'SHOW '(lambda [x]
            (let [x (charg x "MARKER:" "SEMANTIC MARKER WHOSE SUBSETS ARE TO BE EXAMINED.  TYPE <LF> FOR ENTIRE TREE")]
                (treeprint (or x '!SYSTEMS) 'SYSTEM 0))))

(putprop! 'ALL 'SHOW '(lambda [_] (%sent)))

(putprop! 'CURRENT 'SHOW '(lambda [_] (terpri) (print (from NB N))))

(putprop! 'REST 'SHOW '(lambda [_] (terpri) (print N)))

(putprop! 'SCENE 'SHOW '(lambda [_]
            (let [PLANNERSEE nil]
                (terpri)
                (print "CURRENT SCENE:")
                (terpri)
                (dorun (map #'(lambda [obj]
                        (terpri)
                        (pr obj)
                        (print " --> ")
                        (evlis (car (NAMEOBJ obj 'DESCRIBE)))
                        (-print "AT" (cadr (assq obj ATABLE)))
                        (let [obj (THVAL '(THFIND ALL ($? x) (x) (THGOAL (!SUPPORT ($? obj) ($? x)))) (list (list 'obj obj)))]
                            (when obj
                                (-print "SUPPORTS" obj))))
                    '(ßB1 ßB2 ßB3 ßB4 ßB5 ßB6 ßB7 ßB10 ßBOX)))
                (let [obj (THVAL '(THGOAL (!GRASPING ($_ x))) '((x THUNBOUND)))]
                    (terpri)
                    (print "THE HAND IS GRASPING" (if obj (cadar obj) "NOTHING"))
                    nil))))

(§ defq- DEFINE [a]
    (let [*fe* nil TYPE nil MARK nil REST nil TR nil]
        (SETQ a (if a (car a) (REQUEST "WORD:" "ENGLISH WORD TO BE DEFINED")))
        (SETQ TYPE (QUERY "NOUN OR VERB?" '(NOUN VERB) "OTHER TYPES MUST BE DEFINED IN LISP"))
    =>  (or (SETQ MARK (REQUEST "MARKERS:" "LIST OF SEMANTIC MARKERS FOR WORD BEING DEFINED -- TO SEE MARKER TREE, TYPE <LF>"))
            (and (SHOW MARKER !SYSTEMS) (GO =>)))
        (SETQ MARK (listify MARK))
        (COND
            ((= TYPE 'NOUN)
                (putprop! a 'FEATURES '(NOUN NS))
                (putprop! a 'SEMANTICS
                    (list (list 'NOUN (list 'OBJECT (list 'MARKERSß MARK
                        'PROCEDUREß
                        (lis2fy (REQUEST "PROCEDURE:"
                                    "EXPRESSION OR LIST OF EXPRESSIONS TO BE PUT IN PLANNER GOALS TO DESCRIBE OBJECT -- USE *** TO REPRESENT OBJECT BEING DESCRIBED BY WORD -- E.G. (!IS *** !ZOG) OR ((!IS *** !ZOG) (!LOVE ßEVERYONE ***))")))))))
                (RETURN true))
            ((SETQ TR (= (QUERY "TRANSITIVE OR INTRANSITIVE?" '(TRANSITIVE INTRANSITIVE) nil) 'TRANSITIVE))
                (putprop! a 'FEATURES '(VB TRANS INF)))
            (:else (putprop! a 'FEATURES '(VB ITRNS INF))))
        (SETQ REST (list (list (listify (REQUEST "RESTRICTIONS ON SUBJECT:" "LIST OF SEMANTIC MARKERS")))))
        (and TR
            (SETQ REST (concat REST (list (listify (REQUEST "RESTRICTIONS ON OBJECT:" "LIST OF SEMANTIC MARKERS"))))))
        (putprop! a 'SEMANTICS
            (list (list 'VB (list 'RELATION (list
                'MARKERSß MARK
                'RESTRICTIONSß REST
                'PROCEDUREß
                    (lis2fy (REQUEST "PROCEDURE:"
                                "LIST OF EXPRESSIONS TO BE PUT INTO PLANNER GOALS TO DESCRIBE ACTION OR RELATION -- USE !1 FOR SUBJECT, !2 FOR OBJECT.  E.G. (!SUPPORT !1 !2) OR ((!HAPPY !1) (!SMILING !1))")))))))
        true))

(§ defn- HELP []
    (if (= (QUERY "TYPE L FOR LONG FORM (85 LINES) S FOR SHORT (16 LINES)" '(S L) nil) 'S) (UREAD MINIH DOC DSK LANG) (UREAD HELP DOC DSK LANG))
    (THRUTEXT)
    '*)

#_(ns shrdlu.macros)

;; #############################################################
;;
;;              A PRECOMPILER FOR PROGRAMMAR CODE
;;
;; #############################################################

(defn- spread1 [e]
    (cond (term? e)
            (list e (list 'when 'LABELTRACE (list 'passing (list 'quote e))))
        (= (car e) '|)
            (let [predicate (cadr e) t1 (caddr e) t2 (cadddr e) _ (cddddr e) t3 (when _ (car _))
                  list- (lambda [& a] (mapcat #(if (term? (car %)) (list %) %) a))
                  tag- #(if (term? %) (list (list 'GO %)) (list (list 'm! (car %)) (list 'GO 'FAIL)))]
                (list
                    (list 'SETQ 'ßRESULT predicate)
                    (cond
                        (and t1 (nil? t2)) ;; T3 CAN BE EITHER THERE OR NOT
                            (list 'COND (list- 'ßRESULT (if t3 (list 'COND (list- (list 'nil? 'NN) (tag- t3)) (list- ':else (tag- t1))) (tag- t1))))
                        (and (nil? t1) t2 (nil? t3))
                            (list 'COND (list- (list 'nil? 'ßRESULT) (tag- t2)))
                        (and (nil? t1) t2 t3)
                            (list 'COND (list (list 'nil? 'ßRESULT) (list 'COND (list- (list 'nil? 'NN) (tag- t3)) (list- ':else (tag- t2)))))
                        (and t1 t2 (nil? t3))
                            (list 'COND (list- 'ßRESULT (tag- t1)) (list- ':else (tag- t2)))
                        (and t1 t2 t3)
                            (list 'COND (list 'ßRESULT (list 'COND (list- (list 'nil? 'NN) (tag- t3)) (list- ':else (tag- t1)))) (list- ':else (tag- t2)))
                        (and (nil? t1) (nil? t2) t3)
                            (list 'COND (list- (list 'AND (list 'nil? 'NN) 'ßRESULT) (tag- t3)))
                        (and (nil? t1) (nil? t2) (nil? t3))
                            (list 'I-AM-A-TAG))))
        :else (list e)))

(§ defq- PDEFINE [a]
    (list 'DEFUN (car a) 'nil
        (concat (list 'PROG
            (concat '(*fe* H *me* NB *c* SM CUT NN T1 T2 T3 ßRESULT) (cadr a))
            '(SETQ NN true)
            '(SETQ CUT *end*)
            '(set! *c* (buildnode (set! *fe* (reverse REST)) (SETQ NB (or (NB *re*) N)) N (SETQ H *re*) nil))
            '(setr 'PARENT PARENT *c*))
            (mapcat spread1 (cddr a))
            (list 'FAIL
                '(set! *mes* *me*)
                '(SETQ N (or (N *re*) NB))
                '(RETURN nil)
                'RETURN
                '(set! *mes* *me*)
                '(RETURN (rebuild (reverse *fe*) NB N H SM *c*))))))

#_(ns shrdlu.progmr)

;; ###########################################################
;;
;;                         PROGMR
;;  (INTERPRETER FOR THE PROGRAMMAR GRAMMAR WRITING LANGUAGE)
;;
;; ###########################################################

(defn- setmvb [ptr-mvb]
    (let [save *pt*]
        (SETQ MVB ptr-mvb)                              ;; IF THERE ARE SEVERAL CLAUSES ACTIVE AT THE
        (set! *pt* ptr-mvb)                               ;; SAME TIME, IT SETS THE NEAREST ONE.
        (setr 'MVB ptr-mvb (MOVE-PT 'C 'U '(CLAUSE)))
        (set! *pt* save)
        true))

(defn- add-f-pt [feature ptr]
    (putprop! (car ptr) 'FEATURES (cons feature (features ptr)))
    (when (= ptr *c*)
        (set! *fe* (features ptr)))
    true)

(defn- remove-f-pt [feature ptr]
    (putprop! (car ptr) 'FEATURES (SETDIF (getprop (car ptr) 'FEATURES) (list feature)))
    (when (= ptr *c*)
        (set! *fe* (features ptr)))
    true)

(defn- one-word-left [nb] (and (cdr nb) (not (cddr nb))))

(def- smnbreaks nil) ;; A LIST OF SMNFNS WHICH WILL BE BROKEN AT (BEFORE CALLING)

(§ defq- CALLSM [semantic-expression]
    (let [RESULT nil SMNFN (car semantic-expression)]
        (SETQ RESULT (eval (car semantic-expression)))
        (COND ((or (= smnbreaks 'ALL) (memq SMNFN smnbreaks)) (ert nil)))
        RESULT))

(§ defn- MOVE-PT [& a]
    (let [XX nil YY nil L2 nil save *pt*]
    TEST1 (when (and (cdr a) (not (term? (cadr a)))) (GO TEST))
    LOOK1 (SETQ XX (car a))
    LOOK (condp = XX
            'H (if-not (set! *pt* H) (GO FAIL))
            'C (set! *pt* *c*)
            'PC (set! *pt* (H (parent *c*)))
            'LASTSENT (set! *pt* *lastsent*)
            'U (if-not (set! *pt* (parent *pt*)) (GO FAIL))
            'DLC (if-not (set! *pt* (H *pt*)) (GO FAIL))
            'DF (do (SETQ L2 (cons 'DLC (cons 'FR L2))) (SETQ XX 'DLC) (GO LOOK))
            'FR (when (MOVE-PT 'PV) (GO LOOK))
            'NX (if-not (set! *pt* (previous (H (parent *pt*)) (car *pt*))) (GO FAIL))
            'PV (set! *pt* (or (and (= *pt* *c*) (H (parent *c*))) (following (H (parent *pt*)) (car *pt*)) (GO FAIL)))
            (bug "MOVE-PT: ILLEGAL INSTRUCTION"))
    EX  (COND ((or (nil? L2) (nil? (SETQ L2 (cdr L2))))
            (GO TEST)))
        (SETQ XX (car L2))
        (GO LOOK)
    TEST (cond (nil? (cdr a)) (RETURN *pt*)
            (term? (cadr a)) true
            (if (cdadr a) (eval (cadr a)) (isx *pt* (caadr a))) (SETQ a (cdr a))
            :else (GO LOOK1))
        (when (SETQ a (cdr a)) (GO TEST1))
        (RETURN *pt*)
    FAIL (set! *pt* save)
        nil))

(§ defn- MOVE-PTW [& a]
    (let [save *ptw* XX nil]
    TEST1 (when (and (cdr a) (not (term? (cadr a)))) (GO EX))
    LOOK1 (SETQ XX (car a))
    LOOK (condp = XX
            'N (set! *ptw* N)
            'LASTSENT (set! *ptw* (NB *lastsent*))
            'FW (set! *ptw* (NB *pt*))
            'AW (cond (= *pt* *c*) (GO FAIL) (set! *ptw* (N *pt*)) (do (SETQ XX 'PW) (GO LOOK)))
            'LW (cond (= *pt* *c*) (GO FAIL) (set! *ptw* (N *pt*)) (do (SETQ XX 'PW) (GO LOOK)))
            'NW (cond (set! *ptw* (cdr *ptw*)) *ptw* (set! *ptw* (findb *sent* nil)) (GO FAIL))
            'PW (cond (set! *ptw* (findb *sent* *ptw*)) *ptw* (set! *ptw* *sent*) (GO FAIL))
            'SFW (set! *ptw* *sent*)
            'SLW (set! *ptw* (findb *sent* nil))
            (bug "MOVE-PTW: ILLEGAL INSTRUCTION"))
    EX  (cond (nil? (cdr a)) (RETURN *ptw*)
            (term? (cadr a)) true
            (if (cdadr a) (eval (cadr a)) (isx *ptw* (caadr a))) (SETQ a (cdr a))
            :else (GO LOOK1))
        (when (SETQ a (cdr a)) (GO TEST1))
        (RETURN *ptw*)
    FAIL (set! *ptw* save)
        nil))

(defn- apply-grammar [UNIT]
    (if (getprop UNIT 'INTERPRET) (INTERPRET UNIT) (eval (list UNIT))))

(defn- buildnode [features firstword wordafter daughters semantics]
    (let [node (list (gensym 'NODE))]
        (setr 'FEATURES features node)
        (setr 'FIRSTWORD firstword node)
        (setr 'WORDAFTER wordafter node)
        (setr 'DAUGHTERS daughters node)
        (setr 'SEMANTICS semantics node)
        node))

(defn- cq [feature fe] (memq feature fe))

(§ defn- CUT [a]
    (let [b N]
    =>  (COND
            ((= a b) (SETQ CUT a) (SETQ NN (not (= CUT N))) (RETURN true))
            ((= b *end*) (RETURN nil))
            ((SETQ b (cdr b)) (GO =>))
            ((nil? a) (SETQ CUT nil) (SETQ NN N) (RETURN true)))))

(§ defn- CUT-BACK-ONE [] (MOVE-PTW 'N 'PW) (POP) (CUT *ptw*))

(§ defn- F [a]
    (COND ((memq a *fe*) true) ((setr 'FEATURES (set! *fe* (cons a *fe*)) *c*))))

(defn- features [node] (getr 'FEATURES node))

(§ defn- FESET [node features] (setr 'FEATURES features node))

(§ defn- FLUSHME []
    ;; IF YOU HAVEN'T REAHED THE CUT, FLUSHES THE NEXT WORD IN THE SENTENCE.  FAILS IF IT REACHES CUT POINT
    (and N NN (SETQ NN (not (= CUT (SETQ N (cdr N)))))))

(defn- following [a x]
    ;; GET THE ELEMENT OF LIST FOLLOWING MEMBER
    (let [a (memq x a)] (when a (cdr a))))

(§ defq- FQ [a]
    (dorun (map #'(lambda [x] (or (memq x *fe*) (set! *fe* (cons x *fe*)))) a))
    (setr 'FEATURES *fe* *c*))

(defn- getr [register node]
    ;; RETRIEVE THE CONTENTS OF THE REGISTER ASSOCIATED WITH THE GIVEN NODE
    (getprop (car node) register))

(§ defn- H [node] (getr 'DAUGHTERS node))

(§ defq- ISQ [a] (memq (cadr a) (features (eval (car a)))))

(defn- isx [a x] (memq x (features a)))

(defn- m! [x] (set! *me* (cons x *me*)))

(defn- mq! [& a] (set! *me* (cons a *me*)))

(§ defn- N [node] (getr 'WORDAFTER node))

(§ defn- NB [node] (getr 'FIRSTWORD node))

(defn- nextword [n] (car n))                ;; RETURN THE NEXT WORD IN THE SENTENCE

(defn- nextword? [n w] (= (car n) w))

(defn- nq [x] (memq x (features N)))

(defn- parent [node] (getr 'PARENT node))

(defn- parse [& a]
    (if (memq (car a) '(NG CLAUSE VG PREPG ADJG)) (PARSE2 a (memq 'TOPLEVEL a)) (PARSE3 a nil)))

(§ defn- PARSE2 [REST p]
    ;; THIS FUNCTION CALLS THE PROGRAMMAR FUNCTION INDICATED BY THE FIRST MEMBER OF REST - A FEATURE LIST.
    ;; THE PARAMETER P INDICATES WHETHER PARSE2 IS BEING CALLED FROM THE TOPLEVEL.
    ;; IF P IS TRUE, PARSE2 DOES NOT APPEND THE NODE JUST PARSED TO THE PARSING TREE.
    ;; PARSE2 WILL CALL EITHER A COMPILED OR INTERPRETED VERSION OF THE GRAMMAR PROGRAM.
    (let [UNIT (car REST) CREATED-NODE nil *end* nil PARENT nil *re* nil SPECIAL nil nbb N]
        (set! *level* (inc *level*))
        (COND ((= N CUT)
            (set! *level* (dec *level*))
            (RETURN nil)))
        (set! *end* CUT)
        (SETQ NN (not (= N CUT)))
        (SETQ PARENT *c*)
        (COND ((nq 'B-SPECIAL)
            (eval (getr 'B-SPECIAL N))))
        (COND ((= SPECIAL 'SKIP) (GO SKIP))
            ((= SPECIAL 'DONE) (GO DONE))
            ((= SPECIAL 'LOSE) (SETQ N nbb) (GO LOSE)))
        (COND ((nil? (set! *re* (apply-grammar UNIT)))    ;; THIS IS WHERE ALL THE WORK HAPPENS.  IF THE PARSE SUCCEEDS,
            (set! *re* nil)                               ;; IT WILL RETURN THE NODE THAT HAS BEEN BUILT UP
            (SETQ N nbb)                                ;; (SEE THE FUNCTION "INTERPRETATION" IN IN GINTER)
            (GO LOSE)))
    SKIP (COND ((= N CUT))
            ((nq 'SPECIAL) (eval (getr 'SPECIAL N))))
    DONE (or p
            (rebuild (set! *fe* (getprop (car *c*) 'FEATURES))  ;; REBUILDS THE HIGHER LEVEL NODE TO INCORPORATE
                NB                                      ;; THE DAUGHTER THAT WAS JUST PARSED EXCEPT IN THE
                N                                       ;; CASE WHERE THIS NODE IS THE TOPLEVEL
                (SETQ H (concat *re* H))
                SM
                *c*))
    LOSE (SETQ NN (not (= N CUT)))
    OK  (set! *level* (dec *level*))
        *re*))

(§ defn- PARSE3 [rest p]
    ;; PARSE3 IS CALLED TO PARSE JUST THE NEXT WORD IN THE SENTENCE
    (let [XP nil LABL nil *re* nil SPECIAL nil NBB nil NODE nil]
        (COND ((= (SETQ NBB N) CUT) (mq! 'CUT) (RETURN nil))
            ((nq 'B-SPECIAL)                                     ;; IS THE NEXT WORD MARKED SPECL?
            (eval (getr 'B-SPECIAL N))                          ;; YES, DO SOMETHING SPECIALL
            (COND ((= SPECIAL 'SKIP) (GO SKIP))
                ((= SPECIAL 'LOSE) (SETQ N NBB) (RETURN nil))
                ((= SPECIAL 'DONE) (GO DONE)))))
        (COND ((car (SETQ XP rest)))                            ;; IF CALL IS (PARSE NIL FOO)
            ((nextword? N (cadr rest)) (GO OK))                   ;; THEN LOOK FOR EXACT WORD "FOO"
            ((SETQ N NBB) (RETURN nil)))                        ;; IF NOT THERE, FAIL
    LOOP (COND ((not (term? (car XP)))
            (SETQ LABL (cons (caar XP) LABL)))                  ;; IF THE FEATURE IS NOT AN ATOM JUST ADD THE
            ((= (car XP) 'NULL))                               ;; FEATURE TO THE LIST
            ((memq (car XP) (features N)))
            ((memq (car XP) '(COMPONENT BOTH)))
            ((m! (car XP)) (SETQ N NBB) (RETURN nil)))
        (COND ((SETQ XP (cdr XP)) (GO LOOP)))
    OK  (set! *re*
            (buildnode (MEET (concat (features N) LABL) (getprop (car rest) 'ELIM))
                N
                (cdr N)
                'WORD
                (or (nil? (car rest))
                    (and (nil? (SM N)) (undefined N))
                    (cadr (sassq (car rest) (SM N) #(undefined N))))))
        (SETQ N (cdr N))
    SKIP (SETQ NN (not (= N CUT)))
        (COND ((and NN (nq 'SPECIAL))
            (eval (getr 'SPECIAL N))))
    DONE (setr 'PARENT *c* *re*)
        (COND (p *re*)
            (:else (rebuild *fe* NB N (SETQ H (concat *re* H)) SM *c*)))
        *re*))

(§ defn- PARSEREL [a b node]
=>  (COND ((nil? a) (RETURN nil))
        ((not (isx node (caar a))))
        ((apply parse 'CLAUSE 'RSNG (concat (cdar a) b)) (RETURN H)))
    (SETQ a (cdr a))
    (GO =>))

(§ defq- POP [a]
    (COND ((or (nil? a) (nil? (car a)))
            (COND ((nil? H) nil)
                ((SETQ N (NB H))
                    (SETQ H (cdr H))
                    (rebuild *fe* NB N H SM *c*)
                    (SETQ NN (not (= N CUT)))
                    (let [XX nil]
                        (MAP #'(lambda [BACKNODE]
                            (ERRSET
                                (and (MAP #'(lambda [PLACE] (and (= PLACE (NB BACKNODE)) (ERR))) N)
                                    (SETQ XX (cons (car BACKNODE) XX)))))
                            *backref*)
                        (set! *backref* XX)
                        nil)
                    true)))
        ((eval (cons 'POPTO a)) (POP))))

(§ defq- POPTO [a]
    (let [XX H]
    LOOP (COND ((eval (cons 'ISQ (cons 'XX a))))
            ((SETQ XX (cdr XX)) (GO LOOP))
            ((mq! 'POPTO) (RETURN nil)))
    EX  (COND ((= XX H) (RETURN *c*))
            ((POP) (GO EX)))))

(defn- previous [a x]
    ;; GET THE ELEMENT OF LIST BEFORE MEMBER
    (loop [e nil a a]
        (cond (nil? a) nil (= (car a) x) e :else (recur (car a) (cdr a)))))

(§ defn- PTFIND [x yy z]
    (let [foo (car x)]
    UP  (COND ((MOVE-PT 'U) (GO UP)) ((= (NB *pt*) x) (GO ON)))
    DOWN (or (MOVE-PT 'DLC 'PV '(memq foo (NB *pt*))) (RETURN nil))
    ON  (COND ((not (= x (NB *pt*))) (GO DOWN))
            ((= yy true))
            ((MOVE-PT 'DF '(= (N *pt*) yy)))
            ((RETURN nil)))
    CHECK (COND ((eval z) (RETURN *pt*))
            ((not (= yy true)))
            ((MOVE-PT 'DF) (GO CHECK)))
        nil))

(defn- rebuild [features firstword wordafter daughters semantics node]
    (setr 'FEATURES features node)
    (setr 'FIRSTWORD firstword node)
    (setr 'WORDAFTER wordafter node)
    (setr 'DAUGHTERS daughters node)
    (setr 'SEMANTICS semantics node)
    node)

(defn- root [a]
    ;; INPUT = PIECE OF SENTENCE
    ;; OUTPUT = ROOT OF FIRST WORD IN THAT PIECE
    ;; IF WORD HAS NO ROOT PROPERTY, THE ROOT = WORD
    (or (getprop (car a) 'ROOT) (car a)))

(§ defq- RQ [a] (setr 'FEATURES (set! *fe* (SETDIF *fe* a)) *c*)) ;; REMOVE THE FEATURE A FROM FEATURE LIST OF THE CURRENT NODE

(defn- secondword? [n w] (and n (cdr n) (= (cadr n) w)))

(defn- setr [register value node]
    ;; ASSOCIATE THE GIVEN VALUE WITH THE GIVEN NODE UNDER THE GIVEN INDICATOR REGISTER
    (putprop! (car node) register value))

(§ defn- SM [node] (getr 'SEMANTICS node))

(§ defq- TRNSF [a]
    (setr 'FEATURES (set! *fe* (UNION (MEET a (features *pt*)) *fe*)) *c*))

(§ defn- UPREL [x]
    (and (not (term? x))
        (or (memq 'UPREL (features x)) (UPREL (H x)) (UPREL (cdr x))))) ;; FIND NODE WITH UPREL FEATURE

(defn- word [n] (car n))

(§ defn- UPCHECK []
    (and (MOVE-PT 'C 'U '(REL-NOT-FOUND))
        (not (MEET (features *pt*) '(OBJ1Q OBJ1REL OBJ2Q OBJ2REL LOBREL LOBQ)))))

#_(ns shrdlu.ginter)

(§ defq- PDEFINE [a]
    ;; THIS PDEFINE MERELY PUT THE PROGRAMMAR FUNCTION ON THE
    ;; PROPERTY LIST OF THE PARSE NAME UNDER THE INDICATOR
    ;; 'INTERPRET. IT ALSO ADDS THE TAGS FAIL AND RETURN. NOTE THAT
    ;; THE PDEFINE BODY IS SIMILIAR TO PROG BODY. THIS SETS UP
    ;; INTERPRETED PROGRAMMAR EXECUTIONS
    (putprop! (car a) 'INTERPRET
        (concat (cdr a) (list 'FAIL '(RETURN 'FAIL) 'RETURN '(RETURN 'RETURN)))))

(§ defn- INTERPRET [unit]
    ;; INTERPRET IS THE FUNCTION WHICH 'CALLS' AN INTERPRETED PROGRAMMAR PROGRAM.
    ;; IT FIRST DECLARES AND INITIALIZES ALL THE RELAVENT VARIABLES, THEN
    ;; IT EXECUTES THE PROGRAMMAR BODY AS A PROG.  NOTE THE USE OF "RE":
    ;; IT IS SET TO A NODE ONE WISHES TO BE THE INITIAL DAUGHTER OF THIS NODE.
    ;; ONLY CONJ NEEDS THIS HACK.
    (let [*fe* nil H nil *me* nil NB nil *c* nil SM nil CUT nil NN true T1 nil T2 nil T3 nil]
        (SETQ CUT *end*)
        (set! *c* (buildnode (set! *fe* (reverse REST))     ;; FEATURE LIST
            (SETQ NB (or (NB *re*) N))                    ;; BEGINNING IN SENTENCE OF THIS NODE
            N                                           ;; SENTENCE POINTER JUST AFTER THIS NODE
            (SETQ H *re*)                                 ;; DAUGHTERS OF THIS NODE
            nil))                                       ;; SEMANTIC JAZZ
        (setr 'PARENT PARENT *c*)                         ;; SET PARENT REGISTER
        (COND ((= (APPLY #'PROG (getprop unit 'INTERPRET)) 'RETURN)
            (GO =>)))                               ;; APPLY THE PROGRAMMAR PROGRAM
        (set! *mes* *me*)
        (SETQ N (or (N *re*) NB))                         ;; RESET SENTENCE POINTER
        (RETURN nil)
    =>  (set! *mes* *me*)
        (rebuild (reverse *fe*) NB N H SM *c*)))

(defn- gocheck [a]
    ;; THE GOCHECK FUNCTION CHECKS THE BRANCHES OF THE PROGRAMMAR CONDITIONAL.
    ;; IF THE BRANCH IS NIL, GOCHECK MERELY RETURNS.
    ;; IF THE BRANCH IS NON-ATOMIC, IT IS TAKEN AS A FAILURE MESSAGE.
    ;; GOCHECK PUTS THE MESSAGE ON THE MESSAGE LIST AND GOES TO FAIL (IN GINTERP)
    ;; IF THE BRANCH IS ATOMIC, GOCHECK GOES TO IT.
    ;; REMEMBER THAT THE GO LEADS TO A POINT SOMEWHERE IN THE PROGRAMMAR PROGRAM,
    ;; UNLESS IT IS 'FAIL OR 'RETURN.
    (let [x (cadr a)]
        (cond (nil? x) true (term? x) (GO x) :else (do (m! x) (GO FAIL)))))

(§ defq- | [a]
    ;; EVALUATE BRANCH CONDITION.
    ;; IF TRUE AND NO MORE SENTENCE REMAINS AND IF THIRD BRANCH GIVEN, THEN GO TO THIRD BRANCH, ELSE GO TO 2ND BRANCH.
    (gocheck (if (eval (car a)) (if (and (nil? NN) (cdddr a)) (cddr a) a) (cdr a))))

#_(ns shrdlu.gramar)

(§ PDEFINE CLAUSE (POSITION-OF-PRT MVB LOCATIONMARKER SUBJ-VB-BACKUP-TYPE1 POSITION-OF-PTW)

    ENTERING-CLAUSE
        (setr 'TIME (BUILD TSSNODE= (gensym 'TSS)) *c*)
        (| (cq 'SIMP *fe*) SUBJ nil)
        (| (cq 'MAJOR *fe*) INIT SEC)

    INIT
        (SETQ LOCATIONMARKER N)
        (| (and (nq 'BINDER) (parse 'CLAUSE 'BOUND 'INIT)) nil MAJOR FIXIT)
        (FQ BIND)
        (| (CALLSM (SMBIND)) INIT nil)

    FIXIT
        (set! *ptw* CUT)
        (| (CUT (MOVE-PTW)) INIT MAJOR)

    MAJOR
        (CUT *end*)
        (COND ((= *punct* '?) (GO QUEST))
            ((or (cq 'IMPER *fe*) (= *punct* '!)) (GO IMPER)))
        (GO THEREINIT)

    FDEC
        (FQ DECLAR)

    THEREINIT                                                       ;; CONSTRUCTIONS USING THE FUNCTION WORD "THERE"
        (| (and (nextword? N 'THERE)                                  ;; ARE CHECKED FOR EXPLICITLY AND PROCESSED BY A
                (parse nil 'THERE)                                   ;; SPECIAL BLOCK OF CODE (SEE ABOVE)
                (FQ DECLAR))
            THERE
            nil
            (INIT))

    THER2
        (and (nq 'PREP)
            (parse 'PREPG 'INIT)
            (or (CALLSM (SMRELATE H)) (POP)))                       ;; MORE INITIAL (BEFORE THE SUBJECT) MODIFIERS
        (and (nq 'ADV)
            (parse 'ADV 'TIMW)
            (or (CALLSM (SMADVERB)) (POP)))
        (and (nq 'ADV)
            (parse 'ADJG 'ADV 'VBAD)
            (or (CALLSM (SMRELATE H)) (POP)))
        (parse 'NG 'TIME)

        (| (= LOCATIONMARKER N) CLAUSETYPE INIT INPOP)

    ;; THE VARIABLE "LOCATIONMARKER" MARKS THE POSITION OF PTW AT THE TIME THAT IT WAS SET.
    ;; IF IT HAS NOT MOVED (IS STILL EQUAL TO N), THEN THAT INDICATES THAT NOTHING HAS BEEN
    ;; PARSED AND WE CAN GO ON.  OTHERWISE, THERE CAN BE ANY NUMBER OF INITIAL MODIFIERS AND
    ;; THE CODE STARTING AT "INIT" IS REPEATED, AS MANY TIMES AS NECESSARY.  IF PTW HITS THE
    ;; CUT POINT, THEN IT IS ASSUMED THAT SOMETHING WAS MISTAKENLY PARSED AS A MODIFIER WHEN
    ;; IT WAS NOT, AND EVERYTHING IS POPPED OFF (BY THE "INPOP" CODE).

    INPOP
        (| (MOVE-PT 'C 'DLC) nil (INPOP))                             ;; DOES ANYTHING REMAIN ON THE TREE?

    BICUT
        (CUT-BACK-ONE)                                              ;; "CUT-BACK-ONE" IS THE NORMAL BACKINGUP
        (GO INIT)                                                   ;; MECHANISM FOR THE GRAMMAR, IT SETS PTW (POINTER
                                                                    ;; TO THE WORD) BACK ONE FROM WHERE IT WAS AND
                                                                    ;; SETS "CUT" TO PTW. THE FOLLOWING GOTO TELLS
                                                                    ;; WHICH BLOCK OF CODE IS TO BE REPEATED.

    ;; RE-EXAMINE THE CLAUSETYPE, PARTICULARLY TO CHECK FOR VERB-INITIAL IMPERATIVES

    CLAUSETYPE
        (| (cq 'DECLAR *fe*) SUBJ nil)
        (| (and (nq 'VB) (nq 'INF) (parse 'VG 'IMPER) (FQ IMPER))
            VG1
            nil)                                                    ;; SEE THE NOTE UNDER IMPERATIVES BELOW
        (FQ DECLAR)
        (| (cq 'IMPER *fe*) (IMPER) nil)

    ;; ###############################################################
    ;;         TRY TO PARSE A GRAMMATICLY ACCEPTABLE SUBJECT
    ;; ###############################################################

    ;; ONCE THAT IS DONE, SET THE SUBJECT REGISTER (FOR USE BY SEMANTIC ROUTINES AND OTHER PARTS OF THE GRAMMAR)
    ;; AND MOVE ONE TO THE CODE FOR WHICH LOOKS FOR THE MAIN VERB (MVB) - "VG"

    SUBJ (CUT *end*)                                                  ;; RESET CUTPOINT INCASE IT WAS MODIFIED BY
    SUBJ3                                                           ;; PREVIOUS BACKUPS IF THE FIRST WORD INDICATES
        (| (or (and (nextword? N 'TO)                                 ;; THE POSSIBILITY OF A RANK-SHIFTED CLAUSE
                    (parse 'CLAUSE 'RSNG 'TO 'SUBJ))                    ;; SERVING AS THE SUBJECT, THEN TRY TO PARSE ONE
                (and (parse 'CLAUSE 'RSNG 'ING 'SUBJ)))                 ;; AS SUCH FEATURE "SUBJ" INSURES A CHECK THAT ANY
            SUBREG                                                  ;; PRONOUNS FOUND ARE IN SUBJECTIVE CASE.
            nil
            SUBJ1)
    SUBJ4
        (| (parse 'NG 'SUBJ) SUBREG nil SUBJ1)                        ;; IF PARSING THE SUBJ CAUSES THE CUT POINT TO BE
                                                                    ;; REACHED, THEN JUMP TO "SUBJ1" TO SEE IF WE ARE
                                                                    ;; IN CONDITIONS WHERE THAT IS ALLOWED

        ;; WHAT TO DO IF THE SUBJECT CANNOT BE DIRECTLY PARSED?  THIS IS CHECKING FOR THE SITUATION WHERE A QUESTION
        ;; WORD IS ACTING AS LOGICAL SUBJECT AND HAS ALREADY BEEN PARSED AS IN "WHAT IS IN THE BOX?"  THE CLAUSE WILL
        ;; HAVE THIS FEATURE IF IT IS ACTIVE AS A RSQ AND ITS MISSING ELEMENT HAS NOT YET BEEN DETERMINED.  SINCE WE
        ;; CANNOT FIND ANY SUBJECT, WE ASSUME THAT IT IS A SUBJECT-RELATIVE IN THIS CASE.

        (COND ((cq 'REL-NOT-FOUND *fe*)
                (RQ REL-NOT-FOUND)
                (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)
                (GO VB))
            (SUBJ-VB-BACKUP-TYPE1
                (SETQ SUBJ-VB-BACKUP-TYPE1 nil)
                (GO SUBJ11))                                        ;; SEE THE LARGE NOTE ABOUT THIS IN "NOVERB".
            ((and H (ISQ H TIME) (ISQ H NG))
                (setr 'SUBJECT H *c*)
                (GO VB))                                            ;; WHAT WAS INITIALLY PARSED AS A TIME-NG MODIFING
            ((MOVE-PT 'C 'U '(REL-NOT-FOUND))                          ;; THE WHOLE CLAUSE MAY PROBABLY BEEN THE SUBJECT
                                                                    ;; OF THE CLAUSE THIS WORRIES ABOUT RELATIVE
                                                                    ;; CLAUSES. PLEASE NOTE THAT THE CURRENT
                                                                    ;; HALF-VERSION HAS NOT YET GOT ITS HEAD TOGETHER
                                                                    ;; ABOUT RELATIVE CLAUSES.  IE. THE CODE ISN'T
                (setr 'SUBJECT (getr 'RELHEAD *pt*) *c*)                ;; DEBUGGED AND HAS GAPS IN IT ESP. WHO SETS WHAT
                (setr 'RELHEAD (getr 'RELHEAD *pt*) *c*)                ;; REGISTER WHEN THIS WILL BE FIXED BEFORE THE
                (remove-f-pt 'REL-NOT-FOUND *pt*)                     ;; VERSION IS FINALIZED
                (GO VB))
            ((and (cq 'COMPONENT *fe*) NN) (FQ SUBJFORK) (GO VB))         ;; "SARAH ATE DINNER AND WENT TO THE MOVIES."
            (H (POP) (GO SUBJ))                                     ;; POP OFF THE CLOSEST INITIAL MODIFIER AND TRY TO
            ((GO FAIL)))                                            ;; PARSE A SUBJ AGAIN

    HEAD
        (| (or (MOVE-PTW 'N 'PW '(NOUN)) (MOVE-PTW 'N 'PW '(PRON)))       ;; COME HERE (ONLY?) TO TRY TIME PHRASE AS SUBJECT
            nil
            (HEAD))                                                 ;; MOVE PTW TO THE CLOSEST NOUN THEN SET THE CUT

    SUB2
        (| (POP) nil FAIL)                                          ;; POINT TO IT AND ATTEMPT A NEW PARSING IF
        (| (CUT *ptw*) INIT SUB2)                                     ;; NOTHING MORE TO POP, LOSE

    SUBJ1
        (COND ((ISQ H QUOTED)                                       ;; CIRCUMSTANCES UNDER WHICH IT IS ALLRIGHT TO
            (and (ISQ H LIST) (FQ LIST))                            ;; HAVE NOTHING FOLLOWING THE SUBJECT OF THE
            (FQ QUOTED)                                             ;; CLAUSE "  "MUMBLE", SAID JOHN."
            (SETQ H (H H))
            (GO RETSM)))
        (and (cq 'REL-NOT-FOUND *fe*)                                     ;; THIS IS PART OF A BACKUP MECHANISM WHICH NEEDS
            (MOVE-PT 'H 'PV '(QAUX))                                   ;; TO BE MORE THROUGHLY THOUGHT OUT. THE SITUATION
            (COND
                ((ISQ *pt* BE)                                        ;; IS EXPLAINED IN DETAIL IN QUESTION.NGQST MOVE
                    (FQ INT AUXBE)                                  ;; PT TO A VERB WHICH CAN BE AN AUXILLIARY AND
                    (RQ REL-NOT-FOUND)                              ;; WHICH CAN BEGIN A CLAUSE
                    (setr 'COMP (getr 'RELHEAD *c*) *c*)
                    (setr 'SUBJECT H *c*)                             ;; "WHAT COLOR IS THE BLOCK?" OR "HOW BIG IS THE BLOCK?"
                    (setmvb *pt*)
                    (GO ONT))
                ((ISQ *pt* HAVE)
                    (FQ SUBQ)
                    (RQ REL-NOT-FOUND)
                    (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)
                    (GO VBL))))

    SUBJ11
        (| (CUT-BACK-ONE) SUBJ3 (SUBJ11))                           ;; IF WE CAN'T CUT BACK ANY FURTHER, THEN FAIL

    SUBREG
        (setr 'SUBJECT H *c*)                                         ;; THIS SETS THE "SUBJECT" REGISTER OF THE CURRENT
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

    VB  (| (parse 'ADJG 'ADV 'VBAD) VB nil (VB-ADJG))                  ;; PARSE ANY INITIAL MODIFIERS
        (RQ VBLOK)                                                  ;; ?????

    VBL (| (parse 'VG) VBREG nil)                                    ;; ONCE THE VERB GROUP IS PARSED, SET THE REGISTER

    NOVERB
        (COND                                                       ;; WHAT TO DO IF THE VG CANNOT BE DIRECTLY PARSED?
            ((cq 'SUBJFORK *fe*) (FQ VBFORK) (GO FINDOBJ1))
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
        (setr 'VG H *c*)

    ;; ###############################################################
    ;;
    ;;             PARSE ANY OBJECTS REQUIRED BY THE VERB
    ;;
    ;; ###############################################################

    VG1 (CUT *end*)                                                   ;; RESET THE CUTPOINT IN CASE ANYONE CHANGED IT
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
        (| (and (nq 'PRT) (parse 'PRT)) nil DPRT)                             ;; IF THE PARTICLE IS NOT THE WORD FOLLOWING THE VERB, THEN
        (FQ PRT)                                                            ;; IT IS SEARCHED FOR BY CODE AT "DPRT" (DISPLACED PARTICLE)

        (| (setmvb (COMBINATION? (root (NB MVB)) (word (NB H))))            ;; IS THIS A LEGITIMATE COMBINATION OF VERB AND PARTICLE?
            CHECKPASV
            POPRT)

    DPRT
        (| (ISQ H PASV) CHECKPASV nil)                                      ;; SEARCH FOR DISPLACED PARTICLE.  NO DISPLACED PARTICLES
        (| (SETQ POSITION-OF-PRT (MOVE-PTW 'N 'NW '(PRT))) nil FINDOBJ1)       ;; IN PASV'S IF NOT FOUND ASSUME THAT IT IS OPTIONAL AND
        (| (setmvb (COMBINATION? (root (NB MVB)) (word POSITION-OF-PRT)))   ;; WE ARE DEALING WITH THE CASE WITHOUT THE PARTICLE
            nil
            POPRT)
        (| (ISQ MVB TRANS) nil FINDOBJ1)
        (CUT POSITION-OF-PRT)
        (| (parse 'NG 'OBJ 'OBJ1)                                              ;; PARSE UP ANY NOUN GROUP YOU FIND
            POPRT
            FINDOBJ1                                                        ;; IF THERE ARE MORE OR LESS NP'S THAN EXPECTED,
            nil)                                                            ;; THEN DON'T PARSE ANYTHING BUT GO TO NPRT
        (CUT *end*)                                                           ;; INSTEAD. SIMILARLY, IF ANYTHING FOLLOWS THE
        (setr 'OBJ1 H *c*)                                                    ;; DISPLACED PARTICLE THEN A GRAMMATICALLY BAD
        (parse 'PRT)                                                         ;; FORM IS ASSUMED AND THE PIECES POPED OFF
        (FQ PRT DPRT)
        (GO FINDOBJ2)

    POPRT
        (POPTO VG)
        (GO FINDOBJ1)

    CHECKPASV                                                               ;; CHECK THE VERB FOR THE PASSIVE CONSTRUCTION
        (| (and (ISQ H PASV) (FQ PASV) (setr 'OBJ1 (getr 'SUBJECT *c*) *c*))
            FINDOBJ2
            nil
            FINDFAKE2)
        (FQ ACTV)                                                           ;; NOT PASV=ACTIVE
        (GO FINDOBJ1)

    BE
        (FQ BE)
        (and (parse nil 'NOT) (FQ NEG))
        (parse 'ADV 'VBAD)

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
        (| (MOVE-PT 'C 'U '(REL-NOT-FOUND)) OBJ1REL nil)
        (| (and (CANTAKE 1 '(PREPG LOC) 'ITRNSL)
                (MOVE-PT 'C 'U '(QADJ))
                (ISQ (getr 'QADJ *pt*) PLACE)
                (FQ ITRANSL))
            PUTLOBJ
            nil)
        (| (CANPARSE 1 nil 'ITRNS) ONT nil)

    GOOF1
        (or *global-message* (bug "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)

    OBJ1REL
        (setr 'OBJ1 (getr 'RELHEAD *pt*) *c*)
        (remove-f-pt 'REL-NOT-FOUND *pt*)
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
        (| (and (ISQ MVB TRANS2) (MOVE-PT 'C 'U '(REL-NOT-FOUND)))
            OBJ2REL
            nil)
        (| (and (CANTAKE 2 '(PREPG LOC) 'TRANSL)
                (MOVE-PT 'C 'U '(QADJ))
                (ISQ (getr 'QADJ *pt*) PLACE)
                (FQ TRANSL))
            PUTLOBJ
            nil)

    OBJ2TO
        (parse 'ADV 'VBAD)
        (| (COND ((and (nextword? N 'TO)
                    (ISQ MVB TO2)
                    (parse 'PREPG 'TO))                               ;; THE SECOND-OBJECT THAT WE HAVE BEEN LOOKING FOR
                (setr 'OBJ2 (getr 'OBJ1 H) *c*)                       ;; MAY BE A PREPG AS IN "GIVE IT TO THE LIONS"
                (FQ TRANS2TO TRANS2))                               ;; TAKES THE OBJECT OF THE PREPOSITION "TO" AND
            ((and (cq 'PREPQ *fe*)                                        ;; MAKES IT THE OBJ2 OF THE CLAUSE.
                (MOVE-PT 'H 'PV '(QUEST))
                (= (word (MOVE-PTW 'FW)) 'TO)
                (RQ PREPQ)
                (FQ TRANS2TOQ TRANS2)
                (setr 'OBJ2 (getr 'OBJ1 *pt*) *c*))))                   ;; "TO WHOM DID YOU GIVE THE MEAT?"
            ONT
            nil)
        (| (CANPARSE 2 nil 'TRANS) ONT FAIL)

    PUTLOBJ
        (setr 'LOBJ *pt* *c*)
        (setr 'RELHEAD (getr 'QADJ *pt*) *pt*)
        (setr 'QADJ nil *pt*)
        (remove-f-pt 'QADJ *pt*)
        (GO ONT)

    OBJ2REL
        (setr 'OBJ2 (getr 'RELHEAD *pt*) *c*)
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (FQ OBJ2REL)
        (GO ONT)

    FIXSUBJECT
        (setr 'SUBJECT (getr 'OBJ1 *c*) H)
        (GO ONT)

    CHECKIT                                                         ;; CHECK FOR THE POSSIBILITY THAT THE SUBJECT WAS
        (| (= (word (NB (getr 'SUBJECT *c*))) 'IT)                   ;; A DUMMY FUNCTION WORD ("IT"), AS IN "IT WAS NICE TO SEE HIM."
            nil
            ONT)                                                    ;; TO BE ADDED HERE: "JOHN WAS EAGER/EASY TO PLEASE."
        (| (or (and (nextword? N 'TO) (parse 'CLAUSE 'RSNG 'TO 'SUBJ))
                (and (nq 'ING) (parse 'CLAUSE 'RSNG 'ING 'SUBJ))
                (parse 'CLAUSE 'REPORT))
            nil
            ONT)
        (FQ IT)
        (setr 'LOGICAL-SUBJECT H *c*)                                 ;; THE CLAUSE IS THE REAL SUBJECT.
        (GO ONT)

    GOOF2
        (or *global-message* (bug "NEW TRANSITIVITY - SECOND OBJECT"))
        (GO FAIL)

    ;; ###########################################################################################
    ;;
    ;;                               INITIAL SEMANTIC PROCESSING
    ;;
    ;; ###########################################################################################

    ONT
        (| (cq 'PASV *fe*) PONT nil)
    ONT1
        (| (CALLSM (SMCL1)) nil (SMCL1))

        (| (not (cq 'REL-NOT-FOUND *fe*)) TONT nil RETSM)                 ;; IF THE FEATURE "REL-NOT-FOUND" IS PRESENT AT
                                                                    ;; THIS POINT, IT INDICATES THAT WE ARE IN A
        (| (ISQ (getr 'HEAD (getr 'RELHEAD *c*)) TIM1)                ;; RELATIVE CLAUSE AND MAY HAVE TO DO SOME
            nil                                                     ;; GYMNASTICS IF THE CLAUSE IS NOT TO FAIL
            PREPSHORT)                                              ;; MOVE BACK TO A QUESTION-NOUNGROUP, THEN DOWN

    TIMEQ
        (RQ REL-NOT-FOUND)                                          ;; AND BACK TO THE NOUN. IF THAT NOUN IS "TIM1"
        (FQ TIMEQ)                                                  ;; THEN ASSUME WE HAVE FOUND OUR RELATIVE ELEMENT.
        (GO TONT)

    PREPSHORT
        (| (and (nq 'PREP) (parse 'PREPG)) nil (ONT-SHORT-PREP))
        (| (CALLSM (SMRELATE H)) nil (ONTß SMRELATE PREPQ))
        (| (cq 'REL-NOT-FOUND *fe*) PREPSHORT TONT (ONT-NOT-FOUND))       ;; WE HAVE A PREP TO TAKE THE UNATTACHED RELATIVE
                                                                    ;; AS ITS OBJECT. THE FEATURE REL-NOT-FOUND WILL
                                                                    ;; BE REMOVED IF THE PREPG DISCOVERS IT CAN'T FIND

    PONT
        (and (nextword? N 'BY) (parse 'PREPG 'AGENT) (FQ AGENT))        ;; AN OBJECT (THE REMOVING WILL BE DONE WHILE IN PREPG).
        (setr 'LOGICAL-SUBJECT (getr 'OBJ1 H) *c*)                    ;; "LOGICAL" IE. SUBJECT IN RELATIONSHIP
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
        (| (and (nq 'PREP) (parse 'PREPG) (CALLSM (SMRELATE H)))                                  ;; PREPG
            nil
            nil
            RETSM)
        (| (and (nq 'TIMW) (parse 'ADV 'TIMW) (or (CALLSM (smtime)) (GO FAIL)))                    ;; TIMW
            nil
            nil
            RETSM)
        (| (and (not (cq 'BE *fe*)) (parse 'ADJG 'ADV) (or (CALLSM (SMRELATE H)) (GO FAIL)))            ;; ADV
            nil
            nil
            RETSM)
        (| (and (parse 'NG 'TIME) (or (CALLSM (smtime)) (GO FAIL)))                               ;; TIME NOUN GROUP
            nil
            nil
            RETSM)
        (| (and (nq 'PLACE) (parse 'ADV 'PLACE) (or (CALLSM (smplace)) (GO FAIL)))                 ;; PLACE
            nil
            nil
            RETSM)
        (| (and (nq 'BINDER) (parse 'CLAUSE 'BOUND) (or (CALLSM (SMBIND)) (GO FAIL)))              ;; BINDER
            nil
            nil
            RETSM)
        (| (and (nextword? N 'TO) (parse 'CLAUSE 'TO 'ADJUNCT) (or (CALLSM (smtoadj)) (GO FAIL)))    ;; TO CLAUSE (ADJUNCT)
            nil
            nil
            RETSM)
        (| (= N POSITION-OF-PTW) nil TONT RETSM)                   ;; LOOP UNTIL NOTHING ELSE CAN BE PARSED.
        (| (or (not (cq 'TOPLEVEL *fe*)) (nq 'SPECIAL)) RETSM nil)         ;; SPECIAL WORD (E.G. COMMA AND) COULD INDICATE
        (ert "CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL")              ;; A CONJUNCTION OR A BINDER.
        (GO FAIL)

    ;; ##############################################################################
    ;;                                   THERE
    ;;             AS IN:  "THERE IS A BIRD SITTING ON YOUR SHOULDER"
    ;; ##############################################################################

    THERE
        (FQ THERE)
        (CUT *end*)
        (| (parse 'ADV 'TIMW) nil nil (THERE))                        ;; "THERE IS A BIRD.."
        (| (and (parse 'VG) (ISQ MVB BE)) THEF NOTHE (THERE))

    THERQ
        (| (ISQ (MOVE-PT 'H 'PV '(QAUX)) BE) THERQ2 nil)               ;; IF THIS FAILS, THE THERE IS CONSIDERED TO BE
        (| (and (nq 'TIMW) (parse 'ADV 'TIMW)) nil nil (THEREQ))
        (| (and (parse 'VG) (ISQ MVB BE)) THERQ2 nil)
        (RQ POLR2)
        (GO NOTHE)

    THERQ2
        (FQ SUBJTQ) (FQ THERE)
        ;; THIS MAY NOT INTERFACE PROPERLY WITH THE SEMANTIC ROUTINES FOR BE
        (| (cq 'POLAR *fe*) THEF ONT)

    THEF
        (| (and (nq 'ADV) (parse 'ADV 'TIMW)) nil nil (THEF))
        (| (parse 'NG 'SUBJ 'SUBJT) nil THERREL)
        (FQ THERE)
        (setr 'SUBJECT H *c*)
        (GO ONT)

    THERREL
        (| (MOVE-PT 'C 'U '(REL-NOT-FOUND)) nil NOTHE)
        (setr 'SUBJECT (getr 'RELHEAD *pt*) *c*)
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (GO ONT)

    NOTHE
        (RQ THERE)
        (POP THERE)
        (and (nq 'ADV) (parse 'ADV 'PLACE))
        (GO THER2)

    ;; ####################################################################
    ;;                            IMPERATIVES
    ;; ####################################################################

    IMPER
        (| (parse 'NG 'TIME) nil nil IMPOP)                           ;; MODIFIERS WHICH MAY PRECEED THE VERB
        (| (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD)) nil nil IMPOP)
        (| (and (nq 'ADV) (parse 'ADV 'TIMW)) nil nil IMPOP)

    IMPE
        (| (parse 'VG 'IMPER) nil IMPOP)
        (FQ IMPER)
        (GO VG1)

    IMPOP
        (| (POP nil) IMPE (IMPOP))

    ;; ####################################################################
    ;;                             QUESTIONS
    ;; ####################################################################

    QUEST ;; PREP QUESTION
        (FQ QUEST)
        (| (nq 'PREP) nil NGQUES)
        (| (parse 'PREPG) nil NGQUES (PREPQ-INCOMPLETE))             ;; "ON WHICH BLOCK DID YOU PUT IT?"
        (| (ISQ H QUEST) nil QUEST)                                 ;; IF THE PREPG ISN'T THE QUESTION, TRY AGAIN "ON
        (setr 'QADJ H *c*)                                            ;; THAT DAY, WHERE DID YOU GO?" -- MAYBE WE COULD
                                                                    ;; MAKE USE OF THE COMMA CLUE. PREPQ IS HANDLED
        (GO POLAR)                                                  ;; MUCH LIKE QADJS LIKE WHEN AND WHERE THE REST OF
                                                                    ;; THE QUESTION HAS THE SAME SYNTAX AS A POLAR (YES-NO).
    NGQUES ;; NOUN GROUP QUESTION
        (| (parse 'NG 'QUEST) NGQST nil)                              ;; "WHICH ONE IS THE MURDURER?"
        (| (or (and (nextword? N 'HOW)
                (parse 'ADJG 'QUEST)
                (setr 'RELHEAD H *c*))                                ;; "HOW BIG...."
            (and (nq 'QADJ)
                (parse 'QADJ)
                (FQ QADJ)
                (setr 'QADJ H *c*)))                                  ;; "WHAT...?",  "WHERE...?"
            POLAR
            POLAR
            nil)
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))                                      ;; IF ALL THE SENTENCE CONSISTS OF IS THE QUESTION

    ADJQS
        (GO RETURN)                                                 ;; ADJECTIVE THEN WE SHOULD RETURN DIRECTLY

    NGQST
        (setr 'RELHEAD H *c*)

    NGQST2
        (CUT *end*)
        (setr 'SUBJECT H *c*)
        (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD))

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

        (COND ((parse 'VG 'NAUX) (FQ SUBJQ) (GO VG1))
            ((nq 'VB) (FQ REL-NOT-FOUND) (GO POLAR))
            (:else (MOVE-PTW 'N 'PW)
                (POP NG QUEST)
                (CUT *ptw*)
                (GO NGQUES)))                                       ;; POP BACK AND START FIGURING OUT THE QUESTION

    QUEST2                                                          ;; ALL OVER AGAIN
        (| (and (nextword? N 'THERE) (parse nil 'THERE))
            THERQ
            SUBF)                                                   ;; "ARE THERE....?"

    SUBF
        (| (parse 'NG 'SUBJ)                                          ;; PARSE THE SUBJECT OF ANYTHING LIKE: "DID THE
                                                                    ;; WOMAN GET THE JOB?" IF SUCCESSFUL, CONTINUE AT
                                                                    ;; "SUBREG" IN THE NORMAL PART OF THE CLAUSE
                                                                    ;; PROGRAM (RESETTING THE SUBJECT REGISTER)  (THE
            SUBREG                                                  ;; BEGINNING OF THE VERB GROUP SECTION). "SUBJ1"
            nil                                                     ;; WORRIES ABOUT WHAT SHOULD HAPPEN IF THE SUBJECT
            SUBJ1)                                                  ;; SEEMS TO FINISH THE SENTENCE
        (RQ REL-NOT-FOUND)
        (GO BE)

    POLAR
        (| (and (nq 'VB) (parse 'VB 'AUX '(QAUX)) (setr 'QAUX H *c*) (CALLSM (SMVAUX)) (setmvb H))
            nil
            QCHOP)
        (or (cq 'QADJ *fe*) (getr 'RELHEAD *c*) (FQ POLAR))
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
        (COND ((cq 'BOUND *fe*) (GO BOUND))                               ;; CHECK INITIAL FEATURES AND JUMP ACCORDINGLY
            ((cq 'TO *fe*) (GO TO))
            ((cq 'RSQ *fe*) (GO RSQ))
            ((cq 'REPORT *fe*) (GO REPORT))
            ((cq 'ING *fe*) (GO ING))
            (:else (mq! 'RSNG-TYPE) (GO FAIL)))

    BOUND ;; BINDER
        (| (parse 'BINDER) nil (BOUND) (BINDER))
        (SETQ LOCATIONMARKER N)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)                                                   ;; "FDEC" IS NEAR THE TOP OF THE MAJOR CLAUSE

    RSQ
        (setr 'RELHEAD (MOVE-PT 'C 'U '(NG)) *c*)
        (| (cq 'PREPREL *fe*) nil RSQ2)
        (parse 'PREPG 'PRONREL)                                       ;; THIS CALL IS BASED ON INFORMATION PASSED FROM
        (setr 'QADJ H *c*)                                            ;; FAR AWAY AND EXPLAINED IN DETAIL IN THE CODE
        (GO REPORT)                                                 ;; FOR PREPOSITION GROUPS

    RSQ2
        (COND ((parse 'VG 'EN 'PASV)                                   ;; HAVING DETERMINED THAT THE VERB IS PASSIVE IF
                (or (ISQ MVB TRANS) (GO FAIL))                      ;; IT WERE NOT ALSO TRANSITIVE, THEN WE WOULDN'T
                (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)                 ;; KNOW WHAT TO DO WITH WHATEVER WAS PARSED AS A
                (GO VG1))                                           ;; SUBJECT - SO WE FAIL
            ((parse 'VG 'ING)
                (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)
                (GO VG1))
            ((nq 'PRONREL) (parse 'NG 'RELWD) (GO REL))
            ((cq 'COMPONENT *fe*)                                         ;; IN A COMPONENT RELATIVE THE RELWD MIGHT BE IN
                (setr 'RELHEAD                                      ;; THE FIRST CLAUSE.
                    (getr 'RELHEAD (MOVE-PT 'C 'PC))
                    *c*)                                              ;; MAKE RELHEAD SAME AS PREVIOUS COMPONENT RSQ.
                (GO REL))
            ((parse 'NG 'SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
            (:else (GO FAIL)))                                          ;; THIS REALLY ISN'T AN RSQ

    REL
        (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)
        (| (parse 'VG) VG1 nil)                                      ;; OUR FIRST HYPOTHESIS, THAT THE SUBJECT WAS THE
                                                                    ;; RELWORD, WAS JUST PROVEN WRONG SINCE WE CANNOT
                                                                    ;; PARSE THE VG NEXT. SO WE REVISE OUR FEATURES
        (FQ REL-NOT-FOUND)                                          ;; AND JUMP TO PARSE A REAL FULL SUBJECT AS IN
        (GO SUBJ)                                                   ;; "...WHICH MARY THOUGHT WAS CHAUVANISTIC" AS
                                                                    ;; OPPOSED TO "...WHICH WAS CHAUVANISTIC"

    TO
        (| (and (cq 'COMPONENT *fe*) (parse 'VG 'TO 'TODEL)) VG1 nil)        ;; "I WANTED TO DANCE AND SING"
        (| (nextword? N 'FOR) nil TO1)                                ;; THIS IS EXPERIMENTAL
        (parse nil 'FOR)                                             ;; PLEASE CHECK OUT ANY FOR-CLAUSES YOU CAN THINK OF
        (FQ FOR)
        (parse 'NG 'SUBJ 'TOSUBJ)
        (setr 'SUBJECT H *c*)

    TO1
        (| (parse 'VG 'TO) VG1 (TO))

    ING
        (| (MOVE-PTW 'N 'NW '(ING)) nil FAIL)
        (| (or (nq 'ING) (cq 'OBJ2 *fe*) (and (parse 'NG 'SUBJ 'INGSUBJ) (setr 'SUBJECT H *c*) (FQ SUBING) (RQ ING)))
            nil
            nil
            (ING))
        (| (parse 'VG 'ING) VG1 (ING))

    REPORT
        (and (nextword? N 'THAT) (parse nil 'THAT) (FQ THAT))
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
        (COND ((cq 'RELWD *fe*) (GO RELWD))                               ;; CORRESPONDING SPECIAL BLOCKS OF CODE
            ((cq 'QUEST *fe*) (GO QUEST))
            ((or (nq 'QDET) (nq 'QPRON)) (FQ QUEST) (GO QUEST))
            ((cq 'TIME *fe*) (GO TIME))                                   ;; LOOK AT FIRST WORD
            ((nq 'PROPN) (GO PROPN))
            ((nq 'TPRON) (GO TPRON))
            ((nq 'EVERPRON) (GO EVERPRON))
            ((nq 'PRON) (GO PRON)))

    LOOK
        (COND ((nq 'DET) (GO DET))                                   ;; THIS POINT MAY BE JUMPED BACK TO
            ((nq 'NUM) (GO NUM))
            ((or (nq 'ING) (nq 'EN) (nq 'ADJ)) (GO ADJ))
            ((nq 'CLASF) (GO CLASF))
            ((nq 'NUMD) (GO NUMD))
            ((nextword? N 'AT) (GO AT))
            ((nextword? N 'AS) (GO AS))
            ((nq 'NOUN) (GO NOUN))
            ((nq 'TIMORD) (GO TIMORD))
            ((and (cq 'COMPONENT *fe*) (ISQ (MOVE-PT 'PC) QUEST)) (GO QUEST))
            ((mq! 'START) (GO FAIL)))

    ;; #######################################################
    ;; IF YOU CAN PARSE ANY OF THESE SMALL THINGS, YOU'RE DONE
    ;; #######################################################

    START                                                           ;; PARSE A PROPER NOUN

    PROPN
        (parse 'PROPN)
        (FQ DEF PROPNG)
        (| (ISQ H POSS) PROPS nil)
        (| (and NN (nq 'PROPN)) PROPN nil)

    PROPS
        (or (CALLSM (SMPROP)) (GO FAIL))                            ;; EXAMINE ITS SEMANTICS
        (| (ISQ H POSS) POSS PRAG)

    ;; -------------- PRONOUNS ---------------

    PRON
        (| (parse 'PRON 'POSS) POSS nil RED2)                         ;; IS IT POSSESSIVE?

    PRON2
        (| (cq 'NPRON *fe*) (NPRON) nil)
        (| (or (and (cq 'SUBJ *fe*) (parse 'PRON 'SUBJ))                    ;; CHECK SUBJECTIVE OR OBJECTIVE CASE
                (and (or (cq 'OBJ *fe*) (cq 'TOSUBJ *fe*) (cq 'INGSUBJ *fe*)) (parse 'PRON 'OBJ))
                (cq 'INGSUBJ *fe*))
            nil
            (PRON))
        (FQ PRONG DEF)

    PRON3
        (| (CALLSM (SMPRON H)) nil FAIL)                            ;; EXAMINE SEMANTICS OF PN

    PRAG
        (setr 'HEAD H *c*)
        (MOVE-PT 'H)
        (TRNSF NS NPL NFS NEG)                                      ;; MODIFY PN FEATURES TO CORRECT NUMBER
        (GO RETURN)

    ;; -------------- ANYTHING, SOMETHING, ... --------------

    TPRON
        (parse 'TPRON)
        (FQ TPRON)
        (MOVE-PT 'H)
        (TRNSF NS NPL ANY NEG)
        (setr 'HEAD *c* H)
        (and NN (nq 'ADJ) (parse 'ADJ))
        (GO SMNG)

    ;; ----------- WHATEVER, WHENEVER, WHOEVER, ... -----------

    EVERPRON
        (| (and (parse 'PRON 'EVERPRON) (CALLSM (SMPRON H)))
            nil
            FAIL)
        (| (and (parse 'CLAUSE 'RSQ 'NOREL) (CALLSM (SMRELATE H)))
            RETSM
            FAIL)

    AS  (| (and (parse nil 'AS) (parse 'NUMD 'NUMDAS) NN (parse nil 'AS))
            NUMD2
            (AS)
            (AS))

    ;; -------------- AT + NUM ---------------

    AT
        (| (and (parse nil 'AT) (parse 'NUMD 'NUMDAT)) nil (AT) (AT))
    NUMD2
        (| (and (parse 'NUM) (FQ NUM NUMD)) DET1 (NUMD2) INCOM)

    ;; -------------- OTHER NUMBER WORDS ---------------

    NUMD
        (| (parse 'NUMD 'NUMDAN) nil ND3 INCOM)
        (| (parse nil 'THAN) NUMD2 INCOM POPCOM)
    ND3
        (| (parse 'NUMD 'NUMDALONE) NUMD2 (NUMD) (NUMD))

    ;; -------------- TIME WORDS ---------------

    TIME
        (| (and (nq 'TIME) (parse 'NOUN 'TIME)) RETSM nil)
        (| (MOVE-PTW 'N 'NW '(TIM1)) LOOK (TIME))

    TIMORD
        (| (parse 'ORD 'TIMORD) nil FAIL)
        (| (and (parse 'NOUN 'TIM1) (FQ DET DEF) (CALLSM (SMNGTIME)))
            RETURN
            FAIL)

    ;; #################################################
    ;;     THE MAINSTREAM - MORE CMPLICATED NG TYPES
    ;; #################################################

    ;; -------------- PARSE A DETERMINER ---------------

    DET
        (parse 'DET)
        (FQ DET)
        (MOVE-PT 'H)                                                 ;; SHIFT PTR TO THE DETERMINER
        (| (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR)
            IND
            (bug nil)
            INCOM)

    ;; -------------- INDETERMINATE ---------------

    IND
        (| (and (= (word (NB H)) 'ALL) (= (word N) 'THE) (parse 'DET) (FQ DEF))
            NUM
            nil
            (THE))
        (| (and (ISQ H QNTFR) (FQ QNTFR)) QNUM nil)

    ;; -------------- ORDINALS AND NUMBERS ---------------

    ORD
        (| (and (parse 'ORD) (FQ ORD)) nil NUM INCOM)
        (| (and (nextword? N 'OF)                                     ;; TWELTH OF OCTOBER...
                (ISQ (MOVE-PTW 'N 'NW) MONTH)
                (parse nil 'OF)
                (parse 'NOUN 'MONTH)
                (FQ DATE))                                          ;; REMEMBER THAT FEATURES ARE DESIGNED AS AIDS TO
            RETSM                                                   ;; SEMANTIC COMPREHENSION AS WELL AS SYNTACTIC PARSING.
            nil)
        (| (cq 'DEF *fe*) nil ADJ)

    NUM
        (| (parse 'NUM) nil ADJ)                                     ;; LARGE JUMP IF FALSE
        (FQ NUM)
        (| (cq 'DET *fe*) nil DET1)
        (| (COND ((and (ISQ H NS) (cq 'NS *fe*)) (RQ NPL PART)) ((cq 'NPL *fe*) (RQ NS PART)))
            ADJ
            (NUM)
            INCOM)

    DET1
        (if (ISQ H NS) (FQ NS) (FQ NPL))                    ;; EXPLICIT CHECK FOR THE VALUE 1
        (or NN (and (FQ NUMBER) (GO INCOM)))

    NUMBER
        (FQ DET)
        (| (nq 'OF) OF ADJ)

    QNUM
        (| (ISQ H NONUM) OF nil)
        (| (and (parse 'NUM) (FQ NUM)) nil OF)
        (| (COND ((== (SM H) 1) (and (cq 'NS *fe*) (RQ NPL))) ((cq 'NPL *fe*) (RQ NS))) ;; EXPLICIT CHECT FOR THE VALUE 1
            nil
            (NUMD)
            INCOM)
        (| (= (word (NB H)) 'NO) ADJ nil)                          ;; CHECKS FOR WORD "NO"

    ;; -------------- PREPG WITH "OF" ---------------

    OF  (| (and (nq 'OF) (parse 'PREPG 'OF)) SMOF NONE)                ;; "FIVE OF THE BLOCKS"

    SMOF
        (FQ OF)
        (| (or (CALLSM (SMNGOF)) (not (POP))) RETSM INCOM)

    NONE
        (| (= (word (NB H)) 'NONE) INCOM ADJ)

    ;; ----------- PARSE ALL THE ADJECTIVES -----------

    ADJ
        (| (parse 'ADJ) nil EPR INCOM)
        (and (ISQ H COMPAR)
            (FQ COMPARATIVE-MODIFIER)
            (setr 'COMPARATIVE-MODIFIER H *c*))
        (GO ADJ)

    EPR
        (| (or (ISQ H SUP) (ISQ H COMPAR)) nil CLASF INCOM)         ;; WE PARSED AN ADJ AND RAN OUT OF WORDS
        (FQ ADJ)
        (and (nextword? N 'OF)
            (parse 'PREPG 'OF)
            (or (CALLSM (SMNGOF)) (GO FAIL))
            (FQ OF)
            (GO RETSM))

    ;; -------------- PARSE ALL THE CLASIFIERS ---------------

    CLASF
        (| (or (parse 'VB 'ING '(CLASF))                               ;; TRIES TO PARSE THE LARGEST POSSIBLE NG FIRST
                (parse 'VB 'EN '(CLASF))
                (parse 'CLASF))
            CLASF
            nil
            REDUC)

    ;; -------------- AND FINALLY - THE NOUN ---------------

    NOUN
        (| (parse 'NOUN) nil RED2)

        (| (and (cq 'TIME *fe*) (not (ISQ H TIM1))) RED1 nil)

    ;; -------------- MODIFY FEATURES FOR NUMBER AND SUCH --------------

        (SETQ T1 *fe*)
        (COND ((and (ISQ H MASS) (or (cq 'PART *fe*) (not (cq 'DET *fe*))))
            (FQ MASS)))
        (COND ((not (ISQ H NPL)) (RQ NPL PART)))
        (COND ((not (ISQ H NS)) (RQ NS)))
        (COND ((and (not (cq 'DET *fe*)) (not (cq 'NUMD *fe*)))
            (MOVE-PT 'H)
            (TRNSF NPL MASS)))
        (| (MEET *fe* '(NS NPL PART MASS)) nil RED0)

        (| (nextword? N 'THAN) nil SMNG)                              ;; "...A BIGGER BLOCK THAN..."
        (FQ THAN)                                                   ;; THE PRESENCE OF THIS FEATURE IS NOTED BELOW AND IN ADJG

    ;; AT THIS POINT SMNG1 IS CALLED FOR PRELIMINARY CHECKS AND ANALYSIS BEFORE CHECKING QUALIFIERS

    SMNG
        (setr 'HEAD H *c*)                                            ;; SET HEAD REGISTER TO THE NOUN

        (| (and (cq 'OBOFJ *fe*) (not (cq 'DEF *fe*))) FAIL nil)                ;; JUST PARSED
        (or (CALLSM (SMNG1)) (GO FAIL))
        (| (not (ISQ H POSS)) nil POSS RETSM)                       ;; CHECK FOR POSSIVE

    ;; #################################################
    ;;               POSSIBLE QUALIFIERS
    ;; #################################################

        (| (and (cq 'THAN *fe*) (parse 'ADJG)) nil RSQ-TO)                 ;; "...A BIGGER BLOCK THAN..."
        (| (CALLSM (SMRELATE H)) RETSM FAIL)

    RSQ-TO
        (| (and (nextword? N 'TO) (MEET *fe* '(COMP SUBJ)) (parse 'CLAUSE 'RSQ 'TO) (or (CALLSM (SMRELATE H)) (GO POPRET)))
            RETSM
            nil)

    ;; -------------- AS OR COMPARATIVE ---------------

        (| (and (or (nextword? N 'AS) (nq 'COMPAR)) (parse 'ADJG 'THANNEED))
            nil
            PREPNG)                                                 ;; WHAT IS THE REASON FOR THE EXISTANCE OF THIS
        (and (nil? N)                                               ;; STRANGE ANIMAL (ALSO THE ONEBELOW) -- CHECK
            (cq 'SUBJ *fe*)                                               ;; THEM OVER AND HACK THEM PROPERLY
            (ISQ (MOVE-PT 'C 'PV) AUX)
            (ISQ *pt* BE)
            (GO POPRET))                                            ;; AVOIDS ATTACHING MODIFIER WHEN IT GOBBLES TO
        (| (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)               ;; MUCH E.G. IS THE BLOCK ON THE TABLE? DOESN'T
                                                                    ;; WANT "THE BLOCK ON THE TABLE" AS A CONSTITUENT.
                                                                    ;; I ADMIT ITS A HACK.

    ;; -------------- ANY SORT OR PREPOSITION GROUP --------------

    PREPNG
        (| (and (nq 'PREP)
                (not (or (and (nq 'PLACE) (cq 'NOLOC *fe*))
                    (and (cq 'OBJ1 *fe*)
                        (ISQ MVB TRANSL)
                        (not (ISQ (MOVE-PT 'C 'U) QUEST)))))
                (parse 'PREPG 'Q))
            nil
            DISGRSQ)
        (and (nil? N)
            (cq 'SUBJ *fe*)
            (ISQ (MOVE-PT 'C 'PV) AUX)
            (ISQ *pt* BE)
            (not (ISQ (MOVE-PT 'U) NGQ))
            (GO POPRET))
        (| (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)

    ;; CHECK FOR DISGUISED RSQ CLAUSES BY READING THE FAILURE MESSAGES SENT UP FROM PREPG.

    DISGRSQ
        (| (= (car *mes*) 'PREP-WHICH) nil RSQ)
        (set! *mes* (cdr *mes*))
        (| (parse 'CLAUSE 'RSQ 'PREPREL) PREPNG (RSQ-PREPREL) RETSM)

    ;; -------------- ANY OTHER RSQ ---------------

    RSQ
        (| (and (ISQ (MOVE-PT 'C 'U) POLR2) (cq 'SUBJ *fe*) (nq 'VB) (not (cq 'SUBJT *fe*)) (not (ISQ *pt* QADJ)))
            RETSM
            nil)
        (| (parse 'CLAUSE 'RSQ) nil RETSM)
        (| (CALLSM (SMRELATE H)) RETSM POPRET)

    ;; -------------------------------------------------
    ;; THE ENTIRE NG SHOULD HAVE BEEN PROCESSED BY THIS POINT
    ;; -------------------------------------------------

    ;; -------------------------------------------------
    ;; IF AT FIRST YOU DON'T SUCEED.......
    ;; -------------------------------------------------

    RED0
        (set! *fe* T1)
    RED1
        (POP)
    RED2
        (COND ((nil? H) (mq! 'NO) (GO FAIL))
           ((ISQ H NUMBER) (GO INCOM))
           ((and (ISQ H POSS) (or (ISQ H PRON) (and (MOVE-PT 'H 'DLC) (ISQ *pt* PRON))))
                (POP)
                (GO PRON2))
           ((and (nil? (cdr H)) (cq 'DEFPOSS *fe*)) (GO POSSDEF))
           ((and (cq 'QUEST *fe*) (nil? (cdr H))) (GO QDETCHECK))         ;; (CDR H) = T IF THERE IS ONLY ONE DAUGHTER TO THE CURRENT NODE
           ((ISQ H ADJ) (GO EPR))
           ((not (ISQ H CLASF)) (GO INCOM)))

    REDUC
        (POP)
        (| (and (nil? H) (nq 'PROPN)) PROPN NOUN)

    POPCOM
        (POP)

    ;; -------------- INCOMPLETE PHRASES ---------------

    INCOM
        (FQ INCOM)
        (| (and (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM)))
            RETURN
            nil)
        (| (and (nil? CUT) (cq 'NUM *fe*)) SMNG nil)

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
        (| (cq 'INGSUBJ *fe*) RETSM nil)
        ;; IF POSSESSIVE, ALL PREVIOUS MODIFIERS MODIFY THE POSSESSIVE NOUN, NOT THE NG HEAD
        (SETQ H (buildnode (reverse (cons 'POSS (SETDIF *fe* '(COMPONENT)))) NB N H SM))
        (set! *backref* (concat H (cdr *backref*)))
        (| (setr 'FEATURES (set! *fe* (concat '(POSES DET DEF NS NPL) (reverse REST))) *c*)
            nil
            (bug nil))
        (| (or (not NN) (ISQ H DEFPOSS)) nil ORD)

    POSSDEF ;; THE PLACEMENT OF THIS TAG IS A GUESS. THE ORIGINAL IS LOST, ASSUMING THAT IT EVER EXISTED
        (RQ POSES DET DEF)
        (FQ POSSDEF NS NPL)

    ;; -------------- RELATIVES---------------

    QUEST
        (| (parse nil 'HOW) nil QDET FAIL)
        (| (parse nil 'MANY) nil FAIL INCOM)
        (FQ DET NPL INDEF HOWMANY)
        (GO OF)

    QDET
        (| (and (parse 'DET 'QDET) (FQ DET NPL QDET NS))
            QNUM
            nil
            INCOM)

    QPRON
        (| (parse 'PRON 'QPRON) PRON3 FAIL)

    RELWD
        (| (and (parse 'PRONREL)
                (CALLSM (SMSET (SM (MOVE-PT 'C 'U 'U '(NG))))))         ;; SET SM TO THE NOUNGROUP DIRECTLY UPSTAIRS
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
        (set! *fe* (reverse REST))
        (SMSET nil)
        (GO NGSTART))

(§ PDEFINE VG (TENSE)

    ;; ##################################################################
    ;; CHECK INITIAL FEATURES TO SEE IF SOME SPECIAL TYPE OF VG IS WANTED
    ;; ##################################################################

    ENTERING-VG
        (COND ((cq 'TO *fe*) (GO TO))
            ((cq 'EN *fe*) (GO EN))
            ((cq 'ING *fe*) (GO ING))
            ((cq 'IMPER *fe*) (GO IMPER))
            ((ISQ (MOVE-PT 'C 'U) POLR2) (GO POLR2)))                 ;; CHECKS IF THE CLAUSE IS MARKED AS POLR2

    ;; -------------- DISPATCH TABLE FOR EXAMINEING THE FIRST WORD ---------------

    NEW                                                             ;; PARSE THE FIRST WORD WITH APPROPRIATE FEATURES
        (COND ((not (nq 'VB)) (mq! 'VB) (GO FAIL))                     ;; AND JUMP TO CODE THAT KNOWS WHAT SHOULD BE
            ((and (nq 'DO) (parse 'VB 'AUX 'DO)) (GO DO))               ;; LOOKED FOR NEXT IN EACH CASE
            ((and (nq 'MODAL) (parse 'VB 'AUX 'MODAL)) (GO MODAL))
            ((and (nq 'WILL) (parse 'VB 'AUX 'WILL)) (GO WILL))
            ((and (nq 'BE) (parse 'VB 'AUX 'BE)) (GO BE))
            ((and (nq 'HAVE) (parse 'VB 'AUX 'HAVE)) (GO HAVE))
            ((not (parse 'VB '(MVB))) (mq! 'VB) (GO FAIL)))

    SIMPLE
        (MOVE-PT 'C 'DLC)                                             ;; MOVE PT DOWN FROM THE CURRENT NODE BEING PARSED
        (TRNSF VPL INF V3PS)                                        ;; (VG) AND ACROSS TO THE MOST RECENTLY PARSED
        (SETQ TENSE (COND ((and (ISQ *pt* PRESENT) (ISQ *pt* PAST))     ;; DAUGHTER.  IN THIS CASE THAT DAUGHTER WAS PARSED
                    '(PAST-PRESENT))                                ;; IN THE DISPATCH TABLE JUST ABOVE
                ((ISQ *pt* PAST) '(PAST))
                (:else '(PRESENT))))
        (GO REV)

    TO
        (FQ NAGR)                                                   ;; "NAGR" MARKS THAT SUBJECT AND MAIN VERB NEED
        (| (and (parse nil 'NOT) (FQ NEG)) nil nil (NOT))            ;; NOT AGREE IN NUMBER AND PERSON AND INSURES THAT
        (| (or (parse nil 'TO) (cq 'TODEL *fe*)) nil (TO) (TO))            ;; THE AGREEMENT CHECKER AT THE END OF THE PROGRAM
                                                                    ;; ("REV") WILL NOT BE APPLIED "TODEL" MUST BE
        (SETQ TENSE '(INFINITIVE))                                  ;; GIVEN AS AN INITIAL FEATURE OR ELSE THIS
        (GO MODAL2)                                                 ;; STATEMENT FAILS TENSE IS USED TO HOLD THE TENSE
                                                                    ;; WHILE IT IS BEING COLLECTED.

    EN
        (FQ NAGR)
        (| (and (parse nil 'NOT) (FQ NEG)) nil nil (NOT))
        (SETQ TENSE '(PAST))
        (| (and (parse 'VB 'EN '(MVB)) (setmvb H) (FQ PASV)) RETSM FAIL) ;; DONE AT "EN2"

    ING
        (FQ NAGR)
        (| (and (parse nil 'NOT) (FQ NEG)) nil nil (NOT))

    INGADV
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) INGADV nil)
        (SETQ TENSE '(PRESENT))
        (GO BE2)

    IMPER
        (| (and (parse 'VB 'DO 'NEG 'INF) (FQ NEG)) nil nil (DONT))
        (| (and (parse 'VB '(MVB) 'INF) (setmvb H) (CALLSM (SMVG)))
            RETURN
            (IMPER))                                                ;; MVB IS BOUND BY CLAUSE

    POLR2                                                           ;; THE CLAUSE COULD ONLY BE MARKED AS "POLR2"
        (or (set! *pt* (getr 'QAUX (MOVE-PT 'C 'U)))                    ;; ("DID THE...?") IF AN AUX OF SOME VERIETY HAD
            (and (bug "VGßPOLR2") (GO FAIL)))                         ;; ALREADY BEEN PARSED, IF THAT IS NOT THE CASE,
        (SETQ H (list (car *pt*)))                                    ;; THEN WE HAVE A BUG IN THE PROGRAM SOMEWHERE SET
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
        (MOVE-PT 'C 'DLC)                                             ;; MOVE TO THE "DO"
        (TRNSF VPL NEG INF V3PS)                                    ;; ARRANGE ITS FEATURES
        (SETQ TENSE (if (ISQ *pt* PAST) '(PAST) '(PRESENT)))
        (if NN (GO DO2) (GO MVB))                                            ;; GO CONDITIONALY TO THE FIRST TAG IF MORE WORDS
                                                                    ;; REMAIN BEFORE THE CUT POINT, AND TO THE SECOND
                                                                    ;; TAG IF THERE ARE NONE

    DO2 (| (and (parse nil 'NOT) (FQ NEG)) nil nil (NOT))

    ADV2
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) ADV2 nil (ADV))
        (| (parse 'VB '(MVB) 'INF) nil MVB)                            ;; "MVB" ARRANGES FOR A CHECK TO INSURE THAT THE
        (GO REV)                                                    ;; VERB BEING PARSED CAN BE A MAIN VERB

    MODAL
        (FQ NAGR MODAL)
        (SETQ TENSE '(MODAL))
        (if NN (GO MODAL2) (GO INCOMP))

    MODAL2
        (| (and (parse nil 'NOT) (FQ NEG)) nil nil (NOT))

    ADV3
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) ADV3 nil (ADV))
        (COND ((parse 'VB 'BE 'INF) (if NN (GO BE2) (GO MVB)))                  ;; DISPATCH TABLE FOR THE NEXT VERB
            ((parse 'VB 'HAVE 'INF) (if NN (GO HAV2) (GO MVB)))
            ((parse 'VB 'INF '(MVB)) (GO REV))
            (:else (GO INCOMP)))

    WILL
        (FQ NAGR)
        (SETQ TENSE '(FUTURE))
        (if NN (GO MODAL2) (GO INCOMP))                                      ;; THE SAME POSSIBILITIES FOR THE NEXT VERB APPLY
                                                                    ;; AFTER BOTH WILL AND MODALS

    BE
        (MOVE-PT 'C 'DLC)                                             ;; POINT TO WHAT WAS JUST PARSED
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (if (ISQ *pt* PAST) '(PAST) '(PRESENT)))
        (if NN (GO BE2) (GO MVB))

    BE2
        (| (and (parse nil 'NOT) (FQ NEG)) nil nil (NOT))

    ADV4
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) ADV4 nil (ADV))
        (COND ((and (nextword? N 'GOING) (parse 'VB)) (GO GOING))      ;; "...WILL BE GOING TO..."
            ((and (nq 'BE) (parse 'VB 'ING))                           ;; "BE BEING"
                (SETQ TENSE (cons 'PRESENT TENSE))
                (GO EN2))                                           ;; AS IN "BE BEING X'EN(ED)"
            ((and (nq 'ING) (parse 'VB 'ING '(MVB)))                    ;; "BE X'ING"
                (SETQ TENSE (cons 'PRESENT TENSE))
                (GO REV))
            ((cq 'ING *fe*) (mq! 'ING) (GO FAIL)))                          ;; IF TRUE, IT IMPLYS THAT WE STARTED OFF WITH
                                                                    ;; "BEING" - AS IN "BEING EATEN CAN BE UNPLEASANT"
                                                                    ;; - OTHERWISE IT IMPLYS THAT WE HAVE SOMETHING
                                                                    ;; OTHER THAN A VG ON OUR HANDS AND SHOULD FAIL TO
                                                                    ;; WHOEVER CALLED US AND TRY TO PARSE IT
                                                                    ;; DIFFERENTLY

    EN2
        (| (parse 'VB 'EN '(MVB)) nil MVBE)                            ;; THIS ASKS: DO WE HAVE A VERB IN ITS EN FORM
                                                                    ;; WHICH CAN ACT AS A MAIN VERB (IN WHICH CASE IT
        (FQ PASV)                                                   ;; IS MARKED AS PASSIVE AND WE RETURN) OTHERWISE
        (GO REV)                                                    ;; CHECK IF THE VERB BEING POINTED AT IS A
                                                                    ;; LEGITIMATE FORM OF "BE" IN ITS MAIN VERB SENSE
                                                                    ;; - WHICH IS DONE AT "MVBE"

    GOING
        (| (parse nil 'TO) nil GOI)
        (| (nq 'INF) GOING2 nil nil)
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
        (| (ISQ (MOVE-PT 'H 'PV '(VB)) AUX) nil MVB)                   ;; MOVE TO EARLIER AND EARLIER DAUGHTERS  UNTILL
        (| (ISQ *pt* BE) nil (MVBE))                                  ;; YOU REACH A VERB WHICH IS A "QAUX" - IF THERE
                                                                    ;; ARE NONE THEN CONTINUE AT "MVB" IF WHAT YOU ARE
                                                                    ;; POINTING TO (THE "QAUX") IS NOT A FORM OF "BE",
        (setmvb *pt*)                                                 ;; THEN FAIL BECAUSE OF THE UNGRAMATICALITY OF THE
        (GO REV)                                                    ;; CONSTRUCTION OF "BE"'S OTHERWISE MARK IT AS THE
                                                                    ;; MVB AND PREPARE TO RETURN

    HAVE
        (MOVE-PT 'C 'DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ *pt* PAST) (FQ NAGR) '(PAST)) (:else '(PRESENT))))
        (if NN (GO HAV2) (GO MVB))                                           ;; HAV2 WILL CATCH "HAVE HAD", OR "HAVE BEEN ..."

    HAV2
        (| (and (parse nil 'NOT) (FQ NEG)) nil nil (NOT))            ;; OR "HAVE KISSED"

    ADV5
        (| (parse 'ADV) ADV5 nil (ADV))
        (| (parse 'VB 'BE 'EN) nil HAV3)
        (SETQ TENSE (cons 'PAST TENSE))                             ;; "HAVE BEEN ..."
        (if NN (GO BE2) (GO MVB))

    HAV3
        (| (parse 'VB '(MVB) 'EN) nil MVB)
        (SETQ TENSE (cons 'PAST TENSE))                             ;; "HAVE KISSED"
        (GO REV)

    INCOMP
        (FQ INCOMP)
        (GO FAIL)

    MVB
        (| (= (features MVB) (features H)) MVB2 nil)
        (POP VB)                                                    ;; POP OFF EVERY THING UNTILL YOU REACH A VERB
        (| (parse 'VB '(MVB)) nil (MVB))

    MVB2
        (GO REV)

    ;; -------------------------------------------------
    ;;   CHECK AGREEMENT BETWEEN SUBJECT AND MAIN VERB
    ;; -------------------------------------------------

    REV
        (setr 'TENSE TENSE *c*)
        (and NN (parse nil 'NOT) (FQ NEG))
        (COND ((or (= TENSE '(PAST))
                    (cq 'NAGR *fe*)
                    (ISQ (MOVE-PT 'C 'U) IMPER)                       ;; MOVE PT TO THE CLAUSE REMEMBER THAT THE POINTER
                    (ISQ *pt* THERE)                                  ;; STAYS WHERE IT'S PUT UNTILL RETURNING FROM A
                    (ISQ *pt* RSNG))                                  ;; CALL TO PARSE
                (GO NAUX))
            ((set! *pt* (getr 'SUBJECT (MOVE-PT 'C 'U))))               ;; "SUBJECT" IS THE SYNTACTIC SUBJECT OF THE
            (:else (bug "VG -- NO SUBJECT TO CHECK FOR AGREEMENT")))   ;; CLAUSE THAT THE VG IS IN, WHOSE ESSENTIAL
                                                                    ;; DISTINGUISHING FEATURE IS AGREEMENT WITH THE VERB

        (SETQ T3 nil)                                               ;; T3 WILL ACT AS A SWITCH AT "NAGR" BELOW. NOTE
                                                                    ;; THAT IT IS EXPLICITLY SET BY THE CODE BELOW BY
                                                                    ;; THE FOLLOWING CRITERIA;   IF T3 IS NON-NIL THEN
                                                                    ;; SUBJECT AND VERB HAVE BEEN DETERMINED TO AGREE
                                                                    ;; IF IT IS NIL THEN THEY WILL BE CONSIDERED TO
                                                                    ;; AGREE ONLY IF THE FEATURE "PAST-PRESENT" IS ON
        (COND ((ISQ *pt* NFS)                                         ;; THE MVB, IN WHICH CASE, THIS IS EVIDENCE THAT
                (or (SETQ T3 (MEET *fe* '(VFS INF))) (GO NAGR)))      ;; THE PROPER CHOISE OF TENSE IS PAST - WHERE
            ((ISQ *pt* CLAUSE) (or (SETQ T3 (cq 'V3PS *fe*)) (GO NAGR)))    ;; AGREEMENT IS IRRELEVANT (SEE BELOW AT "NAGR")
            ((or (ISQ *pt* NS) (ISQ *pt* MASS))
                (or (and (cq 'V3PS *fe*) (SETQ T3 true))
                    (FESET *pt* (SETDIF (features *pt*) '(NS MASS))))))
        (COND ((or (ISQ *pt* PART) (ISQ *pt* NPL))
            (or (and (MEET *fe* '(INF VPL)) (SETQ T3 true))
                (FESET *pt* (SETDIF (features *pt*) '(PART NPL))))))

    NAGR
        (| (or T3
            (and (= '(PAST-PRESENT) TENSE)                      ;; NOTES WHETHER VERB AND SUBJECT WERE FOUND TO
                (SETQ TENSE '(PAST))))                              ;; AGREE AND FAILS UNLESS A SPECIAL CONDITION
            nil                                                     ;; EXISTS AS NOTED DIRECTLY ABOVE
            (NAGR))

    NAUX
        (setmvb (or (MOVE-PT 'H 'PV '(MVB)) MVB))
        (| (and (cq 'NAUX *fe*) (ISQ (MOVE-PT 'H 'PV '(VB)) AUX) (not (MOVE-PT 'PV 'PV '(VB))))
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
        (| (and (nq 'PREPADV) (parse 'ADV 'PREPADV)) ADV nil (PREPADV)) ;; CHECK FOR ANY INITIAL MODIFING ADVERBS
        (| (COND ((cq 'AGENT *fe*) (nextword? N 'BY))                       ;; EXAMINE THE INITIAL FEATURES OF THE PREPG TO
            ((cq 'LOC *fe*) (nq 'PLACE))                                   ;; CHECK FOR CONSTRAINTS ON THE PREPOSITION
            ((cq 'Q *fe*) (not (nq 'MOTOR)))
            (true))
            nil
            (PREP))                                                 ;; FAIL IF THE CONSTRAINTS AREN'T MET

        ;; PARSE THE PREPOSITION

        (| (parse 'PREP) nil (PREP))
        (MOVE-PT 'H)
        (TRNSF PLACE TIME)                                          ;; THIS IS NOT EXACTLY RIGHT, SINCE "ON WHAT DAY" IS NOT "PLACE"

        ;; AT THIS POINT THE POSSIBILITIES ARE:
        ;;   1. THERE ARE NO MORE WORDS AND THE PREP IS "SHORT"
        ;;   2. YOU HAVE A MULTIPLE WORD PREPOSITION
        ;;   3. IT IS INDEED A SINGLE WORD PREP, PARSE ITS OBJECT

        (SETQ T1 H)                                                 ;; SAVE THE PREPOSITION JUST PARSED IN CASE IT IS
        (and (nq 'PREP2)                                             ;; ONLY THE FIRST WORD OF A MULTIPLE WORD PREPOSITION
            (COND ((SETQ T1 (COMBINATION? (word (NB H)) (word N)))
                    (parse 'PREP2))
                ((SETQ T1 (COMBINATION? (word (NB H)) (word N) (word (cdr N))))
                    (parse 'PREP2)
                    (parse 'PREP2)))
            (SETQ T1 (buildnode (features T1) NB N 'WORD (SM T1)))        ;; CREATE NODE FOR THE COMPOUND WORD
            (setr 'PARENT *c* T1))
        (| (ISQ H NEED2) (NEED2) nil)                               ;; FAIL IF LAST PARSED NEEDS ANOTHER WORD

                                                                    ;; GIVE IT A PARENT
        (setr 'HEAD T1 *c*)                                           ;; SET THE REGESTER "PREP" TO THE CONSTITUENT JUST
                                                                    ;; PARSED - IF IT WAS A MULTIPLE-WORD-PREP THEN
        (or NN (GO SHORT))                                          ;; "PREP" IS SET TO THE NODE WHICH CONTAINS THE
                                                                    ;; ENTIRE FORM NN POINTS TO WHATEVER WORDS ARE
                                                                    ;; LEFT BEFORE THE CUT POINT

        ;; ADD FEATURES TO THE PREPG DEPENDING ON THE PREPOSITION PARSED

        (COND ((= (word H) 'BY) (FQ AGENT)))

    ;; ###################################
    ;; PARSE THE OBJECT TO THE PREPOSITION
    ;; ###################################

    QUEST
        (| (cq 'QUEST *fe*) nil NG)
                                                                    ;; CERTAIN RESTRICTIONS PLACED ON THE POSSIBLE
                                                                    ;; NOUN GROUPS THAT IT CAN TAKE - HENCE THE
                                                                    ;; SPECIAL CALL TO PARSE AS ABOVE, IF THE PREPG IS
        (| (parse 'NG 'QUEST 'OBJ) OBJR (PREPQUEST))                   ;; MARKED WITH THE FEATURE "QUEST" (INDICATING
        (| (and (cq 'OF *fe*) (parse 'NG 'OFOBJ)) OBJR nil)                 ;; THAT IT SHOULD CONTAIN THE QUESTION ELEMENT OF

    NG
        (| (parse 'NG 'OBJ) OBJR nil)                                 ;; THE CLAUSE) THEN WE PARSE IT SPECIALLY SIMPLE

    REL
        (| (nextword? N 'WHICH) nil REST)                             ;; NOUN GROUP - NO RESTRICTIONS
        (| (ISQ (MOVE-PT 'U) CLAUSE) nil (PREP-WHICH))               ;; IF THE NEXT WORD IS A RELWORD, SUCH AS "WHICH"
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
        (| (ISQ *pt* PRONREL) nil PRONREL)                            ;; CHANGES ITS REQUEST FROM (PARSE PREPG Q)  TO
        (set! *mes* (cdr *mes*))                                        ;; (PARSE CLAUSE RSQ PREPREL), WHICH SHOULD PICK
                                                                    ;; UP THE BOTHERSOME PREPG AS AN INITIAL MODIFIER
                                                                    ;; TO THE CLAUSE AND DEAL WITH IT APPROPRIATELY
                                                                    ;; RESET THE FAILURE MESSAGE LIST (WE KNOW TO DO
        (GO P-RELWRD)                                               ;; THIS BECAUSE THE "PRONREL" AS AN INITIAL
                                                                    ;; FEATURE OF THE CLAUSE IMPLICATES THE PASSAGE OF
                                                                    ;; THE PROS CESS DESCRIBED ABOVE)

    PRONREL
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (add-f-pt 'PRONREL *pt*)

    P-RELWRD
        (parse 'NG 'RELWD 'OBJ)
        (setr 'OBJ1 (getr 'HEAD *pt*) *c*)                              ;; THE REGISTER IS ACCESSED BY CODE IN THE PASSIVE
        (GO RETT)                                                   ;; SECTION OF CLAUSE AND BY THE APPROPRIATE

    REST
        (| (parse 'CLAUSE 'RSNG 'ING) OBJR SHORT)                      ;; SEMANTIC SPECIALIST "HEAD" IS HERE THE HEAD OF

    OBJR
        (setr 'OBJ1 H *c*)                                            ;; THE HIGHER NOUNGROUP
        (GO RETT)

    SHORT
        (| (MEET *fe* '(NOSHORT Q)) (SHORT) nil)
        (or (ISQ (MOVE-PT 'C 'U) REL-NOT-FOUND)
            (ISQ (getr 'QUESTION-ELEMENT *pt*) QADJ)
            (GO FAIL))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (add-f-pt 'PREPREL *pt*)
        (setr 'OBJ1 (getr 'RELHEAD (MOVE-PT 'C 'U)) *c*)

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
                (MOVE-PT 'H 'H 'PV '(QUEST))))                          ;; COMPONENT FOR THE FEATURE "QUEST"
            (FQ QUEST))
        (| (CALLSM (SMADJG-PREPG)) RETURN FAIL))

(§ PDEFINE ADJG nil

    ENTERING-ADJG                                                   ;; THIS LABEL IS MARKED BY DEBUGGING ROUTINES AND

    COMPCHECK                                                       ;; IS USEFUL FOR FOLLOWING THE FLOW OF CONTROL
        (| (and (MOVE-PT 'C 'U '(BE)) (not (cq 'COMP *fe*))) FAIL nil)       ;; CONDITIONS WHICH MUST BE MET BY ANY ADJECTIVE
                                                                    ;; GROUP IF THERE IS A FORM OF "BE" IN THE HIGHER
                                                                    ;; CLAUSE, THEN THE ADJG SHOULD HAVE BEEN CALLED
                                                                    ;; WITH THE FEATURE "COMP" FOR COMPLIMENT

        ;; EXAMINE THE INITIAL FEATURES (THOSE DESIGNATED BY THE
        ;; CALLING PROGRAM) ALSO EXAMINE THE NEXT WORD - THESE GIVE
        ;; CLUES AND CONSTRAINTS TO THE STRUCTURE TRYING TO BE PARSED
        ;; AND DIRECT JUMPS TO THE APPROPRIATE SECTIONS OF CODE

        (| (ISQ (MOVE-PT 'C 'U) THAN) nil DISP)                       ;; THE WORD "THAN" WAS DETECTED BY THE IMMEDIATELY
                                                                    ;; UPSTAIRS NG AS FOLLOWING THE HEAD NOUN
        (setr 'HEAD (getr 'COMPARATIVE-MODIFIER *pt*) *c*)              ;; INDICATING A STURCTURE SUCH AS "... A BIGGER
        (GO THAN)                                                   ;; BLOCK THAN THAT ONE ..." "HEAD REFERS TO THE
                                                                    ;; ADJG'S HEAD ADJECTIVE

    DISP
        (| (and (nq 'AS) (parse nil 'AS)) AS nil (AS))
        (| (and (nq 'AS) (parse nil 'AS)) AS nil (AS))
        (| (nextword? N 'HOW) HOW ADV)

    ;; --------------- HOW + ADJG ----------------

    HOW
        (| (and (parse nil 'HOW) (FQ QUEST)) nil FAIL FAIL)
        (| (and (parse 'ADJ) (FQ ADJ) (setr 'HEAD H *c*)) RETSM nil)
        (| (and (parse 'ADV 'VBAD) (FQ VBAD) (setr 'HEAD H *c*)) RETSM FAIL)

    ADV
        (| (parse 'ADV 'ADVADV) ADV nil POPAD)                        ;; THIS LOOPS UNTILL ALL CONTIG- UOUS ADVERBS HAVE
        (| (parse nil 'MORE) nil ADJ)                                ;; BEEN PARSED "MORE" IS EXPLICITLY CHECKED FOR
        (FQ COMPAR)                                                 ;; SINCE IT SIGNALS THE FEATURE, COMPARATIVE

    ADJ
        (| (if (cq 'ADV *fe*) (parse 'ADV 'VBAD) (parse 'ADJ))
            nil
            (ADJ))                                                  ;; IF THE CUT POINT WAS REACHED THEN NO MORE
        (| (setr 'HEAD H *c*) nil nil RETSM)                          ;; PROCESSING (SUCH AS COMPAR BELOW) IS POSSIBLE.

    ;; -------------------------------------------
    ;;               COMPARATIVES
    ;; -------------------------------------------

    ;; IF THE FEATURE "COMPAR" IS ALREADY ON THE LIST, OR IF THE JUST PARSED ADJECTIVE CAN HAVE THAT FEATURE, THEN
    ;; ATTEMPT TO PARSE SOME SORT OF COMPARATIVE CONSTRUCTION (ASSUMING THAT THEREARE ANY MORE WORDS BEFORE THE CUT POINT.)

        (| (or (cq 'COMPAR *fe*) (ISQ H COMPAR)) nil RETSM)
        (FQ COMPAR)
        (| NN nil RETSM)                                            ;; IF THERE ARE UNPARSED WORDS LEFT BEFORE THE CUT
                                                                    ;; POINT THEN THE POSSIBILITY OF MORE COMPLICATED
                                                                    ;; FORMS IS CHECKED FOR

    THAN
        (COND ((not NN) (GO RETSM)))
        (| (parse nil 'THAN) nil RETSM (THAN))
        (RQ THANNEED)                                               ;; THE FEATURE "THANNEEED" MARKS THAT THE WORD
        (FQ THAN)                                                   ;; "THAN" IS EXPLICITLY  REQUIRED IN THE PHRASE.
        (GO SUBJ)

    AS
        (FQ AS)
        (RQ THANNEED)
        (| (and (parse 'ADJ) (setr 'HEAD H *c*)) nil (ADJ) RETSM)
        (| (parse nil 'AS) SUBJ RETSM (AS))

    ;; FIND A SUBJECT FOR THE COMPARATIVE
    ;; IE.  "AS BIG AS ..." , "BIGGER THAN ..."

    SUBJ
        (| (parse 'NG 'SUBJ 'COMPAR) nil (THAN))
        (| (setr 'OBJ1 H *c*) nil nil RETSM)
        (| (and (one-word-left NB) (parse 'VB 'AUX)) nil RETSM)
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
        (| (cq 'THANNEED *fe*) (THANNEED) nil)                            ;; IF ONE OF THE WORDS PARSED REQUIRED A "THAN",
        (| (CALLSM (SMADJG-PREPG)) RETURN (SMADJ)))                 ;; FAIL IF ONE WAS NOT FOUND.

(§ defn- CONJ []
    (let [*end* CUT goodie (apply-grammar 'CONJOIN)]
        (when goodie (set! *re* goodie))))

(§ defn- COMMA []
    (COND ((secondword? N '\") (FLUSHME) true)                           ;; IF " FOLLOWS, FLUSH COMMA AND CONTINUE
        ((CONJ))                                                    ;; IF COMMA IS PART OF CONJOINED STRUCTURE, GREAT
        ((ISQ *re* INIT) (FLUSHME) true)))                               ;; IF COMMA FOLLOWS INITIAL-TYPE PHRASE, FLUSH IT AND CONTINUE DIRECT ADDRESS JAZZ

(§ PDEFINE CONJOIN (PREV)

    ;; THIS PROGRAM IS CALLED TO PARSE A CONJOINED STRUCTURE THE
    ;; FIRST MEMBER OF THE SUPPOSED STRUCTURE HAS ALREADY BEEN
    ;; PARSED AND IS SET TO THE INITIAL DAUGHTER (H) OF THIS NODE
    ;; AN ATTEMPT IS MADE TO PARSE AS MANY CONSTITUENTS JUST LIKE
    ;; IT AS IS POSSIBLE

    ENTERING-CONJOIN                                                ;;  HACK LABEL FOR LABELTRACER

    UP
        (SETQ PREV (nextword N))
        (FLUSHME)
        (COND ((and (= PREV '\,)                                   ;; IF WE HAVE A COMMA AND
                (or (cdr H)                                         ;; IF MORE THAN 1 COMPONENT HAS BEEN PARSED
                (> (- (count (NB H))               ;; OR IF THAT ONE COMPONENT
                            (count (N H)))                         ;; IS MORE THAN 4 WORDS LONG
                    4))
                (memq (nextword N) '(OR AND NOR BUT))
                (F (nextword N)))                                     ;; THEN CHECK FOR COMMA COMBINATION
            (SETQ PREV (list PREV (nextword N)))
            (FLUSHME)))
        (and (term? PREV)
            (MOVE-PTW 'N 'NW '(= (word *ptw*) PREV))
            (CUT *ptw*))
        (and (or (= PREV 'BUT) (= (cadr PREV) 'BUT))
            (nextword? N 'NOT)                                        ;; CHECK FOR BUT-NOT COMBINATION
            (or (FLUSHME) (GO LOSE2))
            (FQ NEGBUT))
        (| (COND ((memq (car REST) '(ADJ NUM NOUN PREP VB ADV))
                (PARSE3 (concat REST '(COMPONENT)) nil))
            ((memq (car REST) '(NG PREPG ADJG))
                (and (not (cq 'OFOBJ *fe*)) (PARSE2 (concat REST '(COMPONENT)) nil)))
            ((= (car REST) 'CLAUSE)
                ((lambda [*lastsent* AUXFE]
                    (and (PARSE2 (concat REST AUXFE '(COMPONENT)) nil)
                        (or (not AUXFE) (F (car AUXFE)))
                        (setr 'TIME (getr 'TIME H) *c*)))             ;; MARK COMPOUND CLAUSE AS TO DECLAR/IMPER FOR ANSGEN
                    (if (ISQ H MAJOR) H *lastsent*)
                    (MEET (features H) '(DECLAR IMPER)))))
            nil
            LOSE2)
        (CUT *end*)                                                   ;; RESTORE CUT POINT
        (COND ((not (term? PREV))
                ;; IF WE HAD COMMA FOLLOWED BY (AND OR BUT NOR), RETURN THE LIST OF GOODIES WE'VE FOUND
                (GO RETSM))
            ((= PREV '\,)
                (COND ((nextword? N COMMA) (FQ LIST) (GO UP))
                    (:else (GO LIST))))
            ((memq PREV '(AND OR NOR BUT))
                (COND ((= BOTH (NB H)) (FQ BOTH)))
                (COND ((or (nextword? N 'BUT)
                        (and (nextword? N PREV)
                            (not (and (= BOTH (NB H))              ;; IF WE HAD THE 'BOTH' WORD AND
                                (= PREV 'AND)))))                  ;; IF THE 'BOTH' WORD WAS "AND", STOP PARSING
                        (FQ LISTA)                                  ;; ELSE GO LOOK FOR THE NEXT COMPONENT
                        (F PREV)
                        (GO UP))
                    (:else (GO LISTA)))))

    LOSE2
        (| (cq 'LISTA *fe*) LISTA nil)

    LIST
        (| (and (= PREV '\,)                                       ;; COME HERE FOR ABORTED LIST AND CHECK FOR APPOSITIVE
                (== (count H) 2)
                (ISQ H NG)
                (not (or (ISQ H PRONG) (ISQ (cdr H) PRONG)))
                (or (nextword? N COMMA) (nil? N)))
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
        (COND ((or (cq 'NG *fe*) (cq 'NOUN *fe*))
                (COND ((cq 'AND *fe*) (FQ NPL))
                    (:else (MOVE-PT 'H) (TRNSF NPL NS MASS NFS))))
            ((cq 'VB *fe*)
                (let [COMMON nil]
                    (SETQ COMMON (getprop 'VB 'ELIM))
                    (MAP #'(lambda [x]
                            (SETQ COMMON (MEET COMMON (features x))))
                    H)
                    nil)
                (FESET (UNION COMMON (features *c*)) *c*)))
        (| (CALLSM (SMCONJ)) RETURN (CONJOINß SMCONJ)))             ;; THEN MARK AS A LIST

(defn- doublequoter [] (apply-grammar 'PARSEQUOTED))

(§ defn- CANTAKE [num type feature]
    (let [vbfeat (features MVB)]
        (COND
            ((memq 'RSNG type)
                (memq (READLIST (concat
                        (COND ((memq 'TO type) '(T O)) ((memq 'ING type) '(I N G)) ((memq 'REPORT type) '(R E P)))
                        '(O B)
                        (list (if (== num 1) '\1 '\2))))
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
                (and (apply parse
                    (concat type
                        (COND ((memq 'COMP type) (SETQ REG 'COMP) nil)
                            (:else (list 'OBJ
                                (SETQ REG (COND
                                    ((or (memq 'LOC type) (memq 'PLACE type)) 'LOBJ)
                                    ((== num 1) 'OBJ1)
                                    (:else 'OBJ2))))))))
                    (setr REG H *c*)))
            (or (nil? feature) (F feature))
            (RETURN true))
        nil))

#_(ns shrdlu.dictio)

;; ###########################################################
;;
;;                          WORDS
;;
;; ###########################################################

(putprop! '\, 'FEATURES '(SPECIAL))
(putprop! '\, 'SPECIAL '(COMMA))

(putprop! '\" 'FEATURES '(B-SPECIAL RELWRD))
(putprop! '\" 'B-SPECIAL '(doublequoter))

(putprop! 'A 'SEMANTICS '((DET true)))
(putprop! 'A 'FEATURES '(DET NS INDEF))

(putprop! 'ABOVE 'SEMANTICS '((PREP (!LOC !ABOVE true))))
(putprop! 'ABOVE 'FEATURES '(PREP PLACE))

(putprop! 'AFTER 'SEMANTICS '((BINDER (smbinder *end* nil))))
(putprop! 'AFTER 'FEATURES '(BINDER TIME))

(putprop! 'ALL 'SEMANTICS '((DET (COND ((cq 'OF *fe*) 'ALL) ((MEET '(NUM DEF) *fe*) 'DEF) ('NDET)))))
(putprop! 'ALL 'FEATURES '(DET NPL QNTFR))

(putprop! 'AN 'IRREGULAR '(A nil nil))

(putprop! 'AND 'FEATURES '(SPECIAL))
(putprop! 'AND 'SEMANTICS true)
(putprop! 'AND 'SPECIAL '(CONJ))

(putprop! 'ANY 'SEMANTICS '((DET 'INDEF)))
(putprop! 'ANY 'FEATURES '(DET ANY NS NPL QNTFR))

(putprop! 'ANYTHING 'SEMANTICS '((TPRON 'INDEF)))
(putprop! 'ANYTHING 'FEATURES '(TPRON ANY NS))

(putprop! 'ARE 'IRREGULAR '(BE (VPL PRESENT) (INF)))

(putprop! 'AS 'SEMANTICS '((nil? true)))
(putprop! 'AS 'FEATURES '(AS))

(putprop! 'ASK 'SEMANTICS '((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!WANT !1 !2 *TIME)))))))))
(putprop! 'ASK 'FEATURES '(VB TRANS INF SUBTOB))

(putprop! 'AT 'SEMANTICS '((NUMD true)))
(putprop! 'AT 'FEATURES '(AT))

(putprop! 'AWAY 'SEMANTICS '((PRT true)))
(putprop! 'AWAY 'FEATURES '(PRT))

(putprop! 'BACK 'SEMANTICS '((PREP2 true) (NOUN true)))
(putprop! 'BACK 'FEATURES '(NOUN NS PREP2))

(putprop! 'BALL 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!MANIP !ROUND) PROCEDUREß ((!IS *** !BALL)))))))
(putprop! 'BALL 'FEATURES '(NOUN NS))

(putprop! 'BE 'FEATURES '(INT AUX VB BE INF))
(putprop! 'BE 'SEMANTICS '((VB ((THERE (!BETHERE)) (INT (!BEINT))))))

(§ defn- !BETHERE []
    (RELATION (RESTRICTIONSß (((!THING) (= (quantifier? SMSUB) 'INDEF))) PROCEDUREß nil)))

(§ defn- !BEINT []
    (or (RELATION
            (RESTRICTIONSß (((!PHYSOB)) (SMCOMP (!PROPERTY)))
                PROCEDUREß (!EVAL (let [PROPERTY nil]
                                    (COND
                                        ((SETQ PROPERTY (MEET (getprop '!PROPERTY 'SYSTEM) (markers? SMCOMP)))
                                            (RETURN (list (list (car PROPERTY) '!1 '!2))))
                                    ((RETURN (list '(!2 !1))))))))
            (RESTRICTIONSß (((!THING)) (SMCOMP (!SYSTEMS) (and (not (refer? SMCOMP)) (= (rel? SMCOMP) SMSUB))))
                PROCEDUREß (!EVAL (relations? SMCOMP)))
            (RESTRICTIONSß (((!THING)) (SMCOMP (!THING) (refer? SMCOMP)))
                PROCEDUREß ((!EVAL (list 'THAMONG '!1 (list 'quote (refer? !2)))))))
        (ert "SORRY, I DON'T UNDERSTAND THE VERB BE, WHEN YOU USE IT LIKE THAT")))

(putprop! 'BEFORE 'SEMANTICS '((BINDER (smbinder nil *start*))))
(putprop! 'BEFORE 'FEATURES '(BINDER TIME))

(putprop! 'BEGIN 'SEMANTICS '((VB
        ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!START !2 *TIME)))))
         (ITRNS (RELATION (RESTRICTIONSß (((!ANIMATE))) MARKERSß (!EVENT) PROCEDUREß ((!START EE *TIME)))))))))
(putprop! 'BEGIN 'FEATURES '(VB TRANS INF TOOB INGOB ITRNS))

(putprop! 'BEGAN 'IRREGULAR '(BEGIN (PAST) (INF)))

(putprop! 'BEHIND 'SEMANTICS '((PREP (!LOC !BEHIND true))))
(putprop! 'BEHIND 'FEATURES '(PREP PLACE))

(putprop! 'BELOW 'SEMANTICS '((PREP (!LOC !ABOVE nil))))
(putprop! 'BELOW 'FEATURES '(PREP PLACE))

(putprop! 'BENEATH 'SEMANTICS '((PREP (!LOC !ABOVE nil))))
(putprop! 'BENEATH 'FEATURES '(PREP PLACE))

(putprop! 'BESIDE 'SEMANTICS '((PREP (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PROCEDUREß ((!NEXTO !1 !2 *TIME)))))))
(putprop! 'BESIDE 'FEATURES '(PREP PLACE))

(putprop! 'BIG 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !BIG) PROCEDUREß ((!MORE !SIZE *** (128 128 128))))))))
(putprop! 'BIG 'FEATURES '(ADJ))

(putprop! 'BLACK 'SEMANTICS '((ADJ (!COLOR !BLACK))))
(putprop! 'BLACK 'FEATURES '(ADJ))

(putprop! 'BLOCK 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!MANIP !RECTANGULAR) PROCEDUREß ((!IS *** !BLOCK)))))))
(putprop! 'BLOCK 'FEATURES '(NOUN NS))

(putprop! 'BLUE 'SEMANTICS '((ADJ (!COLOR !BLUE))))
(putprop! 'BLUE 'FEATURES '(ADJ))

(putprop! 'BOTH 'SEMANTICS '((DET 'DEF)))
(putprop! 'BOTH 'FEATURES '(B-SPECIAL QNTFR DET DEF NPL BOTH))
(putprop! 'BOTH 'B-SPECIAL '(BOTH AND))

(§ defq- BOTH [a]
    ;; HANDLES (BOTH AND) (EITHER OR) (NEITHER NOR) COMBINATIONS
    ;; THE CONJOIN PROGRAM DOES SPECIAL THINGS WHEN BOTH IS SET
    (let [*end* CUT]                                                ;; MAKE END OUT OF PREVIOUS CUT POINT
        (let [CUT nil nbb N BOTH nil]
            (and (FLUSHME)
                (MOVE-PTW 'N 'NW '(= (word *ptw*) (car a)) 'NW)         ;; LOOK FOR THE MATCHING WORD E.G. AND, OR, NOR
                (CUT *end*)
                (SETQ BOTH *ptw*)                                     ;; SAVE POINTER TO THE WORD AFTER THE MATCHING WORD
                (set! *re* (COND
                    ((memq (car REST) '(PREP ADV)) (PARSE3 REST true))
                    ((memq (car REST) '(NG PREPG ADJG CLAUSE)) (PARSE2 REST true))))
                (< (count N) (count BOTH))                          ;; FAIL UNLESS WE PARSED BEYOND MATCHING WORD
                (RETURN (SETQ SPECIAL 'SKIP)))
            (set! *re* nil)
            (SETQ N nbb)
            nil)))

(putprop! 'BOX 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!BOX) PROCEDUREß ((!IS *** !BOX)))))))
(putprop! 'BOX 'FEATURES '(NOUN NS))

(putprop! 'BRICK 'FEATURES '(NOUN NS))

(putprop! 'BUILD 'SEMANTICS '((VB ((TRANS (!BUILD))))))
(putprop! 'BUILD 'FEATURES '(VB INF TRANS))

(putprop! 'BUT 'FEATURES '(SPECIAL))
(putprop! 'BUT 'SEMANTICS true)
(putprop! 'BUT 'SPECIAL '(CONJ))

(putprop! 'BY 'SEMANTICS '((PREP (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PROCEDUREß ((!NEXTO !1 !2 *TIME)))))))
(putprop! 'BY 'FEATURES '(PREP))

(putprop! 'CALL 'SEMANTICS '((VB ((TRANS2 (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!THING)) ((!NAME))) PROCEDUREß ((!CALL !2 !3 *TIME)))))))))
(putprop! 'CALL 'FEATURES '(VB INF TRANS2))

(putprop! 'CAN 'SEMANTICS '((VB true)))
(putprop! 'CAN 'FEATURES '(V3PS VFS VPL VB MODAL AUX))

(putprop! 'CHOOSE 'SEMANTICS '((VB ((TRANS (!NOTICE))))))
(putprop! 'CHOOSE 'FEATURES '(VB INF TRANS))

(putprop! 'CLEAN 'SEMANTICS '((VB true)))
(putprop! 'CLEAN 'FEATURES '(VB INF VPRT TRANS))

(putprop! 'CLEAN-OFF 'ROOT '(CLEAN OFF))
(putprop! 'CLEAN-OFF 'SEMANTICS '((TRANS (!CLEANOFF))))
(putprop! 'CLEAN-OFF 'FEATURES '(COMBINATION TRANS))

(putprop! 'CLEAN-OUT 'ROOT '(CLEAN OUT))
(putprop! 'CLEAN-OUT 'SEMANTICS '((TRANS (!CLEANOFF))))
(putprop! 'CLEAN-OUT 'FEATURES '(COMBINATION TRANS))

(putprop! 'CLEAN-UP 'ROOT '(CLEAN UP))
(putprop! 'CLEAN-UP 'SEMANTICS '((TRANS (!CLEANOFF))))
(putprop! 'CLEAN-UP 'FEATURES '(COMBINATION TRANS))

(putprop! 'CLEAR 'SEMANTICS '((VB true)))
(putprop! 'CLEAR 'FEATURES '(VB INF VPRT TRANS))

(putprop! 'CLEAR-OFF 'ROOT '(CLEAR OFF))
(putprop! 'CLEAR-OFF 'SEMANTICS '((TRANS (!CLEANOFF))))
(putprop! 'CLEAR-OFF 'FEATURES '(COMBINATION TRANS))

(putprop! 'CLEAR-OUT 'ROOT '(CLEAR OUT))
(putprop! 'CLEAR-OUT 'SEMANTICS '((TRANS (!CLEANOFF))))
(putprop! 'CLEAR-OUT 'FEATURES '(COMBINATION TRANS))

(putprop! 'COLOR 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!COLOR) PROCEDUREß ((!IS *** !COLOR)))))))
(putprop! 'COLOR 'FEATURES '(NOUN NS))

(putprop! 'CONSTRUCT 'SEMANTICS '((VB ((TRANS (!BUILD))))))
(putprop! 'CONSTRUCT 'FEATURES '(VB INF TRANS))

(putprop! 'CONTAIN 'SEMANTICS '((VB
        ((TRANS (RELATION
                (RESTRICTIONSß (((!BOX)) ((!PHYSOB))) PROCEDUREß ((!CONTAIN !1 !2 *TIME)))
                (RESTRICTIONSß (((!CONSTRUCT)) ((!THING))) PROCEDUREß ((!PART !2 !1 *TIME)))))))))
(putprop! 'CONTAIN 'FEATURES '(VB INF TRANS))

(putprop! 'CONTAINER 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!BOX) PROCEDUREß ((!IS *** !BOX)))))))
(putprop! 'CONTAINER 'FEATURES '(NOUN NS))

(putprop! 'CORNER 'FEATURES '(NOUN NS))

(putprop! 'CUBE 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!MANIP !RECTANGULAR) PROCEDUREß ((!IS *** !BLOCK) (!EQDIM ***)))))))
(putprop! 'CUBE 'FEATURES '(NOUN NS))

(putprop! 'DID 'IRREGULAR '(DO (PAST V3PS) (INF PRESENT)))

(putprop! 'DO 'SEMANTICS '((VB
        ((TRANS (RELATION
            (RESTRICTIONSß RESTRICTIONSß
            PROCEDUREß ((((!ANIMATE)) ((!EVENT))))
            MARKERSß PROCEDUREß
            PLAUSIBILITYß (!EVAL (or (getprop MAP2 'REFER) (ert "DO DEFINITION"))))))))))
(putprop! 'DO 'FEATURES '(TRANS VFS PRESENT VPL VB AUX DO INF))

(putprop! 'DOES 'IRREGULAR '(DO (V3PS) (VFS VPL INF)))

(putprop! 'DOWN 'SEMANTICS '((PRT true)))
(putprop! 'DOWN 'FEATURES '(PRT))

(putprop! 'DROP 'SEMANTICS '((VB
        ((TRANSL (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP)) (SMOBL (!PLACE *TIME))) PROCEDUREß ((!DROP !1 !2 !3)) MARKERSß ((!MOTION)))))
         (TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!DROP !1 !2 PLACE *TIME)) MARKERSß ((!MOTION)))))))))
(putprop! 'DROP 'FEATURES '(TRANSL TRANSL2 VB INF TRANS))

(putprop! 'EACH 'SEMANTICS '((DET 'ALL)))
(putprop! 'EACH 'FEATURES '(DET NS QNTFR))

(putprop! 'EITHER 'FEATURES '(B-SPECIAL))
(putprop! 'EITHER 'SEMANTICS true)
(putprop! 'EITHER 'B-SPECIAL '(BOTH OR))

(putprop! 'EVERY 'SEMANTICS '((DET 'ALL)))
(putprop! 'EVERY 'FEATURES '(DET NS QNTFR))

(putprop! 'EVERYTHING 'SEMANTICS '((TPRON 'ALL)))
(putprop! 'EVERYTHING 'FEATURES '(TPRON NS))

(putprop! 'EXACTLY 'SEMANTICS '((NUMD (list 'EXACTLY NUM))))
(putprop! 'EXACTLY 'FEATURES '(NUMD NUMDALONE))

(putprop! 'FEW 'SEMANTICS '((NUMD (list '< (inc NUM)))))
(putprop! 'FEW 'FEATURES '(NUMD NUMDAS))

(putprop! 'FEWER 'SEMANTICS '((NUMD (list '< NUM))))
(putprop! 'FEWER 'FEATURES '(NUMD NUMDAN))

(putprop! 'FIND 'SEMANTICS '((VB ((TRANS (!NOTICE))))))
(putprop! 'FIND 'FEATURES '(VB INF TRANS))

(putprop! 'FINISH 'SEMANTICS '((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!END !2 *TIME)))))))))
(putprop! 'FINISH 'FEATURES '(VB INF TRANS INFOB))

(putprop! 'FIVE 'SEMANTICS '((NUM 5)))
(putprop! 'FIVE 'FEATURES '(NUM))

(putprop! 'FOUR 'SEMANTICS '((NUM 4)))
(putprop! 'FOUR 'FEATURES '(NUM))

(putprop! 'FRIEND 'REFER 'ßFRIEND)
(putprop! 'FRIEND 'FEATURES '(NOUN NS))

(putprop! 'FROM 'FEATURES '(PREP))

(putprop! 'FRONT 'SEMANTICS '((NOUN true) (PREP2 true)))
(putprop! 'FRONT 'FEATURES '(NOUN NS PREP2))

(putprop! 'GAVE 'IRREGULAR '(GIVE (PAST) (INF)))

(putprop! 'GIVE 'SEMANTICS '((VB ((TRANS2 (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!GIVE !1 !2 !3 *TIME)))))))))
(putprop! 'GIVE 'FEATURES '(VB INF TRANS2))

(putprop! 'GO 'FEATURES '(ITRNS VB INF))

(putprop! 'GOING 'FEATURES '(VB ITRNS ING))

(putprop! 'GRAB 'SEMANTICS '((VB ((TRANS (!GRASP))))))
(putprop! 'GRAB 'FEATURES '(VB TRANS INF))

(putprop! 'GRASP 'SEMANTICS '((VB ((TRANS (!GRASP))))))
(putprop! 'GRASP 'FEATURES '(VB TRANS INF))

(putprop! 'GREATER 'SEMANTICS '((NUMD (list '> NUM))))
(putprop! 'GREATER 'FEATURES '(NUMD NUMDAN))

(putprop! 'GREEN 'SEMANTICS '((ADJ (!COLOR !GREEN))))
(putprop! 'GREEN 'FEATURES '(ADJ))

(putprop! 'HAD 'IRREGULAR '(HAVE (PAST) (INF)))

(putprop! 'HAND 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!HAND) PROCEDUREß ((!IS *** !HAND)))))))
(putprop! 'HAND 'FEATURES '(NOUN NS))

(putprop! 'HANDLE 'SEMANTICS '((VB ((TRANS (!GRASP))))))
(putprop! 'HANDLE 'FEATURES '(VB INF TRANS))

(putprop! 'HAS 'IRREGULAR '(HAVE (V3PS PRESENT) (INF)))

(putprop! 'HAVE 'SEMANTICS '((VB ((TRANS (!HAVE))))))
(putprop! 'HAVE 'FEATURES '(HAVE VB AUX INF TRANS))

(putprop! 'HIGH 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !HEIGHT RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!HIGH ***)))))))
(putprop! 'HIGH 'FEATURES '(ADJ))

(putprop! 'HOLD 'SEMANTICS '((VB
        ((TRANS (RELATION
              (RESTRICTIONSß (((!HAND)) ((!MANIP))) PROCEDUREß ((!GRASPING !2 *TIME)))
              (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) PROCEDUREß ((!GRASPING !2 *TIME)))))))))
(putprop! 'HOLD 'FEATURES '(VB INF TRANS))

(putprop! 'HE 'FEATURES '(PRON NS SUBJ))

(putprop! 'HER 'IRREGULAR '(SHE (OBJ POSS) (SUBJ)))

(putprop! 'HIM 'IRREGULAR '(HE (OBJ) (SUBJ)))

(putprop! 'HIS 'FEATURES '(PRON POSS))

(putprop! 'HOW 'SEMANTICS '((QADJ true)))
(putprop! 'HOW 'FEATURES '(QADJ))

(putprop! 'HOWEVER 'FEATURES '(PRON EVERPRON))

(putprop! 'I 'SEMANTICS '((PRON (SMSET (list (newcopy 'FRIEND-OSS))))))
(putprop! 'I 'FEATURES '(SUBJ PRON NFS))

(putprop! 'IF 'FEATURES '(BINDER))

(putprop! 'IN 'SEMANTICS '((PREP (!IN))))
(putprop! 'IN 'FEATURES '(ADV PLACE PREP PLACE))

(putprop! 'IN-BACK-OF 'ROOT '(IN BACK OF))
(putprop! 'IN-BACK-OF 'SEMANTICS '(!LOC !BEHIND true))
(putprop! 'IN-BACK-OF 'FEATURES '(PREP COMBINATION))

(putprop! 'IN-FRONT-OF 'ROOT '(IN FRONT OF))
(putprop! 'IN-FRONT-OF 'SEMANTICS '(!LOC !BEHIND nil))
(putprop! 'IN-FRONT-OF 'FEATURES '(PREP COMBINATION))

(putprop! 'INSIDE 'SEMANTICS '((PREP (!IN))))
(putprop! 'INSIDE 'FEATURES '(PREP PLACE))

(putprop! 'INSIDE-OF 'ROOT '(INSIDE OF))
(putprop! 'INSIDE-OF 'SEMANTICS '(!IN))
(putprop! 'INSIDE-OF 'FEATURES '(PREP COMBINATION))

(putprop! 'INTO 'SEMANTICS '((PREP (!IN))))
(putprop! 'INTO 'FEATURES '(PREP PLACE))

(putprop! 'IS 'IRREGULAR '(BE (V3PS PRESENT) (INF)))

(putprop! 'IT 'SEMANTICS '((PRON (SMIT 'IT))))
(putprop! 'IT 'FEATURES '(PRON NS SUBJ OBJ))

(putprop! 'ITS 'IRREGULAR '(IT (POSS) nil))

(putprop! 'KNOW 'FEATURES '(VB INF TRANS REPOB))

(putprop! 'LARGE 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !BIG) PROCEDUREß ((!MORE !SIZE *** (128 128 128))))))))
(putprop! 'LARGE 'FEATURES '(ADJ))

(putprop! 'LEAST 'SEMANTICS '((NUMD (list '> (dec NUM)))))
(putprop! 'LEAST 'FEATURES '(NUMD NUMDAT))

(putprop! 'LEFT 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!DIRECTION) PROCEDUREß ((!DIRECTION !RIGHT nil)))))))
(putprop! 'LEFT 'FEATURES '(NOUN NS))

(putprop! 'LESS 'SEMANTICS '((NUMD (list '< NUM))))
(putprop! 'LESS 'FEATURES '(NUMD NUMDAN))

(putprop! 'LIKE 'SEMANTICS '((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!THING))) PROCEDUREß ((!LIKE !1 !2)))))))))
(putprop! 'LIKE 'FEATURES '(VB INF TRANS))

(putprop! 'LIST 'SEMANTICS '((VB ((TRANS (!NAME))))))
(putprop! 'LIST 'FEATURES '(VB VO TRANS))

(putprop! 'LITTLE 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !LITTLE) PROCEDUREß ((!MORE !SIZE (128 128 128) ***)))))))
(putprop! 'LITTLE 'FEATURES '(ADJ))

(putprop! 'LONG 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !LENGTH RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !LENGTH *** (128 128 128))))))))
(putprop! 'LONG 'FEATURES '(ADJ))

(putprop! 'MAKE 'SEMANTICS '((VB ((TRANS (!BUILD))))))
(putprop! 'MAKE 'FEATURES '(VB INF TRANS))

(putprop! 'MANY 'SEMANTICS '((NUMD (list '> (dec NUM))) (DET true)))
(putprop! 'MANY 'FEATURES '(DET QNTFR NPL NONUM NUMD NUMDAS))

(putprop! 'ME 'IRREGULAR '(I (OBJ) (SUBJ)))

(putprop! 'MORE 'SEMANTICS '((NUMD (list '> NUM))))
(putprop! 'MORE 'FEATURES '(NUMD NUMDAN))

(putprop! 'MOST 'SEMANTICS '((NUMD (list '< (inc NUM)))))
(putprop! 'MOST 'FEATURES '(NUMD NUMDAT DET QNTFR NPL NONUM))

(putprop! 'MOVE 'SEMANTICS '((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) PROCEDUREß ((!PUT !2 PLACE *TIME)) MARKERSß ((!MOTION)))))))))
(putprop! 'MOVE 'FEATURES '(VB INF TRANS))

(putprop! 'MY 'IRREGULAR '(I (POSS) (SUBJ)))

(putprop! 'NAME 'SEMANTICS '((NOUN (OBJECT ((!NAME !ROLE) ((IS *** !NAME) (!CALL ? ***) (!ROLE (!THING) (!CALL !2 !1))))))
           (VB ((TRANS (!NAME))))))
(putprop! 'NAME 'FEATURES '(NOUN NS VB INF TRANS))

(putprop! 'NARROW 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !WIDTH (128 0 0) ***)))))
           (MEASURE (MEASURE DIMENSIONß !WIDTH RESTRICTIONSß (!PHSYOB) DIRECTIONß nil))))
(putprop! 'NARROW 'FEATURES '(ADJ))

(putprop! 'NEITHER 'FEATURES '(B-SPECIAL))
(putprop! 'NEITHER 'SEMANTICS true)
(putprop! 'NEITHER 'B-SPECIAL '(BOTH NOR))

(putprop! 'NICE 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!THING) PROCEDUREß ((!LIKE ßFRIEND ***)))))))
(putprop! 'NICE 'FEATURES '(ADJ))

(putprop! 'NO 'SEMANTICS '((DET 'NO)))
(putprop! 'NO 'FEATURES '(DET QNTFR NS NPL))

(putprop! 'NONE 'SEMANTICS '((DET 'NO)))
(putprop! 'NONE 'FEATURES '(DET QNTFR NPL NS NONUM))

(putprop! 'NOR 'FEATURES '(SPECIAL))
(putprop! 'NOR 'SEMANTICS true)
(putprop! 'NOR 'SPECIAL '(CONJ))

(putprop! 'NOT 'SEMANTICS '((ADV true)))
(putprop! 'NOT 'FEATURES '(ADV NEG))

(putprop! 'NOTHING 'SEMANTICS '((TPRON 'NO)))
(putprop! 'NOTHING 'FEATURES '(TPRON NEG NS))

(putprop! 'NOW 'SEMANTICS '((ADV (or (= (cadr (assq 'TIME *fe*)) 'ßNOW) (ert "NOW DEFINITION")))))
(putprop! 'NOW 'FEATURES '(ADV TIMW))

(putprop! 'OBJECT 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!PHYSOB !VAGUE) PROCEDUREß ((!PHYSOB ***)))))))
(putprop! 'OBJECT 'FEATURES '(NOUN NS))

(putprop! 'OF 'SEMANTICS '((PREP
        (and (cq 'NG *fe*)
            (RELATION
                (RESTRICTIONSß (((!DIRECTION)) ((!PHYSOB)))
                PROCEDUREß ((!EVAL (list '!DIRECTION
                                (cadr (SETQ XX (or (assq '!DIRECTION (cddaar (INTERP MAP1))) (ert "OF DEFINITION"))))
                                (COND ((caddr XX) '*OF) ('!2)) (COND ((caddr XX) '!2) ('*OF)) '*TIME)))))))
           (PREP2 true)))
(putprop! 'OF 'FEATURES '(PREP PREP2 OF))

(putprop! 'OFF 'SEMANTICS '((PRT true)))
(putprop! 'OFF 'FEATURES '(PRT))

(putprop! 'ON 'SEMANTICS '((PREP (!ON))))
(putprop! 'ON 'FEATURES '(PREP PLACE))

(putprop! 'ON-TOP-OF 'ROOT '(ON TOP OF))
(putprop! 'ON-TOP-OF 'SEMANTICS '(!ON))
(putprop! 'ON-TOP-OF 'FEATURES '(PREP COMBINATION))

(putprop! 'ONE 'SEMANTICS '((NOUN (SMONE)) (NUM 1)))
(putprop! 'ONE 'FEATURES '(NUM NOUN NS))

(putprop! 'ONLY 'SEMANTICS '((NUMD (list 'EXACTLY NUM))))
(putprop! 'ONLY 'FEATURES '(NUMD NUMDALONE))

(putprop! 'ONTO 'SEMANTICS '((PREP (!ON))))
(putprop! 'ONTO 'FEATURES '(PREP PLACE))

(putprop! 'OR 'FEATURES '(SPECIAL))
(putprop! 'OR 'SEMANTICS true)
(putprop! 'OR 'SPECIAL '(CONJ))

(putprop! 'OUT 'SEMANTICS '((PRT true)))
(putprop! 'OUT 'FEATURES '(PRT))

(putprop! 'OUT-OF 'ROOT '(OUT OF))
(putprop! 'OUT-OF 'SEMANTICS '(!OUTOF))
(putprop! 'OUT-OF 'FEATURES '(PREP COMBINATION))

(putprop! 'OVER 'SEMANTICS '((PREP (!LOC !ABOVE true))))
(putprop! 'OVER 'FEATURES '(PREP PLACE))

(putprop! 'PICK 'SEMANTICS '((VB ((TRANS (!NOTICE))))))
(putprop! 'PICK 'FEATURES '(VPRT VB INF TRANS))

(putprop! 'PICK-UP 'ROOT '(PICK UP))
(putprop! 'PICK-UP 'SEMANTICS '((TRANS (RELATION
            (RESTRICTIONSß (((!ANIMATE)) ((!MANIP)))
             MARKERSß (!EVENT)
             PROCEDUREß ((!EVAL (if (memq (num? SMOB1) '(1 NS)) '(!PICKUP !2 *TIME) '(!PUTIN !2 ßBOX *TIME)))))))))
(putprop! 'PICK-UP 'FEATURES '(COMBINATION TRANS))

(putprop! 'PLEASE 'FEATURES '(B-SPECIAL))
(putprop! 'PLEASE 'SEMANTICS true)
(putprop! 'PLEASE 'B-SPECIAL '(FLUSHME))

(putprop! 'POINTED 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB !POINTED) PROCEDUREß ((!SHAPE *** !POINTED)))))))
(putprop! 'POINTED 'FEATURES '(ADJ))

(putprop! 'PUT 'PAST 'PUT)
(putprop! 'PUT 'SEMANTICS '((VB
        ((TRANSL (RELATION
              (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB)) (SMOBL (!PLACE)))
               MARKERSß (!EVENT)
               PROCEDUREß (!EVAL
                   (doall (map #'(lambda [%PLNRPHRASE]
                        (COND ((= (car %PLNRPHRASE) '!ON) (list '!PUTON '!2 (cadr %PLNRPHRASE) '*TIME))
                            ((= (car %PLNRPHRASE) '!IN) (list '!PUTIN '!2 (cadr %PLNRPHRASE) '*TIME))
                            ((ert "PUT DEFINITION"))))
                        (relations? SMOBL)))))))))))
(putprop! 'PUT 'FEATURES '(INF PAST VB TRANSL VPRT))

(putprop! 'PUT-AWAY 'ROOT '(PUT AWAY))
(putprop! 'PUT-AWAY 'SEMANTICS '((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTIN !2 ßBOX *TIME)))))))
(putprop! 'PUT-AWAY 'FEATURES '(COMBINATION TRANS))

(putprop! 'PUT-DOWN 'ROOT '(PUT DOWN))
(putprop! 'PUT-DOWN 'SEMANTICS '((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTON !2 ßTABLE *TIME)))))))
(putprop! 'PUT-DOWN 'FEATURES '(COMBINATION TRANS))

(putprop! 'PUT-TOGETHER 'ROOT '(PUT TOGETHER))
(putprop! 'PUT-TOGETHER 'SEMANTICS '((TRANS (!BUILD))))
(putprop! 'PUT-TOGETHER 'FEATURES '(COMBINATION TRANS))

(putprop! 'PYRAMID 'FEATURES '(NOUN NS))
(putprop! 'PYRAMID 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!PHYSOB !POINTED) PROCEDUREß ((!IS *** !PYRAMID)))))))

(putprop! 'RED 'SEMANTICS '((ADJ (!COLOR !RED))))
(putprop! 'RED 'FEATURES '(ADJ))

(putprop! 'RELEASE 'FEATURES '(VB TRANS INF))

(putprop! 'RIGHT 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!DIRECTION) PROCEDUREß ((!DIRECTION !RIGHT true)))))))
(putprop! 'RIGHT 'FEATURES '(NOUN NS))

(putprop! 'ROUND 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB !ROUND) PROCEDUREß ((!SHAPE *** !ROUND)))))))
(putprop! 'ROUND 'FEATURES '(ADJ))

(putprop! 'SAW 'IRREGULAR '(SEE (PAST) (INF)))

(putprop! 'SEE 'FEATURES '(VB INF TRANS))

(putprop! 'SET 'SEMANTICS '((VB true)))
(putprop! 'SET 'FEATURES '(VB INF))

(putprop! 'SET-DOWN 'ROOT '(SET DOWN))
(putprop! 'SET-DOWN 'SEMANTICS '((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!PUTON !2 ßTABLE *TIME)))))))
(putprop! 'SET-DOWN 'FEATURES '(COMBINATION TRANS))

(putprop! 'SHAPE 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!SHAPE) PROCEDUREß ((!IS *** !SHAPE)))))))
(putprop! 'SHAPE 'FEATURES '(NOUN NS))

(putprop! 'SHE 'FEATURES '(PRON SUBJ NS))

(putprop! 'SHORT 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !HEIGHT RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !HEIGHT (128 0 0) ***)))))))
(putprop! 'SHORT 'FEATURES '(ADJ))

(putprop! 'SHRDLU 'REFER 'ßSHRDLU)

(putprop! 'SINCE 'SEMANTICS '((BINDER (smbinder *end* nil))))
(putprop! 'SINCE 'FEATURES '(BINDER TIME))

(putprop! 'SIT 'SEMANTICS '((VB
        ((ITRNSL (RELATION
              (RESTRICTIONSß (((!PHYSOB)) (SMOBL (!PLACE)))
               PROCEDUREß (!EVAL
                   (doall (map #'(lambda [%PLNRPHRASE]
                        (COND ((memq (car %PLNRPHRASE) '(!ON !IN)) (list '!SUPPORT (cadr %PLNRPHRASE) '!1 '*TIME))
                            ((ert "SIT DEFINITION"))))
                        (relations? SMOBL)))))))))))
(putprop! 'SIT 'FEATURES '(VB INF ITRNSL))

(putprop! 'SIZE 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!SIZE) PROCEDUREß ((!IS *** !SIZE)))))))
(putprop! 'SIZE 'FEATURES '(NOUN NS))

(putprop! 'SMALL 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !SIZE RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))
           (ADJ (OBJECT (MARKERSß (!PHYSOB !LITTLE) PROCEDUREß ((!MORE !SIZE (128 128 128) ***)))))))
(putprop! 'SMALL 'FEATURES '(ADJ))

(putprop! 'SOME 'SEMANTICS '((DET 'INDEF)))
(putprop! 'SOME 'FEATURES '(DET QNTFR NS NPL NONUM))

(putprop! 'SOMETHING 'SEMANTICS '((TPRON 'INDEF)))
(putprop! 'SOMETHING 'FEATURES '(TPRON NS))

(putprop! 'SPHERE 'FEATURES '(NOUN NS))

(putprop! 'SQUARE 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB !RECTANGULAR) PROCEDUREß ((!SHAPE ** !RECTANGULAR)))))))
(putprop! 'SQUARE 'FEATURES '(ADJ))

(putprop! 'STACK 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!STACK) PROCEDUREß ((!IS *** !STACK)))))
           (VB ((TRANS (!STACKUP))))))
(putprop! 'STACK 'FEATURES '(NOUN NS VB INF VPRT TRANS))

(putprop! 'STACK-UP 'ROOT '(STACK UP))
(putprop! 'STACK-UP 'SEMANTICS '((TRANS (!STACKUP))))
(putprop! 'STACK-UP 'FEATURES '(COMBINATION TRANS))

(putprop! 'START 'SEMANTICS '((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!START !2 *TIME)))))))))
(putprop! 'START 'FEATURES '(VB INF TRANS INGOB1 TOOB1))

(putprop! 'SUPPORT 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!PHYSOB !ROLE) PROCEDUREß ((!SUPPORT *** ?) (!ROLE (!PHYSOB) (!SUPPORT !1 !2))))))
           (VB ((TRANS (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!MANIP))) PROCEDUREß ((!SUPPORT !1 !2 *TIME)))))))))
(putprop! 'SUPPORT 'FEATURES '(VB INF TRANS IMPERF NOUN NS))

(putprop! 'TABLE 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!TABLE) PROCEDUREß ((!IS *** !TABLE)))))))
(putprop! 'TABLE 'FEATURES '(NOUN NS))

(putprop! 'TAKE 'FEATURES '(VB INF TRANSL TRANS))

(putprop! 'TALL 'SEMANTICS '((MEASURE (MEASURE DIMENSIONß !HEIGHT RESTRICTIONSß (!PHYSOB) DIRECTIONß true))
           (ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !HEIGHT *** (128 0 0))))))))
(putprop! 'TALL 'FEATURES '(ADJ))

(putprop! 'TELL 'SEMANTICS '((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!WANT !1 !2 *TIME)))))))))
(putprop! 'TELL 'FEATURES '(VB INF TRANS2 TOOB2))

(putprop! 'THAN 'SEMANTICS '((nil? true)))
(putprop! 'THAN 'FEATURES '(THAN))

(putprop! 'THANK 'FEATURES '(B-SPECIAL))
(putprop! 'THANK 'SEMANTICS '(THANK))
(putprop! 'THANK 'B-SPECIAL '(THANK))

(§ defn- THANK []
    (when (= (cadr N) 'YOU)
        (print "YOU'RE WELCOME")
        (FLUSHME)
        (FLUSHME)
        (or NN (ß_G))
        (SETQ SPECIAL 'DONE)))

(putprop! 'THAT 'SEMANTICS '((PRONREL true) (DET (SMTHAT)) (nil? true)))
(putprop! 'THAT 'FEATURES '(NS THAT DET DEM DEF PRONREL INCOM))

(putprop! 'THE 'SEMANTICS '((DET true)))
(putprop! 'THE 'FEATURES '(DET NPL NS DEF))

(putprop! 'THEIR 'IRREGULAR '(THEY (POSS) nil))

(putprop! 'THEM 'IRREGULAR '(THEY (OBJ) (SUBJ)))

(def- lastime nil)

(putprop! 'THEN 'SEMANTICS '((ADV
        (and lastime
             (RPLACD (cddadr (or (and (SETQ XX (assq 'TIME *fe*)) (not (term? (cadr XX))) XX) '(TIME (!TIME (PAST) nil))))
                 (list (or (cadddr lastime) (car (cddddr lastime))) (or (car (cddddr lastime)) (cadddr lastime))))))))
(putprop! 'THEN 'FEATURES '(ADV TIMW))

(putprop! 'THERE 'SEMANTICS '((ADV true)))
(putprop! 'THERE 'FEATURES '(ADV PLACE))

(putprop! 'THEY 'SEMANTICS '((PRON (SMIT 'THEY))))
(putprop! 'THEY 'FEATURES '(PRON SUBJ NPL))

(putprop! 'THICK 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !THICKNESS *** (0 128 0))))))
            (MEASURE (MEASURE DIMENSIONß !THICKNESS RESTRICTIONSß (!PHYSOB) DIRECTIONß true))))
(putprop! 'THICK 'FEATURES '(ADJ))

(putprop! 'THIN 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !THICKNESS (0 128 0) ***)))))
           (MEASURE (MEASURE DIMENSIONß !THICKNESS RESTRICTIONSß (!PHYSOB) DIRECTIONß nil))))
(putprop! 'THIN 'FEATURES '(ADJ))

(putprop! 'THING 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!THING !VAGUE !PHYSOB) PROCEDUREß ((!PHYSOB  *** )))))))
(putprop! 'THING 'FEATURES '(NOUN NS))

(putprop! 'THIS 'FEATURES '(NS DET DEM DEF))

(putprop! 'THREE 'SEMANTICS '((NUM 3)))
(putprop! 'THREE 'FEATURES '(NUM))

(putprop! 'TIME 'FEATURES '(NOUN NS TIM1))

(putprop! 'TO 'SEMANTICS '((PREP (RELATION (RESTRICTIONSß (((!PHYSOB)) ((!DIRECTION))) PROCEDUREß ((!EVAL (SUBTOP '!1 '*OF (REFERENCE? SMOB1)))))))))
(putprop! 'TO 'FEATURES '(PREP))

(putprop! 'TOGETHER 'SEMANTICS '((PRT true)))
(putprop! 'TOGETHER 'FEATURES '(PRT))

(putprop! 'TOLD 'IRREGULAR '(TELL (PAST) (INF)))

(putprop! 'TOP 'SEMANTICS '((PREP2 true)))
(putprop! 'TOP 'FEATURES '(PREP2))

(putprop! 'TOUCH 'SEMANTICS '((VB ((TRANS (!GRASP))))))
(putprop! 'TOUCH 'FEATURES '(VB INF TRANS))

(putprop! 'TOY 'SEMANTICS '((NOUN (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MANIP ***)))))))
(putprop! 'TOY 'FEATURES '(NOUN NS))

(putprop! 'TWO 'SEMANTICS '((NUM 2)))
(putprop! 'TWO 'FEATURES '(NUM))

(putprop! 'UNDER 'SEMANTICS '((PREP (!LOC !ABOVE nil))))
(putprop! 'UNDER 'FEATURES '(PREP PLACE))

(putprop! 'UNDERNEATH 'SEMANTICS '((PREP ((!LOC !ABOVE nil)))))
(putprop! 'UNDERNEATH 'FEATURES '(PREP PLACE))

(putprop! 'UP 'SEMANTICS '((PRT true)))
(putprop! 'UP 'FEATURES '(PRT))

(putprop! 'US 'IRREGULAR '(WE (OBJ) (SUBJ)))

(putprop! 'WANT 'SEMANTICS '((VB ((TRANS (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!EVENT))) MARKERSß (!EVENT) PROCEDUREß ((!WANT !1 !2 *TIME)))))))))
(putprop! 'WANT 'FEATURES '(VB INF TRANS TOOB SUBTOB))

(putprop! 'WAS 'IRREGULAR '(BE (V3PS VFS PAST) (INF)))

(putprop! 'WE 'SEMANTICS '((PRON (SMSET (list (newcopy 'WE-OSS))))))
(putprop! 'WE 'FEATURES '(PRON NPL SUBJ))

(putprop! 'WERE 'IRREGULAR '(BE (VPL PAST) (INF)))

(putprop! 'WHAT 'SEMANTICS '((DET true) (PRON (SMSET (list (newcopy 'UNKNOWN-OSS))))))
(putprop! 'WHAT 'FEATURES '(QDET DET NPL PRON QPRON NS))

(putprop! 'WHATEVER 'FEATURES '(PRON EVERPRON NS))

(putprop! 'WE 'REFER '(ßSHRDLU ßFRIEND))

(putprop! 'WHERE 'SEMANTICS '((QADJ (FQ WHERE))))
(putprop! 'WHERE 'FEATURES '(QADJ PLACE))

(putprop! 'WHEREVER 'FEATURES '(PRON EVERPRON NS))

(putprop! 'WHEN 'SEMANTICS '((BINDER (smbinder *start* *end*)) (QADJ (FQ WHEN))))
(putprop! 'WHEN 'FEATURES '(QADJ BINDER TIME))

(putprop! 'WHENEVER 'FEATURES '(BINDER))

(putprop! 'WHICH 'SEMANTICS '((PRONREL true) (DET true)))
(putprop! 'WHICH 'FEATURES '(QDET DET PRONREL NS NPL))

(putprop! 'WHICHEVER 'FEATURES '(DET RSQDET NS NPL))

(putprop! 'WHILE 'SEMANTICS '((BINDER (smbinder *start* *end*))))
(putprop! 'WHILE 'FEATURES '(BINDER TIME))

(putprop! 'WHITE 'SEMANTICS '((ADJ (!COLOR !WHITE))))
(putprop! 'WHITE 'FEATURES '(ADJ))

(putprop! 'WHO 'SEMANTICS '((PRONREL true) (PRON (SMSET (list (newcopy ANIMATE-OSS))))))
(putprop! 'WHO 'FEATURES '(PRONREL QPRON PRON NS))

(putprop! 'WHOEVER 'FEATURES '(PRON EVERPRON NS))

(putprop! 'WHOSE 'FEATURES '(DET QDET NPL NS))

(putprop! 'WHY 'SEMANTICS '((QADJ (FQ WHY))))
(putprop! 'WHY 'FEATURES '(QADJ))

(putprop! 'WHYEVER 'FEATURES '(PRON EVERPRON NS))

(putprop! 'WIDE 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB) PROCEDUREß ((!MORE !WIDTH *** (0 128 0))))))
            (MEASURE (MEASURE DIMENSIONß !WIDTH RESTRICTIONSß (!PHYSOB) DIRECTIONß true))))
(putprop! 'WIDE 'FEATURES '(ADJ))

(putprop! 'WILL 'SEMANTICS '((VB true)))
(putprop! 'WILL 'FEATURES '(VB AUX WILL MODAL V3PS VFS VPL))

(putprop! 'WITH 'FEATURES '(PREP))

(putprop! 'WOULD 'SEMANTICS '((VB true)))
(putprop! 'WOULD 'FEATURES '(VB AUX MODAL))

(putprop! 'YOU 'SEMANTICS '((PRON (SMSET (list (newcopy 'SHRDLU-OSS))))))
(putprop! 'YOU 'FEATURES '(PRON NPL NS SUBJ OBJ))

(putprop! 'YOUR 'IRREGULAR '(YOU (POSS) nil))

;; ############################################################
;;
;;                          !WORDS
;;
;; ############################################################

(putprop! '!ANIMATE 'SYSTEM '(!ROBOT !PERSON))
(putprop! '!ANIMATE 'SYS '(!THING))

(putprop! '!ASMUCH 'THMLIST '((4 '((THUSE TC-ASMUCH)))))

(putprop! '!BELONG 'THMLIST '((3 '((THUSE TC-BELONG)))))

(putprop! '!BLACK 'SYS '(!SPECTRUM))

(putprop! '!BLUE 'SYS '(!SPECTRUM))

(§ defn- !BLUEPRINT [x]
    (let [PARTS nil]
        (COND ((getprop x 'REFER) (RETURN '!2))
            ((nil? (SETQ x (cddaar (INTERP x))))
                (GO DONE)))
    LOOP (COND ((not (= (caar x) 'THGOAL)) (ert "!BLUEPRINT THGOAL"))
            ((= (caadar x) '!IS))
            ((= (caadar x) '!PART) (SETQ PARTS (cons (cadr (cadar x)) PARTS)))
            ((ert "!BLUEPRINT")))
        (and (SETQ x (cdr x)) (GO LOOP))
    DONE (and PARTS
            (getprop (car PARTS) 'REFER)
            (RETURN (getprop (car PARTS) 'REFER)))
        (putprop! 'BLUEPRINT 'SM
            (COND ((nil? PARTS) (getprop 'STACKPARTS 'SM))
                ((cdr PARTS) (ert "!BLUEPRINT PARTS"))
                ((getprop (car PARTS) 'SM))))
        'BLUEPRINT))

(putprop! '!BOX 'SYS '(!PHYSOB))

(§ defn- !BUILD []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!STACK))) MARKERSß (!EVENT) PROCEDUREß ((!EVAL (list '!STACKUP (!BLUEPRINT SMOB1) '*TIME))))))

(putprop! '!CALL 'THMLIST '((3 '((THUSE TC-3)))))

(putprop! '!COLOR 'PRIORITY 192)
(putprop! '!COLOR 'SYS '(!PROPERTY))

(§ defq- !COLOR [a]
    (eval (SUBST (car a) 'COLOR '(OBJECT (MARKERSß (!PHYSOB COLOR) PROCEDUREß ((!COLOR *** COLOR)))))))

(putprop! '!CONSTRUCT 'SYSTEM '(!STACK !ROW))
(putprop! '!CONSTRUCT 'SYS '(!PHYSOB))

(putprop! '!CONTAIN 'PRIORITY -1)

(§ defn- !CLEANOFF []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!CLEARTOP !2 *TIME)))))

(putprop! '!CLEARTOP 'NAMEVENT '(I3 (cons (vbfix 'CLEAN false) (prtput 'OFF OBJ1))))
(putprop! '!CLEARTOP 'THMLIST '((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(§ defn- !DEFINE [x y]
    (list '!DEFINITION
        (cadadr (cdaar (INTERP x)))
        (let [x (gensym 'ATM)]
            (putprop! x 'NEWWORD (INTERP y))
            x)))

(putprop! '!DEFINITION 'NOGOAL true)

(§ defq- !DEFINITION [a]
    (putprop! (cadar a) 'WORD '(NOUN NS))
    (putprop! (cadar a) 'SMNTC
        (SUBST (SUBST '*** (caddr (getprop (cadr a) 'NEWWORD)) (car (getprop (cadr a) 'NEWWORD)))
            'NG
            '((NOUN (SETQ LIST2 (list (SUBST (SUBST (caddar LIST1) '*** 'NG) (caar LIST1) (car LIST1)))))))))

(putprop! '!DIRECTION 'NOGOAL true)

(putprop! '!END 'THMLIST '((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(putprop! '!EQDIM 'NOGOAL true)

(§ defn- !EQDIM [x]
    (SETQ x (size x))
    (and (= (car x) (cadr x)) (= (car x) (caddr x))))

(putprop! '!EQUIV 'PRIORITY 512)

(putprop! '!EVENT 'SYS '(!SYSTEMS))

(putprop! '!EXISTS 'THMLIST '((2 '((THUSE TC-EXISTS))) (3 '((THUSE TCT-EXISTS)))))

(putprop! '!GET-RID-OF 'THMLIST '((2 '((THUSE TCT-EXISTS))) (3 '((THUSE THUSE TCT-3))) (4 '((THUSE TCTE-4)))))
(putprop! '!GET-RID-OF 'NAMEVENT '(I3 (concat (list (vbfix 'GET true) 'RID 'OF) OBJ1)))

(putprop! '!GRASP 'NAMEVENT '(I3 (cons (vbfix 'GRASP false) OBJ1)))
(putprop! '!GRASP 'THMLIST '((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(§ defn- !GRASP []
    (RELATION
        (RESTRICTIONSß (((!ANIMATE)) ((!MANIP)))
        MARKERSß (!EVENT)
        PROCEDUREß ((!EVAL (if (istense *c* 'IMPERF) '(!GRASPING !2 *TIME) '(!GRASP !2 *TIME)))))))

(putprop! '!GRASPING 'THMLIST '((3 '((THUSE TCT-GRASPING)))))

(putprop! '!GREEN 'SYS '(!SPECTRUM))

(putprop! '!HAND 'SYS '(!PHYSOB))

(§ defn- !HAVE []
    (RELATION
        (RESTRICTIONSß (((!THING)) ((!THING) (and (memq '!ROLE (markers? SMOB1)) (CHECK (cadr (assq '!ROLE (relations? SMOB1))) (markers? SMSUB) (systems? SMSUB)))))
            PROCEDUREß ((!SUBST !1 ?)))
        (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB)))
            PROCEDUREß ((!BELONG !2 !1)))))

(putprop! '!HEIGHT 'MEASFN #(caddr (size %)))

(§ defn- !IN []
    (COND ((cq 'LOBJ *fe*)
        (RELATION
            (RESTRICTIONSß (((!THING)) ((!BOX))) MARKERSß (!PLACE) PROCEDUREß ((!IN !2)))))
        ((RELATION
            (RESTRICTIONSß (((!MANIP)) ((!BOX))) PROCEDUREß ((!CONTAIN !2 !1 *TIME)))
            (RESTRICTIONSß (((!MANIP)) ((!HAND))) PROCEDUREß ((!GRASPING !1 *TIME)))
            (RESTRICTIONSß (((!PLACE)) ((!BOX))) PROCEDUREß ((!IN !1 !2)))
            (RESTRICTIONSß (((!MANIP)) ((!CONSTRUCT))) PROCEDUREß ((!PART !1 !2 *TIME)))))))

(putprop! '!IS 'PRIORITY 64)

(putprop! '!LIKE 'TELLABLE true)
(putprop! '!LIKE 'THMLIST '((3 '((THTBF thtrue)))))

(putprop! '!LOC 'THMLIST '((4 '((THUSE TC-LOC))) (5 '((THUSE TCT-LOC)))))

(§ defq- !LOC [a] (!LOC2 (car a) (cadr a)))

(§ defn- !LOC2 [LOCTYPE !LOC]
    (COND ((cq 'LOBJ *fe*)
        (RELATION (RESTRICTIONSß (((!THING)) (LOBJ (!PHYSOB))) MARKERSß (!PLACE) PROCEDUREß ((!EVAL (list '!LOC LOCTYPE !LOC !2))))))
    ((RELATION (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PROCEDUREß ((!EVAL (list '!LOC LOCTYPE (COND (!LOC '!1) ('!2)) (COND (!LOC '!2) ('!1)) '*TIME))))))))

(putprop! '!MANIP 'SYS '(!PHYSOB))

(putprop! '!MORE 'THMLIST '((4 '((THUSE TC-MORE)))))

(putprop! '!NAME 'THMLIST '((2 '((THUSE TC-2)))))
(putprop! '!NAME 'SYS '(!SYSTEMS))

(§ defn- !NAME []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!NAME !2)))))

(putprop! '!NEWWORD 'SYS '(!THING))

(putprop! '!NOTICE 'THMLIST '((2 '((THUSE TC-2)))))

(§ defn- !NOTICE []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB))) MARKERSß (!EVENT) PROCEDUREß ((!NOTICE !2 *TIME)))))

(putprop! '!ON 'THMLIST '((3 '((THUSE TC-ON))) (4 '((THUSE TCT-ON)))))

(§ defn- !ON []
    (COND ((cq 'LOBJ *fe*)
        (RELATION (RESTRICTIONSß (((!THING)) ((!PHYSOB))) MARKERSß (!PLACE) PROCEDUREß ((!ON !2)))))
        ((RELATION
            (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PARAPHRASEß (ANYWHERE ON TOP OF) PROCEDUREß ((!ON !1 !2 *TIME)))
            (RESTRICTIONSß (((!PHYSOB)) ((!MANIP))) PARAPHRASEß (DIRECTLY ON THE SURFACE) PROCEDUREß ((!SUPPORT !2 !1 *TIME)))
            (RESTRICTIONSß (((!PLACE)) ((!PHYSOB))) PROCEDUREß ((!ON !1 !2)))))))

(putprop! '!PACK 'THMLIST '((3 '((THUSE TC-3)))))

(putprop! '!PART 'THMLIST '((3 '((THUSE TC-PART)))))                       ;; PERFORMED ON GRADUATION DAY, JUNE 2, 1972 BY JMH

(putprop! '!PERSON 'SYS '(!ANIMATE))

(putprop! '!PICKUP 'THMLIST '((2 '((THUSE TC-2))) (3 '((THUSE TCT-PICKUP))) (4 '((THUSE TCTE-PICKUP)))))
(putprop! '!PICKUP 'NAMEVENT '(I3 (cons (vbfix 'PICK false) (prtput 'UP OBJ1))))

(putprop! '!PLACE 'SYS '(!SYSTEMS))

(putprop! '!PUT 'THMLIST '((3 '((THUSE TCT-3))) (4 '((THUSE TCT-PUT))) (5 '((THUSE TCTE-PUT)))))

(putprop! '!PUTIN 'THMLIST '((3 '((THUSE TC-3))) (4 '((THUSE TCT-4))) (5 '((THUSE TCT-5)))))

(putprop! '!PUTON 'NAMEVENT '(I4 (cons (vbfix 'PUT true) (concat OBJ1 '(ON) OBJ2))))
(putprop! '!PUTON 'THMLIST '((3 '((THUSE TC-3))) (4 '((THUSE TCT-4))) (5 '((THUSE TCTE-5)))))

(putprop! '!RAISE 'THMLIST '((1 '((THUSE TC-RAISE)))))

(putprop! '!RECTANGULAR 'SYS '(!SHAPES))

(putprop! '!REFERS 'THMLIST '((2 '((THUSE TC-REFERS)))))

(putprop! '!PHYSOB 'SYSTEM '(!BOX !CONSTRUCT !HAND !MANIP !TABLE))
(putprop! '!PHYSOB 'SYS '(!THING))
(putprop! '!PHYSOB 'THMLIST '((2 '((THUSE TC-PHYSOB)))))

(§ defn- !PROPDEFINE [x]
    (putprop! x 'FEATURES '(PROPN NS))               ;; CHANGED TO FEATURES FROM 'WORD' IN THE OLD DICTIONARY
    (putprop! x 'SEMANTICS '((PROPN T))))

(putprop! '!PROPERTY 'SYSTEM '(!COLOR !SIZE !SHAPE))
(putprop! '!PROPERTY 'SYS '(!THING))

(putprop! '!POINTED 'SYS '(!SHAPES))

(putprop! '!RED 'SYS '(!SPECTRUM))

(putprop! '!RELATION 'SYS '(!SYSTEMS))

(putprop! '!ROLE 'NOGOAL true)

(§ defq- !ROLE [_] true)

(putprop! '!ROUND 'SYS '(!SHAPES))

(putprop! '!ROW 'SYS '(!CONSTRUCT))

(putprop! '!ROBOT 'SYS '(!ANIMATE))

(putprop! '!SIZE 'MEASFN '(lambda [x] (reduce + (size x))))
(putprop! '!SIZE 'SYS '(!PROPERTY))

(putprop! '!SHAPE 'PRIORITY 128)
(putprop! '!SHAPE 'SYS '(!PROPERTY))

(putprop! '!STACK 'SYS '(!CONSTRUCT))

(putprop! '!STACKUP 'THMLIST '((2 '((THUSE TC-2)))))
(putprop! '!STACKUP 'NAMEVENT '(I3 (cons (vbfix 'STACK false) (prtput 'UP OBJ1))))

(§ defn- !STACKUP []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!STACKUP !2 *TIME)))))

(putprop! '!START 'THMLIST '((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(putprop! '!SUBST 'NOGOAL true)

(putprop! '!SUPPORT 'PRIORITY 256)
(putprop! '!SUPPORT 'THMLIST '((3 nil) (4 '((THUSE TCT-SUPPORT)))))

(putprop! '!SYSTEMS 'SYSTEM '(!THING !EVENT !NAME !RELATION !PLACE))

(putprop! '!TABLE 'SYS '(!PHYSOB))

(putprop! '!THICKNESS 'MEASFN '(lambda [x] (cadr (size x))))

(putprop! '!THING 'SYS '(!SYSTEMS))
(putprop! '!THING 'SYSTEM '(!ANIMATE !NAME !PHYSOB !PROPERTY))

(putprop! '!UNGRASP 'THMLIST '((1 '((THUSE TC-UNGRASP)))))
(putprop! '!UNGRASP 'NAMEVENT '(I3 (concat (list (vbfix 'LET true) 'GO 'OF) OBJ1)))

(putprop! '!WANT 'THMLIST '((4 '((THUSE TC-WANT4))) (5 '((THUSE TC-WANT5)))))

(putprop! '!WHITE 'SYS '(!SPECTRUM))

(putprop! '!WIDTH 'MEASFN '(lambda [x] (car (size x))))

;; ############################################################
;;
;;                     PARTS OF SPEECH
;;
;; ############################################################

(putprop! 'ADJ 'ELIM '(ADJ SUP COMPAR))
(putprop! 'ADV 'ELIM '(ADV PREPADV TIMW TIM2 ADVADV VBAD PLACE LOBJ))
(putprop! 'BINDER 'ELIM '(BINDER TIME))
(putprop! 'CLASF 'ELIM '(CLASF))
(putprop! 'DET 'ELIM '(DET NPL NS PART DEF INDEF NEG DEM INCOM OFD QNTFR NONUM QDET))
(putprop! 'NOUN 'ELIM '(NOUN POSS MASS NPL NS TIM1 TIME MONTH))
(putprop! 'NUM 'ELIM '(NUM NPL NS))
(putprop! 'NUMD 'ELIM '(NUMD NUMDAN NUMDAT NUMDALONE))
(putprop! 'ORD 'ELIM '(ORD TIMORD))
(putprop! 'POSS 'ELIM '(NOUN NPL NS MASS NFS PRON))
(putprop! 'PREP 'ELIM '(PREP MOTOR PLACE NEED2))
(putprop! 'PREP2 'ELIM '(PREP2))
(putprop! 'PRON 'ELIM '(PRON QPRON EVERPRON POSS SUBJ OBJ NS NPL NFS NEG DEFPOSS))
(putprop! 'PRT 'ELIM '(PRT))
(putprop! 'QADJ 'ELIM '(PLACE QADJ))
(putprop! 'PROPN 'ELIM '(PROPN POSS NS NPL))
(putprop! 'TPRON 'ELIM '(TPRON NS NPL NEG ANY))
(putprop! 'VB 'ELIM '(VB MVB AUX QAUX MODAL WILL BE DO HAVE ING EN INF V3PS QUOTING VFS VPL PAST PRESENT NEG ITRNS TRANS TRANSL TRANS2 TRANSL2 INT ITRNSL INGOB TOOB SUBTOB REPOB INGOB2 TOOB2 SUBTOB2 REPOB2 VPRT TO2 TRANSINT TOOB1 INGOB1 REPOB1))

;; ############################################################
;;
;;     I'M NOT QUITE SURE WHAT TO DO WITH THIS RANDOM STUFF
;;
;; ############################################################

(putprop! 'D 'MOD '((PAST EN) (INF MODAL AUX)))
(putprop! 'G 'MOD '((ING) (INF)))
(putprop! 'R 'MOD '((COMPAR) nil))
(putprop! 'T 'MOD '((SUP) nil))
(putprop! 'N 'MOD '((EN) (INF)))
(putprop! 'S 'MOD '((PRESENT V3PS NPL) (NS INF MODAL AUS MAS)))
(putprop! '* 'MOD '((NEG) (nil)))

(putprop! 'THAMONG 'NOGOAL true)
(putprop! 'THSETQ 'NOGOAL true)
(putprop! 'THGOAL 'NOGOAL true)
(putprop! 'THOR 'NOGOAL true)
(putprop! 'THNOT 'NOGOAL true)
(putprop! 'THAND 'NOGOAL true)
(putprop! 'THPROG 'NOGOAL true)
(putprop! 'THFIND 'NOGOAL true)

;; ############################################################
;;
;;                     PRE-BUILT OSS'S
;;
;; ############################################################

(putprop! 'ANIMATE-OSS 'OSSNODE= 'ANIMATE-OSS)
(putprop! 'ANIMATE-OSS 'MARKERS= '(!ANIMATE !THING !SYSTEMS))
(putprop! 'ANIMATE-OSS 'RELATIONS= '((!IS ($? ANIM) ?)))
(putprop! 'ANIMATE-OSS 'SYSTEMS= '(!THING !SYSTEMS))
(putprop! 'ANIMATE-OSS 'DETERMINER= '(SG-PL INDEF WHICH))
(putprop! 'ANIMATE-OSS 'VARIABLE= 'ANIM)

(putprop! 'FAKE-AGENT 'FEATURES '(NG INDEF SG-PL))
(putprop! 'FAKE-AGENT 'SEMANTICS '(UNKNOWN-OSS-BY))
(putprop! 'FAKE-AGENT 'PARENT '(FAKE-BY-PHRASE))

(putprop! 'FAKE-BY-PHRASE 'FEATURES '(PREPG AGENT))
(putprop! 'FAKE-BY-PHRASE 'FIRSTWORD '(BY))
(putprop! 'FAKE-BY-PHRASE 'DAUGHTERS '(FAKE-AGENT FAKE-BY))

(putprop! 'FAKE-BY 'FEATURES '(PREP BY))
(putprop! 'FAKE-BY 'FIRSTWORD '(BY))
(putprop! 'FAKE-BY 'DAUGHTERS 'WORD)

(putprop! 'FINDEVENTS-OSS 'OSSNODE= 'FINDEVENTS-OSS)
(putprop! 'FINDEVENTS-OSS 'MARKERS= '(!EVENT !SYSTEMS))
(putprop! 'FINDEVENTS-OSS 'SYSTEMS= '(!SYSTEMS))
(putprop! 'FINDEVENTS-OSS 'DETERMINER= '(SG-PL INDEF nil))
(putprop! 'FINDEVENTS-OSS 'VARIABLE= 'FINDEVENTS)

(putprop! 'FRIEND-OSS 'OSSNODE= 'FRIEND-OSS)
(putprop! 'FRIEND-OSS 'MARKERS= '(!PERSON !ANIMATE !THING !SYSTEMS))
(putprop! 'FRIEND-OSS 'SYSTEMS= '(!ANIMATE !THING !SYSTEMS))
(putprop! 'FRIEND-OSS 'REFER= '(ßFRIEND))
(putprop! 'FRIEND-OSS 'DETERMINER= '(1 DEF nil))

(putprop! 'NAME-OSS 'OSSNODE= 'NAME-OSS)
(putprop! 'NAME-OSS 'MARKERS= '(!NAME !THING !SYSTEMS))
(putprop! 'NAME-OSS 'SYSTEMS= '(!THING !SYSTEMS))
(putprop! 'NAME-OSS 'DETERMINER= '(1 DEF nil))

(putprop! 'PLACE-OSS 'OSSNODE= 'PLACE-OSS)
(putprop! 'PLACE-OSS 'MARKERS= '(!PLACE !SYSTEMS))
(putprop! 'PLACE-OSS 'SYSTEMS= '(!SYSTEMS))
(putprop! 'PLACE-OSS 'DETERMINER= '(SG-PL INDEF WHICH))
(putprop! 'PLACE-OSS 'VARIABLE= 'PLACE)

(putprop! 'SHRDLU-OSS 'OSSNODE= 'SHRDLU-OSS)
(putprop! 'SHRDLU-OSS 'MARKERS= '(!ROBOT !ANIMATE !THING !SYSTEMS))
(putprop! 'SHRDLU-OSS 'SYSTEMS= '(!ANIMATE !THING !SYSTEMS))
(putprop! 'SHRDLU-OSS 'REFER= '(ßSHRDLU))
(putprop! 'SHRDLU-OSS 'DETERMINER= '(1 DEF nil))

(putprop! 'STACKPARTS-OSS 'OSSNODE= 'STACKPARTS-OSS)
(putprop! 'STACKPARTS-OSS 'MARKERS= '(!THING !PHYSOB !MANIP !SYSTEMS))
(putprop! 'STACKPARTS-OSS 'SYSTEMS= '(!THING !PHYSOB !SYSTEMS))
(putprop! 'STACKPARTS-OSS 'DETERMINER= '(3 INDEF nil))
(putprop! 'STACKPARTS-OSS 'VARIABLE= 'PART)

(putprop! 'UNKNOWN-OSS 'OSSNODE= 'UNKNOWN-OSS)
(putprop! 'UNKNOWN-OSS 'MARKERS= '(!THING !SYSTEMS !PHYSOB !VAGUE))
(putprop! 'UNKNOWN-OSS 'SYSTEMS= '(!THING !SYSTEMS))
(putprop! 'UNKNOWN-OSS 'DETERMINER= '(SG-PL INDEF WHICH))
(putprop! 'UNKNOWN-OSS 'VARIABLE= 'UNKNOWN)

(putprop! 'UNKNOWN-OSS-BY 'OSSNODE= 'UNKNOWN-OSS-BY)
(putprop! 'UNKNOWN-OSS-BY 'RELATIONS= '((!IS ($? UNKNOWN) ?)))
(putprop! 'UNKNOWN-OSS-BY 'MARKERS= '(!THING !SYSTEMS !PHYSOB !VAGUE))
(putprop! 'UNKNOWN-OSS-BY 'SYSTEMS= '(!THING !SYSTEMS))
(putprop! 'UNKNOWN-OSS-BY 'DETERMINER= '(SG-PL INDEF nil))
(putprop! 'UNKNOWN-OSS-BY 'PARSENODE= '(FAKE-AGENT))
(putprop! 'UNKNOWN-OSS-BY 'VARIABLE= 'UNKNOWN)

(putprop! 'UNKNOWNSG-OSS 'OSSNODE= 'UNKNOWNSG-OSS)
(putprop! 'UNKNOWNSG-OSS 'MARKERS= '(!THING !SYSTEMS !PHYSOB !VAGUE))
(putprop! 'UNKNOWNSG-OSS 'RELATIONS= '((!IS ($? UNKNOWN) ?)))
(putprop! 'UNKNOWNSG-OSS 'SYSTEMS= '(!THING !SYSTEMS))
(putprop! 'UNKNOWNSG-OSS 'DETERMINER= '(NS INDEF WHICH))
(putprop! 'UNKNOWNSG-OSS 'VARIABLE= 'UNKNOWN)

(putprop! 'WE-OSS 'OSSNODE= 'WE-OSS)
(putprop! 'WE-OSS 'MARKERS= '(!ANIMATE !THING !SYSTEMS))
(putprop! 'WE-OSS 'SYSTEMS= '(!ANIMATE !THING !SYSTEMS))
(putprop! 'WE-OSS 'REFER= '(ßSHRDLU ßFRIEND))
(putprop! 'WE-OSS 'AND= '(FRIEND-OSS SHRDLU-OSS))

;; ======>>> TEMPORARY PLACE FOR OSS-PROPERTY DEFS - MOVE WHEN APPROVED

(putprop! 'ANIMATE 'OSS 'ANIMATE-OSS)
(putprop! 'FINDEVENTS 'OSS 'FINDEVENTS-OSS)
(putprop! 'FRIEND 'OSS 'FRIEND-OSS)
(putprop! 'NAME 'OSS 'NAME-OSS)
(putprop! 'PLACE 'OSS 'PLACE-OSS)
(putprop! 'SHRDLU 'OSS 'SHRDLU-OSS)
(putprop! 'STACKPARTS 'OSS 'STACKPARTS-OSS)
(putprop! 'UNKNOWN 'OSS 'UNKNOWN-OSS)
(putprop! 'UNKNOWNSG 'OSS 'UNKNOWNSG-OSS)
(putprop! 'WE 'OSS 'WE-OSS)

(putprop! 'RED 'CONTRAST '!COLOR)
(putprop! 'BLUE 'CONTRAST '!COLOR)
(putprop! 'GREEN 'CONTRAST '!COLOR)
(putprop! 'WHITE 'CONTRAST '!COLOR)
(putprop! 'BLACK 'CONTRAST '!COLOR)
(putprop! 'BIG 'CONTRAST '!SIZE)
(putprop! 'LITTLE 'CONTRAST '!SIZE)
(putprop! 'LARGE 'CONTRAST '!SIZE)
(putprop! 'SMALL 'CONTRAST '!SIZE)
(putprop! 'WIDE 'CONTRAST '!WIDTH)
(putprop! 'NARROW 'CONTRAST '!WIDTH)
(putprop! 'TALL 'CONTRAST '!HEIGHT)
(putprop! 'SHORT 'CONTRAST '!HEIGHT)
(putprop! 'THICK 'CONTRAST '!THICKNESS)
(putprop! 'THIN 'CONTRAST '!THICKNESS)

#_(ns shrdlu.smspec)

;; ############################################################
;;
;;                           SMSPEC
;;                    (SEMANTIC SPECIALISTS)
;;
;; ############################################################

(defn- smtime [] (ert "SMTIME NOT WRITTEN YET"))

(§ defn- SMNEWNOUN []
    (OBJECT (MARKERSß (!NEWNOUN) PROCEDUREß ((!NEWWORD)))))

(§ defn- SMNEWPROPN [] (SMSET (list (newcopy 'NAME-OSS))))

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
                        RSSNODE= (and (rss? INTERP) (gensym 'RSS))
                        OSSNODE= (and (oss? INTERP) (gensym 'OSS))
                        MARKERS= (markers? INTERP)
                        SYSTEMS= (systems? INTERP)
                        REL= (rel? INTERP)
                        AND= (and (or (cq 'BUT *fe*) (cq 'AND *fe*)) INTERPLIST)
                        OR= (and (or (cq 'OR *fe*) (cq 'NOR *fe*)) INTERPLIST))
                    %SM))))
        ;; WHEN THERE IS NO RESTLIST, WE HAVE LOOPED TO THE END OF THE LIST OF CONJUNCTS, AND THE RESULTING INTERPRETATION IS OK.
        ;; THE MAPPING IS DOWN THE LIST OF INTERPRETATIONS FOR A SINGLE CONJUNCT WHILE THE RECURSION GETS US DOWN THE LIST OF CONJUNCTS.
        ;; THUS WE GET EVERY POSSIBLE COMBINATION OF THE INTERPRETATIONS. -- ISN'T LISP SUPER-DUPER-WONDERFUL!
        ;; NOTICE THAT INTERP IS GETTING PICKED UP AS A FREE VARIABLE BY SMCONJ2, EVEN THOUGH IT IS BOUND ONLY INSIDE A MAPCAR INSIDE SMCONJ2.
        ;; THIS WORKS BECAUSE THE CLAUSE CONTAINING IT CAN NEVER GET CALLED EXCEPT BY RECURSION,
        (dorun (map #'(lambda [INTERP] (SMCONJ2 (cons INTERP INTERPLIST) (cdr restlist))) (SM restlist)))))

(§ defn- SMVG [] ;; CALLED INSIDE ANY VG
    (let [TSS nil TENSE nil]
        (SETQ TSS (getr 'TIME (MOVE-PT 'C 'U '(CLAUSE))))
        (and (cq 'NEG *fe*) (add-f-pt 'NEG *pt*))                           ;; NEG IS TRANSFERRED FROM THE VG TO THE CLAUSE IN WHICH IT IS EMBEDDED.
        (SETQ TENSE (getr 'TENSE *c*))
        (COND ((memq TENSE '((PRESENT) (IMPER) (INFINITIVE))) true)
            ((= TENSE '(MODAL))
                (set! *global-message* "THAT DOESN'T MAKE ANY SENSE TO ME.")
                (add-f-pt 'MODAL *pt*))                               ;; CLAUSES ARE ALSO MARKED AS MODAL.
            ((and (= TENSE '(FUTURE)) (ISQ *pt* QUEST) (= (refer? (car (SM (getr 'SUBJECT *pt*)))) '(ßSHRDLU))) ;; FUTURE QUESTIONS WITH "YOU"
                (SETQ TENSE '(PRESENT))                             ;; SUBJECT IS REALLY IMPERATIVE.
                (remove-f-pt 'QUEST *pt*)
                (add-f-pt 'IMPER *pt*))                               ;; THE CLAUSE IS NO LONGER QUESTION, BUT RATHER, IMPERATIVE.
            ((SETDIF TENSE '(PAST PRESENT))
                (global-err "I DON'T KNOW HOW TO HANDLE TENSES INVOLVING FUTURE EVENTS OR MODALS OTHER THAN IN THE PRESENT")))
        (putprop! TSS 'TENSE= TENSE)
        true))

(§ defn- SMPRON [node]
    (eval (SM node))
    (COND ((nil? SM)
        (set! *global-message* (str "I DON'T KNOW WHAT \"" (from (NB H) (N H)) "\" REFERS TO"))))
    SM)

(§ defn- SMVAUX []
    (if (ISQ H NEG) (FQ NEG) true)
    (putprop! (getr 'TIME *c*) 'TENSE= (or (MEET (features H) '(PRESENT PAST MODAL)) (bug "SMVAUX -- FUNNY TENSE"))))

(defn- smplace [] (ert "SMPLACE NOT WRITTEN YET"))

(defn- smtoadj [] (ert "SMTOADJ NOT WRITTEN YET"))

(§ defn- SMPROP []
    ;; THIS IS THE SEMANTICS FOR PROPER NOUNS.  IT PRODUCES TWO
    ;; INTERPRETATIONS.  ONE IS THE OPAQUE REFERENCE TO THE NAME
    ;; ITSELF, AS IN "CALL IT SAM".  THE OTHER IS THE TRANSPARENT
    ;; REFERENT AS IN "PICK UP SAM".
    (SMSET (list
        (BUILD
            OSSNODE= (gensym 'OSS)
            VARIABLE= 'NAME
            DETERMINER= (1 DEF nil)
            PARSENODE= *c*
            MARKERS= (!NAME)
            REFER= (list (word (NB H))))
        (BUILD
            OSSNODE= (gensym 'OSS)
            DETERMINER= (1 DEF nil)
            PARSENODE= *c*
            VARIABLE= (gensym 'X)
            RELATIONS= (list (list '!NAME OSSNODE= (word (NB H)))))))
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

(§ defn- SMADJG-PREPG []
    ;; HANDLES ADJECTIVE GROUPS AND PREPGS BOTH AS COMPLEMENTS AND QUALIFIERS.
    ;; DO NOTHING FOR "BY" PHRASES IN PASSIVE CLAUSES OR "OF" PHRASES LIKE IN "THREE OF THE BLOCKS".
    ;; SEMANTIC SUBJECT IS THE SUBJECT OF AN INTENSIVE OR THE NG TO WHICH THE GROUP IS A QUALIFIER,
    ;; OR THE CLAUSE OF WHICH IT IS AN ADJUNCT.
    (let [X nil SMSUB nil]
        (and (or (cq 'AGENT *fe*) (cq 'OF *fe*)) (RETURN true))
        (setr 'LOGICAL-SUBJECT
            (COND ((cq 'COMP *fe*) (getr 'SUBJECT (MOVE-PT 'C 'U '(CLAUSE))))
                ((cq 'LOBJ *fe*) (or (getr 'OBJ1 (MOVE-PT 'C 'U '(CLAUSE))) (getr 'SUBJECT *pt*)))
                ((ISQ (MOVE-PT 'C 'U '(not (ISQ *pt* COMPONENT)) 'U) NG) *pt*)
                ((ISQ *pt* CLAUSE) *pt*)
                ((bug "SMADJG-PREPG FUNNY POSITION")))
            *c*)
        (SETQ SMSUB (SM (getr 'LOGICAL-SUBJECT *c*)))
        (and (cq 'ADJG *fe*)
            (getr 'OBJ1 *c*)
            (setr 'ADJGHEAD (COMPARE-BUILD (getr 'HEAD *c*) (COND ((cq 'AS *fe*) '!ASMUCH) ((cq 'THAN *fe*) '!MORE) ((bug "SMADJG-PREPG FUNNY TYPE")))) *c*))
        (COND
            ((getr 'OBJ1 *c*) (SMCL1) SM)
            (:else (SMSET
                (let [SM nil]
                    (SMSET (doall (map #'(lambda [OSS]
                        (BUILD
                            OSSNODE= (gensym 'OSS)
                            MARKERS= (markers? OSS)
                            SYSTEMS= (systems? OSS)
                            VARIABLE= (variable? OSS)
                            REFER= (refer? OSS)
                            REL= OSS
                            REFER= (refer? OSS)
                            DETERMINER= '(NS-PL INDEF nil)))
                        SMSUB)))
                    (eval (COND
                        ((or (cq 'COMPAR *fe*) (cq 'SUP *fe*)) (findmeasure (getr 'HEAD *c*)))
                        (:else (SM (getr 'HEAD *c*)))))
                    SM))))))

(§ defn- SMIT [PRONOUN]
    ;; PRONOUN IS (IT THEY ONE) A NODE LIST OF POSSIBLE REFERENTS.
    ;; IS THIS A "DO IT!" COMMAND?  IF SO, RETURN THE LAST EVENT MENTIONED.
    ;; IF THIS PRONOUN HAS BEEN USED BEFORE IN THIS SENTENCE, THEN USE THE SAME CANDIDATES.
    ;; IF THIS PRONOUN WAS USED IN THE PREVIOUS SENTENCE,
    ;; LOOK FOR A STRUCTURE LIKE "A BLOCK WHICH IS TALLER THAN ANYTHING WHICH SUPPORTS IT"
    ;; OR "A BLOCK TALLER THAN ANYTHING WHICH SUPPORTS IT".
    (let [CANDIDATES nil AMBIGUITIES nil]
        (or discourse? (ert "SMIT: DISCOURSE SWITCH NOT ON"))
        (and MVB
            (ISQ MVB DO)
            (cq 'OBJ1 *fe*)
            (RETURN (SMSET LASTEVENT)))
        (COND ((getprop PRONOUN 'BIND)
                (MAP #'(lambda [BINDNODE] (SMIT2 BINDNODE 0)) (getprop PRONOUN 'BIND))
                (RETURN SM))
            ((SMIT2 (getprop PRONOUN 'LASTBIND) 0)
                (GO DONE))
            ((or (MOVE-PT 'C 'U 'U '(NG) 'U 'U '(NG)) (MOVE-PT 'C 'U 'U '(NG) 'U '(COMP) 'PV '(SUBJ)))
                (SMIT2 *pt* 0)
                (MOVE-PT 'C 'U 'U '(NG))
                (COND ((ISQ *pt* DEF)
                    (add-f-pt 'INDEF *pt*)
                    (remove-f-pt 'DEF *pt*)
                    (dorun (map #'(lambda [INTERP] (putprop! INTERP 'DETERMINER= '((EXACTLY 1) INDEF nil))) (SM *pt*)))))
                (RETURN SM))
            ((or (MOVE-PT 'C 'U '(BOUND) 'U) (MOVE-PT 'C 'U '(and (ISQ *pt* CLAUSE) (ISQ *pt* COMPONENT)) 'U 'DLC))
                (SMIT2 (getr 'OBJ2 *pt*) 0)
                (SMIT2 (getr 'OBJ1 *pt*) 0)
                (SMIT2 (getr 'SUBJECT *pt*) 0)
                (and (nil? SM)
                    (ISQ *pt* RSQ)
                    (SMIT2 (getr 'RELHEAD *pt*) 0))
                (and SM (RETURN SM))))
        (SMIT2 (getr 'SUBJECT *lastsent*) 192)
        (SMIT2 (parsenode? *lastrel*) 128)                ;; TRY REL (I.E. QUESTION FOCUS) OF THE LAST SENTENCE
        (MOVE-PT 'LASTSENT 'DLC)
    UP  (COND ((not (MOVE-PT 'PV '(NG))) (GO ON))
            (:else (SMIT2 *pt* 64)))                       ;; GO THROUGH TOP LEVEL NG'S OF LAST SENTENCE
        (and (MOVE-PT 'PV) (GO UP))
    ON  (or SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE ANSREF (NG'S IN LAST ANSWER)
            (MAP #(SMIT2 % 0) *ansname*))
        (or SM ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE BACKREF2 (NG'S IN LAST SENTENCE) LIST
            (MAP #(SMIT2 % 0) *backref2*))
    DONE (putprop! PRONOUN 'BIND CANDIDATES)
        (or (cdr SM) (remprop! (car SM) 'AMBIGUITIES=))
        SM))

(§ defn- SMIT2 [NODE PLAUSIBILITY]
    ;; MAKE SURE NODE IS REALLY THERE.
    ;; QUEST NODES (SUCH AS "WHAT") OR OTHER NODES WITHOUT HEAD NOUNS ARE NOT SUITABLE FOR REFERENTS.
    ;; MAKE SURE THAT NODE HASN'T ALREADY BEEN USED AS REFERENT.
    ;; MAKE SURE NODE AND PRONOUN AGREE IN NUMBER.
    (and NODE
        (getr 'HEAD NODE)
        (not (memq (car NODE) CANDIDATES))
        (COND ((= PRONOUN 'IT)
            (and (ISQ NODE NS) (not (ISQ NODE PRONG))))
            (:else (ISQ NODE NPL)))
        (SETQ CANDIDATES (cons (car NODE) CANDIDATES))
        (SMSET (concat
            (doall (map #'(lambda [REFERENT-OSS]
                (BUILD
                    OSSNODE= (gensym 'OSS)
                    MARKERS= (markers? REFERENT-OSS)
                    SYSTEMS= (systems? REFERENT-OSS)
                    PLAUSIBILITY= PLAUSIBILITY
                    AMBIGUITIES= (list (list OSSNODE= (from (NB NODE) (N NODE)) *c*))
                    REFER= (refer? REFERENT-OSS)
                    VARIABLE= (variable? REFERENT-OSS)
                    PARSENODE= *c* ;; INPUT PARAMETER
                    ;; USE THE REFERENT'S REFERENT, IF IT HAS ONE.
                    DETERMINER= (list (COND ((ISQ *c* NPL) 'NPL) ('NS)) 'INDEF nil)
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
                OSSNODE= (gensym 'OSS)
                VARIABLE= (variable? OFOSS)
                SYSTEMS= (systems? OFOSS)
                MARKERS= (markers? OFOSS)
                PARSENODE= *c*
                DETERMINER= (list (COND ((cq 'NUM *fe*) (SM (MOVE-PT 'H 'PV '(NUM)))) ((ISQ NB BOTH) 2) ('NPL))
                                (COND ((MOVE-PT 'H 'PV '(QNTFR)) (eval (SM *pt*))) ('INDEF))
                                (COND ((cq 'HOWMANY *fe*) 'HOWMANY) ((cq 'QDET *fe*) 'WHICH)))
                RELATIONS= (list (list 'THAMONG (list 'THV (variable? OFOSS)) (list 'quote (refer? OFOSS))))))
        (SM (MOVE-PT 'H 'DLC)))))

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
                ((cq 'NUMD *fe*) ((lambda [NUM] (eval (SM (MOVE-PT 'H 'PV '(NUMD))))) (SM (MOVE-PT 'H 'PV '(NUM)))))
                ((cq 'NUM *fe*) (SM (MOVE-PT 'H 'PV '(NUM))))
                ((cq 'NPL *fe*) (COND ((ISQ NB BOTH) 2) ((cq 'NS *fe*) 'SG-PL) ('NPL)))
                ('NS))
            (COND
                ((cq 'QNTFR *fe*) (eval (SM (MOVE-PT 'H 'PV '(QNTFR)))))
                ((cq 'TPRON *fe*) (eval (SM (MOVE-PT 'H 'PV '(TPRON)))))
                ((cq 'DEF *fe*) 'DEF)
                ((cq 'DET *fe*) 'INDEF)
                ('NDET))
            (COND
                ((cq 'HOWMANY *fe*) 'HOWMANY)
                ((cq 'QDET *fe*) 'WHICH))))
        ;; BUILD AN INITIAL OSS.  SETUP TO LOOP THROUGH ADJECTIVES.
        ;; IF IT'S A TPRON, IT WAS EVALED ABOVE, SO SKIP INCOMPLETES SUCH AS "PICK UP TWO".
        ;; EVAL THE HEAD NOUN IF AN ADJECTIVE ELIMINATES ANY POSSIBLE INTERPRETATION FOR THIS NG,
        ;; FAIL IF WE'VE LOOPED THRU ALL THE MODIFIERS, THEN RETURN THE LIST OF POSSIBLE INTERPRETATIONS.
        ;; IF IT'S A COMPARATIVE OR SUPERLATIVE ADJECTIVE,
        ;; IF IT'S AN ADJECTIVE OR CLASSIFIER, THEN EVAL THE DICTIONARY DEFINITION OF IT.
        (SMSET (list (BUILD
            OSSNODE= (gensym 'OSS)
            PARSENODE= *c*
            VARIABLE= (gensym 'X)
            MARKERS= (and (cq 'TPRON *fe*) '(!VAGUE !PHYSOB !THING))
            RELATIONS= (and (cq 'TPRON *fe*) (list (list '!PHYSOB OSSNODE=)))
            DETERMINER= DETERS)))
        (SETQ WORD-BEING-INTERPRETED H)
        (COND ((ISQ H TPRON) (GO LOOP))
            ((cq 'INCOM *fe*) (SMONE) (GO LOOP)))
        (SMSET (eval (SM WORD-BEING-INTERPRETED)))
    LOOP (COND ((nil? SM) (RETURN nil)))
        (COND ((nil? (SETQ WORD-BEING-INTERPRETED (cdr WORD-BEING-INTERPRETED)))
                (RETURN SM))
            ((or (ISQ WORD-BEING-INTERPRETED COMPAR) (ISQ WORD-BEING-INTERPRETED SUP))
                (eval (findmeasure WORD-BEING-INTERPRETED))
                (GO LOOP))
            ((or (ISQ WORD-BEING-INTERPRETED ADJ) (ISQ WORD-BEING-INTERPRETED CLASF))
                (SMADJ WORD-BEING-INTERPRETED)
                (GO LOOP))
            ((ISQ WORD-BEING-INTERPRETED POSS)
                (SMPOSS)
                (GO LOOP)))
        (GO LOOP)))

(§ defn- SMNG2 []
    ;; CALLED FROM NG WHEN ALL QUALIFIERS HAVE BEEN FOUND.
    ;; BASICALLY, IT SAVES THE NG ON THE BACKREF(ERENCE) LIST, AND CALLS SMNG3
    ;; (ON EACH POSSIBLE NG INTERPRETATION) TO EVAL ANY DEFINITE NOUN GROUPS, EG. "THE RED BLOCK".
    ;; AS USUAL, THE INITIAL OSS LIST IS IN "SM" AND THE FINAL OSS LIST IS PUT IN "SM".

    ;; DON'T USE FAKEY ANSWER NAME NODES FOR REFERENCE.
    ;; QUEST NODES ARE NOT SUITABLE REFERENTS.
    ;; SAVE THIS NG AWAY FOR POSSIBLE LATER BACK REFERENCE.
    ;; GO THRU ALL THE POSSIBLE INTERPRETATIONS OF THIS NOUN GROUP.
    (and (not (cq 'ANSNAME *fe*))
        (getr 'HEAD *c*)
        discourse?
        (set! *backref* (cons (car *c*) *backref*)))
    (SMSET (MAPBLAND #'SMNG3 SM)))

(§ defn- SMNG3 [oss]
    ;; TAKES AN OSS AS ARGUMENT AND TRIES TO FIND ITS REFERENCE IF THE NOUN GROUP IS DEFINITE.
    ;; EXPECT FOR SPECIAL "ONLY DEFINITE" DEFINITES SUCH AS "THE RIGHT" AND "THE THING".
    (let [FINDER nil MUNG nil INTER nil LIST nil CANDIDATES nil UNBOUND nil]
        (COND
            ((not (= (quantifier? oss) 'DEF)) (RETURN oss))        ;; IF ITS NOT DEFINITE OR IT
            ((refer? oss) (RETURN oss))                             ;; ALREADY HAS A REFERENT
            ((cq 'ANSNAME *fe*) (RETURN oss)))                            ;; MARKED,  IF ITS KLUDGY
        (SETQ FINDER
            (PLNR-FINDIFY 'ALL                                      ;; ANSWER NAME, JUST RETURN IT
                (variable? oss)                                     ;; JUST RETURN IT
                (list (variable? oss))
                (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))) ;; BUILDS UP THFIND EXPRESSION
        (putprop! oss 'PLNRCODE= FINDER)
        (SETQ WHO nil)
    UP  (COND
            ((not (SETQ CANDIDATES (THVAL2 WHO FINDER))) (GO TOOFEW))
            ((number? (num? oss)) (COND ((< (count CANDIDATES) (num? oss)) (GO TOOFEW)) ((> (count CANDIDATES) (num? oss)) (GO TOOMANY))))
            ((= (num? oss) 'NS) (COND ((nil? CANDIDATES) (GO TOOFEW)) ((cdr CANDIDATES) (GO TOOMANY))))
            ((memq (num? oss) '(NPL SG-PL)))
            ((ert "SMNG3: SCREWY NUMBER PROPERTY OF OSS")))
        (putprop! oss 'REFER= CANDIDATES)
    DONE (RETURN oss)

    TOOFEW ;; WE DIDN'T FIND ANY (OR ENOUGH) REFERENTS FOR THE NG
        (COND
            ((or (not discourse?) (nil? WHO))
                (set! *global-message* (str "I DON'T KNOW WHAT YOU MEAN BY \"" (from NB N) "\"."))
                (RETURN nil))
            ;; IF WE AREN'T REMEMBERING SENTENCES, FORGET IT IF WE JUST TRIED TO
            ;; FIND EVERYTHING (OR EVERYTHING THAT "HE" KNOWS ABOUT), THEN FAIL
            ((memq WHO '(HE nil))
                (set! *global-message* (str "I DON'T KNOW WHICH " (cdr (from NB N)) " YOU MEAN."))
                (RETURN nil)))
        (SETQ MUNG true)

    TOOMANY ;; ELSE SET UP TO EXPAND THE SENTENCES WE'RE LOOKING AT
        (and (memq WHO '(HE nil))
            (SETQ FINDER (plnr-mung FINDER CANDIDATES)))
        ;; RESTRICT THE POSSIBLE REFERENTS TO BE AMONG THE LIST ALREADY FOUND
        (SETQ WHO (COND
            ((= WHO nil) 'HE)
            ((= WHO 'HE) (list (dec *lastsentno*) (inc *lastsentno*)))
            ((or (not MUNG) (== (car WHO) 1)) (SETQ WHO 'HE) (GO TOOFEW))
            ((cons (dec (car WHO)) (cdr WHO)))))
        (SETQ MUNG nil)
        (GO UP)))

(§ defn- SMONE []
    (let [CONTRAST nil x H] ;; SET X TO DAUGHTERS OF CURRENT NODE
    GO  (COND ((SETQ CONTRAST (getprop (root (NB x)) 'CONTRAST))
                (SETQ CONTRAST (list CONTRAST (root (NB x)))))
            ((SETQ x (cdr x)) (GO GO)))
    UP  (or (and (MOVE-PT 'C 'U 'U '(NG)) (SMONE2 (list (car *pt*))))
            (SMONE2 (parsenode? *lastrel*))
            (SMONE2 *backref*)
            (SMONE2 *ansname*)
            (SMONE2 *backref2*)
            (COND (CONTRAST (SETQ CONTRAST nil) (GO UP)))
            (and (MOVE-PT 'LASTSENT 'DLC 'PV '(NG)) (SMONE2 (list (car *pt*))))
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
    LOOK (COND ((and (= (car CONTRAST) (getprop (root (NB x)) 'CONTRAST)) (not (= (cadr CONTRAST) (root (NB x)))))
                (RETURN (reverse (cdr x))))
            ((SETQ x (cdr x)) (GO LOOK))
            (:else (RETURN nil)))))                                  ;; FAIL, IF NO WORD SUPPLIES CONTRAST

(§ defn- SMPOSS []
    (let [x (SMPOSS2 *c* (MOVE-PT 'H 'PV '(POSS)))]
        (and x (SMRELATE x))))

(§ defn- SMPOSS2 [headnode modnode]
    (let [SM nil SMSUB (SM modnode) SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST (RETQ AMOB1 (SM headnode))]
        (SMSET '(!HAVE))
        (and SM (let [x (gensym 'NODE)] (and x (putprop! x 'SEMANTICS SM) (list x))))))

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
                    (bug "SMRELATE - TO WHOM?"))
                (RETURN (BUILD
                    OSSNODE= (and (oss? REL) (gensym 'OSS))
                    RSSNODE= (and (rss? REL) (gensym 'RSS))
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

(§ defn- SMCL1 []
    (let [SMSUB nil SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST nil]
        ;; SET UP GLOBAL VARIABLES WHICH CONSIST OF POINTERS TO THE SEMANTIC DEFINITIONS
        ;; OF THE VARIOUS NOUN-GROUPS (ALSO RSNG'S) REQUIRED BY THE TRANSITIVITY OF THE VERB.
        (SETQ SMSUB (COND
            ((SETQ SMSUB (getr 'LOGICAL-SUBJECT *c*)) (SM SMSUB))
            ((cq 'IMPER *fe*) '(SHRDLU-OSS))
            ((not (cq 'PASV *fe*)) (SM (or (getr 'SUBJECT *c*) (bug "SMCL1 -- NO SUBJECT"))))
            ((cq 'AGENT *fe*) (bug "SMCL1 -- AGENT MISSING"))
            ('(UNKNOWN-OSS-BY))))
        (SETQ SMOB1 (SM (COND ((cq 'PASV *fe*) (getr 'SUBJECT *c*)) ((getr 'OBJ1 *c*)))))
        (SETQ SMOB2 (SM (getr 'OBJ2 *c*)))
        (SETQ SMOBL (SM (getr 'LOBJ *c*)))
        (SETQ SMCOMP (SM (getr 'COMP *c*)))
        ;; NATURALLY SEVERAL OF THESE GLOBAL VARIABLES (BOUND IN THIS PROG AND ACCESSED IN DEEPER ONES)
        ;; ARE NIL AT THIS POINT IN THE PROCEDURE.  THE FOLLOWING CHECKS ARE PRIMARILY FOR DEBUGGING PURPOSES
        ;; (HENCE THE "ERT") TO INSURE THAT THE NON-NIL REGISTERS AND THE TRANSITIVITY OF THE VERB ARE
        ;; BEING MATCHED IN EVERY CASE.
        (or SMSUB (and (MEET '(THERE ITRNS) *fe*) (GO CHECK)))
        (or SMOB1 (and (or (cq 'TRANS *fe*) (not (cq 'CLAUSE *fe*))) (GO CHECK)))
        (or (and SMOB1 SMOB2) (and (cq 'TRANS2 *fe*) (GO CHECK)))
        (or (and SMOB1 SMOBL) (and (cq 'TRANSL *fe*) (GO CHECK)))
        (or SMCOMP (and (cq 'INT *fe*) (GO CHECK)))
        (GO REL)
    CHECK (ert "BUG: SMCL1 TRANSITIVITY")
    REL (SETQ RELLIST
            (SM (COND
                ((cq 'RSQ *fe*) (getr 'RELHEAD *c*))
                ((or (cq 'PREPG *fe*) (cq 'ADJG *fe*)) (getr 'LOGICAL-SUBJECT *c*))
                ((cq 'QUEST *fe*) (getr 'RELHEAD *c*)))))
        (and (not RELLIST)
            (or (cq 'POLAR *fe*) (cq 'DECLAR *fe*))
            (SETQ X (RELFIND *c*))
            ;; FIND RELATIVE ELEMENT FOR POLAR CLAUSES WHICH CONTAIN INDEFINITE.
            ;; APPLIES TO TOPLEVEL CLAUSES SINCE ONLY THEY CAN HAVE FEATURES POLAR OR DECLAR.
            (or (= X SMSUB)
                (= X SMOB1)
                (= X SMOB2)
                (= X SMOBL)
                (= X SMCOMP)
                (bug "SMCL1 -- POLAR REL DOESN'T MATCH"))
            (SETQ RELLIST X))
        (SETQ TIME (getr 'TIME (MOVE-PT 'C 'U '(CLAUSE))))
        ;; THIS REFERS TO THE SEMANTIC SENSE OF THE VERB WHICH WILL PROBABLY VARY WITH ITS TRANSITIVITY.
        ;; THE VALUE THAT IS FINALLY DETERMINED REPRESENTS ALL POSSIBLE SENSES OF THE MEANING OF THE WORD
        ;; THAT ARE APPROPRIATE TO THE TRANSITIVITY FIGURED OUT BY THE SYNTACTIC PROGRAMS
        (SETQ SENSE-OF-VERB
            (COND
                ((cq 'PREPG *fe*) (SM (SETQ WORD-BEING-INTERPRETED (getr 'HEAD *c*))))
                ((cq 'ADJG *fe*) (SM (SETQ WORD-BEING-INTERPRETED (getr 'ADJGHEAD *c*))))
                ((cadr (assq (car (MEET *fe* '(ITRNS TRANS INT TRANSL TRANS2 THERE ITRNSL))) (SM (SETQ WORD-BEING-INTERPRETED (getr 'MVB *c*))))))))
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
    (COND ((nil? (getprop WORD-BEING-INTERPRETED 'FEATURES)))
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
    ((MEET *fe* '(THERE LOBJ COMP PRT SUBJ OBJ1 OBJ2)))
    ((ISQ WORD-BEING-INTERPRETED NG)
        (and (COND ((ISQ WORD-BEING-INTERPRETED TIM))
            ((and (cq 'REL-NOT-FOUND *fe*)
                (ISQ WORD-BEING-INTERPRETED QUEST)
                (ISQ (H WORD-BEING-INTERPRETED) TIM1))
            (RQ REL-NOT-FOUND)
            (FQ TIMEQ)))
        (smtime)))
    ;; IN WHICH CASE IT WAS ALREADY PROCESSED MIGHT GO AWAY IN A FEW DAYS
    ;; BUG CHATCHER MIGHT WANT TO CHANGE THAT THE REST ARE HOOKS FOR WHEN
    ;; WE FIGURE OUT WHAT TO DO WITH THEM
    ((ISQ WORD-BEING-INTERPRETED PREPG)
        (or (ISQ WORD-BEING-INTERPRETED AGENT)
            (ert "SMCL-MOD BAD PREPG")))
    ((ISQ WORD-BEING-INTERPRETED QADJ)
        (or (MEET *fe* '(LOBJ COMPQ))
            (eval (SM WORD-BEING-INTERPRETED))))
    ((ISQ WORD-BEING-INTERPRETED BOUND))
    ((ISQ WORD-BEING-INTERPRETED BINDER))
    ((ISQ WORD-BEING-INTERPRETED QUEST))
    ((ISQ WORD-BEING-INTERPRETED CLAUSE))
    ((ert "SMCL-MODIFIERS ATTEMPTED TO PROCESS AN UNEXPECTED TYPE OF CONSTITUENT"))))

(§ defn- SMBIND []
    (let [TSS nil EVENT nil *start* nil *end* nil]
        ;; DOES THE SM HAVE MORE THAN ONE VALUE???
        (when (cdr (SM H))
            (ert "I DON'T KNOW WHAT TO DO WITH AMBIGUOUS BOUND CLAUSES"))
        ;; DISPATCH TABLE TO MATCH THE APPROPRIATE ACTION WITH EACH BINDER.
        ;; MOVE TO THE FIRST WORD OF THE CLAUSE (TO THE BINDER) AND CHECK FOR THE FEATURE TIME
        ;; (MAYBE ALSO CHECK FOR THE SM BEING MARKED AS AN EVENT???)
        (when (ISQ (MOVE-PT 'H 'DF) TIME)
            (SETQ TSS (getr 'TIME *c*))
            (SETQ EVENT (FINDEVENTS (car (SM H))))
            (when-not EVENT
                (global-err "NO SUCH THING EVER HAPPENED"))
            (SETQ EVENT (car EVENT))
            (set! *start* (getprop EVENT 'START))
            (set! *end* (getprop EVENT 'END))
            (eval (SM *pt*))
            true)))

(defn- smbinder [start-ev end-ev]
    ;; CALLED FOR A ABINDER - THE FIRST ARGUMENT GIVES THE BEGINNING, SECOND THE END.
    ;; A TYPICAL USE IS THE DEFINITION OF "AFTER", WHICH IS (SMBINDER END NIL) I.E.
    ;; THE EVENT STARTS AFTER THE END OF THE BOUND EVENT, WITH NO SPECIFICATION ON WHEN IT ENDS.
    (putprop! TSS 'START= start-ev)
    (putprop! TSS 'END= end-ev))

#_(ns shrdlu.smutil)

;; ############################################################
;;
;;                          SMUTIL
;;
;; ############################################################

(defn- istense [node arg]
    (let [x (getr 'TIME node)]
        (when-not x (ert "ISTENSE -- NO TIME REGISTER"))
        (let [x (tense? x)]
            (when-not x (ert "ISTENSE -- NO TENSE"))
            (condp = arg
                'PRESENT (memq x '((PRESENT) (PRESENT PRESENT)))
                'FUTURE (= x '(FUTURE))
                'MODAL (= x '(MODAL))
                'IMPERF (and (cdr x) (= (car x) 'PRESENT))
                (ert "ISTENSE -- FUNNY ARG")))))

(defn- imperf? [x] (let [x (tense? x)] (and (cdr x) (= (car x) 'PRESENT))))

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
        (dorun (map #'(lambda [%PROP] (and (SETQ %X (eval %PROP)) (putprop! NODE= %PROP %X))) %PROPS))
        NODE=))

(defn- newcopy [oss]
    (let [new (gensym 'OSS)]
        ;; WATCH OUT -- THIS IS IMPLEMENTATION DEPENDENT,
        ;; AND GETS THE ENTIRE PROPERTY LIST IN OUR LISP.
        (loop [old (PLIST oss)]
            (if (some? old)
                (do (putprop! new (car old) (cadr old)) (recur (cddr old)))
                (do (putprop! new 'PARSENODE= *c*) new)))))

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
                            (let [OSS (eval (car %MARKL)) x (checkrel OSS) CHECK nil]
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
                            RSSNODE= (SETQ RSSNAME (gensym 'RSS))
                            MARKERS= MARKERSß
                            VARIABLE= ((lambda [x] (putprop! x 'RSSVAR x)) (gensym 'EVX))
                            PARSENODE= *c*
                            RELATIONS= (reverse (doall (map #'(lambda [%PLNRPHRASE] (PLNR-NUMSUB '<<<RELATION-ERROR>>> %PLNRPHRASE)) (evalcheck PROCEDUREß))))
                            REL= RELß
                            NEGATIVE= (and (cq 'NEG *fe*) true)
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
    (let [ansrss (getprop answer 'ANSRSS=)]
        (set! *backref2* *backref*)
        (set! *backref* nil)
        (set! *lastrel* (rel? ansrss))
        (dorun (map #(do (putprop! % 'LASTBIND (getprop % 'BIND)) (remprop! % 'BIND)) '(IT THEY ONE)))
        (or
            (cq 'MODAL *fe*)
            (cq 'DECLAR *fe*)
            (MAP #'(lambda [backnode]
                    (let [a (SM backnode)]
                        (when (cdr a) ;; TRUE IF NODE HAD MULTIPLE INTERPRETATIONS
                            (terpri)
                            (pr a)
                            (setr 'SEMANTICS (ert "DOBACKREF: RETURN AN OSS FOR BACKNODE") backnode))
                        (or (refer? (car a)) ;; IF NODE HAS REFERENT, FINE
                            (putprop! (car a) 'REFER=
                                (or (getprop (variable? (car a)) 'BIND) (ert "DOBACKREF: RETURN REFERENT FOR BACKNODE"))))))
                *backref2*))
        ;; A FEW MISSING PIECES GO HERE
        nil))

(defn- evalcheck [l]
    ;; EVALCHECK CHECKS FOR THE PRESENCE OF (!EVAL (MUMBLE ...) ...) IN THE INPUT S-EXPRESSION L.
    ;; IF IT FINDS ONE THEN THE EXPRESSION MUMBLE IS EVALUATED AND REPACES (!EVAL ...), OTHERWISE
    ;; L IS RETURNED JUST THE WAY IT WAS.  HENCE THIS FUNCTION IS THE INVERSE OF QUOTE.
    (cond (term? l) l (= (car l) '!EVAL) (eval (cadr l)) :else l))

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
                (list (SETQ %X (gensym) %XL (concat %XL (cons %X nil)) %X %X))
                (ITERATEX f (cdr l))))
            (or (car l) '(nil))))))

(§ defn- MAPBLAND [fun l]
    ;; THIS IS THE INSTAMATIC CAMERA FUNCTION.  NO MATTER WHAT YOU PUT INTO THE FUNCTION,
    ;; IT AUTOMATICALLY ADJUSTS INTERNALLY TO THE AVAILABLE LIGHT SO THAT WHAT COMES OUT
    ;; THE END ALWAYS LOOKS THE SAME -- ONE BIG NIL-LESS LIST OF ALL THE APPLICATIONS OF
    ;; THE FUNCTION FUN.  THE PURPOSE IN THIS IS SO THAT THE FUNCTION CAN BE EASILY NESTED.
    ;;
    ;; INPUTS:
    ;;     FUN - FUNCTION OF ONE ARGUMENT TO BE APPLIED TO EACH ELEMENT IN L
    ;;     L   - LIST
    ;; VALUE:
    ;;     IF (FUN L) IS AN ATOM, THEN A LIST OF ATOMS.
    ;;     IF (FUN L) IS A LIST, THEN ALL THE LISTS APPENDED, THAT IS A LIST OF ATOMS.
    ;;     IF (FUN L) IS NIL, THEN ALL TRACE DISAPPEARS (SAVE FOR SIDE-EFFECTS).
    (loop [ANS nil l (if (nil? l) '(nil) l)]
        (let [f (fun (car l)) ANS (cond (nil? f) ANS (term? f) (concat ANS (cons f nil)) :else (concat ANS f)) l (cdr l)]
            (if l (recur ANS l) ANS))))

(defn- mapc2 [fun l]
    ;; MAPPING FUNCTION FOR GOING 2 AT A TIME THROUGH A LIST.
    ;; INPUTS:
    ;;     FUN - FUNCTION OF TWO ARGUMENTS
    ;;     L   - LIST (ESPECIALLY ATTRIBUTE VALUE TYPE LIST)
    ;; VALUE:
    ;;  LIST (LIKE MAPCAR) OF FUN APPLIED TO TOP TWO ELEMENTS.
    ;;
    ;; FUN APPLIED TO TOP TWO ELEMENTS.  LIST IS STEPPED TWO AT A TIME.
    (or (nil? l) (do (fun (car l) (cadr l)) (recur fun (cddr l)))))

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
        ((= WHO 'HE) (getprop x 'WHO))
        ((SETQ x (getprop x 'WHO)) (not (or (< x (car WHO)) (> x (cadr WHO)))))))

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
        (or SM (SETQ %VARNAM (gensym 'X)))
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
                            OSSNODE= (SETQ %OSSNODE (gensym 'OSS))
                            PARSENODE= (parsenode? %OSS)
                            MARKERS= (car %CHKRESULT)
                            SYSTEMS= (cadr %CHKRESULT)
                            DETERMINER= (determiner? %OSS)
                            VARIABLE= (variable? %OSS)
                            RELATIONS= (concat (reverse (doall (map #'(lambda [%PLNRPHRASE] (PLNR-NUMSUB %OSS %PLNRPHRASE)) (evalcheck PROCEDUREß)))) (relations? %OSS))
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

(defn- valueput [] (putprop! *thvalue* 'WHO *sentno*))

(defn- plnr-junkify [code]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (cond
        (term? code) code
        (= (car code) 'THGOAL) (list 'THAND code '(valueput))
        (= (car code) 'THFIND) (list 'THAND code (list 'thputprop (quotify (cadr (caddr code))) ''BIND '*thvalue*))
        (or (= (car code) 'THAND) (= (car code) 'THPROG)) (doall (mapcat plnr-junkify2 code))
        :else (doall (map plnr-junkify code))))

(defn- plnr-junkify2 [code]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (cond
        (term? code) (list code)
        (= (car code) 'THGOAL) (list code '(valueput))
        (= (car code) 'THFIND) (list code (list 'thputprop (quotify (cadr (caddr code))) ''BIND '*thvalue*))
        (or (= (car code) 'THAND) (= (car code) 'THPROG)) (list (doall (mapcat plnr-junkify2 code)))
        :else (list (doall (map plnr-junkify code)))))

(§ defn- PLNR-THCONSIFY [varlist exp body]
    ;; GENERATES A CONSEQUENT THEOREM.
    (let [TH (gensym 'THEOREM)]
        (putprop! TH 'THEOREM
            (if (= (car body) 'THPROG)
                (concat (list 'THCONSE (UNION varlist (cadr body)) exp) (cddr body))
                (list 'THCONSE varlist exp body)))
        TH))

(§ defn- PLNR-FINDIFY [mode variable varlist body]
    ;; GENERATES A THFIND STATEMENT FOR THE NOUN GROUP DESCRIBED IN THE OSS.
    ;; IT (CURRENTLY) ASSUMES THAT THE PLNRCODE PROPERTY OF THE OSS IS A LIST
    ;; OF PATERNS OF THGOAL STATEMENTS.  MODE IS DEFINED TO BE <MODE> IN THE
    ;; MICRO-PLANNER DESCRIPTION OF THFIND (SEE AI MEMO !203A) BODY
    ;; IS A SINGLE PLANNER EXPRESSION (POSSIBLY A THAND OR THPROG).
    (condp = (car body)
        'THAND (SETQ body (cdr body))
        'THPROG (do (SETQ varlist (concat varlist (cadr body))) (SETQ body (cddr body)))
        (SETQ body (list body)))
    ;; VARLIST = <SKELETON>
    ;; BODY = <VARIABLE DECLARATIONS>
    (concat (list 'THFIND mode (plnr-var variable) varlist) body))

(defn- plnr-findspec [x]
    ;; GENERATES PARAMETER FOR THFIND FROM THE NOTATION USED IN THE DETERMINER.
    (cond
        (number? x) x
        (memq x '(NS NPL SG-PL)) 1
        (= (car x) 'EXACTLY) (list (cadr x) (inc (cadr x)) nil)
        (= (car x) '>) (inc (cadr x))
        (= (car x) '<) (list 0 (cadr x) nil)
        :else (bug "plnr-findspec -- FUNNY SPECIFICATION")))

(§ defn- PLNR-GOALIFY [plnrphrase]
    ;; TAKES A PLNRPHRASE AND MAKES A THGOAL STATEMENT OUT OF IT UNLESS
    ;; IT ISN'T SUPPOSED TO BE ONE.  ALSO CALLS PLNR-NOTIFY IF APPROPRIATE.
    ;; PRESENT TENSE TIME MARKERS ARE REMOVED TO SIMPLIFY THE MOST COMMON EXPRESSIONS.
    (SETQ plnrphrase (PLNR-REMTIME plnrphrase))
    (COND ((getprop (car plnrphrase) 'NOGOAL) plnrphrase)
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
        (evalcheck %PLNRPHRASE))))

(§ defn- PLNR-RECOMMENDIFY [%PLNRPHRASE]
    ;; LOOKS UP IN THE DICTIONARY A RECOMMENDED THEOREM TO USE IN PROCESSING A PLNRPHRASE BY THGOAL.
    ;; IF IT FINDS ONE, IT TACKS IT ON AS A RECOMMENDATION.
    (let [%ELEMENT (getprop (car %PLNRPHRASE) 'THMLIST)]
        ;; LOOK A RELATION UP IN THE DICTIONARY.  THE ENTRIES ARE SET UP AS A PROPERTY LIST.
        ;; THERE ARE DIFFERENT RECOMMENDATIONS FOR THE SAME RELATION DEPENDING ON THE NUMBER
        ;; OF ARGUMENTS THIS INSTANCE OF IT HAS.  (LENGTH ...) COMPUTES THE NUMBER OF ARGUMENTS
        ;; + 1 AND THE (ASSQ ...) RETRIEVES THE APPROPRIATE RECOMMENDATION USING THIS NUMBER.
        ;; IF THERE IS NO SUCH NUMBER, NUMBERP FAILS AND SOME ARBITARY FUNCTION WHICH
        ;; IS STORED OUT THERE IS EVALUATED TO GIVE THE RECOMMENDATION.
        (and %ELEMENT
            (eval (if (number? (caar %ELEMENT)) (cadr (or (assq (count %PLNRPHRASE) %ELEMENT) '(nil nil))) %ELEMENT)))))

(§ defn- PLNR-REMTIME [exp]
    ;; REMOVES ALL PRESENT TENSE TIME STRUCTURES
    ((lambda [y]
        ;; Y IS BOUND TO A UNIQUE POINTER SO IT CAN'T POSSIBLY SCREW ANYTHING
        ;; IN THE EXPRESSION WHEN IT DOES THE DELETION.  DELQ USES EQ.
        (DELQ y
            (doall (map #'(lambda [x]
                (cond (not (term? x)) x
                    (tss? x) (if (and (tense? x) (not (memq (tense? x) '((PRESENT PRESENT) (MODAL) (PRESENT))))) x y)
                    :else x))
            exp))))
        '(T)))

(defn- plnr-var [x]
    ;; GENERATES SYNTAX FOR VARIABLE NAMES IN PLANNER
    (list 'THV x))

(§ defn- COMPARE-BUILD [node degree]
    ;; USED BY SMADJG-PREPG TO BUILD A PSUEDO VERB FOR THE COMPARATIVE.  SMCL1 IS THEN CALLED.
    (let [RESTRICTIONSß nil DIMENSIONß nil DIRECTIONß nil]
        ;; THESE ARE THE POSSIBLE PARTS OF A MEASURE.
        (SETQQCHECK nil (cdr (findmeasure node)) '(RESTRICTIONSß DIMENSIONß DIRECTIONß) 'MEASURE)
        ;; DEFINITION
        (putprop! 'COMPARE-PSEUDO-VERB 'SEMANTICS
            (list 'RELATION
            (list 'RESTRICTIONSß
                (list (list RESTRICTIONSß)
                (list RESTRICTIONSß))
                'PROCEDUREß (list (list degree DIMENSIONß (COND (DIRECTIONß '!1) ('!2)) (COND (DIRECTIONß '!2) ('!1)))))))
        '(COMPARE-PSEUDO-VERB)))

(defn- findmeasure [node]
    ;; GETS THE MEASURE DEFINITION
    (let [r (root (NB node)) x (assq 'MEASURE (getprop r 'SEMANTICS))]
        (if x (cadr x) (global-err (str "I DON'T KNOW HOW TO COMPARE THINGS WITH RESPECT TO " r)))))

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
            ((= (SETQ x (EXPAND (car exps) (and (nil? (cdr exps)) (rssvar? var) (getprop var 'USED) (plnr-var var)))) '*ORDINAL*))
            ;; A SUBST DEFINITION IF IT IS THE ONLY THING IS TO BE APPLIED TO THE OSS TO WHICH THIS RSS WILL BE RELATED.
            ;; THE VARIABLE FOR A RELATION IS INSERTED INTO THE SECOND PLACE OF THE RELATION IF IT IS REFERRED TO ANYWHERE ELSE.
            ((and (cdr exps) (= (car x) '!SUBST)) (mapc2 #'(lambda [x y] (SETQ exps (SUBST x y exps))) (cdr x)))
            (x (SETQ BODY (cons x BODY))))
        (SETQ exps (cdr exps))
        (GO =>)))

(§ defn- RELFIND [node]
    ;; LOOKS FOR THE REL OF A POLAR
    (let [REL nil]
        (ERRSET
            ;; IT GOESFROM THE BEGINNINGOF THE SENTENCE LOOKING FOR AN INDEFINITE NG,
            ;; EITHER AT THE TOP LEVEL OR AS A FIRST LEVEL PREPOBJ, BUT NOT A COMPLEMENT.
            (MAP #'(lambda [x] (COND
                ((ISQ x NG) (and (not (ISQ x COMP)) (not (ISQ x DEF)) (SETQ REL x) (ERR nil)))
                ((ISQ x LOBJ) (and (ISQ (H x) INDEF) (SETQ REL x) (ERR nil)))
                ((ISQ x PREPG) (and (ISQ (H x) INDEF) (SETQ REL (H x)) (ERR nil)))))
            (reverse (H node))))
        (or REL (and (cq 'PASV *fe*) (not (cq 'AGENT *fe*)) (SETQ REL '(FAKE-AGENT))))
        (and REL (SM REL))))

(§ defn- ORDMAKE [ordinal var body]
    ;; MAKES THE LOGICAL FORM FOR SUPERLATIVES.
    ;; ORDINAL GIVES THE THING BEING COMPARED IN MEASURE FORM.
    (let [NEWVAR (gensym 'X)]
        (PLNR-PROGIFY nil
            (concat body (list (PLNR-NOTIFY true
                (PLNR-PROGIFY (list NEWVAR) (concat (SUBST NEWVAR var body) (list (PLNR-GOALIFY (COMPARE-PROC var NEWVAR ordinal)))))))))))

(§ defn- COMPARE-PROC [var newvar ordinal]
    (let [RESTRICTIONSß nil DIRECTIONß nil DIMENSIONß nil]
        (SETQQCHECK nil ordinal '(RESTRICTIONSß DIRECTIONß DIMENSIONß) 'MEASURE)
        (list '!MORE DIMENSIONß (plnr-var (if DIRECTIONß newvar var)) (plnr-var (if DIRECTIONß var newvar)))))

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
            (COND (ORDINAL (global-err "I CAN'T HANDLE TWO ORDINALS OR SUPERLATIVES AT ONCE"))
                ((SETQ ORDINAL (cadr exp)) '*ORDINAL*)))
        ((= (car exp) '!SUBST)
            (ert "EXPAND - IS !SUBST BEING HANDLED BY SOMEONE ELSE?")
            exp)
        ((let [BODY nil QUANTIFIER nil CHOICE nil VAR nil MULTIPLE (eval (getprop (car exp) 'MULTIPLE))]
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
                        (and (rssvar? (variable? x)) (putprop! (variable? x) 'USED true))
                        (plnr-var (variable? x)))
                    ((SETQ CHOICE (and? x))
                        (ERQSET 'AND)
                        (and MULTIPLE (refer? x) (SETQ CHOICE (refer? x)))
                        '*AND*)
                    ((SETQ CHOICE (or? x))
                        (ERQSET 'OR)
                        '*OR*)
                    ((COND
                        ((rss? x) (ERQSET 'EVENT) (putprop! (variable? x) 'USED true))
                        ((memq (quantifier? x)
                            '(ALL NO)) (ERQSET (quantifier? x)) true)
                        ((memq (quantifier? x) '(NDET INDEF))
                            (COND ((memq (num? x) '(NS SG-PL)) (ERQSET 'INDEF)) ((SETQ CHOICE (plnr-findspec (num? x))) (ERQSET 'FIND))) true))
                        (SETQ BODY (PLNR-DESCRIBE (relations? x) (variable? x) (cons (variable? x) FREEVARS)))
                        (plnr-var (SETQ VAR (variable? x))))
                    ((bug "EXPAND - STRANGE QUANTIFIER"))))
                    (if event (cons (car exp) (cons event (cdr exp))) exp))))
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
    (COND (QUANTIFIER (global-err "I CAN'T HANDLE COMBINATIONS OF QUANTIFIERS AND CONNECTIVES WHICH ARE SO COMPLICATED"))
        ((SETQ QUANTIFIER x))))

(§ defn- SETQQCHECK [%EVALFLAG %LIST %CHECKLIST %NAME]
    ;; SETQQCHECK IS LIKE SETQQ (OR LIKE SETQ DEPENDING ON EVALFLAG) BUT IT CHECKS TO MAKE SURE
    ;; THE VARIABLE NAME IS A MEMBER OF THE %CHECKLIST, AND IF NOT PRINTS AN ERROR MESSAGE.
    (let [%X nil]
    GO  (COND ((nil? %LIST) (RETURN true))
            ((memq (car %LIST) %CHECKLIST)
                (SET (car %LIST) (if %EVALFLAG (eval (cadr %LIST)) (cadr %LIST)))
                (SETQ %LIST (cddr %LIST))
                (GO GO))
            (:else (SETQ %X (ert (str (car %LIST) " IS NOT A LEGAL SPECIFICATION FOR " %NAME)))))
    UP  (COND
            ;; A QUESTION MARK GETS THE LIST OF POSSIBILITIES PRINTED OUT, THEN LETS YOU TRY AGAIN.
            ;; TO DO THIS YOU MUST TYPE (RETURN '?) AT THE ERT.
            ;; IF YOU RETURN ANY OTHER VALUE, IT ASSUMES THIS IS THE VARIABLE NAME INTENDED,
            ;; OTHERWISE IT JUST CAUSES AN ERROR.
            ((= %X '?)
                (terpri)
                (pr %CHECKLIST)
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
        ((not (SETQ x (getprop x 'WHO))) nil)
        ((= WHO 'HE))
        ((< (car WHO) x *lastsentno*))))

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
        (SETQ NEW-SYSTEMS (getprop marker 'SYS))                    ;; GET THE SYSTEMS OF THE NEW MARKER
    =>  (COND ((nil? NEW-SYSTEMS) (RETURN true))
            ((memq (car NEW-SYSTEMS) SYSTEMS) (RETURN nil)) ;; FAIL IF SYSTEM THERE BY ANOTHER PATH
            ((CHECKAMARKER (car NEW-SYSTEMS))
                (SETQ SYSTEMS (cons (car NEW-SYSTEMS) SYSTEMS))
                (SETQ NEW-SYSTEMS (cdr NEW-SYSTEMS))
                (GO =>))
            (:else (RETURN nil)))))

(§ defn- FINDEVENTS [rss]
    ;; FINDS ALL THE EVENTS FITTING THE RSS DESCRIPTION
    (putprop! (variable? rss) 'USED true)
    (THVAL2 nil
        (PLNR-FINDIFY 'ALL (variable? rss) (list (variable? rss))
            (PLNR-DESCRIBE (relations? rss) (variable? rss) (list (variable? rss))))))

(defn- checkrel [oss]
    ;; CHECKS FOR POSSIBLE RELATIVE, EITHER BECAUSE OSS IS ON THE RELLIST,
    ;; OR BECAUSE RSS INVOLVES INSIDE IT AN OSS ON THE RELLIST.
    ;; IT RETURNS EITHER NIL OR A LIST OF WHICH THE FIRST ELEMENT IS THE REAL RELATIVE.
    ;; IT USES THIS FACT TO CHEAT ON RECURSION BY USING MAPCAN.
    (cond
        (oss? oss) (memq oss RELLIST)
        (rss? oss) (doall (mapcat #(when-not (term? %) (mapcat checkrel %)) (relations? oss)))))

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
    (getprop x 'ACTION=))

(defn- ambiguities? [x]
    ;; LIST OF POSSIBLE AMBIGUITIES FOR A SEMANTIC STRUCTURE.
    (getprop x 'AMBIGUITIES=))

(defn- and? [x]
    ;; FIND THE CONJUNCTION LIST OF A CONJOINED SEMANTIC STRUCTURE.
    ;; NIL IF THERE IS NONE.
    (getprop x 'AND=))

(defn- ansrss? [x]
    ;; THE RSS CORRESPONDING TO AN ANSWER NODE (A PROPERTY OF THE ANSNODE)
    (getprop x 'ANSRSS=))

(defn- determiner? [x]
    ;; ACCESS FUNCTION.  GETS DET OF AN OSS.
    (getprop x 'DETERMINER=))

(defn- end? [tss]
    ;; END TIME FOR TSS.
    (getprop tss 'END=))

(defn- markers? [%SENSE]
    ;; ACCESS FUNCTION USED TO GET MARKERS FROM OSS OR RSS.
    (getprop %SENSE 'MARKERS=))

(defn- modifiers? [%XSS]
    ;; ACCESS FUNCTION FOR GETTING THE PLANNER CODE SCHEMA OF AN OSS OR RSS.
    (getprop %XSS 'MODIFIERS=))

(defn- negative? [%XSS]
    ;; ACCESS FUNCTION FOR OSS.
    (getprop %XSS 'NEGATIVE=))

(defn- num? [oss]
    ;; GETS THE NUMBER FIELD OF AN OSS.
    (car (getprop oss 'DETERMINER=)))

(defn- or? [x]
    ;; ACCESS FOR LIST OF CONSTITUENTS IN DISJOINED SEMANTIC STRUCTURE.
    ;; NIL IF IT ISN'T.
    (getprop x 'OR=))

(defn- oss? [x]
    ;; CHECKS TO SEE IF X IS AN OSS.
    (getprop x 'OSSNODE=))

(defn- parsenode? [x] (getprop x 'PARSENODE=))

;; THE PARSE NODE ASSOCIATED WITH THE SEMANTIC STRUCTURE.

(defn- plausibility? [%XSS]
    ;; ACCESS FUNCTION FOR GETTING PLAUSIBILITY OF AN OSS OR RSS.
    (or (getprop %XSS 'PLAUSIBILITY=) 0))

(defn- plnrcode? [x] (getprop x 'PLNRCODE=))

;; THE PLANNERCODE GENERATED WHEN AN OBJECT IS ACTUALLY LOOKED FOR IN THE DATABASE.
;; IT IS NOT USED AGAIN, BUT IS LEFT SITTING AROUND FOR PEOPLE TO LOOK AT.

(defn- qtype? [x] (caddr (getprop x 'DETERMINER=)))

;; QUESTION TYPE FOR QUESTION OSS.

(defn- quantifier? [oss]
    ;; GETS THE DETERMINER FIELD OF AN OSS.
    (cadr (getprop oss 'DETERMINER=)))

(defn- refer? [%XSS]
    ;; ACCESS FUNCTION FOR REFER OF OSS OR RSS.
    (getprop %XSS 'REFER=))

(defn- rel? [x] (getprop x 'REL=))

;; THE OBJECT TO WHICH THIS DESCRIPTION IS TO BE RELATED.

(defn- relations? [x]
    ;; THE MATERIAL THAT WILL BECOME PLANNER CODE.
    (getprop x 'RELATIONS=))

(defn- relmarkers? [x]
    ;; MARKERS HELD BY A RELATIVE CLAUSE PRODUCED BY ITS SELECTION
    ;; RESTRICTIONS, TO BE ATTACHED TO THE OBJECT DESCRIPTION.
    (getprop x 'RELMARKERS=))

(defn- rss? [x]
    ;; CHECKS TO SEE IF X IS AN RSS.
    (getprop x 'RSSNODE=))

(defn- rssvar? [x]
    ;; A VARIABLE OF THE TYPE USED FOR RSS'S -- I.E. EVX3, ETC.
    (getprop x 'RSSVAR))

(defn- start? [tss]
    ;; START TIME FOR TSS.
    (getprop tss 'START=))

(defn- systems? [%SENSE]
    ;; ACCESS FUNCTION FOR GETTING THE SYSTEMS OF AN OSS OR RSS.
    (getprop %SENSE 'SYSTEMS=))

(defn- tense? [x]
    ;; FOR A TSS.
    (getprop x 'TENSE=))

(defn- tss? [x] (getprop x 'TSSNODE=))

;; ASSOCIATED WITH EVERY TIME SEMANTIC STRUCTURE.

(defn- variable? [x]
    ;; ACCESS FUNCTION FOR GETTING THE VARIABLE NAME ASSOCIATED WITH AN OSS OR RSS.
    (getprop x 'VARIABLE=))

(§ defn- SMSET [x] (setr 'SEMANTICS x *c*) (SETQ SM x))

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
        (set! *ansname* nil)
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
        (print \.)
        (terpri)
        ;; DOBACKREF STORES AWAY DISCOURSE INFORMATION.
        (DOBACKREF (car anslist))
        true))

(§ defn- AMBPUT [code]
    ;; PUTS IN THE JUNK FOR DISCOURSE IF THERE IS NO AMBIGUITY, SO THERE IS
    ;; NO NEED TO EVALUATE THE CODE A SECOND TIME WHEN GIVING THE ANSWER.
    (if AMBIG code (plnr-junkify code)))

(§ defn- ANSBUILD [PLAUS ACTION REDEDUCE]
    ;; BUILDS AN ANSWER NODE.
    ;; IF REDEDUCE IS NON-NIL, IT ADDS A REDEDUCTION OF THE ANSWER,
    ;; ADDING THE DISCOURSE JUNK TO THE ACTION.
    (BUILD
        ANSNODE= (gensym 'ANS)
        PLAUSIBILITY= PLAUS
        ANSRSS= RSS
        ACTION= (concat
                    (COND
                        ((and AMBIG REDEDUCE (not (cq 'DECLAR *fe*)))
                            (cons (list 'THVAL2 nil (list 'plnr-junkify (list 'plnrcode? (list 'quote RSS)))) ACTION))
                        (:else ACTION))
                    (and (rel? RSS) (not (cq 'DECLAR *fe*)) (list (list 'putprop! (quotify (rel? RSS)) (quotify 'REFER=) (quotify ANS)))))))

(§ defn- ANSCOMMAND [RSS]
    ;; ANSCOMMAND RESPONDS TO IMPERATIVES.
    ;; PLNR-ANDORIFY COMBINES ANDS AND ORS INTO APPROPRIATE PLANNER THANDS AND THORS.
    (let [exp (PLNR-ANDORIFY RSS) ANS nil SUCCESS nil] (binding [*plan* nil *plan2* nil]
        (putprop! RSS 'PLNRCODE= exp)
        (SETQ exp (AMBPUT exp))
        (SETQ exp (COND
            ((= (car exp) 'THAND) (concat exp '((SETQ SUCCESS true) (set! *plan2* *plan*))))
            (:else (list 'THAND exp '(SETQ SUCCESS true) '(set! *plan2* *plan*)))))
        ;; IN CASE OF MULTIPLE INTERPRETATION, THE SYSTEM USES FAILURE TO WIPE OUT THE EFFECTS OF TRYING OUT ONE OF THEM.
        ;; BEFORE FAILING IT MARKS DOWN WHETHER IT SUCCEEDED AND SAVES THE PLAN FROM BACKTRACKING.
        ;; PLNR-JUNKIFY PUTS ON THE JUNK FOR SAVING THE DISCOURSE REFERENTS ETC.
        (THVAL2 nil (if AMBIG (concat exp '((THFAIL))) exp))
        (RETURN
            ;; THE THIRD ARGUMENT TO ANSBUILD CAUSES THE SYSTEM TO GO BACK THROUGH THE DEDUCTION
            ;; TO GET THE DATABASE STRAIGHT IF THIS ANSWER IS PICKED.  IT ALSO TAKES CARE OF THE BACKREF STUFF.
            (ANSBUILD
                (if SUCCESS (plausibility? RSS) (- (plausibility? RSS) 512))
                (if SUCCESS (concat (reverse *plan2*) '((say "OK"))) '((say "I CAN'T")))
                true)))))

(§ defn- ANSDECLARE [RSS]
    ;; FOR DECLARATIVES.
    (COND
        ((or? RSS)
            (global-err "I DON'T UNDERSTAND DISJUNCTIVE DECLARATIVES"))
        ((and? RSS)
            (let [ANS nil]
                ;; CONJOINED DECLARATIVES ARE HANDLED BY DOING EACH ONE SEPARATELY.
                (SETQ ANS (doall (map #'ANSDECLARE (and? RSS))))
                (RETURN (ANSBUILD
                    (reduce + (map plausibility? ANS))
                    (cons '(say "I UNDERSTAND") (doall (map #'(lambda [x] (DELETE '(say "I UNDERSTAND") (action? x))) ANS)))
                    nil))))
        ((not (istense (parsenode? RSS) 'PRESENT))
            (global-err "I ONLY UNDERSTAND PRESENT TENSE DECLARATIVES"))
        (:else (ANSBUILD
            (plausibility? RSS)
            ;; ANSTHM GENERATES THE APPROPRIATE ASSERTION OR THEOREM.
            (cons '(say "I UNDERSTAND") (doall (map #'(lambda [x] (list 'THADD (quotify (ANSTHM x)) nil)) (relations? RSS))))
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
        (terpri)
        (print "I'M NOT SURE WHAT YOU MEAN BY \"")
        (dorun (map -print (from (NB (caddar AMB)) (N (caddar AMB)))))
        (print "\" IN THE PHRASE \"")
        (dorun (map -print (from (NB (SETQ XX (parent (caddar AMB)))) (N XX))))
        (print "\".")
        (terpri)
        (print "DO YOU MEAN:")
        (dorun (map-indexed #'(lambda [i POSS]
                (terpri) (print (inc i)) (dorun (map -print (cadr POSS)))) ;; THE PARAPHRASE
            POSSIBILITIES))
        (print \?)
        (terpri)
    READ (SETQ XX (READ))
        (COND ((or (not (number? XX)) (> XX (count POSSIBILITIES)))
            (terpri)
            (print "PLEASE TYPE ONE OF THE NUMBERS")
            (terpri)
            (GO READ)))
        (SETQ POSSIBILITIES (nth POSSIBILITIES (dec XX)))
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
        ((or (cq 'IMPER *fe*) (and (cq 'QUEST *fe*) (istense (parsenode? rss) 'FUTURE))) ;; FUTURE QUESTIONS ARE TREATED LIKE COMMANDS.
            (ANSCOMMAND rss))
        ((cq 'DECLAR *fe*)
            (let [x nil]
                (RETURN (COND
                    ((ERRSET (SETQ x (ANSDECLARE rss))) x)
                    ;; THIS STRANGE CONSTRUCTION ALLOWS US A SECOND CHANCE ON DECLARATIVES ABOUT THINGS WHICH CAN'T
                    ;; BE TOLD TO THE SYSTEM.  IF IT RUNS INTO ONE OF THEM, IT TRIES TO ANSWER IT AS A QUESTION.
                    ((= *global-message* "THAT ISN'T THE KIND OF THING I CAN BE TOLD") (ANSQUEST rss))
                    ((ERR nil))))))
        ((cq 'QUEST *fe*) (ANSQUEST rss))
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
    (let [ansnode nil *c* nil N nil CUT nil]
        (SETQ N (cdaar phrase))                                     ;; CDR IS TO REMOVE "SAY"
        (SETQ ansnode (PARSE2 '(NG ANSNAME) true))                     ;; THE T SAYS NOT TO ATTACH THIS TO THE TREE
        (or ansnode
            (RETURN (ert "ANSNAME: FAILURE TO PARSE ANSWER NAME, BUT IF YOU ONLY EXPECT THE ANSWER TO BE AN ADJ, PROCEED THIS AND DON'T WORRY")))
        (set! *ansname* (concat ansnode *ansname*))                     ;; LEAVE NODE AROUND IT ACCESSABLE PLACE
        (putprop! (car (SM ansnode)) 'REFER= (cadr phrase))          ;; PUT THE REFERENT ON AS THE GUY GIVEN BY ANSWER
        nil))

(§ defn- ANSNOREL [rss]
    ;; FOR QUESTIONS WITH NO RELATIVE, LIKE "DID YOU PICK UP THE BLOCK?" OR "WHY DID YOU DO THAT?"
    (let [ans nil type nil code nil NODE (parsenode? rss) var nil]
        (SETQ type (COND ;; THE TYPE SHOULD BE POLAR, WHY, WHERE, WHEN, OR HOW.
            ((ISQ NODE POLAR) 'POLAR)
            ((SETQ type (getr 'QADJ NODE)) (car (NB type)))
            ((bug "ANSNOREL -- FUNNY TYPE"))))
        (putprop! (variable? rss) 'USED true)
        (SETQ code
            (PLNR-DESCRIBE (relations? rss)
                ;; IN PRESENT TENSE CASES, WE DON'T LOOK FOR EVENTS.
                ;; OTHERWISE WE LOOK FOR A SET OF APPROPRIATE EVENTS NO MATTER WHAT THE TYPE.
                (COND ((istense NODE 'PRESENT) nil) ((SETQ var (variable? rss))))
                (list (variable? rss))))
        (putprop! rss 'PLNRCODE= code)
        (RETURN (COND
            ((not var)
                (SETQ ans (THVAL-MULT (AMBPUT code)))
                (ANSBUILD
                    (+ (car ans) (plausibility? rss))
                    (cond (cadr ans) '((say "YES")) (istense NODE 'MODAL) '((say "I DON'T KNOW")) :else '((say "NO")))
                    true))
            ((SETQ ans (THVAL-MULT (PLNR-FINDIFY 'ALL var (list var) (AMBPUT code))))
                (ANSBUILD
                    ;; AN ANSWER IS VERY IMPLAUSIBILE IF IT MENTIONS AN EVENT THE SYSTEM CAN'T FIND.
                    (if (cadr ans) (+ (plausibility? rss) (car ans)) (- (plausibility? rss) 512))
                    (COND ((nil? (cadr ans)) '((say "I CAN'T DISCUSS A NON-EXISTENT EVENT")))
                        ((concat (and (= type 'POLAR) '((say "YES"))) (list (list 'evlis (list 'DESCRIBEVENT (quotify (cadr ans)) (quotify type)))))))
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
                (reduce + (map plausibility? ans))
                (concat
                    (and (not (ISQ (parsenode? RSS) COMPONENT)) '((say "YOU'RE TRYING TO CONFUSE ME.")))
                    (doall (map #'(lambda [QUEST]
                        (concat '((terpri))
                            (list (cons 'say (ELIZA (from (NB (parsenode? (ansrss? QUEST))) (N (parsenode? (ansrss? QUEST)))))))
                            '((print \?) (terpri))
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
        (SETQ PHRASE (cons 'nil (headpart (parsenode? REL))))
        ;; THIS IS FOR THE PART OF THE GENERATOR THAT WILL SUBSITUTE "ONE" FOR NOUN NAMES.
        ;; THE LEADING NIL IS TO MAKE THIS PHRASE COMPATIBLE WITH THE "SAY" PHRASES WHICH THE OTHER PARTS GENERATE.
        ;; UNIVERSALS ARE CONVERTED TO NOT THERE EXISTS NOT.
        (SETQ TYPE (or (qtype? REL) (quantifier? REL) (bug "ANSREL -- NO TYPE")))
        (and (= TYPE 'ALL) (putprop! RSS 'NEGATIVE= true))
        (SETQ CODE (PLNR-FINDIFY 'ALL (variable? REL) (list (variable? REL)) (PLNR-DESCRIBE (cons RSS (relations? REL)) (variable? REL) (list (variable? REL)))))
        (putprop! RSS 'PLNRCODE= CODE)
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
                    (COND ((nil? ANS) '((say "YES"))) ((cons '(say "NO, NOT") (PREPPUT (namelist PHRASE 'INDEF ANS)))))
                    true))
            ((= TYPE 'HOWMANY)
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS))
                    (PREPPUT (namesugar LENGTH REL))
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
                                ((nil? ANS) (if (istense (parsenode? RSS) 'MODAL) '((say "I DON'T KNOW")) '((say "NO"))))
                                (:else (concat '((say "YES,")) (COND
                                    ((istense (parsenode? RSS) 'MODAL) nil)
                                    ((PREPPUT (concat (and (cdr ANS) (concat (namesugar LENGTH REL) '((print \:)))) (namelist PHRASE 'INDEF ANS)))))))))
                        ((number? NUM)
                            ;; THIS IS THE CASE WHERE WE ARE CAGEY AND AVOID ANSWERING YES OR NO.
                            ;; THE NUMBER ISN'T REPEATED IF IT IS THE SAME AS THE NUMBER IN THE SPECIFICATION.
                            (concat
                                (COND
                                    ((= NUM LENGTH) '((say "YES,")))
                                    ((> LENGTH NUM) nil)
                                    ((zero? NUM) '((say "NO,")))
                                    (:else '((say "NO, ONLY"))))
                                (COND
                                    ((= NUM LENGTH) nil)
                                    (:else (PREPPUT (concat (namesugar LENGTH REL) '((print \:))))))
                                (PREPPUT (namelist PHRASE 'INDEF ANS))))
                        ((= (car NUM) 'EXACTLY)
                            (COND ((= LENGTH NUM) '((say "YES")))
                                (:else (cons '(say "NO,") (PREPPUT (namesugar LENGTH RES))))))
                        ((= (car NUM) '>)
                            (cons (COND
                                    ((> LENGTH NUM) '(say "YES,"))
                                    ((zero? LENGTH) '(say "NO,"))
                                    (:else '(say "NO, ONLY")))
                                (PREPPUT (namesugar LENGTH REL))))
                        ((= (car NUM) '<)
                            (cons (COND
                                    ((< LENGTH NUM) '(say "YES,"))
                                    (:else '(say "NO,")))
                                (PREPPUT (namesugar LENGTH REL))))
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
            ((not (getprop (car exp) 'TELLABLE)) (notell))
            (:else
                (SETQ NEG (negative? RSS))
                (SETQ exp (doall (map #'ANSTHMELEMENT (PLNR-REMTIME exp))))
                (RETURN (COND ((not (or VARLIST NEG)) exp)
                    (:else (PLNR-THCONSIFY VARLIST exp (if NEG (PLNR-PROGIFY nil (list BODY '(THFAIL THEOREM))) BODY)))))))
        nil))

(§ defn- ANSTHMADD [oss]
    (SETQ VARLIST (cons (variable? oss) VARLIST))
    (SETQ BODY (COND
        (BODY (PLNR-PROGIFY nil (list BODY (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))))
        (:else (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))))
    (plnr-var (variable? oss)))

(§ defn- ANSTHMELEMENT [x]
    (COND ((not (term? x)) x)
        ((tss? x) (notell))
        ((rss? x) (notell))
        ((not (oss? x)) x)
        ((refer? x) (atomify (refer? x)))
        ((= (quantifier? x) 'ALL) (if NEG (notell) (ANSTHMADD x)))
        ((= (quantifier? x) 'NO) (SETQ NEG true) (ANSTHMADD x))
        ((= (quantifier? x) 'NDET) (ANSTHMADD x))
        ((not (= (quantifier? x) 'INDEF)) (notell))
        ((ISQ (parsenode? x) ANY) (ANSTHMADD x))
        (:else (global-err "YOU HAVE TO TELL ME WHICH"))))

(defn- ansunique [l]
    ;; THIS FUNCTION SHOULD ELIMINATE ANSWERS WHICH GIVE THE SAME
    ;; RESULT EVEN THOUGH THEY INVOLVE DIFFERENT INTERPRETATIONS.
    ;; IT NEEDS TO CHECK FOR SIGNIFICANT DIFFERENCES, E.G. IN WHAT
    ;; GETS PRINTED OR DONE, WHILE IGNORING INSIGNIFICANT ONES,
    ;; E.G. THE NAMES OF ATOMS TO WHICH THINGS ARE ATTACHED.
    ;; FOR THE MOMENT, IT JUST RETURNS THE LIST UNTOUCHED.
    l)

(§ defn- CUTOFF [x]
    ;; FOR CUTTING # OFF OF CONCEPT NAMES TO GET ENGLISH WORDS.
    (READLIST (cdr (EXPLODE x))))

(§ defn- DESCRIBEVENT [event type]
    (SETQ event (car event))
    (COND
        ((= type 'WHERE)
            (global-err "I CAN'T ANSWER \"WHERE\" QUESTIONS YET"))
        ((= type 'WHY)
            (COND ((= (getprop event 'WHY) 'COMMAND) '((say "BECAUSE YOU TOLD ME TO")))
                (:else (cons '(say "TO") (NAMEACTION 'INFINITIVE (getprop event 'WHY))))))
        ((= type 'HOW)
            (let [ans nil]
                (dorun (map #'(lambda [x] (and (= (getprop x 'WHY) event) (SETQ ans (cons x ans)))) EVENTLIST))
                (COND
                    ((nil? ans) '((say "I CAN'T ANALYZE HOW I DID IT")))
                    (:else (concat '((say "BY")) (NAMEACTION 'ING (car ans)) (doall (map #'(lambda [x] (cons '(print \;) (CONS '(say "THEN") (NAMEACTION 'ING x)))) (CDR ans))))))))
        ((or (= type 'POLAR) (= type 'WHEN))
            (COND
                ((= (getprop event 'WHY) 'COMMAND)
                    (COND
                        ((= event (toplevel (car EVENTLIST))) '((say "JUST NOW")))
                        (:else (cons '(say "BEFORE") (NAMEACTION 'PAST (toplevel (car (findb event EVENTLIST))))))))
                (:else (cons '(say "WHILE") (NAMEACTION 'PRES-PAST (toplevel event))))))
        ((bug "DESCRIBEVENT -- FUNNY type"))))

(defn- disput [assertion]
    ;; PUT THE SENTENCE NUMBER ON THE ASSERTION AS A WHO PROPERTY
    (or (not discourse?) (putprop! assertion 'WHO *sentno*)))

(§ defn- ELIZA [node]
    ;; DOES THE OBVIOUS THING.
    (let [xx nil num (count (N node))]
        (APPLY #'concat
            (MAPLIST #'(lambda [word]
                (COND
                    ((not (< num (count word))) nil)                ;; THIS KLUDGE STOPS IT AT THE END OF THE NODE
                    ((SETQ xx (assq (car word) '((I YOU) (ME YOU) (AM ARE) (ARE AM))))
                        (cdr xx))                                   ;; WE RETURN LIST OF THE THING REALLY WANTED, SO
                    ((= (car word) 'YOU)                           ;; THE APPLY APPEND CAN GET RID OF THE EMPTY ONES.
                        (SETQ xx (FINDMOTHER word node))            ;; UNFORTUNATELY, FOR "YOU" IT IS NECESSARY TO
                        (COND ((ISQ xx SUBJ) '(I))                  ;; DECIDE WHETHER IT SHOULD BE REPLACED BY "I" OR
                            ((ISQ xx OBJ) '(YOU))                   ;; "ME", ACCORDING TO WHETHER IT WAS PARSED AS AN
                            ((bug "ELIZA -- SUBJ OBJ"))))             ;; OBJECT OR SUBJECT. FINDMOTHER IS USED TO FIND
                    ((list (car word)))))                           ;; THE PARSE NODE. WORDS OTHER THAN THE SPECIAL
                (NB node)))))                                       ;; ONES GO THROUGH DIRECTLY.

(def- timid 200)

(defn- enough-better [ans1 ans2]
    (> (plausibility? ans1) (+ (plausibility? ans2) timid)))

(§ defn- FINDMOTHER [word node]
    ;; FINDMOTHER TAKES A PLACE IN THE SENTENCE AND A GRAMMAR NODE
    ;; (BOTH ARE ACTUALLY LISTS) AND FINDS THE SINGLE-WORD
    ;; CONSTITUTENT BEGINNING AT THAT PLACE IN THE SENTENCE.
    (COND ((and (= word (NB node)) (= (cdr word) (N node))) node)
        (:else (APPLY #'concat (MAPLIST #'(lambda [NOD] (FINDMOTHER word NOD)) (H node))))))

(defn- headpart [node]
    ;; EVERYTHING UP TO THE NOUN, FOR EXAMPLE "THE RED BLOCK" IN "THE RED BLOCK WHICH ..."
    ;; NOTE THAT NODE IS ACTUALLY A LIST OF NODE (A PROPER GRAMMAR POINTER).
    (and (set! *pt* node) (MOVE-PT 'DLC 'PV '(NOUN)) (from (NB node) (N *pt*))))

(§ defn- LISTNAMES [PHRASE spec names]
    ;; PHRASE IS THE INITIAL THING TO COMPARE FOR USING "ONE", SPEC IS EITHER DEF OR INDEF, AND THE NAMES ARE OF DATA-BASE OBJECTS.
    ;; LISTNAMES PUTS OUT AN ACTION LIST, AS WELL AS PUTTING THINGS ONTO THE BACKREF.
    ;; IT IS CALLED AFTER THE ANSWER HAS BEEN DECIDED ON.
    (let [COUNT nil EXAM nil x nil RES nil ANS nil comma? false]
        ;; NAMEOBJ RETURNS A LIST OF THE OBJECT AND THE ...
        ;; THIS PATCH MAY WELL BE TOTAL OUT OF PHASE WITH THE BACKREF HACKER - DDM 5-12-73 INSTRUCTIONS FOR NAMING IT.
        (SETQ names (doall (map #'(lambda [x] (NAMEOBJ x spec)) names)))
        (COND ((nil? names) (RETURN '(say "NOTHING"))))
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
                    (COND ((propname (caadr PHRASE2)) (car PHRASE2))
                        ;; ANSNAME PARSES THE PHRASE AND PUTS THE ...
                        ;; ANSONE SUBSTITUTES "ONE" IF POSSIBLE
                        (:else (ANSNAME PHRASE2) (ONECHECK (car PHRASE2)))))
                RES)))
        (SETQ ANS (car RES))
    OUTPUT (COND
            ((nil? (SETQ RES (cdr RES))) (RETURN ANS))
            ((cdr RES) (SETQ comma? true) (SETQ ANS (concat ANS '((print \,)) (car RES))))
            ((SETQ ANS (concat ANS (and comma? '((print \,))) '((say "AND")) (car RES)))))
        (GO OUTPUT)))

(defn- pron-prt [particle ng]
    ;; THIS IS EVENTUALLY SUPPOSED TO BE THE PLACE FOR THE PRONOUN-PARTICLE-INTERACTION MAGIC
    ;; TO HAPPEN, IE. "CLEAR OFF THE BLOCK." VS. "CLEAR IT OFF" SINCE "CLEAR OFF IT." IS
    ;; UNGRAMMATICAL AND "CLEAR THE BLOCK OFF." WOULD NOT BE APPROPRIATE IN CASES OF HEAVY-NP'S.
    ;;
    ;; AT THE MOMENT, FOR SIMPLICITY'S SAKE, I'VE IGNORED THE
    ;; PROBLEM AND THE PARTICLE IS ALWAYS PUT BEFORE THE NG.
    (cons (list 'say particle) (namelist-evaled '(nil) 'DEF ng)))

(§ defn- NAMEACTION [TENSE event]
    ;; THIS FUNCTION SETS UP A LIST OF S-EXPRESSIONS WHICH ARE RETURNED TO DESCRIBEVENT AND WHICH
    ;; WHEN EVALUATED WILL PRINT OUT AN ENGLISH DESCRIPTION OF THE SINGLE, SIMPLE EVENT IMBEDDED
    ;; IN THE LIST "THASSERTION" WITH THE TENSE SPECIFIED.
    ;; THE THASSERTION PROPERTY IS A LIST THAT TYPICALLY LOOKS LIKE "(NIL (2 (3 1 ((!GRASP ßE2 ßB6)))))"
    (let [a (car (caddr (cadadr (getprop event 'THASSERTION)))) verb (CUTOFF (car a)) obj1 (caddr a) obj2 (cadddr a)]
        (condp = verb
            'CLEARTOP   (cons (list 'say (vbfix 'CLEAN false)) (pron-prt "OFF" obj1))
            'GET-RID-OF (cons (list 'say (vbfix 'GET true) "RID OF") (namelist-evaled '(nil) 'DEF obj1))
            'GRASP      (cons (list 'say (vbfix 'GRASP true)) (namelist-evaled '(nil) 'DEF obj1))
            'PICKUP     (cons (list 'say (vbfix 'PUT true)) (pron-prt "UP" obj1))
            'PUTON      (concat (cons (list 'say (vbfix 'PUT true)) (namelist-evaled '(nil) 'DEF obj1)) (pron-prt "ON" obj2))
            'PUTIN      (concat (cons (list 'say (vbfix 'PUT true)) (namelist-evaled '(nil) 'DEF obj1)) (pron-prt "IN" obj2))
            'STACKUP    (cons (list 'say (vbfix 'STACK true)) (pron-prt "UP" obj1))
            'RAISEHAND  nil
                        (bug "NAMEACTION: I DON'T KNOW WHAT TO DO WITH THE VERB I GOT"))))

(defn- namelist [one spec listx]
    ;; GENERATES A LIST OF EXPRESSIONS TO BE EVALUATED WHICH WILL CAUSE THE APPROPRIATE NAMELIST TO BE PRINTED OUT.
    ;; THE ARGUMENTS ARE JUST THOSE TO LISTNAMES.
    (list (list 'evlis (list 'LISTNAMES (quotify one) (quotify spec) (quotify listx)))))
    ;; A TYPICAL CALL WOULD RESULT IN A VALUE OF ((EVLIS (LISTNAMES '(A RED BLOCK) 'INDEF '(ßB1 ßB7)))) WHICH WOULD BE EVALUATED LATER.
    ;; NOTE THAT LISTNAMES WILL IN TURN PRODUCE A LIST OF EXPRESSIONS TO BE EVALUATED, WHICH WILL BE CAUGHT BY THE EVLIS.  CONFUSING?

(defn- namelist-evaled [one spec listx]
    (list (eval (list 'LISTNAMES (quotify one) (quotify spec) (quotify listx)))))

(def- numbers* ["NONE" "ONE" "TWO" "THREE" "FOUR" "FIVE" "SIX" "SEVEN" "EIGHT" "NINE" "TEN"])

(defn- namenum [x]
    ;; GENERATES NUMBER NAMES.
    (cond (neg? x) (global-err "I CAN'T COUNT THAT LOW") (< x (count numbers*)) (nth numbers* x) :else (global-err "I CAN'T COUNT THAT HIGH")))

(defn- ansay [x]
    ;; GENERATES THE SYNTAX FOR ANSWER ACTIONS FROM A PHRASE.
    (list (cons 'say x)))

(§ defn- NAMEOBJ [ITEM spec]
    ;; NAMES THE OBJECT IN ENGLISH -- GENERATES LIST OF THINGS TO BE EVALUATED.  SPEC IS EITHER 'INDEF OR 'DEF
    (let [TYPEß nil TYPELIST nil TYPE nil NAMEß nil COLORß nil COLORLIST nil SIZEß nil SIZELIST nil CUBE nil NAME nil x nil]
        (and (SETQ x (assq ITEM '((ßSHRDLU I) (ßFRIEND YOU))))
            (RETURN (list (ansay (cdr x)) (list ITEM))))                        ;;  SPECIAL CASE CHECK
        (THVAL2 nil '(THGOAL (!NAMEOBJ) (THUSE TC-NAMEOBJ)))
        (or TYPELIST
            (ert "NAMEOBJ -- OBJECT WITH NO !IS ASSERTION"))
        ;; DISPUT CHECKS TO SEE IF DISCOURSE IS BEING KEPT, AND IF SO PUTS THE RELEVANT SENTENCE NUMBER AS A PROPERTY ON THE ASSERTION.
        (disput TYPEß)
        (COND ((= (SETQ TYPE (caddar TYPEß)) '!NAME)                           ;; A NAME IS ITS OWN NAME.
                (RETURN (list (ansay (list ITEM)) (list ITEM))))
            ((memq '!PROPERTY (getprop TYPE 'SYS))
                ;; CUTOFF CUTS THE # OFF OF NAMES LIKE !RED AND !POINTED WHICH ARE USED FOR PROPERTIES.
                (RETURN (list (ansay (list (CUTOFF ITEM))) (list ITEM))))
            ((not (cdr TYPELIST))
                (RETURN (list (ansay (list 'THE (CUTOFF TYPE))) (list ITEM))))  ;; THERE IS ONLY ONE OBJECT OF THIS TYPE (E.G. TABLE, BOX, HAND)
            (CUBE (SETQ NAME '(CUBE)))
            ((SETQ NAME (list (CUTOFF TYPE)))))                                 ;; E.G. !BLOCK BECOMES BLOCK.
        (and NAMEß
            (RETURN (list (ansay (list 'THE (car NAME) 'NAMED (caddar NAMEß))) (list ITEM)))) ;; E.G. THE BLOCK NAMED SUPERBLOCK.
        (disput COLORß)                                                         ;; IF WE HAVEN'T RETURNED YET, COLOR WILL BE NEEDED TO FULLY DESCRIBE THE OBJECT.
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
                (cons (concat '(say "THE") NAME)
                    (cons '(say "WHICH SUPPORTS")
                        (LISTNAMES nil 'INDEF x))))                             ;; IF IT SUPPORTS ANYTHING, NAME THEM.
            ((cons (concat '(say "THE") NAME)
                (cons '(say "WHICH IS TO THE RIGHT OF")
                    (COND ((SETQ x (THVAL2 nil '(THFIND ALL ($? x) (x (y ITEM))
                                (($G) (!AT ($? x) ?)) (($G) (!LOC !RIGHT ($? y) ($? x)) (THUSE TC-LOC))))) ;; MAKE SURE IT IS AN ITEM WITH A LOCATION.
                            (LISTNAMES nil 'INDEF x))
                        ('((say "NOTHING"))))))))
            (list ITEM)))))

(putprop! 'TC-NAMEOBJ 'THEOREM
    ;; PLANNER IS CALLED TO SEE HOW MANY OBJECTS FIT VARIOUS FORMS OF THE DESCRIPTION.  IT USES FAILURE TO LOOP THROUGH THEM,
    ;; SUCCESSIVELY FILTERING THEM THROUGH GOALS IN WHICH THEY ARE FORCED TO MATCH THE CHOSEN ITEM.  THIS VALUE IS THE ENTIRE
    ;; TYPE ASSERTION FOR SPECIAL CHECK TO CALL EQUIDIMENSIONAL BLOCKS "CUBE".  THE OR IS TO PREVENT PLANNER FROM FAILING THE
    ;; CHOSEN OBJECT.  IT IS SAVED SO THE SENTENCE NUMBER CAN BE PUT ON ITS PROPERTY LIST IF THE FACT IS USED IN THE DESCRIPTION.
    ;; IF THE ITEM HAS A NAME, NO MORE IS NEEDED.  FIND SOMETHING ELSE OF THE SAME TYPE.  NOTE THAT THIS WILL FIND THE ITEM ITSELF
    ;; ALONG WITH THE OTHERS AND THUS PUT IT ON THE LIST.  THIS KEEPS A LIST OF ALL THE OBJECTS WHICH MAKE IT THIS FAR.  NOTE
    ;; THAT SINCE IT IS SETQ INSTEAD OF THSETQ, BACKUP DOESN'T UNDO IT.  ANYTHING WHICH MAKES IT THIS FAR IS BOTH THE SAME TYPE
    ;; AND THE SAME COLOR.  WE DON'T WANT TO CHECK FOR EXACT EQUALITY OF SIZE, JUST WHETHER THEY WOULD BE CALLED THE SAME THING.
    ;; THE THFAIL SENDS IT BACK UP SEARCHING FOR MORE.
    '(THCONSE ((x ITEM) type color name #_size y z)
        (!NAMEOBJ)
        (($G) (!IS ($? x) ($? type)))
        (SETQ TYPEß *thvalue*)
        (or (SETQ CUBE (and (= ($? type) '!BLOCK) (!EQDIM ($? x))))
            true)
        (THCOND
            ((($G) (!NAME ($? x) ($? name)))
                (SETQ name *thvalue*))
            ((($G) (!IS ($? y) ($? type)))
                (or (not CUBE) (!EQDIM ($? y)))
                (SETQ TYPELIST (cons ($? y) TYPELIST))
                (($G) (!COLOR ($? x) ($? color)))
                (SETQ COLORß *thvalue*)
                (($G) (!COLOR ($? y) ($? color)))
                (SETQ COLORLIST (cons ($? y) COLORLIST))
                (SETQ SIZEß (namesize (size ($? x))))
                (= SIZEß (namesize (size ($? y))))
                (SETQ SIZELIST (cons ($? y) SIZELIST))
                (THFAIL)))))

(defn- namesize [x]
    ;; ACCEPTS EITHER SINGLE NUMBER OR LIST OF DIMENSIONS.
    (let [x (if (number? x) x (reduce + x))]
        (if (<= x 382) 'SMALL 'LARGE)))

(defn- namesugar [num oss]
    ;; GENERATES PHRASES LIKE "THREE OF THEM".
    ;; VAGUE IS FOR WORDS LIKE "ANYTHING", "SOMETHING", "NOTHING" TO AVOID SAYING "OF THEM" WHEN IT ISN'T APPROPRIATE.
    (let [vague (memq '!VAGUE (markers? oss))]
        (list (if (and vague (zero? num)) (list 'say "NOTHING") (list 'say (namenum num) (if vague (if (== num 1) "THING" "THINGS") "OF THEM"))))))

(defn- notell [] (global-err "THAT ISN'T THE KIND OF THING I CAN BE TOLD"))

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
            (= (car OLD) (getprop (car NEW) 'ROOT))
            (= (car NEW) (getprop (car OLD) 'ROOT))
            ;; IF THE NOUNS DON'T MATCH, JUST RETURN WHAT YOU GOT.
            ;; MATCHING INCLUDES PLURALS TO THEIR CORRESPONDING SINGULAR FORMS.
            (RETURN ITEM))
    LOOP (SETQ NEW (cdr NEW))
        (SETQ OLD (cdr OLD))
        (COND ((or (nil? NEW) (nil? OLD) (ISQ NEW NUM) (ISQ NEW DET) (not (= (car NEW) (car OLD))))
            (RETURN (cons
                (reverse (cons (if (ISQ (LAST (car ITEM)) NPL) 'ONES 'ONE) NEW))
                (cdr ITEM)))))
        (GO LOOP)))

(§ defn- ORDNAME [num]
    ;; NAME AN ORDINAL
    (COND ((== num 1) 'ONCE) ((== num 2) 'TWICE)
        ((READLIST (concat (EXPLODE (namenum num)) '(\space T I M E S))))))

(§ defn- PLNR-ANDORIFY [rss]
    ;; TURNS AN RSS INTO A COLLECTION OF PLANNER CODE FOR A COMMAND
    (COND ((and? rss)
            (PLNR-PROGIFY nil (doall (map #'PLNR-ANDORIFY (and? rss)))))
        ((or? rss)
            ;; (PLNR-ORIFY NIL (MAPCAR 'PLNR-ANDORIFY (OR? RSS)))
            (ert "SORRY, PLNR-ORIFY NOT WRITTEN"))
        ((PLNR-PROGIFY nil (doall (map #'PLNR-GOALIFY (relations? rss)))))))

(§ defn- PREPPUT [x]
    (COND ((and (rel? RSS) (set! *pt* (parsenode? (rel? RSS))) (ISQ (MOVE-PT 'U) PREPG))
            (cons (cons 'say (from (NB *pt*) (NB (MOVE-PT 'DLC)))) x))
        (:else x)))

(§ defn- PLURALIZE [item num]
    ;; CONVERTS A SINGULAR NOUNPHRASE OR "ONCE" STATEMENT INTO PLURAL.
    (COND ((> 2 num) item)
        (:else (COND ((memq 'A (car item))
                (cons (PLURALMAKE (SUBST (namenum num) 'A (car item))) (cdr item)))
            ((memq 'ONCE (car item))
                (cons (SUBST (ORDNAME num) 'ONCE (car item)) (cdr item)))
            ((bug "PLURALIZE -- FUNNY item"))))))

(§ defn- PLURALMAKE [phrase]
    ;; CONVERTS SINGULAR PHRASE TO PLURAL.
    (let [SING nil PLURAL nil]
        (or (ISQ (SETQ SING (LAST phrase)) NOUN)
            (bug "PLURALMAKE -- NO NOUN"))
        (SETQ PLURAL (MAKNAM (concat (EXPLODE (car SING)) '(S))))
        (or (getprop PLURAL 'FEATURES)
            (buildword PLURAL '(NOUN NPL) (SM SING) (car SING)))
        (SUBST PLURAL (car SING) phrase)))

(§ defn- THVAL-MULT [code]
    ;; DOES A THVAL WITH DIFFERENT VALUES OF WHO (I.E. NIL (EVERYTHING I KNOW),
    ;; 'HE (EVERYTHING HE KNOWS), AND THE PREVIOUS SENTENCE) USED TO TELL IF AN
    ;; ANSWER COULD HAVE BEEN GENERATED WITH HIS KNOWLEDGE TO SEE WHETHER HE REALLY
    ;; MEANT THIS INTERPRETATION.  RETURNS A LIST OF A PLAUSIBILITY AND THE RESULT
    ;; OF THE THVAL USING ALL THE KNOWLEDGE IN THE DATABASE.
    (let [ANS (THVAL2 nil code)]
        ;; THIS FEATURE IS ONLY RELEVANT IN DISCOURSE AND WHEN THERE ARE AMBIGUITIES.
        (or (and AMBIG discourse?) (RETURN (list 0 ANS)))
        ;; GIVE A VALUE OF 256 IF HE COULDN'T HAVE ANSWERED IT AT ALL.
        (or (= ANS (THVAL2 'HE code)) (RETURN (list 256 ANS)))
        ;; PLAUSIBILITY IS 0 IF HE COULD HAVE ANSWERED IT WITH RECENTLY MENTIONED INFORMATION.
        ;; 128 IF HE COULD ANSWER IT BUT NOT WITH RECENT INFO.
        (if (= ANS (THVAL2 (list (- *sentno* 2) (inc *sentno*)) code)) (list 0 ANS) (list 128 ANS))))

(defn- toplevel [event]
    ;; FINDS THE TOP LEVEL EVENT GOING ON AT THE TIME
    (let [why (getprop event 'WHY)] (if (= why 'COMMAND) event (recur why))))

(defn- findreduce [x y]
    (let [x (cdr x) y (dec y)] (if (zero? y) x (recur x y))))

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
        (putprop! oss 'PLNRCODE= PLNRCODE)
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
                    (SETQ ANS (findreduce ANS (- HAVE NEED)))))
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
                        *thalist*)
                    ANS))))
            (SETQ x (COND (x (cdr x)) ('NOMORE)))
            (GO GO)))
    CONJ (SETQ LOOP (or (and? RSS) (or? RSS)))
    UP  (COND ((getprop (car LOOP) 'REFER)
                (SETQ ANS (concat (getprop (car LOOP) 'REFER) ANS)))
            ((SETQ XX (FINDCHOOSE (car LOOP) x (concat ANS2 ANS)))
                (SETQ ANS (concat XX ANS))))
        (COND ((and ANS (or? oss)))
            ((SETQ LOOP (cdr LOOP)) (GO UP))
            (ANS)
            ((RETURN nil)))
    DONE (and (term? (variable? oss))
            (putprop! (variable? oss) 'BIND (reverse ANS)))
        (atomify (reverse ANS))))

(§ defn- MUNG [l mung]
    (SETQ mung (list 'quote mung))
    (and discourse? (SETQ l (caddr l)))
    (COND ((= (caar (cdddr l)) 'THAMONG)
            (RPLACD (cdar (cddddr l)) mung))
        ((RPLACD (cdddr l) (cons (list 'THAMONG (list 'THV (cadr (caddr l))) mung) (cddddr l))))))

(§ defn- NAMEVENT [event type]
    (let [*thalist* nil EV nil SUBJ nil OBJ1 nil OBJ2 nil]
        (or (SETQ EV (getprop (getprop event 'TYPE) 'NAMEVENT))
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
            (set! *thalist* (list
                (list 'EVENT event)
                (list 'SUBJ (COND ((number? (car EV)) nil) ('I)))
                (list 'OBJ1 nil) (list 'OBJ2 nil))))
            (ert "NAMEVENT THVAL"))
        (dorun (map #'(lambda [x]
                (and (cadr x) (SET (car x) (ert "OOPS! (NAMES (listify (cadr x)) 'EV)"))))
            (cdr *thalist*)))
        (SETQ ANSBACK2 (or ANSBACK true))
        (SETQ LASTANSEV event)
        (RETURN (concat
            (COND ((= type 'PAST) SUBJ) ((= type 'TO) '(TO)))
            (eval (cadr EV))))))

(defn- prtput [x y] (if (cdr y) (cons x y) (conj y x)))

(defn- vbfix [x pp]
    (condp = TENSE
        'ING (let [x (reverse (EXPLODE x))] (READLIST (reverse (concat '(G N I) (vbfix2 x pp) x))))
        'PAST (or (getprop x 'PAST) (let [x (reverse (EXPLODE x))] (READLIST (reverse (concat '(D E) (vbfix2 x pp) x)))))
        'INFINITIVE x
        (bug "VBFIX: WHAT SHOULD I DO WITH THIS TENSE?")))

(defn- vbfix2 [x pp] (when (and pp (memq (car x) CONSO) (memq (cadr x) VOWEL)) (list (car x))))

#_(ns shrdlu.cgram)

;; #################################################################
;;
;;  CGRAM > THE REGULAR GRAMMAR AFTER GOING THROUGH THE PRECOMPILER
;;
;; #################################################################

(§ defn- CLAUSE []
    (let [*fe* nil H nil *me* nil NB nil *c* nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil POSITION-OF-PRTMVB nil LOCATIONMARKER nil SUBJ-VB-BACKUP-TYPE1 nil POSITION-OF-PTW nil]
        (SETQ NN true)
        (SETQ CUT *end*)
        (set! *c* (buildnode (set! *fe* (reverse REST)) (SETQ NB (or (NB *re*) N)) N (SETQ H *re*) nil))
        (setr 'PARENT PARENT *c*)
ENTERING-CLAUSE
        (when LABELTRACE (passing 'ENTERING-CLAUSE))
        (setr 'TIME (BUILD TSSNODE= (gensym 'TSS)) *c*)
        (SETQ ßRESULT (cq 'SIMP *fe*))
        (COND (ßRESULT (GO SUBJ)))
        (SETQ ßRESULT (cq 'MAJOR *fe*))
        (if ßRESULT (GO INIT) (GO SEC))
INIT
        (when LABELTRACE (passing 'INIT))
        (SETQ LOCATIONMARKER N)
        (SETQ ßRESULT (and (nq 'BINDER) (parse 'CLAUSE 'BOUND 'INIT)))
        (when (nil? ßRESULT) (if (nil? NN) (GO FIXIT) (GO MAJOR)))
        (FQ BIND)
        (SETQ ßRESULT (CALLSM (SMBIND)))
        (COND (ßRESULT (GO INIT)))
FIXIT
        (when LABELTRACE (passing 'FIXIT))
        (set! *ptw* CUT)
        (SETQ ßRESULT (CUT (MOVE-PTW)))
        (if ßRESULT (GO INIT) (GO MAJOR))
MAJOR
        (when LABELTRACE (passing 'MAJOR))
        (CUT *end*)
        (COND ((= *punct* '?) (GO QUEST))
        ((or (cq 'IMPER *fe*) (= *punct* '!)) (GO IMPER)))
        (GO THEREINIT)
FDEC
        (when LABELTRACE (passing 'FDEC))
        (FQ DECLAR)
THEREINIT
        (when LABELTRACE (passing 'THEREINIT))
        (SETQ ßRESULT (and (nextword? N 'THERE) (parse nil 'THERE) (FQ DECLAR)))
        (COND (ßRESULT (COND ((nil? NN) (m! INIT) (GO FAIL)) (:else (GO THERE)))))
THER2
        (when LABELTRACE (passing 'THER2))
        (and (nq 'PREP) (parse 'PREPG 'INIT) (or (CALLSM (SMRELATE H)) (POP)))
        (and (nq 'ADV) (parse 'ADV 'TIMW) (or (CALLSM (SMADVERB)) (POP)))
        (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD) (or (CALLSM (SMRELATE H)) (POP)))
        (parse 'NG 'TIME)
        (SETQ ßRESULT (= LOCATIONMARKER N))
        (COND (ßRESULT (if (nil? NN) (GO INPOP) (GO CLAUSETYPE))) (:else (GO INIT)))
INPOP
        (when LABELTRACE (passing 'INPOP))
        (SETQ ßRESULT (MOVE-PT 'C 'DLC))
        (COND ((nil? ßRESULT) (m! INPOP) (GO FAIL)))
BICUT
        (when LABELTRACE (passing 'BICUT))
        (CUT-BACK-ONE)
        (GO INIT)
CLAUSETYPE
        (when LABELTRACE (passing 'CLAUSETYPE))
        (SETQ ßRESULT (cq 'DECLAR *fe*))
        (COND (ßRESULT (GO SUBJ)))
        (SETQ ßRESULT (and (nq 'VB) (nq 'INF) (parse 'VG 'IMPER) (FQ IMPER)))
        (COND (ßRESULT (GO VG1)))
        (FQ DECLAR)
        (SETQ ßRESULT (cq 'IMPER *fe*))
        (COND (ßRESULT (m! IMPER) (GO FAIL)))
SUBJ
        (when LABELTRACE (passing 'SUBJ))
        (CUT *end*)
SUBJ3
        (when LABELTRACE (passing 'SUBJ3))
        (SETQ ßRESULT (or (and (nextword? N 'TO) (parse 'CLAUSE 'RSNG 'TO 'SUBJ)) (and (parse 'CLAUSE 'RSNG 'ING 'SUBJ))))
        (COND (ßRESULT (if (nil? NN) (GO SUBJ1) (GO SUBREG))))
SUBJ4
        (when LABELTRACE (passing 'SUBJ4))
        (SETQ ßRESULT (parse 'NG 'SUBJ))
        (COND (ßRESULT (if (nil? NN) (GO SUBJ1) (GO SUBREG))))
        (COND ((cq 'REL-NOT-FOUND *fe*)
                (RQ REL-NOT-FOUND)
                (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)
                (GO VB))
            (SUBJ-VB-BACKUP-TYPE1 (SETQ SUBJ-VB-BACKUP-TYPE1 nil) (GO SUBJ11))
            ((and H (ISQ H TIME) (ISQ H NG)) (setr 'SUBJECT H *c*) (GO VB))
            ((MOVE-PT 'C 'U '(REL-NOT-FOUND))
                (setr 'SUBJECT (getr 'RELHEAD *pt*) *c*)
                (setr 'RELHEAD (getr 'RELHEAD *pt*) *c*)
                (remove-f-pt 'REL-NOT-FOUND *pt*)
                (GO VB))
            ((and (cq 'COMPONENT *fe*) NN) (FQ SUBJFORK) (GO VB))
            (H (POP) (GO SUBJ))
            ((GO FAIL)))
HEAD
        (when LABELTRACE (passing 'HEAD))
        (SETQ ßRESULT (or (MOVE-PTW 'N 'PW '(NOUN)) (MOVE-PTW 'N 'PW '(PRON))))
        (COND ((nil? ßRESULT) (m! HEAD) (GO FAIL)))
SUB2
        (when LABELTRACE (passing 'SUB2))
        (SETQ ßRESULT (POP))
        (when (nil? ßRESULT) (GO FAIL))
        (SETQ ßRESULT (CUT *ptw*))
        (if ßRESULT (GO INIT) (GO SUB2))
SUBJ1
        (when LABELTRACE (passing 'SUBJ1))
        (COND ((ISQ H QUOTED) (and (ISQ H LIST) (FQ LIST)) (FQ QUOTED) (SETQ H (H H)) (GO RETSM)))
        (and (cq 'REL-NOT-FOUND *fe*) (MOVE-PT 'H 'PV '(QAUX))
            (COND ((ISQ *pt* BE) (FQ INT AUXBE) (RQ REL-NOT-FOUND) (setr 'COMP (getr 'RELHEAD *c*) *c*) (setr 'SUBJECT H *c*) (setmvb *pt*) (GO ONT))
                ((ISQ *pt* HAVE) (FQ SUBQ) (RQ REL-NOT-FOUND) (setr 'SUBJECT (getr 'RELHEAD *c*) *c*) (GO VBL))))
SUBJ11
        (when LABELTRACE (passing 'SUBJ11))
        (SETQ ßRESULT (CUT-BACK-ONE))
        (COND (ßRESULT (GO SUBJ3)) (:else (m! SUBJ11) (GO FAIL)))
SUBREG
        (when LABELTRACE (passing 'SUBREG))
        (setr 'SUBJECT H *c*)
        (GO VB)
VB
        (when LABELTRACE (passing 'VB))
        (SETQ ßRESULT (parse 'ADJG 'ADV 'VBAD))
        (COND (ßRESULT (COND ((nil? NN) (m! VB-ADJG) (GO FAIL)) (:else (GO VB)))))
        (RQ VBLOK)
VBL
        (when LABELTRACE (passing 'VBL))
        (SETQ ßRESULT (parse 'VG))
        (COND (ßRESULT (GO VBREG)))
NOVERB
        (when LABELTRACE (passing 'NOVERB))
        (COND ((cq 'SUBJFORK *fe*) (FQ VBFORK) (GO FINDOBJ1))
            ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
            ((not (ISQ H SUBJ)) (GO FAIL))
            ((ISQ H CLAUSE) (SETQ SUBJ-VB-BACKUP-TYPE1 true) (POP) (GO SUBJ4))
            ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))
VB2
        (when LABELTRACE (passing 'VB2))
        (CUT-BACK-ONE)
        (GO SUBJ3)
VBREG
        (when LABELTRACE (passing 'VBREG))
        (setr 'VG H *c*)
VG1
        (when LABELTRACE (passing 'VG1))
        (CUT *end*)
        (SETQ ßRESULT (ISQ MVB BE))
        (COND (ßRESULT (COND ((nil? NN) (m! BE) (GO FAIL)) (:else (GO BE)))))
        (SETQ ßRESULT (ISQ MVB VPRT))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO CHECKPASV)) (:else (GO CHECKPASV)))))
        (SETQ ßRESULT (and (nq 'PRT) (parse 'PRT)))
        (when (nil? ßRESULT) (GO DPRT))
        (FQ PRT)
        (SETQ ßRESULT (setmvb (COMBINATION? (root (NB MVB)) (word (NB H)))))
        (if ßRESULT (GO CHECKPASV) (GO POPRT))
DPRT
        (when LABELTRACE (passing 'DPRT))
        (SETQ ßRESULT (ISQ H PASV))
        (COND (ßRESULT (GO CHECKPASV)))
        (SETQ ßRESULT (SETQ POSITION-OF-PRT (MOVE-PTW 'N 'NW '(PRT))))
        (when (nil? ßRESULT) (GO FINDOBJ1))
        (SETQ ßRESULT (setmvb (COMBINATION? (root (NB MVB)) (word POSITION-OF-PRT))))
        (when (nil? ßRESULT) (GO POPRT))
        (SETQ ßRESULT (ISQ MVB TRANS))
        (when (nil? ßRESULT) (GO FINDOBJ1))
        (CUT POSITION-OF-PRT)
        (SETQ ßRESULT (parse 'NG 'OBJ 'OBJ1))
        (if ßRESULT (GO POPRT) (GO FINDOBJ1))
        (CUT *end*)
        (setr 'OBJ1 H *c*)
        (parse 'PRT)
        (FQ PRT DPRT)
        (GO FINDOBJ2)
POPRT
        (when LABELTRACE (passing 'POPRT))
        (POPTO VG)
        (GO FINDOBJ1)
CHECKPASV
        (when LABELTRACE (passing 'CHECKPASV))
        (SETQ ßRESULT (and (ISQ H PASV) (FQ PASV) (setr 'OBJ1 (getr 'SUBJECT *c*) *c*)))
        (COND (ßRESULT (if (nil? NN) (GO FINDFAKE2) (GO FINDOBJ2))))
        (FQ ACTV)
        (GO FINDOBJ1)
BE
        (when LABELTRACE (passing 'BE))
        (FQ BE)
        (and (parse nil 'NOT) (FQ NEG))
        (parse 'ADV 'VBAD)
FINDOBJ1
        (when LABELTRACE (passing 'FINDOBJ1))
        (SETQ ßRESULT (or (CANPARSE 1 '(ADJG COMP) 'INT)
                (CANPARSE 1 '(NG COMP) 'INT)))
        (COND (ßRESULT (if (nil? NN) (GO ONT) (GO CHECKIT))))
        (SETQ ßRESULT (or (CANPARSE 1 '(PREPG COMP) 'INT)
                (CANPARSE 1 '(CLAUSE RSNG ING) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG REPORT) 'TRANS)
                (CANPARSE 1 '(CLAUSE RSNG TO) 'TRANS)
                (CANPARSE 1 '(PREPG LOC) 'ITRNSL)
                (CANPARSE 1 '(ADV PLACE) 'ITRNSL)))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (CANPARSE 1 '(NG) 'TRANS))
        (COND (ßRESULT (if (nil? NN) (GO FINDFAKE2) (GO FINDOBJ2))))
FINDFAKE1
        (when LABELTRACE (passing 'FINDFAKE1))
        (SETQ ßRESULT (MOVE-PT 'C 'U '(REL-NOT-FOUND)))
        (COND (ßRESULT (GO OBJ1REL)))
        (SETQ ßRESULT (and (CANTAKE 1 '(PREPG LOC) 'ITRNSL) (MOVE-PT 'C 'U '(QADJ)) (ISQ (getr 'QADJ *pt*) PLACE) (FQ ITRANSL)))
        (COND (ßRESULT (GO PUTLOBJ)))
        (SETQ ßRESULT (CANPARSE 1 nil 'ITRNS))
        (COND (ßRESULT (GO ONT)))
GOOF1
        (when LABELTRACE (passing 'GOOF1))
        (or *global-message* (bug "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)
OBJ1REL
        (when LABELTRACE (passing 'OBJ1REL))
        (setr 'OBJ1 (getr 'RELHEAD *pt*) *c*)
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (FQ OBJ1REL)
FINDOBJ2
        (when LABELTRACE (passing 'FINDOBJ2))
        (SETQ ßRESULT (CANPARSE 2 '(CLAUSE RSNG TO) 'TRANS2))
        (COND (ßRESULT (GO FIXSUBJECT)))
        (SETQ ßRESULT (or (CANPARSE 2 '(ADV PLACE) 'TRANSL) (CANPARSE 2 '(PREPG LOC) 'TRANSL)))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (or (CANPARSE 2 '(ADJG COMP) 'TRANSINT) (CANPARSE 2 '(NG COMP) 'TRANSINT)))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (CANPARSE 2 '(NG) 'TRANS2))
        (COND (ßRESULT (GO ONT)))
FINDFAKE2
        (when LABELTRACE (passing 'FINDFAKE2))
        (SETQ ßRESULT (and (ISQ MVB TRANS2) (MOVE-PT 'C 'U '(REL-NOT-FOUND))))
        (COND (ßRESULT (GO OBJ2REL)))
        (SETQ ßRESULT (and (CANTAKE 2 '(PREPG LOC) 'TRANSL) (MOVE-PT 'C 'U '(QADJ)) (ISQ (getr 'QADJ *pt*) PLACE) (FQ TRANSL)))
        (COND (ßRESULT (GO PUTLOBJ)))
OBJ2TO
        (when LABELTRACE (passing 'OBJ2TO))
        (parse 'ADV 'VBAD)
        (SETQ ßRESULT
            (COND ((and (nextword? N 'TO) (ISQ MVB TO2) (parse 'PREPG 'TO)) (setr 'OBJ2 (getr 'OBJ1 H) *c*) (FQ TRANS2TO TRANS2))
                ((and (cq 'PREPQ *fe*) (MOVE-PT 'H 'PV '(QUEST)) (= (word (MOVE-PTW 'FW)) 'TO) (RQ PREPQ) (FQ TRANS2TOQ TRANS2) (setr 'OBJ2 (getr 'OBJ1 *pt*) *c*)))))
        (COND (ßRESULT (GO ONT)))
        (SETQ ßRESULT (CANPARSE 2 nil 'TRANS))
        (if ßRESULT (GO ONT) (GO FAIL))
PUTLOBJ
        (when LABELTRACE (passing 'PUTLOBJ))
        (setr 'LOBJ *pt* *c*)
        (setr 'RELHEAD (getr 'QADJ *pt*) *pt*)
        (setr 'QADJ nil *pt*)
        (remove-f-pt 'QADJ *pt*)
        (GO ONT)
OBJ2REL
        (when LABELTRACE (passing 'OBJ2REL))
        (setr 'OBJ2 (getr 'RELHEAD *pt*) *c*)
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (FQ OBJ2REL)
        (GO ONT)
FIXSUBJECT
        (when LABELTRACE (passing 'FIXSUBJECT))
        (setr 'SUBJECT (getr 'OBJ1 *c*) H)
        (GO ONT)
CHECKIT
        (when LABELTRACE (passing 'CHECKIT))
        (SETQ ßRESULT (= (word (NB (getr 'SUBJECT *c*))) 'IT))
        (when (nil? ßRESULT) (GO ONT))
        (SETQ ßRESULT (or (and (nextword? N 'TO) (parse 'CLAUSE 'RSNG 'TO 'SUBJ)) (and (nq 'ING) (parse 'CLAUSE 'RSNG 'ING 'SUBJ)) (parse 'CLAUSE 'REPORT)))
        (when (nil? ßRESULT) (GO ONT))
        (FQ IT)
        (setr 'LOGICAL-SUBJECT H *c*)
        (GO ONT)
GOOF2
        (when LABELTRACE (passing 'GOOF2))
        (or *global-message* (bug "NEW TRANSITIVITY - SECOND OBJECT"))
        (GO FAIL)
ONT
        (when LABELTRACE (passing 'ONT))
        (SETQ ßRESULT (cq 'PASV *fe*))
        (COND (ßRESULT (GO PONT)))
ONT1
        (when LABELTRACE (passing 'ONT1))
        (SETQ ßRESULT (CALLSM (SMCL1)))
        (COND ((nil? ßRESULT) (m! SMCL1) (GO FAIL)))
        (SETQ ßRESULT (not (cq 'REL-NOT-FOUND *fe*)))
        (COND (ßRESULT (if (nil? NN) (GO RETSM) (GO TONT))))
        (SETQ ßRESULT (ISQ (getr 'HEAD (getr 'RELHEAD *c*)) TIM1))
        (when (nil? ßRESULT) (GO PREPSHORT))
TIMEQ
        (when LABELTRACE (passing 'TIMEQ))
        (RQ REL-NOT-FOUND)
        (FQ TIMEQ)
        (GO TONT)
PREPSHORT
        (when LABELTRACE (passing 'PREPSHORT))
        (SETQ ßRESULT (and (nq 'PREP) (parse 'PREPG)))
        (COND ((nil? ßRESULT) (m! ONT-SHORT-PREP) (GO FAIL)))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND ((nil? ßRESULT) (m! ONTß) (GO FAIL)))
        (SETQ ßRESULT (cq 'REL-NOT-FOUND *fe*))
        (COND (ßRESULT (COND ((nil? NN) (m! ONT-NOT-FOUND) (GO FAIL)) (:else (GO PREPSHORT)))) (:else (GO TONT)))
PONT
        (when LABELTRACE (passing 'PONT))
        (and (nextword? N 'BY) (parse 'PREPG 'AGENT) (FQ AGENT))
        (setr 'LOGICAL-SUBJECT (getr 'OBJ1 H) *c*)
        (GO ONT1)
TONT
        (when LABELTRACE (passing 'TONT))
        (SETQ ßRESULT (SETQ POSITION-OF-PTW N))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO RETSM)))))
NPASV
        (when LABELTRACE (passing 'NPASV))
        (SETQ ßRESULT (and (nq 'PREP) (parse 'PREPG) (CALLSM (SMRELATE H))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (nq 'TIMW) (parse 'ADV 'TIMW) (or (CALLSM (smtime)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (not (cq 'BE *fe*)) (parse 'ADJG 'ADV) (or (CALLSM (SMRELATE H)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (parse 'NG 'TIME) (or (CALLSM (smtime)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (nq 'PLACE) (parse 'ADV 'PLACE) (or (CALLSM (smplace)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (nq 'BINDER) (parse 'CLAUSE 'BOUND) (or (CALLSM (SMBIND)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (nextword? N 'TO) (parse 'CLAUSE 'TO 'ADJUNCT) (or (CALLSM (smtoadj)) (GO FAIL))))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (= N POSITION-OF-PTW))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO TONT)))))
        (SETQ ßRESULT (or (not (cq 'TOPLEVEL *fe*)) (nq 'SPECIAL)))
        (COND (ßRESULT (GO RETSM)))
        (ert "CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL")
        (GO FAIL)
THERE
        (when LABELTRACE (passing 'THERE))
        (FQ THERE)
        (CUT *end*)
        (SETQ ßRESULT (parse 'ADV 'TIMW))
        (COND ((and (nil? NN) ßRESULT) (m! THERE) (GO FAIL)))
        (SETQ ßRESULT (and (parse 'VG) (ISQ MVB BE)))
        (COND (ßRESULT (COND ((nil? NN) (m! THERE) (GO FAIL)) (:else (GO THEF))))
            (:else (GO NOTHE)))
THERQ
        (when LABELTRACE (passing 'THERQ))
        (SETQ ßRESULT (ISQ (MOVE-PT 'H 'PV '(QAUX)) BE))
        (COND (ßRESULT (GO THERQ2)))
        (SETQ ßRESULT (and (nq 'TIMW) (parse 'ADV 'TIMW)))
        (COND ((and (nil? NN) ßRESULT) (m! THEREQ) (GO FAIL)))
        (SETQ ßRESULT (and (parse 'VG) (ISQ MVB BE)))
        (COND (ßRESULT (GO THERQ2)))
        (RQ POLR2)
        (GO NOTHE)
THERQ2
        (when LABELTRACE (passing 'THERQ2))
        (FQ SUBJTQ)
        (FQ THERE)
        (SETQ ßRESULT (cq 'POLAR *fe*))
        (if ßRESULT (GO THEF) (GO ONT))
THEF
        (when LABELTRACE (passing 'THEF))
        (SETQ ßRESULT (and (nq 'ADV) (parse 'ADV 'TIMW)))
        (COND ((and (nil? NN) ßRESULT) (m! THEF) (GO FAIL)))
        (SETQ ßRESULT (parse 'NG 'SUBJ 'SUBJT))
        (when (nil? ßRESULT) (GO THERREL))
        (FQ THERE)
        (setr 'SUBJECT H *c*)
        (GO ONT)
THERREL
        (when LABELTRACE (passing 'THERREL))
        (SETQ ßRESULT (MOVE-PT 'C 'U '(REL-NOT-FOUND)))
        (when (nil? ßRESULT) (GO NOTHE))
        (setr 'SUBJECT (getr 'RELHEAD *pt*) *c*)
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (GO ONT)
NOTHE
        (when LABELTRACE (passing 'NOTHE))
        (RQ THERE)
        (POP THERE)
        (and (nq 'ADV) (parse 'ADV 'PLACE))
        (GO THER2)
IMPER
        (when LABELTRACE (passing 'IMPER))
        (SETQ ßRESULT (parse 'NG 'TIME))
        (COND ((and (nil? NN) ßRESULT) (GO IMPOP)))
        (SETQ ßRESULT (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD)))
        (COND ((and (nil? NN) ßRESULT) (GO IMPOP)))
        (SETQ ßRESULT (and (nq 'ADV) (parse 'ADV 'TIMW)))
        (COND ((and (nil? NN) ßRESULT) (GO IMPOP)))
IMPE
        (when LABELTRACE (passing 'IMPE))
        (SETQ ßRESULT (parse 'VG 'IMPER))
        (when (nil? ßRESULT) (GO IMPOP))
        (FQ IMPER)
        (GO VG1)
IMPOP
        (when LABELTRACE (passing 'IMPOP))
        (SETQ ßRESULT (POP nil))
        (COND (ßRESULT (GO IMPE)) (:else (m! IMPOP) (GO FAIL)))
QUEST
        (when LABELTRACE (passing 'QUEST))
        (FQ QUEST)
        (SETQ ßRESULT (nq 'PREP))
        (when (nil? ßRESULT) (GO NGQUES))
        (SETQ ßRESULT (parse 'PREPG))
        (COND ((nil? ßRESULT)
            (COND ((nil? NN) (m! PREPQ-INCOMPLETE) (GO FAIL)) (:else (GO NGQUES)))))
        (SETQ ßRESULT (ISQ H QUEST))
        (when (nil? ßRESULT) (GO QUEST))
        (setr 'QADJ H *c*)
        (GO POLAR)
NGQUES
        (when LABELTRACE (passing 'NGQUES))
        (SETQ ßRESULT (parse 'NG 'QUEST))
        (COND (ßRESULT (GO NGQST)))
        (SETQ ßRESULT
        (or (and (nextword? N 'HOW) (parse 'ADJG 'QUEST) (setr 'RELHEAD H *c*)) (and (nq 'QADJ) (parse 'QADJ) (FQ QADJ) (setr 'QADJ H *c*))))
        (if ßRESULT (GO POLAR) (GO POLAR))
        (FQ SHORTQUES)
        (CALLSM (SMADJQSHORT))
ADJQS
        (when LABELTRACE (passing 'ADJQS))
        (GO RETURN)
NGQST
        (when LABELTRACE (passing 'NGQST))
        (setr 'RELHEAD H *c*)
NGQST2
        (when LABELTRACE (passing 'NGQST2))
        (CUT *end*)
        (setr 'SUBJECT H *c*)
        (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD))
        (COND ((parse 'VG 'NAUX) (FQ SUBJQ) (GO VG1))
            ((nq 'VB) (FQ REL-NOT-FOUND) (GO POLAR))
            (:else (MOVE-PTW 'N 'PW) (POP NG QUEST) (CUT *ptw*) (GO NGQUES)))
QUEST2
        (when LABELTRACE (passing 'QUEST2))
        (SETQ ßRESULT (and (nextword? N 'THERE) (parse nil 'THERE)))
        (if ßRESULT (GO THERQ) (GO SUBF))
SUBF
        (when LABELTRACE (passing 'SUBF))
        (SETQ ßRESULT (parse 'NG 'SUBJ))
        (COND (ßRESULT (if (nil? NN) (GO SUBJ1) (GO SUBREG))))
        (RQ REL-NOT-FOUND)
        (GO BE)
POLAR
        (when LABELTRACE (passing 'POLAR))
        (SETQ ßRESULT (and (nq 'VB) (parse 'VB 'AUX '(QAUX)) (setr 'QAUX H *c*) (CALLSM (SMVAUX)) (setmvb H)))
        (when (nil? ßRESULT) (GO QCHOP))
        (or (cq 'QADJ *fe*) (getr 'RELHEAD *c*) (FQ POLAR))
        (FQ POLR2)
        (GO QUEST2)
QCHOP
        (when LABELTRACE (passing 'QCHOP))
        (ert "CLAUSE: QCHOP")
        (SETQ ßRESULT (POPTO CLAUSE BOUND))
        (COND (ßRESULT (GO BICUT)) (:else (m! QCHOP) (GO FAIL)))
SEC
        (when LABELTRACE (passing 'SEC))
        (COND ((cq 'BOUND *fe*) (GO BOUND))
            ((cq 'TO *fe*) (GO TO))
            ((cq 'RSQ *fe*) (GO RSQ))
            ((cq 'REPORT *fe*) (GO REPORT))
            ((cq 'ING *fe*) (GO ING))
            (:else (mq! 'RSNG-TYPE) (GO FAIL)))
BOUND
        (when LABELTRACE (passing 'BOUND))
        (SETQ ßRESULT (parse 'BINDER))
        (COND ((nil? ßRESULT)
            (COND ((nil? NN) (m! BINDER) (GO FAIL)) (:else (m! BOUND) (GO FAIL)))))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RSQ
        (when LABELTRACE (passing 'RSQ))
        (setr 'RELHEAD (MOVE-PT 'C 'U '(NG)) *c*)
        (SETQ ßRESULT (cq 'PREPREL *fe*))
        (when (nil? ßRESULT) (GO RSQ2))
        (parse 'PREPG 'PRONREL)
        (setr 'QADJ H *c*)
        (GO REPORT)
RSQ2
        (when LABELTRACE (passing 'RSQ2))
        (COND ((parse 'VG 'EN 'PASV)
                (or (ISQ MVB TRANS) (GO FAIL))
                (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)
                (GO VG1))
            ((parse 'VG 'ING) (setr 'SUBJECT (getr 'RELHEAD *c*) *c*) (GO VG1))
            ((nq 'PRONREL) (parse 'NG 'RELWD) (GO REL))
            ((cq 'COMPONENT *fe*)
                (setr 'RELHEAD (getr 'RELHEAD (MOVE-PT 'C 'PC)) *c*)
                (GO REL))
            ((parse 'NG 'SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
            (:else (GO FAIL)))
REL
        (when LABELTRACE (passing 'REL))
        (setr 'SUBJECT (getr 'RELHEAD *c*) *c*)
        (SETQ ßRESULT (parse 'VG))
        (COND (ßRESULT (GO VG1)))
        (FQ REL-NOT-FOUND)
        (GO SUBJ)
TO
        (when LABELTRACE (passing 'TO))
        (SETQ ßRESULT (and (cq 'COMPONENT *fe*) (parse 'VG 'TO 'TODEL)))
        (COND (ßRESULT (GO VG1)))
        (SETQ ßRESULT (nextword? N 'FOR))
        (when (nil? ßRESULT) (GO TO1))
        (parse nil 'FOR)
        (FQ FOR)
        (parse 'NG 'SUBJ 'TOSUBJ)
        (setr 'SUBJECT H *c*)
TO1
        (when LABELTRACE (passing 'TO1))
        (SETQ ßRESULT (parse 'VG 'TO))
        (COND (ßRESULT (GO VG1)) (:else (m! TO) (GO FAIL)))
ING
        (when LABELTRACE (passing 'ING))
        (SETQ ßRESULT (MOVE-PTW 'N 'NW '(ING)))
        (when (nil? ßRESULT) (GO FAIL))
        (SETQ ßRESULT (or (nq 'ING) (cq 'OBJ2 *fe*) (and (parse 'NG 'SUBJ 'INGSUBJ) (setr 'SUBJECT H *c*) (FQ SUBING) (RQ ING))))
        (COND ((and (nil? NN) ßRESULT) (m! ING) (GO FAIL)))
        (SETQ ßRESULT (parse 'VG 'ING))
        (COND (ßRESULT (GO VG1)) (:else (m! ING) (GO FAIL)))
REPORT
        (when LABELTRACE (passing 'REPORT))
        (and (nextword? N 'THAT) (parse nil 'THAT) (FQ THAT))
        (SETQ LOCATIONMARKER N)
        (GO FDEC)
RETSM
        (when LABELTRACE (passing 'RETSM))
        (or (CALLSM (SMCL2)) (GO FAIL))
        (GO RETURN)
FAIL
        (set! *mes* *me*)
        (SETQ N (or (N *re*) NB))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild (reverse *fe*) NB N H SM *c*)))

(§ defn- NG []
    (let [*fe* nil H nil *me* nil NB nil *c* nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil]
        (SETQ NN true)
        (SETQ CUT *end*)
        (set! *c* (buildnode (set! *fe* (reverse REST)) (SETQ NB (or (NB *re*) N)) N (SETQ H *re*) nil))
        (setr 'PARENT PARENT *c*)
ENTERING-NG
        (when LABELTRACE (passing 'ENTERING-NG))
NGSTART
        (when LABELTRACE (passing 'NGSTART))
        (COND ((cq 'RELWD *fe*) (GO RELWD))
            ((cq 'QUEST *fe*) (GO QUEST))
            ((or (nq 'QDET) (nq 'QPRON)) (FQ QUEST) (GO QUEST))
            ((cq 'TIME *fe*) (GO TIME))
            ((nq 'PROPN) (GO PROPN))
            ((nq 'TPRON) (GO TPRON))
            ((nq 'EVERPRON) (GO EVERPRON))
            ((nq 'PRON) (GO PRON)))
LOOK
        (when LABELTRACE (passing 'LOOK))
        (COND ((nq 'DET) (GO DET))
            ((nq 'NUM) (GO NUM))
            ((or (nq 'ING) (nq 'EN) (nq 'ADJ)) (GO ADJ))
            ((nq 'CLASF) (GO CLASF))
            ((nq 'NUMD) (GO NUMD))
            ((nextword? N 'AT) (GO AT))
            ((nextword? N 'AS) (GO AS))
            ((nq 'NOUN) (GO NOUN))
            ((nq 'TIMORD) (GO TIMORD))
            ((and (cq 'COMPONENT *fe*) (ISQ (MOVE-PT 'PC) QUEST)) (GO QUEST))
            ((mq! 'START) (GO FAIL)))
START
        (when LABELTRACE (passing 'START))
PROPN
        (when LABELTRACE (passing 'PROPN))
        (parse 'PROPN)
        (FQ DEF PROPNG)
        (SETQ ßRESULT (ISQ H POSS))
        (COND (ßRESULT (GO PROPS)))
        (SETQ ßRESULT (and NN (nq 'PROPN)))
        (COND (ßRESULT (GO PROPN)))
PROPS
        (when LABELTRACE (passing 'PROPS))
        (or (CALLSM (SMPROP)) (GO FAIL))
        (SETQ ßRESULT (ISQ H POSS))
        (if ßRESULT (GO POSS) (GO PRAG))
PRON
        (when LABELTRACE (passing 'PRON))
        (SETQ ßRESULT (parse 'PRON 'POSS))
        (COND (ßRESULT (if (nil? NN) (GO RED2) (GO POSS))))
PRON2
        (when LABELTRACE (passing 'PRON2))
        (SETQ ßRESULT (cq 'NPRON *fe*))
        (COND (ßRESULT (m! NPRON) (GO FAIL)))
        (SETQ ßRESULT (or (and (cq 'SUBJ *fe*) (parse 'PRON 'SUBJ)) (and (or (cq 'OBJ *fe*) (cq 'TOSUBJ *fe*) (cq 'INGSUBJ *fe*)) (parse 'PRON 'OBJ)) (cq 'INGSUBJ *fe*)))
        (COND ((nil? ßRESULT) (m! PRON) (GO FAIL)))
        (FQ PRONG DEF)
PRON3
        (when LABELTRACE (passing 'PRON3))
        (SETQ ßRESULT (CALLSM (SMPRON H)))
        (when (nil? ßRESULT) (GO FAIL))
PRAG
        (when LABELTRACE (passing 'PRAG))
        (setr 'HEAD H *c*)
        (MOVE-PT 'H)
        (TRNSF NS NPL NFS NEG)
        (GO RETURN)
TPRON
        (when LABELTRACE (passing 'TPRON))
        (parse 'TPRON)
        (FQ TPRON)
        (MOVE-PT 'H)
        (TRNSF NS NPL ANY NEG)
        (setr 'HEAD *c* H)
        (and NN (nq 'ADJ) (parse 'ADJ))
        (GO SMNG)
EVERPRON
        (when LABELTRACE (passing 'EVERPRON))
        (SETQ ßRESULT (and (parse 'PRON 'EVERPRON) (CALLSM (SMPRON H))))
        (when (nil? ßRESULT) (GO FAIL))
        (SETQ ßRESULT (and (parse 'CLAUSE 'RSQ 'NOREL) (CALLSM (SMRELATE H))))
        (if ßRESULT (GO RETSM) (GO FAIL))
AS
        (when LABELTRACE (passing 'AS))
        (SETQ ßRESULT (and (parse nil 'AS) (parse 'NUMD 'NUMDAS) NN (parse nil 'AS)))
        (COND (ßRESULT (COND ((nil? NN) (m! AS) (GO FAIL)) (:else (GO NUMD2)))) (:else (m! AS) (GO FAIL)))
AT
        (when LABELTRACE (passing 'AT))
        (SETQ ßRESULT (and (parse nil 'AT) (parse 'NUMD 'NUMDAT)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (m! AT) (GO FAIL)) (:else (m! AT) (GO FAIL)))))
NUMD2
        (when LABELTRACE (passing 'NUMD2))
        (SETQ ßRESULT (and (parse 'NUM) (FQ NUM NUMD)))
        (COND (ßRESULT (if (nil? NN) (GO INCOM) (GO DET1))) (:else (m! NUMD2) (GO FAIL)))
NUMD
        (when LABELTRACE (passing 'NUMD))
        (SETQ ßRESULT (parse 'NUMD 'NUMDAN))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO ND3)))))
        (SETQ ßRESULT (parse nil 'THAN))
        (COND (ßRESULT (if (nil? NN) (GO POPCOM) (GO NUMD2))) (:else (GO INCOM)))
ND3
        (when LABELTRACE (passing 'ND3))
        (SETQ ßRESULT (parse 'NUMD 'NUMDALONE))
        (COND (ßRESULT (COND ((nil? NN) (m! NUMD) (GO FAIL)) (:else (GO NUMD2)))) (:else (m! NUMD) (GO FAIL)))
TIME
        (when LABELTRACE (passing 'TIME))
        (SETQ ßRESULT (and (nq 'TIME) (parse 'NOUN 'TIME)))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (MOVE-PTW 'N 'NW '(TIM1)))
        (COND (ßRESULT (GO LOOK)) (:else (m! TIME) (GO FAIL)))
TIMORD
        (when LABELTRACE (passing 'TIMORD))
        (SETQ ßRESULT (parse 'ORD 'TIMORD))
        (when (nil? ßRESULT) (GO FAIL))
        (SETQ ßRESULT (and (parse 'NOUN 'TIM1) (FQ DET DEF) (CALLSM (SMNGTIME))))
        (if ßRESULT (GO RETURN) (GO FAIL))
DET
        (when LABELTRACE (passing 'DET))
        (parse 'DET)
        (FQ DET)
        (MOVE-PT 'H)
        (SETQ ßRESULT (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR))
        (COND (ßRESULT (if (nil? NN) (GO INCOM) (GO IND))) (:else (m! bug) (GO FAIL)))
IND
        (when LABELTRACE (passing 'IND))
        (SETQ ßRESULT (and (= (word (NB H)) 'ALL) (= (word N) 'THE) (parse 'DET) (FQ DEF)))
        (COND (ßRESULT (COND ((nil? NN) (m! THE) (GO FAIL)) (:else (GO NUM)))))
        (SETQ ßRESULT (and (ISQ H QNTFR) (FQ QNTFR)))
        (COND (ßRESULT (GO QNUM)))
ORD
        (when LABELTRACE (passing 'ORD))
        (SETQ ßRESULT (and (parse 'ORD) (FQ ORD)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO NUM)))))
        (SETQ ßRESULT (and (nextword? N 'OF) (ISQ (MOVE-PTW 'N 'NW) MONTH) (parse nil 'OF) (parse 'NOUN 'MONTH) (FQ DATE)))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (cq 'DEF *fe*))
        (when (nil? ßRESULT) (GO ADJ))
NUM
        (when LABELTRACE (passing 'NUM))
        (SETQ ßRESULT (parse 'NUM))
        (when (nil? ßRESULT) (GO ADJ))
        (FQ NUM)
        (SETQ ßRESULT (cq 'DET *fe*))
        (when (nil? ßRESULT) (GO DET1))
        (SETQ ßRESULT (COND ((and (ISQ H NS) (cq 'NS *fe*)) (RQ NPL PART)) ((cq 'NPL *fe*) (RQ NS PART))))
        (COND (ßRESULT (if (nil? NN) (GO INCOM) (GO ADJ))) (:else (m! NUM) (GO FAIL)))
DET1
        (when LABELTRACE (passing 'DET1))
        (if (ISQ H NS) (FQ NS) (FQ NPL))
        (or NN (and (FQ NUMBER) (GO INCOM)))
NUMBER
        (when LABELTRACE (passing 'NUMBER))
        (FQ DET)
        (SETQ ßRESULT (nq 'OF))
        (if ßRESULT (GO OF) (GO ADJ))
QNUM
        (when LABELTRACE (passing 'QNUM))
        (SETQ ßRESULT (ISQ H NONUM))
        (COND (ßRESULT (GO OF)))
        (SETQ ßRESULT (and (parse 'NUM) (FQ NUM)))
        (when (nil? ßRESULT) (GO OF))
        (SETQ ßRESULT (COND ((== (SM H) 1) (and (cq 'NS *fe*) (RQ NPL))) ((cq 'NPL *fe*) (RQ NS))))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (m! NUMD) (GO FAIL)))))
        (SETQ ßRESULT (= (word (NB H)) 'NO))
        (COND (ßRESULT (GO ADJ)))
OF
        (when LABELTRACE (passing 'OF))
        (SETQ ßRESULT (and (nq 'OF) (parse 'PREPG 'OF)))
        (if ßRESULT (GO SMOF) (GO NONE))
SMOF
        (when LABELTRACE (passing 'SMOF))
        (FQ OF)
        (SETQ ßRESULT (or (CALLSM (SMNGOF)) (not (POP))))
        (if ßRESULT (GO RETSM) (GO INCOM))
NONE
        (when LABELTRACE (passing 'NONE))
        (SETQ ßRESULT (= (word (NB H)) 'NONE))
        (if ßRESULT (GO INCOM) (GO ADJ))
ADJ
        (when LABELTRACE (passing 'ADJ))
        (SETQ ßRESULT (parse 'ADJ))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO EPR)))))
        (and (ISQ H COMPAR) (FQ COMPARATIVE-MODIFIER) (setr 'COMPARATIVE-MODIFIER H *c*))
        (GO ADJ)
EPR
        (when LABELTRACE (passing 'EPR))
        (SETQ ßRESULT (or (ISQ H SUP) (ISQ H COMPAR)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO CLASF)))))
        (FQ ADJ)
        (and (nextword? N 'OF)
            (parse 'PREPG 'OF)
            (or (CALLSM (SMNGOF)) (GO FAIL))
            (FQ OF)
            (GO RETSM))
CLASF
        (when LABELTRACE (passing 'CLASF))
        (SETQ ßRESULT (or (parse 'VB 'ING '(CLASF)) (parse 'VB 'EN '(CLASF)) (parse 'CLASF)))
        (COND (ßRESULT (if (nil? NN) (GO REDUC) (GO CLASF))))
NOUN
        (when LABELTRACE (passing 'NOUN))
        (SETQ ßRESULT (parse 'NOUN))
        (when (nil? ßRESULT) (GO RED2))
        (SETQ ßRESULT (and (cq 'TIME *fe*) (not (ISQ H TIM1))))
        (COND (ßRESULT (GO RED1)))
        (SETQ T1 *fe*)
        (COND ((and (ISQ H MASS) (or (cq 'PART *fe*) (not (cq 'DET *fe*)))) (FQ MASS)))
        (COND ((not (ISQ H NPL)) (RQ NPL PART)))
        (COND ((not (ISQ H NS)) (RQ NS)))
        (COND ((and (not (cq 'DET *fe*)) (not (cq 'NUMD *fe*))) (MOVE-PT 'H) (TRNSF NPL MASS)))
        (SETQ ßRESULT (MEET *fe* '(NS NPL PART MASS)))
        (when (nil? ßRESULT) (GO RED0))
        (SETQ ßRESULT (nextword? N 'THAN))
        (when (nil? ßRESULT) (GO SMNG))
        (FQ THAN)
SMNG
        (when LABELTRACE (passing 'SMNG))
        (setr 'HEAD H *c*)
        (SETQ ßRESULT (and (cq 'OBOFJ *fe*) (not (cq 'DEF *fe*))))
        (COND (ßRESULT (GO FAIL)))
        (or (CALLSM (SMNG1)) (GO FAIL))
        (SETQ ßRESULT (not (ISQ H POSS)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (GO POSS)))))
        (SETQ ßRESULT (and (cq 'THAN *fe*) (parse 'ADJG)))
        (COND ((nil? ßRESULT) (GO RSQ-TO)))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (if ßRESULT (GO RETSM) (GO FAIL))
RSQ-TO
        (when LABELTRACE (passing 'RSQ-TO))
        (SETQ ßRESULT (and (nextword? N 'TO) (MEET *fe* '(COMP SUBJ)) (parse 'CLAUSE 'RSQ 'TO) (or (CALLSM (SMRELATE H)) (GO POPRET))))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (and (or (nextword? N 'AS) (nq 'COMPAR)) (parse 'ADJG 'THANNEED)))
        (when (nil? ßRESULT) (GO PREPNG))
        (and (nil? N) (cq 'SUBJ *fe*) (ISQ (MOVE-PT 'C 'PV) AUX) (ISQ *pt* BE) (GO POPRET))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND (ßRESULT (if (nil? NN) (GO RETSM) (GO RSQ-TO))) (:else (GO POPRET)))
PREPNG
        (when LABELTRACE (passing 'PREPNG))
        (SETQ ßRESULT (and (nq 'PREP) (not (or (and (nq 'PLACE) (cq 'NOLOC *fe*)) (and (cq 'OBJ1 *fe*) (ISQ MVB TRANSL) (not (ISQ (MOVE-PT 'C 'U) QUEST))))) (parse 'PREPG 'Q)))
        (when (nil? ßRESULT) (GO DISGRSQ))
        (and (nil? N)
            (cq 'SUBJ *fe*)
            (ISQ (MOVE-PT 'C 'PV) AUX)
            (ISQ *pt* BE)
            (not (ISQ (MOVE-PT 'U) NGQ))
            (GO POPRET))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (COND (ßRESULT (if (nil? NN) (GO RETSM) (GO RSQ-TO))) (:else (GO POPRET)))
DISGRSQ
        (when LABELTRACE (passing 'DISGRSQ))
        (SETQ ßRESULT (= (car *mes*) 'PREP-WHICH))
        (when (nil? ßRESULT) (GO RSQ))
        (set! *mes* (cdr *mes*))
        (SETQ ßRESULT (parse 'CLAUSE 'RSQ 'PREPREL))
        (COND (ßRESULT (if (nil? NN) (GO RETSM) (GO PREPNG))) (:else (m! RSQ-PREPREL) (GO FAIL)))
RSQ
        (when LABELTRACE (passing 'RSQ))
        (SETQ ßRESULT (and (ISQ (MOVE-PT 'C 'U) POLR2) (cq 'SUBJ *fe*) (nq 'VB) (not (cq 'SUBJT *fe*)) (not (ISQ *pt* QADJ))))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (parse 'CLAUSE 'RSQ))
        (when (nil? ßRESULT) (GO RETSM))
        (SETQ ßRESULT (CALLSM (SMRELATE H)))
        (if ßRESULT (GO RETSM) (GO POPRET))
RED0
        (when LABELTRACE (passing 'RED0))
        (set! *fe* T1)
RED1
        (when LABELTRACE (passing 'RED1))
        (POP)
RED2
        (when LABELTRACE (passing 'RED2))
        (COND ((nil? H) (mq! 'NO) (GO FAIL))
            ((ISQ H NUMBER) (GO INCOM))
            ((and (ISQ H POSS) (or (ISQ H PRON) (and (MOVE-PT 'H 'DLC) (ISQ *pt* PRON)))) (POP) (GO PRON2))
            ((and (nil? (cdr H)) (cq 'DEFPOSS *fe*)) (GO POSSDEF))
            ((and (cq 'QUEST *fe*) (nil? (cdr H))) (GO QDETCHECK))
            ((ISQ H ADJ) (GO EPR))
            ((not (ISQ H CLASF)) (GO INCOM)))
REDUC
        (when LABELTRACE (passing 'REDUC))
        (POP)
        (SETQ ßRESULT (and (nil? H) (nq 'PROPN)))
        (if ßRESULT (GO PROPN) (GO NOUN))
POPCOM
        (when LABELTRACE (passing 'POPCOM))
        (POP)
INCOM
        (when LABELTRACE (passing 'INCOM))
        (FQ INCOM)
        (SETQ ßRESULT (and (ISQ H DET) (ISQ H INCOM) (CALLSM (SMINCOM))))
        (COND (ßRESULT (GO RETURN)))
        (SETQ ßRESULT (and (nil? CUT) (cq 'NUM *fe*)))
        (COND (ßRESULT (GO SMNG)))
QDETCHECK
        (when LABELTRACE (passing 'QDETCHECK))
        (COND ((and (ISQ H QDET) (ISQ (NB H) QPRON)) (POP) (GO QPRON))
            ((and (ISQ H QDET) (ISQ (NB H) EVERPRON)) (POP) (GO EVERPRON)))
        (GO FAIL)
POSS
        (when LABELTRACE (passing 'POSS))
        (or (CALLSM (SMNG2)) (GO FAIL))
POSS2
        (when LABELTRACE (passing 'POSS2))
        (SETQ ßRESULT (cq 'INGSUBJ *fe*))
        (COND (ßRESULT (GO RETSM)))
        (SETQ H (buildnode (reverse (cons 'POSS (SETDIF *fe* '(COMPONENT)))) NB N H SM))
        (set! *backref* (concat H (cdr *backref*)))
        (SETQ ßRESULT (setr 'FEATURES (set! *fe* (concat '(POSES DET DEF NS NPL) (reverse REST))) *c*))
        (COND ((nil? ßRESULT) (m! bug) (GO FAIL)))
        (SETQ ßRESULT (or (not NN) (ISQ H DEFPOSS)))
        (when (nil? ßRESULT) (GO ORD))
POSSDEF
        (when LABELTRACE (passing 'POSSDEF))
        (RQ POSES DET DEF)
        (FQ POSSDEF NS NPL)
QUEST
        (when LABELTRACE (passing 'QUEST))
        (SETQ ßRESULT (parse nil 'HOW))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO FAIL)) (:else (GO QDET)))))
        (SETQ ßRESULT (parse nil 'MANY))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO INCOM)) (:else (GO FAIL)))))
        (FQ DET NPL INDEF HOWMANY)
        (GO OF)
QDET
        (when LABELTRACE (passing 'QDET))
        (SETQ ßRESULT (and (parse 'DET 'QDET) (FQ DET NPL QDET NS)))
        (COND (ßRESULT (if (nil? NN) (GO INCOM) (GO QNUM))))
QPRON
        (when LABELTRACE (passing 'QPRON))
        (SETQ ßRESULT (parse 'PRON 'QPRON))
        (if ßRESULT (GO PRON3) (GO FAIL))
RELWD
        (when LABELTRACE (passing 'RELWD))
        (SETQ ßRESULT (and (parse 'PRONREL) (CALLSM (SMSET (SM (MOVE-PT 'C 'U 'U '(NG)))))))
        (COND (ßRESULT (GO RETURN)))
POPRET
        (when LABELTRACE (passing 'POPRET))
        (POP)
RETSM
        (when LABELTRACE (passing 'RETSM))
        (or (CALLSM (SMNG2)) (GO TRYA))
        (GO RETURN)
TRYA
        (when LABELTRACE (passing 'TRYA))
        (SETQ ßRESULT (ISQ H NOUN))
        (COND ((nil? ßRESULT) (m! TRYA) (GO FAIL)))
        (POP)
        (CUT N)
UP
        (when LABELTRACE (passing 'UP))
        (SETQ ßRESULT (POP))
        (COND (ßRESULT (GO UP)))
        (set! *fe* (reverse REST))
        (SMSET nil)
        (GO NGSTART)
FAIL
        (set! *mes* *me*)
        (SETQ N (or (N *re*) NB))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild (reverse *fe*) NB N H SM *c*)))

(§ defn- VG []
    (let [*fe* nil H nil *me* nil NB nil *c* nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil TENSE nil]
        (SETQ NN true)
        (SETQ CUT *end*)
        (set! *c* (buildnode (set! *fe* (reverse REST)) (SETQ NB (or (NB *re*) N)) N (SETQ H *re*) nil))
        (setr 'PARENT PARENT *c*)
ENTERING-VG
        (when LABELTRACE (passing 'ENTERING-VG))
        (COND ((cq 'TO *fe*) (GO TO))
            ((cq 'EN *fe*) (GO EN))
            ((cq 'ING *fe*) (GO ING))
            ((cq 'IMPER *fe*) (GO IMPER))
            ((ISQ (MOVE-PT 'C 'U) POLR2) (GO POLR2)))
NEW
        (when LABELTRACE (passing 'NEW))
        (COND ((not (nq 'VB)) (mq! 'VB) (GO FAIL))
            ((and (nq 'DO) (parse 'VB 'AUX 'DO)) (GO DO))
            ((and (nq 'MODAL) (parse 'VB 'AUX 'MODAL)) (GO MODAL))
            ((and (nq 'WILL) (parse 'VB 'AUX 'WILL)) (GO WILL))
            ((and (nq 'BE) (parse 'VB 'AUX 'BE)) (GO BE))
            ((and (nq 'HAVE) (parse 'VB 'AUX 'HAVE)) (GO HAVE))
            ((not (parse 'VB '(MVB))) (mq! 'VB) (GO FAIL)))
SIMPLE
        (when LABELTRACE (passing 'SIMPLE))
        (MOVE-PT 'C 'DLC)
        (TRNSF VPL INF V3PS)
        (SETQ TENSE (COND ((and (ISQ *pt* PRESENT) (ISQ *pt* PAST)) '(PAST-PRESENT)) ((ISQ *pt* PAST) '(PAST)) (:else '(PRESENT))))
        (GO REV)
TO
        (when LABELTRACE (passing 'TO))
        (FQ NAGR)
        (SETQ ßRESULT (and (parse nil 'NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! NOT) (GO FAIL)))
        (SETQ ßRESULT (or (parse nil 'TO) (cq 'TODEL *fe*)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (m! TO) (GO FAIL)) (:else (m! TO) (GO FAIL)))))
        (SETQ TENSE '(INFINITIVE))
        (GO MODAL2)
EN
        (when LABELTRACE (passing 'EN))
        (FQ NAGR)
        (SETQ ßRESULT (and (parse nil 'NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! NOT) (GO FAIL)))
        (SETQ TENSE '(PAST))
        (SETQ ßRESULT (and (parse 'VB 'EN '(MVB)) (setmvb H) (FQ PASV)))
        (if ßRESULT (GO RETSM) (GO FAIL))
ING
        (when LABELTRACE (passing 'ING))
        (FQ NAGR)
        (SETQ ßRESULT (and (parse nil 'NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! NOT) (GO FAIL)))
INGADV
        (when LABELTRACE (passing 'INGADV))
        (SETQ ßRESULT (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)))
        (COND (ßRESULT (GO INGADV)))
        (SETQ TENSE '(PRESENT))
        (GO BE2)
IMPER
        (when LABELTRACE (passing 'IMPER))
        (SETQ ßRESULT (and (parse 'VB 'DO 'NEG 'INF) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! DONT) (GO FAIL)))
        (SETQ ßRESULT (and (parse 'VB '(MVB) 'INF) (setmvb H) (CALLSM (SMVG))))
        (COND (ßRESULT (GO RETURN)) (:else (m! IMPER) (GO FAIL)))
POLR2
        (when LABELTRACE (passing 'POLR2))
        (or (set! *pt* (getr 'QAUX (MOVE-PT 'C 'U))) (and (bug "VGßPOLR2") (GO FAIL)))
        (SETQ H (list (car *pt*)))
        (TRNSF NEG)
        (COND ((ISQ H DO) (GO DO))
            ((ISQ H MODAL) (GO MODAL))
            ((ISQ H WILL) (GO WILL))
            ((ISQ H BE) (GO BE))
            ((ISQ H HAVE) (GO HAVE)))
        (ert "BUG VGßPOLR2VB")
        (GO FAIL)
DO
        (when LABELTRACE (passing 'DO))
        (FQ DO)
        (MOVE-PT 'C 'DLC)
        (TRNSF VPL NEG INF V3PS)
        (SETQ TENSE (COND ((ISQ *pt* PAST) '(PAST)) (:else '(PRESENT))))
        (if NN (GO DO2) (GO MVB))
DO2
        (when LABELTRACE (passing 'DO2))
        (SETQ ßRESULT (and (parse nil 'NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! NOT) (GO FAIL)))
ADV2
        (when LABELTRACE (passing 'ADV2))
        (SETQ ßRESULT (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)))
        (COND (ßRESULT (COND ((nil? NN) (m! ADV) (GO FAIL)) (:else (GO ADV2)))))
        (SETQ ßRESULT (parse 'VB '(MVB) 'INF))
        (when (nil? ßRESULT) (GO MVB))
        (GO REV)
MODAL
        (when LABELTRACE (passing 'MODAL))
        (FQ NAGR MODAL)
        (SETQ TENSE '(MODAL))
        (if NN (GO MODAL2) (GO INCOMP))
MODAL2
        (when LABELTRACE (passing 'MODAL2))
        (SETQ ßRESULT (and (parse nil 'NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! NOT) (GO FAIL)))
ADV3
        (when LABELTRACE (passing 'ADV3))
        (SETQ ßRESULT (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)))
        (COND (ßRESULT (COND ((nil? NN) (m! ADV) (GO FAIL)) (:else (GO ADV3)))))
        (COND ((parse 'VB 'BE 'INF) (if NN (GO BE2) (GO MVB)))
            ((parse 'VB 'HAVE 'INF) (if NN (GO HAV2) (GO MVB)))
            ((parse 'VB 'INF '(MVB)) (GO REV))
            (:else (GO INCOMP)))
WILL
        (when LABELTRACE (passing 'WILL))
        (FQ NAGR)
        (SETQ TENSE '(FUTURE))
        (if NN (GO MODAL2) (GO INCOMP))
BE
        (when LABELTRACE (passing 'BE))
        (MOVE-PT 'C 'DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (if (ISQ *pt* PAST) '(PAST) '(PRESENT)))
        (if NN (GO BE2) (GO MVB))
BE2
        (when LABELTRACE (passing 'BE2))
        (SETQ ßRESULT (and (parse nil 'NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! NOT) (GO FAIL)))
ADV4
        (when LABELTRACE (passing 'ADV4))
        (SETQ ßRESULT (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)))
        (COND (ßRESULT (COND ((nil? NN) (m! ADV) (GO FAIL)) (:else (GO ADV4)))))
        (COND ((and (nextword? N 'GOING) (parse 'VB)) (GO GOING))
            ((and (nq 'BE) (parse 'VB 'ING)) (SETQ TENSE (cons 'PRESENT TENSE)) (GO EN2))
            ((and (nq 'ING) (parse 'VB 'ING '(MVB))) (SETQ TENSE (cons 'PRESENT TENSE)) (GO REV))
            ((cq 'ING *fe*) (mq! 'ING) (GO FAIL)))
EN2
        (when LABELTRACE (passing 'EN2))
        (SETQ ßRESULT (parse 'VB 'EN '(MVB)))
        (when (nil? ßRESULT) (GO MVBE))
        (FQ PASV)
        (GO REV)
GOING
        (when LABELTRACE (passing 'GOING))
        (SETQ ßRESULT (parse nil 'TO))
        (when (nil? ßRESULT) (GO GOI))
        (SETQ ßRESULT (nq 'INF))
        (COND (ßRESULT (GO GOING2)))
        (POP)
GOI
        (when LABELTRACE (passing 'GOI))
        (SETQ TENSE (cons 'PRESENT TENSE))
        (GO MVB)
GOING2
        (when LABELTRACE (passing 'GOING2))
        (SETQ TENSE (cons 'FUTURE TENSE))
        (GO MODAL2)
MVBE
        (when LABELTRACE (passing 'MVBE))
        (SETQ ßRESULT (ISQ (MOVE-PT 'H 'PV '(VB)) AUX))
        (when (nil? ßRESULT) (GO MVB))
        (SETQ ßRESULT (ISQ *pt* BE))
        (COND ((nil? ßRESULT) (m! MVBE) (GO FAIL)))
        (setmvb *pt*)
        (GO REV)
HAVE
        (when LABELTRACE (passing 'HAVE))
        (MOVE-PT 'C 'DLC)
        (TRNSF VPL INF V3PS VFS)
        (SETQ TENSE (COND ((ISQ *pt* PAST) (FQ NAGR) '(PAST)) (:else '(PRESENT))))
        (if NN (GO HAV2) (GO MVB))
HAV2
        (when LABELTRACE (passing 'HAV2))
        (SETQ ßRESULT (and (parse nil 'NOT) (FQ NEG)))
        (COND ((and (nil? NN) ßRESULT) (m! NOT) (GO FAIL)))
ADV5
        (when LABELTRACE (passing 'ADV5))
        (SETQ ßRESULT (parse 'ADV))
        (COND (ßRESULT (COND ((nil? NN) (m! ADV) (GO FAIL)) (:else (GO ADV5)))))
        (SETQ ßRESULT (parse 'VB 'BE 'EN))
        (when (nil? ßRESULT) (GO HAV3))
        (SETQ TENSE (cons 'PAST TENSE))
        (if NN (GO BE2) (GO MVB))
HAV3
        (when LABELTRACE (passing 'HAV3))
        (SETQ ßRESULT (parse 'VB '(MVB) 'EN))
        (when (nil? ßRESULT) (GO MVB))
        (SETQ TENSE (cons 'PAST TENSE))
        (GO REV)
INCOMP
        (when LABELTRACE (passing 'INCOMP))
        (FQ INCOMP)
        (GO FAIL)
MVB
        (when LABELTRACE (passing 'MVB))
        (SETQ ßRESULT (= (features MVB) (features H)))
        (COND (ßRESULT (GO MVB2)))
        (POP VB)
        (SETQ ßRESULT (parse 'VB '(MVB)))
        (COND ((nil? ßRESULT) (m! MVB) (GO FAIL)))
MVB2
        (when LABELTRACE (passing 'MVB2))
        (GO REV)
REV
        (when LABELTRACE (passing 'REV))
        (setr 'TENSE TENSE *c*)
        (and NN (parse nil 'NOT) (FQ NEG))
        (COND ((or (= TENSE '(PAST)) (cq 'NAGR *fe*) (ISQ (MOVE-PT 'C 'U) IMPER) (ISQ *pt* THERE) (ISQ *pt* RSNG)) (GO NAUX))
            ((set! *pt* (getr 'SUBJECT (MOVE-PT 'C 'U))))
            (:else (bug "VG -- NO SUBJECT TO CHECK FOR AGREEMENT")))
        (SETQ T3 nil)
        (COND ((ISQ *pt* NFS) (or (SETQ T3 (MEET *fe* '(VFS INF))) (GO NAGR)))
            ((ISQ *pt* CLAUSE) (or (SETQ T3 (cq 'V3PS *fe*)) (GO NAGR)))
            ((or (ISQ *pt* NS) (ISQ *pt* MASS)) (or (and (cq 'V3PS *fe*) (SETQ T3 true)) (FESET *pt* (SETDIF (features *pt*) '(NS MASS))))))
        (COND ((or (ISQ *pt* PART) (ISQ *pt* NPL)) (or (and (MEET *fe* '(INF VPL)) (SETQ T3 true)) (FESET *pt* (SETDIF (features *pt*) '(PART NPL))))))
NAGR
        (when LABELTRACE (passing 'NAGR))
        (SETQ ßRESULT (or T3 (and (= '(PAST-PRESENT) TENSE) (SETQ TENSE '(PAST)))))
        (COND ((nil? ßRESULT) (m! NAGR) (GO FAIL)))
NAUX
        (when LABELTRACE (passing 'NAUX))
        (setmvb (or (MOVE-PT 'H 'PV '(MVB)) MVB))
        (SETQ ßRESULT
        (and (cq 'NAUX *fe*) (ISQ (MOVE-PT 'H 'PV '(VB)) AUX) (not (MOVE-PT 'PV 'PV '(VB)))))
        (COND (ßRESULT (m! NAUX) (GO FAIL)) (:else (GO RETSM)))
POPV
        (when LABELTRACE (passing 'POPV))
        (ert "POPV")
        (GO FAIL)
RETSM
        (when LABELTRACE (passing 'RETSM))
        (SETQ ßRESULT (CALLSM (SMVG)))
        (if ßRESULT (GO RETURN) (GO FAIL))
FAIL
        (set! *mes* *me*)
        (SETQ N (or (N *re*) NB))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild (reverse *fe*) NB N H SM *c*)))

(§ defn- PREPG []
    (let [*fe* nil H nil *me* nil NB nil *c* nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil]
        (SETQ NN true)
        (SETQ CUT *end*)
        (set! *c* (buildnode (set! *fe* (reverse REST)) (SETQ NB (or (NB *re*) N)) N (SETQ H *re*) nil))
        (setr 'PARENT PARENT *c*)
ENTERING-PREPG
        (when LABELTRACE (passing 'ENTERING-PREPG))
ADV
        (when LABELTRACE (passing 'ADV))
        (SETQ ßRESULT (and (nq 'PREPADV) (parse 'ADV 'PREPADV)))
        (COND (ßRESULT (COND ((nil? NN) (m! PREPADV) (GO FAIL)) (:else (GO ADV)))))
        (SETQ ßRESULT (COND ((cq 'AGENT *fe*) (nextword? N 'BY)) ((cq 'LOC *fe*) (nq 'PLACE)) ((cq 'Q *fe*) (not (nq 'MOTOR))) (:else true)))
        (COND ((nil? ßRESULT) (m! PREP) (GO FAIL)))
        (SETQ ßRESULT (parse 'PREP))
        (COND ((nil? ßRESULT) (m! PREP) (GO FAIL)))
        (MOVE-PT 'H)
        (TRNSF PLACE TIME)
        (SETQ T1 H)
        (and (nq 'PREP2)
            (COND ((SETQ T1 (COMBINATION? (word (NB H)) (word N))) (parse 'PREP2))
                ((SETQ T1 (COMBINATION? (word (NB H)) (word N) (word (cdr N)))) (parse 'PREP2) (parse 'PREP2)))
            (SETQ T1 (buildnode (features T1) NB N 'WORD (SM T1)))
            (setr 'PARENT *c* T1))
        (SETQ ßRESULT (ISQ H NEED2))
        (COND (ßRESULT (m! NEED2) (GO FAIL)))
        (setr 'HEAD T1 *c*)
        (or NN (GO SHORT))
        (COND ((= (word H) 'BY) (FQ AGENT)))
QUEST
        (when LABELTRACE (passing 'QUEST))
        (SETQ ßRESULT (cq 'QUEST *fe*))
        (when (nil? ßRESULT) (GO NG))
        (SETQ ßRESULT (parse 'NG 'QUEST 'OBJ))
        (COND (ßRESULT (GO OBJR)) (:else (m! PREPQUEST) (GO FAIL)))
        (SETQ ßRESULT (and (cq 'OF *fe*) (parse 'NG 'OFOBJ)))
        (COND (ßRESULT (GO OBJR)))
NG
        (when LABELTRACE (passing 'NG))
        (SETQ ßRESULT (parse 'NG 'OBJ))
        (COND (ßRESULT (GO OBJR)))
REL
        (when LABELTRACE (passing 'REL))
        (SETQ ßRESULT (nextword? N 'WHICH))
        (when (nil? ßRESULT) (GO REST))
        (SETQ ßRESULT (ISQ (MOVE-PT 'U) CLAUSE))
        (COND ((nil? ßRESULT) (m! PREP-WHICH) (GO FAIL)))
        (SETQ ßRESULT (ISQ *pt* PRONREL))
        (when (nil? ßRESULT) (GO PRONREL))
        (set! *mes* (cdr *mes*))
        (GO P-RELWRD)
PRONREL
        (when LABELTRACE (passing 'PRONREL))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (add-f-pt 'PRONREL *pt*)
P-RELWRD
        (when LABELTRACE (passing 'P-RELWRD))
        (parse 'NG 'RELWD 'OBJ)
        (setr 'OBJ1 (getr 'HEAD *pt*) *c*)
        (GO RETT)
REST
        (when LABELTRACE (passing 'REST))
        (SETQ ßRESULT (parse 'CLAUSE 'RSNG 'ING))
        (if ßRESULT (GO OBJR) (GO SHORT))
OBJR
        (when LABELTRACE (passing 'OBJR))
        (setr 'OBJ1 H *c*)
        (GO RETT)
SHORT
        (when LABELTRACE (passing 'SHORT))
        (SETQ ßRESULT (MEET *fe* '(NOSHORT Q)))
        (COND (ßRESULT (m! SHORT) (GO FAIL)))
        (or (ISQ (MOVE-PT 'C 'U) REL-NOT-FOUND) (ISQ (getr 'QUESTION-ELEMENT *pt*) QADJ) (GO FAIL))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (add-f-pt 'PREPREL *pt*)
        (setr 'OBJ1 (getr 'RELHEAD (MOVE-PT 'C 'U)) *c*)
RETT
        (when LABELTRACE (passing 'RETT))
        (and (or (ISQ H QUEST) (and (ISQ H COMPOUND) (MOVE-PT 'H 'H 'PV '(QUEST)))) (FQ QUEST))
        (SETQ ßRESULT (CALLSM (SMADJG-PREPG)))
        (if ßRESULT (GO RETURN) (GO FAIL))
FAIL
        (set! *mes* *me*)
        (SETQ N (or (N *re*) NB))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild (reverse *fe*) NB N H SM *c*)))

(§ defn- ADJG []
    (let [*fe* nil H nil *me* nil NB nil *c* nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil]
        (SETQ NN true)
        (SETQ CUT *end*)
        (set! *c* (buildnode (set! *fe* (reverse REST)) (SETQ NB (or (NB *re*) N)) N (SETQ H *re*) nil))
        (setr 'PARENT PARENT *c*)
ENTERING-ADJG
        (when LABELTRACE (passing 'ENTERING-ADJG))
COMPCHECK
        (when LABELTRACE (passing 'COMPCHECK))
        (SETQ ßRESULT (and (MOVE-PT 'C 'U '(BE)) (not (cq 'COMP *fe*))))
        (COND (ßRESULT (GO FAIL)))
        (SETQ ßRESULT (ISQ (MOVE-PT 'C 'U) THAN))
        (when (nil? ßRESULT) (GO DISP))
        (setr 'HEAD (getr 'COMPARATIVE-MODIFIER *pt*) *c*)
        (GO THAN)
DISP
        (when LABELTRACE (passing 'DISP))
        (SETQ ßRESULT (and (nq 'AS) (parse nil 'AS)))
        (COND (ßRESULT (COND ((nil? NN) (m! AS) (GO FAIL)) (:else (GO AS)))))
        (SETQ ßRESULT (and (nq 'AS) (parse nil 'AS)))
        (COND (ßRESULT (COND ((nil? NN) (m! AS) (GO FAIL)) (:else (GO AS)))))
        (SETQ ßRESULT (nextword? N 'HOW))
        (if ßRESULT (GO HOW) (GO ADV))
HOW
        (when LABELTRACE (passing 'HOW))
        (SETQ ßRESULT (and (parse nil 'HOW) (FQ QUEST)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO FAIL)) (:else (GO FAIL)))))
        (SETQ ßRESULT (and (parse 'ADJ) (FQ ADJ) (setr 'HEAD H *c*)))
        (COND (ßRESULT (GO RETSM)))
        (SETQ ßRESULT (and (parse 'ADV 'VBAD) (FQ VBAD) (setr 'HEAD H *c*)))
        (if ßRESULT (GO RETSM) (GO FAIL))
ADV
        (when LABELTRACE (passing 'ADV))
        (SETQ ßRESULT (parse 'ADV 'ADVADV))
        (COND (ßRESULT (if (nil? NN) (GO POPAD) (GO ADV))))
        (SETQ ßRESULT (parse nil 'MORE))
        (when (nil? ßRESULT) (GO ADJ))
        (FQ COMPAR)
ADJ
        (when LABELTRACE (passing 'ADJ))
        (SETQ ßRESULT (COND ((cq 'ADV *fe*) (parse 'ADV 'VBAD)) (:else (parse 'ADJ))))
        (COND ((nil? ßRESULT) (m! ADJ) (GO FAIL)))
        (SETQ ßRESULT (setr 'HEAD H *c*))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (or (cq 'COMPAR *fe*) (ISQ H COMPAR)))
        (when (nil? ßRESULT) (GO RETSM))
        (FQ COMPAR)
        (SETQ ßRESULT NN)
        (when (nil? ßRESULT) (GO RETSM))
THAN
        (when LABELTRACE (passing 'THAN))
        (COND ((not NN) (GO RETSM)))
        (SETQ ßRESULT (parse nil 'THAN))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (m! THAN) (GO FAIL)) (:else (GO RETSM)))))
        (RQ THANNEED)
        (FQ THAN)
        (GO SUBJ)
AS
        (when LABELTRACE (passing 'AS))
        (FQ AS)
        (RQ THANNEED)
        (SETQ ßRESULT (and (parse 'ADJ) (setr 'HEAD H *c*)))
        (COND ((nil? ßRESULT) (COND ((nil? NN) (GO RETSM)) (:else (m! ADJ) (GO FAIL)))))
        (SETQ ßRESULT (parse nil 'AS))
        (COND (ßRESULT (COND ((nil? NN) (m! AS) (GO FAIL)) (:else (GO SUBJ)))) (:else (GO RETSM)))
SUBJ
        (when LABELTRACE (passing 'SUBJ))
        (SETQ ßRESULT (parse 'NG 'SUBJ 'COMPAR))
        (COND ((nil? ßRESULT) (m! THAN) (GO FAIL)))
        (SETQ ßRESULT (setr 'OBJ1 H *c*))
        (COND ((and (nil? NN) ßRESULT) (GO RETSM)))
        (SETQ ßRESULT (and (one-word-left NB) (parse 'VB 'AUX)))
        (when (nil? ßRESULT) (GO RETSM))
        (SETQ ßRESULT (CHECK-AGREEMENT H (cdr H)))
        (COND (ßRESULT (GO RETSM)))
        (POP)
        (GO RETSM)
POPAD
        (when LABELTRACE (passing 'POPAD))
        (POP)
        (GO ADJ)
RETSM
        (when LABELTRACE (passing 'RETSM))
        (SETQ ßRESULT (cq 'THANNEED *fe*))
        (COND (ßRESULT (m! THANNEED) (GO FAIL)))
        (SETQ ßRESULT (CALLSM (SMADJG-PREPG)))
        (COND (ßRESULT (GO RETURN)) (:else (m! SMADJ) (GO FAIL)))
FAIL
        (set! *mes* *me*)
        (SETQ N (or (N *re*) NB))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild (reverse *fe*) NB N H SM *c*)))

(§ defn- CONJOIN []
    (let [*fe* nil H nil *me* nil NB nil *c* nil SM nil CUT nil NN nil T1 nil T2 nil T3 nil ßRESULT nil PREV nil]
        (SETQ NN true)
        (SETQ CUT *end*)
        (set! *c* (buildnode (set! *fe* (reverse REST)) (SETQ NB (or (NB *re*) N)) N (SETQ H *re*) nil))
        (setr 'PARENT PARENT *c*)
ENTERING-CONJOIN
        (when LABELTRACE (passing 'ENTERING-CONJOIN))
UP
        (when LABELTRACE (passing 'UP))
        (SETQ PREV (nextword N))
        (FLUSHME)
        (COND ((and (= PREV '\,) (or (cdr H) (> (- (count (NB H)) (count (N H))) 4)) (memq (nextword N) '(OR AND NOR BUT)) (F (nextword N)))
            (SETQ PREV (list PREV (nextword N)))
            (FLUSHME)))
        (and (term? PREV) (MOVE-PTW 'N 'NW '(= (word *ptw*) PREV)) (CUT *ptw*))
        (and (or (= PREV 'BUT) (= (cadr PREV) 'BUT)) (nextword? N 'NOT) (or (FLUSHME) (GO LOSE2)) (FQ NEGBUT))
        (SETQ ßRESULT (COND
            ((memq (car REST) '(ADJ NUM NOUN PREP VB ADV)) (PARSE3 (concat REST '(COMPONENT)) nil))
            ((memq (car REST) '(NG PREPG ADJG)) (and (not (cq 'OFOBJ *fe*)) (PARSE2 (concat REST '(COMPONENT)) nil)))
            ((= (car REST) 'CLAUSE)
                ((lambda [*lastsent* AUXFE]
                    (and (PARSE2 (concat REST AUXFE '(COMPONENT)) nil) (or (not AUXFE) (F (car AUXFE))) (setr 'TIME (getr 'TIME H) *c*)))
                (if (ISQ H MAJOR) H *lastsent*)
                (MEET (features H) '(DECLAR IMPER))))))
        (when (nil? ßRESULT) (GO LOSE2))
        (CUT *end*)
        (COND ((not (term? PREV)) (GO RETSM))
            ((= PREV '\,) (COND ((nextword? N COMMA) (FQ LIST) (GO UP)) (:else (GO LIST))))
            ((memq PREV '(AND OR NOR BUT))
                (COND ((= BOTH (NB H)) (FQ BOTH)))
                (COND ((or (nextword? N 'BUT) (and (nextword? N PREV) (not (and (= BOTH (NB H)) (= PREV 'AND)))))
                        (FQ LISTA)
                        (F PREV)
                        (GO UP))
                    (:else (GO LISTA)))))
LOSE2
        (when LABELTRACE (passing 'LOSE2))
        (SETQ ßRESULT (cq 'LISTA *fe*))
        (COND (ßRESULT (GO LISTA)))
LIST
        (when LABELTRACE (passing 'LIST))
        (SETQ ßRESULT (and (= PREV '\,) (== (count H) 2) (ISQ H NG) (not (or (ISQ H PRONG) (ISQ (cdr H) PRONG))) (or (nextword? N COMMA) (nil? N))))
        (COND ((nil? ßRESULT) (m! CONJOINß) (GO FAIL)))
        (FLUSHME)
        (FQ APPOSITIVE)
        (GO RETSM)
LISTA
        (when LABELTRACE (passing 'LISTA))
        (F PREV)
RETSM
        (when LABELTRACE (passing 'RETSM))
        (FQ COMPOUND)
        (and (> (count H) 2) (FQ LIST))
        (COND ((or (cq 'NG *fe*) (cq 'NOUN *fe*)) (COND ((cq 'AND *fe*) (FQ NPL)) (:else (MOVE-PT 'H) (TRNSF NPL NS MASS NFS))))
            ((cq 'VB *fe*) (let [COMMON nil] (SETQ COMMON (getprop 'VB 'ELIM)) (MAP #'(lambda [x] (SETQ COMMON (MEET COMMON (features x)))) H) nil) (FESET (UNION COMMON (features *c*)) *c*)))
        (SETQ ßRESULT (CALLSM (SMCONJ)))
        (COND (ßRESULT (GO RETURN)) (:else (m! CONJOINß) (GO FAIL)))
FAIL
        (set! *mes* *me*)
        (SETQ N (or (N *re*) NB))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild (reverse *fe*) NB N H SM *c*)))

#_(ns shrdlu.init)

(§ defn- SHRDLU []
    (binding [*thtime* 0 *thtrace* nil *thtree* nil *thxx* nil *thalist* '((nil nil)) *tholist* '((nil nil))
              *global-message* nil
              *savesent* nil *lastsentno* 0 *lastsent* nil *sentno* 1
              *level* nil *sent* nil *punct* nil
              *end* nil BOTH nil *backref* nil *backref2* nil *ansname* nil *lastrel* nil WHO nil *pt* nil *ptw* nil H nil N nil NB nil *fe* nil SM nil *re* nil *mes* nil *c* nil CUT nil
              *savept* nil]
    =>  (*CATCH 'ABORT-PARSER
            (loop []
                (set! *global-message* nil)
                (set! *lastsentno* (inc *lastsentno*))
                (set! *lastsent* *c*)
                (set! *sentno* (inc *sentno*))
                (set! *level* 0)
                (set! *mes* 'NOPE)
                (set! *backref* nil)                  ;; ???????????????????
                (SETQ N (set! *sent* (ETAOIN)))
                (when (and (ERRSET (set! *pt* (set! *c* (parse 'CLAUSE 'MAJOR 'TOPLEVEL)))) *c*)
                    (set! *fe* (features *c*))
                    (SETQ NB *sent*)
                    (SETQ H (H *c*))
                    (SETQ INTERPRETATION (SM *c*))
                    (or (ERRSET (eval '(ANSWER *c*))) (print (or *global-message* "I DON'T UNDERSTAND."))))
                (recur)))
        (GO =>)))

(§ defn -main [& args]
    ;; LOAD '(PLNR THTRAC SYSCOM MORPHO SHOW PROGMR GINTER GRAMAR DICTIO SMSPEC SMASS SMUTIL NEWANS BLOCKS DATA)

    (LABELTRACE CLAUSE NG VG ADJG PREPG CONJOIN)

    (SETQ LABELTRACE nil LABELBREAK nil PLANNERSEE nil)

    (ERRSET (STARTHISTORY))

    (terpri)
    (print "YOU ARE NOW IN A READ-EVAL-PRINT LOOP")
    (terpri)
    (print "TYPE \"GO \" TO ENTER READY STATE")

    (*CATCH 'ABORT-PARSER (ert nil))

    (SHRDLU))


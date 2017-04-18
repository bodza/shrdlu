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

(dynamic- *thexp*)
(dynamic- *thbranch*)
(dynamic- *thabranch*)
(dynamic- *thmessage*)

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

(dynamic- *both*)
(dynamic- *backref*)
(dynamic- *backref2*)
(dynamic- *ansname*)
(dynamic- *lastrel*)
(dynamic- *who*)
(dynamic- *pt*)
(dynamic- *ptw*)
(dynamic- *h*)
(dynamic- *n*)
(dynamic- *nb*)
(dynamic- *fe*)
(dynamic- *sm*)
(dynamic- *re*)
(dynamic- *c*)
(dynamic- *cut*)

(dynamic- *parent*)
(dynamic- *special*)

(dynamic- *savept*)
(dynamic- *nn*)

(dynamic- *tmp*)
(dynamic- *quantifier*)

(dynamic- *position-of-prt*)
(dynamic- *mvb*)
(dynamic- *locationmarker*)
(dynamic- *subj-vb-backup-type1*)
(dynamic- *position-of-ptw*)
(dynamic- *tense*)
(dynamic- *prev*)

(dynamic- *labeltrace*)
(dynamic- *plannersee*)

(dynamic- *grasplist*)
(dynamic- *eventlist*)

(dynamic- *handat*)

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

(defn- setr [node register value] (putprop! (car node) register value))
(defn- getr [node register] (getprop (car node) register))

(declare topcenter abs atab clear diff half endtime ev FINDSPACE goal GROW locgreater MEMOREND memory occupier order PACKO packon packord size starthistory startime support tcent tfind timechk evlis THADD THAMONG THAMONGF THAND thandf THANDT THANTE THAPPLY thapply1 THASS1 THASSERT thassertf thassertt THASVAL THBA THBIND thbi1 THBKPT THBRANCH THBRANCHUN THCOND thcondf THCONDT THCONSE THDEF THDO THDO1 THDOB THERASE therasef theraset THERASING THFAIL thfail? thfail?f thfail?t THFINALIZE THFIND thfindf THFINDT THFLUSH thgal THGO THGOAL thgoalf thgoalt THIP THMATCH2 THCHECK THUNION THMATCH1 THMATCHLIST THMESSAGE thmessagef thmessaget thmungf thmungt thnofail THNOHASH THNOT THNV THOR THOR2 thorf thort thpopt THPROG THPROGA thprogf thprogt thpure thputprop THREM1 thrembindf thrembindt THREMOVE thremprop THRESTRICT THRETURN THRPLACA THRPLACAS THURPLACA THRPLACD THRPLACDS THURPLACD THSETQ thsgal thstate THSUCCEED THTAE THTAG thtagf thtagt thtrue THTRY1 THTRY THUNDOF thundot THUNIQUE thv1 THV THVAL thvar? THVARS2 THVARSUBST THVSETQ thtrace thtrace1 thuntrace THTRACES THSEL %sent DA DISP DP forget ert bug global-err ERTEX combination? findb from meet mod setdif sta union TAB uppercase-ify-char ETAOIN propname buildword undefined passing pev TELLCHOICE SHOWCHOICE SHOWTELL QUERY onoff REQUEST treeprint charg SHOW TELL showmove DEFINE HELP spread1 PDEFINE setmvb add-f-pt remove-f-pt one-word-left MOVE-PT MOVE-PTW apply-grammar buildnode cq cut cut-back-one f! features feset flushme following fq! daughters isq m! wordafter firstword nextword nextword? nq parent parse PARSE2 PARSE3 parserel POP POPTO previous PTFIND rebuild root rq secondword? semantics trnsf word CONJ COMMA doublequoter CANTAKE CANPARSE !BETHERE !BEINT BOTH thank !BLUEPRINT !BUILD !COLOR !CLEANOFF !DEFINE !DEFINITION !EQDIM !GRASP !HAVE !IN !LOC !LOC2 !NAME !NOTICE !ON !PROPDEFINE !ROLE !STACKUP smtime SMNEWNOUN smnewpropn SMCONJ SMCONJ2 SMVG SMPRON SMVAUX smplace smtoadj SMPROP SMADJ SMADJG-PREPG SMIT SMIT2 SMNGOF SMNG1 SMNG2 SMNG3 SMONE SMONE2 SMONE3 SMPOSS SMPOSS2 SMRELATE SMCL1 SMCL2 SMCL-MODIFIERS SMBIND smbinder istense imperf? BUILD newcopy RELATION DOBACKREF evalcheck ITERATE ITERATEX MAPBLAND mapc2 mumble OBJECT valueput plnr-junkify plnr-junkify2 PLNR-THCONSIFY PLNR-FINDIFY plnr-findspec PLNR-GOALIFY plnr-mung PLNR-NOTIFY PLNR-NEWBODY PLNR-PROGIFY PLNR-NUMREL PLNR-NUMSUB PLNR-RECOMMENDIFY PLNR-REMTIME plnr-var COMPARE-BUILD findmeasure MEASURE PLNR-DESCRIBE RELFIND ORDMAKE COMPARE-PROC EXPAND erqset SETQQCHECK thval2 who check-markers CHECKAMARKER FINDEVENTS checkrel action? ambiguities? and? ansrss? determiner? end? markers? modifiers? negative? num? or? oss? parsenode? plausibility? plnrcode? qtype? quantifier? refer? rel? relations? relmarkers? rss? rssvar? start? systems? tense? tss? variable? smset ANSWER ambput ANSBUILD ANSCOMMAND ANSDECLARE ANSELIMINATE parse-assoc ANSGEN ANSNAME ANSNOREL ANSORDER ANSQUEST ANSREL ANSTHM ANSTHMADD ansthmelement ansunique cutoff describevent disput ELIZA enough-better FINDMOTHER headpart LISTNAMES pron-prt nameaction namelist namelist-evaled namenum ansay NAMEOBJ namesize namesugar notell ONECHECK ordname PLNR-ANDORIFY prepput PLURALIZE PLURALMAKE THVAL-MULT toplevel findreduce FINDCHOOSE MUNG vbfix CLAUSE NG VG PREPG ADJG CONJOIN SHRDLU)

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
            ((SETQ THFOO (concat THFOO (list (when (= THT1 'THVRB) (car THCK)))))
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
    (COND (*thmessage* (thpopt) nil)
        ((caddar *thtree*)                                            ;; LIST OF NEW VALUES NON NIL
            (RPLACA (cdadar *thtree*) (caaddr (car *thtree*)))          ;; REPLACE OLD VALUE WITH NEW VALUE
            (RPLACA (cddar *thtree*) (cdaddr (car *thtree*)))           ;; POP NEW VALUES
            (set! *thbranch* *thtree*)                                  ;; STORE AWAY TREE FOR POSSIBLE BACKTRACKING
            (set! *thabranch* *thalist*)                                ;; STORE AWAY THALIST FOR POSSIBLE BACKTRACKING
            (thpopt)                                                ;; POP TREE
            true)                                                      ;; SUCCEED
        (:else (RPLACA (cdadar *thtree*) 'THUNASSIGNED)                   ;; NO NEW VALUES LEFT. RETURN X TO THUNASSIGNED,
            (thpopt)                                                ;; POP TREE AND CONTINUE FAILING.
            nil)))

(§ defq- THAND [a]
    (or (not a)
        (do (THPUSH *thtree* (list 'THAND a nil)) (set! *thexp* (car a)))))

(defn- thandf [] (THBRANCHUN) nil)

(§ defn- THANDT []
    (COND ((cdadar *thtree*)
            (THBRANCH)
            (set! *thexp* (cadr (cadar *thtree*)))
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
        (COND (THY (set! *thexp* (cons 'THDO THY))))
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
    (let [x (thgal (car a) *thalist*)] (and x (not= (cadr x) 'THUNASSIGNED))))

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
            ;; OTHERWISE ADD TO THALIST THE NEW BINDING CELL
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
        ((= *thbranch* *thtree*) (set! *thbranch* nil))
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
        ((RPLACA (cddar *thtree*) (cons (list *thbranch* *thabranch* (cadar *thtree*)) (caddar *thtree*)))
            ;; WE NOW SETQ THBRANCH TO NIL.  IF THE NEXT LINE ALSO SUCCEEDS,
            ;; THVAL WILL LOOK FOR A NIL THBRRANCH TO INDICATE THAT IT SHOULD
            ;; SETQ IT AGAIN TO THE POINT OF SUCCESS
            (set! *thbranch* nil))))

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
    (set! *thexp* (caar tha)))

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
        (do (THPUSH *thtree* (list 'THDO a nil nil)) (set! *thexp* (car a)))))

(§ defn- THDO1 []
    (RPLACA (cdar *thtree*) (cdadar *thtree*))
    (set! *thexp* (caadar *thtree*))
    (when *thbranch*
        (RPLACA (cddar *thtree*) (cons *thbranch* (caddar *thtree*)))
        (set! *thbranch* nil)
        (RPLACA (cdddar *thtree*) (cons *thabranch* (car (cdddar *thtree*))))))

(§ defn- THDOB []
    (COND ((or *thmessage* (nil? (cdadar *thtree*)))
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
                ((= (car tha) 'THMESSAGE) (set! *thmessage* (cadr tha)) (RETURN nil))
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
            (set! *thmessage* (list (cdr THTREE1) (and (cdr tha) (cadr tha))))
            (RETURN nil)
        TAGS (SETQ THX (caddar THTREE1))
        LP2  (COND ((nil? THX) (GO ALP1))
                ((= (caaddr (car THX)) (cadr tha))
                    (set! *thmessage* (list (caar THX) (and (cddr tha) (caddr tha))))
                    (RETURN nil)))
            (SETQ THX (cdr THX))
            (GO LP2))))

(defn- thfail? [prd act]
    (THPUSH *thtree* (list 'thfail? prd act))
    *thvalue*)

(defn- thfail?f []
    (if (eval (cadar *thtree*))
            (eval (let [_ (set! *thmessage* nil) _ (caddar *thtree*)] (thpopt) _))
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
    (set! *thbranch* nil)
    (if (or *thmessage* (< (caadr (set! *thxx* (cdar *thtree*))) (caar *thxx*)))
        (do (thpopt) nil)
        (do (thpopt) (cdadr *thxx*))))

(§ defn- THFINDT []
    (let [THCDAR (cdar *thtree*) THX nil THY nil THZ (caddr THCDAR)]
        (and (memq (SETQ THX (THVARSUBST THZ nil)) (cdadr THCDAR))
            (GO =>))
        (RPLACD (cadr THCDAR) (cons THX (cdadr THCDAR)))
        (and (= (SETQ THY (inc (caadr THCDAR))) (cadar THCDAR))
            (RETURN (PROG2 (set! *thbranch* nil) (and (caddar THCDAR) (cdadr THCDAR)) (thpopt))))
        (RPLACA (cadr THCDAR) THY)
    =>  (set! *thtree* *thbranch*)
        (set! *thalist* *thabranch*)
        (set! *thbranch* nil)
        nil))

(§ defq- THFLUSH [a]
    ;; (THFLUSH) FLUSHES ALL ASSERTIONS AND THEOREMS
    ;; INPUT = SEQUENCE OF INDICATORS DEFAULT =
    ;; EFFECT = FLUSHES THE PROPERTIES OF THESE
    ;; (THASSERTION THCONSE THANTE THERASING)
    ;; INDICATORS FROM ALL ATOMS
    (dorun (map (lambda [b]
        (dorun (map (lambda [c]
            (dorun (map (lambda [d]
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
        (when (nil? THA1) (RETURN nil))
        (THPUSH *thtree* (list 'THGOAL THA2 THA1))                    ;; (THGOAL PATTERN MATCHES)
        (RPLACD (cddar *thtree*) 262143)
        nil))                                                       ;; FAILS TO THGOALF

(defn- thgoalf []
    ;; BASICALLY ALL IT DOES IS TO SEND OFF TO THTRY1 TO TRY ANOTHER POSSIBILITY.
    ;; IF THTRY1 RETURNS NIL, IT MEANS THAT IT COULDN'T FIND ANOTHER POSSIBILITY
    ;; AND WE SHOULD TELL THVAL THAT WE HAVE FAILED.
    ;; ALL THPOPT DOES IS TO LOB THE THGOAL ENTRY OFF THTREE.
    (if *thmessage* (do (thpopt) nil) (or (THTRY1) (do (thpopt) nil))))

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
        (ERRSET (dorun (map (lambda [THY] (or (THY thx) (ERR nil))) thprd)))))

(§ defn- THUNION [l1 l2]
    (dorun (map (lambda [THX]
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
        (if (and (THBIND (cadr x)) (THMATCH1 (caddr x) *thmessage*))
            (do (THPUSH *thtree* (list 'THPROG (cddr x) nil (cddr x)))
                (set! *thmessage* nil)
                (THPROGA))
            (do (set! *thalist* *tholist*) nil))))

(defn- thmessaget [] (thpopt) *thvalue*)

(defn- thmungf [] (evlis (cadar *thtree*)) (thpopt) nil)

(defn- thmungt [] (thpopt) *thvalue*)

(defn- thnofail [thx]
    (putprop! 'THPROG 'THFAIL (if thx 'thprogt 'thprogf)))

(§ defq- THNOHASH [tha]
    (dorun (map #(putprop! (car tha) % 'THNOHASH)
        (or (cdr tha) '(THASSERTION THCONSE THANTE THERASING)))))

(§ defq- THNOT [tha]
    (set! *thexp* (list 'THCOND (list (car tha) '(THFAIL THAND)) '((THSUCCEED)))))

(§ defq- THNV [x] (thv1 (car x)))

(§ defq- THOR [tha]
    (and tha
        (THPUSH *thtree* (list 'THOR tha))
        (set! *thexp* (car tha))))

(§ defn- THOR2 [p]
    (COND (*thmessage* (thpopt) nil)
        ((and (cadar *thtree*) (cdadar *thtree*))
            (RPLACA (cdar *thtree*) (cdadar *thtree*))
            (set! *thexp*
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
                (set! *thexp* (list 'THTAG (cadar x)))
                ;; MOVE POINTER TO NEXT EXPRESSION.
                (RPLACA x (cdar x))
                *thvalue*)
            ;; OTHERWISE NEXT EXPRESSION TO BE EVALUATED IS THE NEXT EXPRESSION OF THE THPROG.
            (:else
                (set! *thexp* (cadar x))
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
                (SETQ THFOO (concat THFOO (list (when (= THON 'THVRB) (car THB1)))))
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
        (dorun (map (lambda [BUCKET]
            (dorun (map (lambda [THATOM]
                (dorun (map (lambda [THWH]
                    (and (SETQ thp (getprop THATOM THWH)) (SETQ thp (assq 1 (cdr thp)))
                        (dorun (map (lambda [LENGTH-BUCKET]
                            (dorun (map (lambda [asrt]
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
            (set! *thbranch* *thtree*)
            (set! *thabranch* *thalist*)
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
            (doall (map (lambda [x]
                    (COND ((not (and (set! *thxx* (getprop x 'THEOREM)) (= (car *thxx*) TYPE)))
                            (terpri)
                            (pr x)
                            (list 'THAPPLY (ert "BAD THEOREM - THTAE") (car THX)))
                        (:else (list 'THAPPLY x (car THX)))))
                (cdr xx))))
        ((= (car xx) 'THTBF)
            (doall (map (lambda [y]
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

(§ defn- THVAL [*thexp* *thalist*]
    ;; CORRESPONDS TO LISP EVAL.
    ;; THEXP IS THE EXPRESSION TO BE THVALUATED.
    ;; THALIST IS THE VARIABLE BINDING LIST.
    (let [*thtree* nil *thvalue* 'THNOVAL *thbranch* nil *tholist* nil *thabranch* nil THE nil *thmessage* nil]
        (SETQ THV '(THV THNV))
        ;; "THE" BECOMES THE CURRENT EXPRESSION.
        ;; THEXP IS RESERVED FOR FURTHER EXPRESSIONS,
        ;; WHICH SHOULD BE THVALED BEFORE WE GO TO THE NEXT ITEM OF ACTUAL CODE.
        ;; FOR EXAMPLE, THASSERT USES THIS FEATURE TO PROCESS ANTECEDENT THEOREMS.
    GO  (SETQ THE *thexp*)
        (set! *thexp* nil)
        ;; EVAL THE CURRENT EXPRESSION TO BE THVALED.
        ;; NOTE THAT EACH PLANNER FUNCTION CORRESPONDS TO THREE LISP FUNCTIONS:
        ;; ONE TO SET THINGS UP (THIS IS WHAT IS GETTING EVALED AT THIS POINT),
        ;; ONE TO HANDLE SUCCESS AND ONE FOR FAILURE.
        (COND ((ERRSET (set! *thvalue* (eval THE))))
            ;; IF THERE WAS A LISP ERROR, REPORT IT TO THE USER.
            (:else (terpri) (pr THE) (set! *thvalue* (ert "LISP ERROR - THVAL"))))
        ;; USUALLY THEMESSAGE WILL BE NIL.
        ;; EXCEPTION IS WHEN USER HAS USED THE THMESSAGE FUNCTION.
    GO1 (COND (*thmessage* (GO MFAIL))
            ;; IF THEXP IS NON NIL, IT MEANS THAT WE HAVE MORE PLANNER TO WORK ON BEFORE GOING TO NEXT LINE OF USER CODE.
            (*thexp* (GO GO))
            ;; IF THVALUE IS NON NIL, IT MEANS THAT SO FAR THE THEOREM IS SUCCEEDING.
            (*thvalue* (GO SUCCEED))
            ;; ELSE WE ARE IN A FAILURE SITUATION.
            (:else (GO FAIL)))
    SUCCEED
        ;; SAVE CURRENT STATE OF THTREE AND THALIST IN CASE WE HAVE TO BACK UP.
        (COND ((nil? *thbranch*)
            (set! *thbranch* *thtree*)
            (set! *thabranch* *thalist*)))
        ;; IF THE THTREE IS NIL, IT MEANS THAT THE THPROG OR WHATEVER HAS BEEN COMPLETED,
        ;; SO THERE ARE NO MORE EXPRESSIONS TO DO.  ALL THEOREMS ACT LIKE A THPROG, INCLUDING
        ;; PUTTING ITS MARK ON THTREE, SEE THAPPLY, HENCE NO NEED TO GROW MORE BRANCHES ON THTREE.
        (COND ((nil? *thtree*)
                (RETURN *thvalue*))
            ;; THIS IS THE NORMAL CASE.
            ;; WE EVAL THE SUCCEED-FUNCTION OF THE PLANNER FUNCTION WHICH JUST SUCCEEDED.
            ((set! *thexp* (getprop (caar *thtree*) 'THSUCCEED))
                (GO GO2))
            ;; IN CASE OF LOSSAGE, LETS THE USER SUCCEED ANYWAY.
            ((ert "BAD SUCCEED - THVAL")
                (GO SUCCEED))
            ((GO FAIL)))
        ;; HAS TO DO WITH FAILURE + MESSAGE
    MFAIL (COND ((= (car *thmessage*) *thtree*)
            (set! *thexp* (cadr *thmessage*))
            (set! *thmessage* nil)
            (GO GO)))
        ;; IF THTREE IS NIL, WE HAVE FAILED THE ENTIRE EXPRESSION.
    FAIL (COND ((nil? *thtree*)
                (RETURN nil))
            ;; NORMAL CASE.  EVAL THE FAILURE FUNCTION ASSOCIATED
            ;; WITH THE PLANNER FUNCTION WHICH JUST FAILED.
            ((set! *thexp* (getprop (caar *thtree*) 'THFAIL))
                (GO GO2))
            ((ert "BAD FAIL - THVAL")
                (GO SUCCEED))
            ((GO FAIL)))
        ;; THEXP AT THIS POINT IS THE APPROPRIATE SUCCESS OR FAILURE ASSOCIATED FUNCTION.
        ;; EVAL IT AND AT THE SAME TIME,
        ;; SET IT TO NIL IN CASE WE NEED THEXP FOR MORE EXPRESSIONS TO BE PROCESSED.
    GO2 (set! *thvalue* ((PROG1 *thexp* (set! *thexp* nil))))
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
(putprop! 'THGOAL 'THFAIL 'thgoalf)
(putprop! 'thfail? 'THFAIL 'thfail?f)
(putprop! 'THAMONG 'THFAIL 'THAMONGF)
(putprop! 'THFIND 'THFAIL 'thfindf)
(putprop! 'THAND 'THFAIL 'thandf)
(putprop! 'THPROG 'THFAIL 'thprogf)
(putprop! 'THMUNG 'THFAIL 'thmungf)
(putprop! 'THASSERT 'THFAIL 'thassertf)
(putprop! 'THERASE 'THFAIL 'therasef)
(putprop! 'THCOND 'THFAIL 'thcondf)
(putprop! 'THOR 'THFAIL 'thorf)
(putprop! 'THDO 'THFAIL 'THDOB)
(putprop! 'THUNDO 'THFAIL 'THUNDOF)
(putprop! 'THMESSAGE 'THFAIL 'thmessagef)
(putprop! 'THREMBIND 'THFAIL 'thrembindf)

(putprop! 'THTAG 'THSUCCEED 'thtagt)
(putprop! 'THGOAL 'THSUCCEED 'thgoalt)
(putprop! 'thfail? 'THSUCCEED 'thfail?t)

(putprop! 'THFIND 'THSUCCEED 'THFINDT)
(putprop! 'THAND 'THSUCCEED 'THANDT)
(putprop! 'THPROG 'THSUCCEED 'thprogt)
(putprop! 'THMUNG 'THSUCCEED 'thmungt)
(putprop! 'THASSERT 'THSUCCEED 'thassertt)
(putprop! 'THERASE 'THSUCCEED 'theraset)
(putprop! 'THCOND 'THSUCCEED 'THCONDT)
(putprop! 'THOR 'THSUCCEED 'thort)
(putprop! 'THDO 'THSUCCEED 'THDOB)
(putprop! 'THUNDO 'THSUCCEED 'thundot)
(putprop! 'THMESSAGE 'THSUCCEED 'thmessaget)
(putprop! 'THREMBIND 'THSUCCEED 'thrembindt)

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
    (lambda []
        (terpri)
        (pr (cadar *thtree*))
        (print " FAILED ")
        (evlis (cddar *thtree*))
        (thpopt)
        nil))

(putprop! 'THTRACES 'THSUCCEED
    (lambda []
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
    (lambda [x b]
        (THPUSH *thtree* (list 'THTRACES (gensym 'B) (and b '(ert nil))))
        (terpri)
        (print "PASSING BKPT ")
        (pr (cadar *thtree*))
        (print \space)
        ;; BY SETTING THBRANCH AND THABRANCH, A TRIPLE IS CREATED BY THVAL FOR BACKTRACKING.
        ;; THEN, THE TREE IS POPPED TO PREVENT THTRACES FROM TYPING OUT THE MEANINGLESS
        ;; THAT THE BREAKPOINT SUCCEEDED.
        (set! *thbranch* *thtree*)
        (set! *thabranch* *thalist*)
        (thpopt)
        (pr x)))

(putprop! 'THGOAL 'THTRACE
    (lambda [x b]
        (THPUSH *thtree* (list 'THTRACES (gensym 'G) '(and *thvalue* (pr *thvalue*)) (and b '(ert nil))))
        (terpri)
        (print "TRYING GOAL ")
        (pr (cadar *thtree*))
        (print \space)
        (pr x)))

(putprop! 'THEOREM 'THTRACE
    (lambda [x b]
        (THPUSH *thtree* (list 'THTRACES x '(and *thvalue* (pr *thvalue*)) (and b '(ert nil))))
        (terpri)
        (print "ENTERING THEOREM ")
        (pr x)))

(putprop! 'THASSERT 'THTRACE
    (lambda [x b]
        (THPUSH *thtree* (list 'THTRACES (gensym 'A) (and b '(ert nil))))
        (terpri)
        (print "ASSERTING ")
        (pr (cadar *thtree*))
        (print \space)
        (pr x)))

(putprop! 'THERASE 'THTRACE
    (lambda [x b]
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
    '(THCONSE (x y z yy zz)
        (($? x) ($? y) ($? z))
        (THGOAL (!CHOOSE ($? y) ($_ yy) ($E (getprop ($? x) 'CHOOSE)))
            (THUSE TC-CHOOSE))
        (THGOAL (!CHOOSE ($? z) ($_ zz) ($E (getprop ($? x) 'CHOOSE2)))
            (THUSE TC-CHOOSE))
        (THGOAL (($? x) ($? yy) ($? zz)) (THTBF thtrue))))

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
    '(THCONSE (x y (WHY (ev)) EV)
        (!CLEARTOP ($? x))
        (term? ($? x))
        (THOR (THGOAL (!SUPPORT ($? x) ?))
        (THAND (THASSERT (!CLEARTOP ($? x))) (THSUCCEED THEOREM)))
        (memory)
    =>  (THCOND ((THGOAL (!SUPPORT ($? x) ($_ y)))
            (THGOAL (!GET-RID-OF ($? y))
                (THNODB)
                (THUSE TC-GET-RID-OF))
            (THGO =>))
            ((THASSERT (!CLEARTOP ($? x)))
            (MEMOREND (!CLEARTOP ($? EV) ($? x)))
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
    '(THCONSE (x y (WHY (ev)) EV)
        (!GET-RID-OF ($? x))
        (or nomem? (THVSETQ ($_ EV) ($? WHY)))
    =>  (THCOND ((nil? ($? x)))
            ((term? ($? x))
                (memory)
                (THGOAL (!FINDSPACE ßTABLE ($E (size ($? x))) ($? x) ($_ y)) (THUSE TC-FINDSPACE))
                (THGOAL (!PUT ($? x) ($? y)) (THNODB) (THUSE TC-PUT))
                (MEMOREND (!GET-RID-OF ($? EV) ($? x))))
            ((THGOAL (!GET-RID-OF ($E (car ($? x)))) (THUSE TC-GET-RID-OF))
                (or (THSETQ ($_ x) (cdr ($? x))) (THSUCCEED THEOREM))
                (THGO =>)))))

(putprop! 'TC-GRASP 'THEOREM
    '(THCONSE (x y (WHY (ev)) EV)
        (!GRASP ($? x))
        (THCOND ((THGOAL (!GRASPING ($? x))) (THSUCCEED THEOREM))
            ((term? ($? x))))
        (memory)
        (THGOAL (!CLEARTOP ($? x)) (THUSE TC-CLEARTOP))
        (THCOND ((THGOAL (!GRASPING ($_ y)))
            (THOR (THGOAL (!UNGRASP) (THNODB) (THUSE TC-UNGRASP))
                (THGOAL (!GET-RID-OF ($? y)) (THNODB) (THUSE TC-GET-RID-OF))))
            ((THSUCCEED)))
        (THSETQ ($_ y) (topcenter ($? x)))
        (THGOAL (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2))
        (THASSERT (!GRASPING ($? x)))
        (MEMOREND (!GRASP ($? EV) ($? x)))
        (or nomem?
            (THSETQ *grasplist* (cons (list *thtime* ($? x)) *grasplist*)))
        (THCOND (doit? (THOR (GRASP ($? x)) (and (UNGRASP) nil)))
            ((THSETQ *plan* (cons (list 'GRASP (list 'quote ($? x))) *plan*))))))

(putprop! 'TC-LOC 'THEOREM
    '(THCONSE (x y z loc)
        (($? loc) ($? x) ($? y) ($? z))
        (THOR (THGOAL (!MANIP ($? y))) (THGOAL (!IS ($? y) !BOX)))
        (THOR (THGOAL (!MANIP ($? z))) (THGOAL (!IS ($? z) !BOX)))
        (not (= ($? y) ($? z)))
        (locgreater LOC ($? y) ($? z)
            ((lambda [x] (COND ((= x '!RIGHT) #'car) ((= x '!BEHIND) #'cadr) ((= x '!ABOVE) #'caddr) ((ert "TC-LOC")))) ($? x)))))

(putprop! 'TC-MAKESPACE 'THEOREM
    '(THCONSE (surf size obj space x (WHY (ev)) EV)
        (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space))
        (THNOT (THGOAL (!IS ($? surf) !BOX)))
        (memory)
    => (THAND (THGOAL (!SUPPORT ($? surf) ($_ x)))
            (THGOAL (!GET-RID-OF ($? x)) (THUSE TC-GET-RID-OF)))
        (THOR (THGOAL (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space)) (THUSE TC-FINDSPACE))
            (THGO =>))
        (MEMOREND (!MAKESPACE ($? EV) ($? surf)))))

(putprop! 'TC-MORE 'THEOREM
    '(THCONSE (measure x y)
        (!MORE ($? measure) ($? x) ($? y))
        (THVSETQ ($_ measure) (getprop ($? measure) 'MEASFN))
        (> (($? measure) ($? x)) (($? measure) ($? y)))))

(putprop! 'TC-MOVEHAND 'THEOREM
    '(THCONSE (x y w z)
        (!MOVEHAND ($? y))
        (THCOND
            ((= ($? y) *handat*) (THSUCCEED THEOREM))
            ((THGOAL (!GRASPING ($? x)))
                (THVSETQ ($_ z) (let [x (atab ($? x)) y (diff ($? y) (tcent '(0 0 0) (caddr x)))]
                    (and (clear y (list (caaddr x) (cadadr (cdr x)) (- 512 (caddr y))) (car x))
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
        (COND ((= ($? y) *handat*) (THSUCCEED THEOREM))
            ((and (<= 32 (car ($? y)) 608) (<= 0 (cadr ($? y)) 608) (<= 0 (caddr ($? y)) 512))))
        (THVSETQ ($_ loc) *handat*)
        (THSETQ *handat* ($? y))
        (THSETQ *thtime* (inc *thtime*))
        (THCOND (doit? (THOR (eval (cons 'MOVETO *handat*))
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
                    (THCOND ((THVSETQ ($_ y) (or (packon ($? x) ($? PYR)) (packon ($? x) ($? BLOCKS))))
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
    '(THCONSE (x (WHY (ev)) EV)
        (!PICKUP ($? x))
        (memory)
        (THGOAL (!GRASP ($? x)) (THUSE TC-GRASP))
        (THGOAL (!RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
        (MEMOREND (!PICKUP ($? EV) ($? x)))))

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
        (clear ($? y) (size ($? x)) ($? x))
        (support ($? y) (size ($? x)) ($? x))
        (THGOAL (!GRASP ($? x)) (THUSE TC-GRASP))
        (THSETQ ($_ z) (tcent ($? y) (size ($? x))))
        (THGOAL (!MOVEHAND ($? z)) (THNODB) (THUSE TC-MOVEHAND))
        (THGOAL (!UNGRASP) (THNODB) (THUSE TC-UNGRASP))))

(putprop! 'TC-PUTIN 'THEOREM
    '(THCONSE (x y z (WHY (ev)) EV)
        (!PUTIN ($? x) ($? y))
        (memory)
        (THCOND ((THGOAL (!PUTON ($? x) ($? y)) (THUSE TC-PUTON))
                (MEMOREND (!PUTIN ($? EV) ($? x) ($? y)))
                (THSUCCEED THEOREM))
            ((THSUCCEED)))
        (THGOAL (!IS ($? y) !BOX))
        (THVSETQ ($_ z)
            (union (listify ($? x))
                (THVAL '(THFIND ALL ($? w) (w) (THGOAL (!ON ($? w) ($? y)))) *thalist*)))
        (THGOAL (!CLEARTOP ($? y)) (THUSE TC-CLEARTOP))
        (THGOAL (!PACK ($? z) ($? y)) (THUSE TC-PACK))
        (MEMOREND (!PUTIN ($? EV) ($? x) ($? y)))))

(putprop! 'TC-PUTON 'THEOREM
    '(THCONSE (x y z (WHY (ev)) EV)
        (!PUTON ($? x) ($? y))
        (term? ($? y))
        (or (cdr ($? x)) (THSETQ ($_ x) (car ($? x))))
        (not (COND ((term? ($? x)) (= ($? x) ($? y))) ((memq ($? y) ($? x)))))
        (memory)
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
        (MEMOREND (!PUTON ($? EV) ($? x) ($? y)))))

(putprop! 'TC-RAISEHAND 'THEOREM
    '(THCONSE ((WHY (ev)) EV)
        (!RAISEHAND)
        (memory)
        (THGOAL (!MOVEHAND ($E (list (car *handat*) (cadr *handat*) 512))) (THNODB) (THUSE TC-MOVEHAND))
        (MEMOREND (!RAISEHAND ($? EV)))))

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
    '(THCONSE (x y BLOCKS PYR (WHY (ev)) EV)
        (!STACKUP ($? x))
        (or (< (reduce + (map #(caddr (size %)) ($? x))) 641)
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
        (memory)
    =>  (THCOND
            ((cdr ($? BLOCKS))
                (THGOAL (!PUTON ($E (cadr ($? BLOCKS))) ($E (car ($? BLOCKS)))) (THUSE TC-PUTON))
                (THSETQ ($_ BLOCKS) (cdr ($? BLOCKS)))
                (THGO =>))
            (($? PYR) (THGOAL (!PUTON ($E (car ($? PYR))) ($E (car ($? BLOCKS)))) (THUSE TC-PUTON)))
            ((MEMOREND (!STACKUP ($? EV) ($? x)))))))

(putprop! 'TC-STARTEND3 'THEOREM
    '(THCONSE (x y EV TIME) (($? x) ($? EV) ($? TIME)) (THGOAL (($? x) ($? y) ($? EV) ($? TIME)) (THUSE TC-STARTEND4))))

(putprop! 'TC-STARTEND4 'THEOREM
    '(THCONSE (x newev z EV TIME)
        (($? x) ($? newev) ($? EV) ($? TIME))
        (or (and (THASVAL ($? x)) (THASVAL ($? EV)) (THASVAL ($? TIME)) (not (THASVAL ($? newev)))) (ert "TC-STARTEND4"))
        (THGOAL (!CHOOSE ($? EV) ($_ z) nil) (THUSE TC-CHOOSE))
        (or (term? ($? z)) (ert "TC-STARTEND4 ATOM"))
        (THSETQ ($_ newev) (gensym 'EV))
        (putprop! ($? newev) 'END
            (putprop! ($? newev) 'START
                (getprop ($? z) (COND ((= ($? x) '!START) 'START) ((= ($? x) '!END) 'END) ((ert "TC-STARTEND (THV x)"))))))
        (timechk ($? newev) ($? TIME))
        (putprop! ($? newev) 'WHY ($? z))
        (putprop! ($? newev) 'TYPE '!START)))

(putprop! 'TC-UNGRASP 'THEOREM
    '(THCONSE (x OBJ (who (ev)) EV)
        (!UNGRASP)
        (THCOND ((THGOAL (!GRASPING ($? x)))
                (memory)
                (THGOAL (!SUPPORT ? ($? x)))
                (THERASE (!GRASPING ($? x)))
                (MEMOREND (!UNGRASP ($? EV) ($? x)))
                (THSETQ *thtime* (inc *thtime*))
                (THCOND (doit? (THOR (UNGRASP) (and (GRASP ($? x)) nil)))
                    ((THSETQ *plan* (cons '(UNGRASP) *plan*)))))
            ((THSUCCEED)))))

(putprop! 'TC-WANT4 'THEOREM
    '(THCONSE (x EV TIME y)
        (!WANT ($? x) ($? EV) ($? TIME))
        (THGOAL (!WANT ($? y) ($? x) ($? EV) ($? TIME)) (THUSE TC-WANT5))))

(putprop! 'TC-WANT5 'THEOREM
    '(THCONSE (x newev EV TIME z)
        (!WANT ($? newev) ($? x) ($? EV) ($? TIME))
        (or (and (THASVAL ($? x)) (THASVAL ($? EV)) (THASVAL ($? TIME))) (ert "TC-WANT5 THASVAL"))
        (= ($? x) 'ßFRIEND)
        (= (getprop ($? EV) 'WHY) 'COMMAND)
        (THSETQ ($_ newev) (gensym 'EV))
        (putprop! ($? newev) 'END
            (putprop! ($? newev) 'START
                (getprop ($? EV) 'START)))
        (timechk ($? newev) ($? TIME))
        (putprop! ($? newev) 'TYPE '!TELL)
        (putprop! ($? newev) 'WHY 'ESP)))

(putprop! 'TCT-EXISTS 'THEOREM
    '(THCONSE nil (!EXISTS ? ?) (THSUCCEED)))

(putprop! 'TCT-PICKUP 'THEOREM
    '(THCONSE (x EV TIME)
        (!PICKUP ($? x) ($? TIME))
        (THOR (THAND (THGOAL (!PICKUP ($? EV) ($? x))) (timechk ($? EV) ($? TIME)))
            (THGOAL (!PICKUP ($? EV) ($? x) ($? TIME)) (THUSE TCTE-PICKUP)))))

(putprop! 'TCT-PUT 'THEOREM
    '(THCONSE (x EV TIME y)
        (!PUT ($? x) ($? y) ($? TIME))
        (THGOAL (!PUT ($? EV) ($? x) ($? y) ($? TIME)) (THUSE TCTE-PUT))))

(putprop! 'TCT-AT 'THEOREM
    '(THCONSE (x y z TIME w)
        (!AT ($? y) ($? z) ($? TIME))
        (THOR (THGOAL (!MANIP ($? y)))
            (THAND (THGOAL (!IS ($? y) !BOX)) (THGOAL (!AT ($? y) ($? z))) (THSUCCEED THEOREM)))
        (THSETQ ($_ x) (tfind ($? y) ($? TIME)))
        (THOR (THSETQ ($_ w) (car ($? x)))
            (THAND (THAMONG ($? w) (cdr ($? x))) (or (not (< (car ($? w)) (or (start? ($? TIME)) -1))) (THFAIL THAND))))
        (THSETQ ($? z) (cadr ($? w)))))

(putprop! 'TCT-LOC 'THEOREM
    '(THCONSE (yy zz x y z TIME)
        (!LOC ($? x) ($? y) ($? z) ($? TIME))
        (THGOAL (!AT ($? y) ($? yy) ($? TIME)) (THUSE TCT-AT))
        (THGOAL (!AT ($? z) ($? zz) ($? TIME)) (THUSE TCT-AT))
        (THGOAL (!TLOC ($? x) ($? y) ($? z)) (THUSE TC-LOC))))

(putprop! 'TCT-SUPPORT 'THEOREM
    '(THCONSE (x y z TIME)
        (!SUPPORT ($? x) ($? y) ($? TIME))
        (THOR (THGOAL (!MANIP ($? y))) (THGOAL (!IS ($? y) !BOX)))
        (THAMONG ($? z) (tfind ($? y) ($? TIME)))
        (not (< (car ($? z)) (or (start? ($? TIME)) -1)))
        (THAMONG ($? x) (list (caddr ($? z))))))

(putprop! 'TCT-2 'THEOREM
    '(THCONSE (x EV TIME) (($? x) ($? TIME)) (THGOAL (($? x) ($? EV) ($? TIME)) (THUSE TCTE-3))))

(putprop! 'TCT-3 'THEOREM
    '(THCONSE (x y EV TIME) (($? x) ($? y) ($? TIME)) (THGOAL (($? x) ($? EV) ($? y) ($? TIME)) (THUSE TCTE-4))))

(putprop! 'TCT-4 'THEOREM
    '(THCONSE (x y z EV TIME) (($? x) ($? y) ($? z) ($? TIME)) (THGOAL (($? x) ($? EV) ($? y) ($? z) ($? TIME)) (THUSE TCTE-5))))

(putprop! 'TCTE-PICKUP 'THEOREM
    '(THCONSE (x EV EVENT TIME)
        (!PICKUP ($? EV) ($? x) ($? TIME))
        (THOR (THAND (THGOAL (!PICKUP ($? EV) ($? x))) (timechk ($? EV) ($? TIME)) (THSUCCEED THEOREM))
            (THSUCCEED))
        (THAMONG ($? EVENT) *eventlist*)
        (memq (getprop ($? EVENT) 'TYPE) '(!PUTON !GET-RID-OF))
        (timechk ($? EVENT) ($? TIME))
        (THOR (THGOAL (!PUTON ($? EVENT) ($? x) ?))
            (THGOAL (!GET-RID-OF ($? EVENT) ($? x))))
        (THVSETQ ($_ EV) (gensym 'E))
        (and (putprop! ($? EV) 'END (putprop! ($? EV) 'START (getprop ($? EVENT) 'END)))
            (putprop! ($? EV) 'TYPE '!PICKUP)
            (putprop! ($? EV) 'WHY ($? EVENT))
            (set! *eventlist* (cons ($? EV) *eventlist*))
            (THASSERT (!PICKUP ($? EV) ($? x))))))

(putprop! 'TCTE-PUT 'THEOREM
    '(THCONSE (x y EV EVENT TIME z)
        (!PUT ($? EV) ($? x) ($? y) ($? TIME))
        (THAMONG ($? EVENT) *eventlist*)
        (memq (getprop ($? EVENT) 'TYPE) '(!PICKUP !PUTON))
        (timechk ($? EVENT) ($? TIME))
        (THOR (THGOAL (!PUTON ($? EVENT) ($? x) ?))
            (THGOAL (!PICKUP ($? EVENT) ($? x))))
        (or (THVSETQ ($_ z) (dec (assq (getprop ($? EVENT) 'END) (getprop ($? x) 'HISTORY))))
            (ert "TCTE-PUT WRONG"))
        (THAMONG ($? y) (list (cadr ($? z))))
        (THSETQ ($_ EV) (gensym 'E))
        (and (putprop! ($? EV) 'END (putprop! ($? EV) 'START (car ($? z))))
            (putprop! ($? EV) 'WHY ($? EVENT))
            (putprop! ($? EV) 'TYPE '!PUT)
            (set! *eventlist* (cons ($? EV) *eventlist*))
            (THASSERT (!PUT ($? EV) ($? x) ($? y))))))

(putprop! 'TCTE-3 'THEOREM
    '(THCONSE (x EV TIME)
        (($? x) ($? EV) ($? TIME))
        (or (THASVAL TIME) (ert "TCTE-3"))
        (THGOAL (($? x) ($? EV)))
        (timechk ($? EV) ($? TIME))))

(putprop! 'TCTE-4 'THEOREM
    '(THCONSE (x y EV TIME)
        (($? x) ($? EV) ($? y) ($? TIME))
        (or (THASVAL ($? TIME)) (ert "TCTE-4"))
        (THGOAL (($? x) ($? EV) ($? y)))
        (timechk ($? EV) ($? TIME))))

(putprop! 'TCTE-5 'THEOREM
    '(THCONSE (x y z EV TIME)
        (($? x) ($? EV) ($? y) ($? z) ($? TIME))
        (or (THASVAL ($? TIME)) (ert "TCT-5"))
        (THGOAL (($? x) ($? EV) ($? y) ($? z)))
        (timechk ($? EV) ($? TIME))))

(putprop! 'TCT-GRASP 'THEOREM
    '(THCONSE (x z TIME)
        (!GRASP ($? x) ($? TIME))
        (THVSETQ ($_ z) (endtime *grasplist* ($? TIME)))
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

(dorun (map #(THADD % nil)
    '(TC-CALL TC-CLEARTOP TC-GET-RID-OF TC-GRASP TC-NAME TC-NOTICE TC-PACK TC-PICKUP TC-PUTIN TC-PUTON TC-RAISEHAND TC-STACKUP TC-UNGRASP TC-ON TC-PHYSOB)))

#_(ns shrdlu.data)

;; ###################################################################
;;
;;      DATA > INITIAL MICROPLANNER DATABASE FOR THE BLOCKS WORLD
;;
;; ###################################################################

(def- doit? true)

(set! *handat* '(32 0 0))

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

(dorun (map #(THADD % nil) '(
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

(defn- clear [loc size obj]
    (let [obj (listify obj)]
        (when (every? true? (map #(<= 0 %1 (- 640 %2)) loc size))
            (loop-when [a ATABLE] a => loc
                (cond (memq (caar a) obj) (recur (cdr a))
                    (let [x1 (cadar a) x2 (caddar a)]
                        (and (< (car loc) (+ (car x1) (car x2))) (< (car x1) (+ (car loc) (car size)))
                            (< (cadr loc) (+ (cadr x1) (cadr x2))) (< (cadr x1) (+ (cadr loc) (cadr size)))
                            (< (caddr loc) (+ (caddr x1) (caddr x2))) (< (caddr x1) (+ (caddr loc) (caddr size)))))
                        nil
                    :else (recur (cdr a)))))))

(defn- diff [x y] (doall (map - x y)))

(defn- half [x] (/ x 2))

(defn- endtime [l time]
    (let-when [e (end? time)] e => l
        (loop-when-recur l (and l (< e (caar l))) (cdr l) => l)))

(defn- ev [] (or nomem? ($? EV)))

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
                (COND ((clear (SETQ v
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

(defn- goal [& a]
    (binding [*plan* nil]
        (THVAL (list 'THGOAL (car a) '(THTBF thtrue)) '((EV COMMAND)))
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
                    (dorun (map (lambda [x]
                        (let [xx nil yy nil]
                            (COND ((or (memq (car x) obj)
                                    (not (< (caadr x) (car _max)))
                                    (not (< (cadadr x) (cadr _max)))
                                    (not (> (SETQ xx (+ (caadr x) (caaddr x))) (car _min)))
                                    (not (> (SETQ yy (+ (cadadr x) (cadr (caddr x)))) (cadr _min)))
                                    (not (> (+ (caddr (cadr x)) (caddr (caddr x))) (caddr loc))))
                                (RETURN nil))
                            ((> (caadr x) (car loc))
                                (SETQ XH (order (list (- (caadr x) (car loc)) (car x)) XH)))
                            ((< xx (car loc))
                                (SETQ XL (order (list (- (car loc) xx) (car x)) XL)))
                            ((SETQ XO (cons (car x) XO))))
                            (COND ((> (cadadr x) (cadr loc))
                                (SETQ YH (order (list (- (cadadr x) (cadr loc)) (car x)) YH)))
                            ((< yy (cadr loc))
                                (SETQ YL (order (list (- (cadr loc) yy) (car x)) YL)))
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
            (:else (dorun (map (lambda [y z w] (let [x (eval w)]
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

(defn- locgreater [loc y z fun]
    (let [loc- #(let [a (atab %2)] (if (= ($? loc) '!LOC) a (list* nil ($? %1) (cddr a))))
          y (loc- 'yy y) z (loc- 'zz z)]
        (not (< (fun (cadr y)) (+ (fun (cadr z)) (fun (caddr z)))))))

(§ defq- MEMOREND [a]
    (or nomem?
        (and (putprop! ($? EV) 'END *thtime*)
            (APPLY #'THASSERT (list (THVARSUBST (car a) nil)))
            (putprop! ($? EV) 'TYPE (caar a)))))

(defn- memory []
    (or nomem?
        (THAND (THVSETQ ($_ EV) (gensym 'E))
            (THSETQ *eventlist* (cons ($? EV) *eventlist*))
            (putprop! ($? EV) 'START *thtime*)
            (putprop! ($? EV) 'WHY ($? WHY)))))

(defn- occupier [x y z]
    (if (neg? z) 'ßTABLE
        (loop-when [a ATABLE] a
            (let [x1 (cadar a) x2 (caddar a)]
                (if (and (< (dec (car x1)) x (+ (car x1) (car x2))) (< (dec (cadr x1)) y (+ (cadr x1) (cadr x2))) (< (dec (caddr x1)) z (+ (caddr x1) (caddr x2))))
                    (caar a)
                    (recur (cdr a)))))))

(defn- order [x y]
    (cond (nil? y) (list x)
        (> (car x) (caar y)) (cons (car y) (order x (cdr y)))
        :else (cons x y)))

(§ defn- PACKO [obj TYPE]
    (let [xx nil]
        (dorun (map (lambda [x]
            (and (THVAL '(THGOAL (!IS ($? x) ($E TYPE))) (list (list 'X x))) (SETQ xx (packord x (size x) xx))))
        obj))
        (doall (map cadr xx))))

(defn- packon [surf l]
    (let [surf (atab surf)]
        (loop-when l l
            (let [x (size (car l))]
                (if (or (< (caaddr surf) (car x)) (< (cadr (caddr surf)) (cadr x)) (< 321 (+ (caddr x) (caddr (cadr surf)) (caddr (caddr surf)))))
                    (recur (cdr l))
                    (car x))))))

(defn- packord [x size l]
    (cond (nil? l) (list (list size x))
        (or (> (caaar l) (car size)) (and (= (car size) (caaar l)) (> (cadaar l) (cadr size))))
            (cons (car l) (packord x size (cdr l)))
        :else (cons (list size x) l)))

(defn- size [x]
    (cond (= x 'ßBOX) '(256 256 192) (= x 'ßTABLE) '(640 640 640) (term? x) (caddr (atab x)) :else x))

(defn- starthistory []
    (set! *grasplist* nil)
    (putprop! 'EE 'WHY 'COMMAND)
    (putprop! 'EE 'START 0)
    (putprop! 'EE 'END 0)
    (putprop! 'EE 'TYPE '!START)
    (set! *eventlist* '(EE))
    (THADD '(!START EE ßDIALOG) nil)
    (dorun (map #(when (getprop (car %) 'THASSERTION)
            (putprop! (car %) 'HISTORY (list (list 0 (cadr %) (cadar (THVAL '(THGOAL (!SUPPORT ($? x) ($? y))) (list (list 'x 'THUNASSIGNED) (list 'y (car %)))))))))
        ATABLE)))

(defn- startime [l time]
    (< (caar l) (or (start? time) -1)))

(defn- support [loc size x]
    (if (zero? (caddr loc)) 'ßTABLE
        (let [loc (occupier (+ (car loc) (half (car size))) (+ (cadr loc) (half (cadr size))) (dec (caddr loc)))]
            (when (and loc (not= loc x)) loc))))

(defn- tcent [x1 x2]
    (list (+ (car x1) (half (car x2))) (+ (cadr x1) (half (cadr x2))) (+ (caddr x1) (caddr x2))))

(defn- tfind [obj time]
    (loop-when [l (getprop obj 'HISTORY)] l
        (if (<= (caar l) (or (end? time) 32767)) l
            (recur (cdr l)))))

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

(defn- forget []
    (set! *lastsentno* 0)
    (set! *lastsent* nil)
    (set! *lastrel* nil)
    (set! *backref* nil)
    (set! *backref2* nil)
    (dorun (map (lambda [x] (dorun (map (lambda [y] (remprop! x y)) '(BIND LASTBIND)))) '(IT THEY ONE)))
    (when *eventlist* (THFLUSH HISTORY) (starthistory)))

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

(defn- combination? [& words]
    ;; THIS FUNCTION CHECKS TO SEE IF THE WORDS PASSED AS ARGS FORM
    ;; A COMBINATION SUCH AS "STACK-UP" OR "ON-TOP-OF" COMBINATIONS
    ;; ARE IN THE DICTIONARY AS A SINGLE ATOM COMPOSED OF THE WORDS
    ;; IN THE COMBINATION SEPARATED BY DASHES ALL COMBINATIONS HAVE
    ;; THE FEATURE "COMBINATION" AND HAVE A ROOT WHICH IS A LIST OF
    ;; THE WORDS IN THE COMBINATION
    (let [a nil]
        (dorun (map #(SETQ a (concat a (cons '- (EXPLODE %)))) words))
        (SETQ a (list (INTERN (MAKNAM (cdr a)))))
        (when (isq a 'COMBINATION) a)))

(defn- findb [x y]
    (cond (nil? x) nil (= (cdr x) y) x :else (recur (cdr x) y)))

(defn- from [a b]
    (when (and a (not= a b)) (cons (word a) (from (cdr a) b))))

(defn- meet [a b]
    ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
    (loop-when [s nil a a] a => (seq (reverse s))
        (recur (if (memq (car a) b) (cons (car a) s) s) (cdr a))))

(defn- mod [a b] (union (setdif a (cadr b)) (car b)))

(defn- setdif [a b]
    (loop-when [s nil a a] a => (seq (reverse s))
        (recur (if (memq (car a) b) s (cons (car a) s)) (cdr a))))

(defn- sta [a b]
    (loop [a a b b] b => true
        (when (and a (= (car a) (car b))) (recur (cdr a) (cdr b)))))

(defn- union [a b]
    (loop-when [a (reverse a) b b] b => (seq (reverse a))
        (recur (if (memq (car b) a) a (cons (car b) a)) (cdr b))))

(def- CHRCT 76)

(§ defn- TAB [n]
    (let [P nil]
        (when (> n LINEL) (RETURN '<TAB>))
    =>  (SETQ P (- LINEL CHRCT))
        (when (not (> n P)) (RETURN '<TAB>))
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
                (buildword WRD (mod (getprop (car x) 'FEATURES) (cdr x)) (semantics x) (car x)))
            ((= (car (LAST WORD)) '=)
                (buildword WRD (COND ((memq '\" WORD) '(PROPN NS POSS)) ('(PROPN NS))) '((PROPN T)) nil)) ;; "sic!
            ((GO CUT)))
        (GO WRD)

        ;; --------------------------------------------
        ;;               MORPHOLOGY CODE
        ;; --------------------------------------------

    CUT (COND
            ((sta WORD '(T \' N)) (SETQ RD (cdddr WORD)) (SETQ WORD (cons '* WORD)) (GO TRY))
            ((sta WORD '(S \')) (SETQ WORD (cddr WORD)) (SETQ POSS WRD) (GO WORD))
            ((sta WORD '(\')) (SETQ WORD (cdr WORD)) (SETQ POSS WRD) (GO WORD))
            ((sta WORD '(Y L)) (SETQ RD (cddr WORD)) (GO LY))
            ((sta WORD '(G N I)) (SETQ RD (cdddr WORD)))
            ((sta WORD '(D E)) (SETQ RD (cddr WORD)))
            ((sta WORD '(N E)) (SETQ RD (cddr WORD)))
            ((sta WORD '(R E)) (SETQ RD (cddr WORD)))
            ((sta WORD '(T S E)) (SETQ RD (cdddr WORD)))
            ((sta WORD '(S)) (SETQ RD (cdr WORD)) (GO SIB))
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
                        (SETQ FEATURES (mod (getprop (SETQ ROOT (car x)) 'FEATURES) (cdr x)))))
                (buildword WRD (mod FEATURES (getprop (car WORD) 'MOD)) (getprop ROOT 'SEMANTICS) ROOT))
            ((= (car RD) 'E) (SETQ RD (cdr RD)) (GO TRY))
            ((GO NO)))

        ;; -------------------------------------------------------
        ;;   BUILD UP THE PROCESSED LIST OF WORDS TO BE RETURNED
        ;; -------------------------------------------------------

    WRD (set! *sent*
            (COND (POSS
                (COND ((or (memq 'NOUN (SETQ FEATURES (getprop WRD 'FEATURES))) (memq 'PROPN FEATURES))
                        ;; IF IT'S A NOUN OR A PROPER NOUN, MARK IT AS POSSESSIVE
                        (buildword POSS (concat (meet FEATURES (getprop 'POSS 'ELIM)) '(POSS)) (getprop WRD 'SEMANTICS) ROOT)
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
            (buildword WRD '(NOUN NS) '((NOUN (SMNEWNOUN)) (PROPN (smnewpropn))) WRD)
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

(putprop! 'TELLABLE 'TELL (lambda [x]
    (APPLY #'TELLABLE
        (list (charg x "CONCEPT:" "ANY PLANNER GOAL PATTERN BEGINNING WITH THIS CONCEPT NAME CAN BE ACCEPTED BY THE SYSTEM AS NEW INFORMATION -- BEWARE OF INTERACTIONS WITH SPECIAL HACKS FOR LOCATION, ETC.")))))

(defn- pev [ev col top]
    (terpri)
    (TAB col)
    (print ev (getprop ev 'TYPE) "TIME:" (getprop ev 'START) "TO" (getprop ev 'END))
    (when top
        (_print "REASON:" (getprop ev 'WHY)))
    (dorun (map #(and (= ev (getprop % 'WHY)) (pev % (+ col 8) nil))
        (reverse *eventlist*))))

(putprop! 'EVENT 'SHOW (lambda [x]
    (let [x (charg x "EVENT:" "EVENT TO BE DISPLAYED -- <LF> FOR ENTIRE EVENT LIST")]
        (if x (pev x 0 true) (dorun (map #(when (= (getprop % 'WHY) 'COMMAND) (pev % 0 true)) (reverse *eventlist*)))))))

(§ defn- SHOWTELLCHOICE [systems info action]
    (SETQ NODE (QUERY "WHICH OPTION?" (let [_ (getprop NODE systems)] (terpri) (pr _) _) (getprop NODE info)))
    (APPLY (getprop NODE action) (list (list NODE))))

(§ defn- TELLCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE SYSTEMS INFO ACTION))

(§ defn- SHOWCHOICE [NODE] (SETQ NODE (car NODE)) (SHOWTELLCHOICE SYSTEMS INFO ACTION))

(§ defn- SHOWTELL [a NODE SYSTEMS INFO ACTION]
    (COND ((nil? a) (SHOWTELLCHOICE SYSTEMS INFO ACTION))
        ((getprop (car a) ACTION)
            (APPLY (getprop (car a) ACTION) (list a)))
        (:else
            (terpri)
            (print "I DON'T KNOW HOW TO" ACTION (car a))))
    '*)

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
        (dorun (map (lambda [x y]
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
    READ (cond
            (memq (ascii (TYIPEEK)) breakchars) (do (READCH) (GO READ))
            (== (TYIPEEK) 10) (do (READCH) (RETURN nil))
            (= (ascii (TYIPEEK)) '?) (do (READCH) (terpri) (print (or help "NO INFORMATION AVAILABLE")) (GO TOP))
            (= (SETQ x (READ)) 'QUIT) (ERR nil)
            :else (RETURN x))
    nil))

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

(putprop! 'CANSHOW 'SHOW #'SHOWCHOICE)
(putprop! 'CANSHOW 'SHOWTREE '(SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT))
(putprop! 'CANSHOW 'SHOWINFO "THINGS WHICH CAN BE DISPLAYED")

(putprop! 'CANTELL 'TELL #'TELLCHOICE)
(putprop! 'CANTELL 'TELLTREE '(LISP PLANNER PARSING DEFINITIONS SEMANTICS))
(putprop! 'CANTELL 'TELLINFO "THINGS WHICH CAN BE SET TO CONTROL HOW THE SYSTEM RUNS")

(putprop! 'SHOW 'SHOW (lambda [_] (treeprint 'CANSHOW 'SHOWTREE 0)))

(putprop! 'TELL 'SHOW (lambda [_] (treeprint 'CANTELL 'TELLTREE 0)))

(putprop! 'LISP 'SHOW #'SHOWCHOICE)
(putprop! 'LISP 'SHOWTREE '(PROPERTY FUNCTION VALUE))
(putprop! 'LISP 'TELL #'TELLCHOICE)
(putprop! 'LISP 'TELLTREE '(FUNCTION))

(putprop! 'STOP 'TELL (lambda [x] (set! *plannersee* (and *plannersee* (if (onoff x "STOP AFTER SHOWING PLANNER INPUT?") true 'NOSTOP)))))

(putprop! 'PLANNER 'SHOWTREE '(ASSERTIONS THEOREM SCENE EVENT))
(putprop! 'PLANNER 'SHOW #'SHOWCHOICE)
(putprop! 'PLANNER 'TELLTREE '(INPUT ACTION THEOREM ASSERTIONS TELLABLE))
(putprop! 'PLANNER 'TELL (lambda [x]
            (cond (nil? (cdr x)) (TELLCHOICE x)
                (= (cadr x) 'ON)
                    (do (print (ascii 12)) (thtrace 'THEOREM 'THASSERT 'THERASE '(THGOAL true true)) (set! *plannersee* true))
                (= (cadr x) 'OFF)
                    (do (print (ascii 12)) (set! *plannersee* nil) (thuntrace))
                :else (TELLCHOICE x))))

(putprop! 'PARSING 'SHOWTREE '(NODE TREE))
(putprop! 'PARSING 'SHOW #'SHOWCHOICE)
(putprop! 'PARSING 'TELLTREE '(NODE LABEL))
(putprop! 'PARSING 'TELL (lambda [x]
            (cond (nil? (cdr x)) (TELLCHOICE x)
                (= (cadr x) 'ON)
                    (do (print (ascii 12)) (set! *labeltrace* true))
                (= (cadr x) 'OFF)
                    (do (print (ascii 12)) (set! *labeltrace* nil))
                :else (TELLCHOICE x))))

(putprop! 'DEFINITIONS 'SHOW #'SHOWCHOICE)
(putprop! 'DEFINITIONS 'SHOWTREE '(UNIT WORD MARKER))
(putprop! 'DEFINITIONS 'TELL #'TELLCHOICE)
(putprop! 'DEFINITIONS 'TELLTREE '(WORD MARKER))

(putprop! 'INPUT 'TELL #(set! *plannersee* (onoff % "TO SEE INPUT TO PLANNER")))
(putprop! 'INPUT 'SHOW #'SHOWCHOICE)
(putprop! 'INPUT 'SHOWTREE '(ALL REST CURRENT))

(putprop! 'RUN 'TELL #'TELLCHOICE)
(putprop! 'RUN 'TELLTREE '(STOP DO))
(putprop! 'RUN 'TELLINFO "PARAMETERS TO CONTROL WHAT SHRDLU DOES AS IT RUNS")

(putprop! 'ASSERTIONS 'TELL (lambda [x] (THVAL (list 'THASSERT (charg x "ASSERTION:" "PLANNER ASSERTION TO BE ADDED TO DATABASE") '(THTBF thtrue)) nil)))
(putprop! 'ASSERTIONS 'SHOW (lambda [x] (DA (charg x "ATOM:" "SHOW ALL ASSERTIONS WHICH CONTAIN THE GIVEN ATOM"))))

(putprop! 'THEOREM 'TELL (lambda [x]
            (let [x (if (cdr x) (cadr x) (gensym 'THEOREM))]
                (putprop! x 'THEOREM
                    (list (QUERY "WHICH THEOREM TYPE?" '(THANTE THERASING THCONSE) "ANTECEDENT, ERASING, OR CONSEQUENT THEOREM")
                        (listify (REQUEST "VARIABLE-LIST:" nil))
                        (REQUEST "PATTERN:" "A LIST ENCLOSED IN PARENS, LIKE (!IS ($? x) !ZOG)")
                        (REQUEST "BODY:" "LIST OF MICROPLANNER STATEMENTS")))
                (THADD x nil)
                (terpri)
                (pr x))))
(putprop! 'THEOREM 'SHOW (lambda [x] (DISP (getprop (charg x "THEOREM-NAME:" "PLANNER THEOREM WHOSE DEFINITION IS TO BE SHOWN") 'THEOREM))))

(defn- showmove [x]
    (apply #'MOVE-PT
        (listify (or x
            (REQUEST "NODE-SPECIFICATION:"
                "C MEANS CURRENT NODE, H IS MOST RECENTLY PARSED FOR OTHER POSSIBILITIES.  SEE THESIS SECTION ON POINTER-MOVING COMMANDS")))))

(putprop! 'NODE 'SHOW (lambda [x]
    (cond (getprop (cadr x) 'FEATURES) (DP (cadr x)) (do (set! *savept* *pt*) (showmove (cdr x))) (do (DP (car *pt*)) (set! *pt* *savept*)) :else (print "NO SUCH NODE"))))

(putprop! 'WORD 'SHOW (lambda [x] (DP (charg x "WORD:" "ENGLISH WORD IN THE VOCABULARY"))))
(putprop! 'WORD 'TELL (lambda [x] (DEFINE (charg x "WORD:" "ENGLISH WORD TO BE DEFINED -- MUST BE NOUN OR VERB"))))

(putprop! 'ACTION 'TELL (lambda [x]
            (let [x (cond (cdr x) (cond (= (cadr x) 'ON) nil (= x 'OFF) '(thuntrace) :else x)
                        (onoff x "WATCH PLANNER PROGRAMS STEP BY STEP?") nil
                        :else '(thuntrace))]
                (if x (thuntrace) (thtrace 'THEOREM 'THGOAL 'THASSERT 'THERASE)))))

(putprop! 'LABEL 'TELL (lambda [x]
            (let [x (if (cdr x) x (list (REQUEST "TYPE LIST OF LABELS, OR ON OR OFF:" "WATCHES PARSER GO PAST PROGRAM LABELS IN THE GRAMMAR")))]
                (set! *labeltrace* (if (= (car x) 'OFF) nil (car x))))))

(putprop! 'MARKER 'TELL (lambda [x]
            (let [x (charg x "MARKER:" "MARKER TO BE ADDED") y (REQUEST "PARENT:" "NODE TO WHICH IT IS ATTACHED IN THE TREE")]
                (putprop! x 'SYS (list y))
                (putprop! y 'SYSTEM (cons x (getprop y 'SYSTEM)))
                nil)))
(putprop! 'MARKER 'SHOW (lambda [x]
            (let [x (charg x "MARKER:" "SEMANTIC MARKER WHOSE SUBSETS ARE TO BE EXAMINED.  TYPE <LF> FOR ENTIRE TREE")]
                (treeprint (or x '!SYSTEMS) 'SYSTEM 0))))

(putprop! 'ALL 'SHOW (lambda [_] (%sent)))

(putprop! 'CURRENT 'SHOW (lambda [_] (terpri) (print (from *nb* *n*))))

(putprop! 'REST 'SHOW (lambda [_] (terpri) (print *n*)))

(putprop! 'SCENE 'SHOW (lambda [_]
            (binding [*plannersee* nil]
                (terpri)
                (print "CURRENT SCENE:")
                (terpri)
                (dorun (map (lambda [obj]
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

(§ defn- DEFINE [word]
    (let [*fe* nil TYPE nil MARK nil TR nil]
        (SETQ word (or word (REQUEST "WORD:" "ENGLISH WORD TO BE DEFINED")))
        (SETQ TYPE (QUERY "NOUN OR VERB?" '(NOUN VERB) "OTHER TYPES MUST BE DEFINED IN LISP"))
    =>  (or (SETQ MARK (REQUEST "MARKERS:" "LIST OF SEMANTIC MARKERS FOR WORD BEING DEFINED -- TO SEE MARKER TREE, TYPE <LF>"))
            (and (SHOW MARKER !SYSTEMS) (GO =>)))
        (SETQ MARK (listify MARK))
        (COND
            ((= TYPE 'NOUN)
                (putprop! word 'FEATURES '(NOUN NS))
                (putprop! word 'SEMANTICS
                    (list (list 'NOUN (list 'OBJECT (list
                        'MARKERSß MARK
                        'PROCEDUREß
                            (lis2fy (REQUEST "PROCEDURE:"
                                        "EXPRESSION OR LIST OF EXPRESSIONS TO BE PUT IN PLANNER GOALS TO DESCRIBE OBJECT -- USE *** TO REPRESENT OBJECT BEING DESCRIBED BY WORD -- E.G. (!IS *** !ZOG) OR ((!IS *** !ZOG) (!LOVE ßEVERYONE ***))")))))))
                (RETURN true))
            ((SETQ TR (= (QUERY "TRANSITIVE OR INTRANSITIVE?" '(TRANSITIVE INTRANSITIVE) nil) 'TRANSITIVE))
                (putprop! word 'FEATURES '(VB TRANS INF)))
            (:else (putprop! word 'FEATURES '(VB ITRNS INF))))
        (let [rs (list (list (listify (REQUEST "RESTRICTIONS ON SUBJECT:" "LIST OF SEMANTIC MARKERS"))))]
            (when TR
                (SETQ rs (concat rs (list (listify (REQUEST "RESTRICTIONS ON OBJECT:" "LIST OF SEMANTIC MARKERS"))))))
            (putprop! word 'SEMANTICS
                (list (list 'VB (list 'RELATION (list
                    'MARKERSß MARK
                    'RESTRICTIONSß rs
                    'PROCEDUREß
                        (lis2fy (REQUEST "PROCEDURE:"
                                    "LIST OF EXPRESSIONS TO BE PUT INTO PLANNER GOALS TO DESCRIBE ACTION OR RELATION -- USE !1 FOR SUBJECT, !2 FOR OBJECT.  E.G. (!SUPPORT !1 !2) OR ((!HAPPY !1) (!SMILING !1))")))))))
            true)))

(§ defn- HELP []
    (if (= (QUERY "TYPE L FOR LONG FORM (85 LINES) S FOR SHORT (16 LINES)" '(S L) nil) 'S) (UREAD MINIH DOC DSK LANG) (UREAD HELP DOC DSK LANG))
    (THRUTEXT)
    '*)

#_(ns shrdlu.progmr)

;; ###########################################################
;;
;;                         PROGMR
;;  (INTERPRETER FOR THE PROGRAMMAR GRAMMAR WRITING LANGUAGE)
;;
;; ###########################################################

(defn- setmvb [ptr-mvb]
    (let [save *pt*]
        (set! *mvb* ptr-mvb)                              ;; IF THERE ARE SEVERAL CLAUSES ACTIVE AT THE
        (set! *pt* ptr-mvb)                               ;; SAME TIME, IT SETS THE NEAREST ONE.
        (setr (MOVE-PT 'C 'U '(CLAUSE)) 'MVB ptr-mvb)
        (set! *pt* save)
        true))

(defn- add-f-pt [feature ptr]
    (putprop! (car ptr) 'FEATURES (cons feature (features ptr)))
    (when (= ptr *c*)
        (set! *fe* (features ptr)))
    true)

(defn- remove-f-pt [feature ptr]
    (putprop! (car ptr) 'FEATURES (setdif (getprop (car ptr) 'FEATURES) (list feature)))
    (when (= ptr *c*)
        (set! *fe* (features ptr)))
    true)

(defn- one-word-left [nb] (and (cdr nb) (not (cddr nb))))

(§ defn- MOVE-PT [& a]
    (let [xx nil L2 nil save *pt*]
    TEST1 (when (and (cdr a) (not (term? (cadr a)))) (GO TEST))
    LOOK1 (SETQ xx (car a))
    LOOK (condp = xx
            'H (if-not (set! *pt* *h*) (GO FAIL))
            'C (set! *pt* *c*)
            'PC (set! *pt* (daughters (parent *c*)))
            'LASTSENT (set! *pt* *lastsent*)
            'U (if-not (set! *pt* (parent *pt*)) (GO FAIL))
            'DLC (if-not (set! *pt* (daughters *pt*)) (GO FAIL))
            'DF (do (SETQ L2 (cons 'DLC (cons 'FR L2))) (SETQ xx 'DLC) (GO LOOK))
            'FR (when (MOVE-PT 'PV) (GO LOOK))
            'NX (if-not (set! *pt* (previous (daughters (parent *pt*)) (car *pt*))) (GO FAIL))
            'PV (set! *pt* (or (and (= *pt* *c*) (daughters (parent *c*))) (following (daughters (parent *pt*)) (car *pt*)) (GO FAIL)))
            (bug "MOVE-PT: ILLEGAL INSTRUCTION"))
    EX  (COND ((or (nil? L2) (nil? (SETQ L2 (cdr L2))))
            (GO TEST)))
        (SETQ xx (car L2))
        (GO LOOK)
    TEST (cond (nil? (cdr a)) (RETURN *pt*)
            (term? (cadr a)) true
            (if (cdadr a) (eval (cadr a)) (isq *pt* (caadr a))) (SETQ a (cdr a))
            :else (GO LOOK1))
        (when (SETQ a (cdr a)) (GO TEST1))
        (RETURN *pt*)
    FAIL (set! *pt* save)
        nil))

(§ defn- MOVE-PTW [& a]
    (let [save *ptw* xx nil]
    TEST1 (when (and (cdr a) (not (term? (cadr a)))) (GO EX))
    LOOK1 (SETQ xx (car a))
    LOOK (condp = xx
            'N (set! *ptw* *n*)
            'LASTSENT (set! *ptw* (firstword *lastsent*))
            'FW (set! *ptw* (firstword *pt*))
            'AW (cond (= *pt* *c*) (GO FAIL) (set! *ptw* (wordafter *pt*)) (do (SETQ xx 'PW) (GO LOOK)))
            'LW (cond (= *pt* *c*) (GO FAIL) (set! *ptw* (wordafter *pt*)) (do (SETQ xx 'PW) (GO LOOK)))
            'NW (cond (set! *ptw* (cdr *ptw*)) *ptw* (set! *ptw* (findb *sent* nil)) (GO FAIL))
            'PW (cond (set! *ptw* (findb *sent* *ptw*)) *ptw* (set! *ptw* *sent*) (GO FAIL))
            'SFW (set! *ptw* *sent*)
            'SLW (set! *ptw* (findb *sent* nil))
            (bug "MOVE-PTW: ILLEGAL INSTRUCTION"))
    EX  (cond (nil? (cdr a)) (RETURN *ptw*)
            (term? (cadr a)) true
            (if (cdadr a) (eval (cadr a)) (isq *ptw* (caadr a))) (SETQ a (cdr a))
            :else (GO LOOK1))
        (when (SETQ a (cdr a)) (GO TEST1))
        (RETURN *ptw*)
    FAIL (set! *ptw* save)
        nil))

(defn- apply-grammar [UNIT] (eval (list UNIT)))

(defn- buildnode [features firstword wordafter daughters semantics]
    (let [node (list (gensym 'NODE))]
        (setr node 'FEATURES features)
        (setr node 'FIRSTWORD firstword)
        (setr node 'WORDAFTER wordafter)
        (setr node 'DAUGHTERS daughters)
        (setr node 'SEMANTICS semantics)
        node))

(defn- rebuild [features firstword wordafter daughters semantics node]
    (setr node 'FEATURES features)
    (setr node 'FIRSTWORD firstword)
    (setr node 'WORDAFTER wordafter)
    (setr node 'DAUGHTERS daughters)
    (setr node 'SEMANTICS semantics)
    node)

(defn- word [node] (car node))

(defn- features [node] (getr node 'FEATURES))
(defn- firstword [node] (getr node 'FIRSTWORD))
(defn- wordafter [node] (getr node 'WORDAFTER))
(defn- daughters [node] (getr node 'DAUGHTERS))
(defn- semantics [node] (getr node 'SEMANTICS))
(defn- parent [node] (getr node 'PARENT))
(defn- root [node] (or (getr node 'ROOT) (word node)))

(defn- cut [a]
    (loop [b *n*]
        (cond
            (= a b) (do (set! *cut* a) (set! *nn* (not= *cut* *n*)) true)
            (= b *end*) nil
            (cdr b) (recur (cdr b))
            (nil? a) (do (set! *cut* nil) (set! *nn* *n*) true))))

(defn- cut-back-one [] (MOVE-PTW 'N 'PW) (POP) (cut *ptw*))

(defn- flushme []
    ;; IF HAVEN'T REACHED THE CUT, FLUSHES THE NEXT WORD IN THE SENTENCE, FAILS OTHERWISE.
    (and *n* *nn* (set! *nn* (not= *cut* (set! *n* (cdr *n*))))))

(defn- following [a x]
    ;; GET THE ELEMENT OF LIST FOLLOWING MEMBER
    (let [a (memq x a)] (when a (cdr a))))

(defn- previous [a x]
    ;; GET THE ELEMENT OF LIST BEFORE MEMBER.
    (loop [e nil a a]
        (cond (nil? a) nil (= (car a) x) e :else (recur (car a) (cdr a)))))

(defn- m! [x] (set! *me* (cons x *me*)))

(defn- nextword [n] (car n))                ;; RETURN THE NEXT WORD IN THE SENTENCE

(defn- nextword? [n w] (= (car n) w))

(defn- secondword? [n w] (and n (cdr n) (= (cadr n) w)))

(defn- parse [& a]
    (if (memq (car a) '(NG CLAUSE VG PREPG ADJG)) (PARSE2 a (memq 'TOPLEVEL a)) (PARSE3 a nil)))

(§ defn- PARSE2 [rest' p]
    ;; THIS FUNCTION CALLS THE PROGRAMMAR FUNCTION INDICATED BY THE FIRST MEMBER OF REST - A FEATURE LIST.
    ;; THE PARAMETER P INDICATES WHETHER PARSE2 IS BEING CALLED FROM THE TOPLEVEL.
    ;; IF P IS TRUE, PARSE2 DOES NOT APPEND THE NODE JUST PARSED TO THE PARSING TREE.
    ;; PARSE2 WILL CALL EITHER A COMPILED OR INTERPRETED VERSION OF THE GRAMMAR PROGRAM.
    (let [*rest* rest' UNIT (car *rest*) *end* nil *parent* nil *re* nil *special* nil nbb *n*]
        (set! *level* (inc *level*))
        (COND ((= *n* *cut*)
            (set! *level* (dec *level*))
            (RETURN nil)))
        (set! *end* *cut*)
        (set! *nn* (not= *n* *cut*))
        (set! *parent* *c*)
        (COND ((nq 'B-SPECIAL)
            (eval (getr *n* 'B-SPECIAL))))
        (COND ((= *special* 'SKIP) (GO SKIP))
            ((= *special* 'DONE) (GO DONE))
            ((= *special* 'LOSE) (set! *n* nbb) (GO LOSE)))
        (COND ((nil? (set! *re* (apply-grammar UNIT)))    ;; THIS IS WHERE ALL THE WORK HAPPENS.  IF THE PARSE SUCCEEDS,
            (set! *re* nil)                               ;; IT WILL RETURN THE NODE THAT HAS BEEN BUILT UP
            (set! *n* nbb)                                ;; (SEE THE FUNCTION "INTERPRETATION" IN IN GINTER)
            (GO LOSE)))
    SKIP (COND ((= *n* *cut*))
            ((nq 'SPECIAL) (eval (getr *n* 'SPECIAL))))
    DONE (or p
            (rebuild *c*
                (set! *fe* (getprop (car *c*) 'FEATURES))  ;; REBUILDS THE HIGHER LEVEL NODE TO INCORPORATE
                *nb*                                      ;; THE DAUGHTER THAT WAS JUST PARSED EXCEPT IN THE
                *n*                                       ;; CASE WHERE THIS NODE IS THE TOPLEVEL
                (set! *h* (concat *re* *h*))
                *sm*))
    LOSE (set! *nn* (not= *n* *cut*))
    OK  (set! *level* (dec *level*))
        *re*))

(§ defn- PARSE3 [rest' p]
    ;; PARSE3 IS CALLED TO PARSE JUST THE NEXT WORD IN THE SENTENCE
    (let [*rest* rest' xp nil labl nil *re* nil *special* nil nbb *n* NODE nil]
        (COND ((= *n* *cut*) (m! 'CUT) (RETURN nil))
            ((nq 'B-SPECIAL)                                     ;; IS THE NEXT WORD MARKED SPECL?
            (eval (getr *n* 'B-SPECIAL))                          ;; YES, DO SOMETHING SPECIALL
            (COND ((= *special* 'SKIP) (GO SKIP))
                ((= *special* 'LOSE) (set! *n* nbb) (RETURN nil))
                ((= *special* 'DONE) (GO DONE)))))
        (COND ((car (SETQ xp *rest*)))                            ;; IF CALL IS (PARSE NIL FOO)
            ((nextword? *n* (cadr *rest*)) (GO OK))                   ;; THEN LOOK FOR EXACT WORD "FOO"
            ((set! *n* nbb) (RETURN nil)))                        ;; IF NOT THERE, FAIL
    LOOP (COND ((not (term? (car xp)))
            (SETQ labl (cons (caar xp) labl)))                  ;; IF THE FEATURE IS NOT AN ATOM JUST ADD THE
            ((= (car xp) 'NULL))                               ;; FEATURE TO THE LIST
            ((memq (car xp) (features *n*)))
            ((memq (car xp) '(COMPONENT BOTH)))
            ((m! (car xp)) (set! *n* nbb) (RETURN nil)))
        (when (SETQ xp (cdr xp)) (GO LOOP))
    OK  (set! *re*
            (buildnode (meet (concat (features *n*) labl) (getprop (car *rest*) 'ELIM))
                *n*
                (cdr *n*)
                'WORD
                (or (nil? (car *rest*))
                    (and (nil? (semantics *n*)) (undefined *n*))
                    (cadr (sassq (car *rest*) (semantics *n*) #(undefined *n*))))))
        (set! *n* (cdr *n*))
    SKIP (set! *nn* (not= *n* *cut*))
        (when (and *nn* (nq 'SPECIAL)) (eval (getr *n* 'SPECIAL)))
    DONE (setr *re* 'PARENT *c*)
        (if p *re* (rebuild *c* *fe* *nb* *n* (set! *h* (concat *re* *h*)) *sm*))
        *re*))

(defn- parserel [a b node]
    (loop [a a] (when (some? a)
        (if (and (isq node (caar a)) (apply parse 'CLAUSE 'RSNG (concat (cdar a) b))) *h* (recur (cdr a))))))

(§ defn- POP [& a]
    (if (and (some? a) (some? (car a)))
        (when (apply POPTO a) (POP))
        (when (and (some? *h*) (set! *n* (firstword *h*)))
            (set! *h* (cdr *h*))
            (rebuild *c* *fe* *nb* *n* *h* *sm*)
            (set! *nn* (not= *n* *cut*))
            (let [xx nil]
                (MAP (lambda [b] (ERRSET (and (MAP #(and (= % (firstword b)) (ERR)) *n*) (SETQ xx (cons (car b) xx))))) *backref*)
                (set! *backref* xx))
            true)))

(§ defn- POPTO [& a]
    (loop [xx *h*]
        (cond (apply isq xx a) (loop [] (cond (= xx *h*) *c* (POP) (recur)))
            (cdr xx) (recur (cdr xx))
            :else (do (m! 'POPTO) nil))))

(§ defn- PTFIND [x y z]
    (let [foo (car x)]
    UP  (COND ((MOVE-PT 'U) (GO UP)) ((= (firstword *pt*) x) (GO ON)))
    DOWN (or (MOVE-PT 'DLC 'PV '(memq foo (firstword *pt*))) (RETURN nil))
    ON  (COND ((not (= x (firstword *pt*))) (GO DOWN))
            ((= y true))
            ((MOVE-PT 'DF '(= (wordafter *pt*) y)))
            ((RETURN nil)))
    CHECK (COND ((eval z) (RETURN *pt*))
            ((not (= y true)))
            ((MOVE-PT 'DF) (GO CHECK)))
        nil))

(defn- cq [feature] (memq feature *fe*))

(defn- f! [x] (if (memq x *fe*) true (setr *c* 'FEATURES (set! *fe* (cons x *fe*)))))

(defn- feset [node features] (setr node 'FEATURES features))

(defn- fq! [& a]
    (dorun (map #(or (memq % *fe*) (set! *fe* (cons % *fe*))) a))
    (setr *c* 'FEATURES *fe*))

(defn- isq [& a] (memq (cadr a) (features (car a))))

(defn- nq [x] (memq x (features *n*)))

(defn- rq [& a] (setr *c* 'FEATURES (set! *fe* (setdif *fe* a))))

(defn- trnsf [& a] (setr *c* 'FEATURES (set! *fe* (union (meet a (features *pt*)) *fe*))))

#_(ns shrdlu.macros)

;; #############################################################
;;
;;              A PRECOMPILER FOR PROGRAMMAR CODE
;;
;; #############################################################

(defn- passing [x]
    (cond
        (term? *labeltrace*) (when *labeltrace* (terpri) (print "PASSING" x) true)
        (memq x *labeltrace*) (do (terpri) (print "PASSING" x) true)))

(defn- spread1 [e]
    (cond (term? e)
            (list e (list 'when '*labeltrace* (list 'passing (list 'quote e))))
        (= (car e) '|)
            (let [predicate (cadr e) t1 (caddr e) t2 (cadddr e) t3 (caddddr e)
                  go- #(if (term? %) (list 'GO %) (list 'GO 'FAIL (list 'm! (car %))))]
                (list (cond
                    (and t1 (nil? t2)) ;; T3 CAN BE EITHER THERE OR NOT
                        (list 'when predicate (if t3 (list 'if '*nn* (go- t1) (go- t3)) (go- t1)))
                    (and (nil? t1) t2 (nil? t3))
                        (list 'when-not predicate (go- t2))
                    (and (nil? t1) t2 t3)
                        (list 'when-not predicate (list 'if '*nn* (go- t2) (go- t3)))
                    (and t1 t2 (nil? t3))
                        (list 'if predicate (go- t1) (go- t2))
                    (and t1 t2 t3)
                        (list 'if predicate (list 'if '*nn* (go- t1) (go- t3)) (go- t2))
                    (and (nil? t1) (nil? t2) t3)
                        (list 'when (list 'and predicate (list 'not '*nn*)) (go- t3))
                    (and (nil? t1) (nil? t2) (nil? t3))
                        (list 'I-AM-A-TAG))))
        :else (list e)))

(§ defq- PDEFINE [a]
    (list 'DEFUN (car a) 'nil
        (concat
            (list 'PROG
                (concat '(*fe* *h* *me* *nb* *c* *sm* *cut* *nn* *tmp*) (cadr a))
                '(set! *nn* true)
                '(set! *cut* *end*)
                '(set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
                '(setr *c* 'PARENT *parent*))
            (mapcat spread1 (cddr a))
            (list 'FAIL
                '(set! *mes* *me*)
                '(set! *n* (or (wordafter *re*) *nb*))
                '(RETURN nil)
                'RETURN
                '(set! *mes* *me*)
                '(RETURN (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*))))))

#_(ns shrdlu.gramar)

(§ PDEFINE CLAUSE (*position-of-prt* *mvb* *locationmarker* *subj-vb-backup-type1* *position-of-ptw*)

    ENTERING-CLAUSE
        (setr *c* 'TIME (BUILD TSSNODE= (gensym 'TSS)))
        (| (cq 'SIMP) SUBJ nil)
        (| (cq 'MAJOR) INIT SEC)

    INIT
        (set! *locationmarker* *n*)
        (| (and (nq 'BINDER) (parse 'CLAUSE 'BOUND 'INIT)) nil MAJOR FIXIT)
        (fq! 'BIND)
        (| (SMBIND) INIT nil)

    FIXIT
        (set! *ptw* *cut*)
        (| (cut (MOVE-PTW)) INIT MAJOR)

    MAJOR
        (cut *end*)
        (cond (= *punct* '?) (GO QUEST)
            (or (cq 'IMPER) (= *punct* '!)) (GO IMPER))
        (GO THEREINIT)

    FDEC
        (fq! 'DECLAR)

    THEREINIT                                                       ;; CONSTRUCTIONS USING THE FUNCTION WORD "THERE"
        (| (and (nextword? *n* 'THERE)                                  ;; ARE CHECKED FOR EXPLICITLY AND PROCESSED BY A
                (parse nil 'THERE)                                   ;; SPECIAL BLOCK OF CODE (SEE ABOVE)
                (fq! 'DECLAR))
            THERE
            nil
            (INIT))

    THER2
        (and (nq 'PREP)
            (parse 'PREPG 'INIT)
            (or (SMRELATE *h*) (POP)))                       ;; MORE INITIAL (BEFORE THE SUBJECT) MODIFIERS
        (and (nq 'ADV)
            (parse 'ADV 'TIMW)
            (or (SMADVERB) (POP)))
        (and (nq 'ADV)
            (parse 'ADJG 'ADV 'VBAD)
            (or (SMRELATE *h*) (POP)))
        (parse 'NG 'TIME)

        (| (= *locationmarker* *n*) CLAUSETYPE INIT INPOP)

    ;; THE VARIABLE "LOCATIONMARKER" MARKS THE POSITION OF PTW AT THE TIME THAT IT WAS SET.
    ;; IF IT HAS NOT MOVED (IS STILL EQUAL TO N), THEN THAT INDICATES THAT NOTHING HAS BEEN
    ;; PARSED AND WE CAN GO ON.  OTHERWISE, THERE CAN BE ANY NUMBER OF INITIAL MODIFIERS AND
    ;; THE CODE STARTING AT "INIT" IS REPEATED, AS MANY TIMES AS NECESSARY.  IF PTW HITS THE
    ;; CUT POINT, THEN IT IS ASSUMED THAT SOMETHING WAS MISTAKENLY PARSED AS A MODIFIER WHEN
    ;; IT WAS NOT, AND EVERYTHING IS POPPED OFF (BY THE "INPOP" CODE).

    INPOP
        (| (MOVE-PT 'C 'DLC) nil (INPOP))                             ;; DOES ANYTHING REMAIN ON THE TREE?

    BICUT
        (cut-back-one)                                              ;; "CUT-BACK-ONE" IS THE NORMAL BACKING UP MECHANISM
        (GO INIT)                                                   ;; FOR THE GRAMMAR.  IT SETS PTW (POINTER TO THE WORD)
                                                                    ;; BACK ONE FROM WHERE IT WAS AND SETS "CUT" TO PTW.
                                                                    ;; THE FOLLOWING GOTO TELLS WHICH BLOCK OF CODE IS TO BE REPEATED.

    ;; RE-EXAMINE THE CLAUSETYPE, PARTICULARLY TO CHECK FOR VERB-INITIAL IMPERATIVES

    CLAUSETYPE
        (| (cq 'DECLAR) SUBJ nil)
        (| (and (nq 'VB) (nq 'INF) (parse 'VG 'IMPER) (fq! 'IMPER))
            VG1
            nil)                                                    ;; SEE THE NOTE UNDER IMPERATIVES BELOW
        (fq! 'DECLAR)
        (| (cq 'IMPER) (IMPER) nil)

    ;; ###############################################################
    ;;         TRY TO PARSE A GRAMMATICLY ACCEPTABLE SUBJECT
    ;; ###############################################################

    ;; ONCE THAT IS DONE, SET THE SUBJECT REGISTER (FOR USE BY SEMANTIC ROUTINES AND OTHER PARTS OF THE GRAMMAR)
    ;; AND MOVE ONE TO THE CODE FOR WHICH LOOKS FOR THE MAIN VERB (MVB) - "VG"

    SUBJ (cut *end*)                                                  ;; RESET CUTPOINT IN CASE IT WAS MODIFIED BY
    SUBJ3                                                           ;; PREVIOUS BACKUPS IF THE FIRST WORD INDICATES
        (| (or (and (nextword? *n* 'TO)                                 ;; THE POSSIBILITY OF A RANK-SHIFTED CLAUSE
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

        (cond (cq 'REL-NOT-FOUND)
                (do (rq 'REL-NOT-FOUND) (setr *c* 'SUBJECT (getr *c* 'RELHEAD)) (GO VB))
            *subj-vb-backup-type1*
                (do (set! *subj-vb-backup-type1* nil) (GO SUBJ11))      ;; SEE THE LARGE NOTE ABOUT THIS IN "NOVERB".
            (and *h* (isq *h* 'TIME) (isq *h* 'NG))
                (do (setr *c* 'SUBJECT *h*) (GO VB))                    ;; WHAT WAS INITIALLY PARSED AS A TIME-NG MODIFING
            (MOVE-PT 'C 'U '(REL-NOT-FOUND))                          ;; THE WHOLE CLAUSE MAY PROBABLY BEEN THE SUBJECT
                                                                    ;; OF THE CLAUSE THIS WORRIES ABOUT RELATIVE
                                                                    ;; CLAUSES. PLEASE NOTE THAT THE CURRENT
                                                                    ;; HALF-VERSION HAS NOT YET GOT ITS HEAD TOGETHER
                                                                    ;; ABOUT RELATIVE CLAUSES.  IE. THE CODE ISN'T
                (do (setr *c* 'SUBJECT (getr *pt* 'RELHEAD))                ;; DEBUGGED AND HAS GAPS IN IT ESP. WHO SETS WHAT
                    (setr *c* 'RELHEAD (getr *pt* 'RELHEAD))                ;; REGISTER WHEN THIS WILL BE FIXED BEFORE THE
                    (remove-f-pt 'REL-NOT-FOUND *pt*)                     ;; VERSION IS FINALIZED
                    (GO VB))
            (and (cq 'COMPONENT) *nn*)
                (do (fq! 'SUBJFORK) (GO VB))                        ;; "SARAH ATE DINNER AND WENT TO THE MOVIES."
            *h* (do (POP) (GO SUBJ))                                     ;; POP OFF THE CLOSEST INITIAL MODIFIER AND TRY TO
            :else (GO FAIL))                                            ;; PARSE A SUBJ AGAIN

    HEAD
        (| (or (MOVE-PTW 'N 'PW '(NOUN)) (MOVE-PTW 'N 'PW '(PRON)))       ;; COME HERE (ONLY?) TO TRY TIME PHRASE AS SUBJECT
            nil
            (HEAD))                                                 ;; MOVE PTW TO THE CLOSEST NOUN THEN SET THE CUT

    SUB2
        (| (POP) nil FAIL)                                          ;; POINT TO IT AND ATTEMPT A NEW PARSING IF
        (| (cut *ptw*) INIT SUB2)                                     ;; NOTHING MORE TO POP, LOSE

    SUBJ1
        (when (isq *h* 'QUOTED)                                       ;; CIRCUMSTANCES UNDER WHICH IT IS ALLRIGHT TO
            (when (isq *h* 'LIST) (fq! 'LIST))                            ;; HAVE NOTHING FOLLOWING THE SUBJECT OF THE
            (fq! 'QUOTED)                                             ;; CLAUSE "  "MUMBLE", SAID JOHN."
            (set! *h* (daughters *h*))
            (GO RETSM))
        (and (cq 'REL-NOT-FOUND)                                     ;; THIS IS PART OF A BACKUP MECHANISM WHICH NEEDS
            (MOVE-PT 'H 'PV '(QAUX))                                   ;; TO BE MORE THROUGHLY THOUGHT OUT. THE SITUATION
            (cond
                (isq *pt* 'BE)                                        ;; IS EXPLAINED IN DETAIL IN QUESTION.NGQST MOVE
                    (do (fq! 'INT 'AUXBE)                                  ;; PT TO A VERB WHICH CAN BE AN AUXILLIARY AND
                        (rq 'REL-NOT-FOUND)                              ;; WHICH CAN BEGIN A CLAUSE
                        (setr *c* 'COMP (getr *c* 'RELHEAD))
                        (setr *c* 'SUBJECT *h*)                             ;; "WHAT COLOR IS THE BLOCK?" OR "HOW BIG IS THE BLOCK?"
                        (setmvb *pt*)
                        (GO ONT))
                (isq *pt* 'HAVE)
                    (do (fq! 'SUBQ)
                        (rq 'REL-NOT-FOUND)
                        (setr *c* 'SUBJECT (getr *c* 'RELHEAD))
                        (GO VBL))))

    SUBJ11
        (| (cut-back-one) SUBJ3 (SUBJ11))                           ;; IF WE CAN'T CUT BACK ANY FURTHER, THEN FAIL

    SUBREG
        (setr *c* 'SUBJECT *h*)                                         ;; THIS SETS THE "SUBJECT" REGISTER OF THE CURRENT
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
        (rq 'VBLOK)                                                  ;; ?????

    VBL (| (parse 'VG) VBREG nil)                                    ;; ONCE THE VERB GROUP IS PARSED, SET THE REGISTER

    NOVERB
        (cond                                                       ;; WHAT TO DO IF THE VG CANNOT BE DIRECTLY PARSED?
            (cq 'SUBJFORK) (do (fq! 'VBFORK) (GO FINDOBJ1))
            (isq *h* 'QUOTED) (do (fq! 'REL-NOT-FOUND) (GO SUBJ4))
            (not (isq *h* 'SUBJ)) (GO FAIL)
            (isq *h* 'CLAUSE)
                (do (set! *subj-vb-backup-type1* true) (POP) (GO SUBJ4))    ;; THIS IS EXACTLY WHAT IT LOOKS LIKE.
                                                                    ;; AN ARBITRARY, NOT TOO WELL THOUGHTOUT BACKUP
                                                                    ;; MECHANISM.  (NEEDLESS TO SAY IT WILL GO AWAY
                                                                    ;; FAST).  WE HAVE BEEN UNABLE TO FIND A VERB
                                                                    ;; AND HAVE NOTICED THAT WE PARSED A CLAUSE OF
                                                                    ;; SOME SORT AS THE SUBJECT.  HYPOTHESIS:  WE
                                                                    ;; MISSINTERPRETED SOMETHING WHILE PARSING THAT
                                                                    ;; CLAUSE AND MANAGED TO SWALLOW UP THE VERB OF
            (isq *h* 'SUBJ) (do (POP) (fq! 'SUBJFORK) (GO VBL)))            ;; THE HIGHER CLAUSE WITH IT.  SOLUTION:  POP OFF
    VB2
        (cut-back-one)                                              ;; THE CLAUSE AND TRY TO REPARSE THE SEGMENT IN
        (GO SUBJ3)                                                  ;; ANOTHER FASHION.  "SUBJ4" IS PLACED THE SUBJECT
                                                                    ;; CODE AFTER LOOKING FOR CLAUSES AND BEFORE NOUN
                                                                    ;; GROUPS.  DEFAULT CUTTING MECHANISM FOR VBL.
    VBREG
        (setr *c* 'VG *h*)

    ;; ###############################################################
    ;;
    ;;             PARSE ANY OBJECTS REQUIRED BY THE VERB
    ;;
    ;; ###############################################################

    VG1 (cut *end*)                                                   ;; RESET THE CUTPOINT IN CASE ANYONE CHANGED IT
        (| (isq *mvb* 'BE) BE nil (BE))                                ;; JUMP TO "BE" PROCESSOR

        ;; There used to be a check here for a quoting MVB with a quoted subject.
        ;; It was deleted because it went to a tag that no longer exists and doesn't seem to have any modern analogs.
        ;; For the original code: see "gramar 19" or earlier.
        ;; It was put in by Jeff Hill in the spring of 1972.

        ;; VERB-PARTICLE COMBINATIONS SUCH AS "PUT ON", "SET DOWN", ETC.
        ;; THEIR ESSENTIAL PROPERTY IS THAT VERB AND PARTICLE CAN BE DISPLACED BY THE OBJECT.
        ;;    "PUT DOWN THE BLOCK."
        ;;    "PUT THE BLOCK DOWN."

        (| (isq *mvb* 'VPRT) nil CHECKPASV CHECKPASV)
        (| (and (nq 'PRT) (parse 'PRT)) nil DPRT)                             ;; IF THE PARTICLE IS NOT THE WORD FOLLOWING THE VERB, THEN
        (fq! 'PRT)                                                            ;; IT IS SEARCHED FOR BY CODE AT "DPRT" (DISPLACED PARTICLE)

        (| (setmvb (combination? (root (firstword *mvb*)) (word (firstword *h*))))            ;; IS THIS A LEGITIMATE COMBINATION OF VERB AND PARTICLE?
            CHECKPASV
            POPRT)

    DPRT
        (| (isq *h* 'PASV) CHECKPASV nil)                                      ;; SEARCH FOR DISPLACED PARTICLE.  NO DISPLACED PARTICLES
        (| (set! *position-of-prt* (MOVE-PTW 'N 'NW '(PRT))) nil FINDOBJ1)       ;; IN PASV'S IF NOT FOUND ASSUME THAT IT IS OPTIONAL AND
        (| (setmvb (combination? (root (firstword *mvb*)) (word *position-of-prt*)))   ;; WE ARE DEALING WITH THE CASE WITHOUT THE PARTICLE
            nil
            POPRT)
        (| (isq *mvb* 'TRANS) nil FINDOBJ1)
        (cut *position-of-prt*)
        (| (parse 'NG 'OBJ 'OBJ1)                                              ;; PARSE UP ANY NOUN GROUP YOU FIND
            POPRT
            FINDOBJ1                                                        ;; IF THERE ARE MORE OR LESS NP'S THAN EXPECTED,
            nil)                                                            ;; THEN DON'T PARSE ANYTHING BUT GO TO NPRT
        (cut *end*)                                                           ;; INSTEAD. SIMILARLY, IF ANYTHING FOLLOWS THE
        (setr *c* 'OBJ1 *h*)                                                    ;; DISPLACED PARTICLE THEN A GRAMMATICALLY BAD
        (parse 'PRT)                                                         ;; FORM IS ASSUMED AND THE PIECES POPED OFF
        (fq! 'PRT 'DPRT)
        (GO FINDOBJ2)

    POPRT
        (POPTO 'VG)
        (GO FINDOBJ1)

    CHECKPASV                                                               ;; CHECK THE VERB FOR THE PASSIVE CONSTRUCTION
        (| (and (isq *h* 'PASV) (fq! 'PASV) (setr *c* 'OBJ1 (getr *c* 'SUBJECT)))
            FINDOBJ2
            nil
            FINDFAKE2)
        (fq! 'ACTV)                                                           ;; NOT PASV=ACTIVE
        (GO FINDOBJ1)

    BE
        (fq! 'BE)
        (and (parse nil 'NOT) (fq! 'NEG))
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
                (isq (getr *pt* 'QADJ) 'PLACE)
                (fq! 'ITRANSL))
            PUTLOBJ
            nil)
        (| (CANPARSE 1 nil 'ITRNS) ONT nil)

    GOOF1
        (or *global-message* (bug "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)

    OBJ1REL
        (setr *c* 'OBJ1 (getr *pt* 'RELHEAD))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (fq! 'OBJ1REL)

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
        (| (and (isq *mvb* 'TRANS2) (MOVE-PT 'C 'U '(REL-NOT-FOUND)))
            OBJ2REL
            nil)
        (| (and (CANTAKE 2 '(PREPG LOC) 'TRANSL)
                (MOVE-PT 'C 'U '(QADJ))
                (isq (getr *pt* 'QADJ) 'PLACE)
                (fq! 'TRANSL))
            PUTLOBJ
            nil)

    OBJ2TO
        (parse 'ADV 'VBAD)
        (| (if (and (nextword? *n* 'TO) (isq *mvb* 'TO2) (parse 'PREPG 'TO))  ;; THE SECOND-OBJECT THAT WE HAVE BEEN LOOKING FOR
                (do (setr *c* 'OBJ2 (getr *h* 'OBJ1))                       ;; MAY BE A PREPG AS IN "GIVE IT TO THE LIONS"
                    (fq! 'TRANS2TO 'TRANS2))                               ;; TAKES THE OBJECT OF THE PREPOSITION "TO" AND
                (and (cq 'PREPQ)                                        ;; MAKES IT THE OBJ2 OF THE CLAUSE.
                    (MOVE-PT 'H 'PV '(QUEST))
                    (= (word (MOVE-PTW 'FW)) 'TO)
                    (rq 'PREPQ)
                    (fq! 'TRANS2TOQ 'TRANS2)
                    (setr *c* 'OBJ2 (getr *pt* 'OBJ1))))                   ;; "TO WHOM DID YOU GIVE THE MEAT?"
            ONT
            nil)
        (| (CANPARSE 2 nil 'TRANS) ONT FAIL)

    PUTLOBJ
        (setr *c* 'LOBJ *pt*)
        (setr *pt* 'RELHEAD (getr *pt* 'QADJ))
        (setr *pt* 'QADJ nil)
        (remove-f-pt 'QADJ *pt*)
        (GO ONT)

    OBJ2REL
        (setr *c* 'OBJ2 (getr *pt* 'RELHEAD))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (fq! 'OBJ2REL)
        (GO ONT)

    FIXSUBJECT
        (setr *h* 'SUBJECT (getr *c* 'OBJ1))
        (GO ONT)

    CHECKIT                                                         ;; CHECK FOR THE POSSIBILITY THAT THE SUBJECT WAS
        (| (= (word (firstword (getr *c* 'SUBJECT))) 'IT)                   ;; A DUMMY FUNCTION WORD ("IT"), AS IN "IT WAS NICE TO SEE HIM."
            nil
            ONT)                                                    ;; TO BE ADDED HERE: "JOHN WAS EAGER/EASY TO PLEASE."
        (| (or (and (nextword? *n* 'TO) (parse 'CLAUSE 'RSNG 'TO 'SUBJ))
                (and (nq 'ING) (parse 'CLAUSE 'RSNG 'ING 'SUBJ))
                (parse 'CLAUSE 'REPORT))
            nil
            ONT)
        (fq! 'IT)
        (setr *c* 'LOGICAL-SUBJECT *h*)                                 ;; THE CLAUSE IS THE REAL SUBJECT.
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
        (| (cq 'PASV) PONT nil)
    ONT1
        (| (SMCL1) nil (SMCL1))

        (| (not (cq 'REL-NOT-FOUND)) TONT nil RETSM)                 ;; IF THE FEATURE "REL-NOT-FOUND" IS PRESENT AT
                                                                    ;; THIS POINT, IT INDICATES THAT WE ARE IN A
        (| (isq (getr (getr *c* 'RELHEAD) 'HEAD) 'TIM1)                ;; RELATIVE CLAUSE AND MAY HAVE TO DO SOME
            nil                                                     ;; GYMNASTICS IF THE CLAUSE IS NOT TO FAIL
            PREPSHORT)                                              ;; MOVE BACK TO A QUESTION-NOUNGROUP, THEN DOWN

    TIMEQ
        (rq 'REL-NOT-FOUND)                                          ;; AND BACK TO THE NOUN. IF THAT NOUN IS "TIM1"
        (fq! 'TIMEQ)                                                  ;; THEN ASSUME WE HAVE FOUND OUR RELATIVE ELEMENT.
        (GO TONT)

    PREPSHORT
        (| (and (nq 'PREP) (parse 'PREPG)) nil (ONT-SHORT-PREP))
        (| (SMRELATE *h*) nil (ONTß SMRELATE PREPQ))
        (| (cq 'REL-NOT-FOUND) PREPSHORT TONT (ONT-NOT-FOUND))       ;; WE HAVE A PREP TO TAKE THE UNATTACHED RELATIVE
                                                                    ;; AS ITS OBJECT. THE FEATURE REL-NOT-FOUND WILL
                                                                    ;; BE REMOVED IF THE PREPG DISCOVERS IT CAN'T FIND

    PONT
        (and (nextword? *n* 'BY) (parse 'PREPG 'AGENT) (fq! 'AGENT))        ;; AN OBJECT (THE REMOVING WILL BE DONE WHILE IN PREPG).
        (setr *c* 'LOGICAL-SUBJECT (getr *h* 'OBJ1))                    ;; "LOGICAL" IE. SUBJECT IN RELATIONSHIP
        (GO ONT1)                                                   ;; TO THE PROPER SEMANTIC INTERPRETATION OF THE
                                                                    ;; MAIN VERB. AGENT-PREPG CAN BE PARSED (REFLECTS
                                                                    ;; THE OPTIONALITY OF THE CONSTRUCTION)

    ;; ###################################################################################
    ;;                    CHECK FOR ADDITIONAL MODIFYING PHRASES
    ;; ###################################################################################

    TONT (| (set! *position-of-ptw* *n*) nil RETSM RETSM)               ;; WE ARE USING THE SAME TECHNIQUE HERE AS WITH THE INITIAL MODIFIERS.
                                                                    ;; IE. LOOP THROUGH THE POSSIBILITIES UNTIL YOU MAKE A PASS THAT ADDS
                                                                    ;; NOTHING NEW.

    NPASV
        (| (and (nq 'PREP) (parse 'PREPG) (SMRELATE *h*))                                  ;; PREPG
            nil
            nil
            RETSM)
        (| (and (nq 'TIMW) (parse 'ADV 'TIMW) (or (smtime) (GO FAIL)))                    ;; TIMW
            nil
            nil
            RETSM)
        (| (and (not (cq 'BE)) (parse 'ADJG 'ADV) (or (SMRELATE *h*) (GO FAIL)))            ;; ADV
            nil
            nil
            RETSM)
        (| (and (parse 'NG 'TIME) (or (smtime) (GO FAIL)))                               ;; TIME NOUN GROUP
            nil
            nil
            RETSM)
        (| (and (nq 'PLACE) (parse 'ADV 'PLACE) (or (smplace) (GO FAIL)))                 ;; PLACE
            nil
            nil
            RETSM)
        (| (and (nq 'BINDER) (parse 'CLAUSE 'BOUND) (or (SMBIND) (GO FAIL)))              ;; BINDER
            nil
            nil
            RETSM)
        (| (and (nextword? *n* 'TO) (parse 'CLAUSE 'TO 'ADJUNCT) (or (smtoadj) (GO FAIL)))    ;; TO CLAUSE (ADJUNCT)
            nil
            nil
            RETSM)
        (| (= *n* *position-of-ptw*) nil TONT RETSM)                   ;; LOOP UNTIL NOTHING ELSE CAN BE PARSED.
        (| (or (not (cq 'TOPLEVEL)) (nq 'SPECIAL)) RETSM nil)         ;; SPECIAL WORD (E.G. COMMA AND) COULD INDICATE
        (ert "CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL")              ;; A CONJUNCTION OR A BINDER.
        (GO FAIL)

    ;; ##############################################################################
    ;;                                   THERE
    ;;             AS IN:  "THERE IS A BIRD SITTING ON YOUR SHOULDER"
    ;; ##############################################################################

    THERE
        (fq! 'THERE)
        (cut *end*)
        (| (parse 'ADV 'TIMW) nil nil (THERE))                        ;; "THERE IS A BIRD.."
        (| (and (parse 'VG) (isq *mvb* 'BE)) THEF NOTHE (THERE))

    THERQ
        (| (isq (MOVE-PT 'H 'PV '(QAUX)) 'BE) THERQ2 nil)               ;; IF THIS FAILS, THE THERE IS CONSIDERED TO BE
        (| (and (nq 'TIMW) (parse 'ADV 'TIMW)) nil nil (THEREQ))
        (| (and (parse 'VG) (isq *mvb* 'BE)) THERQ2 nil)
        (rq 'POLR2)
        (GO NOTHE)

    THERQ2
        (fq! 'SUBJTQ 'THERE)
        ;; THIS MAY NOT INTERFACE PROPERLY WITH THE SEMANTIC ROUTINES FOR BE
        (| (cq 'POLAR) THEF ONT)

    THEF
        (| (and (nq 'ADV) (parse 'ADV 'TIMW)) nil nil (THEF))
        (| (parse 'NG 'SUBJ 'SUBJT) nil THERREL)
        (fq! 'THERE)
        (setr *c* 'SUBJECT *h*)
        (GO ONT)

    THERREL
        (| (MOVE-PT 'C 'U '(REL-NOT-FOUND)) nil NOTHE)
        (setr *c* 'SUBJECT (getr *pt* 'RELHEAD))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (GO ONT)

    NOTHE
        (rq 'THERE)
        (POP 'THERE)
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
        (fq! 'IMPER)
        (GO VG1)

    IMPOP
        (| (POP nil) IMPE (IMPOP))

    ;; ####################################################################
    ;;                             QUESTIONS
    ;; ####################################################################

    QUEST ;; PREP QUESTION
        (fq! 'QUEST)
        (| (nq 'PREP) nil NGQUES)
        (| (parse 'PREPG) nil NGQUES (PREPQ-INCOMPLETE))             ;; "ON WHICH BLOCK DID YOU PUT IT?"
        (| (isq *h* 'QUEST) nil QUEST)                                 ;; IF THE PREPG ISN'T THE QUESTION, TRY AGAIN "ON
        (setr *c* 'QADJ *h*)                                            ;; THAT DAY, WHERE DID YOU GO?" -- MAYBE WE COULD
                                                                    ;; MAKE USE OF THE COMMA CLUE. PREPQ IS HANDLED
        (GO POLAR)                                                  ;; MUCH LIKE QADJS LIKE WHEN AND WHERE THE REST OF
                                                                    ;; THE QUESTION HAS THE SAME SYNTAX AS A POLAR (YES-NO).
    NGQUES ;; NOUN GROUP QUESTION
        (| (parse 'NG 'QUEST) NGQST nil)                              ;; "WHICH ONE IS THE MURDURER?"
        (| (or (and (nextword? *n* 'HOW)
                (parse 'ADJG 'QUEST)
                (setr *c* 'RELHEAD *h*))                                ;; "HOW BIG...."
            (and (nq 'QADJ)
                (parse 'QADJ)
                (fq! 'QADJ)
                (setr *c* 'QADJ *h*)))                                  ;; "WHAT...?",  "WHERE...?"
            POLAR
            POLAR
            nil)
        (fq! 'SHORTQUES)
        (SMADJQSHORT)                                      ;; IF ALL THE SENTENCE CONSISTS OF IS THE QUESTION

    ADJQS
        (GO RETURN)                                                 ;; ADJECTIVE THEN WE SHOULD RETURN DIRECTLY

    NGQST
        (setr *c* 'RELHEAD *h*)

    NGQST2
        (cut *end*)
        (setr *c* 'SUBJECT *h*)
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

        (cond (parse 'VG 'NAUX) (do (fq! 'SUBJQ) (GO VG1))
            (nq 'VB) (do (fq! 'REL-NOT-FOUND) (GO POLAR))
            :else (do (MOVE-PTW 'N 'PW) (POP 'NG 'QUEST) (cut *ptw*) (GO NGQUES)))  ;; POP BACK AND START FIGURING OUT THE QUESTION

    QUEST2                                                          ;; ALL OVER AGAIN
        (| (and (nextword? *n* 'THERE) (parse nil 'THERE))
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
        (rq 'REL-NOT-FOUND)
        (GO BE)

    POLAR
        (| (and (nq 'VB) (parse 'VB 'AUX '(QAUX)) (setr *c* 'QAUX *h*) (SMVAUX) (setmvb *h*))
            nil
            QCHOP)
        (or (cq 'QADJ) (getr *c* 'RELHEAD) (fq! 'POLAR))
        (fq! 'POLR2)
        (GO QUEST2)

    QCHOP
        (ert "CLAUSE: QCHOP")
        (| (POPTO 'CLAUSE 'BOUND) BICUT (QCHOP))

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
        (cond (cq 'BOUND) (GO BOUND)                               ;; CHECK INITIAL FEATURES AND JUMP ACCORDINGLY
            (cq 'TO) (GO TO)
            (cq 'RSQ) (GO RSQ)
            (cq 'REPORT) (GO REPORT)
            (cq 'ING) (GO ING)
            :else (GO FAIL (m! 'RSNG-TYPE)))

    BOUND ;; BINDER
        (| (parse 'BINDER) nil (BOUND) (BINDER))
        (set! *locationmarker* *n*)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)                                                   ;; "FDEC" IS NEAR THE TOP OF THE MAJOR CLAUSE

    RSQ
        (setr *c* 'RELHEAD (MOVE-PT 'C 'U '(NG)))
        (| (cq 'PREPREL) nil RSQ2)
        (parse 'PREPG 'PRONREL)                                       ;; THIS CALL IS BASED ON INFORMATION PASSED FROM
        (setr *c* 'QADJ *h*)                                            ;; FAR AWAY AND EXPLAINED IN DETAIL IN THE CODE
        (GO REPORT)                                                 ;; FOR PREPOSITION GROUPS

    RSQ2
        (cond (parse 'VG 'EN 'PASV)                                   ;; HAVING DETERMINED THAT THE VERB IS PASSIVE IF
                (do (or (isq *mvb* 'TRANS) (GO FAIL))                      ;; IT WERE NOT ALSO TRANSITIVE, THEN WE WOULDN'T
                    (setr *c* 'SUBJECT (getr *c* 'RELHEAD))                 ;; KNOW WHAT TO DO WITH WHATEVER WAS PARSED AS A
                    (GO VG1))                                           ;; SUBJECT - SO WE FAIL
            (parse 'VG 'ING)
                (do (setr *c* 'SUBJECT (getr *c* 'RELHEAD))
                    (GO VG1))
            (nq 'PRONREL) (do (parse 'NG 'RELWD) (GO REL))
            (cq 'COMPONENT)                                         ;; IN A COMPONENT RELATIVE THE RELWD MIGHT BE IN THE FIRST CLAUSE.
                (do (setr *c* 'RELHEAD (getr (MOVE-PT 'C 'PC) 'RELHEAD))  ;; MAKE RELHEAD SAME AS PREVIOUS COMPONENT RSQ.
                    (GO REL))
            (parse 'NG 'SUBJ) (do (fq! 'REL-NOT-FOUND) (GO SUBREG))
            :else (GO FAIL))                                          ;; THIS REALLY ISN'T AN RSQ

    REL
        (setr *c* 'SUBJECT (getr *c* 'RELHEAD))
        (| (parse 'VG) VG1 nil)                                      ;; OUR FIRST HYPOTHESIS, THAT THE SUBJECT WAS THE
                                                                    ;; RELWORD, WAS JUST PROVEN WRONG SINCE WE CANNOT
                                                                    ;; PARSE THE VG NEXT. SO WE REVISE OUR FEATURES
        (fq! 'REL-NOT-FOUND)                                          ;; AND JUMP TO PARSE A REAL FULL SUBJECT AS IN
        (GO SUBJ)                                                   ;; "...WHICH MARY THOUGHT WAS CHAUVANISTIC" AS
                                                                    ;; OPPOSED TO "...WHICH WAS CHAUVANISTIC"

    TO
        (| (and (cq 'COMPONENT) (parse 'VG 'TO 'TODEL)) VG1 nil)        ;; "I WANTED TO DANCE AND SING"
        (| (nextword? *n* 'FOR) nil TO1)                                ;; THIS IS EXPERIMENTAL
        (parse nil 'FOR)                                             ;; PLEASE CHECK OUT ANY FOR-CLAUSES YOU CAN THINK OF
        (fq! 'FOR)
        (parse 'NG 'SUBJ 'TOSUBJ)
        (setr *c* 'SUBJECT *h*)

    TO1
        (| (parse 'VG 'TO) VG1 (TO))

    ING
        (| (MOVE-PTW 'N 'NW '(ING)) nil FAIL)
        (| (or (nq 'ING) (cq 'OBJ2) (and (parse 'NG 'SUBJ 'INGSUBJ) (setr *c* 'SUBJECT *h*) (fq! 'SUBING) (rq 'ING)))
            nil
            nil
            (ING))
        (| (parse 'VG 'ING) VG1 (ING))

    REPORT
        (and (nextword? *n* 'THAT) (parse nil 'THAT) (fq! 'THAT))
        (set! *locationmarker* *n*)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)

    ;; ##############################################################
    ;;                            RETURN
    ;; ##############################################################

    RETSM
        (or (SMCL2) (GO FAIL))
        (GO RETURN))

(§ PDEFINE NG nil

    ENTERING-NG

    NGSTART                                                         ;; EXAMINE INITIAL FEATURES AND JUMP TO
        (cond (cq 'RELWD) (GO RELWD)                               ;; CORRESPONDING SPECIAL BLOCKS OF CODE
            (cq 'QUEST) (GO QUEST)
            (or (nq 'QDET) (nq 'QPRON)) (do (fq! 'QUEST) (GO QUEST))
            (cq 'TIME) (GO TIME)                                   ;; LOOK AT FIRST WORD
            (nq 'PROPN) (GO PROPN)
            (nq 'TPRON) (GO TPRON)
            (nq 'EVERPRON) (GO EVERPRON)
            (nq 'PRON) (GO PRON))

    LOOK
        (cond (nq 'DET) (GO DET)                                   ;; THIS POINT MAY BE JUMPED BACK TO
            (nq 'NUM) (GO NUM)
            (or (nq 'ING) (nq 'EN) (nq 'ADJ)) (GO ADJ)
            (nq 'CLASF) (GO CLASF)
            (nq 'NUMD) (GO NUMD)
            (nextword? *n* 'AT) (GO AT)
            (nextword? *n* 'AS) (GO AS)
            (nq 'NOUN) (GO NOUN)
            (nq 'TIMORD) (GO TIMORD)
            (and (cq 'COMPONENT) (isq (MOVE-PT 'PC) 'QUEST)) (GO QUEST)
            :else (GO FAIL (m! 'START)))

    ;; #######################################################
    ;; IF YOU CAN PARSE ANY OF THESE SMALL THINGS, YOU'RE DONE
    ;; #######################################################

    START                                                           ;; PARSE A PROPER NOUN

    PROPN
        (parse 'PROPN)
        (fq! 'DEF 'PROPNG)
        (| (isq *h* 'POSS) PROPS nil)
        (| (and *nn* (nq 'PROPN)) PROPN nil)

    PROPS
        (or (SMPROP) (GO FAIL))                            ;; EXAMINE ITS SEMANTICS
        (| (isq *h* 'POSS) POSS PRAG)

    ;; -------------- PRONOUNS ---------------

    PRON
        (| (parse 'PRON 'POSS) POSS nil RED2)                         ;; IS IT POSSESSIVE?

    PRON2
        (| (cq 'NPRON) (NPRON) nil)
        (| (or (and (cq 'SUBJ) (parse 'PRON 'SUBJ))                    ;; CHECK SUBJECTIVE OR OBJECTIVE CASE
                (and (or (cq 'OBJ) (cq 'TOSUBJ) (cq 'INGSUBJ)) (parse 'PRON 'OBJ))
                (cq 'INGSUBJ))
            nil
            (PRON))
        (fq! 'PRONG 'DEF)

    PRON3
        (| (SMPRON *h*) nil FAIL)                            ;; EXAMINE SEMANTICS OF PN

    PRAG
        (setr *c* 'HEAD *h*)
        (MOVE-PT 'H)
        (trnsf 'NS 'NPL 'NFS 'NEG)                                      ;; MODIFY PN FEATURES TO CORRECT NUMBER
        (GO RETURN)

    ;; -------------- ANYTHING, SOMETHING, ... --------------

    TPRON
        (parse 'TPRON)
        (fq! 'TPRON)
        (MOVE-PT 'H)
        (trnsf 'NS 'NPL 'ANY 'NEG)
        (setr *h* 'HEAD *c*)
        (and *nn* (nq 'ADJ) (parse 'ADJ))
        (GO SMNG)

    ;; ----------- WHATEVER, WHENEVER, WHOEVER, ... -----------

    EVERPRON
        (| (and (parse 'PRON 'EVERPRON) (SMPRON *h*))
            nil
            FAIL)
        (| (and (parse 'CLAUSE 'RSQ 'NOREL) (SMRELATE *h*))
            RETSM
            FAIL)

    AS  (| (and (parse nil 'AS) (parse 'NUMD 'NUMDAS) *nn* (parse nil 'AS))
            NUMD2
            (AS)
            (AS))

    ;; -------------- AT + NUM ---------------

    AT
        (| (and (parse nil 'AT) (parse 'NUMD 'NUMDAT)) nil (AT) (AT))
    NUMD2
        (| (and (parse 'NUM) (fq! 'NUM 'NUMD)) DET1 (NUMD2) INCOM)

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
        (| (and (parse 'NOUN 'TIM1) (fq! 'DET 'DEF) (SMNGTIME))
            RETURN
            FAIL)

    ;; #################################################
    ;;     THE MAINSTREAM - MORE CMPLICATED NG TYPES
    ;; #################################################

    ;; -------------- PARSE A DETERMINER ---------------

    DET
        (parse 'DET)
        (fq! 'DET)
        (MOVE-PT 'H)                                                 ;; SHIFT PTR TO THE DETERMINER
        (| (trnsf 'NPL 'NS 'PART 'DEF 'INDEF 'ANY 'NEG 'QNTFR)
            IND
            (bug nil)
            INCOM)

    ;; -------------- INDETERMINATE ---------------

    IND
        (| (and (= (word (firstword *h*)) 'ALL) (= (word *n*) 'THE) (parse 'DET) (fq! 'DEF))
            NUM
            nil
            (THE))
        (| (and (isq *h* 'QNTFR) (fq! 'QNTFR)) QNUM nil)

    ;; -------------- ORDINALS AND NUMBERS ---------------

    ORD
        (| (and (parse 'ORD) (fq! 'ORD)) nil NUM INCOM)
        (| (and (nextword? *n* 'OF)                                     ;; TWELTH OF OCTOBER...
                (isq (MOVE-PTW 'N 'NW) 'MONTH)
                (parse nil 'OF)
                (parse 'NOUN 'MONTH)
                (fq! 'DATE))                                          ;; REMEMBER THAT FEATURES ARE DESIGNED AS AIDS TO
            RETSM                                                   ;; SEMANTIC COMPREHENSION AS WELL AS SYNTACTIC PARSING.
            nil)
        (| (cq 'DEF) nil ADJ)

    NUM
        (| (parse 'NUM) nil ADJ)                                     ;; LARGE JUMP IF FALSE
        (fq! 'NUM)
        (| (cq 'DET) nil DET1)
        (| (cond (and (isq *h* 'NS) (cq 'NS)) (rq 'NPL 'PART) (cq 'NPL) (rq 'NS 'PART))
            ADJ
            (NUM)
            INCOM)

    DET1
        (if (isq *h* 'NS) (fq! 'NS) (fq! 'NPL))                    ;; EXPLICIT CHECK FOR THE VALUE 1
        (or *nn* (and (fq! 'NUMBER) (GO INCOM)))

    NUMBER
        (fq! 'DET)
        (| (nq 'OF) OF ADJ)

    QNUM
        (| (isq *h* 'NONUM) OF nil)
        (| (and (parse 'NUM) (fq! 'NUM)) nil OF)
        (| (cond (== (semantics *h*) 1) (and (cq 'NS) (rq 'NPL)) (cq 'NPL) (rq 'NS)) ;; EXPLICIT CHECT FOR THE VALUE 1
            nil
            (NUMD)
            INCOM)
        (| (= (word (firstword *h*)) 'NO) ADJ nil)                          ;; CHECKS FOR WORD "NO"

    ;; -------------- PREPG WITH "OF" ---------------

    OF  (| (and (nq 'OF) (parse 'PREPG 'OF)) SMOF NONE)                ;; "FIVE OF THE BLOCKS"

    SMOF
        (fq! 'OF)
        (| (or (SMNGOF) (not (POP))) RETSM INCOM)

    NONE
        (| (= (word (firstword *h*)) 'NONE) INCOM ADJ)

    ;; ----------- PARSE ALL THE ADJECTIVES -----------

    ADJ
        (| (parse 'ADJ) nil EPR INCOM)
        (and (isq *h* 'COMPAR)
            (fq! 'COMPARATIVE-MODIFIER)
            (setr *c* 'COMPARATIVE-MODIFIER *h*))
        (GO ADJ)

    EPR
        (| (or (isq *h* 'SUP) (isq *h* 'COMPAR)) nil CLASF INCOM)         ;; WE PARSED AN ADJ AND RAN OUT OF WORDS
        (fq! 'ADJ)
        (and (nextword? *n* 'OF)
            (parse 'PREPG 'OF)
            (or (SMNGOF) (GO FAIL))
            (fq! 'OF)
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

        (| (and (cq 'TIME) (not (isq *h* 'TIM1))) RED1 nil)

    ;; -------------- MODIFY FEATURES FOR NUMBER AND SUCH --------------

        (set! *tmp* *fe*)
        (when (and (isq *h* 'MASS) (or (cq 'PART) (not (cq 'DET)))) (fq! 'MASS))
        (when-not (isq *h* 'NPL) (rq 'NPL 'PART))
        (when-not (isq *h* 'NS) (rq 'NS))
        (when (and (not (cq 'DET)) (not (cq 'NUMD))) (MOVE-PT 'H) (trnsf 'NPL 'MASS))
        (| (meet *fe* '(NS NPL PART MASS)) nil RED0)

        (| (nextword? *n* 'THAN) nil SMNG)                              ;; "...A BIGGER BLOCK THAN..."
        (fq! 'THAN)                                                   ;; THE PRESENCE OF THIS FEATURE IS NOTED BELOW AND IN ADJG

    ;; AT THIS POINT SMNG1 IS CALLED FOR PRELIMINARY CHECKS AND ANALYSIS BEFORE CHECKING QUALIFIERS

    SMNG
        (setr *c* 'HEAD *h*)                                            ;; SET HEAD REGISTER TO THE NOUN

        (| (and (cq 'OBOFJ) (not (cq 'DEF))) FAIL nil)                ;; JUST PARSED
        (or (SMNG1) (GO FAIL))
        (| (not (isq *h* 'POSS)) nil POSS RETSM)                       ;; CHECK FOR POSSIVE

    ;; #################################################
    ;;               POSSIBLE QUALIFIERS
    ;; #################################################

        (| (and (cq 'THAN) (parse 'ADJG)) nil RSQ-TO)                 ;; "...A BIGGER BLOCK THAN..."
        (| (SMRELATE *h*) RETSM FAIL)

    RSQ-TO
        (| (and (nextword? *n* 'TO) (meet *fe* '(COMP SUBJ)) (parse 'CLAUSE 'RSQ 'TO) (or (SMRELATE *h*) (GO POPRET)))
            RETSM
            nil)

    ;; -------------- AS OR COMPARATIVE ---------------

        (| (and (or (nextword? *n* 'AS) (nq 'COMPAR)) (parse 'ADJG 'THANNEED))
            nil
            PREPNG)                                                 ;; WHAT IS THE REASON FOR THE EXISTANCE OF THIS
        (and (nil? *n*)                                               ;; STRANGE ANIMAL (ALSO THE ONEBELOW) -- CHECK
            (cq 'SUBJ)                                               ;; THEM OVER AND HACK THEM PROPERLY
            (isq (MOVE-PT 'C 'PV) 'AUX)
            (isq *pt* 'BE)
            (GO POPRET))                                            ;; AVOIDS ATTACHING MODIFIER WHEN IT GOBBLES TO
        (| (SMRELATE *h*) RSQ-TO POPRET RETSM)               ;; MUCH E.G. IS THE BLOCK ON THE TABLE? DOESN'T
                                                                    ;; WANT "THE BLOCK ON THE TABLE" AS A CONSTITUENT.
                                                                    ;; I ADMIT ITS A HACK.

    ;; -------------- ANY SORT OR PREPOSITION GROUP --------------

    PREPNG
        (| (and (nq 'PREP)
                (not (or (and (nq 'PLACE) (cq 'NOLOC))
                    (and (cq 'OBJ1)
                        (isq *mvb* 'TRANSL)
                        (not (isq (MOVE-PT 'C 'U) 'QUEST)))))
                (parse 'PREPG 'Q))
            nil
            DISGRSQ)
        (and (nil? *n*)
            (cq 'SUBJ)
            (isq (MOVE-PT 'C 'PV) 'AUX)
            (isq *pt* 'BE)
            (not (isq (MOVE-PT 'U) 'NGQ))
            (GO POPRET))
        (| (SMRELATE *h*) RSQ-TO POPRET RETSM)

    ;; CHECK FOR DISGUISED RSQ CLAUSES BY READING THE FAILURE MESSAGES SENT UP FROM PREPG.

    DISGRSQ
        (| (= (car *mes*) 'PREP-WHICH) nil RSQ)
        (set! *mes* (cdr *mes*))
        (| (parse 'CLAUSE 'RSQ 'PREPREL) PREPNG (RSQ-PREPREL) RETSM)

    ;; -------------- ANY OTHER RSQ ---------------

    RSQ
        (| (and (isq (MOVE-PT 'C 'U) 'POLR2) (cq 'SUBJ) (nq 'VB) (not (cq 'SUBJT)) (not (isq *pt* 'QADJ)))
            RETSM
            nil)
        (| (parse 'CLAUSE 'RSQ) nil RETSM)
        (| (SMRELATE *h*) RETSM POPRET)

    ;; -------------------------------------------------
    ;; THE ENTIRE NG SHOULD HAVE BEEN PROCESSED BY THIS POINT
    ;; -------------------------------------------------

    ;; -------------------------------------------------
    ;; IF AT FIRST YOU DON'T SUCEED.......
    ;; -------------------------------------------------

    RED0
        (set! *fe* *tmp*)
    RED1
        (POP)
    RED2
        (cond (nil? *h*) (GO FAIL (m! 'NO))
           (isq *h* 'NUMBER) (GO INCOM)
           (and (isq *h* 'POSS) (or (isq *h* 'PRON) (and (MOVE-PT 'H 'DLC) (isq *pt* 'PRON)))) (do (POP) (GO PRON2))
           (and (nil? (cdr *h*)) (cq 'DEFPOSS)) (GO POSSDEF)
           (and (cq 'QUEST) (nil? (cdr *h*))) (GO QDETCHECK)         ;; (CDR H) = T IF THERE IS ONLY ONE DAUGHTER TO THE CURRENT NODE
           (isq *h* 'ADJ) (GO EPR)
           (not (isq *h* 'CLASF)) (GO INCOM))

    REDUC
        (POP)
        (| (and (nil? *h*) (nq 'PROPN)) PROPN NOUN)

    POPCOM
        (POP)

    ;; -------------- INCOMPLETE PHRASES ---------------

    INCOM
        (fq! 'INCOM)
        (| (and (isq *h* 'DET) (isq *h* 'INCOM) (SMINCOM))
            RETURN
            nil)
        (| (and (nil? *cut*) (cq 'NUM)) SMNG nil)

    QDETCHECK
        (cond (and (isq *h* 'QDET) (isq (firstword *h*) 'QPRON)) (do (POP) (GO QPRON))
            (and (isq *h* 'QDET) (isq (firstword *h*) 'EVERPRON)) (do (POP) (GO EVERPRON)))
        (GO FAIL)

    ;; -------------------------------------------------
    ;; POSSESSIVE HANDLER
    ;; -------------------------------------------------

    POSS
        (or (SMNG2) (GO FAIL))

    POSS2
        (| (cq 'INGSUBJ) RETSM nil)
        ;; IF POSSESSIVE, ALL PREVIOUS MODIFIERS MODIFY THE POSSESSIVE NOUN, NOT THE NG HEAD
        (set! *h* (buildnode (reverse (cons 'POSS (setdif *fe* '(COMPONENT)))) *nb* *n* *h* *sm*))
        (set! *backref* (concat *h* (cdr *backref*)))
        (| (setr *c* 'FEATURES (set! *fe* (concat '(POSES DET DEF NS NPL) (reverse *rest*))))
            nil
            (bug nil))
        (| (or (not *nn*) (isq *h* 'DEFPOSS)) nil ORD)

    POSSDEF ;; THE PLACEMENT OF THIS TAG IS A GUESS. THE ORIGINAL IS LOST, ASSUMING THAT IT EVER EXISTED
        (rq 'POSES 'DET 'DEF)
        (fq! 'POSSDEF 'NS 'NPL)

    ;; -------------- RELATIVES---------------

    QUEST
        (| (parse nil 'HOW) nil QDET FAIL)
        (| (parse nil 'MANY) nil FAIL INCOM)
        (fq! 'DET 'NPL 'INDEF 'HOWMANY)
        (GO OF)

    QDET
        (| (and (parse 'DET 'QDET) (fq! 'DET 'NPL 'QDET 'NS))
            QNUM
            nil
            INCOM)

    QPRON
        (| (parse 'PRON 'QPRON) PRON3 FAIL)

    RELWD
        (| (and (parse 'PRONREL) (smset (semantics (MOVE-PT 'C 'U 'U '(NG)))))         ;; SET SM TO THE NOUNGROUP DIRECTLY UPSTAIRS
            RETURN
            nil)

    POPRET
        (POP)

    ;; --------------------------------------------------------------
    ;; RETURN AFTER CALLING SMNG2 TO PROCESS THE COMPLETED NOUN GROUP
    ;; --------------------------------------------------------------

    RETSM
        (or (SMNG2) (GO TRYA))
        (GO RETURN)

    ;; -------------- YOU PROBABLY GOOFED, CUT AND TRY AGAIN. --------------

    TRYA
        (| (isq *h* 'NOUN) nil (TRYA))
        (POP)
        (cut *n*)

    UP
        (| (POP) UP nil)                                            ;; POP EVERYTHING OFF
        (set! *fe* (reverse *rest*))
        (smset nil)
        (GO NGSTART))

(§ PDEFINE VG (*tense*)

    ;; ##################################################################
    ;; CHECK INITIAL FEATURES TO SEE IF SOME SPECIAL TYPE OF VG IS WANTED
    ;; ##################################################################

    ENTERING-VG
        (cond (cq 'TO) (GO TO)
            (cq 'EN) (GO EN)
            (cq 'ING) (GO ING)
            (cq 'IMPER) (GO IMPER)
            (isq (MOVE-PT 'C 'U) 'POLR2) (GO POLR2))                 ;; CHECKS IF THE CLAUSE IS MARKED AS POLR2

    ;; -------------- DISPATCH TABLE FOR EXAMINEING THE FIRST WORD ---------------

    NEW                                                             ;; PARSE THE FIRST WORD WITH APPROPRIATE FEATURES
        (cond (not (nq 'VB)) (GO FAIL (m! 'VB))                     ;; AND JUMP TO CODE THAT KNOWS WHAT SHOULD BE
            (and (nq 'DO) (parse 'VB 'AUX 'DO)) (GO DO)               ;; LOOKED FOR NEXT IN EACH CASE
            (and (nq 'MODAL) (parse 'VB 'AUX 'MODAL)) (GO MODAL)
            (and (nq 'WILL) (parse 'VB 'AUX 'WILL)) (GO WILL)
            (and (nq 'BE) (parse 'VB 'AUX 'BE)) (GO BE)
            (and (nq 'HAVE) (parse 'VB 'AUX 'HAVE)) (GO HAVE)
            (not (parse 'VB '(MVB))) (GO FAIL (m! 'VB)))

    SIMPLE
        (MOVE-PT 'C 'DLC)                                             ;; MOVE PT DOWN FROM THE CURRENT NODE BEING PARSED (VG) AND ACROSS TO THE MOST RECENTLY PARSED
        (trnsf 'VPL 'INF 'V3PS)                                         ;; DAUGHTER.  IN THIS CASE THAT DAUGHTER WAS PARSED IN THE DISPATCH TABLE JUST ABOVE
        (set! *tense* (cond (and (isq *pt* 'PRESENT) (isq *pt* 'PAST)) '(PAST-PRESENT) (isq *pt* 'PAST) '(PAST) :else '(PRESENT)))
        (GO REV)

    TO
        (fq! 'NAGR)                                                   ;; "NAGR" MARKS THAT SUBJECT AND MAIN VERB NEED
        (| (and (parse nil 'NOT) (fq! 'NEG)) nil nil (NOT))            ;; NOT AGREE IN NUMBER AND PERSON AND INSURES THAT
        (| (or (parse nil 'TO) (cq 'TODEL)) nil (TO) (TO))            ;; THE AGREEMENT CHECKER AT THE END OF THE PROGRAM
                                                                    ;; ("REV") WILL NOT BE APPLIED "TODEL" MUST BE
        (set! *tense* '(INFINITIVE))                                  ;; GIVEN AS AN INITIAL FEATURE OR ELSE THIS
        (GO MODAL2)                                                 ;; STATEMENT FAILS TENSE IS USED TO HOLD THE TENSE
                                                                    ;; WHILE IT IS BEING COLLECTED.

    EN
        (fq! 'NAGR)
        (| (and (parse nil 'NOT) (fq! 'NEG)) nil nil (NOT))
        (set! *tense* '(PAST))
        (| (and (parse 'VB 'EN '(MVB)) (setmvb *h*) (fq! 'PASV)) RETSM FAIL) ;; DONE AT "EN2"

    ING
        (fq! 'NAGR)
        (| (and (parse nil 'NOT) (fq! 'NEG)) nil nil (NOT))

    INGADV
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) INGADV nil)
        (set! *tense* '(PRESENT))
        (GO BE2)

    IMPER
        (| (and (parse 'VB 'DO 'NEG 'INF) (fq! 'NEG)) nil nil (DONT))
        (| (and (parse 'VB '(MVB) 'INF) (setmvb *h*) (SMVG))
            RETURN
            (IMPER))                                                ;; MVB IS BOUND BY CLAUSE

    POLR2                                                           ;; THE CLAUSE COULD ONLY BE MARKED AS "POLR2"
        (or (set! *pt* (getr (MOVE-PT 'C 'U) 'QAUX))                    ;; ("DID THE...?") IF AN AUX OF SOME VERIETY HAD
            (and (bug "VGßPOLR2") (GO FAIL)))                         ;; ALREADY BEEN PARSED, IF THAT IS NOT THE CASE,
        (set! *h* (list (car *pt*)))                                    ;; THEN WE HAVE A BUG IN THE PROGRAM SOMEWHERE SET
        (trnsf 'NEG)                                                 ;; THE INITIAL DAUGHTER OF THE VG TO BE THE
        (cond (isq *h* 'DO) (GO DO)                                  ;; PREVIOUSLY PARSED AUX MARK THE VG AS NEG IF
            (isq *h* 'MODAL) (GO MODAL)                              ;; APPROPRIATE (SEE PROGMR FILE FOR THE OPPERATION
            (isq *h* 'WILL) (GO WILL)                                ;; OF THIS FUNCTION) DISPATCH TABLE , CHECKING THE
            (isq *h* 'BE) (GO BE)                                    ;; AUX
            (isq *h* 'HAVE) (GO HAVE))
        (ert "BUG VGßPOLR2VB")                                        ;; NOTHING BUT UNGRAMATICAL NONSENSE SHOULD REACH
        (GO FAIL)                                                   ;; THIS POINT

    ;; ###############################################################
    ;; PROCESSING OF VB'S NOT SPECIALLY MARKED FOR BY INITIAL FEATURES
    ;; ###############################################################

    DO  (fq! 'DO)
        (MOVE-PT 'C 'DLC)                                             ;; MOVE TO THE "DO"
        (trnsf 'VPL 'NEG 'INF 'V3PS)                                    ;; ARRANGE ITS FEATURES
        (set! *tense* (if (isq *pt* 'PAST) '(PAST) '(PRESENT)))
        (if *nn* (GO DO2) (GO MVB))                                            ;; GO CONDITIONALY TO THE FIRST TAG IF MORE WORDS
                                                                    ;; REMAIN BEFORE THE CUT POINT, AND TO THE SECOND
                                                                    ;; TAG IF THERE ARE NONE

    DO2 (| (and (parse nil 'NOT) (fq! 'NEG)) nil nil (NOT))

    ADV2
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) ADV2 nil (ADV))
        (| (parse 'VB '(MVB) 'INF) nil MVB)                            ;; "MVB" ARRANGES FOR A CHECK TO INSURE THAT THE
        (GO REV)                                                    ;; VERB BEING PARSED CAN BE A MAIN VERB

    MODAL
        (fq! 'NAGR 'MODAL)
        (set! *tense* '(MODAL))
        (if *nn* (GO MODAL2) (GO INCOMP))

    MODAL2
        (| (and (parse nil 'NOT) (fq! 'NEG)) nil nil (NOT))

    ADV3
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) ADV3 nil (ADV))
        (cond (parse 'VB 'BE 'INF) (if *nn* (GO BE2) (GO MVB))                  ;; DISPATCH TABLE FOR THE NEXT VERB
            (parse 'VB 'HAVE 'INF) (if *nn* (GO HAV2) (GO MVB))
            (parse 'VB 'INF '(MVB)) (GO REV)
            :else (GO INCOMP))

    WILL
        (fq! 'NAGR)
        (set! *tense* '(FUTURE))
        (if *nn* (GO MODAL2) (GO INCOMP))                                      ;; THE SAME POSSIBILITIES FOR THE NEXT VERB APPLY
                                                                    ;; AFTER BOTH WILL AND MODALS

    BE
        (MOVE-PT 'C 'DLC)                                             ;; POINT TO WHAT WAS JUST PARSED
        (trnsf 'VPL 'INF 'V3PS 'VFS)
        (set! *tense* (if (isq *pt* 'PAST) '(PAST) '(PRESENT)))
        (if *nn* (GO BE2) (GO MVB))

    BE2
        (| (and (parse nil 'NOT) (fq! 'NEG)) nil nil (NOT))

    ADV4
        (| (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) ADV4 nil (ADV))
        (cond (and (nextword? *n* 'GOING) (parse 'VB)) (GO GOING)      ;; "...WILL BE GOING TO..."
            (and (nq 'BE) (parse 'VB 'ING))                           ;; "BE BEING"
                (do (set! *tense* (cons 'PRESENT *tense*))
                    (GO EN2))                                           ;; AS IN "BE BEING X'EN(ED)"
            (and (nq 'ING) (parse 'VB 'ING '(MVB)))                    ;; "BE X'ING"
                (do (set! *tense* (cons 'PRESENT *tense*))
                    (GO REV))
            (cq 'ING) (GO FAIL (m! 'ING)))                          ;; IF TRUE, IT IMPLYS THAT WE STARTED OFF WITH
                                                                    ;; "BEING" - AS IN "BEING EATEN CAN BE UNPLEASANT"
                                                                    ;; - OTHERWISE IT IMPLYS THAT WE HAVE SOMETHING
                                                                    ;; OTHER THAN A VG ON OUR HANDS AND SHOULD FAIL TO
                                                                    ;; WHOEVER CALLED US AND TRY TO PARSE IT
                                                                    ;; DIFFERENTLY

    EN2
        (| (parse 'VB 'EN '(MVB)) nil MVBE)                            ;; THIS ASKS: DO WE HAVE A VERB IN ITS EN FORM
                                                                    ;; WHICH CAN ACT AS A MAIN VERB (IN WHICH CASE IT
        (fq! 'PASV)                                                   ;; IS MARKED AS PASSIVE AND WE RETURN) OTHERWISE
        (GO REV)                                                    ;; CHECK IF THE VERB BEING POINTED AT IS A
                                                                    ;; LEGITIMATE FORM OF "BE" IN ITS MAIN VERB SENSE
                                                                    ;; - WHICH IS DONE AT "MVBE"

    GOING
        (| (parse nil 'TO) nil GOI)
        (| (nq 'INF) GOING2 nil nil)
        (POP)

    GOI
        (set! *tense* (cons 'PRESENT *tense*))                          ;; WE HAVE DETERMINED THAT "GOING" IS THE ACTUAL
        (GO MVB)                                                    ;; MAIN VERB AND SHOULD BE PARSED AS SUCH

    GOING2
        (set! *tense* (cons 'FUTURE *tense*))                           ;; HERE WE DETERMINE THAT THE PHRASE IS ACTUALLY
        (GO MODAL2)                                                 ;; OF THE FORM "...IS GOING TO FALL IN LOVE..."
                                                                    ;; AND WE SHOULD RUN THROUGH THE DISPATCH TABLE AT
                                                                    ;; "MODAL2" TO DETERMINE HOW TO CONTINUE

    MVBE
        (| (isq (MOVE-PT 'H 'PV '(VB)) 'AUX) nil MVB)                   ;; MOVE TO EARLIER AND EARLIER DAUGHTERS  UNTILL
        (| (isq *pt* 'BE) nil (MVBE))                                  ;; YOU REACH A VERB WHICH IS A "QAUX" - IF THERE
                                                                    ;; ARE NONE THEN CONTINUE AT "MVB" IF WHAT YOU ARE
                                                                    ;; POINTING TO (THE "QAUX") IS NOT A FORM OF "BE",
        (setmvb *pt*)                                                 ;; THEN FAIL BECAUSE OF THE UNGRAMATICALITY OF THE
        (GO REV)                                                    ;; CONSTRUCTION OF "BE"'S OTHERWISE MARK IT AS THE
                                                                    ;; MVB AND PREPARE TO RETURN

    HAVE
        (MOVE-PT 'C 'DLC)
        (trnsf 'VPL 'INF 'V3PS 'VFS)
        (set! *tense* (if (isq *pt* 'PAST) (do (fq! 'NAGR) '(PAST)) '(PRESENT)))
        (if *nn* (GO HAV2) (GO MVB))                                           ;; HAV2 WILL CATCH "HAVE HAD", OR "HAVE BEEN ..."

    HAV2
        (| (and (parse nil 'NOT) (fq! 'NEG)) nil nil (NOT))            ;; OR "HAVE KISSED"

    ADV5
        (| (parse 'ADV) ADV5 nil (ADV))
        (| (parse 'VB 'BE 'EN) nil HAV3)
        (set! *tense* (cons 'PAST *tense*))                             ;; "HAVE BEEN ..."
        (if *nn* (GO BE2) (GO MVB))

    HAV3
        (| (parse 'VB '(MVB) 'EN) nil MVB)
        (set! *tense* (cons 'PAST *tense*))                             ;; "HAVE KISSED"
        (GO REV)

    INCOMP
        (fq! 'INCOMP)
        (GO FAIL)

    MVB
        (| (= (features *mvb*) (features *h*)) MVB2 nil)
        (POP 'VB)                                                    ;; POP OFF EVERY THING UNTILL YOU REACH A VERB
        (| (parse 'VB '(MVB)) nil (MVB))

    MVB2
        (GO REV)

    ;; -------------------------------------------------
    ;;   CHECK AGREEMENT BETWEEN SUBJECT AND MAIN VERB
    ;; -------------------------------------------------

    REV
        (setr *c* 'TENSE *tense*)
        (and *nn* (parse nil 'NOT) (fq! 'NEG))
        (cond (or (= *tense* '(PAST))
                    (cq 'NAGR)
                    (isq (MOVE-PT 'C 'U) 'IMPER)                       ;; MOVE PT TO THE CLAUSE REMEMBER THAT THE POINTER
                    (isq *pt* 'THERE)                                  ;; STAYS WHERE IT'S PUT UNTILL RETURNING FROM A
                    (isq *pt* 'RSNG))                                  ;; CALL TO PARSE
                (GO NAUX)
            (set! *pt* (getr (MOVE-PT 'C 'U) 'SUBJECT)) *pt*               ;; "SUBJECT" IS THE SYNTACTIC SUBJECT OF THE CLAUSE THAT THE VG IS IN,
            :else (bug "VG -- NO SUBJECT TO CHECK FOR AGREEMENT"))   ;; WHOSE ESSENTIAL DISTINGUISHING FEATURE IS AGREEMENT WITH THE VERB

        (set! *tmp* nil)                                               ;; TMP WILL ACT AS A SWITCH AT "NAGR" BELOW.
                                                                    ;; NOTE THAT IT IS EXPLICITLY SET BY THE CODE BELOW BY THE FOLLOWING CRITERIA:
                                                                    ;; IF TMP IS NON-NIL, THEN SUBJECT AND VERB HAVE BEEN DETERMINED TO AGREE.
                                                                    ;; IF IT IS NIL, THEN THEY WILL BE CONSIDERED TO AGREE ONLY IF THE FEATURE "PAST-PRESENT" IS ON THE MVB,
                                                                    ;; IN WHICH CASE, THIS IS EVIDENCE THAT THE PROPER CHOICE OF TENSE IS PAST,
                                                                    ;; WHERE AGREEMENT IS IRRELEVANT (SEE BELOW AT "NAGR").
        (cond (isq *pt* 'NFS)
                (or (set! *tmp* (meet *fe* '(VFS INF))) (GO NAGR))
            (isq *pt* 'CLAUSE) (or (set! *tmp* (cq 'V3PS)) (GO NAGR))
            (or (isq *pt* 'NS) (isq *pt* 'MASS))
                (or (and (cq 'V3PS) (set! *tmp* true))
                    (feset *pt* (setdif (features *pt*) '(NS MASS)))))
        (when (or (isq *pt* 'PART) (isq *pt* 'NPL))
            (or (and (meet *fe* '(INF VPL)) (set! *tmp* true))
                (feset *pt* (setdif (features *pt*) '(PART NPL)))))

    NAGR
        (| (or *tmp*
            (and (= '(PAST-PRESENT) *tense*)                      ;; NOTES WHETHER VERB AND SUBJECT WERE FOUND TO
                (set! *tense* '(PAST))))                              ;; AGREE AND FAILS UNLESS A SPECIAL CONDITION
            nil                                                     ;; EXISTS AS NOTED DIRECTLY ABOVE
            (NAGR))

    NAUX
        (setmvb (or (MOVE-PT 'H 'PV '(MVB)) *mvb*))
        (| (and (cq 'NAUX) (isq (MOVE-PT 'H 'PV '(VB)) 'AUX) (not (MOVE-PT 'PV 'PV '(VB))))
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
        (| (SMVG) RETURN FAIL))

(§ PDEFINE PREPG nil

    ENTERING-PREPG

    ADV
        (| (and (nq 'PREPADV) (parse 'ADV 'PREPADV)) ADV nil (PREPADV)) ;; CHECK FOR ANY INITIAL MODIFING ADVERBS
        (| (cond (cq 'AGENT) (nextword? *n* 'BY)                       ;; EXAMINE THE INITIAL FEATURES OF THE PREPG TO
            (cq 'LOC) (nq 'PLACE)                                   ;; CHECK FOR CONSTRAINTS ON THE PREPOSITION
            (cq 'Q) (not (nq 'MOTOR))
            :else true)
            nil
            (PREP))                                                 ;; FAIL IF THE CONSTRAINTS AREN'T MET

        ;; PARSE THE PREPOSITION

        (| (parse 'PREP) nil (PREP))
        (MOVE-PT 'H)
        (trnsf 'PLACE 'TIME)                                          ;; THIS IS NOT EXACTLY RIGHT, SINCE "ON WHAT DAY" IS NOT "PLACE"

        ;; AT THIS POINT THE POSSIBILITIES ARE:
        ;;   1. THERE ARE NO MORE WORDS AND THE PREP IS "SHORT"
        ;;   2. YOU HAVE A MULTIPLE WORD PREPOSITION
        ;;   3. IT IS INDEED A SINGLE WORD PREP, PARSE ITS OBJECT

        (set! *tmp* *h*)                                                 ;; SAVE THE PREPOSITION JUST PARSED IN CASE IT IS
        (and (nq 'PREP2)                                             ;; ONLY THE FIRST WORD OF A MULTIPLE WORD PREPOSITION
            (cond (set! *tmp* (combination? (word (firstword *h*)) (word *n*)))
                    (parse 'PREP2)
                (set! *tmp* (combination? (word (firstword *h*)) (word *n*) (word (cdr *n*))))
                    (do (parse 'PREP2)
                        (parse 'PREP2)))
            (set! *tmp* (buildnode (features *tmp*) *nb* *n* 'WORD (semantics *tmp*)))        ;; CREATE NODE FOR THE COMPOUND WORD
            (setr *tmp* 'PARENT *c*))
        (| (isq *h* 'NEED2) (NEED2) nil)                               ;; FAIL IF LAST PARSED NEEDS ANOTHER WORD

                                                                    ;; GIVE IT A PARENT
        (setr *c* 'HEAD *tmp*)                                           ;; SET THE REGESTER "PREP" TO THE CONSTITUENT JUST
                                                                    ;; PARSED - IF IT WAS A MULTIPLE-WORD-PREP THEN
        (when-not *nn* (GO SHORT))                                          ;; "PREP" IS SET TO THE NODE WHICH CONTAINS THE
                                                                    ;; ENTIRE FORM NN POINTS TO WHATEVER WORDS ARE
                                                                    ;; LEFT BEFORE THE CUT POINT

        ;; ADD FEATURES TO THE PREPG DEPENDING ON THE PREPOSITION PARSED

        (when (= (word *h*) 'BY) (fq! 'AGENT))

    ;; ###################################
    ;; PARSE THE OBJECT TO THE PREPOSITION
    ;; ###################################

    QUEST
        (| (cq 'QUEST) nil NG)
                                                                    ;; CERTAIN RESTRICTIONS PLACED ON THE POSSIBLE
                                                                    ;; NOUN GROUPS THAT IT CAN TAKE - HENCE THE
                                                                    ;; SPECIAL CALL TO PARSE AS ABOVE, IF THE PREPG IS
        (| (parse 'NG 'QUEST 'OBJ) OBJR (PREPQUEST))                   ;; MARKED WITH THE FEATURE "QUEST" (INDICATING
        (| (and (cq 'OF) (parse 'NG 'OFOBJ)) OBJR nil)                 ;; THAT IT SHOULD CONTAIN THE QUESTION ELEMENT OF

    NG
        (| (parse 'NG 'OBJ) OBJR nil)                                 ;; THE CLAUSE) THEN WE PARSE IT SPECIALLY SIMPLE

    REL
        (| (nextword? *n* 'WHICH) nil REST)                             ;; NOUN GROUP - NO RESTRICTIONS
        (| (isq (MOVE-PT 'U) 'CLAUSE) nil (PREP-WHICH))               ;; IF THE NEXT WORD IS A RELWORD, SUCH AS "WHICH"
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
        (| (isq *pt* 'PRONREL) nil PRONREL)                            ;; CHANGES ITS REQUEST FROM (PARSE PREPG Q)  TO
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
        (setr *c* 'OBJ1 (getr *pt* 'HEAD))                              ;; THE REGISTER IS ACCESSED BY CODE IN THE PASSIVE
        (GO RETT)                                                   ;; SECTION OF CLAUSE AND BY THE APPROPRIATE

    REST
        (| (parse 'CLAUSE 'RSNG 'ING) OBJR SHORT)                      ;; SEMANTIC SPECIALIST "HEAD" IS HERE THE HEAD OF

    OBJR
        (setr *c* 'OBJ1 *h*)                                            ;; THE HIGHER NOUNGROUP
        (GO RETT)

    SHORT
        (| (meet *fe* '(NOSHORT Q)) (SHORT) nil)
        (or (isq (MOVE-PT 'C 'U) 'REL-NOT-FOUND)
            (isq (getr *pt* 'QUESTION-ELEMENT) 'QADJ)
            (GO FAIL))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (add-f-pt 'PREPREL *pt*)
        (setr *c* 'OBJ1 (getr (MOVE-PT 'C 'U) 'RELHEAD))

    ;; IF THE REFERENT OF THE RELATIVE CLAUSE THIS SHORT
    ;; PREPOSITION IS ASUMED TO BE IN, HAS NOT BEEN DETERMINED,
    ;; THEN SET THE REGISTER FOR THE OBJECT OF THE PREP.  TO THE
    ;; RELWORD.  IF THERE IS NO RELWORD THEN THE PREPG FAILS
    ;; AFTER SENDING UP A COMPLAINING MESSAGE.

    ;; ---------------  FINAL CHECKS, AND RETURN ---------------

    ;; CHECK IF THIS PREPG SHOULD BE MARKED AS CONTAINING A QUESTION ELEMENT.
    ;; IE. "FOR WHAT", "BETWEEN THE RED BLOCK AND WHICH?" (ECHO)

    RETT
        (and (or (isq *h* 'QUEST)                                      ;; H IS THE NG FOUND FOR AN OBJECT
            (and (isq *h* 'COMPOUND)                                   ;; IF THE NOUN GROUP IS COUMPOUND, CHECK EACH
                (MOVE-PT 'H 'H 'PV '(QUEST))))                          ;; COMPONENT FOR THE FEATURE "QUEST"
            (fq! 'QUEST))
        (| (SMADJG-PREPG) RETURN FAIL))

(§ PDEFINE ADJG nil

    ENTERING-ADJG                                                   ;; THIS LABEL IS MARKED BY DEBUGGING ROUTINES AND

    COMPCHECK                                                       ;; IS USEFUL FOR FOLLOWING THE FLOW OF CONTROL
        (| (and (MOVE-PT 'C 'U '(BE)) (not (cq 'COMP))) FAIL nil)       ;; CONDITIONS WHICH MUST BE MET BY ANY ADJECTIVE
                                                                    ;; GROUP IF THERE IS A FORM OF "BE" IN THE HIGHER
                                                                    ;; CLAUSE, THEN THE ADJG SHOULD HAVE BEEN CALLED
                                                                    ;; WITH THE FEATURE "COMP" FOR COMPLIMENT

        ;; EXAMINE THE INITIAL FEATURES (THOSE DESIGNATED BY THE
        ;; CALLING PROGRAM) ALSO EXAMINE THE NEXT WORD - THESE GIVE
        ;; CLUES AND CONSTRAINTS TO THE STRUCTURE TRYING TO BE PARSED
        ;; AND DIRECT JUMPS TO THE APPROPRIATE SECTIONS OF CODE

        (| (isq (MOVE-PT 'C 'U) 'THAN) nil DISP)                       ;; THE WORD "THAN" WAS DETECTED BY THE IMMEDIATELY
                                                                    ;; UPSTAIRS NG AS FOLLOWING THE HEAD NOUN
        (setr *c* 'HEAD (getr *pt* 'COMPARATIVE-MODIFIER))              ;; INDICATING A STURCTURE SUCH AS "... A BIGGER
        (GO THAN)                                                   ;; BLOCK THAN THAT ONE ..." "HEAD REFERS TO THE
                                                                    ;; ADJG'S HEAD ADJECTIVE

    DISP
        (| (and (nq 'AS) (parse nil 'AS)) AS nil (AS))
        (| (and (nq 'AS) (parse nil 'AS)) AS nil (AS))
        (| (nextword? *n* 'HOW) HOW ADV)

    ;; --------------- HOW + ADJG ----------------

    HOW
        (| (and (parse nil 'HOW) (fq! 'QUEST)) nil FAIL FAIL)
        (| (and (parse 'ADJ) (fq! 'ADJ) (setr *c* 'HEAD *h*)) RETSM nil)
        (| (and (parse 'ADV 'VBAD) (fq! 'VBAD) (setr *c* 'HEAD *h*)) RETSM FAIL)

    ADV
        (| (parse 'ADV 'ADVADV) ADV nil POPAD)                        ;; THIS LOOPS UNTILL ALL CONTIG- UOUS ADVERBS HAVE
        (| (parse nil 'MORE) nil ADJ)                                ;; BEEN PARSED "MORE" IS EXPLICITLY CHECKED FOR
        (fq! 'COMPAR)                                                 ;; SINCE IT SIGNALS THE FEATURE, COMPARATIVE

    ADJ
        (| (if (cq 'ADV) (parse 'ADV 'VBAD) (parse 'ADJ))
            nil
            (ADJ))                                                  ;; IF THE CUT POINT WAS REACHED THEN NO MORE
        (| (setr *c* 'HEAD *h*) nil nil RETSM)                          ;; PROCESSING (SUCH AS COMPAR BELOW) IS POSSIBLE.

    ;; -------------------------------------------
    ;;               COMPARATIVES
    ;; -------------------------------------------

    ;; IF THE FEATURE "COMPAR" IS ALREADY ON THE LIST, OR IF THE JUST PARSED ADJECTIVE CAN HAVE THAT FEATURE, THEN
    ;; ATTEMPT TO PARSE SOME SORT OF COMPARATIVE CONSTRUCTION (ASSUMING THAT THEREARE ANY MORE WORDS BEFORE THE CUT POINT.)

        (| (or (cq 'COMPAR) (isq *h* 'COMPAR)) nil RETSM)
        (fq! 'COMPAR)
        (| *nn* nil RETSM)                                            ;; IF THERE ARE UNPARSED WORDS LEFT BEFORE THE CUT
                                                                    ;; POINT THEN THE POSSIBILITY OF MORE COMPLICATED
                                                                    ;; FORMS IS CHECKED FOR

    THAN
        (when-not *nn* (GO RETSM))
        (| (parse nil 'THAN) nil RETSM (THAN))
        (rq 'THANNEED)                                               ;; THE FEATURE "THANNEEED" MARKS THAT THE WORD
        (fq! 'THAN)                                                   ;; "THAN" IS EXPLICITLY  REQUIRED IN THE PHRASE.
        (GO SUBJ)

    AS
        (fq! 'AS)
        (rq 'THANNEED)
        (| (and (parse 'ADJ) (setr *c* 'HEAD *h*)) nil (ADJ) RETSM)
        (| (parse nil 'AS) SUBJ RETSM (AS))

    ;; FIND A SUBJECT FOR THE COMPARATIVE
    ;; IE.  "AS BIG AS ..." , "BIGGER THAN ..."

    SUBJ
        (| (parse 'NG 'SUBJ 'COMPAR) nil (THAN))
        (| (setr *c* 'OBJ1 *h*) nil nil RETSM)
        (| (and (one-word-left *nb*) (parse 'VB 'AUX)) nil RETSM)
        (| (CHECK-AGREEMENT *h* (cdr *h*))                              ;; CHECKS FOR AGREEMENT IN NUMBER AND PERSON
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
        (| (cq 'THANNEED) (THANNEED) nil)                            ;; IF ONE OF THE WORDS PARSED REQUIRED A "THAN",
        (| (SMADJG-PREPG) RETURN (SMADJ)))                 ;; FAIL IF ONE WAS NOT FOUND.

(§ defn- CONJ []
    (let [*end* *cut* goodie (apply-grammar 'CONJOIN)]
        (when goodie (set! *re* goodie))))

(§ defn- COMMA []
    (COND ((secondword? *n* '\") (flushme) true)                           ;; IF " FOLLOWS, FLUSH COMMA AND CONTINUE
        ((CONJ))                                                    ;; IF COMMA IS PART OF CONJOINED STRUCTURE, GREAT
        ((isq *re* 'INIT) (flushme) true)))                               ;; IF COMMA FOLLOWS INITIAL-TYPE PHRASE, FLUSH IT AND CONTINUE DIRECT ADDRESS JAZZ

(§ PDEFINE CONJOIN (*prev*)

    ;; THIS PROGRAM IS CALLED TO PARSE A CONJOINED STRUCTURE THE
    ;; FIRST MEMBER OF THE SUPPOSED STRUCTURE HAS ALREADY BEEN
    ;; PARSED AND IS SET TO THE INITIAL DAUGHTER (H) OF THIS NODE
    ;; AN ATTEMPT IS MADE TO PARSE AS MANY CONSTITUENTS JUST LIKE
    ;; IT AS IS POSSIBLE

    ENTERING-CONJOIN                                                ;;  HACK LABEL FOR LABELTRACER

    UP
        (set! *prev* (nextword *n*))
        (flushme)
        (when (and (= *prev* '\,)                                   ;; IF WE HAVE A COMMA AND
                (or (cdr *h*)                                         ;; IF MORE THAN 1 COMPONENT HAS BEEN PARSED
                (> (- (count (firstword *h*))               ;; OR IF THAT ONE COMPONENT
                            (count (wordafter *h*)))                         ;; IS MORE THAN 4 WORDS LONG
                    4))
                (memq (nextword *n*) '(OR AND NOR BUT))
                (f! (nextword *n*)))                                     ;; THEN CHECK FOR COMMA COMBINATION
            (set! *prev* (list *prev* (nextword *n*)))
            (flushme))
        (and (term? *prev*)
            (MOVE-PTW 'N 'NW '(= (word *ptw*) *prev*))
            (cut *ptw*))
        (and (or (= *prev* 'BUT) (= (cadr *prev*) 'BUT))
            (nextword? *n* 'NOT)                                        ;; CHECK FOR BUT-NOT COMBINATION
            (or (flushme) (GO LOSE2))
            (fq! 'NEGBUT))
        (| (cond (memq (car *rest*) '(ADJ NUM NOUN PREP VB ADV))
                (PARSE3 (concat *rest* '(COMPONENT)) nil)
            (memq (car *rest*) '(NG PREPG ADJG))
                (and (not (cq 'OFOBJ)) (PARSE2 (concat *rest* '(COMPONENT)) nil))
            (= (car *rest*) 'CLAUSE)
                (let [*lastsent* (if (isq *h* 'MAJOR) *h* *lastsent*) auxfe (meet (features *h*) '(DECLAR IMPER))]
                    (and (PARSE2 (concat *rest* auxfe '(COMPONENT)) nil)
                        (or (not auxfe) (f! (car auxfe)))
                        (setr *c* 'TIME (getr *h* 'TIME)))))             ;; MARK COMPOUND CLAUSE AS TO DECLAR/IMPER FOR ANSGEN
            nil
            LOSE2)
        (cut *end*)                                                   ;; RESTORE CUT POINT
        (cond (not (term? *prev*))
                ;; IF WE HAD COMMA FOLLOWED BY (AND OR BUT NOR), RETURN THE LIST OF GOODIES WE'VE FOUND
                (GO RETSM)
            (= *prev* '\,)
                (if (nextword? *n* COMMA) (do (fq! 'LIST) (GO UP)) (GO LIST))
            (memq *prev* '(AND OR NOR BUT))
                (do (when (= *both* (firstword *h*)) (fq! 'BOTH))
                    (if (or (nextword? *n* 'BUT)
                            (and (nextword? *n* *prev*)
                                (not (and (= *both* (firstword *h*))              ;; IF WE HAD THE 'BOTH' WORD AND
                                    (= *prev* 'AND)))))                  ;; IF THE 'BOTH' WORD WAS "AND", STOP PARSING
                            (do (fq! 'LISTA)                                  ;; ELSE GO LOOK FOR THE NEXT COMPONENT
                                (f! *prev*)
                                (GO UP))
                        (GO LISTA))))

    LOSE2
        (| (cq 'LISTA) LISTA nil)

    LIST
        (| (and (= *prev* '\,)                                       ;; COME HERE FOR ABORTED LIST AND CHECK FOR APPOSITIVE
                (== (count *h*) 2)
                (isq *h* 'NG)
                (not (or (isq *h* 'PRONG) (isq (cdr *h*) 'PRONG)))
                (or (nextword? *n* COMMA) (nil? *n*)))
            nil
            (CONJOINß HOPELESS LIST))
        (flushme)                                                   ;; GET RID OF TRAILING COMMA
        (fq! 'APPOSITIVE)
        (GO RETSM)

    LISTA
        (f! *prev*)

    RETSM
        (fq! 'COMPOUND)                                               ;; CALL SEMANTICS AND RETURN EVERY PARSED BY THIS
        (and (> (count *h*) 2) (fq! 'LIST))                     ;; GOODIE IS COMPOUND IF MORE THAN 2 COMPONENTS
        (cond (or (cq 'NG) (cq 'NOUN))
                (if (cq 'AND) (fq! 'NPL)
                    (do (MOVE-PT 'H) (trnsf 'NPL 'NS 'MASS 'NFS)))
            (cq 'VB)
                (let [common (getprop 'VB 'ELIM)]
                    (MAP #(SETQ common (meet common (features %))) *h*)
                    (feset *c* (union common (features *c*)))))
        (| (SMCONJ) RETURN (CONJOINß SMCONJ)))             ;; THEN MARK AS A LIST

(defn- doublequoter [] (apply-grammar 'PARSEQUOTED))

(§ defn- CANTAKE [num type feature]
    (let [vbfeat (features *mvb*)]
        (cond
            (memq 'RSNG type)
                (memq (READLIST (concat
                        (cond (memq 'TO type) '(T O) (memq 'ING type) '(I N G) (memq 'REPORT type) '(R E P))
                        '(O B)
                        (list (if (== num 1) '\1 '\2))))
                    vbfeat)
            (memq 'COMP type)
                (memq 'INT vbfeat)
            (memq 'NG type)
                (if (== num 1) (not (nil? (meet '(TRANS TRANS2 TRANSL TRANSINT) vbfeat))) (memq 'TRANS2 vbfeat))
            :else (memq feature vbfeat))))

(§ defn- CANPARSE [num type feature]
    (let [REG nil]
        (and (CANTAKE num type feature)
            (or (nil? type)
                (and (apply parse
                    (concat type
                        (if (memq 'COMP type) (do (SETQ REG 'COMP) nil)
                            (list 'OBJ (SETQ REG (cond (or (memq 'LOC type) (memq 'PLACE type)) 'LOBJ (== num 1) 'OBJ1 :else 'OBJ2))))))
                    (setr *c* REG *h*)))
            (or (nil? feature) (f! feature)))))

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

(putprop! 'ALL 'SEMANTICS '((DET (COND ((cq 'OF) 'ALL) ((meet '(NUM DEF) *fe*) 'DEF) ('NDET)))))
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
                PROCEDUREß (!EVAL (let [PROPERTY (meet (getprop '!PROPERTY 'SYSTEM) (markers? SMCOMP))] (if PROPERTY (list (list (car PROPERTY) '!1 '!2)) (list '(!2 !1))))))
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
    (let [*end* *cut*]                                                ;; MAKE END OUT OF PREVIOUS CUT POINT
        (let [*cut* nil nbb *n* *both* nil]
            (and (flushme)
                (MOVE-PTW 'N 'NW '(= (word *ptw*) (car a)) 'NW)         ;; LOOK FOR THE MATCHING WORD E.G. AND, OR, NOR
                (cut *end*)
                (set! *both* *ptw*)                                     ;; SAVE POINTER TO THE WORD AFTER THE MATCHING WORD
                (set! *re* (COND
                    ((memq (car *rest*) '(PREP ADV)) (PARSE3 *rest* true))
                    ((memq (car *rest*) '(NG PREPG ADJG CLAUSE)) (PARSE2 *rest* true))))
                (< (count *n*) (count *both*))                          ;; FAIL UNLESS WE PARSED BEYOND MATCHING WORD
                (RETURN (set! *special* 'SKIP)))
            (set! *re* nil)
            (set! *n* nbb)
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

(putprop! 'I 'SEMANTICS '((PRON (smset (list (newcopy 'FRIEND-OSS))))))
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
        (and (cq 'NG)
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
(putprop! 'PLEASE 'B-SPECIAL '(flushme))

(putprop! 'POINTED 'SEMANTICS '((ADJ (OBJECT (MARKERSß (!PHYSOB !POINTED) PROCEDUREß ((!SHAPE *** !POINTED)))))))
(putprop! 'POINTED 'FEATURES '(ADJ))

(putprop! 'PUT 'PAST 'PUT)
(putprop! 'PUT 'SEMANTICS '((VB
        ((TRANSL (RELATION
              (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB)) (SMOBL (!PLACE)))
               MARKERSß (!EVENT)
               PROCEDUREß (!EVAL
                   (doall (map (lambda [%PLNRPHRASE]
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
                   (doall (map (lambda [%PLNRPHRASE]
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
(putprop! 'THANK 'SEMANTICS '(thank))
(putprop! 'THANK 'B-SPECIAL '(thank))

(defn- thank []
    (when (= (cadr *n*) 'YOU)
        (print "YOU'RE WELCOME")
        (flushme)
        (flushme)
        (when-not *nn* (ß_G))
        (set! *special* 'DONE)))

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

(putprop! 'WE 'SEMANTICS '((PRON (smset (list (newcopy 'WE-OSS))))))
(putprop! 'WE 'FEATURES '(PRON NPL SUBJ))

(putprop! 'WERE 'IRREGULAR '(BE (VPL PAST) (INF)))

(putprop! 'WHAT 'SEMANTICS '((DET true) (PRON (smset (list (newcopy 'UNKNOWN-OSS))))))
(putprop! 'WHAT 'FEATURES '(QDET DET NPL PRON QPRON NS))

(putprop! 'WHATEVER 'FEATURES '(PRON EVERPRON NS))

(putprop! 'WE 'REFER '(ßSHRDLU ßFRIEND))

(putprop! 'WHERE 'SEMANTICS '((QADJ (fq! 'WHERE))))
(putprop! 'WHERE 'FEATURES '(QADJ PLACE))

(putprop! 'WHEREVER 'FEATURES '(PRON EVERPRON NS))

(putprop! 'WHEN 'SEMANTICS '((BINDER (smbinder *start* *end*)) (QADJ (fq! 'WHEN))))
(putprop! 'WHEN 'FEATURES '(QADJ BINDER TIME))

(putprop! 'WHENEVER 'FEATURES '(BINDER))

(putprop! 'WHICH 'SEMANTICS '((PRONREL true) (DET true)))
(putprop! 'WHICH 'FEATURES '(QDET DET PRONREL NS NPL))

(putprop! 'WHICHEVER 'FEATURES '(DET RSQDET NS NPL))

(putprop! 'WHILE 'SEMANTICS '((BINDER (smbinder *start* *end*))))
(putprop! 'WHILE 'FEATURES '(BINDER TIME))

(putprop! 'WHITE 'SEMANTICS '((ADJ (!COLOR !WHITE))))
(putprop! 'WHITE 'FEATURES '(ADJ))

(putprop! 'WHO 'SEMANTICS '((PRONREL true) (PRON (smset (list (newcopy ANIMATE-OSS))))))
(putprop! 'WHO 'FEATURES '(PRONREL QPRON PRON NS))

(putprop! 'WHOEVER 'FEATURES '(PRON EVERPRON NS))

(putprop! 'WHOSE 'FEATURES '(DET QDET NPL NS))

(putprop! 'WHY 'SEMANTICS '((QADJ (fq! 'WHY))))
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

(putprop! 'YOU 'SEMANTICS '((PRON (smset (list (newcopy 'SHRDLU-OSS))))))
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
        (RESTRICTIONSß (((!THING)) ((!THING) (and (memq '!ROLE (markers? SMOB1)) (check-markers (cadr (assq '!ROLE (relations? SMOB1))) (markers? SMSUB) (systems? SMSUB)))))
            PROCEDUREß ((!SUBST !1 ?)))
        (RESTRICTIONSß (((!ANIMATE)) ((!PHYSOB)))
            PROCEDUREß ((!BELONG !2 !1)))))

(putprop! '!HEIGHT 'MEASFN #(caddr (size %)))

(§ defn- !IN []
    (COND ((cq 'LOBJ)
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
    (COND ((cq 'LOBJ)
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
    (COND ((cq 'LOBJ)
        (RELATION (RESTRICTIONSß (((!THING)) ((!PHYSOB))) MARKERSß (!PLACE) PROCEDUREß ((!ON !2)))))
        ((RELATION
            (RESTRICTIONSß (((!PHYSOB)) ((!PHYSOB))) PARAPHRASEß (ANYWHERE ON TOP OF) PROCEDUREß ((!ON !1 !2 *TIME)))
            (RESTRICTIONSß (((!PHYSOB)) ((!MANIP))) PARAPHRASEß (DIRECTLY ON THE SURFACE) PROCEDUREß ((!SUPPORT !2 !1 *TIME)))
            (RESTRICTIONSß (((!PLACE)) ((!PHYSOB))) PROCEDUREß ((!ON !1 !2)))))))

(putprop! '!PACK 'THMLIST '((3 '((THUSE TC-3)))))

(putprop! '!PART 'THMLIST '((3 '((THUSE TC-PART)))))                       ;; PERFORMED ON GRADUATION DAY, JUNE 2, 1972 BY JMH

(putprop! '!PERSON 'SYS '(!ANIMATE))

(putprop! '!PICKUP 'THMLIST '((2 '((THUSE TC-2))) (3 '((THUSE TCT-PICKUP))) (4 '((THUSE TCTE-PICKUP)))))

(putprop! '!PLACE 'SYS '(!SYSTEMS))

(putprop! '!PUT 'THMLIST '((3 '((THUSE TCT-3))) (4 '((THUSE TCT-PUT))) (5 '((THUSE TCTE-PUT)))))

(putprop! '!PUTIN 'THMLIST '((3 '((THUSE TC-3))) (4 '((THUSE TCT-4))) (5 '((THUSE TCT-5)))))

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

(putprop! '!SIZE 'MEASFN #(reduce + (size %)))
(putprop! '!SIZE 'SYS '(!PROPERTY))

(putprop! '!SHAPE 'PRIORITY 128)
(putprop! '!SHAPE 'SYS '(!PROPERTY))

(putprop! '!STACK 'SYS '(!CONSTRUCT))

(putprop! '!STACKUP 'THMLIST '((2 '((THUSE TC-2)))))

(§ defn- !STACKUP []
    (RELATION (RESTRICTIONSß (((!ANIMATE)) ((!MANIP))) MARKERSß (!EVENT) PROCEDUREß ((!STACKUP !2 *TIME)))))

(putprop! '!START 'THMLIST '((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(putprop! '!SUBST 'NOGOAL true)

(putprop! '!SUPPORT 'PRIORITY 256)
(putprop! '!SUPPORT 'THMLIST '((3 nil) (4 '((THUSE TCT-SUPPORT)))))

(putprop! '!SYSTEMS 'SYSTEM '(!THING !EVENT !NAME !RELATION !PLACE))

(putprop! '!TABLE 'SYS '(!PHYSOB))

(putprop! '!THICKNESS 'MEASFN #(cadr (size %)))

(putprop! '!THING 'SYS '(!SYSTEMS))
(putprop! '!THING 'SYSTEM '(!ANIMATE !NAME !PHYSOB !PROPERTY))

(putprop! '!UNGRASP 'THMLIST '((1 '((THUSE TC-UNGRASP)))))

(putprop! '!WANT 'THMLIST '((4 '((THUSE TC-WANT4))) (5 '((THUSE TC-WANT5)))))

(putprop! '!WHITE 'SYS '(!SPECTRUM))

(putprop! '!WIDTH 'MEASFN #(car (size %)))

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

(defn- smnewpropn [] (smset (list (newcopy 'NAME-OSS))))

(§ defn- SMCONJ []
    ;; FOR ALL CONJOINED THINGS -- IT CURRENTLY ONLY HANDLES THINGS
    ;; WHICH HAVE AN OSS OR RSS STRUCTURE AS THEIR SEMANTICS.  THIS
    ;; DOES NOT INCLUDE SINGLE WORDS OF MOST TYPES.  IT USES RECURSION.
    (let [%SM nil] (SMCONJ2 nil *h*) (smset %SM)))

(§ defn- SMCONJ2 [INTERPLIST restlist]
    ;; INTERPLIST IS THE LIST OF INTERPRETATIONS FOR THE CONJUNCTS
    ;; HANDLED SO FAR -- THIS FUNCTION WILL BE CALLED ONCE FOR EACH
    ;; POSSIBLE COMBINATION.  THE MARKERS FOR THE CONJOINED
    ;; STRUCTURE ARE THOSE OF THE FIRST CONJUNCT -- NEEDS MORE
    ;; SOPHISTICATION.  RESTLIST IS THE REST OF NODES YET TO BE HANDLED.
    (let [%X nil]
        (when-not restlist
            (RETURN (SETQ %SM
                (cons (BUILD
                        RSSNODE= (and (rss? INTERP) (gensym 'RSS))
                        OSSNODE= (and (oss? INTERP) (gensym 'OSS))
                        MARKERS= (markers? INTERP)
                        SYSTEMS= (systems? INTERP)
                        REL= (rel? INTERP)
                        AND= (and (or (cq 'BUT) (cq 'AND)) INTERPLIST)
                        OR= (and (or (cq 'OR) (cq 'NOR)) INTERPLIST))
                    %SM))))
        ;; WHEN THERE IS NO RESTLIST, WE HAVE LOOPED TO THE END OF THE LIST OF CONJUNCTS, AND THE RESULTING INTERPRETATION IS OK.
        ;; THE MAPPING IS DOWN THE LIST OF INTERPRETATIONS FOR A SINGLE CONJUNCT WHILE THE RECURSION GETS US DOWN THE LIST OF CONJUNCTS.
        ;; THUS WE GET EVERY POSSIBLE COMBINATION OF THE INTERPRETATIONS. -- ISN'T LISP SUPER-DUPER-WONDERFUL!
        ;; NOTICE THAT INTERP IS GETTING PICKED UP AS A FREE VARIABLE BY SMCONJ2, EVEN THOUGH IT IS BOUND ONLY INSIDE A MAPCAR INSIDE SMCONJ2.
        ;; THIS WORKS BECAUSE THE CLAUSE CONTAINING IT CAN NEVER GET CALLED EXCEPT BY RECURSION,
        (dorun (map (lambda [INTERP] (SMCONJ2 (cons INTERP INTERPLIST) (cdr restlist))) (semantics restlist)))))

(§ defn- SMVG [] ;; CALLED INSIDE ANY VG
    (let [TSS (getr (MOVE-PT 'C 'U '(CLAUSE)) 'TIME) tense nil]
        (when (cq 'NEG) (add-f-pt 'NEG *pt*))                           ;; NEG IS TRANSFERRED FROM THE VG TO THE CLAUSE IN WHICH IT IS EMBEDDED.
        (SETQ tense (getr *c* 'TENSE))
        (cond (memq tense '((PRESENT) (IMPER) (INFINITIVE))) true
            (= tense '(MODAL))
                (do (set! *global-message* "THAT DOESN'T MAKE ANY SENSE TO ME.")
                    (add-f-pt 'MODAL *pt*))                               ;; CLAUSES ARE ALSO MARKED AS MODAL.
            (and (= tense '(FUTURE)) (isq *pt* 'QUEST) (= (refer? (car (semantics (getr *pt* 'SUBJECT)))) '(ßSHRDLU))) ;; FUTURE QUESTIONS WITH "YOU"
                (do (SETQ tense '(PRESENT))                             ;; SUBJECT IS REALLY IMPERATIVE.
                    (remove-f-pt 'QUEST *pt*)
                    (add-f-pt 'IMPER *pt*))                               ;; THE CLAUSE IS NO LONGER QUESTION, BUT RATHER, IMPERATIVE.
            (setdif tense '(PAST PRESENT))
                (global-err "I DON'T KNOW HOW TO HANDLE TENSES INVOLVING FUTURE EVENTS OR MODALS OTHER THAN IN THE PRESENT"))
        (putprop! TSS 'TENSE= tense)
        true))

(§ defn- SMPRON [node]
    (eval (semantics node))
    (COND ((nil? *sm*)
        (set! *global-message* (str "I DON'T KNOW WHAT \"" (from (firstword *h*) (wordafter *h*)) "\" REFERS TO"))))
    *sm*)

(§ defn- SMVAUX []
    (if (isq *h* 'NEG) (fq! 'NEG) true)
    (putprop! (getr *c* 'TIME) 'TENSE= (or (meet (features *h*) '(PRESENT PAST MODAL)) (bug "SMVAUX -- FUNNY TENSE"))))

(defn- smplace [] (ert "SMPLACE NOT WRITTEN YET"))

(defn- smtoadj [] (ert "SMTOADJ NOT WRITTEN YET"))

(§ defn- SMPROP []
    ;; THIS IS THE SEMANTICS FOR PROPER NOUNS.  IT PRODUCES TWO
    ;; INTERPRETATIONS.  ONE IS THE OPAQUE REFERENCE TO THE NAME
    ;; ITSELF, AS IN "CALL IT SAM".  THE OTHER IS THE TRANSPARENT
    ;; REFERENT AS IN "PICK UP SAM".
    (smset (list
        (BUILD
            OSSNODE= (gensym 'OSS)
            VARIABLE= 'NAME
            DETERMINER= (1 DEF nil)
            PARSENODE= *c*
            MARKERS= (!NAME)
            REFER= (list (word (firstword *h*))))
        (BUILD
            OSSNODE= (gensym 'OSS)
            DETERMINER= (1 DEF nil)
            PARSENODE= *c*
            VARIABLE= (gensym 'X)
            RELATIONS= (list (list '!NAME OSSNODE= (word (firstword *h*)))))))
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
    (eval (semantics WORD-BEING-INTERPRETED)))
    ;; EVALUATE THE DEFINITION OF THE ADJECTIVE

(§ defn- SMADJG-PREPG []
    ;; HANDLES ADJECTIVE GROUPS AND PREPGS BOTH AS COMPLEMENTS AND QUALIFIERS.
    ;; DO NOTHING FOR "BY" PHRASES IN PASSIVE CLAUSES OR "OF" PHRASES LIKE IN "THREE OF THE BLOCKS".
    ;; SEMANTIC SUBJECT IS THE SUBJECT OF AN INTENSIVE OR THE NG TO WHICH THE GROUP IS A QUALIFIER,
    ;; OR THE CLAUSE OF WHICH IT IS AN ADJUNCT.
    (let [SMSUB nil]
        (and (or (cq 'AGENT) (cq 'OF)) (RETURN true))
        (setr *c* 'LOGICAL-SUBJECT
            (cond (cq 'COMP) (getr (MOVE-PT 'C 'U '(CLAUSE)) 'SUBJECT)
                (cq 'LOBJ) (or (getr (MOVE-PT 'C 'U '(CLAUSE)) 'OBJ1) (getr *pt* 'SUBJECT))
                (isq (MOVE-PT 'C 'U '(not (isq *pt* 'COMPONENT)) 'U) 'NG) *pt*
                (isq *pt* 'CLAUSE) *pt*
                :else (bug "SMADJG-PREPG FUNNY POSITION")))
        (SETQ SMSUB (semantics (getr *c* 'LOGICAL-SUBJECT)))
        (and (cq 'ADJG)
            (getr *c* 'OBJ1)
            (setr *c* 'ADJGHEAD (COMPARE-BUILD (getr *c* 'HEAD) (COND ((cq 'AS) '!ASMUCH) ((cq 'THAN) '!MORE) ((bug "SMADJG-PREPG FUNNY TYPE"))))))
        (COND
            ((getr *c* 'OBJ1) (SMCL1) *sm*)
            (:else (smset
                (let [*sm* nil]
                    (smset (doall (map (lambda [OSS]
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
                        ((or (cq 'COMPAR) (cq 'SUP)) (findmeasure (getr *c* 'HEAD)))
                        (:else (semantics (getr *c* 'HEAD)))))
                    *sm*))))))

(§ defn- SMIT [PRONOUN]
    ;; PRONOUN IS (IT THEY ONE) A NODE LIST OF POSSIBLE REFERENTS.
    ;; IS THIS A "DO IT!" COMMAND?  IF SO, RETURN THE LAST EVENT MENTIONED.
    ;; IF THIS PRONOUN HAS BEEN USED BEFORE IN THIS SENTENCE, THEN USE THE SAME CANDIDATES.
    ;; IF THIS PRONOUN WAS USED IN THE PREVIOUS SENTENCE,
    ;; LOOK FOR A STRUCTURE LIKE "A BLOCK WHICH IS TALLER THAN ANYTHING WHICH SUPPORTS IT"
    ;; OR "A BLOCK TALLER THAN ANYTHING WHICH SUPPORTS IT".
    (let [CANDIDATES nil AMBIGUITIES nil]
        (or discourse? (ert "SMIT: DISCOURSE SWITCH NOT ON"))
        (and *mvb*
            (isq *mvb* 'DO)
            (cq 'OBJ1)
            (RETURN (smset LASTEVENT)))
        (COND ((getprop PRONOUN 'BIND)
                (MAP (lambda [BINDNODE] (SMIT2 BINDNODE 0)) (getprop PRONOUN 'BIND))
                (RETURN *sm*))
            ((SMIT2 (getprop PRONOUN 'LASTBIND) 0)
                (GO DONE))
            ((or (MOVE-PT 'C 'U 'U '(NG) 'U 'U '(NG)) (MOVE-PT 'C 'U 'U '(NG) 'U '(COMP) 'PV '(SUBJ)))
                (SMIT2 *pt* 0)
                (MOVE-PT 'C 'U 'U '(NG))
                (COND ((isq *pt* 'DEF)
                    (add-f-pt 'INDEF *pt*)
                    (remove-f-pt 'DEF *pt*)
                    (dorun (map (lambda [INTERP] (putprop! INTERP 'DETERMINER= '((EXACTLY 1) INDEF nil))) (semantics *pt*)))))
                (RETURN *sm*))
            ((or (MOVE-PT 'C 'U '(BOUND) 'U) (MOVE-PT 'C 'U '(and (isq *pt* 'CLAUSE) (isq *pt* 'COMPONENT)) 'U 'DLC))
                (SMIT2 (getr *pt* 'OBJ2) 0)
                (SMIT2 (getr *pt* 'OBJ1) 0)
                (SMIT2 (getr *pt* 'SUBJECT) 0)
                (and (nil? *sm*)
                    (isq *pt* 'RSQ)
                    (SMIT2 (getr *pt* 'RELHEAD) 0))
                (and *sm* (RETURN *sm*))))
        (SMIT2 (getr *lastsent* 'SUBJECT) 192)
        (SMIT2 (parsenode? *lastrel*) 128)                ;; TRY REL (I.E. QUESTION FOCUS) OF THE LAST SENTENCE
        (MOVE-PT 'LASTSENT 'DLC)
    UP  (COND ((not (MOVE-PT 'PV '(NG))) (GO ON))
            (:else (SMIT2 *pt* 64)))                       ;; GO THROUGH TOP LEVEL NG'S OF LAST SENTENCE
        (and (MOVE-PT 'PV) (GO UP))
    ON  (or *sm* ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE ANSREF (NG'S IN LAST ANSWER)
            (MAP #(SMIT2 % 0) *ansname*))
        (or *sm* ;; IF WE HAVEN'T YET FOUND A REFERENT MAP DOWN THE BACKREF2 (NG'S IN LAST SENTENCE) LIST
            (MAP #(SMIT2 % 0) *backref2*))
    DONE (putprop! PRONOUN 'BIND CANDIDATES)
        (or (cdr *sm*) (remprop! (car *sm*) 'AMBIGUITIES=))
        *sm*))

(§ defn- SMIT2 [NODE PLAUSIBILITY]
    ;; MAKE SURE NODE IS REALLY THERE.
    ;; QUEST NODES (SUCH AS "WHAT") OR OTHER NODES WITHOUT HEAD NOUNS ARE NOT SUITABLE FOR REFERENTS.
    ;; MAKE SURE THAT NODE HASN'T ALREADY BEEN USED AS REFERENT.
    ;; MAKE SURE NODE AND PRONOUN AGREE IN NUMBER.
    (and NODE
        (getr NODE 'HEAD)
        (not (memq (car NODE) CANDIDATES))
        (COND ((= PRONOUN 'IT)
            (and (isq NODE 'NS) (not (isq NODE 'PRONG))))
            (:else (isq NODE 'NPL)))
        (SETQ CANDIDATES (cons (car NODE) CANDIDATES))
        (smset (concat
            (doall (map (lambda [REFERENT-OSS]
                (BUILD
                    OSSNODE= (gensym 'OSS)
                    MARKERS= (markers? REFERENT-OSS)
                    SYSTEMS= (systems? REFERENT-OSS)
                    PLAUSIBILITY= PLAUSIBILITY
                    AMBIGUITIES= (list (list OSSNODE= (from (firstword NODE) (wordafter NODE)) *c*))
                    REFER= (refer? REFERENT-OSS)
                    VARIABLE= (variable? REFERENT-OSS)
                    PARSENODE= *c* ;; INPUT PARAMETER
                    ;; USE THE REFERENT'S REFERENT, IF IT HAS ONE.
                    DETERMINER= (list (COND ((isq *c* 'NPL) 'NPL) ('NS)) 'INDEF nil)
                    ;; DONE SO THAT IF VARIBLE IS BOUND, PLANNER GENERATOR WILL USE IT.
                    ;; RELATION SAYS THAT THIS OSS "REFERS" TO THE OSS WHOSE VARIABLE NAME IS GIVEN.
                    RELATIONS= (list (list '!REFERS (variable? REFERENT-OSS)))))
                (semantics NODE)))
            *sm*))))

(§ defn- SMNGOF []
    ;; MAP DOWN THE LIST OF "OF" OBJECT INTERPRETATIONS.
    ;; USED TO PROCESS NOUN GROUPS LIKE "THREE OF THE BLOCKS", "BOTH OF THEM".
    ;; SINCE THE OBJECT OF THE "OF" MUST BE DEFINITE (SYNTACTICALLY) AND HAS ALREADY BEEN PROCESSED,
    ;; THE PLANNER CODE BUILT IS JUST A THAMONG EXPRESSION OF THE LIST
    ;; OF POSSIBLE REFERENTS OF THE "OF" OBJECT.
    (smset (MAPBLAND
        (lambda [OFOSS]
            (BUILD
                OSSNODE= (gensym 'OSS)
                VARIABLE= (variable? OFOSS)
                SYSTEMS= (systems? OFOSS)
                MARKERS= (markers? OFOSS)
                PARSENODE= *c*
                DETERMINER= (list (COND ((cq 'NUM) (semantics (MOVE-PT 'H 'PV '(NUM)))) ((isq *nb* 'BOTH) 2) ('NPL))
                                (COND ((MOVE-PT 'H 'PV '(QNTFR)) (eval (semantics *pt*))) ('INDEF))
                                (COND ((cq 'HOWMANY) 'HOWMANY) ((cq 'QDET) 'WHICH)))
                RELATIONS= (list (list 'THAMONG (list 'THV (variable? OFOSS)) (list 'quote (refer? OFOSS))))))
        (semantics (MOVE-PT 'H 'DLC)))))

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
                ((cq 'NUMD) ((lambda [NUM] (eval (semantics (MOVE-PT 'H 'PV '(NUMD))))) (semantics (MOVE-PT 'H 'PV '(NUM)))))
                ((cq 'NUM) (semantics (MOVE-PT 'H 'PV '(NUM))))
                ((cq 'NPL) (COND ((isq *nb* 'BOTH) 2) ((cq 'NS) 'SG-PL) ('NPL)))
                ('NS))
            (COND
                ((cq 'QNTFR) (eval (semantics (MOVE-PT 'H 'PV '(QNTFR)))))
                ((cq 'TPRON) (eval (semantics (MOVE-PT 'H 'PV '(TPRON)))))
                ((cq 'DEF) 'DEF)
                ((cq 'DET) 'INDEF)
                ('NDET))
            (COND
                ((cq 'HOWMANY) 'HOWMANY)
                ((cq 'QDET) 'WHICH))))
        ;; BUILD AN INITIAL OSS.  SETUP TO LOOP THROUGH ADJECTIVES.
        ;; IF IT'S A TPRON, IT WAS EVALED ABOVE, SO SKIP INCOMPLETES SUCH AS "PICK UP TWO".
        ;; EVAL THE HEAD NOUN IF AN ADJECTIVE ELIMINATES ANY POSSIBLE INTERPRETATION FOR THIS NG,
        ;; FAIL IF WE'VE LOOPED THRU ALL THE MODIFIERS, THEN RETURN THE LIST OF POSSIBLE INTERPRETATIONS.
        ;; IF IT'S A COMPARATIVE OR SUPERLATIVE ADJECTIVE,
        ;; IF IT'S AN ADJECTIVE OR CLASSIFIER, THEN EVAL THE DICTIONARY DEFINITION OF IT.
        (smset (list (BUILD
            OSSNODE= (gensym 'OSS)
            PARSENODE= *c*
            VARIABLE= (gensym 'X)
            MARKERS= (and (cq 'TPRON) '(!VAGUE !PHYSOB !THING))
            RELATIONS= (and (cq 'TPRON) (list (list '!PHYSOB OSSNODE=)))
            DETERMINER= DETERS)))
        (SETQ WORD-BEING-INTERPRETED *h*)
        (COND ((isq *h* 'TPRON) (GO LOOP))
            ((cq 'INCOM) (SMONE) (GO LOOP)))
        (smset (eval (semantics WORD-BEING-INTERPRETED)))
    LOOP (when (nil? *sm*) (RETURN nil))
        (COND ((nil? (SETQ WORD-BEING-INTERPRETED (cdr WORD-BEING-INTERPRETED)))
                (RETURN *sm*))
            ((or (isq WORD-BEING-INTERPRETED 'COMPAR) (isq WORD-BEING-INTERPRETED 'SUP))
                (eval (findmeasure WORD-BEING-INTERPRETED))
                (GO LOOP))
            ((or (isq WORD-BEING-INTERPRETED 'ADJ) (isq WORD-BEING-INTERPRETED 'CLASF))
                (SMADJ WORD-BEING-INTERPRETED)
                (GO LOOP))
            ((isq WORD-BEING-INTERPRETED 'POSS)
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
    (and (not (cq 'ANSNAME))
        (getr *c* 'HEAD)
        discourse?
        (set! *backref* (cons (car *c*) *backref*)))
    (smset (MAPBLAND #'SMNG3 *sm*)))

(§ defn- SMNG3 [oss]
    ;; TAKES AN OSS AS ARGUMENT AND TRIES TO FIND ITS REFERENCE IF THE NOUN GROUP IS DEFINITE.
    ;; EXPECT FOR SPECIAL "ONLY DEFINITE" DEFINITES SUCH AS "THE RIGHT" AND "THE THING".
    (let [FINDER nil MUNG nil INTER nil LIST nil CANDIDATES nil UNBOUND nil]
        (COND
            ((not (= (quantifier? oss) 'DEF)) (RETURN oss))        ;; IF ITS NOT DEFINITE OR IT
            ((refer? oss) (RETURN oss))                             ;; ALREADY HAS A REFERENT
            ((cq 'ANSNAME) (RETURN oss)))                            ;; MARKED,  IF ITS KLUDGY
        (SETQ FINDER
            (PLNR-FINDIFY 'ALL                                      ;; ANSWER NAME, JUST RETURN IT
                (variable? oss)                                     ;; JUST RETURN IT
                (list (variable? oss))
                (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))) ;; BUILDS UP THFIND EXPRESSION
        (putprop! oss 'PLNRCODE= FINDER)
        (set! *who* nil)
    UP  (COND
            ((not (SETQ CANDIDATES (thval2 *who* FINDER))) (GO TOOFEW))
            ((number? (num? oss)) (COND ((< (count CANDIDATES) (num? oss)) (GO TOOFEW)) ((> (count CANDIDATES) (num? oss)) (GO TOOMANY))))
            ((= (num? oss) 'NS) (COND ((nil? CANDIDATES) (GO TOOFEW)) ((cdr CANDIDATES) (GO TOOMANY))))
            ((memq (num? oss) '(NPL SG-PL)))
            ((ert "SMNG3: SCREWY NUMBER PROPERTY OF OSS")))
        (putprop! oss 'REFER= CANDIDATES)
    DONE (RETURN oss)

    TOOFEW ;; WE DIDN'T FIND ANY (OR ENOUGH) REFERENTS FOR THE NG
        (COND
            ((or (not discourse?) (nil? *who*))
                (set! *global-message* (str "I DON'T KNOW WHAT YOU MEAN BY \"" (from *nb* *n*) "\"."))
                (RETURN nil))
            ;; IF WE AREN'T REMEMBERING SENTENCES, FORGET IT IF WE JUST TRIED TO
            ;; FIND EVERYTHING (OR EVERYTHING THAT "HE" KNOWS ABOUT), THEN FAIL
            ((memq *who* '(HE nil))
                (set! *global-message* (str "I DON'T KNOW WHICH " (cdr (from *nb* *n*)) " YOU MEAN."))
                (RETURN nil)))
        (SETQ MUNG true)

    TOOMANY ;; ELSE SET UP TO EXPAND THE SENTENCES WE'RE LOOKING AT
        (and (memq *who* '(HE nil))
            (SETQ FINDER (plnr-mung FINDER CANDIDATES)))
        ;; RESTRICT THE POSSIBLE REFERENTS TO BE AMONG THE LIST ALREADY FOUND
        (set! *who* (COND
            ((= *who* nil) 'HE)
            ((= *who* 'HE) (list (dec *lastsentno*) (inc *lastsentno*)))
            ((or (not MUNG) (== (car *who*) 1)) (set! *who* 'HE) (GO TOOFEW))
            ((cons (dec (car *who*)) (cdr *who*)))))
        (SETQ MUNG nil)
        (GO UP)))

(§ defn- SMONE []
    (let [CONTRAST nil x *h*] ;; SET X TO DAUGHTERS OF CURRENT NODE
    GO  (COND ((SETQ CONTRAST (getprop (root (firstword x)) 'CONTRAST))
                (SETQ CONTRAST (list CONTRAST (root (firstword x)))))
            ((SETQ x (cdr x)) (GO GO)))
    UP  (or (and (MOVE-PT 'C 'U 'U '(NG)) (SMONE2 (list (car *pt*))))
            (SMONE2 (parsenode? *lastrel*))
            (SMONE2 *backref*)
            (SMONE2 *ansname*)
            (SMONE2 *backref2*)
            (COND (CONTRAST (SETQ CONTRAST nil) (GO UP)))
            (and (MOVE-PT 'LASTSENT 'DLC 'PV '(NG)) (SMONE2 (list (car *pt*))))
            (ert "SMONE: CAN'T FIND REFERENT FOR \"ONE\""))
        *sm*))

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
        (or (isq WORD-BEING-INTERPRETED 'NOUN)
            (bug "SMONE2: REFERENT OF \"ONE\" IS SCREWED UP"))
        (eval (semantics WORD-BEING-INTERPRETED))                          ;; EVAL THE NOUN DEFINITION
    GO  (and
            (SETQ WORD-BEING-INTERPRETED (cdr WORD-BEING-INTERPRETED))
            (isq WORD-BEING-INTERPRETED 'ADJ)                        ;; IF WE REACHED END OF ADJECTIVES, STOP
            (eval (semantics WORD-BEING-INTERPRETED))
            (GO GO))
        *sm*))

(§ defn- SMONE3 [ONENG]
    ;; SMONE3 TAKES AN NG WHICH IS A POSSIBLE REFERENT FOR "ONE".
    ;; IT FIRST CUTS THE NG TO BE ONLY (NOUN ADJ ... ADJ ETC) I.E.
    ;; IT STRIPS OF QUALIFYING PHRASES.  IF THERE IS NO CONTRAST,
    ;; THEN THIS MUNGED NG IS RETURNED AS THE REFERENT.  IF THERE
    ;; IS A CONTRAST, THEN IT CHECKS TO SEE IF THE NG SATISFIES
    ;; THAT CONTRAST.
    (let [NGWORDS nil x nil]
        (or (isq ONENG 'NG)
            (bug "SMONE3: ONE REFERENT IS NOT A NG"))
        (SETQ NGWORDS (daughters ONENG))
    LOOP (COND ((nil? NGWORDS) (RETURN nil))                        ;; FAIL, IF NG HAS NO NOUN HEAD
            ((isq NGWORDS 'NOUN))                                    ;; IF FIND NOUN HEAD OF NG, WIN
            (:else (SETQ NGWORDS (cdr NGWORDS)) (GO LOOP)))
        (or CONTRAST (RETURN NGWORDS))                              ;; IF THERE IS NO CONTRAST, REFERENT WINS BY DEFAULT
        (SETQ x (reverse NGWORDS))
    LOOK (COND ((and (= (car CONTRAST) (getprop (root (firstword x)) 'CONTRAST)) (not (= (cadr CONTRAST) (root (firstword x)))))
                (RETURN (reverse (cdr x))))
            ((SETQ x (cdr x)) (GO LOOK))
            (:else (RETURN nil)))))                                  ;; FAIL, IF NO WORD SUPPLIES CONTRAST

(§ defn- SMPOSS []
    (let [x (SMPOSS2 *c* (MOVE-PT 'H 'PV '(POSS)))]
        (and x (SMRELATE x))))

(§ defn- SMPOSS2 [headnode modnode]
    (let [*sm* nil SMSUB (semantics modnode) SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST (RETQ AMOB1 (semantics headnode))]
        (smset '(!HAVE))
        (and *sm* (let [x (gensym 'NODE)] (and x (putprop! x 'SEMANTICS *sm*) (list x))))))

;; SMPOSS WORKS BY ACTING LIKE SMCL1 AND SETTING UP AN RSS (HAVE X Y).  NODE IS THE NODE OF THE POSSESSIVE
;; WHICH HAS ALREADY BEEN SEMANTICALLY PROCESSED.  ITS SM CONTAINS THE OSS'S FOR WHOSE DOING THE POSSESSING.
;; THE SM CURRENTLY ACTIVE IS THE THING BEING POSSESSED.

(§ defn- SMRELATE [node]
    ;; RELATES A DESCRIPTIVE RSS TO ANOTHER RSS OR OSS ADDING IT
    ;; TO THE LIST OF RELATIONS.  IT TAKES THE LIST OF SS IN SM,
    ;; AND REPLACES SM WITH THE NEW LIST OF MODIFIED SS'S.  THE
    ;; MODIFYING RSS'S HAVE TO HAVE ONE OF THE SM SS'S AS A REL
    ;; (WHICH SHOULD ALWAYS BE TRUE IF THEY WERE SET UP PROPERLY).
    ((lambda [x] (and x (smset x)))
        (doall (map (lambda [RSS]
            (let [REL (rel? RSS)]
                (or (memq REL *sm*)
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
            (semantics node)))))

(§ defn- SMCL1 []
    (let [SMSUB nil SMOB1 nil SMOB2 nil SMOBL nil SMCOMP nil RELLIST nil]
        ;; SET UP GLOBAL VARIABLES WHICH CONSIST OF POINTERS TO THE SEMANTIC DEFINITIONS
        ;; OF THE VARIOUS NOUN-GROUPS (ALSO RSNG'S) REQUIRED BY THE TRANSITIVITY OF THE VERB.
        (SETQ SMSUB (COND
            ((SETQ SMSUB (getr *c* 'LOGICAL-SUBJECT)) (semantics SMSUB))
            ((cq 'IMPER) '(SHRDLU-OSS))
            ((not (cq 'PASV)) (semantics (or (getr *c* 'SUBJECT) (bug "SMCL1 -- NO SUBJECT"))))
            ((cq 'AGENT) (bug "SMCL1 -- AGENT MISSING"))
            ('(UNKNOWN-OSS-BY))))
        (SETQ SMOB1 (semantics (COND ((cq 'PASV) (getr *c* 'SUBJECT)) ((getr *c* 'OBJ1)))))
        (SETQ SMOB2 (semantics (getr *c* 'OBJ2)))
        (SETQ SMOBL (semantics (getr *c* 'LOBJ)))
        (SETQ SMCOMP (semantics (getr *c* 'COMP)))
        ;; NATURALLY SEVERAL OF THESE GLOBAL VARIABLES (BOUND IN THIS PROG AND ACCESSED IN DEEPER ONES)
        ;; ARE NIL AT THIS POINT IN THE PROCEDURE.  THE FOLLOWING CHECKS ARE PRIMARILY FOR DEBUGGING PURPOSES
        ;; (HENCE THE "ERT") TO INSURE THAT THE NON-NIL REGISTERS AND THE TRANSITIVITY OF THE VERB ARE
        ;; BEING MATCHED IN EVERY CASE.
        (or SMSUB (and (meet '(THERE ITRNS) *fe*) (GO CHECK)))
        (or SMOB1 (and (or (cq 'TRANS) (not (cq 'CLAUSE))) (GO CHECK)))
        (or (and SMOB1 SMOB2) (and (cq 'TRANS2) (GO CHECK)))
        (or (and SMOB1 SMOBL) (and (cq 'TRANSL) (GO CHECK)))
        (or SMCOMP (and (cq 'INT) (GO CHECK)))
        (GO REL)
    CHECK (ert "BUG: SMCL1 TRANSITIVITY")
    REL (SETQ RELLIST
            (semantics (COND
                ((cq 'RSQ) (getr *c* 'RELHEAD))
                ((or (cq 'PREPG) (cq 'ADJG)) (getr *c* 'LOGICAL-SUBJECT))
                ((cq 'QUEST) (getr *c* 'RELHEAD)))))
        (and (not RELLIST)
            (or (cq 'POLAR) (cq 'DECLAR))
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
        (SETQ TIME (getr (MOVE-PT 'C 'U '(CLAUSE)) 'TIME))
        ;; THIS REFERS TO THE SEMANTIC SENSE OF THE VERB WHICH WILL PROBABLY VARY WITH ITS TRANSITIVITY.
        ;; THE VALUE THAT IS FINALLY DETERMINED REPRESENTS ALL POSSIBLE SENSES OF THE MEANING OF THE WORD
        ;; THAT ARE APPROPRIATE TO THE TRANSITIVITY FIGURED OUT BY THE SYNTACTIC PROGRAMS
        (SETQ SENSE-OF-VERB
            (COND
                ((cq 'PREPG) (semantics (SETQ WORD-BEING-INTERPRETED (getr *c* 'HEAD))))
                ((cq 'ADJG) (semantics (SETQ WORD-BEING-INTERPRETED (getr *c* 'ADJGHEAD))))
                ((cadr (assq (car (meet *fe* '(ITRNS TRANS INT TRANSL TRANS2 THERE ITRNSL))) (semantics (SETQ WORD-BEING-INTERPRETED (getr *c* 'MVB))))))))
        (smset (eval SENSE-OF-VERB))
        ;; THIS DETERMINES THE APPROPRIATE SEMANTIC INTERPRETATION(S) FOR THE CLAUSE BY CHECKING
        ;; THE RESTRICTIONS OF EACH DEFINITION AGAINST THE MARKERS OF THE VARIOUS CANDIDATES FOR SMSUB,
        ;; SMOB1, ETC.  THE VALUE OF THE EVALUATION IS A LIST OF RELATION-SEMANTIC-STRUCTURES, ONE FOR
        ;; EACH PLAUSIBLE INTERPRETATION
        (MAP #'SMCL-MODIFIERS *h*) ;; SMCL-MODIFIERS WILL EXAMINE
        (RETURN *sm*)))
        ;; ALL OF THE CONSTITUENTS OF THE CLAUSE THAT WERE NOT INVOLVED IN THE BUILDRSS AND WILL EVALUATE
        ;; THE MEANINGS OF EACH IN TURN FOR THEIR EFFECT ON THE ESTABLISHED SM, THE PARSING TREE, OR
        ;; ANYTHING ELSE THAT WOULD BE APPROPRIATE THE VALUE OF SMCL1 IS NON-NIL ONLY IF SOME
        ;; REASONABLE MEANING HAS BEEN FOUND FOR THE CLAUSE

(§ defn- SMCL2 []
    ;; THERE USED TO BE A CALL TO SMPREPREL AT THIS POINT, BUT IT
    ;; HAS GONE AWAY PENDING FURTHER THOUGHT.
    (MAP #'SMCL-MODIFIERS *h*))
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
    ((meet *fe* '(THERE LOBJ COMP PRT SUBJ OBJ1 OBJ2)))
    ((isq WORD-BEING-INTERPRETED 'NG)
        (and (COND ((isq WORD-BEING-INTERPRETED 'TIM))
            ((and (cq 'REL-NOT-FOUND)
                (isq WORD-BEING-INTERPRETED 'QUEST)
                (isq (daughters WORD-BEING-INTERPRETED) 'TIM1))
            (rq 'REL-NOT-FOUND)
            (fq! 'TIMEQ)))
        (smtime)))
    ;; IN WHICH CASE IT WAS ALREADY PROCESSED MIGHT GO AWAY IN A FEW DAYS
    ;; BUG CHATCHER MIGHT WANT TO CHANGE THAT THE REST ARE HOOKS FOR WHEN
    ;; WE FIGURE OUT WHAT TO DO WITH THEM
    ((isq WORD-BEING-INTERPRETED 'PREPG)
        (or (isq WORD-BEING-INTERPRETED 'AGENT)
            (ert "SMCL-MOD BAD PREPG")))
    ((isq WORD-BEING-INTERPRETED 'QADJ)
        (or (meet *fe* '(LOBJ COMPQ))
            (eval (semantics WORD-BEING-INTERPRETED))))
    ((isq WORD-BEING-INTERPRETED 'BOUND))
    ((isq WORD-BEING-INTERPRETED 'BINDER))
    ((isq WORD-BEING-INTERPRETED 'QUEST))
    ((isq WORD-BEING-INTERPRETED 'CLAUSE))
    ((ert "SMCL-MODIFIERS ATTEMPTED TO PROCESS AN UNEXPECTED TYPE OF CONSTITUENT"))))

(§ defn- SMBIND []
    (let [TSS nil EVENT nil *start* nil *end* nil]
        ;; DOES THE SM HAVE MORE THAN ONE VALUE???
        (when (cdr (semantics *h*))
            (ert "I DON'T KNOW WHAT TO DO WITH AMBIGUOUS BOUND CLAUSES"))
        ;; DISPATCH TABLE TO MATCH THE APPROPRIATE ACTION WITH EACH BINDER.
        ;; MOVE TO THE FIRST WORD OF THE CLAUSE (TO THE BINDER) AND CHECK FOR THE FEATURE TIME
        ;; (MAYBE ALSO CHECK FOR THE SM BEING MARKED AS AN EVENT???)
        (when (isq (MOVE-PT 'H 'DF) 'TIME)
            (SETQ TSS (getr *c* 'TIME))
            (SETQ EVENT (FINDEVENTS (car (semantics *h*))))
            (when-not EVENT
                (global-err "NO SUCH THING EVER HAPPENED"))
            (SETQ EVENT (car EVENT))
            (set! *start* (getprop EVENT 'START))
            (set! *end* (getprop EVENT 'END))
            (eval (semantics *pt*))
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
    (let [x (getr node 'TIME)]
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
        (and MARKERS= (not SYSTEMS=) (SETQ %X (check-markers MARKERS= nil nil)) (SETQ MARKERS= (car %X)) (SETQ SYSTEMS= (cadr %X)))
        (SETQ NODE= (or OSSNODE= RSSNODE= TSSNODE= ANSNODE= (ert "BUILD: NO NODE=")))
        (dorun (map (lambda [%PROP] (and (SETQ %X (eval %PROP)) (putprop! NODE= %PROP %X))) %PROPS))
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
        (lambda ARGLIST
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
                    (doall (map (lambda [%RESTRICTNAM %MARKL %NUM]
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
                        (dorun (map (lambda [%MARKL]
                            (let [OSS (eval (car %MARKL)) x (checkrel OSS) CHECK nil]
                                (and x (SETQ RELß (car x)))
                                (COND
                                    ((not (and (or (nil? (cddr %MARKL)) (eval (caddr %MARKL)))
                                            (SETQ CHECK (check-markers (cadr %MARKL) (markers? OSS) (systems? OSS)))))
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
                            VARIABLE= (let [x (gensym 'EVX)] (putprop! x 'RSSVAR x))
                            PARSENODE= *c*
                            RELATIONS= (reverse (doall (map (lambda [%PLNRPHRASE] (PLNR-NUMSUB '<<<RELATION-ERROR>>> %PLNRPHRASE)) (evalcheck PROCEDUREß))))
                            REL= RELß
                            NEGATIVE= (and (cq 'NEG) true)
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
            (cq 'MODAL)
            (cq 'DECLAR)
            (MAP (lambda [backnode]
                    (let [a (semantics backnode)]
                        (when (cdr a) ;; TRUE IF NODE HAD MULTIPLE INTERPRETATIONS
                            (terpri)
                            (pr a)
                            (setr backnode 'SEMANTICS (ert "DOBACKREF: RETURN AN OSS FOR BACKNODE")))
                        (or (refer? (car a)) ;; IF NODE HAS REFERENT, FINE
                            (putprop! (car a) 'REFER=
                                (or (getprop (variable? (car a)) 'BIND) (ert "DOBACKREF: RETURN REFERENT FOR BACKNODE"))))))
                *backref2*))
        ;; A FEW MISSING PIECES GO HERE
        nil))

(defn- evalcheck [l]
    ;; EVALCHECK CHECKS FOR THE PRESENCE OF (!EVAL (MUMBLE ...) ...) IN THE INPUT S-EXPRESSION L.
    ;; IF IT FINDS ONE THEN THE EXPRESSION MUMBLE IS EVALUATED AND REPLACES (!EVAL ...), OTHERWISE
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
    ;;
    ;; THIS SHOULD BECOME A MACRO SO THAT ITERATE TAILORS A MAPPING FUNCTION
    ;; FOR EACH APPLICATION WHICH IS THEN COMPILED.
    (let [%X nil %XL nil] (eval (ITERATEX (car %L) (cdr %L)))))

(§ defn- ITERATEX [fun l]
    ;; EXPANDS CALL ON ITERATE INTO A TAILORED SET OF MAPBLAND
    ;; CALLS WHICH DO THE APPROPRIATE COMPUTATION WHEN EVALUATED.
    ;; INPUTS:
    ;;      FUN    FUNCTION OF N ARGUMENTS
    ;;      L      LIST OF N LISTS WHICH FUN IS TO BE APPLIED TO
    ;; VALUE:
    ;;  TAILORED FUNCTION
    (if (nil? l) (cons (eval fun) %XL)
        (list 'MAPBLAND
            (list 'FUNCTION (list 'LAMBDA
                ;; %X IS USED TO STORE THE VARIABLE NAME WHICH IT GETS FROM (GENSYM)
                ;; %XL IS USED TO SAVE A LIST OF ALL OF THE VARIABLE NAMES SO FAR SO
                ;; THAT THEY CAN BE GIVEN TO THE FUNCTION AT THE END (CONS %X NIL).
                ;; CREATES A SHINY NEW CELL WHICH IS BOLTED ONTO THE BACK END OF %XL
                ;; BY NCONC.  THIS PAIR IS NECESSARY BECAUSE SETQ RETURNS AS ITS VALUE
                ;; THE RESULT OF THE LAST PAIR THAT IT PROCESSES.  A RUSE. PUTS (NIL)
                ;; IN PLACE OF NIL AS A LIST TO PREVENT MAPBLAND FROM QUITTNG.
                (list (SETQ %X (gensym) %XL (concat %XL (cons %X nil)) %X %X))
                (ITERATEX fun (cdr l))))
            (or (car l) '(nil)))))

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
        (let [x (fun (car l)) ANS (cond (nil? x) ANS (term? x) (concat ANS (cons x nil)) :else (concat ANS x)) l (cdr l)]
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

(defn- mumble [x]
    ;; MUMBLE IS THE PLANNER FILTER FOR LOOKING AT ASSERTIONS TO SEE WHEN THEY WERE MENTIONED
    ;; IN THE DIALOG.  IT USES THE FREE VARIABLE "WHO" TO DECIDE WHAT TO LOOK FOR.
    ;; WHO IS EITHER NIL (USE ANYTHING), "HE" (USE ANYTHING WHICH HAS BEEN MENTIONED), OR A LIST
    ;; OF TWO SENTENCE NUMBERS, MIN AND MAX (USE ANY WHO PROPERTY WHICH IS ON OR BETWEEN THEM).
    ;; THE WHO PROPERTY OF ANY ASSERTION IS A SENTENCE NUMBER WHEN IT WAS MOST RECENTLY MENTIONED.
    (or (nil? *who*)
        (let [x (getprop x 'WHO)]
            (cond (= *who* 'HE) x x (<= (car *who*) x (cadr *who*))))))

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
        (or *sm* (SETQ %VARNAM (gensym 'X)))
        (RETURN (smset
            ;; %OSS IS A SINGLE OSS NODE NAME PICKED OFF OF SM.
            ;; %DEF IS A SINGLE DEFINITION SENSE PICKED OFF OF %DEFL.
            (ITERATE (lambda [%OSS %DEF]
                (let [%OSSNODE nil %CHKRESULT nil MARKERSß nil SYSTEMSß nil PLAUSIBILITYß nil DETß nil RELATIONSß nil PARAPHRASEß nil PROCEDUREß nil]
                    ;; DECODE KEYWORDS
                    (SETQQCHECK nil %DEF '(MARKERSß PLAUSIBILITYß PARAPHRASEß PROCEDUREß) 'OBJECT)
                    ;; CHECK FOR MARKER AGREENT.  IF OK, THEN BUILD OSS, ELSE CHUCK THIS COMBINATION.
                    (and
                        (SETQ %CHKRESULT (check-markers MARKERSß (markers? %OSS) (systems? %OSS)))
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
                            RELATIONS= (concat (reverse (doall (map (lambda [%PLNRPHRASE] (PLNR-NUMSUB %OSS %PLNRPHRASE)) (evalcheck PROCEDUREß)))) (relations? %OSS))
                            REL= (rel? %OSS)
                            ;; THE OSS NAME PROVIDES A UNIQUE LABEL FOR WHERE THE AMBIGUITY OCCURRED FOR LATER COMPARISON.
                            AMBIGUITIES= (concat (ambiguities? %OSS) (and PARAPHRASEß (list (list %OSS PARAPHRASEß WORD-BEING-INTERPRETED))))
                            PLAUSIBILITY= (+ (or (eval PLAUSIBILITYß) 0) (plausibility? %OSS)))))
                    nil))
            *sm* %DEFL)))))

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
                (concat (list 'THCONSE (union varlist (cadr body)) exp) (cddr body))
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
        ((concat (list 'THGOAL plnrphrase '(THDBF mumble)) (PLNR-RECOMMENDIFY plnrphrase)))))

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

(§ defn- PLNR-NEWBODY [x] (SETQ NEWBODY (cons x NEWBODY)))

(§ defn- PLNR-PROGIFY [varlist body]
    ;; SETS UP A THPROG OR THE SIMPLEST EQUIVALENT EXPRESSION FOR
    ;; THE PARTICULAR CASE.  BODY IS A LIST OF EXPRESSIONS
    (let [NEWBODY nil]
        (or body (RETURN nil))
        (dorun (map (lambda [x] (COND
                ((= (car x) 'THPROG)
                    (COND ((meet varlist (cadr x)) (PLNR-NEWBODY x))
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
    (doall (map (lambda [e] ;; E IS AN ATOM OF THE PHRASE
            (cond
                (memq e '(!1 !2 !3)) (PLNR-NUMREL (eval e))
                (= e '***) %ME
                (= e '*TIME) TIME ;; GETS THE CURRENT TIME
                :else e))
        (evalcheck %PLNRPHRASE))))

(§ defn- PLNR-RECOMMENDIFY [%PLNRPHRASE]
    ;; LOOKS UP IN THE DICTIONARY A RECOMMENDED THEOREM TO USE IN PROCESSING A PLNRPHRASE BY THGOAL.
    ;; IF IT FINDS ONE, IT TACKS IT ON AS A RECOMMENDATION.
    (let [e (getprop (car %PLNRPHRASE) 'THMLIST)]
        ;; LOOK A RELATION UP IN THE DICTIONARY.  THE ENTRIES ARE SET UP AS A PROPERTY LIST.
        ;; THERE ARE DIFFERENT RECOMMENDATIONS FOR THE SAME RELATION DEPENDING ON THE NUMBER
        ;; OF ARGUMENTS THIS INSTANCE OF IT HAS.  (LENGTH ...) COMPUTES THE NUMBER OF ARGUMENTS
        ;; + 1 AND THE (ASSQ ...) RETRIEVES THE APPROPRIATE RECOMMENDATION USING THIS NUMBER.
        ;; IF THERE IS NO SUCH NUMBER, NUMBERP FAILS AND SOME ARBITARY FUNCTION WHICH
        ;; IS STORED OUT THERE IS EVALUATED TO GIVE THE RECOMMENDATION.
        (when e
            (eval (if (number? (caar e)) (cadr (or (assq (count %PLNRPHRASE) e) '(nil nil))) e)))))

(§ defn- PLNR-REMTIME [exp]
    ;; REMOVES ALL PRESENT TENSE TIME STRUCTURES
    (let [y '(T)]
        ;; Y IS BOUND TO A UNIQUE POINTER SO IT CAN'T POSSIBLY SCREW ANYTHING
        ;; IN THE EXPRESSION WHEN IT DOES THE DELETION.  DELQ USES EQ.
        (DELQ y
            (doall (map (lambda [x]
                (cond (not (term? x)) x
                    (tss? x) (if (and (tense? x) (not (memq (tense? x) '((PRESENT PRESENT) (MODAL) (PRESENT))))) x y)
                    :else x))
            exp)))))

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
    (let [r (root (firstword node)) x (assq 'MEASURE (getprop r 'SEMANTICS))]
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
            ((and (cdr exps) (= (car x) '!SUBST)) (mapc2 (lambda [x y] (SETQ exps (SUBST x y exps))) (cdr x)))
            (x (SETQ BODY (cons x BODY))))
        (SETQ exps (cdr exps))
        (GO =>)))

(§ defn- RELFIND [node]
    ;; LOOKS FOR THE REL OF A POLAR
    (let [REL nil]
        (ERRSET
            ;; IT GOES FROM THE BEGINNING OF THE SENTENCE LOOKING FOR AN INDEFINITE NG,
            ;; EITHER AT THE TOP LEVEL OR AS A FIRST LEVEL PREPOBJ, BUT NOT A COMPLEMENT.
            (MAP (lambda [x] (COND
                ((isq x 'NG) (and (not (isq x 'COMP)) (not (isq x 'DEF)) (SETQ REL x) (ERR nil)))
                ((isq x 'LOBJ) (and (isq (daughters x) 'INDEF) (SETQ REL x) (ERR nil)))
                ((isq x 'PREPG) (and (isq (daughters x) 'INDEF) (SETQ REL (daughters x)) (ERR nil)))))
            (reverse (daughters node))))
        (or REL (and (cq 'PASV) (not (cq 'AGENT)) (SETQ REL '(FAKE-AGENT))))
        (and REL (semantics REL))))

(§ defn- ORDMAKE [ordinal var body]
    ;; MAKES THE LOGICAL FORM FOR SUPERLATIVES.
    ;; ORDINAL GIVES THE THING BEING COMPARED IN MEASURE FORM.
    (let [x (gensym 'X)]
        (PLNR-PROGIFY nil
            (concat body (list (PLNR-NOTIFY true
                (PLNR-PROGIFY (list x) (concat (SUBST x var body) (list (PLNR-GOALIFY (COMPARE-PROC var x ordinal)))))))))))

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
                ((and? exp) (PLNR-PROGIFY nil (doall (map #(EXPAND % nil) (and? exp)))))
                ((or? exp) (PLNR-ORIFY (doall (map #(EXPAND % nil) (or? exp)))))
                ((PLNR-NOTIFY (negative? exp) (PLNR-DESCRIBE (relations? exp) (variable? exp) (cons (variable? exp) FREEVARS))))))
        ((term? exp) (bug "EXPAND - ATOMIC MODIFIER"))
        ((= (car exp) '*ORDINAL*)
            (COND (ORDINAL (global-err "I CAN'T HANDLE TWO ORDINALS OR SUPERLATIVES AT ONCE"))
                ((SETQ ORDINAL (cadr exp)) '*ORDINAL*)))
        ((= (car exp) '!SUBST)
            (ert "EXPAND - IS !SUBST BEING HANDLED BY SOMEONE ELSE?")
            exp)
        ((let [BODY nil *quantifier* nil CHOICE nil VAR nil MULTIPLE (eval (getprop (car exp) 'MULTIPLE))]
            (SETQ exp
                (doall (map (lambda [x] (COND
                    ((or (not (term? x)) (not (or (rss? x) (oss? x))))
                        x)
                    ((refer? x)
                        (COND ((cdr (refer? x))
                            (COND (MULTIPLE (erqset 'AND) (SETQ CHOICE (refer? x)) '*AND*)
                                ((refer? x))))
                        ((car (refer? x)))))
                    ((memq (variable? x) FREEVARS)
                        (and (rssvar? (variable? x)) (putprop! (variable? x) 'USED true))
                        (plnr-var (variable? x)))
                    ((SETQ CHOICE (and? x))
                        (erqset 'AND)
                        (and MULTIPLE (refer? x) (SETQ CHOICE (refer? x)))
                        '*AND*)
                    ((SETQ CHOICE (or? x))
                        (erqset 'OR)
                        '*OR*)
                    ((COND
                        ((rss? x) (erqset 'EVENT) (putprop! (variable? x) 'USED true))
                        ((memq (quantifier? x)
                            '(ALL NO)) (erqset (quantifier? x)) true)
                        ((memq (quantifier? x) '(NDET INDEF))
                            (COND ((memq (num? x) '(NS SG-PL)) (erqset 'INDEF)) ((SETQ CHOICE (plnr-findspec (num? x))) (erqset 'FIND))) true))
                        (SETQ BODY (PLNR-DESCRIBE (relations? x) (variable? x) (cons (variable? x) FREEVARS)))
                        (plnr-var (SETQ VAR (variable? x))))
                    ((bug "EXPAND - STRANGE QUANTIFIER"))))
                    (if event (cons (car exp) (cons event (cdr exp))) exp))))
            ;; THE EVENT NAME IS STUCK INTO THE SECOND POSITION IF THERE IS ONE.
            (RETURN (COND
                ((nil? *quantifier*) (PLNR-GOALIFY exp))
                ((= *quantifier* 'AND)
                    (PLNR-PROGIFY nil (doall (map #(EXPAND (SUBST % '*AND* exp) nil) CHOICE))))
                ((= *quantifier* 'OR)
                    (PLNR-ORIFY (doall (map #(EXPAND (SUBST % '*OR* exp) nil) CHOICE))))
                ((= *quantifier* 'FIND)
                    (PLNR-FINDIFY CHOICE VAR (list VAR) (PLNR-PROGIFY nil (cons BODY (list (PLNR-GOALIFY exp))))))
                (:else
                    (PLNR-NOTIFY (memq *quantifier* '(ALL NO))
                        (PLNR-PROGIFY (and VAR (list VAR)) (cons BODY (list (PLNR-NOTIFY (= *quantifier* 'ALL) (PLNR-GOALIFY exp)))))))))))))

(defn- erqset [x]
    ;; USED BY EXPAND TO MAKE SURE IT ISN'T GETTING CONFUSED BY TOO
    ;; MANY CONNECTIVES AND QUANTIFIERS IN THE SAME EXPRESSION.
    (if *quantifier* (global-err "I CAN'T HANDLE COMBINATIONS OF QUANTIFIERS AND CONNECTIVES WHICH ARE SO COMPLICATED") (set! *quantifier* x)))

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

(defn- thval2 [who' a]
    (binding [*who* who']
        (when *plannersee*
            (DISP a))
        (THVAL a '((EV COMMAND)))))

(defn- who [x]
    (or (nil? *who*) (term? x)
        (let-when [x (getprop x 'WHO)] x
            (or (= *who* 'HE) (< (car *who*) x *lastsentno*)))))

(defn- check-markers [a MARKERS SYSTEMS]
    ;; TAKES A LIST OF NEW MARKERS AND CHECKS FOR COMPATIBILITY WITH THE EXISTING
    ;; MARKERS AND SYSTEMS (AS GIVEN BY ARGS MARKERS AND SYSTEMS).  IF COMPATIBLE,
    ;; RETURNS A TWO-LIST OF THE NEW MARKERS AND SYSTEMS, ELSE RETURNS NIL.
    (loop-when a a => (list MARKERS SYSTEMS)
        (recur-if (CHECKAMARKER (car a)) (cdr a))))

(§ defn- CHECKAMARKER [marker]
    ;; CHECKS A SINGLE MARKER FOR COMPATIBILITY
    ;; USES FREE VARIABLES:
    ;;    SYSTEMS - THE SYSTEM LIST SO FAR
    ;;    MARKERS - THE MARKER LIST SO FAR
    ;; IF SUCCESSFULL, THE MARKER AND ITS SYSTEM(S) ARE APPENDED TO THESE FREE VARIBLES
    (when (memq marker MARKERS) (RETURN true))               ;; IF MARKER ALREADY THERE, FINE
    (SETQ MARKERS (cons marker MARKERS))                    ;; ADD NEW MARKER TO LIST
    (loop-when [a (getprop marker 'SYS)] a => true          ;; GET THE SYSTEMS OF THE NEW MARKER
        (cond (memq (car a) SYSTEMS) nil                      ;; FAIL IF SYSTEM THERE BY ANOTHER PATH
            (CHECKAMARKER (car a)) (do (SETQ SYSTEMS (cons (car a) SYSTEMS)) (recur (cdr a))))))

(§ defn- FINDEVENTS [rss]
    ;; FINDS ALL THE EVENTS FITTING THE RSS DESCRIPTION
    (putprop! (variable? rss) 'USED true)
    (thval2 nil
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

(defn- parsenode? [x]
    ;; THE PARSE NODE ASSOCIATED WITH THE SEMANTIC STRUCTURE.
    (getprop x 'PARSENODE=))

(defn- plausibility? [%XSS]
    ;; ACCESS FUNCTION FOR GETTING PLAUSIBILITY OF AN OSS OR RSS.
    (or (getprop %XSS 'PLAUSIBILITY=) 0))

(defn- plnrcode? [x]
    ;; THE PLANNERCODE GENERATED WHEN AN OBJECT IS ACTUALLY LOOKED FOR IN THE DATABASE.
    ;; IT IS NOT USED AGAIN, BUT IS LEFT SITTING AROUND FOR PEOPLE TO LOOK AT.
    (getprop x 'PLNRCODE=))

(defn- qtype? [x]
    ;; QUESTION TYPE FOR QUESTION OSS.
    (caddr (getprop x 'DETERMINER=)))

(defn- quantifier? [oss]
    ;; GETS THE DETERMINER FIELD OF AN OSS.
    (cadr (getprop oss 'DETERMINER=)))

(defn- refer? [%XSS]
    ;; ACCESS FUNCTION FOR REFER OF OSS OR RSS.
    (getprop %XSS 'REFER=))

(defn- rel? [x]
    ;; THE OBJECT TO WHICH THIS DESCRIPTION IS TO BE RELATED.
    (getprop x 'REL=))

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

(defn- tss? [x]
    ;; ASSOCIATED WITH EVERY TIME SEMANTIC STRUCTURE.
    (getprop x 'TSSNODE=))

(defn- variable? [x]
    ;; ACCESS FUNCTION FOR GETTING THE VARIABLE NAME ASSOCIATED WITH AN OSS OR RSS.
    (getprop x 'VARIABLE=))

(defn- smset [x] (setr *c* 'SEMANTICS x) (set! *sm* x))

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
        (SETQ AMBIG (cdr (semantics node)))
        (SETQ anslist (ANSORDER (ansunique (doall (map #'ANSGEN (semantics node))))))
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

(defn- ambput [code]
    ;; PUTS IN THE JUNK FOR DISCOURSE IF THERE IS NO AMBIGUITY, SO THERE IS
    ;; NO NEED TO EVALUATE THE CODE A SECOND TIME WHEN GIVING THE ANSWER.
    (if AMBIG code (plnr-junkify code)))

(§ defn- ANSBUILD [PLAUS ACTION REDEDUCE]
    ;; BUILDS AN ANSWER NODE.
    ;; IF REDEDUCE IS NON-NIL, IT ADDS A REDEDUCTION OF THE ANSWER,
    ;; ADDING THE DISCOURSE JUNK TO THE ACTION.
    (BUILD
        ANSNODE= (gensym 'ANS)
        PLAUSIBILITY= PLAUS
        ANSRSS= RSS
        ACTION= (concat
                    (if (and AMBIG REDEDUCE (not (cq 'DECLAR)))
                        (cons (list 'thval2 nil (list 'plnr-junkify (list 'plnrcode? (list 'quote RSS)))) ACTION)
                        ACTION)
                    (and (rel? RSS) (not (cq 'DECLAR)) (list (list 'putprop! (quotify (rel? RSS)) (quotify 'REFER=) (quotify ANS)))))))

(§ defn- ANSCOMMAND [RSS]
    ;; ANSCOMMAND RESPONDS TO IMPERATIVES.
    ;; PLNR-ANDORIFY COMBINES ANDS AND ORS INTO APPROPRIATE PLANNER THANDS AND THORS.
    (let [exp (PLNR-ANDORIFY RSS) ANS nil SUCCESS nil] (binding [*plan* nil *plan2* nil]
        (putprop! RSS 'PLNRCODE= exp)
        (SETQ exp (ambput exp))
        (SETQ exp (if (= (car exp) 'THAND)
            (concat exp '((SETQ SUCCESS true) (set! *plan2* *plan*)))
            (list 'THAND exp '(SETQ SUCCESS true) '(set! *plan2* *plan*))))
        ;; IN CASE OF MULTIPLE INTERPRETATION, THE SYSTEM USES FAILURE TO WIPE OUT THE EFFECTS OF TRYING OUT ONE OF THEM.
        ;; BEFORE FAILING IT MARKS DOWN WHETHER IT SUCCEEDED AND SAVES THE PLAN FROM BACKTRACKING.
        ;; PLNR-JUNKIFY PUTS ON THE JUNK FOR SAVING THE DISCOURSE REFERENTS ETC.
        (thval2 nil (if AMBIG (concat exp '((THFAIL))) exp))
        ;; THE THIRD ARGUMENT TO ANSBUILD CAUSES THE SYSTEM TO GO BACK THROUGH THE DEDUCTION
        ;; TO GET THE DATABASE STRAIGHT IF THIS ANSWER IS PICKED.  IT ALSO TAKES CARE OF THE BACKREF STUFF.
        (ANSBUILD
            (if SUCCESS (plausibility? RSS) (- (plausibility? RSS) 512))
            (if SUCCESS (concat (reverse *plan2*) '((say "OK"))) '((say "I CAN'T")))
            true))))

(§ defn- ANSDECLARE [RSS]
    ;; FOR DECLARATIVES.
    (cond
        (or? RSS)
            (global-err "I DON'T UNDERSTAND DISJUNCTIVE DECLARATIVES")
        (and? RSS)
            (let [ANS nil]
                ;; CONJOINED DECLARATIVES ARE HANDLED BY DOING EACH ONE SEPARATELY.
                (SETQ ANS (doall (map #'ANSDECLARE (and? RSS))))
                (ANSBUILD
                    (reduce + (map plausibility? ANS))
                    (cons '(say "I UNDERSTAND") (doall (map #(DELETE '(say "I UNDERSTAND") (action? %)) ANS)))
                    nil))
        (not (istense (parsenode? RSS) 'PRESENT))
            (global-err "I ONLY UNDERSTAND PRESENT TENSE DECLARATIVES")
        :else (ANSBUILD
            (plausibility? RSS)
            ;; ANSTHM GENERATES THE APPROPRIATE ASSERTION OR THEOREM.
            (cons '(say "I UNDERSTAND") (doall (map #(list 'THADD (quotify (ANSTHM %)) nil) (relations? RSS))))
            nil)))

(§ defn- ANSELIMINATE [anslist]
    ;; ELIMINATES ANSWERS FROM LIST BY ASKING PERSON TO CLEAR UP THE AMBIGUITIES.
    (let [AMB (ambiguities? (ansrss? (car anslist))) POSSIBILITIES nil xx nil]
        (when-not AMB
            (bug "ANSELIMINATE -- NO AMBIGUITIES LIST"))
        ;; POSSIBILITIES IS THE LIST OF POSSIBLE INTERPRETATIONS FOR A SINGLE AMBIGUITY.
        ;; WE ARE INSIDE A LOOP STARTING AT UP WHICH GOES THROUGH ALL THE DIFFERENT POSSIBLE AMBIGUITIES ON THE LIST FOR THE FIRST ANSWER ON ANSLIST.
        ;; ON EACH ANSWER WE LOOK FOR POSSIBLE INTERPRETATIONS FOR THE PARTICULAR NODE WHERE THE AMBIGUITY WAS CREATED.
    UP  (SETQ POSSIBILITIES (list (car AMB)))
        (dorun (map (lambda [ANS]
            (and (SETQ xx (parse-assoc (caar AMB) (ambiguities? (ansrss? ANS))))
                (not (memq xx POSSIBILITIES))
                (SETQ POSSIBILITIES (cons xx POSSIBILITIES))))
            (cdr anslist)))
        (cond (cdr POSSIBILITIES) true
            (SETQ AMB (cdr AMB)) (GO UP)
            :else (bug "ANSELIMINATE -- NO CONFLICT"))
        (terpri)
        (print "I'M NOT SURE WHAT YOU MEAN BY \"")
        (dorun (map -print (from (firstword (caddar AMB)) (wordafter (caddar AMB)))))
        (print "\" IN THE PHRASE \"")
        (dorun (map -print (from (firstword (SETQ xx (parent (caddar AMB)))) (wordafter xx))))
        (print "\".")
        (terpri)
        (print "DO YOU MEAN:")
        (dorun (map-indexed (lambda [i POSS]
                (terpri) (print (inc i)) (dorun (map -print (cadr POSS)))) ;; THE PARAPHRASE
            POSSIBILITIES))
        (print \?)
        (terpri)
    READ (SETQ xx (READ))
        (when (or (not (number? xx)) (> xx (count POSSIBILITIES)))
            (terpri)
            (print "PLEASE TYPE ONE OF THE NUMBERS")
            (terpri)
            (GO READ))
        (SETQ POSSIBILITIES (nth POSSIBILITIES (dec xx)))
        (MAPBLAND (lambda [ANS]
            (SETQ xx (parse-assoc (caar AMB) (ambiguities? (ansrss? ANS))))
            (when (or (not xx) (= xx POSSIBILITIES)) ANS))
        anslist)))

(defn- parse-assoc [oss a]
    ;; PARSE-ASSOC GOES THRU AMBIG-LIST LOOKING FOR AN INTERPRETATION WITH THE SAME PARSE NODE
    (let [ass (car (parsenode? oss))]
        (loop-when a a
            (if (= ass (car (parsenode? (caar a)))) (car a) (recur (cdr a))))))

(§ defn- ANSGEN [rss]
    ;; ANSGEN GENERATES AN ANSWER FOR A SINGLE INTERPRETATION.
    (cond
        (or (cq 'IMPER) (and (cq 'QUEST) (istense (parsenode? rss) 'FUTURE))) ;; FUTURE QUESTIONS ARE TREATED LIKE COMMANDS.
            (ANSCOMMAND rss)
        (cq 'DECLAR)
            (let [x nil]
                (cond
                    (ERRSET (SETQ x (ANSDECLARE rss))) x
                    ;; THIS STRANGE CONSTRUCTION ALLOWS US A SECOND CHANCE ON DECLARATIVES ABOUT THINGS WHICH CAN'T
                    ;; BE TOLD TO THE SYSTEM.  IF IT RUNS INTO ONE OF THEM, IT TRIES TO ANSWER IT AS A QUESTION.
                    (= *global-message* "THAT ISN'T THE KIND OF THING I CAN BE TOLD") (ANSQUEST rss)
                    :else (ERR nil)))
        (cq 'QUEST) (ANSQUEST rss)
        :else (bug "ANSGEN -- WHAT KIND OF SENTENCE IS THIS?")))

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
    (let [ansnode nil *c* nil *n* nil *cut* nil]
        (set! *n* (cdaar phrase))                                     ;; CDR IS TO REMOVE "SAY"
        (SETQ ansnode (PARSE2 '(NG ANSNAME) true))                     ;; THE T SAYS NOT TO ATTACH THIS TO THE TREE
        (when-not ansnode
            (RETURN (ert "ANSNAME: FAILURE TO PARSE ANSWER NAME, BUT IF YOU ONLY EXPECT THE ANSWER TO BE AN ADJ, PROCEED THIS AND DON'T WORRY")))
        (set! *ansname* (concat ansnode *ansname*))                     ;; LEAVE NODE AROUND IT ACCESSABLE PLACE
        (putprop! (car (semantics ansnode)) 'REFER= (cadr phrase))          ;; PUT THE REFERENT ON AS THE GUY GIVEN BY ANSWER
        nil))

(§ defn- ANSNOREL [rss]
    ;; FOR QUESTIONS WITH NO RELATIVE, LIKE "DID YOU PICK UP THE BLOCK?" OR "WHY DID YOU DO THAT?"
    (let [ans nil type nil code nil NODE (parsenode? rss) var nil]
        (SETQ type (cond ;; THE TYPE SHOULD BE POLAR, WHY, WHERE, WHEN, OR HOW.
            (isq NODE 'POLAR) 'POLAR
            (SETQ type (getr NODE 'QADJ)) (car (firstword type))
            :else (bug "ANSNOREL -- FUNNY TYPE")))
        (putprop! (variable? rss) 'USED true)
        (SETQ code
            (PLNR-DESCRIBE (relations? rss)
                ;; IN PRESENT TENSE CASES, WE DON'T LOOK FOR EVENTS.
                ;; OTHERWISE WE LOOK FOR A SET OF APPROPRIATE EVENTS NO MATTER WHAT THE TYPE.
                (when-not (istense NODE 'PRESENT) (SETQ var (variable? rss)))
                (list (variable? rss))))
        (putprop! rss 'PLNRCODE= code)
        (cond
            (not var)
                (do (SETQ ans (THVAL-MULT (ambput code)))
                    (ANSBUILD
                        (+ (car ans) (plausibility? rss))
                        (cond (cadr ans) '((say "YES")) (istense NODE 'MODAL) '((say "I DON'T KNOW")) :else '((say "NO")))
                        true))
            (SETQ ans (THVAL-MULT (PLNR-FINDIFY 'ALL var (list var) (ambput code))))
                (ANSBUILD
                    ;; AN ANSWER IS VERY IMPLAUSIBILE IF IT MENTIONS AN EVENT THE SYSTEM CAN'T FIND.
                    (if (cadr ans) (+ (plausibility? rss) (car ans)) (- (plausibility? rss) 512))
                    (if (nil? (cadr ans))
                        '((say "I CAN'T DISCUSS A NON-EXISTENT EVENT"))
                        (concat (and (= type 'POLAR) '((say "YES"))) (list (list 'evlis (list 'describevent (quotify (cadr ans)) (quotify type))))))
                    true))))

(§ defn- ANSORDER [l]
    ;; ORDERS A LIST BY PLAUSIBILITY HIGHEST FIRST.
GO  (let [x l]
    UP  (cond (nil? (cdr x)) (RETURN l)
            (< (plausibility? (car x)) (plausibility? (cadr x)))
                (let [y (car x)] (RPLACA x (cadr x)) (RPLACA (cdr x) y) (GO GO))
            (SETQ x (cdr x)) (GO UP))))

(§ defn- ANSQUEST [RSS]
    ;; ANSQUEST ANSWERS ALL TYPES OF QUESTIONS BY SENDING THEM OUT
    ;; TO ANSREL OR ANSNOREL DEPENDING ON WHETHER THERE IS A REL.
    (cond (or (or? RSS) (and? RSS))
        (let [ans (doall (map #'ANSQUEST (or (and? RSS) (or? RSS))))]
            (ANSBUILD
                (reduce + (map plausibility? ans))
                (concat
                    (and (not (isq (parsenode? RSS) 'COMPONENT)) '((say "YOU'RE TRYING TO CONFUSE ME.")))
                    (doall (map (lambda [QUEST]
                        (concat '((terpri))
                            (list (cons 'say (ELIZA (from (firstword (parsenode? (ansrss? QUEST))) (wordafter (parsenode? (ansrss? QUEST)))))))
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
        (SETQ ANS (THVAL-MULT (ambput CODE)))
        (SETQ PLAUS (car ANS))
        (SETQ LENGTH (count (SETQ ANS (cadr ANS))))
        (RETURN (COND
            ((= TYPE 'ALL)
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS))
                    (COND ((nil? ANS) '((say "YES"))) ((cons '(say "NO, NOT") (prepput (namelist PHRASE 'INDEF ANS)))))
                    true))
            ((= TYPE 'HOWMANY)
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS))
                    (prepput (namesugar LENGTH REL))
                    true))
            ((memq TYPE '(WHICH WHAT))
                (ANSBUILD
                    (+ PLAUS (plausibility? RSS) (COND (ANS 512) (0)))
                    (prepput (namelist PHRASE 'DEF ANS))
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
                                    ((prepput (concat (and (cdr ANS) (concat (namesugar LENGTH REL) '((print \:)))) (namelist PHRASE 'INDEF ANS)))))))))
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
                                    (:else (prepput (concat (namesugar LENGTH REL) '((print \:))))))
                                (prepput (namelist PHRASE 'INDEF ANS))))
                        ((= (car NUM) 'EXACTLY)
                            (COND ((= LENGTH NUM) '((say "YES")))
                                (:else (cons '(say "NO,") (prepput (namesugar LENGTH RES))))))
                        ((= (car NUM) '>)
                            (cons (COND
                                    ((> LENGTH NUM) '(say "YES,"))
                                    ((zero? LENGTH) '(say "NO,"))
                                    (:else '(say "NO, ONLY")))
                                (prepput (namesugar LENGTH REL))))
                        ((= (car NUM) '<)
                            (cons (COND
                                    ((< LENGTH NUM) '(say "YES,"))
                                    (:else '(say "NO,")))
                                (prepput (namesugar LENGTH REL))))
                        ((ert "ANSREL -- FUNNY NUMBER")))
                    true))
            ((ert "ANSREL-- FUNNY TYPE"))))))

(§ defn- ANSTHM [exp]
    ;; GENRATES A THEOREM OR ASSERTION FOR AN EXPRESSION.
    (let [NEG nil VARLIST nil BODY nil]
        (cond
            ;; NOTELL MARKS THAT THIS ISN'T THE KIND OF ASSERTION IT CAN HANDLE.
            ;; IT USES GLOBAL-ERR VAR AND NEG ARE SET AS FREE VARIABLES BY ANSTHMELEMENT WHICH ANALYZES EACH ELEMENT.
            ;; IF THERE ARE NO VARS, IT IS A SIMPLE ASSERTION.
            (term? exp) (notell)
            (not (getprop (car exp) 'TELLABLE)) (notell)
            :else
                (do (SETQ NEG (negative? RSS))
                    (SETQ exp (doall (map ansthmelement (PLNR-REMTIME exp))))
                    (RETURN (if (not (or VARLIST NEG)) exp
                        (PLNR-THCONSIFY VARLIST exp (if NEG (PLNR-PROGIFY nil (list BODY '(THFAIL THEOREM))) BODY))))))
        nil))

(§ defn- ANSTHMADD [oss]
    (SETQ VARLIST (cons (variable? oss) VARLIST))
    (SETQ BODY (COND
        (BODY (PLNR-PROGIFY nil (list BODY (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))))
        (:else (PLNR-DESCRIBE (relations? oss) (variable? oss) (list (variable? oss))))))
    (plnr-var (variable? oss)))

(defn- ansthmelement [x]
    (cond (not (term? x)) x
        (tss? x) (notell)
        (rss? x) (notell)
        (not (oss? x)) x
        (refer? x) (atomify (refer? x))
        (= (quantifier? x) 'ALL) (if NEG (notell) (ANSTHMADD x))
        (= (quantifier? x) 'NO) (do (SETQ NEG true) (ANSTHMADD x))
        (= (quantifier? x) 'NDET) (ANSTHMADD x)
        (not (= (quantifier? x) 'INDEF)) (notell)
        (isq (parsenode? x) 'ANY) (ANSTHMADD x)
        :else (global-err "YOU HAVE TO TELL ME WHICH")))

(defn- ansunique [l]
    ;; THIS FUNCTION SHOULD ELIMINATE ANSWERS WHICH GIVE THE SAME
    ;; RESULT EVEN THOUGH THEY INVOLVE DIFFERENT INTERPRETATIONS.
    ;; IT NEEDS TO CHECK FOR SIGNIFICANT DIFFERENCES, E.G. IN WHAT
    ;; GETS PRINTED OR DONE, WHILE IGNORING INSIGNIFICANT ONES,
    ;; E.G. THE NAMES OF ATOMS TO WHICH THINGS ARE ATTACHED.
    ;; FOR THE MOMENT, IT JUST RETURNS THE LIST UNTOUCHED.
    l)

(defn- cutoff [x]
    ;; FOR CUTTING # OFF OF CONCEPT NAMES TO GET ENGLISH WORDS.
    (READLIST (cdr (EXPLODE x))))

(defn- describevent [event type]
    (let [event (car event)]
        (cond (= type 'WHERE)
                (global-err "I CAN'T ANSWER \"WHERE\" QUESTIONS YET")
            (= type 'WHY)
                (if (= (getprop event 'WHY) 'COMMAND)
                    '((say "BECAUSE YOU TOLD ME TO"))
                    (cons '(say "TO") (nameaction 'INFINITIVE (getprop event 'WHY))))
            (= type 'HOW)
                (let [ans nil]
                    (dorun (map #(and (= (getprop % 'WHY) event) (SETQ ans (cons % ans))) *eventlist*))
                    (if (nil? ans)
                        '((say "I CAN'T ANALYZE HOW I DID IT"))
                        (concat '((say "BY")) (nameaction 'ING (car ans)) (doall (mapcat #(cons '(print \;) (cons '(say "THEN") (nameaction 'ING %))) (cdr ans))))))
            (or (= type 'POLAR) (= type 'WHEN))
                (if (= (getprop event 'WHY) 'COMMAND)
                        (if (= event (toplevel (car *eventlist*)))
                            '((say "JUST NOW"))
                            (cons '(say "BEFORE") (nameaction 'PAST (toplevel (car (findb event *eventlist*))))))
                    (cons '(say "WHILE") (nameaction 'PRES-PAST (toplevel event))))
            :else (bug "DESCRIBEVENT -- FUNNY type"))))

(defn- disput [assertion]
    ;; PUT THE SENTENCE NUMBER ON THE ASSERTION AS A WHO PROPERTY
    (or (not discourse?) (putprop! assertion 'WHO *sentno*)))

(§ defn- ELIZA [node]
    ;; DOES THE OBVIOUS THING.
    (let [xx nil num (count (wordafter node))]
        (APPLY #'concat
            (MAPLIST (lambda [word]
                (COND
                    ((not (< num (count word))) nil)                ;; THIS KLUDGE STOPS IT AT THE END OF THE NODE
                    ((SETQ xx (assq (car word) '((I YOU) (ME YOU) (AM ARE) (ARE AM))))
                        (cdr xx))                                   ;; WE RETURN LIST OF THE THING REALLY WANTED, SO
                    ((= (car word) 'YOU)                           ;; THE APPLY APPEND CAN GET RID OF THE EMPTY ONES.
                        (SETQ xx (FINDMOTHER word node))            ;; UNFORTUNATELY, FOR "YOU" IT IS NECESSARY TO
                        (COND ((isq xx 'SUBJ) '(I))                  ;; DECIDE WHETHER IT SHOULD BE REPLACED BY "I" OR
                            ((isq xx 'OBJ) '(YOU))                   ;; "ME", ACCORDING TO WHETHER IT WAS PARSED AS AN
                            ((bug "ELIZA -- SUBJ OBJ"))))             ;; OBJECT OR SUBJECT. FINDMOTHER IS USED TO FIND
                    ((list (car word)))))                           ;; THE PARSE NODE. WORDS OTHER THAN THE SPECIAL
                (firstword node)))))                                       ;; ONES GO THROUGH DIRECTLY.

(def- timid 200)

(defn- enough-better [ans1 ans2]
    (> (plausibility? ans1) (+ (plausibility? ans2) timid)))

(§ defn- FINDMOTHER [word node]
    ;; FINDMOTHER TAKES A PLACE IN THE SENTENCE AND A GRAMMAR NODE
    ;; (BOTH ARE ACTUALLY LISTS) AND FINDS THE SINGLE-WORD
    ;; CONSTITUTENT BEGINNING AT THAT PLACE IN THE SENTENCE.
    (if (and (= word (firstword node)) (= (cdr word) (wordafter node))) node (APPLY #'concat (MAPLIST #(FINDMOTHER word %) (daughters node)))))

(defn- headpart [node]
    ;; EVERYTHING UP TO THE NOUN, FOR EXAMPLE "THE RED BLOCK" IN "THE RED BLOCK WHICH ..."
    ;; NOTE THAT NODE IS ACTUALLY A LIST OF NODE (A PROPER GRAMMAR POINTER).
    (and (set! *pt* node) (MOVE-PT 'DLC 'PV '(NOUN)) (from (firstword node) (wordafter *pt*))))

(§ defn- LISTNAMES [PHRASE spec names]
    ;; PHRASE IS THE INITIAL THING TO COMPARE FOR USING "ONE", SPEC IS EITHER DEF OR INDEF, AND THE NAMES ARE OF DATA-BASE OBJECTS.
    ;; LISTNAMES PUTS OUT AN ACTION LIST, AS WELL AS PUTTING THINGS ONTO THE BACKREF.
    ;; IT IS CALLED AFTER THE ANSWER HAS BEEN DECIDED ON.
    (let [COUNT nil EXAM nil x nil RES nil ANS nil comma? false]
        ;; NAMEOBJ RETURNS A LIST OF THE OBJECT AND THE ...
        ;; THIS PATCH MAY WELL BE TOTAL OUT OF PHASE WITH THE BACKREF HACKER - DDM 5-12-73 INSTRUCTIONS FOR NAMING IT.
        (SETQ names (doall (map #(NAMEOBJ % spec) names)))
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
            (doall (map (lambda [PHRASE2]
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
    ;; TO HAPPEN, IE. "CLEAR OFF THE BLOCK." VS. "CLEAR IT OFF.", SINCE "CLEAR OFF IT." IS
    ;; UNGRAMMATICAL AND "CLEAR THE BLOCK OFF." WOULD NOT BE APPROPRIATE IN CASES OF HEAVY-NP'S.
    ;;
    ;; AT THE MOMENT, FOR SIMPLICITY'S SAKE, I'VE IGNORED THE
    ;; PROBLEM AND THE PARTICLE IS ALWAYS PUT BEFORE THE NG.
    (cons (list 'say particle) (namelist-evaled '(nil) 'DEF ng)))

(defn- nameaction [tense event]
    ;; THIS FUNCTION SETS UP A LIST OF S-EXPRESSIONS WHICH ARE RETURNED TO DESCRIBEVENT AND WHICH
    ;; WHEN EVALUATED WILL PRINT OUT AN ENGLISH DESCRIPTION OF THE SINGLE, SIMPLE EVENT IMBEDDED
    ;; IN THE LIST "THASSERTION" WITH THE TENSE SPECIFIED.
    ;; THE THASSERTION PROPERTY IS A LIST THAT TYPICALLY LOOKS LIKE "(NIL (2 (3 1 ((!GRASP ßE2 ßB6)))))"
    (let [a (car (caddr (cadadr (getprop event 'THASSERTION)))) verb (cutoff (car a)) obj1 (caddr a) obj2 (cadddr a)]
        (condp = verb
            'CLEARTOP   (cons (list 'say (vbfix 'CLEAN tense false)) (pron-prt "OFF" obj1))
            'GET-RID-OF (cons (list 'say (vbfix 'GET tense true) "RID OF") (namelist-evaled '(nil) 'DEF obj1))
            'GRASP      (cons (list 'say (vbfix 'GRASP tense true)) (namelist-evaled '(nil) 'DEF obj1))
            'PICKUP     (cons (list 'say (vbfix 'PUT tense true)) (pron-prt "UP" obj1))
            'PUTON      (concat (cons (list 'say (vbfix 'PUT tense true)) (namelist-evaled '(nil) 'DEF obj1)) (pron-prt "ON" obj2))
            'PUTIN      (concat (cons (list 'say (vbfix 'PUT tense true)) (namelist-evaled '(nil) 'DEF obj1)) (pron-prt "IN" obj2))
            'STACKUP    (cons (list 'say (vbfix 'STACK tense true)) (pron-prt "UP" obj1))
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
        (thval2 nil '(THGOAL (!NAMEOBJ) (THUSE TC-NAMEOBJ)))
        (or TYPELIST
            (ert "NAMEOBJ -- OBJECT WITH NO !IS ASSERTION"))
        ;; DISPUT CHECKS TO SEE IF DISCOURSE IS BEING KEPT, AND IF SO PUTS THE RELEVANT SENTENCE NUMBER AS A PROPERTY ON THE ASSERTION.
        (disput TYPEß)
        (COND ((= (SETQ TYPE (caddar TYPEß)) '!NAME)                           ;; A NAME IS ITS OWN NAME.
                (RETURN (list (ansay (list ITEM)) (list ITEM))))
            ((memq '!PROPERTY (getprop TYPE 'SYS))
                ;; CUTOFF CUTS THE # OFF OF NAMES LIKE !RED AND !POINTED WHICH ARE USED FOR PROPERTIES.
                (RETURN (list (ansay (list (cutoff ITEM))) (list ITEM))))
            ((not (cdr TYPELIST))
                (RETURN (list (ansay (list 'THE (cutoff TYPE))) (list ITEM))))  ;; THERE IS ONLY ONE OBJECT OF THIS TYPE (E.G. TABLE, BOX, HAND)
            (CUBE (SETQ NAME '(CUBE)))
            ((SETQ NAME (list (cutoff TYPE)))))                                 ;; E.G. !BLOCK BECOMES BLOCK.
        (and NAMEß
            (RETURN (list (ansay (list 'THE (car NAME) 'NAMED (caddar NAMEß))) (list ITEM)))) ;; E.G. THE BLOCK NAMED SUPERBLOCK.
        (disput COLORß)                                                         ;; IF WE HAVEN'T RETURNED YET, COLOR WILL BE NEEDED TO FULLY DESCRIBE THE OBJECT.
        (SETQ NAME (cons (cutoff (caddar COLORß)) NAME))
        (or (cdr COLORLIST)
            (RETURN (list (ansay (cons 'THE NAME)) (list ITEM))))               ;; THERE ARE NO OTHERS OF THE SAME COLOR.  IF THERE ARE, WE MUST USE SIZE AS WELL.
        (SETQ NAME (cons SIZEß NAME))
        (RETURN (list (COND
            ((nil? (cdr SIZELIST))
                (ansay (cons 'THE NAME)))                                       ;; THE SIZE MANAGES TO FINISH SPECIFYING IT.
            ((= spec 'INDEF)
                (ansay (cons 'A NAME)))                                         ;; IN THE INDEFINITE CASE WE DON'T CARE IF THIS ISN'T A FULL SPECIFICATION.
            ((SETQ x (thval2 nil '(THFIND ALL ($? x) (x (y ITEM)) (($G) (!SUPPORT ($? y) ($? x))))))
                (cons (concat '(say "THE") NAME)
                    (cons '(say "WHICH SUPPORTS")
                        (LISTNAMES nil 'INDEF x))))                             ;; IF IT SUPPORTS ANYTHING, NAME THEM.
            ((cons (concat '(say "THE") NAME)
                (cons '(say "WHICH IS TO THE RIGHT OF")
                    (COND ((SETQ x (thval2 nil '(THFIND ALL ($? x) (x (y ITEM))
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
        (COND ((or (nil? NEW) (nil? OLD) (isq NEW 'NUM) (isq NEW 'DET) (not (= (car NEW) (car OLD))))
            (RETURN (cons
                (reverse (cons (if (isq (LAST (car ITEM)) 'NPL) 'ONES 'ONE) NEW))
                (cdr ITEM)))))
        (GO LOOP)))

(defn- ordname [num]
    ;; NAME AN ORDINAL
    (cond (== num 1) 'ONCE (== num 2) 'TWICE :else (READLIST (concat (EXPLODE (namenum num)) '(\space T I M E S)))))

(§ defn- PLNR-ANDORIFY [rss]
    ;; TURNS AN RSS INTO A COLLECTION OF PLANNER CODE FOR A COMMAND.
    (cond
        (and? rss) (PLNR-PROGIFY nil (doall (map #'PLNR-ANDORIFY (and? rss))))
        (or? rss) #_(PLNR-ORIFY nil (doall (map #'PLNR-ANDORIFY (or? rss)))) (ert "SORRY, PLNR-ORIFY NOT WRITTEN")
        :else (PLNR-PROGIFY nil (doall (map #'PLNR-GOALIFY (relations? rss))))))

(defn- prepput [x]
    (if (and (rel? RSS) (set! *pt* (parsenode? (rel? RSS))) (isq (MOVE-PT 'U) 'PREPG))
        (cons (cons 'say (from (firstword *pt*) (firstword (MOVE-PT 'DLC)))) x)
        x))

(§ defn- PLURALIZE [item num]
    ;; CONVERTS A SINGULAR NOUNPHRASE OR "ONCE" STATEMENT INTO PLURAL.
    (cond (< num 2) item
        (memq 'A (car item)) (cons (PLURALMAKE (SUBST (namenum num) 'A (car item))) (cdr item))
        (memq 'ONCE (car item)) (cons (SUBST (ordname num) 'ONCE (car item)) (cdr item))
        :else (bug "PLURALIZE -- FUNNY item")))

(§ defn- PLURALMAKE [phrase]
    ;; CONVERTS SINGULAR PHRASE TO PLURAL.
    (let [SING nil PLURAL nil]
        (when-not (isq (SETQ SING (LAST phrase)) 'NOUN)
            (bug "PLURALMAKE -- NO NOUN"))
        (SETQ PLURAL (MAKNAM (concat (EXPLODE (car SING)) '(S))))
        (when-not (getprop PLURAL 'FEATURES)
            (buildword PLURAL '(NOUN NPL) (semantics SING) (car SING)))
        (SUBST PLURAL (car SING) phrase)))

(§ defn- THVAL-MULT [code]
    ;; DOES A THVAL WITH DIFFERENT VALUES OF WHO (I.E. NIL (EVERYTHING I KNOW),
    ;; 'HE (EVERYTHING HE KNOWS), AND THE PREVIOUS SENTENCE) USED TO TELL IF AN
    ;; ANSWER COULD HAVE BEEN GENERATED WITH HIS KNOWLEDGE TO SEE WHETHER HE REALLY
    ;; MEANT THIS INTERPRETATION.  RETURNS A LIST OF A PLAUSIBILITY AND THE RESULT
    ;; OF THE THVAL USING ALL THE KNOWLEDGE IN THE DATABASE.
    (let [ANS (thval2 nil code)]
        ;; THIS FEATURE IS ONLY RELEVANT IN DISCOURSE AND WHEN THERE ARE AMBIGUITIES.
        (or (and AMBIG discourse?) (RETURN (list 0 ANS)))
        ;; GIVE A VALUE OF 256 IF HE COULDN'T HAVE ANSWERED IT AT ALL.
        (or (= ANS (thval2 'HE code)) (RETURN (list 256 ANS)))
        ;; PLAUSIBILITY IS 0 IF HE COULD HAVE ANSWERED IT WITH RECENTLY MENTIONED INFORMATION.
        ;; 128 IF HE COULD ANSWER IT BUT NOT WITH RECENT INFO.
        (if (= ANS (thval2 (list (- *sentno* 2) (inc *sentno*)) code)) (list 0 ANS) (list 128 ANS))))

(defn- toplevel [event]
    ;; FINDS THE TOP LEVEL EVENT GOING ON AT THE TIME
    (let [why (getprop event 'WHY)] (if (= why 'COMMAND) event (recur why))))

(defn- findreduce [x y]
    (let [x (cdr x) y (dec y)] (if (zero? y) x (recur x y))))

(§ defn- FINDCHOOSE [oss x ANS2]
    (let [HAVE nil NEED nil xx nil ANS nil PLNRCODE nil LOOP nil]
        (and (refer? oss) (RETURN (atomify (refer? oss))))
        (COND
            ((and? oss)
                (RETURN (MAPBLAND (lambda [oss]
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
            ((SETQ xx (FINDCHOOSE (car LOOP) x (concat ANS2 ANS)))
                (SETQ ANS (concat xx ANS))))
        (COND ((and ANS (or? oss)))
            ((SETQ LOOP (cdr LOOP)) (GO UP))
            (ANS)
            ((RETURN nil)))
    DONE (and (term? (variable? oss))
            (putprop! (variable? oss) 'BIND (reverse ANS)))
        (atomify (reverse ANS))))

(§ defn- MUNG [l mung]
    (SETQ mung (list 'quote mung))
    (when discourse? (SETQ l (caddr l)))
    (if (= (caar (cdddr l)) 'THAMONG)
        (RPLACD (cdar (cddddr l)) mung)
        (RPLACD (cdddr l) (cons (list 'THAMONG (list 'THV (cadr (caddr l))) mung) (cddddr l)))))

(defn- vbfix [x tense pp]
    (let [fix- #(when (and pp (memq (car %) CONSO) (memq (cadr %) VOWEL)) (list (car %)))]
        (condp = tense
            'ING (let [x (reverse (EXPLODE x))] (READLIST (reverse (concat '(G N I) (fix- x) x))))
            'PAST (or (getprop x 'PAST) (let [x (reverse (EXPLODE x))] (READLIST (reverse (concat '(D E) (fix- x) x)))))
            'INFINITIVE x
            (bug "VBFIX: WHAT SHOULD I DO WITH THIS TENSE?"))))

#_(ns shrdlu.cgram)

;; #################################################################
;;
;;  CGRAM > THE REGULAR GRAMMAR AFTER GOING THROUGH THE PRECOMPILER
;;
;; #################################################################

(§ defn- CLAUSE []
    (let [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil *position-of-prt* nil *mvb* nil *locationmarker* nil *subj-vb-backup-type1* nil *position-of-ptw* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-CLAUSE
        (when *labeltrace* (passing 'ENTERING-CLAUSE))
        (setr *c* 'TIME (BUILD TSSNODE= (gensym 'TSS)))
        (when (cq 'SIMP) (GO SUBJ))
        (if (cq 'MAJOR) (GO INIT) (GO SEC))
INIT
        (when *labeltrace* (passing 'INIT))
        (set! *locationmarker* *n*)
        (when-not (and (nq 'BINDER) (parse 'CLAUSE 'BOUND 'INIT)) (if *nn* (GO MAJOR) (GO FIXIT)))
        (fq! 'BIND)
        (when (SMBIND) (GO INIT))
FIXIT
        (when *labeltrace* (passing 'FIXIT))
        (set! *ptw* *cut*)
        (if (cut (MOVE-PTW)) (GO INIT) (GO MAJOR))
MAJOR
        (when *labeltrace* (passing 'MAJOR))
        (cut *end*)
        (cond (= *punct* '?) (GO QUEST) (or (cq 'IMPER) (= *punct* '!)) (GO IMPER))
        (GO THEREINIT)
FDEC
        (when *labeltrace* (passing 'FDEC))
        (fq! 'DECLAR)
THEREINIT
        (when *labeltrace* (passing 'THEREINIT))
        (when (and (nextword? *n* 'THERE) (parse nil 'THERE) (fq! 'DECLAR)) (if *nn* (GO THERE) (GO FAIL (m! 'INIT))))
THER2
        (when *labeltrace* (passing 'THER2))
        (and (nq 'PREP) (parse 'PREPG 'INIT) (or (SMRELATE *h*) (POP)))
        (and (nq 'ADV) (parse 'ADV 'TIMW) (or (SMADVERB) (POP)))
        (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD) (or (SMRELATE *h*) (POP)))
        (parse 'NG 'TIME)
        (if (= *locationmarker* *n*) (if *nn* (GO CLAUSETYPE) (GO INPOP)) (GO INIT))
INPOP
        (when *labeltrace* (passing 'INPOP))
        (when-not (MOVE-PT 'C 'DLC) (GO FAIL (m! 'INPOP)))
BICUT
        (when *labeltrace* (passing 'BICUT))
        (cut-back-one)
        (GO INIT)
CLAUSETYPE
        (when *labeltrace* (passing 'CLAUSETYPE))
        (when (cq 'DECLAR) (GO SUBJ))
        (when (and (nq 'VB) (nq 'INF) (parse 'VG 'IMPER) (fq! 'IMPER)) (GO VG1))
        (fq! 'DECLAR)
        (when (cq 'IMPER) (GO FAIL (m! 'IMPER)))
SUBJ
        (when *labeltrace* (passing 'SUBJ))
        (cut *end*)
SUBJ3
        (when *labeltrace* (passing 'SUBJ3))
        (when (or (and (nextword? *n* 'TO) (parse 'CLAUSE 'RSNG 'TO 'SUBJ)) (parse 'CLAUSE 'RSNG 'ING 'SUBJ)) (if *nn* (GO SUBREG) (GO SUBJ1)))
SUBJ4
        (when *labeltrace* (passing 'SUBJ4))
        (when (parse 'NG 'SUBJ) (if *nn* (GO SUBREG) (GO SUBJ1)))
        (cond
            (cq 'REL-NOT-FOUND) (do (rq 'REL-NOT-FOUND) (setr *c* 'SUBJECT (getr *c* 'RELHEAD)) (GO VB))
            *subj-vb-backup-type1* (do (set! *subj-vb-backup-type1* nil) (GO SUBJ11))
            (and *h* (isq *h* 'TIME) (isq *h* 'NG)) (do (setr *c* 'SUBJECT *h*) (GO VB))
            (MOVE-PT 'C 'U '(REL-NOT-FOUND)) (do (setr *c* 'SUBJECT (getr *pt* 'RELHEAD)) (setr *c* 'RELHEAD (getr *pt* 'RELHEAD)) (remove-f-pt 'REL-NOT-FOUND *pt*) (GO VB))
            (and (cq 'COMPONENT) *nn*) (do (fq! 'SUBJFORK) (GO VB))
            *h* (do (POP) (GO SUBJ))
            :else (GO FAIL))
HEAD
        (when *labeltrace* (passing 'HEAD))
        (when-not (or (MOVE-PTW 'N 'PW '(NOUN)) (MOVE-PTW 'N 'PW '(PRON))) (GO FAIL (m! 'HEAD)))
SUB2
        (when *labeltrace* (passing 'SUB2))
        (when-not (POP) (GO FAIL))
        (if (cut *ptw*) (GO INIT) (GO SUB2))
SUBJ1
        (when *labeltrace* (passing 'SUBJ1))
        (when (isq *h* 'QUOTED) (when (isq *h* 'LIST) (fq! 'LIST)) (fq! 'QUOTED) (set! *h* (daughters *h*)) (GO RETSM))
        (and (cq 'REL-NOT-FOUND) (MOVE-PT 'H 'PV '(QAUX))
            (cond (isq *pt* 'BE) (do (fq! 'INT 'AUXBE) (rq 'REL-NOT-FOUND) (setr *c* 'COMP (getr *c* 'RELHEAD)) (setr *c* 'SUBJECT *h*) (setmvb *pt*) (GO ONT))
                (isq *pt* 'HAVE) (do (fq! 'SUBQ) (rq 'REL-NOT-FOUND) (setr *c* 'SUBJECT (getr *c* 'RELHEAD)) (GO VBL))))
SUBJ11
        (when *labeltrace* (passing 'SUBJ11))
        (if (cut-back-one) (GO SUBJ3) (GO FAIL (m! 'SUBJ11)))
SUBREG
        (when *labeltrace* (passing 'SUBREG))
        (setr *c* 'SUBJECT *h*)
        (GO VB)
VB
        (when *labeltrace* (passing 'VB))
        (when (parse 'ADJG 'ADV 'VBAD) (if *nn* (GO VB) (GO FAIL (m! 'VB-ADJG))))
        (rq 'VBLOK)
VBL
        (when *labeltrace* (passing 'VBL))
        (when (parse 'VG) (GO VBREG))
NOVERB
        (when *labeltrace* (passing 'NOVERB))
        (cond (cq 'SUBJFORK) (do (fq! 'VBFORK) (GO FINDOBJ1))
            (isq *h* 'QUOTED) (do (fq! 'REL-NOT-FOUND) (GO SUBJ4))
            (not (isq *h* 'SUBJ)) (GO FAIL)
            (isq *h* 'CLAUSE) (do (set! *subj-vb-backup-type1* true) (POP) (GO SUBJ4))
            (isq *h* 'SUBJ) (do (POP) (fq! 'SUBJFORK) (GO VBL)))
VB2
        (when *labeltrace* (passing 'VB2))
        (cut-back-one)
        (GO SUBJ3)
VBREG
        (when *labeltrace* (passing 'VBREG))
        (setr *c* 'VG *h*)
VG1
        (when *labeltrace* (passing 'VG1))
        (cut *end*)
        (when (isq *mvb* 'BE) (if *nn* (GO BE) (GO FAIL (m! 'BE))))
        (when-not (isq *mvb* 'VPRT) (GO CHECKPASV))
        (when-not (and (nq 'PRT) (parse 'PRT)) (GO DPRT))
        (fq! 'PRT)
        (if (setmvb (combination? (root (firstword *mvb*)) (word (firstword *h*)))) (GO CHECKPASV) (GO POPRT))
DPRT
        (when *labeltrace* (passing 'DPRT))
        (when (isq *h* 'PASV) (GO CHECKPASV))
        (when-not (set! *position-of-prt* (MOVE-PTW 'N 'NW '(PRT))) (GO FINDOBJ1))
        (when-not (setmvb (combination? (root (firstword *mvb*)) (word *position-of-prt*))) (GO POPRT))
        (when-not (isq *mvb* 'TRANS) (GO FINDOBJ1))
        (cut *position-of-prt*)
        (if (parse 'NG 'OBJ 'OBJ1) (GO POPRT) (GO FINDOBJ1))
        (cut *end*)
        (setr *c* 'OBJ1 *h*)
        (parse 'PRT)
        (fq! 'PRT 'DPRT)
        (GO FINDOBJ2)
POPRT
        (when *labeltrace* (passing 'POPRT))
        (POPTO 'VG)
        (GO FINDOBJ1)
CHECKPASV
        (when *labeltrace* (passing 'CHECKPASV))
        (when (and (isq *h* 'PASV) (fq! 'PASV) (setr *c* 'OBJ1 (getr *c* 'SUBJECT))) (if *nn* (GO FINDOBJ2) (GO FINDFAKE2)))
        (fq! 'ACTV)
        (GO FINDOBJ1)
BE
        (when *labeltrace* (passing 'BE))
        (fq! 'BE)
        (and (parse nil 'NOT) (fq! 'NEG))
        (parse 'ADV 'VBAD)
FINDOBJ1
        (when *labeltrace* (passing 'FINDOBJ1))
        (when (or (CANPARSE 1 '(ADJG COMP) 'INT) (CANPARSE 1 '(NG COMP) 'INT)) (if *nn* (GO CHECKIT) (GO ONT)))
        (when (or (CANPARSE 1 '(PREPG COMP) 'INT) (CANPARSE 1 '(CLAUSE RSNG ING) 'TRANS) (CANPARSE 1 '(CLAUSE RSNG REPORT) 'TRANS) (CANPARSE 1 '(CLAUSE RSNG TO) 'TRANS) (CANPARSE 1 '(PREPG LOC) 'ITRNSL) (CANPARSE 1 '(ADV PLACE) 'ITRNSL)) (GO ONT))
        (when (CANPARSE 1 '(NG) 'TRANS) (if *nn* (GO FINDOBJ2) (GO FINDFAKE2)))
FINDFAKE1
        (when *labeltrace* (passing 'FINDFAKE1))
        (when (MOVE-PT 'C 'U '(REL-NOT-FOUND)) (GO OBJ1REL))
        (when (and (CANTAKE 1 '(PREPG LOC) 'ITRNSL) (MOVE-PT 'C 'U '(QADJ)) (isq (getr *pt* 'QADJ) 'PLACE) (fq! 'ITRANSL)) (GO PUTLOBJ))
        (when (CANPARSE 1 nil 'ITRNS) (GO ONT))
GOOF1
        (when *labeltrace* (passing 'GOOF1))
        (or *global-message* (bug "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)
OBJ1REL
        (when *labeltrace* (passing 'OBJ1REL))
        (setr *c* 'OBJ1 (getr *pt* 'RELHEAD))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (fq! 'OBJ1REL)
FINDOBJ2
        (when *labeltrace* (passing 'FINDOBJ2))
        (when (CANPARSE 2 '(CLAUSE RSNG TO) 'TRANS2) (GO FIXSUBJECT))
        (when (or (CANPARSE 2 '(ADV PLACE) 'TRANSL) (CANPARSE 2 '(PREPG LOC) 'TRANSL)) (GO ONT))
        (when (or (CANPARSE 2 '(ADJG COMP) 'TRANSINT) (CANPARSE 2 '(NG COMP) 'TRANSINT)) (GO ONT))
        (when (CANPARSE 2 '(NG) 'TRANS2) (GO ONT))
FINDFAKE2
        (when *labeltrace* (passing 'FINDFAKE2))
        (when (and (isq *mvb* 'TRANS2) (MOVE-PT 'C 'U '(REL-NOT-FOUND))) (GO OBJ2REL))
        (when (and (CANTAKE 2 '(PREPG LOC) 'TRANSL) (MOVE-PT 'C 'U '(QADJ)) (isq (getr *pt* 'QADJ) 'PLACE) (fq! 'TRANSL)) (GO PUTLOBJ))
OBJ2TO
        (when *labeltrace* (passing 'OBJ2TO))
        (parse 'ADV 'VBAD)
        (when (if (and (nextword? *n* 'TO) (isq *mvb* 'TO2) (parse 'PREPG 'TO)) (do (setr *c* 'OBJ2 (getr *h* 'OBJ1)) (fq! 'TRANS2TO 'TRANS2)) (and (cq 'PREPQ) (MOVE-PT 'H 'PV '(QUEST)) (= (word (MOVE-PTW 'FW)) 'TO) (rq 'PREPQ) (fq! 'TRANS2TOQ 'TRANS2) (setr *c* 'OBJ2 (getr *pt* 'OBJ1)))) (GO ONT))
        (if (CANPARSE 2 nil 'TRANS) (GO ONT) (GO FAIL))
PUTLOBJ
        (when *labeltrace* (passing 'PUTLOBJ))
        (setr *c* 'LOBJ *pt*)
        (setr *pt* 'RELHEAD (getr *pt* 'QADJ))
        (setr *pt* 'QADJ nil)
        (remove-f-pt 'QADJ *pt*)
        (GO ONT)
OBJ2REL
        (when *labeltrace* (passing 'OBJ2REL))
        (setr *c* 'OBJ2 (getr *pt* 'RELHEAD))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (fq! 'OBJ2REL)
        (GO ONT)
FIXSUBJECT
        (when *labeltrace* (passing 'FIXSUBJECT))
        (setr *h* 'SUBJECT (getr *c* 'OBJ1))
        (GO ONT)
CHECKIT
        (when *labeltrace* (passing 'CHECKIT))
        (when-not (= (word (firstword (getr *c* 'SUBJECT))) 'IT) (GO ONT))
        (when-not (or (and (nextword? *n* 'TO) (parse 'CLAUSE 'RSNG 'TO 'SUBJ)) (and (nq 'ING) (parse 'CLAUSE 'RSNG 'ING 'SUBJ)) (parse 'CLAUSE 'REPORT)) (GO ONT))
        (fq! 'IT)
        (setr *c* 'LOGICAL-SUBJECT *h*)
        (GO ONT)
GOOF2
        (when *labeltrace* (passing 'GOOF2))
        (or *global-message* (bug "NEW TRANSITIVITY - SECOND OBJECT"))
        (GO FAIL)
ONT
        (when *labeltrace* (passing 'ONT))
        (when (cq 'PASV) (GO PONT))
ONT1
        (when *labeltrace* (passing 'ONT1))
        (when-not (SMCL1) (GO FAIL (m! 'SMCL1)))
        (when-not (cq 'REL-NOT-FOUND) (if *nn* (GO TONT) (GO RETSM)))
        (when-not (isq (getr (getr *c* 'RELHEAD) 'HEAD) 'TIM1) (GO PREPSHORT))
TIMEQ
        (when *labeltrace* (passing 'TIMEQ))
        (rq 'REL-NOT-FOUND)
        (fq! 'TIMEQ)
        (GO TONT)
PREPSHORT
        (when *labeltrace* (passing 'PREPSHORT))
        (when-not (and (nq 'PREP) (parse 'PREPG)) (GO FAIL (m! 'ONT-SHORT-PREP)))
        (when-not (SMRELATE *h*) (GO FAIL (m! 'ONTß)))
        (if (cq 'REL-NOT-FOUND) (if *nn* (GO PREPSHORT) (GO FAIL (m! 'ONT-NOT-FOUND))) (GO TONT))
PONT
        (when *labeltrace* (passing 'PONT))
        (and (nextword? *n* 'BY) (parse 'PREPG 'AGENT) (fq! 'AGENT))
        (setr *c* 'LOGICAL-SUBJECT (getr *h* 'OBJ1))
        (GO ONT1)
TONT
        (when *labeltrace* (passing 'TONT))
        (when-not (set! *position-of-ptw* *n*) (GO RETSM))
NPASV
        (when *labeltrace* (passing 'NPASV))
        (when (and (nq 'PREP) (parse 'PREPG) (SMRELATE *h*) (not *nn*)) (GO RETSM))
        (when (and (nq 'TIMW) (parse 'ADV 'TIMW) (or (smtime) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (not (cq 'BE)) (parse 'ADJG 'ADV) (or (SMRELATE *h*) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (parse 'NG 'TIME) (or (smtime) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (nq 'PLACE) (parse 'ADV 'PLACE) (or (smplace) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (nq 'BINDER) (parse 'CLAUSE 'BOUND) (or (SMBIND) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (nextword? *n* 'TO) (parse 'CLAUSE 'TO 'ADJUNCT) (or (smtoadj) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when-not (= *n* *position-of-ptw*) (if *nn* (GO TONT) (GO RETSM)))
        (when (or (not (cq 'TOPLEVEL)) (nq 'SPECIAL)) (GO RETSM))
        (ert "CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL")
        (GO FAIL)
THERE
        (when *labeltrace* (passing 'THERE))
        (fq! 'THERE)
        (cut *end*)
        (when (and (parse 'ADV 'TIMW) (not *nn*)) (GO FAIL (m! 'THERE)))
        (if (and (parse 'VG) (isq *mvb* 'BE)) (if *nn* (GO THEF) (GO FAIL (m! 'THERE))) (GO NOTHE))
THERQ
        (when *labeltrace* (passing 'THERQ))
        (when (isq (MOVE-PT 'H 'PV '(QAUX)) 'BE) (GO THERQ2))
        (when (and (nq 'TIMW) (parse 'ADV 'TIMW) (not *nn*)) (GO FAIL (m! 'THEREQ)))
        (when (and (parse 'VG) (isq *mvb* 'BE)) (GO THERQ2))
        (rq 'POLR2)
        (GO NOTHE)
THERQ2
        (when *labeltrace* (passing 'THERQ2))
        (fq! 'SUBJTQ 'THERE)
        (if (cq 'POLAR) (GO THEF) (GO ONT))
THEF
        (when *labeltrace* (passing 'THEF))
        (when (and (nq 'ADV) (parse 'ADV 'TIMW) (not *nn*)) (GO FAIL (m! 'THEF)))
        (when-not (parse 'NG 'SUBJ 'SUBJT) (GO THERREL))
        (fq! 'THERE)
        (setr *c* 'SUBJECT *h*)
        (GO ONT)
THERREL
        (when *labeltrace* (passing 'THERREL))
        (when-not (MOVE-PT 'C 'U '(REL-NOT-FOUND)) (GO NOTHE))
        (setr *c* 'SUBJECT (getr *pt* 'RELHEAD))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (GO ONT)
NOTHE
        (when *labeltrace* (passing 'NOTHE))
        (rq 'THERE)
        (POP 'THERE)
        (and (nq 'ADV) (parse 'ADV 'PLACE))
        (GO THER2)
IMPER
        (when *labeltrace* (passing 'IMPER))
        (when (and (parse 'NG 'TIME) (not *nn*)) (GO IMPOP))
        (when (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD) (not *nn*)) (GO IMPOP))
        (when (and (nq 'ADV) (parse 'ADV 'TIMW) (not *nn*)) (GO IMPOP))
IMPE
        (when *labeltrace* (passing 'IMPE))
        (when-not (parse 'VG 'IMPER) (GO IMPOP))
        (fq! 'IMPER)
        (GO VG1)
IMPOP
        (when *labeltrace* (passing 'IMPOP))
        (if (POP nil) (GO IMPE) (GO FAIL (m! 'IMPOP)))
QUEST
        (when *labeltrace* (passing 'QUEST))
        (fq! 'QUEST)
        (when-not (nq 'PREP) (GO NGQUES))
        (when-not (parse 'PREPG) (if *nn* (GO NGQUES) (GO FAIL (m! 'PREPQ-INCOMPLETE))))
        (when-not (isq *h* 'QUEST) (GO QUEST))
        (setr *c* 'QADJ *h*)
        (GO POLAR)
NGQUES
        (when *labeltrace* (passing 'NGQUES))
        (when (parse 'NG 'QUEST) (GO NGQST))
        (if (or (and (nextword? *n* 'HOW) (parse 'ADJG 'QUEST) (setr *c* 'RELHEAD *h*)) (and (nq 'QADJ) (parse 'QADJ) (fq! 'QADJ) (setr *c* 'QADJ *h*))) (GO POLAR) (GO POLAR))
        (fq! 'SHORTQUES)
        (SMADJQSHORT)
ADJQS
        (when *labeltrace* (passing 'ADJQS))
        (GO RETURN)
NGQST
        (when *labeltrace* (passing 'NGQST))
        (setr *c* 'RELHEAD *h*)
NGQST2
        (when *labeltrace* (passing 'NGQST2))
        (cut *end*)
        (setr *c* 'SUBJECT *h*)
        (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD))
        (cond (parse 'VG 'NAUX) (do (fq! 'SUBJQ) (GO VG1)) (nq 'VB) (do (fq! 'REL-NOT-FOUND) (GO POLAR)) :else (do (MOVE-PTW 'N 'PW) (POP 'NG 'QUEST) (cut *ptw*) (GO NGQUES)))
QUEST2
        (when *labeltrace* (passing 'QUEST2))
        (if (and (nextword? *n* 'THERE) (parse nil 'THERE)) (GO THERQ) (GO SUBF))
SUBF
        (when *labeltrace* (passing 'SUBF))
        (when (parse 'NG 'SUBJ) (if *nn* (GO SUBREG) (GO SUBJ1)))
        (rq 'REL-NOT-FOUND)
        (GO BE)
POLAR
        (when *labeltrace* (passing 'POLAR))
        (when-not (and (nq 'VB) (parse 'VB 'AUX '(QAUX)) (setr *c* 'QAUX *h*) (SMVAUX) (setmvb *h*)) (GO QCHOP))
        (or (cq 'QADJ) (getr *c* 'RELHEAD) (fq! 'POLAR))
        (fq! 'POLR2)
        (GO QUEST2)
QCHOP
        (when *labeltrace* (passing 'QCHOP))
        (ert "CLAUSE: QCHOP")
        (if (POPTO 'CLAUSE 'BOUND) (GO BICUT) (GO FAIL (m! 'QCHOP)))
SEC
        (when *labeltrace* (passing 'SEC))
        (cond (cq 'BOUND) (GO BOUND)
            (cq 'TO) (GO TO)
            (cq 'RSQ) (GO RSQ)
            (cq 'REPORT) (GO REPORT)
            (cq 'ING) (GO ING)
            :else (GO FAIL (m! 'RSNG-TYPE)))
BOUND
        (when *labeltrace* (passing 'BOUND))
        (when-not (parse 'BINDER) (if *nn* (GO FAIL (m! 'BOUND)) (GO FAIL (m! 'BINDER))))
        (set! *locationmarker* *n*)
        (GO FDEC)
RSQ
        (when *labeltrace* (passing 'RSQ))
        (setr *c* 'RELHEAD (MOVE-PT 'C 'U '(NG)))
        (when-not (cq 'PREPREL) (GO RSQ2))
        (parse 'PREPG 'PRONREL)
        (setr *c* 'QADJ *h*)
        (GO REPORT)
RSQ2
        (when *labeltrace* (passing 'RSQ2))
        (cond (parse 'VG 'EN 'PASV) (if (isq *mvb* 'TRANS) (do (setr *c* 'SUBJECT (getr *c* 'RELHEAD)) (GO VG1)) (GO FAIL))
            (parse 'VG 'ING) (do (setr *c* 'SUBJECT (getr *c* 'RELHEAD)) (GO VG1))
            (nq 'PRONREL) (do (parse 'NG 'RELWD) (GO REL))
            (cq 'COMPONENT) (do (setr *c* 'RELHEAD (getr (MOVE-PT 'C 'PC) 'RELHEAD)) (GO REL))
            (parse 'NG 'SUBJ) (do (fq! 'REL-NOT-FOUND) (GO SUBREG))
            :else (GO FAIL))
REL
        (when *labeltrace* (passing 'REL))
        (setr *c* 'SUBJECT (getr *c* 'RELHEAD))
        (when (parse 'VG) (GO VG1))
        (fq! 'REL-NOT-FOUND)
        (GO SUBJ)
TO
        (when *labeltrace* (passing 'TO))
        (when (and (cq 'COMPONENT) (parse 'VG 'TO 'TODEL)) (GO VG1))
        (when-not (nextword? *n* 'FOR) (GO TO1))
        (parse nil 'FOR)
        (fq! 'FOR)
        (parse 'NG 'SUBJ 'TOSUBJ)
        (setr *c* 'SUBJECT *h*)
TO1
        (when *labeltrace* (passing 'TO1))
        (if (parse 'VG 'TO) (GO VG1) (GO FAIL (m! 'TO)))
ING
        (when *labeltrace* (passing 'ING))
        (when-not (MOVE-PTW 'N 'NW '(ING)) (GO FAIL))
        (when (and (or (nq 'ING) (cq 'OBJ2) (and (parse 'NG 'SUBJ 'INGSUBJ) (setr *c* 'SUBJECT *h*) (fq! 'SUBING) (rq 'ING))) (not *nn*)) (GO FAIL (m! 'ING)))
        (if (parse 'VG 'ING) (GO VG1) (GO FAIL (m! 'ING)))
REPORT
        (when *labeltrace* (passing 'REPORT))
        (and (nextword? *n* 'THAT) (parse nil 'THAT) (fq! 'THAT))
        (set! *locationmarker* *n*)
        (GO FDEC)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (or (SMCL2) (GO FAIL))
        (GO RETURN)
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- NG []
    (let [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-NG
        (when *labeltrace* (passing 'ENTERING-NG))
NGSTART
        (when *labeltrace* (passing 'NGSTART))
        (cond (cq 'RELWD) (GO RELWD)
            (cq 'QUEST) (GO QUEST)
            (or (nq 'QDET) (nq 'QPRON)) (do (fq! 'QUEST) (GO QUEST))
            (cq 'TIME) (GO TIME)
            (nq 'PROPN) (GO PROPN)
            (nq 'TPRON) (GO TPRON)
            (nq 'EVERPRON) (GO EVERPRON)
            (nq 'PRON) (GO PRON))
LOOK
        (when *labeltrace* (passing 'LOOK))
        (cond (nq 'DET) (GO DET)
            (nq 'NUM) (GO NUM)
            (or (nq 'ING) (nq 'EN) (nq 'ADJ)) (GO ADJ)
            (nq 'CLASF) (GO CLASF)
            (nq 'NUMD) (GO NUMD)
            (nextword? *n* 'AT) (GO AT)
            (nextword? *n* 'AS) (GO AS)
            (nq 'NOUN) (GO NOUN)
            (nq 'TIMORD) (GO TIMORD)
            (and (cq 'COMPONENT) (isq (MOVE-PT 'PC) 'QUEST)) (GO QUEST)
            :else (GO FAIL (m! 'START)))
START
        (when *labeltrace* (passing 'START))
PROPN
        (when *labeltrace* (passing 'PROPN))
        (parse 'PROPN)
        (fq! 'DEF 'PROPNG)
        (when (isq *h* 'POSS) (GO PROPS))
        (when (and *nn* (nq 'PROPN)) (GO PROPN))
PROPS
        (when *labeltrace* (passing 'PROPS))
        (or (SMPROP) (GO FAIL))
        (if (isq *h* 'POSS) (GO POSS) (GO PRAG))
PRON
        (when *labeltrace* (passing 'PRON))
        (when (parse 'PRON 'POSS) (if *nn* (GO POSS) (GO RED2)))
PRON2
        (when *labeltrace* (passing 'PRON2))
        (when (cq 'NPRON) (GO FAIL (m! 'NPRON)))
        (when-not (or (and (cq 'SUBJ) (parse 'PRON 'SUBJ)) (and (or (cq 'OBJ) (cq 'TOSUBJ) (cq 'INGSUBJ)) (parse 'PRON 'OBJ)) (cq 'INGSUBJ)) (GO FAIL (m! 'PRON)))
        (fq! 'PRONG 'DEF)
PRON3
        (when *labeltrace* (passing 'PRON3))
        (when-not (SMPRON *h*) (GO FAIL))
PRAG
        (when *labeltrace* (passing 'PRAG))
        (setr *c* 'HEAD *h*)
        (MOVE-PT 'H)
        (trnsf 'NS 'NPL 'NFS 'NEG)
        (GO RETURN)
TPRON
        (when *labeltrace* (passing 'TPRON))
        (parse 'TPRON)
        (fq! 'TPRON)
        (MOVE-PT 'H)
        (trnsf 'NS 'NPL 'ANY 'NEG)
        (setr *h* 'HEAD *c*)
        (and *nn* (nq 'ADJ) (parse 'ADJ))
        (GO SMNG)
EVERPRON
        (when *labeltrace* (passing 'EVERPRON))
        (when-not (and (parse 'PRON 'EVERPRON) (SMPRON *h*)) (GO FAIL))
        (if (and (parse 'CLAUSE 'RSQ 'NOREL) (SMRELATE *h*)) (GO RETSM) (GO FAIL))
AS
        (when *labeltrace* (passing 'AS))
        (if (and (parse nil 'AS) (parse 'NUMD 'NUMDAS) *nn* (parse nil 'AS) *nn*) (GO NUMD2) (GO FAIL (m! 'AS)))
AT
        (when *labeltrace* (passing 'AT))
        (when-not (and (parse nil 'AT) (parse 'NUMD 'NUMDAT)) (GO FAIL (m! 'AT)))
NUMD2
        (when *labeltrace* (passing 'NUMD2))
        (if (and (parse 'NUM) (fq! 'NUM 'NUMD)) (if *nn* (GO DET1) (GO INCOM)) (GO FAIL (m! 'NUMD2)))
NUMD
        (when *labeltrace* (passing 'NUMD))
        (when-not (parse 'NUMD 'NUMDAN) (if *nn* (GO ND3) (GO INCOM)))
        (if (parse nil 'THAN) (if *nn* (GO NUMD2) (GO POPCOM)) (GO INCOM))
ND3
        (when *labeltrace* (passing 'ND3))
        (if (and (parse 'NUMD 'NUMDALONE) *nn*) (GO NUMD2) (GO FAIL (m! 'NUMD)))
TIME
        (when *labeltrace* (passing 'TIME))
        (when (and (nq 'TIME) (parse 'NOUN 'TIME)) (GO RETSM))
        (if (MOVE-PTW 'N 'NW '(TIM1)) (GO LOOK) (GO FAIL (m! 'TIME)))
TIMORD
        (when *labeltrace* (passing 'TIMORD))
        (when-not (parse 'ORD 'TIMORD) (GO FAIL))
        (if (and (parse 'NOUN 'TIM1) (fq! 'DET 'DEF) (SMNGTIME)) (GO RETURN) (GO FAIL))
DET
        (when *labeltrace* (passing 'DET))
        (parse 'DET)
        (fq! 'DET)
        (MOVE-PT 'H)
        (if (trnsf 'NPL 'NS 'PART 'DEF 'INDEF 'ANY 'NEG 'QNTFR) (if *nn* (GO IND) (GO INCOM)) (GO FAIL (m! 'BUG)))
IND
        (when *labeltrace* (passing 'IND))
        (when (and (= (word (firstword *h*)) 'ALL) (= (word *n*) 'THE) (parse 'DET) (fq! 'DEF)) (if *nn* (GO NUM) (GO FAIL (m! 'THE))))
        (when (and (isq *h* 'QNTFR) (fq! 'QNTFR)) (GO QNUM))
ORD
        (when *labeltrace* (passing 'ORD))
        (when-not (and (parse 'ORD) (fq! 'ORD)) (if *nn* (GO NUM) (GO INCOM)))
        (when (and (nextword? *n* 'OF) (isq (MOVE-PTW 'N 'NW) 'MONTH) (parse nil 'OF) (parse 'NOUN 'MONTH) (fq! 'DATE)) (GO RETSM))
        (when-not (cq 'DEF) (GO ADJ))
NUM
        (when *labeltrace* (passing 'NUM))
        (when-not (parse 'NUM) (GO ADJ))
        (fq! 'NUM)
        (when-not (cq 'DET) (GO DET1))
        (if (cond (and (isq *h* 'NS) (cq 'NS)) (rq 'NPL 'PART) (cq 'NPL) (rq 'NS 'PART)) (if *nn* (GO ADJ) (GO INCOM)) (GO FAIL (m! 'NUM)))
DET1
        (when *labeltrace* (passing 'DET1))
        (if (isq *h* 'NS) (fq! 'NS) (fq! 'NPL))
        (or *nn* (and (fq! 'NUMBER) (GO INCOM)))
NUMBER
        (when *labeltrace* (passing 'NUMBER))
        (fq! 'DET)
        (if (nq 'OF) (GO OF) (GO ADJ))
QNUM
        (when *labeltrace* (passing 'QNUM))
        (when (isq *h* 'NONUM) (GO OF))
        (when-not (and (parse 'NUM) (fq! 'NUM)) (GO OF))
        (when-not (cond (== (semantics *h*) 1) (and (cq 'NS) (rq 'NPL)) (cq 'NPL) (rq 'NS)) (if *nn* (GO FAIL (m! 'NUMD)) (GO INCOM)))
        (when (= (word (firstword *h*)) 'NO) (GO ADJ))
OF
        (when *labeltrace* (passing 'OF))
        (if (and (nq 'OF) (parse 'PREPG 'OF)) (GO SMOF) (GO NONE))
SMOF
        (when *labeltrace* (passing 'SMOF))
        (fq! 'OF)
        (if (or (SMNGOF) (not (POP))) (GO RETSM) (GO INCOM))
NONE
        (when *labeltrace* (passing 'NONE))
        (if (= (word (firstword *h*)) 'NONE) (GO INCOM) (GO ADJ))
ADJ
        (when *labeltrace* (passing 'ADJ))
        (when-not (parse 'ADJ) (if *nn* (GO EPR) (GO INCOM)))
        (and (isq *h* 'COMPAR) (fq! 'COMPARATIVE-MODIFIER) (setr *c* 'COMPARATIVE-MODIFIER *h*))
        (GO ADJ)
EPR
        (when *labeltrace* (passing 'EPR))
        (when-not (or (isq *h* 'SUP) (isq *h* 'COMPAR)) (if *nn* (GO CLASF) (GO INCOM)))
        (fq! 'ADJ)
        (and (nextword? *n* 'OF)
            (parse 'PREPG 'OF)
            (or (SMNGOF) (GO FAIL))
            (fq! 'OF)
            (GO RETSM))
CLASF
        (when *labeltrace* (passing 'CLASF))
        (when (or (parse 'VB 'ING '(CLASF)) (parse 'VB 'EN '(CLASF)) (parse 'CLASF)) (if *nn* (GO CLASF) (GO REDUC)))
NOUN
        (when *labeltrace* (passing 'NOUN))
        (when-not (parse 'NOUN) (GO RED2))
        (when (and (cq 'TIME) (not (isq *h* 'TIM1))) (GO RED1))
        (set! *tmp* *fe*)
        (when (and (isq *h* 'MASS) (or (cq 'PART) (not (cq 'DET)))) (fq! 'MASS))
        (when-not (isq *h* 'NPL) (rq 'NPL 'PART))
        (when-not (isq *h* 'NS) (rq 'NS))
        (when (and (not (cq 'DET)) (not (cq 'NUMD))) (MOVE-PT 'H) (trnsf 'NPL 'MASS))
        (when-not (meet *fe* '(NS NPL PART MASS)) (GO RED0))
        (when-not (nextword? *n* 'THAN) (GO SMNG))
        (fq! 'THAN)
SMNG
        (when *labeltrace* (passing 'SMNG))
        (setr *c* 'HEAD *h*)
        (when (and (cq 'OBOFJ) (not (cq 'DEF))) (GO FAIL))
        (or (SMNG1) (GO FAIL))
        (when (isq *h* 'POSS) (if *nn* (GO POSS) (GO RETSM)))
        (when-not (and (cq 'THAN) (parse 'ADJG)) (GO RSQ-TO))
        (if (SMRELATE *h*) (GO RETSM) (GO FAIL))
RSQ-TO
        (when *labeltrace* (passing 'RSQ-TO))
        (when (and (nextword? *n* 'TO) (meet *fe* '(COMP SUBJ)) (parse 'CLAUSE 'RSQ 'TO) (or (SMRELATE *h*) (GO POPRET))) (GO RETSM))
        (when-not (and (or (nextword? *n* 'AS) (nq 'COMPAR)) (parse 'ADJG 'THANNEED)) (GO PREPNG))
        (and (nil? *n*) (cq 'SUBJ) (isq (MOVE-PT 'C 'PV) 'AUX) (isq *pt* 'BE) (GO POPRET))
        (if (SMRELATE *h*) (if *nn* (GO RSQ-TO) (GO RETSM)) (GO POPRET))
PREPNG
        (when *labeltrace* (passing 'PREPNG))
        (when-not (and (nq 'PREP) (not (or (and (nq 'PLACE) (cq 'NOLOC)) (and (cq 'OBJ1) (isq *mvb* 'TRANSL) (not (isq (MOVE-PT 'C 'U) 'QUEST))))) (parse 'PREPG 'Q)) (GO DISGRSQ))
        (and (nil? *n*)
            (cq 'SUBJ)
            (isq (MOVE-PT 'C 'PV) 'AUX)
            (isq *pt* 'BE)
            (not (isq (MOVE-PT 'U) 'NGQ))
            (GO POPRET))
        (if (SMRELATE *h*) (if *nn* (GO RSQ-TO) (GO RETSM)) (GO POPRET))
DISGRSQ
        (when *labeltrace* (passing 'DISGRSQ))
        (when-not (= (car *mes*) 'PREP-WHICH) (GO RSQ))
        (set! *mes* (cdr *mes*))
        (if (parse 'CLAUSE 'RSQ 'PREPREL) (if *nn* (GO PREPNG) (GO RETSM)) (GO FAIL (m! 'RSQ-PREPREL)))
RSQ
        (when *labeltrace* (passing 'RSQ))
        (when (and (isq (MOVE-PT 'C 'U) 'POLR2) (cq 'SUBJ) (nq 'VB) (not (cq 'SUBJT)) (not (isq *pt* 'QADJ))) (GO RETSM))
        (when-not (parse 'CLAUSE 'RSQ) (GO RETSM))
        (if (SMRELATE *h*) (GO RETSM) (GO POPRET))
RED0
        (when *labeltrace* (passing 'RED0))
        (set! *fe* *tmp*)
RED1
        (when *labeltrace* (passing 'RED1))
        (POP)
RED2
        (when *labeltrace* (passing 'RED2))
        (cond (nil? *h*) (GO FAIL (m! 'NO))
            (isq *h* 'NUMBER) (GO INCOM)
            (and (isq *h* 'POSS) (or (isq *h* 'PRON) (and (MOVE-PT 'H 'DLC) (isq *pt* 'PRON)))) (do (POP) (GO PRON2))
            (and (nil? (cdr *h*)) (cq 'DEFPOSS)) (GO POSSDEF)
            (and (cq 'QUEST) (nil? (cdr *h*))) (GO QDETCHECK)
            (isq *h* 'ADJ) (GO EPR)
            (not (isq *h* 'CLASF)) (GO INCOM))
REDUC
        (when *labeltrace* (passing 'REDUC))
        (POP)
        (if (and (nil? *h*) (nq 'PROPN)) (GO PROPN) (GO NOUN))
POPCOM
        (when *labeltrace* (passing 'POPCOM))
        (POP)
INCOM
        (when *labeltrace* (passing 'INCOM))
        (fq! 'INCOM)
        (when (and (isq *h* 'DET) (isq *h* 'INCOM) (SMINCOM)) (GO RETURN))
        (when (and (nil? *cut*) (cq 'NUM)) (GO SMNG))
QDETCHECK
        (when *labeltrace* (passing 'QDETCHECK))
        (cond (and (isq *h* 'QDET) (isq (firstword *h*) 'QPRON)) (do (POP) (GO QPRON)) (and (isq *h* 'QDET) (isq (firstword *h*) 'EVERPRON)) (do (POP) (GO EVERPRON)))
        (GO FAIL)
POSS
        (when *labeltrace* (passing 'POSS))
        (or (SMNG2) (GO FAIL))
POSS2
        (when *labeltrace* (passing 'POSS2))
        (when (cq 'INGSUBJ) (GO RETSM))
        (set! *h* (buildnode (reverse (cons 'POSS (setdif *fe* '(COMPONENT)))) *nb* *n* *h* *sm*))
        (set! *backref* (concat *h* (cdr *backref*)))
        (when-not (setr *c* 'FEATURES (set! *fe* (concat '(POSES DET DEF NS NPL) (reverse *rest*)))) (GO FAIL (m! 'BUG)))
        (when-not (or (not *nn*) (isq *h* 'DEFPOSS)) (GO ORD))
POSSDEF
        (when *labeltrace* (passing 'POSSDEF))
        (rq 'POSES 'DET 'DEF)
        (fq! 'POSSDEF 'NS 'NPL)
QUEST
        (when *labeltrace* (passing 'QUEST))
        (when-not (parse nil 'HOW) (if *nn* (GO QDET) (GO FAIL)))
        (when-not (parse nil 'MANY) (if *nn* (GO FAIL) (GO INCOM)))
        (fq! 'DET 'NPL 'INDEF 'HOWMANY)
        (GO OF)
QDET
        (when *labeltrace* (passing 'QDET))
        (when (and (parse 'DET 'QDET) (fq! 'DET 'NPL 'QDET 'NS)) (if *nn* (GO QNUM) (GO INCOM)))
QPRON
        (when *labeltrace* (passing 'QPRON))
        (if (parse 'PRON 'QPRON) (GO PRON3) (GO FAIL))
RELWD
        (when *labeltrace* (passing 'RELWD))
        (when (and (parse 'PRONREL) (smset (semantics (MOVE-PT 'C 'U 'U '(NG))))) (GO RETURN))
POPRET
        (when *labeltrace* (passing 'POPRET))
        (POP)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (or (SMNG2) (GO TRYA))
        (GO RETURN)
TRYA
        (when *labeltrace* (passing 'TRYA))
        (when-not (isq *h* 'NOUN) (GO FAIL (m! 'TRYA)))
        (POP)
        (cut *n*)
UP
        (when *labeltrace* (passing 'UP))
        (when (POP) (GO UP))
        (set! *fe* (reverse *rest*))
        (smset nil)
        (GO NGSTART)
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- VG []
    (let [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil *tense* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-VG
        (when *labeltrace* (passing 'ENTERING-VG))
        (cond (cq 'TO) (GO TO)
            (cq 'EN) (GO EN)
            (cq 'ING) (GO ING)
            (cq 'IMPER) (GO IMPER)
            (isq (MOVE-PT 'C 'U) 'POLR2) (GO POLR2))
NEW
        (when *labeltrace* (passing 'NEW))
        (cond (not (nq 'VB)) (GO FAIL (m! 'VB))
            (and (nq 'DO) (parse 'VB 'AUX 'DO)) (GO DO)
            (and (nq 'MODAL) (parse 'VB 'AUX 'MODAL)) (GO MODAL)
            (and (nq 'WILL) (parse 'VB 'AUX 'WILL)) (GO WILL)
            (and (nq 'BE) (parse 'VB 'AUX 'BE)) (GO BE)
            (and (nq 'HAVE) (parse 'VB 'AUX 'HAVE)) (GO HAVE)
            (not (parse 'VB '(MVB))) (GO FAIL (m! 'VB)))
SIMPLE
        (when *labeltrace* (passing 'SIMPLE))
        (MOVE-PT 'C 'DLC)
        (trnsf 'VPL 'INF 'V3PS)
        (set! *tense* (cond (and (isq *pt* 'PRESENT) (isq *pt* 'PAST)) '(PAST-PRESENT) (isq *pt* 'PAST) '(PAST) :else '(PRESENT)))
        (GO REV)
TO
        (when *labeltrace* (passing 'TO))
        (fq! 'NAGR)
        (when (and (parse nil 'NOT) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'NOT)))
        (when-not (or (parse nil 'TO) (cq 'TODEL)) (GO FAIL (m! 'TO)))
        (set! *tense* '(INFINITIVE))
        (GO MODAL2)
EN
        (when *labeltrace* (passing 'EN))
        (fq! 'NAGR)
        (when (and (parse nil 'NOT) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'NOT)))
        (set! *tense* '(PAST))
        (if (and (parse 'VB 'EN '(MVB)) (setmvb *h*) (fq! 'PASV)) (GO RETSM) (GO FAIL))
ING
        (when *labeltrace* (passing 'ING))
        (fq! 'NAGR)
        (when (and (parse nil 'NOT) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'NOT)))
INGADV
        (when *labeltrace* (passing 'INGADV))
        (when (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) (GO INGADV))
        (set! *tense* '(PRESENT))
        (GO BE2)
IMPER
        (when *labeltrace* (passing 'IMPER))
        (when (and (parse 'VB 'DO 'NEG 'INF) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'DONT)))
        (if (and (parse 'VB '(MVB) 'INF) (setmvb *h*) (SMVG)) (GO RETURN) (GO FAIL (m! 'IMPER)))
POLR2
        (when *labeltrace* (passing 'POLR2))
        (or (set! *pt* (getr (MOVE-PT 'C 'U) 'QAUX)) (and (bug "VGßPOLR2") (GO FAIL)))
        (set! *h* (list (car *pt*)))
        (trnsf 'NEG)
        (cond (isq *h* 'DO) (GO DO)
            (isq *h* 'MODAL) (GO MODAL)
            (isq *h* 'WILL) (GO WILL)
            (isq *h* 'BE) (GO BE)
            (isq *h* 'HAVE) (GO HAVE))
        (ert "BUG VGßPOLR2VB")
        (GO FAIL)
DO
        (when *labeltrace* (passing 'DO))
        (fq! 'DO)
        (MOVE-PT 'C 'DLC)
        (trnsf 'VPL 'NEG 'INF 'V3PS)
        (set! *tense* (if (isq *pt* 'PAST) '(PAST) '(PRESENT)))
        (if *nn* (GO DO2) (GO MVB))
DO2
        (when *labeltrace* (passing 'DO2))
        (when (and (parse nil 'NOT) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'NOT)))
ADV2
        (when *labeltrace* (passing 'ADV2))
        (when (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) (if *nn* (GO ADV2) (GO FAIL (m! 'ADV))))
        (when-not (parse 'VB '(MVB) 'INF) (GO MVB))
        (GO REV)
MODAL
        (when *labeltrace* (passing 'MODAL))
        (fq! 'NAGR 'MODAL)
        (set! *tense* '(MODAL))
        (if *nn* (GO MODAL2) (GO INCOMP))
MODAL2
        (when *labeltrace* (passing 'MODAL2))
        (when (and (parse nil 'NOT) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'NOT)))
ADV3
        (when *labeltrace* (passing 'ADV3))
        (when (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) (if *nn* (GO ADV3) (GO FAIL (m! 'ADV))))
        (cond (parse 'VB 'BE 'INF) (if *nn* (GO BE2) (GO MVB))
            (parse 'VB 'HAVE 'INF) (if *nn* (GO HAV2) (GO MVB))
            (parse 'VB 'INF '(MVB)) (GO REV)
            :else (GO INCOMP))
WILL
        (when *labeltrace* (passing 'WILL))
        (fq! 'NAGR)
        (set! *tense* '(FUTURE))
        (if *nn* (GO MODAL2) (GO INCOMP))
BE
        (when *labeltrace* (passing 'BE))
        (MOVE-PT 'C 'DLC)
        (trnsf 'VPL 'INF 'V3PS 'VFS)
        (set! *tense* (if (isq *pt* 'PAST) '(PAST) '(PRESENT)))
        (if *nn* (GO BE2) (GO MVB))
BE2
        (when *labeltrace* (passing 'BE2))
        (when (and (parse nil 'NOT) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'NOT)))
ADV4
        (when *labeltrace* (passing 'ADV4))
        (when (or (parse 'ADV 'TIMW) (parse 'ADV 'VBAD)) (if *nn* (GO ADV4) (GO FAIL (m! 'ADV))))
        (cond (and (nextword? *n* 'GOING) (parse 'VB)) (GO GOING)
            (and (nq 'BE) (parse 'VB 'ING)) (do (set! *tense* (cons 'PRESENT *tense*)) (GO EN2))
            (and (nq 'ING) (parse 'VB 'ING '(MVB))) (do (set! *tense* (cons 'PRESENT *tense*)) (GO REV))
            (cq 'ING) (GO FAIL (m! 'ING)))
EN2
        (when *labeltrace* (passing 'EN2))
        (when-not (parse 'VB 'EN '(MVB)) (GO MVBE))
        (fq! 'PASV)
        (GO REV)
GOING
        (when *labeltrace* (passing 'GOING))
        (when-not (parse nil 'TO) (GO GOI))
        (when (nq 'INF) (GO GOING2))
        (POP)
GOI
        (when *labeltrace* (passing 'GOI))
        (set! *tense* (cons 'PRESENT *tense*))
        (GO MVB)
GOING2
        (when *labeltrace* (passing 'GOING2))
        (set! *tense* (cons 'FUTURE *tense*))
        (GO MODAL2)
MVBE
        (when *labeltrace* (passing 'MVBE))
        (when-not (isq (MOVE-PT 'H 'PV '(VB)) 'AUX) (GO MVB))
        (when-not (isq *pt* 'BE) (GO FAIL (m! 'MVBE)))
        (setmvb *pt*)
        (GO REV)
HAVE
        (when *labeltrace* (passing 'HAVE))
        (MOVE-PT 'C 'DLC)
        (trnsf 'VPL 'INF 'V3PS 'VFS)
        (set! *tense* (if (isq *pt* 'PAST) (do (fq! 'NAGR) '(PAST)) '(PRESENT)))
        (if *nn* (GO HAV2) (GO MVB))
HAV2
        (when *labeltrace* (passing 'HAV2))
        (when (and (parse nil 'NOT) (fq! 'NEG) (not *nn*)) (GO FAIL (m! 'NOT)))
ADV5
        (when *labeltrace* (passing 'ADV5))
        (when (parse 'ADV) (if *nn* (GO ADV5) (GO FAIL (m! 'ADV))))
        (when-not (parse 'VB 'BE 'EN) (GO HAV3))
        (set! *tense* (cons 'PAST *tense*))
        (if *nn* (GO BE2) (GO MVB))
HAV3
        (when *labeltrace* (passing 'HAV3))
        (when-not (parse 'VB '(MVB) 'EN) (GO MVB))
        (set! *tense* (cons 'PAST *tense*))
        (GO REV)
INCOMP
        (when *labeltrace* (passing 'INCOMP))
        (fq! 'INCOMP)
        (GO FAIL)
MVB
        (when *labeltrace* (passing 'MVB))
        (when (= (features *mvb*) (features *h*)) (GO MVB2))
        (POP 'VB)
        (when-not (parse 'VB '(MVB)) (GO FAIL (m! 'MVB)))
MVB2
        (when *labeltrace* (passing 'MVB2))
        (GO REV)
REV
        (when *labeltrace* (passing 'REV))
        (setr *c* 'TENSE *tense*)
        (and *nn* (parse nil 'NOT) (fq! 'NEG))
        (cond (or (= *tense* '(PAST)) (cq 'NAGR) (isq (MOVE-PT 'C 'U) 'IMPER) (isq *pt* 'THERE) (isq *pt* 'RSNG)) (GO NAUX)
            (set! *pt* (getr (MOVE-PT 'C 'U) 'SUBJECT)) *pt*
            :else (bug "VG -- NO SUBJECT TO CHECK FOR AGREEMENT"))
        (set! *tmp* nil)
        (cond (isq *pt* 'NFS) (or (set! *tmp* (meet *fe* '(VFS INF))) (GO NAGR))
            (isq *pt* 'CLAUSE) (or (set! *tmp* (cq 'V3PS)) (GO NAGR))
            (or (isq *pt* 'NS) (isq *pt* 'MASS)) (or (and (cq 'V3PS) (set! *tmp* true)) (feset *pt* (setdif (features *pt*) '(NS MASS)))))
        (when (or (isq *pt* 'PART) (isq *pt* 'NPL)) (or (and (meet *fe* '(INF VPL)) (set! *tmp* true)) (feset *pt* (setdif (features *pt*) '(PART NPL)))))
NAGR
        (when *labeltrace* (passing 'NAGR))
        (when-not (or *tmp* (and (= '(PAST-PRESENT) *tense*) (set! *tense* '(PAST)))) (GO FAIL (m! 'NAGR)))
NAUX
        (when *labeltrace* (passing 'NAUX))
        (setmvb (or (MOVE-PT 'H 'PV '(MVB)) *mvb*))
        (if (and (cq 'NAUX) (isq (MOVE-PT 'H 'PV '(VB)) 'AUX) (not (MOVE-PT 'PV 'PV '(VB)))) (GO FAIL (m! 'NAUX)) (GO RETSM))
POPV
        (when *labeltrace* (passing 'POPV))
        (ert "POPV")
        (GO FAIL)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (if (SMVG) (GO RETURN) (GO FAIL))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- PREPG []
    (let [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-PREPG
        (when *labeltrace* (passing 'ENTERING-PREPG))
ADV
        (when *labeltrace* (passing 'ADV))
        (when (and (nq 'PREPADV) (parse 'ADV 'PREPADV)) (if *nn* (GO ADV) (GO FAIL (m! 'PREPADV))))
        (when-not (cond (cq 'AGENT) (nextword? *n* 'BY) (cq 'LOC) (nq 'PLACE) (cq 'Q) (not (nq 'MOTOR)) :else true) (GO FAIL (m! 'PREP)))
        (when-not (parse 'PREP) (GO FAIL (m! 'PREP)))
        (MOVE-PT 'H)
        (trnsf 'PLACE 'TIME)
        (set! *tmp* *h*)
        (when (nq 'PREP2)
            (cond (set! *tmp* (combination? (word (firstword *h*)) (word *n*))) (parse 'PREP2)
                (set! *tmp* (combination? (word (firstword *h*)) (word *n*) (word (cdr *n*)))) (do (parse 'PREP2) (parse 'PREP2)))
            (set! *tmp* (buildnode (features *tmp*) *nb* *n* 'WORD (semantics *tmp*)))
            (setr *tmp* 'PARENT *c*))
        (when (isq *h* 'NEED2) (GO FAIL (m! 'NEED2)))
        (setr *c* 'HEAD *tmp*)
        (or *nn* (GO SHORT))
        (when (= (word *h*) 'BY) (fq! 'AGENT))
QUEST
        (when *labeltrace* (passing 'QUEST))
        (when-not (cq 'QUEST) (GO NG))
        (if (parse 'NG 'QUEST 'OBJ) (GO OBJR) (GO FAIL (m! 'PREPQUEST)))
        (when (and (cq 'OF) (parse 'NG 'OFOBJ)) (GO OBJR))
NG
        (when *labeltrace* (passing 'NG))
        (when (parse 'NG 'OBJ) (GO OBJR))
REL
        (when *labeltrace* (passing 'REL))
        (when-not (nextword? *n* 'WHICH) (GO REST))
        (when-not (isq (MOVE-PT 'U) 'CLAUSE) (GO FAIL (m! 'PREP-WHICH)))
        (when-not (isq *pt* 'PRONREL) (GO PRONREL))
        (set! *mes* (cdr *mes*))
        (GO P-RELWRD)
PRONREL
        (when *labeltrace* (passing 'PRONREL))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (add-f-pt 'PRONREL *pt*)
P-RELWRD
        (when *labeltrace* (passing 'P-RELWRD))
        (parse 'NG 'RELWD 'OBJ)
        (setr *c* 'OBJ1 (getr *pt* 'HEAD))
        (GO RETT)
REST
        (when *labeltrace* (passing 'REST))
        (if (parse 'CLAUSE 'RSNG 'ING) (GO OBJR) (GO SHORT))
OBJR
        (when *labeltrace* (passing 'OBJR))
        (setr *c* 'OBJ1 *h*)
        (GO RETT)
SHORT
        (when *labeltrace* (passing 'SHORT))
        (when (meet *fe* '(NOSHORT Q)) (GO FAIL (m! 'SHORT)))
        (or (isq (MOVE-PT 'C 'U) 'REL-NOT-FOUND) (isq (getr *pt* 'QUESTION-ELEMENT) 'QADJ) (GO FAIL))
        (remove-f-pt 'REL-NOT-FOUND *pt*)
        (add-f-pt 'PREPREL *pt*)
        (setr *c* 'OBJ1 (getr (MOVE-PT 'C 'U) 'RELHEAD))
RETT
        (when *labeltrace* (passing 'RETT))
        (and (or (isq *h* 'QUEST) (and (isq *h* 'COMPOUND) (MOVE-PT 'H 'H 'PV '(QUEST)))) (fq! 'QUEST))
        (if (SMADJG-PREPG) (GO RETURN) (GO FAIL))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- ADJG []
    (let [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-ADJG
        (when *labeltrace* (passing 'ENTERING-ADJG))
COMPCHECK
        (when *labeltrace* (passing 'COMPCHECK))
        (when (and (MOVE-PT 'C 'U '(BE)) (not (cq 'COMP))) (GO FAIL))
        (when-not (isq (MOVE-PT 'C 'U) 'THAN) (GO DISP))
        (setr *c* 'HEAD (getr *pt* 'COMPARATIVE-MODIFIER))
        (GO THAN)
DISP
        (when *labeltrace* (passing 'DISP))
        (when (and (nq 'AS) (parse nil 'AS)) (if *nn* (GO AS) (GO FAIL (m! 'AS))))
        (when (and (nq 'AS) (parse nil 'AS)) (if *nn* (GO AS) (GO FAIL (m! 'AS))))
        (if (nextword? *n* 'HOW) (GO HOW) (GO ADV))
HOW
        (when *labeltrace* (passing 'HOW))
        (when-not (and (parse nil 'HOW) (fq! 'QUEST)) (GO FAIL))
        (when (and (parse 'ADJ) (fq! 'ADJ) (setr *c* 'HEAD *h*)) (GO RETSM))
        (if (and (parse 'ADV 'VBAD) (fq! 'VBAD) (setr *c* 'HEAD *h*)) (GO RETSM) (GO FAIL))
ADV
        (when *labeltrace* (passing 'ADV))
        (when (parse 'ADV 'ADVADV) (if *nn* (GO ADV) (GO POPAD)))
        (when-not (parse nil 'MORE) (GO ADJ))
        (fq! 'COMPAR)
ADJ
        (when *labeltrace* (passing 'ADJ))
        (when-not (if (cq 'ADV) (parse 'ADV 'VBAD) (parse 'ADJ)) (GO FAIL (m! 'ADJ)))
        (when (and (setr *c* 'HEAD *h*) (not *nn*)) (GO RETSM))
        (when-not (or (cq 'COMPAR) (isq *h* 'COMPAR)) (GO RETSM))
        (fq! 'COMPAR)
        (when-not *nn* (GO RETSM))
THAN
        (when *labeltrace* (passing 'THAN))
        (when-not *nn* (GO RETSM))
        (when-not (parse nil 'THAN) (if *nn* (GO RETSM) (GO FAIL (m! 'THAN))))
        (rq 'THANNEED)
        (fq! 'THAN)
        (GO SUBJ)
AS
        (when *labeltrace* (passing 'AS))
        (fq! 'AS)
        (rq 'THANNEED)
        (when-not (and (parse 'ADJ) (setr *c* 'HEAD *h*)) (if *nn* (GO FAIL (m! 'ADJ)) (GO RETSM)))
        (if (parse nil 'AS) (if *nn* (GO SUBJ) (GO FAIL (m! 'AS))) (GO RETSM))
SUBJ
        (when *labeltrace* (passing 'SUBJ))
        (when-not (parse 'NG 'SUBJ 'COMPAR) (GO FAIL (m! 'THAN)))
        (when (and (setr *c* 'OBJ1 *h*) (not *nn*)) (GO RETSM))
        (when-not (and (one-word-left *nb*) (parse 'VB 'AUX)) (GO RETSM))
        (when (CHECK-AGREEMENT *h* (cdr *h*)) (GO RETSM))
        (POP)
        (GO RETSM)
POPAD
        (when *labeltrace* (passing 'POPAD))
        (POP)
        (GO ADJ)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (when (cq 'THANNEED) (GO FAIL (m! 'THANNEED)))
        (if (SMADJG-PREPG) (GO RETURN) (GO FAIL (m! 'SMADJ)))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- CONJOIN []
    (let [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil *prev* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-CONJOIN
        (when *labeltrace* (passing 'ENTERING-CONJOIN))
UP
        (when *labeltrace* (passing 'UP))
        (set! *prev* (nextword *n*))
        (flushme)
        (when (and (= *prev* '\,) (or (cdr *h*) (> (- (count (firstword *h*)) (count (wordafter *h*))) 4)) (memq (nextword *n*) '(OR AND NOR BUT)) (f! (nextword *n*)))
            (set! *prev* (list *prev* (nextword *n*)))
            (flushme))
        (and (term? *prev*) (MOVE-PTW 'N 'NW '(= (word *ptw*) *prev*)) (cut *ptw*))
        (and (or (= *prev* 'BUT) (= (cadr *prev*) 'BUT)) (nextword? *n* 'NOT) (or (flushme) (GO LOSE2)) (fq! 'NEGBUT))
        (when-not (cond
            (memq (car *rest*) '(ADJ NUM NOUN PREP VB ADV)) (PARSE3 (concat *rest* '(COMPONENT)) nil)
            (memq (car *rest*) '(NG PREPG ADJG)) (and (not (cq 'OFOBJ)) (PARSE2 (concat *rest* '(COMPONENT)) nil))
            (= (car *rest*) 'CLAUSE)
                (let [*lastsent* (if (isq *h* 'MAJOR) *h* *lastsent*) auxfe (meet (features *h*) '(DECLAR IMPER))]
                    (and (PARSE2 (concat *rest* auxfe '(COMPONENT)) nil) (or (not auxfe) (f! (car auxfe))) (setr *c* 'TIME (getr *h* 'TIME)))))
            (GO LOSE2))
        (cut *end*)
        (cond (not (term? *prev*)) (GO RETSM)
            (= *prev* '\,) (if (nextword? *n* COMMA) (do (fq! 'LIST) (GO UP)) (GO LIST))
            (memq *prev* '(AND OR NOR BUT))
                (do (when (= *both* (firstword *h*)) (fq! 'BOTH))
                (if (or (nextword? *n* 'BUT) (and (nextword? *n* *prev*) (not (and (= *both* (firstword *h*)) (= *prev* 'AND)))))
                    (do (fq! 'LISTA) (f! *prev*) (GO UP))
                    (GO LISTA))))
LOSE2
        (when *labeltrace* (passing 'LOSE2))
        (when (cq 'LISTA) (GO LISTA))
LIST
        (when *labeltrace* (passing 'LIST))
        (when-not (and (= *prev* '\,) (== (count *h*) 2) (isq *h* 'NG) (not (or (isq *h* 'PRONG) (isq (cdr *h*) 'PRONG))) (or (nextword? *n* COMMA) (nil? *n*))) (GO FAIL (m! 'CONJOINß)))
        (flushme)
        (fq! 'APPOSITIVE)
        (GO RETSM)
LISTA
        (when *labeltrace* (passing 'LISTA))
        (f! *prev*)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (fq! 'COMPOUND)
        (and (> (count *h*) 2) (fq! 'LIST))
        (cond (or (cq 'NG) (cq 'NOUN)) (if (cq 'AND) (fq! 'NPL) (do (MOVE-PT 'H) (trnsf 'NPL 'NS 'MASS 'NFS)))
            (cq 'VB) (let [common (getprop 'VB 'ELIM)] (MAP #(SETQ common (meet common (features %))) *h*) (feset *c* (union common (features *c*)))))
        (if (SMCONJ) (GO RETURN) (GO FAIL (m! 'CONJOINß)))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

#_(ns shrdlu.init)

(§ defn- SHRDLU []
    (binding [*thtime* 0 *thtrace* nil *thtree* nil *thxx* nil *thalist* '((nil nil)) *tholist* '((nil nil))
              *global-message* nil
              *savesent* nil *lastsentno* 0 *lastsent* nil *sentno* 1
              *level* nil *sent* nil *punct* nil
              *end* nil *both* nil *backref* nil *backref2* nil *ansname* nil *lastrel* nil *who* nil *pt* nil *ptw* nil *h* nil *n* nil *nb* nil *fe* nil *sm* nil *re* nil *mes* nil *c* nil *cut* nil
              *savept* nil *labeltrace* nil *plannersee* nil]
    =>  (*CATCH 'ABORT-PARSER
            (loop []
                (set! *global-message* nil)
                (set! *lastsentno* (inc *lastsentno*))
                (set! *lastsent* *c*)
                (set! *sentno* (inc *sentno*))
                (set! *level* 0)
                (set! *mes* 'NOPE)
                (set! *backref* nil)                  ;; ???????????????????
                (set! *n* (set! *sent* (ETAOIN)))
                (when (and (ERRSET (set! *pt* (set! *c* (parse 'CLAUSE 'MAJOR 'TOPLEVEL)))) *c*)
                    (set! *fe* (features *c*))
                    (set! *nb* *sent*)
                    (set! *h* (daughters *c*))
                    (or (ERRSET (eval '(ANSWER *c*))) (print (or *global-message* "I DON'T UNDERSTAND."))))
                (recur)))
        (GO =>)))

(§ defn -main [& args]
    ;; LOAD '(PLNR THTRAC SYSCOM MORPHO SHOW PROGMR GINTER GRAMAR DICTIO SMSPEC SMASS SMUTIL NEWANS BLOCKS DATA)
    (binding [*grasplist* nil *eventlist* nil]
        (ERRSET (starthistory))
        (terpri)
        (print "YOU ARE NOW IN A READ-EVAL-PRINT LOOP")
        (terpri)
        (print "TYPE \"GO \" TO ENTER READY STATE")
        (*CATCH 'ABORT-PARSER (ert nil))
        (SHRDLU)))


(ns shrdlu.core)

(defmacro § [& _])

(defmacro def- [s i] `(def ~(vary-meta s assoc :private true) ~i))

(defmacro lambda [& _] `(fn ~@_))
(defmacro defq- [& _] `(defn- ~@_))

(defmacro != [x y] `(not (== ~x ~y)))
(defmacro non-zero? [x] `(not (zero? ~x)))

(defmacro any
    ([f x y] `(~f ~x ~y))
    ([f x y & z] `(let [f# ~f x# ~x _# (any f# x# ~y)] (if _# _# (any f# x# ~@z)))))

(defmacro when' [y & w]
    (let [[_ & w] (if (= '=> (first w)) (rest w) (cons nil w))]
        `(if ~y (do ~@w) ~_)))
(defmacro let-when [x y & w]
    (let [[_ & w] (if (= '=> (first w)) (rest w) (cons nil w))]
        `(let [~@x] (if ~y (do ~@w) ~_))))

(letfn [(l' [x y z w]
    (let [x (cond (vector? x) x (symbol? x) [x x] :else [`_# x]) z (cond (vector? z) `((recur ~@z)) (some? z) `((recur ~z))) [_ & w] (if (= '=> (first w)) (rest w) (cons nil w))]
        `(loop [~@x] (if ~y (do ~@w ~@z) ~_))))]
            (defmacro loop-when [x y & w] (l' x y nil w))
            (defmacro loop-when-recur [x y z & w] (l' x y z w)))
(defmacro recur-if [y z & w]
    (let [z (cond (vector? z) `(recur ~@z) (some? z) `(recur ~z)) _ (if (= '=> (first w)) (second w))]
        `(if ~y ~z ~_)))

(defn- symbol* [& a] (symbol (apply str (apply list* a))))

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

(defn- caddddr [l] (car (cddddr l)))

(defn- llast [l] (let [x (last l)] (when x (list x))))

(defn- term? [q] (not (coll? q)))

(defn- memq [x y] (cond (nil? y) nil (= x (car y)) y :else (recur x (cdr y))))
(defn- assq [x y] (cond (nil? y) nil (= x (caar y)) (car y) :else (recur x (cdr y))))
(def- delq remove)

(def- discourse? true)

(defmacro dynamic-
    ([s] `(def ~(vary-meta s assoc :dynamic true :private true)))
    ([s i] `(def ~(vary-meta s assoc :dynamic true :private true) ~i)))

(dynamic- *thtime*)
(dynamic- *tree*)
(dynamic- *thxx*)
(dynamic- *thalist*)
(dynamic- *tholist*)
(dynamic- *value*)

(dynamic- *expr*)
(dynamic- *branch*)
(dynamic- *thabranch*)

(dynamic- *oops*)
(dynamic- *lastsentno*)
(dynamic- *lastsent*)
(dynamic- *sentno*)
(dynamic- *sent*)
(dynamic- *punct*)

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

(dynamic- *grasplist*)
(dynamic- *eventlist*)

(defn- atomify [x] (cond (term? x) x (cdr x) x :else (car x)))
(defn- listify [x] (if (term? x) (list x) x))
(defn- quotify [x] (list 'quote x))

(defn- -print [x] (print \space) (print x))
(defn- print- [x] (print x) (print \space))

(def- world (atom {}))

(defn- putprop! [x y z] (swap! world assoc-in [x y] z) nil)
(defn- remprop! [x y] (swap! world update x dissoc y) nil)
(defn- getprops [x] (get @world x))
(defn- getprop [x y] (get-in @world [x y]))

(defn- setr [node register value] (putprop! (car node) register value))
(defn- getr [node register] (getprop (car node) register))

(defn- bug!
    ([f m] (bug! f m nil))
    ([f m a] (when a (terpri) (pr a)) (throw (Error. (str f ": " m)))))
(defn- oops! [message] (set! *oops* message) (throw (RuntimeException. message)))

(defn- RPLACA [l x] (§))
(defn- RPLACD [l x] (§))
(defn- ERRSET [_] (§))
(defn- ERR [_] (§))
(defn- SETQ [x y] (§))
(defn- SET [x y] (§))

(defn- map*
    ([s] (map* identity s))
    ([f s] (when-let [s (seq s)] (lazy-seq (cons (f s) (map* f (cdr s)))))))
(defn- subst [x y z] (replace {y x} z))

(declare evlis thadd thamong thamongf thand thandf thandt thante thapply thapply1 thass1 thassert therase thassertf thassertt therasef theraset thasval thbind thbi1 thbranch thbranchun thcond thcondf thcondt thconse thdef thdo thdo1 thdob therasing thfail thfinalize thfind thfindf thfindt thflush thgal thgo thgoal thgoalf thgoalt thip thmatch2 thcheck thunion thmatch1 thmatchlist thmungf thmungt thnofail thnohash thnot thor thor2 thorf thort thpopt thprog thproga thprogf thprogt thpure thputprop assq- threm1 thrembindf thrembindt thremove thremprop threstrict threturn thrplaca thrplacas thurplaca thrplacd thrplacds thurplacd thsetq thsgal thstate thsucceed thtae thtag thtagf thtagt thtrue thtry1 thtry thundof thundot thunique thv1 thv thnv thval thvar? thvars2 thvarsubst thvsetq topcenter showscene atab clear diff half endtime findspace grow locgreater memorend memory occupier order packo packon packord size starthistory startime support tcent tfind timechk %sent forget combination? findb from meet setmod setdif sta union uppercase-ify-char ETAOIN propname buildword undefined setmvb add-f remove-f one-word-left move-pt move-ptw apply-grammar buildnode rebuild word features firstword wordafter daughters semantics parent root cut cut-back-one flushme following previous m! nextword nextword? parse parse2 parse3 parserel pop* popto cq f! feset fq! isq nq rq trnsf passing spread1 conjo comma cantake canparse !bethere !beint both thank !blueprint !build !color !cleanoff !eqdim !grasp !have !in !loc !loc2 !name !notice !on !propdefine !role !stackup smtime smthat smconj smconj2 smvg smpron smvaux smplace smtoadj smadverb smprop smadjqshort smadjg-prepg smit smit2 smngof smng1 smng2 smng3 smone smone2 smone3 smposs smposs2 smrelate smcl1 smcl2 smcl-modifiers smbind smbinder istense imperf? build newcopy relation dobackref evalcheck iterate* iteratex mapbland mapc2 mumble object valueput plnr-junkify plnr-junkify2 plnr-thconsify plnr-findify plnr-findspec plnr-goalify plnr-mung plnr-notify plnr-newbody plnr-progify plnr-numrel plnr-numsub plnr-recommendify plnr-remtime compare-build findmeasure measure plnr-describe relfind ordmake compare-proc expand erqset thval2 who check-markers checkamarker findevents checkrel action? ambiguities? and? ansrss? determiner? end? markers? modifiers? negative? num? or? oss? parsenode? plausibility? plnrcode? qtype? quantifier? refer? rel? relations? relmarkers? rss? rssvar? start? systems? tense? tss? variable? smset answer ambput ansbuild anscommand ansdeclare anseliminate parse-assoc ansgen ansname ansnorel ansorder ansquest ansrel ansthm ansthmadd ansthmelement ansunique cutoff describevent disput eliza enough-better findmother headpart listnames pron-prt nameaction namelist namelist-evaled namenum ansay nameobj namesize namesugar notell onecheck ordname plnr-andorify prepput pluralize pluralmake thval-mult toplevel findreduce findchoose vbfix CLAUSE NG VG PREPG ADJG CONJOIN)

#_(ns shrdlu.data)

;; ###################################################################
;;
;;      DATA > INITIAL MICROPLANNER DATABASE FOR THE BLOCKS WORLD
;;
;; ###################################################################

(dynamic- *handat* '(32 0 0))

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

(§ dorun (map #(thadd % nil) '(
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

    (!color ßB1 !RED)
    (!color ßB2 !GREEN)
    (!color ßB3 !GREEN)
    (!color ßB4 !BLUE)
    (!color ßB5 !RED)
    (!color ßB6 !RED)
    (!color ßB7 !GREEN)
    (!color ßB10 !BLUE)
    (!color ßBOX !WHITE)
    (!color ßTABLE !BLACK)

    (!CALL ßSHRDLU SHRDLU)
    (!CALL ßFRIEND YOU)
)))

(defn- showscene []
    (terpri)
    (print "CURRENT SCENE:")
    (terpri)
    (dorun (map (lambda [obj]
            (terpri)
            (pr obj)
            (print " --> ")
            (dorun (map eval (car (nameobj obj 'DESCRIBE))))
            (-print "AT" (cadr (assq obj ATABLE)))
            (let [obj (thval '(thfind ALL ($? x) (x) (thgoal (!SUPPORT ($? obj) ($? x)))) (list (list 'obj obj)))]
                (when obj
                    (-print "SUPPORTS" obj))))
        '(ßB1 ßB2 ßB3 ßB4 ßB5 ßB6 ßB7 ßB10 ßBOX)))
    (let [obj (thval '(thgoal (!GRASPING ($! x))) '((x THUNBOUND)))]
        (terpri)
        (print "THE HAND IS GRASPING" (if obj (cadar obj) "NOTHING"))
        nil))

#_(ns shrdlu.plnr)

(defmacro $? [x] (list 'thv x))
(defmacro $E [x] (list 'thev x))
(defmacro $! [x] (list 'thnv x))

(defmacro thpush [& a]
    ;; (THPUSH THTREE NEWINFO) CONSES NEWINFO ONTO THTREE.
    (list 'set! (car a) (list 'cons (cadr a) (car a))))

(defn- evlis [l]
    ;; EVLIS EVALS ELEMENTS OF ARG, THEN RETURNS ARG.
    (dorun (map eval l))
    l)

(dynamic- *thtt*)
(dynamic- *thnf*)
(dynamic- *thwh*)
(dynamic- *thlas*)
(dynamic- *thttl*)
(dynamic- *thfst*)
(dynamic- *thfstp*)
(dynamic- *thml*)
(dynamic- *thal*)
(dynamic- *thon*)
(dynamic- *thbs*)
(dynamic- *thy1*)
(dynamic- *thy*)
(dynamic- *thz1*)
(dynamic- *thz*)

(defn- thadd [thtt' thpl]
    ;; THADD ADDS THEOREMS OR ASSERTION TO THE INPUT
    ;; THTT - NAME OF THM OR ACTUAL ASSERTION
    ;; THPL - PROPERTY LIST TO BE PLACED ON ASSERTION DATABASE INPUTS
    ;; RETURNS NIL IF ALREADY THERE, ELSE RETURNS THTT
    (binding [*thtt* thtt' *thnf* nil *thwh* nil *thlas* nil *thttl* nil *thfst* nil *thfstp* nil]
        ;; IF THTT IS ATOMIC, WE ARE ASSERTING A THEOREM
        (let [a (if (term? *thtt*)
                    (let [x (getprop *thtt* 'THEOREM)]
                        ;; IF NO THEOREM PROPERTY, THE GUY MADE A MISTAKE
                        (when-not x
                            (bug! 'thadd "CAN'T THASSERT, NO THEOREM" *thtt*))
                        ;; THWH NOW SET TO KIND OF THEOREM, LIKE THERASING
                        (set! *thwh* (car x))
                        ;; MAKE AN EXTRA POINTER TO THTT
                        (set! *thttl* *thtt*)
                        ;; IF WE HAVE A PL FOR OUR THEOREM, IT GOES ON THE ATOM WHICH IS THE NAME OF THE THEOREM
                        ;; GO THROUGH ITEMS ON PL ONE BY ONE
                        (loop-when-recur thpl thpl (cddr thpl)
                            (thputprop *thtt* (car thpl) (cadr thpl)))
                        (caddr x))
                    (do ;; SO WE HAVE AN ASSERTION TO ASSERT, MAKE THWH REFLECT THIS FACT
                        (set! *thwh* 'thassertion)
                        ;; PROPERTY LIST IS "CDR" OF ASSERTION
                        (set! *thttl* (cons *thtt* thpl))
                        *thtt*))]
            (set! *thnf* 0) ;; THNF IS COUNTER SAYING WHICH ATOM WE ARE FILING UNDER
            (set! *thlas* (count a)) ;; THLAS IS THE NUMBER OF TOP LEVEL ITEMS
            (set! *thfst* true)
            ;; THFST SAYS WE ARE TRYING TO PUT THE ITEM IN FOR THE FIRST TIME.
            ;; WE NEED TO KNOW THIS SINCE THE FIRST TIME THROUGH
            ;; WE MUST TEST THAT THE ASSERTEE IS NOT ALREADY THERE.
            ;; THCK IS INITIALLY THE ASSERTION OR THEOREM PATTERN.
            ;; THE FIRST TIME WE GO INTO THE DATABASE WE CHECK TO SEE IF THE ITEM IS THERE.
            ;; THAT MEANS DOING AN EQUAL TEST ON EVERY ITEM IN THE BUCKET.
            ;; AFTER THE FIRST TIME THIS IS NOT NECESSARY.
            ;; SINCE VARIABLES WILL IN GENERAL HAVE MANY MORE ITEMS IN THEIR BUCKET,
            ;; WE WILL WANT TO DO OUR CHECK ON A NON VARIABLE ITEM IN THE PATTERN.
            (loop [a' nil a a]
                (if (nil? a)
                    ;; THCK NIL MEANS THAT ALL THE ITEMS IN THE PATTERN ARE VARIABLES,
                    ;; SO WE TRY AGAIN ONLY THIS TIME DOING EQUAL CHECK ON
                    ;; THE FIRST VARIABLE.  THFOO NOW IS SIMPLY THE PATTERN.
                    (do (set! *thnf* 0)
                        (set! *thfst* nil)
                        ;; THFSTP SAYS WE AGAIN NEED TO CHECK FOR ASSERTEE
                        ;; BEING IN DATABASE, BUT NOW USE VARIABLES FOR EQ CHECK.
                        (set! *thfstp* true)
                        (recur nil a'))
                    ;; THIP IS THE WORKHORSE FOR THADD.
                    ;; IF IT RETURNS NIL, THE ASSERTEE IS ALREADY IN, SO FAIL.
                    (let [x (thip (car a))]
                        ;; THOK WHICH IS RETURN BY THIP SAYS THAT THE ASSERTEE IS NOT IN ALREADY.
                        (cond (= x 'THOK)
                            (do (set! *thfst* nil)
                                (dorun (map thip (cdr a)))
                                (set! *thnf* 0)
                                (dorun (map thip a'))
                                *thttl*)
                        ;; OTHERWISE WE GO AROUND AGAIN, STILL LOOKING FOR A NON VARIABLE ITEM TO DO THE EQ CHECK.
                        x (recur (concat a' (list (when (= x 'THVRB) (car a)))) (cdr a)))))))))

(defq- thamong [& a]
    ;; EXAMPLE - (THAMONG ($? X) (THFIND ... ))
    ;; $E - (THAMONG ($E ($? X)) (THFIND ... )) CAUSES THE THVALUE OF ($? X) TO BE THE FIRST INPUT TO THAMONG.
    ;; THXX SET TO OLD BINDING CELL OF ($? X) (OR ($E ($? X))) IF ($? X) VALUES PUSHED ONTO THTREE AND THAMONG
    ;; FAILS TO THUNASSIGNED, OLD VALUE AND LIST OF NEW THAMONGF.
    (set! *thxx* (thgal (if (= (caar a) 'thev) (thval (cadar a) *thalist*) (car a)) *thalist*))
    (if (= (cadr *thxx*) 'thunassigned)
        (do (thpush *tree* (list 'thamong *thxx* (thval (cadr a) *thalist*))) nil)
        (memq (cadr *thxx*) (thval (cadr a) *thalist*))))       ;; IF ($? X) ASSIGNED, THAMONG REDUCES TO A MEMBERSHIP TEST

(defn- thamongf []                                                 ;; (CAR THTREE) = (THAMONG OLDBINDINGCELL (NEW VALUES))
    (if (caddar *tree*)                                            ;; LIST OF NEW VALUES NON NIL
        (do (RPLACA (cdadar *tree*) (caaddr (car *tree*)))          ;; REPLACE OLD VALUE WITH NEW VALUE
            (RPLACA (cddar *tree*) (cdaddr (car *tree*)))           ;; POP NEW VALUES
            (set! *branch* *tree*)                                  ;; STORE AWAY TREE FOR POSSIBLE BACKTRACKING
            (set! *thabranch* *thalist*)                                ;; STORE AWAY THALIST FOR POSSIBLE BACKTRACKING
            (thpopt)                                                ;; POP TREE
            true)                                                      ;; SUCCEED
        (do (RPLACA (cdadar *tree*) 'thunassigned)                   ;; NO NEW VALUES LEFT. RETURN X TO THUNASSIGNED,
            (thpopt)                                                ;; POP TREE AND CONTINUE FAILING.
            nil)))

(defq- thand [& a]
    (or (not a)
        (do (thpush *tree* (list 'thand a nil)) (set! *expr* (car a)))))

(defn- thandf [] (thbranchun) nil)

(defn- thandt []
    (if (cdadar *tree*)
        (do (thbranch)
            (set! *expr* (cadr (cadar *tree*)))
            (RPLACA (cdar *tree*) (cdadar *tree*)))
        (thpopt))
    *value*)

(defq- thante [& a]
    ;; DEFINES AND OPTIONALLY ASSERTS ANTECEDENT THEOREMS
    (thdef 'thante a))

(defq- thapply [& a]
    ;; THAPPLY1 DOES THE REAL WORK, ALL WE DO IS GET THE THEOREM OFF THE PROPERTY LIST
    (thapply1 (car a) (getprop (car a) 'THEOREM) (cadr a)))

(defn- thapply1 [thm thb dat]
    ;; MAKE SURE THE THEOREM PATTERN MATCHES THE GOAL
    (if (and (thbind (cadr thb)) (thmatch1 dat (caddr thb)))
        ;; AS FAR AS THTREE GOES, ALL THEOREMS LOOK LIKE THPROG, AND
        ;; WHEN YOU COME DOWN TO IT, THEY ALL ACT LIKE THPROGS.
        (do (thpush *tree* (list 'thprog (cddr thb) nil (cddr thb)))
            ;; CALL THE MAIN THPROG WORKHORSE.
            (thproga)
            true)
        ;; IF THE THEOREM PATTERN DIDN'T MATCH, START FAILING.
        (do (set! *thalist* *tholist*) (thpopt) nil)))

(defn- thass1 [a assert?]
    (binding [*thy1* nil *thy* nil]
        ;; IF YOU SEE "THPSEUDO", SET FLAG "PSEUDO" TO T.
        (let [pseudo? (and (cdr a) (= (caadr a) 'THPSEUDO))
              ;; IF (CAR THA) IS AN ATOM, WE ARE ASSERTING (ERASING) A THEOREM.
              ;; THVARSUBST SUBSTITUTES THE ASSIGNMENTS FOR ALL ASSIGNED VARIABLES.
              ;; THPURE CHECKS THAT ALL VARIABLES ARE ASSIGNED.
              thx (car a) [? thx] (if (term? thx) [true thx] (let [thx (thvarsubst thx nil)] [(thpure thx) thx]))]
            ;; IF WE ARE NOT REALLY ASSERTING, THE VARIABLES DO NOT ALL HAVE TO BE ASSIGNED.
            (when-not (or ? pseudo?)
                (bug! 'thass1 "IMPURE ASSERTION OR ERASURE" thx))
            (let [a (if pseudo? (cddr a) (cdr a))
                  ;; THX IS NOW WHAT WE ARE ASSERTING, AND THA IS THE RECOMMENDATION LIST.
                  ;; WE ARE NOW GOING TO PHYSICALLY ADD OR REMOVE ITEM.
                  ;; IF THPSEUDO, DON'T ALTER THE DATABASE.
                  [thx a]
                    (cond pseudo? [(list thx) a]
                        ;; IF P IS "T", WE ARE ASSERTING, SO USE THADD.
                        ;; THPROP SAYS "MY" CADR IS TO BE EVALED TO GET THE PROPERTY LIST,
                        ;; AND REMOVE THPROP FROM THE RECOMMENDATION LIST.
                        assert? (let [[y a] (if (and a (= (caar a) 'THPROP)) [(eval (cadar a)) (cdr a)] [nil a])]
                            ;; THADD TAKES TWO ARGS: THE FIRST IS ITEM TO BE ADDED,
                            ;; THE SECOND IS THE PROPERTY LIST FOR THE ITEM.
                            [(thadd thx (set! *thy* y)) a])
                        ;; OTHERWISE WE ARE ERASING, SO USE THREMOVE.
                        :else [(thremove thx) a])]
                ;; THE LAST ITEM WILL BE NIL ONLY IF THADD OR THREMOVE FAILED.
                ;; THAT IS, IF THE ITEM TO BE ADDED WAS ALREADY THERE, OR THE ONE TO BE REMOVED WASN'T.
                (when thx
                    ;; TYPE IS THE KIND OF THEOREM WE WILL BE LOOKING FOR.
                    (let [type (if assert? 'thante 'therasing)]
                        ;; IF WE ACTUALLY MUNGED THE DATABASE, PUT THE FACT IN THTREE.
                        (when-not pseudo?
                            (thpush *tree* (list (if assert? 'thassert 'therase) thx *thy*)))
                        ;; MAPCAN IS A MAC-LISP FUNCTION, LIKE MAPCAR BUT USES NCONC.
                        ;; THTAE LOOKS AT THE RECOMENDATION LIST AND PRODUCES A LIST OF INSTRUCTIONS ABOUT WHAT THEOREMS TO TRY.
                        (set! *thy* (doall (mapcat #(thtae % thx type) a)))
                        ;; THEXP IS A HACK TELLING THVAL TO THVAL THIS ITEM BEFORE IT GOES ON TO THE NEXT LINE OF PLANNER CODE.
                        ;; THEXP IS NOW (THDO <APPROPRIATE ANTECEDENT OR ERASING THEOREMS>).
                        (when *thy* (set! *expr* (cons 'thdo *thy*)))
                        thx))))))

;; THASS1 IS USED FOR BOTH ASSERTING AND ERASING, THE "T" AS SECOND ARG TELLS IT THAT WE ARE ASSERTING.

(defq- thassert [& a] (thass1 a true))
(defq- therase [& a] (thass1 a nil))

(defn- thassertf []
    (let [a (cadar *tree*)]
        (thremove (if (term? a) a (car a)))
        (thpopt)
        nil))

(defn- thassertt []
    (let [a (cadar *tree*)]
        (thpopt)
        a))

(defn- therasef []
    (let [a (cadar *tree*)]
        (thadd (if (term? a) a (car a)) (if (term? a) nil (cdr a)))
        (thpopt)
        nil))

(defn- theraset []
    (let [a (cadar *tree*)]
        (thpopt)
        a))

(defq- thasval [& a]
    (let [x (thgal (car a) *thalist*)] (and x (not= (cadr x) 'thunassigned))))

(defn- thbind [a]
    ;; WHEN WE ENTER A NEW THEOREM OR THPROG, WE MUST BIND THE NEW VARIABLES.  A IS THE VARIABLE LIST.
    (set! *tholist* *thalist*) ;; THOLIST IS THE OLD THALIST.
    (or (nil? a) ;; IF A IS NIL, THERE IS NOTHING TO DO.
        ;; WHEN A IS NIL, WE ARE DONE AND JUST PUT A MARKER ON THTREE WITH A POINTER TO THE OLD THALIST,
        ;; SO IT CAN BE RESTORED.
        (loop-when a a => (do (thpush *tree* (list 'THREMBIND *tholist*)) true)
            ;; OTHERWISE ADD TO THALIST THE NEW BINDING CELL.
            (thpush *thalist*
                ;; THE FIRST ELEMENT IS THE NAME OF THE VARIABLE.  IF THE ENTRY IS AN ATOM,
                ;; WE ARE JUST GIVEN THE VARIABLE AND ITS INITIAL ASSIGNMENT IS "THUNASSIGNED",
                ;; I.E. NO INITIAL ASSIGNMENT.
                (cond (term? (car a)) (list (car a) 'thunassigned)
                    ;; OTHERWISE OUR ENTRY IS A LIST.
                    ;; IF THE FIRST ELEMENT OF THE LIST IS THRESTRICT,
                    ;; WE ADD THE RESTRICTION TO THE BINDING CELL.
                    ;; THE CDDR OF THE CELL GIVES THE RESTRICTION LIST.
                    (= (caar a) 'threstrict) (concat (thbi1 (cadar a)) (cddar a))
                    ;; OTHERWISE WE ARE GIVEN BOTH THE VARIABLE AND ITS
                    ;; INITIAL ASSIGNMENT, SO MAKE THE SECOND ELEMENT OF THE
                    ;; BINDING CELL A POINTER TO THE INITIAL ASSIGNMENT.
                    :else (list (caar a) (eval (cadar a)))))
            ;; REPEAT FOR THE NEXT VARIABLE IN THE LIST.
            (recur (cdr a)))))

(defn- thbi1 [x]
    (if (term? x) (list x 'thunassigned) (list (car x) (eval (cadr x)))))

(defn- thbranch []
    ;; THBRANCH IS CALLED BY THPROGT AND WE ARE SUCCEEDING BACKWARDS.
    ;; CAR THTREE IS THE THPROG MARKING.
    (cond ;; THERE ARE NO MORE EXPRESSIONS TO EXECUTE IN THE THPROG.
        (not (cdadar *tree*)) true
        (= *branch* *tree*) (set! *branch* nil)
        ;; NORMAL CASE
        ;; CADDAR THTREE IS THE SECOND OF THE THREE ARGS ON THE THPROG MARK
        ;; THBRANCH AND THABRANCH ARE POINTERS TO THE THTREE AND THALIST
        ;; RESPECTIVELY AT THE POINT WHERE WE HAD JUST SUCCEEDED.
        ;; IN GENERAL, BY THE TIME WE GET BACK TO THE THPROG MARK ON THTREE,
        ;; WE HAVE REMOVED THE THINGS PUT ON THTREE BY THE SUCCESSFUL
        ;; LAST LINE OF THE THPROG.
        ;; WE WILL NOW STORE THIS INFORMATION ON THE THPROG MARK,
        ;; SO THAT IF WE FAIL, WE WILL HAVE RECORDS OF WHAT HAPPEND.
        ;; IT IS STORED BY HACKING THE SECOND ARG TO THE THPROG MARK.
        (RPLACA (cddar *tree*) (cons (list *branch* *thabranch* (cadar *tree*)) (caddar *tree*)))
            ;; WE NOW SETQ THBRANCH TO NIL.  IF THE NEXT LINE ALSO SUCCEEDS,
            ;; THVAL WILL LOOK FOR A NIL THBRRANCH TO INDICATE THAT IT SHOULD
            ;; SETQ IT AGAIN TO THE POINT OF SUCCESS.
            (set! *branch* nil)))

(defn- thbranchun []
    ;; WE ARE NOW FAILING.  THBRANCHUN IS CALLED BY THPROGF.
    (let [x (caddar *tree*)]
        (if x ;; IF THE SECOND ARG TO THE PROG MARK IS NON-NIL, IT MEANS THAT THERE ARE PREVIOUS LINES IN THE THPROG TO FAIL BACK TO.
            (do ;; A COMPARISON OF THIS WITH WHAT HAPPEND IN THBRANCH WILL REVEAL THAT
                ;; ALL WE ARE DOING HERE IS RESTORING THE PROG MARK TO IS STATE BEFORE THE LAST SUCCESS.
                (RPLACA (cdar *tree*) (caddar x))
                (RPLACA (cddar *tree*) (cdr x))
                ;; RESET THALIST AND THTREE.
                (set! *thalist* (cadar x))
                (set! *tree* (caar x))
                true)
            ;; THERE AREN'T ANY MORE THINGS IN THE THPROG TO TRY,
            ;; SO JUST RETURN NIL.
            (do (thpopt) nil))))

(defq- thcond [& a]
    (thpush *tree* (list 'thcond a nil))
    (set! *expr* (caar a)))

(defn- thcondf [] (thor2 nil))

(defn- thcondt []
    (RPLACA (car *tree*) 'thand)
    (RPLACA (cdar *tree*) (caadar *tree*))
    *value*)

(defq- thconse [& a]
    ;; DEFINES AND OPTIONALLY ASSERTS CONSEQUENT THEOREMS
    (thdef 'thconse a))

(defn- thdef [type a]
    ;; DEFINES AND OPTIONALLY ASSERTS THEOREMS
    (let [[name body] (if (term? (car a)) [(car a) (cdr a)] [(gensym (condp = type 'thconse 'TC-G 'thante 'TA-G 'therasing 'TE-G)) a])
          [noassert? body] (if (= (car body) 'THNOASSERT) [true (cdr body)] [false body])]
        (thputprop name 'THEOREM (cons type body))
        (terpri)
        (pr name)
        (cond
            noassert? (-print "DEFINED, BUT NOT ASSERTED")
            (thass1 (list name) true) (-print "DEFINED AND ASSERTED")
            :else (-print "REDEFINED"))
        true))

(defq- thdo [& a]
    (or (not a)
        (do (thpush *tree* (list 'thdo a nil nil)) (set! *expr* (car a)))))

(defn- thdo1 []
    (RPLACA (cdar *tree*) (cdadar *tree*))
    (set! *expr* (caadar *tree*))
    (when *branch*
        (RPLACA (cddar *tree*) (cons *branch* (caddar *tree*)))
        (set! *branch* nil)
        (RPLACA (cdddar *tree*) (cons *thabranch* (car (cdddar *tree*))))))

(defn- thdob []
    (if (cdadar *tree*) (thdo1) (do (RPLACA (car *tree*) 'THUNDO) true)))

(defq- therasing [& a]
    ;; THERASING DEFINES AND OPTIONALLY ASSERTS ERASING THEOREMS
    (thdef 'therasing a))

(defn- thfail [] nil)

(defq- thfinalize [& a]
    (when' a => (bug! 'thfinalize "BAD CALL")
        (if (term? a) a
            (let [[a a2] (condp = (car a) 'thtag [a (cadr a)] 'THEOREM [(list 'thprog) nil] [a nil])]
                (set! *tree* (cons nil *tree*))
                (loop-when [t1 *tree*] (cdr t1) => (bug! 'thfinalize "OVERPOP" a)
                    (let-when [x (cadr t1)
                          [? t1] (cond
                            (and a2 (= (car x) 'thprog) (memq a2 (cadddr x)))
                                (loop-when [x (cddr x)] (car x) => [false (cdr t1)]
                                    (if (= (caaddr (caar x)) a2)
                                        [true nil]
                                        (do (RPLACA x (cdar x)) (recur x))))
                            (or (= (car x) 'thprog) (= (car x) 'thand))
                                (do (RPLACA (cddr x) nil) [(= (car x) (car a)) (cdr t1)])
                            (= (car x) 'THREMBIND)
                                [(= (car x) (car a)) (cdr t1)]
                            :else
                                (do (RPLACD t1 (cddr t1)) [(= (car x) (car a)) t1]))
                    ] ? => (recur t1)
                        (set! *tree* (cdr *tree*))
                        true))))))

(defq- thfind [& a]
    (thbind (caddr a))
    (thpush *tree*
        (list 'thfind
            (cond (= (car a) 'ALL) '(1 nil nil)                             ;; STANDARD ALL
                (number? (car a)) (list (car a) (car a) true)             ;; SINGLE NUMBER
                (number? (caar a)) (car a)                                ;; WINOGRAD CROCK FORMAT
                (= (caar a) 'EXACTLY) (list (cadar a) (inc (cadar a)) nil)
                (= (caar a) 'AT-MOST) (list 1 (inc (cadar a)) nil)
                (= (caar a) 'AS-MANY-AS) (list 1 (cadar a) true)
                :else (cons (cadar a)                                       ;; ONLY THING LEFT IS AT-LEAST
                    (cond (nil? (cddar a)) (list nil true)                  ;; NO AT-MOST
                        (= (caddar a) 'AT-MOST) (list (inc (car (cdddar a))) nil)
                        :else (list (car (cdddar a)) true))))
            (cons 0 nil)
            (cadr a)))
    (thpush *tree* (list 'thprog (cddr a) nil (cddr a)))
    (thproga))

(defn- thfindf []
    (set! *branch* nil)
    (set! *thxx* (cdar *tree*))
    (thpopt)
    (when-not (< (caadr *thxx*) (caar *thxx*)) (cdadr *thxx*)))

(defn- thfindt []
    (let-when [a (cdar *tree*) x (thvarsubst (caddr a) nil)
          ? (when-not (memq x (cdadr a))
                (RPLACD (cadr a) (cons x (cdadr a)))
                (let-when [y (inc (caadr a))] (not= (cadar a) y) => :break
                    (RPLACA (cadr a) y)
                    nil))
    ] (not ?) => (let [_ (set! *branch* nil) _ (and (caddar a) (cdadr a))] (thpopt) _)
        (set! *tree* *branch*)
        (set! *thalist* *thabranch*)
        (set! *branch* nil)
        nil))

(defn- thflush [& a]
    ;; FLUSH ALL ASSERTIONS AND THEOREMS
    ;; INPUT = SEQUENCE OF INDICATORS
    ;; DEFAULT = (THASSERTION THCONSE THANTE THERASING)
    ;; EFFECT = FLUSHES THE PROPERTIES OF THESE INDICATORS FROM ALL ATOMS
    (dorun (map (lambda [b]
        (dorun (map (lambda [c]
            (dorun (map (lambda [d]
                (remprop! d b))
            c)))
        (§ MAKOBLIST nil))))
    (or a '(thassertion thconse thante therasing)))))

(defn- thgal [x a]
    ;; (THGAL ($? X) THALIST) RETURNS THE BINDING CELL (X -) OF X ON THALIST
    (set! *thxx* x)
    (or (assq (cadr x) a) (bug! 'thgal "THUNBOUND" x)))

(defq- thgo [& a]
    (apply thsucceed (cons 'thtag a)))

(defq- thgoal [& a]
    ;; THA = (PATTERN RECOMMENDATION)
    ;; PATTERN IS EITHER EXPLICIT, THE VALUE OF A PLANNER VARIABLE OR THVAL OF $E...
    ;; THA2 = INSTANTIATED PATTERN
    ;; THA1 = RECOMMENDATIONS
    (binding [*thy* nil *thy1* nil *thz* nil *thz1* nil]
        (let [a1 (thvarsubst (car a) true) a2 (cdr a)
              a2 (condp = (caar a2)                         ;; SHOULD DATABASE BE SEARCHED?  TRIED IF NO RECS
                    'THANUM (cons (list 'THNUM (cadar a2)) (cons '(THDBF thtrue) (cdr a2)))
                    'THNODB (cdr a2)                        ;; TRIED IF REC NOT THNODB OR (THDBF PRED)
                    'THDBF a2
                    (cons '(THDBF thtrue) a2))
              a2 (doall (mapcat #(thtry % a1) a2))]         ;; THEOREMS AND ASSERTIONS SATISFYING RECS APPENDED TO RECS
            (when a2
                (thpush *tree* (list 'thgoal a1 a2))      ;; (THGOAL PATTERN MATCHES)
                (RPLACD (cddar *tree*) 262143))
            nil)))                                          ;; FAILS TO THGOALF

(defn- thgoalf []
    ;; BASICALLY ALL IT DOES IS TO SEND OFF TO THTRY1 TO TRY ANOTHER POSSIBILITY.
    ;; IF THTRY1 RETURNS NIL, IT MEANS THAT IT COULDN'T FIND ANOTHER POSSIBILITY
    ;; AND WE SHOULD TELL THVAL THAT WE HAVE FAILED.
    ;; ALL THPOPT DOES IS TO LOB THE THGOAL ENTRY OFF THTREE.
    (or (thtry1) (do (thpopt) nil)))

(defn- thgoalt []
    (let [_ (if (= *value* 'thnoval) (thvarsubst (cadar *tree*) nil) *value*)] (thpopt) _))

(defn- thip [thi]
    ;; THI IS AN ITEM FROM THE ASSERTION OR PATTERN OF THE THEOREM BEING ENTERED.
    ;; THNF IS A FREE VARIABLE FROM THADD (WHO CALLS THIS BUGGER).
    ;; IT SAYS WE ARE LOOKING AT THE N'TH PLACE IN THE PATTERN.
    (set! *thnf* (inc *thnf*))
    ;; THI1 IS THE NAME OF THE ATOM TO LOOK UNDER WHEN THI IS A USUAL ATOM.
    ;; THI1 = THI NUMBERS DON'T HAVE PROPERTY LISTS, SO THEY DON'T COUNT AS
    ;; NORMAL ATOMS, NOR DOES "?" SINCE IT IS A SORT OF VARIABLE IN PLANNER.
    (let-when [[thi _]
            (cond (and (term? thi) (not (= thi '?)) (not (number? thi))) [thi nil]
                ;; SEE IF THI IS A VARIABLE.
                ;; IF WE ARE DOING THIS FOR THE FIRST TIME, DON'T CONSIDER VARIABLES.
                ;; FOR EXPLANATION WHY, SEE THADD.
                (or (= thi '?) (thvar? thi)) (if *thfst* [nil 'THVRB] ['THVRB nil])
                :else [nil 'THVRB])
    ] (nil? _) => _
        ;; OTHERWISE THI IS SOMETHING WITH NO PROPERTY LIST LIKE A NUMBER, OR LIST.
        ;; RETURNING THVRB TO THADD TELLS IT THAT EVERYTHING IS OK SO FAR,
        ;; BUT NOTHING WAS DONE ON THIS ITEM.
        (let [x (getprop thi *thwh*)]
            ;; THWH IS THE NAME OF THE PROPERTY TO LOOK UNDER ON THE ATOM.
            ;; IF THIS PROPERTY IS NOT THERE, THEN WE MUST PUT IT THERE.
            ;; IN PARTICULAR, NO PROPERTY MEANS THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE.
            (cond (not x) (do (putprop! thi *thwh* (list nil (list *thnf* (list *thlas* 1 *thttl*)))) 'THOK)
                ;; IF THE PROPERTY IS "THNOHASH", IT MEANS THAT WE SHOULD NOT BOTHER TO INDEX UNDER THIS ATOM,
                ;; SO JUST RETURN TO THADD.
                (= x 'thnohash) 'THBQF
                ;; LOOK ON THE PROPERTY LIST ENTRY TO SEE IF THERE IS A SUB-ENTRY FOR PATTERNS WITH THIS ATOM IN THE THNF'TH POSITION.
                ;; IF NOT, HACK THE ENTRY SO THERE IS.
                ;; AGAIN THIS IMPLIES THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE.
                :else (let [y (assq *thnf* (cdr x))] (cond
                (not y) (do (concat x (list (list *thnf* (list *thlas* 1 *thttl*)))) 'THOK)
                ;; NOW LOOK WITHIN THE SUB-ENTRY FOR A SUB-SUB-ENTRY.
                ;; I.E. THOSE PATTERNS WHICH ARE ALSO OF THE CORRECT TOTAL LENGTH.
                ;; THLAS IS A VARIABLE FROM THADD WHICH GIVES THE LENGTH OF THE ASSERTEE.
                ;; AGAIN, IF NOT THERE, HACK IT IN.
                :else (let [z (assq *thlas* (cdr y))] (cond
                (not z) (do (concat y (list (list *thlas* 1 *thttl*))) 'THOK)
                ;; THIS BRANCH SAYS THAT WE STILL NEED TO CHECK THAT THE ASSERTEE HAS NEVER BEEN ASSERTED BEFORE.
                ;; THIS MEANS THAT WE MUST LOOK DOWN THE REMAINING SUB-SUB-BUCKET LOOKING FOR THE ASSERTEE.
                (and (or *thfst* *thfstp*)
                        ;; RANDOMNESS DUE TO THE FACT THAT ASSERTIONS HAVE PROPERTY LIST ON THEM,
                        ;; WHILE THEOREM NAMES ARE ATOMS WHOES PROPERTY LISTS ARE OF THE USUAL "INVISIBLE" VARIETY.
                        (if (= *thwh* 'thassertion) (assq *thtt* (cddr z)) (memq *thtt* (cddr z))))
                    ;; IF THE ASSERTEE IS FOUND, RETURN NIL INDICATING FAILURE.
                    nil
                :else (let [sv (cddr z)] (cond
                ;; HACK IN THE LATEST ENTRY INTO THE SUB-SUB-BUCKET.
                sv (do (RPLACA (cdr z) (inc (cadr z)))
                        (RPLACD (cdr z) (concat (list *thttl*) sv))
                        'THOK)
                ;; IF WE GET TO THIS POINT, EVERYTHING IS OK, SO TELL THADD SO.
                :else 'THOK))))))))))

(defn- thmatch2 [thx thy]
    ;; THX IS ONE ITEM FROM THE PATTERN.  THY IS THE CORRESPONDING ITEM FROM THE CANDIDATE.
    ;; THMATCH2 DECIDES IF THE TWO ITEMS REALLY MATCH.
    ;; THOLIST IS THALIST WHICH WAS IN EXISTANCE BEFORE WE STARTED WORKING ON THE CURRENT LINE OF PLANNER CODE.
    ;; STANDARD CHECK FOR $E
    (let [thx (if (= (car thx) 'thev) (thval (cadr thx) *tholist*) thx)
          thy (if (= (car thy) 'thev) (thval (cadr thy) *thalist*) thy)]
        (cond ;; IF EITHER IS A ?, ANYTHING WILL MATCH, SO OK.
            (= thx '?) true
            (= thy '?) true
            ;; IF EITHER IS A VARIABLE, THINGS GET MESSY.
            (or (memq (car thx) '(thv thnv threstrict)) (memq (car thy) '(thv thnv threstrict)))
                (let [[xpair thx]
                        ;; IF THX IS A NORMAL VARIALBE, ESP. WE ARE NOT INTRODUCING NEW RESTRICTIONS AT THIS TIME,
                        ;; THEN XPAIR IS JUST THE BINDING LIST.
                        (cond (thvar? thx) [(thgal thx *tholist*) thx]
                            ;; WE MUST HACK A NEW RESTRICTION ONTO THE BINDING LIST.
                            (= (car thx) 'threstrict)
                                ;; WE ARE "RESTRICTING" A ?.
                                ;; SINCE ? HAS NO BINDING LIST, WE MAKE UP A PSEUDO BINDING LIST.
                                (if (= (cadr thx) '?)
                                    [(cons '? (cons 'thunassigned (concat (cddr thx) nil))) '(thnv ?)]
                                    ;; WE ARE RESTRICTING A VARIABLE.  THIS MEANS THAT WE MUST PUT IT ON THE BINDING LIST.
                                    (let [u (thgal (cadr thx) *tholist*)]
                                        ;; THUNION MAKES SURE WE DON'T PUT THE SAME RESTRICTION ON TWICE.
                                        (thrplacds (cdr u) (thunion (cddr u) (cddr thx)))
                                        [u (cadr thx)]))
                            :else [nil thx])
                      [ypair thy]
                        ;; NOTE THAT IF THX IS NOT A VARIABLE, THEN XPAIR IS ().
                        ;; WE DO THE EXACT SAME THING FOR THY AS WE JUST DID FOR THX.
                        (cond (thvar? thy) [(thgal thy *thalist*) thy]
                            (= (car thy) 'threstrict)
                                (if (= (cadr thy) '?)
                                    [(cons '? (cons 'thunassigned (concat (cddr thy) nil))) '(thnv ?)]
                                    (let [u (thgal (cadr thy) *thalist*)]
                                        (thrplacds (cdr u) (thunion (cddr u) (cddr thy)))
                                        [u (cadr thy)]))
                            :else [nil thy])]
                    ;; X AND Y PAIR ARE THE RESPECTIVE BINDING CELLS WHICH WILL HAVE ANY NEW RESTRICTIONS MENTIONED.
                    ;; IF THX OR THY IS NOT A VARIABLE (I.E. THE OTHER IS), THEN X OR Y PAIR WILL BE NIL.
                    (cond
                        (and xpair
                                ;; THX IS A VARIABLE.  THIS SEES IF THX IS UNASSIGNED.
                                (or (= (car thx) 'thnv) (and (= (car thx) 'thv) (= (cadr xpair) 'thunassigned)))
                                ;; THCHECK MAKES SURE THE RESTRICTIONS (IF ANY) ON THX ARE COMPATIBLE WITH THY.
                                (thcheck (cddr xpair) (if ypair (cadr ypair) thy)))
                            ;; FURTHERMORE, THY IS ALSO A VARIABLE.
                            ;; THIS MEANS WE MUST DO THE MYSTERIOUS VARIABLE LINKING.
                            (if ypair
                                    (do (thrplacas (cdr xpair) (cadr ypair))
                                        ;; IF THY ALSO HAS RESTRICTIONS WHEN WE LINK VARIABLES, WE COMBINE RESTRICTIONS.
                                        (when (cddr ypair)
                                            (thrplacds (cdr xpair) (thunion (cddr xpair) (cddr ypair))))
                                        (thrplacds ypair (cdr xpair)))
                                ;; IF THY IS NOT A VARIALBE, JUST ASSIGN THX TO THY.
                                ;; THRPLACAS WILL HACK THML, THE FREE VARIABLE FROM THMATCH1.
                                (thrplacas (cdr xpair) thy))
                        (and ypair
                                ;; THY IS A VARIABLE AND THX IS EITHER A CONSTANT OR A PREVIOUSLY ASSIGNED VARIALBE.
                                (or (= (car thy) 'thnv)
                                    ;; FURTHERMORE THY IS UNASSIGNED.
                                    (and (= (car thy) 'thv) (= (cadr ypair) 'thunassigned)))
                                ;; MAKE SURE RESTRICTIONS ARE OK.
                                (thcheck (cddr ypair) (if xpair (cadr xpair) thx)))
                            ;; IF THX IS A VARIABLE, LINK.  OTHERWISE JUST ASSIGN THY TO THX.
                            (thrplacas (cdr ypair) (if xpair (cadr xpair) thx))
                        ;; THX IS AN ASSIGED VARIABLE, SO JUST MAKE SURE ITS ASSIGNMENT IS EQUAL TO THY.
                        (and xpair (= (cadr xpair) (if ypair (cadr ypair) thy))) true
                        ;; THX IS A CONSTANT, THY IS A VARIABLE, AND THEY ARE EQUAL.
                        (and ypair (= (cadr ypair) thx)) true
                        ;; LOOSE, SO RETURN WITH AN ERROR.
                        :else (ERR nil)))
            ;; IF THE TWO ARE EQUAL, NATURALLY THEY MATCH.
            (= thx thy) true
            ;; IF NOT, THEY DON'T, SO REPORT FAILURE.
            :else (ERR nil))))

(defn- thcheck [thprd thx]
    (or (nil? thprd)
        (= thx 'thunassigned)
        (ERRSET (dorun (map (lambda [thy] (or (thy thx) (ERR nil))) thprd)))))

(defn- thunion [l1 l2]
    (dorun (map #(when-not (memq % l2) (SETQ l2 (cons % l2))) l1))
    l2)

(defn- thmatch1 [thx thy]
    ;; THX IS THE PATTERN TO BE MATCHED.  THY IS THE POSSIBLE CANDIDATE.
    ;; THMATCH1 DOES PRELIMINARY WORK BEFORE HANDING THE PATTERN AND CANDIDATE OFF TO THMATCH2,
    ;; WHO DOES THE REAL WORK.
    (binding [*thml* nil]
        ;; THML IS A FREE VARIABLE WHO WILL BE HACKED BY THMATCH2 WHEN THMATCH2 IS DONE.
        ;; THML WILL HAVE A RECORD OF ALL VARIABLE ASSIGNMENTS MADE DURING THE MATCH.
        ;; NATURALLY, WE MUST KEEP TRACK, SO IF WE FAIL BACK, WE CAN UNDO THEM.
        ;; WE HAVE TO CHECK THAT THE PATTERN AND CANDIDATE ARE OF THE SAME LENGTH
        ;; SINCE THE USER MAY HAVE SPECIFIED THE CANDIDATE WITH A "THUSE" RECOMMENDATION.
        (let [thx (if (= (car thx) 'thev) (thval (cadr thx) *tholist*) thx)]
            ;; IF THE MATCH FAILS, THMATCH2 EXITS WITH AN ERR
            ;; WILL BE "TRUE" PROVIDED THE MATCH WORKED
            (if (and (= (count thx) (count thy)) (ERRSET (dorun (map thmatch2 thx thy))))
                ;; SO RECORD THE ASSIGNMENTS ON THTREE
                (do (when *thml* (thpush *tree* (list 'THMUNG *thml*))) true)
                ;; IF THE MATCH FAILED, WE MAY STILL HAVE SOME ASSIGNEMENTS ALREADY MADE.
                ;; THESE MUST IMMEDIATELY BE UNDONE.  EVLIS JUST EVALS EVERYTHING ON THML,
                ;; WHICH IS A LIST OF EXPRESSIONS, WHICH WHEN EVALED, UNASSIGN THE VARIABLES.
                (do (dorun (map eval *thml*)) nil)))))

(defn- thmatchlist [thtb thwh]
    ;; THTB IS A PATTERN WHICH EVENTUALLY IS TO BE MATCHED.
    ;; THWH SAYS IF IT IS AN ASSERTION, CONSEQUENT THEOREM, ETC.
    ;; THMATCHLIST GOES THROUGH THE DATABASE, LOOKING ON ALL THE BUCKETS OF THE ATOMS IN THE PATTERN.
    ;; IT RETURNS THE SHORTEST BUCKET TO THGOAL.
    ;; THNF = COUNTER WHICH SAYS WHICH PATTERN ITEM WE ARE WORKING ON.
    ;; THAL = LENGTH OF PATTERN.
    (binding [*thnf* 0 *thal* (count thtb)]
        ;; THL IS THE LENGTH OF THE SHORTEST BUCKET FOUND SO FAR.
        ;; INITIALLY IT IS SET TO A VERY LARGE NUMBER.
        ;; THB1 WILL BE THE REMAINDER OF THE PATTERN TO YET BE WORKED ON.
        ;; WHEN IT IS NIL, WE ARE DONE, SO RETURN THE BUCKET.
        ;; THL1 IS THE BUCKET UNDER THE ATOM.
        ;; THL2 IS THE BUCKET UNDER THE VARIABLE IN THAT POSITION.
        ;; IF WE ARE WORKING ON AN ASSERTION, THL2 WILL BE () SINCE THERE ARE NO VARIABLES IN ASSERTIONS.
        ;; IN THEOREMS, WE MUST TAKE INTO ACCOUNT THE FACT THAT THE THEOREM MAY HAVE EITHER THE CORRECT ATOM,
        ;; OR A VARIALBE IN A GIVEN POSITION, AND STILL MATCH.
        (loop-when [thl 34359738367 thl1 nil thl2 nil thb1 thtb] thb1 => (if thl2 (concat thl1 thl2) thl1)
            (set! *thnf* (inc *thnf*))
            ;; THB2 IS THE ITEM WE ARE WORKING ON IN THIS PASS.  IF IT'S NOT A NORMAL ATOM, SKIP IT AND GO TO NEXT PASS.
            (let-when [thb2 (car thb1) thb1 (cdr thb1)] (and (term? thb2) (not (number? thb2)) (not= thb2 '?)) => (recur thl thl1 thl2 thb1)
                (let-when [tha1 (getprop thb2 thwh)
                      ;; IF THE ITEM DOES NOT HAVE THE PROPERTY ON ITS PROPERTY LIST, THEN IT OBVIOUSLY DOSEN'T HAVE ANY BUCKET AT ALL.
                      ;; SO THA1, WHICH RECORDS THE NUMBER IN THE BUCKET IS SET TO 0.
                      ;; IF A BUCKET IS FOUND, THE FIRST THING IN THE BUCKET WILL BE THE NUMBER OF GOODIES THERE.
                      ;; THE REST WILL BE THE GOODIES.  THE FIRST 0 IN THA1 THEN SAYS THAT THERE WAS NO BUCKET.
                      ;; THE SECOND 0 IS JUST A DUMMY FOR THE GOODIES WHICH ARN'T THERE.
                      ;; SAME IF THERE IS NO SUB-BUCKET FOR THE ATOM IN THE CORRECT POSITION.
                      ;; SAME FOR SUB-SUB-BUCKET (PATTERN LENGTH).
                      tha1 (cond (not tha1) '(0 0)
                            ;; IF IT IS A THNOHASH, WE IGNORE IT JUST LIKE A LIST, OR NUMBER.
                            (= tha1 'thnohash) nil
                            :else (let [tha1 (assq *thnf* (cdr tha1))] (cond
                            (not tha1) '(0 0)
                            :else (let [tha1 (assq *thal* (cdr tha1))] (or tha1 '(0 0))))))
                ] (not tha1) => (recur thl thl1 thl2 thb1)
                    (let [thrn (cadr tha1) tha1 (cddr tha1)]
                        ;; IF IT'S AN ASSERTION, WE DON'T HAVE TO LOOK FOR VARIABLES.
                        (if (= thwh 'thassertion)
                            ;; IF THERE IS NO BUCKET, THEN RETURN, SINCE NOTHING WILL MATCH THE PATTERN.
                            (when-not (zero? thrn)
                                ;; IF THE NEW BUCKET IS SMALLER, IT BECOMES THE SMALLEST SO FAR.
                                (let [[thl thl1] (if (> thl thrn) [thrn tha1] [thl thl1])]
                                    (recur thl thl1 thl2 thb1)))
                            ;; THVRB IS THE ATOM WHICH HAS THE BUCKET FOR VARIABLES.
                            ;; WE WILL NOW LOOK TO SEE IF THERE ARE ANY THEOREMS WHICH HAVE A VARIABLE IN THE CORRECT POSSITION.
                            (let [tha2 (getprop 'THVRB thwh)
                                  tha2 (cond (not tha2) '(0 0)
                                        :else (let [tha2 (assq *thnf* (cdr tha2))] (cond
                                        (not tha2) '(0 0)
                                        :else (let [tha2 (assq *thal* (cdr tha2))] (or tha2 '(0 0))))))
                                  thrvc (cadr tha2) tha2 (cddr tha2)
                                  ;; SEE IF THE SUM OF THE NUMBER OF GOODIES IN THE ATOM BUCKET PLUS
                                  ;; THE NUMBER IN THE VARIABLE BUCKET IS GREATER THAN THE SMALLEST
                                  ;; NUMBER SO FAR.  IF SO, WE KEEP THE PREVIOUS NUMBER.
                                  ;; OTHERWISE THIS BECOMES THE NEW SMALLEST,
                                  ;; AND THL1 AND THL2 ARE POINTERS TO THE NEWLY DISCOVERD BUCKETS.
                                  [thl thl1 thl2] (if (> (+ thrvc thrn) thl) [thl thl1 thl2] [(+ thrvc thrn) tha1 tha2])]
                                (recur thl thl1 thl2 thb1)))))))))

(defn- thmungf [] (dorun (map eval (cadar *tree*))) (thpopt) nil)

(defn- thmungt [] (thpopt) *value*)

(defn- thnofail [x]
    (putprop! 'thprog 'thfail (if x 'thprogt 'thprogf)))

(defq- thnohash [& a]
    (dorun (map #(putprop! (car a) % 'thnohash)
        (or (cdr a) '(thassertion thconse thante therasing)))))

(defq- thnot [& a]
    (set! *expr* (list 'thcond (list (car a) '(thfail)) '((thsucceed)))))

(defq- thor [& a]
    (when a
        (thpush *tree* (list 'thor a))
        (set! *expr* (car a))))

(defn- thor2 [p]
    (if (and (cadar *tree*) (cdadar *tree*))
        (do (RPLACA (cdar *tree*) (cdadar *tree*))
            (set! *expr* (let [x (caadar *tree*)] (if p (do (when-not (cadar *tree*) (thpopt)) x) (car x)))))
        (do (thpopt) nil)))

(defn- thorf [] (thor2 true))

(defn- thort [] (thpopt) *value*)

(defn- thpopt [] (set! *tree* (cdr *tree*)))

(defq- thprog [& a]
    ;; THBIND HACKS THALIST TO BIND THE VARIABLES.
    ;; IT ALSO HACKS THTREE SO WE CAN UNDO IT IF NEEDED.
    (thbind (car a))
    ;; PUT THPROG MARK ON THTREE.
    ;; THE FIRST THA IS A POINTER ONE BEFORE THE NEXT PART OF THE THPROG TO BE HANDELED.
    ;; THE SECOND ONE WILL BE KEPT WHOLE TO SEARCH FOR PROG TAGS.
    (thpush *tree* (list 'thprog a nil a))
    ;; CALL WORKHORSE
    (thproga))

(defn- thproga []
    (let [x (cdar *tree*)]
        ;; ODD CASE WHERE THE THPROG HAS NO SUBEXPRESSIONS.  RETURN SUCCESS.
        (cond (nil? (cdar x)) (do (thpopt) 'thnoval)
            ;; NEXT ITEM IS AN ATOM, HENCE A THPROG TAG.
            (term? (cadar x))
                ;; USE THEXP TO MARK IT ON THTREE.
                (do (set! *expr* (list 'thtag (cadar x)))
                    ;; MOVE POINTER TO NEXT EXPRESSION.
                    (RPLACA x (cdar x))
                    *value*)
            ;; OTHERWISE NEXT EXPRESSION TO BE EVALUATED IS THE NEXT EXPRESSION OF THE THPROG.
            :else
                (do (set! *expr* (cadar x))
                    ;; MOVE POINTER TO NEXT EXPRESSION.
                    (RPLACA x (cdar x))
                    *value*))))

;; THBRANCH AND THBRANCHUN ARE THE MAIN FUNCTIONS IN CHARGE OF HANDLING THE EFFECTS OF SUCCESS AND FAILURE.
;; THEY ARE ONLY CALLED BY THPROGT AND THPROGF.

(defn- thprogf [] (thbranchun) nil)

(defn- thprogt [] (thbranch) (thproga))

;; MAKE SURE THAT THE PATTERN HAS NO UNASSIGNED VARIABLES IN IT.  X, NATURALLY ENOUGH, IS THE PATTERN.
;; SINCE THPURE IS ALWAYS CALLED AFTER THVARSUBST, ANY VARIABLES WHICH DO HAVE ASSIGNMENTS WILL HAVE GONE AWAY,
;; REPLACED BY THEIR ASSIGNMENTS, SO ALL WE NEED TO DO IS LOOKING FOR ANY VARIABLES APPEARING AT ALL.

(defn- thpure [x] (not-any? thvar? x))

(defn- thputprop [ato ind val]
    (thpush *tree* (list 'THMUNG (list (list 'putprop! (quotify ato) (quotify ind) (quotify (getprop ato ind))))))
    (putprop! ato ind val))

(defn- assq- [x a ?]
    ;; JUST LIKE ASSQ IN LISP, ONLY RETURN WITH THE POINTER 1 ELEMENT PRIOR TO THE ONE ASKED FOR.
    (loop [a a]
        (if (= (if ? (cadr a) (caadr a)) x) a (let [a (cdr a)] (when (cdr a) (recur a))))))

(defn- threm1 [x]
    ;; THREM1 IS ROUGHLY THE SAME AS THIP, BUT FOR REMOVING ASSERTIONS FROM THE DATABASE,
    ;; HENCE ALL COMMENTS WILL BE GUIDES TO THE CORRESPONDENCE BETWEEN THREM1 AND THIP.
    ;; THB = THI IN THIP.
    (set! *thnf* (inc *thnf*))
    ;; THA AND THA1 DO THE WORK OF THT1 IN THIP.
    ;; THA1 = THT2,
    ;; THA3 = THT3,
    ;; THA4, THA5, THONE, AND THPC ARE NEW.
    (let-when [[x _]
            (cond (and (term? x) (not (= x '?)) (not (number? x))) [x nil]
                (or (= x '?) (thvar? x)) (if *thfst* [nil 'THVRB] ['THVRB nil])
                :else [nil 'THVRB])
    ] (nil? _) => _
        ;; ALL THE REST SERVES THE SAME PURPOSE AS THE SECOND COND IN THIP.
        ;; IT WAS ORIGINALLY WRITTEN AS A SINGLE COND, BUT THE COMPILER BARFED ON IT,
        ;; SO IT WAS BROKEN UP INTO BITE SIZE PIECES.
        (let-when [a1 (getprop x *thwh*)] a1
            (if (= a1 'thnohash) 'THBQF
                (let-when [a2 (assq- *thnf* a1 nil)] a2
                    (let-when [a3 (assq- *thal* (cadr a2) nil)] a3
                        (let-when [? (not= *thwh* 'thassertion) a4 (cadr a3)
                              a5 (if (or *thfst* *thfstp*) (assq- *thbs* (cdr a4) ?) (assq- (if ? *thon* (car *thon*)) (cdr a4) ?))] a5
                            (let [thone (cadr a5)]
                                (RPLACD a5 (cddr a5))
                                (if-not (== (cadr a4) 1)
                                    (do (RPLACA (cdr a4) (dec (cadr a4)))
                                        thone)
                                    (do (RPLACD a3 (cddr a3))
                                        (when-not (cdadr a2)
                                            (RPLACD a2 (cddr a2))
                                            (when-not (cdr a1)
                                                (remprop! x *thwh*)))
                                        thone))))))))))

(defn- thrembindf [] (set! *thalist* (cadar *tree*)) (thpopt) nil)

(defn- thrembindt [] (set! *thalist* (cadar *tree*)) (thpopt) *value*)

(defn- thremove [thb]
    ;; THREMOVE IS ANALOGOUS TO THADD EXCEPT IT REMOVES RATHER THAN ADDS.
    ;; AS WITH THREM1, ALL COMMENTS WILL EXPLICATE THE ANALOGY, SO ONE SHOULD FIRST BECOME FAMILIAR WITH THADD.
    ;; THB = THTT IN THADD.
    ;; THREMOVE TAKES ONLY ONE ARG SINCE THE PROPERTY LIST FOR THE ASSERTION PLAYS NO ROLE IN REMOVING THE ASSERTION.
    ;; THB1 AND THON TOGETHER SHARE THE WORK OF THT1 AND THCK IN THADD.  THAL = THLAS.  THBS = THTTL.
    (binding [*thwh* nil *thnf* nil *thal* nil *thon* nil *thbs* nil *thfst* nil *thfstp* nil]
        ;; THE STRUCTURE OF THE TWO PROGRAMS IS VIRTUALLY IDENTICAL.
        (let [a (if (term? thb)
                    (let [x (getprop thb 'THEOREM)]
                        (set! *thbs* thb)
                        (set! *thwh* (car x))
                        (caddr x))
                    (do (set! *thwh* 'thassertion)
                        (set! *thbs* thb)))]
            (set! *thnf* 0)
            (set! *thal* (count a))
            (set! *thfst* true)
            (loop [a' nil a a]
                (cond (nil? a)
                        (do (set! *thnf* 0)
                            (set! *thfst* nil)
                            (set! *thfstp* true)
                            (recur nil a'))
                    (nil? (set! *thon* (threm1 (car a))))
                        nil
                    (memq *thon* '(THBQF THVRB))
                        (recur (concat a' (list (when (= *thon* 'THVRB) (car a)))) (cdr a))
                    :else
                        (do (set! *thfst* nil)
                            (dorun (map threm1 (cdr a)))
                            (set! *thnf* 0)
                            (dorun (map threm1 a'))
                            *thon*))))))

(defn- thremprop [ato ind]
    (thpush *tree* (list 'THMUNG (list (list 'putprop! (quotify ato) (quotify ind) (quotify (getprop ato ind))))))
    (remprop! ato ind))

(defq- threstrict [& a]
    (let [x (thgal (car a) *thalist*)]
        (if (term? x)
            (do (terpri) (print- "THRESTRICT IGNORED - CONTINUING"))
            (thrplacd (cdr x) (thunion (cddr x) (cdr a))))
        x))

(defq- threturn [& a]
    (apply thsucceed (cons 'thprog a)))

(defn- thrplaca [x y]
    (binding [*thml* nil]
        (thrplacas x y)
        (thpush *tree* (list 'THMUNG *thml*))
        x))

(defn- thrplacas [x y]
    (thpush *thml* (list 'thurplaca x (car x)))
    (RPLACA x y))

(defq- thurplaca [& a] (RPLACA (car a) (cadr a)))

(defn- thrplacd [x y]
    (binding [*thml* nil]
        (thrplacds x y)
        (thpush *tree* (list 'THMUNG *thml*))
        x))

(defn- thrplacds [x y]
    (thpush *thml* (list 'thurplacd x (cdr x)))
    (RPLACD x y))

(defq- thurplacd [& a] (RPLACD (car a) (cadr a)))

(defq- thsetq [& a']
    (binding [*thml* nil]
        (loop [a a']
            (cond
                (nil? a)
                    (do (thpush *tree* (list 'THMUNG *thml*))
                        *value*)
                (nil? (cdr a))
                    (bug! 'thsetq "ODD NUMBER OF GOODIES" a')
                (term? (car a))
                    (do (thpush *thml* (list 'SETQ (car a) (quotify (eval (car a)))))
                        (SET (car a) (set! *value* (eval (cadr a))))
                        (recur (cddr a)))
                :else
                    (do (thrplacas (cdr (thsgal (car a)))
                        (set! *value* (thval (cadr a) *thalist*)))
                        (recur (cddr a)))))))

(defn- thsgal [x]
    (or (assq (cadr x) *thalist*) (let [y (list (cadr x) 'thunassigned)] (set! *thalist* (conj *thalist* y)) y)))

(defn- thstate [& a]
    ;; PRINTS THAT PART OF THE STATE OF THE MICRO-PLANNER WORLD SPECIFIED BY THE INDICATORS IN REREADABLE FORM.
    ;; NOTE THAT IT IS BLIND TO ASSERTIONS THAT BEGIN WITH EITHER NUMBERS, LIST STRUCTURE, NOHASHED ATOMS OR NON-INTERNED ATOMS.
    (terpri)
    (pr '(THDATA))
    (dorun (map (lambda [c]
        (dorun (map (lambda [d]
            (dorun (map (lambda [e]
                (let-when [b (getprop d e) b (and b (assq 1 (cdr b)))] b
                    (dorun (map (lambda [f]
                        (dorun (map (lambda [g]
                                (terpri)
                                (if (= e 'thassertion) (pr g) (pr (list g))))
                            (cddr f))))
                        (cdr b)))))
                (or a '(thassertion thconse thante therasing)))))
            c)))
        (§ MAKOBLIST nil)))
    (terpri)
    nil)

(defq- thsucceed [& a]
    (when a
        (let [a (if (= (car a) 'THEOREM) (cons 'thprog (cdr a)) a)]
            (set! *branch* *tree*)
            (set! *thabranch* *thalist*)
            (loop-when [] *tree* => (bug! 'thsucceed "OVERPOP" a)
                (condp = (caar *tree*)
                    'THREMBIND
                        (do (set! *thalist* (cadar *tree*)) (thpopt) (recur))
                    (car a)
                        (do (thpopt) (if (cdr a) (eval (cadr a)) 'thnoval))
                    (when' (and (= (car a) 'thtag) (= (caar *tree*) 'thprog)) => (do (thpopt) (recur))
                        (let-when [x (memq (cadr a) (cadddr (car *tree*)))] x => (do (thpopt) (recur))
                            (RPLACA (cdar *tree*) (cons nil x))
                            (thprogt))))))))

(defn- thtae [a thx type]
    (cond (term? a) nil
        (= (car a) 'THUSE)
            (doall (map (lambda [x] (set! *thxx* (getprop x 'THEOREM))
                    (if (and *thxx* (= (car *thxx*) type))
                        (list 'thapply x (car thx))
                        (bug! 'thtae "BAD THEOREM" x)))
                (cdr a)))
        (= (car a) 'THTBF)
            (doall (mapcat #(when (apply (cadr a) %) (list (list 'thapply % (car thx))))
                (if *thy1* *thy* (do (set! *thy1* true) (set! *thy* (thmatchlist (car thx) type))))))
        :else (bug! 'thtae "UNCLEAR RECOMMENDATION" a)))

(defq- thtag [& a]
    (when (car a)
        (thpush *tree* (list 'thtag (car a)))))

(defn- thtagf [] (thpopt) nil)

(defn- thtagt [] (thpopt) *value*)

(defn- thtrue [_] true)

(defn- thtry1 []
    ;; TRIES NEXT RECOMMENDATION ON TREE FOR THGOAL.
    ;; THZ = (THGOAL PATTERN EXPANDED-RECOMMENDATIONS)
    ;; THY = RECOMMENDATIONS
    (let [z (car *tree*) y (cddr z)]
        (RPLACD y (dec (cdr y)))
        (loop-when [] (and (car y) (non-zero? (cdr y)))
            (let [x (caar y)]
                (condp = (car x)
                    'THNUM (do (RPLACD y (cadr x)) (RPLACA y (cdar y)) (recur))
                    'THDBF (loop [] (set! *tholist* *thalist*)
                            (when' (caddr x) => (do (RPLACA y (cdar y)) (recur))
                                (let-when [w (caaddr x)] (let [_ (and ((cadr x) w) (thmatch1 (cadr z) (car w)))] (RPLACA (cddr x) (cdaddr x)) _) => (recur)
                                    w)))
                    'THTBF (loop []
                            (when' (caddr x) => (do (RPLACA y (cdar y)) (recur))
                                (let-when [t (caaddr x) w (getprop t 'THEOREM)] (and w (= (car w) 'thconse)) => (bug! 'thtry1 "BAD THEOREM" t)
                                    (when' (let [_ (and ((cadr x) (caaddr x)) (thapply1 t w (cadr z)))] (RPLACA (cddr x) (cdaddr x)) _) => (recur)
                                        true)))))))))

(defn- thtry [x a1]
    ;; THTRY IS IN CHARGE OF MAKING UP THE "THINGS TO DO" LIST, WHICH IS PUT ON THTREE.
    ;; SO WHENEVER WE FAIL BACK TO A THGOAL, WE GO TO THE NEXT "THING TO DO".
    ;; X IS THE LIST OF RECOMMENDATIONS.
    ;; ANY ATOMIC RECOMMENDATION IS IGNORED.  THIS IS USEFUL IN ERROR RECOVERY.
    (when-not (term? x)
        (condp = (car x)
            ;; HAVE A THEOREM BASE FILTER.
            'THTBF
                ;; MAKE UP A LIST WHICH GIVES
                ;; 1 - THE INDICATOR "THTBF"
                ;; 2 - THE ACTUAL FILTER (THTRUE IS THE MOST COMMON)
                ;; 3 - THE BUCKET RETURNED BY THMATCHLIST
                (do (when (not *thz1*) (set! *thz1* true) (set! *thz* (thmatchlist a1 'thconse)))
                    (when *thz* (list (list 'THTBF (cadr x) *thz*))))
            ;; DO THE SAME THING, ONLY FOR DATABASE FILTERS.
            'THDBF
                (do (when (not *thy1*) (set! *thy1* true) (set! *thy* (thmatchlist a1 'thassertion)))
                    (when *thy* (list (list 'THDBF (cadr x) *thy*))))
            ;; THUSE STATEMENTS ARE TRANSLATED INTO THTBF THTRUE STATEMENTS,
            ;; WHICH THE "BUCKET" IS THE LIST GIVEN IN THE THUSE.
            'THUSE
                (list (list 'THTBF 'thtrue (cdr x)))
            'THNUM
                (list x)
            (bug! 'thtry "UNCLEAR RECOMMENDATION" x))))

(defn- thundof []
    (when' (caddar *tree*) => (thpopt)
        (set! *thxx* (cddar *tree*))
        (set! *thalist* (caadr *thxx*))
        (RPLACA (cdr *thxx*) (cdadr *thxx*))
        (set! *tree* (caar *thxx*))
        (RPLACA *thxx* (cdar *thxx*)))
    nil)

(defn- thundot [] (thpopt) true)

(defn- thunique [& a]
    (let [a (cons 'thunique a)]
        (loop-when [x *thalist*] x => (do (thpush *thalist* a) true)
            (when-not (and (= (caar x) 'thunique) (= (car x) a))
                (recur (cdr x))))))

(defn- thv1 [x]
    ;; (THV1 'X) IS THE VALUE OF THE PLANNER VARIABLE.
    ;; ($? X) RETURNS ERROR MESSAGE IF X UNBOUND OR UNASSIGNED.
    (set! *thxx* x)
    (let [v (cadr (or (assq x *thalist*) (bug! 'thv1 "THUNBOUND" x)))]
        (if (= v 'thunassigned) (bug! 'thv1 "THUNASSIGNED" x) v)))

(defq- thv [& a]
    ;; (THV X) IS THE VALUE OF THE PLANNER VARIABLE ($? X)
    (thv1 (car a)))

(defq- thnv [& a]
    (thv1 (car a)))

(defn- thval [expr' thalist']
    ;; CORRESPONDS TO LISP EVAL.
    ;; THEXP IS THE EXPRESSION TO BE THVALUATED.  THALIST IS THE VARIABLE BINDING LIST.
    (binding [*expr* expr' *thalist* thalist' *tree* nil *value* 'thnoval *branch* nil *tholist* nil *thabranch* nil]
        ;; E BECOMES THE CURRENT EXPRESSION.
        ;; THEXP IS RESERVED FOR FURTHER EXPRESSIONS, WHICH SHOULD BE THVALED BEFORE WE GO TO THE NEXT ITEM OF ACTUAL CODE.
        ;; FOR EXAMPLE, THASSERT USES THIS FEATURE TO PROCESS ANTECEDENT THEOREMS.
        (loop []
            ;; EVAL THE CURRENT EXPRESSION TO BE THVALED.
            ;; NOTE THAT EACH PLANNER FUNCTION CORRESPONDS TO THREE LISP FUNCTIONS:
            ;; ONE TO SET THINGS UP (THIS IS WHAT IS GETTING EVALED AT THIS POINT), ONE TO HANDLE SUCCESS AND ONE FOR FAILURE.
            (let [e *expr*] (set! *expr* nil) (set! *value* (eval e))
                (let [? (loop []
                            (let-when [? (cond
                                        ;; IF THEXP IS NON NIL, PLANNER HAS MORE TO WORK ON BEFORE GOING TO NEXT LINE OF USER CODE.
                                        *expr* :loop
                                        ;; IF THVALUE IS NON NIL, IT MEANS THAT SO FAR THE THEOREM IS SUCCEEDING.
                                        *value*
                                            (do ;; SAVE CURRENT STATE OF THTREE AND THALIST IN CASE WE HAVE TO BACK UP.
                                                (when-not *branch* (set! *branch* *tree*) (set! *thabranch* *thalist*))
                                                ;; IF THTREE IS NIL, THERE ARE NO MORE EXPRESSIONS TO PROCESS.
                                                ;; ALL THEOREMS ACT LIKE A THPROG, INCLUDING PUTTING ITS MARK ON THTREE, SEE THAPPLY,
                                                ;; HENCE NO NEED TO GROW MORE BRANCHES ON THTREE.
                                                (cond (nil? *tree*) :succeed
                                                    ;; THIS IS THE NORMAL CASE.
                                                    ;; WE EVAL THE SUCCEED-FUNCTION OF THE PLANNER FUNCTION WHICH JUST SUCCEEDED.
                                                    (set! *expr* (getprop (caar *tree*) 'thsucceed)) nil
                                                    :else (bug! 'thval "MISSING THSUCCEED")))
                                        :else ;; WE ARE IN A FAILURE SITUATION.
                                            ;; IF THTREE IS NIL, WE HAVE FAILED THE ENTIRE EXPRESSION.
                                            (cond (nil? *tree*) :fail
                                                ;; NORMAL CASE.
                                                ;; EVAL THE FAILURE FUNCTION ASSOCIATED WITH THE PLANNER FUNCTION WHICH JUST FAILED.
                                                (set! *expr* (getprop (caar *tree*) 'thfail)) nil
                                                :else (bug! 'thval "MISSING THFAIL")))
                            ] (not ?) => ?
                                ;; THEXP AT THIS POINT IS THE APPROPRIATE SUCCESS OR FAILURE ASSOCIATED FUNCTION.
                                ;; EVAL IT AND AT THE SAME TIME,
                                ;; SET IT TO NIL IN CASE WE NEED THEXP FOR MORE EXPRESSIONS TO BE PROCESSED.
                                (let [f *expr*] (set! *expr* nil) (set! *value* (f))
                                    ;; GO THROUGH THE ENTIRE PROCESS AGAIN
                                    ;; REMOVING EXPRESSIONS FROM THTREE UNTIL WE GET BACK TO THE ENTRY PUT ON BY THPROG.
                                    ;; AT THIS POINT IT EVALS THPROGT, AND SEE THAT LISTING.
                                    (recur))
                            ))]
                    (condp = ? :loop (recur) :succeed *value* :fail nil))))))

(defn- thvar? [x]
    ;; IS X A PLANNER VARIABLE?
    (memq (car x) '(thv thnv)))

(defn- thvars2 [x y]
    ;; THIS IS THE WORKHORSE FOR THVARSUBST.
    ;; X IS A SINGLE ITEM FROM A PATTERN.
    ;; IF IT'S AN ATOM, NOTHING NEED BE DONE.
    (if (term? x) x
        ;; IF THE EXPRESSION HAS A $E BEFORE IT, THVAL BEFORE GOING ON.
        (let [x (if (= (car x) 'thev) (thval (cadr x) *thalist*) x)]
            ;; IF THE ITEM IS NOT A VARIABLE, IT MUST BE SOME RANDOM LIST, SO IT HAS NO ASSIGNED VALUE.
            (if-not (thvar? x) x
                ;; AT THIS POINT X MUST BE A VARIABLE, SO FIND ITS ASSIGNMENT, THAT'S WHAT THGAL DOES.
                ;; THALIST IS WHERE THE VARIABLE ASSIGNMENTS RESIDE.
                (let [a (thgal x *thalist*)]
                    ;; IF THE VARIABLE IS UNASSIGNED, THEN RETURN THE ACTUAL VARIABLE.
                    (cond (= (cadr a) 'thunassigned) x
                        ;; THY WILL BE T JUST IN THE CASES WHERE THVARSUBST WAS CALLED BY A THGOAL SITUATION.
                        ;; IT IS THEN NECESSARY TO IMMEDIATELY HACK IN A THUNASSIGNED SO THAT IF THE SAME VARIABLE IS USED
                        ;; TWICE IN THE SAME PATTERN, WE WON'T PUT IN ITS OLD VALUE THE SECOND TIME IT IS ENCOUNTERED.
                        (and y (= (car x) 'thnv)) (do (thrplaca (cdr a) 'thunassigned) x)
                        ;; OTHERWISE THE ASSIGNMENT IS THE SECOND ELEMENT IN THE BINDING LIST.
                        :else (cadr a)))))))

(defn- thvarsubst [x y]
    ;; THX IS A GOAL OR ASSERTION PATTERN OR THEOREM NAME.
    ;; THIS FUNCTION RETURNS THE SAME PATTERN, EXCEPT IN PLACE OF ALL ASSIGNED VARIABLES
    ;; THERE WILL BE THE VALUES THEY ARE ASSIGNED TO.
    ;; IF THE CAR IS THEV, IT MEANS THAT THERE WAS A $E BEFORE THE PATTERN,
    ;; IN WHICH CASE WE ARE TO GET THE REAL PATTERN BY THVALUATING WHAT IS THERE.
    ;; IF THE PATTERN IS A SINGLE VARIABLE, THE PROGRAM ASSUMES THERE SHOULD BE AN IMPLICIT THVAL.
    (let [x (cond (= (car x) 'thev) (thval (cadr x) *thalist*) (thvar? x) (eval x) :else x)]
        ;; UNLESS THE ASSERTEE IS A THEOREM NAME, GO THROUGH IT PLACE BY PLACE WITH THVARS2.
        (if (term? x) x (doall (map #(thvars2 % y) x)))))

(defq- thvsetq [& a']
    (loop-when [a a'] a => *value*
        (if (cdr a)
            (set! *value* (car (RPLACA (cdr (thsgal (car a))) (thval (cadr a) *thalist*))))
            (bug! 'thvsetq "ODD NUMBER OF GOODIES" a'))
        (recur (cddr a))))

(putprop! 'thtag 'thfail thtagf)
(putprop! 'thgoal 'thfail thgoalf)
(putprop! 'thamong 'thfail thamongf)
(putprop! 'thfind 'thfail thfindf)
(putprop! 'thand 'thfail thandf)
(putprop! 'thprog 'thfail thprogf)
(putprop! 'THMUNG 'thfail thmungf)
(putprop! 'thassert 'thfail thassertf)
(putprop! 'therase 'thfail therasef)
(putprop! 'thcond 'thfail thcondf)
(putprop! 'thor 'thfail thorf)
(putprop! 'thdo 'thfail thdob)
(putprop! 'THUNDO 'thfail thundof)
(putprop! 'THREMBIND 'thfail thrembindf)

(putprop! 'thtag 'thsucceed thtagt)
(putprop! 'thgoal 'thsucceed thgoalt)

(putprop! 'thfind 'thsucceed thfindt)
(putprop! 'thand 'thsucceed thandt)
(putprop! 'thprog 'thsucceed thprogt)
(putprop! 'THMUNG 'thsucceed thmungt)
(putprop! 'thassert 'thsucceed thassertt)
(putprop! 'therase 'thsucceed theraset)
(putprop! 'thcond 'thsucceed thcondt)
(putprop! 'thor 'thsucceed thort)
(putprop! 'thdo 'thsucceed thdob)
(putprop! 'THUNDO 'thsucceed thundot)
(putprop! 'THREMBIND 'thsucceed thrembindt)

#_(ns shrdlu.blockp)

;; ################################################################
;;
;;     BLOCKP > MICRO-PLANNER CODE FOR THE "BLOCKS" MICRO-WORLD
;;
;; ################################################################

(putprop! 'TA-AT 'THEOREM
    '(thante (x y) (!AT ($? x) ($? y)) (thrplaca (cdr (atab ($? x))) ($? y))))

(putprop! 'TA-CONTAIN 'THEOREM
    '(thante (x y z)
        (!AT ($? x) ?)
        (thgoal (!MANIP ($? x)))
        (thgoal (!SUPPORT ($? y) ($? x)))
        (thor (thand (thgoal (!IS ($? y) !BOX)) (thvsetq ($! z) ($? y)))
            (thgoal (!CONTAIN ($? z) ($? y))))
        (thassert (!CONTAIN ($? z) ($? x)))))

(putprop! 'TA-EXISTS 'THEOREM
    '(thante (x) (!EXISTS ($? x)) (thsucceed)))

(def- nostacks? true)

(putprop! 'TA-SUPP 'THEOREM
    '(thante (x y z)
        (!AT ($? x) ($? y))
        (thcond ((thvsetq ($! z) (support ($? y) (size ($? x)) ($? x)))
            (thcond ((thgoal (!MANIP ($? z)))
                (thgoal (!SHAPE ($? z) !RECTANGULAR)))
                ((thsucceed)))
            (thassert (!SUPPORT ($? z) ($? x)))
            (thcond ((thgoal (!CLEARTOP ($? z)))
                (therase (!CLEARTOP ($? z))))
                ((thsucceed)))
            (thcond (nostacks?)
                ((thnot (thgoal (!MANIP ($? z)))))
                ((thand (thgoal (!PART ($? z) ($! y)))
                    (thgoal (!IS ($? y) !STACK)))
                (thassert (!PART ($? x) ($? y))))
                ((thvsetq ($! y) (gensym 'STACK))
                (thassert (!PART ($? x) ($? y)))
                (thassert (!PART ($? z) ($? y)))
                (thassert (!IS ($? y) !STACK))
                (thassert (!EXISTS ($? y)) (THUSE TA-EXISTS)))))
            ((thgoal (!GRASPING ($? x))))
            ((bug! 'ta-supp nil)))))

(putprop! 'TC-2 'THEOREM
    '(thconse (x y yy)
        (($? x) ($? y))
        (thgoal (!CHOOSE ($? y) ($! yy) ($E (getprop ($? x) 'CHOOSE)))
            (THUSE TC-CHOOSE))
        (thgoal (($? x) ($? yy)) (THTBF thtrue))))

(putprop! 'TC-3 'THEOREM
    '(thconse (x y z yy zz)
        (($? x) ($? y) ($? z))
        (thgoal (!CHOOSE ($? y) ($! yy) ($E (getprop ($? x) 'CHOOSE)))
            (THUSE TC-CHOOSE))
        (thgoal (!CHOOSE ($? z) ($! zz) ($E (getprop ($? x) 'CHOOSE2)))
            (THUSE TC-CHOOSE))
        (thgoal (($? x) ($? yy) ($? zz)) (THTBF thtrue))))

(putprop! 'TC-ASMUCH 'THEOREM
    '(thconse (measure x y)
        (!ASMUCH measure ($? x) ($? y))
        (thvsetq ($! measure) (getprop ($? measure) 'MEASFN))
        (not (< (($? measure) ($? x)) (($? measure) ($? y))))))

(putprop! 'TC-BELONG 'THEOREM
    '(thconse (x y)
        (!BELONG ($? x) ($? y))
        (thamong ($? y) '(ßSHRDLU))
        (thgoal (!PHYSOB ($? x)) (THUSE TC-PHYSOB))))

(putprop! 'TC-CALL 'THEOREM
    '(thconse (x y z)
        (!CALL ($? x) ($? y))
        (thcond ((thgoal (!CALL ($! z) ($? y)))
                (terpri)
                (pr ($? y))
                (-print "NAME ALREADY USED")
                nil)
            ((thassert (!CALL ($? x) ($? y)))
                (thassert (!IS ($? y) !NAME))
                (!propdefine ($? y))
                true))))

(putprop! 'TC-CHOOSE 'THEOREM
    '(thconse (x y z w)
        (!CHOOSE ($? x) ($? y) ($? z))
        (thcond
            ((and (thasval ($? x)) (not (oss? ($? x)))) (thsetq ($! y) ($? x)))
            ((thasval ($? x))
                (or (not discourse?)
                    (thputprop (variable? ($? x)) 'NG ($? x)))
                (thsetq ($! y) (findchoose ($? x) ($? z) nil)))
        ((thgoal (!MANIP ($? y))) (thnot (thgoal (!SUPPORT ($? y) ?)))))))

(putprop! 'TC-CHOOSEPLACE 'THEOREM
    '(thconse (x) (!CHOOSEPLACE ($? x)) (bug! 'tc-chooseplace nil)))

(putprop! 'TC-CLEARTOP 'THEOREM
    '(thconse (x y (why ($? ev)) ev)
        (!CLEARTOP ($? x))
        (term? ($? x))
        (thor (thgoal (!SUPPORT ($? x) ?))
        (thand (thassert (!CLEARTOP ($? x))) (thsucceed THEOREM)))
        (memory ($? why))
    =>  (thcond ((thgoal (!SUPPORT ($? x) ($! y)))
                (thgoal (!GET-RID-OF ($? y)) (THNODB) (THUSE TC-GET-RID-OF))
                (thgo =>))
            ((thassert (!CLEARTOP ($? x)))
                (memorend ($? ev) '(!CLEARTOP ($? ev) ($? x)))
                (thsucceed THEOREM)))))

(putprop! 'TC-EXISTS 'THEOREM
    '(thconse (x) (!EXISTS ($? x)) (thsucceed)))

(putprop! 'TC-FINDSPACE 'THEOREM
    '(thconse (surf size obj space)
        (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space))
        (thor (and (not (memq ($? surf) '(ßBOX ßTABLE))) (not (getprop '!NOCLEAR 'thassertion))
            (thsetq ($! space) (findspace 'CENTER ($? surf) ($? size) ($? obj))))
        (and (or (= ($? surf) 'ßBOX) (and (not (= ($? surf) 'ßTABLE)) (getprop '!NOCLEAR 'thassertion)))
            (thsetq ($! space) (findspace 'PACK ($? surf) ($? size) ($? obj))))
        (thsetq ($! space) (findspace 'RANDOM ($? surf) ($? size) ($? obj))))))

(putprop! 'TC-GET-RID-OF 'THEOREM
    '(thconse (x y (why ($? ev)) ev)
        (!GET-RID-OF ($? x))
        (thvsetq ($! ev) ($? why))
    =>  (thcond ((nil? ($? x)))
            ((term? ($? x))
                (memory ($? why))
                (thgoal (!FINDSPACE ßTABLE ($E (size ($? x))) ($? x) ($! y)) (THUSE TC-FINDSPACE))
                (thgoal (!PUT ($? x) ($? y)) (THNODB) (THUSE TC-PUT))
                (memorend ($? ev) '(!GET-RID-OF ($? ev) ($? x))))
            ((thgoal (!GET-RID-OF ($E (car ($? x)))) (THUSE TC-GET-RID-OF))
                (or (thsetq ($! x) (cdr ($? x))) (thsucceed THEOREM))
                (thgo =>)))))

(putprop! 'TC-GRASP 'THEOREM
    '(thconse (x y (why ($? ev)) ev)
        (!GRASP ($? x))
        (thcond ((thgoal (!GRASPING ($? x))) (thsucceed THEOREM))
            ((term? ($? x))))
        (memory ($? why))
        (thgoal (!CLEARTOP ($? x)) (THUSE TC-CLEARTOP))
        (thcond ((thgoal (!GRASPING ($! y)))
            (thor (thgoal (!UNGRASP) (THNODB) (THUSE TC-UNGRASP))
                (thgoal (!GET-RID-OF ($? y)) (THNODB) (THUSE TC-GET-RID-OF))))
            ((thsucceed)))
        (thsetq ($! y) (topcenter ($? x)))
        (thgoal (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2))
        (thassert (!GRASPING ($? x)))
        (memorend ($? ev) '(!GRASP ($? ev) ($? x)))
        (thsetq *grasplist* (cons (list *thtime* ($? x)) *grasplist*))
        (thor (GRASP ($? x)) (and (UNGRASP) nil))))

(putprop! 'TC-LOC 'THEOREM
    '(thconse (x y z loc)
        (($? loc) ($? x) ($? y) ($? z))
        (thor (thgoal (!MANIP ($? y))) (thgoal (!IS ($? y) !BOX)))
        (thor (thgoal (!MANIP ($? z))) (thgoal (!IS ($? z) !BOX)))
        (not= ($? y) ($? z))
        (locgreater LOC ($? y) ($? z)
            (condp = ($? x) '!RIGHT car '!BEHIND cadr '!ABOVE caddr (bug! 'tc-loc nil)))))

(putprop! 'TC-MAKESPACE 'THEOREM
    '(thconse (surf size obj space x (why ($? ev)) ev)
        (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space))
        (thnot (thgoal (!IS ($? surf) !BOX)))
        (memory ($? why))
    => (thand (thgoal (!SUPPORT ($? surf) ($! x)))
            (thgoal (!GET-RID-OF ($? x)) (THUSE TC-GET-RID-OF)))
        (thor (thgoal (!FINDSPACE ($? surf) ($? size) ($? obj) ($? space)) (THUSE TC-FINDSPACE))
            (thgo =>))
        (memorend ($? ev) '(!MAKESPACE ($? ev) ($? surf)))))

(putprop! 'TC-MORE 'THEOREM
    '(thconse (measure x y)
        (!MORE ($? measure) ($? x) ($? y))
        (thvsetq ($! measure) (getprop ($? measure) 'MEASFN))
        (> (($? measure) ($? x)) (($? measure) ($? y)))))

(putprop! 'TC-MOVEHAND 'THEOREM
    '(thconse (x y w z)
        (!MOVEHAND ($? y))
        (thcond
            ((= ($? y) *handat*) (thsucceed THEOREM))
            ((thgoal (!GRASPING ($? x)))
                (thvsetq ($! z) (let [x (atab ($? x)) y (diff ($? y) (tcent '(0 0 0) (caddr x)))]
                    (when (clear y (list (caaddr x) (cadadr (cdr x)) (- 512 (caddr y))) (car x)) y)))
                (thgoal (!AT ($? x) ($! w)))
                (therase (!AT ($? x) ($? w)) (THUSE TE-SUPP TE-CONTAIN))
                (thassert (!AT ($? x) ($? z)) (THUSE TA-AT TA-SUPP TA-CONTAIN))
                (thgoal (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2))
                (thputprop ($? x) 'HISTORY
                    (cons (list *thtime* ($? z) (cadar (or (thval '(thgoal (!SUPPORT ($? y) ($? x))) (cons (list 'y 'thunassigned) *thalist*)) '((nil ßHAND)))) nil)
                        (getprop ($? x) 'HISTORY))))
        ((thgoal (!MOVEHAND2 ($? y)) (THNODB) (THUSE TC-MOVEHAND2))))))

(putprop! 'TC-MOVEHAND2 'THEOREM
    '(thconse (y loc)
        (!MOVEHAND2 ($? y))
        (if (= ($? y) *handat*) (thsucceed THEOREM)
            (and (<= 32 (car ($? y)) 608) (<= 0 (cadr ($? y)) 608) (<= 0 (caddr ($? y)) 512)))
        (thvsetq ($! loc) *handat*)
        (thsetq *handat* ($? y))
        (thsetq *thtime* (inc *thtime*))
        (thor (eval (cons 'MOVETO *handat*)) (and (eval (cons 'MOVETO ($? loc))) nil))))

(putprop! 'TC-NAME 'THEOREM
    '(thconse (x)
        (!NAME ($? x))
        (thvsetq ($! x) (listify ($? x)))
        (thvsetq ($! x) (thfind ALL ($? y) (y z) (thamong ($? z) ($? x)) (thor (thgoal (!CALL ($? z) ($? y))) (thsetq ($! y) ($? z)))))))

(putprop! 'TC-NOTICE 'THEOREM
    '(thconse (x)
        (!NOTICE ($? x))
        (or (BLINK ($? x)) (thsucceed))))

(putprop! 'TC-ON 'THEOREM
    '(thconse (x y z)
        (!ON ($? x) ($? y))
        (thor (thgoal (!SUPPORT ($? y) ($? x))) (thand (thasval ($? x)) (thgoal (!SUPPORT ($! z) ($? x))) (thgoal (!ON ($? z) ($? y)) (THUSE TC-ON))))))

(putprop! 'TC-PACK 'THEOREM
    '(thconse (obj surf blocks pyr x y)
        (!PACK ($? obj) ($? surf))
        (or (thvsetq ($! blocks) (packo ($? obj) '!BLOCK)) true)
        (or (thvsetq ($! pyr) (packo ($? obj) '!PYRAMID)) true)
    =>  (thcond ((nil? ($? blocks))
            (thcond ((nil? ($? pyr)) (thsucceed THEOREM))
                ((thvsetq ($! y) (findspace 'PACK ($? surf) (size (car ($? pyr))) (car ($? pyr))))
                    (thgoal (!PUT ($E (car ($? pyr))) ($? y)) (THUSE TC-PUT))
                    (or (thsetq ($? pyr) (cdr ($? pyr))) true)
                    (thgo =>))))
                ((thsetq ($! x) (car ($? blocks)))
                    (thvsetq ($? y) (findspace 'PACK ($? surf) (size ($? x)) ($? x)))
                    (thgoal (!PUT ($? x) ($? y)) (THUSE TC-PUT))
                    (or (thsetq ($? blocks) (cdr ($? blocks))) true)
                    (thcond ((thvsetq ($! y) (or (packon ($? x) ($? pyr)) (packon ($? x) ($? blocks))))
                            (thgoal (!PUTON ($? y) ($? x)) (THUSE TC-PUTON))
                            (if (memq ($? y) ($? pyr))
                                (thsetq ($! pyr) (delq ($? y) (concat ($? pyr) nil)))
                                (thsetq ($! blocks) (delq ($? y) (concat ($? blocks) nil)))))
                        ((thsucceed)))
                    (thgo =>)))))

(putprop! 'TC-PART 'THEOREM
    '(thconse (x y z)
        (!PART ($? x) ($? y))
        (thgoal (!IS ($? y) !STACK))
        (thgoal (!CHOOSE ($? x) ($! z) '(((thgoal (!PART ($? *) ($? y)))))) (THUSE TC-CHOOSE))
        (or (not (term? ($? z))) (thsetq ($! z) (list ($? z))))
    =>  (thcond ((nil? ($? z)) (thsucceed))
            ((thgoal (!PART ($E (car ($? z))) ($? y)))
                (or (thsetq ($! z) (cdr ($? z))) true)
                (thgo =>))
            ((thfail)))))

(putprop! 'TC-PHYSOB 'THEOREM
    '(thconse (x)
        (!PHYSOB ($? x))
        (thor (thgoal (!MANIP ($? x))) (thamong ($? x) '(ßBOX ßTABLE ßHAND)))))

(putprop! 'TC-PICKUP 'THEOREM
    '(thconse (x (why ($? ev)) ev)
        (!PICKUP ($? x))
        (memory ($? why))
        (thgoal (!GRASP ($? x)) (THUSE TC-GRASP))
        (thgoal (!RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
        (memorend ($? ev) '(!PICKUP ($? ev) ($? x)))))

(putprop! 'TC-REFERS 'THEOREM
    '(thconse (x)
        (!REFERS ($? x))
        (eval (list 'thsetq (list 'thv ($? x)) (quotify (atomify (getprop ($? x) 'BIND)))))))

(putprop! 'TC-PUT 'THEOREM
    '(thconse (x y z)
        (!PUT ($? x) ($? y))
        (thcond ((thasval ($? y))
                (thcond ((term? ($? y)) (thgoal (!CHOOSEPLACE ($? y)) (THUSE TC-CHOOSEPLACE)))
                    ((thsucceed))))
            ((thgoal (!GET-RID-OF ($? x)) (THNODB) (THUSE TC-GET-RID-OF))
                (thsucceed THEOREM)))
        (clear ($? y) (size ($? x)) ($? x))
        (support ($? y) (size ($? x)) ($? x))
        (thgoal (!GRASP ($? x)) (THUSE TC-GRASP))
        (thsetq ($! z) (tcent ($? y) (size ($? x))))
        (thgoal (!MOVEHAND ($? z)) (THNODB) (THUSE TC-MOVEHAND))
        (thgoal (!UNGRASP) (THNODB) (THUSE TC-UNGRASP))))

(putprop! 'TC-PUTIN 'THEOREM
    '(thconse (x y z (why ($? ev)) ev)
        (!PUTIN ($? x) ($? y))
        (memory ($? why))
        (thcond ((thgoal (!PUTON ($? x) ($? y)) (THUSE TC-PUTON))
                (memorend ($? ev) '(!PUTIN ($? ev) ($? x) ($? y)))
                (thsucceed THEOREM))
            ((thsucceed)))
        (thgoal (!IS ($? y) !BOX))
        (thvsetq ($! z)
            (union (listify ($? x))
                (thval '(thfind ALL ($? w) (w) (thgoal (!ON ($? w) ($? y)))) *thalist*)))
        (thgoal (!CLEARTOP ($? y)) (THUSE TC-CLEARTOP))
        (thgoal (!PACK ($? z) ($? y)) (THUSE TC-PACK))
        (memorend ($? ev) '(!PUTIN ($? ev) ($? x) ($? y)))))

(putprop! 'TC-PUTON 'THEOREM
    '(thconse (x y z (why ($? ev)) ev)
        (!PUTON ($? x) ($? y))
        (term? ($? y))
        (or (cdr ($? x)) (thsetq ($! x) (car ($? x))))
        (not (if (term? ($? x)) (= ($? x) ($? y)) (memq ($? y) ($? x))))
        (memory ($? why))
        (thcond ((term? ($? x))
                (thgoal (!CLEARTOP ($? x)) (THUSE TC-CLEARTOP))
                (thor (thgoal (!FINDSPACE ($? y) ($E (size ($? x))) ($? x) ($! z)) (THUSE TC-FINDSPACE))
                    (and (nil? (getprop '!NOCLEAR 'thassertion))
                        (thgoal (!FINDSPACE ($? y) ($E (size ($? x))) ($? x) ($! z)) (THUSE TC-MAKESPACE))))
                (thgoal (!PUT ($? x) ($? z)) (THNODB) (THUSE TC-PUT)))
            ((thassert (!NOCLEAR))
                (thprog ((w ($? x)))
                =>  (thor (thgoal (!PUTON ($E (car ($? w))) ($? y)) (THUSE TC-PUTON))
                        (thfail))
                    (thor (thsetq ($? w) (cdr ($? w))) (threturn true))
                    (thgo =>))
            (therase (!NOCLEAR)))
            ((thnot (thgoal (!IS ($? y) !BOX)))
                (thgoal (!CLEARTOP ($? y)) (THUSE TC-CLEARTOP))
                (thgoal (!PACK ($? x) ($? y)) (THUSE TC-PACK))))
        (memorend ($? ev) '(!PUTON ($? ev) ($? x) ($? y)))))

(putprop! 'TC-RAISEHAND 'THEOREM
    '(thconse ((why ($? ev)) ev)
        (!RAISEHAND)
        (memory ($? why))
        (thgoal (!MOVEHAND ($E (list (car *handat*) (cadr *handat*) 512))) (THNODB) (THUSE TC-MOVEHAND))
        (memorend ($? ev) '(!RAISEHAND ($? ev)))))

(putprop! 'TC-STACK 'THEOREM
    '(thconse (x y)
        (!IS ($? x) !STACK)
        (not (thasval ($? x)))
        (thgoal (!MANIP ($? y)))
        (thgoal (!SUPPORT ($? y) ?))
        (thnot (thand (thgoal (!PART ($? y) ($! x)))
            (thgoal (!IS ($? x) !STACK))))
    =>  (thgoal (!SUPPORT ($! x) ($? y)))
        (thcond ((memq ($? x) '(ßTABLE ßBOX)))
            ((thsetq ($! y) ($? x)) (thgo =>)))
        (thsetq ($! x) (gensym 'STACK))
        (thassert (!IS ($? x) !STACK))
        (thassert (!EXISTS ($? x)))
        (thfind ALL
            ($? z)
            (z)
            (thgoal (!ON ($? z) ($? y)) (THUSE TC-ON))
            (thand (thassert (!PART ($? z) ($? x))) (thfinalize thand)))))

(putprop! 'TC-STACKUP 'THEOREM
    '(thconse (x y blocks pyr (why ($? ev)) ev)
        (!STACKUP ($? x))
        (or (< (reduce + (map #(caddr (size %)) ($? x))) 641)
            (not (DPRINT2 "TOO HIGH,")))
        (thcond
            ((and ($? x) (cdr ($? x))))
            ((thsetq ($! x)
                (concat ($? x)
                    (thval (list 'thfind
                        (if ($? x) 2 3)
                        '($? y)
                        '(y)
                        '(thor (thand (thgoal (!IS ($? y) !BLOCK)) (thnot (thgoal (!SUPPORT ($? y) ?)))) (thgoal (!IS ($? y) !BLOCK)))
                        '(not (= ($? x) ($? y))))
                        *thalist*)))))
        (not (and (thvsetq ($! pyr) (packo ($? x) '!PYRAMID)) (cdr ($? pyr))))
        (thvsetq ($! blocks) (cons 'ßTABLE (packo ($? x) '!BLOCK)))
        (memory ($? why))
    =>  (thcond
            ((cdr ($? blocks))
                (thgoal (!PUTON ($E (cadr ($? blocks))) ($E (car ($? blocks)))) (THUSE TC-PUTON))
                (thsetq ($! blocks) (cdr ($? blocks)))
                (thgo =>))
            (($? pyr) (thgoal (!PUTON ($E (car ($? pyr))) ($E (car ($? blocks)))) (THUSE TC-PUTON)))
            ((memorend ($? ev) '(!STACKUP ($? ev) ($? x)))))))

(putprop! 'TC-STARTEND3 'THEOREM
    '(thconse (x y ev time)
        (($? x) ($? ev) ($? time))
        (thgoal (($? x) ($? y) ($? ev) ($? time)) (THUSE TC-STARTEND4))))

(putprop! 'TC-STARTEND4 'THEOREM
    '(thconse (x newev z ev time)
        (($? x) ($? newev) ($? ev) ($? time))
        (or (and (thasval ($? x)) (thasval ($? ev)) (thasval ($? time)) (not (thasval ($? newev)))) (bug! 'tc-startend4 nil))
        (thgoal (!CHOOSE ($? ev) ($! z) nil) (THUSE TC-CHOOSE))
        (or (term? ($? z)) (bug! 'tc-startend4 nil))
        (thsetq ($! newev) (gensym 'EV))
        (putprop! ($? newev) 'END
            (putprop! ($? newev) 'START
                (getprop ($? z) (cond (= ($? x) '!START) 'START (= ($? x) '!END) 'END :else (bug! 'tc-startend4 nil)))))
        (timechk ($? newev) ($? time))
        (putprop! ($? newev) 'WHY ($? z))
        (putprop! ($? newev) 'TYPE '!START)))

(putprop! 'TC-UNGRASP 'THEOREM
    '(thconse (x #_obj (why ($? ev)) ev)
        (!UNGRASP)
        (thcond ((thgoal (!GRASPING ($? x)))
                (memory ($? why))
                (thgoal (!SUPPORT ? ($? x)))
                (therase (!GRASPING ($? x)))
                (memorend ($? ev) '(!UNGRASP ($? ev) ($? x)))
                (thsetq *thtime* (inc *thtime*))
                (thor (UNGRASP) (and (GRASP ($? x)) nil)))
            ((thsucceed)))))

(putprop! 'TC-WANT4 'THEOREM
    '(thconse (x ev time y)
        (!WANT ($? x) ($? ev) ($? time))
        (thgoal (!WANT ($? y) ($? x) ($? ev) ($? time)) (THUSE TC-WANT5))))

(putprop! 'TC-WANT5 'THEOREM
    '(thconse (x newev ev time z)
        (!WANT ($? newev) ($? x) ($? ev) ($? time))
        (or (and (thasval ($? x)) (thasval ($? ev)) (thasval ($? time))) (bug! 'tc-want5 nil))
        (= ($? x) 'ßFRIEND)
        (= (getprop ($? ev) 'WHY) 'COMMAND)
        (thsetq ($! newev) (gensym 'EV))
        (putprop! ($? newev) 'END
            (putprop! ($? newev) 'START
                (getprop ($? ev) 'START)))
        (timechk ($? newev) ($? time))
        (putprop! ($? newev) 'TYPE '!TELL)
        (putprop! ($? newev) 'WHY 'ESP)))

(putprop! 'TCT-EXISTS 'THEOREM
    '(thconse nil (!EXISTS ? ?) (thsucceed)))

(putprop! 'TCT-PICKUP 'THEOREM
    '(thconse (x ev time)
        (!PICKUP ($? x) ($? time))
        (thor (thand (thgoal (!PICKUP ($? ev) ($? x))) (timechk ($? ev) ($? time)))
            (thgoal (!PICKUP ($? ev) ($? x) ($? time)) (THUSE TCTE-PICKUP)))))

(putprop! 'TCT-PUT 'THEOREM
    '(thconse (x ev time y)
        (!PUT ($? x) ($? y) ($? time))
        (thgoal (!PUT ($? ev) ($? x) ($? y) ($? time)) (THUSE TCTE-PUT))))

(putprop! 'TCT-AT 'THEOREM
    '(thconse (x y z time w)
        (!AT ($? y) ($? z) ($? time))
        (thor (thgoal (!MANIP ($? y)))
            (thand (thgoal (!IS ($? y) !BOX)) (thgoal (!AT ($? y) ($? z))) (thsucceed THEOREM)))
        (thsetq ($! x) (tfind ($? y) ($? time)))
        (thor (thsetq ($! w) (car ($? x)))
            (thand (thamong ($? w) (cdr ($? x))) (or (not (< (car ($? w)) (or (start? ($? time)) -1))) (thfail))))
        (thsetq ($? z) (cadr ($? w)))))

(putprop! 'TCT-LOC 'THEOREM
    '(thconse (yy zz x y z time)
        (!LOC ($? x) ($? y) ($? z) ($? time))
        (thgoal (!AT ($? y) ($? yy) ($? time)) (THUSE TCT-AT))
        (thgoal (!AT ($? z) ($? zz) ($? time)) (THUSE TCT-AT))
        (thgoal (!TLOC ($? x) ($? y) ($? z)) (THUSE TC-LOC))))

(putprop! 'TCT-SUPPORT 'THEOREM
    '(thconse (x y z time)
        (!SUPPORT ($? x) ($? y) ($? time))
        (thor (thgoal (!MANIP ($? y))) (thgoal (!IS ($? y) !BOX)))
        (thamong ($? z) (tfind ($? y) ($? time)))
        (not (< (car ($? z)) (or (start? ($? time)) -1)))
        (thamong ($? x) (list (caddr ($? z))))))

(putprop! 'TCT-2 'THEOREM
    '(thconse (x ev time) (($? x) ($? time)) (thgoal (($? x) ($? ev) ($? time)) (THUSE TCTE-3))))

(putprop! 'TCT-3 'THEOREM
    '(thconse (x y ev time) (($? x) ($? y) ($? time)) (thgoal (($? x) ($? ev) ($? y) ($? time)) (THUSE TCTE-4))))

(putprop! 'TCT-4 'THEOREM
    '(thconse (x y z ev time) (($? x) ($? y) ($? z) ($? time)) (thgoal (($? x) ($? ev) ($? y) ($? z) ($? time)) (THUSE TCTE-5))))

(putprop! 'TCTE-PICKUP 'THEOREM
    '(thconse (x ev event time)
        (!PICKUP ($? ev) ($? x) ($? time))
        (thor (thand (thgoal (!PICKUP ($? ev) ($? x))) (timechk ($? ev) ($? time)) (thsucceed THEOREM))
            (thsucceed))
        (thamong ($? event) *eventlist*)
        (memq (getprop ($? event) 'TYPE) '(!PUTON !GET-RID-OF))
        (timechk ($? event) ($? time))
        (thor (thgoal (!PUTON ($? event) ($? x) ?))
            (thgoal (!GET-RID-OF ($? event) ($? x))))
        (thvsetq ($! ev) (gensym 'E))
        (and (putprop! ($? ev) 'END (putprop! ($? ev) 'START (getprop ($? event) 'END)))
            (putprop! ($? ev) 'TYPE '!PICKUP)
            (putprop! ($? ev) 'WHY ($? event))
            (set! *eventlist* (cons ($? ev) *eventlist*))
            (thassert (!PICKUP ($? ev) ($? x))))))

(putprop! 'TCTE-PUT 'THEOREM
    '(thconse (x y ev event time z)
        (!PUT ($? ev) ($? x) ($? y) ($? time))
        (thamong ($? event) *eventlist*)
        (memq (getprop ($? event) 'TYPE) '(!PICKUP !PUTON))
        (timechk ($? event) ($? time))
        (thor (thgoal (!PUTON ($? event) ($? x) ?))
            (thgoal (!PICKUP ($? event) ($? x))))
        (or (thvsetq ($! z) (dec (assq (getprop ($? event) 'END) (getprop ($? x) 'HISTORY))))
            (bug! 'tcte-put nil))
        (thamong ($? y) (list (cadr ($? z))))
        (thsetq ($! ev) (gensym 'E))
        (and (putprop! ($? ev) 'END (putprop! ($? ev) 'START (car ($? z))))
            (putprop! ($? ev) 'WHY ($? event))
            (putprop! ($? ev) 'TYPE '!PUT)
            (set! *eventlist* (cons ($? ev) *eventlist*))
            (thassert (!PUT ($? ev) ($? x) ($? y))))))

(putprop! 'TCTE-3 'THEOREM
    '(thconse (x ev time)
        (($? x) ($? ev) ($? time))
        (or (thasval time) (bug! 'tcte-3 nil))
        (thgoal (($? x) ($? ev)))
        (timechk ($? ev) ($? time))))

(putprop! 'TCTE-4 'THEOREM
    '(thconse (x y ev time)
        (($? x) ($? ev) ($? y) ($? time))
        (or (thasval ($? time)) (bug! 'tcte-4 nil))
        (thgoal (($? x) ($? ev) ($? y)))
        (timechk ($? ev) ($? time))))

(putprop! 'TCTE-5 'THEOREM
    '(thconse (x y z ev time)
        (($? x) ($? ev) ($? y) ($? z) ($? time))
        (or (thasval ($? time)) (bug! 'tct-5 nil))
        (thgoal (($? x) ($? ev) ($? y) ($? z)))
        (timechk ($? ev) ($? time))))

(putprop! 'TCT-GRASP 'THEOREM
    '(thconse (x z time)
        (!GRASP ($? x) ($? time))
        (thvsetq ($! z) (endtime *grasplist* ($? time)))
    =>  (thcond ((or (nil? ($? z)) (startime ($? z) ($? time))) (thfail))
            ((or (and (not (thasval ($? x))) (thsetq ($! x) (cadar ($? z)))) (= ($? x) (cadar ($? z)))))
            ((thsetq ($! z) (cdr ($? z))) (thgo =>))
            ((thfail)))))

(putprop! 'TE-CONTAIN 'THEOREM
    '(therasing (x y)
        (!AT ($? x) ?)
        (thgoal (!CONTAIN ($! y) ($? x)))
        (therase (!CONTAIN ($? y) ($? x)))))

(putprop! 'TE-EXISTS 'THEOREM
    '(therasing (x) (!EXISTS ($? x)) (thsucceed)))

(putprop! 'TE-SUPP 'THEOREM
    '(therasing (x y z)
        (!AT ($? x) ?)
        (thcond ((thgoal (!SUPPORT ($? x) ($! y))) (bug! 'te-supp nil))
            ((thgoal (!SUPPORT ($! y) ($? x)))
                (therase (!SUPPORT ($? y) ($? x)))
                (thcond
                    ((thgoal (!PART ($? x) ($! y)))
                        (therase (!PART ($? x) ($? y)))
                        (thcond ((thfind 2 ($? w) (w) (thgoal (!PART ($? w) ($? y))))
                                (thsucceed THEOREM))
                            ((thgoal (!PART ($! z) ($? y)))
                                (therase (!PART ($? z) ($? y))))
                            ((thsucceed)))
                        (therase (!EXISTS ($? y)) (THUSE TE-EXISTS)))
                    ((thsucceed)))))))

(defn- topcenter [x] (let [x (atab x)] (tcent (cadr x) (caddr x))))

(putprop! '!CLEARTOP 'CHOOSE
    '(((thgoal (!SUPPORT ($? *) ?)))))

(putprop! '!GRASP 'CHOOSE
    '(((thnot (thgoal (!GRASPING ($? *)))) (thgoal (!CLEARTOP ($? *))))
     ((thnot (thgoal (!GRASPING ($? *)))))))

(putprop! '!PICKUP 'CHOOSE
    '(((thgoal (!SUPPORT ? ($? *))) (thgoal (!CLEARTOP ($? *))))
     ((thgoal (!SUPPORT ? ($? *))))))

(putprop! '!PUTIN 'CHOOSE
    '(((thnot (thgoal (!CONTAIN ßBOX ($? *)))) (thgoal (!CLEARTOP ($? *))))
     ((thnot (thgoal (!CONTAIN ßBOX ($? *)))))))

(putprop! '!PUTIN 'CHOOSE2
    '(((thgoal (!IS ($? *) !BOX)))))

(putprop! '!PUTON 'CHOOSE
    '(((thgoal (!CLEARTOP ($? *))))))

(putprop! '!PUTON 'CHOOSE2
    '(((thgoal (!CLEARTOP ($? *))) (thnot (thgoal (!IS ($? *) !PYRAMID))))
     ((thnot (thgoal (!IS ($? *) !PYRAMID))))))

(putprop! '!STACKUP 'CHOOSE
    '(((thgoal (!CLEARTOP ($? *))) (thnot (thgoal (!IS ($? *) !PYRAMID))))
     ((thnot (thgoal (!IS ($? *) !PYRAMID))))))

(dorun (map #(thadd % nil)
    '(TC-CALL TC-CLEARTOP TC-GET-RID-OF TC-GRASP TC-NAME TC-NOTICE TC-PACK TC-PICKUP TC-PUTIN TC-PUTON TC-RAISEHAND TC-STACKUP TC-UNGRASP TC-ON TC-PHYSOB)))

#_(ns shrdlu.blockl)

;; ################################################################
;;
;;            BLOCKL - LISP CODE FOR THE BLOCKS WORLD
;;
;; ################################################################

(defn- atab [x] (or (assq x ATABLE) (bug! 'atab "NOT FOUND IN ATABLE" x)))

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

(defn- findspace [type surf size obj]
    (let [obj (listify obj)]
        (when-not (memq surf obj)
            (let-when [[[_min _max level :as ?] _]
                    (if (= surf 'ßTABLE) ['(0 0) '(640 640) 0]
                        (let [a (atab surf)]
                            (cond (= type 'CENTER)
                                    (let [v (list (max 0 (+ (caadr a) (half (- (caaddr a) (car size)))))
                                                  (max 0 (+ (cadadr a) (half (- (cadr (caddr a)) (cadr size)))))
                                                  (+ (caddr (cadr a)) (caddr (caddr a))))]
                                        [nil (when (clear v size obj) v)])
                                (= (car a) 'ßBOX)
                                    [[(list (caadr a) (cadadr a)) (list (+ (caaddr a) (caadr a)) (+ (cadr (caddr a)) (cadadr a))) 1] nil]
                                :else
                                    (let [x1 (half (car size)) y1 (half (cadr size))]
                                        [[(list (max 0 (- (caadr a) x1)) (max 0 (- (cadadr a) y1)))
                                          (list (min (dec (+ (caaddr a) (caadr a) x1)) 640) (min (dec (+ (cadr (caddr a)) (cadadr a) y1)) 640))
                                          (+ (caddr (cadr a)) (caddr (caddr a)))] nil]))))
            ] ? => _
                (let [x1 (- (car _max) (car _min)) y1 (- (cadr _max) (cadr _min))]
                    (loop-when [n (dec 8)] (pos? n)
                        (let [v (grow (list (+ (car _min) (rand-int x1)) (+ (cadr _min) (rand-int y1)) level) _min _max obj)]
                            (if (or (not v) (< (- (caadr v) (caar v)) (car size)) (< (- (cadadr v) (cadar v)) (cadr size)))
                                (recur (dec n))
                                (condp = type
                                    'RANDOM (list (half (- (+ (caar v) (caadr v)) (car size))) (half (- (+ (cadar v) (cadadr v)) (cadr size))) level)
                                    'PACK (list (caar v) (cadar v) level))))))))))

(defn- grow [loc _min _max obj]
    (let [obj (listify obj) m (atom {})]
        (when-not (or
                (neg? (caar (swap! m assoc :xl (list (list (- (car loc) (car _min)) nil)))))
                (neg? (caar (swap! m assoc :xh (list (list (- (car _max) (car loc)) nil)))))
                (neg? (caar (swap! m assoc :yl (list (list (- (cadr loc) (cadr _min)) nil)))))
                (neg? (caar (swap! m assoc :yh (list (list (- (cadr _max) (cadr loc)) nil)))))
                (nil? (ERRSET (dorun (map (lambda [a]
                        (when (and (not (memq (car a) obj)) (< (caadr a) (car _max)) (< (cadadr a) (cadr _max)))
                            (let-when [x (+ (caadr a) (caaddr a))] (< (car _min) x)
                                (let-when [y (+ (cadadr a) (cadr (caddr a)))] (< (cadr _min) y)
                                    (when (< (caddr loc) (+ (caddr (cadr a)) (caddr (caddr a))))
                                        (cond
                                            (< (car loc) (caadr a)) (swap! m update :xh #(order (list (- (caadr a) (car loc)) (car a)) %))
                                            (< x (car loc))         (swap! m update :xl #(order (list (- (car loc) x) (car a)) %))
                                            :else                   (swap! m update :xo #(cons (car a) %)))
                                        (cond
                                            (< (cadr loc) (cadadr a)) (swap! m update :yh #(order (list (- (cadadr a) (cadr loc)) (car a)) %))
                                            (< y (cadr loc))          (swap! m update :yl #(order (list (- (cadr loc) y) (car a)) %))
                                            (memq (car a) (:xo @m)) (ERR nil)
                                            :else                     (swap! m update :yo #(cons (car a) %)))
                                        nil)))))
                        ATABLE)))))
            (loop []
                (let [g (min (caar (:xl @m)) (caar (:xh @m)) (caar (:yl @m)) (caar (:yh @m)))]
                    (if (== g 1024)
                        (list
                            (list (- (car loc) (cadar (:xl @m))) (- (cadr loc) (cadar (:yl @m))))
                            (list (+ (car loc) (cadar (:xh @m))) (+ (cadr loc) (cadar (:yh @m)))))
                        (do (dorun (map (lambda [y z w]
                                (let-when [x (get @m w)] (<= (caar x) g)
                                    (if (and (cadar x) (not (memq (cadar x) (get @m y))))
                                        (swap! m assoc z (cons (cadar x) (get @m z)) w (cdr x))
                                        (RPLACA x (list 2000 (caar x))))
                                    nil))
                                [:yo :yo :xo :xo]
                                [:xo :xo :yo :yo]
                                [:xl :xh :yl :yh]))
                            (recur))))))))

(defn- locgreater [loc y z fun]
    (let [loc- #(let [a (atab %2)] (if (= ($? loc) '!LOC) a (list* nil ($? %1) (cddr a))))
          y (loc- 'yy y) z (loc- 'zz z)]
        (not (< (fun (cadr y)) (+ (fun (cadr z)) (fun (caddr z)))))))

(defn- memorend [ev a]
    (and (putprop! ev 'END *thtime*)
        (apply thassert (list (thvarsubst a nil)))
        (putprop! ev 'TYPE (car a))))

(defn- memory [why]
    (§ thand (thvsetq ($! ev) (gensym 'E))
        (thsetq *eventlist* (cons ($? ev) *eventlist*))
        (putprop! ($? ev) 'START *thtime*)
        (putprop! ($? ev) 'WHY why)))

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

(dynamic- *type*)
(dynamic- *xx*)

(defn- packo [obj type']
    (binding [*type* type' *xx* nil]
        (dorun (map (lambda [x]
            (and (thval '(thgoal (!IS ($? x) ($E *type*))) (list (list 'X x))) (set! *xx* (packord x (size x) *xx*))))
        obj))
        (doall (map cadr *xx*))))

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
    (thadd '(!START EE ßDIALOG) nil)
    (dorun (map #(when (getprop (car %) 'thassertion)
            (putprop! (car %) 'HISTORY (list (list 0 (cadr %) (cadar (thval '(thgoal (!SUPPORT ($? x) ($? y))) (list (list 'x 'thunassigned) (list 'y (car %)))))))))
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

(defn- forget []
    (set! *lastsentno* 0)
    (set! *lastsent* nil)
    (set! *lastrel* nil)
    (set! *backref* nil)
    (set! *backref2* nil)
    (dorun (map (lambda [x] (dorun (map (lambda [y] (remprop! x y)) '(BIND LASTBIND)))) '(IT THEY ONE)))
    (when *eventlist* (thflush 'HISTORY) (starthistory)))

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

(defn- combination? [& words]
    ;; THIS FUNCTION CHECKS TO SEE IF THE WORDS PASSED AS ARGS FORM
    ;; A COMBINATION SUCH AS "STACK-UP" OR "ON-TOP-OF" COMBINATIONS
    ;; ARE IN THE DICTIONARY AS A SINGLE ATOM COMPOSED OF THE WORDS
    ;; IN THE COMBINATION SEPARATED BY DASHES ALL COMBINATIONS HAVE
    ;; THE FEATURE "COMBINATION" AND HAVE A ROOT WHICH IS A LIST OF
    ;; THE WORDS IN THE COMBINATION
    (let [a (list (symbol* (interpose '- words)))]
        (when (isq a 'COMBINATION) a)))

(defn- findb [x y]
    (cond (nil? x) nil (= (cdr x) y) x :else (recur (cdr x) y)))

(defn- from [a b]
    (when (and a (not= a b)) (cons (word a) (from (cdr a) b))))

(defn- meet [a b]
    ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
    (loop-when [s nil a a] a => (seq (reverse s))
        (recur (if (memq (car a) b) (cons (car a) s) s) (cdr a))))

(defn- setmod [a b] (union (setdif a (cadr b)) (car b)))

(defn- setdif [a b]
    (loop-when [s nil a a] a => (seq (reverse s))
        (recur (if (memq (car a) b) s (cons (car a) s)) (cdr a))))

(defn- sta [a b]
    (loop-when [a a b b] b => true
        (when (and a (= (car a) (car b))) (recur (cdr a) (cdr b)))))

(defn- union [a b]
    (loop-when [a (reverse a) b b] b => (seq (reverse a))
        (recur (if (memq (car b) a) a (cons (car b) a)) (cdr b))))

#_(ns shrdlu.morpho)

;; ################################################################################
;;
;;                    MORPHO - CODE FOR MORPHOLOGICAL ANALYSIS
;;
;;                INCLUDES ETAOIN, THE INPUT HANDLER FOR THE SYSTEM
;;
;; ################################################################################

(def- FINAL (seq ".?!"))
(def- PUNCL (seq ".?!:;"))

(def- VOWEL (cons nil "AEIOUY"))
(def- CONSO (seq "BCDFGHJKLMNPQRSTVWXZ"))
(def- LRSZV (seq "LRSZV"))
(def- CGSJVZ (seq "CGSJVZ"))

(defn- uppercase-ify-char [c] (if (< 96 c 123) (- c 32) c))

(dynamic- *word*)
(dynamic- *wrd*)
(dynamic- *rd*)
(dynamic- *poss*)

(§ defn- ETAOIN []
    (binding [*word* nil *wrd* nil *rd* nil *poss* nil] (set! *sent* (set! *punct* nil))
    DO  (terpri)
        (print "READY")
        (terpri)
        (when (or *sent* *word*)
            (dorun (map -print (reverse *sent*)))
            (print \space)
            (dorun (map print (reverse *word*))))
    CHAR (loop []
            (let [c (ascii (uppercase-ify-char (TYI)))]
                (cond (or (= c \space) (= c (ascii 13))) (GO WORD)
                    (= c (ascii 127))
                        (cond *word* (do (print (car *word*)) (set! *word* (cdr *word*)))
                                *sent* (do (print (car *sent*)) (set! *sent* (cdr *sent*))))
                    (memq c PUNCL)
                        (do (set! *punct* c)
                            (if *word* (GO WORD) (GO PUNC)))
                    (or (number? c) (and (= c '=) (nil? *word*)) (memq c VOWEL) (memq c CONSO))
                        (set! *word* (cons c *word*)))
                (recur)))
    WORD (cond (nil? *word*) (GO CHAR)
            (and (set! *wrd* (ERRSET (symbol* (reverse *word*)))) (number? (set! *wrd* (car *wrd*))))
                (do (set! *sent* (cons *wrd* *sent*))
                    (buildword *wrd* (or (and (zero? (dec *wrd*)) ['NUM 'NS]) ['NUM]) [['NUM *wrd*]] nil)) ;; NO ROOT FOR NUMBERS
            (nil? *wrd*) (do (set! *wrd* (reverse *word*)) (GO NO))
            :else
                (or (getprop *wrd* 'FEATURES)     ;; IF A WORD HAS FEATURES, IT'S PROPERTIES ARE ALL SET UP IN THE DICTIONARY
                    (let [x (getprop *wrd* 'IRREGULAR)]
                        (cond x
                                (buildword *wrd* (setmod (getprop (car x) 'FEATURES) (cdr x)) (semantics x) (car x))
                            (= (last *word*) '=)
                                (buildword *wrd* (if (memq '\" *word*) ['PROPN 'NS 'POSS] ['PROPN 'NS]) [['PROPN true]] nil) ;; "sic!
                            :else (GO CUT)))))
        (GO WRD)
    CUT (cond
            (sta *word* '(T \' N)) (do (set! *rd* (cdddr *word*)) (set! *word* (cons '* *word*)) (GO TRY))
            (sta *word* '(S \')) (do (set! *word* (cddr *word*)) (set! *poss* *wrd*) (GO WORD))
            (sta *word* '(\')) (do (set! *word* (cdr *word*)) (set! *poss* *wrd*) (GO WORD))
            (sta *word* '(Y L)) (do (set! *rd* (cddr *word*)) (GO LY))
            (sta *word* '(G N I)) (set! *rd* (cdddr *word*))
            (sta *word* '(D E)) (set! *rd* (cddr *word*))
            (sta *word* '(N E)) (set! *rd* (cddr *word*))
            (sta *word* '(R E)) (set! *rd* (cddr *word*))
            (sta *word* '(T S E)) (set! *rd* (cdddr *word*))
            (sta *word* '(S)) (do (set! *rd* (cdr *word*)) (GO SIB))
            :else (GO NO))
        (let [lst (car *rd*) nxt (cadr *rd*)]
            (cond (and (memq lst CONSO) (not (memq lst LRSZV)) (= lst nxt)) (set! *rd* (cdr *rd*))
                (= lst 'I) (set! *rd* (cons 'Y (cdr *rd*)))
                (or (and (memq lst CONSO) (memq nxt VOWEL) (not (= nxt 'E)) (memq (caddr *rd*) CONSO))
                    (and (memq lst LRSZV) (memq nxt CONSO) (not (memq nxt LRSZV)))
                    (and (= lst 'H) (= nxt 'T))
                    (and (memq lst CGSJVZ) (or (memq nxt LRSZV) (and (memq nxt VOWEL) (memq (caddr *rd*) VOWEL)))))
                        (set! *rd* (cons 'E *rd*))))
        (GO TRY)
    LY  (when (and (memq (car *rd*) VOWEL) (not (= (car *rd*) 'E)) (memq (cadr *rd*) CONSO))
            (set! *rd* (cons 'E *rd*)))
        (SETQ ROOT (symbol* (reverse *rd*)))
        (when (memq 'ADJ (getprop ROOT 'FEATURES))
            ;; TEMP NIL SEMANTICS ;; ROOT IS THE ADJECTIVE
            (buildword *wrd* ['ADV 'VBAD] nil ROOT)
            (GO WRD))
        (GO NO)
    SIB (let [lst (car *rd*) nxt (cadr *rd*)]
            (cond (not= lst 'E) true
                (= nxt 'I) (set! *rd* (cons 'Y (cddr *rd*)))
                (= nxt 'X) (set! *rd* (cdr *rd*))
                (and (= nxt 'H) (not (= (caddr *rd*) 'T))) (set! *rd* (cdr *rd*))
                (and (memq nxt '(S Z)) (= nxt (caddr *rd*))) (set! *rd* (cddr *rd*))))
    TRY (SETQ ROOT (symbol* (reverse *rd*)))
        (let [features (getprop ROOT 'FEATURES)]
            (cond (or features (let [x (getprop ROOT 'IRREGULAR)] (and x (SETQ features (setmod (getprop (SETQ ROOT (car x)) 'FEATURES) (cdr x))))))
                    (buildword *wrd* (setmod features (getprop (car *word*) 'MOD)) (getprop ROOT 'SEMANTICS) ROOT)
                (= (car *rd*) 'E) (do (set! *rd* (cdr *rd*)) (GO TRY))
                :else (GO NO)))
    WRD (set! *sent*
            (if *poss*
                (let [features (getprop *wrd* 'FEATURES)]
                    (if (or (memq 'NOUN features) (memq 'PROPN features))
                        ;; IF IT'S A NOUN OR A PROPER NOUN, MARK IT AS POSSESSIVE
                        (do (buildword *poss* (conj (meet features (getprop 'POSS 'ELIM)) 'POSS) (getprop *wrd* 'SEMANTICS) ROOT)
                            (cons *poss* *sent*))
                        ;; CAN WE GENERALIZE IT???
                        (do (buildword "'S" ['VB 'BE 'V3PS 'PRES] (getprop 'BE 'SEMANTICS) 'BE)
                            (cons "'S" (cons *wrd* *sent*)))))
                (cons *wrd* *sent*)))
    PUNC (when *punct*
            (if (memq *punct* FINAL)
                (RETURN (reverse *sent*)) ;; RETURN POINT !!!!!!!!!!!!!
                (set! *sent* (cons *punct* *sent*)))
            (set! *punct* nil))
        (set! *word* (set! *poss* nil))
        (GO CHAR)
    NO  (terpri)
        (print (str "SORRY, I DON'T KNOW THE WORD \"" *wrd* "\"."))
        (terpri)
        (print "PLEASE TYPE <LF> AND CONTINUE THE SENTENCE.")
        (while (!= (TYI) 10) nil)
        (set! *word* (set! *punct* nil))
        (GO DO)))

(defn- propname [x] (= (car (name x)) \=))

(defn- buildword [word features semantics root]
    (putprop! word 'FEATURES features)
    (putprop! word 'SEMANTICS semantics)
    (when root
        (putprop! word 'ROOT root))
    word)

(defn- undefined [n] (print (word n)) (bug! 'undefined nil))

#_(ns shrdlu.progmr)

;; ###########################################################
;;
;;                         PROGMR
;;  (INTERPRETER FOR THE PROGRAMMAR GRAMMAR WRITING LANGUAGE)
;;
;; ###########################################################

(defn- setmvb [node]
    (let [save *pt*]
        (set! *mvb* node)                              ;; IF THERE ARE SEVERAL CLAUSES ACTIVE AT THE
        (set! *pt* node)                               ;; SAME TIME, IT SETS THE NEAREST ONE.
        (setr (move-pt 'C 'U '(CLAUSE)) 'MVB node)
        (set! *pt* save)
        true))

(defn- add-f [feature node]
    (putprop! (car node) 'FEATURES (cons feature (features node)))
    (when (= node *c*)
        (set! *fe* (features node)))
    true)

(defn- remove-f [feature node]
    (putprop! (car node) 'FEATURES (setdif (getprop (car node) 'FEATURES) (list feature)))
    (when (= node *c*)
        (set! *fe* (features node)))
    true)

(defn- one-word-left [nb] (and (cdr nb) (not (cddr nb))))

(defn- move-pt [& a]
    (let [save *pt*]
        (loop [a a]
            (let-when [[? a]
                    (loop-when a (and (cdr a) (not (term? (cadr a)))) => [nil a]
                        (cond (nil? (cdr a)) [:break nil]
                            (term? (cadr a)) (if (cdr a) (recur (cdr a)) [:break nil])
                            (if (cdadr a) (eval (cadr a)) (isq *pt* (caadr a))) (if (cddr a) (recur (cddr a)) [:break nil])
                            :else [nil a]))
            ] (not ?) => *pt*
                (let-when [?
                        (loop [x (car a) b nil]
                            (let-when [?
                                    (condp = x
                                        'H (when-not (set! *pt* *h*) :fail)
                                        'C (do (set! *pt* *c*) nil)
                                        'PC (do (set! *pt* (daughters (parent *c*))) nil)
                                        'LASTSENT (do (set! *pt* *lastsent*) nil)
                                        'U (when-not (set! *pt* (parent *pt*)) :fail)
                                        'DLC (when-not (set! *pt* (daughters *pt*)) :fail)
                                        'DF ['DLC (cons 'DLC (cons 'FR b))]
                                        'FR (when (move-pt 'PV) [x b])
                                        'NX (when-not (set! *pt* (previous (daughters (parent *pt*)) (car *pt*))) :fail)
                                        'PV (when-not (set! *pt* (or (and (= *pt* *c*) (daughters (parent *c*))) (following (daughters (parent *pt*)) (car *pt*)))) :fail)
                                        (bug! 'move-pt "ILLEGAL INSTRUCTION" x))
                            ] (not ?) => (if (vector? ?) (let [[x b] ?] (recur x b)) ?)
                                (let [b (and b (cdr b))] (when b (recur (car b) b)))))
                ] (not ?) => (do (set! *pt* save) nil)
                    (recur a))))))

(defn- move-ptw [& a]
    (let [save *ptw*]
        (loop [a a]
            (let-when [[? a]
                    (loop-when a (and (cdr a) (not (term? (cadr a)))) => [nil a]
                        (cond
                            (term? (cadr a)) (if (cdr a) (recur (cdr a)) [:break nil])
                            (if (cdadr a) (eval (cadr a)) (isq *ptw* (caadr a))) (if (cddr a) (recur (cddr a)) [:break nil])
                            :else [nil a]))
            ] (not ?) => *ptw*
                (let-when [?
                        (loop [x (car a)]
                            (condp = x
                                'N (do (set! *ptw* *n*) nil)
                                'LASTSENT (do (set! *ptw* (firstword *lastsent*)) nil)
                                'FW (do (set! *ptw* (firstword *pt*)) nil)
                                'LW (cond (= *pt* *c*) :fail (set! *ptw* (wordafter *pt*)) (recur 'PW))
                                'NW (cond (set! *ptw* (cdr *ptw*)) nil (set! *ptw* (findb *sent* nil)) :fail)
                                'PW (cond (set! *ptw* (findb *sent* *ptw*)) nil (set! *ptw* *sent*) :fail)
                                'SFW (do (set! *ptw* *sent*) nil)
                                'SLW (do (set! *ptw* (findb *sent* nil)) nil)
                                (bug! 'move-ptw "ILLEGAL INSTRUCTION" x)))
                ] (not ?) => (do (set! *ptw* save) nil)
                    (recur-if (cdr a) [a] => *ptw*))))))

(defn- apply-grammar [unit] (eval (list unit)))

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

(defn- cut-back-one [] (move-ptw 'N 'PW) (pop*) (cut *ptw*))

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

(defn- parse [& a]
    (if (memq (car a) '(NG CLAUSE VG PREPG ADJG)) (parse2 a (memq 'TOPLEVEL a)) (parse3 a nil)))

(dynamic- *rest*)

(defn- parse2 [rest' p]
    ;; THIS FUNCTION CALLS THE PROGRAMMAR FUNCTION INDICATED BY THE FIRST MEMBER OF REST - A FEATURE LIST.
    ;; THE PARAMETER P INDICATES WHETHER PARSE2 IS BEING CALLED FROM THE TOPLEVEL.
    ;; IF P IS TRUE, PARSE2 DOES NOT APPEND THE NODE JUST PARSED TO THE PARSING TREE.
    (when (not= *n* *cut*)
        (binding [*rest* rest' *end* *cut* *parent* *c* *re* nil]
            (set! *nn* true)
            (let [unit (car *rest*) nbb *n*
                  ? (when (nq 'B-SPECIAL)
                        (binding [*special* nil]
                            (eval (getr *n* 'B-SPECIAL))
                            (condp = *special* 'SKIP :skip 'DONE :done 'LOSE (do (set! *n* nbb) :lose) nil)))
                  ;; THIS IS WHERE ALL THE WORK HAPPENS.
                  ;; IF THE PARSE SUCCEEDS, IT WILL RETURN THE NODE THAT HAS BEEN BUILT UP.
                  ? (or ? (when-not (set! *re* (apply-grammar unit))
                            (set! *n* nbb)
                            :lose))
                  ? (or (when (not= ? :skip) ?)
                        (when (and (not= *n* *cut*) (nq 'SPECIAL))
                            (eval (getr *n* 'SPECIAL))
                            nil))
                  ? (or (when (not= ? :done) ?)
                        (when-not p
                            (set! *fe* (getprop (car *c*) 'FEATURES))
                            (set! *h* (concat *re* *h*))
                            ;; REBUILDS THE HIGHER LEVEL NODE TO INCORPORATE THE DAUGHTER THAT WAS JUST PARSED,
                            ;; EXCEPT IN THE CASE WHERE THIS NODE IS THE TOPLEVEL.
                            (rebuild *c* *fe* *nb* *n* *h* *sm*)
                            nil))]
                (set! *nn* (not= *n* *cut*))
                *re*))))

(defn- parse3 [rest' p]
    ;; PARSE3 IS CALLED TO PARSE JUST THE NEXT WORD IN THE SENTENCE.
    (when' (not= *n* *cut*) => (do (m! 'CUT) nil)
        (binding [*rest* rest' *re* nil]
            (let-when [nbb *n*
                  ? (when (nq 'B-SPECIAL)
                        (binding [*special* nil]
                            (eval (getr *n* 'B-SPECIAL))
                            (condp = *special* 'SKIP :skip 'DONE :done 'LOSE (do (set! *n* nbb) :lose) nil)))
            ] (not= ? :lose) => nil
                ;; IF CALL IS (PARSE NIL FOO), THEN LOOK FOR EXACT WORD "FOO".
                ;; IF NOT THERE, FAIL.
                (let-when [? (or ?
                            (let-when [a *rest* [? f]
                                    (cond (car a)
                                        (loop [f nil a a]
                                            ;; IF THE FEATURE IS NOT AN ATOM, JUST ADD THE FEATURE TO THE LIST.
                                            (let [[? f]
                                                    (cond (not (term? (car a))) [nil (cons (caar a) f)]
                                                        (or (= (car a) 'NULL) (memq (car a) (features *n*)) (memq (car a) '(COMPONENT BOTH))) [nil f]
                                                        :else (do (m! (car a)) (set! *n* nbb) [:lose nil]))]
                                                (recur-if (and (not ?) (cdr a)) [f (cdr a)] => [? f])))
                                    (nextword? *n* (cadr a)) [nil nil]
                                    :else (do (set! *n* nbb) [:lose nil]))
                            ] (not ?) => ?
                                (set! *re*
                                    (buildnode (meet (concat (features *n*) f) (getprop (car *rest*) 'ELIM)) *n* (cdr *n*) 'WORD
                                        (or (nil? (car *rest*)) (and (nil? (semantics *n*)) (undefined *n*)) (cadr (or (assq (car *rest*) (semantics *n*)) (undefined *n*))))))
                                (set! *n* (cdr *n*)))
                                nil)
                ] (not= ? :lose) => nil
                    (let [? (or (when (not= ? :skip) ?)
                                (when (and (set! *nn* (not= *n* *cut*)) (nq 'SPECIAL))
                                    (eval (getr *n* 'SPECIAL))
                                    nil))]
                        (setr *re* 'PARENT *c*)
                        (when-not p
                            (set! *h* (concat *re* *h*))
                            (rebuild *c* *fe* *nb* *n* *h* *sm*))
                        *re*))))))

(defn- parserel [a b node]
    (loop [a a] (when (some? a)
        (if (and (isq node (caar a)) (apply parse 'CLAUSE 'RSNG (concat (cdar a) b))) *h* (recur (cdr a))))))

(dynamic- *pop*)

(defn- pop* [& a]
    (if (and a (car a))
        (when (apply popto a) (pop*))
        (when (and *h* (set! *n* (firstword *h*)))
            (set! *h* (cdr *h*))
            (rebuild *c* *fe* *nb* *n* *h* *sm*)
            (set! *nn* (not= *n* *cut*))
            (do (set! *pop* nil)
                (dorun (map* (lambda [b] (ERRSET (when *n* (dorun (map* #(and (= % (firstword b)) (ERR)) *n*)) (set! *pop* (cons (car b) *pop*))))) *backref*))
                (set! *backref* *pop*))
            true)))

(defn- popto [& a]
    (loop [x *h*]
        (cond (apply isq x a) (loop [] (cond (= x *h*) *c* (pop*) (recur)))
            (cdr x) (recur (cdr x))
            :else (do (m! 'POPTO) nil))))

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
            (list e (list 'when '*labeltrace* (list 'passing (quotify e))))
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

(defmacro grammar! [& a]
    (list 'defn- (car a) '[]
        (concat
            (list 'binding
                (into '[*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil] (cadr a))
                '(set! *nn* true)
                '(set! *cut* *end*)
                '(set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
                '(setr *c* 'PARENT *parent*))
            (mapcat spread1 (cddr a))
            (list 'FAIL
                '(set! *mes* *me*)
                '(set! *n* (or (wordafter *re*) *nb*))
                '(RETURN nil)
                'RETURN
                '(set! *mes* *me*)
                '(rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))))

#_(ns shrdlu.gramar)

(§ grammar! CLAUSE [*position-of-prt* nil *mvb* nil *locationmarker* nil *subj-vb-backup-type1* nil *position-of-ptw* nil]

    ENTERING-CLAUSE
        (setr *c* 'TIME (build 'TSSNODE= (gensym 'TSS)))
        (| (cq 'SIMP) SUBJ nil)
        (| (cq 'MAJOR) INIT SEC)

    INIT
        (set! *locationmarker* *n*)
        (| (and (nq 'BINDER) (parse 'CLAUSE 'BOUND 'INIT)) nil MAJOR FIXIT)
        (fq! 'BIND)
        (| (smbind) INIT nil)

    FIXIT
        (set! *ptw* *cut*)
        (| (cut (move-ptw)) INIT MAJOR)

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
            (or (smrelate *h*) (pop*)))                       ;; MORE INITIAL (BEFORE THE SUBJECT) MODIFIERS
        (and (nq 'ADV)
            (parse 'ADV 'TIMW)
            (or (smadverb) (pop*)))
        (and (nq 'ADV)
            (parse 'ADJG 'ADV 'VBAD)
            (or (smrelate *h*) (pop*)))
        (parse 'NG 'TIME)

        (| (= *locationmarker* *n*) CLAUSETYPE INIT INPOP)

    ;; THE VARIABLE "LOCATIONMARKER" MARKS THE POSITION OF PTW AT THE TIME THAT IT WAS SET.
    ;; IF IT HAS NOT MOVED (IS STILL EQUAL TO N), THEN THAT INDICATES THAT NOTHING HAS BEEN
    ;; PARSED AND WE CAN GO ON.  OTHERWISE, THERE CAN BE ANY NUMBER OF INITIAL MODIFIERS AND
    ;; THE CODE STARTING AT "INIT" IS REPEATED, AS MANY TIMES AS NECESSARY.  IF PTW HITS THE
    ;; CUT POINT, THEN IT IS ASSUMED THAT SOMETHING WAS MISTAKENLY PARSED AS A MODIFIER WHEN
    ;; IT WAS NOT, AND EVERYTHING IS POPPED OFF (BY THE "INPOP" CODE).

    INPOP
        (| (move-pt 'C 'DLC) nil (INPOP))                             ;; DOES ANYTHING REMAIN ON THE TREE?

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
            (move-pt 'C 'U '(REL-NOT-FOUND))                          ;; THE WHOLE CLAUSE MAY PROBABLY BEEN THE SUBJECT
                                                                    ;; OF THE CLAUSE THIS WORRIES ABOUT RELATIVE
                                                                    ;; CLAUSES. PLEASE NOTE THAT THE CURRENT
                                                                    ;; HALF-VERSION HAS NOT YET GOT ITS HEAD TOGETHER
                                                                    ;; ABOUT RELATIVE CLAUSES.  IE. THE CODE ISN'T
                (do (setr *c* 'SUBJECT (getr *pt* 'RELHEAD))                ;; DEBUGGED AND HAS GAPS IN IT ESP. WHO SETS WHAT
                    (setr *c* 'RELHEAD (getr *pt* 'RELHEAD))                ;; REGISTER WHEN THIS WILL BE FIXED BEFORE THE
                    (remove-f 'REL-NOT-FOUND *pt*)                     ;; VERSION IS FINALIZED
                    (GO VB))
            (and (cq 'COMPONENT) *nn*)
                (do (fq! 'SUBJFORK) (GO VB))                        ;; "SARAH ATE DINNER AND WENT TO THE MOVIES."
            *h* (do (pop*) (GO SUBJ))                                     ;; POP OFF THE CLOSEST INITIAL MODIFIER AND TRY TO
            :else (GO FAIL))                                            ;; PARSE A SUBJ AGAIN

    HEAD
        (| (or (move-ptw 'N 'PW '(NOUN)) (move-ptw 'N 'PW '(PRON)))       ;; COME HERE (ONLY?) TO TRY TIME PHRASE AS SUBJECT
            nil
            (HEAD))                                                 ;; MOVE PTW TO THE CLOSEST NOUN THEN SET THE CUT

    SUB2
        (| (pop*) nil FAIL)                                          ;; POINT TO IT AND ATTEMPT A NEW PARSING IF
        (| (cut *ptw*) INIT SUB2)                                     ;; NOTHING MORE TO POP, LOSE

    SUBJ1
        (when (isq *h* 'QUOTED)                                       ;; CIRCUMSTANCES UNDER WHICH IT IS ALLRIGHT TO
            (when (isq *h* 'LIST) (fq! 'LIST))                            ;; HAVE NOTHING FOLLOWING THE SUBJECT OF THE
            (fq! 'QUOTED)                                             ;; CLAUSE "  "MUMBLE", SAID JOHN."
            (set! *h* (daughters *h*))
            (GO RETSM))
        (and (cq 'REL-NOT-FOUND)                                     ;; THIS IS PART OF A BACKUP MECHANISM WHICH NEEDS
            (move-pt 'H 'PV '(QAUX))                                   ;; TO BE MORE THROUGHLY THOUGHT OUT. THE SITUATION
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
                (do (set! *subj-vb-backup-type1* true) (pop*) (GO SUBJ4))    ;; THIS IS EXACTLY WHAT IT LOOKS LIKE.
                                                                    ;; AN ARBITRARY, NOT TOO WELL THOUGHTOUT BACKUP
                                                                    ;; MECHANISM.  (NEEDLESS TO SAY IT WILL GO AWAY
                                                                    ;; FAST).  WE HAVE BEEN UNABLE TO FIND A VERB
                                                                    ;; AND HAVE NOTICED THAT WE PARSED A CLAUSE OF
                                                                    ;; SOME SORT AS THE SUBJECT.  HYPOTHESIS:  WE
                                                                    ;; MISSINTERPRETED SOMETHING WHILE PARSING THAT
                                                                    ;; CLAUSE AND MANAGED TO SWALLOW UP THE VERB OF
            (isq *h* 'SUBJ) (do (pop*) (fq! 'SUBJFORK) (GO VBL)))            ;; THE HIGHER CLAUSE WITH IT.  SOLUTION:  POP OFF
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
        (| (set! *position-of-prt* (move-ptw 'N 'NW '(PRT))) nil FINDOBJ1)       ;; IN PASV'S IF NOT FOUND ASSUME THAT IT IS OPTIONAL AND
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
        (popto 'VG)
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
        (| (or (canparse 1 '(ADJG COMP) 'INT) (canparse 1 '(NG COMP) 'INT))
            CHECKIT
            nil
            ONT)
        (| (or (canparse 1 '(PREPG COMP) 'INT)
                (canparse 1 '(CLAUSE RSNG ING) 'TRANS)
                (canparse 1 '(CLAUSE RSNG REPORT) 'TRANS)
                (canparse 1 '(CLAUSE RSNG TO) 'TRANS)
                (canparse 1 '(PREPG LOC) 'ITRNSL)
                (canparse 1 '(ADV PLACE) 'ITRNSL))
            ONT
            nil)
        (| (canparse 1 '(NG) 'TRANS)
            FINDOBJ2
            nil
            FINDFAKE2)

    FINDFAKE1
        (| (move-pt 'C 'U '(REL-NOT-FOUND)) OBJ1REL nil)
        (| (and (cantake 1 '(PREPG LOC) 'ITRNSL)
                (move-pt 'C 'U '(QADJ))
                (isq (getr *pt* 'QADJ) 'PLACE)
                (fq! 'ITRANSL))
            PUTLOBJ
            nil)
        (| (canparse 1 nil 'ITRNS) ONT nil)

    GOOF1
        (or *oops* (bug! 'clause "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)

    OBJ1REL
        (setr *c* 'OBJ1 (getr *pt* 'RELHEAD))
        (remove-f 'REL-NOT-FOUND *pt*)
        (fq! 'OBJ1REL)

    FINDOBJ2
        (| (canparse 2 '(CLAUSE RSNG TO) 'TRANS2)
            FIXSUBJECT
            nil)
        (| (or (canparse 2 '(ADV PLACE) 'TRANSL) (canparse 2 '(PREPG LOC) 'TRANSL))
            ONT
            nil)
        (| (or (canparse 2 '(ADJG COMP) 'TRANSINT) (canparse 2 '(NG COMP) 'TRANSINT))
            ONT
            nil)
        (| (canparse 2 '(NG) 'TRANS2) ONT nil)

    FINDFAKE2
        (| (and (isq *mvb* 'TRANS2) (move-pt 'C 'U '(REL-NOT-FOUND)))
            OBJ2REL
            nil)
        (| (and (cantake 2 '(PREPG LOC) 'TRANSL)
                (move-pt 'C 'U '(QADJ))
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
                    (move-pt 'H 'PV '(QUEST))
                    (= (word (move-ptw 'FW)) 'TO)
                    (rq 'PREPQ)
                    (fq! 'TRANS2TOQ 'TRANS2)
                    (setr *c* 'OBJ2 (getr *pt* 'OBJ1))))                   ;; "TO WHOM DID YOU GIVE THE MEAT?"
            ONT
            nil)
        (| (canparse 2 nil 'TRANS) ONT FAIL)

    PUTLOBJ
        (setr *c* 'LOBJ *pt*)
        (setr *pt* 'RELHEAD (getr *pt* 'QADJ))
        (setr *pt* 'QADJ nil)
        (remove-f 'QADJ *pt*)
        (GO ONT)

    OBJ2REL
        (setr *c* 'OBJ2 (getr *pt* 'RELHEAD))
        (remove-f 'REL-NOT-FOUND *pt*)
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
        (or *oops* (bug! 'clause "NEW TRANSITIVITY - SECOND OBJECT"))
        (GO FAIL)

    ;; ###########################################################################################
    ;;
    ;;                               INITIAL SEMANTIC PROCESSING
    ;;
    ;; ###########################################################################################

    ONT
        (| (cq 'PASV) PONT nil)
    ONT1
        (| (smcl1) nil (SMCL1))

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
        (| (smrelate *h*) nil (ONTß))
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
        (| (and (nq 'PREP) (parse 'PREPG) (smrelate *h*))                                  ;; PREPG
            nil
            nil
            RETSM)
        (| (and (nq 'TIMW) (parse 'ADV 'TIMW) (or (smtime) (GO FAIL)))                    ;; TIMW
            nil
            nil
            RETSM)
        (| (and (not (cq 'BE)) (parse 'ADJG 'ADV) (or (smrelate *h*) (GO FAIL)))            ;; ADV
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
        (| (and (nq 'BINDER) (parse 'CLAUSE 'BOUND) (or (smbind) (GO FAIL)))              ;; BINDER
            nil
            nil
            RETSM)
        (| (and (nextword? *n* 'TO) (parse 'CLAUSE 'TO 'ADJUNCT) (or (smtoadj) (GO FAIL)))    ;; TO CLAUSE (ADJUNCT)
            nil
            nil
            RETSM)
        (| (= *n* *position-of-ptw*) nil TONT RETSM)                   ;; LOOP UNTIL NOTHING ELSE CAN BE PARSED.
        (| (or (not (cq 'TOPLEVEL)) (nq 'SPECIAL)) RETSM nil)         ;; SPECIAL WORD (E.G. COMMA AND) COULD INDICATE
        (bug! 'clause "SOMETHING LEFT OVER AT TOP LEVEL")              ;; A CONJUNCTION OR A BINDER.

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
        (| (isq (move-pt 'H 'PV '(QAUX)) 'BE) THERQ2 nil)               ;; IF THIS FAILS, THE THERE IS CONSIDERED TO BE
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
        (| (move-pt 'C 'U '(REL-NOT-FOUND)) nil NOTHE)
        (setr *c* 'SUBJECT (getr *pt* 'RELHEAD))
        (remove-f 'REL-NOT-FOUND *pt*)
        (GO ONT)

    NOTHE
        (rq 'THERE)
        (pop* 'THERE)
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
        (| (pop* nil) IMPE (IMPOP))

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
        (smadjqshort)                                      ;; IF ALL THE SENTENCE CONSISTS OF IS THE QUESTION

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
            :else (do (move-ptw 'N 'PW) (pop* 'NG 'QUEST) (cut *ptw*) (GO NGQUES)))  ;; POP BACK AND START FIGURING OUT THE QUESTION

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
        (| (and (nq 'VB) (parse 'VB 'AUX '(QAUX)) (setr *c* 'QAUX *h*) (smvaux) (setmvb *h*))
            nil
            QCHOP)
        (or (cq 'QADJ) (getr *c* 'RELHEAD) (fq! 'POLAR))
        (fq! 'POLR2)
        (GO QUEST2)

    QCHOP
        (| (popto 'CLAUSE 'BOUND) BICUT (QCHOP))

    ;; ##############################################################################
    ;;                              SECONDARY CLAUSES
    ;; ##############################################################################

    ;; SECONDARY CLAUSES ARE PRINCABLY THOSE THAT ARE NOT MAJOR.
    ;; THIS INCLUDES ADJUNCTS, RANK-SHIFTED-NOUN-GROUP'S, RANK-SHIFTED-QUALIFIERS, FOR-TO CLAUSES AND OTHERS.
    ;; IF THE CLAUSE IS MARKED "RSQ", THEN IT AUTOMATICALLY WILL HAVE SEVERAL SPECIAL REGISTERS ASSOCIATED WITH IT
    ;; TO FACILITATE SEMANTIC PROCESSING.  'RELWORD WILL POINT TO THE INITIAL RELATIVE PRONOUN IF THERE IS ONE
    ;; (THAT, WHICH, WHO...).  ALSO "REL-NOT-FOUND" IS A TEMPORARY FEATURE, WHICH IS RELEVANT DURING THE PROCESSING
    ;; OF A CLAUSE, IT INDICATES THAT THE ELEMENT OF THE CLAUSE WHICH WAS TAKEN OVER INTO THE RELATIVE WORD
    ;; (SUBJ, OBJ1, ...) HAS NOT YET BEEN DETERMINED.  'RELHEAD IS A REGISTER WHICH POINTS TO WHATEVER THE CLAUSE
    ;; MODIFIES.  IN THE CASE OF AN RSQ, IT IS SET INITIALLY TO "(GETR 'HEAD (MOVE-PT U))" WHICH IS THE HEAD NOUN
    ;; OF THE NP WITHIN WHICH THE RSQ IS BEING PROCESSED.

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
        (setr *c* 'RELHEAD (move-pt 'C 'U '(NG)))
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
                (do (setr *c* 'RELHEAD (getr (move-pt 'C 'PC) 'RELHEAD))  ;; MAKE RELHEAD SAME AS PREVIOUS COMPONENT RSQ.
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
        (| (move-ptw 'N 'NW '(ING)) nil FAIL)
        (| (or (nq 'ING) (cq 'OBJ2) (and (parse 'NG 'SUBJ 'INGSUBJ) (setr *c* 'SUBJECT *h*) (fq! 'SUBING) (rq 'ING)))
            nil
            nil
            (ING))
        (| (parse 'VG 'ING) VG1 (ING))

    REPORT
        (and (nextword? *n* 'THAT) (parse nil 'THAT) (fq! 'THAT))
        (set! *locationmarker* *n*)                                     ;; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
        (GO FDEC)

    RETSM
        (or (smcl2 *h*) (GO FAIL))
        (GO RETURN))

(§ grammar! NG nil

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
            (and (cq 'COMPONENT) (isq (move-pt 'PC) 'QUEST)) (GO QUEST)
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
        (or (smprop) (GO FAIL))                            ;; EXAMINE ITS SEMANTICS
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
        (| (smpron *h*) nil FAIL)                            ;; EXAMINE SEMANTICS OF PN

    PRAG
        (setr *c* 'HEAD *h*)
        (move-pt 'H)
        (trnsf 'NS 'NPL 'NFS 'NEG)                                      ;; MODIFY PN FEATURES TO CORRECT NUMBER
        (GO RETURN)

    ;; -------------- ANYTHING, SOMETHING, ... --------------

    TPRON
        (parse 'TPRON)
        (fq! 'TPRON)
        (move-pt 'H)
        (trnsf 'NS 'NPL 'ANY 'NEG)
        (setr *h* 'HEAD *c*)
        (and *nn* (nq 'ADJ) (parse 'ADJ))
        (GO SMNG)

    ;; ----------- WHATEVER, WHENEVER, WHOEVER, ... -----------

    EVERPRON
        (| (and (parse 'PRON 'EVERPRON) (smpron *h*))
            nil
            FAIL)
        (| (and (parse 'CLAUSE 'RSQ 'NOREL) (smrelate *h*))
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
        (| (move-ptw 'N 'NW '(TIM1)) LOOK (TIME))

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
        (move-pt 'H)                                                 ;; SHIFT PTR TO THE DETERMINER
        (| (trnsf 'NPL 'NS 'PART 'DEF 'INDEF 'ANY 'NEG 'QNTFR)
            IND
            (BUG)
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
                (isq (move-ptw 'N 'NW) 'MONTH)
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
        (| (or (smngof) (not (pop*))) RETSM INCOM)

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
            (or (smngof) (GO FAIL))
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
        (when (and (not (cq 'DET)) (not (cq 'NUMD))) (move-pt 'H) (trnsf 'NPL 'MASS))
        (| (meet *fe* '(NS NPL PART MASS)) nil RED0)

        (| (nextword? *n* 'THAN) nil SMNG)                              ;; "...A BIGGER BLOCK THAN..."
        (fq! 'THAN)                                                   ;; THE PRESENCE OF THIS FEATURE IS NOTED BELOW AND IN ADJG

    ;; AT THIS POINT SMNG1 IS CALLED FOR PRELIMINARY CHECKS AND ANALYSIS BEFORE CHECKING QUALIFIERS

    SMNG
        (setr *c* 'HEAD *h*)                                            ;; SET HEAD REGISTER TO THE NOUN

        (| (and (cq 'OBOFJ) (not (cq 'DEF))) FAIL nil)                ;; JUST PARSED
        (or (smng1) (GO FAIL))
        (| (not (isq *h* 'POSS)) nil POSS RETSM)                       ;; CHECK FOR POSSIVE

    ;; #################################################
    ;;               POSSIBLE QUALIFIERS
    ;; #################################################

        (| (and (cq 'THAN) (parse 'ADJG)) nil RSQ-TO)                 ;; "...A BIGGER BLOCK THAN..."
        (| (smrelate *h*) RETSM FAIL)

    RSQ-TO
        (| (and (nextword? *n* 'TO) (meet *fe* '(COMP SUBJ)) (parse 'CLAUSE 'RSQ 'TO) (or (smrelate *h*) (GO POPRET)))
            RETSM
            nil)

    ;; -------------- AS OR COMPARATIVE ---------------

        (| (and (or (nextword? *n* 'AS) (nq 'COMPAR)) (parse 'ADJG 'THANNEED))
            nil
            PREPNG)                                                 ;; WHAT IS THE REASON FOR THE EXISTANCE OF THIS
        (and (nil? *n*)                                               ;; STRANGE ANIMAL (ALSO THE ONEBELOW) -- CHECK
            (cq 'SUBJ)                                               ;; THEM OVER AND HACK THEM PROPERLY
            (isq (move-pt 'C 'PV) 'AUX)
            (isq *pt* 'BE)
            (GO POPRET))                                            ;; AVOIDS ATTACHING MODIFIER WHEN IT GOBBLES TO
        (| (smrelate *h*) RSQ-TO POPRET RETSM)               ;; MUCH E.G. IS THE BLOCK ON THE TABLE? DOESN'T
                                                                    ;; WANT "THE BLOCK ON THE TABLE" AS A CONSTITUENT.
                                                                    ;; I ADMIT ITS A HACK.

    ;; -------------- ANY SORT OR PREPOSITION GROUP --------------

    PREPNG
        (| (and (nq 'PREP)
                (not (or (and (nq 'PLACE) (cq 'NOLOC))
                    (and (cq 'OBJ1)
                        (isq *mvb* 'TRANSL)
                        (not (isq (move-pt 'C 'U) 'QUEST)))))
                (parse 'PREPG 'Q))
            nil
            DISGRSQ)
        (and (nil? *n*)
            (cq 'SUBJ)
            (isq (move-pt 'C 'PV) 'AUX)
            (isq *pt* 'BE)
            (not (isq (move-pt 'U) 'NGQ))
            (GO POPRET))
        (| (smrelate *h*) RSQ-TO POPRET RETSM)

    ;; CHECK FOR DISGUISED RSQ CLAUSES BY READING THE FAILURE MESSAGES SENT UP FROM PREPG.

    DISGRSQ
        (| (= (car *mes*) 'PREP-WHICH) nil RSQ)
        (set! *mes* (cdr *mes*))
        (| (parse 'CLAUSE 'RSQ 'PREPREL) PREPNG (RSQ-PREPREL) RETSM)

    ;; -------------- ANY OTHER RSQ ---------------

    RSQ
        (| (and (isq (move-pt 'C 'U) 'POLR2) (cq 'SUBJ) (nq 'VB) (not (cq 'SUBJT)) (not (isq *pt* 'QADJ)))
            RETSM
            nil)
        (| (parse 'CLAUSE 'RSQ) nil RETSM)
        (| (smrelate *h*) RETSM POPRET)

    ;; -------------------------------------------------
    ;; THE ENTIRE NG SHOULD HAVE BEEN PROCESSED BY THIS POINT
    ;; -------------------------------------------------

    ;; -------------------------------------------------
    ;; IF AT FIRST YOU DON'T SUCEED.......
    ;; -------------------------------------------------

    RED0
        (set! *fe* *tmp*)
    RED1
        (pop*)
    RED2
        (cond (nil? *h*) (GO FAIL (m! 'NO))
           (isq *h* 'NUMBER) (GO INCOM)
           (and (isq *h* 'POSS) (or (isq *h* 'PRON) (and (move-pt 'H 'DLC) (isq *pt* 'PRON)))) (do (pop*) (GO PRON2))
           (and (nil? (cdr *h*)) (cq 'DEFPOSS)) (GO POSSDEF)
           (and (cq 'QUEST) (nil? (cdr *h*))) (GO QDETCHECK)         ;; (CDR H) = T IF THERE IS ONLY ONE DAUGHTER TO THE CURRENT NODE
           (isq *h* 'ADJ) (GO EPR)
           (not (isq *h* 'CLASF)) (GO INCOM))

    REDUC
        (pop*)
        (| (and (nil? *h*) (nq 'PROPN)) PROPN NOUN)

    POPCOM
        (pop*)

    ;; -------------- INCOMPLETE PHRASES ---------------

    INCOM
        (fq! 'INCOM)
        (| (and (isq *h* 'DET) (isq *h* 'INCOM) (SMINCOM))
            RETURN
            nil)
        (| (and (nil? *cut*) (cq 'NUM)) SMNG nil)

    QDETCHECK
        (cond (and (isq *h* 'QDET) (isq (firstword *h*) 'QPRON)) (do (pop*) (GO QPRON))
            (and (isq *h* 'QDET) (isq (firstword *h*) 'EVERPRON)) (do (pop*) (GO EVERPRON)))
        (GO FAIL)

    ;; -------------------------------------------------
    ;; POSSESSIVE HANDLER
    ;; -------------------------------------------------

    POSS
        (or (smng2) (GO FAIL))

    POSS2
        (| (cq 'INGSUBJ) RETSM nil)
        ;; IF POSSESSIVE, ALL PREVIOUS MODIFIERS MODIFY THE POSSESSIVE NOUN, NOT THE NG HEAD
        (set! *h* (buildnode (reverse (cons 'POSS (setdif *fe* '(COMPONENT)))) *nb* *n* *h* *sm*))
        (set! *backref* (concat *h* (cdr *backref*)))
        (| (setr *c* 'FEATURES (set! *fe* (concat '(POSES DET DEF NS NPL) (reverse *rest*)))) nil (BUG))
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
        (| (and (parse 'PRONREL) (smset (semantics (move-pt 'C 'U 'U '(NG)))))         ;; SET SM TO THE NOUNGROUP DIRECTLY UPSTAIRS
            RETURN
            nil)

    POPRET
        (pop*)

    RETSM
        (or (smng2) (GO TRYA))
        (GO RETURN)

    ;; -------------- YOU PROBABLY GOOFED, CUT AND TRY AGAIN. --------------

    TRYA
        (| (isq *h* 'NOUN) nil (TRYA))
        (pop*)
        (cut *n*)

    UP
        (| (pop*) UP nil)                                            ;; POP EVERYTHING OFF
        (set! *fe* (reverse *rest*))
        (smset nil)
        (GO NGSTART))

(§ grammar! VG [*tense* nil]

    ;; ##################################################################
    ;; CHECK INITIAL FEATURES TO SEE IF SOME SPECIAL TYPE OF VG IS WANTED
    ;; ##################################################################

    ENTERING-VG
        (cond (cq 'TO) (GO TO)
            (cq 'EN) (GO EN)
            (cq 'ING) (GO ING)
            (cq 'IMPER) (GO IMPER)
            (isq (move-pt 'C 'U) 'POLR2) (GO POLR2))                 ;; CHECKS IF THE CLAUSE IS MARKED AS POLR2

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
        (move-pt 'C 'DLC)                                             ;; MOVE PT DOWN FROM THE CURRENT NODE BEING PARSED (VG) AND ACROSS TO THE MOST RECENTLY PARSED
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
        (| (and (parse 'VB '(MVB) 'INF) (setmvb *h*) (smvg))
            RETURN
            (IMPER))                                                ;; MVB IS BOUND BY CLAUSE

    POLR2                                                           ;; THE CLAUSE COULD ONLY BE MARKED AS "POLR2"
        (or (set! *pt* (getr (move-pt 'C 'U) 'QAUX))                    ;; ("DID THE...?") IF AN AUX OF SOME VERIETY HAD
            (bug! 'vgßpolr2 nil))                                      ;; ALREADY BEEN PARSED, IF THAT IS NOT THE CASE,
        (set! *h* (list (car *pt*)))                                    ;; THEN WE HAVE A BUG IN THE PROGRAM SOMEWHERE SET
        (trnsf 'NEG)                                                 ;; THE INITIAL DAUGHTER OF THE VG TO BE THE
        (cond (isq *h* 'DO) (GO DO)                                  ;; PREVIOUSLY PARSED AUX MARK THE VG AS NEG IF
            (isq *h* 'MODAL) (GO MODAL)                              ;; APPROPRIATE (SEE PROGMR FILE FOR THE OPPERATION
            (isq *h* 'WILL) (GO WILL)                                ;; OF THIS FUNCTION) DISPATCH TABLE , CHECKING THE AUX
            (isq *h* 'BE) (GO BE)
            (isq *h* 'HAVE) (GO HAVE))
        (bug! 'vgßpolr2vb nil)                                        ;; NOTHING BUT UNGRAMATICAL NONSENSE SHOULD REACH THIS POINT

    ;; ###############################################################
    ;; PROCESSING OF VB'S NOT SPECIALLY MARKED FOR BY INITIAL FEATURES
    ;; ###############################################################

    DO  (fq! 'DO)
        (move-pt 'C 'DLC)                                             ;; MOVE TO THE "DO"
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
        (move-pt 'C 'DLC)                                             ;; POINT TO WHAT WAS JUST PARSED
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
        (pop*)

    GOI
        (set! *tense* (cons 'PRESENT *tense*))                          ;; WE HAVE DETERMINED THAT "GOING" IS THE ACTUAL
        (GO MVB)                                                    ;; MAIN VERB AND SHOULD BE PARSED AS SUCH

    GOING2
        (set! *tense* (cons 'FUTURE *tense*))                           ;; HERE WE DETERMINE THAT THE PHRASE IS ACTUALLY
        (GO MODAL2)                                                 ;; OF THE FORM "...IS GOING TO FALL IN LOVE..."
                                                                    ;; AND WE SHOULD RUN THROUGH THE DISPATCH TABLE AT
                                                                    ;; "MODAL2" TO DETERMINE HOW TO CONTINUE

    MVBE
        (| (isq (move-pt 'H 'PV '(VB)) 'AUX) nil MVB)                   ;; MOVE TO EARLIER AND EARLIER DAUGHTERS  UNTILL
        (| (isq *pt* 'BE) nil (MVBE))                                  ;; YOU REACH A VERB WHICH IS A "QAUX" - IF THERE
                                                                    ;; ARE NONE THEN CONTINUE AT "MVB" IF WHAT YOU ARE
                                                                    ;; POINTING TO (THE "QAUX") IS NOT A FORM OF "BE",
        (setmvb *pt*)                                                 ;; THEN FAIL BECAUSE OF THE UNGRAMATICALITY OF THE
        (GO REV)                                                    ;; CONSTRUCTION OF "BE"'S OTHERWISE MARK IT AS THE
                                                                    ;; MVB AND PREPARE TO RETURN

    HAVE
        (move-pt 'C 'DLC)
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
        (pop* 'VB)                                                    ;; POP OFF EVERY THING UNTILL YOU REACH A VERB
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
                    (isq (move-pt 'C 'U) 'IMPER)                       ;; MOVE PT TO THE CLAUSE REMEMBER THAT THE POINTER
                    (isq *pt* 'THERE)                                  ;; STAYS WHERE IT'S PUT UNTILL RETURNING FROM A
                    (isq *pt* 'RSNG))                                  ;; CALL TO PARSE
                (GO NAUX)
            (set! *pt* (getr (move-pt 'C 'U) 'SUBJECT)) *pt*               ;; "SUBJECT" IS THE SYNTACTIC SUBJECT OF THE CLAUSE THAT THE VG IS IN,
            :else (bug! 'vg "NO SUBJECT TO CHECK FOR AGREEMENT"))   ;; WHOSE ESSENTIAL DISTINGUISHING FEATURE IS AGREEMENT WITH THE VERB

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
        (setmvb (or (move-pt 'H 'PV '(MVB)) *mvb*))
        (| (and (cq 'NAUX) (isq (move-pt 'H 'PV '(VB)) 'AUX) (not (move-pt 'PV 'PV '(VB))))
            (NAUX)
            RETSM)                                                  ;; THE VB MAY HAVE THE FEATURE "NAUX" WHICH
                                                                    ;; INDICATES THAT IT CAN NEVER SERVE AS THE
                                                                    ;; AUXILLIARY OF ANOTHER VERB. IF THE PRESENT
                                                                    ;; PARSING REQUIRES IT TO THEN IT FAILS WE CHECK
                                                                    ;; BY SEEING IF THE VG CONTAINS ONLY ONE VERB,
                                                                    ;; WHICH IS AN AUX.

    POPV
        (bug! 'vg "POPV")

    RETSM
        (| (smvg) RETURN FAIL))

(§ grammar! PREPG nil

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
        (move-pt 'H)
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
        (| (isq (move-pt 'U) 'CLAUSE) nil (PREP-WHICH))               ;; IF THE NEXT WORD IS A RELWORD, SUCH AS "WHICH"
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
        (| (isq *pt* 'PRONREL) nil PRONREL)                            ;; CHANGES ITS REQUEST FROM (PARSE PREPG Q) TO
        (set! *mes* (cdr *mes*))                                        ;; (PARSE CLAUSE RSQ PREPREL), WHICH SHOULD PICK
                                                                    ;; UP THE BOTHERSOME PREPG AS AN INITIAL MODIFIER
                                                                    ;; TO THE CLAUSE AND DEAL WITH IT APPROPRIATELY
                                                                    ;; RESET THE FAILURE MESSAGE LIST (WE KNOW TO DO
        (GO P-RELWRD)                                               ;; THIS BECAUSE THE "PRONREL" AS AN INITIAL
                                                                    ;; FEATURE OF THE CLAUSE IMPLICATES THE PASSAGE OF
                                                                    ;; THE PROS CESS DESCRIBED ABOVE)

    PRONREL
        (remove-f 'REL-NOT-FOUND *pt*)
        (add-f 'PRONREL *pt*)

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
        (or (isq (move-pt 'C 'U) 'REL-NOT-FOUND)
            (isq (getr *pt* 'QUESTION-ELEMENT) 'QADJ)
            (GO FAIL))
        (remove-f 'REL-NOT-FOUND *pt*)
        (add-f 'PREPREL *pt*)
        (setr *c* 'OBJ1 (getr (move-pt 'C 'U) 'RELHEAD))

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
                (move-pt 'H 'H 'PV '(QUEST))))                          ;; COMPONENT FOR THE FEATURE "QUEST"
            (fq! 'QUEST))
        (| (smadjg-prepg) RETURN FAIL))

(§ grammar! ADJG nil

    ENTERING-ADJG                                                   ;; THIS LABEL IS MARKED BY DEBUGGING ROUTINES AND

    COMPCHECK                                                       ;; IS USEFUL FOR FOLLOWING THE FLOW OF CONTROL
        (| (and (move-pt 'C 'U '(BE)) (not (cq 'COMP))) FAIL nil)       ;; CONDITIONS WHICH MUST BE MET BY ANY ADJECTIVE
                                                                    ;; GROUP IF THERE IS A FORM OF "BE" IN THE HIGHER
                                                                    ;; CLAUSE, THEN THE ADJG SHOULD HAVE BEEN CALLED
                                                                    ;; WITH THE FEATURE "COMP" FOR COMPLIMENT

        ;; EXAMINE THE INITIAL FEATURES (THOSE DESIGNATED BY THE
        ;; CALLING PROGRAM) ALSO EXAMINE THE NEXT WORD - THESE GIVE
        ;; CLUES AND CONSTRAINTS TO THE STRUCTURE TRYING TO BE PARSED
        ;; AND DIRECT JUMPS TO THE APPROPRIATE SECTIONS OF CODE

        (| (isq (move-pt 'C 'U) 'THAN) nil DISP)                       ;; THE WORD "THAN" WAS DETECTED BY THE IMMEDIATELY
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
        (pop*)
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
        (pop*)                                                       ;; IF THE CUT POINT WAS HIT HAVING ONLY PARSED
        (GO ADJ)                                                    ;; ADVERBS, POP OFF THE FINAL ADV AND TRY TO
                                                                    ;; REPARSE IT AS AN ADJECTIVE

    ;; FINAL CHECKS ON COMPARATIVES (SEMANTIC AND OTHERWISE)

    RETSM
        (| (cq 'THANNEED) (THANNEED) nil)                            ;; IF ONE OF THE WORDS PARSED REQUIRED A "THAN",
        (| (smadjg-prepg) RETURN (SMADJ)))                 ;; FAIL IF ONE WAS NOT FOUND.

(defn- conjo []
    (binding [*end* *cut*]
        (let [a (apply-grammar 'CONJOIN)]
            (when a (set! *re* a)))))

(defn- comma []
    (let [? (conjo)]
        (cond ? ?                                                    ;; IF COMMA IS PART OF CONJOINED STRUCTURE, GREAT
            (isq *re* 'INIT) (do (flushme) true))))                    ;; IF COMMA FOLLOWS INITIAL-TYPE PHRASE, FLUSH IT AND CONTINUE DIRECT ADDRESS JAZZ

(§ grammar! CONJOIN [*prev* nil]

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
            (move-ptw 'N 'NW '(= (word *ptw*) *prev*))
            (cut *ptw*))
        (and (or (= *prev* 'BUT) (= (cadr *prev*) 'BUT))
            (nextword? *n* 'NOT)                                        ;; CHECK FOR BUT-NOT COMBINATION
            (or (flushme) (GO LOSE2))
            (fq! 'NEGBUT))
        (| (cond (memq (car *rest*) '(ADJ NUM NOUN PREP VB ADV))
                (parse3 (concat *rest* '(COMPONENT)) nil)
            (memq (car *rest*) '(NG PREPG ADJG))
                (and (not (cq 'OFOBJ)) (parse2 (concat *rest* '(COMPONENT)) nil))
            (= (car *rest*) 'CLAUSE)
                (let [*lastsent* (if (isq *h* 'MAJOR) *h* *lastsent*) auxfe (meet (features *h*) '(DECLAR IMPER))]
                    (and (parse2 (concat *rest* auxfe '(COMPONENT)) nil)
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
                    (do (move-pt 'H) (trnsf 'NPL 'NS 'MASS 'NFS)))
            (cq 'VB)
                (let [common (getprop 'VB 'ELIM)]
                    (dorun (map* #(SETQ common (meet common (features %))) *h*))
                    (feset *c* (union common (features *c*)))))
        (| (smconj *h*) RETURN (CONJOINß)))             ;; THEN MARK AS A LIST

(defn- cantake [num type feature]
    (let [vbfeat (features *mvb*)]
        (cond (memq 'RSNG type)
                (memq (symbol* (concat (cond (memq 'TO type) '(T O) (memq 'ING type) '(I N G) (memq 'REPORT type) '(R E P)) '(O B) (list (if (== num 1) '\1 '\2)))) vbfeat)
            (memq 'COMP type) (memq 'INT vbfeat)
            (memq 'NG type) (if (== num 1) (meet '(TRANS TRANS2 TRANSL TRANSINT) vbfeat) (memq 'TRANS2 vbfeat))
            :else (memq feature vbfeat))))

(defn- canparse [num type feature]
    (and (cantake num type feature)
        (or (nil? type)
            (let [[reg a]
                    (if (memq 'COMP type) ['COMP nil] (let [reg (cond (or (memq 'LOC type) (memq 'PLACE type)) 'LOBJ (== num 1) 'OBJ1 :else 'OBJ2)] [reg (list 'OBJ reg)]))]
                (and (apply parse (concat type a)) (setr *c* reg *h*)))
        (or (nil? feature) (f! feature)))))

#_(ns shrdlu.dictio)

;; ###########################################################
;;
;;                          WORDS
;;
;; ###########################################################

(putprop! '\, 'FEATURES ['SPECIAL])
(putprop! '\, 'SPECIAL '(comma))

(putprop! 'A 'FEATURES ['DET 'NS 'INDEF])
(putprop! 'A 'SEMANTICS [['DET true]])

(putprop! 'ABOVE 'FEATURES ['PREP 'PLACE])
(putprop! 'ABOVE 'SEMANTICS [['PREP '(!loc '!ABOVE true)]])

(putprop! 'AFTER 'FEATURES ['BINDER 'TIME])
(putprop! 'AFTER 'SEMANTICS [['BINDER '(smbinder *tss* *end* nil)]])

(putprop! 'ALL 'FEATURES ['DET 'NPL 'QNTFR])
(putprop! 'ALL 'SEMANTICS [['DET '(cond (cq 'OF) 'ALL (meet '(NUM DEF) *fe*) 'DEF :else 'NDET)]])

(putprop! 'AN 'IRREGULAR '(A nil nil))

(putprop! 'AND 'FEATURES ['SPECIAL])
(putprop! 'AND 'SEMANTICS true)
(putprop! 'AND 'SPECIAL '(conjo))

(putprop! 'ANY 'FEATURES ['DET 'ANY 'NS 'NPL 'QNTFR])
(putprop! 'ANY 'SEMANTICS [['DET 'INDEF]])

(putprop! 'ANYTHING 'FEATURES ['TPRON 'ANY 'NS])
(putprop! 'ANYTHING 'SEMANTICS [['TPRON 'INDEF]])

(putprop! 'ARE 'IRREGULAR '(BE (VPL PRESENT) (INF)))

(putprop! 'AS 'FEATURES ['AS])
(putprop! 'AS 'SEMANTICS [['NULL true]])

(putprop! 'ASK 'FEATURES ['VB 'TRANS 'INF 'SUBTOB])
(putprop! 'ASK 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!EVENT))) :markers '(!EVENT) :procedure '((!WANT *!1* *!2* *TIME))])]]]])

(putprop! 'AT 'FEATURES ['AT])
(putprop! 'AT 'SEMANTICS [['NUMD true]])

(putprop! 'AWAY 'FEATURES ['PRT])
(putprop! 'AWAY 'SEMANTICS [['PRT true]])

(putprop! 'BACK 'FEATURES ['NOUN 'NS 'PREP2])
(putprop! 'BACK 'SEMANTICS [['PREP2 true] ['NOUN true]])

(putprop! 'BALL 'FEATURES ['NOUN 'NS])
(putprop! 'BALL 'SEMANTICS [['NOUN '(object [:markers '(!MANIP !ROUND) :procedure '((!IS *** !BALL))])]])

(putprop! 'BE 'FEATURES ['INT 'AUX 'VB 'BE 'INF])
(putprop! 'BE 'SEMANTICS [['VB [['THERE '(!bethere)] ['INT '(!beint)]]]])

(defn- !bethere []
    (relation [:restrictions '(((!THING) (= (quantifier? *smsub*) 'INDEF))) :procedure nil]))

(defn- !beint []
    (or (relation
            [:restrictions '(((!PHYSOB)) (*smcomp* (!PROPERTY)))
                :procedure '(!EVAL (let [prop (meet (getprop '!PROPERTY 'SYSTEM) (markers? *smcomp*))] (if prop (list (list (car prop) '*!1* '*!2*)) (list '(*!2* *!1*)))))]
            [:restrictions '(((!THING)) (*smcomp* (!SYSTEMS) (and (not (refer? *smcomp*)) (= (rel? *smcomp*) *smsub*))))
                :procedure '(!EVAL (relations? *smcomp*))]
            [:restrictions '(((!THING)) (*smcomp* (!THING) (refer? *smcomp*)))
                :procedure '((!EVAL (list 'thamong '*!1* (quotify (refer? *!2*)))))])
        (oops! "SORRY, I DON'T UNDERSTAND THE VERB BE, WHEN YOU USE IT LIKE THAT.")))

(putprop! 'BEFORE 'FEATURES ['BINDER 'TIME])
(putprop! 'BEFORE 'SEMANTICS [['BINDER '(smbinder *tss* nil *start*)]])

(putprop! 'BEGIN 'FEATURES ['VB 'TRANS 'INF 'TOOB 'INGOB 'ITRNS])
(putprop! 'BEGIN 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!EVENT))) :markers '(!EVENT) :procedure '((!START *!2* *TIME))])]
                                   ['ITRNS '(relation [:restrictions '(((!ANIMATE))) :markers '(!EVENT) :procedure '((!START EE *TIME))])]]]])

(putprop! 'BEGAN 'IRREGULAR '(BEGIN (PAST) (INF)))

(putprop! 'BEHIND 'FEATURES ['PREP 'PLACE])
(putprop! 'BEHIND 'SEMANTICS [['PREP '(!loc '!BEHIND true)]])

(putprop! 'BELOW 'FEATURES ['PREP 'PLACE])
(putprop! 'BELOW 'SEMANTICS [['PREP '(!loc '!ABOVE false)]])

(putprop! 'BENEATH 'FEATURES ['PREP 'PLACE])
(putprop! 'BENEATH 'SEMANTICS [['PREP '(!loc '!ABOVE false)]])

(putprop! 'BESIDE 'FEATURES ['PREP 'PLACE])
(putprop! 'BESIDE 'SEMANTICS [['PREP '(relation [:restrictions '(((!PHYSOB)) ((!PHYSOB))) :procedure '((!NEXTO *!1* *!2* *TIME))])]])

(putprop! 'BIG 'FEATURES ['ADJ])
(putprop! 'BIG 'SEMANTICS [['MEASURE '(measure :dimension '!SIZE :restrictions '(!PHYSOB) :direction true)]
                            ['ADJ '(object [:markers '(!PHYSOB !BIG) :procedure '((!MORE !SIZE *** (128 128 128)))])]])

(putprop! 'BLACK 'FEATURES ['ADJ])
(putprop! 'BLACK 'SEMANTICS [['ADJ '(!color '!BLACK)]])

(putprop! 'BLOCK 'FEATURES ['NOUN 'NS])
(putprop! 'BLOCK 'SEMANTICS [['NOUN '(object [:markers '(!MANIP !RECTANGULAR) :procedure '((!IS *** !BLOCK))])]])

(putprop! 'BLUE 'FEATURES ['ADJ])
(putprop! 'BLUE 'SEMANTICS [['ADJ '(!color '!BLUE)]])

(putprop! 'BOTH 'FEATURES ['B-SPECIAL 'QNTFR 'DET 'DEF 'NPL 'BOTH])
(putprop! 'BOTH 'SEMANTICS [['DET 'DEF]])
(putprop! 'BOTH 'B-SPECIAL '(both 'AND))

(defn- both [& a]
    ;; HANDLES (BOTH AND) (EITHER OR) (NEITHER NOR) COMBINATIONS.
    ;; THE CONJOIN PROGRAM DOES SPECIAL THINGS WHEN BOTH IS SET.
    (binding [*end* *cut* *cut* nil *both* nil] (let [nbb *n*]              ;; MAKE END OUT OF PREVIOUS CUT POINT
        (if (and (flushme)
                (move-ptw 'N 'NW '(= (word *ptw*) (car a)) 'NW)             ;; LOOK FOR THE MATCHING WORD E.G. AND, OR, NOR
                (cut *end*)
                (set! *both* *ptw*)                                         ;; SAVE POINTER TO THE WORD AFTER THE MATCHING WORD
                (set! *re* (cond
                    (memq (car *rest*) '(PREP ADV)) (parse3 *rest* true)
                    (memq (car *rest*) '(NG PREPG ADJG CLAUSE)) (parse2 *rest* true)))
                (< (count *n*) (count *both*)))                             ;; FAIL UNLESS WE PARSED BEYOND MATCHING WORD
            (set! *special* 'SKIP)
            (do (set! *re* nil) (set! *n* nbb) nil)))))

(putprop! 'BOX 'FEATURES ['NOUN 'NS])
(putprop! 'BOX 'SEMANTICS [['NOUN '(object [:markers '(!BOX) :procedure '((!IS *** !BOX))])]])

(putprop! 'BRICK 'FEATURES ['NOUN 'NS])

(putprop! 'BUILD 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'BUILD 'SEMANTICS [['VB [['TRANS '(!build)]]]])

(putprop! 'BUT 'FEATURES ['SPECIAL])
(putprop! 'BUT 'SEMANTICS true)
(putprop! 'BUT 'SPECIAL '(conjo))

(putprop! 'BY 'FEATURES ['PREP])
(putprop! 'BY 'SEMANTICS [['PREP '(relation [:restrictions '(((!PHYSOB)) ((!PHYSOB))) :procedure '((!NEXTO *!1* *!2* *TIME))])]])

(putprop! 'CALL 'FEATURES ['VB 'INF 'TRANS2])
(putprop! 'CALL 'SEMANTICS [['VB [['TRANS2 '(relation [:restrictions '(((!ANIMATE)) ((!THING)) ((!name))) :procedure '((!CALL *!2* *!3* *TIME))])]]]])

(putprop! 'CAN 'FEATURES ['V3PS 'VFS 'VPL 'VB 'MODAL 'AUX])
(putprop! 'CAN 'SEMANTICS [['VB true]])

(putprop! 'CHOOSE 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'CHOOSE 'SEMANTICS [['VB [['TRANS '(!notice)]]]])

(putprop! 'CLEAN 'FEATURES ['VB 'INF 'VPRT 'TRANS])
(putprop! 'CLEAN 'SEMANTICS [['VB true]])

(putprop! 'CLEAN-OFF 'ROOT '(CLEAN OFF))
(putprop! 'CLEAN-OFF 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'CLEAN-OFF 'SEMANTICS [['TRANS '(!cleanoff)]])

(putprop! 'CLEAN-OUT 'ROOT '(CLEAN OUT))
(putprop! 'CLEAN-OUT 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'CLEAN-OUT 'SEMANTICS [['TRANS '(!cleanoff)]])

(putprop! 'CLEAN-UP 'ROOT '(CLEAN UP))
(putprop! 'CLEAN-UP 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'CLEAN-UP 'SEMANTICS [['TRANS '(!cleanoff)]])

(putprop! 'CLEAR 'FEATURES ['VB 'INF 'VPRT 'TRANS])
(putprop! 'CLEAR 'SEMANTICS [['VB true]])

(putprop! 'CLEAR-OFF 'ROOT '(CLEAR OFF))
(putprop! 'CLEAR-OFF 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'CLEAR-OFF 'SEMANTICS [['TRANS '(!cleanoff)]])

(putprop! 'CLEAR-OUT 'ROOT '(CLEAR OUT))
(putprop! 'CLEAR-OUT 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'CLEAR-OUT 'SEMANTICS [['TRANS '(!cleanoff)]])

(putprop! 'COLOR 'FEATURES ['NOUN 'NS])
(putprop! 'COLOR 'SEMANTICS [['NOUN '(object [:markers '(!COLOR) :procedure '((!IS *** !COLOR))])]])

(putprop! 'CONSTRUCT 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'CONSTRUCT 'SEMANTICS [['VB [['TRANS '(!build)]]]])

(putprop! 'CONTAIN 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'CONTAIN 'SEMANTICS [['VB [['TRANS
    '(relation
        [:restrictions '(((!BOX)) ((!PHYSOB))) :procedure '((!CONTAIN *!1* *!2* *TIME))]
        [:restrictions '(((!CONSTRUCT)) ((!THING))) :procedure '((!PART *!2* *!1* *TIME))])]]]])

(putprop! 'CONTAINER 'FEATURES ['NOUN 'NS])
(putprop! 'CONTAINER 'SEMANTICS [['NOUN '(object [:markers '(!BOX) :procedure '((!IS *** !BOX))])]])

(putprop! 'CORNER 'FEATURES ['NOUN 'NS])

(putprop! 'CUBE 'FEATURES ['NOUN 'NS])
(putprop! 'CUBE 'SEMANTICS [['NOUN '(object [:markers '(!MANIP !RECTANGULAR) :procedure '((!IS *** !BLOCK) (!eqdim ***))])]])

(putprop! 'DID 'IRREGULAR '(DO (PAST V3PS) (INF PRESENT)))

(putprop! 'DO 'FEATURES ['TRANS 'VFS 'PRESENT 'VPL 'VB 'AUX 'DO 'INF])
(putprop! 'DO 'SEMANTICS [['VB [['TRANS
    '(relation [:restrictions '(((!ANIMATE)) ((!EVENT))) :markers '(!EVENT) :procedure '(!EVAL (or (getprop MAP2 'REFER) (bug! 'do "DEFINITION")))])]]]])

(putprop! 'DOES 'IRREGULAR '(DO (V3PS) (VFS VPL INF)))

(putprop! 'DOWN 'FEATURES ['PRT])
(putprop! 'DOWN 'SEMANTICS [['PRT true]])

(putprop! 'DROP 'FEATURES ['TRANSL 'TRANSL2 'VB 'INF 'TRANS])
(putprop! 'DROP 'SEMANTICS [['VB
    [['TRANSL '(relation [:restrictions '(((!ANIMATE)) ((!MANIP)) (*smobl* (!PLACE *TIME))) :procedure '((!DROP *!1* *!2* *!3*)) :markers '((!MOTION))])]
     ['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!PHYSOB))) :markers '(!EVENT) :procedure '((!DROP *!1* *!2* PLACE *TIME)) :markers '((!MOTION))])]]]])

(putprop! 'EACH 'FEATURES ['DET 'NS 'QNTFR])
(putprop! 'EACH 'SEMANTICS [['DET 'ALL]])

(putprop! 'EITHER 'FEATURES ['B-SPECIAL])
(putprop! 'EITHER 'SEMANTICS true)
(putprop! 'EITHER 'B-SPECIAL '(both 'OR))

(putprop! 'EVERY 'FEATURES ['DET 'NS 'QNTFR])
(putprop! 'EVERY 'SEMANTICS [['DET 'ALL]])

(putprop! 'EVERYTHING 'FEATURES ['TPRON 'NS])
(putprop! 'EVERYTHING 'SEMANTICS [['TPRON 'ALL]])

(putprop! 'EXACTLY 'FEATURES ['NUMD 'NUMDALONE])
(putprop! 'EXACTLY 'SEMANTICS [['NUMD '(list 'EXACTLY *num*)]])

(putprop! 'FEW 'FEATURES ['NUMD 'NUMDAS])
(putprop! 'FEW 'SEMANTICS [['NUMD '(list '< (inc *num*))]])

(putprop! 'FEWER 'FEATURES ['NUMD 'NUMDAN])
(putprop! 'FEWER 'SEMANTICS [['NUMD '(list '< *num*)]])

(putprop! 'FIND 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'FIND 'SEMANTICS [['VB [['TRANS '(!notice)]]]])

(putprop! 'FINISH 'FEATURES ['VB 'INF 'TRANS 'INFOB])
(putprop! 'FINISH 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!EVENT))) :markers '(!EVENT) :procedure '((!END *!2* *TIME))])]]]])

(putprop! 'FIVE 'FEATURES ['NUM])
(putprop! 'FIVE 'SEMANTICS [['NUM 5]])

(putprop! 'FOUR 'FEATURES ['NUM])
(putprop! 'FOUR 'SEMANTICS [['NUM 4]])

(putprop! 'FRIEND 'FEATURES ['NOUN 'NS])
(putprop! 'FRIEND 'REFER 'ßFRIEND)

(putprop! 'FROM 'FEATURES ['PREP])

(putprop! 'FRONT 'FEATURES ['NOUN 'NS 'PREP2])
(putprop! 'FRONT 'SEMANTICS [['NOUN true] ['PREP2 true]])

(putprop! 'GAVE 'IRREGULAR '(GIVE (PAST) (INF)))

(putprop! 'GIVE 'FEATURES ['VB 'INF 'TRANS2])
(putprop! 'GIVE 'SEMANTICS [['VB [['TRANS2 '(relation [:restrictions '(((!ANIMATE)) ((!ANIMATE)) ((!PHYSOB))) :markers '(!EVENT) :procedure '((!GIVE *!1* *!2* *!3* *TIME))])]]]])

(putprop! 'GO 'FEATURES ['ITRNS 'VB 'INF])

(putprop! 'GOING 'FEATURES ['VB 'ITRNS 'ING])

(putprop! 'GRAB 'FEATURES ['VB 'TRANS 'INF])
(putprop! 'GRAB 'SEMANTICS [['VB [['TRANS '(!grasp)]]]])

(putprop! 'GRASP 'FEATURES ['VB 'TRANS 'INF])
(putprop! 'GRASP 'SEMANTICS [['VB [['TRANS '(!grasp)]]]])

(putprop! 'GREATER 'FEATURES ['NUMD 'NUMDAN])
(putprop! 'GREATER 'SEMANTICS [['NUMD '(list '> *num*)]])

(putprop! 'GREEN 'FEATURES ['ADJ])
(putprop! 'GREEN 'SEMANTICS [['ADJ '(!color '!GREEN)]])

(putprop! 'HAD 'IRREGULAR '(HAVE (PAST) (INF)))

(putprop! 'HAND 'FEATURES ['NOUN 'NS])
(putprop! 'HAND 'SEMANTICS [['NOUN '(object [:markers '(!HAND) :procedure '((!IS *** !HAND))])]])

(putprop! 'HANDLE 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'HANDLE 'SEMANTICS [['VB [['TRANS '(!grasp)]]]])

(putprop! 'HAS 'IRREGULAR '(HAVE (V3PS PRESENT) (INF)))

(putprop! 'HAVE 'FEATURES ['HAVE 'VB 'AUX 'INF 'TRANS])
(putprop! 'HAVE 'SEMANTICS [['VB [['TRANS '(!have)]]]])

(putprop! 'HIGH 'FEATURES ['ADJ])
(putprop! 'HIGH 'SEMANTICS [['MEASURE '(measure :dimension '!HEIGHT :restrictions '(!PHYSOB) :direction true)]
                             ['ADJ '(object [:markers '(!PHYSOB) :procedure '((!HIGH ***))])]])

(putprop! 'HOLD 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'HOLD 'SEMANTICS [['VB [['TRANS
    '(relation
        [:restrictions '(((!HAND)) ((!MANIP))) :procedure '((!GRASPING *!2* *TIME))]
        [:restrictions '(((!ANIMATE)) ((!MANIP))) :procedure '((!GRASPING *!2* *TIME))])]]]])

(putprop! 'HE 'FEATURES ['PRON 'NS 'SUBJ])

(putprop! 'HER 'IRREGULAR '(SHE (OBJ POSS) (SUBJ)))

(putprop! 'HIM 'IRREGULAR '(HE (OBJ) (SUBJ)))

(putprop! 'HIS 'FEATURES ['PRON 'POSS])

(putprop! 'HOW 'FEATURES ['QADJ])
(putprop! 'HOW 'SEMANTICS [['QADJ true]])

(putprop! 'HOWEVER 'FEATURES ['PRON 'EVERPRON])

(putprop! 'I 'FEATURES ['SUBJ 'PRON 'NFS])
(putprop! 'I 'SEMANTICS [['PRON '(smset (list (newcopy 'FRIEND-OSS)))]])

(putprop! 'IF 'FEATURES ['BINDER])

(putprop! 'IN 'FEATURES ['ADV 'PLACE 'PREP 'PLACE])
(putprop! 'IN 'SEMANTICS [['PREP '(!in)]])

(putprop! 'IN-BACK-OF 'ROOT '(IN BACK OF))
(putprop! 'IN-BACK-OF 'FEATURES ['PREP 'COMBINATION])
(putprop! 'IN-BACK-OF 'SEMANTICS '(!loc '!BEHIND true))

(putprop! 'IN-FRONT-OF 'ROOT '(IN FRONT OF))
(putprop! 'IN-FRONT-OF 'FEATURES ['PREP 'COMBINATION])
(putprop! 'IN-FRONT-OF 'SEMANTICS '(!loc '!BEHIND false))

(putprop! 'INSIDE 'FEATURES ['PREP 'PLACE])
(putprop! 'INSIDE 'SEMANTICS [['PREP '(!in)]])

(putprop! 'INSIDE-OF 'ROOT '(INSIDE OF))
(putprop! 'INSIDE-OF 'FEATURES ['PREP 'COMBINATION])
(putprop! 'INSIDE-OF 'SEMANTICS '(!in))

(putprop! 'INTO 'FEATURES ['PREP 'PLACE])
(putprop! 'INTO 'SEMANTICS [['PREP '(!in)]])

(putprop! 'IS 'IRREGULAR '(BE (V3PS PRESENT) (INF)))

(putprop! 'IT 'FEATURES ['PRON 'NS 'SUBJ 'OBJ])
(putprop! 'IT 'SEMANTICS [['PRON '(smit 'IT)]])

(putprop! 'ITS 'IRREGULAR '(IT (POSS) nil))

(putprop! 'KNOW 'FEATURES ['VB 'INF 'TRANS 'REPOB])

(putprop! 'LARGE 'FEATURES ['ADJ])
(putprop! 'LARGE 'SEMANTICS [['MEASURE '(measure :dimension '!SIZE :restrictions '(!PHYSOB) :direction true)]
                              ['ADJ '(object [:markers '(!PHYSOB !BIG) :procedure '((!MORE !SIZE *** (128 128 128)))])]])

(putprop! 'LEAST 'FEATURES ['NUMD 'NUMDAT])
(putprop! 'LEAST 'SEMANTICS [['NUMD '(list '> (dec *num*))]])

(putprop! 'LEFT 'FEATURES ['NOUN 'NS])
(putprop! 'LEFT 'SEMANTICS [['NOUN '(object [:markers '(!DIRECTION) :procedure '((!DIRECTION !RIGHT nil))])]])

(putprop! 'LESS 'FEATURES ['NUMD 'NUMDAN])
(putprop! 'LESS 'SEMANTICS [['NUMD '(list '< *num*)]])

(putprop! 'LIKE 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'LIKE 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!THING))) :procedure '((!LIKE *!1* *!2*))])]]]])

(putprop! 'LIST 'FEATURES ['VB 'VO 'TRANS])
(putprop! 'LIST 'SEMANTICS [['VB [['TRANS '(!name)]]]])

(putprop! 'LITTLE 'FEATURES ['ADJ])
(putprop! 'LITTLE 'SEMANTICS [['MEASURE '(measure :dimension '!SIZE :restrictions '(!PHYSOB) :direction nil)]
                               ['ADJ '(object [:markers '(!PHYSOB !LITTLE) :procedure '((!MORE !SIZE (128 128 128) ***))])]])

(putprop! 'LONG 'FEATURES ['ADJ])
(putprop! 'LONG 'SEMANTICS [['MEASURE '(measure :dimension '!LENGTH :restrictions '(!PHYSOB) :direction true)]
                             ['ADJ '(object [:markers '(!PHYSOB) :procedure '((!MORE !LENGTH *** (128 128 128)))])]])

(putprop! 'MAKE 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'MAKE 'SEMANTICS [['VB [['TRANS '(!build)]]]])

(putprop! 'MANY 'FEATURES ['DET 'QNTFR 'NPL 'NONUM 'NUMD 'NUMDAS])
(putprop! 'MANY 'SEMANTICS [['NUMD '(list '> (dec *num*))] ['DET true]])

(putprop! 'ME 'IRREGULAR '(I (OBJ) (SUBJ)))

(putprop! 'MORE 'FEATURES ['NUMD 'NUMDAN])
(putprop! 'MORE 'SEMANTICS [['NUMD '(list '> *num*)]])

(putprop! 'MOST 'FEATURES ['NUMD 'NUMDAT 'DET 'QNTFR 'NPL 'NONUM])
(putprop! 'MOST 'SEMANTICS [['NUMD '(list '< (inc *num*))]])

(putprop! 'MOVE 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'MOVE 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!PHYSOB))) :procedure '((!PUT *!2* PLACE *TIME)) :markers '((!MOTION))])]]]])

(putprop! 'MY 'IRREGULAR '(I (POSS) (SUBJ)))

(putprop! 'NAME 'FEATURES ['NOUN 'NS 'VB 'INF 'TRANS])
(putprop! 'NAME 'SEMANTICS [['NOUN '(object ['(!NAME !ROLE) '((IS *** !NAME) (!CALL ? ***) (!role (!THING) (!CALL *!2* *!1*)))])]
                             ['VB [['TRANS '(!name)]]]])

(putprop! 'NARROW 'FEATURES ['ADJ])
(putprop! 'NARROW 'SEMANTICS [['ADJ '(object [:markers '(!PHYSOB) :procedure '((!MORE !WIDTH (128 0 0) ***))])]
                               ['MEASURE '(measure :dimension '!WIDTH :restrictions '(!PHSYOB) :direction nil)]])

(putprop! 'NEITHER 'FEATURES ['B-SPECIAL])
(putprop! 'NEITHER 'SEMANTICS true)
(putprop! 'NEITHER 'B-SPECIAL '(both 'NOR))

(putprop! 'NICE 'FEATURES ['ADJ])
(putprop! 'NICE 'SEMANTICS [['ADJ '(object [:markers '(!THING) :procedure '((!LIKE ßFRIEND ***))])]])

(putprop! 'NO 'FEATURES ['DET 'QNTFR 'NS 'NPL])
(putprop! 'NO 'SEMANTICS [['DET 'NO]])

(putprop! 'NONE 'FEATURES ['DET 'QNTFR 'NPL 'NS 'NONUM])
(putprop! 'NONE 'SEMANTICS [['DET 'NO]])

(putprop! 'NOR 'FEATURES ['SPECIAL])
(putprop! 'NOR 'SEMANTICS true)
(putprop! 'NOR 'SPECIAL '(conjo))

(putprop! 'NOT 'FEATURES ['ADV 'NEG])
(putprop! 'NOT 'SEMANTICS [['ADV true]])

(putprop! 'NOTHING 'FEATURES ['TPRON 'NEG 'NS])
(putprop! 'NOTHING 'SEMANTICS [['TPRON 'NO]])

(putprop! 'NOW 'FEATURES ['ADV 'TIMW])
(putprop! 'NOW 'SEMANTICS [['ADV '(or (= (cadr (assq 'TIME *fe*)) 'ßNOW) (bug! 'now "DEFINITION"))]])

(putprop! 'OBJECT 'FEATURES ['NOUN 'NS])
(putprop! 'OBJECT 'SEMANTICS [['NOUN '(object [:markers '(!PHYSOB !VAGUE) :procedure '((!PHYSOB ***))])]])

(putprop! 'OF 'FEATURES ['PREP 'PREP2 'OF])
(putprop! 'OF 'SEMANTICS [['PREP '(and (cq 'NG)
                                (relation
                                    [:restrictions '(((!DIRECTION)) ((!PHYSOB)))
                                        :procedure '((!EVAL (let [a (or (assq '!DIRECTION (cddaar (§ INTERP MAP1))) (bug! 'of "DEFINITION"))]
                                                        (list '!DIRECTION (cadr a) (if (caddr a) '*OF '*!2*) (if (caddr a) '*!2* '*OF) '*TIME))))]))]
                           ['PREP2 true]])

(putprop! 'OFF 'FEATURES ['PRT])
(putprop! 'OFF 'SEMANTICS [['PRT true]])

(putprop! 'ON 'FEATURES ['PREP 'PLACE])
(putprop! 'ON 'SEMANTICS [['PREP '(!on)]])

(putprop! 'ON-TOP-OF 'ROOT '(ON TOP OF))
(putprop! 'ON-TOP-OF 'FEATURES ['PREP 'COMBINATION])
(putprop! 'ON-TOP-OF 'SEMANTICS '(!on))

(putprop! 'ONE 'FEATURES ['NUM 'NOUN 'NS])
(putprop! 'ONE 'SEMANTICS [['NOUN '(smone)] ['NUM 1]])

(putprop! 'ONLY 'FEATURES ['NUMD 'NUMDALONE])
(putprop! 'ONLY 'SEMANTICS [['NUMD '(list 'EXACTLY *num*)]])

(putprop! 'ONTO 'FEATURES ['PREP 'PLACE])
(putprop! 'ONTO 'SEMANTICS [['PREP '(!on)]])

(putprop! 'OR 'FEATURES ['SPECIAL])
(putprop! 'OR 'SEMANTICS true)
(putprop! 'OR 'SPECIAL '(conjo))

(putprop! 'OUT 'FEATURES ['PRT])
(putprop! 'OUT 'SEMANTICS [['PRT true]])

(putprop! 'OUT-OF 'ROOT '(OUT OF))
(putprop! 'OUT-OF 'FEATURES ['PREP 'COMBINATION])
(putprop! 'OUT-OF 'SEMANTICS '(!OUTOF))

(putprop! 'OVER 'FEATURES ['PREP 'PLACE])
(putprop! 'OVER 'SEMANTICS [['PREP '(!loc '!ABOVE true)]])

(putprop! 'PICK 'FEATURES ['VPRT 'VB 'INF 'TRANS])
(putprop! 'PICK 'SEMANTICS [['VB [['TRANS '(!notice)]]]])

(putprop! 'PICK-UP 'ROOT '(PICK UP))
(putprop! 'PICK-UP 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'PICK-UP 'SEMANTICS [['TRANS
    '(relation
        [:restrictions '(((!ANIMATE)) ((!MANIP)))
            :markers '(!EVENT)
            :procedure '((!EVAL (if (memq (num? *smob1*) '(1 NS)) '(!PICKUP *!2* *TIME) '(!PUTIN *!2* ßBOX *TIME))))])]])

(putprop! 'PLEASE 'FEATURES ['B-SPECIAL])
(putprop! 'PLEASE 'SEMANTICS true)
(putprop! 'PLEASE 'B-SPECIAL '(flushme))

(putprop! 'POINTED 'FEATURES ['ADJ])
(putprop! 'POINTED 'SEMANTICS [['ADJ '(object [:markers '(!PHYSOB !POINTED) :procedure '((!SHAPE *** !POINTED))])]])

(putprop! 'PUT 'PAST 'PUT)
(putprop! 'PUT 'FEATURES ['INF 'PAST 'VB 'TRANSL 'VPRT])
(putprop! 'PUT 'SEMANTICS [['VB [['TRANSL
    '(relation
        [:restrictions '(((!ANIMATE)) ((!PHYSOB)) (*smobl* (!PLACE)))
            :markers '(!EVENT)
            :procedure '(!EVAL
                (doall (map #(condp = (car %)
                                '!ON (list '!PUTON '*!2* (cadr %) '*TIME)
                                '!IN (list '!PUTIN '*!2* (cadr %) '*TIME)
                                (bug! 'put "DEFINITION"))
                    (relations? *smobl*))))])]]]])

(putprop! 'PUT-AWAY 'ROOT '(PUT AWAY))
(putprop! 'PUT-AWAY 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'PUT-AWAY 'SEMANTICS [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!MANIP))) :markers '(!EVENT) :procedure '((!PUTIN *!2* ßBOX *TIME))])]])

(putprop! 'PUT-DOWN 'ROOT '(PUT DOWN))
(putprop! 'PUT-DOWN 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'PUT-DOWN 'SEMANTICS [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!MANIP))) :markers '(!EVENT) :procedure '((!PUTON *!2* ßTABLE *TIME))])]])

(putprop! 'PUT-TOGETHER 'ROOT '(PUT TOGETHER))
(putprop! 'PUT-TOGETHER 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'PUT-TOGETHER 'SEMANTICS [['TRANS '(!build)]])

(putprop! 'PYRAMID 'FEATURES ['NOUN 'NS])
(putprop! 'PYRAMID 'SEMANTICS [['NOUN '(object [:markers '(!PHYSOB !POINTED) :procedure '((!IS *** !PYRAMID))])]])

(putprop! 'RED 'FEATURES ['ADJ])
(putprop! 'RED 'SEMANTICS [['ADJ '(!color '!RED)]])

(putprop! 'RELEASE 'FEATURES ['VB 'TRANS 'INF])

(putprop! 'RIGHT 'FEATURES ['NOUN 'NS])
(putprop! 'RIGHT 'SEMANTICS [['NOUN '(object [:markers '(!DIRECTION) :procedure '((!DIRECTION !RIGHT true))])]])

(putprop! 'ROUND 'FEATURES ['ADJ])
(putprop! 'ROUND 'SEMANTICS [['ADJ '(object [:markers '(!PHYSOB !ROUND) :procedure '((!SHAPE *** !ROUND))])]])

(putprop! 'SAW 'IRREGULAR '(SEE (PAST) (INF)))

(putprop! 'SEE 'FEATURES ['VB 'INF 'TRANS])

(putprop! 'SET 'FEATURES ['VB 'INF])
(putprop! 'SET 'SEMANTICS [['VB true]])

(putprop! 'SET-DOWN 'ROOT '(SET DOWN))
(putprop! 'SET-DOWN 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'SET-DOWN 'SEMANTICS [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!MANIP))) :markers '(!EVENT) :procedure '((!PUTON *!2* ßTABLE *TIME))])]])

(putprop! 'SHAPE 'FEATURES ['NOUN 'NS])
(putprop! 'SHAPE 'SEMANTICS [['NOUN '(object [:markers '(!SHAPE) :procedure '((!IS *** !SHAPE))])]])

(putprop! 'SHE 'FEATURES ['PRON 'SUBJ 'NS])

(putprop! 'SHORT 'FEATURES ['ADJ])
(putprop! 'SHORT 'SEMANTICS [['MEASURE '(measure :dimension '!HEIGHT :restrictions '(!PHYSOB) :direction nil)]
                              ['ADJ '(object [:markers '(!PHYSOB) :procedure '((!MORE !HEIGHT (128 0 0) ***))])]])

(putprop! 'SHRDLU 'REFER 'ßSHRDLU)

(putprop! 'SINCE 'FEATURES ['BINDER 'TIME])
(putprop! 'SINCE 'SEMANTICS [['BINDER '(smbinder *tss* *end* nil)]])

(putprop! 'SIT 'FEATURES ['VB 'INF 'ITRNSL])
(putprop! 'SIT 'SEMANTICS [['VB [['ITRNSL
    '(relation
        [:restrictions '(((!PHYSOB)) (*smobl* (!PLACE)))
            :procedure '(!EVAL
                (doall (map #(if (memq (car %) '(!ON !IN)) (list '!SUPPORT (cadr %) '*!1* '*TIME) (bug! 'sit "DEFINITION"))
                    (relations? *smobl*))))])]]]])

(putprop! 'SIZE 'FEATURES ['NOUN 'NS])
(putprop! 'SIZE 'SEMANTICS [['NOUN '(object [:markers '(!SIZE) :procedure '((!IS *** !SIZE))])]])

(putprop! 'SMALL 'FEATURES ['ADJ])
(putprop! 'SMALL 'SEMANTICS [['MEASURE '(measure :dimension '!SIZE :restrictions '(!PHYSOB) :direction nil)]
                              ['ADJ '(object [:markers '(!PHYSOB !LITTLE) :procedure '((!MORE !SIZE (128 128 128) ***))])]])

(putprop! 'SOME 'FEATURES ['DET 'QNTFR 'NS 'NPL 'NONUM])
(putprop! 'SOME 'SEMANTICS [['DET 'INDEF]])

(putprop! 'SOMETHING 'FEATURES ['TPRON 'NS])
(putprop! 'SOMETHING 'SEMANTICS [['TPRON 'INDEF]])

(putprop! 'SPHERE 'FEATURES ['NOUN 'NS])

(putprop! 'SQUARE 'FEATURES ['ADJ])
(putprop! 'SQUARE 'SEMANTICS [['ADJ '(object [:markers '(!PHYSOB !RECTANGULAR) :procedure '((!SHAPE ** !RECTANGULAR))])]])

(putprop! 'STACK 'FEATURES ['NOUN 'NS 'VB 'INF 'VPRT 'TRANS])
(putprop! 'STACK 'SEMANTICS [['NOUN '(object [:markers '(!STACK) :procedure '((!IS *** !STACK))])]
                              ['VB [['TRANS '(!stackup)]]]])

(putprop! 'STACK-UP 'ROOT '(STACK UP))
(putprop! 'STACK-UP 'FEATURES ['COMBINATION 'TRANS])
(putprop! 'STACK-UP 'SEMANTICS [['TRANS '(!stackup)]])

(putprop! 'START 'FEATURES ['VB 'INF 'TRANS 'INGOB1 'TOOB1])
(putprop! 'START 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!EVENT))) :markers '(!EVENT) :procedure '((!START *!2* *TIME))])]]]])

(putprop! 'SUPPORT 'FEATURES ['VB 'INF 'TRANS 'IMPERF 'NOUN 'NS])
(putprop! 'SUPPORT 'SEMANTICS [['NOUN '(object [:markers '(!PHYSOB !ROLE) :procedure '((!SUPPORT *** ?) (!role (!PHYSOB) (!SUPPORT *!1* *!2*)))])]
                                ['VB [['TRANS '(relation [:restrictions '(((!PHYSOB)) ((!MANIP))) :procedure '((!SUPPORT *!1* *!2* *TIME))])]]]])

(putprop! 'TABLE 'FEATURES ['NOUN 'NS])
(putprop! 'TABLE 'SEMANTICS [['NOUN '(object [:markers '(!TABLE) :procedure '((!IS *** !TABLE))])]])

(putprop! 'TAKE 'FEATURES ['VB 'INF 'TRANSL 'TRANS])

(putprop! 'TALL 'FEATURES ['ADJ])
(putprop! 'TALL 'SEMANTICS [['MEASURE '(measure :dimension '!HEIGHT :restrictions '(!PHYSOB) :direction true)]
                             ['ADJ '(object [:markers '(!PHYSOB) :procedure '((!MORE !HEIGHT *** (128 0 0)))])]])

(putprop! 'TELL 'FEATURES ['VB 'INF 'TRANS2 'TOOB2])
(putprop! 'TELL 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!EVENT))) :markers '(!EVENT) :procedure '((!WANT *!1* *!2* *TIME))])]]]])

(putprop! 'THAN 'FEATURES ['THAN])
(putprop! 'THAN 'SEMANTICS [['NULL true]])

(putprop! 'THANK 'FEATURES ['B-SPECIAL])
(putprop! 'THANK 'SEMANTICS '(thank))
(putprop! 'THANK 'B-SPECIAL '(thank))

(defn- thank []
    (when (= (cadr *n*) 'YOU)
        (print "YOU'RE WELCOME")
        (flushme)
        (flushme)
        (when-not *nn* (§ ß_G))
        (set! *special* 'DONE)))

(putprop! 'THAT 'FEATURES ['NS 'THAT 'DET 'DEM 'DEF 'PRONREL 'INCOM])
(putprop! 'THAT 'SEMANTICS [['PRONREL true] ['DET '(smthat)] ['NULL true]])

(putprop! 'THE 'FEATURES ['DET 'NPL 'NS 'DEF])
(putprop! 'THE 'SEMANTICS [['DET true]])

(putprop! 'THEIR 'IRREGULAR '(THEY (POSS) nil))

(putprop! 'THEM 'IRREGULAR '(THEY (OBJ) (SUBJ)))

(def- lastime nil)

(putprop! 'THEN 'FEATURES ['ADV 'TIMW])
(putprop! 'THEN 'SEMANTICS [['ADV
    '(and lastime
        (RPLACD (cddadr (or (let [x (assq 'TIME *fe*)] (and x (not (term? (cadr x))) x)) '(TIME (!TIME (PAST) nil))))
            (list (or (cadddr lastime) (car (cddddr lastime))) (or (car (cddddr lastime)) (cadddr lastime)))))]])

(putprop! 'THERE 'FEATURES ['ADV 'PLACE])
(putprop! 'THERE 'SEMANTICS [['ADV true]])

(putprop! 'THEY 'FEATURES ['PRON 'SUBJ 'NPL])
(putprop! 'THEY 'SEMANTICS [['PRON '(smit 'THEY)]])

(putprop! 'THICK 'FEATURES ['ADJ])
(putprop! 'THICK 'SEMANTICS [['ADJ '(object [:markers '(!PHYSOB) :procedure '((!MORE !THICKNESS *** (0 128 0)))])]
                              ['MEASURE '(measure :dimension '!THICKNESS :restrictions '(!PHYSOB) :direction true)]])

(putprop! 'THIN 'FEATURES ['ADJ])
(putprop! 'THIN 'SEMANTICS [['ADJ '(object [:markers '(!PHYSOB) :procedure '((!MORE !THICKNESS (0 128 0) ***))])]
                             ['MEASURE '(measure :dimension '!THICKNESS :restrictions '(!PHYSOB) :direction nil)]])

(putprop! 'THING 'FEATURES ['NOUN 'NS])
(putprop! 'THING 'SEMANTICS [['NOUN '(object [:markers '(!THING !VAGUE !PHYSOB) :procedure '((!PHYSOB  *** ))])]])

(putprop! 'THIS 'FEATURES ['NS 'DET 'DEM 'DEF])

(putprop! 'THREE 'FEATURES ['NUM])
(putprop! 'THREE 'SEMANTICS [['NUM 3]])

(putprop! 'TIME 'FEATURES ['NOUN 'NS 'TIM1])

(putprop! 'TO 'FEATURES ['PREP])
(putprop! 'TO 'SEMANTICS [['PREP '(relation [:restrictions '(((!PHYSOB)) ((!DIRECTION))) :procedure '((!EVAL (SUBTOP '*!1* '*OF (REFERENCE? *smob1*))))])]])

(putprop! 'TOGETHER 'FEATURES ['PRT])
(putprop! 'TOGETHER 'SEMANTICS [['PRT true]])

(putprop! 'TOLD 'IRREGULAR '(TELL (PAST) (INF)))

(putprop! 'TOP 'FEATURES ['PREP2])
(putprop! 'TOP 'SEMANTICS [['PREP2 true]])

(putprop! 'TOUCH 'FEATURES ['VB 'INF 'TRANS])
(putprop! 'TOUCH 'SEMANTICS [['VB [['TRANS '(!grasp)]]]])

(putprop! 'TOY 'FEATURES ['NOUN 'NS])
(putprop! 'TOY 'SEMANTICS [['NOUN '(object [:markers '(!PHYSOB) :procedure '((!MANIP ***))])]])

(putprop! 'TWO 'FEATURES ['NUM])
(putprop! 'TWO 'SEMANTICS [['NUM 2]])

(putprop! 'UNDER 'FEATURES ['PREP 'PLACE])
(putprop! 'UNDER 'SEMANTICS [['PREP '(!loc '!ABOVE false)]])

(putprop! 'UNDERNEATH 'FEATURES ['PREP 'PLACE])
(putprop! 'UNDERNEATH 'SEMANTICS [['PREP '(!loc '!ABOVE false)]])

(putprop! 'UP 'FEATURES ['PRT])
(putprop! 'UP 'SEMANTICS [['PRT true]])

(putprop! 'US 'IRREGULAR '(WE (OBJ) (SUBJ)))

(putprop! 'WANT 'FEATURES ['VB 'INF 'TRANS 'TOOB 'SUBTOB])
(putprop! 'WANT 'SEMANTICS [['VB [['TRANS '(relation [:restrictions '(((!ANIMATE)) ((!EVENT))) :markers '(!EVENT) :procedure '((!WANT *!1* *!2* *TIME))])]]]])

(putprop! 'WAS 'IRREGULAR '(BE (V3PS VFS PAST) (INF)))

(putprop! 'WE 'FEATURES ['PRON 'NPL 'SUBJ])
(putprop! 'WE 'SEMANTICS [['PRON '(smset (list (newcopy 'WE-OSS)))]])

(putprop! 'WERE 'IRREGULAR '(BE (VPL PAST) (INF)))

(putprop! 'WHAT 'FEATURES ['QDET 'DET 'NPL 'PRON 'QPRON 'NS])
(putprop! 'WHAT 'SEMANTICS [['DET true] ['PRON '(smset (list (newcopy 'UNKNOWN-OSS)))]])

(putprop! 'WHATEVER 'FEATURES ['PRON 'EVERPRON 'NS])

(putprop! 'WE 'REFER '(ßSHRDLU ßFRIEND))

(putprop! 'WHERE 'FEATURES ['QADJ 'PLACE])
(putprop! 'WHERE 'SEMANTICS [['QADJ '(fq! 'WHERE)]])

(putprop! 'WHEREVER 'FEATURES ['PRON 'EVERPRON 'NS])

(putprop! 'WHEN 'FEATURES ['QADJ 'BINDER 'TIME])
(putprop! 'WHEN 'SEMANTICS [['BINDER '(smbinder *tss* *start* *end*)] ['QADJ '(fq! 'WHEN)]])

(putprop! 'WHENEVER 'FEATURES ['BINDER])

(putprop! 'WHICH 'FEATURES ['QDET 'DET 'PRONREL 'NS 'NPL])
(putprop! 'WHICH 'SEMANTICS [['PRONREL true] ['DET true]])

(putprop! 'WHICHEVER 'FEATURES ['DET 'RSQDET 'NS 'NPL])

(putprop! 'WHILE 'FEATURES ['BINDER 'TIME])
(putprop! 'WHILE 'SEMANTICS [['BINDER '(smbinder *tss* *start* *end*)]])

(putprop! 'WHITE 'FEATURES ['ADJ])
(putprop! 'WHITE 'SEMANTICS [['ADJ '(!color '!WHITE)]])

(putprop! 'WHO 'FEATURES ['PRONREL 'QPRON 'PRON 'NS])
(putprop! 'WHO 'SEMANTICS [['PRONREL true] ['PRON '(smset (list (newcopy ANIMATE-OSS)))]])

(putprop! 'WHOEVER 'FEATURES ['PRON 'EVERPRON 'NS])

(putprop! 'WHOSE 'FEATURES ['DET 'QDET 'NPL 'NS])

(putprop! 'WHY 'FEATURES ['QADJ])
(putprop! 'WHY 'SEMANTICS [['QADJ '(fq! 'WHY)]])

(putprop! 'WHYEVER 'FEATURES ['PRON 'EVERPRON 'NS])

(putprop! 'WIDE 'FEATURES ['ADJ])
(putprop! 'WIDE 'SEMANTICS [['ADJ '(object [:markers '(!PHYSOB) :procedure '((!MORE !WIDTH *** (0 128 0)))])]
                             ['MEASURE '(measure :dimension '!WIDTH :restrictions '(!PHYSOB) :direction true)]])

(putprop! 'WILL 'FEATURES ['VB 'AUX 'WILL 'MODAL 'V3PS 'VFS 'VPL])
(putprop! 'WILL 'SEMANTICS [['VB true]])

(putprop! 'WITH 'FEATURES ['PREP])

(putprop! 'WOULD 'FEATURES ['VB 'AUX 'MODAL])
(putprop! 'WOULD 'SEMANTICS [['VB true]])

(putprop! 'YOU 'FEATURES ['PRON 'NPL 'NS 'SUBJ 'OBJ])
(putprop! 'YOU 'SEMANTICS [['PRON '(smset (list (newcopy 'SHRDLU-OSS)))]])

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

(defn- !blueprint [x]
    (if (getprop x 'REFER) '*!2*
        (let [a (loop-when [a nil x (cddaar (§ INTERP x))] x => a
                    (cond (not= (caar x) 'thgoal) (bug! '!blueprint nil)
                        (= (caadar x) '!IS) (recur a (cdr x))
                        (= (caadar x) '!PART) (recur (cons (cadr (cadar x)) a) (cdr x))
                        :else (bug! '!blueprint nil)))
              x (when a (getprop (car a) 'REFER))]
            (if x x
                (do (putprop! 'BLUEPRINT 'SM
                        (cond (nil? a) (getprop 'STACKPARTS 'SM)
                            (cdr a) (bug! '!blueprint nil)
                            :else (getprop (car a) 'SM)))
                    'BLUEPRINT)))))

(putprop! '!BOX 'SYS '(!PHYSOB))

(defn- !build []
    (relation [:restrictions '(((!ANIMATE)) ((!STACK))) :markers '(!EVENT) :procedure '((!EVAL (list '!STACKUP (!blueprint *smob1*) '*TIME)))]))

(putprop! '!CALL 'THMLIST '((3 '((THUSE TC-3)))))

(putprop! '!COLOR 'PRIORITY 192)
(putprop! '!COLOR 'SYS '(!PROPERTY))

(defn- !color [x]
    (object (vector :markers (list '!PHYSOB x) :procedure (list (list '!color '*** x)))))

(putprop! '!CONSTRUCT 'SYSTEM '(!STACK !ROW))
(putprop! '!CONSTRUCT 'SYS '(!PHYSOB))

(putprop! '!CONTAIN 'PRIORITY -1)

(defn- !cleanoff []
    (relation [:restrictions '(((!ANIMATE)) ((!PHYSOB))) :markers '(!EVENT) :procedure '((!CLEARTOP *!2* *TIME))]))

(putprop! '!CLEARTOP 'THMLIST '((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(putprop! '!DIRECTION 'NOGOAL true)

(putprop! '!END 'THMLIST '((3 '((THUSE TC-STARTEND3))) (4 '((THUSE TC-STARTEND4)))))

(putprop! '!EQDIM 'NOGOAL true)

(defn- !eqdim [x]
    (let [x (size x)]
        (and (= (car x) (cadr x)) (= (car x) (caddr x)))))

(putprop! '!EQUIV 'PRIORITY 512)

(putprop! '!EVENT 'SYS '(!SYSTEMS))

(putprop! '!EXISTS 'THMLIST '((2 '((THUSE TC-EXISTS))) (3 '((THUSE TCT-EXISTS)))))

(putprop! '!GET-RID-OF 'THMLIST '((2 '((THUSE TCT-EXISTS))) (3 '((THUSE THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(putprop! '!GRASP 'THMLIST '((2 '((THUSE TC-2))) (3 '((THUSE TCT-3))) (4 '((THUSE TCTE-4)))))

(defn- !grasp []
    (relation
        [:restrictions '(((!ANIMATE)) ((!MANIP)))
            :markers '(!EVENT)
            :procedure '((!EVAL (if (istense *c* 'IMPERF) '(!GRASPING *!2* *TIME) '(!GRASP *!2* *TIME))))]))

(putprop! '!GRASPING 'THMLIST '((3 '((THUSE TCT-GRASPING)))))

(putprop! '!GREEN 'SYS '(!SPECTRUM))

(putprop! '!HAND 'SYS '(!PHYSOB))

(defn- !have []
    (relation
        [:restrictions '(((!THING)) ((!THING) (and (memq '!ROLE (markers? *smob1*)) (check-markers (cadr (assq '!ROLE (relations? *smob1*))) (markers? *smsub*) (systems? *smsub*)))))
            :procedure '((!SUBST *!1* ?))]
        [:restrictions '(((!ANIMATE)) ((!PHYSOB)))
            :procedure '((!BELONG *!2* *!1*))]))

(putprop! '!HEIGHT 'MEASFN #(caddr (size %)))

(defn- !in []
    (if (cq 'LOBJ)
        (relation
            [:restrictions '(((!THING)) ((!BOX))) :markers '(!PLACE) :procedure '((!IN *!2*))])
        (relation
            [:restrictions '(((!MANIP)) ((!BOX))) :procedure '((!CONTAIN *!2* *!1* *TIME))]
            [:restrictions '(((!MANIP)) ((!HAND))) :procedure '((!GRASPING *!1* *TIME))]
            [:restrictions '(((!PLACE)) ((!BOX))) :procedure '((!IN *!1* *!2*))]
            [:restrictions '(((!MANIP)) ((!CONSTRUCT))) :procedure '((!PART *!1* *!2* *TIME))])))

(putprop! '!IS 'PRIORITY 64)

(putprop! '!LIKE 'TELLABLE true)
(putprop! '!LIKE 'THMLIST '((3 '((THTBF thtrue)))))

(putprop! '!LOC 'THMLIST '((4 '((THUSE TC-LOC))) (5 '((THUSE TCT-LOC)))))

(defn- !loc [& a] (!loc2 (car a) (cadr a)))

(dynamic- *loctype*)
(dynamic- *locneg*)

(defn- !loc2 [loctype' locneg']
    (binding [*loctype* loctype' *locneg* locneg']
        (if (cq 'LOBJ)
            (relation [:restrictions '(((!THING)) (LOBJ (!PHYSOB))) :markers '(!PLACE) :procedure '((!EVAL (list '!LOC *loctype* *locneg* *!2*)))])
            (relation [:restrictions '(((!PHYSOB)) ((!PHYSOB))) :procedure '((!EVAL (list '!LOC *loctype* (if *locneg* '*!1* '*!2*) (if *locneg* '*!2* '*!1*) '*TIME)))]))))

(putprop! '!MANIP 'SYS '(!PHYSOB))

(putprop! '!MORE 'THMLIST '((4 '((THUSE TC-MORE)))))

(putprop! '!NAME 'THMLIST '((2 '((THUSE TC-2)))))
(putprop! '!NAME 'SYS '(!SYSTEMS))

(defn- !name []
    (relation [:restrictions '(((!ANIMATE)) ((!PHYSOB))) :markers '(!EVENT) :procedure '((!NAME *!2*))]))

(putprop! '!NOTICE 'THMLIST '((2 '((THUSE TC-2)))))

(defn- !notice []
    (relation [:restrictions '(((!ANIMATE)) ((!PHYSOB))) :markers '(!EVENT) :procedure '((!NOTICE *!2* *TIME))]))

(putprop! '!ON 'THMLIST '((3 '((THUSE TC-ON))) (4 '((THUSE TCT-ON)))))

(defn- !on []
    (if (cq 'LOBJ)
        (relation
            [:restrictions '(((!THING)) ((!PHYSOB))) :markers '(!PLACE) :procedure '((!ON *!2*))])
        (relation
            [:restrictions '(((!PHYSOB)) ((!PHYSOB))) :paraphrase '(ANYWHERE ON TOP OF) :procedure '((!ON *!1* *!2* *TIME))]
            [:restrictions '(((!PHYSOB)) ((!MANIP))) :paraphrase '(DIRECTLY ON THE SURFACE) :procedure '((!SUPPORT *!2* *!1* *TIME))]
            [:restrictions '(((!PLACE)) ((!PHYSOB))) :procedure '((!ON *!1* *!2*))])))

(putprop! '!PACK 'THMLIST '((3 '((THUSE TC-3)))))

(putprop! '!PART 'THMLIST '((3 '((THUSE TC-PART)))))            ;; PERFORMED ON GRADUATION DAY, JUNE 2, 1972 BY JMH

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

(defn- !propdefine [x]
    (putprop! x 'FEATURES ['PROPN 'NS])               ;; CHANGED TO FEATURES FROM 'WORD' IN THE OLD DICTIONARY
    (putprop! x 'SEMANTICS [['PROPN true]]))

(putprop! '!PROPERTY 'SYSTEM '(!COLOR !SIZE !SHAPE))
(putprop! '!PROPERTY 'SYS '(!THING))

(putprop! '!POINTED 'SYS '(!SHAPES))

(putprop! '!RED 'SYS '(!SPECTRUM))

(putprop! '!RELATION 'SYS '(!SYSTEMS))

(putprop! '!ROLE 'NOGOAL true)

(defq- !role [& _] true)

(putprop! '!ROUND 'SYS '(!SHAPES))

(putprop! '!ROW 'SYS '(!CONSTRUCT))

(putprop! '!ROBOT 'SYS '(!ANIMATE))

(putprop! '!SIZE 'MEASFN #(reduce + (size %)))
(putprop! '!SIZE 'SYS '(!PROPERTY))

(putprop! '!SHAPE 'PRIORITY 128)
(putprop! '!SHAPE 'SYS '(!PROPERTY))

(putprop! '!STACK 'SYS '(!CONSTRUCT))

(putprop! '!STACKUP 'THMLIST '((2 '((THUSE TC-2)))))

(defn- !stackup []
    (relation [:restrictions '(((!ANIMATE)) ((!MANIP))) :markers '(!EVENT) :procedure '((!STACKUP *!2* *TIME))]))

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

(putprop! 'ADJ 'ELIM ['ADJ 'SUP 'COMPAR])
(putprop! 'ADV 'ELIM ['ADV 'PREPADV 'TIMW 'TIM2 'ADVADV 'VBAD 'PLACE 'LOBJ])
(putprop! 'BINDER 'ELIM ['BINDER 'TIME])
(putprop! 'CLASF 'ELIM ['CLASF])
(putprop! 'DET 'ELIM ['DET 'NPL 'NS 'PART 'DEF 'INDEF 'NEG 'DEM 'INCOM 'OFD 'QNTFR 'NONUM 'QDET])
(putprop! 'NOUN 'ELIM ['NOUN 'POSS 'MASS 'NPL 'NS 'TIM1 'TIME 'MONTH])
(putprop! 'NUM 'ELIM ['NUM 'NPL 'NS])
(putprop! 'NUMD 'ELIM ['NUMD 'NUMDAN 'NUMDAT 'NUMDALONE])
(putprop! 'ORD 'ELIM ['ORD 'TIMORD])
(putprop! 'POSS 'ELIM ['NOUN 'NPL 'NS 'MASS 'NFS 'PRON])
(putprop! 'PREP 'ELIM ['PREP 'MOTOR 'PLACE 'NEED2])
(putprop! 'PREP2 'ELIM ['PREP2])
(putprop! 'PRON 'ELIM ['PRON 'QPRON 'EVERPRON 'POSS 'SUBJ 'OBJ 'NS 'NPL 'NFS 'NEG 'DEFPOSS])
(putprop! 'PRT 'ELIM ['PRT])
(putprop! 'QADJ 'ELIM ['PLACE 'QADJ])
(putprop! 'PROPN 'ELIM ['PROPN 'POSS 'NS 'NPL])
(putprop! 'TPRON 'ELIM ['TPRON 'NS 'NPL 'NEG 'ANY])
(putprop! 'VB 'ELIM ['VB 'MVB 'AUX 'QAUX 'MODAL 'WILL 'BE 'DO 'HAVE 'ING 'EN 'INF 'V3PS 'QUOTING 'VFS 'VPL 'PAST 'PRESENT 'NEG 'ITRNS 'TRANS 'TRANSL 'TRANS2 'TRANSL2 'INT 'ITRNSL 'INGOB 'TOOB 'SUBTOB 'REPOB 'INGOB2 'TOOB2 'SUBTOB2 'REPOB2 'VPRT 'TO2 'TRANSINT 'TOOB1 'INGOB1 'REPOB1])

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

(putprop! 'thamong 'NOGOAL true)
(putprop! 'thand 'NOGOAL true)
(putprop! 'thfind 'NOGOAL true)
(putprop! 'thgoal 'NOGOAL true)
(putprop! 'thnot 'NOGOAL true)
(putprop! 'thor 'NOGOAL true)
(putprop! 'thprog 'NOGOAL true)
(putprop! 'thsetq 'NOGOAL true)

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

(putprop! 'FAKE-AGENT 'FEATURES ['NG 'INDEF 'SG-PL])
(putprop! 'FAKE-AGENT 'SEMANTICS '(UNKNOWN-OSS-BY))
(putprop! 'FAKE-AGENT 'PARENT '(FAKE-BY-PHRASE))

(putprop! 'FAKE-BY-PHRASE 'FEATURES ['PREPG 'AGENT])
(putprop! 'FAKE-BY-PHRASE 'FIRSTWORD '(BY))
(putprop! 'FAKE-BY-PHRASE 'DAUGHTERS '(FAKE-AGENT FAKE-BY))

(putprop! 'FAKE-BY 'FEATURES ['PREP 'BY])
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
(putprop! 'WE-OSS 'CONJUNCTS= '(FRIEND-OSS SHRDLU-OSS))

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

(defn- smtime [] (bug! 'smtime "NOT WRITTEN YET"))

(defn- smthat [] (bug! 'smthat "NOT WRITTEN YET"))

(defn- smconj [node]
    ;; FOR ALL CONJOINED THINGS -- IT CURRENTLY ONLY HANDLES THINGS WHICH HAVE AN OSS OR RSS STRUCTURE AS THEIR SEMANTICS.
    ;; THIS DOES NOT INCLUDE SINGLE WORDS OF MOST TYPES.  IT USES RECURSION.
    (smset (smconj2 nil nil node)))

(defn- smconj2 [i a l]
    ;; A IS THE LIST OF INTERPRETATIONS FOR THE CONJUNCTS HANDLED SO FAR -- THIS FUNCTION WILL BE CALLED ONCE FOR EACH POSSIBLE COMBINATION.
    ;; THE MARKERS FOR THE CONJOINED STRUCTURE ARE THOSE OF THE FIRST CONJUNCT -- NEEDS MORE SOPHISTICATION.
    ;; L IS THE REST OF NODES YET TO BE HANDLED.
    ;; WHEN THERE IS NO L, WE HAVE LOOPED TO THE END OF THE LIST OF CONJUNCTS, AND THE RESULTING INTERPRETATION IS OK.
    ;; THE MAPPING IS DOWN THE LIST OF INTERPRETATIONS FOR A SINGLE CONJUNCT WHILE THE RECURSION GETS US DOWN THE LIST OF CONJUNCTS.
    ;; THUS WE GET EVERY POSSIBLE COMBINATION OF THE INTERPRETATIONS. -- ISN'T LISP SUPER-DUPER-WONDERFUL!
    (if l (doall (mapcat #(smconj2 % (cons % a) (cdr l)) (semantics l)))
        (list (build
            'RSSNODE= (when (rss? i) (gensym 'RSS))
            'OSSNODE= (when (oss? i) (gensym 'OSS))
            'MARKERS= (markers? i)
            'SYSTEMS= (systems? i)
            'REL= (rel? i)
            'CONJUNCTS= (when (or (cq 'BUT) (cq 'AND)) a)
            'DISJUNCTS= (when (or (cq 'OR) (cq 'NOR)) a)))))

(defn- smvg [] ;; CALLED INSIDE ANY VG
    (let [tss (getr (move-pt 'C 'U '(CLAUSE)) 'TIME)]
        (when (cq 'NEG) (add-f 'NEG *pt*))                           ;; NEG IS TRANSFERRED FROM THE VG TO THE CLAUSE IN WHICH IT IS EMBEDDED.
        (let [tense (getr *c* 'TENSE)
              tense (cond (memq tense '((PRESENT) (IMPER) (INFINITIVE))) tense
                        (= tense '(MODAL))
                            (do (set! *oops* "THAT DOESN'T MAKE ANY SENSE TO ME.")
                                (add-f 'MODAL *pt*)                  ;; CLAUSES ARE ALSO MARKED AS MODAL.
                                tense)
                        (and (= tense '(FUTURE)) (isq *pt* 'QUEST) (= (refer? (car (semantics (getr *pt* 'SUBJECT)))) '(ßSHRDLU))) ;; FUTURE QUESTIONS WITH "YOU"
                            (let [tense '(PRESENT)]                     ;; SUBJECT IS REALLY IMPERATIVE.
                                (remove-f 'QUEST *pt*)
                                (add-f 'IMPER *pt*)                  ;; THE CLAUSE IS NO LONGER QUESTION, BUT RATHER, IMPERATIVE.
                                tense)
                        (setdif tense '(PAST PRESENT))
                            (oops! "I DON'T KNOW HOW TO HANDLE TENSES INVOLVING FUTURE EVENTS OR MODALS OTHER THAN IN THE PRESENT.")
                        :else tense)]
            (putprop! tss 'TENSE= tense)
            true)))

(defn- smpron [node]
    (eval (semantics node))
    (when-not *sm*
        (set! *oops* (str "I DON'T KNOW WHAT \"" (from (firstword *h*) (wordafter *h*)) "\" REFERS TO.")))
    *sm*)

(defn- smvaux []
    (when (isq *h* 'NEG) (fq! 'NEG))
    (putprop! (getr *c* 'TIME) 'TENSE= (or (meet (features *h*) '(PRESENT PAST MODAL)) (bug! 'smvaux "FUNNY TENSE"))))

(defn- smplace [] (bug! 'smplace "NOT WRITTEN YET"))

(defn- smtoadj [] (bug! 'smtoadj "NOT WRITTEN YET"))

(defn- smadverb [] (bug! 'smadverb "NOT WRITTEN YET"))

(defn- smprop []
    ;; THIS IS THE SEMANTICS FOR PROPER NOUNS.  IT PRODUCES TWO
    ;; INTERPRETATIONS.  ONE IS THE OPAQUE REFERENCE TO THE NAME
    ;; ITSELF, AS IN "CALL IT SAM".  THE OTHER IS THE TRANSPARENT
    ;; REFERENT AS IN "PICK UP SAM".
    (smset (list
        (build
            'OSSNODE= (gensym 'OSS)
            'VARIABLE= 'NAME
            'DETERMINER= '(1 DEF nil)
            'PARSENODE= *c*
            'MARKERS= '(!NAME)
            'REFER= (list (word (firstword *h*))))
        (let [name (gensym 'OSS)] (build
            'OSSNODE= name
            'DETERMINER= '(1 DEF nil)
            'PARSENODE= *c*
            'VARIABLE= (gensym 'X)
            'RELATIONS= (list (list '!NAME name (word (firstword *h*))))))))
    (smng2))

(defn- smadjqshort [] (bug! 'smadjqshort "NOT WRITTEN YET"))

(defn- smadjg-prepg []
    ;; HANDLES ADJECTIVE GROUPS AND PREPGS BOTH AS COMPLEMENTS AND QUALIFIERS.
    ;; DO NOTHING FOR "BY" PHRASES IN PASSIVE CLAUSES OR "OF" PHRASES LIKE IN "THREE OF THE BLOCKS".
    ;; SEMANTIC SUBJECT IS THE SUBJECT OF AN INTENSIVE OR THE NG TO WHICH THE GROUP IS A QUALIFIER,
    ;; OR THE CLAUSE OF WHICH IT IS AN ADJUNCT.
    (if (or (cq 'AGENT) (cq 'OF)) true
        (do (setr *c* 'LOGICAL-SUBJECT
                (cond (cq 'COMP) (getr (move-pt 'C 'U '(CLAUSE)) 'SUBJECT)
                    (cq 'LOBJ) (or (getr (move-pt 'C 'U '(CLAUSE)) 'OBJ1) (getr *pt* 'SUBJECT))
                    (isq (move-pt 'C 'U '(not (isq *pt* 'COMPONENT)) 'U) 'NG) *pt*
                    (isq *pt* 'CLAUSE) *pt*
                    :else (bug! 'smadjg-prepg "FUNNY POSITION")))
            (let [smsub (semantics (getr *c* 'LOGICAL-SUBJECT))]
                (and (cq 'ADJG)
                    (getr *c* 'OBJ1)
                    (setr *c* 'ADJGHEAD (compare-build (getr *c* 'HEAD) (cond (cq 'AS) '!ASMUCH (cq 'THAN) '!MORE :else (bug! 'smadjg-prepg "FUNNY TYPE")))))
                (if (getr *c* 'OBJ1) (do (smcl1) *sm*)
                    (smset (binding [*sm* nil]
                        (smset (doall (map (lambda [x]
                            (build
                                'OSSNODE= (gensym 'OSS)
                                'MARKERS= (markers? x)
                                'SYSTEMS= (systems? x)
                                'VARIABLE= (variable? x)
                                'REFER= (refer? x)
                                'REL= x
                                'REFER= (refer? x)
                                'DETERMINER= '(NS-PL INDEF nil)))
                            smsub)))
                        (eval (if (or (cq 'COMPAR) (cq 'SUP)) (findmeasure (getr *c* 'HEAD)) (semantics (getr *c* 'HEAD))))
                        *sm*)))))))

(dynamic- *pronoun*)
(dynamic- *candidates*)

(dynamic- *lastevent*)

(defn- smit [pronoun']
    ;; PRONOUN IS (IT THEY ONE) A NODE LIST OF POSSIBLE REFERENTS.
    ;; IS THIS A "DO IT!" COMMAND?  IF SO, RETURN THE LAST EVENT MENTIONED.
    ;; IF THIS PRONOUN HAS BEEN USED BEFORE IN THIS SENTENCE, THEN USE THE SAME CANDIDATES.
    ;; IF THIS PRONOUN WAS USED IN THE PREVIOUS SENTENCE,
    ;; LOOK FOR A STRUCTURE LIKE "A BLOCK WHICH IS TALLER THAN ANYTHING WHICH SUPPORTS IT"
    ;; OR "A BLOCK TALLER THAN ANYTHING WHICH SUPPORTS IT".
    (binding [*pronoun* pronoun' *candidates* nil]
        (when-not discourse? (bug! 'smit "DISCOURSE SWITCH NOT ON"))
        (let-when [last- (lambda []
                            (smit2 (getr *lastsent* 'SUBJECT) 192)
                            (smit2 (parsenode? *lastrel*) 128)          ;; TRY REL (I.E. QUESTION FOCUS) OF THE LAST SENTENCE
                            (move-pt 'LASTSENT 'DLC)
                            (loop-when (move-pt 'PV '(NG))              ;; GO THROUGH TOP LEVEL NG'S OF LAST SENTENCE
                                (smit2 *pt* 64)
                                (when (move-pt 'PV) (§ recur)))
                            (when-not *sm*                              ;; FIND A REFERENT MAP IN ANSNAME (NG'S IN LAST ANSWER)
                                (dorun (map* #(smit2 % 0) *ansname*)))
                            (when-not *sm*                              ;; FIND A REFERENT MAP IN BACKREF2 (NG'S IN LAST SENTENCE)
                                (dorun (map* #(smit2 % 0) *backref2*))))
                   done- (lambda []
                            (putprop! *pronoun* 'BIND *candidates*)
                            (when-not (cdr *sm*) (remprop! (car *sm*) 'AMBIGUITIES=)))
        ] (not (and *mvb* (isq *mvb* 'DO) (cq 'OBJ1))) => (smset *lastevent*)
            (cond (getprop *pronoun* 'BIND)
                    (dorun (map* #(smit2 % 0) (getprop *pronoun* 'BIND)))
                (smit2 (getprop *pronoun* 'LASTBIND) 0)
                    (done-)
                (or (move-pt 'C 'U 'U '(NG) 'U 'U '(NG)) (move-pt 'C 'U 'U '(NG) 'U '(COMP) 'PV '(SUBJ)))
                    (do (smit2 *pt* 0)
                        (move-pt 'C 'U 'U '(NG))
                        (when (isq *pt* 'DEF)
                            (add-f 'INDEF *pt*)
                            (remove-f 'DEF *pt*)
                            (dorun (map #(putprop! % 'DETERMINER= '((EXACTLY 1) INDEF nil)) (semantics *pt*)))))
                (or (move-pt 'C 'U '(BOUND) 'U) (move-pt 'C 'U '(and (isq *pt* 'CLAUSE) (isq *pt* 'COMPONENT)) 'U 'DLC))
                    (do (smit2 (getr *pt* 'OBJ2) 0)
                        (smit2 (getr *pt* 'OBJ1) 0)
                        (smit2 (getr *pt* 'SUBJECT) 0)
                        (when (and (nil? *sm*) (isq *pt* 'RSQ))
                            (smit2 (getr *pt* 'RELHEAD) 0))
                        (when-not *sm* (last-) (done-)))
                :else (do (last-) (done-)))
            *sm*)))

(defn- smit2 [node plausibility]
    ;; MAKE SURE NODE IS REALLY THERE.
    ;; QUEST NODES (SUCH AS "WHAT") OR OTHER NODES WITHOUT HEAD NOUNS ARE NOT SUITABLE FOR REFERENTS.
    ;; MAKE SURE THAT NODE HASN'T ALREADY BEEN USED AS REFERENT.
    ;; MAKE SURE NODE AND PRONOUN AGREE IN NUMBER.
    (and node
        (getr node 'HEAD)
        (not (memq (car node) *candidates*))
        (if (= *pronoun* 'IT)
            (and (isq node 'NS) (not (isq node 'PRONG)))
            (isq node 'NPL))
        (set! *candidates* (cons (car node) *candidates*))
        (smset (concat
            (doall (map (lambda [x]
                (let [name (gensym 'OSS)] (build
                    'OSSNODE= name
                    'MARKERS= (markers? x)
                    'SYSTEMS= (systems? x)
                    'PLAUSIBILITY= plausibility
                    'AMBIGUITIES= (list (list name (from (firstword node) (wordafter node)) *c*))
                    'REFER= (refer? x)
                    'VARIABLE= (variable? x)
                    'PARSENODE= *c* ;; INPUT PARAMETER
                    ;; USE THE REFERENT'S REFERENT, IF IT HAS ONE.
                    'DETERMINER= (list (if (isq *c* 'NPL) 'NPL 'NS) 'INDEF nil)
                    ;; DONE SO THAT IF VARIBLE IS BOUND, PLANNER GENERATOR WILL USE IT.
                    ;; RELATION SAYS THAT THIS OSS "REFERS" TO THE OSS WHOSE VARIABLE NAME IS GIVEN.
                    'RELATIONS= (list (list '!REFERS (variable? x))))))
                (semantics node)))
            *sm*))))

(defn- smngof []
    ;; MAP DOWN THE LIST OF "OF" OBJECT INTERPRETATIONS.
    ;; USED TO PROCESS NOUN GROUPS LIKE "THREE OF THE BLOCKS", "BOTH OF THEM".
    ;; SINCE THE OBJECT OF THE "OF" MUST BE DEFINITE (SYNTACTICALLY) AND HAS ALREADY BEEN PROCESSED,
    ;; THE PLANNER CODE BUILT IS JUST A THAMONG EXPRESSION OF THE LIST
    ;; OF POSSIBLE REFERENTS OF THE "OF" OBJECT.
    (smset (mapbland (lambda [x]
            (build
                'OSSNODE= (gensym 'OSS)
                'VARIABLE= (variable? x)
                'SYSTEMS= (systems? x)
                'MARKERS= (markers? x)
                'PARSENODE= *c*
                'DETERMINER= (list (cond (cq 'NUM) (semantics (move-pt 'H 'PV '(NUM))) (isq *nb* 'BOTH) 2 :else 'NPL)
                                (if (move-pt 'H 'PV '(QNTFR)) (eval (semantics *pt*)) 'INDEF)
                                (cond (cq 'HOWMANY) 'HOWMANY (cq 'QDET) 'WHICH))
                'RELATIONS= (list (list 'thamong (list 'thv (variable? x)) (quotify (refer? x))))))
        (semantics (move-pt 'H 'DLC)))))

(dynamic- *num*)
(dynamic- *word-being*)

(defn- smng1 []
    ;; SMNG1 IS CALLED AS SOON AS THE HEAD OF A NOUN GROUP IS PARSED.  IT FIRST BUILDS A SKELETON OSS
    ;; CONTAINING ONLY THE DETERMINERS AND ORDINALS.  IT THEN EVAL'S THE DICTIONARY DEFINITION OF THE
    ;; HEAD NOUN WHICH SHOULD BUILD OSS'S FOR EACH POSSIBLE INTERPRETATION OF THE NOUN.  IT THEN CYCLES
    ;; THROUGH ALL THE GOODIES IN FROUNT OF THE HEAD NOUN, EVALING THEIR DEFINITIONS.  THE FREE VARIABLE
    ;; "SM" IS USED TO KEEP THE LIST OF OSS'S DURING THIS ENTIRE PROCESS.  NOTE THE SPECIAL HANDLING OF
    ;; TPRONS (ANYTHING, SOMETHING, ETC.) AND OF SUPERLATIVE AND COMPARATIVE ADJECTIVES.
    ;;
    ;; BUILD AN INITIAL OSS.  SETUP TO LOOP THROUGH ADJECTIVES.
    ;; IF IT'S A TPRON, IT WAS EVALED ABOVE, SO SKIP INCOMPLETES SUCH AS "PICK UP TWO".
    ;; EVAL THE HEAD NOUN IF AN ADJECTIVE ELIMINATES ANY POSSIBLE INTERPRETATION FOR THIS NG,
    ;; FAIL IF WE'VE LOOPED THRU ALL THE MODIFIERS, THEN RETURN THE LIST OF POSSIBLE INTERPRETATIONS.
    ;; IF IT'S A COMPARATIVE OR SUPERLATIVE ADJECTIVE,
    ;; IF IT'S AN ADJECTIVE OR CLASSIFIER, THEN EVAL THE DICTIONARY DEFINITION OF IT.
    (binding [*word-being* nil]
        (smset (list
            (let [name (gensym 'OSS)] (build
                'OSSNODE= name
                'PARSENODE= *c*
                'VARIABLE= (gensym 'X)
                'MARKERS= (when (cq 'TPRON) '(!VAGUE !PHYSOB !THING))
                'RELATIONS= (when (cq 'TPRON) (list (list '!PHYSOB name)))
                'DETERMINER= (list
                                (cond
                                    (cq 'NUMD) (binding [*num* (semantics (move-pt 'H 'PV '(NUM)))] (eval (semantics (move-pt 'H 'PV '(NUMD)))))
                                    (cq 'NUM) (semantics (move-pt 'H 'PV '(NUM)))
                                    (cq 'NPL) (cond (isq *nb* 'BOTH) 2 (cq 'NS) 'SG-PL :else 'NPL)
                                    :else 'NS)
                                (cond
                                    (cq 'QNTFR) (eval (semantics (move-pt 'H 'PV '(QNTFR))))
                                    (cq 'TPRON) (eval (semantics (move-pt 'H 'PV '(TPRON))))
                                    (cq 'DEF) 'DEF
                                    (cq 'DET) 'INDEF
                                    :else 'NDET)
                                (cond
                                    (cq 'HOWMANY) 'HOWMANY
                                    (cq 'QDET) 'WHICH))))))
        (set! *word-being* *h*)
        (cond (isq *h* 'TPRON) true
            (cq 'INCOM) (do (smone) true)
            :else (smset (eval (semantics *word-being*))))
        (loop []
            (when *sm*
                (set! *word-being* (cdr *word-being*))
                (when' *word-being* => *sm*
                    (cond
                        (or (isq *word-being* 'COMPAR) (isq *word-being* 'SUP))
                            (eval (findmeasure *word-being*))
                        (or (isq *word-being* 'ADJ) (isq *word-being* 'CLASF))
                            (eval (semantics *word-being*))
                        (isq *word-being* 'POSS)
                            (smposs))
                    (recur))))))

(defn- smng2 []
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
    (smset (mapbland smng3 *sm*)))

(defn- smng3 [oss]
    ;; TAKES AN OSS AS ARGUMENT AND TRIES TO FIND ITS REFERENCE IF THE NOUN GROUP IS DEFINITE.
    ;; EXPECT FOR SPECIAL "ONLY DEFINITE" DEFINITES SUCH AS "THE RIGHT" AND "THE THING".
    ;; IF IT'S NOT DEFINITE, OR ALREADY HAS A REFERENT MARKED, OR IT'S KLUDGY ANSWER NAME, JUST RETURN IT.
    (if (or (not= (quantifier? oss) 'DEF) (refer? oss) (cq 'ANSNAME)) oss
        ;; BUILDS UP THFIND EXPRESSION
        (let [finder (plnr-findify 'ALL (variable? oss) (list (variable? oss)) (plnr-describe (relations? oss) (variable? oss) (list (variable? oss))))]
            (putprop! oss 'PLNRCODE= finder)
            (set! *who* nil)
            (loop [finder finder]
                (let-when [candidates (thval2 *who* finder)
                      ? (cond
                            (not candidates) :toofew
                            (number? (num? oss)) (cond (< (count candidates) (num? oss)) :toofew (> (count candidates) (num? oss)) :toomany)
                            (= (num? oss) 'NS) (cond (nil? candidates) :toofew (cdr candidates) :toomany)
                            (memq (num? oss) '(NPL SG-PL)) nil
                            :else (bug! 'smng3 "SCREWY NUMBER PROPERTY OF OSS"))
                ] ? => (do (putprop! oss 'REFER= candidates) oss)
                    (let-when [?
                            (loop [? ? mung nil]
                                (let-when [[? mung]
                                        (when' (= ? :toofew) => [nil mung]
                                            ;; WE DIDN'T FIND ANY (OR ENOUGH) REFERENTS FOR THE NG
                                            [(cond
                                                (or (not discourse?) (nil? *who*))
                                                    (do (set! *oops* (str "I DON'T KNOW WHAT YOU MEAN BY \"" (from *nb* *n*) "\"."))
                                                        :break)
                                                ;; IF WE AREN'T REMEMBERING SENTENCES, FORGET IT IF WE JUST TRIED TO
                                                ;; FIND EVERYTHING (OR EVERYTHING THAT "HE" KNOWS ABOUT), THEN FAIL
                                                (memq *who* '(HE nil))
                                                    (do (set! *oops* (str "I DON'T KNOW WHICH " (cdr (from *nb* *n*)) " YOU MEAN."))
                                                        :break))
                                            true])
                                ] (not ?) => ?
                                    ;; ELSE SET UP TO EXPAND THE SENTENCES WE'RE LOOKING AT
                                    (let [finder (if (memq *who* '(HE nil)) (plnr-mung finder candidates) finder)]
                                        ;; RESTRICT THE POSSIBLE REFERENTS TO BE AMONG THE LIST ALREADY FOUND
                                        (cond
                                            (nil? *who*) (do (set! *who* 'HE) nil)
                                            (= *who* 'HE) (do (set! *who* (list (dec *lastsentno*) (inc *lastsentno*))) nil)
                                            (or (not mung) (== (car *who*) 1)) (do (set! *who* 'HE) (recur :toofew mung))
                                            :else (do (set! *who* (cons (dec (car *who*)) (cdr *who*))) nil)))))
                    ] (not ?) => nil
                        (recur finder)))))))

(defn- smone []
    (loop [x (loop-when [a *h*] a
                (let [w (root (firstword a)) x (getprop w 'CONTRAST)]
                    (if x (list x w) (recur (cdr a)))))]
        (cond (and (move-pt 'C 'U 'U '(NG)) (smone2 (list (car *pt*)) x)) *sm*
            (or (smone2 (parsenode? *lastrel*) x) (smone2 *backref* x) (smone2 *ansname* x) (smone2 *backref2* x)) *sm*
            x (recur nil)
            (and (move-pt 'LASTSENT 'DLC 'PV '(NG)) (smone2 (list (car *pt*)) x)) *sm*
            :else (bug! 'smone "CAN'T FIND REFERENT FOR \"ONE\""))))

(defn- smone2 [a contrast]
    ;; SMONE2 TAKES IN A LIST OF NOUN GROUP NODES AND TRIES TO SEE
    ;; IF ANY OF THOSE NOUN GROUPS COULD BE THE REFERENT FOR "ONE".
    (binding [*word-being* nil]
        ;; IF X IS EMPTY, FAIL.
        ;; TRY TO SEE IF FIRST NG OF X SATIFIES CONTRAST AND/OR COULD BE REFERENT, ELSE TRY NEXT NG IN X.
        (when (loop-when a a (set! *word-being* (smone3 a contrast)) (or *word-being* (recur (cdr a))))
            ;; AT THIS POINT WORD-BEING-INTERPRETED IS (SHOULD BE) A LIST OF WORD NODES OF THE NG,
            ;; WHICH IS THE REFERENT FOR "ONE".  WE NOW PROCEED TO BUILD UP AN OSS FOR THE "ONE" NG.
            ;; THE LIST IS IN ORDER (NOUN ADJ ... ADJ ETC NUM DET).  ONLY THE NOUN AND THE ADJ'S ARE USED.
            (when-not (isq *word-being* 'NOUN)
                (bug! 'smone2 "REFERENT OF \"ONE\" IS SCREWED UP"))
            (loop []
                (eval (semantics *word-being*))                          ;; EVAL THE NOUN DEFINITION
                (set! *word-being* (cdr *word-being*))
                (when (and *word-being* (isq *word-being* 'ADJ))    ;; IF WE REACHED END OF ADJECTIVES, STOP
                    (recur)))
            *sm*)))

(defn- smone3 [a contrast]
    ;; SMONE3 TAKES AN NG, WHICH IS A POSSIBLE REFERENT FOR "ONE".
    ;; IT FIRST CUTS THE NG TO BE ONLY (NOUN ADJ ... ADJ ETC), I.E. IT STRIPS OF QUALIFYING PHRASES.
    ;; IF THERE IS NO CONTRAST, THEN THIS MUNGED NG IS RETURNED AS THE REFERENT.
    ;; IF THERE IS A CONTRAST, THEN IT CHECKS TO SEE IF THE NG SATISFIES THAT CONTRAST.
    (when-not (isq a 'NG)
        (bug! 'smone3 "ONE REFERENT IS NOT A NG" a))
    (let-when [a (loop-when-recur [a (daughters a)] (and a (not (isq a 'NOUN))) [(cdr a)] => a)] (and a contrast) => a
        (loop [a (reverse a)]
            (let [x (root (firstword a))]
                (cond (and (= (car contrast) (getprop x 'CONTRAST)) (not= (cadr contrast) x))
                        (reverse (cdr a))
                    (cdr a) (recur (cdr a)))))))

(defn- smposs []
    ;; SMPOSS WORKS BY ACTING LIKE SMCL1 AND SETTING UP AN RSS (HAVE X Y).  NODE IS THE NODE OF THE POSSESSIVE
    ;; WHICH HAS ALREADY BEEN SEMANTICALLY PROCESSED.  ITS SM CONTAINS THE OSS'S FOR WHOSE DOING THE POSSESSING.
    ;; THE SM CURRENTLY ACTIVE IS THE THING BEING POSSESSED.
    (let [node (smposs2 *c* (move-pt 'H 'PV '(POSS)))]
        (and node (smrelate node))))

(dynamic- *smsub*)
(dynamic- *smob1*)
(dynamic- *smob2*)
(dynamic- *smobl*)
(dynamic- *smcomp*)
(dynamic- *rellist*)

(defn- smposs2 [headnode modnode]
    (binding [*sm* nil *smsub* (semantics modnode) *smob1* (semantics headnode) *smob2* nil *smobl* nil *smcomp* nil *rellist* *smob1*]
        (smset '(!have))
        (and *sm* (let [x (gensym 'NODE)] (and x (putprop! x 'SEMANTICS *sm*) (list x))))))

(defn- smrelate [node]
    ;; RELATES A DESCRIPTIVE RSS TO ANOTHER RSS OR OSS ADDING IT TO THE LIST OF RELATIONS.
    ;; IT TAKES THE LIST OF SS IN SM, AND REPLACES SM WITH THE NEW LIST OF MODIFIED SS'S.
    ;; THE MODIFYING RSS'S HAVE TO HAVE ONE OF THE SM SS'S AS A REL
    ;; (WHICH SHOULD ALWAYS BE TRUE IF THEY WERE SET UP PROPERLY).
    ((lambda [x] (and x (smset x)))
        (doall (map (lambda [rss]
            (let [rel (rel? rss)]
                (when-not (memq rel *sm*)
                    (bug! 'smrelate "TO WHOM?"))
                (build
                    'OSSNODE= (and (oss? rel) (gensym 'OSS))
                    'RSSNODE= (and (rss? rel) (gensym 'RSS))
                    'MARKERS= (or (and (relmarkers? rss) (car (relmarkers? rss))) (markers? rel))
                    'SYSTEMS= (or (and (relmarkers? rss) (cadr (relmarkers? rss))) (systems? rel))
                    'PLAUSIBILITY= (plausibility? rss)
                    'PARSENODE= (parsenode? rel)
                    'AMBIGUITIES= (ambiguities? rss)
                    'VARIABLE= (variable? rel)
                    'NEGATIVE= (negative? rel)
                    'DETERMINER= (determiner? rel)
                    'RELATIONS= (cons rss (relations? rel))
                    'REL= (rel? rel))))
            (semantics node)))))

(dynamic- *time*)

(defn- smcl1 []
    (binding [*smsub* (getr *c* 'LOGICAL-SUBJECT) *smob1* nil *smob2* nil *smobl* nil *smcomp* nil *rellist* nil]
        ;; SET UP GLOBAL VARIABLES WHICH CONSIST OF POINTERS TO THE SEMANTIC DEFINITIONS
        ;; OF THE VARIOUS NOUN-GROUPS (ALSO RSNG'S) REQUIRED BY THE TRANSITIVITY OF THE VERB.
        (set! *smsub*
            (cond *smsub* (semantics *smsub*)
                (cq 'IMPER) '(SHRDLU-OSS)
                (not (cq 'PASV)) (semantics (or (getr *c* 'SUBJECT) (bug! 'smcl1 "NO SUBJECT")))
                (cq 'AGENT) (bug! 'smcl1 "AGENT MISSING")
                :else '(UNKNOWN-OSS-BY)))
        (set! *smob1* (semantics (getr *c* (if (cq 'PASV) 'SUBJECT 'OBJ1))))
        (set! *smob2* (semantics (getr *c* 'OBJ2)))
        (set! *smobl* (semantics (getr *c* 'LOBJ)))
        (set! *smcomp* (semantics (getr *c* 'COMP)))
        ;; NATURALLY SEVERAL OF THESE GLOBAL VARIABLES (BOUND IN THIS PROG AND ACCESSED IN DEEPER ONES)
        ;; ARE NIL AT THIS POINT IN THE PROCEDURE.  THE FOLLOWING CHECKS ARE PRIMARILY FOR DEBUGGING PURPOSES
        ;; (HENCE THE "ERT") TO INSURE THAT THE NON-NIL REGISTERS AND THE TRANSITIVITY OF THE VERB ARE
        ;; BEING MATCHED IN EVERY CASE.
        (or *smsub* (and (meet '(THERE ITRNS) *fe*) (bug! 'smcl1 "TRANSITIVITY")))
        (or *smob1* (and (or (cq 'TRANS) (not (cq 'CLAUSE))) (bug! 'smcl1 "TRANSITIVITY")))
        (or (and *smob1* *smob2*) (and (cq 'TRANS2) (bug! 'smcl1 "TRANSITIVITY")))
        (or (and *smob1* *smobl*) (and (cq 'TRANSL) (bug! 'smcl1 "TRANSITIVITY")))
        (or *smcomp* (and (cq 'INT) (bug! 'smcl1 "TRANSITIVITY")))
        (set! *rellist*
            (semantics (cond (cq 'RSQ) (getr *c* 'RELHEAD) (or (cq 'PREPG) (cq 'ADJG)) (getr *c* 'LOGICAL-SUBJECT) (cq 'QUEST) (getr *c* 'RELHEAD))))
        (when (and (not *rellist*) (or (cq 'POLAR) (cq 'DECLAR)))
            ;; FIND RELATIVE ELEMENT FOR POLAR CLAUSES WHICH CONTAIN INDEFINITE.
            ;; APPLIES TO TOPLEVEL CLAUSES SINCE ONLY THEY CAN HAVE FEATURES POLAR OR DECLAR.
            (let-when [x (relfind *c*)] x
                (when' (any = x *smsub* *smob1* *smob2* *smobl* *smcomp*) => (bug! 'smcl1 "POLAR REL DOESN'T MATCH" x)
                    (set! *rellist* x))))
        (set! *time* (getr (move-pt 'C 'U '(CLAUSE)) 'TIME))
        ;; THIS REFERS TO THE SEMANTIC SENSE OF THE VERB WHICH WILL PROBABLY VARY WITH ITS TRANSITIVITY.
        ;; THE VALUE THAT IS FINALLY DETERMINED REPRESENTS ALL POSSIBLE SENSES OF THE MEANING OF THE WORD
        ;; THAT ARE APPROPRIATE TO THE TRANSITIVITY FIGURED OUT BY THE SYNTACTIC PROGRAMS.
        (let [sense-of-verb
                (cond
                    (cq 'PREPG) (semantics (set! *word-being* (getr *c* 'HEAD)))
                    (cq 'ADJG) (semantics (set! *word-being* (getr *c* 'ADJGHEAD)))
                    :else (cadr (assq (car (meet *fe* '(ITRNS TRANS INT TRANSL TRANS2 THERE ITRNSL))) (semantics (set! *word-being* (getr *c* 'MVB))))))]
            ;; THIS DETERMINES THE APPROPRIATE SEMANTIC INTERPRETATION(S) FOR THE CLAUSE BY CHECKING
            ;; THE RESTRICTIONS OF EACH DEFINITION AGAINST THE MARKERS OF THE VARIOUS CANDIDATES FOR SMSUB,
            ;; SMOB1, ETC.  THE VALUE OF THE EVALUATION IS A LIST OF RELATION-SEMANTIC-STRUCTURES, ONE FOR
            ;; EACH PLAUSIBLE INTERPRETATION.
            (smset (eval sense-of-verb))
            ;; SMCL-MODIFIERS WILL EXAMINE ALL OF THE CONSTITUENTS OF THE CLAUSE THAT WERE NOT INVOLVED
            ;; IN THE BUILDRSS AND WILL EVALUATE THE MEANINGS OF EACH IN TURN FOR THEIR EFFECT ON THE
            ;; ESTABLISHED SM, THE PARSING TREE, OR ANYTHING ELSE THAT WOULD BE APPROPRIATE.
            ;; THE VALUE OF SMCL1 IS NON-NIL ONLY IF SOME REASONABLE MEANING HAS BEEN FOUND FOR THE CLAUSE.
            (smcl2 *h*)
            *sm*)))

(defn- smcl2 [node]
    ;; AS IN SMCL1, WE NEED TO SCAN THE CONSTITUENTS OF THE CLAUSE AND ALLOW THEM TO MAKE
    ;; WHATEVER MODIFICATION ARE APPROPRIATE.
    (dorun (map* smcl-modifiers node))
    node)

(defn- smcl-modifiers [x]
    ;; AS IN CONSTITUENT, THIS PROCEDURE IS BASICLY ONE LARGE DISPATCH TABLE WHICH
    ;; ARRANGES THAT THE PROPER KIND OF PROCESSING HAPPEN TO THE APPROPRIATE CONSTITUENT.
    ;; SOME SHOULD BE IGNORED SINCE THEY HAVE ALREADY BEEN DEALT WITH AND OTHERS SHOULD
    ;; BE EVALUATED AS MODIFIERS OR FOR THEIR SIDE-EFFECTS.
    (cond (nil? (getprop x 'FEATURES)) true
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
    (meet *fe* '(THERE LOBJ COMP PRT SUBJ OBJ1 OBJ2)) true
    (isq x 'NG)
        (and (cond (isq x 'TIM) true
                (and (cq 'REL-NOT-FOUND) (isq x 'QUEST) (isq (daughters x) 'TIM1))
                    (do (rq 'REL-NOT-FOUND)
                        (fq! 'TIMEQ)))
            (smtime))
    ;; IN WHICH CASE IT WAS ALREADY PROCESSED MIGHT GO AWAY IN A FEW DAYS
    ;; BUG CHATCHER MIGHT WANT TO CHANGE THAT THE REST ARE HOOKS FOR WHEN
    ;; WE FIGURE OUT WHAT TO DO WITH THEM
    (isq x 'PREPG)
        (or (isq x 'AGENT)
            (bug! 'smcl-modifiers "BAD PREPG" x))
    (isq x 'QADJ)
        (or (meet *fe* '(LOBJ COMPQ))
            (eval (semantics x)))
    (isq x 'BOUND) true
    (isq x 'BINDER) true
    (isq x 'QUEST) true
    (isq x 'CLAUSE) true
    :else (bug! 'smcl-modifiers "ATTEMPTED TO PROCESS AN UNEXPECTED TYPE OF CONSTITUENT" x)))

(dynamic- *tss*)

(defn- smbind []
    ;; DOES THE SM HAVE MORE THAN ONE VALUE???
    (when (cdr (semantics *h*))
        (oops! "I DON'T KNOW WHAT TO DO WITH AMBIGUOUS BOUND CLAUSES."))
    (binding [*tss* nil *start* nil *end* nil]
        ;; DISPATCH TABLE TO MATCH THE APPROPRIATE ACTION WITH EACH BINDER.
        ;; MOVE TO THE FIRST WORD OF THE CLAUSE (TO THE BINDER) AND CHECK FOR THE FEATURE TIME
        ;; (MAYBE ALSO CHECK FOR THE SM BEING MARKED AS AN EVENT???)
        (when (isq (move-pt 'H 'DF) 'TIME)
            (set! *tss* (getr *c* 'TIME))
            (let [e (findevents (car (semantics *h*)))]
                (when-not e
                    (oops! "NO SUCH THING EVER HAPPENED."))
                (set! *start* (getprop (car e) 'START))
                (set! *end* (getprop (car e) 'END))
                (eval (semantics *pt*))
                true))))

(defn- smbinder [tss start end]
    ;; CALLED FOR A BINDER - THE FIRST ARGUMENT GIVES THE BEGINNING, SECOND THE END.
    ;; A TYPICAL USE IS THE DEFINITION OF "AFTER", WHICH IS (SMBINDER END NIL) I.E.
    ;; THE EVENT STARTS AFTER THE END OF THE BOUND EVENT, WITH NO SPECIFICATION ON WHEN IT ENDS.
    (putprop! tss 'START= start)
    (putprop! tss 'END= end))

#_(ns shrdlu.smutil)

;; ############################################################
;;
;;                          SMUTIL
;;
;; ############################################################

(defn- istense [node arg]
    (let [x (getr node 'TIME)]
        (when-not x (bug! 'istense "NO TIME REGISTER"))
        (let [x (tense? x)]
            (when-not x (bug! 'istense "NO TENSE"))
            (condp = arg
                'PRESENT (memq x '((PRESENT) (PRESENT PRESENT)))
                'FUTURE (= x '(FUTURE))
                'MODAL (= x '(MODAL))
                'IMPERF (and (cdr x) (= (car x) 'PRESENT))
                (bug! 'istense "FUNNY ARG" arg)))))

(defn- imperf? [x] (let [x (tense? x)] (and (cdr x) (= (car x) 'PRESENT))))

(defn- build
    ;; BUILD AN OSS OR RSS FROM A LIST OF KEYWORDS FOLLOWED BY THEIR VALUES.
    ;; POSSIBLE KEYWORDS:
    [& {ossnode      'OSSNODE=      ;; NODE NAME FOR AN OSS
        rssnode      'RSSNODE=      ;; NODE NAME FOR AN RSS
        tssnode      'TSSNODE=
        ansnode      'ANSNODE=
        negative     'NEGATIVE=
        refer        'REFER=
        plnrcode     'PLNRCODE=
        markers      'MARKERS=      ;; LIST OF SEMANTIC MARKERS
        systems      'SYSTEMS=      ;; LIST OF SYSTEMS FOR THOSE MARKERS -- USED FOR FAST MARKER CHECKING?
        relmarkers   'RELMARKERS=
        plausibility 'PLAUSIBILITY= ;; INTEGER BETWEEN 0 AND 512 USED IN DISAMBIGNUATION
        determiner   'DETERMINER=   ;; DETERMINER INFORMATION -- A 3-LIST
        conjuncts    'CONJUNCTS=    ;; LIST OF CONJUNCTS
        disjuncts    'DISJUNCTS=    ;; LIST OF DISJUNCTS
        ambiguities  'AMBIGUITIES=  ;; LIST OF POTENTIAL AMBIGUITIES FOR THIS INTERPRETATION
        relations    'RELATIONS=
        variable     'VARIABLE=     ;; NAME OF VARIABLE TO BE USED IN BUILDING PLANNER CODE
        rel          'REL=
        parsenode    'PARSENODE=    ;; CORRESPONDING NODE OF PARSE TREE
        tense        'TENSE=
        ansrss       'ANSRSS=
        action       'ACTION=
    :as m}]
    ;; VALUE: THE NODE NAME OF THE OSS OR RSS CONSTRUCTED.  AN ATOM.
    ;; SIDE-EFFECTS: USES PUTPROP TO ATTACH PROPERTIES TO NODE NAMES.
    (let [? (and rssnode (not markers))
          markers (if ? '(!RELATION) markers) m (if ? (assoc m 'MARKERS= markers) m)
          ? (and markers (not systems))
          [markers systems] (if ? (let [x (check-markers markers nil nil)] [(car x) (cadr x)]) [markers systems]) m (if ? (assoc m 'MARKERS= markers 'SYSTEMS= systems) m)
          node (or ossnode rssnode tssnode ansnode (bug! 'build "NO NODE="))]
        (reduce-kv #(when %3 (putprop! node %2 %3)) nil m)
        node))

(defn- newcopy [oss]
    (let [new (gensym 'OSS)]
        ;; WATCH OUT -- THIS IS IMPLEMENTATION DEPENDENT,
        ;; AND GETS THE ENTIRE PROPERTY LIST IN OUR LISP.
        (loop [old (mapcat concat (getprops oss))]
            (if (some? old)
                (do (putprop! new (car old) (cadr old)) (recur (cddr old)))
                (do (putprop! new 'PARSENODE= *c*) new)))))

(dynamic- *relß*)
(dynamic- *relmarkersß*)
(dynamic- *!1*)
(dynamic- *!2*)
(dynamic- *!3*)

(defn- relation [& a]
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
    ;;   PLAUSIBILITYß   EVALUATED TO GET INTEGER FROM 0 TO 512 INDICATING RELATIVE LIKELIHOOD OF THIS DEFINITION SENSE.
    ;;   MARKERSß        SEMANTIC MARKERS
    ;;
    ;; VALUE:
    ;;   LIST OF RSS NODE NAMES CREATED.
    ;;
    ;; SIDE-EFFECTS:
    ;;   CREATES AN RSS BY CALLING BUILD.
    (iterate* (lambda [a smsub' smob1' smob2' smobl' smcomp']
        (binding [*smsub* smsub' *smob1* smob1' *smob2* smob2' *smobl* smobl' *smcomp* smcomp']
            ;; NOTICE THAT WITHIN THIS LAMBDA EXPRESSION THAT
            ;; SMSUB = ONE OSS FOR SEMANTIC SUBJECT
            ;; SMOB1 = ONE OSS FOR SEMANTIC OBJECT 1
            ;; SMOB2 = ONE OSS FOR SEMANTIC OBJECT 2
            ;; SMOBL = ONE OSS FOR LOCATIVE OBJECT
            ;; SMCOMP = ONE OSS FOR SEMANTIC COMPLEMENT
            ;; WHEREAS OUTSIDE OF THE LAMBDA EXPRESSION EACH OF THESE NAMES REPRESENTS A LIST OF THE SAME.
            ;; THIS IS TO ALLOW DICTIONARY WRITERS TO USE THESE SELF SAME NAMES IN WRITING DEFINITIONS, A SIMPLY TERRIBLE IDEA.
            (let [{:keys [restrictions procedure plausibility paraphrase markers]} a]
                (binding [*relß* nil *relmarkersß* nil *!1* nil *!2* nil *!3* nil]
                    ;; RESTRICTIONSß IS EXPANDED HERE PUTTING IN IMPLICIT REGISTER REFERENCES, SO IT CAN BE UNIFORMLY GOBBLED BELOW.
                    ;; %MARKL IS A SINGLE MARKER LIST FROM ON OF THE RESTRICTIONS IN THE DEFINITION, E.G. (!PHYSOB !RED).
                    ;; %RESTRICTNAM IS A NAME LIKE SMSUB, SMOBL, SMCOMP, .... WHICH REFERS TO REGISTERS ELSEWHERE
                    ;; IN THE PROGRAM WHOSE MARKERS MUST BE COMPATIBLE WITH %MARKL AS CHECKED BELOW.
                    ;; %NUM IS THE NUMBER WHICH WILL BE USED TO SUBSTITUTE IN THE DICTIONARY EXPRESSION.
                    (let [restrictions
                            (doall (map (lambda [name marks num]
                                    (let [x (if (term? (car marks)) marks (cons name marks))] (SET num (eval (car x))) x))
                                '(*smsub* *smob1* *smob2*) restrictions '(*!1* *!2* *!3*)))]
                        ;; CHECK THAT THIS DEFINITION SENSE MEETS ALL OF THE RESTRICTIONS SET FORTH IN THE DEFINITION UNDER RESTRICTIONSß.
                        (when (ERRSET
                                ;; ENCLOSED IN A ERRSET SO THAT THE FAILURE OF A CHECK CAN CAUSE IMMEDIATE ESCAPE FROM THE MAPC
                                ;; AND HENCE TO THE AND WHICH CUTS OFF ALL FURTHER PROCESSING OF THIS DEFINITION SENSE.
                                ;; TEMPORARY STORAGE ON THE PROPERTY LIST OF TEMP USED TO AVOID SEARCHING FOR THESE ITEMS ON THE
                                ;; ONE HAND OR THE CONFLICT OF NAMES BETWEEN THE MARKERS RESULTING FROM CHECKING THE REL ARE
                                ;; SAVED TO PUT ON IT LATER WHEN THE CLAUSE IS RELATED.
                                (dorun (map (lambda [marks]
                                    (let [oss (eval (car marks)) x (checkrel oss)]
                                        (when x (set! *relß* (car x)))
                                        (let [check (when (or (nil? (cddr marks)) (eval (caddr marks))) (check-markers (cadr marks) (markers? oss) (systems? oss)))]
                                            (cond (not check) (ERR nil)
                                                (= oss *relß*) (set! *relmarkersß* check))
                                            nil)))
                                    restrictions)))
                            ;; SUBJECT RESTRICTION MARKERS USED IN THE DEFINITION AND THE REGISTERS OF THE SAME NAME REFERENCED AS FREE
                            ;; VARIABLES IN THIS PROGRAM.  ON THE OTHER HAND, IF THE RESTRICTIONS HAVE BEEN MET, THEN BUILD AN RSS NODE.
                            ;; RSSNODE= IS KEYWORD FOR INPUT INTO BUILD OF RSS NODE NAME.  IN THE CALL TO BUILD THE ITEMS ENDING IN =
                            ;; ARE KEYWORDS WHOSE VALUE IS THE FOLLOWING ITEM.  MARKERSß, OF COURSE IS A VARIABLE IN THIS FUNCTION.
                            ;; THIS CALL JUST SAYS SEND TO BUILD FOR MARKERS= THE VALUE SENT TO RELATION FOR MARKERSß.  THE PARSENODE
                            ;; IS THE CURRENT NODE.  ***, #1, #2, AND #3 SUBSTITUTIONS DONE.  THIS IS FIRST STEP IN BUILDING PLANNER CODE.
                            ;; NUMSUB IS THE ***, #1, #2, #3 SUBSTITUTION FUNCTION.  %PLNRPHRASE IS ONE CHUNK OF CONDENSED PLANNER CODE
                            ;; LIKE (!COLOR *** !RED).
                            (let [name (gensym 'RSS)] (build
                                'RSSNODE= name
                                'MARKERS= markers
                                'VARIABLE= (let [x (gensym 'EVX)] (putprop! x 'RSSVAR x))
                                'PARSENODE= *c*
                                'RELATIONS= (reverse (doall (map #(plnr-numsub '<<<RELATION-ERROR>>> %) (evalcheck procedure))))
                                'REL= *relß*
                                'NEGATIVE= (and (cq 'NEG) true)
                                'RELMARKERS= *relmarkersß*
                                'PLAUSIBILITY= (+ (plausibility? *smsub*) (plausibility? *smob1*) (plausibility? *smob2*) (or (eval plausibility) 0))
                                'AMBIGUITIES= (concat (ambiguities? *!1*) (ambiguities? *!2*) (ambiguities? *!3*) (and paraphrase (list (list name paraphrase *word-being*))))))))))))
        a *smsub* *smob1* *smob2* *smobl* *smcomp*))

(defn- dobackref [answer]
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
        (or (cq 'MODAL) (cq 'DECLAR)
            (dorun (map* #(let [a (semantics %)]
                    (when (cdr a) ;; TRUE IF NODE HAD MULTIPLE INTERPRETATIONS
                        (bug! 'dobackref "RETURN AN OSS FOR BACKNODE" a))
                    (or (refer? (car a)) ;; IF NODE HAS REFERENT, FINE
                        (putprop! (car a) 'REFER=
                            (or (getprop (variable? (car a)) 'BIND) (bug! 'dobackref "RETURN REFERENT FOR BACKNODE" a)))))
                *backref2*)))
        ;; A FEW MISSING PIECES GO HERE
        nil))

(defn- evalcheck [l]
    ;; EVALCHECK CHECKS FOR THE PRESENCE OF (!EVAL (MUMBLE ...) ...) IN THE INPUT S-EXPRESSION L.
    ;; IF IT FINDS ONE THEN THE EXPRESSION MUMBLE IS EVALUATED AND REPLACES (!EVAL ...), OTHERWISE
    ;; L IS RETURNED JUST THE WAY IT WAS.  HENCE THIS FUNCTION IS THE INVERSE OF QUOTE.
    (cond (term? l) l (= (car l) '!EVAL) (eval (cadr l)) :else l))

(dynamic- *%X*)
(dynamic- *%XL*)

(defq- iterate* [& l]
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
    (binding [*%X* nil *%XL* nil] (eval (iteratex (car l) (cdr l)))))

(defn- iteratex [fun l]
    ;; EXPANDS CALL ON ITERATE INTO A TAILORED SET OF MAPBLAND
    ;; CALLS WHICH DO THE APPROPRIATE COMPUTATION WHEN EVALUATED.
    ;; INPUTS:
    ;;      FUN    FUNCTION OF N ARGUMENTS
    ;;      L      LIST OF N LISTS WHICH FUN IS TO BE APPLIED TO
    ;; VALUE:
    ;;  TAILORED FUNCTION
    (if (nil? l) (cons (eval fun) *%XL*)
        (do (set! *%X* (gensym)) (set! *%XL* (concat *%XL* (cons *%X* nil)))
            (list 'mapbland
                (list 'lambda
                    ;; %X IS USED TO STORE THE VARIABLE NAME WHICH IT GETS FROM (GENSYM)
                    ;; %XL IS USED TO SAVE A LIST OF ALL OF THE VARIABLE NAMES SO FAR SO
                    ;; THAT THEY CAN BE GIVEN TO THE FUNCTION AT THE END (CONS %X NIL).
                    ;; CREATES A SHINY NEW CELL WHICH IS BOLTED ONTO THE BACK END OF %XL
                    ;; BY NCONC.  THIS PAIR IS NECESSARY BECAUSE SETQ RETURNS AS ITS VALUE
                    ;; THE RESULT OF THE LAST PAIR THAT IT PROCESSES.  A RUSE. PUTS (NIL)
                    ;; IN PLACE OF NIL AS A LIST TO PREVENT MAPBLAND FROM QUITTNG.
                    (list *%X*)
                    (iteratex fun (cdr l)))
                (or (car l) '(nil))))))

(defn- mapbland [fun l]
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
    (loop [a nil l (if (nil? l) '(nil) l)]
        (let [x (fun (car l)) a (cond (nil? x) a (term? x) (concat a (cons x nil)) :else (concat a x)) l (cdr l)]
            (if l (recur a l) a))))

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

(defn- object [& a]
    ;; %DEFL IS THE LIST OF DEFINITION SENSES.
    ;; CONSTRUCTS OSS FOR GARDEN VARIETY NOUNS AND ADJECTIVES.
    ;; USED IN DEFINITIONS.
    ;;
    ;; INPUTS:
    ;;  INPUTS ARE IN KEYWORD FORMAT (ATTRIBUTE-VALUE PAIRS).
    ;;  THE KEYWORD IS NOT EVALUATED BUT ITS VLUE IS.
    ;; POSIBLE KEYWORDS:
    ;;     MARKERSß            LIST OF SEMANTIC MARKERS
    ;;     PLAUSIBILITYß       EVALS TO INTEGER FROM 0 TO 512 INDICATING RELATIVE LIKELIHOOD OF THIS DEFINITION SENSE
    ;; FREE VARIABLE INPUT:
    ;;     SM  -  A LIST OF CURRENT OSS'S WITH WHICH THE TO-BE-CREATED ONES MUST BE COMPATIBLE.
    ;; VALUE:
    ;;     A NEW LIST OF OSS'S TO TAKE THE PLACE OF THE OLD ONE.
    ;; SIDE-EFFECTS:
    ;;  SM IS RESET TO THE VALUE OF OBJECT.
    ;;  A SET OF OSS'S ARE CREATED AT THE GLOBAL LEVEL.
    (smset
        ;; %OSS IS A SINGLE OSS NODE NAME PICKED OFF OF SM.
        ;; %DEF IS A SINGLE DEFINITION SENSE PICKED OFF OF %DEFL.
        (iterate* (lambda [oss a]
            (let [{:keys [markers plausibility paraphrase procedure]} a]
                ;; CHECK FOR MARKER AGREENT.  IF OK, THEN BUILD OSS, ELSE CHUCK THIS COMBINATION.
                (let-when [ms* (check-markers markers (markers? oss) (systems? oss))] ms*
                    ;; BUILD OSS COMBINING INFORMATION FROM CURRENT OSS WITH INFORMATION IN THE DEFINITION.
                    ;; NOTE THAT THE INITIAL OSS WHICH GETS BUILT UP FOR A WORD DEPENDS NOT ONLY ON ITS DEFINITION,
                    ;; BUT ALSO ON THE CONTEXT IN WHICH IT IS USED.
                    (build
                        'OSSNODE= (gensym 'OSS)
                        'PARSENODE= (parsenode? oss)
                        'MARKERS= (car ms*)
                        'SYSTEMS= (cadr ms*)
                        'DETERMINER= (determiner? oss)
                        'VARIABLE= (variable? oss)
                        'RELATIONS= (concat (reverse (doall (map #(plnr-numsub oss %) (evalcheck procedure)))) (relations? oss))
                        'REL= (rel? oss)
                        ;; THE OSS NAME PROVIDES A UNIQUE LABEL FOR WHERE THE AMBIGUITY OCCURRED FOR LATER COMPARISON.
                        'AMBIGUITIES= (concat (ambiguities? oss) (when paraphrase (list (list oss paraphrase *word-being*))))
                        'PLAUSIBILITY= (+ (or (eval plausibility) 0) (plausibility? oss))))))
            *sm* a)))

;; ######################################################
;;
;;              PLANNER BUILDING ROUTINES
;;
;; ######################################################

(defn- valueput [] (putprop! *value* 'WHO *sentno*))

(defn- plnr-junkify [code]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (cond
        (term? code) code
        (= (car code) 'thgoal) (list 'thand code '(valueput))
        (= (car code) 'thfind) (list 'thand code (list 'thputprop (quotify (cadr (caddr code))) ''BIND '*value*))
        (or (= (car code) 'thand) (= (car code) 'thprog)) (doall (mapcat plnr-junkify2 code))
        :else (doall (map plnr-junkify code))))

(defn- plnr-junkify2 [code]
    ;; PUTS DISCOURSE STUFF INTO CODE
    (cond
        (term? code) (list code)
        (= (car code) 'thgoal) (list code '(valueput))
        (= (car code) 'thfind) (list code (list 'thputprop (quotify (cadr (caddr code))) ''BIND '*value*))
        (or (= (car code) 'thand) (= (car code) 'thprog)) (list (doall (mapcat plnr-junkify2 code)))
        :else (list (doall (map plnr-junkify code)))))

(defn- plnr-thconsify [varlist exp body]
    ;; GENERATES A CONSEQUENT THEOREM.
    (let [th (gensym 'THEOREM)]
        (putprop! th 'THEOREM
            (if (= (car body) 'thprog)
                (concat (list 'thconse (union varlist (cadr body)) exp) (cddr body))
                (list 'thconse varlist exp body)))
        th))

(defn- plnr-findify [mode variable varlist body]
    ;; GENERATES A THFIND STATEMENT FOR THE NOUN GROUP DESCRIBED IN THE OSS.
    ;; IT (CURRENTLY) ASSUMES THAT THE PLNRCODE PROPERTY OF THE OSS IS A LIST
    ;; OF PATERNS OF THGOAL STATEMENTS.  MODE IS DEFINED TO BE <MODE> IN THE
    ;; MICRO-PLANNER DESCRIPTION OF THFIND (SEE AI MEMO !203A) BODY
    ;; IS A SINGLE PLANNER EXPRESSION (POSSIBLY A THAND OR THPROG).
    (let [[varlist body]
            (condp = (car body)
                'thand [varlist (cdr body)]
                'thprog [(concat varlist (cadr body)) (cddr body)]
                [varlist (list body)])]
        (concat (list 'thfind mode (list 'thv variable) varlist) body)))

(defn- plnr-findspec [x]
    ;; GENERATES PARAMETER FOR THFIND FROM THE NOTATION USED IN THE DETERMINER.
    (cond
        (number? x) x
        (memq x '(NS NPL SG-PL)) 1
        (= (car x) 'EXACTLY) (list (cadr x) (inc (cadr x)) nil)
        (= (car x) '>) (inc (cadr x))
        (= (car x) '<) (list 0 (cadr x) nil)
        :else (bug! 'plnr-findspec "FUNNY SPECIFICATION" x)))

(defn- plnr-goalify [a]
    ;; TAKES A PLNRPHRASE AND MAKES A THGOAL STATEMENT OUT OF IT UNLESS
    ;; IT ISN'T SUPPOSED TO BE ONE.  ALSO CALLS PLNR-NOTIFY IF APPROPRIATE.
    ;; PRESENT TENSE TIME MARKERS ARE REMOVED TO SIMPLIFY THE MOST COMMON EXPRESSIONS.
    (let [a (plnr-remtime a)]
        (if (getprop (car a) 'NOGOAL) a
            (concat (list 'thgoal a '(THDBF mumble)) (plnr-recommendify a)))))

(defn- plnr-mung [findexpr candidates]
    ;; DOES A HORRIBLE THING: MUNGS A THFIND EXPRESSION WHICH FINDS A NOUN GROUP REFERENCE.
    ;; IT PUTS A THAMONG EXPRESSION AS THE FIRST STATEMENT OF THE THFIND.  IF THERE IS ALREADY
    ;; A THAMONG EXPRESSION IN THE THFIND, THEN MUNG JUST CLOBBERS THE LIST IN THAT THAMONG.
    (cons (car findexpr)
        (cons (cadr findexpr)
            (cons (caddr findexpr)
                (cons (cadddr findexpr)
                    (cons (list 'thamong (caddr findexpr) (quotify candidates))
                        (cond (= (caaddr (cdr findexpr)) 'thfind) (cddddr (cdr findexpr))
                            :else (cddddr findexpr))))))))

(defn- plnr-notify [neg a]
    ;; PUTS IN THNOT, BUT ELIMINATE DOUBLE NEGATIVES.
    (cond (not neg) a (= (car a) 'thnot) (cadr a) :else (list 'thnot a)))

(dynamic- *newbody*)

(defn- plnr-newbody [x] (set! *newbody* (cons x *newbody*)))

(defn- plnr-progify [varlist body]
    ;; SETS UP A THPROG OR THE SIMPLEST EQUIVALENT EXPRESSION FOR
    ;; THE PARTICULAR CASE.  BODY IS A LIST OF EXPRESSIONS.
    (when body
        (binding [*newbody* nil]
            (dorun (map (lambda [x]
                    (condp = (car x)
                        'thprog
                            (if (meet varlist (cadr x)) (plnr-newbody x)
                                (do (SETQ varlist (concat varlist (cadr x))) (dorun (map plnr-newbody (cddr x)))))
                        'thand
                            (dorun (map plnr-newbody (cdr x)))
                        (plnr-newbody x)))
                body))
            (cond
                varlist (cons 'thprog (cons varlist (reverse *newbody*)))
                (cdr *newbody*) (cons 'thand (reverse *newbody*))
                :else (car *newbody*)))))

(defn- plnr-numrel [oss]
    ;; THIS IS USED BY PLNR-NUMSUB TO HAVE THE VARIABLE NAME SUBSTITUTED
    ;; FOR THE OSS WHICH IS THE REL OF A PARTICULAR EXPRESSION.
    (if (memq oss *rellist*) (set! *relß* oss) oss))

(defn- plnr-numsub [me a]
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
                (memq e '(*!1* *!2* *!3*)) (plnr-numrel (eval e))
                (= e '***) me
                (= e '*TIME) *time* ;; GETS THE CURRENT TIME
                :else e))
        (evalcheck a))))

(defn- plnr-recommendify [a]
    ;; LOOKS UP IN THE DICTIONARY A RECOMMENDED THEOREM TO USE IN PROCESSING A PLNRPHRASE BY THGOAL.
    ;; IF IT FINDS ONE, IT TACKS IT ON AS A RECOMMENDATION.
    (let-when [e (getprop (car a) 'THMLIST)] e
        ;; LOOK A RELATION UP IN THE DICTIONARY.  THE ENTRIES ARE SET UP AS A PROPERTY LIST.
        ;; THERE ARE DIFFERENT RECOMMENDATIONS FOR THE SAME RELATION DEPENDING ON THE NUMBER
        ;; OF ARGUMENTS THIS INSTANCE OF IT HAS.  (LENGTH ...) COMPUTES THE NUMBER OF ARGUMENTS
        ;; + 1 AND THE (ASSQ ...) RETRIEVES THE APPROPRIATE RECOMMENDATION USING THIS NUMBER.
        ;; IF THERE IS NO SUCH NUMBER, NUMBERP FAILS AND SOME ARBITARY FUNCTION WHICH
        ;; IS STORED OUT THERE IS EVALUATED TO GIVE THE RECOMMENDATION.
        (eval (if (number? (caar e)) (cadr (or (assq (count a) e) '(nil nil))) e))))

(defn- plnr-remtime [exp]
    ;; REMOVES ALL PRESENT TENSE TIME STRUCTURES
    (let [y '(T)]
        ;; Y IS BOUND TO A UNIQUE POINTER SO IT CAN'T POSSIBLY SCREW ANYTHING
        ;; IN THE EXPRESSION WHEN IT DOES THE DELETION.  DELQ USES EQ.
        (delq y
            (doall (map (lambda [x]
                (cond (not (term? x)) x
                    (tss? x) (if (and (tense? x) (not (memq (tense? x) '((PRESENT PRESENT) (MODAL) (PRESENT))))) x y)
                    :else x))
            exp)))))

(defn- compare-build [node degree]
    ;; USED BY SMADJG-PREPG TO BUILD A PSUEDO VERB FOR THE COMPARATIVE.  SMCL1 IS THEN CALLED.
    (let [{:keys [restrictions dimension direction]} (cdr (findmeasure node))]
        (putprop! 'COMPARE-PSEUDO-VERB 'SEMANTICS
            (list 'relation
                (vector :restrictions (list (list restrictions) (list restrictions))
                        :procedure (list (list degree dimension (if direction '*!1* '*!2*) (if direction '*!2* '*!1*))))))
        '(COMPARE-PSEUDO-VERB)))

(defn- findmeasure [node]
    ;; GETS THE MEASURE DEFINITION
    (let [r (root (firstword node)) x (assq 'MEASURE (getprop r 'SEMANTICS))]
        (if x (cadr x) (oops! (str "I DON'T KNOW HOW TO COMPARE THINGS WITH RESPECT TO " r)))))

(defn- measure [& a]
    ;; USED TO GENERATE ORDINALS -- IT IS CALLED WHEN A MEASURE DEFINITION IS EVALLED
    (object (vector :markers (cadr (memq :restrictions a)) :procedure (list (list '*ORDINAL* a)))))

(dynamic- *ordinal*)

(defn- plnr-describe [a var freevars]
    ;; BUILDS THE PLANNER DESCRIPTION, IGNORING THE QUANTIFIER ACCOUNTS FOR ORDINALS, SUBSTS, ETC.
    (binding [*ordinal* nil]
        (loop-when [body nil a a] a => (if *ordinal* (ordmake *ordinal* var body) (plnr-progify nil body))
            (let [x (expand (car a) (and (nil? (cdr a)) (rssvar? var) (getprop var 'USED) (list 'thv var)) freevars)
                  body (cond
                        (= x '*ORDINAL*) body
                        ;; A SUBST DEFINITION IF IT IS THE ONLY THING IS TO BE APPLIED TO THE OSS TO WHICH THIS RSS WILL BE RELATED.
                        ;; THE VARIABLE FOR A RELATION IS INSERTED INTO THE SECOND PLACE OF THE RELATION IF IT IS REFERRED TO ANYWHERE ELSE.
                        (and (cdr a) (= (car x) '!SUBST)) (do (mapc2 (lambda [x y] (SETQ a (subst x y a))) (cdr x)) body)
                        x (cons x body)
                        :else body)]
                (recur body (cdr a))))))

(dynamic- *rel*)

(defn- relfind [node]
    ;; LOOKS FOR THE REL OF A POLAR
    (binding [*rel* nil]
        (ERRSET
            ;; IT GOES FROM THE BEGINNING OF THE SENTENCE LOOKING FOR AN INDEFINITE NG,
            ;; EITHER AT THE TOP LEVEL OR AS A FIRST LEVEL PREPOBJ, BUT NOT A COMPLEMENT.
            (dorun (map* (lambda [x]
                (cond
                    (isq x 'NG) (and (not (isq x 'COMP)) (not (isq x 'DEF)) (set! *rel* x) (ERR nil))
                    (isq x 'LOBJ) (and (isq (daughters x) 'INDEF) (set! *rel* x) (ERR nil))
                    (isq x 'PREPG) (and (isq (daughters x) 'INDEF) (set! *rel* (daughters x)) (ERR nil))))
                (reverse (daughters node)))))
        (or *rel* (and (cq 'PASV) (not (cq 'AGENT)) (set! *rel* '(FAKE-AGENT))))
        (and *rel* (semantics *rel*))))

(defn- ordmake [ordinal var body]
    ;; MAKES THE LOGICAL FORM FOR SUPERLATIVES.
    ;; ORDINAL GIVES THE THING BEING COMPARED IN MEASURE FORM.
    (let [x (gensym 'X)]
        (plnr-progify nil
            (concat body (list (plnr-notify true
                (plnr-progify (list x) (concat (subst x var body) (list (plnr-goalify (compare-proc var x ordinal)))))))))))

(defn- compare-proc [var newvar ordinal]
    (let [{:keys [restrictions dimension direction]} ordinal]
        (list '!MORE dimension (list 'thv (if direction newvar var)) (list 'thv (if direction var newvar)))))

(dynamic- *choice*)
(dynamic- *var*)
(dynamic- *body*)

(defn- expand [exp event freevars]
    ;; THE HEART OF THE PLANNER BUILDER.
    ;; EXPANDS AN EXPRESSION WORRYING ABOUT THE QUANTIFIERS AND CONNECTIVES OF ITS CONSTITUENTS.
    ;; IT DOESN'T REALLY HANDLE EXPRESSIONS WITH MORE THAN ONE QUANTIFIED SS UNLESS ONE OF THEM IS THE REL.
    ;; THE EVENT IS NON-NIL ONLY IF THE EVENT-NAME IS TO BE INCLUDED IN THE EXPANSION OF THE EXPRESSION.
    (cond (rss? exp)
            (cond
                (and? exp) (plnr-progify nil (doall (map #(expand % nil freevars) (and? exp))))
                (or? exp) #_(PLNR-ORIFY (doall (map #(expand % nil freevars) (or? exp)))) (bug! 'plnr-orify "NOT WRITTEN YET")
                :else (plnr-notify (negative? exp) (plnr-describe (relations? exp) (variable? exp) (cons (variable? exp) freevars))))
        (term? exp) (bug! 'expand "ATOMIC MODIFIER" exp)
        (= (car exp) '*ORDINAL*) (if *ordinal* (oops! "I CAN'T HANDLE TWO ORDINALS OR SUPERLATIVES AT ONCE.") (do (set! *ordinal* (cadr exp)) '*ORDINAL*))
        (= (car exp) '!SUBST) (bug! 'expand "IS !SUBST BEING HANDLED BY SOMEONE ELSE?")
        :else
            (binding [*quantifier* nil *choice* nil *var* nil *body* nil]
                (let [multiple (eval (getprop (car exp) 'MULTIPLE))
                      exp (doall (map (lambda [x]
                                (cond (or (not (term? x)) (not (or (rss? x) (oss? x)))) x
                                    (refer? x)
                                        (if (cdr (refer? x)) (if multiple (do (erqset 'AND) (set! *choice* (refer? x)) '*AND*) (refer? x)) (car (refer? x)))
                                    (memq (variable? x) freevars)
                                        (do (when (rssvar? (variable? x)) (putprop! (variable? x) 'USED true)) (list 'thv (variable? x)))
                                    (set! *choice* (and? x))
                                        (do (erqset 'AND) (when (and multiple (refer? x)) (set! *choice* (refer? x))) '*AND*)
                                    (set! *choice* (or? x))
                                        (do (erqset 'OR) '*OR*)
                                    (cond
                                        (rss? x)
                                            (do (erqset 'EVENT) (putprop! (variable? x) 'USED true))
                                        (memq (quantifier? x) '(ALL NO))
                                            (do (erqset (quantifier? x)) true)
                                        (memq (quantifier? x) '(NDET INDEF))
                                            (do (cond (memq (num? x) '(NS SG-PL)) (erqset 'INDEF)
                                                    (set! *choice* (plnr-findspec (num? x))) (erqset 'FIND))
                                                true))
                                        (do (set! *body* (plnr-describe (relations? x) (variable? x) (cons (variable? x) freevars)))
                                            (list 'thv (set! *var* (variable? x))))
                                    :else (bug! 'expand "STRANGE QUANTIFIER")))
                            (if event (cons (car exp) (cons event (cdr exp))) exp)))]
                    (cond ;; THE EVENT NAME IS STUCK INTO THE SECOND POSITION IF THERE IS ONE.
                        (nil? *quantifier*) (plnr-goalify exp)
                        (= *quantifier* 'AND) (plnr-progify nil (doall (map #(expand (subst % '*AND* exp) nil freevars) *choice*)))
                        (= *quantifier* 'OR) #_(PLNR-ORIFY (doall (map #(expand (subst % '*OR* exp) nil freevars) *choice*))) (bug! 'plnr-orify "NOT WRITTEN YET")
                        (= *quantifier* 'FIND) (plnr-findify *choice* *var* (list *var*) (plnr-progify nil (cons *body* (list (plnr-goalify exp)))))
                        :else (plnr-notify (memq *quantifier* '(ALL NO))
                                (plnr-progify (and *var* (list *var*)) (cons *body* (list (plnr-notify (= *quantifier* 'ALL) (plnr-goalify exp)))))))))))

(defn- erqset [x]
    ;; USED BY EXPAND TO MAKE SURE IT ISN'T GETTING CONFUSED BY TOO
    ;; MANY CONNECTIVES AND QUANTIFIERS IN THE SAME EXPRESSION.
    (if *quantifier* (oops! "I CAN'T HANDLE COMBINATIONS OF QUANTIFIERS AND CONNECTIVES WHICH ARE SO COMPLICATED.") (set! *quantifier* x)))

(defn- thval2 [who' a]
    (binding [*who* who']
        (thval a '((EV COMMAND)))))

(defn- who [x]
    (or (nil? *who*) (term? x)
        (let-when [x (getprop x 'WHO)] x
            (or (= *who* 'HE) (< (car *who*) x *lastsentno*)))))

(dynamic- *markers*)
(dynamic- *systems*)

(defn- check-markers [a markers' systems']
    ;; TAKES A LIST OF NEW MARKERS AND CHECKS FOR COMPATIBILITY WITH THE EXISTING
    ;; MARKERS AND SYSTEMS (AS GIVEN BY ARGS MARKERS AND SYSTEMS).  IF COMPATIBLE,
    ;; RETURNS A TWO-LIST OF THE NEW MARKERS AND SYSTEMS, ELSE RETURNS NIL.
    (binding [*markers* markers' *systems* systems']
        (loop-when a a => (list *markers* *systems*)
            (recur-if (checkamarker (car a)) (cdr a)))))

(defn- checkamarker [marker]
    ;; CHECKS A SINGLE MARKER FOR COMPATIBILITY.
    ;; USES FREE VARIABLES:
    ;;    SYSTEMS - THE SYSTEM LIST SO FAR
    ;;    MARKERS - THE MARKER LIST SO FAR
    ;; IF SUCCESSFUL, THE MARKER AND ITS SYSTEM(S) ARE APPENDED TO THESE FREE VARIBLES.
    (if (memq marker *markers*) true
        (do (set! *markers* (cons marker *markers*))
            (loop-when [a (getprop marker 'SYS)] a => true
                (cond (memq (car a) *systems*) nil                      ;; FAIL, IF SYSTEM IS THERE BY ANOTHER PATH
                    (checkamarker (car a)) (do (set! *systems* (cons (car a) *systems*)) (recur (cdr a))))))))

(defn- findevents [rss]
    ;; FINDS ALL THE EVENTS FITTING THE RSS DESCRIPTION.
    (putprop! (variable? rss) 'USED true)
    (thval2 nil
        (plnr-findify 'ALL (variable? rss) (list (variable? rss))
            (plnr-describe (relations? rss) (variable? rss) (list (variable? rss))))))

(defn- checkrel [oss]
    ;; CHECKS FOR POSSIBLE RELATIVE, EITHER BECAUSE OSS IS ON THE RELLIST,
    ;; OR BECAUSE RSS INVOLVES INSIDE IT AN OSS ON THE RELLIST.
    ;; IT RETURNS EITHER NIL OR A LIST OF WHICH THE FIRST ELEMENT IS THE REAL RELATIVE.
    ;; IT USES THIS FACT TO CHEAT ON RECURSION BY USING MAPCAN.
    (cond
        (oss? oss) (memq oss *rellist*)
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
    (getprop x 'CONJUNCTS=))

(defn- ansrss? [x]
    ;; THE RSS CORRESPONDING TO AN ANSWER NODE (A PROPERTY OF THE ANSNODE)
    (getprop x 'ANSRSS=))

(defn- determiner? [x]
    ;; ACCESS FUNCTION.  GETS DET OF AN OSS.
    (getprop x 'DETERMINER=))

(defn- end? [tss]
    ;; END TIME FOR TSS.
    (getprop tss 'END=))

(defn- markers? [sense]
    ;; ACCESS FUNCTION USED TO GET MARKERS FROM OSS OR RSS.
    (getprop sense 'MARKERS=))

(defn- modifiers? [xss]
    ;; ACCESS FUNCTION FOR GETTING THE PLANNER CODE SCHEMA OF AN OSS OR RSS.
    (getprop xss 'MODIFIERS=))

(defn- negative? [xss]
    ;; ACCESS FUNCTION FOR OSS.
    (getprop xss 'NEGATIVE=))

(defn- num? [oss]
    ;; GETS THE NUMBER FIELD OF AN OSS.
    (car (getprop oss 'DETERMINER=)))

(defn- or? [x]
    ;; ACCESS FOR LIST OF CONSTITUENTS IN DISJOINED SEMANTIC STRUCTURE.
    ;; NIL IF IT ISN'T.
    (getprop x 'DISJUNCTS=))

(defn- oss? [x]
    ;; CHECKS TO SEE IF X IS AN OSS.
    (getprop x 'OSSNODE=))

(defn- parsenode? [x]
    ;; THE PARSE NODE ASSOCIATED WITH THE SEMANTIC STRUCTURE.
    (getprop x 'PARSENODE=))

(defn- plausibility? [xss]
    ;; ACCESS FUNCTION FOR GETTING PLAUSIBILITY OF AN OSS OR RSS.
    (or (getprop xss 'PLAUSIBILITY=) 0))

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

(defn- refer? [xss]
    ;; ACCESS FUNCTION FOR REFER OF OSS OR RSS.
    (getprop xss 'REFER=))

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

(defn- systems? [sense]
    ;; ACCESS FUNCTION FOR GETTING THE SYSTEMS OF AN OSS OR RSS.
    (getprop sense 'SYSTEMS=))

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

(dynamic- *ambig*)

;; TOP LEVEL ANSWER FUNCTION FOR ANY INPUT SENTENCE, WHETHER IT IS COMMAND, QUESTION, OR STATEMENT.

(defn- answer [node]
    ;; CLEAR OUT ANSWER NAMES SAVED FOR BACKREF(ERENCE), I.E. MORE THAN ONE RSS FOR THE SENTENCE.
    (set! *ansname* nil)
    ;; AMBIG IS A FLAG SET IF THERE IS A POSSIBLE AMBIGUITY.
    (binding [*ambig* (cdr (semantics node))]
        ;; A IS THE LIST OF POSSIBLE ANSWERS.
        ;; ANSGEN GENERATES AN ANSWER FOR EACH INTERPRETATION.
        ;; ANSUNIQUE TAKES OUT REDUNDANT ONES IN THE CASE THAT DIFFERENT INTERPRETATIONS LEAD TO THE SAME ANSWER.
        ;; ANSORDER ORDERS THE REMAINING ONES BY PLAUSIBILITY.
        (let [a (ansorder (ansunique (doall (map ansgen (semantics node)))))
              ;; IF NO ANSWER IS CLEARLY BEST, ASK THE USER FOR CLARIFICATION AND TRY AGAIN.
              a (loop-when-recur a (and (cdr a) (not (enough-better (car a) (cadr a)))) (anseliminate a) => a)]
            ;; THE ACTION INCLUDES BOTH THE THINGS TO BE DONE AND THE INSTRUCTIONS FOR PRINTING A RESPONSE.
            (dorun (map eval (action? (car a))))
            (print \.)
            (terpri)
            ;; DOBACKREF STORES AWAY DISCOURSE INFORMATION.
            (dobackref (car a))
            true)))

(defn- ambput [code]
    ;; PUTS IN THE JUNK FOR DISCOURSE IF THERE IS NO AMBIGUITY, SO THERE IS
    ;; NO NEED TO EVALUATE THE CODE A SECOND TIME WHEN GIVING THE ANSWER.
    (if *ambig* code (plnr-junkify code)))

(defn- ansbuild [rss ans plaus action rededuce]
    ;; BUILDS AN ANSWER NODE.
    ;; IF REDEDUCE IS NON-NIL, IT ADDS A REDEDUCTION OF THE ANSWER,
    ;; ADDING THE DISCOURSE JUNK TO THE ACTION.
    (build
        'ANSNODE= (gensym 'ANS)
        'PLAUSIBILITY= plaus
        'ANSRSS= rss
        'ACTION= (concat
                    (if (and *ambig* rededuce (not (cq 'DECLAR)))
                        (cons (list 'thval2 nil (list 'plnr-junkify (list 'plnrcode? (quotify rss)))) action)
                        action)
                    (and (rel? rss) (not (cq 'DECLAR)) (list (list 'putprop! (quotify (rel? rss)) (quotify 'REFER=) (quotify ans)))))))

(dynamic- *success*)

(defn- anscommand [rss]
    ;; ANSCOMMAND RESPONDS TO IMPERATIVES.
    (binding [*success* nil]
        ;; PLNR-ANDORIFY COMBINES ANDS AND ORS INTO APPROPRIATE PLANNER THANDS AND THORS.
        (let [exp (plnr-andorify rss)]
            (putprop! rss 'PLNRCODE= exp)
            (let [exp (ambput exp)
                  exp (if (= (car exp) 'thand)
                        (concat exp '((set! *success* true)))
                        (list 'thand exp '(set! *success* true)))]
                ;; IN CASE OF MULTIPLE INTERPRETATION, THE SYSTEM USES FAILURE TO WIPE OUT THE EFFECTS OF TRYING OUT ONE OF THEM.
                ;; BEFORE FAILING, IT MARKS DOWN WHETHER IT SUCCEEDED AND SAVES THE PLAN FROM BACKTRACKING.
                ;; PLNR-JUNKIFY PUTS ON THE JUNK FOR SAVING THE DISCOURSE REFERENTS, ETC.
                (thval2 nil (if *ambig* (concat exp '((thfail))) exp))
                ;; THE LAST ARGUMENT TO ANSBUILD CAUSES THE SYSTEM TO GO BACK THROUGH THE DEDUCTION
                ;; TO GET THE DATABASE STRAIGHT IF THIS ANSWER IS PICKED.  IT ALSO TAKES CARE OF THE BACKREF STUFF.
                (ansbuild rss nil
                    (if *success* (plausibility? rss) (- (plausibility? rss) 512))
                    (if *success* '((say "OK")) '((say "I CAN'T")))
                    true)))))

(defn- ansdeclare [rss]
    ;; FOR DECLARATIVES.
    (cond (or? rss)
            (oops! "I DON'T UNDERSTAND DISJUNCTIVE DECLARATIVES.")
        (and? rss)
            ;; CONJOINED DECLARATIVES ARE HANDLED BY DOING EACH ONE SEPARATELY.
            (let [ans (doall (map ansdeclare (and? rss)))]
                (ansbuild rss ans
                    (reduce + (map plausibility? ans))
                    (cons '(say "I UNDERSTAND") (doall (mapcat #(delq '(say "I UNDERSTAND") (action? %)) ans)))
                    nil))
        (not (istense (parsenode? rss) 'PRESENT))
            (oops! "I ONLY UNDERSTAND PRESENT TENSE DECLARATIVES.")
        :else (ansbuild rss (§ ANS)
                (plausibility? rss)
                ;; ANSTHM GENERATES THE APPROPRIATE ASSERTION OR THEOREM.
                (cons '(say "I UNDERSTAND") (doall (map #(list 'thadd (quotify (ansthm % (negative? rss))) nil) (relations? rss))))
                nil)))

(defn- anseliminate [anslist]
    ;; ELIMINATES ANSWERS FROM LIST BY ASKING PERSON TO CLEAR UP THE AMBIGUITIES.
    (let [amb (ambiguities? (ansrss? (car anslist)))]
        (when-not amb
            (bug! 'anseliminate "NO AMBIGUITIES LIST"))
        ;; POSSIBILITIES IS THE LIST OF POSSIBLE INTERPRETATIONS FOR A SINGLE AMBIGUITY.
        ;; WE ARE INSIDE A LOOP WHICH GOES THROUGH ALL THE DIFFERENT POSSIBLE AMBIGUITIES ON THE LIST FOR THE FIRST ANSWER ON ANSLIST.
        ;; ON EACH ANSWER WE LOOK FOR POSSIBLE INTERPRETATIONS FOR THE PARTICULAR NODE WHERE THE AMBIGUITY WAS CREATED.
        (loop [amb amb]
            (let [possibilities (list (car amb))]
                (dorun (map #(let [x (parse-assoc (caar amb) (ambiguities? (ansrss? %)))]
                                (when (and x (not (memq x possibilities)))
                                    (SETQ possibilities (cons x possibilities))))
                    (cdr anslist)))
                (cond (cdr possibilities)
                        (do (terpri)
                            (print "I'M NOT SURE WHAT YOU MEAN BY \"")
                            (dorun (map -print (let [x (caddar amb)] (from (firstword x) (wordafter x)))))
                            (print "\" IN THE PHRASE \"")
                            (dorun (map -print (let [x (parent (caddar amb))] (from (firstword x) (wordafter x)))))
                            (print "\".")
                            (terpri)
                            (print "DO YOU MEAN:")
                            (dorun (map-indexed (lambda [i poss]
                                    (terpri) (print (inc i)) (dorun (map -print (cadr poss)))) ;; THE PARAPHRASE
                                possibilities))
                            (print \?)
                            (terpri)
                            (let [n (loop-when [n (read)] (or (not (number? n)) (not (<= 1 n (count possibilities)))) => n
                                        (terpri)
                                        (print "PLEASE TYPE ONE OF THE NUMBERS")
                                        (terpri)
                                        (recur (read)))
                                  poss (nth possibilities (dec n))]
                                (mapbland (lambda [ans]
                                        (let [x (parse-assoc (caar amb) (ambiguities? (ansrss? ans)))]
                                            (when (or (not x) (= x poss)) ans)))
                                    anslist)))
                    (cdr amb) (recur (cdr amb))
                    :else (bug! 'anseliminate "NO CONFLICT"))))))

(defn- parse-assoc [oss a]
    ;; PARSE-ASSOC GOES THRU AMBIG-LIST LOOKING FOR AN INTERPRETATION WITH THE SAME PARSE NODE
    (let [ass (car (parsenode? oss))]
        (loop-when a a
            (if (= ass (car (parsenode? (caar a)))) (car a) (recur (cdr a))))))

(defn- ansgen [rss]
    ;; ANSGEN GENERATES AN ANSWER FOR A SINGLE INTERPRETATION.
    (cond
        (or (cq 'IMPER) (and (cq 'QUEST) (istense (parsenode? rss) 'FUTURE))) ;; FUTURE QUESTIONS ARE TREATED LIKE COMMANDS.
            (anscommand rss)
        (cq 'DECLAR)
            (let [x (ERRSET (ansdeclare rss))]
                (cond x x
                    ;; THIS STRANGE CONSTRUCTION ALLOWS US A SECOND CHANCE ON DECLARATIVES ABOUT THINGS WHICH CAN'T
                    ;; BE TOLD TO THE SYSTEM.  IF IT RUNS INTO ONE OF THEM, IT TRIES TO ANSWER IT AS A QUESTION.
                    (= *oops* "THAT ISN'T THE KIND OF THING I CAN BE TOLD.") (ansquest rss)
                    :else (ERR nil)))
        (cq 'QUEST) (ansquest rss)
        :else (bug! 'ansgen "WHAT KIND OF SENTENCE IS THIS?")))

(defn- ansname [phrase]
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
    (binding [*c* nil *n* (cdaar phrase) *cut* nil] ;; CDR IS TO REMOVE "SAY"
        (let [ansnode (parse2 '(NG ANSNAME) true)]                     ;; THE T SAYS NOT TO ATTACH THIS TO THE TREE
            (when' ansnode => (bug! 'ansname "FAILURE TO PARSE ANSWER NAME, BUT IF YOU ONLY EXPECT THE ANSWER TO BE AN ADJ, PROCEED THIS AND DON'T WORRY")
                (set! *ansname* (concat ansnode *ansname*))                     ;; LEAVE NODE AROUND IT ACCESSABLE PLACE
                (putprop! (car (semantics ansnode)) 'REFER= (cadr phrase))          ;; PUT THE REFERENT ON AS THE GUY GIVEN BY ANSWER
                nil))))

(defn- ansnorel [rss]
    ;; FOR QUESTIONS WITH NO RELATIVE, LIKE "DID YOU PICK UP THE BLOCK?" OR "WHY DID YOU DO THAT?"
    (let [node (parsenode? rss)
          ;; THE TYPE SHOULD BE POLAR, WHY, WHERE, WHEN, OR HOW.
          type (if (isq node 'POLAR) 'POLAR (let [x (getr node 'QADJ)] (if x (car (firstword x)) (bug! 'ansnorel "FUNNY TYPE"))))]
        (putprop! (variable? rss) 'USED true)
        ;; IN PRESENT TENSE CASES, WE DON'T LOOK FOR EVENTS.
        ;; OTHERWISE, WE LOOK FOR A SET OF APPROPRIATE EVENTS NO MATTER WHAT THE TYPE.
        (let [var (when-not (istense node 'PRESENT) (variable? rss))
              code (plnr-describe (relations? rss) var (list (variable? rss)))]
            (putprop! rss 'PLNRCODE= code)
            (if (not var)
                (let [ans (thval-mult (ambput code))]
                    (ansbuild rss ans
                        (+ (car ans) (plausibility? rss))
                        (cond (cadr ans) '((say "YES")) (istense node 'MODAL) '((say "I DON'T KNOW")) :else '((say "NO")))
                        true))
                (let-when [ans (thval-mult (plnr-findify 'ALL var (list var) (ambput code)))] ans
                    (ansbuild rss ans
                        ;; AN ANSWER IS VERY IMPLAUSIBILE IF IT MENTIONS AN EVENT THE SYSTEM CAN'T FIND.
                        (if (cadr ans) (+ (plausibility? rss) (car ans)) (- (plausibility? rss) 512))
                        (if (cadr ans)
                            (concat (and (= type 'POLAR) '((say "YES"))) (list (list 'evlis (list 'describevent (quotify (cadr ans)) (quotify type)))))
                            '((say "I CAN'T DISCUSS A NON-EXISTENT EVENT")))
                        true))))))

(defn- ansorder [l]
    ;; ORDERS A LIST BY PLAUSIBILITY HIGHEST FIRST.
    (loop-when [x l] (cdr x) => l
        (if (< (plausibility? (car x)) (plausibility? (cadr x)))
            (let [y (car x)] (RPLACA x (cadr x)) (RPLACA (cdr x) y) (ansorder l))
            (recur (cdr x)))))

(defn- ansquest [rss]
    ;; ANSQUEST ANSWERS ALL TYPES OF QUESTIONS BY SENDING THEM OUT TO ANSREL OR ANSNOREL DEPENDING ON WHETHER THERE IS A REL.
    (cond (or (or? rss) (and? rss))
        (let [ans (doall (map ansquest (or (and? rss) (or? rss))))]
            (ansbuild rss ans
                (reduce + (map plausibility? ans))
                (concat
                    (and (not (isq (parsenode? rss) 'COMPONENT)) '((say "YOU'RE TRYING TO CONFUSE ME.")))
                    (doall (mapcat (lambda [q]
                            (concat '((terpri))
                                (list (cons 'say (eliza (let [x (parsenode? (ansrss? q))] (from (firstword x) (wordafter x))))))
                                '((print \?) (terpri))
                                ;; CONJOINED QUESTIONS ARE HANDLED BY SIMPLY REPEATING EACH PART AND ANSWERING IT SEPARATELY.
                                (action? q)))
                        ans)))
                nil))
        (rel? rss)
            (ansrel rss)
        :else
            (ansnorel rss)))

(defn- ansrel [rss]
    ;; ANSREL HANDLES ALL QUESTIONS WITH A RELATIVE NG OF ANY TYPE.
    (let-when [rel (rel? rss)] rel => (bug! 'ansrel "NO REL")
        ;; THIS IS FOR THE PART OF THE GENERATOR THAT WILL SUBSITUTE "ONE" FOR NOUN NAMES.
        ;; THE LEADING NIL IS TO MAKE THIS PHRASE COMPATIBLE WITH THE "SAY" PHRASES WHICH THE OTHER PARTS GENERATE.
        ;; UNIVERSALS ARE CONVERTED TO NOT THERE EXISTS NOT.
        (let-when [phrase (cons 'nil (headpart (parsenode? rel))) type (or (qtype? rel) (quantifier? rel))] type => (bug! 'ansrel "NO TYPE")
            (when (= type 'ALL) (putprop! rss 'NEGATIVE= true))
            (let [code (plnr-findify 'ALL (variable? rel) (list (variable? rel)) (plnr-describe (cons rss (relations? rel)) (variable? rel) (list (variable? rel))))]
                (putprop! rss 'PLNRCODE= code)
                ;; CONSING THE RSS ONTO THE THINGS TO BE DESCRIBED HAS THE EFFECT OF PUTTING THE RELATION INTO THE DESCRIPTION OF THE OBJECT.
                ;; DISAMB PUTS IN THE JUNK IF THERE IS NO AMBIGUIT, AVOIDING HAVING TO GO THROUGH THE EVALUATION A SECOND TIME.
                ;; THVAL-MULT RETURNS A LIST OF A PLAUSIBILITY AND AN ANSWER.
                (let [ans (thval-mult (ambput code)) plaus (car ans) ans (cadr ans) len (count ans)]
                    (cond
                        (= type 'ALL)
                            (ansbuild rss ans
                                (+ plaus (plausibility? rss))
                                (if (nil? ans) '((say "YES")) (cons '(say "NO, NOT") (prepput rss (namelist phrase 'INDEF ans))))
                                true)
                        (= type 'HOWMANY)
                            (ansbuild rss ans
                                (+ plaus (plausibility? rss))
                                (prepput rss (namesugar len rel))
                                true)
                        (memq type '(WHICH WHAT))
                            (ansbuild rss ans
                                (+ plaus (plausibility? rss) (if ans 512 0))
                                (prepput rss (namelist phrase 'DEF ans))
                                true)
                        (= type 'INDEF)
                            (let [num (num? rel)]
                                (ansbuild rss ans
                                    (+ plaus (plausibility? rss))
                                    (cond
                                        (memq num '(NS SG-PL))
                                            (if ans (concat '((say "YES,"))
                                                        (when-not (istense (parsenode? rss) 'MODAL)
                                                            (prepput rss (concat (and (cdr ans) (concat (namesugar len rel) '((print \:)))) (namelist phrase 'INDEF ans)))))
                                                (if (istense (parsenode? rss) 'MODAL) '((say "I DON'T KNOW")) '((say "NO"))))
                                        (number? num)
                                            ;; THIS IS THE CASE WHERE WE ARE CAGEY AND AVOID ANSWERING YES OR NO.
                                            ;; THE NUMBER ISN'T REPEATED IF IT IS THE SAME AS THE NUMBER IN THE SPECIFICATION.
                                            (concat
                                                (cond (= num len) '((say "YES,")) (< num len) nil (zero? num) '((say "NO,")) :else '((say "NO, ONLY")))
                                                (cond (= num len) nil :else (prepput rss (concat (namesugar len rel) '((print \:)))))
                                                (prepput rss (namelist phrase 'INDEF ans)))
                                        (= (car num) 'EXACTLY)
                                            (cond (= num len) '((say "YES")) :else (cons '(say "NO,") (prepput rss (namesugar len rel))))
                                        (= (car num) '>)
                                            (cons
                                                (cond (> len num) '(say "YES,") (zero? len) '(say "NO,") :else '(say "NO, ONLY"))
                                                (prepput rss (namesugar len rel)))
                                        (= (car num) '<)
                                            (cons
                                                (cond (< len num) '(say "YES,") :else '(say "NO,"))
                                                (prepput rss (namesugar len rel)))
                                        :else (bug! 'ansrel "FUNNY NUMBER" num))
                                    true))
                        :else (bug! 'ansrel "FUNNY TYPE" type)))))))

(dynamic- *neg*)
(dynamic- *varlist*)
(dynamic- *body*)

(defn- ansthm [exp neg']
    ;; GENERATES A THEOREM OR ASSERTION FOR AN EXPRESSION.
    (binding [*neg* neg' *varlist* nil *body* nil]
        ;; NOTELL MARKS THAT THIS ISN'T THE KIND OF ASSERTION IT CAN HANDLE.
        ;; IT USES GLOBAL-ERR VAR AND NEG ARE SET AS FREE VARIABLES BY ANSTHMELEMENT WHICH ANALYZES EACH ELEMENT.
        ;; IF THERE ARE NO VARS, IT IS A SIMPLE ASSERTION.
        (if (or (term? exp) (not (getprop (car exp) 'TELLABLE))) (do (notell) nil)
            (let-when [exp (doall (map ansthmelement (plnr-remtime exp)))] (or *varlist* *neg*) => exp
                (plnr-thconsify *varlist* exp (if *neg* (plnr-progify nil (list *body* '(thfail))) *body*))))))

(defn- ansthmadd [oss]
    (let [_ (plnr-describe (relations? oss) (variable? oss) (list (variable? oss)))]
        (set! *varlist* (cons (variable? oss) *varlist*))
        (set! *body* (if *body* (plnr-progify nil (list *body* _)) _))
        (list 'thv (variable? oss))))

(defn- ansthmelement [x]
    (cond (not (term? x)) x
        (tss? x) (notell)
        (rss? x) (notell)
        (not (oss? x)) x
        (refer? x) (atomify (refer? x))
        (= (quantifier? x) 'ALL) (if *neg* (notell) (ansthmadd x))
        (= (quantifier? x) 'NO) (do (set! *neg* true) (ansthmadd x))
        (= (quantifier? x) 'NDET) (ansthmadd x)
        (not (= (quantifier? x) 'INDEF)) (notell)
        (isq (parsenode? x) 'ANY) (ansthmadd x)
        :else (oops! "YOU HAVE TO TELL ME WHICH")))

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
    (symbol* (cdr (name x))))

(defn- describevent [event type]
    (let [event (car event)]
        (cond (= type 'WHERE)
                (oops! "I CAN'T ANSWER \"WHERE\" QUESTIONS YET.")
            (= type 'WHY)
                (if (= (getprop event 'WHY) 'COMMAND)
                    '((say "BECAUSE YOU TOLD ME TO"))
                    (cons '(say "TO") (nameaction 'INFINITIVE (getprop event 'WHY))))
            (= type 'HOW)
                (let [ans nil]
                    (dorun (map #(and (= (getprop % 'WHY) event) (SETQ ans (cons % ans))) *eventlist*))
                    (if (nil? ans)
                        '((say "I CAN'T ANALYZE HOW I DID IT"))
                        (concat '((say "BY")) (nameaction 'ING (car ans)) (doall (mapcat #(cons '(print \;) (cons '(say "THEN") (nameaction 'ING %))) (cdr ans))))))
            (or (= type 'POLAR) (= type 'WHEN))
                (if (= (getprop event 'WHY) 'COMMAND)
                        (if (= event (toplevel (car *eventlist*)))
                            '((say "JUST NOW"))
                            (cons '(say "BEFORE") (nameaction 'PAST (toplevel (car (findb event *eventlist*))))))
                    (cons '(say "WHILE") (nameaction 'PRES-PAST (toplevel event))))
            :else (bug! 'describevent "FUNNY TYPE" type))))

(defn- disput [assertion]
    ;; PUT THE SENTENCE NUMBER ON THE ASSERTION AS A WHO PROPERTY
    (or (not discourse?) (putprop! assertion 'WHO *sentno*)))

(defn- eliza [node]
    ;; DOES THE OBVIOUS THING.
    ;; WE RETURN LIST OF THE THING REALLY WANTED, SO THE APPLY APPEND CAN GET RID OF THE EMPTY ONES.
    ;; UNFORTUNATELY, FOR "YOU" IT IS NECESSARY TO DECIDE WHETHER IT SHOULD BE REPLACED BY "I" OR "ME",
    ;; WHETHER IT WAS PARSED AS AN OBJECT OR SUBJECT.  FINDMOTHER IS USED TO FIND THE PARSE NODE.
    ;; WORDS OTHER THAN THE SPECIAL ONES GO THROUGH DIRECTLY.
    (let [num (count (wordafter node))]
        (apply concat
            (doall (map* (lambda [word]
                    ;; THIS KLUDGE STOPS IT AT THE END OF THE NODE.
                    (when (< num (count word))
                        (let [x (assq (car word) '((I YOU) (ME YOU) (AM ARE) (ARE AM)))]
                            (cond x (cdr x)
                                (= (car word) 'YOU)
                                    (let [x (findmother word node)]
                                        (cond (isq x 'SUBJ) '(I) (isq x 'OBJ) '(YOU) :else (bug! 'eliza "SUBJ OBJ")))
                                :else (list (car word))))))
                (firstword node))))))

(def- timid 200)

(defn- enough-better [ans1 ans2]
    (> (plausibility? ans1) (+ (plausibility? ans2) timid)))

(defn- findmother [word node]
    ;; FINDMOTHER TAKES A PLACE IN THE SENTENCE AND A GRAMMAR NODE (BOTH ARE ACTUALLY LISTS)
    ;; AND FINDS THE SINGLE-WORD CONSTITUTENT BEGINNING AT THAT PLACE IN THE SENTENCE.
    (if (and (= word (firstword node)) (= (cdr word) (wordafter node))) node (apply concat (doall (map* #(findmother word %) (daughters node))))))

(defn- headpart [node]
    ;; EVERYTHING UP TO THE NOUN, FOR EXAMPLE "THE RED BLOCK" IN "THE RED BLOCK WHICH ..."
    ;; NOTE THAT NODE IS ACTUALLY A LIST OF NODE (A PROPER GRAMMAR POINTER).
    (and (set! *pt* node) (move-pt 'DLC 'PV '(NOUN)) (from (firstword node) (wordafter *pt*))))

(dynamic- *phrase*)

(defn- listnames [phrase' spec names]
    ;; PHRASE IS THE INITIAL THING TO COMPARE FOR USING "ONE", SPEC IS EITHER DEF OR INDEF, AND THE NAMES ARE OF DATABASE OBJECTS.
    ;; LISTNAMES PUTS OUT AN ACTION LIST, AS WELL AS PUTTING THINGS ONTO THE BACKREF.
    ;; IT IS CALLED AFTER THE ANSWER HAS BEEN DECIDED ON.
    (binding [*phrase* phrase']
        ;; NAMEOBJ RETURNS A LIST OF THE OBJECT AND THE ...
        ;; THIS PATCH MAY WELL BE TOTAL OUT OF PHASE WITH THE BACKREF HACKER - DDM 5-12-73 INSTRUCTIONS FOR NAMING IT.
        (let-when [names (doall (map #(nameobj % spec) names))] names => '(say "NOTHING")
            (let [res (loop [res nil names names]
                    (let [[n exam names]
                            (loop [n 1 exam (car names) names (cdr names)]
                                (let-when [x (assq (car exam) names)] x => [n exam names]
                                    (recur (inc n) (list (car exam) (concat (cadr x) (cadr exam))) (delq x names))))
                          ;; WHEN THERE ARE TWO OBJECTS WITH THE SAME ENGLISH DESCRIPTIONS, A JOINT OBJECT IS PRODUCED COMBINING THE OBJECTS.
                          ;; THE COUNT IS LATER USED TO PUT IN THE APPROPRIATE NUMBER, AND THE DESCRIPTION IS CHECKED TO SEE IF "ONE" CAN BE USED.
                          ;; ADD THE ONE JUST PRODUCED TO THE RESULT LIST.  TRY ANOTHER.
                          res (cons (cons (pluralize (car exam) n) (cdr exam)) res)]
                        (recur-if names [res names] => res)))]
                ;; ANSNAME PARSES THE PHRASE AND PUTS THE ...
                ;; ANSONE SUBSTITUTES "ONE" IF POSSIBLE.
                (loop [res (doall (map #(if (propname (caadr %)) (car %) (do (ansname %) (onecheck (car %)))) res)) ans (car res) comma? false]
                    (let-when [res (cdr res)] res => ans
                        (if (cdr res)
                            (recur res (concat ans '((print \,)) (car res)) true)
                            (recur res (concat ans (when comma? '((print \,))) '((say "AND")) (car res)) comma?))))))))

(defn- pron-prt [particle ng]
    ;; THIS IS EVENTUALLY SUPPOSED TO BE THE PLACE FOR THE PRONOUN-PARTICLE-INTERACTION MAGIC
    ;; TO HAPPEN, IE. "CLEAR OFF THE BLOCK." VS. "CLEAR IT OFF.", SINCE "CLEAR OFF IT." IS
    ;; UNGRAMMATICAL AND "CLEAR THE BLOCK OFF." WOULD NOT BE APPROPRIATE IN CASES OF HEAVY-NP'S.
    ;;
    ;; AT THE MOMENT, FOR SIMPLICITY'S SAKE, I'VE IGNORED THE
    ;; PROBLEM AND THE PARTICLE IS ALWAYS PUT BEFORE THE NG.
    (cons (list 'say particle) (namelist-evaled '(nil) 'DEF ng)))

(defn- nameaction [tense event]
    ;; THIS FUNCTION SETS UP A LIST OF S-EXPRESSIONS WHICH ARE RETURNED TO DESCRIBEVENT AND WHICH
    ;; WHEN EVALUATED WILL PRINT OUT AN ENGLISH DESCRIPTION OF THE SINGLE, SIMPLE EVENT IMBEDDED
    ;; IN THE LIST "THASSERTION" WITH THE TENSE SPECIFIED.
    ;; THE THASSERTION PROPERTY IS A LIST THAT TYPICALLY LOOKS LIKE "(NIL (2 (3 1 ((!GRASP ßE2 ßB6)))))"
    (let [a (car (caddr (cadadr (getprop event 'thassertion)))) verb (cutoff (car a)) obj1 (caddr a) obj2 (cadddr a)]
        (condp = verb
            'CLEARTOP   (cons (list 'say (vbfix 'CLEAN tense false)) (pron-prt "OFF" obj1))
            'GET-RID-OF (cons (list 'say (vbfix 'GET tense true) "RID OF") (namelist-evaled '(nil) 'DEF obj1))
            'GRASP      (cons (list 'say (vbfix 'GRASP tense true)) (namelist-evaled '(nil) 'DEF obj1))
            'PICKUP     (cons (list 'say (vbfix 'PUT tense true)) (pron-prt "UP" obj1))
            'PUTON      (concat (cons (list 'say (vbfix 'PUT tense true)) (namelist-evaled '(nil) 'DEF obj1)) (pron-prt "ON" obj2))
            'PUTIN      (concat (cons (list 'say (vbfix 'PUT tense true)) (namelist-evaled '(nil) 'DEF obj1)) (pron-prt "IN" obj2))
            'STACKUP    (cons (list 'say (vbfix 'STACK tense true)) (pron-prt "UP" obj1))
            'RAISEHAND  nil
                        (bug! 'nameaction "I DON'T KNOW WHAT TO DO WITH THE VERB I GOT" verb))))

(defn- namelist [one spec listx]
    ;; GENERATES A LIST OF EXPRESSIONS TO BE EVALUATED WHICH WILL CAUSE THE APPROPRIATE NAMELIST TO BE PRINTED OUT.
    ;; THE ARGUMENTS ARE JUST THOSE TO LISTNAMES.
    (list (list 'evlis (list 'listnames (quotify one) (quotify spec) (quotify listx)))))
    ;; A TYPICAL CALL WOULD RESULT IN A VALUE OF ((EVLIS (LISTNAMES '(A RED BLOCK) 'INDEF '(ßB1 ßB7)))) WHICH WOULD BE EVALUATED LATER.
    ;; NOTE THAT LISTNAMES WILL IN TURN PRODUCE A LIST OF EXPRESSIONS TO BE EVALUATED, WHICH WILL BE CAUGHT BY THE EVLIS.  CONFUSING?

(defn- namelist-evaled [one spec listx]
    (list (eval (list 'listnames (quotify one) (quotify spec) (quotify listx)))))

(def- numbers* ["NONE" "ONE" "TWO" "THREE" "FOUR" "FIVE" "SIX" "SEVEN" "EIGHT" "NINE" "TEN"])

(defn- namenum [x]
    ;; GENERATES NUMBER NAMES.
    (cond (neg? x) (oops! "I CAN'T COUNT THAT LOW") (< x (count numbers*)) (nth numbers* x) :else (oops! "I CAN'T COUNT THAT HIGH")))

(defn- ansay [x]
    ;; GENERATES THE SYNTAX FOR ANSWER ACTIONS FROM A PHRASE.
    (list (cons 'say x)))

(dynamic- *item*)
(dynamic- *typeß*)
(dynamic- *typelist*)
(dynamic- *nameß*)
(dynamic- *colorß*)
(dynamic- *colorlist*)
(dynamic- *sizeß*)
(dynamic- *sizelist*)
(dynamic- *cube*)

(defn- nameobj [item' spec]
    ;; NAMES THE OBJECT IN ENGLISH -- GENERATES LIST OF THINGS TO BE EVALUATED.  SPEC IS EITHER 'INDEF OR 'DEF.
    (binding [*item* item' *typeß* nil *typelist* nil *nameß* nil *colorß* nil *colorlist* nil *sizeß* nil *sizelist* nil *cube* nil]
        (let-when [x (assq *item* '((ßSHRDLU I) (ßFRIEND YOU)))] (not x) => (list (ansay (cdr x)) (list *item*))
            (thval2 nil '(thgoal (!NAMEOBJ) (THUSE TC-NAMEOBJ)))
            (when-not *typelist*
                (bug! 'nameobj "OBJECT WITH NO !IS ASSERTION"))
            (disput *typeß*) ;; DISPUT CHECKS TO SEE IF DISCOURSE IS BEING KEPT, AND IF SO, PUTS THE RELEVANT SENTENCE NUMBER AS A PROPERTY ON THE ASSERTION.
            (let [type (caddar *typeß*)]
                (cond (= type '!NAME)
                        (list (ansay (list *item*)) (list *item*)) ;; A NAME IS ITS OWN NAME.
                    (memq '!PROPERTY (getprop type 'SYS))
                        (list (ansay (list (cutoff *item*))) (list *item*)) ;; CUTOFF CUTS THE # OFF OF NAMES LIKE !RED AND !POINTED WHICH ARE USED FOR PROPERTIES.
                    (not (cdr *typelist*))
                        (list (ansay (list 'THE (cutoff type))) (list *item*)) ;; THERE IS ONLY ONE OBJECT OF THIS TYPE (E.G. TABLE, BOX, HAND).
                    :else
                        ;; E.G. !BLOCK BECOMES BLOCK.  ;; E.G. THE BLOCK NAMED SUPERBLOCK.
                        (let-when [name (if *cube* '(CUBE) (list (cutoff type)))] (not *nameß*) => (list (ansay (list 'THE (car name) 'NAMED (caddar *nameß*))) (list *item*))
                            ;; IF WE HAVEN'T RETURNED YET, COLOR WILL BE NEEDED TO FULLY DESCRIBE THE OBJECT.
                            (disput *colorß*)
                            ;; THERE ARE NO OTHERS OF THE SAME COLOR.  IF THERE ARE, WE MUST USE SIZE AS WELL.
                            (let-when [name (cons (cutoff (caddar *colorß*)) name)] (cdr *colorlist*) => (list (ansay (cons 'THE name)) (list *item*))
                            (let [name (cons *sizeß* name)] (list
                                (cond
                                    (nil? (cdr *sizelist*))
                                        (ansay (cons 'THE name)) ;; THE SIZE MANAGES TO FINISH SPECIFYING IT.
                                    (= spec 'INDEF)
                                        (ansay (cons 'A name)) ;; IN THE INDEFINITE CASE WE DON'T CARE IF THIS ISN'T A FULL SPECIFICATION.
                                    :else
                                        (let [x (thval2 nil '(thfind ALL ($? x) (x (y *item*)) (thgoal (!SUPPORT ($? y) ($? x)))))]
                                            (if x
                                                (cons (concat '(say "THE") name)
                                                    (cons '(say "WHICH SUPPORTS") (listnames nil 'INDEF x))) ;; IF IT SUPPORTS ANYTHING, NAME THEM.
                                                (cons (concat '(say "THE") name)
                                                    (cons '(say "WHICH IS TO THE RIGHT OF")
                                                        (let [x (thval2 nil ;; MAKE SURE IT IS AN ITEM WITH A LOCATION.
                                                                '(thfind ALL ($? x) (x (y *item*)) (thgoal (!AT ($? x) ?)) (thgoal (!LOC !RIGHT ($? y) ($? x)) (THUSE TC-LOC))))]
                                                            (if x (listnames nil 'INDEF x) '((say "NOTHING")))))))))
                                (list *item*))))))))))

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
    '(thconse ((x *item*) type color name #_size y z)
        (!NAMEOBJ)
        (thgoal (!IS ($? x) ($? type)))
        (set! *typeß* *value*)
        (or (set! *cube* (and (= ($? type) '!BLOCK) (!eqdim ($? x))))
            true)
        (thcond
            ((thgoal (!NAME ($? x) ($? name)))
                (SETQ name *value*))
            ((thgoal (!IS ($? y) ($? type)))
                (or (not *cube*) (!eqdim ($? y)))
                (set! *typelist* (cons ($? y) *typelist*))
                (thgoal (!color ($? x) ($? color)))
                (set! *colorß* *value*)
                (thgoal (!color ($? y) ($? color)))
                (set! *colorlist* (cons ($? y) *colorlist*))
                (set! *sizeß* (namesize (size ($? x))))
                (= *sizeß* (namesize (size ($? y))))
                (set! *sizelist* (cons ($? y) *sizelist*))
                (thfail)))))

(defn- namesize [x]
    ;; ACCEPTS EITHER SINGLE NUMBER OR LIST OF DIMENSIONS.
    (let [x (if (number? x) x (reduce + x))]
        (if (<= x 382) 'SMALL 'LARGE)))

(defn- namesugar [num oss]
    ;; GENERATES PHRASES LIKE "THREE OF THEM".
    ;; VAGUE IS FOR WORDS LIKE "ANYTHING", "SOMETHING", "NOTHING" TO AVOID SAYING "OF THEM" WHEN IT ISN'T APPROPRIATE.
    (let [vague (memq '!VAGUE (markers? oss))]
        (list (if (and vague (zero? num)) (list 'say "NOTHING") (list 'say (namenum num) (if vague (if (== num 1) "THING" "THINGS") "OF THEM"))))))

(defn- notell [] (oops! "THAT ISN'T THE KIND OF THING I CAN BE TOLD."))

(defn- onecheck [item]
    ;; CHECKS TO SEE IF A SUBSTITUTE "ONE" CAN BE USED.
    ;; ITEM IS A SINGLE "SAY" PHRASE.
    ;; PHRASE IS A FREE VARIABLE IN LISTNAMES.
    (if (= *phrase* '(nil))
        (do (set! *phrase* (car item)) item)
        ;; IF THE NOUNS DON'T MATCH, JUST RETURN WHAT YOU GOT.
        ;; MATCHING INCLUDES PLURALS TO THEIR CORRESPONDING SINGULAR FORMS.
        (let-when [a (reverse *phrase*) b (reverse (car item))] (or (= (car a) (car b)) (= (car a) (getprop (car b) 'ROOT)) (= (car b) (getprop (car a) 'ROOT))) => item
            (loop [a a b b]
                (let [a (cdr a) b (cdr b)]
                    (if (or (nil? a) (nil? b) (isq b 'NUM) (isq b 'DET) (not= (car a) (car b)))
                        (cons (reverse (cons (if (isq (llast (car item)) 'NPL) 'ONES 'ONE) b)) (cdr item))
                        (recur a b)))))))

(defn- ordname [num]
    ;; NAME AN ORDINAL.
    (cond (== num 1) 'ONCE (== num 2) 'TWICE :else (symbol (str (namenum num) \space 'TIMES))))

(defn- plnr-andorify [rss]
    ;; TURNS AN RSS INTO A COLLECTION OF PLANNER CODE FOR A COMMAND.
    (cond
        (and? rss) (plnr-progify nil (doall (map plnr-andorify (and? rss))))
        (or? rss) #_(PLNR-ORIFY nil (doall (map plnr-andorify (or? rss)))) (bug! 'plnr-orify "NOT WRITTEN YET")
        :else (plnr-progify nil (doall (map plnr-goalify (relations? rss))))))

(defn- prepput [rss x]
    (if (and (rel? rss) (set! *pt* (parsenode? (rel? rss))) (isq (move-pt 'U) 'PREPG))
        (cons (cons 'say (from (firstword *pt*) (firstword (move-pt 'DLC)))) x)
        x))

(defn- pluralize [item num]
    ;; CONVERTS A SINGULAR NOUNPHRASE OR "ONCE" STATEMENT INTO PLURAL.
    (cond (< num 2) item
        (memq 'A (car item)) (cons (pluralmake (subst (namenum num) 'A (car item))) (cdr item))
        (memq 'ONCE (car item)) (cons (subst (ordname num) 'ONCE (car item)) (cdr item))
        :else (bug! 'pluralize "FUNNY ITEM" item)))

(defn- pluralmake [phrase]
    ;; CONVERTS SINGULAR PHRASE TO PLURAL.
    (let-when [sing (llast phrase)] (isq sing 'NOUN) => (bug! 'pluralmake "NO NOUN")
        (let [plural (symbol (str (car sing) 'S))]
            (when-not (getprop plural 'FEATURES)
                (buildword plural ['NOUN 'NPL] (semantics sing) (car sing)))
            (subst plural (car sing) phrase))))

(defn- thval-mult [code]
    ;; DOES A THVAL WITH DIFFERENT VALUES OF WHO (I.E. NIL (EVERYTHING I KNOW),
    ;; 'HE (EVERYTHING HE KNOWS), AND THE PREVIOUS SENTENCE) USED TO TELL IF AN
    ;; ANSWER COULD HAVE BEEN GENERATED WITH HIS KNOWLEDGE TO SEE WHETHER HE REALLY
    ;; MEANT THIS INTERPRETATION.  RETURNS A LIST OF A PLAUSIBILITY AND THE RESULT
    ;; OF THE THVAL USING ALL THE KNOWLEDGE IN THE DATABASE.
    (let [ans (thval2 nil code)] (cond
        ;; THIS FEATURE IS ONLY RELEVANT IN DISCOURSE AND WHEN THERE ARE AMBIGUITIES.
        (not (and *ambig* discourse?)) (list 0 ans)
        ;; GIVE A VALUE OF 256 IF HE COULDN'T HAVE ANSWERED IT AT ALL.
        (not= ans (thval2 'HE code)) (list 256 ans)
        ;; PLAUSIBILITY IS 0 IF HE COULD HAVE ANSWERED IT WITH RECENTLY MENTIONED INFORMATION.
        ;; 128 IF HE COULD ANSWER IT BUT NOT WITH RECENT INFO.
        (= ans (thval2 (list (- *sentno* 2) (inc *sentno*)) code)) (list 0 ans)
        :else (list 128 ans))))

(defn- toplevel [event]
    ;; FINDS THE TOP LEVEL EVENT GOING ON AT THE TIME
    (let [why (getprop event 'WHY)] (if (= why 'COMMAND) event (recur why))))

(defn- findreduce [x y]
    (let [x (cdr x) y (dec y)] (if (zero? y) x (recur x y))))

(dynamic- *ans2*)

(defn- findchoose [oss x ans2']
    (binding [*ans2* ans2']
        (cond
            (refer? oss) (atomify (refer? oss))
            (and? oss) (mapbland #(let [_ (findchoose % x *ans2*)] (set! *ans2* (concat _ *ans2*)) _) (and? oss))
            (or? oss) (loop-when [a (or? oss)] a (or (findchoose (car a) x *ans2*) (recur (cdr a))))
            :else
                (let [bind- #(let [_ (reverse %)] (when (term? (variable? oss)) (putprop! (variable? oss) 'BIND _)) (atomify _))
                      plnrcode (plnr-describe (relations? oss) (variable? oss) (list (variable? oss)))]
                    (putprop! oss 'PLNRCODE= plnrcode)
                    (cond
                        (= (quantifier? oss) 'ALL)
                            (atomify (thval (plnr-findify 'ALL (variable? oss) (list (variable? oss)) plnrcode) nil))
                        (or (and? oss) (or? oss))
                            (let [ans (loop-when [ans nil a (or (and? (§ RSS)) (or? (§ RSS)))] a => ans
                                        (let [_ (getprop (car a) 'REFER)
                                              ans (if _ (concat _ ans) (let [_ (findchoose (car a) x (concat *ans2* ans))] (if _ (concat _ ans) ans)))]
                                            (if (and ans (or? oss)) ans (recur ans (cdr a)))))]
                                (when ans (bind- ans)))
                        :else
                            (let [need (num? oss) need (if (term? need) need (cadr need)) need (if (= need 'NS) 1 need)]
                                (loop [ans nil have 0 x x]
                                    (let [[? ans] (if (= have need) [true ans] (if (< have need) [false ans] (let [ans (findreduce ans (- have need))] [ans ans])))]
                                        (cond ? (bind- ans)
                                            (not= x 'NOMORE)
                                                (let [? (plnr-findify (list 1 (- need have) true) (variable? oss) (list (variable? oss))
                                                            (plnr-progify nil
                                                                (concat (list plnrcode)
                                                                    (subst (variable? oss) '*** '((not (or (memq (thv ***) ans) (memq (thv ***) *ans2*)))))
                                                                    (and x (subst (variable? oss) '* (car x))))))
                                                    ans (concat (thval ? *thalist*) ans)]
                                                    (recur ans (count ans) (if x (cdr x) 'NOMORE))))))))))))

(defn- vbfix [x tense pp]
    (let [fix- #(when (and pp (memq (car %) CONSO) (memq (cadr %) VOWEL)) (list (car %)))]
        (condp = tense
            'ING (let [x (reverse (name x))] (symbol* (reverse (concat '(G N I) (fix- x) x))))
            'PAST (or (getprop x 'PAST) (let [x (reverse (name x))] (symbol* (reverse (concat '(D E) (fix- x) x)))))
            'INFINITIVE x
            (bug! 'vbfix "WHAT SHOULD I DO WITH THIS TENSE?" tense))))

#_(ns shrdlu.cgram)

;; #################################################################
;;
;;  CGRAM > THE REGULAR GRAMMAR AFTER GOING THROUGH THE PRECOMPILER
;;
;; #################################################################

(§ defn- CLAUSE []
    (binding [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil *position-of-prt* nil *mvb* nil *locationmarker* nil *subj-vb-backup-type1* nil *position-of-ptw* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-CLAUSE
        (when *labeltrace* (passing 'ENTERING-CLAUSE))
        (setr *c* 'TIME (build 'TSSNODE= (gensym 'TSS)))
        (when (cq 'SIMP) (GO SUBJ))
        (if (cq 'MAJOR) (GO INIT) (GO SEC))
INIT
        (when *labeltrace* (passing 'INIT))
        (set! *locationmarker* *n*)
        (when-not (and (nq 'BINDER) (parse 'CLAUSE 'BOUND 'INIT)) (if *nn* (GO MAJOR) (GO FIXIT)))
        (fq! 'BIND)
        (when (smbind) (GO INIT))
FIXIT
        (when *labeltrace* (passing 'FIXIT))
        (set! *ptw* *cut*)
        (if (cut (move-ptw)) (GO INIT) (GO MAJOR))
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
        (and (nq 'PREP) (parse 'PREPG 'INIT) (or (smrelate *h*) (pop*)))
        (and (nq 'ADV) (parse 'ADV 'TIMW) (or (smadverb) (pop*)))
        (and (nq 'ADV) (parse 'ADJG 'ADV 'VBAD) (or (smrelate *h*) (pop*)))
        (parse 'NG 'TIME)
        (if (= *locationmarker* *n*) (if *nn* (GO CLAUSETYPE) (GO INPOP)) (GO INIT))
INPOP
        (when *labeltrace* (passing 'INPOP))
        (when-not (move-pt 'C 'DLC) (GO FAIL (m! 'INPOP)))
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
            (move-pt 'C 'U '(REL-NOT-FOUND)) (do (setr *c* 'SUBJECT (getr *pt* 'RELHEAD)) (setr *c* 'RELHEAD (getr *pt* 'RELHEAD)) (remove-f 'REL-NOT-FOUND *pt*) (GO VB))
            (and (cq 'COMPONENT) *nn*) (do (fq! 'SUBJFORK) (GO VB))
            *h* (do (pop*) (GO SUBJ))
            :else (GO FAIL))
HEAD
        (when *labeltrace* (passing 'HEAD))
        (when-not (or (move-ptw 'N 'PW '(NOUN)) (move-ptw 'N 'PW '(PRON))) (GO FAIL (m! 'HEAD)))
SUB2
        (when *labeltrace* (passing 'SUB2))
        (when-not (pop*) (GO FAIL))
        (if (cut *ptw*) (GO INIT) (GO SUB2))
SUBJ1
        (when *labeltrace* (passing 'SUBJ1))
        (when (isq *h* 'QUOTED) (when (isq *h* 'LIST) (fq! 'LIST)) (fq! 'QUOTED) (set! *h* (daughters *h*)) (GO RETSM))
        (and (cq 'REL-NOT-FOUND) (move-pt 'H 'PV '(QAUX))
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
            (isq *h* 'CLAUSE) (do (set! *subj-vb-backup-type1* true) (pop*) (GO SUBJ4))
            (isq *h* 'SUBJ) (do (pop*) (fq! 'SUBJFORK) (GO VBL)))
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
        (when-not (set! *position-of-prt* (move-ptw 'N 'NW '(PRT))) (GO FINDOBJ1))
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
        (popto 'VG)
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
        (when (or (canparse 1 '(ADJG COMP) 'INT) (canparse 1 '(NG COMP) 'INT)) (if *nn* (GO CHECKIT) (GO ONT)))
        (when (or (canparse 1 '(PREPG COMP) 'INT) (canparse 1 '(CLAUSE RSNG ING) 'TRANS) (canparse 1 '(CLAUSE RSNG REPORT) 'TRANS) (canparse 1 '(CLAUSE RSNG TO) 'TRANS) (canparse 1 '(PREPG LOC) 'ITRNSL) (canparse 1 '(ADV PLACE) 'ITRNSL)) (GO ONT))
        (when (canparse 1 '(NG) 'TRANS) (if *nn* (GO FINDOBJ2) (GO FINDFAKE2)))
FINDFAKE1
        (when *labeltrace* (passing 'FINDFAKE1))
        (when (move-pt 'C 'U '(REL-NOT-FOUND)) (GO OBJ1REL))
        (when (and (cantake 1 '(PREPG LOC) 'ITRNSL) (move-pt 'C 'U '(QADJ)) (isq (getr *pt* 'QADJ) 'PLACE) (fq! 'ITRANSL)) (GO PUTLOBJ))
        (when (canparse 1 nil 'ITRNS) (GO ONT))
GOOF1
        (when *labeltrace* (passing 'GOOF1))
        (or *oops* (bug! 'clause "NEW TRANSITIVITY - FIRST OBJ"))
        (GO FAIL)
OBJ1REL
        (when *labeltrace* (passing 'OBJ1REL))
        (setr *c* 'OBJ1 (getr *pt* 'RELHEAD))
        (remove-f 'REL-NOT-FOUND *pt*)
        (fq! 'OBJ1REL)
FINDOBJ2
        (when *labeltrace* (passing 'FINDOBJ2))
        (when (canparse 2 '(CLAUSE RSNG TO) 'TRANS2) (GO FIXSUBJECT))
        (when (or (canparse 2 '(ADV PLACE) 'TRANSL) (canparse 2 '(PREPG LOC) 'TRANSL)) (GO ONT))
        (when (or (canparse 2 '(ADJG COMP) 'TRANSINT) (canparse 2 '(NG COMP) 'TRANSINT)) (GO ONT))
        (when (canparse 2 '(NG) 'TRANS2) (GO ONT))
FINDFAKE2
        (when *labeltrace* (passing 'FINDFAKE2))
        (when (and (isq *mvb* 'TRANS2) (move-pt 'C 'U '(REL-NOT-FOUND))) (GO OBJ2REL))
        (when (and (cantake 2 '(PREPG LOC) 'TRANSL) (move-pt 'C 'U '(QADJ)) (isq (getr *pt* 'QADJ) 'PLACE) (fq! 'TRANSL)) (GO PUTLOBJ))
OBJ2TO
        (when *labeltrace* (passing 'OBJ2TO))
        (parse 'ADV 'VBAD)
        (when (if (and (nextword? *n* 'TO) (isq *mvb* 'TO2) (parse 'PREPG 'TO)) (do (setr *c* 'OBJ2 (getr *h* 'OBJ1)) (fq! 'TRANS2TO 'TRANS2)) (and (cq 'PREPQ) (move-pt 'H 'PV '(QUEST)) (= (word (move-ptw 'FW)) 'TO) (rq 'PREPQ) (fq! 'TRANS2TOQ 'TRANS2) (setr *c* 'OBJ2 (getr *pt* 'OBJ1)))) (GO ONT))
        (if (canparse 2 nil 'TRANS) (GO ONT) (GO FAIL))
PUTLOBJ
        (when *labeltrace* (passing 'PUTLOBJ))
        (setr *c* 'LOBJ *pt*)
        (setr *pt* 'RELHEAD (getr *pt* 'QADJ))
        (setr *pt* 'QADJ nil)
        (remove-f 'QADJ *pt*)
        (GO ONT)
OBJ2REL
        (when *labeltrace* (passing 'OBJ2REL))
        (setr *c* 'OBJ2 (getr *pt* 'RELHEAD))
        (remove-f 'REL-NOT-FOUND *pt*)
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
        (or *oops* (bug! 'clause "NEW TRANSITIVITY - SECOND OBJECT"))
        (GO FAIL)
ONT
        (when *labeltrace* (passing 'ONT))
        (when (cq 'PASV) (GO PONT))
ONT1
        (when *labeltrace* (passing 'ONT1))
        (when-not (smcl1) (GO FAIL (m! 'SMCL1)))
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
        (when-not (smrelate *h*) (GO FAIL (m! 'ONTß)))
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
        (when (and (nq 'PREP) (parse 'PREPG) (smrelate *h*) (not *nn*)) (GO RETSM))
        (when (and (nq 'TIMW) (parse 'ADV 'TIMW) (or (smtime) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (not (cq 'BE)) (parse 'ADJG 'ADV) (or (smrelate *h*) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (parse 'NG 'TIME) (or (smtime) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (nq 'PLACE) (parse 'ADV 'PLACE) (or (smplace) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (nq 'BINDER) (parse 'CLAUSE 'BOUND) (or (smbind) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when (and (nextword? *n* 'TO) (parse 'CLAUSE 'TO 'ADJUNCT) (or (smtoadj) (GO FAIL)) (not *nn*)) (GO RETSM))
        (when-not (= *n* *position-of-ptw*) (if *nn* (GO TONT) (GO RETSM)))
        (when (or (not (cq 'TOPLEVEL)) (nq 'SPECIAL)) (GO RETSM))
        (bug! 'clause "SOMETHING LEFT OVER AT TOP LEVEL")
THERE
        (when *labeltrace* (passing 'THERE))
        (fq! 'THERE)
        (cut *end*)
        (when (and (parse 'ADV 'TIMW) (not *nn*)) (GO FAIL (m! 'THERE)))
        (if (and (parse 'VG) (isq *mvb* 'BE)) (if *nn* (GO THEF) (GO FAIL (m! 'THERE))) (GO NOTHE))
THERQ
        (when *labeltrace* (passing 'THERQ))
        (when (isq (move-pt 'H 'PV '(QAUX)) 'BE) (GO THERQ2))
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
        (when-not (move-pt 'C 'U '(REL-NOT-FOUND)) (GO NOTHE))
        (setr *c* 'SUBJECT (getr *pt* 'RELHEAD))
        (remove-f 'REL-NOT-FOUND *pt*)
        (GO ONT)
NOTHE
        (when *labeltrace* (passing 'NOTHE))
        (rq 'THERE)
        (pop* 'THERE)
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
        (if (pop* nil) (GO IMPE) (GO FAIL (m! 'IMPOP)))
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
        (smadjqshort)
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
        (cond (parse 'VG 'NAUX) (do (fq! 'SUBJQ) (GO VG1)) (nq 'VB) (do (fq! 'REL-NOT-FOUND) (GO POLAR)) :else (do (move-ptw 'N 'PW) (pop* 'NG 'QUEST) (cut *ptw*) (GO NGQUES)))
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
        (when-not (and (nq 'VB) (parse 'VB 'AUX '(QAUX)) (setr *c* 'QAUX *h*) (smvaux) (setmvb *h*)) (GO QCHOP))
        (or (cq 'QADJ) (getr *c* 'RELHEAD) (fq! 'POLAR))
        (fq! 'POLR2)
        (GO QUEST2)
QCHOP
        (when *labeltrace* (passing 'QCHOP))
        (if (popto 'CLAUSE 'BOUND) (GO BICUT) (GO FAIL (m! 'QCHOP)))
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
        (setr *c* 'RELHEAD (move-pt 'C 'U '(NG)))
        (when-not (cq 'PREPREL) (GO RSQ2))
        (parse 'PREPG 'PRONREL)
        (setr *c* 'QADJ *h*)
        (GO REPORT)
RSQ2
        (when *labeltrace* (passing 'RSQ2))
        (cond (parse 'VG 'EN 'PASV) (if (isq *mvb* 'TRANS) (do (setr *c* 'SUBJECT (getr *c* 'RELHEAD)) (GO VG1)) (GO FAIL))
            (parse 'VG 'ING) (do (setr *c* 'SUBJECT (getr *c* 'RELHEAD)) (GO VG1))
            (nq 'PRONREL) (do (parse 'NG 'RELWD) (GO REL))
            (cq 'COMPONENT) (do (setr *c* 'RELHEAD (getr (move-pt 'C 'PC) 'RELHEAD)) (GO REL))
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
        (when-not (move-ptw 'N 'NW '(ING)) (GO FAIL))
        (when (and (or (nq 'ING) (cq 'OBJ2) (and (parse 'NG 'SUBJ 'INGSUBJ) (setr *c* 'SUBJECT *h*) (fq! 'SUBING) (rq 'ING))) (not *nn*)) (GO FAIL (m! 'ING)))
        (if (parse 'VG 'ING) (GO VG1) (GO FAIL (m! 'ING)))
REPORT
        (when *labeltrace* (passing 'REPORT))
        (and (nextword? *n* 'THAT) (parse nil 'THAT) (fq! 'THAT))
        (set! *locationmarker* *n*)
        (GO FDEC)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (or (smcl2 *h*) (GO FAIL))
        (GO RETURN)
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- NG []
    (binding [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil]
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
            (and (cq 'COMPONENT) (isq (move-pt 'PC) 'QUEST)) (GO QUEST)
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
        (or (smprop) (GO FAIL))
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
        (when-not (smpron *h*) (GO FAIL))
PRAG
        (when *labeltrace* (passing 'PRAG))
        (setr *c* 'HEAD *h*)
        (move-pt 'H)
        (trnsf 'NS 'NPL 'NFS 'NEG)
        (GO RETURN)
TPRON
        (when *labeltrace* (passing 'TPRON))
        (parse 'TPRON)
        (fq! 'TPRON)
        (move-pt 'H)
        (trnsf 'NS 'NPL 'ANY 'NEG)
        (setr *h* 'HEAD *c*)
        (and *nn* (nq 'ADJ) (parse 'ADJ))
        (GO SMNG)
EVERPRON
        (when *labeltrace* (passing 'EVERPRON))
        (when-not (and (parse 'PRON 'EVERPRON) (smpron *h*)) (GO FAIL))
        (if (and (parse 'CLAUSE 'RSQ 'NOREL) (smrelate *h*)) (GO RETSM) (GO FAIL))
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
        (if (move-ptw 'N 'NW '(TIM1)) (GO LOOK) (GO FAIL (m! 'TIME)))
TIMORD
        (when *labeltrace* (passing 'TIMORD))
        (when-not (parse 'ORD 'TIMORD) (GO FAIL))
        (if (and (parse 'NOUN 'TIM1) (fq! 'DET 'DEF) (SMNGTIME)) (GO RETURN) (GO FAIL))
DET
        (when *labeltrace* (passing 'DET))
        (parse 'DET)
        (fq! 'DET)
        (move-pt 'H)
        (if (trnsf 'NPL 'NS 'PART 'DEF 'INDEF 'ANY 'NEG 'QNTFR) (if *nn* (GO IND) (GO INCOM)) (GO FAIL (m! 'BUG)))
IND
        (when *labeltrace* (passing 'IND))
        (when (and (= (word (firstword *h*)) 'ALL) (= (word *n*) 'THE) (parse 'DET) (fq! 'DEF)) (if *nn* (GO NUM) (GO FAIL (m! 'THE))))
        (when (and (isq *h* 'QNTFR) (fq! 'QNTFR)) (GO QNUM))
ORD
        (when *labeltrace* (passing 'ORD))
        (when-not (and (parse 'ORD) (fq! 'ORD)) (if *nn* (GO NUM) (GO INCOM)))
        (when (and (nextword? *n* 'OF) (isq (move-ptw 'N 'NW) 'MONTH) (parse nil 'OF) (parse 'NOUN 'MONTH) (fq! 'DATE)) (GO RETSM))
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
        (if (or (smngof) (not (pop*))) (GO RETSM) (GO INCOM))
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
            (or (smngof) (GO FAIL))
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
        (when (and (not (cq 'DET)) (not (cq 'NUMD))) (move-pt 'H) (trnsf 'NPL 'MASS))
        (when-not (meet *fe* '(NS NPL PART MASS)) (GO RED0))
        (when-not (nextword? *n* 'THAN) (GO SMNG))
        (fq! 'THAN)
SMNG
        (when *labeltrace* (passing 'SMNG))
        (setr *c* 'HEAD *h*)
        (when (and (cq 'OBOFJ) (not (cq 'DEF))) (GO FAIL))
        (or (smng1) (GO FAIL))
        (when (isq *h* 'POSS) (if *nn* (GO POSS) (GO RETSM)))
        (when-not (and (cq 'THAN) (parse 'ADJG)) (GO RSQ-TO))
        (if (smrelate *h*) (GO RETSM) (GO FAIL))
RSQ-TO
        (when *labeltrace* (passing 'RSQ-TO))
        (when (and (nextword? *n* 'TO) (meet *fe* '(COMP SUBJ)) (parse 'CLAUSE 'RSQ 'TO) (or (smrelate *h*) (GO POPRET))) (GO RETSM))
        (when-not (and (or (nextword? *n* 'AS) (nq 'COMPAR)) (parse 'ADJG 'THANNEED)) (GO PREPNG))
        (and (nil? *n*) (cq 'SUBJ) (isq (move-pt 'C 'PV) 'AUX) (isq *pt* 'BE) (GO POPRET))
        (if (smrelate *h*) (if *nn* (GO RSQ-TO) (GO RETSM)) (GO POPRET))
PREPNG
        (when *labeltrace* (passing 'PREPNG))
        (when-not (and (nq 'PREP) (not (or (and (nq 'PLACE) (cq 'NOLOC)) (and (cq 'OBJ1) (isq *mvb* 'TRANSL) (not (isq (move-pt 'C 'U) 'QUEST))))) (parse 'PREPG 'Q)) (GO DISGRSQ))
        (and (nil? *n*)
            (cq 'SUBJ)
            (isq (move-pt 'C 'PV) 'AUX)
            (isq *pt* 'BE)
            (not (isq (move-pt 'U) 'NGQ))
            (GO POPRET))
        (if (smrelate *h*) (if *nn* (GO RSQ-TO) (GO RETSM)) (GO POPRET))
DISGRSQ
        (when *labeltrace* (passing 'DISGRSQ))
        (when-not (= (car *mes*) 'PREP-WHICH) (GO RSQ))
        (set! *mes* (cdr *mes*))
        (if (parse 'CLAUSE 'RSQ 'PREPREL) (if *nn* (GO PREPNG) (GO RETSM)) (GO FAIL (m! 'RSQ-PREPREL)))
RSQ
        (when *labeltrace* (passing 'RSQ))
        (when (and (isq (move-pt 'C 'U) 'POLR2) (cq 'SUBJ) (nq 'VB) (not (cq 'SUBJT)) (not (isq *pt* 'QADJ))) (GO RETSM))
        (when-not (parse 'CLAUSE 'RSQ) (GO RETSM))
        (if (smrelate *h*) (GO RETSM) (GO POPRET))
RED0
        (when *labeltrace* (passing 'RED0))
        (set! *fe* *tmp*)
RED1
        (when *labeltrace* (passing 'RED1))
        (pop*)
RED2
        (when *labeltrace* (passing 'RED2))
        (cond (nil? *h*) (GO FAIL (m! 'NO))
            (isq *h* 'NUMBER) (GO INCOM)
            (and (isq *h* 'POSS) (or (isq *h* 'PRON) (and (move-pt 'H 'DLC) (isq *pt* 'PRON)))) (do (pop*) (GO PRON2))
            (and (nil? (cdr *h*)) (cq 'DEFPOSS)) (GO POSSDEF)
            (and (cq 'QUEST) (nil? (cdr *h*))) (GO QDETCHECK)
            (isq *h* 'ADJ) (GO EPR)
            (not (isq *h* 'CLASF)) (GO INCOM))
REDUC
        (when *labeltrace* (passing 'REDUC))
        (pop*)
        (if (and (nil? *h*) (nq 'PROPN)) (GO PROPN) (GO NOUN))
POPCOM
        (when *labeltrace* (passing 'POPCOM))
        (pop*)
INCOM
        (when *labeltrace* (passing 'INCOM))
        (fq! 'INCOM)
        (when (and (isq *h* 'DET) (isq *h* 'INCOM) (SMINCOM)) (GO RETURN))
        (when (and (nil? *cut*) (cq 'NUM)) (GO SMNG))
QDETCHECK
        (when *labeltrace* (passing 'QDETCHECK))
        (cond (and (isq *h* 'QDET) (isq (firstword *h*) 'QPRON)) (do (pop*) (GO QPRON)) (and (isq *h* 'QDET) (isq (firstword *h*) 'EVERPRON)) (do (pop*) (GO EVERPRON)))
        (GO FAIL)
POSS
        (when *labeltrace* (passing 'POSS))
        (or (smng2) (GO FAIL))
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
        (when (and (parse 'PRONREL) (smset (semantics (move-pt 'C 'U 'U '(NG))))) (GO RETURN))
POPRET
        (when *labeltrace* (passing 'POPRET))
        (pop*)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (or (smng2) (GO TRYA))
        (GO RETURN)
TRYA
        (when *labeltrace* (passing 'TRYA))
        (when-not (isq *h* 'NOUN) (GO FAIL (m! 'TRYA)))
        (pop*)
        (cut *n*)
UP
        (when *labeltrace* (passing 'UP))
        (when (pop*) (GO UP))
        (set! *fe* (reverse *rest*))
        (smset nil)
        (GO NGSTART)
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- VG []
    (binding [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil *tense* nil]
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
            (isq (move-pt 'C 'U) 'POLR2) (GO POLR2))
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
        (move-pt 'C 'DLC)
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
        (if (and (parse 'VB '(MVB) 'INF) (setmvb *h*) (smvg)) (GO RETURN) (GO FAIL (m! 'IMPER)))
POLR2
        (when *labeltrace* (passing 'POLR2))
        (or (set! *pt* (getr (move-pt 'C 'U) 'QAUX)) (bug! 'vgßpolr2 nil))
        (set! *h* (list (car *pt*)))
        (trnsf 'NEG)
        (cond (isq *h* 'DO) (GO DO)
            (isq *h* 'MODAL) (GO MODAL)
            (isq *h* 'WILL) (GO WILL)
            (isq *h* 'BE) (GO BE)
            (isq *h* 'HAVE) (GO HAVE))
        (bug! 'vgßpolr2vb nil)
DO
        (when *labeltrace* (passing 'DO))
        (fq! 'DO)
        (move-pt 'C 'DLC)
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
        (move-pt 'C 'DLC)
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
        (pop*)
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
        (when-not (isq (move-pt 'H 'PV '(VB)) 'AUX) (GO MVB))
        (when-not (isq *pt* 'BE) (GO FAIL (m! 'MVBE)))
        (setmvb *pt*)
        (GO REV)
HAVE
        (when *labeltrace* (passing 'HAVE))
        (move-pt 'C 'DLC)
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
        (pop* 'VB)
        (when-not (parse 'VB '(MVB)) (GO FAIL (m! 'MVB)))
MVB2
        (when *labeltrace* (passing 'MVB2))
        (GO REV)
REV
        (when *labeltrace* (passing 'REV))
        (setr *c* 'TENSE *tense*)
        (and *nn* (parse nil 'NOT) (fq! 'NEG))
        (cond (or (= *tense* '(PAST)) (cq 'NAGR) (isq (move-pt 'C 'U) 'IMPER) (isq *pt* 'THERE) (isq *pt* 'RSNG)) (GO NAUX)
            (set! *pt* (getr (move-pt 'C 'U) 'SUBJECT)) *pt*
            :else (bug! 'vg "NO SUBJECT TO CHECK FOR AGREEMENT"))
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
        (setmvb (or (move-pt 'H 'PV '(MVB)) *mvb*))
        (if (and (cq 'NAUX) (isq (move-pt 'H 'PV '(VB)) 'AUX) (not (move-pt 'PV 'PV '(VB)))) (GO FAIL (m! 'NAUX)) (GO RETSM))
POPV
        (when *labeltrace* (passing 'POPV))
        (bug! 'vg "POPV")
RETSM
        (when *labeltrace* (passing 'RETSM))
        (if (smvg) (GO RETURN) (GO FAIL))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- PREPG []
    (binding [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil]
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
        (move-pt 'H)
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
        (when-not (isq (move-pt 'U) 'CLAUSE) (GO FAIL (m! 'PREP-WHICH)))
        (when-not (isq *pt* 'PRONREL) (GO PRONREL))
        (set! *mes* (cdr *mes*))
        (GO P-RELWRD)
PRONREL
        (when *labeltrace* (passing 'PRONREL))
        (remove-f 'REL-NOT-FOUND *pt*)
        (add-f 'PRONREL *pt*)
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
        (or (isq (move-pt 'C 'U) 'REL-NOT-FOUND) (isq (getr *pt* 'QUESTION-ELEMENT) 'QADJ) (GO FAIL))
        (remove-f 'REL-NOT-FOUND *pt*)
        (add-f 'PREPREL *pt*)
        (setr *c* 'OBJ1 (getr (move-pt 'C 'U) 'RELHEAD))
RETT
        (when *labeltrace* (passing 'RETT))
        (and (or (isq *h* 'QUEST) (and (isq *h* 'COMPOUND) (move-pt 'H 'H 'PV '(QUEST)))) (fq! 'QUEST))
        (if (smadjg-prepg) (GO RETURN) (GO FAIL))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- ADJG []
    (binding [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil]
        (set! *nn* true)
        (set! *cut* *end*)
        (set! *c* (buildnode (set! *fe* (reverse *rest*)) (set! *nb* (or (firstword *re*) *n*)) *n* (set! *h* *re*) nil))
        (setr *c* 'PARENT *parent*)
ENTERING-ADJG
        (when *labeltrace* (passing 'ENTERING-ADJG))
COMPCHECK
        (when *labeltrace* (passing 'COMPCHECK))
        (when (and (move-pt 'C 'U '(BE)) (not (cq 'COMP))) (GO FAIL))
        (when-not (isq (move-pt 'C 'U) 'THAN) (GO DISP))
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
        (pop*)
        (GO RETSM)
POPAD
        (when *labeltrace* (passing 'POPAD))
        (pop*)
        (GO ADJ)
RETSM
        (when *labeltrace* (passing 'RETSM))
        (when (cq 'THANNEED) (GO FAIL (m! 'THANNEED)))
        (if (smadjg-prepg) (GO RETURN) (GO FAIL (m! 'SMADJ)))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

(§ defn- CONJOIN []
    (binding [*fe* nil *h* nil *me* nil *nb* nil *c* nil *sm* nil *cut* nil *nn* nil *tmp* nil *prev* nil]
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
        (and (term? *prev*) (move-ptw 'N 'NW '(= (word *ptw*) *prev*)) (cut *ptw*))
        (and (or (= *prev* 'BUT) (= (cadr *prev*) 'BUT)) (nextword? *n* 'NOT) (or (flushme) (GO LOSE2)) (fq! 'NEGBUT))
        (when-not (cond
            (memq (car *rest*) '(ADJ NUM NOUN PREP VB ADV)) (parse3 (concat *rest* '(COMPONENT)) nil)
            (memq (car *rest*) '(NG PREPG ADJG)) (and (not (cq 'OFOBJ)) (parse2 (concat *rest* '(COMPONENT)) nil))
            (= (car *rest*) 'CLAUSE)
                (let [*lastsent* (if (isq *h* 'MAJOR) *h* *lastsent*) auxfe (meet (features *h*) '(DECLAR IMPER))]
                    (and (parse2 (concat *rest* auxfe '(COMPONENT)) nil) (or (not auxfe) (f! (car auxfe))) (setr *c* 'TIME (getr *h* 'TIME)))))
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
        (cond (or (cq 'NG) (cq 'NOUN)) (if (cq 'AND) (fq! 'NPL) (do (move-pt 'H) (trnsf 'NPL 'NS 'MASS 'NFS)))
            (cq 'VB) (let [common (getprop 'VB 'ELIM)] (dorun (map* #(SETQ common (meet common (features %))) *h*)) (feset *c* (union common (features *c*)))))
        (if (smconj *h*) (GO RETURN) (GO FAIL (m! 'CONJOINß)))
FAIL
        (set! *mes* *me*)
        (set! *n* (or (wordafter *re*) *nb*))
        (RETURN nil)
RETURN
        (set! *mes* *me*)
        (rebuild *c* (reverse *fe*) *nb* *n* *h* *sm*)))

#_(ns shrdlu.init)

(defn -main [& args]
    ;; LOAD '(PLNR SYSCOM MORPHO PROGMR GRAMAR DICTIO SMSPEC SMASS SMUTIL NEWANS BLOCKS DATA)
    (binding [*grasplist* nil *eventlist* nil
              *thtime* 0 *tree* nil *thxx* nil *thalist* '((nil nil)) *tholist* '((nil nil))
              *oops* nil
              *lastsentno* 0 *lastsent* nil *sentno* 1
              *sent* nil *punct* nil
              *end* nil *both* nil *backref* nil *backref2* nil *ansname* nil *lastrel* nil *who* nil *pt* nil *ptw* nil *h* nil *n* nil *nb* nil *fe* nil *sm* nil *re* nil *mes* nil *c* nil *cut* nil
              *labeltrace* nil]
            (ERRSET (starthistory))
            (loop []
                (set! *oops* nil)
                (set! *lastsentno* (inc *lastsentno*))
                (set! *lastsent* *c*)
                (set! *sentno* (inc *sentno*))
                (set! *mes* 'NOPE)
                (set! *backref* nil)                  ;; ???????????????????
                (set! *n* (set! *sent* (ETAOIN)))
                (when (and (ERRSET (set! *pt* (set! *c* (parse 'CLAUSE 'MAJOR 'TOPLEVEL)))) *c*)
                    (set! *fe* (features *c*))
                    (set! *nb* *sent*)
                    (set! *h* (daughters *c*))
                    (when-not (ERRSET (answer *c*)) (print (or *oops* "I DON'T UNDERSTAND."))))
                (recur))))


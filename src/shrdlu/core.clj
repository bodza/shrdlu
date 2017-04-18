§ anno/winograd/README

This directory contains the source files for SHRDLU, a program
 written by Terry Winograd at the MIT AI Lab in 1968-1970.  For
 a full description, see Terry Winograd, Understanding Natural
 Language, Academic Press, 1972.

The version here is a cleaned up version done by Stu Card,
 Andee Rubin, and Terry Winograd in 1972.   The files are
 available for anyone who wants to use them (let me know
if you get something running!).

Terry Winograd (winograd@cs.stanford.edu)

--------

Excerpt from the  PROVISIONAL SHRDLU USERS' MANUAL (see file shrdlu/manual):

BRIEF DESCRIPTION OF SHRDLU
---------------------------

         SHRDLU IS A SYSTEM FOR THE  COMPUTER  UNDERSTANDING
    OF  ENGLISH.    THE  SYSTEM  ANSWERS QUESTIONS, EXECUTES
    COMMANDS, AND  ACCEPTS  INFORMATION  IN  NORMAL  ENGLISH
    DIALOG.    IT  USES  SEMANTIC INFORMATION AND CONTEXT TO
    UNDERSTAND DISCOURSE AND TO DISAMBIGUATE SENTENCES.   IT
    COMBINES  A COMPLETE SYNTACTIC ANALYSIS OF EACH SENTENCE
    WITH A "HEURISTIC  UNDERSTANDER"  WHICH  USES  DIFFERENT
    KINDS  OF  INFORMATION  ABOUT A SENTENCE, OTHER PARTS OF
    THE DISCOURSE, AND GENERAL INFORMATION ABOUT  THE  WORLD
    IN DECIDING WHAT THE SENTENCE MEANS.

         SHRDLU  IS  BASED  ON  THE  BELIEF  THAT A COMPUTER
    CANNOT DEAL  REASONABLY  WITH  LANGUAGE  UNLESS  IT  CAN
    "UNDERSTAND"  THE  SUBJECT IT IS DISCUSSING. THE PROGRAM
    IS GIVEN A DETAILED MODEL OF THE KNOWLEDGE NEEDED  BY  A
    SIMPLE  ROBOT  HAVING  ONLY A HAND AND AN EYE.  THE USER
    CAN GIVE IT  INSTRUCTIONS  TO  MANIPULATE  TOY  OBJECTS,
    INTERROGATE  IT ABOUT THE SCENE, AND GIVE IT INFORMATION
    IT WILL USE IN DEDUCTION.  IN ADDITION  TO  KNOWING  THE
    PROPERTIES  OF  TOY  OBJECTS,  THE  PROGRAM HAS A SIMPLE
    MODEL OF  ITS  OWN  MENTALITY.    IT  CAN  REMEMBER  AND
    DISCUSS ITS PLANS AND ACTIONS AS WELL AS CARRY THEM OUT.
    IT ENTERS INTO A DIALOG WITH  A  PERSON,  RESPONDING  TO
    ENGLISH  SENTENCES WITH ACTIONS AND ENGLISH REPLIES, AND
    ASKING FOR CLARIFICATION  WHEN  ITS  HEURISTIC  PROGRAMS
    CANNOT  UNDERSTAND A SENTENCE THROUGH USE OF CONTEXT AND
    PHYSICAL KNOWLEDGE.

         IN THE PROGRAMS, SYNTAX, SEMANTICS,  AND  INFERENCE
    ARE INTEGRATED IN A "VERTICAL" SYSTEM IN WHICH EACH PART
    IS CONSTANTLY COMMUNICATING WITH THE OTHERS. SHRDLU USES
    SYSTEMIC  GRAMMAR, A TYPE OF SYNTACTIC ANALYSIS WHICH IS
    DESIGNED  TO  DEAL  WITH   SEMANTICS.      RATHER   THAN
    CONCENTRATING  ON  THE EXACT FORM OF RULES FOR THE SHAPE
    OF LINGUISTIC  CONSTITUENTS,  IT  IS  STRUCTURED  AROUND
    CHOICES   FOR  CONVEYING  MEANING.    IT  ABSTRACTS  THE
    RELEVANT FEATURES OF THE LINGUISTIC STRUCTURES WHICH ARE
    IMPORTANT FOR INTERPRETING THEIR MEANING.

         IN  SHRDLU  MANY KINDS OF KNOWLEDGE ARE REPRESENTED
    IN THE FORM OF PROCEDURES RATHER THAN TABLES OF RULES OR
    LISTS  OF  PATTERNS.    BY DEVELOPING SPECIAL PROCEDURAL
    LANGUAGES FOR GRAMMAR, SEMANTICS, AND  DEDUCTIVE  LOGIC,
    THE  FLEXIBILITY  AND  POWER OF PROGRAMMING LANGUAGES IS
    GAINED    WHILE    RETAINING    THE    REGULARITY    AND
    UNDERSTANDABILITY  OF SIMPLER RULE FORMS.  EACH PIECE OF
    KNOWLEDGE CAN BE A PROCEDURE, AND CAN CALL ON ANY  OTHER
    PIECE OF KNOWLEDGE IN THE SYSTEM.

IMPLEMENTATION AND VERSION INFORMATION
--------------------------------------

        SHRDLU  WAS  PROGRAMMED  AT   THE   MIT   ARTIFICIAL
    INTELLIGENCE  LABORATORY  BY  T.   WINOGRAD AS PART OF A
    DOCTORAL DISSERTATION IN MATHEMATICS.

        THE PROGRAM WAS MODIFIED DURING THE LAST YEAR BY  T.
    WINOGRAD,  D.  MACDONALD, J. HILL, AND S. CARD TO CHANGE
    SOME OF ITS INTERNAL REPRESENTATIONS  AND  TO  MAKE  THE
    CODE  EASIER  TO UNDERSTAND FOR PERSONS WISHING TO STUDY
    THE PROGRAM.  NO MAJOR ATTEMPTS WERE  MADE  TO  INCREASE
    ITS POWER.

        THE PROGRAM RUNNING AT C-MU IS THE MODIFIED VERSION.
    THE DISPLAY FACILITIES OF  THE  PROGRAM  HAVE  NOT  BEEN
    IMPLEMENTED  AT  C-MU.  THE PROGRAM WAS COAXED AWAY FROM
    MIT'S  INCOMPATIBLE  TIME-SHARING   SYSTEM   (ITS)   AND
    CONVERTED  TO RUN UNDER THE DEC TOPS10 (10-50) OPERATING
    SYSTEM BY CONVERTING MACLISP ITSELF  (AND  TO  DO  THAT,
    CONVERTING  THE  MIDAS  ASSEMBLY LANGUAGE).  THE MACLISP
    CONVERSION WAS DONE BY GEORGE ROBERTSON.

THE VERSION OF SHRDLU BEING DISTRIBUTED FROM  CMU  IS  NAMED
    THE  C1  VERSION.  IT IS CURRENT WITH THE MIT VERSION TO
    JUNE 1972.  THE SHOW AND TELL USER INTERFACE AND VARIOUS
    CHANGES   WERE  ADDED  FOR  THE  C-MU  WORKSHOP  ON  NEW
    TECHNOLOGIES IN COGNITIVE RESEARCH IN JUNE 1972.

        SHRDLU IS  WRITTEN  IN  MACLISP  1.6  (VINTAGE  JUNE
    1972).  IT USES ABOUT 100 TO 140K 36-BIT WORDS OF MEMORY
    ON A PDP-10.

§ anno/winograd/blurb

    This directory is maintained by Dave McDonald  -DDM  rm.824

-There is no working version of the entire SHRDLU program because
there is no working implementation of MICROPLANNER. Contributions
of same gladly accepted.

-There is, however, a working version of the PROGRAMMAR parser and
grammar that Terry and I developed. It lives as SHRDL1;TS PARSER
and is relatively self-explanitory when started from DDT.

-This directory contains the expr code for the entire SHRDLU system
plus what documentation was ever written and a cross-INDEX'ing.
Requests are often made for the system by people at other instalations
and i send them the contents of this directory

                    7/29/75    -ddm

§ anno/winograd/demo

(SETQ ERRLIST '((VALRET ':TWDEMO/ )))
(IOC W)
(SETQ PCONSTANT 1)(SETQ PRANDOM 0)
(SETQ WAIT 100)
(SETQ MECONSTANT 1)(SETQ MERANDOM 0)
(SETQ YOUCONSTANT 1)(SETQ YOURANDOM 3)
(SETQ TEXTSIZE 2)
(DISME)(DISYOU)(DTHRU)

@  PICK UP A BIG RED BLOCK.@
(DISME)(FRAME WAIT)
(MOVETO 100 340 500)
(GRASP (QUOTE :B7))
(MOVETO 600 374 200)
(UNGRASP)
(MOVETO 100 440 300)
(GRASP (QUOTE :B6))
(MOVETO 100 440 1000)
(DTHRU)
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@GRASP THE PYRAMID.@
(DISME)(FRAME WAIT)(DTHRU)
@  I DON'T UNDERSTAND WHICH PYRAMID YOU MEAN.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  FIND A BLOCK WHICH IS TALLER THAN THE ONE YOU ARE HOLDING AND PUT IT INTO THE BOX.@
(DISME)(FRAME WAIT)(DTHRU))
@BY "IT", I ASSUME YOU MEAN THE BLOCK WHICH IS TALLER THAN THE ONE I AM HOLDING.@
(MOVETO 140 600 300) (UNGRASP) (MOVETO 400 700 400) (GRASP (QUOTE :B10)) (MOVETO 700 1100 401) (UNGRASP)(DTHRU)
(DISME)(DTHRU)
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHAT DOES THE BOX CONTAIN?@
(DISME)(FRAME WAIT)(DTHRU)
@  THE BLUE PYRAMID AND THE BLUE BLOCK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHAT IS THE PYRAMID SUPPORTED BY?@
(DISME)(FRAME WAIT)(DTHRU)
@  THE BOX.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  HOW MANY BLOCKS ARE NOT IN THE BOX?@
(DISME)(FRAME WAIT)(DTHRU)
@ FOUR OF THEM.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  IS AT LEAST ONE OF THEM NARROWER THAN THE ONE WHICH I TOLD YOU TO PICK UP?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES,  THE RED CUBE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  IS IT SUPPORTED?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES,  BY THE TABLE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  CAN THE TABLE PICK UP BLOCKS?@
(DISME)(FRAME WAIT)(DTHRU)
@  NO.@
(FRAME WAIT)(DISYOU)(DTHRU)
@CAN A PYRAMID BE SUPPORTED BY A BLOCK?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES.@
(FRAME WAIT)(DISYOU)(DTHRU)
@CAN A PYRAMID SUPPORT A PYRAMID?@
(DISME)(FRAME WAIT)(DTHRU)
@  I DON'T KNOW.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  STACK UP TWO PYRAMIDS.@
(DISME)(FRAME WAIT)(DTHRU)
@  I CAN'T.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WILL YOU PLEASE STACK UP BOTH OF THE RED BLOCKS AND EITHER A GREEN CUBE OR A PYRAMID?@
(DISME)(FRAME WAIT)
(MOVETO 140 600 300)
(GRASP (QUOTE :B6))
(MOVETO 274 1000 300)
(UNGRASP)
(MOVETO 600 374 200)
(GRASP (QUOTE :B7))
(MOVETO 274 1000 500)
(UNGRASP)
(MOVETO 150 140 200)
(GRASP (QUOTE :B2))
(MOVETO 1000 274 100)
(UNGRASP)
(MOVETO 150 140 100)
(GRASP (QUOTE :B1))
(MOVETO 274 1000 600)
(UNGRASP)
(DTHRU)
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHICH CUBE IS SITTING ON THE TABLE?@
(DISME)(FRAME WAIT)(DTHRU)
@  THE LARGE GREEN ONE WHICH SUPPORTS THE RED PYRAMID.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  IS THERE A LARGE BLOCK BEHIND A PYRAMID?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES, THREE OF THEM:  A LARGE RED ONE, A LARGE GREEN CUBE, AND THE BLUE ONE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  PUT A SMALL ONE ONTO THE GREEN CUBE WHICH SUPPORTS A PYRAMID.@
(DISME)(FRAME WAIT)
(GRASP (QUOTE :B1))
(MOVETO 477 40 300)
(UNGRASP)
(DTHRU)
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@PUT THE LITTLEST PYRAMID ON TOP OF IT.@
(DISME)(FRAME WAIT)
(MOVETO 1000 274 100)
(GRASP (QUOTE :B2))
(MOVETO 477 40 400)
(UNGRASP)
(DTHRU)
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  DOES THE SHORTEST THING THE TALLEST PYRAMID'S SUPPORT SUPPORTS SUPPORT ANYTHING GREEN?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES,  THE GREEN PYRAMID.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHAT COLOR IS IT?@
(DISME)(FRAME WAIT)(DTHRU)
@BY "IT", I ASSUME YOU MEAN THE SHORTEST THING THE TALLEST PYRAMID'S SUPPORT SUPPORTS.@
(FRAME WAIT) (DISME)(DTHRU)
@  RED@
(FRAME WAIT)(DISYOU)(DTHRU)
@  HOW MANY THINGS ARE ON TOP OF GREEN CUBES?@
(DISME)(FRAME WAIT)(DTHRU)
@  I'M NOT SURE WHAT YOU MEAN BY "ON TOP OF "IN THE PHRASE "ON TOP OF GREEN CUBES "  .
  DO YOU MEAN:
1 - DIRECTLY ON THE SURFACE
2 - ANWHERE ON TOP OF ?@
(FRAME WAIT)(DISAD 'YOU NIL  '/ 2)(DISME)(DTHRU)
@ THREE OF THEM.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  HAD YOU TOUCHED ANY PYRAMID BEFORE YOU PUT THE GREEN ONE ON THE LITTLE CUBE?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES,  THE GREEN ONE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHEN DID YOU PICK IT UP?@
(DISME)(FRAME WAIT)(DTHRU)
@  WHILE I WAS STACKING UP THE RED CUBE, A LARGE RED BLOCK AND A LARGE GREEN CUBE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHY?@
(DISME)(FRAME WAIT)(DTHRU)
@  TO GET RID OF IT.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHY DID YOU DO THAT?@
(DISME)(FRAME WAIT)(DTHRU)
@  TO CLEAN OFF THE RED CUBE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHY DID YOU CLEAR OFF THAT CUBE?@
(DISME)(FRAME WAIT)(DTHRU)
@  TO PUT IT ON A LARGE GREEN CUBE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHY DID YOU DO THAT?@
(DISME)(FRAME WAIT)(DTHRU)
@  TO STACK UP THE RED CUBE , A LARGE RED BLOCK AND A LARGE GREEN CUBE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHY DID YOU DO THAT?@
(DISME)(FRAME WAIT)(DTHRU)
@  BECAUSE YOU ASKED ME TO.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  HOW DID YOU DO IT?@
(DISME)(FRAME WAIT)(DTHRU)
@ BY PUTTING A LARGE RED BLOCK ON THE TABLE ; THEN LETTING GO OF IT ; THEN PUTTING A LARGE GREEN CUBE ON IT ; THEN LETTING GO OF THAT CUBE ; THEN PUTTING THE RED CUBE ON THAT CUBE ; THEN LETTING GO OF THAT CUBE.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  HOW MANY OBJECTS DID YOU TOUCH WHILE YOU WERE DOING IT?@
(DISME)(FRAME WAIT)(DTHRU)
@ FOUR OF THEM.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHAT DID THE RED CUBE SUPPORT BEFORE YOU STARTED TO CLEAN IT OFF?@
(DISME)(FRAME WAIT)(DTHRU)
@  THE GREEN PYRAMID.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  THERE WERE FIVE BLOCKS TO THE LEFT OF THE BOX THEN.@
(DISME)(FRAME WAIT)(DTHRU)
@ NO, ONLY FOUR OF THEM: THE RED CUBE , TWO LARGE GREEN CUBES AND A LARGE RED BLOCK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  PUT THE BLUE PYRAMID ON THE BLOCK IN THE BOX.@
(UNGRASP)
(MOVETO 740 740 201)
(GRASP (QUOTE :B4))
(MOVETO 700 1100 601)
(DTHRU)
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  IS THERE ANYTHING WHICH IS BIGGER THAN EVERY PYRAMID BUT IS NOT AS WIDE AS THE THING THAT SUPPORTS IT?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES,  THE BLUE BLOCK. @
(FRAME WAIT)(DISYOU)(DTHRU)
@ DOES A STEEPLE@
(DISME)(DTHRU)
@SORRY, I DON'T KNOW THE WORD "STEEPLE".  PLEASE TYPE TWO SPACES.@
LI(DISYOU)(DTHRU)
@  A "STEEPLE" IS A STACK WHICH CONTAINS TWO GREEN CUBES AND A PYRAMID.@
(DISME)(FRAME WAIT)(DTHRU)
@  I UNDERSTAND.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  ARE THERE ANY STEEPLES NOW?@
(DISME)(FRAME WAIT)(DTHRU)
@  NO.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  BUILD ONE.@
(DISME)(FRAME WAIT)
(MOVETO 274 1000 500)
(GRASP (QUOTE :B7))
(MOVETO 200 320 200)
(UNGRASP)
(MOVETO 477 40 400)
(GRASP (QUOTE :B2))
(MOVETO 200 110 100)
(UNGRASP)
(MOVETO 477 40 300)
(GRASP (QUOTE :B1))
(MOVETO 1000 274 100)
(UNGRASP)
(MOVETO 540 140 500)
(GRASP (QUOTE :B5))
(MOVETO 40 500 300)
(UNGRASP)
(MOVETO 500 100 200)
(GRASP (QUOTE :B3))
(MOVETO 200 320 400)
(UNGRASP)
(MOVETO 40 500 300)
(GRASP (QUOTE :B5))
(MOVETO 200 320 700)
(UNGRASP)
(DTHRU)
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  CALL THE BIGGEST BLOCK "SUPERBLOCK".@
@  OK.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  HAVE YOU PICKED SUPERBLOCK UP SINCE WE BEGAN?@
(DISME)(FRAME WAIT)(DTHRU)
@  YES.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  WHY DID YOU DROP IT?@
(DISME)(FRAME WAIT)(DTHRU)
@BY "IT", I ASSUME YOU MEAN THE SUPERBLOCK.
 I CAN'T EXPLAIN A NON-EXISTENT EVENT.@
(FRAME WAIT)(DISYOU)(DTHRU)
@  IS THERE ANYTHING TO THE RIGHT OF THE RED PYRAMID?@
(DISME)(FRAME WAIT)(DTHRU)
@ YES, FOUR THINGS:  THE BOX , THE RED CUBE, THE BLUE PYRAMID AND THE BLOCK NAMED SUPERBLOCK@
(FRAME WAIT)(DISYOU)(DTHRU)
@  THANK YOU.@
(DISME)(FRAME WAIT)(DTHRU)
@  YOU'RE WELCOME!@
(AND(READCH)(NOT ^A)(VALRET ':KILL/ ))

§ anno/winograd/fasl

NET-MAIL-FROM-HOST:1200000013
RETURN-PATH:@SAIL.STANFORD.EDU,@DIAMOND.S4CC.Symbolics.COM:Greenwald@STONY-BROOK.SCRC.Symbolics.COM
TO:"mly-lispm@MC.LCS.MIT.EDU

TEXT;-1
Received: from SAIL.STANFORD.EDU (TCP 1200000013) by MC.LCS.MIT.EDU 22 Dec 86 09:26:46 EST
Received: from [128.81.51.3] by SAIL.STANFORD.EDU with TCP; 22 Dec 86  06:10:54 PST
Received: from SWALLOW.S4CC.Symbolics.COM by DIAMOND.S4CC.Symbolics.COM via CHAOS with CHAOS-MAIL id 39985; Mon 22-Dec-86 09:09:17 EST
Date: Mon, 22 Dec 86 09:08 EST
From: Michael Greenwald <Greenwald@STONY-BROOK.SCRC.Symbolics.COM>
Subject: sharp plus question
To: DCP@QUABBIN.SCRC.Symbolics.COM, DFM%JASPER@LIVE-OAK.LCS.MIT.EDU,
    common-lisp@SU-AI.ARPA
cc: dfm@JASPER.Palladian.COM
In-Reply-To: <861219125356.4.DCP@KOYAANISQATSI.S4CC.Symbolics.COM>
Message-ID: <861222090840.6.GREENWALD@SWALLOW.S4CC.Symbolics.COM>

    Date: Fri, 19 Dec 86 12:53 EST
    From: David C. Plummer <DCP@QUABBIN.SCRC.Symbolics.COM>

    Date: Fri, 19 Dec 86 11:59 EST
    From: Don Morrison <dfm@JASPER.Palladian.COM>

    What should the second form in the following read as?

        (push :mumble *features*)
        '(#-mumble #+mumble 1 2 3)

    I would expect '(2 3).  Both implementations I've tried read '(3), which
    is completely unintuitive to me.  Such a thing can easily come up in real
    life (e.g. commenting out with #+(or) something that's already under
    #+mumble).

                          Don Morrison
                          Palladian Software, Inc.

    This is curious.  In the Symbolics 7.0 implementation,
        '(#+non-existent-feature #-non-existent-feature 1 2 3)
        '(#-non-existent-feature #+non-existent-feature 1 2 3)
        '(#+LISPM #-LISPM 1 2 3)
    each read as '(2 3) but
        '(#-LISPM #+LISPM 1 2 3)
    does read as '(3).  This does seem wrong on the following grounds:
        #-LISPM goes into the mode "read me a form, and ignore it."  It
        recursively invokes the reader.
        The reader gets #+LISPM.  #+LISPM goes into the mode "read
        me a form and don't ignore it."

Currently, the Symbolics' reader treats both #+ and #- as "ignore next
form" when *READ-SUPPRESS* is T.  This is probably a misinterpretation
of *READ-SUPPRESS*.  It was done to solve the problem of an illegally
formed expression following a #+ inside a form with *READ-SUPPRESS* 'T.

For example,
  (PROGN
    #+IMPLEMENTATION-X
    (INCF (FROB-KNOB GROZZLE)
      #+SYS:GREEPS-ALLOWED 3
      #-(CAR FGR:*GROZZLE-MODES*) 2)
    ....)

Is the correct interpretation to obey #+ and #- even inside a
*READ-SUPPRESS*?  (while still suppressing errors inside the feature
specification?)

Clearly #+NON-FEATURE (A B #-NON-FEATURE C), shouldn't cause read errors
while reading C.

The question is whether the internal "feature specification" should be
read with *READ-SUPPRESS* bound specially to NIL or not.
If we keep the current binding of *READ-SUPPRESS*, then feature will
always be NIL (*READ-SUPPRESS* causes all extended tokens to be NIL).
If we bind *READ-SUPPRESS* to NIL to read the feature specification,
then syntactic "errors" in the feature can cause errors.

The problem of supporting (by ignoring) non-standard syntax in feature
specifications doesn't need to be part of COMMON-LISP (I don't think
CL allows extensions there), the question of nested #-'s does need to be
made unambiguous.

Implementations (Symbolics' for example) that want to be generous in
what they accept without error, can handle that themselves.

                        This recursively invokes
        the reader.
            The reader reads 1.
        #+LISPM does not ignore the 1, so it returns it as the thing
        read.
        #-LISPM is given 1 as the result of the read, and ignores it.
        2 and 3 are still in the input stream, so I don't know how both
        of them manage to get ignored.

    [I'm not sure what the current state of our mailer is, but I think mail
    addressed to DFM%JASPER@LIVE-OAK.LCS.MIT.EDU will eventually get to me.]

I'm not on the COMMON-LISP mailing list, but someone from Symbolics can
forward to me any replies, if there is some trouble with the return
paths.

§ anno/winograd/file-note

Date: Wed, 26 Aug 87 10:39 EDT
From: John C. Mallery <JCMA@AI.AI.MIT.EDU>
Subject: Re: Schrdlu [sic]
To: WINOGRAD@CSLI.STANFORD.EDU
In-Reply-To: The message of 24 Aug 87 18:35 EDT from WINOGRAD@CSLI.STANFORD.EDU
Message-ID: <870826103913.1.JCMA@MORRISON.AI.MIT.EDU>

    Date: Mon 24 Aug 87 15:35:00-PDT
    From: WINOGRAD@CSLI.STANFORD.EDU

    That's very interesting.  In had totally lost track of the files (I
    don't even have a listing!).  Can you give me more info so I can get
    copies of the files?

I include a dired of the directory below.

    I often get requests from people who want a running version, and always
    ask them to let me know if they find one.

Well, I don't know about running.  ITS and Maclisp have both evolved.  You might
be able to find backup versions of the lisp it runs under but ITS changes
would need to be rehacked.  It would probably be simplest to just rewrite
the system in common lisp.  Short of that, you could have somebody rehack
the interface and update the lisp to run in a common lisp.

    And there it was back at MIT all the time! (I had looked on AI, but not
    MC). --t

Well, it was on backup tape and CENT@AI retrieved it for some reason.  Alan
Bawden (Alan@AI.AI.MIT.EDU) can give you substantially more information about
running it again under ITS or finding an old version of Maclisp.  But, it
seems that updating is definitely a better bet.  I would imagine that a
version for PCs might be of value.

So, got any new papers that I want?  Are you going to Hayward's sociology
conference?

MC: SHRDLU; * *
  Free: #0=5733
  251 blocks in the files listed
  0   !THIS! !DIR!     1    142(36)        07/29/75 10:55:35 (04/29/87)   CENT
  0   #FILES 6         2   1373(36)        06/26/74 18:06:52 (08/18/87)   CENT
  0   .MACR  21        1    872(36)        10/20/76 21:02:13 (03/20/86)   CENT
  0   AR0    JAN71    46  47104(36)        05/06/76 22:24:59 (03/20/86)   CENT
  0   AR1    1        17  17408(36)        09/03/77 16:32:41 (03/20/86)   CENT
  0   AR2    1        14  14336(36)        09/03/77 16:28:36 (03/20/86)   CENT
  0   BLOCKL 4         3   2139(36)        11/03/74 13:31:29 (03/20/86)   CENT
  0   BLOCKP 3         5   4817(36)        10/20/76 21:03:25 (03/20/86)   CENT
  0   CGRAM  31       11  11010(36)        10/20/76 22:17:54 (03/20/86)   CENT
  0   DATA   6         1    703(36)        10/20/76 22:18:42 (03/20/86)   CENT
  0   DEMO   FLICK     2   1559(36)        06/22/74 19:40:38 (03/20/86)   CENT
  0   DICTIO 73        9   8643(36)        04/08/73 18:37:12 (03/20/86)   CENT
  0   GINTER 5         1    573(36)        05/07/74 13:47:29 (03/20/86)   CENT
  0   GRAMAR 28       15  14570(36)        06/02/75 14:25:06 (08/18/87)   CENT
  0   GRAPHF FASL      6   5243(36)        05/06/76 22:39:03 (02/27/86)   CENT
  0   GRAPHF STUFF     2   1700(36)        05/06/76 22:38:26 (03/20/86)   CENT
  0   HELP   DOC       2   1190(36)        02/12/73 16:31:57 (08/18/87)   CENT
  0   LISP   USAGE    10  10001(36)        06/26/74 18:23:43 (03/20/86)   CENT
  0   LOADER 18        1    707(36)        01/06/75 14:40:06 (08/18/87)   CENT
  0   MANNEW 2         1    575(36)        04/21/74 16:40:30 (03/20/86)   CENT
  0   MANUAL CMU      14  14209(36)        10/17/72 11:41:38 (06/18/86)   CENT
  0   MINIH  DOC       1    264(36)        02/12/73 16:31:41 (03/20/86)   CENT
  0   MORPHO 13        3   2113(36)        12/31/76 13:10:50 (03/20/86)   CENT
  0   NEWANS 78       10  10105(36)        11/03/74 18:31:01 (03/20/86)   CENT
  0   PARSER 8         3   2378(36)        05/06/76 22:37:40 (08/18/87)   CENT
  0   PLNR   182      16  76934(7)         08/18/87 17:05:39 (08/18/87)   AI0
  0   PROGMR 57        3   3041(36)        06/02/75 14:25:29 (03/20/86)   CENT
  0   SETUP  62        2   1178(36)        11/03/74 13:32:50 (03/20/86)   CENT
  0   SHOW   13        5   4340(36)        11/03/74 13:22:58 (08/18/87)   CENT
  0   SHRDLU (INIT)    1    491(36)        05/26/77 20:24:21 (01/17/87)   CENT
  0   SMASS  19        1    766(36)        11/03/74 13:26:51 (03/20/86)   CENT
  0   SMSPEC 96        7   6894(36)        10/31/76 11:17:05 (03/20/86)   CENT
  0   SMUTIL 148       9   9091(36)        11/03/74 18:10:43 (03/20/86)   CENT
  0   SYSCOM 180       4   3799(36)        12/31/76 13:06:41 (03/20/86)   CENT
  0   THTRAC 22        2   1239(36)        08/30/01 14:41:37 (03/20/86)   CENT
  0   TS     TWDEMO   20  20371(36)        05/06/76 22:36:44 (08/18/87)   CENT

§ anno/winograd/help

         Instructions for Running SHRDLU

     SHRDLU can be in 4 basic states, COMMAND, READY, RUN,
and REQUEST.  It is initially in READY when loaded.

***
******COMMAND STATE
***

     In this state, SHRDLU expects the user to type a command.
It lets you know this by typing ">>>".
A command is a line containing one or more words, separated by
spaces and terminated by two carriage returns (<CR>).  The first
word must be one of the three words SHOW, TELL, or GO.  The
SHOW command is used to ask the system to show such things
as definitions, structures it is building, and the states of various
parameters and switches.  TELL is used to tell the system new
definitions and settings.
     After executing a COMMAND, the system is ready for
another one, and prompts by saying >>>.
You can leave COMMAND state by typing T,NIL, or $P (<alt mode>-P)
instead of a command.  This will cause the program to continue
whatever it was doing before it entered COMMAND
state, or to go to READY state if it was not already in the
process of analyzing a sentence.  If instead, you type the
command "GO", it will drop the sentence it is working on, and go
into READY state for a new one.
      Of course, the COMMAND state is just a slightly-fudged
LISP listen loop, so you can type any atom or S-expression at it
to be evaluted, go from it into the LISP editor, define new functions,etc.

****
******COMMAND FORMATS
****

     The SHOW and TELL commands are based on  trees
(one tree for each).  The first word of the command is SHOW
or TELL, the second names a node in the corresponding tree,
and the rest are any arguments appropriate to the action
at that node.
For example, the command:

SHOW FUNCTION MEET

will display the contents of the LISP function "MEET".

SHOW SHOW

displays the "SHOW" tree, while for the "TELL" tree, you type

SHOW TELL

     If all of the arguments are not specified, the system will
request more.  For example, typing:
SHOW FUNCTION
would cause it to type out:
FUNCTION:
requesting the name of a function from the user.  It is then
in REQUEST state (see below.)
     Non-terminal nodes of the tree may or may not have corresponding
actions.  For example, the command

TELL PLANNER OFF

causes the entire batch of PLANNER tracing devices to be turned off
at once, even though there are subnodes of the PLANNER node which
can be used to turn individual features on and off selectively.
If there is no action associated with such a node, the system will
ask the user to select one of its subnodes.
     If you type "SHOW" or "TELL" followed by two carriage return,
it will guide you through the choices, using REQUESTS
(see below).
     Typing HELP <CR> <CR> will allow you to see this message or
a shorter version which contains just the most salient details of the four states.

*****
*****REQUEST STATE
*****

     SHRDLU can request two kinds of information from
the user.  If it wants him to CHOOSE between a set of alternatives,
it will end the request with a ?.  If it wants him to SPECIFY
a name or list of names, it will end it with a :.
     For a CHOOSE, all it needs is enough information to decide
between the alternatives.  Begin typing your choice, and when it
is complete enough, type a <period>.  If you type fewer
letters than necessary (e.g. typing just a P, when PLANNER
and PARSING are among the choices) it will do nothing and wait
for you to continue.  If you type more than necessary it doesn't
matter.
     For a SPECIFY, type the required response, terminated by a
<CR>.  If you type a <LF> with nothing else, it will take some
default response which is appropriate to the action (For example,
typing a <LF> in response to a request for which properties of an
atomare to be displayed will have the effect of displaying
all of them.
    For either SPECIFY or CHOOSE, you can get more information on
what is expected by typing a ?<CR>.  It will then give you the
request again.  Typing QUIT<CR> at a "SPECIFY" REQUEST or QUIT
<CR> at a "CHOOSE" REQUEST will cause the entire command
of which it was a part to be discarded without finishing,
returning to COMMAND state.

*****
******READY STATE
*****

     The READY state is entered only when a new English sentence is to
be input.  You can tell you are in it when the sytem types
READY
Respond by typing in an English sentence in normal punctuation
(i.e. ending with a question mark or period).
The system will automatically begin processing it, entering
RUN state.  To get into a COMMAND state while entering a sentence,
type  <alt-mode> <alt-mode>.

*****
******RUN STATE
*****

     Whenever a sentence is input, the system begins to RUN.  It
will stop at selected places, entering COMMAND state so the user can
SHOW things and TELL it things before continuing.  There are various
TELL commands which explain how to change these stopping points.
When a T,NIL, or <alt>-P is typed at the COMMAND state, the system returns to RUN
and continues.

     Any word which appears in the SHOW or TELL trees can be
abbreviated by typing its first two letters.  For example,
our first command above could have been abbreviated as:
SH FU MEET
Note that arguments cannot be abbreviated since the system has no
list to check the abbreviations against.  This is also true
of responses to a "SPECIFY" REQUEST.  Responses to a "CHOOSE" REQUEST
are abbreviated by typing any initial letter string followed by
<period> as described above.

§ anno/winograd/lisp

======================================================================
              LISP FUNCTIONS AND WHERE THEY WERE CALLED
======================================================================

*DIF
           CALLED BY USER FUNCTIONS-
                                 THVAL-MULT DP

-
           CALLED BY USER FUNCTIONS-
                                 TIME-SINCE

=
           CALLED BY USER FUNCTIONS-
                                 GROW

ADD1
           CALLED BY USER FUNCTIONS-
                                 FINDNUM THVAL-MULT NAMENUM LISTNAMES
                                 ANSELIMINATE SMNG3 PLNR-FINDSPEC
                                 MAKESYM CLEANX SHRDLU

AND
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF DEFINE QUERY SUBLEAF
                                 SHOWSCENE ***TOPLEVEL*** PEV
                                 UNLABELTRACE PASSING LABELTRACE
                                 STARTHISTORY PACKORD PACKO OCCUPIER
                                 MEMOREND FINDSPACE CLEAR VBFIX2
                                 VBFIX NAMEVENT MUNG FINDCHOOSE
                                 THVAL-MULT PREPPUT ONECHECK
                                 NAMESUGAR NAMEOBJ LISTNAMES HEADPART
                                 FINDMOTHER DESCRIBEVENT ANSWER
                                 ANSREL ANSQUEST ANSNOREL ANSGEN
                                 ANSELIMINATE SMBIND SMCL-MODIFIERS
                                 SMCL1 SMRELATE SMPOSS2 SMPOSS SMONE3
                                 SMONE2 SMONE SMNG3 SMNG2 SMIT2 SMIT
                                 SMADJG-PREPG SMVG THVAL2 EXPAND
                                 RELFIND PLNR-DESCRIBE PLNR-REMTIME
                                 PLNR-RECOMMENDIFY MAPBLAND BUILD
                                 IMPERF? ISTENSE CANPARSE BOTH :
                                 UPCHECK UPREL SECONDWORD? POP PARSE3
                                 PARSE2 ONLY-ONE-WORD-LEFT FOLLOWING
                                 FLUSHME MOVE-PTW MOVE-PT
                                 ONE-WORD-LEFT REMOVE-F-PT ADD-F-PT
                                 DEFS WALLP PR2 ETNEW ERTEX DISP
                                 COMBINATION? CLEANX BUILDWORDLIST
                                 BUILDWORD DP ETAOIN TIMER SHRDLU DA

APPEND
           CALLED BY USER FUNCTIONS-
                                 VBFIX PRTPUT NAMEVENT FINDCHOOSE
                                 NAMEOBJ LISTNAMES FINDMOTHER ELIZA
                                 DESCRIBEVENT ANSREL ANSQUEST
                                 ANSNOREL ANSNAME ANSCOMMAND
                                 NAMEACTION SMNG3 SMPRON SETQQCHECK
                                 ORDMAKE PLNR-PROGIFY PLNR-GOALIFY
                                 PLNR-FINDIFY MAPBLAND CANPARSE
                                 CANTAKE PARSE3 PARSE2 MAKESYM ETAOIN

APPLY
           CALLED BY USER FUNCTIONS-
                                 SHOWMOVE SHOWPROP SHOWTELLCHOICE
                                 SHOWTELL ***TOPLEVEL*** MEMOREND
                                 NAMESIZE FINDMOTHER ELIZA ANSQUEST
                                 ANSDECLARE MEASURE CANPARSE
                                 INTERPRET DSAY SHRDLU

ASCII
           CALLED BY USER FUNCTIONS-
                                 REQUEST QUERY ***TOPLEVEL***

ASSOC
           CALLED BY USER FUNCTIONS-
                                 SHOWSCENE NAMEOBJ LISTNAMES
                                 FINDMEASURE

ASSQ
           CALLED BY USER FUNCTIONS-
                                 ATAB ELIZA SMCL1

ATOM
           CALLED BY USER FUNCTIONS-
                                 UNLABELTRACE PASSING LABELTRACE SIZE
                                 LISTIFY FINDCHOOSE ANSTHMELEMENT
                                 ANSTHM CHECKREL WHO EXPAND
                                 PLNR-REMTIME PLNR-JUNKIFY2
                                 PLNR-JUNKIFY MAPBLAND EVALCHECK
                                 ATOMIFY GOCHECK UPREL PARSE3
                                 MOVE-PTW MOVE-PT PR2 PR1 ERTEX NTH
                                 LIS2FY

CAAAR
           CALLED BY USER FUNCTIONS-
                                 PACKORD

CAADDR
           CALLED BY USER FUNCTIONS-
                                 PACKON GROW FINDSPACE PLNR-MUNG

CAADR
           CALLED BY USER FUNCTIONS-
                                 GROW FINDSPACE LISTNAMES MOVE-PTW
                                 MOVE-PT

CAAR
           CALLED BY USER FUNCTIONS-
                                 TFIND STARTIME ORDER OCCUPIER
                                 MEMOREND GROW FINDSPACE ENDTIME
                                 CLEAR MUNG PARSE-ASSOC ANSELIMINATE
                                 PARSEREL PARSE3

CADAAR
           CALLED BY USER FUNCTIONS-
                                 PACKORD

CADADR
           CALLED BY USER FUNCTIONS-
                                 GROW FINDSPACE NAMEACTION

CADAR
           CALLED BY USER FUNCTIONS-
                                 SHOWSCENE STARTHISTORY OCCUPIER GROW
                                 FINDSPACE CLEAR

CADDAR
           CALLED BY USER FUNCTIONS-
                                 OCCUPIER CLEAR NAMEOBJ ANSELIMINATE

CADDDR
           CALLED BY USER FUNCTIONS-
                                 IASS NAMEACTION PLNR-MUNG
                                 BUILDWORDLIST
CADDR
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL*** UNLABELTRACE
                                 LABELTRACE TCENT SUPPORT SIZE PACKON
                                 OCCUPIER LOCGREATER GROW FINDSPACE
                                 CLEAR PARAP MUNG NAMEACTION QTYPE?
                                 PLNR-MUNG PLNR-JUNKIFY2 PLNR-JUNKIFY
                                 BUILDWORDLIST BCWL ETAOIN

CADR
           CALLED BY USER FUNCTIONS-
                                 DEFINETHEOREM ONOFF ***TOPLEVEL***
                                 CHARG SHOWSCENE TCENT SUPPORT
                                 STARTHISTORY PACKORD PACKON PACKO
                                 OCCUPIER LOCGREATER GROW FINDSPACE
                                 CLEAR VBFIX2 PARAP NAMEVENT MUNG
                                 IASS FINDNUM FINDCHOOSE LISTNAMES
                                 ANSWER ANSREL ANSORDER ANSNOREL
                                 ANSNAME ANSELIMINATE SMCL1 SMONE3
                                 QUANTIFIER? SETQQCHECK EXPAND
                                 MEASURE FINDMEASURE PLNR-PROGIFY
                                 PLNR-NOTIFY PLNR-MUNG PLNR-FINDSPEC
                                 PLNR-FINDIFY PLNR-THCONSIFY
                                 PLNR-JUNKIFY2 PLNR-JUNKIFY MUMBLE
                                 MAPC2 NEWCOPY BUILD GOCOND GOCHECK
                                 SECONDWORD? PARSE3 ISQ MOVE-PTW
                                 MOVE-PT DEFS MOD DISP DEFLIST
                                 BUILDWORDLIST BCWL DP ETAOIN

CAR
           CALLED BY USER FUNCTIONS-
                                 DEFINE ***TOPLEVEL*** SHOWPROP QUERY
                                 SHOWTELL SHOWCHOICE TELLCHOICE
                                 SHOWSCENE UNLABELTRACE LABELTRACE
                                 TCENT SUPPORT STARTHISTORY PACKORD
                                 PACKON ORDER OCCUPIER MEMOREND GROW
                                 GOAL FINDSPACE CLEAR VBFIX2 PARAP
                                 NAMEVENT FINDNUM FINDCHOOSE
                                 PLURALMAKE PLURALIZE ONECHECK
                                 NAMEOBJ LISTNAMES ELIZA DESCRIBEVENT
                                 ANSWER ANSTHM ANSREL ANSORDER
                                 ANSNOREL ANSNAME PARSE-ASSOC
                                 ANSELIMINATE ANSCOMMAND NAMEACTION
                                 SMBIND SMCL1 SMONE3 SMONE SMNG3
                                 SMNG2 SMIT2 SMIT SMVG NUMBER?
                                 CHECKAMARKER CHECK WHO SETQQCHECK
                                 EXPAND PLNR-DESCRIBE
                                 PLNR-RECOMMENDIFY PLNR-PROGIFY
                                 PLNR-NOTIFY PLNR-MUNG PLNR-GOALIFY
                                 PLNR-FINDSPEC PLNR-FINDIFY
                                 PLNR-THCONSIFY PLNR-JUNKIFY2
                                 PLNR-JUNKIFY MUMBLE MAPC2 MAPBLAND
                                 ITERATEX EVALCHECK DOBACKREF NEWCOPY
                                 BUILD IMPERF? ISTENSE ATOMIFY BOTH
                                 GOCOND PDEFINE WORD SETR ROOT PTFIND
                                 PREVIOUS POP PARSE3 PARSE2 PARSE NQ
                                 NEXTWORD? NEXTWORD NEXTW GETR CQ
                                 MOVE-PTW MOVE-PT REMOVE-F-PT
                                 ADD-F-PT DEFS UNION STA SETDIF MOD
                                 MEET ETNEW ERTEX DISP DEFLIST
                                 BUILDWORDLIST BCWL PROPNAME DP
                                 ETAOIN NTH LIS2FY

CATCH
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SUBLEAF SHRDLU

CDAAR
           CALLED BY USER FUNCTIONS-
                                 ANSNAME

CDADR
           CALLED BY USER FUNCTIONS-
                                 MOVE-PTW MOVE-PT

CDAR
           CALLED BY USER FUNCTIONS-
                                 MUNG

CDDDDR
           CALLED BY USER FUNCTIONS-
                                 MUNG PLNR-MUNG

CDDDR
           CALLED BY USER FUNCTIONS-
                                 MUNG : BUILDWORDLIST ETAOIN

CDDR
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL*** UNLABELTRACE
                                 LABELTRACE LOCG2 SETQQCHECK
                                 PLNR-PROGIFY PLNR-FINDIFY
                                 PLNR-THCONSIFY MAPC2 NEWCOPY :
                                 ONE-WORD-LEFT DEFS DP ETAOIN

CDR
           CALLED BY USER FUNCTIONS-
                                 DEFINETHEOREM ***TOPLEVEL*** CHARG
                                 SHOWPROP QUERY UNLABELTRACE
                                 LABELTRACE TFIND PACKORD PACKON
                                 ORDER OCCUPIER GROW ENDTIME CLEAR
                                 PRTPUT PARAP NAMEVENT FINDREDUCE
                                 FINDCHOOSE PLURALIZE ONECHECK
                                 NAMEOBJ LISTNAMES FINDMOTHER ELIZA
                                 DESCRIBEVENT CUTOFF ANSWER ANSREL
                                 ANSORDER PARSE-ASSOC ANSELIMINATE
                                 SMBIND SMONE3 SMONE2 SMONE SMNG3
                                 SMNG1 SMIT SMCONJ2 CHECKAMARKER
                                 CHECK SETQQCHECK EXPAND
                                 PLNR-DESCRIBE COMPARE-BUILD
                                 PLNR-PROGIFY PLNR-MUNG PLNR-FINDIFY
                                 MAPBLAND ITERATEX DOBACKREF NEWCOPY
                                 IMPERF? ISTENSE ATOMIFY : PDEFINE
                                 UPREL SECONDWORD? PREVIOUS POPTO POP
                                 PARSEREL PARSE3 ONLY-ONE-WORD-LEFT
                                 FOLLOWING FLUSHME CUT MOVE-PTW
                                 MOVE-PT ONE-WORD-LEFT DEFS UNION STA
                                 SETDIF PRINTC MEET FROM FINDB ETNEW
                                 DISP DEFLIST COMBINATION? BCWL DP
                                 ETAOIN NTH

COND
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW HELP DEFINE
                                 DEFINETHEOREM ONOFF CHARG SHOWPROP
                                 REQUEST QUERY SUBL2 SHOWTELL
                                 SHOWSCENE ***TOPLEVEL***
                                 UNLABELTRACE PASSING LABELTRACE
                                 TIMECHK TFIND SUPPORT SIZE PACKORD
                                 PACKON ORDER OCCUPIER LOCG2 LISTIFY
                                 GROW FINDSPACE ENDTIME CLEAR ABSVAL
                                 VBFIX PRTPUT PARAP NAMEVENT MUNG
                                 IASS FINDREDUCE FINDNUM FINDCHOOSE
                                 TOPLEVEL THVAL-MULT PLURALIZE
                                 PREPPUT PLNR-ANDORIFY ORDNAME
                                 ONECHECK NAMESUGAR NAMESIZE NAMEOBJ
                                 LISTNAMES FINDMOTHER ELIZA
                                 DESCRIBEVENT ANSWER ANSTHMELEMENT
                                 ANSTHMADD ANSTHM ANSREL ANSQUEST
                                 ANSORDER ANSNOREL ANSGEN PARSE-ASSOC
                                 ANSELIMINATE ANSDECLARE ANSCOMMAND
                                 AMBPUT NAMEACTION SMBIND
                                 SMCL-MODIFIERS SMCL1 SMONE3 SMONE2
                                 SMONE SMNG3 SMNG1 SMIT2 SMIT
                                 SMADJG-PREPG SMVAUX SMPRON SMVG
                                 CHECKREL CHECKAMARKER CHECK WHO
                                 THVAL2 SETQQCHECK ERQSET EXPAND
                                 COMPARE-PROC RELFIND PLNR-DESCRIBE
                                 FINDMEASURE COMPARE-BUILD
                                 PLNR-REMTIME PLNR-NUMSUB PLNR-NUMREL
                                 PLNR-PROGIFY PLNR-NOTIFY PLNR-MUNG
                                 PLNR-GOALIFY PLNR-FINDSPEC
                                 PLNR-FINDIFY PLNR-THCONSIFY
                                 PLNR-JUNKIFY2 PLNR-JUNKIFY MUMBLE
                                 MAPC2 MAPBLAND ITERATEX EVALCHECK
                                 DOBACKREF NEWCOPY ISTENSE ATOMIFY
                                 CANPARSE CANTAKE BOTH COMMA CONJ
                                 GOCOND GOCHECK : INTERPRET PTFIND
                                 PREVIOUS POPTO POP PARSEREL PARSE3
                                 PARSE2 PARSE F CUT APPLY-GRAMMAR
                                 MOVE-PTW MOVE-PT SPACE TAB DEFS
                                 UNION STA SETDIF PRINTC PRINTEXT
                                 PRINT2 PR1 MEET MAKESYM FROM FINDB
                                 ERTEX DISP DP DTABLE ETAOIN
                                 INTEROGATE SHRDLU NTH LIS2FY

CONS
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL*** SHOWPROP QUERY
                                 LABELTRACE PACKORD ORDER MEMORY
                                 LOCG2 GROW PRTPUT PARAP MUNG
                                 PLURALIZE PREPPUT ONECHECK NAMESUGAR
                                 NAMEOBJ LISTNAMES DESCRIBEVENT
                                 ANSTHMADD ANSREL ANSELIMINATE
                                 ANSDECLARE ANSAY SAYIFY PRON-PRT
                                 NAMEACTION SMNG3 SMNG2 SMIT2 SMCONJ2
                                 CHECKAMARKER SETQQCHECK EXPAND
                                 PLNR-DESCRIBE PLNR-PROGIFY
                                 PLNR-NEWBODY PLNR-MUNG MAPBLAND
                                 ITERATEX POP PARSE3 MQ M FQ F
                                 MOVE-PT ADD-F-PT UNION SETDIF PR2
                                 MEET FROM ERTEX BUG COMBINATION?
                                 BCWL ETAOIN

CURSORPOS
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF

DEFPROP
           CALLED BY USER FUNCTIONS-
                                 STARTHISTORY

DELETE
           CALLED BY USER FUNCTIONS-
                                 ANSDECLARE

DELQ
           CALLED BY USER FUNCTIONS-
                                 LISTNAMES PLNR-REMTIME

DIFFERENCE
           CALLED BY USER FUNCTIONS-
                                 GROW FINDSPACE DIFF FINDCHOOSE
                                 ANSNOREL ANSCOMMAND TAB

DO
           CALLED BY USER FUNCTIONS-
                                 CLEANX
EQ
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW HELP DEFINE ONOFF
                                 REQUEST QUERY SUBL2 ***TOPLEVEL***
                                 PEV SUPPORT SIZE PACKORD LOCG2
                                 FINDSPACE VBFIX PARAP NAMEVENT MUNG
                                 IASS FINDNUM FINDCHOOSE TOPLEVEL
                                 ONECHECK NAMEOBJ FINDMOTHER ELIZA
                                 DESCRIBEVENT ANSTHMELEMENT ANSREL
                                 ANSNOREL PARSE-ASSOC ANSCOMMAND
                                 NAMEACTION SMONE3 SMNG3 SMIT2 WHO
                                 THVAL2 SETQQCHECK EXPAND
                                 PLNR-DESCRIBE PLNR-NUMSUB
                                 PLNR-PROGIFY PLNR-NOTIFY PLNR-MUNG
                                 PLNR-FINDSPEC PLNR-FINDIFY
                                 PLNR-THCONSIFY PLNR-JUNKIFY2
                                 PLNR-JUNKIFY MUMBLE EVALCHECK
                                 NEWCOPY IMPERF? ISTENSE CANTAKE
                                 INTERPRET SECONDWORD? PTFIND
                                 PREVIOUS POPTO POP PARSE3 PARSE2
                                 NEXTWORD? NEXTW FLUSHME CUT MOVE-PTW
                                 MOVE-PT REMOVE-F-PT ADD-F-PT DP STA
                                 PRINTC FROM FINDB ETNEW ERTEX
                                 PROPNAME ETAOIN INTEROGATE

EQUAL
           CALLED BY USER FUNCTIONS-
                                 REQUEST THVAL-MULT ORDNAME ONECHECK
                                 NAMESUGAR ANSGEN ANSELIMINATE SMCL1
                                 SMVG ISTENSE CANPARSE CANTAKE CLEANX
                                 ETAOIN NTH

ERR
           CALLED BY USER FUNCTIONS-
                                 REQUEST QUERY GROW ANSGEN RELFIND
                                 POP

ERRSET
           CALLED BY USER FUNCTIONS-
                                 SW STARTHISTORY GROW ANSGEN RELFIND
                                 POP ERTEX ETAOIN SHRDLU
EVAL
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW LOCG2 GROW NAMEVENT
                                 SAYIFY NAMELIST-EVALED SMBIND
                                 SMCL-MODIFIERS SMCL1 SMONE2 SMNG1
                                 SMADJG-PREPG SMADJ SMPRON SETQQCHECK
                                 EXPAND PLNR-RECOMMENDIFY PLNR-NUMSUB
                                 ITERATEX ITERATE EVALCHECK BUILD :
                                 PTFIND POPTO POP PARSEREL PARSE3
                                 PARSE2 ISQ APPLY-GRAMMAR MOVE-PTW
                                 MOVE-PT CALLSM PRINTC PRINTEXT ERTEX
                                 DISP COMBINATION? INTEROGATE
                                 PARSEVAL

EXPLODE
           CALLED BY USER FUNCTIONS-
                                 QUERY ABBREVIATE VBFIX PLURALMAKE
                                 ORDNAME NAMEOBJ CUTOFF MAKESYM
                                 COMBINATION? BCWL PROPNAME

FLATSIZE
           CALLED BY USER FUNCTIONS-
                                 PRINT3 PRINT2

FUNCTION
           CALLED BY USER FUNCTIONS-
                                 PACKO GROW PARAP NAMEVENT IASS SMNG2
                                 SMNGOF SMIT2 SMIT PLNR-NUMSUB PARSE3
                                 FQ PR2 DEFLIST CLEANOUT ETAOIN

GENSYM
           CALLED BY USER FUNCTIONS-
                                 ITERATEX

GET
           CALLED BY USER FUNCTIONS-
                                 DEBUGMODE TREEPRINT SUBL2 SUBLEAF
                                 SHOWTELL ***TOPLEVEL*** PEV
                                 UNLABELTRACE LABELTRACE TIMECHK
                                 TFIND STARTHISTORY VBFIX PARAP
                                 NAMEVENT FINDCHOOSE TOPLEVEL
                                 PLURALMAKE ONECHECK NAMEOBJ
                                 DESCRIBEVENT ANSTHM NAMEACTION
                                 SMBIND SMCL-MODIFIERS SMONE3 SMONE
                                 SMIT VARIABLE? TSS? TENSE? SYSTEMS?
                                 START? RSSVAR? RSS? RELMARKERS?
                                 RELATIONS? REL? REFER? QUANTIFIER?
                                 QTYPE? PLNRCODE? PLAUSIBILITY?
                                 PARSENODE? OSS? OR? NUMBER?
                                 NEGATIVE? MODIFIERS? MARKERS? END?
                                 DETERMINER? ANSRSS? AND?
                                 AMBIGUITIES? ACTION? CHECKAMARKER
                                 WHO PLNR-DESCRIBE FINDMEASURE
                                 PLNR-RECOMMENDIFY PLNR-GOALIFY
                                 MUMBLE DOBACKREF INTERPRET ROOT
                                 PARSE3 PARSE2 GETR APPLY-GRAMMAR
                                 REMOVE-F-PT PR2 MAKESYM ERTEX DISP
                                 CLEANX CLEANUP CLEANOUT DTABLE
                                 ETAOIN DA

GO
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW DEFINE REQUEST QUERY
                                 UNLABELTRACE LABELTRACE TFIND PACKON
                                 OCCUPIER GROW FINDSPACE ENDTIME
                                 CLEAR FINDREDUCE FINDCHOOSE ONECHECK
                                 LISTNAMES ANSWER ANSORDER
                                 PARSE-ASSOC ANSELIMINATE SMCL1
                                 SMONE3 SMONE2 SMONE SMNG3 SMNG1 SMIT
                                 CHECKAMARKER CHECK SETQQCHECK
                                 PLNR-DESCRIBE MAPC2 MAPBLAND NEWCOPY
                                 GOCOND GOCHECK INTERPRET PTFIND
                                 PREVIOUS POPTO PARSEREL PARSE3
                                 PARSE2 CUT MOVE-PTW MOVE-PT SPACE
                                 TAB DEFS UNION STA SETDIF PRINTC
                                 MEET ERTEX DP ETAOIN INTEROGATE
                                 SHRDLU NTH

GREATERP
           CALLED BY USER FUNCTIONS-
                                 TFIND PACKORD PACKON ORDER GROW
                                 ENDTIME CLEAR FINDCHOOSE PLURALIZE
                                 NAMESIZE ENOUGH-BETTER ANSREL
                                 ANSELIMINATE SMNG3 MUMBLE SPACE TAB
                                 PRINT3 PRINT2 CLEANX

INTERN
           CALLED BY USER FUNCTIONS-
                                 MAKESYM COMBINATION? BCWL

LAST
           CALLED BY USER FUNCTIONS-
                                 ETNEW ETAOIN

LENGTH
           CALLED BY USER FUNCTIONS-
                                 FINDCHOOSE ELIZA ANSREL ANSELIMINATE
                                 SMNG3 BOTH

LESSP
           CALLED BY USER FUNCTIONS-
                                 TIMECHK STARTIME OCCUPIER LOCGREATER
                                 GROW FINDSPACE CLEAR ELIZA ANSREL
                                 ANSORDER SMNG3 WHO MUMBLE BOTH NTH

LIST
           CALLED BY USER FUNCTIONS-
                                 DEFINE DEFINETHEOREM SHOWTELLCHOICE
                                 SHOWTELL SHOWSCENE ***TOPLEVEL***
                                 LABELTRACE TCENT STARTHISTORY
                                 PACKORD PACKO ORDER MEMOREND LISTIFY
                                 GROW GOAL FINDSPACE VBFIX2 PRTPUT
                                 NAMEVENT MUNG FINDNUM FINDCHOOSE
                                 THVAL-MULT NAMESUGAR NAMELIST
                                 NAMEOBJ LISTNAMES ELIZA ANSTHMADD
                                 ANSTHM ANSREL ANSNOREL ANSELIMINATE
                                 ANSDECLARE ANSCOMMAND ANSAY PRON-PRT
                                 NAMELIST-EVALED SMPOSS2 SMONE SMNG3
                                 SMNG1 SMPROP SMNEWPROPN FINDEVENTS
                                 CHECK SETQQCHECK EXPAND COMPARE-PROC
                                 ORDMAKE MEASURE COMPARE-BUILD
                                 PLNR-VAR PLNR-NOTIFY PLNR-MUNG
                                 PLNR-GOALIFY PLNR-FINDSPEC
                                 PLNR-FINDIFY PLNR-THCONSIFY
                                 PLNR-JUNKIFY2 PLNR-JUNKIFY ITERATEX
                                 CANPARSE CANTAKE PDEFINE BUILDNODE
                                 REMOVE-F-PT WALLP PR2 PR1
                                 COMBINATION? BCWL DTABLE ETAOIN
                                 LIS2FY QUOTIFY

LISTIFY
      THIS FUNCTION WAS A SYSTEM FUNCTION TILL YOU REDEFINED IT
           CALLED BY USER FUNCTIONS-
                                 DEFINE DEFINETHEOREM SHOWMOVE
                                 SHOWPROP GROW FINDSPACE CLEAR
                                 NAMEVENT

MAKNAM
           CALLED BY USER FUNCTIONS-
                                 PLURALMAKE MAKESYM COMBINATION? BCWL

MAP
           CALLED BY USER FUNCTIONS-
                                 UNLABELTRACE LABELTRACE SMCL2 SMCL1
                                 SMIT RELFIND DOBACKREF POP

MAPC
           CALLED BY USER FUNCTIONS-
                                 SW NOPAUSES QUIETMODE TREEPRINT
                                 QUERY SUBL2 SUBLEAF SHOWSCENE
                                 ***TOPLEVEL*** PEV UNLABELTRACE
                                 LABELTRACE STARTHISTORY PACKO GROW
                                 NAMEVENT IASS ANSELIMINATE SMIT
                                 PLNR-PROGIFY DOBACKREF BUILD SAY PR2
                                 ERTEX DEFLIST COMBINATION? CLEANUP
                                 CLEANOUT BUILDWORDLIST BCWL % DTABLE
                                 ETAOIN INTEROGATE

MAPCAN
           CALLED BY USER FUNCTIONS-
                                 DESCRIBEVENT ANSQUEST ANSDECLARE
                                 CHECKREL PLNR-JUNKIFY2 PLNR-JUNKIFY

MAPCAR
           CALLED BY USER FUNCTIONS-
                                 QUERY ABBREVIATE PACKO DIFF CLEAR
                                 PLNR-ANDORIFY LISTNAMES DESCRIBEVENT
                                 ANSWER ANSTHM ANSQUEST ANSDECLARE
                                 SAYIFY SMRELATE SMIT2 SMADJG-PREPG
                                 SMCONJ2 EXPAND PLNR-REMTIME
                                 PLNR-NUMSUB PLNR-JUNKIFY2
                                 PLNR-JUNKIFY FQ

MAPLIST
           CALLED BY USER FUNCTIONS-
                                 FINDMOTHER ELIZA PR1

MAX
           CALLED BY USER FUNCTIONS-
                                 FINDSPACE

MEMBER
           CALLED BY USER FUNCTIONS-
                                 REQUEST QUERY ANSELIMINATE SMVG
                                 PLNR-REMTIME ISTENSE ISX F ERTEX

MEMQ
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL*** PASSING GROW
                                 FINDSPACE CLEAR VBFIX2 PARAP IASS
                                 PLURALIZE NAMESUGAR NAMEOBJ ANSREL
                                 SMRELATE SMNG3 SMIT2 CHECKREL
                                 CHECKAMARKER SETQQCHECK EXPAND
                                 MEASURE PLNR-NUMSUB PLNR-NUMREL
                                 PLNR-FINDSPEC CANPARSE CANTAKE BOTH
                                 UPREL PARSE3 PARSE2 PARSE NQ ISQ FQ
                                 FOLLOWING FEATURE? CQ UNION SETDIF
                                 PR2 MEET ERTEX DP ETAOIN

MIN
           CALLED BY USER FUNCTIONS-
                                 GROW FINDSPACE

MINUS
           CALLED BY USER FUNCTIONS-
                                 ABSVAL TIMER

MINUSP
           CALLED BY USER FUNCTIONS-
                                 OCCUPIER GROW ABSVAL

NCONC
           CALLED BY USER FUNCTIONS-
                                 DEFINE DEFINETHEOREM PLURALMAKE
                                 ORDNAME NAMEOBJ SMIT2 PLNR-FINDIFY
                                 PLNR-THCONSIFY MAPBLAND ITERATEX
                                 PDEFINE COMBINATION? BCWL

NOT
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF QUERY UNLABELTRACE
                                 TIMECHK TFIND LOCGREATER GROW
                                 FINDSPACE ENDTIME ONECHECK NAMEOBJ
                                 ELIZA DISPUT ANSWER ANSTHMELEMENT
                                 ANSTHM ANSQUEST ANSNOREL
                                 ANSELIMINATE ANSDECLARE SMCL1 SMONE3
                                 SMNG3 SMNG2 SMIT2 SMIT WHO EXPAND
                                 RELFIND PLNR-REMTIME PLNR-NOTIFY
                                 MUMBLE BUILD CANTAKE UPCHECK UPREL
                                 PTFIND POP PARSEREL PARSE3 PARSE2
                                 ONLY-ONE-WORD-LEFT FLUSHME CUT
                                 MOVE-PTW MOVE-PT ONE-WORD-LEFT TAB
                                 FROM ERTEX ETAOIN SHRDLU

NULL
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL*** SHOWPROP SHOWTELL
                                 PACKORD PACKON ORDER OCCUPIER GROW
                                 ENDTIME CLEAR PARAP ONECHECK NAMEOBJ
                                 LISTNAMES DESCRIBEVENT ANSREL
                                 ANSORDER ANSNOREL PARSE-ASSOC
                                 SMCL-MODIFIERS SMONE3 SMONE2 SMNG3
                                 SMNG1 SMIT SMPRON CHECKAMARKER CHECK
                                 WHO SETQQCHECK EXPAND PLNR-DESCRIBE
                                 MUMBLE MAPC2 MAPBLAND ITERATEX
                                 NEWCOPY CANPARSE CANTAKE GOCHECK :
                                 PREVIOUS POP PARSEREL PARSE3 PARSE2
                                 CUT MOVE-PTW MOVE-PT DEFS UNION STA
                                 SETDIF PRINTC MEET FINDB ETAOIN

NUMBERP
           CALLED BY USER FUNCTIONS-
                                 NAMEVENT FINDNUM NAMESIZE ANSREL
                                 ANSELIMINATE SMNG3 PLNR-FINDSPEC
                                 ETAOIN

OR
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF DEBUGMODE DEFINE
                                 SHOWMOVE ***TOPLEVEL*** REQUEST
                                 QUERY TIMECHK TFIND STARTIME PACKORD
                                 PACKON MEMORY MEMOREND GROW
                                 FINDSPACE EV ENDTIME ATAB VBFIX
                                 PARAP NAMEVENT IASS FINDCHOOSE
                                 THVAL-MULT PLURALMAKE ONECHECK
                                 NAMESIZE NAMENUM NAMEOBJ DISPUT
                                 DESCRIBEVENT ANSTHM ANSREL ANSQUEST
                                 ANSNAME ANSGEN ANSELIMINATE SMBIND
                                 SMCL-MODIFIERS SMCL1 SMRELATE SMONE3
                                 SMONE2 SMONE SMNG3 SMNG1 SMIT
                                 SMADJG-PREPG SMVAUX SMCONJ2
                                 PLAUSIBILITY? EXPAND RELFIND
                                 PLNR-PROGIFY PLNR-JUNKIFY2
                                 PLNR-JUNKIFY OBJECT MUMBLE ITERATEX
                                 DOBACKREF BUILD ISTENSE CANPARSE
                                 INTERPRET UPREL ROOT PTFIND POP
                                 PARSE3 PARSE2 FQ MOVE-PT CALLSM
                                 PRINT3 PR2 MAKESYM FROM ERTEX CLEANX
                                 BUILDWORD ETAOIN SHRDLU

PLUS
           CALLED BY USER FUNCTIONS-
                                 TREEPRINT PEV TCENT SUPPORT PACKON
                                 OCCUPIER LOCGREATER GROW FINDSPACE
                                 CLEAR NAMESIZE ENOUGH-BETTER ANSREL
                                 ANSQUEST ANSNOREL ANSDECLARE ERTEX

PRINC
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW TREEPRINT QUERY
                                 SHOWSCENE PEV UNLABELTRACE PASSING
                                 LABELTRACE IASS ANSWER ANSELIMINATE
                                 SPACE TAB UNDEFINED PRINTC PRINT3
                                 PRINT2 ERTEX DISP % DP DTABLE ETAOIN
                                 TIMER

PRINT
           CALLED BY USER FUNCTIONS-
                                 DEFINETHEOREM QUERY SHOWSCENE
                                 UNLABELTRACE PASSING LABELTRACE
                                 ANSWER ANSELIMINATE SETQQCHECK
                                 DOBACKREF PARSE2 MOVE-PT ERTEX
                                 BUILDWORDLIST DTABLE ETAOIN TIMER
                                 SHRDLU

PROG
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW SWITCHES DEFINE
                                 ***TOPLEVEL*** SHOWPROP REQUEST
                                 QUERY SHOWSCENE UNLABELTRACE
                                 LABELTRACE TFIND PACKON PACKO
                                 OCCUPIER GROW FINDSPACE ENDTIME
                                 CLEAR PARAP NAMEVENT IASS FINDREDUCE
                                 FINDCHOOSE THVAL-MULT PLURALMAKE
                                 ONECHECK NAMESUGAR NAMEOBJ LISTNAMES
                                 ELIZA DESCRIBEVENT ANSWER ANSTHM
                                 ANSREL ANSQUEST ANSORDER ANSNOREL
                                 ANSNAME ANSGEN PARSE-ASSOC
                                 ANSELIMINATE ANSDECLARE ANSCOMMAND
                                 NAMELIST-EVALED NAMEACTION SMBIND
                                 SMCL1 SMRELATE SMPOSS2 SMPOSS SMONE3
                                 SMONE2 SMONE SMNG3 SMNG1 SMIT
                                 SMADJG-PREPG SMVG SMCONJ2 SMCONJ
                                 CHECKAMARKER CHECK THVAL2 SETQQCHECK
                                 EXPAND COMPARE-PROC ORDMAKE RELFIND
                                 PLNR-DESCRIBE COMPARE-BUILD
                                 PLNR-RECOMMENDIFY PLNR-PROGIFY
                                 PLNR-THCONSIFY OBJECT MAPC2 MAPBLAND
                                 ITERATE DOBACKREF NEWCOPY BUILD
                                 ISTENSE CANPARSE CANTAKE BOTH CONJ
                                 INTERPRET PTFIND PREVIOUS POPTO POP
                                 PARSEREL PARSE3 PARSE2 CUT BUILDNODE
                                 MOVE-PTW MOVE-PT SETMVB SPACE TAB
                                 DEFS WALLP UNION STA SETDIF PRINTC
                                 MEET ERTEX COMBINATION? CLEANX DP
                                 ETAOIN INTEROGATE SHRDLU NTH

PROG2
           CALLED BY USER FUNCTIONS-
                                 UNDEFINED PRINT3

PUTPROP
           CALLED BY USER FUNCTIONS-
                                 DEFINE ***TOPLEVEL*** DEFINETHEOREM
                                 ABBREVIATE UNLABELTRACE LABELTRACE
                                 STARTHISTORY MEMORY MEMOREND
                                 FINDCHOOSE DISPUT ANSREL ANSNOREL
                                 ANSNAME ANSCOMMAND SMBINDER SMPOSS2
                                 SMNG3 SMIT SMVAUX SMVG FINDEVENTS
                                 EXPAND COMPARE-BUILD PLNR-THCONSIFY
                                 VALUEPUT DOBACKREF NEWCOPY BUILD
                                 PDEFINE SETR REMOVE-F-PT ADD-F-PT
                                 DEFS MAKESYM DEFLIST CLEANX CLEANUP
                                 CLEANOUT BUILDWORD

QUOTE
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW DEBUGMODE NOPAUSES
                                 QUIETMODE HELP DEFINE DEFINETHEOREM
                                 ONOFF SHOWMOVE SHOW TREEPRINT TELL
                                 SHOWPROP REQUEST QUERY SHOWTELL
                                 SHOWSCENE ABBREVIATE PEV
                                 UNLABELTRACE PASSING LABELTRACE
                                 TIMECHK TFIND SUPPORT STARTHISTORY
                                 SIZE PACKO OCCUPIER MEMORY MEMOREND
                                 LOCG2 LOCGREATER GROW GOAL FINDSPACE
                                 CLEAR VBFIX PARAP NAMEVENT MUNG
                                 FINDNUM FINDCHOOSE TOPLEVEL
                                 THVAL-MULT PLURALMAKE PLURALIZE
                                 PREPPUT ORDNAME ONECHECK NAMESUGAR
                                 NAMESIZE NAMENUM NAMELIST NAMEOBJ
                                 LISTNAMES FINDMOTHER ELIZA DISPUT
                                 DESCRIBEVENT ANSWER ANSTHMELEMENT
                                 ANSTHM ANSREL ANSQUEST ANSNOREL
                                 ANSNAME ANSGEN ANSELIMINATE
                                 ANSDECLARE ANSCOMMAND ANSAY SAYIFY
                                 PRON-PRT NAMELIST-EVALED NAMEACTION
                                 SMBINDER SMBIND SMCL-MODIFIERS SMCL1
                                 SMRELATE SMPOSS2 SMONE3 SMONE SMNG3
                                 SMNG2 SMNG1 SMIT2 SMIT SMADJG-PREPG
                                 SMVAUX SMPRON SMVG SMCONJ2
                                 SMNEWPROPN SMSET VARIABLE? TSS?
                                 TENSE? SYSTEMS? START? RSSVAR? RSS?
                                 RELMARKERS? RELATIONS? REL? REFER?
                                 QUANTIFIER? QTYPE? PLNRCODE?
                                 PLAUSIBILITY? PARSENODE? PARENT?
                                 OSS? OR? NUMBER? NEGATIVE?
                                 MODIFIERS? MARKERS? END? DETERMINER?
                                 ANSRSS? AND? AMBIGUITIES? ACTION?
                                 CHECKREL FINDEVENTS CHECKAMARKER WHO
                                 THVAL2 SETQQCHECK EXPAND
                                 COMPARE-PROC ORDMAKE RELFIND
                                 PLNR-DESCRIBE MEASURE FINDMEASURE
                                 COMPARE-BUILD PLNR-VAR PLNR-REMTIME
                                 PLNR-RECOMMENDIFY PLNR-NUMSUB
                                 PLNR-PROGIFY PLNR-NOTIFY PLNR-MUNG
                                 PLNR-GOALIFY PLNR-FINDSPEC
                                 PLNR-FINDIFY PLNR-THCONSIFY VALUEPUT
                                 PLNR-JUNKIFY2 PLNR-JUNKIFY OBJECT
                                 MUMBLE MAPBLAND ITERATEX EVALCHECK
                                 DOBACKREF NEWCOPY BUILD IMPERF
                                 IMPERF? ISTENSE CANPARSE CANTAKE
                                 DOUBLEQUOTER BOTH COMMA CONJ
                                 INTERPRET PDEFINE UPCHECK UPREL
                                 TRNSF SM RQ ROOT REBUILD POP PARSE3
                                 PARSE2 PARSE PARENT NB N H FQ FESET
                                 FE F BUILDNODE APPLY-GRAMMAR
                                 MOVE-PTW MOVE-PT REMOVE-F-PT
                                 ADD-F-PT SETMVB SPACE TAB PRINTC
                                 PRINT3 PRINT2 PR2 PR1 MAKESYM ETNEW
                                 ERTEX BUG DISP COMBINATION? CLEANX
                                 CLEANUP CLEANOUT BUILDWORDLIST
                                 BUILDWORD BCWL PROPNAME DP DTABLE
                                 ETAOIN INTEROGATE TIMER SHRDLU
                                 ***TOPLEVEL*** DA QUOTIFY

QUOTIENT
           CALLED BY USER FUNCTIONS-
                                 DIV2 TIME-SINCE

RANDOM
           CALLED BY USER FUNCTIONS-
                                 FINDSPACE

READ
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF REQUEST ANSELIMINATE
                                 ERTEX

READCH
           CALLED BY USER FUNCTIONS-
                                 SW REQUEST QUERY ERTEX ETAOIN
                                 INTEROGATE

READLIST
           CALLED BY USER FUNCTIONS-
                                 ABBREVIATE VBFIX ORDNAME NAMEOBJ
                                 CUTOFF CANTAKE ETNEW ETAOIN

REMAINDER
           CALLED BY USER FUNCTIONS-
                                 FINDSPACE

REMOB
           CALLED BY USER FUNCTIONS-
                                 CLEANX

REMPROP
           CALLED BY USER FUNCTIONS-
                                 SMIT DOBACKREF

RETURN
           CALLED BY USER FUNCTIONS-
                                 SW DEFINE REQUEST QUERY TFIND PACKON
                                 PACKO OCCUPIER GROW FINDSPACE
                                 ENDTIME CLEAR PARAP NAMEVENT IASS
                                 FINDREDUCE FINDCHOOSE THVAL-MULT
                                 PLURALMAKE ONECHECK NAMESUGAR
                                 NAMEOBJ LISTNAMES ELIZA DESCRIBEVENT
                                 ANSWER ANSTHM ANSREL ANSQUEST
                                 ANSORDER ANSNOREL ANSNAME ANSGEN
                                 PARSE-ASSOC ANSELIMINATE ANSDECLARE
                                 ANSCOMMAND NAMELIST-EVALED
                                 NAMEACTION SMBIND SMCL1 SMRELATE
                                 SMPOSS2 SMPOSS SMONE3 SMONE2 SMONE
                                 SMNG3 SMNG1 SMIT SMADJG-PREPG SMVG
                                 SMCONJ2 SMCONJ CHECKAMARKER CHECK
                                 THVAL2 SETQQCHECK EXPAND
                                 COMPARE-PROC ORDMAKE RELFIND
                                 PLNR-DESCRIBE COMPARE-BUILD
                                 PLNR-RECOMMENDIFY PLNR-PROGIFY
                                 PLNR-THCONSIFY OBJECT MAPC2 MAPBLAND
                                 ITERATE NEWCOPY BUILD ISTENSE
                                 CANPARSE CANTAKE BOTH CONJ INTERPRET
                                 PTFIND PREVIOUS POPTO PARSEREL
                                 PARSE3 PARSE2 CUT BUILDNODE MOVE-PTW
                                 MOVE-PT SETMVB TAB DEFS UNION STA
                                 SETDIF PRINTC MEET ERTEX
                                 COMBINATION? CLEANX DP ETAOIN
                                 INTEROGATE NTH

REVERSE
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL*** PEV GOAL VBFIX
                                 FINDCHOOSE ONECHECK ANSCOMMAND
                                 SMONE3 RELFIND PLNR-PROGIFY
                                 INTERPRET WALLP UNION SETDIF PR1
                                 MEET ETNEW ERTEX ETAOIN

RPLACA
           CALLED BY USER FUNCTIONS-
                                 GROW ANSORDER

RPLACD
           CALLED BY USER FUNCTIONS-
                                 UNLABELTRACE LABELTRACE MUNG

RUNTIME
           CALLED BY USER FUNCTIONS-
                                 ERTEX TIME-SINCE SHRDLU

SASSOC
           CALLED BY USER FUNCTIONS-
                                 PARSE3

SASSQ
           CALLED BY USER FUNCTIONS-
                                 IASS
SET
           CALLED BY USER FUNCTIONS-
                                 NOPAUSES QUIETMODE GROW NAMEVENT
                                 SETQQCHECK

SETQ
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW SWITCHES DEBUGMODE
                                 USERMODE NORMALFEATUREMODE DEFINE
                                 DEFINETHEOREM SHOWMOVE REQUEST QUERY
                                 SHOWCHOICE TELLCHOICE SHOWSCENE
                                 ***TOPLEVEL*** UNLABELTRACE PASSING
                                 LABELTRACE LBK TFIND SUPPORT
                                 STARTHISTORY PACKON PACKO OCCUPIER
                                 GROW GOAL FINDSPACE ENDTIME CLEAR
                                 VBFIX PARAP NAMEVENT MUNG IASS
                                 FINDREDUCE FINDCHOOSE THVAL-MULT
                                 PLURALMAKE PREPPUT ONECHECK
                                 NAMESUGAR NAMESIZE NAMEOBJ LISTNAMES
                                 HEADPART ELIZA DESCRIBEVENT ANSWER
                                 ANSTHMELEMENT ANSTHMADD ANSTHM
                                 ANSREL ANSQUEST ANSORDER ANSNOREL
                                 ANSNAME ANSGEN PARSE-ASSOC
                                 ANSELIMINATE ANSDECLARE ANSCOMMAND
                                 NAMELIST-EVALED NAMEACTION SMBIND
                                 SMCL1 SMRELATE SMPOSS2 SMPOSS SMONE3
                                 SMONE2 SMONE SMNG3 SMNG2 SMNG1 SMIT2
                                 SMADJG-PREPG SMPRON SMVG SMCONJ2
                                 SMSET CHECKAMARKER CHECK WHO THVAL2
                                 SETQQCHECK ERQSET EXPAND ORDMAKE
                                 RELFIND PLNR-DESCRIBE FINDMEASURE
                                 PLNR-RECOMMENDIFY PLNR-NUMREL
                                 PLNR-PROGIFY PLNR-NEWBODY
                                 PLNR-GOALIFY PLNR-FINDIFY
                                 PLNR-THCONSIFY OBJECT MUMBLE MAPC2
                                 MAPBLAND ITERATEX DOBACKREF NEWCOPY
                                 BUILD IMPERF? ISTENSE CANPARSE
                                 CANTAKE BOTH CONJ INTERPRET TRNSF RQ
                                 PTFIND PREVIOUS POPTO POP PARSEREL
                                 PARSE3 PARSE2 MQ MP M FQ FLUSHME F
                                 CUT BUILDNODE MOVE-PTW MOVE-PT
                                 REMOVE-F-PT ADD-F-PT SETMVB
                                 RESTOREPT SPACE TAB DEFS WALLP UNION
                                 STA SETDIF PRINTC PR2 MEET MAKESYM
                                 ETNEW ERTEX GLOBAL-ERR COMBINATION?
                                 CLEANX DP ETAOIN INTEROGATE SHRDLU
                                 NTH

SSTATUS
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL***

STATUS
           CALLED BY USER FUNCTIONS-
                                 WALLP DISP

SUB1
           CALLED BY USER FUNCTIONS-
                                 SUPPORT OCCUPIER FINDSPACE
                                 FINDREDUCE SMNG3 SPACE ETAOIN NTH

SUBLIS
           CALLED BY USER FUNCTIONS-
                                 PARAP

SUBST
           CALLED BY USER FUNCTIONS-
                                 FINDCHOOSE PLURALMAKE PLURALIZE
                                 EXPAND ORDMAKE PLNR-DESCRIBE

TERPRI
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF SW TREEPRINT SHOWSCENE
                                 PEV ANSWER ANSELIMINATE WALLP PRINTC
                                 PRINTEXT PRINT3 PRINT2 ERTEX DISP %
                                 DP ETAOIN INTEROGATE

THROW
           CALLED BY USER FUNCTIONS-
                                 SUBL2 ERTEX

TRACE
           CALLED BY USER FUNCTIONS-
                                 ***TOPLEVEL***

TYI
           CALLED BY USER FUNCTIONS-
                                 ETAOIN

TYIPEEK
           CALLED BY USER FUNCTIONS-
                                 REQUEST ERTEX ETAOIN

TYO
           CALLED BY USER FUNCTIONS-
                                 QUERY WALLP DISP ETAOIN

UREAD
           CALLED BY USER FUNCTIONS-
                                 INITIALSTUFF HELP

ZEROP
           CALLED BY USER FUNCTIONS-
                                 FINDSPACE FINDREDUCE NAMESUGAR
                                 ANSREL ETAOIN

(*DIF - = ADD1 AND APPEND APPLY ASCII ASSOC ASSQ ATOM CAAAR CAADDR CAA
DR CAAR CADAAR CADADR CADAR CADDAR CADDDR CADDR CADR CAR CATCH CDAAR C
DADR CDAR CDDDDR CDDDR CDDR CDR COND CONS CURSORPOS DEFPROP DELETE DEL
Q DIFFERENCE DO EQ EQUAL ERR ERRSET EVAL EXPLODE FLATSIZE FUNCTION GEN
SYM GET GO GREATERP INTERN LAST LENGTH LESSP LIST LISTIFY MAKNAM MAP M
APC MAPCAN MAPCAR MAPLIST MAX MEMBER MEMQ MIN MINUS MINUSP NCONC NOT N
ULL NUMBERP OR PLUS PRINC PRINT PROG PROG2 PUTPROP QUOTE QUOTIENT RAND
OM READ READCH READLIST REMAINDER REMOB REMPROP RETURN REVERSE RPLACA
RPLACD RUNTIME SASSOC SASSQ SET SETQ SSTATUS STATUS SUB1 SUBLIS SUBST
TERPRI THROW TRACE TYI TYIPEEK TYO UREAD ZEROP)

§ anno/winograd/mannew

     On the A.I. machine, a reasonably fluent
and debuged version of SHRDLU is alway availlable as
SHRDLU > DSK SHRDLU and can be run as a job
by typing SHRDLU<cntrl. k> at DDT. This version is
in interpreted with
all its options including a grind and trace. Consequently
it occupies arround 200k of core (caution: indiscriminately
running a job that big in the middle of the day is a

good way to make enemies!!!!! Alway check the level
of system usage befoere loading.
.sp 1
     The first sign of life from SHRDLU looks
like this:
          SHRDLU version 101   created on 4-27-73
           type <space> to go to "ready" state, type "?" for help
          >>>
.sp 1
     The version number has certain accessable
information associated with it describing the actual
files used in this version and any phenomena peculiar to it.
the prompt characters ">>>" are produced by the system's
handcrafted real-eval-print loop. When this has
been printed you are boht talking to LISP where you can do any
of the normal things (check values of bound variables, evaluate functions.
etc.) and you can use the SHOW-TELL mechanism which is designed
as a convenient way to fiddle with SHRDLU's knobs & switches,
define new wiords, examine sturctures as they are set up, etc.
More about it later.
     SHRDLU is dumped with all its display options turned
off so that the initial command state is a reasionable place to
set the parameters the way you want them before running any
sentences. All the options will be described in the next
section.
.sp 1
     Moving from this state to the one where
SHRDLU accepts sentences is doen by typing
"t " (the letter "t" followed by a space). The responce
will be:
          READY <cr>
     At this point you can type in a sentence and
it will be interpreted and responded to. Mistyped
letters within a word can bve erased
by typing a <rubout> in the usua; manner and unintended
words can be removed by typing a <rubout> agter an
intervening break character (comma or space usualy). As soon
as the sentence terminating punctuation (. ? !) has been
typed, the sentence is automaticly processed and unless
some display options have been turned on, (or you stumbled across a bug !!)
the next thing that you see will be the
responce to what you typed in from SHRDLU.A switch can be set (see the next section)
to cause a break at this point (immediately
after a response) and
permit the examination of the various structures that were added to the program's
memory for example. Without the break (or after it has been dismissed
we get another "READY" and can continue as before.
     The break-loop (otherwise known as "command state")
can be entered fron the "ready state" by typing <altmode><altmode>
instead of stsrting a sentence. Exiting from it will result
in another "READY".

§ anno/winograd/manual

                             PROVISIONAL

                        SHRDLU USERS' MANUAL

                             (Version 0)

                   S. Card, A. Rubin, T. Winograd

                          For CMU-1 Version
                            10/50 Monitor

                           August 14, 1972
                     Carnegie-Mellon University
                        Pittsburgh, Pa 15213

BRIEF DESCRIPTION OF SHRDLU
---------------------------

         SHRDLU IS A SYSTEM FOR THE  COMPUTER  UNDERSTANDING
    OF  ENGLISH.    THE  SYSTEM  ANSWERS QUESTIONS, EXECUTES
    COMMANDS, AND  ACCEPTS  INFORMATION  IN  NORMAL  ENGLISH
    DIALOG.    IT  USES  SEMANTIC INFORMATION AND CONTEXT TO
    UNDERSTAND DISCOURSE AND TO DISAMBIGUATE SENTENCES.   IT
    COMBINES  A COMPLETE SYNTACTIC ANALYSIS OF EACH SENTENCE
    WITH A "HEURISTIC  UNDERSTANDER"  WHICH  USES  DIFFERENT
    KINDS  OF  INFORMATION  ABOUT A SENTENCE, OTHER PARTS OF
    THE DISCOURSE, AND GENERAL INFORMATION ABOUT  THE  WORLD
    IN DECIDING WHAT THE SENTENCE MEANS.

         SHRDLU  IS  BASED  ON  THE  BELIEF  THAT A COMPUTER
    CANNOT DEAL  REASONABLY  WITH  LANGUAGE  UNLESS  IT  CAN
    "UNDERSTAND"  THE  SUBJECT IT IS DISCUSSING. THE PROGRAM
    IS GIVEN A DETAILED MODEL OF THE KNOWLEDGE NEEDED  BY  A
    SIMPLE  ROBOT  HAVING  ONLY A HAND AND AN EYE.  THE USER
    CAN GIVE IT  INSTRUCTIONS  TO  MANIPULATE  TOY  OBJECTS,
    INTERROGATE  IT ABOUT THE SCENE, AND GIVE IT INFORMATION
    IT WILL USE IN DEDUCTION.  IN ADDITION  TO  KNOWING  THE
    PROPERTIES  OF  TOY  OBJECTS,  THE  PROGRAM HAS A SIMPLE
    MODEL OF  ITS  OWN  MENTALITY.    IT  CAN  REMEMBER  AND
    DISCUSS ITS PLANS AND ACTIONS AS WELL AS CARRY THEM OUT.
    IT ENTERS INTO A DIALOG WITH  A  PERSON,  RESPONDING  TO
    ENGLISH  SENTENCES WITH ACTIONS AND ENGLISH REPLIES, AND
    ASKING FOR CLARIFICATION  WHEN  ITS  HEURISTIC  PROGRAMS
    CANNOT  UNDERSTAND A SENTENCE THROUGH USE OF CONTEXT AND
    PHYSICAL KNOWLEDGE.

         IN THE PROGRAMS, SYNTAX, SEMANTICS,  AND  INFERENCE
    ARE INTEGRATED IN A "VERTICAL" SYSTEM IN WHICH EACH PART
    IS CONSTANTLY COMMUNICATING WITH THE OTHERS. SHRDLU USES
    SYSTEMIC  GRAMMAR, A TYPE OF SYNTACTIC ANALYSIS WHICH IS
    DESIGNED  TO  DEAL  WITH   SEMANTICS.      RATHER   THAN
    CONCENTRATING  ON  THE EXACT FORM OF RULES FOR THE SHAPE
    OF LINGUISTIC  CONSTITUENTS,  IT  IS  STRUCTURED  AROUND
    CHOICES   FOR  CONVEYING  MEANING.    IT  ABSTRACTS  THE
    RELEVANT FEATURES OF THE LINGUISTIC STRUCTURES WHICH ARE
    IMPORTANT FOR INTERPRETING THEIR MEANING.

         IN  SHRDLU  MANY KINDS OF KNOWLEDGE ARE REPRESENTED
    IN THE FORM OF PROCEDURES RATHER THAN TABLES OF RULES OR
    LISTS  OF  PATTERNS.    BY DEVELOPING SPECIAL PROCEDURAL
    LANGUAGES FOR GRAMMAR, SEMANTICS, AND  DEDUCTIVE  LOGIC,
    THE  FLEXIBILITY  AND  POWER OF PROGRAMMING LANGUAGES IS
    GAINED    WHILE    RETAINING    THE    REGULARITY    AND
    UNDERSTANDABILITY  OF SIMPLER RULE FORMS.  EACH PIECE OF
    KNOWLEDGE CAN BE A PROCEDURE, AND CAN CALL ON ANY  OTHER
    PIECE OF KNOWLEDGE IN THE SYSTEM.

IMPLEMENTATION AND VERSION INFORMATION
--------------------------------------

    SHRDLU  WAS  PROGRAMMED  AT   THE   MIT   ARTIFICIAL
    INTELLIGENCE  LABORATORY  BY  T.   WINOGRAD AS PART OF A
    DOCTORAL DISSERTATION IN MATHEMATICS.

    THE PROGRAM WAS MODIFIED DURING THE LAST YEAR BY  T.
    WINOGRAD,  D.  MACDONALD, J. HILL, AND S. CARD TO CHANGE
    SOME OF ITS INTERNAL REPRESENTATIONS  AND  TO  MAKE  THE
    CODE  EASIER  TO UNDERSTAND FOR PERSONS WISHING TO STUDY
    THE PROGRAM.  NO MAJOR ATTEMPTS WERE  MADE  TO  INCREASE
    ITS POWER.

    THE PROGRAM RUNNING AT C-MU IS THE MODIFIED VERSION.
    THE DISPLAY FACILITIES OF  THE  PROGRAM  HAVE  NOT  BEEN
    IMPLEMENTED  AT  C-MU.  THE PROGRAM WAS COAXED AWAY FROM
    MIT'S  INCOMPATIBLE  TIME-SHARING   SYSTEM   (ITS)   AND
    CONVERTED  TO RUN UNDER THE DEC TOPS10 (10-50) OPERATING
    SYSTEM BY CONVERTING MACLISP ITSELF  (AND  TO  DO  THAT,
    CONVERTING  THE  MIDAS  ASSEMBLY LANGUAGE).  THE MACLISP
    CONVERSION WAS DONE BY GEORGE ROBERTSON.

THE VERSION OF SHRDLU BEING DISTRIBUTED FROM  CMU  IS  NAMED
    THE  C1  VERSION.  IT IS CURRENT WITH THE MIT VERSION TO
    JUNE 1972.  THE SHOW AND TELL USER INTERFACE AND VARIOUS
    CHANGES   WERE  ADDED  FOR  THE  C-MU  WORKSHOP  ON  NEW
    TECHNOLOGIES IN COGNITIVE RESEARCH IN JUNE 1972.

    SHRDLU IS  WRITTEN  IN  MACLISP  1.6  (VINTAGE  JUNE
    1972).  IT USES ABOUT 100 TO 140K 36-BIT WORDS OF MEMORY
    ON A PDP-10.

THE SHRDLU DISTRIBUTION PACKAGE
-------------------------------

    A  distribution  package  of  the C1 system has been
    make up containing basically

    1. a SAV copy of MACLSP which runs under a version
    of the DEC 10/50 monitor on a PDP-10. TRACE and GRIND
    files are also included.

    2.  a MICROPLANNER (EXPR version)

    3.  a SHRDLU, in two parts.

Together  these constitute a SHRDLU Kit from which it should
    be possible to fashion a full SHRDLU  or  a  parser-only
    system  or  a  MICROPLANNER.   It  is  thought  that the
    package has a good chance of running on any (unmodified)
    10/50  system  or  on  any  other  system  with  a fully
    compatible MACLISP (ha!).

The system currently running at C-mu uses 134K of core,  but
    with  hand  editing this should go down to 100k.  Please
    note that a SAVE file of the system occupies between 800
    and  1000  blocks  of  disk  space.  A SAVE file is not,
    therefor, even remotely close to fitting onto a DECTAPE.

MACLISP:
    MACLISP  has  not been completely implemented.  This
    version is, however, sufficient to run SHRDLU.  A couple
    perversities:

    1. most LISP control characters don't work.  To
    do a <control g> type (IOC G).

    2. Because of an incompatibility problem, UWRITE files
    and (IOC B) printer output are double spaced.

    3. occasionally the garbage collector fails.
    (IOC D) makes it print out every time it is garbage-
    collecting so you can usually figure out what
    happened.  (IOC C) turns off this printout.  To
    get around the garbage collector's always failing
    in the same place in your program, try forcing
    a garbage collect with (GC) so that it gets to
    garbage collect in some other part of your program.

It is possible to obtain a version of MICROPLANNER which has
    been  compliled  into  LAP  in which case you need a LAP
    loader (which comes in both LAP and EXPR versions).  The
    LISP  compiler does work in 10-50 MACLISP so if you want
    to roll your own that is available too.   All of this is
    another good couple of DECTAPES.

SHRDLU is distributed as 2 large files to prevent parts from
    falling off and for ease  in  loading.   The  boundaries
    between  the  traditional  SHRDLU files used for editing
    are indicate by (e.g.)  ;;;[S->GLOBAL]  which  indicates
    the  Start  of a subfile called GLOBAL.  TECO buffs take
    note.

CAVEAT EMPTOR
-------------

This  set  of  probrams  is  distributed  in  the  cause  of
    dissemination of research.  There is no warranty  either
    expressed  or  implied  for  labor, parts, or servicing.
    Various bugs are known to lurk in  the  systems:  SHRDLU
    bugs  +  MICROPLANNER bugs + MACLISP bugs + MIDAS bugs +
    conversion  bugs   +   Monitor   bugs   +   installation
    incompatability  bugs  +  hardware bugs. Furthermore the
    system is presently in a state  of  development  (active
    and  suspended)  with various uncompleted projects here,
    notes to friends there, etc.

ASSEMBLING YOUR SHRDLU KIT
--------------------------

Before you begin assembling a SHRDLU it is probably wise  to
    get a listing of all of files (except MACLSP!!!).  Start
    by listing this one.  It is also helpful to transfer all
    of  the  files  to  dsk.  The names are designed so that
    this  can  be  done  with  pip  in   few   instructions.
    BEWARE!!!!  when  moving MACLSP around with PIP remember
    to use the /B switch.

Also note that the alt-mode which MACLSP looks for  to  tell
    it  that  you are through doing allocation is an 033 and
    not a 175 or 176.  The MACLISP editor also  requires  an
    033.   If  you  don't  have one type <space> after every
    allocation opportunity when entering MACLSP.  And  don't
    type (EDIT) or you'll get stuck.

Below  is  the  TTY listing for building a SHRDLU.  What you
    type is indicated in lower case letters, what  it  types
    in capitals.

..........STEP 1: Reading in and allocating MACLSP

.run dsk:maclsp

BIG NUMBER LISP 229
ALLOC? y
CORE = 26       170
FXS = 2000      $

*

..........STEP 2: Reading in MICROPLANNER

(uread plnr c1)
(DSK +)
(ioc q)
T

COMMENT

C1

DECLARE

    .
    .
    .
   etc

..........STEP 3: Reading in Trace and GRIND files

(uread trace c1)
(DSK +)
(ioc q)
T

TRACE 16, LOADING:
(? ?? REMTRACE UNTRACE TRACE)
FOR EXPLANATION, TYPE
(?)
(uread grind c1)
(DSK +)
(ioc q)
T
LOADING GRIND C1
*
(uread grinit c1)
(DSK +)
(ioc q)
T

BUILD

(ERT ERTSTOP ERTERR GLOBAL-ERR BUG SAY)

PDEFINE

DEFS

(OBJECT RELATION)

(thinit)

((PRINT (QUOTE MICRO-PLANNER)) (PRINC THVERSION) (COND ((ERRSET (APPLY
 (QUOTE UREAD) (APPEND (QUOTE (/.PLNR/. /(INIT/))) (CRUNIT))) NIL) (SE
TQ ERRLIST (CDDDDR ERRLIST)) (SETQ THTREE NIL) (SETQ THLEVEL NIL) (THE
RT TH%0% READING /.PLNR/. /(INIT/)))) (SETQ ERRLIST (CDDDDR ERRLIST))
(SETQ THINF NIL) (SETQ THTREE NIL) (SETQ THLEVEL NIL) (THERT TOP LEVEL
))

..........STEP3-1/2: Arming MICROPLANNER - This MUST be done to activate Macro-characters for subsequent readins

(ioc q)QUIT
MICRO-PLANNER C1
>>>  TOP-LEVEL
LISTENING  THVAL

..........STEP 3-3/4: (OPTIONAL) This is a good place to do a SAVE to hedge against subsequent disaster.

^C
.sav dsk:shrdlu
JOB SAVED

.start

>>>  TOP LEVEL
LISTENING  THVAL

..........STEP 4: Reading first SHRDLU file

(uread shrdl1 c1)
DSK ")
(ioc q)
T

ERT

ERTEX

PRINT2

   .
   .
   .
  ETC

..........STEP 5: Reading 2nd SHRDLU file

(uread shrdl2 c1)
(DSK ")
(ioc q)
T
ABSVAL

   .
   .
   .
  etc

..........STEP 6: Arming SHRDLU

(ioc g)
(ioc g)QUIT

..........STEP 7: INITIAL switch settings. Don't you dare.

      At this point SHRDLU will print out a list of some
      its internal switches and invite you to change them.
      The first time you run the program you should probably
      leave them as they are (type space after each arrow).
      Within SHRDLU these same switches can be changed
      with the TELL command.  Or, if you insist, this same
      display can be brought back by evaluating (SW).

      Then SHRDLU will print
READY

..........STEP 8: Saving SHRDLU

      Save immediately before you mung up the nice clean
      core image inside by playing with it.

^C
.sav dsk:shrdlu
JOB SAVED

..........STEP 9: Playing with SHRDLU.

.start

READY
      Now SHRDLU wants to eat a sentence.  And please don't
      forget the proper punctuation, especially the terminal
      punctuation mark at the end. Type sentences in
      upper case.  A capitalization is indicated by (e.g.) =A.
      This is useful for getting proper nouns across to
      the program.  The beginning of a sentence need not be capitalized.

      See Appendices 2 through 6 for examples
 To see if program is running try
      doing

<control x>
LISTENING---> tell parsing node on
*
LISTENING---> <CONTROL X>
READY
PICK UP A BIG RED BLOCK.

      The parser should start happily spewing forth
      trace messages.

INSTRUCTIONS FOR RUNNING SHRDLU
-------------------------------

     SHRDLU can be in 4 basic states, COMMAND, READY, RUN,
and REQUEST.  It is initially in COMMAND when loaded.

***
******COMMAND STATE
***

     In this state, SHRDLU expects the user to type a command.
It lets you know this by typing "LISTENING-->".
A command is a line containing one or more words, separated by
spaces and terminated by a carriage return (<CR>).  The first
word must be one of the three words SHOW, TELL, or GO.  The
SHOW command is used to ask the system to show such things
as definitions, structures it is building, and the states of various
parameters and switches.  TELL is used to tell the system new
definitions and settings.
     After executing a COMMAND, the system is ready for
another one, and prompts by saying LISTENING-->.
You can leave COMMAND state by typing <CONTROL X>
instead of a command.  This will cause the program to continue
whatever it was doing before it entered COMMAND
state, or to go to READY state if it was not already in the
process of analyzing a sentence.  If instead, you type the
command "GO", it will drop the sentence it is working on, and go
into READY state for a new one.

****
******COMMAND FORMATS
****

     The SHOW and TELL commands are based on a ZOG-like tree
(one tree for each).  The first word of the command is SHOW
or TELL, the second names a node in the corresponding tree,
and the rest are any arguments appropriate to the action
at that node.
For example, the command:

SHOW FUNCTION MEET

will display the contents of the LISP function "MEET".

SHOW SHOW

displays the "SHOW" tree, while for the "TELL" tree, you type

SHOW TELL

     If all of the arguments are not specified, the system will
request more.  For example, typing:
SHOW FUNCTION
would cause it to type out:
FUNCTION:
requesting the name of a function from the user.  It is then
in REQUEST state (see below.)
     Non-terminal nodes of the tree may or may not have corresponding
actions.  For example, the command

TELL PLANNER OFF

causes the entire batch of PLANNER tracing devices to be turned off
at once, even though there are subnodes of the PLANNER node which
can be used to turn individual features on and off selectively.
If there is no action associated with such a node, the system will
ask the user to select one of its subnodes.
     If you type "SHOW" or "HELP" followed by a carriage return,
it will guide you through the choices, using REQUESTS
(see below).

*****
*****REQUEST STATE
*****

     SHRDLU can request two kinds of information from
the user.  If it wants him to CHOOSE between a set of alternatives,
it will end the request with a ?.  If it wants him to SPECIFY
a name or list of names, it will end it with a :.
     For a CHOOSE, all it needs is enough information to decide
between the alternatives.  Begin typing your choice, and when it
is complete enough, type a <control x>.  If you type fewer
letters than necessary (e.g. typing just a P, when PLANNER
and PARSING are among the choices) it will ring the bell and wait
for you to continue.  If you type more than necessary it doesn't
matter.
     For a SPECIFY, type the required response, terminated by a
<CR>.  If you type a <CR> with nothing else, it will take some
default response which is appropriate to the action (For example,
typing a <CR> in response to a request for which properties of an
atomare to be displayed will have the effect of displaying
all of them.
    For either SPECIFY or CHOOSE, you can get more information on
what is expected by typing a ?<CR>.  It will then give you the
request again.  Typing QUIT<CR> at a "SPECIFY" REQUEST or QUIT
<CONTROL X> at a "CHOOSE" REQUEST will cause the entire command
of which it was a part to be discarded without finishing,
returning to COMMAND state.

*****
******READY STATE
*****

     The READY state is entered only when a new English sentence is to
be input.  You can tell you are in it when the sytem types
READY
Respond by typing in an English sentence in normal punctuation
(i.e. ending with a question mark or period) followed by a <CR>.
The system will automatically begin processing it, entering
RUN state.  To get into a COMMAND state while entering a sentence,
type a <CONTROL X>.

*****
******RUN STATE
*****

     Whenever a sentence is input, the system begins to RUN.  It
will stop at selected places, entering COMMAND state so the user can
SHOW things and TELL it things before continuing.  There are various
TELL commands which explain how to change these stopping points.
When a <CONTROL X> is typed at the COMMAND, the system returns to RUN
and continues.

*****
*******ABBREVIATIONS
*****

     Any word which appears in the SHOW or TELL trees can be
abbreviated by typing its first two letters.  For example,
our first command above could have been abbreviated as:
SH FU MEET
Note that arguments cannot be abbreviated since the system has no
list to check the abbreviations against.  This is also true
of responses to a "SPECIFY" REQUEST.  Responses to a "CHOOSE" REQUEST
are abbreviated by typing any initial letter string followed by
<CONTROL X> as described above.

To  see examples of the various commands, run one of the demonstration
programs.  They inclde explanatory comments.

SOME FACTS ABOUT THE PROGRAM
----------------------------

1. PARSER
    SHRDLU calls the parser by calling

    (PARSE <CONSTITUENT-CLASS> LIST-OF-SEMANTIC-FEATURES)

    This causes the 'PROGRAMMAR' program which embodies
the grammar for that constituent-class to be run.  Turning
on the parser trace causes a message 'PASSING ...' to
print out as each node in the grammar for the
constituent-class is passed (with a flowchart or program
listing of the grammar you can follow the parsing path
through it).

    At various points the grammar calls on 'Semantic
Specialists' with the

    (CALLSM <SEMANTIC-SPECIALIST>)

function.  If this function returns NIL, the Semantic-Specialist
objected to the parse.

    As the parser progresses it builds and rebuilds a
parser-tree whose nodes are labeled NODE1, NODE2, ... etc.
When the parse-trace is turned on, these nodes are displayed
whenever they are created or changed.

    The parse-tree is really a complex data structure to
which the syntactic and semantic routines contribute
information as it is developed.

    An important concept is that the surface structure of
the sentence is never destroyed.  The parse-node itself
contains pointers to the beginning (FIRSTWORD) and end
(WORDAFTER) of the constituent in the surface structure.
Whenever the grammar finds it has been partially fooled
and must try another parsing it adjusts these pointers.

2. SEMANTIC-SPECIALISTS

    Part of the work of the Semantic Specialists is to build up
interpretive semantic structures as the parsing of the sentence
proceeds.  The semantic structures built include OSS'S (Object
Semantic Structures), RSS'S (Relational Semantic Structures, TSS's
(Time Semantic Structures) and ANS's (Answer Semantic Structures).
    The Semantic Specialists are all entered through the function

    (CALLSM <SEMANTIC-SPECIALIST>)

from the gramar.

3. INFERENCE MECHANISM - PLANNER

    SHRDLU makes all references to its world model
through the function

    (THVAL2 <MICROPLANNER-STATEMENT>).

REFERENCES FOR MORE INFORMATION
-------------------------------

MACLISP
    1. MIT AI Memo 116A PDP-6 LISP (LISP 1.6)
    2. MIT AI Memo 190 - Interim LISP Users' Guide
    3. LISP INFO update file

The first 2 can only be obtained from the MIT AI Laboratory.
The last is available for this version of MACLISP from me (S.C.) at CMU) as well.
These documents contain what you have to know to really use
MACLISP as a programming system (provided you already know
how to use LISP before you read them).  They are probably not
necessary to run SHRDLU (until you get into trouble).

MICROPLANNER
    1. Winograd's thesis is the best introduction to it.
    2. A manual is available from MIT AI Laboratory.
    3. Ira Goldstein (MIT AI Laboratory) has an excellent
        demonstration program for teaching it.
    4. For the real PLANNER stuff there's Hewett's Thesis
       which has been printed and is available from MIT AI Lab.
    5. Hewett also has several AI Memos and papers in the
       two Int'l Conferences on AI.

    6. Sussman and McDermott and there
       friends have recently created a warring siblisng
       to PLANNER called CONNIVER which is explained in
       two AI Memos.

SHRDLU
    1. Winograd's thesis has been published in various
       forms including as issue of
       COGNITIVE PSYCHOLOGY and a book.

    2. You are currently reading what is probably the
       most obscure SHRDLU reference.

APPENDIX 1
----------

      ****** SHRDLU States and What They Want    *****

READY   (types out READY)
     English sentence ended with punctuation, followed by <CR>
        or <CONTROL X> to go to COMMAND
COMMAND  (types out LISTENING--->)
     Command terminated by <CR>
      or <CONTROL X> to proceed program
      or GO<CR> to prepare for new sentence
REQUEST  (types out request)
   CHOOSE (request ends with ?)
     Enough of any choice to specify it, followed by
          <CONTROL X>
        or ?<CONTROL X> to see information and choices
        or QUIT<CONTROL X> to abort command
   SPECIFY   (request ends with :)
     Name or list, ended with <CR>
        or just <CR> for default value (if appropriate)
        or QUIT<CR> to abort command
        or ?<CR> for info
Abbreviate commands by first two letters.

APPENDIX 2:
----------

PARSER TRACES EXAMPLE
======================
(NOTE ^X MEANS <CONTROL-X>)
LISTENING---> SHOW

(SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT)
WHICH OPTION? SHOW
CANSHOW
       SHOW
       TELL
       LISP
               PROPERTY
               FUNCTION
               VALUE
       PLANNER
               ASSERTIONS
               THEOREM
               SCENE
       PARSING
               NODE
               TREE
       DEFINITIONS
               UNIT
               WORD
               MARKER
       INPUT
               ALL
               REST
               CURRENT
*
LISTENING---> SHOW SCENE
  CURRENT SCENE

:B1 -->  A SMALL RED BLOCK  AT (110 100 0)SUPPORTS (:B2)
:B2 -->  A SMALL GREEN PYRAMID  AT (110 100 100)
:B3 -->  A LARGE GREEN BLOCK  AT (400 0 0)SUPPORTS (:B5)
:B4 -->  A LARGE BLUE PYRAMID  AT (640 640 1)
:B5 -->  A SMALL RED PYRAMID  AT (500 100 200)
:B6 -->  A LARGE RED BLOCK  AT (0 300 0)SUPPORTS (:B7)
:B7 -->  A LARGE GREEN BLOCK  AT (0 240 300)
:B10 -->  A LARGE BLUE BLOCK  AT (300 640 0)
:BOX -->  A LARGE WHITE BOX  AT (600 600 0)SUPPORTS (:B4)
THE HAND IS GRASPING  NOTHING
*
LISTENING---> TELL PARSING

(NODE LABEL ATTEMPT)
WHICH OPTION? NODE
ON OR OFF? ON
LISTENING---> TELL SEMANTICS

DO SEMANTIC ANALYSIS? YES
SHOW BUILDING OF SEMANTIC STRUCTURES? NO
*
LISTENING---> TELL PLANNER OFF

*
LISTENING---> TELL PL

(INPUT ACTION THEOREM ASSERTIONS)
WHICH OPTION? INPUT
ON OR OFF? ON
*
*
LISTENING---> GO
QUIT
READY
PICK UP A BIG RED BLOCK.

[1]

[NODE1]
   SEMANTICS      NIL
   DAUGHTERS      NIL
   WORDAFTER      (PICK UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (TOPLEVEL MAJOR CLAUSE)

>>>
LISTENING--->TE AT ON

*
LISTENING---> 
PASSING ENTERING-CLAUSE

[TSS1]
   TSSNODE=       TSS1

>>>
LISTENING---> 
PASSING INIT
PASSING MAJOR
PASSING THEREINIT
PASSING THER2
(1 ENTER PARSE (NG TIME))

[NODE2]
   SEMANTICS      NIL
   DAUGHTERS      NIL
   WORDAFTER      (PICK UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (TIME NG)

>>>
LISTENING---> 
PASSING ENTERING-NG
PASSING NGSTART
PASSING TIME
PASSING FAIL
(1 EXIT PARSE NIL)
PASSING CLAUSETYPE
(1 ENTER PARSE (VG IMPER))

[NODE3]
   SEMANTICS      NIL
   DAUGHTERS      NIL

   WORDAFTER      (PICK UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (IMPER VG)

>>>
LISTENING---> 
PASSING ENTERING-VG
PASSING IMPER
(2 ENTER PARSE (VB DO NEG INF))
(2 EXIT PARSE NIL)
(2 ENTER PARSE (VB (MVB) INF))

[NODE4]
   SEMANTICS      ((TRANS (#NOTICE)))
   DAUGHTERS      WORD
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (VPRT VB INF TRANS MVB)

>>>
LISTENING---> 

[NODE3]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE4)
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (IMPER VG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE3))
PASSING RETURN

[NODE3]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE4)
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (VG IMPER)

>>>
LISTENING---> 

[NODE1]
   MVB            (NODE4)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      NIL
   DAUGHTERS      (NODE3)
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (TOPLEVEL MAJOR CLAUSE)

>>>
LISTENING---> 
(1 EXIT PARSE (NODE3))
PASSING VG1
(1 ENTER PARSE (PRT))

[NODE5]
   SEMANTICS      T
   DAUGHTERS      WORD
   WORDAFTER      (A BIG RED BLOCK)
   FIRSTWORD      (UP A BIG RED BLOCK)
   FEATURES       (PRT)

>>>
LISTENING---> 

[NODE1]
   MVB            (NODE4)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      NIL
   DAUGHTERS      (NODE5 NODE3)
   WORDAFTER      (A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (ACTV IMPER TOPLEVEL MAJOR CLAUSE)

>>>
LISTENING---> SHOW TREE

NODE-SPECIFICATION: C
(((PICK UP) (ACTV IMPER TOPLEVEL MAJOR CLAUSE)
             NIL
             (((PICK) (VG IMPER) NIL ((PICK (VPRT VB INF TRANS MVB))))
              (UP (PRT))))
 NIL)
*
LISTENING--->
(1 EXIT PARSE (NODE1))
PASSING TRANSP
(1 ENTER PARSE (NG OBJ OBJ1))

[NODE6]
   SEMANTICS      NIL
   DAUGHTERS      NIL
   WORDAFTER      (A BIG RED BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (OBJ1 OBJ NG)

>>>
LISTENING---> 
PASSING ENTERING-NG
PASSING NGSTART
PASSING LOOK
PASSING DET
(2 ENTER PARSE (DET))

[NODE7]
   SEMANTICS      T
   DAUGHTERS      WORD
   WORDAFTER     (A BIG RED BLOCK)

   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (DET NS INDEF)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE7)
   WORDAFTER      (BIG RED BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (OBJ1 OBJ NG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING IND
PASSING ADJ
(2 ENTER PARSE (ADJ))

[NODE10]
   SEMANTICS      (OBJECT
                      (MARKERS: (#PHYSOB #BIG)
                       PROCEDURE: ((#MORE #SIZE
                                          ***
                                          (200 200
                                               200)))))
   DAUGHTERS      WORD
   WORDAFTER      (RED BLOCK)
   FIRSTWORD      (BIG RED BLOCK)
   FEATURES       (ADJ)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE10 NODE7)
   WORDAFTER      (RED BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (INDEF NS DET OBJ1 OBJ NG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING ADJ
(2 ENTER PARSE (ADJ))

[NODE11]
   SEMANTICS      (#COLOR #RED)
   DAUGHTERS      WORD
   WORDAFTER      (BLOCK)
   FIRSTWORD      (RED BLOCK)
   FEATURES       (ADJ)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE11 NODE10 NODE7)
   WORDAFTER      (BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (INDEF NS DET OBJ1 OBJ NG)

>>>
LISTENING---> SHOW CURRENT

A BIG RED
*
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING ADJ
(2 ENTER PARSE (ADJ))
(2 EXIT PARSE NIL)
PASSING CLASF
(2 ENTER PARSE (VB ING (CLASF)))
(2 EXIT PARSE NIL)
(2 ENTER PARSE (VB EN (CLASF)))
(2 EXIT PARSE NIL)
(2 ENTER PARSE (CLASF))
(2 EXIT PARSE NIL)
PASSING NOUN
(2 ENTER PARSE (NOUN))

[NODE12]
   SEMANTICS      (OBJECT
                      (MARKERS: (#MANIP
                                 #RECTANGULAR)
                       PROCEDURE: ((#IS *** #BLOCK))))
   DAUGHTERS      WORD
   WORDAFTER      NIL
   FIRSTWORD      (BLOCK)

   FEATURES       (NOUN NS)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE12 NODE11 NODE10 NODE7)
   WORDAFTER      NIL
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (INDEF NS DET OBJ1 OBJ NG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING SMNG
(1 ENTER CALLSM ((SMNG1)))

>>>
LISTENING---> 
(1 EXIT CALLSM (OSS4))
PASSING RETSM
(1 ENTER CALLSM ((SMNG2)))
(1 EXIT CALLSM (OSS4))
PASSING RETURN

[NODE6]
   HEAD   (NODE12 NODE11 NODE10 NODE7)
   PARENT         (NODE1)
   SEMANTICS      (OSS4)
   DAUGHTERS      (NODE12 NODE11 NODE10 NODE7)
   WORDAFTER      NIL
   FIRSTWORD      (A BIG RED BLOCK)

   FEATURES       (NG OBJ OBJ1 DET NS INDEF)

>>>
LISTENING---> 

[NODE1]
   MVB            (PICK-UP)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      NIL
   DAUGHTERS      (NODE6 NODE5 NODE3)
   WORDAFTER      NIL
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (PRT ACTV
                       IMPER
                       TOPLEVEL
                       MAJOR
                       CLAUSE)

>>>
LISTENING---> SHOW WORD PICK-UP

[PICK-UP]
   FEATURES       (COMBINATION TRANS)
   SEMANTICS      ((TRANS
                    (RELATION
                        (RESTRICTIONS: (((#ANIMATE))
                                        ((#MANIP)))
                         MARKERS: (#EVENT)
                         PROCEDURE: ((#EVAL
                                      (COND
                                       ((MEMQ (NUMBER? SMOB1)
                                              '(1 NS))
                                        '(#PICKUP #2 *TIME))
                                       ('(#PUTIN
                                          #2
                                          :BOX
                                          *TIME)))))))))
   ROOT   (PICK UP)

>>>
LISTENING---> 
*
LISTENING---> 
(1 EXIT PARSE (NODE6))
PASSING OBB
PASSING FQPRT
PASSING ONT
(1 ENTER CALLSM ((SMCL1)))

>>>
LISTENING---> 
(1 EXIT CALLSM (RSS1))
PASSING RETSM
(1 ENTER CALLSM ((SMCL2)))
(1 EXIT CALLSM (NODE6 NODE5 NODE3))
PASSING RETURN

[NODE1]
   OBJ1   (NODE6 NODE5 NODE3)
   MVB            (PICK-UP)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      (RSS1)
   DAUGHTERS      (NODE6 NODE5 NODE3)
   WORDAFTER      NIL
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (CLAUSE MAJOR
                          TOPLEVEL
                          IMPER
                          ACTV
                          PRT
                          TRANS)

>>>
LISTENING---> SHOW TREE C

(((PICK UP A BIG RED BLOCK)
  (CLAUSE MAJOR TOPLEVEL IMPER ACTV PRT TRANS)
  (RSS1)
  (((PICK) (VG IMPER) NIL ((PICK (VPRT VB INF TRANS MVB))))
   (UP (PRT))
   ((A BIG RED BLOCK)
    (NG OBJ OBJ1 DET NS INDEF)
    (OSS4)
    ((A (DET NS INDEF)) (BIG (ADJ)) (RED (ADJ)) (BLOCK (NOUN NS))))))
 NIL)
*
LISTENING---> 

(THAND (THGOAL (#PICKUP OSS4) (THDBF MUMBLE) (THUSE TC-2))
        (VALUEPUT)
        (SETQ SUCCESS T)
        (SETQ PLAN2 PLAN))
>>>  FOR PLANNER
OK.

APPENDIX 3
----------

THIS IS THE SEMANTIC STRUCTURES TRACE FOR "PICK UP A BIG RED BLOCK"
===================================================================
LISTENING---> SHOW

(SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT)
WHICH OPTION? SHOW
CANSHOW
       SHOW
       TELL
       LISP
               PROPERTY
               FUNCTION
               VALUE
       PLANNER
               ASSERTIONS
               THEOREM
               SCENE
       PARSING
               NODE
               TREE
       DEFINITIONS
               UNIT
               WORD
               MARKER
       INPUT
               ALL
               REST
               CURRENT
*
LISTENING---> SHOW SCENE
  CURRENT SCENE

:B1 -->  A SMALL RED BLOCK  AT (110 100 0)SUPPORTS (:B2)
:B2 -->  A SMALL GREEN PYRAMID  AT (110 100 100)
:B3 -->  A LARGE GREEN BLOCK  AT (400 0 0)SUPPORTS (:B5)
:B4 -->  A LARGE BLUE PYRAMID  AT (640 640 1)
:B5 -->  A SMALL RED PYRAMID  AT (500 100 200)
:B6 -->  A LARGE RED BLOCK  AT (0 300 0)SUPPORTS (:B7)
:B7 -->  A LARGE GREEN BLOCK  AT (0 240 300)
:B10 -->  A LARGE BLUE BLOCK  AT (300 640 0)
:BOX -->  A LARGE WHITE BOX  AT (600 600 0)SUPPORTS (:B4)
THE HAND IS GRASPING  NOTHING
*
LISTENING---> TELL PARSING OFF

*
LISTENING---> TELL SEMANTICS

DO SEMANTIC ANALYSIS? YES
SHOW BUILDING OF SEMANTIC STRUCTURES? YES
*
LISTENING---> TELL PLANNER OFF

*
LISTENING---> TELL PL

(INPUT ACTION THEOREM ASSERTIONS)
WHICH OPTION? INPUT
ON OR OFF? ON
*
LISTENING---> GO
QUIT
READY
PICK UP A BIG RED BLOCK.

>>>
LISTENING---> SH NODE C

[NODE3]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE4)
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (VG IMPER)

>>>
LISTENING---> SHOW NODE C

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE12 NODE11 NODE10 NODE7)
   WORDAFTER      NIL
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (INDEF NS DET OBJ1 OBJ NG)

[OSS1]
   OSSNODE=       OSS1
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   DETERMINER=    (NS INDEF NIL)

>>>
LISTENING---> SH CU

A BIG RED BLOCK
*
LISTENING---> 

[OSS2]
   OSSNODE=       OSS2
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   RELATIONS=     ((#IS OSS1 #BLOCK))
   DETERMINER=    (NS INDEF NIL)
   PLAUSIBILITY= 0
   SYSTEMS=       (#SHAPES #PHYSOB #THING #SYSTEMS)
   MARKERS=       (#SHAPES #RECTANGULAR
                           #SYSTEMS
                           #THING
                           #PHYSOB
                           #MANIP)

>>>
LISTENING---> SHOW MA #SHAPES

#SHAPES
*
LISTENING---> SHOW MA #PHYSOB

#PHYSOB
       #BOX
       #CONSTRUCT
               #STACK
               #ROW
       #HAND
       #MANIP
       #TABLE
*
LISTENING---> 

[OSS3]
   OSSNODE=       OSS3
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   RELATIONS=     ((#COLOR OSS2 #RED)
                   (#IS OSS1 #BLOCK))
   DETERMINER=    (NS INDEF NIL)
   PLAUSIBILITY=  0
   SYSTEMS=     (#SPECTRUM #SHAPES
                           #PHYSOB
                           #THING
                           #SYSTEMS)
   MARKERS=       (#SPECTRUM #RED
                             #SHAPES
                             #RECTANGULAR
                             #SYSTEMS
                             #THING
                             #PHYSOB
                             #MANIP)

>>>
LISTENING---> 

[OSS4]
   OSSNODE=       OSS4
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   RELATIONS=     ((#MORE #SIZE OSS3 (200 200 200))
                   (#COLOR OSS2 #RED)
                   (#IS OSS1 #BLOCK))
   DETERMINER=    (NS INDEF NIL)
   PLAUSIBILITY= 0
   SYSTEMS=       (#SPECTRUM #SHAPES
                             #PHYSOB
                             #THING
                             #SYSTEMS)
   MARKERS=       (#BIG #SPECTRUM
                        #RED
                        #SHAPES
                        #RECTANGULAR
                        #SYSTEMS
                        #THING
                        #PHYSOB
                        #MANIP)

>>>
LISTENING---> SHOW NODE C

[NODE1]
   MVB            (PICK-UP)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      NIL
   DAUGHTERS      (NODE6 NODE5 NODE3)
   WORDAFTER      NIL
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (PRT ACTV
                       IMPER
                       TOPLEVEL
                       MAJOR
                       CLAUSE)

[RSS1]
   PARSENODE=     (NODE1)
   RSSNODE=       RSS1
   VARIABLE=      EVX1
   RELATIONS=     ((#PICKUP OSS4 TSS1))
   PLAUSIBILITY= 0
   SYSTEMS=       (#SYSTEMS)
   MARKERS=       (#SYSTEMS #EVENT)

>>>
LISTENING---> SH NODE C

[NODE1]
   OBJ1   (NODE6 NODE5 NODE3)
   MVB            (PICK-UP)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      (RSS1)
   DAUGHTERS      (NODE6 NODE5 NODE3)
   WORDAFTER      NIL
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (CLAUSE MAJOR
                          TOPLEVEL
                          IMPER
                          ACTV
                          PRT
                          TRANS)

>>>
LISTENING---> SHOW TREE C

(((PICK UP A BIG RED BLOCK)
  (CLAUSE MAJOR TOPLEVEL IMPER ACTV PRT TRANS)
  (RSS1)
  (((PICK) (VG IMPER) NIL ((PICK (VPRT VB INF TRANS MVB))))
   (UP (PRT))
   ((A BIG RED BLOCK)
    (NG OBJ OBJ1 DET NS INDEF)
    (OSS4)
    ((A (DET NS INDEF)) (BIG (ADJ)) (RED (ADJ)) (BLOCK (NOUN NS))))))
 NIL)
*
LISTENING---> 

(THAND (THGOAL (#PICKUP OSS4) (THDBF MUMBLE) (THUSE TC-2))
        (VALUEPUT)
        (SETQ SUCCESS T)
        (SETQ PLAN2 PLAN))
>>>  FOR PLANNER
OK.

APPENDIX 4
----------

THIS IS THE INFERENCE TRACE FOR "PICK UP A BIG RED BLOCK."
===============================================================
 ]--> LOADING SHRDLU
         <CONTROL N> GETS BACK TO ZOG
>>> SHRDLU COMMAND STATE, TYPE HELP <CR> FOR INSTRUCTIONS.
LISTENING---> SHOW

(SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT)
WHICH OPTION? SHOW
CANSHOW
       SHOW
       TELL
       LISP
               PROPERTY
               FUNCTION
               VALUE
       PLANNER
               ASSERTIONS
               THEOREM
               SCENE
       PARSING
               NODE
               TREE
       DEFINITIONS
               UNIT
               WORD
               MARKER
       INPUT
               ALL
               REST
               CURRENT
*
LISTENING---> SHOW SCENE
  CURRENT SCENE

:B1 -->  A SMALL RED BLOCK  AT (110 100 0)SUPPORTS (:B2)
:B2 -->  A SMALL GREEN PYRAMID  AT (110 100 100)
:B3 -->  A LARGE GREEN BLOCK  AT (400 0 0)SUPPORTS (:B5)
:B4 -->  A LARGE BLUE PYRAMID  AT (640 640 1)
:B5 -->  A SMALL RED PYRAMID  AT (500 100 200)
:B6 -->  A LARGE RED BLOCK  AT (0 300 0)SUPPORTS (:B7)
:B7 -->  A LARGE GREEN BLOCK  AT (0 240 300)
:B10 -->  A LARGE BLUE BLOCK  AT (300 640 0)
:BOX -->  A LARGE WHITE BOX  AT (600 600 0)SUPPORTS (:B4)
THE HAND IS GRASPING  NOTHING
*
LISTENING---> TELL PARSING OFF
*
LISTENING---> TELL SEMANTICS

DO SEMANTIC ANALYSIS? YES
SHOW BUILDING OF SEMANTIC STRUCTURES? NO
*
LISTENING---> TELL PLANNER OFF
*
LISTENING---> GO
QUIT
READY
PICK UP A BIG RED BLOCK.

*
LISTENING---> 

(THAND (THGOAL (#PICKUP OSS4) (THDBF MUMBLE) (THUSE TC-2))
        (VALUEPUT)
        (SETQ SUCCESS T)
        (SETQ PLAN2 PLAN))
>>>  FOR PLANNER
LISTENING---> 
>>>  ENTERING FINDCHOOSE EXPR (OSS X ANS2)

*
LISTENING---> SHOW VALUE OSS

OSS4
*
LISTENING---> 
>>>  EXITING FINDCHOOSE
LISTENING---> SHOW VALUE X

:B6
*
LISTENING---> SHOW PLANNER

(ASSERTIONS THEOREM SCENE)
WHICH OPTION? ASSERTIONS
ATOM: ?

SHOW ALL ASSERTIONS WHICH CONTAIN THE GIVEN ATOM
ATOM: :B6

(((#COLOR :B6 #RED)) ((#SHAPE :B6 #RECTANGULAR))
                      ((#SUPPORT :B6 :B7))
                      ((#AT :B6 (0 300 0)))
                      ((#IS :B6 #BLOCK))
                      ((#MANIP :B6))
                      ((#SUPPORT :TABLE :B6)))
*
LISTENING---> TELL PLANNER ON

*
LISTENING---> 
TRYING GOAL G1 (#PICKUP :B6)
>>>
LISTENING---> 
ENTERING THEOREM TC-PICKUP
TRYING GOAL G2 (#GRASP :B6)
>>>
LISTENING---> SHOW THEOREM TC-PICKUP

(THCONSE (X (WHY (EV)) EV) (#PICKUP $?X)
          (MEMORY)

          (THGOAL (#GRASP $?X) (THUSE TC-GRASP))
          (THGOAL (#RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
          (MEMOREND (#PICKUP $?EV $?X)))
*
ENTERING THEOREM TC-GRASP
TRYING GOAL G3 (#GRASPING :B6)
G3 FAILED
TRYING GOAL G4 (#CLEARTOP :B6)
ENTERING THEOREM TC-CLEARTOP
TRYING GOAL G5 (#SUPPORT :B6 ?)
G5 SUCCEEDED ((#SUPPORT :B6 :B7))
TRYING GOAL G6 (#SUPPORT :B6 (THNV Y))
G6 SUCCEEDED ((#SUPPORT :B6 :B7))
TRYING GOAL G7 (#GET-RID-OF :B7)
ENTERING THEOREM TC-GET-RID-OF
TRYING GOAL G10 (#FINDSPACE :TABLE (200 200 200) :B7 (THNV Y))
ENTERING THEOREM TC-FINDSPACE
TC-FINDSPACE SUCCEEDED THNOVAL
G10 SUCCEEDED
ENTERING THEOREM TC-PUT
TRYING GOAL G12 (#GRASP :B7)
ENTERING THEOREM TC-GRASP

TRYING GOAL G13 (#GRASPING :B7)
G13 FAILED
TRYING GOAL G14 (#CLEARTOP :B7)
G14 SUCCEEDED ((#CLEARTOP :B7))
TRYING GOAL G15 (#GRASPING (THNV Y))
G15 FAILED
TRYING GOAL G16 (#MOVEHAND2 (100 340 500))
ENTERING THEOREM TC-MOVEHAND2
TC-MOVEHAND2 SUCCEEDED THNOVAL
G16 SUCCEEDED (#MOVEHAND2 (100 340 500))
ASSERTING A17 (#GRASPING :B7)
A17 SUCCEEDED
ASSERTING A20 (#GRASP E5 :B7)
A20 SUCCEEDED
TC-GRASP SUCCEEDED THNOVAL
G12 SUCCEEDED (#GRASP :B7)
>>>
LISTENING---> SHOW PR E5

[E5]
   TYPE   #GRASP
   THASSERTION    (NIL (2 (3 1 ((#GRASP E5 :B7)))))
   END            1
   WHY            E4
   START          0

*
TRYING GOAL G21 (#MOVEHAND (600 374 200))
ENTERING THEOREM TC-MOVEHAND
TRYING GOAL G22 (#GRASPING (THV X))
G22 SUCCEEDED ((#GRASPING :B7))
TRYING GOAL G23 (#AT :B7 (THNV W))
G23 SUCCEEDED ((#AT :B7 (0 240 300)))
ERASING E24 (#AT :B7 (0 240 300))
ENTERING THEOREM TE-SUPP
TRYING GOAL G25 (#SUPPORT :B7 (THNV Y))
G25 FAILED
TRYING GOAL G26 (#SUPPORT (THNV Y) :B7)
G26 SUCCEEDED ((#SUPPORT :B6 :B7))
ERASING E27 (#SUPPORT :B6 :B7)
E27 SUCCEEDED
TRYING GOAL G30 (#PART :B7 (THNV Y))
G30 FAILED
TE-SUPP SUCCEEDED THNOVAL
ENTERING THEOREM TE-CONTAIN
TRYING GOAL G31 (#CONTAIN (THNV Y) :B7)
G31 FAILED
TE-CONTAIN FAILED
E24 SUCCEEDED
ASSERTING A32 (#AT :B7 (500 274 0))
ENTERING THEOREM TA-AT
TA-AT SUCCEEDED THNOVAL
ENTERING THEOREM TA-SUPP
TRYING GOAL G33 (#MANIP :TABLE)
G33 FAILED
ASSERTING A34 (#SUPPORT :TABLE :B7)
A34 SUCCEEDED
TRYING GOAL G35 (#CLEARTOP :TABLE)
G35 FAILED
TA-SUPP SUCCEEDED THNOVAL
ENTERING THEOREM TA-CONTAIN
TRYING GOAL G36 (#MANIP :B7)
G36 SUCCEEDED ((#MANIP :B7))
TRYING GOAL G37 (#SUPPORT (THV Y) :B7)
G37 SUCCEEDED ((#SUPPORT :TABLE :B7))
TRYING GOAL G40 (#IS :TABLE #BOX)
G40 FAILED
TRYING GOAL G41 (#CONTAIN (THV Z) :TABLE)
G41 FAILED
G37 FAILED
G36 FAILED
TA-CONTAIN FAILED

A32 SUCCEEDED
TRYING GOAL G42 (#MOVEHAND2 (600 374 200))
ENTERING THEOREM TC-MOVEHAND2
TC-MOVEHAND2 SUCCEEDED THNOVAL
G42 SUCCEEDED (#MOVEHAND2 (600 374 200))
TRYING GOAL G43 (#SUPPORT (THV Y) :B7)
G43 SUCCEEDED ((#SUPPORT :TABLE :B7))
TC-MOVEHAND SUCCEEDED THNOVAL
G21 SUCCEEDED (#MOVEHAND (600 374 200))
TRYING GOAL G44 (#UNGRASP)
ENTERING THEOREM TC-UNGRASP
TRYING GOAL G45 (#GRASPING (THV X))
G45 SUCCEEDED ((#GRASPING :B7))
TRYING GOAL G46 (#SUPPORT ? :B7)
G46 SUCCEEDED ((#SUPPORT :TABLE :B7))
ERASING E47 (#GRASPING :B7)
E47 SUCCEEDED
ASSERTING A50 (#UNGRASP E6 :B7)
A50 SUCCEEDED
TC-UNGRASP SUCCEEDED THNOVAL
G44 SUCCEEDED (#UNGRASP)
TC-PUT SUCCEEDED THNOVAL
G11 SUCCEEDED (#PUT :B7 (500 274 0))
ASSERTING A51 (#GET-RID-OF E4 :B7)
A51 SUCCEEDED
TC-GET-RID-OF SUCCEEDED THNOVAL
G7 SUCCEEDED (#GET-RID-OF :B7)
TRYING GOAL G52 (#SUPPORT :B6 (THNV Y))
G52 FAILED
ASSERTING A53 (#CLEARTOP :B6)
A53 SUCCEEDED
ASSERTING A54 (#CLEARTOP E3 :B6)
A54 SUCCEEDED
TC-CLEARTOP SUCCEEDED THNOVAL
G4 SUCCEEDED (#CLEARTOP :B6)
>>>
LISTENING---> SHOW EVENT E3

E3  (#CLEARTOP E3 :B6)  --  TIME0 TO 3
    E6  (#UNGRASP E6 :B7)  --  TIME2 TO 2
    E4  (#GET-RID-OF E4 :B7)  --  TIME0 TO 3
         E5  (#GRASP E5 :B7)  --  TIME0 TO 1
*
TRYING GOAL G55 (#GRASPING (THNV Y))
G55 FAILED
TRYING GOAL G56 (#MOVEHAND2 (100 440 300))
ENTERING THEOREM TC-MOVEHAND2
TC-MOVEHAND2 SUCCEEDED THNOVAL
G56 SUCCEEDED (#MOVEHAND2 (100 440 300))
ASSERTING A57 (#GRASPING :B6)
A57 SUCCEEDED
ASSERTING A60 (#GRASP E2 :B6)
A60 SUCCEEDED
TC-GRASP SUCCEEDED THNOVAL
G2 SUCCEEDED (#GRASP :B6)
TRYING GOAL G61 (#RAISEHAND)
ENTERING THEOREM TC-RAISEHAND
TRYING GOAL G62 (#MOVEHAND (100 440 1000))
>>>
LISTENING---> TELL PL

(INPUT ACTION THEOREM ASSERTIONS)
WHICH OPTION? ACTION
ON OR OFF? OFF

*
LISTENING---> TELL ACTION (THASSERT)

*
ASSERTING A63 (#AT :B6 (0 300 500))
A63 SUCCEEDED
G62 SUCCEEDED (#MOVEHAND (100 440 1000))
ASSERTING A64 (#RAISEHAND E7)
A64 SUCCEEDED  SUCCEEDED (#RAISEHAND)
ASSERTING A65 (#PICKUP E1 :B6)
A65 SUCCEEDED
TC-PICKUP SUCCEEDED THNOVAL

G1 SUCCEEDED (#PICKUP :B6)
>>>
LISTENING---> SHOW EVENT E1

E1  (#PICKUP E1 :B6)  --  TIME 0 TO 5
    E2  (#GRASP E2 :B6)  --  TIME 0 TO 4
         E3  (#CLEARTOP E3 :B6)  --  TIME 0 TO 3
              E4  (#GET-RID-OF E4 :B7)  --  TIME 0 TO 3
                   E5  (#GRASP E5 :B7)  --  TIME 0 TO 1
              E6  (#UNGRASP E6 :B7)  --  TIME 2 TO 2
    E7  (#RAISEHAND E7)  --  TIME 4 TO 5
(EE LISTENING---> 

[ANS1]
   ACTION=        ((MOVETO 100 340 500) (GRASP ':B7)
                                        (MOVETO 600 374 200)
                                        (UNGRASP)
                                        (MOVETO 100 440 300)
                                        (GRASP ':B6)
                                        (MOVETO 100 440 1000)
                                        (SAY OK))
   ANSRSS=        RSS1
   ANSNODE=       ANS1
   PLAUSIBILITY= 0

*
MOVETO (100 340 500)
GRASP :B7
MOVETO (600 374 200)
UNGRASP
MOVETO (100 440 300)
GRASP :B6
MOVETO (100 440 1000)OK .

APPENDIX 5
----------

 THIS IS ALL THREE TRACES COMBINED FOR "PICK UP A BIG RED BLOCK."
(I.E. PARSING, SEMANTIC STRUCTURES AND INFERENCE)
=================================================================
>SHRDLU RUN!
>>> SHRDLU COMMAND STATE, TYPE HELP <CR> FOR INSTRUCTIONS.
LISTENING---> SHOW

(SHOW TELL LISP PLANNER PARSING DEFINITIONS INPUT)
WHICH OPTION? SHOW
CANSHOW
       SHOW
       TELL
       LISP
               PROPERTY
               FUNCTION
               VALUE
       PLANNER
               ASSERTIONS
               THEOREM
               SCENE
       PARSING
               NODE
               TREE
       DEFINITIONS
               UNIT
               WORD
               MARKER
       INPUT
               ALL
               REST
               CURRENT
*
LISTENING---> SHOW SCENE
  CURRENT SCENE

:B1 -->  A SMALL RED BLOCK  AT (110 100 0)SUPPORTS (:B2)
:B2 -->  A SMALL GREEN PYRAMID  AT (110 100 100)
:B3 -->  A LARGE GREEN BLOCK  AT (400 0 0)SUPPORTS (:B5)
:B4 -->  A LARGE BLUE PYRAMID  AT (640 640 1)
:B5 -->  A SMALL RED PYRAMID  AT (500 100 200)
:B6 -->  A LARGE RED BLOCK  AT (0 300 0)SUPPORTS (:B7)
:B7 -->  A LARGE GREEN BLOCK  AT (0 240 300)
:B10 -->  A LARGE BLUE BLOCK  AT (300 640 0)
:BOX -->  A LARGE WHITE BOX  AT (600 600 0)SUPPORTS (:B4)
THE HAND IS GRASPING  NOTHING
*
LISTENING---> TELL PARSING

(NODE LABEL ATTEMPT)
WHICH OPTION? NODE
ON OR OFF? ON

*
LISTENING---> TELL LABEL ON

*
LISTENING---> TELL SEMANTICS

DO SEMANTIC ANALYSIS? YES
SHOW BUILDING OF SEMANTIC STRUCTURES? YES
*
LISTENING---> TELL PLANNER OFF

*
LISTENING---> TELL PL

(INPUT ACTION THEOREM ASSERTIONS)
WHICH OPTION? INPUT
ON OR OFF? ON
*
LISTENING---> TE FUNCTION FINDCHOOSE

TRACE BREAK UNTRACE OR UNBREAK? BREAK
FINDCHOOSE
*
LISTENING---> GO
QUIT
READY
PICK UP A BIG RED BLOCK.

[1]

[NODE1]
   SEMANTICS      NIL
   DAUGHTERS      NIL
   WORDAFTER      (PICK UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (TOPLEVEL MAJOR CLAUSE)

>>>
LISTENING--->TE AT ON

*
LISTENING---> 
PASSING ENTERING-CLAUSE

[TSS1]
   TSSNODE=       TSS1

>>>
LISTENING---> 
PASSING INIT
PASSING MAJOR
PASSING THEREINIT
PASSING THER2
(1 ENTER PARSE (NG TIME))

[NODE2]
   SEMANTICS      NIL
   DAUGHTERS      NIL
   WORDAFTER      (PICK UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (TIME NG)

>>>
LISTENING---> 
PASSING ENTERING-NG
PASSING NGSTART
PASSING TIME
PASSING FAIL
(1 EXIT PARSE NIL)
PASSING CLAUSETYPE
(1 ENTER PARSE (VG IMPER))

[NODE3]
   SEMANTICS      NIL
   DAUGHTERS      NIL

   WORDAFTER      (PICK UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (IMPER VG)

>>>
LISTENING---> 
PASSING ENTERING-VG
PASSING IMPER
(2 ENTER PARSE (VB DO NEG INF))
(2 EXIT PARSE NIL)
(2 ENTER PARSE (VB (MVB) INF))

[NODE4]
   SEMANTICS      ((TRANS (#NOTICE)))
   DAUGHTERS      WORD
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (VPRT VB INF TRANS MVB)

>>>
LISTENING---> 

[NODE3]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE4)
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (IMPER VG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE3))
PASSING RETURN

[NODE3]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE4)
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (VG IMPER)

>>>
LISTENING---> 

[NODE1]
   MVB            (NODE4)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      NIL
   DAUGHTERS      (NODE3)
   WORDAFTER      (UP A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (TOPLEVEL MAJOR CLAUSE)

>>>
LISTENING---> 
(1 EXIT PARSE (NODE3))
PASSING VG1
(1 ENTER PARSE (PRT))

[NODE5]
   SEMANTICS      T
   DAUGHTERS      WORD
   WORDAFTER      (A BIG RED BLOCK)
   FIRSTWORD      (UP A BIG RED BLOCK)
   FEATURES       (PRT)

>>>
LISTENING---> 

[NODE1]
   MVB            (NODE4)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      NIL
   DAUGHTERS      (NODE5 NODE3)
   WORDAFTER      (A BIG RED BLOCK)
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (ACTV IMPER TOPLEVEL MAJOR CLAUSE)

>>>
LISTENING---> SHOW TREE

NODE-SPECIFICATION: C
(((PICK UP) (ACTV IMPER TOPLEVEL MAJOR CLAUSE)
             NIL
             (((PICK) (VG IMPER) NIL ((PICK (VPRT VB INF TRANS MVB))))
              (UP (PRT))))
 NIL)
*
LISTENING--->
(1 EXIT PARSE (NODE1))
PASSING TRANSP
(1 ENTER PARSE (NG OBJ OBJ1))

[NODE6]
   SEMANTICS      NIL
   DAUGHTERS      NIL
   WORDAFTER      (A BIG RED BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (OBJ1 OBJ NG)

>>>
LISTENING---> 
PASSING ENTERING-NG
PASSING NGSTART
PASSING LOOK
PASSING DET
(2 ENTER PARSE (DET))

[NODE7]
   SEMANTICS      T
   DAUGHTERS      WORD
   WORDAFTER     (A BIG RED BLOCK)

   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (DET NS INDEF)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE7)
   WORDAFTER      (BIG RED BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (OBJ1 OBJ NG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING IND
PASSING ADJ
(2 ENTER PARSE (ADJ))

[NODE10]
   SEMANTICS      (OBJECT
                      (MARKERS: (#PHYSOB #BIG)
                       PROCEDURE: ((#MORE #SIZE
                                          ***
                                          (200 200
                                               200)))))
   DAUGHTERS      WORD
   WORDAFTER      (RED BLOCK)
   FIRSTWORD      (BIG RED BLOCK)
   FEATURES       (ADJ)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE10 NODE7)
   WORDAFTER      (RED BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (INDEF NS DET OBJ1 OBJ NG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING ADJ
(2 ENTER PARSE (ADJ))

[NODE11]
   SEMANTICS      (#COLOR #RED)
   DAUGHTERS      WORD
   WORDAFTER      (BLOCK)
   FIRSTWORD      (RED BLOCK)
   FEATURES       (ADJ)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE11 NODE10 NODE7)
   WORDAFTER      (BLOCK)
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (INDEF NS DET OBJ1 OBJ NG)

>>>
LISTENING---> SHOW CURRENT

A BIG RED
*
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING ADJ
(2 ENTER PARSE (ADJ))
(2 EXIT PARSE NIL)
PASSING CLASF
(2 ENTER PARSE (VB ING (CLASF)))
(2 EXIT PARSE NIL)
(2 ENTER PARSE (VB EN (CLASF)))
(2 EXIT PARSE NIL)
(2 ENTER PARSE (CLASF))
(2 EXIT PARSE NIL)
PASSING NOUN
(2 ENTER PARSE (NOUN))

[NODE12]
   SEMANTICS      (OBJECT
                      (MARKERS: (#MANIP
                                 #RECTANGULAR)
                       PROCEDURE: ((#IS *** #BLOCK))))
   DAUGHTERS      WORD
   WORDAFTER      NIL
   FIRSTWORD      (BLOCK)

   FEATURES       (NOUN NS)

>>>
LISTENING---> 

[NODE6]
   PARENT         (NODE1)
   SEMANTICS      NIL
   DAUGHTERS      (NODE12 NODE11 NODE10 NODE7)
   WORDAFTER      NIL
   FIRSTWORD      (A BIG RED BLOCK)
   FEATURES       (INDEF NS DET OBJ1 OBJ NG)

>>>
LISTENING---> 
(2 EXIT PARSE (NODE6))
PASSING SMNG
(1 ENTER CALLSM ((SMNG1)))

[OSS1]
   OSSNODE=       OSS1
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   DETERMINER=    (NS INDEF NIL)

>>>
LISTENING---> SH CU

A BIG RED BLOCK
*
LISTENING---> 

[OSS2]
   OSSNODE=       OSS2
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   RELATIONS=     ((#IS OSS1 #BLOCK))
   DETERMINER=    (NS INDEF NIL)
   PLAUSIBILITY= 0
   SYSTEMS=       (#SHAPES #PHYSOB #THING #SYSTEMS)
   MARKERS=       (#SHAPES #RECTANGULAR
                           #SYSTEMS
                           #THING
                           #PHYSOB
                           #MANIP)

>>>
LISTENING---> SHOW MA #SHAPES

#SHAPES
*
LISTENING---> SHOW MA #PHYSOB

#PHYSOB
       #BOX
       #CONSTRUCT
               #STACK
               #ROW
       #HAND
       #MANIP
       #TABLE
*
LISTENING---> 

[OSS3]
   OSSNODE=       OSS3
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   RELATIONS=     ((#COLOR OSS2 #RED)
                   (#IS OSS1 #BLOCK))
   DETERMINER=    (NS INDEF NIL)
   PLAUSIBILITY=  0
   SYSTEMS=     (#SPECTRUM #SHAPES
                           #PHYSOB
                           #THING
                           #SYSTEMS)
   MARKERS=       (#SPECTRUM #RED
                             #SHAPES
                             #RECTANGULAR
                             #SYSTEMS
                             #THING
                             #PHYSOB
                             #MANIP)

>>>
LISTENING---> 

[OSS4]
   OSSNODE=       OSS4
   PARSENODE=     (NODE6)
   VARIABLE=      X1
   RELATIONS=     ((#MORE #SIZE OSS3 (200 200 200))
                   (#COLOR OSS2 #RED)
                   (#IS OSS1 #BLOCK))
   DETERMINER=    (NS INDEF NIL)
   PLAUSIBILITY= 0
   SYSTEMS=       (#SPECTRUM #SHAPES
                             #PHYSOB
                             #THING
                             #SYSTEMS)
   MARKERS=       (#BIG #SPECTRUM
                        #RED
                        #SHAPES
                        #RECTANGULAR
                        #SYSTEMS
                        #THING
                        #PHYSOB
                        #MANIP)

>>>
LISTENING---> 
(1 EXIT CALLSM (OSS4))
PASSING RETSM
(1 ENTER CALLSM ((SMNG2)))
(1 EXIT CALLSM (OSS4))
PASSING RETURN

[NODE6]
   HEAD   (NODE12 NODE11 NODE10 NODE7)
   PARENT         (NODE1)
   SEMANTICS      (OSS4)
   DAUGHTERS      (NODE12 NODE11 NODE10 NODE7)
   WORDAFTER      NIL
   FIRSTWORD      (A BIG RED BLOCK)

   FEATURES       (NG OBJ OBJ1 DET NS INDEF)

>>>
LISTENING---> 

[NODE1]
   MVB            (PICK-UP)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      NIL
   DAUGHTERS      (NODE6 NODE5 NODE3)
   WORDAFTER      NIL
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (PRT ACTV
                       IMPER
                       TOPLEVEL
                       MAJOR
                       CLAUSE)

>>>
LISTENING---> SHOW WORD PICK-UP

[PICK-UP]
   FEATURES       (COMBINATION TRANS)
   SEMANTICS      ((TRANS
                    (RELATION
                        (RESTRICTIONS: (((#ANIMATE))
                                        ((#MANIP)))
                         MARKERS: (#EVENT)
                         PROCEDURE: ((#EVAL
                                      (COND
                                       ((MEMQ (NUMBER? SMOB1)
                                              '(1 NS))
                                        '(#PICKUP #2 *TIME))
                                       ('(#PUTIN
                                          #2
                                          :BOX
                                          *TIME)))))))))
   ROOT   (PICK UP)

>>>
LISTENING---> 
*
LISTENING---> 
(1 EXIT PARSE (NODE6))
PASSING OBB
PASSING FQPRT
PASSING ONT
(1 ENTER CALLSM ((SMCL1)))

[RSS1]
   PARSENODE=     (NODE1)
   RSSNODE=       RSS1
   VARIABLE=      EVX1
   RELATIONS=     ((#PICKUP OSS4 TSS1))
   PLAUSIBILITY= 0
   SYSTEMS=       (#SYSTEMS)
   MARKERS=       (#SYSTEMS #EVENT)

>>>
LISTENING---> 
(1 EXIT CALLSM (RSS1))
PASSING RETSM
(1 ENTER CALLSM ((SMCL2)))
(1 EXIT CALLSM (NODE6 NODE5 NODE3))
PASSING RETURN

[NODE1]
   OBJ1   (NODE6 NODE5 NODE3)
   MVB            (PICK-UP)
   TIME   TSS1
   PARENT         NIL
   SEMANTICS      (RSS1)
   DAUGHTERS      (NODE6 NODE5 NODE3)
   WORDAFTER      NIL
   FIRSTWORD      (PICK UP A BIG RED BLOCK)
   FEATURES       (CLAUSE MAJOR
                          TOPLEVEL
                          IMPER
                          ACTV
                          PRT
                          TRANS)

>>>
LISTENING---> SHOW TREE C

(((PICK UP A BIG RED BLOCK)
  (CLAUSE MAJOR TOPLEVEL IMPER ACTV PRT TRANS)
  (RSS1)
  (((PICK) (VG IMPER) NIL ((PICK (VPRT VB INF TRANS MVB))))
   (UP (PRT))
   ((A BIG RED BLOCK)
    (NG OBJ OBJ1 DET NS INDEF)
    (OSS4)
    ((A (DET NS INDEF)) (BIG (ADJ)) (RED (ADJ)) (BLOCK (NOUN NS))))))
 NIL)
*
LISTENING---> 

(THAND (THGOAL (#PICKUP OSS4) (THDBF MUMBLE) (THUSE TC-2))
        (VALUEPUT)
        (SETQ SUCCESS T)
        (SETQ PLAN2 PLAN))
>>>  FOR PLANNER
LISTENING---> 
>>>  ENTERING FINDCHOOSE EXPR (OSS X ANS2)

*
LISTENING---> SHOW VALUE OSS

OSS4
*
LISTENING---> 
>>>  EXITING FINDCHOOSE
LISTENING---> SHOW VALUE X

:B6
*
LISTENING---> SHOW PLANNER

(ASSERTIONS THEOREM SCENE)
WHICH OPTION? ASSERTIONS
ATOM: ?

SHOW ALL ASSERTIONS WHICH CONTAIN THE GIVEN ATOM
ATOM: :B6

(((#COLOR :B6 #RED)) ((#SHAPE :B6 #RECTANGULAR))
                      ((#SUPPORT :B6 :B7))
                      ((#AT :B6 (0 300 0)))
                      ((#IS :B6 #BLOCK))
                      ((#MANIP :B6))
                      ((#SUPPORT :TABLE :B6)))
*
LISTENING---> TELL PLANNER ON

*
LISTENING---> 
TRYING GOAL G1 (#PICKUP :B6)
>>>
LISTENING---> 
ENTERING THEOREM TC-PICKUP
TRYING GOAL G2 (#GRASP :B6)
>>>
LISTENING---> SHOW THEOREM TC-PICKUP

(THCONSE (X (WHY (EV)) EV) (#PICKUP $?X)
          (MEMORY)

          (THGOAL (#GRASP $?X) (THUSE TC-GRASP))
          (THGOAL (#RAISEHAND) (THNODB) (THUSE TC-RAISEHAND))
          (MEMOREND (#PICKUP $?EV $?X)))
*
ENTERING THEOREM TC-GRASP
TRYING GOAL G3 (#GRASPING :B6)
G3 FAILED
TRYING GOAL G4 (#CLEARTOP :B6)
ENTERING THEOREM TC-CLEARTOP
TRYING GOAL G5 (#SUPPORT :B6 ?)
G5 SUCCEEDED ((#SUPPORT :B6 :B7))
TRYING GOAL G6 (#SUPPORT :B6 (THNV Y))
G6 SUCCEEDED ((#SUPPORT :B6 :B7))
TRYING GOAL G7 (#GET-RID-OF :B7)
ENTERING THEOREM TC-GET-RID-OF
TRYING GOAL G10 (#FINDSPACE :TABLE (200 200 200) :B7 (THNV Y))
ENTERING THEOREM TC-FINDSPACE
TC-FINDSPACE SUCCEEDED THNOVAL
G10 SUCCEEDED
ENTERING THEOREM TC-PUT
TRYING GOAL G12 (#GRASP :B7)
ENTERING THEOREM TC-GRASP

TRYING GOAL G13 (#GRASPING :B7)
G13 FAILED
TRYING GOAL G14 (#CLEARTOP :B7)
G14 SUCCEEDED ((#CLEARTOP :B7))
TRYING GOAL G15 (#GRASPING (THNV Y))
G15 FAILED
TRYING GOAL G16 (#MOVEHAND2 (100 340 500))
ENTERING THEOREM TC-MOVEHAND2
TC-MOVEHAND2 SUCCEEDED THNOVAL
G16 SUCCEEDED (#MOVEHAND2 (100 340 500))
ASSERTING A17 (#GRASPING :B7)
A17 SUCCEEDED
ASSERTING A20 (#GRASP E5 :B7)
A20 SUCCEEDED
TC-GRASP SUCCEEDED THNOVAL
G12 SUCCEEDED (#GRASP :B7)
>>>
LISTENING---> SHOW PR E5

[E5]
   TYPE   #GRASP
   THASSERTION    (NIL (2 (3 1 ((#GRASP E5 :B7)))))
   END            1
   WHY            E4
   START          0

*
TRYING GOAL G21 (#MOVEHAND (600 374 200))
ENTERING THEOREM TC-MOVEHAND
TRYING GOAL G22 (#GRASPING (THV X))
G22 SUCCEEDED ((#GRASPING :B7))

TRYING GOAL G23 (#AT :B7 (THNV W))
G23 SUCCEEDED ((#AT :B7 (0 240 300)))
ERASING E24 (#AT :B7 (0 240 300))
ENTERING THEOREM TE-SUPP
TRYING GOAL G25 (#SUPPORT :B7 (THNV Y))
G25 FAILED
TRYING GOAL G26 (#SUPPORT (THNV Y) :B7)
G26 SUCCEEDED ((#SUPPORT :B6 :B7))
ERASING E27 (#SUPPORT :B6 :B7)
E27 SUCCEEDED
TRYING GOAL G30 (#PART :B7 (THNV Y))
G30 FAILED
TE-SUPP SUCCEEDED THNOVAL
ENTERING THEOREM TE-CONTAIN
TRYING GOAL G31 (#CONTAIN (THNV Y) :B7)
G31 FAILED
TE-CONTAIN FAILED
E24 SUCCEEDED
ASSERTING A32 (#AT :B7 (500 274 0))
ENTERING THEOREM TA-AT
TA-AT SUCCEEDED THNOVAL
ENTERING THEOREM TA-SUPP
TRYING GOAL G33 (#MANIP :TABLE)
G33 FAILED
ASSERTING A34 (#SUPPORT :TABLE :B7)
A34 SUCCEEDED
TRYING GOAL G35 (#CLEARTOP :TABLE)
G35 FAILED
TA-SUPP SUCCEEDED THNOVAL
ENTERING THEOREM TA-CONTAIN
TRYING GOAL G36 (#MANIP :B7)
G36 SUCCEEDED ((#MANIP :B7))
TRYING GOAL G37 (#SUPPORT (THV Y) :B7)
G37 SUCCEEDED ((#SUPPORT :TABLE :B7))
TRYING GOAL G40 (#IS :TABLE #BOX)
G40 FAILED
TRYING GOAL G41 (#CONTAIN (THV Z) :TABLE)
G41 FAILED
G37 FAILED
G36 FAILED
TA-CONTAIN FAILED

A32 SUCCEEDED
TRYING GOAL G42 (#MOVEHAND2 (600 374 200))
ENTERING THEOREM TC-MOVEHAND2
TC-MOVEHAND2 SUCCEEDED THNOVAL
G42 SUCCEEDED (#MOVEHAND2 (600 374 200))
TRYING GOAL G43 (#SUPPORT (THV Y) :B7)
G43 SUCCEEDED ((#SUPPORT :TABLE :B7))
TC-MOVEHAND SUCCEEDED THNOVAL
G21 SUCCEEDED (#MOVEHAND (600 374 200))
TRYING GOAL G44 (#UNGRASP)
ENTERING THEOREM TC-UNGRASP
TRYING GOAL G45 (#GRASPING (THV X))
G45 SUCCEEDED ((#GRASPING :B7))
TRYING GOAL G46 (#SUPPORT ? :B7)
G46 SUCCEEDED ((#SUPPORT :TABLE :B7))
ERASING E47 (#GRASPING :B7)
E47 SUCCEEDED
ASSERTING A50 (#UNGRASP E6 :B7)
A50 SUCCEEDED
TC-UNGRASP SUCCEEDED THNOVAL
G44 SUCCEEDED (#UNGRASP)
TC-PUT SUCCEEDED THNOVAL
G11 SUCCEEDED (#PUT :B7 (500 274 0))
ASSERTING A51 (#GET-RID-OF E4 :B7)
A51 SUCCEEDED
TC-GET-RID-OF SUCCEEDED THNOVAL
G7 SUCCEEDED (#GET-RID-OF :B7)
TRYING GOAL G52 (#SUPPORT :B6 (THNV Y))
G52 FAILED
ASSERTING A53 (#CLEARTOP :B6)
A53 SUCCEEDED
ASSERTING A54 (#CLEARTOP E3 :B6)
A54 SUCCEEDED
TC-CLEARTOP SUCCEEDED THNOVAL
G4 SUCCEEDED (#CLEARTOP :B6)
>>>
LISTENING---> SHOW EVENT E3

E3  (#CLEARTOP E3 :B6)  --  TIME0 TO 3
    E6  (#UNGRASP E6 :B7)  --  TIME2 TO 2
    E4  (#GET-RID-OF E4 :B7)  --  TIME0 TO 3
         E5  (#GRASP E5 :B7)  --  TIME0 TO 1
*
TRYING GOAL G55 (#GRASPING (THNV Y))
G55 FAILED
TRYING GOAL G56 (#MOVEHAND2 (100 440 300))
ENTERING THEOREM TC-MOVEHAND2
TC-MOVEHAND2 SUCCEEDED THNOVAL
G56 SUCCEEDED (#MOVEHAND2 (100 440 300))
ASSERTING A57 (#GRASPING :B6)
A57 SUCCEEDED
ASSERTING A60 (#GRASP E2 :B6)
A60 SUCCEEDED
TC-GRASP SUCCEEDED THNOVAL
G2 SUCCEEDED (#GRASP :B6)
TRYING GOAL G61 (#RAISEHAND)
ENTERING THEOREM TC-RAISEHAND
TRYING GOAL G62 (#MOVEHAND (100 440 1000))
>>>
LISTENING---> TELL PL

(INPUT ACTION THEOREM ASSERTIONS)
WHICH OPTION? ACTION
ON OR OFF? OFF

*
LISTENING---> TELL ACTION (THASSERT)

*
ASSERTING A63 (#AT :B6 (0 300 500))
A63 SUCCEEDED
G62 SUCCEEDED (#MOVEHAND (100 440 1000))
ASSERTING A64 (#RAISEHAND E7)
A64 SUCCEEDED  SUCCEEDED (#RAISEHAND)
ASSERTING A65 (#PICKUP E1 :B6)
A65 SUCCEEDED
TC-PICKUP SUCCEEDED THNOVAL

G1 SUCCEEDED (#PICKUP :B6)
>>>
LISTENING---> SHOW EVENT E1

E1  (#PICKUP E1 :B6)  --  TIME 0 TO 5
    E2  (#GRASP E2 :B6)  --  TIME 0 TO 4
         E3  (#CLEARTOP E3 :B6)  --  TIME 0 TO 3
              E4  (#GET-RID-OF E4 :B7)  --  TIME 0 TO 3
                   E5  (#GRASP E5 :B7)  --  TIME 0 TO 1
              E6  (#UNGRASP E6 :B7)  --  TIME 2 TO 2
    E7  (#RAISEHAND E7)  --  TIME 4 TO 5
(EE LISTENING---> 

[ANS1]
   ACTION=        ((MOVETO 100 340 500) (GRASP ':B7)
                                        (MOVETO 600 374 200)
                                        (UNGRASP)
                                        (MOVETO 100 440 300)
                                        (GRASP ':B6)
                                        (MOVETO 100 440 1000)
                                        (SAY OK))
   ANSRSS=        RSS1
   ANSNODE=       ANS1
   PLAUSIBILITY= 0

*
MOVETO (100 340 500)
GRASP :B7
MOVETO (600 374 200)
UNGRASP
MOVETO (100 440 300)
GRASP :B6
MOVETO (100 440 1000)OK .

APPENDIX 6
----------

THIS SHOWS THE PROCESS OF ADDING WORDS, THEOREMS AND ASSERTIONS
TO SHRDLU'S DATA BASE AND THEIR INTERACTION IN THE UNDERSTANDING
OF NEW SENTENCES.
==================================================================

READY

>>>
LISTENING---> TELL WORD

WORD: SKUNK

NOUN OR VERB? NOUN
MARKERS: (#ANIMAL)

PROCEDURE: ?

LIST OF EXPRESSIONS TO BE PUT IN PLANNER GOALS TO DESCRIBE OBJECT -
USE *** TO REPRESENT OBJECT BEING DESCRIBED BY WORD
PROCEDURE: ((*** #IS-A #SKUNK))

LISTENING---> SHOW WORD SKUNK

[SKUNK]
   SEMANTICS      ((NOUN
                    (OBJECT
                        (MARKERS: (#ANIMAL)
                         PROCEDURE: ((*** #IS-A
                                          #SKUNK))))))
   FEATURES       (NOUN NS)

>>>
LISTENING---> TELL MA

MARKER: #ANIMAL

PARENT: #PHYSOB

*
LISTENING---> TELL WORD ANIMAL

WORD: ANIMAL

NOUN OR VERB? NOUN
MARKERS: (#ANIMAL)

PROCEDURE: ((*** #IS-A #ANIMAL))

>>>
LISTENING---> TELL THEOREM

THEOREM-TYPE: ?

ANTECEDENT, ERASING, OR CONSEQUENT THEOREM
THE CHOICES ARE:
(THANTE THERASING THCONSE)
THEOREM-TYPE: THANTE
VARIABLE-LIST: (X)

PATTERN: ($?X #IS-A #SKUNK)

BODY: ((THASSERT  ($?X #IS-A #ANIMAL)))

*
LISTENING---> SHOW THEOREM THEOREM1

 (THANTE (X)
($?X #IS-A #SKUNK) (THASSERT ($?X #IS-A #ANIMAL)))
*
LISTENING---> TELL PLANNER ON

*
LISTENING---> TELL AS

ASSERTION: (=FLOWER #IS-A #SKUNK)

ASSERTING A66 (=FLOWER #IS-A #SKUNK)
ENTERING THEOREM THEOREM1
ASSERTING A67 (=FLOWER #IS-A #ANIMAL)
A67 SUCCEEDED
THEOREM1 SUCCEEDED THNOVAL
A66 SUCCEEDED
*
LISTENING---> SHOW AS =FLOWER

(((=FLOWER #IS-A #ANIMAL)) ((=FLOWER #IS-A #SKUNK)))
*
LISTENING---> TELL WORD

WORD: STINK

NOUN OR VERB? VERB
MARKERS: (#EVENT)

TRANSITIVE OR INTRANSITIVE? INTRANSITIVE
RESTRICTIONS ON SUBJECT: (#PHYSOB)

LISTENING--->

PROCEDURE:  ?
LIST OF EXPRESSIONS TO BE PUT INTO PLANNER GOALS TO DESCRIBE ACTION
OR RELATION -- USE #1 FOR SUBJECT, #2 FOR OBJECT.
PROCEDURE:  ((#ODERIFEROUS #1))
*
LISTENING---> TELL TELLABLE #ODERIFEROUS

*
LISTENING---> TELL PA OFF

*
LISTENING---> GO
QUIT
READY
ALL SKUNKS STINK.
[1]

[TSS1]
   TSSNODE=       TSS1

>>>
LISTENING---> 

[OSS1]
   OSSNODE=       OSS1
   PARSENODE=     (NODE3)
   VARIABLE=      X1
   DETERMINER=    (NPL NDET NIL)

>>>
LISTENING---> 

[OSS2]
   OSSNODE=       OSS2
   PARSENODE=     (NODE3)
   VARIABLE=      X1
   RELATIONS=     ((OSS1 #IS-A #SKUNK))
   DETERMINER=    (NPL NDET NIL)
   PLAUSIBILITY= 0
   SYSTEMS=       (#PHYSOB #THING #SYSTEMS)
   MARKERS=       (#SYSTEMS #THING #PHYSOB #ANIMAL)

>>>
LISTENING---> 

[TSS2]
   TSSNODE=       TSS2

>>>
LISTENING---> 

[RSS1]
   PARSENODE=     (NODE1)
   RSSNODE=       RSS1
   REL=   OSS2
   VARIABLE=      EVX1
   RELATIONS=     ((#ODERIFEROUS OSS2))
   PLAUSIBILITY= 0
   RELMARKERS=    ((#SYSTEMS #THING #PHYSOB #ANIMAL)
                   (#PHYSOB #THING #SYSTEMS))
   SYSTEMS=       (#SYSTEMS)
   MARKERS=       (#SYSTEMS #EVENT)

>>>
LISTENING---> 

[ANS1]
   ACTION=        ((SAY I UNDERSTAND)
                   (THADD 'THEOREM2 NIL))
   ANSRSS=        RSS1
   ANSNODE=       ANS1
   PLAUSIBILITY= 0

>>>
LISTENING---> SH TH THEOREM2

(THCONSE (X1) (#ODERIFEROUS $?X1)
          (THGOAL ($?X1 #IS-A #SKUNK) (THDBF MUMBLE))

          )
*
LISTENING---> TELL SE

DO SEMANTIC ANALYSIS? YES
SHOW BUILDING OF SEMANTIC STRUCTURES? NO
*
LISTENING---> 
* I UNDERSTAND .

>>>
LISTENING---> 
READY
DOES ANY ANIMAL STINK?

(THAND (THFIND ALL
                $?X2
                (X2)
                (THGOAL ($?X2 #IS-A #ANIMAL) (THDBF MUMBLE))
                (THGOAL (#ODERIFEROUS $?X2) (THDBF MUMBLE)
                                            (THTBF THTRUE)))
        (THPUTPROP 'X2 THVALUE 'BIND))
>>>  FOR PLANNER
LISTENING---> TELL PLANNER ON

*
LISTENING---> 
TRYING GOAL G70 ((THV X2) #IS-A #ANIMAL)
>>>
LISTENING---> 
G70 SUCCEEDED ((=FLOWER #IS-A #ANIMAL))
>>>
LISTENING---> 
TRYING GOAL G71 (#ODERIFEROUS =FLOWER)
>>>
LISTENING---> 
ENTERING THEOREM THEOREM2
TRYING GOAL G72 (=FLOWER #IS-A #SKUNK)
>>>
LISTENING---> 
G72 SUCCEEDED ((=FLOWER #IS-A #SKUNK))
>>>
LISTENING---> 
THEOREM2 SUCCEEDED THNOVAL
G71 SUCCEEDED (#ODERIFEROUS =FLOWER)
>>>
LISTENING---> 
* YES,FLOWER.

§ anno/winograd/minih

      ****** SHRDLU States and What They Want    *****

READY   (types out READY)
     English sentence ended with punctuation,
        or <alt-mode> <alt-mode> to go to COMMAND
COMMAND  (types out >>>)
     Command terminated by <CR> <CR>
      or T,NIL or <alt>-P to proceed program
      or GO<CR> to prepare for new sentence
      or HELP <CR> for help messages (of which this is one)
REQUEST  (types out request)
   CHOOSE (request ends with ?)
     Enough of any choice to specify it, followed by
          <period>
        or ?<CR> to see information and choices
        or QUIT<CR> to abort command
   SPECIFY   (request ends with :)
     Name or list, ended with <CR>
        or just <LF> for default value (if appropriate)
        or QUIT<CR> to abort command
        or ?<CR> for info
Abbreviate commands by first two letters.

@

§ anno/winograd/files

********************************************************************************
*
*       This describes the files maintained on the directory SHRDLU which
*          go into making up the Natural Language Understanding system designed
*          by Terry Winograd (currently at Stanford University)
*
*       The files are ordered alphabeticly within the directory, but are
*          described here in related groups, and are listed by the printing
*          routine in similar groups
*
********************************************************************************

-- Files which support the BLOCKS micro-world

  BLOCKP >  Microplanner code for essentially all the functions of the
             BLOCKS world. - there are some supplementary materials at the
             end of the dictionary and data files

  BLOCKL >  LISP code to support the microplanner code of BLOCKS. In particular
             this includes the routine for finding space on an object and
             for maintaining the history list.

  DATA >    The initial contents (assertions) of the BLOCKS world's
             microplanner data base. Plus a few addittional function calls
             for initializing all other aspects of the micro-world.

  PLNR 180  This is a complete EXPR code version of MICROPLANNER. Calls to
             MICROPLANNER in SHRDLU are always done by calling the function
             THVAL2, contained in the semantics files, rather than the usual
             call to THVAL. PLNR 180 can be loaded independantly and run
             as a toplevel MICROPLANNER if desired. A compiled version of
             the files is availlable as PLNR FASL on SHRDLU, and TS PLNR on
             SYS should point to a core dump of a toplevel MICROPLANNER

  THTRAC 18 This is a file of tracing routines designed for tracing opperations
             during the evaluation of MICROPLANNER code

--  Toplevel files for the language understanding system --

  SYSCOM >  Contains the toplevel function SHRDLU as well as usefull extentions
             to LISP, special printing functions, break functions, garbage
             collection functions etc.

  MORPHO >  Does all the morphological analysis required in the system via
             the function ETAOIN which is also the function called by SHRDLU
             to read in a user's sentence from the console.

  SHOW >    This is the "Show - Tell" user package developed for use at a
             workshop at CMU in 1972. It provides a convenient way for a
             largely naieve user to interogate and change the data structures
             used in the system. Consult the MANUAL CMU for further details

  SETUP >   A loose ends file for setting global variables, initializing
             trace routines, and setting up the initial environment. It is
             intended to be the final file read into the system before
             making a core dump.

--  Files concerned with syntax

  PROGMR >  This file contains all the functions that comprise the programmar
             language fro writing grammars. It expects to be entered from a
             toplevel call to PARSE as done in SHRDLU, and executes grammar
             programs via an intermediary function, APPLY-GRAMMAR, to permit
             differing opperations in the cases of compiled and intrepreted
             code.

  GINTER >  "Grammar Interpreter" - Contains the calling sequence for use when
             running an uncompiled grammar. A precompiling pass is necessary
             on grammar functions to remove extra levels of function calls
             and to incorporate the calling program used in GINTER to eliminate
             GOTO's out of scope.

  GRAMAR >  The entire syntactic recognition grammar for the system. See
             A.I. MEMO 282 "Grammar for the People" for comprehensive flowcharts
             of the functions in this file.

  DICTIO >  Specifications and entrys for all of SHRDLU's vocabulary and
             related atomic symbols. This file contains all aspects of their
             definitions: syntax, semantics, macro-expanders, etc.

-- Semantics files --

  SMSPEC >  "Semantic Specialists"  This contains all the functions which know
             how to interpret the syntactic forms that will be found by the
             grammar: noun groups, relative clauses, pronouns, etc.. All calls
             to these functions come from the syntactic routines through the
             function SMCALL which is in PROGMR.

  SMUTIL >  Utility routines used by all the semantic functions for building
             nodes, evaluating definitions, marker checks, backreference maintanence,
             quantifier adjusting, building MIRCOPLANNER code.

  SMASS >   Access functions that are used by the two previous files to grovel
             through their data structures while retaining their perspicuity.

-- Answering --

  NEWANS >  "new answer" Code for finding the answers to questions, absorbing
             new information, and following commands, then fromulating the
             appropriate English answers. This code is extensively interfaced
             with MICROPLANNER.

-- Documentation --

  MANUAL CMU, MANNEW >, HELP DOC, MINIH DOC

            These files are unfortunately all the documentation availlable on the
             care and feeding of the system appart from Winograd's book (which is
             far and away the best reference to the details of its opperation).
             MANUAL CMU, as its name implys, was developed for the workshop at
             CMU and it the most through overall guide. HELP DOC and MINIH DOC
             are essentially excerpts from it which were intended as online references.
             MANNEW is grossly incomplete, but it is also the most up to date, and
             covers initial ussage of hte system.

  .INDEX >  This is a through cross-reference for all the LISP code used in the system.
             It lists functions and variables alphabeticly with listings of their
             interactions with the rest of the code (functions called, variables
             bound ...). Also of interest is the file LISP >, which alphabeticly lists
             all MACLISP functions ever invoked in the code and the functions that they
             were invoked from.

-- At the end of the directory, the files whose first names are "Z" are the origninal
    files of the system circa January 1971. They use the old list oriented data
    structures and are essentially uncommented. However they can often be of use
    when trying to see if alternate formulations of some algorithm were ever
    considered.

§ anno/winograd/blockp

################################################################

        BLOCKP >

    MICRO-PLANNER CODE FOR THE "BLOCKS" MICRO-WORLD
################################################################

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
                  ((THVSETQ $_Y (MAKESYM (QUOTE STACK)))
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
          (THGOAL (#CHOOSE $?Y $_YY $E (GET $?X (QUOTE CHOOSE)))
              (THUSE TC-CHOOSE))
          (THGOAL ($?X $?YY) (THTBF THTRUE)))
     THEOREM)

(DEFPROP TC-3
     (THCONSE (X Y Z YY ZZ)
          ($?X $?Y $?Z)
          (THGOAL (#CHOOSE $?Y $_YY $E (GET $?X (QUOTE CHOOSE)))
              (THUSE TC-CHOOSE))
          (THGOAL (#CHOOSE $?Z $_ZZ $E (GET $?X (QUOTE CHOOSE2)))
              (THUSE TC-CHOOSE))
          (THGOAL ($?X $?YY $?ZZ) (THTBF THTRUE)))
     THEOREM)

(DEFPROP TC-ASMUCH
     (THCONSE (MEASURE X Y)
          (#ASMUCH MEASURE $?X $?Y)
          (THVSETQ $_MEASURE (GET $?MEASURE (QUOTE MEASFN)))
          (NOT (LESSP ($?MEASURE $?X) ($?MEASURE $?Y))))
     THEOREM)

(DEFPROP TC-BELONG
     (THCONSE (X Y)
          (#BELONG $?X $?Y)
          (THAMONG $?Y (QUOTE (:SHRDLU)))
          (THGOAL (#PHYSOB $?X) (THUSE TC-PHYSOB)))
     THEOREM)

(DEFPROP TC-CALL
     (THCONSE (X Y Z)
          (#CALL $?X $?Y)
          (THCOND ((THGOAL (#CALL $_Z $?Y))
               (PRINT $?Y)
               (NOT (PRINT (QUOTE NAME-ALREADY-USED))))
              ((THASSERT (#CALL $?X $?Y))
               (THASSERT (#IS $?Y #NAME))
               (#PROPDEFINE $?Y)
               (OR DOIT (SETQ PLAN (CONS T PLAN))))))
     THEOREM)

(DEFPROP TC-CHOOSE
 (THCONSE
  (X Y Z W)
  (#CHOOSE $?X $?Y $?Z)
  (THCOND
   ((AND (THASVAL $?X) (NOT (OSS? $?X))) (THSETQ $_Y $?X))
   ((THASVAL $?X)
    (OR (NULL DISCOURSE)
    (THPUTPROP (VARIABLE? $?X) $?X (QUOTE NG)))
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
          GO
          (THCOND ((THGOAL (#SUPPORT $?X $_Y))
               (THGOAL (#GET-RID-OF $?Y)
                   (THNODB)
                   (THUSE TC-GET-RID-OF))
               (THGO GO))
              ((THASSERT (#CLEARTOP $?X))
               (MEMOREND (#CLEARTOP $?EV $?X))
               (THSUCCEED THEOREM))))
     THEOREM)

(DEFPROP TC-EXISTS (THCONSE (X) (#EXISTS $?X) (THSUCCEED)) THEOREM)

(DEFPROP TC-FINDSPACE
     (THCONSE (SURF SIZE OBJ SPACE)
          (#FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE)
          (THOR (AND (NOT (MEMQ $?SURF (QUOTE (:BOX :TABLE))))
                 (NOT (GET (QUOTE #NOCLEAR) (QUOTE THASSERTION)))
                 (THSETQ $_SPACE (FINDSPACE (QUOTE CENTER)
                            $?SURF
                            $?SIZE
                            $?OBJ)))
            (AND (OR (EQ $?SURF (QUOTE :BOX))
                 (AND (NOT (EQ $?SURF (QUOTE :TABLE)))
                      (GET (QUOTE #NOCLEAR)
                       (QUOTE THASSERTION))))
                 (THSETQ $_SPACE (FINDSPACE (QUOTE PACK)
                            $?SURF
                            $?SIZE
                            $?OBJ)))
            (THSETQ $_SPACE (FINDSPACE (QUOTE RANDOM)
                           $?SURF
                           $?SIZE
                           $?OBJ))))
     THEOREM)

(DEFPROP TC-GET-RID-OF
     (THCONSE (X Y (WHY (EV)) EV)
          (#GET-RID-OF $?X)
          (OR NOMEM (THVSETQ $_EV $?WHY))
          UP
          (THCOND ((NULL $?X))
              ((ATOM $?X)
               (MEMORY)
               (THGOAL (#FINDSPACE :TABLE $E (SIZE $?X) $?X $_Y)
                   (THUSE TC-FINDSPACE))
               (THGOAL (#PUT $?X $?Y) (THNODB) (THUSE TC-PUT))
               (MEMOREND (#GET-RID-OF $?EV $?X)))
              ((THGOAL (#GET-RID-OF $E (CAR $?X))
                   (THUSE TC-GET-RID-OF))
               (OR (THSETQ $_X (CDR $?X)) (THSUCCEED THEOREM))
               (THGO UP))))
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
                 (THGOAL (#GET-RID-OF $?Y)
                     (THNODB)
                     (THUSE TC-GET-RID-OF))))
              ((THSUCCEED)))
          (THSETQ $_Y (TOPCENTER $?X))
          (THGOAL (#MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2))
          (THASSERT (#GRASPING $?X))
          (MEMOREND (#GRASP $?EV $?X))
          (OR NOMEM
              (THSETQ GRASPLIST (CONS (LIST THTIME $?X) GRASPLIST)))
          (THCOND (DOIT (THOR (GRASP $?X) (AND (UNGRASP) NIL)))
              ((THSETQ PLAN (CONS (LIST (QUOTE GRASP)
                            (LIST (QUOTE QUOTE) $?X))
                          PLAN)))))
     THEOREM)

(DEFPROP TC-LOC
     (THCONSE (X Y Z LOC)
          ($?LOC $?X $?Y $?Z)
          (THOR (THGOAL (#MANIP $?Y)) (THGOAL (#IS $?Y #BOX)))
          (THOR (THGOAL (#MANIP $?Z)) (THGOAL (#IS $?Z #BOX)))
          (NOT (EQ $?Y $?Z))
          (LOCGREATER $?Y $?Z ((LAMBDA (X) (COND ((EQ X (QUOTE #RIGHT))
                              (QUOTE CAR))
                             ((EQ X (QUOTE #BEHIND))
                              (QUOTE CADR))
                             ((EQ X (QUOTE #ABOVE))
                              (QUOTE CADDR))
                             ((ERT TC-LOC))))
                       $?X)))
     THEOREM)

(DEFPROP TC-MAKESPACE
     (THCONSE (SURF SIZE OBJ SPACE X (WHY (EV)) EV)
          (#FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE)
          (THNOT (THGOAL (#IS $?SURF #BOX)))
          (MEMORY)
          TAG
          (THAND (THGOAL (#SUPPORT $?SURF $_X))
             (THGOAL (#GET-RID-OF $?X) (THUSE TC-GET-RID-OF)))
          (THOR (THGOAL (#FINDSPACE $?SURF $?SIZE $?OBJ $?SPACE)
                (THUSE TC-FINDSPACE))
            (THGO TAG))
          (MEMOREND (#MAKESPACE $?EV $?SURF)))
     THEOREM)

(DEFPROP TC-MORE
     (THCONSE (MEASURE X Y)
          (#MORE $?MEASURE $?X $?Y)
          (THVSETQ $_MEASURE (GET $?MEASURE (QUOTE MEASFN)))
          (GREATERP ($?MEASURE $?X) ($?MEASURE $?Y)))
     THEOREM)

(DEFPROP TC-MOVEHAND
 (THCONSE
  (X Y W Z)
  (#MOVEHAND $?Y)
  (THCOND
   ((EQUAL HANDAT $?Y) (THSUCCEED THEOREM))
   ((THGOAL (#GRASPING $?X))
    (THVSETQ $_Z
         (PROG (X Y)
           (SETQ X (ATAB $?X))
           (AND (CLEAR (SETQ Y (DIFF $?Y
                         (TCENT (QUOTE (0 0 0)) (CADDR X))))
                   (LIST (CAADDR X)
                     (CADADR (CDR X))
                     (DIFFERENCE 1000 (CADDR Y)))
                   (CAR X))
            (RETURN Y))))
    (THGOAL (#AT $?X $_W))
    (THERASE (#AT $?X $?W) (THUSE TE-SUPP TE-CONTAIN))
    (THASSERT (#AT $?X $?Z) (THUSE TA-AT TA-SUPP TA-CONTAIN))
    (THGOAL (#MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2))
    (OR
     NOMEM
     (THPUTPROP $?X
        (CONS (LIST THTIME
                $?Z
                (CADAR (OR (THVAL (QUOTE (THGOAL (#SUPPORT $?Y
                                       $?X)))
                          (CONS (LIST (QUOTE Y)
                              (QUOTE THUNASSIGNED))
                            THALIST))
                       (QUOTE ((NIL :HAND)))))
                NIL)
              (GET $?X (QUOTE HISTORY)))
        (QUOTE HISTORY))))
   ((THGOAL (#MOVEHAND2 $?Y) (THNODB) (THUSE TC-MOVEHAND2)))))
 THEOREM)

(DEFPROP TC-MOVEHAND2
     (THCONSE (Y LOC)
          (#MOVEHAND2 $?Y)
          (COND ((EQUAL $?Y HANDAT) (THSUCCEED THEOREM))
            ((AND (LESSP 37 (CAR $?Y) 1141)
                  (LESSP -1 (CADR $?Y) 1141)
                  (LESSP -1 (CADDR $?Y) 1001))))
          (THVSETQ $_LOC HANDAT)
          (THSETQ HANDAT $?Y)
          (THSETQ THTIME (ADD1 THTIME))
          (THCOND (DOIT (THOR (EVAL (CONS (QUOTE MOVETO) HANDAT))
                      (PROG (ADJUST) (EVAL (CONS (QUOTE MOVETO)
                                 $?LOC)))))
              ((THSETQ PLAN
                   (CONS (CONS (QUOTE MOVETO) $?Y) PLAN)))))
     THEOREM)

(DEFPROP TC-NAME
     (THCONSE (X)
          (#NAME $?X)
          (THVSETQ $_X (LISTIFY $?X))
          (THVSETQ $_X (THFIND ALL
                       $?Y
                       (Y Z)
                       (THAMONG $?Z $?X)
                       (THOR (THGOAL (#CALL $?Z $?Y))
                         (THSETQ $_Y $?Z))))
          (MAPC (QUOTE PRINT) $?X))
     THEOREM)

(DEFPROP TC-NOTICE
     (THCONSE (X)
          (#NOTICE $?X)
          (COND (DOIT (BLINK $?X) (THSUCCEED))
            ((THSETQ PLAN (CONS (LIST (QUOTE BLINK)
                          (LIST (QUOTE QUOTE) $?X))
                        PLAN)))))
     THEOREM)

(DEFPROP TC-ON
     (THCONSE (X Y Z) (#ON $?X $?Y) (THOR (THGOAL (#SUPPORT $?Y $?X))
                          (THAND (THASVAL $?X)
                             (THGOAL (#SUPPORT $_Z $?X))
                             (THGOAL (#ON $?Z $?Y)
                                 (THUSE TC-ON)))))
     THEOREM)

(DEFPROP TC-PACK
     (THCONSE (OBJ SURF BLOCKS PYR X Y)
          (#PACK $?OBJ $?SURF)
          (OR (THVSETQ $_BLOCKS (PACKO $?OBJ (QUOTE #BLOCK))) T)
          (OR (THVSETQ $_PYR (PACKO $?OBJ (QUOTE #PYRAMID))) T)
          GO
          (THCOND ((NULL $?BLOCKS)
               (THCOND ((NULL $?PYR) (THSUCCEED THEOREM))
                   ((THVSETQ $_Y (FINDSPACE (QUOTE PACK)
                                $?SURF
                                (SIZE (CAR $?PYR))
                                (CAR $?PYR)))
                    (THGOAL (#PUT $E (CAR $?PYR) $?Y)
                        (THUSE TC-PUT))
                    (OR (THSETQ $?PYR (CDR $?PYR)) T)
                    (THGO GO))))
              ((THSETQ $_X (CAR $?BLOCKS))
               (THVSETQ $?Y (FINDSPACE (QUOTE PACK)
                           $?SURF
                           (SIZE $?X)
                           $?X))
               (THGOAL (#PUT $?X $?Y) (THUSE TC-PUT))
               (OR (THSETQ $?BLOCKS (CDR $?BLOCKS)) T)
               (THCOND ((THVSETQ $_Y (OR (PACKON $?X $?PYR)
                             (PACKON $?X $?BLOCKS)))
                    (THGOAL (#PUTON $?Y $?X) (THUSE TC-PUTON))
                    (COND ((MEMQ $?Y $?PYR)
                       (THSETQ $_PYR
                           (DELQ $?Y
                             (APPEND $?PYR NIL))))
                      ((THSETQ $_BLOCKS
                           (DELQ $?Y (APPEND $?BLOCKS
                                     NIL))))))
                   ((THSUCCEED)))
               (THGO GO))))
     THEOREM)

(DEFPROP TC-PART
     (THCONSE (X Y Z)
          (#PART $?X $?Y)
          (THGOAL (#IS $?Y #STACK))
          (THGOAL (#CHOOSE $?X $_Z (QUOTE (((THGOAL (#PART $?* $?Y))))))
              (THUSE TC-CHOOSE))
          (OR (NOT (ATOM $?Z)) (THSETQ $_Z (LIST $?Z)))
          GO
          (THCOND ((NULL $?Z) (THSUCCEED))
              ((THGOAL (#PART $E (CAR $?Z) $?Y))
               (OR (THSETQ $_Z (CDR $?Z)) T)
               (THGO GO))
              ((THFAIL))))
     THCONSE)

(DEFPROP TC-PHYSOB
     (THCONSE (X)
          (#PHYSOB $?X)
          (THOR (THGOAL (#MANIP $?X))
            (THAMONG $?X (QUOTE (:BOX :TABLE :HAND)))))
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
     (THCONSE(X)
          (#REFERS $?X)
          (EVAL(LIST 'THSETQ (LIST 'THV $?X)(LIST 'QUOTE (ATOMIFY (GET $?X 'BIND))))))
     THEOREM)

(DEFPROP TC-PUT
     (THCONSE (X Y Z)
          (#PUT $?X $?Y)
          (THCOND ((THASVAL $?Y)
               (THCOND ((ATOM $?Y) (THGOAL (#CHOOSEPLACE $?Y)
                               (THUSE TC-CHOOSEPLACE)))
                   ((THSUCCEED))))
              ((THGOAL (#GET-RID-OF $?X)
                   (THNODB)
                   (THUSE TC-GET-RID-OF))
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
                  (THVAL (QUOTE (THFIND ALL
                            $?W
                            (W)
                            (THGOAL (#ON $?W $?Y))))
                     THALIST)))
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
               (THOR (THGOAL (#FINDSPACE $?Y $E (SIZE $?X) $?X $_Z)
                     (THUSE TC-FINDSPACE))
                 (AND (NULL (GET (QUOTE #NOCLEAR)
                         (QUOTE THASSERTION)))
                      (THGOAL (#FINDSPACE $?Y
                              $E
                              (SIZE $?X)
                              $?X
                              $_Z)
                          (THUSE TC-MAKESPACE))))
               (THGOAL (#PUT $?X $?Z) (THNODB) (THUSE TC-PUT)))
              ((THASSERT (#NOCLEAR))
               (THPROG ((W $?X))
                   UP
                   (THOR (THGOAL (#PUTON $E (CAR $?W) $?Y)
                         (THUSE TC-PUTON))
                     (THFAIL THPROG))
                   (THOR (THSETQ $?W (CDR $?W)) (THRETURN T))
                   (THGO UP))
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
          (THGOAL (#MOVEHAND $E (LIST (CAR HANDAT) (CADR HANDAT) 1000))
              (THNODB)
              (THUSE TC-MOVEHAND))
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
          GO
          (THGOAL (#SUPPORT $_X $?Y))
          (THCOND ((MEMQ $?X (QUOTE (:TABLE :BOX))))
              ((THSETQ $_Y $?X) (THGO GO)))
          (THSETQ $_X (MAKESYM (QUOTE STACK)))
          (THASSERT (#IS $?X #STACK))
          (THASSERT (#EXISTS $?X))
          (THFIND ALL
              $?Z
              (Z)
              (THGOAL (#ON $?Z $?Y) (THUSE TC-ON))
              (THAND (THASSERT (#PART $?Z $?X))
                 (THFINALIZE THAND))))
     THEOREM)

(DEFPROP TC-STACKUP
 (THCONSE
  (X Y BLOCKS PYR (WHY (EV)) EV)
  (#STACKUP $?X)
  (OR (LESSP (APPLY (QUOTE PLUS)
            (MAPCAR (QUOTE (LAMBDA (X) (CADDR (SIZE X)))) $?X))
         1201)
      (NOT (DPRINT2 (QUOTE TOO/ HIGH/,))))
  (THCOND
   ((AND $?X (CDR $?X)))
   ((THSETQ
     $_X
     (APPEND $?X
         (THVAL (LIST (QUOTE THFIND)
              (COND ((NULL $?X) 3) (2))
              (QUOTE $?Y)
              (QUOTE (Y))
              (QUOTE (THOR (THAND (THGOAL (#IS $?Y #BLOCK))
                          (THNOT (THGOAL (#SUPPORT $?Y ?))))
                       (THGOAL (#IS $?Y #BLOCK))))
              (QUOTE (NOT (EQ $?X $?Y))))
            THALIST)))))
  (COND ((THVSETQ $_PYR (PACKO $?X (QUOTE #PYRAMID))) (NULL (CDR $?PYR))) (T))
  (THVSETQ $_BLOCKS (CONS (QUOTE :TABLE) (PACKO $?X (QUOTE #BLOCK))))
  (MEMORY)
  GO
  (THCOND ((CDR $?BLOCKS) (THGOAL (#PUTON $E (CADR $?BLOCKS) $E (CAR $?BLOCKS))
                  (THUSE TC-PUTON))
              (THSETQ $_BLOCKS (CDR $?BLOCKS))
              (THGO GO))
      ($?PYR (THGOAL (#PUTON $E (CAR $?PYR) $E (CAR $?BLOCKS))
             (THUSE TC-PUTON)))
      ((MEMOREND (#STACKUP $?EV $?X)))))
 THEOREM)

(DEFPROP TC-STARTEND3
     (THCONSE (X Y EV TIME) ($?X $?EV $?TIME) (THGOAL ($?X $?Y $?EV $?TIME)
                              (THUSE TC-STARTEND4)))
     THEOREM)

(DEFPROP TC-STARTEND4
     (THCONSE (X NEWEV Z EV TIME)
          ($?X $?NEWEV $?EV $?TIME)
          (OR (AND (THASVAL $?X)
               (THASVAL $?EV)
               (THASVAL $?TIME)
               (NOT (THASVAL $?NEWEV)))
              (ERT TC-STARTEND4))
          (THGOAL (#CHOOSE $?EV $_Z NIL) (THUSE TC-CHOOSE))
          (OR (ATOM $?Z) (ERT TC-STARTEND4 ATOM))
          (THSETQ $_NEWEV (MAKESYM (QUOTE EV)))
          (PUTPROP $?NEWEV
               (PUTPROP $?NEWEV
                    (GET $?Z (COND ((EQ $?X (QUOTE #START))
                            (QUOTE START))
                           ((EQ $?X (QUOTE #END))
                            (QUOTE END))
                           ((ERT TC-STARTEND (THV X)))))
                    (QUOTE START))
               (QUOTE END))
          (TIMECHK $?NEWEV $?TIME)
          (PUTPROP $?NEWEV $?Z (QUOTE WHY))
          (PUTPROP $?NEWEV (QUOTE #START) (QUOTE TYPE)))
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
                   ((THSETQ PLAN
                        (CONS (QUOTE (UNGRASP)) PLAN)))))
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
          (OR (AND (THASVAL $?X) (THASVAL $?EV) (THASVAL $?TIME))
              (ERT TC-WANT5 THASVAL))
          (EQ $?X (QUOTE :FRIEND))
          (EQ (GET $?EV (QUOTE WHY)) (QUOTE COMMAND))
          (THSETQ $_NEWEV (MAKESYM (QUOTE EV)))
          (PUTPROP $?NEWEV
               (PUTPROP $?NEWEV
                    (GET $?EV (QUOTE START))
                    (QUOTE START))
               (QUOTE END))
          (TIMECHK $?NEWEV $?TIME)
          (PUTPROP $?NEWEV (QUOTE #TELL) (QUOTE TYPE))
          (PUTPROP $?NEWEV (QUOTE ESP) (QUOTE WHY)))
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
          (THOR (THGOAL (#MANIP $?Y)) (THAND (THGOAL (#IS $?Y #BOX))
                             (THGOAL (#AT $?Y $?Z))
                             (THSUCCEED THEOREM)))
          (THSETQ $_X(TFIND $?Y $?TIME))
(THOR(THSETQ $_W(CAR $?X))
(THAND(THAMONG $?W (CDR $?X))
(OR          (NOT (LESSP (CAR $?W) (OR (START? $?TIME) -1)))
(THFAIL THAND))
))

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
     (THCONSE (X EV TIME) ($?X $?TIME) (THGOAL ($?X $?EV $?TIME)
                           (THUSE TCTE-3)))
     THEOREM)

(DEFPROP TCT-3
     (THCONSE (X Y EV TIME) ($?X $?Y $?TIME) (THGOAL ($?X $?EV $?Y $?TIME)
                             (THUSE TCTE-4)))
     THEOREM)

(DEFPROP TCT-4
     (THCONSE (X Y Z EV TIME)
          ($?X $?Y $?Z $?TIME)
          (THGOAL ($?X $?EV $?Y $?Z $?TIME) (THUSE TCTE-5)))
     THEOREM)

(DEFPROP TCTE-PICKUP
     (THCONSE (X EV EVENT TIME)
          (#PICKUP $?EV $?X $?TIME)
          (THOR (THAND (THGOAL (#PICKUP $?EV $?X))
                   (TIMECHK $?EV $?TIME)
                   (THSUCCEED THEOREM))
            (THSUCCEED))
          (THAMONG $?EVENT EVENTLIST)
          (MEMQ (GET $?EVENT (QUOTE TYPE)) (QUOTE (#PUTON #GET-RID-OF)))
          (TIMECHK $?EVENT $?TIME)
          (THOR (THGOAL (#PUTON $?EVENT $?X ?))
            (THGOAL (#GET-RID-OF $?EVENT $?X)))
          (THVSETQ $_EV (MAKESYM (QUOTE E)))
          (AND (PUTPROP $?EV
                (PUTPROP $?EV
                     (GET $?EVENT (QUOTE END))
                     (QUOTE START))
                (QUOTE END))
               (PUTPROP $?EV (QUOTE #PICKUP) (QUOTE TYPE))
               (PUTPROP $?EV $?EVENT (QUOTE WHY))
               (SETQ EVENTLIST (CONS $?EV EVENTLIST))
               (THASSERT (#PICKUP $?EV $?X))))
     THEOREM)

(DEFPROP TCTE-PUT
     (THCONSE (X Y EV EVENT TIME Z)
          (#PUT $?EV $?X $?Y $?TIME)
          (THAMONG $?EVENT EVENTLIST)
          (MEMQ (GET $?EVENT (QUOTE TYPE)) (QUOTE (#PICKUP #PUTON)))
          (TIMECHK $?EVENT $?TIME)
          (THOR (THGOAL (#PUTON $?EVENT $?X ?))
            (THGOAL (#PICKUP $?EVENT $?X)))
          (OR (THVSETQ $_Z (SUB1 (ASSQ (GET $?EVENT (QUOTE END))
                           (GET $?X (QUOTE HISTORY)))))
              (ERT TCTE-PUT WRONG))
          (THAMONG $?Y (LIST (CADR $?Z)))
          (THSETQ $_EV (MAKESYM (QUOTE E)))
          (AND (PUTPROP $?EV
                (PUTPROP $?EV (CAR $?Z) (QUOTE START))
                (QUOTE END))
               (PUTPROP $?EV $?EVENT (QUOTE WHY))
               (PUTPROP $?EV (QUOTE #PUT) (QUOTE TYPE))
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
          UP
          (THCOND ((OR (NULL $?Z) (STARTIME $?Z $?TIME)) (THFAIL))
              ((OR (AND (NOT (THASVAL $?X))
                    (THSETQ $_X (CADAR $?Z)))
                   (EQ $?X (CADAR $?Z))))
              ((THSETQ $_Z (CDR $?Z)) (THGO UP))
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
                 (THCOND ((THGOAL (#PART $?X $_Y))
                      (THERASE (#PART $?X $?Y))
                      (THCOND ((THFIND 2
                               $?W
                               (W)
                               (THGOAL (#PART $?W $?Y)))
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

(DEFUN UNION (A B) (PROG NIL
            UP     (COND ((NULL A) (RETURN B))
                   ((MEMQ (CAR A) B))
                   ((SETQ B (CONS (CAR A) B))))
             (SETQ A (CDR A))
             (GO UP)))

(IOC V)

(QUOTE (COMMANDS ARE:
         #CLEARTOP
         #GET-RID-OF
         #GRASP
         #PACK
         #PICKUP
         #PUTIN
         #PUTON
         #RAISEHAND
         #STACKUP
         #UNGRASP))

(QUOTE (PREDICATES ARE: #LOC #SUPPORT #ON #PHYSOB))

§ anno/winograd/blockl

(declare (genprefix blockl))

;;;################################################################
;;;
;;;          BLOCKL - lisp code for the BLOCKS world
;;;
;;;################################################################

(DEFUN ABSVAL (X) (COND ((MINUSP X) (MINUS X)) (X)))

(DEFUN ATAB (X) (OR (ASSQ X ATABLE) (ERT ATABLE)))

(DEFUN CLEAR
       (LOC SIZE OBJ)
       (PROG (W X1 X2)
         (SETQ OBJ (LISTIFY OBJ))
         (AND (MEMQ NIL
            (MAPCAR (QUOTE (LAMBDA (X Y)
                           (AND (GREATERP X -1)
                            (GREATERP 1201 (PLUS X Y))
                            T)))
                LOC
                SIZE))
          (RETURN NIL))
         (SETQ W ATABLE)
    GO   (COND ((NULL W) (RETURN LOC))
           ((MEMQ (CAAR W) OBJ))
           ((AND (LESSP (CAR LOC) (PLUS (CAR (SETQ X1 (CADAR W)))
                        (CAR (SETQ X2 (CADDAR W)))))
             (LESSP (CAR X1) (PLUS (CAR LOC) (CAR SIZE)))
             (LESSP (CADR LOC) (PLUS (CADR X1) (CADR X2)))
             (LESSP (CADR X1) (PLUS (CADR LOC) (CADR SIZE)))
             (LESSP (CADDR LOC) (PLUS (CADDR X1) (CADDR X2)))
             (LESSP (CADDR X1) (PLUS (CADDR LOC) (CADDR SIZE))))
            (RETURN NIL)))
         (SETQ W (CDR W))
         (GO GO)))

(DEFUN DIFF (X Y) (MAPCAR (FUNCTION DIFFERENCE) X Y))

(DEFUN DIV2 (X) (QUOTIENT X 2))

(DEFUN ENDTIME (LIST TIME) (PROG (Y)
                 (OR (SETQ Y (END? TIME)) (RETURN LIST))
                UP     (COND ((NULL LIST) (RETURN NIL))
                       ((NOT (GREATERP (CAAR LIST) Y))
                    (RETURN LIST))
                       ((SETQ LIST (CDR LIST)) (GO UP)))))

(DEFUN EV NIL (OR NOMEM $?EV))

(DEFUN FINDSPACE
 (TYPE SURF SIZE OBJ)
 (PROG (XYMAX XYMIN N V X1 X2)
       (SETQ OBJ (LISTIFY OBJ))
       (AND (MEMQ SURF OBJ) (RETURN NIL))
       (COND ((EQ SURF (QUOTE :TABLE)) (SETQ XYMIN (QUOTE (0 0)))
                       (SETQ XYMAX (QUOTE (1200 1200)))
                       (SETQ LEVEL 0)
                       (GO ON))
         ((SETQ X (ATAB SURF))))
       (COND
    ((EQ TYPE (QUOTE CENTER))
     (COND ((CLEAR (SETQ V
                 (LIST (MAX 0 (PLUS (CAADR X)
                        (DIV2 (DIFFERENCE (CAADDR X)
                                  (CAR SIZE)))))
                   (MAX 0
                    (PLUS (CADADR X)
                          (DIV2 (DIFFERENCE (CADR (CADDR X))
                                (CADR SIZE)))))
                   (PLUS (CADDR (CADR X)) (CADDR (CADDR X)))))
               SIZE
               OBJ)
        (RETURN V))
           ((RETURN NIL))))
    ((EQ (CAR X) (QUOTE :BOX))
     (SETQ XYMIN (LIST (CAADR X) (CADADR X)))
     (SETQ XYMAX (LIST (PLUS (CAADDR X) (CAADR X))
               (PLUS (CADR (CADDR X)) (CADADR X))))
     (SETQ LEVEL 1))
    ((SETQ X1 (DIV2 (CAR SIZE)))
     (SETQ Y1 (DIV2 (CADR SIZE)))
     (SETQ XYMAX
           (LIST (MIN 1200 (SUB1 (PLUS (CAADDR X) (CAADR X) X1)))
             (MIN 1200 (SUB1 (PLUS (CADR (CADDR X)) (CADADR X) Y1)))))
     (SETQ XYMIN (LIST (MAX 0 (DIFFERENCE (CAADR X) X1))
               (MAX 0 (DIFFERENCE (CADADR X) Y1))))
     (SETQ LEVEL (PLUS (CADDR (CADR X)) (CADDR (CADDR X))))))
  ON   (SETQ N 10)
       (SETQ X1 (DIFFERENCE (CAR XYMAX) (CAR XYMIN)))
       (SETQ Y1 (DIFFERENCE (CADR XYMAX) (CADR XYMIN)))
  GO   (COND ((ZEROP (SETQ N (SUB1 N))) (RETURN NIL))
         ((OR (NOT (SETQ V
                 (GROW (LIST (PLUS (CAR XYMIN)
                           (REMAINDER (ABSVAL (RANDOM)) X1))
                     (PLUS (CADR XYMIN)
                           (REMAINDER (ABSVAL (RANDOM)) Y1))
                     LEVEL)
                   XYMIN
                   XYMAX
                   OBJ)))
          (LESSP (DIFFERENCE (CAADR V) (CAAR V)) (CAR SIZE))
          (LESSP (DIFFERENCE (CADADR V) (CADAR V)) (CADR SIZE)))
          (GO GO))
         ((RETURN (COND ((EQ TYPE (QUOTE RANDOM))
                 (LIST (DIV2 (DIFFERENCE (PLUS (CAAR V) (CAADR V))
                             (CAR SIZE)))
                   (DIV2 (DIFFERENCE (PLUS (CADAR V) (CADADR V))
                             (CADR SIZE)))
                   LEVEL))
                ((EQ TYPE (QUOTE PACK))
                 (LIST (CAAR V) (CADAR V) LEVEL))
                ((ERT FINDSPACE /-- TYPE))))))))

(DEFUN GOAL
       FEXPR
       (X)
       (SETQ PLAN NIL)
       (THVAL (LIST (QUOTE THGOAL) (CAR X) (QUOTE (THTBF THTRUE)))
          (QUOTE ((EV COMMAND))))
       (EVLIS (REVERSE PLAN)))

(DEFUN GROW
 (LOC MIN MAX OBJ)
 (PROG (GROW XL XH XO YL YH YO)
       (SETQ OBJ (LISTIFY OBJ))
       (COND
    ((OR
      (MINUSP (CAAR (SETQ XL (LIST (LIST (DIFFERENCE (CAR LOC) (CAR MIN))
                         NIL)))))
      (MINUSP (CAAR (SETQ XH (LIST (LIST (DIFFERENCE (CAR MAX) (CAR LOC))
                         NIL)))))
      (MINUSP (CAAR (SETQ YL (LIST (LIST (DIFFERENCE (CADR LOC) (CADR MIN))
                         NIL)))))
      (MINUSP (CAAR (SETQ YH (LIST (LIST (DIFFERENCE (CADR MAX) (CADR LOC))
                         NIL)))))
      (NULL
       (ERRSET
        (MAPC
         (FUNCTION
          (LAMBDA (X)
           (PROG (XX YY)
             (COND ((OR (MEMQ (CAR X) OBJ)
                (NOT (LESSP (CAADR X) (CAR MAX)))
                (NOT (LESSP (CADADR X) (CADR MAX)))
                (NOT (GREATERP (SETQ XX (PLUS (CAADR X)
                                  (CAADDR X)))
                           (CAR MIN)))
                (NOT (GREATERP (SETQ YY (PLUS (CADADR X)
                                  (CADR (CADDR X))))
                           (CADR MIN)))
                (NOT (GREATERP (PLUS (CADDR (CADR X))
                             (CADDR (CADDR X)))
                           (CADDR LOC))))
                (RETURN NIL))
               ((GREATERP (CAADR X) (CAR LOC))
                (SETQ XH
                  (ORDER (LIST (DIFFERENCE (CAADR X) (CAR LOC))
                           (CAR X))
                     XH)))
               ((LESSP XX (CAR LOC))
                (SETQ XL (ORDER (LIST (DIFFERENCE (CAR LOC) XX)
                          (CAR X))
                        XL)))
               ((SETQ XO (CONS (CAR X) XO))))
             (COND ((GREATERP (CADADR X) (CADR LOC))
                (SETQ YH (ORDER (LIST (DIFFERENCE (CADADR X)
                                  (CADR LOC))
                          (CAR X))
                        YH)))
               ((LESSP YY (CADR LOC))
                (SETQ YL (ORDER (LIST (DIFFERENCE (CADR LOC) YY)
                          (CAR X))
                        YL)))
               ((MEMQ (CAR X) XO) (ERR NIL))
               ((SETQ YO (CONS (CAR X) YO)))))))
         ATABLE))))
     (RETURN NIL)))
  GO   (COND ((= (SETQ GROW (MIN (CAAR XL) (CAAR XH) (CAAR YL) (CAAR YH)))
          2000)
          (RETURN (LIST (LIST (DIFFERENCE (CAR LOC) (CADAR XL))
                  (DIFFERENCE (CADR LOC) (CADAR YL)))
                (LIST (PLUS (CAR LOC) (CADAR XH))
                  (PLUS (CADR LOC) (CADAR YH))))))
         ((MAPC (FUNCTION (LAMBDA (Y Z W)
                      (PROG (X)
                        (SETQ X (EVAL W))
                        (COND ((GREATERP (CAAR X) GROW))
                          ((OR (NULL (CADAR X))
                               (MEMQ (CADAR X)
                                 (EVAL Y)))
                           (RPLACA X (LIST 2000
                                   (CAAR X))))
                          ((SET Z (CONS (CADAR X)
                                (EVAL Z)))
                           (SET W (CDR X)))))))
            (QUOTE (YO YO XO XO))
            (QUOTE (XO XO YO YO))
            (QUOTE (XL XH YL YH)))
          (GO GO)))))

(DEFUN LISTIFY (X) (COND ((ATOM X) (LIST X)) (X)))

(declare (*expr fn))

(DEFUN LOCGREATER (X Y FN) ((LAMBDA (XX YY)
                    (NOT (LESSP (FN (CADR XX))
                        (PLUS (FN (CADR YY))
                              (FN (CADDR YY))))))
                (LOCG2 (QUOTE $?YY) X)
                (LOCG2 (QUOTE $?ZZ) Y)))

(DEFUN LOCG2 (X Y) (COND ((EQ $?LOC (QUOTE #LOC)) (ATAB Y))
             ((CONS NIL (CONS (EVAL X) (CDDR (ATAB Y)))))))

(DEFUN MEMOREND FEXPR (A) (OR NOMEM (AND (PUTPROP $?EV THTIME (QUOTE END))
                     (APPLY (QUOTE THASSERT)
                        (LIST (THVARSUBST (CAR A) NIL )))
                     (PUTPROP $?EV (CAAR A) (QUOTE TYPE)))))

(DEFUN MEMORY NIL (OR NOMEM (THAND (THVSETQ $_EV (MAKESYM (QUOTE E)))
                   (THSETQ EVENTLIST (CONS $?EV EVENTLIST))
                   (PUTPROP $?EV THTIME (QUOTE START))
                   (PUTPROP $?EV $?WHY (QUOTE WHY)))))

(DEFUN OCCUPIER
       (X Y Z)
       (PROG (W X1 X2)
         (COND ((MINUSP Z) (RETURN (QUOTE :TABLE))))
         (SETQ W ATABLE)
    GO   (COND ((NULL W) (RETURN NIL))
           ((AND (LESSP (SUB1 (CAR (SETQ X1 (CADAR W))))
                X
                (PLUS (CAR X1) (CAR (SETQ X2 (CADDAR W)))))
             (LESSP (SUB1 (CADR X1)) Y (PLUS (CADR X1) (CADR X2)))
             (LESSP (SUB1 (CADDR X1)) Z (PLUS (CADDR X1)
                              (CADDR X2))))
            (RETURN (CAAR W))))
         (SETQ W (CDR W))
         (GO GO)))

(DEFUN ORDER (X Y) (COND ((NULL Y) (LIST X))
             ((GREATERP (CAR X) (CAAR Y))
              (CONS (CAR Y) (ORDER X (CDR Y))))
             ((CONS X Y))))

(DEFUN PACKO
       (OBJ TYPE)
       (PROG (XX)
         (MAPC (FUNCTION (LAMBDA (X)
                     (AND (THVAL (QUOTE (THGOAL (#IS $?X
                                     $E
                                     TYPE)))
                         (LIST (LIST (QUOTE X) X)))
                      (SETQ XX (PACKORD X (SIZE X) XX)))))
           OBJ)
         (RETURN (MAPCAR (QUOTE CADR) XX))))

(DEFUN PACKON
       (SURF LIST)
       (PROG (X)
         (SETQ SURF (ATAB SURF))
    GO   (COND ((NULL LIST) (RETURN NIL))
           ((OR (GREATERP (CAR (SETQ X (SIZE (CAR LIST))))
                  (CAADDR SURF))
            (GREATERP (CADR X) (CADR (CADDR SURF)))
            (GREATERP (PLUS (CADDR X)
                    (CADDR (CADR SURF))
                    (CADDR (CADDR SURF)))
                  501))
            (SETQ LIST (CDR LIST))
            (GO GO))
           ((RETURN (CAR X))))))

(DEFUN PACKORD
       (X SIZE LIST)
       (COND ((NULL LIST) (LIST (LIST SIZE X)))
         ((OR (GREATERP (CAAAR LIST) (CAR SIZE))
          (AND (EQ (CAR SIZE) (CAAAR LIST))
               (GREATERP (CADAAR LIST) (CADR SIZE))))
          (CONS (CAR LIST) (PACKORD X SIZE (CDR LIST))))
         ((CONS (LIST SIZE X) LIST))))

(DEFUN SIZE (X) (COND ((EQ X (QUOTE :BOX)) (QUOTE (400 400 300)))
              ((EQ X (QUOTE :TABLE)) (QUOTE (1200 1200 1200)))
              ((ATOM X) (CADDR (ATAB X)))
              (X)))

(DEFUN STARTHISTORY
 NIL
 (SETQ THTIME 0)
 (SETQ GRASPLIST NIL)
 (DEFPROP EE COMMAND WHY)
 (DEFPROP EE 0 START)
 (DEFPROP EE 0 END)
 (DEFPROP EE #START TYPE)
 (SETQ EVENTLIST (QUOTE (EE)))
 (THADD (QUOTE (#START EE :DIALOG)) NIL)
 (ERRSET (CLEANOUT E) NIL)
 (MAPC
  (QUOTE
   (LAMBDA (X)
    (AND (GET (CAR X) (QUOTE THASSERTION))
     (PUTPROP (CAR X)
          (LIST (LIST 0
                  (CADR X)
                  (CADAR (THVAL (QUOTE (THGOAL (#SUPPORT $?X $?Y)))
                        (LIST (LIST (QUOTE X)
                            (QUOTE THUNASSIGNED))
                          (LIST (QUOTE Y) (CAR X)))))))
          (QUOTE HISTORY)))))
  ATABLE))

(DEFUN STARTIME (LIST TIME) (LESSP (CAAR LIST) (OR (START? TIME) -1)))

(DEFUN SUPPORT
       (LOC SIZE X)
       (COND ((EQ (CADDR LOC) 0) (QUOTE :TABLE))
         ((SETQ LOC (OCCUPIER (PLUS (CAR LOC) (DIV2 (CAR SIZE)))
                  (PLUS (CADR LOC) (DIV2 (CADR SIZE)))
                  (SUB1 (CADDR LOC))))
          (COND ((EQ LOC X) NIL) (LOC)))))

(DEFUN TCENT (X1 X2) (LIST (PLUS (CAR X1) (DIV2 (CAR X2)))
               (PLUS (CADR X1) (DIV2 (CADR X2)))
               (PLUS (CADDR X1) (CADDR X2))))

(DEFUN TFIND (X Y) (PROG (Z)
             (OR (SETQ Z (GET X (QUOTE HISTORY))) (RETURN NIL))
            UP     (COND ((NOT (GREATERP (CAAR Z)
                           (OR (END? Y) 77777)))
                (RETURN Z))
                   ((SETQ Z (CDR Z)) (GO UP)))))

(DEFUN TIMECHK
       (EV TIME)
       (COND ((IMPERF? TIME)
          (NOT (OR (LESSP (GET EV (QUOTE END)) (OR (START? TIME) -1))
               (LESSP (OR (END? TIME) 777777)
                  (GET EV (QUOTE START))))))
         ((NOT (OR (LESSP (GET EV (QUOTE START)) (OR (START? TIME) -1))
               (LESSP (OR (END? TIME) 777777)
                  (GET EV (QUOTE END))))))))

§ anno/winograd/data

################################################################

        DATA >

   initial microplanner data base for the blocks world

################################################################

(THFLUSH THASSERTION)

(SETQ ATABLE (QUOTE ((:B1 (110 100 0) (100 100 100))
             (:B2 (110 100 100) (100 100 100))
             (:B3 (400 0 0) (200 200 200))
             (:B4 (640 640 1) (200 200 200))
             (:B5 (500 100 200) (100 100 300))
             (:B6 (0 300 0) (200 300 300))
             (:B7 (0 240 300) (200 200 200))
             (:B10 (300 640 0) (200 100 400))
             (:BW1 (570 570 0) (10 400 300))
             (:BW2 (570 570 0) (400 10 300))
             (:BW3 (570 1200 0) (400 10 300))
             (:BW4 (1200 570 0) (10 400 300))
             (:BOX (600 600 0) (400 400 1)))))

(SETQ DISPLAY-AS (QUOTE ((:B1 #DISPLAY #BLOCK (110 100 0) (100 100 100) RED)
            (:B2 #DISPLAY #PYRAMID (110 100 100 ) (100 100 100) GREEN)
            (:B3 #DISPLAY #BLOCK (400 0 0) (200 200 200) GREEN)
            (:B4 #DISPLAY #PYRAMID (640 640 1) (200 200 200) BLUE)
            (:B5 #DISPLAY #PYRAMID (500 100 200) (100 100 300) RED)
            (:B6 #DISPLAY #BLOCK (0 300 0) (200 300 300) RED)
            (:B7 #DISPLAY #BLOCK (0 240 300) (200 200 200) GREEN)
            (:B10 #DISPLAY #BLOCK (300 640 0) (200 100 400) BLUE)
            (:HAND #DISPLAY #HAND (40 0 0) (0 0 0) WHITE)
            (:TABLE #DISPLAY #TABLE (0 0 0) (1000 1000 0) BLACK)
            (:BOX #DISPLAY #BOX (600 600 0) (376 376 300) WHITE))))

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

((#AT :B1 (100 100 0)))
((#AT :B2 (100 100 100)))
((#AT :B3 (400 0 0)))
((#AT :B4 (640 640 1)))
((#AT :B5 (500 100 200)))
((#AT :B6 (0 300 0)))
((#AT :B7 (0 240 300)))
((#AT :B10 (300 640 0)))

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

((#AT :BOX (600 600 0)))
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

(SETQ HANDAT (SETQ HANDAT2 (QUOTE (40 0 0))))

(SETQ THTIME 0)

(THFLUSH HISTORY)

(ERRSET (STARTHISTORY))

(SETQ PLAN NIL)

(MAPC (QUOTE (LAMBDA (X Y) (PUTPROP X (LIST Y) (QUOTE COLOR))))
      (QUOTE (:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B10))
      (QUOTE (CB1 CB2 CB3 CB4 CB5 CB6 CB7 CB10)))

(IOC V)

§ anno/winograd/plnr

;kset fonts;22fg kst,,,

(declare (genprefix plnr))

(COMMENT DO NOT GRIND THIS FILE WITH THE STANDARD GRIND)

(SETQ THVERSION (CADR (STATUS UREAD)))

(DECLARE (PRINT (LIST 'SETQ 'THVERSION (LIST 'QUOTE (CADR (STATUS UREAD))))))

(DECLARE (*FEXPR THAPPLY
         THGENAME
         THSTATE
         THANTE
         THERASING
         THCONSE
         THDUMP
         THRESTRICT
         THBKPT
         THUNIQUE
         THVSETQ
         THMESSAGE
         THDO
         THGOAL
         THERASE
         THAND
         THNV
         THSUCCEED
         THAMONG
         THCOND
         THSETQ
         THASSERT
         THASVAL
         THERT
         THGO
         THFAIL
         THOR
         THFIND
         THFINALIZE
         THRETURN
         THPROG
         THFLUSH
         THNOT
         THV))

(DECLARE (MACROS T) (GENPREFIX TH))

(SETQ SYMBOLS T)

(COND ((ERRSET (AND PURE (SETQ LOW (PAGEBPORG))))) (' (NOT PURIFIED)))

(DEFUN THREAD                    ;FUNCTION FOR THE /$ READ MACRO

       ;;EXPANDS _ TO (THNV (READ)) EXPANDS A TO ASSERT  ;EXPANDS G TO GOAL EXPANDS T TO THTBF THTRUE
       NIL                    ;EXPANDS ? TO (THV (READ)) EXPANDS E TO (THEV
                        ;(READ))
  (PROG (CHAR)                    ;EXPANDS R TO THRESTRICT

    ;;TREATS & - - & AS A COMMENT
    (RETURN (COND ((EQ (SETQ CHAR (READCH)) (QUOTE ?))
               (LIST (QUOTE THV) (READ)))
              ((EQ CHAR (QUOTE E))
               (LIST (QUOTE THEV) (READ)))
              ((EQ CHAR (QUOTE _))
               (LIST (QUOTE THNV) (READ)))
              ((EQ CHAR (QUOTE &))
               (PROG NIL
              CHLP (COND ((EQ (QUOTE &) (READCH))
                      (RETURN (QUOTE (COMMENT)))))
                 (GO CHLP)))
              ((EQ CHAR (QUOTE T))
               (QUOTE (THTBF THTRUE)))
              ((EQ CHAR (QUOTE R)) (QUOTE THRESTRICT))
              ((EQ CHAR (QUOTE G)) (QUOTE THGOAL))
              ((EQ CHAR (QUOTE A)) (QUOTE THASSERT))
              ((EQ CHAR 'N) (LIST 'THANUM (READ)))
              ((PRINT (QUOTE ILLEGAL-PREFIX))
               (PRINC (QUOTE $))
               (PRINC CHAR)
               (PRINC (READ))
               (ERR NIL))))))

(DEFUN THPUSH
       MACRO
       (A)                                   ;(THPUSH THTREE NEWINFO) CONSES NEWINFO ONTO
       (LIST (QUOTE SETQ)                           ;THTREE
         (CADR A)
         (LIST (QUOTE CONS) (CADDR A) (CADR A))))

(DEFUN EVLIS
       (X)                                   ;EVLIS EVALS ELEMENTS OF ARG THEN RETURNS ARG
       (MAPC (FUNCTION EVAL) X))

(DEFUN THPRINT2 (X) (PRINC (QUOTE / )) (PRINC X))

(DEFUN THPRINTC (X) (TERPRI) (PRINC X) (PRINC '/ ))

(DECLARE (SPECIAL THTT THFST THTTL THLAS THNF THWH THFSTP))

(DEFUN THADD                                   ;THADD ADDS THEOREMS OR ASSERTION TO THE

       ;;INPUT - THPL - PROPERTY LIST TO BE PLACED ON    ;ASSERTION
       (THTT THPL)                               ;DATABASE INPUTS - THTT - NAME OF THM OR ACTUAL
                                       ;ASSERTION
       (PROG (THNF THWH THCK THLAS THTTL THT1 THFST THFSTP THFOO)      ;RETURNS NIL IF ALREADY THERE ELSE RETURNS THTT
         (SETQ THCK
           (COND ((ATOM THTT)

              ;;IF THTT IS ATOMIC WE ARE ASSERTING A THEOREM
              (OR (SETQ THT1 (GET THTT (QUOTE THEOREM)))

                  ;;IF NO THEOREM PROPERTY THE GUY MADE A MISTAKE
                  (PROG2 (PRINT THTT) (THERT CANT
                             THASSERT/,
                             NO
                             THEOREM
                             -
                             THADD)))
              (SETQ THWH (CAR THT1))

              ;;THWH NOW SET TO KIND OF THEOREM, LIKE THERASING
              (SETQ THTTL THTT)

              ;;MAKE AN EXTRA POINTER TO THTT
              (AND THPL

                   ;;IF WE HAVE A PL FOR OUR THEOREM, IT GOES ON
                   ;;THE ATOM WHICH IS THE NAME OF THE THEOREM
                   (PROG NIL

                     ;;GO THROUGH ITEMS ON PL ONE BY ONE
                LP   (THPUTPROP THTT
                        (CADR THPL)
                        (CAR THPL))
                     (COND ((SETQ THPL (CDDR THPL))
                        (GO LP)))))
              (CADDR THT1))
             (T (SETQ THWH (QUOTE THASSERTION))

                ;;SO WE HAVE AN ASSERTION TO ASSERT, MAKE THWH REFLECT THIS FACT
                (SETQ THTTL (CONS THTT THPL))

                ;;PROPERTY LIST IS "CDR" OF ASSERTION
                THTT)))
         (SETQ THNF 0.)

         ;;THNF IS COUNTER SAYING WHICH ATOM WE ARE FILING UNDER
         (SETQ THLAS (LENGTH THCK))

         ;;THLAS IS THE NUMBER OF TOP LEVEL ITEMS
         (SETQ THFST T)

         ;;THFST SAYS WE ARE TRYING TO PUT THE ITEM IN FOR THE FIRST TIME
         ;;WE NEED TO KNOW THIS SINCE THE FIRST TIME THROUGH
         ;;WE MUST TEST THAT THE ASSERTEE IS NOT ALREADY THERE
         ;;THCK IS INITIALLY THE ASSERTION OR THEOREM PATTERN
         ;;THE FIRST TIME WE GO INTO THE DATABASE WE CHECK TO
         ;;SEE IF THE ITEM IS THERE
         ;;THAT MEANS DOING AN EQUAL TEST ON EVERY
         ;;ITEM IN THE BUCKET.  AFTER THE FIRST TIME THIS IS NOT
         ;;NECESSARY.  SINCE VARIABLES WILL IN GENERAL HAVE MANY
         ;;MORE ITEMS IN THEIR BUCKET WE WILL WANT TO DO OUR
         ;;CHECK ON A NON VARIABLE ITEM IN THE PATTERN
    THP1 (COND ((NULL THCK)
                ;;THCK NIL MEANS THAT ALL THE ITEMS IN THE PATTERN ARE VARIABLES
                ;;SO WE TRY AGAIN ONLY THIS TIME DOING EQUAL CHECK ON
                ;;THE FIRST VARIABLE. THFOO NOW IS SIMPLY THE PATTERN
                (SETQ THCK THFOO)
                (SETQ THNF 0.)
                (SETQ THFOO (SETQ THFST NIL))

                ;;THFIRSTP SAYS WE AGAIN NEED TO CHECK FOR ASSERTEE
                ;;BEING IN DATA BASE, BUT NOW USE VARIABLES FOR EQ CHECK
                (SETQ THFSTP T)
                (GO THP1))
           ((NULL (SETQ THT1 (THIP (CAR THCK)))) (RETURN NIL))

           ;;THIP IS THE WORKHORSE FOR THADD IF IT RETURNS NIL
           ;;IT MEANS THE ASSERTEE IS ALREADY IN, SO FAIL
           ((EQ THT1 (QUOTE THOK)))

           ;;THOK WHICH IS RETURN BY THIP
           ;;SAYS THAT THE ASSERTEE IS NOT IN ALREADY
           ((SETQ THFOO

              ;;OTHERWISE WE GO AROUND AGAIN, STILL LOOKING FOR A NON
              ;;VARIABLE ITEM TO DO THE EQ CHECK
              (NCONC THFOO
                 (LIST (COND ((EQ THT1 (QUOTE THVRB))
                          (CAR THCK))))))
            (SETQ THCK (CDR THCK))
            (GO THP1)))
         (SETQ THFST NIL)
         (MAPC (FUNCTION THIP) (CDR THCK))
         (SETQ THNF 0.)
         (MAPC (FUNCTION THIP) THFOO)
         (RETURN THTTL)))

(DECLARE (UNSPECIAL THTT THFST THFSTP THTTL THLAS THNF THWH))

(DECLARE (SPECIAL THTREE THALIST THXX))

(DEFUN THAMONG
       FEXPR
  (THA)                        ;EXAMPLE - (THAMONG $?X (THFIND ... ))
  (COND                        ;$E - (THAMONG $E$?X (THFIND ... )) CAUSES THE
                        ;THVALUE OF                  ;$?X    ;TO BE THE FIRST INPUT TO THAMONG. THXX SET ;TO
    ((EQ (CADR (SETQ THXX (THGAL (COND ((EQ (CAAR THA)    ;OLD BINDING CELL OF $?X (OR $E$?X) IF $?X

                        ;;VALUES PUSHED ONTO THTREE AND THAMONG FAILS TO
                        (QUOTE THEV))    ;THUNASSIGNED, OLD VALUE AND LIST OF NEW
                    (THVAL (CADAR THA)    ;THAMONGF.
                           THALIST))
                       (T (CAR THA)))
                 THALIST)))
     (QUOTE THUNASSIGNED))
     (THPUSH THTREE (LIST (QUOTE THAMONG)
              THXX
              (THVAL (CADR THA) THALIST)))
     NIL)                    ;IF $?X ASSIGNED, THAMONG REDUCES TO A
    (T (MEMBER (CADR THXX) (THVAL (CADR THA) THALIST)))))    ;MEMBERSHIP TEST

(DECLARE (UNSPECIAL THTREE THALIST THXX))

(DECLARE (SPECIAL THALIST THBRANCH THABRANCH THTREE THML))

(DEFUN THAMONGF                                   ;(CAR THTREE) = (THAMONG OLDBINDINGCELL (NEW
       NIL                                   ;VALUES))
       (COND (THMESSAGE (THPOPT) NIL)
         ((CADDAR THTREE)                           ;LIST OF NEW VALUES NON NIL
          (RPLACA (CDADAR THTREE) (CAADDR (CAR THTREE)))           ;REPLACE OLD VALUE WITH NEW VALUE
          (RPLACA (CDDAR THTREE) (CDADDR (CAR THTREE)))           ;POP NEW VALUES
          (SETQ THBRANCH THTREE)                       ;STORE AWAY TREE FOR POSSIBLE BACKTRACKING
          (SETQ THABRANCH THALIST)                       ;STORE AWAY THALIST FOR POSSIBLE BACKTRACKING
          (THPOPT)                               ;POP TREE
          T)                               ;SUCCEED
         (T (RPLACA (CDADAR THTREE) (QUOTE THUNASSIGNED))           ;NO NEW VALUES LEFT. RETURN X TO THUNASSIGNED,
        (THPOPT)                           ;POP TREE AND CONTINUE FAILING.
        NIL)))

(DECLARE (UNSPECIAL THALIST THBRANCH THABRANCH THTREE THML))

(DECLARE (SPECIAL THTREE THEXP))

(DEFUN THAND FEXPR (A) (OR (NOT A)
               (PROG2 (THPUSH THTREE
                      (LIST (QUOTE THAND) A NIL))
                  (SETQ THEXP (CAR A)))))

(DECLARE (UNSPECIAL THTREE THEXP))

(DEFUN THANDF NIL (THBRANCHUN) NIL)

(DECLARE (SPECIAL THTREE THVALUE THEXP))

(DEFUN THANDT
       NIL
       (COND ((CDADAR THTREE) (THBRANCH)
                  (SETQ THEXP (CADR (CADAR THTREE)))
                  (RPLACA (CDAR THTREE) (CDADAR THTREE)))
         ((THPOPT)))
       THVALUE)

(DECLARE (UNSPECIAL THTREE THVALUE THEXP))

(DEFUN THANTE FEXPR
       (THX)                                   ;DEFINES AND OPTIONALLY ASSERTS ANTECEDENT
       (THDEF (QUOTE THANTE) THX))                       ;THEOREMS)

(DECLARE (SPECIAL THTREE THTRACE THOLIST THALIST))

(DEFUN THAPPLY FEXPR (L) (THAPPLY1 (CAR L)

                   ;;THAPPLY1 DOES THE REAL WORK, ALL WE DO IS GET THE THEOREM OFF THE
                   ;;PROPERTY LIST
                   (GET (CAR L) (QUOTE THEOREM))
                   (CADR L)))

(DEFUN THAPPLY1
       (THM THB DAT)

       ;;MAKE SURE THE THEOREM PATTERN MATCHES THE GOAL
       (COND ((AND (THBIND (CADR THB)) (THMATCH1 DAT (CADDR THB)))
          (AND THTRACE (THTRACES (QUOTE THEOREM) THM))

          ;;AS FAR AS THTREE GOES, ALL THEOREMS LOOK LIKE THPROG, AND
          ;;WHEN YOU COME DOWN TO IT, THEY ALL ACT LIKE THPROGS
          (THPUSH THTREE
              (LIST (QUOTE THPROG) (CDDR THB) NIL (CDDR THB)))

          ;;CALL THE MAIN THPROG WORKHORSE
          (THPROGA)
          T)

         ;;IF THE THEOREM PATTERN DIDN'T MATCH, START FAILING
         (T (SETQ THALIST THOLIST) (THPOPT) NIL)))

(DECLARE (UNSPECIAL THTREE THTRACE THOLIST THALIST))

(DECLARE (SPECIAL THALIST TYPE THX THTREE THEXP THTRACE THY1 THY))

(DEFUN THASS1
 (THA P)
 (PROG (THX THY1 THY TYPE PSEUDO)
       (AND (CDR THA) (EQ (CAADR THA) (QUOTE THPSEUDO)) (SETQ PSEUDO
                                  T))

       ;;IF YOU SEE "THPSEUDO" SET FLAG "PSEUDO" TO T
       (OR (ATOM (SETQ THX (CAR THA)))

       ;;IF (CAR THA) IS AN ATOM WE ARE ASSERTING (ERRASING) A THEOREM
       (THPURE (SETQ THX (THVARSUBST THX NIL)))

       ;;THVARSUBST SUBSTITUTES THE ASSIGNMENTS FOR ALL ASSIGNED VARIABLES
       ;;THPURE CHECKS THAT ALL VARIABLES ARE ASSIGNED
       PSEUDO

       ;;IF WE ARE NOT REALLY ASSERTING, THE VARIABLES DO NOT ALL HAVE TO BE ASSIGNED
       (PROG2 (PRINT THX)
          (THERT IMPURE ASSERTION OR ERASURE - THASS1)))
       (AND THTRACE (NOT PSEUDO) (THTRACES (COND (P (QUOTE THASSERT))
                         ((QUOTE THERASE)))
                       THX))
       (SETQ THA (COND (PSEUDO (CDDR THA)) ((CDR THA))))

       ;;THX IS NOW WHAT WE ARE ASSERTING, AND THA IS THE RECOMMENDATION LIST
       (OR

    ;;WE ARE NOW GOING TO PHYSICALLY ADD OR REMOVE ITEM
    (SETQ
     THX
     (COND (PSEUDO (LIST THX))

           ;;IF THPSEUDO, DON'T ALTER THE DATA BASE
           ;;IF P IS "T" WE ARE ASSERTING SO USE THADD
           (P (THADD THX

             ;;THADD TAKES TWO ARGS THE FIRST IS ITEM TO BE ADDED
             ;;THE SECOND IS THE PROPERTY LIST FOR THE ITEM
             (SETQ THY
                   (COND ((AND THA (EQ (CAAR THA)

                           ;;THPROP SAYS "MY CADR IS TO BE EVALED TO GET THE PROPERTY LIST
                           (QUOTE THPROP)))
                      (PROG2 0.
                         (EVAL (CADAR THA))

                         ;;AND REMOVE THPROP FROM THE RECOMENDATION LIST
                         (SETQ THA
                           (CDR THA))))))))

           ;;OTHERWISE WE ARE ERASING, SO USE THREMOVE
           (T (THREMOVE THX))))

    ;;THE LAST ITEM WILL BE NIL ONLY IF THADD OR THREMOVE FAILED.
    ;;THAT IS, IF THE ITEM TO BE ADDED WAS ALREADY THERE,
    ;;OR THE ONE TO BE REMOVED, WASN'T.
    (RETURN NIL))

       ;;TYPE IS THE KIND OF THEOREM WE WILL BE LOOKING FOR
       (COND (P (SETQ TYPE (QUOTE THANTE)))
         ((SETQ TYPE (QUOTE THERASING))))

       ;;IF WE ACTUALLY MUNGED THE DATABASE, PUT THE FACT IN THTREE
       (OR PSEUDO
       (THPUSH THTREE
           (LIST (COND (P (QUOTE THASSERT)) ((QUOTE THERASE)))
             THX
             THY)))
       (SETQ THY (MAPCAN (FUNCTION THTAE) THA))

       ;;MAPCAN IS A MAC-LISP FUNCTION, LIKE MAPCAR BUT USES NCONC
       ;;THTAE LOOKS AT THE RECOMENDATION LIST AND PRODUCES A
       ;;LIST OF INSTRUCTIONS ABOUT WHAT THEOREMS TO TRY
       (COND (THY (SETQ THEXP (CONS 'THDO THY))))

       ;;THEXP IS A HACK TELLING THVAL TO THVAL THIS ITEM
       ;;BEFORE IT GOES ON TO THE NEXT LINE OF PLANNER CODE
       ;;THEXP IS NOW (THDO  <APPROPRIATE ANTECEDENT OR ERASING THEOREMS>)
       (RETURN THX)))

(DECLARE (UNSPECIAL THALIST TYPE THX THTREE THEXP THTRACE THY1 THY))

(DEFUN THASSERT FEXPR (THA) (THASS1 THA T))                   ;THASS1 IS USED FOR BOTH ASSERTING AND ERASING,   ;THE "T" AS SECOND ARG TELLS IT THAT WE ARE       ;ASSERTING.

(DECLARE (SPECIAL THTREE))

(DEFUN THASSERTF
       NIL
       (THREMOVE (COND ((ATOM (CADAR THTREE)) (CADAR THTREE))
               (T (CAADAR THTREE))))
       (THPOPT)
       NIL)

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THTREE))

(DEFUN THASSERTT NIL (PROG2 0. (CADAR THTREE) (THPOPT)))

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THALIST))

(DEFUN THASVAL
       FEXPR
       (X)
       ((LAMBDA (X) (AND X (NOT (EQ (CADR X) (QUOTE THUNASSIGNED)))))
    (THGAL (CAR X) THALIST)))

(DECLARE (UNSPECIAL THALIST) (SPECIAL THPC))

(DEFUN THBA

       ;;JUST LIKE ASSQ IN LISP, ONLY RETURN WITH THE POINTER 1
       ;;ELEMENT PRIOR TO THE ONE ASKED FOR
       ;;USED ONLY BY THAD AND THREMOVE
       (TH1 TH2)
       (PROG (THP)
         (SETQ THP TH2)
    THP1 (AND (EQ (COND (THPC (CADR THP)) (T (CAADR THP))) TH1)
          (RETURN THP))
         (OR (CDR (SETQ THP (CDR THP))) (RETURN NIL))
         (GO THP1)))

(DEFUN THBAP

       ;;LIKE THBA, ONLY USED EQUAL RATHER THAN EQ
       (TH1 TH2)
       (PROG (THP)
         (SETQ THP TH2)
    THP1 (AND (EQUAL (COND (THPC (CADR THP)) (T (CAADR THP))) TH1)
          (RETURN THP))
         (OR (CDR (SETQ THP (CDR THP))) (RETURN NIL))
         (GO THP1)))

(DECLARE (UNSPECIAL THPC) (SPECIAL THTREE THOLIST THALIST))

(DEFUN THBIND

       ;;WHEN WE ENTER A NEW THEOREM OR THPROG
       ;;WE MUST BIND THE NEW VARIABLES.  A IS THE VARIABLE LIST
       (A)

       ;;THOLIST IS THE OLD THALIST
       (SETQ THOLIST THALIST)

       ;;IF A IS NIL THERE IS NOTHING TO DO
       (OR (NULL A)
       (PROG NIL
        GO     (COND
               ;;WHEN A IS NIL WE ARE DONE AND JUST PUT A MARKER
               ;;ON THTREE WITH A POINTER TO THE OLD THALIST
               ;;SO IT CAN BE RESTORED
               ((NULL A)
            (THPUSH THTREE
                (LIST (QUOTE THREMBIND) THOLIST))
            (RETURN T)))

         ;;OTHERWISE ADD TO THE ALIST THE NEW BINDING CELL
         (THPUSH THALIST
             (COND ((ATOM (CAR A))

                ;;THE FIRST ELEMENT IS THE NAME OF THE VARIABLE
                ;;IF THE ENTRY IS AN ATOM, THEN WE ARE JUST GIVEN THE
                ;;VARIABLE AND ITS INITIAL ASSIGNMENT IS "THUNASSIGNED"
                ;;I.E., NO INITIAL ASSIGNMENT
                (LIST (CAR A) (QUOTE THUNASSIGNED)))

                   ;;OTHERWISE OUR ENTRY IS A LIST
                   ;;IF THE FIRST ELEMENT OF THE LIST IS $R OR THRESTRICT
                   ;;WE ADD THE RESTRICTION TO THE BINDING CELL
                   ;;THE CDDR OF THE CELL GIVES THE RESTRICTION LIST
                   ((EQ (CAAR A) (QUOTE THRESTRICT))
                (NCONC (THBI1 (CADAR A)) (CDDAR A)))

                   ;;OTHERWISE WE ARE GIVEN BOTH THE VARIABLE AND ITS
                   ;;INITIAL ASSIGNMENT, SO MAKE THE SECOND ELEMENT OF THE
                   ;;BINDING CELL A POINTER TO THE INITIAL ASSIGNMENT
                   (T (LIST (CAAR A) (EVAL (CADAR A))))))
         (SETQ A (CDR A))

         ;;REPEAT FOR THE NEXT VARIABLE IN THE LIST
         (GO GO))))

(DECLARE (UNSPECIAL THOLIST THTREE THALIST))

(DEFUN THBI1 (X) (COND ((ATOM X) (LIST X (QUOTE THUNASSIGNED))) (T (LIST (CAR X) (EVAL (CADR X))))))

(DECLARE (SPECIAL THTRACE THVALUE))

(DEFUN THBKPT FEXPR (L) (OR (AND THTRACE (THTRACES (QUOTE THBKPT) L)) THVALUE))

(DECLARE (UNSPECIAL THTRACE THVALUE))

(DECLARE (SPECIAL THBRANCH THABRANCH THTREE))

(DEFUN THBRANCH
       NIL

       ;;THBRANCH IS CALLED BY THPROGT
       ;;AND WE ARE SUCCEEDING BACKWARDS
       ;;CAR THTREE IS THE THPROG MARKING
       (COND ;;THERE ARE NO MORE EXPRESSIONS TO EXECUTE IN THE THPROG
         ((NOT (CDADAR THTREE)))
         ((EQ THBRANCH THTREE) (SETQ THBRANCH NIL))

         ;;NORMAL CASE
         ;;CADDAR THTREE IS THE SECOND OF THE THREE ARGS ON THE THPROG MARK
         ;;THBRANCH AND THABRANCH ARE POINTERS TO THE THTREE AND THALIST
         ;;RESPECTIVELY AT THE POINT WHERE WE HAD JUST SUCCEEDED.
         ;;IN GENERAL, BY THE TIME WE GET BACK TO THE THPROG MARK ON THTREE
         ;;WE HAVE REMOVED THE THINGS PUT ON THTREE BY THE SUCCESSFUL
         ;;LAST LINE OF THE THPROG
         ;;WE WILL NOW STORE THIS INFORMATION ON THE THPROG MARK SO
         ;;THAT IF WE FAIL WE WILL HAVE RECORDS OF WHAT HAPPEND
         ;;IT IS STORED BY HACKING THE SECOND ARG TO THE THPROG MARK
         ((RPLACA (CDDAR THTREE)
              (CONS (LIST THBRANCH THABRANCH (CADAR THTREE))
                (CADDAR THTREE)))

          ;;WE NOW SETQ THBRANCH TO NIL.  IF THE NEXT LINE ALSO SUCCEEDS,
          ;;THVAL WILL LOOK FOR A NIL THBRRANCH TO INDICATE THAT IT SHOULD
          ;;SETQ IT AGAIN TO THE POINT OF SUCCESS
          (SETQ THBRANCH NIL))))

(DECLARE (UNSPECIAL THBRANCH THABRANCH THTREE))

(DECLARE (SPECIAL THTREE THALIST))

(DEFUN THBRANCHUN
       NIL

       ;;WE ARE NOW FAILING.  THBRANCHUN IS CALLED BY THPROGF
       (PROG (X) (RETURN (COND ;;IF THE SECOND ARG
                   ;;TO THE PROG MARK IS NON-NIL IT MEANS THAT THERE ARE
                   ;;PREVIOUS LINES IN THE THPROG TO FAIL BACK TO
                   ((SETQ X (CADDAR THTREE))

                ;;A COMPAIRISON OF THIS WITH WHAT HAPPEND IN THBRANCK
                ;;WILL REVEAL THAT ALL WE ARE DOING HERE IS RESTORING
                ;;THE PROG MARK TO IS STATE BEFORE THE LAST SUCCESS
                (RPLACA (CDAR THTREE) (CADDAR X))
                (RPLACA (CDDAR THTREE) (CDR X))

                ;;RESET THALIST AND THTREE
                (SETQ THALIST (CADAR X))
                (SETQ THTREE (CAAR X))
                T)

                   ;;THERE AREN'T ANY MORE THINGS IN THE THPROG TO TRY
                   ;;SO JUST RETURN NIL
                   (T (THPOPT) NIL)))))

(DECLARE (UNSPECIAL THTREE THALIST))
(DECLARE (SPECIAL THTREE THEXP))

(DEFUN THCOND
       FEXPR
       (THA)
       (THPUSH THTREE (LIST (QUOTE THCOND) THA NIL))
       (SETQ THEXP (CAAR THA)))

(DECLARE (UNSPECIAL THTREE THEXP))

(DEFUN THCONDF NIL (THOR2 NIL))

(DECLARE (SPECIAL THTREE THVALUE))

(DEFUN THCONDT
       NIL
       (RPLACA (CAR THTREE) (QUOTE THAND))
       (RPLACA (CDAR THTREE) (CAADAR THTREE))
       THVALUE)

(DECLARE (UNSPECIAL THTREE THVALUE))

(COMMENT THCONSE DEFINES AND OPTIONALLY ASSERTS CONSEQUENT THEOREMS)

(DEFUN THCONSE FEXPR (THX) (THDEF (QUOTE THCONSE) THX))

(DEFUN THDATA NIL (PROG (X)
           GO    (TERPRI)
            (COND ((NULL (SETQ X (READ NIL))) (RETURN T))
                  ((PRINT (THADD (CAR X) (CDR X)))))
            (GO GO)))

(COMMENT THDEF DEFINES AND OPTIONALLY ASSERTS THEOREMS)

(DEFUN THDEF
 (THMTYPE THX)
 (PROG (THNOASSERT? THMNAME THMBODY)
       (COND ((NOT (ATOM (CAR THX)))
          (SETQ THMBODY THX)
          (COND ((EQ THMTYPE (QUOTE THCONSE))
             (SETQ THMNAME (THGENAME TC-G)))
            ((EQ THMTYPE (QUOTE THANTE))
             (SETQ THMNAME (THGENAME TA-G)))
            ((EQ THMTYPE (QUOTE THERASING))
             (SETQ THMNAME (THGENAME TE-G)))))
         ((SETQ THMNAME (CAR THX)) (SETQ THMBODY (CDR THX))))      ;THNOOASSERT FEATURE
       (COND ((EQ (CAR THMBODY) (QUOTE THNOASSERT))
          (SETQ THNOASSERT? T)
          (SETQ THMBODY (CDR THMBODY))))
       (THPUTPROP THMNAME (CONS THMTYPE THMBODY) (QUOTE THEOREM))
       (COND
    (THNOASSERT?
     (PRINT (LIST THMNAME 'DEFINED 'BUT 'NOT 'ASSERTED)))
    ((THASS1 (LIST THMNAME) T)
     (PRINT (LIST THMNAME 'DEFINED 'AND 'ASSERTED)))
    (T (PRINT (LIST THMNAME 'REDEFINED))))
       (RETURN T)))

(DECLARE (SPECIAL THTREE THEXP))

(DEFUN THDO
       FEXPR
       (A)
       (OR (NOT A)
       (PROG2 (THPUSH THTREE (LIST (QUOTE THDO) A NIL NIL))
          (SETQ THEXP (CAR A)))))

(DECLARE (UNSPECIAL THTREE THEXP))

(DECLARE (SPECIAL THTREE THEXP THBRANCH THABRANCH))

(DEFUN THDO1
       NIL
       (RPLACA (CDAR THTREE) (CDADAR THTREE))
       (SETQ THEXP (CAADAR THTREE))
       (COND (THBRANCH (RPLACA (CDDAR THTREE)
                   (CONS THBRANCH (CADDAR THTREE)))
               (SETQ THBRANCH NIL)
               (RPLACA (CDDDAR THTREE)
                   (CONS THABRANCH
                     (CAR (CDDDAR THTREE)))))))

(DECLARE (UNSPECIAL THTREE THEXP THBRANCH THABRANCH))

(DECLARE (SPECIAL THTREE))

(DEFUN THDOB NIL (COND ((OR THMESSAGE (NULL (CDADAR THTREE)))
            (RPLACA (CAR THTREE) (QUOTE THUNDO))
            T)
               ((THDO1))))

(DECLARE (UNSPECIAL THTREE))

(DEFUN THDUMP
       FEXPR
       (THFILE)
       (APPLY 'UWRITE (COND (THFILE (CDDR THFILE))))
       (IOC R)
       (THSTATE)
       (APPLY 'UFILE THFILE))

(DEFUN THERASE FEXPR (THA) (THASS1 THA NIL))

(DECLARE (SPECIAL THTREE))

(DEFUN THERASEF
       NIL
       (THADD (COND ((ATOM (CADAR THTREE)) (CADAR THTREE))
            (T (CAADAR THTREE)))
          (COND ((ATOM (CADAR THTREE)) NIL) (T (CDADAR THTREE))))
       (THPOPT)
       NIL)

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THTREE))

(DEFUN THERASET NIL (PROG2 0. (CADAR THTREE) (THPOPT)))

(DECLARE (UNSPECIAL THTREE))

(COMMENT THERASING DEFINES AND OPTIONALLY ASSERTS ERASING THEOREMS)

(DEFUN THERASING FEXPR (THX) (THDEF (QUOTE THERASING) THX))

(DECLARE (SPECIAL THINF THTREE THMESSAGE))

(DEFUN THFAIL
       FEXPR
       (THA)
       (AND THA
        (PROG (THTREE1 THA1 THX)
         F      (SETQ THA1 (COND ((EQ (CAR THA) (QUOTE THEOREM))
                    (QUOTE THPROG))
                   ((EQ (CAR THA) (QUOTE THTAG))
                    (QUOTE THPROG))
                   ((EQ (CAR THA) (QUOTE THINF))
                    (SETQ THINF T)
                    (RETURN NIL))
                   ((EQ (CAR THA) (QUOTE THMESSAGE))
                    (SETQ THMESSAGE (CADR THA))
                    (RETURN NIL))
                   (T (CAR THA))))
          (SETQ THTREE1 THTREE)
         LP1  (COND ((NULL THTREE1)
             (PRINT THA)
             (COND ((ATOM (SETQ THA (THERT NOT
                               FOUND
                               -
                               THFAIL)))
                (RETURN THA))
                   (T (GO F))))
            ((EQ (CAAR THTREE1) THA1) (GO ELP1)))
         ALP1 (SETQ THTREE1 (CDR THTREE1))
          (GO LP1)
         ELP1 (COND ((EQ (CAR THA) (QUOTE THTAG))
             (COND ((MEMQ (CADR THA)
                      (CADDDR (CAR THTREE1)))
                (GO TAGS))
                   (T (GO ALP1)))))
          (SETQ THMESSAGE (LIST (CDR THTREE1)
                    (AND (CDR THA) (CADR THA))))
          (RETURN NIL)
         TAGS (SETQ THX (CADDAR THTREE1))
         LP2  (COND ((NULL THX) (GO ALP1))
            ((EQ (CAADDR (CAR THX)) (CADR THA))
             (SETQ THMESSAGE
                   (LIST (CAAR THX)
                     (AND (CDDR THA) (CADDR THA))))
             (RETURN NIL)))
          (SETQ THX (CDR THX))
          (GO LP2))))

(DECLARE (UNSPECIAL THINF THTREE THMESSAGE))

(DECLARE (SPECIAL THTREE THVALUE))

(DEFUN THFAIL?
       (PRD ACT)
       (THPUSH THTREE (LIST (QUOTE THFAIL?) PRD ACT))
       THVALUE)

(DECLARE (UNSPECIAL THTREE THVALUE))

(DECLARE (SPECIAL THTREE THMESSAGE))

(DEFUN THFAIL?F NIL (COND ((EVAL (CADAR THTREE))
               (EVAL (PROG2 (SETQ THMESSAGE NIL)
                  (CADDAR THTREE)
                  (THPOPT))))
              (T (THPOPT) NIL)))

(DECLARE (UNSPECIAL THTREE THMESSAGE))

(DECLARE (SPECIAL THVALUE))

(DEFUN THFAIL?T NIL (THPOPT) THVALUE)

(DECLARE (UNSPECIAL THVALUE) (SPECIAL THTREE))

(DEFUN THFINALIZE
       FEXPR
       (THA)
       (PROG (THTREE1 THT THX)
         (COND ((NULL THA)
            (SETQ THA (THERT BAD CALL - THFINALIZE))))
         (COND ((ATOM THA) (RETURN THA))
           ((EQ (CAR THA) (QUOTE THTAG))
            (SETQ THT (CADR THA)))
           ((EQ (CAR THA) (QUOTE THEOREM))
            (SETQ THA (LIST (QUOTE THPROG)))))
         (SETQ THTREE (SETQ THTREE1 (CONS NIL THTREE)))
    PLUP (SETQ THX (CADR THTREE1))
         (COND ((NULL (CDR THTREE1)) (PRINT THA)
                     (THERT OVERPOP - THFINALIZE))
           ((AND THT
             (EQ (CAR THX) (QUOTE THPROG))
             (MEMQ THT (CADDDR THX)))
            (GO RTLEV))
           ((OR (EQ (CAR THX) (QUOTE THPROG))
            (EQ (CAR THX) (QUOTE THAND)))
            (RPLACA (CDDR THX) NIL)
            (SETQ THTREE1 (CDR THTREE1)))
           ((EQ (CAR THX) (QUOTE THREMBIND))
            (SETQ THTREE1 (CDR THTREE1)))
           ((RPLACD THTREE1 (CDDR THTREE1))))
         (COND ((EQ (CAR THX) (CAR THA)) (GO DONE)))
         (GO PLUP)
    RTLEV(SETQ THX (CDDR THX))
    LEVLP(COND ((NULL (CAR THX)) (SETQ THTREE1 (CDR THTREE1))
                     (GO PLUP))
           ((EQ (CAADDR (CAAR THX)) THT) (GO DONE)))
         (RPLACA THX (CDAR THX))
         (GO LEVLP)
    DONE (SETQ THTREE (CDR THTREE))
         (RETURN T)))

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THTREE))

(DEFUN THFIND
 FEXPR
 (THA)
 (THBIND (CADDR THA))
 (THPUSH THTREE
     (LIST (QUOTE THFIND)
           (COND ((EQ (CAR THA) 'ALL) ' (1. NIL NIL))           ;STANDARD ALL
             ((NUMBERP (CAR THA))
              (LIST (CAR THA) (CAR THA) T))               ;SINGLE NUMBER
             ((NUMBERP (CAAR THA)) (CAR THA))               ;WINOGRAD CROCK FORMAT
             ((EQ (CAAR THA) 'EXACTLY)
              (LIST (CADAR THA) (ADD1 (CADAR THA)) NIL))
             ((EQ (CAAR THA) 'AT-MOST)
              (LIST 1. (ADD1 (CADAR THA)) NIL))
             ((EQ (CAAR THA) 'AS-MANY-AS)
              (LIST 1. (CADAR THA) T))
             (T (CONS (CADAR THA)                   ;ONLY THING LEFT IS AT-LEAST
                  (COND ((NULL (CDDAR THA)) (LIST NIL T))  ;NO AT-MOST
                    ((EQ (CADDAR THA) 'AT-MOST)
                     (LIST (ADD1 (CAR (CDDDAR THA)))
                       NIL))
                    (T (LIST (CAR (CDDDAR THA))
                         T))))))
           (CONS 0. NIL)
           (CADR THA)))
 (THPUSH THTREE (LIST (QUOTE THPROG) (CDDR THA) NIL (CDDR THA)))
 (THPROGA))

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THTREE THBRANCH THXX))

(DEFUN THFINDF
       NIL
       (SETQ THBRANCH NIL)
       (COND ((OR THMESSAGE (LESSP (CAADR (SETQ THXX (CDAR THTREE)))
                   (CAAR THXX)))
          (THPOPT)
          NIL)
         (T (THPOPT) (CDADR THXX))))

(DECLARE (UNSPECIAL THTREE THBRANCH THXX))

(DECLARE (SPECIAL THTREE THALIST THBRANCH THABRANCH))

(DEFUN THFINDT
       NIL
       (PROG (THX THY THZ THCDAR)
         (SETQ THZ (CADDR (SETQ THCDAR (CDAR THTREE))))
         (AND (MEMBER (SETQ THX (THVARSUBST THZ NIL))
              (CDADR THCDAR))
          (GO GO))
         (RPLACD (CADR THCDAR) (CONS THX (CDADR THCDAR)))
         (AND (EQ (SETQ THY (ADD1 (CAADR THCDAR))) (CADAR THCDAR))
          (RETURN (PROG2 (SETQ THBRANCH NIL)
                 (AND (CADDAR THCDAR) (CDADR THCDAR))
                 (THPOPT))))
         (RPLACA (CADR THCDAR) THY)
    GO   (SETQ THTREE THBRANCH)
         (SETQ THALIST THABRANCH)
         (SETQ THBRANCH NIL)
         (RETURN NIL)))

(DECLARE (UNSPECIAL THTREE THALIST THBRANCH THABRANCH))

(DECLARE (SPECIAL B))

(DEFUN THFLUSH                                   ;(THFLUSH) FLUSHES ALL ASSERTIONS AND THEOREMS
 FEXPR                                       ;INPUT = SEQUENCE OF INDICATORS DEFAULT =

 ;;EFFECT = FLUSHES THE PROPERTIES OF THESE
 (A)                                       ;(THASSERTION THCONSE THANTE THERASING)
 (MAPC                                       ;INDICATORS FROM ALL ATOMS
  (FUNCTION
   (LAMBDA (B)
    (MAPC (FUNCTION (LAMBDA (C)
                (MAPC (FUNCTION (LAMBDA (D)
                            (REMPROP D B)))
                  C)))
      (MAKOBLIST NIL))))
  (COND (A) (' (THASSERTION THCONSE THANTE THERASING)))))

(DECLARE (UNSPECIAL B))

(DECLARE (SPECIAL THXX))

(DEFUN THGAL                                   ;(THGAL $?X THALIST) RETURNS THE BINDING CELL (X
       (X Y)                                   ;-) OF X ON THALIST
       (SETQ THXX X)
       (SASSQ (CADR X) Y (FUNCTION (LAMBDA NIL
                       (PRINT THXX)
                       (THERT THUNBOUND THGAL)))))

(DECLARE (UNSPECIAL THXX))

(DECLARE (SPECIAL THGENAME))

(DEFUN THGENAME
       FEXPR                                   ;GENERATES UNIQUE NAME WITH ARG AS PREFIX
       (X)
       (READLIST (NCONC (EXPLODE (CAR X))
            (EXPLODE (SETQ THGENAME (ADD1 THGENAME))))))

(DECLARE (UNSPECIAL THGENAME))

(DEFUN THGO FEXPR (X) (APPLY (QUOTE THSUCCEED)
                 (CONS (QUOTE THTAG) X)))

(DECLARE (SPECIAL THTREE THTRACE THZ1 THZ THY1 THY THA2))

(DEFUN THGOAL
 FEXPR
 (THA)                                       ;THA = (PATTERN RECOMMENDATION)
 (PROG (THY THY1 THZ THZ1 THA1 THA2)                       ;PATTERN IS EITHER EXPLICIT, THE VALUE OF A
       (SETQ THA2 (THVARSUBST (CAR THA) T))                   ;PLANNER VARIABLE OR THVAL OF $E... THA2 =
       (SETQ THA1 (CDR THA))                           ;INSTANTIATED PATTERN THA1 = RECOMMENDATIONS
       (COND ((OR (NULL THA1)                           ;SHOULD DATA BASE BE SEARCHED TRYED IF NO RECS
          (AND (NOT (AND (EQ (CAAR THA1) 'THANUM)
                 (SETQ THA1
                       (CONS (LIST 'THNUM
                           (CADAR THA1))
                         (CONS (LIST 'THDBF
                             'THTRUE)
                           (CDR THA1))))))
               (NOT (AND (EQ (CAAR THA1) (QUOTE THNODB))       ;TRIED IF REC NOT THNODB OR (THDBF PRED)
                 (PROG2 (SETQ THA1 (CDR THA1)) T)))
               (NOT (EQ (CAAR THA1) (QUOTE THDBF)))))
          (SETQ THA1
            (CONS (LIST (QUOTE THDBF) (QUOTE THTRUE)) THA1))))
       (SETQ THA1 (MAPCAN (FUNCTION THTRY) THA1))               ;THMS AND ASSERTIONS SATISFYING RECS APPENDED TO
       (AND THTRACE (THTRACES (QUOTE THGOAL) THA2))               ;RECS
       (COND ((NULL THA1) (RETURN NIL)))
       (THPUSH THTREE (LIST (QUOTE THGOAL) THA2 THA1))               ;(THGOAL PATTERN MATCHES)
       (RPLACD (CDDAR THTREE) 262143.)
       (RETURN NIL)))                               ;FAILS TO THGOALF

(DECLARE (UNSPECIAL THTREE THTRACE THZ1 THZ THY1 THY THA2))

(DECLARE (SPECIAL THMESSAGE))

(DEFUN THGOALF
       NIL

       ;;BASICALLY ALL IT DOES IS TO SEND OFF TO
       ;;THTRY1 TO TRY ANOTHER POSSIBILITY
       ;;IF THTRY1 RETURNS NIL IT MEANS THAT IT COULDN'T FIND ANOTHER
       ;;POSSIBILITY AND WE SHOULD TELL THVAL THAT WE HAVE FAILED
       ;;ALL THPOPT DOES IS TO LOB THE THGOAL ENTRY OFF THTREE
       (COND (THMESSAGE (THPOPT) NIL) ((THTRY1)) (T (THPOPT) NIL)))

(DECLARE (UNSPECIAL THMESSAGE))

(DECLARE (SPECIAL THTREE THVALUE))

(DEFUN THGOALT NIL (PROG2 0.
              (COND ((EQ THVALUE (QUOTE THNOVAL))
                 (THVARSUBST (CADAR THTREE) NIL))
                (THVALUE))
              (THPOPT)))

(DECLARE (UNSPECIAL THTREE THVALUE))

(DECLARE (SPECIAL THTT THFSTP THFST THTTL THLAS THNF THWH))

(DEFUN THIP
       (THI)

       ;;THI IS AN ITEM FROM THE ASSERTION OR PATTERN OF THE THEOREM BEING ENTERED
       (PROG (THT1 THT3 THSV THT2 THI1)
         (SETQ THNF (ADD1 THNF))

         ;;THNF IS A FREE VARIABLE FROM THADD (WHO CALLS THIS BUGER)
         ;;IT SAYS WE ARE LOOKING AT THE N'TH PLACE IN THE PATTERN
         (COND ((AND (ATOM THI)
             (NOT (EQ THI (QUOTE ?)))
             (NOT (NUMBERP THI)))

            ;;THI1 IS THE NAME OF THE ATOM TO LOOK UNDER
            ;;WHEN THI IS A USUAL ATOM THI1 = THI
            ;;NUMBERS DON'T HAVE PROPERTY LISTS SO THEY DON'T COUNT
            ;;AS NORMAL ATOMS, NOR DOES "?" SINCE IT IS A SORT OF
            ;;VARIABLE IN PLANNER
            (SETQ THI1 THI))
           ((OR (EQ THI (QUOTE ?))
            (MEMQ (CAR THI) (QUOTE (THV THNV))))

            ;;SEE IF THI IS A VARIABLE
            (COND (THFST (RETURN (QUOTE THVRB)))

              ;;IF WE ARE DOING THIS FOR THE FIRST TIME, DON'T CONSIDER VARIABLES
              ;;FOR EXPLANATION WHY, SEE THADD
              ((SETQ THI1 (QUOTE THVRB)))))
           ((RETURN (QUOTE THVRB))))

         ;;OTHERWISE THI IS SOMETHING WITH NO PROPERTY LIST LIKE A NUMBER, OR LIST
         ;;RETURNING THVRB TO THADD TELLS IT THAT EVERYTHING IS OK SO
         ;;FAR, BUT NOTHING WAS DONE ON THIS ITEM
         (COND ((NOT (SETQ THT1 (GET THI1 THWH)))

            ;;THWH IS THE NAME OF THE PROPERTY TO LOOK UNDER ON THE ATOM
            ;;IF THIS PROPERTY IS NOT THERE THEN WE MUST PUT IT THERE
            ;;IN PARTICULAR, NO PROPERTY MEANS THAT THE
            ;;ASSERTEE HAS NEVER BEEN ASSERTED BEFORE
            (PUTPROP THI1
                 (LIST NIL
                   (LIST THNF (LIST THLAS 1. THTTL)))
                 THWH))
           ((EQ THT1 (QUOTE THNOHASH)) (RETURN (QUOTE THBQF)))

           ;;IF THE PROPERTY IS "THNOHASH" IT MEANS THAT WE
           ;;SHOULD NOT BOTHER TO INDEX UNDER THIS ATOM, SO
           ;;JUST RETURN TO THADD
           ((NOT (SETQ THT2 (ASSQ THNF (CDR THT1))))

            ;;LOOK ON THE PROPERTY LIST ENTRY TO SEE
            ;;IF THERE IS A SUB-ENTRY FOR PATTERNS WITH THIS ATOM         ;;IN THE THNF'TH POSITION
            ;;IF NOT, HACK THE ENTRY SO THERE IS.
            ;;AGAIN THIS IMPLIES THAT THE ASSERTEE HAS NEVER
            ;;BEEN ASSERTED BEFORE
            (NCONC THT1
               (LIST (LIST THNF (LIST THLAS 1. THTTL)))))
           ((NOT (SETQ THT3 (ASSQ THLAS (CDR THT2))))

            ;;NOW LOOK WITHIN THE SUB-ENTRY FOR A SUB-SUB-ENTRY.
            ;;I.E. THOSE PATTERNS WHICH ARE ALSO OF THE CORRECT
            ;;TOTAL LENGTH
            ;;THLAS IS A VARIABLE FROM THADD WHICH GIVES THE
            ;;LENGTH OF THE ASSERTEE
            ;;AGAIN, IF NOT THERE, HACK IT IN
            (NCONC THT2 (LIST (LIST THLAS 1. THTTL))))
           ((AND (OR THFST THFSTP)

             ;;THIS BRANCH SAYS THAT WE STILL NEED
             ;;TO CHECK THAT THE ASSERTEE HAS
             ;;NEVER BEEN ASSERTED BEFORE
             ;;THIS MEANS THAT WE MUST LOOK DOWN THE REMAINING
             ;;SUB-SUB-BUCKET LOOKING FOR THE ASSERTEE
             (COND ((EQ THWH (QUOTE THASSERTION))
                (ASSOC THTT (CDDR THT3)))

                   ;;RANDOMNESS DUE TO THE FACT THAT ASSERTIONS
                   ;;HAVE PROPERY LIST ON THEM, WHILE THEOREM NAMES
                   ;;ARE ATOMS WHOES PROPERTY LISTS ARE OF THE
                   ;;USUAL "INVISIBLE" VARIETY
                   (T (MEMQ THTT (CDDR THT3)))))

            ;;IF THE ASSERTEE IS FOUND RETURN NIL
            ;;INDICATING FAILURE
            (RETURN NIL))
           ((SETQ THSV (CDDR THT3))

            ;;HACK IN THE LATEST ENTRY INTO THE SUB-SUB-BUCKET
            (RPLACA (CDR THT3) (ADD1 (CADR THT3)))
            (RPLACD (CDR THT3) (NCONC (LIST THTTL) THSV))))

         ;;IF WE GET TO THIS POINT EVERYTHING
         ;;IS OK SO TELL THADD SO
         (RETURN (QUOTE THOK))))

(DECLARE (UNSPECIAL THTT THFST THFSTP THTTL THLAS THNF THWH))

(DECLARE (SPECIAL THOLIST THALIST THX THY))

(DEFUN THMATCH2

 ;;THX IS ONE ITEM FROM THE PATTERN
 ;;THY IS THE CORESPONDING ITEM FROM THE CANDIDATE
 ;;THMATCH2 DECIDES IF THE TWO ITEMS REALLY MATCH
 (THX THY)

 ;;THOLIST IS THE "THALIST" WHICH WAS IN EXISTANCE BEFORE
 ;;WE STARTED WORKING ON THE CURRENT LINE OF PLANNER CODE
 ;;STANDARD CHECK FOR $E
 (AND (EQ (CAR THX) (QUOTE THEV))
      (SETQ THX (THVAL (CADR THX) THOLIST)))
 (AND (EQ (CAR THY) (QUOTE THEV))
      (SETQ THY (THVAL (CADR THY) THALIST)))
 (COND

  ;;IF EITHER IS A ? ANYTHING WILL MATCH, SO OK
  ((EQ THX (QUOTE ?)))
  ((EQ THY (QUOTE ?)))

  ;;IF EITHER IS A VARIABLE THINGS GET MESSY.
  ;;  EVERYTHING DOWN TO ***** IS
  ;;CONCERNED WITH THIS CASE
  ((OR (MEMQ (CAR THX) (QUOTE (THV THNV THRESTRICT)))
       (MEMQ (CAR THY) (QUOTE (THV THNV THRESTRICT))))
   ((LAMBDA (XPAIR YPAIR)

     ;;X AND Y PAIR ARE THE RESPECTIVE BINDING CELLS WHICH
     ;;WILL HAVE ANY NEW RESTRICTIONS MENTIONED.  IF THX OR
     ;;THY IS NOT A VARIABLE (I.E. THE OTHER IS ) THEN X OR Y PAIR WILL
     ;;BE NIL
     (COND ((AND XPAIR

         ;;THX IS A VARIABLE
         ;;THIS SEES IF THX IS UNASSIGNED
         (OR (EQ (CAR THX) (QUOTE THNV))
             (AND (EQ (CAR THX) (QUOTE THV))
              (EQ (CADR XPAIR) (QUOTE THUNASSIGNED))))

         ;;THCHECK MACKES SURE THE RESTRICTIONS (IF ANY) ON
         ;;THX ARE COMPATIBLE WITH THY
         (THCHECK (CDDR XPAIR)
              (COND (YPAIR (CADR YPAIR)) (T THY))))

        ;;FURTHERMORE, THY IS ALSO A VARIABLE
        ;;THIS MEANS WE MUST DO THE MYSTERIOUS VARIABLE LINKING
        (COND (YPAIR (THRPLACAS (CDR XPAIR) (CADR YPAIR))

             ;;IF THY ALSO HAS RESTRICTIONS, WHEN WE
             ;;LINK VARIABLES WE COMBINE RESTRICTIONS
             (AND (CDDR YPAIR)
                  (THRPLACDS (CDR XPAIR)
                     (THUNION (CDDR XPAIR)
                          (CDDR YPAIR))))
             (THRPLACDS YPAIR (CDR XPAIR)))

          ;;IF THY IS NOT A VARIALBE, JUST ASSIGN THX TO THY
          ;;THRPLACAS WILL HACK THML THE FREE VARIABLE FROM THMATCH1
          (T (THRPLACAS (CDR XPAIR) THY))))

       ;;IN THIS COND PAIR THY IS A VARIABLE AND THX IS EITHER
       ;;A CONSTANT OR A PREVIOUSLY ASSIGNED VARIALBE
       ((AND YPAIR
         (OR (EQ (CAR THY) (QUOTE THNV))

             ;;FURTHERMORE THY IS UNASSIGNED
             (AND (EQ (CAR THY) (QUOTE THV))
              (EQ (CADR YPAIR) (QUOTE THUNASSIGNED))))

         ;;MAKE SURE RESTRICTIONS ARE OK
         (THCHECK (CDDR YPAIR)
              (COND (XPAIR (CADR XPAIR)) (T THX))))

        ;;IF THX IS A VARIABLE, LINK
        (COND (XPAIR (THRPLACAS (CDR YPAIR) (CADR XPAIR)))

          ;;OTHERWISE JUST ASSIGN THY TO THX
          (T (THRPLACAS (CDR YPAIR) THX))))

       ;;THX IS AN ASSIGED VARIABLE, SO JUST MAKE
       ;;SURE ITS ASSIGNEMENT IS EQUAL TO THY
       ((AND XPAIR (EQUAL (CADR XPAIR)
                  (COND (YPAIR (CADR YPAIR)) (T THY)))))

       ;;THX IS A CONSTANT, THY IS A VARIABLE, AND THEY ARE EQUAL
       ((AND YPAIR (EQUAL (CADR YPAIR) THX)))

       ;;LOOSE, SO RETURN WITH AN ERROR
       (T (ERR NIL))))

    ;;
    ;;THE FOLLOWING TWO CONDS BIND XPAIR AND YPAIR RESPECTIVELY
    ;;
    (COND ;;IF THX IS A NORMAL VARIALBE, IN PARTICULAR
      ;;WE ARE NOT INTRODUCING NEW RESTRICTIONS AT THIS TIME,
      ;;THEN X PAIR IS JUST THE BINDING LIST
      ((THVAR THX) (THGAL THX THOLIST))

      ;;WE MUST HACK A NEW RESTRICTION ONTO THE
      ;;BINDING LIST
      ((EQ (CAR THX) (QUOTE THRESTRICT))

       ;;WE ARE "RESTRICTING" A ?.  SINCE ? HAS NO
       ;;BINDING LIST, WE MAKE UP A PSEUDO BINDING LIST
       (COND ((EQ (CADR THX) (QUOTE ?))
          (PROG2 0.
             (CONS (QUOTE ?)
                   (CONS (QUOTE THUNASSIGNED)
                     (APPEND (CDDR THX) NIL)))
             (SETQ THX (QUOTE (THNV ?)))))

         ;;WE ARE RESTRICTING A VARIABLE.  THIS MEANS THAT
         ;;WE MUST PUT IN ON THE BINDING LIST
         (T ((LAMBDA (U)
                 (THRPLACDS (CDR U)

                    ;;THUNION MAKES SURE WE DON'T PUT THE SAME RESTRICTION ON TWICE
                    (THUNION (CDDR U) (CDDR THX)))
                 (SETQ THX (CADR THX))
                 U)
             (THGAL (CADR THX) THOLIST))))))

    ;;NOTE THAT IF THX IS NOT A VARIABLE THEN XPAIR IS ()
    ;;
    ;;WE DO THE EXACT SAME THING FOR THY AS WE JUST DID FOR THX
    ;;
    (COND ((THVAR THY) (THGAL THY THALIST))
      ((EQ (CAR THY) (QUOTE THRESTRICT))
       (COND ((EQ (CADR THY) (QUOTE ?))
          (PROG2 0.
             (CONS (QUOTE ?)
                   (CONS (QUOTE THUNASSIGNED)
                     (APPEND (CDDR THY) NIL)))
             (SETQ THY (QUOTE (THNV ?)))))
         (T ((LAMBDA (U)
                 (THRPLACDS (CDR U)
                    (THUNION (CDDR U) (CDDR THY)))
                 (SETQ THY (CADR THY))
                 U)
             (THGAL (CADR THY) THALIST))))))))

  ;;***************
  ;;IF THE TWO ARE EQUAL, NATURALLY THEY MATCH
  ((EQUAL THX THY))

  ;;IF NOT, THEY DON'T, SO REPORT FAILURE
  (T (ERR NIL))))

(DECLARE (UNSPECIAL THOLIST THALIST THX THY) (SPECIAL THX THPRD))

(DEFUN THCHECK
       (THPRD THX)
       (OR (NULL THPRD)
       (EQ THX (QUOTE THUNASSIGNED))
       (ERRSET (MAPC (FUNCTION (LAMBDA (THY)
                       (OR (THY THX) (ERR NIL))))
             THPRD))))

(DECLARE (UNSPECIAL THX THPRD) (SPECIAL THY THX THTREE THOLIST THML))

(DECLARE (SPECIAL L2))

(DEFUN THUNION
       (L1 L2)
       (MAPC (FUNCTION (LAMBDA (THX)
                   (COND ((MEMBER THX L2))
                     (T (SETQ L2 (CONS THX L2))))))
         L1)
       L2)

(DECLARE (UNSPECIAL L2))

(DECLARE (SPECIAL THX THALIST THOLIST))

(DEFUN THMATCH THX ((LAMBDA (THOLIST THALIST)
                (THMATCH1 (ARG 1.) (ARG 2.)))
            (COND ((GREATERP THX 2.) (ARG 3.)) (T THALIST))
            (COND ((GREATERP THX 3.) (ARG 4.)) (T THALIST))))

(DEFUN THMATCH1

       ;;THX IS THE PATTERN TO BE MATCHED
       ;;THY IS THE POSSIBLE CANDIDATE
       (THX THY)

       ;;THMATCH1 DOES PRELIMINARLY WORK BEFORE HANDING
       ;;THE PATTERN AND CANDIDATE OFF TO THMATCH2
       ;;WHO DOES THE REAL WORK
       (PROG (THML)

         ;;THML IS A FREE VARIABLE WHO WILL BE HACKED BY THMATCH2
         ;;WHEN THMATCH2 IS DONE, THML WILL HAVE A RECORD OF ALL VARIABLE
         ;;ASSIGNMENTS MADE DURING THE MATCH.  NATURALLY
         ;;WE MUST KEEP TRACK SO IF WE FAIL BACK WE CAN UNDO THEM.
         ;;WE HAVE TO CHECK THAT THE PATTERN AND CANDIDATE
         ;;ARE OF THE SAME LENGTH SINCE THE USER MAY HAVE
         ;;SPECIFIED THE CANDIDATE WITH A "THUSE" RECOMMENDATION
         (COND ((AND (EQ (LENGTH (COND ((EQ (CAR THX)
                        (QUOTE THEV))
                        (SETQ THX
                          (THVAL (CADR THX)
                             THOLIST)))
                       (THX)))
                 (LENGTH THY))

             ;;IF THE MATCH FAILS, THMATCH2 EXITS WITH AN ERR
             ;;WILL BE "TRUE" PROVIDED THE MATCH WORKED
             (ERRSET (MAPC (FUNCTION THMATCH2) THX THY)))

            ;;SO RECORD THE ASSIGNMENTS ON THTREE
            (AND THML
             (THPUSH THTREE (LIST (QUOTE THMUNG) THML)))
            (RETURN T))

           ;;IF THE MATCH FAILED, WE MAY STILL HAVE
           ;;SOME ASSIGNEMENTS ALREADY MADE.  THESE
           ;;MUST IMMEDIATELY BE UNDONE.  EVLIS JUST EVALS
           ;;EVERYTHING ON THML WHICH IS A LIST OF EXPRESSIONS
           ;;WHICH, WHEN EVALED, UNASSIGN THE VARIABLES
           (T (EVLIS THML) (RETURN NIL)))))

(DECLARE (UNSPECIAL THY THX THTREE THOLIST THML))

(DECLARE (SPECIAL THNF THWH THALIST))

(DEFUN THMATCHLIST
       (THTB THWH)

       ;;THTB IS A PATTERN WHICH EVENTUALLY IS TO BE MATCHED
       ;;THWH SAYS IF IT IS AN ASSERTION, CONSEQUENT THEOREM, ETC.
       ;;THMATCHLIST GOES THROUGH THE DATA BASE, LOOKING ON ALL
       ;;THE BUCKETS OF THE ATOMS IN THE PATTERN
       ;;IT RETURNS THE SHORTEST BUCKET TO THGOAL
       (PROG (THB1 THB2 THL THNF THAL THA1 THA2 THRN THL1 THL2 THRVC)
         (SETQ THL 34359738367.)

         ;;THL IS THE LENGTH OF THE SHORTEST BUCKET FOUND SO FAR
         ;;INITIALLY IT IS SET TO A VERY LARGE NUMBER
         (SETQ THNF 0.)

         ;;COUNTER WHICH SAYS WHICH PATTERN ITEM WE ARE WORKING ON
         (SETQ THAL (LENGTH THTB))

         ;;LENGTH OF PATTERN
         (SETQ THB1 THTB)

         ;;THB1 WILL BE THE REMAINDER OF THE PATTERN TO YET BE WORKED ON
         ;;WHEN IT IS NIL, WE ARE DONE
         ;;SO RETURN THE BUCKET.  THL1 IS THE BUCKET UNDER THE ATOM
         ;;THL2 IS THE BUCKET UNDER THE VARIABLE IN THAT POSITION
         ;;IF WE ARE WORKING ON AN ASSERTION, THL2 WILL BE () SINCE
         ;;THERE ARE NO VARIABLES IN ASSERTIONS
         ;;IN THEOREMS, WE MUST TAKE INTO ACCOUNT THE FACT THAT
         ;;THE THEOREM MAY HAVE EITHER THE CORRECT ATOM, OR A
         ;;VARIALBE IN A GIVEN POSITION, AND STILL MATCH
    THP1 (OR THB1
         (RETURN (COND (THL2 (APPEND THL1 THL2)) (THL1))))

         ;;ADD1 TO POSITION COUNTER
         (SETQ THNF (ADD1 THNF))

         ;;THB2 IS THE ITEM WE ARE WORKING ON IN THIS PASS
         (SETQ THB2 (CAR THB1))

         ;;UPDATE THB1
         (SETQ THB1 (CDR THB1))
    THP3 (COND ((OR (NULL (ATOM THB2))

            ;;IF THE ITEM IS NOT A NORMAL ATOM, SKIP IT AND
            ;;GO TO NEXT PASS
            (NUMBERP THB2)
            (EQ THB2 (QUOTE ?)))
            (GO THP1))

           ;;IF THE ITEM DOES NOT HAVE THE PROPERTY ON ITS PROPERTY
           ;;LIST, THEN IT OBVIOUSLY DOSEN'T HAVE ANY BUCKET AT ALL
           ;;SO THA1, WHICH RECORDS THE NUMBER IN THE BUCKET IS SET TO 0
           ((NOT (SETQ THA1 (GET THB2 THWH)))

            ;;IF A BUCKET IS FOUND, THE FIRST THING
            ;;IN THE BUCKET WILL BE THE NUMBER OF GOODIES THERE
            ;;THE REST WILL BE THE GOODIES.  THE FIRST 0 IN THA1
            ;;THEN SAYS THAT THERE WAS NO BUCKET.  THE SECOND
            ;;0 IS JUST A DUMMY FOR THE GOODIES WHICH ARN'T THERE
            (SETQ THA1 (QUOTE (0. 0.))))

           ;;IF IT IS A THNOHASH WE IGNOR IT JUST LIKE
           ;;A LIST, OR NUMBER
           ((EQ THA1 (QUOTE THNOHASH)) (GO THP1))

           ;;SAME IF THERE IS NO SUB-BUCKET FOR THE ATOM
           ;;IN THE CORRECT POSITION
           ((NOT (SETQ THA1 (ASSQ THNF (CDR THA1))))
            (SETQ THA1 (QUOTE (0. 0.))))

           ;;SAME FOR SUB-SUB-BUCKET (PATTERN LENGTH)
           ((NOT (SETQ THA1 (ASSQ THAL (CDR THA1))))
            (SETQ THA1 (QUOTE (0. 0.)))))
         (SETQ THRN (CADR THA1))
         (SETQ THA1 (CDDR THA1))

         ;;IF ITS AN ASSERTION, THEN WE DONT HAVE TO LOOK FOR VARIABLES
         (AND (EQ THWH (QUOTE THASSERTION)) (GO THP2))

         ;;THVRB IS THE ATOM WHICH HAS THE BUCKET FOR VARIABLES
         ;;WE WILL NOW LOOK TO SEE IF THERE ARE ANY THEOREMS WHICH
         ;;HAVE A VARIABLE IN THE CORRECT POSSITION
         (COND ((NOT (SETQ THA2 (GET (QUOTE THVRB) THWH)))
            (SETQ THA2 (QUOTE (0. 0.))))
           ((NOT (SETQ THA2 (ASSQ THNF (CDR THA2))))
            (SETQ THA2 (QUOTE (0. 0.))))
           ((NOT (SETQ THA2 (ASSQ THAL (CDR THA2))))
            (SETQ THA2 (QUOTE (0. 0.)))))
         (SETQ THRVC (CADR THA2))
         (SETQ THA2 (CDDR THA2))

         ;;SEE IF THE SUM OF THE NUMBER OF GOODIES IN THE ATOM BUCKET PLUS
         ;;THE NUMBER IN THE VARIABLE BUCKET IS GREATER THAN THE SMALLEST
         ;;NUMBER SO FAR.  IF SO WE KEEP THE PREVIOUS NUMBER
         (AND (GREATERP (PLUS THRVC THRN) THL) (GO THP1))

         ;;OTHERWISE THIS BECOMES THE NEW SMALLEST
         (SETQ THL (PLUS THRVC THRN))

         ;;AND THL1 AND THL2 ARE POINTERS TO THE NEWLY DISCOVERD BUCKETS
         (SETQ THL1 THA1)
         (SETQ THL2 THA2)

         ;;GO BACK FOR ANOTHER PASS
         (GO THP1)

         ;;THIS SECTION IS FOR ASSERTIONS, I.E., DON'T HAVE TO CONSIDER VARIABLES
    THP2 (COND
           ;;IF THERE IS NO BUCKET THEN RETURN SINCE NOTHING WILL MATCH THE
           ;;PATTERN
           ((EQ THRN 0.) (RETURN NIL))

           ;;IF THE NEW BUCKET IS SMALLER, IT BECOMES THE SMALLEST SO FAR
           ((GREATERP THL THRN) (SETQ THL1 THA1)
                    (SETQ THL THRN)))

         ;;GO BACK FOR ANOTHER PASS
         (GO THP1)))

(DECLARE (UNSPECIAL THNF THWH THALIST))

(DECLARE (SPECIAL THTREE THVALUE))

(DEFUN THMESSAGE
       FEXPR
       (THA)
       (THPUSH THTREE (CONS 'THMESSAGE THA))
       THVALUE)

(DECLARE (UNSPECIAL THTREE THVALUE))

(DECLARE (SPECIAL THALIST THOLIST THTREE THMESSAGE))

(DEFUN THMESSAGEF NIL (PROG (BOD)
                (SETQ BOD (CAR THTREE))
                (THPOPT)
                (COND ((AND (THBIND (CADR BOD))
                    (THMATCH1 (CADDR BOD)
                          THMESSAGE))
                   (THPUSH THTREE (LIST (QUOTE THPROG)
                            (CDDR BOD)
                            NIL
                            (CDDR BOD)))
                   (SETQ THMESSAGE NIL)
                   (RETURN (THPROGA)))
                  (T (SETQ THALIST THOLIST) ))
                (RETURN NIL)))

(DECLARE (UNSPECIAL THALIST THOLIST THTREE THMESSAGE))

(DECLARE (SPECIAL THVALUE))

(DEFUN THMESSAGET NIL (THPOPT) THVALUE)

(DECLARE (UNSPECIAL THVALUE))

(DECLARE (SPECIAL THTREE))

(DEFUN THMUNGF NIL (EVLIS (CADAR THTREE)) (THPOPT) NIL)

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THVALUE))

(DEFUN THMUNGT NIL (THPOPT) THVALUE)

(DECLARE (UNSPECIAL THVALUE))

(DEFUN THNOFAIL (THX) (COND (THX (DEFPROP THPROG THPROGT THFAIL))
                (T (DEFPROP THPROG THPROGF THFAIL))))

(DECLARE (SPECIAL THA))

(DEFUN THNOHASH
       FEXPR
       (THA)
       (MAPC (FUNCTION (LAMBDA (X) (PUTPROP (CAR THA)
                        (QUOTE THNOHASH)
                        X)))
         (OR (CDR THA)
         (QUOTE (THASSERTION THCONSE THANTE THERASING)))))

(DECLARE (UNSPECIAL THA))

(DECLARE (SPECIAL THEXP))

(DEFUN THNOT FEXPR (THA) (SETQ THEXP
                   (LIST (QUOTE THCOND)
                     (LIST (CAR THA)
                       (QUOTE (THFAIL THAND)))
                     (QUOTE ((THSUCCEED))))))

(DECLARE (UNSPECIAL THEXP))

(DEFUN THNV FEXPR (X) (THV1 (CAR X)))

(DECLARE (SPECIAL THTREE THEXP))

(DEFUN THOR FEXPR (THA) (AND THA
                 (THPUSH THTREE (LIST (QUOTE THOR) THA))
                 (SETQ THEXP (CAR THA))))

(DECLARE (UNSPECIAL THTREE THEXP))

(DECLARE (SPECIAL THTREE THEXP))

(DEFUN THOR2 (P) (COND (THMESSAGE (THPOPT) NIL)
               ((AND (CADAR THTREE) (CDADAR THTREE))
            (RPLACA (CDAR THTREE) (CDADAR THTREE))
            (SETQ THEXP (COND (P (PROG2 0.
                            (CAADAR THTREE)
                            (OR (CADAR THTREE)
                            (THPOPT))))
                      ((CAR (CAADAR THTREE))))))
               (T (THPOPT) NIL)))

(DECLARE (UNSPECIAL THTREE THEXP))

(DEFUN THORF NIL (THOR2 T))

(DECLARE (SPECIAL THVALUE))

(DEFUN THORT NIL (THPOPT) THVALUE)

(DECLARE (UNSPECIAL THVALUE))

(DECLARE (SPECIAL THTREE))

(DEFUN THPOPT NIL (SETQ THTREE (CDR THTREE)))

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THTREE))

(DEFUN THPROG
       FEXPR
       (THA)

       ;;THBIND HACKS THALIST TO BIND THE VARIABLES
       ;;IT ALSO HACKS THTREE SO WE CAN UNDO IT IF NEEDED
       (THBIND (CAR THA))

       ;;PUT THPROG MARK ON THTREE
       ;;THE FIRST THA IS A POINTER ONE BEFORE
       ;;THE NEXT PART OF THE THPROG TO BE HANDELED
       ;;THE SECOND ONE WILL BE KEPT WHOLE TO SEARCH FOR PROG TAGS
       (THPUSH THTREE (LIST (QUOTE THPROG) THA NIL THA))

       ;;CALL WORKHORSE
       (THPROGA))

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THEXP THVALUE THTREE))

(DEFUN THPROGA
       NIL
       ((LAMBDA (X) (COND
              ;;ODD CASE WHERE THE THPROG HAS NO SUBEXPRESSIONS
              ;;RETURN SUCCESS
              ((NULL (CDAR X)) (THPOPT) (QUOTE THNOVAL))

              ;;NEXT ITEM IS AN ATOM, HENCE A THPROG TAG
              ((ATOM (CADAR X))

               ;;USE THEXP TO MARK IT ON THTREE
               (SETQ THEXP (LIST (QUOTE THTAG) (CADAR X)))

               ;;MOVE POINTER TO NEXT EXPRESSION
               (RPLACA X (CDAR X))
               THVALUE)

              ;;OTHERWISE NEXT EXPRESSION TO BE EVALUATED IS
              ;;THE NEXT EXPRESSION OF THE THPROG
              (T (SETQ THEXP (CADAR X))

                 ;;MOVE POINTER TO NEXT EXPRESSION
                 (RPLACA X (CDAR X))
                 THVALUE)))
    (CDAR THTREE)))

(DECLARE (UNSPECIAL THEXP THVALUE THTREE))

;;THBRANCH AND THBRANCHUN ARE THE MAIN FUNCTIONS
;;IN CHARGE OF HANDELING THE EFFECTS OF SUCCESS AND FAILURE
;;THEY ARE ONLY CALLED BY THPROGT AND F

(DEFUN THPROGF NIL (THBRANCHUN) NIL)

(DEFUN THPROGT NIL (THBRANCH) (THPROGA))

(DECLARE (SPECIAL XX))

(DEFUN THPURE

       ;;CHECKS TO MAKE SURE THAT THE PATTERN HAS NO
       ;;UNASSIGNED VARIABLES IN IT.
       (XX)

       ;;XX, NATURALLY ENOUGH IS THE PATTERN
       ;;SINCE THPURE IS ALWAYS CALLED AFTER THVARSUBST
       ;;ANY VARIABLES WHICH DO HAVE ASSIGNMENTS WILL HAVE
       ;;GONE AWAY, RREPLACED BY THEIR ASSIGNMENTS
       ;;SO ALL WE NEED DO IS LOOK FOR ANY VARIABLES APPEARING AT ALL
       (ERRSET (MAPC (FUNCTION (LAMBDA (Y) (AND (THVAR Y) (ERR NIL))))
             XX)))

(DECLARE (UNSPECIAL XX))

(DECLARE (SPECIAL THTREE))

(DEFUN THPUTPROP
       (ATO VAL IND)
       (THPUSH THTREE
           (LIST (QUOTE THMUNG)
             (LIST (LIST (QUOTE PUTPROP)
                 (LIST (QUOTE QUOTE) ATO)
                 (LIST (QUOTE QUOTE) (GET ATO IND))
                 (LIST (QUOTE QUOTE) IND)))))
       (PUTPROP ATO VAL IND))

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THBS THON THAL THFST THNF THWH))

(DECLARE (SPECIAL THFSTP))

(DECLARE (SPECIAL THPC))

(DEFUN THREM1

       ;;THIS FUNCTION IS ROUGHLY THE SAME AS
       ;;THIP, EXCEPT WHILE THIP ADDS ASSERTIONS TO THE
       ;;DATABASE, THREM1 REMOVES THEM
       ;;HENCE ALL COMMENTS WILL BE GUIDES TO
       ;;THE CORRESPONDENCE BETWEEN THREM1 AND THIP
       (THB)

       ;;THB = THI IN THIP
       (PROG (THA THSV THA1 THA2 THA3 THA4 THA5 THONE THPC)

         ;;THA AND THA1 DO THE WORK OF THT1 IN THIP
         ;;THA1 = THT2
         ;;THA3 = THT3
         ;;THA4,5 , THONE, AND THPC ARE NEW
         (SETQ THNF (ADD1 THNF))

         ;;THIS COND SERVES THE SAME PURPOSE AS THE
         ;;FIRST COND IN THIP
         (COND ((AND (ATOM THB)
             (NOT (EQ THB (QUOTE ?)))
             (NOT (NUMBERP THB)))
            (SETQ THA THB))
           ((OR (EQ THB (QUOTE ?))
            (MEMQ (CAR THB) (QUOTE (THV THNV))))
            (COND (THFST (RETURN (QUOTE THVRB)))
              ((SETQ THA (QUOTE THVRB)))))
           ((RETURN (QUOTE THVRB))))

         ;;ALL THE REST SERVES THE SAME PURPOSE AS THE
         ;;SECOND COND IN THIP IT WAS ORRIGINALLY
         ;;WRITTEN AS A SINGLE COND, BUT THE
         ;;COMPILER BARFED ON IT SO IT
         ;;WAS BROKEN UP INTO BITE SIZE PIECES
         (SETQ THA1 (GET THA THWH))
         (OR THA1 (RETURN NIL))
         (AND (EQ THA1 (QUOTE THNOHASH)) (RETURN (QUOTE THBQF)))
         (SETQ THA2 (THBA THNF THA1))
         (OR THA2 (RETURN NIL))
         (SETQ THA3 (THBA THAL (CADR THA2)))
         (OR THA3 (RETURN NIL))
         (SETQ THA4 (CADR THA3))
         (SETQ THPC (NOT (EQ THWH (QUOTE THASSERTION))))
         (SETQ THA5
           (COND ((OR THFST THFSTP) (THBAP THBS (CDR THA4)))
             ((THBA (COND (THPC THON) (T (CAR THON)))
                (CDR THA4)))))
         (OR THA5 (RETURN NIL))
         (SETQ THONE (CADR THA5))
         (RPLACD THA5 (CDDR THA5))
         (AND (NOT (EQ (CADR THA4) 1.))
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

(DECLARE (UNSPECIAL THPC THBS THON THAL THFST THFSTP THNF THWH))

(DECLARE (SPECIAL THALIST THTREE))

(DEFUN THREMBINDF NIL (SETQ THALIST (CADAR THTREE)) (THPOPT) NIL)

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THTREE THVALUE))

(DEFUN THREMBINDT NIL (SETQ THALIST (CADAR THTREE)) (THPOPT) THVALUE)

(DECLARE (UNSPECIAL THALIST THTREE THVALUE))

(DECLARE (SPECIAL THBS THON THAL THFSTP THFST THNF THWH))

(DEFUN THREMOVE

       ;;THIS FUNCTION IS ANALAGOUS TO THADD EXCEPT
       ;;THREMOVE REMOVES RATHER THAN ADDS
       ;;AS WITH THREM1, ALL COMMENTS WILL EXPLICATE THE ANALOGY
       ;;SO ONE SHOULD FIRST BECOME FAMILIAR WITH THADD
       (THB)

       ;;THB = THTT IN THADD, THREMOVE TAKES ONLY ONE
       ;;ARG SINCE THE PROPERTY LIST FOR THE ASSERTION PLAYS NO ROLE
       ;;IN REMOVING THE ASSERTION
       (PROG (THB1 THWH THNF THAL THON THBS THFST THFSTP THFOO)

         ;;THB1 AND THON TOGETHER SHARE THE WORK OF THT1 AND THCK IN THADD
         ;;THAL = THLAS
         ;;THBS = THTTL
         (SETQ THNF 0.)

         ;;THE STRUCTURE OF THE TWO PROGRAMS IS VIRTUALLY IDENTICAL
         (SETQ THB1
           (COND ((ATOM THB)
              (SETQ THBS THB)
              (SETQ THWH
                (CAR (SETQ THB1
                       (GET THB
                        (QUOTE THEOREM)))))
              (CADDR THB1))
             ((SETQ THWH (QUOTE THASSERTION))
              (SETQ THBS THB))))
         (SETQ THAL (LENGTH THB1))
         (SETQ THFST T)
    THP1 (COND ((NULL THB1) (SETQ THB1 THFOO)
                (SETQ THNF 0.)
                (SETQ THFST (SETQ THFOO NIL))
                (SETQ THFSTP T)
                (GO THP1))
           ((NULL (SETQ THON (THREM1 (CAR THB1))))
            (RETURN NIL))
           ((MEMQ THON (QUOTE (THBQF THVRB)))
            (SETQ THFOO
              (NCONC THFOO
                 (LIST (COND ((EQ THON (QUOTE THVRB))
                          (CAR THB1))))))
            (SETQ THB1 (CDR THB1))
            (GO THP1)))
         (SETQ THFST NIL)
         (MAPC (FUNCTION THREM1) (CDR THB1))
         (SETQ THNF 0.)
         (MAPC (FUNCTION THREM1) THFOO)
         (RETURN THON)))

(DECLARE (UNSPECIAL THBS THON THAL THFST THFSTP THNF THWH))

(DECLARE (SPECIAL THTREE))

(DEFUN THREMPROP
       (ATO IND)
       (THPUSH THTREE
           (LIST (QUOTE THMUNG)
             (LIST (LIST (QUOTE PUTPROP)
                 (LIST (QUOTE QUOTE) ATO)
                 (LIST (QUOTE QUOTE) (GET ATO IND))
                 (LIST (QUOTE QUOTE) IND)))))
       (REMPROP ATO IND))

(DECLARE (UNSPECIAL THTREE))

(DECLARE (SPECIAL THALIST))

(DEFUN THRESTRICT
       FEXPR
       (THB)
       (PROG (X)
         (COND ((ATOM (SETQ X (THGAL (CAR THB) THALIST)))
            (THPRINTC 'THRESTRICT/ IGNORED/ -/ CONTINUING))
           ((THRPLACD (CDR X) (THUNION (CDDR X) (CDR THB)))))
         (RETURN X)))

(DECLARE (UNSPECIAL THALIST))

(DEFUN THRETURN FEXPR (X) (APPLY (QUOTE THSUCCEED)
                 (CONS (QUOTE THPROG) X)))

(DECLARE (SPECIAL THTREE THML))

(DEFUN THRPLACA (X Y) (PROG (THML)
                (THRPLACAS X Y)
                (THPUSH THTREE (LIST (QUOTE THMUNG) THML))
                (RETURN X)))

(DECLARE (UNSPECIAL THTREE THML))

(DECLARE (SPECIAL THML))

(DEFUN THRPLACAS
       (X Y)
       (THPUSH THML (LIST (QUOTE THURPLACA) X (CAR X)))
       (RPLACA X Y))

(DEFUN THURPLACA FEXPR (L) (RPLACA (CAR L) (CADR L)))

(DECLARE (UNSPECIAL THML))

(DECLARE (SPECIAL THTREE THML))

(DEFUN THRPLACD (X Y) (PROG (THML)
                (THRPLACDS X Y)
                (THPUSH THTREE (LIST (QUOTE THMUNG) THML))
                (RETURN X)))

(DECLARE (UNSPECIAL THTREE THML))

(DECLARE (SPECIAL THML))

(DEFUN THRPLACDS
       (X Y)
       (THPUSH THML (LIST (QUOTE THURPLACD) X (CDR X)))
       (RPLACD X Y))

(DEFUN THURPLACD FEXPR (L) (RPLACD (CAR L) (CADR L)))

(DECLARE (UNSPECIAL THML))

(DECLARE (SPECIAL THTREE THALIST THVALUE THML))

(DEFUN THSETQ
       FEXPR
       (THL1)
       (PROG (THML THL)
         (SETQ THL THL1)
    LOOP (COND ((NULL THL)
            (THPUSH THTREE (LIST (QUOTE THMUNG) THML))
            (RETURN THVALUE))
           ((NULL (CDR THL))
            (PRINT THL1)
            (THERT ODD NUMBER OF GOODIES - THSETQ))
           ((ATOM (CAR THL))
            (THPUSH THML (LIST (QUOTE SETQ)
                       (CAR THL)
                       (LIST (QUOTE QUOTE)
                         (EVAL (CAR THL)))))
            (SET (CAR THL) (SETQ THVALUE (EVAL (CADR THL)))))
           (T (THRPLACAS (CDR (THSGAL (CAR THL)))
                 (SETQ THVALUE
                       (THVAL (CADR THL) THALIST)))))
         (SETQ THL (CDDR THL))
         (GO LOOP)))

(DECLARE (UNSPECIAL THTREE THALIST THVALUE THML))

(DECLARE (SPECIAL X THALIST))

(DEFUN THSGAL
 (X)
 (SASSQ (CADR X)
    THALIST
    (FUNCTION (LAMBDA NIL (PROG (Y)
                    (SETQ Y
                      (LIST (CADR X)
                        (QUOTE THUNASSIGNED)))
                    (NCONC (GET (QUOTE THALIST)
                        (QUOTE VALUE))
                       (LIST Y))
                    (RETURN Y))))))

(DECLARE (UNSPECIAL X THALIST))

(DECLARE (SPECIAL THINDICATORS THP THWH THATOM))

(DEFUN THSTATE
 FEXPR
 (THINDICATORS)                                   ;PRINTS THAT PART OF THE STATE OF THE
                                       ;MICRO-PLANNER WORLD SPECIFIED BY THE INDICATORS
                                       ;IN REREADABLE FORM. NOTE THAT         ;IT IS  ;BLIND TO ASSERTIONS THAT BEGIN WITH
 (PROG (THP)                                   ;;EITHER NUMBERS, LIST STRUCTURE, NOHASHED ATOMS
       (PRINT (QUOTE (THDATA)))                           ;OR NON-INTERNED ATOMS.
       (MAPC
    (FUNCTION
     (LAMBDA (BUCKET)
      (MAPC
       (FUNCTION
        (LAMBDA (THATOM)
         (MAPC
          (FUNCTION
           (LAMBDA (THWH)
        (AND
         (SETQ THP (GET THATOM THWH))
         (SETQ THP (ASSOC 1. (CDR THP)))
         (MAPC
          (FUNCTION
           (LAMBDA (LENGTH-BUCKET)
            (MAPC
             (FUNCTION (LAMBDA (ASRT)
                       (COND ((EQ THWH
                          (QUOTE THASSERTION))
                          (PRINT ASRT))
                         ((PRINT (LIST ASRT))))))
             (CDDR LENGTH-BUCKET))))
          (CDR THP)))))
          (COND (THINDICATORS)
            (' (THASSERTION THANTE THCONSE THERASING))))))
       BUCKET)))
    (MAKOBLIST NIL))
       (PRINT NIL)))

(DECLARE (UNSPECIAL THINDICATORS THP THWH THATOM))

(DECLARE (SPECIAL THTREE THALIST THBRANCH THABRANCH THA))

(DEFUN THSUCCEED
       FEXPR
       (THA)
       (OR (NOT THA)
       (PROG (THX)
         (AND (EQ (CAR THA) (QUOTE THEOREM))
              (SETQ THA (CONS (QUOTE THPROG) (CDR THA))))
         (SETQ THBRANCH THTREE)
         (SETQ THABRANCH THALIST)
        LOOP (COND ((NULL THTREE) (PRINT THA)
                      (THERT OVERPOP - THSUCCEED))
               ((EQ (CAAR THTREE) (QUOTE THREMBIND))
            (SETQ THALIST (CADAR THTREE))
            (THPOPT)
            (GO LOOP))
               ((EQ (CAAR THTREE) (CAR THA))
            (THPOPT)
            (RETURN (COND ((CDR THA) (EVAL (CADR THA)))
                      ((QUOTE THNOVAL)))))
               ((AND (EQ (CAR THA) (QUOTE THTAG))
                 (EQ (CAAR THTREE) (QUOTE THPROG))
                 (SETQ THX (MEMQ (CADR THA)
                         (CADDDR (CAR THTREE)))))
            (RPLACA (CDAR THTREE) (CONS NIL THX))
            (RETURN (THPROGT)))
               (T (THPOPT) (GO LOOP))))))

(DECLARE (UNSPECIAL THTREE THALIST THBRANCH THABRANCH THA))

(DECLARE (SPECIAL XX TYPE THX THY1 THY THXX))

(DEFUN THTAE
 (XX)
 (COND
  ((ATOM XX) NIL)
  ((EQ (CAR XX) (QUOTE THUSE))
   (MAPCAR
    (FUNCTION (LAMBDA (X)
              (COND ((NOT (AND (SETQ THXX
                         (GET X (QUOTE THEOREM)))
                       (EQ (CAR THXX) TYPE)))
                 (PRINT X)
                 (LIST 'THAPPLY
                   (THERT BAD THEOREM /-THTAE)
                   (CAR THX)))
                (T (LIST (QUOTE THAPPLY) X (CAR THX))))))
    (CDR XX)))
  ((EQ (CAR XX) (QUOTE THTBF))
   (MAPCAN (FUNCTION (LAMBDA (Y) (COND (((CADR XX) Y)
                    (LIST (LIST (QUOTE THAPPLY)
                            Y
                            (CAR THX)))))))
       (COND (THY1 THY) ((SETQ THY1 T)
                 (SETQ THY (THMATCHLIST (CAR THX) TYPE))))))
  (T (PRINT XX) (THTAE (THERT UNCLEAR RECCOMMENDATION /-THTAE)))))

(DECLARE (UNSPECIAL XX TYPE THX THY1 THY THXX))

(DECLARE (SPECIAL THTREE))

(DEFUN THTAG FEXPR (L) (AND (CAR L)
                (THPUSH THTREE
                    (LIST (QUOTE THTAG) (CAR L)))))

(DECLARE (UNSPECIAL THTREE))

(DEFUN THTAGF NIL (THPOPT) NIL)

(DECLARE (SPECIAL THVALUE))

(DEFUN THTAGT NIL (THPOPT) THVALUE)

(DECLARE (UNSPECIAL THVALUE))

(DEFUN THTRUE (X) T)

(DECLARE (SPECIAL THTREE THOLIST THALIST))

(DEFUN THTRY1                                   ;TRIES NEXT RECOMMENDATION ON TREE FOR THGOAL
       NIL
       (PROG (THX THY THZ THW THEOREM)
         (SETQ THZ (CAR THTREE))                       ;= (THGOAL PATTERN EXPANDED-RECOMMENDATIONS)
         (SETQ THY (CDDR THZ))                       ;= RECOMMENDATIONS
         (RPLACD THY (SUB1 (CDR THY)))
    NXTREC
         (COND ((OR (NULL (CAR THY)) (ZEROP (CDR THY)))
            (RETURN NIL)))                       ;RECOMMENDATIONS EXHAUSTED. FAIL
         (SETQ THX (CAAR THY))
         (GO (CAR THX))
    THNUM(RPLACD THY (CADR THX))
         (RPLACA THY (CDAR THY))
         (GO NXTREC)
    THDBF(SETQ THOLIST THALIST)
         (COND ((NULL (CADDR THX)) (RPLACA THY (CDAR THY))
                       (GO NXTREC))               ;NO MORE CANDIDATES SATISFYING THIS REC.
           ((PROG2 0.                           ;TRY NEXT REC
               (AND ((CADR THX) (SETQ THW (CAADDR THX)))
                (THMATCH1 (CADR THZ) (CAR THW)))
               (RPLACA (CDDR THX) (CDADDR THX)))
            (RETURN THW))
           (T (GO THDBF)))
    THTBF(COND ((NULL (CADDR THX)) (RPLACA THY (CDAR THY))
                       (GO NXTREC)))               ;NO MORE CANDIDATES SATISFYING THIS REC.
         (SETQ THEOREM (CAADDR THX))
    THTBF1
         (COND ((NOT (AND (SETQ THW                       ;TRY NEXT REC
                    (GET THEOREM (QUOTE THEOREM)))
                  (EQ (CAR THW) (QUOTE THCONSE))))
            (PRINT THEOREM)
            (COND ((EQ (SETQ THEOREM
                     (THERT BAD THEOREM - THTRY1))
                   'T)
               (GO NXTREC))
              (T (GO THTBF1)))))
         (COND ((PROG2 0.
               (AND ((CADR THX) (CAADDR THX))
                (THAPPLY1 THEOREM THW (CADR THZ)))
               (RPLACA (CDDR THX) (CDADDR THX)))
            (RETURN T))
           (T (GO THTBF)))))

(DECLARE (UNSPECIAL THTREE THOLIST THALIST))

(DECLARE (SPECIAL THZ1 THZ THY1 THY THA2))

(DEFUN THTRY

 ;;THTRY IS IN CHARGE OF MAKING UP THE "THINGS TO DO" LIST
 ;;WHICH IS PUT ON THTREE.  SO WHENEVER WE FAIL BACK
 ;;TO A THGOAL, WE GO TO THE NEXT "THING TO DO"
 (X)

 ;;X IS THE LIST OF RECOMMENDATIONS
 (COND ;;ANY ATOMIC RECOMMENDATION IS IGNORED,  THIS
       ;;IS USEFUL IN ERROR RECOVERY
       ((ATOM X) NIL)

       ;;HAVE A THEOREM BASE FILTER
       ((EQ (CAR X) (QUOTE THTBF))

    ;;MAKE UP A LIST WHICH GIVES, 1 - THE INDICATOR "THTBF"
    ;; 2 - THE ACTUAL FILTER (THTRUE IS THE MOST COMMON)
    ;; 3 - THE BUCKET RETURNED BY THMATCHLIST
    (COND ((NOT THZ1) (SETQ THZ1 T) (SETQ THZ (THMATCHLIST THA2 'THCONSE))))
    (COND (THZ (LIST (LIST 'THTBF (CADR X) THZ))) (T NIL)))

       ;;DO THE SAME THING, ONLY FOR DATA BASE FILTERS
       ((EQ (CAR X) (QUOTE THDBF))
    (COND ((NOT THY1) (SETQ THY1 T) (SETQ THY (THMATCHLIST THA2 'THASSERTION))))
    (COND (THY (LIST (LIST 'THDBF (CADR X) THY))) (T NIL)))

       ;;THUSE STATEMENTS ARE TRANSLATED INTO THTBF THTRUE
       ;;STATEMENTS, WHICH THE "BUCKET" IS THE LIST GIVEN IN THE THUSE
       ((EQ (CAR X) (QUOTE THUSE))
    (LIST (LIST (QUOTE THTBF) (QUOTE THTRUE) (CDR X))))
       ((EQ (CAR X) 'THNUM) (LIST X))
       (T (PRINT X) (THTRY (THERT UNCLEAR RECOMMENDATION - THTRY)))))

(DECLARE (UNSPECIAL THZ1 THZ THY1 THY THA2))

(DECLARE (SPECIAL THTREE THALIST THXX))

(DEFUN THUNDOF
       NIL
       (COND ((NULL (CADDAR THTREE)) (THPOPT))
         (T (SETQ THXX (CDDAR THTREE))
        (SETQ THALIST (CAADR THXX))
        (RPLACA (CDR THXX) (CDADR THXX))
        (SETQ THTREE (CAAR THXX))
        (RPLACA THXX (CDAR THXX))))
       NIL)

(DECLARE (UNSPECIAL THTREE THALIST THXX))

(DEFUN THUNDOT NIL (THPOPT) T)

(DECLARE (SPECIAL THALIST))

(DEFUN THUNIQUE
       FEXPR
       (THA)
       (SETQ THA (CONS (QUOTE THUNIQUE) (MAPCAR (FUNCTION EVAL) THA)))
       (PROG (X)
         (SETQ X THALIST)
    LP   (COND ((NULL X) (THPUSH THALIST THA) (RETURN T))
           ((EQ (CAAR X) (QUOTE THUNIQUE))
            (COND ((EQUAL (CAR X) THA) (RETURN NIL)))))
         (SETQ X (CDR X))
         (GO LP)))

(DECLARE (UNSPECIAL THALIST))

(DECLARE (SPECIAL THALIST THXX))

(DEFUN THV1
 (X)                                       ;(THV1 'X) IS THE VALUE OF THE PLANNER VARIABLE
 (SETQ THXX X)                                   ;$?X RETURNS ERROR MESSAGE IF X UNBOUND OR
 (COND ((EQ (SETQ X (CADR (SASSQ X                       ;UNASSIGNED
                 THALIST
                 (FUNCTION (LAMBDA NIL
                           (PRINT THXX)
                           (THERT THUNBOUND
                              -
                              THV1))))))
        (QUOTE THUNASSIGNED))
    (PRINT THXX)
    (THERT THUNASSIGNED - THV1))
       (T X)))

(DECLARE (UNSPECIAL THALIST THXX))

(DEFUN THV
       FEXPR
       (X)                                   ;(THV X) IS THE VALUE OF THE PLANNER VARIABLE
       (THV1 (CAR X)))                               ;$?X

(DECLARE (SPECIAL THLEVEL
          THSTEP
          THSTEPF
          THSTEPT
          THSTEPD
          THMESSAGE
          ^A
          THV
          THINF
          THE
          THTREE
          THOLIST
          THEXP
          THALIST
          THVALUE
          THBRANCH
          THABRANCH))

(DEFUN THVAL

       ;;CORESPONDS TO LISP EVAL
       ;;THEXP IS THE EXPRESSION TO BE THVALUATED
       ;;THALIST IS THE VARIABLE BINDING LIST
       (THEXP THALIST)

       ;;ALL THPUSH DOES IS TO CONSE ON THE SSECOND ITEM TO THE FIRST
       (THPUSH THLEVEL (LIST THTREE THALIST))
       (PROG (THTREE THVALUE THBRANCH THOLIST THABRANCH THE THMESSAGE)
         (SETQ THV (QUOTE (THV THNV)))
         (SETQ THVALUE 'THNOVAL)

         ;;THE BECOMES THE CURRENT EXPRESSION
         ;;THEXP IS RESERVED FOR FURTHER EXPRESSIONS
         ;;WHICH SHOULD BE THVALED BEFORE WE GO TO THE NEXT
         ;;ITEM OF ACTUAL CODE.  FOR EXAMPLE, THASSERT USES
         ;;THIS FEATURE TO PROCESS ANTECEDENT THEOREMS
    GO   (SETQ THE THEXP)
         (SETQ THEXP NIL)

         ;;TYPING ^A (CONTROL A) AT MAC-AI LISP CAUSES ^A (UPARROW A)
         ;;TO BE SET TO T. THIS CAN BE DONE WHILE A FUNCTION
         ;;IS BEING PROCESSED.  THE NET EFFECT IS TO TEMPORARILY
         ;;HALT EVALUAION
         (COND (^A (SETQ ^A NIL)
               (OR (THERT ^A - THVAL) (GO FAIL))))

         ;;THSTEP AND ITS RELATIVES ARE FOR STEPPING THROUGH
         ;;PLANNER FUNCTIONS IN A SPECIAL WAY.  TO THIS DATE
         ;;ONLY SUSSMAN KNOWS EXACTLY WHAT IT IS SUPPOSE TO DO
         ;;YOU CAN SAFELY IGNORE ANY EXPRESSION WHICH MENTIONS IT
         (COND (THSTEP (EVAL THSTEP)))

         ;;EVAL THE CURRENT EXPRESSION TO BE THVALED.  NOTE
         ;;THAT EACH PLANNER FUNCTION CORESPONDS TO THREE LISP FUNCTIONS
         ;;ONE TO SET THINGS UP (THIS IS WHAT IS GETTING EVALED AT THIS POINT
         ;;ONE TO HANDLE SUCCESS AND ONE FOR FAILURE
         (COND ((ERRSET (SETQ THVALUE (EVAL THE))))

           ;;IF THERE WAS A LISP ERROR, REPORT IT TO THE USER
           (T (PRINT THE)
              (SETQ THVALUE (THERT LISPERROR - THVAL))))
    GO1  (COND (THSTEPD (EVAL THSTEPD)))

         ;;USUALLY THEMESSAGE WILL BE NIL.  EXCEPTION IS WHEN
         ;;USER HAS USED THE THMESSAGE FUNCTION
         (COND (THMESSAGE (GO MFAIL))

           ;;IF THEXP IS NON NIL IT MEANS THAT WE HAVE
           ;;MORE PLANNER TO WORK ON BEFORE GOING TO NEXT LINE OF USER CODE
           (THEXP (GO GO))

           ;;IF THVALUE IS NON NIL IT MEANS THAT SO FAR THE THEOREM IS SUCCEEDING
           (THVALUE (GO SUCCEED))

           ;;ELSE WE ARE IN A FAILURE SITUATION
           (T (GO FAIL)))

         ;;HANDLES SUCCESS
    SUCCEED
         (COND (THSTEPT (EVAL THSTEPT)))

         ;;SAVE CURRENT STATE OF THTREE AND THALIST IN CASE
         ;;WE HAVE TO BACK UP
         (COND ((NULL THBRANCH) (SETQ THBRANCH THTREE)
                    (SETQ THABRANCH THALIST)))

         ;;IF THE THTREE IS NIL IT MEANS THAT THE THPROG OR WHATEVER HAS BEEN
         ;;COMPLETED SO THERE ARE NO MORE EXPRESSIONS TO DO,
         ;;ALL THEOREMS ACT LIKE A THPROG, INCLUDING PUTTING
         ;;ITS MARK ON THTREE SEE THAPPLY
         ;;HENCE NO NEED TO GROW MORE BRANCHES ON THTREE
         (COND ((NULL THTREE) (SETQ THLEVEL (CDR THLEVEL))
                  (RETURN THVALUE))

           ;;THIS IS THE NORMAL CASE.  WE EVAL THE SUCCEED-FUNCTION
           ;;OF THE PLANNER FUNCTION WHICH JUST SUCCEEDED
           ((SETQ THEXP (GET (CAAR THTREE) (QUOTE THSUCCEED)))
            (GO GO2))

           ;;IN CASE OF LOSSAGE LETS THE USER SUCCEED ANYWAY
           ((THERT BAD SUCCEED - THVAL) (GO SUCCEED))
           ((GO FAIL)))

         ;;HAS TO DO WITH FAILURE + MESSAGE
    MFAIL(COND ((EQ (CAR THMESSAGE) THTREE)
            (SETQ THEXP (CADR THMESSAGE))
            (SETQ THMESSAGE NIL)
            (GO GO)))
    FAIL (COND (THSTEPF (EVAL THSTEPF)))

         ;;IF THTREE IS NIL WE HAVE FAILED THE ENTIRE EXPRESSION
         (COND ((NULL THTREE) (SETQ THLEVEL (CDR THLEVEL))
                  (RETURN NIL))

           ;;NORMAL CASE, EVAL THE FAILURE FUNCTION ASSOCIATED
           ;;WITH THE PLANNER FUNCTION WHICH JUST FAILED
           ((SETQ THEXP (GET (CAAR THTREE) (QUOTE THFAIL)))
            (GO GO2))
           ((THERT BAD FAIL - THVAL) (GO SUCCEED))
           ((GO FAIL)))

         ;;THEXP AT THIS POINT IS THE APPROPRIATE SUCCESS OR
         ;;FAILURE ASSOCIATED FUNCTION.  EVAL IT AND AT THE SAME
         ;;TIME, SET IT TO NIL IN CASE WE NEED THEXP FOR MORE EXPRESSIONS
         ;;TO BE PROCESSED
    GO2  (SETQ THVALUE ((PROG2 0. THEXP (SETQ THEXP NIL))))

         ;;GO THROUGH ENTIRE PROCESS AGAIN
         ;;A TYPICAL PROCESS IN SUCCESS IS TO KEEP REMOVING EXPRESSIONS FROM THTREE UNTIL
         ;;WE GET BACK TO THE THREE ENTRY PUT ON BY THPROG
         ;;AT THIS POIN IT EVALS THPROGT, AND SEE THAT LISTING
         (GO GO1)))
(DECLARE (UNSPECIAL THSTEP
            THSTEPF
            THSTEPT
            THSTEPD
            THLEVEL
            THMESSAGE
            ^A
            THV
            THINF
            THE
            THTREE
            THOLIST
            THEXP
            THALIST
            THVALUE
            THBRANCH
            THABRANCH))

(DEFUN THVAR
       (X)                                   ;PREDICATE - IS ITS INPUT A PLANNER VARIABLE
       (MEMQ (CAR X) (QUOTE (THV THNV))))

(DECLARE (SPECIAL THALIST THY))

(DEFUN THVARS2

       ;;THIS IS THE WORKHORSE FOR THVARSUBST
       (X)

       ;;X IS A SINGLE ITEM FROM A PATTERN
       (PROG (A)
         (AND (ATOM X) (RETURN X))

         ;;IF ITS AN ATOM NOTHING NEED BE DONE
         (AND (EQ (CAR X) (QUOTE THEV))
          (SETQ X (THVAL (CADR X) THALIST)))

         ;;IF THE EXPRESSION HAS A $E BEFORE IT, THVAL BEFORE GOING ON
         (OR (THVAR X) (RETURN X))

         ;;IF THE ITEM IS NOT A VARIABLE IT MUST BE
         ;;SOME RANDOM LIST, SO IT HAS NO  ASSIGNED VALUE
         (SETQ A (THGAL X THALIST))

         ;;AT THIS POINT X MUST BE A VARIABLE, SO FIND ITS
         ;;ASSIGNMENT, THATS WHAT THGAL DOES
         ;;THALIST IS WHERE THE VARIABLE ASSIGNMENTS RESIDE
         (RETURN (COND ((EQ (CADR A) (QUOTE THUNASSIGNED)) X)

               ;;IF THE VARIABLE IS UNASSIGNED
               ;;THEN RETURN THE ACTUAL VARIABLE
               ((AND THY (EQ (CAR X) 'THNV))

                ;;THY WILL BE T JUST IN THE CASES
                ;;WHERE THVARSUBST WAS CALLED BY A THGOAL SITUATION
                ;;IT IS THEN NECESSARY TO IMMEDIATELY HACK IN A
                ;;THUNASSIGNED SO THAT IF THE SAME VARIABLE IS USED
                ;;TWICE IN THE SAME PATTERN WE WON'T PUT
                ;;IN ITS OLD VALUE THE SECOND TIME IT IS ENCOUNTERED
                (THRPLACA (CDR A) 'THUNASSIGNED)
                X)

               ;;OTHERWISE THE ASSIGNMENT IS THE SECOND ELEMENT
               ;;IN THE BINDING LIST
               (T (CADR A))))))

(DEFUN THVARSUBST
       (THX THY)

       ;;THX IS A GOAL OR ASSERTION PATTERN OR THEOREM NAME
       ;;THIS FUNCTION RETURNS THE SAME PATTERN, EXCEPT
       ;;IN PLACE OF ALL ASSIGNED VARIABLES WILL BE THE
       ;;VALUES THEY ARE ASSIGNED TO
       (COND ((EQ (CAR THX) (QUOTE THEV))

          ;;IF THE CAR IS THEV IT MEANS THAT THERE WAS
          ;;A $E BEFORE THE PATTERN, IN WHICH CASE WE
          ;;ARE TO GET THE REAL PATTERN BY THVALUATING WHAT
          ;;IS THERE
          (SETQ THX (THVAL (CADR THX) THALIST)))
         ((THVAR THX) (SETQ THX (EVAL THX))))

       ;;THVAR TESTS TO SEE IF ARG IS A VARIABLE
       ;;IF THE PATTERN IS A SINGLE VARIABLE THE PROGRAM ASSUMES
       ;;THERE SHOULD BE AN IMPLICIT THVAL.
       ;;UNLESS THE ASSERTEE IS A THEOREM NAME
       ;;GO THROUGH IT PLACE BY PLACE WITH THVARS2
       (COND ((ATOM THX) THX) (T (MAPCAR (FUNCTION THVARS2) THX))))

(DECLARE (UNSPECIAL THALIST THY))

(DECLARE (SPECIAL THALIST THVALUE THA))

(DEFUN THVSETQ
       FEXPR
       (THA)
       (PROG (A)
         (SETQ A THA)
    LOOP (COND ((NULL A) (RETURN THVALUE))
           ((NULL (CDR A))
            (PRINT THA)
            (THERT ODD NUMBER OF GOODIES-THSETQ))
           (T (SETQ THVALUE
                (CAR (RPLACA (CDR (THSGAL (CAR A)))
                     (THVAL (CADR A) THALIST))))))
         (SETQ A (CDDR A))
         (GO LOOP)))

(DECLARE (UNSPECIAL THALIST THVALUE THA))

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

(DECLARE (SPECIAL THALIST THLEVEL THINF))
(DEFUN THERT
       FEXPR

       ;;THERT IS THE BREAK FUNCTION, AND ALSO THE TOP LEVEL FUNCTION
       ;;IT IS CALLED DIRECTLY BY LISP BEFORE LISP
       ;;GOES INTO THE READ EVAL LOOP.
       ;;FOR HOW THIS IS DONE, SEE MAC-AI LISP DOCUMENTATION
       ;;IN ESSENCE THERT CONTAINS ITS OWN LOOP, WHICH IS READ THVAL.
       (/0ERTA)

       ;;/0ERTA IS THE ERROR MESSAGE TO BE PRINTED
       ;;OUT WHEN THERT IS USED FOR ERROR BREAKING
       (PROG (/0LISTEN ^W ^Q)
         (PRINT (QUOTE >>>))
         (COND
           ;;SPECIAL MESSAGE PRINTOUT
           ((EQ (CAR /0ERTA) 'TH%0%)
            (MAPC (FUNCTION THPRINT2) (CDR /0ERTA))
            (IOC Q))

           ;;THE NORMAL MESSAGE PRINTOUT
           ((MAPC (FUNCTION THPRINT2) /0ERTA)
            (PRINT (QUOTE LISTENING))

            ;;IF WE ARE AT TOP LEVEL THLEVEL WILL BE NIL
            (OR THLEVEL (THPRINT2 (QUOTE THVAL)))))

         ;;GO INTO READ LOOP
    /0LISTEN
         (SETQ THINF NIL)

         ;;LINEFEED
         (TERPRI)

         ;;READ IN S EXPRESSION.
         (ERRSET (COND ((EQ (SETQ /0LISTEN (READ)) (QUOTE P))
                (RETURN T))                       ;$P IMPLIES PROCEDE
               ((AND (NOT (ATOM /0LISTEN))               ;($P EXP) IMPLIES PROCEDE AND OUTPUT (EVAL EXP)
                 (EQ (CAR /0LISTEN) (QUOTE P)))
                (RETURN (EVAL (CADR /0LISTEN))))
               (THLEVEL (PRINT (EVAL /0LISTEN)))           ;EVAL LISTENING IF NOT AT TOP LEVEL
               (T (PRINT (THVAL /0LISTEN THALIST)))))      ;THVAL LISTENING AT TOP LEVEL
         (GO /0LISTEN)))

(DECLARE (SPECIAL PURE
          LOW
          THXX
          THTRACE
          THALIST
          THTREE
          ERRLIST
          THGENAME
          THLEVEL))
(DEFUN THINIT
       FEXPR
       (L)
       (COND ((AND L PURE) (LAPPURIFY LOW (PAGEBPORG))
               (SETQ PURE NIL)))
       (SETQ THGENAME 0.)
       (SETQ THSTEP NIL)
       (SETQ THSTEPD NIL)
       (SETQ THSTEPT NIL)
       (SETQ THSTEPF NIL)
       (SETQ THXX NIL)
       (SETQ THTRACE NIL)
       (SETQ THALIST (QUOTE ((NIL NIL))))
       (SSTATUS MACRO $ (QUOTE THREAD))
       (SETQ ERRLIST
         (QUOTE ((PRINT (QUOTE MICRO-PLANNER))
             (PRINC THVERSION)
             (COND ((ERRSET (APPLY 'UREAD
                       (APPEND '(/.PLNR/.
                             /(INIT/))
                           (CRUNIT)))
                    NIL)
                (SETQ ERRLIST (CDDDDR ERRLIST))
                (SETQ THTREE NIL)
                (SETQ THLEVEL NIL)
                (THERT TH%0% READING /.PLNR/. /(INIT/))))
             (SETQ ERRLIST (CDDDDR ERRLIST))
             (SETQ THINF NIL)
             (SETQ THTREE NIL)
             (SETQ THLEVEL NIL)
             (THERT TOP LEVEL)))))

(DECLARE (UNSPECIAL PURE
            LOW
            THXX
            THTRACE
            THALIST
            ERRLIST
            THTREE
            THLEVEL
            THGENAME
            THINF))

§ anno/winograd/thtrac

(COMMENT FOR PLNR 159 AND GREATER, THPRINTC CAN BE ELIMINATED)

;SYSTEM FUNCTIONS SUCH AS THGOAL, THASSERT, THERASE AND THEOREM
;(ALL THMS) ARE TRACED IF THEY ARE ON 'THTRACE'.  THTRACES1 PUTS
;THEM THERE AND THUNTRACE TAKES THEM OFF.

;THTRACE IS INITIALLY SET TO NIL BY TS PLNR

(DEFUN THTRACE FEXPR (L) (MAPC (FUNCTION THTRACE1) L))

(DEFUN
 THTRACE1
 (X)
 (PROG (Y)

    ;VARIETY OF POSSIBLE INPUT FORMATS TRANSFORMED TO STANDARD
    ;3 ELEMENT LIST (OBJECT-TO-BE-TRACED TRACE-CONDITION BREAK CONDITION)
       (SETQ X (COND ((ATOM X) (LIST X T NIL))
             ((CDDR X) X)
             ((NULL (CDR X)) (PRINT X)
                     (PRINC 'BAD/ FORMAT)
                     (RETURN NIL))
             ((LIST (CAR X) (CADR X) NIL))))

    ;IF OBJECT-TO-BE-TRACED IS A PARTICULAR THEOREM, THEN THE TRIPLET
    ;'(THEOREM (THSEL 'CADR)(THSEL 'CADDDR)) IS GUARANTEED TO
    ;BE ON THTRACE IN ADDITION TO THE STANDARD TRIPLET
       (COND ((GET (CAR X) 'THEOREM)
          (COND ((SETQ Y (ASSQ 'THEOREM THTRACE))
             (RPLACD Y '((THSEL 'CADR)
                       (THSEL 'CADDR))))
            ((SETQ THTRACE
               (LIST X
                 (APPEND '(THEOREM (THSEL 'CADR)
                             (THSEL 'CADDR))
                     THTRACE)))))))

    ;THTRACE IS UPDATED. IF THE OBJECT-TO-BE-TRACED IS ALREADY ON
    ;THTHRACE THEN THE TRACE AND BREAK CONDITIONS ARE UPDATED.
    ;ELSE THE WHOLE TRIPLET IS PLACED ON THTRACE
       (COND ((SETQ Y (ASSQ (CAR X) THTRACE)) (RPLACD Y (CDR X)))
         ((SETQ THTRACE (CONS X THTRACE))))

       (RETURN X)))

;THUNTRACE REMOVES ELEMENTS OF ITS ARG FROM THTRACE
;IF NOT GIVEN ANY ARGS, THUNTRACE SETS THTRACE TO NIL
(DEFUN THUNTRACE
       FEXPR
       (L)
       (COND (L (SETQ THTRACE (MAPCAN (FUNCTION (LAMBDA (X)
                            (COND ((MEMQ (CAR X) L)
                                   (PRINT X)
                                   NIL)
                                  ((LIST X)))))
                      THTRACE)))
         ((MAPC (FUNCTION PRINT) THTRACE) (SETQ THTRACE NIL)))
       'DONE)
;THTRACES IS ACTIVATED BY THGOAL, THASSERT, ... IF THTRACE IS NON-NIL
;THF IS SET TO THE PARTICULAR CANDIDATE FOR TRACEAGE, E.G.
;TO 'THGOAL IF THE PLANNER FUNCTION THGOAL ACTIVATED THTRACES
;THL = THE INSTANTIATED ARG OF THF. SEE DESC OF X ON NEXT PAGE

(DEFUN THTRACES (THF THL) (PROG (THY THZ THB)
                (AND
                     ;THY SET TO TRIPLET ON THTRACE. IF NOT THERE, NO TRACING
                     (SETQ THY (ASSQ THF THTRACE))

                     ;IF BOTH TRACE AND BREAK ARE FALSE, DON'T TRACE
                     ;SIDE EFFECT - THB SET TO VALUE OF BREAK
                     (OR (SETQ THB (THVAL (CADDR THY) THALIST))
                     (THVAL (CADR THY) THALIST))

                     ;THZ IS SET TO THE TRACE FUNCTION FOR THE OBJECT-TO-BE-TRACED
                     (OR (SETQ THZ (GET THF 'THTRACE))
                     (THERT THTRACES - TRACE LOSSAG))

                     ;THE TRACE FN IS EXECUTED
                     (THZ THL THB)

                     ;IF THB IS NON-NIL, BREAK
                     THB
                     (THERT))))

;THE CAR OF THE TREE IS '(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;THUS, THESE TWO FNS PRINT THE NAME OF THE TRACE POINT, "FAIL"-OR-"SUCCEED"
;PRINT THVALUE IF ANY, AND FINALLY BREAK IF (THERT) IS PRESENT,
;THEN POP THE TREE

(DEFPROP THTRACES
     (LAMBDA NIL
         (PRINT (CADAR THTREE))
         (PRINC 'FAILED/ )
         (EVLIS (CDDAR THTREE))
         (THPOPT)
         NIL)
     THFAIL)

(DEFPROP THTRACES
     (LAMBDA NIL
         (PRINT (CADAR THTREE))
         (PRINC 'SUCCEEDED/ )
         (EVLIS (CDDAR THTREE))
         (THPOPT)
         THVALUE)
     THSUCCEED)
;THE TRACE FNS THBKPT, THGOAL, THEOREM, THASSERT, AND THERASE PUSH ONTO THE TREE
;'(THTRACES NAME-OF-TRACE-POINT OPTIONAL-PRINT-OF-THVALUE (THERT)-OR-NIL)
;X=THL=INSTANTIATED GOAL, ASSERTION OR ERASURE, NAME OF THE THM OR
;MESSAGE OF THE BREAKPOINT

(DEFPROP THBKPT
     (LAMBDA (X B)
         (THPUSH THTREE (LIST 'THTRACES
                      (THGENS B)
                      (AND B '(THERT))))
         (THPRINTC 'PASSING/ BKPT)
         (PRIN1 (CADAR THTREE))
         (PRINC '/ )
         ;BY SETTING THBRANCH AND THABRANCH, A TRIPLE IS CREATED
         ;BY THVAL FOR BACKTRACKING.  THEN, THE TREE IS POPPED
         ;TO PREVENT THTRACES FROM TYPING OUT THE MEANINGLESS
         ;THAT THE BREAKPOINT SUCCEEDED.
         (SETQ THBRANCH THTREE)
         (SETQ THABRANCH THALIST)
         (THPOPT)
         (PRIN1 X))
     THTRACE)

(DEFPROP THGOAL
     (LAMBDA (X B)
         (THPUSH THTREE (LIST 'THTRACES
                      (THGENS G)
                      '(AND THVALUE (PRIN1 THVALUE))
                      (AND B '(THERT))))
         (THPRINTC 'TRYING/ GOAL)
         (PRIN1 (CADAR THTREE))
         (PRINC '/ )
         (PRIN1 X))
     THTRACE)

(DEFPROP THEOREM
     (LAMBDA (X B)
         (THPUSH THTREE (LIST 'THTRACES
                      X
                      '(AND THVALUE (PRIN1 THVALUE))
                      (AND B '(THERT))))
         (THPRINTC 'ENTERING/ THEOREM)
         (PRIN1 X))
     THTRACE)

(DEFPROP THASSERT
     (LAMBDA (X B)
         (THPUSH THTREE (LIST 'THTRACES
                      (THGENS A)
                      (AND B '(THERT))))
         (PRINT 'ASSERTING)
         (PRIN1 (CADAR THTREE))
         (PRINC '/ )
         (PRIN1 X))
     THTRACE)

(DEFPROP THERASE
     (LAMBDA (X B)
         (THPUSH THTREE (LIST 'THTRACES
                      (THGENS E)
                      (AND B '(THERT))))
         (PRINT 'ERASING)
         (PRIN1 (CADAR THTREE))
         (PRINC '/ )
         (PRIN1 X))
     THTRACE);UTILITY FNS

;FOR THE TRACE-OBJECT 'THEOREM, IF ANY SPECIFIC THMS ARE TRACED,
;    '(THSEL 'CADR) AND '(THSEL 'CADDDR)
;ARE THE TRACE AND BREAK PREDICATES.  HENCE THTRACES CAUSES
;THESE EXPR'S TO BE THVALED. THL IS SET TO THE SECOND ARG
;OF THTRACES WHICH IN THIS CASE IS PRESUMABLY THE NAME OF
;THE PARTICULAR THM THAT ACTIVATED THTRACES. THSEL FIRST
;CHECKS TO SEE WHETHER THIS THM IS INDEPENDENTLY ON THTRACE.
;IF NOT, IT DOES NO MORE. BUT IF IT IS, THX GETS SET TO THE THM'S
;TRIPLET. THEN THX GETS SET TO EITHER THE TRACE (ARG='CADR) OR THE BREAK
;(ARG='CADDDR) CONDITION OF THE TRIPLET. FINALLY, THESE CONDITIONS ARE THVALED
;THUS, THSEL SERVES THE PURPOSE OF REFERENCING THE TRACE AND BREAK
;PREDICATES OF PARTICULAR THMS ON THTRACE

(DEFUN THSEL (THF) (PROG (THX) (RETURN (AND (SETQ THX (ASSQ THL THTRACE))
                    (SETQ THX (THF THX))
                    (THVAL THX THALIST)))))

;MAKES A NAME WITH PREFIX X AND SUFFIX A UNIQUE NUMBER
(DEFUN THGENS FEXPR (X) (MAKNAM (NCONC (EXPLODE (CAR X))
                  (EXPLODE (SETQ THGENS (ADD1 THGENS))))))

(SETQ THGENS 0)

(DEFUN THPRINTC (X) (TERPRI) (PRINC X) (PRINC '/ ))

§ anno/winograd/syscom

(DECLARE (GENPREFIX SYSCOM))

;;;*********************************************************************
;;;
;;;                 SYSCOM    - TOPLEVEL AND GENERAL UTILITY FUNCTIONS
;;;
;;;**********************************************************************

(DEFUN SHRDLU NIL
       (PROG (ERT-TIME END AMB TIMAMB BOTH BACKREF BACKREF2 ANSNAME
          LASTREL WHO PT PTW SENT PUNCT IGNORE H N NB FE SM RE
          MES MESP C CUT CURTIME STATE GLOBAL-MESSAGE LEVEL
          P-TIME SMN-TIME PLNR-TIME ANS-TIME ANS-PLNR-TIME
          SH-GCTIME)
         (CLEANOUT TSS EVX NODE ANS OSS RSS X)               ;FLUSH OLD GENSYMS
    CATCH-LOOP
         (CATCH
          (PROG NIL
           LOOP (SETQ SENTNO (ADD1 SENTNO)
              PARSINGS 0.
              LEVEL 0.
              LASTSENTNO (ADD1 LASTSENTNO)
              LASTSENT C
              GLOBAL-MESSAGE NIL
              MES 'NOPE
              BACKREF NIL                        ;???????????????????
              RUNTIME (RUNTIME)
              SH-GCTIME (STATUS GCTIME)
              PLNR-TIME 0.
              ANS-PLNR-TIME 0.
              SMN-TIME 0.
              ERT-TIME 0.)
           UP   (SETQ N (SETQ SENT (ETAOIN)))
            (OR ANNOYANCE (PRINT *1))
            (AND ^Q (%))
            (IOC S)
            (AND IGNORE (GO UP))
            ;;;
            (COND
             ((AND
               (COND
            (TOPLEVEL-ERRSET?
             (ERRSET
              (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
            (T (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
               C)
              (OR ANNOYANCE (PRINT *2))
              (SETQ FE (FE C))
              (SETQ NB SENT)
              (SETQ H (H C))
              (SETQ INTERPRETATION (SM C))
              (AND SH-BEFOREANSWER-PAUSE
               (ERT BEFORE ANSWERING))
              (COND
               (SMN (AND SH-PARSE-PAUSE
                 (ERT PARSING COMPLETED))
                (GO LOOP))
               ((NOT ANSWER?)
            (AND SH-PARSESMNTC-PAUSE
                 (ERT ANALYSIS COMPLETED)))
               ((COND
             (TOPLEVEL-ERRSET?
              (ERRSET (TIME-ANSWER '(ANSWER C))))
             (T (TIME-ANSWER '(ANSWER C)))))
               ((APPLY 'SAY
                   (OR GLOBAL-MESSAGE
                   '(I DON/'T UNDERSTAND/.))))))
             ((PRINT *3)
              (APPLY 'SAY
                 (OR GLOBAL-MESSAGE
                 '(I DON/'T UNDERSTAND/.)))))
            (SHRDLU-TIMER)
            (AND MOBYTEST-IN-PROGRESS (AFTER-EACH-SENTENCE))
            (AND SH-STANDARD-PRINTOUT (SHSTPO))
            (AND SH-AFTERANSWER-PAUSE (ERT))
            (GO LOOP))
          ABORT-PARSER)
         (GO CATCH-LOOP)))

(DEFUN TIMER (T0 T1) (QUOTIENT (- T1 T0) 1000000.0))

(DEFUN PARSEVAL (A)
       (PROG (P-TTIME P-GC SM-TIME MP-TIME RETURN-NODE)
         (SETQ P-GC (STATUS GCTIME)
           SM-TIME 0.
           MP-TIME 0.
           P-TTIME (RUNTIME))
         (SETQ RETURN-NODE (EVAL (CONS 'PARSE A)))
         (SETQ P-TIME (DIFFERENCE (TIMER P-TTIME (RUNTIME))
                      SM-TIME
                      PLNR-TIME))
         (OR (= P-GC (STATUS GCTIME))
         (SETQ P-TIME
               (DIFFERENCE P-TIME
                   (TIMER P-GC (STATUS GCTIME)))))
         (SETQ SMN-TIME SM-TIME PLNR-TIME MP-TIME)
         (RETURN RETURN-NODE)))

(SETQ PARSEARGS '(CLAUSE MAJOR TOPLEVEL))

;;*page

;;;********************************************
;;;
;;;   test package !!  -experimental version
;;;
;;;********************************************
;;; how to use:
;;;
;;;   from within an  break at "READY", open, via uread, the file that
;;;   contains the sentences to be tested (see sample files on LANG;) and
;;;   open a file to write onto and do a (IOC r) whenever thing are set
;;;   up (remember that all prints will copy to the file after the ioc
;;;   is executed so a sneaky way to comment the output file is to say
;;;   "(say ...)" or some such.)
;;;     Next set the (global) variable "mobytest-in-progress" to non-nil.
;;;   This will evade every break that the system does via ERTEX - that
;;;   should be all of them but at the moment (8/6/74) that can't be
;;;   guarenteed.
;;;     Functions below trap at the obvious places and could be tailored
;;;   to desired stuff.
;;;     At this point, the preliminaries are over; proceed the  break
;;;   and type a "m" and the next READY. - it should take off.
;;;

(DEFUN AFTER-EACH-SENTENCE NIL
       (COND (C (WALLP C) (DP (CAR (SM C)))))
       (TYO 12.))                                ;form feed

(DEFUN END-OF-FILE-CONDITION NIL
       (AND ^R (UFILE SHTRCE >))
       (AND GO-AWAY (VALRET 'U)))

(SETQ GO-AWAY NIL MOBYTEST-IN-PROGRESS NIL)

;;*page

;;;********************************************************************************
;;;                        Fancy timing package
;;;********************************************************************************

(DEFUN SHRDLU-TIMER NIL
       (PROG (BASE)
         (OR SH-PRINT-TIME (RETURN T))
         (SETQ BASE 10.)
         (TERPRI)
         (PRINC 'TOTAL/ TIME/ USED:/ )
         (PRINC (TIMER RUNTIME (RUNTIME)))
         (PRINTC '/ / AMOUNT/ SPENT/ IN/ GARBAGE/ COLLECTION)
         (PRINC (TIMER SH-GCTIME (STATUS GCTIME)))
         (OR (EQ SH-PRINT-TIME 'FANCY) (RETURN T))
         (TERPRI)
         (PRINC 'BREAKDOWN:)
         (PRINTC '/ / / PARSING)
         (PRINC P-TIME)
         (PRINTC '/ / / SEMANTICS)
         (PRINC SMN-TIME)
         (PRINTC '/ / / MICROPLANNER)
         (PRINTC '/ / / / / / FOR/ SEMANTICS)
         (PRINC PLNR-TIME)
         (PRINTC '/ / / / / / FOR/ ANSWERING)
         (PRINC ANS-PLNR-TIME)
         (PRINTC '/ / / ANSWERING)
         (PRINC ANS-TIME)
         (TERPRI)))

(DEFUN TIME-ANSWER (REAL-CALL)
       (PROG (MP-TIME SM-TIME PLNR-TIME ANS-TTIME GC RESULT)
         (SETQ MP-TIME 0.
           SM-TIME 0.
           GC (STATUS GCTIME)
           ANS-TTIME (RUNTIME)
           PLNR-TIME 0.)
         (SETQ RESULT (EVAL REAL-CALL))
         (SETQ ANS-TIME
           (DIFFERENCE (TIMER ANS-TTIME (RUNTIME)) PLNR-TIME))
         (OR (= GC (STATUS GCTIME))
         (SETQ ANS-TIME
               (DIFFERENCE ANS-TIME
                   (TIMER GC (STATUS GCTIME)))))
         (SETQ ANS-PLNR-TIME MPLNR-TIME
           SMN-TIME (PLUS SMN-TIME SM-TIME))
         (RETURN RESULT)))

(DEFUN PARSE-STATISTICS NIL
       (COND ((= PARSINGS 0.)                           ;initialization
          (PUTPROP 'PARSINGS 0. 'WINS)))
       (AND RE
        (PUTPROP 'PARSINGS
             (1+ (GET 'PARSINGS 'WINS))
             'WINS))
       (SETQ PARSINGS (1+ PARSINGS)))

;;; these next two are left over from previous incarnations
;;;(DEFUN TIMER NIL
;;;       (AND SH-PRINT-TIME
;;;        (PRINT 'TIME-USED)
;;;        (PRINC (DIFFERENCE (TIME-SINCE RUNTIME) ERT-TIME))))

(DEFUN TIME-SINCE (X) (QUOTIENT (- (RUNTIME) X) 1000000.0))

;;*page

;;;****************************************************************
;;;        Functions that extract input from the user
;;;****************************************************************

(DEFUN INTEROGATE FEXPR (MESSAGE)
       (PROG (CH)
    MES  (MAPC (FUNCTION PRINT3) MESSAGE)
         (TERPRI)
         (COND ((MEMQ (SETQ CH (READCH)) '(Y /y))
            (RETURN T))
           ;;;  ((EQ CH '?)
           ;;;   (EVAL (GET 'FLUSH 'EXPLANATION))
           ;;;   (GO MES))
           (T (RETURN NIL)))))

(DEFPROP DEFLIST
     (LAMBDA (LIST) (MAPC (FUNCTION (LAMBDA (A)
                        (PUTPROP (CAR A)
                             (CADR A)
                             (CAR LIST))))
                  (CDR LIST))
            (CAR LIST))
     FEXPR)

;;*PAGE

;;;****************************************************************
;;;           specialized and not so, output routines
;;;****************************************************************

(DEFUN % NIL                                    ;THIS FUNCTION PRINTS THE CURRENT SENTENCE
       (TERPRI)
       (MAPC 'PRINT3 SENT)
       (PRINC PUNCT))

(DEFUN DA (X)
       (AND
    (GET X 'THASSERTION)
    (DISP
     (APPLY 'APPEND
        (MAPCAR 'CDDR
            (APPLY 'APPEND
                   (MAPCAR 'CDR
                       (CDR (GET X
                         'THASSERTION)))))))))

(DEFPROP DISP
     (LAMBDA (0A)
         (AND (STATUS TTY) (TYO 12.))
         (TERPRI)
         (AND (CDR 0A)
              (PRINC (CAR 0A))
              (PRINC '/ >>/ )
              (PRINC (CADR 0A))
              (TERPRI))
         (SPRINT (COND ((CDR 0A) (GET (CAR 0A) (CADR 0A)))
                   ((EVAL (CAR 0A))))
             LINEL
             0.)
         *4)
     FEXPR)

(DEFUN DTABLE (L)
       (PRINT =LINE)
       (MAPC '(LAMBDA (X)
              (PRINTC (TAB 5.) X (TAB 22.) '= (EVAL X))
              (COND ((GET X 'TURNED)
                 (TAB 30.)
                 (PRINC (LIST (GET X 'TURNED))))))
         L)
       (PRINTC =LINE))

(DEFUN DP (X)
       (PROG (PLIST)
         (TERPRI)
         (TERPRI)
         (PRINC '[)
         (PRINC X)
         (PRINC '])
         (SETQ PLIST (plist X))
    A    (COND ((MEMQ (CAR PLIST) '(PNAME VALUE)) (GO B)))
         (TERPRI)
         (TAB 4.)
         (PRINC (CAR PLIST))
         (SPRINT (CADR PLIST) (*DIF LINEL 18.) 18.)
    B    (COND ((SETQ PLIST (CDDR PLIST)) (GO A)))
         (TERPRI)
         (AND DPSTOP (ERT))
         (RETURN '*)))

(DEFUN FEXPR DSAY (L) (APPLY 'SAY L))

;;*page

;;;****************************************************************
;;;        functions for hand-tailored garbage collection
;;;****************************************************************

(DEFUN FORGET NIL
       (SETQ LASTSENT NIL
         LASTREL NIL
         BACKREF NIL
         BACKREF2 NIL
         LASTTIME NIL
         LASTPLACE NIL)
       (SETQ LASTSENTNO 0.)
       (MAPC '(LAMBDA (PN) (MAPC '(LAMBDA (PROP) (REMPROP PN PROP))
                 '(BIND LASTBIND)))
         '(IT THEY ONE))
       (AND EVENTLIST (PROGN (THFLUSH HISTORY) (STARTHISTORY))))

;;; THIS FUNCTION HAS ALSO INCLUDED A CALL TO "PLNRCLEAN"
;;; TO SCRUB AWAY THE EVENTLIST - BUT THE DETAILS OF ITS
;;; MICROPLANNER MANIPULATIONS ARE STILL BEING CHECKED FOR
;;; VERACTITY IN THE PRESENT DAY ENVIRONMENT (6/24/74)
;;; THE CODE WAS:
;;;  (DEFUN PLNRCLEAN (X)
;;;     (MAPC '(LAMBDA (Y)
;;;               (MAPC '(LAMBDA (Z)
;;;                         (THREMOVE (CAR Z)) )
;;;                     (CDDR Y)))
;;;           (GET X 'THASSERTION)) )
;;;
;;; AND THE CALL WAS:
;;;    (MAPC 'PLNRCLEAN EVENTLIST)
;;;

(DEFUN CLEANOUT FEXPR (LIST)                            ;REMOB'S ALL GENSYMS OF THE MEMBERS OF LIST
       (MAPC (FUNCTION (LAMBDA (A)
                   (CLEANX A 0. (GET A 'MAKESYM))
                   (PUTPROP A 0. 'MAKESYM)))
         LIST))

(DEFUN CLEANUP FEXPR (SYMBOL-LIST)
       ;;CLEANUP IS USED TO GET RID OF GENSYMS NO LONGER NEEDED ALL
       ;;GENSYMS FROM THE NUMBER "OLD" TO THE NUMBER "NEW" ARE
       ;;REMOB'ED THE "OLD" AND "NEW" PROPERTIES ARE UPDATED
       (MAPC '(LAMBDA (SYMBOL)
              (CLEANX SYMBOL
                  (GET SYMBOL 'OLD)
                  (PUTPROP SYMBOL
                       (GET SYMBOL 'NEW)
                       'OLD))
              (PUTPROP SYMBOL
                   (GET SYMBOL 'MAKESYM)
                   'NEW))
         SYMBOL-LIST))

(DEFUN CLEANX (A B C)
       ;; CLEANX REMOB'S GENSYMS OF THE SYMBOL "A" FROM B+1 UP TO AND
       ;;INCLUDING C
       (PROG (SAVE I)
         (SETQ B (OR B 0.))
         (SETQ SAVE (GET A 'MAKESYM))
         (AND C
          (GREATERP C B)
          (PUTPROP A B 'MAKESYM)
          (DO I B (ADD1 I) (EQUAL I C) (REMOB (MAKESYM A))))
         (RETURN (PUTPROP A SAVE 'MAKESYM))))

;;*PAGE

;;;****************************************************************
;;;        a most complete and sophisticated break package
;;;****************************************************************

(DEFPROP THERT ERT FEXPR)

(DEFUN ERT FEXPR (MESSAGE) (ERTEX MESSAGE NIL T))                ;ALWAYS STOPS, NEVER CAUSES ABORTION. USED FOR
                                       ;GUARENTEED STOPS AS IN DEBUGGING OR ETAOIN

(DEFUN ERTERR FEXPR (MESSAGE) (ERTEX MESSAGE T NIL))                ;USED FOR KNOWN WIERD STATES SUCH AS CHOP. USES
                                       ;"NOSTOP" SWITCH, CAUSES ABORTION

(DEFUN BUG FEXPR (MESSAGE)
       (ERTEX (CONS 'BUG!!!!!!!!!! MESSAGE) T NIL))                ; MARKES UNANTICIPATED WIERD STATES WHICH
                                       ;INDICATE MISTAKES IN THE CODE.

(DEFUN GLOBAL-ERR FEXPR (MESSAGE)
       (ERTEX (SETQ GLOBAL-MESSAGE MESSAGE) T NIL))                ; MARKES KNOWN INADEQUACIES OF THE SYSTEM.
                                       ;SWITCHABLE STOP, CAUSES ABORTION

(DEFUN ERTEX (MESSAGE CAUSE-ABORTION IGNORE-NOSTOP-SWITCH?)
       (PROG (ERT-TIME GLOP EXP ST-BUFFER BUILDING-ST-FORM ^W ^Q
          FIRSTWORD)
         (AND MOBYTEST-IN-PROGRESS (IOC W))
         (AND NOSTOP
          (NOT IGNORE-NOSTOP-SWITCH?)
          (AND CAUSE-ABORTION
               (THROW CAUSE-ABORTION ABORT-PARSER))
          (RETURN T))
         (SETQ ERT-TIME (RUNTIME))
         (TERPRI)
         (MAPC (FUNCTION PRINT3) MESSAGE)
         (AND MOBYTEST-IN-PROGRESS
          (THROW 'MOBYTEST ABORTPARSER))
    PRINT(SETQ FIRSTWORD T ST-BUFFER NIL BUILDING-ST-FORM NIL)     ;"ST" REFERS TO SHOW, TELL.
         (COND (ZOG-USER (PRINT 'LISTENING--->))
           (T (PRINT '>>>)))
    LISTEN
         (COND
          ;;SHELP UP SPURIOUS CHARACTERS
          ((MEMBER (TYIPEEK) '(32. 10.))                   ;SP, LF
           (READCH)
           (GO LISTEN))
          ;;CHECK FOR DELIMITER
          ((EQ (TYIPEEK) 13.)                       ;CARRIAGE RETURN
           (COND (BUILDING-ST-FORM (SETQ EXP               ;DELIMITER CASE
                         (REVERSE ST-BUFFER))
                       (GO EVAL-EXP))
             (T (READCH)                       ;SPURIOUS CHARACTER CASE
            (GO LISTEN)))))
         ;;;
         (OR (ERRSET (SETQ GLOP (READ))) (GO PRINT))
         ;;;
         (COND ((ATOM GLOP)
            (SETQ GLOP (OR (GET GLOP 'ABBREV) GLOP))
            (COND ((MEMQ GLOP '(T P NIL))               ;LEAVE-LOOP CHARS
               (SETQ ERT-TIME
                 (PLUS (TIME-SINCE ERT-TIME)
                       ERT-TIME))               ;ERT-TIME IS BOUND BY SHRDLU
               (RETURN GLOP))
              ((EQ GLOP 'GO)                   ;CAUSE RETURN TO READY-STATE
               (THROW 'GO ABORT-PARSER))
              (BUILDING-ST-FORM (SETQ ST-BUFFER
                          (CONS GLOP
                            ST-BUFFER))
                        (GO LISTEN))
              ((AND FIRSTWORD
                (MEMQ GLOP '(SHOW TELL)))
               (SETQ BUILDING-ST-FORM T
                 ST-BUFFER (CONS GLOP ST-BUFFER)
                 FIRSTWORD NIL)
               (GO LISTEN))
              (ZOGUSER (PRINC GLOP)
                   (SAY ISN/'T A COMMAND)
                   (TERPRI)
                   (GO PRINT))
              (T (SETQ EXP GLOP) (GO EVAL-EXP))))
           (T (COND ((EQ (CAR GLOP) 'RETURN)
                 (RETURN (EVAL (CADR GLOP))))
                (T (SETQ EXP GLOP) (GO EVAL-EXP)))))
         ;;;
    EVAL-EXP
         (COND (ERT-ERRSET? (ERRSET (PRINT (EVAL EXP))))
           (T (PRINT (EVAL EXP))))
         (GO PRINT)))

;;*PAGE

(DEFUN COMBINATION? FEXPR (WORDS)
       ;;THIS FUNCTION CHECKS TO SEE IF THE WORDS PASSED AS ARGS FORM
       ;;A COMBINATION SUCH AS "STACK-UP" OR "ON-TOP-OF" COMBINATIONS
       ;;ARE IN THE DICTIONARY AS A SINGLE ATOM COMPOSED OF THE WORDS
       ;;IN THE COMBINATION SEPARATED BY DASHES ALL COMBINATIONS HAVE
       ;;THE FEATURE "COMBINATION" AND HAVE A ROOT WHICH IS A LIST OF
       ;;THE WORDS IN THE COMBINATION
       (PROG (COMBINE)
         (MAPC
          '(LAMBDA (X)
        (SETQ COMBINE (NCONC COMBINE
                     (CONS '-
                       (EXPLODE (EVAL X))))))
          WORDS)
         (SETQ COMBINE (LIST (INTERN (MAKNAM (CDR COMBINE)))))
         (AND (ISQ COMBINE COMBINATION) (RETURN COMBINE))
         (RETURN NIL)))

(SETQ CONSO '(B C D F G H J K L M N P Q R S T V W X Z))

(DEFPROP FINDB
     (LAMBDA (X Y) (COND ((NULL X) NIL)
                 ((EQ Y (CDR X)) X)
                 (T (FINDB (CDR X) Y))))
     EXPR)

(DEFPROP FROM
     (LAMBDA (A B) (COND ((OR (NOT A) (EQ A B)) NIL)
                 (T (CONS (WORD A) (FROM (CDR A) B)))))
     EXPR)

(DEFUN MAKESYM (A)
       ;; FUNCTION MAKESYM MAKES UP A GENSYM OF ITS ARG
       (PUTPROP A
        (ADD1 (OR (GET A 'MAKESYM) 0.))
        'MAKESYM)
       (SETQ A (MAKNAM (APPEND (OR (GET A 'EXPLO)
                   (PUTPROP A
                        (EXPLODE A)
                        'EXPLO))
                   (EXPLODE (GET A 'MAKESYM)))))
       (COND (MAKEINTERN (INTERN A)) (A)))

(DEFUN LIS2FY (X)
       (COND ((ATOM X) (LIST (LIST X)))
         ((ATOM (CAR X)) (LIST X))
         (X)))

(DEFUN MEET (A MEET)
       ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
       (PROG (SET)
    GO   (COND ((NULL A) (RETURN (REVERSE SET)))
           ((MEMQ (CAR A) MEET)
            (SETQ SET (CONS (CAR A) SET))))
         (SETQ A (CDR A))
         (GO GO)))

(DEFPROP MOD (LAMBDA (A B) (UNION (SETDIF A (CADR B)) (CAR B))) EXPR)

(DEFUN NTH (NUM LIST)
       (COND ((ATOM LIST) (ERT NTH - ILLEGAL LIST))
         ((LESSP NUM 1.) (ERT NTH - ILLEGAL NUMBER)))
       (PROG NIL
    UP   (COND ((EQUAL NUM 1.) (RETURN (CAR LIST)))
           ((SETQ LIST (CDR LIST))
            (SETQ NUM (SUB1 NUM))
            (GO UP))
           (T (ERT NTH - LIST TOO SHORT)))))

(DEFPROP PR1
     (LAMBDA (A)
         (COND ((ATOM (H A)) (LIST (WORD (NB A)) (FE A)))
               ((PR2 (SM A))
            (LIST (FROM (NB A) (N A))
                  (FE A)
                  (SM A)
                  (COND ((ATOM (H A)) '/ )
                    ((MAPLIST (FUNCTION PR1)
                          (REVERSE (H A)))))))))
     EXPR)

(DEFPROP
 PR2
 (LAMBDA (A)
  (OR
   (ATOM A)
   (MAPC
    (FUNCTION (LAMBDA (B)
              (AND (GET B 'SM)
               (OR (MEMQ B ALIST)
                   (SETQ ALIST
                     (CONS (LIST B
                         (GET B 'SM)
                         (GET B
                              'REFER))
                       ALIST))))))
    A)))
 EXPR)

(DEFUN PRINT2 (X)
       (COND ((GREATERP CHRCT (FLATSIZE X)) (PRINC '/ ))
         (T (TERPRI)))
       (PRINC X))

(DEFUN PRINT3 (X)
       (PROG2 (OR (GREATERP CHRCT (FLATSIZE X)) (TERPRI))
          (PRINC X)
          (PRINC '/ )))

(DEFUN PRINTEXT (TEXT)
       (COND (TEXT (TERPRI)
           (EVAL (CONS 'SAY (LISTIFY TEXT))))))

(DEFPROP PRINTC
     (LAMBDA (L) (PROG (TEST)
               (TERPRI)
              =>   (COND ((NULL L) (RETURN NIL)))
               (SETQ TEST (EVAL (CAR L)))
               (COND ((EQ TEST '<TAB>))
                 (T (PRINC TEST) (PRINC '/ )))
               (SETQ L (CDR L))
               (GO =>)))
     FEXPR)

(DEFUN QUOTIFY (X) (LIST 'QUOTE X))

(DEFPROP SAY (LAMBDA (A) (MAPC (FUNCTION PRINT3) A)) FEXPR)

(DEFUN SETDIF (A SETDIF)
       (PROG (SET)
    GO   (COND ((NULL A) (RETURN (REVERSE SET)))
           ((MEMQ (CAR A) SETDIF))
           ((SETQ SET (CONS (CAR A) SET))))
         (SETQ A (CDR A))
         (GO GO)))

(DEFPROP STA
     (LAMBDA (A B) (PROG NIL
            GO   (COND ((NULL B) (RETURN T))
                   ((NULL A))
                   ((EQ (CAR A) (CAR B))
                    (SETQ A (CDR A))
                    (SETQ B (CDR B))
                    (GO GO)))))
     EXPR)

(DEFUN UNION (A B)
       (PROG (SET)
         (SETQ SET (REVERSE A))
    GO   (COND ((NULL B) (RETURN (REVERSE SET)))
           ((MEMQ (CAR B) SET))
           ((SETQ SET (CONS (CAR B) SET))))
         (SETQ B (CDR B))
         (GO GO)))

(DEFPROP WALLP
     (LAMBDA (A) (PROG (ALIST LINEL)
               (SETQ LINEL WPLINEL)
               (AND (STATUS TTY) (TYO 12.))
               (TERPRI)
               (SPRINT (LIST (PR1 A) (REVERSE ALIST))
                   LINEL
                   0.)))
     EXPR)

(SETQ WPLINEL 72.)

(DEFUN DEFS FEXPR (L)
       (PROG (A)
         (AND (NULL (CDR L)) (RETURN L))
         (SETQ A (CAR L))
         (SETQ L (CDR L))
    LOOP (PUTPROP A (CADR L) (CAR L))
         (COND ((SETQ L (CDDR L)) (GO LOOP)))
         (RETURN A)))

(DEFPROP TAB
     (LAMBDA (N) (PROG (P)
               (COND ((GREATERP N LINEL)
                  (RETURN '<TAB>)))
              A       (SETQ P (DIFFERENCE LINEL CHRCT))
               (COND ((NOT (GREATERP N P))
                  (RETURN '<TAB>)))
               (PRINC '/ )
               (GO A)))
     EXPR)

(DEFUN SPACE (N)
       (PROG (NN)
    A    (COND ((GREATERP N 0.)
            (PRINC '/ )
            (SETQ N (SUB1 N))
            (GO A)))))

§ anno/winograd/morpho

(declare (genprefix morpho))

;;;********************************************************************************
;;;
;;;               MORPHO  - code for morphological analysis
;;;
;;;           includes ETAOIN, the input handler for the system
;;;
;;;********************************************************************************

(DEFUN ETAOIN NIL
       (PROG (WORD NEWWORD CHAR ALTN ALREADY-BLGING-NEWWRD WRD LAST
          NEXT Y WORD1 X RD POSS)
    THRU (SETQ SENT (SETQ WORD (SETQ PUNCT (SETQ POSS NIL))))
         (PRINT 'READY)
         (TERPRI)
         (AND MOBYREAD (IOC Q))
    CHAR (COND ((EQUAL (TYIPEEK) 24.) (READCH) (ERT) (GO THRU)); "cntrl-x" break
;left over from CMU
                   ((= (tyipeek) 3.)
(or (and mobyread (end-of-file-condition))
    (bug etaoin: about to read eof)) )
)
         (setq char (cond ((greaterp 123. (setq char (tyi)) 96.) (- char 32.))
                  ((greaterp 91. char 64.) char)
                  (t char))
                  char (ascii char)
                   ;;this little hack maps all lowercase letters into uppercase.
           ;;a more reasonable thing to do would be to hack the chtrans
           ;;property of the current readtable, but this was quicker to
           ;;patch.
                   )
             (cond ((EQ char '/ ) (GO WORD))           ;DELIMITER
           ((MEMQ CHAR ALTMODE)
            (setq char (ascii (uppercase-ify-char (tyi))) )
            (COND ((MEMQ char ALTMODE)
               (ERT)
               (GO THRU))
                                       ;ALTMODE-ALTMODE
              ((EQ CHAR 'C) (TYO 12.) (GO DO))
                                       ;ALTMODE-C
              ((EQ CHAR 'R) (TERPRI) (GO DO))
                                       ;ALTMODE-R
              ((AND (EQ CHAR 'S) SAVESENT)
                                       ;ALTMODE-S CAUSES THE LAST SENTENCE TYPED IN TO
               (SETQ SENT (CAR SAVESENT))
                                       ;RETURNED AS THE SENTENCE TO BE INTERPRETED
               (SETQ PUNCT (CDR SAVESENT))
               (%)
               (RETURN SENT))
              ((EQ CHAR 'N)
               (SETQ NEWWORD (NOT NEWWORD)
                 ALTN (NOT ALTN))
               (GO CHAR))
                                       ;ALTMODE-N COMPLEMENTS THE NEWWORD FLAG, WHICH
              ((EQ CHAR 'Q)
                                       ;DETERMINES WHETHER UNRECOGNIZED WORDS WILL BE
               (IOC Q)
                                       ;CONSIDERED SPELLING ERRORS OR NEW WORDS.
               (SETQ IGNORE NIL)
               (GO THRU))
                                       ;ALTMODE-Q CAUSES READIN FROM DISK FILE.
              ((EQ CHAR 'M)
               (IOC Q)
               (SETQ IGNORE NIL MOBYREAD T)
               (GO thru))
              ((EQ CHAR 'I)
               (SETQ IGNORE T)
               (IOC Q)
               (GO THRU))
                                       ;ALTMODE-I IGNORES SENTENCE READ FROM FILE.
              ((GO THRU))))
           ((EQ CHAR RUBOUT)
            (COND (WORD (PRINC (CAR WORD))
                (SETQ WORD (CDR WORD)))
              (SENT (PRINT (CAR SENT))
                (SETQ SENT (CDR SENT))))
            (GO CHAR))
           ((EQ CHAR CARRET) (GO WORD))
           ((MEMQ CHAR PUNCL)
            (SETQ PUNCT CHAR)
                                       ;DELIMITER
            (AND WORD (GO WORD))
            (GO PUNC)))
         (AND
          (OR (AND (EQ CHAR '")
               (NOT ALREADY-BLGING-NEWRD)
               (SETQ NEWWORD (SETQ ALREADY-BLGING-NEWRD T))
               (GO CHAR))
          (AND (EQ CHAR '")
               ALREaDY-BLGING-NEWRD
               (NOT (SETQ ALREADY-BLGING-NEWRD NIL))
               (GO WORD))
                                       ;WITHIN THIS "AND" ARE ALL THE CHARACTERS THAT
          (NUMBERP CHAR)
                                       ;ARE UNDERSTOOD BY THE SYSTEM
          (AND (EQ CHAR '=) (NULL WORD))
          (MEMQ CHAR VOWEL)
          (MEMQ CHAR CONSO))
          (SETQ WORD (CONS CHAR WORD)))
         (GO CHAR)
    DO   (PRINT 'READY)
         (TERPRI)
         (MAPC (FUNCTION (LAMBDA (X) (PRINT2 X))) (REVERSE SENT))
         (PRINC '/ )
         (MAPC (FUNCTION PRINC) (REVERSE WORD))
         (GO CHAR)
    WORD (COND ((NULL WORD) (GO CHAR))
           ((EQUAL WORD '(P L E H)) (HELP) (GO THRU))
           ((AND (SETQ WRD (ERRSET (READLIST (REVERSE WORD))))
             (NUMBERP (SETQ WRD (CAR WRD))))
            (SETQ SENT (CONS WRD SENT))
            (BUILDWORD WRD
                   (OR (AND (ZEROP (SUB1 WRD))
                    '(NUM NS))
                   '(NUM))
                   (LIST 'NUM WRD)
                   NIL))
                                       ;NO ROOT FOR NUMBERS
           ((NULL WRD) (SETQ WRD (REVERSE WORD)) (GO NO))
           ((GET WRD 'FEATURES))
                                       ;IF A WORD HAS FEATURES, IT'S PROPERTIES
           ((SETQ X (GET WRD 'IRREGULAR))
                                       ;ARE ALL SET UP IN THE DICTIONARY
            (BUILDWORD WRD
                   (MOD (GET (CAR X) 'FEATURES)
                    (CDR X))
                   (SM X)
                   (CAR X)))
           ((EQ (CAR (LAST WORD)) '=)
            (BUILDWORD WRD
                   (COND ((MEMQ '" WORD)
                      '(PROPN NS POSS))
                     ('(PROPN NS)))
                   '((PROPN T))
                   NIL))
           ((GO CUT)))
         (GO WRD)

         ;;;---------------------------------------------
         ;;;              MORPHOLOGY CODE
         ;;;--------------------------------------------
    CUT  (COND ((STA WORD '(T " N))
            (SETQ RD (CDDDR WORD))
            (SETQ WORD (CONS '* WORD))
            (GO TRY))
           ((STA WORD '(S "))
            (SETQ WORD (CDDR WORD))
            (SETQ POSS WRD)
            (GO WORD))
           ((STA WORD '("))
            (SETQ WORD (CDR WORD))
            (SETQ POSS WRD)
            (GO WORD))
           ((STA WORD '(Y L))
            (SETQ RD (CDDR WORD))
            (GO LY))
           ((STA WORD '(G N I)) (SETQ RD (CDDDR WORD)))
           ((STA WORD '(D E)) (SETQ RD (CDDR WORD)))
           ((STA WORD '(N E)) (SETQ RD (CDDR WORD)))
           ((STA WORD '(R E)) (SETQ RD (CDDR WORD)))
           ((STA WORD '(T S E)) (SETQ RD (CDDDR WORD)))
           ((STA WORD '(S))
            (SETQ RD (CDR WORD))
            (GO SIB))
           (T (GO NO)))
         (SETQ LAST (CAR RD))
         (SETQ NEXT (CADR RD))
         (COND ((AND (MEMQ LAST CONSO)
             (NOT (MEMQ LAST LIQUID))
             (EQ LAST NEXT))
            (SETQ RD (CDR RD)))
           ((EQ LAST 'I)
            (SETQ RD (CONS 'Y (CDR RD))))
           ((OR (AND (MEMQ LAST CONSO)
                 (MEMQ NEXT VOWEL)
                 (NOT (EQ NEXT 'E))
                 (MEMQ (CADDR RD) CONSO))
            (AND (MEMQ LAST LIQUID)
                 (MEMQ NEXT CONSO)
                 (NOT (MEMQ NEXT LIQUID)))
            (AND (EQ LAST 'H) (EQ NEXT 'T))
            (AND (MEMQ LAST '(C G S J V Z))
                 (OR (MEMQ NEXT LIQUID)
                 (AND (MEMQ NEXT VOWEL)
                      (MEMQ (CADDR RD) VOWEL)))))
            (SETQ RD (CONS 'E RD))))
         (GO TRY)
    LY   (COND ((AND (MEMQ (CAR RD) VOWEL)
             (NOT (EQ (CAR RD) 'E))
             (MEMQ (CADR RD) CONSO))
            (SETQ RD (CONS 'E RD))))
         (COND ((MEMQ 'ADJ
              (GET (SETQ ROOT (READLIST (REVERSE RD)))
                   'FEATURES))
            (BUILDWORD WRD
                   '(ADV VBAD)
                   NIL
                                       ;TEMP NIL SEMANTICS
                   ROOT)
                                       ;ROOT IS THE ADJECTIVE
            (GO WRD)))
         (GO NO)
    SIB  (SETQ LAST (CAR RD))
         (SETQ NEXT (CADR RD))
         (COND ((NOT (EQ LAST 'E)))
           ((EQ NEXT 'I)
            (SETQ RD (CONS 'Y (CDDR RD))))
           ((EQ NEXT 'X) (SETQ RD (CDR RD)))
           ((AND (EQ NEXT 'H)
             (NOT (EQ (CADDR RD) 'T)))
            (SETQ RD (CDR RD)))
           ((AND (MEMQ NEXT '(S Z))
             (EQ NEXT (CADDR RD)))
            (SETQ RD (CDDR RD))))
    TRY  (COND
          ((OR
        (SETQ FEATURES
              (GET (SETQ ROOT (READLIST (REVERSE RD)))
               'FEATURES))
        (AND (SETQ X (GET ROOT 'IRREGULAR))
             (SETQ FEATURES
               (MOD (GET (SETQ ROOT (CAR X))
                     'FEATURES)
                (CDR X)))))
           (BUILDWORD WRD
              (MOD FEATURES (GET (CAR WORD) 'MOD))
              (GET ROOT 'SEMANTICS)
              ROOT))
          ((EQ (CAR RD) 'E) (SETQ RD (CDR RD)) (GO TRY))
          ((GO NO)))

         ;;;----------------------------------------------------
         ;;;  BUILD UP THE PROCESSED LIST OF WORDS TO BE RETURNED
         ;;;----------------------------------------------------
    WRD  (SETQ
          SENT
          (COND (POSS (COND ((OR (MEMQ 'NOUN
                       (SETQ FEATURES
                         (GET WRD
                              'FEATURES)))
                                       ;IF IT'S A NOUN
                     (MEMQ 'PROPN FEATURES))
                                       ;OR A PROPER NOUN
                 (BUILDWORD POSS
                        (APPEND (MEET FEATURES
                                       ;MARK IT AS POSSESSIVE
                              (GET 'POSS
                                   'ELIM))
                            '(POSS))
                        (GET WRD
                         'SEMANTICS)
                        ROOT)
                 (CONS POSS SENT))
                ((BUILDWORD '"S
                                       ; CAN WE GENERALIZE IT???
                        '(VB BE V3PS PRES)
                        (GET 'BE
                         'SEMANTICS)
                        'BE)
                 (CONS '"S (CONS WRD SENT)))))
            ((CONS WRD SENT))))
    PUNC (COND
          (PUNCT (COND ((AND (EQ PUNCT '?) (NULL SENT))
                (HELP)
                (GO THRU))
               ((MEMQ PUNCT FINAL)
                (RETURN (CAR (SETQ SAVESENT
                           (CONS (REVERSE SENT)
                                       ;RETURN POINT !!!!!!!!!!!!!
                             PUNCT)))))
               ((SETQ SENT (CONS PUNCT SENT))))))
         (SETQ PUNCT NIL)
         (SETQ WORD (SETQ POSS NIL))
         (GO CHAR)
    NO   (COND (NEWWORD (BUILDWORD WRD
                       '(NOUN NS)
                       '((NOUN (SMNEWNOUN))
                     (PROPN (SMNEWPROPN)))
                       WRD)
                (OR ALTN (SETQ NEWWORD NIL))
                (GO PUNC)))
         (TERPRI)
         (SAY *SORRY I DON/'T KNOW THE WORD ")
         (PRINC WRD)
         (PRINC '/ "/.)
         (TERPRI)
         (SAY PLEASE TYPE <LF> AND CONTINUE THE SENTENCE/.)
    NOGO (OR (EQUAL (TYI) 10.) (GO NOGO))
         (SETQ PUNCT NIL WORD NIL)
         (GO DO)))

(DEFUN PROPNAME (X) (EQ (CAR (EXPLODE X)) '=))

(DEFUN BCWL FEXPR (A)
                                       ;DEFINES COMBINATIONS OF WORDS
       (MAPC
    '(LAMBDA (X)
      (MAPC
       '(LAMBDA (Y)
         (BUILDWORD
          (INTERN (MAKNAM (NCONC (EXPLODE (CAR X))
                     (CONS '-
                       (EXPLODE (CAR Y))))))
          (CONS 'COMBINATION (CADR Y))
          (CADDR Y)
          (LIST (CAR X) (CAR Y))))
       (CDR X)))
    A)
       T)

(DEFUN BUILDWORD (WORD FEATURES SEMANTICS ROOT)
       (PUTPROP WORD FEATURES 'FEATURES)
       (PUTPROP WORD (OR SMN SEMANTICS) 'SEMANTICS)
       (AND ROOT (PUTPROP WORD ROOT 'ROOT))
       WORD)

(DEFUN BUILDWORDLIST FEXPR (A)
                                       ;DEFINES WORDS
       (MAPC '(LAMBDA (X)
                                       ;ROOT IS OPTIONAL
              (PRINT (BUILDWORD (CAR X)
                    (CADR X)
                    (CADDR X)
                    (AND (CDDDR X) (CADDDR X)))))
         A))

(SETQ CARRET '/
)

(DEFUN ETNEW NIL
       (AND (EQ (CAR WORD) '")
        (EQ (CAR (LAST WORD)) '")
        (SETQ WRD (READLIST (CDR (REVERSE (CDR WORD)))))
        (BUILDWORD WRD
               '(NOUN NS)
               '((NOUN (NEWWORD)))
               NIL)))

(SETQ FINAL '(/. ? !))

(SETQ CONSO '(B C D F G H J K L M N P Q R S T V W X Z))

0.

(SETQ LIQUID '(L R S Z V))

(SETQ PUNCL '(/. ? : /; " !))

(SETQ RUBOUT (ASCII 127.))

(DEFPROP UNDEFINED
     (LAMBDA NIL (PROG2 (PRINC (WORD N)) (ERT UNDEFINED)))
     EXPR)

(DEFUN UPPERCASE-IFY-CHAR (CHAR) (COND ((GREATERP 123. CHAR 96.) (- CHAR 32.)) (T CHAR)))

(SETQ VOWEL '(NIL A E I O U Y))

(SETQ SPACE '/ )

§ anno/winograd/show

(declare (genprefix show))

;;;  quickies

(defun shstpo nil ;"sh-standard-printout"
(parsings))

(defun parsings nil
(printc '/ / ratio/ of/ winning/ parses/ to/ total/ )
(princ (get 'parsings 'wins))
(princ '//)
(princ parsings))

(defun parsetrace labels
(cond ((= (arg nil) 0)
       (setq parsetrace 'all))
      (t (setq parsetrace (listify labels))) ))

(defun parsebreak labels
(cond ((= (arg nil) 0)
       (setq parsebreak 'all))
      (t (setq parsebreak (listify labels))) ))

(defun fancytimer off?
(cond ((= (arg nil) 1)
       (setq sh-print-time nil))
      (t (setq sh-print-time 'fancy)) ))

(defun totaltime off?
(cond ((= (arg nil) 1)
       (setq sh-print-time nil))
      (t (setq sh-print-time t)) ))

(defun smntrace off?
(cond ((= (arg nil) 1)
       (setq smntrace nil))
      (t (setq smntrace t)) ))

(defun smnbreak off?
(cond ((= (arg nil) 1)
       (setq smnbreak nil))
      (t (setq smnbreak t)) ))

(DEFUN LBK FEXPR (LABELS) (SETQ LABELBREAK LABELS))
(DEFUN LABELTRACE FEXPR (A)
       (MAPC
    '(LAMBDA (X)
      (PROG (BODY)
        (PRINT X)
        (COND ((GET X 'LABELTRACED)
               (PRINC 'ALLREADY-)
               (GO TRACED))
              ((GET X 'INTERPRET)
               (SETQ BODY (CDR (GET X 'INTERPRET))))
              ((GET X 'EXPR)
               (SETQ BODY (CDDR (CADDR (GET X 'EXPR)))))
              (T (PRINC 'CAN/'T/ BE-) (GO TRACED)))
        (MAP '(LAMBDA (Y)
                  (AND (ATOM (CAR Y))
                   (RPLACD Y
                       (CONS (LIST 'PASSING
                               (LIST 'QUOTE
                                 (CAR Y)))
                         (CDR Y)))))
             BODY)
        (PUTPROP X T 'LABELTRACED)
       TRACED
        (PRINC 'LABELTRACED)))
    A))

(DEFUN PASSING (A)
       (SETQ LASTLABEL A)
       (AND (COND ((ATOM LABELTRACE)
           (AND LABELTRACE (PRINT 'PASSING) (PRINC A)))
          ((MEMQ A LABELTRACE)
           (PRINT 'PASSING)
           (PRINC A)))
        (COND ((ATOM LABELBREAK)
           (AND LABELBREAK (ERT LABELBREAK)))
          ((MEMQ A LABELBREAK) (ERT LABELBREAK)))))

(SETQ LABELTRACE NIL)

(SETQ LABELBREAK NIL)

(DEFUN UNLABELTRACE FEXPR (A)
       (MAPC
    '(LAMBDA (X)
         (PROG (BODY)
               (PRINT X)
               (COND ((NOT (GET X 'LABELTRACED))
                  (PRINC 'ISN/'T/ ALLREADY-)
                  (GO TRACED))
                 ((GET X 'INTERPRET)
                  (SETQ BODY (CDR (GET X
                           'INTERPRET))))
                 ((GET X 'EXPR)
                  (SETQ BODY (CDDR (CADDR (GET X
                               'EXPR)))))
                 (T (PRINC 'CAN/'T/ BE-)
                (GO TRACED)))
               (MAP '(LAMBDA (Y) (AND (ATOM (CAR Y))
                          (RPLACD Y (CDDR Y))))
                BODY)
               (PUTPROP X NIL 'LABELTRACED)
               (PRINC 'UN)
          TRACED
               (PRINC 'LABELTRACED)))
    A))

(DEFS TELLABLE
      TELL
      '(LAMBDA (X) (APPLY 'TELLABLE
             (LIST (CHARG X
                      'CONCEPT:
                      '(ANY PLANNER
                        GOAL
                        PATTERN
                        BEGGININGWHITH
                        THIS
                        CONCEPT
                        NAME
                        CAN
                        BE
                        ACCEPTED
                        BY
                        THE
                        SYSTEM
                        ASNEW
                        INFORMATION
                        --
                        BEWARE
                        OF
                        INTERACTIONS
                        WITH
                        SPECIALHACKS
                        FOR
                        LOCATION/,
                        ETC/.))))))

(DEFUN PEV (EV COL TOP)
       (TERPRI)
       (TAB COL)
       (PRINC EV)
       (PRINC '/ / )
       (PRINC (GET EV 'TYPE))
       (PRINC '/ / TIME:/ )
       (PRINC (GET EV 'START))
       (PRINC '/ TO/ )
       (PRINC (GET EV 'END))
       (AND TOP
        (PRINC '/ REASON:/ )
        (PRINC (GET EV 'WHY)))
       (MAPC '(LAMBDA (X) (AND (EQ EV (GET X 'WHY))
                   (PEV X (PLUS COL 8.) NIL)))
         (REVERSE EVENTLIST)))

(DEFS EVENT
      SHOW
      (LAMBDA (X)
          (SETQ X (CHARG X
                 'EVENT:
                 '(EVENT TO
                     BE
                     DISPLAYED
                     --<LF>
                     FOR
                     ENTIRE
                     EVENT
                     LIST)))
          (COND (X (PEV X 0. T))
            (T (MAPC '(LAMBDA (Y)
                      (AND (EQ 'COMMAND
                           (GET Y 'WHY))
                       (PEV Y 0. T)))
                 (REVERSE EVENTLIST))))))

(DEFUN ABBREVIATE FEXPR (A)
       (MAPCAR '(LAMBDA (X)
            (PUTPROP (READLIST (MAPCAR '(LAMBDA (X Y) X)
                           (EXPLODE X)
                           '(T T)))
                 X
                 'ABBREV))
           A)
       'DONE)

(ABBREVIATE SHOW
        TELL
        LISP
        PLANNER
        PARSING
        DEFINITIONS
        SCENE
        INPUT
        RUN
        SEMANTICS
        PROPERTY
        FUNCTION
        VALUE
        ASSERTIONS
        THEOREM
        SCENE
        ACTION
        NODE
        TREE
        LABEL
        ATTEMPT
        UNIT
        WORD
        MARKER
        ALL
        REST
        CURRENT
        STOP
        DO)

(DEFUN SHOWSCENE (X)
       (PROG (PLANNERSEE)
         (TERPRI)
         (TAB 16.)
         (PRINC 'CURRENT/ SCENE)
         (TERPRI)
         (TERPRI)
         (MAPC
          '(LAMBDA (OBJ)
        (PRINT OBJ)
        (PRINC '-->/ / )
        (EVLIS (CAR (NAMEOBJ OBJ 'DESCRIBE)))
        (PRINC '/ AT/ )
        (PRINC (CADR (ASSOC OBJ ATABLE)))
        (AND (SETQ OBJ
               (THVAL '(THFIND ALL
                       $?X
                       (X)
                       (THGOAL (#SUPPORT $?OBJ
                                 $?X)))
                  (LIST (LIST 'OBJ OBJ))))
             (TAB 13.)
             (PRINC 'SUPPORTS/ )
             (PRINC OBJ)))
          '(:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B10 :BOX))
         (TERPRI)
         (SAY THE HAND IS GRASPING)
         (PRINC '/ )
         (PRINC (COND ((SETQ OBJ
                 (THVAL '(THGOAL (#GRASPING $_X))
                    '((X THUNBOUND))))
               (CADAR OBJ))
              (T 'NOTHING)))))

(DEFUN TELLCHOICE (NODE) (SETQ NODE (CAR NODE)) (SHOWTELLCHOICE))

(DEFUN SHOWCHOICE (NODE) (SETQ NODE (CAR NODE)) (SHOWTELLCHOICE))

(DEFUN SHOWTELL (A NODE SYSTEMS INFO ACTION)
       (COND ((NULL A) (SHOWTELLCHOICE))
         ((GET (CAR A) ACTION)
          (APPLY (GET (CAR A) ACTION) (LIST A)))
         ((PRINTEXT '(I DON/'T KNOW HOW TO))
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
         (SETQ EXPL (MAPCAR 'EXPLODE
                (CONS 'QUIT CHOICES)))
    TOP  (SETQ CH2 (CONS 'QUIT CHOICES) EX2 EXPL)
         (PRINTEXT TEXT)
    READ (COND ((MEMBER (SETQ CHAR (READCH)) BREAKCHARS)
            (COND ((NOT NOTINIT) (GO READ))
              ((CDR CH2) (TYO 7.) (GO READ))
              (T (MAPC 'PRINC (CAR EX2))
                 (AND (EQ (CAR CH2) 'QUIT)
                  (ERR NIL))
                 (RETURN (CAR CH2)))))
           ((EQ CHAR (ASCII 10.)) (GO READ))
           ((EQ CHAR '?) (PRINTEXT HELP) (GO CHOICES)))
         (SETQ CH3 NIL EX3 NIL)
         (MAPC '(LAMBDA (X Y) (AND (EQ CHAR (CAR X))
                       (SETQ CH3 (CONS Y CH3))
                       (SETQ EX3 (CONS (CDR X) EX3))))
           EX2
           CH2)
         (AND CH3
          (SETQ EX2 EX3 CH2 CH3)
          (SETQ NOTINIT T)
          (GO READ))
    GO   (OR (MEMBER (READCH) BREAKCHARS) (GO GO))
    CHOICES
         (PRINTEXT '(THE CHOICES ARE:))
         (PRINT CHOICES)
         (GO TOP)))

(DEFUN REQUEST (TEXT HELP)
       (PROG (X)
    TOP  (PRINTEXT TEXT)
    READ (COND ((MEMBER (ASCII (TYIPEEK)) BREAKCHARS)
            (READCH)
            (GO READ))
           ((EQUAL (TYIPEEK) 10.) (READCH) (RETURN NIL))
           ((EQ (ASCII (TYIPEEK)) '?)
            (READCH)
            (PRINTEXT (OR HELP
                  '(NO INFORMATION AVAILABLE)))
            (GO TOP))
           ((EQ (SETQ X (READ)) 'QUIT) (ERR NIL))
           (T (RETURN X)))))

(DEFUN SHOWPROP (X)
       (COND ((NULL X)
          (SHOWPROP (CONS (REQUEST 'ATOM:
                       '(THE NAME
                         OF
                         THE
                         ATOM
                         WHOSE
                         PROPERTY
                         (IES)
                         YOU
                         WANT
                         TO
                         EXAMINE))
                  (LISTIFY (REQUEST 'PROPERTY:
                        '(THE PROPERTY
                              (IES)
                              YOU
                              WANT
                              TO
                              SEE/.
                              A
                              LINE
                              FEED
                              MEANS
                              ALL
                              PROPERTIES
                              OF
                              THE
                              ATOM))))))
         ((CDR X) (APPLY 'DISP X))
         (T (PROG (DPSTOP) (DP (CAR X))))))

(DEFUN TELL FEXPR (A)
       (SHOWTELL A
         'CANTELL
         'TELLTREE
         'TELLINFO
         'TELL))

(DEFUN TREEPRINT (ROOT TR COL)
       (TERPRI)
       (TAB COL)
       (PRINC ROOT)
       (MAPC '(LAMBDA (X) (TREEPRINT X TR (PLUS COL 8.)))
         (GET ROOT TR))
       '*)

(DEFUN CHARG (X TEXT HELP)
       (COND ((CDR X) (CADR X)) (T (REQUEST TEXT HELP))))

(DEFUN SHOW FEXPR (A)
       (SHOWTELL A
         'CANSHOW
         'SHOWTREE
         'SHOWINFO
         'SHOW))

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
      (LAMBDA (X) (TREEPRINT 'CANSHOW 'SHOWTREE 0.)))

(DEFS TELL
      SHOW
      (LAMBDA (X) (TREEPRINT 'CANTELL 'TELLTREE 0.)))

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
      '(LAMBDA (X) (PRINTEXT '(NOT YET DEFINED))))

(DEFS STOP
      TELL
      (LAMBDA (X) (SETQ DPSTOP (ONOFF X
                      '(STOP AFTER
                         DISPLAYING
                         EACH
                         NODEAND
                         SEMANTIC
                         STRUCTURE?)))
          (SETQ PLANNERSEE
            (AND PLANNERSEE
                 (COND ((ONOFF X
                       '(STOP AFTER
                          SHOWING
                          PLANNER
                          INPUT?))
                    T)
                   ('NOSTOP))))))

(DEFS PLANNER
      SHOWTREE
      (ASSERTIONS THEOREM SCENE EVENT)
      SHOW
      SHOWCHOICE
      TELLTREE
      (INPUT ACTION THEOREM ASSERTIONS TELLABLE )
      TELL
      (LAMBDA (X)
          (COND ((NULL (CDR X)) (TELLCHOICE X))
            ((EQ (CADR X) 'ON)
             (IOC W)
             (THTRACE THEOREM THASSERT THERASE (THGOAL T T))
             (SETQ PLANNERSEE T))
            ((EQ (CADR X) 'OFF)
             (IOC W)
             (SETQ PLANNERSEE NIL)
             (THUNTRACE))
            (T (TELLCHOICE X)))
          (IOC V)))

(DEFS PARSING
      SHOWTREE
      (NODE TREE)
      SHOW
      SHOWCHOICE
      TELLTREE
      (NODE LABEL ATTEMPT)
      TELL
      (LAMBDA (X) (COND ((NULL (CDR X)) (TELLCHOICE X))
            ((EQ (CADR X) 'ON)
             (IOC W)
             (SETQ PARSENODE-SEE T LABELTRACE T)
             (TRACE CALLSM PARSE))
            ((EQ (CADR X) 'OFF)
             (IOC W)
             (SETQ PARSENODE-SEE NIL LABELTRACE NIL)
             (UNTRACE CALLSM PARSE))
            (T (TELLCHOICE X)))
          (IOC V)))

(DEFS DEFINITIONS
      SHOWTREE
      (UNIT WORD MARKER )
      SHOW
      SHOWCHOICE
      TELL
      TELLCHOICE
      TELLTREE
      (WORD  MARKER))

(DEFS INPUT
      TELL
      (LAMBDA (X) (SETQ PLANNERSEE
            (ONOFF X '(TO SEE INPUT TO PLANNER))))
      SHOW
      SHOWCHOICE
      SHOWTREE
      (ALL REST CURRENT))

(DEFS SEMANTICS
      TELL
      (LAMBDA (X) (SETQ SMN NIL BUILD-SEE T SMN-STOP T)
          (COND ((EQ (QUERY '(DO SEMANTIC ANALYSIS?)
                    '(YES NO)
                    NIL)
                 'NO)
             (SETQ SMN T))
            ((EQ (QUERY '(SHOW BUILDING
                       OF
                       SEMANTIC
                       STRUCTURES?)
                    '(YES NO)
                    NIL)
                 'NO)
             (SETQ BUILD-SEE NIL))
            ((EQ (QUERY '(STOP AFTER
                       DISPLAYING
                       SEMANTIC
                       STRUCTURES?)
                    '(YES NO)
                    NIL)
                 'NO)
             (SETQ SMN-STOP NIL)))))

(DEFS RUN
      TELLTREE
      (STOP DO)
      TELL
      TELLCHOICE
      TELLINFO
      '(PARAMETERS TO CONTROL WHAT SHRDLU DOES AS IT RUNS))

(DEFS PROPERTY SHOW (LAMBDA (X) (SHOWPROP (CDR X))))

(DEFS VALUE
      SHOW
      (LAMBDA (X) (DISP (EVAL (CHARG X
                     'EXPRESSION:
                     '(EXPRESSION TO
                          BE
                          EVALUATED
                          BY
                          THE
                          LISP
                          INTERPRETER))))))

(DEFS FUNCTION
      TELL
      (LAMBDA (X) (SETQ X (LIST (CHARG X
                       'FUNCTION:
                       '(LISP FUNCTION
                          WHOSE
                          ACTION
                          IS
                          TO
                          BE
                          TRACED))
                (COND ((AND (CDR X)
                        (CDDR X)
                        (MEMQ (CADDR X)
                          '(TRACE BREAK
                            UNTRACE
                            UNBREAK)))
                       (CADDR X))
                      (T (QUERY '(TRACE BREAK
                            UNTRACE
                            OR
                            UNBREAK?)
                        '(TRACE BREAK
                            UNTRACE
                            UNBREAK)
                        '(TRACE CAUSES
                            PRINTOUT
                            ON
                            ENTRYAND
                            EXIT
                            OF
                            FUNCTION/.
                            BREAK
                            CAUSES
                            LISP
                            TO
                            STOP
                            ON
                            ENTRY
                            ANDEXIT/,
                            ACCEPTING
                            USER
                            COMMANDS
                            AND
                            CONTINUING
                            WHEN
                            <CONTROL
                            X>
                            IS
                            TYPED/.))))))
          (APPLY (SUBST 'WBREAK 'BREAK (CADR X))
             (LIST (CAR X))))
      SHOW
      (LAMBDA (X) (APPLY 'GB
             (LIST (CHARG X
                      'FUNCTION:
                      '(LISP FUNCTION
                         WHOSE
                         LISP
                         DEFINITION
                         IS
                         TO
                         BE
                         SHOWN))))))

(DEFS ASSERTIONS
      TELL
      (LAMBDA (X) (THVAL (LIST 'THASSERT
                   (CHARG X
                      'ASSERTION:
                      '(PLANNER ASSERTION
                        TO
                        BE
                        ADDED
                        TO
                        DATA
                        BASE))
                   '(THTBF THTRUE))
             NIL))
      SHOW
      (LAMBDA (X) (DA (CHARG X
                 'ATOM:
                 '(SHOW ALL
                    ASSERTIONS
                    WHICH
                    CONTAIN
                    THE
                    GIVEN
                    ATOM)))))

(DEFS THEOREM
      TELL
      DEFINETHEOREM
      SHOW
      (LAMBDA (X) (DISP (GET (CHARG X
                    'THEOREM-NAME:
                    '(PLANNER THEOREM
                          WHOSE
                          DEFINITION
                          IS
                          TO
                          BE
                          SHOWN))
                 'THEOREM))))

(DEFS NODE
      TELL
      (LAMBDA (X) (SETQ PARSENODE-SEE T NODE-STOP T)
          (COND ((EQ (QUERY '(SEE SUCCESSFUL
                      PARSE
                      NODES
                      BEING
                      BUILT?)
                    '(YES NO)
                    NIL)
                 'NO)
             (SETQ PARSENODE-SEE NIL))
            ((EQ (QUERY '(STOP AFTER DISPLAY OF NODES?)
                    '(YES NO)
                    NIL)
                 'NO)
             (SETQ NODE-STOP NIL))))
      SHOW
      (LAMBDA (X)
          (COND ((GET (CADR X) 'FEATURES) (DP (CADR X)))
            ((SHOWMOVE (CDR X))
             (PROG (DPSTOP) (DP (CAR PT)))
             (RESTOREPT))
            (T (SAY NO SUCH NODE)))))

(DEFS TREE
      SHOW
      (LAMBDA (X) (COND ((GET (CADR X) 'FEATURES)
             (WALLP (LIST (CADR X))))
            ((SHOWMOVE (CDR X)) (WALLP PT) (RESTOREPT))
            (T (SAY NO SUCH NODE)))))

(DEFS UNIT
      SHOW
      (LAMBDA (X) (APPLY 'DG
             (OR (CDR X)
                 (LIST(REQUEST 'UNIT:
                      '(GRAMMAR UNIT
                        WHOSE
                        PROGRAM
                        IS
                        TO
                        BE
                        EXAMINED
                        --
                        E/.G/.
                        CLAUSE
                        NG
                        PREPG
                        VG
                        ADJG)))))))

(DEFS WORD
      SHOW
      (LAMBDA (X) (DP (CHARG X
                 'WORD:
                 '(ENGLISH WORD IN THE VOCABULARY))))
      TELL
      (LAMBDA (X) (APPLY 'DEFINE
             (LIST (CHARG X
                      'WORD:
                      '(ENGLISH WORD
                        TO
                        BE
                        DEFINED
                        --
                        MUST
                        BE
                        NOUN
                        OR
                        VERB))))))

(DEFS ACTION
      TELL
      (LAMBDA (X)
          (COND ((CDR X)
             (COND ((EQ (CADR X) 'ON) (SETQ X NIL))
               ((EQ X 'OFF)
                (SETQ X '(THUNTRACE)))))
            ((ONOFF X
                '(WATCH PLANNER PROGRAMS STEP BY STEP?))
             (SETQ X NIL))
            (T (SETQ X '(THUNTRACE))))
           (COND (X (THUNTRACE))
                     (T (APPLY 'THTRACE '(THEOREM THGOAL THASSERT THERASE))))))

(DEFS LABEL
      TELL
      (LAMBDA (X) (OR (CDR X)
              (SETQ X (LIST (REQUEST '(TYPE LIST
                            OF
                            LABELS/,
                            OR
                            ON
                            OR
                            OFF:)
                         '(WATCHES PARSER
                               GO
                               PAST
                               PROGRAM
                               LABELS
                               IN
                               THE
                               GRAMMAR)))))
          (SETQ LABELTRACE (COND ((EQ (CAR X) 'OFF)
                      NIL)
                     (T (CAR X))))))

(DEFS ATTEMPT
      TELL
      (LAMBDA (X) (COND ((ONOFF X
                '(TO SEE
                     ALL
                     ATTEMPTS
                     TO
                     PARSE
                     SYNTACTIC
                     UNITS/,
                     INCLUDING
                     FAILURES))
             (TRACE PARSE)
             (TRACE CALLSM))
            (T (UNTRACE PARSE)))))

(DEFUN SHOWMOVE (X)
       (SETQ SAVEPT PT)
       (APPLY 'MOVE-PT
          (LISTIFY (OR X
               (REQUEST 'NODE-SPECIFICATION:
                    '(C MEANS
                    CURRENT
                    NODE
                    --
                    H
                    IS
                    MOST
                    RECENTLY
                    PARSED
                    FOR
                    OTHER
                    POSSIBILITIES/,
                    SEE
                    THESIS
                    SECTION
                    ON
                    POINTER-MOVING
                    COMMANDS))))))

(DEFUN ONOFF (ARG HELP)
       (COND ((EQ (CADR ARG) 'ON) T)
         ((EQ (CADR ARG) 'OFF) NIL)
         ((EQ 'ON
          (QUERY '(ON OR OFF?)
             '(ON OFF)
             HELP)))))

(DEFUN DEFINETHEOREM (X)
       (PUTPROP (COND ((CDR X) (SETQ X (CADR X)))
              (T (SETQ X (MAKESYM 'THEOREM))))
        (NCONC (LIST (QUERY '(WHICH THEOREM TYPE?)
                    '(THANTE THERASING THCONSE)
                    '(ANTECEDENT/, ERASING/,
                           OR
                           CONSEQUENT
                           THEOREM))
                 (LISTIFY (REQUEST 'VARIABLE-LIST:
                           NIL))
                 (REQUEST 'PATTERN:
                      '(A LIST
                      ENCLOSED
                      IN
                      PARENS/,
                      LIKE
                      (#IS $?X #ZOG)))
                 (REQUEST 'BODY:
                      '(LIST OF
                         MICROPLANNER
                         STAEMENTS))))
        'THEOREM)
       (THADD X NIL)
       (PRINT X))

(DEFS MARKER
      TELL
      (LAMBDA (X)
          (PROG (Y)
            (PUTPROP (SETQ X (CHARG X
                        'MARKER:
                        '(MARKER TO BE ADDED)))
                 (LIST (SETQ Y
                     (REQUEST 'PARENT:
                          '(NODE TO
                             WHICH
                             IT
                             ISATTACHED
                             IN
                             THE
                             TREE))))
                 'SYS)
            (PUTPROP Y
                 (CONS X (GET Y 'SYSTEM))
                 'SYSTEM)))
      SHOW
      (LAMBDA (X) (TREEPRINT (OR (CHARG X
                    'MARKER:
                    '(SEMANTIC MARKER
                           WHOSE
                           SUBSETS
                           ARE
                           TO
                           BE
                           EXAMINED/.
                           TYPE
                           <LF>
                           FOR
                           ENTIRE
                           TREE/.))
                 '#SYSTEMS)
                 'SYSTEM
                 0.)))

(DEFS ALL SHOW (LAMBDA (X) (%)))

(DEFS CURRENT SHOW (LAMBDA (X) (PRINTEXT (FROM NB N))))

(DEFS REST SHOW (LAMBDA (X) (PRINTEXT N)))

(DEFS SCENE SHOW SHOWSCENE)

(DEFUN DEFINE FEXPR (A)
       (PROG (FE TYPE MARK REST TR)
         (SETQ A  (COND  (A (CAR A))
                      (T  (REQUEST 'WORD: '( ENGLISH WORD TO
                                            BE DEFINED)))))
         (SETQ TYPE
           (QUERY '(NOUN OR VERB?)
              '(NOUN VERB)
              '(OTHER TYPES MUST BE DEFINED IN LISP)))
    MAR  (OR (SETQ MARK (REQUEST 'MARKERS:
                     '(LIST OF
                        SEMANTIC
                        MARKERS
                        FOR
                        WORD
                        BEING
                        DEFINED
                        -
                        TO
                        SEE
                        MARKER
                        TREE
                        TYPE
                        <LF>)))
         (AND (SHOW MARKER #SYSTEMS) (GO MAR)))
         (SETQ MARK (LISTIFY MARK))
         (COND
          ((EQ TYPE 'NOUN)
           (PUTPROP A '(NOUN NS) 'FEATURES)
           (PUTPROP
        A
        (LIST
         (LIST
          'NOUN
          (LIST
           'OBJECT
           (LIST
            'MARKERS:
            MARK
            'PROCEDURE:
            (LIS2FY (REQUEST 'PROCEDURE:
                     '(EXPRESSION OR
                          LIST
                          OF
                          EXPRESSIONS
                          TO
                          BE
                          PUT
                          IN
                          PLANNER
                          GOALS
                          TO
                          DESCRIBE
                          OBJECT
                          -
                          USE
                          ***
                          TO
                          REPRESENT
                          OBJECT
                          BEING
                          DESCRIBED
                          BY
                          WORD
                          --
                          E/.G/.
                          (#IS *** #ZOG)
                          OR
                          ((#IS *** #ZOG)
                           (#LOVE :EVERYONE
                              ***)))))))))
        'SEMANTICS)
           (RETURN T))
          ((SETQ TR (EQ (QUERY '(TRANSITIVE OR INTRANSITIVE?)
                   '(TRANSITIVE INTRANSITIVE)
                   NIL)
                'TRANSITIVE))
           (PUTPROP A '(VB TRANS INF) 'FEATURES))
          (T (PUTPROP A '(VB ITRNS INF) 'FEATURES)))
         (SETQ
          REST
          (LIST (LIST (LISTIFY (REQUEST '(RESTRICTIONS ON
                               SUBJECT:)
                        '(LIST OF
                           SEMANTIC
                           MARKERS))))))
          (AND
           TR
           (SETQ
        REST
        (NCONC REST
               (LIST (LISTIFY (REQUEST '(RESTRICTIONS ON
                                  OBJECT:)
                           '(LIST OF
                              SEMANTIC
                              MARKERS)))))))
          (PUTPROP
           A
           (LIST
        (LIST
         'VB
         (LIST
          'RELATION
          (LIST 'MARKERS:
            MARK
            'RESTRICTIONS:
            REST
            'PROCEDURE:
            (LIS2FY (REQUEST 'PROCEDURE:
                     '(LIST OF
                        EXPRESSIONS
                        TO
                        BE
                        PUT
                        INTO
                        PLANNER
                        GOALS
                        TO
                        DESCRIBE
                        ACTION
                        OR
                        RELATION
                        --
                        USE
                        #1
                        FOR
                        SUBJECT/,
                        #2
                        FOR
                        OBJECT/.E/.G/.
                        (#SUPPORT #1 #2)
                        OR
                        ((#HAPPY #1)
                         (#SMILING #1)))))))))
           'SEMANTICS)
          (RETURN T))))

(DEFUN HELP NIL
       (COND ((EQ 'S
          (QUERY '(TYPE L
                FOR
                LONG
                FORM
                (85. LINES)
                S
                FOR
                SHORT
                (16. LINES))
             '(S L)
             NIL))
          (UREAD MINIH DOC DSK LANG))
         (T (UREAD HELP DOC DSK LANG)))
       (THRUTEXT)
       '*)

(DEFUN LIS2FY (X)
       (COND ((ATOM X) (LIST (LIST X)))
         ((ATOM (CAR X)) (LIST X))
         (X)))

§ anno/winograd/setup

(declare (genprefix setup))

;;;################################################################
;;;
;;;           SETUP - initialization file for SHRDLU
;;;
;;;################################################################

(setq parsings 0) ;atom used in the timing package

(SETQ ELSE
      T
      SAVESENT
      NIL
      ALTMODE
      (ASCII 27.)
      DOT
      (ASCII 46.)
      *1
      '[1]
      *2
      '[2]
      *3
      '[3]
      *4
      '[4]
      *5
      '[5]
      *6
      '[6]
      LASTSENTNO
      0.
      SENTNO
      1.
      UNMKD
      '(COMPONENT BOTH)
      LASTIME
      NIL)

(SETQ DPSTOP
      NIL
      NODE-STOP
      NIL
      SMN-STOP
      NIL
      ERT-TIME
      0.
      ALTMODE
      (LIST (ASCII 27.))
      BREAKCHARS
      (LIST (ASCII 32.) (ASCII 13.) (ASCII 46.))
      LINEL
      65.
      =LINE
      '========================================================)

(OR (GET 'CLAUSE 'SUBR)
    (LABELTRACE CLAUSE NG VG ADJG PREPG CONJOIN))

;;;**********************************************************************
;;;            SWITCHES AND SWITCH-SETTING PACKAGES
;;;**********************************************************************

(SETQ FEATURESWITCHES '(MOBYREAD DISCOURSE NOMEM IASSUME TIMID))

(SETQ PAUSESWITCHES
      '(ANS-AFTEREVALUATION-PAUSE ANS-AFTERFORMULATION-PAUSE
                  EVALANS-PAUSE
                  SH-SENT-PAUSE
                  SH-BEFOREANSWER-PAUSE
                  SH-FINISHED-PAUSE
                  PNS-BK
                  PLNRSEE-PAUSE))

(SETQ CONTROLSWITCHES '(NOSTOP ANSWER?
                   SMN
                   TOPLEVEL-ERRSET?
                   ERT-ERRSET?
                   MAKEINTERN))

(SETQ DISPLAYSWITCHES '(PARSETRACE PARSEBREAK
                   PARSENODE-SEE
                   LABELTRACE
                   MAKE-VERBOSE
                   LABELBREAK
                   BUILDSEE
                   BUILD-SEE
                   PLANNERSEE
                   SH-PRINT-TIME))

;;;*************************

(SETQ MAKE-VERBOSE
      NIL
      PARSETRACE
      NIL
      PARSEBREAK
      NIL
      PARSENODE-SEE
      NIL
      LABELTRACE
      NIL
      LABELBREAK
      NIL
      BUILDSEE
      NIL
      BUILD-SEE
      NIL
      PLANNERSEE
      NIL
      SH-PRINT-TIME
      NIL)

(SETQ MOBYREAD
      NIL
      DISCOURSE
      T
      WANT-DISPLAY
      NIL
      NOMEM
      NIL
      IASSUME
      T
      TIMID
      200.)

(SETQ MAKEINTERN NIL)

(SETQ SH-BEFOREANSWER-PAUSE
      NIL
      ANS-AFTEREVALUATION-PAUSE
      NIL
      ANS-AFTERFORMULATION-PAUSE
      NIL
      EVALANS-PAUSE
      NIL
      NOSTOP
      NIL
      ANSWER?
      T
      SMN
      NIL
      DOIT
      NIL
      TOPLEVEL-ERRSET?
      NIL
      ERT-ERRSET?
      T
      SH-PARSE-PAUSE
      NIL
      SH-PARSESMNTC-PAUSE
      NIL
      SH-AFTERANSWER-PAUSE
      NIL
      PNS-BK
      NIL
      PLNRSEE-PAUSE
      NIL)

;;;***********************************

(DEFUN QUIETMODE NIL
       (MAPC '(LAMBDA (X) (SET X NIL)) DISPLAYSWITCHES))

(DEFUN NOPAUSES NIL
       (MAPC '(LAMBDA (X) (SET X NIL)) PAUSESWITCHES))

(DEFUN NORMALFEATUREMODE NIL
       (SETQ MOBYREAD NIL DISCOURSE T NOMEM NIL IASUME T TIMID 200.))

(DEFUN USERMODE NIL
       (QUIETMODE)
       (NORMALFEATUREMODE)
       (NOPAUSES)
       (SETQ NOSTOP
         T
         ANSWER?
         T
         SMN
         NIL
         TOPLEVEL-ERRSET?
         T
         ERT-ERRSET
         T)
       (SETQ *RSET NIL)
       (IOC C)
       (SETQ SH-PRINT-TIME T))

(DEFUN DEBUGMODE NIL
       (QUIETMODE)
       (NORMALFEATUREMODE)
       (NOPAUSES)
       (SETQ NOSTOP
         NIL
         ANSWER?
         T
         SMN
         NIL
         TOPLEVEL-ERRSET?
         NIL
         ERT-ERRSET
         T)
       (SETQ *RSET T)
       (IOC D))

(SETQ ZOG-USER NIL ZOGUSER NIL)

;;;*****************************************************************
;;;           INITIALIZATION ROUTINES
;;;*****************************************************************

(DEFUN INITIALSTUFF (version date note)
       (SUSPEND)
       (CURSORPOS 'C)
       (TERPRI)
       (PRINC 'SHRDLU/ VERSION/ )
       (princ version)
       (princ '/ / / )
       (PRINC 'LOADED/ )
       (PRINC date )
       (princ '/ )
       (PRINC 'IN/ BLISP/ )
       (princ (status lispversion))
       (TERPRI)
       (SAY REFER COMMENTS AND QUESTIONS TO DDM)
       (TERPRI)
       (TERPRI)
(and note (progn (terpri)(apply 'say note)
            (terpri)(terpri)))
;;;       (SAY -IF YOU ARE NEAR A DEC-340)
;;;       (TERPRI)
;;;       (PRINC '/ / / / / )
;;;       (OR (AND (INTEROGATE DO YOU WANT THE DISPLAY /(TYPE "Y/ " OR "N/ "/))
;;;        (SETQ WANT-DISPLAY T))
;;;       (SETQ WANT-DISPLAY NIL))
;;;       (COND ((NOT WANT-DISPLAY) (NO340)))
;;;       (COND ((EQ WANT-DISPLAY T)
;;;          (UREAD GRAPHF INIT DSK RBRN)
;;;          (IOC Q)
;;;          (PROG (D V)
;;;            (SETQ D '((NIL)))
;;;           MORE (COND ((NOT (EQ D (SETQ V (READ D))))
;;;               (EVAL V)
;;;               (GO MORE)))))
;;;         (T (SETQ PH-TURN-ON
;;;              NIL
;;;              GP-LINES
;;;              NIL
;;;              GP-SURFACE
;;;              NIL
;;;              GP-HANDIT
;;;              NIL
;;;              GP-NEWOBLOCAT
;;;              NIL
;;;              PH-BLOCKS
;;;              NIL)
;;;        (GP-INITIAL)))
;;;       (TERPRI)
       (SAY YOU ARE NOW IN A READ-EVAL-PRINT LOOP)
       (TERPRI)
       (SAY TYPE "GO/ " TO ENTER READY STATE)
       (CATCH (ERT) ABORT-PARSER)
       (sstatus toplevel '(shrdlu))
       (SHRDLU))

(DEBUGMODE)

(setq sh-standard-printout t smnbreak nil smntrace nil makintern t annoyance nil)

(SSTATUS PAGEPAUSE T)

(IOC D)
(setq errlist nil)

(SETQ Z (status tty) w1 (car z) w2 (cadr z))
(setq w1 (boole 7 w1 020202020202)
      w2 (boole 7 w2 020202020202) )
(sstatus tty w1 w2)

§ anno/winograd/progmr

(DECLARE (GENPREFIX PROGMR))

;;;**********************************************************
;;;
;;;                         PROGMR
;;;     (INTERPRETER FOR THE PROGRAMMAR GRAMMAR WRITING LANGUAGE)
;;;
;;;############################################################

(DEFUN RESTOREPT NIL (SETQ PT SAVEPT))

(DEFUN SETMVB (PTR-MVB)
       (PROG (SAVE)
         (SETQ MVB PTR-MVB)                           ;IF THERE ARE SEVERAL CLAUSES ACTIVE AT THE
         (SETQ SAVE PT)
         (SETQ PT PTR-MVB)                           ;SAME TIME IT SETS THE NEAEST ONE.
         (SETR 'MVB PTR-MVB (MOVE-PT C U (CLAUSE)))
         (SETQ PT SAVE)
         (RETURN T)))

(DEFUN ADD-F-PT (FEATURE PTR)
       (PUTPROP (CAR PTR) (CONS FEATURE (FE PTR)) 'FEATURES)
       (AND (EQ PTR C) (SETQ FE (FE PTR)))
       T)

(DEFUN REMOVE-F-PT (FEATURE PTR)
       (PUTPROP (CAR PTR)
        (SETDIF (GET (CAR PTR) 'FEATURES)
            (LIST FEATURE))
        'FEATURES)
       (AND (EQ PTR C) (SETQ FE (FE PTR)))
       T)

(DEFUN ONE-WORD-LEFT NIL (AND (CDR NB) (NOT (CDDR NB))))

(SETQ SMNBREAKS NIL
      ;;;   a list of smnfns which will
      ;;;    be broken at (before calling)
      )

(DEFUN CALLSM FEXPR (SEMANTIC-EXPRESSION)
       (PROG (RESULT MPLNR-TIME SM-TTIME GC SMNFN)
         (SETQ SMNFN (CAR SEMANTIC-EXPRESSION))
         (AND SMNTRACE
          (APPLY 'SAY
             (LIST 'SEMANTICS
                   '*****
                   UNIT
                   'CALLING
                   SMNFN)))
         (AND SMN
          (COND ((OR (EQ SMNBREAKS T) (MEMQ SMNFN SMNBREAKS))
             (RETURN (ERT)))
            (T (RETURN T))))
         (AND SMNTRACE
          (PROGN (PRINTC '/ / CALLSM:/ )
             (PRINC (CAR SEMANTIC-EXPRESSION))))
         (SETQ MPLNR-TIME 0.)
         (SETQ GC (STATUS GCTIME)
           SM-TTIME (RUNTIME)
           RESULT (EVAL (CAR SEMANTIC-EXPRESSION)))
         (SETQ SM-TIME (PLUS SM-TIME
                 (DIFFERENCE (TIMER SM-TTIME
                            (RUNTIME))
                         MPLNR-TIME)))
         (OR (= GC (STATUS GCTIME))
         (SETQ SM-TIME
               (DIFFERENCE SM-TIME (TIMER GC (STATUS GCTIME)))
               P-GC
               (STATUS GCTIME)))
         (SETQ MP-TIME (PLUS MP-TIME MPLNR-TIME))
         (AND SMNTRACE
          (PROGN (PRINTC 'CALLSM/ RETURNING:/ )
             (PRINC RESULT)))
         (COND ((OR (EQ SMNBREAKS 'ALL)
            (MEMQ SMNFN SMNBREAKS))
            (ERT)))
         (RETURN RESULT)))

(DEFUN MOVE-PT FEXPR (L)
       (PROG (XX YY L2 EXEC SAVE)
         (SETQ EXEC L)
         (SETQ SAVE PT)
    TEST1(COND ((AND (CDR EXEC) (NOT (ATOM (CADR EXEC))))
            (GO TEST)))
    LOOK1(SETQ XX (CAR EXEC))
    LOOK (COND ((EQ XX 'H)
            (OR (SETQ PT H) (GO FAIL))
            (GO EX))
           ((EQ XX 'C) (SETQ PT C) (GO EX))
           ((EQ XX 'PC)
            (SETQ PT (H (PARENT C)))
            (GO EX))
           ((EQ XX 'LASTSENT)
            (SETQ PT LASTSENT)
            (GO EX))
           ((EQ XX 'U)
            (OR (SETQ PT (PARENT PT)) (GO FAIL)))
           ((EQ XX 'DLC)
            (OR (SETQ PT (H PT)) (GO FAIL)))
           ((EQ XX 'DF)
            (SETQ L2 (CONS 'DLC (CONS 'FR L2)))
            (SETQ XX 'DLC)
            (GO LOOK))
           ((EQ XX 'FR)
            (COND ((MOVE-PT PV) (GO LOOK))))
           ((EQ XX 'NX)
            (OR (SETQ PT (PREVIOUS (H (PARENT PT)) (CAR PT)))
            (GO FAIL)))
           ((EQ XX 'PV)
            (SETQ PT (OR (AND (EQ PT C) (H (PARENT C)))
                 (FOLLOWING (H (PARENT PT)) (CAR PT))
                 (GO FAIL))))
           (T (PRINT XX) (ERT MOVE-PT ILLEGAL INSTRUCTION)))
    EX   (COND ((OR (NULL L2) (NULL (SETQ L2 (CDR L2))))
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
    TEST1(COND ((AND (CDR EXEC) (NOT (ATOM (CADR EXEC))))
            (GO EX)))
    LOOK1(SETQ XX (CAR EXEC))
    LOOK (COND ((EQ XX 'N) (SETQ PTW N))
           ((EQ XX 'LASTSENT) (SETQ PTW (NB LASTSENT)))
           ((EQ XX 'FW) (SETQ PTW (NB PT)))
           ((EQ XX 'AW)
            (COND ((EQ PT C) (GO FAIL))
              ((SETQ PTW (N PT))
               (SETQ XX 'PW)
               (GO LOOK))))
           ((EQ XX 'LW)
            (COND ((EQ PT C) (GO FAIL))
              ((SETQ PTW (N PT))
               (SETQ XX 'PW)
               (GO LOOK))))
           ((EQ XX 'NW)
            (COND ((SETQ PTW (CDR PTW)))
              ((SETQ PTW (FINDB SENT NIL)) (GO FAIL))))
           ((EQ XX 'PW)
            (COND ((SETQ PTW (FINDB SENT PTW)))
              ((SETQ PTW SENT) (GO FAIL))))
           ((EQ XX 'SFW) (SETQ PTW SENT))
           ((EQ XX 'SLW) (SETQ PTW (FINDB SENT NIL)))
           ((BUG MOVE-PTW ILLEGAL INSTRUCTION)))
    EX   (COND ((NULL (CDR EXEC)) (RETURN PTW))
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
     (LAMBDA (A) (PROG (B C)
               (SETQ B N)
              LOOP (COND ((EQ A B)
                  (SETQ CUT A)
                  (SETQ NN (NOT (EQ CUT N)))
                  (RETURN T))
                 ((EQ B END) (RETURN NIL))
                 ((SETQ B (CDR B)) (GO LOOP))
                 ((NULL A)
                  (SETQ CUT NIL)
                  (SETQ NN N)
                  (RETURN T)))))
     EXPR)

(DEFUN CUT-BACK-ONE NIL (MOVE-PTW N PW) (POP) (CUT PTW))

(DEFPROP F
     (LAMBDA (A) (COND ((MEMBER A FE) T)
               ((SETR 'FEATURES
                  (SETQ FE (CONS A FE))
                  C))))
     EXPR)

(DEFUN FE (NODE) (GETR 'FEATURES NODE))

(DEFUN FEATURE? (FEATURE) (MEMQ FEATURE FE))

(DEFUN FESET (NODE FEATURES) (SETR 'FEATURES FEATURES NODE))

(DEFUN FLUSHME NIL
       ;; IF YOU HAVEN'T REAHED THE CUT, FLUSHES THE NEXT WORD IN THE
       ;;SENTENCE.  FAILS IF IT REACHES CUT POINT
       (AND N NN (SETQ NN (NOT (EQ CUT (SETQ N (CDR N)))))))

(DEFUN FOLLOWING (LIST MEMBER)
       ;; GET THE ELEMENT OF LIST FOLLOWING MEMBER
       (AND (MEMQ MEMBER LIST) (CDR (MEMQ MEMBER LIST))))

(DEFUN FQ FEXPR (A)
       (MAPCAR
    (FUNCTION (LAMBDA (X) (OR (MEMQ X FE) (SETQ FE (CONS X FE)))))
    A)
       (SETR 'FEATURES FE C))

(DEFUN GETR (REGISTER NODE)
       ;; THIS FUNCTION RETRIEVES THE CONTENTS OF THE REGISTER
       ;;ASSOCIATED WITH THE GIVEN NODE
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

(DEFUN NEXTWORD NIL (CAR N))                            ;RETURN THE NEXT WORD IN THE SENTENCE

(DEFUN NEXTWORD? (A) (EQ (CAR N) A))

(DEFUN NQ FEXPR (A) (MEMQ (CAR A) (FE N)))

(DEFUN ONLY-ONE-WORD-LEFT NIL (AND N (NOT (CDR N))))

(DEFUN PARENT (NODE) (GETR 'PARENT NODE))

(DEFUN PARSE FEXPR (A)
       (COND ((MEMQ (CAR A) '(NG CLAUSE VG PREPG ADJG))
          (PARSE2 A (MEMQ 'TOPLEVEL A)))
         ((PARSE3 A NIL))))

(DEFUN PARSE2 (REST P)
       ;;THIS FUNCTION CALLS THE PROGRAMMAR FUNCTION INDICATED BY THE
       ;;FIRST MEMBER OF REST - A FEATURE LIST THE PARAMETER P
       ;;INDICATES WHETHER PARSE2 IS BEING CALLED FROM THE TOPLEVEL
       ;;IF P IS TRUE, PARSE2 DOES NOT APPEND THE NODE JUST PARSED TO
       ;;THE PARSING TREE PARSE2 WILL CALL EITHER A COMPILED OR
       ;;INTERPRETED VERSION OF THE GRAMMAR PROGRAM
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
             (PROGN (PRINTC '/ / SPECIAL/ WORD)
                (PRINC (CAR N))))
            (EVAL (GETR 'B-SPECIAL N))))
         (COND ((EQ SPECIAL 'SKIP) (GO SKIP))
           ((EQ SPECIAL 'DONE) (GO DONE))
           ((EQ SPECIAL 'LOSE) (SETQ N NBB) (GO LOSE)))
         (AND PARSETRACE
          (PROGN (TERPRI)
             (PRINC '/()
             (PRINC LEVEL)
             (PRINC '/ ####/ PARSING:/ )
             (PRINC REST)))
         (COND ((NULL (SETQ RE (APPLY-GRAMMAR UNIT)))           ;THIS IS WHERE ALL THE WORK HAPPENS. IF THE
            (SETQ RE NIL)                       ;PARSE SUCEEDS, IT WILL RETURN THE NODE THAT HAS
            (SETQ N NBB)                       ;BEEN BUILT UP (SEE THE FUNCTION "INTERPRETATION" IN IN GINTER)
            (GO LOSE)))
    SKIP (COND ((EQ N CUT))
           ((NQ SPECIAL) (EVAL (GETR 'SPECIAL N))))
    DONE (OR P
         (REBUILD (SETQ FE (GET (CAR C) 'FEATURES))           ;REBUILDS THE HIGHER LEVEL NODE TO INCORPORATE
              NB                           ;THE DAUGHTER THAT WAS JUST PARSED EXCEPT IN THE
              N                           ;CASE WHERE THIS NODE IS THE TOPLEVEL
              (SETQ H (APPEND RE H))
              SM
              C))
    LOSE (SETQ NN (NOT (EQ N CUT)))
    OK   (COND ((AND RE
             (OR (EQ PARSETRACE 'ALL)
                 (EQ PARSEBREAK 'ALL)
                 (MEMQ UNIT PARSEBREAK)
                 (MEMQ UNIT PARSETRACE)))
            (TERPRI)
            (PRINC '/()
            (PRINC LEVEL)
            (PRINC '/ PARSE/ SUCEEDED:/ )
            (PRINC UNIT)
            (PRINC '/ / )
            (PRINC (FROM (NB RE) N))
            (AND PARSENODE-SEE (DP (CAR RE)))
            (AND (OR (EQ PARSEBREAK 'ALL)
                 (MEMQ UNIT PARSEBREAK))
             (ERT)))
           ((OR PARSEBREAK PARSETRACE)
            (TERPRI)
            (PRINC '/()
            (PRINC LEVEL)
            (PRINC '/ PARSE/ FAILED)
            (AND (OR (EQ PARSEBREAK 'ALL)
                 (MEMQ UNIT PARSEBREAK))
             (ERT))))
         (PARSE-STATISTICS)                           ;defined in SYSCOM
         (SETQ LEVEL (1- LEVEL))
         (RETURN RE)))

(DEFUN PARSE3 (REST P)
       ;; PARSE3 IS CALLED TO PARSE JUST THE NEXT WORD IN THE
       ;;SENTENCE
       (PROG (XP LABL RE SPECIAL NBB NODE)
         (COND ((EQ (SETQ NBB N) CUT) (MQ CUT) (RETURN NIL))
           ((NQ B-SPECIAL)                       ;IS THE NEXT WORD MARKED SPECL?
            (EVAL (GETR 'B-SPECIAL N))                   ;YES, DO SOMETHING SPECIALL
            (COND ((EQ SPECIAL 'SKIP) (GO SKIP))
              ((EQ SPECIAL 'LOSE)
               (SETQ N NBB)
               (RETURN NIL))
              ((EQ SPECIAL 'DONE) (GO DONE)))))
         (COND ((CAR (SETQ XP REST)))                   ;IF CALL IS (PARSE NIL FOO)
           ((NEXTWORD? (CADR REST)) (GO OK))               ;THEN LOOK FOR EXACT WORD "FOO"
           ((SETQ N NBB) (RETURN NIL)))                   ;IF NOT THERE, FAIL
    LOOP (COND ((NOT (ATOM (CAR XP)))
            (SETQ LABL (CONS (CAAR XP) LABL)))               ;IF THE FEATURE IS NOT AN ATOM JUST ADD THE
           ((EQ (CAR XP) 'NULL))                   ;FEATURE TO THE LIST
           ((MEMQ (CAR XP) (FE N)))
           ((MEMQ (CAR XP) UNMKD))
           ((M (CAR XP)) (SETQ N NBB) (RETURN NIL)))
         (COND ((SETQ XP (CDR XP)) (GO LOOP)))
    OK   (SETQ
          RE
          (BUILDNODE (MEET (APPEND (FE N) LABL)
                   (GET (CAR REST) 'ELIM))
             N
             (CDR N)
             'WORD
             (OR SMN
                 (NULL (CAR REST))
                 (AND (NULL (SM N)) (UNDEFINED))
                 (CADR (SASSOC (CAR REST)
                       (SM N)
                       (FUNCTION UNDEFINED))))))
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
    GO   (COND ((NULL A) (RETURN NIL))
           ((NOT (ISX NODE (CAAR A))))
           ((EVAL (APPEND '(PARSE CLAUSE RSNG)
                  (CDAR A)
                  B))
            (RETURN H)))
         (SETQ A (CDR A))
         (GO GO)))

(DEFUN POP FEXPR (A)
       (COND
    ((OR (NULL A) (NULL (CAR A)))
     (COND
      ((NULL H) NIL)
      ((SETQ N (NB H))
       (SETQ H (CDR H))
       (REBUILD FE NB N H SM C)
       (SETQ NN (NOT (EQ N CUT)))
       (OR
        SMN
        (PROG (XX)
          (MAP
           '(LAMBDA (BACKNODE)
             (ERRSET
              (AND (MAP '(LAMBDA (PLACE)
                     (AND (EQ PLACE (NB BACKNODE))
                          (ERR)))
                N)
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
    EX   (COND ((EQ XX H) (RETURN C)) ((POP) (GO EX)))))

(DEFUN PREVIOUS (LIST MEMBER)
       ;; GET THE ELEMENT OF LIST BEFORE MEMBER
       (PROG (GOODIE)
    GO   (COND ((NULL LIST) (RETURN NIL))
           ((EQ MEMBER (CAR LIST)) (RETURN GOODIE))
           (T (SETQ GOODIE (CAR LIST))
              (SETQ LIST (CDR LIST))))
         (GO GO)))

(DEFUN PTFIND (X YY Z)
       (PROG (FOO)
         (SETQ FOO (CAR X))
    UP   (COND ((MOVE-PT U) (GO UP)) ((EQ (NB PT) X) (GO ON)))
    DOWN (OR (MOVE-PT DLC PV (MEMQ FOO (NB PT))) (RETURN NIL))
    ON   (COND ((NOT (EQ X (NB PT))) (GO DOWN))
           ((EQ YY T))
           ((MOVE-PT DF (EQ (N PT) YY)))
           ((RETURN NIL)))
    CHECK(COND ((EVAL Z) (RETURN PT))
           ((NOT (EQ YY T)))
           ((MOVE-PT DF) (GO CHECK)))))

(DEFUN REBUILD (FEATURES FIRSTWORD WORDAFTER DAUGHTERS SEMANTICS
        NODE)
       (SETR 'FEATURES FEATURES NODE)
       (SETR 'FIRSTWORD FIRSTWORD NODE)
       (SETR 'WORDAFTER WORDAFTER NODE)
       (SETR 'DAUGHTERS DAUGHTERS NODE)
       (SETR 'SEMANTICS SEMANTICS NODE)
       NODE)

(DEFUN ROOT (X)
       ;;; INPUT= PIECE OF SENTENCE
       ;;; OUTPUT= ROOT OF FIRST WORD IN THAT PIECE
       ;;; IF WORD HAS NO ROOT PROPERTY, THE ROOT == WORD
       (OR (GET (CAR X) 'ROOT) (CAR X)))

(DEFUN RQ FEXPR (A) (SETR 'FEATURES (SETQ FE (SETDIF FE A)) C))        ;REMOVE THE FEATURE A FROM FEATURE LIST OF THE CURRENT NODE

(DEFUN SECONDWORD? (WORD) (AND N (CDR N) (EQ (CADR N) WORD)))

(DEFUN SETR (REGISTER VALUE NODE)
       ;; THIS FUNCTION ASSOCIATES THE GIVEN VALUE WITH THE GIVEN
       ;;NODE UNDER THE GIVEN INDICATOR, REGISTER
       (PUTPROP (CAR NODE) VALUE REGISTER))

(DEFUN SM (NODE) (GETR 'SEMANTICS NODE))

(DEFUN TRNSF FEXPR (A)
       (SETR 'FEATURES
         (SETQ FE (UNION (MEET A (FE PT)) FE))
         C))

(DEFUN UPREL (X)
       (AND (NOT (ATOM X))
        (OR (MEMQ 'UPREL (FE X))                       ; FIND NODE WITH UPREL FEATURE
        (UPREL (H X))
        (UPREL (CDR X)))))

(DEFUN WORD (N) (CAR N))

(DEFPROP UPCHECK
     (LAMBDA NIL (AND (MOVE-PT C U (REL-NOT-FOUND))
              (NOT (MEET (FE PT)
                     '(OBJ1Q OBJ1REL
                         OBJ2Q
                         OBJ2REL
                         LOBREL
                         LOBQ)))))
     EXPR)

§ anno/winograd/ginter

(DEFUN PDEFINE FEXPR (A)
       ;;THIS PDEFINE MERELY PUT THE PROGRAMMAR FUNCTION ON THE
       ;;PROPERTY LIST OF THE PARSE NAME UNDER THE INDICATOR
       ;;'INTERPRET. IT ALSO ADDS THE TAGS FAIL AND RETURN. NOTE THAT
       ;;THE PDEFINE BODY IS SIMILIAR TO PROG BODY. THIS SETS UP
       ;;INTERPRETED PROGRAMMAR EXECUTIONS
       (PUTPROP (CAR A)
        (NCONC (CDR A)
               (LIST 'FAIL
                 '(RETURN 'FAIL)
                 'RETURN
                 '(RETURN 'RETURN)))
        'INTERPRET))

(DEFUN INTERPRET (UNIT)
       ;; INTERPRET IS THE FUNCTION WHICH 'CALLS' AN INTERPRETED
       ;;PROGRAMMAR PROGRAM.  IT FIRST DECLARES AND INITIALIZES ALL
       ;;THE RELAVENT VARIABLES THEN IT EXECUTES THE PROGRAMMAR BODY
       ;;AS A PROG NOTE THE USE OF "RE". IT IS SET TO A NODE ONE
       ;;WISHES TO BE THE INITIAL DAUGHTER OF THIS NODE - ONLY CONJ
       ;;NEEDS THIS HACK
       (PROG (FE H ME NB C SM CUT NN T1 T2 T3)
         (SETQ NN T)
         (SETQ CUT END)
         (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))           ;FEATURE LIST
                (SETQ NB (OR (NB RE) N))           ;BEGINNING IN SENTENCE OF THIS NODE
                N                       ;SENTENCE POINTER JUST AFTER THIS NODE
                (SETQ H RE)                   ;DAUGHTERS OF THIS NODE
                NIL))                       ;SEMANTIC JAZZ
         (SETR 'PARENT PARENT C)                       ;SET PARENT REGISTER
         (COND ((EQ (APPLY 'PROG
                   (GET UNIT 'INTERPRET))
            'RETURN)
            (GO RETURN)))                       ;APPLY THE PROGRAMMAR PROGRAM
    FAIL (SETQ MES ME)
         (SETQ N (OR (N RE) NB))                       ;RESET SENTENCE POINTER
         (RETURN NIL)
    RETURN
         (SETQ MES ME)
         (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN : FEXPR (BRANCH)
       (COND ((EVAL (CAR BRANCH))                       ;EVALUATE BRANCH CONDITION
          (COND ((AND (NULL NN) (CDDDR BRANCH))
             (GOCHECK (CDDR BRANCH)))                   ;IF TRUE AND NO MORE SENTENCE REMAINS
            (T (GOCHECK BRANCH))))                   ;AND IF THIRD BRANCH GIVEN,THEN GO TO THIRD
         (T (GOCHECK (CDR BRANCH)))))                    ;BRANCH 2ND BRANCH

(DEFUN GOCHECK (LABEL)
       ;;THE GOCHECK FUNCTION CHECKS THE BRANCHES OF THE PROGRAMMAR
       ;;CONDITIONAL IF THE BRANCH IS NIL, GOCHECK MERELY RETURNS IF
       ;;THE BRANCH IS NON-ATOMIC, IT IS TAKEN AS A FAILURE MESAGE
       ;;GOCHECK PUTS THE MESAGE ON THE MESSAGE LIST AND GOES TO
       ;;FAIL(IN GINTERP) IF THE BRANCH IS ATOMIC, GOCHECK GOES TO IT
       ;;REMEMBER THAT THE GO LEADS TO A POINT SOMEWHERE IN THE
       ;;PROGRAMMAR PROGRAM UNLESS IT IS 'FAIL OR 'RETURN
       (COND ((NULL (CADR LABEL)) T)
         ((ATOM (CADR LABEL)) (GO (CADR LABEL)))
         (T (M (CADR LABEL)) (GO FAIL))))

(DEFUN GOCOND FEXPR (A)
       ;;GOCOND GOES TO THE 1ST OR 2ND OF TWO TAGS DEPENDING IF THERE
       ;;REMAINS ANY MORE OF THE SENTENCE YET TO BE PARSED
       (COND (NN (GO (CAR A))) (T (GO (CADR A)))))

§ anno/winograd/gramar

(PDEFINE CLAUSE (POSITION-OF-PRT MVB LOCATIONMARKER
         SUBJ-VB-BACKUP-TYPE1 POSITION-OF-PTW)

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
FDEC    (FQ DECLAR)
     ;;;
    THEREINIT                                   ;CONSTRUCTIONS USING THE FUNCTION WORD "THERE"
     (: (AND (NEXTWORD? 'THERE)                       ;ARE CHECKED FOR EXPLICITLY AND PROCESSED BY A
         (PARSE NIL THERE)                       ;SPECIAL BLOCK OF CODE (SEE ABOVE)
         (FQ DECLAR))
        THERE
        NIL
        (INIT))

     ;;;
    THER2(AND (NQ PREP)
          (PARSE PREPG INIT)
          (OR (CALLSM (SMRELATE H))                       ;MORE INITIAL (BEFORE THE SUBJECT) MODIFIERS
          (POP)))
     (AND (NQ ADV)
          (PARSE ADV TIMW)
          (OR (CALLSM (SMADVERB)) (POP)))
     (AND (NQ ADV)
          (PARSE ADJG ADV VBAD)
          (OR (CALLSM (SMRELATE H)) (POP)))
     (PARSE NG TIME)

     ;;;
     (: (EQ LOCATIONMARKER N) CLAUSETYPE INIT INPOP)

     ;; THE VARIABLE "LOCATIONMARKER" MARKS THE POSITION OF PTW
     ;;AT THE TIME THAT IT WAS SET.  IF IT HAS NOT MOVED (IS
     ;;STILL EQUAL TO N) THAN THAT INDICATES THAT NOTHING HAS
     ;;BEEN PARSED AND WE CAN GO ON, OTHERWISE, THERE CAN BE ANY
     ;;NUMBER OF INITIAL MODIFIERS AND THE CODE STARTING AT
     ;;"INIT" IS REPEATED, AS MANY TIMES AS NECESSARY.  IF PTW
     ;;HITS THE CUT POINT, THAN IT IS ASSUMED THAT SOMETHING WAS
     ;;MISTAKENLY PARSED AS A MODIFIER WHEN IT WAS NOT, AND
     ;;EVERYTHING IS POPPED OFF (BY THE "INPOP" CODE)
    INPOP(: (MOVE-PT C DLC) NIL (INPOP))                   ;DOES ANYTHING REMAIN ON THE TREE?
    BICUT(CUT-BACK-ONE)                               ;"CUT-BACK-ONE" IS THE NORMAL BACKINGUP
     (GO INIT)                               ;MECHANISM FOR THE GRAMMAR, IT SETS PTW (POINTER
                                       ;TO THE WORD) BACK ONE FROM WHERE IT WAS AND
                                       ;SETS "CUT" TO PTW. THE FOLLOWING GOTO TELLS
                                       ;WHICH BLOCK OF CODE IS TO BE REPEATED.

     ;;;
     ;;;------------ RE-EXAMINE THE CLAUSETYPE, PARTICULARLY TO CHECK FOR VERB-INITIAL IMPERATIVES
     ;;;
    CLAUSETYPE
     (: (CQ DECLAR) SUBJ NIL)
     (: (AND (NQ VB) (NQ INF) (PARSE VG IMPER) (FQ IMPER))
        VG1
        NIL)                               ;SEE THE NOTE UNDER IMPERATIVES BELOW
     (FQ DECLAR)
     (: (CQ IMPER) (IMPER) NIL)

     ;;;
     ;;;
     ;;;**********************************************************************
     ;;;                  TRY TO PARSE A GRAMMATICLY ACCEPTABLE SUBJECT.
     ;;;
     ;;;  ONCE THAT IS DONE, SET THE SUBJECT REGISTER (FOR USE BY SEMANTIC ROUTINES AND OTHER PARTS OF THE GRAMMAR)
     ;;;  AND MOVE ONE TO THE CODE FOR WHICH LOOKS FOR THE MAIN VERB (MVB) -"VG"
     ;;;**********************************************************************
     ;;;
    SUBJ (CUT END)                               ;RESET CUTPOINT INCASE IT WAS MODIFIED BY
    SUBJ3                                   ;PREVIOUS BACKUPS IF THE FIRST WORD INDICATES
     (: (OR (AND (NEXTWORD? 'TO)                       ;THE POSSIBILITY OF A RANK-SHIFTED CLAUSE
             (PARSE CLAUSE RSNG TO SUBJ))               ;SERVING AS THE SUBJECT, THEN TRY TO PARSE ONE
        (AND (PARSE CLAUSE RSNG ING SUBJ)))               ;AS SUCH FEATURE "SUBJ" INSURES A CHECK THAT ANY
        SUBREG                               ;PRONOUNS FOUND ARE IN SUBJECTIVE CASE.
        NIL
        SUBJ1)

     ;;;
     ;;;
    SUBJ4(: (PARSE NG SUBJ) SUBREG NIL SUBJ1)                   ;IF PARSING THE SUBJ CAUSES THE CUT POINT TO BE
                                       ;REACHED, THEN JUMP TO "SUBJ1" TO SEE IF WE ARE
                                       ;IN CONDITIONS WHERE THAT IS ALLOWED

     ;;;
     ;;;                                   ;WHAT TO DO IF THE SUBJECT CANNOT BE DIRECTLY
                                       ;PARSED THIS IS CHECKING FOR THE SITUATION WHERE
                                       ;A QUESTION WORD IS ACTING AS LOGICAL SUBJECT
     (COND ((CQ REL-NOT-FOUND)                       ;AND HAS ALREADY BEEN PARSED AS IN "WHAT IS IN
                                       ;THE BOX?" THE CLAUSE WILL HAVE THIS FEATURE IF
                                       ;IT IS ACTIVE AS A RSQ AND ITS MISSING ELEMENT
        (RQ REL-NOT-FOUND)                       ;HAS NOT YET BEEN DETERMINED.  SINCE WE CANNOT
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)               ;FIND ANY SUBJECT, WE ASSUME THAT IT IS A
        (GO VB))                           ;SUBJECT-RELATIVE IN THIS CASE.
           (SUBJ-VB-BACKUP-TYPE1 (SETQ SUBJ-VB-BACKUP-TYPE1 NIL)
                     (GO SUBJ11))               ;SEE THE LARGE NOTE ABOUT THIS IN "NOVERB".
           ((AND H (ISQ H TIME) (ISQ H NG))
        (SETR 'SUBJECT H C)
        (GO VB))                           ;WHAT WAS INITIALLY PARSED AS A TIME-NG MODIFING
           ((MOVE-PT C U (REL-NOT-FOUND))                   ;THE WHOLE CLAUSE MAY PROBABLY BEEN THE SUBJECT
                                       ;OF THE CLAUSE THIS WORRIES ABOUT RELATIVE
                                       ;CLAUSES. PLEASE NOTE THAT THE CURRENT
                                       ;HALF-VERSION HAS NOT YET GOT ITS HEAD TOGETHER
                                       ;ABOUT RELATIVE CLAUSES. -IE. THE CODE ISN'T
        (SETR 'SUBJECT (GETR 'RELHEAD PT) C)               ;DEBUGGED AND HAS GAPS IN IT ESP. WHO SETS WHAT
        (SETR 'RELHEAD (GETR 'RELHEAD PT) C)               ;REGISTER WHEN THIS WILL BE FIXED BEFORE THE
        (REMOVE-F-PT 'REL-NOT-FOUND PT)                   ;VERSION IS FINALIZED
        (GO VB))
           ((AND (CQ COMPONENT) NN) (FQ SUBJFORK) (GO VB))           ;"SARAH ATE DINNER AND WENT TO THE MOVIES."
           (H (POP) (GO SUBJ))                       ;POP OFF THE CLOSEST INITIAL MODIFIER AND TRY TO
           ((GO FAIL)))                           ;PARSE A SUBJ AGAIN

     ;;;
     ;;;
    HEAD (: (OR (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON)))           ;COME HERE (ONLY?) TO TRY TIME PHRASE AS SUBJECT
        NIL
        (HEAD))                               ;MOVE PTW TO THE CLOSEST NOUN THEN SET THE CUT
    SUB2 (: (POP) NIL FAIL)                           ;POINT TO IT AND ATTEMPT A NEW PARSING IF
     (: (CUT PTW) INIT SUB2)                       ;NOTHING MORE TO POP, LOSE

     ;;;
    SUBJ1(COND ((ISQ H QUOTED)                           ;CIRCUMSTANCES UNDER WHICH IT IS ALLRIGHT TO
        (AND (ISQ H LIST) (FQ LIST))                   ;HAVE NOTHING FOLLOWING THE SUBJECT OF THE
        (FQ QUOTED)                           ;CLAUSE "  "MUMBLE", SAID JOHN."
        (SETQ H (H H))
        (GO RETSM)))
     (AND (CQ REL-NOT-FOUND)                       ;THIS IS PART OF A BACKUP MECHANISM WHICH NEEDS
          (MOVE-PT H PV (QAUX))                       ;TO BE MORE THROUGHLY THOUGHT OUT. THE SITUATION
          (COND ((ISQ PT BE)                       ;IS EXPLAINED IN DETAIL IN QUESTION.NGQST MOVE
             (FQ INT AUXBE)                       ;PT TO A VERB WHICH CAN BE AN AUXILLIARY AND
             (RQ REL-NOT-FOUND)                       ;WHICH CAN BEGIN A CLAUSE
             (SETR 'COMP (GETR 'RELHEAD C) C)
             (SETR 'SUBJECT H C)                   ;"WHAT COLOR IS THE BLOCK?" OR "HOW BIG IS THE
             (SETMVB PT)                       ;BLOCK?"
             (GO ONT))
            ((ISQ PT HAVE)
             (FQ SUBQ)
             (RQ REL-NOT-FOUND)
             (SETR 'SUBJECT (GETR 'RELHEAD C) C)
             (GO VBL))))

     ;;;
    SUBJ11
     (: (CUT-BACK-ONE) SUBJ3 (SUBJ11))                   ;IF WE CAN'T CUT BACK ANY FURTHER, THEN FAIL
    SUBREG
     (SETR 'SUBJECT H C)                           ;THIS SETS THE "SUBJECT" REGISTER OF THE CURRENT
     (GO VB)                               ;CURRENT NODE TO WHATEVER IS POINTED TO BY "H"
                                       ;(IN THIS CASE THAT WOULD BE THE MOST RECENTLY
                                       ;PARSED DAUGHTER OF THE CURRENT NODE)

     ;;;
     ;;;*******************************************************************
     ;;;                          PARSE A VERB GROUP
     ;;;
     ;;;   ONCE THE VERB GROUP IS PARSED, THE TRANSITIVITY OF THE MAIN VERBIS EXAMINED - CAUSEING THE
     ;;;   APPROPRIATE SECTIONS OF THE CODE BELOW TO BE EXECUTED AND TO ATTEMPT TO PARSE THE REQUIRED OBJECTS
     ;;;      ONCE ALL THE OBJECTS HAVE BEEN PARSED, THE PROGRAM JUMPS TO THE TAG "ONT", WHERE A SEMANTICS PROGRAM
     ;;;   IS CALLED TO MAKE SENCE OUT OF EVERYTHING
     ;;;******************************************************************
     ;;;
    VB     (: (PARSE ADJG ADV VBAD) VB NIL (VB-ADJG))               ;PARSE ANY INITIAL MODIFIERS
     (RQ VBLOK)                               ;?????

     ;;;
    VBL     (: (PARSE VG) VBREG NIL)                       ;ONCE THE VERB GROUP IS PARSED, SET THE REGISTER

     ;;;
    NOVERB
     (COND ((CQ SUBJFORK) (FQ VBFORK) (GO FINDOBJ1))           ;WHAT    TO DO IF THE VG CANNOT BE DIRECTLY
           ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))           ;PARSED
           ((NOT (ISQ H SUBJ)) (GO FAIL))
           ((ISQ H CLAUSE)
        (SETQ SUBJ-VB-BACKUP-TYPE1 T)
        (POP)
        (GO SUBJ4))                           ;THIS IS EXACTLY WHAT IS LOOKS LIKE. IE. AN
                                       ;ARBITRARY, NOT TOO WELL THOUGHTOUT BACKUP
                                       ;MECHANISM. (NEEDLESS TO SAY IT WILL GO AWAY
                                       ;FAST)  WE HAVE BEEN UNABLE TO FIND A VERB AND
                                       ;HAVE NOTICED THAT WE PARSED A CLAUSE OF SOME
                                       ;SORT AS THE SUBJECT. HYPOTHESIS: WE
                                       ;MISSINTERPRETED SOMETHING WHILE PARSEING THAT
                                       ;CLAUSE AND MANAGED TO SWALLOW UP THE VERB OF
           ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))           ;THE HIGHER CLAUSE WITH IT. SOLUTION: POP OFF
    VB2     (CUT-BACK-ONE)                               ;THE CLAUSE AND TRY TO  REPARSE THE SEGMENT IN
     (GO SUBJ3)                               ;ANOTHER FASHION. "SUBJ4" IS PLACED THE THE
                                       ;SUBJECT CODE AFTER LOOKING FOR CLAUSES AND
                                       ;BEFORE NOUN GROUPS. DEFAULT CUTTING MECHANISM
                                       ;FOR VBL

     ;;;
    VBREG(SETR 'VG H C)

     ;;;*******************************************************************
     ;;;
     ;;;                                   PARSE ANY OBJECTS REQUIRED BY THE VERB
     ;;;
     ;;;*******************************************************************
     ;;;
    VG1     (CUT END)                               ;RESET THE CUTPOINT IN CASE ANYONE CHANGED IT

     ;;;
     (: (ISQ MVB BE) BE NIL (BE))                       ;JUMP TO "BE" PROCESSOR

     ;;;
     ;;; There used to be a check here for a quoting MVB with
;;; a quoted subject. It was deleted because it went to a tag that no longer
;;; exists and doesn't seem to have any modern analogs.
;;; for the original code: see "gramar 19" or earlier. It was put in by
;;; Jeff Hill in the spring of 1972.

     ;;;
     ;;;--------------------------------------------------   VERB-PARTICLE COMBINATIONS
     ;;;
     ;;;     - SUCH AS "PUT ON", "SET DOWN", ETC. -   THEIR ESSENTIAL PROPERTY IS THAT VERB AND PARTICLE
     ;;;     CAN BE DISPLACED BY THE OBJECT.
     ;;;        "PUT DOWN THE BLOCK."
     ;;;        "PUT THE BLOCK DOWN."
     ;;;
     ;;;
     ;;;
     (: (ISQ MVB VPRT) NIL CHECKPASV CHECKPASV)
     (: (AND (NQ PRT) (PARSE PRT)) NIL DPRT)               ;IF THE PARTICLE IS NOT THE WORD FOLLOWING THE
     (FQ PRT)                               ;VERB THEN IT IS SEARCHED FOR BY CODE AT "DPRT"
                                       ;(DISPLACED PARTICLE)

     ;;;
     (: (SETMVB (COMBINATION? (ROOT (NB MVB)) (WORD (NB H))))      ;IS THIS A LEGITIMATE COMBINATION OF VERB AND
        CHECKPASV                               ;PARTICLE ?
        POPRT)
    DPRT (: (ISQ H PASV) CHECKPASV NIL)                       ;SEARCH FOR DISPLACED PARTICLE  NO DISPLACED
     (: (SETQ POSITION-OF-PRT (MOVE-PTW N NW (PRT))) NIL FINDOBJ1) ;PARTICLES IN PASV'S IF NOT FOUND ASSUME THAT IT
     (: (SETMVB (COMBINATION? (ROOT (NB MVB))               ;IS OPTIONAL AND WE ARE DEALING WITH THE CASE
                  (WORD POSITION-OF-PRT)))           ;WITHOUT THE PARTICLE
        NIL
        POPRT)
     (: (ISQ MVB TRANS) NIL FINDOBJ1)
     (CUT POSITION-OF-PRT)
     (: (PARSE NG OBJ OBJ1)                           ;PARSE UP ANY NOUN GROUP YOU FIND
        POPRT
        FINDOBJ1                               ;IF THERE ARE MORE OR LESS NP'S THAN EXPECTED,
        NIL)                               ;THEN DON'T PARSE ANYTHING BUT GO TO NPRT
     (CUT END)                               ;INSTEAD. SIMILARLY, IF ANYTHING FOLLOWS THE
     (SETR 'OBJ1 H C)                           ;DISPLACED PARTICLE THEN A GRAMMATICALLY BAD
     (PARSE PRT)                               ;FORM IS ASSUMED AND THE PIECES POPED OFF
     (FQ PRT DPRT)
     (GO FINDOBJ2)
    POPRT(POPTO VG)
     (GO FINDOBJ1)
;;;--------------------------  CHECK THE VERB FOR THE PASSIVE CONSTRUCTION
    CHECKPASV
     (: (AND (ISQ H PASV)
         (FQ PASV)
         (SETR 'OBJ1 (GETR 'SUBJECT C) C))
        FINDOBJ2
        NIL
        FINDFAKE2)
     (FQ ACTV)                               ;NOT PASV=ACTIVE
     (GO FINDOBJ1)

     ;;;
    BE     (FQ BE)
     (AND (PARSE NIL NOT) (FQ NEG))
     (PARSE ADV VBAD)
    FINDOBJ1
     (: (OR (CANPARSE 1. '(ADJG COMP) 'INT)
        (CANPARSE 1. '(NG COMP) 'INT))
        CHECKIT
        NIL
        ONT)
     (: (OR (CANPARSE 1. '(PREPG COMP) 'INT)
        (CANPARSE 1. '(CLAUSE RSNG ING) 'TRANS)
        (CANPARSE 1.
              '(CLAUSE RSNG REPORT)
              'TRANS)
        (CANPARSE 1. '(CLAUSE RSNG TO) 'TRANS)
        (CANPARSE 1. '(PREPG LOC) 'ITRNSL)
        (CANPARSE 1. '(ADV PLACE) 'ITRNSL))
        ONT
        NIL)
     (: (CANPARSE 1. '(NG) 'TRANS)
        FINDOBJ2
        NIL
        FINDFAKE2)
    FINDFAKE1
     (: (MOVE-PT C U (REL-NOT-FOUND)) OBJ1REL NIL)
     (: (AND (CANTAKE 1. '(PREPG LOC) 'ITRNSL)
         (MOVE-PT C U (QADJ))
         (ISQ (GETR 'QADJ PT) PLACE)
         (FQ ITRANSL))
        PUTLOBJ
        NIL)
     (: (CANPARSE 1. NIL 'ITRNS) ONT NIL)
    GOOF1(OR GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - FIRST OBJ))
     (GO FAIL)
    OBJ1REL
     (SETR 'OBJ1 (GETR 'RELHEAD PT) C)
     (REMOVE-F-PT 'REL-NOT-FOUND PT)
     (FQ OBJ1REL)
    FINDOBJ2
     (: (CANPARSE 2. '(CLAUSE RSNG TO) 'TRANS2)
        FIXSUBJECT
        NIL)
     (: (OR (CANPARSE 2. '(ADV PLACE) 'TRANSL)
        (CANPARSE 2. '(PREPG LOC) 'TRANSL))
        ONT
        NIL)
     (: (OR (CANPARSE 2. '(ADJG COMP) 'TRANSINT)
        (CANPARSE 2. '(NG COMP) 'TRANSINT))
        ONT
        NIL)
     (: (CANPARSE 2. '(NG) 'TRANS2) ONT NIL)
    FINDFAKE2
     (: (AND (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND)))
        OBJ2REL
        NIL)
     (: (AND (CANTAKE 2. '(PREPG LOC) 'TRANSL)
         (MOVE-PT C U (QADJ))
         (ISQ (GETR 'QADJ PT) PLACE)
         (FQ TRANSL))
        PUTLOBJ
        NIL)
    OBJ2TO
     (PARSE ADV VBAD)
     (: (COND ((AND (NEXTWORD? 'TO)
            (ISQ MVB TO2)
            (PARSE PREPG TO))                   ;THE SECOND-OBJECT THAT WE HAVE BEEN LOOKING FOR
           (SETR 'OBJ2 (GETR 'OBJ1 H) C)               ;MAY BE A PREPG AS IN "GIVE IT TO THE LIONS"
           (FQ TRANS2TO TRANS2))                   ;TAKES THE OBJECT OF THE PREPOSITION "TO" AND
          ((AND (CQ PREPQ)                       ;MAKES IT THE OBJ2 OF THE CLAUSE.
            (MOVE-PT H PV (QUEST))
            (EQ (WORD (MOVE-PTW FW)) 'TO)
            (RQ PREPQ)
            (FQ TRANS2TOQ TRANS2)
            (SETR 'OBJ2
                  (GETR 'OBJ1 PT)
                  C))))                       ;"TO WHOM DID YOU GIVE THE MEAT?"
        ONT
        NIL)
     (: (CANPARSE 2. NIL 'TRANS) ONT FAIL)
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
    CHECKIT                                   ;CHECK FOR THE POSSIBILITY THAT THE SUBJECT WAS
     (: (EQ (WORD (NB (GETR 'SUBJECT C))) 'IT)               ;A  DUMMY   FUNCTION WORD ( "IT" ), AS IN "IT
        NIL                                   ;WAS NICE TO SEE HIM."Q
        ONT)                               ;TO BE ADDED HERE:JOHN WAS EAGER/EASY TO PLEASE
     (: (OR (AND (NEXTWORD? 'TO)
             (PARSE CLAUSE RSNG TO SUBJ))
        (AND (NQ ING) (PARSE CLAUSE RSNG ING SUBJ))
        (PARSE CLAUSE REPORT))
        NIL
        ONT)
     (FQ IT)
     (SETR 'LOGICAL-SUBJECT H C)                       ;THE CLAUSE IS THE REAL SUBJECT.
     (GO ONT)
    GOOF2(OR GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - SECOND OBJECT))
     (GO FAIL)

     ;;;
     ;;;***************************************************************************************************
     ;;;
     ;;;                               INITIAL SEMANTIC PROCESSING
     ;;;
     ;;;*****************************************************************************************************
    ONT     (: (CQ PASV) PONT NIL)
    ONT1 (: (CALLSM (SMCL1)) NIL (SMCL1))

     ;;;
     (: (NOT (CQ REL-NOT-FOUND)) TONT NIL RETSM)               ;IF THE FEATURE "REL-NOT-FOUND" IS PRESENT AT
                                       ;THIS POINT, IT INDICATES THAT WE ARE IN A
     (: (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1)               ;RELATIVE CLAUSE AND MAY HAVE TO DO SOME
        NIL                                   ;GYMNASTICS IF THE CLAUSE IS NOT TO FAIL
        PREPSHORT)                               ;MOVE BACK TO A QUESTION-NOUNGROUP, THEN DOWN
    TIMEQ(RQ REL-NOT-FOUND)                           ;AND BACK TO THE NOUN. IF THAT NOUN IS "TIM1"
     (FQ TIMEQ)                               ;THEN ASSUME WE HAVE FOUND OUR RELATIVE ELEMENT.
     (GO TONT)

     ;;;
    PREPSHORT
     (: (AND (NQ PREP) (PARSE PREPG)) NIL (ONT-SHORT-PREP))
     (: (CALLSM (SMRELATE H)) NIL (ONT: SMRELATE PREPQ))
     (: (CQ REL-NOT-FOUND) PREPSHORT TONT (ONT-NOT-FOUND))           ; WE HAVE A PREP TO TAKE THE UNATTACHED RELATIVE
                                       ;AS ITS OBJECT. THE FEATURE REL-NOT-FOUND WILL
                                       ;BE REMOVED IF THE PREPG DISCOVERS IT CAN'T FIND
    PONT (AND (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))           ;AN OBJECT (THE REMOVING WILL BE DONE WHILE IN
     (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)               ;PREPG). "LOGICAL" IE. SUBJECT IN RELATIONSHIP
     (GO ONT1)                               ;TO THE PROPER SEMANTIC INTERPRETATION OF THE
                                       ;MAIN VERB. AGENT-PREPG CAN BE PARSED (REFLECTS
                                       ;THE OPTIONALITY OF THE CONSTRUCTION)

     ;;;************************************************************************************
     ;;;                    CHECK FOR ADDITIONAL MODIFYING PHRASES
     ;;;************************************************************************************
    TONT (: (SETQ POSITION-OF-PTW N) NIL RETSM RETSM)               ;WE ARE USING THE SAME TECHNIQUE HERE AS WITH
                                       ;THE INITIAL MODIFIERS. IE. LOOP THROUGH THE
                                       ;POSSIBILITIES UNTILL YOU MAKE A PASS THAT ADDS
                                       ;NOTHING NEW.

     ;;;************************************* PREPG
    NPASV(: (AND (NQ PREP) (PARSE PREPG) (CALLSM (SMRELATE H)))
        NIL
        NIL
        RETSM)

     ;;;********************************** TIMW
     (: (AND (NQ TIMW)
         (PARSE ADV TIMW)
         (OR (CALLSM (SMTIME)) (GO FAIL)))
        NIL
        NIL
        RETSM)

     ;;;************************************* ADV
     (: (AND (NOT (CQ BE))
         (PARSE ADJG ADV)
         (OR (CALLSM (SMRELATE H)) (GO FAIL)))
        NIL
        NIL
        RETSM)

     ;;;************************************** TIME NOUN GROUP
     (: (AND (PARSE NG TIME) (OR (CALLSM (SMTIME)) (GO FAIL)))
        NIL
        NIL
        RETSM)

     ;;;************************************* PLACE
     (: (AND (NQ PLACE)
         (PARSE ADV PLACE)
         (OR (CALLSM (SMPLACE)) (GO FAIL)))
        NIL
        NIL
        RETSM)

     ;;;************************************ BINDER
     (: (AND (NQ BINDER)
         (PARSE CLAUSE BOUND)
         (OR (CALLSM (SMBIND)) (GO FAIL)))
        NIL
        NIL
        RETSM)

     ;;;************************************** TO CLAUSE (ADJUNCT)
     (: (AND (NEXTWORD? 'TO)
         (PARSE CLAUSE TO ADJUNCT)
         (OR (CALLSM (SMTOADJ)) (GO FAIL)))
        NIL
        NIL
        RETSM)

     ;;;
     (: (EQ N POSITION-OF-PTW) NIL TONT RETSM)               ;LOOP UNTILL NOTHING ELSE CAN BE PARSED
     (: (OR (NOT (CQ TOPLEVEL)) (NQ SPECIAL)) RETSM NIL)           ;SPECIAL WORD (E.G. COMMA AND) COULD INDICATE A
     (ERT CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL)               ;CONJUNCTION OR A BINDER
     (GO FAIL)

     ;;;****************************************************************************************
     ;;;                                   THERE
     ;;;
     ;;;             AS IN:  "THERE IS A BIRD SITTING ON YOUR SHOULDER"
     ;;;
     ;;;****************************************************************************************
    THERE(FQ THERE)
     (CUT END)
     (: (PARSE ADV TIMW) NIL NIL (THERE))                   ; "THERE IS A BIRD.."
     (: (AND (PARSE VG) (ISQ MVB BE)) THEF NOTHE (THERE))

     ;;;
    THERQ(: (ISQ (MOVE-PT H PV (QAUX)) BE) THERQ2 NIL)               ;IF THIS FAILS, THE THERE IS CONSIDERED TO BE
     (: (AND (NQ TIMW) (PARSE ADV TIMW)) NIL NIL (THEREQ))
     (: (AND (PARSE VG) (ISQ MVB BE)) THERQ2 NIL)
     (RQ POLR2)
     (GO NOTHE)
    THERQ2
(FQ SUBJTQ) (FQ THERE) ;
;THIS MAY NOT INTERFACE PROPERLY
;WITH THE SEMANTIC ROUTINES FOR BE
     (: (CQ POLAR) THEF ONT)

     ;;;
    THEF (: (AND (NQ ADV) (PARSE ADV TIMW)) NIL NIL (THEF))
     (: (PARSE NG SUBJ SUBJT) NIL THERREL)
(FQ THERE)
     (SETR 'SUBJECT H C)
     (GO ONT)

     ;;;
    THERREL
     (: (MOVE-PT C U (REL-NOT-FOUND)) NIL NOTHE)
     (SETR 'SUBJECT (GETR 'RELHEAD PT) C)
     (REMOVE-F-PT 'REL-NOT-FOUND PT)
     (GO ONT)
    NOTHE(RQ THERE)
     (POP THERE)
     (AND (NQ ADV) (PARSE ADV PLACE))
     (GO THER2)

     ;;;************************************************************************************************
     ;;;
     ;;;                                       IMPERATIVES
     ;;;
     ;;;************************************************************************************************
     ;;;
    IMPER(: (PARSE NG TIME) NIL NIL IMPOP)                   ;MODIFIERS WHICH MAY PRECEED THE VERB
     (: (AND (NQ ADV) (PARSE ADJG ADV VBAD)) NIL NIL IMPOP)
     (: (AND (NQ ADV) (PARSE ADV TIMW)) NIL NIL IMPOP)

     ;;;
    IMPE (: (PARSE VG IMPER) NIL IMPOP)
     (FQ IMPER)
     (GO VG1)

     ;;;
    IMPOP(: (POP NIL) IMPE (IMPOP))

     ;;;***************************************************************************************
     ;;;
     ;;;                                                 QUESTIONS
     ;;;
     ;;;***************************************************************************************
    QUEST(FQ QUEST)

     ;;;***************************** PREP QUESTION
     (: (NQ PREP) NIL NGQUES)
     (: (PARSE PREPG) NIL NGQUES (PREPQ-INCOMPLETE))           ;"ON WHICH BLOCK DID YOU PUT IT?"
     (: (ISQ H QUEST) NIL QUEST)                       ;IF THE PREPG ISN'T THE QUESTION, TRY AGAIN "ON
     (SETR 'QADJ H C)                           ;THAT DAY, WHERE DID YOU GO?" -- MAYBE WE COULD
                                       ;MAKE USE OF THE COMMA CLUE. PREPQ IS HANDLED
     (GO POLAR)                               ;MUCH LIKE QADJS LIKE WHEN AND WHERE THE REST OF
                                       ;THE QUESTION HAS THE SAME SYNTAX AS A POLAR
                                       ;(YES-NO).

     ;;;***************************** NOUN GROUP QUESTION
    NGQUES
     (: (PARSE NG QUEST) NGQST NIL)                       ;"WHICH ONE IS THE MURDURER?"
     (: (OR (AND (NEXTWORD? 'HOW)
             (PARSE ADJG QUEST)
             (SETR 'RELHEAD H C))                   ;"HOW BIG...."
        (AND (NQ QADJ)
             (PARSE QADJ)
             (FQ QADJ)
             (SETR 'QADJ H C)))                       ;"WHAT...?",  "WHERE...?"
        POLAR
        POLAR
        NIL)
     (FQ SHORTQUES)
     (CALLSM (SMADJQSHORT))                           ;IF ALL THE SENTENCE CONSISTS OF IS THE QUESTION
    ADJQS(GO RETURN)                               ;ADJECTIVE THEN WE SHOULD RETURN DIRECTLY

     ;;;
    NGQST(SETR 'RELHEAD H C)
    NGQST2
     (CUT END)
     (SETR 'SUBJECT H C)
     (AND (NQ ADV) (PARSE ADJG ADV VBAD))

     ;;WE HAVE HERE A VERY INTERESTING SITUATION INVOLVING A
     ;;TEMPORARY AMBIGUITY IN THE INTERPRETATION OF CERTAIN VERB
     ;;WHICH IS ELIMINATED WHEN MORE CONSTITUENTS OF THE CLAUSE
     ;;HAVE BEEN PARSED.  CONSIDER SENTENCES LIKE:
     ;;;        WHICH BOX CONTAINS A RED BLOCK?
     ;;;        WHICH HAND HAS THE M&M'S?
     ;;;        WHICH HAND HAS HE BEEN HAVING TROUBLE WITH?
     ;;A THIS POINT IN THE CLAUSE PROGRAM WE HAVE PARSED THE
     ;;FIRST NG AND ARE ABOUT TO PARSE THE VERB GROUP.  IN THE
     ;;FIRST SENTENCE WE WILL NEVER HAVE ANY PROBLEM BECAUSE THE
     ;;VERB IS MARKED WITH THE FEATURE "NAUX" MEANING THAT IT CAN
     ;;NEVER SERVE A AN AUXILLIARY TO ANOTHER VERB.  HOWEVER IS
     ;;THE VERB WE SEE AT THIS POINT IN THE PARSEING IS A FORM OF
     ;;"HAVE" OR "BE" WE CANNOT YET DETERMINE WHETHER IT IS IN A
     ;;CONSTRUCTION SUCH AS THE SECOND OR THE THIRD SENTENCE.  -
     ;;IS IT THE MAIN VERB OF THE CLAUSE OR ONLY AN AUX TO A VERB
     ;;WHICH WE HAVEN'T PARSED YET??? WHAT WE DO IT MAKE A
     ;;TENTATIVE DECISION ONE WAY OR THE OTHER AND THEN MAKE
     ;;ARRANGEMENTS TO CHANGE THINGS LATER IF WE FIND WE WERE
     ;;WRONG.  ONCE WE HAVE PARSED THE NEXT NOUN GROUP WE CAN
     ;;MAKE THE FINAL DECISION.  OUR TENTATIVE CHOICE IS TO CALL
     ;;THE VERB WE JUST PARSED THE MAIN VERB OF THE SENTENCE.
     ;;THEN WE KNOW THAT IF ANOTHER VERB FOLLOWS THE NEXT NG WHEN
     ;;WE SHOULDN'T EXPECT ONE THAT WE HAVE MADE THE WRONG CHOICE
     ;;AND SHOULD REARRANGE OUR ANALYSIS
     (COND ((PARSE VG NAUX) (FQ SUBJQ) (GO VG1))
           ((NQ VB) (FQ REL-NOT-FOUND) (GO POLAR))
           (T (MOVE-PTW N PW)
          (POP NG QUEST)
          (CUT PTW)
          (GO NGQUES)))                           ;POP BACK AND START FIGURING OUT THE QUESTION
    QUEST2                                   ;ALL OVER AGAIN
     (: (AND (NEXTWORD? 'THERE) (PARSE NIL THERE))
        THERQ
        SUBF)                               ;"ARE THERE....?"

     ;;;
    SUBF (: (PARSE NG SUBJ)                           ;PARSE THE SUBJECT OF ANYTHING LIKE: "DID THE
                                       ;WOMAN GET THE JOB?" IF SUCCESSFUL, CONTINUE AT
                                       ;"SUBREG" IN THE NORMAL PART OF THE CLAUSE
                                       ;PROGRAM (RESETTING THE SUBJECT REGISTER)  (THE
        SUBREG                               ;BEGINNING OF THE VERB GROUP SECTION). "SUBJ1"
        NIL                                   ;WORRIES ABOUT WHAT SHOULD HAPPEN IF THE SUBJECT
        SUBJ1)                               ;SEEMS TO FINISH THE SENTENCE
     (RQ REL-NOT-FOUND)
     (GO BE)

     ;;;************* POLAR
    POLAR(: (AND (NQ VB)
         (PARSE VB AUX (QAUX))
         (SETR 'QAUX H C)
         (CALLSM (SMVAUX))
         (SETMVB H))
        NIL
        QCHOP)
     (OR (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
     (FQ POLR2)
     (GO QUEST2)

     ;;;
    QCHOP(ERT CLAUSE: QCHOP)
     (: (POPTO CLAUSE BOUND) BICUT (QCHOP))

     ;;;********************************************************************************************
     ;;;
     ;;;                                              SECONDARY CLAUSES
     ;;;
     ;;;********************************************************************************************
     ;;;
     ;; SECONDARY CLAUSES ARE PRINCABLY THOSE THAT ARE NOT MAJOR.
     ;;THIS INCLUDES ADJUNCTS, RANK-SHIFTED-NOUN-GROUP'S,
     ;;RANK-SHIFTED-QUALIFIERS, FOR-TO CLAUSES AND OTHERS.;;; IF
     ;;THE CLAUSE IS MARKED "RSQ", THEN IT AUTOMATICALLY WILL
     ;;HAVE SEVERAL SPECIAL REGISTERS ASSOCIATED WITH IT TO
     ;;FACILITATE SEMANTIC PROCESSING.  'RELWORD WILL POINT TO
     ;;THE INITIAL RELATIVE PRONOUN IF THERE IS ONE (THAT, WHICH,
     ;;WHO...).  ALSO "REL-NOT-FOUND" IS A TEMPORARY FEATURE
     ;;WHICH IS RELIVANT DURING THE PROCESSING OF A CLAUSE, IT
     ;;INDICATES THAT THE ELEMENT OF THE CLAUSE WHICH WAS TAKEN
     ;;OVER INTO THE RELATIVE WORD (SUBJ, OBJ1...) HAS NOT YET
     ;;BEEN DETERMINED.  'RELHEAD IS A REGISTER WHICH POINTS TO
     ;;WHATEVER THE CLAUSE MODIFIES.  IN THE CASE OF AN RSQ IT IS
     ;;SET INITIALLY TO "(GETR 'HEAD (MOVE-PT U))" WHICH IS THE
     ;;HEAD NOUN OF THE NP WITHIN WHICH THE RSQ IS BEING
     ;;PROCESSED
     ;;;
     ;;;
     ;;;
     ;;;
    SEC     (COND ((CQ BOUND) (GO BOUND))                       ;CHECK INITIAL FEATURES AND JUMP ACCORDINGLY
           ((CQ TO) (GO TO))
           ((CQ RSQ) (GO RSQ))
           ((CQ REPORT) (GO REPORT))
           ((CQ ING) (GO ING))
           (T (MQ RSNG-TYPE) (GO FAIL)))

     ;;;
     ;;;
     ;;; --------------- BINDER  ---------------
    BOUND(: (PARSE BINDER) NIL (BOUND) (BINDER))
     (SETQ LOCATIONMARKER N)                       ; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
     (GO FDEC)                               ;"FDEC" IS NEAR THE TOP OF THE MAJOR CLAUSE

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;; --------------- RSQ  ---------------
    RSQ     (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
     (: (CQ PREPREL) NIL RSQ2)
     (PARSE PREPG PRONREL)                           ;THIS CALL IS BASED ON INFORMATION PASSED FROM
     (SETR 'QADJ H c)                               ;FAR AWAY AND EXPLAINED IN DETAIL IN THE CODE
     (GO REPORT)                               ;FOR PREPOSITION GROUPS

     ;;;
    RSQ2 (COND ((PARSE VG EN PASV)                       ;HAVING DETERMINED THAT THE VERB IS PASSIVE IF
        (OR (ISQ MVB TRANS) (GO FAIL))                   ;IT WERE NOT ALSO TRANSITIVE, THEN WE WOULDN'T
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)               ;KNOW WHAT TO DO WITH WHATEVER WAS PARSED AS A
        (GO VG1))                           ;SUBJECT - SO WE FAIL
           ((PARSE VG ING)
        (SETR 'SUBJECT (GETR 'RELHEAD C) C)
        (GO VG1))
           ((NQ PRONREL) (PARSE NG RELWD) (GO REL))
           ((CQ COMPONENT)                           ; IN A COMPONENT RELATIVE THE RELWD MIGHT BE IN
        (SETR 'RELHEAD                           ;THE FIRST CLAUSE.
              (GETR 'RELHEAD (MOVE-PT C PC))
              C)                           ; MAKE RELHEAD SAME AS PREVIOUS COMPONENT RSQ.
        (GO REL))
           ((PARSE NG SUBJ) (FQ REL-NOT-FOUND) (GO SUBREG))
           (T (GO FAIL)))                           ;THIS REALLY ISN'T AN RSQ

     ;;;
    REL     (SETR 'SUBJECT (GETR 'RELHEAD C) C)
     (: (PARSE VG) VG1 NIL)                           ;OUR FIRST HYPOTHESIS, THAT THE SUBJECT WAS THE
                                       ;RELWORD, WAS JUST PROVEN WRONG SINCE WE CANNOT
                                       ;PARSE THE VG NEXT. SO WE REVISE OUR FEATURES
     (FQ REL-NOT-FOUND)                           ;AND JUMP TO PARSE A REAL FULL SUBJECT AS IN
     (GO SUBJ)                               ;"...WHICH MARY THOUGHT WAS CHAUVANISTIC" AS
                                       ;OPPOSED TO "...WHICH WAS CHAUVANISTIC"

     ;;; --------------- TO  ---------------
    TO     (: (AND (CQ COMPONENT) (PARSE VG TO TODEL)) VG1 NIL)           ;"I WANTED TO DANCE AND SING"
     (: (NEXTWORD? 'FOR) NIL TO1)                       ;THIS IS EXPERIMENTAL
     (PARSE NIL FOR)                           ;PLEASE CHECK OUT ANY FOR-CLAUSES YOU CAN THINK
     (FQ FOR)                               ;OF
     (PARSE NG SUBJ TOSUBJ)
     (SETR 'SUBJECT H C)
    TO1     (: (PARSE VG TO) VG1 (TO))

     ;;;
     ;;;
     ;;;
     ;;; --------------- ING  ---------------
    ING     (: (MOVE-PTW N NW (ING)) NIL FAIL)
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

     ;;; --------------- REPORT ---------------
    REPORT
     (AND (NEXTWORD? 'THAT) (PARSE NIL THAT) (FQ THAT))
     (SETQ LOCATIONMARKER N)                       ; DO THIS TO ACT LIKE MAJOR DECLARATIVE CLAUSE
     (GO FDEC)

     ;;;******************************************************************
     ;;;                                RETURN
     ;;;***********************************************************************
    RETSM(OR (CALLSM (SMCL2)) (GO FAIL))
     (GO RETURN))

(PDEFINE NG
     NIL
    ENTERING-NG

     ;;;
     ;;;
     ;;;
    NGSTART                                   ;EXAMINE INITIAL FEATURES AND JUMP TO
     (COND ((CQ RELWD) (GO RELWD))                       ;CORRESPONDING SPECIAL BLOCKS OF CODE
           ((CQ QUEST) (GO QUEST))
           ((OR (NQ QDET) (NQ QPRON)) (FQ QUEST) (GO QUEST))
           ((CQ TIME) (GO TIME))                       ;LOOK AT FIRST WORD
           ((NQ PROPN) (GO PROPN))
           ((NQ TPRON) (GO TPRON))
           ((NQ EVERPRON) (GO EVERPRON))
           ((NQ PRON) (GO PRON)))

     ;;;
     ;;;
     ;;;
     ;;;
    LOOK (COND ((NQ DET) (GO DET))                       ;THIS POINT MAY BE JUMPED BACK TO
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

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;; IF YOU CAN PARSE ANY OF THESE SMALL THINGS, YOU'RE DONE
     ;;;--------------------------------------------------
    START                                   ;PARSE A PROPER NOUN
    PROPN(PARSE PROPN)
     (FQ DEF PROPNG)
     (: (ISQ H POSS) PROPS NIL)
     (: (AND NN (NQ PROPN)) PROPN NIL)
    PROPS(OR (CALLSM (SMPROP)) (GO FAIL))                   ;EXAMINE ITS SEMANTICS
     (: (ISQ H POSS) POSS PRAG)

     ;;;
     ;;;
     ;;;--------------- PRONOUNS ---------------
     ;;;
    PRON (: (PARSE PRON POSS) POSS NIL RED2)                   ;IS IT POSSESSIVE?
    PRON2(: (CQ NPRON) (NPRON) NIL)
     (: (OR (AND (CQ SUBJ) (PARSE PRON SUBJ))               ;CHECK SUBJECTIVE OR OBJECTIVE
        (AND (OR (CQ OBJ) (CQ TOSUBJ) (CQ INGSUBJ))
             (PARSE PRON OBJ))                       ;CASE....
        (CQ INGSUBJ))
        NIL
        (PRON))
     (FQ PRONG DEF)
    PRON3(: (CALLSM (SMPRON H)) NIL FAIL)                   ;EXAMINE SEMANTICS OF PN
    PRAG (SETR 'HEAD H C)
     (MOVE-PT H)
     (TRNSF NS NPL NFS NEG)                           ;MODIFY PN FEATURES TO CORRECT
     (GO RETURN)                               ;NUMBER...

     ;;;
     ;;;
     ;;;---------------  ...ANYTHING, SOMETHING, ...  --------------
    TPRON(PARSE TPRON)
     (FQ TPRON)
     (MOVE-PT H)
     (TRNSF NS NPL ANY NEG)
     (SETR 'HEAD C H)
     (AND NN (NQ ADJ) (PARSE ADJ))
     (GO SMNG)

     ;;;
     ;;; ----- WHATEVER, WHENEVER, WHEVER....
     ;;;
     ;;;
    EVERPRON
     (: (AND (PARSE PRON EVERPRON) (CALLSM (SMPRON H))) NIL FAIL)
     (: (AND (PARSE CLAUSE RSQ NOREL) (CALLSM (SMRELATE H)))
        RETSM
        FAIL)

     ;;;
     ;;;
     ;;;
     ;;;--------------- AS ---------------
    AS     (: (AND (PARSE NIL AS) (PARSE NUMD NUMDAS) NN (PARSE NIL AS))
        NUMD2
        (AS)
        (AS))

     ;;;
     ;;;
     ;;;--------------- AT + NUM ---------------
    AT     (: (AND (PARSE NIL AT) (PARSE NUMD NUMDAT)) NIL (AT) (AT))
    NUMD2(: (AND (PARSE NUM) (FQ NUM NUMD)) DET1 (NUMD2) INCOM)

     ;;;
     ;;;--------------- OTHER NUMBER WORDS ---------------
    NUMD (: (PARSE NUMD NUMDAN) NIL ND3 INCOM)
     (: (PARSE NIL THAN) NUMD2 INCOM POPCOM)
    ND3     (: (PARSE NUMD NUMDALONE) NUMD2 (NUMD) (NUMD))

     ;;;
     ;;;--------------- TIME WORDS ---------------
    TIME (: (AND (NQ TIME) (PARSE NOUN TIME)) RETSM NIL)
     (: (MOVE-PTW N NW (TIM1)) LOOK (TIME))
    TIMORD
     (: (PARSE ORD TIMORD) NIL FAIL)
     (: (AND (PARSE NOUN TIM1) (FQ DET DEF) (CALLSM (SMNGTIME)))
        RETURN
        FAIL)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;; THE MAINSTREAM......  -MORE CMPLICATED NG TYPES
     ;;;--------------------------------------------------
     ;;;
     ;;;--------------- PARSE A DETERMINER ---------------
    DET     (PARSE DET)
     (FQ DET)
     (MOVE-PT H)                               ;SHIFT PTR TO THE DETERMINER
     (: (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR)
        IND
        (BUG)
        INCOM)

     ;;;
     ;;;
     ;;;--------------- INDETERMINATE ---------------
    IND     (: (AND (EQ (WORD (NB H)) 'ALL)
         (EQ (WORD N) 'THE)
         (PARSE DET)
         (FQ DEF))
        NUM
        NIL
        (THE))
     (: (AND (ISQ H QNTFR) (FQ QNTFR)) QNUM NIL)

     ;;;
     ;;;
     ;;;--------------- ORDINALS AND NUMBERS ---------------
    ORD     (: (AND (PARSE ORD) (FQ ORD)) NIL NUM INCOM)
     (: (AND (NEXTWORD? 'OF)                       ;TWELTH OF OCTOBER...
         (ISQ (MOVE-PTW N NW) MONTH)
         (PARSE NIL OF)
         (PARSE NOUN MONTH)
         (FQ DATE))                           ;REMEMBER THAT FEATURES ARE DESIGNED AS AIDS TO
        RETSM                               ;SEMANTIC COMPREHENSION AS WELL AS SYNTACTIC
        NIL)                               ;PARSING.
     (: (CQ DEF) NIL ADJ)
    NUM     (: (PARSE NUM) NIL ADJ)                       ;LARGE JUMP IF FALSE
     (FQ NUM)
     (: (CQ DET) NIL DET1)
     (: (COND ((AND (ISQ H NS) (CQ NS)) (RQ NPL PART))
          ((CQ NPL) (RQ NS PART)))
        ADJ
        (NUM)
        INCOM)
    DET1 (COND ((ISQ H NS) (FQ NS)) (T (FQ NPL)))               ;EXPLICIT CHECK FOR THE VALUE 1
     (OR NN (AND (FQ NUMBER) (GO INCOM)))
    NUMBER
     (FQ DET)
     (: (NQ OF) OF ADJ)
    QNUM (: (ISQ H NONUM) OF NIL)
     (: (AND (PARSE NUM) (FQ NUM)) NIL OF)
     (: (COND ((EQ (SM H) 1.) (AND (CQ NS) (RQ NPL)))           ;EXPLICIT CHECT FOR THE VALUE 1
          ((CQ NPL) (RQ NS)))
        NIL
        (NUMD)
        INCOM)

     ;;;
     ;;;
     (: (EQ (WORD (NB H)) 'NO) ADJ NIL)                   ;CHECKS FOR WORD "NO"

     ;;;
     ;;;
     ;;;--------------- PREPG WITH "OF" ---------------
    OF     (: (AND (NQ OF) (PARSE PREPG OF)) SMOF NONE)               ;"FIVE OF THE BLOCKS"
    SMOF (FQ OF)
     (: (OR (CALLSM (SMNGOF)) (NOT (POP))) RETSM INCOM)

     ;;;
     ;;;
    NONE (: (EQ (WORD (NB H)) 'NONE) INCOM ADJ)

     ;;;
     ;;;
     ;;;
     ;;;-------------PARSE ALL THE ADJECTIVES ---------
    ADJ     (: (PARSE ADJ) NIL EPR INCOM)
     (AND (ISQ H COMPAR)
          (FQ COMPARATIVE-MODIFIER)
          (SETR 'COMPARATIVE-MODIFIER H C))
     (GO ADJ)
    EPR     (: (OR (ISQ H SUP) (ISQ H COMPAR)) NIL CLASF INCOM)           ;WE PARSED AN ADJ AND RAN OUT OF WORDS
     (FQ ADJ)
     (AND (NEXTWORD? 'OF)
          (PARSE PREPG OF)
          (OR (CALLSM (SMNGOF)) (GO FAIL))
          (FQ OF)
          (GO RETSM))

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;---------------PARSE ALL THE CLASIFIERS ---------------
     ;;;
    CLASF(: (OR (PARSE VB ING (CLASF))                       ;TRIES TO PARSE THE LARGEST POSSIBLE NG FIRST
        (PARSE VB EN (CLASF))
        (PARSE CLASF))
        CLASF
        NIL
        REDUC)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- AND FINALLY...... THE NOUN ---------------
    NOUN (: (PARSE NOUN) NIL RED2)

     ;;;
     (: (AND (CQ TIME) (NOT (ISQ H TIM1))) RED1 NIL)

     ;;;
     ;;;--------------- MODIFY FEATURES FOR NUMBER AND SUCH --------------
     (SETQ T1 FE)
     (COND ((AND (ISQ H MASS) (OR (CQ PART) (NOT (CQ DET))))
        (FQ MASS)))
     (COND ((NOT (ISQ H NPL)) (RQ NPL PART)))
     (COND ((NOT (ISQ H NS)) (RQ NS)))
     (COND ((AND (NOT (CQ DET)) (NOT (CQ NUMD)))
        (MOVE-PT H)
        (TRNSF NPL MASS)))
     (: (MEET FE '(NS NPL PART MASS)) NIL RED0)

     ;;;*******************  "...A BIGGER BLOCK THAN...."
     (: (NEXTWORD? 'THAN) NIL SMNG)
     (FQ THAN)                               ;THE PRESENCE OF THIS FEATURE IS NOTED BELOW AND
                                       ;IN ADJG

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;; AT THIS POINT SMNG1 IS CALLED FOR PRELIMINARY CHECKS AND
     ;;ANALYSIS BEFORE CHECKING QUALIFIERS
     ;;;--------------------------------------------------
    SMNG

     ;;;
     (SETR 'HEAD H C)                           ;SET HEAD REGISTER TO THE NOUN

     ;;;
     ;;;
     (: (AND (CQ OBOFJ) (NOT (CQ DEF))) FAIL NIL)               ;JUST PARSED
     (OR (CALLSM (SMNG1)) (GO FAIL))
     (: (NOT (ISQ H POSS)) NIL POSS RETSM)                   ;CHECK FOR POSSIVE

     ;;;
     ;;;--------------------------------------------------
     ;; POSSIBLE QUALIFIERS
     ;;;--------------------------------------------------
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;***********"....A BIGGER BLOCK THAN..."
     (: (AND (CQ THAN) (PARSE ADJG)) NIL RSQ-TO)
     (: (CALLSM (SMRELATE H)) RETSM FAIL)

     ;;;
     ;;;
     ;;;--------------- RSQ TO ---------------
    RSQ-TO
     (: (AND (NEXTWORD? 'TO)
         (MEET FE '(COMP SUBJ))
         (PARSE CLAUSE RSQ TO)
         (OR (CALLSM (SMRELATE H)) (GO POPRET)))
        RETSM
        NIL)

     ;;;
     ;;;
     ;;;--------------- AS OR COMPARATIVE ---------------
     (: (AND (OR (NEXTWORD? 'AS) (NQ COMPAR))
         (PARSE ADJG THANNEED))
        NIL
        PREPNG)                               ;WHAT IS THE REASON FOR THE EXISTANCE OF THIS
     (AND (NULL N)                               ;STRANGE ANIMAL (ALSO THE ONEBELOW) -- CHECK
          (CQ SUBJ)                               ;THEM OVER AND HACK THEM PROPERLY
          (ISQ (MOVE-PT C PV) AUX)
          (ISQ PT BE)
          (GO POPRET))                           ;AVOIDS ATTACHING MODIFIER WHEN IT GOBBLES TO
     (: (CALLSM (SMRELATE H)) RSQ-TO POPRET RETSM)               ;MUCH E.G. IS THE BLOCK ON THE TABLE? DOESN'T
                                       ;WNAT "THE BLOCK ON THE TABLE" AS A CONSTITUENT.
                                       ;I ADMIT ITS A HACK.

     ;;;
     ;;;
     ;;;--------------- ANY SORT OR PREPOSITION GROUP --------------
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

     ;;;
    DISGRSQ

     ;; CHECK FOR DISGUISED RSQ CLAUSES BY READING THE FAILURE
     ;;MESSAGES SENT UP FROM PREPG.
     (: (EQ (CAR MES) 'PREP-WHICH) NIL RSQ)
     (SETQ MES (CDR MES))
     (: (PARSE CLAUSE RSQ PREPREL) PREPNG (RSQ-PREPREL) RETSM)

     ;;;
     ;;;
     ;;;--------------- ANY OTHER RSQ ---------------
     ;;;
    RSQ     (: (AND (ISQ (MOVE-PT C U) POLR2)
         (CQ SUBJ)
         (NQ VB)
         (NOT (CQ SUBJT))
         (NOT (ISQ PT QADJ)))
        RETSM
        NIL)
     (: (PARSE CLAUSE RSQ) NIL RETSM)
     (: (CALLSM (SMRELATE H)) RETSM POPRET)

     ;;;
     ;;;--------------------------------------------------
     ;; THE ENTIRE NG SHOULD HAVE BEEN PROCESSED BY THIS POINT
     ;;;--------------------------------------------------
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;; IF AT FIRST YOU DON'T SUCEED.......
     ;;;--------------------------------------------------
    RED0 (SETQ FE T1)
    RED1 (POP)
    RED2 (COND ((NULL H) (MQ NO) (GO FAIL))
           ((ISQ H NUMBER) (GO INCOM))
           ((AND (ISQ H POSS)
             (OR (ISQ H PRON)
             (AND (MOVE-PT H DLC) (ISQ PT PRON))))
        (POP)
        (GO PRON2))
           ((AND (NULL (CDR H)) (CQ DEFPOSS)) (GO POSSDEF))
           ((AND (CQ QUEST) (NULL (CDR H))) (GO QDETCHECK))           ;(CDR H) = T IF THERE IS ONLY ONE DAUGHTER TO
           ((ISQ H ADJ) (GO EPR))                       ;THE CURRENT NODE
           ((NOT (ISQ H CLASF)) (GO INCOM)))
    REDUC(POP)
     (: (AND (NULL H) (NQ PROPN)) PROPN NOUN)

     ;;;
     ;;;
     ;;;
    POPCOM
     (POP)

     ;;;
     ;;;--------------- INCOMPLETE PHRASES ---------------
    INCOM(FQ INCOM)
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

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;; POSSESSIVE HANDLER
     ;;;--------------------------------------------------
    POSS (OR (CALLSM (SMNG2)) (GO FAIL))
    POSS2(: (CQ INGSUBJ) RETSM NIL)
     (SETQ H (BUILDNODE (REVERSE (CONS 'POSS               ;IF POSSESSIVE, ALL PREVIOUS MODIFIERS MODIFY
                       (SETDIF FE               ;THE POSSESSIVE NOUN, NOT THE NG HEAD
                           '(COMPONENT))))
                NB
                N
                H
                SM))
     (SETQ BACKREF (APPEND H (CDR BACKREF)))
     (: (SETR 'FEATURES
          (SETQ FE (APPEND '(POSES DET DEF NS NPL)
                   (REVERSE REST)))
          C)
        NIL
        (BUG))
     (: (OR (NOT NN) (ISQ H DEFPOSS)) NIL ORD)
possdef                            ;the placement of this tag is a
                            ;guess. The original is lost,
                            ;assuming that it ever existed
     (RQ POSES DET DEF)
     (FQ POSSDEF NS NPL)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- RELATIVES---------------
     ;;;
    QUEST(: (PARSE NIL HOW) NIL QDET FAIL)
     (: (PARSE NIL MANY) NIL FAIL INCOM)
     (FQ DET NPL INDEF HOWMANY)
     (GO OF)
    QDET (: (AND (PARSE DET QDET) (FQ DET NPL QDET NS))
        QNUM
        NIL
        INCOM)
    QPRON(: (PARSE PRON QPRON) PRON3 FAIL)

     ;;;
     ;;;
     ;;;
    RELWD(: (AND (PARSE PRONREL)
         (CALLSM (SMSET (SM (MOVE-PT C U U (NG))))))           ;SET SM TO THE NOUNGROUP DIRECTLY UPSTAIRS
        RETURN
        NIL)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;-
    POPRET
     (POP)

     ;;;
     ;;;--------------------------------------------------
     ;; RETURN AFTER CALLING SMNG2 TO PROCESS THE COMPLETED NOUN
     ;;GROUP
     ;;;--------------------------------------------------
    RETSM(OR (CALLSM (SMNG2)) (GO TRYA))
     (GO RETURN)

     ;;;
     ;;;
     ;;;
     ;;;--------------- YOU PROBABLY GOOFED, CUT AND TRY AGAIN. --------------
    TRYA (: (ISQ H NOUN) NIL (TRYA))
     (POP)
     (CUT N)
    UP     (: (POP) UP NIL)                           ;POP EVERYTHING OFF
     (SETQ FE (REVERSE REST))
     (SMSET NIL)
     (GO NGSTART))

(PDEFINE VG
     (TENSE)

     ;;;
     ;;;--------------------------------------------------
     ;; CHECK INITIAL FEATURES TO SEE IF SOME SPECIAL TYPE OF VG
     ;;IS WANTED
     ;;;--------------------------------------------------
     ;;;
    ENTERING-VG
     (COND ((CQ TO) (GO TO))
           ((CQ EN) (GO EN))
           ((CQ ING) (GO ING))
           ((CQ IMPER) (GO IMPER))
           ((ISQ (MOVE-PT C U) POLR2) (GO POLR2)))               ;CHECKS IF THE CLAUSE IS MARKED AS POLR2

     ;;;
     ;;;
     ;;;
     ;;;--------------- DISPATCH TABLE FOR EXAMINEING THE FIRST WORD ---------------
    NEW                                       ;PARSE THE FIRST WORD WITH APPROPRIATE FEATURES
     (COND ((NOT (NQ VB)) (MQ VB) (GO FAIL))               ;AND JUMP TO CODE THAT KNOWS WHAT SHOULD BE
           ((AND (NQ DO) (PARSE VB AUX DO)) (GO DO))           ;LOOKED FOR NEXT IN EACH CASE
           ((AND (NQ MODAL) (PARSE VB AUX MODAL)) (GO MODAL))
           ((AND (NQ WILL) (PARSE VB AUX WILL)) (GO WILL))
           ((AND (NQ BE) (PARSE VB AUX BE)) (GO BE))
           ((AND (NQ HAVE) (PARSE VB AUX HAVE)) (GO HAVE))
           ((NOT (PARSE VB (MVB))) (MQ VB) (GO FAIL)))

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- SIMPL ---------------
    SIMPLE
     (MOVE-PT C DLC)                           ;MOVE PT DOWN FROM THE CURRENT NODE BEING PARSED
     (TRNSF VPL INF V3PS)                           ;(VG) AND ACROSS TO THE MOST RECENTLY PARSED
     (SETQ TENSE (COND ((AND (ISQ PT PRESENT) (ISQ PT PAST))       ;DAUGHTER. IN THIS CASE THAT DAUGHTER WAS PARSED
                '(PAST-PRESENT))                   ;IN THE DISPATCH TABLE JUST ABOVE
               ((ISQ PT PAST) '(PAST))
               (T '(PRESENT))))
     (GO REV)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- TO ---------------
    TO     (FQ NAGR)                               ;"NAGR" MARKS THAT SUBJECT AND MAIN VERB NEED
     (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))           ;NOT AGREE IN NUMBER AND PERSON AND INSURES THAT
     (: (OR (PARSE NIL TO) (CQ TODEL)) NIL (TO) (TO))           ;THE AGREEMENT CHECKER AT THE END OF THE PROGRAM
                                       ;("REV") WILL NOT BE APPLIED "TODEL" MUST BE
     (SETQ TENSE '(INFINITIVE))                       ;GIVEN AS AN INITIAL FEATURE OR ELSE THIS
     (GO MODAL2)                               ;STATEMENT FAILS TENSE IS USED TO HOLD THE TENSE
                                       ;WHILE IT IS BEING COLLECTED.

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- EN ---------------
    EN     (FQ NAGR)
     (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))
     (SETQ TENSE '(PAST))
     (: (AND (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)) RETSM FAIL) ;DONE AT "EN2"

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- ING ---------------
    ING     (FQ NAGR)
     (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))
    INGADV
     (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) INGADV NIL)
     (SETQ TENSE '(PRESENT))
     (GO BE2)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- IMPER ---------------
    IMPER(: (AND (PARSE VB DO NEG INF) (FQ NEG)) NIL NIL (DONT))
     (: (AND (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG)))
        RETURN
        (IMPER))                               ;MVB IS BOUND BY CLAUSE

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- POLR2 ---------------
    POLR2                                   ;THE CLAUSE COULD ONLY BE MARKED AS "POLR2"
     (OR (SETQ PT (GETR 'QAUX (MOVE-PT C U)))               ;("DID THE...?") IF AN AUX OF SOME VERIETY HAD
         (AND (BUG VG:POLR2) (GO FAIL)))                   ;ALREADY BEEN PARSED, IF THAT IS NOT THE CASE,
     (SETQ H (LIST (CAR PT)))                       ;THEN WE HAVE A BUG IN THE PROGRAM SOMEWHERE SET
     (TRNSF NEG)                               ;THE INITIAL DAUGHTER OF THE VG TO BE THE
     (COND ((ISQ H DO) (GO DO))                       ;PREVIOUSLY PARSED AUX MARK THE VG AS NEG IF
           ((ISQ H MODAL) (GO MODAL))                   ;APPROPRIATE (SEE PROGMR FILE FOR THE OPPERATION
           ((ISQ H WILL) (GO WILL))                       ;OF THIS FUNCTION) DISPATCH TABLE , CHECKING THE
           ((ISQ H BE) (GO BE))                       ;AUX
           ((ISQ H HAVE) (GO HAVE)))
     (ERT BUG VG:POLR2VB)                           ;NOTHING BUT UNGRAMATICAL NONSENSE SHOULD REACH
     (GO FAIL)                               ;THIS POINT

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;; PROCESSING OF VB'S NOT SPECIALLY MARKED FOR BY INITIAL
     ;;FEATURES
     ;;;--------------------------------------------------
     ;;;
     ;;;
     ;;;--------------- DO ---------------
    DO     (FQ DO)
     (MOVE-PT C DLC)                           ;MOVE TO THE "DO"
     (TRNSF VPL NEG INF V3PS)                       ;ARRANGE ITS FEATURES
     (SETQ TENSE (COND ((ISQ PT PAST) '(PAST))
               (T '(PRESENT))))
     (GOCOND DO2 MVB)                           ;GO CONDITIONALY TO THE FIRST TAG IF MORE WORDS
                                       ;REMAIN BEFORE THE CUT POINT, AND TO THE SECOND
                                       ;TAG IF THERE ARE NONE

     ;;;
    DO2     (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))
    ADV2 (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV2 NIL (ADV))
     (: (PARSE VB (MVB) INF) NIL MVB)                   ;"MVB" ARRANGES FOR A CHECK TO INSURE THAT THE
     (GO REV)                               ;VERB BEING PARSED CAN BE A MAIN VERB

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- MODAL ---------------
    MODAL(FQ NAGR MODAL)
     (SETQ TENSE '(MODAL))
     (GOCOND MODAL2 INCOMP)
    MODAL2
     (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))
    ADV3 (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV3 NIL (ADV))

     ;;;
     (COND ((PARSE VB BE INF) (GOCOND BE2 MVB))               ;DISPATCH TABLE FOR THE NEXT VERB
           ((PARSE VB HAVE INF) (GOCOND HAV2 MVB))
           ((PARSE VB INF (MVB)) (GO REV))
           (T (GO INCOMP)))

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;---------------------WILL----------
    WILL (FQ NAGR)
     (SETQ TENSE '(FUTURE))
     (GOCOND MODAL2 INCOMP)                           ;THE SAME POSSIBILITIES FOR THE NEXT VERB APPLY
                                       ;AFTER BOTH WILL AND MODALS

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- BE ---------------
    BE     (MOVE-PT C DLC)                           ;POINT TO WHAT WAS JUST PARSED
     (TRNSF VPL INF V3PS VFS)
     (SETQ TENSE (COND ((ISQ PT PAST) '(PAST))
               (T '(PRESENT))))
     (GOCOND BE2 MVB)
    BE2     (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))
    ADV4 (: (OR (PARSE ADV TIMW) (PARSE ADV VBAD)) ADV4 NIL (ADV))

     ;;;
     (COND ((AND (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))           ;"...WILL BE GOING TO..."
           ((AND (NQ BE) (PARSE VB ING))                   ;"BE BEING"
        (SETQ TENSE (CONS 'PRESENT TENSE))
        (GO EN2))                           ;AS IN "BE BEING X'EN(ED)"
           ((AND (NQ ING) (PARSE VB ING (MVB)))               ;"BE X'ING"
        (SETQ TENSE (CONS 'PRESENT TENSE))
        (GO REV))
           ((CQ ING) (MQ ING) (GO FAIL))                   ;IF TRUE, IT IMPLYS THAT WE STARTED OFF WITH
)                                       ;"BEING" - AS IN "BEING EATEN CAN BE UNPLEASANT"
                                       ;- OTHERWISE IT IMPLYS THAT WE HAVE SOMETHING
                                       ;OTHER THAN A VG ON OUR HANDS AND SHOULD FAIL TO
                                       ;WHOEVER CALLED US AND TRY TO PARSE IT
                                       ;DIFFERENTLY

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- EN2 ---------------
    EN2     (: (PARSE VB EN (MVB)) NIL MVBE)                   ;THIS ASKS -DO WE HAVE A VERB IN ITS EN FORM
                                       ;WHICH CAN ACT AS A MAIN VERB (IN WHICH CASE IT
     (FQ PASV)                               ;IS MARKED AS PASSIVE AND WE RETURN)OTHERWISE
     (GO REV)                               ;CHECK IF THE VERB BEING POINTED AT IS A
                                       ;LEGITIMATE FORM OF "BE" IN ITS MAIN VERB SENSE
                                       ;- WHICH IS DONE AT "MVBE"

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- GOING ---------------
    GOING(: (PARSE NIL TO) NIL GOI)
     (: (NQ INF) GOING2 NIL NIL)
     (POP)
    GOI     (SETQ TENSE (CONS 'PRESENT TENSE))                   ;WE HAVE DETERMINED THAT "GOING" IS THE ACTUAL
     (GO MVB)                               ;MAIN VERB AND SHOULD BE PARSED AS SUCH
    GOING2
     (SETQ TENSE (CONS 'FUTURE TENSE))                   ;HERE WE DETERMINE THAT THE PHRASE IS ACTUALLY
     (GO MODAL2)                               ;OF THE FORM "...IS GOING TO FALL IN LOVE..."
                                       ;AND WE SHOULD RUN THROUGH THE DISPATCH TABLE AT
                                       ;"MODAL2" TO DETERMINE HOW TO CONTINUE

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- MVBE ---------------
    MVBE (: (ISQ (MOVE-PT H PV (VB)) AUX) NIL MVB)               ;MOVE TO EARLIER AND EARLIER DAUGHTERS  UNTILL
     (: (ISQ PT BE) NIL (MVBE))                       ;YOU REACH A VERB WHICH IS A "QAUX" - IF THERE
                                       ;ARE NONE THEN CONTINUE AT "MVB" IF WHAT YOU ARE
                                       ;POINTING TO (THE "QAUX") IS NOT A FORM OF "BE",
     (SETMVB PT)                               ;THEN FAIL BECAUSE OF THE UNGRAMATICALITY OF THE
     (GO REV)                               ;CONSTRUCTION OF "BE"'S OTHERWISE MARK IT AS THE
                                       ;MVB AND PREPARE TO RETURN

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- HAVE ---------------
    HAVE (MOVE-PT C DLC)
     (TRNSF VPL INF V3PS VFS)
     (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST))
               (T '(PRESENT))))
     (GOCOND HAV2 MVB)                           ;HAV2 WILL CATCH "HAVE HAD", OR "HAVE BEEN .."
    HAV2 (: (AND (PARSE NIL NOT) (FQ NEG)) NIL NIL (NOT))           ;OR "HAVE KISSED"
    ADV5 (: (PARSE ADV) ADV5 NIL (ADV))
     (: (PARSE VB BE EN) NIL HAV3)
     (SETQ TENSE (CONS 'PAST TENSE))                   ;"HAVE BEEN..."
     (GOCOND BE2 MVB)
    HAV3 (: (PARSE VB (MVB) EN) NIL MVB)
     (SETQ TENSE (CONS 'PAST TENSE))                   ;"HAVE KISSED"
     (GO REV)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- INCOM ---------------
    INCOMP
     (FQ INCOMP)
     (GO FAIL)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- MVB ---------------
    MVB     (: (EQ (FE MVB) (FE H)) MVB2 NIL)
     (POP VB)                               ;POP OFF EVERY THING UNTILL YOU REACH A VERB
     (: (PARSE VB (MVB)) NIL (MVB))
    MVB2 (GO REV)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;;;            CHECK AGREEMENT BETWEEN SUBJECT AND MAIN VERB
     ;;;--------------------------------------------------
    REV     (SETR 'TENSE TENSE C)
     (AND NN (PARSE NIL NOT) (FQ NEG))
     (COND ((OR (EQUAL TENSE '(PAST))
            (CQ NAGR)
            (ISQ (MOVE-PT C U) IMPER)                   ;MOVE PT TO THE CLAUSE REMEMBER THAT THE POINTER
            (ISQ PT THERE)                       ;STAYS WHERE IT'S PUT UNTILL RETURNING FROM A
            (ISQ PT RSNG))                       ;CALL TO PARSE
        (GO NAUX))
           ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))           ;"SUBJECT" IS THE SYNTACTIC SUBJECT OF THE
           (T (ERTERR VG -- NO SUBJECT TO CHECK FOR AGREEMENT)))   ;CLAUSE THAT THE VG IS IN, WHOSE ESSENTIAL
                                       ;DISTINGUISHING FEATURE IS AGREEMENT WITH THE
                                       ;VERB

     ;;;
     ;;;
     ;;;
     (SETQ T3 NIL)                               ;T3 WILL ACT AS A SWITCH AT "NAGR BELOW. NOTE
                                       ;THAT IT IS EXPLICITLY SET BY THE CODE BELOW BY
                                       ;THE FOLLOWING CRITERIA;   IF T3 IS NON-NIL THEN
                                       ;SUBJECT AND VERB HAVE BEEN DETERMINED TO AGREE
                                       ;IF IT IS NIL THEN THEY WILL BE CONSIDERED TO
                                       ;AGREE ONLY IF THE FEATURE "PAST-PRESENT" IS ON
     (COND ((ISQ PT NFS)                           ;THE MVB, IN WHICH CASE, THIS IS EVIDENCE THAT
        (OR (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))           ;THE PROPER CHOISE OF TENSE IS PAST - WHERE
           ((ISQ PT CLAUSE) (OR (SETQ T3 (CQ V3PS)) (GO NAGR)))    ;AGREEMENT IS IRRELEVANT (SEE BELOW AT "NAGR")
           ((OR (ISQ PT NS) (ISQ PT MASS))
        (OR (AND (CQ V3PS) (SETQ T3 T))
            (FESET PT (SETDIF (FE PT) '(NS MASS))))))
     (COND ((OR (ISQ PT PART) (ISQ PT NPL))
        (OR (AND (MEET FE '(INF VPL)) (SETQ T3 T))
            (FESET PT (SETDIF (FE PT) '(PART NPL))))))

     ;;;
     ;;;
     ;;;
     ;;;--------------- NAGR ---------------
    NAGR (: (OR T3
        (AND (EQUAL '(PAST-PRESENT) TENSE)               ;NOTES WHETHER VERB AND SUBJECT WERE FOUND TO
             (SETQ TENSE '(PAST))))                   ;AGREE AND FAILS UNLESS A SPECIAL CONDITION
        NIL                                   ;EXISTS AS NOTED DIRECTLY ABOVE
        (NAGR))

     ;;;
     ;;;
     ;;;
     ;;;--------------- NAUX ---------------
    NAUX (SETMVB (OR (MOVE-PT H PV (MVB)) MVB))
     (: (AND (CQ NAUX)
         (ISQ (MOVE-PT H PV (VB)) AUX)
         (NOT (MOVE-PT PV PV (VB))))
        (NAUX)
        RETSM)                               ;THE VB MAY HAVE THE FEATURE "NAUX" WHICH
                                       ;INDICATES THAT IT CAN NEVER SERVE AS THE
                                       ;AUXILLIARY OF ANOTHER VERB. IF THE PRESENT
                                       ;PARSING REQUIRES IT TO THEN IT FAILS WE CHECK
                                       ;BY SEEING IF THE VG CONTAINS ONLY ONE VERB,
                                       ;WHICH IS AN AUX.

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------- POPV ---------------
    POPV (ERT POPV)
     (GO FAIL)

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;--------------------------------------------------
     ;; RETURN AND CHECK SEMANTICS
     ;;;--------------------------------------------------
    RETSM(: (CALLSM (SMVG)) RETURN FAIL))

(PDEFINE PREPG
     NIL
    ENTERING-PREPG

     ;;;
    ADV     (: (AND (NQ PREPADV) (PARSE ADV PREPADV)) ADV NIL (PREPADV))  ;CHECK FOR ANY INITIAL MODIFING ADVERBS

     ;;;
     (: (COND ((CQ AGENT) (NEXTWORD? 'BY))                   ;EXAMINE THE INITIAL FEATURES OF THE PREPG TO
          ((CQ LOC) (NQ PLACE))                       ;CHECK FOR CONSTRAINTS ON THE PREPOSITION
          ((CQ Q) (NOT (NQ MOTOR)))
          (T))
        NIL
        (PREP))                               ;FAIL IF THE CONSTRAINTS AREN'T MET

     ;;;
     ;;;----------------------------------------------
     ;; PARSE THE PREPOSITION
     ;;;----------------------------------------------
     ;;;
     (: (PARSE PREP) NIL (PREP))
     (MOVE-PT H)
     (TRNSF PLACE TIME)                           ;THIS IS NOT  EXACTLY RIGHT,SINCE  "ON WHAT DAY"
                                       ;IS NOT "PLACE"

     ;;;
     ;;;
     ;;;
     ;;;AT THIS POINT THE POSSIBILITIES ARE:
     ;;;   1. THERE ARE NO MORE WORDS AND THE PREP IS "SHORT"
     ;;;   2. YOU HAVE A MULTIPLE WORD PREPOSITION
     ;;;   3. IT IS INDEED A SINGLE WORD PREP, PARSE ITS OBJECT
     ;;;
     (SETQ T1 H)                               ;SAVE THE PREPOSITION JUST PARSED IN CASE IT IS
     (AND (NQ PREP2)                           ;ONLY THE FIRST WORD OF A MULTIPLE WORD
          (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N)))   ;PREPOSITION
             (PARSE PREP2))
            ((SETQ T1 (COMBINATION? (WORD (NB H))
                        (WORD N)
                        (WORD (CDR N))))
             (PARSE PREP2)
             (PARSE PREP2)))

          ;;;
          (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))           ;CREATE NODE FOR THE COMPOUND WORD
          (SETR 'PARENT C T1))                       ;
     (: (ISQ H NEED2) (NEED2) NIL)                       ;FAIL IF LAST PARSED NEEDS ANOTHER WORD

     ;;;                                   ;GIVE IT A PARENT
     (SETR 'HEAD T1 C)                           ;SET THE REGESTER "PREP" TO THE CONSTITUENT JUST
                                       ;PARSED - IF IT WAS A MULTIPLE-WORD-PREP THEN
     (OR NN (GO SHORT))                           ;"PREP" IS SET TO THE NODE WHICH CONTAINS THE
                                       ;ENTIRE FORM NN POINTS TO WHATEVER WORDS ARE
                                       ;LEFT BEFORE THE CUT POINT

     ;;;
     ;;;----------- ADD FEATURES TO THE PREPG DEPENDING ON THE PREPOSITION PARSED --------
     (COND ((EQ (WORD H) 'BY) (FQ AGENT)))

     ;;;
     ;;;
     ;;;
     ;;;
     ;;;
     ;;;-----------------------------------------------------
     ;; PARSE THE OBJECT TO THE PREPOSITION
     ;;;-----------------------------------------------------
     ;;;                                   ;CERTAIN RESTRICTIONS PLACED ON THE POSSIBLE
    QUEST(: (CQ QUEST) NIL NG)                           ;NOUN GROUPS THAT IT CAN TAKE - HENSE THE
                                       ;SPECIAL CALL TO PARSE AS ABOVE, IF THE PREPG IS
     (: (PARSE NG QUEST OBJ) OBJR (PREPQUEST))               ;MARKED WITH THE FEATURE "QUEST" (INDICATING
     (: (AND (CQ OF) (PARSE NG OFOBJ)) OBJR NIL)               ;THAT IT SHOULD CONTAIN THE QUESTION ELEMENT OF
    NG     (: (PARSE NG OBJ) OBJR NIL)                       ;THE CLAUSE) THEN WE PARSE IT SPECIALLY SIMPLE
    REL     (: (NEXTWORD? 'WHICH) NIL REST)                   ;NOUN GROUP - NO RESTRICTIONS
     (: (ISQ (MOVE-PT U) CLAUSE) NIL (PREP-WHICH))               ;IF THE NEXT WORD IS A RELWORD, SUCH AS "WHICH"
                                       ;OR "WHOM", THEN A FAIRLY STRICT SET OF
                                       ;CONSTRAINTS APPLY. THE PREPG IS REQUIRED TO BE
                                       ;WITHIN A RANK-SHIFTED-QUALIFIER CLAUSE (RSQ)
                                       ;WHERE IT CAN APPEAR AT PRACTICLY ANY POINT -"OF
                                       ;WHOM WERE YOU SPEAKING" - "GIVE THE THE
                                       ;MANUSCRIPT, THE LETTERING ON THE COVER OF WHICH
                                       ;IS LARGER THAN PROSCRIBED IN THE GOVERNMENT
                                       ;MANUAL" (HONEST - I HAD ONE OF THOSE IN A
                                       ;LINGUITICS CLASS). IT IS MOST LIKELY THAT THE
                                       ;CONSTRUCTION WILL APPEAR AT THE START OF THE
                                       ;CLAUSE AND THAT ACCORDINGLY, THE NOUNGROUP
                                       ;PROGRAM WHICH SAW IT WILL HAVE INITIALLY ASKED
                                       ;TO PARSE A PREPG, INSTEAD OF THE RSQ WHICH THE
                                       ;CONSTRUCTION ACTUALLY INFERS. BECAUSE OF THIS,
                                       ;PREPG FAILS WITH THE MESSAGE "PREP-WHICH" IF IT
                                       ;IS NOT EXPLICITLY WITHIN AN RSQ. THE FAILURE
                                       ;MESSAGE IS READ BY THE NG PROGRAM WHICH THEN
     (: (ISQ PT PRONREL) NIL PRONREL)                   ;CHANGES ITS REQUEST FROM (PARSE PREPG Q)  TO
     (SETQ MES (CDR MES))                           ;(PARSE CLAUSE RSQ PREPREL), WHICH SHOULD PICK
                                       ;UP THE BOTHERSOME PREPG AS AN INITIAL MODIFIER
                                       ;TO THE CLAUSE AND DEAL WITH IT APPROPRIATELY
                                       ;RESET THE FAILURE MESSAGE LIST (WE KNOW TO DO
     (GO P-RELWRD)                               ;THIS BECAUSE THE "PRONREL" AS AN INITIAL
    PRONREL                                   ;FEATURE OF THE CLAUSE IMPLICATES THE PASSAGE OF
     (REMOVE-F-PT 'REL-NOT-FOUND PT)                   ;THE PROS CESS DESCRIBED ABOVE)
     (ADD-F-PT 'PRONREL PT)
    P-RELWRD
     (PARSE NG RELWD OBJ)
     (SETR 'OBJ1 (GETR 'HEAD PT) C)                       ;THE REGISTER IS ACCESSED BY CODE IN THE PASSIVE
     (GO RETT)                               ;SECTION OF CLAUSE AND BY THE APPROPRIATE
    REST (: (PARSE CLAUSE RSNG ING) OBJR SHORT)                   ;SEMANTIC SPECIALIST "HEAD" IS HERE THE HEAD OF
    OBJR (SETR 'OBJ1 H C)                           ;THE HIGHER NOUNGROUP
     (GO RETT)

     ;;;
     ;;;
     ;;;------------------SHORT---------------
    SHORT(: (MEET FE '(NOSHORT Q)) (SHORT) NIL)
     (OR (ISQ (MOVE-PT C U) REL-NOT-FOUND)
         (ISQ (GETR 'QUESTION-ELEMENT PT) QADJ)
         (GO FAIL))
     (REMOVE-F-PT 'REL-NOT-FOUND PT)
     (ADD-F-PT 'PREPREL PT)
     (SETR 'OBJ1 (GETR 'RELHEAD (MOVE-PT C U)) C)

     ;; IF THE REFERENT OF THE RELATIVE CLAUSE THIS SHORT
     ;;PREPOSITION IS ASUMED TO BE IN, HAS NOT BEEN DETERMINED,
     ;;THEN SET THE REGISTER FOR THE OBJECT OF THE PREP.  TO THE
     ;;RELWORD.  IF THERE IS NO RELWORD THEN THE PREPG FAILS
     ;;AFTER SENDING UP A COMPLAINING MESSAGE.
     ;;;
     ;;;
     ;;;
     ;;;-----------------------  FINAL CHECKS, AND  RETURN --------------------
    RETT

     ;;CHECK IF THIS PREPG SHOULD BE MARKED AS CONTAINING A
     ;;QUESTION ELEMENT.  IE.  "FOR WHAT", "BETWEEN THE RED BLOCK
     ;;AND WHICH?" (ECHO)
     (AND (OR (ISQ H QUEST)                           ;H IS THE NG FOUND FOR AN OBJECT
          (AND (ISQ H COMPOUND)                       ;IF THE NOUN GROUP IS COUMPOUND, CHECK EACH
               (MOVE-PT H H PV (QUEST))))               ;COMPONENT FOR THE FEATURE "QUEST"
          (FQ QUEST))

     ;;;
     ;;;
     ;;;
     (: (CALLSM (SMADJG-PREPG)) RETURN FAIL))

(PDEFINE ADJG
     NIL
    ENTERING-ADJG                               ;THIS LABEL IS MARKED BY DEBUGGING ROUTINES  AND
    COMPCHECK                                   ;IS USEFUL FOR FOLLOWING THE FLOW OF CONTROL
     (: (AND (MOVE-PT C U (BE)) (NOT (CQ COMP))) FAIL NIL)           ;CONDITIONS WHICH MUST BE MET BY ANY ADJECTIVE
                                       ;GROUP IF THERE IS A FORM OF "BE" IN THE HIGHER
                                       ;CLAUSE, THEN THE ADJG SHOULD HAVE BEEN CALLED
                                       ;WITH THE FEATURE "COMP" FOR COMPLIMENT

     ;; EXAMINE THE INITIAL FEATURES (THOSE DESIGNATED BY THE
     ;;CALLING PROGRAM) ALSO EXAMINE THE NEXT WORD - THESE GIVE
     ;;CLUES AND CONSTRAINTS TO THE STRUCTURE TRYING TO BE PARSED
     ;;AND DIRECT JUMPS TO THE APPROPRIATE SECTIONS OF CODE
     ;;;
     (: (ISQ (MOVE-PT C U) THAN) NIL DISP)                   ;THE WORD "THAN" WAS DETECTED BY THE IMMEDIATELY
                                       ;UPSTAIRS NG AS FOLLOWING THE HEAD NOUN
     (SETR 'HEAD (GETR 'COMPARATIVE-MODIFIER PT) C)               ;INDICATING A STURCTURE SUCH AS "..A BIGGER
     (GO THAN)                               ;BLOCK THAN THAT ONE..." "HEAD REFERS TO THE
                                       ;ADJG'S HEAD ADJECTIVE

     ;;;
     ;;;
    DISP (: (AND (NQ AS) (PARSE NIL AS)) AS NIL (AS))
     (: (AND (NQ AS) (PARSE NIL AS)) AS NIL (AS))
     (: (NEXTWORD? 'HOW) HOW ADV)

     ;;;----------------- HOW + ADJG ------
    HOW     (: (AND (PARSE NIL HOW) (FQ QUEST)) NIL FAIL FAIL)
     (: (AND (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C))
        RETSM
        NIL)
     (: (AND (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C))
        RETSM
        FAIL)
    ADV     (: (PARSE ADV ADVADV) ADV NIL POPAD)                   ;THIS LOOPS UNTILL ALL CONTIG- UOUS ADVERBS HAVE
     (: (PARSE NIL MORE) NIL ADJ)                       ;BEEN PARSED "MORE" IS EXPLICITLY CHECKED FOR
     (FQ COMPAR)                               ;SINCE IT SIGNALS THE FEATURE, COMPARATIVE
    ADJ     (: (COND ((CQ ADV) (PARSE ADV VBAD)) (T (PARSE ADJ)))
        NIL
        (ADJ))                               ;IF THE CUT POINT WAS REACHED THEN NO MORE
     (: (SETR 'HEAD H C) NIL NIL RETSM)                   ;PROCESSING (SUCH AS COMPAR BELOW) IS POSSIBLE.

     ;;;----------------------------------------
     ;; COMPARATIVES
     ;;;---------------------------------------- IF THE FEATURE
     ;;"COMPAR" IS ALREADY ON THE LIST, OR IF THE JUST PARSED
     ;;ADJECTIVE CAN HAVE THAT FEATURE, THEN ATTEMPT TO PARSE
     ;;SOME SORT OF COMPARATIVE CONSTRUCTION (ASSUMING THAT
     ;;THEREARE ANY MORE WORDS BEFORE THE CUT POINT.)
     (: (OR (CQ COMPAR) (ISQ H COMPAR)) NIL RETSM)
     (FQ COMPAR)
     (: NN NIL RETSM)                           ;IF THERE ARE UNPARSED WORDS LEFT BEFORE THE CUT
                                       ;POINT THEN THE POSSIBILITY OF MORE COMPLICATED
                                       ;FORMS IS CHECKED FOR

     ;;;------------------ THAN ----------
    THAN (COND ((NOT NN) (GO RETSM)))
     (: (PARSE NIL THAN) NIL RETSM (THAN))
     (RQ THANNEED)                               ;THE FEATURE "THANNEEED" MARKS THAT THE WORD
     (FQ THAN)                               ;"THAN" IS EXPLICITLY  REQUIRED IN THE PHRASE.
     (GO SUBJ)

     ;;;-------------------- AS -------
    AS     (FQ AS)
     (RQ THANNEED)
     (: (AND (PARSE ADJ) (SETR 'HEAD H C)) NIL (ADJ) RETSM)
     (: (PARSE NIL AS) SUBJ RETSM (AS))

     ;;;--------------- FIND A SUBJECT FOR THE COMPARATIVE
     ;; IE.  "AS BIG AS ..." , "BIGGER THAN ..."
    SUBJ (: (PARSE NG SUBJ COMPAR) NIL (THAN))
     (: (SETR 'OBJ1 H C) NIL NIL RETSM)
     (: (AND (ONE-WORD-LEFT) (PARSE VB AUX)) NIL RETSM)
     (: (CHECK-AGREEMENT H (CDR H))                       ;CHECKS FOR AGREEMENT IN NUMBER AND PERSON
        RETSM                               ;BETWEEN THE NG PARSED AS SUBJ AND THE
        NIL)                               ;JUST-PARSED VERB
     (POP)
     (GO RETSM)

     ;; AT PRESENT, THIS ENTIRE ROUTINE IS INADIQUATE IN SEVERAL
     ;;RESPECTS: THE EXISTING BACKUP MECHANISM CORRECTLY REFUSES
     ;;TO PARSE THE "ARE" IN "SOME PEOPLE BIGGER THAN JOHN ARE
     ;;STANDING..." AS PART OF THE ADJG, BUT DOES SO ONLY BECAUSE
     ;;OF THE DISSAGREEMENT IN NUMBER (CHECKED FOR ABOVE) MORE
     ;;COMPLICATED FORMS (THE VERB IS BY NO MEANS LIMITED TO
     ;;FORMS OF "BE" ), IF IT IS PARSABLE AT ALL WITHOUT
     ;;CONSIDERABLE PRINCIPLED MODIFICATION OF THE CODE, IT WILL
     ;;BE CAUGHT BY THE GENERAL CUTTING MECHANISM WHICH REPARSES
     ;;THE SUBJ-NP WHEN THE VERB IS MISSING OR INCOMPATABLE (SEE
     ;;CLAUSE).
     ;;;
     ;;;
     ;;;
    POPAD(POP)                                   ;IF THE CUT POINT WAS HIT HAVING ONLY PARSED
     (GO ADJ)                               ;ADVERBS, POP OFF THE FINAL ADV AND TRY TO
                                       ;REPARSE IT AS AN ADJECTIVE

     ;;;----------------------- FINAL CHECKS ON COMPARATIVES    (SEMANTIC AND OTHERWISE)
    RETSM(: (CQ THANNEED) (THANNEED) NIL)                   ;IF ONE OF THE WORDS PARSED REQUIRED A "THAN",
     (: (CALLSM (SMADJG-PREPG)) RETURN (SMADJ)))               ;FAIL IF ONE WAS NOT FOUND.

(DEFUN CONJ NIL
       (PROG (END GOODIE)
         (SETQ END CUT)
         (COND ((SETQ GOODIE (APPLY-GRAMMAR 'CONJOIN))
            (RETURN (SETQ RE GOODIE)))
           (T (RETURN NIL)))))

(DEFUN COMMA NIL
       (COND ((SECONDWORD? '") (FLUSHME) T)                   ;IF " FOLLOWS, FLUSH COMMA AND CONTINUE
         ((CONJ))                               ; IF COMMA IS PART OF CONJOINED STRUCTURE, GREAT
         ((ISQ RE INIT) (FLUSHME) T)                   ;IF COMMA FOLLOWS INITIAL-TYPE PHRASE, FLUSH IT
                                       ;AND CONTINUE

         ;; DIRECT ADDRESS JAZZ
))

(PDEFINE CONJOIN
     (PREV)

     ;; THIS PROGRAM IS CALLED TO PARSE A CONJOINED STRUCTURE THE
     ;;FIRST MEMBER OF THE SUPPOSED STRUCTURE HAS ALREADY BEEN
     ;;PARSED AND IS SET TO THE INITIAL DAUGHTER (H) OF THIS NODE
     ;;AN ATTEMPT IS MADE TO PARSE AS MANY CONSTITUENTS JUST LIKE
     ;;IT AS IS POSSIBLE
    ENTERING-CONJOIN                               ;  HACK LABEL FOR LABELTRACER
    UP     (SETQ PREV (NEXTWORD))
     (FLUSHME)
     (COND ((AND (EQ PREV '/,)                       ;IF WE HAVE A COMMA AND
             (OR (CDR H)                       ;IF MORE THAN 1 COMPONENT HAS BEEN PARSED
             (GREATERP (DIFFERENCE (LENGTH (NB H))           ;OR IF THAT ONE COMPONENT
                           (LENGTH (N H)))           ;IS MORE THAN 4 WORDS LONG
                   4.))
             (MEMQ (NEXTWORD) '(OR AND NOR BUT))
             (F (NEXTWORD)))                       ;THEN CHECK FOR COMMA COMBINATION
        (SETQ PREV (LIST PREV (NEXTWORD)))
        (FLUSHME)))
     (AND (ATOM PREV)
          (MOVE-PTW N NW (EQ (WORD PTW) PREV))
          (CUT PTW))
     (AND (OR (EQ PREV 'BUT) (EQ (CADR PREV) 'BUT))
          (NEXTWORD? 'NOT)                           ;CHECK FOR BUT-NOT COMBINATION
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
                       C)))                   ;MARK COMPOUND CLAUSE AS TO DECLAR/IMPER FOR
            (COND ((ISQ H MAJOR) H) (LASTSENT))               ;ANSGEN
            (MEET (FE H) '(DECLAR IMPER)))))
        NIL
        LOSE2)
     (CUT END)                               ;RESTORE CUT POINT
     (COND ((NOT (ATOM PREV))

        ;;IF WE HAD COMMA FOLLOWED BY (AND OR BUT NOR) RETURN
        ;;THE LIST OF GOODIES WE'VE FOUND
        (GO RETSM))
           ((EQ PREV '/,)
        (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP))
              (T (GO LIST))))
           ((MEMQ PREV '(AND OR NOR BUT))
        (COND ((EQ BOTH (NB H)) (FQ BOTH)))
        (COND ((OR (NEXTWORD? 'BUT)
               (AND (NEXTWORD? PREV)
                (NOT (AND (EQ BOTH (NB H))           ;IF WE HAD THE 'BOTH' WORD AND
                      (EQ PREV 'AND)))))           ; IF THE 'BOTH' WORD WAS "AND", STOP PARSING
               (FQ LISTA)                       ; ELSE GO LOOK FOR THE NEXT COMPONENT
               (F PREV)
               (GO UP))
              (T (GO LISTA)))))
    LOSE2(: (CQ LISTA) LISTA NIL)
    LIST (: (AND (EQ PREV '/,)                           ;COME HERE FOR ABORTED LIST AND CHECK FOR
         (EQUAL (LENGTH H) 2.)                       ;APPOSITIVE
         (ISQ H NG)
         (NOT (OR (ISQ H PRONG) (ISQ (CDR H) PRONG)))
         (OR (NEXTWORD? COMMA) (NULL N)))
        NIL
        (CONJOIN: HOPELESS LIST))
     (FLUSHME)                               ;GET RID OF TRAILING COMMA
     (FQ APPOSITIVE)
     (GO RETSM)
    LISTA(F PREV)
    RETSM(FQ COMPOUND)                               ;CALL SEMANTICS AND RETURN EVERY PARSED BY THIS
     (AND (GREATERP (LENGTH H) 2.) (FQ LIST))               ;GOODIE IS COMPOUND IF MORE THAN 2 COMPONENTS
     (COND ((OR (CQ NG) (CQ NOUN))
        (COND ((CQ AND) (FQ NPL))
              (T (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
           ((CQ VB)
        (PROG (COMMON)
              (SETQ COMMON (GET 'VB 'ELIM))
              (MAP '(LAMBDA (X)
                    (SETQ COMMON (MEET COMMON (FE X))))
               H))
        (FESET (UNION COMMON (FE C)) C)))
     (: (CALLSM (SMCONJ)) RETURN (CONJOIN: SMCONJ)))           ;THEN MARK AS A LIST

(DEFUN BOTH

 ;;HANDLES (BOTH AND) (EITHER OR) (NEITHER NOR) COMBINATIONS THE
 ;;CONJOIN PROGRAM DOES SPECIAL THINGS WHEN BOTH IS SET
 FEXPR (A)
       (PROG (END)
         (SETQ END CUT)                           ;MAKE END OUT OF PREVIOUS CUT POINT
         (RETURN (PROG (CUT NBB BOTH)
               (SETQ NBB N)
               (AND (FLUSHME)
                (MOVE-PTW N
                      NW
                      (EQ (WORD PTW) (CAR A))
                      NW)                   ;LOOK FOR THE MATCHING WORD E.G.  AND,OR,NOR
                (CUT END)
                (SETQ BOTH PTW)                   ;SAVE POINTER TO THE WORD AFTER THE MATCHING
                (SETQ RE (COND ((MEMQ (CAR REST)       ;WORD
                              '(PREP ADV))
                        (PARSE3 REST T))
                           ((MEMQ (CAR REST)
                              '(NG PREPG
                            ADJG CLAUSE))
                        (PARSE2 REST T))))
                (LESSP (LENGTH N) (LENGTH BOTH))       ;FAIL UNLESS WE PARSED BEYOND MATCHING WORD
                (RETURN (SETQ SPECIAL 'SKIP)))
               (SETQ RE NIL)
               (SETQ N NBB)
               (RETURN NIL)))))

(DEFUN DOUBLEQUOTER NIL (APPLY-GRAMMAR 'PARSEQUOTED))

(DEFUN CANTAKE (NUM TYPE FEATURE)
       (PROG (VBFEAT)
         (SETQ VBFEAT (FE MVB))
         (RETURN
          (COND ((MEMQ 'RSNG TYPE)
             (MEMQ
              (READLIST (APPEND (COND ((MEMQ 'TO TYPE)
                           '(T O))
                          ((MEMQ 'ING TYPE)
                           '(I N G))
                          ((MEMQ 'REPORT
                             TYPE)
                           '(R E P)))
                    '(O B)
                    (LIST (COND ((EQ NUM 1.)
                             '/1)
                            (T '/2)))))
              VBFEAT))
            ((MEMQ 'COMP TYPE)
             (MEMQ 'INT VBFEAT))
            ((MEMQ 'NG TYPE)
             (COND ((EQUAL NUM 1.)
                (NOT (NULL (MEET '(TRANS TRANS2
                             TRANSL
                             TRANSINT)
                         VBFEAT))))
               (T (MEMQ 'TRANS2 VBFEAT))))
            (T (MEMQ FEATURE VBFEAT))))))

(DEFUN CANPARSE (NUM TYPE FEATURE)
       (PROG (REG)
         (AND
          (CANTAKE NUM TYPE FEATURE)
          (OR
           (NULL TYPE)
           (AND
        (APPLY
         'PARSE
         (APPEND TYPE
             (COND ((MEMQ 'COMP TYPE)
                (SETQ REG 'COMP)
                NIL)
                   (T (LIST 'OBJ
                    (SETQ REG
                          (COND ((OR (MEMQ 'LOC TYPE)(MEMQ 'PLACE TYPE))
                             'LOBJ)
                            ((EQUAL NUM 1.)
                             'OBJ1)
                            (T 'OBJ2))))))))
        (SETR REG H C)))
          (OR (NULL FEATURE) (F FEATURE))
          (RETURN T))))

§ anno/winograd/dictio

;;;===========================================================
;;;
;;;                         WORDS
;;;
;;;===========================================================

(DEFS /, FEATURES (SPECIAL) SPECIAL (COMMA))

(DEFS " FEATURES (B-SPECIAL RELWRD) B-SPECIAL (DOUBLEQUOTER))

(DEFS A SEMANTICS ((DET T)) FEATURES (DET NS INDEF))

(DEFS ABOVE SEMANTICS ((PREP (#LOC #ABOVE T))) FEATURES (PREP PLACE))

(DEFS AFTER SEMANTICS ((BINDER (SMBINDER END NIL))) FEATURES (BINDER TIME))

(DEFS ALL
    SEMANTICS ((DET (COND ((CQ OF) 'ALL)
              ((MEET '(NUM DEF) FE) 'DEF)
              ('NDET))))
    FEATURES (DET NPL QNTFR))

(DEFS AN IRREGULAR (A NIL NIL))

(DEFS AND FEATURES (SPECIAL) SEMANTICS T SPECIAL (CONJ))

(DEFS ANY SEMANTICS ((DET 'INDEF)) FEATURES (DET ANY NS NPL QNTFR))

(DEFS ANYTHING SEMANTICS ((TPRON 'INDEF)) FEATURES (TPRON ANY NS))

(DEFS ARE IRREGULAR (BE (VPL PRESENT) (INF)))

(DEFS AS SEMANTICS ((NULL T)) FEATURES (AS))

(DEFS ASK
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONS: (((#ANIMATE))
                        ((#EVENT)))
  MARKERS: (#EVENT)
                 PROCEDURE: ((#WANT #1 #2 *TIME))))))))
    FEATURES (VB TRANS INF SUBTOB))

(DEFS AT SEMANTICS ((NUMD T)) FEATURES (AT))

(DEFS AWAY SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS BACK SEMANTICS ((PREP2 T) (NOUN T)) FEATURES (NOUN NS PREP2))

(DEFS BALL
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#MANIP #ROUND)
              PROCEDURE: ((#IS *** #BALL))))))
    FEATURES (NOUN NS))

(DEFS BE
    FEATURES ( INT AUX VB BE INF)
    SEMANTICS ((VB
        ((THERE (#BETHERE))
         (INT (#BEINT))))))

(DEFUN #BETHERE NIL
          (RELATION
              (RESTRICTIONS: (((#THING)
                       (EQ (QUANTIFIER? SMSUB)
                       'INDEF)))
               PROCEDURE: NIL)))

(DEFUN #BEINT NIL          (COND
           ((RELATION
            (RESTRICTIONS: (((#PHYSOB))
                    (SMCOMP (#PROPERTY)))
             PROCEDURE: (#EVAL
                     (PROG (PROPERTY)
                      (COND
                       ((SETQ
                     PROPERTY
                     (MEET (GET '#PROPERTY
                            'SYSTEM)
                           (MARKERS? SMCOMP)))
                    (RETURN
                     (LIST (LIST (CAR PROPERTY)
                             '#1
                             '#2))))
                       ((RETURN (LIST '(#2 #1))))))))
            (RESTRICTIONS: (((#THING))
                    (SMCOMP
                     (#SYSTEMS)
                     (AND (NOT (REFER? SMCOMP))
                          (EQ (REL? SMCOMP)
                          SMSUB))))
             PROCEDURE: (#EVAL (RELATIONS? SMCOMP)))
            (RESTRICTIONS: (((#THING))
                    (SMCOMP (#THING)
                        (REFER? SMCOMP)))
             PROCEDURE: ((#EVAL
                      (LIST 'THAMONG
                        '#1
                        (LIST 'QUOTE
                          (REFER? #2))))))))
           (T (ERTSTOP SORRY I DON 'T UNDERSTAND THE
                   VERB BE WHEN YOU USE IT LIKE
                   THAT))))

(DEFS BEFORE SEMANTICS ((BINDER (SMBINDER NIL START))) FEATURES (BINDER TIME))

(DEFS BEGIN
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONS: (((#ANIMATE)) ((#EVENT)))
  MARKERS: (#EVENT)
                 PROCEDURE: ((#START #2 *TIME)))))
         (ITRNS
          (RELATION
              (RESTRICTIONS: (((#ANIMATE)))
  MARKERS: (#EVENT)
               PROCEDURE: ((#START EE *TIME))))))))
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
    SEMANTICS ((PREP (RELATION
             (RESTRICTIONS: (((#PHYSOB)) ((#PHYSOB)))
              PROCEDURE: ((#NEXTO #1 #2 *TIME))))))
    FEATURES (PREP PLACE))

(DEFS BIG
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #SIZE
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 T))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB #BIG)
             PROCEDURE: ((#MORE #SIZE *** (128. 128.
                                128.)))))))
    FEATURES (ADJ))

(DEFS BLACK SEMANTICS ((ADJ (#COLOR #BLACK))) FEATURES (ADJ))

(DEFS BLOCK
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#MANIP #RECTANGULAR)
              PROCEDURE: ((#IS *** #BLOCK))))))
    FEATURES (NOUN NS))

(DEFS BLUE SEMANTICS ((ADJ (#COLOR #BLUE))) FEATURES (ADJ))

(DEFS BOTH
    SEMANTICS ((DET 'DEF))
    FEATURES (B-SPECIAL QNTFR DET DEF NPL BOTH)
    B-SPECIAL (BOTH AND)
    FEXPR (LAMBDA (A)
       (PROG (END)
         (SETQ END CUT)
         (RETURN (PROG (CUT NBB BOTH)
                   (SETQ NBB N)
                   (AND (FLUSHME)
                    (** N
                    NW
                    (EQ (WORD PTW) (CAR A))
                    NW)
                    (CUT END)
                    (SETQ BOTH PTW)
                    (SETQ RE
                      (COND ((MEMQ (CAR REST)
                               '(PREP ADV))
                         (PARSE3 REST T))
                        ((MEMQ (CAR REST)
                               '(NG PREPG
                             ADJG
                             CLAUSE))
                         (PARSE2 REST T))))
                    (LESSP (LENGTH N) (LENGTH BOTH))
                    (RETURN (SETQ SPECIAL 'SKIP)))
                   (SETQ RE NIL)
                   (SETQ N NBB)
                   (RETURN NIL))))))

(DEFS BOX
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#BOX)
              PROCEDURE: ((#IS *** #BOX))))))
    FEATURES (NOUN NS))

(DEFS BRICK FEATURES (NOUN NS))

(DEFS BUILD
    SEMANTICS ((VB ((TRANS (#BUILD)))))
    FEATURES (VB INF TRANS))

(DEFS BUT FEATURES (SPECIAL) SEMANTICS T SPECIAL (CONJ))

(DEFS BY
    SEMANTICS ((PREP (RELATION
             (RESTRICTIONS: (((#PHYSOB)) ((#PHYSOB)))
              PROCEDURE: ((#NEXTO #1 #2 *TIME))))))
    FEATURES (PREP))

(DEFS CALL
    SEMANTICS ((VB
        ((TRANS2
          (RELATION
              (RESTRICTIONS: (((#ANIMATE)) ((#THING))
                           ((#NAME)))
               PROCEDURE: ((#CALL #2 #3 *TIME))))))))
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
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#COLOR)
              PROCEDURE: ((#IS *** #COLOR))))))
    FEATURES (NOUN NS))

(DEFS CONSTRUCT
    SEMANTICS ((VB ((TRANS (#BUILD)))))
    FEATURES (VB INF TRANS))

(DEFS CONTAIN
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONS: (((#BOX)) ((#PHYSOB)))
                 PROCEDURE: ((#CONTAIN #1 #2 *TIME)))
                (RESTRICTIONS: (((#CONSTRUCT))
                        ((#THING)))
                 PROCEDURE: ((#PART #2 #1 *TIME))))))))
    FEATURES (VB INF TRANS))

(DEFS CONTAINER
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#BOX)
              PROCEDURE: ((#IS *** #BOX))))))
    FEATURES (NOUN NS))

(DEFS CORNER FEATURES (NOUN NS))

(DEFS CUBE
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#MANIP #RECTANGULAR)
              PROCEDURE: ((#IS *** #BLOCK) (#EQDIM ***))))))
    FEATURES (NOUN NS))

(DEFS DID IRREGULAR (DO (PAST V3PS) (INF PRESENT)))

(DEFS DO
    SEMANTICS ((VB
        ((TRANS
          (RELATION
              (RESTRICTIONS: RESTRICTIONS:
               PROCEDURE: ((((#ANIMATE)) ((#EVENT))))
               MARKERS: PROCEDURE:
               PLAUSIBILITY: (#EVAL (OR (GET MAP2 'REFER)
                        (ERT DO
                             DEFINITION)))))))))
    FEATURES (TRANS VFS PRESENT VPL VB AUX DO INF))

(DEFS DOES IRREGULAR (DO (V3PS) (VFS VPL INF)))

(DEFS DOWN SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS DROP
    SEMANTICS ((VB
        ((TRANSL
          (RELATION
              (RESTRICTIONS: (((#ANIMATE))
                      ((#MANIP))
                      (SMOBL (#PLACE *TIME)))
               PROCEDURE: ((#DROP #1 #2 #3))
               MARKERS: ((#MOTION)))))
         (TRANS (RELATION
                (RESTRICTIONS: (((#ANIMATE))
                        ((#PHYSOB)))
  MARKERS: (#EVENT)
                 PROCEDURE: ((#DROP #1
                        #2
                        PLACE
                        *TIME))
                 MARKERS: ((#MOTION))))))))
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
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONS: (((#ANIMATE))
                        ((#EVENT)))
  MARKERS: (#EVENT)
                 PROCEDURE: ((#END #2 *TIME))))))))
    FEATURES (VB INF TRANS INFOB))

(DEFS FIVE SEMANTICS ((NUM 5.)) FEATURES (NUM))

(DEFS FOUR SEMANTICS ((NUM 4.)) FEATURES (NUM))

(DEFS FRIEND REFER :FRIEND FEATURES (NOUN NS))

(DEFS FROM FEATURES (PREP))

(DEFS FRONT SEMANTICS ((NOUN T) (PREP2 T)) FEATURES (NOUN NS PREP2))

(DEFS GAVE IRREGULAR (GIVE (PAST) (INF)))

(DEFS GIVE
    SEMANTICS ((VB
        ((TRANS2
          (RELATION
              (RESTRICTIONS: (((#ANIMATE)) ((#ANIMATE))
                           ((#PHYSOB)))
  MARKERS: (#EVENT)
               PROCEDURE: ((#GIVE #1 #2 #3 *TIME))))))))
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
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#HAND)
              PROCEDURE: ((#IS *** #HAND))))))
    FEATURES (NOUN NS))

(DEFS HANDLE
    SEMANTICS ((VB ((TRANS (#GRASP)))))
    FEATURES (VB INF TRANS))

(DEFS HAS IRREGULAR (HAVE (V3PS PRESENT) (INF)))

(DEFS HAVE
    SEMANTICS ((VB ((TRANS (#HAVE)))))
    FEATURES (HAVE VB AUX INF TRANS))

(DEFS HIGH
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #HEIGHT
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 T))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#HIGH ***))))))
    FEATURES (ADJ))

(DEFS HOLD
    SEMANTICS ((VB
        ((TRANS
          (RELATION
              (RESTRICTIONS: (((#HAND)) ((#MANIP)))
               PROCEDURE: ((#GRASPING #2 *TIME)))
              (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
               PROCEDURE: ((#GRASPING #2 *TIME))))))))
    FEATURES (VB INF TRANS))

(DEFS HE FEATURES (PRON NS SUBJ))

(DEFS HER IRREGULAR (SHE (OBJ POSS) (SUBJ)))

(DEFS HIM IRREGULAR (HE (OBJ) (SUBJ)))

(DEFS HIS FEATURES (PRON POSS))

(DEFS HOW SEMANTICS ((QADJ T)) FEATURES (QADJ))

(DEFS HOWEVER FEATURES (PRON EVERPRON))

(DEFS I
    SEMANTICS ((PRON (SMSET (LIST(NEWCOPY 'FRIEND-OSS)))))
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
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #SIZE
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 T))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB #BIG)
             PROCEDURE: ((#MORE #SIZE *** (128. 128.
                                128.)))))))
    FEATURES (ADJ))

(DEFS LEAST
    SEMANTICS ((NUMD (LIST '> (SUB1 NUM))))
    FEATURES (NUMD NUMDAT))

(DEFS LEFT
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#DIRECTION)
              PROCEDURE: ((#DIRECTION #RIGHT NIL))))))
    FEATURES (NOUN NS))

(DEFS LESS
    SEMANTICS ((NUMD (LIST '< NUM)))
    FEATURES (NUMD NUMDAN))

(DEFS LIKE
    SEMANTICS ((VB ((TRANS (RELATION
                   (RESTRICTIONS: (((#ANIMATE))
                           ((#THING)))
                PROCEDURE: ((#LIKE #1 #2))))))))
    FEATURES (VB INF TRANS))

(DEFS LIST SEMANTICS ((VB ((TRANS (#NAME))))) FEATURES (VB VO TRANS))

(DEFS LITTLE
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #SIZE
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 NIL))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB #LITTLE)
             PROCEDURE: ((#MORE #SIZE
                        (128. 128. 128.)
                        ***))))))
    FEATURES (ADJ))

(DEFS LONG
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #LENGTH
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 T))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#MORE #LENGTH
                        ***
                        (128. 128. 128.)))))))
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
    SEMANTICS ((VB ((TRANS (RELATION
                   (RESTRICTIONS: (((#ANIMATE))
                           ((#PHYSOB)))
                PROCEDURE: ((#PUT #2 PLACE *TIME))
                MARKERS: ((#MOTION))))))))
    FEATURES (VB INF TRANS))

(DEFS MY IRREGULAR (I (POSS) (SUBJ)))

(DEFS NAME
    SEMANTICS ((NOUN (OBJECT
             ((#NAME #ROLE) ((IS *** #NAME)
                     (#CALL ? ***)
                     (#ROLE (#THING)
                        (#CALL #2 #1))))))
           (VB ((TRANS (#NAME)))))
    FEATURES (NOUN NS VB INF TRANS))

(DEFS NARROW
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#MORE #WIDTH (128. 0. 0.) ***)))))
            (MEASURE (MEASURE DIMENSION:
                 #WIDTH
                 RESTRICTIONS:
                 (#PHSYOB)
                 DIRECTION:
                 NIL)))
    FEATURES (ADJ))

(DEFS NEITHER FEATURES (B-SPECIAL) SEMANTICS T B-SPECIAL (BOTH NOR))

(DEFS NICE
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#THING)
             PROCEDURE: ((#LIKE :FRIEND ***))))))
    FEATURES (ADJ))

(DEFS NO SEMANTICS ((DET 'NO)) FEATURES (DET QNTFR NS NPL))

(DEFS NONE
    SEMANTICS ((DET 'NO))
    FEATURES (DET QNTFR NPL NS NONUM))

(DEFS NOR FEATURES (SPECIAL) SEMANTICS T SPECIAL (CONJ))

(DEFS NOT SEMANTICS ((ADV T)) FEATURES (ADV NEG))

(DEFS NOTHING SEMANTICS ((TPRON 'NO)) FEATURES (TPRON NEG NS))

(DEFS NOW
    SEMANTICS ((ADV (OR (EQ (CADR (ASSQ 'TIME FE)) ':NOW)
            (ERT NOW DEFINITION))))
    FEATURES (ADV TIMW))

(DEFS OBJECT
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#PHYSOB #VAGUE)
              PROCEDURE: ((#PHYSOB ***))))))
    FEATURES (NOUN NS))

(DEFS OF
    SEMANTICS ((PREP
        (AND
         (CQ NG)
         (RELATION
             (RESTRICTIONS: (((#DIRECTION)) ((#PHYSOB)))
              PROCEDURE: ((#EVAL
                   (LIST
                    '#DIRECTION
                    (CADR
                     (SETQ
                      XX
                      (OR
                       (ASSQ '#DIRECTION
                         (CDDAAR (INTERP MAP1)))
                       (ERT OF DEFINITION))))
                    (COND ((CADDR XX) '*OF)
                      ('#2))
                    (COND ((CADDR XX) '#2)
                      ('*OF))
                    '*TIME)))))))
           (PREP2 T))
    FEATURES (PREP PREP2 OF))

(DEFS OFF SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS ON SEMANTICS ((PREP (#ON))) FEATURES (PREP PLACE))

(DEFS ON-TOP-OF
    ROOT (ON TOP OF)
    SEMANTICS (#ON)
    FEATURES (PREP COMBINATION))

(DEFS ONE SEMANTICS ((NOUN (SMONE)) (NUM 1.)) FEATURES (NUM NOUN NS))

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
    SEMANTICS ((TRANS
        (RELATION
            (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
  MARKERS: (#EVENT)
             PROCEDURE: ((#EVAL (COND ((MEMQ (NUMBER? SMOB1)
                             '(1. NS))
                           '(#PICKUP #2 *TIME))
                          ('(#PUTIN #2
                            :BOX
                            *TIME)))))))))
    FEATURES (COMBINATION TRANS))

(DEFS PLEASE FEATURES (B-SPECIAL) SEMANTICS T B-SPECIAL (FLUSHME))

(DEFS POINTED
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#PHYSOB #POINTED)
             PROCEDURE: ((#SHAPE *** #POINTED))))))
    FEATURES (ADJ))

(DEFS PUT
    PAST PUT
    SEMANTICS ((VB
        ((TRANSL
          (RELATION
              (RESTRICTIONS: (((#ANIMATE))
                      ((#PHYSOB))
                      (SMOBL (#PLACE)))
  MARKERS: (#EVENT)
               PROCEDURE: (#EVAL
                   (MAPCAR
                    '(LAMBDA (%PLNRPHRASE)
                      (COND ((EQ (CAR %PLNRPHRASE)
                         '#ON)
                         (LIST '#PUTON
                           '#2
                           (CADR %PLNRPHRASE)
                           '*TIME))
                        ((EQ (CAR %PLNRPHRASE)
                         '#IN)
                         (LIST '#PUTIN
                           '#2
                           (CADR %PLNRPHRASE)
                           '*TIME))
                        ((ERT PUT DEFINITION))))
                    (RELATIONS? SMOBL)))))))))
    FEATURES (INF PAST VB TRANSL VPRT))

(DEFS PUT-AWAY
    ROOT (PUT AWAY)
    SEMANTICS ((TRANS (RELATION
              (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
  MARKERS: (#EVENT)
               PROCEDURE: ((#PUTIN #2 :BOX *TIME))))))
    FEATURES (COMBINATION TRANS))

(DEFS PUT-DOWN
    ROOT (PUT DOWN)
    SEMANTICS ((TRANS (RELATION
              (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
  MARKERS: (#EVENT)
               PROCEDURE: ((#PUTON #2 :TABLE *TIME))))))
    FEATURES (COMBINATION TRANS))

(DEFS PUT-TOGETHER
    ROOT (PUT TOGETHER)
    SEMANTICS ((TRANS (#BUILD)))
    FEATURES (COMBINATION TRANS))

(DEFS PYRAMID
    FEATURES (NOUN NS)
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#PHYSOB #POINTED)
              PROCEDURE: ((#IS *** #PYRAMID)))))))

(DEFS RED SEMANTICS ((ADJ (#COLOR #RED))) FEATURES (ADJ))

(DEFS RELEASE FEATURES (VB TRANS INF))

(DEFS RIGHT
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#DIRECTION)
              PROCEDURE: ((#DIRECTION #RIGHT T))))))
    FEATURES (NOUN NS))

(DEFS ROUND
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#PHYSOB #ROUND)
             PROCEDURE: ((#SHAPE *** #ROUND))))))
    FEATURES (ADJ))

(DEFS SAW IRREGULAR (SEE (PAST) (INF)))

(DEFS SEE FEATURES (VB INF TRANS))

(DEFS SET SEMANTICS ((VB T)) FEATURES (VB INF))

(DEFS SET-DOWN
    ROOT (SET DOWN)
    SEMANTICS ((TRANS (RELATION
              (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
  MARKERS: (#EVENT)
               PROCEDURE: ((#PUTON #2 :TABLE *TIME))))))
    FEATURES (COMBINATION TRANS))

(DEFS SHAPE
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#SHAPE)
              PROCEDURE: ((#IS *** #SHAPE))))))
    FEATURES (NOUN NS))

(DEFS SHE FEATURES (PRON SUBJ NS))

(DEFS SHORT
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #HEIGHT
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 NIL))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#MORE #HEIGHT
                        (128. 0. 0.)
                        ***))))))
    FEATURES (ADJ))

(DEFS SHRDLU REFER :SHRDLU)

(DEFS SINCE SEMANTICS ((BINDER (SMBINDER END NIL))) FEATURES (BINDER TIME))

(DEFS SIT
    SEMANTICS ((VB
        ((ITRNSL
          (RELATION
              (RESTRICTIONS: (((#PHYSOB))
                      (SMOBL (#PLACE)))
               PROCEDURE: (#EVAL
                   (MAPCAR
                    '(LAMBDA (%PLNRPHRASE)
                      (COND ((MEMQ (CAR %PLNRPHRASE)
                           '(#ON #IN))
                         (LIST '#SUPPORT
                           (CADR %PLNRPHRASE)
                           '#1
                           '*TIME))
                        ((ERT SIT DEFINITION))))
                    (RELATIONS? SMOBL)))))))))
    FEATURES (VB INF ITRNSL))

(DEFS SIZE
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#SIZE)
              PROCEDURE: ((#IS *** #SIZE))))))
    FEATURES (NOUN NS))

(DEFS SMALL
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #SIZE
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 NIL))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB #LITTLE)
             PROCEDURE: ((#MORE #SIZE
                        (128. 128. 128.)
                        ***))))))
    FEATURES (ADJ))

(DEFS SOME
    SEMANTICS ((DET 'INDEF))
    FEATURES (DET QNTFR NS NPL NONUM))

(DEFS SOMETHING SEMANTICS ((TPRON 'INDEF)) FEATURES (TPRON NS))

(DEFS SPHERE FEATURES (NOUN NS))

(DEFS SQUARE
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#PHYSOB #RECTANGULAR)
             PROCEDURE: ((#SHAPE ** #RECTANGULAR))))))
    FEATURES (ADJ))

(DEFS STACK
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#STACK)
              PROCEDURE: ((#IS *** #STACK)))))
           (VB ((TRANS (#STACKUP)))))
    FEATURES (NOUN NS VB INF VPRT TRANS))

(DEFS STACK-UP
    ROOT (STACK UP)
    SEMANTICS ((TRANS (#STACKUP)))
    FEATURES (COMBINATION TRANS))

(DEFS START
    SEMANTICS ((VB
        ((TRANS
          (RELATION
              (RESTRICTIONS: (((#ANIMATE)) ((#EVENT)))
  MARKERS: (#EVENT)
               PROCEDURE: ((#START #2 *TIME))))))))
    FEATURES (VB INF TRANS INGOB1 TOOB1))

(DEFS SUPPORT
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#PHYSOB #ROLE)
              PROCEDURE: ((#SUPPORT *** ?)
                      (#ROLE (#PHYSOB)
                         (#SUPPORT #1 #2))))))
           (VB
        ((TRANS
          (RELATION
              (RESTRICTIONS: (((#PHYSOB)) ((#MANIP)))
               PROCEDURE: ((#SUPPORT #1 #2 *TIME))))))))
    FEATURES (VB INF TRANS IMPERF NOUN NS))

(DEFS TABLE
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#TABLE)
              PROCEDURE: ((#IS *** #TABLE))))))
    FEATURES (NOUN NS))

(DEFS TAKE FEATURES (VB INF TRANSL TRANS))

(DEFS TALL
    SEMANTICS ((MEASURE (MEASURE DIMENSION:
                 #HEIGHT
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 T))
           (ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#MORE #HEIGHT *** (128. 0. 0.)))))))
    FEATURES (ADJ))

(DEFS TELL
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONS: (((#ANIMATE))
                        ((#EVENT)))
  MARKERS: (#EVENT)
                 PROCEDURE: ((#WANT #1 #2 *TIME))))))))
    FEATURES (VB INF TRANS2 TOOB2))

(DEFS THAN SEMANTICS ((NULL T)) FEATURES (THAN))

(DEFS THANK FEATURES (B-SPECIAL) SEMANTICS  (THANK)B-SPECIAL (THANK))

(DEFUN THANK NIL
    (COND ((EQ (CADR N) 'YOU)
            (SAY YOU'RE WELCOME)
            (FLUSHME)
            (FLUSHME)
          (OR NN (IOC G))
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
             (RPLACD (CDDADR (OR (AND (SETQ XX (ASSQ 'TIME
                                 FE))
                          (NOT (ATOM (CADR XX)))
                          XX)
                     '(TIME (#TIME (PAST) NIL))))
                 (LIST (OR (CADDDR LASTIME)
                       (CAR (CDDDDR LASTIME)))
                   (OR (CAR (CDDDDR LASTIME))
                       (CADDDR LASTIME)))))))
    FEATURES (ADV TIMW))

(DEFS THERE SEMANTICS ((ADV T)) FEATURES (ADV PLACE))

(DEFS THEY
    SEMANTICS ((PRON (SMIT 'THEY)))
    FEATURES (PRON SUBJ NPL))

(DEFS THICK
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#MORE #THICKNESS *** (0. 128. 0.))))))
            (MEASURE (MEASURE DIMENSION:
                 #THICKNESS
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 T)))
    FEATURES (ADJ))

(DEFS THIN
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#MORE #THICKNESS
                        (0. 128. 0.)
                        ***)))))
           (MEASURE (MEASURE DIMENSION:
                 #THICKNESS
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 NIL)))
    FEATURES (ADJ))

(DEFS THING
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#THING #VAGUE #PHYSOB)
              PROCEDURE: ((#PHYSOB  *** ))))))
    FEATURES (NOUN NS))

(DEFS THIS FEATURES (NS DET DEM DEF))

(DEFS THREE SEMANTICS ((NUM 3.)) FEATURES (NUM))

(DEFS TIME FEATURES (NOUN NS TIM1))

(DEFS TO
    SEMANTICS ((PREP
        (RELATION
            (RESTRICTIONS: (((#PHYSOB)) ((#DIRECTION)))
             PROCEDURE: ((#EVAL
                  (SUBTOP '#1
                      '*OF
                      (REFERENCE? SMOB1))))))))
    FEATURES (PREP))

(DEFS TOGETHER SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS TOLD IRREGULAR (TELL (PAST) (INF)))

(DEFS TOP SEMANTICS ((PREP2 T)) FEATURES (PREP2))

(DEFS TOUCH
    SEMANTICS ((VB ((TRANS (#GRASP)))))
    FEATURES (VB INF TRANS))

(DEFS TOY
    SEMANTICS ((NOUN (OBJECT
             (MARKERS: (#PHYSOB)
              PROCEDURE: ((#MANIP ***))))))
    FEATURES (NOUN NS))

(DEFS TWO SEMANTICS ((NUM 2.)) FEATURES (NUM))

(DEFS UNDER
    SEMANTICS ((PREP (#LOC #ABOVE NIL)))
    FEATURES (PREP PLACE))

(DEFS UNDERNEATH
    SEMANTICS ((PREP ((#LOC #ABOVE NIL))))
    FEATURES (PREP PLACE))

(DEFS UP SEMANTICS ((PRT T)) FEATURES (PRT))

(DEFS US IRREGULAR (WE (OBJ) (SUBJ)))

(DEFS WANT
    SEMANTICS ((VB
        ((TRANS (RELATION
                (RESTRICTIONS: (((#ANIMATE))
                        ((#EVENT)))
  MARKERS: (#EVENT)
                 PROCEDURE: ((#WANT #1 #2 *TIME))))))))
    FEATURES (VB INF TRANS TOOB SUBTOB))

(DEFS WAS IRREGULAR (BE (V3PS VFS PAST) (INF)))

(DEFS WE
    SEMANTICS ((PRON(SMSET (LIST (NEWCOPY 'WE-OSS)))))
    FEATURES (PRON NPL SUBJ))

(DEFS WERE IRREGULAR (BE (VPL PAST) (INF)))

(DEFS WHAT
    SEMANTICS ((DET T) (PRON (SMSET (LIST(NEWCOPY 'UNKNOWN-OSS)))))
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
    SEMANTICS ((PRONREL T)
           (PRON (SMSET (LIST(NEWCOPY ANIMATE-OSS)))))
    FEATURES (PRONREL QPRON PRON NS))

(DEFS WHOEVER FEATURES (PRON EVERPRON NS))

(DEFS WHOSE FEATURES (DET QDET NPL NS))

(DEFS WHY SEMANTICS ((QADJ (FQ WHY))) FEATURES (QADJ))

(DEFS WHYEVER FEATURES (PRON EVERPRON NS))

(DEFS WIDE
    SEMANTICS ((ADJ (OBJECT
            (MARKERS: (#PHYSOB)
             PROCEDURE: ((#MORE #WIDTH *** (0. 128. 0.))))))
            (MEASURE (MEASURE DIMENSION:
                 #WIDTH
                 RESTRICTIONS:
                 (#PHYSOB)
                 DIRECTION:
                 T)))
    FEATURES (ADJ))

(DEFS WILL SEMANTICS ((VB T)) FEATURES (VB AUX WILL MODAL V3PS VFS VPL))

(DEFS WITH FEATURES (PREP))

(DEFS WOULD SEMANTICS ((VB T)) FEATURES (VB AUX MODAL))

(DEFS YOU
    SEMANTICS ((PRON (SMSET (LIST (NEWCOPY 'SHRDLU-OSS)))))
    FEATURES (PRON NPL NS SUBJ OBJ))

(DEFS YOUR IRREGULAR (YOU (POSS) NIL))

;;;============================================================
;;;
;;;                         #WORDS
;;;
;;;============================================================

(DEFS #ANIMATE SYSTEM (#ROBOT #PERSON) SYS (#THING))

(DEFS #ASMUCH THMLIST ((4. '((THUSE TC-ASMUCH)))))

(DEFS #BELONG THMLIST ((3. '((THUSE TC-BELONG)))))

(DEFS #BLACK SYS (#SPECTRUM))

(DEFS #BLUE SYS (#SPECTRUM))

(DEFS #BLUEPRINT
    EXPR (LAMBDA (X)
         (PROG (PARTS)
               (COND ((GET X 'REFER) (RETURN '#2))
                 ((NULL (SETQ X (CDDAAR (INTERP X))))
                  (GO DONE)))
          LOOP (COND ((NOT (EQ (CAAR X) 'THGOAL))
                  (ERT BLUEPRINT THGOAL))
                 ((EQ (CAADAR X) '#IS))
                 ((EQ (CAADAR X) '#PART)
                  (SETQ PARTS
                    (CONS (CADR (CADAR X)) PARTS)))
                 ((ERT #BLUEPRINT)))
               (AND (SETQ X (CDR X)) (GO LOOP))
          DONE (AND PARTS
                (GET (CAR PARTS) 'REFER)
                (RETURN (GET (CAR PARTS) 'REFER)))
               (PUTPROP 'BLUEPRINT
                (COND ((NULL PARTS) (GET 'STACKPARTS
                             'SM))
                      ((CDR PARTS)
                       (ERT #BLUEPRINT PARTS))
                      ((GET (CAR PARTS) 'SM)))
                'SM)
               (RETURN 'BLUEPRINT))))

(DEFS #BOX SYS (#PHYSOB))

(DEFS #BUILD
    EXPR (LAMBDA NIL
      (RELATION
          (RESTRICTIONS: (((#ANIMATE)) ((#STACK)))
  MARKERS: (#EVENT)
           PROCEDURE: ((#EVAL (LIST '#STACKUP
                    (#BLUEPRINT SMOB1)
                    '*TIME)))))))

(DEFS #CALL THMLIST ((3. '((THUSE TC-3)))))

(DEFS #COLOR
    FEXPR (LAMBDA (A)
       (EVAL (SUBST (CAR A)
            'COLOR
            '(OBJECT
                 (MARKERS: (#PHYSOB COLOR)
                  PROCEDURE: ((#COLOR *** COLOR)))))))
    PRIORITY 192.
    SYS (#PROPERTY))

(DEFS #CONSTRUCT SYSTEM (#STACK #ROW) SYS (#PHYSOB))

(DEFS #CONTAIN PRIORITY -1.)

(DEFS #CLEANOFF
    EXPR (LAMBDA NIL
         (RELATION
             (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB)))
  MARKERS: (#EVENT)
              PROCEDURE: ((#CLEARTOP #2 *TIME))))))

(DEFS #CLEARTOP
    NAMEVENT (I3 (CONS (VBFIX 'CLEAN NIL)
               (PRTPUT 'OFF OBJ1)))
    THMLIST ((2. '((THUSE TC-2))) (3. '((THUSE TCT-3)))
                  (4. '((THUSE TCTE-4)))))

(DEFS #DEFINE
    EXPR (LAMBDA (X Y)
         (LIST '#DEFINITION
               (CADADR (CDAAR (INTERP X)))
               (PROG (X)
                 (PUTPROP (SETQ X (MAKESYM 'ATM))
                      (INTERP Y)
                      'NEWWORD)
                 (RETURN X)))))

(DEFS #DEFINITION
    FEXPR (LAMBDA (A)
       (PUTPROP (CADAR A) '(NOUN NS) 'WORD)
       (PUTPROP
        (CADAR A)
        (SUBST (SUBST '***
              (CADDR (GET (CADR A) 'NEWWORD))
              (CAR (GET (CADR A) 'NEWWORD)))
           'NG
           '((NOUN (SETQ LIST2
                 (LIST (SUBST (SUBST (CADDAR LIST1)
                             '***
                             'NG)
                          (CAAR LIST1)
                          (CAR LIST1)))))))
        'SMNTC))
    NOGOAL T)

(DEFS #DIRECTION NOGOAL T)

(DEFS #END
    THMLIST ((3. '((THUSE TC-STARTEND3)))
         (4. '((THUSE TC-STARTEND4)))))

(DEFS #EQDIM
    EXPR (LAMBDA (X)
         (SETQ X (SIZE X))
         (AND (EQ (CAR X) (CADR X)) (EQ (CAR X) (CADDR X))))
    NOGOAL T)

(DEFS #EQUIV PRIORITY 512.)

(DEFS #EVENT SYS (#SYSTEMS))

(DEFS #EXISTS
    THMLIST ((2. '((THUSE TC-EXISTS)))
         (3. '((THUSE TCT-EXISTS)))))

(DEFS #GET-RID-OF
    THMLIST ((2. '((THUSE TCT-EXISTS)))
         (3. '((THUSE THUSE TCT-3)))
         (4. '((THUSE TCTE-4))))
    NAMEVENT (I3 (APPEND (LIST (VBFIX 'GET T) 'RID 'OF)
             OBJ1)))

(DEFS #GRASP
    EXPR (LAMBDA NIL
      (RELATION
          (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
  MARKERS: (#EVENT)
           PROCEDURE: ((#EVAL (COND ((IMPERF)
                     '(#GRASPING #2 *TIME))
                    ('(#GRASP #2 *TIME))))))))
    NAMEVENT (I3 (CONS (VBFIX 'GRASP NIL) OBJ1))
    THMLIST ((2. '((THUSE TC-2))) (3. '((THUSE TCT-3)))
                  (4. '((THUSE TCTE-4)))))

(DEFS #GRASPING THMLIST ((3. '((THUSE TCT-GRASPING)))))

(DEFS #GREEN SYS (#SPECTRUM))

(DEFS #HAND SYS (#PHYSOB))

(DEFS #HAVE
    EXPR (LAMBDA NIL
      (RELATION
          (RESTRICTIONS: (((#THING))
                  ((#THING)
                   (AND
                (MEMQ '#ROLE (MARKERS? SMOB1))
                (CHECK
                 (CADR (ASSOC '#ROLE
                          (RELATIONS? SMOB1)))
                 (MARKERS? SMSUB)
                 (SYSTEMS? SMSUB)))))
           PROCEDURE: ((#SUBST #1 ?)))
          (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB)))
           PROCEDURE: ((#BELONG #2 #1))))))

(DEFS #HEIGHT MEASFN (LAMBDA (X) (CADDR (SIZE X))))

(DEFS #IN
    EXPR (LAMBDA NIL
         (COND ((CQ LOBJ)
            (RELATION
                (RESTRICTIONS: (((#THING)) ((#BOX)))
        MARKERS: (#PLACE)
                 PROCEDURE: ((#IN #2)))))
               ((RELATION
                (RESTRICTIONS: (((#MANIP)) ((#BOX)))
                 PROCEDURE: ((#CONTAIN #2 #1 *TIME)))
                (RESTRICTIONS: (((#MANIP)) ((#HAND)))
                 PROCEDURE: ((#GRASPING #1 *TIME)))
                (RESTRICTIONS: (((#PLACE)) ((#BOX)))
                 PROCEDURE: ((#IN #1 #2)))
                (RESTRICTIONS: (((#MANIP))
                        ((#CONSTRUCT)))
                 PROCEDURE: ((#PART #1 #2 *TIME))))))))

(DEFS #IS PRIORITY 64.)

(DEFS #LIKE TELLABLE T THMLIST ((3. '((THTBF THTRUE)))))

(DEFS #LOC
    THMLIST ((4. '((THUSE TC-LOC))) (5. '((THUSE TCT-LOC))))
    FEXPR (LAMBDA (A) (#LOC2 (CAR A) (CADR A))))

(DEFS #LOC2
    EXPR (LAMBDA (LOCTYPE #LOC)
      (COND
       ((CQ LOBJ)
        (RELATION
        (RESTRICTIONS: (((#THING)) (LOBJ (#PHYSOB)))
MARKERS: (#PLACE)
         PROCEDURE: ((#EVAL (LIST '#LOC
                      LOCTYPE
                      #LOC
                      #2))))))
       ((RELATION
        (RESTRICTIONS: (((#PHYSOB)) ((#PHYSOB)))
         PROCEDURE: ((#EVAL (LIST '#LOC
                      LOCTYPE
                      (COND (#LOC '#1)
                        ('#2))
                      (COND (#LOC '#2)
                        ('#1))
                      '*TIME)))))))))

(DEFS #MANIP SYS (#PHYSOB))

(DEFS #MORE THMLIST ((4. '((THUSE TC-MORE)))))

(DEFS #NAME
    THMLIST ((2. '((THUSE TC-2))))
    EXPR (LAMBDA NIL
         (RELATION
             (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB)))
  MARKERS: (#EVENT)
              PROCEDURE: ((#NAME #2)) )))
    SYS (#SYSTEMS))

(DEFS #NEWWORD SYS (#THING))

(DEFS #NOTICE
    THMLIST ((2. '((THUSE TC-2))))
    EXPR (LAMBDA NIL
         (RELATION
             (RESTRICTIONS: (((#ANIMATE)) ((#PHYSOB)))
  MARKERS: (#EVENT)
              PROCEDURE: ((#NOTICE #2 *TIME))))))

(DEFS #ON
    THMLIST ((3. '((THUSE TC-ON))) (4. '((THUSE TCT-ON))))
    EXPR (LAMBDA NIL
         (COND ((CQ LOBJ)
            (RELATION
                (RESTRICTIONS: (((#THING)) ((#PHYSOB)))
MARKERS: (#PLACE)
                 PROCEDURE: ((#ON #2)))))
               ((RELATION
                (RESTRICTIONS: (((#PHYSOB))
                        ((#PHYSOB)))
                PARAPHRASE: (ANYWHERE ON TOP OF)
                 PROCEDURE: ((#ON #1 #2 *TIME)))
                (RESTRICTIONS: (((#PHYSOB)) ((#MANIP)))
                PARAPHRASE: (DIRECTLY ON THE SURFACE)
                 PROCEDURE: ((#SUPPORT #2 #1 *TIME)))
                (RESTRICTIONS: (((#PLACE)) ((#PHYSOB)))
                 PROCEDURE: ((#ON #1 #2))))))))

(DEFS #PACK THMLIST ((3. '((THUSE TC-3)))))

(DEFS #PART THMLIST ((3. '((THUSE TC-PART)))))    ;PERFORMED ON GRADUATION DAY, JUNE 2, 1972 BY JMH

(DEFS #PERSON SYS (#ANIMATE))

(DEFS #PICKUP
    THMLIST ((2. '((THUSE TC-2))) (3. '((THUSE TCT-PICKUP)))
                  (4. '((THUSE TCTE-PICKUP))))
    NAMEVENT (I3 (CONS (VBFIX 'PICK NIL)
               (PRTPUT 'UP OBJ1))))

(DEFS #PLACE SYS (#SYSTEMS))

(DEFS #PUT
    THMLIST ((3. '((THUSE TCT-3))) (4. '((THUSE TCT-PUT)))
                   (5. '((THUSE TCTE-PUT)))))

(DEFS #PUTIN
    THMLIST ((3. '((THUSE TC-3))) (4. '((THUSE TCT-4)))
                  (5. '((THUSE TCT-5)))))

(DEFS #PUTON
    NAMEVENT (I4 (CONS (VBFIX 'PUT T)
               (APPEND OBJ1 '(ON) OBJ2)))
    THMLIST ((3. '((THUSE TC-3)))(4. '((THUSE TCT-4))) (5. '((THUSE TCTE-5)))))

(DEFS #RAISE THMLIST ((1. '((THUSE TC-RAISE)))))

(DEFS #RECTANGULAR SYS (#SHAPES))

(DEFS #REFERS THMLIST ((2 '((THUSE TC-REFERS)))))

(DEFS #ROLE FEXPR (LAMBDA (A) T) NOGOAL T)

(DEFS #PHYSOB
    SYSTEM (#BOX #CONSTRUCT #HAND #MANIP #TABLE)
    SYS (#THING)
    THMLIST ((2. '((THUSE TC-PHYSOB)))))

(DEFS #PROPDEFINE
    EXPR (LAMBDA (X)
         (PUTPROP X '(PROPN NS) 'FEATURES)               ;CHANGED TO FEATURES FROM 'WORD' IN THE OLD
         (PUTPROP X '((PROPN T)) 'SEMANTICS)))               ;DICTIONARY

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

(DEFS #SHAPE PRIORITY 128. SYS (#PROPERTY))

(DEFS #STACK SYS (#CONSTRUCT))

(DEFS #STACKUP
    THMLIST ((2. '((THUSE TC-2))))
    EXPR (LAMBDA NIL
         (RELATION
             (RESTRICTIONS: (((#ANIMATE)) ((#MANIP)))
  MARKERS: (#EVENT)
              PROCEDURE: ((#STACKUP #2 *TIME)))))
    NAMEVENT (I3 (CONS (VBFIX 'STACK NIL)
               (PRTPUT 'UP OBJ1))))

(DEFS #START
    THMLIST ((3. '((THUSE TC-STARTEND3)))
         (4. '((THUSE TC-STARTEND4)))))

(DEFS #SUBST NOGOAL T)

(DEFS #SUPPORT
    PRIORITY 256.
    THMLIST ((3. NIL) (4. '((THUSE TCT-SUPPORT)))))

(DEFS #SYSTEMS SYSTEM (#THING #EVENT #NAME #RELATION #PLACE))

(DEFS #TABLE SYS (#PHYSOB))

(DEFS #THICKNESS MEASFN (LAMBDA (X) (CADR (SIZE X))))

(DEFS #THING SYS (#SYSTEMS) SYSTEM (#ANIMATE #NAME #PHYSOB #PROPERTY))

(DEFS #UNGRASP
    THMLIST ((1. '((THUSE TC-UNGRASP))))
    NAMEVENT (I3 (APPEND (LIST (VBFIX 'LET T) 'GO 'OF)
             OBJ1)))

(DEFS #WANT
    THMLIST ((4. '((THUSE TC-WANT4))) (5. '((THUSE TC-WANT5)))))

(DEFS #WHITE SYS (#SPECTRUM))

(DEFS #WIDTH MEASFN (LAMBDA (X) (CAR (SIZE X))))

;;;=============================================================
;;;
;;;                    PARTS OF SPEECH
;;;
;;;============================================================

(DEFS ADJ ELIM (ADJ SUP COMPAR))

(DEFS ADV ELIM (ADV PREPADV TIMW TIM2 ADVADV VBAD PLACE LOBJ))

(DEFS BINDER ELIM (BINDER TIME))

(DEFS CLASF ELIM (CLASF))

(DEFS DET
    ELIM (DET NPL
          NS
          PART
          DEF
          INDEF
          NEG
          DEM
          INCOM
          OFD
          QNTFR
          NONUM
          QDET))

(DEFS NOUN ELIM (NOUN POSS MASS NPL NS TIM1 TIME MONTH))

(DEFS NUM ELIM (NUM NPL NS))

(DEFS NUMD ELIM (NUMD NUMDAN NUMDAT NUMDALONE))

(DEFS ORD ELIM (ORD TIMORD))

(DEFS POSS ELIM (NOUN NPL NS MASS NFS PRON))

(DEFS PREP ELIM (PREP MOTOR PLACE NEED2))

(DEFS PREP2 ELIM (PREP2))

(DEFS PRON
    ELIM (PRON QPRON EVERPRON POSS SUBJ OBJ NS NPL NFS NEG DEFPOSS))

(DEFS PRT ELIM (PRT))

(DEFS QADJ ELIM (PLACE QADJ))

(DEFS PROPN ELIM (PROPN POSS NS NPL))

(DEFS TPRON ELIM (TPRON NS NPL NEG ANY))

(DEFS VB
    ELIM (VB MVB
         AUX
         QAUX
         MODAL
         WILL
         BE
         DO
         HAVE
         ING
         EN
         INF
         V3PS
         QUOTING
         VFS
         VPL
         PAST
         PRESENT
         NEG
         ITRNS
         TRANS
         TRANSL
         TRANS2
         TRANSL2
         INT
         ITRNSL
         INGOB
         TOOB
         SUBTOB
         REPOB
         INGOB2
         TOOB2
         SUBTOB2
         REPOB2
         VPRT
         TO2
         TRANSINT
         TOOB1
         INGOB1
         REPOB1))

;;;============================================================
;;;
;;;                    I'M NOT QUITE SURE WHAT TO
;;;                    DO WITH THIS RANDOM STUFF
;;;
;;;============================================================

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

;;;============================================================
;;;
;;;                    PRE-BUILT OSS'S
;;;
;;;============================================================

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
    DETERMINER= (1. DEF NIL))

(DEFS NAME-OSS
    OSSNODE= NAME-OSS
    MARKERS= (#NAME #THING #SYSTEMS)
    SYSTEMS= (#THING #SYSTEMS)
    DETERMINER= (1. DEF NIL))

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
    DETERMINER= (1. DEF NIL))

(DEFS STACKPARTS-OSS
    OSSNODE= STACKPARTS-OSS
    MARKERS= (#THING #PHYSOB #MANIP #SYSTEMS)
    SYSTEMS= (#THING #PHYSOB #SYSTEMS)
    DETERMINER= (3. INDEF NIL)
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
   RELATIONS=  ((#IS $?UNKNOWN ?) )
    SYSTEMS= (#THING #SYSTEMS)
    DETERMINER= (NS INDEF WHICH)
    VARIABLE= UNKNOWN)

(DEFS WE-OSS
    OSSNODE= WE-OSS
    MARKERS= (#ANIMATE #THING #SYSTEMS)
    SYSTEMS= (#ANIMATE #THING #SYSTEMS)
    REFER= (:SHRDLU :FRIEND)
    AND= (FRIEND-OSS SHRDLU-OSS))

;;;=======>>> TEMPORARY PLACE FOR OSS-PROPERTY DEFS - MOVE WHEN APPROVED

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
     (RED #COLOR)
     (BLUE #COLOR)
     (GREEN #COLOR)
     (WHITE #COLOR)
     (BLACK #COLOR)
     (BIG #SIZE)
     (LITTLE #SIZE)
     (LARGE #SIZE)
     (SMALL #SIZE)
     (WIDE #WIDTH)
     (NARROW #WIDTH)
     (TALL #HEIGHT)
     (SHORT #HEIGHT)
     (THICK #THICKNESS)
     (THIN #THICKNESS))

§ anno/winograd/smspec

(declare (genprefix smspec))

;;;############################################################
;;;
;;;                         SMSPEC
;;;                    (SEMANTIC SPECIALISTS)
;;;
;;;############################################################

(DEFUN SMTIME NIL (ERT SMTIME NOT WRITTEN YET))

(DEFUN SMTIME2 NIL (ERT SMTIME2 NOT WRITTEN YET))

(DEFUN SMNEWNOUN NIL
       (OBJECT (MARKERS: (#NEWNOUN) PROCEDURE: ((#NEWWORD)))))

(DEFUN SMNEWPROPN NIL (SMSET (LIST (NEWCOPY 'NAME-OSS))))

(DEFUN SMCONJ NIL
       ;;FOR ALL CONJOINED THINGS -- IT CURRENTLY ONLY HANDLES THINGS
       ;;WHICH HAVE AN OSS OR RSS STRUCTURE AS THEIR SEMANTICS.  THIS
       ;;DOES NOT INCLUDE SINGLE WORDS OF MOST TYPES.  IT USES
       ;;RECURSION
       (PROG (%SM) (SMCONJ2 NIL H) (RETURN (SMSET %SM))))

(DEFUN SMCONJ2 (INTERPLIST RESTLIST)
       ;;INTERPLIST IS THE LIST OF INTERPRETATIONS FOR THE CONJUNCTS
       ;;HANDLED SO FAR -- THIS FUNCTION WILL BE CALLED ONCE FOR EACH
       ;;POSSIBLE COMBINATION.  THE MARKERS FOR THE CONJOINED
       ;;STRUCTURE ARE THOSE OF THE FIRST CONJUNCT -- NEEDS MORE
       ;;SOPHISTICATION.  RESTLIST IS THE REST OF NODES YET TO BE
       ;;HANDLED.
       (PROG (%X)
         (OR RESTLIST
         (RETURN (SETQ %SM
                   (CONS (BUILD RSSNODE=
                        (AND (RSS? INTERP)
                         (MAKESYM 'RSS))
                        OSSNODE=
                        (AND (OSS? INTERP)
                         (MAKESYM 'OSS))
                        MARKERS=
                        (MARKERS? INTERP)
                        SYSTEMS=
                        (SYSTEMS? INTERP)
                        REL=
                        (REL? INTERP)
                        AND=
                        (AND (OR (CQ BUT)
                             (CQ AND))
                         INTERPLIST)
                        OR=
                        (AND (OR (CQ OR) (CQ NOR))
                         INTERPLIST))
                     %SM))))
                                       ;WHEN THERE IS NO RESTLIST,
         (MAPCAR '(LAMBDA (INTERP) (SMCONJ2 (CONS INTERP
                              INTERPLIST)
                                       ;WE HAVE LOOPED TO THE END OF
                        (CDR RESTLIST)))
                                       ;THE LIST OF CONJUNCTS, AND
             (SM RESTLIST))))
                                       ;THE RESULTING INTERPRETATION IS OK. THE MAPPING
               ;IS DOWN THE LIST OF INTERPRETATIONS FOR A
               ;SINGLE CONJUNCT WHILE THE RECURSION GETS US
               ;DOWN THE LIST OF CONJUNCTS.  THUS WE GET EVERY
               ;POSSIBLE COMBINATION OF THE INTERPRETATIONS. --
               ;ISN'T LISP SUPER-DUPER-WONDERFUL! NOTICE THAT
               ;INTERP IS GETTING PICKED UP AS A FREE VARIABLE
               ;BY SMCONJ2, EVEN THOUGH IT IS BOUND ONLY INSIDE
               ;A MAPCAR INSIDE SMCONJ2. THIS WORKS BECAUSE THE
               ;CLAUSE CONTAINING IT CAN NEVER GET CALLED
               ;EXCEPT BY RECURSION,

(DEFUN SMVG NIL
       ;;CALLED INSIDE ANY VG
       (PROG (TSS TENSE)
         (SETQ TSS (GETR 'TIME (MOVE-PT C U (CLAUSE))))
         (AND (CQ NEG) (ADD-F-PT 'NEG PT))
                                       ;NEG IS TRANSFERRED FROM THE
         (SETQ TENSE (GETR 'TENSE C))
                                       ;VG TO THE CLAUSE IN WHICH IT
         (COND ((MEMBER TENSE
                '((PRESENT) (IMPER) (INFINITIVE)))
            T)
                                       ;IS EMBEDDED.
           ((EQUAL TENSE '(MODAL))
            (SETQ GLOBAL-MESSAGE '(THAT DOESN/'T
                        MAKE
                        ANY
                        SENSE
                        TO
                        ME/.))
            (ADD-F-PT 'MODAL PT))
                                       ;CLAUSES ARE ALSO MARKED AS
           ((AND (EQUAL TENSE '(FUTURE))
                                       ;MODAL.
             (ISQ PT QUEST)
             (EQUAL (REFER? (CAR (SM (GETR 'SUBJECT
                               PT))))
                '(:SHRDLU)))
                                       ; FUTURE QUESTIONS WITH "YOU"
            (SETQ TENSE '(PRESENT))
                                       ;SUBJECT IS REALLY IMPERATIVE
            (REMOVE-F-PT 'QUEST PT)
                                       ; THE CLAUSE IS NO LONGER
            (ADD-F-PT 'IMPER PT))
                                       ;QUESTION  BUT RATHER,
           ((SETDIF TENSE '(PAST PRESENT))
                                       ;IMPERATIVE
            (GLOBAL-ERR '(I DON/'T
                    KNOW
                    HOW
                    TO
                    HANDLE
                    TENSES
                    INVOLVING
                    FUTURE
                    EVENTS
                    OR
                    MODALS
                    OTHER
                    THAN
                    IN
                    THE
                    PRESENT))))
         (PUTPROP TSS TENSE 'TENSE=)
         (RETURN T)))

(DEFUN SMADJGQSHORT NIL (ERT SMADJQSHORT NOT WRITTEN YET))

(DEFUN SMPRON (NODE)
       (EVAL (SM NODE))
       (COND ((NULL SM)
          (SETQ GLOBAL-MESSAGE (APPEND '(I DON/'T KNOW WHAT ")
                       (FROM (NB H) (N H))
                       '(" REFERS TO)))))
       SM)

(DEFUN SMVAUX NIL
       (COND ((ISQ H NEG) (FQ NEG)) (T))
       (PUTPROP (GETR 'TIME C)
        (OR (MEET (FE H) '(PRESENT PAST MODAL))
            (ERTERR SMVAUX -- FUNNY TENSE))
        'TENSE=))

(DEFUN SMADV NIL (ERT SMADV NOT WRITTEN YET))

(DEFUN SMPLACE NIL (ERT SMPLACE NOT WRITTEN YET))

(DEFUN SMTOADJ NIL (ERT SMTOADJ (UNCT) NOT WRITTEN YET))

(DEFUN SMPROP NIL
       ;;THIS IS THE SEMANTICS FOR PROPER NOUNS.  IT PRODUCES TWO
       ;;INTERPRETATIONS.  ONE IS THE OPAQUE REFERENCE TO THE NAME
       ;;ITSELF, AS IN "CALL IT SAM".  THE OTHER IS THE TRANSPARENT
       ;;REFERENT AS IN "PICK UP SAM".
       (SMSET (LIST (BUILD OSSNODE=
               (MAKESYM 'OSS)
               VARIABLE=
               'NAME
               DETERMINER=
               (1. DEF NIL)
               PARSENODE=
               C
               MARKERS=
               (#NAME)
               REFER=
               (LIST (WORD (NB H))))
            (BUILD OSSNODE=
               (MAKESYM 'OSS)
               DETERMINER=
               (1. DEF NIL)
               PARSENODE=
               C
               VARIABLE=
               (MAKESYM 'X)
               RELATIONS=
               (LIST (LIST '#NAME
                       OSSNODE=
                       (WORD (NB H)))))))
       (SMNG2))

(DEFUN SMADJ (WORD-BEING-INTERPRETED)
       ;; THIS FUNCTION TAKES AS INPUT THE PARSE NODE FOR AN
       ;;ADJECTIVE - NOT COMPARATIVE OR SUPERLATIVE.  IT JUST EVAL'S
       ;;THE DEFINITION.  THAT DEFINITION (WHICH SHOULD BE AN NMEANS)
       ;;MAP'S DOWN THE LIST OF OSS'S IN THE FREE VARIABLE "SM".  IT
       ;;CHECKS FOR MARKER COMPATIBILITY AND ATTACHES ITS "PLANNER"
       ;;TO THAT OF THE OSS.  IT SHOULD MAKE UP NEW OSS'S IN CASE OF
       ;;MULTIPLE INTERPRETATIONS OF THE PHRASE.  THE OSS'S IT
       ;;CREATES ARE LEFT IN THE FREE VARIABLE "SM".  THIS FUNCTION
       ;;CALLED BY: SMNG1 IT NEEDS TO BE HAIRED UP FOR CONJOINED
       ;;ADJECTIVES LIKE "GREEN AND RED BALL".
       (EVAL (SM WORD-BEING-INTERPRETED)))
                                       ; EVALUATE THE DEFINITION OF THE ADJECTIVE

;;---------------------------------------------
;;;--------------------------------------------

(DEFUN SMADJG-PREPG NIL
                                       ;HANDLES ADJECTIVE GROUPS AND
       (PROG (X SMSUB)
                                       ;PREPGS BOTH AS COMPLEMENTS
         (AND (OR (CQ AGENT) (CQ OF)) (RETURN T))
                                       ;AND QUALIFIERS DO NOTHING
         (SETR 'LOGICAL-SUBJECT
                                       ;FOR "BY" PHRASES IN PASSIVE
           (COND ((CQ COMP)
              (GETR 'SUBJECT
                (MOVE-PT C U (CLAUSE))))
                                       ;CLAUSES OR "OF" PHRASES LIKE
             ((CQ LOBJ)
                                       ;IN THREE OF THE BLOCKS.
              (OR (GETR 'OBJ1
                    (MOVE-PT C U (CLAUSE)))
                                       ;SEMANTIC SUBJECT IS THE
                  (GETR 'SUBJECT PT)))
                                       ;SUBJECT OF AN INTENSIVE OR
             ((ISQ (MOVE-PT C
                    U
                    (NOT (ISQ PT COMPONENT))
                    U)
                   NG)
              PT)
                                       ;THE NG TO WHICH THE GROUP IS
             ((ISQ PT CLAUSE) PT)
                                       ;A QUALIFIER, OR THE CLAUSE
             ((ERTERR SMADJG-PREPG FUNNY POSITION)))
                                       ;OF WHICH IT IS AN ADJUNCT.
           C)
         (SETQ SMSUB (SM (GETR 'LOGICAL-SUBJECT C)))
         (AND (CQ ADJG)
          (GETR 'OBJ1 C)
          (SETR 'ADJGHEAD
            (COMPARE-BUILD (GETR 'HEAD C)
                       (COND ((CQ AS) '#ASMUCH)
                         ((CQ THAN) '#MORE)
                         ((ERTERR SMADJG-PREPG
                              FUNNY
                              TYPE))))
            C))
         (COND
          ((GETR 'OBJ1 C) (SMCL1) (RETURN SM))
          ((RETURN
        (SMSET
         (PROG (SM)
               (SMSET (MAPCAR
                   '(LAMBDA (OSS)
                    (BUILD OSSNODE=
                           (MAKESYM 'OSS)
                           MARKERS=
                           (MARKERS? OSS)
                           SYSTEMS=
                           (SYSTEMS? OSS)
                           VARIABLE=
                           (VARIABLE? OSS)
                           REFER=
                           (REFER? OSS)
                           REL=
                           OSS
                           REFER=
                           (REFER? OSS)
                           DETERMINER=
                           '(NS-PL INDEF NIL)))
                   SMSUB))
               (EVAL (COND ((OR (CQ COMPAR) (CQ SUP))
                    (FINDMEASURE (GETR 'HEAD
                               C)))
                   (T (SM (GETR 'HEAD C)))))
               (RETURN SM))))))))

;;;--------------------------------------------

(DEFUN SMIT (PRONOUN)
                                       ; PRONOUN IS (IT THEY ONE)
       (PROG (CANDIDATES AMBIGUITIES)
                                       ; A NODE LIST OF POSSIBLE
         (OR DISCOURSE (ERT SMIT: DISCOURSE SWITCH NOT ON))
                                       ;REFERENTS
         (AND MVB;IS THIS A "DO IT!" COMMAND?
          (ISQ MVB DO)
          (CQ OBJ1)
          (RETURN (SMSET LASTEVENT)))
                                       ;IF SO, RETURN THE LAST EVENT
         (COND ((GET PRONOUN 'BIND)
                                       ;MENTIONED IF THIS PRONOUN
            (MAP (FUNCTION (LAMBDA (BINDNODE)
                       (SMIT2 BINDNODE 0.)))
                                       ;HAS BEEN USED BEFORE IN THIS
             (GET PRONOUN 'BIND))
                                       ;SENTENCE THEN USE THE SAME
            (RETURN SM))
                                       ;CANDIDATES
           ((SMIT2 (GET PRONOUN 'LASTBIND) 0.)
                                       ; IF THIS PRONOUN WAS USED IN
            (GO DONE))
                                       ;THE PREVIOUS SENTENCE
           ((OR (MOVE-PT C U U (NG) U U (NG))
                                       ;LOOK FOR A STRUCTURE LIKE  "
            (MOVE-PT C U U (NG) U (COMP) PV (SUBJ)))
                                       ;A BLOCK WHICH IS TALLER THAN
            (SMIT2 PT 0.)
                                       ;ANYTHING WHICH SUPPORTS IT"
            (MOVE-PT C U U (NG))
                                       ;OR "A
            (COND ((ISQ PT DEF)
               (ADD-F-PT 'INDEF PT)
               (REMOVE-F-PT 'DEF PT)
               (MAPC '(LAMBDA (INTERP)
                      (PUTPROP INTERP
                           '((EXACTLY 1.)
                             INDEF
                             NIL)
                           'DETERMINER=))
                 (SM PT))))
            (RETURN SM))
                                       ;BLOCK TALLER THAN ANYTHING
           ((OR (MOVE-PT C U (BOUND) U)
                                       ;WHICH SUPPORTS IT"
            (MOVE-PT C
                 U
                 (AND (ISQ PT CLAUSE)
                      (ISQ PT COMPONENT))
                 U
                 DLC))
            (SMIT2 (GETR 'OBJ2 PT) 0.)
            (SMIT2 (GETR 'OBJ1 PT) 0.)
            (SMIT2 (GETR 'SUBJECT PT) 0.)
            (AND (NULL SM)
             (ISQ PT RSQ)
             (SMIT2 (GETR 'RELHEAD PT) 0.))
            (AND SM (RETURN SM))))
         (SMIT2 (GETR 'SUBJECT LASTSENT) 192.)
         (SMIT2 (PARSENODE? LASTREL) 128.)
                                       ;TRY REL (I.E. QUESTION FOCUS
         (MOVE-PT LASTSENT DLC)
                                       ;) OF THE LAST SENTENCE. GO
    UP   (COND ((NOT (MOVE-PT PV (NG))) (GO ON))
           (ELSE (SMIT2 PT 64.)))
                                       ;THROUGH TOP LEVEL NG'S OF
         (AND (MOVE-PT PV) (GO UP))
                                       ;LAST SENTENCE
    ON   (OR SM  ; IF WE HAVEN'T YET FOUND A
         (MAP (FUNCTION (LAMBDA (ANSNODE) (SMIT2 ANSNODE 0.)))
                                       ;REFERENT MAP DOWN THE ANSREF
              ANSNAME))
                                       ;(NG'S IN LAST ANSWER)
         (OR SM  ; IF WE HAVEN'T YET FOUND A
         (MAP
          (FUNCTION (LAMBDA (BACKNODE) (SMIT2 BACKNODE 0.)))
                                       ;REFERENT MAP DOWN THE
          BACKREF2))
                                       ;BACKREF2 (NG'S IN LAST
    DONE (PUTPROP PRONOUN CANDIDATES 'BIND)
                                       ;SENTENCE) LIST
         (OR (CDR SM) (REMPROP (CAR SM) 'AMBIGUITIES=))
         (RETURN SM)))

(DEFUN SMIT2 (NODE PLAUSIBILITY)
       (AND
    NODE           ; MAKE SURE NODE IS REALLY
    (GETR 'HEAD NODE)
                                       ;THERE  QUEST NODES (SUCH AS
    (NOT (MEMQ (CAR NODE) CANDIDATES))
                                       ;"WHAT") OR OTHER NODES WITHOUT HEAD NOUNS ARE
    (COND ((EQ PRONOUN 'IT)
                                       ;NOT SUITABLE FOR REFERENTS  MAKE SURE THAT NODE
           (AND (ISQ NODE NS) (NOT (ISQ NODE PRONG))))
                                       ;HASN'T ALREADY BEEN USED AS
          (ELSE (ISQ NODE NPL)))
                                       ;REFERENT  MAKE SURE NODE AND
    (SETQ CANDIDATES (CONS (CAR NODE) CANDIDATES))
                                       ;PRONOUN AGREE IN NUMBER
    (SMSET
     (NCONC
      (MAPCAR
       (FUNCTION
        (LAMBDA (REFERENT-OSS)
            (BUILD OSSNODE=
               (MAKESYM 'OSS)
               MARKERS=
               (MARKERS? REFERENT-OSS)
               SYSTEMS=
               (SYSTEMS? REFERENT-OSS)
               PLAUSIBILITY=
               PLAUSIBILITY
               AMBIGUITIES=
               (LIST (LIST OSSNODE=
                       (FROM (NB NODE) (N NODE))
                       C))
               REFER=
               (REFER? REFERENT-OSS)
               VARIABLE=
               (VARIABLE? REFERENT-OSS)
                                       ; INPUT PARAMETER
               PARSENODE=
               C
                                       ; USE THE REFERENT'S REFERENT
               DETERMINER=
                                       ;IF IT HAS ONE
               (LIST (COND ((ISQ C NPL) 'NPL)
                       ('NS))
                 'INDEF
                 NIL)
               RELATIONS=
               (LIST (LIST '#REFERS
                                       ; DONE SO THAT IF VARIBLE IS BOUND, PLANNER
                       (VARIABLE? REFERENT-OSS))))))
                                       ;GENERATOR WILL USE IT  RELATION SAYS
       (SM NODE));THAT THIS OSS "REFERS" TO
      SM))))     ;THE OSS WHOSE VARIABLE NAME IS GIVEN END OF BUILD

(DEFUN SMNGOF NIL
       ;; USED TO PROCESS NOUN GROUPS LIKE= "THREE OF THE BLOCKS"
       ;;"BOTH OF THEM"
       ;;;
       ;; SINCE THE OBJECT OF THE "OF" MUST BE
       ;;DEFINITE(SYNTACTICALLY) AND HAS ALREADY BEEN PROCESSED, THE
       ;;PLANNER CODE BUILT IS JUST A THAMONG EXPRESSION OF THE LIST
       ;;OF POSSIBLE REFERENTS OF THE "OF" OBJECT
       ;;;
       (SMSET
    (MAPBLAND
     (FUNCTION
      (LAMBDA (OFOSS)
          (BUILD OSSNODE=
             (MAKESYM 'OSS)
             VARIABLE=
             (VARIABLE? OFOSS)
             SYSTEMS=
             (SYSTEMS? OFOSS)
             MARKERS=
             (MARKERS? OFOSS)
             PARSENODE=
             C
             DETERMINER=
             (LIST (COND ((CQ NUM)
                      (SM (MOVE-PT H PV (NUM))))
                     ((ISQ NB BOTH) 2.)
                     ('NPL))
                   (COND ((MOVE-PT H PV (QNTFR))
                      (EVAL (SM PT)))
                     ('INDEF))
                   (COND ((CQ HOWMANY) 'HOWMANY)
                     ((CQ QDET) 'WHICH)))
             RELATIONS=
             (LIST (LIST 'THAMONG
                     (LIST 'THV
                       (VARIABLE? OFOSS))
                     (LIST 'QUOTE
                       (REFER? OFOSS)))))))
     (SM (MOVE-PT H DLC)))))
                                       ;MAP DOWN THE LIST OF "OF" OBJECT
                                       ;INTERPRETATIONS

;;;=============================================================

(DEFUN SMNG1 NIL

       ;;; SMNG1 IS CALLED AS SOON AS TJHE HEAD OF A NOUN GROUP IUS
       ;;PARSED.  IT FIRST BUILDS A SKELETON OSS CONTAINING ONLY THE
       ;;DETERMINERS AND ORDINALS.  IT THEN EVAL'S THE DICTIONARY
       ;;DEFINITION OF THE HEAD NOUN WHICH SHOULD BUILD OSS'S FOR
       ;;EACH POSSIBLE INTERPRETATION OF THE NOUN.  IT THEN CYCLES
       ;;THROUGH ALL THE GOODIES IN FROUNT OF THE HEAD NOUN, EVALING
       ;;THEIR DEFINITIONS.  THE FREE VARIABLE "SM" IS USED TO KEEP
       ;;THE LIST OF OSS'S DURING THIS ENTIRE PROCESS.  ; NOTE THE
       ;;SPECIAL HANDLING OF TPRONS (ANYTHING SOMETHING ETC.) AND OF
       ;;SUPERLATIVE AND COMPARATIVE ADJECTIVES.  ;
       (PROG (WORD-BEING-INTERPRETED DETERS)
         (SETQ DETERS
           (LIST (COND ((CQ NUMD)
                ((LAMBDA (NUM)
                     (EVAL (SM (MOVE-PT H
                                PV
                                (NUMD)))))
                 (SM (MOVE-PT H PV (NUM)))))
                   ((CQ NUM) (SM (MOVE-PT H PV (NUM))))
                   ((CQ NPL)
                (COND ((ISQ NB BOTH) 2.)
                      ((CQ NS) 'SG-PL)
                      ('NPL)))
                   ('NS))
             (COND ((CQ QNTFR)
                (EVAL (SM (MOVE-PT H PV (QNTFR)))))
                   ((CQ TPRON)
                (EVAL (SM (MOVE-PT H PV (TPRON)))))
                   ((CQ DEF) 'DEF)
                   ((CQ DET) 'INDEF)
                   ('NDET))
             (COND ((CQ HOWMANY) 'HOWMANY)
                   ((CQ QDET) 'WHICH))))

         ;;;
         (SMSET (LIST (BUILD OSSNODE=
                 (MAKESYM 'OSS)
                 PARSENODE=
                 C
                 VARIABLE=
                 (MAKESYM 'X)
                 MARKERS=
                 (AND (CQ TPRON)
                      '(#VAGUE #PHYSOB #THING))
                 RELATIONS=
                 (AND (CQ TPRON)
                      (LIST (LIST '#PHYSOB
                          OSSNODE=)))
                 DETERMINER=
                 DETERS)))
                                       ;BUILD AN INITIAL OSS
         (SETQ WORD-BEING-INTERPRETED H)
                                       ;SETUP TO LOOP THROUGH
         (COND ((ISQ H TPRON) (GO LOOP))
                                       ;ADJECTIVES IF ITS A TPRON,
           ((CQ INCOM) (SMONE) (GO LOOP)))
                                       ;ITS WAS EVALED ABOVE SO SKIP
         (SMSET (EVAL (SM WORD-BEING-INTERPRETED)))
                                       ;INCOMPLETES SUCH AS "PICK UP
    LOOP (COND ((NULL SM) (RETURN NIL)))
                                       ;TWO" EVAL THE HEAD NOUN IF
         (COND ((NULL (SETQ WORD-BEING-INTERPRETED
                (CDR WORD-BEING-INTERPRETED)))
                                       ;AN ADJECTIVE ELIMINATES ANY
            (RETURN SM))
                                       ;POSSIBLE INTERPRETATION FOR
           ((OR (ISQ WORD-BEING-INTERPRETED COMPAR)
                                       ;THIS NG, FAIL IF WE'VE
            (ISQ WORD-BEING-INTERPRETED SUP))
                                       ;LOOPED THRU ALL THE MODIFIERS,  THEN RETURN THE
            (EVAL (FINDMEASURE WORD-BEING-INTERPRETED))
                                       ;LIST OF POSSIBLE INTERPRETATIONS.  IF ITS A
            (GO LOOP))
                                       ;COMPARATIVE OR SUPERLATIVE
           ((OR (ISQ WORD-BEING-INTERPRETED ADJ)
                                       ;ADJECTIVE
            (ISQ WORD-BEING-INTERPRETED CLASF))
                                       ; IF ITS AN ADJECTIVE OR
            (SMADJ WORD-BEING-INTERPRETED)
                                       ;CLASSIFIER THEN EVAL THE
            (GO LOOP))
                                       ;DICTIONARY DEFINITION OF IT
           ((ISQ WORD-BEING-INTERPRETED POSS)
            (SMPOSS)
            (GO LOOP)))
         (GO LOOP)))

;;;=============================================================

(DEFUN SMNG2 NIL
       ;; CALLED FROM NG WHEN ALL QUALIFIERS HAVE BEEN FOUND.
       ;;BASICALLY, IT SAVE THE NG ON THE BACKREF(ERENCE) LIST, AND
       ;;CALLS SMNG3 (ON EACH POSSIBLE NG INTERPRETATION) TO EVAL ANY
       ;;DEFINITE NOUN GROUPS EG.  "THE RED BLOCK." AS USUAL, THE
       ;;INITIAL OSS LIST IS IN "SM" AND THE FINAL OSS LIST IS PUT IN
       ;;"SM" ;
       (AND (NOT (CQ ANSNAME))
                                       ; DON'T USE FAKEY ANSWER NAME
        (GETR 'HEAD C)
                                       ;NODES FOR REFERENCE  QUEST
        DISCOURSE;NODES ARE NOT SUITABLE
        (SETQ BACKREF (CONS (CAR C) BACKREF)))
                                       ;REFERENTS SAVE THIS NG AWAY
       (SMSET (MAPBLAND (FUNCTION SMNG3) SM)))
                                       ;FOR POSSIBLE LATER BACK REFERENCE  GO THRU ALL
               ;THE POSSIBLE INTERPRETATIONS OF THIS NOUN GROUP

(DEFUN SMNG3 (OSS)
       ;; TAKES AN OSS AS ARGUMENT AND TRIES TO FIND ITS REFERENCE IF
       ;;THE NOUN GROUP IS DEFINITE.  EXCEXT FOR SPECIAL "ONLY
       ;;DEFINITE" DEFINITES SUCH AS "THE RIGHT" AND "THE THING"
       (PROG (FINDER MUNG INTER LIST CANDIDATES UNBOUND)
         (COND ((NOT (EQ (QUANTIFIER? OSS) 'DEF))
            (RETURN OSS))
                                       ;IF ITS NOT DEFINITE OR IT
           ((REFER? OSS) (RETURN OSS))
                                       ;ALREADY HAS A REFERENT
           ((CQ ANSNAME) (RETURN OSS)))
                                       ;MARKED,  IF ITS KLUDGY
         (SETQ
          FINDER
          (PLNR-FINDIFY 'ALL
                                       ;ANSWER NAME, JUST RETURN IT
                (VARIABLE? OSS)
                                       ;JUST RETURN IT
                (LIST (VARIABLE? OSS))
                (PLNR-DESCRIBE (RELATIONS? OSS)
                       (VARIABLE? OSS)
                       (LIST (VARIABLE? OSS)))))
                                       ; BUILDS UP THFIND EXPRESSION
         (PUTPROP OSS FINDER 'PLNRCODE=)
         (SETQ WHO NIL)
    UP   (COND ((NOT (SETQ CANDIDATES (THVAL2 WHO FINDER)))
            (GO TOOFEW))
           ((NUMBERP (NUMBER? OSS))
            (COND ((LESSP (LENGTH CANDIDATES) (NUMBER? OSS))
               (GO TOOFEW))
              ((GREATERP (LENGTH CANDIDATES)
                     (NUMBER? OSS))
               (GO TOOMANY))))
           ((EQ (NUMBER? OSS) 'NS)
            (COND ((NULL CANDIDATES) (GO TOOFEW))
              ((CDR CANDIDATES) (GO TOOMANY))))
           ((MEMQ (NUMBER? OSS) '(NPL SG-PL)))
           ((ERT SMNG3= SCREWY NUMBER PROPERTY OF OSS)))

         ;;;
         (PUTPROP OSS CANDIDATES 'REFER=)
    DONE (RETURN OSS)

         ;;;
    TOOFEW       ; WE DIDN'T FIND ANY (OR
         (COND ((OR (NULL DISCOURSE) (NULL WHO))
                                       ;ENOUGH) REFERENTS FOR THE NG
            (SETQ GLOBAL-MESSAGE (APPEND '(I DON/'T
                             KNOW
                             WHAT
                             YOU
                             MEAN
                             BY
                             ")
                         (FROM NB N)
                         '("/.)))
            (RETURN NIL))
                                       ;IF WE AREN'T REMEMBERING
           ((MEMQ WHO '(HE NIL))
                                       ;SENTENCES, FORGET IT IF WE JUST TRIED TO FIND
            (SETQ GLOBAL-MESSAGE (APPEND '(I DON/'T
                             KNOW
                             WHICH)
                                       ;EVERYTHING (OR EVERYTHING
                         (CDR (FROM NB N))
                                       ;THAT "HE" KNOWS ABOUT)
                         '(YOU MEAN/.)))
                                       ;THEN FAIL
            (RETURN NIL)))
         (SETQ MUNG T)
                                       ; ELSE SET UP TO EXPAND THE
    TOOMANY      ;SENTENCES WE'RE LOOKING AT
         (AND (MEMQ WHO '(HE NIL))
          (SETQ FINDER (PLNR-MUNG FINDER CANDIDATES)))
                                       ;RESTRICT THE POSSIBLE
         (SETQ WHO
           (COND ((EQ WHO NIL) 'HE)
                                       ;REFERENTS TO BE AMUNG THE
             ((EQ WHO 'HE)
                                       ;LIST ALREADY FOUND
              (LIST (SUB1 LASTSENTNO) (ADD1 LASTSENTNO)))
             ((OR (NOT MUNG) (EQ (CAR WHO) 1.))
              (SETQ WHO 'HE)
              (GO TOOFEW))
             ((CONS (SUB1 (CAR WHO)) (CDR WHO)))))
         (SETQ MUNG NIL)
         (GO UP)))

(DEFUN SMONE NIL
       (PROG (CONTRAST X)
         (SETQ X H)
                                       ; SET  X TO DAUGHTERS OF
    GO   (COND ((SETQ CONTRAST (GET (ROOT (NB X))
                    'CONTRAST))
                                       ;CURRENT NODE
            (SETQ CONTRAST (LIST CONTRAST (ROOT (NB X)))))
           ((SETQ X (CDR X)) (GO GO)))
    UP   (OR (AND (MOVE-PT C U U (NG)) (SMONE2 (LIST (CAR PT))))
         (SMONE2 (PARSENODE? LASTREL))
         (SMONE2 BACKREF)
         (SMONE2 ANSNAME)
         (SMONE2 BACKREF2)
         (COND (CONTRAST (SETQ CONTRAST NIL) (GO UP)))
         (AND (MOVE-PT LASTSENT DLC PV (NG))
              (SMONE2 (LIST (CAR PT))))
         (ERT SMONE= CAN/'T FIND REFERENT FOR "ONE"))
         (RETURN SM)))

(DEFUN SMONE2 (X)
       ;; SMONE2 TAKES IN A LIST OF NOUN GROUP NODES AND TRIES TO SEE
       ;;IF ANY OF THOSE NOUN GROUPS COULD BE THE REFERENT FOR "ONE".
       (PROG (WORD-BEING-INTERPRETED)
    UP   (COND ((NULL X) (RETURN NIL))
                                       ;IF X IS EMPTY, FAIL
           ((SETQ WORD-BEING-INTERPRETED (SMONE3 X)))
                                       ;TRY TO SEE IF FIRST NG OF X
           (ELSE (SETQ X (CDR X)) (GO UP)))
                                       ;SATIFIES CONTRAST AND/OR COULD BE REFERENT
               ;ELSE TRY NEXT NG IN X
         ;; AT THIS POINT WORD-BEING-INTERPRETED IS (SHOULD BE) A
         ;;LIST A WORD NODES OF THE NG WHICH IS THE REFERENT FOR
         ;;"ONE" WE NOW PROCEED TO BUILD UP AN OSS FOR THE "ONE"
         ;;NG THE LIST IS IN ORDER(NOUN ADJ ...  ADJ ETC NUM DET)
         ;;ONLY THE NOUN AND THE ADJ'S ARE USED
         (OR (ISQ WORD-BEING-INTERPRETED NOUN)
         (BUG SMONE2: REFERENT OF "ONE" IS SCREWED UP))
         (EVAL (SM WORD-BEING-INTERPRETED))
                                       ; EVAL THE NOUN DEFINITION
    GO   (AND
          (SETQ WORD-BEING-INTERPRETED
            (CDR WORD-BEING-INTERPRETED))
          (ISQ WORD-BEING-INTERPRETED ADJ)
                                       ; IF WE REACHED END OF
          (EVAL (SM WORD-BEING-INTERPRETED))
                                       ;ADJECTIVES, STOP
          (GO GO))
         (RETURN SM)))

(DEFUN SMONE3 (ONENG)
       ;; SMONE3 TAKES AN NG WHICH IS A POSSIBLE REFERENT FOR "ONE".
       ;;IT FIRST CUTS THE NG TO BE ONLY (NOUN ADJ ...  ADJ ETC) I.E.
       ;;IT STRIPS OF QUALIFYING PHRASES.  IF THERE IS NO CONTRAST
       ;;THEN THIS MUNGED NG IS RETURNED AS THE REFERENT.  IF THERE
       ;;IS A CONTRAST, PEN IT CHECKS T SEE IF THE NG SATISFIES
       ;;THAT CONTRAST.
       (PROG (NGWORDS X)
         (OR (ISQ ONENG NG)
         (BUG SMONE3: ONE REFERENT IS NOT A NG))
         (SETQ NGWORDS (H ONENG))
    LOOP (COND ((NULL NGWORDS) (RETURN NIL))
                                       (!AIL IF G HAS NK NCUJ HE@
         ! ((ISQ@GQORDS FOUN))
                                       ; IF FIND NOUN HEAD OF NG,
           (ELSE (SETQ FGWORDS (CDR NGWORDS)) (GO LOOP)))
                                    ;WIN
         (OR CONTRAST (RETURN NGWORDS))
                                       ; IF PERE IQ NO CONTRAST,
         (SDQ X (REVE@SE NGWORDS))
                                       ;REFERENT WINS BY DEFAULT
    LOOK (COND ((AND (EQ (AR CONTRAST)
                 (GET (ROOT (NB X)) 'CONTRAST))
             (NAT (EQ (CADR CONTRAST) (@OOT (NB X) )))
            (RETURN (EVERSE (CDR X))))
           ((SETQ X (CDR X)) (GO LOOK))
           (ELSE (RETURN NIL)))))
                                       ; FAIL IF NO WORD SUPPLYS CONTRAST

(DEFUN SMPOSS NIL
       (PROG (X)
         (RETURN (AND (SETQ X (SMOSS2 C (MOVE-PT H PV (POSS))))
              (SMRELATE X)))))

(DEFUN SMPORS2 (HEADNODE MODNODE)
       (PROG (X SM SMSUB SMOB1 SMOB2 SMOBL SLOL RELLIST)
         (SETQ SMRUB (SM MODNODE)
         (SETQ RELLIST (RETQ AMOB1 (SM HEADNKDE)))
         (SMSET '(#HAVE))
         (RETTN (AND SM
              (SETQ X (MAKESYM 'NODE))
              (PUTPROP X SM 'SEMANTICS)
              (LIST X)))))

;; SMPOSS WORKS BY ACTING LIKE SMCL1 AND SETTING UP AN RSS (HAVE X Y) .  NODE IS THE NODE OF THE POSSESSIVE
;;WHICH HAS ALREADY BEEN SEMANTICALLY PROCESSED.  ITS SM CONTAINS THE OSS'S FOR WHOSE DOING THE POSSESSING.
;;THE SM CURRENTLY ACTIVE IS THE THING BEING POSSESSED.
;;;=============================================================

(DEFUN SMRELATE (NODE)
       ;; RELATES A DESCRIPTIVE RSS TO ANOTHER RSS OR OSS ADDING IT
       ;;TO THE LIST OF RELATIONS.  IT TAKES THE LIST OF SS IN SM,
       ;;AND REPLACES SM WITH THE NEW LIST OF MODIFIED SS"S.  THE
       ;;MODIFYING RSS"S HAVE TO HAVE ONE OF THE SM SS"S AS A REL
       ;;(WHICH SHOULD ALWAYS BE TRUE IB THE WERE SET UP PROPERLY).
       ((LAMBDA (X) (AND X (SASET @)))
    (MAPCAR
     '(LAMBDA (RPS)
       (PPCG (PL) 
         (OH (MEMQ (SETQ REL (REL? RSS)) SM)
             (ERTERR SM@ELATE - TO WHOM?))
         (RETURN (BUILD OSSNODE=
                (AND (OSS? REL) (MAKESYM 'OSS))
                RSSJODE=
                (AND (RSS? REL) (MAKEAYM 'RSS))
                MARKERS=
                (OR (AND (RELMARKERS? RSS)
                     (CAR (RELMARKERS? RSS)))
                    (MARKERS? REL))
                SYSTEMS=
                (OR (AND (RELMARKERS? RSS)
                     (CADR (RELMARKERS? RSS)))
                    (SYSTEMS? REL))
                PLAUSIBILITY=
                (PLAUSIBILITY? RSS)
                PARSENODE=
                (PARSENODE? REL)
                AMBIGUITIES=
                (AMBIGUITIES? RSS)
                VARIABLE=
                (VARIABLE? REL)
                NEGATIVE=
                (NEGATIVE? REL)
                DETERMINER=
                (DETERMINER? REL)
                RELATIONS=
                (CONS RSS (RELATIONS? REL))
                REL=
                (REL? REL)))))
     (SM NODE))))

;;;------------------------------------------------------

(DEFUN SMCL1 NIL
       (PROG (SMSUB SMOB1 SMOB2 SMOBL SMCOMP RELLIST)

         ;;;
         ;;;        SET UP GLOBAL VARIABLES WHICH CONSIST OF POINTERS TO THE SEMANTIC DEFINITIONS
         ;;;     OF THE VARIOUS NOUN-GROUPS (ALSO RSNG'S) REQUIRED BY THE TRANSITIVITY OF THE VERB
         (SETQ SMSUB
           (COND ((SETQ SMSUB (GETR 'LOGICAL-SUBJECT C))
              (SM SMSUB))
             ((CQ IMPER) '(SHRDLU-OSS))
             ((NOT (CQ PASV))
              (SM (OR (GETR 'SUBJECT C)
                  (ERTERR SMCL1 -- NO SUBJECT))))
             ((CQ AGENT) (ERTERR SMCL1 -- AGENT MISSING))
             ('(UNKNOWN-OSS-BY))))
         (SETQ SMOB1 (SM (COND ((CQ PASV)
                    (GETR 'SUBJECT C))
                   ((GETR 'OBJ1 C)))))
         (SETQ SMOB2 (SM (GETR 'OBJ2 C)))
         (SETQ SMOBL (SM (GETR 'LOBJ C)))
         (SETQ SMCOMP (SM (GETR 'COMP C)))
                                       ;NATURALLY SEVERAL OF THESE
         (OR SMSUB
                                       ;GLOBAL VARIABLES (BOUND IN
         (AND (MEET '(THERE ITRNS) FE)
                                       ;THIS PROG AND ACCESSED IN
              (GO CHECK)))
                                       ;DEEPER ONES) ARE NIL AT THIS POINT IN THE
               ;PROCEDURE. THE FOLLOWING CHECKS ARE PRIMARILY
         (OR SMOB1
                                       ;FOR DEBUGGING PURPOSES (HENSE THE "ERT")
         (AND (OR (CQ TRANS) (NOT (CQ CLAUSE))) (GO CHECK)))
                                       ;TO INSURE THAT THE NON-NIL
         (OR (AND SMOB1 SMOB2) (AND (CQ TRANS2) (GO CHECK)))
                                       ;REGISTERS AND THE
         (OR (AND SMOB1 SMOBL) (AND (CQ TRANSL) (GO CHECK)))
                                       ;TRANSITIVITY OF THE VERB ARE
         (OR SMCOMP (AND (CQ INT) (GO CHECK)))
                                       ;BEING MATCHED IN EVERY CASE.
         (GO REL)
    CHECK(ERT BUG: SMCL1 TRANSITIVITY)
    REL  (SETQ RELLIST
           (SM (COND ((CQ RSQ) (GETR 'RELHEAD C))
                 ((OR (CQ PREPG) (CQ ADJG))
                  (GETR 'LOGICAL-SUBJECT C))
                 ((CQ QUEST) (GETR 'RELHEAD C)))))
         (AND (NOT RELLIST)
          (OR (CQ POLAR) (CQ DECLAR))
          (SETQ X (RELFIND C))
          (OR (EQUAL X SMSUB)
              (EQUAL X SMOB1)
                                       ; FIND RELATIVE ELEMENT FOR
              (EQUAL X SMOB2)
                                       ;POLAR CLAUSES WHICH CONTAIN INDEFINITE. APPLIES
              (EQUAL X SMOBL)
                                       ;TO TOPLEVEL CLAUSES SINCE ONLY THEY CAN HAVE
              (EQUAL X SMCOMP)
                                       ;FEATURES POLAR OR DECLAR.
              (ERTERR SMCL1 -- POLAR REL DOESN/'T MATCH))
          (SETQ RELLIST X))

         ;;;
         (SETQ TIME (GETR 'TIME (MOVE-PT C U (CLAUSE))))

         ;;;
         (SETQ SENSE-OF-VERB
                                       ;THIS REFERS TO THE SEMANTIC
           (COND ((CQ PREPG)
                                       ;SENSE OF THE VERB
              (SM (SETQ WORD-BEING-INTERPRETED
                    (GETR 'HEAD C))))
             ((CQ ADJG)
              (SM (SETQ WORD-BEING-INTERPRETED
                    (GETR 'ADJGHEAD C))))
             ((CADR (ASSQ (CAR (MEET FE
                         '(ITRNS TRANS
                                       ;WHICH WILL PROPABLY  VARY WITH ITS
               ;TRANSITIVITY. THE VALUE THAT IS FINALLY
                             INT
                                       ;DETERMINED REPRESENTS ALL POSSIBLE SENSES OF
               ;THE MEANING OF THE WORD THAT ARE APPROPRIATE TO
                             TRANSL
                                       ;THE TRANSITIVITY FIGURED OUT BY THE SYNTACTIC
                             TRANS2
                                       ;PROGRAMS
                             THERE
                             ITRNSL)))
                      (SM (SETQ WORD-BEING-INTERPRETED
                        (GETR 'MVB
                              C))))))))
         (SMSET (EVAL SENSE-OF-VERB))
                                       ;THIS DETERMINES THE APPROPRIATE SEMANTIC
               ;INTERPRETATION(S) FOR THE CLAUSE BY CHECKING
               ;THE RESTRICTIONS OF EACH DEFINITION AGAINST THE
               ;MARKERS OF THE VARIOUS CANDIDATES FOR SMSUB,
               ;SMOB1, ETC.  THE VALUE OF THE EVALUATION IS A
               ;LIST OF RELAT ION-SEMANTIC-STRUCTURES, ONE FOR
               ;EACH PLAUSIBLE INTERPRETATION

         ;;; ;
         (MAP (FUNCTION SMCL-MODIFIERS) H)
                                       ;SMCL-MODIFIERS WILL EXAMINE
         (RETURN SM)))
                                       ;ALL OF THE CONSTITUENTS OF THE CLAUSE THAT WERE
               ;NOT INVOLVED IN THE BUILDRSS AND WILL EVALUATE
               ;THE MEANINGS OF EACH IN TURN FOR THEIR EFFECT
               ;ON THE ESTABLISHED SM, THE PARSING TREE, OR
               ;ANYTHINGELSE THAT WOULD BE APPROPRIATE THE
               ;VALUE OF SMCL1 IS NON-NIL ONLY IF SOME
               ;REASONABLE MEANING HAS BEEN FOUND FOR THE
                                       ;CLAUSE

(DEFUN SMCL2 NIL
       ;; THERE USED TO BE A CALL TO SMPREPREL AT THIS POINT, BUT IT
       ;;HAS GONE AWAY PENDING FURTHER THOUGHT.
       (MAP (FUNCTION SMCL-MODIFIERS) H)
                                       ;AS IN SMCL1 WE NEED TO SCAN THE CONSTITUENTS OF
               ;THE CLAUSE AND ALLOW THEM TO MAKE WHATEVER
               ;MODIFICATION ARE APPROPRIATE

       ;;;
       )

(DEFUN SMCL-MODIFIERS (WORD-BEING-INTERPRETED)
                                       ;AS IN CONSTITUENT
       ;; THIS PROCEDURE IS BASICLY ONE LARGE DISPATCH TABLE WHICH
       ;;ARRANGES THAT THE PROPER KIND OF PROCESSING HAPPEN TO THE
       ;;APPROPRIATE CONSTITUENT.  SOME SHOULD BE IGNORED SINCE THEY
       ;;HAVE ALREADY BEEN DEALT WITH AND OTHERS SHOULD BE EVALUATED
       ;;AS MODIFIERS OR FOR THEIR SIDE-EFFECTS ;
       (COND ((NULL (GET WORD-BEING-INTERPRETED 'FEATURES)))
                                       ;IF THE CONSTITUENT HAS A NULL FEATURE LIST THEN
;;;         ((OR (ISQ WORD-BEING-INTERPRETED VG)
;;;                                       ;IT IS A FUNCTION WORD (IE. (PARSE
;;;          (ISQ WORD-BEING-INTERPRETED AUX))
;;;                                       ;NIL FOR)) WHICH SHOULD BE
;;;          (AND (ISQ WORD-BEING-INTERPRETED NEG)
;;;                                       ;IGNORED
;;;           (FQ NEG)
;;;           (buildrss word-being-interpreted
;;;                 'neg 'neg)))
;;;                                       ;THIS HAS THEEFFECT OF CHECKING IFTHEVERB IS
;;;               ;NEGATIVE AND THEN ARRANGING THAT THE FEAURE
         ((MEET FE '(THERE LOBJ COMP PRT SUBJ OBJ1 OBJ2)))
                                       ;LIST OF THEWHOLE CLAUSE AND OF ITS MEANING
         ((ISQ WORD-BEING-INTERPRETED NG)
                                       ;AGREE. ******* MAYBE SOMEONE ELSE SHOULD DO IT
          (AND (COND ((ISQ WORD-BEING-INTERPRETED TIM))
                                       ;?????????????? IGNORE ALL CONSTITUENTS WITH
             ((AND (CQ REL-NOT-FOUND)
                                       ;THESE FEATURES SKIPS TO THE OTHER
                   (ISQ WORD-BEING-INTERPRETED QUEST)
                                       ;PART OF THE "AND" TO CALL ANOTHER SEMANTIC
                   (ISQ (H WORD-BEING-INTERPRETED) TIM1))
                                       ;SPECIALIST CF. REFERENCE IN CLAUSE.SEC
              (RQ REL-NOT-FOUND)
                                       ;EG. "DAY" IN "WHAT DAY
              (FQ TIMEQ))
                                       ;IS..." TIE UP AS SYNTACTIC LOOSE END
             ;;GIVE IT A REFERENCE PROP.  -WHY ?????????
             )
           (SMTIME)))
         ((ISQ WORD-BEING-INTERPRETED PREPG)
          (OR (ISQ WORD-BEING-INTERPRETED AGENT)
                                       ;IN WHICH CASE IT WAS ALREADY
                                       ;PROCESSED MIGHT GO AWAY IN A
          (ERT SMCL-MOD BADPREPG)))
                                       ;FEW DAYS BUG CHATCHER
         ((ISQ WORD-BEING-INTERPRETED QADJ)
          (OR (MEET FE '(LOBJ COMPQ))
          (EVAL (SM WORD-BEING-INTERPRETED))))
                                       ;MIGHT WANT TO CHANGE THAT

         ;;;
         ((ISQ WORD-BEING-INTERPRETED BOUND))
                                       ; THE REST ARE HOOKS FOR WHEN
         ((ISQ WORD-BEING-INTERPRETED BINDER))
                                       ;WE FIGURE OUTWHATTO DO WITH
         ((ISQ WORD-BEING-INTERPRETED QUEST))
                                       ;THEM
         ((ISQ WORD-BEING-INTERPRETED CLAUSE))
         ((ERT SMCL-MODIFIERS ATTEMPTED TO PROCESS AN
           UNEXPECTED TYPE OF CONSTITUENT))))

(DEFUN SMBIND NIL
       (PROG (TSS EVENT START END)
         (AND (CDR (SM H))
                                       ;does the sm have more than
          (ERT I
               DON/'T
               KNOW
               WHAT
               TO
               DO
               WITH
               AMBIGUOUS
               BOUND
               CLAUSES))
                                       ;one value???
         (COND ((ISQ (MOVE-PT H DF) TIME)
                                       ;dispatch table to match the appropriate action
               ;with each binder  move to the first word of the
               ;clause (to the binder) and check for the
                                       ;feature time
            ;;(maybe also check for the sm being marked as an
            ;;event???)
            (SETQ TSS (GETR 'TIME C))
            (OR (SETQ EVENT (FINDEVENTS (CAR (SM H))))
            (GLOBAL-ERR '(NO SUCH THING EVER HAPPENED)))
            (SETQ EVENT (CAR EVENT))
            (SETQ START (GET EVENT 'START))
            (SETQ END (GET EVENT 'END))
            (EVAL (SM PT))
            (RETURN T)))))

(DEFUN SMBINDER (START-EV END-EV)
       ;;CALLED FOR A ABINDER - THE FIRST ARGUMENT GIVES THE
       ;;BEGINNING, SECOND THE END. A TYPICAL USE IS THE DEFINITION
       ;;OF "AFTER", WHICH IS (SMBINDER END NIL) I.E.  THE EVENT
       ;;STARTS AFTER THE END OF THE BOUND EVENT, WITH NO
       ;;SPECIFICATION ON WHEN IT ENDS.
       (PUTPROP TSS START-EV 'START=)
       (PUTPROP TSS END-EV 'END=))

§ anno/winograd/smutil

(declare (genprefix smutil))

;;;############################################################
;;;
;;;                         SMUTIL
;;;
;;;############################################################

(DEFUN ATOMIFY (X) (COND ((ATOM X) X) ((CDR X) X) (T (CAR X))))

(DEFUN ISTENSE (NODE ARG)
       ;;CHECKS VARIOUS THINGS ABOUT TENSE.
       (PROG (X)
         (OR (SETQ X (GETR 'TIME NODE))
         (ERT ISTENSE -- NO TIME REGISTER))
         (OR (SETQ X (TENSE? X)) (ERT ISTENSE -- NO TENSE))
         (RETURN (COND ((EQ ARG 'PRESENT)
                (MEMBER X
                    '((PRESENT) (PRESENT PRESENT))))
               ((EQ ARG 'FUTURE)
                (EQUAL X '(FUTURE)))
               ((EQ ARG 'MODAL)
                (EQUAL X '(MODAL)))
               ((EQ ARG 'IMPERF)
                (AND (CDR X)
                 (EQ (CAR X) 'PRESENT)))
               (T (ERT ISTENSE -- FUNNY ARG))))))

(DEFUN IMPERF? (TSS)
       (AND (CDR (SETQ X (TENSE? X))) (EQ (CAR X) 'PRESENT)))

(DEFUN IMPERF NIL (ISTENSE C 'IMPERF))

(DEFUN BUILD FEXPR (%L)
       ;;BUILD CONSTRUCTS AN OSS OR AN RSS FROM ITS ARGUMENTS.
       ;;;
       ;;;INPUTS:
       ;; A LIST OF KEYWORDS FOLLOWED BY THEIR VALUES.  EVERY OTHER
       ;;ARGUMENT IS EVALUATED (THAT IS KEYWORDS SHOULD NOT BE
       ;;QUOTED).  POSSIBLE KEYWORDS:
       ;;;     MARKERS=         <LIST OF SEMANTIC MARKERS>
       ;;;     SYSTEMS=         <LIST OF SYSTEMS FOR THOSE MARKERS--USED FOR FAST
       ;;;                          MARKER CHECKING?>
       ;;;     PLAUSIBILITY=    <INTEGER BETWEEN 0 AND 1000 USED IN DISAMBIGNUATION>
       ;;;     RSSNODE=         <NODE NAME FOR AN RSS>
       ;;;     OSSNODE=         <NODE NAME FOR AN OSS>
       ;;;     PARSENODE=      <CORRESPONDING NODE OF PARSE TREE>
       ;;;     VARIABLE=          <NAME OF VARIABLE TO BE USED IN BUILDING PLANNER
       ;;;                      CODE>
       ;;;     DETERMINER=             <DETERMINER INFORMATION - A 3-LIST>
       ;;;     RELATIONS=
       ;;;     AMBIGUITIES=     <LIST OF POTENTIAL AMBIGUITIES FOR THIS INTERPRETATION>
       ;;;     AND=             <LIST OF CONJUNCTS>
       ;;;     OR=              <LIST OF DISJUNCTS>
       ;;;
       ;;;VALUE:
       ;; THE NODE NAME OF THE OSS OR RSS CONSTRUCTED.  AN ATOM.
       ;;;
       ;;;SIDE-EFFECTS:
       ;; USES PUTPROP TO ATTACH PROPERTIES TO NODE NAMES
       ;;;
       ;;;
       (PROG (%X NEGATIVE= REFER= PLNRCODE= MARKERS= SYSTEMS=
          TENSE= TSSNODE= RELMARKERS= ANSNODE= ACTION= ANSRSS=
          PLAUSIBILITY= DETERMINER= AND= OR= AMBIGUITIES=
          RELATIONS= VARIABLE= VARLIST= REL= RSSNODE=
          PARSENODE= OSSNODE= NODE= %PROPS)
         (SETQQCHECK T
             %L
             (SETQ %PROPS '(NEGATIVE= REFER=
                          PLNRCODE=
                          MARKERS=
                          SYSTEMS=
                          RELMARKERS=
                          PLAUSIBILITY=
                          DETERMINER=
                          AND=
                          OR=
                          AMBIGUITIES=
                          RELATIONS=
                          VARIABLE=
                          VARLIST=
                          REL=
                          RSSNODE=
                          PARSENODE=
                          OSSNODE=
                          TSSNODE=
                          TENSE=
                          ANSNODE=
                          ANSRSS=
                          ACTION=))
             'BUILD)
         (AND RSSNODE=
          (NOT MARKERS=)
          (SETQ MARKERS= '(#RELATION)))
         (AND MARKERS=
          (NOT SYSTEMS=)
          (SETQ %X (CHECK MARKERS= NIL NIL))
          (SETQ MARKERS= (CAR %X))
          (SETQ SYSTEMS= (CADR %X)))
         (SETQ NODE= (OR OSSNODE=
                 RSSNODE=
                 TSSNODE=
                 ANSNODE=
                 (ERT /././.BUILD: NO NODE=)))
         (MAPC '(LAMBDA (%PROP) (AND (SETQ %X (EVAL %PROP))
                     (PUTPROP NODE= %X %PROP)))
           %PROPS)
         (AND BUILD-SEE ((LAMBDA (DPSTOP) (DP NODE=)) SMN-STOP))
         (RETURN NODE=)))

(DEFUN NEWCOPY (OSS)
       (PROG (OLD NEW)
         (SETQ NEW (MAKESYM 'OSS))
         (SETQ OLD (CDR OSS))
                                       ;WATCH OUT -- THIS IS IMPLEMENTATION DEPENDENT,
    UP   (COND ((NULL OLD)
                                       ;AND GETS THE ENTIRE PROPERTY LIST IN OUR LISP.
            (PUTPROP NEW C 'PARSENODE=)
            (RETURN NEW))
           ((EQ (CAR OLD) 'PNAME))
           ((PUTPROP NEW (CADR OLD) (CAR OLD))))
         (SETQ OLD (CDDR OLD))
         (GO UP)))

;;;=============================================================================

(DEFUN RELATION FEXPR (%DEFL)

       ;;; CONSTRUCTS RSS'S FOR GARDEN VARIETY VERBS.  USED IN DEFINITION OF SAME.
       ;;;
       ;;;INPUTS:
       ;; INPUTS ARE IN KEYWORD FORMAT (ATTRIBUTE-VALUE PAIRS).  THE
       ;;KEYWORD IS NOT EVALUATED BUT ITS VALUE IS.  POSSIBLE
       ;;KEYWORDS:
       ;;;     RESTRICTIONS:     LIST OF RESTRICTIONS ON VARIOUS SEMANTIC REGISTERS
       ;;;                       EACH RESTRICTION (CALLED %MARL FOR MARKER LIST
       ;;;                       IN THE CODE) IS A LIST
       ;;;                          EITHER WHOSE CAR IS A REGISTER NAME (E.G. SMSUB)
       ;;;                                 AND WHOSE CADR IS A LIST OF MARKERS
       ;;;                             OR WHOSE CAR IS A LIST OF MARKERS IN WHICH
       ;;;                                CASE
       ;;;                                THE ASSOCIATED REGISTER NAME IS DETERMINED
       ;;;                                BY THE POSITION IN RESTRICTIONS:.
       ;;;                                   SMSUB FOR CAR
       ;;;                                   SMOB1 FOR CADR
       ;;;                                   SMOB2 FOR CADDR
       ;;;     PROCEDURE:        CONDENSED PLANNER CODE SCHEMA
       ;;;     MARKERS:          SEMANTIC MARKERS
       ;;;     PLAUSIBILITY:     EVALUATED TO GET INTEGER FROM 0 TO 1000 INDICATING RELATIVE
       ;;;                          LIKLIHOOD OF THIS DEFINITION SENSE.
       ;;;     REL:                ******>>>
       ;;;
       ;;;VALUE:
       ;;;     LIST OF RSS NODE NAMES CREATED
       ;;;
       ;;;SIDE-EFFECTS:
       ;;;     CREATES AN RSS BY CALLING BUILD
       (ITERATE
    '(LAMBDA ARGLIST
      (PROG (SMCOMP SMSUB SMOB1 SMOB2 SMOBL MARKERS:
         RESTRICTIONS: PLAUSIBILITY: REL: PARAPHRASE:
         RELMARKERS: RSSNAME PROCEDURE: #1 #2 #3 %NEWRSS
         %OSSNODE)
        (SETQ %DEF
              (ARG 1.)
              SMSUB
              (ARG 2.)
              SMOB1
              (ARG 3.)
              SMOB2
              (ARG 4.)
              SMOBL
              (ARG 5.)
              SMCOMP
              (ARG 6.))
                                       ;AN LEXPR IS USED HERE IN ORDER TO GET AROUND
               ;THE LIMITATION OF FIVE EXPR ARGUMENTS IN
               ;COMPILED CODE.   NOTICE THAT WITHIN THIS LAMBDA
               ;EXPRESSION THAT  SMSUB = ONE OSS FOR SEMANTIC
               ;SUBJECT SMOB1 = ONE OSS FOR SEMANTIC OBJECT 1
               ;SMOB2 = ONE OSS FOR SEMANTIC OBJECT 2 SMOBL =
               ;ONE OSS FOR LOCATIVE OBJECT SMCOMP = ONE OSS
               ;FOR SEMANTIC COMPLEMENT WHEREAS OUTSIDE OF THE
               ;LAMBDA EXPRESSION EACH OF THESE NAMES
               ;REPRESENTS A LIST OF THE SAME. THIS IS TO ALLOW
        (SETQQCHECK NIL
                                       ;DICTIONARY WRITERS TO USE THESE SELF SAME NAMES
                %DEF
                                       ;IN WRITING DEFINITIONS, A SIMPLY TERRIBLE IDEA.
                '(RESTRICTIONS: PROCEDURE:
                        PLAUSIBILITY:
                        PARAPHRASE:
                        MARKERS:)
                'RELATION)
                                       ;(EVAL ...) DECODES KEYWORD ARGUMENTS. SETQQ
                                       ;EFFECTIVLY QUOTES BOTH PAIRS
        ;; RESTRICTIONS: IS EXPANDED HERE PUTING IN IMPLICIT
        ;;REGISTER REFERENCES SO THAT IT CAN BE UNIFORMLY
        ;;GOBBLED BELOW
        (SETQ RESTRICTIONS:
              (MAPCAR '(LAMBDA (%RESTRICTNAM %MARKL %NUM)
                                       ;MARKL IS A SINGLE MARKER LIST FROM ON OF THE
                       ((LAMBDA (X)
                        (SET %NUM
                             (EVAL (CAR X)))
                        X)
                    (COND ((ATOM (CAR %MARKL))
                                       ;RESTRICTIONS IN THE DEFINITION, E.G. (#PHYSOB
                           %MARKL)
                                       ;#RED).    %RESTRICTNAM IS A NAME LIKE SMSUB,
                          ((CONS %RESTRICTNAM
                                       ;SMOBL, SMCOMP, .... WHICH REFERS TO REGISTERS
                             %MARKL)))))
                  '(SMSUB SMOB1 SMOB2)
                  RESTRICTIONS:
                  '(#1 #2 #3)))
                                       ;ELSEWHERE IN THE PROGRAM WHOSE MARKERS MUST BE
        (AND ;COMPATIBLE WITH %MARKL AS CHECKED BELOW.%NUM IS
               ;THE NUMBER WHICH WILL BE USED TO SUBSTITUTE IN
                                       ;THE DICTIONARY EXPRESSION.
         ;; CHECK THAT THIS DEFINITION SENSE MEETS ALL OF THE
         ;;RESTRICTIONS SET FORTH IN THE DEFINITION UNDER
         ;;RESTRICTIONS:.
         (ERRSET
          (MAPC
           '(LAMBDA (%MARKL)
                                       ;ENCLOSED IN A ERRSET SO THAT THE FAILURE OF A
             (PROG (OSS X CHECK)
               (SETQ OSS (EVAL (CAR %MARKL)))
               (AND (SETQ X (CHECKREL OSS))
                (SETQ REL: (CAR X)))
               (COND
                ((NOT (AND (OR (NULL (CDDR %MARKL))
                                       ;CHECK CAN CAUSE IMMEDIATE ESCAPE FROM THE MAPC
                       (EVAL (CADDR %MARKL)))
                       (SETQ CHECK
                         (CHECK (CADR %MARKL)
                                       ;AND THENCE TO THE AND WHICH CUTS OFF ALL
                            (MARKERS? OSS)
                                       ;FURTHER PROCESSING OF THIS DEFINITION SENSE
                            (SYSTEMS? OSS)))))
                                       ;TEMPORARY STORAGE ON THE PROPERTY LIST OF TEMP
                 (ERR NIL))
                                       ;USED TO AVOID SEARCHING FOR THESE ITEMS ON THE
                ((EQ OSS REL:)
                                       ;ONE HAND OR THE CONFLICT OF NAMES BETWEEN THE
                 (SETQ RELMARKERS: CHECK)))))
                                       ;THE MARKERS RESULTING FROM CHECKING THE REL ARE
           RESTRICTIONS:))
                                       ;SAVED TO PUT ON IT LATER WHEN THE CLAUSE IS
               ;RELATED. SUBJECT RESTRICTION MARKERS USED IN
               ;THE DEFINITION AND THE REGISTERS OF THE SAME
               ;NAME REFERENCED AS FREE VARIABLES IN THIS
                                       ;PROGRAM ON THE OTHER HAND
         ;; IF THE RESTRICTIONS HAVE BEEN MET THEN BUILD AN
         ;;RSS NODE
         (SETQ
          %NEWRSS
          (BUILD
                                       ;NEWRSS IS THE NEW RSS NODE NAME CREATED BY
           RSSNODE=
                                       ;BUILD RSSNODE= IS KEYWORD FOR INPUT INTO BUILD
           (SETQ RSSNAME (MAKESYM 'RSS))
                                       ;OF RSS NODE NAME.  IN THE CALL TO BUILD THE
           MARKERS=
                                       ;ITEMS ENDING IN = ARE KEYWORDS WHOSE VALUE IS
           MARKERS:
                                       ;THE
           VARIABLE=
           ((LAMBDA (X) (PUTPROP X X 'RSSVAR))
                                       ;FOLLWING ITEM. MARKERS:, OF COURSE IS A
            (MAKESYM 'EVX))
           PARSENODE=
           C ;VARIABLE IN THIS FUNCTION. THIS CALL JUST SAYS
           RELATIONS=
           (REVERSE
                                       ;SEND TO BUILD FOR MARKERS= THE VALUE SENT TO
            (MAPCAR
             '(LAMBDA (%PLNRPHRASE)
                                       ;RELATION FOR MARKERS:   THE PARSENODE IS THE
                  (PLNR-NUMSUB '<<<RELATION-ERROR>>>
                                       ;CURRENT NODE ***, #1, #2, AND #3 SUBSTITUTIONS
                       %PLNRPHRASE))
                                       ;DONE .  THIS IS FIRST STEP IN BUILDING PLANNER
             (EVALCHECK PROCEDURE:)))
           REL=
           REL:
                                       ;CODE.
           NEGATIVE=
           (AND (CQ NEG) T)
           RELMARKERS=
           RELMARKERS:
           PLAUSIBILITY=
           (PLUS (PLAUSIBILITY? SMSUB)
                                       ;NUMSUB IS THE ****, #!, #", #3 SUBSTITUTION
             (PLAUSIBILITY? SMOB1)
                                       ;FUNCTION %PLNRPHRASE IS ONE CHUNK OF CONDENSED
             (PLAUSIBILITY? SMOB2)
                                       ;PLANNER CODE LIKE (#COLOR *** #RED)
             (OR (EVAL PLAUSIBILITY:) 0.))
           AMBIGUITIES=
           (APPEND
            (AMBIGUITIES? #1)
            (AMBIGUITIES? #2)
            (AMBIGUITIES? #3)
            (AND PARAPHRASE:
             (LIST (LIST RSSNAME
                     PARAPHRASE:
                     WORD-BEING-INTERPRETED)))))))
        (RETURN %NEWRSS)))
    %DEFL
    SMSUB
    SMOB1
    SMOB2
    SMOBL
    SMCOMP))

1.

;;;============================================================

(DEFUN DOBACKREF (ANSWER)
       ;; CALLED WHEN THE PROCESSING OF A SENTENCE IS COMPLETED.
       ;;SETS UP VARIABLES SO THAT NEXT SENTENCE MAY REFER TO GOODIES
       ;;IN THIS SENTENCE.
       ;;; DOES THE FOLLOWING:
       ;;;    1. SET LASTREL TO REL OF THIS SENTENCE
       ;;;    2. SET LASTEVENT TO THE EVENT DESCRIBED IN THIS SENTENCE
       ;;;       FOR REFERENCE OF "DO IT!" COMMAND.
       ;;;    3. SET LASTANSEVENT TO THE EVENT DESCRIBED IN THE
       ;;;       ANSWER OF THIS SENTENCE. FOR REFERENCE OF "THAT"
       ;;;       IN QUERY: "WHY DID YOU DO THAT?"
       ;;;    4. SET BACKREF2 TO BACKREF AND (!!!) GO THROUGH ALL
       ;;;       THINGS ON BACKREF AND SET THEIR REFER(ENT)= PROPERTY
       ;;;       TO THE BIND OF THEIR VARIABLES. ALSO MAKE SURE EACH
       ;;;       NG NODE ON BACKREF POINT TO THE RIGHT OSS IN ITS SM.
       ;;;       THIS ENSURES THAT EACH OSS POINTS TO THE ACTUAL
       ;;;       THING IT WAS ASSUMED TO REFER TO.
       ;;;    5. REMOVE THE BIND PROPERTY OF PRONOUNS (IT THEY) USED
       ;;;       IN THIS SENTENCE AND MAKE IT THEIR LASTBIND PROPERTY.
       (PROG (ANSRSS)
         (SETQ ANSRSS (GET ANSWER 'ANSRSS=))
         (SETQ BACKREF2 BACKREF)
         (SETQ BACKREF NIL)
         (SETQ LASTREL (REL? ANSRSS))
         (MAPC '(LAMBDA (PRONOUN) (PUTPROP PRONOUN
                           (GET PRONOUN
                            'BIND)
                           'LASTBIND)
                      (REMPROP PRONOUN 'BIND))
           '(IT THEY ONE))
         (OR
          (CQ MODAL)
          (CQ DECLAR)
          (MAP
           '(LAMBDA (BACKNODE)
         (COND ((CDR (SM BACKNODE))
                                       ;TRUE IF NODE HAD MULTIPLE INTERPRETATIONS
            (PRINT (SM BACKNODE))
            (SETR 'SEMANTICS
                  (ERT DOBACKREF:
                   RETURN
                   AN
                   OSS
                   FOR
                   BACKNODE)
                  BACKNODE)))
         (COND
          ((REFER? (CAR (SM BACKNODE))))
                                       ;IF NODE HAS REFERENT, FINE
          (ELSE
           (PUTPROP (CAR (SM BACKNODE))
                (OR (GET (VARIABLE? (CAR (SM BACKNODE)))
                     'BIND)
                (ERT DOBACKREF:
                     RETURN
                     REFERENT
                     FOR
                     BACKNODE))
                'REFER=))))
           BACKREF2))

         ;;; A FEW MISSING PIECES
         ;;; GO HERE
         ))

;;;=======================================================I

(DEFUN EVALCHECK (L)
       ;;EVALCHECK CHECKS FOR THE PRESENCE OF (#EVAL (MUMBLE ...)...)
       ;;IN THE INPUT S-EXPRESSION L.  IF IT FINDS ONE THEN THE
       ;;EXPRESSION MUMBLE IS EVALUATED AND REPACES (#EVAL ...),
       ;;OTHERWISE L IS RETURNED JUST
THE WAY IT WAS.  HENCE THIS
       ;;FUNCTION IS THE INVERSE OF QUOTE.
       (COND ((ATOM L) L)
         ((EQ (CAR L) '#EVAL) (EVAL (CADR L)))
         (L)))

;;;=========================================================================

(DEFUN ITERATE FEXPR (%L)
       ;; GENERALIZED MAPPING FUNCTION.  APPLIES FUNCTION TO THE
       ;;CARTESIAN PRODUCT OF N LISTS GIVEN AS ARGUMENTS.
       ;;;INPUTS:
       ;;;     (CAR %L)     FUNCTIONAL ARGUMENT.  FUNCTION OF N ARGS WHERE
       ;;;                  N IS THE NUMBER OF LISTS GIVEN.
       ;;;     (CDR %L)     THE N LISTS
       ;;;
       ;;;VALUE:
       ;; THE RESULT OF APPLYING THE FUNCTION IS MAPBLANDED TOGETHER.
       ;;THAT IS IF EACH APPLICATION RESULTS IN AN ATOM, THE
       ;;RESULTING LIST IS A LIST OF ATOMS, IF EACH APPLICATION
       ;;RESULTS IN A LIST OF ATOMS, THE RESULTS IS STILL A LIST OF
       ;;ATOMS.  IF THE APPLICATION RESULTS IN NIL, THE NIL VANISHES
       ;;FROM THE RESULTANT LIST.
       (PROG (%X %XL) (RETURN (EVAL (ITERATEX (CAR %L) (CDR %L))))))
                                       ;THIS SHOULD BECOME A MACRO SO THAT ITERATE
               ;TAILORS A MAPPING FUNCTION FOR EACH APPLICATION
                                       ;WHICH IS THEN COMPILED

;;;=============================================================================

(DEFUN ITERATEX (F L)
       ;; EXPANDS CALL ON ITERATE INTO A TAILORED SET OF MAPBLAND
       ;;CALLS WHICH DO THE APPROPRIATE COMPUTATION WHEN EVALUATED
       ;;;INPUTS:
       ;;;      F      FUNCTION OF N ARGUMENTS
       ;;;      L      LIST OF N LISTS WHICH F IS TO BE APPLIED TO
       ;;;VALUE:
       ;; TAILORED FUNCTION
       (COND ((NULL L) (CONS (EVAL F) %XL))
         ((LIST 'MAPBLAND
            (LIST 'FUNCTION
              (LIST 'LAMBDA
                (LIST (SETQ %X
                                       ;%X IS USED TO STORE THE VARIABLE NAME WHICH IT
                        (GENSYM)
                                       ;GETS FROM (GENSYM)
                        %XL
                                       ;%XL IS USED TO SAVE A LIST OF ALL  OF THE
                        (NCONC %XL (CONS %X NIL))
                                       ;VARIABLE NAMES SO FAR SO THAT THEY CAN BE GIVEN
               ;TO THE FUNCTION AT THE END (CONS %X NIL)
                        %X
                                       ;CREATES A SHINY NEW CELL WHICH IS BOLTED ONTO
                        %X))
                                       ;THE BACK END OF %XL BY NCONC THIS PAIR IS
                (ITERATEX F (CDR L))))
                                       ;NECESSARY BECAUSE SETQ RETURNS AS ITS VALUE THE
            (OR (CAR L) '(NIL))))))
                                       ;RESULT OF THE LAST PAIR THAT IT PROCESSES.  A
               ;RUSE. PUTS (NIL) IN PLACE OF NIL AS A LIST TO
               ;PREVENT MAPBLAND FROM QUITTNG.

;;;=============================================================================

(DEFUN MAPBLAND (FN L)
       ;; THIS IS THE INSTAMATIC CAMERA FUNCTION.  NO MATTER WHAT YOU
       ;;PUT INTO THE FUNCTION IT AUTOMATICALLY ADJUSTS INTERNALLY TO
       ;;THE AVAILABLE LIGHT SO THAT WHAT COMES OUT THE END ALWAYS
       ;;LOOKS THE SAME -- ONE BIG NIL-LESS LIST OF ALL THE
       ;;APPLICATIONS OF THE FUNCTION FN.  THE PURPOSE IN THIS IS SO
       ;;THAT THE FUNCTION CAN BE EASILY NESTED.
       ;;;INPUTS:
       ;;;     FN  -  FUNCTION OF ONE ARGUMENT TO BE APPLIED TO EACH ELEMENT IN
       ;;L
       ;;;     L   -  LIST
       ;;;VALUE:
       ;;;     IF (FN L) IS AN ATOM, THEN A LIST OF ATOMS
       ;;;     IF (FN L) IS A LIST, THEN ALL THE LISTS APPENDED, THAT IS A LIST
       ;;OF ATOMS.
       ;;;     IF (FN L) IS NIL, THEN ALL TRACE DISAPPEARS (SAVE FOR
       ;;SIDE-EFFECTS).
       (PROG (ANS F)
         (AND (NULL L) (SETQ L '(NIL)))
    A    (COND ((NULL (SETQ F ((EVAL 'FN) (CAR L)))))
           ((ATOM F) (SETQ ANS (NCONC ANS (CONS F NIL))))
           ((SETQ ANS (APPEND ANS F))))
         (SETQ L (CDR L))
         (AND L (GO A))
         (RETURN ANS)
         (GO A)))

;;;=============================================================================

(DEFUN MAPC2 (FN L)
       ;; MAPPING FUNCTION FOR GOING 2 AT A TIME THROUGH A LIST
       ;;;INPUTS:
       ;;;     FN   -   FUNCTION OF TWO ARGUMENTS
       ;;;     L    -   LIST (ESPECIALLY ATTRIBUTE
       ;;VALUE TYPE LIST)
       ;;;VALUE:
       ;;;       LIST (LIKE MAPCAR) OF FN APPLIED TO
       ;;TOP TWO ELEMENTS
       (PROG (DUMMY) ;DUMMY IS USED TO ESCAPE FROM A SYSTEM ERROR
    A    (COND ((NULL L) (RETURN T)))
                                       ;WHICH OCCURS WHEN NIL IS USED
         ((EVAL 'FN) (CAR L) (CADR L))
                                       ;       FN APPLIED TO TOP TWO ELEMENTS. EVAL
         (SETQ L (CDDR L))
                                       ;IS TO AVOID CONFLICT WITH FUNCTION REALLY NAMED
         (GO A)))
                                       ;FN LIST IS STEPPED TWO AT A TIME

;;;============================================================

(DEFUN MUMBLE (X)
       ;;MUMBLE IS THE PLANNER FILTER FOR LOOKING AT ASSERTIONS TO
       ;;SEE WHEN THEY WERE MENTIONED IN THE DIALOG. IT USES THE FREE
       ;;VARIABLE "WHO" TO DECIDE WHAT TO LOOK FOR. WHO IS EITHER NIL
       ;;(USE ANYTHING) "HE" (USE ANYTHING WHICH HAS BEEN MENTIONED)
       ;;OR A LIST OF TWO SENTENCE NUMBERS, MIN AND MAX (USE ANY WHO
       ;;PROPERTY WHICH IS ON OR BETWEEN THEM). THE WHO PROPERTY OF
       ;;ANY ASSERTION IS A SENTENCE NUMBER WHEN IT WAS MOST RECENTLY
       ;;MENTIONED.
       (COND ((NULL WHO))
         ((EQ WHO 'HE) (GET X 'WHO))
         ((SETQ X (GET X 'WHO))
          (NOT (OR (LESSP X (CAR WHO))
               (GREATERP X (CADR WHO)))))))

;;;============================================================

(DEFUN OBJECT FEXPR (%DEFL)
                                       ;%DEFL IS THE LIST OF DEFINITION SENSES
       ;; CONSTRUCTS OSS FOR GARDEN VARIETY NOUNS AND ADJECTIVES.
       ;;USED IN DEFINITIONS.
       ;;;INPUTS:
       ;; INPUTS ARE IN KEYWORD FORMAT (ATTRIBUTE-VALUE PAIRS).  THE
       ;;KEYWORD IS NOT EVALUATED BUT ITS VLUE IS.  POSIBLE KEYWORDS:
       ;;;     MARKERS:            LIST OF SEMANTIC MARKERS
       ;;;     PLAUSIBILITY:       EVALS TO INTEGER FROM 0 TO 1000 INDICATING RELATIVE
       ;;;                         LIKLIHOOD OF THIS DEFINITION SENSE
       ;;;     DET:                ****I'M TOO LAZY TOO LOKK THIS UP NOW
                                       ; FILL IN DET********************
       ;;;FREE VARIABLE INPUT:
       ;;;     SM  -  A LIST OF CURRENT OSS'S WITH WHICH THE TO-BE-CREATED ONES
       ;;;            MUST BE COMPATIBLE
       ;;;VALUE:
       ;;;     A NEW LIST OF OSS'S TO TAKE THE PLACE OF THE OLD ONE.
       ;;;SIDE-EFFECTS:
       ;; SM IS RESET TO THE VALUE OF OBJECT
       ;;;  A SET OF OSS'S ARE CREATED AT THE GLOBAL LEVEL
       (PROG (%VARNAM)
                                       ;PROG TO DECLARE VARIABLE
         (OR SM (SETQ %VARNAM (MAKESYM 'X)))
                                       ;IF SM IS EMPTY (AS IN THE CASE OF A HEAD) MAKE
         (RETURN ;A NEW VARIABLE SYSMBOL, OTHERWISE THE
          (SMSET ;APPROPRIATE %VARNAM WILL BE DECIDED INSIDE THE
           (ITERATE
                                       ;ITERATE LOOP.
        '(LAMBDA (%OSS %DEF)
                                       ;%OSS IS A SINGLE OSS NODE NAME PICKED OFF OF
          (PROG (%OSSNODE %CHKRESULT MARKERS: SYSTEMS:
             PLAUSIBILITY: DET: RELATIONS: PARAPHRASE:
             PROCEDURE:)
                                       ;SM. %DEF IS A SINGLE DEFINITION SENSE PICKED
                                       ;OFF OF %DEFL.

            ;;;****************
            ;; DECODE KEYWORDS
            ;;;***************
            (SETQQCHECK NIL
                    %DEF
                    '(MARKERS: PLAUSIBILITY:
                           PARAPHRASE:
                           PROCEDURE:)
                    'OBJECT)

            ;;;****************
            ;; CHECK FOR MARKER AGREENT.  IF OK THEN
            ;;BUILD OSS, ELSE CHUCK THIS COMBINATION
            ;;;****************
            (AND
             (SETQ %CHKRESULT
                   (CHECK MARKERS:
                      (MARKERS? %OSS)
                      (SYSTEMS? %OSS)))

             ;;;****************
             ;;BUILD OSS COMBINING INFORMATION FROM
             ;;CURRENT OSS WITH INFORMATION IN THE
             ;;DEFINITION.  NOTE THAT THE INITIAL OSS
             ;;WHICH GETS BUILT UP FOR A WORD DEPENDS NOT
             ;;ONLY ON ITS DEFINITION BUT ALSO ON THE
             ;;CONTEXT IN WHICH IT IS USED.
             ;;;****************
             (RETURN
              (BUILD
               OSSNODE=
               (SETQ %OSSNODE (MAKESYM 'OSS))
               PARSENODE=
               (PARSENODE? %OSS)
               MARKERS=
               (CAR %CHKRESULT)
               SYSTEMS=
               (CADR %CHKRESULT)
               DETERMINER=
               (DETERMINER? %OSS)
               VARIABLE=
               (VARIABLE? %OSS)
               RELATIONS=
               (NCONC
                (REVERSE
                 (MAPCAR
                  '(LAMBDA (%PLNRPHRASE)
                       (PLNR-NUMSUB %OSS %PLNRPHRASE))
                  (EVALCHECK PROCEDURE:)))
                (RELATIONS? %OSS))
               REL=
               (REL? %OSS)
               AMBIGUITIES=
               (APPEND
                (AMBIGUITIES? %OSS)
                (AND
                 PARAPHRASE:
                 (LIST (LIST %OSS
                     PARAPHRASE:
                     WORD-BEING-INTERPRETED))))
               PLAUSIBILITY=
               (PLUS (OR (EVAL PLAUSIBILITY:) 0.)
                                       ; THE OSS NAME PROVIDES A UNIQUE LABEL FOR WHERE
                 (PLAUSIBILITY? %OSS)))))))
                                       ;THE AMBIGUITY OCCURRED FOR LATER COMPARISON
        SM
        %DEFL)))))

;;;===========================================================
;;;
;;;                    PLANNER BUILDING ROUTINES
;;;
;;;============================================================

(DEFUN PLNR-JUNKIFY (CODE)
       ;;PUTS DISCOURSE STUFF INTO CODE
       (COND ((ATOM CODE) CODE)
         ((EQ (CAR CODE) 'THGOAL)
          (LIST 'THAND CODE '(VALUEPUT)))
         ((EQ (CAR CODE) 'THFIND)
          (LIST 'THAND
            CODE
            (LIST 'THPUTPROP
              (QUOTIFY (CADR (CADDR CODE)))
              'THVALUE
              ''BIND)))
         ((OR (EQ (CAR CODE) 'THAND)
          (EQ (CAR CODE) 'THPROG))
          (MAPCAN 'PLNR-JUNKIFY2 CODE))
         ((MAPCAR 'PLNR-JUNKIFY CODE))))

;;;============================================================

(DEFUN PLNR-JUNKIFY2 (CODE)
       ;;PUTS DISCOURSE STUFF INTO CODE
       (COND ((ATOM CODE) (LIST CODE))
         ((EQ (CAR CODE) 'THGOAL)
          (LIST CODE '(VALUEPUT)))
         ((EQ (CAR CODE) 'THFIND)
          (LIST CODE
            (LIST 'THPUTPROP
              (QUOTIFY (CADR (CADDR CODE)))
              'THVALUE
              ''BIND)))
         ((OR (EQ (CAR CODE) 'THAND)
          (EQ (CAR CODE) 'THPROG))
          (LIST (MAPCAN 'PLNR-JUNKIFY2 CODE)))
         ((LIST (MAPCAR 'PLNR-JUNKIFY CODE)))))

;;;============================================================

(DEFUN VALUEPUT NIL (PUTPROP THVALUE SENTNO 'WHO))

(DEFUN PLNR-THCONSIFY (VARLIST EXP BODY)
       ;;GENERATES A CONSEQUENT THEOREM.
       (PROG (TH)
         (SETQ TH (MAKESYM 'THEOREM))
         (PUTPROP TH
              (COND ((EQ (CAR BODY) 'THPROG)
                 (NCONC (LIST 'THCONSE
                      (UNION VARLIST (CADR BODY))
                      EXP)
                    (CDDR BODY)))
                (T (LIST 'THCONSE
                     VARLIST
                     EXP
                     BODY)))
              'THEOREM)
         (RETURN TH)))

(DEFUN PLNR-FINDIFY (MODE VARIABLE VARLIST BODY)
       ;; GENERATES A THFIND STATEMENT FOR THE NOUN GROUP DESCRIBED
       ;;IN THE OSS.  IT (CURRENTLY) ASSUMES THAT THE PLNRCODE
       ;;PROPERTY OF THE OSS IS A LIST OF PATERNS OF THGOAL
       ;;STATEMENTS.  MODE IS DEFINED TO BE <MODE> IN THE
       ;;MICRO-PLANNER DESCRIPTION OF THFIND (SEE AI MEMO #203A) BODY
       ;;IS A SINGLE PLANNER EXPRESSION (POSSIBLY A THAND OR THPROG)
       ;;;
       (COND ((EQ (CAR BODY) 'THAND) (SETQ BODY (CDR BODY)))
         ((EQ (CAR BODY) 'THPROG)
          (SETQ VARLIST (APPEND VARLIST (CADR BODY)))
          (SETQ BODY (CDDR BODY)))
         ((SETQ BODY (LIST BODY))))
       (NCONC (LIST 'THFIND
            MODE
            (PLNR-VAR VARIABLE)
                                       ; <SKELETON>
            VARLIST)
                                       ; <VARIABLE DECLARATIONS>
          BODY))

;;;=============================================================

(DEFUN PLNR-FINDSPEC (X)
       ;;GENERATES PAMETER FOR THFIND FROM THE NOTATION USED IN THE
       ;;DETERMINER?
       (COND ((NUMBERP X) X)
         ((MEMQ X '(NS NPL SG-PL)) 1.)
         ((EQ (CAR X) 'EXACTLY)
          (LIST (CADR X) (ADD1 (CADR X)) NIL))
         ((EQ (CAR X) '>) (ADD1 (CADR X)))
         ((EQ (CAR X) '<) (LIST 0. (CADR X) NIL))
         ((ERTERR PLNR-FINDSPEC -- FUNNY SPECIFICATION))))

;;;=============================================================

(DEFUN PLNR-GOALIFY (PLNRPHRASE)
       ;;TAKES A PLNRPHRASE AND MAKES A THGOAL STATEMENT OUT OF IT
       ;;UNLESS IT ISN'T SUPPOSED TO BE ONE.  ALSO CALLS PLNR-NOTIFY
       ;;IF APPROPRIATE
       (SETQ PLNRPHRASE (PLNR-REMTIME PLNRPHRASE))
                                       ;PRESENT TENSE TIME MARKERS ARE REMOVED TO
       (COND ((GET (CAR PLNRPHRASE) 'NOGOAL) PLNRPHRASE)
                                       ;SIMPLIFY THE MOST COMMON EXPRESSIONS
         ((APPEND (LIST 'THGOAL
                PLNRPHRASE
                '(THDBF MUMBLE))
              (PLNR-RECOMMENDIFY PLNRPHRASE)))))

(DEFUN PLNR-MUNG (FINDEXPR CANDIDATES)
       ;; DOES A HORRIBLE THING : MUNGS A THFIND EXPRESSION WHICH
       ;;FINDS A NOUN GROUP REFERENCE.  IT PUTS A THAMONG EXPRESSION
       ;;AS THE FIRST STATEMENT OF THE THFIND.  IF THERE IS ALREADY A
       ;;THAMONG EXPRESSION IN THE THFIND THEN MUNG JUST CLOBBERS THE
       ;;LIST IN THAT THAMONG.
       ;;;
       (CONS
    (CAR FINDEXPR)
    (CONS (CADR FINDEXPR)
          (CONS (CADDR FINDEXPR)
            (CONS (CADDDR FINDEXPR)
              (CONS (LIST 'THAMONG
                      (CADDR FINDEXPR)
                      (QUOTIFY CANDIDATES))
                (COND ((EQ (CAADDR (CDR FINDEXPR))
                       'THFIND)
                       (CDDDDR (CDR FINDEXPR)))
                      (T (CDDDDR FINDEXPR)))))))))

;;;============================================================

(DEFUN PLNR-NOTIFY (NEG? %PLNRPHRASE)

       ;;;PUTS IN THNOT
       (COND ((NOT NEG?) %PLNRPHRASE)
         ((EQ (CAR %PLNRPHRASE) 'THNOT) (CADR %PLNRPHRASE))
                                       ;BUT ELIMINATE DOUBLE NEGATIVES.
         ((LIST 'THNOT %PLNRPHRASE))))

;;;============================================================

(DEFUN PLNR-NEWBODY (X) (SETQ NEWBODY (CONS X NEWBODY)))

(DEFUN PLNR-PROGIFY (VARLIST BODY)
       ;;SETS UP A THPROG OR THE SIMPLEST EQUIVALENT EXPRESSION FOR
       ;;THE PARTICULAR CASE.  BODY IS A LIST OF EXPRESSIONS
       (PROG (NEWBODY)
         (OR BODY (RETURN NIL))
         (MAPC '(LAMBDA (X)
                (COND ((EQ (CAR X) 'THPROG)
                   (COND ((MEET VARLIST (CADR X))
                      (PLNR-NEWBODY X))
                     (T (SETQ VARLIST
                          (APPEND VARLIST
                              (CADR X)))
                        (MAPC 'PLNR-NEWBODY
                          (CDDR X)))))
                  ((EQ (CAR X) 'THAND)
                   (MAPC 'PLNR-NEWBODY
                     (CDR X)))
                  ((PLNR-NEWBODY X))))
           BODY)
         (RETURN (COND (VARLIST (CONS 'THPROG
                      (CONS VARLIST
                        (REVERSE NEWBODY))))
               ((CDR NEWBODY)
                (CONS 'THAND (REVERSE NEWBODY)))
               ((CAR NEWBODY))))))

;;;============================================================

(DEFUN PLNR-NUMREL (OSS)
       ;; THIS IS USED BY PLNR-NUMSUB TO HAVE THE VARIABLE NAME
       ;;SUBSTITUTED FOR THE OSS WHICH IS THE REL OF A PARTICULAR
       ;;EXPRESSION.
       (COND ((MEMQ OSS RELLIST) (SETQ REL: OSS)) (OSS)))

;;;============================================================

(DEFUN PLNR-NUMSUB (%ME %PLNRPHRASE)
       ;;FUNCTION WHICH SUBSTITUTES THE PROPER PARSE TIME VARIABLES
       ;;FOR #1, #2, #3, AND *** IN THE PLANNER SHCEMAS FROM
       ;;DICTIONARY DEFINITIONS.
       ;;;INPUTS:
       ;;;     %ME    - OSS FOR CURRENT OBJECT
       ;;;     %PLNRPHRASE - A PHRASE FROM THE DICTIONARY IN WHICH SUBSTITUTIONS ARE TO BE MADE
       ;;;FREE VARIABLE INPUTS:
       ;;;     #1, #2, #3, CORRESPOND TO POSITIONS IN THE
       ;;; RESTRICTION LIST OF THE DEFINITION. EACH POINTS TO A SINGLE OSS
       ;;; OR NIL IF NOT APPLICABLE.
       ;;;     SMSUB     - ONE OSS FROM SMSUB REGISTER
       ;;;     SMOB1    - ONE OSS FROM SMOB1 REGISTER
       ;;;     SMOB2    - ONE OSS FROM SMOB2 REGISTER
       ;;;     *TIME      - CURRENT TIME
       ;;;VALUE:
       ;;;THE CONDENSED PLANNER CODE AFTER THE SUBSTITUTIONS HAVE BEEN MADE
       (MAPCAR (FUNCTION (LAMBDA (%ELEMENT)
                                       ;%ELEMENT IS AN ATOM OF THE PHRASE
                 (COND ((MEMQ %ELEMENT
                          '(#1 #2 #3))
                    (PLNR-NUMREL (EVAL %ELEMENT)))
                       ((EQ %ELEMENT '***) %ME)
                       ((EQ %ELEMENT '*TIME)
                    TIME)
                                       ; GETS THE CURRENT TIME
                       (%ELEMENT))))
           (EVALCHECK %PLNRPHRASE)))

;;;============================================================

(DEFUN PLNR-RECOMMENDIFY (%PLNRPHRASE)
       ;;LOOKS UP IN THE DICTIONARY A RECOMMENDED THEOREM TO USE IN
       ;;PROCESSING A PLNRPHRASE BY THGOAL.  IF IT FINDS ONE IT TACKS
       ;;IT ON AS A RECOMENDATION.
       (PROG (%ELEMENT)
         (RETURN
          (AND (SETQ %ELEMENT (GET (CAR %PLNRPHRASE)
                       'THMLIST))
                                       ;LOOK A RELATION UP IN THE DICTIONARY.  THE
           (EVAL (COND ((NUMBERP (CAAR %ELEMENT))
                                       ;ENTRIES ARE SET UP AS A PROPERTY LIST.  THERE
                (CADR (OR (ASSQ (LENGTH %PLNRPHRASE)
                                       ;ARE DIFFERENT RECOMMENDATIONS FOR THE SAME
                        %ELEMENT)
                      '(NIL NIL))))
                                       ;RELATION DEPENDING ON THE NUMBER OF ARGUMENTS
                   (%ELEMENT)))))))
                                       ;THIS INSTANCE OF IT HAS.  (LENGTH ...) COMPUTES
               ;THE NUMBER OF ARGUMENTS + 1 AND THE (ASSQ ...)
               ;RETRIEVES THE
APPROPRIATE RECOMMENDATION USING
               ;THIS NUMBER.  IF THERE IS NO SUCH  NUMBER,
               ;NUMBERP FAILS AND SOME ARBITARY FUNCTION WHICH
               ;IS STORED OUT THERE IS EVALUATED TO GIVE THE
                                       ;RECOMMENDATION.

;;;=============================================================

(DEFUN PLNR-REMTIME (EXP)
       ;;REMOVES ALL PRESENT TENSE TIME STRUCTURES
       ((LAMBDA (Y)
     (DELQ
      Y
      (MAPCAR
       '(LAMBDA (X)
         (COND ((NOT (ATOM X)) X)
           ((TSS? X)
            (COND ((AND (TENSE? X)
                (NOT (MEMBER (TENSE? X)
                         '((PRESENT PRESENT)
                           (MODAL) (PRESENT)))))
               X)
              (Y)))
           (X)))
       EXP)))
    '(T)))       ;Y IS BOUND TO A UNIQUE POINTER SO IT CAN'T
               ;POSSIBLY SCREW ANYTHING IN THE EXPRESSSION WHEN
                                       ;IT DOES THE DELETION.  DELQ USES EQ.

;;;=============================================================

(DEFUN PLNR-VAR (X)
       ;;GENERATES SYNTAX FOR VARIABLE NAMES IN PLANNER
       (LIST 'THV X))

;;;============================================================

(DEFUN COMPARE-BUILD (NODE DEGREE)
       ;;USED BY SMADJG-PREPG TO BUILD A PSUEDO VERB FOR THE
       ;;COMPARATIVE.  SMCL1 IS THEN CALLED
       (PROG (RESTRICTIONS: DIMENSION: DIRECTION:)
                                       ;THESE ARE THE POSSIBLE PARTS OF A MEASURE
         (SETQQCHECK NIL
             (CDR (FINDMEASURE NODE))
             '(RESTRICTIONS: DIMENSION: DIRECTION:)
             'MEASURE)
                                       ;DEFINITION
         (PUTPROP 'COMPARE-PSEUDO-VERB
              (LIST 'RELATION
                (LIST 'RESTRICTIONS:
                  (LIST (LIST RESTRICTIONS:)
                    (LIST RESTRICTIONS:))
                  'PROCEDURE:
                  (LIST (LIST DEGREE
                          DIMENSION:
                          (COND (DIRECTION: '#1)
                            ('#2))
                          (COND (DIRECTION: '#2)
                            ('#1))))))
              'SEMANTICS)
         (RETURN '(COMPARE-PSEUDO-VERB))))

;;;=============================================================

(DEFUN FINDMEASURE (NODE)
       ;;GETS THE MEASURE DEFINITION
       (COND ((SETQ X (ASSOC 'MEASURE
                 (GET (ROOT (NB NODE)) 'SEMANTICS)))
          (CADR X))
         ((GLOBAL-ERR (APPEND '(I DON"T
                      KNOW
                      HOW
                      TO
                      COMPARE
                      THINGS
                      WITH
                      RESPECT
                      TO)
                  (LIST (ROOT (NB NODE))))))))

;;;=============================================================

(DEFUN MEASURE FEXPR (MEAS)
       ;;USED TO GENERATE ORDINALS -- IT IS CALLED WHEN A MEASURE
       ;;DEFINITION IS EVALLED
       (APPLY 'OBJECT
          (LIST (LIST 'MARKERS:
              (CADR (MEMQ 'RESTRICTIONS: MEAS))
              'PROCEDURE:
              (LIST (LIST '*ORDINAL* MEAS))))))

;;;=============================================================

(DEFUN PLNR-DESCRIBE (EXPS VAR FREEVARS)
       ;;BUILDS THE PLANNER DESCRIPTION, IGNORING THE QUANTIFIER
       ;;ACOUNTS FOR ORDINALS, SUBSTS, ETC.
       (PROG (ORDINAL BODY X)
    UP   (COND ((NULL EXPS)
            (RETURN (COND (ORDINAL (ORDMAKE ORDINAL VAR BODY))
                  ((PLNR-PROGIFY NIL BODY)))))
           ((EQ (SETQ X (EXPAND (CAR EXPS)
                    (AND (NULL (CDR EXPS))
                         (RSSVAR? VAR)
                         (GET VAR 'USED)
                         (PLNR-VAR VAR))))
            '*ORDINAL*))
           ((AND (CDR EXPS) (EQ (CAR X) '#SUBST))
                                       ;A SUBST DEFINITION IF IT IS THE ONLY THING IS
            (MAPC2 '(LAMBDA (X Y) (SETQ EXPS
                                       ;TO BE APPLIED TO THE OSS TO WHICH THIS RSS WILL
                        (SUBST X Y EXPS)))
                                       ;BE RELATED.
               (CDR X)))
                                       ;THE VARIABLE FOR A RELATION IS INSERTED INTO
           (X (SETQ BODY (CONS X BODY))))
                                       ;THE SECOND PLACE OF THE RELATION IF IT IS
         (SETQ EXPS (CDR EXPS))
                                       ;REFERRED TO ANYWHERE ELSE.
         (GO UP)))

;;;=============================================================

(DEFUN RELFIND (NODE)
       ;;LOOKS FOR THE REL OF A POLAR
       (PROG (REL)
         (ERRSET (MAP '(LAMBDA (X) (COND ((ISQ X NG)
                          (AND (NOT (ISQ X COMP))
                           (NOT (ISQ X DEF))
                           (SETQ REL X)
                           (ERR NIL)))
                         ((ISQ X LOBJ)
                          (AND (ISQ (H X) INDEF)
                           (SETQ REL X)
                           (ERR NIL)))
                         ((ISQ X PREPG)
                          (AND (ISQ (H X) INDEF)
                           (SETQ REL (H X))
                           (ERR NIL)))))
              (REVERSE (H NODE))))
                                       ;IT GOESFROM THE BEGINNINGOF THE SENTENCE
         (OR REL
         (AND (CQ PASV)
              (NOT (CQ AGENT))
              (SETQ REL '(FAKE-AGENT))))
         (RETURN (AND REL (SM REL)))))
                                       ;LOOKING FOR AN INDEFINITE NG, EITHER AT THE TOP
               ;LEVEL OR AS A FIRST LEVEL PREPOBJ, BUT NOT A
                                       ;COMPLEMENT.

;;;=============================================================

(DEFUN ORDMAKE (ORDINAL VAR BODY)
       ;;MAKES THE LOGICAL FORM FOR SUPERLATIVES ORDINAL GIVES THE
       ;;THING BEING COMPARED IN MEASURE FORM
       (PROG (NEWVAR)
         (SETQ NEWVAR (MAKESYM 'X))
         (RETURN
          (PLNR-PROGIFY
           NIL
           (APPEND
        BODY
        (LIST
         (PLNR-NOTIFY
          T
          (PLNR-PROGIFY
           (LIST NEWVAR)
           (APPEND
            (SUBST NEWVAR VAR BODY)
            (LIST
             (PLNR-GOALIFY (COMPARE-PROC VAR
                         NEWVAR
                         ORDINAL))))))))))))

;;;=============================================================

(DEFUN COMPARE-PROC (VAR NEWVAR ORDINAL)
       (PROG (RESTRICTIONS: DIRECTION: DIMENSION:)
         (SETQQCHECK NIL
             ORDINAL
             '(RESTRICTIONS: DIRECTION: DIMENSION:)
             'MEASURE)
         (RETURN (LIST '#MORE
               DIMENSION:
               (PLNR-VAR (COND (DIRECTION: NEWVAR) (VAR)))
               (PLNR-VAR (COND (DIRECTION: VAR)
                       (NEWVAR)))))))

;;;=============================================================

(DEFUN EXPAND (EXP EVENT)
       ;;THE HEART OF THE PLANNER BUILDER.  EXPANDS AN EXPRESSION
       ;;WORRYING ABOUT THE QUANTIFIERS AND CONNECTIVES OF ITS
       ;;CONSTITUENTS.  IT DOESN"T REALLY HANDLE EXPRESSIONS WITH
       ;;MORE THAN ONE QUANTIFIED SS UNLESS ONE OF THEM IS THE REL.
       ;;THE EVENT IS NON-NIL ONLY IF THE EVENT-NAME IS TO BE
       ;;INCLUDED IN THE EXPANSION OF THE EXPRESSION.
       (COND
    ((RSS? EXP)
     (COND ((AND? EXP)
        (PLNR-PROGIFY NIL
                  (MAPCAR '(LAMBDA (X) (EXPAND X NIL))
                      (AND? EXP))))
           ((OR? EXP)
        (PLNR-ORIFY (MAPCAR '(LAMBDA (X) (EXPAND X NIL))
                    (OR? EXP))))
           ((PLNR-NOTIFY (NEGATIVE? EXP)
                 (PLNR-DESCRIBE (RELATIONS? EXP)
                        (VARIABLE? EXP)
                        (CONS (VARIABLE? EXP)
                          FREEVARS))))))
    ((ATOM EXP) (BUG EXPAND - ATOMIC MODIFIER))
    ((EQ (CAR EXP) '*ORDINAL*)
     (COND (ORDINAL (GLOBAL-ERR '(I CAN"T
                    HANDLE
                    TWO
                    ORDINALS
                    OR
                    SUPERLATIVES
                    AT
                    ONCE)))
           ((SETQ ORDINAL (CADR EXP)) '*ORDINAL*)))
    ((EQ (CAR EXP) '#SUBST)
     (ERT EXPAND - IS #SUBST BEING HANDLED BY SOMEONE ELSE?)
     EXP)
    ((PROG (BODY QUANTIFIER CHOICE VAR MULTIPLE)
           (SETQ MULTIPLE (EVAL (GET (CAR EXP) 'MULTIPLE)))
           (SETQ
        EXP
        (MAPCAR
         '(LAMBDA (X)
           (COND
            ((OR (NOT (ATOM X)) (NOT (OR (RSS? X) (OSS? X))))
             X)
            ((REFER? X)
             (COND ((CDR (REFER? X))
                (COND (MULTIPLE (ERQSET 'AND)
                        (SETQ CHOICE (REFER? X))
                        '*AND*)
                  ((REFER? X))))
               ((CAR (REFER? X)))))
            ((MEMQ (VARIABLE? X) FREEVARS)
             (AND (RSSVAR? (VARIABLE? X))
              (PUTPROP (VARIABLE? X) T 'USED))
             (PLNR-VAR (VARIABLE? X)))
            ((SETQ CHOICE (AND? X))
             (ERQSET 'AND)
             (AND MULTIPLE
              (REFER? X)
              (SETQ CHOICE (REFER? X)))
             '*AND*)
            ((SETQ CHOICE (OR? X))
             (ERQSET 'OR)
             '*OR*)
            ((COND ((RSS? X)
                (ERQSET 'EVENT)
                (PUTPROP (VARIABLE? X) T 'USED))
               ((MEMQ (QUANTIFIER? X) '(ALL NO))
                (ERQSET (QUANTIFIER? X))
                T)
               ((MEMQ (QUANTIFIER? X)
                  '(NDET INDEF))
                (COND ((MEMQ (NUMBER? X)
                     '(NS SG-PL))
                   (ERQSET 'INDEF))
                  ((SETQ CHOICE
                     (PLNR-FINDSPEC (NUMBER? X)))
                   (ERQSET 'FIND)))
                T))
             (SETQ BODY
               (PLNR-DESCRIBE (RELATIONS? X)
                      (VARIABLE? X)
                      (CONS (VARIABLE? X)
                        FREEVARS)))
             (PLNR-VAR (SETQ VAR (VARIABLE? X))))
            ((ERTERR EXPAND - STRANGE QUANTIFIER))))
         (COND (EVENT (CONS (CAR EXP) (CONS EVENT (CDR EXP))))
               (T EXP))))
                                       ;THE EVENT NAME IS STUCK INTO THE SECOND
           (RETURN
                                       ;POSITION IF THERE IS ONE.
        (COND
         ((NULL QUANTIFIER) (PLNR-GOALIFY EXP))
         ((EQ QUANTIFIER 'AND)
          (PLNR-PROGIFY NIL
                (MAPCAR '(LAMBDA (X)
                         (EXPAND (SUBST X
                                '*AND*
                                EXP)
                             NIL))
                    CHOICE)))
         ((EQ QUANTIFIER 'OR)
          (PLNR-ORIFY (MAPCAR '(LAMBDA (X)
                           (EXPAND (SUBST X
                                  '*OR*
                                  EXP)
                               NIL))
                      CHOICE)))
         ((EQ QUANTIFIER 'FIND)
          (PLNR-FINDIFY
           CHOICE
           VAR
           (LIST VAR)
           (PLNR-PROGIFY NIL
                 (CONS BODY
                       (LIST (PLNR-GOALIFY EXP))))))
         (T
          (PLNR-NOTIFY
           (MEMQ QUANTIFIER '(ALL NO))
           (PLNR-PROGIFY
            (AND VAR (LIST VAR))
            (CONS
             BODY
             (LIST (PLNR-NOTIFY (EQ QUANTIFIER 'ALL)
                    (PLNR-GOALIFY EXP)))))))))))))

;;;=============================================================

(DEFUN ERQSET (X)
       ;;USED BY EXPAND TO MAKE SURE IT ISN"T GETTING CONFUSED BY TOO
       ;;MANY CONNECTIVES AND QUANTIFIERS IN THE SAME EXPRESSION
       (COND (QUANTIFIER (GLOBAL-ERR '(I CAN"T
                     HANDLE
                     COMBINATIONS
                     OF
                     QUANTIFIERS
                     AND
                     CONNECTIVES
                     WHICH
                     ARE
                     SO
                     COMPLICATED)))
         ((SETQ QUANTIFIER X))))

;;;============================================================

(DEFUN SETQQCHECK (%EVALFLAG %LIST %CHECKLIST %NAME)
       ;;SETQQCHECK IS LIKE SETQQ (OR LIKE SETQ DEPENDING ON
       ;;EVALFLAG) BUT IT CHECKS TO MAKE SURE THE VARIABLE NAME IS A
       ;;MEMBER OF THE %CHECKLIST, AND IF NOT PRINTS AN ERROR
       ;;MESSAGE.
       (PROG (%X)
    GO   (COND ((NULL %LIST) (RETURN T))
           ((MEMQ (CAR %LIST) %CHECKLIST)
            (SET (CAR %LIST)
             (COND (%EVALFLAG (EVAL (CADR %LIST)))
                   (ELSE (CADR %LIST))))
            (SETQ %LIST (CDDR %LIST))
            (GO GO))
           (T (SETQ %X
                (APPLY 'ERT
                   (CONS (CAR %LIST)
                     (APPEND '(IS NOT
                              A
                              LEGAL
                              SPECIFICATION
                              FOR)
                         (LIST %NAME)))))))
    UP   (COND ((EQ %X '?)
            (PRINT %CHECKLIST)
            (SETQ %X (ERT foo: setqqcheck ????))
            (GO UP))
                                       ;A QUESTION MARK GETS THE LIST OF POSSIBILITIES
               ;PRINTED OUT, THEN LETS YOU TRY AGAIN.  TO DO
               ;THIS YOU MUST TYPE (RETURN '?) AT THE  ERT.  IF
               ;YOU RETURN ANY OTHER VALUE, IT ASSUMES THIS IS
           ((SETQ %LIST (CONS %X (CDR %LIST))) (GO GO)))))
                                       ;THE VARIABLE NAME INTENDED, OTHERWISE IT JUST
                                       ;CAUSES AN ERROR.

;;;============================================================

(DEFUN THVAL2 (WHO AA)
       (PROG (RESULT X mplnr-ttime m-gc)
         (SETQ THLEVEL '(T))
         (SETQ X (SETQ RESULT '(NIL)))
         (AND PLANNERSEE
          (DISP AA)
          PLNRSEE-PAUSE
          (ERT FOR PLANNER))
(and (not (eq result x))
     (return result))
(setq mplnr-ttime (runtime) m-gc (status gctime) )
(setq result (thval aa '((ev command)) ))
(setq mplnr-time (timer mplnr-ttime (runtime)))
(or (= m-gc (status gctime))
    (setq mplnr-time (difference mplnr-time (timer m-gc (status gctime)))
          gc (status gctime)) )
(return result)
))

;;;============================================================

(DEFUN WHO (X)
       (COND ((NULL WHO))
         ((ATOM X))
         ((NOT (SETQ X (GET X 'WHO))) NIL)
         ((EQ WHO 'HE))
         ((LESSP (CAR WHO) X LASTSENTNO))))

(DEFUN CHECK (NEW-MARKERS MARKERS SYSTEMS)

       ;;;
       ;; TAKES A LIST OF NEW MARKERS AND CHECKS FOR COMPATIBILITY
       ;;WITH THE EXISTING MARKERS AND SYSTEMS (AS GIVEN BY ARGS
       ;;MARKERS AND SYSTEMS).  IF COMPATIBLE, RETURNS A TWO-LIST OF
       ;;THE NEW MARKERS AND SYSTEMS, ELSE RETURNS NIL
       ;;;
       (PROG NIL
    LOOP (COND ((NULL NEW-MARKERS)
            (RETURN (LIST MARKERS SYSTEMS)))
           ((CHECKAMARKER (CAR NEW-MARKERS))
            (SETQ NEW-MARKERS (CDR NEW-MARKERS))
            (GO LOOP))
           (T (RETURN NIL)))))
                                       ; FAIL IF CHECKAMARKER FAILS

;;;=============================================================================

(DEFUN CHECKAMARKER (MARKER)

       ;;;
       ;;; CHECKS A SINGLE MARKER FOR COMPATIBILITY
       ;;; USES FREE VARIABLES:
       ;;;    SYSTEMS - THE SYSTEM LIST SO FAR
       ;;;    MARKERS - THE MARKER LIST SO FAR
       ;;; IF SUCCESSFULL, THE MARKER AND ITS SYSTEM(S) ARE APPENDED
       ;;; TO THESE FREE VARIBLES
       ;;;
       (PROG (NEW-SYSTEMS)
         (COND ((MEMQ MARKER MARKERS) (RETURN T)))
                                       ;IF MARKER ALREADY THERE, FINE
         (SETQ MARKERS (CONS MARKER MARKERS))
                                       ; ADD NEW MARKER TO LIST
         (SETQ NEW-SYSTEMS (GET MARKER 'SYS))
                                       ;GET THE SYSTEMS OF THE NEW MARKER
    SYS  (COND ((NULL NEW-SYSTEMS) (RETURN T))
           ((MEMQ (CAR NEW-SYSTEMS) SYSTEMS) (RETURN NIL))
                                       ;FAIL IF SYSTEM THERE BY ANOTHER PATH
           ((CHECKAMARKER (CAR NEW-SYSTEMS))
            (SETQ SYSTEMS (CONS (CAR NEW-SYSTEMS) SYSTEMS))
            (SETQ NEW-SYSTEMS (CDR NEW-SYSTEMS))
            (GO SYS))
           (T (RETURN NIL)))))

(DEFUN FINDEVENTS (RSS)
       ;;FINDS ALL THE EVENTS FITTING THE RSS DESCRIPTION
       (PUTPROP (VARIABLE? RSS) T 'USED)
       (THVAL2 NIL
           (PLNR-FINDIFY 'ALL
                 (VARIABLE? RSS)
                 (LIST (VARIABLE? RSS))
                 (PLNR-DESCRIBE (RELATIONS? RSS)
                        (VARIABLE? RSS)
                        (LIST (VARIABLE? RSS))))))

(DEFUN CHECKREL (OSS)
       ;;CHECKS FOR POSSIBLE RELATIVE, EITHER BECAUSE OSS IS ON THE
       ;;RELLIST, OR BECUASE RSS INVOLVES INSIDE IT AN OSS ON THE
       ;;RELLIST
       (COND ((OSS? OSS) (MEMQ OSS RELLIST))
                                       ;IT RETURNS EITHER NIL OR A LIST OF WHICH THE
         ((RSS? OSS)
                                       ;FIRST ELEMENT IS THE REAL RELATIVE. IT USES
          (MAPCAN
           '(LAMBDA (RELATION)
                                       ;THIS FACT TO CHEAT ON RECURSION BY USING
            (COND ((ATOM RELATION) NIL)
                                       ;MAPCAN.
                  ((MAPCAN 'CHECKREL RELATION))))
           (RELATIONS? OSS)))))

§ anno/winograd/smass

(declare (genprefix smass))

;;;################################################################
;;;
;;;            SMASS  -  semantic access functions
;;;
;;;################################################################

(DEFUN ACTION? (X)
       ;;THE WORKING PART OF AN ANSWER -- TELLS WHAT TO DO IF THE
       ;;ANSWER IS THE ONE TO BE GIVEN.  MIGHT INCLUDE ACTIONS ON THE
       ;;DISPLAY, AS WELL AS THINGS TO BE PRINTED AND VARIABLES TO BE
       ;;SET, ETC.  THE WHOLE THING IS EVAL-LISTED.
       (GET X 'ACTION=))

(DEFUN AMBIGUITIES? (X)
       ;;LIST OF POSSIBLE AMBIGUITIES FOR A SEMANTIC STRUCTURE
       (GET X 'AMBIGUITIES=))

(DEFUN AND? (X)
       ;;FIND THE CONJUNCTION LIST OF A CONJOINED SEMANTIC STRUCTURE
       ;;NIL IF THERE IS NONE
       (GET X 'AND=))

(DEFUN ANSRSS? (X)
       ;;THE RSS CORRESPONDING TO AN ANSWER NODE (A PROPERTY OF THE
       ;;ANSNODE)
       (GET X 'ANSRSS=))

(DEFUN DETERMINER? (X)
       ;;;     ACCESS FUNCTION. GETS DET OF AN OSS.
       (GET X 'DETERMINER=))

(DEFUN END? (TSS)
       ;;END TIME FOR TSS
       (GET TSS 'END=))

(DEFUN MARKERS? (%SENSE)
       ;; ACCESS FUNCTION USED TO GET MARKERS FROM OSS OR RSS
       (GET %SENSE 'MARKERS=))

(DEFUN MODIFIERS? (%XSS)
       ;; ACCESS FUNCTION FOR GETTING THE PLANNER CODE SCHEMA OF AN
       ;;OSS OR RSS
       (GET %XSS 'MODIFIERS=))

(DEFUN NEGATIVE? (%XSS)
       ;;; ACCESS FUNCTION FOR OSS
       (GET %XSS 'NEGATIVE=))

(DEFUN NUMBER? (OSS)
       ;; GETS THE NUMBER FIELD OF AN OSS
       (CAR (GET OSS 'DETERMINER=)))

(DEFUN OR? (X)
       ;;ACCESS FOR LIST OF CONSTITUENTS IN DISJOINED SEMANTIC
       ;;STRUCTURE NIL IF IT ISNT
       (GET X 'OR=))

(DEFUN OSS? (X)
       ;;CHECKS TO SEE IF X IS AN OSS
       (GET X 'OSSNODE=))

(DEFUN PARENT? (NODE) (GETR 'PARENT NODE))

(DEFUN PARSENODE? (X) (GET X 'PARSENODE=))

;;THE PARSE NODE ASSOCIATED WITH THE SEMANTIC STRUCTURE

(DEFUN PLAUSIBILITY? (%XSS)
       ;; ACCESS FUNCTION FOR GETTING PLAUSIBILITY OF AN OSS OR RSS
       (OR (GET %XSS 'PLAUSIBILITY=) 0.))

(DEFUN PLNRCODE? (X) (GET X 'PLNRCODE=))

;;THE PLANNERCODE GENERATED WHEN AN OBJECT IS ACTUALLY LOOKED FOR IN THE DATA BASE.  IT IS NOT USED AGAIN,
;;BUT IS LEFT SITTING AROUND FOR PEOPLE TO LOOK AT.

(DEFUN QTYPE? (X) (CADDR (GET X 'DETERMINER=)))

;;QUESTION TYPE FOR QUESTION OSS

(DEFUN QUANTIFIER? (OSS)
       ;; GETS THE DETERMINER FIELD OF AN OSS
       (CADR (GET OSS 'DETERMINER=)))

(DEFUN REFER? (%XSS)
       ;;ACCESS FUNCTION FOR REFER OF OSS OR RSS
       (GET %XSS 'REFER=))

(DEFUN REL? (X) (GET X 'REL=))

;;THE OBJECT TO WHICH THIS DESCRIPTION IS TO BE RELATED.

(DEFUN RELATIONS? (X)
       ;; THE MATERIAL THAT WILL BECOME PLANNER CODE
       (GET X 'RELATIONS=))

(DEFUN RELMARKERS? (X)
       ;;MARKERS HELD BY A RELATIVE CLAUSE PRODUCED BY ITS SELECTION
       ;;RESTRICTIONS, TO BE ATTACHED TO THE OBJECT DESCRIPTION
       (GET X 'RELMARKERS=))

(DEFUN RSS? (X)
       ;;CHECKS TO SEE IF X IS AN RSS
       (GET X 'RSSNODE=))

(DEFUN RSSVAR? (X)
       ;;A VARIABLE OF THE TYPE USED FOR RSS'S -- I.E.  EVX3, ETC.
       (GET X 'RSSVAR))

(DEFUN START? (TSS)
       ;;START TIME FOR TSS
       (GET TSS 'START=))

(DEFUN SYSTEMS? (%SENSE)
       ;; ACCESS FUNCTION FOR GETTING THE SYSTEMS OF AN OSS OR RSS
       (GET %SENSE 'SYSTEMS=))

(DEFUN TENSE? (X)
       ;;FOR A TSS
       (GET X 'TENSE=))

(DEFUN TSS? (X) (GET X 'TSSNODE=))

;;ASSOCIATED WITH EVERY TIME SEMANTIC STRUCTURE.

(DEFUN VARIABLE? (X)
 ;; ACCESS FUNCTION FOR GETTING THE VARIABLE NAME ASSOCIATED WITH AN OSS OR RSS
   (GET X 'VARIABLE=))

(DEFUN SMSET (X) (SETR 'SEMANTICS X C) (SETQ SM X))

§ anno/winograd/newans

(declare (genprefix newans))

;;;################################################################
;;;
;;;         NEWANS - (new) Answering component
;;;
;;;################################################################

(DEFUN ANSWER (NODE)

       ;;THE TOP LEVEL ANSWER FUNCTION CALLED TO CARRY OUT THE
       ;;RESULTS OF ANY INPUT SENTENCE, WHETHER COMMAND, QUESTION, OR
       ;;STATEMENT.
       (PROG (ANSLIST AMBIG)                            ;ANSLIST IS THE LIST OF POSSIBLE ANSWERS. AMBIG
         (SETQ ANSNAME NIL)                           ;IS A FLAG SET IF THERE IS A POSSIBLE AMBIGUITY
         (SETQ AMBIG (CDR (SM NODE)))                   ;CLEAR OUT ANSWER NAMES SAVED FOR
         (SETQ ANSLIST                           ;BACKREF(ERENCE) ..I.E. MORE THAN ONE RSS FOR
           (ANSORDER (ANSUNIQUE (MAPCAR 'ANSGEN               ;THE SENTENCE.
                        (SM NODE)))))           ;ANSGEN GENERATES AN ANSWER FOR EACH
    CHOOSE                                   ;INTERPRETATION. ANSUNIQUE TAKES OUT REDUNDANT
         (COND ((AND (CDR ANSLIST)                       ;ONES IN THE CASE THAT DIFFERENT INTERPRETATIONS
             (NOT (ENOUGH-BETTER (CAR ANSLIST)           ;LEAD TO THE SAME ANSWER.  ANSORDER ORDERS THE
                         (CADR ANSLIST))))           ;REMAINING ONES BY PLAUSIBILITY.
            (SETQ ANSLIST (ANSELIMINATE ANSLIST))
            (GO CHOOSE)))                       ;IF NO ANSWER IS CLEARLY BEST, ASK THE USER FOR
         (or annoyance (PRINT *3))                           ;CLARIFICATION AND TRY AGAIN.
TEST-LOOP
         (AND ANS-AFTERFORMULATION-PAUSE  (ERT ANSWER HAS BEEN DETERMINED))
         (EVLIS (ACTION? (CAR ANSLIST)))                   ;THE ACTION INCLUDES BOTH THE THINGS TO BE DONE
         (PRINC '/.)                           ;AND THE INSTRUCTIONS FOR PRINTING A RESPONSE.
         (TERPRI)
(AND ANS-TEST? (GO TEST-LOOP))
         (DOBACKREF (CAR ANSLIST))                       ;DOBACKREF STORES AWAY DISCOURSE INFORMATION
         (RETURN T)))

;;;############################################################

(DEFUN AMBPUT (CODE)

       ;;PUTS IN THE JUNK FOR DISCOURSE IF THERE IS NO AMBIGUITY, SO
       ;;THERE IS NO NEED TO EVALUATE THE CODE A SECOND TIME WHEN
       ;;GIVING THE ANSWER.
       (COND (AMBIG CODE) (T (PLNR-JUNKIFY CODE))))

;;;############################################################

(DEFUN ANSAY (X)

       ;;GENERATES THE SYNTAX FOR ANSWER ACTIONS FROM A PHRASE.
       (LIST (CONS 'SAY X)))

;;;############################################################

(DEFUN ANSBUILD (PLAUS ACTION REDEDUCE)

       ;;BUILDS AN ANSWER NODE.  IF REDEDUCE IS NON-NIL, IT ADDS A
       ;;REDEDUCTION OF THE ANSWER, ADDING THE DISCOURSE JUNK TO THE
       ;;ACTION.
       (BUILD ANSNODE=
          (MAKESYM 'ANS)
          PLAUSIBILITY=
          PLAUS
          ANSRSS=
          RSS
          ACTION=
          (APPEND (COND ((AND AMBIG REDEDUCE (NOT (CQ DECLAR)))
                 (CONS (LIST 'THVAL2
                     NIL
                     (LIST 'PLNR-JUNKIFY
                           (LIST 'PLNRCODE?
                             (LIST 'QUOTE
                               RSS))))
                   ACTION))
                (T ACTION))
              (AND (REL? RSS)
               (NOT (CQ DECLAR))
               (LIST (LIST 'PUTPROP
                       (QUOTIFY (REL? RSS))
                       (QUOTIFY ANS)
                       (QUOTIFY 'REFER=)))))))

(DEFUN ANSCOMMAND (RSS)

       ;;ANSCOMMAND RESPONDS TO IMPERATIVES.
       (PROG (EXP ANS SUCCESS PLAN PLAN2)
         (SETQ EXP (PLNR-ANDORIFY RSS))                   ;PLNR-ANDORIFY COMBINES ANDS AND ORS INTO
         (PUTPROP RSS EXP 'PLNRCODE=)                   ;APPROPRIATE PLANNER THANDS AND THORS.
         (SETQ EXP (AMBPUT EXP))
         (SETQ EXP (COND ((EQ (CAR EXP) 'THAND)
                  (APPEND EXP
                      '((SETQ SUCCESS T)
                    (SETQ PLAN2 PLAN))))
                 (T (LIST 'THAND
                      EXP
                      '(SETQ SUCCESS T)
                      '(SETQ PLAN2 PLAN)))))
         (THVAL2 NIL
             (COND (AMBIG (APPEND EXP '((THFAIL))))           ;IN CASE OF MULTIPLE INTERPRETATION, THE SYSTEM
               (T EXP)))                       ;USES FAILURE TO WIPE OUT THE EFFECTS OF TRYING
         (RETURN                               ;OUT ONE OF
          (ANSBUILD (COND (SUCCESS (PLAUSIBILITY? RSS))           ;                   ;THEM. BEFORE
                  (T (DIFFERENCE (PLAUSIBILITY? RSS)       ;FAILING IT MARKS DOWN WHETHER IT SUCCEEDED AND
                         512.)))               ;SAVES THE PLAN FROM BACKTRACKING. PLNR-JUNKIFY
            (COND (SUCCESS (APPEND (REVERSE PLAN2)           ;PUTS ON THE JUNK FOR SAVING THE DISCOURSE
                           '((SAY OK))))           ;REFERENTS ETC. THE THIRD ARGUMENT TO ANSBUILD
                  (T '((SAY I CAN/'T))))               ;CAUSES THE SYSTEM TO GO BACK THROUGH THE
            T))))                           ;DEDUCTION TO GET THE DATA BASE STRAIGHT IF THIS
                                       ;ANSWER IS PICKED.  IT ALSO TAKES CARE OF THE
                                       ;BACKREF STUFF.

;;;############################################################

(DEFUN ANSDECLARE (RSS)

       ;;FOR DECLARATIVES.
       (COND
    ((OR? RSS)
     (GLOBAL-ERR I DON/'T UNDERSTAND DISJUNCTIVE DECLARATIVES))
    ((AND? RSS)
     (PROG (ANS)
           (SETQ ANS (MAPCAR 'ANSDECLARE (AND? RSS)))           ;CONJOINED DECLARATIVES ARE HANDLED BY DOING
           (RETURN
        (ANSBUILD
         (APPLY 'PLUS                           ;EACH ONE SEPARATELY.
            (MAPCAR 'PLAUSIBILITY? ANS))
         (CONS '(SAY I UNDERSTAND)
               (MAPCAN '(LAMBDA (X)
                    (DELETE '(SAY I UNDERSTAND)
                        (ACTION? X)))
                   ANS))
         NIL))))
    ((NOT (ISTENSE (PARSENODE? RSS) 'PRESENT))
     (GLOBAL-ERR I ONLY UNDERSTAND PRESENT TENSE DECLARATIVES))
    (T (ANSBUILD (PLAUSIBILITY? RSS)
             (CONS '(SAY I UNDERSTAND)
               (MAPCAR '(LAMBDA (X)
                        (LIST 'THADD
                          (QUOTIFY (ANSTHM X))
                          NIL))
                   (RELATIONS? RSS)))
             NIL))))                           ;ANSTHM GENERATES THE APPROPRIATE ASSERTION OR
                                       ;THEOREM.

;;;############################################################

(DEFUN ANSELIMINATE (ANSLIST)

       ;;ELIMINATES ANSWERS FROM LIST BY ASKING PERSON TO CLEAR UP
       ;;THE AMBIGUITIES.
       (PROG (AMB POSSIBILITIES XX)
         (OR (SETQ AMB (AMBIGUITIES? (ANSRSS? (CAR ANSLIST))))
         (BUG ANSELIMINATE -- NO AMBIGUITIES LIST))
    UP   (SETQ POSSIBILITIES (LIST (CAR AMB)))               ;POSSIBILITIES IS THE LIST OF POSSIBLE
         (MAPC                                ;INTERPRETATIONS FOR A SINGLE AMBIGUITY.  WE ARE
          '(LAMBDA (ANS)                            ;INSIDE A LOOP STARTING AT UP WHICH GOES THROUGH
        (AND (SETQ XX                           ;ALL THE DIFFERENT POSSIBLE AMBIGUITIES ON THE
               (PARSE-ASSOC (CAAR AMB)               ;LIST FOR THE FIRST ANSWER ON ANSLIST.
                    (AMBIGUITIES? (ANSRSS? ANS))))
             (NOT (MEMBER XX POSSIBILITIES))
             (SETQ POSSIBILITIES (CONS XX POSSIBILITIES))))    ;ON EACH ANSWER WE LOOK FOR POSSIBLE
          (CDR ANSLIST))                           ;INTERPRETATIONS FOR THE PARTICULAR NODE WHERE
         (COND ((CDR POSSIBILITIES) T)                   ;THE AMBIGUITY WAS CREATED.
           ((SETQ AMB (CDR AMB)) (GO UP))
           (T (BUG ANSELIMINATE -- NO CONFLICT)))
         (TERPRI)
         (SAY I/'M NOT SURE WHAT YOU MEAN BY ")
         (MAPC 'PRINT2
           (FROM (NB (CADDAR AMB)) (N (CADDAR AMB))))
         (SAY " IN THE PHRASE ")
         (MAPC 'PRINT2
           (FROM (NB (SETQ XX (PARENT? (CADDAR AMB))))
             (N XX)))
         (PRINC '"/.)
         (TERPRI)
         (SAY DO YOU MEAN:)
         (SETQ XX 0.)
         (MAPC '(LAMBDA (POSS) (PRINT (SETQ XX (ADD1 XX)))
                   (MAPC 'PRINT2 (CADR POSS)))           ;THE PARAPHRASE
           POSSIBILITIES)
         (PRINC '?)
         (TERPRI)
    READ (SETQ XX (READ))
         (COND ((OR (NOT (NUMBERP XX))
            (GREATERP XX (LENGTH POSSIBILITIES)))
            (TERPRI)
            (SAY PLEASE TYPE ONE OF THE NUMBERS)
            (TERPRI)
            (GO READ)))
         (SETQ POSSIBILITIES (NTH XX POSSIBILITIES))
         (RETURN
          (MAPBLAND
           '(LAMBDA (ANS)
         (COND
          ((OR
            (NOT
             (SETQ
              XX
              (PARSE-ASSOC (CAAR AMB)
                   (AMBIGUITIES? (ANSRSS? ANS)))))
            (EQUAL XX POSSIBILITIES))
           ANS)))
           ANSLIST))))

(DEFUN PARSE-ASSOC (OSS AMBIG-LIST)

       ;;; PARSE-ASSOC GOES THRU AMBIG-LIST LOOKING FOR AN INTERPRETATION
       ;;; WITH THE SAME PARSE NODE
       ;;;
       (PROG (ASS)
         (SETQ ASS (CAR (PARSENODE? OSS)))
    LOOP (COND ((NULL AMBIG-LIST) (RETURN NIL))
           ((EQ ASS (CAR (PARSENODE? (CAAR AMBIG-LIST))))
            (RETURN (CAR AMBIG-LIST))))
         (SETQ AMBIG-LIST (CDR AMBIG-LIST))
         (GO LOOP)))

;;;############################################################

(DEFUN ANSGEN (RSS)

       ;;ANSGEN GENERATES AN ANSWER FOR A SINGLE INTERPRETATION.
       (COND ((OR (CQ IMPER)
          (AND (CQ QUEST)
               (ISTENSE (PARSENODE? RSS) 'FUTURE)))           ;FUTURE QUESTIONS ARE TREATED LIKE COMMANDS.
          (ANSCOMMAND RSS))
         ((CQ DECLAR)
          (PROG (X)
            (RETURN (COND ((ERRSET (SETQ X (ANSDECLARE RSS)))
                   X)
                  ((EQUAL GLOBAL-MESSAGE
                      '(THAT ISN/'T
                         THE
                         KIND
                         OF
                         THING
                         I
                         CAN
                         BE
                         TOLD))
                   (ANSQUEST RSS))
                  ((ERR NIL))))))               ;THIS STRANGE CONSTRUCTION ALLOWS US A SECOND
         ((CQ QUEST) (ANSQUEST RSS))                   ;CHANCE ON DECLARATIVES ABOUT THINGS WHICH CAN'T
         ((BUG ANSGEN -- WHAT KIND OF SENTENCE IS THIS?))))           ;BE TOLD TO THE SYSTEM.  IF IT RUNS INTO ONE OF
                                       ;THEM IT TRIES TO ANSWER IT AS A QUESTION.

;;;#####################################################

(DEFUN ANSNAME (PHRASE)

       ;; THIS IS THE FUNCTION WHICH PARSES THE NAME PHRASES
       ;;GENERATED BY THE ANSWER ROUTINES SO THAT THEY CAN BE USED AS
       ;;REFERENTS FOR PRONOUNS (IT THEY ONE).  ITS INPUT IS A TWO-
       ;;LIST.  THE SECOND MEMBER IS THE ACTUAL REFERENT OF THE
       ;;PHRASE.  THE FIRST IS A LIST OF COMMANDS FOR SAYING THE NAME
       ;;OF AN OBJECT(S).  THE FIRST MEMBER OF THIS COMMAND LIST IS
       ;;GUARANTEED (BY ANSWER, VIA TW) TO BE A "SAY" COMMAND WHICH
       ;;ENDS WITH THE HEAD NOUN OF THE PHRASE.  NOTE THAT ANSNAME IS
       ;;CALLED BEFORE ONEIFYING AND ITIFYING AND THE REST OF THAT
       ;;CRAP.
       ;;;
       ;; ANSNAME WORKS BY CALLED PARSE NG ON THE FIRST COMMAND OF
       ;;THE LIST.  IT WANTS TO HAVE A PARSENODE AND AN OSSNODE BUILT
       ;;UP FOR THE OBJECTS.  HOWEVER, IT DOES NOT WANT REFERENT
       ;;ASSIGNMENT DONE BY SMNG3, SINCE IT ALREADY KNOWS THE
       ;;REFERENT.  THE FEATURE "ANSNAME" IS ADDED TO THE INITIAL NG
       ;;PARSE LIST SPECIFICALLY SO SMNG3 WILL IGNORE THIS NOUN
       ;;GROUP.
       ;;;
       ;; THE WAY ANSNAME WORKS IS THE DECLARE A LOT OF THE RELAVENT
       ;;PARSE FREE VARIABLES SO THAT IT LOOKS A LITTLE LIKE SHRDLU.
       ;;THE CRITICAL VARIABLES ARE:
       ;;; CUT - WHICH TELLS THE NG GUY HOW FAR TO GO.
       ;;; N - WHICH CONTAINS THE CURRENT SENTENCE.
       ;;; C - WHICH CONTAINS THE PARENT OF THE NEXT NODE.
       ;;;     WE WANT C TO BE NIL TO STOP THE NG PROGRAM FROM
       ;;;    CRAWLING OVER THE PARSE TREE.
       ;;;
       (PROG (ANSNODE C N CUT)
         (SETQ N (CDAAR PHRASE))                       ; CDR IS TO REMOVE "SAY"
         (SETQ ANSNODE (PARSE2 '(NG ANSNAME) T))               ; THE T SAYS NOT TO ATTACH THIS TO THE TREE
         (OR ANSNODE
         (RETURN (ERT ANSNAME:
                  FAILURE
                  TO
                  PARSE
                  ANSWER
                  NAME
                  BUT
                  IF
                  YOU
                  ONLY
                  EXPECT
                  THE
                  ANSWER
                  TO
                  BE
                  AN
                  ADJ,
                  PROCEED
                  THIS
                  AND
                  DON
                  'T
                  WORRY)))
         (SETQ ANSNAME (APPEND ANSNODE ANSNAME))               ; LEAVE NODE AROUND IT ACCESSABLE PLACE
         (PUTPROP (CAR (SM ANSNODE))
              (CADR PHRASE)
              'REFER=)))                       ; PUT THE REFERENT ON AS THE GUY GIVEN BY ANSWER

;;;############################################################

(DEFUN ANSNOREL (RSS)

       ;;FOR QUESTIONS WITH NO RELATIVE, LIKE "DID YOU PICK UP THE
       ;;BLOCK?"  OR "WHY DID YOU DO THAT?"
       (PROG (ANS TYPE CODE NODE VAR)
         (SETQ NODE (PARSENODE? RSS))
         (SETQ TYPE (COND ((ISQ NODE POLAR) 'POLAR)               ;THE TYPE SHOULD BE POLAR, WHY, WHERE, WHEN, OR
                  ((SETQ TYPE (GETR 'QADJ NODE))
                   (CAR (NB TYPE)))                   ;HOW.
                  ((BUG ANSNOREL -- FUNNY TYPE))))
         (PUTPROP (VARIABLE? RSS) T 'USED)
         (SETQ CODE
           (PLNR-DESCRIBE (RELATIONS? RSS)
                  (COND ((ISTENSE NODE
                          'PRESENT)
                     NIL)                   ;IN PRESENT TENSE CASES, WE DON'T LOOK FOR
                    ((SETQ VAR (VARIABLE? RSS))))  ;EVENTS. OTHERWISE WE LOOK FOR A SET OF
                  (LIST (VARIABLE? RSS))))           ;APPROPRIATE EVENTS NO MATTER WHAT THE TYPE.
         (PUTPROP RSS CODE 'PLNRCODE=)
         (RETURN
          (COND
           ((NOT VAR)
        (SETQ ANS (THVAL-MULT (AMBPUT CODE)))
        (ANSBUILD (PLUS (CAR ANS) (PLAUSIBILITY? RSS))
              (COND ((CADR ANS) '((SAY YES)))
                ((ISTENSE NODE 'MODAL)
                 '((SAY I DON/'T KNOW)))
                (T '((SAY NO))))
              T))
           ((SETQ ANS (THVAL-MULT (PLNR-FINDIFY 'ALL
                            VAR
                            (LIST VAR)
                            (AMBPUT CODE))))
        (ANSBUILD
         (COND ((CADR ANS)
            (PLUS (PLAUSIBILITY? RSS) (CAR ANS)))           ;AN ANSWER IS VERY IMPLAUSIBILE IF IT MENTIONS
               (T (DIFFERENCE (PLAUSIBILITY? RSS) 512.)))      ;AN EVENT THE SYSTEM CAN'T FIND.
         (COND ((NULL (CADR ANS))
            '((SAY I CAN/'TDISCUSSA NON-EXISTENT EVENT)))
               ((APPEND (AND (EQ TYPE 'POLAR)
                     '((SAY YES)))
                (LIST (LIST 'EVLIS
                        (LIST 'DESCRIBEVENT
                          (QUOTIFY (CADR ANS))
                          (QUOTIFY TYPE)))))))
         T))))))

;;;############################################################

(DEFUN ANSORDER (LIST)

       ;;ORDERS A LIST BY PLAUSIBILITY HIGHEST FIRST.
       (PROG (X Y)
    GO   (SETQ X LIST)
    UP   (COND ((NULL (CDR X)) (RETURN LIST))
           ((LESSP (PLAUSIBILITY? (CAR X))
               (PLAUSIBILITY? (CADR X)))
            (SETQ Y (CAR X))
            (RPLACA X (CADR X))
            (RPLACA (CDR X) Y)
            (GO GO))
           ((SETQ X (CDR X)) (GO UP)))))

;;;############################################################

(DEFUN ANSQUEST (RSS)

       ;;ANSQUEST ANSWERS ALL TYPES OF QUESTIONS BY SENDING THEM OUT
       ;;TO ANSREL OR ANSNOREL DEPENDING ON WHETHER THERE IS A REL.
       (COND
    ((OR (OR? RSS) (AND? RSS))
     (PROG (ANS)
           (SETQ ANS (MAPCAR 'ANSQUEST
                 (OR (AND? RSS) (OR? RSS))))
           (RETURN
        (ANSBUILD
         (APPLY 'PLUS
            (MAPCAR 'PLAUSIBILITY? ANS))
         (APPEND
          (AND (NOT (ISQ (PARSENODE? RSS) COMPONENT))
               '((SAY YOU/'RE TRYING TO CONFUSE ME/.)))
          (MAPCAN
           '(LAMBDA (QUEST)
             (APPEND
              '((TERPRI))
              (ANSAY
               (ELIZA
            (FROM (NB (PARSENODE? (ANSRSS? QUEST)))
                  (N (PARSENODE? (ANSRSS? QUEST))))))
              '((PRINC '?) (TERPRI))                   ;CONJOINED QUESTIONS ARE HANDLED BY SIMPLY
              (ACTION? QUEST)))                       ;REPEATING EACH PART AND ANSWERING IT
           ANS))
         NIL))))                           ;SEPARATELY.
    ((REL? RSS) (ANSREL RSS))
    (T (ANSNOREL RSS))))

;;;############################################################

(DEFUN ANSREL (RSS)

       ;;ANSREL HANDLES ALL QUESTIONS WITH A RELATIVE NG OF ANY TYPE
       (PROG (TYPE REL CODE PLAUS ANS PHRASE LENGTH NUM)
         (OR (SETQ REL (REL? RSS)) (BUG ANSREL -- NO REL))
         (SETQ PHRASE (CONS 'NIL
                (HEADPART (PARSENODE? REL))))           ;THIS IS FOR THE PART OF THE GENERATOR THAT WILL
         (SETQ TYPE (OR (QTYPE? REL)                   ;SUBSITUTE "ONE" FOR NOUN NAMES.  THE LEADING
                (QUANTIFIER? REL)
                (BUG ANSREL -- NO TYPE)))               ;NIL IS TO MAKE THIS PHRASE COMPATIBLE WITH THE
         (AND (EQ TYPE 'ALL)
          (PUTPROP RSS T 'NEGATIVE=))                   ;"SAY" PHRASES WHICH THE OTHER PARTS GENERATE.
         (PUTPROP                               ;UNIVERSALS ARE CONVERTED TO NOT THERE EXISTS
          RSS                               ;NOT.
          (SETQ
           CODE
           (PLNR-FINDIFY 'ALL
                 (VARIABLE? REL)
                 (LIST (VARIABLE? REL))
                 (PLNR-DESCRIBE (CONS RSS
                          (RELATIONS? REL))
                        (VARIABLE? REL)
                        (LIST (VARIABLE? REL)))))
          'PLNRCODE=)                           ;CONSING THE RSS ONTO THE THINGS TO BE DESCRIBED
         (SETQ ANS (THVAL-MULT (AMBPUT CODE)))               ;HAS THE EFFECT OF PUTTING THE RELATION INTO THE
         (SETQ PLAUS (CAR ANS))                       ;DESCRIPTION OF THE OBJECT. DISAMB PUTS IN THE
         (SETQ LENGTH (LENGTH (SETQ ANS (CADR ANS))))           ;JUNK IF THERE IS NO AMBIGUIT, AVOIDING HAVING
         (RETURN                               ;TO GOTHROUGH THE EVALUATION A SECOND TIME.
          (COND                               ;THVAL-MULT RETURNS A LIST  OF A PLAUSIBILITY
           ((EQ TYPE 'ALL)                           ;AND AN ANSWER.
        (ANSBUILD (PLUS PLAUS (PLAUSIBILITY? RSS))
              (COND ((NULL ANS) '((SAY YES)))
                ((CONS '(SAY NO, NOT)
                       (PREPPUT (NAMELIST PHRASE
                              'INDEF
                              ANS)))))
              T))
           ((EQ TYPE 'HOWMANY)
        (ANSBUILD (PLUS PLAUS (PLAUSIBILITY? RSS))
              (PREPPUT (NAMESUGAR LENGTH REL))
              T))
           ((MEMQ TYPE '(WHICH WHAT))
        (ANSBUILD (PLUS PLAUS
                (PLAUSIBILITY? RSS)
                (COND (ANS 512.) (0.)))
              (PREPPUT (NAMELIST PHRASE 'DEF ANS))
              T))
           ((EQ TYPE 'INDEF)
        (SETQ NUM (NUMBER? REL))
        (ANSBUILD
         (PLUS PLAUS (PLAUSIBILITY? RSS))
         (COND
          ((MEMQ NUM '(NS SG-PL))
           (COND
            ((NULL ANS)
             (COND ((ISTENSE (PARSENODE? RSS) 'MODAL)
                '((SAY I DON/'T KNOW)))
               (T '((SAY NO)))))
            (T
             (APPEND
              '((SAY YES,))
              (COND
               ((ISTENSE (PARSENODE? RSS) 'MODAL) NIL)
               ((PREPPUT
             (APPEND (AND (CDR ANS)
                      (APPEND (NAMESUGAR LENGTH REL)
                          '((PRINC ':))))
                 (NAMELIST PHRASE
                       'INDEF
                       ANS)))))))))
          ((NUMBERP NUM)
           (APPEND (COND ((EQ NUM LENGTH)
                  '((SAY YES,)))
                 ((GREATERP LENGTH NUM) NIL)           ;THIS IS THE CASE WHERE WE ARE CAGEY AND AVOID
                 ((ZEROP NUM) '((SAY NO,)))           ;ANSWERING YES OR NO.
                 (T '((SAY NO, ONLY))))
               (COND ((EQ NUM LENGTH) NIL)               ;THE NUMBER ISN'T REPEATED IF IT IS THE SAME AS
                 (T (PREPPUT (APPEND (NAMESUGAR LENGTH
                                REL)   ;THE NUMBER IN THE SPECIFICATION.
                             '((PRINC ':))))))
               (PREPPUT (NAMELIST PHRASE
                          'INDEF
                          ANS))))
          ((EQ (CAR NUM) 'EXACTLY)
           (COND ((EQ LENGTH NUM) '((SAY YES)))
             (T (CONS '(SAY NO,)
                  (PREPPUT (NAMESUGAR LENGTH RES))))))
          ((EQ (CAR NUM) '>)
           (CONS (COND ((GREATERP LENGTH NUM)
                '(SAY YES,))
                   ((ZEROP LENGTH) '(SAY NO,))
                   (T '(SAY NO, ONLY)))
             (PREPPUT (NAMESUGAR LENGTH REL))))
          ((EQ (CAR NUM) '<)
           (CONS (COND ((LESSP LENGTH NUM) '(SAY YES,))
                   (T '(SAY NO,)))
             (PREPPUT (NAMESUGAR LENGTH REL))))
          ((ERT ANSREL -- FUNNY NUMBER)))
         T))
           ((ERT ANSREL-- FUNNY TYPE))))))

;;;############################################################

(DEFUN ANSTHM (EXP)

       ;;GENRATES A THEOREM OR ASSERTION FOR AN EXPRESSION
       (PROG (NEG VARLIST BODY)
         (COND
          ((ATOM EXP) (NOTELL))                       ;NOTELL MARKS THAT THIS ISN'T THE KIND OF
          ((NOT (GET (CAR EXP) 'TELLABLE)) (NOTELL))           ;ASSERTION IT CAN HANDLE.  IT USES GLOBAL-ERR
          (T
           (SETQ NEG (NEGATIVE? RSS))
           (SETQ EXP (MAPCAR 'ANSTHMELEMENT
                 (PLNR-REMTIME EXP)))
           (RETURN
        (COND
         ((NOT (OR VARLIST NEG)) EXP)                   ;VAR AND NEG ARE SET AS FREE VARIABLES BY
         (T
          (PLNR-THCONSIFY
           VARLIST                           ;ANSTHMELEMENT WHICH ANALYZES EACH ELEMENT. IF
           EXP                               ;THERE ARE NO VARS, IT IS A SIMPLE ASSERTION.
           (COND (NEG (PLNR-PROGIFY NIL
                        (LIST BODY
                          '(THFAIL THEOREM))))
             (T BODY))))))))))

;;;############################################################

(DEFUN ANSTHMADD (OSS)
       (SETQ VARLIST (CONS (VARIABLE? OSS) VARLIST))
       (SETQ
    BODY
    (COND
     (BODY
      (PLNR-PROGIFY
       NIL
       (LIST BODY
         (PLNR-DESCRIBE (RELATIONS? OSS)
                (VARIABLE? OSS)
                (LIST (VARIABLE? OSS))))))
     (T (PLNR-DESCRIBE (RELATIONS? OSS)
               (VARIABLE? OSS)
               (LIST (VARIABLE? OSS))))))
       (PLNR-VAR (VARIABLE? OSS)))

;;;############################################################

(DEFUN ANSTHMELEMENT (X)
       (COND ((NOT (ATOM X)) X)
         ((TSS? X) (NOTELL))
         ((RSS? X) (NOTELL))
         ((NOT (OSS? X)) X)
         ((REFER? X) (ATOMIFY (REFER? X)))
         ((EQ (QUANTIFIER? X) 'ALL)
          (COND (NEG (NOTELL)) (T (ANSTHMADD X))))
         ((EQ (QUANTIFIER? X) 'NO)
          (SETQ NEG T)
          (ANSTHMADD X))
         ((EQ (QUANTIFIER? X) 'NDET) (ANSTHMADD X))
         ((NOT (EQ (QUANTIFIER? X) 'INDEF)) (NOTELL))
         ((ISQ (PARSENODE? X) ANY) (ANSTHMADD X))
         (T (GLOBAL-ERR YOU HAVE TO TELL ME WHICH))))

;;;############################################################

(DEFUN ANSUNIQUE (LIST)

       ;;THIS FUNCTION SHOULD ELIMINATE ANSWERS WHICH GIVE THE SAME
       ;;RESULT EVEN THHOUGH THEY INVOLVE DIFFERENT INTERPRETATIONS.
       ;;IT NEEDS TO CHECK FOR SIGNIFICANT DIFFERENCES, E.G.  IN WHAT
       ;;GETS PRINTED OR DONE, WHILE IGNORING INSIGNIFICANT ONES,
       ;;E.G.  THE NAMES OF ATOMS TO WHICH THINGS ARE ATTACHED.  FOR
       ;;THE MOMENT, IT JUST RETURNS THE LIST UNTOUCHED.
       LIST)

                           ;FROM BOTH THE INPUT SENTENCE AND THE ANSWER.

(SETQ ANS-TEST? NIL)

;;;############################################################

(DEFUN ATOMIFY (X) (COND ((ATOM X) X) ((CDR X) X) ((CAR X))))

;;;############################################################

(DEFUN CUTOFF (X)

       ;;FOR CUTTING # OFF OF CONCEPT NAMES TO GET ENGLISH WORDS
       (READLIST (CDR (EXPLODE X))))

;;;############################################################

(DEFUN DESCRIBEVENT (EVENT TYPE)
       (PROG (ANS)
         (SETQ EVENT (CAR EVENT))
         (RETURN
          (COND
           ((EQ TYPE 'WHERE)
        (GLOBAL-ERR I CAN/'T ANSWER "WHERE" QUESTIONS YET))
           ((EQ TYPE 'WHY)
        (COND ((EQ (GET EVENT 'WHY) 'COMMAND)
               '((SAY BECAUSE YOU TOLD ME TO)))
              (T (CONS '(SAY TO)
                   (NAMEACTION 'INFINITIVE
                       (GET EVENT
                        'WHY))))))
           ((EQ TYPE 'HOW)
        (MAPCAR '(LAMBDA (X)
                 (AND (EQ (GET X 'WHY) EVENT)
                      (SETQ ANS (CONS X ANS))))
            EVENTLIST)
        (COND
         ((NULL ANS)
          '((SAY I CAN/'T ANALYZE HOW I DID IT)))
         (T
          (APPEND
           '((SAY BY))
           (NAMEACTION 'ING (CAR ANS))
           (MAPCAN
            '(LAMBDA (X)
                 (CONS '(PRINC '/;)
                   (CONS '(SAY THEN)
                     (NAMEACTION 'ING X))))
            (CDR ANS))))))
           ((OR (EQ TYPE 'POLAR) (EQ TYPE 'WHEN))
        (COND
         ((EQ (GET EVENT 'WHY) 'COMMAND)
          (COND
           ((EQ EVENT (TOPLEVEL (CAR EVENTLIST)))
            '((SAY JUST NOW)))
           (T
            (CONS
             '(SAY BEFORE)
             (NAMEACTION
              'PAST
              (TOPLEVEL (CAR (FINDB EVENT EVENTLIST))))))))
         (T (CONS '(SAY WHILE)
              (NAMEACTION 'PRES-PAST
                      (TOPLEVEL EVENT))))))
           ((BUG DESCRIBEVENT -- FUNNY TYPE))))))

;;;############################################################

(DEFUN DISPUT (ASSERTION)

       ;;PUT THE SENTENCE NUMBER ON THE ASSERTION AS A WHO PROPERTY
       (OR (NOT DISCOURSE) (PUTPROP ASSERTION SENTNO 'WHO)))

;;;############################################################

(DEFUN ELIZA (NODE)

       ;;DOES THE OBVIOUS THING
       (PROG (XX NUM)
         (SETQ NUM (LENGTH (N NODE)))
         (RETURN
          (APPLY
           'APPEND
           (MAPLIST
        '(LAMBDA (WORD)
             (COND ((NOT (LESSP NUM (LENGTH WORD))) NIL)   ;THIS KLUDGE STOPS IT AT THE END OF THE NODE
                   ((SETQ XX (ASSQ (CAR WORD)
                           '((I YOU) (ME YOU)
                         (AM ARE) (ARE AM))))
                (CDR XX))                   ;WE RETURN LIST OF THE THING REALLY WANTED, SO
                   ((EQ (CAR WORD) 'YOU)               ;THE APPLY APPEND CAN GET RID OF THE EMPTY ONES.
                (SETQ XX (FINDMOTHER WORD NODE))       ;UNFORTUNATELY, FOR "YOU" IT IS NECESSARY TO
                (COND ((ISQ XX SUBJ) '(I))           ;DECIDE WHETHER IT SHOULD BE REPLACED BY "I" OR
                      ((ISQ XX OBJ) '(YOU))           ;"ME", ACCORDING TO WHETHER IT WAS PARSED AS AN
                      ((BUG ELIZA -- SUBJ OBJ))))      ;OBJECT OR SUBJECT. FINDMOTHER IS USED TO FIND
                   ((LIST (CAR WORD)))))               ;THE PARSE NODE. WORDS OTHER THAN THE SPECIAL
        (NB NODE))))))                           ;ONES GO THROUGH DIRECTLY.

;;;############################################################

(DEFUN ENOUGH-BETTER (ANS1 ANS2)
       (GREATERP (PLAUSIBILITY? ANS1)
         (PLUS (PLAUSIBILITY? ANS2) TIMID)))

;;;############################################################

(DEFUN FINDMOTHER (WORD NODE)

       ;;FINDMOTHER TAKES A PLACE IN THE SENTENCE AND A GRAMMAR NODE
       ;;(BOTH ARE ACTUALLY LISTS) AND FINDS THE SINGLE-WORD
       ;;CONSTITUTENT BEGINNING AT THAT PLACE IN THE SENTENCE.
       (COND ((AND (EQ WORD (NB NODE)) (EQ (CDR WORD) (N NODE))) NODE)
         (T (APPLY 'APPEND
               (MAPLIST '(LAMBDA (NOD) (FINDMOTHER WORD NOD))
                (H NODE))))))

;;;############################################################

(DEFUN HEADPART (NODE)
       (AND (SETQ PT NODE)
        (MOVE-PT DLC PV (NOUN))
        (FROM (NB NODE) (N PT))))                       ;EVERYTHING UP TO THE NOUN, FOR EXAMPLE "THE RED
                                       ;BLOCK" IN "THE RED BLOCK WHICH..." NOTE THAT
                                       ;NODE IS ACTUALLY A LIST OF NODE (A PROPER
                                       ;GRAMMAR POINTER).

;;;############################################################

(DEFUN LISTNAMES (PHRASE SPEC NAMES)

       ;;PHRASE IS THE INITIAL THING TO COMPARE FOR USING "ONE", SPEC
       ;;IS EITHER DEF OR INDEF, AND THE NAMES ARE OF DATA-BASE
       ;;OBJECTS.  LISTNAMES PUTS OUT AN ACTION LIST, AS WELL AS
       ;;PUTTING THINGS ONTO THE BACKREF.  IT IS CALLED AFTER THE
       ;;ANSWER HAS BEEN DECIDED ON.
       (PROG (COUNT EXAM X RES ANS COMMA?)
         (SETQ NAMES (MAPCAR '(LAMBDA (X) (NAMEOBJ X SPEC))
                 NAMES))                   ;NAMEOBJ RETURNS A LIST OF THE OBJECT AND THE
         (COND ((NULL NAMES) (RETURN '(SAY NOTHING))))           ;THIS PATCH MAY WELL BE TOTALLOUT OF PHASE WITH
    UP   (SETQ COUNT 1.)                           ;THE BACKREF HACKER - DDM 5-12-73 INSTRUCTIONS
         (SETQ EXAM (CAR NAMES))                       ;FOR NAMING IT.
         (SETQ NAMES (CDR NAMES))
    BACK (COND ((SETQ X (ASSOC (CAR EXAM) NAMES))
            (SETQ NAMES (DELQ X NAMES))
            (SETQ COUNT (ADD1 COUNT))
            (SETQ EXAM (LIST (CAR EXAM)
                     (APPEND (CADR X) (CADR EXAM))))
            (GO BACK)))                           ;WHEN THERE ARE TWO OBJECTS WITH THE SAME
         (SETQ RES (CONS (CONS (PLURALIZE (CAR EXAM) COUNT)
                   (CDR EXAM))
                 RES))                       ;ENGLISH DESCRIPTIONS, A JOINT OBJECT IS
         (AND NAMES (GO UP))                       ;PRODUCED COMBINING THE OBJECTS. THE COUNT IS
         (SETQ                                ;LATER USED TO PUT IN THE APPROPRIATE NUMBER,
          RES                               ;AND THE DESCRIPTION IS CHECKED TO SEE IF "ONE"
          (MAPCAR '(LAMBDA (PHRASE2)                    ;CAN BE USED. ADD THE ONE JUST PRODUCED TO THE
                   (COND ((PROPNAME (CAADR PHRASE2))       ;RESULT LIST. TRY ANOTHER.
                      (CAR PHRASE2))
                     (T (ANSNAME PHRASE2)           ;ANSNAME PARSES THE PHRASE AND PUTS THE
                    (ONECHECK (CAR PHRASE2)))))    ;ANSONE SUBSTITUTES "ONE" IF POSSIBLE
              RES))
         (SETQ ANS (CAR RES))
    OUTPUT
         (COND ((NULL (SETQ RES (CDR RES))) (RETURN ANS))
           ((CDR RES)
            (SETQ COMMA? T)
            (SETQ ANS (APPEND ANS
                      '((PRINC '/,))
                      (CAR RES))))
           ((SETQ ANS (APPEND ANS
                      (AND COMMA?
                       '((PRINC '/,)))
                      '((SAY AND))
                      (CAR RES)))))
         (GO OUTPUT)))

;;;############################################################

(DEFUN NAMEACTION (TENSE EVENT)

       ;;THIS FUNCTION SETS UP A LIST OF S-EXPRESSIONS
       ;;WHICH ARE RETURNED TO DESCRIBEVENT AND WHICH
       ;;WHEN EVALUATED WILL PRINT OUT AN ENGLISH DESCRIPTION
       ;;OF THE SINGLE, SIMPLE EVENT IMBEDDED IN THE LIST
       ;;"THASSERTION" WITH THE TENSE SPECIFIED
       (PROG (PLNR-FORM VERB OBJ1 OBJ2)
         (SETQ PLNR-FORM
           (CAR (CADDR (CADADR (GET EVENT
                        'THASSERTION))))           ;THE THASSERTION PROPERTY IS A LIST THAT
           VERB                               ;TYPICALLY LOOKS LIKE  "(NIL (2 (3 1 ((#GRASP
           (CUTOFF (CAR PLNR-FORM))                   ;:E2 :B6)))))"
           OBJ1
           (CADDR PLNR-FORM)
           OBJ2
           (CADDDR PLNR-FORM))
(SETQ FOOBAR
         (COND ((EQ VERB 'CLEARTOP)
            (CONS (SAYIFY (VBFIX 'CLEAN NIL))               ;SAYIFY WRAPS THE FUNCTION "SAY" ARROUND A LIST
              (PRON-PRT 'OFF OBJ1)))               ;OF WORDS AND RETURNS THE RESULTING S-EXPRESSION
           ((EQ VERB 'GET-RID-OF)                   ;NAMELIST-EVALED '(NIL) 'DEF  RETURNS A LIST (!!!) OF
            (CONS (SAYIFY (VBFIX 'GET T)               ;S-EXPRESSIONS
                  'RID
                  'OF)
              (NAMELIST-EVALED '(NIL) 'DEF OBJ1)))
           ((EQ VERB 'GRASP)
            (CONS (SAYIFY (VBFIX 'GRASP T))
              (NAMELIST-EVALED '(NIL) 'DEF OBJ1)))
           ((EQ VERB 'PICKUP)
            (CONS (SAYIFY (VBFIX 'PUT T))
              (PRON-PRT 'UP OBJ1)))
           ((EQ VERB 'PUTON)
            (APPEND (CONS (SAYIFY (VBFIX 'PUT T))
                  (NAMELIST-EVALED '(NIL)
                        'DEF
                        OBJ1))
                (CONS '(SAY ON)
                  (NAMELIST-EVALED '(NIL)
                        'DEF
                        OBJ2))))
           ((EQ VERB 'STACKUP)
            (CONS (VBFIX STACK T) (PRON-PRT 'UP OBJ1)))
           ((EQ VERB 'RAISEHAND) NIL)
           (T (BUG NAMEACTION
               -
               I
               DON/'T
               KNOW
               WHAT
               TO
               DO
               WITH
               THE
               VERB
               I
               GOT))))
(RETURN FOOBAR)))

;;;############################################################

(DEFUN NAMELIST (ONE SPEC LISTX)

       ;;GENERATES A LIST OF EXPRESSIONS TO BE EVALUATED WHICH WILL
       ;;CAUSE THE APPROPRIATE NAMELIST TO BE PRINTED OUT.  THE
       ;;ARGUMENTS ARE JUST THOSE TO LISTNAMES.
       (LIST (LIST 'EVLIS
           (LIST 'LISTNAMES
             (QUOTIFY ONE)
             (QUOTIFY SPEC)
             (QUOTIFY LISTX)))))                   ;A TYPICAL CALL WOULD RESULT IN A VALUE OF
                                       ;((EVLIS(LISTNAMES '(A RED BLOCK) 'INDEF '(:B1
                                       ;:B7)))) WHICH WOULD BE EVALUATED LATER. NOTE
                                       ;THAT LISTNAMES WILL IN TURN PRODUCE A LIST OF
                                       ;EXPRESSIONS TO BE EVALUATED, WHICH WILL BE
                                       ;CAUGHT BY THE EVLIS.  CONFUSING?

;;;############################################################

(DEFUN NAMELIST-EVALED (ONE SPEC LISTX)
    (PROG (F)
    (SETQ F (LIST 'LISTNAMES
    (QUOTIFY ONE)
    (QUOTIFY SPEC)
    (QUOTIFY LISTX)))
    (RETURN (LIST (EVAL F)))))

;;;############################################################

(DEFUN NAMENUM (X)

       ;;GENERATES NUMBER NAMES
       (OR (NTH (ADD1 X)
        '(NONE ONE
               TWO
               THREE
               FOUR
               FIVE
               SIX
               SEVEN
               EIGHT
               NINE
               TEN))
       (GLOBAL-ERR I CAN/'T COUNT THAT HIGH)))

;;;############################################################

(DEFUN NAMEOBJ (ITEM SPEC)

       ;;NAMES THE OBJECT IN ENGLISH -- GENERATES LIST OF THINGS TO
       ;;BE EVALUATED.  SPEC IS EITHER 'INDEF OR 'DEF
       (PROG (TYPE: TYPELIST TYPE NAME: COLOR: COLORLIST SIZE:
          SIZELIST CUBE NAME X)
         (AND (SETQ X (ASSOC ITEM
                 '((:SHRDLU I) (:FRIEND YOU))))
          (RETURN (LIST (ANSAY (CDR X)) (LIST ITEM))))           ;  SPECIAL CASE CHECK
         (THVAL2 NIL
             '(THGOAL (#NAMEOBJ) (THUSE TC-NAMEOBJ)))
         (OR TYPELIST
         (ERT NAMEOBJ -- OBJECT WITH NO #IS ASSERTION))
         (DISPUT TYPE:)                           ;DISPUT CHECKS TO SEE IF DISCOURSE IS BEING
         (COND ((EQ (SETQ TYPE (CADDAR TYPE:)) '#NAME)           ;KEPT, AND IF SO PUTS THE RELEVANT SENTENCE
            (RETURN (LIST (ANSAY (LIST ITEM)) (LIST ITEM))))   ;NUMBER AS A PROPERTY ON THE ASSERTION. A NAME
           ((MEMQ '#PROPERTY (GET TYPE 'SYS))               ;IS ITS OWN NAME
            (RETURN (LIST (ANSAY (LIST (CUTOFF ITEM)))
                  (LIST ITEM))))               ;CUTOFF CUTS THE # OFF OF NAMES LIKE #RED AND
           ((NOT (CDR TYPELIST))                   ;#POINTED WHICH ARE USED FOR PROPERTIES.
            (RETURN (LIST (ANSAY (LIST 'THE
                           (CUTOFF TYPE)))
                  (LIST ITEM))))               ; THERE IS ONLY ONE OBJECT OF THIS TYPE (E.G.
           (CUBE (SETQ NAME '(CUBE)))                   ;TABLE, BOX, HAND)
           ((SETQ NAME (LIST (CUTOFF TYPE)))))               ;E.G. #BLOCK BECOMES BLOCK.
         (AND NAME:
          (RETURN (LIST (ANSAY (LIST 'THE
                         (CAR NAME)
                         'NAMED
                         (CADDAR NAME:)))
                (LIST ITEM))))                   ;E.G. THE BLOCK NAMED SUPERBLOCK.
         (DISPUT COLOR:)                           ;IF WE HAVEN'T RETURNED YET, COLOR
         (SETQ NAME (CONS (CUTOFF (CADDAR COLOR:)) NAME))           ;WILL BE NEEDED TO FULLY DESCRIBE THE OBJECT.
         (OR (CDR COLORLIST)
         (RETURN (LIST (ANSAY (CONS 'THE NAME))
                   (LIST ITEM))))                   ;THERE ARE NO OTHERS OF THE SAME COLOR. IF THERE
         (SETQ NAME (CONS SIZE: NAME))                   ;ARE, WE MUST USE SIZE AS WELL
         (RETURN
          (LIST
           (COND
        ((NULL (CDR SIZELIST))
         (ANSAY (CONS 'THE NAME)))                   ;THE SIZE MANAGES TO FINISH SPECIFYING IT.
        ((EQ SPEC 'INDEF)
         (ANSAY (CONS 'A NAME)))                   ;IN THE INDEFINITE CASE WE DON'T CARE IF THIS
        ((SETQ X (THVAL2 NIL                       ;ISN'T A FULL SPECIFICATION.
                 '(THFIND ALL
                      $?X
                      (X (Y ITEM))
                      ($G (#SUPPORT $?Y $?X)))))
         (CONS (APPEND '(SAY THE) NAME)
               (CONS '(SAY WHICH SUPPORTS)
                 (LISTNAMES NIL 'INDEF X))))           ;IF IT SUPPORTS ANYTHING, NAME THEM.
        ((CONS
          (APPEND '(SAY THE) NAME)
          (CONS
           '(SAY WHICH IS TO THE RIGHT OF)
           (COND ((SETQ
               X
               (THVAL2 NIL
                   '(THFIND ALL
                        $?X
                        (X (Y ITEM))
                        ($G (#AT $?X ?))           ;MAKE SURE IT IS AN ITEM WITH A LOCATION.
                        ($G (#LOC #RIGHT $?Y $?X)
                        (THUSE TC-LOC)))))
              (LISTNAMES NIL 'INDEF X))
             ('((SAY NOTHING))))))))
           (LIST ITEM)))))

;;;############################################################

(DEFPROP TC-NAMEOBJ
     (THCONSE ((X ITEM) TYPE COLOR NAME SIZE Y Z)               ; PLANNER IS CALLED TO SEE HOW MANY OBJECTS FIT
          (#NAMEOBJ)                           ;VARIOUS FORMS OF THE DESCRIPTION  IT USES
          ($G (#IS $?X $?TYPE))                       ;FAILURE TO LOOP THROUGH THEM, SUCCESSIVELY
          (SETQ TYPE: THVALUE)                       ;FILTERING THEM THROUGH GOALS IN WHICH THEY ARE
          (OR (SETQ CUBE (AND (EQ $?TYPE '#BLOCK)           ;FORCED TO MATCH THE CHOSEN ITEM  THIS IS VALUE
                      (#EQDIM $?X)))               ;IS THE ENTIRE TYPE ASSERTION FOR
              T)                           ;SPECIAL CHECK TO CALL EQUIDIMENSIONAL BLOCKS
          (THCOND (($G (#NAME $?X $?NAME))               ;"CUBE" THE OR IS TO PREVENT PLANNER FROM
               (SETQ NAME THVALUE))                   ;FAILING THE CHOSEN OBJECT.  IT IS SAVED SO THE
              (($G (#IS $?Y $?TYPE))               ;SENTENCE NUMBER CAN BE PUT ON ITS PROPERTY LIST
               (OR (NOT CUBE) (#EQDIM $?Y))               ;IF THE FACT IS USED IN THE DESCRIPTION.  IF THE
               (SETQ TYPELIST (CONS $?Y TYPELIST))           ;ITEM HAS A NAME, NO MORE IS NEEDED. FIND
               ($G (#COLOR $?X $?COLOR))               ;SOMETHING ELSE OF THE SAME TYPE. NOTE THAT THIS
               (SETQ COLOR: THVALUE)               ;WILL FIND THE ITEM ITSELF ALONG WITH THE OTHERS
               ($G (#COLOR $?Y $?COLOR))               ;AND THUS PUT IT ON THE LIST.  THIS KEEPS A LIST
               (SETQ COLORLIST (CONS $?Y COLORLIST))       ;OF ALL THE OBJECTS WHICH MAKE IT THIS FAR.
               (SETQ SIZE: (NAMESIZE (SIZE $?X)))           ;NOTE THAT SINCE IT IS SETQ INSTEAD OF THSETQ,
               (EQ SIZE: (NAMESIZE (SIZE $?Y)))           ;BACKUP DOESN'T UNDO IT. ANYTHING WHICH MAKES IT
               (SETQ SIZELIST (CONS $?Y SIZELIST))           ;THIS FAR IS BOTH THE SAME TYPE AND THE SAME
               (THFAIL))))                       ;COLOR WE DON'T WANT TO CHECK FOR EXACT EQUALITY
     THEOREM)                               ;OF SIZE, JUST WHETHER THEY WOULD BE CALLED THE
                                       ;SAME THING.  THE THFAIL SENDS IT BACK UP
                                       ;SEARCHING FOR MORE.

;;;############################################################

(DEFUN NAMESIZE (X)
       (OR (NUMBERP X) (SETQ X (APPLY 'PLUS X)))               ;ACCEPTS EITHER SINGLE NUMBER OR LIST OF
       (COND ((GREATERP X 383.) 'LARGE)                       ;DIMENSIONS.
         (T 'SMALL)))

;;;############################################################

(DEFUN NAMESUGAR (NUM OSS)

       ;;GENERATES PHRASES LIKE "THREE OF THEM"
       (PROG (VAGUE)
         (SETQ VAGUE (MEMQ '#VAGUE (MARKERS? OSS)))               ;VAGUE IS FOR WORDS LIKE "ANYTHING",
         (RETURN
          (LIST
           (CONS 'SAY                           ;"SOMETHING", "NOTHING" TO AVOID SAYING "OF
             (COND ((AND VAGUE (ZEROP NUM)) '(NOTHING))           ;THEM" WHEN IT ISN'T APPROPRIATE.
               ((CONS (NAMENUM NUM)
                  (COND (VAGUE (COND ((EQUAL NUM 1.)
                              '(THING))
                             ('(THINGS))))
                    ('(OF THEM)))))))))))

;;;############################################################

(DEFUN NOTELL NIL
       (GLOBAL-ERR THAT
           ISN
           'T
           THE
           KIND
           OF
           THING
           I
           CAN
           BE
           TOLD))

;;;############################################################

(DEFUN ONECHECK (ITEM)

       ;;CHECKS TO SEE IF A SUBSTITUTE "ONE" CAN BE USED.  ITEM IS A
       ;;SINGLE "SAY" PHRASE.  "PHRASE" IS A FREE VARIABLE IN
       ;;LISTNAMES
       (PROG (ANS OLD NEW)
         (AND (EQUAL PHRASE '(NIL))
          (SETQ PHRASE (CAR ITEM))
          (RETURN ITEM))
         (SETQ OLD (REVERSE PHRASE))
         (SETQ NEW (REVERSE (CAR ITEM)))
         (OR (EQ (CAR OLD) (CAR NEW))
         (EQ (CAR OLD) (GET (CAR NEW) 'ROOT))
         (EQ (CAR NEW) (GET (CAR OLD) 'ROOT))
         (RETURN ITEM))                           ;IF THE NOUNS DON'T MATCH, JUST RETURN WHAT YOU
    LOOP (SETQ NEW (CDR NEW))                       ;GOT. MATCHING INCLUDES PLURALS TO THEIR
         (SETQ OLD (CDR OLD))                       ;CORRESPONDING SINGULAR FORMS.
         (COND
          ((OR (NULL NEW)
           (NULL OLD)
           (ISQ NEW NUM)
           (ISQ NEW DET)
           (NOT (EQ (CAR NEW) (CAR OLD))))
           (RETURN
        (CONS (REVERSE (CONS (COND ((ISQ (LAST (CAR ITEM))
                         NPL)
                        'ONES)
                       (T 'ONE))
                     NEW))
              (CDR ITEM)))))
         (GO LOOP)))

;;;############################################################

(DEFUN ORDNAME (NUM)

       ;;NAME AN ORDINAL
       (COND ((EQUAL NUM 1.) 'ONCE)
         ((EQUAL NUM 2.) 'TWICE)
         ((READLIST (NCONC (EXPLODE (NAMENUM NUM))
                   '(/ T I M E S))))))

(DEFLIST PAST (PUT PUT))

;;;############################################################

(DEFUN PLNR-ANDORIFY (RSS)

       ;;TURNS AN RSS INTO A COLLECTION OF PLANNER CODE FOR A COMMAND
       (COND ((AND? RSS)
          (PLNR-PROGIFY NIL
                (MAPCAR 'PLNR-ANDORIFY
                    (AND? RSS))))
         ((OR? RSS)
          ;;;(PLNR-ORIFY NIL
          ;;;            (MAPCAR 'PLNR-ANDORIFY (OR? RSS)))
          (ert sorry, plnr-orify not written))
         ((PLNR-PROGIFY NIL
                (MAPCAR 'PLNR-GOALIFY
                    (RELATIONS? RSS))))))

;;;############################################################

(DEFUN PREPPUT (X)
       (COND ((AND (REL? RSS)
           (SETQ PT (PARSENODE? (REL? RSS)))
           (ISQ (MOVE-PT U) PREPG))
          (CONS (CONS 'SAY
              (FROM (NB PT) (NB (MOVE-PT DLC))))
            X))
         (T X)))

;;;############################################################

(DEFUN PLURALIZE (ITEM NUM)

       ;;CONVERTS A SINGULAR NOUNPHRASE OR "ONCE" STATEMENT INTO
       ;;PLURAL.
       (COND ((GREATERP 2. NUM) ITEM)
         (T (COND ((MEMQ 'A (CAR ITEM))
               (CONS (PLURALMAKE (SUBST (NAMENUM NUM)
                        'A
                        (CAR ITEM)))
                 (CDR ITEM)))
              ((MEMQ 'ONCE (CAR ITEM))
               (CONS (SUBST (ORDNAME NUM)
                    'ONCE
                    (CAR ITEM))
                 (CDR ITEM)))
              ((BUG PLURALIZE -- FUNNY ITEM))))))

;;;############################################################

(DEFUN PLURALMAKE (PHRASE)

       ;;CONVERTS SINGULAR PHRASE TO PLURAL
       (PROG (SING PLURAL)
         (OR (ISQ (SETQ SING (LAST PHRASE)) NOUN)
         (BUG PLURALMAKE -- NO NOUN))
         (SETQ PLURAL (MAKNAM (NCONC (EXPLODE (CAR SING))
                     '(S))))
         (OR (GET PLURAL 'FEATURES)
         (BUILDWORD PLURAL
                '(NOUN NPL)
                (SM SING)
                (CAR SING)))
         (RETURN (SUBST PLURAL (CAR SING) PHRASE))))

;;;################################################################

(DEFUN PRON-PRT (PARTICLE NG)

       ;;THIS IS EVENTUALLY SUPPOSED TO BE THE PLACE FOR THE
       ;;PRONOUN-PARTICLE-INTERACTION MAGIC TO HAPPEN.
       ;;(IE. "CLEAR OFF THE BLOCK." VS. "CLEAR IT OFF" SINCE "CLEAR OFF IT."
       ;;IS UNGRAMMATICAL AND "CLEAR THE BLOCK OFF." WOULD NOT BE
       ;;APPROPRIATE IN CASES OF HEAVY-NP'S)
       ;;;
       ;;AT THE MOMENT, FOR SIMPLICITY'S SAKE, I'VE IGNORED THE
       ;;PROBLEM AND THE PARTICLE IS ALWAYS PUT BEFORE THE NG.
       ;;;
       (CONS (LIST 'SAY PARTICLE)
         (NAMELIST-EVALED  '(NIL) 'DEF NG)))

;;;################################################################

(DEFUN SAYIFY FEXPR (EXP-LIST)
       (CONS 'SAY
         (MAPCAR '(LAMBDA (Y) (EVAL Y)) EXP-LIST)))

;;;############################################################

(DEFUN THVAL-MULT (CODE)

       ;;DOES A THVAL WITH DIFFERENT VALUES OF WHO (I.E.  NIL
       ;;(EVERYTHING I KNOW), 'HE (EVERYTHING HE KNOWS) , AND THE
       ;;PREVIOUS SENTENCE.) USED TO TELL IF AN ANSWER COULD HAVE
       ;;BEEN GENERATED WITH HIS KNOWLEDGE TO SEE WHETHER HE REALLY
       ;;MEANT THIS INTERPRETATION.  RETURNS A LIST OF A PLAUSIBILITY
       ;;AND THE RESULT OF THE THVAL USING ALL THE KNOWLEDGE IN THE
       ;;DATA BASE.
       (PROG (ANS)
         (SETQ ANS (THVAL2 NIL CODE))
         (OR (AND AMBIG DISCOURSE) (RETURN (LIST 0. ANS)))           ;THIS FEATURE IS ONLY RELEVANT IN DISCOURSE AND
         (OR (EQUAL ANS (THVAL2 'HE CODE))                   ;WHEN THERE ARE AMBIGUITIES.
         (RETURN (LIST 256. ANS)))                   ;GIVE A VALUE OF 400 IF HE COULDN'T HAVE
         (RETURN (COND ((EQUAL ANS
                   (THVAL2 (LIST (*DIF SENTNO 2.)      ;ANSWERED IT AT ALL.
                         (ADD1 SENTNO))
                       CODE))
                (LIST 0. ANS))                   ;PLAUSIBILITY IS 0 IF HE COULD HAVE ANSWERED IT
               ((LIST 128. ANS))))))               ;WITH RECENTLY MENTIONED INFORMATION. 200 IF HE
                                       ;COULD ANSWER IT BUT NOT WITH RECENT INFO.

;;;############################################################

(DEFUN TOPLEVEL (EVENT)

       ;;FINDS THE TOP LEVEL EVENT GOING ON AT THE TIME
       (COND ((EQ (GET EVENT 'WHY) 'COMMAND) EVENT)
         (T (TOPLEVEL (GET EVENT 'WHY)))))

;;;############################################################

(DEFUN FINDCHOOSE (OSS X ANS2)
       (PROG (HAVE NEED XX ANS PLNRCODE LOOP)
         (AND (REFER? OSS) (RETURN (ATOMIFY (REFER? OSS))))
         (COND
          ((AND? OSS)
           (RETURN
        (MAPBLAND '(LAMBDA (OSS)
                   (PROG (Y)
                     (SETQ Y (FINDCHOOSE OSS
                                 X
                                 ANS2))
                     (SETQ ANS2 (APPEND Y ANS2))
                     (RETURN Y)))
              (AND? OSS))))
          ((OR? OSS)
           (SETQ LOOP (OR? OSS))
           (RETURN (PROG (Y)
            GO   (COND ((SETQ Y (FINDCHOOSE (CAR LOOP)
                            X
                            ANS2))
                    (RETURN Y))
                   ((SETQ LOOP (CDR LOOP))
                    (GO GO)))))))
         (SETQ PLNRCODE (PLNR-DESCRIBE (RELATIONS? OSS)
                       (VARIABLE? OSS)
                       (LIST (VARIABLE? OSS))))
         (PUTPROP OSS PLNRCODE 'PLNRCODE=)
         (COND
          ((EQ (QUANTIFIER? OSS) 'ALL)
           (RETURN
        (ATOMIFY (THVAL (PLNR-FINDIFY 'ALL
                          (VARIABLE? OSS)
                          (LIST (VARIABLE? OSS))
                          PLNRCODE)
                NIL))))
          ((OR (AND? OSS) (OR? OSS)) (GO CONJ)))
         (OR (ATOM (SETQ NEED (NUMBER? OSS)))
         (SETQ NEED (CADR NEED)))
         (AND (EQ NEED 'NS) (SETQ NEED 1.))
         (SETQ HAVE 0.)
    GO   (COND
          ((OR (EQ HAVE NEED)
           (AND (GREATERP HAVE NEED)
            (SETQ ANS (FINDREDUCE ANS
                          (DIFFERENCE HAVE
                              NEED)))))
           (GO DONE))
          ((EQ X 'NOMORE) (RETURN NIL))
          ((SETQ
        HAVE
        (LENGTH
         (SETQ
          ANS
          (APPEND
           (THVAL
            (PLNR-FINDIFY
             (LIST 1. (DIFFERENCE NEED HAVE) T)
             (VARIABLE? OSS)
             (LIST (VARIABLE? OSS))
             (PLNR-PROGIFY
              NIL
              (APPEND (LIST PLNRCODE)
                  (SUBST (VARIABLE? OSS)
                     '***
                     '((NOT (OR (MEMQ (THV ***) ANS)
                        (MEMQ (THV ***)
                              ANS2)))))
                  (AND X
                   (SUBST (VARIABLE? OSS)
                      '*
                      (CAR X))))))
            THALIST)
           ANS))))
           (SETQ X (COND (X (CDR X)) ('NOMORE)))
           (GO GO)))
    CONJ (SETQ LOOP (OR (AND? RSS) (OR? RSS)))
    UP   (COND ((GET (CAR LOOP) 'REFER)
            (SETQ ANS (APPEND (GET (CAR LOOP) 'REFER)
                      ANS)))
           ((SETQ XX
              (FINDCHOOSE (CAR LOOP) X (APPEND ANS2 ANS)))
            (SETQ ANS (APPEND XX ANS))))
         (COND ((AND ANS (OR? OSS)))
           ((SETQ LOOP (CDR LOOP)) (GO UP))
           (ANS)
           ((RETURN NIL)))
    DONE (AND (ATOM (VARIABLE? OSS))
          (PUTPROP (VARIABLE? OSS)
               (REVERSE ANS)
               'BIND))
         (RETURN (ATOMIFY (REVERSE ANS)))))

;;;############################################################

(DEFUN FINDNUM (X)
       (COND ((NUMBERP X) X)
         ((EQ (CAR X) 'EXACTLY)
          (LIST (CADR X) (ADD1 (CADR X)) NIL))
         ((EQ (CAR X) '>) (ADD1 (CADR X)))
         ((EQ (CAR X) '<) (CADR X))
         ((EQ X 'NS) 1.)
         ((EQ X 'NPL) 2.)
         ((ERT FINDNUM))))

;;;############################################################

(DEFUN FINDREDUCE (X Y)
       (PROG NIL
    UP   (SETQ X (CDR X))
         (COND ((ZEROP (SETQ Y (SUB1 Y))) (RETURN X)) ((GO UP)))))

;;;############################################################

(DEFPROP IASS
     (LAMBDA (X)
         (PROG (XX)
               (OR (SETQ XX
                 (CADR (SASSQ X
                          (CADR (CADDDR ANS))
                          (FUNCTION SASS))))
               (RETURN T))
               (SAY /
BY)               (PRINC (COND ((EQ X (Quote IT)) (Quote "IT"))
                    ((MEMQ (Quote THEY) (FROM SENT NIL))
                     (Quote "THEY"))
                    ((Quote "THEM"))))
               (SAY , I ASSUME YOU)
               (PRINC (Quote MEAN))
               (MAPC (FUNCTION PRINT2) (PARAP XX))
               (RETURN (PRINC (Quote /./
)))))    EXPR)

;;;############################################################

(DEFUN MUNG (LIST MUNG)
       (SETQ MUNG (LIST 'QUOTE MUNG))
       (AND DISCOURSE (SETQ LIST (CADDR LIST)))
       (COND ((EQ (CAAR (CDDDR LIST)) 'THAMONG)
          (RPLACD (CDAR (CDDDDR LIST)) MUNG))
         ((RPLACD (CDDDR LIST)
              (CONS (LIST 'THAMONG
                  (LIST 'THV
                    (CADR (CADDR LIST)))
                  MUNG)
                (CDDDDR LIST))))))

;;;############################################################

(DEFUN NAMEVENT (EVENT TYPE)
       (PROG (THALIST EV SUBJ OBJ1 OBJ2)
         (OR (SETQ EV (GET (GET EVENT 'TYPE)
                   'NAMEVENT))
         (ERT NAMEVENT))
         (OR
          (THVAL (LIST 'THGOAL
               (COND ((EQ (CAR EV) 2.)
                  '(? $?EVENT))
                 ((EQ (CAR EV) 3.)
                  '(? $?EVENT (THNV SUBJ)))
                 ((EQ (CAR EV) 'I3)
                  '(? $?EVENT (THNV OBJ1)))
                 ((EQ (CAR EV) 4.)
                  '(? $?EVENT
                      (THNV SUBJ)
                      (THNV OBJ1)))
                 ((EQ (CAR EV) 'I4)
                  '(? $?EVENT
                      (THNV OBJ1)
                      (THNV OBJ2)))
                 ((EQ (CAR EV) 5.)
                  '(? $?EVENT
                      (THNV SUBJ)
                      (THNV OBJ1)
                      (THNV OBJ2)))
                 ((ERT NAMEVENT DATA))))
             (SETQ THALIST
               (LIST (LIST 'EVENT EVENT)
                 (LIST 'SUBJ
                       (COND ((NUMBERP (CAR EV)) NIL)
                         ('I)))
                 (LIST 'OBJ1 NIL)
                 (LIST 'OBJ2 NIL))))
          (ERT NAMEVENT THVAL))
         (MAPC
          (FUNCTION (LAMBDA (X)
                (AND (CADR X)
                     (SET (CAR X)
                      (ert undef-fn: names NAMES (LISTIFY (CADR X))
                         'EV)))))
          (CDR THALIST))
         (SETQ ANSBACK2 (OR ANSBACK T))
         (SETQ LASTANSEV EVENT)
         (RETURN (APPEND (COND ((EQ TYPE 'PAST) SUBJ)
                   ((EQ TYPE 'TO)
                    '(TO)))
                 (EVAL (CADR EV))))))

;;;############################################################

(DEFUN PARAP () (ERT YOU LOSE, PARAP IS FLUSHED UNTILL IT CAN BE FIGURED OUT))

;;;(DEFPROP
;;; PARAP
;;; (LAMBDA (X)
;;;  (PROG (Y)
;;;    (SETQ Y
;;;          (COND ((OR (EQ X (GET (Q IT) (Q LASTT)))
;;;             (EQ X (GET (Q THEY) (Q LASTT))))
;;;             (Q (THE SAME THING)))
;;;            ((ert iassume: some implementation dependant code used to
;;;be executed at this point and no one has figured out yet quite what it was
;;;trying to accomplish/. sorry/, you lose))
;;;                    ;;; ( SETQ Y
;;;            ;;;       (SUBLIS '((YOU #777777
;;;            ;;;              PNAME
;;;            ;;;              (-29527900160.))
;;;            ;;;             (I #777777 PNAME (-20603830272.))
;;;            ;;;             (ARE #777777
;;;            ;;;              PNAME
;;;            ;;;              (-33499906048.)))
;;;            ;;;           (OR (FASSOC (FUNCTION CADDDR)
;;;            ;;;                   X
;;;            ;;;                   BACKREF
;;;            ;;;                   (FUNCTION NILL))
;;;            ;;;               (FASSOC (FUNCTION CADDDR)
;;;            ;;;                   X
;;;            ;;;                   BACKREF2
;;;            ;;;                   (FUNCTION NILL)))))
;;;            ;;;  (FROM (CADR Y) (CADDR Y)))
;;;            ((ERT IASSUME))))
;;;    (RETURN (COND ((MEMQ (CAR Y)
;;;                 (Q (THE THOSE THIS THAT THESE YOUR MY)))
;;;               Y)
;;;              ((MEMQ (CAR Y) (Q (A AN SOME ANY)))
;;;               (CONS (Q THE) (CDR Y)))
;;;              ((MEMQ (CAR Y) (Q (SOMETHING ANYTHING)))
;;;               (CONS (Q THE)
;;;                 (CONS (Q THING)
;;;                   (COND ((NULL (CDR Y)) NIL)
;;;                     ((EQ (CADR Y)
;;;                          (Q (WHICH THAT)))
;;;                      (CDR Y))
;;;                     ((CONS (Q WHICH)
;;;                        (CONS (Q IS)
;;;                              (CDR Y))))))))
;;;              ((CONS (Q THE) Y))))))
;;; EXPR)
;;;

;;;############################################################

(DEFUN PRTPUT (X Y) (COND ((CDR Y) (CONS X Y)) ((APPEND Y (LIST X)))))

;;;############################################################

(DEFUN VBFIX (X PP)
       (COND ((EQ TENSE 'ING)
          (SETQ X (REVERSE (EXPLODE X)))
          (READLIST (REVERSE (APPEND '(G N I)
                     (VBFIX2 X)
                     X))))
         ((EQ TENSE 'PAST)
          (OR (GET X 'PAST)
          (AND (SETQ X (REVERSE (EXPLODE X)))
               (READLIST (REVERSE (APPEND '(D E)
                          (VBFIX2 X)
                          X))))))
         ((EQ TENSE 'INFINITIVE) X)
         (T (BUG VBFIX - WHAT DO I DO WITH THIS TENSE?))))

;;;############################################################

(DEFUN VBFIX2 (X)
       (AND PP
        (MEMQ (CAR X) CONSO)
        (MEMQ (CADR X) VOWEL)
        (LIST (CAR X))))

§ anno/winograd/cgram

(declare (genprefix cgram))

################################################################

        CGRAM >

    the regular grammar after going through the precompiler
################################################################

(DEFUN CLAUSE NIL
       (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT POSITION-OF-PRTMVB LOCATIONMARKER
          SUBJ-VB-BACKUP-TYPE1 POSITION-OF-PTW)
         (SETQ NN T)
         (SETQ CUT END)
         (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))
                (SETQ NB (OR (NB RE) N))
                N
                (SETQ H RE)
                NIL))
         (SETR 'PARENT PARENT C)
    ENTERING-CLAUSE
         (AND LABELTRACE (PASSING 'ENTERING-CLAUSE))
         (SETR 'TIME (BUILD TSSNODE= (MAKESYM 'TSS)) C)
         (SETQ :RESULT (CQ SIMP))
         (COND (:RESULT (GO SUBJ)))
         (SETQ :RESULT (CQ MAJOR))
         (COND (:RESULT (GO INIT)) (T (GO SEC)))
    INIT (AND LABELTRACE (PASSING 'INIT))
         (SETQ LOCATIONMARKER N)
         (SETQ :RESULT (AND (NQ BINDER) (PARSE CLAUSE BOUND INIT)))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO FIXIT)) (T (GO MAJOR)))))
         (FQ BIND)
         (SETQ :RESULT (CALLSM (SMBIND)))
         (COND (:RESULT (GO INIT)))
    FIXIT(AND LABELTRACE (PASSING 'FIXIT))
         (SETQ PTW CUT)
         (SETQ :RESULT (CUT (MOVE-PTW)))
         (COND (:RESULT (GO INIT)) (T (GO MAJOR)))
    MAJOR(AND LABELTRACE (PASSING 'MAJOR))
         (CUT END)
         (COND ((EQ PUNCT '?) (GO QUEST))
           ((OR (CQ IMPER) (EQ PUNCT '!)) (GO IMPER)))
         (GO THEREINIT)
    FDEC (AND LABELTRACE (PASSING 'FDEC))
         (FQ DECLAR)
    THEREINIT
         (AND LABELTRACE (PASSING 'THEREINIT))
         (SETQ :RESULT (AND (NEXTWORD? 'THERE) (PARSE NIL THERE) (FQ DECLAR)))
         (COND (:RESULT (COND ((NULL NN) (M INIT) (GO FAIL)) (T (GO THERE)))))
    THER2(AND LABELTRACE (PASSING 'THER2))
         (AND (NQ PREP) (PARSE PREPG INIT) (OR (CALLSM (SMRELATE H)) (POP)))
         (AND (NQ ADV) (PARSE ADV TIMW) (OR (CALLSM (SMADVERB)) (POP)))
         (AND (NQ ADV) (PARSE ADJG ADV VBAD) (OR (CALLSM (SMRELATE H)) (POP)))
         (PARSE NG TIME)
         (SETQ :RESULT (EQ LOCATIONMARKER N))
         (COND (:RESULT (COND ((NULL NN) (GO INPOP)) (T (GO CLAUSETYPE)))) (T (GO INIT)))
    INPOP(AND LABELTRACE (PASSING 'INPOP))
         (SETQ :RESULT (MOVE-PT C DLC))
         (COND ((NULL :RESULT) (M INPOP) (GO FAIL)))
    BICUT(AND LABELTRACE (PASSING 'BICUT))
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
    SUBJ (AND LABELTRACE (PASSING 'SUBJ))
         (CUT END)
    SUBJ3(AND LABELTRACE (PASSING 'SUBJ3))
         (SETQ :RESULT (OR (AND (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ))
                   (AND (PARSE CLAUSE RSNG ING SUBJ))))
         (COND (:RESULT (COND ((NULL NN) (GO SUBJ1)) (T (GO SUBREG)))))
    SUBJ4(AND LABELTRACE (PASSING 'SUBJ4))
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
    HEAD (AND LABELTRACE (PASSING 'HEAD))
         (SETQ :RESULT (OR (MOVE-PTW N PW (NOUN)) (MOVE-PTW N PW (PRON))))
         (COND ((NULL :RESULT) (M HEAD) (GO FAIL)))
    SUB2 (AND LABELTRACE (PASSING 'SUB2))
         (SETQ :RESULT (POP))
         (COND ((NULL :RESULT) (GO FAIL)))
         (SETQ :RESULT (CUT PTW))
         (COND (:RESULT (GO INIT)) (T (GO SUB2)))
    SUBJ1(AND LABELTRACE (PASSING 'SUBJ1))
         (COND ((ISQ H QUOTED)
            (AND (ISQ H LIST) (FQ LIST))
            (FQ QUOTED)
            (SETQ H (H H))
            (GO RETSM)))
         (AND (CQ REL-NOT-FOUND)
          (MOVE-PT H PV (QAUX))
          (COND ((ISQ PT BE)
             (FQ INT AUXBE)
             (RQ REL-NOT-FOUND)
             (SETR 'COMP (GETR 'RELHEAD C) C)
             (SETR 'SUBJECT H C)
             (SETMVB PT)
             (GO ONT))
            ((ISQ PT HAVE)
             (FQ SUBQ)
             (RQ REL-NOT-FOUND)
             (SETR 'SUBJECT (GETR 'RELHEAD C) C)
             (GO VBL))))
    SUBJ11
         (AND LABELTRACE (PASSING 'SUBJ11))
         (SETQ :RESULT (CUT-BACK-ONE))
         (COND (:RESULT (GO SUBJ3)) (T (M SUBJ11) (GO FAIL)))
    SUBREG
         (AND LABELTRACE (PASSING 'SUBREG))
         (SETR 'SUBJECT H C)
         (GO VB)
    VB   (AND LABELTRACE (PASSING 'VB))
         (SETQ :RESULT (PARSE ADJG ADV VBAD))
         (COND (:RESULT (COND ((NULL NN) (M VB-ADJG) (GO FAIL)) (T (GO VB)))))
         (RQ VBLOK)
    VBL  (AND LABELTRACE (PASSING 'VBL))
         (SETQ :RESULT (PARSE VG))
         (COND (:RESULT (GO VBREG)))
    NOVERB
         (AND LABELTRACE (PASSING 'NOVERB))
         (COND ((CQ SUBJFORK) (FQ VBFORK) (GO FINDOBJ1))
           ((ISQ H QUOTED) (FQ REL-NOT-FOUND) (GO SUBJ4))
           ((NOT (ISQ H SUBJ)) (GO FAIL))
           ((ISQ H CLAUSE) (SETQ SUBJ-VB-BACKUP-TYPE1 T) (POP) (GO SUBJ4))
           ((ISQ H SUBJ) (POP) (FQ SUBJFORK) (GO VBL)))
    VB2  (AND LABELTRACE (PASSING 'VB2))
         (CUT-BACK-ONE)
         (GO SUBJ3)
    VBREG(AND LABELTRACE (PASSING 'VBREG))
         (SETR 'VG H C)
    VG1  (AND LABELTRACE (PASSING 'VG1))
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
    DPRT (AND LABELTRACE (PASSING 'DPRT))
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
    POPRT(AND LABELTRACE (PASSING 'POPRT))
         (POPTO VG)
         (GO FINDOBJ1)
    CHECKPASV
         (AND LABELTRACE (PASSING 'CHECKPASV))
         (SETQ :RESULT (AND (ISQ H PASV)
                (FQ PASV)
                (SETR 'OBJ1 (GETR 'SUBJECT C) C)))
         (COND (:RESULT (COND ((NULL NN) (GO FINDFAKE2)) (T (GO FINDOBJ2)))))
         (FQ ACTV)
         (GO FINDOBJ1)
    BE   (AND LABELTRACE (PASSING 'BE))
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
         (SETQ :RESULT (AND (CANTAKE 1 '(PREPG LOC) 'ITRNSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ ITRANSL)))
         (COND (:RESULT (GO PUTLOBJ)))
         (SETQ :RESULT (CANPARSE 1 NIL 'ITRNS))
         (COND (:RESULT (GO ONT)))
    GOOF1(AND LABELTRACE (PASSING 'GOOF1))
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
         (SETQ :RESULT (OR (CANPARSE 2 '(ADV PLACE) 'TRANSL)
                   (CANPARSE 2 '(PREPG LOC) 'TRANSL)))
         (COND (:RESULT (GO ONT)))
         (SETQ :RESULT (OR (CANPARSE 2 '(ADJG COMP) 'TRANSINT)
                   (CANPARSE 2 '(NG COMP) 'TRANSINT)))
         (COND (:RESULT (GO ONT)))
         (SETQ :RESULT (CANPARSE 2 '(NG) 'TRANS2))
         (COND (:RESULT (GO ONT)))
    FINDFAKE2
         (AND LABELTRACE (PASSING 'FINDFAKE2))
         (SETQ :RESULT (AND (ISQ MVB TRANS2) (MOVE-PT C U (REL-NOT-FOUND))))
         (COND (:RESULT (GO OBJ2REL)))
         (SETQ :RESULT (AND (CANTAKE 2 '(PREPG LOC) 'TRANSL)
                (MOVE-PT C U (QADJ))
                (ISQ (GETR 'QADJ PT) PLACE)
                (FQ TRANSL)))
         (COND (:RESULT (GO PUTLOBJ)))
    OBJ2TO
         (AND LABELTRACE (PASSING 'OBJ2TO))
         (PARSE ADV VBAD)
         (SETQ :RESULT
           (COND ((AND (NEXTWORD? 'TO) (ISQ MVB TO2) (PARSE PREPG TO))
              (SETR 'OBJ2 (GETR 'OBJ1 H) C)
              (FQ TRANS2TO TRANS2))
             ((AND (CQ PREPQ)
                   (MOVE-PT H PV (QUEST))
                   (EQ (WORD (MOVE-PTW FW)) 'TO)
                   (RQ PREPQ)
                   (FQ TRANS2TOQ TRANS2)
                   (SETR 'OBJ2 (GETR 'OBJ1 PT) C)))))
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
         (SETQ :RESULT (OR (AND (NEXTWORD? 'TO) (PARSE CLAUSE RSNG TO SUBJ))
                   (AND (NQ ING) (PARSE CLAUSE RSNG ING SUBJ))
                   (PARSE CLAUSE REPORT)))
         (COND ((NULL :RESULT) (GO ONT)))
         (FQ IT)
         (SETR 'LOGICAL-SUBJECT H C)
         (GO ONT)
    GOOF2(AND LABELTRACE (PASSING 'GOOF2))
         (OR GLOBAL-MESSAGE (ERTERR NEW TRANSITIVITY - SECOND OBJECT))
         (GO FAIL)
    ONT  (AND LABELTRACE (PASSING 'ONT))
         (SETQ :RESULT (CQ PASV))
         (COND (:RESULT (GO PONT)))
    ONT1 (AND LABELTRACE (PASSING 'ONT1))
         (SETQ :RESULT (CALLSM (SMCL1)))
         (COND ((NULL :RESULT) (M SMCL1) (GO FAIL)))
         (SETQ :RESULT (NOT (CQ REL-NOT-FOUND)))
         (COND (:RESULT (COND ((NULL NN) (GO RETSM)) (T (GO TONT)))))
         (SETQ :RESULT (ISQ (GETR 'HEAD (GETR 'RELHEAD C)) TIM1))
         (COND ((NULL :RESULT) (GO PREPSHORT)))
    TIMEQ(AND LABELTRACE (PASSING 'TIMEQ))
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
    PONT (AND LABELTRACE (PASSING 'PONT))
         (AND (NEXTWORD? 'BY) (PARSE PREPG AGENT) (FQ AGENT))
         (SETR 'LOGICAL-SUBJECT (GETR 'OBJ1 H) C)
         (GO ONT1)
    TONT (AND LABELTRACE (PASSING 'TONT))
         (SETQ :RESULT (SETQ POSITION-OF-PTW N))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO RETSM)) (T (GO RETSM)))))
    NPASV(AND LABELTRACE (PASSING 'NPASV))
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
         (SETQ :RESULT (AND (NEXTWORD? 'TO)
                (PARSE CLAUSE TO ADJUNCT)
                (OR (CALLSM (SMTOADJ)) (GO FAIL))))
         (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
         (SETQ :RESULT (EQ N POSITION-OF-PTW))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO RETSM)) (T (GO TONT)))))
         (SETQ :RESULT (OR (NOT (CQ TOPLEVEL)) (NQ SPECIAL)))
         (COND (:RESULT (GO RETSM)))
         (ERT CLAUSE: SOMETHING LEFT OVER AT TOP LEVEL)
         (GO FAIL)
    THERE(AND LABELTRACE (PASSING 'THERE))
         (FQ THERE)
         (CUT END)
         (SETQ :RESULT (PARSE ADV TIMW))
         (COND ((AND (NULL NN) :RESULT) (M THERE) (GO FAIL)))
         (SETQ :RESULT (AND (PARSE VG) (ISQ MVB BE)))
         (COND (:RESULT (COND ((NULL NN) (M THERE) (GO FAIL)) (T (GO THEF))))
           (T (GO NOTHE)))
    THERQ(AND LABELTRACE (PASSING 'THERQ))
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
    THEF (AND LABELTRACE (PASSING 'THEF))
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
    NOTHE(AND LABELTRACE (PASSING 'NOTHE))
         (RQ THERE)
         (POP THERE)
         (AND (NQ ADV) (PARSE ADV PLACE))
         (GO THER2)
    IMPER(AND LABELTRACE (PASSING 'IMPER))
         (SETQ :RESULT (PARSE NG TIME))
         (COND ((AND (NULL NN) :RESULT) (GO IMPOP)))
         (SETQ :RESULT (AND (NQ ADV) (PARSE ADJG ADV VBAD)))
         (COND ((AND (NULL NN) :RESULT) (GO IMPOP)))
         (SETQ :RESULT (AND (NQ ADV) (PARSE ADV TIMW)))
         (COND ((AND (NULL NN) :RESULT) (GO IMPOP)))
    IMPE (AND LABELTRACE (PASSING 'IMPE))
         (SETQ :RESULT (PARSE VG IMPER))
         (COND ((NULL :RESULT) (GO IMPOP)))
         (FQ IMPER)
         (GO VG1)
    IMPOP(AND LABELTRACE (PASSING 'IMPOP))
         (SETQ :RESULT (POP NIL))
         (COND (:RESULT (GO IMPE)) (T (M IMPOP) (GO FAIL)))
    QUEST(AND LABELTRACE (PASSING 'QUEST))
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
           (OR (AND (NEXTWORD? 'HOW)
                (PARSE ADJG QUEST)
                (SETR 'RELHEAD H C))
               (AND (NQ QADJ) (PARSE QADJ) (FQ QADJ) (SETR 'QADJ H C))))
         (COND (:RESULT (GO POLAR)) (T (GO POLAR)))
         (FQ SHORTQUES)
         (CALLSM (SMADJQSHORT))
    ADJQS(AND LABELTRACE (PASSING 'ADJQS))
         (GO RETURN)
    NGQST(AND LABELTRACE (PASSING 'NGQST))
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
    SUBF (AND LABELTRACE (PASSING 'SUBF))
         (SETQ :RESULT (PARSE NG SUBJ))
         (COND (:RESULT (COND ((NULL NN) (GO SUBJ1)) (T (GO SUBREG)))))
         (RQ REL-NOT-FOUND)
         (GO BE)
    POLAR(AND LABELTRACE (PASSING 'POLAR))
         (SETQ :RESULT (AND (NQ VB)
                (PARSE VB AUX (QAUX))
                (SETR 'QAUX H C)
                (CALLSM (SMVAUX))
                (SETMVB H)))
         (COND ((NULL :RESULT) (GO QCHOP)))
         (OR (CQ QADJ) (GETR 'RELHEAD C) (FQ POLAR))
         (FQ POLR2)
         (GO QUEST2)
    QCHOP(AND LABELTRACE (PASSING 'QCHOP))
         (ERT CLAUSE: QCHOP)
         (SETQ :RESULT (POPTO CLAUSE BOUND))
         (COND (:RESULT (GO BICUT)) (T (M QCHOP) (GO FAIL)))
    SEC  (AND LABELTRACE (PASSING 'SEC))
         (COND ((CQ BOUND) (GO BOUND))
           ((CQ TO) (GO TO))
           ((CQ RSQ) (GO RSQ))
           ((CQ REPORT) (GO REPORT))
           ((CQ ING) (GO ING))
           (T (MQ RSNG-TYPE) (GO FAIL)))
    BOUND(AND LABELTRACE (PASSING 'BOUND))
         (SETQ :RESULT (PARSE BINDER))
         (COND ((NULL :RESULT)
            (COND ((NULL NN) (M BINDER) (GO FAIL)) (T (M BOUND) (GO FAIL)))))
         (SETQ LOCATIONMARKER N)
         (GO FDEC)
    RSQ  (AND LABELTRACE (PASSING 'RSQ))
         (SETR 'RELHEAD (MOVE-PT C U (NG)) C)
         (SETQ :RESULT (CQ PREPREL))
         (COND ((NULL :RESULT) (GO RSQ2)))
         (PARSE PREPG PRONREL)
         (SETR 'QADJ H C)
         (GO REPORT)
    RSQ2 (AND LABELTRACE (PASSING 'RSQ2))
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
    REL  (AND LABELTRACE (PASSING 'REL))
         (SETR 'SUBJECT (GETR 'RELHEAD C) C)
         (SETQ :RESULT (PARSE VG))
         (COND (:RESULT (GO VG1)))
         (FQ REL-NOT-FOUND)
         (GO SUBJ)
    TO   (AND LABELTRACE (PASSING 'TO))
         (SETQ :RESULT (AND (CQ COMPONENT) (PARSE VG TO TODEL)))
         (COND (:RESULT (GO VG1)))
         (SETQ :RESULT (NEXTWORD? 'FOR))
         (COND ((NULL :RESULT) (GO TO1)))
         (PARSE NIL FOR)
         (FQ FOR)
         (PARSE NG SUBJ TOSUBJ)
         (SETR 'SUBJECT H C)
    TO1  (AND LABELTRACE (PASSING 'TO1))
         (SETQ :RESULT (PARSE VG TO))
         (COND (:RESULT (GO VG1)) (T (M TO) (GO FAIL)))
    ING  (AND LABELTRACE (PASSING 'ING))
         (SETQ :RESULT (MOVE-PTW N NW (ING)))
         (COND ((NULL :RESULT) (GO FAIL)))
         (SETQ :RESULT (OR (NQ ING)
                   (CQ OBJ2)
                   (AND (PARSE NG SUBJ INGSUBJ)
                    (SETR 'SUBJECT H C)
                    (FQ SUBING)
                    (RQ ING))))
         (COND ((AND (NULL NN) :RESULT) (M ING) (GO FAIL)))
         (SETQ :RESULT (PARSE VG ING))
         (COND (:RESULT (GO VG1)) (T (M ING) (GO FAIL)))
    REPORT
         (AND LABELTRACE (PASSING 'REPORT))
         (AND (NEXTWORD? 'THAT) (PARSE NIL THAT) (FQ THAT))
         (SETQ LOCATIONMARKER N)
         (GO FDEC)
    RETSM(AND LABELTRACE (PASSING 'RETSM))
         (OR (CALLSM (SMCL2)) (GO FAIL))
         (GO RETURN)
    FAIL (SETQ MES ME)
         (SETQ N (OR (N RE) NB))
         (RETURN NIL)
    RETURN
         (SETQ MES ME)
         (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN NG NIL
       (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT)
         (SETQ NN T)
         (SETQ CUT END)
         (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))
                (SETQ NB (OR (NB RE) N))
                N
                (SETQ H RE)
                NIL))
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
    LOOK (AND LABELTRACE (PASSING 'LOOK))
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
    START(AND LABELTRACE (PASSING 'START))
    PROPN(AND LABELTRACE (PASSING 'PROPN))
         (PARSE PROPN)
         (FQ DEF PROPNG)
         (SETQ :RESULT (ISQ H POSS))
         (COND (:RESULT (GO PROPS)))
         (SETQ :RESULT (AND NN (NQ PROPN)))
         (COND (:RESULT (GO PROPN)))
    PROPS(AND LABELTRACE (PASSING 'PROPS))
         (OR (CALLSM (SMPROP)) (GO FAIL))
         (SETQ :RESULT (ISQ H POSS))
         (COND (:RESULT (GO POSS)) (T (GO PRAG)))
    PRON (AND LABELTRACE (PASSING 'PRON))
         (SETQ :RESULT (PARSE PRON POSS))
         (COND (:RESULT (COND ((NULL NN) (GO RED2)) (T (GO POSS)))))
    PRON2(AND LABELTRACE (PASSING 'PRON2))
         (SETQ :RESULT (CQ NPRON))
         (COND (:RESULT (M NPRON) (GO FAIL)))
         (SETQ :RESULT (OR (AND (CQ SUBJ) (PARSE PRON SUBJ))
                   (AND (OR (CQ OBJ) (CQ TOSUBJ) (CQ INGSUBJ)) (PARSE PRON OBJ))
                   (CQ INGSUBJ)))
         (COND ((NULL :RESULT) (M PRON) (GO FAIL)))
         (FQ PRONG DEF)
    PRON3(AND LABELTRACE (PASSING 'PRON3))
         (SETQ :RESULT (CALLSM (SMPRON H)))
         (COND ((NULL :RESULT) (GO FAIL)))
    PRAG (AND LABELTRACE (PASSING 'PRAG))
         (SETR 'HEAD H C)
         (MOVE-PT H)
         (TRNSF NS NPL NFS NEG)
         (GO RETURN)
    TPRON(AND LABELTRACE (PASSING 'TPRON))
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
    AS   (AND LABELTRACE (PASSING 'AS))
         (SETQ :RESULT (AND (PARSE NIL AS) (PARSE NUMD NUMDAS) NN (PARSE NIL AS)))
         (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO NUMD2))))
           (T (M AS) (GO FAIL)))
    AT   (AND LABELTRACE (PASSING 'AT))
         (SETQ :RESULT (AND (PARSE NIL AT) (PARSE NUMD NUMDAT)))
         (COND ((NULL :RESULT) (COND ((NULL NN) (M AT) (GO FAIL)) (T (M AT) (GO FAIL)))))
    NUMD2(AND LABELTRACE (PASSING 'NUMD2))
         (SETQ :RESULT (AND (PARSE NUM) (FQ NUM NUMD)))
         (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO DET1))))
           (T (M NUMD2) (GO FAIL)))
    NUMD (AND LABELTRACE (PASSING 'NUMD))
         (SETQ :RESULT (PARSE NUMD NUMDAN))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO ND3)))))
         (SETQ :RESULT (PARSE NIL THAN))
         (COND (:RESULT (COND ((NULL NN) (GO POPCOM)) (T (GO NUMD2)))) (T (GO INCOM)))
    ND3  (AND LABELTRACE (PASSING 'ND3))
         (SETQ :RESULT (PARSE NUMD NUMDALONE))
         (COND (:RESULT (COND ((NULL NN) (M NUMD) (GO FAIL)) (T (GO NUMD2))))
           (T (M NUMD) (GO FAIL)))
    TIME (AND LABELTRACE (PASSING 'TIME))
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
    DET  (AND LABELTRACE (PASSING 'DET))
         (PARSE DET)
         (FQ DET)
         (MOVE-PT H)
         (SETQ :RESULT (TRNSF NPL NS PART DEF INDEF ANY NEG QNTFR))
         (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO IND)))) (T (M BUG) (GO FAIL)))
    IND  (AND LABELTRACE (PASSING 'IND))
         (SETQ :RESULT (AND (EQ (WORD (NB H)) 'ALL)
                (EQ (WORD N) 'THE)
                (PARSE DET)
                (FQ DEF)))
         (COND (:RESULT (COND ((NULL NN) (M THE) (GO FAIL)) (T (GO NUM)))))
         (SETQ :RESULT (AND (ISQ H QNTFR) (FQ QNTFR)))
         (COND (:RESULT (GO QNUM)))
    ORD  (AND LABELTRACE (PASSING 'ORD))
         (SETQ :RESULT (AND (PARSE ORD) (FQ ORD)))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO NUM)))))
         (SETQ :RESULT (AND (NEXTWORD? 'OF)
                (ISQ (MOVE-PTW N NW) MONTH)
                (PARSE NIL OF)
                (PARSE NOUN MONTH)
                (FQ DATE)))
         (COND (:RESULT (GO RETSM)))
         (SETQ :RESULT (CQ DEF))
         (COND ((NULL :RESULT) (GO ADJ)))
    NUM  (AND LABELTRACE (PASSING 'NUM))
         (SETQ :RESULT (PARSE NUM))
         (COND ((NULL :RESULT) (GO ADJ)))
         (FQ NUM)
         (SETQ :RESULT (CQ DET))
         (COND ((NULL :RESULT) (GO DET1)))
         (SETQ :RESULT (COND ((AND (ISQ H NS) (CQ NS)) (RQ NPL PART))
                 ((CQ NPL) (RQ NS PART))))
         (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO ADJ)))) (T (M NUM) (GO FAIL)))
    DET1 (AND LABELTRACE (PASSING 'DET1))
         (COND ((ISQ H NS) (FQ NS)) (T (FQ NPL)))
         (OR NN (AND (FQ NUMBER) (GO INCOM)))
    NUMBER
         (AND LABELTRACE (PASSING 'NUMBER))
         (FQ DET)
         (SETQ :RESULT (NQ OF))
         (COND (:RESULT (GO OF)) (T (GO ADJ)))
    QNUM (AND LABELTRACE (PASSING 'QNUM))
         (SETQ :RESULT (ISQ H NONUM))
         (COND (:RESULT (GO OF)))
         (SETQ :RESULT (AND (PARSE NUM) (FQ NUM)))
         (COND ((NULL :RESULT) (GO OF)))
         (SETQ :RESULT (COND ((EQ (SM H) 1) (AND (CQ NS) (RQ NPL))) ((CQ NPL) (RQ NS))))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (M NUMD) (GO FAIL)))))
         (SETQ :RESULT (EQ (WORD (NB H)) 'NO))
         (COND (:RESULT (GO ADJ)))
    OF   (AND LABELTRACE (PASSING 'OF))
         (SETQ :RESULT (AND (NQ OF) (PARSE PREPG OF)))
         (COND (:RESULT (GO SMOF)) (T (GO NONE)))
    SMOF (AND LABELTRACE (PASSING 'SMOF))
         (FQ OF)
         (SETQ :RESULT (OR (CALLSM (SMNGOF)) (NOT (POP))))
         (COND (:RESULT (GO RETSM)) (T (GO INCOM)))
    NONE (AND LABELTRACE (PASSING 'NONE))
         (SETQ :RESULT (EQ (WORD (NB H)) 'NONE))
         (COND (:RESULT (GO INCOM)) (T (GO ADJ)))
    ADJ  (AND LABELTRACE (PASSING 'ADJ))
         (SETQ :RESULT (PARSE ADJ))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO EPR)))))
         (AND (ISQ H COMPAR)
          (FQ COMPARATIVE-MODIFIER)
          (SETR 'COMPARATIVE-MODIFIER H C))
         (GO ADJ)
    EPR  (AND LABELTRACE (PASSING 'EPR))
         (SETQ :RESULT (OR (ISQ H SUP) (ISQ H COMPAR)))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO CLASF)))))
         (FQ ADJ)
         (AND (NEXTWORD? 'OF)
          (PARSE PREPG OF)
          (OR (CALLSM (SMNGOF)) (GO FAIL))
          (FQ OF)
          (GO RETSM))
    CLASF(AND LABELTRACE (PASSING 'CLASF))
         (SETQ :RESULT (OR (PARSE VB ING (CLASF)) (PARSE VB EN (CLASF)) (PARSE CLASF)))
         (COND (:RESULT (COND ((NULL NN) (GO REDUC)) (T (GO CLASF)))))
    NOUN (AND LABELTRACE (PASSING 'NOUN))
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
    SMNG (AND LABELTRACE (PASSING 'SMNG))
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
         (SETQ :RESULT (AND (NEXTWORD? 'TO)
                (MEET FE '(COMP SUBJ))
                (PARSE CLAUSE RSQ TO)
                (OR (CALLSM (SMRELATE H)) (GO POPRET))))
         (COND (:RESULT (GO RETSM)))
         (SETQ :RESULT (AND (OR (NEXTWORD? 'AS) (NQ COMPAR)) (PARSE ADJG THANNEED)))
         (COND ((NULL :RESULT) (GO PREPNG)))
         (AND (NULL N) (CQ SUBJ) (ISQ (MOVE-PT C PV) AUX) (ISQ PT BE) (GO POPRET))
         (SETQ :RESULT (CALLSM (SMRELATE H)))
         (COND (:RESULT (COND ((NULL NN) (GO RETSM)) (T (GO RSQ-TO)))) (T (GO POPRET)))
    PREPNG
         (AND LABELTRACE (PASSING 'PREPNG))
         (SETQ :RESULT (AND (NQ PREP)
                (NOT (OR (AND (NQ PLACE) (CQ NOLOC))
                     (AND (CQ OBJ1)
                          (ISQ MVB TRANSL)
                          (NOT (ISQ (MOVE-PT C U) QUEST)))))
                (PARSE PREPG Q)))
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
         (COND (:RESULT (COND ((NULL NN) (GO RETSM)) (T (GO PREPNG))))
           (T (M RSQ-PREPREL) (GO FAIL)))
    RSQ  (AND LABELTRACE (PASSING 'RSQ))
         (SETQ :RESULT (AND (ISQ (MOVE-PT C U) POLR2)
                (CQ SUBJ)
                (NQ VB)
                (NOT (CQ SUBJT))
                (NOT (ISQ PT QADJ))))
         (COND (:RESULT (GO RETSM)))
         (SETQ :RESULT (PARSE CLAUSE RSQ))
         (COND ((NULL :RESULT) (GO RETSM)))
         (SETQ :RESULT (CALLSM (SMRELATE H)))
         (COND (:RESULT (GO RETSM)) (T (GO POPRET)))
    RED0 (AND LABELTRACE (PASSING 'RED0))
         (SETQ FE T1)
    RED1 (AND LABELTRACE (PASSING 'RED1))
         (POP)
    RED2 (AND LABELTRACE (PASSING 'RED2))
         (COND ((NULL H) (MQ NO) (GO FAIL))
           ((ISQ H NUMBER) (GO INCOM))
           ((AND (ISQ H POSS) (OR (ISQ H PRON) (AND (MOVE-PT H DLC) (ISQ PT PRON))))
            (POP)
            (GO PRON2))
           ((AND (NULL (CDR H)) (CQ DEFPOSS)) (GO POSSDEF))
           ((AND (CQ QUEST) (NULL (CDR H))) (GO QDETCHECK))
           ((ISQ H ADJ) (GO EPR))
           ((NOT (ISQ H CLASF)) (GO INCOM)))
    REDUC(AND LABELTRACE (PASSING 'REDUC))
         (POP)
         (SETQ :RESULT (AND (NULL H) (NQ PROPN)))
         (COND (:RESULT (GO PROPN)) (T (GO NOUN)))
    POPCOM
         (AND LABELTRACE (PASSING 'POPCOM))
         (POP)
    INCOM(AND LABELTRACE (PASSING 'INCOM))
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
    POSS (AND LABELTRACE (PASSING 'POSS))
         (OR (CALLSM (SMNG2)) (GO FAIL))
    POSS2(AND LABELTRACE (PASSING 'POSS2))
         (SETQ :RESULT (CQ INGSUBJ))
         (COND (:RESULT (GO RETSM)))
         (SETQ H (BUILDNODE (REVERSE (CONS 'POSS (SETDIF FE '(COMPONENT))))
                NB
                N
                H
                SM))
         (SETQ BACKREF (APPEND H (CDR BACKREF)))
         (SETQ :RESULT (SETR 'FEATURES
                 (SETQ FE (APPEND '(POSES DET DEF NS NPL)
                          (REVERSE REST)))
                 C))
         (COND ((NULL :RESULT) (M BUG) (GO FAIL)))
         (SETQ :RESULT (OR (NOT NN) (ISQ H DEFPOSS)))
         (COND ((NULL :RESULT) (GO ORD)))
    POSSDEF
         (AND LABELTRACE (PASSING 'POSSDEF))
         (RQ POSES DET DEF)
         (FQ POSSDEF NS NPL)
    QUEST(AND LABELTRACE (PASSING 'QUEST))
         (SETQ :RESULT (PARSE NIL HOW))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO FAIL)) (T (GO QDET)))))
         (SETQ :RESULT (PARSE NIL MANY))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO INCOM)) (T (GO FAIL)))))
         (FQ DET NPL INDEF HOWMANY)
         (GO OF)
    QDET (AND LABELTRACE (PASSING 'QDET))
         (SETQ :RESULT (AND (PARSE DET QDET) (FQ DET NPL QDET NS)))
         (COND (:RESULT (COND ((NULL NN) (GO INCOM)) (T (GO QNUM)))))
    QPRON(AND LABELTRACE (PASSING 'QPRON))
         (SETQ :RESULT (PARSE PRON QPRON))
         (COND (:RESULT (GO PRON3)) (T (GO FAIL)))
    RELWD(AND LABELTRACE (PASSING 'RELWD))
         (SETQ :RESULT (AND (PARSE PRONREL) (CALLSM (SMSET (SM (MOVE-PT C U U (NG)))))))
         (COND (:RESULT (GO RETURN)))
    POPRET
         (AND LABELTRACE (PASSING 'POPRET))
         (POP)
    RETSM(AND LABELTRACE (PASSING 'RETSM))
         (OR (CALLSM (SMNG2)) (GO TRYA))
         (GO RETURN)
    TRYA (AND LABELTRACE (PASSING 'TRYA))
         (SETQ :RESULT (ISQ H NOUN))
         (COND ((NULL :RESULT) (M TRYA) (GO FAIL)))
         (POP)
         (CUT N)
    UP   (AND LABELTRACE (PASSING 'UP))
         (SETQ :RESULT (POP))
         (COND (:RESULT (GO UP)))
         (SETQ FE (REVERSE REST))
         (SMSET NIL)
         (GO NGSTART)
    FAIL (SETQ MES ME)
         (SETQ N (OR (N RE) NB))
         (RETURN NIL)
    RETURN
         (SETQ MES ME)
         (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN VG NIL
       (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT TENSE)
         (SETQ NN T)
         (SETQ CUT END)
         (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))
                (SETQ NB (OR (NB RE) N))
                N
                (SETQ H RE)
                NIL))
         (SETR 'PARENT PARENT C)
    ENTERING-VG
         (AND LABELTRACE (PASSING 'ENTERING-VG))
         (COND ((CQ TO) (GO TO))
           ((CQ EN) (GO EN))
           ((CQ ING) (GO ING))
           ((CQ IMPER) (GO IMPER))
           ((ISQ (MOVE-PT C U) POLR2) (GO POLR2)))
    NEW  (AND LABELTRACE (PASSING 'NEW))
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
         (SETQ TENSE (COND ((AND (ISQ PT PRESENT) (ISQ PT PAST)) '(PAST-PRESENT))
                   ((ISQ PT PAST) '(PAST))
                   (T '(PRESENT))))
         (GO REV)
    TO   (AND LABELTRACE (PASSING 'TO))
         (FQ NAGR)
         (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
         (SETQ :RESULT (OR (PARSE NIL TO) (CQ TODEL)))
         (COND ((NULL :RESULT) (COND ((NULL NN) (M TO) (GO FAIL)) (T (M TO) (GO FAIL)))))
         (SETQ TENSE '(INFINITIVE))
         (GO MODAL2)
    EN   (AND LABELTRACE (PASSING 'EN))
         (FQ NAGR)
         (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
         (SETQ TENSE '(PAST))
         (SETQ :RESULT (AND (PARSE VB EN (MVB)) (SETMVB H) (FQ PASV)))
         (COND (:RESULT (GO RETSM)) (T (GO FAIL)))
    ING  (AND LABELTRACE (PASSING 'ING))
         (FQ NAGR)
         (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
    INGADV
         (AND LABELTRACE (PASSING 'INGADV))
         (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
         (COND (:RESULT (GO INGADV)))
         (SETQ TENSE '(PRESENT))
         (GO BE2)
    IMPER(AND LABELTRACE (PASSING 'IMPER))
         (SETQ :RESULT (AND (PARSE VB DO NEG INF) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M DONT) (GO FAIL)))
         (SETQ :RESULT (AND (PARSE VB (MVB) INF) (SETMVB H) (CALLSM (SMVG))))
         (COND (:RESULT (GO RETURN)) (T (M IMPER) (GO FAIL)))
    POLR2(AND LABELTRACE (PASSING 'POLR2))
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
    DO   (AND LABELTRACE (PASSING 'DO))
         (FQ DO)
         (MOVE-PT C DLC)
         (TRNSF VPL NEG INF V3PS)
         (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (T '(PRESENT))))
         (COND (NN (GO DO2)) (T (GO MVB)))
    DO2  (AND LABELTRACE (PASSING 'DO2))
         (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
    ADV2 (AND LABELTRACE (PASSING 'ADV2))
         (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
         (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV2)))))
         (SETQ :RESULT (PARSE VB (MVB) INF))
         (COND ((NULL :RESULT) (GO MVB)))
         (GO REV)
    MODAL(AND LABELTRACE (PASSING 'MODAL))
         (FQ NAGR MODAL)
         (SETQ TENSE '(MODAL))
         (COND (NN (GO MODAL2)) (T (GO INCOMP)))
    MODAL2
         (AND LABELTRACE (PASSING 'MODAL2))
         (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
    ADV3 (AND LABELTRACE (PASSING 'ADV3))
         (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
         (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV3)))))
         (COND ((PARSE VB BE INF) (GOCOND BE2 MVB))
           ((PARSE VB HAVE INF) (GOCOND HAV2 MVB))
           ((PARSE VB INF (MVB)) (GO REV))
           (T (GO INCOMP)))
    WILL (AND LABELTRACE (PASSING 'WILL))
         (FQ NAGR)
         (SETQ TENSE '(FUTURE))
         (COND (NN (GO MODAL2)) (T (GO INCOMP)))
    BE   (AND LABELTRACE (PASSING 'BE))
         (MOVE-PT C DLC)
         (TRNSF VPL INF V3PS VFS)
         (SETQ TENSE (COND ((ISQ PT PAST) '(PAST)) (T '(PRESENT))))
         (COND (NN (GO BE2)) (T (GO MVB)))
    BE2  (AND LABELTRACE (PASSING 'BE2))
         (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
    ADV4 (AND LABELTRACE (PASSING 'ADV4))
         (SETQ :RESULT (OR (PARSE ADV TIMW) (PARSE ADV VBAD)))
         (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV4)))))
         (COND ((AND (NEXTWORD? 'GOING) (PARSE VB)) (GO GOING))
           ((AND (NQ BE) (PARSE VB ING))
            (SETQ TENSE (CONS 'PRESENT TENSE))
            (GO EN2))
           ((AND (NQ ING) (PARSE VB ING (MVB)))
            (SETQ TENSE (CONS 'PRESENT TENSE))
            (GO REV))
           ((CQ ING) (MQ ING) (GO FAIL)))
    EN2  (AND LABELTRACE (PASSING 'EN2))
         (SETQ :RESULT (PARSE VB EN (MVB)))
         (COND ((NULL :RESULT) (GO MVBE)))
         (FQ PASV)
         (GO REV)
    GOING(AND LABELTRACE (PASSING 'GOING))
         (SETQ :RESULT (PARSE NIL TO))
         (COND ((NULL :RESULT) (GO GOI)))
         (SETQ :RESULT (NQ INF))
         (COND (:RESULT (GO GOING2)))
         (POP)
    GOI  (AND LABELTRACE (PASSING 'GOI))
         (SETQ TENSE (CONS 'PRESENT TENSE))
         (GO MVB)
    GOING2
         (AND LABELTRACE (PASSING 'GOING2))
         (SETQ TENSE (CONS 'FUTURE TENSE))
         (GO MODAL2)
    MVBE (AND LABELTRACE (PASSING 'MVBE))
         (SETQ :RESULT (ISQ (MOVE-PT H PV (VB)) AUX))
         (COND ((NULL :RESULT) (GO MVB)))
         (SETQ :RESULT (ISQ PT BE))
         (COND ((NULL :RESULT) (M MVBE) (GO FAIL)))
         (SETMVB PT)
         (GO REV)
    HAVE (AND LABELTRACE (PASSING 'HAVE))
         (MOVE-PT C DLC)
         (TRNSF VPL INF V3PS VFS)
         (SETQ TENSE (COND ((ISQ PT PAST) (FQ NAGR) '(PAST)) (T '(PRESENT))))
         (COND (NN (GO HAV2)) (T (GO MVB)))
    HAV2 (AND LABELTRACE (PASSING 'HAV2))
         (SETQ :RESULT (AND (PARSE NIL NOT) (FQ NEG)))
         (COND ((AND (NULL NN) :RESULT) (M NOT) (GO FAIL)))
    ADV5 (AND LABELTRACE (PASSING 'ADV5))
         (SETQ :RESULT (PARSE ADV))
         (COND (:RESULT (COND ((NULL NN) (M ADV) (GO FAIL)) (T (GO ADV5)))))
         (SETQ :RESULT (PARSE VB BE EN))
         (COND ((NULL :RESULT) (GO HAV3)))
         (SETQ TENSE (CONS 'PAST TENSE))
         (COND (NN (GO BE2)) (T (GO MVB)))
    HAV3 (AND LABELTRACE (PASSING 'HAV3))
         (SETQ :RESULT (PARSE VB (MVB) EN))
         (COND ((NULL :RESULT) (GO MVB)))
         (SETQ TENSE (CONS 'PAST TENSE))
         (GO REV)
    INCOMP
         (AND LABELTRACE (PASSING 'INCOMP))
         (FQ INCOMP)
         (GO FAIL)
    MVB  (AND LABELTRACE (PASSING 'MVB))
         (SETQ :RESULT (EQ (FE MVB) (FE H)))
         (COND (:RESULT (GO MVB2)))
         (POP VB)
         (SETQ :RESULT (PARSE VB (MVB)))
         (COND ((NULL :RESULT) (M MVB) (GO FAIL)))
    MVB2 (AND LABELTRACE (PASSING 'MVB2))
         (GO REV)
    REV  (AND LABELTRACE (PASSING 'REV))
         (SETR 'TENSE TENSE C)
         (AND NN (PARSE NIL NOT) (FQ NEG))
         (COND ((OR (EQUAL TENSE '(PAST))
            (CQ NAGR)
            (ISQ (MOVE-PT C U) IMPER)
            (ISQ PT THERE)
            (ISQ PT RSNG))
            (GO NAUX))
           ((SETQ PT (GETR 'SUBJECT (MOVE-PT C U))))
           (T (ERTERR VG -- NO SUBJECT TO CHECK FOR AGREEMENT)))
         (SETQ T3 NIL)
         (COND ((ISQ PT NFS) (OR (SETQ T3 (MEET FE '(VFS INF))) (GO NAGR)))
           ((ISQ PT CLAUSE) (OR (SETQ T3 (CQ V3PS)) (GO NAGR)))
           ((OR (ISQ PT NS) (ISQ PT MASS))
            (OR (AND (CQ V3PS) (SETQ T3 T))
            (FESET PT (SETDIF (FE PT) '(NS MASS))))))
         (COND ((OR (ISQ PT PART) (ISQ PT NPL))
            (OR (AND (MEET FE '(INF VPL)) (SETQ T3 T))
            (FESET PT (SETDIF (FE PT) '(PART NPL))))))
    NAGR (AND LABELTRACE (PASSING 'NAGR))
         (SETQ :RESULT (OR T3
                   (AND (EQUAL '(PAST-PRESENT) TENSE)
                    (SETQ TENSE '(PAST)))))
         (COND ((NULL :RESULT) (M NAGR) (GO FAIL)))
    NAUX (AND LABELTRACE (PASSING 'NAUX))
         (SETMVB (OR (MOVE-PT H PV (MVB)) MVB))
         (SETQ :RESULT
           (AND (CQ NAUX) (ISQ (MOVE-PT H PV (VB)) AUX) (NOT (MOVE-PT PV PV (VB)))))
         (COND (:RESULT (M NAUX) (GO FAIL)) (T (GO RETSM)))
    POPV (AND LABELTRACE (PASSING 'POPV))
         (ERT POPV)
         (GO FAIL)
    RETSM(AND LABELTRACE (PASSING 'RETSM))
         (SETQ :RESULT (CALLSM (SMVG)))
         (COND (:RESULT (GO RETURN)) (T (GO FAIL)))
    FAIL (SETQ MES ME)
         (SETQ N (OR (N RE) NB))
         (RETURN NIL)
    RETURN
         (SETQ MES ME)
         (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN PREPG NIL
       (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT)
         (SETQ NN T)
         (SETQ CUT END)
         (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))
                (SETQ NB (OR (NB RE) N))
                N
                (SETQ H RE)
                NIL))
         (SETR 'PARENT PARENT C)
    ENTERING-PREPG
         (AND LABELTRACE (PASSING 'ENTERING-PREPG))
    ADV  (AND LABELTRACE (PASSING 'ADV))
         (SETQ :RESULT (AND (NQ PREPADV) (PARSE ADV PREPADV)))
         (COND (:RESULT (COND ((NULL NN) (M PREPADV) (GO FAIL)) (T (GO ADV)))))
         (SETQ :RESULT (COND ((CQ AGENT) (NEXTWORD? 'BY))
                 ((CQ LOC) (NQ PLACE))
                 ((CQ Q) (NOT (NQ MOTOR)))
                 (T)))
         (COND ((NULL :RESULT) (M PREP) (GO FAIL)))
         (SETQ :RESULT (PARSE PREP))
         (COND ((NULL :RESULT) (M PREP) (GO FAIL)))
         (MOVE-PT H)
         (TRNSF PLACE TIME)
         (SETQ T1 H)
         (AND (NQ PREP2)
          (COND ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N))) (PARSE PREP2))
            ((SETQ T1 (COMBINATION? (WORD (NB H)) (WORD N) (WORD (CDR N))))
             (PARSE PREP2)
             (PARSE PREP2)))
          (SETQ T1 (BUILDNODE (FE T1) NB N 'WORD (SM T1)))
          (SETR 'PARENT C T1))
         (SETQ :RESULT (ISQ H NEED2))
         (COND (:RESULT (M NEED2) (GO FAIL)))
         (SETR 'HEAD T1 C)
         (OR NN (GO SHORT))
         (COND ((EQ (WORD H) 'BY) (FQ AGENT)))
    QUEST(AND LABELTRACE (PASSING 'QUEST))
         (SETQ :RESULT (CQ QUEST))
         (COND ((NULL :RESULT) (GO NG)))
         (SETQ :RESULT (PARSE NG QUEST OBJ))
         (COND (:RESULT (GO OBJR)) (T (M PREPQUEST) (GO FAIL)))
         (SETQ :RESULT (AND (CQ OF) (PARSE NG OFOBJ)))
         (COND (:RESULT (GO OBJR)))
    NG   (AND LABELTRACE (PASSING 'NG))
         (SETQ :RESULT (PARSE NG OBJ))
         (COND (:RESULT (GO OBJR)))
    REL  (AND LABELTRACE (PASSING 'REL))
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
    REST (AND LABELTRACE (PASSING 'REST))
         (SETQ :RESULT (PARSE CLAUSE RSNG ING))
         (COND (:RESULT (GO OBJR)) (T (GO SHORT)))
    OBJR (AND LABELTRACE (PASSING 'OBJR))
         (SETR 'OBJ1 H C)
         (GO RETT)
    SHORT(AND LABELTRACE (PASSING 'SHORT))
         (SETQ :RESULT (MEET FE '(NOSHORT Q)))
         (COND (:RESULT (M SHORT) (GO FAIL)))
         (OR (ISQ (MOVE-PT C U) REL-NOT-FOUND)
         (ISQ (GETR 'QUESTION-ELEMENT PT) QADJ)
         (GO FAIL))
         (REMOVE-F-PT 'REL-NOT-FOUND PT)
         (ADD-F-PT 'PREPREL PT)
         (SETR 'OBJ1 (GETR 'RELHEAD (MOVE-PT C U)) C)
    RETT (AND LABELTRACE (PASSING 'RETT))
         (AND (OR (ISQ H QUEST) (AND (ISQ H COMPOUND) (MOVE-PT H H PV (QUEST))))
          (FQ QUEST))
         (SETQ :RESULT (CALLSM (SMADJG-PREPG)))
         (COND (:RESULT (GO RETURN)) (T (GO FAIL)))
    FAIL (SETQ MES ME)
         (SETQ N (OR (N RE) NB))
         (RETURN NIL)
    RETURN
         (SETQ MES ME)
         (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN ADJG NIL
       (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT)
         (SETQ NN T)
         (SETQ CUT END)
         (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))
                (SETQ NB (OR (NB RE) N))
                N
                (SETQ H RE)
                NIL))
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
    DISP (AND LABELTRACE (PASSING 'DISP))
         (SETQ :RESULT (AND (NQ AS) (PARSE NIL AS)))
         (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO AS)))))
         (SETQ :RESULT (AND (NQ AS) (PARSE NIL AS)))
         (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO AS)))))
         (SETQ :RESULT (NEXTWORD? 'HOW))
         (COND (:RESULT (GO HOW)) (T (GO ADV)))
    HOW  (AND LABELTRACE (PASSING 'HOW))
         (SETQ :RESULT (AND (PARSE NIL HOW) (FQ QUEST)))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO FAIL)) (T (GO FAIL)))))
         (SETQ :RESULT (AND (PARSE ADJ) (FQ ADJ) (SETR 'HEAD H C)))
         (COND (:RESULT (GO RETSM)))
         (SETQ :RESULT (AND (PARSE ADV VBAD) (FQ VBAD) (SETR 'HEAD H C)))
         (COND (:RESULT (GO RETSM)) (T (GO FAIL)))
    ADV  (AND LABELTRACE (PASSING 'ADV))
         (SETQ :RESULT (PARSE ADV ADVADV))
         (COND (:RESULT (COND ((NULL NN) (GO POPAD)) (T (GO ADV)))))
         (SETQ :RESULT (PARSE NIL MORE))
         (COND ((NULL :RESULT) (GO ADJ)))
         (FQ COMPAR)
    ADJ  (AND LABELTRACE (PASSING 'ADJ))
         (SETQ :RESULT (COND ((CQ ADV) (PARSE ADV VBAD)) (T (PARSE ADJ))))
         (COND ((NULL :RESULT) (M ADJ) (GO FAIL)))
         (SETQ :RESULT (SETR 'HEAD H C))
         (COND ((AND (NULL NN) :RESULT) (GO RETSM)))
         (SETQ :RESULT (OR (CQ COMPAR) (ISQ H COMPAR)))
         (COND ((NULL :RESULT) (GO RETSM)))
         (FQ COMPAR)
         (SETQ :RESULT NN)
         (COND ((NULL :RESULT) (GO RETSM)))
    THAN (AND LABELTRACE (PASSING 'THAN))
         (COND ((NOT NN) (GO RETSM)))
         (SETQ :RESULT (PARSE NIL THAN))
         (COND ((NULL :RESULT) (COND ((NULL NN) (M THAN) (GO FAIL)) (T (GO RETSM)))))
         (RQ THANNEED)
         (FQ THAN)
         (GO SUBJ)
    AS   (AND LABELTRACE (PASSING 'AS))
         (FQ AS)
         (RQ THANNEED)
         (SETQ :RESULT (AND (PARSE ADJ) (SETR 'HEAD H C)))
         (COND ((NULL :RESULT) (COND ((NULL NN) (GO RETSM)) (T (M ADJ) (GO FAIL)))))
         (SETQ :RESULT (PARSE NIL AS))
         (COND (:RESULT (COND ((NULL NN) (M AS) (GO FAIL)) (T (GO SUBJ)))) (T (GO RETSM)))
    SUBJ (AND LABELTRACE (PASSING 'SUBJ))
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
    POPAD(AND LABELTRACE (PASSING 'POPAD))
         (POP)
         (GO ADJ)
    RETSM(AND LABELTRACE (PASSING 'RETSM))
         (SETQ :RESULT (CQ THANNEED))
         (COND (:RESULT (M THANNEED) (GO FAIL)))
         (SETQ :RESULT (CALLSM (SMADJG-PREPG)))
         (COND (:RESULT (GO RETURN)) (T (M SMADJ) (GO FAIL)))
    FAIL (SETQ MES ME)
         (SETQ N (OR (N RE) NB))
         (RETURN NIL)
    RETURN
         (SETQ MES ME)
         (RETURN (REBUILD (REVERSE FE) NB N H SM C))))

(DEFUN CONJ NIL
       (PROG (END GOODIE)
         (SETQ END CUT)
         (COND ((SETQ GOODIE (APPLY-GRAMMAR 'CONJOIN)) (RETURN (SETQ RE GOODIE)))
           (T (RETURN NIL)))))

(DEFUN COMMA NIL
       (COND ((SECONDWORD? '") (FLUSHME) T) ((CONJ)) ((ISQ RE INIT) (FLUSHME) T)))

(DEFUN CONJOIN NIL
       (PROG (FE H ME NB C SM CUT NN T1 T2 T3 :RESULT PREV)
         (SETQ NN T)
         (SETQ CUT END)
         (SETQ C (BUILDNODE (SETQ FE (REVERSE REST))
                (SETQ NB (OR (NB RE) N))
                N
                (SETQ H RE)
                NIL))
         (SETR 'PARENT PARENT C)
    ENTERING-CONJOIN
         (AND LABELTRACE (PASSING 'ENTERING-CONJOIN))
    UP   (AND LABELTRACE (PASSING 'UP))
         (SETQ PREV (NEXTWORD))
         (FLUSHME)
         (COND ((AND (EQ PREV '/,)
             (OR (CDR H) (GREATERP (DIFFERENCE (LENGTH (NB H)) (LENGTH (N H))) 4))
             (MEMQ (NEXTWORD) '(OR AND NOR BUT))
             (F (NEXTWORD)))
            (SETQ PREV (LIST PREV (NEXTWORD)))
            (FLUSHME)))
         (AND (ATOM PREV) (MOVE-PTW N NW (EQ (WORD PTW) PREV)) (CUT PTW))
         (AND (OR (EQ PREV 'BUT) (EQ (CADR PREV) 'BUT))
          (NEXTWORD? 'NOT)
          (OR (FLUSHME) (GO LOSE2))
          (FQ NEGBUT))
         (SETQ :RESULT
           (COND ((MEMQ (CAR REST) '(ADJ NUM NOUN PREP VB ADV))
              (PARSE3 (APPEND REST '(COMPONENT)) NIL))
             ((MEMQ (CAR REST) '(NG PREPG ADJG))
              (AND (NOT (CQ OFOBJ))
                   (PARSE2 (APPEND REST '(COMPONENT)) NIL)))
             ((EQ (CAR REST) 'CLAUSE)
              ((LAMBDA (LASTSENT AUXFE)
                   (AND (PARSE2 (APPEND REST AUXFE '(COMPONENT)) NIL)
                    (OR (NOT AUXFE) (F (CAR AUXFE)))
                    (SETR 'TIME (GETR 'TIME H) C)))
               (COND ((ISQ H MAJOR) H) (LASTSENT))
               (MEET (FE H) '(DECLAR IMPER))))))
         (COND ((NULL :RESULT) (GO LOSE2)))
         (CUT END)
         (COND ((NOT (ATOM PREV)) (GO RETSM))
           ((EQ PREV '/,)
            (COND ((NEXTWORD? COMMA) (FQ LIST) (GO UP)) (T (GO LIST))))
           ((MEMQ PREV '(AND OR NOR BUT))
            (COND ((EQ BOTH (NB H)) (FQ BOTH)))
            (COND ((OR (NEXTWORD? 'BUT)
                   (AND (NEXTWORD? PREV)
                    (NOT (AND (EQ BOTH (NB H)) (EQ PREV 'AND)))))
               (FQ LISTA)
               (F PREV)
               (GO UP))
              (T (GO LISTA)))))
    LOSE2(AND LABELTRACE (PASSING 'LOSE2))
         (SETQ :RESULT (CQ LISTA))
         (COND (:RESULT (GO LISTA)))
    LIST (AND LABELTRACE (PASSING 'LIST))
         (SETQ :RESULT (AND (EQ PREV '/,)
                (EQUAL (LENGTH H) 2)
                (ISQ H NG)
                (NOT (OR (ISQ H PRONG) (ISQ (CDR H) PRONG)))
                (OR (NEXTWORD? COMMA) (NULL N))))
         (COND ((NULL :RESULT) (M CONJOIN:) (GO FAIL)))
         (FLUSHME)
         (FQ APPOSITIVE)
         (GO RETSM)
    LISTA(AND LABELTRACE (PASSING 'LISTA))
         (F PREV)
    RETSM(AND LABELTRACE (PASSING 'RETSM))
         (FQ COMPOUND)
         (AND (GREATERP (LENGTH H) 2) (FQ LIST))
         (COND ((OR (CQ NG) (CQ NOUN))
            (COND ((CQ AND) (FQ NPL)) (T (MOVE-PT H) (TRNSF NPL NS MASS NFS))))
           ((CQ VB)
            (PROG (COMMON)
              (SETQ COMMON (GET 'VB 'ELIM))
              (MAP '(LAMBDA (X) (SETQ COMMON (MEET COMMON (FE X)))) H))
            (FESET (UNION COMMON (FE C)) C)))
         (SETQ :RESULT (CALLSM (SMCONJ)))
         (COND (:RESULT (GO RETURN)) (T (M CONJOIN:) (GO FAIL)))
    FAIL (SETQ MES ME)
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
                (SETQ RE (COND ((MEMQ (CAR REST) '(PREP ADV))
                        (PARSE3 REST T))
                           ((MEMQ (CAR REST)
                              '(NG PREPG ADJG CLAUSE))
                        (PARSE2 REST T))))
                (LESSP (LENGTH N) (LENGTH BOTH))
                (RETURN (SETQ SPECIAL 'SKIP)))
               (SETQ RE NIL)
               (SETQ N NBB)
               (RETURN NIL)))))

(DEFUN DOUBLEQUOTER NIL (APPLY-GRAMMAR 'PARSEQUOTED))

(DEFUN CANTAKE (NUM TYPE FEATURE)
       (PROG (VBFEAT)
         (SETQ VBFEAT (FE MVB))
         (RETURN (COND ((MEMQ 'RSNG TYPE)
                (MEMQ
                 (READLIST (APPEND (COND ((MEMQ 'TO TYPE) '(T O))
                             ((MEMQ 'ING TYPE) '(I N G))
                             ((MEMQ 'REPORT TYPE)
                              '(R E P)))
                           '(O B)
                           (LIST (COND ((EQ NUM 1) '/1)
                               (T '/2)))))
                 VBFEAT))
               ((MEMQ 'COMP TYPE) (MEMQ 'INT VBFEAT))
               ((MEMQ 'NG TYPE)
                (COND ((EQUAL NUM 1)
                   (NOT (NULL (MEET '(TRANS TRANS2 TRANSL TRANSINT)
                            VBFEAT))))
                  (T (MEMQ 'TRANS2 VBFEAT))))
               (T (MEMQ FEATURE VBFEAT))))))

(DEFUN CANPARSE (NUM TYPE FEATURE)
       (PROG (REG)
         (AND (CANTAKE NUM TYPE FEATURE)
          (OR (NULL TYPE)
              (AND (APPLY 'PARSE
                  (APPEND TYPE
                      (COND ((MEMQ 'COMP TYPE)
                         (SETQ REG 'COMP)
                         NIL)
                        (T (LIST 'OBJ
                             (SETQ REG
                                   (COND ((OR (MEMQ 'LOC
                                        TYPE)
                                      (MEMQ 'PLACE
                                        TYPE))
                                      'LOBJ)
                                     ((EQUAL NUM 1)
                                      'OBJ1)
                                     (T 'OBJ2))))))))
               (SETR REG H C)))
          (OR (NULL FEATURE) (F FEATURE))
          (RETURN T))))

REMEMBER/ TO/ UFILE

§ anno/winograd/init

;;;   THIS IS A PACKAGE FOR LOADING SHRDLU'S INTO CORE FROM THE DISK FILES.
;;;    THE PROCEDURE IS TO FIRST LOAD A BLISP (IGNORE ALLOCATIONS, THE
;;;    PROGRAMS DO THEIR OWN). AND UREAD THIS FILE. EXECUTING "LOADSHRDLU"
;;;    WILL GENERATE (AFTER SOME TIME) A FULLY INTERPRETED VERSION.
;;;    PARTIALLY COMPILED MIXES ARE AVAILLABLE, AS SEEN BELOW.
;;;    THE VARIABLE "VERSION-FILES" KEEPS A RUNNING TAB OF THE FILES
;;;    LOADER VIA "LOADER". IF ANY ERRORS OCCUR DURING READIN THEY
;;;    ARE PROTECTED BY AN "ERRSET" AND LOADING CONTINUES. (NOTE !! IF AN
;;;    UNBOUND PAREN CAUSES THE FILE TO BE TERMINATED TOO SOON, YOU'LL
;;;    NEVER NOTICE)
;;;

(comment symbol 5000)   ;this file can now be used as a lisp init file via. the jcl line

(SETQ *RSET T)

(DEFUN LOADER (*!?KEY)
       (OR (ERRSET (EVAL (LIST 'UREAD
                   *!?KEY
                   '>
                   'DSK
                   'SHRDLU))
           NIL)
       (AND (PRINT *!?KEY)
        (PRINC 'NOT-FOUND)
        (RETURN NIL)))
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
       (ALLOC '(LIST 60000
             FIXNUM
             15000
             SYMBOL
              15000
                     array 500 ))
       (SETQ PURE NIL)
;       (MAPC 'LOADER '(PLNR THTRAC))
;       (THINIT)
;       (SETQ THINF NIL THTREE NIL THLEVEL NIL)
;       (setq errlist nil)   ;removes micro-planner's fangs
       (MAPC 'LOADER '(SYSCOM MORPHO SHOW))
       (MAPC 'LOADER '(PROGMR GINTER GRAMAR DICTIO))
       (MAPC 'LOADER '(SMSPEC SMASS SMUTIL))
;       (LOADER 'NEWANS)
;       (MAPC 'LOADER '(BLOCKS DATA))
       (FASLOAD TRACE FASL COM COM)
;       (FASLOAD GRAPHF FASL DSK SHRDL1)
       (LOADER 'SETUP)
;       (fasload grindef fasl com com)
       'CONSTRUCTION/ COMPLETED)

;this now loads all of SHRDLU except the blocks world code, microplanner, and the
;answering capability.    - the shift to bibop lisp seems to have crippled microplanner
;in a non-obvious way, and the answering code is useless without the blocks world.

(loadshrdlu)

§ anno/winograd/loader

;;;   THIS IS A PACKAGE FOR LOADING SHRDLU'S INTO CORE FROM THE DISK FILES.
;;;    THE PROCEDURE IS TO FIRST LOAD A BLISP (IGNORE ALLOCATIONS, THE
;;;    PROGRAMS DO THEIR OWN). AND UREAD THIS FILE. EXECUTING "LOADSHRDLU"
;;;    WILL GENERATE (AFTER SOME TIME) A FULLY INTERPRETED VERSION.
;;;    PARTIALLY COMPILED MIXES ARE AVAILLABLE, AS SEEN BELOW.
;;;    THE VARIABLE "VERSION-FILES" KEEPS A RUNNING TAB OF THE FILES
;;;    LOADER VIA "LOADER". IF ANY ERRORS OCCUR DURING READIN THEY
;;;    ARE PROTECTED BY AN "ERRSET" AND LOADING CONTINUES. (NOTE !! IF AN
;;;    UNBOUND PAREN CAUSES THE FILE TO BE TERMINATED TOO SOON, YOU'LL
;;;    NEVER NOTICE)
;;;

(SETQ *RSET T)

(DEFUN LOADER (*!?KEY)
       (OR (ERRSET (EVAL (LIST 'UREAD
                   *!?KEY
                   '>
                   'DSK
                   'SHRDLU))
           NIL)
       (AND (PRINT *!?KEY)
        (PRINC 'NOT-FOUND)
        (RETURN NIL)))
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
   (princ '/ )
   (PRINC (CADR SPECS))
   (OR (ERRSET (APPLY 'FASLOAD SPECS))
       (ERT lossage in loading - try again ?)))

(SETQ VERSION-FILES NIL)

(DEFUN LOADSHRDLU NIL
       (ALLOC '(LIST 320000
             FIXNUM
             15000
             SYMBOL
              15000
                     array 500 ))
       (SETQ PURE NIL)
       (MAPC 'LOADER '(PLNR THTRAC))
       (THINIT)
       (SETQ THINF NIL THTREE NIL THLEVEL NIL)
       (setq errlist nil)   ;removes micro-planner's fangs
       (MAPC 'LOADER '(SYSCOM MORPHO SHOW))
       (MAPC 'LOADER '(PROGMR GINTER GRAMAR DICTIO))
       (MAPC 'LOADER '(SMSPEC SMASS SMUTIL))
       (LOADER 'NEWANS)
       (MAPC 'LOADER '(BLOCKS DATA))
       (FASLOAD TRACE FASL COM COM)
       (FASLOAD GRAPHF FASL DSK SHRDL1)
       (LOADER 'SETUP)
       (fasload grindef fasl com com)
       'CONSTRUCTION/ COMPLETED)

(defun loadparser nil
    (mapc 'loader '(syscom morpho show))
    (mapc 'loader '(progmr ginter gramar dictio))
    (loader 'setup)
    (load parser > dsk shrdl1)        ;load is defined on ddm;*load >
    'complete-call-setup-num-date)

(DEFUN PARSER-compiled NIL
    (SETQ PURE NIL)
    (FLOAD FASL SYSCOM DSK SHRDL1)
    (FLOAD FASL MORPHO DSK SHRDL1)
    (FLOAD FASL SHOW DSK SHRDL1)
    ;;;
    (FLOAD FASL PROGMR DSK SHRDL1)
    (FLOAD FASL GRAMAR DSK SHRDL1)
    (LOADER 'DICTIO)
    ;;;
    (FLOAD TRACE FASL COM COM)
    (FLOAD FASL SETUP DSK SHRDL1)
    ;;;
    (load parser > dsk shrdl1)
    'PARSER-LOADED)

(DEFUN SHRDLU-COMPILED ()
    (SETQ PURE NIL)
    (FLOAD FASL SYSCOM  DSK SHRDL1)
    (FLOAD FASL MORPHO DSK SHRDL1)
    (FLOAD FASL SHOW DSK SHRDL1)
;;
    (FLOAD FASL PROGMR DSK SHRDL1)
    (FLOAD FASL gRAMar  DSK SHRDL1)
    (LOADER 'DICTIO)
;;
    (FLOAD FASL SMSPEC DSK SHRDL1)
    (FLOAD FASL SMASS DSK SHRDL1)
    (FLOAD FASL SMUTIL DSK SHRDL1)
;;
    (FLOAD FASL NEWANS DSK SHRDL1)
;;
    (FLOAD FASL PLNR DSK SHRDL1)
    (LOADER 'THTRAC)
    (THINIT)
    (SETQ THINF NIL THTREE NIL THLEVEL NIL)
    (setq errlist nil)
    (FLOAD FASL BLOCKL DSK SHRDL1)
    (LOADER 'BLOCKP)
    (LOADER 'DATA)
;;
    (FLOAD GRAPHF FASL DSK SHRDL1)
    (FLOAD TRACE FASL COM COM)
    (FLOAD FASL SETUP DSK SHRDL1)
    'COMPLETED)

§ anno/winograd/macros

################################################################

        .MACR 21

   a precompiler for programmer code
################################################################

(DEFUN LIST-NO-NILS FEXPR (ELEMENTS)
       (DO ((TAKEUP-REEL)
        (TEMP NIL (EVAL (CAR ELEMENTS)))
        (ELEMENTS ELEMENTS (CDR ELEMENTS)))
       ((NULL ELEMENTS) (REVERSE TAKEUP-REEL))
       (AND TEMP (SETQ TAKEUP-REEL (CONS TEMP TAKEUP-REEL)))))

(DEFUN TOPLEVEL-LIST FEXPR (ELEMENTS)
       ;;ACTS LIKE LIST EXCEPT THAT IF ANY ELEMEMNT EVALUATES IN TO
       ;;MORE THAN A SINGLE ELEMENT ( - RETURNS A LIST WHOSE CAR IS
       ;;ALSO A LIST - ) THEN THE ELEMENTS OF THAT ELEMENT ARE ADDED
       ;;TO THE SAME LEVEL AS THE SEPARATE ELEMENTS IN THE CALL
       (MAPCAN '(LAMBDA (ELEMENT) (SETQ ELEMENT (EVAL ELEMENT))
                  (COND ((ATOM (CAR ELEMENT))
                     (LIST ELEMENT))
                    (T ELEMENT)))
           ELEMENTS))

(DEFUN GRAM-COMP (FILE)
       (PROG (^Q UNIQUE ^R ^D)
         (OR (APPLY 'UREAD FILE)
         (RETURN 'BAD-FILE-SPECS))
         (IOC Q)
         (SETQ UNIQUE (GENSYM))
         (APPLY 'UWRITE (STATUS CRUNIT))
         (DO ((R (READ UNIQUE) (READ UNIQUE)))
         ((EQ R UNIQUE))
         (COND ((MEMQ (CAR R) '(DEFUN SETQ DEFPROP)))
               (T (SETQ R (EVAL R))))
         ((LAMBDA (^R) (SPRINTER R)) T)))
       'REMEMBER/ TO/ UFILE)

(DEFUN PDEFINE FEXPR (MOBY)
       (LIST 'DEFUN
         (CAR MOBY)
         'NIL
         (APPEND (LIST 'PROG
               (APPEND '(FE H
                    ME
                    NB
                    C
                    SM
                    CUT
                    NN
                    T1
                    T2
                    T3
                    :RESULT)
                   (CADR MOBY))
               '(SETQ NN T)
               '(SETQ CUT END)
               '(SETQ C
                  (BUILDNODE (SETQ FE (REVERSE REST))
                         (SETQ NB (OR (NB RE) N))
                         N
                         (SETQ H RE)
                         NIL))
               '(SETR 'PARENT PARENT C))
             (APPLY ':-SPREAD (CDDR MOBY))
             (LIST 'FAIL
               '(SETQ MES ME)
               '(SETQ N (OR (N RE) NB))
               '(RETURN NIL)
               'RETURN
               '(SETQ MES ME)
               '(RETURN (REBUILD (REVERSE FE)
                         NB
                         N
                         H
                         SM
                         C))))))

(DEFUN :-SPREAD FEXPR (LIST)
       (MAPCAN
    '(LAMBDA (EXP)
      (PROG (PREDICATE T1 T2 T3)
        (COND
         ((ATOM EXP)
          (RETURN (LIST EXP
                (LIST 'AND
                      'LABELTRACE
                      (LIST 'PASSING (list 'quote exp))))))
         ((EQ (CAR EXP) 'GOCOND)
          (RETURN (LIST (LIST 'COND
                      (LIST 'NN
                        (LIST 'GO
                          (CADR EXP)))
                      (LIST 'T
                        (LIST 'GO
                          (CADDR EXP)))))))
         ((EQ (CAR EXP) ':)
          (SETQ PREDICATE
            (CADR EXP)
            T1
            (CADDR EXP)
            T2
            (CADDDR EXP))
          (AND (CDDDDR EXP) (SETQ T3 (CAR (CDDDDR EXP))))
          (RETURN
           (LIST
            (LIST 'SETQ ':RESULT PREDICATE)
            (COND
             ((AND T1 (NULL T2))
              ;;T3 CAN BE EITHER THERE OR NOT
              (LIST
               'COND
               (TOPLEVEL-LIST
            ':RESULT
            (COND
             (T3 (LIST 'COND
                   (TOPLEVEL-LIST (LIST 'NULL
                            'NN)
                          (TAG-CHECK T3))
                   (TOPLEVEL-LIST 'T
                          (TAG-CHECK T1))))
             (T (TAG-CHECK T1))))))
             ((AND (NULL T1) T2 (NULL T3))
              (LIST 'COND
                (TOPLEVEL-LIST (LIST 'NULL
                         ':RESULT)
                       (TAG-CHECK T2))))
             ((AND (NULL T1) T2 T3)
              (LIST
               'COND
               (LIST (LIST 'NULL ':RESULT)
                 (LIST 'COND
                   (TOPLEVEL-LIST (LIST 'NULL
                            'NN)
                          (TAG-CHECK T3))
                   (TOPLEVEL-LIST 'T
                          (TAG-CHECK T2))))))
             ((AND T1 T2 (NULL T3))
              (LIST 'COND
                (TOPLEVEL-LIST ':RESULT
                       (TAG-CHECK T1))
                (TOPLEVEL-LIST 'T (TAG-CHECK T2))))
             ((AND T1 T2 T3)
              (LIST
               'COND
               (LIST ':RESULT
                 (LIST 'COND
                   (TOPLEVEL-LIST (LIST 'NULL
                            'NN)
                          (TAG-CHECK T3))
                   (TOPLEVEL-LIST 'T
                          (TAG-CHECK T1))))
               (TOPLEVEL-LIST 'T (TAG-CHECK T2))))
             ((AND (NULL T1) (NULL T2) T3)
              (LIST 'COND
                (TOPLEVEL-LIST (LIST 'AND
                         (LIST 'NULL
                               'NN)
                         ':RESULT)
                       (TAG-CHECK T3))))
             ((AND (NULL T1) (NULL T2) (NULL T3))
              (LIST 'I-AM-A-TAG))))))
         (T (RETURN (LIST EXP))))))
    LIST))

(DEFUN TAG-CHECK (TAG-EXP)
       (COND ((ATOM TAG-EXP) (LIST (LIST 'GO TAG-EXP)))
         (T (LIST (LIST 'M (CAR TAG-EXP))
              (LIST 'GO 'FAIL)))))

§ anno/winograd/parser

;;;################################################################
;;;
;;;   PARSER - setup file for parsing system in programmar
;;;
;;;################################################################

(defun setup (gram-num date)
    (suspend)
    (cursorpos 'c)
    (terpri)
    (princ 'shrdlu/'/s/ P/a/r/s/e/r/ / / )
    (princ '/u/s/i/n/g/ /g/r/a/m/m/a/r/ )
    (princ gram-num)
    (terpri)
    (princ date)
    (princ '/ / lisp/ )
    (princ (status lispversion))
    (terpri)
    (terpri)
    (say this is a read-eval-print loop)
    (say type "go/ " to enter ready state)
    (catch (ert) abort-parser)
    (sstatus toplevel '(parser))
    (parser))

(setq makeintern t ;;;  switch for interning the atoms created
;;;   for the node structure
  sh-standard-printout nil ;;;  switch for evaluating display functions
;;;   in the function SHSTPO (the SHOW file)
  sh-afteranswer-pause t  ;;;   switch for causing a break after each
;;;  sentence is processed.
    )

(setq annoyance t  ;;;  turns off the [1] printouts in SHRDLU
 smn t ;;;   turns off evaluation by real smn-fns
)

(setq car t cdr t ;;;  annoying patch to keep *RSET happy
     )

(DEFUN parser NIL
       (PROG (ERT-TIME END AMB TIMAMB BOTH BACKREF BACKREF2 ANSNAME
          LASTREL WHO PT PTW SENT PUNCT IGNORE H N NB FE SM RE
          MES MESP C CUT CURTIME STATE GLOBAL-MESSAGE LEVEL
          P-TIME SMN-TIME PLNR-TIME ANS-TIME ANS-PLNR-TIME
          SH-GCTIME)
         (CLEANOUT TSS EVX NODE ANS OSS RSS X)               ;FLUSH OLD GENSYMS
    CATCH-LOOP
         (CATCH
          (PROG NIL
           LOOP (SETQ SENTNO (ADD1 SENTNO)
              PARSINGS 0.
              LEVEL 0.
              LASTSENTNO (ADD1 LASTSENTNO)
              LASTSENT C
              GLOBAL-MESSAGE NIL
              MES 'NOPE
              BACKREF NIL                        ;???????????????????
              RUNTIME (RUNTIME)
              SH-GCTIME (STATUS GCTIME)
              PLNR-TIME 0.
              ANS-PLNR-TIME 0.
              SMN-TIME 0.
              ERT-TIME 0.)
           UP   (SETQ N (SETQ SENT (ETAOIN)))
            (OR ANNOYANCE (PRINT *1))
            (AND ^Q (%))
            (IOC S)
            (AND IGNORE (GO UP))
            ;;;
            (COND
             ((AND
               (COND
            (TOPLEVEL-ERRSET?
             (ERRSET
              (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
            (T (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))))
               C)
              (OR ANNOYANCE (PRINT *2))
              (SETQ FE (FE C))
              (SETQ NB SENT)
              (SETQ H (H C))
              (SETQ INTERPRETATION (SM C))
(terpri)
(princ 'time/ spent/ parsing/ )
(princ p-time))
             ((PRINT *3)
              (APPLY 'SAY
                 (OR GLOBAL-MESSAGE
                 '(I DON/'T UNDERSTAND/.)))))
            (AND MOBYTEST-IN-PROGRESS (AFTER-EACH-SENTENCE))
            (AND SH-STANDARD-PRINTOUT (SHSTPO))
            (AND SH-AFTERANSWER-PAUSE (ERT))
            (GO LOOP))
          ABORT-PARSER)
         (GO CATCH-LOOP)))

(DEFUN ETAOIN NIL
;;;  has a patch added to permit online definition
;;;  of an unknown word's syntactic features
;;;
       (PROG (WORD NEWWORD CHAR ALTN ALREADY-BLGING-NEWWRD WRD LAST features
          NEXT Y WORD1 X RD POSS)
    THRU (SETQ SENT (SETQ WORD (SETQ PUNCT (SETQ POSS NIL))))
         (PRINT 'READY)
         (TERPRI)
         (AND MOBYREAD (IOC Q))
    CHAR (COND ((EQUAL (TYIPEEK) 24.) (READCH) (ERT) (GO THRU)); "cntrl-x" break
;left over from CMU
                   ((= (tyipeek) 3.)
(or (and mobyread (end-of-file-condition))
    (bug etaoin: about to read eof)) )
)
         (setq char (cond ((greaterp 123. (setq char (tyi)) 96.) (- char 32.))
                  ((greaterp 91. char 64.) char)
                  (t char))
                  char (ascii char)
                   ;;this little hack maps all lowercase letters into uppercase.
           ;;a more reasonable thing to do would be to hack the chtrans
           ;;property of the current readtable, but this was quicker to
           ;;patch.
                   )
             (cond ((EQ char '/ ) (GO WORD))           ;DELIMITER
           ((MEMQ CHAR ALTMODE)
            (setq char (ascii (uppercase-ify-char (tyi))) )
            (COND ((MEMQ char ALTMODE)
               (ERT)
               (GO THRU))
                                       ;ALTMODE-ALTMODE
              ((EQ CHAR 'C) (TYO 12.) (GO DO))
                                       ;ALTMODE-C
              ((EQ CHAR 'R) (TERPRI) (GO DO))
                                       ;ALTMODE-R
              ((AND (EQ CHAR 'S) SAVESENT)
                                       ;ALTMODE-S CAUSES THE LAST SENTENCE TYPED IN TO
               (SETQ SENT (CAR SAVESENT))
                                       ;RETURNED AS THE SENTENCE TO BE INTERPRETED
               (SETQ PUNCT (CDR SAVESENT))
               (%)
               (RETURN SENT))
              ((EQ CHAR 'N)
               (SETQ NEWWORD (NOT NEWWORD)
                 ALTN (NOT ALTN))
               (GO CHAR))
                                       ;ALTMODE-N COMPLEMENTS THE NEWWORD FLAG, WHICH
              ((EQ CHAR 'Q)
                                       ;DETERMINES WHETHER UNRECOGNIZED WORDS WILL BE
               (IOC Q)
                                       ;CONSIDERED SPELLING ERRORS OR NEW WORDS.
               (SETQ IGNORE NIL)
               (GO THRU))
                                       ;ALTMODE-Q CAUSES READIN FROM DISK FILE.
              ((EQ CHAR 'M)
               (IOC Q)
               (SETQ IGNORE NIL MOBYREAD T)
               (GO thru))
              ((EQ CHAR 'I)
               (SETQ IGNORE T)
               (IOC Q)
               (GO THRU))
                                       ;ALTMODE-I IGNORES SENTENCE READ FROM FILE.
              ((GO THRU))))
           ((EQ CHAR RUBOUT)
            (COND (WORD (PRINC (CAR WORD))
                (SETQ WORD (CDR WORD)))
              (SENT (PRINT (CAR SENT))
                (SETQ SENT (CDR SENT))))
            (GO CHAR))
           ((EQ CHAR CARRET) (GO WORD))
           ((MEMQ CHAR PUNCL)
            (SETQ PUNCT CHAR)
                                       ;DELIMITER
            (AND WORD (GO WORD))
            (GO PUNC)))
         (AND
          (OR (AND (EQ CHAR '")
               (NOT ALREADY-BLGING-NEWRD)
               (SETQ NEWWORD (SETQ ALREADY-BLGING-NEWRD T))
               (GO CHAR))
          (AND (EQ CHAR '")
               ALREaDY-BLGING-NEWRD
               (NOT (SETQ ALREADY-BLGING-NEWRD NIL))
               (GO WORD))
                                       ;WITHIN THIS "AND" ARE ALL THE CHARACTERS THAT
          (NUMBERP CHAR)
                                       ;ARE UNDERSTOOD BY THE SYSTEM
          (AND (EQ CHAR '=) (NULL WORD))
          (MEMQ CHAR VOWEL)
          (MEMQ CHAR CONSO))
          (SETQ WORD (CONS CHAR WORD)))
         (GO CHAR)
    DO   (PRINT 'READY)
         (TERPRI)
         (MAPC (FUNCTION (LAMBDA (X) (PRINT2 X))) (REVERSE SENT))
         (PRINC '/ )
         (MAPC (FUNCTION PRINC) (REVERSE WORD))
         (GO CHAR)
    WORD (COND ((NULL WORD) (GO CHAR))
           ((EQUAL WORD '(P L E H)) (HELP) (GO THRU))
           ((AND (SETQ WRD (ERRSET (READLIST (REVERSE WORD))))
             (NUMBERP (SETQ WRD (CAR WRD))))
            (SETQ SENT (CONS WRD SENT))
            (BUILDWORD WRD
                   (OR (AND (ZEROP (SUB1 WRD))
                    '(NUM NS))
                   '(NUM))
                   (LIST 'NUM WRD)
                   NIL))
                                       ;NO ROOT FOR NUMBERS
           ((NULL WRD) (SETQ WRD (REVERSE WORD)) (GO NO))
           ((GET WRD 'FEATURES))
                                       ;IF A WORD HAS FEATURES, IT'S PROPERTIES
           ((SETQ X (GET WRD 'IRREGULAR))
                                       ;ARE ALL SET UP IN THE DICTIONARY
            (BUILDWORD WRD
                   (MOD (GET (CAR X) 'FEATURES)
                    (CDR X))
                   (SM X)
                   (CAR X)))
           ((EQ (CAR (LAST WORD)) '=)
            (BUILDWORD WRD
                   (COND ((MEMQ '" WORD)
                      '(PROPN NS POSS))
                     ('(PROPN NS)))
                   '((PROPN T))
                   NIL))
           ((GO CUT)))
         (GO WRD)

         ;;;---------------------------------------------
         ;;;              MORPHOLOGY CODE
         ;;;--------------------------------------------
    CUT  (COND ((STA WORD '(T " N))
            (SETQ RD (CDDDR WORD))
            (SETQ WORD (CONS '* WORD))
            (GO TRY))
           ((STA WORD '(S "))
            (SETQ WORD (CDDR WORD))
            (SETQ POSS WRD)
            (GO WORD))
           ((STA WORD '("))
            (SETQ WORD (CDR WORD))
            (SETQ POSS WRD)
            (GO WORD))
           ((STA WORD '(Y L))
            (SETQ RD (CDDR WORD))
            (GO LY))
           ((STA WORD '(G N I)) (SETQ RD (CDDDR WORD)))
           ((STA WORD '(D E)) (SETQ RD (CDDR WORD)))
           ((STA WORD '(N E)) (SETQ RD (CDDR WORD)))
           ((STA WORD '(R E)) (SETQ RD (CDDR WORD)))
           ((STA WORD '(T S E)) (SETQ RD (CDDDR WORD)))
           ((STA WORD '(S))
            (SETQ RD (CDR WORD))
            (GO SIB))
           (T (GO NO)))
         (SETQ LAST (CAR RD))
         (SETQ NEXT (CADR RD))
         (COND ((AND (MEMQ LAST CONSO)
             (NOT (MEMQ LAST LIQUID))
             (EQ LAST NEXT))
            (SETQ RD (CDR RD)))
           ((EQ LAST 'I)
            (SETQ RD (CONS 'Y (CDR RD))))
           ((OR (AND (MEMQ LAST CONSO)
                 (MEMQ NEXT VOWEL)
                 (NOT (EQ NEXT 'E))
                 (MEMQ (CADDR RD) CONSO))
            (AND (MEMQ LAST LIQUID)
                 (MEMQ NEXT CONSO)
                 (NOT (MEMQ NEXT LIQUID)))
            (AND (EQ LAST 'H) (EQ NEXT 'T))
            (AND (MEMQ LAST '(C G S J V Z))
                 (OR (MEMQ NEXT LIQUID)
                 (AND (MEMQ NEXT VOWEL)
                      (MEMQ (CADDR RD) VOWEL)))))
            (SETQ RD (CONS 'E RD))))
         (GO TRY)
    LY   (COND ((AND (MEMQ (CAR RD) VOWEL)
             (NOT (EQ (CAR RD) 'E))
             (MEMQ (CADR RD) CONSO))
            (SETQ RD (CONS 'E RD))))
         (COND ((MEMQ 'ADJ
              (GET (SETQ ROOT (READLIST (REVERSE RD)))
                   'FEATURES))
            (BUILDWORD WRD
                   '(ADV VBAD)
                   NIL
                                       ;TEMP NIL SEMANTICS
                   ROOT)
                                       ;ROOT IS THE ADJECTIVE
            (GO WRD)))
         (GO NO)
    SIB  (SETQ LAST (CAR RD))
         (SETQ NEXT (CADR RD))
         (COND ((NOT (EQ LAST 'E)))
           ((EQ NEXT 'I)
            (SETQ RD (CONS 'Y (CDDR RD))))
           ((EQ NEXT 'X) (SETQ RD (CDR RD)))
           ((AND (EQ NEXT 'H)
             (NOT (EQ (CADDR RD) 'T)))
            (SETQ RD (CDR RD)))
           ((AND (MEMQ NEXT '(S Z))
             (EQ NEXT (CADDR RD)))
            (SETQ RD (CDDR RD))))
    TRY  (COND
          ((OR
        (SETQ FEATURES
              (GET (SETQ ROOT (READLIST (REVERSE RD)))
               'FEATURES))
        (AND (SETQ X (GET ROOT 'IRREGULAR))
             (SETQ FEATURES
               (MOD (GET (SETQ ROOT (CAR X))
                     'FEATURES)
                (CDR X)))))
           (BUILDWORD WRD
              (MOD FEATURES (GET (CAR WORD) 'MOD))
              (GET ROOT 'SEMANTICS)
              ROOT))
          ((EQ (CAR RD) 'E) (SETQ RD (CDR RD)) (GO TRY))
          ((GO NO)))

         ;;;----------------------------------------------------
         ;;;  BUILD UP THE PROCESSED LIST OF WORDS TO BE RETURNED
         ;;;----------------------------------------------------
    WRD  (SETQ
          SENT
          (COND (POSS (COND ((OR (MEMQ 'NOUN
                       (SETQ FEATURES
                         (GET WRD
                              'FEATURES)))
                                       ;IF IT'S A NOUN
                     (MEMQ 'PROPN FEATURES))
                                       ;OR A PROPER NOUN
                 (BUILDWORD POSS
                        (APPEND (MEET FEATURES
                                       ;MARK IT AS POSSESSIVE
                              (GET 'POSS
                                   'ELIM))
                            '(POSS))
                        (GET WRD
                         'SEMANTICS)
                        ROOT)
                 (CONS POSS SENT))
                ((BUILDWORD '"S
                                       ; CAN WE GENERALIZE IT???
                        '(VB BE V3PS PRES)
                        (GET 'BE
                         'SEMANTICS)
                        'BE)
                 (CONS '"S (CONS WRD SENT)))))
            ((CONS WRD SENT))))
    PUNC (COND
          (PUNCT (COND ((AND (EQ PUNCT '?) (NULL SENT))
                (HELP)
                (GO THRU))
               ((MEMQ PUNCT FINAL)
                (RETURN (CAR (SETQ SAVESENT
                           (CONS (REVERSE SENT)
                                       ;RETURN POINT !!!!!!!!!!!!!
                             PUNCT)))))
               ((SETQ SENT (CONS PUNCT SENT))))))
         (SETQ PUNCT NIL)
         (SETQ WORD (SETQ POSS NIL))
         (GO CHAR)
    NO   (COND (NEWWORD (BUILDWORD WRD
                       '(NOUN NS)
                       '((NOUN (SMNEWNOUN))
                     (PROPN (SMNEWPROPN)))
                       WRD)
                (OR ALTN (SETQ NEWWORD NIL))
                (GO PUNC)))
         (TERPRI)
         (SAY *SORRY I DON/'T KNOW THE WORD ")
         (PRINC WRD)
         (PRINC '/ "/.)
         (TERPRI)
(cond (define-online
(terpri)
(say what are its syntactic features?)
(setq features (read))
(buildword wrd features 'dummy wrd)
(terpri)
(mapc '(lambda (w) (print2 w)) (reverse sent))
(print2 wrd)
(princ '/ )
(go char)
))
         (SAY PLEASE TYPE <LF> AND CONTINUE THE SENTENCE/.)
    NOGO (OR (EQUAL (TYI) 10.) (GO NOGO))
         (SETQ PUNCT NIL WORD NIL)
         (GO DO)))

(defun build fexpr (foo)
 ;;;  this is a semantic function which packages
;;;  semantic nodes.
 t )
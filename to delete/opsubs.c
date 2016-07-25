/* ../src/opsubs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     File  opsubs.f */

/*     opfile   oplook   opnumb   opscan   optokn   opuppr */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Subroutine */ int opfile_(integer *ioptns, integer *iprint, integer *isumm,
	 logical *listop, logical *newopt, integer *inform__, S_fp opkey)
{
    /* Format strings */
    static char fmt_2000[] = "(//\002 XXX  Error while looking for an OPTION"
	    "S file on unit\002,i7/\002 XXX  The file should start with BEGIN"
	    ", SKIP or ENDRUN\002/\002 XXX  but the first record found was th"
	    "e following:\002//\002 ---->\002,a//\002 XXX  Continuing to look"
	    " for OPTIONS file...\002)";
    static char fmt_2200[] = "(//\002 XXX  End-of-file encountered while pro"
	    "cessing\002,\002 an OPTIONS file on unit\002,i6)";
    static char fmt_2300[] = "(//\002 XXX  End-of-file encountered while loo"
	    "king for\002,\002 an OPTIONS file on unit\002,i6)";

    /* System generated locals */
    integer i__1;
    cilist ci__1;

    /* Builtin functions */
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsfe(cilist *), e_wsfe(
	    void);

    /* Local variables */
    integer nkey, nread;
    char token[16*1], buffer[72], oldbuf[72];
    extern /* Subroutine */ int optokn_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    char key[16];

    /* Fortran I/O blocks */
    static cilist io___6 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___7 = { 0, 0, 0, fmt_2000, 0 };
    static cilist io___9 = { 0, 0, 0, fmt_2200, 0 };
    static cilist io___10 = { 0, 0, 0, fmt_2200, 0 };
    static cilist io___11 = { 0, 0, 0, fmt_2300, 0 };
    static cilist io___12 = { 0, 0, 0, fmt_2300, 0 };


/*     ================================================================== */
/*     opfile  reads the options file from unit  iOptns  and loads the */
/*     options into the relevant elements of the integer and real */
/*     parameter arrays. */

/*     Systems Optimization Laboratory, Stanford University. */
/*     University of California, San Diego. */
/*     This version dated September 12 1995. */
/*     ================================================================== */
/*     Return if the unit number is out of range. */
    if (*ioptns < 0 || *ioptns > 99) {
	*inform__ = 1;
	return 0;
    }
/*     ------------------------------------------------------------------ */
/*     Look for  BEGIN, ENDRUN  or  SKIP. */
/*     ------------------------------------------------------------------ */
    nread = 0;
L50:
    ci__1.cierr = 0;
    ci__1.ciend = 1;
    ci__1.ciunit = *ioptns;
    ci__1.cifmt = "(a)";
    i__1 = s_rsfe(&ci__1);
    if (i__1 != 0) {
	goto L930;
    }
    i__1 = do_fio(&c__1, buffer, (ftnlen)72);
    if (i__1 != 0) {
	goto L930;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L930;
    }
    ++nread;
    nkey = 1;
    optokn_(buffer, &nkey, token, (ftnlen)72, (ftnlen)16);
    s_copy(key, token, (ftnlen)16, (ftnlen)16);
    if (s_cmp(key, "ENDRUN", (ftnlen)16, (ftnlen)6) == 0) {
	goto L940;
    }
    if (s_cmp(key, "BEGIN", (ftnlen)16, (ftnlen)5) != 0) {
	if (nread == 1 && s_cmp(key, "SKIP", (ftnlen)16, (ftnlen)4) != 0) {
	    if (*iprint > 0) {
		io___6.ciunit = *iprint;
		s_wsfe(&io___6);
		do_fio(&c__1, (char *)&(*ioptns), (ftnlen)sizeof(integer));
		do_fio(&c__1, buffer, (ftnlen)72);
		e_wsfe();
	    }
	    if (*isumm > 0) {
		io___7.ciunit = *isumm;
		s_wsfe(&io___7);
		do_fio(&c__1, (char *)&(*ioptns), (ftnlen)sizeof(integer));
		do_fio(&c__1, buffer, (ftnlen)72);
		e_wsfe();
	    }
	}
	goto L50;
    }
/*     ------------------------------------------------------------------ */
/*     BEGIN found. */
/*     This is taken to be the first line of an options file. */
/*     Read the second line to see if it is NOLIST. */
/*     ------------------------------------------------------------------ */
    s_copy(oldbuf, buffer, (ftnlen)72, (ftnlen)72);
    ci__1.cierr = 0;
    ci__1.ciend = 1;
    ci__1.ciunit = *ioptns;
    ci__1.cifmt = "(a)";
    i__1 = s_rsfe(&ci__1);
    if (i__1 != 0) {
	goto L920;
    }
    i__1 = do_fio(&c__1, buffer, (ftnlen)72);
    if (i__1 != 0) {
	goto L920;
    }
    i__1 = e_rsfe();
    if (i__1 != 0) {
	goto L920;
    }
    (*opkey)(iprint, isumm, listop, buffer, key, (ftnlen)72, (ftnlen)16);
    if (s_cmp(key, "NOLIST", (ftnlen)16, (ftnlen)6) == 0) {
	*listop = FALSE_;
    }
    if (*listop) {
	if (*newopt) {
	    if (*iprint > 0) {
		ci__1.cierr = 0;
		ci__1.ciunit = *iprint;
		ci__1.cifmt = "(// a / a /)";
		s_wsfe(&ci__1);
		do_fio(&c__1, " Optional Parameters", (ftnlen)20);
		do_fio(&c__1, " -------------------", (ftnlen)20);
		e_wsfe();
	    }
	    *newopt = FALSE_;
	}
	if (*iprint > 0) {
	    ci__1.cierr = 0;
	    ci__1.ciunit = *iprint;
	    ci__1.cifmt = "(6x, a )";
	    s_wsfe(&ci__1);
	    do_fio(&c__1, oldbuf, (ftnlen)72);
	    do_fio(&c__1, buffer, (ftnlen)72);
	    e_wsfe();
	}
    }
/*     ------------------------------------------------------------------ */
/*     Read the rest of the file. */
/*     ------------------------------------------------------------------ */
/* +    while (key .ne. 'END') loop */
L100:
    if (s_cmp(key, "END", (ftnlen)16, (ftnlen)3) != 0) {
	ci__1.cierr = 0;
	ci__1.ciend = 1;
	ci__1.ciunit = *ioptns;
	ci__1.cifmt = "(a)";
	i__1 = s_rsfe(&ci__1);
	if (i__1 != 0) {
	    goto L920;
	}
	i__1 = do_fio(&c__1, buffer, (ftnlen)72);
	if (i__1 != 0) {
	    goto L920;
	}
	i__1 = e_rsfe();
	if (i__1 != 0) {
	    goto L920;
	}
	if (*listop) {
	    if (*iprint > 0) {
		ci__1.cierr = 0;
		ci__1.ciunit = *iprint;
		ci__1.cifmt = "( 6x, a )";
		s_wsfe(&ci__1);
		do_fio(&c__1, buffer, (ftnlen)72);
		e_wsfe();
	    }
	}
	(*opkey)(iprint, isumm, listop, buffer, key, (ftnlen)72, (ftnlen)16);
	if (s_cmp(key, "LIST", (ftnlen)16, (ftnlen)4) == 0) {
	    *listop = TRUE_;
	}
	if (s_cmp(key, "NOLIST", (ftnlen)16, (ftnlen)6) == 0) {
	    *listop = FALSE_;
	}
	goto L100;
    }
/* +    end while */
    *inform__ = 0;
    return 0;
L920:
    if (*iprint > 0) {
	io___9.ciunit = *iprint;
	s_wsfe(&io___9);
	do_fio(&c__1, (char *)&(*ioptns), (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (*isumm > 0) {
	io___10.ciunit = *isumm;
	s_wsfe(&io___10);
	do_fio(&c__1, (char *)&(*ioptns), (ftnlen)sizeof(integer));
	e_wsfe();
    }
    *inform__ = 2;
    return 0;
L930:
    if (*iprint > 0) {
	io___11.ciunit = *iprint;
	s_wsfe(&io___11);
	do_fio(&c__1, (char *)&(*ioptns), (ftnlen)sizeof(integer));
	e_wsfe();
    }
    if (*isumm > 0) {
	io___12.ciunit = *isumm;
	s_wsfe(&io___12);
	do_fio(&c__1, (char *)&(*ioptns), (ftnlen)sizeof(integer));
	e_wsfe();
    }
    *inform__ = 3;
    return 0;
L940:
    if (*iprint > 0) {
	ci__1.cierr = 0;
	ci__1.ciunit = *iprint;
	ci__1.cifmt = "(// 6x, a)";
	s_wsfe(&ci__1);
	do_fio(&c__1, buffer, (ftnlen)72);
	e_wsfe();
    }
    if (*isumm > 0) {
	ci__1.cierr = 0;
	ci__1.ciunit = *isumm;
	ci__1.cifmt = "(// 6x, a)";
	s_wsfe(&ci__1);
	do_fio(&c__1, buffer, (ftnlen)72);
	e_wsfe();
    }
    *inform__ = 4;
    return 0;
/*     end of  opfile. */
} /* opfile_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* Subroutine */ int oplook_(integer *ndict, char *dictry, logical *alpha, 
	char *key, integer *entry__, ftnlen dictry_len, ftnlen key_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char flag__[16];
    integer mark, last, i__, first, length;
    extern /* Subroutine */ int opscan_(char *, integer *, integer *, integer 
	    *, ftnlen);
    char target[16];



/* Description and usage: */

/*       Performs dictionary lookups.  A pointer is returned if a */
/*    match is found between the input key and the corresponding */
/*    initial characters of one of the elements of the dictionary. */
/*    If a "synonym" has been provided for an entry, the search is */
/*    continued until a match to a primary dictionary entry is found. */
/*    Cases of no match, or multiple matches, are also provided for. */

/*     Dictionary entries must be left-justified, and may be alphabetized */
/*    for faster searches.  Secondary entries, if any, are composed of */
/*    two words separated by one or more characters such as blank, tab, */
/*    comma, colon, or equal sign which are treated as non-significant */
/*    by OPSCAN.  The first entry of each such pair serves as a synonym */
/*    for the second, more fundamental keyword. */

/*       The ordered search stops after the section of the dictionary */
/*    having the same first letters as the key has been checked, or */
/*    after a specified number of entries have been examined.  A special */
/*    dictionary entry, the vertical bar '|', will also terminate the */
/*    search.  This will speed things up if an appropriate dictionary */
/*    length parameter cannot be determined.  Both types of search are */
/*    sequential.  See "Notes" below for some suggestions if efficiency */
/*    is an issue. */


/* Parameters: */

/*    Name    Dimension  Type  I/O/S  Description */
/*    NDICT               I    I      Number of dictionary entries to be */
/*                                    examined. */
/*    DICTRY  NDICT       C    I      Array of dictionary entries, */
/*                                    left-justified in their fields. */
/*                                    May be alphabetized for efficiency, */
/*                                    in which case ALPHA should be */
/*                                    .TRUE.  Entries with synonyms are */
/*                                    of the form */
/*                                    'ENTRY : SYNONYM', where 'SYNONYM' */
/*                                    is a more fundamental entry in the */
/*                                    same dictionary.  NOTE: Don't build */
/*                                    "circular" dictionaries! */
/*    ALPHA               L    I      Indicates whether the dictionary */
/*                                    is in alphabetical order, in which */
/*                                    case the search can be terminated */
/*                                    sooner. */
/*    KEY                 C    I/O    String to be compared against the */
/*                                    dictionary.  Abbreviations are OK */
/*                                    if they correspond to a unique */
/*                                    entry in the dictionary.  KEY is */
/*                                    replaced on termination by its most */
/*                                    fundamental equivalent dictionary */
/*                                    entry (uppercase, left-justified) */
/*                                    if a match was found. */
/*    ENTRY               I      O    Dictionary pointer.  If > 0, it */
/*                                    indicates which entry matched KEY. */
/*                                    In case of trouble, a negative */
/*                                    value means that a UNIQUE match */
/*                                    was not found - the absolute value */
/*                                    of ENTRY points to the second */
/*                                    dictionary entry that matched KEY. */
/*                                    Zero means that NO match could be */
/*                                    found.  ENTRY always refers to the */
/*                                    last search performed - */
/*                                    in searching a chain of synonyms, */
/*                                    a non-positive value will be */
/*                                    returned if there is any break, */
/*                                    even if the original input key */
/*                                    was found. */


/* External references: */

/*    Name    Description */
/*    OPSCAN  Finds first and last significant characters. */


/* Environment:  Digital VAX-11/780 VMS FORTRAN (FORTRAN 77). */
/*               Appears to satisfy the ANSI Fortran 77 standard. */


/* Notes: */

/*    (1)  IMPLICIT NONE is non-standard.  (Has been commented out.) */

/*    (2)  We have assumed that the dictionary is not too big.  If */
/*         many searches are to be done or if the dictionary has more */
/*         than a dozen or so entries, it may be advantageous to build */
/*         an index array of pointers to the beginning of the section */
/*         of the dictionary containing each letter, then pass in the */
/*         portion of the dictionary beginning with DICTRY (INDEX). */
/*         (This won't generally work for dictionaries with synonyms.) */
/*         For very large problems, a completely different approach may */
/*         be advisable, e.g. a binary search for ordered dictionaries. */

/*    (3)  OPLOOK is case sensitive.  In most applications it will be */
/*         necessary to use an uppercase dictionary, and to convert the */
/*         input key to uppercase before calling OPLOOK.  Companion */
/*         routines OPTOKN and PAIRS, available from the author, already */
/*         take care of this. */

/*    (4)  The key need not be left-justified.  Any leading (or */
/*         trailing) characters which are "non-significant" to OPSCAN */
/*         will be ignored.  These include blanks, horizontal tabs, */
/*         commas, colons, and equal signs.  See OPSCAN for details. */

/*    (5)  The ASCII collating sequence for character data is assumed. */
/*         (N.B. This means the numerals precede the alphabet, unlike */
/*         common practice!)  This should not cause trouble on EBCDIC */
/*         machines if DICTRY just contains alphabetic keywords. */
/*         Otherwise it may be necessary to use the FORTRAN lexical */
/*         library routines to force use of the ASCII sequence. */

/*    (6)  Parameter NUMSIG sets a limit on the length of significant */
/*         dictionary entries.  Special applications may require that */
/*         this be increased.  (It is 16 in the present version.) */

/*    (7)  No protection against "circular" dictionaries is provided: */
/*         don't claim that A is B, and that B is A.  All synonym chains */
/*         must terminate!  Other potential errors not checked for */
/*         include duplicate or mis-ordered entries. */

/*    (8)  The handling of ambiguities introduces some ambiguity: */

/*            ALPHA = .TRUE.  A potential problem, when one entry */
/*                            looks like an abbreviation for another */
/*                            (eg. does 'A' match 'A' or 'AB'?) was */
/*                            resolved by dropping out of the search */
/*                            immediately when an "exact" match is found. */

/*            ALPHA = .FALSE. The programmer must ensure that the above */
/*                            situation does not arise: each dictionary */
/*                            entry must be recognizable, at least when */
/*                            specified to full length.  Otherwise, the */
/*                            result of a search will depend on the */
/*                            order of entries. */


/* Author:  Robert Kennelly, Informatics General Corporation. */


/* Development history: */

/*    24 Feb. 1984  RAK/DAS  Initial design and coding. */
/*    25 Feb. 1984    RAK    Combined the two searches by suitable */
/*                           choice of terminator FLAG. */
/*    28 Feb. 1984    RAK    Optional synonyms in dictionary, no */
/*                           longer update KEY. */
/*    29 Mar. 1984    RAK    Put back replacement of KEY by its */
/*                           corresponding entry. */
/*    21 June 1984    RAK    Corrected bug in error handling for cases */
/*                           where no match was found. */
/*    23 Apr. 1985    RAK    Introduced test for exact matches, which */
/*                           permits use of dictionary entries which */
/*                           would appear to be ambiguous (for ordered */
/*                           case).  Return -I to point to the entry */
/*                           which appeared ambiguous (had been -1). */
/*                           Repaired loop termination - had to use */
/*                           equal length strings or risk quitting too */
/*                           soon when one entry is an abbreviation */
/*                           for another.  Eliminated HIT, reduced */
/*                           NUMSIG to 16. */
/*    15 Nov. 1985    MAS    Loop 20 now tests .LT. FLAG, not .LE. FLAG. */
/*                           If ALPHA is false, FLAG is now '|', not '{'. */
/*    26 Jan. 1986    PEG    Declaration of FLAG and TARGET modified to */
/*                           conform to ANSI-77 standard. */
/* ----------------------------------------------------------------------- */
/*     Variable declarations. */
/*     ---------------------- */
/*     IMPLICIT NONE */
/*     Parameters. */
/*     Variables. */
/*     CHARACTER */
/*    $   DICTRY (NDICT) * (*), FLAG * (NUMSIG), */
/*    $   KEY * (*), TARGET * (NUMSIG) */
/*     Procedures. */
/*     Executable statements. */
/*     ---------------------- */
    /* Parameter adjustments */
    dictry -= dictry_len;

    /* Function Body */
    *entry__ = 0;
/*     Isolate the significant portion of the input key (if any). */
    first = 1;
/* Computing MIN */
    i__1 = i_len(key, key_len);
    last = min(i__1,16);
    opscan_(key, &first, &last, &mark, key_len);
    if (mark > 0) {
	s_copy(target, key + (first - 1), (ftnlen)16, mark - (first - 1));
/*        Look up TARGET in the dictionary. */
L10:
	length = mark - first + 1;
/*           Select search strategy by cunning choice of termination test */
/*           flag.  The vertical bar is just about last in both the */
/*           ASCII and EBCDIC collating sequences. */
	if (*alpha) {
	    s_copy(flag__, target, (ftnlen)16, (ftnlen)16);
	} else {
	    s_copy(flag__, "|", (ftnlen)16, (ftnlen)1);
	}
/*           Perform search. */
/*           --------------- */
	i__ = 0;
L20:
	++i__;
	if (s_cmp(target, dictry + i__ * dictry_len, length, length) == 0) {
	    if (*entry__ == 0) {
/*                    First "hit" - must still guard against ambiguities */
/*                    by searching until we've gone beyond the key */
/*                    (ordered dictionary) or until the end-of-dictionary */
/*                    mark is reached (exhaustive search). */
		*entry__ = i__;
/*                    Special handling if match is exact - terminate */
/*                    search.  We thus avoid confusion if one dictionary */
/*                    entry looks like an abbreviation of another. */
/*                    This fix won't generally work for un-ordered */
/*                    dictionaries! */
		first = 1;
		last = 16;
		opscan_(dictry + *entry__ * dictry_len, &first, &last, &mark, 
			dictry_len);
		if (mark == length) {
		    i__ = *ndict;
		}
	    } else {
/*                    Oops - two hits!  Abnormal termination. */
/*                    --------------------------------------- */
		*entry__ = -i__;
		return 0;
	    }
	}
/*           Check whether we've gone past the appropriate section of the */
/*           dictionary.  The test on the index provides insurance and an */
/*           optional means for limiting the extent of the search. */
	if (s_cmp(dictry + i__ * dictry_len, flag__, length, (ftnlen)16) < 0 
		&& i__ < *ndict) {
	    goto L20;
	}
/*           Check for a synonym. */
/*           -------------------- */
	if (*entry__ > 0) {
/*              Look for a second entry "behind" the first entry.  FIRST */
/*              and MARK were determined above when the hit was detected. */
	    first = mark + 2;
	    opscan_(dictry + *entry__ * dictry_len, &first, &last, &mark, 
		    dictry_len);
	    if (mark > 0) {
/*                 Re-set target and dictionary pointer, then repeat the */
/*                 search for the synonym instead of the original key. */
		s_copy(target, dictry + (*entry__ * dictry_len + (first - 1)),
			 (ftnlen)16, mark - (first - 1));
		*entry__ = 0;
		goto L10;
	    }
	}
    }
    if (*entry__ > 0) {
	s_copy(key, dictry + *entry__ * dictry_len, key_len, dictry_len);
    }
/*     Normal termination. */
/*     ------------------- */
    return 0;
/*     End of OPLOOK */
} /* oplook_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
logical opnumb_(char *string, ftnlen string_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    char atom[1];
    integer nexp, j, nplus, ndigit, length;
    logical number;
    integer npoint, nminus;

/* *********************************************************************** */
/*     Description and usage: */

/*        A simple(-minded) test for numeric data is implemented by */
/*        searching an input string for legitimate characters: */
/*                digits 0 to 9, D, E, -, + and . */
/*        Insurance is provided by requiring that a numeric string */
/*        have at least one digit, at most one D, E or . */
/*        and at most two -s or +s.  Note that a few ambiguities remain: */

/*           (a)  A string might have the form of numeric data but be */
/*                intended as text.  No general test can hope to detect */
/*                such cases. */

/*           (b)  There is no check for correctness of the data format. */
/*                For example a meaningless string such as 'E1.+2-' */
/*                will be accepted as numeric. */

/*        Despite these weaknesses, the method should work in the */
/*        majority of cases. */


/*     Parameters: */

/*        Name    Dimension  Type  I/O/S  Description */
/*        OPNUMB              L      O    Set .TRUE. if STRING appears */
/*                                        to be numerical data. */
/*        STRING              C    I      Input data to be tested. */


/*     Environment:  ANSI FORTRAN 77. */


/*     Notes: */

/*        (1)  It is assumed that STRING is a token extracted by */
/*             OPTOKN, which will have converted any lower-case */
/*             characters to upper-case. */

/*        (2)  OPTOKN pads STRING with blanks, so that a genuine */
/*             number is of the form  '1234        '. */
/*             Hence, the scan of STRING stops at the first blank. */

/*        (3)  COMPLEX data with parentheses will not look numeric. */


/*     Systems Optimization Laboratory, Stanford University. */
/*     12 Nov  1985    Initial design and coding, starting from the */
/*                     routine ALPHA from Informatics General, Inc. */
/* *********************************************************************** */
    ndigit = 0;
    nexp = 0;
    nminus = 0;
    nplus = 0;
    npoint = 0;
    number = TRUE_;
    length = i_len(string, string_len);
    j = 0;
L10:
    ++j;
    *(unsigned char *)atom = *(unsigned char *)&string[j - 1];
    if (*(unsigned char *)atom >= '0' && *(unsigned char *)atom <= '9') {
	++ndigit;
    } else if (*(unsigned char *)atom == 'D' || *(unsigned char *)atom == 'E')
	     {
	++nexp;
    } else if (*(unsigned char *)atom == '-') {
	++nminus;
    } else if (*(unsigned char *)atom == '+') {
	++nplus;
    } else if (*(unsigned char *)atom == '.') {
	++npoint;
    } else if (*(unsigned char *)atom == ' ') {
	j = length;
    } else {
	number = FALSE_;
    }
    if (number && j < length) {
	goto L10;
    }
    ret_val = number && ndigit >= 1 && nexp <= 1 && nminus <= 2 && nplus <= 2 
	    && npoint <= 1;
    return ret_val;
/*     End of OPNUMB */
} /* opnumb_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* Subroutine */ int opscan_(char *string, integer *first, integer *last, 
	integer *mark, ftnlen string_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    char ch__1[1];

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer begin;
    char ht[1];
    integer length, end;



/* Description and usage: */

/*       Looks for non-blank fields ("tokens") in a string, where the */
/*    fields are of arbitrary length, separated by blanks, tabs, commas, */
/*    colons, or equal signs.  The position of the end of the 1st token */
/*    is also returned, so this routine may be conveniently used within */
/*    a loop to process an entire line of text. */

/*       The procedure examines a substring, STRING (FIRST : LAST), which */
/*    may of course be the entire string (in which case just call OPSCAN */
/*    with FIRST <= 1 and LAST >= LEN (STRING) ).  The indices returned */
/*    are relative to STRING itself, not the substring. */


/* Parameters: */

/*    Name    Dimension  Type  I/O/S  Description */
/*    STRING              C    I      Text string containing data to be */
/*                                    scanned. */
/*    FIRST               I    I/O    Index of beginning of substring. */
/*                                    If <= 1, the search begins with 1. */
/*                                    Output is index of beginning of */
/*                                    first non-blank field, or 0 if no */
/*                                    token was found. */
/*    LAST                I    I/O    Index of end of substring. */
/*                                    If >= LEN (STRING), the search */
/*                                    begins with LEN (STRING).  Output */
/*                                    is index of end of last non-blank */
/*                                    field, or 0 if no token was found. */
/*    MARK                I      O    Points to end of first non-blank */
/*                                    field in the specified substring. */
/*                                    Set to 0 if no token was found. */


/* Environment:  Digital VAX-11/780 VMS FORTRAN (FORTRAN 77). */
/*               ANSI Fortran 77, except for the tab character HT. */

/* Notes: */

/*    (1)  IMPLICIT NONE is non-standard.  Constant HT (Tab) is defined */
/*         in a non-standard way:  the CHAR function is not permitted */
/*         in a PARAMETER declaration (OK on VAX, though).  For Absoft */
/*         FORTRAN 77 on 68000 machines, use HT = 9.  In other cases, it */
/*         may be best to declare HT as a variable and assign */
/*         HT = CHAR(9) on ASCII machines, or CHAR(5) for EBCDIC. */

/*    (2)  The pseudo-recursive structure was chosen for fun.  It is */
/*         equivalent to three DO loops with embedded GO TOs in sequence. */

/*    (3)  The variety of separators recognized limits the usefulness of */
/*         this routine somewhat.  The intent is to facilitate handling */
/*         such tokens as keywords or numerical values.  In other */
/*         applications, it may be necessary for ALL printing characters */
/*         to be significant.  A simple modification to statement */
/*         function SOLID will do the trick. */


/* Author:  Robert Kennelly, Informatics General Corporation. */


/* Development history: */

/*    29 Dec. 1984    RAK    Initial design and coding, (very) loosely */
/*                           based on SCAN_STRING by Ralph Carmichael. */
/*    25 Feb. 1984    RAK    Added ':' and '=' to list of separators. */
/*    16 Apr. 1985    RAK    Defined SOLID in terms of variable DUMMY */
/*                           (previous re-use of STRING was ambiguous). */

/* ----------------------------------------------------------------------- */
/*     Variable declarations. */
/*     ---------------------- */
/*     IMPLICIT NONE */
/*     Parameters. */
/*     Variables. */
/*     Statement functions. */
/*     Executable statements. */
/*     ---------------------- */
/* ***  HT     = CHAR(9) for ASCII machines, CHAR(5) for EBCDIC. */
    *(unsigned char *)ht = '\t';
    *mark = 0;
    length = i_len(string, string_len);
    begin = max(*first,1);
    end = min(length,*last);
/*     Find the first significant character ... */
    i__1 = end;
    for (*first = begin; *first <= i__1; ++(*first)) {
	*(unsigned char *)&ch__1[0] = *(unsigned char *)&string[*first - 1];
	if (*(unsigned char *)&ch__1[0] != ' ' && *(unsigned char *)&ch__1[0] 
		!= ':' && *(unsigned char *)&ch__1[0] != ',' && *(unsigned 
		char *)&ch__1[0] != '=' && *(unsigned char *)&ch__1[0] != *(
		unsigned char *)ht) {
/*           ... then the end of the first token ... */
	    i__2 = end - 1;
	    for (*mark = *first; *mark <= i__2; ++(*mark)) {
		i__3 = *mark;
		s_copy(ch__1, string + i__3, (ftnlen)1, *mark + 1 - i__3);
		if (! (*(unsigned char *)&ch__1[0] != ' ' && *(unsigned char *
			)&ch__1[0] != ':' && *(unsigned char *)&ch__1[0] != 
			',' && *(unsigned char *)&ch__1[0] != '=' && *(
			unsigned char *)&ch__1[0] != *(unsigned char *)ht)) {
/*                 ... and finally the last significant character. */
		    i__3 = *mark;
		    for (*last = end; *last >= i__3; --(*last)) {
			*(unsigned char *)&ch__1[0] = *(unsigned char *)&
				string[*last - 1];
			if (*(unsigned char *)&ch__1[0] != ' ' && *(unsigned 
				char *)&ch__1[0] != ':' && *(unsigned char *)&
				ch__1[0] != ',' && *(unsigned char *)&ch__1[0]
				 != '=' && *(unsigned char *)&ch__1[0] != *(
				unsigned char *)ht) {
			    return 0;
			}
/* L10: */
		    }
/*                 Everything past the first token was a separator. */
		    ++(*last);
		    return 0;
		}
/* L20: */
	    }
/*           There was nothing past the first token. */
	    *last = *mark;
	    return 0;
	}
/* L30: */
    }
/*     Whoops - the entire substring STRING (BEGIN : END) was composed of */
/*     separators ! */
    *first = 0;
    *mark = 0;
    *last = 0;
    return 0;
/*     End of OPSCAN */
} /* opscan_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* Subroutine */ int optokn_(char *string, integer *number, char *list, 
	ftnlen string_len, ftnlen list_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer mark, last, i__, first, count;
    extern /* Subroutine */ int opscan_(char *, integer *, integer *, integer 
	    *, ftnlen), opuppr_(char *, ftnlen);



/* Description and usage: */

/*       An aid to parsing input data.  The individual "tokens" in a */
/*    character string are isolated, converted to uppercase, and stored */
/*    in an array.  Here, a token is a group of significant, contiguous */
/*    characters.  The following are NON-significant, and hence may */
/*    serve as separators:  blanks, horizontal tabs, commas, colons, */
/*    and equal signs.  See OPSCAN for details.  Processing continues */
/*    until the requested number of tokens have been found or the end */
/*    of the input string is reached. */


/* Parameters: */

/*    Name    Dimension  Type  I/O/S  Description */
/*    STRING              C    I      Input string to be analyzed. */
/*    NUMBER              I    I/O    Number of tokens requested (input) */
/*                                    and found (output). */
/*    LIST    NUMBER      C      O    Array of tokens, changed to upper */
/*                                    case. */


/* External references: */

/*    Name    Description */
/*    OPSCAN  Finds positions of first and last significant characters. */
/*    OPUPPR  Converts a string to uppercase. */


/* Environment:  Digital VAX-11/780 VMS FORTRAN (FORTRAN 77). */
/*               Appears to satisfy the ANSI Fortran 77 standard. */


/* Notes: */

/*    (1)  IMPLICIT NONE is non-standard.  (Has been commented out.) */


/* Author:  Robert Kennelly, Informatics General Corporation. */


/* Development history: */

/*    16 Jan. 1984    RAK    Initial design and coding. */
/*    16 Mar. 1984    RAK    Revised header to reflect full list of */
/*                           separators, repaired faulty WHILE clause */
/*                           in "10" loop. */
/*    18 Sep. 1984    RAK    Change elements of LIST to uppercase one */
/*                           at a time, leaving STRING unchanged. */

/* ----------------------------------------------------------------------- */
/*     Variable declarations. */
/*     ---------------------- */
/*     IMPLICIT NONE */
/*     Parameters. */
/*     Variables. */
/*     Procedures. */
/*     Executable statements. */
/*     ---------------------- */
/*     WHILE there are tokens to find, loop UNTIL enough have been found. */
    /* Parameter adjustments */
    list -= list_len;

    /* Function Body */
    first = 1;
    last = i_len(string, string_len);
    count = 0;
L10:
/*        Get delimiting indices of next token, if any. */
    opscan_(string, &first, &last, &mark, string_len);
    if (last > 0) {
	++count;
/*           Pass token to output string array, then change case. */
	s_copy(list + count * list_len, string + (first - 1), list_len, mark 
		- (first - 1));
	opuppr_(list + count * list_len, list_len);
	first = mark + 2;
	if (count < *number) {
	    goto L10;
	}
    }
/*     Fill the rest of LIST with blanks and set NUMBER for output. */
    i__1 = *number;
    for (i__ = count + 1; i__ <= i__1; ++i__) {
	s_copy(list + i__ * list_len, " ", list_len, (ftnlen)1);
/* L20: */
    }
    *number = count;
/*     Termination. */
/*     ------------ */
    return 0;
/*     End of OPTOKN */
} /* optokn_ */

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* Subroutine */ int opuppr_(char *string, ftnlen string_len)
{
    /* Initialized data */

    static char low[26] = "abcdefghijklmnopqrstuvwxyz";
    static char upp[26] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char c__[1];
    integer i__, j;


/* ACRONYM:  UPper CASE */

/* PURPOSE:  This subroutine changes all lower case letters in the */
/*           character string to upper case. */

/* METHOD:   Each character in STRING is treated in turn.  The intrinsic */
/*           function INDEX effectively allows a table lookup, with */
/*           the local strings LOW and UPP acting as two tables. */
/*           This method avoids the use of CHAR and ICHAR, which appear */
/*           be different on ASCII and EBCDIC machines. */

/* ARGUMENTS */
/*    ARG       DIM     TYPE I/O/S DESCRIPTION */
/*  STRING       *       C   I/O   Character string possibly containing */
/*                                 some lower-case letters on input; */
/*                                 strictly upper-case letters on output */
/*                                 with no change to any non-alphabetic */
/*                                 characters. */

/* EXTERNAL REFERENCES: */
/*  LEN    - Returns the declared length of a CHARACTER variable. */
/*  INDEX  - Returns the position of second string within first. */

/* ENVIRONMENT:  ANSI FORTRAN 77 */

/* DEVELOPMENT HISTORY: */
/*     DATE  INITIALS  DESCRIPTION */
/*   06/28/83   CLH    Initial design. */
/*   01/03/84   RAK    Eliminated NCHAR input. */
/*   06/14/84   RAK    Used integer PARAMETERs in comparison. */
/*   04/21/85   RAK    Eliminated DO/END DO in favor of standard code. */
/*   09/10/85   MAS    Eliminated CHAR,ICHAR in favor of LOW, UPP, INDEX. */

/* AUTHOR: Charles Hooper, Informatics General, Palo Alto, CA. */

/* ----------------------------------------------------------------------- */
    i__1 = i_len(string, string_len);
    for (j = 1; j <= i__1; ++j) {
	*(unsigned char *)c__ = *(unsigned char *)&string[j - 1];
	if (*(unsigned char *)c__ >= 'a' && *(unsigned char *)c__ <= 'z') {
	    i__ = i_indx(low, c__, (ftnlen)26, (ftnlen)1);
	    if (i__ > 0) {
		*(unsigned char *)&string[j - 1] = *(unsigned char *)&upp[i__ 
			- 1];
	    }
	}
/* L10: */
    }
    return 0;
/*     End of OPUPPR */
} /* opuppr_ */


%{
/*{{{ first part of declarations */
#include "bibtex.h" 
char		yytext[BIBYYLMAX];
// #define YYDEBUG		1		/* need for -d option support */
#define YYERROR_VERBOSE 1  /* better warning messages */
#define YYSTYPE		SEXP    /* semantic values */
#define streql(s, t)	(!strcmp((s), (t)))

/**
 * Aliases to R standard PROTECT macro, in case we want to do 
 * something else as well
 */
#define _PROTECT_WITH_INDEX(s,index) { PROTECT_WITH_INDEX(s,index);  } 
#define _PROTECT(s) { PROTECT(s); }
#define _UNPROTECT(n) { UNPROTECT(n);  }
#define _UNPROTECT_PTR(s) { UNPROTECT_PTR(s);  }
#define _REPROTECT(s,index) REPROTECT(s,index)

/**
 * Set to 1 when a syntax error was seen to indicate that 
 * tokens supplied by the lexer should not be wrapped into SEXP
 */
static int recovering ;                             

/**
 * Set to 1 after a syntax error was seen, and before the 
 * recovering process has started
 */
static int popping ; 

/**
 * used in the popping mechanism, the error message is compared to this
 * and the popping stops if anything else happens
 */ 
const char* error_msg_popping = "Error: popping";

/** 
 * The keyname of the current entry (used in warning messages)
 */
static char* currentKey = 0 ; 

/**
 * the line number where the current entry starts (used in warning messages)
 */
int currentKeyLine ;

static SEXP srcfile;
char * bibfile ; 

/** 
 * this is defined as a macro, so that it has access to yylval to be able
 * to unprotect it. 
 * 
 * the macro sets the "popping" to 1 to indicate that symbols being 
 * destructed should be UNPROTECT'ed as well, and calls the _yyerror 
 * function which sends an R warning with the problem
 */
#define yyerror(s) \
do { \
	_UNPROTECT_PTR( yylval ) ; \
	popping = 1; \
	_yyerror(s); \
} \
while(0) ;

extern YYLTYPE yylloc ;

# define YYLLOC_DEFAULT(Current, Rhs, N)				\
	do	{ 								\
		if (YYID (N)){								\
		  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
		  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
		  (Current).first_byte   = YYRHSLOC (Rhs, 1).first_byte;	\
		  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
		  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
		  (Current).last_byte    = YYRHSLOC (Rhs, N).last_byte;		\
		} else	{								\
		  (Current).first_line   = (Current).last_line   =		\
		    YYRHSLOC (Rhs, 0).last_line;				\
		  (Current).first_column = (Current).last_column =		\
		    YYRHSLOC (Rhs, 0).last_column;				\
		  (Current).first_byte   = (Current).last_byte =		\
		    YYRHSLOC (Rhs, 0).last_byte;				\
		} \
	} while (YYID (0))

// #define XXDEBUG 1

/* functions used in the parsing process */
SEXP makeSrcRef(YYLTYPE) ;
static SEXP xx_object_list_1(SEXP);
static SEXP xx_object_list_2(SEXP,SEXP);
static SEXP xx_object(SEXP);
static SEXP xx_atobject_comment(SEXP);
static SEXP xx_atobject_entry(SEXP, YYLTYPE);
static SEXP xx_atobject_include(SEXP);
static SEXP xx_atobject_preamble(SEXP);
static SEXP xx_atobject_string(SEXP);
static SEXP xx_token_entry( SEXP, SEXP);
static SEXP xx_token_entry_empty(SEXP) ;
static SEXP xx_entry_head( SEXP, SEXP) ;
static SEXP xx_entry_head_nokey( SEXP ) ;
static SEXP xx_keyname_key( SEXP) ;
static SEXP xx_keyname_abbrev( SEXP) ;
static SEXP xx_include( SEXP ) ;
static SEXP xx_preamble(SEXP) ;
static SEXP xx_string(SEXP) ;
static SEXP xx_value( SEXP, SEXP) ;
static SEXP xx_assignement_list1(SEXP);
static SEXP xx_assignement_list2(SEXP, SEXP);
static SEXP xx_assignement(SEXP, SEXP);
static SEXP xx_lhs_field( SEXP ) ;
static SEXP xx_lhs_abbrev( SEXP );
static SEXP xx_space( SEXP ); 
static SEXP xx_space_inline( SEXP ) ;
static SEXP xx_space_newline( SEXP ) ;
static SEXP xx_forward( SEXP ) ;
static SEXP xx_null( ) ;
static SEXP xx_expand_abbrev( SEXP ) ;
static SEXP xx_simple_value( SEXP ) ;

/* functions to unprotect one or more SEXP */
void junk1( SEXP); 
void junk2( SEXP, SEXP); 
void junk3( SEXP, SEXP, SEXP); 
void junk4( SEXP, SEXP, SEXP, SEXP); 
void junk5( SEXP, SEXP, SEXP, SEXP, SEXP); 
void junk6( SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 
void junk7( SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

static PROTECT_INDEX INCLUDE_INDEX ;
static PROTECT_INDEX COMMENT_INDEX ;
static PROTECT_INDEX STRING_INDEX ;
static PROTECT_INDEX PREAMBLE_INDEX ;
static PROTECT_INDEX ENTRIES_INDEX ;

static SEXP includes; 
static SEXP comments; 
static SEXP strings; 
static SEXP preamble; 
static SEXP entries; 

static void recordInclude( SEXP ) ;
static void recordComment( SEXP ) ;
static void recordString( SEXP ) ;
static void recordPreamble( SEXP ) ;
static SEXP asVector( SEXP, int ); 
/*}}}*/

/*{{{ Grammar */
%}

%token TOKEN_ABBREV	
%token TOKEN_AT	
%token TOKEN_COMMA
%token TOKEN_COMMENT
%token TOKEN_ENTRY
%token TOKEN_EQUALS	
%token TOKEN_FIELD
%token TOKEN_INCLUDE
%token TOKEN_INLINE	
%token TOKEN_KEY
%token TOKEN_LBRACE
%token TOKEN_LITERAL
%token TOKEN_NEWLINE
%token TOKEN_PREAMBLE
%token TOKEN_RBRACE	
%token TOKEN_SHARP
%token TOKEN_SPACE
%token TOKEN_STRING
%token TOKEN_VALUE
%token TOKEN_UNKNOWN

%nonassoc TOKEN_EQUALS
%left TOKEN_SPACE TOKEN_INLINE TOKEN_NEWLINE
%left TOKEN_SHARP

%destructor { 
	
	/* 
	this handles UNPROTECTING SEXP that are popped when a syntax 
	error is detected. When a syntax error is detected, 
	the following happens : 
		- yyerror is called which sets recovering and popping to 1
		- some symbols are "popped" from the semantic 
			value stack using this destructor, 
			this is: as many symbols as it takes to be back to this rule: 
			
			| error TOKEN_RBRACE
			
			then "popping" is set to 0 so that no more symbols are 
			UNPROTECTED
			
			then, the lexer provides as many tokens as necessary to 
			present the TOKEN_RBRACE token, however tokens are not 
			converted to SEXP because recovering is 1
			
			finally, when TOKEN_RBRACE is seen, recovering is set to 0
			to indicate that tokens should now be converted to SEXP 
			again
			
			The issue is that all symbols (terminals and non terminals
			have to be listed in this destructor. (There probably is 
			a better way)
	*/
	
	if( popping ){
		if( streql( error_msg_popping, yymsg ) ){
			UNPROTECT_PTR( $$ ) ;
		} else{
			popping = 0; 
		}
	}
} opt_space space single_space assignment assignment_list assignment_lhs value simple_value include preamble entry entry_head string key_name comment TOKEN_ABBREV	 TOKEN_AT	 TOKEN_COMMA TOKEN_COMMENT TOKEN_ENTRY TOKEN_EQUALS	 TOKEN_FIELD TOKEN_INCLUDE TOKEN_INLINE	 TOKEN_KEY TOKEN_LBRACE TOKEN_LITERAL TOKEN_NEWLINE TOKEN_PREAMBLE TOKEN_RBRACE	 TOKEN_SHARP TOKEN_SPACE TOKEN_STRING TOKEN_VALUE TOKEN_UNKNOWN 

%% 
file:		  opt_space 					{ junk1($1); YYACCEPT ; }
		| opt_space object_list opt_space { junk3($1, $2, $3) ; YYACCEPT ; }
		;

object_list: object 						{ $$ = xx_object_list_1($1);  }
		| object_list opt_space object 	{ $$ = xx_object_list_2($1,$3); junk1($2) ; }
		;

object:	TOKEN_AT opt_space at_object {
			$$ = xx_object($3); 
			junk2($1,$2); 
		}
		| anything opt_space object {
			/* this eats whatever is between two entries, lexing until 
				a TOKEN_AT is found */
			$$ = xx_forward($3); junk2($1,$2) ; 
		} 
	;

anything:  TOKEN_ABBREV    { $$ = xx_forward( $1) ; }	
		|  TOKEN_COMMA     { $$ = xx_forward( $1) ; }
		|  TOKEN_COMMENT   { $$ = xx_forward( $1) ; }
		|  TOKEN_ENTRY     { $$ = xx_forward( $1) ; }
		|  TOKEN_EQUALS	   { $$ = xx_forward( $1) ; }
		|  TOKEN_FIELD     { $$ = xx_forward( $1) ; }
		|  TOKEN_INCLUDE   { $$ = xx_forward( $1) ; }
		|  TOKEN_INLINE	   { $$ = xx_forward( $1) ; }
		|  TOKEN_KEY       { $$ = xx_forward( $1) ; }
		|  TOKEN_LBRACE    { $$ = xx_forward( $1) ; }
		|  TOKEN_LITERAL   { $$ = xx_forward( $1) ; }
		|  TOKEN_NEWLINE   { $$ = xx_forward( $1) ; }
		|  TOKEN_PREAMBLE   { $$ = xx_forward( $1) ; }
		|  TOKEN_RBRACE	   { $$ = xx_forward( $1) ; }
		|  TOKEN_SHARP     { $$ = xx_forward( $1) ; }
		|  TOKEN_SPACE     { $$ = xx_forward( $1) ; }
		|  TOKEN_STRING    { $$ = xx_forward( $1) ; }
		|  TOKEN_VALUE     { $$ = xx_forward( $1) ; }
		|  TOKEN_UNKNOWN   { $$ = xx_forward( $1) ; }

at_object:	  comment 						{ $$ = xx_atobject_comment($1); }
		| entry 							{ $$ = xx_atobject_entry($1, @$);}
		| include 							{ $$ = xx_atobject_include($1);}
		| preamble 							{ $$ = xx_atobject_preamble($1);}
		| string							{ $$ = xx_atobject_string($1);}
		| error TOKEN_RBRACE 				{ $$ = xx_null() ; YYUSE($2) ; recovering = 0; }
		;

comment:	  TOKEN_COMMENT opt_space TOKEN_LITERAL {junk2($1,$2); $$ = xx_forward($3); }
		;

entry:	  entry_head assignment_list TOKEN_RBRACE 						{ $$ = xx_token_entry( $1, $2); junk1($3); }
		| entry_head assignment_list TOKEN_COMMA opt_space TOKEN_RBRACE 	{ $$ = xx_token_entry( $1, $2); junk3($3,$4,$5); }
		| entry_head TOKEN_RBRACE 											{ $$ = xx_token_entry_empty($1) ; junk1($2) ; }
		;

entry_head:	  TOKEN_ENTRY opt_space TOKEN_LBRACE opt_space key_name opt_space TOKEN_COMMA opt_space { $$ = xx_entry_head( $1, $5) ; junk6($2,$3,$4,$6,$7,$8) ; }
		| TOKEN_ENTRY opt_space TOKEN_LBRACE opt_space TOKEN_COMMA opt_space { $$ = xx_entry_head_nokey( $1) ; junk5($2,$3,$4,$5,$6) ; }
		;

key_name:	  TOKEN_KEY 	{ $$ = xx_keyname_key( $1) ;}
		| TOKEN_ABBREV 		{ $$ = xx_keyname_abbrev( $1) ; }
		;

include:	  TOKEN_INCLUDE opt_space TOKEN_LITERAL { $$ = xx_include( $3 ) ; junk2($1,$2) ; }
		;

preamble:	  TOKEN_PREAMBLE opt_space TOKEN_LBRACE opt_space value opt_space TOKEN_RBRACE { $$ = xx_preamble($5) ; junk6($1,$2,$3,$4,$6,$7) ; }
		;

string:		  TOKEN_STRING opt_space TOKEN_LBRACE opt_space assignment opt_space TOKEN_RBRACE { $$ = xx_string($5) ; junk6( $1, $2, $3, $4, $6, $7) ; }
		;

value:	  	  simple_value {$$ = xx_forward($1) ; }
		| value opt_space TOKEN_SHARP opt_space simple_value { $$ = xx_value( $1, $5) ; junk3( $2, $3, $4);  }
		;

simple_value:	  TOKEN_VALUE { $$ = xx_simple_value($1); }
		| TOKEN_ABBREV { $$ = xx_expand_abbrev($1); }
		;

assignment_list:  assignment 									{ $$ = xx_assignement_list1($1); }
		| assignment_list TOKEN_COMMA opt_space assignment 	{ $$ = xx_assignement_list2($1, $4); junk2($2,$3); }
		;

assignment:	  assignment_lhs opt_space TOKEN_EQUALS opt_space value opt_space {  $$ = xx_assignement($1, $5); junk4($2, $3, $4, $6); }
		;

assignment_lhs:	  TOKEN_FIELD 	{ $$ = xx_lhs_field( $1 ) ; }
		| TOKEN_ABBREV 			{ $$ = xx_lhs_abbrev( $1 ); }
		;

opt_space:							{ $$ = xx_null() ; }
		| space						{ $$ = xx_forward($1) ;}
		;

space:	single_space				{ $$ = xx_forward($1) ;}
		| space single_space		{ $$ = xx_forward($1); junk1($2) ; } 
		;

single_space:	  TOKEN_SPACE		{ $$ = xx_space( $1 ) ; }
		| TOKEN_INLINE				{ $$ = xx_space_inline( $1 ) ; }
		| TOKEN_NEWLINE				{ $$ = xx_space_newline( $1 ) ; }
		;
%%

/*}}} end of grammar */


/*{{{ functions borrowed from gram.y */
#ifdef Win32
static char * fixmode(const char *mode){
    /* Rconnection can have a mode of 4 chars plus a null; we might
     * add one char */
    static char fixedmode[6];
    fixedmode[4] = '\0';
    strncpy(fixedmode, mode, 4);
    if (!strpbrk(fixedmode, "bt")) {
		strcat(fixedmode, "t");
    }
    return fixedmode;
}

#else
#define fixmode(mode) (mode)
#endif

FILE * _fopen(const char *filename, const char *mode){
    return(filename ? fopen(filename, fixmode(mode)) : NULL );
}
/*}}}*/

/*{{{ yyerror */
void _yyerror(const char *s){
	warning( "\n%s:%d:%d\n\t%s\n\tDropping the entry `%s` (starting at line %d) ", 
		bibfile, line_number, col_number, s, currentKey, currentKeyLine ) ;
	
	/* indicates that we are recovering from an error */
	recovering = 1 ;
}
/*}}}*/

/*{{{ yywarning */
static void yywarning(const char *s){
	warning( "\n%s:%d:%d\n\t%s", bibfile, line_number, col_number, s ) ;
}
/*}}}*/

/*{{{ R interface */
/**
 * .Internal( "do_read_bib", file = file )
 */
SEXP do_read_bib(SEXP args) {
	SEXP filename = CADR(args) ;
	const char* fname = CHAR(STRING_ELT(filename,0) ) ;
	bibfile = (char*)fname ;
	
	const char* encoding = CHAR(STRING_ELT( CADDR(args), 0 ) ); 
	known_to_be_latin1 = known_to_be_utf8 = FALSE;
	if(streql(encoding, "latin1")) {
		known_to_be_latin1 = TRUE;
	} else if(streql(encoding, "UTF-8"))  {
		known_to_be_utf8 = TRUE;
	} else if(!streql( encoding, "unknown") ){
		warning( "encoding '%s' will be ignored", encoding ) ;
	}
	
	srcfile = CADDDR(args);
	
	FILE* fp ;
	if((fp = _fopen(R_ExpandFileName( fname ), "r")) == NULL){
		error( "unable to open file to read", 0);
	}
	yyset_in( fp ) ; /* so that the lexer reads from the file */
#if YYDEBUG
	yydebug = 0 ;    /* setting this to 1 gives a lot of messages */
#endif
	popping = 0; 
	line_number = 1;
	col_number = 0; 
	byte_number = 0; 
	/* set up the data */
	_PROTECT_WITH_INDEX( includes = NewList() , &INCLUDE_INDEX ) ;
	_PROTECT_WITH_INDEX( comments = NewList() , &COMMENT_INDEX ) ;
	_PROTECT_WITH_INDEX( strings  = NewList() , &STRING_INDEX ) ;
	_PROTECT_WITH_INDEX( preamble = NewList() , &PREAMBLE_INDEX ) ;
	_PROTECT_WITH_INDEX( entries  = NewList() , &ENTRIES_INDEX ) ;
	
	/* call the parser */
	recovering = 0; 
	/* int res = yyparse() ; */
	yyparse() ;
	
	/* structure the data */
	SEXP ans; 
	if( isNull( CDR(entries) ) ){
		PROTECT( ans = allocVector( VECSXP, 0)  ) ;
	} else {
		PROTECT( ans = CDR(entries) )  ;
	}
	SEXP obj ;
	_PROTECT(obj = asVector( comments, 0 ) ); setAttrib( ans , install("comment") , obj ); _UNPROTECT_PTR( obj ) ;
	_PROTECT(obj = asVector( includes, 0 ) ); setAttrib( ans , install("include") , obj ); _UNPROTECT_PTR( obj ) ; 
	_PROTECT(obj = asVector( strings , 1 ) ); setAttrib( ans , install("strings") , obj ); _UNPROTECT_PTR( obj ) ; 
	_PROTECT(obj = asVector( preamble, 0 ) ); setAttrib( ans , install("preamble"), obj ); _UNPROTECT_PTR( obj ) ;
	_UNPROTECT_PTR( entries ) ;
	_UNPROTECT_PTR( ans );

	fclose(fp);
	
	return ans ;
}
/*}}}*/

/*{{{ xx_* parser helpers */
 
/** 
 * Object list with one object
 * 
 * @param object the object
 */
static SEXP xx_object_list_1(SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_object_list_1>\n" ) ;
#endif
	SEXP ans, tmp ;
	_PROTECT( tmp = NewList() ) ;
	if( object == R_NilValue) {
		_PROTECT( ans = tmp ) ;
	} else{
		_PROTECT( ans = GrowList( tmp, object) ) ;
	}
	_UNPROTECT_PTR( tmp) ;
	_UNPROTECT_PTR( object) ;
#ifdef XXDEBUG
	Rprintf( "</xx_object_list_1>\n" ) ;
#endif
	return ans ;
}

/** 
 * Adds an object to an object list
 * 
 * @param 
 */
static SEXP xx_object_list_2(SEXP list, SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_object_list_2>\n" ) ;
#endif
	SEXP ans ;
	if( object == R_NilValue ){
		_PROTECT( ans = list );
	} else{
		_PROTECT( ans = GrowList( list, object ) );
	}
	_UNPROTECT_PTR( object ) ;
	_UNPROTECT_PTR( list ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_object_list_2>\n" ) ;
#endif
	return ans ; 
}

/** 
 * recognizes an object, adds it to the list of entries
 *
 * @param object object
 */
static SEXP xx_object(SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_aobject>\n" ) ;
#endif
	SEXP ans; 
	_PROTECT( ans = object ) ;
	_UNPROTECT_PTR( object ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_aobject>\n" ) ;
#endif

	return ans ;
}

/**
 * Comment object
 * 
 * @param object
 */
static SEXP xx_atobject_comment(SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_atobject_comment>\n" ) ;
#endif
	SEXP ans; 
	_PROTECT( ans = R_NilValue ) ; 
	recordComment( object ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_atobject_comment>\n" ) ;
#endif
	return ans ; 
}

/**
 * Entry object
 *
 * @param object the entry object
 */
static SEXP xx_atobject_entry(SEXP object, YYLTYPE loc){
#ifdef XXDEBUG
	Rprintf( "<xx_atobject_entry>\n" ) ;
#endif
	SEXP ans, head, o, names; 
	_PROTECT( head = getAttrib( object, install("head") ) ); 
	int n = length( object ) ;
	_PROTECT( ans   = allocVector( STRSXP, n) ) ;
	_PROTECT( names = allocVector( STRSXP, n) ) ;
	
	_PROTECT( o = object ) ; 
	int i;
	for( i=0; i<n; i++){
		SET_STRING_ELT( ans  , i, STRING_ELT(CAR(o),0) ) ;
		SET_STRING_ELT( names, i, STRING_ELT(getAttrib(CAR(o), install("names")),0) ) ;
		o = CDR(o ) ;
	}
	_UNPROTECT(1); // o
	
	SEXP entry ;
	_PROTECT( entry = allocVector( STRSXP, 1 ) ) ;
	SET_STRING_ELT( entry  , 0, STRING_ELT(head, 1) ) ;
	
	SEXP h ;
	_PROTECT( h = allocVector( STRSXP, 1 ) ) ;
	SET_STRING_ELT( h  , 0, STRING_ELT(head, 0) ) ;
	
	setAttrib( ans, install( "entry"), entry ) ;
	setAttrib( ans, install( "names"), names ) ;
	setAttrib( ans, install( "key"), h ) ;
	
	_UNPROTECT( 2 ) ; // entry, h, o
	_UNPROTECT_PTR( object ); 
	_UNPROTECT_PTR( names ); 
	_UNPROTECT_PTR( head ) ;
	
	SEXP res; 
	_PROTECT( res = GrowList( entries , ans ) ) ;
	_REPROTECT( entries = res , ENTRIES_INDEX ) ;
	_UNPROTECT_PTR( res ) ;
	
	SEXP srcref ; 
	_PROTECT( srcref = makeSrcRef( loc ) );
	setAttrib( ans, install( "srcref"), srcref );
	_UNPROTECT( 1) ; // srcref
	
	
#ifdef XXDEBUG
	Rprintf( "</xx_atobject_entry>\n" ) ;
#endif
	_UNPROTECT_PTR( ans ) ;
	_PROTECT( ans = R_NilValue ); 
	return ans ;
}

/** 
 * Include object
 *
 * @param object the include object
 */
static SEXP xx_atobject_include(SEXP object ){
#ifdef XXDEBUG
	Rprintf( "<xx_atobject_include>\n" ) ;
#endif
	SEXP ans ;
	_PROTECT( ans = R_NilValue ) ;
	recordInclude( object ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_atobject_include>\n" ) ;
#endif
	return ans ;
}

/** 
 * A preamble object
 */ 
static SEXP xx_atobject_preamble(SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_atobject_preamble>\n" ) ;
#endif
	SEXP ans ;
	_PROTECT( ans = R_NilValue ) ;
	recordPreamble( object ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_atobject_preamble>\n" ) ;
#endif
	return ans ;
}

/** 
 * string object
 */ 
static SEXP xx_atobject_string(SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_atobject_string>\n" ) ;
#endif
	SEXP ans ;
	_PROTECT( ans = R_NilValue ) ;
	recordString( object ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_atobject_string>\n" ) ;
#endif
	return ans ; 
}

/** 
 * actual entry object
 *
 * @param head the entry head
 * @param list the assignement list
 */ 
static SEXP xx_token_entry( SEXP head, SEXP list){
#ifdef XXDEBUG
	Rprintf( "<xx_token_entry>\n" ) ;
#endif
	SEXP data ;
	_PROTECT( data = CDR(list) )  ;
	setAttrib( data, install("head"), head) ;
	_UNPROTECT_PTR( list ) ;
	_UNPROTECT_PTR( head ) ;
	
#ifdef XXDEBUG
	Rprintf( "</xx_token_entry>\n" ) ;
#endif
	return data; 
}

/** 
 * actual entry object with no assignements
 *
 * @param head
 */
static SEXP xx_token_entry_empty(SEXP head){
#ifdef XXDEBUG
	Rprintf( "<xx_token_entry_empty>\n" ) ;
#endif
	SEXP ans; 
	_PROTECT( ans = R_NilValue ) ;
	setAttrib( ans, install("head"), head) ;
	_UNPROTECT_PTR( head ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_token_entry_empty>\n" ) ;
#endif
	return ans; 
}

/** 
 * entry head
 *
 * @param kind kind of entry
 * @param keyname the name of the entry
 */
static SEXP xx_entry_head( SEXP kind, SEXP keyname ){
#ifdef XXDEBUG
	Rprintf( "<xx_entry_head>\n" ) ;
#endif
	SEXP ans ;
	_PROTECT( ans = allocVector( STRSXP, 2) ) ;
	SET_STRING_ELT( ans, 0, STRING_ELT(keyname, 0) ) ;
	SET_STRING_ELT( ans, 1, STRING_ELT(kind, 0) ) ;
	_UNPROTECT_PTR(kind) ;
	_UNPROTECT_PTR(keyname) ;
	
#ifdef XXDEBUG
	Rprintf( "</xx_entry_head>\n" ) ;
#endif
	return ans ;
}  
   
/** 
 * entry head
 *
 * @param kind kind of entry
 */
static SEXP xx_entry_head_nokey( SEXP kind){
#ifdef XXDEBUG
	Rprintf( "<xx_entry_head>\n" ) ;
#endif
	SEXP ans ;
	_PROTECT( ans = allocVector( STRSXP, 2) ) ;
	SET_STRING_ELT( ans, 0, NA_STRING ) ;
	SET_STRING_ELT( ans, 1, STRING_ELT(kind, 0) ) ;
	_UNPROTECT_PTR(kind) ;
	warning( "\n%s:%d:%d\n\tno key for the entry at line %d", 
			bibfile, line_number, col_number, currentKeyLine ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_entry_head>\n" ) ;
#endif
	return ans ;
}  

/** 
 * sets the currentKey as the first element of the 'key' parameter
 * and returns currentKey
 */
char* set_current_key( SEXP key ){
	// free currentKey if needed 
	if( currentKey ) free( currentKey ) ;
	
	// grab the first element of key
	currentKey = ( length( key ) > 0 ) ? strdup( CHAR( STRING_ELT(key,0) ) ) : 0 ;
	return currentKey ;
}

/** 
 * name of an entry
 *
 * @param key keyname
 */
static SEXP xx_keyname_key( SEXP key){
#ifdef XXDEBUG
	Rprintf( "<xx_keyname_key/>\n" ) ;
#endif
	currentKey = set_current_key( key ) ;
	currentKeyLine = line_number ;
	return key; 
}

/** 
 * name of an entry
 */ 
static SEXP xx_keyname_abbrev( SEXP abbrev){
	SEXP res =  xx_expand_abbrev( abbrev ) ;
	currentKey = set_current_key( abbrev ) ; 
	currentKeyLine = line_number ;
	return res; 
}

/**
 * "include" object
 *
 * @param object
 */ 
static SEXP xx_include( SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_include/>\n" ) ;
#endif
	return object ;
}

/** 
 * preamble object
 *
 * @param object preamble object
 */
static SEXP xx_preamble(SEXP value){
#ifdef XXDEBUG
	Rprintf( "<xx_preamble/>\n" ) ;
#endif
	return value; 
}

/**
 * string object
 *
 * @param object the assignement
 */
static SEXP xx_string(SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_string/>\n" ) ;
#endif
	return object ;
}

/** 
 * value, paste( left, right , sep = "" )
 *
 * @param 
 */
static SEXP xx_value( SEXP left , SEXP right ){
#ifdef XXDEBUG
	Rprintf( "<xx_value>\n" ) ;
#endif
	SEXP ans;
	const char* left_ = CHAR( STRING_ELT( left, 0) ) ;
	const char* right_ = CHAR( STRING_ELT( right, 0) ) ;
	int n_left = strlen( left_);
	int n_right = strlen( right_);
	int n = n_left + n_right ;
	char res[n] ;
	int i, j;
	for( i=0; i<n_left; i++){
		res[i] = left_[i] ;
	}
	for( j=0; j<n_right; j++, i++){
		res[i] = right_[j] ;
	}
	
	_PROTECT( ans = allocVector( STRSXP, 1) ) ;
	SET_STRING_ELT( ans, 0, STRING_ELT( mkString2( res, n_left + n_right ), 0) ) ;
	_UNPROTECT_PTR( right ) ; 
	_UNPROTECT_PTR( left ) ; 
#ifdef XXDEBUG
	Rprintf( "</xx_value>\n" ) ;
#endif
	return ans ;
}

/** 
 * creates a new assignement list
 *
 * @param object assignement object
 */
static SEXP xx_assignement_list1(SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_assignement_list1>\n" ) ;
#endif
	SEXP ans, tmp; 
	_PROTECT( tmp = NewList( ) ) ;
	_PROTECT( ans = GrowList( tmp, object) ) ;
	_UNPROTECT_PTR( tmp ) ;
	_UNPROTECT_PTR( object ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_assignement_list1>\n" ) ;
#endif
	return ans ;
}

/**
 * Augments an assignement list with a new assignement
 *
 * @param list assignement list
 * @param object new assignement
 */
static SEXP xx_assignement_list2(SEXP list, SEXP object){
#ifdef XXDEBUG
	Rprintf( "<xx_assignement_list2>\n" ) ;
#endif
	SEXP ans ;
	_PROTECT( ans = GrowList( list, object) ) ;
	_UNPROTECT_PTR( list ) ;
	_UNPROTECT_PTR( object ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_assignement_list2>\n" ) ;
#endif
	return ans; 
}

/**
 * assignement                
 *
 * @param lhs left side
 * @param value value
 */
static SEXP xx_assignement(SEXP lhs, SEXP value){
#ifdef XXDEBUG
	Rprintf( "<xx_assignement>\n" ) ;
#endif
	SEXP ans;
	_PROTECT( ans = value ) ;
	setAttrib( ans, install("names"), lhs ) ;
	_UNPROTECT_PTR( lhs ) ;
	_UNPROTECT_PTR( value ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_assignement>\n" ) ;
#endif
	return ans; 
}

/**
 * field lhs
 *
 * @param field 
 */
static SEXP xx_lhs_field( SEXP field){
#ifdef XXDEBUG
	Rprintf( "<xx_lhs_field/>\n" ) ;
#endif
	return field ;
}

static SEXP xx_lhs_abbrev( SEXP abbrev){
	return xx_expand_abbrev( abbrev ) ;
}


static SEXP xx_space( SEXP s ){
#ifdef XXDEBUG
	Rprintf( "<xx_space/>\n" ) ;
#endif
	return s ;
}

static SEXP xx_space_inline( SEXP s ){
#ifdef XXDEBUG
	Rprintf( "<xx_space_inline/>\n" ) ;
#endif
	return s ;
}

static SEXP xx_space_newline( SEXP s ){
#ifdef XXDEBUG
	Rprintf( "<xx_space_newline/>\n" ) ;
#endif
	return s ;
}

static SEXP xx_forward( SEXP s ){
#ifdef XXDEBUG
	Rprintf( "<xx_forward/>\n" ) ;
#endif
	return s ;
}

static SEXP xx_simple_value( SEXP s ){
#ifdef XXDEBUG
	Rprintf( "<xx_simple_value>\n" ) ;
#endif
	SEXP ans;
	/* all of what follows simply removes the " from the start and the end
	  of the value, it might be better to do this in the lexer instead */
	const char* data = CHAR( STRING_ELT( s, 0 ) ); 
	int n = strlen(data);
	if( n >= 2) {
		char first = data[0]; 
		char last = data[n-1];
		if( first == '"' && last == '"' ){
			char noquote[n-2] ;
			for( int i=1; i<n-1; i++){
				noquote[i-1] = data[i] ;
			}
			_PROTECT( ans = allocVector( STRSXP, 1 ) ); 
			SET_STRING_ELT( ans, 0, STRING_ELT(mkString2(noquote, n-2), 0) ) ;
		} else{
			_PROTECT( ans = s ) ;
		}
	} else{
		_PROTECT( ans = s ) ;
	}
	
	_UNPROTECT_PTR( s ) ;
#ifdef XXDEBUG
	Rprintf( "</xx_simple_value>\n" ) ;
#endif
	return ans ;
}



static SEXP xx_null( ){
#ifdef XXDEBUG
	Rprintf( "<xx_null>\n" ) ;
#endif
	SEXP ans; 
	_PROTECT( ans = R_NilValue ); 
#ifdef XXDEBUG
	Rprintf( "</xx_null>\n" ) ;
#endif
	return ans;
}

/*}}}*/

/*{{{ various record functions */
static void recordInclude( SEXP object ){
	SEXP tmp ;
	_PROTECT( tmp = GrowList( includes, object ) ); 
	_REPROTECT( includes = tmp, INCLUDE_INDEX ) ;
	_UNPROTECT_PTR( tmp ) ;
	_UNPROTECT_PTR( object ) ;
}

static void recordComment( SEXP object ){
	SEXP tmp ;
	_PROTECT( tmp = GrowList( comments, object ) ); 
	_REPROTECT( comments = tmp, COMMENT_INDEX ) ;
	_UNPROTECT_PTR( tmp ) ;
	_UNPROTECT_PTR( object ) ;
}
static void recordString( SEXP object ){
	SEXP tmp ;
	_PROTECT( tmp = GrowList( strings, object ) ); 
	_REPROTECT( strings = tmp, STRING_INDEX ) ;
	_UNPROTECT_PTR( tmp ) ;
	_UNPROTECT_PTR( object ) ;
}
static void recordPreamble( SEXP object ){
	SEXP tmp ;
	_PROTECT( tmp = GrowList( preamble, object ) ); 
	_REPROTECT( preamble = tmp, PREAMBLE_INDEX ) ;
	_UNPROTECT_PTR( tmp ) ;
	_UNPROTECT_PTR( object ) ;
}

static SEXP xx_expand_abbrev( SEXP abbrev ){
	SEXP ans, tmp ;
	/* use the abbreviation name by default */
	_PROTECT( ans = allocVector( STRSXP, 1 ) ) ;
	SET_STRING_ELT( ans, 0, STRING_ELT( abbrev, 0) ) ;
	
	_PROTECT( tmp = CDR(strings) ) ;
	int n = length( tmp ) ;
	const char * target = CHAR( STRING_ELT( abbrev, 0) ) ;
	SEXP item ;
	for(int i=0; i<n; i++){
		item = CAR(tmp);
		if( streql( CHAR( STRING_ELT( getAttrib( item, install("names") ), 0 ) ) , target ) ){
				SET_STRING_ELT( ans, 0, STRING_ELT( item, 0) ) ;
				break ;
		};
		tmp = CDR( tmp ) ;
	}
	_UNPROTECT(1); // tmp
	_UNPROTECT_PTR( abbrev ) ;
	return ans ;
}

/*}}}*/
                  
/*{{{ setToken */
/** 
 * Called from the lexer to make a SEXP of the token
 *
 * @param token the token we make a SEXP from
 * @param len number of characters of the token
 */
void setToken( const char* token, int len ){
	if( recovering ){
#ifdef XXDEBUG
		Rprintf( "recovering (%d): %s\n", recovering, token ) ;
#endif
		recovering++; 
	} else{
		_PROTECT( yylval = mkString2(token,  len) ) ;
		yylloc.first_line   = start_line_number ;
		yylloc.first_column = start_col_number ;
		yylloc.first_byte   = start_byte_number ;
		yylloc.last_line    = line_number ;
		yylloc.last_column  = col_number ;
		yylloc.last_byte    = byte_number ;
	}
}

/** 
 * utility to make a STRSXP
 *
 * @param s character string
 * @param len number of characters
 */
SEXP mkString2(const char *s, int len){
    SEXP t;
    cetype_t enc = CE_NATIVE;
    _PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, len, enc));
    _UNPROTECT_PTR(t);
    return t;
}
/*}}}*/

/*{{{ junks */
void junk1( SEXP s){
#ifdef XXDEBUG
	Rprintf( " *~\n" ) ; 
#endif
	_UNPROTECT_PTR( s ) ; 
}

void junk2( SEXP s1, SEXP s2){
	junk1(s1);
	junk1(s2); 
}

void junk3( SEXP s1, SEXP s2, SEXP s3){
	junk1(s1);
	junk1(s2);
	junk1(s3);
}

void junk4( SEXP s1, SEXP s2, SEXP s3, SEXP s4){
	junk1(s1);
	junk1(s2);
	junk1(s3);
	junk1(s4);
}


void junk5( SEXP s1, SEXP s2, SEXP s3, SEXP s4, SEXP s5){
	junk1(s1);
	junk1(s2);
	junk1(s3);
	junk1(s4);
	junk1(s5);
}

void junk6( SEXP s1, SEXP s2, SEXP s3, SEXP s4, SEXP s5, SEXP s6){
	junk1(s1);
	junk1(s2);
	junk1(s3);
	junk1(s4);
	junk1(s5);
	junk1(s6);
}

void junk7( SEXP s1, SEXP s2, SEXP s3, SEXP s4, SEXP s5, SEXP s6, SEXP s7){
	junk1(s1);
	junk1(s2);
	junk1(s3);
	junk1(s4);
	junk1(s5);
	junk1(s6);
	junk1(s7);
}
/*}}}*/

/*{{{ asVector */
/**
 * list( a = "aa", b = "bb") -> c( a = "aa", b = "bb" ) 
 */
static SEXP asVector( SEXP x, int donames){
	SEXP ans, names = R_NilValue ; 
	SEXP tmp ;
	int n = length( CDR(x) ) ;
	_PROTECT( ans   = allocVector( STRSXP, n) ) ;
	if( donames ){
		_PROTECT( names = allocVector( STRSXP, n) ) ;
	}
	SEXP item; 
	_PROTECT( tmp = CDR( x ) );
	for( int i=0; i<n; i++){
		item = CAR(tmp); 
		SET_STRING_ELT( ans  , i, STRING_ELT(item, 0) ) ;
		if( donames){
			SET_STRING_ELT( names, i, STRING_ELT( getAttrib(item, install("names") ), 0) ) ;
		}
		tmp = CDR(tmp);
	}
	_UNPROTECT(1) ; // tmp
	if( donames ){
		setAttrib( ans, install("names"), names ) ;
		_UNPROTECT_PTR(names) ;
	}
	_UNPROTECT_PTR(x) ; 
	_UNPROTECT_PTR(ans) ; 
	return ans; 
}
/*}}}*/

SEXP makeSrcRef(YYLTYPE loc){
	/* the '+ 1' here adjust the columns and bytes to 
look like the srcref class of R that does 
not work with offsets 
	*/
	SEXP ans; 
	_PROTECT( ans = allocVector( INTSXP, 6) ) ;
	INTEGER(ans)[0] = last_at_location.first_line; 
	INTEGER(ans)[1] = last_at_location.first_byte + 1; 
	INTEGER(ans)[2] = loc.last_line; 
	INTEGER(ans)[3] = loc.last_byte + 1; 
	INTEGER(ans)[4] = last_at_location.first_column + 1; 
	INTEGER(ans)[5] = loc.last_column + 1; 
	_UNPROTECT( 1) ;
	setAttrib( ans, install("srcfile"), srcfile ) ;
	setAttrib( ans, install("class"), mkString2( "srcref", 6 ) ) ;
	return ans ;
}

void dummy_bibparse(){
	yywarning( "" ) ;
}


/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */


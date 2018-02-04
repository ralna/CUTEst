/*********************************************************************/
/***    Copyright (c) Robert J. Vanderbei, 1994                    ***/
/***    All Rights Reserved                                        ***/
/*********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#ifndef GNU
#if !defined(__APPLE__)
#include <malloc.h>
#endif
#endif

#define MY_MALLOC_THRESH 1
#ifdef MALLOCDEBUG

int nmalloc=0, ncalloc=0, nrealloc=0, nfree=0;

#define	MALLOC(name,len,type) {	\
	nmalloc++; \
	if ( (len)*sizeof(type) >= MY_MALLOC_THRESH ) \
	    printf("%5d MALLOC(  %12s, %12s=%-6d," #type " )\n", \
	    nmalloc, #name, #len, len); \
	if ( ( (name) =	(type *)malloc( (len) * sizeof(type) ) ) == NULL \
	       && (len)>0) { \
	    printf( "MALLOC(" #name "," #len "=%d," #type ")" \
		  ": cannot allocate space", len); \
	    exit(1); \
	} \
}

#define	CALLOC(name,len,type) {	\
	int i; \
	ncalloc++; \
	if ( (len)*sizeof(type) >= MY_MALLOC_THRESH ) \
	    printf("%5d CALLOC(  %12s, %12s=%-6d," #type " )\n", \
	    ncalloc, #name, #len, len); \
	if ( ( (name) = (type *)calloc( (len) , sizeof(type) ) ) == NULL \
	       && (len)>0) { \
	    printf( "CALLOC(" #name "," #len "=%d," #type ")" \
	          ": cannot allocate space", len); \
	    exit(1); \
	} \
	for (i=0; i<(len); i++) name[i] = 0; \
}

#define	REALLOC(name,len,type) { \
	nrealloc++; \
	if ( (len)*sizeof(type) >= MY_MALLOC_THRESH ) \
	    printf("%5d REALLOC( %12s, %12s=%-6d," #type " )\n", \
	    nrealloc, #name, #len, len); \
	if (((name) = (type *)realloc( (name), (len)*sizeof(type) )) == NULL \
	       && (len)>0) { \
	    printf( "REALLOC(" #name "," #len "=%d," #type ")" \
		  ": cannot reallocate space", len); \
	    exit(11); \
	} \
}

#define	FREE(name) { \
	nfree++; \
	printf("%5d FREE(    %12s )\n", nfree, #name); \
	if ( (name) != NULL ) free( (name) ); \
	(name) = NULL; \
}

#else

#define	MALLOC(name,len,type) {	\
	if ( ( (name) = (type *)malloc( (len) * sizeof(type) ) ) == NULL \
		&& (len)>0) { \
	    printf( "MALLOC(" #name "," #len "=%d," #type ")" \
		  ": cannot allocate space", len); \
	    exit(1); \
	} \
}

#define	CALLOC(name,len,type) { \
	int i; \
	if ( ( (name) = (type *)calloc( (len) , sizeof(type) ) ) == NULL \
	        && (len)>0) { \
	    printf( "CALLOC(" #name "," #len "=%d," #type ")" \
		  ": cannot allocate space", len); \
	    exit(1); \
	} \
	for (i=0; i<(len); i++) name[i] = 0; \
}

#define	REALLOC(name,len,type) { \
	if (((name) = (type *)realloc( (name), (len)*sizeof(type) )) == NULL \
	       && (len)>0) { \
	    printf( "REALLOC(" #name "," #len "=%d," #type ")" \
		  ": cannot reallocate space", len); \
	    exit(1); \
	} \
}

#define	FREE(name) { \
	if ( (name) != NULL ) free( (name) ); \
	(name) = NULL; \
}

#endif

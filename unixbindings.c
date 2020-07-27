#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/compatibility.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

//#define _XOPEN_SOURCE
#define __USE_XOPEN
#include <time.h>
#include <string.h>

/* ============================================= */
/*                                               */
/* ============================================= */
value unixtime_of_string( value datetime_string, value format_string )
{

  CAMLparam2( datetime_string, format_string );
  CAMLlocal1( result );
  char     *rest;
  struct   tm timestruct;
  long int time;

  char* timestring = String_val(datetime_string);
  memset(&timestruct, 0, sizeof(struct tm));

  printf("Erster Parameter:  %s\n", String_val(datetime_string));
  printf("Zweiter Parameter: %s\n", String_val(format_string));

  rest = strptime(String_val(datetime_string), String_val(format_string), &timestruct);
  if( rest == NULL )
  {
    caml_failwith("Parsing error");
  }
  else if( strlen(rest) == 0 )
  {
    //fprintf(stderr, "Anything is fine!\n");
    ;
  }
  else
  {
    //fprintf(stderr, "Not completely parsed (but no error)!\n");
    ;
  }

  time = mktime(&timestruct);
  //printf("Zeitwert: %ld\n", time);


  CAMLreturn( Val_long(time) );
}

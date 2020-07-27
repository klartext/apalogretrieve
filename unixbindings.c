#include <caml/mlvalues.h>
#include <caml/compatibility.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>


/* ============================================= */
/*                                               */
/* ============================================= */
CAMLprim value unixtime_of_string( value name_arg )
{
  CAMLparam1( name_arg );
  CAMLlocal1( result );
  result = caml_alloc (1, 0);

  double time = 3.1415926;

  char* name = String_val(name_arg);




  //lept_retval = pixReadHeader(name, &pformat, &pw, &ph, &pbps, &pspp, &piscmap);

  /* store results in Caml-value! :-) */
  /* -------------------------------- */
  //Store_field (result, 0, Double_val(time));
  //Store_field (result, 1, Val_long(ph));
  //Double_val(time);


  CAMLreturn( result );
}

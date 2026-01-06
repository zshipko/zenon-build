(*
** Testing simple ATS2 build
*)

#define ATS_DYNLOADFLAG 0
#include "share/atspre_staload.hats"
#include "./message.sats"


implement
main0 () = {
  val () = println! ("Testing simple ATS2 build")
  val () = println! (message)
  val x = 10 + 20
  val () = println! ("10 + 20 = ", x)
}

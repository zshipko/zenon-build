(*
** Testing simple ATS2 build
*)

#include "share/atspre_staload.hats"

implement
main0 () = {
  val () = println! ("Testing simple ATS2 build")
  val () = println! ("Hello from ATS2!")
  val x = 10 + 20
  val () = println! ("10 + 20 = ", x)
}

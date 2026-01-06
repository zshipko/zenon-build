fun main () =
  let
    val _ = print "Testing simple SML build\n"
    val _ = print ("factorial 6 = " ^ Int.toString (factorial 6) ^ "\n")
  in
    OS.Process.success
  end

val _ = main ()

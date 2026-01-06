fun factorial 0 = 1
  | factorial n =
      n * factorial (n - 1)

fun main () =
  let
    val _ = print "Testing simple SML build\n"
    val _ = print ("factorial 6 = " ^ Int.toString (factorial 6) ^ "\n")
  in
    OS.Process.success
  end

val _ = main ()



val xs = for (i <- 0 until 5)
  yield for(j <- 0 until 5) yield Variable(s"x${i}${j}")
Alldiff(xs)
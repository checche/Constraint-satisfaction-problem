import Application.csp
import TestAdd.{vars, x1, x2, x3}

/*
 * 作成したプログラムのアプリケーションを記述するためのファイル
 */
object Application extends App {

  val xs = for (i <- 0 until 5)
    yield for(j <- 0 until 5) yield Variable(s"x${i}${j}")

  // val x1 = Variable(s"x${}")
  val dom1 = Domain(Seq(1,2,3,4,5))

  var ds:  Map[Variable, Domain] = Map.empty
  for(i <- 0 until 5; j <- 0 until 5) {
    ds += xs(i)(j) -> dom1
  }

  def alldiff(xs:Seq[Variable]) :Seq[Constraint] = {
    for (i <- 0 until xs.size; j <- i+1 until xs.size) yield Ne(xs(i), xs(j))
  }

  val cs1 = for (i <- 0 until 5) yield alldiff(xs(i))
  val cs2 = for(j <- 0 until 5) yield alldiff(for(i <- 0 until 5) yield xs(i)(j))


  val cs = cs1++cs2

  val csp = CSP(xs.flatten, ds, cs.flatten)


  csp.toSugar.foreach(println)
}


object Test9 extends App {

  val csp = cspFactory.fromFile("CspFiles/PLS03.csp")

  csp.toSugar.foreach(println)
}

object Test extends App {

  gt01

  // gt02

  // gt03

  // gt04

  // bt01

  def gt01 = {
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    val x3 = Variable("x3")

    val vars = Seq(x1,x2,x3)

    val doms =
      Map(
        x1 -> Domain(Seq(1, 2, 3)),
        x2 -> Domain(Seq(1, 2, 3)),
        x3 -> Domain(Seq(1, 2, 3)))

    val cons =
      Seq(
        Ne(x1, x2),
        Ne(x1, x3),
        Ne(x2, x3))

    val csp = new CSP(vars, doms, cons)

    val solver = new BT

    val solution = solver.solve(csp)


    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def gt02 = {
    val x1 = Variable("x1")
    val x2 = Variable("x2")
    val x3 = Variable("x3")

    val vars = Seq(x1, x2, x3)

    val doms =
      Seq(
        x1 -> Domain(Seq(1, 2)),
        x2 -> Domain(Seq(1, 2)),
        x3 -> Domain(Seq(1, 2))).toMap

    val cons =
      Seq(
        Ne(x1, x2),
        Ne(x1, x3),
        Ne(x2, x3))

    val csp = new CSP(vars, doms, cons)

    val solver = new GTsol

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }
  }

  def gt03 = {
    val csp = cspFactory.fromFile("CspFiles/PLS03.csp")

    val solver = new GTsol

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def gt04 = {
    val csp = cspFactory.fromFile("CspFiles/PLS04.csp")

    val solver = new GTsol

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }
  def bt01 = {
    val csp = cspFactory.fromFile("CspFiles/NQueen04.csp")

    val solver = new BT

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }
}

object TestAlldiff extends App {
  var n = 3

  val xs = for (i <- 0 until n)
    yield for(j <- 0 until n) yield Variable(s"x${i}${j}")

  val dom1 = Domain(Range(1 , n+1))

  var ds:  Map[Variable, Domain] = Map.empty
  for(i <- 0 until n; j <- 0 until n) {
    ds += xs(i)(j) -> dom1
  }


  val cs1 = for (i <- 0 until n) yield Alldifferent(xs(i))
  val cs2 = for(j <- 0 until n) yield Alldifferent(for(i <- 0 until n) yield xs(i)(j))


  val cs = cs1++cs2

  val csp = CSP(xs.flatten, ds, cs)

  csp.toSugar.foreach(println)

  val solver = new GTsol

  val solution = solver.solve(csp)


  if (solution.nonEmpty) {
    println("s SAT")
    println(solution.get)
  } else {
    println("s UNSAT")
  }

}

object TestAdd extends App {
  val x1 = Variable("x1")
  val x2 = Variable("x2")
  val x3 = Variable("x3")

  val vars = Seq(x1,x2,x3)

  val doms =
    Map(
      x1 -> Domain(Seq(1, 2, 3)),
      x2 -> Domain(Seq(1, 2, 3)),
      x3 -> Domain(Seq(1, 2, 3)))

  val cons =
    Seq(
      Ne(x1, x2),
      Ne(x1, Add(Seq(x2,x3))),
      Gt(Add(Seq(x1, x2, x3)), Num(-4))
    )


  val csp = new CSP(vars, doms, cons)


  csp.toSugar.foreach(println)

  val solver = new BT

  val solution = solver.solve(csp)


  if (solution.nonEmpty) {
    println("s SAT")
    println(solution.get)
  } else {
    println("s UNSAT")
  }

}


object TestMul extends App {
  val x1 = Variable("x1")
  val x2 = Variable("x2")
  val x3 = Variable("x3")

  val vars = Seq(x1,x2,x3)

  val doms =
    Map(
      x1 -> Domain(Seq(1, 2, 3)),
      x2 -> Domain(Seq(1, 2, 3)),
      x3 -> Domain(Seq(1, 2, 3)))

  val cons =
    Seq(
      Alldifferent(Seq(Mul(x1,x2), Mul(x2,x3), Mul(x3, x1)))
    )


  val csp = new CSP(vars, doms, cons)


  csp.toSugar.foreach(println)

  val solver = new BT

  val solution = solver.solve(csp)


  if (solution.nonEmpty) {
    println("s SAT")
    println(solution.get)
  } else {
    println("s UNSAT")
  }

}

object plspSolver {
  def main(args: Array[String]): Unit = {

    val id: String = "180x217x" // 学籍番号を書く

    val fileName = args(0)

    println(s"ID: $id")
    println(s"CSP: $fileName")

    val csp = cspFactory.fromFile(fileName)

    println("c Parse Done.")

    val solver: CspSolver = new BT // new "自分ソルバークラス" を書く
    val solution = solver.solve(csp)
    if (solution.nonEmpty) {
      println("s SAT")
      printAssignment(solution.get)
    } else {
      println("s UNSAT")
    }

  }

  def printAssignment(a: Assignment) = {
    a.amap.map { case (x, v) => s"v ${x.name} = $v" }.foreach(println)
  }
}

object TestCspfile extends App {
  bt01
  def bt01 = {
    val csp = cspFactory.fromFile("CSPFiles/Alldiff-NQueen04.csp")

    val solver = new BT

    val solution = solver.solve(csp)

    if (solution.nonEmpty) {
      println("s SAT")
      println(solution.get)
    } else {
      println("s UNSAT")
    }

  }
}

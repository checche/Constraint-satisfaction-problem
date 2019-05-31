/*
 * CSP ソルバーに関するクラスを定義するためのファイル
 */
abstract class CspSolver {
  def solve(csp: CSP): Option[Assignment]
}

class GTsol extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {
    def gt(xs: Seq[Variable], partialAssign: Assignment): Option[Assignment] = {

      if (xs.isEmpty) {
        print("Generate: ")
        println(partialAssign.amap.map{case (x,v) => s"${x.name} = $v"}.mkString(", "))
        print("Test: ")
        if (csp.isSatisfiedWith(partialAssign)) {
          println("OK\n")
          return Some(partialAssign)
        } else {
          println("NG\n")
          return None
        }
      }

      val x = xs.head
      for (v <- csp.doms(x).values) {
        println(s"${x.name} -> $v is added.")
        val sol = gt(xs.tail, partialAssign + (x -> v))
        if (sol.nonEmpty)
          return sol
      }
      None
    }

    gt(csp.vars, Assignment())
  }
}

class BT extends CspSolver {
  def solve(csp: CSP): Option[Assignment] = {

    def divideVariable(xs: Seq[Variable], dom:Map[Variable,Domain]): (Variable, Seq[Variable]) = {
      val m = xs.minBy(x => dom(x).size)
      val variableSet = xs.toSet
      val remain = (variableSet-m).toSeq
      return (m, remain)
    }

    def selectVariable(xs: Seq[Variable], dom: Map[Variable,Domain]): (Variable, Seq[Variable]) = divideVariable(xs, dom)
    def valueOrder(dom: Domain): Seq[Int] = dom.values

    def bt(xs: Seq[Variable], partialAssign: Assignment): Option[Assignment] = {

      if (xs.isEmpty)
        return Some(partialAssign)

      val (x, remain) = selectVariable(xs, csp.doms)
      for (v <- valueOrder(csp.doms(x))) {
        val partial = partialAssign + (x -> v) // 変数 x に値 v を新たに割当てる
        /* CSP の制約の中で現在値が割当たっている変数上のもの (検査が可能な制約) */
        val consToBeTested = csp.cons.filter(c => c.vars.forall(partial.contains))
        /* 検査が通れば次の値割当てを行う */
        if (consToBeTested.forall(_.isSatisfiedWith(partial))) {
          val sol = bt(remain, partial)
          if (sol.nonEmpty)
            return sol
        }
      }
      None
    }

    bt(csp.vars, Assignment())
  }
}

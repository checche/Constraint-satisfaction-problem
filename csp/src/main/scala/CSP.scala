
/*
 * 制約充足問題 (Constraint Satisfaction Problem; CSP) に関するクラスを定義するためのファイル
 */

abstract class Expression

/**
  *
  */
abstract class Term extends Expression {
  // Term に含まれている整数変数の集合を返す
  def vars: Set[Variable]
  // 値割当て a が与えられた時, Term の値 を返す.
  def valuedWith(a: Assignment): Int
}

// name: 変数名
case class Variable(name: String) extends Term {
  // Variable は, 自分自身を整数変数として持つので, Set(this) を返す
  def vars = Set(this)
  // 値割当て a が与えられた時, a が自分自身に割当てている値を返す．
  def valuedWith(a: Assignment) = a(this)
}



/* HPの図にはないが継承関係がある */
// values: ドメインに含まれる値
case class Domain(values: Seq[Int]) extends Expression {
  // ドメインの値の中で一番小さいものを返すメソッド
  def lb = values.min
  // ドメインの値の中で一番大きいを返すメソッド
  def ub = values.max
  // ドメインのサイズ (値の数) を返すメソッド
  def size = values.size
}

abstract class Constraint extends Expression {
  // 制約に含まれる整数変数の集合
  def vars: Set[Variable]

  // 与えられた値割当てによって制約が充足されるかどうかを返す
  def isSatisfiedWith(a: Assignment): Boolean
}

case class CSP(
                var vars: Seq[Variable],
                var doms:Map[Variable, Domain],
                var cons: Seq[Constraint]
              ) {
  // 空のドメインの値の存在 (CSP は即 UNSAT になる) を返すメソッド
  def hasNoEmptyDomain = doms.forall(_._2.size > 0)

  // 与えられた値割当てによってCSPが充足されるかどうかを返す
  def isSatisfiedWith(a: Assignment) = cons.forall(_.isSatisfiedWith(a))

  // 変数と制約のマップ
  lazy val var2cons = (for (x <- vars) yield x -> cons.filter(_.vars.contains(x))).toMap

  // CSP を Sugar CSP Description 形式の文字列に変換するメソッド
  def toSugar(t: Expression): String = t match {
    case x: Variable => s"(int ${x.name} ${toSugar(doms(x))})"
    case i: Num => s"${i.n}"
    case d: Domain => if (d.values.size == d.ub - d.lb + 1) s"${d.lb} ${d.ub}" else d.values.mkString(" ")
    case _ => toSugarInTerm(t)
  }

  // Term内を再帰的にToSugarするTerm内のVariableを(int x dom)という形にはしない
  def toSugarInTerm(t:Expression): String = t match {
    case x: Variable => x.name
    case i: Num => s"${i.n}"
    case Add(xs: Seq[Term]) => s"(add ${xs.map(i => toSugarInTerm(i)).mkString(""," ","")})"
    case Mul(x1: Term, x2: Term) => s"(mul ${toSugarInTerm(x1)} ${toSugarInTerm(x2)})"
    case Ne(x1: Term, x2: Term) => s"(ne ${toSugarInTerm(x1)} ${toSugarInTerm(x2)})"
    case Eq(x1: Term, x2: Term) => s"(eq ${toSugarInTerm(x1)} ${toSugarInTerm(x2)})"
    case Ge(x1: Term, x2: Term) => s"(ge ${toSugarInTerm(x1)} ${toSugarInTerm(x2)})"
    case Gt(x1: Term, x2: Term) => s"(gt ${toSugarInTerm(x1)} ${toSugarInTerm(x2)})"
    case Alldifferent(xs: Seq[Term]) => s"(alldiff ${xs.map(i => toSugarInTerm(i)).mkString(""," ","")})"
  }

  def toSugar: Seq[String] = {
    vars.map(toSugar(_)) ++ cons.map(toSugar(_))
  }
}

/* CSP()と定義すると自動で以下の処理を行ってくれるようになる。 */
// コンパニオンオブジェクト (とファクリメソッド) の実装
object CSP {
  def apply() = new CSP(Seq.empty, Map.empty, Seq.empty)
}

case class Assignment(amap: Map[Variable, Int]) {
  def apply(x: Variable) = amap(x)

  def contains(x: Variable) = amap.contains(x)

  def +(x: Variable, v: Int) = Assignment(amap + (x -> v))

  def +(xv: Tuple2[Variable, Int]) = Assignment(amap + (xv._1 -> xv._2))

  // Assignment を Domain に変換するメソッド
  def toDoms: Map[Variable, Domain] = amap.map { xv => xv._1 -> Domain(Seq(xv._2)) }

  override def toString = {
    amap.map{case (x, v) => s"v ${x.name} = $v"}.mkString("\n")
  }
}

// 空の Assignment インスタンスを生成するためのファクトリメソッドを実装する.
object Assignment {
  def apply(): Assignment = Assignment(Map.empty)
}


// n: 定数の値
case class Num(n: Int) extends Term {
  // Num は, 自分自身を整数定数として持つので何も返さない
  def vars = Set()
  // 値割当て a が与えられた時,自分自身の値を返す．
  def valuedWith(a: Assignment) = this.n
}

case class Add(ts: Seq[Term]) extends Term {
  def vars = ts.map(t => t.vars).toSet.flatten
  def valuedWith(a: Assignment): Int = ts.map(t => t.valuedWith(a)).sum
}

case class Mul(t1: Term, t2: Term) extends Term {
  def vars = t1.vars ++ t2.vars
  def valuedWith(a: Assignment): Int = t1.valuedWith(a) * t2.valuedWith(a)
}

/**
  * x1とx2が異なる値を持つことを表す制約
  * @param t1
  * @param t2
  */
case class Ne(t1: Term, t2: Term) extends Constraint {
  def vars = t1.vars ++ t2.vars
  def isSatisfiedWith(a: Assignment) = t1.valuedWith(a) != t2.valuedWith(a)
}

case class Eq(t1: Term, t2: Term) extends Constraint {
  def vars = t1.vars ++ t2.vars
  def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) == t2.valuedWith(a)
}

case class Ge(t1: Term, t2: Term) extends Constraint {
  def vars = t1.vars ++ t2.vars
  def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) >= t2.valuedWith(a)
}

case class Gt(t1: Term, t2: Term) extends Constraint {
  def vars = t1.vars ++ t2.vars
  def isSatisfiedWith(a: Assignment): Boolean = t1.valuedWith(a) > t2.valuedWith(a)
}

case class Alldifferent(ts: Seq[Term]) extends Constraint {
  def vars: Set[Variable] = ts.map(t => t.vars).flatten.toSet
  def isSatisfiedWith(a:Assignment): Boolean = {
    for (i <- 0 until ts.size; j <- i + 1 until ts.size){
      if(!Ne(ts(i), ts(j)).isSatisfiedWith(a)) return false
    }
    return true
  }
}
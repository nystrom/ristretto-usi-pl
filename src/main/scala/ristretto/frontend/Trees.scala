package ristretto.frontend

// Extractors for Ristretto ASTs.
object Trees {
  import ristretto.frontend.RistrettoSyntax._

  object Arith {
    def unapply(e: Exp): Option[(Exp, Exp)] = e match {
      case Add(e1, e2) => Some((e1, e2))
      case Sub(e1, e2) => Some((e1, e2))
      case Mul(e1, e2) => Some((e1, e2))
      case Div(e1, e2) => Some((e1, e2))
      case Mod(e1, e2) => Some((e1, e2))
      case _ => None
    }
  }
  object Equality {
    def unapply(e: Exp): Option[(Exp, Exp)] = e match {
      case Eq(e1, e2) => Some((e1, e2))
      case Ne(e1, e2) => Some((e1, e2))
      case _ => None
    }
  }
  object Rel {
    def unapply(e: Exp): Option[(Exp, Exp)] = e match {
      case Lt(e1, e2) => Some((e1, e2))
      case Gt(e1, e2) => Some((e1, e2))
      case Le(e1, e2) => Some((e1, e2))
      case Ge(e1, e2) => Some((e1, e2))
      case _ => None
    }
  }
  object Bool {
    def unapply(e: Exp): Option[(Exp, Exp)] = e match {
      case And(e1, e2) => Some((e1, e2))
      case Or(e1, e2) => Some((e1, e2))
      case _ => None
    }
  }
  object BooleanLit {
    def unapply(e: Exp): Option[Boolean] = e match {
      case True() => Some(true)
      case False() => Some(false)
      case _ => None
    }
  }
}

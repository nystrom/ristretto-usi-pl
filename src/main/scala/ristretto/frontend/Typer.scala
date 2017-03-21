package ristretto.frontend

// Type checker for Ristretto ASTs.
object Typer {
  import ristretto.frontend.RistrettoSyntax._
  import ristretto.frontend.Trees._
  import ristretto.main.{Compiler => Errors}

  // Types. This extends the types in the concrete syntax with unknown and
  // function types.
  sealed trait Type
  case class IntType() extends Type {
    override def toString = "int"
  }
  case class BooleanType() extends Type {
    override def toString = "boolean"
  }
  case class VoidType() extends Type {
    override def toString = "void"
  }
  case class ArrayType(t: Type) extends Type {
    override def toString = t + "[]"
  }
  case class FunType(args: List[Type], ret: Type) extends Type {
    override def toString = args.mkString("(", ", ", ")") + " -> " + ret
  }
  case class UnknownType() extends Type {
    override def toString = "unknown"
  }

  def makeFunType(params: List[Param], ty: TyTree): Type = {
    FunType(params.map { case Param(tt, x) => convertTyTree(tt) }, convertTyTree(ty))
  }

  def makeEnv(dfns: List[Def]) = dfns.foldLeft(Map.empty: Env) {
    case (env, FunDef(ty, x, params, body)) =>
      env + (x -> makeFunType(params, ty))
    case (env, ExternDef(ty, x, params)) =>
      env + (x -> makeFunType(params, ty))
  }

  def typeCheck(r: Root): Unit = r match {
    case Root(dfns) =>
      val env = makeEnv(dfns)
      for (dfn <- dfns) {
        typeCheck(dfn)(env)
      }
  }

  def typeCheck(dfn: Def)(env: Env): Unit = dfn match {
    case FunDef(ty, x, params, body) =>
      // Introduce a fake variable named `return` to hold the return type.
      typeCheck(body)(env ++ (Param(ty, "return")::params).map {
        case Param(ty, x) => (x -> convertTyTree(ty))
      })
    case ExternDef(ty, x, params) =>
  }

  type Name = String
  type Env = Map[Name, Type]

  def typeCheck(e: Stm)(env: Env): Unit = e match {
    case Block(ss) =>
    case CallStm(e) =>
    case VarDefStm(t, x, e) =>
    case Assign(x, e) =>
    case ArrayAssign(a, is, e) =>
    case IfElse(cond, e1, e2) =>
    case IfThen(cond, e1) =>
    case While(cond, e) =>
    case Return(None) =>
    case Return(Some(e)) =>
  }

  def typeCheck(e: Exp)(env: Env): Type = e match {
    case IntLit(_) =>
      UnknownType()
    case BooleanLit(_) =>
      UnknownType()
    case StringLit(_) =>
      UnknownType()
    case Arith(e1, e2) =>
      UnknownType()
    case Equality(e1, e2) =>
      UnknownType()
    case Rel(e1, e2) =>
      UnknownType()
    case Bool(e1, e2) =>
      UnknownType()
    case Not(e) =>
      UnknownType()
    case Neg(e) =>
      UnknownType()
    case Var(x) =>
      UnknownType()
    case ArrayGet(a, is) =>
      UnknownType()
    case ArrayLength(e1) =>
      UnknownType()
    case NewArray(ty, es) =>
      UnknownType()
    case ArrayLit(e::Nil) =>
      UnknownType()
    case ArrayLit(e::es) =>
      UnknownType()
    case Call(f, es) =>
      UnknownType()
  }

  // Convert a type tree into a type.
  def convertTyTree(ty: TyTree): Type = ty match {
    case IntTyTree() => IntType()
    case BooleanTyTree() => BooleanType()
    case VoidTyTree() => VoidType()
    case ArrayTyTree(t) => ArrayType(convertTyTree(t))
  }

}

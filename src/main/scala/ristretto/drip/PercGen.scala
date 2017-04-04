package ristretto.drip

// Generate Drip from Ristretto ASTs
object PercGen {
  import ristretto.perc.{PercSyntax => P}
  import ristretto.drip.{DripSyntax => D}
  import ristretto.main.{Compiler => Errors}

  type Label = String
  type Temp = String

  type TempName = String
  type Name = String

  def translate(t: D.Root): P.Root = t match {
    case D.Root(dprocs) =>
      val procs = dprocs map {
        case D.Proc(f, params, body) =>
          val t = newTemp()
          P.Proc(f, params.map {
            case x => x
          }, translate(body, t) :+ P.Ret(P.Temp(t)))
      }
      P.Root(procs)
  }

  def translate(s: D.Stm): List[P.Stm] = s match {
    case D.Nop() =>
      P.Nop()::Nil

    case D.ErrorStm(n) =>
      P.ErrorStm(n)::Nil

    case D.CJmp(e, tgt) =>
      // translate to P.CJmp
      Errors.fatalError(s"translation of $s to Perc is unimplemented")

    case D.Jmp(tgt) =>
      // translate to P.Jmp
      Errors.fatalError(s"translation of $s to Perc is unimplemented")

    case D.Store(offset, address, value) =>
      // translate to P.Store
      Errors.fatalError(s"translation of $s to Perc is unimplemented")

    case D.Move(t, e) =>
      translate(e, t)

    case D.LabelStm(l) =>
      P.LabelStm(l)::Nil
  }

  // Translate the expression `e`, writing the result into temporary `t`
  def translate(e: D.Exp, t: TempName): List[P.Stm] = e match {
    case D.Begin(stms, e) =>
      val ps = stms flatMap { s => translate(s) }
      ps ++ translate(e, t)

    case D.Call(f, es) =>
      // Be careful to evaluate all arguments into temporaries *and then*
      // set all the argument registers. Otherwise a later argument
      // might overwrite one of the earlier arguments.

      // Evaluate the arguments into new temporaries.
      // Set the argument registers using P.SetArg.
      // Then P.Call
      Errors.fatalError(s"translation of $e to Perc is unimplemented")

    case D.Alloc(sz) =>
      // Translate into a call to "ristretto_alloc"
      val tsz = newTemp()
      translate(sz, tsz) :+ P.SetArg(0, P.Temp(tsz)) :+ P.Call(t, P.Global("ristretto_alloc"))

    case D.Load(offset, addr) =>
      // Translate into P.Load
      Errors.fatalError(s"translation of $e to Perc is unimplemented")

    case D.BinOp(op, e1, e2) =>
      // Arithmetic expressions are straightforward.
      //
      // But comparisons are translated into a conditional jump.
      // Here's one possible translation for EQ:
      // t := [[ EQ e1 e2 ]] =
      //   t1 := [[ e1 ]]
      //   t2 := [[ e2 ]]
      //   t3 := SUB t1 t2
      //   move t = 1
      //   cjmp EQ t3 0 L
      //   move t = 0
      //   L:
      //
      // t := [[ ADD e1 e2 ]]
      Errors.fatalError(s"translation of $e to Perc is unimplemented")

    case D.Temp(x) if x == t =>
      Nil

    case D.Temp(x) =>
      P.Move(t, P.Temp(x))::Nil

    case D.Lit(n) =>
      P.Move(t, P.Lit(n))::Nil

    case D.Global(x) =>
      P.Move(t, P.Global(x))::Nil
  }

  def newLabel(): Label = {
    ristretto.main.FreshId.freshId("Lperc")
  }

  def newTemp(): Temp = {
    ristretto.main.FreshId.freshId("tperc")
  }
}

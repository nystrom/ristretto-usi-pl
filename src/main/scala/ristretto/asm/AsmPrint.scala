package ristretto.asm

object AsmPrint {
  import ristretto.asm.AsmPrettyPrinter
  import ristretto.asm.AsmSyntax._

  def print(out: java.io.PrintStream, a: Root): Unit = {
    out.println("\t.section	__TEXT,__text,regular,pure_instructions")
    out.println("\t.macosx_version_min 10, 12")

    a match {
      case Root(procs) =>
        procs foreach {
          case Proc(label, insns) =>
            out.println(s"\t.globl _$label")
            out.println("\t.p2align 4, 0x90")
            insns foreach {
              case Label(label) =>
                out.println(s"$label:")
              case insn =>
                out.println(s"\t${AsmPrettyPrinter.show(insn)}")
            }
            out.println()
        }
    }
  }
}

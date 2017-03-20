package ristretto.main

object Compiler extends Errors {
  def main(args: Array[String]): Unit = {
    import ristretto.frontend.RistrettoPrettyPrinter
    import ristretto.frontend.Ristretto
    import ristretto.frontend.RistrettoSyntax.{ASTNode, Root}
    import ristretto.drip.DripPrettyPrinter
    import ristretto.perc.PercPrettyPrinter
    import org.bitbucket.inkytonik.kiama.util.FileSource
    import java.io._

    if (args.length == 0) {
      println("usage: Compiler file.r")
    }
    else {
      val sourceName = args(0)
      try {
        val src = FileSource(sourceName)
        try {
          val p = new Ristretto(src, Posns)
          val result = p.pRoot(0)
          if (result.hasValue) {
            result.semanticValue[ASTNode] match {
              case t: Root =>
                println(RistrettoPrettyPrinter.show(t))
                ristretto.frontend.Typer.typeCheck(t)
                val d = ristretto.frontend.DripGen.translate(t)
                println(d)
                println(DripPrettyPrinter.show(d))
                val p = ristretto.drip.PercGen.translate(d)
                println(PercPrettyPrinter.show(p))
              case _ =>
                println(p.formatParseError(result.parseError, true))
            }
          }
          else {
            println(p.formatParseError(result.parseError, true))
          }
        }
        catch {
          case e: IOException => println(e.getMessage)
        }
      }
      catch {
        case e: IOException => println(e.getMessage)
      }
    }
  }
}

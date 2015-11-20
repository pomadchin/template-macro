package annotations

import utils.MacroApplication
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.collection.mutable.{Map => MMap, ListBuffer}

final class template(fun: Any) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro templateMacro.apply
}

class templateMacro(val c: Context) extends MacroApplication {

  import c.universe._
  import c.Expr

  // TODO: absract over function global vals usage
  def apply(annottees: Expr[Any]*): Expr[Any] = {
    val tree = MacroApp(c.macroApplication).termArgs.head.head
    val expr = c.Expr[Any](c.typecheck(tree))
    val unapplyArgType = expr.actualType.typeArgs.last
    val q"(..${fargs: List[ValDef]}) => ${fbody: Tree}" = tree
    val fargsMap = fargs.zipWithIndex.map { case (v, i) => v.name.toString -> (i, v.name) }.toMap

    // TODO: avoid mutable collections usage
    val indexedMutableBuffer = MMap[Int, ListBuffer[Tree]]()

    def recursiveMatch(subtree: List[Tree]): List[Tree] = {
      subtree.map {
        case Apply(fun, args) => Apply(fun, recursiveMatch(args))
        case arg => {
          fargsMap.get(arg.toString()).fold(arg){ case (index, fa) =>
            val indexedTerm = indexedMutableBuffer.get(index).fold({
              val term = TermName(s"${fa}0")
              indexedMutableBuffer.put(index, ListBuffer(q"$term"))
              term
            })({ case buff =>
              val term = TermName(s"$fa${buff.length}")
              buff += q"$term"
              term
            })
            pq"$indexedTerm @ ${Ident(termNames.WILDCARD)}" }
        }
      }
    }

    val result = recursiveMatch(List(fbody)).head
    val sortedBuffer = indexedMutableBuffer.toList.sortBy(_._1)

    val buffer =
      if(sortedBuffer.map(_._2.length > 1).reduce(_ && _)) cq"$result => None"
      else cq"$result => Some((..${sortedBuffer.flatMap(_._2)}))"

    c.Expr[Any] {
      annottees.map(_.tree).toList match {
        case q"object $name extends ..$parents { ..$body }" :: Nil =>
          q"""
          object $name extends ..$parents {
            def apply = $tree
            def unapply(a: $unapplyArgType): Option[(..${expr.actualType.typeArgs.init})] = a match {
              case $buffer
              case _ => None
            }
            ..$body
          }
        """
      }
    }
  }
}

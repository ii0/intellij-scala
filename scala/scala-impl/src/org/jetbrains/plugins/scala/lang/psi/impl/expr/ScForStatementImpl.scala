package org.jetbrains.plugins.scala
package lang
package psi
package impl
package expr

import com.intellij.lang.ASTNode
import com.intellij.openapi.progress.ProcessCanceledException
import com.intellij.psi._
import com.intellij.psi.scope._
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaCode._
import org.jetbrains.plugins.scala.lang.psi.impl.expr.ScForStatementImpl._
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result._
import org.jetbrains.plugins.scala.lang.resolve.StdKinds
import org.jetbrains.plugins.scala.lang.resolve.processor.CompletionProcessor
import org.jetbrains.plugins.scala.macroAnnotations.{Cached, ModCount}
import org.jetbrains.plugins.scala.project.{ProjectPsiElementExt, ScalaLanguageLevel}

import scala.collection.mutable

/**
  * @author Alexander Podkhalyuzin
  *         Date: 06.03.2008
  */
class ScForStatementImpl(node: ASTNode) extends ScExpressionImplBase(node) with ScForStatement {
  def isYield: Boolean = findChildByType[PsiElement](ScalaTokenTypes.kYIELD) != null

  def enumerators: Option[ScEnumerators] = findChild(classOf[ScEnumerators])

  // Binding patterns in reverse order
  def patterns: Seq[ScPattern] = enumerators.toSeq.flatMap(_.patterns)

  override def getDesugaredExpr(forDisplay: Boolean): Option[ScExpression] = {
    val result =
      if (forDisplay) generateDesugaredExprWithPatternMapping(forDisplay = true)
      else getDesugarizedExprWithPatternMapping

    result map { case (expr, _) => expr }
  }


  override def getDesugaredPatternAnalog(pattern: ScPattern): Option[ScPattern] = {
    getDesugarizedExprWithPatternMapping flatMap {
      case (_, mapping) =>
        mapping.get(pattern)
    }
  }

  // we only really need to cache the version that is used by type inference
  @Cached(ModCount.getBlockModificationCount, this)
  private def getDesugarizedExprWithPatternMapping: Option[(ScExpression, Map[ScPattern, ScPattern])] =
    generateDesugaredExprWithPatternMapping(forDisplay = false)

  override def processDeclarations(processor: PsiScopeProcessor,
                                   state: ResolveState,
                                   lastParent: PsiElement,
                                   place: PsiElement): Boolean = {

    val enumerators: ScEnumerators = this.enumerators match {
      case None => return true
      case Some(x) => x
    }
    if (lastParent == enumerators) return true
    enumerators.processDeclarations(processor, state, null, place)
  }

  protected def bodyToText(expr: ScExpression): String = expr.getText

  private def generateDesugaredExprWithPatternMapping(forDisplay: Boolean): Option[(ScExpression, Map[ScPattern, ScPattern])] = {
    def findPatternElement(e: PsiElement, org: ScPattern): Option[ScPattern] = {
      Some(e) flatMap {
        case pattern: ScPattern if pattern.getTextLength == org.getTextLength =>
          Some(pattern)
        case _ if e.getTextLength <= org.getTextLength =>
          findPatternElement(e.getParent, org)
      }
    }

    generateDesugaredExprTextWithPatternMapping(forDisplay) flatMap {
      case (desugaredText, mapping) =>
        try {
          ScalaPsiElementFactory.createOptionExpressionWithContextFromText(desugaredText, this.getContext, this) map {
            expr =>
              lazy val patternMapping = mapping.flatMap {
                case (originalPattern, idx) =>
                  Option(expr.findElementAt(idx)) flatMap { findPatternElement(_, originalPattern) } map {
                    p => originalPattern -> p
                  }

              }.toMap
              expr -> (if (forDisplay) Map.empty else patternMapping)
          }
        } catch {
          case p: ProcessCanceledException => throw p
          case _: Throwable => None
        }
    }
  }

  private def generateDesugaredExprTextWithPatternMapping(forDisplay: Boolean): Option[(String, Seq[(ScPattern, Int)])] = {
    var _nextNameIdx = 0
    def newNameIdx(): String = {
      _nextNameIdx += 1
      if (forDisplay) _nextNameIdx.toString else "forIntellij" + _nextNameIdx
    }
    val `=>` = ScalaPsiUtil.functionArrow(getProject)

    val underscores = ScUnderScoreSectionUtil.underscores(this).zipWithIndex.toMap
    def underscoreName(i: Int): String = s"forAnonParam$$$i"

    def allUnderscores(expr: PsiElement): Seq[ScUnderscoreSection] = {
      expr match {
        case underscore: ScUnderscoreSection => Seq(underscore)
        case _ => expr.getChildren.flatMap(allUnderscores)
      }
    }

    def normalizeUnderscores(expr: ScExpression): ScExpression = {
      expr match {
        case underscore: ScUnderscoreSection =>
          underscores.
            get(underscore).
            map(underscoreName).
            map(ScalaPsiElementFactory.createReferenceExpressionFromText).
            getOrElse(underscore)
        case _ =>
          allUnderscores(expr) map {
            underscores.get
          } match {
            case underscoreIndices if !underscoreIndices.exists(_.isDefined) =>
              expr
            case underscoreIndices =>
              val copyOfExpr = expr.copy().asInstanceOf[ScExpression]
              for {
                (underscore, Some(index)) <- allUnderscores(copyOfExpr) zip underscoreIndices
                name = underscoreName(index)
                referenceExpression = ScalaPsiElementFactory.createReferenceExpressionFromText(name)
              } {
                underscore.replaceExpression(referenceExpression, removeParenthesis = false)
              }
              copyOfExpr
          }
      }
    }

    def toTextWithNormalizedUnderscores(expr: ScExpression): String = {
      normalizeUnderscores(expr).getText
    }

    def needsPatternMatchFilter(pattern: ScPattern): Boolean =
      !pattern.isIrrefutableFor(if (forDisplay) pattern.expectedType else None)

    val resultText: StringBuilder = new StringBuilder
    val patternMappings = mutable.Map.empty[ScPattern, Int]

    def markPatternHere(pattern: ScPattern): Unit = {
      if (pattern != null && !patternMappings.contains(pattern))
        patternMappings += pattern -> resultText.length
    }

    def appendFunc[R](funcName: String, args: Seq[(Option[ScPattern], String)], forceCases: Boolean = false, forceBlock: Boolean = false)
                  (appendBody: => R): R = {
      val argPatterns = args.flatMap(_._1)
      val needsCase = !forDisplay || forceCases || args.size > 1  || argPatterns.exists(needsDeconstruction)
      val needsParenthesis = args.size > 1 || !needsCase && argPatterns.exists(needsParenthesisAsLambdaArgument)

      resultText ++= "."
      resultText ++= funcName

      resultText ++= (if (needsCase) " { case " else if (forceBlock) " { " else "(")

      if (needsParenthesis)
        resultText ++= "("
      if (args.isEmpty) {
        resultText ++= "_"
      }
      for (((p, text), idx) <- args.zipWithIndex) {
        if (idx != 0) {
          resultText ++= ", "
        }
        p.foreach(markPatternHere)
        resultText ++= text
      }
      if (needsParenthesis)
        resultText ++= ")"
      resultText ++= " "
      resultText ++= `=>`
      resultText ++= " "

      // append the body part
      val ret = appendBody

      resultText ++= (if (needsCase || forceBlock) " }" else ")")
      ret
    }

    def appendGen(gen: ScGenerator, restEnums: Seq[PsiElement]): Unit = {
      val rvalue = Option(gen.rvalue).map(normalizeUnderscores)
      val isLastGen = !restEnums.exists(_.isInstanceOf[ScGenerator])

      val pattern = gen.pattern
      type Arg = (Option[ScPattern], String)
      var initialArg = Seq(Some(pattern) -> pattern.getText)

      // start with the generator expression
      val generatorNeedsParenthesis = rvalue.exists {
        rvalue =>
          val inParenthesis = code"($rvalue).foo".getFirstChild.asInstanceOf[ScParenthesisedExpr]
          ScalaPsiUtil.needParentheses(inParenthesis, inParenthesis.innerElement.get)
      }

      if (generatorNeedsParenthesis)
        resultText ++= "("
      resultText ++= rvalue.map(_.getText).getOrElse("???")
      if (generatorNeedsParenthesis)
        resultText ++= ")"

      // add guards and assignment enumerators
      lazy val hasWithFilter = rvalue.exists(rvalue => {
        val rvalueTy = rvalue.`type`().getOrAny
        // try to use withFilter
        // if the type does not have a withFilter method use filter
        // in the case that we want to desugar for the user,
        // check if the type has a filter function,
        // Because if it doesn't have neither, use withFilter as fallback
        hasMethod(rvalueTy, "withFilter") ||
          (forDisplay && !hasMethod(rvalueTy, "filter"))
      })

      // since 2.12 the compiler doesn't rewrite withFilter to filter
      val filterFunc = if (this.scalaLanguageLevel.exists(_ >= ScalaLanguageLevel.Scala_2_12) || hasWithFilter) "withFilter" else "filter"

      if (needsPatternMatchFilter(pattern)) {
        appendFunc(filterFunc, initialArg, forceCases = true) {
          resultText ++= "true; case _ => false"
        }
      }

      case class ForBinding(forBinding: ScForBinding) {
        def rvalue: Option[ScExpression] = Option(forBinding.rvalue)

        def pattern: Option[ScPattern] = Option(forBinding.pattern)

        def isWildCard: Boolean = pattern.exists(_.isInstanceOf[ScWildcardPattern])

        val ownName: Option[String] = pattern collect {
          case p: ScNamingPattern => p.name
          case p: ScReferencePattern => p.name
          case p: ScTypedPattern => p.name
        }

        val name: String = ownName.getOrElse("v$" + newNameIdx())

        def patternText: String =
          pattern map { pattern => pattern.getText } getOrElse name

        def rvalueText: String =
          rvalue map {
            toTextWithNormalizedUnderscores
          } getOrElse "???"
      }

      def printForBindings(forBindings: Seq[ForBinding]): Unit = {
        forBindings foreach {
          binding =>
            val pattern = binding.pattern
            val patternText = binding.patternText

            resultText ++= "val "
            if (binding.ownName.isDefined || (forDisplay && binding.isWildCard)) {
              pattern.foreach(markPatternHere)
              resultText ++= patternText
            } else {
              val needsParenthesis = forDisplay && pattern.exists(needsParenthesisAsNamedPattern)
              resultText ++= binding.name
              resultText ++= "@"
              if (needsParenthesis)
                resultText ++= "("
              pattern.foreach(markPatternHere)
              resultText ++= patternText
              if (needsParenthesis)
                resultText ++= ")"
            }
            resultText ++= " = "
            resultText ++= binding.rvalueText
            resultText ++= "; "
        }
      }

      // before a guard can be printed, all forBindings have to be mapped into the argument tuple
      def printForBindingMap(forBindings: Seq[ForBinding], args: Seq[Arg]): Seq[Arg] = {
        if (forBindings.nonEmpty) {
          appendFunc("map", args, forceBlock = true) {
            printForBindings(forBindings)

            // remove wildcards
            val argsWithoutWildcards = args.filterNot(_._2 == "_")

            val usedBindings = if (forDisplay) forBindings.filter(!_.isWildCard) else forBindings
            val needsArgParenthesis = argsWithoutWildcards.length + usedBindings.size != 1

            // if args is empty we return ()
            if (needsArgParenthesis)
              resultText ++= "("
            resultText ++= (argsWithoutWildcards.map(_._2) ++ usedBindings.map(_.name)).mkString(", ")
            if (needsArgParenthesis)
              resultText ++= ")"

            argsWithoutWildcards ++ usedBindings.map(b => b.pattern -> b.patternText)
          }
        } else args
      }

      val (forBindingsAndGuards, nextEnums) = restEnums span { !_.isInstanceOf[ScGenerator] }

      val (forBindingsInGenBody, generatorArgs) = {
        // accumulate all ForBindings for the next guard or
        forBindingsAndGuards.foldLeft[(Seq[ForBinding], Seq[Arg])]((Seq.empty, initialArg)) {
          case ((forBindings, args), forBinding: ScForBinding) =>
            (forBindings :+ ForBinding(forBinding), args)

          case ((forBindings, args), guard: ScGuard) =>
            val argsWithBindings = printForBindingMap(forBindings, args)

            appendFunc(filterFunc, argsWithBindings) {
              resultText ++= guard.expr.map(toTextWithNormalizedUnderscores).getOrElse("???")
              if (!forDisplay) {
                // make sure the result type is Boolean to support type checking
                resultText ++= "; true"
              }
            }

            (Seq.empty, argsWithBindings)
        }
      }

      val funcText = if (isLastGen)
        if (isYield) "map" else "foreach"
      else
        "flatMap"

      appendFunc(funcText, generatorArgs, forceBlock = forBindingsInGenBody.nonEmpty) {
        printForBindings(forBindingsInGenBody)

        nextEnums.headOption match {
          case Some(nextGen: ScGenerator) =>
            appendGen(nextGen, nextEnums.tail)
          case _ =>
            assert(nextEnums.isEmpty)

            // sometimes the body of a for loop is enclosed in {}
            // we can remove these brackets
            def withoutBodyBrackets(e: ScExpression): ScExpression = e match {
              case ScBlockExpr.Expressions(inner) => inner
              case _ => e
            }

            resultText ++= body.map(normalizeUnderscores).map(withoutBodyBrackets).map(bodyToText).getOrElse("{}")
        }
      }
    }

    val firstGen = enumerators.flatMap(_.generators.headOption) getOrElse {
      return None
    }

    val enums = firstGen.withNextSiblings.filter { //that is from PsiElementExt
      case _: ScGuard | _: ScForBinding | _: ScGenerator => true
      case _ => false
    }.toList

    appendGen(firstGen, enums.tail)
    val desugaredExprText = resultText.toString
    if (underscores.nonEmpty) {
      val lambdaPrefix = underscores.values.map(underscoreName) match {
        case Seq(arg) => arg + " " + `=>` + " "
        case args => args.mkString("(", ", ", ") ") + `=>` + " "
      }

      Some(lambdaPrefix + desugaredExprText -> patternMappings.toSeq.map { case (p, idx) => (p, idx + lambdaPrefix.length) })
    } else {
      Some(desugaredExprText, patternMappings.toSeq)
    }
  }

  private def hasMethod(ty: ScType, methodName: String): Boolean = {
    var found = false
    val processor: CompletionProcessor= new CompletionProcessor(StdKinds.methodRef, this, isImplicit = true) {
      override protected def execute(namedElement: PsiNamedElement)
                                    (implicit state: ResolveState): Boolean = {
        super.execute(namedElement)
        found = !levelSet.isEmpty
        !found
      }

      override protected val forName = Some(methodName)
    }
    processor.processType(ty, this)
    found
  }

  override protected def innerType: TypeResult = {
    getDesugaredExpr() flatMap {
      case f: ScFunctionExpr => f.result
      case e => Some(e)
    } match {
      case Some(newExpr) => newExpr.getNonValueType()
      case None => Failure("Cannot create expression")
    }
  }

  def getLeftParenthesis = Option(findChildByType[PsiElement](ScalaTokenTypes.tLPARENTHESIS))

  def getRightParenthesis = Option(findChildByType[PsiElement](ScalaTokenTypes.tRPARENTHESIS))

  override def toString: String = "ForStatement"
}

object ScForStatementImpl {
  private def needsDeconstruction(pattern: ScPattern): Boolean = {
    pattern match {
      case null => false
      case _: ScWildcardPattern => false
      case _: ScReferencePattern => false
      case _: ScTypedPattern => false
      case _ => true
    }
  }

  private def needsParenthesisAsLambdaArgument(pattern: ScPattern): Boolean = {
    pattern match {
      case _: ScWildcardPattern => false
      case _: ScReferencePattern => false
      case _ => true
    }
  }

  private def needsParenthesisAsNamedPattern(pattern: ScPattern): Boolean = {
    pattern match {
      case _: ScWildcardPattern => false
      case _: ScReferencePattern => false
      case _: ScTuplePattern => false
      case _: ScConstructorPattern => false
      case _: ScParenthesisedPattern => false
      case _: ScStableReferenceElementPattern => false
      case _ => true
    }
  }
}
package scalaz

trait Show[-A] {
  def show(a: A): List[Char]
}

trait Shows {
  def show[A](f: A => List[Char]) = new Show[A] {
    def show(a: A) = f(a)
  }

  def shows[A](f: A => String): Show[A] = show[A](f(_).toList)

  def showA[A] = shows[A](_.toString)
}

object Show {
  import Scalaz._

  implicit lazy val DigitShow: Show[Digit] = IntShow ∙ ((_: Digit).toInt)

  implicit lazy val OrderingShow: Show[Ordering] = showA

  implicit lazy val UnitShow: Show[Unit] = showA

  implicit lazy val ThrowableShow: Show[Throwable] = showA

  implicit lazy val StringShow: Show[String] = showA

  implicit lazy val IntShow: Show[Int] = showA

  implicit lazy val IntMultiplicationShow: Show[IntMultiplication] = IntShow ∙ ((_: IntMultiplication).value)

  implicit lazy val BooleanShow: Show[Boolean] = showA

  implicit lazy val BooleanConjunctionShow: Show[BooleanConjunction] = BooleanShow ∙ ((_: BooleanConjunction).value)

  implicit lazy val CharShow: Show[Char] = showA

  implicit lazy val CharMultiplicationShow: Show[CharMultiplication] = CharShow ∙ ((_: CharMultiplication).value)

  implicit lazy val ByteShow: Show[Byte] = showA

  implicit lazy val ByteMultiplicationShow: Show[ByteMultiplication] = ByteShow ∙ ((_: ByteMultiplication).value)

  implicit lazy val LongShow: Show[Long] = showA

  implicit lazy val LongMultiplicationShow: Show[LongMultiplication] = LongShow ∙ ((_: LongMultiplication).value)

  implicit lazy val ShortShow: Show[Short] = showA

  implicit lazy val ShortMultiplicationShow: Show[ShortMultiplication] = ShortShow ∙ ((_: ShortMultiplication).value)

  implicit lazy val FloatShow: Show[Float] = showA

  implicit lazy val DoubleShow: Show[Double] = showA

  implicit lazy val BigIntegerShow: Show[java.math.BigInteger] = showA[java.math.BigInteger]

  implicit lazy val BigIntegerMultiplicationShow: Show[BigIntegerMultiplication] = BigIntegerShow ∙ ((_: BigIntegerMultiplication).value)

  implicit lazy val BigIntShow: Show[BigInt] = showA

  implicit lazy val BigIntMultiplicationShow: Show[BigIntMultiplication] = BigIntShow ∙ ((_: BigIntMultiplication).value)

  implicit lazy val NodeSeqShow: Show[xml.NodeSeq] = showA

  implicit def NonEmptyListShow[A](implicit sa: Show[A]): Show[NonEmptyList[A]] = IterableShow(sa) ∙ ((_: NonEmptyList[A]).list)

  implicit def ZipStreamShow[A](implicit sa: Show[A]): Show[ZipStream[A]] = IterableShow(sa) ∙ ((_: ZipStream[A]).value)

  implicit def ZipperShow[A](implicit sa: Show[A]): Show[Zipper[A]] = show((z: Zipper[A]) =>
      z.lefts.reverse.show ++ " " ++ sa.show(z.focus) ++ " " ++ z.rights.show)

  implicit def TreeShow[A](implicit sa: Show[A]): Show[Tree[A]] = show((t: Tree[A]) =>
      '{' :: sa.show(t.rootLabel) ++ " " ++ t.subForest.show ++ "}")

  implicit def TreeLocShow[A](implicit sa: Show[A]): Show[TreeLoc[A]] = show((t: TreeLoc[A]) =>
      t.toTree.show ++ "@" ++ t.parents.map(_._1.length).reverse.show)

  implicit def IterableShow[A](implicit sa: Show[A]): Show[Iterable[A]] = show(as => {
    val i = as.iterator
    val k = new collection.mutable.ListBuffer[Char]
    k += '['
    while (i.hasNext) {
      val n = i.next
      k ++= n.show
      if (i.hasNext)
        k += ','
    }
    k += ']'
    k.toList
  })

  implicit def Tuple1Show[A](implicit sa: Show[A]): Show[Tuple1[A]] = show(a => {
    val k = new collection.mutable.ListBuffer[Char]
    k += '('
    k ++= a._1.show
    k += ')'
    k.toList
  })

  implicit def Tuple2Show[A, B](implicit sa: Show[A], sb: Show[B]): Show[(A, B)] = show {
    case (a, b) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple3Show[A, B, C](implicit sa: Show[A], sb: Show[B], sc: Show[C]): Show[(A, B, C)] = show {
    case (a, b, c) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple4Show[A, B, C, D](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D]): Show[(A, B, C, D)] = show {
    case (a, b, c, d) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple5Show[A, B, C, D, E](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D], se: Show[E]): Show[(A, B, C, D, E)] = show {
    case (a, b, c, d, e) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k ++= ", ".toList
      k ++= e.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple6Show[A, B, C, D, E, F](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D], se: Show[E], sf: Show[F]): Show[(A, B, C, D, E, F)] = show {
    case (a, b, c, d, e, f) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k ++= ", ".toList
      k ++= e.show
      k ++= ", ".toList
      k ++= f.show
      k += ')'
      k.toList
    }
  }

  implicit def Tuple7Show[A, B, C, D, E, F, G](implicit sa: Show[A], sb: Show[B], sc: Show[C], sd: Show[D], se: Show[E], sf: Show[F], sg: Show[G]): Show[(A, B, C, D, E, F, G)] = show {
    case (a, b, c, d, e, f, g) => {
      val k = new collection.mutable.ListBuffer[Char]
      k += '('
      k ++= a.show
      k ++= ", ".toList
      k ++= b.show
      k ++= ", ".toList
      k ++= c.show
      k ++= ", ".toList
      k ++= d.show
      k ++= ", ".toList
      k ++= e.show
      k ++= ", ".toList
      k ++= f.show
      k ++= ", ".toList
      k ++= g.show
      k += ')'
      k.toList
    }
  }

  implicit def Function0Show[A](implicit sa: Show[A]): Show[Function0[A]] = show(_.apply.show)

  implicit def OptionShow[A](implicit sa: Show[A]): Show[Option[A]] = shows(_ map (_.shows) toString)

  implicit def EitherShow[A, B](implicit sa: Show[A], sb: Show[B]): Show[Either[A, B]] = shows(e => (((_: A).shows) <-: e :-> (_.shows)).toString)

  implicit def ValidationShow[E, A](implicit se: Show[E], sa: Show[A]): Show[Validation[E, A]] = shows {
    case Success(a) => "Success(" + a.shows + ")"
    case Failure(e) => "Failure(" + e.shows + ")"
  }

  implicit def JavaIterableShow[A](implicit sa: Show[A]): Show[java.lang.Iterable[A]] = show(as => {
    val k = new collection.mutable.ListBuffer[Char]
    val i = as.iterator
    k += '['
    while (i.hasNext) {
      val n = i.next
      k ++= n.show
      if (i.hasNext)
        k += ','
    }
    k += ']'
    k.toList
  })

  implicit def JavaMapShow[K, V](implicit sk: Show[K], sv: Show[V]): Show[java.util.Map[K, V]] = show(m => {
    val z = new collection.mutable.ListBuffer[Char]
    z += '{'
    val i = m.keySet.iterator
    while (i.hasNext) {
      val k = i.next
      val v = m get k
      z ++= k.show
      z ++= " -> ".toList
      z ++= v.show
      if (i.hasNext)
        z += ','
    }
    z += '}'
    z.toList
  })
}

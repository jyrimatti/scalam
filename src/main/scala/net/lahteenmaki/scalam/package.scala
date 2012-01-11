package net.lahteenmaki

import scalala.scalar.Scalar

import Vector._

package object scalam {
	sealed trait Bool
	sealed trait True extends Bool
	sealed trait False extends Bool

	sealed trait Comparison {
		type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up
		type le = Match[True, True, False, Bool]
	}

	sealed trait GT extends Comparison {
		type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
	}
	sealed trait LT extends Comparison {
		type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
	}
	sealed trait EQ extends Comparison {
		type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
	}

	trait Fold[-Elem, Value] {
		type Apply[E <: Elem, V <: Value] <: Value
	}

	sealed trait D {
		type Match[NonZero[N <: D] <: Up, IfZero <: Up, Up] <: Up
		type FoldR[Init <: Type, Type, F <: Fold[D, Type]] <: Type
		type Compare[N <: D] <: Comparison
	}
	private[scalam] sealed trait D0 extends D {
		type Match[NonZero[N <: D] <: Up, IfZero <: Up, Up] = IfZero
		type FoldR[Init <: Type, Type, F <: Fold[D, Type]] = Init
		type Compare[N <: D] = N#Match[ConstLT, EQ, Comparison]
		type ConstLT[A] = LT
	}
	sealed trait Succ[N <: D] extends D {
		type Match[NonZero[N <: D] <: Up, IfZero <: Up, Up] = NonZero[N]
		type FoldR[Init <: Type, Type, F <: Fold[D, Type]] = F#Apply[Succ[N], N#FoldR[Init, Type, F]]
		type Compare[O <: D] = O#Match[N#Compare, GT, Comparison]
	}
	type Add[A <: D, B <: D] = A#FoldR[B, D, Inc]
	type Inc = Fold[D, D] {
		type Apply[N <: D, Acc <: D] = Succ[Acc]
	}

	class IntRep[Dim <: D](val value: Int) {
		def succ = new IntRep[Succ[Dim]](value + 1)
	}
	implicit def intRep1 = new IntRep[D1](1)
	implicit def intRepN[Dim <: D](implicit rep: IntRep[Dim]): IntRep[Succ[Dim]] = rep.succ

  type D1 = Succ[D0]
	type D2 = Succ[D1]
	type D3 = Succ[D2]
	type D4 = Succ[D3]
	type D5 = Succ[D4]
	type D6 = Succ[D5]
	type D7 = Succ[D6]
	type D8 = Succ[D7]
	type D9 = Succ[D8]
	type D10 = Succ[D9]
	type D11 = Succ[D10]
	type D12 = Succ[D11]
	type D13 = Succ[D12]
	type D14 = Succ[D13]
	type D15 = Succ[D14]
	type D16 = Succ[D15]
	type D17 = Succ[D16]
	type D18 = Succ[D17]
	type D19 = Succ[D18]
	type D20 = Succ[D19]
	type D21 = Succ[D20]
	type D22 = Succ[D21]

	implicit def productInfo1[T]: Product1[T] => (D1, T) = null
	implicit def productInfo2[T]: Product2[T, T] => (D2, T) = null
	implicit def productInfo3[T]: Product3[T, T, T] => (D3, T) = null
	implicit def productInfo4[T]: Product4[T, T, T, T] => (D4, T) = null
	implicit def productInfo5[T]: Product5[T, T, T, T, T] => (D5, T) = null
	implicit def productInfo6[T]: Product6[T, T, T, T, T, T] => (D6, T) = null
	implicit def productInfo7[T]: Product7[T, T, T, T, T, T, T] => (D7, T) = null
	implicit def productInfo8[T]: Product8[T, T, T, T, T, T, T, T] => (D8, T) = null
	implicit def productInfo9[T]: Product9[T, T, T, T, T, T, T, T, T] => (D9, T) = null
	implicit def productInfo10[T]: Product10[T, T, T, T, T, T, T, T, T, T] => (D10, T) = null
	implicit def productInfo11[T]: Product11[T, T, T, T, T, T, T, T, T, T, T] => (D11, T) = null
	implicit def productInfo12[T]: Product12[T, T, T, T, T, T, T, T, T, T, T, T] => (D12, T) = null
	implicit def productInfo13[T]: Product13[T, T, T, T, T, T, T, T, T, T, T, T, T] => (D13, T) = null
	implicit def productInfo14[T]: Product14[T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D14, T) = null
	implicit def productInfo15[T]: Product15[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D15, T) = null
	implicit def productInfo16[T]: Product16[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D16, T) = null
	implicit def productInfo17[T]: Product17[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D17, T) = null
	implicit def productInfo18[T]: Product18[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D18, T) = null
	implicit def productInfo19[T]: Product19[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D19, T) = null
	implicit def productInfo20[T]: Product20[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D20, T) = null
	implicit def productInfo21[T]: Product21[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D21, T) = null
	implicit def productInfo22[T]: Product22[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T] => (D22, T) = null

	implicit def productAsVector1[T](p: Product1[T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector2[T](p: Product2[T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector3[T](p: Product3[T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector4[T](p: Product4[T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector5[T](p: Product5[T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector6[T](p: Product6[T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector7[T](p: Product7[T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector8[T](p: Product8[T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector9[T](p: Product9[T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector10[T](p: Product10[T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector11[T](p: Product11[T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector12[T](p: Product12[T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector13[T](p: Product13[T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector14[T](p: Product14[T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector15[T](p: Product15[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector16[T](p: Product16[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector17[T](p: Product17[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector18[T](p: Product18[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector19[T](p: Product19[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector20[T](p: Product20[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector21[T](p: Product21[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
	implicit def productAsVector22[T](p: Product22[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T])(implicit s: Scalar[T]) = Vector(p)
}
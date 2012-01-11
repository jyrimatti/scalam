package net.lahteenmaki.scalam

import scalala._
import scalala.operators._
import scalala.scalar.Scalar
import scalala.operators.BinaryOp._
import scalala.tensor.dense.DenseMatrix

class ColumnVector[T, Dim <: D] private[scalam] (m: DenseMatrix[T]) extends Matrix[T, Dim, D1](m) {
	private[scalam] def this(vals: Array[T])(implicit s: Scalar[T]) = this(DenseMatrix(vals.map(v => Array(v)(s.manifest)): _*)(tensor.LiteralRow.array, s))

	override def T = new RowVector[T, Dim](m.t.toDense)

	def +[B](other: ColumnVector[B, Dim])(implicit o: BinOp[B, OpAdd]) = new ColumnVector[B, Dim](m :+ other.m)

	override def *[B](value: B)(implicit o: BinOpScalar[B, OpMulMatrixBy], s: Scalar[B]) = new ColumnVector[B, Dim](m.*[DenseMatrix[T], B, DenseMatrix[B]](value))

	override def *[B, ResultCols <: D](other: Matrix[B, D1, ResultCols])(implicit o: BinOp[B, OpMulMatrixBy]) = new Matrix[B, Dim, ResultCols](m.*[DenseMatrix[T], DenseMatrix[B], DenseMatrix[B]](other.m))

	def ++[OtherD <: D](other: Matrix[T, OtherD, D1])(implicit s: Scalar[T]) = new ColumnVector[T, Add[Dim, OtherD]](DenseMatrix.vertcat(m, other.m))

	def apply[Index <: D](implicit upperBound: Index#Compare[Dim]#le =:= True, i: IntRep[Index]): T = m(i.value - 1, 0)

	def length: Int = super.size._1
}

class RowVector[T, Dim <: D] private[scalam] (m: DenseMatrix[T]) extends Matrix[T, D1, Dim](m) {
	private[scalam] def this(vals: Array[T])(implicit s: Scalar[T]) = this(DenseMatrix(vals)(tensor.LiteralRow.array, s))

	override def T = new ColumnVector[T, Dim](m.t.toDense)

	def +[B](other: RowVector[B, Dim])(implicit o: BinOp[B, OpAdd]) = new RowVector[B, Dim](m :+ other.m)

	override def *[B](value: B)(implicit o: BinOpScalar[B, OpMulMatrixBy], s: Scalar[B]) = new RowVector[B, Dim](m.*[DenseMatrix[T], B, DenseMatrix[B]](value))

	override def *[B, ResultCols <: D](other: Matrix[B, Dim, ResultCols])(implicit o: BinOp[B, OpMulMatrixBy]) = new Matrix[B, D1, ResultCols](m.*[DenseMatrix[T], DenseMatrix[B], DenseMatrix[B]](other.m))

	def ++[OtherD <: D](other: Matrix[T, D1, OtherD])(implicit s: Scalar[T]) = new RowVector[T, Add[Dim, OtherD]](DenseMatrix.horzcat(m, other.m))

	def apply[Index <: D](implicit upperBound: Index#Compare[Dim]#le =:= True, i: IntRep[Index]): T = m(0, i.value - 1)

	def length: Int = super.size._2
}

object Vector {
	def apply[T](t: T)(implicit s: Scalar[T]): RowVector[T, D1] = new RowVector(Array(t)(s.manifest))
	def apply[T, Dim <: D, P <: Product](vals: P)(implicit productInfo: P => (Dim, T), s: Scalar[T]): RowVector[T, Dim] =
		new RowVector(vals.productIterator.asInstanceOf[Iterator[T]].toArray(s.manifest))
}
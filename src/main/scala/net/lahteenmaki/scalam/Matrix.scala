package net.lahteenmaki.scalam

import scalala._
import scalala.scalar._
import scalala.operators._
import scalala.tensor.dense.DenseMatrix

class Matrix[T, Rows <: D, Cols <: D](private[scalam] val m: DenseMatrix[T]) extends Equals {
	type BinOpScalar[B, O <: OpType] = BinaryOp[DenseMatrix[T], B, O, DenseMatrix[B]]
	type BinOp[B, O <: OpType] = BinaryOp[DenseMatrix[T], DenseMatrix[B], O, DenseMatrix[B]]

	private[scalam] def this(rows: Array[T]*)(implicit s: Scalar[T]) = this(DenseMatrix(rows: _*))

	def T = new Matrix[T, Cols, Rows](m.t.toDense)

	def size = (m.numRows, m.numCols)

	def +[B](other: Matrix[B, Rows, Cols])(implicit o: BinOp[B, OpAdd]) = new Matrix[B, Rows, Cols](m.+[DenseMatrix[T], DenseMatrix[B], DenseMatrix[B]](other.m))

	def *[B](value: B)(implicit o: BinOpScalar[B, OpMulMatrixBy], s: Scalar[B]) = new Matrix[B, Rows, Cols](m.*[DenseMatrix[T], B, DenseMatrix[B]](value))

	def *[B, ResultCols <: D](other: Matrix[B, Cols, ResultCols])(implicit o: BinOp[B, OpMulMatrixBy]) = new Matrix[B, Rows, ResultCols](m.*[DenseMatrix[T], DenseMatrix[B], DenseMatrix[B]](other.m))

	def canEqual(that: Any) = that.isInstanceOf[Matrix[_, _, _]]
	override def equals(that: Any) = that.isInstanceOf[Matrix[_, _, _]] && m.equals(that.asInstanceOf[Matrix[_, _, _]].m)

	def apply[RowIndex <: D, ColIndex <: D](implicit rowBound: RowIndex#Compare[Rows]#le =:= True, colBound: ColIndex#Compare[Cols]#le =:= True, row: IntRep[RowIndex], col: IntRep[ColIndex]): T = m(row.value - 1, col.value - 1)

	override def toString = m.toString
}

object Matrix {
	def apply[T, Cols <: D, P <: Product](row: RowVector[T, Cols])(implicit s: Scalar[T], man: Manifest[Array[T]]): Matrix[T, D1, Cols] =
		new Matrix[T, D1, Cols](row.m.data)

	def apply[T, Rows <: D, Cols <: D, V[T, Cols <: D] <: RowVector[T, Cols], P <: Product](rows: P)(implicit c: P => (Rows, V[T, Cols]), s: Scalar[T], man: Manifest[Array[T]]): Matrix[T, Rows, Cols] =
		new Matrix[T, Rows, Cols](rows.productIterator.map(_.asInstanceOf[RowVector[T, _]].m.data).toArray(man): _*)

	def rand[Dim <: D](implicit dim: IntRep[Dim]): Matrix[Int, Dim, Dim] = rand[Dim, Dim]
	def rand[Rows <: D, Cols <: D](implicit rows: IntRep[Rows], cols: IntRep[Cols]) =
		new Matrix[Int, Rows, Cols](DenseMatrix.randi(rows.value * cols.value, rows.value, cols.value))

	def ones[T, Dim <: D](implicit dim: IntRep[Dim], s: Scalar[T]): Matrix[T, Dim, Dim] = ones[T, Dim, Dim]
	def ones[T, Rows <: D, Cols <: D](implicit rows: IntRep[Rows], cols: IntRep[Cols], s: Scalar[T]) =
		new Matrix[T, Rows, Cols](DenseMatrix.ones(rows.value, cols.value))

	def zeros[T, Dim <: D](implicit dim: IntRep[Dim], s: Scalar[T]): Matrix[T, Dim, Dim] = ones[T, Dim, Dim]
	def zeros[T, Rows <: D, Cols <: D](implicit rows: IntRep[Rows], cols: IntRep[Cols], s: Scalar[T]) =
		new Matrix[T, Rows, Cols](DenseMatrix.zeros(rows.value, cols.value))
}
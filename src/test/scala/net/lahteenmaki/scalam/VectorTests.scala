package net.lahteenmaki.scalamTest

import org.scalatest.matchers._
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import net.lahteenmaki.scalam._


@RunWith(classOf[JUnitRunner])
class VectorTests extends WordSpec with ShouldMatchers {
	"Vector" should {
		"construct" when {
			"one element" in {
				Vector(1).length should be(1)
			}
			"multiple elements" in {
				Vector(1,2).length should be(2)
				Vector(1,2,3).length should be(3)
			}
		}
		"construct from tuple" when {
			"tuple1" in {
				Tuple1(1).length should be(1)
			}
			"tupleN" in {
				(1,2).T.length should be(2)
				(1,2,3,4,5,6,7,8,9,10).T.length should be(10)
			}
		}
	}
	"Transpose" should {
		"produce a transposed vector" in {
			val v = Vector(1,2)
			v.T.size should be(v.size.swap)
		}
		"should produce original when transposed again" in {
			val v = Vector(1,2)
			v.T.T should equal(v)
		}
	}
	"Addition" should {
		"work for same dimensions" in {
			Vector(1,2) + Vector(3,4) should equal(Vector(4,6))
		}
		"produce a vector" in {
			assert( (Vector(1,2) + Vector(3,4)).isInstanceOf[RowVector[_,_]] )
		}
	}
	"Vector multiplication" should {
		"work for row*col" in {
			Vector(1,2) * Vector(3,4).T should equal(Vector(11))
		}
		"work for col*row" in {
			Vector(1,2).T * Vector(3,4) should equal(Matrix(Vector(3,4),Vector(6,8)))
		}
	}
	"Scalar multiplication" when {
		"applied to a RowVector" should {
			"work for the element type" in {
				Vector(1,2) * 5 should equal(Vector(5,10))
			}
			"work for other type" in {
				Vector(1,2) * 5.5 should equal(Vector(5.5,11.0))
			}
			"produce a vector for a vector" in {
				assert( (Vector(1,2) * 2).isInstanceOf[RowVector[_,_]] )
			}
		}
		"applied to a ColumnVector" should {
			"work for the element type" in {
				Vector(1,2).T * 5 should equal(Vector(5,10).T)
			}
			"work for other type" in {
				Vector(1,2).T * 5.5 should equal(Vector(5.5,11.0).T)
			}
			"produce a vector for a vector" in {
				assert( (Vector(1,2).T * 2).isInstanceOf[ColumnVector[_,_]] )
			}
		}
	}
	"Concatenation" should {
		"work for ColumnVectors" in {
			val v = Vector(1,2).T ++ Vector(3,4,5).T
			v should equal(Vector(1,2,3,4,5).T)
		}
		"work for RowVectors" in {
			val v = Vector(1,2) ++ Vector(3,4,5)
			v should equal(Vector(1,2,3,4,5))
		}
	}
	"Indexing" should {
		"be allowed within length" when {
			"RowVector" in {
				val v = Vector(2,4,6)
				v[D1] should equal(2)
				v[D3] should equal(6)
			}
			"ColumnVector" in {
				val v = Vector(2,4,6).T
				v[D1] should equal(2)
				v[D3] should equal(6)
			}
			"after concatenation" in {
				val v: RowVector[Int,D4] = Vector(1,2) ++ Vector(3,4)
				v[D1] should equal(1)
				v[D4] should equal(4)
			}
		}
	}
	"Adding more dimension types" should {
		"work" in {
			val v7 = Vector(1,2,3,4,5,6,7)
			val v21 = v7 ++ v7 ++ v7
			type D23 = Succ[D22]
			val v23 = v21 ++ Vector(22,23)
			val v23c: RowVector[Int,D23] = v23
			v23c[D23] should equal(23)
			v23[D23] should equal(23)
		}
	}
}
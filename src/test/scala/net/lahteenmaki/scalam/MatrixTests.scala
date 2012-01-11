package net.lahteenmaki.scalamTest

import org.scalatest.matchers._
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import net.lahteenmaki.scalam._

@RunWith(classOf[JUnitRunner])
class MatrixTests extends WordSpec with ShouldMatchers {
	"Matrix" should {
		"construct" when {
			"one element" in {
				Matrix(Vector(1,2)).size should be((1,2))
			}
			"multiple elements" in {
				Matrix(Vector(1,2),Vector(3,4),Vector(5,6)).size should be((3,2))
			}
			"other than integer" in {
				Matrix(Vector(1.5,2.0)).size should be((1,2))
			}
		}
	}
	"Matrix.rand" should {
		"produce a matrix of correct size" when {
			"one size specified" in {
				Matrix.rand[D5].size should be ((5,5))
			}
			"two sizes specified" in {
				Matrix.rand[D5,D7].size should be ((5,7))
			}
		}
	}
	"Matrix.ones" should {
		"produce a matrix of correct size" when {
			"one size specified" in {
				Matrix.ones[Int,D5].size should be ((5,5))
			}
			"two sizes specified" in {
				Matrix.ones[Int,D5,D7].size should be ((5,7))
			}
		}
		"produce a matrix of only ones" in {
			Matrix.ones[Int,D2] should equal (Matrix(Vector(1,1),Vector(1,1)))
		}
		"work for other than Int" in {
			Matrix.ones[Double,D2] should equal (Matrix(Vector(1.0,1.0),Vector(1.0,1.0)))
		}
	}
	"Addition" should {
		"work for same dimensions" when {
			"one row" in {
				Matrix(Vector(1,2)) + Matrix(Vector(3,4)) should equal(Matrix(Vector(4,6)))
			}
			"multiple rows" in {
				val m = Matrix(Vector(1,2), Vector(3,4)) + Matrix(Vector(1,2), Vector(1,4))
				m should equal(Matrix(Vector(2,4), Vector(4,8)))
			}
		}
		"work for other type" in {
			Matrix(Vector(1,2)) + Matrix(Vector(3.0,4.0)) should equal(Matrix(Vector(4.0,6.0)))
		}
	}
	"Scalar multiplication" should {
		"work for the element type" in {
			Matrix.ones[Int,D2] * 5 should equal(Matrix(Vector(5,5),Vector(5,5)))
		}
		"work for other type" in {
			Matrix.ones[Int,D2] * 5.5 should equal(Matrix(Vector(5.5,5.5),Vector(5.5,5.5)))
		}
	}
	"Matrix multiplication" should {
		"work for compatible dimensions" in {
			Matrix.ones[Int,D2,D1] * Matrix.ones[Int,D1,D2] should equal(Matrix.ones[Int,D2,D2])
		}
		"work for other element type" in {
		  Matrix(Vector(1),Vector(2)) * Matrix(Vector(3.1,4.1)) should equal(Matrix(Vector(3.1, 4.1), Vector(6.2,8.2)))
		}
		"work for vectors" when {
			"RowVector" in {
				Matrix.ones[Int,D2,D1] * Vector(1,2) should equal(Matrix(Vector(1,2),Vector(1,2)))
			}
			"ColumnVector" in {
				Matrix.ones[Int,D2,D2] * Vector(1,2).T should equal(Matrix(Vector(3),Vector(3)))
			}
		}
	}
	"Indexing" should {
		"be allowed within length" in {
			val m = Matrix(Vector(1,2),Vector(3,4))
			m[D1,D1] should equal(1)
			m[D1,D2] should equal(2)
			m[D2,D1] should equal(3)
			m[D2,D2] should equal(4)
		}
	}
}
import net.lahteenmaki.scalam._

// vectors
val v2 = Vector(1,2)
val v3 = Vector(1,2,3)
Vector(1.0,2.0)

Vector(1,2.0)


// transpose
v2.T
(1,2).T


// scalar multiplication
v2*2
v2*2.0


// addition
v2 + v2
Vector(1,2) + Vector(1.0,2.0)

v2 + v3


// vector multiplication
v2 * v2.T

v2 * v2
v2 * v3


// concatenation
v2 ++ v3
val v: RowVector[Int,D5] = v2 ++ v3

v2 ++ v2.T


// indexing
v2[D1]
v2[D2]

v2[D3]


// matrices
val m22 = Matrix.ones[Int,D2]
val m23 = Matrix.ones[Int,D2,D3]
Matrix.zeros[Double,D2]
Matrix.rand[D5,D5]


m22.T


m22 + m22

m22 + m23


m22 * 5.5


m22 * m23
m22 * v2
v3 * Matrix.rand[D1,D5]

m23 * m22


m23[D1,D1]
m23[D2,D3]

m23[D3,D3]



val v7 = Vector(1,2,3,4,5,6,7)
val v21 = v7 ++ v7 ++ v7
val v23 = v21 ++ Vector(22,23)
v23[D23]
type D23 = Succ[D22]
v23[D23]


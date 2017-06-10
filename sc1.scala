// import pandas as pd
// import numpy as np
// import matplotlib.pyplot as plt
import scala.io.Source
import scala.collection.mutable.ListBuffer

// tag_headers = ['user_id', 'movie_id', 'tag', 'timestamp']
// tags = pd.read_table('data/tags.dat', sep='::', header=None, names=tag_headers)
// rating_headers = ['user_id', 'movie_id', 'rating', 'timestamp']
// ratings = pd.read_table('data/ratings.dat', sep='::', header=None, names=rating_headers)
// movie_headers = ['movie_id', 'title', 'genres']
// movies = pd.read_table('data/movies.dat',sep='::', header=None, names=movie_headers)
// movie_titles = movies.title.tolist()

val filename = "data/rat.csv"
val users = new ListBuffer[Int]()
val movies = new ListBuffer[Int]()
// Define rating
class Rating( uid: Int, mid: Int, rat: Double ){
    var user_id: Int = uid
    var movie_id: Int = mid
    var rate: Double = rat
    users += uid
    movies += mid
    def getUserId(): Int = user_id
    def getMovieId(): Int = movie_id
    def getRate(): Double = rate
    override def toString: String = s" user_id: $user_id , movie_id: $movie_id, rate: $rate"  
}
// Read file
var ratings = new ListBuffer[Rating]()
var ratingArr = ratings.to[Array]
var numLine: Int = 0
for (line <- Source.fromFile(filename).getLines()) {
  numLine += 1
  if(numLine>1) {
    val uid = line.split(",")(0).toInt 
    val mid = line.split(",")(1).toInt
    val rat = line.split(",")(2).toDouble
    var temp = new Rating(uid,mid,rat)
    ratings += temp
  }
}
var ratingArr = ratings.to[Array]
// Check Pivot Q
def createPivotQ( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratingArr ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = r.getRate()
    }
  }
  return res
}
// Check Pivot W
def createPivotW( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratingArr ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = 1.0
    }
  }
  return res
}
// Parameter
val lambda_ = 0.1
val n_factors = 100
val m = ( users.to[Array].reduceLeft(_ max _) - users.to[Array].reduceLeft(_ min _) ) + 1
val n = ( movies.to[Array].reduceLeft(_ max _) - movies.to[Array].reduceLeft(_ min _) ) + 1
val n_iterations = 20
// *** //

// errors = []
// for ii in range(n_iterations):
//     X = np.linalg.solve(np.dot(Y, Y.T) + lambda_ * np.eye(n_factors), np.dot(Y, Q.T)).T
//     Y = np.linalg.solve(np.dot(X.T, X) + lambda_ * np.eye(n_factors), np.dot(X.T, Q))
//     if ii % 100 == 0:
//         print('{}th iteration is completed'.format(ii))
//     errors.append(get_error(Q, X, Y, W))
// Q_hat = np.dot(X, Y)
// print('Error of rated movies: {}'.format(get_error(Q, X, Y, W)))

// Transpose
import scala.collection.mutable.ListBuffer
val test = new ListBuffer[Array[Double]]()
test += Array(4.0,7.0,2.0,1.0)
test += Array(3.0,9.0,8.0,6.0)
def transe( arr: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    val temp = arr
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr.apply(0).length - 1 ){
        var lineArr = new Array[Double](arr.length)
        for( j <- 0 to arr.length - 1 ){
            lineArr(j) = temp.apply(j).apply(i)
        }
        res += lineArr
    }
    return res
}
// Mult
import scala.collection.mutable.ListBuffer
val test1 = new ListBuffer[Array[Double]]()
test1 += Array(1.0,2.0,3.0)
test1 += Array(4.0,5.0,6.0)
val test2 = new ListBuffer[Array[Double]]()
test2 += Array(7.0,8.0)
test2 += Array(9.0,10.0)
test2 += Array(11.0,12.0)
def sub_mul( i: Int, j: Int, arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): Double = {
    var res = 0.0
    for( k <- 0 to arr1.apply(0).length - 1 ){
        res += arr1.apply(i).apply(k) * arr2.apply(k).apply(j)
    }
    return res
}
def mult( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var m = arr1.length
    var n = arr2.apply(0).length
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to m-1 ){
        var lineArr = new Array[Double](n)
        for( j <- 0 to n-1 ){
            lineArr(j) = sub_mul( i, j, arr1, arr2 ) 
        }
        res += lineArr
    }
    return res
}

var errors = new ListBuffer[Double]()
for( ii <- 0 to n_iterations - 1 ){
    X = transe(solve( , ))
    Y = solve( , )
}

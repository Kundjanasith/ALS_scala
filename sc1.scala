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
test1 += Array(1.0,0.0)
test1 += Array(0.0,1.0)
val test2 = new ListBuffer[Array[Double]]()
test2 += Array(4.0,1.0)
test2 += Array(2.0,2.0)
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
// Inverse 
import scala.collection.mutable.ListBuffer
import scala.util.control._
val test1 = new ListBuffer[Array[Double]]()
test1 += Array(7.0,2.0,1.0)
test1 += Array(0.0,3.0,-1.0)
test1 += Array(-3.0,4.0,-2.0)
def assign( posI: Int, posJ: Int, value: Double, arr: ListBuffer[Array[Double]]): ListBuffer[Array[Double]] = {
   var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr.length - 1 ){
        var lineArr = new Array[Double](arr.apply(0).length)
        for( j <- 0 to arr.apply(0).length - 1 ){
            if(i==posI&&j==posJ){
                lineArr(j) = value
            }
            else{
                lineArr(j) = arr.apply(i).apply(j)
            }
        } 
        res += lineArr
    }
    return res
}
def inverse( arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    // var res = new ListBuffer[Array[Double]]
    var I = new ListBuffer[Array[Double]]
    var C = new ListBuffer[Array[Double]]
    if(arr1.length!=arr1.apply(0).length) { return null }
    print(arr1)
    for( i <- 0 to arr1.length - 1 ){
        var lineArrI = new Array[Double](arr1.length)
        var lineArrC = new Array[Double](arr1.length)
        for( j <- 0 to arr1.length - 1){
            if(i==j){
                lineArrI(j) = 1
            }
            else{
                lineArrI(j) = 0
            }
            lineArrC(j) = arr1.apply(i).apply(j)
        }
        I += lineArrI
        C += lineArrC
    }
    for( i <- 0 to arr1.length - 1 ){
        var e = C.apply(i).apply(i)
        if(e==0){
            val loop = new Breaks;
            loop.breakable {
                for( ii <- i+1 to arr1.length - 1 ){
                    if(C.apply(ii).apply(i)!=0){
                        for( j <- 0 to arr1.length - 1 ){
                            print(j)
                            e = C.apply(i)(j)
                            // C.apply(i).apply(j) = C.apply(ii).apply(j)
                            // C.apply(ii).apply(j) = e
                            C = assign(i,j,C.apply(ii).apply(j),C)
                            C = assign(ii,j,e,C)
                            e = I.apply(i)(j)
                            // I.apply(i).apply(j) = I.apply(ii).apply(j)
                            // I.apply(ii).apply(j) = e
                            I = assign(i,j,I.apply(ii).apply(j),I)
                            I = assign(ii,j,e,I)
                        }
                        loop.break
                    }
                }
            }
            e = C.apply(i)(i)
            if(e==0){ return null}
        }
        for( j <- 0 to arr1.length - 1 ){
            // C.apply(i).apply(j) = C.apply(i).apply(j)/e
            C = assign(i,j,C.apply(i).apply(j)/e,C)
            // I.apply(i).apply(j) = I.apply(i).apply(j)/e
            I = assign(i,j,I.apply(i).apply(j)/e,I)
        }
        for( ii <- 0 to arr1.length - 1 ){
            if(ii==i){
                // continue
            }
            else {
                e = C.apply(ii).apply(i)
                for( j <- 0 to arr1.length - 1 ){
                    // C.apply(ii).apply(j) = C.apply(ii).apply(j) - e*C.apply(i).apply(j)
                    C = assign(ii,j,C.apply(ii).apply(j) - e*C.apply(i).apply(j),C)
                    // I.apply(ii).apply(j) = I.apply(ii).apply(j) - e*I.apply(i).apply(j)
                    I = assign(ii,j,I.apply(ii).apply(j) - e*I.apply(i).apply(j),I)
                }
            }
        } 
    } 
    return I
}
// Solve
import scala.collection.mutable.ListBuffer
val test1 = new ListBuffer[Array[Double]]()
test1 += Array(7.0,5.0)
test1 += Array(3.0,-2.0)
val test2 = new ListBuffer[Array[Double]]()
test2 += Array(3.0)
test2 += Array(22.0)
def solve( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    var in_arr1 = inverse(arr1)
    res = mult(inverse(arr1),arr2)
    return res
}
// Eye
import scala.collection.mutable.ListBuffer
def eyeStar( lambda: Int, n: Int): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to n - 1 ){
        var lineArr = new Array[Double](n)
        for( j <- 0 to n - 1 ){
            if(i==j){
                lineArr(j) = 1 * lambda
            }
            else{
                lineArr(j) = 0
            }
        }
        res += lineArr
    }
    return res 
}
// Plus 
def plus( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) + arr2.apply(i).apply(j)
        }
        res += lineArr
    }
    return res 
}
// Minus
def minus( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) - arr2.apply(i).apply(j)
        }
        res += lineArr
    }
    return res 
}
// Star
def star( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) * arr2.apply(i).apply(j)
        }
        res += lineArr
    }
    return res 
}
// sumR
def sumR( arr: ListBuffer[Array[Double]]): Double = {
    var res = 0.0
    for( i <- 0 to arr.length - 1 ){
        for( j <- 0 to arr.apply(0).length - 1){
            res = res + arr.apply(i).apply(j)
        }
    }
    return res
}
// Get error
def get_error( Q: ListBuffer[Array[Double]], X: ListBuffer[Array[Double]], Y: ListBuffer[Array[Double]], W: ListBuffer[Array[Double]] ): Double = {
    var xy = mult(X,Y)
    var qxy = minus(Q,xy)
    var wqxy = star(W,xy)
    var wqxy2 = start(wqxy,wqxy)
    return sumR(wqxy2)
}   
var errors = new ListBuffer[Double]()
for( ii <- 0 to n_iterations - 1 ){
    X = transe( solve( plus( mult( Y, transe(Y) ) ,  eyeStar( lambda_, n_factors ) ), mult( Y, transe(Q) ) ))
    Y = solve(  plus( mult( transe(X), X ) ,  eyeStar( lambda_, n_factors ) ), mult( transe(X), Q ) )
    // if(ii%100==0){
        println(ii+"iteration is completed")
    // }
    errors += get_error( Q, X, Y, W )
}
var Q_hat = mult( X, Y )

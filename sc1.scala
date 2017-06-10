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

def print_readable( arr: ListBuffer[Array[Double]] ){
    print("[")
    for( i <- 0 to arr.length - 1 ){
        print("(")
        for( j <- 0 to arr.apply(0).length - 1 ){
            print( arr.apply(i).apply(j) + ":" )
        }
        print(")")
    }
    println("]")
}
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

val lambda_ = 0.1
val n_factors = 10
val m = ( users.to[Array].reduceLeft(_ max _) - users.to[Array].reduceLeft(_ min _) ) + 1
val n = ( movies.to[Array].reduceLeft(_ max _) - movies.to[Array].reduceLeft(_ min _) ) + 1
val n_iterations = 10
// var ratingArr = ratings.to[Array]
// Check Pivot Q
def createPivotQ( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  print(ratingArr.length)
  for( r <- ratings ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
    //   println("TREEEE")
      res = r.getRate()
    }
  }
  return res
}
var Q = new ListBuffer[Array[Double]]()
for( i <- users.to[Array].reduceLeft(_ min _) to users.to[Array].reduceLeft(_ max _) ){
  var lineArr = new Array[Double](n)
  for( j <- movies.to[Array].reduceLeft(_ min _) to movies.to[Array].reduceLeft(_ max _) ){
    lineArr(j-movies.to[Array].reduceLeft(_ min _)) = createPivotQ( i, j )
  }
  Q += lineArr
}
// println("###############")
// print_readable(Q)
// println("###############")
// Check Pivot W
def createPivotW( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratings ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = 1.0
    }
  }
  return res
}
var W = new ListBuffer[Array[Double]]()
for( i <- users.to[Array].reduceLeft(_ min _) to users.to[Array].reduceLeft(_ max _) ){
  var lineArr = new Array[Double](n)
  for( j <- movies.to[Array].reduceLeft(_ min _) to movies.to[Array].reduceLeft(_ max _) ){
    lineArr(j-movies.to[Array].reduceLeft(_ min _)) = createPivotW( i, j )
  }
  W += lineArr
}

// Parameter

// *** //

// Matrix X
var X = new ListBuffer[Array[Double]]()
for( i <- 0 to m - 1 ){
  var lineArr = new Array[Double](n_factors)
  for( j <- 0 to n_factors - 1 ){
    lineArr(j) = ( Math.random * 5 )
  }
  X += lineArr
}
// println("999999999999")
print_readable(X)
// Matrix 
var Y = new ListBuffer[Array[Double]]()
for( i <- 0 to n_factors - 1 ){
  var lineArr = new Array[Double](n)
  for( j <- 0 to  n - 1 ){
    lineArr(j) = ( Math.random * 5 )
  }
  Y += lineArr
}
// print_readable(Y)
// Transpose
// import scala.collection.mutable.ListBuffer
// val test = new ListBuffer[Array[Double]]()
// test += Array(4.0,7.0,2.0,1.0)
// test += Array(3.0,9.0,8.0,6.0)
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
// import scala.collection.mutable.ListBuffer
// val test1 = new ListBuffer[Array[Double]]()
// test1 += Array(1.0,0.0)
// test1 += Array(0.0,1.0)
// val test2 = new ListBuffer[Array[Double]]()
// test2 += Array(4.0,1.0)
// test2 += Array(2.0,2.0)
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
// import scala.collection.mutable.ListBuffer
import scala.util.control._
// val test1 = new ListBuffer[Array[Double]]()
// test1 += Array(7.0,2.0,1.0)
// test1 += Array(0.0,3.0,-1.0)
// test1 += Array(-3.0,4.0,-2.0)
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
    // print(arr1)
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
                            // print(j)
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
// import scala.collection.mutable.ListBuffer
// val test1 = new ListBuffer[Array[Double]]()
// test1 += Array(7.0,5.0)
// test1 += Array(3.0,-2.0)
// val test2 = new ListBuffer[Array[Double]]()
// test2 += Array(3.0)
// test2 += Array(22.0)
def solve( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    var in_arr1 = inverse(arr1)
    res = mult(inverse(arr1),arr2)
    return res
}
// Eye
import scala.collection.mutable.ListBuffer
def eyeStar( lambda: Double, n: Int): ListBuffer[Array[Double]] = {
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
def get_error( q: ListBuffer[Array[Double]], x: ListBuffer[Array[Double]], y: ListBuffer[Array[Double]], w: ListBuffer[Array[Double]] ): Double = {
    var xy = mult(x,y)
    var qxy = minus(q,xy)
    var wqxy = star(w,qxy)
    var wqxy2 = star(wqxy,wqxy)
    println("++++++")
    print_readable(x)
    // print(y)
    // println(xy)
    // println(sumR(wqxy2))
    println("++++++")
    return sumR(wqxy2)
}   
var errors = new ListBuffer[Double]()
// for( ii <- 0 to n_iterations - 1 ){
for( ii <- 0 to 5 - 1 ){
    println("============================")
    // print_readable(X)
    var x1 = plus( mult( Y, transe(Y) ) ,  eyeStar( lambda_, n_factors ) )
    // print_readable(x1)
    var x2 = mult( Y, transe(Q) )
    // print_readable(x2)
    var x3 = solve(x1,x2)
    // print_readable(x3)
    X = transe(x3)
    
    println("111111111111111")
    print_readable(X)
    // X = transe( solve( plus( mult( Y, transe(Y) ) ,  eyeStar( lambda_, n_factors ) ), mult( Y, transe(Q) ) ))
    var y1 = plus( mult( transe(X), X ) ,  eyeStar( lambda_, n_factors ) )
    var y2 = mult( transe(X), Q )
    var y3 = solve(y1,y2)
    Y = y3 
    // Y = solve(  plus( mult( transe(X), X ) ,  eyeStar( lambda_, n_factors ) ), mult( transe(X), Q ) )
    // if(ii%100==0){
    println(ii+"iteration is completed")
    // }
    var error = get_error(Q, X, Y, W)
    println("Error of rated movies: "+error)
    errors += error
    println("============================")
}
// var Q_hat = mult( X, Y )
// val movies_id = new ListBuffer[Int]()
// val movies_title = new ListBuffer[String]()
// class Movie( mid: Int, mti: String){
//     movies_id += mid
//     movies_title += mti
//     def getMovieId(): Int = mid
//     def getMovieTitle(): String = mti
//     override def toString: String = s" movie_id: $mid , movie_title: $mti"  
// }
// import scala.io.Source
// import scala.collection.mutable.ListBuffer
// val filename2 = "data/movies.csv"
// var movieZ = new ListBuffer[Movie]()
// var numLine1: Int = 0
// for (line <- Source.fromFile(filename2).getLines()) {
//   numLine1 += 1
//   if(numLine1>1) {
//     val movie_id = line.split(",")(0).toInt 
//     val movie_title = line.split(",")(1).toString
//     var temp = new Movie(movie_id,movie_title)
//     movieZ += temp

//   }
// }
// // Min
// def minimum( arr1: ListBuffer[Array[Double]] ): Double = {
//     var res = arr1.apply(0).apply(0)
//     for( i <- 0 to arr1.length - 1 ){
//         for( j <- 0 to arr1.apply(0).length - 1 ){
//             if( arr1.apply(i).apply(j) < res ){
//                 res = arr1.apply(i).apply(j)
//             }
//         }
//     }
//     return res 
// }
// // Max
// def maximum( arr1: ListBuffer[Array[Double]] ): Double = {
//     var res = arr1.apply(0).apply(0)
//     for( i <- 0 to arr1.length - 1 ){
//         for( j <- 0 to arr1.apply(0).length - 1 ){
//             if( arr1.apply(i).apply(j) > res ){
//                 res = arr1.apply(i).apply(j)
//             }
//         }
//     }
//     return res 
// }
// // minus_const
// def minus_const( const: Double, arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
//     var res = new ListBuffer[Array[Double]]
//     for( i <- 0 to arr1.length -1 ){
//         var lineArr = new Array[Double](arr1.apply(0).length)
//         for( j <- 0 to arr1.apply(0).length - 1 ){
//             lineArr(j) = arr1.apply(i).apply(j) - const
//         }
//         res += lineArr
//     }
//     return res
// }
// // const_minus
// def const_minus( const: Double, arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
//     var res = new ListBuffer[Array[Double]]
//     for( i <- 0 to arr1.length -1 ){
//         var lineArr = new Array[Double](arr1.apply(0).length)
//         for( j <- 0 to arr1.apply(0).length - 1 ){
//             lineArr(j) = const - arr1.apply(i).apply(j)
//         }
//         res += lineArr
//     }
//     return res
// }
// // star_const
// def star_const( const: Double, arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
//     var res = new ListBuffer[Array[Double]]
//     for( i <- 0 to arr1.length -1 ){
//         var lineArr = new Array[Double](arr1.apply(0).length)
//         for( j <- 0 to arr1.apply(0).length - 1 ){
//             lineArr(j) = arr1.apply(i).apply(j) * const
//         }
//         res += lineArr
//     }
//     return res
// }
// // argmax
// import scala.collection.mutable.ListBuffer
// val test1 = new ListBuffer[Array[Double]]()
// test1 += Array(0.0,1.0,2.0)
// test1 += Array(3.0,4.0,5.0)
// def indiceMax( arr1: Array[Double] ): Int = {
//     var index = 0
//     var max = arr1.apply(0)
//     for( i <- 0 to arr1.length - 1 ){
//         if(arr1.apply(i)>max){
//             max = arr1.apply(i)
//             index = i
//         }
//     }   
//     return index
// }
// def argmax( arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
//     var res = new ListBuffer[Array[Double]]
//     var lineArr = new Array[Double](arr1.length)
//     for( j <- 0 to arr1.length - 1 ){
//         lineArr(j) = indiceMax(arr1.apply(j))
//     }
//     res += lineArr
//     return res
// }
// def print_recommendation( W: ListBuffer[Array[Double]], Q: ListBuffer[Array[Double]], Qhat: ListBuffer[Array[Double]]){
//     var Qhat1 = minus_const(minimum(Qhat),Qhat)
//     var Qhat2 = star_const(5.0/maximum(Qhat1),Qhat1)
//     var m_id = argmax( star( const_minus( 5, Qhat2 ), W) )
//     for( i <- 1 to m ){
//         for( j <- m_id ){
//             println(i+"--"+j)
//         }
//     }
// }
// print_recommendation( W, Q , Q_hat)


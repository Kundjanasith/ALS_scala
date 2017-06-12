import scala.io.Source
import scala.util.control._


val filename = "data/rat1.csv"
val file_size = Source.fromFile(filename).getLines().length
val users = new Array[Int](file_size)
val movies = new Array[Int](file_size)
val ratings = new Array[Double](file_size)

println("Start read file ")
var indexArr = 0
for (line <- Source.fromFile(filename).getLines()) {
    println(line)
    users(indexArr) = line.split(",")(0).toInt
    movies(indexArr) = line.split(",")(1).toInt
    ratings(indexArr) = line.split(",")(2).toDouble
    indexArr = indexArr + 1
}

val lambda_ = 0.1
val n_factors = 10
val u_min = users.reduceLeft(_ min _)
val u_max = users.reduceLeft(_ max _)
val u = ( u_max - u_min ) + 1
val m_min = movies.reduceLeft(_ min _)
val m_max = movies.reduceLeft(_ max _)
val m = ( m_max - m_min ) + 1
val n_iterations = 10

// Check Pivot Q
def createPivotQ( uid: Int, mid: Int ): Double = {
  println("Create Pivot Q "+uid+"+"+mid)
  for( i <- 0 to file_size - 1 ){
      if(users(i)==uid&&movies(i)==mid) { return ratings(i) }
  }
  return 0.0
}
// Check Pivot W
def createPivotW( uid: Int, mid: Int ): Double = {
  println("Create Pivot W "+uid+"+"+mid)
  for( i <- 0 to file_size - 1 ){
      if(users(i)==uid&&movies(i)==mid) { return 1.0 }
  }
  return 0.0
}
val W = new Array[Array[Double]](u)
val Q = new Array[Array[Double]](u)
for( i <- 0 to u-1 ){
  var lineArrW = new Array[Double](m)
  var lineArrQ = new Array[Double](m)
  for( j <- 0 to m-1 ){
    lineArrW(j) = createPivotW(i+u_min,j+m_min)
    lineArrQ(j) = createPivotQ(i+u_min,j+m_min)
  }
  W(i) = lineArrW
  Q(i) = lineArrQ
}
// Matrix X
var X = new Array[Array[Double]](u).map( y=> new Array[Double](n_factors).map(x=>Math.random*5) )
// Matrix Y
var Y = new Array[Array[Double]](n_factors).map( y=> new Array[Double](m).map(x=>Math.random*5) )
// Mult
def multT[A](a: Array[Array[A]], b: Array[Array[A]])(implicit n: Numeric[A]) = { 
  import n._
  for (row <- a)
  yield for(col <- b.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
}
def mult( arr1: Array[Array[Double]], arr2: Array[Array[Double]] ): Array[Array[Double]] = {
    var temp = multT(arr1,arr2)
    val res = new Array[Array[Double]](temp.length)
    for( i <- 0 to temp.length - 1 ){
        res(i) = temp.apply(i).to[Array]
    }
    return res
}
// Inverse
def inverse( arr1: Array[Array[Double]] ): Array[Array[Double]] = {
    var I = new Array[Array[Double]](arr1.length)
    var C = new Array[Array[Double]](arr1.length)
    if(arr1.length!=arr1.apply(0).length) { return null }
    for( i <- 0 to arr1.length - 1 ){
        var lineArrI = new Array[Double](arr1.length)
        var lineArrC = new Array[Double](arr1.length)
        for( j <- 0 to arr1.length - 1 ){
            lineArrI(j) = if (i==j) 1 else 0
            lineArrC(j) = arr1.apply(i).apply(j)
        }
        I(i) = lineArrI
        C(i) = lineArrC
    }
    for( i <- 0 to arr1.length - 1 ){
        var e = C.apply(i).apply(i)
        if(e==0){
            var loop = new Breaks;
            loop.breakable{
                for( ii <- i+1 to arr1.length-1 ){
                    if(C.apply(ii).apply(i)!=0){
                        for( j <- 0 to arr1.length-1 ){
                            e = C.apply(i).apply(j)
                            C(i)(j) = C.apply(ii).apply(j)
                            C(ii)(j) = e
                            e = I.apply(i).apply(j)
                            I(i)(j) = I.apply(ii).apply(j)
                            I(ii)(j) = e
                        }
                        loop.break
                    }
                }
            }
            e = C.apply(i).apply(i)
            if(e==0){ return null }
        }
        for( j <- 0 to arr1.length-1 ){
            C(i)(j) = C.apply(i).apply(j)/e
            I(i)(j) = I.apply(i).apply(j)/e
        }
        for( ii <- 0 to arr1.length-1 ){
            if(ii!=i){
                e = C.apply(ii).apply(i)
                for( j <- 0 to arr1.length-1 ){
                    C(ii)(j) = C.apply(ii).apply(j) - e*C.apply(i).apply(j)
                    I(ii)(j) = I.apply(ii).apply(j) - e*I.apply(i).apply(j)
                }
            }
        }
    }
    return I
}
def solve( arr1: Array[Array[Double]], arr2: Array[Array[Double]] ): Array[Array[Double]] = {
    return mult(inverse(arr1),arr2)
}
// Eye
def eyeStar( lambda: Double, n: Int): Array[Array[Double]] = {
    var res = new Array[Array[Double]](n)
    for( i <- 0 to n-1 ){
       var lineArr = new Array[Double](n)
       for( j <- 0 to n-1 ){
            lineArr(j) = if (i==j) 1 * lambda else 0
       }
       res(i) = lineArr
    }
    return res
}
// Plus
def plus( arr1: Array[Array[Double]], arr2: Array[Array[Double]] ): Array[Array[Double]] = {
    var res = new Array[Array[Double]](arr1.length)
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) + arr2.apply(i).apply(j)
        }
        res(i) = lineArr
    }
    return res 
}
// Minus
def minus( arr1: Array[Array[Double]], arr2: Array[Array[Double]] ): Array[Array[Double]] = {
    var res = new Array[Array[Double]](arr1.length)
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) - arr2.apply(i).apply(j)
        }
        res(i) = lineArr
    }
    return res 
}
// Star
def star( arr1: Array[Array[Double]], arr2: Array[Array[Double]] ): Array[Array[Double]] = {
    var res = new Array[Array[Double]](arr1.length)
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) * arr2.apply(i).apply(j)
        }
        res(i) = lineArr
    }
    return res 
}
// sumR
def sumR( arr: Array[Array[Double]]): Double = {
    var res = 0.0
    arr.foreach( x => { res += x.reduceLeft(_ + _) })
    return res
}
// Get error
def get_error( q: Array[Array[Double]], x: Array[Array[Double]], y: Array[Array[Double]], w: Array[Array[Double]] ): Double = {
    var xy = mult(x,y)
    var qxy = minus(q,xy)
    var wqxy = star(w,qxy)
    var wqxy2 = star(wqxy,wqxy)
    return sumR(wqxy2)
}   
for( ii <- 0 to n_iterations - 1 ){
    println("===================================================")
    var x1 = plus( mult( Y, Y.transpose ) ,  eyeStar( lambda_, n_factors ) )
    var x2 = mult( Y, Q.transpose )
    var x3 = solve(x1,x2)
    X = x3.transpose
    var y1 = plus( mult( X.transpose, X ) ,  eyeStar( lambda_, n_factors ) )
    var y2 = mult( X.transpose, Q )
    var y3 = solve(y1,y2)
    Y = y3 
    println(ii+" iteration is completed")
    var error = get_error(Q, X, Y, W)
    println("Error of rated movies: "+error)
    println("===================================================")
}
var Q_hat = mult( X, Y )
val filename1 = "data/movies1.csv"
val file1_size = Source.fromFile(filename1).getLines().length
val movies_id = new Array[Int](file1_size)
val movies_title = new Array[String](file1_size)
var indexArr1 = 0
for (line <- Source.fromFile(filename1).getLines()) {
    println(line)
    movies_id(indexArr1) = line.split(",")(0).toInt
    movies_title(indexArr1) = line.split(",")(1).toString
    indexArr1 = indexArr1 + 1
}
// Min
def minimum( arr1: Array[Array[Double]] ): Double = {
    var temp = new Array[Double](arr1.length)
    for( i <- 0 to arr1.length-1 ){
        temp(i) = arr1.apply(i).reduceLeft(_ min _)
    }
    return temp.reduceLeft(_ min _)
}
// Max
def maximum( arr1: Array[Array[Double]] ): Double = {
    var temp = new Array[Double](arr1.length)
    for( i <- 0 to arr1.length-1 ){
        temp(i) = arr1.apply(i).reduceLeft(_ max _)
    }
    return temp.reduceLeft(_ max _)
}
// minus_const
def minus_const( const: Double, arr1: Array[Array[Double]] ): Array[Array[Double]] = {
    var res = arr1.map( x => x.map( y => y-const ))
    return res
}
// star_const
def star_const( const: Double, arr1: Array[Array[Double]] ): Array[Array[Double]] = {
    var res = arr1.map( x => x.map( y => y*const ))
    return res
}
def map_movie_title( mid: Int ): String = {
    var res = ""
    for( i <- 0 to movies_id.length ){
        if(i+1==mid){
            res = movies_title(i)
        }
    }
    return res
}
def print_movie_rank( arr: Array[Double]){
    var movie_index = new Array[Int](5)
    var rate = new Array[Double](5)
    for( i <- 0 to 4 ){  
        var rat = arr.reduceLeft(_ max _)
        val loop1 = new Breaks;
        loop1.breakable {
            for( j <- 0 to arr.length - 1 ){              
                if(arr(j)==rat){
                    movie_index(i) = j + 1 
                    rate(i) = rat
                    arr(j) = 0
                    loop1.break
                }
            }
        }
    }
    for( i <- 0 to 4 ){
        println( movie_index(i)+":"+map_movie_title(movie_index(i))+"-"+rate(i))
    }
}
def print_recommendation( W: Array[Array[Double]], Q: Array[Array[Double]], Qhat: Array[Array[Double]]){
    var Qhat1 = minus_const(minimum(Qhat),Qhat)
    var Qhat2 = star_const(5.0/maximum(Qhat1),Qhat1)
    println("Result : ")
    for( i <- u_min to u_max ){
        println("############################")
        println("[ User : "+i+" ]")
        print_movie_rank(Qhat2.apply(i-1))
        println("############################")
    }
}
print_recommendation( W, Q , Q_hat)
System.exit(0)
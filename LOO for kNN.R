

avg <- function(x)
  
{
  sum(x) / length(x)
  
}


colors <-
  c("setosa" = "red",
    "versicolor" = "green",
    "virginica" = "blue")



plot(iris[, 3:4],
     pch = 21,
     bg = colors[iris$Species],
     col = colors[iris$Species])

euclideanDistance <- function(u, v) {
  sqrt(sum((u - v) ^ 2))
}



sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
  {
    l <- dim(xl)[1] 
    n <- dim(xl)[2] - 1
    
    distances <- matrix(NA, l, 2) 
    for (i in 1:l) {
        distances[i,] <- c(i, metricFunction(xl[i, 1:n], z))
      }
    
    orderedXl <- xl[order(distances[, 2]),] 
    return (orderedXl)
    
  }

kNN <- function(xl, z, k) {
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance) 
  n <- dim(orderedXl)[2] - 1 
  classes <-orderedXl[1:k, n + 1] 
  counts <- table(classes)
  class <- names(which.max(counts)) 
  return (class)
}
 

#z <-c(2.7, 1) 
#xl <-iris[, 3:5] 
#class <-kNN(xl, z, k = 6) 
#points(z[1], z[2], pch = 22, bg = colors[class], col = colors[class], asp = 1, lwd = 5)

kNN <- function(){
  vec = c(seq(1, 150)) 
  for (k in 1:150) {
    cnt <- 0
    
    for (i in 1:150) {
      x_el <- c(iris[i, 3], iris[i, 4])
      x_sample <- iris[-i, 3:5] 
      class <- kNN(x_sample, x_el, k) 
      if (iris[i, 5] != class) {
          cnt = cnt + 1
        }
    } 
    vec[k] <- cnt / 150
    print(vec[k])
  }
}
plot(
  seq(1, 150), vec,
  type="p", col="red", bg= "red", pch = 21, ylab = "Error", xlab = "k", main = "LOO for kN"
)




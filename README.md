# SMPR


# v <- iris[, 3:4]
# p <- c(3, 1)
# 
# avg <- function(x)
# {
#   sum(x)/length(x)
# }
# 
# ax <- avg(iris[iris$Species == "setosa", 3])
# ay <- avg(iris[iris$Species == "setosa", 4])
# bx <- avg(iris[iris$Species == "versicolor", 3])
# by <- avg(iris[iris$Species == "versicolor", 4])
# cx <- avg(iris[iris$Species == "virginica", 3])
# cy <- avg(iris[iris$Species == "virginica", 4])
# 
# colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
# plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
# points(ax, ay, pch = 20, col = "black")
# points(bx, by, pch = 20, col = "black")
# points(cx, cy, pch = 20, col = "black")
# points(p, pch = 20, col = "yellow", lwd = 9)
# 
# dist <- function(u, v)
# {
#   sqrt(sum((u - v)^2))
# }
# a <- dist(c(ax, ay), p)
# b <- dist(c(bx, by), p)
# c <- dist(c(cx, cy), p)
# min(c(a, b, c))


euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}


sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1

  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }

  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

kNN <- function(xl, z, k)
{

  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
 
  class <- names(which.max(counts))
  return (class)
}


colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
     = colors[iris$Species], asp = 1)


z <- c(2.7, 1)
xl <- iris[, 3:5 ]
class <- kNN(xl, z, k=6)
points(z[1], z[2], pch = 22, bg = colors[class], col = colors[class], asp = 1, lwd = 5)

axis_x <-c(min(iris[, 3]), max(iris[, 3])) 
axis_y <-c(min(iris[, 4]), max(iris[, 4])) 


vec = c(seq(1,50))
for (k in 2:49){
  cnt <- 0
  
  for (i in 1:length(iris[,3])){
    x_el <- c(iris[i, 3], iris[i, 4])
    x_sample <- iris[-i, 3:5]
    class <- kNN(x_sample, x_el, k)
    if (iris[i,5] == class){
      cnt = cnt + 1
    }
  }
  vec[k-1] <- cnt/length(iris[,3]) 
}

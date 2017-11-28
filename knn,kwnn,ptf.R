

avg <- function(x)
  
{
  sum(x) / length(x)
  
}


#colors <-c("setosa" = "red","versicolor" = "green","virginica" = "blue")



#plot(iris[, 3:4],    pch = 21,bg = colors[iris$Species],col = colors[iris$Species])

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
  classes <- orderedXl[1:k, n + 1] 
  counts <- table(classes)
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  
  for (i in seq(1:k)){
    
    m[[classes[i]]] <- m[[classes[i]]] + 1
    
  }
 
  class <- names(which.max(m)) 
  return (class)
}

kwNN <- function(xl, z, k, q) {
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance) 
  n <- dim(orderedXl)[2] - 1 
  classes <- orderedXl[1:k, n + 1] 
  counts <- table(classes)
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in seq(1:k)){
    w <- q ^ i
    m[[classes[i]]] <- m[[classes[i]]] + w
    
  }

  class <- names(which.max(m)) 
  #print(m)
  
  return (class)
}


ptnF <- function(xl, z, y, h) {
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance) 
  n <- dim(orderedXl)[2] - 1
  classes <-orderedXl[1:150, n+1]
 
  counts <- table(classes)
  m <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in seq(1:149)){
    r <- euclideanDistance(xl[i, 1:2], z) / h
    
    w <- y[i] * (1 / (r*r+1))
    m[[classes[i]]] <- m[[classes[i]]] + w
  }
  
  class <- names(which.max(m)) 
  #print(m)
  
  return (class)
}

all_set <- iris

howError <- function(xl, y, h){
  cnt <- 0
  for (i in 1:dim(all_set)[1]){
    x_el <- c(all_set[i, 3], all_set[i, 4])
    x_sample <- all_set[-i, 3:5] 
    class <- ptnF(x_sample, x_el, y, 2.78)
    if (all_set[i, 5] != class) {
      cnt <- cnt + 1
    }
  }
  return (cnt)
}

LOOPtnF <- function(){
  j <- 1

    cnt <- 0
    gammas <- seq(1, dim(all_set)[1])*0
    
    err <- 5
    while (err >=  5){
      err <- 0
      for (i in 1:149) {
        x_el <-c(iris[i, 3], iris[i, 4]) 
        x_sample <- iris[-i, 3:5] 
        class <- ptnF(x_sample, x_el, gammas, 1.7)
        if (iris[i, 5] != class) {
          gammas[i] <- gammas[i] + 1
          err <- err + 1
        }
      }
      #err <- howError(all_set, gammas, 2.78)
      print(j)
      j <- j + 1
      print("error")
      print(err)
      print(gammas)
    }
   
    j <- j + 1

 
}


par(mar = rep(2, 4))


k <- 5
segment <- seq(from = 0.0, to = 1.5, by = 0.05)

LOO <- function(classificator){
  j <- 1
  
  vec <- seq(1, length(segment))
  
  for (q in segment) {
      cnt <- 0
      for (i in 1:150) {
          x_el <-c(iris[i, 3], iris[i, 4]) 
          x_sample <- iris[-i, 3:5]  
          class <- classificator(x_sample, x_el, k, q)
          
          if (iris[i, 5] != class) { 
            cnt <- cnt + 1
          }
      }
        
      vec[j] <- cnt / dim(all_set)[1]
      print(j)
      
      print(vec[j])
      j <- j + 1
  
    }
  return (vec)
}

#loo_kwnn <- LOO(kwNN)
loo_ptnF <- LOOPtnF()








library('ggplot2')

#task 9

main()

main <- function() {
  #cat(sprintf("Gay boy #%d", ))
  
  pressure <- c(0.164, 0.328, 0.656, 0.984, 1.312, 1.640) #H
  expiration <- c(0.448, 0.432, 0.421, 0.417, 0.414, 0.412) #n
  
  df <- data.frame(
    x = pressure,
    y = expiration
  )
  
  ggplot(data = df, aes(x = pressure, y = expiration)) +
    geom_line() +
    geom_point() 
  
  m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 90), nrow = 3, ncol = 3, byrow = T)
  x <- c(1, 2, 3)
  print(m)
  SolveGause(m, 3, x)
  print(m)
  
}

SolveGause <- function(mat, size, vec){
  tempvec <- vec
  tmpmatrix <- mat
  for(i in 1 : size){
    coefficients <- rep(0, size)
    for(i_n in i : size){
      coefficients[i_n] = tmpmatrix[i_n, i] / tmpmatrix[i, i]
    }
    #print(coefficients)
    j = i + 1
    for(i_m in i : size){
      for(i_n in j : size){
        if(j > size){
          break
        }
        #cat(sprintf("Gay boy #%d\n", i_n))
        #cat(sprintf("Gay girl #%d\n", i_m))
        tmp <- tmpmatrix[i_n, i_m] - (tmpmatrix[i, i_m] * coefficients[i_n])
        tmpmatrix[i_n, i_m] <- tmp
      }
    }
    print(tmpmatrix)
  }
}

m <- matrix(c(10, 20, 30, 40), nrow = 2, ncol = 2, byrow = T)
x <- c(1, 2)
SolveGause(m, 2, x)
m
m[1, 2]


y <- c(x, 4, 5, 6, 0)
u <- rep(1, 7)

z <- c(2)

length(c)

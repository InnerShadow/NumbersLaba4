
library('ggplot2')

#task 9

main()

main <- function() {
  #cat(sprintf("Gay boy #%d", ))
  #cat(sprintf("Gay boy #%d\n", i_n))
  #cat(sprintf("Gay girl #%d\n", i_m))
  
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
  x <- c(1, 2, 5)
  Print(m, x)
  SolveGause(m, 3, x)
  Print(m, x)
  
}

SolveGause <- function(mat, size, vec){
  tmpvec <- vec
  tmpmatrix <- mat
  for(i in 1 : size){
    
    coefficients <- rep(0, size)
    for(i_n in i : size){
      coefficients[i_n] = tmpmatrix[i_n, i] / tmpmatrix[i, i]
    }
    
    j = i + 1
    for(i_m in i : size){
      for(i_n in j : size){
        if(j > size){
          break
        }
        tmp <- tmpmatrix[i_n, i_m] - (tmpmatrix[i, i_m] * coefficients[i_n])
        tmpmatrix[i_n, i_m] <- tmp
      }
    }

    j = i + 1
    for(i_n in j : size){
      if(j > size){
        break
      }
      tmp <- tmpvec[i_n] - (tmpvec[i] * coefficients[i_n])
      tmpvec[i_n] <- tmp
    }
    
    tmp <- tmpvec[i] / tmpmatrix[i, i]
    tmpvec[i] <- tmp
    
    for(i_n in size : i){
      tmp <- tmpmatrix[i, i_n] / tmpmatrix[i, i]
      tmpmatrix[i, i_n] <- tmp
    }
  }
  
  Print(tmpmatrix, tmpvec)
  
  resualt <- rep(0, size)
  for(i in size : 1){
    value <- tmpvec[i]
    for(i_n in size : 1){
      tmp <- value - (tmpmatrix[i, i_n] * resualt[i_n])
      value <- tmp
    }
    resualt[i] <- value / tmpmatrix[i, i]
  }
  print(resualt)

}

Print <- function(mtr, vec){
  print(mtr)
  print(vec)
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

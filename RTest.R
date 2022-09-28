
library('ggplot2')

#task 9
#n = (a / H) + b

main()

main <- function() {
  N <- c(6)
  m <- c(5)
  
  pressure <- c(0.164, 0.328, 0.656, 0.984, 1.312, 1.640) #H
  expiration <- c(0.448, 0.432, 0.421, 0.417, 0.414, 0.412) #n
  
  df <- data.frame(
    x = pressure,
    y = expiration
  )
  
  ggplot(data = df, aes(x = pressure, y = expiration)) +
    geom_line() + geom_point() 
  
  
  POWERX <- rep(0, ((m * 2) + 1))
  POWERX[1] <- m + 1
  for(i in 1 : (m * 2)){
    value <- 0
    for(j in 1 : N){
      
      value <- value + pressure[j] ^ i
    }
    POWERX[(i + 1)] <- value
  }
  cat("\nPower: ", POWERX, "\n")
  
  
  SUM41 <- matrix(rep(0, (m + 1) ^ 2), nrow = (m + 1), ncol = (m + 1), byrow = T)
  SUM41[1, 1] = N
  for(l in 1 : (m + 1)){
    for(j in 1 : (m + 1)){
      if(l == 1 && j == 1){
        next
      }
      
      k <- l + j - 1 # was - 2
      SUM41[l, j] <- POWERX[k]
    }
  }
  cat("\nMatrix:\n ")
  print(SUM41)
  
  
  PRAW <- rep(0, (m + 1))
  for(l in 1 : (m + 1)){
    value <- 0
    for(i in 1 : N){
      value <- value + expiration[i] * pressure[i] ^ (l - 1)
    }
    PRAW[l] <- value
  }
  cat("\nPRAW: ", PRAW, "\n")
  
  
  SOLUTION <- SolveGause(SUM41, m + 1, PRAW)
  cat("\nSolution: ", SOLUTION, "\n")
  
  
  S <- 0
  for(i in 1 : N){
    value <- expiration[i]
    for(j in 1 : (m + 1)){
      value <- value - (SOLUTION[j] * (pressure[i] ^ (j - 1)))
    }
    value <- value ^ 2
    S <- S + value
  }
  S <- S * (1 / (N - m))
  S <- S ^ 0.5
  cat("\nS: ", S, "\n")
  
  
  X0 <- as.double(readline("Enter X0: "))
  Xn <- as.double(readline("Ener Xn: "))
  Beatings <- as.integer(readline("Enter number of beatings: "))
  step <- (Xn - X0) / Beatings
  XVec <- rep(0, Beatings)
  YVec <- rep(0, Beatings)
  i <- 1
  
  while(X0 <= Xn){
    XVec[i] <- X0
    YVec[i] <- GetConut(X0, SOLUTION, m)
    i <- i + 1
    X0 <- X0 + step
  }
  
  finaldata <- data.frame(
    x = XVec,
    y = YVec,
    col = YVec
  )
  
  ggplot(data = finaldata, aes(x = x, y = y, col = col)) +
    geom_line() + geom_point() 
  
}









SolveGause <- function(mat, size, vec){
  tmpvec <- vec
  tmpmatrix <- mat
  for(i in 1 : size){
    
    coefficients <- rep(0, size)
    for(i_n in i : size){
      coefficients[i_n] <- tmpmatrix[i_n, i] / tmpmatrix[i, i]
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
  
  result <- rep(0, size)
  for(i in size : 1){
    value <- tmpvec[i]
    for(i_n in size : 1){
      tmp <- value - (tmpmatrix[i, i_n] * result[i_n])
      value <- tmp
    }
    result[i] <- value / tmpmatrix[i, i]
  }

  return(result)
}


GetConut <- function(x, coefficients, m){
  value <- 0
  for(i in 1 : (m + 1)){
    value <- value + (coefficients[i] * (x ^ (i - 1)))
  }
  return (value)
}


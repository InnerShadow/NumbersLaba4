
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
  
}

m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = T)
m[1, 2]

x <- c(1, 2)
y <- c(x, 4, 5, 6, 0)
u <- rep(1, 7)

z <- c(2)

length(c)

S <- matrix(c(1,0,0,1),2,2)

mt1 <- c(-1,1)
mt2 <- c(2,4)
mt3 <- c(-2,2)
n1 <- 30
n2 <- 30
n3 <- 30
n <- n1 + n2 +n3
g <- 3

X1 <- mvrnorm(n1, mt1, S)
X2 <- mvrnorm(n2, mt2, S)
X3 <- mvrnorm(n3, mt3, S)
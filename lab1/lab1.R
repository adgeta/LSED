rm(list=ls())
library(MASS)
library(scatterplot3d)

X1 <- data.frame(x = c(2,2,2,1,3), y = c(2,1,3,2,2))
X2 <- data.frame(x = c(6,6,6,5,7), y = c(0,1,-1,0,0))

m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)

S1 <- cov(X1)
S2 <- cov(X2)

n1 <- nrow(X1)
n2 <- nrow(X2)
n <- n1 + n2

W <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n - 2)

a <- ginv(W) %*% (m2 - m1) 

plot(X1, xlim = c(-2,7), ylim = c(-2,4), pch = 21, col = "#AAAAAA", bg = "#EEEEEE", cex = 3, xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 21, col = "#AAAAAA", cex = 3, bg = "#EEEEEE")
text(X1, "1", col = "blue", font = 2)
text(X2, "2", col = "orange", font = 2)
b <- 0.5 * t(a) %*% (m1 + m2)
abline(0, a[2] / a[1], col = "red", lty = 3)
abline(b / a[2], -a[1] / a[2], col = "red", lwd = 2)

S <- matrix(c(1,0,0,1),2,2)

mt1 <- c(2,2)
mt2 <- c(6,0)
n1 <- 60
n2 <- 60
n <- n1 + n2

X1 <- mvrnorm(n1, mt1, S)
X2 <- mvrnorm(n2, mt2, S) 

plot(X1, ylim = c(-5,5), xlim = c(-2,10), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 19, col = "orange") 

n1 <- nrow(X1)
n2 <- nrow(X2)
n <- n1 + n2

W <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n - 2)

a <- ginv(W) %*% (mt2 - mt1) 

b <- 0.5 * t(a) %*% (mt1 + mt2)
abline(0, a[2] / a[1], col = "red", lty = 2)
abline(b / a[2], -a[1] / a[2], col = "red", lwd = 2) 

X1 <- data.frame(x = c(2, 2, 2, 1, 3, 2, 2), y = c(2, 1, 3, 2, 2, 2, 2), z = c(2, 2, 2, 2, 2, 1, 3))
X2 <- data.frame(x = c(4, 4, 4, 3, 5, 4, 4), y = c(4, 3, 5, 4, 4, 4, 4), z = c(4, 4, 4, 4, 4, 3, 5)) 

# Wyznaczenie wartości średnich
m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)

# Wyznaczenie macierzy kowariancji
S1 <- cov(X1)
S2 <- cov(X2)

# Liczba elementów klas 1 i 2 oraz ich całkowitej liczba
n1 <- nrow(X1)
n2 <- nrow(X2)
n <- n1 + n2

# Wyznaczenie macierzy zmienności wewnątrzgrupowej W
W <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n - 2)

# Wyznaczenie wektora a
a <- ginv(W) %*% (m2 - m1)

# Wyznaczanie wyrazu wolnego
b <- -0.5 * t(a) %*% (m1 + m2)

# Rysowanie punktów klasy 1
s3d <- scatterplot3d(X1, pch = 21, color = "#AAAAAA", bg = "#EEEEEE", cex.symbols = 3, xlim = c(0, 5), ylim = c(0, 5), zlim = c(0, 5), angle = 123)

# Rzutowanie współprzędnych punktów z 3D na 2D
X1.coords <- s3d$xyz.convert(X1)
X2.coords <- s3d$xyz.convert(X2)

# Tekst dla punktów klasy 1
text(X1.coords, "1", col = "blue", font = 2)

# Kreslenie hiperplaszczyzny
s3d$plane3d(-b / a[3], -a[1] / a[3], -a[2] / a[3], draw_lines = F, draw_polygon = T, polygon_args = list(col = rgb(1, 1, 0, 0.4)))

# Rysowanie punktów klasy 2
s3d$points3d(X2, pch = 21, col = "#AAAAAA", bg = "#EEEEEE", cex = 3)

# Tekst dla punktów klasy 2
text(X2.coords, "2", col = "orange", font = 2) 


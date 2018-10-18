
D <- diag(c(2,2))

I <- diag(c(1,1))

A <- matrix(c(3/9,5/9,4/13,6/13), ncol=2)
A <- matrix(c(3/19,5/19,4/23,6/23), ncol=2)

LI <- inv(I-A)

LI %*% D

Inp1 <- A %*% D

Inp2 <- A %*% A %*% D

Inp3 <- A %*% A %*% A %*% D

Inp4 <- A %*% A %*% A %*% A %*% D

D + Inp1 + Inp2 + Inp3 + Inp4

A1 <- A 

A2 <- A %*% A

A3 <- A %*% A %*% A 

A4 <- A %*% A %*% A %*% A 

A5 <- A %*% A %*% A %*% A %*% A

A6 <- A %*% A %*% A %*% A %*% A %*% A
A7 <- A %*% A %*% A %*% A %*% A %*% A %*% A



I + A1 + A2 + A3 + A4 + A5 + A6 + A 7
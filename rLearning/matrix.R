y <- matrix(1:20, nrow=5, ncol=4)
cells <- c(1, 26, 24, 68)
rnames <-c("R1", "R2")
cnames <-c("C1", "C2")
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rnames, cnames))
mymatrix
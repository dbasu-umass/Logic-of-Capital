# This code can be used to replicate the examples
# discussed in Chapter 7 of THE LOGIC OF CAPITAL
# This code is also printed in the appendix to
# Chapter 7 in the book


# ------------------------------------------------ #
# ------------ Standard Interpretation ----------- #


# ----- Quantities that are given ------- #

# -- Input-output matrix
A <- matrix(c(186/450, 12/450, 9/450,
              54/21, 6/21, 6/21, 
              30/60, 3/60, 15/60), 
            ncol = 3)

# -- Labour input vector
l <- c(18/450, 12/21, 30/60)

# -- Real wage bundle
b <- c(2, 0, 1/6)

# -- Net output
y <- c(180, 0, 30)

# Check the dimensions of vectors and matrix
# If number of columns of A is not equal
# to the length of the other three vectors
# do not proceed. Check data.
(dim(A))
(length(l))
(length(b))
(length(y))


# --------- Gross Output --------- #
# Create identity matrix
n <- ncol(A)
I <- diag(n)

# The "solve" function gives the inverse, 
# and %*% is used for matrix multiplication
# The result will be displayed on screen
(Q <- solve(I - A) %*% y)



# --------- Value System --------- #

# Vector of values
(lambda <- t(l)%*%solve(I - A))

# Value embodied in the net product ...
(lambda %*% y)

# Is equal to the total labour to produce gross output
(l%*%Q)

# Value of real wage bundle (value of labour power)
(vrb <- lambda %*% b)

# Rate of exploitation
(e <- (1/vrb)-1)



# --------- Price System: 1 --------- #
# Rate of profit calculations

# Maximum eigenvalue of A
jj_A <- eigen(A)$values
(lambda_mA <- max(jj_A))

# Maximal rate of profit
(R <- (1/lambda_mA)-1)

# Augmented input matrix
(M <- A + b%*%t(l))

# Maximum eigenvalue of M
jj_M <- eigen(M)$values
(lambda_mM <- max(jj_M))

# Uniform rate of profit
(r_e <- (1/lambda_mM)-1)


# --------- Price System: 2 --------- #
# Relative price calculations

# M1 matrix
(M1 <- I - (1/lambda_mM)*M)

# Pre-multiply M1 with a price vector
# Choose any two equations
# Solve for relative prices
# Here we solve in terms of p3
A1 <- M1[1:2,1:2]
b1 <- M1[3,1:2]
(p12 <- solve(t(A1),b1))

# Relative price vector in terms of p3
(p <- c(-p12,1))

# --------- Price System: 3 --------- #
# Using numeraire to close system

# Numeraire = third commodity, i.e. p3=1
(p <- c(-p12,1))

# Numeraire = nominal wage rate
# Equation capturing this numeraire:
# 2 * p_1 + 0.167 * p_3 = 1
# To solve for prices, we will
# replace one equation with the
# numeraire equation.

# Transpose of M1
(M2 <- t(M1))

# Create new matrix by appending
# vector of coefficients of numeraire equation
ncoef <- c(2, 0, 0.167)
(M3 <- rbind(M2[1:2,1:3],ncoef))

# Create new right hand side vector
(bn <- c(0,0,1))

# Solve: M3 * p = bn
# To get the vector of prices of production
(solve(M3,bn))


# --------- Price System: 4 --------- #
# Using invariance principles to close system

# -- Invariance principle 1
# Net output remains unchanged with transformation
# Equation: 6 * p_1 + 1 * p_3 = 2  

# Transpose of M1
(M2 <- t(M1))

# Create new matrix by appending
# vector of coefficients of numeraire equation
inv1coef <- c(6, 0, 1)
(M4 <- rbind(M2[1:2,1:3],inv1coef))

# Create new right hand side vector
(binv1 <- c(0,0,2))

# Solve: M4 * p = binv1
# To get the vector of prices of production
(solve(M4,binv1))


# -- Invariance principle 2
# Gross output remains unchanged with transformation
# Equation: 450*p_1 + 21*p_2 + 60*p_3 = 174.55  

# Transpose of M1
(M2 <- t(M1))

# Create new matrix by appending
# vector of coefficients of numeraire equation
inv2coef <- c(450, 21, 60)
(M5 <- rbind(M2[1:2,1:3],inv2coef))

# Create new right hand side vector
(binv2 <- c(0,0,174.55))

# Solve: M5 * p = binv2
# To get the vector of prices of production
(solve(M5,binv2))


# -- Invariance principle 3
# Real wage bundle remains unchanged with transformation
# Equation: 2*p_1 + + 0.167*p_3 = 0.52  

# Transpose of M1
(M2 <- t(M1))

# Create new matrix by appending
# vector of coefficients of numeraire equation
inv3coef <- c(2, 0, 0.167)
(M6 <- rbind(M2[1:2,1:3],inv3coef))

# Create new right hand side vector
(binv3 <- c(0,0,0.52))

# Solve: M6 * p = binv3
# To get the vector of prices of production
(solve(M6,binv3))


# -- Invariance principle 4
# Surplus value = profit

# Total surplus value
((1-vrb)*(l %*% Q))

# Total profit = r p MQ
# Pre-multiply the vector below with 
# the price vector to get total profit
(r_e*M%*%Q)

# Equation for invariance principle:
# 390*p1 + 21*p2 + 40*p3 = 323.67

# Transpose of M1
(M2 <- t(M1))

# Create new matrix by appending
# vector of coefficients of numeraire equation
inv4coef <- c(390, 21, 40)
(M7 <- rbind(M2[1:2,1:3],inv4coef))

# Create new right hand side vector
(binv4 <- c(0,0,323.67))

# Solve: M4 * p = binv1
# To get the vector of prices of production
(p <- solve(M7,binv4))


# ------------------------------------------- #
# ------------ New Interpretation ----------- #

# Key quantities that are given
# A = input output matrix
# l = labour input vector
# w = nominal wage rate
# v = value of labour power
# y = net output vector

# ----- Quantities that are given ------- #

# -- Input-output matrix
A <- matrix(c(186/450, 12/450, 9/450,
              54/21, 6/21, 6/21, 
              30/60, 3/60, 15/60), 
            ncol = 3)

# -- Labour input vector
(l <- c(18/450, 12/21, 30/60))

# -- Nominal wage rate
(w <- 1)

# -- Value of labour power
(v <- 1/3)

# -- Net output
y <- c(180, 0, 30)


# --------- Computed quantities --------- #

# -- Compute Gross Output
I <- diag(3)
(Q <- solve(I - A) %*% y)


# -- Maximum eigenvalue of A
jj_A <- eigen(A)$values
(lambda_mA <- max(jj_A))

# -- Maximal rate of profit
(R <- (1/lambda_mA)-1)


# -- Define Function
# The roots of this function will be 
# used to compute the uniform rate of profit
myfunc <- function(r2){
  # Given variables
  D2=(I-A)%*%Q
  l2=l
  A2=A
  L2=(l%*%Q)/v
  # Compute
  C2 = solve(I-(1+r2)*A2)
  E2 = C2%*%D2
  B2 = (1+r2)*l2%*%E2
  i2 = B2-L2
  return(i2)
}

# Find root to get uniform rate of profit
(r <- uniroot(myfunc,c(0,R-0.1))$root)

# Solve: p = (1+r)wl*[I-(1+r)A]^{-1}
# To get the vector of prices of production
(p <- (1+r)*w*l%*%(solve(I-(1+r)*A)))

# --- Calculating the MEV (monetary expression of value)
# Create identity matrix
n <- ncol(A)
I <- diag(n)

# Vector of values
(lambda <- t(l)%*%solve(I - A))

# MEV/MELT (monetary expression of value)
(melt <- (p%*%y)/(lambda %*%y))
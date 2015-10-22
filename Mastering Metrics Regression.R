### Mastering Metrics Regression #####
#####################################

## Set Workspace


## Load data ##
Table2.1 <- read.csv(file = "Table 2.1.csv") # load csv
Table2.1$Private <- c(1, 1, 0, 1, 0, 1, 1, 0, 0) #revisit to run apply "if=" function instead of manual entry
Table2.1$A <- c(1, 1, 1, 0, 0, 0, 0, 0, 0) #revisit to run apply function instead of manual entry
FiveStudents <- Table2.1[1:5,] #subset students to first five 


## Estimate Equation 2.3 from first five students in Table 2.1
## Long model...  Y_i = alpha + beta P_i + gamma A_i + e_i
## Income = alpha + Private College Dummy + A_i = dummy group A
## Why are we only using the first five students?

LongRegression <- lm(X1996.Earnings ~ Private + A, data = FiveStudents)
LongRegression

## Estimate short version of 2.3 (page 70)
## Short model... Y_i = alpha + beta P_i + e_i

ShortRegression <- lm(X1996.Earnings ~ Private, data = FiveStudents)
ShortRegression

## Regress ommitted variable A on private dummy school
OmmittedA <- lm(Private ~ A, data = FiveStudents)
OmmittedA



#### Excercise 2
# Estimated Test Scores = 689.47 - 3.41*STR - 1.62*AVGINC + 0.19*AVGINC*STR
# Summary stats: 
#         Mean SD
# AVGINC   15   7
# STR      20   2

# A. predicted when AVGINC=8 and STR =20
a <- 689.47 - (3.41*20) - (1.62*8) + (0.19*8*20)
a

# B. predicted when avginc=8 and str=22
b <- 689.47 - (3.41*22) - (1.62*8) + (0.19*8*22)
b

# C. predicted when avginc=15 and str=20
c <- 689.47 - (3.41*20) - (1.62*15) + (0.19*15*20)
c

# D. predicted when avginc=15 and str=22
d <- 689.47 - (3.41*22) - (1.62*15) + (0.19*15*22)
d

# E. predicted when avginc=22 and str=20
e <- 689.47 - (3.41*20) - (1.62*22) + (0.19*22*20)
e

# F. predicted when avginc=22 and str=22
f <- 689.47 - (3.41*22) - (1.62*22) + (0.19*22*22)
f

# G. subtract a from b
g <- b-a
g  # holding income constant, higher str decrease testscores

# H. subtract c from d
h <- d-c
h # holding income constant, higher stress dreceases testscores but at a lower rate than a lower income bracket

# I. subtract e from f
i <- f-e
i # the interaction term effect overrides the negative effects of STR and income. 

#J, Str...


## Derive B0+ B1*X + B2*X^2 with respect to x
#  d'x  = B1 + 2*B2*x

## Derive B0 + B1*X1 + B2*X2 + B3*X1*X2 with respect to X1
# d'x1 = B1 + B3*X2


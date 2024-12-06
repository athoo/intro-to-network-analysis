x <- 3         # Assignment
x              # Evaluate the expression and print result

y <- 4         # Assignment
y + 5          # Evaluation, y remains 4

z <- x + 17*y  # Assignment
z              # Evaluation

2==2  # Equality
2!=2  # Inequality

x <= y # less than or equal: "<", ">", and ">=" also work

# NA - missing or undefined data

5 + NA      # When used in an expression, the result is generally NA
is.na(5+NA) # Check if missing


# NULL - an empty object, e.g. a null/empty list
10 + NULL     # use returns an empty object (length zero)
is.null(NULL) # check if NULL

5/0
is.finite(5/0) # Check if a number is finite (it is not).

0/0
is.nan(0/0)

# vector
v1 <- c(1, 5, 11, 33)       # Numeric vector, length 4
v2 <- c("hello","world")    # Character vector, length 2 (a vector of strings)
v3 <- c(TRUE, TRUE, FALSE)  # Logical vector, same as c(T, T, F)

v4 <- c(v1,v2,v3,"boo")     # All elements turn into strings

v <- 1:7         # same as c(1,2,3,4,5,6,7)  
v <- rep(0, 77)  # repeat zero 77 times: v is a vector of 77 zeroes
v <- rep(1:3, times=2) # Repeat 1,2,3 twice  
v <- rep(1:10, each=2) # Repeat each element twice  
v <- seq(10,20,2) # sequence: numbers between 10 and 20, in jumps of 2  
v1 <- 1:5         # 1,2,3,4,5
v2 <- rep(1,5)    # 1,1,1,1,1 

length(v1)
length(v2)

v1 + v2      # Element-wise addition
v1 + 1       # Add 1 to each element
v1 * 2       # Multiply each element by 2
v1 + c(1,7)  # This doesn't work: (1,7) is a vector of different length

sum(v1)      # The sum of all elements
mean(v1)     # The average of all elements
sd(v1)       # The standard deviation
cor(v1,v1*5) # Correlation between v1 and v1*5 

v1 > 2       # Each element is compared to 2, returns logical vector
v1==v2       # Are corresponding elements equivalent, returns logical vector.
v1!=v2       # Are corresponding elements *not* equivalent? Same as !(v1==v2)
(v1>2) | (v2>0)   # | is the boolean OR, returns a vector.
(v1>2) & (v2>0)   # & is the boolean AND, returns a vector.
(v1>2) || (v2>0)  # || is the boolean OR, returns a single value
(v1>2) && (v2>0)  # && is the boolean AND, ditto

v1[3]             # third element of v1
v1[2:4]           # elements 2, 3, 4 of v1
v1[c(1,3)]        # elements 1 and 3 - note that your indexes are a vector
v1[c(T,T,F,F,F)]  # elements 1 and 2 - only the ones that are TRUE
v1[v1>3]          # v1>3 is a logical vector TRUE for elements >3

v1[6:10] <- 6:10

length(v1) <- 15 # the last 5 elements are added as missing data: NA

# Factors are used to store categorical data.
eye.col.v <- c("brown", "green", "brown", "blue", "blue", "blue")         #vector
eye.col.f <- factor(c("brown", "green", "brown", "blue", "blue", "blue")) #factor
eye.col.v
eye.col.f

# R will identify the different levels of the factor - e.g. all distinct values. The data is stored internally as integers - each number corresponding to a factor level.
levels(eye.col.f)  # The levels (distinct values) of the factor (categorical var)
as.numeric(eye.col.f)  # As numeric values: 1 is  blue, 2 is brown, 3 is green
as.numeric(eye.col.v)  # The character vector can not be coerced to numeric
as.character(eye.col.f)  
as.character(eye.col.v) 

# A matrix is a vector with two dimensions:
m <- rep(1, 20)   # A vector of 20 elements, all 1
dim(m) <- c(5,4)  # Dimensions set to 5 & 4, so m is now a 5x4 matrix

#2-dimension
m <- matrix(data=1, nrow=5, ncol=4)  # same matrix as above, 5x4, full of 1s
m <- matrix(1,5,4)                       # same matrix as above
dim(m)                               # What are the dimensions of m?

m <- cbind(1:5, 5:1, 5:9)  # Bind 3 vectors as columns, 5x3 matrix
m <- rbind(1:5, 5:1, 5:9)  # Bind 3 vectors as rows, 3x5 matrix

m <- matrix(1:10,10,10)
m[2,3]  # Matrix m, row 2, column 3 - a single cell
m[2,]   # The whole second row of m as a vector
m[,2]   # The whole second column of m as a vector
m[1:2,4:6] # submatrix: rows 1 and 2, columns 4, 5 and 6
m[-1,]     # all rows *except* the first one

# Are elements in row 1 equivalent to corresponding elements from column 1:
m[1,]==m[,1] 
# A logical matrix: TRUE for m elements >3, FALSE otherwise:
m>3 
# Selects only TRUE elements - that is ones greater than 3:
m[m>3]

t(m)          # Transpose m     
m <- t(m)     # Assign m the transposed m
m %*% t(m)    # %*% does matrix multiplication
m * m         # * does element-wise multiplication

# array is any-dimension and all elements must be of the same data type
a <- array(data=1:18,dim=c(3,3,2)) # 3d with dimensions 3x3x2
a <- array(1:18,c(3,3,2))          # the same array

# Lists are collections of objects. A single list can contain all kinds of elements - character strings, numeric vectors, matrices, other lists, and so on. The elements of lists are often named for easier access.
l1 <- list(boo=v1,foo=v2,moo=v3,zoo="Animals!")  # A list with four components
l2 <- list(v1,v2,v3,"Animals!")

l3 <- list()
l4 <- NULL

l1["boo"]   # Access boo with single brackets: this returns a list.
l1[["boo"]] # Access boo with double brackets: this returns the numeric vector
l1[[1]]     # Returns the first component of the list, equivalent to above.
l1$boo      # Named elements can be accessed with the $ operator, as with [[]]

l3[[1]] <- 11 # add an element to the empty list l3
l4[[3]] <- c(22, 23) # add a vector as element 3 in the empty list l4. 

l1[[5]] <- "More elements!" # The list l1 had 4 elements, we're adding a 5th here.
l1[[8]] <- 1:11 

l1$Something <- "A thing"  # Adds a ninth element - "A thing", named "Something"

# The data frame is a special kind of list used for storing dataset tables. Think of rows as cases, columns as variables. Each column is a vector or factor.
dfr1 <- data.frame( ID=1:4,
                    FirstName=c("John","Jim","Jane","Jill"),
                    Female=c(F,F,T,T), 
                    Age=c(22,33,44,55) )

dfr1$FirstName   # Access the second column of dfr1. 

dfr1$FirstName <- as.vector(dfr1$FirstName)

dfr2 <- data.frame(FirstName=c("John","Jim","Jane","Jill"), stringsAsFactors=F)

dfr2$FirstName   # Success: not a factor.

dfr1[1,]   # First row, all columns

dfr1[,1]   # First column, all rows

dfr1$Age   # Age column, all rows

dfr1[1:2,3:4] # Rows 1 and 2, columns 3 and 4 - the gender and age of John & Jim

dfr1[c(1,3),] # Rows 1 and 3, all columns

dfr1[dfr1$Age>30,2]

mean ( dfr1[dfr1$Female==TRUE,4] )

# flow control and loops
# if (condition) expr1 else expr2

x <- 5; y <- 10

if (x==0) y <- 0 else y <- y/x #  

y

# for (variable in sequence) expr

ASum <- 0; AProd <- 1

for (i in 1:x)  
{
  ASum <- ASum + i
  AProd <- AProd * i
}

ASum  # equivalent to sum(1:x)
AProd # equivalent to prod(1:x)

# while (condition) expr
while (x > 0) {print(x); x <- x-1;}

# repeat expr, use break to exit the loop
repeat { print(x); x <- x+1; if (x>10) break}

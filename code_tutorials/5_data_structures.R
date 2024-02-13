# Vectors ----

"hello, world"
?character
class("hello, world")
vec <- c("hello, world", "goodbye")
length(vec)
class(vec)

character
numeric
?numeric
class(2)
long_vec <- c(0:100)
class(long_vec)
length(long_vec)
?c()

logical
TRUE
FALSE
?logical

vec_logical <- c(TRUE, TRUE, FALSE)
class(vec_logical)

mixed_vector <- c("hello", 1, TRUE, "TRUE", "1")
class(mixed_vector)

character
numeric/integer
logical

# Matrices ----
?matrix
matrix(data = 1:30, nrow = 3, ncol = 10, byrow = FALSE)
matrix(data = 1:30, nrow = 3, ncol = 10, byrow = TRUE)
matrix_test <- matrix(data = 1:30, nrow = 3, ncol = 10, byrow = TRUE)

matrix_test[1,0]
matrix_test[1,5]
matrix_test[3,10]

dim(matrix_test)
nrow(matrix_test)
ncol(matrix_test)
head(matrix_test)
?head

tail(matrix_test)

# Lists ----

matrix_temp <- matrix(1:20, 2, 10)
vector_temp_1 <- c(3, 7, 12, 47)
vector_temp_2 <- c("hello", "goodbye")

list_save <- list(matrix_temp, vector_temp_1, vector_temp_2)

list_save[[1]]
list_save[[3]]
list_save[[1]][1,8]

list_save_2 <- list("two row ten column matrix" = matrix_temp, "numeric vector" = vector_temp_1, "character vector" = vector_temp_2)
list_save_2

list_save_2$`two row ten column matrix`

# Data frames ----

mat1 <- matrix(1:20, 4, 5)
mat1
mat_as_data_frame <- as.data.frame(mat1)
mat_as_data_frame

colnames(mat1)
colnames(mat_as_data_frame)

mat_as_data_frame[, "V3"]
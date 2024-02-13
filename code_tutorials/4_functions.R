# Built-in functions ----
?mean
x <- 0:100
mean(x)

5 + 1
"+"(5,1)
?"+"
# Writing new functions ----
help("function")

add_ten <- function(number) {number + 10}

add_ten(15)
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
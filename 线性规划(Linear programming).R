# Set up problem: maximize
# x1 + 9 * x2 + x3
# subject to
# x1 + 2 * x2 + 3 * x3 <= 9
# 3 * x1 + 2 * x2 + 2 * x3 <= 15
install.packages('lpSolve',dependencies = T)
library(lpSolve)
f.obj <- c (1, 9, 3)
f.con <- matrix ( c (1, 2, 3, 3, 2, 2), nrow = 2, byrow = TRUE )
f.dir <- c ( "<=" , "<=" )
f.rhs <- c (9, 15)
lp.result <- lp ( "max" , f.obj, f.con, f.dir, f.rhs)
#输出为(Output is)：Success: the objective function is 40.5
lp.result
#解为#The solution is
lp.result$solution
#输出为(Output is)[1] 0.0 4.5 0.0

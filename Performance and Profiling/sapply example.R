colClass <-

df.cols <- sapply(df, class)

mat <- matrix(1:24, nrow = 4)
apply(mat[, 1:3], 2, paste, collapse = "-")

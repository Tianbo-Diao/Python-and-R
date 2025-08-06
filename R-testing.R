library(Rfast)


measure_runtime <- function(func, ...) {
  start_time = Sys.time()  
  result = do.call(func, list(...))
  runtime = as.numeric(Sys.time() - start_time, units = "secs")  # Convert runtime to seconds
  return(list(result = result, runtime = runtime))
}

RandomData <- function(n, p) {
  mean_vec = rep(0, p)
  cov_mat = 0.5 ^ abs(outer(1:p, 1:p, "-"))
  x = Rfast::rmvnorm(n, mean_vec, cov_mat)
}

MatrixMultiplication <- function(p) {
  a = matrix(rnorm(p * p), p, p)
  b = t(a) %*% a
  return(b)
}

Data_result = measure_runtime(RandomData, n=200, p=2000)
cat("随机数生成结果: ", dim(Data_result$result), "\n")
sprintf("随机数生成时间为 %.2f 秒", Data_result$runtime)

Matrix_result = measure_runtime(MatrixMultiplication, p=2000)
cat("矩阵乘法结果: ", dim(Matrix_result$result), "\n")
cat("矩阵乘法时间: ", Matrix_result$runtime, " seconds\n")

# win original: 4 seconds, 6.8 seconds
# win OpenBLAS: 0.8 seconds, 0.5 seconds
# mac mini M4: 1.2 seconds, 2 seconds
# mac mini M4 Accelerating R: 0.1 seconds, 0.12 seconds


method_metrics <- function(method_result, beta_index, beta_value) {
  SC = as.integer( all(beta_index %in% method_result$index) )
  CF = base::setequal(method_result$index, beta_index)
  AMS = length(method_result$index)
  PSR = length( base::intersect(method_result$index, beta_index) ) / length(beta_index)
  FDR = length( base::setdiff(method_result$index, beta_index) ) / length(method_result$index)
  ME = sqrt(sum((beta_value - method_result$beta_value)^2))  
  
  return(c(SC, CF, AMS, PSR, FDR, ME))
}







library(Rfast)

# run time of the method
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

DistributedRandom <- function(n, p, m) {
  mean_vec = rep(0, p)
  cov_mat = 0.5^abs(outer(1:p, 1:p, "-"))
  x = vector("list", length = m)
  for (i in 1:m) {
    x[[i]] = Rfast::rmvnorm(n, mean_vec, cov_mat)
  }
  return(x)
}

Data_result = measure_runtime(RandomData, n=200, p=2000)
cat("随机数生成结果: ", dim(Data_result$result), "\n")
sprintf("随机数生成时间为 %.2f 秒", Data_result$runtime)

Matrix_result = measure_runtime(MatrixMultiplication, p=2000)
cat("矩阵乘法结果: ", dim(Matrix_result$result), "\n")
cat("矩阵乘法时间: ", Matrix_result$runtime, " seconds\n")

Distributed_result = measure_runtime(DistributedRandom, n=6000, p=600, m=60)
sprintf("分布式随机数生成时间为 %.2f 秒", Distributed_result$runtime)

# win original: 4 seconds, 6.8 seconds
# win OpenBLAS: 0.8 seconds, 0.5 seconds
# mac mini M4: 1.2 seconds, 2 seconds
# mac mini M4 Accelerating R: 0.1 seconds, 0.12 seconds


# metrics for method results
method_metrics <- function(method_result, beta_true) {
  beta_index1 = which(beta_true !=0)
  beta_index2 = which(method_result$beta !=0)
  SC = as.integer( all(beta_index1 %in% beta_index2) )
  CF = base::setequal(beta_index2, beta_index1)
  AMS = length(beta_index2)
  PSR = length( base::intersect(beta_index2, beta_index1) ) / length(beta_index1)
  FDR = length( base::setdiff(beta_index2, beta_index1) ) / length(beta_index2)
  ME = norm(as.matrix(method_result$beta - beta_true), type = 'f')
  return(c(SC, CF, AMS, PSR, FDR, ME))
}









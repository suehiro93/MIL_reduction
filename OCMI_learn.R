library(compiler)
max_iter <- 30
threshold <- 1e-5

# solve convex optimization for one-class MIL
# C is a hyperparameter which controls the trade-off between the regularization and the penalty
# iteratively solves because of the max function
OCMI_learn <- function(bags.list, instance_nums, C){
  # w_1, ..., w_d, xi_1, ..., xi_m
  dimension <- ncol(bags.list[[1]])
  sample_num <- length(instance_nums)
  instances.mat <- matlist2mat2(bags.list, instance_nums)
  cvec <- c(rep(0,dimension), rep(C/sample_num, sample_num))
  Qmat <- matrix(0, nrow=dimension+(sample_num), ncol=dimension+(sample_num))
  Qmat[1:dimension, 1:dimension] <- diag(1, dimension, dimension)
  temp_w <- rep(1, dimension)
  lb <- c(rep(-Inf,dimension), rep(0,sample_num)) 
  ub <- c(rep(Inf,dimension), rep(Inf,sample_num))
  sense <- rep("G",sample_num)
  bvec <- rep(1,sample_num)
  instance_star.mat <- matrix(NA, nrow = sample_num, ncol = dimension)  
  obj_value_old <- Inf
  for(i in 1:max_iter){
    for(j in 1:sample_num){
      instance_star.mat[j, ] <- bags.list[[j]][which.max(tcrossprod(temp_w, bags.list[[j]])), ]
    }
    Amat <- cbind(instance_star.mat,diag(sample_num)) 
    solution <- Rcplex(cvec=cvec, Qmat=Qmat, Amat=Amat, lb=lb, ub=ub, bvec=bvec, sense=sense)
    variables <- solution$xopt
    temp_w <- variables[1:dimension] 
    if((obj_value_old - solution$obj) <= threshold ){
      break
    }else{
      obj_value_old <- solution$obj
    }
  }
  return(list(w=temp_w))
}


matlist2mat2 <- function(matlist, row_nums){
  total_rownum <- sum(row_nums)
  colnum <- ncol(matlist[[1]])
  ret.mat <- matrix(NA, nrow=total_rownum, ncol=colnum)
  row_id <- 1
  for(i in 1:length(matlist)){
    ret.mat[row_id:(row_id+row_nums[i]-1),] <- matlist[[i]]
    row_id <- row_id + row_nums[i]
  }
  return(ret.mat)
}

matlist2mat2 <- cmpfun(matlist2mat2)


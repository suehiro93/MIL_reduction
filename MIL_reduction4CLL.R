make_data4complementary_learning <-function(cl_data.mat, cl_labels.list, cl_real_labels){
  bags_num <- length(cl_labels.list)
  bags.list <- vector("list", length = bags_num)
  instance_nums <- rep(NA, bags_num)
  dimension <- ncol(cl_data.mat)
  unique_labels <- unique(sort(cl_real_labels))
  class_num <- length(unique_labels)
  # complementarily labeled data
  for(i in 1:bags_num){
    target_instance <- cl_data.mat[i, ]
    cl_labels <- cl_labels.list[[i]]
    cl_labels_num <- length(cl_labels)
    target_block.list <- vector("list", cl_labels_num)
    for(k in 1:cl_labels_num){
      target_block.list[[k]] <- (((cl_labels[k] - 1)*dimension) + 1):(cl_labels[k]*dimension)
    }
    instances.mat <- matrix(0, nrow=class_num-cl_labels_num, ncol=dimension * class_num)
    untarget_labels <- unique_labels[-cl_labels]
    instance_id <- 1
    for(j in untarget_labels){
      target_block2 <- (((j - 1)*dimension) + 1):(j*dimension)
      for(k in 1:cl_labels_num){
        instances.mat[instance_id, target_block.list[[k]]] <- -target_instance
      }
      instances.mat[instance_id, target_block2] <- target_instance
      instance_id <- instance_id + 1
    }
    bags.list[[i]] <- instances.mat
    instance_nums[i] <- instance_id - 1
  }
  bag_labels <- c(rep(1, bags_num))
  return(list(bags.list = bags.list, bag_labels = bag_labels, 
              instance_nums = instance_nums, original_labels = c(cl_real_labels)))
}

make_ALLcomplementary_labeled_data <- function(data.mat, labels, comp_labels_num = 1, seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)    
  }
  unique_labels <- unique(labels)
  if(is.null(comp_labels_num)){
    comp_labels_num <- length(unique_labels)-2
  }
  sample_num <- length(labels)
  rand_id <- 1:sample_num
  labeled_data.mat <- data.mat[-rand_id, ]
  comp_labeled_data.mat <- data.mat[rand_id, ]
  labeled_labels <- labels[-rand_id]
  comp_labeled_labels <- labels[rand_id]
  comp_labels.list <- vector("list", length = length(rand_id))
  for(i in 1:length(rand_id)){
    target_label <- comp_labeled_labels[i]
    comp_num <- sample(1:(comp_labels_num), 1)
    comp_labels.list[[i]] <- sample(setdiff(unique_labels, target_label), comp_num)
  }
  ret_data.mat <- rbind(labeled_data.mat, comp_labeled_data.mat)
  return(list(cl_data.mat = ret_data.mat,
              cl_labels.list = comp_labels.list,
              cl_real_labels = comp_labeled_labels))
}

make_synthetic_multiclass_data <- function(data_num, dimension, class_num){
  if(dimension < class_num){
    message("Please set as dimension > class_num")
    return(NULL)
  }
  class_chara_dim <- round(dimension/class_num)
  class_sample_num <- round(data_num/class_num)
  dimension <- class_chara_dim * class_num
  data_num <-  class_sample_num * class_num
  data.mat <- matrix(rnorm(data_num*dimension), nrow=data_num, ncol=dimension)
  target_dim <- 1:class_chara_dim
  target_id <- 1:class_sample_num
  labels <- rep(0, data_num)
  for(i in 1:class_num){
    labels[target_id] <- i
    for(j in target_dim){
      data.mat[target_id, j] <- rnorm(class_sample_num, mean=2)
    }
    target_dim <- target_dim + class_chara_dim
    target_id <- target_id + class_sample_num
  }
  return(list(data.mat = data.mat, labels = labels))
}


convert_MIhypo2MChypo <- function(MI_hypo, original_labels){
  unique_labels <- unique(original_labels)
  w <- MI_hypo$w
  #  b <- MI_hypo$b
  class_num <- length(unique_labels)
  dimension <- length(w)/class_num
  w.mat <- matrix(w, dimension, class_num)
  return(list(w.mat=w.mat, classes=sort(unique_labels)))
}

predict_multiclass <- function(MC_hypo, test_data.mat){
  values <- crossprod(MC_hypo$w.mat, t(test_data.mat))
  predicted_class <- rep(NA, ncol(values))
  for(i in 1:ncol(values)){
    predicted_class[i] <- which.max(values[,i])
  }
  return(predicted_class)
}
# setting of artificial datasets
DIMS <- c(50)
CLASS_NUMS <- c(5,10, 25)
SAMPLE_NUMS <- c(1000)
Cs <- c(0.01,1,100)
W_BIAS <- TRUE
DIR <- "./data/"
dir.create(DIR)

# output the datasets into csv files
data_output <-TRUE
CORES_NUM <- 1 # parallel computation over trials
library(pforeach)
library(Rcplex)
source("./MIL_reduction4CLL.R")
source("./data2csv.R")
source("./OCMI_learn.R")
for(C in Cs){
  accs <- pforeach(i= 1:10, .cores=CORES_NUM, .c=c)({
    set.seed(i)
    for(DIM in DIMS){
      for(SAMPLE_NUM in SAMPLE_NUMS){
        for(CLASS_NUM in CLASS_NUMS){
          temp_file_name <- paste("d", DIM, "n", SAMPLE_NUM, "k", CLASS_NUM, "s", i, sep="")
          temp_dir <- paste(DIR,temp_file_name, sep="")
          dir.create(temp_dir)
          message(temp_file_name)
          train_file_name <- paste(temp_dir,"/train_", temp_file_name, sep="")
          test_file_name <- paste(temp_dir,"/test_", temp_file_name, sep="")
          syn_data_train <- make_synthetic_multiclass_data(SAMPLE_NUM, DIM, CLASS_NUM)
          comp_syn_data <- make_ALLcomplementary_labeled_data(syn_data_train$data.mat, syn_data_train$labels)
          syn_data_test <- make_synthetic_multiclass_data(SAMPLE_NUM, DIM, CLASS_NUM)
          if(data_output==TRUE){
            compdata2csv(comp_syn_data, train_file_name)
            data2csv(syn_data_test, test_file_name)
          }
          # consider a bias term by adding a constant feature
          if(W_BIAS == TRUE){
            train_num <- nrow(comp_syn_data$cl_data.mat)
            test_num <- nrow(syn_data_test$data.mat)
            train_bias_features <- rep(0.1, train_num)
            test_bias_features <- rep(0.1, test_num)
            comp_syn_data$cl_data.mat <- cbind(comp_syn_data$cl_data.mat, train_bias_features)
            syn_data_train$data.mat <- cbind(syn_data_train$data.mat, train_bias_features)
            syn_data_test$data.mat <- cbind(syn_data_test$data.mat, test_bias_features)
          }
          comp_syn_bags.list <- make_data4complementary_learning(comp_syn_data$cl_data.mat,
                                                                 comp_syn_data$cl_labels.list,
                                                                 comp_syn_data$cl_real_labels)
          start <- proc.time()
          MI_model <- OCMI_learn(comp_syn_bags.list$bags.list, comp_syn_bags.list$instance_nums, C)
          end <- proc.time()
          train_time <- (end-start)[3]
          MC_model <- convert_MIhypo2MChypo(MI_model, comp_syn_bags.list$original_labels)
          pr_train <- predict_multiclass(MC_model, syn_data_train$data.mat)
          pr_test <- predict_multiclass(MC_model, syn_data_test$data.mat)
          message("train_acc: ", length(which((pr_train==syn_data_train$labels)==TRUE))/length(syn_data_train$labels))
          message("acc: ", length(which((pr_test==syn_data_test$labels)==TRUE))/length(syn_data_test$labels))
          test_acc <- length(which((pr_test==syn_data_test$labels)==TRUE))/length(syn_data_test$labels)
          write.csv(matrix(c(test_acc, train_time),nrow = 1), file = paste(temp_dir,"/zours_result_C",C,".csv",sep=""))
          test_acc
        }
      }
    }
  })
}
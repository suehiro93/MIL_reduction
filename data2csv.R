compdata2csv <- function(comp_data, file_name){
  ord_labels <- comp_data$cl_real_labels
  comp_data.mat <- comp_data$cl_data.mat
  comp_labels <- as.numeric(unlist(comp_data$cl_labels.list))
  write.table(cbind(ord_labels, comp_data.mat), file=paste(file_name, "_ord.csv", sep = ""), col.names = F, sep = ",", quote=FALSE, row.names=FALSE)
  write.table(cbind(comp_labels, comp_data.mat), file=paste(file_name, "_comp.csv", sep = ""), col.names = F, sep = ",", quote=FALSE, row.names=FALSE)
}


data2csv <- function(data, file_name){
  data.mat <- data$data.mat
  data_labels <- as.numeric(data$labels)
  write.table(cbind(data_labels, data.mat), file=paste(file_name, ".csv", sep = ""), col.names = F, sep = ",", quote=FALSE, row.names=FALSE)  
}
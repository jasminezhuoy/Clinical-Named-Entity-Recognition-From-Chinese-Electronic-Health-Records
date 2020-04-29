source("Code/Dictionary.R")
source("Code/CRF.R")

##### Preprocessing testing - Dictionary
DICT_test <- function(path, DICT){
  EHR_text <- readLines(path,encoding = "UTF-8")[1]
  EHR_text <- EHR_text[1]
  DICT_result <- DICT_annotate(EHR_text, DICT)
  return(DICT_result)
}


##### Preprocessing testing - CRF
test = crf_cbind_attributes(test_data, 
                            terms = c("text","POS"), by = c("cate_id", "doc_id", "sent_id"),
                            from = -2, to = 2, ngram_max = 3, sep = "-")
attributes <- grep("text|POS", colnames(test), value=TRUE)


##### Precision, Recall and F1 score
Performance <- function(TP,TPFP,TPFN){
  P <- TP/TPFP
  R <- TP/TPFN
  F1 <- 2*P*R/(P+R)
  DICT_per <- round(data.frame("Precision"=P, "Recall"=R, "F1"=F1),4)
  return(DICT_per)
}


##### Evaluate
Evaluate <- function(testpath, method){
  filedir <- dir(path = testpath,full.names = T)
  dir1 <- c()
  TP <- 0 #True Positive
  TPFP <- 0 #True Positive + False Positive
  TPFN <- 0 #True Positive + False Negative (all true entities)
  data1 <- data.frame()
  dataset <- data.frame()
  for(i in filedir){
    dir1 <- dir(path = i,full.names = T)
    for(j in dir1){
      if(!grepl("original",j)){
        #true
        mannual <- read_delim(file = j,delim = "\t",col_names= F, col_types = cols())
        #result
        if(method == "Dictionary"){
          result <- DICT_test(paste0(j,"original.txt"), DICT) #annotate based on dict
        }
        else if(method == "CRF"){
          #Annotate based on CRF
          doc_id <- as.integer(unlist(strsplit(unlist(strsplit(sub(".txt","",j),"/"))[4],"-"))[2])
          cate_id <- as.integer(unlist(strsplit(unlist(strsplit(j,"/"))[3],"-"))[1])
          test_log <- test$doc_id==doc_id&test$cate_id==cate_id
          predict_data <- predict(model, newdata = test[test_log,attributes], group = test[test_log,]$doc_id)
          predict_data <- cbind(test$text[test_log],predict_data$label)
          result <- CRF_result_transform(predict_data)
        }
        else{
          print('Wrong method.')
          break
        }
        if(nrow(mannual)!=0){
          names(mannual) <- c("entity","start","end","entity_type")
          TP_Entity <- merge(result,mannual) #only keep true positive
        }
        else{
          TP_Entity <- data.frame()
        }
        TP <- TP+nrow(TP_Entity)
        TPFP <- TPFP+nrow(result)
        TPFN <- TPFN+nrow(mannual)
      }
    }
  }
}


DICT_performance <- Evaluate("Data/Test dataset", "Dictionary")
CRF_performance <- Evaluate("Data/Test dataset", "CRF")
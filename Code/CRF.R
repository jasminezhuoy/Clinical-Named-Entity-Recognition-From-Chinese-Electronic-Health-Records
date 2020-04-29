#install.packages("jiebaR")
#install.packages("crfsuite")
library(readr)
library(jiebaR)
library(crfsuite)


##### Create bag-of-pos feature
TAG_jieba <- function(original_text){
  tag <- worker("tag",symbol=TRUE) #Annotator
  POS_tag = tag <= original_text
  POS_name = names(POS_tag)
  POS = c()
  for (i in 1:length(POS_tag)){
    POS=c(POS,rep(POS_name[i],nchar(POS_tag[i])))
  }
  return(POS)
}


##### Load data in CRF format
CRF_load <- function(path){
  filedir <- dir(path = path,full.names = T)
  result_data <- data.frame()
  for(i in filedir){
    dir1 <- dir(path = i,full.names = T)
    for(j in dir1){
      if(grepl("original",j)){#if file is original text
        original_text <- readLines(j,encoding = "UTF-8")#encode bt UTF-8
        original_text <- original_text[1]
        original_text_list <- unlist(strsplit(original_text,split = ""))
        #string to vector
        label <- rep("O",length(original_text_list))
        if(length(original_text_list)>0){
          prepro_data <- data.frame(text=original_text_list,
                                    labels=label,
                                    stringsAsFactors = F)
          annotation_path <- sub("original.txt","",j)
          annotation_data <- read_delim(file = annotation_path,
                                        delim = "\t", col_names= F,
                                        col_types = cols(X1 = col_character(),X2 = col_integer(),X3 = col_integer(),X4 = col_character()))
          doc_id <- rep(as.integer(unlist(strsplit(unlist(strsplit(sub(".txtoriginal.txt","",j),"/"))[4],"-"))[2]), length(original_text_list)) #doc id
          cate_id <- rep(as.integer(unlist(strsplit(unlist(strsplit(j,"/"))[3],"-"))[1]), length(original_text_list))#category id
          text_id <- 1:length(prepro_data$text)
          sent_index <- text_id[prepro_data$text=="。"]
          if(length(sent_index)==0|length(text_id)==1){#if file is empty or no full stop
            sent_index <- length(text_id)
          }else if(sent_index[length(sent_index)]!=length(text_id)){#if no full stop in the end
            sent_index <- c(sent_index,length(text_id))
          }
          sent_index <- sent_index-c(0,sent_index)[-length(sent_index)-1]#sentence length
          sent_id <- c()
          for(index in 1:length(sent_index)){
            sent_id <- c(sent_id,rep(index, sent_index[index]))#sentence id
          }
          POS = TAG_jieba(original_text)#create bag-of-pos feature
          if(nrow(annotation_data)>0){
            entity_type <- annotation_data$X4
            entity_type[entity_type=="身体部位"] <- "I-body"
            entity_type[entity_type=="检查和检验"] <- "I-test"
            entity_type[entity_type=="症状和体征"] <- "I-symp"
            entity_type[entity_type=="疾病和诊断"] <- "I-dis"
            entity_type[entity_type=="治疗"] <- "I-treat"
            annotation_data$X4 <- entity_type
            for(k in 1:nrow(annotation_data)){
              prepro_data[as.integer(annotation_data[k,2]):as.integer(annotation_data[k,3])+1,2] <- annotation_data[k,4]
              prepro_data[as.integer(annotation_data[k,2])+1,2] <- sub("I-","B-",annotation_data[k,4])
            }
          }
          result_data <- rbind(result_data,cbind(cate_id,doc_id,sent_id,POS,prepro_data))
        }
      }
    }
  }
  names(result_data) <- c("cate_id","doc_id","sent_id","POS","text","label")
  return(result_data)
}


##### Load training data
train_data <- CRF_load("Data/Training dataset")


##### Load testing data
test_data <- CRF_load("Data/Test dataset")
head(test_data)


##### Preprocess training data
train = crf_cbind_attributes(train_data, 
                             terms = c("text","POS"), #character feature and bag-of-pos feature
                             by = c("cate_id", "doc_id", "sent_id"),#group by sentence
                             from = -2, to = 2, ngram_max = 3, sep = "-") #set window size
head(train)


##### Training
attributes <- grep("text|POS", colnames(train), value=TRUE)
model <- crf(y = train$label, 
             x = train[,attributes], 
             group = train$doc_id,
             method = "lbfgs")


##### Transform results
CRF_result_transform <- function(predict_data){
  result <- data.frame()
  j <- 0
  label_BIO <- c("start")
  label_type <- c("start")
  for(i in 1:nrow(predict_data)){
    temp_label <- as.character(predict_data[i,2])
    temp <- unlist(strsplit(temp_label,"-"))# 将标签拆分，例如：B-dis拆分成B和dis
    if(temp[1]=="O"){
      temp[1] <- "O"
      temp[2] <- "O"
    }
    if(label_BIO[i]=="O" & temp[1]!="B"){
      temp[1] <- "O"
      temp[2] <- "O"
    }
    label_BIO <- c(label_BIO,temp[1])
    label_type <- c(label_type,temp[2])
    if(temp[1]=="B"){
      j <- j+1
      result[j,1] <- predict_data[i,1]
      result[j,2] <- i-1
      result[j,3] <- i-1
      result[j,4] <- temp[2]
    }
    if(temp[1]=="I" & temp[2]==label_type[i] & j!=0){#####?
      result[j,1] <- paste(result[j,1],predict_data[i,1],sep = "")
      result[j,3] <- i-1
    }
  }
  if(nrow(result)!=0){
    names(result) <- c("entity","start","end","entity_type")
    result[result$entity_type=="body",4] <- "身体部位"
    result[result$entity_type=="test",4] <- "检查和检验"
    result[result$entity_type=="symp",4] <- "症状和体征"
    result[result$entity_type=="dis",4] <- "疾病和诊断"
    result[result$entity_type=="treat",4] <- "治疗"
  }
  return(result)
}


##### Preprocess testing data
test = crf_cbind_attributes(test_data, 
                            terms = c("text","POS"), by = c("cate_id", "doc_id", "sent_id"),
                            from = -2, to = 2, ngram_max = 3, sep = "-")
attributes <- grep("text|POS", colnames(test), value=TRUE)


##### Testing
filedir <- dir(path = "Data/Test dataset",full.names = T)
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
      #Annotate based on CRF
      doc_id <- as.integer(unlist(strsplit(unlist(strsplit(sub(".txt","",j),"/"))[4],"-"))[2])
      cate_id <- as.integer(unlist(strsplit(unlist(strsplit(j,"/"))[3],"-"))[1])
      test_log <- test$doc_id==doc_id&test$cate_id==cate_id
      predict_data <- predict(model, newdata = test[test_log,attributes], group = test[test_log,]$doc_id)
      predict_data <- cbind(test$text[test_log],predict_data$label)
      result <- CRF_result_transform(predict_data)
    }
    if(nrow(mannual)!=0){
      names(mannual) <- c("entity","start","end","entity_type")
      TP_Entity <- merge(result,mannual) #only keep true positive
    }else{
      TP_Entity <- data.frame()
    }
    TP <- TP+nrow(TP_Entity)
    TPFP <- TPFP+nrow(result)
    TPFN <- TPFN+nrow(mannual)
  }
}


##### Precision, Recall and F1 score
Performance <- function(TP,TPFP,TPFN){
  P <- TP/TPFP
  R <- TP/TPFN
  F1 <- 2*P*R/(P+R)
  DICT_per <- round(data.frame("Precision"=P, "Recall"=R, "F1"=F1),4)
  return(DICT_per)
}
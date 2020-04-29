library(readr)


##### Load training data
filedir <- dir(path="Data/Training dataset", full.names=T)
dir1 <- c()
data1 <- data.frame()
dataset <- data.frame()
for(i in filedir){
  dir1 <- dir(path = i,full.names = T)
  for(j in dir1){
    if(!grepl("original",j)){ 
      data1 <- read_delim(file = j,delim = "\t",col_names= F, col_types = cols())
      if(nrow(data1)>0){
        dataset <- rbind(dataset,data1)
      }
    }
  }
}


##### Create dictionary
DICT_create <- function(dataset){
  names(dataset) <- c("entity","start","end","entity_type")
  dictionary <- dataset[,c(1,4)]
  dictionary <- unique(dictionary)
  return(dictionary)
}

DICT <- DICT_create(dataset)


##### Annotate data based on dictionary
DICT_annotate <- function(EHR_text, dictionary){ 
  dic_entity <- dictionary$entity
  dic_entity_type <- dictionary$entity_type
  
  result <- data.frame()
  current_result <- data.frame()
  for(i in 1:length(dic_entity)){
    if(grepl(dic_entity[i],EHR_text)){
      index <- gregexpr(dic_entity[i],EHR_text)#find text position
      index <- unlist(index)
      rows <- length(index)
      current_result <- data.frame()
      current_result[1:rows,1] <- dic_entity[i]
      current_result[1:rows,2] <- index-1
      current_result[1:rows,3] <- index-1+nchar(dic_entity[i])-1
      current_result[1:rows,4] <- dic_entity_type[i]
      result <- rbind(result,current_result)
    }
  }
  if(nrow(result)!=0){#if file is not empty
    names(result) <- c("entity","start","end","entity_type")
    result <- result[order(result$start),]
  }
  return(result)
}

EHR_text <- readLines("Data/Test dataset/02-病史特点/病史特点-302.txtoriginal.txt",encoding = "UTF-8")
EHR_text <- EHR_text[1]
DICT_result <- DICT_annotate(EHR_text, DICT)


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
      EHR_text <- readLines(paste0(j,"original.txt"),encoding = "UTF-8")
      EHR_text <- EHR_text[1]
      result <- DICT_annotate(EHR_text, DICT) #annotate based on dict
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


##### Precision, Recall and F1 score
Performance <- function(TP,TPFP,TPFN){
  P <- TP/TPFP
  R <- TP/TPFN
  F1 <- 2*P*R/(P+R)
  DICT_per <- round(data.frame("Precision"=P, "Recall"=R, "F1"=F1),4)
  return(DICT_per)
}
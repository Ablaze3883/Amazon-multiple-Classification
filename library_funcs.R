iskclean<-function(nm){
  rm(list=nm,envir = .GlobalEnv); invisible(gc())
}

dummydataframe <- function(){
  return(data.frame(matrix(ncol = 0,nrow = 0)))
}

Countwords<-function(x){
  str_count(x,'\\w+')
}

UniqueWords<-function(x){
  unique(unlist(strsplit(x," ")))
}

RemoveShortWords<-function(x){
  y=unlist(strsplit(x," "))
  paste(y[ncahr(y)>=gminworldlen],collapse = " ")
}

RemoveStopWords<-function(x){
  x=unlist(rm_stopwords(x,tm::stopwords("english")))
  paste(x,collapse = " ")
}

model.predict.accu<-function(model,df){
  prediction<-predict(model,df)
  xtab<-table(df$Y,prediction)
  A=as.matrix(xtab)
  acc=sum(A[row(A)==col(A)])/nrow(df)
  return(acc)
}

GBMModel.predict.accu<-function(model,df,ntrees){
  predict.class <-predict(model,newdata=df,n.trees=ntrees,
                          type="response")
  pred_class<-apply(predict.class,1,which.max)
  xtab<-table(df$Y,pred_class)
  A=as.matrix(xtab)
  acc=sum(A[row(A)==col(A)])/nrow(df)
  return(acc)
}
FillNAWithMean<-function(data){
  for(i in 1:ncol(data)){
    data[ , i][is.na(data[ ,i])]<-mean(data[ ,i], na.rm=TRUE)
  }
  return(data)
}

model.predict.onevsall.metrics<-function(model,df,predictions){
  cm<-vector("list",lenght(levels(df$Y)))
  for(i in seq_along(cm)){
    positive.class<-levels(df$Y)[i]
    cm[[i]]<-confusionMatrix(prediction,df$Y,
                             positive=positive.class)
  }
  c<-cm[[1]]$byClass
  re<-sum(c[,"Recall"])/nrow(c)
  pr<-sum(c[,"Precision"])/nrow(c)
  acc<-sum(c[,"Balanced Accuracy"])/nrow(c)
  f1<-2*((re*pr)/(re+pr))
  print(paste0("Macro F1 is:",round(f1,2)))
  
  acc2= lenght(which(predictions==df$Y))/lenght(df$Y)
  print(paste0("Macro Accuracy is:",round(acc,2),",",round(acc2,2)))
  
  metrics<-c("Balanced Accuracy","Precision","Recall","F1")
  print("Classwise metrics is - ")
  print(cm[[1]]$byClass[,metrics])
}


#'HFRS function
#'This function calculates the Hospital Frailty Risk Score from ICD codes
#'@export
#'@keywords frailty, risk, hfrs
#'@param df The data frame containing ICD codes
#'@param columns array identifying the columns containing ICD codes
#'@examples data$HFRS<-hfrs(dataframe, c("D1","D2","D3","D4","D5"))
#'@examples hfrs(dataframe,1:20)
#'hfrs()

hfrs<-function(df, columns){
  data<-df
  diagnoses <- columns
  
  points<-system.file("extdata","points.tsv",package = "HFRS")
  points<-read.csv(points,sep = '\t',T)
  
  for(d in diagnoses){
    data[,d]=substr(data[,d],1,3)
    data[paste0("match_",d)]<-charmatch(data[,d],points[,1])
    
    for (i in 1:nrow(data)){
      if(is.na(data[i,paste0("match_",d)])){
        data[i,paste0("points_",d)]<-0
      }else{
        if(data[i,paste0("match_",d)] >= 1){
          data[i,paste0("points_",d)]<-points[data[i,paste0("match_",d)],7]
        }
        else{
          data[i,paste0("points_",d)]<-0
        }
      }
    }
  }
  data$score <- rowSums(data[,grep("points", names(data))])
  return(data$score)
}

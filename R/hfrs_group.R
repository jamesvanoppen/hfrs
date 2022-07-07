#'HFRS_group function
#'This function assigns the risk category (Low, Intermediate, High) to Hospital Frailty Risk Score data
#'@export
#'@keywords frailty, risk, hfrs
#'@param df The data frame containing ICD codes
#'@param hfrs the variable for HFRS score
#'@examples data$HFRS_category<-hfrs_group(dataframe, "hfrs_score")
#'hfrs_group()


hfrs_group<-function(df, hfrs){
  data<-df
  score <- hfrs

  data$hfrs_group <- cut(data[,score],
                         breaks = c(-Inf,5,15,Inf),
                         labels = c(1,2,3))
  data$hfrs_group <- as.numeric(data$hfrs_group)
  data$hfrs_group <- ordered(data$hfrs_group,
                           levels=c(1,2,3),
                           labels=c("Low","Intermediate","High"))
  
  return(data$hfrs_group)
}



#########################################################################################################################
#########################################################################################################################
#map_stats
#'A collection of measures for evaluating Self-Organizing Maps (SOM) models
#'
#'map_stats calculates common measures of model stats for a SOM model
#'@param  map_obj an object with class 'kohonen'
#'@return a data.frame of values
#'@return TSS     The total sum of squares
#'@return WCSS    The within-class sum of squares
#'@return TOTWCSS The total within-class sum of squares
#'@return BCSS    The between-class sum of squares, i.e. TSS-TOTWCSS
#'@return R2     The total variance explained by the SOM classes (BCSS/TSS)
#'@return ADJ_R2 The total variance explained by the SOM classes adjusted for class number
#'@return MAE     The mean absolute error (MAE) derived from the mean class assignment distance, i.e., quantization error
#'@return RMSE    The root-mean-square-error derived from class assignment distances, i.e., quantization error
#'@export

map_stats<-function(map_obj){
  xdim<-map_obj$grid$xdim
  ydim<-map_obj$grid$ydim
  mapk<-xdim*ydim

  #Data
  trn_dat<-data.frame(map_obj$data)
  n<-dim(trn_dat)[1]
  p<-dim(trn_dat)[2]

  MAP_ID<-map_obj$unit.classif
  trn_dat2<-cbind(MAP_ID,trn_dat)

  #Set range of functions to evaluate sum-of-squares
  #Total Sum of Squares
  TSS <- sum(scale(trn_dat, scale = FALSE)^2)

  #Within-class sum of squares
  WCSS=NULL

  for (i in 1:mapk){
    datasub1=subset(trn_dat2, MAP_ID == i)
    wss<-sum(scale(datasub1[,-1], scale = FALSE)^2)
    WCSS<-c(WCSS, wss)
  }

  #Total sum of within-class sum-of-squares
  TotWCSS<-sum(WCSS)

  #Between class sum of squares
  BCSS<-TSS-TotWCSS

  #calculate reduction in variance explained by mapping (i.e., R-squared)
  R2<-BCSS/TSS
  ADJ_R2 <- 1-(TotWCSS*(n-1))/(TSS*(n-mapk))

  ########################################################################
  #Calculate distance-based measures of error
  MAE<-mean(map_obj$distances)
  RMSE<-sqrt(mean(map_obj$distances^2))

  #######################################################################
  #Calculate AIC_k
  k <- mapk #number of clusters/nodes/profiles
  n <- n #number of observations
  p <- p #number of variables
  D <- TotWCSS
  AIC_k<-(D + 2*p*k)

  ########################################################################
  mapstats<-aweSOM::somQuality(map_obj, trn_dat )

  #stats measures
  QE<-mapstats$err.quant
  R22<-mapstats$err.varratio
  TE<-mapstats$err.topo
  KL<-mapstats$err.kaski
  ########################################################################
  mapstats<-list("TOTSS"=TSS, "WCSS"=WCSS,
                 "TOTWCSS"=TotWCSS, "BCSS"=BCSS,
                 "R2"=R2, "ADJ_R2"=ADJ_R2, "MAE"=MAE, "RMSE"=RMSE,
                 "AIC"=AIC_k, "QUANT_ERROR"=QE, "R22"=R22, "TOPO_ERROR"=TE, "KL_ERROR"=KL)
  return(mapstats)
}

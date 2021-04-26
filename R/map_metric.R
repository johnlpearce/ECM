#########################################################################################################################
#########################################################################################################################
#map_metric
#'A function for extracting classifications and coordinates from self-Organizing Map (SOM) models
#'
#'map_stats calculates common measures of model stats for a SOM model
#'@param  map_obj an object with class 'kohonen'
#'@return a dataframe object with classiciation and coordinate assignments for each training observation.
#'@export

map_metric<-function(map_obj){
  #Extract Data
  trn_data<-data.frame(map_obj$data)
  trn_data$OBS<-as.numeric(row.names(trn_data))

  data_metric<-trn_data

  #Extract class assignments
  data_metric$NODE<-map_obj$unit.classif

  #Create Exposure Metric from ECM
  #Map GRID and coordinates
  ecm_grid<-data.frame(NODE=1:length(map_obj$grid$pts[,1]),
                       U=as.numeric(map_obj$grid$pts[,1]),
                       V=as.numeric(map_obj$grid$pts[,2]))
  ecm_metric<-merge(data_metric, ecm_grid, by="NODE")

  ecm_metric2<-ecm_metric[,c("OBS", "NODE", "U", "V")]
  rownames(ecm_metric2)<-ecm_metric2$OBS
  ecm_metric3<-ecm_metric2[order(ecm_metric2$OBS),]

  return(ecm_metric3)
}




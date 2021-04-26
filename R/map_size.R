############################################################################################
###########################################################################################
#'Assesses mapping characteristics across multiple ECM sizes
#'
#'map_size provides diagnostic plots and summaries of select criteria for determining how the size of the SOM influences characteristics of the SOM model.
#'@param trn_dat is a data object to train the map. Should be a numerical or factor based data matrix.
#'@param kmn is the minimum number of map units (aka nodes) to evaluate. Default value is 2.
#'@param kmx is maximum number of map units (aka nodes) to evaluate. Default is 5*sqrt(n).
#'@param itermax. maximum number of iterations passed to map_ecm. Default is 500*number of map nodes (k).
#'@param nstarts. number of random initializatons passed to map_inits. Default is 5.
#'@param maptopo. map topology passed to map_ecm. Default is hexagonal.
#'@param distmet. distance method passed to map_ecm. Default is Euclidean.
#'@param lmode.  self-organizing map learning algorithm passed to map_ecm
#'@param symsize sets symbol size on plots
#'@return Panels a-d on the diagnostic plot illustrate common model performance metrics as a function of map size. Panels e-f examine within-class-sum-of-squares and frequency distributions as a function of map size. A list of model and class-level perfomance statistics is also returned.
#'  \itemize{
#'  \item {xdim} x dimension
#'  \item {ydim} y dimension
#'  \item {k} Number of map nodes
#'  \item {R2} R2
#'  \item {ADJ_R2} Adjusted R2
#'  \item {MAE} mean absolute error based on class assignment distances
#'  \item {RMSE} root-mean-square-error based on class assignment distances
#'  \item {AIC} a form of Akaikes Information Criteria applied to clustering algorithms
#'  \item {TotWCSS}  Total Within-Cluster Sum-of-Squares
#'  \item {N} Number class assignments
#'  \item {FREQ} Proportion of class assignments
#'  \item {WCD} average within-class distance
#'  \item {BCD}  average between-class distance
#'  \item {WB_Ratio}  WCD/BCD
#'
#' }
#'@details An important step in the application of ECM are the user provided SOM inputs for the dimensions of the mapping (i.e., size). Here we provide common model performance metrics and class-level evaluations in effort to assist the user in determining an appropriate map size.
#'@export


#Develop a set of functions to evaluate a range of common Self-Organizing Map sizes
map_size<-function(trn_dat, kmn=NULL, kmx=NULL, itermax.=NULL, nstarts.=NULL, maptopo.=NULL, distmet.=NULL,
                  lmode.=NULL, symsize=1) {

  #set Data
  #Data
  trn_dat<-data.frame(trn_dat)
  n<-dim(trn_dat)[1]
  p<-dim(trn_dat)[2]

  varnames<-names(trn_dat)
  nvars<-p
  nobs<-n

  #Set training data
  data_trn<-data.matrix(trn_dat)
  #set Data distance matrix
  dist_mat<-stats::dist(x=data_trn, method="euclidean")

  #Set function defaults for map_ecm
  if (is.null(kmn)) kmn <- 2
  if (is.null(kmx)) kmx <- 5*sqrt(nobs)

  #Set common dimensions
  som_x<-c(2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20)
  som_y<-c(1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9, 9,10,10,11,11,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20)
  som_k<-som_x*som_y
  som_k

  #Set test range of sizes
  minsize<-which(abs(som_k-kmn)==min(abs(som_k-kmn)))
  maxsize<-which(abs(som_k-kmx)==min(abs(som_k-kmx)))

  #Set function defaults for map_ecm
  if (is.null(maptopo.)) maptopo. <- "hexagonal"
  if (is.null(distmet.)) distmet.<-"euclidean"
  if (is.null(lmode.)) lmode. <- "online"
  if (is.null(nstarts.)) nstarts. <- 5

  MAP_EVAL<-data.frame()
  CLASS_EVAL<-data.frame()

  for (i in minsize:maxsize){
    somx<-som_x[i]
    somy<-som_y[i]
    somk<-somx*somy

    if (is.null(itermax.)) itermax. <- 500*(somx*somy)

    mi<-map_inits(trn_dat=data_trn, xdim=somx, ydim=somy, nstarts=nstarts.)

    opt_init<-mi$opt_init

    map_trn<-map_ecm(trn_dat=data_trn, xdim=somx, ydim=somy, itermax=itermax.,
                    maptopo=maptopo., distmet=distmet., lmode=lmode., inits=opt_init)
    print(summary(map_trn))

    #Apply map_ecm summary function
    map_stat<-map_stats(map_trn)

    map_eval<-data.frame(xdim=somx, ydim=somy, k=somk,
                       R2=map_stat$R2,
                       ADJ_R2=map_stat$ADJ_R2,
                       MAE=map_stat$MAE,
                       RMSE=map_stat$RMSE,
                       AIC=map_stat$AIC,
                       TotWCSS=map_stat$TOTWCSS)


    class_eval<-data.frame(xdim=somx, ydim=somy, k=somk,
                           N=as.numeric(table(map_trn$unit.classif)),
                           FREQ=100*(as.numeric(table(map_trn$unit.classif))/sum(as.numeric(table(map_trn$unit.classif)))),
                           WCSS=map_stat$WCSS,
                           BCSS=map_stat$BCSS,
                           WB_RATIO=mean(map_stat$WCSS)/map_stat$BCSS)


    MAP_EVAL<-rbind(MAP_EVAL,map_eval)
    CLASS_EVAL<-rbind(CLASS_EVAL, class_eval)
  }

  #Evaluation Plots
  opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,3,2), pty="s", cex=1, xpd=FALSE)
  graphics::par(mfrow=c(3,2), mar=c(4,4,1.5,1), family='serif', ask=FALSE, xpd=FALSE)

  #Plots
  plot(MAP_EVAL$ADJ_R2~MAP_EVAL$k, ylab="Proportion of Variance", xlab="Number of nodes (k)",
       main="a) Adjusted R2", type="b", pch=19, cex=symsize, col="darkgrey",
       ylim=c(0,1))

  plot(MAP_EVAL$RMSE~MAP_EVAL$k, ylab="Distance", xlab="Number of nodes (k)",
       main="b) Root-Mean-Square-Error (RMSE)", type="b", pch=19, cex=symsize, col="darkgrey",
       ylim=c(0,max(MAP_EVAL$RMSE)*1.2))

  plot(MAP_EVAL$MAE~MAP_EVAL$k, ylab="Distance", xlab="Number of nodes (k)",
       main="c) Mean-Absolute-Error (MAE)", type="b", pch=19, cex=symsize, col="darkgrey",
       ylim=c(0,max(MAP_EVAL$MAE)*1.2))

  plot(MAP_EVAL$AIC~MAP_EVAL$k, ylab="AIC", xlab="Number of nodes (k)",
       main="d) Akaike Information Criterion (AIC)", type="b", pch=19, cex=symsize, col="darkgrey",
       ylim=c(0,max(MAP_EVAL$AIC)*1.2))

  graphics::stripchart(CLASS_EVAL$WCSS~CLASS_EVAL$k, ylab="", xlab="Number of nodes (k)",
          main="e) Within-Class Sum-of-Squares (WCSS)", vertical = TRUE, pch=19, cex=symsize, col="darkgrey")

  graphics::stripchart(CLASS_EVAL$N~CLASS_EVAL$k, ylab="Count", xlab="Number of nodes (k)",
          main="f) Within-Class Sample Size (n)", vertical = TRUE, pch=19, cex=symsize, col="darkgrey")


  on.exit(graphics::par(opar))
  on.exit(graphics::layout(1))

  ECM_SIZE<-list(MAP_EVAL=MAP_EVAL, CLASS_EVAL=CLASS_EVAL)
  return(invisible(ECM_SIZE))

}
############################################################################################

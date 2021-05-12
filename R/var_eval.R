############################################################################################
#var_eval: A Collection of functions to perform generalized data evaluation
############################################################################################
#'A data assessment and screening tool
#'
#'Performs a generalized evaluation of the data in order to examine standardized distributions of data via boxplots, variable specific missing-value patterns, and variable specific zero-value patterns. A table of summary statistics is also provided.
#'@param dat is a data frame object
#'@param labsize specifies cex value of labels on plots default is 0.75
#'@param symsize specifies cex value of symbols on plots default is 2
#'@param varlabs specifies x-axis variable labels
#'@param colormod can be used to customize plot color schemes. The default uses terrain palletes from grDevices
#'@details
#'An important first step in the evaluation of multivariate data sets is to examine candidate variables individually. This assessment and screening tool is designed to assist understanding individual data distributions and patterning with problematic features such as missing and zero values.
#'@export
#'@return a dataframe consisting of summary statistics
#'  \itemize{
#'  \item {VARIABLE} Name of variable
#'  \item {TYPE}  variable class
#'  \item {N}  number of observations
#'  \item {NAs}  number of missing values
#'  \item {ZEROs}  number of zero values
#'  \item {MEAN}  mean value
#'  \item {SD}  standard deviation
#'  \item {MIN}  minimum value
#'  \item {Q1}  lower quartile
#'  \item {MED}  median value
#'  \item {Q3}  upper quartile
#'  \item {MAX}  maximum value
#'  \item {IqR}  interquartile range
#'  \item {CoV}  coefficient of variation, i.e., SD/MEAN
#'  \item {CVrank}  ranking of CoV values
#'  }
#'@examples
#'#NIEHS Mixtures Workshop dataset1
#'data(dataset1)
#'var_eval(dataset1)

var_eval  <-  function(dat, labsize=1, symsize=2, colormod=NULL, varlabs=NULL)
{
  #Set up data
  data<-data.frame(dat)
  vars<-names(data)
  obs<-dim(data)[1]
  p<-dim(data)[2]

  #Set function defaults
  if (is.null(colormod)) colormod <- grDevices::terrain.colors(n=p)
  if (is.null(varlabs)) varlabs <- ifelse(nchar(vars)<7,vars,substr(vars, 0,7))

  #Evaluate Variable completeness and missing values
  N<-apply(apply(data,2,stats::complete.cases),2,sum)
  N.per<-round(100*(N/dim(data)[1]),2)
  NAs<-apply(apply(data,2,is.na),2,sum)
  NAs.per<-round(100*(NAs/dim(data)[1]),2)

  #Evaluate Zero Values
  ZERO<-function (x){length(which(x == 0))}
  ZEROs<-round(apply(data,2,FUN=ZERO),2)
  ZEROs.per<-round(100*(ZEROs/dim(data)[1]),2)

  #Summary Statistics
  AVG<-round(apply(data,2,FUN=mean, na.rm=TRUE),5)
  MED<-round(apply(data,2,FUN=stats::median, na.rm=TRUE),5)
  SD<-round(apply(data,2,FUN=stats::sd, na.rm=TRUE),5)
  CV<-round(SD/AVG,5)
  CVrank<-rank(-CV, ties.method = "random")
  MN<-round(apply(data,2,FUN=min, na.rm=TRUE),5)
  Q1<-round(apply(data,2,FUN=stats::quantile, na.rm=TRUE)[2,],5)
  Q3<-round(apply(data,2,FUN=stats::quantile, na.rm=TRUE)[4,],5)
  IR<-round(apply(data,2,FUN=stats::IQR, na.rm=TRUE),5)
  MX<-round(apply(data,2,FUN=max, na.rm=TRUE),5)
  vartype<-unlist(lapply(data,class))

  #Summary Table
  summ.tab<-cbind(VARIABLE=vars, TYPE=vartype, N=N, NAs=NAs, ZEROs=ZEROs,
                  MEAN=AVG, SD=SD, MIN=MN, Q1=Q1, MED=MED, Q3=Q3, MAX=MX,
                  IqR=IR, CoV=CV, CVrank=CVrank)
  summ.tab<-data.frame(summ.tab)

  #############################################################
  #Evaluation Plots
  opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,3,2), pty="s", xpd=FALSE)
  graphics::par(mar=c(4,2,1,0.5), xaxs="r", xpd=FALSE, pty="m")
  graphics::layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), heights=c(3,1,1), widths=c(2,2,2),respect = TRUE)

  #Produce a standardized boxplot
  graphics::boxplot(data, col=colormod, las=2, ylab="Unit Values",
                    names=varlabs, cex.axis=labsize)
  graphics::title(main="a) Variable Boxplots")

  plot(summ.tab$NAs, pch=20, col=colormod, ylab="Count (n)",
       xlab="", main="b) Missing Values", ylim=c(0,obs), xaxt="n",
       cex=symsize)
  graphics::axis(side=1, at=1:length(vars), labels=varlabs, las=2, cex.axis=labsize)


  plot(summ.tab$ZEROs, pch=20, col=colormod, ylab="Count (n)",
       xlab="", main="c) Values equal to zero ", ylim=c(0,obs), xaxt="n",
       cex=symsize)
  graphics::axis(side=1, at=1:length(vars), labels=varlabs, las=2, cex.axis=labsize)


  #Reset plot window to normal
  on.exit(graphics::par(opar))
  on.exit(graphics::layout(1))

  #Return dataframe with summary statistics
  return(invisible(summ.tab))

}



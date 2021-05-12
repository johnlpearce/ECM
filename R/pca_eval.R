############################################################################################
#pcaeval: Performs principal component analysis
############################################################################################
#'Principal Component Analysis with Diagnostic Plots
#'
#'Performs principal component analysis on the data and returns the results as an object of class prcomp. Scree, variance, and loading plots are provided.
#'
#'@param dat is a data object
#'@param labsize sets label size
#'@param varlabs specifies x-axis variable labels
#'@param colormod can be used to customize plot color schemes. The default uses terrain palletes from grDevices
#'@param ... arguments passed to or from other methods
#'@details
#'Assessing patterns of intercorrelations between x-variables is an important aspect in multivariate analysis. This assessment tool can assist with understanding the complexitiy of the underlying data by identifiying the important primary modes of variance that exist. In most The evaluation tool is based upon application of prcomp for stats. The
#'@return a list of pca results with class "prcomp"
#'@export
#'@examples
#'#NIEHS Mixtures Workshop dataset1
#'data(dataset1)
#'pca_eval(scale(dataset1[,2:9]))


pca_eval<-function(dat, labsize=1, colormod=NULL, varlabs=NULL, ...){
  #Set up data
  data<-data.frame(dat)
  vars<-names(data)
  obs<-dim(data)[1]
  p<-dim(data)[2]

  #Set function defaults
  if (is.null(colormod)) colormod <- grDevices::terrain.colors(n=p)
  if (is.null(varlabs)) varlabs <- ifelse(nchar(vars)<7,vars,substr(vars, 0,7))

  #Conduct PCA
  pcamod<-stats::prcomp(data, center=FALSE, scale.=FALSE)

  pcaloadings<-pcamod$rotation

  #Explore the variance explained by each component
  pr_var=pcamod$sdev^2
  #Calculate the proportion of variance explained
  pve=round(pr_var/sum(pr_var),2)

  #Evaluation Plots
  opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,2,1), pty="s", xpd=FALSE)
  graphics::par(mar=c(4,4,2,1), pty="m", xpd=FALSE)
  graphics::layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), heights=c(3,1), widths=c(2,2,3),respect = TRUE)

  stats::biplot(pcamod, xlabs=rep(".", dim(pcamod$x)[1]), cex=c(2,1), col=c("darkgrey", "darkred"),
                main="a) Biplot")


  #Plot the component Standard deviations to see which are useful
  plot(pcamod$sdev^2, pch=19, ylab="Eigenvalue", xlab="Principal Component",
       cex=1, type="b", main="b) Scree Plot", col="darkgrey")
  graphics::abline(h=1, col="black", lty=2)


  plot(pve, xlab="Principal Component", ylab="Proportion of Variance", ylim=c(0,1), type="b",
       pch=17, col="darkgrey", main="c) Variance Plot", cex=1)
  graphics::points(cumsum(pve), type='b',pch=19, col="darkgrey")
  graphics::legend("topleft", pch=c(17,19), col=c("darkgrey", "darkgrey"), bty="n",
                   legend=c("Individual", "Cumulative"), cex=1, pt.cex=0.9)

  #Reset plot window to normal
  on.exit(graphics::layout(1))
  on.exit(graphics::par(opar))

  return(invisible(pcamod))
}

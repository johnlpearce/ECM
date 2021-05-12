############################################################################################
#clus_eval: A function to explore data clustering/grouping structure
############################################################################################
############################################################################################
#'Exploring clustering/grouping structure in data
#'
#'Designed to assist users who wish to employ SOM/ECM for a clustering applications. Applies standard approaches to assist with identification of grouping structure in multivariate data.
#'@param dat is a data object
#'@param kmx user specified maximum number of clusters/groups to examine. Default is 10.
#'@param distmet the distance metric to calculate, "euclidean" is the default.
#'@param itermax maximum number of iterations allowed for kmeans. Default is 500*k.
#'@param nstarts number of random initializatons for kmeans to employ. Default is 5
#'@param symsize sets symbol size on plots
#'@return Panel a illustrates multi-dimensional scaling (MDS) results. Panel b presents a scree plot of k-means results. A dataframe with cluster/group statistics is also returned.
#'  \itemize{
#'  \item {K} Number of Clusters
#'  \item {WCSS}  Total Within-Cluster Sum-of-Squares. Lower values reflect clusters with less internal variability.
#'  \item {BCSS}  Between-Cluster Sum-of-Squares. Higher values reflect clusterings with more distinction.
#'  \item {WB_Ratio}  Presents the ratio between WCSS and BCSS sum-of-squares. Values below 1 are desired as they reflect clusterings where the within-cluster varibility is lower than the between-cluster varibility.
#'  \item {CH}  the Calinski-Harabasz is a sum-of-squares based clustering statistic. In brief, the index incorporates the WB ratio but penalizes by cluster number. Higher values reflect better 'clustering'.
#'  \item {SW}  the average silhoutte width. The silhouette ranges from −1 to +1, where a high value indicates that the objects are well matched to its own cluster and poorly matched to neighboring clusters. If the value is a high value (near +1), then the clustering configuration is appropriate. If the average is a low or negative value, then the clustering configuration may have too many or too few clusters.
#'  \item {ADJ_R2}  the adjusted R2 for the clustering model. Higher values indicate that the clustering
#' }
#'@details
#'Many unsupervised learning algorithms (e.g., SOM, kmeans) require the number of groupings for the algorithm to seek out as a user input. This tool assists users with this decision using two traditional strategies often applied in cluster analysis.
#'Understanding patterns in multivariate data can be assisted by low-dimensional visualization that seek to represent similarity of individual observations in a dataset. Here, we employ multi-dimensional scaling (MDS) to construct a 2-D mapping that projects the pairwise distances among a set of observations into a configuration of points mapped onto abstract coordinate space. Here, we employ MDS as an ordination technique in order visualize information within the data's distance matrix. Similar objects are closer in space and thus multiple isolated regions of high-density will be presented if clustering is obvious.
#'Second, multiple applications of k-means are used to internally assess how grouping structure changes as a function of the number of clusters. Results are presented as a scree plot based on the total within cluster sum-of-squares (WCSS) for each data partition. An ideal plot will present clear 'elbowing', where the measure decreases more slowly as the number of groupings increases.
#'
#'MDS is implemented via cmdscale() and k-means employs kmeans() via the stats package.
#'@export
#'@examples
#'#Example dataset3
#'data(dataset3)
#'clus_eval(scale(dataset3))

clus_eval<-function(dat, kmx=10, distmet="euclidean", itermax=NULL, nstarts=NULL, symsize=1){

  #Set up data
  data<-data.frame(dat)
  vars<-names(data)
  n<-dim(data)[1]
  p<-dim(data)[2]

  #Set training data
  data_trn<-data
  #set Data distance matrix
  dist_mat<-stats::dist(x=data_trn, method=distmet)

  TOTSS<-NULL
  WCSS=NULL
  BCSS=NULL
  WBR=NULL
  R2<-NULL
  CH<-NULL
  SW<-NULL


  for (i in 2:kmx){
    k<-i
    if (is.null(itermax)) itermax <- 500*(k)
    if (is.null(nstarts)) nstarts <- 5

    kmod<-stats::kmeans(x=data_trn, centers=k, iter.max=itermax, nstart=nstarts)

    kclusters<-kmod$cluster
    kcenters<-kmod$centers

    #Total Sum-of-squares
    totss<-kmod$totss

    #Mean within-cluster sum of squares
    totwcss<-kmod$tot.withinss

    #Mean Between-cluster sum of squares
    bcss<-kmod$betweenss

    #WB_Ratio
    wbr<-totwcss/bcss

    #Adjusted R2
    rsq <- 1-(totwcss*(n-1))/(totss*(n-k))

    #Calinski-Harabanz
    ch<-fpc::calinhara(x=data_trn, clustering=kclusters, cn = k)

    #Silhouette
    si<-cluster::silhouette(kclusters, dist=dist_mat)
    ssi <- summary(si)
    sw<-ssi$avg.width

    TOTSS<-c(TOTSS, totss)
    WCSS<-c(WCSS, totwcss)
    BCSS<-c(BCSS,bcss)
    WBR<-c(WBR,wbr)
    R2<-c(R2,rsq)
    CH<-c(CH,ch)
    SW<-c(SW,sw)

  }

  #Examine group structure using common cluster statistics
  clusstat<-data.frame(K=seq(2,kmx,1), TOTSS=TOTSS, WCSS=WCSS, BCSS=BCSS, WB_RATIO=WBR, CH=CH, SW=SW, ADJ_R2=R2)

  #Evaluation Plots
  opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,3,2), pty="s", cex=1, xpd=FALSE)
  graphics::par(mar=c(4,4,1.5,1), family="serif", xaxs="r", ask=FALSE, cex=symsize, xpd=FALSE, pty="m")
  graphics::layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE), heights=c(2,1,1), widths=c(2,2),respect = TRUE)


  #Apply multidimensional scaling to visualize clustering structure
  loc <- stats::cmdscale(dist_mat)
  x <- loc[, 1]
  y <- -loc[, 2] # reflect so North is at the top
  ## note asp = 1, to ensure Euclidean distances are represented correctly
  plot(x, y, type = "p", xlab = "MDS X", ylab = "MDS Y", asp = 1, axes = TRUE,
       main = "a) Multidimensional Scaling (MDS)", pch=21, cex=symsize, col="darkgrey", bg="darkgrey",  cex.axis=1, cex.lab=1)
  #text(x, y, rownames(loc), cex = 0.6)
  graphics::box()
  graphics::box(which="outer")

  ##########################################################################
  #Elbow Method
  plot(x=clusstat[,"K"], y=clusstat[,"WCSS"], pch=19, cex=symsize, col="darkgrey", xlab="Number of Clusters (k)",
       ylab="Within Cluster Sum-of-Squares", main="b) Elbow Method ",type="b", cex.axis=1, cex.lab=1)

  #WB Ratio
  plot(x=clusstat[,"K"], y=clusstat[,"WB_RATIO"], pch=19, cex=symsize, col="darkgrey", xlab="Number of Clusters (k)",
       ylab="WCSS/BCSS", main="c) Within/Between Ratio ",type="b", cex.axis=1, cex.lab=1)
  graphics::abline(h=1, col="darkgrey",lty=2)

  #Calinski Harabanz
  plot(x=clusstat[,"K"], y=clusstat[,"CH"], pch=19, cex=symsize, col="darkgrey", xlab="Number of Clusters (k)",
       ylab="", main="d) Calinski-Harabasz Index ",type="b", cex.axis=1, cex.lab=1)
  nc_ch<-which(clusstat$CH == max(clusstat$CH))
  graphics::abline(v=nc_ch+1, lty=2, col="darkgrey")

  #Silhouette
  plot(x=clusstat[,"K"], y=clusstat[,"SW"], pch=19, cex=symsize, col="darkgrey", xlab="Number of Clusters (k)",
       ylab="", main="e) Silhouette Width ",type="b", cex.axis=1, cex.lab=1, ylim=c(-1,1))
  nc_sw<-which(clusstat$SW == max(clusstat$SW))
  graphics::abline(v=nc_sw+1, lty=2, col="darkgrey")

  #Reset plot window to normal
  on.exit(graphics::par(opar))
  on.exit(graphics::layout(1))

  #Return cluster stats
  return(invisible(clusstat))

}
############################################################################################

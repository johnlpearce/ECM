#########################################################################################################################
#########################################################################################################################
#'map_ecm: Constructs an Exposure Continuum Map using Kohonen's Self-Organizing Map algorithm
#'
#'@param trn_dat is a data object to map. Should be a numerical or factor based data matrix.
#'@param xdim is x-dimension of the SOM
#'@param ydim is y-dimension of the SOM
#'@param maptopo specifies the topology of the SOM grid as either "rectangular" or "hexagonal"
#'@param distmet specifies dissimilarity metric. Current options include "sumofsquares", "euclidean", "manhattan", and "tanimoto". Default is to use "Euclidean" for continuous data, and "tanimoto" for factors.
#'@param lmode specifies the learning algorithm. The default is "online" but "batch" and "pbatch" are available via kohonen
#'@param itermax specifies the number if learning iterations
#'@param inits specifies if optimal initialization values are to be used.
#'@return a 'kohonen' object
#'@description This function applies Kohonen's Self-Organizing Map algorithm in order to construct a low-dimensional mapping of the data.
#'@references Kohonen, T. (1995) Self-Organizing Maps. Springer-Verlag
#'@export

map_ecm <- function (trn_dat, xdim=5, ydim=4, maptopo=NULL, itermax=NULL, inits=NULL,
                    distmet=NULL, lmode=NULL)

{

   #Data matrix
   X<-data.matrix(trn_dat)
   n<-dim(trn_dat)[1]
   p<-dim(trn_dat)[2]

   #Set function defaults
   if (is.null(maptopo)) maptopo <- "hexagonal"
   if (is.null(itermax)) itermax <- 500*(xdim*ydim)
   if (is.null(distmet)) distmet<-"euclidean"
   if (is.null(lmode)) lmode <- "online"


   #Apply SOM via Kohonens C code
   ecm<-kohonen::som(X,grid=kohonen::somgrid(xdim=xdim, ydim=ydim, topo=maptopo),
                        rlen=itermax, mode=lmode, alpha=c(0.05, 0.01),
                        dist.fcts=distmet)


   return(ecm)

}


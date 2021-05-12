#########################################################################################################################
#########################################################################################################################
#'map_inits: Identifies initial values for optimal Exposure Continuum Map fit via SOM
#'
#'@param trn_dat is a data object to map. Should be a numerical or factor based data matrix.
#'@param xdim is x-dimension of the SOM
#'@param ydim is y-dimension of the SOM
#'@param maptopo specifies the topology of the SOM grid as either "rectangular" or "hexagonal"
#'@param distmet specifies dissimilarity metric. Current options include "sumofsquares", "euclidean", "manhattan", and "tanimoto". Default is to use "Euclidean" for continuous data, and "tanimoto" for factors.
#'@param lmode specifies the learning algorithm. The default is "online" but "batch" and "pbatch" are available via kohonen
#'@param itermax specifies the number if learning iterations
#'@param seedopt specifies method for initialization values. "pca.sample", "pca" or "ran" accepted.
#'@param seedeval specifies the evaluation statistic for seed optimalization.
#'@param nstarts specifies number of initialization schemes to test if inits are not provided
#'@return a data.frame containing evaluation measures
#'@export

map_inits <- function (trn_dat, xdim=4, ydim=3, maptopo=NULL, itermax=NULL,
                       seedopt="pca.sample", seedeval="KL",
                       distmet=NULL, nstarts=NULL, lmode=NULL)

{

   #Data matrix
   trn_dat<-data.matrix(trn_dat)
   n<-dim(trn_dat)[1]
   p<-dim(trn_dat)[2]

   #Set function defaults
   if (is.null(maptopo)) maptopo <- "hexagonal"
   if (is.null(itermax)) itermax <- 500*(xdim*ydim)
   if (is.null(distmet)) distmet<-"euclidean"
   if (is.null(lmode)) lmode <- "online"
   if (is.null(nstarts)) nstarts <- 5

   nstart<-nstarts

   #Identify optimal initial values using multiple seeds
   seed_vals=1:nstarts

   #Set eval object
   QE<-NULL
   R2<-NULL
   TE<-NULL
   KL<-NULL

   for(i in 1:nstarts){
      ### RNG Seed (for reproducibility)
      set.seed(seed_vals[i])
      ### Initialization (PCA grid)
      inits <- aweSOM::somInit(trn_dat, nrows=xdim, ncols=ydim, method=seedopt)

      init_map<-kohonen::som(X=trn_dat, grid=kohonen::somgrid(xdim=xdim,ydim=ydim, topo=maptopo),
                             rlen=itermax, mode=lmode, alpha=c(0.05, 0.01),
                             dist.fcts=distmet, init=inits)
      mapstat<-map_stats(init_map)

      #Fit measures
      qe<-mapstat$QUANT_ERROR
      r2<-mapstat$R2
      te<-mapstat$TOPO_ERROR
      kl<-mapstat$KL_ERROR

      #Capture measure for each seed
      QE<-c(QE,qe)
      R2<-c(R2,r2)
      TE<-c(TE,te)
      KL<-c(KL,kl)
   }

   seed_eval<-data.frame(VAL=seed_vals, QE=QE, R2=R2, TE=TE, KL=KL)

   seedset<-seed_vals[which(get(seedeval) == min(get(seedeval)))]
   set.seed(seedset)
   inits<-aweSOM::somInit(traindat=trn_dat, nrows=xdim, ncols=ydim, method=seedopt)

   mapinits<-list(seed_eval=seed_eval, seed_set=seedset, opt_init=inits)
   return(mapinits)
}


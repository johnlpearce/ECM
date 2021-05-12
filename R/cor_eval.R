############################################################################################
#cor_eval: A Collection of functions to explore data correlations
############################################################################################
############################################################################################
#'A tool to evaluate and plot correlation structure
#'
#'cor_eval explores data correlations
#'@param dat is a data object
#'@param cormeth passes correlation method to cor()
#'@param labsize sets label size
#'@param varlabs specifies variable labels
#'@return a correlation matrix
#'@export
#'@examples
#'#NIEHS Mixtures Workshop dataset1
#'data(dataset1)
#'cor_eval(dataset1[,2:9])

cor_eval<-function(dat, cormeth="spearman", labsize=1, varlabs=NULL){
  #Set up data
  data<-data.frame(dat)
  vars<-names(data)
  obs<-dim(data)[1]
  p<-dim(data)[2]

  #Set function defaults
  if (is.null(varlabs)) varlabs <- ifelse(nchar(vars)<7,vars,substr(vars, 0,7))

  #set column names
  colnames(data)<-varlabs

  #Store base graphics
  opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,3,2), pty="s", cex=1, xpd=FALSE)
  graphics::layout(1)

  #Identy pairwise correlations
  cormat<-round(stats::cor(data, method=cormeth, use="pairwise.complete.obs"),2)

  ##############################################################################
  #Set helper functions
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }

  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }

  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- stats::as.dist((1-cormat)/2)
    hc <- stats::hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }

  ##############################################################################
  # Reorder the correlation matrix
  cormat2 <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat2)
  # Melt the correlation matrix
  melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
  melted_cormat
  value<-round(melted_cormat$value,1)
  Var1<-melted_cormat$Var1
  Var2<-melted_cormat$Var2

  ##############################################################################
  # Create a ggheatmap
  ggheatmap <- ggplot2::ggplot(melted_cormat, ggplot2::aes(Var2, Var1, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "darkblue", high = "darkred", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlation") +
    ggplot2::theme_minimal()+ # minimal theme
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1,
                                     size = 10, hjust = 1)) +
    ggplot2::coord_fixed()

  # Add text to the heatmap
  print(
  ggheatmap +
    #ggplot2::geom_text(ggplot2::aes(Var2, Var1, label = value), color = "black", size = 3) +
    ggplot2::theme(
              axis.title.x = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_blank(),
              panel.border = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.ticks = ggplot2::element_blank(),
              legend.justification = c(1, 0),
              legend.position = c(0.4, 0.8),
              legend.direction = "horizontal") +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 8, barheight = 1.5,
                                 title.position = "top", title.hjust = 0.5))
  )

  #Reset plot window to normal
  on.exit(graphics::par(opar))
  on.exit(graphics::layout(1))

  #Export correlation matrix
  return(invisible(cormat))

}
############################################################################################

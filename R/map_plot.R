#########################################################################################################################
#########################################################################################################################
#map_plot
#A collection of plotting functions to support illustration of Self-Organizing Maps (SOM) results

#Generate plots for SOM profiles
#'Plots profiles using radial segment diagrams
#'@param map_obj a SOM object
#'@param varnames specifies variable names
#'@param colormod can be used to customize plot color schemes. The default is terrain.colors().
#'@param nodelab whether or not to display reference labels on each node
#'@param labtype specifies type of label. "IDs" or "XYs" accepted.
#'@param labsize sets size of node labels
#'@param addFreq whether or not to display frequency labels
#'@param freqtype determines type of frequency labels. Relative frequencies can be specified with "frq" or counts with "cnts"
#'@param freqsize frequency label cex value
#'@param addXY adds map coordinates to plot
#'@param addleg adds legend to map
#'@param legloc defaults to "bottom" but "right" is also option
#'@param legsymsize legend symbol cex value
#'@param leglabsize legend text cex value
#'@param legtxtlas legend text position rotation passed to las. Numeric in {0,1,2,3}; the style of axis label.
#'@param ... other parameters
#'@details Radial segment diagrams provide a compact way to illustrate profiles derived from SOM. Here diagrams are presented along the SOM grid in order provide a compact visualization of SOM results.s the map.
#'@return a kohonen map where radial segment diagrams are used to illustrate profiles for each map node
#'@export

map_plot<-function(map_obj, varnames=NULL, colormod=NULL,
                      nodelab=TRUE, labsize=1, labtype="IDs",
                      addXY=TRUE, addFreq=FALSE, freqtype="frq", freqsize=1,
                      addleg="TRUE", legloc="bottom",legsymsize=2, leglabsize=1, legtxtlas=2, ...)
{

  xdim<-map_obj$grid$xdim
  ydim<-map_obj$grid$ydim
  mapk<-xdim*ydim

  #Data
  trn_dat<-data.frame(map_obj$data)
  vars<-names(trn_dat)
  n<-dim(trn_dat)[1]
  p<-dim(trn_dat)[2]

  #Set Profile labels
  IDs<-paste("[",1:mapk,"]", sep="")
  map_grid<-data.frame(map_obj$grid$pts)
  map_grid$ID<-1:mapk
  XYs<-paste(round(map_obj$grid$pts[,1],1),round(map_obj$grid$pts[,2],1), sep="")

  #Summarize Frequency Assignments and set labels
  N<-table(map_obj$unit.classif)
  FREQ<-round(100*(table(map_obj$unit.classif)/sum(table(map_obj$unit.classif))),2)
  freqtab<-data.frame(N, FREQ)
  freqtab<-merge(map_grid, freqtab, by.x="ID", by.y="Var1", all.x=TRUE)
  freqtab$Var1.1<-NULL
  colnames(freqtab)<-c("ID","x","y","N","FREQ")
  freqtab2<-freqtab[order(as.numeric(freqtab$ID)),]

  FREQ<-freqtab2$FREQ
  N<-freqtab2$N

  #Set function defaults
  if (is.null(colormod)) colormod <- grDevices::terrain.colors(n=p)
  if (is.null(varnames)) varnames <- ifelse(nchar(vars)<12,vars,substr(vars, 0,12))

  #Specify label type
  if ( identical(labtype, "IDs")) {
    labtypes <- IDs
  }  else {
    labtypes<- XYs
  }

  #Determine which labels to use
  if ( identical(nodelab, TRUE)) {
    nodelabs<-labtypes
  }  else {
    nodelabs<- ""
  }

  #Specify Frequency labels
  if ( identical(addFreq, TRUE)) {
    freqlabs <- paste(FREQ,"%", sep="")
    nlabs<-paste("n=", N, sep="")
  }  else {
    freqlabs<-""
    nlabs<-""
  }

  #Specify which frequency labels to use
  if ( identical(freqtype, "frq")) {
    freqlab <- freqlabs
  }  else {
    freqlab<-nlabs
  }

  #Specify if legend should be included
  if (identical (addleg, FALSE)) {

    #Generate plot without legend
    opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,3,2), pty="s", cex=1)
    graphics::par(mar=c(5,4,.1,.1))
    graphics::stars(x=data.frame(map_obj$codes), locations=map_obj$grid$pts,
                    draw.segments=TRUE, axes=TRUE, scale=TRUE,
                    len = 0.4, col.segments=colormod,
                    labels=NULL, ylim=c(0.5, ydim), lty='blank')

    #Specify which borders to use
    if(identical(map_obj$grid$topo, "rectangular")){
      graphics::symbols(map_obj$grid$pts,
                        squares = rep(1, nrow(map_obj$grid$pts)),
                        inches = FALSE, add = TRUE,
                        fg = "black", bg = NA)
    } else {graphics::symbols(map_obj$grid$pts,
                              circles = rep(.5, nrow(map_obj$grid$pts)),
                              inches = FALSE, add = TRUE,
                              fg = "black", bg = NA)}

    graphics::text(x=map_obj$grid$pts[,1], y=map_obj$grid$pts[,2]+.425,
                   labels=nodelabs, font=2, cex=labsize)

    graphics::text(x=map_obj$grid$pts[,1], y=map_obj$grid$pts[,2]-.4,
                   labels=freqlab,
                   font=2, cex=freqsize)

    on.exit(graphics::par(opar))

  } else {

    if (identical (legloc, "right")) {
    #Create Profile plot with legend on the right side
    opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,3,2), pty="s", cex=1)
    graphics::par(mfrow=c(1,1),mar=c(5,4,.1,.1), pty="m", xpd=FALSE)
    graphics::layout(matrix(c(1,1,2,2), 2, 2, byrow = FALSE), heights=c(1), widths=c(1,.3),respect = FALSE)

    graphics::stars(x=data.frame(map_obj$codes), locations=map_obj$grid$pts,
                    draw.segments=TRUE, axes=TRUE, scale=TRUE,
                    len = 0.4, col.segments=colormod,
                    labels=NULL, ylim=c(0.5, ydim), lty='blank')

    #Specify which borders to use
    if(identical(map_obj$grid$topo, "rectangular")){
      graphics::symbols(map_obj$grid$pts,
                        squares = rep(1, nrow(map_obj$grid$pts)),
                        inches = FALSE, add = TRUE,
                        fg = "black", bg = NA)
    } else {graphics::symbols(map_obj$grid$pts,
                              circles = rep(.5, nrow(map_obj$grid$pts)),
                              inches = FALSE, add = TRUE,
                              fg = "black", bg = NA)}

    graphics::text(x=map_obj$grid$pts[,1], y=map_obj$grid$pts[,2]+.425,
                   labels=nodelabs, font=2, cex=labsize)

    graphics::text(x=map_obj$grid$pts[,1], y=map_obj$grid$pts[,2]-.4,
                   labels=freqlab,
                   font=2, cex=freqsize)


    #Plot Legend on right panel
    graphics::par(mar=c(.1,.1,.1,.1))
    plot(x=rep(1.1,p), y=rev(1:p), pch=22, cex=legsymsize, col="black", bg=colormod,
         xlim=c(1, (p +.25)), axes=FALSE, xlab="", ylab="")

    graphics::box()
    graphics::axis(side=4, at=rev(1:p), pos=1.1, labels=varnames,
                   cex.axis=leglabsize, las=legtxtlas, tick=FALSE)

   #Reset plot window to normal
    on.exit(graphics::par(opar))
    on.exit(graphics::layout(1))

    } else {
      opar<-graphics::par(mfrow=c(1,1), mar=c(5,4,3,2), pty="s", cex=1)
      graphics::par(mfrow=c(1,1),mar=c(5,4,.1,.1), pty="m", xpd=FALSE)
      graphics::layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights=c(5,1), widths=c(1,1),respect = FALSE)

      graphics::stars(x=data.frame(map_obj$codes), locations=map_obj$grid$pts,
                      draw.segments=TRUE, axes=TRUE, scale=TRUE,
                      len = 0.4, col.segments=colormod,
                      labels=NULL, ylim=c(0.5, ydim), lty='blank')

      #Specify which borders to use
      if(identical(map_obj$grid$topo, "rectangular")){
        graphics::symbols(map_obj$grid$pts,
                          squares = rep(1, nrow(map_obj$grid$pts)),
                          inches = FALSE, add = TRUE,
                          fg = "black", bg = NA)
      } else {graphics::symbols(map_obj$grid$pts,
                                circles = rep(.5, nrow(map_obj$grid$pts)),
                                inches = FALSE, add = TRUE,
                                fg = "black", bg = NA)}

      graphics::text(x=map_obj$grid$pts[,1], y=map_obj$grid$pts[,2]+.425,
                     labels=nodelabs, font=2, cex=labsize)

      graphics::text(x=map_obj$grid$pts[,1], y=map_obj$grid$pts[,2]-.4,
                     labels=freqlab,
                     font=2, cex=freqsize)


      #Plot Legend on lower panel
      graphics::par(mar=c(rep(1,4)))
      plot(x=1:p, y=rep(.9,p), pch=22, cex=legsymsize, col="black", bg=colormod,
           ylim=c(0,1), xlim=c(1, (p +.25)), axes=FALSE, xlab="", ylab="")
      graphics::box(which="outer")
      graphics::axis(side=1, at=1:p, pos=0.85, labels=varnames, cex.axis=leglabsize, las=legtxtlas, tick=FALSE)

      #Reset plot window to normal
      on.exit(graphics::par(opar))
      on.exit(graphics::layout(1))

    }
  }
}






#' Plot TrajectoriesGraph object to PDF file
#'
#' @param g TrajectoriesGraph object
#' @param layout layout of an igraph object
#' @param outputPdfFullpath Full path to output PDF file
#' @param nodesizes Which values to use for node sizes. By default, uses count values of the nodes from TrajectoriesGraph object
#' @param linknumbers Which numbers to use when calculating the width of on edges. By default, uses numcohortExact values of the edges from TrajectoriesGraph object
#' @param linklabels Which numbers to show on edges. By default, uses linknumbers. Set to NA to use the default value.
#' @param title Title of the graph
#'
#' @return
#' @export
#'
#' @examples
plotTrajectoriesGraph<-function(g,layout=igraph::layout_nicely(g),outputPdfFullpath=F,nodesizes=igraph::V(g)$count,linknumbers=igraph::E(g)$numcohortExact, linklabels=NA, title="") {

  if(!'TrajectoriesGraph' %in% class(g)) stop('Error in plotTrajectoriesGraph(): object class of g must be TrajectoriesGraph.')


  if(outputPdfFullpath!=F) {
    #pointsize a bit depends on the number of nodes. pointsize=11 is OK of 20 nodes. pointsize=3 is OK for 300 events. The corresponding formula is:
    n=length(igraph::V(g))
    p=round(0.000066*n*n-0.05*n+12)
    pdf(outputPdfFullpath,width=11, height=8, paper="special", pointsize=p)
  }

  if(length(igraph::E(g))==0) {
    logger::log_warn('There are 0 edges on that graph. Nothing to plot.')
    return()
  }

  if(!is.vector(linknumbers) && linknumbers==F) {
    linknumbers=""
    normalizedLinknumbers=1
  } else {
    if(max(linknumbers)==min(linknumbers)) { #for cases when there is only 1 link
      normalizedLinknumbers=rep(1,length(linknumbers))
    } else {
      normalizedLinknumbers=linknumbers/max(linknumbers)
      #if(max(linknumbers)<=100) {
      #  normalizedLinknumbers=linknumbers/max(linknumbers)
      #} else {
      #  normalizedLinknumbers=(linknumbers - min(linknumbers))/(max(linknumbers)-min(linknumbers))
      #}
    }
  }
  edgewidth=sqrt(normalizedLinknumbers)*5
  edgelabel_cex=0.5+0.8*sqrt(normalizedLinknumbers) #font size

  #edge alpha depends on linknumbers
  rgb1<-sapply(igraph::E(g)$color ,col2rgb)
  edgecolor<-rgb(rgb1[1,],rgb1[2,],rgb1[3,],alpha=255*(0.2+0.8*sqrt(normalizedLinknumbers)),maxColorValue=255)
  #edge text color is darker
  edgelabelcolor<-rgb(round(rgb1[1,]/2),round(rgb1[2,]/2),round(rgb1[3,]/2),alpha=255*(0.2+0.8*sqrt(normalizedLinknumbers)),maxColorValue=255)

  #Update 15 Oct 2020: always turn 0-count edges to lightgray, font size 0 to suppress "0" label
  edgecolor[linknumbers==0]<-rgb(220,220,220,alpha=128,maxColorValue=255)
  edgewidth[linknumbers==0]<-1
  edgelabel_cex[linknumbers==0]<-0.5
  if(is.na(linklabels[1])) linklabels=linknumbers

  #normalized Event count
  igraph::V(g)$size<-nodesizes/max(nodesizes)

  #Update 15 Oct 2020: always turn 0-count nodes to lightgray
  vertexcolor=igraph::V(g)$color
  vertexcolor[nodesizes==0]<-rgb(196,196,196,alpha=128,maxColorValue=255)
  vertexlabelcolor=igraph::V(g)$labelcolor
  vertexlabelcolor[igraph::V(g)$size==0]<-rgb(196,196,196,alpha=128,maxColorValue=255)

  #E(g)$normalizedNumcohortCustom = (E(g)$numcohortCustom - min(E(g)$numcohortCustom))/(max(E(g)$numcohortCustom)-min(E(g)$numcohortCustom))

  plot(g,
       layout=layout,
       margin=0,
       #layout[,2]<-layout[,2]*2, #y-telje peal rohkem laiali
       vertex.color = vertexcolor,
       vertex.frame.color= vertexcolor,
       vertex.size=sqrt(igraph::V(g)$size)*10,
       vertex.label.font=1,
       vertex.label.cex = sqrt(1+igraph::V(g)$size),
       vertex.label.color = vertexlabelcolor,
       vertex.label = igraph::V(g)$concept_name,

       #vertex.label = paste(node_labels$dgn,node_labels$name),
       #edge.label=NA, #round(100*links$width),
       edge.label=linklabels, #round(100*links$width),
       #edge.label=E(g)$numcohortCustom, #round(100*links$width),
       edge.color=edgecolor,# E(g)$color,
       edge.label.font =1,
       edge.label.color=edgelabelcolor,
       edge.label.cex = edgelabel_cex,
       edge.width=edgewidth,
       edge.arrow.size=1, #sqrt(normalizedLinknumbers)*4,#iGraph currently always takes only the first value.
       #edge.arrow.width=round(E(g)$weight*1),
       edge.curved=0.1,

       vertex.label.dist=igraph::V(g)$size/10, #labeli kaugus nodest. Default=0 - siis on label node peal
       vertex.label.degree=-pi/2, #labeli asukoht node suhtes. 0=paremal. pi/2... jne vt https://www.rdocumentation.org/packages/igraph/versions/0.3.3/topics/tkplot
       asp=0
  )
  title(title,cex.main=1,col.main="black")

  if(outputPdfFullpath!=F) {
    dev.off()
    logger::log_info(paste0('PDF graph was written to ',outputPdfFullpath))
  }

}

#' Title
#'
#' @param g
#' @param layout
#' @param outputPdfFullpath
#'
#' @return
#' @export
#'
#' @examples
plotIgraph<-function(g,layout=layout_nicely(g),outputPdfFullpath=F,nodesizes=V(g)$count,linknumbers=E(g)$numcohortExact, title="") {



  #layout<-layout_as_tree(g, flip.y=T)


  if(outputPdfFullpath!=F) {
    #pointsize a bit depends on the number of nodes. pointsize=11 is OK of 20 nodes. pointsize=3 is OK for 300 events. The corresponding formula is:
    n=length(V(g))
    p=round(0.000066*n*n-0.05*n+12)
    pdf(outputPdfFullpath,width=11, height=8, paper="special", pointsize=p)
  }

  if(length(E(g))==0) {
    print('There are 0 edges on that graph. Nothing to plot.')
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

  #edge alpha depends on linknumbers
  rgb1<-sapply(E(g)$color ,col2rgb)
  edgecolor<-rgb(rgb1[1,],rgb1[2,],rgb1[3,],alpha=255*(0.2+0.8*sqrt(normalizedLinknumbers)),maxColorValue=255)
  #edge text color is darker
  edgelabelcolor<-rgb(round(rgb1[1,]/2),round(rgb1[2,]/2),round(rgb1[3,]/2),alpha=255*(0.2+0.8*sqrt(normalizedLinknumbers)),maxColorValue=255)


  #normalized Event count
  V(g)$size<-nodesizes/max(nodesizes)

  #E(g)$normalizedNumcohortCustom = (E(g)$numcohortCustom - min(E(g)$numcohortCustom))/(max(E(g)$numcohortCustom)-min(E(g)$numcohortCustom))

  plot(g,
       layout=layout,
       margin=0,
       #layout[,2]<-layout[,2]*2, #y-telje peal rohkem laiali
       vertex.color = V(g)$color,
       vertex.frame.color= V(g)$color,
       vertex.size=sqrt(V(g)$size)*10,
       vertex.label.font=1,
       vertex.label.cex = sqrt(1+V(g)$size),
       vertex.label.color = V(g)$labelcolor,

       #vertex.label = paste(node_labels$dgn,node_labels$name),
       #edge.label=NA, #round(100*links$width),
       edge.label=linknumbers, #round(100*links$width),
       #edge.label=E(g)$numcohortCustom, #round(100*links$width),
       edge.color=edgecolor,# E(g)$color,
       edge.label.font =1,
       edge.label.color=edgelabelcolor,
       edge.label.cex = 0.5+0.8*sqrt(normalizedLinknumbers),
       edge.width=sqrt(normalizedLinknumbers)*5,#round(20*E(g)$weight),
       edge.arrow.size=1, #sqrt(normalizedLinknumbers)*4,#iGraph currently always takes only the first value.
       #edge.arrow.width=round(E(g)$weight*1),
       edge.curved=0.1,
       #weights=E(g)$weight,
       vertex.label.dist=V(g)$size/10, #labeli kaugus nodest. Default=0 - siis on label node peal
       vertex.label.degree=-pi/2, #labeli asukoht node suhtes. 0=paremal. pi/2... jne vt https://www.rdocumentation.org/packages/igraph/versions/0.3.3/topics/tkplot
       asp=0
  )
  title(title,cex.main=1,col.main="black")

  if(outputPdfFullpath!=F) {
    dev.off()
    print(paste0('PDF graph was written to ',outputPdfFullpath))
  }

}

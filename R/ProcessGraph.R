
RECT.WIDTH=400
RECT.HEIGHT=140
RECT.XMARGIN=200
RECT.YMARGIN=100

drawProcessGraph<-function(annotated.pairs,filename='myplot.pdf',trajectoryAnalysisArgs, title='General process') {

  logger::log_info('Drawing process graph...')

  require(grDevices)

  pairs<-annotated.pairs

  NUM.PAIRS=nrow(pairs)

  # identify filters
  stat<-pairs %>% group_by(FAILED_FILTER) %>% summarise(n=n())
  filters<-stat %>% filter(FAILED_FILTER!='') %>% arrange(FAILED_FILTER)
  NUM.SPLITS=nrow(filters)



  # Open a pdf file
  pdf(filename,
      width=Trajectories:::getImgWidth()/72,
      height=Trajectories:::getImgHeight(num.splits=NUM.SPLITS)/72
      )



  #Create empty plot
  plot(x=c(0, Trajectories:::getImgWidth()),
       y=c(0, Trajectories:::getImgHeight(num.splits=NUM.SPLITS)),
       type = "n",
       xlab = "",
       ylab = "",
       xaxt='n',
       yaxt='n',
       axes=FALSE,
       frame.plot=FALSE,
       main = title,
       cex.main=2
  )

  # INITIAL NODE
  Trajectories:::trajectoryRect(grid.x=1, grid.y=1, n=NUM.PAIRS, n.total=NUM.PAIRS, text='Total number of event pairs to analyze', num.splits=NUM.SPLITS)
  Trajectories:::bottomArrow(grid.x=1, grid.y=1, text="", num.splits=NUM.SPLITS)

  if(nrow(filters)>0) {
    for(f in 1:nrow(filters)) {
      t=filters[f,'FAILED_FILTER']
      n=filters[f,'n']
      Trajectories:::addSplit(y.step=1+f, text=t, n.no=n, n.total=NUM.PAIRS, num.splits=NUM.SPLITS)
    }
  } else {
    f=0
  }

  # FINAL NODE
  n=stat %>% filter(FAILED_FILTER=='')
  if(nrow(n)>0) {
    n<-n %>% pull(n)
  } else {
    #no directional pairs found
   n=0
  }
  Trajectories:::trajectoryRect(grid.x=1, grid.y=1+f+1, n=n, n.total=NUM.PAIRS, text='Directional event pairs', num.splits=NUM.SPLITS)




  #store to file
  #dev.print(png, file = filename, width = getImgWidth(), height = Trajectories:::getImgHeight(num.splits=NUM.SPLITS))
  # Close the pdf file
  dev.off()

  logger::log_info('...done. Process graph saved as PNG image to {filename}.')

}





#arrows(x.center, y.offset -2*fontsize, x1 = x.center, y1 = y.offset-10*fontsize, length = 0.1, angle = 30, code = 2)






getImgWidth<-function() {
  return(2*(Trajectories:::RECT.WIDTH+Trajectories:::RECT.XMARGIN))
}
getImgHeight<-function(num.splits=4) {
  total.num.rows=num.splits+2
  return(total.num.rows*(Trajectories:::RECT.YMARGIN+Trajectories:::RECT.HEIGHT))
}

getPosFromGrid<-function(grid.x=2,grid.y=1,num.splits=4) {

  x=(grid.x-1 +0.5)*(Trajectories:::RECT.WIDTH+Trajectories:::RECT.XMARGIN) - Trajectories:::RECT.WIDTH/2
  y=Trajectories:::getImgHeight(num.splits=num.splits) - (grid.y-1 +0.5)*(Trajectories:::RECT.HEIGHT+Trajectories:::RECT.YMARGIN) + Trajectories:::RECT.HEIGHT/2

  return(c(x,y))
}

bottomArrow<-function(grid.x=1,grid.y=1,text='Yes', num.splits=4) {

  xy=Trajectories:::getPosFromGrid(grid.x=grid.x,grid.y=grid.y, num.splits=num.splits)
  x=xy[1]
  y=xy[2]

  arrows(x0=x+Trajectories:::RECT.WIDTH/2, y-Trajectories:::RECT.HEIGHT, x1 = x+Trajectories:::RECT.WIDTH/2, y1 = y-Trajectories:::RECT.HEIGHT-Trajectories:::RECT.YMARGIN, length = 0.3, angle = 30, code = 2, lwd=2)
  text(x=x+Trajectories:::RECT.WIDTH/2 + 30,
       y=y-Trajectories:::RECT.HEIGHT -Trajectories:::RECT.YMARGIN/2,
       #adj = c(0,0), #left-justification
       text,
       cex=2)
}

rightArrow<-function(grid.x=1,grid.y=1,text='No', num.splits=4) {

  xy=Trajectories:::getPosFromGrid(grid.x=grid.x,grid.y=grid.y, num.splits=num.splits)
  x=xy[1]
  y=xy[2]

  arrows(x0=x+Trajectories:::RECT.WIDTH, y-Trajectories:::RECT.HEIGHT/2, x1 = x+Trajectories:::RECT.WIDTH+RECT.XMARGIN, y1 = y-Trajectories:::RECT.HEIGHT/2, length = 0.3, angle = 30, code = 2, lwd=2)
  text(x=x+Trajectories:::RECT.WIDTH+Trajectories:::RECT.XMARGIN/2,
       y=y-Trajectories:::RECT.HEIGHT/2+Trajectories:::RECT.HEIGHT/4,
       #adj = c(0,0), #left-justification
       text,
       cex=2)
}

trajectoryRect<-function(grid.x=1, grid.y=1, n=4625, n.total=7733, text='Low power for detecting RR as close to 1 as in discovery study', num.splits=3) {

  xy=Trajectories:::getPosFromGrid(grid.x=grid.x,grid.y=grid.y, num.splits=num.splits)
  x=xy[1]
  y=xy[2]

  row.height=30

  rect(xleft=x, ybottom=y-Trajectories:::RECT.HEIGHT, xright=x+Trajectories:::RECT.WIDTH, ytop=y, lwd=2)

  if(!is.na(text)) {
    text2<-strwrap(text, width = 26) #wrap to 26-character length strings
    if(!is.na(n)) text2<-c(text2,paste0("n=",n," (",round(n*100/n.total),'%)'))
  } else {
    if(!is.na(n)) {
      text2<-c(paste0("n=",n," (",round(n*100/n.total,1),'%)'))
    } else {
      text2<-c()
    }
  }

  if(length(text2)>0) {
    for(i in 1:length(text2)) {
      #print(paste(i,y-rect.height/2 + (length(text2)/2)*row.height - (i-1)*row.height))
      text(x=x+Trajectories:::RECT.WIDTH/2,
           y=y-Trajectories:::RECT.HEIGHT/2 + ((length(text2)-1)/2)*row.height - (i-1)*row.height,
           #adj = c(0,0), #left-justification
           text2[i],
           cex=2)
    }
  }

}




addSplit<-function(y.step=1, text='Blah blah blah', n.no=555, n.total=10000, num.splits=4) {


  Trajectories:::trajectoryRect(grid.x=1, grid.y=y.step, n=NA, n.total=n.total, text=text, num.splits=num.splits)

  Trajectories:::rightArrow(grid.x=1, grid.y=y.step, text="No", num.splits=num.splits)
  Trajectories:::bottomArrow(grid.x=1, grid.y=y.step, text="Yes", num.splits=num.splits)

  Trajectories:::trajectoryRect(grid.x=2, grid.y=y.step,  n=n.no, n.total=n.total, text=NA, num.splits=num.splits)

}



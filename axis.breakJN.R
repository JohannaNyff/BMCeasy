### Function adapted from https://www.rdocumentation.org/packages/plotrix/versions/3.7-6/topics/axis.break
### and slightly modified.

# axis.break places a break marker at the position "breakpos" 
# in user coordinates on the axis nominated - see axis().

axis.breakJN<-function(axis=1,breakpos,bgcol="white",breakcol="black",
                     style=c("slash","zigzag"),brw=0.02, Lwd=1) {
  
  if(!missing(breakpos)) {
    # get the coordinates of the outside of the plot
    figxy<-par("usr")
    # flag if either axis is logarithmic
    xaxl<-par("xlog")
    yaxl<-par("ylog")
    # calculate the x and y offsets for the break
    xw<-(figxy[2]-figxy[1])*brw
    yw<-(figxy[4]-figxy[3])*brw
    if(xaxl && (axis == 1 || axis == 3)) breakpos<-log10(breakpos)
    if(yaxl && (axis == 2 || axis == 4)) breakpos<-log10(breakpos)
    # set up the "blank" rectangle (left, bottom, right, top)
    switch(axis,
           br<-c(breakpos-xw/2,figxy[3]-yw/2,breakpos+xw/2,figxy[3]+yw/2),
           br<-c(figxy[1]-xw/2,breakpos-yw/2,figxy[1]+xw/2,breakpos+yw/2),
           br<-c(breakpos-xw/2,figxy[4]-yw/2,breakpos+xw/2,figxy[4]+yw/2),
           br<-c(figxy[2]-xw/2,breakpos-yw/2,figxy[2]+xw/2,breakpos+yw/2),
           stop("Improper axis specification."))
    # get the current setting of xpd
    old.xpd<-par("xpd")
    # don't cut the break off at the edge of the plot
    par(xpd=T)
    # correct for logarithmic axes
    if(xaxl) br[c(1,3)]<-10^br[c(1,3)]
    if(yaxl) br[c(2,4)]<-10^br[c(2,4)]
    # draw the "blank" rectangle
    rect(br[1],br[2],br[3],br[4],col=bgcol,border=bgcol)
    if(style == "slash") {
      # calculate the slash ends
      if(axis == 1 || axis == 3) {
        xbegin<-c(breakpos-xw,breakpos)
        xend<-c(breakpos,breakpos+xw)
        ybegin<-c(br[2],br[2])
        yend<-c(br[4],br[4])
        if(xaxl) {
          xbegin<-10^xbegin
          xend<-10^xend
        }
      }
      else {
        xbegin<-c(br[1],br[1])
        xend<-c(br[3],br[3])
        ybegin<-c(breakpos-yw,breakpos)
        yend<-c(breakpos,breakpos+yw)
        if(yaxl) {
          ybegin<-10^ybegin
          yend<-10^yend
        }
      }
    }
    else {
      # calculate the zigzag ends
      if(axis == 1 || axis == 3) {
        xbegin<-c(breakpos-xw/2,breakpos-xw/4,breakpos+xw/4)
        xend<-c(breakpos-xw/4,breakpos+xw/4,breakpos+xw/2)
        ybegin<-c(ifelse(yaxl,10^figxy[3+(axis==3)],figxy[3+(axis==3)]),br[4],br[2])
        yend<-c(br[4],br[2],ifelse(yaxl,10^figxy[3+(axis==3)],figxy[3+(axis==3)]))
        if(xaxl) {
          xbegin<-10^xbegin
          xend<-10^xend
        }
      }
      else {
        xbegin<-c(ifelse(xaxl,10^figxy[1+(axis==4)],figxy[1+(axis==4)]),br[1],br[3])
        xend<-c(br[1],br[3],ifelse(xaxl,10^figxy[1+(axis==4)],figxy[1+(axis==4)]))
        ybegin<-c(breakpos-yw/2,breakpos-yw/4,breakpos+yw/4)
        yend<-c(breakpos-yw/4,breakpos+yw/4,breakpos+yw/2)
        if(yaxl) {
          ybegin<-10^ybegin
          yend<-10^yend
        }
      }
    }
    # draw the segments
    segments(xbegin,ybegin,xend,yend,col=breakcol,lty=1, lwd=Lwd)
    # restore xpd
    par(xpd=old.xpd)
  }
  else {
    cat("Usage: axis.break(axis=1,breakpos,bgcol=\"white\",breakcol=\"black\",\n")
    cat("\tstyle=c(\"slash\",\"zigzag\"),brw=0.02)\n")
  }
}
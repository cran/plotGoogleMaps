bubbleLegend <-
function (shape="t",
                        attribute,
                        colPalette=NULL,
                        legendName="Legend",
                        bgc='#B0C4DEFF',
                        scale.level=1,
                        strokeColor="#FFAA00") {

if(strokeColor==""){strokeColor='black'}
png(filename =paste(legendName,'.png',sep=""), width=nlevels(factor(attribute))*50,
       height=nlevels(factor(attribute))*50,units = "px", bg="white")
par(mai=c(0,0,0,0),bg=bgc)
plot(0,xlab="",ylab="",type="n", axes=F,xlim=c(0,3*nlevels(factor(attribute))),
                                       ylim=c(0,3*nlevels(factor(attribute)) ))

pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")

		niv  <- levels(factor(attribute))
	if(is.null(colPalette)){
	  cols<-pal(length(niv))
    }else{cols <-colPalette}
    
    

	k=1

	if (shape=="t"){
	              for(i in nlevels(factor(attribute)):1) {
	              x<-1.5+ cos(seq(-pi/6,2*pi,by=pi*2/3)) *1.5*scale.level[k]
	              y<- i*3-1.5+ sin(seq(-pi/6,2*pi,by=pi*2/3)) *1.5*scale.level[k]
	              polygon(x,
		            y,
			          col=cols[k],
                border=strokeColor)
                text(6,i*3-1.5,niv[k],cex=1)
                k=k+1}
                graph1 <- dev.cur()
                dev.off(graph1)

  }else if (shape=="q"){
         for(i in nlevels(factor(attribute)):1) {
	              x<-1.5+ cos(seq(-pi/4,2*pi,by=pi/2)) *1.5*scale.level[k]
	              y<- i*3-1.5+ sin(seq(-pi/4,2*pi,by=pi/2)) *1.5*scale.level[k]
	              polygon(x,
		            y,
			          col=cols[k],
                border=strokeColor)
                text(6,i*3-1.5,niv[k],cex=1)
                k=k+1}
                graph1 <- dev.cur()
                dev.off(graph1)
     }   else{
             for(i in nlevels(factor(attribute)):1) {
             x<-1.5+cos(c(seq(0,2*pi,.2),0)) *1.5*scale.level[k]
             y<-i*3-1.5+sin(c(seq(0,2*pi,.2),0)) *1.5*scale.level[k]
          polygon(x,
		            y,
			          col=cols[k],
                border=strokeColor)
                text(6,i*3-1.5,niv[k],cex=1)
                k=k+1}
                graph1 <- dev.cur()
                dev.off(graph1)
             }


    }

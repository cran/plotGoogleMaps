lineLegend <-
function (attribute,colPalette,linew=1,legendName="Legend",bgc='#B0C4DEFF') {
                       
		niv  <- attribute 

png(filename =paste(legendName,'.png',sep=""), width=250, height=length(niv)*25,units = "px", bg="white")
par(mai=c(0,0,0,0),bg=bgc)
plot(0,xlab="",ylab="",type="n", axes=F,xlim=c(0,3),ylim=c(0,length(niv)))

  if (length(niv)!=length(linew)) {
                           linew=rep(linew[1],length(niv)) }
cols <-colPalette

k=1
	for(i in nlevels(factor(attribute)):1) {
	segments( 0 ,i-.5    , 1 , i-.5 ,
			col=cols[k], lwd=linew[k])
	 text(1.4,i-.5,niv[k],cex=1)
    k=k+1}
    graph1 <- dev.cur()
     dev.off(graph1)
    }

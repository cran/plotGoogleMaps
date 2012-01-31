segmentLegend <-
function (attribute,
                        colPalette=rainbow(length(attribute)),
                        border=NA,
                        legendName="Legend",
                        bgc='#B0C4DEFF') {


png(filename=paste(legendName,'.png',sep=""), width=220,
       height=220,units = "px", bg="white")
par(par(mai=c(0.25,1,0.25,0.25),
omi=c(0.1,0.1,0.1,0.1)),bg=bgc)


	niv  <- attribute
  cols <-as.character(substr(colPalette,1,7))

                  x<-rep(1,length(niv))

            pie(  x,
                  labels=niv,
                  clockwise=FALSE,
                  radius=1,
                  col= cols,
                  bg=bgc,
                  init.angle=90)

                graph1 <- dev.cur()
                dev.off(graph1)



    }

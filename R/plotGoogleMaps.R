plotGoogleMaps <-
function(SP,
                            filename="",
                            zcol=1,
                            add=FALSE,
                            previousMap=NULL,
                            colPalette=NULL,
                            strokeColor="#FFAA00",
                            strokeOpacity=1,
                            strokeWeight=1,
                            geodesic=TRUE,
                            clickable=TRUE,
                            draggableMarker=FALSE,
                            flat=TRUE,
                            visible=TRUE,
                            zIndex="null",
                               map.width="80%",
                               map.height="100%",
                               layerName="",
                               control.width="20%",
                               control.height="100%",
                               zoom=15,
                               fitBounds=TRUE,
                               mapTypeId = "HYBRID",
                               disableDoubleClickZoom =FALSE,
                               draggable= TRUE ,
                               keyboardShortcuts=TRUE,
                               mapTypeControlOptions='DEFAULT',
                               navigationControl=TRUE,
                               navigationControlOptions='DEFAULT',
                               scaleControlOptions= 'STANDARD',
                               noClear=FALSE,
                               scrollwheel =TRUE     ,
                               streetViewControl= FALSE ){
 
 #################################################################################
#################################################################################

createPolygon<-function(oneSPpolygonsSlot,
                         name="polygon",
                         fillColor="#00AAFF",
                         fillOpacity=0.5,
                         map="map",
                         strokeColor="#FFAA00",
                         strokeOpacity=1,
                         strokeWeight=1,
                         geodesic=TRUE,
                         clickable=TRUE,
                         zIndex="null") {

              if (clickable!=FALSE)
                  {clickable='true'}else{ clickable='false' }

              if (geodesic!=FALSE)
                 {geodesic='true' }else{  geodesic='false'}

listOfPolygonsPerPolygon<-sapply(slot(oneSPpolygonsSlot, "Polygons"),
 function(x) slot(x, "coords"))
plotOrder<-slot(oneSPpolygonsSlot, "plotOrder")
pts<-rep("",length(plotOrder))

   if (length(plotOrder)>1) {

              for(j in 1:(length(plotOrder)-1)){
              lonlat<-listOfPolygonsPerPolygon[[plotOrder[j]]]
              pts[j]=paste('new google.maps.LatLng(',
                           lonlat[1:((length(lonlat)/2-1)),2],',',
                           lonlat[1:((length(lonlat)/2-1)),1],'),\n',collapse="")
              pts[j]=paste(pts[j],'new google.maps.LatLng(',
                           lonlat[length(lonlat)/2,2],',',lonlat[length(lonlat)/2,1],')')
              pts[j]=paste('[',pts[j],'], \n')
              }
       lonlat<-listOfPolygonsPerPolygon[[plotOrder[j+1]]]
       pts[j+1]=paste('new google.maps.LatLng(',
                       lonlat[1:((length(lonlat)/2-1)),2],',',
                       lonlat[1:((length(lonlat)/2-1)),1],'),\n',collapse="")
       pts[j+1]=paste(pts[j+1],'new google.maps.LatLng(',
                     lonlat[length(lonlat)/2,2],',',lonlat[length(lonlat)/2,1],')')
       pts[j+1]=paste('[',pts[j+1],'] \n')
       xxx=paste(pts[1:(j+1)],collapse="")
       paths=paste('[',xxx,']')
         }else{

       lonlat<-slot(slot(oneSPpolygonsSlot, "Polygons")[[1]],"coords")
       paths=paste('new google.maps.LatLng(',
                   lonlat[1:((length(lonlat)/2-1)),2],',',
                   lonlat[1:((length(lonlat)/2-1)),1],'),\n',collapse="")
       paths=paste('[',paths,'new google.maps.LatLng(',
                   lonlat[length(lonlat)/2,2],',',lonlat[length(lonlat)/2,1],')]')

  }






x<-paste('var ',name,'= new google.maps.Polygon({ \n path:',paths,', \n','map:',
         map,', \n clickable:',clickable,',\n fillColor: "',fillColor,
         '",\n strokeColor: "',strokeColor,'", \n strokeOpacity:',
         strokeOpacity,',\n fillOpacity:',fillOpacity,',\n strokeWeight:',
         strokeWeight,',\n geodesic:',geodesic,',\n zIndex:',zIndex,'});',sep="")

return(x)

}
##########################################################################

createLine<-function(lonlatmatrix,
                     name="line",
                     map="map",
                     strokeColor="#FFAA00",
                     strokeOpacity=1,
                     strokeWeight=1,
                     geodesic=TRUE,
                     clickable=TRUE,
                     zIndex="null") {

              if (clickable!=FALSE)
                  {clickable='true'}
              else {
                  clickable='false'
             }

              if (geodesic!=FALSE)
                { geodesic='true'}
              else{
                 geodesic='false'
              }

pts=paste('new google.maps.LatLng(',lonlatmatrix[1:((length(lonlatmatrix)/2-1)),2],
          ',',lonlatmatrix[1:((length(lonlatmatrix)/2-1)),1],'),\n',collapse="")
pts=paste('[',pts,'new google.maps.LatLng(',lonlatmatrix[length(lonlatmatrix)/2,2],
          ',',lonlatmatrix[length(lonlatmatrix)/2,1],')]')

x<-paste('var ',name,'= new google.maps.Polyline({ \n path:',pts,', \n','map:',
          map,', \n clickable:',clickable,',\n strokeColor: "',strokeColor,
          '", \n strokeOpacity:',strokeOpacity,',\n strokeWeight:',strokeWeight,
          ',\n geodesic:',geodesic,',\n zIndex:',zIndex,'});')
return(x)

}
###############################################################################
createMarker<-function(lonlat,
                       name="marker",
                       title="Point" ,
                       map="map",
                       clickable=TRUE,
                       draggable=FALSE,
                       flat=TRUE,
                       visible=TRUE,
                       zIndex="null",...) {
# ...  shape="" , icon="",shadow="",cursor=""

              if (clickable!=FALSE)
                 { clickable='true' }
              else { clickable='false'}

              if (draggable!=FALSE)
                 {draggable='true'}
              else  {draggable='false'}

              if (flat!=FALSE)
                 flat='true'
              else { flat='false'}

              if (visible!=FALSE)
                  visible='true'
              else {visible='false'}



x<-paste('var ',name,'= new google.maps.Marker({ \n
         position: new google.maps.LatLng(',lonlat[2],',',lonlat[1],'),
         \n','map:',map,',\n','title:',title,',\n clickable:',clickable,',
         \n draggable:',draggable,', \n flat:',flat,',\n visible:',visible,',','
         \n zIndex:',zIndex)

argsList <- list(...)
vectorNames<-names(argsList)

 if (length(argsList)>1){

     for(i in 1:(length(argsList)-1)) {
     x<-paste(x,', \n',vectorNames[i],':"',argsList[[i]],'"',sep="")
     }
     x<-paste(x,', \n',vectorNames[i+1],':"',argsList[[i+1]],'"}); ',sep="")
  } else { if( length(argsList)==1 ){
          x<-paste(x,', \n',vectorNames[1],':"',argsList[[1]],'"',sep="")}

   x<-paste(x,'}); ') }
   return(x)

}
################################################################################

createInitialization<-function(SP,
                               name="map",
                               zoom=15,
                               mapTypeId = "HYBRID",
                               fitBounds=TRUE,
                               add=FALSE,
                               divname="map_canvas",
                               disableDefaultUI=FALSE,
                               disableDoubleClickZoom =FALSE,
                               draggable= TRUE ,
                               keyboardShortcuts=TRUE,
                               mapTypeControlOptions='DEFAULT',
                               navigationControl=TRUE,
                               navigationControlOptions='DEFAULT',
                               noClear=FALSE,
                               scaleControl=TRUE,
                               scaleControlOptions= 'STANDARD',
                               scrollwheel =TRUE     ,
                               streetViewControl= FALSE) {


                                if (scaleControl!=FALSE)
                                   {scaleControl='true'}else{ scaleControl='false' }

                               if (disableDefaultUI!=FALSE)
                                   {disableDefaultUI='true'}else{ disableDefaultUI='false' }

                               if (disableDoubleClickZoom!=FALSE)
                                   {disableDoubleClickZoom='true'}else{ disableDoubleClickZoom='false' }

                               if (draggable!=FALSE)
                                   {draggable='true'}else{ draggable='false' }

                               if (keyboardShortcuts!=FALSE)
                                   {keyboardShortcuts='true'}else{ keyboardShortcuts='false' }

                               if (navigationControl!=FALSE)
                                   {navigationControl='true'}else{ navigationControl='false' }

                               if (noClear!=FALSE)
                                   {noClear='true'}else{ noClear='false' }

                               if (scaleControl!=FALSE)
                                   {scaleControl='true'}else{ scaleControl='false'
                                    scaleControlOptions='null'}

                               if (scrollwheel!=FALSE)
                                   {scrollwheel='true'}else{ scrollwheel='false' }

                               if (streetViewControl!=FALSE)
                                   {streetViewControl='true'}else{ streetViewControl='false' }


mapTypeId<-paste('google.maps.MapTypeId.',mapTypeId,sep="")
mapTypeControlOptions<-paste('{style: google.maps.MapTypeControlStyle.',
                                      mapTypeControlOptions,'}',sep="")
navigationControlOptions<-paste('{style: google.maps.NavigationControlStyle.',
                                navigationControlOptions ,'}',sep="")
if(!is.null(scaleControlOptions)){
scaleControlOptions<-paste('{style: google.maps.ScaleControlStyle.',
scaleControlOptions ,'}',sep="")  }

SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))
Centar=c(mean(SP.ll@bbox[1,]),mean(SP.ll@bbox[2,]))
sw<-c(SP.ll@bbox[2,1],SP.ll@bbox[1,1])
ne<-c(SP.ll@bbox[2,2],SP.ll@bbox[1,2])



x<-'function initialize() { \n'

x<-paste(x,'var latlng = new google.maps.LatLng(',Centar[2],',',Centar[1],') ; \n')
x<-paste(x,'\n var myOptions = { zoom:',zoom,', \n center: latlng',', \n mapTypeId:',mapTypeId,
' ,\n disableDefaultUI:',disableDefaultUI,' ,\n disableDoubleClickZoom:',disableDoubleClickZoom,
' ,\n  draggable:',draggable,' ,\n  keyboardShortcuts: ', keyboardShortcuts,
' ,\n mapTypeControlOptions:', mapTypeControlOptions,  ' ,\n  navigationControl:',navigationControl,
' ,\n navigationControlOptions:',navigationControlOptions,' ,\n noClear:',noClear,
 ' ,\n scaleControl:', scaleControl ,' ,\n scaleControlOptions:',scaleControlOptions,
' ,\n  scrollwheel:', scrollwheel, ' ,\n streetViewControl:',streetViewControl,'} ; \n')



x<-paste(x,'\n',name)
x<-paste(x,'= new google.maps.Map(document.getElementById("',divname,'"),myOptions); \n',sep="")

if( fitBounds==TRUE){
x<-paste(x,' ',name,'.fitBounds(new google.maps.LatLngBounds(
 new google.maps.LatLng(',sw[1],',',sw[2],'),
 new google.maps.LatLng( ',ne[1],',',ne[2],'))); ',sep="")  }

if (add==FALSE){x<- paste(x,'}')}

return(x)


}
################################################################################
createInfoWindowEvent<-function(Line_or_Polygon,
                                map="map",
                                event="click",
                                content="The content",
                                position="event.latLng",
                                disableAutoPan=FALSE,
                                maxWidth="null",
                                pixelOffset="null",
                                zIndex="null") {

             if (disableAutoPan!=FALSE)
                  {disableAutoPan='true'}else{ disableAutoPan='false' }


          if (is.numeric(position)) {
          position=paste('position=new google.maps.LatLng(',position[2],',',position[1],')')
          }
           if (is.numeric(position)) {
          pixelOffset=paste('new google.maps.Size',pixelOffset[1],',',pixelOffset[1],')')
          }
x=paste( ' var infowindow = new google.maps.InfoWindow({ content: "" }); \n google.maps.event.addListener(',Line_or_Polygon,
',"',event,'",function(event){ \n infowindow.content="',
content,'"; \n  infowindow.position =',position,
'; \n infowindow.disableAutoPan=', disableAutoPan,
'; \n infowindow.maxWidth=',maxWidth,
';\n infowindow.pixelOffset=',
pixelOffset,';\n infowindow.zIndex='
,zIndex,'; infowindow.open(',map,')}); ',sep="")

return(x)

}
################################################################################
createInfoWindowEventM<-function(Marker,
                                map="map",
                                event="click",
                                content="The content",
                                position="event.latLng",
                                disableAutoPan=FALSE,
                                maxWidth="null",
                                pixelOffset="null",
                                zIndex="null") {

             if (disableAutoPan!=FALSE)
                  {disableAutoPan='true'}else{ disableAutoPan='false' }


          if (is.numeric(position)) {
          position=paste('position=new google.maps.LatLng(',position[2],',',position[1],')')
          }
           if (is.numeric(position)) {
          pixelOffset=paste('new google.maps.Size',pixelOffset[1],',',pixelOffset[1],')')
          }
x=paste( ' var infowindow = new google.maps.InfoWindow({ content: "" }); \n google.maps.event.addListener(',Marker,
',"',event,'",function(event){ \n infowindow.content="',
content,'"; \n  infowindow.position =',position,
'; \n infowindow.disableAutoPan=', disableAutoPan,
'; \n infowindow.maxWidth=',maxWidth,
';\n infowindow.pixelOffset=',
pixelOffset,';\n infowindow.zIndex='
,zIndex,'; infowindow.open(',map,',' ,Marker,')}); ',sep="")

return(x)

}
################################################################################
PolyCol<-function(attribute,colPalette=NULL) {
      # attribute=soil.ll@data$ID
if (length(colPalette)>1){
   if(!is.numeric(attribute) && !is.character(attribute) ) {

              if(nlevels(attribute)== length(colPalette)){
                x<-factor(attribute,labels=colPalette)
                return(as.character(substr(x,1,7)))
                }else{
                stop("length of colPallete should match number of factor levels")
                }
   }else{

    if( is.numeric(attribute)){
     x<-factor(cut(attribute,length(colPalette) ),labels=colPalette)
     return(as.character(substr(x,1,7)))
     }
   }

 if(nlevels(factor(attribute))== length(colPalette)){
 x<-factor(factor(attribute),labels=colPalette)
         return(as.character(substr(x,1,7)))
         }else{
          stop("length of colPallete should match number of factor levels")
         }
}else{
     if(!is.null(colPalette)){
      x<-rep(colPalette,length(attribute))
       return(substr(x,1,7))}else{
        x<-factor(factor(attribute),labels=rainbow(nlevels(factor(attribute))))
         return(as.character(substr(x,1,7)))
       }
}

}
#################################################################################
polyLegend<-function (attribute,colPalette=NULL,legendName="Legend",bgc='#B0C4DEFF') {
png(file=paste(legendName,'.png',sep=""), width=250, height=nlevels(factor(attribute))*25,units = "px", bg="white")
par(mai=c(0,0,0,0),bg=bgc)
plot(0,xlab="",ylab="",type="n", axes=FALSE,xlim=c(0,3),ylim=c(0,nlevels(factor(attribute))))


		niv  <- levels(factor(attribute))
	if(is.null(colPalette)){
	  cols<-rainbow(length(niv))
    }else{cols <-colPalette}

	k=1
	for(i in nlevels(factor(attribute)):1) {
	polygon(c(1  ,1   ,0  ,0, 1),
		        c(i-1  ,i    , i , i-1 , i-1),
			col=cols[k])
	 text(2,i-.5,niv[k],cex=1)
    k=k+1}
     graph1 <- dev.cur()
     dev.off(graph1)
    }

#################################################################################
lineLegend<-function (attribute,colPalette,legendName="Legend",bgc='#B0C4DEFF') {
png(file=paste(legendName,'.png',sep=""), width=250, height=nlevels(factor(attribute))*25,units = "px", bg="white")
par(mai=c(0,0,0,0),bg=bgc)
plot(0,xlab="",ylab="",type="n", axes=FALSE,xlim=c(0,3),ylim=c(0,nlevels(factor(attribute))))

	niv  <- levels(factor(attribute))
if(is.null(colPalette)){
	  cols<-rainbow(length(niv))
    }else{cols <-colPalette}
	k=1
	for(i in nlevels(factor(attribute)):1) {
	segments( 0 ,i-.5    , 1 , i-.5 ,
			col=cols[k], lwd=3)
	 text(1.4,i-.5,niv[k],cex=1)
    k=k+1}
    graph1 <- dev.cur()
     dev.off(graph1)
    }

################################################################################
rasterLegend<-function (attribute,colPalette,legendName="Legend",bgc='#B0C4DEFF') {
png(file=paste(legendName,'.png',sep=""), width=250, height=nlevels(factor(attribute))*25,units = "px", bg="white")
par(mai=c(0,0,0,0),bg=bgc)
plot(0,xlab="",ylab="",type="n", axes=FALSE,xlim=c(0,3),ylim=c(0,nlevels(factor(attribute))))
	niv  <- levels(factor(attribute))
	  if(is.null(colPalette)){
	  cols<-rainbow(length(niv))
    }else{cols <-colPalette}
	k=1
	for(i in nlevels(factor(attribute)):1) {
	polygon(c(1  ,1   ,0  ,0, 1),
		        c(i-1  ,i    , i , i-1 , i-1),
			col=cols[k], border=NA)
	 text(2,i-.5,niv[k],cex=1)
    k=k+1}
     graph1 <- dev.cur()
     dev.off(graph1)
    }
 ###############################################################################
 ###############################################################################
 
 SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))
disableDefaultUI=FALSE
Centar=c(mean(SP.ll@bbox[1,]),mean(SP.ll@bbox[2,]))
sw<-c(SP.ll@bbox[2,1],SP.ll@bbox[1,1])
ne<-c(SP.ll@bbox[2,2],SP.ll@bbox[1,2])
attribute=SP@data[,zcol]
nameOfSP<-sapply(as.list(substitute({SP})[-1]), deparse)
nameOfSP<-gsub('[!,",#,$,%,&,(,),*,+,-,.,/,:,;,<,=,>,?,@,^,`,|,~]', "_", nameOfSP)
nameOfSP<-gsub('[[]', "_", nameOfSP)
nameOfSP<-gsub('[]]', "_", nameOfSP)
if(filename==""){
filename <- paste(nameOfSP,'.htm',sep="")}

if(layerName==""){
layerName=nameOfSP}

rgb<-col2rgb(strokeColor)
strokeColor<-rgb(rgb[1],rgb[2],rgb[3],maxColorValue=255)

if (!is.list(previousMap)) {
functions<-""
# Creating functions for checkbox comtrol, Show , Hide and Toggle control
# Set of JavaScript functionalities
functions<-paste(functions,' function setOpacL(MLPArray,textname) {
opacity=0.01*parseInt(document.getElementById(textname).value) \n
for (var i = 0; i < MLPArray.length; i++) { MLPArray[i].setOptions
({strokeOpacity: opacity}); } } \n',sep="")
functions<-paste(functions,'function showO(MLPArray,boxname) { \n
for (var i = 0; i < MLPArray.length; i++) { \n MLPArray[i].setMap(map); } \n
 document.getElementById(boxname).checked = true; } \n ',sep="")
functions<-paste(functions,'function hideO(MLPArray,boxname) { \n
for (var i = 0; i < MLPArray.length; i++) { \n MLPArray[i].setMap(null);} \n
 document.getElementById(boxname).checked = false; } \n ',sep="")
functions<-paste(functions,'function boxclick(box,MLPArray,boxname)
{ \n if (box.checked) { \n showO(MLPArray,boxname); \n }
 else { \n hideO(MLPArray,boxname);} } \n',sep="")
functions<-paste(functions,' function setOpac(MLPArray,textname)
 {opacity=0.01*parseInt(document.getElementById(textname).value) \n for
  (var i = 0; i < MLPArray.length; i++) { MLPArray[i].setOptions({strokeOpacity:
  opacity, fillOpacity: opacity}); } } \n',sep="")
functions<-paste(functions,' function setLineWeight(MLPArray,textnameW)
 {weight=parseInt(document.getElementById(textnameW).value) \n
 for (var i = 0; i < MLPArray.length; i++)
  { MLPArray[i].setOptions({strokeWeight: weight}); } } \n',sep="")
functions<-paste(functions,'function legendDisplay(box,divLegendImage){ \n
element = document.getElementById(divLegendImage).style; \n if (box.checked)
 { element.display="block";} else {  element.display="none";}} \n',sep="")
functions<-paste(functions,'function showR(R,boxname) { R.setMap(map);
 \n document.getElementById(boxname).checked = true; } \n ',sep="")
functions<-paste(functions,' function hideR(R,boxname) { R.setMap(null);
 \n document.getElementById(boxname).checked = false; } \n ',sep="")
functions<-paste(functions,'function boxclickR(box,R,boxname) { \n if (box.checked)
{ \n showR(R,boxname);\n } else { \n hideR(R,boxname);} }  \n',sep="")
functions<-paste(functions,'function legendDisplay(box,divLegendImage){
\n element = document.getElementById(divLegendImage).style; \n if (box.checked)
 { element.display="block";} else {  element.display="none";}} \n',sep="")
 
init<-createInitialization(SP.ll,
                               add=TRUE,
                               zoom=zoom,
                               fitBounds=fitBounds,
                               mapTypeId = mapTypeId,
                               disableDefaultUI=disableDefaultUI,
                               disableDoubleClickZoom =disableDoubleClickZoom,
                               draggable= draggable ,
                               keyboardShortcuts=keyboardShortcuts,
                               mapTypeControlOptions=mapTypeControlOptions,
                               scaleControlOptions=scaleControlOptions,
                               navigationControl=navigationControl,
                               navigationControlOptions=navigationControlOptions,
                               noClear=noClear,
                               scrollwheel =scrollwheel    ,
                               streetViewControl= streetViewControl)
# Put all functions together
functions<-paste( functions,init, sep="")  }else{ functions<- previousMap$functions}


starthtm=paste('<html> \n <head> \n <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
\n <style type="text/css">  \n html { height: 100% } \n body { height: 100%; margin: 0px; padding: 0px }
 \n #map_canvas { float: left;
  width:', map.width,';
  height:' , map.height,'; }
 \n #cBoxes {float: left;
  width:', control.width,';
  height: ', control.height,';
 overflow:auto;
 background-color:#b0c4de } \n </style> \n
 <script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"> </script>  \n
 <script language="javascript"> ')

################################################################################

if (class(SP)[1]=="SpatialPoints"){

            pointsName<-paste('markers',nameOfSP,sep="")
            # Create chechk box name for checkbox control
            boxname<-paste(nameOfSP,'box',sep="")
            
            
            if (!is.list(previousMap)) {
            var<-""
            # Declare variables in JavaScript marker and map
            var<-c(' var marker \n var map \n')
            # Create all markers and store them in markersArray - PointsName
            }else{ var<-previousMap$var}
            var<-paste(var,'var ',pointsName,'=[] ;')
            var1=""
            for(i in 1:length(SP.ll@coords[,1])){
            var1<-paste(var1,createMarker(SP.ll@coords[i,],
                                         title=paste('"',nameOfSP,
                                                ' NO: ',as.character(i),'"',sep=""),
                                               clickable=clickable,
                                               draggable=draggableMarker,
                                               flat=flat,
                                               visible=visible,
                                               zIndex=zIndex),'\n',sep="")   
            var1<-paste(var1,pointsName,'.push(marker); \n',sep="")  }
            
            # Put all variables together
            var<-paste(var,var1)
            
            
            functions<-paste(functions,'showO(',pointsName,',"',boxname,'");',sep="")
            
            if (!is.list(previousMap)) {
            endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
            } else { endhtm<- previousMap$endhtm }
            
            endhtm<- paste(endhtm,'<input type="checkbox" id="',boxname,
            '" onClick=\'boxclick(this,',pointsName,',"',boxname,'");\' /> <b>', layerName ,'<b> <hr />',sep="")
                                                          }
else if   (class(SP)[1]=="SpatialPointsDataFrame") {
              pointsName<-paste('markers',nameOfSP,sep="")
              # Create chechk box name for checkbox control
              boxname<-paste(nameOfSP,'box',sep="")
              att<-rep(NA,.5*length(slot(SP.ll,"coords")))
              att1=""
              
              
              if (!is.list(previousMap)) {
              var<-""
              # Declare variables in JavaScript marker and map
              var<-c(' var marker \n var map \n')
              # Create all markers and store them in markersArray - PointsName
              }else{ var<-previousMap$var}
              var<-paste(var,'var ',pointsName,'=[] ;')
              var1=""
              for(i in 1:length(SP.ll@coords[,1])){
              var1<-paste(var1,createMarker(SP.ll@coords[i,],title=paste('"',nameOfSP,' NO: ',as.character(i),'"',sep=""),
                                                 clickable=clickable,
                                                 draggable=draggableMarker,
                                                 flat=flat,
                                                 visible=visible,
                                                 zIndex=zIndex
                                                  ),'\n',sep="")
              var1<-paste(var1,pointsName,'.push(marker); \n',sep="")
              
                                for(k in 1:length(names(SP.ll@data))){
                                attrib=paste(names(SP.ll@data)[k],':',SP.ll@data[i,k],'<br>')
                                att1=paste(att1,attrib)
                                 }
                                att[i]=att1
                                att1=""
              }
              
              var<-paste(var,var1)
              infW<-""
              for(i in 1:length(SP.ll@coords[,1])){
              infW<-paste(infW,createInfoWindowEventM(Marker=paste(pointsName,'[',i-1,'] ',sep=""),content=att[i]))}
              
              functions<-paste(functions,infW,'showO(',pointsName,',"',boxname,'");',sep="")
              
              if (!is.list(previousMap)) {
              endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
              } else { endhtm<- previousMap$endhtm }
              
              endhtm<- paste(endhtm,'<input type="checkbox" id="',boxname,'" onClick=\'boxclick(this,',pointsName,',"',boxname,'");\' /> <b>', layerName ,'<b> <hr />',sep="")
                                                             }
else if   (class(SP)[1]=="SpatialLines"){
                lineName<-paste('line',nameOfSP,sep="")
                boxname<-paste(nameOfSP,'box',sep="")
                textname<- paste(nameOfSP,'text',sep="")
                textnameW<-paste(textname,'W',sep="")
                attribute=seq(1,length(SP.ll@lines))
                
                
                
                if (!is.list(previousMap)) {
                var<-""
                # Declare variables in JavaScript marker and map
                var<-c(' \n var map \n')
                # Create all markers and store them in markersArray - PointsName
                }else{ var<-previousMap$var}
                
                var<-paste(var,'var ',lineName,'=[] ; \n')
                var1=""
                xx<-PolyCol(attribute,colPalette)
                for(i in 1:length(SP.ll@lines)){
                lls<-(slot(SP.ll,"lines"))[[i]]
                lonlat<-slot(slot(lls,"Lines")[[1]],"coords")
                var1<-paste(var1,createLine(lonlat,
                                            strokeColor=xx[i],
                                            strokeOpacity=strokeOpacity,
                                            strokeWeight=strokeWeight,
                                            geodesic=geodesic,
                                            clickable=clickable,
                                            zIndex=zIndex),'\n',sep="")
                var1<-paste(var1,lineName,'.push(line); \n',sep="")  }
                
                # Put all variables together
                var<-paste(var,var1)
                
                
                
                functions<-paste(functions,'showO(',lineName,',"',boxname,'");',sep="")
                
                if (!is.list(previousMap)) {
                endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
                } else { endhtm<- previousMap$endhtm }
                
                endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> <input type="checkbox" id="',boxname,'" onClick=\'boxclick(this,',lineName,',"',boxname,'");\' /> ', layerName ,' </td> </tr> \n',sep="")
                endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',textname,'" value="100" onChange=\'setOpacL(',lineName,',"',textname,'")\' size=3 /> Opacity (0-100 %) </td> </tr> \n',sep="")
                endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',textnameW,'" value="1" onChange=\'setLineWeight(',lineName,',"',textnameW,'")\' size=3 /> Line weight (pixels) </td> </tr> </table> \n',sep="")
                
                }
else if   (class(SP)[1]=="SpatialLinesDataFrame")     {
            lineName<-paste('line',nameOfSP,sep="")
            boxname<-paste(nameOfSP,'box',sep="")
            textname<- paste(nameOfSP,'text',sep="")
            textnameW<-paste(textname,'W',sep="")
            att<-rep(NA,length(slot(SP.ll,"lines")))
            divLegendImage<- paste(nameOfSP,'_Legend',sep="")
            legendboxname<-paste('box',divLegendImage,sep="")
            for(i in 1:length(SP.ll@data)) {
            if( identical(attribute,SP.ll@data[,i])){
             attributeName<-names(SP.ll@data)[i]  }
            }
            att1=""
            
            
            if (!is.list(previousMap)) {
            var<-""
            # Declare variables in JavaScript marker and map
            var<-c(' \n var map \n')
            # Create all markers and store them in markersArray - PointsName
            }else{ var<-previousMap$var}
            
            var<-paste(var,'var ',lineName,'=[] ; \n')
            var1=""
            xx<-PolyCol(attribute,colPalette)
            pp<-lineLegend(attribute,colPalette=xx,legendName=divLegendImage)
            for(i in 1:length(SP.ll@lines)){
            lls<-(slot(SP.ll,"lines"))[[i]]
            lonlat<-slot(slot(lls,"Lines")[[1]],"coords")
            var1<-paste(var1,createLine(lonlat,
                                        strokeColor=xx[i]  ,
                                        strokeOpacity=strokeOpacity,
                                        strokeWeight=strokeWeight,
                                        geodesic=geodesic,
                                        clickable=clickable,
                                        zIndex=zIndex ),'\n',sep="")
            var1<-paste(var1,lineName,'.push(line); \n',sep="")
                               for(k in 1:length(names(SP.ll@data))){
                               attrib=paste(names(SP.ll@data)[k],':',SP.ll@data[i,k],'<br>')
                               att1=paste(att1,attrib)
                                }
                              att[i]=att1
                              att1=""
            }
            
            # Put all variables together
            var<-paste(var,var1)
            
            infW<-""
            
            for(i in 1:length(SP.ll@lines)){
            infW<-paste(infW,createInfoWindowEvent(Line_or_Polygon=paste(lineName,
                        '[',i-1,'] ',sep=""),content=att[i]),' \n')}
            
            functions<-paste(functions,infW,'showO(',lineName,',"',boxname,'");',sep="")
            
            
            if (!is.list(previousMap)) {
            endhtm<-c('</script> \n </head> \n <body onload="initialize()"> 
                       \n <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
            } else { endhtm<- previousMap$endhtm }
            
            endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> 
                           <input type="checkbox" id="',boxname,'" 
                           onClick=\'boxclick(this,',lineName,',"',
                           boxname,'");\' /> <b>', layerName ,
                           '<b> </td> </tr> \n',sep="")
            endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',
                           textname,'" value="100" onChange=\'setOpacL(',
                           lineName,',"',textname,'")\' size=3 /> 
                           Opacity (0-100 %) </td> </tr> \n',sep="")
            endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',
                           textnameW,'" value="1" onChange=\'setLineWeight(',
                           lineName,',"',textnameW,'")\' size=3 /> 
                           Line weight (pixels) </td> </tr> </table> \n',sep="")
            endhtm<- paste(endhtm,' \n <tr> \n  <td> <input type="checkbox" id="'
                           ,legendboxname,'" onClick=\'legendDisplay(this,"',
                           divLegendImage,'");\' /> LEGEND ', layerName ,' ',
                           attributeName,'<div style="display:none;" id="',
                           divLegendImage,'"> <img src="',divLegendImage,
                           '.png" alt="Legend"></div> 
                           </td> </tr> \n </table> \n <hr> \n',sep="")
            
            
                                                                       }
else if   (class(SP)[1]=="SpatialPolygons")             {
              polyName<-paste('poly',nameOfSP,sep="")
              boxname<-paste(nameOfSP,'box',sep="")
              textname<- paste(nameOfSP,'text',sep="")
              textnameW<-paste(textname,'W',sep="")
              attribute=seq(1,length(SP.ll@polygons))
              
              
              if (!is.list(previousMap)) {
              var<-""
              # Declare variables in JavaScript marker and map
              var<-c(' \n var map \n')
              # Create all markers and store them in markersArray - PointsName
              }else{ var<-previousMap$var}
              
              var<-paste(var,'var ',polyName,'=[] ; \n')
              var1=""
              xx<-PolyCol(attribute,colPalette)
              for(i in 1:length(SP.ll@polygons)){
              var1<-paste(var1,createPolygon(SP.ll@polygons[[i]],
                                       fillColor=xx[i],
                                       strokeColor=strokeColor,
                                       strokeOpacity=strokeOpacity,
                                       strokeWeight=strokeWeight,
                                       geodesic=geodesic,
                                       clickable=clickable,
                                       zIndex=zIndex),'\n',sep="")
              var1<-paste(var1,polyName,'.push(polygon); \n',sep="")  }
              
              # Put all variables together
              var<-paste(var,var1)
              
              
              functions<-paste(functions,'\n showO(',polyName,',"',boxname,'"); \n',sep="")
              
              
              if (!is.list(previousMap)) {
              endhtm<-c('</script> \n </head> \n <body onload="initialize()"> 
                        \n <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
              } else { endhtm<- previousMap$endhtm }
              
              endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> <input 
                            type="checkbox" id="',boxname,'" onClick=\'boxclick(this,',
                            polyName,',"',boxname,'");\' /> ', 
                            layerName ,' </td> </tr> \n',sep="")
              endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',
                             textname,'" value="50" onChange=\'setOpac(',
                             polyName,',"',textname,'")\' size=3 /> 
                             Opacity (0-100 %) </td> </tr> \n',sep="")
              endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',
                             textnameW,'" value="1" onChange=\'setLineWeight(',
                              polyName,',"',textnameW,'")\' size=3 /> Line weight
                               (pixels) </td> </tr> \n </table> \n',sep="")
              
               }
               
else if   (class(SP)[1]=="SpatialPolygonsDataFrame")      {

                  polyName<-paste('poly',nameOfSP,sep="")
                  boxname<-paste(nameOfSP,'box',sep="")
                  textname<- paste(nameOfSP,'text',sep="")
                  divLegendImage<- paste(nameOfSP,'_Legend',sep="")
                  legendboxname<-paste('box',divLegendImage,sep="")
                  textnameW<-paste(textname,'W',sep="")
                  
                  
                  
                  for(i in 1:length(SP.ll@data)) {
                  if( identical(attribute,SP.ll@data[,i])){
                   attributeName<-names(SP.ll@data)[i]  }
                  }
                  
                  att<-rep(NA,length(slot(SP.ll,"polygons")))
                  att1=""
                  
                  if (!is.list(previousMap)) {
                  var<-""
                  # Declare variables in JavaScript marker and map
                  var<-c(' \n var map \n')
                  # Create all markers and store them in markersArray - PointsName
                  }else{ var<-previousMap$var}
                  
                  var<-paste(var,'var ',polyName,'=[] ; \n')
                  var1=""
                  xx<-PolyCol(attribute,colPalette)
                  pp<-polyLegend(attribute,colPalette=colPalette,legendName=divLegendImage)
                  
                  for(i in 1:length(SP.ll@polygons)){
                  var1<-paste(var1,createPolygon(SP.ll@polygons[[i]],
                                           fillColor=xx[i],
                                           strokeColor=strokeColor,
                                           strokeOpacity=strokeOpacity,
                                           strokeWeight=strokeWeight,
                                           geodesic=geodesic,
                                           clickable=clickable,
                                           zIndex=zIndex),'\n',sep="")
                  var1<-paste(var1,polyName,'.push(polygon); \n',sep="")
                                     for(k in 1:length(names(SP.ll@data))){
                                     attrib=paste(names(SP.ll@data)[k],':',SP.ll@data[i,k],'<br>')
                                     att1=paste(att1,attrib)
                                      }
                                    att[i]=att1
                                    att1=""
                  }
                  
                  # Put all variables together
                  var<-paste(var,var1)
                  
                  infW<-""
                  
                  for(i in 1:length(SP.ll@polygons)){
                  infW<-paste(infW,createInfoWindowEvent(Line_or_Polygon=
                      paste(polyName,'[',i-1,'] ',sep=""),content=att[i]),' \n')}
                  
                  functions<-paste(functions,infW,'showO(',polyName,',"',boxname,'");',sep="")
                  
                  
                  
                  if (!is.list(previousMap)) {
                  endhtm<-c('</script> \n </head> \n <body onload="initialize()">
                        \n <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
                  } else { endhtm<- previousMap$endhtm }
                  
                  endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> 
                                 <input type="checkbox" id="',boxname,'" 
                                 onClick=\'boxclick(this,',polyName,',"',boxname,
                                 '");\' /> <b> ', layerName,'<b> </td> </tr> \n',sep="")
                  endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',
                                 textname,'" value="50" onChange=\'setOpac(',
                                 polyName,',"',textname,'")\' size=3 /> 
                                 Opacity (0-100 %) </td> </tr> \n',sep="")
                  endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" 
                                 id="',textnameW,'" value="1" onChange=\'
                                 setLineWeight(',polyName,',"',textnameW,'")\' 
                                 size=3 /> Line weight (pixels) </td> </tr> \n ',sep="")
                  endhtm<- paste(endhtm,' \n <tr> \n  <td> <input type="checkbox" id="',
                                 legendboxname,'" onClick=\'legendDisplay(this,"',
                                 divLegendImage,'");\' /> LEGEND ', layerName ,' ',
                                 attributeName,'<div style="display:none;" id="',
                                 divLegendImage,'"> <img src="',divLegendImage,
                                 '.png" alt="Legend"></div> </td> </tr>
                                  \n </table> \n <hr> \n',sep="")
                                     }
else if (class(SP)[1]=="SpatialPixelsDataFrame" || class(SP)[1]=="SpatialGridDataFrame") {
                rasterName<-paste('raster',nameOfSP,sep="")
                boxname<-paste(nameOfSP,'box',sep="")
                divLegendImage<- paste(nameOfSP,'_Legend',sep="")
                legendboxname<-paste('box',divLegendImage,sep="")
                
                for(i in 1:length(SP.ll@data)) {
                if( identical(attribute,SP.ll@data[,i])){
                 attributeName<-names(SP.ll@data)[i]  }
                }
                
                if (!is.list(previousMap)) {
                var<-""
                # Declare variables in JavaScript marker and map
                var<-c(' \n var map \n')
                # Create all markers and store them in markersArray - PointsName
                }else{ var<-previousMap$var}
                var<-paste(var,'\n var ',rasterName,'imageBounds = new google.maps.LatLngBounds
                (new google.maps.LatLng(',sw[1],',',sw[2],'),
                new google.maps.LatLng(',ne[1],',',ne[2],')); \n',sep="")
                var<-paste(var,'var ', rasterName ,'= new google.maps.GroundOverlay("',rasterName,
                '.png",',rasterName,'imageBounds); \n',sep="")
                
                
                xx<-as.character(substr(colPalette,1,7))
                         if(is.factor(attribute)){
                              pp<-rasterLegend(attribute,
                              colPalette=xx,legendName=divLegendImage)
                              SP$arg1111<-as.numeric(SP[attributeName]@data[,1])
                              }else{
                              pp<-rasterLegend(attribute=levels(cut(attribute,length(colPalette))),
                              colPalette=xx,legendName=divLegendImage)
                              SP$arg1111<-SP[zcol]@data[,1]}
                SGqk <- GE_SpatialGrid(SP.ll)
                png(file=paste(rasterName,'.png',sep=""), width=2*SGqk$width, height=2*SGqk$height, bg="transparent")
                par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
                image(as.image.SpatialGridDataFrame(SP["arg1111"]), col=colPalette)
                
                dev.off()
                
                
                 functions<-paste(functions,'showR(',rasterName,',"',boxname,'");',sep="")
                
                
                if (!is.list(previousMap)) {
                endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
                } else { endhtm<- previousMap$endhtm }
                
                endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> <input type="checkbox" id="',
                                  boxname,'" onClick=\'boxclickR(this,',rasterName,',"',boxname,
                                  '");\' /> <b> ', layerName ,'<b> </td> </tr> \n',sep="")
                endhtm<- paste(endhtm,' \n <tr> \n  <td> <input type="checkbox" id="',
                      legendboxname,'" onClick=\'legendDisplay(this,"',divLegendImage,'");\' /> LEGEND ',
                      layerName  ,' ',attributeName,'<div style="display:none;" id="',divLegendImage,
                       '"> <img src="',divLegendImage,'.png" alt="Legend"></div> </td> </tr> \n </table> \n <hr> \n',sep="")
                                                                              }
else  {
    message("SP object must be Spatial class!") }
    


if (add==FALSE){functions<- paste(functions,'}')
endhtm<-paste(endhtm,'</div> \n </body>  \n  </html>')
write(starthtm, filename,append=FALSE)
write(var, filename,append=TRUE)
write(functions, filename,append=TRUE)
write(endhtm, filename,append=TRUE)}


x <- list(starthtm=starthtm,var=var, functions=functions,endhtm=endhtm)
return(x)


              }


segmentGoogleMaps <-
function(SP,
                     zcol=1:length(SP@data),
                     filename="",
                     max.radius=100,
                     scalelist=TRUE,
                     do.sqrt = FALSE,
                     add=F,
                     previousMap=NULL,
                     colPalette=rainbow(ncol(SP@data[,zcol])),
                     strokeColor="#FFAA00",
                     strokeOpacity=1,
                     strokeWeight=1,
                     fillOpacity=0.7,
                     geodesic=TRUE,
                     clickable=TRUE,
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
                               streetViewControl= FALSE) {


################################################################################
################################################################################
################################################################################

disableDefaultUI=FALSE

nameOfSP<-sapply(as.list(substitute({SP})[-1]), deparse)
nameOfSP<-gsub("\\s","", nameOfSP)
nameOfSP<-gsub('[!,",#,$,%,&,(,),*,+,-,.,/,:,;,<,=,>,?,@,^,`,|,~]', "x", nameOfSP)
nameOfSP<-gsub('[[]', "X", nameOfSP)
nameOfSP<-gsub('[]]', "X", nameOfSP)

SP<-SP[,zcol]
SP <-as(SP, "SpatialPointsDataFrame")
SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))

Centar=c(mean(SP.ll@bbox[1,]),mean(SP.ll@bbox[2,]))
sw<-c(SP.ll@bbox[2,1],SP.ll@bbox[1,1])
ne<-c(SP.ll@bbox[2,2],SP.ll@bbox[1,2])


###################################################


if(filename==""){
filename <- paste(nameOfSP,'.htm',sep="")}
attribute=SP@data[,zcol]
polyName<-paste('poly',nameOfSP,sep="")
boxname<-paste(nameOfSP,'box',sep="")
textname<- paste(nameOfSP,'text',sep="")
divLegendImage<-tempfile("Legend")  
divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
legendboxname<-paste('box',divLegendImage,sep="")
textnameW<-paste(textname,'W',sep="")

if(strokeColor!=""){
rgbc<-col2rgb(strokeColor)
strokeColor<-rgb(rgbc[1],rgbc[2],rgbc[3],maxColorValue=255) }

if(!is.null(colPalette)){
rgbc<-col2rgb(colPalette)
colPalette<-apply(rgbc,2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))}

if(layerName==""){
layerName=nameOfSP}

    if(scalelist){
    xdata<-SP@data[,zcol]
    xdata <- apply(xdata, 2L, function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE)))
    xsum <- apply(xdata, 1L,function(x) ( sum(x)))
    scalelist<-xsum/max(xsum)
    scalelist<-sqrt(scalelist)} else{ scalelist<-rep(1,length(SP.ll@coords[,1])) }



attributeName<-paste(names(SP.ll@data[,zcol]),collapse ="",sep=" ")

if(layerName==""){
layerName=paste(nameOfSP,attributeName)}


att<-rep(NA,length(SP.ll@coords[,1]))
att1=""

if (!is.list(previousMap)) {
var<-""
# Declare variables in JavaScript marker and map
var<-c(' \n var map \n')
# Create all markers and store them in markersArray - PointsName
}else{ var<-previousMap$var}

var<-paste(var,'var ',polyName,'=[] ; \n')
var1=""

pp<-segmentLegend(attribute=names(SP.ll@data[,zcol]),
                        colPalette=colPalette,
                        legendName=divLegendImage,
                        border=ifelse(strokeColor=="",NA,strokeColor),
                        bgc='#B0C4DEFF')

for(i in 1:length(SP.ll@coords[,1])){
var1<-paste(var1, createSphereSegment(SP.ll[i,zcol],
                              max.radius=max.radius,  #m
                             name="polygon",
                             key.entries = as.numeric(SP.ll@data[i,zcol]),
                             scalelist= scalelist[i],
                             do.sqrt = do.sqrt,
                             fillColor=colPalette,
                             fillOpacity=fillOpacity,
                             map="map",
                             strokeColor=strokeColor,
                             strokeOpacity= strokeOpacity,
                             strokeWeight=strokeWeight,
                             geodesic=geodesic,
                             clickable=clickable,
                             zIndex=zIndex),'\n',sep="")
                             
                for(jj in 1:length(zcol)){         
      var1<-paste(var1,polyName,'.push(polygon[',jj-1 ,']); \n',sep="")  }
      
                         for(k in 1:length(names(SP.ll@data))){
                         attrib=paste(names(SP.ll@data)[k],':',SP.ll@data[i,k],'<br>')
                         att1=paste(att1,attrib)
                          }
          att[i]=att1
          att1=""
                                       }
# Put all variables together
var<-paste(var,var1)
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
                               add=T,
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
infW<-""

for(i in 1:length(SP.ll@coords[,1])){
   for(k in  ncol(SP.ll@data):1){
infW<-paste(infW,createInfoWindowEvent(Line_or_Polygon=
paste(polyName,'[',3*i-k,'] ',sep=""),content=att[i]),' \n')}}

functions<-paste(functions,infW,'showO(',polyName,',"',boxname,'");',sep="")


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

if (!is.list(previousMap)) {
endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n <div id="map_canvas"></div>  \n <div id="cBoxes"> \n')
} else { endhtm<- previousMap$endhtm }

endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> <input type="checkbox" id="',
               boxname,'" onClick=\'boxclick(this,',polyName,',"',boxname,'");\' /> <b> ',
               layerName ,'<b> </td> </tr> \n',sep="")
endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',
               textname,'" value="50" onChange=\'setOpac(',polyName,',"',
               textname,'")\' size=3 /> Opacity (0-100 %) </td> </tr> \n',sep="")
endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',textnameW,
               '" value="1" onChange=\'setLineWeight(',polyName,',"',textnameW,
               '")\' size=3 /> Line weight (pixels) </td> </tr> \n ',sep="")
endhtm<- paste(endhtm,' \n <tr> \n  <td> <input type="checkbox" checked="checked" id="',legendboxname, 
               '" onClick=\'legendDisplay(this,"',divLegendImage,'");\' /> LEGEND ',  
               layerName ,'<div style="display:block;" id="',
               divLegendImage,'"> <img src="',divLegendImage,
               '.png" alt="Legend"></div> </td> </tr> \n </table> \n <hr> \n',sep="")

if (add==F){functions<- paste(functions,'}')
endhtm<-paste(endhtm,'</div> \n </body>  \n  </html>')
write(starthtm, filename,append=F)
write(var, filename,append=TRUE)
write(functions, filename,append=TRUE)
write(endhtm, filename,append=TRUE)
browseURL(filename)}


x <- list(starthtm=starthtm,var=var, functions=functions,endhtm=endhtm)
return(x)
}

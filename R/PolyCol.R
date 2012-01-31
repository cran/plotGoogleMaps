PolyCol <-
function(attribute,colPalette=NULL) {
      # attribute=soil.ll@data$ID
          pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
          
if(!is.numeric(attribute)){ attribute<-as.factor( attribute)}
          
if(length(colPalette)==1) {
              x<- rep(colPalette,length(attribute))
              col.data<-list(cols=as.character(substr(x,1,7)),col.uniq=colPalette, att=ifelse(!is.factor(attribute),paste("[",min(attribute)," , ",max(attribute),"]",sep=""), " "))
              return(col.data) }
          
if(is.null(colPalette) ){
      colPalette<-pal(min(10,length(attribute) ) ) }else{ xx<-colPalette<-as.character(substr(colPalette,1,7)) }
                
if(is.factor(attribute)){
        
                        if(length(colPalette)!=nlevels(attribute)) {
                               xx<-colPalette<- as.character(substr(pal(nlevels(attribute)),1,7))    }
                               
    x<-factor(attribute,labels=colPalette)
    col.data<-list(cols=as.character(substr(x,1,7)),col.uniq=colPalette, att=levels(attribute) )
    return(col.data)
                              
                        }else{
                               bre<-quantile(attribute, seq(1,length(colPalette))/length(colPalette))
                               breakss<-factor(c(min(attribute),bre))
                               break_unique<-as.numeric(levels(breakss))
                                     
                                     if(length(colPalette)>=length(break_unique)){
                                         colPalette<-colPalette[1:length(break_unique)] } else{
                                                                     colPalette<- as.character(substr(colPalette[1:length(break_unique)-1],1,7))}
                                                                      
                                    atr<-cut(attribute, break_unique ,include.lowest = TRUE, dig.lab=6)                                 
                                    x<-factor(atr,labels=colPalette[1:(length(break_unique)-1)] )
                                    col.data<-list(cols=as.character(substr(x,1,7)),col.uniq=colPalette, att=levels(atr) )
                                    return(col.data)                               
                               
                               }
                                         
                                         
                                         }

PolyCol <-
function(attribute,colPalette=NULL) {
      # attribute=soil.ll@data$ID
          pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
        if(is.null(colPalette)){
colPalette<-pal(min(5,length(attribute) ) )}else { xx<-colPalette<-as.character(substr(colPalette,1,7)) }
                
        if(is.factor(attribute)){
        
                        if(length(colPalette)!=nlevels(attribute)) {
                               xx<-colPalette<- as.character(substr(pal(nlevels(attribute)),1,7))    }
                              
                              }else{
                               bre<-quantile(attribute, seq(1,length(colPalette))/length(colPalette))
                                breakss<-factor(c(min(attribute),bre))
                                break_unique<-as.numeric(levels(breakss))
                                     if(length(colPalette)>=length(break_unique)){
                                         colPalette<-colPalette[1:length(break_unique)] } else{
                                                                     colPalette<- as.character(substr(colPalette[1:length(break_unique)-1],1,7))} 
                               
                                                                      
                                                                      }
                
                
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
     x<-factor(cut(attribute, break_unique ,include.lowest = TRUE, dig.lab=6),labels=colPalette[1:length(break_unique)-1] )
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
       pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
        x<-factor(factor(attribute),labels=pal(nlevels(factor(attribute))))
         return(as.character(substr(x,1,7)))
       }
}

}

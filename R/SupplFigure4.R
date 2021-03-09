#' @title SupplFigure4
#' @description Plot supplementary figure 4
#' @param df_human Human immunopeptidomicvs dataframe from GetHumanMHCIdata
#' @return supplementary figure 4
#' @seealso 
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_smooth}},\code{\link[ggplot2]{facet_wrap}}
#' @rdname SupplFigure4
#' @export 
#' @importFrom ggplot2 ggplot aes geom_smooth facet_wrap
SupplFigure4<- function(df_human){
  dfh<-df_human
  dfh$Donor_sequence <- apply(cbind(dfh['Donor'],dfh['sequence']),1,function(x){ paste0(as.character(x[1]),'_',x[2] )} )
  dfhw_area<-reshape(cbind(dfh[c(21,6,8)]),direction="wide",timevar="Tissue",idvar=c("Donor_sequence"))
  dfhw_area$RankN<-rowSums(!is.na(dfhw_area[,2:ncol(dfhw_area)]))
  dfhw_area$Av.Area<- rowMeans(dfhw_area[,2:(ncol(dfhw_area)-1) ],na.rm = T )
  dfhw_area$Donor<- data.frame(unlist((do.call(rbind,strsplit(dfhw_area$Donor_sequence,'_')))))[,1]
  dfhw_area<- subset(dfhw_area,RankN>1)
  suppressWarnings( ggplot2::ggplot(dfhw_area,ggplot2::aes(x=RankN,y=Av.Area ))+ggplot2::geom_smooth(method = loess)+ggplot2::facet_wrap(~Donor,scales='free')    )      
  }



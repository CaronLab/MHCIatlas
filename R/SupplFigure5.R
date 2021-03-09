#' @title SupplFigure5
#' @description Plot supplementary figure 5
#' @param df_human Human immunopeptidomicvs dataframe from GetHumanMHCIdata
#' @return supplementary figure 5
#' @seealso 
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_smooth}},\code{\link[ggplot2]{facet_wrap}}
#' @rdname SupplFigure5
#' @export 
#' @importFrom ggplot2 ggplot aes geom_smooth facet_wrap
SupplFigure5<- function(df_human){
  dfh<-df_human
  dfh$Donor_sequence <- apply(cbind(dfh['Donor'],dfh['sequence']),1,function(x){ paste0(as.character(x[1]),'_',x[2] )} )
  dfhw_rank<-reshape(cbind(dfh[c(21,6,12)]),direction="wide",timevar="Tissue",idvar=c("Donor_sequence"))
  dfhw_rank$RankN<-rowSums(!is.na(dfhw_rank[,2:ncol(dfhw_rank)]))
  dfhw_rank$Av.rank<- rowMeans(dfhw_rank[,2:(ncol(dfhw_rank)-1) ],na.rm = T )
  dfhw_rank$Donor<- data.frame(unlist((do.call(rbind,strsplit(dfhw_rank$Donor_sequence,'_')))))[,1]
  dfhw_rank<- subset(dfhw_rank,RankN>1)
  suppressWarnings( ggplot2::ggplot(dfhw_rank,ggplot2::aes(x=RankN,y=Av.rank ))+ggplot2::geom_smooth(method = loess)+ggplot2::facet_wrap(~Donor,scales='free')   )      
}


#' @title SupplFigure3
#' @description Plot supplementary figure 3
#' @param df_mouse Mouse immunopeptidomicvs dataframe from GetMouseMHCIdata
#' @return supplementary figure 3 
#' @seealso 
#'  \code{\link[stats]{reshape}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_smooth}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{sec_axis}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}}
#' @rdname SupplFigure3
#' @export 
#' @importFrom stats reshape
#' @importFrom ggplot2 ggplot aes geom_smooth scale_y_continuous sec_axis theme element_text
SupplFigure3<- function(df_mouse){
  #Internal
  getIndices <- function(dataframe,string){ which(grepl(string,tolower(names(dataframe))))}
  #Main
  dfm_area<- stats::reshape( cbind(df_mouse[c(1,4,6)]),idvar="Peptide",direction="wide",timevar="Tissue" )
  dfm_rank<- stats::reshape( cbind(df_mouse[c(1,2,6)]),idvar="Peptide",direction="wide",timevar="Tissue" )
  dfm_area$RankN<- rowSums(!is.na(dfm_area[,2:ncol(dfm_area)]))
  dfm_area$MeanArea<- rowMeans(dfm_area[getIndices(dfm_area,'area\\.')],na.rm = T)
  dfm_rank$RankN<- rowSums(!is.na(dfm_rank[,2:ncol(dfm_rank)]))
  dfm_rank$MeanRank<- rowMeans(dfm_rank[getIndices(dfm_rank,'rank\\.')],na.rm = T)
  ggplot2::ggplot(dfm_area,ggplot2::aes(x=RankN,y=log10(MeanArea) ))+ggplot2::geom_smooth()
  ggplot2::ggplot(dfm_rank,ggplot2::aes(x=RankN,y=MeanRank ))+ggplot2::geom_smooth()
  
  dfm_combined<- cbind(dfm_area[c(1,21:22)],dfm_rank[c(22)])
  scale_factor<- log10(mean( dfm_area$MeanArea,na.rm = T ))/mean(dfm_rank$MeanRank,na.rm = T)
  ggplot2::ggplot(dfm_combined,ggplot2::aes(x=RankN))+ggplot2::geom_smooth(ggplot2::aes(y=log10(MeanArea) ) )+
    ggplot2::geom_smooth(ggplot2::aes(y=MeanRank*scale_factor,col='red' ))+
    ggplot2::scale_y_continuous(name = "Intensity, log10", sec.axis=ggplot2::sec_axis(~./scale_factor , name = "NetMHCpan4.0 Rank"))+
    ggplot2::theme(
      axis.title.y.left=ggplot2::element_text(color="blue"),
      axis.text.y.left=ggplot2::element_text(color="blue"),
      axis.title.y.right=ggplot2::element_text(color="red"),
      axis.text.y.right=ggplot2::element_text(color="red"),
      legend.position = "none"
    )
}

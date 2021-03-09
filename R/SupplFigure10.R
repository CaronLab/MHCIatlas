#' @title SupplFigure10
#' @description Plot supplementary figure 10
#' @param corr_human Correlation analysis human from MakeCorrHuman
#' @param corr_mouse Correlation analysis mouse from MakeCorrMouse
#' @param plots_human Plots human from PlotsHumanProtCorr
#' @param plots_mouse Plots mouse from PlotsMouseProtCorr
#' @return Supplementary Figure 10
#' @seealso 
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{scale_colour_brewer}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}}
#'  \code{\link[cowplot]{ggdraw}},\code{\link[cowplot]{draw_plot}},\code{\link[cowplot]{draw_plot_label}}
#' @rdname SupplFigure10
#' @export 
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_brewer theme element_blank
#' @importFrom cowplot ggdraw draw_plot draw_plot_label
SupplFigure10<- function(corr_human,corr_mouse,plots_human,plots_mouse){
  corr_h<- subset(corr_human[[1]],RankSig>1)
  corr_hs<- corr_h[c('Gene_names','MeanCorr_slope')]
  corr_hs$Dir<- apply(corr_hs['MeanCorr_slope'],1,function(x) {ifelse(x>0,1,-1)})
  corr_hs$Org<- 'Human'
  
  
  corr_m<-subset(corr_mouse[[1]],SigCorr==1)
  corr_ms<- corr_m[c('Gene.names','Corr.slope')]
  corr_ms$Dir<- apply(corr_ms['Corr.slope'],1,function(x) {ifelse(x>0,1,-1)})
  corr_ms$Org<- 'Mouse'
  colnames(corr_ms)<-c('Gene_names','Slope','Dir','Org')
  colnames(corr_hs)<-c('Gene_names','Slope','Dir','Org')
  corrs<- rbind(corr_hs,corr_ms)
  pA<-ggplot2::ggplot(corrs,ggplot2::aes(x=Org,fill=factor(Dir),position='fill'))+ggplot2::geom_bar(position='fill') +ggplot2::scale_fill_brewer(palette="Set1")+ ggplot2::theme(legend.title =ggplot2::element_blank()  ) 
  pB<- plots_mouse[['Uchl1']]
  pC<- plots_human[['PSMB5']]
  pD<- plots_mouse[['Ube2n']]
  
  SupplFigure10<- cowplot::ggdraw() +
    cowplot::draw_plot(pA, x = 0, y = .5, width = .5, height = .5) +
    cowplot::draw_plot(pB, x = .5, y = .5, width = .5, height = .5) +
    cowplot::draw_plot(pC, x = 0, y =0, width = .5, height =.5) +
    cowplot::draw_plot(pD, x = .5, y = 0, width = .5, height = .5) +
    cowplot::draw_plot_label(label = c("A", "B", "C","D"), size = 15,
                             x = c(0, 0.5, 0,0.5), y = c(1, 1, 0.5,0.5))
  print(SupplFigure10)
}
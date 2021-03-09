#' @title mkFigure6
#' @description makes plot for Figure 6 B,C and D (A is only the cartoon)
#' @param corr_human correlation output from MakeCorrHuman
#' @param corr_mouse correlation output from MakeCorrMouse
#' @param GO_human data frame containing GO_terms for human proteins that significantly correlate (See output corr_human), file contains column with GO_terms named 'Gene.Set.Name' and column with qvalue named 'FDR.qvalue', Default is taking GO terms as in publication, Default: NA
#' @param GO_mouse data frame containing GO_terms for mouse proteins that significantly correlate (See output corr_human), file contains column with GO_terms named 'Gene.Set.Name' and column with qvalue named 'FDR.qvalue', Default is taking GO terms as in publication, Default: NA
#' @return Figure 6 and GO term data
#' @details DETAILS
#' @examples 
#' \dontrun{
#' mkFigure6(corr_human,corr_mouse)
#' }
#' @seealso 
#'  \code{\link[reshape2]{melt}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{coord_flip}},\code{\link[ggplot2]{scale_colour_gradient}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}},\code{\link[ggplot2]{scale_x_discrete}}
#'  \code{\link[scales]{label_wrap}}
#'  \code{\link[cowplot]{ggdraw}},\code{\link[cowplot]{draw_plot}},\code{\link[cowplot]{draw_plot_label}}
#' @rdname mkFigure6
#' @export 
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip scale_color_gradient theme element_blank scale_x_discrete
#' @importFrom scales wrap_format
#' @importFrom cowplot ggdraw draw_plot draw_plot_label
mkFigure6<- function(corr_human,corr_mouse,GO_human=NA,GO_mouse=NA){
  if(is.na(GO_human)==T ){
    fileh<- read.csv(system.file("extdata", "human_GOterms.csv", package = "MHCIatlas"))
  }else{
    fileh<- GO_human 
  }
  if(is.na(GO_mouse)==T ){
    filem<- read.csv(system.file("extdata", "mouse_GOterms.csv", package = "MHCIatlas"))
  }else{
    filem<- GO_mouse 
  }
  dfm<-merge(fileh,filem,by='Gene.Set.Name',suffixes = c('_human','_mouse'))
  dfm$Gene.Set.Name<-gsub('GO_','',dfm$Gene.Set.Name)
  dfm_qval<- dfm[c("Gene.Set.Name","FDR.qvalue_human","FDR.qvalue_mouse")]  
  dfm_qvm<- reshape2::melt(dfm_qval,id=c('Gene.Set.Name'))
  colnames(dfm_qvm)<- c('GO_term','Organism','qValue') 
  p1<-ggplot2::ggplot(dfm_qvm,ggplot2::aes(x=factor(GO_term),fill=factor(Organism), y= -log10(qValue),position='dodge' ))+ggplot2::geom_bar(stat='identity',position='dodge',color='black')+ggplot2::coord_flip()+
    ggplot2::scale_color_gradient(low="navy", high="red")+ggplot2::theme(legend.position = c(0.9, 0.2),legend.title = ggplot2::element_blank(),axis.text.x=element_text(colour = 'black',size=9),axis.text.y=element_text(colour = 'black',size=7),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), panel.border = element_rect(fill=NA) )+xlab('Gene Ontology')+ylab('-log10(q-Value)')+ggplot2::scale_x_discrete(labels = scales::wrap_format(10))+scale_fill_brewer(palette="Set1")
  plots_human<- PlotsHumanProtCorr(corr_human)
  plots_mouse<- PlotsMouseProtCorr(corr_mouse)
  pm<- plots_mouse[['Psmb3']]
  ph<- plots_human[['CD84']]
  Figure6<- cowplot::ggdraw() +
    cowplot::draw_plot(p1, x = 0, y = 0, width = .65, height = 1) +
    cowplot::draw_plot(ph+ggplot2::theme(legend.position = c(0.1,0.8)), x = .65, y = .5, width = .35, height = .5) +
    cowplot::draw_plot(pm, x = 0.65, y =0, width = .35, height =.5) +
    cowplot::draw_plot_label(label = c("B", "C", "D"), size = 15,
                             x = c(0, 0.65,0.65), y = c(1, 1, 0.5))
  return(list(Figure6,fileh,filem) )
}

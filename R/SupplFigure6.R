#' @title SupplFigure6
#' @description Plot supplementary figure 6
#' @param df_mouse Mouse immunopeptidomicvs dataframe from GetMouseMHCIdata
#' @param df_human Human immunopeptidomicvs dataframe from GetHumanMHCIdata
#' @return supplementary figure 6
#' @seealso 
#'  \code{\link[dplyr]{count}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{labs}}
#'  \code{\link[ggforce]{facet_zoom}}
#'  \code{\link[cowplot]{ggdraw}},\code{\link[cowplot]{draw_plot}},\code{\link[cowplot]{draw_plot_label}}
#' @rdname SupplFigure6
#' @export 
#' @importFrom magrittr `%>%`
#' @importFrom dplyr count
#' @importFrom ggplot2 ggplot aes geom_bar xlab ylab
#' @importFrom ggforce facet_zoom
#' @importFrom cowplot ggdraw draw_plot draw_plot_label
SupplFigure6<- function(df_mouse,df_human){
  `%>%` <- magrittr::`%>%`
  
  RankN_mouse<-data.frame(df_mouse["Peptide" ] %>% dplyr::count(Peptide))
  pA<-ggplot2::ggplot(RankN_mouse,ggplot2::aes(x=factor(n)))+ggplot2::geom_bar()+ggplot2::xlab('# of measurements across tissues (Mouse)')+ggplot2::ylab('Peptide counts')
  RankN_human<-data.frame(df_human["sequence" ] %>% dplyr::count(sequence))
  pB<-ggplot2::ggplot(RankN_human,ggplot2::aes(x=n ))+ggplot2::geom_bar()+ggplot2::xlab('# of measurements across tissues (Human)')+ggplot2::ylab('Peptide counts')+ggforce::facet_zoom(ylim=c(0,30))
  house_h<- HousekeepersHuman(df_human)
  pC<- print(house_h[[1]][[1]])
  pD<- house_h[[1]][[2]]
  dfh_DN14<- subset(df_human,Donor=='AUT01-DN14')
  RankN_DN14<-data.frame(dfh_DN14["sequence" ] %>% dplyr::count(sequence))
  pE<-ggplot2::ggplot(RankN_DN14,ggplot2::aes(x=factor(n)))+ggplot2::geom_bar()+ggplot2::xlab('# of measurements across tissues (AUT01-DN14)')+ggplot2::ylab('Peptide counts')
 
  dfh_A0101<- subset(df_human,Best_Donor.Allele=='A01:01')
  RankN_A0101<-data.frame(dfh_A0101["sequence" ] %>% dplyr::count(sequence))
  pF<-ggplot2::ggplot(RankN_A0101,ggplot2::aes(x=n))+ggplot2::geom_bar()+ggplot2::xlab('# of measurements across tissues (Human A01:01)')+ggplot2::ylab('Peptide counts')
  
  SupplFigure6<- cowplot::ggdraw() +
    cowplot::draw_plot(pA, x = 0, y = 0.66, width = .5, height = 0.33) +
    cowplot::draw_plot(pB, x = .5, y = 0.66, width = .5, height = 0.33) +
    cowplot::draw_plot(pC, x = 0, y = 0.33, width = .5, height = 0.33) +
    cowplot::draw_plot(pD, x = .5, y = 0.33, width = .5, height = 0.33) +
    cowplot::draw_plot(pE, x = 0, y = 0, width = .5, height = 0.33) +
    cowplot::draw_plot(pF, x = 0.5, y = 0, width = .5, height = 0.33) +
    cowplot::draw_plot_label(label = c("A", "B","C","D","E","F"), size = 15,
                             x = c(0, 0.5,0, 0.5,0, 0.5), y = c(1, 1,0.66,0.66,0.33,0.33))
  print(SupplFigure6)
  
  }
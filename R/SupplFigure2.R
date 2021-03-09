#' @title SupplFigure2
#' @description Function to plot Supplementary Figure 2
#' @param df_human Human immonopeptidome data frame from function GetHumanMHCIdata
#' @return Supplementary Figure 2 as plot
#' @examples 
#' \dontrun{
#' SupplFigure2(df_human)
#' }
#' @seealso 
#'  \code{\link[data.table]{as.data.table}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{coord_flip}},\code{\link[ggplot2]{labs}}
#'  \code{\link[cowplot]{ggdraw}},\code{\link[cowplot]{draw_plot}},\code{\link[cowplot]{draw_plot_label}}
#' @rdname SupplFigure2
#' @export 
#' @importFrom data.table as.data.table
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip ylab xlab
#' @importFrom cowplot ggdraw draw_plot draw_plot_label
SupplFigure2<- function(df_human){
  dfh<- df_human
  dfh_s<- subset(dfh,Tissue=='Spleen'| Tissue=='BoneMarrow'| Tissue=='Kidney'|Tissue=='Lung'| Tissue=='Liver'| Tissue=='Colon')
  #Deconvoluted by Allele:
  dfh_ss<- cbind(dfh_s[1],dfh_s[6:7],dfh_s[8],dfh_s[11])
  alleles<- unique(dfh_ss[5])
  prop_tissues<- list()
  for (i in 1:nrow(alleles)) {
    Xs_tmp<- subset(dfh_ss,Best_Donor.Allele==alleles[i,])[1:4]
    Xs <- data.table::as.data.table(Xs_tmp)
    Xss<- data.frame(Xs[,list(mm= round(mean(median_int),digits=3)),c('sequence','Tissue') ])
    dfhr_ss<-reshape(Xss,direction="wide",timevar="Tissue",idvar=c("sequence"))
    if(ncol(dfhr_ss)!=7){prop_tissues[[i]]<-NA}else{
      dfhr_ss$RankN<- rowSums(!is.na(dfhr_ss[,2:ncol(dfhr_ss)]))
      maxRN<- max(dfhr_ss$RankN)
      prop_tissues[[i]]<- nrow(subset(dfhr_ss,RankN==maxRN)) / length(unique(subset(dfh,Best_Donor.Allele==alleles[i,])$sequence))
    }
  }
  df_prop_tissues<-data.frame(do.call(rbind,prop_tissues))
  df_prop_tissues$Allele<- alleles[,1]
  colnames(df_prop_tissues)<- c('Proportion','Alleles')
  p1<-ggplot2::ggplot(na.omit(df_prop_tissues),ggplot2::aes(x=factor(Alleles),y=Proportion*100))+ggplot2::geom_bar(stat='Identity')+ggplot2::coord_flip()+ggplot2::ylab('% of peptides shared')+ggplot2::xlab('Allele')

  
  #Deconvoluted by Patient (Alleles are not patients, note that variable alleles was kept beacuse of my lazyness:
  dfh_s<- subset(dfh,Tissue=='Spleen'| Tissue=='BoneMarrow'| Tissue=='Kidney'|Tissue=='Lung'| Tissue=='Liver'| Tissue=='Colon')
  #Deconvoluted by Allele:
  dfh_ss<- cbind(dfh_s[1],dfh_s[6:7],dfh_s[8],dfh_s[11])
  alleles<- unique(dfh_ss[3])
  prop_tissues<- list()
  patients<- list()
  for (i in 1:nrow(alleles)) {
    Xs_tmp<- subset(dfh_ss,Donor==alleles[i,])[1:4]
    Xs <- data.table::as.data.table(Xs_tmp)
    Xss<- data.frame(Xs[,list(mm= round(mean(median_int),digits=3)),c('sequence','Tissue') ])
    dfhr_ss<-reshape(Xss,direction="wide",timevar="Tissue",idvar=c("sequence"))
    patients[[i]]<- as.character(alleles[i,])
    if(ncol(dfhr_ss)!=7){prop_tissues[[i]]<-NA}else{
      dfhr_ss$RankN<- rowSums(!is.na(dfhr_ss[,2:ncol(dfhr_ss)]))
      maxRN<- max(dfhr_ss$RankN)
      prop_tissues[[i]]<- nrow(subset(dfhr_ss,RankN==maxRN)) / length(unique(subset(dfh,Donor==alleles[i,])$sequence))
    }
  }
  df_prop_tissues<-data.frame(do.call(rbind,prop_tissues))
  df_prop_tissues$Patient<- do.call(rbind,patients)[,1]
  colnames(df_prop_tissues)<- c('Proportion','Patient')
  p2<-ggplot2::ggplot(na.omit(df_prop_tissues),ggplot2::aes(x=factor(Patient),y=Proportion*100))+ggplot2::geom_bar(stat='Identity')+ggplot2::coord_flip()+ggplot2::ylab('% of peptides shared')+ggplot2::xlab("Patient")
  SupplFigure2<- cowplot::ggdraw() +
    cowplot::draw_plot(p1, x = 0, y = 0, width = .5, height = 1) +
    cowplot::draw_plot(p2, x = .5, y = 0, width = .5, height = 1) +
    cowplot::draw_plot_label(label = c("A", "B"), size = 15,
                             x = c(0, 0.5), y = c(1, 1))
  print(SupplFigure2)
  
}

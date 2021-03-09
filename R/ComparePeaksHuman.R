#' @title ComparePeaksHuman
#' @description Function to compare human MHCI atlas to simple Peaks output (peptide.csv) to assess extent of overlapping peptides.
#' @param peaks_peptide_output The data frame containing the peaks output (peptide.csv)
#' @param df_human Data frame containing the human MHCI atlas data
#' @return venn diagramm plot and dataframe of input annotated with presence/absence of these peptides in the human MHCI atlas
#' @examples 
#' \dontrun{
#'  ComparePeaksHuman(peaks_peptide_output,df_human)
#' }
#' @seealso 
#'  \code{\link[limma]{venn}}
#' @rdname ComparePeaksHuman
#' @export 
#' @importFrom limma vennCounts vennDiagram
ComparePeaksHuman<- function(peaks_peptide_output,df_human){
  getIndices <- function(dataframe,string){ which(grepl(string,tolower(names(dataframe))))}
  
  df<-peaks_peptide_output[c(getIndices(peaks_peptide_output,'peptide'),getIndices(peaks_peptide_output,'area'))]
  colnames(df)<- c('sequence',names(df)[2:ncol(df)])
  dfh<- df_human[c('sequence','median_int')]
  dfh<- dfh[order(dfh$sequence,dfh$median_int,decreasing = T),]
  dfh<- dfh[!duplicated(dfh$sequence),]
  dfm<- merge(dfh,df,by='sequence',all=T)
  m_dat<- as.matrix(dfm[2:ncol(dfm)])
  m_dat[m_dat>=0]<- 1
  m_dat[is.na(m_dat)]<- 0
  df_venn<- data.frame(dfm['sequence'],m_dat)
  colnames(df_venn)<- c(names(df_venn[1]),'MHCI human atlas',gsub('Area.','',names(df_venn)[3:ncol(df_venn)] ))
  Venn_obj<-limma::vennCounts(df_venn[2:ncol(df_venn)])
  limma::vennDiagram(Venn_obj)
  df_out<- merge(df_venn,peaks_peptide_output,by.x='sequence',by.y='Peptide',all=F)
  return(df_out)
}

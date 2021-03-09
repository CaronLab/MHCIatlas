## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  ?MHCIatlas::NameOfFunction

## ----eval=FALSE---------------------------------------------------------------
#  ?MHCIatlas::MakeCorrMouse

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github('CaronLab/MHCIatlas')

## ----eval=FALSE---------------------------------------------------------------
#  requiredpackages<-c('BiocManager', 'FactoMineR', 'factoextra',
#  'stringr', 'ggplot2', 'cowplot', 'tidyr', 'stats','broom', 'ggrepel',
#  'ggpmisc','dplyr', 'magrittr', 'data.table', 'Matrix', 'pheatmap',
#  'reshape2', 'ggplotify', 'RColorBrewer', 'forcats',
#  'rtracklayer', 'TxDb.Mmusculus.UCSC.mm10.knownGene',
#  'TxDb.Hsapiens.UCSC.hg38.knownGene', 'AnnotationDbi',
#  'org.Mm.eg.db','GenomicFeatures', 'zoo', 'limma',
#  'webr', 'org.Hs.eg.db', 'data.table', 'scales','ggforce')
#  for (pkg in requiredpackages) {
#    if (pkg %in% rownames(installed.packages()) == FALSE)
#    {install.packages(pkg)
#      tryCatch({BiocManager::install(pkg)},error=function(e){pkg})}
#  }

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github('CaronLab/MHCIatlas')

## ----eval=FALSE---------------------------------------------------------------
#  library(MHCIatlas)

## ----eval=FALSE---------------------------------------------------------------
#  df_human<- GetHumanMHCIdata(NetMHC_Rank_Threshold = 2,return_all_rawData = FALSE,
#                              omitThymus = TRUE)
#  df_mouse<- GetMouseMHCIdata(NetMHC_Rank_Threshold = 2,return_all_rawData = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  F2<-mkFigure2(df_human)

## ----eval=FALSE---------------------------------------------------------------
#  print(F2[[2]])

## ----eval=F-------------------------------------------------------------------
#  F3<-mkFigure3(df_human, df_mouse)

## ----eval=FALSE---------------------------------------------------------------
#  print(F3)

## ----eval=F-------------------------------------------------------------------
#  BasicAnalHuman(df_human,df_mouse)
#  BasicAnalMouse(df_mouse)

## ----eval=F-------------------------------------------------------------------
#  mkMouseConnectivityMap(df_mouse)

## ----eval=F-------------------------------------------------------------------
#  F4<-mkFigure4(df_mouse)

## ----eval=FALSE---------------------------------------------------------------
#  print(F4)

## ----eval=F-------------------------------------------------------------------
#  HumanHousekeepers<- HousekeepersHuman(df_human)
#  F5<-mkFigure5(
#    df_human,
#    HumanHousekeepers,
#    df_mouse,
#    useDefaultCons = TRUE,
#    ConsMouse = NA,
#    ConsHuman = NA
#  )

## ----eval=FALSE---------------------------------------------------------------
#  print(F5)

## ----eval=F-------------------------------------------------------------------
#  ConsHuman<- ConservationHuman(
#    df_human,
#    HumanHousekeepers,
#    pathBW_human = "~/Downloads/hg38.phastCons100way.bw",
#    samplesize = 2000,
#    quantile = 4,
#    ts_DonorSpecific = FALSE,
#    MinTissuesPerDonor = 15,
#    returnplots = TRUE
#  )
#  ConsMouse<- ConservationMouse(
#    df_mouse,
#    pathBW_mouse = "~/Downloads/mm10.60way.phastCons.bw",
#    samplesize = 2000,
#    returnplots = TRUE
#  )

## ----eval=F-------------------------------------------------------------------
#  HumanHousekeepers<- HousekeepersHuman(df_human)
#  F5<-mkFigure5(
#    df_human,
#    HumanHousekeepers,
#    df_mouse,
#    useDefaultCons = FALSE,
#    ConsMouse = ConsMouse,
#    ConsHuman = ConsHuman
#  )

## ----eval=FALSE---------------------------------------------------------------
#  print(F5)

## ----eval=FALSE---------------------------------------------------------------
#  corr_human<- MakeCorrHuman(df_human,Donors = c('all'),
#                             deconvolute_byHLAGene = FLASE,pValue_Threshold = 0.05,
#                             rsq_Threshold = 0.4,runAnalCorrHuman = TRUE)
#  corr_mouse<- MakeCorrMouse(df_mouse,pValue_Threshold = 0.01,rsq_Threshold = 0.4,useSILAC = F)

## ----eval=FALSE---------------------------------------------------------------
#  F6<-mkFigure6(corr_human,corr_mouse,GO_human = NA,GO_mouse = NA)

## ----echo=FALSE---------------------------------------------------------------
head(read.csv(system.file("extdata", "mouse_GOterms.csv", package = "MHCIatlas"))[c("Gene.Set.Name","FDR.qvalue")])

## ----eval=FALSE---------------------------------------------------------------
#  HumanHousekeepers<- HousekeepersHuman(df_human)
#  F6<-mkFigure6(corr_human,corr_mouse,GO_human = NA,GO_mouse = NA)

## ----eval=FALSE---------------------------------------------------------------
#  print(F6)

## ----eval=FALSE---------------------------------------------------------------
#  plots_human <- PlotsHumanProtCorr(
#    corr_human,
#    SigGene_names = NULL,
#    allSigprots = TRUE,
#    RankSigThreshold = 2,
#    path_filename = NA,
#    return_list_of_plots = TRUE
#  )
#  
#  plots_mouse <- PlotsMouseProtCorr(
#    corr_mouse,
#    pValue_Threshold = 0.01,
#    SigGene_names = NULL,
#    path_filename = NA,
#    return_list_of_plots = TRUE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  plots_mouse[['Erap1']]

## ----eval=FALSE---------------------------------------------------------------
#  names(plots_mouse)

## ----eval=FALSE---------------------------------------------------------------
#  plots_mouse[['CD84']]
#  names(plots_human)

## ----eval=FALSE---------------------------------------------------------------
#  mkHumanConnectivityMap(df_human)[[2]] #Supplementary Figure 1
#  SupplFigure2(df_human)
#  SupplFigure3(df_mouse)
#  SupplFigure4(df_human)
#  SupplFigure5(df_human)
#  SupplFigure6(df_mouse,df_human)
#  HousekeepersHuman(df_human) #Supplementary Figure 7
#  SupplFigure10(corr_human, corr_mouse, plots_human, plots_mouse)

## ----eval=FALSE---------------------------------------------------------------
#  read.csv(system.file("extdata", "mouse_GOterms.csv", package = "MHCIatlas"))

## ----eval=FALSE---------------------------------------------------------------
#  read.csv(system.file("extdata", "human_GOterms.csv", package = "MHCIatlas"))


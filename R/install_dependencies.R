requiredpackages<-c('BiocManager', 'FactoMineR', 'factoextra', 'stringr', 'ggplot2', 'cowplot', 'tidyr', 'stats',
                    'broom', 'ggrepel', 'ggpmisc', 'dplyr', 'magrittr', 'data.table', 'Matrix', 'pheatmap',
                    'reshape2', 'ggplotify', 'RColorBrewer', 'forcats', 'rtracklayer', 'TxDb.Mmusculus.UCSC.mm10.knownGene',
                    'TxDb.Hsapiens.UCSC.hg38.knownGene', 'AnnotationDbi', 'org.Mm.eg.db', 'GenomicFeatures', 'zoo', 'limma',
                    'webr', 'org.Hs.eg.db', 'data.table', 'scales','ggforce')
for (pkg in requiredpackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)
    tryCatch({BiocManager::install(pkg)},error=function(e){pkg})}
}
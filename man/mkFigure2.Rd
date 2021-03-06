% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mkFigure2.R
\name{mkFigure2}
\alias{mkFigure2}
\title{mkFigure2}
\usage{
mkFigure2(df_human)
}
\arguments{
\item{df_human}{Human MHCI tissue draft data frame from GetHumanMHCI data}
}
\value{
List with ggplot2 objects and data to generate the figure
}
\description{
Function to plot Figure 2 of the manuscript
}
\details{
DETAILS
}
\examples{
\dontrun{
F2 <- mkFigure2(df_human)
}
}
\seealso{
\code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{facet_wrap}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}},\code{\link[ggplot2]{coord_flip}},\code{\link[ggplot2]{labs}},\code{\link[ggplot2]{scale_colour_brewer}}
 \code{\link[cowplot]{ggdraw}},\code{\link[cowplot]{draw_plot}},\code{\link[cowplot]{draw_plot_label}}
 \code{\link[ggplotify]{as.ggplot}}
}

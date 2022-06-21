\name{gbm}
\alias{gbm}
\docType{data}
\title{
Multi-omic profiling of glioblastoma samples
}
\description{
These data serve as an example dataset to execute RFCluster on.These were processed and originally included in the iCluster R package which is likely to be archived on CRAN. I
have reincluded this dataset here to permit the example to be run.
}
\usage{data("gbm")}
\format{
A list of matrices containing Copy Number, Methylation and Expression estimates for 55 GBMs for 1500-1800 genes. Data were originally derived by The Cancer Genome Atlas project.
}
\source{
https://doi.org/10.1093/bioinformatics/btp659
}
\examples{
data(gbm)
}
\keyword{datasets}

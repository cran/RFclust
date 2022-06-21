\name{RFCluster}
\alias{RFCluster}
\title{
A wrapper for Random Forest Consensus Clustering
}

\description{

This takes a list of matrices of different data types , features in rows, samples in columns, and performs random forest clustering (one-dimensional). When multiple data types are available this is one way of modelling the data together.

}

\usage{
RFCluster(Data, ClustAlg = "pam", MaxK, nTrees = 1000,
exportFigures = "pdf", ClustReps = 500, ProjectName = "RFCluster",
verbose = TRUE, ...)
}


\arguments{
  \item{Data}{
Named list, contains matrices with samples in columns, features in rows. The names of the list should represent the platform or the feature type, such as expression, or CN, or clin; as long as it is distinct.
}
  \item{ClustAlg}{
  Algorithm for consensus clustering
  }
  \item{MaxK}{
  Maximum number of clusters you are searching for
  }
  \item{nTrees}{
  How many trees are we using in the random forest to generate a proximity matrix?
  }
  \item{ProjectName}{
  Name of the project, to annotate plots and other output
  }
  \item{ClustReps}{
  Number of replicates for consensus clustering
  }
  \item{verbose}{
  Should output be verbose?
  }
  \item{exportFigures}{
  Format of the results file for figures et cetera to be exported to
  }
  \item{...}{
  Other optional arguments, passed onto ConsensusClusterPlus; see that package's documentation for a full set.
  }
}

\value{

Standard output for ConsensusClusterPlus runs.

}
\references{
Monti, S., Tamayo, P., Mesirov, J. et al. Machine Learning (2003) 52: 91. https://doi.org/10.1023/A:1023949509487

Tao Shi & Steve Horvath (2006) Unsupervised Learning With Random Forest Predictors, Journal of Computational and Graphical Statistics, 15:1, 118-138, DOI: 10.1198/106186006X94072

}
\author{
Ankur Chakravarthy, PhD
}

\examples{

library(RFclust)

#Get GBM example data from the iCluster package, repackaged to maintain CRAN compatibility
data(gbm)

#Transpose so columns are samples and features are rows
gbm.t <- lapply(gbm, t)

#Make sure the sample names are the same across the matrices for the different
#samples - the code breaks otherwise

colnames(gbm.t[[2]]) <- colnames(gbm.t[[3]]) <- colnames(gbm.t[[1]])

#Run function on that dataset - these methods are computationally intensive
#so automatic testing during build has been disabled (takes > 5s).
#Users may test the software by running the code separately as the example is reproducible
\donttest{
Test.cluster <- RFCluster(Data = gbm.t, ClustAlg = "pam", MaxK = 5,
nTrees = 10, ProjectName = "RFCluster_Test", ClustReps = 50 , writeTable = FALSE, plot = NULL)
unlink("RFCluster_Test",recursive = TRUE)
}
}

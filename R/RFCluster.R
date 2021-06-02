##RFClust
##This is the workhorse function.
#' @export

RFCluster <- function(Data, ClustAlg = "pam", MaxK, nTrees = 1000, exportFigures = "pdf", ClustReps = 500, ProjectName = "RFCluster", verbose = TRUE, ...) {

  requireNamespace("ConsensusClusterPlus")
  requireNamespace("randomForest")

  ##Sanity check for sample matching.
if(length(Data) > 1) {
  Samples <- lapply(Data, colnames)
  Intersected <- Reduce(intersect, Samples)

    message("Standardising platforms, annotating features")
  for(i in 1:length(Data)) {

    Data[[i]] <- Data[[i]][,match(Intersected, colnames(Data[[i]]))]
    rownames(Data[[i]]) <- paste0(rownames(Data[[i]]),".", names(Data)[[i]])

  }

    CombinedData <- do.call(rbind, Data)

    } else {

    CombinedData <- unlist(Data)

    }

  CombinedData <- t(CombinedData)

  ## Run Random Forest Analysis

    ProximityMatrix <- randomForest::randomForest(CombinedData, ntree = nTrees, proximity = TRUE, oob.proximity = TRUE )
    ProximityMatrix <- 1-ProximityMatrix$proximity
    ProximityMatrix <- as.dist(ProximityMatrix)

  message("Proximity Matrix Computed")


  ## Run Consensus Clustering

  message("Running Consensus Clustering")
  Clusters <- ConsensusClusterPlus::ConsensusClusterPlus(ProximityMatrix, clusterAlg = ClustAlg, maxK = MaxK, pItem = 0.8, reps = ClustReps , plot = exportFigures, title = ProjectName, verbose = T)
  message("Completed")


  ## Add MDS representation

  return(Clusters)

}

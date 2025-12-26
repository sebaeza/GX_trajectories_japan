library(igraph)
library(parallel)
library(progress)

core_periphery_parallel <- function(graph, n_cores = NULL, verbose = TRUE) {
  if (is.null(n_cores))
    n_cores <- max(1, parallel::detectCores() - 1)
  if (verbose) message(sprintf("Using %d cores", n_cores))
  
  node_names <- V(graph)$name
  if (is.null(node_names))
    node_names <- as.character(1:vcount(graph))
  
  # Use sparse matrix to avoid memory overload
  A <- igraph::as_adjacency_matrix(graph, type = "both", sparse = TRUE)
  degrees <- igraph::degree(graph, mode = "all", loops = FALSE)
  thresholds <- sort(unique(degrees))
  
  if (verbose) {
    message(sprintf("Processing network with %d nodes", vcount(graph)))
    message(sprintf("Testing %d threshold values", length(thresholds)))
  }
  
  if (verbose) {
    pb <- progress_bar$new(
      format = "Processing [:bar] :percent eta: :eta",
      total = length(thresholds),
      clear = FALSE,
      width = 60
    )
  }
  
  results <- matrix(NA, nrow = length(thresholds), ncol = 4)
  colnames(results) <- c("threshold", "correlation", "core_size", "periphery_size")
  
  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    result <- process_threshold_sparse_cpp(A, degrees, t)
    assignments <- result$assignments
    core_size <- sum(assignments)
    results[i,] <- c(t,
                     result$correlation,
                     core_size,
                     length(assignments) - core_size)
    if (verbose) pb$tick()
  }
  
  results <- as.data.frame(results)
  if (verbose) message("\nFinding optimal threshold...")
  best_idx <- which.max(results$correlation)
  optimal_threshold <- results$threshold[best_idx]
  if (verbose) message("Creating final assignments...")
  final_result <- process_threshold_sparse_cpp(A, degrees, optimal_threshold)
  final_assignments <- final_result$assignments
  
  node_assignments <- data.frame(
    node = node_names,
    degree = degrees,
    assignment = final_assignments,
    stringsAsFactors = FALSE
  )
  node_assignments$group <- ifelse(node_assignments$assignment == 1, "core", "periphery")
  node_assignments <- node_assignments[order(-node_assignments$degree), ]
  
  if (verbose) message("Done!")
  
  structure <- list(
    corr = results$correlation[best_idx],
    threshold = optimal_threshold,
    core_size = sum(final_assignments),
    periphery_size = length(final_assignments) - sum(final_assignments),
    node_assignments = node_assignments,
    core_nodes = node_names[final_assignments == 1],
    periphery_nodes = node_names[final_assignments == 0],
    threshold_results = results,
    optimal_threshold = optimal_threshold,
    all_thresholds = thresholds,
    network_size = vcount(graph),
    density = edge_density(graph)
  )
  class(structure) <- c("core_periphery", "list")
  return(structure)
}

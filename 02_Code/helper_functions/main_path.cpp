// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <vector>
#include <deque>
#include <limits>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerVector global_standard_main_path(const arma::sp_mat& A) {
  // Number of nodes
  unsigned int n = A.n_rows;
  
  // Build adjacency and reverse adjacency lists and compute in-degrees.
  vector<vector<unsigned int>> adj(n), rev_adj(n);
  vector<unsigned int> indeg(n, 0);
  for (arma::sp_mat::const_iterator it = A.begin(); it != A.end(); ++it) {
    unsigned int i = it.row();
    unsigned int j = it.col();
    if(*it != 0){
      adj[i].push_back(j);
      rev_adj[j].push_back(i);
      indeg[j]++;
    }
  }
  
  // Compute topological order using Kahn's algorithm.
  deque<unsigned int> Q;
  vector<unsigned int> topo;
  for (unsigned int i = 0; i < n; i++) {
    if (indeg[i] == 0) Q.push_back(i);
  }
  while (!Q.empty()){
    unsigned int u = Q.front();
    Q.pop_front();
    topo.push_back(u);
    for (unsigned int v : adj[u]){
      indeg[v]--;
      if (indeg[v] == 0) Q.push_back(v);
    }
  }
  if(topo.size() != n) {
    stop("Graph is not a DAG");
  }
  
  // Compute forward counts f[u]: number of paths from any source to u.
  vector<double> f(n, 0.0);
  for (unsigned int i = 0; i < n; i++){
    if(rev_adj[i].empty()) { // source node
      f[i] = 1.0;
    }
  }
  for (unsigned int u : topo) {
    for (unsigned int v : adj[u]){
      f[v] += f[u];
    }
  }
  
  // Compute backward counts b[u]: number of paths from u to any sink.
  vector<double> b(n, 0.0);
  for (unsigned int i = 0; i < n; i++){
    if(adj[i].empty()) { // sink node
      b[i] = 1.0;
    }
  }
  for (int idx = topo.size()-1; idx >= 0; idx--){
    unsigned int u = topo[idx];
    for (unsigned int v : adj[u]){
      b[u] += b[v];
    }
  }
  
  // Dynamic programming to compute the global-standard main path.
  // dp[u] holds the maximum cumulative SPNP weight from any source to u.
  vector<double> dp(n, -std::numeric_limits<double>::infinity());
  vector<int> pred(n, -1);
  for (unsigned int i = 0; i < n; i++){
    if(rev_adj[i].empty()){
      dp[i] = 0.0; // initialize source nodes
    }
  }
  for (unsigned int u : topo) {
    for (unsigned int v : adj[u]){
      double weight = f[u] * b[v];  // SPNP weight on edge (u,v)
      if(dp[u] + weight > dp[v]){
        dp[v] = dp[u] + weight;
        pred[v] = u;
      }
    }
  }
  
  // Identify the sink with the highest cumulative weight.
  double best = -std::numeric_limits<double>::infinity();
  int best_node = -1;
  for (unsigned int i = 0; i < n; i++){
    if(adj[i].empty() && dp[i] > best){
      best = dp[i];
      best_node = i;
    }
  }
  if(best_node == -1){
    stop("No sink found in the graph");
  }
  
  // Reconstruct the main path by backtracking from the best sink.
  vector<int> path;
  for (int cur = best_node; cur != -1; cur = pred[cur]) {
    path.push_back(cur);
  }
  reverse(path.begin(), path.end());
  
  // Return the path as 1-indexed (to match R's indexing)
  IntegerVector res(path.size());
  for (unsigned int i = 0; i < path.size(); i++){
    res[i] = path[i] + 1;
  }
  return res;
}

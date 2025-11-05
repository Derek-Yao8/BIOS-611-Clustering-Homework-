####################################################################################
# Task 2
library(plotly)
library(ggplot2)
library(cluster)
generate_shell_clusters <- function(n_shells, k_per_shell, max_radius, noise_sd = 0.1) {
  # Define evenly spaced radii
  inner_radius <- max_radius / (n_shells + 1)
  radii <- seq(inner_radius, max_radius, length.out = n_shells)
  
  out_list <- vector("list", n_shells)
  
  for (s in seq_len(n_shells)) {
    # Determine how many points this shell gets
      k <- k_per_shell
    
    r_base <- radii[s]
    
    # Generate random directions uniformly on unit sphere
    M <- matrix(rnorm(k * 3), ncol = 3)
    dirs <- M / sqrt(rowSums(M^2))
    
    # Add Gaussian noise to radius for shell thickness
    r_noisy <- pmax(1e-8, r_base + rnorm(k, mean = 0, sd = noise_sd))
    
    coords <- dirs * r_noisy
    
    out_list[[s]] <- data.frame(
      x = coords[, 1],
      y = coords[, 2],
      z = coords[, 3],
      shell_id = s,
      r_base = r_base,
      r_noisy = r_noisy
    )
  }
  
  do.call(rbind, out_list)
}

############################################### Interactive 3D Plot
library(plotly)

set.seed(888)
dat <- generate_shell_clusters(4, 500, max_radius = 4, noise_sd = 0.12)
dat$shell_id <- factor(dat$shell_id)
dat$tooltip <- sprintf(
  "Shell: %s<br>x: %.3f<br>y: %.3f<br>z: %.3f<br>r_base: %.2f<br>r_noisy: %.2f",
  dat$shell_id, dat$x, dat$y, dat$z, dat$r_base, dat$r_noisy
)

plot_ly(
  dat,
  x = ~x, y = ~y, z = ~z,
  color = ~shell_id,
  type = "scatter3d",
  mode = "markers",
  text = ~tooltip,
  hoverinfo = "text",
  marker = list(size = 2, opacity = 0.7)
) |>
  layout(scene = list(aspectmode = "data"))



############################################### Wrapper function
spectral_cluster_alg <-   function(x, k, d_threshold = 1) {
    
    X <- as.matrix(x)
    n <- nrow(X)
    
    # 1) Build Similarity Graph
    dmat <- as.matrix(dist(X, method = "euclidean"))
    A <- (dmat <= d_threshold) * 1L
    diag(A) <- 0L
    A <- (A | t(A)) * 1L
    
    deg <- rowSums(A)
    
    # 2) Symmetric Normalized Laplacian
    D <- diag(deg)
    L <- D - A
    invsqrt_deg <- ifelse(deg > 0, 1 / sqrt(deg), 0)
    Dmhalf <- diag(invsqrt_deg, n, n)
    Lsym <- Dmhalf %*% L %*% Dmhalf
    
    # 3) Eigen-decomposition
    ee <- eigen(Lsym, symmetric = TRUE)
    ord <- order(ee$values, decreasing = FALSE)
    U <- ee$vectors[, ord[seq_len(k)], drop = FALSE]
    
    # Row-normalize
    rnorms <- sqrt(rowSums(U^2))
    rnorms[rnorms == 0] <- 1
    normalized_eigenvectors <- U / rnorms
    
    # k-means on normalized eigenvector matrix
    km <- stats::kmeans(normalized_eigenvectors, centers = k, nstart = 25,
                        iter.max = 1000, algorithm = "Lloyd")
    
    return(km)
  }

############################################### Simulation
radii_grid <- 10:0

sim_results <- lapply(radii_grid, function(Rmax) {
  dat <- generate_shell_clusters(n_shells = 4, k_per_shell = 100, max_radius = Rmax, noise_sd = 0.1)
  X <- as.matrix(dat[, c("x", "y", "z")])
  
  g <- clusGap(X, FUNcluster = spectral_cluster_alg, K.max = 5, B = 10, verbose = FALSE)
  
  k_firstSEmax <- maxSE(g$Tab[, "gap"], g$Tab[, "SE.sim"], method = "firstSEmax")
  k_globalMax  <- which.max(g$Tab[, "gap"])
  
  data.frame(
    max_radius   = Rmax,
    k_firstSEmax = k_firstSEmax,
    k_globalMax  = k_globalMax
  )
})

summary_df <- do.call(rbind, sim_results)
summary_df <- summary_df[order(summary_df$max_radius, decreasing = TRUE), ]
print(summary_df, row.names = FALSE)
  


ggplot(summary_df, aes(x = max_radius, y = k_firstSEmax)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 3, color = "steelblue") +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red", linewidth = 1) +
  scale_x_reverse(breaks = summary_df$max_radius) +   # larger radius on left → failure to the right
  labs(
    title = "Spectral Clustering Estimated Number of Clusters vs. Shell Radius",
    x = "Maximum Shell Radius",
    y = "Estimated Number of Clusters (Gap Statistic)"
  ) +
  theme_minimal(base_size = 14)


ggsave("/home/rstudio/work/Spectral_clustering_plot.png", width = 16, height = 5, units = "in", dpi = 300)


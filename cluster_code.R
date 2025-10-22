library(cluster)
library(ggplot2)

####################################################################################
# Task 1
# Generate 
# ---- 1. Data generator ----
generate_hypercube_clusters <- function(n, k, side_length, noise_sd = 1.0) {
  centers <- diag(side_length, n)
  X_list <- vector("list", n)
  for (i in seq_len(n)) {
    noise <- matrix(rnorm(k * n, mean = 0, sd = noise_sd), nrow = k, ncol = n)
    X_list[[i]] <- sweep(noise, 2, centers[i, ], `+`)
  }
  X <- do.call(rbind, X_list)
  X
}

# ---- 2. Gap-statistic wrapper ----
estimate_k <- function(X, K.max, B = 50, nstart = 20, iter.max = 50) {
  # Ensure numeric matrix
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  # Define kmeans wrapper in correct format for clusGap
  FUN_km <- function(x, k) stats::kmeans(x, centers = k, nstart = nstart, iter.max = iter.max)
  set.seed(1)
  g <- clusGap(X, FUN = FUN_km, K.max = K.max, B = B)
  list(
    k_firstSEmax = maxSE(g$Tab[, "gap"], g$Tab[, "SE.sim"], method = "firstSEmax"),
    k_globalMax  = which.max(g$Tab[, "gap"]),
    gap = g
  )
}

# ---- 3. Simulation parameters ----
dims <- c(6, 5, 4, 3, 2)
side_lengths <- 10:1
k <- 100
noise_sd <- 1.0
B <- 50

# ---- 4. Run simulation ----
set.seed(123)
results <- data.frame()

for (n in dims) {
  for (s in side_lengths) {
    cat(sprintf("Running n=%d, side_length=%d...\n", n, s))
    X <- generate_hypercube_clusters(n, k, s, noise_sd)
    K.max <- max(2, n + 3)
    est <- estimate_k(X, K.max = K.max, B = B, nstart = 20, iter.max = 50)
    results <- rbind(results, data.frame(
      n = n,
      side_length = s,
      true_k = n,
      k_hat = est$k_firstSEmax
    ))
  }
}

# ---- 5. Visualization ----
ggplot(results, aes(x = side_length, y = k_hat, color = factor(n))) +
  geom_point(size = 2) +
  geom_line() +
  geom_line(aes(y = true_k), linetype = "dashed", color = "black") +
  scale_x_reverse(breaks = 10:1) +
  labs(
    title = "Gap Statistic Estimates vs. Cluster Separation",
    subtitle = "Dashed line = true number of clusters",
    x = "Side length (clusters move closer → right to left)",
    y = "Estimated number of clusters (firstSEmax rule)",
    color = "Dimension (true #clusters)"
  ) +
  theme_minimal(base_size = 13)











####################################################################################
# Task 2









#' Co-inertia Analysis Example
#'
#' This script demonstrates co-inertia analysis using simulated ecological data.
#' Authors: Antoine Lucas, Julien Petot, Chloé Tellier
#'
#' Required packages: ade4, adegraphics

library(ade4)
library(adegraphics)

# =============================================================================
# 1. CTMI Function (Cardinal Temperature Model with Inflection)
# =============================================================================

#' Fish abundance based on temperature (Cardinal Temperature Model)
#' @param T Temperature value
#' @param param Vector of parameters: c(Tmin, Topt, Tmax, Muopt)
#' @return Abundance value
CTMI <- function(T, param) {
  Tmin <- param[1]
  Topt <- param[2]
  Tmax <- param[3]
  Muopt <- param[4]

  if (T <= Tmin || T >= Tmax) {
    return(0)
  } else {
    Num <- (T - Tmax) * (T - Tmin)^2
    Den <- (Topt - Tmin) *
      ((Topt - Tmin) * (T - Topt) - (Topt - Tmax) * (Topt + Tmin - 2 * T))
    return(Muopt * Num / Den)
  }
}

# =============================================================================
# 2. Simulate Abundance Data (Table 1)
# =============================================================================

# 16 stations with their average temperatures
Tstations <- seq(from = 5, to = 80, by = 5)

# Species parameters
nsp <- 10
topt <- seq(from = 5, to = 100, length = nsp) # Optimal temperatures
tmin <- 0.953 * topt - 28.913 # Minimum temperatures
tmax <- 1.101 * topt + 3.203 # Maximum temperatures

# Build abundance data frame (10 species × 16 stations)
data <- data.frame(matrix(nrow = nsp, ncol = length(Tstations)))
colnames(data) <- paste0("st", seq_along(Tstations))
rownames(data) <- paste0("sp", round(topt, 0))

for (i in 1:nsp) {
  for (j in seq_along(Tstations)) {
    data[i, j] <- round(
      CTMI(T = Tstations[j], param = c(tmin[i], topt[i], tmax[i], 1000)),
      0
    )
  }
}

# Visualize species distribution along temperature gradient
Taxis <- 1:100
cols <- rev(rainbow(nsp, end = 5 / 6))

plot(
  x = Taxis,
  y = sapply(Taxis, CTMI, param = c(tmin[1], topt[1], tmax[1], 1000)),
  type = "l",
  col = cols[1],
  main = paste("Distribution of", nsp, "species"),
  xlab = "Temperature [°C]",
  ylab = "Abundance",
  lwd = 2
)
for (i in 2:nsp) {
  lines(
    x = Taxis,
    y = sapply(Taxis, CTMI, param = c(tmin[i], topt[i], tmax[i], 1000)),
    col = cols[i],
    lwd = 2
  )
}

# Shuffle data (simulate real-world scenario where order is unknown)
set.seed(42)
data.vv <- data[sample(nrow(data)), sample(ncol(data))]

# Visualize original structure
table.value(data, clegend = 0, main = "Original data (ordered)")
table.value(data.vv, clegend = 0, main = "Shuffled data")

# =============================================================================
# 3. Simulate Temperature Data (Table 2)
# =============================================================================

# Temperature measurements at each station for each hour of the day
set.seed(1)
mesureT <- sapply(Tstations, function(x) {
  10 * sin(seq(0, pi, length = 24)) + x + rnorm(n = 24, sd = 5)
})
colnames(mesureT) <- paste0("st", 1:16)
rownames(mesureT) <- paste0("h", 1:24)

# Visualize temperature curves
plot(
  0,
  type = "n",
  xlim = c(1, 24),
  ylim = range(mesureT),
  xlab = "Hour",
  ylab = "Temperature",
  las = 1,
  main = "Station temperatures throughout the day"
)
apply(mesureT, 2, lines)

# =============================================================================
# 4. Factorial Analyses
# =============================================================================

# Correspondence Analysis on abundance data
afc <- dudi.coa(data.vv, scannf = FALSE)
scatter(afc, main = "CA - Abundance data")

# Reorder data based on CA to reveal gradient
data.vv.ord <- data.vv[order(afc$li[, 1]), order(afc$co[, 1])]
table.value(data.vv.ord, clegend = 0, main = "Reordered data")

# Principal Component Analysis on temperature data
acp <- dudi.pca(t(mesureT), scannf = FALSE)
scatter(acp, main = "PCA - Temperature data")

# F1 represents the temperature gradient
plot(
  Tstations,
  acp$li[, 1],
  ylab = "F1",
  xlab = "Temperature",
  main = "First PCA axis = temperature gradient",
  pch = 19
)

# =============================================================================
# 5. Co-inertia Analysis
# =============================================================================

# Prepare tables with common individuals in rows
tab1 <- as.data.frame(t(data))
tab2 <- as.data.frame(t(mesureT))

# Verification 1: Same dimensions
stopifnot(nrow(tab1) == nrow(tab2))

# Verification 2: Same row names in same order
stopifnot(all.equal(rownames(tab1), rownames(tab2)))

# Perform analyses with consistent weights
coa <- dudi.coa(df = tab1, scannf = FALSE, nf = 2)
pca <- dudi.pca(df = tab2, row.w = coa$lw, scannf = FALSE, nf = 2)

# Verification 3: Same weights
stopifnot(all.equal(pca$lw, coa$lw))

# Co-inertia analysis
cia <- coinertia(dudiX = pca, dudiY = coa, scannf = FALSE, nf = 2)

# Variance explained by first two factors
cat(
  "Variance explained by F1:",
  round(cia$eig[1] / sum(cia$eig) * 100, 1),
  "%\n"
)
cat(
  "Variance explained by F2:",
  round(cia$eig[2] / sum(cia$eig) * 100, 1),
  "%\n"
)

# =============================================================================
# 6. Test of Co-structure Significance
# =============================================================================

# RV coefficient
cat("RV coefficient:", round(cia$RV, 3), "\n")

# Monte-Carlo test
ciatest <- randtest(cia, nrepet = 999, fixed = 2)
plot(ciatest, main = "Monte-Carlo test for RV coefficient")

# =============================================================================
# 7. Summary Visualization
# =============================================================================

par(mfrow = c(2, 2))

# Temperature data (PCA)
table.value(
  pca$tab,
  clabel.row = 0.7,
  clabel.col = 0.7,
  main = "PCA table (temperature)"
)

# Abundance data (CA)
table.value(
  t(coa$tab),
  clabel.row = 0.7,
  clabel.col = 0.7,
  main = "CA table (abundance)"
)

# Co-inertia table
table.value(
  cia$tab,
  clabel.row = 0.7,
  clabel.col = 0.7,
  main = "Co-inertia table"
)

par(mfrow = c(1, 1))

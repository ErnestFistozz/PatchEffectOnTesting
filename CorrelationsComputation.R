library("Kendall")
# Read data from file
test_data <- total_data[total_data$TouchedConfigFiles == 0,]

# Load Kendall's tau function
library("Kendall")

# Define a function to calculate Kendall's tau coefficient
calculate_tau <- function(data) {
  # Split data by repoName
  
  repo_groups <- split(data, data$unique_label)
  # Initialize empty vectors to store results
  repos <- character()
  taus <- numeric()
  
  # Loop through each repo
  for (repo_name in names(repo_groups)) {
    repo_data <- repo_groups[[repo_name]]
    print("CurrentProject")
    print(repo_data)
    print("EndCurrentProject")
    coverage_sd <- ifelse(all(is.na(repo_data$Coverage)), 0, sd(repo_data$Coverage, na.rm = TRUE))
    patchCoverage_sd <- ifelse(all(is.na(repo_data$PatchCoverage)), 0, sd(repo_data$PatchCoverage, na.rm = TRUE))
    
    if (is.na(coverage_sd) || is.na(patchCoverage_sd)) {
      tau <- 0
    } else {
      tau <- cor(repo_data$Coverage, repo_data$PatchCoverage, method = "kendall")
    }
    # Store repoName and corresponding tau value
    tau <- round(tau, 3)
    repos <- c(repos, repo_name)
    taus <- c(taus, tau)
  }
  
  # Create a data frame with repoName and tau values
  result <- data.frame(RepositoryName = repos, tau = taus)
  return(result)
}

# Calculate Kendall's tau coefficient
tau_results <- calculate_tau(test_data)

# Overall Results

overrallPatchToCoverageTau <- cor(test_data$PatchCoverage, test_data$Coverage, method = "kendall")
overrallPatchToDMMTau <- cor(test_data$PatchCoverage, test_data$DMM, method = "kendall")
overrallDMMToCoverageTau <- cor(test_data$DMM, test_data$Coverage, method = "kendall")

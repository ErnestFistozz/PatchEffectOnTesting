library(gridExtra)
library(ggplot2)
library(cowplot)
library(dplyr)
library(stringr)

coveralls_df <- read.csv('FinalCoverallsAsyncResults.csv')
coveralls_df_headers <- c("TimeStamp","CommitHash","RepositoryName", "BranchName", "Coverage",
                          "SystemLines", "TouchedTestFiles","TouchedCodeFiles","TouchedTestCodeFiles",
                          "TouchedConfigFiles","CodePatchSize", "TestPatchSize", "ConfigPatchSize",
                          "PatchCoverage","DMMUnitSize","DMMUnitComplexity","DMMUnitInterface","DMM","CrapMetric")

colnames(coveralls_df) <- coveralls_df_headers


NetFlixGenie <- subset(coveralls_df, RepositoryName == "Netflix/genie")
coveralls_df <- coveralls_df[coveralls_df$RepositoryName != "Netflix/genie", ]

coveralls_df$TimeStamp <- as.POSIXct(coveralls_df$TimeStamp)

repo_summary <- coveralls_df %>%
  arrange(TimeStamp) %>%
  group_by(RepositoryName) %>%
  summarise(
    TimeRange = as.integer(round(as.numeric(difftime(max(TimeStamp), min(TimeStamp), units = "days")) / 30.44)),  # Round to the nearest integer
    Builds = n(),
    StartCoverage = first(PatchCoverage),
    EndCoverage = last(PatchCoverage)
  )

# Filter based on summary
filtered_repo_names <- repo_summary$RepositoryName[repo_summary$Builds >= 100 & repo_summary$TimeRange >= 24]
filtered_data <- coveralls_df[coveralls_df$RepositoryName %in% filtered_repo_names, ]

# unique repository name
repo_names <- unique(filtered_data$RepositoryName)
# Assigning unique labels
labels <- paste0("CL", sprintf("%02d", seq_along(repo_names)))
names(labels) <- repo_names
# Adding labels to data
filtered_data$unique_label <- labels[filtered_data$RepositoryName]

coveralls_ordered_df <- filtered_data %>%
  group_by(RepositoryName) %>%
  arrange(desc(n())) %>%
  ungroup()

# #####################################################

codecov_df <- read.csv('FinalCodecovAsyncResults.csv')
codecov_df_headers <- c("TimeStamp","CommitHash","RepositoryName", "BranchName", "Coverage",
                        "SystemLines", "TouchedTestFiles","TouchedCodeFiles","TouchedTestCodeFiles","TouchedConfigFiles",
                        "CodePatchSize", "TestPatchSize", "ConfigPatchSize",
                        "PatchCoverage","ApiPatchCoverage","DMMUnitSize","DMMUnitComplexity","DMMUnitInterface","DMM","CrapMetric")

colnames(codecov_df) <- codecov_df_headers
codecov_df <- codecov_df %>% select(-ApiPatchCoverage) # remove API Coverage

# Add NetFlixGennie ToCodeCov
codecov_df <- merge(codecov_df, NetFlixGenie, all = TRUE)

codecov_summary <- codecov_df %>%
  group_by(RepositoryName) %>%
  summarise(
    Timediff = as.integer(round(as.numeric(difftime(max(TimeStamp), min(TimeStamp), units = "days")) / 30.44)),  # Round to the nearest integer
    TotalBuilds = n()
  )

# Filter based on summary
codecov_filtered_repo_names <- codecov_summary$RepositoryName[codecov_summary$TotalBuilds >= 100 & codecov_summary$Timediff >= 24]
filtered_codecov_data <- codecov_df[codecov_df$RepositoryName %in% codecov_filtered_repo_names, ]

# unique repository name
repository_names <- unique(filtered_codecov_data$RepositoryName)
# Assigning unique labels
labels <- paste0("CV", sprintf("%02d", seq_along(repository_names)))
names(labels) <- repository_names
# Adding labels to data
filtered_codecov_data$unique_label <- labels[filtered_codecov_data$RepositoryName]

codecov_ordered_df <- filtered_codecov_data %>%
  group_by(RepositoryName) %>%
  arrange(desc(n())) %>%
  ungroup()

## Summary Statistics
coverallDataSummary <- coveralls_ordered_df %>%
  arrange(TimeStamp) %>%
  group_by(RepositoryName,unique_label) %>%
  summarise(
    TimeSpan = as.integer(round(as.numeric(difftime(max(TimeStamp), min(TimeStamp), units = "days")) / 30.44)),  # Round to the nearest integer
    NoBuilds = n(),
    StartCoverage = first(PatchCoverage),
    EndCoverage = last(PatchCoverage)
  )

codecovDataSummary <- codecov_ordered_df %>%
  arrange(TimeStamp) %>%
  group_by(RepositoryName,unique_label) %>%
  summarise(
    TimeDifference = as.integer(round(as.numeric(difftime(max(TimeStamp), min(TimeStamp), units = "days")) / 30.44)),  # Round to the nearest integer
    TotalNoBuilds = n(),
    StartCoverage = first(PatchCoverage),
    EndCoverage = last(PatchCoverage)
  )

########### Combine

#sourceCodeCoveralls <- coveralls_ordered_df[coveralls_ordered_df$TouchedConfigFiles == 1, ]
#sourceCodeCodecov <- codecov_ordered_df[codecov_ordered_df$TouchedConfigFiles == 1, ]


total_data <- merge(coveralls_ordered_df, codecov_ordered_df, all = TRUE)

# unique_repo_names <- unique(coveralls_ordered_df$RepositoryName)
# num_unique_repo_names <- length(unique_repo_names)


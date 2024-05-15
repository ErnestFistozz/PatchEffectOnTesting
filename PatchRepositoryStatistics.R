# Install and load necessary packages
if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)

data <- coveralls_df


# Convert TimeStamp to datetime
data$TimeStamp <- as.POSIXct(data$TimeStamp)

# Arrange data by RepositoryName and TimeStamp
data_sorted <- data %>% arrange(RepositoryName, TimeStamp)

repo_summary <- data_sorted %>%
  group_by(RepositoryName) %>%
  summarise(
    StartTime = min(TimeStamp),
    EndTime = max(TimeStamp),
    StartCoverage = first(SystmeCoverage),
    EndCoverage = last(SystmeCoverage),
    TimeRange = as.integer(round(as.numeric(difftime(max(TimeStamp), min(TimeStamp), units = "days")) / 30.44)),  # Round to the nearest integer
    Builds = n(),  # Number of rows (builds) per RepositoryName
    #CodeFile_Percentage = round(sum(TouchedCodeFiles) / n() * 100, 2),
    #TestFile_Percentage = round(sum(TouchedTestFiles) / n() * 100, 2),
    #TestCodeFile_Percentage = round(sum(TouchedTestCodeFiles) / n() * 100, 2),
    #ConfigFile_Percentage = round(sum(TouchedConfigFiles) / n() * 100, 2),
    #Avg_CodeSize = as.integer(mean(CodePatchSize)),
    #Avg_TestSize = as.integer(mean(TestPatchSize)),
    #Avg_ConfigSize = as.integer(mean(ConfigPatchSize))
  )

# Print the result
print(repo_summary)



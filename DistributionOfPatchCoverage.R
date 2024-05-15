# Define bins for grouping the data
bins <- c(-1, 0, 25, 50, 75, 99, 100)

#df <- coveralls_df#[coveralls_df$TouchedConfigFiles == 0, ]

#unique_labels <- paste0("CL", sprintf("%02d", 1:nlevels(as.factor(grouped_data$RepositoryName))))
#names(unique_labels) <- levels(as.factor(grouped_data$RepositoryName))
#grouped_data$unique_label <- unique_labels[grouped_data$RepositoryName]

# Create the ggplot

# Aggregate the data and calculate the count and percentage

#patch_df <- filtered_data

patch_df <- total_data[total_data$TouchedConfigFiles == 0,]

grouped_data <- aggregate(PatchCoverage ~ unique_label + cut(PatchCoverage, breaks = bins, include.lowest = TRUE), patch_df, length)
total_counts <- aggregate(PatchCoverage ~ unique_label, patch_df, length)
grouped_data <- merge(grouped_data, total_counts, by = "unique_label")
colnames(grouped_data) <- c("unique_label", "bin", "count", "total_count")
grouped_data$percentage <- (grouped_data$count / grouped_data$total_count) * 100


plot_patch_distribution <- function(data, filename){
  p <- ggplot(data, aes(x = percentage*100, y = unique_label, fill = bin)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("darkred", "red", "yellow", "lightgreen", "green", "darkgreen"),
                    labels = c("0", "(0-25]", "(25-50]", "(50-75]", "(75-100)", "100")) +
  theme_minimal() +
  theme( legend.position = "bottom",
    axis.text.y = element_text( size=13),
    axis.text.x = element_text(  size=13),
    axis.line.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text( size = 16),
    panel.grid = element_blank())+
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100))
  print(p)
  ggsave(str_interp("${filename}.pdf"), plot = p, width = 14, height = 9)
}

plot_patch_distribution(grouped_data, "distributionOfPatchCoverage")


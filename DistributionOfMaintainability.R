# Define bins for grouping the data
bins <- c(-1, 0, .25, .50, .75, .999, 1.00)

#df <- coveralls_df#[coveralls_df$TouchedConfigFiles == 0, ]

#unique_labels <- paste0("CL", sprintf("%02d", 1:nlevels(as.factor(dmm_data$RepositoryName))))
#names(unique_labels) <- levels(as.factor(dmm_data$RepositoryName))
#dmm_data$unique_label <- unique_labels[dmm_data$RepositoryName]

# Create the ggplot

# Aggregate the data and calculate the count and percentage

#patch_df <- filtered_data

patch_df <- total_data[total_data$TouchedConfigFiles == 0,]

dmm_data <- aggregate(DMM ~ unique_label + cut(DMM, breaks = bins, include.lowest = TRUE), patch_df, length)
total_counts <- aggregate(DMM ~ unique_label, patch_df, length)
dmm_data <- merge(dmm_data, total_counts, by = "unique_label")
colnames(dmm_data) <- c("unique_label", "bin", "count", "total_count")
dmm_data$percentage <- (dmm_data$count / dmm_data$total_count) * 100


plot_patch_distribution <- function(data, filename){
  p <- ggplot(data, aes(x = percentage*100, y = unique_label, fill = bin)) +
    geom_bar(stat = "identity", position = "fill") +
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    scale_fill_manual(values = c("darkred", "red", "yellow", "lightgreen", "green", "darkgreen"),
                      labels = c("0", "(0-0.25]", "(0.25-0.50]", "(0.50-0.75]", "(0.75-1.00)", "1.00")) +
    theme_minimal() +
    theme( legend.position = "bottom",
           axis.text.y = element_text(size=13),
           axis.text.x = element_text( size=13),
           axis.line.y = element_blank(),
           legend.title = element_blank(),
           legend.text = element_text(size = 16),
           panel.grid = element_blank())+
    guides(fill = guide_legend(nrow = 1)) +
    scale_x_continuous(labels = scales::percent_format(scale = 100))
  print(p)
  ggsave(str_interp("${filename}.pdf"), plot = p, width = 14, height = 9)
}

plot_patch_distribution(dmm_data, "distributionOfDMM")


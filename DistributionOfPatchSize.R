#Production - ready

plot_size_dist <- function(data, filename) {
  p <- ggplot(data, aes(x = totalFiles, y = unique_label)) +
    geom_boxplot(fill = "lightblue", color = "darkgreen") +
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(face = "bold",  size=13),
          axis.text.x = element_text(face = "bold",  size=20),
          axis.line.y = element_blank(),
          legend.title = element_blank(),
          panel.grid = element_blank()) +
    scale_x_continuous(trans = 'log10',
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x)))
  print(p)
  ggsave(str_interp("${filename}.pdf"), plot = p, width = 16, height = 9)
}

sourceLoC_df <- total_data[total_data$TouchedConfigFiles == 0,]
totalFilesSize <- sourceLoC_df %>%
  rowwise() %>%
  mutate(totalFiles = sum(CodePatchSize, TestPatchSize))

plot_size_dist(totalFilesSize, "distributionOfPatchSize")






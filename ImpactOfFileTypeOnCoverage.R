# Arrange data by timestamp
orderedData <- total_data %>%
  arrange(RepositoryName, TimeStamp)


tolerance <- 1e-10 

coverageDiff <- orderedData %>%
  group_by(RepositoryName, unique_label) %>%
  mutate(coverage_diff = Coverage - lag(Coverage,default = first(Coverage)),
         change_category = case_when(
           TouchedConfigFiles == 0 & coverage_diff < 0 ~ "Decrease",
           TouchedConfigFiles == 0 &  abs(coverage_diff) < tolerance  ~ "No change",
           TouchedConfigFiles == 0 & coverage_diff > 0 ~ "Increase",
           TouchedConfigFiles == 1 & coverage_diff < 0 ~ "Negative",
           TouchedConfigFiles == 1 &  abs(coverage_diff) < tolerance  ~ "No Impact",
           TouchedConfigFiles == 1 & coverage_diff > 0 ~ "Positive",
           TRUE ~ "Other"
         )
  ) %>%
  ungroup()


coverageDiffPercentage <- coverageDiff %>%
  group_by(RepositoryName, change_category, unique_label) %>%
  summarize(
    count = n(),
    percentage = n() / nrow(coverageDiff) * 100
  ) %>%
  group_by(RepositoryName,unique_label) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

color_mapping <- c(
  "Decrease" = "darkred",
  "No change" = "yellow",
  "Increase" = "green",
  "Negative" = "red",
  "No Impact" = "yellow",
  "Positive" = "darkgreen")

p <- ggplot(coverageDiffPercentage, aes(x = percentage , y = unique_label, fill = change_category)) +
  geom_bar(stat = "identity",position = "stack") +
  labs(
    title = NULL,
    y = NULL,
    x = NULL) +
  scale_fill_manual(values = color_mapping,
                    breaks = c("Decrease", "Negative", "No Impact", "No change", "Increase","Positive"),
                    name = "Patch Touching Source Code\nPatch Touching Non-Source Code") +
  #scale_fill_manual(values = color_mapping,
  #                  breaks = c("Negative", "No Impact", "Positive"),
  #                  name = "Category 2") +
  #scale_fill_manual(values = color_mapping) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme(legend.position = "bottom",
        axis.text.y = element_text( size=13),
        axis.text.x = element_text( size=13),
        axis.line = element_blank(),
        # axis.line.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.key.size=unit(10,"pt"),
        legend.text = element_text(size = 14),
        aspect.ratio=0.6,
        text=element_text(size=10),
        strip.text = element_text(size = 10),
        strip.background = element_blank(), 
        axis.ticks.x = element_blank(),
        title = element_text( size = 13),
        axis.ticks.length = unit(0, "cm"))
  theme_minimal()

print(p)
filename <- "PatchImpactToCoverage"
ggsave(str_interp("${filename}.pdf"), plot = p, width = 14, height = 9)

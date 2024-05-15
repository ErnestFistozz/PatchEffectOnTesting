library(gridExtra)
library(ggplot2)
library(cowplot)
#df <- read.csv('DataFile.csv')
#df <- read.table("RawData.txt",sep = ',')
data_headers <- c("date", "hash", "coverage", "lines", "complexity")
df <- read.table("rawData.txt", sep = ",", col.names = data_headers)
df$date <- as.Date(df$date)

raw_data <- df


EMA <- function(x, n) {
  alpha <- 2 / (n + 1)
  ema <- numeric(length(x))
  ema[1] <- x[1]
  for (i in 2:length(x)) {
    ema[i] <- alpha * x[i] + (1 - alpha) * ema[i - 1]
  }
  return(ema)
}

# Calculate EMA for coverage
coverage <- df$coverage
ema_values <- EMA(coverage, 30)  # Adjust the smoothing factor (n) as needed

# Filter out noisy points in coverage
for (i in 2:length(coverage)) {
  if (coverage[i] < 0.7 * ema_values[i - 1]) {  # Check if the drop is 30% or more
    coverage[i] <- ema_values[i - 1]  # Replace with the previous EMA value
  }
}

# Update the coverage column in dataframe
df$coverage <- coverage

# Convert date column to Date format



plot_raw <- ggplot(data = raw_data, aes(x = date, y = coverage)) +
  geom_line(color = "black") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.line.y = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank()) 
  labs(title = NULL,
       x = NULL,
       y = "Coverage")+
  ylim(0, 50) 
 


plot_clean <- ggplot(data = df, aes(x = date, y = coverage)) +
  geom_line(color = "black") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.line.y = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank()) 
  labs(title = NULL,
      x = NULL,
       y = "Coverage")+
  ylim(0, 50)

filename <- "ApacheGobblin"
final <- plot_grid(plot_raw, plot_clean, ncol = 1,label_x = "Timestamp")
print(final)
#ggsave(str_interp("${filename}.pdf"), plot = final, width = 10, height = 6)


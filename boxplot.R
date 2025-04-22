# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
#############################3
#imptact facotor
#############################
df <- read_excel("SW data/rb data.xlsx")

df$datagroup <- factor(df$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

# Plot

  ggplot(df, aes(x=datagroup, y=Impact_Factor, fill=datagroup)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for Impact Factor by Data Group",
         x = "Data Group",
         y = "Impact Factor") +
    theme_minimal() +
    scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))


#logimpact factor
  logdf <- read_excel("SW data/rb data.xlsx", sheet = "logimpact")
  logdf$datagroup <- factor(logdf$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))
  
  # Plot
  
  ggplot(logdf, aes(x=datagroup, y=ln_impactFactor, fill=datagroup)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    labs(title = "Boxplot for log(Impact Factor) by Data Group",
         x = "Data Group",
         y = "log(Impact Factor)") +
    theme_minimal() +
    scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))
  
  
  
  #############################3
  #number of cluster
  ############################# 
df2 <- read_excel("SW data/rb data.xlsx", sheet = "cluster")  

df2$datagroup <- factor(df2$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

# Plot

ggplot(df2, aes(x=datagroup, y=Number_of_clusters, fill=datagroup)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for Number of Cluster by Data Group",
       x = "Data Group",
       y = "Number of Cluster") +
  theme_minimal() +
  scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))


mean_Number_of_clusters <- mean(df2$Number_of_clusters,na.rm = TRUE)
sd_Number_of_clusters <- sd(df2$Number_of_clusters,na.rm = TRUE)

sum(is.na(df2$Number_of_clusters))
df2 <- na.omit(df2)
# Filter out extreme values: more than 3 standard deviations from the mean
df_filtered <- df2[abs(df2$Number_of_clusters - mean_Number_of_clusters) <= 3 * sd_Number_of_clusters, ]

df_filtered <- df2[abs(df2$Number_of_clusters - mean_Number_of_clusters) <= 3 * sd_Number_of_clusters, ]
df_filtered$datagroup <- factor(df_filtered$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

ggplot(df_filtered, aes(x=datagroup, y=Number_of_clusters, fill=datagroup)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for Number of Cluster by Data Group (exclude 3sd apart)",
       x = "Data Group",
       y = "Number of Cluster") +
  theme_minimal() +
  scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))



#log number of cluster
logdf2 <- read_excel("SW data/rb data.xlsx", sheet = "logcluster")
logdf2$datagroup <- factor(logdf2$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

# Plot

ggplot(logdf2, aes(x=datagroup, y=ln_Number_of_clusters, fill=datagroup)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for log(Number of Cluster) by Data Group",
       x = "Data Group",
       y = "log (Number of Cluster)") +
  theme_minimal() +
  scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))







#############################3
#sample size
############################# 



df3 <- read_excel("SW data/rb data.xlsx", sheet = "samplesize")
df3$datagroup <- factor(df3$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

# Plot

ggplot(df3, aes(x=datagroup, y=Sample_size, fill=datagroup)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for Sample Size by Data Group",
       x = "Data Group",
       y = "Sample Size") +
  theme_minimal() +
  scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))


mean_Sample_size <- mean(df3$Sample_size,na.rm = TRUE)
sd_Sample_size <- sd(df3$Sample_size,na.rm = TRUE)

sum(is.na(df3$Sample_size))

# Filter out extreme values: more than 3 standard deviations from the mean
df_filtered <- df3[abs(df3$Sample_size - mean_Sample_size) <= 3 * sd_Sample_size, ]
df_filtered$datagroup <- factor(df_filtered$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

ggplot(df_filtered, aes(x=datagroup, y=Sample_size, fill=datagroup)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for Sample_size by Data Group (exclude 3sd apart)",
       x = "Data Group",
       y = "Sample_size") +
  theme_minimal() +
  scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))


#logsample size
logdf3 <- read_excel("SW data/rb data.xlsx", sheet = "logsamlpesize")
logdf3$datagroup <- factor(logdf3$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

# Plot

ggplot(logdf3, aes(x=datagroup, y=ln_Sample_size, fill=datagroup)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for log(Sample Size) by Data Group",
       x = "Data Group",
       y = "log(Sample Size)") +
  theme_minimal() +
  scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))










df4 <- read_excel("SW data/rb data.xlsx", sheet = "publish year")
df4$datagroup <- factor(df4$datagroup, levels = c(0, 1), labels = c("Data not available", "Data available"))

# Plot

ggplot(df4, aes(x=datagroup, y=Publication_year, fill=datagroup)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  labs(title = "Boxplot for Publication year by Data Group",
       x = "Data Group",
       y = "Publication year ") +
  theme_minimal() +
  scale_fill_manual(values = c("Data not available" = "#FF6347", "Data available" = "#4682B4"))

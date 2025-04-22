# fliud trial 
#the overall Fluid volume in a cluster cross-over trial in each period.
#We also want this by hospital site. So one overall, and then 7 separate ones for each hospital

library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
fluidtry <- read_excel("volumn graph/fluidtry.xlsx")

df <-fluidtry
df$arm <- factor(df$arm, levels = c("Ringer’s Lactate", "Normal Saline"))
df$fluid <-factor(df$fluid, levels = c("Ringer’s Lactate", "Normal Saline"))
df$valueround<- format(df$valueround, big.mark = ",", scientific = FALSE)




df$Site


######################
#All Sites
######################

df2 <- df %>%
  filter(Site == "All Sites")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )
 
df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 200000, by = 25000),  # Customize as needed
  )+
  labs(
  #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
  x = "Study Period",
  y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
      values = c(
        "Normal Saline" = "#ACE1AF"   , # Light Green
        "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
      )) 


#scale_fill_manual(values = c("SomeGroup" = "#94F097".
 #                            values = c("SomeGroup" = "#94F097")
  #                           values = c("SomeGroup" = "#98FF98")
  #                           values = c("SomeGroup" = "#77DD77")
  
#                          values = c("SomeGroup" = "#ACE1AF"                            
#"Normal Saline" = "#7CCD7C",))
#94F097	A custom blend between #90EE90 and #98FB98
#Mint Green	#98FF98	Slightly brighter than palegreen, still soft
#Pastel Green	#77DD77	A more saturated but gentle green
#Celadon	#ACE1AF



######################
#Site 1 – TOH General
######################

df2 <- df %>%
  filter(Site == "Site 1 – TOH General")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )

df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 300000, by = 5000),  # Customize as needed
  )+
  labs(
    #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
    x = "Study Period",
    y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
    values = c(
      "Normal Saline" = "#ACE1AF"   , # Light Green
      "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
    )) 






######################
#Site 2 – Hamilton General
######################

df2 <- df %>%
  filter(Site == "Site 2 – Hamilton General")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )

df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 300000, by = 5000),  # Customize as needed
  )+
  labs(
    #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
    x = "Study Period",
    y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
    values = c(
      "Normal Saline" = "#ACE1AF"   , # Light Green
      "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
    )) 







######################
#Site 3 – Queensway Carleton
######################

df2 <- df %>%
  filter(Site == "Site 3 – Queensway Carleton")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )

df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 25000, by = 2500),  # Customize as needed
  )+
  labs(
    #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
    x = "Study Period",
    y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
    values = c(
      "Normal Saline" = "#ACE1AF"   , # Light Green
      "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
    )) 



######################
#Site 4 – TOH Civic
######################

df2 <- df %>%
  filter(Site == "Site 4 – TOH Civic")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )

df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 25000, by = 2500),  # Customize as needed
  )+
  labs(
    #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
    x = "Study Period",
    y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
    values = c(
      "Normal Saline" = "#ACE1AF"   , # Light Green
      "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
    )) 





######################
#Site 5 – Montfort
######################

df2 <- df %>%
  filter(Site == "Site 5 – Montfort")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )

df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 12000, by = 2500),  # Customize as needed
  )+
  labs(
    #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
    x = "Study Period",
    y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
    values = c(
      "Normal Saline" = "#ACE1AF"   , # Light Green
      "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
    )) 






######################
#Site 6 – London Victoria
######################

df2 <- df %>%
  filter(Site == "Site 6 – London Victoria")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )

df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 30000, by = 5000),  # Customize as needed
  )+
  labs(
    #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
    x = "Study Period",
    y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
    values = c(
      "Normal Saline" = "#ACE1AF"   , # Light Green
      "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
    )) 





######################
#Site 7 – London University
######################

df2 <- df %>%
  filter(Site == "Site 7 – London University")%>%
  mutate(
    label = paste0(valueround, "\n(", round(per1 * 100, 1), "%)")
  )

df2

ggplot(data=df2, aes(x=arm, y=value, fill=fluid)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.8)+
  geom_text(aes(label=label), vjust=-0.1, hjust = 0.5,  color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  scale_y_continuous(
    breaks = seq(0, 25000, by = 5000),  # Customize as needed
  )+
  labs(
    #title = "Overal All Sites Fluid Volume in a Cluster Cross-over Trial in Each Period",
    x = "Study Period",
    y = "Fluid volume (Litres)")+
  labs(fill = "Fluid Inventory Usage")+
  scale_fill_manual(
    values = c(
      "Normal Saline" = "#ACE1AF"   , # Light Green
      "Ringer’s Lactate" = "#4F94CD"     # Steel Blue 1
    )) 







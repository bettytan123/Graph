#sw graph bar

# library
library(ggplot2)
library(ggrepel)
swdatatry <- read_excel("SW data/swdatatry.xlsx")

categories <- c(
  "Statement indicates \nno data available \n(n = 16)",
  "Statement indicates \ndata available upon \nrequest (n = 89)",
  "No statement\n/Unclear (n = 98)",
  "Dataset linked, \nattached or posted \n(n = 15)"
)

# Repeat categories twice
category_column <- rep(categories, 2)


# Create the dataframe
df <- data.frame(category_column, swdatatry[2:5])
df$category_column <- factor(df$category_column, levels = c("Statement indicates \nno data available \n(n = 16)", "Statement indicates \ndata available upon \nrequest (n = 89)", "No statement\n/Unclear (n = 98)", "Dataset linked, \nattached or posted \n(n = 15)"))
# Rename the columns
colnames(df) <- c("status", "datagroup", "value", "percent", "valueprint")

# Display the dataframe
print(df)



# Stacked mauniscript 
ggplot(df, aes(fill=datagroup, y=value, x=status)) + 
  geom_bar(position="stack", stat="identity", width = 0.5)+
  labs(
    title = "Data Sharing According to Manuscript Statement (N= 218)", 
    #subtitle = "graphipate ",
    x = "Data Sharing Statement",
    y = "Frequency (Percentage)",
    fill = "Data Availability"
  ) +
  geom_text(aes(label=valueprint),
            position = position_stack(vjust = 0.5),colour="black",size=5)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), # Center title
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 13, face = "bold"),  # X-axis label size
    axis.title.y = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 13))  # Increase legend text size  # Y-axis label size
 


#angle = 45, hjust = 1, vjust = 1,


#sw graph pie
swdatapie <- read_excel("SW data/swdatapie.xlsx")
#swdatapie2$Value1 <- ifelse(is.na(bargraph2$Value1),"",bargraph2$Value1)
swdatapie$status <- factor(swdatapie$status, levels = c("Data obtained", "Authors could not be contacted", "Data sharing too complex", "Declined/Data no longer available","Lost to follow up","Terminated follow up","No respondent"))

ggplot(swdatapie, aes(x="", y=percent, group=status, fill=status,color=status)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0, direction = -1) +
  facet_grid(.~ datagroup)+
  theme_void() +
  labs(
    title = "Data Sharing Response from Email Response for Two Groups", 
    #subtitle = "Group1: data were available upon request (N=89) | Group2: no/unclear data sharing statements (N=98)",
    fill = "Data Sharing Responses"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), # Center title
    legend.title = element_text(size = 13, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 13),
    strip.text = element_text(size = 13))+
  scale_fill_brewer(palette="Set2")+ 
  #geom_label_repel(y=0.4,label = swdatapie$valueprint1,show.legend = FALSE, nudge_x = 1,  color="black",size=5)+
  geom_text(aes(x=1.6, label = valueprint),position = position_stack(vjust = 0.5), color="black",size=5)
  #geom_text(aes(label = valueprint1),position = position_stack(vjust = 0.5), color="black",size=5)+
  #geom_label_repel(aes(y = 1, label = valueprint1, size=4,show.legend = FALSE, nudge_x = 1)) 
  
    # Increase legend text size  # Y-axis label size

  #myPalette <- brewer.pal(5, "Set2") 
  #paste0(round(proportion*100), "%")




#data pie all for data sharing statment 
datapieall <- read_excel("SW data/datapieall.xlsx")
datapieall$Data_sharing_responses <- factor(datapieall$Data_sharing_responses, levels = c("Data obtained", "Authors could not be contacted", "Data sharing too complex", "Declined/Data no longer available","Lost to follow up","Terminated follow up","No respondent","Not emailed/Ineligible"))


ggplot(datapieall, aes(x="", y=Percent, fill=Data_sharing_responses)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0, direction = -1) +
  theme_void() +
  labs(
    title = "Data Sharing Response from All N=218 Participants", 
    #subtitle = "Group1: data were available upon request (N=89) | Group2: no/unclear data sharing statements (N=98)",
    fill = "Data Sharing Responses"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), # Center title
    legend.title = element_text(size = 13, face = "bold"),  # Increase legend title size
    legend.text = element_text(size = 13),
    strip.text = element_text(size = 13))+
  scale_fill_brewer(palette="Set2")+ 
  geom_text(aes(x=1.6, label = valueprint),position = position_stack(vjust = 0.5), color="black",size=5)


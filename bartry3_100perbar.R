library(ggplot2)
bargraph2$Question <- factor(bargraph2$Question, levels=paste0("B",c(1,2,4,6,7,9,10)))
bargraph2$Words <- ifelse(is.na(bargraph2$Words),"",bargraph2$Words)
bargraph2$Value1 <- ifelse(is.na(bargraph2$Value1),"",bargraph2$Value1)

bargraph2$Value2 <- ifelse(is.na(bargraph2$Value2),"",bargraph2$Value2)
bargraph2$Value3 <- ifelse(is.na(bargraph2$Value3),"",bargraph2$Value3)


y = rep(1,28)

library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
dt <- mpg%>%
  dplyr::group_by(manufacturer, class)%>%
  dplyr::tally()%>%
  dplyr::mutate(percent=n/sum(n))
pl <- ggplot(data = dt,aes(x= manufacturer, y = n,fill = class))
pl <- pl + geom_bar(stat="identity", position ="fill")
pl <- pl + geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
                     position=position_fill(vjust=0.5), colour="white")

pl <- pl + theme_minimal()
pl <- pl + labs(title ="My title")
pl <- pl + labs(subtitle ="My subtitle")
pl <- pl + labs(caption ="My caption")
pl <- pl  + labs(x ="Car Brand", y = "Percentage")

pl <- pl + coord_flip()
pl


# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
sum(value)
# Stacked + percent
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="fill", stat="identity")





ggplot(bargraph2, aes(x = Response, y = Percent, fill = Understand)) +
  geom_bar(stat = "identity", position = "fill",width = 0.9) +
  facet_grid(rows = vars(Question)) +
  coord_flip()+
  scale_x_discrete(expand = expansion(add = c(2, 2.5)))+
  labs(
    title = "Section B (Yes: response 4 or 5 vs No: response 1 or 2 or 3) between RAFF4 Patients VS Non-RAFF4 Patients", 
    subtitle = "When you agreed to participate/were approached to participate in the RAFF4 trial, how well do you feel you understood...",
    x = "Question | Patient Group",
    y = "Response Frequency(Percentage)",
    fill = "Understanding Level"
  ) +
  geom_text(aes(label=paste0(Value, " (", sprintf("%1.1f", Percent), "%)")),
            position=position_fill(vjust=0.5), colour="black",size=3)+
  geom_text(aes(label=Words,x=3.5,y=0),hjust=0,size=3)
  
  
  
  
  geom_text(aes(label = Value1),size = 3,position = position_stack(vjust = 0.5))+
  #geom_text(aes(label=Words, x=2.5,y=as.numeric(Question)),hjust=0.1,vjust=0.7,size=3)+
  geom_text(aes(label = Value2),size = 3,position = position_stack(vjust = 0.5))+
  #geom_text(aes(label = Value3),size = 3,position = position_stack(vjust = 2))+
  geom_text(aes(label = Value3, y = y+0.15 ), position = position_dodge(0.9),vjust = 0.3,hjust=-1.5,size=3)+
  geom_text(aes(label=Words,x=3.5,y=0),hjust=0,size=3)


x=as.numeric(Question)+0.25

ggplot(data = df, aes(x, y, group = grp)) +
  geom_col(aes(fill = grp), position = "dodge") +
  geom_text(
    aes(label = y, y = y + 0.15),
    position = position_dodge(0.9),
    vjust = 0
  )



#geom_text(
aes(label = Value2, y = y+0.15 ),
position = position_dodge(0.9),
vjust = 0,hjust=-3,size=3
)
x=as.numeric(bargraph2$Question)
x

?geom_text

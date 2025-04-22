bargraph3$Question <- factor(bargraph3$Question, levels=paste0("B",c(1,2,4,6,7,9,10)))
bargraph3$Words <- ifelse(is.na(bargraph3$Words),"",bargraph3$Words)
bargraph3$Words1 <- ifelse(is.na(bargraph3$Words1),"",bargraph3$Words1)
bargraph3$Value1 <- ifelse(is.na(bargraph3$Value1),"",bargraph3$Value1)
bargraph3$Value2 <- ifelse(is.na(bargraph3$Value2),"",bargraph3$Value2)
bargraph3$Value3 <- ifelse(is.na(bargraph3$Value3),"",bargraph3$Value3)

ggplot(bargraph3, aes(x = Response, y = Value, fill = Understand)) +
  geom_bar(stat = "identity", position = "stack",width = 0.5) +
  facet_grid(rows = vars(Question)) +
  labs(
    title = "Section B (Yes: response 4 or 5 vs No: response 1 or 2 or 3) between RAFF4 Patients VS Non-RAFF4 Patients", 
    subtitle = "When you agreed to participate/were approached to participate in the RAFF4 trial, how well do you feel you understood...",
    x = "Question | Patient Group",
    y = "Response Frequency",
    fill = "Understanding Level"
  ) +
  coord_flip() +
  ylim(0,175)+
  xlim(0,3)+
  geom_text(aes(label = Value1),size = 3,position = position_stack(vjust = 0.5))+
  geom_text(aes(label=Words, x=2.5,y=as.numeric(Question)),hjust=0.1,vjust=0.7,size=3)+
  geom_text(aes(label = Value2),size = 3,position = position_stack(vjust = 0.7))+
  geom_text(aes(label = Value3),size = 3,position = position_stack(vjust = 1.9))+
  scale_x_continuous(expand = expansion(mult = c(0.5, 0.5)))




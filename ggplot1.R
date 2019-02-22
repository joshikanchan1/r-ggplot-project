library('tidyverse')
getwd()
dir()

interviews_plotting <- read_csv("data_output/interviews_plotting.csv")

ggplot(data = interviews_plotting)

ggplot(data = interviews_plotting,
       aes(x = no_membrs, y = number_items)) +
       geom_point()
       
interviews_plot +
       geom_line()
   
interviews_plot +
  geom_point()    

interviews_plot +
  geom_bar()
   
ggplot(data = interviews_plotting,
       aes(x = no_membrs, y = number_items)) +
       geom_point(alpha = 0.5)

ggplot(data = interviews_plotting,
       aes(x = no_membrs, y = number_items)) +
       geom_jitter(alpha = 0.5)

interviews_plot <- ggplot(data = interviews_plotting, aes(x=no_membrs, y=number-items))
       geom_jitter(alpha=0.5, width = 0.15, height = 0.15)

ggsave("fig_output/membrs_item.png", interviews_plot, width = 15, height = 10, 
       dpi = 72)

ggplot(data=interviews_plotting, aes(x=no_membrs, y=number_items)) +
  geom_jitter(alpha=0.5, width=0.2, height = 0.2, size=4)+
  
ggplot(data=interviews_plotting, aes(x=no_membrs, y=number_items)) +
  geom_jitter(aes(color=village), alpha=0.5, width=0.2, height = 0.2, size=4)+
  geom_smooth(method = "lm")

ggplot(data=interviews_plotting, aes(x=no_membrs, y=number_items, color=village)) +
  geom_jitter(alpha=0.5, width=0.2, height = 0.2, size=4)+
  geom_smooth(method = "lm")

interviews_plot <- ggplot(data=interviews_plotting, 
                          aes(x=no_membrs, y=number_items, color=village)) +
  geom_jitter(alpha=0.5, width=0.2, height = 0.2, size=4)+
  geom_smooth(method = "lm")

ggsave("fig_output/membrs_item.png", interviews_plot, width = 15, height = 10, 
       dpi = 72)

ggplot(data = interviews_plotting,
       aes(x = rooms, y = village)) +
       geom_point(alpha = 0.5)

ggplot(data = interviews_plotting,
       aes(x = village, y = rooms)) +
       geom_jitter(alpha = 0.5)

interviews_plot <- ggplot(data=interviews_plotting, 
                          aes(x=rooms, y=village, color=respondent_wall_type)) +
  geom_jitter(alpha=0.5, width=0.2, height = 0.2, size=4)+
  geom_smooth(method = "lm")

ggplot(data=interviews_plotting, aes(x=village, y=rooms)) +
  geom_jitter(aes(color=village), alpha=0.5, width=0.2, height = 0.2, size=2)+
  geom_smooth(method = "lm")

ggplot(data=interviews_plotting, aes(x=village, y=rooms)) +
  geom_jitter(aes(color=respondent_wall_type), alpha=0.5, width=0.2, height = 0.2, size=3)+
  geom_smooth(method = "lm")

ggplot(data=interviews_plotting, aes(x=respondent_wall_type, y=village)) +
  geom_jitter(aes(color=respondent_wall_type), alpha=0.5, width=0.2, height = 0.2, size=2)+
  geom_smooth(method = "lm")

ggplot(interviews_plotting, aes(x = no_membrs))+
  geom_histogram()

ggplot(interviews_plotting, aes(x = no_membrs))+
  geom_histogram(binwidth = 1, color="white")

ggplot(interviews_plotting, aes(x = no_membrs, fill=village))+
  geom_histogram(binwidth = 1, color="white")

ggplot(interviews_plotting, aes(x = no_membrs, color=village))+
  geom_freqpoly(binwidth = 1)

ggplot(interviews_plotting, aes(x = no_membrs, y=stat(density), color=village))+
  geom_freqpoly(binwidth = 1)

ggplot(interviews_plotting, aes(x = no_membrs, color=village))+
  geom_density()

ggplot(interviews_plotting, aes(x = no_membrs, fill=village))+
  geom_density(alpha=0.4)

ggplot(interviews_plotting, aes(x = no_membrs, fill=village, color=village))+
  geom_density(alpha=0.1)

ggplot(data = interviews_plotting,
       aes(x = respondent_wall_type, fill = village)) +
       geom_bar(position = "dodge") + facet_grid(~ respondent_wall_type)

ggplot(data = interviews_plotting,
       aes(x = village, fill = village)) +
       geom_bar(position = "dodge") + facet_wrap(~ respondent_wall_type)

ggplot(data = interviews_plotting,
       aes(x = village, fill = village)) +
       geom_bar(position = "dodge") + facet_wrap(~ respondent_wall_type) +
       theme_bw()
#graphs per village_bar graph
counts = table(interviews_plotting$village, interviews_plotting$respondent_wall_type)
counts = data.frame(counts)
colnames(counts) = c("village", "wall_type", "Freq")

ggplot(counts,aes(x=wall_type, y=Freq,fill=village))+
  geom_col(position = "dodge")

ggplot(interviews_plotting, 
       aes(x = respondent_wall_type, fill = village))+
  geom_bar(position = "fill")+
  ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), colour = "white")

ggplot(interviews_plotting, 
       aes(x = village, fill = respondent_wall_type))+
  geom_bar(position = "fill")+
  ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position = position_fill(vjust = 0.5), colour = "white")

#boxplots

ggplot(data = interviews_plotting,
       aes(x = respondent_wall_type, y = rooms)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, width = 0.2, height = 0.2, colour = "tomato")

ggplot(data = interviews_plotting,
       aes(x = respondent_wall_type, y = rooms, fill = village)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, width = 0.2, height = 0.2, colour = "tomato")

ggplot(data = interviews_plotting,
       aes(x = respondent_wall_type, y = rooms, fill = village)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5, width = 0.2, height = 0.2, colour = "tomato")

#village wise box plot by wall_type and room numbers
ggplot(data = interviews_plotting,
       aes(x = respondent_wall_type, y = rooms)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha=0.5, width = 0.2, height = 0.2, 
              aes(colour = village))

ggplot(data = interviews_plotting,
       aes(x = respondent_wall_type, fill = village)) +
  geom_bar(position = "dodge") + 
  facet_wrap(~respondent_wall_type, y=liv_count)) + 
  geom_jitter(alpha=0.5, width = 0.2, height = 0.2, 
              aes(colour = village))

#rooms vs respondent_wall_type by member of irrgn assc

ggplot(data = interviews_plotting,
       aes(x = respondent_wall_type, fill = village)) +
  geom_bar(position = "dodge") + 
  facet_wrap(~respondent_wall_type, y=liv_count)) + 
  geom_jitter(alpha=0.5, width = 0.2, height = 0.2, 
              aes(colour = memb_assoc))

#bar_graph
ggplot(data = interviews_plotting,
  aes(x = respondent_wall_type, fill = village)) +
  geom_bar(position = "dodge") + 
  facet_wrap(~respondent_wall_type) + 
  scale_fill_brewer(palette = "RdBu") 

#detailed boxplot
ggplot(data = interviews_plotting,
  aes(x = respondent_wall_type, y=liv_count, fill = memb_assoc, 
      colour = memb_assoc)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha=0.5, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             jitter.height = 0.1)) 
  
#didn't work
ggplot(data = interviews_plotting,
  aes(x = respondent_wall_type, y=liv_count, fill = memb_assoc, 
      colour = memb_assoc)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha=0.5, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             jitter.height = 0.1)) +
  geom_text(aes(label = stat(count), y = 5.5,
            position = position_dodge(), colour = "white"))

counts <- data.frame(table(interviews_plotting$respondent_wall_type, 
                           interviews_plotting$memb_assoc, useNA = "ifany"))

name(counts) <- c("wall_type", "memb_assoc", "Freq")

#Violin plots

ggplot(data = interviews_plotting,
  aes(x = respondent_wall_type, y = rooms)) +
  geom_violin(alpha=0) +
  geom_jitter(alpha=0.5, width=0.2, height=0.2, colour = "tomato")

#violin plots per village by wall type and no of rooms

ggplot(data = interviews_plotting,
  aes(x = respondent_wall_type, y = rooms, colour = village)) +
  geom_violin(alpha=0) +
  geom_jitter(alpha=0.5, width=0.2, height=0.2, colour = "tomato")

#contour plot

str(interviews_plotting)

ggplot(data = interviews_plotting,
  aes(x = no_membrs, y = years_liv, z =liv_count)) +
  geom_contour()

ggplot(data = interviews_plotting, aes(fill=respondent_wall_type, x=village)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  ylab("Proportion") + xlab("village")

ggplot(data = interviews_plotting, aes(fill=respondent_wall_type, x=village)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  ylab("Proportion") + xlab("village") + 
  ggtitle("Proportion of wall type by village")

#combined bar

ggplot(data = interviews_plotting, aes(fill=respondent_wall_type, x=village)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  labs(x "village", y ="Proportion", title="Wall type by village")

#legend_walltype

ggplot(data = interviews_plotting, aes(fill=respondent_wall_type, x=village)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  labs(x "village", y ="Proportion", title="Wall type by village") +
  guides(fill=guide_legend(title = "Wall type"))

#inserting heading with command scale_fill_discrete

ggplot(data = interviews_plotting, aes(fill=respondent_wall_type, x=village)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  labs(x "village", y ="Proportion", title="Wall type by village") +
  scale_fill_discrete(labels=("burnt brick", "cement", "mud daub", "sun bricks")) +
  guides(fill=guide_legend(title="Wall type"))

#using memb_assoc
#facet for columns and rows, 
#add lable in x-graph
ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2)


#add lable in x-graph - text in x-bar

labels = c("", "") 

ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  scale_x_discrete(labels= c("burnt brick", "cement", "mud daub", "sun bricks")) +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2) +
  theme(axis.text.x = element_text(angle=45))

ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  scale_x_discrete(labels= c("burnt brick", "cement", "mud daub", "sun bricks")) +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# changing themes

ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  scale_x_discrete(labels= c("burnt brick", "cement", "mud daub", "sun bricks")) +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))


ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  scale_x_discrete(labels= c("burnt brick", "cement", "mud daub", "sun bricks")) +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2) +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  scale_x_discrete(labels= c("burnt brick", "cement", "mud daub", "sun bricks")) +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#Changing font size

ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  scale_x_discrete(labels= c("burnt brick", "cement", "mud daub", "sun bricks")) +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2) +
  theme_classic() +
  ggtitle("Plot title") +
  guides(fill=guide_legend(title="Member\nAssociation")) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 10), 
       plot.title = element_text(hjust=0.5))

my_theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10) +
                    plot.title = element(hjust=0.5))


ggplot(data = interviews_plotting, aes(fill=memb_assoc, x=respondent_wall_type)) +
  geom_bar(position = "fill") +
  stat_count(geom = "text", aes(label=stat(count)), 
             position=position_fill(vjust=0.5), colour="white") +
  scale_x_discrete(labels= c("burnt brick", "cement", "mud daub", "sun bricks")) +
  scale_y_continuous(breaks=c(0, 0.5, 1)) +
  labs(x = "Wall type", y ="Proportion") + facet_wrap(~village, nrow=2) +
  theme_classic() +
  ggtitle("Plot title") +
  guides(fill=guide_legend(title="Member\nAssociation")) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 10), 
       plot.title = element_text(hjust=0.5))



V <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
V <- 
  
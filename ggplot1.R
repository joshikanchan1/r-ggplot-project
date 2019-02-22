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

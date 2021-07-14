library(tidytuesdayR)
library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(ggpubr)

tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo

head(scoobydoo)
str(scoobydoo)

scooby6 <- scoobydoo %>%
  select(matches("snack|caught"), -unmask_other, -caught_other, -caught_not) %>% 
  mutate_if(is.character, as.factor)

scooby6 <- scooby6 %>% 
  pivot_longer(everything(), names_to = "person", values_to = "number") %>% 
  filter(number == TRUE) %>% 
  group_by(person) %>% 
  count(person) %>% 
  separate(person, c("act", "name"), sep = "_") %>%
  mutate(name = factor(name)) %>% 
  ungroup() %>% 
  group_by(name)

plot1 <- ggplot(scooby6, aes(x = name, y = n)) +
  geom_point(aes(shape = act), size = 3) +
  geom_line(aes(group = name), color ="#CB4335") +
  coord_flip() +
  theme_ipsum(axis_title_size = 0) +
  scale_shape_manual(values = c(19,15)) +
  scale_y_continuous(limits = c(0,160)) +
  labs(title = "Scooby Doo",
       subtitle = "Character's Performance:Reward") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(color = c("#8E44AD", "#2980B9", "#7E5109", "#52BE80", "#F39C12"),
                                   size = 15),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 15, face = "bold")
        )

plot2 <- ggplot(scooby5, aes(x = name, y = n, fill = act)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#7E5109", "#52BE80")) +
  theme_ipsum(axis_title_size = 0) +
  labs(title = "Who loves to take credits?",
       caption = "#TidyTuesday Week 29 \nData: ScoobyPedia",
       subtitle = "I'm the one who <span style = 'color:#7E5109'><b>catch</b></span>the bad guy, you
       <span style = 'color:#52BE80'><b>unmask</b></span> them smh") +
  theme(legend.position = "none",
        axis.text.x = element_text(color = c("#8E44AD", "#2980B9", "#7E5109", "#52BE80", "#F39C12")),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = element_markdown(size = 10, face = "bold", hjust = 0.5)
        )

ggarrange(plot1, plot2, nrow = 2)

#dodged barplot

scooby4 <- scoobydoo %>%
  select(matches("snack|unmask|caught"), -unmask_other, -caught_other, -caught_not) %>% 
  mutate_if(is.character, as.factor)

scooby4 <- scooby4 %>% 
  pivot_longer(everything(), names_to = "person", values_to = "number") %>% 
  filter(number == TRUE) %>% 
  group_by(person) %>% 
  count(person) %>% 
  separate(person, c("act", "name"), sep = "_") %>%
  mutate(name = factor(name)) %>% 
  ungroup() %>% 
  group_by(name)

ggplot(scooby4, aes(x = name, y = n, fill = act)) +
  geom_bar(stat = "identity", position = "dodge")

scooby5 <- scoobydoo %>% 
  select(matches("unmask|caught"), -unmask_other, -caught_other, -caught_not) %>% 
  mutate_if(is.character, as.factor)

scooby5 <- scooby5 %>% 
  pivot_longer(everything(), names_to = "person", values_to = "number") %>% 
  filter(number == TRUE) %>% 
  group_by(person) %>% 
  count(person) %>% 
  separate(person, c("act", "name"), sep = "_") %>%
  mutate(name = factor(name)) %>% 
  ungroup() %>% 
  group_by(name)

ggplot(scooby5, aes(x = name, y = n, fill = act)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#7E5109", "#52BE80")) +
  theme_ipsum(axis_title_size = 0) +
  labs(title = "Who Loves to Take Credit?",
       )




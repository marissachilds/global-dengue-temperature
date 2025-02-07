library(tidyverse)
library(cowplot)

tasks_all = read.csv("./ref_tables/country_tasks.csv")

#load in world map from geoboundaries
areas <-readRDS("./data/dengue_temp_full.rds") %>%
  mutate(time = as.Date(paste(year, month, 1, sep="-"), "%Y-%m-%d"))%>%
  mutate(id = paste(id, country)) %>%
  arrange(country) %>%
  mutate(id = fct_inorder(factor(id, ordered=TRUE)))

areas %>% group_by(id, country) %>%
  summarize(non_na = sum(!is.na(dengue_cases))) %>%
  filter(non_na > 0) -> areas_withdata

areas %<>% right_join(areas_withdata) %>% filter(!is.na(dengue_cases))

areas$id <- factor(areas$id, levels = rev(sort(unique(areas$id))))
areas$inc_log <- log(areas$dengue_cases/areas$pop)
areas$inc_log <- ifelse(is.infinite(areas$inc_log),NA, areas$inc_log)

uni_prov <- areas %>% select(id, country) %>% distinct() %>% map_df(rev)# %>% filter(country != "BRA")

# Hlines and y-axis labels for country groupings
mid_ind<-sapply(unique(uni_prov$country), function(x) floor(mean(which(uni_prov$country==x))))
last_ind<-sapply(unique(uni_prov$country), function(x) max(which(uni_prov$country==x))+.5)
uni_prov$name<-""
uni_prov$name[mid_ind]<-unique(uni_prov$country)

# plot monthly data smoothly
year_seq <- seq(from=1992, to=2020, by=5)
breaks <- make_datetime(year_seq, 1)

{ggplot(areas %>% mutate(panel = ifelse(country == "BRA", 2, 1))) +
  geom_tile(aes(x=as.character(time), y=id, fill=inc_log))+
  geom_hline(data = data.frame(yval = last_ind, panel = 1), 
             aes(yintercept=yval), colour="lightblue")+
  geom_vline(xintercept=as.character(make_datetime(1992:2020)), colour="lightblue", lty="dashed")+
  scale_y_discrete(breaks=uni_prov$id, labels=uni_prov$name)+
  scale_fill_viridis_c(name="Incidence (Log)", na.value = 'black', option = "magma",
                     limits=range(areas$inc_log, na.rm=TRUE))+
  scale_x_discrete(breaks = as.character(breaks), labels = as.character(year_seq))+
  facet_wrap(~panel, nrow = 1, scales = "free") + 
  ylab("Province")+
  xlab("Time")+
  theme_classic()+
  theme(axis.ticks=element_blank(), 
        legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank())} %>% 
  ggsave(filename = "./figures/figureS2_dengue_heatmap.png", 
         width = 6*1.2, height = 8*1.2)

areas %>% 
  mutate(time = as.Date(paste(year, month, 1, sep="-"), "%Y-%m-%d")) %>% 
  summarize(total_cases = sum(dengue_cases, na.rm = TRUE),
            mean_temp = mean(mean_2m_air_temp_degree1, na.rm = TRUE), 
            .by = c(country, time)) %>% 
  arrange(country, time) %>%
  group_by(country) %>%
  filter(cumsum(total_cases) > 0) %>%
  ungroup() %>%
  mutate(country = if_else(country == "LKA1", "LKA", country)) %>% 
  # join with panel with full set of months to fill missing months with NAs 
  {full_join(., 
             summarise(., time = list(seq(min(time), max(time), by = "month")), .by = country) %>% 
               unnest_longer(time))} %>% 
  {ggplot(., aes(x = time, y = total_cases)) +
  geom_line() +
  facet_wrap(~ country, scales = "free", nrow = 6) +
  labs(y = "total cases",
       x = NULL) +  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) } %>% 
  ggsave(filename = "./figures/figureS3_dengue_timeseries.png", 
         width = 6*1.3, height = 8*1.3)

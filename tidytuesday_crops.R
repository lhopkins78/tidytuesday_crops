library(ggthemes)
library(tidyverse)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

key_crop_avgs <- key_crop_yields %>% 
  pivot_longer(cols=c(`Wheat (tonnes per hectare)`:`Bananas (tonnes per hectare)`),
               names_to="crop") %>%
  mutate(crop = gsub(" \\(.*","", crop)) %>%
  group_by(Year, crop) %>%
  summarise(avg_per_hectare=mean(value, na.rm=TRUE))

key_crop_top <- key_crop_yields %>%
  filter(Year==max(key_crop_yields$Year), !is.na(Code)) %>%
  pivot_longer(cols=c(`Wheat (tonnes per hectare)`:`Bananas (tonnes per hectare)`),
               names_to="crop") %>%
  mutate(crop = gsub(" \\(.*","", crop)) %>%
  group_by(crop) %>%
  top_n(1)

key_crop_trend <- key_crop_yields %>% 
  pivot_longer(cols=c(`Wheat (tonnes per hectare)`:`Bananas (tonnes per hectare)`),
               names_to="crop") %>%
  mutate(crop = gsub(" \\(.*","", crop)) %>%
  group_by(Entity, Year, crop) %>%
  summarise(avg_per_hectare=mean(value, na.rm=TRUE))

top_crop <- key_crop_trend %>% 
  inner_join(key_crop_top, by=c("Entity"="Entity", "crop"="crop"), suffix=c("", "match")) %>%
  mutate(Entity=gsub("United Arab Emirates", "UAE", Entity))

 ggplot(key_crop_trend, aes(Year, avg_per_hectare, col=Entity, group=Entity)) + 
  geom_line(alpha=0.3, col="grey") +
  geom_line(data=key_crop_avgs, 
            aes(Year, avg_per_hectare, group=crop ), col="black", size=1.5, alpha=0.9) +
  geom_line(data=top_crop, 
            aes(Year, avg_per_hectare, group=crop), col="#06c258", size=2, alpha=0.7) +
  geom_text(data=top_crop %>% filter(Year==2018), 
            aes(Year, avg_per_hectare, group=crop, label=Entity), 
            col="black", size=5, alpha=0.7, nudge_x = -15,
            family="American Typewriter") +
  facet_wrap(~crop, scales="free_y", nrow=3) +
  theme_few() +
  theme(legend.position="none",
        text=element_text(family="American Typewriter", size=15),
                          plot.title=element_text(size=40, face="bold")) +
  labs(x="Year", y="Tonnes per hectare", caption="Source: Our World in Data. Viz: @lauriejhopkins",
       title="Cream of the crop", subtitle="2018 crop yield leaders")
 
ggsave("crop.png", dpi="retina")

 
  
  
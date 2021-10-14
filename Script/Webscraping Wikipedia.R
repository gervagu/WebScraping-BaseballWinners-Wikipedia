#Web sraping data from the Baseball World Cup Tournament Wikipedia page

library(rvest)
library(tidyverse)
library(hrbrthemes)

#Select link and read it
wiki_link = "https://en.wikipedia.org/wiki/Baseball_World_Cup#Tournament_results" 
wiki_page = read_html(wiki_link)

#Create a table. Select all tables in the Wikipedia page. Then, select the specif table to scrape; in this case number 3
baseball = wiki_page %>% html_nodes("table") %>% .[3] %>%
#Select only the first element of the table
  html_table(fill = TRUE) %>% .[[1]]

#Remove total column
baseball <- baseball[-c(16),]

#Reshape data
baseball_reshaped <- baseball %>% 
gather (medal, value, Gold, Silver, Bronze, factor_key = T)


#Change levels: Order of importance of the medals: Gold, Silver, Bronze
baseball_reshaped$medal <- factor(baseball_reshaped$medal,
                            ordered = T,
                            levels = c("Gold", "Silver", "Bronze"))

#Order of the countries: number of Gold medals won
baseball_reshaped$Country <- factor(baseball_reshaped$Country,
                                 ordered = T,
                                 levels = c("Cuba", "United States", "Venezuela", "Colombia", "South Korea",
                                            "Puerto Rico", "Dominican Republic", "Great Britain", "Netherlands",
                                            "Nicaragua", "Mexico", "Japan", "Chinese Taipei", "Panama", "Canada"))

#Graph countries with most medals 
ggplot(baseball_reshaped, aes(
  x=medal, 
  y=value, 
  fill= medal, 
  label=value))+
  geom_bar(stat='identity')+
  geom_text(data=baseball_reshaped %>% 
              filter(value>=1), #don't display value 0
              aes (vjust=0.05, 
              fontface="bold"))+  
  facet_wrap(~Country,
             ncol=5, 
             nrow = 3)+
  labs(x=NULL, 
       y=NULL, 
       title="Countries with most medals at the Baseball World Cup (1938-2011)",
       caption="Gersán Vásquez Gutiérrez (@gervagu) | Source: Wikipedia")+
  scale_fill_manual(values = c("#fdcc0d",
                               "#D3D3D3",
                               "#a97142"))+
  theme_ipsum()+
  theme(
    legend.position="bottom",
    legend.title=element_blank(),
    legend.text = element_text(size=16),
    axis.ticks=element_blank(),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_blank(),
    plot.title=element_text(hjust = 0.5, size = 22, face="bold"),
    plot.title.position = "plot",
    plot.caption=element_text(size=12, margin=margin(t=12), color="#7a7d7e"),
    strip.text = element_text(hjust = 0.5, size = 15,vjust = 0.75,face="bold")) 

ggsave("baseballmedal.png",width=55,height=30,units='cm',dpi=300)

library(ggpubr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(maps)
library(readxl)
library(viridis)
library(RColorBrewer)
#####################################################################
#ALBANIA FIGURES
##GDP
GDPcapitaconstant<-filter(ALB_GDP, Indicator=="GDP per capita (constant 2010 US$)")
GDPconstant<-filter(ALB_GDP, Indicator=="GDP (constant 2010 US$)")
GDPcapitagrowth<-filter(ALB_GDP, Indicator=="GDP per capita growth (annual %)")
GDPgrowth<-filter(ALB_GDP, Indicator=="GDP growth (annual %)")

ggplot(GDPcapitaconstant, aes(x = Year, y = Value)) +
  geom_line(color="steelblue4", size=0.7) + 
  theme_stata() + 
  labs(title="Real GDP per capita (constant 2010 US$)", x ="Year", y = "Real GDP per capita in US$") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggplot(GDPconstant, aes(x = Year, y = Value/1000000)) +
  geom_line(color="steelblue4", size=0.7) + 
  theme_stata() + 
  labs(title="Real GDP (constant 2010 US$)", x ="Year", y = "Real GDP in million US$") + 
  scale_y_continuous(label=scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

ggplot(GDPcapitagrowth, aes(x = Year, y = Value)) +
  geom_line(color="steelblue4", size=0.7) + 
  theme_stata() +
  labs(title="GDP per capita growth (annual %)",x ="Year", y = "GDP per capita growth %") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_hline(yintercept = 0, linetype="dotted")

ggplot(GDPgrowth, aes(x = Year, y = Value)) +
  geom_line(color="steelblue4", size=0.7) + 
  theme_stata() + 
  labs(title="GDP growth (annual %)", x ="Year", y = "GDP growth %") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

## MAP
world_map <- map_data("world")

SEE <- c(
  "Albania", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Kosovo",
  "Montenegro", "Macedonia", "Serbia", "Slovenia") 

SEE.maps <- map_data("world", region = SEE)

Corruption_SEE <- read_excel("local file source", 
                             sheet = "sheet")

SEE.corrupt <- filter(Corruption_SEE, Year==2019)
SEE.corrupt <- SEE.corrupt %>%
  rename(region = Economy)

MAP <- left_join(SEE.corrupt, SEE.maps, by = "region")

SEE.corrupt2 <- filter(Corruption_SEE, Year==2013)
SEE.corrupt2 <- SEE.corrupt2 %>%
  rename(region = Economy)

MAP <- left_join(SEE.corrupt2, SEE.maps, by = "region")

region.lab.data <- SEE.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(MAPS2, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Bribery.Incidence), color = "white") +
  scale_fill_gradient2(low = "khaki1", mid = "orange", high = "red4", breaks=c(10,20,30),labels=c("10%","20%","30%")) +
  theme_void() +
  labs(title="Bribery Incidence in South Eastern Europe (2013)", caption="Note: The percent of firms experiencing at least one bribe payment request across 6 public transactions dealing with utilities access, permits, licenses, and taxes.", fill="Bribery Incidence") +
  with(region.lab.data, annotate(geom="text", x = long, y=lat, label = region, size = 2.5))

CPI <- c(35, 36, 38, 39, 36, 33, 31)
Year <- c(2019, 2018, 2017, 2016, 2015, 2014, 2013)
df_CPI <- data.frame(CPI, Year)

ggplot(df_CPI, aes(x=Year, y=CPI)) +
  geom_segment( aes(x=Year, xend=Year, y=0, yend=CPI), color="grey") +
  geom_point( color="steelblue4", size=4) +
  theme_stata() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("Year") +
  ylab("CPI")

df_CPI %>%
  tail(10) %>%
  ggplot(aes(x=Year, y=CPI)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="steelblue4", size=4) +
  scale_x_discrete(limits = c(2019, 2018, 2017, 2016, 2015, 2014, 2013)) + 
  scale_y_discrete(limits = c(32, 34, 36, 38)) +
  theme_stata() +
  ggtitle("Evolution of Corruption Perception Index in Albania")
##########################
library(foreign)
data2019 <- read.dta("local file source")

library(readstata13)
data2013 <- read.dta13("local data source")
data2009 <- read.dta13("local data source")
data2007 <- read.dta13("local data source")

m1a <- c("m1a")

m1a_2019 <- data2019[m1a]
m1a_2019 <- as.data.frame(m1a_2019)
m1a_df <- m1a_df %>% replace_with_na(replace = list(m1a = c("Don't know (spontaneous)", "Does not apply")))

m1a_df$m1a <- as.numeric(m1a_df$m1a)
m1a_df$m1a <- m1a_df$m1a-2
m1a_df <- m1a_df %>% replace_with_na(replace = list(m1a = c(0,-1)))

library(ggthemes)

ggplot(m1a_df, aes(x=m1a))+
  geom_histogram(stat="count", color="darkblue", fill="lightblue") +
  theme_stata() +
  theme(axis.text.x=element_text(color = "black", size=10, angle=45, vjust=.8, hjust=0.8),
        axis.title.x=element_blank())+
  ggtitle("Biggest Obstacle affecting Albanian Enterprises")+

prop2007 <- prop.table(table(m1a_2007))
prop2007 <- round(prop2007, digits=4)
prop2009 <- prop.table(table(m1a_2009))
prop2009 <- round(prop2009, digits=4)
prop2013 <- prop.table(table(m1a_2013))
prop2013 <- round(prop2013, digits=4)
prop2019 <- prop.table(table(m1a_2019))
prop2019 <- round(prop2019, digits=4)

var <- c('Access to finance', 'Access to land', 'Business licensing and permits', 'Corruption',
         'Courts', 'Crime, theft and disorder', 'Customs and trade regulations', 'Electricity',
         'Inadequately educatd workforce','Labor regulations', 'Political instability',
         'Practices of competitors in the informal sector', 'Tax administration','Tax rates',
         'Transport')

value2007 <- c(0.0724,0.023,0.0329,0.1184,0.0132,0.0099,0.0439,0.2961,0.0526,0.0066,0.0724,0.1382,0.0263,0.0592,0.0055)

value2009 <- c(0.0571,0.0229,0.0171,0.08,0.0057,0.0114,0.0400,0.3371,0.0514,0.0114,0.0743,0.1257,0.0171,0.0743,0.0171)

value2013 <- c(0.1194, 0.0556,0.0083,0.0500,0.0056,0.0139,0.0139,0.1278,0.0167,0.0028,0.0667,0.1833,0.1167,0.1056,0.0167)

value2019 <- c(0.0292, 0.0186,0.0159,0.0902,0.0239,0.0265,0.0398,0.0849,0.1114,0.0186,0.0822,0.1061,0.0318,0.2228,0.0769)

obstacle_df <- data.frame(var, value2007, value2009, value2013, value2019)
obstacle_dfm <- melt(obstacle_df[,c('var', 'value2007','value2009','value2013', 'value2019')],id.vars = 1)

library(wesanderson)

ggplot(obstacle_dfm, aes(x = var, y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity", position = "dodge") +
  scale_fill_manual(name="Year", breaks=c("value2007", "value2009", "value2013", "value2019"),
  labels=c("2007", "2009", "2013", "2019"),
  values=wes_palette(n=4, name="Darjeeling2")) +
  theme_stata() +
  theme(axis.text.x=element_text(color = "black", size=11, angle=45, vjust=1, hjust=1),
        axis.text=element_text(size=11),
        axis.title.y = element_text(size = 11)) +
  labs(title="Obstacles affecting Albanian Enterprises in different survey years",
     x ="", y = "Relative frequency")


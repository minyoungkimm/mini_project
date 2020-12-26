# 월별 데이터 전처리
# 미세먼지
pollution <- read.csv("data/pollution.csv")

names(pollution)[7] <- "미세먼지"
names(pollution)[8] <- "초미세먼지"
str(pollution)


date <- as.Date(as.character(pollution$측정일시),format="%Y%m%d")
month <- format(date,"%m")
pollution <- cbind(pollution,month)
pollution %>% 
  group_by(month) %>% 
  summarise(mean_dust = mean(미세먼지,na.rm=T)) -> pollution_month

pollution_month$month[1:9] <- 1:9

date <- as.Date(as.character(pollution$측정일시),format="%Y%m%d")
day <- format(date,"%d")
pollution <- cbind(pollution,day)
pollution %>% 
  group_by(day) %>% 
  summarise(mean_dust = mean(미세먼지,na.rm=T)) -> pollution_day

pollution_day$day[-1] <- 1:30
pollution_day <- pollution_day[-1]


# 태양광에너지
energy <- read.csv("data/energy.csv")
str(energy)
date <- energy$거래일
date <- as.Date(energy$거래일,format="%Y-%m-%d")
month <- format(date,"%m")
energy <- cbind(energy,month)
year <- format(date,"%Y")
energy <- cbind(energy,year)
energy %>% 
  filter(year=="2018" & 전력거래량 > 0) %>% 
  group_by(month) %>% 
  summarise(mean_energy = mean(전력거래량,na.rm=T)) -> energy_month

energy_month$month[1:9] <- 1:9


mdata <- merge(x = pollution_month, 
                      y = energy_month, 
                      by = 'month', 
                      all.x = TRUE)
mdata$month <- as.numeric(mdata$month)
mdata %>% 
  arrange(month) -> mdata

View(mdata)


# 박스 플롯
boxplot(mdata$mean_dust)
library(plotly)
box_plot <- plot_ly(y=mdata$mean_dust,type="box")
box_plot
box_plot <- plot_ly(y=mdata$mean_dust,type="box",boxpoints="all",jitter=0.3)
box_plot


ggplot(mdata) +
  theme_bw() +
  theme(line = element_blank()) +
  geom_boxplot(aes(x=1,y=mean_dust), alpha = 1, size = 0.75, width = 0.25) +
  geom_point(aes(x=1,y=mean_dust),alpha = 0.5, position = position_jitter(width = 0.1))




# 산점도
library(ggplot2)
ggplot(data=mdata,aes(x=mean_dust,y=mean_energy))+geom_point()


# 막대그래프
ggplot(data=mdata, aes(x=month,y=mean_energy)) + geom_bar(stat="identity")
ggplot(data=mdata, aes(x=month,y=mean_dust)) + geom_bar(stat="identity")


##### 구별 녹지비율/미세먼지

# 전국 오염도 지도 시각화
dust <- read.csv("data/오존.csv")
head(dust)


library(stringi)
library(devtools)
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
changeCode(korpop1)
library(dplyr)
korpop1 <- rename(korpop1, pop=총인구_명,name=행정구역별_읍면동)
install.packages("ggiraphExtra")
library(ggiraphExtra)
changeCode(kormap1)

pollution_map <- merge(x = changeCode(korpop1), 
                   y = dust, 
                   by = 'name', 
                   all.x = TRUE)

install.packages("mapproj")
library(mapproj)
ggChoropleth(data = pollution_map, aes(fill = 미세먼지, map_id = code),
             map = kormap1)
ggChoropleth(data = pollution_map, aes(fill = 초미세먼지, map_id = code),
             map = kormap1)
ggChoropleth(data = pollution_map, aes(fill = 오존, map_id = code),
             map = kormap1)
ggChoropleth(data = pollution_map, aes(fill = 오존농도, map_id = code),
             map = kormap1)
ggChoropleth(data = pollution_map, aes(fill = 이산화질소, map_id = code),
             map = kormap1)
ggChoropleth(data = pollution_map, aes(fill = 이산화질소농도, map_id = code),
             map = kormap1)


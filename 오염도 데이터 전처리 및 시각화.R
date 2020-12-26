library(ggplot2)
library(dplyr)
library(MASS)
library(treemap)
library(plotly)
library(gridExtra)
library(ggmap)
library(leaflet)

#### 오염도 데이터 전처리 #####
pollution <- read.csv("data/pollution.csv")
str(pollution)
table(pollution$측정소명)


# 25개의 구 단위만 추출
pollution <- pollution[grep("구$",pollution$측정소명),]
table(pollution$측정소명)


# 변수명 수정
names(pollution)[7] <- "미세먼지"
names(pollution)[8] <- "초미세먼지"
names(pollution)[2] <- "시군구명"


# 결측값 탐색
install.packages("VIM")
library(VIM)
aggr_plot <- aggr(pollution, col=c('gray','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(pollution), cex.axis=.7, gap=3, ylab=c("Histogram of missing data", "Pattern"))

install.packages("mice")
library(mice)
md.pattern(pollution)

boxplot(pollution$미세먼지)
boxplot(pollution$초미세먼지,
        main = "미세먼지",
        xlab = "㎍/m^3",
        ylab = "fine_dust",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)


# 결측값 처리(평균으로 대체)
mean(pollution$미세먼지,na.rm=T) #38.1
pollution$미세먼지 <- ifelse(is.na(pollution$미세먼지), 38.1, pollution$미세먼지) 
mean(pollution$초미세먼지,na.rm=T) #21.8
pollution$초미세먼지 <- ifelse(is.na(pollution$초미세먼지), 21.8, pollution$초미세먼지)
table(is.na(pollution$미세먼지))

mean(pollution$오존농도,na.rm=T) #0.02
pollution$오존농도 <- ifelse(is.na(pollution$오존농도), 0.02, pollution$오존농도) 
mean(pollution$이산화질소농도,na.rm=T) #0.028
pollution$이산화질소농도 <- ifelse(is.na(pollution$이산화질소농도), 0.028, pollution$이산화질소농도)
mean(pollution$이산화탄소농도,na.rm=T) #0.51
pollution$이산화탄소농도 <- ifelse(is.na(pollution$이산화탄소농도), 0.51, pollution$이산화탄소농도) 
mean(pollution$ 아황산가스,na.rm=T) #0.004
pollution$ 아황산가스 <- ifelse(is.na(pollution$ 아황산가스), 0.004, pollution$아황산가스)

missing <-table(is.na(pollution))

# 산점도
par(mfrow=c(1,2))
ggplot(data=pollution,aes(x=초미세먼지,y=오존농도)) +
  geom_point(colour='skyblue') +
  geom_smooth(method='lm')
ggplot(data=pollution,aes(x=미세먼지,y=오존농도)) +
  geom_point(colour='skyblue') +
  geom_smooth(method='lm')
ggplot(data=pollution,aes(x=초미세먼지,y=이산화탄소농도)) +
  geom_point(colour='skyblue') +
  geom_smooth(method='lm')
ggplot(data=pollution,aes(x=미세먼지,y=이산화탄소농도)) +
  geom_point(colour='skyblue') +
  geom_smooth(method='lm')
ggplot(data=pollution,aes(x=이산화질소농도,y=이산화탄소농도)) +
  geom_point(colour='skyblue') +
  geom_smooth(method='lm')

pairs(pollution[,c(3:8)], pch=16, col="blue")



# 일별 요약(오염도)

View(pollution)
date <- as.Date(as.character(pollution$측정일시),format="%Y%m%d")
day <- format(date,"%d")
pollution <- cbind(pollution,day)
pollution %>% 
  group_by(day) %>% 
  summarise(미세먼지_평균 = mean(미세먼지),
            초미세먼지_평균 = mean(초미세먼지),
            오존농도_평균 = mean(오존농도),
            이산화질소농도_평균 = mean(이산화질소농도),
            이산화탄소농도_평균 = mean(이산화탄소농도),
            아황산가스_평균 = mean(아황산가스),
            ) -> pollution_day

pollution_day$day[1:9] <- 1:9



energy <- read.csv("data/energy.csv")
table(is.na(energy)) # 결측값 없음
str(energy)
date <- energy$거래일
date <- as.Date(energy$거래일,format="%Y-%m-%d")
day <- format(date,"%d")
energy <- cbind(energy,day)
year <- format(date,"%Y")
energy <- cbind(energy,year)
energy %>% 
  filter(year=="2018" & 전력거래량 > 0) %>% 
  group_by(day) %>% 
  summarise(mean_energy = mean(전력거래량)) -> energy_day

energy_day$day[1:9] <- 1:9
energy_day$day <- as.numeric(energy_day$day)


ddata <- merge(x = pollution_day, 
               y = energy_day, 
               by = 'day', 
               all.x = TRUE)





hist(pollution_day$미세먼지_평균)
hist(pollution_day$초미세먼지_평균)
hist(pollution_day$오존농도_평균)
hist(pollution_day$이산화질소농도_평균)
hist(pollution_day$이산화탄소농도_평균)
hist(pollution_day$아황산가스_평균)

hist(pollution_day$미세먼지_평균, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "미세먼지",
     main = "미세먼지 농도 분포") +
lines(density(pollution_day$미세먼지_평균), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")


# 월별 요약
summary(is.na(pollution))
View(pollution)
date <- as.Date(as.character(pollution$측정일시),format="%Y%m%d")
month <- format(date,"%m")
pollution <- cbind(pollution,month)
pollution %>% 
  group_by(month) %>% 
  summarise(미세먼지_합 = sum(미세먼지),
                   초미세먼지_합 = sum(초미세먼지),
                   오존농도_합 = sum(오존농도),
                   이산화질소농도_합 = sum(이산화질소농도),
                   이산화탄소농도_합 = sum(이산화탄소농도),
                   아황산가스_합 = sum(아황산가스),
  ) -> pollution_month

pollution_month$month[1:9] <- 1:9

# 산점도
pairs(pollution_month[,c(2:7)], pch=16, col="blue")

boxplot(pollution_month$미세먼지_평균)
boxplot(pollution_month$미세먼지_평균,
        main = "미세먼지",
        xlab = "농도",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
boxplot(pollution_month$초미세먼지_평균,
        main = "미세먼지",
        xlab = "농도",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
boxplot(pollution_month$오존농도_평균,
        main = "미세먼지",
        xlab = "농도",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
boxplot(pollution_month$이산화질소농도_평균,
        main = "미세먼지",
        xlab = "농도",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
boxplot(pollution_month$이산화탄소농도_평균,
        main = "미세먼지",
        xlab = "농도",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
boxplot(pollution_month$아황산가스_평균,
        main = "미세먼지",
        xlab = "농도",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)
# 월별 평균내기 전
par(mfrow = c(1,4))
boxplot(pollution$미세먼지)
boxplot(pollution$미세먼지,
        main = "미세먼지",
        xlab = "농도",
        col = "orange",
        border = "brown",
        #horizontal = TRUE,
        notch = FALSE
)
boxplot(pollution$오존농도,
        main = "오존",
        xlab = "농도",
        col = "orange",
        border = "brown",
        #horizontal = TRUE,
        notch = FALSE
)

boxplot(pollution$이산화탄소농도,
        main = "이산화탄소",
        xlab = "농도",
        col = "orange",
        border = "brown",
        #horizontal = TRUE,
        notch = FALSE
)
boxplot(pollution$이산화질소농도,
        main = "이산화질소",
        xlab = "농도",
        col = "orange",
        border = "brown",
        #horizontal = TRUE,
        notch = FALSE
)
hist(pollution$미세먼지)
#hist(pollution$초미세먼지)
#hist(pollution$오존농도)
hist(pollution$이산화질소농도)
hist(pollution$이산화탄소농도)
#hist(pollution$아황산가스)

### 월별 박스플랏 ###
par(mfrow=c(1,1))
plot_ly(pollution,y=~미세먼지,x=~month,color=~month,type="box")
#plot_ly(pollution,y=~초미세먼지,x=~month,color=~month,type="box")
#plot_ly(pollution,y=~오존농도,x=~month,color=~month,type="box")
plot_ly(pollution,y=~이산화질소농도,x=~month,color=~month,type="box")
plot_ly(pollution,y=~이산화탄소농도,x=~month,color=~month,type="box")
#plot_ly(pollution,y=~아황산가스,x=~month,color=~month,type="box")





energy <- read.csv("data/energy.csv")
summary(is.na(energy)) # 결측값 없음
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

## 월별 오염도,에너지 합치기
mdata <- merge(x = pollution_month, 
               y = energy_month, 
               by = 'month', 
               all.x = TRUE)



# 구별로 요약
pollution_gu <- pollution %>% 
  group_by(측정소명) %>% 
  summarise(미세먼지 = sum(미세먼지),초미세먼지 = sum(초미세먼지),아황산가스 = sum(아황산가스),
                이산화탄소 = sum(이산화탄소농도),이산화질소 = sum(이산화질소농도),오존 = sum(오존농도))
View(pollution_gu)
pairs(pollution_gu[,c(2:7)], pch=16, col="blue")

#write.csv(pollution_gu,file="data/pollution_gu.csv")



write.csv(green,file="green.csv")

## 구별 미세먼지 지도 시각화
install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)


id <- read.csv("data/seoul_id.csv", header = TRUE) #시각화할 데이터셋
map <- readShapePoly("data/TL_SCCO_SIG.shp") #지리 정보 데이터셋

pollution_id <- merge(x = pollution_gu, 
                                 y = id, 
                                 by = '시군구명', 
                                 all.x = TRUE)

map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
P_merge <- merge(seoul_map, pollution_id, by='id')
View(P_merge)
str(P_merge)


#ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')
pplot <- ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = fine_dust))
#ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = ultrafine_dust))

pp <- pplot + scale_fill_gradient(low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") 
+ theme_bw() + labs(title = "서울시 구별 미세먼지") 
+ theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
library(plotly)
ggplotly(pp)

# 녹지면적 시각화
green <- read.csv("data/green.csv", header = TRUE)
View(green)
green <- green[-1,]
names(green)[3] <- "시군구명"
str(green)
table(green$시군구명)
green <- green[,3:5]
green$합계1 <- as.integer(green$합계1)
green$합계2 <- gsub(",","",green$합계2)
green$합계2 <- as.numeric(green$합계2)
green_id <- merge(x = green, 
                      y = id, 
                      by = '시군구명', 
                      all.x = TRUE)

G_merge <- merge(seoul_map, green_id, by='id')
View(G_merge)


ggplot() + geom_polygon(data = G_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')
plot <- ggplot() + geom_polygon(data = G_merge, aes(x=long, y=lat, group=group, fill = 합계2))
#ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = ultrafine_dust))

p <- plot + scale_fill_gradient(low = "#ecf9f2", high = "#26734d", space = "Lab", guide = "colourbar") 
+ theme_bw() + labs(title = "서울시 구별 미세먼지") 
+ theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
library(plotly)
ggplotly(p)



# 데이터 통합
energy_month <- read.csv("data/energy_month.csv", header = TRUE)
str(energy_month)
pollution_month <- read.csv("data/pollution_month.csv", header = TRUE)
str(pollution_month)
pollution_month$월 <- as.numeric(gsub("[ㄱ-힣]","",pollution_month$월))
pollution_id <- merge(x = energy_month, 
                      y = pollution_month, 
                      by = '월', 
                      all.x = TRUE)
View(pollution_id)





install.packages("PerformanceAnalytics")
install.packages("corrplot")

library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(plotly)

df <- read.csv("D:/다운로드/미니프로젝트관련/de.csv")
df1 <- read.csv("D:/다운로드/미니프로젝트관련/de1.csv")
df2 <- read.csv("D:/다운로드/미니프로젝트관련/태양광.csv")
df3 <- read.csv("D:/다운로드/미니프로젝트관련/미세먼지.csv")
df4 <- read.csv("D:/다운로드/미니프로젝트관련/이산화탄소.csv")
df5 <- read.csv("D:/다운로드/미니프로젝트관련/그외.csv")
df6 <- read.csv("D:/다운로드/미니프로젝트관련/전국_미세먼지_PM10.csv")

#상관관계
chart.Correlation(df, histogram=TRUE, pch=19)
df.cor <- cor(df)
corrplot(df.cor, method="number")


#선그래프 출력
ggplot(df2, aes(x=측정월, y=수치)) + geom_line(size=2, aes(group=종류,colour=종류))
ggplot(df3, aes(x=측정월, y=수치)) + geom_line(size=2, aes(group=종류,colour=종류))
ggplot(df4, aes(x=측정월, y=수치)) + geom_line(size=2, aes(group=종류,colour=종류))
ggplot(df5, aes(x=측정월, y=수치)) + geom_line(size=2, aes(group=종류,colour=종류))
ggplot(df6, aes(x=측정월, y=수치)) + geom_line(size=2, aes(group=지역,colour=지역))
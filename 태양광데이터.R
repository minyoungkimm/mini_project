library(ggplot2)
library(dplyr)
library(MASS)
library(treemap)
library(plotly)
library(gridExtra)
library(ggmap)
library(leaflet)


energy <- read.csv("data/energy.csv")
date <- as.Date(energy$거래일,format="%Y-%m-%d")
month <- format(date,"%m")
energy <- cbind(energy,month)
year <- format(date,"%Y")
energy <- cbind(energy,year)
energy %>% 
  filter(year=="2018" & 전력거래량 > 0) -> energy2


boxplot(energy2$전력거래량,
        main = "태양광에너지",
        xlab = "거래량",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

# 월별 박스플랏
plot_ly(energy2,y=~전력거래량,x=~month,color=~month,type="box")






# 일별 요약
energy <- read.csv("data/energy.csv")
summary(is.na(energy)) # 결측값 없음
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
  summarise(mean_energy = sum(전력거래량)) -> energy_day

energy_day$day[1:9] <- 1:9
energy_day$day <- as.numeric(energy_day$day)

boxplot(energy_day$mean_energy)
boxplot(energy_day$mean_energy,
        main = "태양광 에너지",
        xlab = "전력양",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)



# 월별 요약
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
  summarise(sum_energy = sum(전력거래량)) -> energy_month

energy_month$month[1:9] <- 1:9



boxplot(energy_month$mean_energy)
boxplot(energy_month$mean_energy,
        main = "태양광 에너지",
        ylab = "전력양",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = FALSE
)

## 오염도 태양광 데이터 결합(월별)
mdata <- merge(x = pollution_month, 
               y = energy_month, 
               by = 'month', 
               all.x = TRUE)
# 산점도
ggplot(data=mdata,aes(x=미세먼지_합,y=오존농도_합)) +
  geom_point(colour='red') +
  geom_smooth(method='lm')



### 회귀분석 ###
mdata.lm <- lm(sum_energy ~ 미세먼지_합 + 초미세먼지_합 + 오존농도_합 + 이산화질소농도_합 + 이산화탄소농도_합 + 아황산가스_합, data=mdata)
summary(mdata.lm)


attach(mdata)
# 표준화
st_mdata <- cbind(mdata, st_미세먼지=(미세먼지_합-mean(미세먼지_합))/sd(미세먼지_합), 
                  st_초미세먼지=(초미세먼지_합-mean(초미세먼지_합))/sd(초미세먼지_합),
                  st_이산화질소=(이산화질소농도_합-mean(이산화질소농도_합))/sd(이산화질소농도_합),
                  st_이산화탄소=(이산화탄소농도_합-mean(이산화탄소농도_합))/sd(이산화탄소농도_합),
                  st_아황산가스=(아황산가스_합-mean(아황산가스_합))/sd(아황산가스_합),
                  st_오존=(오존농도_합-mean(오존농도_합))/sd(오존농도_합),
                  st_태양광=(sum_energy-mean(sum_energy))/sd(sum_energy))
attach(st_fdata)
st_data.lm <- lm(st_태양광 ~ st_미세먼지+st_초미세먼지+st_이산화질소+st_이산화탄소+st_아황산가스+st_오존, data=st_mdata)  
#plot(st_age, st_cost, xlab="사용연도", ylab="정비비용", pch=19, col="blue", cex.lab=1.5)
#title("변수 표준화 후의 사용연도와 정비비용", cex.main=2, col.main="red") 
#abline(st_factory.lm, col="red")
summary(st_data.lm)
detach(st_fdata)
# 일변량 회귀
st_data.lm1 <- lm(st_태양광 ~ st_이산화탄소+st_오존,data=st_mdata) 
summary(st_data.lm1)




st_mdata1 <- st_mdata[,9:15]
start.lm <- lm(st_태양광~1, data=st_mdata1) # 가장 간단한 모델
full.lm <- lm(st_태양광~., data=st_mdata1) # 풀 모델

#forward
step(start.lm, scope=list(lower=start.lm, upper=full.lm), direction="forward")
#backward
step(full.lm, data=tadata, direction="backward")
#stepwise
step(start.lm, scope=list(upper=full.lm), data=tadata, direction="both")

# 표준화 안하고
mdata1 <- mdata[,-1] 
start.lm <- lm(sum_energy~1, data=mdata1) # 가장 간단한 모델
full.lm <- lm(sum_energy~., data=mdata1) # 풀 모델

#forward
step(start.lm, scope=list(lower=start.lm, upper=full.lm), direction="forward")
#backward
step(full.lm, data=tadata, direction="backward")
#stepwise
step(start.lm, scope=list(upper=full.lm), data=tadata, direction="both")

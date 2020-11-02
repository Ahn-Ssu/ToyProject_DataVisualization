#최종

setwd("C:/Users/LeeChanhyo/Desktop/Campus/90 - data_visualize/")

library(ggplot2)
library(dplyr) 
library(tidyverse) #dplyr패키지와 충돌하므로, tidyverse패키지는는 stats::filter(), stats::lag()로 사용
library(lubridate)
library(readxl)
library(plotly)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

seoul_floating_jan <- read.csv("floating_population/fpopl_202001.csv") %>% as_tibble()
seoul_floating_feb <- read.csv("floating_population/fpopl_202002.csv") %>% as_tibble()
seoul_floating_mar <- read.csv("floating_population/fpopl_202003.csv") %>% as_tibble()
seoul_floating_apr <- read.csv("floating_population/fpopl_202004.csv") %>% as_tibble()
seoul_floating_may <- read.csv("floating_population/fpopl_202005.csv") %>% as_tibble()
seoul_floating_jun <- read.csv("floating_population/fpopl_seoul_06.csv") %>% as_tibble()

seoul_floating_jan <- seoul_floating_jan %>% dplyr::select(base_ymd, adstrd_code, popltn_cascnt)
seoul_floating_feb <- seoul_floating_feb %>% dplyr::select(base_ymd, adstrd_code, popltn_cascnt)
seoul_floating_mar <- seoul_floating_mar %>% dplyr::select(base_ymd, adstrd_code, popltn_cascnt)
seoul_floating_apr <- seoul_floating_apr %>% dplyr::select(base_ymd, adstrd_code, popltn_cascnt)
seoul_floating_may <- seoul_floating_may %>% dplyr::select(base_ymd, adstrd_code, popltn_cascnt)
seoul_floating_jun <- seoul_floating_jun %>% dplyr::select(base_ymd, adstrd_code, popltn_cascnt)


sum_seoul_floating_jan <- seoul_floating_jan %>% group_by(base_ymd, adstrd_code) %>% 
  summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
  mutate(adstrd_code_more = adstrd_code %/% 1000) %>% 
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))

sum_seoul_floating_jan <- sum_seoul_floating_jan%>%
  mutate(base_ymd=(ymd(base_ymd))) 

sum_seoul_floating_feb <- seoul_floating_feb %>% group_by(base_ymd, adstrd_code) %>% 
  summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
  mutate(adstrd_code_more = adstrd_code %/% 1000) %>% 
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))

sum_seoul_floating_feb <- sum_seoul_floating_feb%>%
  mutate(base_ymd=(ymd(base_ymd))) 

sum_seoul_floating_mar <- seoul_floating_mar %>% group_by(base_ymd, adstrd_code) %>% 
  summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
  mutate(adstrd_code_more = adstrd_code %/% 1000) %>% 
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))

sum_seoul_floating_mar <- sum_seoul_floating_mar%>%
  mutate(base_ymd=(ymd(base_ymd))) 

sum_seoul_floating_apr <- seoul_floating_apr %>% group_by(base_ymd, adstrd_code) %>% 
  summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
  mutate(adstrd_code_more = adstrd_code %/% 1000) %>% 
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))

sum_seoul_floating_apr <- sum_seoul_floating_apr%>%
  mutate(base_ymd=(ymd(base_ymd))) 

sum_seoul_floating_may <- seoul_floating_may %>% group_by(base_ymd, adstrd_code) %>% 
  summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
  mutate(adstrd_code_more = adstrd_code %/% 1000) %>% 
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))

sum_seoul_floating_may <- sum_seoul_floating_may%>%
  mutate(base_ymd=(ymd(base_ymd))) 

sum_seoul_floating_jun <- seoul_floating_jun %>% group_by(base_ymd, adstrd_code) %>% 
  summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
  mutate(adstrd_code_more = adstrd_code %/% 1000) %>% 
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))

sum_seoul_floating_jun <- sum_seoul_floating_jun%>%
  mutate(base_ymd=(ymd(base_ymd))) 

seoul_floating_src <- read.csv("category_data/SeoulFloating.csv") %>% as_tibble() #추가 데이터로있는 Seoul의 유동인구 파일
h_location <- read.csv("other_datas/GULocation.csv", fileEncoding = "euc-kr") %>% as_tibble() #지역-영문-위도-경도 매핑되어있는 파일
accum_patient_src <- read.csv("category_data/PatientInfo.csv") %>% as_tibble() #코로나 환자정보 파일
accum_patient <- filter(accum_patient_src, ymd(confirmed_date)<= ymd("2020-06-30")) #해당 기간의 환자 정보만 취합

accum_patient_byweek <- accum_patient %>% 
  mutate(confirmed_date=week(ymd(confirmed_date)))

gu_accum_patient <- accum_patient_byweek %>%
  filter(province=="Seoul") %>%
  group_by(city) %>%
  summarise(new_patient=n())

seoul_pop <- read_xlsx("other_datas/Seoul_population.xlsx") %>% as_tibble() #서울 자치구별 인구 수 파일
region <- read.csv("category_data/Region.csv") %>% as_tibble() #각 자치구별 지역 정보 파일
region <- filter(region, code<11000) #서울 지역 정보만 취합
popl_density <-read.table(file="other_datas/seoul_density.txt",
                          header=TRUE,
                          sep ="\t",
                          fileEncoding = "utf-8")
popl_density <- rename(popl_density, "시군구명_한글"="지역")
popl_density <- merge(popl_density, h_location, by="시군구명_한글")
popl_density$밀도 <- as.numeric(gsub(",", "", popl_density$인구밀도.명...))

seoul_floating <- seoul_floating_src %>%
  mutate(date_form=(ymd(date))) %>% #문자열 형태의 날짜를 Date 데이터 타입으로 변환
  dplyr::select(date_form, city, fp_num) #필요한 것은 날짜, 자치구, 유동인구수만 필요함

seoul_floating <- seoul_floating %>% #도시, 날짜별로 유동인구 합침(성별-시간별로 나누어져있는 데이터)
  group_by(date_form, city) %>%
  summarise(Sum_fpNum = sum(fp_num)) %>%
  mutate(Sum_fpNum = Sum_fpNum) %>%
  mutate(city = city)

seoul_floating <- replace(seoul_floating, list=c(2), values=c(replace(seoul_floating$city, seoul_floating$city=="Dongjag-gu", "Dongjak-gu"))) #영어표현이 다른 도시명 수정
#위도, 경도 표현으로 Euclidean 거리 계산
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

#전처리 마친 데이터셋
final_dataset <- data.frame(id=c(), city=c(), latitude=c(), longitude=c(), 
                            distance_ITAEWON=c(), distance_GURO=c(), 
                            total_population=c(), city_type=c(), 
                            floating_popl_1=c(), floating_popl_2=c(), floating_popl_3=c(), floating_popl_4=c(), floating_popl_5=c(), floating_popl_6=c(),
                            floating_popl_1_weekday=c(), floating_popl_2_weekday=c(), floating_popl_3_weekday=c(), floating_popl_4_weekday=c(), floating_popl_5_weekday=c(), floating_popl_6_weekday=c(),
                            floating_popl_1_weekend=c(), floating_popl_2_weekend=c(), floating_popl_3_weekend=c(), floating_popl_4_weekend=c(), floating_popl_5_weekend=c(), floating_popl_6_weekend=c())

#지역별로 필요한 데이터 취합 후 저장
for(i in 1:nrow(h_location)){
  print(i)
  r_id = h_location[i,"시군구코드"] %>% as.numeric()
  r_city = h_location[i, "시군구명_영문"] %>% as.character()
  r_city_k = h_location[i,"시군구명_한글"] %>% as.character()
  r_latitude = h_location[i, "위도"] %>% as.double()
  r_longitude = h_location[i, "경도"] %>% as.double()
  yongsan_lat = filter(h_location, 시군구명_영문=="Yongsan-gu")[1, "위도"] %>% as.double()
  yongsan_lon = filter(h_location, 시군구명_영문=="Yongsan-gu")[1, "경도"] %>% as.double()
  guro_lat = filter(h_location, 시군구명_영문=="Guro-gu")[1, "위도"] %>% as.double()
  guro_lon = filter(h_location, 시군구명_영문=="Guro-gu")[1, "경도"] %>% as.double()
  r_distance_ITAEWON = earth.dist(yongsan_lon, yongsan_lat, r_longitude, r_latitude) %>% as.double()
  r_distance_GURO = earth.dist(guro_lon, guro_lat, r_longitude, r_latitude) %>% as.double()
  r_total_population = (filter(seoul_pop, 자치구==r_city_k))[1, "인구...4"] %>% as.integer()
  temp_total_school = ((filter(region, city==r_city))[1, "elementary_school_count"] %>% as.integer())
  #+((filter(region, city==r_city))[1, "kindergarten_count"]%>%as.integer())
  r_school_ratio = (temp_total_school%>%as.double()/r_total_population%>%as.double())*100000 #십만명당 학교수
  table_floating_popl_1 <- (filter(sum_seoul_floating_jan, adstrd_code_more==r_id))
  table_floating_popl_2 <- (filter(sum_seoul_floating_feb, adstrd_code_more==r_id))
  table_floating_popl_3 <- (filter(sum_seoul_floating_mar, adstrd_code_more==r_id))
  table_floating_popl_4 <- (filter(sum_seoul_floating_apr, adstrd_code_more==r_id))
  table_floating_popl_5 <- (filter(sum_seoul_floating_may, adstrd_code_more==r_id))
  table_floating_popl_6 <- (filter(sum_seoul_floating_jun, adstrd_code_more==r_id))
  
  r_floating_popl_1 = sum(table_floating_popl_1$sum_popltn_cascnt_01)
  r_floating_popl_2 = sum(table_floating_popl_2$sum_popltn_cascnt_01)
  r_floating_popl_3 = sum(table_floating_popl_3$sum_popltn_cascnt_01)
  r_floating_popl_4 = sum(table_floating_popl_4$sum_popltn_cascnt_01)
  r_floating_popl_5 = sum(table_floating_popl_5$sum_popltn_cascnt_01)
  r_floating_popl_6 = sum(table_floating_popl_6$sum_popltn_cascnt_01)
  
  r_floating_popl_1_weekday = sum(filter(table_floating_popl_1, weekdays(base_ymd)!="토요일"&&weekdays(base_ymd)!="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_2_weekday = sum(filter(table_floating_popl_2, weekdays(base_ymd)!="토요일"&&weekdays(base_ymd)!="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_3_weekday = sum(filter(table_floating_popl_3, weekdays(base_ymd)!="토요일"&&weekdays(base_ymd)!="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_4_weekday = sum(filter(table_floating_popl_4, weekdays(base_ymd)!="토요일"&&weekdays(base_ymd)!="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_5_weekday = sum(filter(table_floating_popl_5, weekdays(base_ymd)!="토요일"&&weekdays(base_ymd)!="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_6_weekday = sum(filter(table_floating_popl_5, weekdays(base_ymd)!="토요일"&&weekdays(base_ymd)!="일요일")$sum_popltn_cascnt_01)
  
  r_floating_popl_1_weekend = sum(filter(table_floating_popl_1, weekdays(base_ymd)=="토요일"||weekdays(base_ymd)=="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_2_weekend = sum(filter(table_floating_popl_2, weekdays(base_ymd)=="토요일"||weekdays(base_ymd)=="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_3_weekend = sum(filter(table_floating_popl_3, weekdays(base_ymd)=="토요일"||weekdays(base_ymd)=="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_4_weekend = sum(filter(table_floating_popl_4, weekdays(base_ymd)=="토요일"||weekdays(base_ymd)=="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_5_weekend = sum(filter(table_floating_popl_5, weekdays(base_ymd)=="토요일"||weekdays(base_ymd)=="일요일")$sum_popltn_cascnt_01)
  r_floating_popl_6_weekend = sum(filter(table_floating_popl_5, weekdays(base_ymd)=="토요일"||weekdays(base_ymd)=="일요일")$sum_popltn_cascnt_01)
  
  
  
  row_data <- data.frame(id=c(r_id), city=c(r_city), latitude=c(r_latitude), longitude=c(r_longitude), 
                         distance_ITAEWON=c(r_distance_ITAEWON), distance_GURO=c(r_distance_GURO),
                         total_population=c(r_total_population), school_ratio=c(r_school_ratio), 
                         floating_popl_1=c(r_floating_popl_1), floating_popl_2=c(r_floating_popl_2), 
                         floating_popl_3=c(r_floating_popl_3), floating_popl_4=c(r_floating_popl_4), 
                         floating_popl_5=c(r_floating_popl_5), floating_popl_6=c(r_floating_popl_6),
                         floating_popl_1_weekday=c(r_floating_popl_1_weekday), floating_popl_2_weekday=c(r_floating_popl_2_weekday), 
                         floating_popl_3_weekday=c(r_floating_popl_3_weekday), floating_popl_4_weekday=c(r_floating_popl_4_weekday), 
                         floating_popl_5_weekday=c(r_floating_popl_5_weekday), floating_popl_6_weekday=c(r_floating_popl_6_weekday),
                         floating_popl_1_weekend=c(r_floating_popl_1_weekend), floating_popl_2_weekend=c(r_floating_popl_2_weekend), 
                         floating_popl_3_weekend=c(r_floating_popl_3_weekend), floating_popl_4_weekend=c(r_floating_popl_4_weekend),
                         floating_popl_5_weekend=c(r_floating_popl_5_weekend), floating_popl_6_weekend=c(r_floating_popl_6_weekend))
  
  final_dataset <- rbind(final_dataset, row_data)
}

byweek_sum_seoul_floating_jan <- sum_seoul_floating_jan %>% 
  mutate(base_ymd=week(base_ymd)) %>%
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_byweek = sum(sum_popltn_cascnt_01))

byweek_sum_seoul_floating_feb <- sum_seoul_floating_feb %>% 
  mutate(base_ymd=week(base_ymd)) %>%
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_byweek = sum(sum_popltn_cascnt_01))

byweek_sum_seoul_floating_mar <- sum_seoul_floating_mar %>% 
  mutate(base_ymd=week(base_ymd)) %>%
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_byweek = sum(sum_popltn_cascnt_01))

byweek_sum_seoul_floating_apr <- sum_seoul_floating_apr %>% 
  mutate(base_ymd=week(base_ymd)) %>%
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_byweek = sum(sum_popltn_cascnt_01))

byweek_sum_seoul_floating_may <- sum_seoul_floating_may %>% 
  mutate(base_ymd=week(base_ymd)) %>%
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_byweek = sum(sum_popltn_cascnt_01))

byweek_sum_seoul_floating_jun <- sum_seoul_floating_jun %>% 
  mutate(base_ymd=week(base_ymd)) %>%
  group_by(base_ymd, adstrd_code_more) %>% 
  summarise(sum_popltn_byweek = sum(sum_popltn_cascnt_01))

byweek_sum_seoul_floating_all <- rbind(rbind(rbind(rbind(rbind(byweek_sum_seoul_floating_jan, byweek_sum_seoul_floating_feb), byweek_sum_seoul_floating_mar),byweek_sum_seoul_floating_apr), byweek_sum_seoul_floating_may), byweek_sum_seoul_floating_jun)
byweek_sum_seoul_floating_all <- byweek_sum_seoul_floating_all %>% group_by(base_ymd) %>% summarise(sum_popltn_byweek = sum(sum_popltn_byweek))
sum_accum_patient <- accum_patient_byweek %>% group_by(confirmed_date) %>% summarise(n=n())
byweek_sum_seoul_floating_all <- byweek_sum_seoul_floating_all %>% filter(base_ymd>=3)

cov(x=sum_accum_patient$confirmed_date, y=byweek_sum_seoul_floating_all$sum_popltn_byweek, use="complete.obs", method=c("pearson")) #피어슨 공분산
cor(x=sum_accum_patient$confirmed_date, y=byweek_sum_seoul_floating_all$sum_popltn_byweek, use="complete.obs", method=c("pearson")) #피어슨 상관관계계수

byweek_sum_seoul_floating_all <- byweek_sum_seoul_floating_all %>% rename("confirmed_date"="base_ymd")
relation_df <- left_join(sum_accum_patient, byweek_sum_seoul_floating_all, by="confirmed_date")

cov(x=relation_df$n, y=relation_df$sum_popltn_byweek, use="complete.obs", method=c("pearson"))
cor(x=relation_df$n, y=relation_df$sum_popltn_byweek, use="complete.obs", method=c("pearson"))

out2 <- lm(n ~ sum_popltn_byweek , data=relation_df)
summary(out2)

plot_relation2 <- ggplot(relation_df, aes(x=n, y=sum_popltn_byweek)) +
  geom_point(color='gray75') + 
  theme_bw() + labs(title = "확진자 수-유동인구 상관관계도") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) + 
  stat_smooth(method = 'lm', se=F, color='red') + 
  geom_text(x=200, y=3.8e+08, label="y=-4.188e-06x - 1.766e+03") + 
  geom_text(x=600, y=3.5e+08, label="R²=0.199")
ggplotly(plot_relation2)

cov(x=relation_df$sum_popltn_byweek, y=relation_df$n, use="complete.obs", method=c("pearson"))
cor(x=relation_df$sum_popltn_byweek, y=relation_df$n, use="complete.obs", method=c("pearson"))

out2_inverse <- lm(sum_popltn_byweek ~ n , data=relation_df)
summary(out2_inverse)

plot_relation2_inverse <- ggplot(relation_df, aes(x=sum_popltn_byweek, y=n)) +
  geom_point(color='gray75') + 
  theme_bw() + labs(title = "유동인구-확진자 수 상관관계도") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  stat_smooth(method = 'lm', se=F, color='red') + 
  geom_text(x=3.8e+08, y=350, label="y=-47511x + 380467309") + 
  geom_text(x=3.5e+08, y=600, label="R²=0.1999")
ggplotly(plot_relation2_inverse)

#도시-거리 그래프
graph_distance_ITAEWON <- ggplot(data=final_dataset, aes(x=city, y=distance_ITAEWON, fill=distance_ITAEWON)) +
  geom_bar(stat="identity", position="dodge") + scale_fill_continuous(guide=FALSE, type = "viridis") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
graph_distance_GURO <- ggplot(data=final_dataset, aes(x=city, y=distance_GURO, fill=distance_GURO)) +
  geom_bar(stat="identity", position="dodge") + scale_fill_continuous(guide=FALSE, type = "viridis") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplotly(graph_distance_ITAEWON)
ggplotly(graph_distance_GURO)

map <- shapefile("other_datas/SIG_202005/SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
map <- gBuffer(map, byid = TRUE, width=0)
new_map <- fortify(map, region='SIG_CD')
#----------------------------------------
new_map$id <- as.numeric(new_map$id) #맵 아이디를 숫자형으로 변환
seoul_map <- new_map[new_map$id <= 11740,] #맵에서 코드 11740 이하의 서울 지역만 추출

map_data_merge <- merge(seoul_map, final_dataset, by='id') #id를 기준으로 seoul_map 데이터프레임과 exercise_data 병합
plot <- ggplot() + geom_polygon(data = map_data_merge, aes(x=long, y=lat, group=group, fill = floating_popl_3_weekday/floating_popl_2_weekday)) #Sum_fpNum을 바탕으로 채우기
plot <- plot + scale_fill_gradient("변화율", low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") + theme_bw() + labs(title = "서울시 2~3월 평일 유동인구 변화율") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
ggplotly(plot)

plot_decrease <- ggplot() + geom_polygon(data = map_data_merge, aes(x=long, y=lat, group=group, fill = (floating_popl_3_weekday-floating_popl_2_weekday)/floating_popl_2_weekday)) #Sum_fpNum을 바탕으로 채우기
plot_decrease <- plot_decrease + scale_fill_gradient("증감률", low = "#ffe5e5", high = "#ff3232", space = "Lab", guide = "colourbar") + theme_bw() + labs(title = "서울시 2~3월 평일 유동인구 증감율") + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
ggplotly(plot_decrease)

plot_weekend_decrease <- ggplot() +
  geom_polygon(data = map_data_merge, aes(x=long, y=lat, group=group, fill = (floating_popl_3_weekend-floating_popl_2_weekend)/floating_popl_2_weekend)) +
  scale_fill_continuous("증감률", type="viridis", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "서울시 2~3월 주말 유동인구 증감율") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data=h_location, aes(x=경도, y=위도, label=paste(시군구명_한글), color="white"))
ggplotly(plot_weekend_decrease)

plot_weekend_decrease_1 <- ggplot() +
  geom_polygon(data = map_data_merge, aes(x=long, y=lat, group=group, fill = (floating_popl_3_weekday-floating_popl_1_weekday)/floating_popl_1_weekday)) +
  scale_fill_continuous("증감률", type="viridis", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "서울시 1월 대비 3월 평일 유동인구 증감율 및 인구밀도") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data=h_location, aes(x=경도, y=위도, label=paste(시군구명_한글))) +
  geom_text(data=popl_density, aes(x=경도, y=위도, label=paste("", (밀도%>%as.integer())/1000, sep="\n\n")))
ggplotly(plot_weekend_decrease_1)


density_merge <- merge(seoul_map, rename(popl_density, "id"="시군구코드"), by='id') #id를 기준으로 seoul_map 데이터프레임과 exercise_data 병합
plot_density <- ggplot() +
  geom_polygon(data = density_merge, aes(x=long, y=lat, group=group, fill = 밀도)) +
  scale_fill_gradient("인구밀도(명/km*km)", low="#99E8F9", high="#07008D", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "서울시 2019년 자치구별 인구밀도") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data=h_location, aes(x=경도, y=위도, label=paste(시군구명_한글)), color="white")
ggplotly(plot_density)


map_data_merge <- left_join(map_data_merge, rename(popl_density, "id"="시군구코드")%>%dplyr::select(id,시군구명_한글,밀도), by="id")
map_data_merge <- map_data_merge %>%
  mutate(increase_rate = ((floating_popl_3-floating_popl_1)/floating_popl_1)%>%as.double())

final_dataset <- left_join(final_dataset, rename(popl_density, "id"="시군구코드")%>%dplyr::select(id,시군구명_한글,밀도), by="id")
final_dataset <- final_dataset %>%
  mutate(increase_rate = ((floating_popl_3-floating_popl_1)/floating_popl_1)%>%as.double())

cov(x=final_dataset$increase_rate, y=final_dataset$밀도, use="complete.obs", method=c("pearson")) #피어슨 공분산
cor(x=final_dataset$increase_rate, y=final_dataset$밀도, use="complete.obs", method=c("pearson")) #피어슨 상관계수

plot_rate <- ggplot() +
  geom_polygon(data = map_data_merge, aes(x=long, y=lat, group=group, fill = ((floating_popl_3-floating_popl_1)/floating_popl_1)*100/(밀도%>%as.integer()/10000))) +
  scale_fill_continuous("증감률", type="viridis", space = "Lab", guide = "colourbar") +
  theme_bw() + labs(title = "서울시 1월 대비 3월 인구밀도당 유동인구 증감율") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data=h_location, aes(x=경도, y=위도, label=paste(시군구명_한글)), color="red")
ggplotly(plot_rate)

out <- lm(increase_rate ~ 밀도, data=final_dataset)
summary(out)

out_map <- lm(increase_rate ~ 밀도, data=map_data_merge)
summary(out_map)

plot_relation <- ggplot(final_dataset, aes(x=밀도, y=increase_rate)) +
  geom_point(color='gray75') + 
  theme_bw() + labs(title = "인구밀도-증감률 상관관계도") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  stat_smooth(method = 'lm', se=F, color='red') + 
  geom_text(x=20000, y=-0.10, label="y=3.744e-06 - 1.783e-01") + 
  geom_text(x=10000, y=-0.12, label="R²=0.1193")
ggplotly(plot_relation)


relation_df <- relation_df %>% rename("주차"="confirmed_date")

scaleFactor <- max(relation_df$n) / max(relation_df$sum_popltn_byweek)

plots_two <- ggplot(relation_df, aes(x=주차)) +
  geom_line(aes(y=n), col="blue") +
  geom_line(aes(y=sum_popltn_byweek * scaleFactor), col="red") +
  scale_y_continuous(name="n", sec.axis=sec_axis(~./scaleFactor, name="sum_popltn_byweek")) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  )

ggplotly(plots_two)


plots <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "주별 신규 확진자 수"
)
fig <- plot_ly()
fig <- fig %>% add_lines(x = relation_df$주차, y = relation_df$sum_popltn_byweek, name = "유동인구", yaxis="y1")
fig <- fig %>% add_lines(x = relation_df$주차, y = relation_df$n, name = "신규확진자", yaxis = "y2")
fig <- fig %>% layout(
  title = "주별 신규 확진자 수 및 주별 서울 유동인구 수", yaxis2 = plots,
  xaxis = list(title="주차")
)

fig

gu_accum_patient_noetc <- gu_accum_patient %>% filter(city != "etc" )
gu_accum_patient_noetc <- gu_accum_patient_noetc %>% rename("시군구명_영문"="city")
relation_density_patientNum <- left_join(gu_accum_patient_noetc, (popl_density%>%dplyr::select(시군구명_영문, 밀도)), by="시군구명_영문")
cov(x=relation_density_patientNum$new_patient, y=relation_density_patientNum$밀도, use="complete.obs", method=c("pearson")) #피어슨 공분산
cor(x=relation_density_patientNum$new_patient, y=relation_density_patientNum$밀도, use="complete.obs", method=c("pearson")) #피어슨 상관계수

out_den_pat <- lm(new_patient ~ 밀도, data=relation_density_patientNum)
summary(out_den_pat)

plot_relation_density_patient <- ggplot(relation_density_patientNum, aes(x=밀도, y=new_patient)) +
  geom_point(color='gray75') + 
  theme_bw() + labs(title = "인구밀도-확진자수 상관관계도") +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  stat_smooth(method = 'lm', se=F, color='red') + 
  geom_text(x=20000, y=-0.10, label="y=5.596e-04 + 3.954e+01") + 
  geom_text(x=10000, y=-0.12, label="R²=0.01288")
ggplotly(plot_relation_density_patient)


  ##################################
  # 데이터셋 깔아놨지라
  
  fpopl1 <- read.csv(file = "c:/COVID19_data/fpopl_202001.csv", header = T); fpopl1
  
  fpopl2 <- read.csv(file = "c:/COVID19_data/fpopl_202002.csv", header = T); fpopl2
  
  fpopl3 <- read.csv(file = "c:/COVID19_data/fpopl_202003.csv", header = T); fpopl3
  
  fpopl4 <- read.csv(file = "c:/COVID19_data/fpopl_202004.csv", header = T); fpopl4
  
  fpopl5 <- read.csv(file = "c:/COVID19_data/fpopl_202005.csv", header = T); fpopl5
  
  fpopl6 <- read.csv(file = "c:/COVID19_data/fpopl_seoul_06.csv", header = T); fpopl6
  
  fpopl1_pop <- sum(fpopl1$popltn_cascnt)
  fpopl2_pop <- sum(fpopl2$popltn_cascnt)
  fpopl3_pop <- sum(fpopl3$popltn_cascnt)
  fpopl4_pop <- sum(fpopl4$popltn_cascnt)
  
  table()
  qplot()
  
  # 구 번호와 이름 매핑
  adstrd_code_to_name <- example04_ver2 %>% select(c("adstrd_code_more"))
  adstrd_code_to_name <- adstrd_code_to_name %>% arrange(adstrd_code_more)
  adstrd_code_to_name $ name <- c("종로구", "중구", "용산구", "성동구", "광진구", "동대문구", "중랑구", "성북구", "강북구", "도봉구", "노원구", "은평구", "서대문구", "마포구", "양천구", "강서구", "구로구", "금천구", "영등포구", "동작구", "관악구", "서초구", "강남구", "송파구", "강동구")
  
  
  table(fpopl1_200101_11620695$sexdstn_se_code)
  table(is.na(fpopl1_200101_11620695))
  
  fpopl1_200101_pop <- sum(fpopl1_200101$popltn_cascnt)
  qplot(fpopl1_200101$popltn_cascnt)
  
  
  
  fpopl1_200101_11620695 <- fpopl1 %>% filter(etl_date == 20200101) %>% 
    filter(adstrd_code  == 11620695)
  
  fpopl1_200101_11620695 %>% filter(sexdstn_se_code == 'M') %>% 
    arrange(desc(popltn_cascnt)) %>% 
    head(10)
  
  example1 <- fpopl1_200101_11620695 %>% group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt)) %>% 
    arrange(-mean_popltn_cascnt)
  
  example1 <- fpopl1_200101_11620695 %>% group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  ggplot(data=fpopl1_200101_11620695, aes(x=agrde_se_code, y=popltn_cascnt)) + geom_point()
  
  example01 <- fpopl1 %>% group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  example01_M <- fpopl1 %>% filter(sexdstn_se_code == "M") %>% 
    group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  example01_F <- fpopl1 %>% filter(sexdstn_se_code == "F") %>% 
    group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  example01_M $ sex <- c("M") 
  example01_F $ sex <- c("F") 
  example01_sex <- bind_rows(example01_M, example01_F) %>% 
    arrange(agrde_se_code)
  
  ggplot(data=example01_sex, aes(x=agrde_se_code, y=mean_popltn_cascnt, fill = sex)) + geom_col(position = "dodge")
  ggplotly(ggplot(data=example01_sex, aes(x=agrde_se_code, y=mean_popltn_cascnt, fill = sex)) + geom_col(position = "dodge"))
  
  
  example01_200101 <- fpopl1_200101 %>% group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  ggplot(data=example01, aes(x=(agrde_se_code), y=mean_popltn_cascnt, group = 1)) + geom_point() + geom_line()
  
  ggplot(data=example01, aes(x=agrde_se_code, y=mean_popltn_cascnt)) + geom_col()
  
  ggplot(data=example01_200101, aes(x=agrde_se_code, y=mean_popltn_cascnt)) + geom_col()
  
  ggplot(data=example1, aes(x=agrde_se_code, y=mean_popltn_cascnt)) + geom_point()
  ggplot(data=example1, aes(x=agrde_se_code, y=mean_popltn_cascnt)) + geom_col()
  
  ggplotly(ggplot(data=example1, aes(x=agrde_se_code, y=mean_popltn_cascnt)) + geom_col())
  
  
  # 성별별 유동인구 차이
  fpopl <- bind_rows(fpopl1, fpopl2, fpopl3, fpopl4, fpopl5, fpopl6)
  
  example01 <- fpopl %>% group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  example01_M <- fpopl %>% filter(sexdstn_se_code == "M") %>% 
    group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  example01_F <- fpopl %>% filter(sexdstn_se_code == "F") %>% 
    group_by(agrde_se_code) %>% 
    summarise(mean_popltn_cascnt = mean(popltn_cascnt))
  
  example01_M $ sex <- c("M") 
  example01_F $ sex <- c("F") 
  example01_sex <- bind_rows(example01_M, example01_F) %>% 
    arrange(agrde_se_code)
  
  ggplotly(ggplot(data=example01_sex, aes(x=agrde_se_code, y=mean_popltn_cascnt, fill = sex)) + geom_col(position = "dodge"))
  
  
  # 월별 유동인구 일별 추이
  example02_mon01 <- fpopl3 %>% group_by(etl_date) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  
  ggplotly(ggplot(data=example02_mon01, aes(x=etl_date, y=sum_popltn_cascnt)) + geom_point() +geom_line() +  labs(title = "1월 유동인구 일별 추이", x = "날짜", y = "총 유동인구"))
  
  example02_mon02 <- fpopl3 %>% group_by(etl_date) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  
  ggplotly(ggplot(data=example02_mon02, aes(x=etl_date, y=sum_popltn_cascnt)) + geom_point() +geom_line() +  labs(title = "2월 유동인구 일별 추이", x = "날짜", y = "총 유동인구"))
  
  
  example02_mon03 <- fpopl3 %>% group_by(etl_date) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  
  ggplotly(ggplot(data=example02_mon03, aes(x=etl_date, y=sum_popltn_cascnt)) + geom_point() +geom_line() +  labs(title = "3월 유동인구 일별 추이", x = "날짜", y = "총 유동인구"))
  
  # 서울시 환자 추출
  
  patient_info <- read.csv(file = "c:/COVID19_data/PatientInfo.csv", header = T); patient_info
  
  patient_info <- patient_info %>% arrange(confirmed_date)
  
  patient_seoul <-  patient_info %>% 
    filter(province == "Seoul")
  
  patient_seoul $ num <- 1:1312
  patient_seoul <-  patient_info %>% 
    filter(province == "Seoul")
  
  
  ggplot(data=patient_seoul, aes(x=confirmed_date)) + geom_bar()
  ggplotly(ggplot(data=patient_seoul, aes(x=confirmed_date)) + geom_bar())
  
  # 1월 대비 2월 감소율 추이
  
  example04_01 <- fpopl1 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(mean_popltn_cascnt = mean(sum_popltn_cascnt))
  
  ggplot(data=example04_01, aes(x=adstrd_code_more, y=mean_popltn_cascnt)) + geom_col()
  
  example04_02 <- fpopl2 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(mean_popltn_cascnt = mean(sum_popltn_cascnt))
  
  ggplot(data=example04_02, aes(x=adstrd_code_more, y=mean_popltn_cascnt)) + geom_col()
  
  example04_01 $ month <- c("01") 
  example04_02 $ month <- c("02") 
  example04 <- bind_rows(example04_01, example04_02) %>% 
    arrange(adstrd_code_more)
  
  ggplotly(ggplot(data=example04, aes(x=adstrd_code_more, y=mean_popltn_cascnt, fill = month)) + geom_col(position="dodge"))
  
  
  # 1월 대비 2월 지역별 감소율 추이
  
  
  example04_01 <- fpopl1 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(mean_popltn_cascnt_01 = mean(sum_popltn_cascnt))
  
  ggplot(data=example04_01, aes(x=adstrd_code_more, y=mean_popltn_cascnt_01)) + geom_col()
  
  example04_02 <- fpopl2 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(mean_popltn_cascnt_02 = mean(sum_popltn_cascnt))
  
  ggplot(data=example04_02, aes(x=adstrd_code_more, y=mean_popltn_cascnt_02)) + geom_col()
  
  
  example04_ver2 <- left_join(example04_01, example04_02, by="adstrd_code_more")
  example04_ver2 <- left_join(example04_ver2, adstrd_code_to_name, by="adstrd_code_more")
  example04_ver2 <- example04_ver2 %>% mutate(dec_rate = (mean_popltn_cascnt_02 - mean_popltn_cascnt_01) / mean_popltn_cascnt_01 * 100) %>% 
    arrange(dec_rate)
  
  ggplotly(ggplot(data=example04_ver2, aes(x=reorder(name, dec_rate), y=dec_rate)) + geom_col())
  
  
  # 1월 대비 2월 지역별 감소율 추이_ver2
  why <- fpopl1 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  
  example04_01 <- fpopl1 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_01, aes(x=adstrd_code_more, y=sum_popltn_cascnt_01)) + geom_col()
  
  example04_02 <- fpopl2 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(sum_popltn_cascnt_02 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_02, aes(x=adstrd_code_more, y=sum_popltn_cascnt_02)) + geom_col()
  
  
  example04_ver2 <- left_join(example04_01, example04_02, by="adstrd_code_more")
  example04_ver2 <- left_join(example04_ver2, adstrd_code_to_name, by="adstrd_code_more")
  example04_ver2 <- example04_ver2 %>% mutate(dec_rate = (sum_popltn_cascnt_02 - sum_popltn_cascnt_01) / sum_popltn_cascnt_01 * 100) %>% 
    arrange(dec_rate)
  
  ggplotly(ggplot(data=example04_ver2, aes(x=reorder(name, dec_rate), y=dec_rate)) + geom_col()  +  labs(title = "1월 대비 2월 유동인구 지역별 감소 추이", x = "지역", y = "감소율"))
  
  # 1월 대비 3월 유동인구 지역별 감소 추이
  why <- fpopl1 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  
  example04_01 <- fpopl1 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_01, aes(x=adstrd_code_more, y=sum_popltn_cascnt_01)) + geom_col()
  
  example04_03 <- fpopl3 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(sum_popltn_cascnt_03 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_03, aes(x=adstrd_code_more, y=sum_popltn_cascnt_03)) + geom_col()
  
  
  example04_ver2 <- left_join(example04_01, example04_03, by="adstrd_code_more")
  example04_ver2 <- left_join(example04_ver2, adstrd_code_to_name, by="adstrd_code_more")
  example04_ver2 <- example04_ver2 %>% mutate(dec_rate = (sum_popltn_cascnt_03 - sum_popltn_cascnt_01) / sum_popltn_cascnt_01 * 100) %>% 
    arrange(dec_rate)
  
  ggplotly(ggplot(data=example04_ver2, aes(x=reorder(name, dec_rate), y=dec_rate)) + geom_col()  +  labs(title = "1월 대비 3월 유동인구 지역별 감소 추이", x = "지역", y = "감소율"))
  
  
  
  ### 4-2-1 구로구 어느 세대에서 감소율이 컸지?
  example04_ver2_mon01 <- fpopl1 %>%
    group_by(agrde_se_code) %>% 
    summarise(sum_popltn_cascnt_01 = sum(popltn_cascnt))
  
  ggplot(data=example04_ver2_mon01, aes(x=agrde_se_code, y=sum_popltn_cascnt_01)) + geom_col()
  
  example04_ver2_mon03 <- fpopl3 %>%
    group_by(agrde_se_code) %>% 
    summarise(sum_popltn_cascnt_03 = sum(popltn_cascnt))
  
  ggplot(data=example04_ver2_mon02, aes(x=agrde_se_code, y=sum_popltn_cascnt_02)) + geom_col()
  
  example04_ver3 <- left_join(example04_ver2_mon01, example04_ver2_mon03, by="agrde_se_code")
  example04_ver3 <- example04_ver3 %>% mutate(dec_rate = (sum_popltn_cascnt_03 - sum_popltn_cascnt_01) / sum_popltn_cascnt_01 * 100) %>% 
    arrange(dec_rate)
  
  ggplotly(ggplot(data=example04_ver3, aes(x=reorder(agrde_se_code, dec_rate), y=dec_rate)) + geom_col() +  labs(title = "1월 대비 3월 유동인구 연령별 감소 추이", x = "연령", y = "감소율"))
  
  
  
  ### 4-2-2 어느지역의 특성?
  
  region <- read.csv(file = "c:/COVID19_data/region.csv", header = T); 
  seoul_region <- region[region$code <= 10250,]
  
  seoul_region <- seoul_region %>% arrange(-elderly_population_ratio); View(seoul_region)
  seoul_region <- seoul_region %>% mutate(sum_ele_kinder = elementary_school_count + kindergarten_count)
  seoul_region <- seoul_region %>% arrange(-sum_ele_kinder); View(seoul_region)
  
  ## 4-3 1 -> 3월 감소추이 
  
  example04_01 <- fpopl1 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(sum_popltn_cascnt_01 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_01, aes(x=adstrd_code_more, y=sum_popltn_cascnt_01)) + geom_col()
  
  example04_03 <- fpopl3 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(mean_popltn_cascnt_03 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_03, aes(x=adstrd_code_more, y=sum_popltn_cascnt_03)) + geom_col()
  
  
  example04_ver3 <- left_join(example04_01, example04_03, by="adstrd_code_more")
  example04_ver3 <- left_join(example04_ver3, adstrd_code_to_name, by="adstrd_code_more")
  example04_ver3 <- example04_ver3 %>% mutate(dec_rate = (mean_popltn_cascnt_03 - mean_popltn_cascnt_01) / mean_popltn_cascnt_01 * 100) %>% 
    arrange(dec_rate)
  
  ggplotly(ggplot(data=example04_ver3, aes(x=reorder(name, dec_rate), y=dec_rate)) + geom_col())
  
  
  
  ## 4-4: 2->3월 감소추이
  
  example04_02 <- fpopl2 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(sum_popltn_cascnt_02 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_02, aes(x=adstrd_code_more, y=sum_popltn_cascnt_02)) + geom_col()
  
  example04_03 <- fpopl3 %>% group_by(adstrd_code) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt)) %>% 
    mutate(adstrd_code_more = adstrd_code %/% 10000) %>% 
    group_by(adstrd_code_more) %>% 
    summarise(sum_popltn_cascnt_03 = sum(sum_popltn_cascnt))
  
  ggplot(data=example04_03, aes(x=adstrd_code_more, y=mean_popltn_cascnt_03)) + geom_col()
  
  
  example04_ver4 <- left_join(example04_02, example04_03, by="adstrd_code_more")
  example04_ver4 <- left_join(example04_ver4, adstrd_code_to_name, by="adstrd_code_more")
  example04_ver4 <- example04_ver4 %>% mutate(dec_rate = (sum_popltn_cascnt_03 - sum_popltn_cascnt_02) / sum_popltn_cascnt_02 * 100) %>% 
    arrange(dec_rate)
  
  ggplotly(ggplot(data=example04_ver4, aes(x=reorder(name, dec_rate), y=dec_rate)) + geom_col() + labs(title = "2월 대비 3월 유동인구 지역별 감소 추이", x = "지역", y = "감소율"))
  
  
  
  
  ## 4-5 (2->3월 성동구에는 무슨일이)
  example04_ver5_mon02 <- fpopl2 %>% filter(adstrd_code %/% 10000 == 1120) %>% 
    group_by(agrde_se_code) %>% 
    summarise(sum_popltn_cascnt_02 = sum(popltn_cascnt))
  
  ggplot(data=example04_ver5_mon02, aes(x=agrde_se_code, y=sum_popltn_cascnt_02)) + geom_col()
  
  example04_ver5_mon03 <- fpopl3 %>% filter(adstrd_code %/% 10000 == 1120) %>% 
    group_by(agrde_se_code) %>% 
    summarise(sum_popltn_cascnt_03 = sum(popltn_cascnt))
  
  ggplot(data=example04_ver5_mon03, aes(x=agrde_se_code, y=sum_popltn_cascnt_03)) + geom_col()
  
  example04_ver5 <- left_join(example04_ver5_mon02, example04_ver5_mon03, by="agrde_se_code")
  example04_ver5 <- example04_ver5 %>% mutate(inc_rate = (sum_popltn_cascnt_03 - sum_popltn_cascnt_02) / sum_popltn_cascnt_02 * 100) %>% 
    arrange(inc_rate)
  
  ggplotly(ggplot(data=example04_ver5, aes(x=reorder(agrde_se_code, -inc_rate), y=inc_rate)) + geom_col() + labs(title = "2월 대비 3월 성동구 연령별 증가 추이", x = "연령", y = "증가율"))
  
  
  ####### 5 주중 주말
  install.packages('lubridate')
  library('lubridate')
  
  example05_01 <- fpopl1 %>% group_by(base_ymd) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  example05_01$base_ymd <- as.numeric(example05_01$base_ymd)
  
  example05_02 <- fpopl2 %>% group_by(base_ymd) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  example05_02$base_ymd <- as.numeric(example05_02$base_ymd)
  
  example05_03 <- fpopl3 %>% group_by(base_ymd) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  example05_03$base_ymd <- as.numeric(example05_03$base_ymd)
  
  example05_04 <- fpopl4 %>% group_by(base_ymd) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  example05_04$base_ymd <- as.numeric(example05_04$base_ymd)
  
  example05_05 <- fpopl5 %>% group_by(base_ymd) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  example05_05$base_ymd <- as.numeric(example05_05$base_ymd)
  
  example05_06 <- fpopl6 %>% group_by(base_ymd) %>% 
    summarise(sum_popltn_cascnt = sum(popltn_cascnt))
  example05_06$base_ymd <- as.numeric(example05_06$base_ymd)
  
  example05 <- bind_rows(example05_01, example05_02, example05_03, example05_04, example05_05, example05_06)
  
  example05 <- example05 %>% mutate(date_more = ymd(base_ymd)) %>% 
    mutate(week = week(date_more)) %>% 
    group_by(week) %>% 
    summarise(sum_popltn_cascnt = sum(sum_popltn_cascnt))
  
  
  ggplotly(ggplot(data=example05, aes(x=week, y=sum_popltn_cascnt)) + geom_line() + labs(title = "유동인구 주차별 비교" , x = "주차", y = "유동인구의 총합"))
  
  
  patient_info <- patient_info %>% arrange(confirmed_date)
  
  patient_seoul_week <-  patient_info %>% 
    filter(province == "Seoul")
  
  patient_seoul_week $ num <- 1:1312
  patient_seoul_week <-  patient_info %>% 
    filter(province == "Seoul") %>% 
    count(confirmed_date) %>% 
    mutate(week = week(confirmed_date)) %>% 
    group_by(week) %>% 
    summarise(patient_count = sum(n))
  
  week <- 1:3
  patient_count <- c(0, 0, 0)
  week_example <- data.frame(week,patient_count)
  
  patient_seoul_week <- bind_rows(week_example, patient_seoul_week)
  
  ggplotly(ggplot(data=patient_seoul_week, aes(x=week, y=patient_count)) + geom_line() + labs(title = "확진자 주차별 비교"))
  
  ggplot(example05_main, aes(x=week)) + 
    geom_line(aes(y = sum_popltn_cascnt), color = "darkred") + 
    geom_line(aes(y = patient_count), color="steelblue", linetype="twodash")
  
  
  example05_main <- left_join(example05, patient_seoul_week, by = "week") %>% 
    filter(week < 16)
  
  
  cov(x=example05_main$patient_count, y=example05_main$sum_popltn_cascnt, use="complete.obs", method = c("pearson"))
  cor(x=example05_main$patient_count, y=example05_main$sum_popltn_cascnt, use="complete.obs", method = c("pearson"))
  
  
  install.packages('tidyverse')
  library(tidyverse)
  
  p <- ggplot(example05_main, aes(x = week)) +
    geom_line(aes(y = sum_popltn_cascnt, color = 'PK')) +
    labs(y = "Concentration (PK) or Effect (PD)", 
         x = "Time (h)",
         colour = "Parameter") +
    theme_light(); p
  
  

install_github('dkahle/ggmap')

station_data <-read.csv("역별_주소_및_전화번호.csv")
str(station_data)

station_code <- as.character(station_data$구주소)

googleAPIkey <-"AIzaSyDrukGXGw3S_tIfEFMvdpaOVcI9FVzvXUY"

register_google(googleAPIkey)

station_code <-geocode(station_code)

head(station_code)

# 구글지도를 활용하여 서울시 지도에

#지하철역과 주변 아파트 실거래가 표시

#일부 데이터가 NA값으로 나올 경우에는 
#아래 처럼 인코딩을 변경하여 다시 좌표를 구한다.

#문자형으로 변환하고 utf8로 변환한 후 위도와 경도로 변환

station_code <- as.character(station_data$구주소) %>% enc2utf8() %>% geocode()

#station_code 데이터 앞부분 확인
head(station_code)

#---------------------------------------------------------------------------------
#기존 station data에 위도/경도 정보를 추가
#station_data와 station code를 합친 후 station_code_final에 할당
station_code_final <-cbind(station_data,station_code)

#station_code_final의 앞부분 확인
head(station_code_final)

################################################################################
#3. 아파트 실거래가 데이터 가공하기

#------------------------------------------------------------------------------
#3-1.전용 면적별 거래가격
#csv 파일을 가져와서 apart_data변수에 할당
apart_data <-read.csv("아파트_실거래가.csv")

#apart_data 앞부분 데이터 확인
head(apart_data)

#ceiling() /floor()
#전용면적의 값을 반올림하여 정수로 표현

apart_data$전용면적 = round(apart_data$전용면적)

#데이터 앞부분 확인
head(apart_data)

#count():지정한 집단별 행의 갯수
#count(데이터셋,컬럼명)
#arrange():기본형(오름차순)/arrange(desc())를 이용하면 내림차순 정렬가능

#전용면적을 기준으로 빈도를 구한 후 빈도에 따라 내림차순 정렬
count(apart_data,전용면적) %>% arrange(desc(n))

#전용 면적이 85인 데이터만 추출하여 apart_data_85에 할당
apart_data_data_85 <- subset(apart_data,전용면적 == "85")

#-------------------------------------------------------------------------
#3-2.아파트 단지별 평균 거래 금액

# 쉼표를 공백("")으로 대체하여 제거

apart_data_data_85$거래금액 <-gsub(",","",apart_data_data_85$거래금액)

#결과 확인하기
head(apart_data_data_85)

#거래금액을 정수형으로 변환 : as.integer(거래금액)
#단지명별 평균을 구한후 mean(as.integer(거래금액)-단지명)
#apart_data_data_85_cost 변수에 할당.

#aggregate() : R내중함수, 그룹별로 묶어서 연산할때 사용.
#aggregate(집계할 내용,데이터 셋,집계 함수)
#집계할 내용 :연살칼럼명 ~기준칼럼명


apart_data_data_85_cost <- aggregate(as.integer(거래금액)~단지명,
                                     apart_data_data_85,
                                     mean)
#apart_data_data_85_cos 앞부분 확인
head(apart_data_data_85_cost)

#"as.integer(거래금액)"을
#"거래금액"으로 변경하여 저장
#rename(데이터 셋,"변경후 이름" = "변경전 이름")
apart_data_data_85_cost <- rename(apart_data_data_85_cost,"거래금액"="as.integer(거래금액)")
#결과 확인하기
head(apart_data_data_85_cost)

#단지명이 중복된 행을 제거하고, duplicated(apart_data_85$단지명)
#apart_data_data_85에 저장
#duplicated() :중복행 제거 함수 중복된값(TRUE)/ 처음 나오는 값(fALSE) FALSE에 해당하는 값들을 배열로 반환

apart_data_data_85 <- apart_data_data_85[!duplicated(apart_data_data_85$단지명),]
#결과 확인하기
head(apart_data_data_85)

#"단지명"을 기준으로
#중복 제거한 데이터 셋(apart_data_data_85)에
#평균 거래 금액 데이터 셋(apart_data_data_85_cost) 합치기

#이떄 left_join() 함수를 사용.
#left_join(데이터 셋,데이터 셋,기준컬럼)
#합한 후 동일한 컬럼명이 존재할 경우,
#컬렴명_x 컬럼명_y형태로 자동구분

names(apart_data_data_85)
names(apart_data_data_85_cost)

apart_data_data_85 <- left_join(apart_data_data_85,apart_data_data_85_cost,by="단지명")

names(apart_data_data_85)
head(apart_data_data_85)

table(is.na(apart_data_data_85))

#평균 거래금액("거래금액.y")을 이용하여 시각화 작업을 할 예정.
#"단지명","시군구","번지","전용면적","거래금액.y"만 추출하고 저장
#
#
#
#
apart_data_data_85 <- apart_data_data_85 %>%  select("단지명",
                                                     "시군구",
                                                     "번지",
                                                     "전용면적",
                                                     "거래금액.y")
#"거래금액.y"를 "거래금액"으로 변경한 후 저장
apart_data_data_85 <- rename(apart_data_data_85,"거래금액" ="거래금액.y")
names(apart_data_data_85)

#------------------------------------------------------------------------------
#3-3. 시군구와 번지를 하나로 합치기

#테스트 코드
#"시군구"와"번지" 열을 합친 후, :paste(컬럼명,컬럼명)
#apart_address에 저장
#주의 사항: paste()함수를 이용하여 칼럼과 칼럼사이에 공백 발생.
#공백 없이 합할 경우:paste0()함수를 사용.

apart_address <- paste(apart_data_data_85$시군구,apart_data_data_85$번지) %>% data.frame()
#결과 확인하기
head(apart_address)

#"."을 "주소"로 변경하여 저장
apart_address <- rename(apart_address,"주소"=".")

#결과 확인하기
head(apart_address)

#--------------------------------------------------------------------------------------
#3-4.좌표 정보 추가후 최종 데이터 만들기
#아파트 주소를 위,경도로 변환하여
#apart_address_code에 저장
apart_address_code <- as.character(apart_address$주소) %>%  enc2utf8() %>%  geocode()
View(apart_address_code)

#데이터 세트를 합친후
#apart_data85:단지명,전용 면적,거래금액(평균거래금액) 추출
#apart_address:주소 추출
#appart_address_code:위도, 경도 추출
#일부 열만 apart_code final에 저장
apart_code_final <- cbind(apart_data_data_85,apart_address,apart_address_code) %>% select("단지명","전용면적","거래금액","주소",lon,lat)
View(apart_code_final)

#####################################################################################
# 4. 구글 지도에 지하철역과 아파트 가격 표시하기
#-----------------------------------------------------------------------------------
#4-1.마포구 지도 가져오기
#마포구 지도 정보를 가져와 mapo_map에 저장
mapo_map <- get_googlemap("mapogu",maptype = "roadmap",zoom=12)

#구글 지도 호출
ggmap(mapo_map)

#4-2. 지하철역 위치 및 아파트 가격 정보 표시하기
#ggplot2 패키지 설치
install.packages("ggplot2")
#ggplot2 패키지 로드
library(ggplot2)

#지하철 역 위치 표시 및 여명 표시

ggmap(mapo_map)+geom_point(data= station_code_final,
                           aes(x=lon,y=lat),
                           colour="red",
                           size=3)+
  geom_text(data=station_code_final,aes(label=역명,vjust=-1))

#홍대입구역 지도 정보를 가져와 hongdae_map 변수에 저장

hongdae_map <- get_googlemap("hongdae station",maptype="roadmap",zoom=15)

ggmap(hongdae_map )
#홍대 입구역 지도에 지하철 정보 및 아파트 정보 일괄 표시
ggmap(hongdae_map)+geom_point(data = station_code_final,
                              aes(x=lon,y=lat),
                              colour = "blue",
                              size = 6)+
  geom_text(data = station_code_final,
            aes(label = 역명,vjust = -1),colour ="red")+
  geom_point(data = apart_code_final,
             aes(x=lon,y=lat))+
  geom_text(data=apart_code_final,
            aes(label =단지명,vjust=-1),colour="red")+
  geom_text(data=apart_code_final,
            aes(label=거래금액,vjust=1),colour="red")
#09-1. ‘한국 복지패널 데이터’ 분석 준비하기

#한국 복지 패널 데이터
#한국 보건사회 연구원 발간
#가구의 경제활동을 연구해 정책 지원에 반영할 목적
#2006~2015년까지 전국에서 7000여 가구를 선언해 매년 추적 조사
#경제 활동,생활시래, 복지욕구등 수천 개 변수에 대한 정보로 구성

#패키지 준비하기

install.packages("foreign") #foreign 패키지 설치
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
#데이터 준비하기
#데이터 불러오기

raw_weifare <- read.spss(file="Koweps_hpc10_2015_beta1.sav",to.data.frame = T)

#복사본 만들기

welfare <-raw_weifare

dim(welfare)#행의 개수와 열의 개수를 세는 함수

#데이터 검토하기
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summarise(welfare)

#대구모 데이터는 변수가 많고 변수명이 코드로 되어 있어서 전체 데이터 구조를 한눈에 파악하기 어렵다.
#변수명을 쉬운 단어로 바꾼후 분석에 사용할 변수를 각각 파악해야함

#변수명 바꾸기

welfare <- rename(welfare,
                  sex = h10_g3, #성별
                  birth = h10_g4, #태어난 연도
                  marriage = h10_g10,#혼인 상태
                  religion = h10_g11,#종교
                  income = p1002_8aq1, #월급
                  code_job = h10_eco9, #직종코드
                  code_region = h10_reg7) #지역 코드
names(welfare)

#데이터 분석 절차
#1단계 변수 검토 및 전처리
#2단계 변수간 관계 분석


#09-2 성별에 따른 월급차이 -"성별에 따라 월급이 다를까?"
#분석 절차
#1.변수 검토 및 전처리
# - 성별
# - 월급
class(welfare$sex)
table(welfare$sex)
#이상측 결측 처리
welfare$sex <-ifelse(welfare ==9,NA,welfare$sex)

#결측치 확인
table(is.na(welfare$sex))

#성별 항목 이름 부여
welfare$sex <- ifelse(welfare$sex == 1,"male","female")

qplot(welfare$sex)

#월급 변수 검토 및 전처리
#1.변수 검토하기
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income)+xlim(0,1000)

#2.전처리 
welfare$income <-ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))
#2.변수 간 관계 분석
# - 성별 월급 평균표 만들기
# - 그래프 만들기
#1.성별 월급 평균표 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income

ggplot(data=sex_income,aes(x=sex,y=mean_income))+geom_col()


#09-3 나이와 월급의 관계 "몇살떄 월급을 가장 많이 받을까?"
#분석절차
#1.변수 검토 및 전처리
# 나이
# 월급
#변수간 관계분석
#나이에 따른 월급 평균표 만들기 

#1. 변수 검토하기
class(welfare$birth)
#이상치 확인
summary(welfare$birth)

qplot(welfare$birth)

#2.월급 전처리(위에서 처리함)

#이상치 확인
summary(welfare$birth)

#결측치 확인
table(is.na(welfare$birth))
#이상치 결측 처리
welfare$birth <- ifelse(welfare$birth==9999,NA,welfare$birth)

table(is.na(welfare$birth))

#3.파생변수 만들기 -나이
welfare$age <- 2015 - welfare$birth +1

summary(welfare$age)

qplot(welfare$age)
#나이와 월급의 관계 분석하기
#1.나이에 따른 월급 평균표 만들기

age_income <- welfare %>%
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income)
rank(age_income)
#2.그래프 만들기
ggplot(data=age_income, aes(x=age,y=mean_income))+geom_line()

#09-4연령대에 따른 월급차이
#어떤 연령대의 월급이 가장 많을까?
#분석 절차
#1. 변수 검토 및 전처리
#-연령대
#-월급
#2.변수간 관계분석
#-연령대별 월급 평균표 만들기
#-그래프 만들기

#파생변수 만들기 - 연령대
welfare <- welfare %>% 
  mutate(ageg=ifelse(age<30,"young",
                     ifelse(age<=59,"middle","old")))
table(welfare$ageg)

qplot(welfare$ageg)

#연령대에 따른 월급 차이 분석하기
#1.연령대별 월급 평균표 만들기
ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income

#2.그래프 만들기
ggplot(data=ageg_income, aes(x=ageg,y=mean_income))+geom_col()

#막대정렬:초년, 중년,노년 나이순
ggplot(data = ageg_income,aes(x=ageg,y=mean_income))+geom_col()+scale_x_discrete(limits = c("young","middle","old"))

#성별 연령별 급여 
#09-5 연령대 및 성별 월급 차이
#-"성별 월급 차이는 연령대별로 다를까?"
#분석 절차
#1.변수 검토 및 전처리
#연령대
#성별
#월급
#2.변수간 관계분석
#연령대 및 성별 월급 평균표 만들기
#그래프 만들기

#연령대 및 성별 월급 평균표 만들기
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income=mean(income))

sex_income
#2.그래프 만들기
ggplot(data = sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col()+scale_x_discrete(limits = c("young","middle","old"))

#성별 막대 분리
ggplot(data = sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col(position = "dodge")+scale_x_discrete(limits = c("young","middle","old"))

#나이 및 성별 월급 차이 분석하기
#성별 연령별 월급 평균표 만들기

sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age)
ggplot(data=sex_age, aes(x=age,y=mean_income,col=sex))+geom_line()

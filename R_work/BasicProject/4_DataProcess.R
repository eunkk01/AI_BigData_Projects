#### 기술 통계량 ####

## table()

aws <- read.delim("../data/AWS_sample.txt", sep="#", fileEncoding = "euc-kr")
head(aws) 
str(aws)   # = info 정보를 알려줌

table(aws$AWS_ID)   # 분할표(빈도수) = value_counts
table(aws$AWS_ID, aws$X.)

table(aws[, c("AWS_ID", "X.")])

aws[2500:3100, "X."] = "modified"
aws[2500:3100, ]
table(aws$AWS_ID, aws$X.)

prop.table(table(aws$AWS_ID))        # 각 데이터가 차지하는 비율 알려줌
prop.table(table(aws$AWS_ID)) * 100  # 백분율

paste(prop.table(table(aws$AWS_ID)) * 100, "%")
prop.table(table(aws$Wind))

## 기술 통계 함수의 모듈화

# 각 컬럼 단위로 빈도와 최대, 최소값 계산

test <- read.csv("../data/test.csv", header = T)
head(test)
length(test)
str(test)

table(test$"A")
max(test$A)
min(test$"A")

col = names(test)
col

data_summary <- function(df){
  for(i in 1:length(df)){
    cat(i, "번째 컬럼의 빈도 분석 결과 ")
    print(table(df[i]))
    cat("\n")
  }
  for(i in 1:length(df)){
    f <- table(df[i])
    cat(i, "번째 컬럼의 최대빈도값/최소빈도값 결과 : ")
    cat("max =", max(f), "/ min =", min(f), "\n")
  }
}

data_summary(test)

#### dplyr ####

install.packages("dplyr")
library(dplyr)
ls("package:dplyr")

exam <- read.csv("../data/csv_exam.csv")
exam

## 1. filter() : 행을 기준으로 걸러줌 = where

# 1반 학생들의 데이터 추출

exam[which(exam$class=="1"),]
exam[exam$class=="1",]
subset(exam, class==1)  # subset() : 조건에 맞는 데이터 추출하기

filter(exam, class==1)
exam %>% filter(class==1)

# 2반이면서 영어 점수가 80점 이상인 데이터 추출

exam[which(exam$class=="2" & exam$english >= 80),]
exam %>% filter(class=="2" & english>=80)

# 1, 3, 5반에 해당하는 데이터만 추출

exam %>% filter(class==1 | class==3 | class==5)
exam %>% filter(class %in% c(1, 3, 5))


## 2. select() : 열기준으로 걸러줌

# 수학 점수만 추출

exam[, "math"]         # 벡터 타입
exam %>% select(math)  # df 타입

# 반, 수학, 영어점수 추출

exam[, c(2, 3, 4)]
exam %>% select(class, math, english)

# 1반 학생들의 수학 점수만 추출(2명만 표시)
# SELECT class, math FROM exam WHERE class==1  limit 2;

result <- filter(exam, class==1)
result <- select(result, math)
head(result, 2)

exam %>% filter(class==1) %>% select(math) %>% head(2)


## 3. arrange() : 정렬

exam %>% arrange(math)
exam %>% arrange(desc(math))   # 내림차순
exam %>% arrange(class, math)  # 다중 정렬 가능


## 4. mutate() : 파생변수 만드는 함수

exam$sum <- exam$math + exam$english + exam$science
exam

exam$mean <- exam$sum / 3
exam

exam <- exam[, -c(6, 7)]  # 컬럼 삭제
exam

exam <- exam %>% mutate(sum=math+english+science, mean=sum/3) # 추가후 원본에 꼭 넣어주기
exam


## 5. summerize() : 여러 함수 한번에 적용 가능

exam %>% summarize(mean_math=mean(math), sum_math=sum(math), 
                   median_math=median(math), count=n())


## 6. group_by()

exam %>% group_by(class) %>% summarize(mean_math=mean(math), sum_math=sum(math), 
                                       median_math=median(math), count=n())


## 7. left_join() : 열, bind_rows(): 행

test1 <- data.frame(id=c(1:5), midterm=c(60, 70, 80, 90, 85))
test2 <- data.frame(id=c(1:5), midterm=c(70, 86, 65, 96, 80))

left_join(test1, test2, by="id") # wide형
bind_rows(test1, test2)          # long형


## 8. 연습문제1

install.packages("ggplot2")
library(ggplot2)

head(ggplot2::mpg)
str(ggplot2::mpg)  # tibble : df를 개선한 타입
class(ggplot2::mpg)

mpg <- as.data.frame(ggplot2::mpg)
class(mpg)
tail(mpg)
names(mpg)
dim(mpg)
View(mpg)
mpg

# (1) 배기량(displ)이 4이하인 차량의 모델명, 배기량, 생산연도 조회

mpg %>% filter(displ<=4) %>% select(model, displ, year)

# (2) 통합연비 파생변수를 만들고 통합연비로 내림차순 정렬 후 3개의 행만 선택해서 조회
#     통합연비 total <- (cty +  hwy)/2

mpg$total <- (mpg$cty + mpg$hwy) / 2
head(mpg)
mpg %>% arrange(desc(total)) %>% head(3)

# (3) 회사별로 "suv"차량의 도시 및 고속도로 통합연비 평균을 구해 내림차순으로 정렬하고
#     1위~5위까지 조회

mpg %>% filter(class=="suv") %>% group_by(manufacturer) %>% 
  summarize(mean_total=mean(total)) %>% arrange(desc(mean_total)) %>% head(5)

# (4) 어떤 회사의 hwy연비가 가장 높은지 알아보려한다. 
#     hwy평균이 가장 높은 회사 세 곳을 조회

mpg %>% group_by(manufacturer) %>% select(hwy) %>% 
  summarize(mean_hwy=mean(hwy)) %>% arrange(desc(mean_hwy)) %>% head(3)

# (5) 어떤 회사에서 compact(경차) 차종을 가장 많이 생산하는지 알아보려 한다.
#     각 회사별 경차 차종 수를 내림차순으로 정렬

mpg %>% group_by(manufacturer) %>% filter(class=="compact") %>% 
  summarize(count=n()) %>% arrange(desc(count))

# (6) 연료별 가격을 구해서 새로운 데이터 프레임을 만든 후(feul) 기존 데이터셋과 병합하여 출력
#     c:CNG = 2.35, d:Disel = 2.38, e:Ethanol + 2.11 p:Premium = 2.76, r:Regula r= 2.2

n <- c("c", "d", "e", "p", "r")
p <- c(2.35, 2.38, 2.11, 2.76, 2.2)
feul <- data.frame(fl=n, price=p)
feul

mpg <- left_join(mpg, feul, by="fl")
mpg %>% head(5)

# (7) 통합 연비의 기준치를 통해 합격(pass)/불합격(fail)을 부여하는 test란 이름의
#     파생변수 생성, 이때 기준은 20

mpg$test <- ifelse(mpg$total>=20, "pass", "fail")

for(i in mpg$total){
  if(i >= 20){
    return(mpg$test <- "pass")
  }else{
    return(mpg$test <- "fail")
  }
}

# (8) test에 대해 합격과 불합격을 받은 자동차가 각각 몇 대인지

mpg %>% group_by(test) %>% summarize(count=n())

# (9) 통합 연비 등급을 A, B, C 세 등급으로 나누는 파생변수를 추가(grade)
#     30이상이면 A, 20~29는 B, 20미만이면 C로 분류

mpg$grade <- ifelse(mpg$total>=30, "A", ifelse(mpg$total>=20, "B", "C"))

for(total in mpg$total){
  if(total >= 30){
    mpg$grade <- "A"
  }else if(total >= 20){
    mpg$grade <- "B"
  }else{
    mpg$grade <- "C"
  }
}

mpg %>% group_by(grade, manufacturer) %>% summarise(count = n())

## 9. 연습문제2

midwest <- as.data.frame(ggplot2::midwest)
str(midwest)
head(midwest)

# (1) 전체 인구 대비 미성년 인구 백분율(ratio_child) 파생변수 추가

midwest$ratio_child <- ((midwest$poptotal - midwest$popadults) / midwest$poptotal) * 100
head(midwest, 5)

# (2) 미성년 인구 백분율이 가장 높은 상위 5개 지역(county)의 미성년 인구 백분율 조회

midwest %>% arrange(desc(ratio_child)) %>% head(5)

# (3) 기준에 따라 미성년 비율 등급변수를 추가(grade), 각 등급에 몇개의 지역이 있는지 조회
#     기준 : 미성년 인구 백분율이 40 이상이면 "large", 30이상이면 "middle", 그 외 "small"

midwest$grade <- ifelse(midwest$ratio_child>=40, "large", 
                        ifelse(midwest$ratio_child>=30, "middle", "small"))

# (4) 전체 인구 대비 아시아인 인구 백분율(ratio_asian)변수를 추가
#     하위 10개 지역의 state, county, 아시아인 인구 백분율을 조회

midwest$ratio_asian <- (midwest$popasian / midwest$poptotal) * 100
# re2 = head(midwest %>% arrange(ratio_asian), 10)
# re2 %>% select(state, county, ratio_asian)
midwest %>% arrange(ratio_asian) %>% head(10) %>% select(state, county, ratio_asian)


#### Data Processing ####

## 1) 변수 이름 변경

df_raw <- data.frame(var1=c(1:3), var2=c(2:4))
df_raw

# 기본(내장) 함수
names(df_raw)                   # 컬럼 이름 알려줌
names(df_raw) <- c("v1", "v2")  # 컬럼 이름 변경(원본에서 바로)
df_raw

# dplyr
library(dplyr)

df_raw <- rename(df_raw, var1=v1, var2=v2)  # 복사본에서 변경
df_raw                                      # after colname = before colname


## 2) 결측치 처리

data1 <- read.csv("../data/dataset.csv", header=T)

head(data1)
str(data1)

# resident : 1~5까지의 값을 갖는 명목변수로 거주지를 나타냄
# gender : 1~2까지의 값을 갖는 명목변수로 남/녀를 나타냄
# job : 1~3까지의 값을 갖는 명목변수. 직업을 나타냄
# age : 양적변수(비율) : 2~69
# position : 1~5까지의 값을 갖는 명목변수. 직위를 나타냄
# price : 양적변수(비율) : 2.1 ~ 7.9
# survey : 만족도 조사 : 1~5까지 명목변수

# (1) 결측치 확인
is.na(data1)  # True가 결측치
table(is.na(data1))

summary(data1)  # 기본 통계량 + 결측치 개수 알려줌
summary(data1$price)

# (2) 결측치 제거
sum(data1$price)          # 결측치 때문에 계산 안됨
sum(data1$price, na.rm=T) # 결측치 제외 TRUE로 -> 제외하고 계산해줌


p <- data1[!is.na(data1$price), "price"]
summary(p)

price2 <- na.omit(data1$price)  # 결측치 제거해줌
summary(price2)
summary(data1$price)

table(is.na(data1$price))
table(is.na(price2))      # 결측치 30개 제거됨

# (3) 결측치 대체

# 0으로
price3 <- ifelse(is.na(data1$price), 0, data1$price)
summary(price3)
price3

# 평균으로
price4 <- ifelse(is.na(data1$price), mean(data1$price, na.rm=T), data1$price)
summary(price4)
price4


## 3) 이상치 처리

# 질적 데이터 : 도수분포표, 분할표 -> 막대 그래프, 원 그래프
table(data1$gender)  # 0, 5 : 이상치
barplot(table(data1$gender))
pie(table(data1$gender))

# 양적 데이터 : 산술평균, 중앙값, 조화평균 -> 히스토그램, 상자 그래프, 시계열, 산포도
plot(data1$price)  # 기본 그래프가 산포도
boxplot(data1$price)

# 이상치 확인
data2 <- subset(data1, price>=2 & price<=8)  # 이상치 기준 정하기
subset(data1, price>=2 & price<=8)
length(data1$price)
length(data2$price)

plot(data2$price)
boxplot(data2$price)

length(data1$age)
length(data2$age)
summary(data1$age)
summary(data2$age)
plot(data2$age)
boxplot(data2$age)

#### Feature Engineering ####

View(data2)

## 가독성을 위한 데이터 변경(파생변수 추가) : 사람이 분석하기 위해 문자로 변경
data2$resident2[data2$resident==1] <- "1.서울특별시"
data2$resident2[data2$resident==2] <- "2.인천광역시"
data2$resident2[data2$resident==3] <- "3.대전광역시"
data2$resident2[data2$resident==4] <- "4.대구광역시"
data2$resident2[data2$resident==5] <- "5.시구군"


## 척도 변경 : Binning(양적->질적)

# 나이 변수를 청년층(30세 이하), 중년층(31~55이하), 장년층(56~)
data2$age2[data2$age<=30] <- "청년층"
data2$age2[data2$age>30 & data2$age<=55] <- "중년층"
data2$age2[data2$age>55] <- "장년층"


## 역코딩 : 주로 설문조사에서 사용되는 리쿠르트 척도의 순서를 바꾸고자 할 때 사용
## 1->5, 2->4, .., 5->1

table(data2$survey)

data2$survey2 <- 6 - data2$survey
table(data2$survey2)


## 척도 변경 : Dummy(질적->양적)

# one hot : 0과 1만 이용해서 변경(가중치 붙지 않게)
house_data <- read.csv("../data/user_data.csv", header = T)

View(house_data)
str(house_data)

# house_type(거주 유형) : 단독 주택(1), 다가구 주택(2), 아파트(3), 오피스텔(4)
# 1,2 -> 0 / 3,4 -> 1 : 그저 구별하기 위해 나눠둔 것(의미X)

house_data$house_type2 <- ifelse(house_data$house_type==1 | house_data$house_type==2, 0, 1)
table(house_data$house_type2)


## 데이터 구조 변경(wide type, long type)
## reshape, reshape2, tidyr, ...
## melt() : wide -> long, cast() : long -> wide (데이터까지 재구성해줌)

install.packages("reshape2")
library(reshape2)

head(airquality) # 공기질 데이터, wide형, 컬럼별로 구성..
str(airquality)

m1 <- melt(airquality, id.vars = c("Month", "Day"))
View(m1)
View(airquality)

m2 <- melt(airquality, id.vars = c("Month", "Day"), 
           variable.name = "Climate_var", value.name = "Climate_val")
View(m2)

?dcast()  # 배열에 유리. formula : ~로 표시, 일관된 형태 유지, 통계와 관련되어 많이 사용
dc1 <- dcast(m2, Month+Day ~ Climate_var)
View(dc1)


## 예제1
data1 <- read.csv("../data/data.csv")
View(data1)  # 똑같은 값이 반복됨 -> long형

# 날짜별로 컬럼 생성해서 wide형으로 변경
d1 <- dcast(data1, Date~ Customer_ID+Buy )
d1 <- dcast(data1, Customer_ID ~ Date)



# 다시 long형으로 변경
d2 <- melt(d1, id.vars = "Customer_ID", variable.name = "Date", value.name = "Buy")


## 예제2
data2 <- read.csv("../data/pay_data.csv")
View(data2)  # long형

# product_type을 wide형으로 변경
d3 <- dcast(data2, user_id+pay_method~product_type)

#### 주제 : 극단적 선택의 비율은 어느 연령대가 가장 높은가? ####
# (사망 원인 통계)
# 부제 : 자살 방지를 위한 도움의 손길은 누구에게 더 지원해야 할까
# 결과를 도출할 때 정확한 수치가 동반되어야 함(남자 몇명 여자 몇명 중 남자가 몇명 더 많아...)

library(dplyr)
data <- read.csv("../data/2019_suicide.csv", skip = 1)
View(data)
str(data)

# 불필요한 컬럼 제거, 컬럼명 변경 #
data <- data[, -1]
names(data) <- c("성별", "연령", "사망자수", "사망률")

# 사망률 "-"를 0으로 변경 후 숫자형으로 변환 #
data$사망률 <- ifelse(data$사망률=="-", 0, data$사망률)
data$사망률 <- as.numeric(data$사망률)

#----- 성별로 사망자수 확인 -----#

d1 <- data %>% group_by(성별) %>% summarize(suicide_cnt=sum(사망자수))
d1
# 단순히 사망자수만 고려하면 남자가 더 많은 극단적 선택을 했음을 알 수 있다.


#----- 연령별로 사망자수 확인 -----#

d2 <- data %>% group_by(연령) %>% summarize(suicide_cnt=sum(사망자수)) %>% arrange(desc(suicide_cnt)) %>% head(5)
d2
# 45-49세인 연령대가 제일 많은 극단적 선택을 했음을 알 수 있다.


#----- 연령대와 성별을 같이 고려해 사망자수 확인 -----#

# 성별이 "계"인 행 지운 데이터를 다른 데이터 프레임에 담기 #
d3 <- data[!(data$성별=="계"), ]

d3 <- d3 %>% group_by(연령, 성별) %>% summarize(suicide_cnt=sum(사망자수)) %>% arrange(desc(suicide_cnt))
head(d3)
# 남자 55-59세가 가장 많이 극단적 선택을 했음을 알 수 있다
# 전체 성별에서 연령대만 봤을 땐 45-49세가 가장 많았지만 성별과 같이 비교해보면 달라짐을 알 수 있다.


#----- 성별마다 가장 많은 극단적 선택을 하는 연령대 뽑아내기 -----#
d3[d3$성별=="남자",] %>% head(1)
d3[d3$성별=="여자",] %>% head(1)

# 연령대만 고려한다면 45-49세인 분들에게 더 많은 도움의 손길을 드려야하고,
# 성별을 조합해 본다면 55-59세 남성분과 35-39세 여자분들에게 더 많은 도움의 손길을 드려야 한다는 결과를 낼 수 있다.


#### Database 연동 ####

# https://mariadb.com/kb/en/rmariadb/

install.packages("rJava")
install.packages("DBI")
install.packages("RMySQL")

library(RMySQL)

conn <- dbConnect(MySQL(), dbname="testdb", user="root", password="1111") # db연결
conn

dbListTables(conn)         # db에 있는 table 조회

result <- dbGetQuery(conn, "select * from emp")  # table 조회
result

dbListFields(conn, "emp")  # table에 있는 columns 조회

dbDisconnect(conn)         # db종료


## DML

dbSendQuery(conn, "delete from tbla")
dbGetQuery(conn, "select * from tbla")


## 파일로부터 데이터를 읽어들여 DB에 저장

file_score <- read.csv("../data/score.csv")
file_score

dbWriteTable(conn, "score", file_score, row.names=F)  # table 생성해서 데이터 넣어줌

result <- dbGetQuery(conn, "select * from score")
result

dbDisconnect(conn)

#### sqldf(추가로..) ####
# R에서 SQL 문법을 그대로 사용 가능하게 해주는 모듈
# DB연동은 아님!

# RMySQL연결 끊기
detach("package:RMySQL", unload = T)

install.packages("sqldf")
library(sqldf)

head(iris)
sqldf("select * from iris limit 6")

# 품종별로 sepal.length, petal.length를 10개 조회
# 단, petal.length가 큰 순으로 조회

iris %>% select(Species, Sepal.Length, Petal.Length) %>% group_by(Species) %>% 
  arrange(desc(Petal.Length)) %>% head(10)

sqldf('select Species, "Sepal.Length", "Petal.Length" from iris 
      group by Species order by "Petal.Length" limit 10')

# 중복된 품종 제거

unique(iris$Species)

sqldf('select distinct species from iris')

table(iris$Species)

sqldf('select Species, count(Species) as Count from iris group by Species')

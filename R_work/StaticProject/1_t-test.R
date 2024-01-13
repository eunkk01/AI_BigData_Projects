#### Power Analysis ####

# 적정한 표본의 개수를 산출
# cohen's d(effective size) : 두 집단의 평균차를 두 집단의 표준편차의 합으로 나눠준 것
# abs(mean.1-mean.2) / sqrt((sd.1^2 + sd.2^2)/2)

ky <- read.csv("../data/KY.csv", header=T)
View(ky)
table(ky$group) # 데이터 각 그룹마다 20개씩, 충분한가?

# 그룹별 점수 평균 구하기
mean.1 <- mean(ky$score[ky$group==1])
mean.2 <- mean(ky$score[ky$group==2])
cat(mean.1, mean.2)

sd.1 <- sd(ky$score[ky$group==1])
sd.2 <- sd(ky$score[ky$group==2])
cat(sd.1, sd.2)

effective_size <- abs(mean.1-mean.2) / sqrt((sd.1^2 + sd.2^2)/2)
effective_size

install.packages("pwr")
library(pwr)

pwr.t.test(d = effective_size, alternative = "two.sided", type = "two.sample", 
           power = 0.8, sig.level = 0.05)
# alternative = "two.sided" : 검정 종류("two.sided"(default), "greater" or "less")
# type = "two.samlpe" : 집단 타입("two.sample", "one.sample", "paired")
# power = 0.8 : 1종, 2종 오류에 가중치 부여(거의 0.8로 고정)
# sig.level = 0.05 : 유의수준

#### 사례1 : 두 집단의 평균 비교 ####

install.packages("moonBook")
library(moonBook)

# 경기도 소재 대학병원에서 2년동안 내원한 급성 관상 동맥 증후군 환자 데이터
head(acs)
str(acs)
summary(acs)

### 가설설정 ###
# 주제 : 두 집단(여성, 남성)의 나이 차이가 있는가?
# 귀무가설 : 남성과 여성의 평균 나이에 대해 차이가 없다
# 대립가설 : 남성과 여성의 평균 나이에 대해 차이가 있다

man.mean <- mean(acs$age[acs$sex=="Male"])
woman.mean <-mean(acs$age[acs$sex=="Female"])
cat(man.mean, woman.mean) # -> 차이가 유의미한가? 그저 우연인가?


### 정규분포 테스트 ###

moonBook::densityplot(age~sex, data=acs) # formula : 종속변수~독립변수

## 가설 설정 ##
# 주제 : 두 집단의 정규분포 여부
# 귀무가설 : 두 집단이 정규분포다
# 대립가설 : 두 집단이 정규분포가 아니다

# shapiro.test() : 정규분포인지 테스트 해주는 함수
shapiro.test(acs$age[acs$sex=="Male"])   # p-value=0.2 -> 정규분포O
shapiro.test(acs$age[acs$sex=="Female"]) # p-value=0.00000.. -> 정규분포X
# 정규분포가 아니므로 t-test 사용 불가
# but 데이터 수가 많아 중심극한정리에 의해 정규분포라 볼 수 있음


### 등분산 테스트 ###

## 가설 설정 ##
# 주제 : 두 집단의 등분산 여부
# 귀무가설 : 두 집단이 등분산이다
# 대립가설 : 두 집단이 등분산이 아니다

# var.test() : 등분산인지 테스트해주는 함수
var.test(age~sex, data=acs)  # p-value = 0.3866 -> 등분산O


### t-test ###

?t.test
t.test(age~sex, data=acs, alt="two.sided", var.equal=T)
# p-value < 2.2e-16 -> 0.05보다 작으므로 큰 차이가 존재. 따라서 귀무가설 기각.
# 성별에 따른 나이 차이가 존재한다.


### 정규분포가 아닐 때 : MWW ###

wilcox.test(age~sex, data=acs)
# p-value < 2.2e-16 -> 0.05보다 작으므로 큰 차이가 존재. 따라서 귀무가설 기각.


### 등분산이 아닐 때 : Weltch's t-test ###

t.test(age~sex, data=acs, alt="two.sided", var.equal=F) # var.equal=F : F로만 바꿔주면 됨
# p-value < 2.2e-16 -> 0.05보다 작으므로 큰 차이가 존재. 따라서 귀무가설 기각.

#### 사례2 : 집단이 하나인 경우 ####

## A회사의 건전지 수명이 1000시간일 때 무작위로 뽑은 10개의 건전지 수명에 대해 
## 샘플이 모집단과 다르다고 할 수 있는가?

# 귀무가설 : 표본의 평균은 모집단의 평균과 같다.
# 대립가설 : 표본의 평균은 모집단의 평균과 다르다.

a <- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

# 정규분포 테스트 #
shapiro.test(a) # p-value = 0.9781 -> 정규분포O

# t-test #
t.test(a, mu=1000, alt="two.sided") # mu=1000 : 모집단의 평균
t.test(a, mu=1000, alt="less")


## 어떤 학급의 수학 평균 점수가 55점이었다. 0교시 수업을 하고 다시 성적을 살펴 보았다.

b <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
length(b)

mean.b <- mean(b)
mean.b

# 정규분포 테스트 #
shapiro.test(b)  # p-value = 0.1058 -> 정규분포O

# t-test #
t.test(b, mu=55, alt="two.sided")
t.test(b, mu=55, alt="greater")
# p-value = 0.4046 -> 차이 존재X, 귀무가설 채택

#### 사례3 : 하나의 집단을 시간에 따라 비교 ####

# 약 투여 전후 수면 차이 데이터
str(sleep)
View(sleep)

## 주제 : 같은 집단에 대해 수면시간 증가량의 차이가 있는가?
# 귀무가설 : 차이 존재 X
# 대립가설 : 차이 존재 O

## Independent two sample ##
# 아이디 없이 서로 다른 두 집단이라 두고 테스트
sleep2 <- sleep[, -3]
View(sleep2)

tapply(sleep2$extra, sleep2$group, mean)    # -> 한번에 평균 구하기
mean.1 <- mean(sleep2$extra[sleep2$group==1])
mean.2 <- mean(sleep2$extra[sleep2$group==2])
cat(mean.1, mean.2)

# 정규분포 확인 #
shapiro.test(sleep2$extra[sleep2$group==1])   # p-value = 0.4079 -> 정규분포O
shapiro.test(sleep2$extra[sleep2$group==2])   # p-value = 0.3511 -> 정규분포O
# with(sleep2, shapiro.test(extra[group==2]))

# 등분산 확인 #
var.test(extra~group, data=sleep2)  # p-value = 0.7427 -> 등분산O

# t-test #
t.test(extra~group, data=sleep2, alt="two.sided", var.equal=T) # p-value = 0.07919
# p-value가 0.05보다 크므로 귀무가설 채택, 차이 존재 X

## paired sample ##
# 같은 집단이라 두고 테스트

# paired sample t-test #
t.test(extra~group, data=sleep, var.equal=T, paired=T) # p-value = 0.002833
# p-value가 0.05보다 작으므로 귀무가설 기각, 차이 존재 O


## 그래프 그리기
before = subset(sleep, group==1, extra)
before

after = subset(sleep, group==2, extra)
after

install.packages("PairedData") # Paired data를 합칠 때 사용하는 모듈
library(PairedData)

s_graph <- paired(before, after)
s_graph

plot(s_graph, type="profile") + theme_bw()
# type="profile" : 같은 집단의 변화를 알 수 있게 나타내줌
# theme_bw() : 배경 흰색으로, 그래프 선 검은색으로

#### 실습1 : 시와 군에 따라서 합계 출산율의 차이가 있는지 ####

## 주제 : 시와 군에 따라서 합계 출산율의 차이가 있는지 알아보자 ##
# 귀무가설 : 차이가 없다.
# 대립가설 : 차이가 있다.

mydata <- read.csv("../data/independent.csv")
# dummy : 0은 군을, 1은 시를 나타낸다.
str(mydata)
View(mydata)

library(moonBook)
moonBook::densityplot(birth_rate~dummy, data=mydata)

# 평균 확인하기 #
mean.1 <- mean(mydata$birth_rate[mydata$dummy==1])
mean.0 <- mean(mydata$birth_rate[mydata$dummy==0])
cat(mean.1, mean.0)

# 정규분포인지 확인 #
shapiro.test(mydata$birth_rate[mydata$dummy==1]) # p-value = 0.001476 -> 정규분포X
shapiro.test(mydata$birth_rate[mydata$dummy==0]) # p-value = 0.009702 -> 정규분포X

# MWW test #
wilcox.test(birth_rate~dummy, data=mydata) # p-value = 0.04152
# p-value가 0.05보다 작으므로 귀무가설 기각, 차이가 존재한다.


# 만약 정규분포 였다면..
var.test(birth_rate~dummy, data=mydata) # p-value = 6.118e-05
t.test(birth_rate~dummy, data=mydata, var.equal=F) # p-value = 0.01335 : welch t-test
# 만약 등분산도 맞다면..
t.test(birth_rate~dummy, data=mydata, var.equal=T) # p-value = 0.01507

#### 실습2 : 오토와 수동에 따라 연비 차이가 있는가 ####

## 주제 : 오토와 수동에 따라 연비 차이가 있는가?

# 기본 데이터셋에 있는 mtcars
# am : 0은 오토, 1은 수동

str(mtcars)

moonBook::densityplot(mpg~am, data=mtcars)

mean.0 <- mean(mtcars$mpg[mtcars$am==0])
mean.1 <- mean(mtcars$mpg[mtcars$am==1])
cat(mean.0, mean.1)

shapiro.test(mtcars$mpg[mtcars$am==0]) # p-value = 0.8987 -> 정규분포O
shapiro.test(mtcars$mpg[mtcars$am==1]) # p-value = 0.5363 -> 정규분포O

var.test(mpg~am, data=mtcars) # p-value = 0.06691 -> 등분산O
var.test(mtcars[mtcars$am==1, 1], mtcars[mtcars$am==0, 1])

t.test(mpg~am, data=mtcars, var.equal=T) # p-value = 0.000285
# t.test(mpg~am, data=mtcars, var.equal=F) # p-value = 0.001374
# p-value가 0.05보다 작으므로 귀무가설 기각, 즉 오토와 수동에 따른 연비 차이가 존재한다.

#### 실습3 : 쥐의 몸무게가 약 투약 전후 차이가 존재하는가 ####

## 주제 : 쥐의 몸무게가 약 투약 전후 차이가 존재하는가? ##

mydata <- read.csv("../data/pairedData.csv")
mydata

mean.b <- mean(mydata$before)
mean.a <- mean(mydata$After)
cat(mean.b, mean.a)

## long형으로 변환 1
library(reshape2)
mydata1 <- melt(mydata, id="ID", variable.name = "GROUP", value.name = "RESULT")
mydata1

## long형으로 변환 2
install.packages("tidyr")
library(tidyr)
mydata2 <- gather(mydata, key="GROUP", value="RESULT", -ID)
mydata2

moonBook::densityplot(RESULT~GROUP, data=mydata1)

shapiro.test(mydata1$RESULT[mydata1$GROUP=="before"]) # p-value = 0.2768
shapiro.test(mydata1$RESULT[mydata1$GROUP=="After"]) # p-value = 0.2894

# 같은 집단일 경우 등분산 확인은 pass~
# var.test(RESULT~GROUP, data=mydata1) # p-value = 0.1825

t.test(RESULT~GROUP, data=mydata1, paired=T) # p-value = 6.2e-09

## wide형으로 진행
t.test(mydata$before, mydata$After, paired=T) # p-value = 6.2e-09
# p-value가 0.05보다 작으므로 귀무가설 기각, 즉 약 투여 전후 몸무게 차이가 존재한다고 할 수 있다.

# 그래프 그리기 (long형으로 진행)
before <- subset(mydata1, GROUP=="before", RESULT)
after <- subset(mydata1, GROUP=="After", RESULT)

plot(paired(before, after), type="profile")

#### 실습4 : 시별로 2010년도와 2015년도의 출산율의 차이가 있는가 ####

## 주제 : 시별로 2010년도와 2015년도의 출산율의 차이가 있는가? ##

mydata <- read.csv("../data/paired.csv", fileEncoding = "euc-kr")
str(mydata)

mean.2010 <- mean(mydata$birth_rate_2010)
mean.2015 <- mean(mydata$birth_rate_2015)
cat(mean.2010, mean.2015)

myd1 <- melt(mydata, id=c("ID", "cities"), variable.name = "GROUP", value.name = "RESULT")
myd1

mydata1 <- gather(mydata, key="GROUP", value="RESULT", -c(ID, cities))
mydata1

shapiro.test(mydata1$RESULT[mydata1$GROUP=="birth_rate_2010"]) # p-value = 4.333e-05
shapiro.test(mydata1$RESULT[mydata1$GROUP=="birth_rate_2015"]) # p-value = 0.01947

wilcox.test(RESULT~GROUP, data=mydata1, paired=T) # p-value = 0.4513
# p-value가 0.05보다 크므로 귀무가설 채택, 즉 출산율의 차이가 없다.
# t.test(RESULT~GROUP, data=mydata1, paired=T)

before <- subset(mydata1, GROUP=="birth_rate_2010", RESULT)
after <- subset(mydata1, GROUP=="birth_rate_2015", RESULT)

plot(paired(before, after), type="profile")

#### 실습5 : 시험 회차별 점수 변화가 존재하는가 ####

# https://www.kaggle.com/kappernielsen/independent-t-test-example
mat <- read.csv("../data/student-mat.csv", header=T)
View(mat)
str(mat) # G1, G2, G3 : 수학시험 점수

## 주제1 : 남녀별로 각 시험에 대해 평균 차이가 존재하는가?
## 주제2 : 같은 사람에 대해 성적의 차이가 존재하는가?(G1과 G3의 차이 확인)

## 주제1 ##

# 남녀의 빈도수 확인 #
table(mat$sex)
summary(mat$G1)
summary(mat$G2)
summary(mat$G3)

library(dplyr)
mat %>% group_by(sex) %>% summarise(mean_g1 = mean(G1), mean_g2 = mean(G2), mean_g3 = mean(G3),
                                    sd_g1 = sd(G1), sd_g2 = sd(G2), sd_g3 = sd(G3))
# 편차가 커지는 것으로 보아 잘하는 사람과 못하는 사람의 차이가 커지고 있어보인다.

# G1 #
shapiro.test(mat$G1[mat$sex=="F"]) # p-value = 5.691e-05
shapiro.test(mat$G1[mat$sex=="M"]) # p-value = 0.01363

wilcox.test(mat$G1[mat$sex=="F"], mat$G1[mat$sex=="M"]) # p-value = 0.05957
wilcox.test(G1~sex, data=mat)
# p값이 0.05보다 크므로 귀무가설 채택, 즉 첫번째 시험에선 남녀 성적 차이가 없다.

# G2 #
shapiro.test(mat$G2[mat$sex=="F"]) # p-value = 7.68e-05
shapiro.test(mat$G2[mat$sex=="M"]) # p-value = 0.0002798

wilcox.test(mat$G2[mat$sex=="F"], mat$G2[mat$sex=="M"]) # p-value = 0.04874
wilcox.test(G2~sex, data=mat)
# p값이 0.05보다 작으므로 귀무가설 기각, 즉 두번째 시험에선 남녀 성적 차이가 존재한다.

# G3 #
shapiro.test(mat$G3[mat$sex=="F"]) # p-value = 7.168e-09
shapiro.test(mat$G3[mat$sex=="M"]) # p-value = 8.972e-08

wilcox.test(mat$G3[mat$sex=="F"], mat$G3[mat$sex=="M"]) # p-value = 0.04065
wilcox.test(G3~sex, data=mat)
# p값이 0.05보다 작으므로 귀무가설 기각, 즉 세번째 시험에선 남녀 성적 차이가 존재한다.

# 정규분포라 하면..
var.test(G1~sex, data=mat)
var.test(G2~sex, data=mat)
var.test(G3~sex, data=mat)

t.test(G1~sex, data=mat, var.equal=T, alt="two.sided") # p-value = 0.06825
t.test(G2~sex, data=mat, var.equal=T, alt="two.sided") # p-value = 0.07051
t.test(G3~sex, data=mat, var.equal=T, alt="two.sided") # p-value = 0.03987

moonBook::densityplot(G1~sex, data=mat)
moonBook::densityplot(G2~sex, data=mat)
moonBook::densityplot(G3~sex, data=mat)


## 주제2 ##

mat %>% summarise(mean_g1=mean(G1), mean_g3=mean(G3))

mydata <- gather(mat, key="GROUP", value="RESULT", "G1", "G3")
str(mydata)

t.test(RESULT~GROUP, data=mydata, paired=T)      # p-value = 0.0004291
wilcox.test(RESULT~GROUP, data=mydata, paired=T) # p-value = 0.3153


mat2 <- mat[c("G1", "G3")]
mat3 <- melt(mat2, variable.name = "GROUP", value.name = "RESULT")

moonBook::densityplot(RESULT~GROUP, data=mat3)

shapiro.test(mat3$RESULT[mat3$GROUP=="G1"]) # p-value = 2.454e-06
shapiro.test(mat3$RESULT[mat3$GROUP=="G3"]) # p-value = 8.836e-13

wilcox.test(RESULT~GROUP, data=mat3, paired=T) # p-value = 0.3153
# p값이 0.05보다 크므로 귀무가설 기각, 즉 첫번째 시험과 세번째 시험의 점수 차이가 존재한다.

before <- subset(mat3, GROUP=="G1", RESULT)
after <- subset(mat3, GROUP=="G3", RESULT)

plot(paired(before, after), type="profile")

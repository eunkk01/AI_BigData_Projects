#### 단일(단순) 회귀 분석 ####
# y = ax + b

str(women)

plot(weight ~ height, data=women)

fit <- lm(weight ~ height, data=women)
fit
#  (Intercept)       height  
# -87.52(절편)     3.45(기울기)

plot(weight ~ height, data=women)
abline(fit, col="blue")
# abline() : plot() 위에 그래프 추가

summary(fit)
# Multiple R-squared:  0.991  -> 결정계수, 1에 가까울수록 신뢰도 UP!
# Adjusted R-squared:  0.9903 -> 다중회귀분석 땐 이거 보기

cor.test(women$weight, women$height)
# 0.9954948 * 0.9954948 = 결정계수


## 설명력을 검증하기 위한 그래프 ##
plot(fit)

par(mfrow=c(2,2))
plot(fit)
# 1 : 선형성 확인, 0을 중심으로 아무 모양없이 모여있어야 선형성을 이룬다고 할 수 있음
# 2 : 정규분포 확인, 하나의 선에 데이터들이 가까이 모여있어야 정규분포를 이룬다고 할 수 있음
# 3 : 등분산성 확인, 아무 모양없이 자연스럽게 흩어져 있어야 함
# 4 : 이상치 확인(다중회귀분석때 이용)


## 다항 회귀 분석(Polynomial Regression) ##
fit1 <- lm(weight ~ height + I(height^2), data=women)
fit1
# (Intercept)       height  I(height^2)  
#   261.87818     -7.34832      0.08306

summary(fit1) # Multiple R-squared:  0.9995

plot(weight ~ height, data=women)
lines(women$height, fitted(fit1), col="red")

par(mfrow=c(2,2))
plot(fit1)

shapiro.test(resid(fit)) # p-value = 0.1866
shapiro.test(resid(fit1)) # p-value = 0.506 -> 보정한 게 정규분포에 더 가까움

#### 실습1 : 유치원 수가 많은 지역의 합계 출산율도 높은가 ####

## 주제 : 유치원 수가 많은 지역의 합계 출산율도 높은가? ##
##        또는 합계 출산율이 유치원 수의 영향을 받는가?

mydata <- read.csv("../data/regression.csv")
View(mydata)
# social_welfare : 사회복지시설, active_firms : 사업체수
# urban_park : 도시 공원,        doctor : 의사수
# tris : 폐수배출업소,           kindergarten : 유치원

str(mydata)

fit <- lm(birth_rate ~ kindergarten, data=mydata)
summary(fit)  # Multiple R-squared:  0.03945 -> 약하다..

par(mfrow=c(2,2))
plot(fit)

shapiro.test(resid(fit)) # p-value = 8.088e-05 -> 정규분포X

## 튜닝 : 데이터 조정 ##
fit1 <- lm(log(birth_rate) ~ log(kindergarten), data=mydata)
summary(fit1)  # Multiple R-squared:  0.04382 -> 미세하게 증가

plot(fit1)

shapiro.test(resid(fit1)) # p-value = 0.2227 -> 정규분포O


fit2 <- lm(birth_rate ~ dummy, data=mydata)
summary(fit2)  # Multiple R-squared:  0.03811

#### 다중 회귀 분석 ####
# y = a1x1 + a2x2 + ... + anxn + b

house <- read.csv("../data/kc_house_data.csv", header=T)
str(house)
# price : 가격(종속 변수)


## 가설 : 거실의 크기가 크면 매매 가격이 비쌀 것이다. ##

reg1 <- lm(price ~ sqft_living, data=house)
summary(reg1)       # Multiple R-squared:  0.4929
#               Estimate(+-관계 확인)    Pr(>|t|)
# sqft_living        280.624               <2e-16
# 280.624 : x값이 1중가할 때 y값은 280증가, 즉 1평마다 280 증가


## sqft_living, bathrooms, sqft_lot, floors 4가지 먼저 고려해보자 ##
## -> 집의 크기와 변수들이 모두 관계가 깊어보임(다중 공산성 의심됨)

attach(house)
x <- cbind(sqft_living, bathrooms, sqft_lot, floors)
cor(x)  # 독립변수들의 상관관계 확인
#             sqft_living  bathrooms     sqft_lot       floors
# sqft_living   1.0000000 0.75466528  0.172825661  0.353949290
# bathrooms     0.7546653 1.00000000  0.087739662  0.500653173
# sqft_lot      0.1728257 0.08773966  1.000000000 -0.005200991
# floors        0.3539493 0.50065317 -0.005200991  1.000000000

cor(x, price) # 종속변수와 독립변수들의 상관관계 확인
# sqft_living 0.70203505
# bathrooms   0.52513751
# sqft_lot    0.08966086
# floors      0.25679389

reg2 <- lm(price ~ sqft_living + floors, data=house) # 층수 추가
summary(reg2)  # Adjusted R-squared:  0.4929
#              Estimate     Pr(>|t|)    
# sqft_living    279.28    <2e-16 ***
# floors        6458.26    0.0667 .  -> 예측값 +로 유의미함, but p-value가 애매함

# 조절변수 추가해보자
reg2_1 <- lm(price ~ sqft_living + floors + sqft_living:floors, data=house) 
summary(reg2_1)  # Adjusted R-squared:  0.4994 
#                      Estimate  Pr(>|t|)    
# sqft_living         1.727e+02  <2e-16 ***
# floors             -1.164e+05  <2e-16 *** -> 유의미했던 예측값이 -로 변해버림 
# sqft_living:floors  6.306e+01  <2e-16 ***         why? 다중 공산성 확인해보자

## 다중 공산성 확인 ##
install.packages("car")
library(car)
vif(reg2_1)
# sqft_living     floors  sqft_living:floors 
#   11.968867   6.112919           21.987677 > 10 -> 독립변수끼리 연관이 높아 같이 사용 불가


## 다른 변수들을 가지고 시도 ##
x <- cbind(floors, sqft_above, sqft_basement)
cor(x)
#                   floors  sqft_above sqft_basement
# floors         1.0000000  0.52388471   -0.24570454
# sqft_above     0.5238847  1.00000000   -0.05194331
# sqft_basement -0.2457045 -0.05194331    1.00000000

x <- cbind(floors, bedrooms)
cor(x) # 0.1754289 -> 낮은 편이다!

reg3 <- lm(price ~ floors + bedrooms, data=house)
summary(reg3)  # Adjusted R-squared:  0.1374
#             Estimate  Pr(>|t|)    
# floors        142188   < 2e-16 ***
# bedrooms      107234   < 2e-16 
vif(reg3)     # -> 낮다!


unique(waterfront)
x <- cbind(floors, bedrooms, waterfront) # 변수추가
cor(x)
cor(x, price)

reg4 <- lm(price ~ floors + bedrooms + waterfront, data=house)
summary(reg4) # Adjusted R-squared:  0.2067 -> 증가!
vif(reg4)     # -> 낮다!

reg5 <- lm(price ~ floors + bedrooms + waterfront + bedrooms:waterfront, data=house)
summary(reg5) # 조절변수 추가했더니 waterfront의 예측값이 -로 변함
vif(reg5)     # 다중 공산성이 높아버림.. -> 철회

reg6 <- lm(price ~ floors + bedrooms + waterfront + floors:waterfront, data=house)
summary(reg6)
vif(reg6)     # 높긴 하지만 좋은 영향을 주므로 안고가도 괜찮! 높다고 무조건 버릴 필요는 없음!


#### 실습2 : 선형모델 선택 과정(무엇이 범죄율에 영향을 미치는가) ####

## 주제 : 무엇이 범죄율에 영향을 미칠까? ##

library(car)
View(state.x77)
# Population : 인구수, Illiteracy : 문맹률, Life Exp : 생활지수, Frost : 결빙률, area : 면적

states <- as.data.frame(state.x77[ , c("Murder", "Population", "Illiteracy", "Income", "Frost")])
View(states)

fit <- lm(Murder ~ ., data=states) # .으로 표시하면 모든 변수를 독립변수로 지정 가능
summary(fit)  # Adjusted R-squared:  0.5285
#              Estimate Std. Error t value Pr(>|t|)    
# Population  2.237e-04  9.052e-05   2.471   0.0173 *
# Illiteracy  4.143e+00  8.744e-01   4.738 2.19e-05 *** -> 가장 큰 관련
# Income      6.442e-05  6.837e-04   0.094   0.9253     -> 관련 없어보이지만 냅둬보기
# Frost       5.813e-04  1.005e-02   0.058   0.9541     -> 관련 없어보이지만 냅둬보기


##--------------- 다중 공산성 확인 ---------------##
sqrt(vif(fit)) # 다중 공산성은 문제 없다~


##----------------- 이상치 확인 ------------------##
## 이상치 확인할 수 있는 그래프 : influencePlot() ##
influencePlot(fit, id=list(method="identify"))
# y축 값이 0을 기준으로 표준편차가 2배이상 큰 값을 이상치로 간주
# x축 값이 0을 기준으로 큰 지레점보다 큰 값을 이상치로 간주


##-------------- 회귀모형의 교정 --------------##
## 조건 확인(선형성, 정규분포, 등분산, 독립성) ##

par(mfrow=c(2,2))
plot(fit)
# 1 : 선형성 -> 무작위로 흩어져 있으므로 만족
# 2 : 정규분포 -> 만족
# 3 : 등분산성 -> 무작위로 흩어져 있어 보이므로 만족
# 4 : 이상치 -> 만족


# 정규성을 만족하지 않는다면.. : 결과변수(종속변수)에 람다승 #
# 주로 -2, -1, -0.5, 0, 0.5, 1, 2의 값으로 람다승
shapiro.test(resid(fit)) # p-value = 0.6672

# powerTransform() : 어떤 값을 람다승할지 알려주는 함수
powerTransform(states$Murder)          # 0.605542 
summary(powerTransform(states$Murder)) # 추가 정보 확인


# 선형성을 만족하지 않는다면.. #

# boxTidwell() : 어떤 값을 람다승할지 알려주는 함수
boxTidwell(Murder ~ Population + Illiteracy, data=states)
#            MLE of lambda Score Statistic (t) Pr(>|t|)
# Population       0.86939             -0.3228   0.7483
# Illiteracy       1.35812              0.6194   0.5388

states$Population <- states$Population ^ 0.85
states$Illiteracy <- states$Illiteracy ^ 1.35
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)


# 등분산성을 만족하지 않는다면.. #

# ncvTest() : 등분산성 확인해주는 함수
ncvTest(fit)  # p = 0.23367
# spreadLevelPlot() : 어떤 값을 람다승할지 알려주는 함수
spreadLevelPlot(fit)  # 1.209626


##----------- 회귀모형의 선택 -----------##
## 가장 영향을 많이 끼치는 독립변수 선택 ##
# 1. Backward Stepwise Regression : 모든 독립변수를 대상으로 하나씩 빼는 방법 #
# 2. Forward Stepwise Regression : 독립변수를 하나씩 추가하면서 테스트하는 방법 #
# -> AIC값이 나빠지면 그 직전의 것을 선택하기 때문에
#    미처 비교해보지 못한 변수가 발생하는 아쉬움이 있음
# 3. All Subset Regression : 모든 독립변수를 하나씩 다 해보는 방법(설명계수로 판별) #

# AIC(Akaike's Information Criterion, 측정 기준값) : 작을수록 좋음

# AIC 확인 #
fit1 <- lm(Murder ~ ., data=states)
summary(fit1) # Adjusted R-squared:  0.5327 
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2) # Adjusted R-squared:  0.5518

AIC(fit1, fit2)
# fit1  6 241.1874
# fit2  4 237.2824 -> fit2가 더 작으므로 변수 2개로 하는게 더 좋음

# Backward #
full.model <- lm(Murder ~ ., data=states)
reduce.model <- step(full.model, direction = "backward")
reduce.model
# lm(formula = Murder ~ Population + Illiteracy, data = states)

# Forward #
min.model <- lm(Murder ~ 1, data=states)
fwd.model <- step(min.model, direction = "forward", 
                  scope = (Murder ~ Population + Illiteracy + Income + Frost ))
fwd.model
# lm(formula = Murder ~ Illiteracy + Population, data = states)

# All Subset #
install.packages("leaps")
library(leaps)

leap <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, 
                   data=states, nbest=4)
leap

par(mfrow=c(1,1))
plot(leap, scale="adjr2")  # scale="adjr2" : 범위를 설명계수로 지정
# 설명계수가 클수록 좋은 것

#### 실습3 : 독립변수들이 출산률과 관계가 있는가 ####

# 정규성, 등분산성, 다증공선성 검증
# 독립변수들이 출산률과 관계가 있는가?
# 가장 영향력있는 변수들은 무엇인가?

mydata <- read.csv("../data/regression.csv", fileEncoding="euc-kr")
View(mydata)

mydata <- mydata[, -1]

fit <- lm(birth_rate ~ ., data=mydata)
summary(fit) # Adjusted R-squared:  0.1089


##-------------- 회귀모형의 교정 --------------##
str(mydata)
par(mfrow=c(2,2))
plot(fit)

# backward #
full.model <- lm(birth_rate ~ ., data=mydata)
back.model <- step(full.model, direction = "backward", trace = 0)
back.model
# lm(formula = birth_rate ~ social_welfare + active_firms + pop + 
#                               tris + kindergarten, data = mydata)

fit2 <- lm(formula = birth_rate ~ social_welfare + active_firms + pop + 
             tris + kindergarten, data = mydata)
summary(fit2) # Adjusted R-squared:  0.1184
plot(fit2)


# 정규분포 교정 #
shapiro.test(resid(fit2))         # p-value = 0.0001742 -> 정규분포X
powerTransform(mydata$birth_rate) # -1.056671

fit3 <- lm(formula = birth_rate^-1 ~ social_welfare + active_firms + pop + 
             tris + kindergarten, data = mydata)
summary(fit3)             # Adjusted R-squared:  0.1128
shapiro.test(resid(fit3)) # p-value = 0.4624 -> 정규분포 만족
plot(fit3)


# 다중공선성 확인 #
sqrt(vif(fit))
sqrt(vif(fit2))
sqrt(vif(fit3))   # 전부 문제 없음


# 등분산 교정 #
ncvTest(fit)  # p = 0.00030067
ncvTest(fit2) # p = 0.0009955
ncvTest(fit3) # p = 0.03703

spreadLevelPlot(fit3)  # 3.364431
fit4 <- lm(formula = (birth_rate^-1)^3.36 ~ social_welfare + active_firms + pop + 
             tris + kindergarten, data = mydata)
summary(fit4) # Adjusted R-squared:  0.0812 -> 설명계수가 안좋아짐 -> 건너뛰자..
ncvTest(fit4) # p = 0.31951

fit5 <- lm(formula = log(birth_rate) ~ social_welfare + active_firms + pop + 
             tris + kindergarten, data = mydata) 
summary(fit5) # Adjusted R-squared:  0.1189

#### 실습4 : 공공자전거 분석(R버전) ####

mydata <- read.csv("../data/SeoulBikeData.csv")
View(mydata)
str(mydata)
summary(mydata)

## 1. 시간대별로 평균 몇대가 대여 되었을까? ##
library(dplyr)
result1 <- mydata %>% group_by(Hour) %>% summarise(mean_rent = mean(Rented.Bike.Count))


## 2. 위의 결과를 시각화 ##
# annotate() : 그래프 위에 텍스트 추가
library(ggplot2)
ggplot(data=result1, aes(x=Hour, y=mean_rent)) + geom_line(color="blue", size=1.5) + 
  geom_vline(aes(xintercept = 8), color="red", size=1.5) +
  geom_vline(aes(xintercept = 18), color="red", size=1.5) +
  annotate(geom="text", x=6, y=1100, label="출근", size=8) +
  annotate(geom="text", x=16, y=1500, label="퇴근", size=8)


## 3. 선형 회귀 ##
attach(mydata)
reg <- lm(Rented.Bike.Count ~ ., data=mydata)
summary(reg) # Adjusted R-squared:  0.6599 -> 전처리 하면 더 올라갈 듯

## 온도가 23도일 때 자전거는 몇 대정도 대여될까? 이런 거 해보기


#### 로지스틱 회귀 분석 ####
# 일반화 선형 모델 : glm()

titanic <- read.csv("../data/train.csv", header=T)
View(titanic)

# 종속변수 : Survived(1:생존, 0:사망)
# 독립변수 : Pclass(1st, 2nd, 3rd로 파생변수 생성)

titanic$Pclass1 <- ifelse(titanic$Pclass==1, 1, 0)
titanic$Pclass2 <- ifelse(titanic$Pclass==2, 1, 0)
titanic$Pclass3 <- ifelse(titanic$Pclass==3, 1, 0)
View(titanic)

titanic$Pclass4 <- ifelse(titanic$Pclass==3, 0, 1)

reg1 <- lm(Survived ~ Pclass2 + Pclass3, data=titanic)
summary(reg1)
# 0.62963 - 0.15680 -> 0.47283
# 0.62963 - 0.38727 -> 0.24236

reg2 <- glm(Survived ~ Pclass2 + Pclass3, data=titanic, family=binomial)
summary(reg2)

(exp(0.5306)-1)*100  # 69.9952
(exp(-0.6394)-1)*100 # -47.23911
(exp(-1.6704)-1)*100 # -81.18282

# 절편(intercept): 독립변수가 모두 0일때 종속변수 y의 값.

# Age, Fare, Gender, SibSp
titanic$GenderFemale <- ifelse(titanic$Sex == "female", 1, 0)
titanic$GenderMale <- ifelse(titanic$Sex == "male", 1, 0)

reg_sex <- glm(Survived ~ GenderFemale, data=titanic, family=binomial)
summary(reg_sex)


#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   0.5306     0.1409   3.766 0.000166 ***
#   Pclass2      -0.6394     0.2041  -3.133 0.001731 ** 
#   Pclass3      -1.6704     0.1759  -9.496  < 2e-16 ***
# 
#               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -1.1398     0.1053 -10.822  < 2e-16 ***
#   Pclass1       1.6704     0.1759   9.496  < 2e-16 ***
#   Pclass2       1.0310     0.1814   5.684 1.31e-08 ***
#   Pclass3           NA         NA      NA       NA 

#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -1.4571     0.1064  -13.70   <2e-16 ***
#   GenderFemale   2.5137     0.1672   15.04   <2e-16 ***
#   GenderMale         NA         NA      NA       NA

#                Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -1.4571     0.1064  -13.70   <2e-16 ***
#   GenderFemale   2.5137     0.1672   15.04   <2e-16 ***

unique(titanic$SibSp)
titanic$SibSp0 <- ifelse(titanic$SibSp==0, 1, 0)
titanic$SibSp1 <- ifelse(titanic$SibSp==1, 1, 0)
titanic$SibSp2 <- ifelse(titanic$SibSp==2, 1, 0)
titanic$SibSp3 <- ifelse(titanic$SibSp==3, 1, 0)
titanic$SibSp4 <- ifelse(titanic$SibSp==4, 1, 0)
titanic$SibSp5 <- ifelse(titanic$SibSp==5, 1, 0)
titanic$SibSp8 <- ifelse(titanic$SibSp==8, 1, 0)


reg3 <- glm(Survived ~ Age + Fare + GenderFemale + 
              SibSp1 + SibSp2 + SibSp3 + SibSp4 + SibSp5,
            data = titanic, family = binomial)
summary(reg3)

#                  Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -1.036889   0.252444  -4.107 4.00e-05 ***
#   Age           -0.022406   0.007236  -3.096  0.00196 ** 
#   Fare           0.014946   0.002945   5.075 3.87e-07 ***
#   GenderFemale   2.411534   0.197999  12.180  < 2e-16 ***
#   SibSp1         0.050573   0.223206   0.227  0.82075    
#   SibSp2        -0.664773   0.527546  -1.260  0.20763    
#   SibSp3        -2.461568   0.825169  -2.983  0.00285 ** 
#   SibSp4        -2.091337   0.728233  -2.872  0.00408 ** 
#   SibSp5       -15.807048 582.928110  -0.027  0.97837 

table(titanic$Survived, titanic$SibSp)
210/(398+210)
112/(112+97)

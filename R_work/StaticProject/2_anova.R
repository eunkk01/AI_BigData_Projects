#### type1 : One Way ANOVA ####

library(moonBook)

View(acs)
str(acs)

### 진단 결과(요인)에 따른 저밀도 콜레스테롤 수치(LDLC)를 알고 싶다. ###
# 진단결과(Dx) : STEMI(급성심근경색), NSTEMI(만성심근경색), unstable angina(협심증)
# 진단결과(요인) 3가지 -> ANOVA

moonBook::densityplot(LDLC~Dx, data=acs)

# 정규분포 확인 #
shapiro.test(acs$LDLC[acs$Dx=="NSTEMI"])          # p-value = 1.56e-08
shapiro.test(acs$LDLC[acs$Dx=="STEMI"])           # p-value = 0.6066
shapiro.test(acs$LDLC[acs$Dx=="Unstable Angina"]) # p-value = 2.136e-07

# 정규분포 확인하는 또 다른 방법 : ANOVA일 때 -> aov()
out <- aov(LDLC~Dx, data=acs) # Residuals :잔차
# resid() : 잔차만 뽑아오기 -> 잔차가 정규분포를 이루는지 확인
shapiro.test(resid(out))     # p-value = 1.024e-11

# 등분산 확인 #
bartlett.test(LDLC~Dx, data=acs) # p-value = 0.1857

## ANOVA 검정(정규분포 & 등분산인 경우) : aov() ##
out <- aov(LDLC~Dx, data=acs)
summary(out) # Pr(>F) = 0.00377
# p-value가 0.05보다 작으므로 귀무가설 기각, 즉 차이가 존재한다.

## 연속변수가 아니거나 정규분포가 아닌 경우 : kruskal.test() ##
kruskal.test(LDLC~Dx, data=acs) # p-value = 0.004669

## 정규분포지만 등분산이 아닌 경우 : oneway.test() ##
oneway.test(LDLC~Dx, data=acs, var.equal = F) # p-value = 0.007471


### 사후검정 ###

## aov() 사용했을 경우(정규분포이고 등분산) : TukeyHSD() ##
TukeyHSD(out)
# STEMI-NSTEMI           : p adj = 0.0599378
# Unstable Angina-NSTEMI : p adj = 0.0024227
# Unstable Angina-STEMI  : p adj = 0.4419434
# Unstable Angina-NSTEMI만 p값이 0.05보다 작으므로 둘 사이에만 차이가 존재한다.

## kruskal.test() 사용했을 경우(연속변수 or 정규분포가 아닌 경우) : kruskalmc() ##
install.packages("pgirmess")
library(pgirmess)
kruskalmc(acs$LDLC, acs$Dx)
# NSTEMI-STEMI           : stat.signif = FALSE
# NSTEMI-Unstable Angina : stat.signif = TRUE
# STEMI-Unstable Angina  : stat.signif = FALSE

## oneway.test() 사용했을 경우(정규분포지만 등분산이 아닌 경우) ##
install.packages("nparcomp")
library(nparcomp)
result <- mctp(LDLC~Dx, data=acs)
summary(result)
# 2 - 1 : p.Value = 0.174758984
# 3 - 1 : p.Value = 0.002971895
# 3 - 2 : p.Value = 0.195778457


#### 실습1 : 품종별로 Sepal.width의 평균 차이가 있는가? ####

## 주제 : 품종별로 Sepal.width의 평균 차이가 있는가? ##
# 만약 있다면 어느 품종과 차이가 있는가 #

str(iris)

moonBook::densityplot(Sepal.Width~Species, data=iris)

## 조건 확인 ##
out = aov(Sepal.Width~Species, data=iris)
shapiro.test(resid(out)) # p-value = 0.323 -> 정규분포O

bartlett.test(Sepal.Width~Species, data=iris) # p-value = 0.3515 -> 등분산O

## ANOVA 검정 ##
out <- aov(Sepal.Width~Species, data=iris)
summary(out) # Pr(>F) = <2e-16
# p값이 0.05보다 작으므로 귀무가설 기각, 즉 차이가 존재한다.

## 사후검정 ##
TukeyHSD(out)
# versicolor-setosa    : p adj = 0.0000000
# virginica-setosa     : p adj = 0.0000000
# virginica-versicolor : p adj = 0.0087802
# 3가지 경우 모두 차이가 존재한다. 그 중 setosa와 붙은 것들이 큰 차이를 보인다.

#### 실습2 : 시, 군, 구에 따라서 합계 출산율의 차이가 있는가? ####

## 주제 : 시, 군, 구에 따라서 합계 출산율의 차이가 있는가? ##
# 있다면 어느 것과 차이가 있는가 #

mydata <- read.csv("../data/anova_one_way.csv", fileEncoding = "euc-kr")

str(mydata)
View(mydata)

moonBook::densityplot(birth_rate~ad_layer, data=mydata)

## 조건 학인 ##
out <- aov(birth_rate~ad_layer, data=mydata)
shapiro.test(resid(out)) # p-value = 5.788e-07 -> 정규분포X

## kruskal 검정 ##
kruskal.test(birth_rate~ad_layer, data=mydata) # p-value < 2.2e-16 -> 귀무가설 기각, 즉 차이 존재.
summary(out) # Pr(>F) = <2e-16

## 사후검정 ##
kruskalmc(mydata$birth_rate, mydata$ad_layer)
# 자치구-자치군 : stat.signif = TRUE
# 자치구-자치시 : stat.signif = TRUE
# 자치군-자치시 : stat.signif = FALSE
TukeyHSD(out)

library(ggplot2)
ggplot(mydata, aes(birth_rate, ad_layer, col="ad_layer")) + geom_boxplot()

#### 실습3 : 지불 방식별로 총 지불 금액이 차이가 있는가? ####

# 실습데이터 : http://www.kaggle.com
library(dplyr)
telco <- read.csv("../data/Telco-Customer-Churn.csv", header=T)
str(telco)
View(telco)
# Contract : 결제 방식, PaymentMethod : 지불방식, TotalCharges : 총 지불금액, Churn : 해지여부

## 주제 : 지불 방식별로 총 지불 금액이 차이가 있는가? ##
# 있다면 무엇과 차이가 있는가 #

# 지불방식(PaymentMethod) : Electronic check, Mailed check, Bank transfer, Credit card
# 총지불금액(TotalCharges)

# 값 지불 방식 별로 인원수와 평균 금액 조회 #
telco2 <- telco %>% filter(!is.na(TotalCharges))

telco2 %>% group_by(telco2$PaymentMethod) %>% 
  summarise(mean_charges = mean(TotalCharges), count = n())

telco %>% select(PaymentMethod, TotalCharges) %>%
  group_by(PaymentMethod) %>% summarise(count=n(), mean=mean(TotalCharges, na.rm=T))

moonBook::densityplot(TotalCharges~PaymentMethod, data=telco2)

## 조건 확인 ##
shapiro.test(telco2$TotalCharges[telco2$PaymentMethod=="Bank transfer (automatic)"]) # p-value < 2.2e-16
shapiro.test(telco2$TotalCharges[telco2$PaymentMethod=="Credit card (automatic)"])   # p-value < 2.2e-16
shapiro.test(telco2$TotalCharges[telco2$PaymentMethod=="Electronic check"])          # p-value < 2.2e-16
shapiro.test(telco2$TotalCharges[telco2$PaymentMethod=="Mailed check"])              # p-value < 2.2e-16
# shapiro.test는 5000개 이상이면 정규분포라 간주해서 검사 안헤줌 -> 앤더슨 달링 테스트 이용 #


# 정규분포라 가정하고 .. #
## 등분산 테스트 ##
bartlett.test(TotalCharges~PaymentMethod, data=telco2) # p-value < 2.2e-16
## welch's anova ##
oneway.test(TotalCharges~PaymentMethod, data=telco2, var.equal=F) # p-value < 2.2e-16
## 사후검정 ##
library(nparcomp)
result <- mctp(TotalCharges~PaymentMethod, data=telco2)
summary(result)
plot(result)


# 정규분포가 아니면 .. #
## kruskal 검증 ## 
kruskal.test(TotalCharges~PaymentMethod, data=telco2) # p-value < 2.2e-16

## 사후검정 ##
kruskalmc(telco2$TotalCharges, telco2$PaymentMethod)
# Bank transfer (automatic)-Credit card (automatic) : stat.signif = FALSE
# Bank transfer (automatic)-Electronic check        : stat.signif = TRUE
# Bank transfer (automatic)-Mailed check            : stat.signif = TRUE
# Credit card (automatic)-Electronic check          : stat.signif = TRUE
# Credit card (automatic)-Mailed check              : stat.signif = TRUE
# Electronic check-Mailed check                     : stat.signif = TRUE

#### type2 : Two Way ANOVA ####

mydata <- read.csv("../data/anova_two_way.csv")
str(mydata)
View(mydata)

## 조건 확인 ##
out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, data=mydata)
# ad_layer:multichild -> 교차변수를 추가해줘야 둘을 같이 묶어 확인한 결과도 출력할 수 있음
shapiro.test(resid(out)) # p-value = 2.862e-06

summary(out)
# ad_layer            : Pr(>F) = <2e-16
# multichild          : Pr(>F) = 0.1466
# ad_layer:multichild : Pr(>F) = 0.0263 -> 두가지 요인을 같이 비교한 결과

## (정규분포라 가정하고)사후검정 ##
result <- TukeyHSD(out)
result

## 구가 NO일 때 NO인 것과 차이를 보인다.
#자치군:NO-자치구:NO    0.33269631  0.22321791  0.44217472 0.0000000
#자치시:NO-자치구:NO    0.25969520  0.14704477  0.37234563 0.0000000
## 군이 YES일때 NO인 것 모두와 차이를 보인다.
## ->다자녀 지원제도를 시행하는 군과 하지 않는 구, 군, 시의 차이를 알아본다.
#자치군:YES-자치구:NO   0.68234375  0.34779759  1.01688991 0.0000002
#자치군:YES-자치군:NO   0.34964744  0.01687205  0.68242282 0.0331697
#자치군:YES-자치시:NO   0.42264855  0.08881617  0.75648093 0.0045349
#자치군:YES-자치구:YES  0.64495000  0.20951050  1.08038950 0.0004339
#자치시:YES-자치군:YES -0.47241667 -0.89141852 -0.05341481 0.0170592

## 군은 제도를 시행 유무와 상관없이 시행하는 시, 구와 차이가 나타나지 않는다.
## 즉 시, 구는 제도 시행 유무가 출산율에 큰 영향을 미치지 않는다고 볼 수 있다.
#자치구:YES-자치군:NO  -0.29530256 -0.59475532  0.00415019 0.0557066
#자치구:YES-자치구:NO   0.03739375 -0.26402560  0.33881310 0.9992350
#자치시:YES-자치구:NO   0.20992708 -0.06721660  0.48707076 0.2524103
#자치시:YES-자치군:NO  -0.12276923 -0.39777277  0.15223431 0.7938268
#자치구:YES-자치시:NO  -0.22230145 -0.52292838  0.07832548 0.2778173
#자치시:YES-자치시:NO  -0.04976812 -0.32604976  0.22651353 0.9954370
#자치시:NO-자치군:NO   -0.07300111 -0.18027855  0.03427632 0.3709479
#자치시:YES-자치구:YES  0.17253333 -0.22052525  0.56559192 0.8052735

## 시와 구는 지원 유무와 상관없이 차이가 나타나지 않지만
## 군에 다자녀 정책을 지원했을 때에는 효과를 보임을 알 수 있다.

#### 실습4 : 지불 방식과 계약기간에 따라 총 지불 금액의 차이가 있는가? ####

telco <- read.csv("../data/Telco-Customer-Churn.csv", header=T)
str(telco)
View(telco)

# 원인(독립)변수 : PaymentMethod, Contract
# 결과(종속)변수 : TotalCharges

out <- aov(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract, data=telco)

moonBook::densityplot(TotalCharges~PaymentMethod, data=telco)
moonBook::densityplot(TotalCharges~Contract, data=telco)

bartlett.test(TotalCharges~PaymentMethod, data=telco)  # p-value < 2.2e-16
bartlett.test(TotalCharges~Contract, data=telco)       # p-value < 2.2e-16

oneway.test(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract, 
            data=telco, var.equal = F)                 # p-value < 2.2e-16

out <- aov(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract, data=telco)
summary(out)
# PaymentMethod           Pr(>F) : <2e-16 ***
# Contract                Pr(>F) : <2e-16 ***
# PaymentMethod:Contract  Pr(>F) : <2e-16 ***

TukeyHSD(out)

ggplot(telco, aes(x=PaymentMethod, y=TotalCharges, col=Contract)) + geom_boxplot()
# Two year일때와 Month-to-month일 때가 지불 방식과 무관하게 총 지불금액의 차이가 크게 나타난다.
# Mailed check와 Electronic check일때의 총 지불금액 차이가 가장 크다.
# Bank transfer (automatic)와 Credit card (automatic)는 계약 기간마다 경향이 비슷다.
# 즉, Two year면서 Electronic check로 가입을 하는 고객들이 많은 금액을 지불한다고 보여지므로 이쪽을 권유해야 한다.
# 반대로 Month-to-monthd이면서 Mailed check인 고객들은 금방 해지하는 경향이 크므로 가입시 피하도록 권유해야 한다.


#### type3 : Repeated Measure ANOVA ####

## 주제 : 6명을 대상으로 관찰했을 때 운동능력향상에 차이가 있는가? ##

df <- data.frame()
df <- edit(df)
df

means <- c(mean(df$pre), mean(df$after3m), mean(df$after6m))
means

plot(means, type="o", lty=2, col=2)


install.packages("car")
library(car)

?Anova

multimodels <- lm(cbind(df$pre, df$after3m, df$after6m)~1)    # 선형회귀모델 구하는 함수
trials <- factor(c("pre", "after3m", "after6m"), ordered = F) # 변수명을 범주형으로 바꿔주기

## S3 method for class 'mlm' ##
model1 <- Anova(multimodels, idata=data.frame(trials), idesign=~trials, type="III")
model1

summary(model1, multivariate=F)
# trials : Pr(>F) = 0.001886 => 차이가 존재함
# Mauchly Tests for Sphericity : p-value = 0.18795 > 0.05 => 구형성 타당(신뢰할만한 결과임)
# Greenhouse-Geisser and Huynh-Feldt Corrections : GG eps = 0.63838, HF eps = 0.760165


## 사후검정 ##

# long형으로..
library(tidyr)
dflong <- gather(df, key = "id", value = "score", -id)
dflong

with(dflong, pairwise.t.test(score, id, paired = T, p.adjust.method = "bonferroni"))
#         after3m after6m
# after6m 0.0053  -       -> 3m과 6m 차이가 존재
# pre     0.4842  0.0305  -> 이전과 3m은 차이 없지만 6m과는 차이 존재

# aov() -> 독립성 깨져도 구형성 타당한지 확인 불가, 정규분포인지 확인은 가능
out <- aov(score~id, data=dflong)
shapiro.test(resid(out)) # p-value = 0.3638 -> 정규분포

TukeyHSD(out) # -> 보정 필요(같은 집단을 여러번 비교해 독립성이 깨져서)


#### 실습5 ####

rm <- read.csv("../data/onewaysample.csv", header=T)
View(rm)

rm <- rm[, 2:6]

means <- c(mean(rm$score0), mean(rm$score1), mean(rm$score3), mean(rm$score6))
means

plot(means, type="o", lty=2, col=2)

multimodels <- lm(cbind(rm$score0, rm$score1, rm$score3, rm$score6)~1)   # 선형회귀모델
trials <- factor(c("score0", "score1", "score3", "score6"), ordered = F) # 범주형으로 바꿔주기

model1 <- Anova(multimodels, idata=data.frame(trials), idesign=~trials, type="III")
model1

summary(model1, multivariate=F)
# trials : Pr(>F) = 1.473e-14
# Mauchly Tests for Sphericity : p-value = 0.25494

rmlong <- gather(rm, key="id", value="score", -id)
rmlong

with(rmlong, pairwise.t.test(score, id, paired = T, p.adjust.method = "bonferroni"))
#        score0  score1  score3 
# score1 0.00085 -       -      
# score3 5.9e-06 0.00031 -      
# score6 4.8e-06 5.0e-05 0.00038
# => 전반적으로 모두 차이가 존재한다. 이 중 첫 시험과 마지막 시험의 차이가 가장 극명하다.

# 정규분포 확인해보장
out <- aov(score~id, data=rmlong)
shapiro.test(resid(out))
summary(out)
TukeyHSD(out) # 보정필요 : 0.5/n 보다 크면 귀무가설 채택, 작으면 귀무가설 기각


#### 실습5 연장 : Friedman Test(정규분포 아닐 때) ####
?friedman.test
RoundingTimes <- matrix(c(5.40, 5.50, 5.55, 5.85, 5.70, 5.75, 5.20, 5.60, 5.50, 
                          5.55, 5.50, 5.40, 5.90, 5.85, 5.70, 5.45, 5.55, 5.60,
                          5.40, 5.40, 5.35, 5.45, 5.50, 5.35, 5.25, 5.15, 5.00, 
                          5.85, 5.80, 5.70, 5.25, 5.20, 5.10, 5.65, 5.55, 5.45,
                          5.60, 5.35, 5.45, 5.05, 5.00, 4.95, 5.50, 5.50, 5.40, 
                          5.45, 5.55, 5.50, 5.55, 5.55, 5.35, 5.45, 5.50, 5.55,
                          5.50, 5.45, 5.25, 5.65, 5.60, 5.40, 5.70, 5.65, 5.55, 
                          6.30, 6.30, 6.25),
                        nrow = 22, byrow = TRUE, 
                        dimnames = list(1 : 22, c("Round Out", "Narrow Angle", "Wide Angle")))
RoundingTimes

library(reshape2)
rt <- melt(RoundingTimes)
rt

out <- aov(value~Var2, data=rt)
shapiro.test(resid(out)) # p-value = 0.001112 -> 정규분포X

boxplot(value~Var2, data=rt)

friedman.test(RoundingTimes) # p-value = 0.003805 -> 차이 존재

# 사후검정 #
# https://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/
friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape:     Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  
  
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  
  
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]    # In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,    ### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  
  
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")    # this is the post hoc of the friedman test
      
      
      # plotting
      if(to.plot.parallel & to.plot.boxplot)    par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)    # adding some spacing from both sides of the plot
        
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide")     #[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))    # adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
        
      }
      
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
      
    }    else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}

friedman.test.with.post.hoc(value~Var2|Var1, rt)
# Narrow Angle - Round Out  0.623939296
# Wide Angle - Round Out    0.003534336
# Wide Angle - Narrow Angle 0.053774441

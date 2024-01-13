#### 키보드 입력 ####

# scan() : 벡터 입력
# edit() : 데이터 프레임 직접 입력받아 생성

a <- scan()  # 숫자 형식의 데이터 입력, 입력을 중단할 경우 빈칸에 엔터
a

?scan()
b <- scan(what=character())  # what=character() : 문자도 입력 받을 수 있게 해줌
b

df <- data.frame()
df <- edit(df)
df

#### 파일 입력 ####

# read.csv(), read.table(), read.delim()
# read.xlsx(), read.spss()

# read.table()
# file.choose() : 파일 탐색기 창이 뜸, 불러올 파일 직접 선택
# na.strings = "" : 결측치 표시해둔 기호를 na로 바꾸기
?read.table()

student <- read.table("../data/student.txt")
student

student1 <- read.table("../data/student1.txt", header = TRUE)  # 제목도 데이터로 입력받음
student1

student2 <- read.table(file.choose(), header = T, sep = ";")
student2

student3 <- read.table("../data/student3.txt", header = T, na.strings = c("-", "+", "&"))
student3


# read.xlsx()
install.packages("xlsx") # xlsx파일을 불러오기 위한 모듈 설치
library(rJava)           # library = import
library(xlsx)

studentx <- read.xlsx(file.choose(), sheetIndex = 1)
studentx1 <- read.xlsx(file.choose(), sheetName = "emp2")
studentx1


# read.spss()
# to.data.frame = T : 테이블 형식일 때 TRUE로 해줘야함
install.packages("foreign")
library(foreign)

raw_welfare <- read.spss("../data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
raw_welfare

#### 화면 출력 ####

# 변수명
# (식)
# print()
# cat()

x <- 10
y <- 20
z <- x + y
z

(z <- x + y) # 식의 결과 바로 출력
print(z)
print(z <- x + y)

# print("x+y의 결과는", z, "입니다") : 파이썬과 달리 하나의 데이터만 출력 가능
cat("x+y의 결과는", z, "입니다")     # 여러 데이터 연결 가능


#### 파일 출력 ####

# write.csv(), write.table(), write.xlsx()

studentx <- read.xlsx("../data/studentexcel.xlsx", sheetIndex = 1)
studentx
class(studentx)

write.table(studentx, "../data/stud1.txt")  # 인덱스도 같이 저장됨
write.table(studentx, "../data/stud2.txt", row.names=F)           # 인덱스 빼고 저장
write.table(studentx, "../data/stud3.txt", row.names=F, quote=F)  # "" 빼고 저장

write.csv(studentx, "../data/stud4.csv", row.names=F, quote=F)

library(rJava)
library(xlsx)
write.xlsx(studentx, "../data/stud5.xlsx")

#### rda 파일 출력 ####

# 이진수로 저장하거나 이진수로 저장된 파일 불러올 때
# save()
# load()

save(studentx, file = "../data/stud6.rda")

rm(studentx)  # 변수 제거
studentx      # Error 발생

load("../data/stud6.rda")
studentx

#### sink() ####

?data()
data()   # 샘플 데이터 저장소

data(iris)
head(iris)
tail(iris)
str(iris)

sink("../data/iris.txt")  # 경로 지정해두면 데이터가 화면대신 해당 파일에서 출력

head(iris)
tail(iris)
str(iris)

sink()   # 파일 출력 종료

head(iris)

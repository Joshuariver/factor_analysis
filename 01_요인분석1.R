# 요인분석


# [개념]
#
# 변수들 간의 상관관계(correlation)를 이용하여 관측된 변수들에 영향을 미치고 있는 숨어 있는 공통인자를 찾아내는 통계기법
#  요인분석의 목적은 다수의 변수들을 정보손실을 최소화하면서 소수의 요인들(factors)로 축약, 검사나 척도의 개발 과정에서 
#  <<측정도구의 타당성>>을 파악하기 위해 많이 사용됨
#  요인분석을 통해 적절히 묶이지 않는 문항은 해당 요인을 측정하는 데 타당하지 못한 것으로 간주되어 검사문항에서 제외됨
#  요인분석에는 독립변수와 종속변수의 구분이 없음


# [요인분석의 기본가정]
#
# 사례 수: 일반적으로 100~200개 이상의 사례를 사용할 것을 권장. 변수와 표본수의 비율은 1:5 정도
# 사례 수 50개 이하: 매우 부족, 100개: 다소 부족(최소 기준), 200개: 적당, 300개: Good
#  다중공선성(Multicollinearity): 요인분석은 변수들 간의 상관을 기초로 하기 때문에 다중공선성의 제약을 덜 받는 편이다. 
# 그러나 지나친 다중공선성은 문제가 되는데, 일반적으로 요인의 고유값(Eigen value)이 0에 근접하면 다중공선성에 문제가 
# 있다고 판단할 수 있다.
#
#   표본 상관 행렬의 적절성 검정
# ① 표본 적절성 측정치(KMO): KMO 값이 1에 가까울 수록 요인분석하기에 적합함을 의미. 기준: .50이상
# ② Bartlett의 구형성 검정: 변수들 간의 상관이 0인지를 검정함. 유의확률이 작을수록(p<.05)요인분석하기에 적합함을 의미
# ③ 개별 변수에 대한 표본의 적정성 검정(MSA): 역 이미지 행렬에 제시되는 값으로 클수록 요인분석하기에 적합함을 의미. 기준: .50 이상


# [요인분석의 순서]
# 
# 요인추출(Extraction)모델: 주성분분석 사용, 요인 수를 최소화하며 정보손실을 최소화하는 방법
# 고유값(Eigen value): '1'이상을 기준
#  한 요인이 설명할 수 있는 변수의 갯수
# 요인회전방법(factor rotation): 배리맥스(Varimax) 방법
# 요인적재량(factor loading): 각 변수와 요인간의 상관관계 정도  .30이상(최소)
# 회전된 성분행렬의 값을 요인적재량이라고 할 수 있음. 절대값으로 .40이상이면 괜찮고, .50을 넘으면 중요한 변수라 인정.
#  요인의 해석을 보다 용이하게 하는 방식



rm(list=ls())
# setwd("~/R/Factor Analysis/SPSS_요인분석/data/01_수업")
setwd("~/R/Factor Analysis/SPSS_요인분석")

library(dplyr)
library(haven)
library(psych)

df <- read_spss("data/01_수업/1. 요인분석.sav")
df <- df[,c(2:10)]  # x1은 구매의도(종속변수) 이므로 제외

colnames(df) <- c("고품질","유명상표","값어치있음","분실염려가 없음","광고제품과 동일","환불용이"
                  ,"시간절약","구매가 용이","원하는 장소에 배달")

str(df)
names(df)

df=df[complete.cases(df),] # 관측치 중 공란이 남아있는 관측치 데이터를 찾아서 제거
describe(df)

df.matrix <- as.matrix(df)

options(digits = 3)
Hmisc::rcorr(df.matrix) # 상관분석 매트릭스를 생성 / 상관분석 유의확률표 같이 출력



bt.df <- bartlett.test(df)   # p값이 0.05 이하이어야 함.
print(bt.df, digits = 3)  
df.kmo <- KMO(df)  # Overall MSA 가 0.5 이상이어야 함. 더불어 각 항목별 MSA가 0.5보다 커야함. (작으면 해당 항목은 제외)
print(df.kmo, digits = 3)


# Scree Plot
plot(prcomp(df, type="l", sub = "Scree Plot"))  #요인분석에 적잘한 분류의 숫자를 정하기 위한 분석
# 그래프상 분산이 1 이상인 그래프의 숫자로 통상 결정
psych::scree(df)   # 요인 갯수 정하기. 요인적재량이 1이상일 경우의 적절한 요인 갯수 보기
psych::fa.parallel(df)



df.f1 <- principal(df,3,rotate = "none", scores = TRUE)
df.f1$communality # 공통성(communality): 성분행렬 각 성분 자승의 합 / 기준은 .50 이상
print(df.f1, digits = 3)

# the component loadings (from the pattern matrix)
# the h2 (communalities) : 성분행렬 각 성분 자승의 합, 0.5이상이면 OK. 절대적은 아님.
# .50 미만인 경우 ① 해당 변수를 무시하고 나머지 변수들을 중심으로 요인해석
#             ② 해당 변수를 제거하고 요인분석을 다시 실시( 해당변수를 제거하더라도 연구 목적에 크게 위배되지 않는경우에 사용)
# the u2 (the uniqueness)
# com (the complexity of the component loadings for that variable
# Proportion Var (회전 제곱합 적재량 )
# Cumulative Var (회전 제곱합 누적 적재량): 3개의 요인이 전체 분산의 72%를 설명하고 있음.



df.f2 <- principal(df,3,rotate = "varimax", scores = TRUE)
print(df.f2$loadings, digits = 5)  # 회전 제곱합 적재량  SS loadings: 전체, Proportion Var: 분산% (*100하며 %로 표현)
                # Cumulative Var: 누적% (*100하며, % 표현)
                # 3개의 요인은 전체 분산의 71.784%를 설명한다는 의미


round(unclass(df.f1$loadings), 3)  # 회전되기 전 요인행렬

round(unclass(df.f2$loadings), 3)  # 회전된 요인행렬


                         




# 아래쪽 내용은 일단 보류


# 요인분석(maximum likelihood factor analysis)
# rotation = "varimax"

df.f3 <- factanal(df, factors = 3,rotation = "varimax", # "varimax", "promax", "none" 
                  scores="regression") # "regression", "Bartlett"
print(df.f3, digits = 3)



# 요인숫자를 지정하는 요인분석


rm(list=ls())
# setwd("~/R/Factor Analysis/SPSS_요인분석/data/01_수업")
setwd("~/R/Factor Analysis/SPSS_요인분석")

library(dplyr)
library(haven)
library(psych)

df <- read_spss("data/01_수업/3. 요인분석-요인숫자지정.sav")



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



df.f1 <- principal(df,4,rotate = "none", scores = TRUE)
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



df.f2 <- principal(df,4,rotate = "varimax", scores = TRUE)
print(df.f2$loadings, digits = 5)  # 회전 제곱합 적재량  SS loadings: 전체, Proportion Var: 분산% (*100하며 %로 표현)
# Cumulative Var: 누적% (*100하며, % 표현)
# 3개의 요인은 전체 분산의 71.784%를 설명한다는 의미
print(fa.sort(df.f2,polar=FALSE), digits = 3, cut = .3)

# 여가균형 07, 11, 09 문항과 가족균형 04, 07 문항이 같은 요인으로 묶인 것으로 나타남.
# 이 경우 요인의 수정이 요구됨
# 요인분석은 변수 간의 상관에 기초한 분석이기 대문에, 문항을 하나씩 제거하면서 실시해야 함.

round(unclass(df.f1$loadings), 3)  # 회전되기 전 요인행렬

round(unclass(df.f2$loadings), 3)  # 회전된 요인행렬


# <문항제거방법>
# 1. 여러 요인에 걸쳐 높은 요인적재량( factor loading)을 보이는 문항을 중심으로 하나씩 제거
#   다시 말해 양다리 걸친 것을 제거
# 2. 요인적재량의 크기가 어느정도가 되어야 하는지에 대한 명확한 기준은 없으나 최소한 ±.30이상은 되어야 함.
# 그러나 최근에는 좀 더 보수적인 기준으로 ± .40 이상으로 보는 견해가 지배적임.

print(fa.sort(df.f2,polar=FALSE), digits = 3, cut = .4)


# 우선 "성장균형04"를 제거하고 다시 verimax 회전을 적용한 요인분석 실시

df.a <- subset(df,select = -WFF4)

df.f3 <- principal(df.a,4,rotate = "varimax", scores = TRUE)
print(fa.sort(df.f3,polar=FALSE), digits = 3, cut = .3)

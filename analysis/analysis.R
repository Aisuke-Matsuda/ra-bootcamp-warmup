library(tidyverse)
library(readxl)

##"master.csv"を読み込む
master<-readr::read_csv("master.csv")

##"master"の各列に含まれるNAの数を数える
sum(is.na(master$instnm))
sum(is.na(master$semester))
sum(is.na(master$quarter))
sum(is.na(master$year))
sum(is.na(master$yearofsem))
sum(is.na(master$after))
sum(is.na(master$totcohortsize))
sum(is.na(master$w_cohortsize))
sum(is.na(master$m_cohortsize))
sum(is.na(master$tot4yrgrads))
sum(is.na(master$m_4yrgrads))
sum(is.na(master$w_4yrgrads))
sum(is.na(master$women_gradrate_4yr))
sum(is.na(master$womengradrate4yr))
sum(is.na(master$gradrate4yr))
sum(is.na(master$mengradrate4yr))
sum(is.na(master$instatetuition))
sum(is.na(master$costs))
sum(is.na(master$faculty))
sum(is.na(master$white_cohortsize))
sum(is.na(master$per_white_cohort))
sum(is.na(master$per_women_cohort))

##各年度における"4年卒業率"のデータセットを作成
gradrate4yr_byyear<-master%>%
  aggregate(gradrate4yr~year, mean)

##"4年卒業率"の平均推移を表す折れ線グラフ
gradrate4yr_byyear%>%
  ggplot(aes(x=year, y=gradrate4yr))+
  geom_line()+geom_point()+theme_classic()+
  scale_x_continuous(limits = c(1990, 2010))+
  scale_y_continuous(limits = c(0.25, 0.45))


##各年度における"semester制導入率"のデータセットを作成
semester_byyear<-master%>%
  aggregate(semester~year, mean)

##"semester導入率"の平均推移を表す折れ線グラフ
semester_byyear%>%
  ggplot(aes(x=year, y=semester))+
  geom_line()+geom_point()+theme_classic()+
  scale_x_continuous(limits = c(1990, 2010))+
  scale_y_continuous(limits = c(0.8, 1))

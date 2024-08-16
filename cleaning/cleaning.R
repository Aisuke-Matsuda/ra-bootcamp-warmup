library(tidyverse)
library(readxl)


##データの読み込み
semester1<-readr::read_csv("semester_data_1.csv")
semester2<-readr::read_csv("semester_data_2.csv")


##列の結合
semester_total<-rbind(semester1, semester2)

##列の削除
semester_total<-semester_total%>%dplyr::select(, -6)


##outcomeデータの読み込み
outcome1<-read_excel("1991.xlsx", sheet=1)
outcome2<-read_excel("1992.xlsx", sheet=1)
outcome3<-read_excel("1993.xlsx", sheet=1)
outcome5<-read_excel("1995.xlsx", sheet=1)
outcome6<-read_excel("1996.xlsx", sheet=1)                                                       
outcome7<-read_excel("1997.xlsx", sheet=1)                                                       
outcome8<-read_excel("1998.xlsx", sheet=1)                                                       
outcome9<-read_excel("1999.xlsx", sheet=1)                                                       
outcome10<-read_excel("2000.xlsx", sheet=1)                                                      
outcome11<-read_excel("2001.xlsx", sheet=1)                                                      
outcome12<-read_excel("2002.xlsx", sheet=1)                                                      
outcome13<-read_excel("2003.xlsx", sheet=1)                                                      
outcome14<-read_excel("2004.xlsx", sheet=1)                                                      
outcome15<-read_excel("2005.xlsx", sheet=1)                                                      
outcome16<-read_excel("2006.xlsx", sheet=1)
outcome17<-read_excel("2007.xlsx", sheet=1)                                                      
outcome18<-read_excel("2008.xlsx", sheet=1)                                                      
outcome19<-read_excel("2009.xlsx", sheet=1)                                                    
outcome20<-read_excel("2010.xlsx", sheet=1)


##outcomeデータの結合
outcome_total<-rbind(outcome1, outcome2, outcome3, outcome5
                     , outcome6, outcome7, outcome8, outcome9
                     , outcome10, outcome11, outcome12, outcome13
                     , outcome14, outcome15, outcome16, outcome17
                     , outcome18, outcome19, outcome20)

##outcomeデータの中の"totcohortsize", "m_4yrgrads"を数値データに変換する
outcome_total$totcohortsize<-
  as.numeric(outcome_total$totcohortsize)
outcome_total$m_4yrgrads<-
  as.numeric(outcome_total$m_4yrgrads)


##女子学生の4年卒業率に0.01をかけて、0から1のスケールに変換する
outcome_total<-
  outcome_total%>%
  dplyr::mutate(womengraduate4yr=women_gradrate_4yr*0.01)

##男女合計の4年卒業率と男子学生の4年卒業率を計算
outcome_total<-
  outcome_total%>%dplyr::mutate(gradrate4yr=tot4yrgrads/totcohortsize)
outcome_total<-
  outcome_total%>%dplyr::mutate(mengradrate4yr=m_4yrgrads/m_cohortsize)


##covariatesデータの読み込み
covariate<-
  read_excel("covariates.xlsx", sheet=1)

##列名の変更
covariate<-
  dplyr::rename(.data = covariate, unitid=university_id)


##unitid のデータからaaaaを取り除く
covariate<-
  covariate%>%dplyr::mutate(unitid=substr(unitid, start=1, stop=6))

##‘category’列に含まれる’instatetuition’, ‘costs’, ’faculty’, ’white_cohortsize’を別の列として追加
covariate<-
  covariate%>%pivot_wider(names_from = "category", values_from = "value")


## "unitid", "year", "white_cohortsize", "faculty", "instatetuition", "costs"を数値データに変換
covariate$unitid<-
  as.numeric((covariate$unitid))
covariate$year<-
  as.numeric(covariate$year)
covariate$white_cohortsize<-
  as.numeric(covariate$white_cohortsize)
covariate$faculty<-
  as.numeric(covariate$faculty)
covariate$instatetuition<-
  as.numeric(covariate$instatetuition)
covariate$costs<-
  as.numeric(covariate$costs)

## covariate データの期間を他のデータと揃える(データの削除)
covariate<-covariate%>%subset(year!=1987 & year!=1988 & year!=1989 & year!=1990
                             & year!=2011 & year!=2012& year!=2013 & year!=2014
                             & year!=2015 & year!=2016)

##covariatesに含まれるunitidをoutcomeデータに揃える
master_data<-merge(covariate, outcome_total, by=c("unitid", "year"))

##master_dataをcovariate, outcome_total, semester_totalを結合して作製
master_data<-
  left_join(master_data, semester_total, by=c("unitid", "year"))

##"per_white_cohort", "per_women_cohort"を計算してmaster_dataに代入
master_data<-
  master_data%>%dplyr::mutate(per_white_cohort=white_cohortsize/totcohortsize)
master_data<-
  master_data%>%dplyr::mutate(per_women_cohort=w_cohortsize/totcohortsize)


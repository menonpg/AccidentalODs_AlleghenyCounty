library(readr)
AccidentalOD_Allegheny_County<- read_csv("1c59b26a-1684-4bfb-92f7-205b947530cf.csv")

AccidentalOD_Allegheny_County <- AccidentalOD_Allegheny_County %>% mutate(AgeGrp = cut(age, breaks = c(0, 18, 21, 30, 40, 50, 60, 100)))

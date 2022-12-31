data.imt.lxm.supSet<-read.xlsx(file = "Modelling_SupAirSet.xlsx",sheetIndex = 1)
data.imt.lxm.supSet.schedule<-data.table(time=c(1:7200))
data.imt.lxm.supSet.schedule[,minute:=floor(time/60)+1]
data.imt.lxm.supSet.schedule<-merge(x=data.imt.lxm.supSet.schedule,y=data.imt.lxm.supSet,
                                    all.x=TRUE,by.x="minute",by.y="time")

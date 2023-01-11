## script to do confounder data processing ##

## two-year period indicators ##
twoyrints<-data.table('year'=2001:2018,'yearint'=rep(c('2001-2002','2003-2004',
                                                       '2005-2006','2007-2008',
                                                       '2009-2010','2011-2012',
                                                       '2013-2014','2015-2016',
                                                       '2017-2018'),each=2),
                      'yearcode'=rep(1:9,each=2))

## hiv (goes back to 2008) ##
hiv<-read.csv('hiv/HIV_prevalence_timeseries_editted.csv')
## need to aggregate rates across the two age groups ##
setDT(hiv)
hiv[,pop:=Cases/(Rate.per.100000/100000)]
hiv_aa<-hiv[,.(cases=sum(Cases),pop=sum(pop)),by=.(Year,Geography,FIPS)]
hiv_aa[,rate:=cases/pop]
hiv_aa<-hiv_aa[Year %in% as.character(2008:2019)]
setnames(hiv_aa,c('Year','Geography','FIPS'),c('year','stname','stcode'))
## merge in the two-year intervals ##
hiv_aa[,year:=as.numeric(as.character(year))]
hiv2<-merge(hiv_aa,twoyrints,by='year')
## compute HIV rate for two-year intervals ##
hiv_fin<-hiv2[,.(cases=sum(cases),pop=sum(pop)),by=.(stname,stcode,yearint,yearcode)]
hiv_fin[,rate:=(cases/pop)*100000]
hiv_fin[,c('cases','pop'):=NULL]
hiv_fin[,stname:=gsub("[[:punct:]]", "", stname)]

## maternal age (2003-2006) ##
matage1<-read.table('maternal_age/Natality, 2003-2006_editted.txt',header=T,sep='\t')
setDT(matage1)
matage1[,c('Notes','Year.Code','Births'):=NULL]
setnames(matage1,names(matage1),c('stname','stcode','year','matage'))

## maternal age (2007-2020) ##
matage2<-read.table('maternal_age/Natality, 2007-2020_editted.txt',header=T,sep='\t')
setDT(matage2)
matage2[,c('Notes','Year.Code','Births'):=NULL]
setnames(matage2,names(matage2),c('stname','stcode','year','matage'))

## combine the time periods ##
matage_comb<-rbind(matage1,matage2)

## merge in the two-year intervals ##
matage_comb<-merge(matage_comb,twoyrints,by='year')

## average maternal age by state and two-year period ##
matage_fin<-matage_comb[,.(matage=mean(matage)),by=.(stname,stcode,yearint,yearcode)]

## percent hispanic (2000-2009) ##
hisp1<-read.csv('hispanic/hispanic_2000-2010.csv')
setDT(hisp1)
hisp1<-hisp1[STATE>0 & AGEGRP>0 & AGEGRP<=6 & SEX==0 & RACE==0]
hisp1[,c('REGION','DIVISION','SEX','RACE'):=NULL]
hisp1_lf<-melt(hisp1,id.vars=c('STATE','NAME','ORIGIN','AGEGRP'))
hisp1_lf<-hisp1_lf[,.(pop=sum(value)),by=.(STATE,NAME,ORIGIN,variable)]
hisp1_lf<-hisp1_lf[!(variable %in% c('ESTIMATESBASE2000','CENSUS2010POP','POPESTIMATE2010'))]
hisp1_fin<-dcast(hisp1_lf, STATE+NAME+variable ~ ORIGIN, value.var = "pop")
setDT(hisp1_fin)
setnames(hisp1_fin,old=c('0','1','2'),new=c('total','nothisp','hisp'))
hisp1_fin[,pct_hisp:=100*hisp/total]

## percent hispanic (2010-2020) ##
hisp2<-read.csv('hispanic/hispanic_2010-2019.csv')
setDT(hisp2)
hisp2<-hisp2[AGE<30 & SEX==0]
hisp2[,c('SUMLEV','REGION','DIVISION','SEX'):=NULL]
hisp2_lf<-melt(hisp2,id.vars=c('STATE','NAME','ORIGIN','RACE','AGE'))
hisp2_lf<-hisp2_lf[,.(pop=sum(value)),by=.(STATE,NAME,ORIGIN,variable)]
hisp2_lf<-hisp2_lf[!(variable %in% c('ESTIMATESBASE2010','CENSUS2010POP'))]
hisp2_fin<-dcast(hisp2_lf, STATE+NAME+variable ~ ORIGIN, value.var = "pop")
setDT(hisp2_fin)
setnames(hisp2_fin,old=c('0','1','2'),new=c('total','nothisp','hisp'))
hisp2_fin[,pct_hisp:=100*hisp/total]

## combine the decades of data ##
hisp_comb<-rbind(hisp1_fin,hisp2_fin)
hisp_comb[,year:=as.numeric(substr(variable,start=12,stop=15))]

## aggregate by two-year periods ##
hisp_comb<-merge(hisp_comb,twoyrints,by='year')
hisp_fin<-hisp_comb[,.(pct_hisp=mean(pct_hisp)),by=.(STATE,NAME,yearint,yearcode)]
setnames(hisp_fin,c('STATE','NAME'),c('stcode','stname'))

## percent heat with fuel oil ##
heat<-NULL
for (i in 2009:2020){
  heat<-rbind(heat, data.frame(get_acs(geography = "state",variables=paste0('B25040_00',c(1,5)),year=i,output='wide'),
                               ## use the center year of 5-year ACS ##
                               'year'=i-2))
}

setDT(heat)
heat[,pct_fueloil:=100*B25040_005E/B25040_001E]
heat<-heat[NAME!='Puerto Rico',.(GEOID,NAME,year,pct_fueloil)]
setnames(heat,c('GEOID','NAME'),c('stcode','stname'))
heat[,stcode:=as.numeric(as.character(stcode))]

heat_comb<-merge(heat,twoyrints,by='year')
heat_fin<-heat_comb[,.(pct_fueloil=mean(pct_fueloil)),by=.(stcode,stname,yearint,yearcode)]

## merge together all of the confounder data ##
confounders<-merge(hisp_fin,matage_fin,by=c('stcode','stname','yearint','yearcode'),all.x=T)
confounders<-merge(confounders,hiv_fin,by=c('stcode','stname','yearint','yearcode'),all.x=T)
confounders<-merge(confounders,heat_fin,by=c('stcode','stname','yearint','yearcode'),all.x=T)

## backward imputation for missing maternal age, HIV status, and heating fuel ##
confounders<-confounders[order(stcode,yearcode)]
confounders[, matage := na.locf(matage, fromLast=TRUE), by = stcode]
confounders[, hivrate := na.locf(rate, fromLast=TRUE), by = stcode]
confounders[, pct_fueloil := na.locf(pct_fueloil, fromLast=TRUE), by = stcode]
confounders[,rate:=NULL]
confounders[,stname:=tolower(stname)]

save(confounders,file='processed_data/confounders.RData')

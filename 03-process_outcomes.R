## this script reads in the outcome data, processes, and merges with confounder data ##

# read in and format the npcr state code dictionary ##
codein<-read.table('npcr/timeseries_iccc/state_codes.txt',
                   header=F,stringsAsFactors = F,sep='\n')

splitcode<-strsplit(x=codein$V1,split='=')
stcode<-data.frame('state'=as.numeric(unlist(lapply(splitcode,function(x) x[1]))),
                   'stname'=as.character(tolower(unlist(lapply(splitcode,function(x) x[2])))))

## read in the npcr leukemia and lymphoma two-year rate data ##
allfile<-list.files('npcr/timeseries_iccc')

allfile<-allfile[grep('.csv',allfile)]

ll<-NULL
for (i in 1:length(allfile)){
  temp<-read.csv(paste0(
    'npcr/timeseries_iccc/',
    allfile[i]),stringsAsFactors = F)
  ll<-rbind(ll,data.frame(temp,'year'=substr(allfile[i],1,9),'yearcode'=i))
}

setDT(ll)

setnames(ll,names(ll),c('state','type','rate','count','pop','year','yearcode'))

## pick off only all lymphoma and all leukemia ##
ll<-ll[(type %in% c(0,10))]

ll[rate=='^',rate:=NA]
ll[count=='^',count:=NA]

ll[,rate:=as.numeric(rate)]
ll[,count:=as.numeric(count)]

## merge with state code dataset ##
ll<-merge(ll,stcode,by='state')

## add in various treatment indicators for the summary stats/plots ##

ll[,denom:=count/rate]
ll[,post:=yearcode>5]
ll[,ak:=stname=='alaska']
ll[,trt:=as.numeric((stname %in% c('alaska'))
                    & yearcode>5)]

ll[,trtplot:='Control States']
ll[ak==T & yearcode<5,trtplot:='Alaska (pre-MSAT)']
ll[ak==T & yearcode>=5,trtplot:='Alaska (post-MSAT)']
ll[,trtplot:=factor(trtplot,levels=c('Alaska (pre-MSAT)','Alaska (post-MSAT)','Control States'))]

ll[,ctype:=recode(type,'0'='Leukemia','10'='Lymphoma')]

## merge in confounders ##
ll<-merge(ll,confounders,by=c('stname','yearcode'))

## separate datasets for leukemia and lymphoma ##
leuk<-ll[type==0]
## remove states with missingness ##
rmst<-unique(leuk[is.na(rate),stname])
leuk<-leuk[!(stname %in% rmst)]

ll<-ll[!((stname %in% rmst) & type==0)]

lymph<-ll[type==10]
## remove states with missingness ##
rmst<-unique(lymph[is.na(rate),stname])
lymph<-lymph[!(stname %in% rmst)]

ll<-ll[!((stname %in% rmst) & type==10)]

save(ll,leuk,lymph,file='processed_data/outcomes.RData')
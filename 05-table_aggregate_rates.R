## leukemia ##

## ak ##
ak_leuk<-c(round(leuk[ak==T & post==F,sum(count)/sum(denom)],2),
           round(leuk[ak==T & post==T,sum(count)/sum(denom)],2),
           round(leuk[ak==T & post==T,sum(count)/sum(denom)]-leuk[ak==T & post==F,sum(count)/sum(denom)],2))

## rest ##
rest_leuk<-c(round(leuk[ak==F & post==F,sum(count,na.rm=T)/sum(denom,na.rm=T)],2),
             round(leuk[ak==F & post==T,sum(count,na.rm=T)/sum(denom,na.rm=T)],2),
             round(leuk[ak==F & post==T,sum(count,na.rm=T)/sum(denom,na.rm=T)]-leuk[ak==F & post==F,sum(count,na.rm=T)/sum(denom,na.rm=T)],2))


## lymphoma ##

## ak ##
ak_lymph<-c(round(lymph[ak==T & post==F,sum(count)/sum(denom)],2),
            round(lymph[ak==T & post==T,sum(count)/sum(denom)],2),
            round(lymph[ak==T & post==T,sum(count)/sum(denom)]-lymph[ak==T & post==F,sum(count)/sum(denom)],2))

## rest ##
rest_lymph<-c(round(lymph[ak==F & post==F,sum(count,na.rm=T)/sum(denom,na.rm=T)],2),
              round(lymph[ak==F & post==T,sum(count,na.rm=T)/sum(denom,na.rm=T)],2),
              round(lymph[ak==F & post==T,sum(count,na.rm=T)/sum(denom,na.rm=T)]-lymph[ak==F & post==F,sum(count,na.rm=T)/sum(denom,na.rm=T)],2))

## make table ##
aggtable<-rbind(c(ak_leuk,rest_leuk),c(ak_lymph,rest_lymph))
rownames(aggtable)<-c('Leukemia','Lymphoma')
colnames(aggtable)<-c(paste(rep(c('AK','Rest'),each=3),rep(c('pre','post','diff'),2)))
print(aggtable)

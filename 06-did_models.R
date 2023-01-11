## leukemia ##
## linear mixed model with state random effects #
leukmod<-lmer(rate~ak+post+ak*post + ns(pct_hisp,df=3)+matage+hivrate +pct_fueloil + (1|stname),data=leuk)
print(data.frame(summary(leukmod)$coefficients,confint(leukmod)[-(1:2),]))
leuk_coef<-summary(leukmod)$coefficients[,1]
## inputs to DID plots ##
## model predictions of ak pre rates, ak post rates, other pre rates, other post rates, ak pre rates (again) ##
leuk_pred<-predict(leukmod,newdata=data.table('ak'=c(T,T,F,F,T),
                                              'post'=c(F,T,F,T,F),
                                              'pct_hisp'=rep(mean(leuk$pct_hisp,na.rm=T),5),
                                              'matage'=rep(mean(leuk$matage,na.rm=T),5),
                                              'hivrate'=rep(mean(leuk$hivrate,na.rm=T),5),
                                              'pct_fueloil'=rep(mean(leuk$pct_fueloil,na.rm=T),5)),re.form=NA)
## add ak post counterfactual rates ##
leuk_pred<-c(leuk_pred,leuk_pred[2]-leuk_coef[length(leuk_coef)])

## lymphoma ##
## linear mixed model with state random effects #
lymphmod<-lmer(rate~ak+post+ak*post+ ns(pct_hisp,df=3)+matage+hivrate + pct_fueloil+ (1|stname),data=lymph)
print(data.frame(summary(lymphmod)$coefficients,confint(lymphmod)[-(1:2),]))
lymph_coef<-summary(lymphmod)$coefficients[,1]
## inputs to DID plots ##
## model predictions of ak pre rates, ak post rates, other pre rates, other post rates, ak pre rates (again) ##
lymph_pred<-predict(lymphmod,newdata=data.table('ak'=c(T,T,F,F,T),
                                                'post'=c(F,T,F,T,F),
                                                'pct_hisp'=rep(mean(lymph$pct_hisp,na.rm=T),5),
                                                'matage'=rep(mean(lymph$matage,na.rm=T),5),
                                                'hivrate'=rep(mean(lymph$hivrate,na.rm=T),5),
                                                'pct_fueloil'=rep(mean(lymph$pct_fueloil,na.rm=T),5)),re.form=NA)
## add ak post counterfactual rates ##
lymph_pred<-c(lymph_pred,lymph_pred[2]-lymph_coef[length(lymph_coef)])

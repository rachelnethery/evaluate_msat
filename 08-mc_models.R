## set seed because gsynth uses cross val ##
set.seed(16)

## leukemia ##
leuk_nosm <- gsynth(rate~trt+ ns(pct_hisp,df=3)+matage+hivrate +pct_fueloil, data = as.data.frame(leuk),
                    index = c("state","year"), force = "two-way",estimator = 'mc',lambda = seq(.001,.005,length.out = 10),
                    CV=T,se=F)
plot(leuk_nosm)
plot(leuk_nosm, type = "counterfactual", raw = "none", main="")

## set seed because gsynth uses cross val ##
#set.seed(111)

## lymphoma ##
lymph_nosm <- gsynth(rate~trt+ns(pct_hisp,df=3)+matage+hivrate +pct_fueloil, data = as.data.frame(lymph), 
                     index = c("state","year"), force = "two-way",estimator = 'mc',lambda = seq(.001,.005,length.out = 10),
                     CV=T,se=F)
plot(lymph_nosm)
plot(lymph_nosm, type = "counterfactual", raw = "none", main="")

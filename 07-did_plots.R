## put leukemia average rates into a ggplot-able dataset ##
plotdf<-data.frame('pop'=rep(c('Alaska','Rest of US','Counterfactual Alaska'),each=2),
                   'time'=rep(c('pre-MSAT','post-MSAT'),3),
                   'leuk'=leuk_pred,
                   'lymph'=lymph_pred)

## create DID plot ##
## order time factor levels appropriately ##
plotdf$time<-factor(plotdf$time,levels=c('pre-MSAT','post-MSAT'))

## melt data into long form by disease type for ggplot ##
plotdf<-reshape2::melt(plotdf,id.vars=c('pop','time'))

plotdf$variable<-recode(plotdf$variable,'leuk'='Leukemia','lymph'='Lymphoma')

## extract plot legend for cowplot ##
ff<-get_legend(ggplot(subset(plotdf,variable=='Leukemia'), aes(x=time, y=value, group=pop)) +
                 geom_line(aes(color=pop,linetype=pop),size=1)+
                 scale_color_manual(values=c("#0072B2","#0072B2","#D55E00"))+
                 scale_linetype_manual(values = c(1,3,1))+
                 ylab('Age-Adjusted Rate (per 100,000)')+
                 theme_bw()+
                 theme(legend.position = "bottom",
                       legend.title = element_blank(),
                       legend.text = element_text(size=11),
                       panel.grid.minor = element_blank(),
                       axis.title.x=element_blank(),
                       axis.text.x = element_text(color='black'),
                       axis.text.y = element_text(color='black'))+
                 coord_cartesian(xlim = c(1.4, 2)))

## leukemia did plot ##
p<-ggplot(subset(plotdf,variable=='Leukemia'), aes(x=time, y=value, group=pop)) +
  geom_line(aes(color=pop,linetype=pop),size=1)+
  scale_color_manual(values=c("#0072B2","#0072B2","#D55E00"))+
  scale_linetype_manual(values = c(1,3,1))+
  ylab('Age-Adjusted Rate (per 100,000)')+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=11),
        axis.text.x = element_text(color='black',size=11),
        axis.text.y = element_text(color='black',size=11))+
  coord_cartesian(xlim = c(1.4, 2))

## add intervention effect estimate with brace ##
p1<-p+geom_brace(aes(c(2.05,2.15),
                     c(leuk_pred[2], leuk_pred[6]),
                     label=paste0("Intervention\n Effect= ",round(leuk_coef[length(leuk_coef)],2))), inherit.data=F, labelsize=4,rotate=90)

## lymphoma did plot ##
q<-ggplot(subset(plotdf,variable=='Lymphoma'), aes(x=time, y=value, group=pop)) +
  geom_line(aes(color=pop,linetype=pop),size=1)+
  scale_color_manual(values=c("#0072B2","#0072B2","#D55E00"))+
  scale_linetype_manual(values = c(1,3,1))+
  ylab('Age-Adjusted Rate (per 100,000)')+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=11),
        axis.text.x = element_text(color='black',size=11),
        axis.text.y = element_text(color='black',size=11))+
  coord_cartesian(xlim = c(1.4, 2))

## add intervention effect estimate with brace ##
p2<-q+geom_brace(aes(c(2.05,2.15),
                     c(lymph_pred[2], lymph_pred[6]),
                     label=paste0("Intervention\n Effect= ",round(lymph_coef[length(lymph_coef)],2))), inherit.data=F, labelsize=4,rotate=90)

## side-by-side plots with cowplot ##
pboth<-plot_grid(p1,p2,labels = c('A','B'))

pdf('figures/did_plots_rev.pdf',width=10,height=6)
print(plot_grid(pboth,ff,ncol=1,rel_heights = c(3, .4)))
dev.off()

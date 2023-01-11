
scplots<-data.table('ctype'=rep(c('Leukemia','Lymphoma'),each=18),
                    'ltype'=rep(rep(c('Observed','Counterfactual'),each=9),2),
                    'rate'=0,'att'=0)

scplots[ctype=='Leukemia',rate:=c(leuk_nosm$Y.tr,leuk_nosm$Y.ct)]
scplots[ctype=='Leukemia',att:=leuk_nosm$att.avg]


scplots[ctype=='Lymphoma',rate:=c(lymph_nosm$Y.tr,lymph_nosm$Y.ct)]
scplots[ctype=='Lymphoma',att:=lymph_nosm$att.avg]


scplots[,year:=rep(levels(ll[,year]),4)]

#scplots[,gp:=paste0(ctype,bw,ltype)]

scplots[,ltype:=factor(ltype,levels = c('Observed','Counterfactual'),labels=c('Alaska','Synthetic Control'))]

scplots[,att:=paste0('IE= ',round(att,2))]

mm<-ggplot(scplots, aes(x=year, y=rate,group=ltype)) +
  geom_line(aes(color=ltype,linetype=ltype),size=1)+
  scale_color_manual(values=c("black","dodgerblue3"))+
  geom_vline(xintercept = '2009-2010')+
  facet_grid(~ctype)+
  ylab('Age-Adjusted Rate (per 100,000)')+
  theme_bw()+
  geom_label(aes(label=att), 
             x=8,
             y=6,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.35,
             color = "black",
             fill="darkseagreen3")+
  theme( axis.title.x = element_blank(),
         axis.text.x = element_text(angle=55,hjust = 1),
         legend.title = element_blank(),
         legend.position = 'bottom')

pdf('figures/mc_plots_rev.pdf',height=5,width=9)
print(mm)
dev.off()

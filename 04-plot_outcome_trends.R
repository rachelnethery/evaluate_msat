## this script creates time trend plots of the outcome ##

mm<-ggplot(ll, aes(x=year, y=rate, group=stname)) +
  geom_line(aes(color=trtplot,size=ak))+
  scale_size_manual(values = c('TRUE'=1, 'FALSE'=0.5),guide='none')+
  scale_color_manual(values=c("#FC8D6280","red", "#99999950"))+
  geom_vline(xintercept = '2009-2010')+
  ylab('Age-Adjusted Rate (per 100,000)')+
  facet_wrap(~ctype)+
  theme_bw()+
  theme( axis.title.x = element_blank(),
         axis.text.x = element_text(angle=55,hjust = 1),
         legend.title = element_blank(),
         legend.position = 'bottom')

pdf('figures/outcome_trends.pdf',height=5,width=9)
print(mm)
dev.off()

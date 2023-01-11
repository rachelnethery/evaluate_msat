## this script creates time trend plots of the confounders ##

yaxisnames<-c('Percent Hispanic (age<30)','Average Maternal Age',
              'HIV Prevalence (ages 13-34)','Percent Heating with Fuel Oil')

confounders[,trtplot:='Control States']
confounders[stname=='alaska' & yearcode<5,trtplot:='Alaska (pre-MSAT)']
confounders[stname=='alaska' & yearcode>=5,trtplot:='Alaska (post-MSAT)']
confounders[,trtplot:=factor(trtplot,levels=c('Alaska (pre-MSAT)','Alaska (post-MSAT)','Control States'))]

varnames<-c('pct_hisp','matage','hivrate','pct_fueloil')

for (i in 1:length(varnames)){
pdf(paste0('figures/',varnames[i],'_trends.pdf'),
    height=4,width=7)
print(ggplot(confounders, aes(x=yearint, y=get(varnames[i]), group=stname)) +
    geom_line(aes(color=trtplot,size=(stname=='alaska')))+
    scale_size_manual(values = c('TRUE'=1, 'FALSE'=0.5),guide='none')+
    scale_color_manual(values=c("#FC8D6280","red", "#99999950"))+
    geom_vline(xintercept = '2009-2010')+
    ylab(yaxisnames[i])+
    theme_bw()+
    theme( axis.title.x = element_blank(),
           axis.text.x = element_text(angle=55,hjust = 1),
           legend.title = element_blank(),
           legend.position = 'bottom')
)
dev.off()
}

confounders[,trtplot:=NULL]

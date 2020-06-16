Regional_temp <- rbind(extract.cumulative("Yorkshire.and.The.Humber"),
                       extract.cumulative("North.East"),
                       extract.cumulative("North.West"),
                       extract.cumulative("East.Midlands"),
                       extract.cumulative("West.Midlands"),
                       extract.cumulative("East"),
                       extract.cumulative("London"),
                       extract.cumulative("South.West"),
                       extract.cumulative("South.East"),
                       extract.cumulative("England"),
                       extract.cumulative("Wales"))



Regional_temp<-Regional_temp[Regional_temp$Week.ended==as.Date(c.date,"%d/%m/%y"),]

Regional_temp$Week.ended<-NULL
Regional_temp$`50%`<-NULL
Regional_temp <- Regional_temp %>% rename(lwr=`2.5%`,upr=`97.5%`)

Regional_temp<-Regional_temp %>% mutate(excess.Deaths.upr=Deaths-Covid-lwr, 
                                        excess.Deaths.lwr=Deaths-Covid-upr, 
                                        excess.Deaths.expected=Deaths-Covid-fit) # this temp used for coefficient/multiplier (see below)

temp3<-Regional_temp %>% group_by(region) %>% summarise(
  allDeaths = Deaths,  
  exptd = fit, 
  per.expted=100*fit/Deaths,
  exptd.upr = upr,
  per.expted.upr=100*upr/Deaths,
  exptd.lwr = lwr,
  per.expted.lwr=100*lwr/Deaths,
  Covid.num = Covid,
  per.Covid=100*Covid/Deaths,
  exces.nCovid = Deaths-Covid-fit,
  per.exces.nCovid=100*(Deaths-Covid-fit)/Deaths,
  exces.nCovid.lwr = Deaths-Covid-upr,
  per.exces.nCovid.lwr=100*(Deaths-Covid-upr)/Deaths,
  exces.nCovid.upr = Deaths-Covid-lwr,
  per.exces.nCovid.upr=100*(Deaths-Covid-lwr)/Deaths)

temp3$allDeaths <- format(temp3$allDeaths, big.mark=",")

temp3$exptd <- paste0(format(round(temp3$exptd), big.mark=","), "\n(", 
                      format(round(temp3$per.expted, digits = 1), big.mark=","), "%)", 
                      sep = "")
temp3$exptd_bounds <- paste0(format(round(temp3$exptd.lwr), big.mark=","), " -", 
                             format(round(temp3$exptd.upr), big.mark=","), "\n(", 
                             round(temp3$per.expted.lwr, digits = 1),"% - ", 
                             round(temp3$per.expted.upr, digits = 1), "%)", sep = "")

temp3$exces.nCovid <- paste0(format(round(temp3$exces.nCovid), big.mark=","), "\n(", 
                             round(temp3$per.exces.nCovid, digits = 1), "%)", sep = "")
temp3$exces_bounds <- paste0(format(round(temp3$exces.nCovid.lwr), big.mark=","), " - ", 
                             format(round(temp3$exces.nCovid.upr), big.mark=","), "\n(", 
                             round(temp3$per.exces.nCovid.lwr, digits = 1),"% - ", 
                             round(temp3$per.exces.nCovid.upr, digits = 1), "%)", sep = "")

temp3$Covid.num <- paste0(format(round(temp3$Covid.num), big.mark=","), "\n(", 
                          round(temp3$per.Covid, digits = 1), "%)", sep = "")
temp3$region <- gsub("[.]"," ",temp3$region)
EW <- temp3[temp3$region %in% c("England", "Wales"),]
temp3 <- temp3[!temp3$region %in% c("England", "Wales"),]
temp3 <- rbind(temp3, EW)
regional_tab <- temp3 %>% select(region, allDeaths, exptd, exptd_bounds, Covid.num, exces.nCovid, exces_bounds)
colnames(regional_tab) <- c("Region", "Reported all-cause Deaths", "Expected (%)", "Expected 95% prediction interval (%)", "Covid (%)",
                            "Excess non-Covid", "Excess non-Covid 95% prediction interval (%)")

kable(regional_tab)%>%
  kable_styling(bootstrap_options = c("striped","condensed"),full_width = F, position = "left")

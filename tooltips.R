library(tidyr)
library(dplyr)
library(purrr)
library(highcharter)

#Read in data
me_raw = read.csv("measles.csv")

#Make long from wide
me_raw_long = gather(data=me_raw, Month, Total, January:December)
 
#Up front filtering
me_raw_filter = me_raw_long %>%
                filter(!(Year==2011) & Country %in% c('Canada','United States of America'))

#Make a Totals Dataset
totals =  me_raw_filter %>% 
          group_by(Year, Country) %>% 
          mutate(Total=sum(na.rm=TRUE,Total)) %>%
          distinct(Year, .keep_all = TRUE)  %>%
          select(Year, Country, Total)  
           

#Make Monthly Dataset
months = me_raw_filter %>%
         select(Year, Country, Month, Total) %>% 
         nest(-Year,-Country) %>%
         mutate(
            data = map(data, mutate_mapping,  hcaes(categories=Month, y=Total)),
            data = map(data, list_parse)
          ) %>%
         rename(ttdata = data)


#Merge Them
all_me <- left_join(totals, months, by = c("Country","Year"))


#Plot it
hchart(all_me, "line", hcaes(x = Year, y = Total, group=Country)) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_xAxis(allowDecimals=FALSE, title=list(text="Year")) %>%
  hc_yAxis(title=list(text="Counts")) %>%
  hc_title(text = paste("Yearly Measles Count in Canada and the United States of America"),
           margin = 20, align = "left",
           style = list(useHTML = TRUE)) %>%
  hc_subtitle(text = "Source: World Health Organization (WHO)",
              align = "left") %>%
 
  hc_tooltip(useHTML = TRUE,
            headerFormat = "<b>{point.key}</b>",
            pointFormatter = tooltip_chart(accesor = "ttdata",
                                           width="500",
                                             hc_opts = list(xAxis = list(title=list(text="Month"),
                                                                    labels=list(style=list(color="white")), 
                                                                    categories=list(list(categories= c('Jan', 'Feb', 'Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')))),
                                                            yAxis = list(labels=list(style=list(color="white")))
                                                            )
                                          )
                                           )

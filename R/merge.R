colnames(AD.df)
colnames(Site.df)
colnam es(VAPV1.df)
colnames(VATap.df)
colnames(WTB.df)

names(AD.df)[names(AD.df) == "Ad group"] <- "Ad Group"

Site <- Site %>% rename(Placement = `Placement Domain`)
VAPV1 <- VAPV1 %>% rename(Placement = `Placement Domain`)
VATap <- VATap %>% rename(Placement = `Placement Domain`)
WTB <- WTB %>% rename(Placement = `Placement Domain`)


AD.df <- AD %>% group_by(Country, Placement, Campaign, `Ad group`) %>%
  summarize(Clicks = sum(Clicks),
            Impressions = sum(Impressions),
            Cost = sum(Cost))

Site.df <- Site %>% group_by(Country, Placement, Campaign, `Ad Group`) %>%
  summarize(Pageviews = sum(Pageviews),
            NewUsers = sum(`New Users`),
            Sessions = sum(Sessions),
            Bounces = sum(Bounces),
            TimeOnPage = sum(`Time on Page`))


VAPV1.df <- VAPV1 %>% group_by(Country, Placement, Campaign, `Ad Group`) %>%
  summarize(VAPV1 = sum(Sessions))

VATap.df <- VATap %>% group_by(Country, Placement, Campaign, `Ad Group`) %>%
  summarize(VATap = sum(Sessions))

WTB.df <- WTB %>% group_by(Country, Placement, Campaign, `Ad Group`) %>%
  summarize(WTB = sum(Pageviews))


Placement.df <- merge(AD.df, Site.df, by = c("Country", "Placement", "Campaign", "Ad Group"), all = TRUE) %>% replace(is.na(.), 0)
Placement.df <- merge(Placement.df, VAPV1.df, by = c("Country", "Placement", "Campaign", "Ad Group"), all = TRUE) %>% replace(is.na(.), 0)
Placement.df <- merge(Placement.df, VATap.df, by = c("Country", "Placement", "Campaign", "Ad Group"), all = TRUE) %>% replace(is.na(.), 0)
Placement.df <- merge(Placement.df, WTB.df, by = c("Country", "Placement", "Campaign", "Ad Group"), all = TRUE) %>% replace(is.na(.), 0)


WriteXLS(Placement.df, ExcelFileName="~/merge/2018-03-08/Placement.xlsx", AdjWidth=TRUE, BoldHeaderRow=TRUE)

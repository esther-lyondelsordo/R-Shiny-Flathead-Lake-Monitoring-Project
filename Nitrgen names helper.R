### Cnverting varius chem names to a single consistant name

####Dataframe names should mater but this should be dne afer the original dataframe has been spread by Param####



EpiChemsJEE<-
  JEEScrape %>%
  select(everything(), -EndDepth, -TimeCol, -StartDepth) %>%
  rename(Chl = "Chl-a") %>%
  mutate(Chla = coalesce(Chl, CHLa)) %>%
  select(everything(), -Chl, -CHLa) %>%
  rename(NH4N = "NH4-N",
         NO23 = "NO2/3",
         NO3NO2 = "NO3/NO2",
         NO3N = "NO3-N",
         NO2N ="NO2-N") %>%
  mutate(NHx = rowSums(cbind (NH4N, NH3), na.rm=TRUE),
         NOx = rowSums(cbind (NO2N, NO23, NO3, NO3N, NO3NO2), na.rm=T),
         DIN = rowSums(cbind(NOx, NHx), na.rm=T),
        ArcTN = ifelse(is.na(TKN), NA, TKN + DIN),
        TotalN = coalesce(ArcTN, TN, TPN)) %>%
  select(SRP, TotalN, TP, Chla, Year, Month, CollectionDate)%>%
  distinct()
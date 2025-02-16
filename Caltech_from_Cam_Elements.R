
{
# Carbonates --------------------------------------------------------------

Carbonate<-Mg_isotope_database[Mg_isotope_database$Type=="Carbonate",c("Sub.Type","Sub.Sub.Type","Sub.Sub.Sub.Type","Sub.Sub.Sub.Sub.Type","Source","d26Mg")]
factor(Carbonate[Carbonate$Sub.Type=="Marine","Sub.Sub.Sub.Type"])

Carbonate[Carbonate$Sub.Sub.Sub.Type=="Mollusc","Source"]


All_marine_carbonate<-Carbonate[Carbonate$Sub.Type=="Marine",c("Sub.Type","Sub.Sub.Type","Sub.Sub.Sub.Type","Sub.Sub.Sub.Sub.Type","Source","d26Mg")]
factor(All_marine_carbonate[All_marine_carbonate$Sub.Type=="Marine","Sub.Sub.Sub.Type"])
All_marine_carbonate<-All_marine_carbonate[All_marine_carbonate$Sub.Sub.Sub.Type=="Aragonite"|
                                           All_marine_carbonate$Sub.Sub.Sub.Type=="LMC"|
                                             All_marine_carbonate$Sub.Sub.Sub.Type=="HMC"|
                                             All_marine_carbonate$Sub.Sub.Sub.Type=="Limestone"
                                             ,c("Sub.Type","Sub.Sub.Type","Sub.Sub.Sub.Type","Sub.Sub.Sub.Sub.Type","Source","d26Mg")]
factor(All_marine_carbonate[All_marine_carbonate$Sub.Type=="Marine","Sub.Sub.Sub.Type"])
levels(as.factor(All_marine_carbonate[All_marine_carbonate$Sub.Type=="Marine","Source"]))




#All_marine_carbonate<-as_tibble(rbind(Calcite,All_marine_carbonate))%>%drop_na(d26Mg)
All_marine_carbonate<-as_tibble(All_marine_carbonate)%>%drop_na(d26Mg)
All_marine_carbonate$Type<-"Marine Carbonate"
All_marine_carbonate<-All_marine_carbonate[,c("Sub.Type", "Sub.Sub.Sub.Type", "Source","d26Mg","Type")];colnames(All_marine_carbonate)<-c("Sub.Type", "Sub.Sub.Type", "Source","d26Mg","Type")


All_marine_carbonate%>%
  group_by(Sub.Sub.Type) %>%
  dplyr::summarise(median = median(d26Mg), mean=mean(d26Mg), sd=sd(d26Mg),n = n()) %>%
  arrange(median) 


# Dolomites --------------------------------------------------------------
Dolomite<-Carbonate[Carbonate$Sub.Sub.Sub.Type=="Dolomite"
                                |Carbonate$Sub.Sub.Sub.Type=="Dolostone"
                                |Carbonate$Sub.Sub.Sub.Type=="Dolcrete *"
                                |Carbonate$Sub.Sub.Sub.Type=="Dolomicrite"
                                ,c("Sub.Type","Sub.Sub.Type","Source","d26Mg")]

factor(Dolomite$Source)

Dolomite<-as_tibble(Dolomite)%>%drop_na(d26Mg)
Dolomite$Type<-"Dolomite"
mean(Dolomite$d26Mg)

# Fresh Waters --------------------------------------------------------------
Water<-Mg_isotope_database[Mg_isotope_database$Type=="Water",c("Sub.Type","Sub.Sub.Type","Sub.Sub.Sub.Type","Source","d26Mg")]
factor(Water$Sub.Sub.Type)

Water[Water$Sub.Type=="River Water",c("Sub.Sub.Type","Sub.Sub.Sub.Type","Source","d26Mg")]

Terrestrial_Water<-Water[Water$Sub.Sub.Type=="River Water"
                    |Water$Sub.Sub.Type=="Soil Pore Water"
                    |Water$Sub.Sub.Type=="Groundwater"
                    |Water$Sub.Sub.Type=="Precipitation"
                    |Water$Sub.Sub.Type=="Drip water"
                    ,c("Sub.Sub.Type","Sub.Sub.Sub.Type","Source","d26Mg")]

Terrestrial_Water<-as_tibble(Terrestrial_Water)%>%drop_na(d26Mg)
Terrestrial_Water$Type<-"Terrestrial Water"
Terrestrial_Water$Sub.Sub.Sub.Type<-Terrestrial_Water$Sub.Sub.Type
dput(colnames(Terrestrial_Water))

colnames(Terrestrial_Water)<-c("Sub.Type", "Sub.Sub.Type", "Source", "d26Mg", "Type"
)

# Seawater --------------------------------------------------------------
Seawater<-Water[Water$Sub.Sub.Type=="Seawater"
                         ,c("Sub.Sub.Type","Sub.Sub.Sub.Type","Source","d26Mg")]

Seawater<-as_tibble(Seawater)%>%drop_na(d26Mg)
Seawater$Type<-"Seawater"
colnames(Seawater)<-c("Sub.Type", "Sub.Sub.Type", "Source", "d26Mg", "Type")

# Hydrothermal fluids --------------------------------------------------------------
Hydrothermal<-Water[Water$Sub.Sub.Type=="Hydrothermal Fluids"
                ,c("Sub.Sub.Type","Sub.Sub.Sub.Type","Source","d26Mg")]



Hydrothermal<-as_tibble(Hydrothermal)%>%drop_na(d26Mg)
Hydrothermal$Type<-"Hydrothermal Fluids"
colnames(Hydrothermal)<-c("Sub.Type", "Sub.Sub.Type", "Source", "d26Mg", "Type")


# Silicate Rock --------------------------------------------------------------
Silicate<-Mg_isotope_database[Mg_isotope_database$Type=="Silicate Rock",c("Sub.Type","Sub.Sub.Type","Source","d26Mg")]
factor(Silicate$Sub.Type)
Silicate$Type="Silicate Rock"
Silicate<-as_tibble(Silicate)%>%drop_na(d26Mg)
Silicate
# Soil --------------------------------------------------------------
Soil<-Mg_isotope_database[Mg_isotope_database$Type=="Soil",c("Sub.Type","Sub.Sub.Type","Source","d26Mg")]
factor(Soil$Sub.Type)
Soil$Type<-"Soil"
Soil<-as_tibble(Soil)%>%drop_na(d26Mg)


Caltech_data<-rbind(Seawater, Terrestrial_Water, Dolomite, All_marine_carbonate,Silicate,Soil,Hydrothermal)%>%drop_na(d26Mg)
factor(Caltech_data$Sub.Sub.Type)
levels(factor(Caltech_data$Sub.Sub.Type))
factor(levels(Caltech_data$Sub.Sub.Type))









# Plot --------------------------------------------------------------------

#Determine the plot order
lvls<-Caltech_data %>%
  group_by(Type) %>%
  dplyr::summarise(
    median = median(d26Mg), 
    mean=mean(d26Mg),
    n = n()
  )%>%mutate(
    name=paste(Type,", n=(",n,")", sep="")
  )%>%arrange(mean) 






}


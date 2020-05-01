etholiad <- read.csv("C:/Users/User/Desktop/Dysgu_Peirianyddol/etholiad.csv")
View(etholiad)
#Plyg rydym yn trio datrys ei label

plyg <- c("Cymru", "Benyw", "Na", "Ty Rhes", "Na")

#Cyfrifo tebygolrwydd bob plaid

teb_plyg_geidwadol <- 1
for (i in 1:5)
{
  if (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Geidwadol"),names(etholiad)[i]]==plyg[i])==0)
  {
    teb_plyg_geidwadol <- teb_plyg_geidwadol * (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Geidwadol"),names(etholiad)[i]]==plyg[i])+1)/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Geidwadol"),names(etholiad)[i]])
  }else
  {
    teb_plyg_geidwadol <- teb_plyg_geidwadol * sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Geidwadol"),names(etholiad)[i]]==plyg[i])/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Geidwadol"),names(etholiad)[i]])
  }
}
teb_plyg_geidwadol <- teb_plyg_geidwadol * sum(etholiad[,"Plaid.Wleidyddol"]=="Plaid Geidwadol")/length(etholiad[,"Plaid.Wleidyddol"])

teb_plyg_llafur <- 1
for (i in 1:5)
{
  if (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Lafur"),names(etholiad)[i]]==plyg[i])==0)
  {
    teb_plyg_llafur <- teb_plyg_llafur * (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Lafur"),names(etholiad)[i]]==plyg[i])+1)/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Lafur"),names(etholiad)[i]])
  }else
  {
    teb_plyg_llafur <- teb_plyg_llafur * sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Lafur"),names(etholiad)[i]]==plyg[i])/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Plaid Lafur"),names(etholiad)[i]])
  }
}
teb_plyg_llafur <- teb_plyg_llafur * sum(etholiad[,"Plaid.Wleidyddol"]=="Plaid Lafur")/length(etholiad[,"Plaid.Wleidyddol"])

teb_plyg_rhyddfrydol <- 1
for (i in 1:5)
{
  if (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Democrataidd Rhyddfrydol"),names(etholiad)[i]]==plyg[i])==0)
  {
    teb_plyg_rhyddfrydol <- teb_plyg_rhyddfrydol * (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Democrataidd Rhyddfrydol"),names(etholiad)[i]]==plyg[i])+1)/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Democrataidd Rhyddfrydol"),names(etholiad)[i]])
  }else
  {
    teb_plyg_rhyddfrydol <- teb_plyg_rhyddfrydol * sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Democrataidd Rhyddfrydol"),names(etholiad)[i]]==plyg[i])/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Democrataidd Rhyddfrydol"),names(etholiad)[i]])
  }
}
teb_plyg_rhyddfrydol <- teb_plyg_rhyddfrydol * sum(etholiad[,"Plaid.Wleidyddol"]=="Democrataidd Rhyddfrydol")/length(etholiad[,"Plaid.Wleidyddol"])

teb_plyg_arall <- 1
for (i in 1:5)
{
  if (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Arall"),names(etholiad)[i]]==plyg[i])==0)
  {
    teb_plyg_arall <- teb_plyg_arall * (sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Arall"),names(etholiad)[i]]==plyg[i])+1)/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Arall"),names(etholiad)[i]])
  }else
  {
    teb_plyg_arall <- teb_plyg_arall * sum(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Arall"),names(etholiad)[i]]==plyg[i])/length(etholiad[which(etholiad[,"Plaid.Wleidyddol"]=="Arall"),names(etholiad)[i]])
  }
}
teb_plyg_arall <- teb_plyg_arall * sum(etholiad[,"Plaid.Wleidyddol"]=="Arall")/length(etholiad[,"Plaid.Wleidyddol"])

#Cyfrifo fwyaf tebygol

fector_teb <- c(teb_plyg_geidwadol, teb_plyg_llafur, teb_plyg_rhyddfrydol, teb_plyg_arall)
Pleidiau <- c("Plaid Geidwadol","Plaid Lafur","Democrataidd Rhyddfrydol","Arall")
Pleidiau[which.max(fector_teb)]

##Ailcychwyn

Plaid.Wleidyddol <- c(rep(x = "Plaid Geidwadol", 55), rep(x = "Plaid Lafur", 52), rep(x = "Democrataidd Rhyddfrydol", 21), rep(x = "Arall", 72))

Plaid.Wleidyddol <- c(rep(1,200))

Pleidiau <- c("Plaid Geidwadol","Plaid Lafur","Democrataidd Rhyddfrydol","Arall")

Perchen.ar.fwy.nag.un.ty <- c(rep("Yndw",43), rep("Na", 157))
Perchen.ar.fwy.nag.un.ty <- sample(Perchen.ar.fwy.nag.un.ty, size = 200, replace = FALSE)

Rhyw <- c(rep("Gwryw",87), rep("Benyw", 113))
Rhyw <- sample(Rhyw, 200, FALSE)

Gwlad <- c(rep("Cymru", 37), rep("Alban", 46), rep("Gogledd Iwerddon",33), rep("Lloegr", 84))
Gwlad <- sample(Gwlad, 200, FALSE)

Hapus.gyda.plaid.yn.pwer <- c(rep("Yndw", 31), rep("Na", 169))
Hapus.gyda.plaid.yn.pwer <- sample(Hapus.gyda.plaid.yn.pwer, 200, FALSE)

Math.o.dy <- c(rep("Byngalo", 29), rep("Bwthyn", 9), rep("Datgysylltiedig",45), rep("Fflat", 32), rep("Ty Par", 27), rep("Ty Rhes",58))
Math.o.dy <- sample(Math.o.dy, 200, FALSE)

for (i in 1:200)
{
  if (Math.o.dy[i]=="Byngalo")
  {
    Plaid.Wleidyddol[i] <- sample(Pleidiau, size = 1, prob = c(0.3,0.2,0.3,0.2))
  }
  if (Math.o.dy[i]=="Bwthyn")
  {
    Plaid.Wleidyddol[i] <- sample(Pleidiau, size = 1, prob = c(0.2,0.2,0.2,0.4))
  }
  if (Math.o.dy[i]=="Datgysylltiedig")
  {
    Plaid.Wleidyddol[i] <- sample(Pleidiau, size = 1, prob = c(0.4,0.2,0.2,0.2))
  }
  if (Math.o.dy[i]=="Fflat")
  {
    Plaid.Wleidyddol[i] <- sample(Pleidiau, size = 1, prob = c(0.1,0.5,0.3,0.1))
  }
  if (Math.o.dy[i]=="Ty Par")
  {
    Plaid.Wleidyddol[i] <- sample(Pleidiau, size = 1, prob = c(0.2,0.2,0.3,0.3))
  }
  if (Math.o.dy[i]=="Ty Rhes")
  {
    Plaid.Wleidyddol[i] <- sample(Pleidiau, size = 1, prob = c(0.1,0.6,0.2,0.1))
  }
  if (Perchen.ar.fwy.nag.un.ty[i] == "Yndw")
  {
    Plaid.Wleidyddol[i] <- "Plaid Geidwadol"
  }
  if (Hapus.gyda.plaid.yn.pwer[i] == "Yndw")
  {
    Plaid.Wleidyddol[i] <- "Plaid Geidwadol"
  }
}
data <- data.frame("Gwlad" = Gwlad, "Rhyw" = Rhyw, "Hapus gyda'r plaid yn pwer" = Hapus.gyda.plaid.yn.pwer, "Math o dy" = Math.o.dy, "Perchen ar fwy nag un ty" = Perchen.ar.fwy.nag.un.ty, "Plaid Wleidyddol" = Plaid.Wleidyddol)
View(data)

###  Addasu data
etholiad <- read.csv("C:/Users/User/Desktop/Dysgu_Peirianyddol/etholiad.csv")

plyg <- c("Cymru", "Gwryw", "Na", "Anghywir", "Ty Par", "Na")
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
teb_plyg_geidwadol * sum(etholiad[,"Plaid.Wleidyddol"]=="Plaid Geidwadol")/length(etholiad[,"Plaid.Wleidyddol"])

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
teb_plyg_llafur * sum(etholiad[,"Plaid.Wleidyddol"]=="Plaid Lafur")/length(etholiad[,"Plaid.Wleidyddol"])

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
teb_plyg_rhyddfrydol * sum(etholiad[,"Plaid.Wleidyddol"]=="Democrataidd Rhyddfrydol")/length(etholiad[,"Plaid.Wleidyddol"])

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
teb_plyg_arall * sum(etholiad[,"Plaid.Wleidyddol"]=="Arall")/length(etholiad[,"Plaid.Wleidyddol"])

fector_teb <- c(teb_plyg_geidwadol, teb_plyg_llafur, teb_plyg_rhyddfrydol, teb_plyg_arall)
Pleidiau <- c("Plaid Geidwadol","Plaid Lafur","Democrataidd Rhyddfrydol","Arall")
Pleidiau[which.max(fector_teb)]

etholiad <- read.csv("C:/Users/User/Desktop/Dysgu_Peirianyddol/etholiad.csv")

View(etholiad)

plyg <- c("Lloegr", "Benyw", "Na", "Ty Rhes", "Na")



tebygolrwydd_amodol <- function(plaid, colofn, gwerth, data)
{
  (sum(data[which(data[,"Plaid.Wleidyddol"]==plaid),names(data)[colofn]]==gwerth)+1)/(length(data[which(data[,"Plaid.Wleidyddol"]==plaid),names(data)[colofn]])+length(levels(etholiad[,colofn])))
}

tebygolrwydd <- function(plaid, data)
{
  sum(etholiad[,"Plaid.Wleidyddol"]==plaid)/length(etholiad[,"Plaid.Wleidyddol"])
}

rhagfynegi <- function(data, plyg)
{
  pleidiau <- c("Plaid Geidwadol", "Plaid Lafur", "Democrataidd Rhyddfrydol", "Arall")
  fector_tebygolrwydd <- c()
  for (i in pleidiau)
  {
    tebygolrwydd_plaid <- 1
    for (j in 1:(length(names(data))-1))
    {
      tebygolrwydd_plaid <- tebygolrwydd_plaid * tebygolrwydd_amodol(i, j, plyg[j], data)
    }
    tebygolrwydd_plaid <- tebygolrwydd_plaid * tebygolrwydd(i, data)
    fector_tebygolrwydd <- c(fector_tebygolrwydd, tebygolrwydd_plaid)
  }
  return(pleidiau[which.max(fector_tebygolrwydd)])
}

rhagfynegi(data = etholiad, plyg = plyg)
    
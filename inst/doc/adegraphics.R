### R code from vignette source 'adegraphics.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: adegraphics.Rnw:35-36
###################################################
options(width = 90)


###################################################
### code chunk number 2: args
###################################################
library(ade4)
library(adegraphics)
args(s.label)


###################################################
### code chunk number 3: gargsVSclass
###################################################
source("gargsVSclass.R")


###################################################
### code chunk number 4: adegraphics.Rnw:197-199
###################################################
data(olympic)
pca1 <- dudi.pca(olympic$tab, scannf = FALSE)


###################################################
### code chunk number 5: plot1
###################################################
g1 <- s1d.barchart(pca1$eig, p1d.horizontal = F, ppolygons.col = "white")


###################################################
### code chunk number 6: adegraphics.Rnw:213-215
###################################################
class(g1)
showClass("C1.barchart")


###################################################
### code chunk number 7: adegraphics.Rnw:219-220
###################################################
slotNames(g1)


###################################################
### code chunk number 8: adegraphics.Rnw:239-240
###################################################
g1@data


###################################################
### code chunk number 9: plot2
###################################################
g2 <- s.corcircle(pca1$co)


###################################################
### code chunk number 10: adegraphics.Rnw:255-257
###################################################
class(g2)
g2@g.args


###################################################
### code chunk number 11: plot3
###################################################
update(g2, fullcircle = FALSE)
g2@g.args


###################################################
### code chunk number 12: adegraphics.Rnw:285-286
###################################################
getcall(g1) ## equivalent to g1@Call


###################################################
### code chunk number 13: plot4
###################################################
g3 <- s.label(pca1$li)
g4 <- s.arrow(5 * pca1$c1, add = TRUE)
class(g4)


###################################################
### code chunk number 14: plot5
###################################################
zoom(g3, zoom = 2, center = c(2, -2))


###################################################
### code chunk number 15: adegraphics.Rnw:350-351
###################################################
fac.score <- factor(olympic$score < 8000, labels = c("MT8000", "LT8000"))


###################################################
### code chunk number 16: plot6
###################################################
g5 <- s.class(pca1$li, fac.score, col = c("red", "blue"), chullSize = 1, ellipseSize = 0, plabels.cex = 2, pbackground.col = "grey85", paxes.draw = TRUE)


###################################################
### code chunk number 17: plot7
###################################################
g6 <- superpose(g5, g3, plot = TRUE) ## equivalent to g5 + g3
class(g6)


###################################################
### code chunk number 18: plot8 (eval = FALSE)
###################################################
## g5
## s.label(pca1$li, add = TRUE)


###################################################
### code chunk number 19: plot8b
###################################################
rbindADEg(cbindADEg(g2, g3), cbindADEg(g5, g6), plot = TRUE)


###################################################
### code chunk number 20: plot9
###################################################
g7 <- insert(g2, g6, posi = c(0.65, 0.65, 0.95, 0.95))
class(g7)


###################################################
### code chunk number 21: adegraphics.Rnw:408-414
###################################################
length(g7)
names(g7)
names(g7) <- c("chulls", "labels", "cor")
class(g7[1])
class(g7[[1]])
class(g7$chulls)


###################################################
### code chunk number 22: plot10
###################################################
pos.mat <- getpositions(g7)
pos.mat
pos.mat[3,] <- c(0.1, 0.7, 0.3, 0.9)
update(g7, positions = pos.mat)


###################################################
### code chunk number 23: plot11
###################################################
g7[[3]] <- g1
g7


###################################################
### code chunk number 24: plot12
###################################################
addhist(g3)


###################################################
### code chunk number 25: plot13
###################################################
ADEgS(adeglist = list(g2, g3), layout = c(1, 2))


###################################################
### code chunk number 26: plot14
###################################################
mlay <- matrix(c(1, 1, 0, 1, 1, 0, 0, 0, 2), byrow = T, nrow = 3)
mlay
ADEgS(adeglist = list(g6, g2), layout = mlay)


###################################################
### code chunk number 27: plot15
###################################################
mpos <- rbind(c(0, 0.3, 0.7, 1), c(0.5, 0, 1, 0.5))
ADEgS(adeglist = list(g3, g5), positions = mpos)


###################################################
### code chunk number 28: plot16
###################################################
ADEgS(list(g5, g3), add = matrix(c(0, 1, 0, 0), byrow = TRUE, ncol = 2))


###################################################
### code chunk number 29: plot17
###################################################
data(jv73)
pca2 <- dudi.pca(jv73$morpho, scannf = FALSE)
s.label(pca2$li)


###################################################
### code chunk number 30: plot18
###################################################
g8 <- s.label(pca2$li, facets = jv73$fac.riv)
length(g8)
names(g8)


###################################################
### code chunk number 31: plot19
###################################################
s.class(pca2$li, fac = jv73$fac.riv, col = rainbow(12), facets = jv73$fac.riv)


###################################################
### code chunk number 32: plot20
###################################################
pca1 <- dudi.pca(olympic$tab, scannf = FALSE, nf = 4)
g9 <- s.corcircle(pca1$co, xax = 1:2, yax = 3:4)
length(g9)
names(g9)
g9@positions


###################################################
### code chunk number 33: plot21
###################################################
dim(pca1$li)
s1d.boxplot(pca1$li, fac.score, col = c("red", "blue"), psub = list(position = "topleft", cex = 2))


###################################################
### code chunk number 34: plot22
###################################################
s.value(pca2$li, pca2$tab, symbol = "circle")


###################################################
### code chunk number 35: plot23
###################################################
data(meaudret)
pca3 <- dudi.pca(meaudret$env, scannf = FALSE)
pca4 <- dudi.pca(meaudret$spe, scale = FALSE, scannf = FALSE)
coi1 <- coinertia(pca3, pca4, scannf = FALSE, nf = 3)
g10 <- plot(coi1)
class(g10)
names(g10)
g10@Call


###################################################
### code chunk number 36: adegraphics.Rnw:625-627
###################################################
library(lattice)
sort(names(trellis.par.get()))


###################################################
### code chunk number 37: plot24
###################################################
d <- scale(olympic$tab)
g11 <- table.image(d, plot = FALSE)
g12 <- table.image(d, axis.line = list(col = "blue"), axis.text = list(col = "red"), plot = FALSE)
ADEgS(c(g11, g12), layout = c(1, 2))


###################################################
### code chunk number 38: adegraphics.Rnw:648-649
###################################################
names(adegpar())


###################################################
### code chunk number 39: adegraphics.Rnw:654-656
###################################################
adegpar("ppoints")
adegpar()$ppoints


###################################################
### code chunk number 40: paramVSparam
###################################################
source("paramVSparam.R")


###################################################
### code chunk number 41: paramVSfunction
###################################################
source("paramVSfunction.R")


###################################################
### code chunk number 42: plot25
###################################################
oldadegpar <- adegpar()
adegpar("plabels")
g13 <- s.label(dfxy = pca1$li, plot = FALSE)

adegpar(plabels = list(col = "blue", cex = 1.5), paxes.draw = TRUE)
adegpar("plabels")
g14 <- s.label(dfxy = pca1$li, plot = FALSE)

ADEgS(c(g13, g14), layout = c(1, 2))


###################################################
### code chunk number 43: adegraphics.Rnw:705-706
###################################################
adegpar(oldadegpar)


###################################################
### code chunk number 44: plot25b
###################################################
adegpar("ppoints")
s.label(dfxy = pca1$li, plabels.cex = 0, ppoints = list(col = c(2, 4, 5), cex = 1.5, pch = 15))
adegpar("ppoints")


###################################################
### code chunk number 45: plot25c
###################################################
s.label(pca2$li, facets = jv73$fac.riv, samelimits = FALSE)


###################################################
### code chunk number 46: plot26
###################################################
g15 <- plot(coi1, pbackground.col = "steelblue")


###################################################
### code chunk number 47: plot27
###################################################
names(g15)
plot(coi1, XYmatch.pbackground.col = "steelblue",  XYmatch.pgrid.col = "red", eig.ppolygons.col="orange")


###################################################
### code chunk number 48: plot28
###################################################
data(meaudret)
g16 <- s.label(pca3$li, plot = FALSE)
g17 <- s.label(pca3$li, ppoints.col= "red", plabels = list(box = list(draw = FALSE), optim = TRUE), plot = FALSE)
ADEgS(c(g16, g17), layout = c(1, 2))


###################################################
### code chunk number 49: plot29
###################################################
g18 <- s.class(pca3$li, fac = meaudret$design$season, plot = FALSE)
g19 <- s.class(pca3$li, fac = meaudret$design$season, ellipseSize = 0, chullSize = 1, starSize = 0.5, col = TRUE, plot = FALSE)
g20 <- s.class(pca3$li, fac = meaudret$design$season, pellipses.lwd = 2, pellipses.border = 2:5, pellipses.col = 2:5, plot = FALSE)
g21 <- s.class(pca3$li, fac = meaudret$design$season, ellipseSize = 0, chullSize = 0, ppolygons.lwd = 2, plines.col = 2:5, starSize = 1.2, plot = FALSE)

ADEgS(c(g18, g19, g20, g21), layout = c(2, 2))


###################################################
### code chunk number 50: plot30
###################################################
data(rpjdl)
coa2 <- dudi.coa(rpjdl$fau, scannf = FALSE, nf = 3)
g22 <- s.value(coa2$li, coa2$li[,3], plot = FALSE)
g23 <- s.value(coa2$li, coa2$li[,3], method = "color", ppoints.cex = 0.8, plegend.size= 0.8, plot = FALSE)
g24 <- s.value(coa2$li, coa2$li[,3], plegend.size = 0.8, ppoints.cex = 0.8, symbol = "square", method = "color", key = list(columns = 1), col = colorRampPalette(c("yellow", "blue"))(6), plot = FALSE)
g25 <- s.value(coa2$li, coa2$li[, 3], center = 0, method = "size", ppoints.cex = 0.6, symbol = "circle", col = c("yellow", "red"), plot = FALSE)
ADEgS(c(g22, g23, g24, g25), layout = c(2, 2))


###################################################
### code chunk number 51: plot31
###################################################
score1 <- c(rnorm(1000, mean = -0.5, sd = 0.5), rnorm(1000, mean = 1))
fac1 <- rep(c("A", "B"), each = 1000)
g26 <- s1d.density(score1, fac1, pback.col = "grey75", plot = FALSE)
g27 <- s1d.density(score1, fac1, col = c(2, 4), plot = FALSE)
g28 <- s1d.density(score1, fac1, col = c(2, 4), p1d.reverse = TRUE, p1d.horizontal = FALSE, p1d.rug.draw = FALSE, plot = FALSE)
g29 <- s1d.density(score1, fac1, col = c(2, 4), ppolygons.alpha = 0.2, p1d = list(rug = list(tck = 1, line = FALSE)), plot = FALSE)
ADEgS(c(g26, g27, g28, g29), layout = c(2, 2))


###################################################
### code chunk number 52: plot32
###################################################
library(Guerry)
library(sp)
data(gfrance85)
region.names <- data.frame(gfrance85)[, 5]
col.region <- colors()[c(149, 254, 468, 552, 26)]
g30 <- s.class(coordinates(gfrance85), region.names, porigin.include = FALSE, plot = FALSE)
g31 <- s.class(coordinates(gfrance85), region.names, ellipseSize = 0, starSize = 0, Sp = gfrance85, pgrid.draw = F, pSp.col = col.region[region.names], pSp.alpha = 0.4, plot = FALSE)
ADEgS(c(g30, g31), layout = c(1, 2))


###################################################
### code chunk number 53: plot32b
###################################################
s.Spatial(gfrance85[,7:12])


###################################################
### code chunk number 54: plot33
###################################################
data(mafragh, package = "ade4")
g32 <- s.label(mafragh$xy, nb = mafragh$nb, plot = FALSE)
g33 <- s.label(mafragh$xy, nb = mafragh$nb, pnb.ed.col = "red", plab.cex = 0, pnb.node = list(cex = 3, col = "blue"), ppoints.col = "green", plot = FALSE)
ADEgS(c(g32, g33), layout = c(1, 2))


###################################################
### code chunk number 55: plot34
###################################################
data(euro123, package = "ade4")
df <- rbind.data.frame(euro123$in78, euro123$in86, euro123$in97)
row.names(df) <- paste(row.names(euro123$in78), rep(c(1, 2, 3), rep(12, 3)), sep = "")
g34 <- triangle.label(df, label = row.names(df), showposition = TRUE, plot = FALSE)
g35 <- triangle.label(euro123$in78, plabels.cex = 0, ppoints.cex = 2, addmean = TRUE, show = FALSE, plot = FALSE)
ADEgS(c(g34, g35), layout = c(1, 2))



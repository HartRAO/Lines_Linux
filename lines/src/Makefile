#
# Makefile for lines
#
# created : 3/7/96 MJG
#vtoai.o  and vtori.o replaced by xtoai and xtori 070308
#
# so that make does not confuse .mod file with modula2 code; for Debian Lenny
# %.o : %.mod
INSTALL = /usr/local/bin
LFLAG1 =  -lpgplot
# standard up to Debian Etch:
#LFLAG2 =  -lpng
# for Ubuntu ibex:
#LFLAG2 =  -lpng -lf2c
# for Debian Lenny:
#LFLAG2 = -lf2c
#LFLAG3 = -L/usr/X11R6/lib -lX11
#
# Fortran compiler and compilation flags
# standard up to Debian Etch:
#FCOMPL=g77
# for Ubuntu ibex:  and Debian Lenny(?)
FCOMPL=gfortran -ff2c
# FFLAGC=-Wall -O
FFLAGC=-Wall -g
FFLAGD=-fno-backslash
#
OBJS =  lines.o  \
adpol.o \
aitov.o \
altcalc.o \
altdatgr.o \
alterdat.o \
arms.o \
ascfilav.o \
ascfilin.o \
atmos.o \
azaltg.o \
b50tog.o \
b50toj20.o \
bells.o \
boxcar.o \
cmdflopn.o \
cmdget.o \
cmdparse.o \
cmdselct.o \
commalbl.o \
covsrt.o \
coordcnv.o \
date_obs2jd.o \
dm2doy.o \
dtdms.o \
dthms.o \
eda_endpts.o \
eda_hann.o \
eda_median.o \
eda_medof3.o \
eda_rsm.o \
eda_runmed.o \
eda_s3rssh.o \
eda_s4253h.o \
eda_s2.o \
eda_s3.o \
eda_s3r.o \
eda_s4.o \
eda_s5.o \
eda_sort.o \
eda_split.o \
envval.o \
er_close.o \
er_eof.o \
er_exist.o \
er_open.o \
er_read.o \
er_write.o \
fdel.o \
fixdown.o \
fold.o \
fors1.o \
four1.o \
func2dgauss.o \
fxj.o \
fxjmed.o \
fxrl1.o \
gausfit2ds.o \
gaussj.o \
gccorr.o \
gcparm.o \
getdate.o \
gtob50.o \
hamm.o \
helpless.o \
j20tob50.o \
j2000pm.o \
j2000pr.o \
jd.o \
jdcorr.o \
jdparm.o \
ktojy.o \
ktojy1720.o \
ktojy5000.o \
ktojy6668.o \
ktojy12178.o \
lfft4.o \
lgfit2.o \
lineinit.o \
linfit.o \
linteg.o \
lintstat.o \
lintwide.o \
lipoly.o \
lispline.o \
listdat.o \
listdir.o \
listasca.o \
listascf.o \
listhkp.o \
listmem.o \
lmsp.o \
locase.o \
logfopen.o \
loopcmd.o \
loopparm.o \
maxmin.o \
mean.o \
medianfilt.o \
memadd.o \
memav.o \
memclr.o \
memcopy.o \
memmult.o \
memop.o \
mgfit.o \
mgparm.o \
mrqmin.o \
mrqcof.o \
nchar.o \
ncombine.o \
offset.o \
ohktojy.o \
pause.o \
pbola.o \
pca.o \
pca2.o \
plconfau.o \
plcursps.o \
plotdata.o \
plscale.o \
plsetup.o \
pltext.o \
poblock.o \
pssset.o \
pssf.o \
qmemused.o \
rdascfl.o \
rdmcafl.o \
rdnccscsv.o \
ritov.o \
scalprod.o \
setparm.o \
settcont.o \
smooth.o \
spacelbl.o \
spl.o \
splineatx.o \
stfromjd.o \
syscmd.o \
tmblock.o \
tsmidha.o \
tsysch.o \
upcase.o \
vellim.o \
velwin.o \
vh.o \
wrtascfl.o \
wrtascf2.o \
wrtgsfl.o \
wrtmcafl.o \
wrttmfl.o \
xedit.o \
xtoai.o \
xtori.o

lines: $(OBJS)
	$(FCOMPL) -o $@ $(OBJS) $(LFLAG1) $(LFLAG2) $(LFLAG3)

.f.o:
	$(FCOMPL) -c -g $(FFLAGC) $(FFLAGD) $<


install : 
	cp lines $(INSTALL)

clean : 
	/bin/rm -f *.o core junk *.ps *.out *~ lines

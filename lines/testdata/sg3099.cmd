# lines command file to do pointing correction on freq-sw data
do i 1 3 1
   ra sg3099.asc i i new i
end
pca -59.8
# clear set memory 8 and add the on source spectra
clr 8
ra sg3099.asc 3 4 add 
mc pc 

lf sg3398.asc
pause
clr all
pb clear
pb -43.193     -39.709
pb -31.392     -25.660
pb -17.456     -14.534
do i 1 3 1
  ra sg3398.asc i i  new i
  po pb 3 i
end
pca -38.8
clr 12
ra sg3398.asc 3 4 add 
mc pc 
sf
pl
pause
pb clear
pb -43.193     -40.046
pb -31.617     -29.032
po pb 3
pl
wa g3398-av.asc 

clr all
pb clear
pb -66.081 -63.720
pb -56.752 -49.672
pb -41.692 -37.534
do i 1 3 1
  ra sg3099.asc i i  new i
  po pb 3 i
  pause
end
pca -59.8
clr
pause
ra sg3099.asc 3 4 add
mc pc

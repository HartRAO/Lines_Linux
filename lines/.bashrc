#

#LINES_HLP is used to set the path to the lines help files
# edit as appropriate for your path structure,
# i.e. where the lines source code is found on your PC.
# for PCs away from hartrao this is likely to be:
export LINES_HLP="~/lines/doc/"
# ~/.bashrc: Execute by bash(1) for non-login shells

# If not running interactively do not do anything
[ -z "$PS1" ] && return


# append to history file do not overwrite it
shopt -s histappend

# setting the history length in bash(1)
#
# HISTSIZE = 1000
# HISTFILESIZE = 2000

# PGPLOT variables 
#
# PGPLOT_DIR is the directory containing the PGPLOT library
#  one of the following two lines MUST be uncommented and edited if necessary
#  to reflect where the PGPLOT library is to be found
#
#  if  PGPLOT_DIR added in /usr/local/lib/pgplot, then use:
export PGPLOT_DIR="/usr/local/lib/pgplot"
#
#  if PGPLOT is installed as part of linux distribution in /usr/local/pgplot
#  then use:
#export PGPLOT_DIR="/usr/local/pgplot"
#
#
# PGPLOT_DEV is the default PGPLOT plot device
export PGPLOT_DEV="/xw"
#
# PGPLOT_PS lines set the postscript plot size and offset (in milliinches);
#  the values are chosen to prevent cropping on an HP Deskjet850 printer.
export PGPLOT_PS_WIDTH="7500"
export PGPLOT_PS_HEIGHT="10000"
export PGPLOT_PS_HOFFSET="500"
export PGPLOT_PS_VOFFSET="600"
#
#PGPLOT_FOREGROUND colour overrides the default white and
#PGPLOT_BACKGROUND colour overrides the default black for
# the interactive display, thus matching the postscript black on white.
export PGPLOT_FOREGROUND="black"
export PGPLOT_BACKGROUND="white"
#

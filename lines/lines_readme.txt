Running LINES under LINUX			MJG 2007/08/21
=========================

LINES is the spectroscopy analysis program for spectra obtained with the
old Hartebeesthoek correlator, or multicolumn ascii data such as drift
scans.  It uses routines from a variety of sources and is in fortran 77.

It comes with no guarantees whatsoever, but the author does attempt to
assist those who use it to get meaningful results.

These notes describe how to set up the program on linux.


1. Configure your PC to run PGPLOT
----------------------------------

LINES calls PGPLOT routines to do graphics.  LINES is designed to use PGPLOT
version 5.2 or later.

Obtain PGPLOT from http://astro.caltech.edu/~tjp/pgplot/
However it may already be packaged for your version of linux.

To be able to run programs calling the PGPLOT graphics package
from your Linux box, if you are using the BASH shell (default), 
you need to edit the file called

.bash_profile
or 
.bashrc

in your /home/<yourname> directory.

.bash_profile (for BASH/LINUX) should be edited to contain the lines
contained within --------:

This assumes that .bash_profile is executed each time a new shell is opened.
Type 'set' to see if the environment variables have actually been set.
If not, try putting the commands into .bashrc, open a new command shell and
type 'set'.

Read thru these lines carefully and comment/comment what you need.

MAKE SURE THAT THE PATHS LISTED BELOW MATCH HOW LINES AND PGPLOT
IS INSTALLED ON YOUR COMPUTER

-------------------------------------------------------------
#
# PGPLOT variables 
#
# PGPLOT_DIR is the directory containing the PGPLOT library
#  one of the following two lines MUST be uncommented and edited if necessary
#  to reflect where the PGPLOT library is to be found
#
#  if  PGPLOT_DIR added in /usr/local/lib/pgplot, then use:
#export PGPLOT_DIR="/usr/local/lib/pgplot/"
#
#  if PGPLOT is installed as part of linux distribution in /usr/local/pgplot
#  then use:
#export PGPLOT_DIR="/usr/local/pgplot/"
#
#
# PGPLOT_DEV is the default PGPLOT plot device
export PGPLOT_DEV="/xwin"
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
#LINES_HLP is used to set the path to the lines help files
# edit as appropriate for your path structure,
# i.e. where the lines source code is found on your PC.
# for PCs away from hartrao this is likely to be:
export LINES_HLP="~/lines/doc/"
----------------------------------------------------------



To make the changes effective, from /home/<yourname>, type 

. .bash_profile
or
. .bashrc

or exit from the window manager and log on again.


2. Installing Lines away from HartRAO
-------------------------------------

First edit the Makefile so that "make install" puts the executable LINES in
the directory of your choice.  The standard location at Hart is
/usr/local/bin/  you may want it to be /usr/bin

run Makefile in /lines/src to compile the program:

make		compile and link to create executable "lines"
make install 	install "lines" to the directory specified in Makefile
                edit this path for the destination as needed,
                e.g. to /usr/bin
make clean	gets rid of intermediate files in /src



3.  Data files
--------------
'Lines' has not been trained to read data in FITS-format files.
Data from Hart is converted to ascii formats first:
* for spectroscopic data, use spfits_2asc
* for continuum drift scan data, use drift_fits2ascv12

The program can also read generic data in multicolumn ascii files.

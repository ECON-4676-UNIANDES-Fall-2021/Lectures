/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Teaching/2021/BDL/Lectures/Lecture29/figures/"


use partyid4, clear

tab party3

logit dem_ind income, nolog
outreg2 using "example_parties.tex", replace

logit rep_ind income, nolog
outreg2 using "example_parties.tex", append

logit dem_rep income, nolog
outreg2 using "example_parties.tex", append

mlogit party3 income, nolog base(2)
outreg2 using "example_parties.tex", append


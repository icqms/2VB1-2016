rem ****  convert pdb to hin, extract all alternate residue structures ***
rem **** next compiles needs readhin.cmn 
rem ifort -O2 -MP -c readhin.for
rem ifort pdb2hin-aniso.for readhin.obj
rem pdb2hin-aniso 2VB1 < pdb2hin-aniso_AAA.dat
rem move /y 2VB1.hin 2VB1AAA.hin
rem pdb2hin-aniso 2VB1 < pdb2hin-aniso_BBB.dat
rem move /y 2VB1.hin 2VB1BBB.hin
rem pdb2hin-aniso 2VB1 < pdb2hin-aniso_AAC.dat
rem move /y 2VB1.hin 2VB1AAC.hin
rem
rem **** check all residues for completion, fix simple errors, enforce template
rem **** next compiles needs readhin.cmn template.for template.cmn checkres.cmn
rem ifort -extend_source checkres.for readhin.obj
checkres 2VB1AAA > 2VB1AAA.checkres
checkres 2VB1AAC > 2VB1AAC.checkres
checkres 2VB1BBB > 2VB1BBB.checkres
rem
rem **** Monte Carlo cal of configurations for 8-copy cell 
copy 2VB1???c.hin config
cd config
rem **** first make 72 sample water structures, needs data file icosa
ifort makeh2o.for
makeh2o > makeh2o.out
rem
rem next compile needs readhin.cmn config.cmn, needs data file lj.dat groups.dat
ifort -O2 -MP -c rand.for
ifort -O2 -MP -extend_source -traceback config.for readhin.obj rand.obj
config A > config_A.out

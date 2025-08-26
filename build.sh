echo ":- style_check(-singleton)." > nars.pl
#echo ":- pack_remove(swicli)." >> nars.pl
cat utils.pl formulas.pl rules.pl config.pl deriver.pl $1 >> nars.pl

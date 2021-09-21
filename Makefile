build:
	ca65 snakenes.asm -o snakenes.o --debug-info -l snakenes.lst 
	ld65 snakenes.o -o snakenes.nes -t nes --dbgfile snakenes.dbg

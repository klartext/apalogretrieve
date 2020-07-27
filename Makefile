all: native-code
#all: byte-code
#all: debug-code


#SOURCES :=  cli.ml tools.ml parsetreetypes.ml scriptparser.mly scriptlexer.mll parsers.ml network.ml evaluate.ml main.ml

SOURCES := unixbindings.c logentry.ml compressed.ml lexcombinedlog.mll readcombinedlog.ml querygrammar.mly querytypes.ml querylex.mll   datetime.ml  main.ml


PACKS := pcre netstring netsys netclient nettls-gnutls xmlm yojson netunidata

OCAMLFLAGS := -w +11+22+26+32+33+34+35+36+37+38+39-58 -safe-string

YFLAGS=-v
OCAMLYACC = ocamlyacc

RESULT = apalogretrieve


include OCamlMakefile

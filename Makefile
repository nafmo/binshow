# Makefile f”r BinShow

.pas.tpu:
  tpc $*

binshow.exe: binshow.pas color.inc mkcrt.tpu
  tpc binshow
  lzeshell binshow.exe
  del binshow.old
